#include "ssa.h"

#include "types.h"
#include "parser.h"

#include "passbody.h"
#include "passzero.h"

#define NM_ALWAYS_VOID \
  ( NM(THROW) | NM(BREAK) | NM(CONTINUE) | NM(JUMP) \
    | NM(LET /* When in LIR form */ ) \
    | NM(RETURN) | NM(NOOP) | NM(WHILE) )

// Used to minimize the number of spurious vars introduced because we do not
// yet have typing information.
static bool is_always_void(struct node *node) {
  if (NM(node->which) & NM_ALWAYS_VOID) {
    return true;
  }

  switch (node->which) {
  case BIN:
    if (OP_IS_ASSIGN(node->as.BIN.operator)) {
      return true;
    }
    return false;
  case BLOCK:
    return is_always_void(subs_last(node));
  case IF:
    return is_always_void(subs_at(node, 1)) || is_always_void(subs_last(node));
  case MATCH:
    FOREACH_SUB_EVERY(s, node, 2, 2) {
      if (is_always_void(s)) {
        return true;
      }
    }
    return false;
  case CATCH:
    return is_always_void(subs_first(node));
  case TRY:
    FOREACH_SUB(s, node) {
      if (is_always_void(s)) {
        return true;
      }
    }
    return false;
  default:
    return false;
  }
}

static bool is_block_like(struct node *node) {
  return NM(node->which) & (NM(IF) | NM(TRY) | NM(MATCH) | NM(BLOCK));
}

static void insert_assign(struct module *mod,
                          struct node *statement_parent,
                          struct node *defn,
                          struct node *block) {
  assert(block->which == BLOCK);
  struct node *last = subs_last(block);
  node_subs_remove(block, last);

  struct node *assign = mk_node(mod, block, BIN);
  assign->codeloc = last->codeloc;
  assign->as.BIN.operator = TASSIGN;
  struct node *name = mk_node(mod, assign, IDENT);
  name->as.IDENT.name = node_ident(defn);
  node_subs_append(assign, last);

  error e = catchup(mod, NULL, assign, CATCHUP_BEFORE_CURRENT_SAME_TOP);
  assert(!e);
}

static void defname_replace_block_like_expr(struct module *mod,
                                            struct node *statement_parent,
                                            struct node *defn) {
  struct node *expr = subs_last(defn);
  if (!is_block_like(expr)) {
    return;
  }

  if (is_always_void(expr)) {
    return;
  }

  // FIXME: when inserting an assign, its LHS is not tracked by ssa_user in
  // the DEFNAME. Which is why we cannot yet use
  // try_remove_unnecessary_ssa_defname().

  node_subs_remove(defn, expr);
  (void) mk_node(mod, defn, INIT);

  struct node *let = parent(defn);
  node_subs_insert_after(statement_parent, let, expr);

  switch (expr->which) {
  case BLOCK:
    insert_assign(mod, statement_parent, defn, expr);
    break;
  case IF:
    insert_assign(mod, statement_parent, defn, subs_at(expr, 1));
    insert_assign(mod, statement_parent, defn, subs_last(expr));
    break;
  case TRY:
    insert_assign(mod, statement_parent, defn, subs_first(expr));
    FOREACH_SUB_EVERY(s, expr, 1, 1) {
      insert_assign(mod, statement_parent, defn, subs_first(s));
    }
    break;
  case MATCH:
    FOREACH_SUB_EVERY(s, expr, 2, 2) {
      insert_assign(mod, statement_parent, defn, s);
    }
    break;
  default:
    assert(false);
  }
}

static void ssa_sub(struct module *mod, struct node *statement,
                    struct node *node, struct node *sub) {
  assert(parent(sub) == node);
  if ((NM(sub->which) & (NM(IDENT) | NM(CALLNAMEDARG)))
      || is_always_void(sub)) {
    return;
  }

  struct node *statement_parent = parent(statement);

  if (sub == statement) {
    assert(node->which == BLOCK);
    if (mod->state->fun_state->block_state->prev != NULL) {
      mod->state->fun_state->block_state->current_statement
        = mod->state->fun_state->block_state->prev->current_statement;
    }
  }

  struct node *before = statement;

  struct node *let = mk_node(mod, statement_parent, LET);
  let->codeloc = sub->codeloc;
  node_subs_remove(statement_parent, let);
  node_subs_insert_before(statement_parent, before, let);

  struct node *defn = mk_node(mod, let, DEFNAME);
  defn->as.DEFNAME.ssa_user = sub;
  // At this point we don't know if 'sub' is a type expression (and not a
  // value) for which we should have used DEFALIAS. type_inference_ident()
  // will correct this when we know what 'sub' is.

  const ident g = gensym(mod);
  struct node *name = mk_node(mod, defn, IDENT);
  name->as.IDENT.name = g;
  struct node *new_sub = node_new_subnode(mod, defn);
  node_move_content(new_sub, sub);

  defname_replace_block_like_expr(mod, statement_parent, defn);

  node_set_which(sub, IDENT);
  sub->as.IDENT.name = g;

  error e;
  e = catchup(mod, NULL, let, CATCHUP_BEFORE_CURRENT_SAME_TOP);
  assert(!e);

  INVARIANT_NODE(sub);
}

STEP_NM(step_ssa_convert,
        ~(NM(MODULE) | NM(MODULE_BODY) | STEP_NM_DEFS | NM_ALWAYS_VOID));
error step_ssa_convert(struct module *mod, struct node *node,
                       void *user, bool *stop) {
  DSTEP(mod, node);
  if (mod->state->fun_state == NULL
      || mod->state->fun_state->block_state == NULL
      || is_always_void(node)) {
    return 0;
  }

  struct node *par = parent(node);
  struct node *statement = mod->state->fun_state->block_state->current_statement;

  switch (par->which) {
  case BIN:
    if (OP_IS_ASSIGN(par->as.BIN.operator) && node == subs_last(par)) {
      ssa_sub(mod, statement, par, node);
      break;
    }
    // fallthrough
  case SIZEOF:
  case ALIGNOF:
  case UN:
  case TUPLE:
  case CALLNAMEDARG:
  case INIT:
  case RETURN:
  case PRE:
  case POST:
  case INVARIANT:
  case THROW:
    ssa_sub(mod, statement, par, node);
    break;

  case CALL:
    if (subs_count_atleast(par, 2) && node != subs_first(par)) {
      ssa_sub(mod, statement, par, node);
    }
    break;

  case BLOCK:
    if (node == subs_last(par)) {
      ssa_sub(mod, node, par, node);
    }
    break;
  default:
    break;
  }
  assert(mod->state->fun_state->block_state->current_statement == NULL
         || parent(mod->state->fun_state->block_state->current_statement)->which == BLOCK);
  return 0;
}

static error ex_ssa_conversion(struct module *mod, struct node *root) {
  int module_depth = 0;
  mod->stage->state->passing = 0;
  mod->state->furthest_passing = 0;
  return passzero[0](mod, root, &module_depth, -1);
}

EXAMPLE_NCC_EMPTY(ssa_return) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G_IDENT(name, "foo");
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(ret, "u32"));
     G(body, BLOCK,
       G(r, RETURN,
         G(b, BLOCK,
           G(x, NUMBER,
             x->as.NUMBER.value = "0")))));
  assert(0 == ex_ssa_conversion(mod, n));
  check_structure(n,
                  "DEFFUN",
                  " IDENT",
                  " GENARGS",
                  " FUNARGS",
                  "  IDENT",
                  " BLOCK",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    INIT",
                  "  BLOCK",
                  "   LET",
                  "    DEFNAME",
                  "     IDENT",
                  "     NUMBER",
                  "   BIN",
                  "    IDENT",
                  "    IDENT",
                  "  RETURN",
                  "   IDENT",
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_return2) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G_IDENT(name, "foo");
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(ret, "u32"));
     G(body, BLOCK,
       G(r, RETURN,
         G(cmp, BIN,
           cmp->as.BIN.operator = TLT;
           G_IDENT(aa, "a");
           G(b, BIN,
             b->as.BIN.operator = TDOT;
             G_IDENT(s2, "self");
             G_IDENT(bb, "a"))))));
  assert(0 == ex_ssa_conversion(mod, n));
  check_structure(n,
                  "DEFFUN",
                  " IDENT",
                  " GENARGS",
                  " FUNARGS",
                  "  IDENT",
                  " BLOCK",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    BIN",
                  "     IDENT",
                  "     IDENT",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    BIN",
                  "     IDENT",
                  "     IDENT",
                  "  RETURN",
                  "   IDENT",
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_bin) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(ret, "void"));
     G(block, BLOCK,
       G(add, BIN,
         add->as.BIN.operator = TPLUS;
         G(mult, BIN,
           mult->as.BIN.operator = TMINUS;
           G(va, IDENT);
           G(vb, IDENT));
         G(vc, IDENT))));
  assert(0 == ex_ssa_conversion(mod, n));
  check_structure(n,
                  "DEFFUN",
                  " GENARGS",
                  " FUNARGS",
                  "  IDENT",
                  " BLOCK",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    BIN",
                  "     IDENT",
                  "     IDENT",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    BIN",
                  "     IDENT",
                  "     IDENT",
                  "  IDENT",
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_call) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(ret, "void"));
     G(block, BLOCK,
       G(call, CALL,
         G(fun, BIN,
           fun->as.BIN.operator = TDOT;
           G(fun1, IDENT);
           G(fun2, IDENT));
         G(mult, BIN,
           mult->as.BIN.operator = TMINUS;
           G(va, IDENT);
           G(vb, IDENT));
         G(vc, IDENT))));
  assert(0 == ex_ssa_conversion(mod, n));
  check_structure(n,
                  "DEFFUN",
                  " GENARGS",
                  " FUNARGS",
                  "  IDENT",
                  " BLOCK",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    BIN",
                  "     IDENT",
                  "     IDENT",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    CALL",
                  "     BIN",
                  "      IDENT",
                  "      IDENT",
                  "     IDENT",
                  "     IDENT",
                  "  IDENT",
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_let_one) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(ret, "void"));
     G(block, BLOCK,
       G(let, LET,
         G(defp, DEFPATTERN,
           G(name, IDENT);
           G(call, CALL,
             G(fun, BIN,
               fun->as.BIN.operator = TDOT;
               G(fun1, IDENT);
               G(fun2, IDENT));
             G(mult, BIN,
               mult->as.BIN.operator = TMINUS;
               G(va, IDENT);
               G(vb, IDENT));
             G(vc, IDENT))))));
  assert(0 == ex_ssa_conversion(mod, n));
  check_structure(n,
                  "DEFFUN",
                  " GENARGS",
                  " FUNARGS",
                  "  IDENT",
                  " BLOCK",
                  "  NOOP",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    BIN",
                  "     IDENT",
                  "     IDENT",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    CALL",
                  "     BIN",
                  "      IDENT",
                  "      IDENT",
                  "     IDENT",
                  "     IDENT",
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_let_two) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(ret, "void"));
     G(block, BLOCK,
       G(let, CALL,
         G(fun, IDENT);
         G(x, INIT);
         G(y, INIT))));
  assert(0 == ex_ssa_conversion(mod, n));
  check_structure(n,
                  "DEFFUN",
                  " GENARGS",
                  " FUNARGS",
                  "  IDENT",
                  " BLOCK",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    INIT",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    CALL",
                  "     IDENT",
                  "     IDENT",
                  "     IDENT",
                  "  IDENT",
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_let_three) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(ret, "void"));
     G(block, BLOCK,
       G(let, LET,
         G(defp, DEFPATTERN,
           G(name, IDENT);
           G(call, CALL,
             G(fun, BIN,
               fun->as.BIN.operator = TDOT;
               G(path1, IDENT);
               G(path2, IDENT));
             G(mult, BIN,
               mult->as.BIN.operator = TMINUS;
               G(va, IDENT);
               G(vb, IDENT));
             G(vc, IDENT)));
         G(defp2, DEFPATTERN,
           G(name2, IDENT);
           G(call2, CALL,
             G(fun2, IDENT);
             G(x, IDENT);
             G(y, IDENT))))));
  assert(0 == ex_ssa_conversion(mod, n));
  check_structure(n,
                  "DEFFUN",
                  " GENARGS",
                  " FUNARGS",
                  "  IDENT",
                  " BLOCK",
                  "  NOOP",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    BIN",
                  "     IDENT",
                  "     IDENT",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    CALL",
                  "     BIN",
                  "      IDENT",
                  "      IDENT",
                  "     IDENT",
                  "     IDENT",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    CALL",
                  "     IDENT",
                  "     IDENT",
                  "     IDENT",
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_while) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(ret, "void"));
     G(body, BLOCK,
       G(whil, WHILE,
         G(cmp, BIN,
           cmp->as.BIN.operator = TGT;
           G(a, INIT);
           G(b, INIT));
         G(loop, BLOCK,
           G(i, IF,
             G(cmp2, BIN,
               cmp2->as.BIN.operator = TLT;
               G_IDENT(aa, "a");
               G_IDENT(bb, "b"));
             G(yes, BLOCK,
               G(assign1, BIN,
                 assign1->as.BIN.operator = TASSIGN;
                 G_IDENT(c, "c");
                 G(d, INIT))))))));
  assert(0 == ex_ssa_conversion(mod, n));
  check_structure(n,
                  "DEFFUN",
                  " GENARGS",
                  " FUNARGS",
                  "  IDENT",
                  " BLOCK",
                  "  WHILE",
                  "   BLOCK",
                  "    LET",
                  "     DEFNAME",
                  "      IDENT",
                  "      INIT",
                  "    LET",
                  "     DEFNAME",
                  "      IDENT",
                  "      INIT",
                  "    LET",
                  "     DEFNAME",
                  "      IDENT",
                  "      BIN",
                  "       IDENT",
                  "       IDENT",
                  "    IDENT",
                  "   BLOCK",
                  "    IF",
                  "     BLOCK",
                  "      LET",
                  "       DEFNAME",
                  "        IDENT",
                  "        BIN",
                  "         IDENT",
                  "         IDENT",
                  "      IDENT",
                  "     BLOCK",
                  "      LET",
                  "       DEFNAME",
                  "        IDENT",
                  "        INIT",
                  "      BIN",
                  "       IDENT",
                  "       IDENT",
                  "     BLOCK",
                  "      NOOP",
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_if_one) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(ret, "void"));
     G(body, BLOCK,
       G(let, LET,
         G(defp, DEFPATTERN,
           G(e1, IDENT);
           G(end1, IDENT));
         G(such, BLOCK,
           G(i, IF,
             G(cmp, BIN,
               cmp->as.BIN.operator = TLT;
               G(end2, IDENT);
               G(beg1, IDENT));
             G(yes, BLOCK,
               G(assign1, BIN,
                 assign1->as.BIN.operator = TASSIGN;
                 G(e2, IDENT);
                 G(beg2, IDENT))))))));
  assert(0 == ex_ssa_conversion(mod, n));
  check_structure(n,
                  "DEFFUN",
                  " GENARGS",
                  " FUNARGS",
                  "  IDENT",
                  " BLOCK",
                  "  NOOP",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    IDENT",
                  "  BLOCK",
                  "   IF",
                  "    BLOCK",
                  "     LET",
                  "      DEFNAME",
                  "       IDENT",
                  "       BIN",
                  "        IDENT",
                  "        IDENT",
                  "     IDENT",
                  "    BLOCK",
                  "     BIN",
                  "      IDENT",
                  "      IDENT",
                  "    BLOCK",
                  "     NOOP",
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_if_two) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(ret, "void"));
     G(body, BLOCK,
       G(ifx, IF,
         G(acc, BIN,
           acc->as.BIN.operator = TDOT;
           G_IDENT(self, "self");
           G_IDENT(f, "f"));
         G(ba, BLOCK,
           G_IDENT(a, "a"));
         G(bb, BLOCK,
           G_IDENT(b, "b")))));
  assert(0 == ex_ssa_conversion(mod, n));
  check_structure(n,
                  "DEFFUN",
                  " GENARGS",
                  " FUNARGS",
                  "  IDENT",
                  " BLOCK",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    INIT",
                  "  IF",
                  "   BLOCK",
                  "    LET",
                  "     DEFNAME",
                  "      IDENT",
                  "      BIN",
                  "       IDENT",
                  "       IDENT",
                  "    IDENT",
                  "   BLOCK",
                  "    BIN",
                  "     IDENT",
                  "     IDENT",
                  "   BLOCK",
                  "    BIN",
                  "     IDENT",
                  "     IDENT",
                  "  IDENT",
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_logical_or) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(ret, "void"));
     G(body, BLOCK,
       G(or, BIN,
         or->as.BIN.operator = Tor;
         G(z, BIN,
           z->as.BIN.operator = TLT;
           G_IDENT(x, "x");
           G(y, INIT));
         G_IDENT(a, "a"))));
  assert(0 == ex_ssa_conversion(mod, n));
  check_structure(n,
                  "DEFFUN",
                  " GENARGS",
                  " FUNARGS",
                  "  IDENT",
                  " BLOCK",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    INIT",
                  "  IF",
                  "   BLOCK",
                  "    LET",
                  "     DEFNAME",
                  "      IDENT",
                  "      INIT",
                  "    LET",
                  "     DEFNAME",
                  "      IDENT",
                  "      BIN",
                  "       IDENT",
                  "       IDENT",
                  "    IDENT",
                  "   BLOCK",
                  "    LET",
                  "     DEFNAME",
                  "      IDENT",
                  "      BOOL",
                  "    BIN",
                  "     IDENT",
                  "     IDENT",
                  "   BLOCK",
                  "    BIN",
                  "     IDENT",
                  "     IDENT",
                  "  IDENT",
                  NULL);
}

static STEP_NM(step_remove_ident_from_use_chain,
               NM(IDENT));
static error step_remove_ident_from_use_chain(struct module *mod, struct node *node,
                                              void *user, bool *stop) {
  if (node->as.IDENT.next_use != NULL) {
    assert(node->as.IDENT.next_use->which == IDENT);
    node->as.IDENT.next_use->as.IDENT.prev_use = node->as.IDENT.prev_use;
  }

  if (node->as.IDENT.prev_use != NULL) {
    if (node->as.IDENT.prev_use->which == IDENT) {
      node->as.IDENT.prev_use->as.IDENT.next_use = node->as.IDENT.next_use;
    }

    struct node *def = node->as.IDENT.def;
    if (def->which == DEFNAME || def->which == DEFARG) {
      struct phi_tracker_state *phi_st = get_phi_tracker(def);
      if (phi_st->last == node) {
        phi_st->last = node->as.IDENT.prev_use;
      }
    }
  }

  return 0;
}

static error pass_remove_expr_from_use_chain(struct module *mod, struct node *root,
                                             void *user, ssize_t shallow_last_up) {
  PASS(
    ,
    UP_STEP(step_remove_ident_from_use_chain);
    ,
    );
  return 0;
}

static void remove_expr_from_use_chain(struct module *mod, struct node *expr) {
  error e = pass_remove_expr_from_use_chain(mod, expr, NULL, -1);
  assert(!e);
}

static void remove_unnecessary_ssa_defname(struct module *mod, struct node *defn) {
  assert(defn->which == DEFNAME);

  struct node *let = parent(defn);
  assert(!subs_count_atleast(let, 2));
  assert(parent(let)->which == BLOCK);

  struct node *expr = subs_last(defn);
  struct node *user = defn->as.DEFNAME.ssa_user;

  struct node *block = let;
  do {
    block = parent(block);
    assert(block->which == BLOCK);
  } while (block->as.BLOCK.is_scopeless);
  scope_undefine_ssa_var(&block->scope, node_ident(defn));

  remove_expr_from_use_chain(mod, expr);

  node_subs_remove(let, defn);
  node_set_which(let, NOOP);

  defn->as.DEFNAME.ssa_user = NULL;
  node_subs_remove(defn, expr);

  struct typ *typ_copy = expr->typ;
  node_move_content(user, expr);
  set_typ(&user->typ, typ_copy);
  node_set_which(defn, NOOP);
  node_subs_remove(defn, subs_first(defn));
}

static bool needed_as_rvalue(struct node *user) {
  struct node *par = parent(user);
  return ((NM(par->which) & NM(BLOCK)) && needed_as_rvalue(par))
    || (NM(par->which) & (NM(IF) | NM(WHILE) | NM(MATCH)));
}

bool try_remove_unnecessary_ssa_defname(struct module *mod, struct node *defn) {
  assert(defn->which == DEFNAME);
  if (defn->as.DEFNAME.ssa_user == NULL) {
    return false;
  }

  struct node *expr = subs_last(defn);
  struct node *user = defn->as.DEFNAME.ssa_user;
  struct node *paruser = parent(user);

  bool remove = false;

  if ((defn->flags & NODE_IS_TYPE)
      || typ_equal(defn->typ, TBI_VOID)) {
    remove = true;
  } else if (needed_as_rvalue(user)) {
    remove = false;
  } else if (
    // Any OP_BIN_ACC left at this stage is a field access.
    (expr->which == BIN
     && OP_KIND(expr->as.BIN.operator) == OP_BIN_ACC)
    || (expr->which == UN
        && OP_KIND(expr->as.UN.operator) == OP_UN_DEREF
        && paruser->which == UN
        && OP_KIND(paruser->as.UN.operator) == OP_UN_REFOF)
    || (expr->which == UN
        && OP_KIND(expr->as.UN.operator) == OP_UN_DEREF
        && paruser->which == BIN
        && OP_IS_ASSIGN(paruser->as.BIN.operator)
        && subs_first(paruser) == user)
    || ((expr->which == INIT || expr->which == CALL)
        && paruser->which == RETURN
        && !typ_isa(module_retval_get(mod)->typ, TBI_RETURN_BY_COPY))) {
    remove = true;
  }

  if (remove) {
    remove_unnecessary_ssa_defname(mod, defn);
  }
  return remove;
}
