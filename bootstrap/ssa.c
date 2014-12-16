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

#define NM_DOESNT_EVER_NEED_SUB \
  ( NM(IDENT) | NM(CALLNAMEDARG) | NM(CATCH) )

#define NM_DOESNT_NEED_SUB \
  ( NM_DOESNT_EVER_NEED_SUB | NM(NUMBER) | NM(NIL) | NM(BOOL) )

static bool doesnt_need_sub(struct node *node, struct node *sub) {
  if (!(NM(sub->which) & NM_DOESNT_NEED_SUB)) {
    return false;
  }

  if (NM(sub->which) & NM_DOESNT_EVER_NEED_SUB) {
    return true;
  }

  switch (node->which) {
  case UN:
    return OP_KIND(node->as.UN.operator) != OP_UN_REFOF;
  case BIN:
    return OP_KIND(node->as.BIN.operator) != OP_BIN_ACC;
  default:
    return false;
  }
}

static bool is_block_like(struct node *node) {
  return NM(node->which) & (NM(IF) | NM(TRY) | NM(MATCH) | NM(BLOCK));
}

static void insert_assign(struct module *mod,
                          struct node *block,
                          ident var) {
  assert(block->which == BLOCK);
  struct node *last = subs_last(block);
  node_subs_remove(block, last);

  struct node *assign = mk_node(mod, block, BIN);
  assign->codeloc = last->codeloc;
  assign->as.BIN.operator = TASSIGN;
  struct node *name = mk_node(mod, assign, IDENT);
  name->as.IDENT.name = var;
  node_subs_append(assign, last);

  error e = catchup(mod, NULL, assign, CATCHUP_BEFORE_CURRENT_SAME_TOP);
  assert(!e);
}

static void defname_replace_block_like_expr(struct module *mod,
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
  node_subs_insert_after(parent(let), let, expr);

  const ident name = node_ident(defn);
  switch (expr->which) {
  case BLOCK:
    insert_assign(mod, expr, name);
    break;
  case IF:
    insert_assign(mod, subs_at(expr, 1), name);
    insert_assign(mod, subs_last(expr), name);
    break;
  case TRY:
    insert_assign(mod, subs_first(expr), name);
    FOREACH_SUB_EVERY(s, expr, 1, 1) {
      insert_assign(mod, subs_first(s), name);
    }
    break;
  case MATCH:
    FOREACH_SUB_EVERY(s, expr, 2, 2) {
      insert_assign(mod, s, name);
    }
    break;
  default:
    assert(false);
  }
}

static struct node *find_current_statement(struct node *node) {
  if (node->which == BLOCK) {
    return subs_last(node);
  }
  struct node *c = node;
  while (parent_const(c)->which != BLOCK) {
    c = parent(c);
  }
  return c;
}

static ERROR ssa_sub(struct module *mod, struct node *node, struct node *sub) {
  assert(parent(sub) == node);
  if (doesnt_need_sub(node, sub)
      || is_always_void(sub)) {
    return 0;
  }

  struct node *statement = find_current_statement(node);
  struct node *statement_parent = parent(statement);
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

  defname_replace_block_like_expr(mod, defn);

  node_set_which(sub, IDENT);
  sub->as.IDENT.name = g;

  error e = catchup(mod, NULL, let, CATCHUP_BEFORE_CURRENT_SAME_TOP);
  EXCEPT(e);

  INVARIANT_NODE(sub);
  return 0;
}

// step_ssa_convert() cannot work on excepted nodes. In the operator
// function rewrite:
//   0 + 1 -> Operator_plus @0 @1
// 0 and 1 are excepted. So we need to come after the fact and double check.
// TODO: see if we can reduce the number of nodes that
// step_ssa_convert_shallow_catchup() gets called on.
static ERROR fix_ssa_sub(struct module *mod, struct node *node, struct node *sub) {
  if (sub->excepted > 0) {
    error e = ssa_sub(mod, node, sub);
    EXCEPT(e);
  }
  return 0;
}

STEP_NM(step_ssa_convert_shallow_catchup,
        NM(BIN) | NM(SIZEOF) | NM(ALIGNOF) | NM(UN) | NM(TUPLE) |
        NM(CALLNAMEDARG) | NM(INIT) | NM(RETURN) | NM(PRE) | NM(POST) |
        NM(INVARIANT) | NM(THROW) | NM(CALL) | NM(BLOCK));
error step_ssa_convert_shallow_catchup(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  DSTEP(mod, node);
  if (mod->state->top_state->is_propagating_constant) {
    return 0;
  }

  if (mod->state->fun_state == NULL || !mod->state->fun_state->in_block) {
    return 0;
  }

  error e;
  switch (node->which) {
  case BIN:
    if (OP_IS_ASSIGN(node->as.BIN.operator)) {
      e = ssa_sub(mod, node, subs_last(node));
      EXCEPT(e);
      break;
    } else if (node->as.BIN.operator == Tisa) {
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
    FOREACH_SUB(sub, node) {
      e = fix_ssa_sub(mod, node, sub);
      EXCEPT(e);
    }
    break;

  case CALL:
    if (subs_count_atleast(node, 2)) {
      FOREACH_SUB_EVERY(sub, node, 1, 1) {
        e = fix_ssa_sub(mod, node, sub);
        EXCEPT(e);
      }
    }
    break;

  case BLOCK:
    if (NM(parent(node)->which) & (NM(DEFFUN) | NM(DEFMETHOD))) {
      break;
    } else if (subs_last(node)->which == BIN
               && subs_last(node)->as.BIN.operator == Tisa
               && parent_const(node)->which == IF
               && subs_first_const(parent_const(node)) == node) {
      break;
    } else {
      e = fix_ssa_sub(mod, node, subs_last(node));
      EXCEPT(e);
    }
    break;
  default:
    break;
  }
  return 0;
}

static ERROR ssa_convert_global(struct module *mod, struct node *node) {
  struct node *par = parent(node);
  switch (par->which) {
  case UN:
    break;
  default:
    return 0;
  }

  if (doesnt_need_sub(par, node)) {
    return 0;
  }

  struct node *top = par, *pretop = par;
  while (parent_const(top) != mod->body) {
    pretop = top;
    top = parent(top);
  }

  if (top->which != LET && pretop->which != LET) {
    return 0;
  }

  struct node *where = pretop->which == LET ? pretop : top;
  struct node *parwhere = parent(where);

  GSTART();
  G0(let, parwhere, LET,
     let->flags |= NODE_IS_GLOBAL_LET;
    G(defn, DEFNAME,
      G(name, IDENT,
        name->as.IDENT.name = gensym(mod));
      G(dst, 0,
        node_move_content(dst, node))));
  defn->as.DEFNAME.ssa_user = node;

  node_subs_remove(parwhere, let);
  node_subs_insert_before(parwhere, where, let);

  node_set_which(node, IDENT);
  node->as.IDENT.name = node_ident(name);

  error e = catchup(mod, NULL, let, CATCHUP_BEFORE_CURRENT_SAME_TOP);
  assert(!e);

  INVARIANT_NODE(sub);

  return 0;
}

STEP_NM(step_ssa_convert,
        ~(NM(MODULE) | NM(MODULE_BODY) | STEP_NM_DEFS | NM_ALWAYS_VOID
          | NM_DOESNT_EVER_NEED_SUB));
error step_ssa_convert(struct module *mod, struct node *node,
                       void *user, bool *stop) {
  if (mod->state->top_state->is_propagating_constant) {
    return 0;
  }

  DSTEP(mod, node);
  if (mod->state->fun_state == NULL) {
    return ssa_convert_global(mod, node);
  } else if (!mod->state->fun_state->in_block
             || is_always_void(node)) {
    return 0;
  }

  struct node *par = parent(node);

  error e;
  switch (par->which) {
  case BIN:
    if (OP_IS_ASSIGN(par->as.BIN.operator)) {
      if (node == subs_last(par)
          || (node == subs_first(par) && node->which != TUPLE)) {
        e = ssa_sub(mod, par, node);
        EXCEPT(e);
      }
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
    e = ssa_sub(mod, par, node);
    EXCEPT(e);
    break;

  case CALL:
    if (subs_count_atleast(par, 2) && node != subs_first(par)) {
      e = ssa_sub(mod, par, node);
      EXCEPT(e);
    }
    break;

  case BLOCK:
    if (node == subs_last(par)) {
      if (NM(parent(par)->which) & (NM(DEFFUN) | NM(DEFMETHOD))) {
        break;
      } else if (node->which == BIN
                 && node->as.BIN.operator == Tisa
                 && parent_const(par)->which == IF
                 && subs_first_const(parent_const(par)) == par) {
        break;
      } else {
        e = ssa_sub(mod, par, node);
        EXCEPT(e);
      }
    }
    break;
  default:
    break;
  }
  return 0;
}

static ERROR ex_ssa_conversion(struct module *mod, struct node *root) {
  int module_depth = 0;
  mod->stage->state->passing = 0;
  mod->state->furthest_passing = 0;
  return passzero[0](mod, root, &module_depth, -1);
}

EXAMPLE_NCC_EMPTY(ssa_global_let) {
  GSTART();
  G0(let, mod->body, LET,
     G(defn, DEFNAME,
       G_IDENT(name, "foo");
       G(un, UN,
         un->as.UN.operator = TREFDOT;
         G(bar, STRING,
           bar->as.STRING.value = "bar"))));
  assert(0 == ex_ssa_conversion(mod, let));
  check_structure(parent(let),
                  "MODULE_BODY",
                  " LET",
                  "  DEFNAME",
                  "   IDENT",
                  "   STRING",
                  " LET",
                  "  DEFNAME",
                  "   IDENT",
                  "   UN",
                  "    IDENT",
                  NULL);
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
       G_IDENT(retval, "void"));
     G(block, BLOCK,
       G(ret, RETURN,
         G(add, BIN,
           add->as.BIN.operator = TPLUS;
           G(mult, BIN,
             mult->as.BIN.operator = TMINUS;
             G(va, IDENT);
             G(vb, IDENT));
           G(vc, IDENT)))));
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
                  "  RETURN",
                  "   IDENT",
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_call) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(retval, "void"));
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
                  "  CALL",
                  "   BIN",
                  "    IDENT",
                  "    IDENT",
                  "   IDENT",
                  "   IDENT",
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_call_tuple) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(retval, "void"));
     G(block, BLOCK,
       G(let, LET,
         G(defp, DEFPATTERN,
           G(tu, TUPLE,
             G_IDENT(x1, "x");
             G_IDENT(y1, "y"))
           G(call, CALL,
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
                  "    CALL",
                  "     IDENT",
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
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_call_tuple_assign) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(retval, "void"));
     G(block, BLOCK,
       G(let, LET,
         G(defp, DEFPATTERN,
           G(tu1, TUPLE,
             G_IDENT(x1, "x");
             G_IDENT(y1, "y")));
         G(such, BLOCK,
           G(ass, BIN,
             ass->as.BIN.operator = TASSIGN;
             G(tu2, TUPLE,
               G_IDENT(x2, "x");
               G_IDENT(y2, "y"))
             G(call, CALL,
               G(vc, IDENT)))))));
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
                  "    INIT",
                  "  LET",
                  "   DEFNAME",
                  "    IDENT",
                  "    INIT",
                  "  BLOCK",
                  "   BLOCK",
                  "    NOOP",
                  "    LET",
                  "     DEFNAME",
                  "      IDENT",
                  "      CALL",
                  "       IDENT",
                  "    LET",
                  "     DEFNAME",
                  "      IDENT",
                  "      BIN",
                  "       IDENT",
                  "       IDENT",
                  "    BIN",
                  "     IDENT",
                  "     IDENT",
                  "    LET",
                  "     DEFNAME",
                  "      IDENT",
                  "      BIN",
                  "       IDENT",
                  "       IDENT",
                  "    BIN",
                  "     IDENT",
                  "     IDENT",
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_let_one) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(retval, "void"));
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
       G_IDENT(retval, "void"));
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
                  "  CALL",
                  "   IDENT",
                  "   IDENT",
                  "   IDENT",
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_let_three) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(retval, "void"));
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
       G_IDENT(retval, "void"));
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
       G_IDENT(retval, "void"));
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
       G_IDENT(retval, "void"));
     G(body, BLOCK,
       G(ret, RETURN,
         G(ifx, IF,
           G(acc, BIN,
             acc->as.BIN.operator = TDOT;
             G_IDENT(self, "self");
             G_IDENT(f, "f"));
           G(ba, BLOCK,
             G_IDENT(a, "a"));
           G(bb, BLOCK,
             G_IDENT(b, "b"))))));
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
                  "  RETURN",
                  "   IDENT",
                  NULL);
}

EXAMPLE_NCC_EMPTY(ssa_logical_or) {
  GSTART();
  G0(n, mod->body, DEFFUN,
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
       G_IDENT(retval, "void"));
     G(body, BLOCK,
       G(ret, RETURN,
         G(or, BIN,
           or->as.BIN.operator = Tor;
           G(z, BIN,
             z->as.BIN.operator = TLT;
             G_IDENT(x, "x");
             G(y, INIT));
           G_IDENT(a, "a")))));
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
                  "  RETURN",
                  "   IDENT",
                  NULL);
}

static STEP_NM(step_remove_ident_from_use_chain,
               NM(IDENT));
static ERROR step_remove_ident_from_use_chain(struct module *mod, struct node *node,
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

static ERROR pass_remove_expr_from_use_chain(struct module *mod, struct node *root,
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

  struct node *expr = subs_last(defn);
  struct node *user = defn->as.DEFNAME.ssa_user;

  struct node *block = parent(let);
  assert(NM(block->which) & (NM(BLOCK) | NM(MODULE_BODY)));
  while (block->which == BLOCK && block->as.BLOCK.is_scopeless) {
    block = parent(block);
    assert(block->which == BLOCK);
  }
  scope_undefine_ssa_var(node_scope(block), node_ident(defn));

  remove_expr_from_use_chain(mod, expr);

  node_subs_remove(let, defn);
  node_set_which(let, NOOP);

  defn->as.DEFNAME.ssa_user = NULL;
  node_subs_remove(defn, expr);

  node_move_content(user, expr);
  unset_typ(&user->typ);
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

STEP_NM(step_insert_nullable_void,
        NM(UN));
error step_insert_nullable_void(struct module *mod, struct node *node,
                                void *user, bool *stop) {
  DSTEP(mod, node);

  if (OP_KIND(node->as.UN.operator) != OP_UN_REFOF
      || node->as.UN.is_explicit) {
    return 0;
  }
  struct node *term = subs_first(node);
  if (!typ_equal(term->typ, TBI_VOID)) {
    return 0;
  }

  struct node *statement = find_current_statement(node);
  struct node *statement_parent = parent(statement);

  node_subs_remove(node, term);
  node_subs_insert_before(statement_parent, statement, term);

  GSTART();
  G0(x, node, IDENT,
     x->as.IDENT.name = idents_add_string(mod->gctx,
                                          "Nonnull_void",
                                          strlen("Nonnull_void")));

  error e = catchup(mod, NULL, x, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  return 0;
}
