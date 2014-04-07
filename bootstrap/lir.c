#include "lir.h"

#include "passes.h"

STEP_NM(step_add_sequence_points,
        NM(BLOCK));
error step_add_sequence_points(struct module *mod, struct node *node,
                               void *user, bool *stop) {
  DSTEP(mod, node);

  if (!node->as.BLOCK.is_scopeless && subs_count_atleast(node, 2)) {
    FOREACH_SUB(s, node) {
      struct node *seq_point = mk_node(mod, node, BLOCK);
      seq_point->as.BLOCK.is_scopeless = true;
      node_subs_remove(node, seq_point);
      node_subs_insert_after(node, s, seq_point);
      node_subs_remove(node, s);
      node_subs_append(seq_point, s);
    }
  }

  return 0;
}

struct lir_loop_state {
  struct lir_loop_state *prev;

  struct node *entry;
  struct node *exit;
};

struct lir_try_state {
  struct lir_try_state *prev;

  ident error;
  struct node *first_catch;
};

struct lir_state {
  struct lir_state *prev;

  struct lir_loop_state *loop_state;
  struct lir_try_state *try_state;
};

static void rewrite_bool_op(struct module *mod, struct node *node) {
  assert(node->which == BIN);
  const ident op = node->as.BIN.operator;

  struct node *left = subs_first(node);
  struct node *right = subs_last(node);
  node_subs_remove(node, left);
  node_subs_remove(node, right);

  switch (op) {
  case Tand:
    {
      node_set_which(node, IF);
      struct node *cond_block = mk_node(mod, node, BLOCK);
      node_subs_append(cond_block, left);
      struct node *yes = mk_node(mod, node, BLOCK);
      node_subs_append(yes, right);
      struct node *no = mk_node(mod, node, BLOCK);
      struct node *t = mk_node(mod, no, BOOL);
      t->as.BOOL.value = false;
      break;
    }
  case Tor:
    {
      node_set_which(node, IF);
      struct node *cond_block = mk_node(mod, node, BLOCK);
      node_subs_append(cond_block, left);
      struct node *yes = mk_node(mod, node, BLOCK);
      struct node *t = mk_node(mod, yes, BOOL);
      t->as.BOOL.value = true;
      struct node *no = mk_node(mod, node, BLOCK);
      node_subs_append(no, right);
      break;
    }
  default:
    assert(false);
  }
}

static error find_catch(struct node **r,
                        struct module *mod, struct node *for_error,
                        struct lir_state *st, ident label) {
  error e;
  if (st->try_state == NULL) {
    e = mk_except(mod, for_error, "missing surrounding try block");
    THROW(e);
  }

  struct node *c = st->try_state->first_catch;
  assert(c->which == CATCH);

  if (label == ID__NONE) {
    if (next(c) != NULL) {
      e = mk_except(mod, for_error,
                    "no label for throw or except,"
                    " but multiple catch clauses");
      THROW(e);
    } else {
      if (c->as.CATCH.is_user_label) {
        e = mk_except(mod, for_error,
                      "no label for throw or except,"
                      " but catch clause has a label");
        THROW(e);
      }

      *r = c;
      return 0;
    }
  } else {
    while (c != NULL) {
      assert(c->which == CATCH);
      if (c->as.CATCH.label == label) {
        *r = c;
        return 0;
      }
      c = next(c);
    }
  }

  e = mk_except(mod, for_error, "catch label '%s' not found",
                idents_value(mod->gctx, label));
  THROW(e);
  return 0;
}

static struct node *insert_temporary(struct module *mod, uint32_t flags,
                                     bool is_globalenv,
                                     struct node *let_block, struct node *before,
                                     struct node *expr) {
  struct node *let = mk_node(mod, let_block, LET);
  if (before != NULL) {
    node_subs_remove(let_block, let);
    node_subs_insert_before(let_block, before, let);
  }
  struct node *defn = mk_node(mod, let, DEFNAME);
  defn->as.DEFNAME.is_globalenv = is_globalenv;
  struct node *name = mk_node(mod, defn, IDENT);
  name->as.IDENT.name = gensym(mod);
  node_subs_append(defn, expr);

  struct node *repl = mk_node(mod, let_block, IDENT);
  repl->as.IDENT.name = name->as.IDENT.name;
  node_subs_remove(let_block, repl);
  return repl;
}

static error extract_defnames(struct module *mod,
                              const struct toplevel *toplevel, uint32_t flags,
                              bool is_alias, bool is_globalenv,
                              struct node *let_block, struct node *before,
                              struct node *pattern, struct node *expr) {
  error e;

  if (is_alias) {
    if (pattern->which != IDENT) {
      e = mk_except(mod, pattern, "invalid name for alias");
      THROW(e);
    }
  }

  if (expr != NULL) {
    if (pattern->which == EXCEP || pattern->which == TUPLE) {
      expr = insert_temporary(mod, flags, is_globalenv, let_block, before, expr);
    } else if (pattern->which != IDENT) {
      e = mk_except(mod, pattern, "FIXME: value destruct not yet supported");
      THROW(e);
    }
  }

  switch (pattern->which) {
  case IDENT:
    {
      struct node *let = mk_node(mod, let_block, LET);
      let->as.LET.toplevel = *toplevel;
      let->flags = flags;
      if (before != NULL) {
        node_subs_remove(let_block, let);
        node_subs_insert_before(let_block, before, let);
      }

      if (is_alias) {
        struct node *defa = mk_node(mod, let, DEFALIAS);
        defa->flags = flags;
        node_subs_append(defa, pattern);
        if (expr == NULL) {
          expr = mk_node(mod, defa, IDENT);
          expr->as.IDENT.name = ID_TBI_ANY;
        } else {
          node_subs_append(defa, expr);
        }
        return 0;
      } else {
        struct node *defn = mk_node(mod, let, DEFNAME);
        defn->flags = flags;
        defn->as.DEFNAME.is_globalenv = is_globalenv;
        node_subs_append(defn, pattern);
        if (expr == NULL) {
          expr = mk_node(mod, defn, INIT);
        } else {
          node_subs_append(defn, expr);
        }
        return 0;
      }
    }
    break;

  case EXCEP:
    {
      if (expr == NULL) {
        e = mk_except(mod, pattern, "except must be used with an expression");
        THROW(e);
      }

      struct node *i = mk_node(mod, let_block, IF);
      struct node *test_block = mk_node(mod, i, BLOCK);
      struct node *test = mk_node(mod, test_block, CALL);
      struct node *op = mk_node(mod, test, BIN);
      op->as.BIN.operator = TDOT;
      node_subs_append(op, expr);
      struct node *op_name = mk_node(mod, op, IDENT);
      op_name->as.IDENT.name = ID_OPERATOR_TEST;

      struct node *yes = mk_node(mod, i, BLOCK);
      struct node *th = mk_node(mod, yes, THROW);
      th->as.THROW.label = pattern->as.EXCEP.label;
      struct node *e = mk_node(mod, th, IDENT);
      e->as.IDENT.name = node_ident(expr);
      return 0;
    }
    break;

  case TUPLE:
    {
      size_t n = 0;
      struct node *nxt = subs_first(pattern);
      do {
        struct node *t = nxt;
        nxt = next(nxt);

        node_subs_remove(pattern, t);

        if (expr == NULL) {
          e = extract_defnames(mod, toplevel, flags, is_alias, is_globalenv,
                               let_block, before, t, NULL);
          EXCEPT(e);
        } else if (expr->which == TUPLE) {
          e = extract_defnames(mod, toplevel, flags, is_alias, is_globalenv,
                               let_block, before, t, subs_at(expr, n));
          EXCEPT(e);
        } else {
          assert(expr->which == IDENT);

          struct node *ex = mk_node(mod, let_block, BIN);
          ex->as.BIN.operator = TDOT;
          node_subs_remove(let_block, ex);

          struct node *name = mk_node(mod, ex, IDENT);
          name->as.IDENT.name = node_ident(expr);
          struct node *field = mk_node(mod, ex, IDENT);
          char s[8] = { 0 };
          snprintf(s, ARRAY_SIZE(s), "x%zu", n);
          field->as.IDENT.name = idents_add_string(mod->gctx, s, strlen(s));

          e = extract_defnames(mod, toplevel, flags, is_alias, is_globalenv,
                               let_block, before, t, ex);
          EXCEPT(e);
        }
        n += 1;
      } while (nxt != NULL);
    }
    break;

  case TYPECONSTRAINT:
    {
      struct node *n = subs_first(pattern);
      node_subs_remove(pattern, n);
      struct node *t = subs_last(pattern);
      node_subs_remove(pattern, t);

      struct node *c = mk_node(mod, pattern, TYPECONSTRAINT);
      node_subs_remove(pattern, c);
      if (expr == NULL) {
        expr = mk_node(mod, c, INIT);
      } else {
        node_subs_append(c, expr);
      }
      node_subs_append(c, t);

      e = extract_defnames(mod, toplevel, flags, is_alias, is_globalenv,
                           let_block, before, n, c);
      EXCEPT(e);
    }
    break;

  default:
    e = mk_except(mod, pattern, "invalid construct in pattern");
    THROW(e);
  }

  return 0;
}

static error lir_conversion_defpattern(struct module *mod,
                                       const struct toplevel *toplevel, uint32_t flags,
                                       struct node *let_block, struct node *before,
                                       struct node *defp) {
  assert(defp->which == DEFPATTERN);

  node_subs_remove(parent(defp), defp);
  const size_t count = subs_count(defp);
  struct node *expr, *pattern = subs_first(defp);
  node_subs_remove(defp, pattern);
  if (count == 1) {
    expr = NULL;
  } else if (count == 2) {
    expr = subs_last(defp);
    node_subs_remove(defp, expr);
  } else {
    assert(false);
  }

  const bool is_alias = defp->as.DEFPATTERN.is_alias;
  const bool is_globalenv = defp->as.DEFPATTERN.is_globalenv;

  extract_defnames(mod, toplevel, flags, is_alias, is_globalenv,
                   let_block, before, pattern, expr);

  return 0;
}

static void ensure_in_block(struct module *mod, struct node *x) {
  struct node *node = parent(x);

  struct node *x_block = mk_node(mod, node, BLOCK);
  node_subs_remove(node, x_block);
  node_subs_replace(node, x, x_block);
  node_subs_append(x_block, x);
}

STEP_NM(step_lir_conversion_down,
        -1);
error step_lir_conversion_down(struct module *mod, struct node *node,
                               void *user, bool *stop) {
  DSTEP(mod, node);

  INVARIANT_NODE(node);

  if (mod->state->lir_state == NULL) {
    PUSH_STATE(mod->state->lir_state);
  }
  struct lir_state *st = mod->state->lir_state;

  error e;
  switch (node->which) {
  case BIN:
    if (OP_KIND(node->as.BIN.operator) == OP_BIN_SYM_BOOL) {
      rewrite_bool_op(mod, node);
    }
    break;
  case FUTURE:
    break;
  case FOR:
    {
      node_set_which(node, BLOCK);

      struct node *orig_var = subs_first(node);
      struct node *orig_expr = subs_at(node, 1);
      struct node *orig_block = subs_last(node);
      node_subs_remove(node, orig_var);
      node_subs_remove(node, orig_expr);
      node_subs_remove(node, orig_block);

      GSTART();
      G0(let_it, node, LET,
         G(it, DEFPATTERN,
           G(it_var, IDENT,
             it_var->as.IDENT.name = gensym(mod));
           G(it_expr, UN,
             it_expr->as.UN.operator = TREFBANG;
             node_subs_append(it_expr, orig_expr)));

         G(let_it_block, BLOCK,
           G(loop, WHILE,
             G(v, CALL,
               G(vm, BIN,
                 vm->as.BIN.operator = TDOT;
                 G(vmi, IDENT,
                   vmi->as.IDENT.name = node_ident(it_var));
                 G(vmm, IDENT,
                   vmm->as.IDENT.name = ID_HAS_NEXT)));
             G(loop_block, BLOCK,
               G(let_var, LET,
                 G(var, DEFPATTERN,
                   node_subs_append(var, orig_var);
                   G(g, CALL,
                     G(gm, BIN,
                       gm->as.BIN.operator = TBANG;
                       G(gmi, IDENT,
                         gmi->as.IDENT.name = node_ident(it_var));
                       G(gmm, IDENT,
                         gmm->as.IDENT.name = ID_NEXT)))))))));

      node_subs_append(let_var, orig_block);
      break;
    }
  case MATCH:
    ensure_in_block(mod, subs_first(node));
    break;
  case WHILE:
    ensure_in_block(mod, subs_first(node));

    PUSH_STATE(st->loop_state);
    st->loop_state->entry = node;
    st->loop_state->exit = next(node);
    break;
  case BREAK:
    node_set_which(node, JUMP);
    node->as.JUMP.to = st->loop_state->exit;
    node->as.JUMP.is_break = true;
    break;
  case CONTINUE:
    node_set_which(node, JUMP);
    node->as.JUMP.to = st->loop_state->entry;
    node->as.JUMP.is_continue = true;
    break;
  case IF:
    {
      ensure_in_block(mod, subs_first(node));
      struct node *block = next(subs_first(node));

      if (next(block) == NULL) {
        // No else case.
        struct node *block_noop = mk_node(mod, node, BLOCK);
        (void)mk_node(mod, block_noop, NOOP);
      } else if (next(next(block)) == NULL) {
        // Next is else case.
        // noop
      } else {
        // Next is elif.
        // As the current pass descends, the same step will convert this new IF.
        struct node *els = mk_node(mod, node, BLOCK);
        node_subs_remove(node, els);
        node_subs_insert_after(node, block, els);

        struct node *elif = mk_node(mod, els, IF);
        struct node *n = next(els);
        while (n != NULL) {
          struct node *nxt = next(n);
          node_subs_remove(node, n);
          node_subs_append(elif, n);
          n = nxt;
        }
      }
    }
    break;
  case TRY:
    PUSH_STATE(st->try_state);
    st->try_state->error = node->as.TRY.error;
    st->try_state->first_catch = subs_at(subs_last(subs_first(node)), 1);
    break;
  case CATCH:
    if (node->as.CATCH.label == ID__NONE
        && next(st->try_state->first_catch) != NULL) {
      e = mk_except(mod, node, "when multiple catch blocks are present,"
                    " they must all have a label");
      THROW(e);
    }
    break;
  case THROW:
    {
      struct node *c = NULL;
      e = find_catch(&c, mod, node, st, node->as.THROW.label);
      EXCEPT(e);

      struct node *expr = subs_last(node);
      node_subs_remove(node, expr);

      node_set_which(node, BLOCK);

      GSTART();
      G0(assign, node, BIN,
         assign->as.BIN.operator = TASSIGN;
         G(error, IDENT,
           error->as.IDENT.name = st->try_state->error);
         node_subs_append(assign, expr));
      G0(j, node, JUMP,
         j->as.JUMP.to = subs_first(c);
         j->as.JUMP.label = c->as.CATCH.label);
    }
    break;
  case LET:
    {
      const bool was_single = !subs_count_atleast(node, 2);
      if (was_single
          && (subs_first_const(node)->which == DEFNAME
              || subs_first_const(node)->which == DEFALIAS)) {
        // Already in LIR.
        break;
      }

      struct node *par = parent(node);
      struct node *before = next(node);
      const struct toplevel toplevel = node->as.LET.toplevel;

      struct node *defp = subs_first(node);
      while (defp != NULL && defp->which != BLOCK) {
        struct node *nxt = next(defp); // record next before removing defp.

        if (defp->which == DEFPATTERN) {
          e = lir_conversion_defpattern(mod, &toplevel, node->flags,
                                        par, before, defp);
          EXCEPT(e);
        }

        defp = nxt;
      }

      if (defp != NULL && defp->which == BLOCK) {
        // Move LET mutating block to after all the DEFNAMES, not
        // below them.
        node_subs_remove(node, defp);
        if (before == NULL) {
          node_subs_append(par, defp);
        } else {
          node_subs_insert_before(par, before, defp);
        }
      }

      node_set_which(node, NOOP);
    }
    break;
  case PRE:
    break;
  case POST:
    break;
  case INVARIANT:
    break;
  case EXAMPLE:
    break;
  default:
    break;
  }

  switch (node->which) {
  case DEFPATTERN:
  case EXCEP:
    assert(false && "Removed by pass higher in LIR conversion");
    break;
  default:
    break;
  }

#ifndef _NDEBUG
  // Structural invariant checks for LIR.
  switch (node->which) {
  case DEFNAME:
    assert(subs_count(node) == 2);
    break;
  case JUMP:
    assert(node->as.JUMP.to->which == BLOCK);
    break;
  case IF:
  case WHILE:
    FOREACH_SUB(s, node) {
      assert(s->which == BLOCK);
    }
    break;
  case NOOP:
    assert(!subs_count_atleast(node, 1));
    break;
  default:
    break;
  }

  assert(!(NM(node->which) & NMASK_HIR_ONLY));
#endif

  INVARIANT_NODE(node);

  return 0;
}

STEP_NM(step_lir_conversion_up,
        NM(WHILE) | NM(TRY));
error step_lir_conversion_up(struct module *mod, struct node *node,
                             void *user, bool *stop) {
  DSTEP(mod, node);

  struct lir_state *st = mod->state->lir_state;
  switch (node->which) {
  case WHILE:
    POP_STATE(st->loop_state);
    break;
  case TRY:
    POP_STATE(st->try_state);
    break;
  default:
    assert(false);
  }

  if (node->which == MODULE) {
    POP_STATE(mod->state->lir_state);
  }

  return 0;
}

static error ex_lir_conversion(struct module *mod, struct node *root) {
  void *user = NULL;
  size_t shallow_last_up = -1;
  PASS(
    DOWN_STEP(step_lir_conversion_down);
    ,
    UP_STEP(step_lir_conversion_up);
    );
  return 0;
}

EXAMPLE_NCC_EMPTY(lir_bin) {
  {
    GSTART();
    G0(n, mod->body, BIN,
      n->as.BIN.operator = Tor;
      G(cond1, IDENT);
      G(cond2, BLOCK,
        G(cond21, IDENT)));
    assert(0 == ex_lir_conversion(mod, n));
    check_structure(n,
                    "IF",
                    " BLOCK",
                    "  IDENT",
                    " BLOCK",
                    "  BOOL",
                    " BLOCK",
                    "  BLOCK",
                    "   IDENT",
                    NULL);
  }
  {
    GSTART();
    G0(n, mod->body, BIN,
      n->as.BIN.operator = Tand;
      G(cond1, IDENT);
      G(cond2, BLOCK,
        G(cond21, IDENT)));
    assert(0 == ex_lir_conversion(mod, n));
    check_structure(n,
                    "IF",
                    " BLOCK",
                    "  IDENT",
                    " BLOCK",
                    "  BLOCK",
                    "   IDENT",
                    " BLOCK",
                    "  BOOL",
                    NULL);
  }
  {
    GSTART();
    G0(n, mod->body, BIN,
      n->as.BIN.operator = Tand;
      G(cond1, BIN,
        cond1->as.BIN.operator = Tor;
        G(cond11, IDENT);
        G(cond12, BLOCK,
          G(cond121, IDENT)));
      G(cond2, BLOCK,
        G(cond21, IDENT)));
    assert(0 == ex_lir_conversion(mod, n));
    check_structure(n,
                    "IF",
                    " BLOCK",
                    "  IF",
                    "   BLOCK",
                    "    IDENT",
                    "   BLOCK",
                    "    BOOL",
                    "   BLOCK",
                    "    BLOCK",
                    "     IDENT",
                    " BLOCK",
                    "  BLOCK",
                    "   IDENT",
                    " BLOCK",
                    "  BOOL",
                    NULL);
  }
}

EXAMPLE_NCC_EMPTY(lir_if) {
  {
    GSTART();
    G0(n, mod->body, IF,
      G(cond1, IDENT);
      G(yes1, BLOCK,
        G(noop1, NOOP)));
    assert(0 == ex_lir_conversion(mod, n));
    check_structure(n,
                    "IF",
                    " BLOCK",
                    "  IDENT",
                    " BLOCK",
                    "  NOOP",
                    " BLOCK",
                    "  NOOP", NULL);
  }
  {
    GSTART();
    G0(n, mod->body, IF,
      G(cond1, IDENT);
      G(yes1, BLOCK,
        G(noop1, NOOP));
      G(yes2, BLOCK,
        G(noop2, NOOP)));
    assert(0 == ex_lir_conversion(mod, n));
    check_structure(n,
                    "IF",
                    " BLOCK",
                    "  IDENT",
                    " BLOCK",
                    "  NOOP",
                    " BLOCK",
                    "  NOOP", NULL);
  }
  {
    GSTART();
    G0(n, mod->body, IF,
      G(cond1, IDENT);
      G(yes1, BLOCK,
        G(noop1, NOOP));
      G(cond2, IDENT);
      G(yes2, BLOCK,
        G(noop2, NOOP));
      G(no, BLOCK,
        G(noop3, NOOP)););
    assert(0 == ex_lir_conversion(mod, n));
    check_structure(n,
                    "IF",
                    " BLOCK",
                    "  IDENT",
                    " BLOCK",
                    "  NOOP",
                    " BLOCK",
                    "  IF",
                    "   BLOCK",
                    "    IDENT",
                    "   BLOCK",
                    "    NOOP",
                    "   BLOCK",
                    "    NOOP",
                    NULL);
  }
}

EXAMPLE_NCC_EMPTY(lir_throw) {
  GSTART();
  G0(n, mod->body, TRY,
     G(let, LET,
       G(defp, DEFPATTERN,
         G_IDENT(err, "err"));
       G(defpb, BLOCK,
         G(b, BLOCK,
           G(t, THROW,
             G(ti, IDENT)));
         G(c, CATCH,
           G(cb, BLOCK,
             G(cn, NOOP))))));
  assert(0 == ex_lir_conversion(mod, n));
  check_structure(n,
                  "TRY",
                  " NOOP",
                  " LET",
                  "  DEFNAME",
                  "   IDENT",
                  "   INIT",
                  " BLOCK",
                  "  BLOCK",
                  "   BLOCK",
                  "    BIN",
                  "     IDENT",
                  "     IDENT",
                  "    JUMP",
                  "  CATCH",
                  "   BLOCK",
                  "    NOOP",
                  NULL);
}

EXAMPLE_NCC_EMPTY(lir_let) {
  {
    GSTART();
    G0(n, mod->body, BLOCK,
       G(let, LET,
         G(d1, DEFPATTERN,
           G(n1, IDENT));
         G(d2, DEFPATTERN,
           G(n2, IDENT);
           G(e2, IDENT));
         G(d3, DEFPATTERN,
           G(n3, IDENT);
           G(e3, BLOCK,
             G(i3, INIT)))));
    assert(0 == ex_lir_conversion(mod, n));
    check_structure(n,
                    "BLOCK",
                    " NOOP",
                    " LET",
                    "  DEFNAME",
                    "   IDENT",
                    "   INIT",
                    " LET",
                    "  DEFNAME",
                    "   IDENT",
                    "   IDENT",
                    " LET",
                    "  DEFNAME",
                    "   IDENT",
                    "   BLOCK",
                    "    INIT",
                    NULL);
  }
  {
    GSTART();
    G0(n, mod->body, BLOCK,
       G(let, LET,
         G(d1, DEFPATTERN,
           G(n1, IDENT));
         G(d2, DEFPATTERN,
           G(n2, IDENT);
           G(e2, IDENT));
         G(such, BLOCK,
           G(call, CALL,
             G(m1, IDENT);
             G(m2, IDENT))));
       G(noop, NOOP));
    assert(0 == ex_lir_conversion(mod, n));
    check_structure(n,
                    "BLOCK",
                    " NOOP",
                    " LET",
                    "  DEFNAME",
                    "   IDENT",
                    "   INIT",
                    " LET",
                    "  DEFNAME",
                    "   IDENT",
                    "   IDENT",
                    " BLOCK",
                    "  CALL",
                    "   IDENT",
                    "   IDENT",
                    " NOOP",
                    NULL);
  }
}