#include "lir.h"

#include "passes.h"
#include "parser.h"
#include "types.h"

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

static void rewrite_ptr_op(struct module *mod, struct node *node) {
  assert(node->which == BIN);
  switch (node->as.BIN.operator) {
  case Telse:
    {
      struct node *a = subs_first(node);
      struct node *b = subs_last(node);
      node_subs_remove(node, a);
      node_subs_remove(node, b);

      node_set_which(node, BLOCK);
      GSTART();
      G0(let, node, LET,
         G(defp, DEFPATTERN,
           G(tmpa, IDENT,
             tmpa->as.IDENT.name = gensym(mod));
           node_subs_append(defp, a)));
      G0(xif, node, IF,
         G(cond, UN,
           cond->as.UN.operator = TPOSTQMARK;
           G(tmpa2, IDENT,
             tmpa2->as.IDENT.name = node_ident(tmpa)));
         G(yes, BLOCK,
           G(da, UN,
             da->as.UN.operator = T__DEOPT_DEREFDOT;
             G(tmpa3, IDENT,
               tmpa3->as.IDENT.name = node_ident(tmpa))));
         G(no, BLOCK,
           node_subs_append(no, b)));
    }
    break;
  default:
    break;
  }
}

static void rewrite_opt_acc_op(struct module *mod, struct node *node) {
  assert(node->which == BIN);
  enum token_type op = node->as.BIN.operator, accop, refop;
  switch (op) {
  case TOPTDOT:
    accop = TDOT;
    refop = TREFDOT;
    break;
  case TOPTBANG:
    accop = TBANG;
    refop = TREFBANG;
    break;
  case TOPTSHARP:
    accop = TSHARP;
    refop = TREFSHARP;
    break;
  case TOPTATDOT:
    accop = TATDOT;
    refop = TREFDOT;
    break;
  case TOPTATBANG:
    accop = TATBANG;
    refop = TREFBANG;
    break;
  default:
    assert(false);
  }

  struct node *left = subs_first(node);
  struct node *right = subs_last(node);
  node_subs_remove(node, left);
  node_subs_remove(node, right);

  node_set_which(node, BLOCK);
  GSTART();
  G0(let, node, LET,
     G(defp, DEFPATTERN,
       G(v, IDENT,
         v->as.IDENT.name = gensym(mod));
       node_subs_append(defp, left));
     G(letblock, BLOCK,
       G(ifisnotnil, IF,
         G(isnotnil, UN,
           isnotnil->as.UN.operator = TPOSTQMARK;
           G(v2, IDENT,
             v2->as.IDENT.name = node_ident(v)));
         G(yes, BLOCK,
           G(nillable, UN,
             nillable->as.UN.operator = T__NULLABLE;
             G(ref, UN,
               ref->as.UN.operator = refop;
               G(acc, BIN,
                 acc->as.BIN.operator = accop;
                 G(v3, IDENT,
                   v3->as.IDENT.name = node_ident(v));
                 node_subs_append(acc, right)))));
         G(no, BLOCK,
           G(nil, NIL)))));
}

static ERROR rewrite_tuple_assign(struct module *mod, struct node *node) {
  assert(node->which == BIN);
  struct node *left = subs_first(node);
  assert(left->which == TUPLE);
  struct node *right = subs_last(node);

  if (right->which == TUPLE) {
    if (subs_count(left) != subs_count(right)) {
      error e = mk_except(mod, node, "mismatched tuple sizes in assignment");
      THROW(e);
    }

    node_subs_remove(node, left);
    node_subs_remove(node, right);
    node_set_which(node, BLOCK);
    GSTART();

    struct node *l, *r;
    while ((l = subs_first(left)) != NULL && (r = subs_first(right)) != NULL) {
      node_subs_remove(left, l);
      node_subs_remove(right, r);

      G0(ass, node, BIN,
         ass->as.BIN.operator = TASSIGN;
         node_subs_append(ass, l);
         node_subs_append(ass, r));
    }
  } else {
    node_subs_remove(node, left);
    node_subs_remove(node, right);
    node_set_which(node, BLOCK);
    GSTART();

    G0(let, node, LET,
       G(defp, DEFPATTERN,
         G(tmp, IDENT,
           tmp->as.IDENT.name = gensym(mod));
         node_subs_append(defp, right)));

    size_t n = 0;
    struct node *l;
    while ((l = subs_first(left)) != NULL) {
      node_subs_remove(left, l);

      char buf[8] = { 0 };
      snprintf(buf, ARRAY_SIZE(buf), "X%zu", n);
      const ident x = idents_add_string(mod->gctx, buf, strlen(buf));

      G0(ass, node, BIN,
         ass->as.BIN.operator = TASSIGN;
         node_subs_append(ass, l);
         G(r, BIN,
           r->as.BIN.operator = TDOT;
           G(ntmp, IDENT,
             ntmp->as.IDENT.name = node_ident(tmp));
           G(nx, IDENT,
             nx->as.IDENT.name = x)));

      n += 1;
    }
  }

  return 0;
}

static ERROR rewrite_excep(struct module *mod, struct lir_state *st,
                           struct node *par, struct node *before,
                           struct node *excep, struct node *expr) {
  GSTART();
  G0(i, par, IF,
     i->codeloc = expr->codeloc);
  if (before != NULL) {
    node_subs_remove(par, i);
    node_subs_insert_before(par, before, i);
  }

  G0(test_block, i, BLOCK,
     G(test, CALL,
       G(op, BIN,
         op->as.BIN.operator = TDOT;
         G(constraint, TYPECONSTRAINT,
           node_subs_append(constraint, expr);
           G(error, DIRECTDEF,
             set_typ(&error->as.DIRECTDEF.typ, TBI_ERROR)));
         G(op_name, IDENT,
           op_name->as.IDENT.name = ID_OPERATOR_TEST))));

  G0(yes, i, BLOCK);
  if (st->try_state == NULL) {
    // Then except is a conditional return.
    G0(th, yes, RETURN,
       th->as.RETURN.is_flexible_except = true;
       G(e, IDENT,
         e->as.IDENT.name = node_ident(expr)));
  } else {
    G0(th, yes, THROW,
       th->as.THROW.label = excep->as.EXCEP.label;
       G(e, IDENT,
         e->as.IDENT.name = node_ident(expr)));
  }
  return 0;
}

static ERROR find_catch(struct node **r,
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
  let->codeloc = expr->codeloc;
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

static ident create_globalenv_header(struct module *mod, uint32_t flags,
                                     const struct toplevel *toplevel,
                                     struct node *next_let,
                                     struct node *id, struct node *expr) {
  static const char POSTFIX[] = "_$Nenvheader";
  const char *base = idents_value(mod->gctx, node_ident(id));
  char *s = calloc(strlen(base) + ARRAY_SIZE(POSTFIX), sizeof(char));
  sprintf(s, "%s%s", base, POSTFIX);
  const ident name = idents_add_string(mod->gctx, s, strlen(s));
  free(s);

  struct node *let_block = parent(next_let);
  GSTART();
  G0(let, let_block, LET,
     let->codeloc = id->codeloc;
     let->as.LET.toplevel = *toplevel;
     let->flags = flags;
     G(defn, DEFNAME,
       defn->flags = flags;
       G(n, IDENT,
         n->as.IDENT.name = name);
       G(typc, TYPECONSTRAINT,
         G(exprh, INIT);
         G(rienvh, UN,
           rienvh->as.UN.operator = TREFSHARP;
           G(ienvh, CALL,
             G_IDENT(envh, "Envheader");
             G(rexpr, 0,
               node_deepcopy(mod, rexpr, subs_last(expr))))))));
  node_subs_remove(let_block, let);
  node_subs_insert_before(let_block, next_let, let);
  return name;
}

static ERROR extract_defnames(struct module *mod, struct lir_state *st,
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
      let->codeloc = pattern->codeloc;
      let->as.LET.toplevel = *toplevel;
      let->flags = flags;
      if (before != NULL) {
        node_subs_remove(let_block, let);
        node_subs_insert_before(let_block, before, let);
      }

      struct node *def = NULL;
      if (is_alias) {
        def = mk_node(mod, let, DEFALIAS);
        def->flags = flags;
        node_subs_append(def, pattern);
        if (expr == NULL) {
          expr = mk_node(mod, def, IDENT);
          expr->as.IDENT.name = ID_TBI_ANY;
        } else {
          node_subs_append(def, expr);
        }
      } else {
        ident globalenv_header_name = ID__NONE;
        if (is_globalenv){
          globalenv_header_name = create_globalenv_header(mod, flags, toplevel,
                                                          let, pattern, expr);
        }

        def = mk_node(mod, let, DEFNAME);
        def->flags = flags;
        def->as.DEFNAME.is_globalenv = is_globalenv;
        def->as.DEFNAME.globalenv_header_name = globalenv_header_name;
        node_subs_append(def, pattern);
        if (expr == NULL) {
          expr = mk_node(mod, def, INIT);
        } else {
          node_subs_append(def, expr);
        }
      }
      return 0;
    }
    break;

  case EXCEP:
    {
      if (expr == NULL) {
        e = mk_except(mod, pattern, "except must be used with an expression");
        THROW(e);
      }

      e = rewrite_excep(mod, st, let_block, before, pattern, expr);
      EXCEPT(e);
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
          e = extract_defnames(mod, st, toplevel, flags, is_alias, is_globalenv,
                               let_block, before, t, NULL);
          EXCEPT(e);
        } else if (expr->which == TUPLE) {
          e = extract_defnames(mod, st, toplevel, flags, is_alias, is_globalenv,
                               let_block, before, t, subs_at(expr, n));
          EXCEPT(e);
        } else {
          assert(expr->which == IDENT);

          struct node *ex = mk_node(mod, let_block, BIN);
          ex->codeloc = expr->codeloc;
          ex->as.BIN.operator = TDOT;
          node_subs_remove(let_block, ex);

          struct node *name = mk_node(mod, ex, IDENT);
          name->as.IDENT.name = node_ident(expr);
          struct node *field = mk_node(mod, ex, IDENT);
          char s[8] = { 0 };
          snprintf(s, ARRAY_SIZE(s), "X%zu", n);
          field->as.IDENT.name = idents_add_string(mod->gctx, s, strlen(s));

          e = extract_defnames(mod, st, toplevel, flags, is_alias, is_globalenv,
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

      e = extract_defnames(mod, st, toplevel, flags, is_alias, is_globalenv,
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

static ERROR lir_conversion_defpattern(struct module *mod, struct lir_state *st,
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

  error e = extract_defnames(mod, st, toplevel, flags, is_alias, is_globalenv,
                             let_block, before, pattern, expr);
  EXCEPT(e);

  return 0;
}

static void lir_conversion_foreach(struct module *mod, struct node *node) {
  node_set_which(node, BLOCK);

  struct node *orig_var = subs_first(node);
  struct node *orig_expr = subs_at(node, 1);
  struct node *orig_block = subs_last(node);
  node_subs_remove(node, orig_var);
  node_subs_remove(node, orig_expr);
  node_subs_remove(node, orig_block);

  GSTART();
  G0(let_it, node, LET,
     G(expr, DEFPATTERN,
       G(expr_var, IDENT,
         expr_var->as.IDENT.name = gensym(mod));
       G(expr_var_call, CALL,
         G_IDENT(expr_var_fun, "Take_ref_if_value__"))
         node_subs_append(expr_var_call, orig_expr));
     G(it, DEFPATTERN,
       G(it_var, IDENT,
         it_var->as.IDENT.name = gensym(mod));
       G(it_expr, CALL,
         G(it_expr_iter, BIN,
           it_expr_iter->as.BIN.operator = TDOT;
           G(it_expr_all, BIN,
             it_expr_all->as.BIN.operator = TDOT;
             G(it_expr_self, IDENT,
               it_expr_self->as.IDENT.name = node_ident(expr_var));
             G_IDENT(it_expr_all_fun, "All"));
           G_IDENT(it_expr_fun, "Iter"))));

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
             G(idx, DEFPATTERN,
               G(idx_var, IDENT,
                 idx_var->as.IDENT.name = gensym(mod));
               G(g, CALL,
                 G(gm, BIN,
                   gm->as.BIN.operator = TBANG;
                   G(gmi, IDENT,
                     gmi->as.IDENT.name = node_ident(it_var));
                   G(gmm, IDENT,
                     gmm->as.IDENT.name = ID_NEXT))));
             G(var, DEFPATTERN,
               node_subs_append(var, orig_var);
               G(g2, CALL,
                 G(gm2, BIN,
                   gm2->as.BIN.operator = TDOT;
                   G(gmi2, IDENT,
                     gmi2->as.IDENT.name = node_ident(expr_var));
                   G_IDENT(gmm2, "Operator_at"));
                 G(gma2, IDENT,
                   gma2->as.IDENT.name = node_ident(idx_var)))))))));

         node_subs_append(let_var, orig_block);
}

static void lir_conversion_for(struct module *mod, struct node *node) {
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
}

static void ensure_in_block(struct module *mod, struct node *x) {
  struct node *node = parent(x);

  struct node *x_block = mk_node(mod, node, BLOCK);
  x_block->codeloc = x->codeloc;
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
  case STRING:
    node->flags |= NODE_IS_LOCAL_STATIC_CONSTANT;
    break;
  case TYPECONSTRAINT:
    node->flags |= subs_first_const(node)->flags & NODE_IS_LOCAL_STATIC_CONSTANT;
    break;
  case UN:
    break;
  case BIN:
    {
      const struct node *left = subs_first_const(node);
      enum token_type op = node->as.BIN.operator;
      switch (OP_KIND(op)) {
      case OP_BIN_SYM_BOOL:
        rewrite_bool_op(mod, node);
        break;
      case OP_BIN_SYM_PTR:
        rewrite_ptr_op(mod, node);
        break;
      case OP_BIN_OPT_ACC:
        rewrite_opt_acc_op(mod, node);
        break;
      default:
        if (op == TASSIGN && left->which == TUPLE) {
          e = rewrite_tuple_assign(mod, node);
          EXCEPT(e);
        } else if (op == TASSIGN && left->which == EXCEP) {
          struct node *excep = subs_first(node);
          struct node *expr = subs_last(node);
          node_subs_remove(node, excep);
          node_subs_remove(node, expr);

          node_set_which(node, LET);
          GSTART();
          G0(defn, node, DEFNAME,
             G(tmp, IDENT,
               tmp->as.IDENT.name = gensym(mod))
             node_subs_append(defn, expr));

          struct node *before = next(node);
          G0(block, parent(node), BLOCK);
          if (before != NULL) {
            node_subs_remove(parent(node), block);
            node_subs_insert_before(parent(node), before, block);
          }

          G0(tmp2, node, IDENT, tmp2->as.IDENT.name = node_ident(tmp));
          node_subs_remove(node, tmp2);

          e = rewrite_excep(mod, st, block, NULL, excep, tmp2);
          EXCEPT(e);
        } else if (op == Tin) {
          GSTART();
          struct node *el = subs_first(node);
          struct node *co = subs_last(node);
          node_subs_remove(node, el);
          node_subs_remove(node, co);
          node_set_which(node, CALL);
          G0(m, node, BIN,
             m->as.BIN.operator = TDOT;
             node_subs_append(m, co);
             G_IDENT(f, "Operator_in"));
          node_subs_append(node, el);
        }
      }
    }
    break;
  case DEFNAME:
    {
      struct node *expr = subs_last(node);
      if (NM(expr->which) & (NM(IF) | NM(WHILE) | NM(FOR)
                             | NM(MATCH) | NM(TRY) | NM(LET))) {
        ensure_in_block(mod, expr);
      }
    }
  case CALL:
    if (node_ident(subs_first(node)) == ID_NULLABLE) {
      node_set_which(node, UN);
      node_subs_remove(node, subs_first(node));
      node->as.UN.operator = T__NULLABLE;
    }
    break;
  case FOR:
    if (node->as.FOR.is_foreach) {
      lir_conversion_foreach(mod, node);
    } else {
      lir_conversion_for(mod, node);
    }
    break;
  case MATCH:
    ensure_in_block(mod, subs_first(node));
    break;
  case WHILE:
    ensure_in_block(mod, subs_first(node));

    PUSH_STATE(st->loop_state);
    st->loop_state->entry = subs_first(node);
    if (next(node) == NULL) {
      GSTART();
      G0(noop, parent(node), NOOP);
      st->loop_state->exit = noop;
    } else {
      st->loop_state->exit = next(node);
    }
    break;
  case BREAK:
    node_set_which(node, JUMP);
    node->as.JUMP.is_break = true;
    break;
  case CONTINUE:
    node_set_which(node, JUMP);
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
          e = lir_conversion_defpattern(mod, st, &toplevel, node->flags,
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

static ERROR ex_lir_conversion(struct module *mod, struct node *root) {
  void *user = NULL;
  size_t shallow_last_up = -1;
  PASS(
    DOWN_STEP(step_lir_conversion_down);
    ,
    UP_STEP(step_lir_conversion_up);
    ,
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
  {
    GSTART();
    G0(n, mod->body, TRY,
       G(elet, LET,
         G(edefp, DEFPATTERN,
           G_IDENT(err, "err"));
         G(defpb, BLOCK,
           G(b, BLOCK,
             G(let, LET,
               G(d1, DEFPATTERN,
                 G(n1, EXCEP);
                 G_IDENT(n2, "OK")));
             G(ilet, LET,
               G(id1, DEFPATTERN,
                 G(in1, EXCEP);
                 G_IDENT(in2, "INVAL"))));
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
                    "   NOOP",
                    "   LET",
                    "    DEFNAME",
                    "     IDENT",
                    "     IDENT",
                    "   IF",
                    "    BLOCK",
                    "     BLOCK",
                    "      CALL",
                    "       BIN",
                    "        TYPECONSTRAINT",
                    "         IDENT",
                    "         DIRECTDEF",
                    "        IDENT",
                    "    BLOCK",
                    "     BLOCK",
                    "      BIN",
                    "       IDENT",
                    "       IDENT",
                    "      JUMP",
                    "    BLOCK",
                    "     NOOP",
                    "   NOOP",
                    "   LET",
                    "    DEFNAME",
                    "     IDENT",
                    "     IDENT",
                    "   IF",
                    "    BLOCK",
                    "     BLOCK",
                    "      CALL",
                    "       BIN",
                    "        TYPECONSTRAINT",
                    "         IDENT",
                    "         DIRECTDEF",
                    "        IDENT",
                    "    BLOCK",
                    "     BLOCK",
                    "      BIN",
                    "       IDENT",
                    "       IDENT",
                    "      JUMP",
                    "    BLOCK",
                    "     NOOP",
                    "  CATCH",
                    "   BLOCK",
                    "    NOOP",
                    NULL);
  }
}
