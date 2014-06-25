#include "passbody.h"

#include "table.h"
#include "types.h"
#include "instantiate.h"
#include "parser.h"
#include "phi.h"
#include "ssa.h"
#include "inference.h"
#include "constraints.h"
#include "topdeps.h"

#include "passzero.h"
#include "passfwd.h"

static STEP_NM(step_increment_def_name_passed,
               NM(DEFNAME) | NM(DEFALIAS));
static error step_increment_def_name_passed(struct module *mod, struct node *node,
                                            void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case DEFNAME:
    node->as.DEFNAME.passed = mod->stage->state->passing;
    break;
  case DEFALIAS:
    node->as.DEFALIAS.passed = mod->stage->state->passing;
    break;
  default:
    assert(false);
    break;
  }
  return 0;
}

static const ident operator_ident[TOKEN__NUM] = {
  [Tor] = ID_OPERATOR_OR,
  [Tand] = ID_OPERATOR_AND,
  [Tnot] = ID_OPERATOR_NOT,
  [TLE] = ID_OPERATOR_LE,
  [TLT] = ID_OPERATOR_LT,
  [TGT] = ID_OPERATOR_GT,
  [TGE] = ID_OPERATOR_GE,
  [TEQ] = ID_OPERATOR_EQ,
  [TNE] = ID_OPERATOR_NE,
  [TBWOR] = ID_OPERATOR_BWOR,
  [TBWXOR] = ID_OPERATOR_BWXOR,
  [TBWAND] = ID_OPERATOR_BWAND,
  [TLSHIFT] = ID_OPERATOR_LSHIFT,
  [TRSHIFT] = ID_OPERATOR_RSHIFT,
  [TBWOR_ASSIGN] = ID_OPERATOR_ASSIGN_BWOR,
  [TBWXOR_ASSIGN] = ID_OPERATOR_ASSIGN_BWXOR,
  [TBWAND_ASSIGN] = ID_OPERATOR_ASSIGN_BWAND,
  [TLSHIFT_ASSIGN] = ID_OPERATOR_ASSIGN_LSHIFT,
  [TRSHIFT_ASSIGN] = ID_OPERATOR_ASSIGN_RSHIFT,
  [TPLUS] = ID_OPERATOR_PLUS,
  [TMINUS] = ID_OPERATOR_MINUS,
  [TDIVIDE] = ID_OPERATOR_DIVIDE,
  [TMODULO] = ID_OPERATOR_MODULO,
  [TTIMES] = ID_OPERATOR_TIMES,
  [TPLUS_ASSIGN] = ID_OPERATOR_ASSIGN_PLUS,
  [TMINUS_ASSIGN] = ID_OPERATOR_ASSIGN_MINUS,
  [TDIVIDE_ASSIGN] = ID_OPERATOR_ASSIGN_DIVIDE,
  [TMODULO_ASSIGN] = ID_OPERATOR_ASSIGN_MODULO,
  [TTIMES_ASSIGN] = ID_OPERATOR_ASSIGN_TIMES,
  [TUMINUS] = ID_OPERATOR_UMINUS,
  [TBWNOT] = ID_OPERATOR_BWNOT,
};

static STEP_NM(step_check_no_literals_left,
               NM(NUMBER) | NM(NUL));
static error step_check_no_literals_left(struct module *mod, struct node *node,
                                         void *user, bool *stop) {
  if (typ_is_literal(node->typ)) {
    char *s = pptyp(mod, node->typ);
    error e = mk_except_type(mod, node,
                             "literal of type '%s' did not unify"
                             " to a concrete type", s);
    free(s);
    THROW(e);
  }

  return 0;
}

static bool string_literal_has_length_one(const char *s) {
  const size_t len = strlen(s);
  if (s == NULL) {
    return false;
  } else if (len <= 2) {
    return false;
  } else if (s[1] == '\\') {
    return len == 4;
  } else {
    return len == 3;
  }
}

static STEP_NM(step_weak_literal_conversion,
               NM(STRING) | NM(BOOL));
static error step_weak_literal_conversion(struct module *mod, struct node *node,
                                          void *user, bool *stop) {
  DSTEP(mod, node);

  ident id;
  struct typ *lit_typ;

  switch (node->which) {
  case STRING:
    if (typ_equal(node->typ, TBI_STATIC_STRING)) {
      return 0;
    }

    if (typ_equal(node->typ, TBI_CHAR)) {
      if (!string_literal_has_length_one(node->as.STRING.value)) {
        error e = mk_except_type(mod, node,
                                 "string literal '%s' does not have length 1,"
                                 " cannot coerce to char",
                                 node->as.STRING.value);
        THROW(e);
      }
      return 0;
    }

    id = ID_FROM_STATIC_STRING;
    lit_typ = TBI_STATIC_STRING;
    break;
  case BOOL:
    if (typ_equal(node->typ, TBI_BOOL)) {
      return 0;
    }
    id = ID_FROM_BOOL;
    lit_typ = TBI_BOOL;
    break;
  default:
    assert(false && "Unreached");
    return 0;
  }

  struct typ *saved_typ = node->typ;

  struct node *literal = node_new_subnode(mod, node);
  node_subs_remove(node, literal);
  node_move_content(literal, node);

  node_set_which(node, CALL);

  struct node *fun = mk_node(mod, node, DIRECTDEF);
  struct node *fund = node_get_member(typ_definition(saved_typ), id);
  assert(fund != NULL);
  set_typ(&fun->as.DIRECTDEF.typ, fund->typ);
  fun->as.DIRECTDEF.flags = NODE_IS_TYPE;

  node_subs_append(node, literal);
  set_typ(&literal->typ, lit_typ);

  const struct node *except[] = { literal, NULL };
  error e = catchup(mod, except, node,
                    CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static enum token_type operator_call_arg_refop(const struct typ *tfun, size_t n) {
  const struct typ *arg0 = typ_generic_functor_const(typ_function_arg_const(tfun, n));
  assert(arg0 != NULL && typ_is_reference(arg0));

  if (typ_equal(arg0, TBI_REF) || typ_equal(arg0, TBI_NREF)) {
    return TREFDOT;
  } else if (typ_equal(arg0, TBI_MREF) || typ_equal(arg0, TBI_NMREF)) {
    return TREFBANG;
  } else if (typ_equal(arg0, TBI_MMREF) || typ_equal(arg0, TBI_NMMREF)) {
    return TREFSHARP;
  } else {
    assert(false);
    return 0;
  }
}

static error gen_operator_call(struct module *mod, struct node *node,
                               ident operator_name,
                               struct node *left, struct node *right,
                               enum catchup_for catchup_for) {
  struct typ *tfun = node_get_member(typ_definition(left->typ),
                                     operator_name)->typ;

  node_set_which(node, CALL);
  struct node *fun = mk_node(mod, node, DIRECTDEF);
  set_typ(&fun->as.DIRECTDEF.typ, tfun);
  fun->as.DIRECTDEF.flags = NODE_IS_TYPE;

  const struct node *except[3] = { NULL, NULL, NULL };
  except[0] = left;
  expr_ref(mod, node, operator_call_arg_refop(tfun, 0), left);

  if (right != NULL) {
    except[1] = right;
    expr_ref(mod, node, operator_call_arg_refop(tfun, 1), right);
  }

  error e = catchup(mod, except, node, catchup_for);
  EXCEPT(e);

  return 0;
}

static STEP_NM(step_operator_call_inference,
               NM(UN) | NM(BIN));
static error step_operator_call_inference(struct module *mod, struct node *node,
                                          void *user, bool *stop) {
  DSTEP(mod, node);

  enum token_type op;
  struct node *left = NULL;
  struct node *right = NULL;

  switch (node->which) {
  case UN:
    op = node->as.UN.operator;
    left = subs_first(node);
    break;
  case BIN:
    op = node->as.BIN.operator;
    left = subs_first(node);
    right = subs_last(node);
    break;
  default:
    assert(false && "Unreached");
    return 0;
  }

  switch (OP_KIND(op)) {
  case OP_BIN_SYM_PTR:
    return 0;
  case OP_UN_BOOL:
  case OP_UN_ARITH:
  case OP_UN_BW:
  case OP_BIN:
  case OP_BIN_SYM:
  case OP_BIN_SYM_BOOL:
  case OP_BIN_SYM_ARITH:
  case OP_BIN_SYM_BW:
  case OP_BIN_BW_RHS_UNSIGNED:
    break;
  default:
    return 0;
  }

  const struct typ *l0 = typ_generic_functor_const(left->typ);
  if (typ_isa(left->typ, TBI_NATIVE_BITWISE_INTEGER)
      || typ_isa(left->typ, TBI_NATIVE_BOOLEAN)
      || typ_isa(left->typ, TBI_NATIVE_FLOATING)
      || (l0 != NULL && typ_isa(l0, TBI_ENUM))) {
    return 0;
  }

  struct node *dleft = typ_definition(left->typ);
  if (dleft->which == DEFTYPE
      && dleft->as.DEFTYPE.kind == DEFTYPE_ENUM
      && typ_isa(dleft->as.DEFTYPE.tag_typ, TBI_NATIVE_BITWISE_INTEGER)) {
    return 0;
  }

  if (operator_ident[op] == 0) {
    return 0;
  }

  node_subs_remove(node, left);
  if (right != NULL) {
    node_subs_remove(node, right);
  }
  error e = gen_operator_call(mod, node, operator_ident[op], left, right,
                              CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static STEP_NM(step_ctor_call_inference,
               NM(RETURN) | NM(BIN) | NM(DEFNAME) | NM(TYPECONSTRAINT) |
               NM(CALL) | NM(INIT));
static error step_ctor_call_inference(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);
  return 0;
}

static STEP_NM(step_array_ctor_call_inference,
               NM(INIT));
static error step_array_ctor_call_inference(struct module *mod, struct node *node,
                                            void *user, bool *stop) {
  DSTEP(mod, node);

  if (!node->as.INIT.is_array || (node->flags & NODE_IS_TYPE)) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_TRIVIAL_ARRAY_CTOR)) {
    return 0;
  }

  struct typ *saved_typ = node->typ;

  struct node *array = node_new_subnode(mod, node);
  node_subs_remove(node, array);
  node_move_content(array, node);

  node_set_which(node, CALL);

  struct typ *tfun = node_get_member(typ_definition(saved_typ), ID_FROM_ARRAY)->typ;
  set_typ(&array->typ, typ_generic_arg(typ_function_arg(tfun, 0), 0));

  GSTART();
  G0(fun, node, DIRECTDEF,
     set_typ(&fun->as.DIRECTDEF.typ, tfun);
     fun->as.DIRECTDEF.flags = NODE_IS_TYPE);
  G0(ref_array, node, UN,
     ref_array->as.UN.operator = TREFDOT;
     node_subs_append(ref_array, array));

  const struct node *except[] = { array, NULL };
  error e = catchup(mod, except, node, CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static STEP_NM(step_dtor_call_inference,
               NM(BLOCK));
static error step_dtor_call_inference(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);
  // FIXME
  return 0;
}

static bool expr_is_literal_initializer(struct node **expr, struct module *mod, struct node *node) {
  if (node->which == INIT) {
    if (expr != NULL) {
      *expr = node;
    }
    return true;
  } else if (node->which == BLOCK) {
    return expr_is_literal_initializer(expr, mod, subs_last(node));
  } else {
    return false;
  }
}

static bool expr_is_return_through_ref(struct node **expr, struct module *mod, struct node *node) {
  if (typ_isa(node->typ, TBI_RETURN_BY_COPY)) {
    return false;
  }

  if (expr_is_literal_initializer(expr, mod, node)) {
    return true;
  } else if (node->which == CALL) {
    *expr = node;
    return true;
  } else if (node->which == BLOCK) {
    return expr_is_return_through_ref(expr, mod, subs_last(node));
  } else {
    return false;
  }
}

static error assign_copy_call_inference(struct module *mod, struct node *node) {
  struct node *left = subs_first(node);
  struct node *right = subs_at(node, 1);

  node_subs_remove(node, left);
  node_subs_remove(node, right);

  error e = gen_operator_call(mod, node, ID_COPY_CTOR, left, right,
                              CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);
  return 0;
}

static error defname_copy_call_inference(struct module *mod, struct node *node) {
  struct node *left = mk_node(mod, node, IDENT);
  left->as.IDENT.name = node_ident(node);
  node_subs_remove(node, left);

  struct node *right = subs_last(node);
  node_subs_remove(node, right); // steal defname expression
  struct node *init = mk_node(mod, node, INIT);
  set_typ(&init->typ, right->typ);

  struct node *within = parent(parent(node));
  struct node *copycall = node_new_subnode(mod, within);
  node_subs_remove(within, copycall);
  node_subs_insert_after(within, parent(node), copycall);

  // We need 'left' to be caught up.
  node_subs_append(copycall, left);
  error e = catchup(mod, NULL, left, CATCHUP_AFTER_CURRENT);
  assert(!e);
  node_subs_remove(copycall, left);

  e = gen_operator_call(mod, copycall, ID_COPY_CTOR, left, right,
                        CATCHUP_AFTER_CURRENT);
  EXCEPT(e);

  return 0;
}

static STEP_NM(step_copy_call_inference,
               NM(BIN) | NM(DEFNAME));
static error step_copy_call_inference(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);
  struct node *left;
  struct node *right;
  switch (node->which) {
  case BIN:
    if (node->as.BIN.operator == TASSIGN) {
      left = subs_first(node);
      right = subs_last(node);
      break;
    }
    return 0;
  case DEFNAME:
    if (node->as.DEFNAME.ssa_user != NULL) {
      return 0;
    }
    left = node;
    right = subs_last(node);
    if (right != NULL && right->which != INIT) {
      break;
    }
    return 0;
  default:
    assert(false && "Unreached");
    return 0;
  }

  if ((right->flags & NODE_IS_TEMPORARY)) {
    // It's OK to trivial copy temporaries: it's a move.
    return 0;
  }

  if (typ_isa(left->typ, TBI_TRIVIAL_COPY)) {
    return 0;
  }

  if (expr_is_return_through_ref(NULL, mod, right)) {
    return 0;
  }

  error e = typ_check_isa(mod, right, right->typ, TBI_COPYABLE);
  EXCEPT(e);

  switch (node->which) {
  case BIN:
    e = assign_copy_call_inference(mod, node);
    break;
  case DEFNAME:
    e = defname_copy_call_inference(mod, node);
    break;
  default:
    assert(false);
  }
  EXCEPT(e);
  return 0;
}

static bool need_insert_dyn(struct module *mod,
                            const struct typ *intf,
                            const struct typ *concrete) {
  return typ_is_dyn(intf) && typ_is_dyn_compatible(concrete);
}

static error insert_dyn(struct node **src,
                        struct module *mod, struct node *node,
                        struct typ *target) {
  struct node *d = mk_node(mod, node, DYN);
  set_typ(&d->as.DYN.intf_typ, target);

  node_subs_remove(node, d);
  node_subs_replace(node, *src, d);
  node_subs_append(d, *src);

  const struct node *except[] = { *src, NULL };
  error e = catchup(mod, except, d, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  *src = d;

  return 0;
}

static error try_insert_dyn(struct node **src,
                            struct module *mod, struct node *node,
                            struct typ *target) {
  if (!need_insert_dyn(mod, target, (*src)->typ)) {
    return 0;
  }

  error e = insert_dyn(src, mod, node, target);
  EXCEPT(e);
  return 0;
}

static STEP_NM(step_dyn_inference,
               NM(RETURN) | NM(BIN) | NM(DEFNAME) | NM(TYPECONSTRAINT) |
               NM(CALL) | NM(INIT));
static error step_dyn_inference(struct module *mod, struct node *node,
                                void *user, bool *stop) {
  DSTEP(mod, node);
  const struct node *target;
  struct node *src;

  error e;
  switch (node->which) {
  case RETURN:
    if (!subs_count_atleast(node, 1)) {
      return 0;
    }
    target = module_retval_get(mod);
    src = subs_first(node);

    e = try_insert_dyn(&src, mod, node, target->typ);
    EXCEPT(e);
    return 0;
  case BIN:
    if (node->as.BIN.operator == TASSIGN) {
      target = subs_first(node);
      src = subs_at(node, 1);
      e = try_insert_dyn(&src, mod, node, target->typ);
      EXCEPT(e);
    }
    return 0;
  case DEFNAME:
    if (!(node->flags & NODE_IS_TYPE)) {
      target = subs_first(node);
      src = subs_last(node);
      if (src != NULL) {
        e = try_insert_dyn(&src, mod, node, target->typ);
        EXCEPT(e);
      }
    }
    return 0;
  case TYPECONSTRAINT:
    target = subs_first(node);
    src = subs_last(node);
    e = try_insert_dyn(&src, mod, node, target->typ);
    EXCEPT(e);
    return 0;
  case CALL:
    if (node->flags & NODE_IS_TYPE) {
      return 0;
    }

#define GET_TYP(target) \
    ( (target)->as.DEFARG.is_vararg \
      ? typ_generic_arg((target)->typ, 0) : (target)->typ)

    const struct node *funargs = subs_at_const(
      typ_definition_const(subs_first_const(node)->typ), IDX_FUNARGS);
    const struct node *target = subs_first_const(funargs);

    FOREACH_SUB_EVERY(src, node, 1, 1) {
      e = try_insert_dyn(&src, mod, node, GET_TYP(target));
      EXCEPT(e);

      if (!target->as.DEFARG.is_vararg) {
        target = next_const(target);
      }
    }

#undef GET_TYP

    return 0;
  case INIT:
    // FIXME: support missing
    return 0;
  default:
    assert(false && "Unreached");
    return 0;
  }
}

static bool is_block_like(struct node *node) {
  switch (node->which) {
  case IF:
  case TRY:
  case MATCH:
  case BLOCK:
    return true;
  default:
    return false;
  }
}

static void block_insert_value_assign(struct module *mod, struct node *block,
                                      struct node *target, ident target_name) {
  assert(block->which == BLOCK);

  struct node *last = subs_last(block);
  while (last->typ == TBI__NOT_TYPEABLE) {
    last = prev(last);
  }

  struct node *assign = mk_node(mod, block, BIN);
  assign->as.BIN.operator = TASSIGN;
  if (target != NULL) {
    node_subs_append(assign, target);
  } else {
    struct node *left = mk_node(mod, assign, IDENT);
    left->as.IDENT.name = target_name;
  }
  node_subs_remove(block, assign);
  node_subs_replace(block, last, assign);

  node_subs_append(assign, last);
  set_typ(&block->typ, TBI_VOID);

  const struct node *except[] = { last, NULL };
  error e = catchup(mod, except, assign, CATCHUP_BELOW_CURRENT);
  assert(!e);
}

static void block_like_insert_value_assign(struct module *mod, struct node *node,
                                           struct node *target, ident target_name) {
  struct node *last_elif = NULL;
  switch (node->which) {
  case IF:
    FOREACH_SUB_EVERY(b, node, 1, 2) {
      block_insert_value_assign(mod, b, target, target_name);
      last_elif = b;
    }
    if (last_elif != subs_last(node)) {
      struct node *els = subs_last(node);
      block_insert_value_assign(mod, els, target, target_name);
    }
    break;
  case TRY:
    block_insert_value_assign(mod, subs_first(node), target, target_name);
    block_insert_value_assign(mod, subs_at(node, 2), target, target_name);
    // FIXME: other CATCH blocks?
    break;
  case MATCH:
    FOREACH_SUB_EVERY(b, node, 2, 2) {
      block_insert_value_assign(mod, b, target, target_name);
    }
    break;
  case BLOCK:
    block_insert_value_assign(mod, node, target, target_name);
    break;
  default:
    assert(false);
    break;
  }
}

static const struct node *retval_name(struct module *mod) {
  const struct node *retval = module_retval_get(mod);
  assert(subs_count_atleast(retval, 1));
  return subs_first_const(retval);
}

static STEP_NM(step_store_return_through_ref_expr,
               NM(RETURN) | NM(DEFNAME) | NM(BIN));
static error step_store_return_through_ref_expr(struct module *mod, struct node *node,
                                                void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *expr = NULL;
  struct node *real_expr = NULL;

  switch (node->which) {
  case RETURN:
    if (!subs_count_atleast(node, 1)
        || typ_equal(subs_first(node)->typ, TBI_VOID)) {
      return 0;
    }

    expr = subs_first(node);
    if (expr_is_return_through_ref(&real_expr, mod, expr)) {
      // Keep node->as.RETURN.return_through_ref_expr null as the
      // subexpression CALL or INIT will directly write to it.
      if (real_expr != NULL) {
        if (real_expr->which == INIT) {
          real_expr->as.INIT.target_expr = retval_name(mod);
          node->as.RETURN.forced_return_through_ref = true;
        } else if (real_expr->which == CALL) {
          expr->as.CALL.return_through_ref_expr = retval_name(mod);
        }
      } else if (is_block_like(expr)) {
        block_like_insert_value_assign(mod, expr, NULL, node_ident(retval_name(mod)));
      } else {
        assert(false);
      }
    } else if (!typ_isa(expr->typ, TBI_RETURN_BY_COPY)
               && typ_isa(expr->typ, TBI_COPYABLE)) {
      // FIXME need to insert copy_ctor

      if (expr->which == IDENT
          && node_ident(retval_name(mod)) == node_ident(expr)) {
        // noop
      } else if (is_block_like(expr)) {
        block_like_insert_value_assign(mod, expr, NULL, node_ident(retval_name(mod)));

        node->as.RETURN.return_through_ref_expr = retval_name(mod);
      } else {
        node->as.RETURN.return_through_ref_expr = retval_name(mod);
      }
    }
    return 0;
  case DEFNAME:
    expr = next(subs_first(node));
    if (expr == NULL) {
      // noop
    } else if (expr_is_literal_initializer(&real_expr, mod, expr)) {
      real_expr->as.INIT.target_expr = subs_first(node);
    } else if (expr_is_return_through_ref(&real_expr, mod, expr)) {
      real_expr->as.CALL.return_through_ref_expr = subs_first(node);
    }
    return 0;
  case BIN:
    if (!OP_IS_ASSIGN(node->as.BIN.operator)) {
      return 0;
    }
    struct node *left = subs_first(node);
    struct node *right = subs_last(node);
    if (expr_is_literal_initializer(&real_expr, mod, right)) {
      real_expr->as.INIT.target_expr = left;
    } else if (expr_is_return_through_ref(&real_expr, mod, right)) {
      real_expr->as.CALL.return_through_ref_expr = left;
    }
    return 0;
  default:
    assert(false && "Unreached");
    return 0;
  }
}

static  error add_dyn_topdep_each(struct module *mod, struct typ *t, struct typ *intf,
                                  bool *stop, void *user) {
  struct node *r = NULL;
  error e = reference(&r, mod, NULL, TREFDOT, intf);
  assert(!e);
  topdeps_record(mod, r->typ);

  e = reference(&r, mod, NULL, TREFBANG, intf);
  assert(!e);
  topdeps_record(mod, r->typ);

  e = reference(&r, mod, NULL, TREFSHARP, intf);
  assert(!e);
  topdeps_record(mod, r->typ);
  return 0;
}

static STEP_NM(step_add_dyn_topdep,
               NM(DEFTYPE));
static error step_add_dyn_topdep(struct module *mod, struct node *node,
                                 void *user, bool *stop) {
  DSTEP(mod, node);

  if (typ_is_reference(node->typ)) {
    return 0;
  }

  typ_isalist_foreach(mod, node->typ, ISALIST_FILTEROUT_PREVENT_DYN,
                      add_dyn_topdep_each, NULL);
  return 0;
}

error passbody0(struct module *mod, struct node *root,
                void *user, ssize_t shallow_last_up) {
  // first
  PASS(
    DOWN_STEP(step_push_state);
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_stop_already_early_typing);
    DOWN_STEP(step_rewrite_wildcards);
    DOWN_STEP(step_type_destruct_mark);
    DOWN_STEP(step_type_mutability_mark);
    DOWN_STEP(step_type_gather_retval);
    DOWN_STEP(step_type_gather_excepts);
    ,
    UP_STEP(step_type_inference);
    UP_STEP(step_remove_typeconstraints);
    UP_STEP(step_type_drop_excepts);
    UP_STEP(step_gather_remaining_weakly_concrete);
    ,
    FINALLY_STEP(step_pop_state);
    );
    return 0;
}

static error passbody1(struct module *mod, struct node *root,
                       void *user, ssize_t shallow_last_up) {
  // second
  PASS(
    DOWN_STEP(step_push_state);
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_type_gather_retval);
    DOWN_STEP(step_check_no_literals_left);
    ,
    UP_STEP(step_insert_nullable_void);
    UP_STEP(step_weak_literal_conversion);
    UP_STEP(step_operator_call_inference);
    UP_STEP(step_ctor_call_inference);
    UP_STEP(step_array_ctor_call_inference);
    UP_STEP(step_dtor_call_inference);
    UP_STEP(step_copy_call_inference);
    UP_STEP(step_dyn_inference);

    UP_STEP(step_store_return_through_ref_expr);
    UP_STEP(step_add_dyn_topdep);
    ,
    FINALLY_STEP(step_pop_state);
    );
    return 0;
}

static error passbody2(struct module *mod, struct node *root,
                       void *user, ssize_t shallow_last_up) {
  // second
  PASS(
    DOWN_STEP(step_push_state);
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_type_gather_retval);
    DOWN_STEP(step_ident_non_local_scope);
    DOWN_STEP(step_branching_down);
    DOWN_STEP(step_branching_block_down);
    DOWN_STEP(step_branching_block_down_phi);
    ,
    UP_STEP(step_track_ident_use);
    UP_STEP(step_increment_def_name_passed);

    UP_STEP(step_branching_block_up_phi);
    UP_STEP(step_branching_up);
    ,
    FINALLY_STEP(step_pop_state);
    );
    return 0;
}

a_pass passbody[] = { passbody0, passbody1, passbody2 };
