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
#include "unify.h"

#include "passzero.h"
#include "passfwd.h"

static STEP_NM(step_increment_def_name_passed,
               NM(DEFNAME) | NM(DEFALIAS));
static ERROR step_increment_def_name_passed(struct module *mod, struct node *node,
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
  [TOVLSHIFT] = ID_OPERATOR_OVLSHIFT,
  [TRSHIFT] = ID_OPERATOR_RSHIFT,
  [TBWOR_ASSIGN] = ID_OPERATOR_ASSIGN_BWOR,
  [TBWXOR_ASSIGN] = ID_OPERATOR_ASSIGN_BWXOR,
  [TBWAND_ASSIGN] = ID_OPERATOR_ASSIGN_BWAND,
  [TOVLSHIFT_ASSIGN] = ID_OPERATOR_ASSIGN_OVLSHIFT,
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
  [TOVPLUS] = ID_OPERATOR_OVPLUS,
  [TOVMINUS] = ID_OPERATOR_OVMINUS,
  [TOVDIVIDE] = ID_OPERATOR_OVDIVIDE,
  [TOVMODULO] = ID_OPERATOR_OVMODULO,
  [TOVTIMES] = ID_OPERATOR_OVTIMES,
  [TOVPLUS_ASSIGN] = ID_OPERATOR_ASSIGN_OVPLUS,
  [TOVMINUS_ASSIGN] = ID_OPERATOR_ASSIGN_OVMINUS,
  [TOVDIVIDE_ASSIGN] = ID_OPERATOR_ASSIGN_OVDIVIDE,
  [TOVMODULO_ASSIGN] = ID_OPERATOR_ASSIGN_OVMODULO,
  [TOVTIMES_ASSIGN] = ID_OPERATOR_ASSIGN_OVTIMES,
  [TUMINUS] = ID_OPERATOR_UMINUS,
  [TBWNOT] = ID_OPERATOR_BWNOT,
};

static STEP_NM(step_check_no_literals_left,
               NM(NUMBER) | NM(NIL));
static ERROR step_check_no_literals_left(struct module *mod, struct node *node,
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

// Finish the job from unify_with_defincomplete_entrails().
static STEP_NM(step_init_insert_automagic,
               NM(INIT));
static ERROR step_init_insert_automagic(struct module *mod, struct node *node,
                                        void *user, bool *stop) {
  if (node->as.INIT.is_array
      || node->as.INIT.is_range
      || node->as.INIT.is_bounds
      || node->as.INIT.is_defchoice_external_payload_constraint
      // Already handled in type_inference_typeconstraint_defchoice_init.
      || node->as.INIT.for_tag != ID__NONE) {
    return 0;
  }

  error e;
  struct node *nxt = subs_first(node);
  while (nxt != NULL) {
    struct node *f = nxt;
    struct node *expr = next(f);
    nxt = next(expr); // Save now, we may move expr.

    struct tit *tf = typ_definition_one_member(node->typ, node_ident(f));
    struct typ *target = tit_typ(tf);
    struct typ *value = expr->typ;
    const bool target_isopt = typ_is_optional(target);
    const bool target_isref = typ_is_reference(target);
    const bool value_isopt = typ_is_optional(value);
    const bool value_isref = typ_is_reference(value);

    // Symmetric to unify_with_defincomplete_entrails().
    if (!target_isref && value_isref) {
      e = try_insert_automagic_de(mod, expr);
      EXCEPT(e);
    }
    if (!target_isopt && value_isopt) {
      e = try_insert_automagic_de(mod, expr);
      EXCEPT(e);
    } else if (target_isopt && !value_isopt) {
      struct node *n = mk_node(mod, node, UN);
      n->as.UN.operator = TPREQMARK;
      node_subs_remove(node, n);
      node_subs_replace(node, expr, n);
      node_subs_append(n, expr);

      const struct node *except[] = { expr, NULL };
      e = catchup(mod, except, n, CATCHUP_BELOW_CURRENT);
      EXCEPT(e);
    }

    tit_next(tf);
  }
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

static ERROR gen_operator_call(struct module *mod, struct node *node,
                               ident operator_name,
                               struct node *left, struct node *right,
                               enum catchup_for catchup_for) {
  struct typ *tfun = typ_member(left->typ, operator_name);

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
static ERROR step_operator_call_inference(struct module *mod, struct node *node,
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
  case OP_UN_ADDARITH:
  case OP_UN_BW:
  case OP_BIN:
  case OP_BIN_SYM:
  case OP_BIN_SYM_BOOL:
  case OP_BIN_SYM_ADDARITH:
  case OP_BIN_SYM_ARITH:
  case OP_BIN_SYM_INTARITH:
  case OP_BIN_SYM_OVARITH:
  case OP_BIN_SYM_BW:
  case OP_BIN_BW_RHS_UNSIGNED:
    break;
  default:
    return 0;
  }

  const struct typ *l0 = typ_generic_functor_const(left->typ);
  // FIXME should not let non-sized overflow operations through.
  if (typ_isa(left->typ, TBI_NATIVE_INTEGER)
      || typ_isa(left->typ, TBI_NATIVE_BOOLEAN)
      || typ_isa(left->typ, TBI_NATIVE_FLOATING)
      || (l0 != NULL && typ_isa(l0, TBI_ENUM))) {
    return 0;
  }

  if (typ_definition_which(left->typ) == DEFTYPE
      && typ_definition_deftype_kind(left->typ) == DEFTYPE_ENUM
      // FIXME should not let non-sized overflow operations through.
      && typ_isa(typ_definition_tag_type(left->typ), TBI_NATIVE_INTEGER)) {
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
static ERROR step_ctor_call_inference(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);
  return 0;
}

static STEP_NM(step_array_ctor_call_inference,
               NM(INIT));
static ERROR step_array_ctor_call_inference(struct module *mod, struct node *node,
                                            void *user, bool *stop) {
  DSTEP(mod, node);

  if (!node->as.INIT.is_array || (node->flags & NODE_IS_TYPE)) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_ANY_SLICE)) {
    return 0;
  }

  struct typ *saved_typ = node->typ;

  struct node *array = node_new_subnode(mod, node);
  node_subs_remove(node, array);
  node_move_content(array, node);

  node_set_which(node, CALL);

  struct typ *tfun = typ_member(saved_typ, ID_FROM_SLICE);
  unset_typ(&array->typ);
  set_typ(&array->typ, typ_function_arg(tfun, 0));

  GSTART();
  G0(fun, node, DIRECTDEF,
     set_typ(&fun->as.DIRECTDEF.typ, tfun);
     fun->as.DIRECTDEF.flags = NODE_IS_TYPE);
  node_subs_append(node, array);

  const struct node *except[] = { array, NULL };
  error e = catchup(mod, except, node, CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

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

static STEP_NM(step_from_string_call_inference,
               NM(STRING));
static ERROR step_from_string_call_inference(struct module *mod, struct node *node,
                                            void *user, bool *stop) {
  DSTEP(mod, node);

  if (typ_equal(node->typ, TBI_LITERALS_STRING)) {
    error e = unify(mod, node, node->typ, TBI_STRING);
    EXCEPT(e);
    return 0;
  }

  if (typ_equal(node->typ, TBI_STRING)) {
    return 0;
  }

  struct typ *saved_typ = node->typ;
  if (typ_definition_which(saved_typ) == DEFINTF && typ_is_ungenarg(saved_typ)) {
    return 0;
  }

  if (typ_equal(saved_typ, TBI_RUNE)) {
    if (!string_literal_has_length_one(node->as.STRING.value)) {
      error e = mk_except_type(mod, node,
                               "string literal %s does not have length 1,"
                               " cannot coerce to Rune",
                               node->as.STRING.value);
      THROW(e);
    }
  }

  struct node *s = node_new_subnode(mod, node);
  node_subs_remove(node, s);
  node_move_content(s, node);

  node_set_which(node, CALL);

  struct typ *tfun = typ_member(saved_typ, ID_FROM_STRING);
  unset_typ(&s->typ);
  set_typ(&s->typ, typ_function_arg(tfun, 0));

  GSTART();
  G0(fun, node, DIRECTDEF,
     set_typ(&fun->as.DIRECTDEF.typ, tfun);
     fun->as.DIRECTDEF.flags = NODE_IS_TYPE);
  node_subs_append(node, s);

  const struct node *except[] = { s, NULL };
  error e = catchup(mod, except, node, CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static STEP_NM(step_from_number_literal_call_inference,
               NM(NUMBER));
static ERROR step_from_number_literal_call_inference(struct module *mod, struct node *node,
                                                     void *user, bool *stop) {
  DSTEP(mod, node);

  if (typ_isa(node->typ, TBI_NATIVE_INTEGER) || typ_isa(node->typ, TBI_FLOATING)) {
    return 0;
  }
  struct typ *saved_typ = node->typ;
  if (typ_definition_which(saved_typ) == DEFINTF && typ_is_ungenarg(saved_typ)) {
    return 0;
  }

  struct node *s = mk_node(mod, node, STRING);
  node_subs_remove(node, s);
  // Steal it:
  s->as.STRING.value = node->as.NUMBER.value;

  node_set_which(node, CALL);

  struct typ *tfun = typ_member(saved_typ, ID_FROM_NUMBER_LITERAL);
  GSTART();
  G0(fun, node, DIRECTDEF,
     set_typ(&fun->as.DIRECTDEF.typ, tfun);
     fun->as.DIRECTDEF.flags = NODE_IS_TYPE);
  node_subs_append(node, s);

  error e = catchup(mod, NULL, node, CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static STEP_NM(step_dtor_call_inference,
               NM(DEFNAME));
static ERROR step_dtor_call_inference(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);

  if (!typ_isa(node->typ, TBI_TRIVIAL_DTOR)) {
    struct tit *m = typ_definition_one_member(node->typ, ID_DTOR);
    if (m != NULL) {
      struct typ *mt = tit_typ(m);
      topdeps_record(mod, mt);
      tit_next(m);
    }
  }
  return 0;
}

// Before inserting copy ctor and before assignments may be removed for
// return-through-ref.
static STEP_NM(step_dtor_before_assign,
               NM(BIN));
static ERROR step_dtor_before_assign(struct module *mod, struct node *node,
                                     void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->as.BIN.operator != TASSIGN) {
    return 0;
  }

  struct node *left = subs_first(node);
  if (typ_isa(left->typ, TBI_TRIVIAL_DTOR)
      || !typ_isa(left->typ, TBI_DEFAULT_DTOR)) {
    return 0;
  }

  struct node *stat = find_current_statement(node);
  struct node *par = parent(stat);
  struct node *new_stat = node_new_subnode(mod, par);
  node_subs_remove(par, new_stat);
  node_subs_insert_before(par, stat, new_stat);

  struct node *left_copy = node_new_subnode(mod, new_stat);
  node_deepcopy(mod, left_copy, left);
  set_typ(&left_copy->typ, left->typ);
  node_subs_remove(new_stat, left_copy);

  error e = gen_operator_call(mod, new_stat, ID_DTOR, left_copy, NULL,
                              CATCHUP_BEFORE_CURRENT_SAME_TOP);
  EXCEPT(e);

  return 0;
}

static bool expr_is_literal_initializer(struct node **expr, struct module *mod, struct node *node) {
  if (node->which == INIT || node->which == TUPLE) {
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
    if (expr != NULL) {
      *expr = node;
    }
    return true;
  } else if (node->which == BLOCK) {
    return expr_is_return_through_ref(expr, mod, subs_last(node));
  } else {
    return false;
  }
}

static ERROR assign_copy_call_inference(struct module *mod, struct node *node) {
  struct node *left = subs_first(node);
  struct node *right = subs_at(node, 1);

  node_subs_remove(node, left);
  node_subs_remove(node, right);

  error e = gen_operator_call(mod, node, ID_COPY_CTOR, left, right,
                              CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);
  return 0;
}

static ERROR defname_copy_call_inference(struct module *mod, struct node *node) {
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

static ERROR arg_copy_call_inference(struct module *mod, struct node *node,
                                     struct node *expr) {
  const ident g = gensym(mod);
  GSTART();
  G0(narg, node, IDENT,
     narg->as.IDENT.name = g);
  node_subs_remove(node, narg);
  node_subs_replace(node, expr, narg);

  // We rely on the Copy_ctor insertion on the DEFNAME newly created.

  struct node *stat = find_current_statement(node);
  struct node *par = parent(stat);
  G0(let, par, LET,
     G(defn, DEFNAME,
       G(defni, IDENT,
         defni->as.IDENT.name = g);
       node_subs_append(defn, expr)));
  node_subs_remove(par, let);
  node_subs_insert_before(par, stat, let);

  const struct node *except[] = { expr, NULL };
  error e = catchup(mod, except, let, CATCHUP_BEFORE_CURRENT_SAME_TOP);
  EXCEPT(e);

  // Now that the Copy_ctor has been inferred, the result belongs to the
  // callee (etc.), not us. Let's make sure we don't Dtor it.
  defn->flags |= NODE_IS_MOVED_AWAY;

  e = catchup(mod, NULL, narg, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  return 0;
}

static ERROR try_replace_with_copy(struct module *mod, struct node *node,
                                   struct node *expr) {
  if (typ_isa(expr->typ, TBI_TRIVIAL_COPY)) {
    return 0;
  }

  if (expr_is_return_through_ref(NULL, mod, expr)) {
    return 0;
  }

  if ((node->flags | expr->flags) & NODE_IS_MOVED_AWAY) {
    // It's OK to trivial copy temporaries: it's a move.
    return 0;
  }

  error e = typ_check_isa(mod, expr, expr->typ, TBI_COPYABLE);
  EXCEPT(e);

  switch (node->which) {
  case BIN:
    e = assign_copy_call_inference(mod, node);
    EXCEPT(e);
    break;
  case DEFNAME:
    e = defname_copy_call_inference(mod, node);
    EXCEPT(e);
    break;
  case CALL:
  case INIT:
  case TUPLE:
    e = arg_copy_call_inference(mod, node, expr);
    EXCEPT(e);
    break;
  default:
    assert(false);
  }
  return 0;
}

static STEP_NM(step_copy_call_inference,
               NM(BIN) | NM(DEFNAME) | NM(CALL) | NM(INIT) | NM(TUPLE));
static ERROR step_copy_call_inference(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);
  if (mod->state->fun_state == NULL || !mod->state->fun_state->in_block) {
    return 0;
  }
  if (node->flags & NODE_IS_TYPE) {
    return 0;
  }

  struct node *right, *nxt;
  size_t every;
  error e;
  switch (node->which) {
  case BIN:
    if (node->as.BIN.operator == TASSIGN) {
      right = subs_last(node);
      break;
    }
    return 0;
  case DEFNAME:
    if (node->as.DEFNAME.ssa_user != NULL) {
      return 0;
    }
    right = subs_last(node);
    if (right != NULL && right->which != INIT) {
      break;
    }
    return 0;
  case CALL:
    nxt = next(subs_first(node));
    while (nxt != NULL) {
      struct node *arg = nxt;
      nxt = next(nxt);
      e = try_replace_with_copy(mod, node, arg);
      EXCEPT(e);
    }
    return 0;
  case TUPLE:
    nxt = subs_first(node);
    while (nxt != NULL) {
      struct node *arg = nxt;
      nxt = next(nxt);
      e = try_replace_with_copy(mod, node, arg);
      EXCEPT(e);
    }
    return 0;
  case INIT:
    if (node->as.INIT.is_array) {
      nxt = subs_first(node);
      every = 1;
    } else {
      nxt = subs_first(node);
      if (nxt != NULL) {
        nxt = next(nxt);
      }
      every = 2;
    }
    while (nxt != NULL) {
      struct node *arg = nxt;
      nxt = next(nxt);
      if (every == 2 && nxt != NULL) {
        nxt = next(nxt);
      }
      e = try_replace_with_copy(mod, node, arg);
      EXCEPT(e);
    }
    return 0;
  default:
    assert(false && "Unreached");
    return 0;
  }

  e = try_replace_with_copy(mod, node, right);
  EXCEPT(e);
  return 0;
}

static bool need_insert_dyn(struct module *mod,
                            const struct typ *intf,
                            const struct typ *concrete) {
  return (typ_is_dyn(intf) && typ_is_dyn_compatible(concrete))
    || (typ_is_dyn(intf) && typ_is_dyn(concrete) && !typ_equal(typ_dyn_intf(intf), typ_dyn_intf(concrete)));
}

static ERROR insert_dyn(struct node **src,
                        struct module *mod, struct node *node,
                        struct typ *target) {
  topdeps_record_dyn(mod, (*src)->typ);
  topdeps_record_mkdyn(mod, typ_generic_arg((*src)->typ, 0));
  topdeps_record_dyn(mod, target);

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

static ERROR try_insert_dyn(struct node **src,
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
static ERROR step_dyn_inference(struct module *mod, struct node *node,
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

    const struct node *fun = subs_first_const(node);
    const ssize_t first_vararg = typ_function_first_vararg(fun->typ);

    size_t n = 0;
    FOREACH_SUB_EVERY(src, node, 1, 1) {
      struct typ *target = typ_function_arg(fun->typ, n);
      if (n == first_vararg) {
        if (src->which == CALLNAMEDARG && src->as.CALLNAMEDARG.is_slice_vararg) {
          break;
        }
        target = typ_generic_arg(target, 0);
      }

      e = try_insert_dyn(&src, mod, node, target);
      EXCEPT(e);

      if (n != first_vararg) {
        n += 1;
      }
    }

#undef GET_TYP

    return 0;
  case INIT:
    if (typ_is_isalist_literal(node->typ)) {
      return 0;
    } else if (node->as.INIT.is_array) {
      assert(typ_isa(node->typ, TBI_ANY_SLICE) && "we're after step_array_ctor_call_inference");
      struct typ *target = typ_generic_arg(node->typ, 0);
      FOREACH_SUB(el, node) {
        src = el;
        e = try_insert_dyn(&src, mod, node, target);
        EXCEPT(e);
      }
      return 0;
    } else if (node->as.INIT.is_defchoice_external_payload_constraint) {
      return 0;
    } else if (node->as.INIT.for_tag != ID__NONE) {
      struct tit *leaf = typ_definition_one_member(node->typ,
                                                   node->as.INIT.for_tag);
      FOREACH_SUB_EVERY(name, node, 0, 2) {
        src = next(name);
        struct tit *target = tit_defchoice_lookup_field(leaf, node_ident(name));
        e = try_insert_dyn(&src, mod, node, tit_typ(target));
        EXCEPT(e);
        tit_next(target);
      }
      tit_next(leaf);
      return 0;
    } else {
      FOREACH_SUB_EVERY(name, node, 0, 2) {
        src = next(name);
        struct tit *target = typ_definition_one_member(node->typ, node_ident(name));
        e = try_insert_dyn(&src, mod, node, tit_typ(target));
        EXCEPT(e);
        tit_next(target);
      }
      return 0;
    }
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
  unset_typ(&block->typ);
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
static ERROR step_store_return_through_ref_expr(struct module *mod, struct node *node,
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
      struct node *par = parent(node);
      node_subs_remove(node, expr);

      const ident name = node_ident(retval_name(mod));
      if (expr->which == IDENT && name == node_ident(expr)) {
        // noop
      } else {
        GSTART();
        G0(ass, par, BIN,
           ass->as.BIN.operator = TASSIGN;
           G(n, IDENT,
             n->as.IDENT.name = name);
           node_subs_append(ass, expr));
        node_subs_remove(par, ass);
        node_subs_insert_before(par, node, ass);

        const struct node *except[] = { expr, NULL };
        error e = catchup(mod, except, ass, CATCHUP_BEFORE_CURRENT_SAME_TOP);
        EXCEPT(e);
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

static STEP_NM(step_add_dyn_topdep,
               NM(DEFTYPE) | NM(DEFINTF) | NM(BIN) | NM(CALL));
static ERROR step_add_dyn_topdep(struct module *mod, struct node *node,
                                 void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which == BIN) {
    if (node->as.BIN.operator == Tisa) {
      topdeps_record_mkdyn(mod, subs_last_const(node)->typ);
    }
    return 0;
  } else if (node->which == CALL) {
    if (typ_definition_ident(subs_first(node)->typ) == ID_DYNCAST) {
      topdeps_record_mkdyn(mod, typ_generic_arg(subs_first(node)->typ, 0));
    }
    return 0;
  }

  if (typ_is_reference(node->typ)) {
    return 0;
  }

  if (node->which == DEFTYPE && node->as.DEFTYPE.newtype_expr != NULL) {
    topdeps_record_dyn(mod, node->as.DEFTYPE.newtype_expr->typ);
    return 0;
  }

  topdeps_record_dyn(mod, node->typ);
  return 0;
}

static STEP_NM(step_add_newtype_pretend_wrapper_topdep,
               NM(DEFFUN) | NM(DEFMETHOD));
static ERROR step_add_newtype_pretend_wrapper_topdep(struct module *mod, struct node *node,
                                                     void *user, bool *stop) {
  DSTEP(mod, node);

  if ((node->which == DEFFUN && node->as.DEFFUN.is_newtype_pretend_wrapper)
      || (node->which == DEFMETHOD && node->as.DEFMETHOD.is_newtype_pretend_wrapper)) {
    struct tit *real = typ_definition_one_member(parent_const(node)->as.DEFTYPE.newtype_expr->typ,
                                                 node_ident(node));
    topdeps_record_newtype_actual(mod, tit_typ(real));
    tit_next(real);
  }

  return 0;
}

static STEP_NM(step_add_arg_dtor_topdep,
               NM(BLOCK));
static ERROR step_add_arg_dtor_topdep(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);
  if (!(NM(parent_const(node)->which) & (NM(DEFFUN) | NM(DEFMETHOD)))) {
    return 0;
  }

  const struct node *funargs = subs_at_const(parent_const(node), IDX_FUNARGS);
  FOREACH_SUB_CONST(arg, funargs) {
    if (next_const(arg) == NULL) {
      break;
    }
    if (!typ_isa(arg->typ, TBI_TRIVIAL_DTOR) && typ_isa(arg->typ, TBI_DEFAULT_DTOR)) {
      struct tit *it = typ_definition_one_member(arg->typ, ID_DTOR);
      topdeps_record(mod, tit_typ(it));
      tit_next(it);
    }
  }
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
    DOWN_STEP(step_type_destruct_mark);
    DOWN_STEP(step_type_mutability_mark);
    DOWN_STEP(step_type_gather_retval);
    DOWN_STEP(step_type_gather_excepts);
    ,
    UP_STEP(step_type_inference);
    UP_STEP(step_remove_typeconstraints);
    UP_STEP(step_type_drop_excepts);
    UP_STEP(step_increment_def_name_passed);
    ,
    FINALLY_STEP(step_pop_state);
    );
    return 0;
}

static ERROR passbody1(struct module *mod, struct node *root,
                       void *user, ssize_t shallow_last_up) {
  // second
  PASS(
    DOWN_STEP(step_push_state);
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_type_gather_retval);
    DOWN_STEP(step_check_no_literals_left);
    ,
    UP_STEP(step_init_insert_automagic);
    UP_STEP(step_operator_call_inference);
    UP_STEP(step_ctor_call_inference);
    UP_STEP(step_array_ctor_call_inference);
    UP_STEP(step_from_string_call_inference);
    UP_STEP(step_from_number_literal_call_inference);
    UP_STEP(step_dtor_call_inference);
    UP_STEP(step_dtor_before_assign);
    UP_STEP(step_copy_call_inference);
    UP_STEP(step_dyn_inference);

    UP_STEP(step_local_constant_substitution);

    UP_STEP(step_store_return_through_ref_expr);
    UP_STEP(step_add_dyn_topdep);
    UP_STEP(step_add_newtype_pretend_wrapper_topdep);
    UP_STEP(step_add_arg_dtor_topdep);
    ,
    FINALLY_STEP(step_pop_state);
    );
    return 0;
}

static ERROR passbody2(struct module *mod, struct node *root,
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
    DOWN_STEP(step_branching_block_down_phi_insert);
    ,
    UP_STEP(step_track_ident_use);
    UP_STEP(step_increment_def_name_passed);

    UP_STEP(step_branching_block_up_phi_insert);
    UP_STEP(step_branching_up);
    ,
    FINALLY_STEP(step_pop_state);
    );
    return 0;
}

a_pass passbody[] = { passbody0, passbody1, passbody2 };
