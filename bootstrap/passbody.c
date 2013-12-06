#include "passbody.h"

#include "table.h"
#include "types.h"
#include "scope.h"
#include "unify.h"

#include "passzero.h"
#include "passfwd.h"

static error step_push_fun_state(struct module *mod, struct node *node,
                                 void *user, bool *stop) {
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
  case EXAMPLE:
    PUSH_STATE(mod->state->fun_state);
    break;
  default:
    break;
  }
  return 0;
}

static error step_pop_fun_state(struct module *mod, struct node *node,
                                void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
  case EXAMPLE:
    POP_STATE(mod->state->fun_state);
    break;
  default:
    break;
  }
  return 0;

}

static error step_detect_not_dyn_intf_down(struct module *mod, struct node *node,
                                           void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    mod->state->fun_state->fun_uses_final = FALSE;
    break;
  default:
    break;
  }
  return 0;
}

static error step_detect_not_dyn_intf_up(struct module *mod, struct node *node,
                                         void *user, bool *stop) {
  DSTEP(mod, node);

  if (mod->state->fun_state == NULL) {
    // Not in a function.
    return 0;
  }

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    node_toplevel(node)->is_not_dyn = mod->state->fun_state->fun_uses_final
      || node->subs[IDX_GENARGS]->subs_count != 0;
    break;
  case IDENT:
    if (node_ident(node) == ID_FINAL) {
      mod->state->fun_state->fun_uses_final = TRUE;
    }
    break;
  case DEFARG:
    if (rew_find_subnode_in_parent(node_parent(node), node) == 0
        && node_parent(node)->which == DEFMETHOD) {
      // We just found self as a method argument on the way up, doesn't count.
      assert(mod->state->fun_state->fun_uses_final);
      mod->state->fun_state->fun_uses_final = FALSE;
    }
    break;
  default:
    break;
  }
  return 0;
}

static error step_rewrite_wildcards(struct module *mod, struct node *node,
                                    void *user, bool *stop) {
  DSTEP(mod, node);

#define SET_UNLESS_ZERO(dst, src) if (src != 0) { dst = src; }

  switch (node->which) {
  case DEFMETHOD:
    if (node->subs[IDX_GENARGS]->subs_count == 0) {
      break;
    }
    struct node *def = NULL;
    error e = scope_lookup_ident_immediate(&def, node, mod, node->scope,
                                           ID_WILDCARD_REF_ARG, TRUE);
    if (e) {
      break;
    }
    if (typ_equal(def->typ, TBI_ANY_REF)) {
      // noop
    } else if (typ_equal(def->typ, TBI_REF)) {
      mod->state->fun_state->ref_wildcard = TREFDOT;
      mod->state->fun_state->nulref_wildcard = TNULREFDOT;
      mod->state->fun_state->deref_wildcard = TDEREFDOT;
      mod->state->fun_state->wildcard = TDOT;
    } else if (typ_equal(def->typ, TBI_MREF)) {
      mod->state->fun_state->ref_wildcard = TREFBANG;
      mod->state->fun_state->nulref_wildcard = TNULREFBANG;
      mod->state->fun_state->deref_wildcard = TDEREFBANG;
      mod->state->fun_state->wildcard = TBANG;
    } else if (typ_equal(def->typ, TBI_MMREF)) {
      mod->state->fun_state->ref_wildcard = TREFSHARP;
      mod->state->fun_state->nulref_wildcard = TNULREFSHARP;
      mod->state->fun_state->deref_wildcard = TDEREFSHARP;
      mod->state->fun_state->wildcard = TSHARP;
    } else {
      assert(FALSE);
    }
    break;
  case UN:
    switch (node->as.UN.operator) {
    case TREFWILDCARD:
      SET_UNLESS_ZERO(node->as.UN.operator, mod->state->fun_state->ref_wildcard);
      break;
    case TNULREFWILDCARD:
      SET_UNLESS_ZERO(node->as.UN.operator, mod->state->fun_state->nulref_wildcard);
      break;
    case TDEREFWILDCARD:
      SET_UNLESS_ZERO(node->as.UN.operator, mod->state->fun_state->deref_wildcard);
      break;
    default:
      break;
    }
    break;
  case BIN:
    switch (node->as.BIN.operator) {
    case TWILDCARD:
      SET_UNLESS_ZERO(node->as.UN.operator, mod->state->fun_state->wildcard);
      break;
    default:
      break;
    }
    break;
  default:
    break;
  }

#undef SET_UNLESS_ZERO

  return 0;
}

static void mark_subs(struct module *mod, struct node *node, struct typ *mark,
                      size_t begin, size_t end, size_t incr) {
  for (size_t n = begin; n < end; n += incr){
    node->subs[n]->typ = mark;
  }
}

static void inherit(struct module *mod, struct node *node) {
  if (node->typ == TBI__NOT_TYPEABLE) {
    mark_subs(mod, node, node->typ, 0, node->subs_count, 1);
  }
}

error step_type_destruct_mark(struct module *mod, struct node *node,
                              void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which == MODULE) {
    return 0;
  }

  inherit(mod, node);

  struct typ *not_typeable = TBI__NOT_TYPEABLE;

  switch (node->which) {
  case BIN:
    if (OP_KIND(node->as.BIN.operator) == OP_BIN_ACC) {
      mark_subs(mod, node, not_typeable, 1, node->subs_count, 1);
    }
    break;
  case CALL:
    if (node->subs[0]->typ == NULL) {
      // Otherwise, we are rewriting this expression and we should not touch
      // subs[0].
      mark_subs(mod, node, TBI__CALL_FUNCTION_SLOT, 0, 1, 1);
    }
    break;
  case INIT:
    if (!node->as.INIT.is_array) {
      mark_subs(mod, node, TBI__NOT_TYPEABLE, 0, node->subs_count, 2);
    }
    break;
  case DEFFUN:
  case DEFMETHOD:
    node->subs[0]->typ = not_typeable;
    break;
  case DEFTYPE:
  case DEFINTF:
    node->subs[0]->typ = not_typeable;
    break;
  case DEFFIELD:
  case DEFARG:
  case DEFGENARG:
  case SETGENARG:
    node->subs[0]->typ = not_typeable;
    break;
  case MODULE_BODY:
    node->typ = not_typeable;
    break;
  case DEFCHOICE:
    node->subs[0]->typ = not_typeable;
    break;
  default:
    break;
  }

  return 0;
}

static error step_type_mutability_mark(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  DSTEP(mod, node);
  struct typ *mutable = TBI__MUTABLE;
  struct typ *mercurial = TBI__MERCURIAL;

  switch (node->which) {
  case BIN:
    switch (node->as.BIN.operator) {
    case TASSIGN:
    case TPLUS_ASSIGN:
    case TMINUS_ASSIGN:
    case TTIMES_ASSIGN:
    case TDIVIDE_ASSIGN:
    case TMODULO_ASSIGN:
    case TBWAND_ASSIGN:
    case TBWOR_ASSIGN:
    case TBWXOR_ASSIGN:
    case TRSHIFT_ASSIGN:
    case TLSHIFT_ASSIGN:
      mark_subs(mod, node, mutable, 0, 1, 1);
      break;
    default:
      break;
    }
    break;
  case UN:
    if (OP_KIND(node->as.UN.operator) != OP_UN_REFOF) {
      return 0;
    }

    // Below, we're interested in catching cases like: @#(self!p)

    struct node *arg= node->subs[0];
    switch (node->as.UN.operator) {
    case TREFDOT:
      // no-op
      break;
    case TREFBANG:
      if (arg->typ != NULL) {
        if (arg->which == BIN && !(arg->flags & NODE_IS_TYPE)) {
          error e = typ_check_deref_against_mark(mod, arg, TBI__MUTABLE,
                                                 arg->as.BIN.operator);
          EXCEPT(e);
        }
      } else {
        mark_subs(mod, node, mutable, 0, 1, 1);
      }
      break;
    case TREFWILDCARD:
    case TREFSHARP:
      if (arg->typ != NULL) {
        if (arg->which == BIN && !(arg->flags & NODE_IS_TYPE)) {
          error e = typ_check_deref_against_mark(mod, arg, TBI__MERCURIAL,
                                                 arg->as.BIN.operator);
          EXCEPT(e);
        }
        return 0;
      } else {
        mark_subs(mod, node, mercurial, 0, 1, 1);
      }
      break;
    default:
      break;
    }
    break;
  default:
    break;
  }

  return 0;
}

static error step_type_gather_retval(struct module *mod, struct node *node,
                                     void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    module_retval_set(mod, node_fun_retval(node));
    break;
  default:
    break;
  }
  return 0;
}

// FIXME: This is O(depth * number_throw_except).
// Would be O(number_throw_except) if we remembered whether we're in the TRY
// or in one of the CATCH, when descending.
static error check_in_try(struct module *mod, struct node *node, const char *which) {
  error e;

  struct try_state *st = module_excepts_get(mod);
  if (st == NULL) {
    goto fail;
  }

  const struct node *p = node;
  do {
    p = node_parent_const(p);
    if (p->which == CATCH) {
      goto fail;
    }
  } while (p->which != TRY);

  goto ok;

fail:
  e = mk_except(mod, node, "%s not in try block", which);
  THROW(e);

ok:
  return 0;
}

static error step_type_gather_excepts(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);
  error e;
  switch (node->which) {
  case TRY:
    module_excepts_open_try(mod, node);
    break;
  case DEFNAME:
    if (node->as.DEFNAME.is_excep) {
      e = check_in_try(mod, node, "except");
      EXCEPT(e);
      module_excepts_push(mod, node);
    }
    break;
  case THROW:
    e = check_in_try(mod, node, "throw");
    EXCEPT(e);
    module_excepts_push(mod, node);
    break;
  default:
    break;
  }
  return 0;
}

static error step_excepts_store_label(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *label_ident = NULL;

  const char *which = NULL;
  switch (node->which) {
  case DEFNAME:
    if (node->as.DEFNAME.is_excep) {
      which = "except";
      label_ident = node->as.DEFNAME.excep_label_ident;
      break;
    }
    return 0;
  case THROW:
    which = "throw";
    if (node->subs_count == 2) {
      label_ident = node->subs[0];
    }
    break;
  default:
    return 0;
  }

  struct node *tryy = module_excepts_get(mod)->tryy;
  struct node *eblock = tryy->subs[0]->subs[1];

  ident label = ID__NONE;
  error e;

  if (label_ident == NULL) {
    if (eblock->subs_count != 2) {
      e = mk_except(mod, node,
                    "try block has multiple catch,"
                    " %s must use a label", which);
      THROW(e);
    }

    assert(eblock->subs[1]->which == CATCH);
    label = eblock->subs[1]->as.CATCH.label;

  } else {
    if (eblock->subs_count == 2) {
      assert(eblock->subs[1]->which == CATCH);
      if (!eblock->subs[1]->as.CATCH.is_user_label) {
        e = mk_except(mod, node,
                      "try block has a single catch without a label,"
                      " %s must not use a label",
                      which);
        THROW(e);
      }
    }

    struct node *def = NULL;
    e = scope_lookup(&def, mod, node->scope, label_ident, FALSE);
    EXCEPT(e);

    if (def->which != CATCH || def->scope->parent->node != eblock) {
      e = mk_except(mod, label_ident,
                    "invalid label '%s'",
                    idents_value(mod->gctx, node_ident(label_ident)));
      THROW(e);
    }

    label = node_ident(label_ident);
  }

  switch (node->which) {
  case DEFNAME:
    node->as.DEFNAME.excep_label = label;
    node->as.DEFNAME.excep_error = tryy->as.TRY.error;
    break;
  case THROW:
    node->as.THROW.label = label;
    node->as.THROW.error = tryy->as.TRY.error;
    break;
  default:
    assert(FALSE);
  }

  return 0;
}

static error step_rewrite_defname_no_expr(struct module *mod, struct node *node,
                                          void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFNAME) {
    return 0;
  }

  return 0;
}

static error step_rewrite_sum_constructors(struct module *mod, struct node *node,
                                           void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != CALL) {
    return 0;
  }

  struct node *fun = node->subs[0];
  struct node *dfun = typ_definition(fun->typ);
  if (dfun->which != DEFTYPE
      || dfun->as.DEFTYPE.kind != DEFTYPE_SUM) {
    return 0;
  }

  struct node *member = NULL;
  error e = scope_lookup(&member, mod, fun->scope, fun, FALSE);
  EXCEPT(e);
  if (member->which != DEFCHOICE) {
    return 0;
  }

  struct node *mk_fun = mk_node(mod, node, BIN);
  mk_fun->as.BIN.operator = TDOT;
  rew_append(mk_fun, fun);
  struct node *mk = mk_node(mod, mk_fun, IDENT);
  mk->as.IDENT.name = ID_MK;

  rew_move_last_over(node, 0, TRUE);

  const struct node *except[] = { fun, NULL };
  e = catchup(mod, except, mk_fun, node->scope, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  return 0;
}

static error do_instantiate(struct node **result,
                            struct module *mod, struct typ *t,
                            struct typ **explicit_args, size_t arity,
                            bool tentative) {
  assert(arity == 0 || arity == typ_generic_arity(t));

  struct node *gendef = typ_definition(t);
  struct node *pristine = node_toplevel(gendef)->instances[0];
  struct node *instance = add_instance_deepcopy_from_pristine(mod, gendef,
                                                              pristine, tentative);
  set_typ(&node_toplevel(instance)->our_generic_functor_typ, t);

  struct node *genargs = instance->subs[IDX_GENARGS];
  const size_t first = typ_generic_first_explicit_arg(t);
  for (size_t n = 0; n < first; ++n) {
    struct node *ga = genargs->subs[n];
    ga->which = SETGENARG;
    // FIXME leaking ga->subs[1]
    ga->subs[1]->which = DIRECTDEF;
    set_typ(&ga->subs[1]->as.DIRECTDEF.typ,
            typ_create_tentative(typ_generic_arg(t, n)));
    ga->subs[1]->as.DIRECTDEF.flags = NODE_IS_TYPE;
  }

  for (size_t n = 0; n < arity; ++n) {
    struct node *ga = genargs->subs[first + n];
    ga->which = SETGENARG;
    // FIXME leaking ga->subs[1]
    ga->subs[1]->which = DIRECTDEF;
    set_typ(&ga->subs[1]->as.DIRECTDEF.typ, explicit_args[n]);
    ga->subs[1]->as.DIRECTDEF.flags = NODE_IS_TYPE;
  }

  error e = catchup_instantiation(mod, node_module_owner(gendef),
                                  instance, gendef->scope->parent,
                                  tentative);
  EXCEPT(e);

  if (result != NULL) {
    *result = instance;
  }

  return 0;
}

static struct typ *find_existing_instance(struct module *mod,
                                          struct typ *t,
                                          struct typ **args,
                                          size_t arity) {
  const size_t first = typ_generic_first_explicit_arg(t);
  assert(typ_generic_arity(t) - first == arity);

  const struct node *d = typ_definition_const(t);
  const struct toplevel *toplevel = node_toplevel_const(d);
  for (size_t n = 1; n < toplevel->instances_count; ++n) {
    struct typ *i = toplevel->instances[n]->typ;

    size_t a;
    for (a = first; a < arity; ++a) {
      if (!typ_equal(typ_generic_arg_const(i, a), args[a - first])) {
        break;
      }
    }

    if (a == arity) {
      return i;
    }
  }

  return NULL;
}

// Same as find_existing_instance(), but with different arguments.
static struct typ *find_existing_instance_for_tentative(struct module *mod,
                                                        const struct typ *t) {
  const struct node *d = typ_definition_const(typ_generic_functor_const(t));

  const struct toplevel *toplevel = node_toplevel_const(d);
  for (size_t n = 1; n < toplevel->instances_count; ++n) {
    struct typ *i = toplevel->instances[n]->typ;

    if (typ_equal(t, i)) {
      return i;
    }
  }

  return NULL;
}

static error instance(struct node **result,
                      struct module *mod,
                      struct node *for_error, size_t for_error_offset,
                      struct typ *t, struct typ **explicit_args, size_t arity) {
  const size_t first = typ_generic_first_explicit_arg(t);
  assert(arity == typ_generic_arity(t) - first);

  const bool tentative = instantiation_is_tentative(mod, t, explicit_args, arity);
  if (tentative) {
    struct typ **args = calloc(arity, sizeof(struct typ *));
    for (size_t n = 0; n < arity; ++n) {
      args[n] = typ_create_tentative(typ_generic_arg(t, n));
    }

    error e = do_instantiate(result, mod, t, args, arity, TRUE);
    EXCEPT(e);

    for (size_t n = 0; n < arity; ++n) {
      e = unify(mod, for_error->subs[for_error_offset + n],
                typ_generic_arg((*result)->typ, first + n),
                explicit_args[n]);
      EXCEPT(e);
    }

    free(args);
  } else {
    struct typ *r = find_existing_instance(mod, t, explicit_args, arity);
    if (r != NULL) {
      if (result != NULL) {
        *result = typ_definition(r);
      }
      return 0;
    }
    error e = do_instantiate(result, mod, t, explicit_args, arity, FALSE);
    EXCEPT(e);
  }

  return 0;
}

static error typ_ref(struct node **result,
                     struct module *mod, struct node *for_error,
                     enum token_type op, struct typ *typ) {
  error e = instance(result, mod, for_error, 0,
                     mod->gctx->builtin_typs_for_refop[op], &typ, 1);
  EXCEPT(e);

  return 0;
}

static error type_inference_un(struct module *mod, struct node *node) {
  assert(node->which == UN);
  error e;
  const enum token_type operator = node->as.UN.operator;
  struct node *term = node->subs[0];

  struct node *i = NULL;
  switch (OP_KIND(operator)) {
  case OP_UN_REFOF:
    // FIXME: it's not OK to take a mutable reference of:
    //   fun foo p:@t = void
    //     let mut = @!(p.)
    e = typ_ref(&i, mod, node, operator, term->typ);
    EXCEPT(e);
    set_typ(&node->typ, i->typ);
    node->flags |= term->flags & NODE__TRANSITIVE;
    break;
  case OP_UN_DEREF:
    e = typ_check_can_deref(mod, term, term->typ, operator);
    EXCEPT(e);
    e = typ_check_deref_against_mark(mod, node, node->typ, operator);
    EXCEPT(e);
    set_typ(&node->typ, typ_generic_arg(term->typ, 0));
    node->flags |= term->flags & NODE__TRANSITIVE;
    break;
  case OP_UN_BOOL:
    set_typ(&node->typ, TBI_BOOL);
    e = unify(mod, node, node->typ, term->typ);
    EXCEPT(e);
    break;
  case OP_UN_ARITH:
    set_typ(&node->typ, typ_create_tentative(TBI_ARITHMETIC));
    e = unify(mod, node, node->typ, term->typ);
    EXCEPT(e);
    break;
  case OP_UN_BW:
    set_typ(&node->typ, typ_create_tentative(TBI_BITWISE));
    e = unify(mod, node, node->typ, term->typ);
    EXCEPT(e);
    break;
  default:
    assert(FALSE);
  }

  return 0;
}

// for_error and for_error_offset are not actually used, as this call should
// never fail, but we need something to pass down.
static struct typ *try_wrap_ref_compatible(struct module *mod,
                                           struct node *for_error,
                                           size_t for_error_offset,
                                           struct typ *t) {
  if (!typ_is_reference(t)) {
    return t;
  }

  struct node *i = NULL;
  error e = instance(&i, mod, for_error, for_error_offset,
                     TBI__REF_COMPATIBLE, &t, 1);
  assert(!e);

  return typ_create_tentative(i->typ);
}

static error check_assign_not_types(struct module *mod, struct node *left,
                                    struct node *right) {
  error e;
  if ((left->flags & NODE_IS_TYPE)) {
    e = mk_except_type(mod, left, "cannot assign to a type variable");
    THROW(e);
  }
  if ((right->flags & NODE_IS_TYPE)) {
    e = mk_except_type(mod, right, "cannot assign a type");
    THROW(e);
  }
  return 0;
}

static error type_inference_bin_sym(struct module *mod, struct node *node) {
  assert(node->which == BIN);

  struct node *left = node->subs[0];
  struct node *right = node->subs[1];
  const enum token_type operator = node->as.BIN.operator;

  error e;
  if (operator == TASSIGN) {
    e = check_assign_not_types(mod, left, right);
    EXCEPT(e);

    e = unify(mod, node,
              try_wrap_ref_compatible(mod, node, 0, left->typ), right->typ);
    EXCEPT(e);
  } else {
    e = unify(mod, node, left->typ, right->typ);
    EXCEPT(e);
  }

  switch (OP_KIND(operator)) {
  case OP_BIN_SYM_BOOL:
    set_typ(&node->typ, left->typ);
    break;
  case OP_BIN_SYM_ARITH:
    e = check_assign_not_types(mod, left, right);
    EXCEPT(e);

    switch (operator) {
    case TPLUS_ASSIGN:
    case TMINUS_ASSIGN:
    case TTIMES_ASSIGN:
    case TDIVIDE_ASSIGN:
    case TMODULO_ASSIGN:
      e = typ_check_isa(mod, node, left->typ, TBI_ARITHMETIC);
      EXCEPT(e);

      set_typ(&node->typ, TBI_VOID);
      left->flags |= right->flags & NODE__TRANSITIVE;
      break;
    default:
      set_typ(&node->typ, typ_create_tentative(TBI_ARITHMETIC));
      e = unify(mod, node, node->typ, left->typ);
      EXCEPT(e);
      break;
    }
    break;
  case OP_BIN_SYM_BW:
    e = check_assign_not_types(mod, left, right);
    EXCEPT(e);

    switch (operator) {
    case TBWAND_ASSIGN:
    case TBWOR_ASSIGN:
    case TBWXOR_ASSIGN:
    case TRSHIFT_ASSIGN:
    case TLSHIFT_ASSIGN:
      e = typ_check_isa(mod, node, left->typ, TBI_BITWISE);
      EXCEPT(e);

      set_typ(&node->typ, TBI_VOID);
      left->flags |= right->flags & NODE__TRANSITIVE;
      break;
    default:
      set_typ(&node->typ, typ_create_tentative(TBI_BITWISE));
      e = unify(mod, node, node->typ, left->typ);
      EXCEPT(e);
      break;
    }
    break;
  case OP_BIN_SYM_PTR:
    e = typ_check_isa(mod, node, left->typ, TBI_ANY_ANY_REF);
    EXCEPT(e);
    e = typ_check_isa(mod, node, right->typ, TBI_ANY_ANY_REF);
    EXCEPT(e);
    set_typ(&node->typ, TBI_BOOL);
    break;
  case OP_BIN_SYM:
    switch (operator) {
    case TLE:
    case TLT:
    case TGT:
    case TGE:
    case TEQ:
    case TNE:
      if (typ_equal(left->typ, TBI_BOOL)) {
        // We want to propagate the link status of the terms when used as a
        // weakly concrete.
        set_typ(&node->typ, left->typ);
      } else {
        set_typ(&node->typ, TBI_BOOL);
      }
      break;
    default:
      set_typ(&node->typ, TBI_VOID);
      break;
    }
    break;
  default:
    set_typ(&node->typ, TBI_VOID);
    break;
  }

  return 0;
}

static void bin_accessor_maybe_ref(struct scope **parent_scope,
                                   struct module *mod, struct node *parent) {
  if (typ_is_reference(parent->typ)) {
    *parent_scope = typ_definition(typ_generic_arg(parent->typ, 0))->scope;
  }
}

static void bin_accessor_maybe_defchoice(struct scope **parent_scope, struct node *for_error,
                                         struct module *mod, struct node *parent) {
  if (parent->flags & NODE_IS_DEFCHOICE) {
    assert(parent->which == BIN);

    struct node *defchoice = NULL;
    error e = scope_lookup_ident_immediate(&defchoice, for_error, mod,
                                           typ_definition(parent->typ)->scope,
                                           node_ident(parent->subs[1]), FALSE);
    assert(!e);
    assert(defchoice->which == DEFCHOICE);

    *parent_scope = defchoice->scope;
  }
}

static error rewrite_unary_call(struct module *mod, struct node *node, struct typ *tfun) {
  struct scope *parent_scope = node->scope->parent;

  struct node *fun = calloc(1, sizeof(struct node));
  memcpy(fun, node, sizeof(*fun));
  set_typ(&fun->typ, tfun);
  fun->scope->node = fun;

  memset(node, 0, sizeof(*node));
  node->which = CALL;
  rew_append(node, fun);

  const struct node *except[] = { fun, NULL };
  error e = catchup(mod, except, node, parent_scope, CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);
  return 0;
}

static error type_inference_bin_accessor(struct module *mod, struct node *node) {
  error e;

  enum token_type operator = node->as.BIN.operator;
  const struct typ *mark = node->typ;

  struct node *container = node->subs[0];
  struct scope *container_scope = typ_definition(container->typ)->scope;
  bin_accessor_maybe_ref(&container_scope, mod, container);
  bin_accessor_maybe_defchoice(&container_scope, node, mod, container);

  struct node *for_error = node->subs[1];
  struct node *field = NULL;
  e = scope_lookup_ident_immediate(&field, for_error, mod, container_scope,
                                   node_ident(node->subs[1]), FALSE);
  EXCEPT(e);

  if (field->which == IMPORT && !field->as.IMPORT.intermediate_mark) {
    e = scope_lookup(&field, mod, mod->gctx->modules_root.scope,
                     field->subs[0], FALSE);
    assert(!e);
  }

  if (typ_is_function(field->typ) && mark != TBI__CALL_FUNCTION_SLOT) {
    if (node_fun_explicit_args_count(field) != 0) {
      e = mk_except_call_args_count(mod, node, field, 0, 0);
      THROW(e);
    }

    e = rewrite_unary_call(mod, node, field->typ);
    EXCEPT(e);
  } else {
    if (operator == TWILDCARD
        && typ_is_reference(field->typ)) {
      assert(typ_is_reference(field->typ));
      struct node *i = NULL;
      e = typ_ref(&i, mod, node, TREFWILDCARD,
                  typ_generic_arg(field->typ, 0));
      assert(!e);
      set_typ(&node->typ, i->typ);
    } else {
      set_typ(&node->typ, field->typ);
    }
    assert(field->which != BIN || field->flags != 0);
    node->flags = field->flags;
  }

  if (!(node->flags & NODE_IS_TYPE)) {
    e = typ_check_deref_against_mark(mod, node, mark, operator);
    EXCEPT(e);
  }

  return 0;
}

static error type_inference_bin_rhs_unsigned(struct module *mod, struct node *node) {
  error e;

  e = unify(mod, node->subs[1], node->subs[1]->typ, TBI_U32);
  EXCEPT(e);

  set_typ(&node->typ, typ_create_tentative(TBI_BITWISE));
  e = unify(mod, node, node->subs[0]->typ, node->typ);
  EXCEPT(e);

  return 0;
}

static error type_inference_bin_rhs_type(struct module *mod, struct node *node) {
  error e;

  if (!(node->subs[1]->flags & NODE_IS_TYPE)) {
    e = mk_except_type(mod, node->subs[1], "right-hand side not a type");
    THROW(e);
  }

  e = unify(mod, node, node->subs[0]->typ, node->subs[1]->typ);
  EXCEPT(e);

  return 0;
}

static error type_inference_bin(struct module *mod, struct node *node) {
  assert(node->which == BIN);

  switch (OP_KIND(node->as.BIN.operator)) {
  case OP_BIN_SYM:
  case OP_BIN_SYM_BOOL:
  case OP_BIN_SYM_ARITH:
  case OP_BIN_SYM_BW:
  case OP_BIN_SYM_PTR:
    return type_inference_bin_sym(mod, node);
  case OP_BIN_BW_RHS_UNSIGNED:
    return type_inference_bin_rhs_unsigned(mod, node);
  case OP_BIN_ACC:
    return type_inference_bin_accessor(mod, node);
  case OP_BIN_RHS_TYPE:
    return type_inference_bin_rhs_type(mod, node);
  default:
    assert(FALSE);
    return 0;
  }
}

static error typ_tuple(struct node **result, struct module *mod, struct node *node) {
  const size_t arity = node->subs_count;
  struct typ **args = calloc(arity, sizeof(struct typ *));
  for (size_t n = 0; n < arity; ++n) {
    args[n] = node->subs[n]->typ;
  }

  error e = instance(result, mod, node, 0,
                     typ_lookup_builtin_tuple(mod, arity), args, arity);
  EXCEPT(e);

  free(args);

  return 0;
}

static error type_inference_tuple(struct module *mod, struct node *node) {
  for (size_t n = 0; n < node->subs_count; ++n) {
    if (n > 0 && (node->flags & NODE_IS_TYPE) != (node->subs[n]->flags & NODE_IS_TYPE)) {
      error e = mk_except_type(mod, node->subs[n], "tuple combines values and types");
      THROW(e);
    }
    node->flags |= (node->subs[n]->flags & NODE__TRANSITIVE);
  }

  struct node *i = NULL;
  error e = typ_tuple(&i, mod, node);
  EXCEPT(e);

  set_typ(&node->typ, i->typ);

  return 0;
}

static error type_inference_tupleextract(struct module *mod, struct node *node) {
  struct node *expr = node->subs[node->subs_count - 1];
  assert(node->subs_count == typ_generic_arity(expr->typ) + 1
         && typ_isa(expr->typ, TBI_ANY_TUPLE));

  for (size_t n = 0; n < node->subs_count - 1; ++n) {
    set_typ(&node->subs[n]->typ, typ_generic_arg(expr->typ, n));
  }

  set_typ(&node->typ, node->subs[node->subs_count - 1]->typ);
  node->flags = node->subs[node->subs_count - 1]->flags; // Copy all flags, transparent node.

  return 0;
}

static void type_inference_init_named(struct module *mod, struct node *node) {
  // FIXME: Detached node, would have to be freed when releasing the
  // mod fun_state in which it is recorded below.
  //
  struct node *littype = calloc(1, sizeof(struct node));
  littype->which = DEFNAMEDLITERAL;
  struct node *littype_name = mk_node(mod, littype, IDENT);
  littype_name->as.IDENT.name = gensym(mod);
  (void)mk_node(mod, littype, GENARGS);
  struct node *littype_body = mk_node(mod, littype, BLOCK);

  const size_t arity = node->subs_count / 2;
  struct typ **args = calloc(arity, sizeof(struct typ *));
  for (size_t n = 0; n < node->subs_count; n += 2) {
    const struct node *left = node->subs[n];
    const struct node *right = node->subs[n+1];

    struct node *f = mk_node(mod, littype_body, DEFFIELD);
    struct node *name = mk_node(mod, f, IDENT);
    name->as.IDENT.name = node_ident(left);
    struct node *t = mk_node(mod, f, DIRECTDEF);
    set_typ(&t->as.DIRECTDEF.typ, right->typ);
    t->as.DIRECTDEF.flags = NODE_IS_TYPE;

    args[n / 2] = right->typ;
  }

  const bool tentative = TRUE;
  free(args);
  error e = catchup_instantiation(mod, mod,
                                  littype, node->scope,
                                  tentative);
  assert(!e);

  set_typ(&node->typ, typ_create_tentative(littype->typ));
}

static error type_inference_init_array(struct module *mod, struct node *node) {
  struct typ *el = typ_create_tentative(typ_generic_arg(TBI_STATIC_ARRAY, 0));
  struct node *i = NULL;
  error e = instance(&i, mod, node, 0,
                     TBI_STATIC_ARRAY, &el, 1);
  EXCEPT(e);

  set_typ(&node->typ, typ_create_tentative(i->typ));

  for (size_t n = 0; n < node->subs_count; n += 1) {
    error e = unify(mod, node->subs[n], node->subs[n]->typ,
                    typ_generic_arg(node->typ, 0));
    EXCEPT(e);
  }

  return 0;
}

static error type_inference_init(struct module *mod, struct node *node) {
  assert(node->which == INIT);
  if (node->as.INIT.is_array) {
    return type_inference_init_array(mod, node);
  } else {
    type_inference_init_named(mod, node);
    return 0;
  }
}

static error type_inference_return(struct module *mod, struct node *node) {
  assert(node->which == RETURN);

  if (node->subs_count > 0) {
    error e = unify(mod, node->subs[0], node->subs[0]->typ,
                    try_wrap_ref_compatible(mod, node, 1,
                                            module_retval_get(mod)->typ));
    EXCEPT(e);
  }

  set_typ(&node->typ, TBI_VOID);

  return 0;
}

static enum token_type refop_for_accop[] = {
  [TDOT] = TREFDOT,
  [TBANG] = TREFBANG,
  [TSHARP] = TREFSHARP,
  [TWILDCARD] = TREFWILDCARD,
};

static enum token_type accop_for_refop[] = {
  [TREFDOT] = TDOT,
  [TREFBANG] = TBANG,
  [TREFSHARP] = TSHARP,
  [TREFWILDCARD] = TWILDCARD,
};

static enum token_type derefop_for_accop[] = {
  [TDOT] = TDEREFDOT,
  [TBANG] = TDEREFBANG,
  [TSHARP] = TDEREFSHARP,
  [TWILDCARD] = TDEREFWILDCARD,
};

static struct node *expr_ref(enum token_type refop, struct node *node) {
  if (node->which == BIN && OP_KIND(node->as.BIN.operator) == OP_BIN_ACC) {
    // Of the form
    //   self.x.y!method args
    // which was transformed to
    //   type.method @!self.x.y args
    // We actually need
    //   type.method @!self.x!y args
    // This is assuming that typing has checked the transformation below is
    // legal.
    node->as.BIN.operator = accop_for_refop[refop];
  }

  struct node *n = calloc(1, sizeof(struct node));
  n->which = UN;
  n->as.UN.operator = refop;
  n->subs_count = 1;
  n->subs = calloc(n->subs_count, sizeof(struct node *));
  n->subs[0] = node;
  return n;
}

static struct node *self_ref_if_value(struct module *mod,
                                      enum token_type access, struct node *node) {
  if (typ_is_reference(node->typ)) {
    return node;
  } else {
    return expr_ref(access, node);
  }
}

static error prepare_call_arguments(struct module *mod, struct node *node) {
  struct node *fun = node->subs[0];

  if (node->subs_count > 1 && (node->subs[1]->flags & NODE_IS_TYPE)) {
    // Explicit generic function instantiation.
    return 0;
  }

  struct node *dfun = typ_definition(fun->typ);
  switch (dfun->which) {
  case DEFFUN:
    if (node_fun_explicit_args_count(dfun) != node->subs_count - 1) {
      error e = mk_except_call_args_count(mod, node, dfun, 0,
                                          node->subs_count - 1);
      THROW(e);
    }
    break;
  case DEFMETHOD:
    if (fun->which == BIN) {
      if ((fun->subs[0]->flags & NODE_IS_TYPE)) {
        // Form (type.method self ...).
        if (1 + node_fun_explicit_args_count(dfun) != node->subs_count - 1) {
          error e = mk_except_call_args_count(mod, node, dfun, 1,
                                              node->subs_count - 1);
          THROW(e);
        }
      } else {
        // Form (self.method ...); rewrite as (type.method self ...).
        if (node_fun_explicit_args_count(dfun) != node->subs_count - 1) {
          error e = mk_except_call_args_count(mod, node, dfun, 0,
                                              node->subs_count - 1);
          THROW(e);
        }

        struct node *m = mk_node(mod, node, DIRECTDEF);
        set_typ(&m->as.DIRECTDEF.typ, fun->typ);
        m->as.DIRECTDEF.flags = NODE_IS_TYPE;
        rew_move_last_over(node, 0, TRUE);

        struct node *self = self_ref_if_value(mod,
                                              refop_for_accop[fun->as.BIN.operator],
                                              fun->subs[0]);
        rew_append(node, self);
        rew_insert_last_at(node, 1);

        const struct node *except[] = { fun->subs[0], NULL };
        error e = catchup(mod, except, self, node->scope, CATCHUP_BELOW_CURRENT);
        EXCEPT(e);

        e = typ_check_can_deref(mod, fun, self->typ,
                                derefop_for_accop[fun->as.BIN.operator]);
        EXCEPT(e);

        e = catchup(mod, NULL, m, node->scope, CATCHUP_BELOW_CURRENT);
        EXCEPT(e);
      }
    } else if ((fun->flags & NODE_IS_TYPE) && fun->which == CALL) {
      // Generic method instantiation: (type.method u32 i32) self
      if (1 + node_fun_explicit_args_count(dfun) != node->subs_count - 1) {
        error e = mk_except_call_args_count(mod, node, dfun, 1,
                                            node->subs_count - 1);
        THROW(e);
      }
    }
    break;
  default:
    assert(FALSE);
  }

  return 0;
}

static error explicit_instantiation(struct module *mod, struct node *node) {
  error e;
  struct typ *t = node->subs[0]->typ;
  const size_t arity = node->subs_count - 1;

  const size_t first = typ_generic_first_explicit_arg(t);
  const size_t explicit_arity = typ_generic_arity(t) - first;
  if (arity != explicit_arity) {
    e = mk_except_type(mod, node,
                       "invalid number of explicit generic arguments:"
                       " %zu expected, but %zu given",
                       explicit_arity, arity);
    THROW(e);
  }

  struct typ **args = calloc(node->subs_count - 1, sizeof(struct typ *));
  for (size_t n = 0; n < arity; ++n) {
    args[n] = node->subs[1 + n]->typ;
  }

  struct node *i = NULL;
  e = instance(&i, mod, node, 1,
               t, args, arity);
  EXCEPT(e);
  free(args);

  set_typ(&node->typ, i->typ);
  node->flags |= NODE_IS_TYPE;

  return 0;
}

static error implicit_function_instantiation(struct module *mod, struct node *node) {
  error e;
  struct typ *tfun = node->subs[0]->typ;
  const size_t arity = node->subs_count - 1;

  // Already checked in prepare_call_arguments().
  assert(arity == typ_function_arity(tfun));

  const size_t gen_arity = typ_generic_arity(tfun);
  struct typ **args = calloc(gen_arity, sizeof(struct typ *));
  for (size_t n = 0; n < typ_generic_arity(tfun); ++n) {
    args[n] = typ_create_tentative(typ_generic_arg(tfun, n));
  }

  struct node *i = NULL;
  e = instance(&i, mod, node, 0, tfun, args, gen_arity);
  assert(!e);

  for (size_t n = 0; n < typ_function_arity(i->typ); ++n) {
    e = unify(mod, node->subs[1 + n],
              try_wrap_ref_compatible(mod, node, 1,
                                      typ_function_arg(i->typ, n)),
              node->subs[1 + n]->typ);
    EXCEPT(e);
  }

  free(args);

  set_typ(&node->subs[0]->typ, i->typ);
  set_typ(&node->typ, typ_function_return(i->typ));

  return 0;
}

static error function_instantiation(struct module *mod, struct node *node) {
  assert(node->subs_count >= 2);

  if (node->subs[1]->flags & NODE_IS_TYPE) {
    return explicit_instantiation(mod, node);
  } else {
    return implicit_function_instantiation(mod, node);
  }
}

static error check_consistent_either_types_or_values(struct module *mod,
                                                     struct node **subs,
                                                     size_t count) {
  uint32_t flags = 0;
  for (size_t n = 0; n < count; ++n) {
    struct node *s = subs[n];
    if (n > 0 && (flags & NODE_IS_TYPE) != (s->flags & NODE_IS_TYPE)) {
      error e = mk_except_type(mod, s, "expression combines types and values");
      THROW(e);
    }
    flags |= s->flags;
  }

  return 0;
}

static error type_inference_explicit_unary_call(struct module *mod, struct node *node, struct node *dfun) {
  if (dfun->which == DEFFUN && node->subs_count != 1) {
    error e = mk_except_call_args_count(mod, node, dfun, 0, node->subs_count - 1);
    THROW(e);
  } else if (dfun->which == DEFMETHOD && node->subs_count != 2) {
    error e = mk_except_call_args_count(mod, node, dfun, 1, node->subs_count - 1);
    THROW(e);
  }

  if (dfun->which == DEFMETHOD) {
    error e = unify(mod, node->subs[1],
                    try_wrap_ref_compatible(mod, node, 1,
                                            typ_function_arg(dfun->typ, 0)),
                    node->subs[1]->typ);
    EXCEPT(e);
  }

  set_typ(&node->typ, typ_function_return(dfun->typ));

  return 0;
}

static error type_inference_call(struct module *mod, struct node *node) {
  error e;
  struct node *fun = node->subs[0];
  struct typ *tfun = fun->typ;
  struct node *dfun = typ_definition(tfun);

  if (!node_is_fun(dfun)) {
    if (!node_can_have_genargs(dfun)
        || dfun->subs[IDX_GENARGS]->subs_count == 0) {
      e = mk_except_type(mod, fun, "not a generic type");
      THROW(e);
    }

    e = explicit_instantiation(mod, node);
    EXCEPT(e);

    return 0;
  }

  e = prepare_call_arguments(mod, node);
  EXCEPT(e);

  e = check_consistent_either_types_or_values(mod,
                                              node->subs + 1,
                                              node->subs_count - 1);
  EXCEPT(e);

  if (dfun->subs[IDX_GENARGS]->subs_count > 0
      && node_toplevel_const(dfun)->our_generic_functor_typ == NULL) {
    e = function_instantiation(mod, node);
    EXCEPT(e);

    return 0;
  }

  if (node_fun_explicit_args_count(dfun) == 0) {
    return type_inference_explicit_unary_call(mod, node, dfun);
  }

  for (size_t n = 1; n < node->subs_count; ++n) {
    e = unify(mod, node->subs[n],
              try_wrap_ref_compatible(mod, node, 1,
                                      typ_function_arg(tfun, n-1)),
              node->subs[n]->typ);
    EXCEPT(e);
  }

  set_typ(&node->typ, typ_function_return(tfun));

  return 0;
}

static error type_inference_block(struct module *mod, struct node *node) {
  error e;

  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *s = node->subs[n];
    if ((s->flags & NODE_IS_TYPE)) {
      e = mk_except_type(mod, s, "block statements cannot be type names");
      THROW(e);
    }
  }

  if (node->subs_count > 0) {
    for (size_t n = 0; n < node->subs_count - 1; ++n) {
      struct node *s = node->subs[n];
      if (!typ_equal(s->typ, TBI_VOID)) {
        e = mk_except_type(mod, s,
                           "intermediate statements in a block must be of type void"
                           " (except the last one), not '%s'",
                           typ_pretty_name(mod, s->typ));
        THROW(e);
      }
    }
    if (node->subs[node->subs_count - 1]->which == RETURN) {
      // FIXME: should make sure there are no statements after a RETURN.
      set_typ(&node->typ, TBI_VOID);
    } else {
      set_typ(&node->typ, node->subs[node->subs_count - 1]->typ);
    }
  } else {
    set_typ(&node->typ, TBI_VOID);
  }

  return 0;
}

static error type_inference_if(struct module *mod, struct node *node) {
  error e;

  for (size_t n = 0; n < node->subs_count-1; n += 2) {
    e = unify(mod, node->subs[n], node->subs[n]->typ,
              typ_create_tentative(TBI_GENERALIZED_BOOLEAN));
    EXCEPT(e);
  }

  set_typ(&node->typ, node->subs[1]->typ);

  for (size_t n = 3; n < node->subs_count; n += 2) {
    struct node *elif = node->subs[n];
    e = unify(mod, elif, node->typ, elif->typ);
    EXCEPT(e);
  }

  if (node->subs_count % 2 == 1) {
    struct node *els = node->subs[node->subs_count-1];
    e = unify(mod, els, node->typ, els->typ);
    EXCEPT(e);
  } else {
    if (!typ_equal(node->typ, TBI_VOID)) {
      e = mk_except_type(mod, node,
                         "if statement is not of type void but is missing an else branch");
      THROW(e);
    }
  }

  return 0;
}

static error unify_match_pattern(struct module *mod, struct node *expr, struct node *pattern) {
  struct node *d = typ_definition(expr->typ);
  assert(d->which == DEFTYPE);
  const bool enum_or_sum = d->as.DEFTYPE.kind == DEFTYPE_ENUM
    || d->as.DEFTYPE.kind == DEFTYPE_SUM;

  error e;
  if (!enum_or_sum) {
    e = mk_except_type(mod, expr,
                       "must match over an enum or sum type (FIXME: for now)");
    THROW(e);
  }

  if (node_ident(pattern) == ID_OTHERWISE) {
    set_typ(&pattern->typ, expr->typ);
    return 0;
  }

  if (d->which == DEFTYPE
      && enum_or_sum
      && pattern->which == IDENT) {
    struct node *field = NULL;
    e = scope_lookup_ident_immediate(&field, pattern, mod,
                                     d->scope,
                                     node_ident(pattern),
                                     TRUE);
    if (!e) {
      e = typ_check_equal(mod, pattern, expr->typ, field->typ);
      EXCEPT(e);
      set_typ(&pattern->typ, field->typ);
      pattern->flags = field->flags;
      return 0;
    } else {
      // drop e
    }
  }

  e = unify(mod, pattern, pattern->typ, expr->typ);
  EXCEPT(e);

  return 0;
}

static error type_inference_match(struct module *mod, struct node *node) {
  error e;

  struct node *expr = node->subs[0];
  for (size_t n = 1; n < node->subs_count; n += 2) {
    e = unify_match_pattern(mod, expr, node->subs[n]);
    EXCEPT(e);
  }

  set_typ(&node->typ, node->subs[2]->typ);
  for (size_t n = 4; n < node->subs_count; n += 2) {
    e = unify(mod, node->subs[n], node->subs[n]->typ, node->typ);
    EXCEPT(e);
  }

  return 0;
}

static error unify_try_errors(struct typ **exu, struct module *mod,
                              struct try_state *st) {
  for (size_t n = 0; n < st->count; ++n) {
    struct node *exc = st->excepts[n];

    switch (exc->which) {
    case THROW:
      if (exc->subs_count == 2) {
        exc = exc->subs[1];
      } else {
        exc = exc->subs[0];
      }
      break;
    case DEFNAME:
      exc = exc->as.DEFNAME.expr;
      break;
    default:
      assert(FALSE);
    }

    if (*exu == NULL) {
      *exu = exc->typ;
    } else {
      error e = unify(mod, exc, *exu, exc->typ);
      EXCEPT(e);
    }
  }

  return 0;
}

static error type_inference_try(struct module *mod, struct node *node) {
  node->typ = NULL;

  error e;
  struct try_state *st = module_excepts_get(mod);

  if (st->count == 0) {
    e = mk_except(mod, node,
                  "try block has no except or throw statement,"
                  " catch is unreachable");
    THROW(e);
  }

  struct typ *exu = NULL;
  e = unify_try_errors(&exu, mod, st);
  EXCEPT(e);

  struct node *elet = node->subs[0];
  struct node *edefp = elet->subs[0];
  struct node *eident = edefp->subs[0];
  set_typ(&eident->typ, exu);

  struct node *eblock = elet->subs[1];
  struct node *main_block = eblock->subs[0];
  struct typ *u = main_block->typ;

  for (size_t n = 1; n < eblock->subs_count; ++n) {
    struct node *catch = eblock->subs[n];
    struct node *let = catch->subs[0];
    struct node *defp = let->subs[0];
    struct node *error = defp->subs[1];
    struct node *block = catch->subs[1];

    e = unify(mod, error, error->typ, exu);
    EXCEPT(e);

    e = unify(mod, block, block->typ, u);
    EXCEPT(e);
  }

  set_typ(&node->typ, u);

  return 0;
}

static void type_inference_ident_unknown(struct module *mod, struct node *node) {
  // FIXME: Detached node, would have to be freed when releasing the
  // mod fun_state in which it is recorded below.
  //
  struct node *unk = calloc(1, sizeof(struct node));
  unk->which = DEFUNKNOWNIDENT;
  G(unk_name, unk, IDENT,
    unk_name->as.IDENT.name = gensym(mod));
  G(genargs, unk, GENARGS);
  G(unk_body, unk, BLOCK,
    G(unk_ident, unk_body, IDENT,
      unk_ident->as.IDENT.name = node_ident(node)));

  const bool tentative = TRUE;
  error e = catchup_instantiation(mod, mod, unk, node->scope, tentative);
  assert(!e);

  // Special marker, so we can rewrite it with the final enum or sum scope
  // in step_check_no_unknown_ident_left().
  node->as.IDENT.non_local_scope = unk->scope;

  set_typ(&node->typ, typ_create_tentative(unk->typ));
}

static error type_inference_ident(struct module *mod, struct node *node) {
  if (node_ident(node) == ID_OTHERWISE) {
    set_typ(&node->typ, typ_create_tentative(TBI_ANY));
    return 0;
  }

  struct node *def = NULL;
  error e = scope_lookup(&def, mod, node->scope, node, TRUE);
  if (e == EINVAL) {
    type_inference_ident_unknown(mod, node);
    return 0;
  }

  const enum node_which parent_which = node_parent_const(def)->which;
  if (parent_which == MODULE || parent_which == DEFTYPE || parent_which == DEFINTF) {
    node->as.IDENT.non_local_scope = def->scope->parent;
  } else if (def->flags & NODE_IS_GLOBAL_LET) {
    node->as.IDENT.non_local_scope = def->scope->parent->parent->parent;
  }

  if (def->which == DEFFIELD) {
    e = mk_except(mod, node,
                  "fields identifiers must be accessed through 'self'");
    THROW(e);
  }

  if (def->which == CATCH) {
    // 'node' is a throw or except label.
    return 0;
  }

  if (def->which == DEFNAME && def->typ == NULL) {
    // This happens when typing an IDENT in the pattern of a DEFPATTERN:
    // 'def' is the corresponding DEFNAME and not yet typed (as it appears
    // later in the tree).
    set_typ(&def->typ, typ_create_tentative(TBI_ANY));

    // FIXME: Cannot detect if an ident is used before its definition, e.g.:
    //   block
    //     x = a
    //     let a = 0
  }

  if (typ_is_function(def->typ)
      && node->typ != TBI__CALL_FUNCTION_SLOT) {
    if (node_fun_explicit_args_count(typ_definition(def->typ)) != 0) {
      e = mk_except_call_args_count(mod, node, typ_definition(def->typ), 0, 0);
      THROW(e);
    }
    e = rewrite_unary_call(mod, node, def->typ);
    EXCEPT(e);
  } else {
    set_typ(&node->typ, def->typ);
    node->flags = def->flags;
  }

  return 0;
}

static struct typ* number_literal_typ(struct module *mod, struct node *node) {
  assert(node->which == NUMBER);
  if (strchr(node->as.NUMBER.value, '.') != NULL) {
    return TBI_LITERALS_FLOATING;
  } else {
    return TBI_LITERALS_INTEGER;
  }
}

static bool string_literal_has_length_one(const char *s) {
  const size_t len = strlen(s);
  if (s == NULL) {
    return FALSE;
  } else if (len <= 2) {
    return FALSE;
  } else if (s[1] == '\\') {
    return len == 4;
  } else {
    return len == 3;
  }
}

error step_type_inference(struct module *mod, struct node *node,
                          void *user, bool *stop) {
  DSTEP(mod, node);
  error e;

  if (node->typ == TBI__NOT_TYPEABLE) {
    return 0;
  }

  switch (node->which) {
  case DEFINTF:
  case DEFTYPE:
  case DEFFUN:
  case DEFMETHOD:
  case DEFNAMEDLITERAL:
  case DEFCONSTRAINTLITERAL:
  case DEFUNKNOWNIDENT:
    assert(node->typ != NULL);
    // Already typed.
    return 0;
  case IMPORT:
    if (node->typ != NULL) {
      // Already typed.
      return 0;
    }
    break;
  default:
    break;
  }

  assert(node->typ == NULL
         || node->typ == TBI__MUTABLE
         || node->typ == TBI__MERCURIAL
         || node->typ == TBI__CALL_FUNCTION_SLOT
         || node->which == DEFNAME
         || typ_definition_const(node->typ)->which == MODULE
         || typ_definition_const(node->typ)->which == ROOT_OF_ALL);

  switch (node->which) {
  case NUL:
    set_typ(&node->typ, typ_create_tentative(TBI_LITERALS_NULL));
    break;
  case IDENT:
    e = type_inference_ident(mod, node);
    EXCEPT(e);
    break;
  case DEFNAME:
    if (node->typ == NULL && node_ident(node) == ID_OTHERWISE) {
      set_typ(&node->typ, node->as.DEFNAME.pattern->typ);
    }

    assert(node->typ == node->as.DEFNAME.pattern->typ);

    if (node->as.DEFNAME.expr != NULL) {
      e = unify(mod, node,
                node->as.DEFNAME.expr->typ,
                node->as.DEFNAME.pattern->typ);
      EXCEPT(e);

      node->as.DEFNAME.pattern->flags |= node->as.DEFNAME.expr->flags;
    }

    if (node->as.DEFNAME.is_excep) {
      struct node *tryy = module_excepts_get(mod)->tryy;
      struct node *err = tryy->subs[0]->subs[0]->subs[1];
      assert(err->which == DEFNAME);
      e = unify(mod, node, node->typ, err->typ);
      EXCEPT(e);
    }

    node->flags = node->as.DEFNAME.pattern->flags;

    const struct node *let = node_parent_const(node_parent_const(node));
    assert(let->which == LET);
    node->flags |= (let->flags & NODE_IS_GLOBAL_LET);
    break;
  case NUMBER:
    set_typ(&node->typ, typ_create_tentative(number_literal_typ(mod, node)));
    break;
  case BOOL:
    set_typ(&node->typ, typ_create_tentative(TBI_BOOL));
    break;
  case STRING:
    set_typ(&node->typ, typ_create_tentative(TBI_STATIC_STRING));
    break;
  case SIZEOF:
    set_typ(&node->typ, TBI_SIZE);
    break;
  case BIN:
    e = type_inference_bin(mod, node);
    EXCEPT(e);
    break;
  case UN:
    e = type_inference_un(mod, node);
    EXCEPT(e);
    break;
  case TUPLE:
    e = type_inference_tuple(mod, node);
    EXCEPT(e);
    break;
  case TUPLEEXTRACT:
    e = type_inference_tupleextract(mod, node);
    EXCEPT(e);
    break;
  case TUPLENTH:
    node->typ = typ_create_tentative(TBI_COPYABLE);
    break;
  case CALL:
    e = type_inference_call(mod, node);
    EXCEPT(e);
    break;
  case INIT:
    e = type_inference_init(mod, node);
    EXCEPT(e);
    break;
  case RETURN:
    e = type_inference_return(mod, node);
    EXCEPT(e);
    break;
  case BLOCK:
    e = type_inference_block(mod, node);
    EXCEPT(e);
    break;
  case CATCH:
    set_typ(&node->typ, node->subs[node->subs_count - 1]->typ);
    break;
  case THROW:
    {
      struct node *tryy = module_excepts_get(mod)->tryy;
      struct node *err = tryy->subs[0]->subs[0]->subs[1];
      assert(err->which == DEFNAME);
      e = unify(mod, node, node->subs[node->subs_count - 1]->typ, err->typ);
      EXCEPT(e);
      set_typ(&node->typ, TBI_VOID);
      break;
    }
  case BREAK:
  case CONTINUE:
  case NOOP:
    set_typ(&node->typ, TBI_VOID);
    break;
  case IF:
    e = type_inference_if(mod, node);
    EXCEPT(e);
    break;
  case FOR:
    set_typ(&node->typ, TBI_VOID);
    struct node *it = node->subs[IDX_FOR_IT]
      ->subs[IDX_FOR_IT_DEFP]
      ->subs[IDX_FOR_IT_DEFP_DEFN];
    e = unify(mod, it, it->typ, typ_create_tentative(TBI_ITERATOR));
    EXCEPT(e);
    e = typ_check_equal(mod, node_for_block(node),
                        node_for_block(node)->typ,
                        TBI_VOID);
    EXCEPT(e);
    break;
  case WHILE:
    set_typ(&node->typ, TBI_VOID);
    struct node *cond = node->subs[0];
    e = unify(mod, cond, cond->typ, typ_create_tentative(TBI_GENERALIZED_BOOLEAN));
    EXCEPT(e);
    struct node *block = node->subs[1];
    e = typ_check_equal(mod, block, block->typ, TBI_VOID);
    EXCEPT(e);
    break;
  case MATCH:
    e = type_inference_match(mod, node);
    EXCEPT(e);
    break;
  case TRY:
    e = type_inference_try(mod, node);
    EXCEPT(e);
    break;
  case DYN:
    assert(typ_is_reference(node->subs[0]->typ));
    set_typ(&node->typ, node->as.DYN.intf_typ);
    break;
  case TYPECONSTRAINT:
    set_typ(&node->typ, node->subs[1]->typ);
    e = unify(mod, node->subs[0], node->subs[0]->typ, node->typ);
    EXCEPT(e);
    break;
  case DEFARG:
    set_typ(&node->typ, node->subs[1]->typ);
    break;
  case DEFGENARG:
  case SETGENARG:
    set_typ(&node->typ, node->subs[1]->typ);
    node->flags |= NODE_IS_TYPE;
    break;
  case DEFPATTERN:
    set_typ(&node->typ, TBI_VOID);
    break;
  case DEFFIELD:
    set_typ(&node->typ, node->subs[1]->typ);
    break;
  case EXAMPLE:
    e = unify(mod, node->subs[0], node->subs[0]->typ,
              typ_create_tentative(TBI_BOOL));
    EXCEPT(e);
    set_typ(&node->typ, TBI_VOID);
    break;
  case LET:
    if (node_has_tail_block(node)) {
      set_typ(&node->typ, node->subs[node->subs_count - 1]->typ);
    } else {
      set_typ(&node->typ, TBI_VOID);
    }
    break;
  case DELEGATE:
  case PRE:
  case POST:
  case INVARIANT:
    set_typ(&node->typ, TBI_VOID);
    break;
  case ISALIST:
  case GENARGS:
  case FUNARGS:
  case IMPORT:
    node->typ = TBI__NOT_TYPEABLE;
    break;
  case ISA:
    set_typ(&node->typ, node->subs[0]->typ);
    node->flags = node->subs[0]->flags & NODE__TRANSITIVE;
    break;
  case DIRECTDEF:
    set_typ(&node->typ, node->as.DIRECTDEF.typ);
    node->flags = node->as.DIRECTDEF.flags;
    break;
  case DEFCHOICE:
    set_typ(&node->typ, node_parent(node)->typ);
    break;
  default:
    break;
  }

  assert(node->typ != NULL
         || (node->which == IDENT && "tolerate when its DEFNAME not yet typed"));
  return 0;
}

static error step_remove_typeconstraints(struct module *mod, struct node *node,
                                         void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->which == TYPECONSTRAINT && !node->as.TYPECONSTRAINT.in_pattern) {
    struct node **subs = node->subs;
    struct node *sub = node->subs[0];
    struct scope *parent = node->scope->parent;

    *node = *sub;
    node->scope->parent = parent;
    node->scope->node = node;

    free(sub);
    free(subs);
  }

  return 0;
}

static error step_type_drop_excepts(struct module *mod, struct node *node,
                                    void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case TRY:
    module_excepts_close_try(mod);
    return 0;
  default:
    return 0;
  }
}

HTABLE_SPARSE(idents_set, bool, ident);
implement_htable_sparse(__attribute__((unused)) static, idents_set, bool, ident);

static size_t defchoice_count(struct node *deft) {
  assert(deft->which == DEFTYPE);

  size_t r = 0;
  for (size_t n = 0; n < deft->subs_count; ++n) {
    struct node *d = deft->subs[n];
    if (d->which == DEFCHOICE) {
      r += 1;
    }
  }
  return r;
}

static error step_check_exhaustive_match(struct module *mod, struct node *node,
                                         void *user, bool *stop) {
  if (node->which != MATCH) {
    return 0;
  }

  struct node *expr = node->subs[0];
  struct node *dexpr = typ_definition(expr->typ);
  const bool enum_or_sum = dexpr->as.DEFTYPE.kind == DEFTYPE_ENUM
    || dexpr->as.DEFTYPE.kind == DEFTYPE_SUM;

  if (!enum_or_sum) {
    return 0;
  }

  struct idents_set set;
  idents_set_init(&set, 0);
  idents_set_set_delete_val(&set, FALSE);
  idents_set_set_custom_hashf(&set, ident_hash);
  idents_set_set_custom_cmpf(&set, ident_cmp);

  error e = 0;
  for (size_t n = 1; n < node->subs_count; n += 2) {
    struct node *p = node->subs[n];
    ident id;
    switch (p->which) {
    case IDENT:
      id = node_ident(p);
      if (id == ID_OTHERWISE) {
        if (n != node->subs_count - 2) {
          e = mk_except(mod, p, "default pattern '_' must be last");
          GOTO_THROW(e);
        }
        // No need to check further.
        goto ok;
      }
      break;
    case BIN:
      assert(OP_KIND(p->as.BIN.operator) == OP_BIN_ACC);
      id = node_ident(p->subs[1]);
      break;
    default:
      assert(FALSE);
    }

    if (idents_set_get(&set, id) != NULL) {
      e = mk_except(mod, p, "duplicated match case");
      GOTO_THROW(e);
    }

    idents_set_set(&set, id, TRUE);
  }

  if (idents_set_count(&set) != defchoice_count(dexpr)) {
    e = mk_except_type(mod, node, "non-exhaustive match");
    GOTO_THROW(e);
  }

ok:
except:
  idents_set_destroy(&set);
  return e;
}

error step_gather_final_instantiations(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    break;
  default:
    return 0;
  }

  struct module_state *st = mod->state;
  if (st->tentative_instantiations == NULL) {
    return 0;
  }

  for (size_t n = 0; n < st->tentative_instantiations_count; ++n) {
    struct typ *t = st->tentative_instantiations[n]->typ;
    if (typ_definition_const(t) == NULL) {
      // 't' was cleared in link_to_final()
      continue;
    }

    if (typ_generic_arity(t) == 0) {
      // For instance, a DEFNAMEDLITERAL that unified to a non-generic.
      continue;
    }

    if (typ_is_reference(t)) {
      continue;
    }

    if (typ_is_tentative(t)) {
      // By now, this instance should not be tentative anymore, as all its
      // generic arguments should have been linked to final types.
      for (size_t m = 0; m < typ_generic_arity(t); ++m) {
        struct typ *arg = typ_generic_arg(t, m);
        assert(!typ_is_tentative(arg));
      }
    }

    struct typ *functor = typ_generic_functor(t);
    const size_t arity = typ_generic_arity(t);

    if (typ_definition_const(functor)->which == DEFINTF) {
      continue;
    }
    for (size_t m = 0; m < arity; ++m) {
      if (typ_definition_const(typ_generic_arg_const(t, m))->which == DEFINTF) {
        continue;
      }
    }

    struct typ *existing = find_existing_instance_for_tentative(mod, t);
    if (existing != NULL) {
      typ_link_to_existing_final(existing, t);
      continue;
    }

    struct typ **args = calloc(arity, sizeof(struct typ *));
    for (size_t m = 0; m < arity; ++m) {
      args[m] = typ_generic_arg(t, m);
    }

    error e = do_instantiate(NULL, mod, functor, args, arity, FALSE);
    EXCEPT(e);

    free(args);
  }

  free(st->tentative_instantiations);
  st->tentative_instantiations = NULL;
  st->tentative_instantiations_count = 0;

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

static error step_check_no_literals_left(struct module *mod, struct node *node,
                                         void *user, bool *stop) {
  switch (node->which) {
  case NUMBER:
  case NUL:
    break;
  default:
    return 0;
  }

  if (typ_is_literal(node->typ)) {
    char *s = typ_pretty_name(mod, node->typ);
    error e = mk_except_type(mod, node,
                             "literal of type '%s' did not unify"
                             " to a concrete type", s);
    free(s);
    THROW(e);
  }

  return 0;
}

static error step_check_no_unknown_ident_left(struct module *mod, struct node *node,
                                              void *user, bool *stop) {
  switch (node->which) {
  case IDENT:
    break;
  default:
    return 0;
  }

  if (node->typ == NULL) {
    return 0;
  }

  const struct node *d = typ_definition_const(node->typ);
  if (d->which == DEFUNKNOWNIDENT) {
    error e = mk_except_type(mod, node,
                             "bare ident '%s' was never resolved",
                             idents_value(mod->gctx, node_ident(d->subs[2]->subs[0])));
    THROW(e);
  }

  struct scope *non_local_scope = node->as.IDENT.non_local_scope;
  if (non_local_scope != NULL
      && non_local_scope->node->which == DEFUNKNOWNIDENT
      && d->which == DEFTYPE
      && d->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    struct node *def = NULL;
    error e = scope_lookup_ident_immediate(&def, node, mod, d->scope,
                                           node_ident(node), FALSE);
    assert(!e);
    node->as.IDENT.non_local_scope = def->scope->parent;
  }

  return 0;
}

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
    return 0;
  }

  struct node copy = *node;

  memset(node, 0, sizeof(*node));
  node->which = CALL;

  struct node *fun = mk_node(mod, node, DIRECTDEF);
  struct node *fund = node_get_member(mod, typ_definition(copy.typ), id);
  assert(fund != NULL);
  set_typ(&fun->as.DIRECTDEF.typ, fund->typ);
  fun->as.DIRECTDEF.flags = NODE_IS_TYPE;

  struct node *literal = node_new_subnode(mod, node);
  *literal = copy;
  fix_scopes_after_move(literal);
  set_typ(&literal->typ, lit_typ);

  const struct node *except[] = { literal, NULL };
  error e = catchup(mod, except, node, copy.scope->parent,
                    CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static enum token_type operator_call_arg_refop(const struct module *mod,
                                               const struct typ *tfun, size_t n) {
  const struct typ *arg0 = typ_generic_functor_const(typ_function_arg_const(tfun, n));
  assert(arg0 != NULL && typ_is_reference(arg0));

  if (typ_equal(arg0, TBI_REF) || typ_equal(arg0, TBI_NREF)) {
    return TREFDOT;
  } else if (typ_equal(arg0, TBI_MREF) || typ_equal(arg0, TBI_NMREF)) {
    return TREFBANG;
  } else if (typ_equal(arg0, TBI_MMREF) || typ_equal(arg0, TBI_NMMREF)) {
    return TREFSHARP;
  } else {
    assert(FALSE);
    return 0;
  }
}

static error gen_operator_call(struct module *mod,
                               struct scope *saved_parent, struct node *node,
                               ident operator_name, struct node *left, struct node *right,
                               enum catchup_for catchup_for) {
  struct typ *tfun = node_get_member(mod, typ_definition(left->typ),
                                     operator_name)->typ;

  memset(node, 0, sizeof(*node));
  node->which = CALL;
  struct node *fun = mk_node(mod, node, DIRECTDEF);
  set_typ(&fun->as.DIRECTDEF.typ, tfun);
  fun->as.DIRECTDEF.flags = NODE_IS_TYPE;

  const struct node *except[3] = { NULL, NULL, NULL };
  except[0] = left;
  rew_append(node, expr_ref(operator_call_arg_refop(mod, tfun, 0), left));

  if (right != NULL) {
    except[1] = right;
    rew_append(node, expr_ref(operator_call_arg_refop(mod, tfun, 1), right));
  }

  error e = catchup(mod, except, node, saved_parent, catchup_for);
  EXCEPT(e);

  return 0;
}

static error step_operator_call_inference(struct module *mod, struct node *node,
                                          void *user, bool *stop) {
  DSTEP(mod, node);

  enum token_type op;
  struct node *left = NULL;
  struct node *right = NULL;

  switch (node->which) {
  case UN:
    op = node->as.UN.operator;
    left = node->subs[0];
    break;
  case BIN:
    op = node->as.BIN.operator;
    left = node->subs[0];
    right = node->subs[1];
    break;
  default:
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

  if (typ_isa(left->typ, TBI_NATIVE_INTEGER)
      || typ_isa(left->typ, TBI_NATIVE_BOOLEAN)
      || typ_isa(left->typ, TBI_NATIVE_FLOATING)) {
    return 0;
  }

  struct node *dleft = typ_definition(left->typ);
  if (dleft->which == DEFTYPE
      && dleft->as.DEFTYPE.kind == DEFTYPE_ENUM
      && typ_isa(dleft->as.DEFTYPE.choice_typ, TBI_NATIVE_INTEGER)) {
    return 0;
  }

  if (operator_ident[op] == 0) {
    return 0;
  }

  error e = gen_operator_call(mod, node->scope->parent, node,
                              operator_ident[op], left, right,
                              CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static error gen_operator_test_call(struct module *mod, struct node *node, size_t n) {
  struct node *expr = node->subs[n];
  if (typ_equal(expr->typ, TBI_BOOL)) {
    return 0;
  }

  struct node *test = node_new_subnode(mod, node);
  rew_move_last_over(node, n, TRUE);
  error e = gen_operator_call(mod, node->scope->parent, test,
                              ID_OPERATOR_TEST, expr, NULL,
                              CATCHUP_BELOW_CURRENT);
  EXCEPT(e);
  return 0;
}

static error step_operator_test_call_inference(struct module *mod, struct node *node,
                                               void *user, bool *stop) {
  DSTEP(mod, node);
  error e;

  switch (node->which) {
  case DEFNAME:
    if (node->as.DEFNAME.is_excep) {
      e = gen_operator_test_call(mod, node, IDX_DEFNAME_EXCEP_TEST);
      EXCEPT(e);
    }
    break;
  default:
    break;
  }

  return 0;
}

static error step_ctor_call_inference(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);
  return 0;
}

static error step_array_ctor_call_inference(struct module *mod, struct node *node,
                                            void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->which != INIT || !node->as.INIT.is_array) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_TRIVIAL_ARRAY_CTOR)) {
    return 0;
  }

  struct node copy = *node;

  memset(node, 0, sizeof(*node));
  node->which = CALL;
  struct node *fun = mk_node(mod, node, DIRECTDEF);
  set_typ(&fun->as.DIRECTDEF.typ,
          node_get_member(mod, typ_definition(copy.typ), ID_MKV)->typ);
  fun->as.DIRECTDEF.flags = NODE_IS_TYPE;

  struct node *ref_array = mk_node(mod, node, UN);
  ref_array->as.UN.operator = TREFDOT;
  struct node *array = node_new_subnode(mod, ref_array);
  *array = copy;
  fix_scopes_after_move(array);
  set_typ(&array->typ,
          typ_generic_arg(typ_function_arg(fun->as.DIRECTDEF.typ, 0), 0));

  const struct node *except[] = { array, NULL };
  error e = catchup(mod, except, node, copy.scope->parent,
                    CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static error step_dtor_call_inference(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);
  return 0;
}

static bool expr_is_literal_initializer(struct node **init, struct module *mod, struct node *expr) {
  if (expr->which == INIT) {
    if (init != NULL) {
      *init = expr;
    }
    return TRUE;
  } else {
    return expr->which == TYPECONSTRAINT
      && expr_is_literal_initializer(init, mod, expr->subs[0]);
  }
}

static bool expr_is_return_through_ref(struct node **init, struct module *mod, struct node *expr) {
  return (expr_is_literal_initializer(init, mod, expr) || expr->which == CALL)
    && !typ_isa(expr->typ, TBI_RETURN_BY_COPY);
}

static error assign_copy_call_inference(struct module *mod, struct node *node) {
  error e = gen_operator_call(mod, node->scope->parent, node,
                              ID_COPY_CTOR, node->subs[0], node->subs[1],
                              CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);
  return 0;
}

static error defname_copy_call_inference(struct module *mod, struct node *node) {
  struct node *let = node_parent(node_parent(node));
  assert(let->which == LET);

  struct node *within;
  if (node_has_tail_block(let)) {
    within = let->subs[let->subs_count-1];
  } else {
    within = mk_node(mod, let, BLOCK);
    error e = catchup(mod, NULL, within, let->scope, CATCHUP_BELOW_CURRENT);
    EXCEPT(e);
  }

  struct node *left = node->as.DEFNAME.pattern;
  struct node *right = node->as.DEFNAME.expr;
  node->as.DEFNAME.expr = NULL; // Steal right.
  struct scope *saved_parent = within->scope->parent;

  struct node *copycall = node_new_subnode(mod, within);
  error e = gen_operator_call(mod, saved_parent, copycall,
                              ID_COPY_CTOR, left, right,
                              CATCHUP_AFTER_CURRENT);
  EXCEPT(e);
  return 0;
}

static error step_copy_call_inference(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);
  struct node *left;
  struct node *right;
  switch (node->which) {
  case BIN:
    if (node->as.BIN.operator == TASSIGN) {
      left = node->subs[0];
      right = node->subs[1];
      break;
    }
    return 0;
  case DEFNAME:
    if (!(node->flags & NODE_IS_TYPE)) {
      left = node->as.DEFNAME.pattern;
      right = node->as.DEFNAME.expr;
      if (right != NULL) {
        break;
      }
    }
    return 0;
  default:
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

  error e = typ_check_isa(mod, left, left->typ, TBI_COPYABLE);
  EXCEPT(e);

  switch (node->which) {
  case BIN:
    e = assign_copy_call_inference(mod, node);
    break;
  case DEFNAME:
    e = defname_copy_call_inference(mod, node);
    break;
  default:
    assert(FALSE);
  }
  EXCEPT(e);
  return 0;
}

static error check_exhaustive_intf_impl_eachisalist(struct module *mod,
                                                    struct typ *t,
                                                    struct typ *intf,
                                                    bool *stop,
                                                    void *user) {
  (void) user;
  const struct node *deft = typ_definition_const(t);

  // FIXME: Remove
  if (typ_isa(t, TBI_ANY_TUPLE)) {
    return 0;
  }

  const struct node *dintf = typ_definition_const(intf);

  for (size_t m = 0; m < dintf->subs_count; ++m) {
    const struct node *f = dintf->subs[m];
    if (f->which != DEFFUN && f->which != DEFMETHOD) {
      continue;
    }

    if (node_get_member_const(mod, deft, node_ident(f)) == NULL) {
      error e = mk_except_type(mod, deft,
                               "type '%s' isa '%s' but does not implement '%s'",
                               typ_pretty_name(mod, deft->typ),
                               typ_pretty_name(mod, intf),
                               idents_value(mod->gctx, node_ident(f)));
      THROW(e);
    }

    // FIXME check that the prototype is an exact match.
  }

  return 0;
}

static error step_check_exhaustive_intf_impl(struct module *mod, struct node *node,
                                             void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFTYPE) {
    return 0;
  }

  if (typ_is_pseudo_builtin(node->typ)) {
    return 0;
  }

  error e = typ_isalist_foreach(mod, node->typ, ISALIST_FILTER_TRIVIAL_ISALIST,
                                check_exhaustive_intf_impl_eachisalist, NULL);
  EXCEPT(e);

  return 0;
}

static bool need_insert_dyn(struct module *mod,
                            const struct typ *intf,
                            const struct typ *concrete) {
  // FIXME: further constraint on intf and concrete: their genarg, if any,
  // must be concrete.
  return
    typ_is_dyn(intf)
    && typ_is_reference(concrete)
    && typ_definition_const(typ_generic_arg_const(concrete, 0))->which != DEFINTF;
}

static error insert_dyn(struct module *mod, struct node *node,
                        const struct node *target, struct node *src) {
  struct node *d = mk_node(mod, node, DYN);
  set_typ(&d->as.DYN.intf_typ, target->typ);

  const size_t where = rew_find_subnode_in_parent(node, src);
  rew_move_last_over(node, where, TRUE);
  rew_append(d, src);

  const struct node *except[] = { target, src, NULL };
  error e = catchup(mod, except, d, node->scope, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  return 0;
}

static error try_insert_dyn(struct module *mod, struct node *node,
                            const struct node *target, struct node *src) {
  if (!need_insert_dyn(mod, target->typ, src->typ)) {
    return 0;
  }

  error e = insert_dyn(mod, node, target, src);
  EXCEPT(e);
  return 0;
}

static error step_dyn_inference(struct module *mod, struct node *node,
                                void *user, bool *stop) {
  DSTEP(mod, node);
  const struct node *target;
  struct node *src;

  error e;
  switch (node->which) {
  case RETURN:
    if (node->subs_count == 0) {
      return 0;
    }
    target = module_retval_get(mod);
    src = node->subs[0];

    e = try_insert_dyn(mod, node, target, src);
    EXCEPT(e);
    return 0;
  case BIN:
    if (node->as.BIN.operator == TASSIGN) {
      target = node->subs[0];
      src = node->subs[1];
      e = try_insert_dyn(mod, node, target, src);
      EXCEPT(e);
    }
    return 0;
  case DEFNAME:
    if (!(node->flags & NODE_IS_TYPE)) {
      target = node->as.DEFNAME.pattern;
      src = node->as.DEFNAME.expr;
      if (src != NULL) {
        e = try_insert_dyn(mod, node, target, src);
        EXCEPT(e);
      }
    }
    return 0;
  case TYPECONSTRAINT:
    target = node->subs[0];
    src = node->subs[1];
    e = try_insert_dyn(mod, node, target, src);
    EXCEPT(e);
    return 0;
  case CALL:
    if (node->flags & NODE_IS_TYPE) {
      return 0;
    }

    struct node *funargs = typ_definition(node->subs[0]->typ)->subs[IDX_FUNARGS];
    for (size_t n = 1; n < node->subs_count; ++n) {
      struct node *arg = node->subs[n];
      if (arg->which == BLOCK) {
        break;
      }
      target = funargs->subs[n - 1];
      src = arg;
      e = try_insert_dyn(mod, node, target, src);
      EXCEPT(e);
    }
    return 0;
  default:
    return 0;
  }
}

static bool is_block_like(struct node *node) {
  switch (node->which) {
  case IF:
  case TRY:
  case MATCH:
  case BLOCK:
    return TRUE;
  default:
    return FALSE;
  }
}

static void block_insert_value_assign(struct module *mod, struct node *block,
                                      struct node *target, ident target_name) {
  assert(block->which == BLOCK);

  const size_t where = block->subs_count - 1;
  struct node *last = block->subs[where];

  struct node *assign = mk_node(mod, block, BIN);
  assign->as.BIN.operator = TASSIGN;
  if (target != NULL) {
    rew_append(assign, target);
  } else {
    struct node *left = mk_node(mod, assign, IDENT);
    left->as.IDENT.name = target_name;
  }
  rew_move_last_over(block, where, TRUE);

  rew_append(assign, last);
  set_typ(&block->typ, TBI_VOID);

  const struct node *except[] = { last, NULL };
  error e = catchup(mod, except, assign, block->scope, CATCHUP_BELOW_CURRENT);
  assert(!e);
}

static void block_like_insert_value_assign(struct module *mod, struct node *node,
                                           struct node *target, ident target_name) {
  switch (node->which) {
  case IF:
    for (size_t n = 1; n < node->subs_count; n += 2) {
      block_insert_value_assign(mod, node->subs[n], target, target_name);
    }
    if (node->subs_count % 2 == 1) {
      struct node *els = node->subs[node->subs_count-1];
      block_insert_value_assign(mod, els, target, target_name);
    }
    break;
  case TRY:
    block_insert_value_assign(mod, node->subs[0], target, target_name);
    block_insert_value_assign(mod, node->subs[2], target, target_name);
    break;
  case MATCH:
    for (size_t n = 2; n < node->subs_count; n += 2) {
      block_insert_value_assign(mod, node->subs[n], target, target_name);
    }
    break;
  case BLOCK:
    block_insert_value_assign(mod, node, target, target_name);
    break;
  default:
    assert(FALSE);
    break;
  }
}

static error step_move_assign_in_block_like(struct module *mod, struct node *node,
                                            void *user, bool *stop) {
  if (node->which != BIN || !OP_IS_ASSIGN(node->as.BIN.operator)) {
    return 0;
  }

  struct node *left = node->subs[0];
  struct node *right = node->subs[1];
  if (!is_block_like(right)) {
    return 0;
  }

  block_like_insert_value_assign(mod, right, left, 0);

  struct scope *saved_parent = node->scope->parent;
  free(node->scope);
  memset(node, 0, sizeof(*node));
  *node = *right;
  fix_scopes_after_move(node);
  node->scope->parent = saved_parent;
  set_typ(&node->typ, TBI_VOID);

  return 0;
}

static error step_move_defname_expr_in_let_block(struct module *mod, struct node *node,
                                                 void *user, bool *stop) {
  if (node->which != DEFPATTERN) {
    return 0;
  }

  // Need to process them backwards for cases like:
  //   let x, y = block -> 0;;, block -> 1;;
  // where we prepend the blocks to the let-block, such that:
  //   let x, y
  //     block -> x = 0
  //     block -> y = 0
  struct node *defp_block = NULL;
  for (ssize_t n = node->subs_count - 1; n >= 0; --n) {
    struct node *d = node->subs[n];
    if (d->which != DEFNAME) {
      continue;
    }

    struct node *expr = d->as.DEFNAME.expr;
    if (expr == NULL) {
      continue;
    } else if (is_block_like(expr)) {
      block_like_insert_value_assign(mod, expr, d->as.DEFNAME.pattern, 0);

      if (defp_block == NULL) {
        struct node *let = node_parent(node);
        struct node *target_let_block = NULL;

        if (node_has_tail_block(let)) {
          const bool first_defpattern_in_let = rew_find_subnode_in_parent(let, node) == 0;
          if (first_defpattern_in_let) {
            struct node *let_block = let->subs[let->subs_count-1];
            target_let_block = mk_node(mod, let, BLOCK);
            rew_move_last_over(let, let->subs_count - 2, TRUE);
            rew_append(target_let_block, let_block);

            const struct node *except[] = { let_block, NULL };
            error e = catchup(mod, except, target_let_block, let->scope,
                              CATCHUP_AFTER_CURRENT);
            assert(!e);
          } else {
            target_let_block = let->subs[let->subs_count - 1];
          }
        } else {
          target_let_block = mk_node(mod, let, BLOCK);
          error e = catchup(mod, NULL, target_let_block, let->scope,
                            CATCHUP_AFTER_CURRENT);
          assert(!e);
        }

        defp_block = mk_node(mod, target_let_block, BLOCK);
        error e = catchup(mod, NULL, defp_block, target_let_block->scope,
                          CATCHUP_AFTER_CURRENT);
        assert(!e);
      }

      d->as.DEFNAME.expr = NULL;
      rew_prepend(defp_block, expr);
      expr->scope->parent = defp_block->scope;
    }
  }

  return 0;
}

static const struct node *retval_name(struct module *mod) {
  const struct node *retval = module_retval_get(mod);
  assert(retval->subs_count > 0);
  return retval->subs[0];
}

static error step_store_return_through_ref_expr(struct module *mod, struct node *node,
                                                void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *expr = NULL;
  struct node *init_expr = NULL;

  switch (node->which) {
  case RETURN:
    if (node->subs_count == 0
        || typ_equal(node->subs[0]->typ, TBI_VOID)) {
      return 0;
    }

    expr = node->subs[0];
    if (expr_is_return_through_ref(&init_expr, mod, expr)) {
      // Keep node->as.RETURN.return_through_ref_expr null as the
      // subexpression CALL or INIT will directly write to it.
      if (init_expr != NULL) {
        init_expr->as.INIT.target_expr = retval_name(mod);
        node->as.RETURN.forced_return_through_ref = TRUE;
      } else if (expr->which == CALL) {
        expr->as.CALL.return_through_ref_expr = retval_name(mod);
      } else if (is_block_like(expr)) {
        block_like_insert_value_assign(mod, expr, NULL, node_ident(retval_name(mod)));
      } else {
        assert(FALSE);
      }
    } else if (!typ_isa(expr->typ, TBI_RETURN_BY_COPY)
               && typ_isa(expr->typ, TBI_COPYABLE)) {
      // FIXME need to insert copy_ctor

      if (node->subs[0]->which == IDENT
          && node_ident(retval_name(mod)) == node_ident(node->subs[0])) {
        // noop
      } else if (is_block_like(expr)) {
        block_like_insert_value_assign(mod, expr, NULL, node_ident(retval_name(mod)));

        node->as.RETURN.return_through_ref_expr = retval_name(mod);
      } else {
        node->as.RETURN.return_through_ref_expr = retval_name(mod);
      }
    }
    return 0;
  case DEFPATTERN:
    for (ssize_t n = node->subs_count - 1; n >= 0; --n) {
      struct node *d = node->subs[n];
      if (d->which != DEFNAME) {
        continue;
      }
      expr = d->as.DEFNAME.expr;
      init_expr = NULL;
      if (expr == NULL) {
        // noop
      } else if (expr_is_literal_initializer(&init_expr, mod, expr)) {
        init_expr->as.INIT.target_expr = d->as.DEFNAME.pattern;
      } else if (expr->which == CALL && expr_is_return_through_ref(NULL, mod, expr)) {
        expr->as.CALL.return_through_ref_expr = d->as.DEFNAME.pattern;
      }
    }
    return 0;
  case BIN:
    if (!OP_IS_ASSIGN(node->as.BIN.operator)) {
      return 0;
    }
    struct node *left = node->subs[0];
    struct node *right = node->subs[1];
    init_expr = NULL;
    if (expr_is_literal_initializer(&init_expr, mod, right)) {
      init_expr->as.INIT.target_expr = left;
    } else if (right->which == CALL && expr_is_return_through_ref(NULL, mod, right)) {
      right->as.CALL.return_through_ref_expr = left;
    }
    return 0;
  default:
    return 0;
  }
}

static bool is_significant(const struct node *node) {
  return node->which != TYPECONSTRAINT;
}

static void closest_significant_parent(struct node **parent, struct node *node) {
  struct node *n = node;
  do {
    n = node_parent(n);
  } while (!is_significant(n));
  *parent = n;
}

static bool block_like_needs_temporary(struct module *mod,
                                       struct node *node) {
  assert(is_block_like(node));
  struct node *significant_parent = NULL;
  closest_significant_parent(&significant_parent, node);

  if (is_block_like(significant_parent)) {
    return FALSE;
  }

  if (significant_parent->which == RETURN
      && !typ_isa(node->typ, TBI_RETURN_BY_COPY)) {
    return FALSE;
  } else if (significant_parent->which == DEFPATTERN) {
    return FALSE;
  } else if (significant_parent->which == BIN
             && OP_IS_ASSIGN(significant_parent->as.BIN.operator)) {
    return FALSE;
  } else {
    return TRUE;
  }
}

struct temporaries {
  struct node **rvalues;
  ident *gensyms;
  size_t count;
};

static void temporaries_add(struct temporaries *temps, struct node *node) {
  temps->count += 1;
  temps->rvalues = realloc(temps->rvalues, temps->count * sizeof(*temps->rvalues));
  temps->rvalues[temps->count - 1] = node;
}

static error step_gather_temporary_rvalues(struct module *mod, struct node *node,
                                           void *user, bool *stop) {
  DSTEP(mod, node);
  struct temporaries *temps = user;

  if (!is_significant(node)) {
    return 0;
  }

  struct node *significant_parent = NULL;
  closest_significant_parent(&significant_parent, node);

  switch (node->which) {
  case UN:
    if (OP_KIND(node->as.UN.operator) == OP_UN_REFOF
        && node_is_rvalue(node->subs[0])) {
      if (node->as.UN.operator != TREFDOT) {
        error e = mk_except(mod, node, "Cannot take a mutating reference of a rvalue");
        THROW(e);
      }

      temporaries_add(temps, node);
    }
    break;
  case INIT:
    if (significant_parent->which == RETURN
        && !typ_isa(node->typ, TBI_RETURN_BY_COPY)) {
      break;
    }
    if (significant_parent->which == DEFPATTERN) {
      break;
    }
    if (significant_parent->which == BIN
        && OP_IS_ASSIGN(significant_parent->as.BIN.operator)) {
      break;
    }
    if (significant_parent->which == UN
        && OP_KIND(significant_parent->as.UN.operator) == OP_UN_REFOF) {
      break;
    }
    temporaries_add(temps, node);
    break;
  case CALL:
    if ((node->flags & NODE_IS_TYPE)) {
      break;
    }
    if (typ_isa(node->typ, TBI_RETURN_BY_COPY)) {
      break;
    }
    if (significant_parent->which == RETURN) {
      break;
    }
    if (significant_parent->which == BIN
        && OP_IS_ASSIGN(significant_parent->as.BIN.operator)) {
      break;
    }
    if (significant_parent->which == UN
        && OP_KIND(significant_parent->as.UN.operator) == OP_UN_REFOF) {
      break;
    }
    if (significant_parent->which == DEFPATTERN) {
      break;
    }
    temporaries_add(temps, node);
    break;
  case TUPLEEXTRACT:
    if (node->subs[node->subs_count - 1]->which != IDENT) {
      temporaries_add(temps, node);
    }
    break;
  case IF:
  case TRY:
  case MATCH:
    if (typ_equal(node->typ, TBI_VOID)) {
      break;
    }

    if (!block_like_needs_temporary(mod, node)) {
      break;
    }

    temporaries_add(temps, node);
    break;
  case BLOCK:
    *stop = TRUE;
    break;
  default:
    return 0;
  }

  return 0;
}

static void declare_temporaries(struct module *mod, struct node *statement,
                                struct temporaries *temps) {
  temps->gensyms = calloc(temps->count, sizeof(*temps->gensyms));

  struct node *let = NULL;
  if (statement->which == LET) {
    let = statement;
  } else {
    // We are going to move the current stepping node down in the tree, and
    // it would normally be in the except list and not be processed by later
    // steps in the current pass. But these steps may be crucial as
    // 'statement' could be anything at all. So we must force these steps by
    // hand. Yes, it's hacky at best.

    struct node copy;
    copy = *statement;

    let = statement;
    memset(let, 0, sizeof(*let));
    let->which = LET;
    struct node *block = mk_node(mod, let, BLOCK);
    struct node *new_statement = node_new_subnode(mod, block);
    *new_statement = copy;
    fix_scopes_after_move(new_statement);

    const struct node *except[] = { new_statement, NULL };
    error e = catchup(mod, except, let, copy.scope->parent, CATCHUP_REWRITING_CURRENT);
    assert(!e);

    const struct pass *pa = passes(mod->stage->state->passing);
    PUSH_STATE(mod->state->step_state);
    for (size_t s = mod->state->step_state->prev->stepping + 1; pa->ups[s] != NULL; ++s) {
      mod->state->step_state->upward = TRUE;
      mod->state->step_state->stepping = s;

      bool stop = FALSE;
      e = pa->ups[s](mod, new_statement, NULL, &stop);
      assert(!e);
      assert(!stop);
    }
    POP_STATE(mod->state->step_state);
  }

  statement = NULL;

  for (size_t n = 0; n < temps->count; ++n) {
    const struct node *rv = temps->rvalues[n];
    struct node *defp = mk_node(mod, let, DEFPATTERN);
    rew_insert_last_at(let, n);

    struct node *typc = mk_node(mod, defp, TYPECONSTRAINT);

    struct node *tmp_name = mk_node(mod, typc, IDENT);
    const ident g = gensym(mod);
    tmp_name->as.IDENT.name = g;
    temps->gensyms[n] = g;

    struct node *typ = mk_node(mod, typc, DIRECTDEF);
    if (rv->which == UN && OP_KIND(rv->as.UN.operator) == OP_UN_REFOF) {
      assert(typ_generic_arity(rv->typ) == 1);
      set_typ(&typ->as.DIRECTDEF.typ, typ_generic_arg(rv->typ, 0));
    } else {
      set_typ(&typ->as.DIRECTDEF.typ, rv->typ);
    }
    typ->as.DIRECTDEF.flags = NODE_IS_TYPE;

    error e = catchup(mod, NULL, let->subs[n], let->scope, CATCHUP_BELOW_CURRENT);
    assert(!e);
  }
}

static error step_define_temporary_rvalues(struct module *mod, struct node *node,
                                           void *user, bool *stop) {
  DSTEP(mod, node);
  if (!node_is_statement(node)) {
    return 0;
  }

  static const step temprvalue_down[] = {
    NULL,
  };

  static const step temprvalue_up[] = {
    step_gather_temporary_rvalues,
    NULL,
  };

  struct temporaries temps = { 0 };

  PUSH_STATE(mod->state->step_state);
  error e = pass(mod, node, temprvalue_down, temprvalue_up, -1, &temps);
  EXCEPT(e);
  POP_STATE(mod->state->step_state);

  if (temps.count == 0) {
    return 0;
  }

  declare_temporaries(mod, node, &temps);

  for (size_t n = 0; n < temps.count; ++n) {
    const ident g = temps.gensyms[n];
    struct node *rv = temps.rvalues[n];

    struct node *rv_parent = NULL;
    closest_significant_parent(&rv_parent, rv);
    const size_t rv_where = rew_find_subnode_in_parent(rv_parent, rv);

    struct node *nrv = mk_node(mod, rv_parent, BLOCK);
    rew_move_last_over(rv_parent, rv_where, TRUE);
    struct node *assign = mk_node(mod, nrv, BIN);
    assign->as.BIN.operator = TASSIGN;

    struct node *target = mk_node(mod, assign, IDENT);
    target->as.IDENT.name = g;

    const struct node *except[2];
    except[1] = NULL;

    if (rv->which == UN && OP_KIND(rv->as.UN.operator) == OP_UN_REFOF) {
      rv->subs[0]->flags |= NODE_IS_TEMPORARY;
      rew_append(assign, rv->subs[0]);
      except[0] = rv->subs[0];

      struct node *nvalue = mk_node(mod, nrv, UN);
      nvalue->as.UN.operator = rv->as.UN.operator;
      struct node *nvalue_name = mk_node(mod, nvalue, IDENT);
      nvalue_name->as.IDENT.name = g;
    } else if (rv->which == TUPLEEXTRACT) {
      struct node *tuple = rv->subs[rv->subs_count - 1];
      rew_pop(rv, TRUE);
      rew_append(assign, tuple);
      except[0] = tuple;

      struct node *extractor = mk_node(mod, nrv, TUPLEEXTRACT);
      for (size_t n = 0; n < typ_generic_arity(tuple->typ); ++n) {
        // We want to reuse the original TUPLENTH from 'rv' as they may be
        // pointed to by nearby DEFNAME.expr, so their location in memory
        // cannot change.
        struct node *nth = rv->subs[n];
        memset(nth, 0, sizeof(*nth));
        nth->which = TUPLENTH;
        nth->as.TUPLENTH.nth = n;
        rew_append(extractor, nth);
      }
      struct node *nvalue = mk_node(mod, extractor, IDENT);
      nvalue->as.IDENT.name = g;

    } else {
      rv->flags |= NODE_IS_TEMPORARY;
      rew_append(assign, rv);
      except[0] = rv;

      struct node *nvalue = mk_node(mod, nrv, IDENT);
      nvalue->as.IDENT.name = g;
    }

    e = catchup(mod, except, nrv, rv_parent->scope, CATCHUP_BELOW_CURRENT);
    EXCEPT(e);
  }

  free(temps.rvalues);
  free(temps.gensyms);

  return 0;
}

const struct pass passbody[] = {
  {
    PASS_BODY, "first",
    {
      step_stop_submodules,
      step_stop_marker_tbi,
      step_stop_already_morningtypepass,
      step_push_fun_state,
      step_detect_not_dyn_intf_down,
      step_rewrite_wildcards,
      step_type_destruct_mark,
      step_type_mutability_mark,
      step_type_gather_retval,
      step_type_gather_excepts,
      NULL,
    },
    {
      step_excepts_store_label,
      step_rewrite_defname_no_expr,
      step_rewrite_sum_constructors,
      step_detect_not_dyn_intf_up,
      step_type_inference,
      step_remove_typeconstraints,
      step_type_drop_excepts,
      step_check_exhaustive_match,
      step_gather_final_instantiations,
      step_pop_fun_state,
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_BODY, "second",
    {
      step_stop_submodules,
      step_stop_marker_tbi,
      step_push_fun_state,
      step_type_gather_retval,
      step_check_no_literals_left,
      step_check_no_unknown_ident_left,
      NULL,
    },
    {
      step_weak_literal_conversion,
      step_operator_call_inference,
      step_operator_test_call_inference,
      step_ctor_call_inference,
      step_array_ctor_call_inference,
      step_dtor_call_inference,
      step_copy_call_inference,
      step_check_exhaustive_intf_impl,
      step_dyn_inference,

      step_define_temporary_rvalues,
      step_move_assign_in_block_like,
      step_move_defname_expr_in_let_block,
      step_store_return_through_ref_expr,

      step_pop_fun_state,
      step_complete_instantiation,
      NULL,
    }
  },
};
