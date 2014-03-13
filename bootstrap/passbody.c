#include "passbody.h"

#include "table.h"
#include "types.h"
#include "scope.h"
#include "unify.h"
#include "constraints.h"
#include "ssa.h"

#include "passzero.h"
#include "passfwd.h"

noinline__
static void record_topdep(struct module *mod, struct typ *t) {
  struct top_state *st = mod->state->top_state;
  if (st == NULL
      || typ_is_tentative(t)
      || typ_is_pseudo_builtin(t)
      || typ_is_generic_functor(t)
      || (st->top->typ != NULL && typ_equal(st->top->typ, t))
      || typ_generic_arity(t) == 0) {
    return;
  }

  struct toplevel *toplevel = node_toplevel(st->top);
  if (toplevel->topdeps == NULL) {
    toplevel->topdeps = calloc(1, sizeof(*toplevel->topdeps));
    typset_fullinit(toplevel->topdeps);
  }

  uint32_t *value = typset_get(toplevel->topdeps, t);
  if (value == NULL) {
    typset_set(toplevel->topdeps, t, st->topdep_mask);
  } else {
    *value |= st->topdep_mask;
  }
}

STEP_FILTER(step_push_fun_state,
            SF(DEFFUN) | SF(DEFMETHOD) | SF(EXAMPLE));
error step_push_fun_state(struct module *mod, struct node *node,
                          void *user, bool *stop) {
  DSTEP(mod, node);

  PUSH_STATE(mod->state->fun_state);

  return 0;
}

STEP_FILTER(step_pop_fun_state,
            SF(DEFFUN) | SF(DEFMETHOD) | SF(EXAMPLE));
error step_pop_fun_state(struct module *mod, struct node *node,
                         void *user, bool *stop) {
  DSTEP(mod, node);

  POP_STATE(mod->state->fun_state);

  return 0;

}

STEP_FILTER(step_set_topdep_mask,
            SF(BLOCK));
static error step_set_topdep_mask(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  DSTEP(mod, node);

  struct top_state *st = mod->state->top_state;
  switch (node_parent(node)->which) {
  case DEFFUN:
  case DEFMETHOD:
    if (typ_generic_arity(st->top->typ) == 0) {
      st->topdep_mask &= TOP_IS_INLINE;
    }
    break;
  case EXAMPLE:
    st->topdep_mask &= TOP_IS_INLINE;
    break;
  default:
    break;
  }

  return 0;
}

static STEP_FILTER(step_detect_not_dyn_intf_down,
                   SF(DEFFUN) | SF(DEFMETHOD));
static error step_detect_not_dyn_intf_down(struct module *mod, struct node *node,
                                           void *user, bool *stop) {
  DSTEP(mod, node);

  mod->state->fun_state->fun_uses_final = false;

  return 0;
}

static STEP_FILTER(step_detect_not_dyn_intf_up,
                   SF(DEFFUN) | SF(DEFMETHOD) | SF(IDENT) | SF(DEFARG));
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
    if (mod->state->fun_state->fun_uses_final
        || node_subs_count_atleast(node_subs_at_const(node, IDX_GENARGS), 1)) {
      node_toplevel(node)->flags |= TOP_IS_NOT_DYN;
    }
    break;
  case IDENT:
    if (node_ident(node) == ID_FINAL) {
      mod->state->fun_state->fun_uses_final = true;
    }
    break;
  case DEFARG:
    if (node_subs_first(node_parent(node)) == node
        && node_parent(node)->which == DEFMETHOD) {
      // We just found self as a method argument on the way up, doesn't count.
      assert(mod->state->fun_state->fun_uses_final);
      mod->state->fun_state->fun_uses_final = false;
    }
    break;
  default:
    assert(false && "Unreached");
    break;
  }
  return 0;
}

static STEP_FILTER(step_rewrite_wildcards,
                   SF(DEFMETHOD) | SF(UN) | SF(BIN));
static error step_rewrite_wildcards(struct module *mod, struct node *node,
                                    void *user, bool *stop) {
  DSTEP(mod, node);

#define SET_UNLESS_ZERO(dst, src) if (src != 0) { dst = src; }

  switch (node->which) {
  case DEFMETHOD:
    if (!node_subs_count_atleast(node_subs_at_const(node, IDX_GENARGS), 1)) {
      break;
    }
    struct node *def = NULL;
    error e = scope_lookup_ident_immediate(&def, node, mod, &node->scope,
                                           ID_WILDCARD_REF_ARG, true);
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
      assert(false);
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
    assert(false && "Unreached");
    break;
  }

#undef SET_UNLESS_ZERO

  return 0;
}

static void mark_subs(struct module *mod, struct node *node, struct typ *mark,
                      struct node *first, struct node *last, size_t incr) {
  if (first == NULL) {
    return;
  }

  struct node *n = first;
  while (true) {
    n->typ = mark;

    for (size_t i = 0; i < incr; ++i) {
      if (n == last) {
        return;
      }

      n = node_next(n);
    }
  }
}

static void inherit(struct module *mod, struct node *node) {
  if (node->typ == TBI__NOT_TYPEABLE) {
    mark_subs(mod, node, node->typ,
              node_subs_first(node), node_subs_last(node), 1);
  }
}

STEP_FILTER(step_type_destruct_mark,
            SF(BIN) | SF(CALL) | SF(INIT) | SF(DEFFUN) |
            SF(DEFMETHOD) | SF(DEFTYPE) | SF(DEFINTF) | SF(DEFFIELD) |
            SF(DEFARG) | SF(DEFGENARG) | SF(SETGENARG) | SF(MODULE_BODY) |
            SF(DEFCHOICE) | SF(WITHIN) | SF(THROW));
error step_type_destruct_mark(struct module *mod, struct node *node,
                              void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which == MODULE) {
    return 0;
  }

  inherit(mod, node);

  struct typ *not_typeable = TBI__NOT_TYPEABLE;
  struct node *first = node_subs_first(node);
  struct node *last = node_subs_last(node);

  switch (node->which) {
  case BIN:
    if (OP_KIND(node->as.BIN.operator) == OP_BIN_ACC) {
      mark_subs(mod, node, not_typeable, node_next(first), last, 1);
    }
    break;
  case CALL:
    if (first->typ == NULL) {
      // Otherwise, we are rewriting this expression and we should not touch
      // first.
      first->typ = TBI__CALL_FUNCTION_SLOT;
    }
    break;
  case INIT:
    if (!node->as.INIT.is_array) {
      mark_subs(mod, node, TBI__NOT_TYPEABLE, first, last, 2);
    }
    break;
  case WITHIN:
    if (node_subs_count_atleast(node, 1)
        && first->which != WITHIN) {
      // So it will not resolve in type_inference_ident(), but through
      // type_inference_within().
      first->typ = not_typeable;
    }
    break;
  case DEFFUN:
  case DEFMETHOD:
    first->typ = not_typeable;
    break;
  case DEFTYPE:
  case DEFINTF:
    first->typ = not_typeable;
    break;
  case DEFFIELD:
  case DEFARG:
  case DEFGENARG:
  case SETGENARG:
    first->typ = not_typeable;
    break;
  case MODULE_BODY:
    node->typ = not_typeable;
    break;
  case DEFCHOICE:
    first->typ = not_typeable;
    break;
  case THROW:
    if (node_subs_count_atleast(node, 2)) {
      first->typ = not_typeable;
    }
    break;
  default:
    assert(false && "Unreached");
    break;
  }

  return 0;
}

static STEP_FILTER(step_type_mutability_mark,
                   SF(BIN) | SF(UN));
static error step_type_mutability_mark(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  DSTEP(mod, node);
  struct typ *mutable = TBI__MUTABLE;
  struct typ *mercurial = TBI__MERCURIAL;

  struct node *first = node_subs_first(node);

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
      first->typ = mutable;
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

    switch (node->as.UN.operator) {
    case TREFDOT:
      // no-op
      break;
    case TREFBANG:
      if (first->typ != NULL) {
        if (first->which == BIN && !(first->flags & NODE_IS_TYPE)) {
          error e = typ_check_deref_against_mark(mod, first, TBI__MUTABLE,
                                                 first->as.BIN.operator);
          EXCEPT(e);
        }
      } else {
        first->typ = mutable;
      }
      break;
    case TREFWILDCARD:
    case TREFSHARP:
      if (first->typ != NULL) {
        if (first->which == BIN && !(first->flags & NODE_IS_TYPE)) {
          error e = typ_check_deref_against_mark(mod, first, TBI__MERCURIAL,
                                                 first->as.BIN.operator);
          EXCEPT(e);
        }
        return 0;
      } else {
        first->typ = mercurial;
      }
      break;
    default:
      break;
    }
    break;
  default:
    assert(false && "Unreached");
    break;
  }

  return 0;
}

static STEP_FILTER(step_type_gather_retval,
                   SF(DEFFUN) | SF(DEFMETHOD));
static error step_type_gather_retval(struct module *mod, struct node *node,
                                     void *user, bool *stop) {
  DSTEP(mod, node);

  module_retval_set(mod, node_fun_retval(node));

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

static STEP_FILTER(step_type_gather_excepts,
                   SF(TRY) | SF(DEFNAME) | SF(THROW));
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
    assert(false && "Unreached");
    break;
  }
  return 0;
}

static STEP_FILTER(step_excepts_store_label,
                   SF(DEFNAME) | SF(THROW));
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
    if (node_subs_count(node) == 2) {
      label_ident = node_subs_first(node);
    }
    break;
  default:
    assert(false && "Unreached");
    return 0;
  }

  struct node *tryy = module_excepts_get(mod)->tryy;
  struct node *eblock = node_subs_at(node_subs_first(tryy), 1);

  ident label = ID__NONE;
  error e;

  if (label_ident == NULL) {
    if (node_subs_count_atleast(eblock, 3)) {
      e = mk_except(mod, node,
                    "try block has multiple catch,"
                    " %s must use a label", which);
      THROW(e);
    }

    struct node *catch = node_subs_at(eblock, 1);
    assert(catch->which == CATCH);
    label = catch->as.CATCH.label;

  } else {
    if (!node_subs_count_atleast(eblock, 3)) {
      struct node *catch = node_subs_at(eblock, 1);
      assert(catch->which == CATCH);
      if (!catch->as.CATCH.is_user_label) {
        e = mk_except(mod, node,
                      "try block has a single catch without a label,"
                      " %s must not use a label",
                      which);
        THROW(e);
      }
    }

    struct node *def = NULL;
    e = scope_lookup(&def, mod, &node->scope, label_ident, false);
    EXCEPT(e);

    if (def->which != CATCH || scope_node(def->scope.parent) != eblock) {
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
    assert(false);
  }

  return 0;
}

static STEP_FILTER(step_rewrite_defname_no_expr,
                   SF(DEFNAME));
static error step_rewrite_defname_no_expr(struct module *mod, struct node *node,
                                          void *user, bool *stop) {
  DSTEP(mod, node);

  return 0;
}

static STEP_FILTER(step_rewrite_union_constructors,
                   SF(CALL));
static error step_rewrite_union_constructors(struct module *mod, struct node *node,
                                             void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *fun = node_subs_first(node);
  struct node *dfun = typ_definition(fun->typ);
  if (dfun->which != DEFTYPE
      || dfun->as.DEFTYPE.kind != DEFTYPE_UNION) {
    return 0;
  }

  struct node *member = NULL;
  error e = scope_lookup(&member, mod, &fun->scope, fun, false);
  EXCEPT(e);
  if (member->which != DEFCHOICE) {
    return 0;
  }

  struct node *mk_fun = mk_node(mod, node, BIN);
  node_subs_remove(node, mk_fun);
  node_subs_replace(node, fun, mk_fun);
  mk_fun->as.BIN.operator = TDOT;
  node_subs_append(mk_fun, fun);
  struct node *mk = mk_node(mod, mk_fun, IDENT);
  mk->as.IDENT.name = ID_MK;

  const struct node *except[] = { fun, NULL };
  e = catchup(mod, except, mk_fun, &node->scope, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  return 0;
}

static error do_instantiate(struct node **result,
                            struct module *mod, struct typ *t,
                            struct typ **explicit_args, size_t arity,
                            bool tentative) {
  assert(arity == 0 || arity == typ_generic_arity(t));

  struct node *gendef = typ_definition(t);
  struct node *pristine = node_toplevel(gendef)->generic->instances[0];
  struct node *instance = add_instance_deepcopy_from_pristine(mod, gendef,
                                                              pristine, tentative);
  set_typ(&node_toplevel(instance)->generic->our_generic_functor_typ, t);

  const size_t first = typ_generic_first_explicit_arg(t);
  struct node *genargs = node_subs_at(instance, IDX_GENARGS);
  struct node *ga = node_subs_first(genargs);
  for (size_t n = 0; n < first; ++n) {
    node_set_which(ga, SETGENARG);
    struct node *ga_arg = node_subs_last(ga);
    node_set_which(ga_arg, DIRECTDEF);
    set_typ(&ga_arg->as.DIRECTDEF.typ,
            typ_create_tentative(typ_generic_arg(t, n)));
    ga_arg->as.DIRECTDEF.flags = NODE_IS_TYPE;

    ga = node_next(ga);
  }

  for (size_t n = 0; n < arity; ++n) {
    node_set_which(ga, SETGENARG);
    struct node *ga_arg = node_subs_last(ga);
    node_set_which(ga_arg, DIRECTDEF);
    set_typ(&ga_arg->as.DIRECTDEF.typ, explicit_args[n]);
    ga_arg->as.DIRECTDEF.flags = NODE_IS_TYPE;

    ga = node_next(ga);
  }

  error e = catchup_instantiation(mod, node_module_owner(gendef),
                                  instance, gendef->scope.parent,
                                  tentative);
  EXCEPT(e);

  *result = instance;

  return 0;
}

static struct typ *find_existing_final(struct module *mod,
                                       struct typ *t,
                                       struct typ **args,
                                       size_t arity) {
  const size_t first = typ_generic_first_explicit_arg(t);
  assert(typ_generic_arity(t) - first == arity);

  const struct node *d = typ_definition_const(t);
  const struct toplevel *toplevel = node_toplevel_const(d);
  for (size_t n = 1; n < toplevel->generic->instances_count; ++n) {
    struct typ *i = toplevel->generic->instances[n]->typ;
    if (typ_is_tentative(i)) {
      continue;
    }

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

// Same as find_existing_final(), but with different arguments.
static struct typ *find_existing_final_for_tentative(struct module *mod,
                                                     const struct typ *t) {
  const struct node *d = typ_definition_const(typ_generic_functor_const(t));

  const struct toplevel *toplevel = node_toplevel_const(d);
  for (size_t n = 1; n < toplevel->generic->instances_count; ++n) {
    struct typ *i = toplevel->generic->instances[n]->typ;
    if (!typ_is_tentative(i) && typ_equal(t, i)) {
      return i;
    }
  }

  return NULL;
}

static error instance(struct node **result,
                      struct module *mod,
                      const struct node *for_error, size_t for_error_offset,
                      struct typ *t, struct typ **explicit_args, size_t arity) {
  const size_t first = typ_generic_first_explicit_arg(t);
  assert(arity == typ_generic_arity(t) - first);

  const bool tentative = instantiation_is_tentative(mod, t, explicit_args, arity);
  if (tentative) {
    struct typ **args = calloc(arity, sizeof(struct typ *));
    for (size_t n = 0; n < arity; ++n) {
      args[n] = typ_create_tentative(typ_generic_arg(t, n));
    }

    error e = do_instantiate(result, mod, t, args, arity, true);
    EXCEPT(e);

    const struct node *fe = node_subs_at_const(for_error, for_error_offset);
    for (size_t n = 0; n < arity; ++n) {
      e = unify(mod, fe,
                typ_generic_arg((*result)->typ, first + n),
                explicit_args[n]);
      EXCEPT(e);

      fe = node_next_const(fe);
    }

    free(args);
  } else {
    struct typ *r = find_existing_final(mod, t, explicit_args, arity);
    if (r != NULL) {
      if (result != NULL) {
        *result = typ_definition(r);
      }
      return 0;
    }
    error e = do_instantiate(result, mod, t, explicit_args, arity, false);
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
  struct node *term = node_subs_first(node);

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
    assert(false);
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
                     typ_create_tentative(TBI__REF_COMPATIBLE), &t, 1);
  assert(!e);

  return i->typ;
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

  struct node *left = node_subs_first(node);
  struct node *right = node_subs_last(node);
  const enum token_type operator = node->as.BIN.operator;

  error e;
  if (operator == TASSIGN) {
    e = check_assign_not_types(mod, left, right);
    EXCEPT(e);

    e = unify(mod, node,
              try_wrap_ref_compatible(mod, node, 0, left->typ),
              right->typ);
    EXCEPT(e);

    left->flags |= right->flags & NODE__ASSIGN_TRANSITIVE;
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

static error fill_in_optional_args(struct module *mod, struct node *node,
                                   const struct typ *tfun) {
  const struct node *dfun = typ_definition_const(tfun);
  const size_t dmin = node_fun_min_args_count(dfun);
  const size_t dmax = node_fun_max_args_count(dfun);

  if (dmin == dmax) {
    return 0;
  }

  ssize_t first_vararg;
  switch (dfun->which) {
  case DEFFUN:
    first_vararg = dfun->as.DEFFUN.first_vararg;
    break;
  case DEFMETHOD:
    first_vararg = dfun->as.DEFMETHOD.first_vararg;
    break;
  default:
    assert(false);
    break;
  }

  const struct node *darg = node_subs_at_const(
    node_subs_at_const(dfun, IDX_FUNARGS), dmin);
  struct node *arg;
  if (dmin == 0) {
    // We use this form, so that 'arg' will be NULL if 'node' it's a unary call.
    arg = node_next(node_subs_first(node));
  } else {
    arg = node_subs_at(node, dmin + 1);
  }
  ssize_t n;
  error e;

  for (n = dmin; n < dmax && (first_vararg == - 1 || n < first_vararg); ++n) {
    if (arg == NULL) {
      G(named, node, CALLNAMEDARG,
        named->as.CALLNAMEDARG.name = node_ident(darg);
        G(nul, named, NUL));
      e = catchup(mod, NULL, named, &node->scope, CATCHUP_BELOW_CURRENT);
      assert(!e);

    } else if (arg->which != CALLNAMEDARG) {
      // Assume this is the first vararg

      if (first_vararg == -1) {
        e = mk_except(mod, arg, "excessive positional argument"
                      " or optional argument lacks a name");
        THROW(e);
      }

      G(named, node, CALLNAMEDARG,
        named->as.CALLNAMEDARG.name = node_ident(darg);
        G(nul, named, NUL));

      node_subs_remove(node, named);
      node_subs_insert_before(node, arg, named);

      e = catchup(mod, NULL, named, &node->scope, CATCHUP_BELOW_CURRENT);
      assert(!e);

    } else if (arg->which == CALLNAMEDARG) {
      const ident name = node_ident(arg);
      while (node_ident(darg) != name) {
        darg = node_next_const(darg);
        if (darg == NULL) {
          e = mk_except(mod, arg, "named argument '%s' has bad name"
                        " or appears out of order",
                        idents_value(mod->gctx, name));
          THROW(e);
        }
      }

      arg = node_next(arg);
    }

    darg = node_next_const(darg);
  }

  assert(arg == NULL || first_vararg >= 0);
  while (arg != NULL) {
    if (arg->which == CALLNAMEDARG) {
      const ident name = node_ident(arg);
      e = mk_except(mod, arg, "excess named argument '%s' or appears out of order",
                    idents_value(mod->gctx, name));
      THROW(e);
    }
    arg = node_next(arg);
  }

  return 0;
}

static error rewrite_unary_call(struct module *mod, struct node *node, struct typ *tfun) {
  struct scope *parent_scope = node->scope.parent;

  struct node *fun = node_new_subnode(mod, node);
  node_subs_remove(node, fun);
  node_move_content(fun, node);
  node_subs_append(node, fun);
  node_set_which(node, CALL);
  set_typ(&fun->typ, tfun);

  const struct node *except[] = { fun, NULL };
  error e = catchup(mod, except, node, parent_scope, CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  e = fill_in_optional_args(mod, node, tfun);
  assert(!e);

  return 0;
}

static void bin_accessor_maybe_ref(struct scope **parent_scope,
                                   struct module *mod, struct node *parent) {
  if (typ_is_reference(parent->typ)) {
    *parent_scope = &typ_definition(typ_generic_arg(parent->typ, 0))->scope;
  }
}

static void bin_accessor_maybe_defchoice(struct scope **parent_scope, struct node *for_error,
                                         struct module *mod, struct node *parent) {
  ident tag = ID__NONE;
  if (constraint_has_common_root_tag(&tag, mod, parent)) {
    struct node *defchoice = NULL;
    error e = scope_lookup_ident_immediate(&defchoice, for_error, mod,
                                           &typ_definition(parent->typ)->scope,
                                           tag, false);
    assert(!e);
    assert(defchoice->which == DEFCHOICE);

    *parent_scope = &defchoice->scope;
  }
}

static error type_inference_bin_accessor(struct module *mod, struct node *node) {
  error e;

  enum token_type operator = node->as.BIN.operator;
  const struct typ *mark = node->typ;

  struct node *container = node_subs_first(node);
  struct scope *container_scope = &typ_definition(container->typ)->scope;
  bin_accessor_maybe_ref(&container_scope, mod, container);
  bin_accessor_maybe_defchoice(&container_scope, node, mod, container);

  const bool container_is_tentative = typ_is_tentative(scope_node(container_scope)->typ);

  struct node *name = node_subs_last(node);
  struct node *field = NULL;
  e = scope_lookup_ident_immediate(&field, name, mod, container_scope,
                                   node_ident(name), container_is_tentative);
  if (container_is_tentative && e == EINVAL) {
    struct node *dinc = defincomplete_create(mod, node);
    defincomplete_add_field(mod, node, dinc, node_ident(name), TBI_ANY);
    e = defincomplete_catchup(mod, dinc);
    EXCEPT(e);

    e = unify(mod, node, container->typ, dinc->typ);
    EXCEPT(e);

    e = scope_lookup_ident_immediate(&field, name, mod, &dinc->scope,
                                     node_ident(name), false);
    EXCEPT(e);
  } else {
    EXCEPT(e);
  }

  if (field->which == IMPORT && !field->as.IMPORT.intermediate_mark) {
    e = scope_lookup(&field, mod, &mod->gctx->modules_root.scope,
                     node_subs_first(field), false);
    assert(!e);
  }

  if (typ_is_function(field->typ) && mark != TBI__CALL_FUNCTION_SLOT) {
    const bool is_method = typ_definition_const(field->typ)->which == DEFMETHOD;
    if (node_fun_min_args_count(field) != (is_method ? 1 : 0)) {
      e = mk_except_call_args_count(mod, node, field, is_method, 0);
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
    if (!(node->flags & NODE_IS_TEMPORARY)) {
      e = typ_check_deref_against_mark(mod, node, mark, operator);
      EXCEPT(e);
    }
  }

  return 0;
}

static error type_inference_bin_rhs_unsigned(struct module *mod, struct node *node) {
  error e;
  struct node *left = node_subs_first(node);
  struct node *right = node_subs_last(node);

  e = unify(mod, right, right->typ, TBI_U32);
  EXCEPT(e);

  set_typ(&node->typ, typ_create_tentative(TBI_BITWISE));
  e = unify(mod, node, left->typ, node->typ);
  EXCEPT(e);

  return 0;
}

static error type_inference_bin_rhs_type(struct module *mod, struct node *node) {
  error e;
  struct node *left = node_subs_first(node);
  struct node *right = node_subs_last(node);

  if (!(right->flags & NODE_IS_TYPE)) {
    e = mk_except_type(mod, right, "right-hand side not a type");
    THROW(e);
  }

  e = unify(mod, node, left->typ, right->typ);
  EXCEPT(e);

  set_typ(&node->typ, left->typ);

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
    assert(false);
    return 0;
  }
}

static error typ_tuple(struct node **result, struct module *mod, struct node *node) {
  const size_t arity = node_subs_count(node);
  struct typ **args = calloc(arity, sizeof(struct typ *));
  size_t n = 0;
  FOREACH_SUB(s, node) {
    args[n] = s->typ;
    n += 1;
  }

  error e = instance(result, mod, node, 0,
                     typ_lookup_builtin_tuple(mod, arity), args, arity);
  EXCEPT(e);

  free(args);

  return 0;
}

static error type_inference_tuple(struct module *mod, struct node *node) {
  size_t n = 0;
  FOREACH_SUB(s, node) {
    if (n > 0 && (node->flags & NODE_IS_TYPE) != (s->flags & NODE_IS_TYPE)) {
      error e = mk_except_type(mod, s, "tuple combines values and types");
      THROW(e);
    }
    node->flags |= (s->flags & NODE__TRANSITIVE);
    n += 1;
  }

  struct node *i = NULL;
  error e = typ_tuple(&i, mod, node);
  EXCEPT(e);

  set_typ(&node->typ, i->typ);

  return 0;
}

static error type_inference_tupleextract(struct module *mod, struct node *node) {
  struct node *expr = node_subs_last(node);
  assert(node_subs_count(node) == typ_generic_arity(expr->typ) + 1
         && typ_isa(expr->typ, TBI_ANY_TUPLE));

  size_t n = 0;
  FOREACH_SUB(s, node) {
    if (node_next(s) == NULL) {
      break;
    }
    set_typ(&s->typ, typ_generic_arg(expr->typ, n));
    n += 1;
  }

  struct node *value = node_subs_last(node);
  set_typ(&node->typ, value->typ);
  node->flags = value->flags; // Copy all flags, transparent node.

  return 0;
}

static void type_inference_init_named(struct module *mod, struct node *node) {
  struct node *dinc = defincomplete_create(mod, node);

  FOREACH_SUB_EVERY(s, node, 0, 2) {
    const struct node *left = s;
    const struct node *right = node_next(s);
    defincomplete_add_field(mod, s, dinc, node_ident(left), right->typ);
  }

  error e = defincomplete_catchup(mod, dinc);
  assert(!e);
  set_typ(&node->typ, typ_create_tentative(dinc->typ));
}

static error type_inference_init_array(struct module *mod, struct node *node) {
  struct typ *el = typ_create_tentative(typ_generic_arg(TBI_STATIC_ARRAY, 0));
  struct node *i = NULL;
  error e = instance(&i, mod, node, 0,
                     TBI_STATIC_ARRAY, &el, 1);
  EXCEPT(e);

  set_typ(&node->typ, typ_create_tentative(i->typ));

  FOREACH_SUB(s, node) {
    error e = unify(mod, s, s->typ,
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

  if (node_subs_count_atleast(node, 1)) {
    struct node *arg = node_subs_first(node);
    error e = unify(mod, arg, arg->typ,
                    try_wrap_ref_compatible(mod, node, 0,
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

static struct node *expr_ref(struct module *mod, struct node *parent,
                             enum token_type refop, struct node *node) {
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

  struct node *n = mk_node(mod, parent, UN);
  n->as.UN.operator = refop;
  node_subs_append(n, node);
  return n;
}

static error rewrite_self(struct module *mod, struct node *node,
                          struct node *fun) {
  assert(fun->which == BIN);

  struct node *old_self = node_subs_first(fun);
  struct node *self;
  if (typ_is_reference(old_self->typ)) {
    node_subs_remove(fun, old_self);
    node_subs_insert_after(node, node_subs_first(node), old_self);
    self = old_self;
  } else {
    node_subs_remove(fun, old_self);
    enum token_type access = refop_for_accop[fun->as.BIN.operator];
    struct node *s = expr_ref(mod, node, access, old_self);
    node_subs_remove(node, s);
    node_subs_insert_after(node, node_subs_first(node), s);
    self = s;
  }

  const struct node *except[] = { old_self, NULL };
  error e = catchup(mod, except, self, &node->scope, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  if (typ_is_reference(self->typ)) {
    e = typ_check_can_deref(mod, fun, self->typ,
                            derefop_for_accop[fun->as.BIN.operator]);
    EXCEPT(e);
  }

  return 0;
}

static error try_insert_const_ref(struct module *mod, struct node *node,
                                  const struct typ *target, struct node *arg) {
  struct node *parent = node;
  struct node *real_arg = arg;
  const bool is_named = arg->which == CALLNAMEDARG;
  if (is_named) {
    parent = arg;
    real_arg = node_subs_first(arg);
  }

  if (typ_is_reference(target) && !typ_is_reference(real_arg->typ)) {
    if (typ_isa(target, TBI_ANY_ANY_REF)
        && !typ_isa(target, TBI_ANY_MUTABLE_REF)) {
      struct node *before = node_prev(real_arg);

      node_subs_remove(parent, real_arg);
      struct node *ref_arg = expr_ref(mod, parent, TREFDOT, real_arg);
      node_subs_remove(parent, ref_arg);
      node_subs_insert_after(parent, before, ref_arg);

      if (is_named) {
        unset_typ(&arg->typ);
      }

      const struct node *except[] = { real_arg, NULL };
      error e = catchup(mod, except,
                        is_named ? arg : ref_arg,
                        &node->scope,
                        CATCHUP_BELOW_CURRENT);
      EXCEPT(e);
    }
  } else if (typ_is_reference(target) && typ_is_reference(real_arg->typ)) {
    if (real_arg->which == UN
        && real_arg->as.UN.operator == TREFDOT
        && real_arg->as.UN.is_explicit) {
      error e = mk_except(mod, real_arg, "explicit '@' operators are not"
                          " allowed for unqualified const references");
      THROW(e);
    }
  }

  return 0;
}

static error process_const_ref_call_arguments(struct module *mod,
                                              struct node *node,
                                              const struct typ *tfun) {
  if (!node_subs_count_atleast(node, 2)) {
    return 0;
  }

  const struct node *dfun = typ_definition_const(tfun);
  const ssize_t first_vararg = node_fun_first_vararg(dfun);

  error e;
  ssize_t n = 0;
  struct node *last = NULL;
  struct node *next = node_subs_at(node, 1);
  while (next != NULL) {
    if (n == first_vararg) {
      break;
    }

    // We record 'next' now as try_insert_const_ref() may move 'arg'.
    struct node *arg = next;
    next = node_next(next);

    e = try_insert_const_ref(mod, node,
                             typ_function_arg_const(tfun, n), arg);
    EXCEPT(e);

    n += 1;
    last = arg;
  }

  if (n == first_vararg) {
    const struct typ *target = typ_generic_arg_const(
      typ_function_arg_const(tfun, n), 0);

    struct node *next = last == NULL
      ? node_next(node_subs_first(node)) : node_next(last);
    while (next != NULL) {
      // We record 'next' now as try_insert_const_ref() may move 'arg'.
      struct node *arg = next;
      next = node_next(next);

      e = try_insert_const_ref(mod, node, target, arg);
      EXCEPT(e);
    }
  }

  return 0;
}

static error prepare_call_arguments(struct module *mod, struct node *node) {
  error e;
  struct node *fun = node_subs_first(node);

  const struct node *dfun = typ_definition_const(fun->typ);
  const size_t dmin = node_fun_min_args_count(dfun);
  const size_t dmax = node_fun_max_args_count(dfun);

  const size_t args = node_subs_count(node) - 1;

  switch (dfun->which) {
  case DEFFUN:
    if (args < dmin || args > dmax) {
      e = mk_except_call_args_count(mod, node, dfun, false, args);
      THROW(e);
    }
    break;
  case DEFMETHOD:
    if (fun->which == BIN) {
      if ((node_subs_first(fun)->flags & NODE_IS_TYPE)) {
        // Form (type.method self ...).
        if (args < dmin || args > dmax) {
          e = mk_except_call_args_count(mod, node, dfun, false, args);
          THROW(e);
        }
      } else {
        // Form (self.method ...); rewrite as (type.method self ...).
        if (args+1 < dmin || args+1 > dmax) {
          e = mk_except_call_args_count(mod, node, dfun, true, args);
          THROW(e);
        }

        struct node *m = mk_node(mod, node, DIRECTDEF);
        set_typ(&m->as.DIRECTDEF.typ, fun->typ);
        m->as.DIRECTDEF.flags = NODE_IS_TYPE;
        node_subs_remove(node, m);
        node_subs_replace(node, fun, m);

        e = rewrite_self(mod, node, fun);
        EXCEPT(e);

        e = catchup(mod, NULL, m, &node->scope, CATCHUP_BELOW_CURRENT);
        EXCEPT(e);
      }
    } else if ((fun->flags & NODE_IS_TYPE)) {
      assert(fun->which == CALL || fun->which == DIRECTDEF);
      // Generic method instantiation: (type.method u32 i32) self
      // or DIRECTDEF.
      if (args < dmin || args > dmax) {
        e = mk_except_call_args_count(mod, node, dfun, false, args);
        THROW(e);
      }
    } else {
      assert(false && "Unreached");
    }
    break;
  default:
    assert(false);
  }

  e = fill_in_optional_args(mod, node, fun->typ);
  EXCEPT(e);

  e = process_const_ref_call_arguments(mod, node, fun->typ);
  EXCEPT(e);

  return 0;
}

static error explicit_instantiation(struct module *mod, struct node *node) {
  error e;
  struct typ *t = node_subs_first(node)->typ;
  const size_t arity = node_subs_count(node) - 1;

  const size_t first = typ_generic_first_explicit_arg(t);
  const size_t explicit_arity = typ_generic_arity(t) - first;
  if (arity != explicit_arity) {
    e = mk_except_type(mod, node,
                       "invalid number of explicit generic arguments:"
                       " %zu expected, but %zu given",
                       explicit_arity, arity);
    THROW(e);
  }

  struct typ **args = calloc(arity, sizeof(struct typ *));
  size_t n = 0;
  FOREACH_SUB_EVERY(s, node, 1, 1) {
    args[n] = s->typ;
    n += 1;
  }

  struct node *i = NULL;
  e = instance(&i, mod, node, 1, t, args, arity);
  EXCEPT(e);
  free(args);

  set_typ(&node->typ, i->typ);
  record_topdep(mod, i->typ);
  node->flags |= NODE_IS_TYPE;

  return 0;
}

static error implicit_function_instantiation(struct module *mod, struct node *node) {
  error e;
  struct typ *tfun = node_subs_first(node)->typ;
  const size_t arity = node_subs_count(node) - 1;

  // Already checked in prepare_call_arguments().
  assert(arity == typ_function_arity(tfun));

  const size_t gen_arity = typ_generic_arity(tfun);
  struct typ **args = calloc(gen_arity, sizeof(struct typ *));
  for (size_t n = 0; n < gen_arity; ++n) {
    args[n] = typ_create_tentative(typ_generic_arg(tfun, n));
  }

  struct node *i = NULL;
  e = instance(&i, mod, typ_definition_const(tfun), 0,
               tfun, args, gen_arity);
  assert(!e);

  size_t n = 0;
  FOREACH_SUB_EVERY(s, node, 1, 1) {
    e = unify(mod, s, s->typ,
              try_wrap_ref_compatible(mod, node, 1+n,
                                      typ_function_arg(i->typ, n)));
    EXCEPT(e);
    n += 1;
  }

  free(args);

  set_typ(&node_subs_first(node)->typ, i->typ);
  record_topdep(mod, i->typ);
  set_typ(&node->typ, typ_function_return(i->typ));

  return 0;
}

static error function_instantiation(struct module *mod, struct node *node) {
  assert(node_subs_count_atleast(node, 2));

  error e;
  if (node_subs_at(node, 1)->flags & NODE_IS_TYPE) {
    e = explicit_instantiation(mod, node);
    EXCEPT(e);
  } else {
    e = implicit_function_instantiation(mod, node);
    EXCEPT(e);
  }

  return 0;
}

static error check_consistent_either_types_or_values(struct module *mod,
                                                     struct node *arg0) {
  uint32_t flags = 0;
  for (struct node *s = arg0; s != NULL; s = node_next(s)) {
    if (s != arg0 && (flags & NODE_IS_TYPE) != (s->flags & NODE_IS_TYPE)) {
      error e = mk_except_type(mod, s, "expression combines types and values");
      THROW(e);
    }
    flags |= s->flags;
  }

  return 0;
}

static error type_inference_explicit_unary_call(struct module *mod, struct node *node, struct node *dfun) {
  const size_t subs_count = node_subs_count(node);
  if (dfun->which == DEFFUN && subs_count != 1) {
    error e = mk_except_call_args_count(mod, node, dfun, false, subs_count - 1);
    THROW(e);
  } else if (dfun->which == DEFMETHOD && subs_count != 2) {
    error e = mk_except_call_args_count(mod, node, dfun, false, subs_count - 1);
    THROW(e);
  }

  if (dfun->which == DEFMETHOD) {
    struct node *self = node_subs_at(node, 1);
    error e = unify(mod, self, self->typ,
                    try_wrap_ref_compatible(mod, node, 1,
                                            typ_function_arg(dfun->typ, 0)));
    EXCEPT(e);
  }

  set_typ(&node->typ, typ_function_return(dfun->typ));

  return 0;
}

static error type_inference_call(struct module *mod, struct node *node) {
  error e;
  struct node *fun = node_subs_first(node);
  struct typ *tfun = fun->typ;
  struct node *dfun = typ_definition(tfun);

  if (!node_is_fun(dfun)
      || (node_subs_count_atleast(node, 2)
          && (node_subs_at(node, 1)->flags & NODE_IS_TYPE))) {

    if (!node_is_fun(dfun)
        && (!node_can_have_genargs(dfun)
            || !node_subs_count_atleast(node_subs_at(dfun, IDX_GENARGS), 1))) {
      e = mk_except_type(mod, fun, "not a generic type");
      THROW(e);
    }

    e = explicit_instantiation(mod, node);
    EXCEPT(e);

    return 0;
  }

  e = prepare_call_arguments(mod, node);
  EXCEPT(e);

  e = check_consistent_either_types_or_values(mod, try_node_subs_at(node, 1));
  EXCEPT(e);

  node->flags |= NODE_IS_TEMPORARY;

  if (node_subs_count_atleast(node_subs_at(dfun, IDX_GENARGS), 1)
      && node_toplevel_const(dfun)->generic->our_generic_functor_typ == NULL) {
    e = function_instantiation(mod, node);
    EXCEPT(e);

    return 0;
  }

  if (node_fun_max_args_count(dfun) == 0) {
    return type_inference_explicit_unary_call(mod, node, dfun);
  }

  const ssize_t first_vararg = node_fun_first_vararg(dfun);
  ssize_t n = 0;
  FOREACH_SUB_EVERY(arg, node, 1, 1) {
    if (n == first_vararg) {
      break;
    }
    e = unify(mod, arg, arg->typ,
              try_wrap_ref_compatible(mod, node, 1+n,
                                      typ_function_arg(tfun, n)));
    EXCEPT(e);
    n += 1;
  }

  if (n == first_vararg) {
    struct typ *target = try_wrap_ref_compatible(
      mod, node, 1,
      typ_generic_arg(typ_function_arg(tfun, n), 0));

    FOREACH_SUB_EVERY(arg, node, 1 + n, 1) {
      e = unify(mod, arg, arg->typ, target);
      EXCEPT(e);
    }
  }

  set_typ(&node->typ, typ_function_return(tfun));

  return 0;
}

static error type_inference_block(struct module *mod, struct node *node) {
  error e;

  FOREACH_SUB(s, node) {
    if ((s->flags & NODE_IS_TYPE)) {
      e = mk_except_type(mod, s, "block statements cannot be type names");
      THROW(e);
    }
  }

  FOREACH_SUB(s, node) {
    if (node_next(s) == NULL) {
      break;
    }
    if (!typ_equal(s->typ, TBI_VOID)) {
      e = mk_except_type(mod, s,
                         "intermediate statements in a block must be of type void"
                         " (except the last one), not '%s'",
                         typ_pretty_name(mod, s->typ));
      THROW(e);
    }
  }

  if (node_subs_last(node)->which == RETURN) {
    // FIXME: should make sure there are no statements after a RETURN.
    set_typ(&node->typ, TBI_VOID);
  } else {
    set_typ(&node->typ, node_subs_last(node)->typ);
  }

  return 0;
}

static error type_inference_if(struct module *mod, struct node *node) {
  error e;

  FOREACH_SUB_EVERY(s, node, 0, 2) {
    if (node_next_const(s) == NULL) {
      break;
    }
    e = unify(mod, s, s->typ,
              typ_create_tentative(TBI_GENERALIZED_BOOLEAN));
    EXCEPT(e);
  }

  set_typ(&node->typ, node_subs_at(node, 1)->typ);

  struct node *last_elif = node_subs_at(node, 1);
  if (node_subs_count_atleast(node, 3)) {
    FOREACH_SUB_EVERY(b, node, 3, 2) {
      e = unify(mod, b, node->typ, b->typ);
      EXCEPT(e);
      last_elif = b;
    }
  }

  if (last_elif != node_subs_last(node)) {
    struct node *els = node_next(last_elif);
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
  const bool enum_or_union = d->as.DEFTYPE.kind == DEFTYPE_ENUM
    || d->as.DEFTYPE.kind == DEFTYPE_UNION;

  error e;
  if (!enum_or_union) {
    e = mk_except_type(mod, expr,
                       "must match over an enum or sum type (FIXME: for now)");
    THROW(e);
  }

  if (node_ident(pattern) == ID_OTHERWISE) {
    set_typ(&pattern->typ, expr->typ);
    return 0;
  }

  if (d->which == DEFTYPE
      && enum_or_union
      && pattern->which == IDENT) {
    struct node *field = NULL;
    e = scope_lookup_ident_immediate(&field, pattern, mod,
                                     &d->scope,
                                     node_ident(pattern),
                                     true);
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

  struct node *expr = node_subs_first(node);
  FOREACH_SUB_EVERY(s, node, 1, 2) {
    e = unify_match_pattern(mod, expr, s);
    EXCEPT(e);
  }

  set_typ(&node->typ, node_subs_at(node, 2)->typ);
  if (node_subs_count_atleast(node, 4)) {
    FOREACH_SUB_EVERY(s, node, 4, 2) {
      e = unify(mod, s, s->typ, node->typ);
      EXCEPT(e);
    }
  }

  return 0;
}

static error unify_try_errors(struct typ **exu, struct module *mod,
                              struct try_state *st) {
  for (size_t n = 0, count = vecnode_count(&st->excepts); n < count; ++n) {
    struct node *exc = vecnode_get(&st->excepts, n);

    switch (exc->which) {
    case THROW:
      if (node_subs_count(exc) == 2) {
        exc = node_subs_at(exc, 1);
      } else {
        exc = node_subs_first(exc);
      }
      break;
    case DEFNAME:
      exc = exc->as.DEFNAME.expr;
      break;
    default:
      assert(false);
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

  if (vecnode_count(&st->excepts) == 0) {
    e = mk_except(mod, node,
                  "try block has no except or throw statement,"
                  " catch is unreachable");
    THROW(e);
  }

  struct typ *exu = NULL;
  e = unify_try_errors(&exu, mod, st);
  EXCEPT(e);

#define f(n) node_subs_first(n)
#define s(n) node_subs_at(n, 1)
  struct node *elet = f(node);
  struct node *edefp = f(elet);
  struct node *eident = f(edefp);
  set_typ(&eident->typ, exu);

  struct node *eblock = s(elet);
  struct node *main_block = f(eblock);
  struct typ *u = main_block->typ;

  FOREACH_SUB_EVERY(catch, eblock, 1, 1) {
    struct node *let = f(catch);
    struct node *defp = f(let);
    struct node *error = s(defp);
    struct node *block = s(catch);

    e = unify(mod, error, error->typ, exu);
    EXCEPT(e);

    e = unify(mod, block, block->typ, u);
    EXCEPT(e);
  }
#undef s
#undef f

  set_typ(&node->typ, u);

  return 0;
}

static bool in_a_body_pass(struct module *mod) {
  return mod->stage->state->passing >= PASSZERO_COUNT + PASSFWD_COUNT;
}

static error type_inference_ident_unknown(struct module *mod, struct node *node) {
  error e;
  if (!in_a_body_pass(mod)) {
    e = mk_except(mod, node, "unknown ident '%s'",
                  idents_value(mod->gctx, node_ident(node)));
    EXCEPT(e);
  }

  struct node *unk = defincomplete_create(mod, node);
  defincomplete_set_ident(mod, node, unk, node_ident(node));
  e = defincomplete_catchup(mod, unk);
  EXCEPT(e);

  // Special marker, so we can rewrite it with the final enum or sum scope
  // in step_check_no_unknown_ident_left().
  node->as.IDENT.non_local_scope = &unk->scope;

  set_typ(&node->typ, typ_create_tentative(unk->typ));
  return 0;
}

static struct phi_tracker_state *ident_def_phi_tracker(struct node *def) {
  switch (def->which) {
  case DEFNAME:
    if (def->as.DEFNAME.phi_state == NULL) {
      PUSH_STATE(def->as.DEFNAME.phi_state);
    }
    return def->as.DEFNAME.phi_state;
  case DEFARG:
    if (def->as.DEFARG.phi_state == NULL) {
      PUSH_STATE(def->as.DEFARG.phi_state);
    }
    return def->as.DEFARG.phi_state;
  default:
    assert(false);
    return NULL;
  }
}

static struct node *insert_conditioned_phi(struct module *mod,
                                           struct branch_state *br_st,
                                           struct node *pre_branch_use,
                                           bool is_after_current) {
  if (br_st->branching->which == TRY) {
    // FIXME: unsupported in try/catch
    return pre_branch_use;
  }

  struct node *block = node_branching_nth_branch(br_st->branching, br_st->nth_branch);
  assert(block->which == BLOCK);
  struct node *phi = mk_node(mod, block, PHI);
  phi->as.PHI.is_conditioned = true;
  vecnode_push(&phi->as.PHI.ancestors, pre_branch_use);

  node_subs_remove(block, phi);
  node_subs_insert_before(block, node_subs_first(block), phi);

  error e = catchup(mod, NULL, phi, &block->scope,
                    is_after_current ? CATCHUP_AFTER_CURRENT
                    : CATCHUP_BEFORE_CURRENT);
  assert(!e);

  return phi;
}

static void track_ident_use(struct module *mod, struct node *node,
                            struct node *def, bool is_after_current) {
  assert(node->which == IDENT);
  node->as.IDENT.def = def;

  // FIXME: inserting PHI under a branching for names defined in the current
  // branch block, unnecessarily.

  switch (def->which) {
  case DEFGENARG:
  case SETGENARG:
  case DEFFUN:
  case DEFMETHOD:
  case DEFTYPE:
  case DEFINTF:
    node->as.IDENT.prev_use = def;
    return;

  case DEFNAME:
  case DEFARG:
    break;
  default:
    return;
  }

#define OR_ELSE(p, def) ( (p) != NULL ? (p) : (def) )

  struct phi_tracker_state *phi_st = ident_def_phi_tracker(def);
  struct branch_state *br_st = mod->state->branch_state;

  if (br_st == NULL || br_st->nth_branch == -1) {
    // Not in a branch.

    node->as.IDENT.prev_use = OR_ELSE(phi_st->last, def);
    phi_st->last = node;
    return;
  }

  assert(br_st->branching != NULL);

  if (br_st->branching == phi_st->branching && br_st->nth_branch == phi_st->nth_branch) {
    // We've already seen a use under this branching node and this branch.
    node->as.IDENT.prev_use
      = OR_ELSE(phi_st->per_branch_last[phi_st->nth_branch], def);
    phi_st->per_branch_last[phi_st->nth_branch] = node;
    return;
  }

  if (br_st->branching == phi_st->branching && br_st->nth_branch != phi_st->nth_branch) {
    // We've already seen a use under this branching node, but in a previous
    // branch.
    phi_st->nth_branch = br_st->nth_branch;

    struct node *phi = insert_conditioned_phi(mod, br_st,
                                              OR_ELSE(phi_st->prev->last, def),
                                              is_after_current);
    if (!is_after_current) {
      node->as.IDENT.prev_use = phi;
      phi_st->per_branch_last[phi_st->nth_branch] = node;
    } else {
      phi_st->per_branch_last[phi_st->nth_branch] = phi;
    }
    return;
  }

  // This is the first use under this branching node.
  switch (def->which) {
  case DEFNAME:
    PUSH_STATE(def->as.DEFNAME.phi_state);
    phi_st = def->as.DEFNAME.phi_state;
    break;
  case DEFARG:
    PUSH_STATE(def->as.DEFARG.phi_state);
    phi_st = def->as.DEFARG.phi_state;
    break;
  default:
    assert(false);
  }
  phi_st->branching = br_st->branching;
  phi_st->nth_branch = br_st->nth_branch;
  phi_st->per_branch_last = calloc(
    node_branching_exhaustive_branch_count(phi_st->branching),
    sizeof(*phi_st->per_branch_last));

  struct node *phi = insert_conditioned_phi(mod, br_st,
                                            OR_ELSE(phi_st->prev->last, def),
                                            is_after_current);
  if (!is_after_current) {
    node->as.IDENT.prev_use = phi;
    phi_st->per_branch_last[phi_st->nth_branch] = node;
  } else {
    phi_st->per_branch_last[phi_st->nth_branch] = phi;
  }

  if (nodeset_count(&br_st->need_phi) == 0) {
    nodeset_init(&br_st->need_phi, 0);
    nodeset_set_delete_val(&br_st->need_phi, false);
    nodeset_set_custom_hashf(&br_st->need_phi, node_ptr_hash);
    nodeset_set_custom_cmpf(&br_st->need_phi, node_ptr_cmp);
  }
  nodeset_set(&br_st->need_phi, def, true);

#undef OR_ELSE
}

static void track_match_ident_use(struct module *mod, struct node *pattern) {
  assert(pattern->which == IDENT);
  struct node *parent = node_parent(pattern);
  assert(parent->which == MATCH);
  struct node *expr = node_subs_first(parent);
  assert(expr->which == IDENT);

  struct node *def = NULL;
  error e = scope_lookup(&def, mod, &expr->scope, expr, false);
  assert(!e);

  track_ident_use(mod, expr, def, true);
}

static STEP_FILTER(step_branch_down,
                   STEP_FILTER_BRANCHING);
static error step_branch_down(struct module *mod, struct node *node,
                              void *user, bool *stop) {
  DSTEP(mod, node);
  PUSH_STATE(mod->state->branch_state);
  struct branch_state *st = mod->state->branch_state;
  st->branching = node;
  st->nth_branch = -1;
  return 0;
}

static size_t find_in_parent(const struct node *parent, const struct node *node) {
  size_t n = 0;
  FOREACH_SUB_CONST(s, parent) {
    if (s == node) {
      return n;
    }
    n += 1;
  }
  assert(false);
  return 0;
}

static STEP_FILTER(step_branch_block_down,
                   SF(BLOCK));
static error step_branch_block_down(struct module *mod, struct node *node,
                                    void *user, bool *stop) {
  DSTEP(mod, node);
  const struct node *parent = node_parent_const(node);
  const size_t nth_sub = find_in_parent(parent, node);
  struct branch_state *st = mod->state->branch_state;

  switch (parent->which) {
  case WHILE:
  case FOR:
    st->nth_branch = 0;
    break;
  case IF:
    if (nth_sub % 2 == 0) {
      // When the condition is itself a block 'if (block -> cond)'.
      st->nth_branch = -1;
    } else if (nth_sub % 2 == 1) {
      st->nth_branch = nth_sub / 2;
    } else if (node_subs_last_const(parent) == node) {
      // Else branch.
      st->nth_branch = nth_sub / 2 + 1;
    }
    break;
  case MATCH:
    assert(nth_sub % 2 == 0);
    st->nth_branch = (nth_sub - 2) / 2;

    track_match_ident_use(mod, node_prev(node));
    break;
  case TRY:
    st->nth_branch = nth_sub;
    break;
  default:
    return 0;
  }

  assert(st->branching == parent);

  return 0;
}

static STEP_FILTER(step_branch_block_up,
                   SF(BLOCK));
static error step_branch_block_up(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  DSTEP(mod, node);
  struct branch_state *st = mod->state->branch_state;
  if (st == NULL) {
    return 0;
  }

  const struct node *parent = node_parent_const(node);
  if (st->branching == parent) {
    st->nth_branch = -1;
  }

  return 0;
}

struct phi_insertion_state {
  struct module *mod;
  struct node *branching;
  struct node *parent;
};

static int phi_insertion_foreach(const struct node **_def, bool *value, void *user) {
  struct node *def = (struct node *) *_def;
  struct phi_insertion_state *st = user;

  assert(def->which == DEFNAME || def->which == DEFARG);
  struct phi_tracker_state *phi_st = ident_def_phi_tracker(def);

  struct node *phi = mk_node(st->mod, st->parent, PHI);
  node_subs_remove(st->parent, phi);
  node_subs_insert_after(st->parent, st->branching, phi);

  for (size_t nth = 0, count = node_branching_exhaustive_branch_count(st->branching);
       nth < count; ++nth) {
    struct node *use = phi_st->per_branch_last[nth];
    if (use != NULL) {
      vecnode_push(&phi->as.PHI.ancestors, use);
    }
  }

  error e = catchup(st->mod, NULL, phi, &st->parent->scope, CATCHUP_AFTER_CURRENT);
  assert(!e);

  phi_st->prev->last = phi;
  switch (def->which) {
  case DEFNAME:
    POP_STATE(def->as.DEFNAME.phi_state);
    break;
  case DEFARG:
    POP_STATE(def->as.DEFARG.phi_state);
    break;
  default:
    assert(false);
    break;
  }

  return 0;
}

static void phi_insertion(struct module *mod, struct node *branching) {
  struct phi_insertion_state st = {
    .mod = mod,
    .branching = branching,
    .parent = node_parent(branching),
  };

  struct branch_state *br_st = mod->state->branch_state;
  nodeset_foreach(&br_st->need_phi, phi_insertion_foreach, &st);
}

static STEP_FILTER(step_branch_up,
                   STEP_FILTER_BRANCHING);
static error step_branch_up(struct module *mod, struct node *node,
                            void *user, bool *stop) {
  DSTEP(mod, node);

  phi_insertion(mod, node);

  POP_STATE(mod->state->branch_state);
  return 0;
}

static STEP_FILTER(step_push_block_state,
                   SF(BLOCK));
static error step_push_block_state(struct module *mod, struct node *node,
                                           void *user, bool *stop) {
  DSTEP(mod, node);
  PUSH_STATE(mod->state->fun_state->block_state);
  return 0;
}

static STEP_FILTER(step_pop_block_state,
                   SF(BLOCK));
static error step_pop_block_state(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  DSTEP(mod, node);
  POP_STATE(mod->state->fun_state->block_state);
  return 0;
}

static STEP_FILTER(step_record_current_statement,
                   -1);
static error step_record_current_statement(struct module *mod, struct node *node,
                                           void *user, bool *stop) {
  DSTEP(mod, node);
  if (node_parent(node)->which == BLOCK) {
    mod->state->fun_state->block_state->current_statement = node;
  }
  return 0;
}

static bool is_name_of_globalenv(const struct node *node) {
  const struct node *parent = node_parent_const(node);
  if (parent->which == TYPECONSTRAINT
      && node_subs_first_const(parent) == node) {
    const struct node *pparent = node_parent_const(parent);
    return pparent->which == DEFPATTERN && pparent->as.DEFPATTERN.is_globalenv;
  } else {
    return false;
  }
}

static error type_inference_ident(struct module *mod, struct node *node) {
  if (is_name_of_globalenv(node)) {
    set_typ(&node->typ, typ_create_tentative(TBI_ANY));
    return 0;
  }

  if (node_ident(node) == ID_OTHERWISE) {
    set_typ(&node->typ, typ_create_tentative(TBI_ANY));
    return 0;
  }

  struct node *def = NULL;
  error e = scope_lookup(&def, mod, &node->scope, node, true);
  if (e == EINVAL) {
    e = type_inference_ident_unknown(mod, node);
    EXCEPT(e);
    return 0;
  }

  const enum node_which dparent_which = node_parent_const(def)->which;
  if (dparent_which == MODULE || dparent_which == DEFTYPE || dparent_which == DEFINTF) {
    node->as.IDENT.non_local_scope = def->scope.parent;
  } else if (def->flags & NODE_IS_GLOBAL_LET) {
    node->as.IDENT.non_local_scope = def->scope.parent->parent->parent;
  } else {
    track_ident_use(mod, node, def, false);
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

  assert(def->which != DEFNAME || def->typ != NULL);

  if (typ_is_function(def->typ)
      && node->typ != TBI__CALL_FUNCTION_SLOT) {
    if (node_fun_min_args_count(typ_definition(def->typ)) != 0) {
      e = mk_except_call_args_count(mod, node, typ_definition(def->typ), false, 0);
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

static error type_inference_within(struct module *mod, struct node *node) {
  node->typ = NULL;

  error e;
  struct node *def = NULL;
  struct node *first = node_subs_first(node);

  if (node->which == WITHIN) {
    const struct node *modbody = NULL;
    if (first->which == BIN) {
      struct node *ffirst = node_subs_first(first);
      e = type_inference_within(mod, ffirst);
      EXCEPT(e);

      modbody = typ_definition_const(ffirst->typ);

      if (modbody->which != MODULE_BODY) {
        e = mk_except(mod, node, "invalid within expression,"
                      " must point to a globalenv declaration");
        THROW(e);
      }
    } else if (first->which == IDENT) {
      modbody = node_module_owner_const(node)->body;
    } else {
      goto malformed;
    }

    e = scope_lookup_ident_immediate(&def, node, mod,
                                     &modbody->as.MODULE_BODY.globalenv_scope,
                                     node_ident(node_subs_last_const(node)), false);
    EXCEPT(e);
  } else if (node->which == IDENT) {
    e = scope_lookup(&def, mod, &node->scope, node, false);
    EXCEPT(e);
  } else if (node->which == BIN) {
    e = type_inference_within(mod, first);
    EXCEPT(e);

    e = scope_lookup_ident_immediate(&def, node,
                                     mod, &typ_definition(first->typ)->scope,
                                     node_ident(node_subs_last_const(node)), false);
    EXCEPT(e);
  } else {
    goto malformed;
  }

  node->typ = def->typ;
  node->flags |= def->flags;
  return 0;

malformed:
  e = mk_except(mod, node, "malformed within expression");
  THROW(e);
}

STEP_FILTER(step_type_inference,
            -1);
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
  case DEFINCOMPLETE:
    assert(node->typ != NULL);
    // Already typed.
    record_topdep(mod, node->typ);
    return 0;
  case IMPORT:
    if (node->typ != NULL) {
      // Already typed.
      record_topdep(mod, node->typ);
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
    if (node->as.DEFNAME.expr != NULL) {
      set_typ(&node->typ, node->as.DEFNAME.expr->typ);
      node->as.DEFNAME.pattern->flags |= node->as.DEFNAME.expr->flags;
    } else {
      set_typ(&node->typ, typ_create_tentative(TBI_ANY));
    }
    assert(node->typ != NULL);

    if (node->as.DEFNAME.is_excep) {
      struct node *tryy = module_excepts_get(mod)->tryy;
      struct node *err = node_subs_at(node_subs_first(node_subs_first(tryy)), 1);
      assert(err->which == DEFNAME);
      e = unify(mod, node, node->typ, err->typ);
      EXCEPT(e);
    }

    node->flags = node->as.DEFNAME.pattern->flags;

    const struct node *defp = node_parent_const(node);
    const struct node *let = node_parent_const(defp);
    assert(let->which == LET);
    node->flags |= (let->flags & NODE_IS_GLOBAL_LET);
    break;
  case DEFPATTERN:
    FOREACH_SUB(d, node) {
      if (d->which == DEFNAME) {
        if (node->as.DEFPATTERN.is_globalenv) {
          struct typ *t = d->as.DEFNAME.pattern->typ;
          if (!typ_is_reference(t)) {
            e = mk_except_type(mod, node, "globalenv must declare a reference");
            THROW(e);
          }
        }

        e = unify(mod, d, d->typ, d->as.DEFNAME.pattern->typ);
        EXCEPT(e);
      }
    }

    set_typ(&node->typ, TBI_VOID);
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
  case ALIGNOF:
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
    set_typ(&node->typ, typ_create_tentative(TBI_COPYABLE));
    break;
  case CALLNAMEDARG:
    set_typ(&node->typ, node_subs_first(node)->typ);
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
    set_typ(&node->typ, node_subs_last(node)->typ);
    break;
  case THROW:
    {
      struct node *tryy = module_excepts_get(mod)->tryy;
      struct node *err = node_subs_at(node_subs_first(node_subs_first(tryy)), 1);
      assert(err->which == DEFNAME);
      e = unify(mod, node, node_subs_last(node)->typ, err->typ);
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
    struct node *it = node_subs_at(
      node_subs_at(
        node_subs_at(node, IDX_FOR_IT),
        IDX_FOR_IT_DEFP),
      IDX_FOR_IT_DEFP_DEFN);

    struct node *r = NULL;
    e = typ_ref(&r, mod, node, TREFBANG,
                typ_create_tentative(TBI_ITERATOR));
    assert(!e);

    e = unify(mod, it, it->typ, r->typ);
    EXCEPT(e);
    e = typ_check_equal(mod, node_for_block(node),
                        node_for_block(node)->typ,
                        TBI_VOID);
    EXCEPT(e);
    break;
  case WHILE:
    set_typ(&node->typ, TBI_VOID);
    struct node *cond = node_subs_first(node);
    e = unify(mod, cond, cond->typ, typ_create_tentative(TBI_GENERALIZED_BOOLEAN));
    EXCEPT(e);
    struct node *block = node_subs_at(node, 1);
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
    assert(typ_is_reference(node_subs_first(node)->typ));
    set_typ(&node->typ, node->as.DYN.intf_typ);
    break;
  case TYPECONSTRAINT:
    if (node->as.TYPECONSTRAINT.is_constraint) {
      set_typ(&node->typ, node_subs_first(node)->typ);
    } else {
      set_typ(&node->typ, node_subs_first(node)->typ);
      e = unify(mod, node_subs_first(node),
                node_subs_first(node)->typ, node_subs_last(node)->typ);
      EXCEPT(e);
    }
    node->flags |= node_subs_first(node)->flags;
    node->flags |= node_subs_last(node)->flags & NODE__ASSIGN_TRANSITIVE;
    // Copy flags back, as TYPECONSTRAINT are elided in
    // step_remove_typeconstraints().
    node_subs_first(node)->flags |= node->flags;
    break;
  case DEFARG:
    set_typ(&node->typ, node_subs_at(node, 1)->typ);
    if (node->as.DEFARG.is_optional) {
      e = typ_check_isa(mod, node, node->typ, TBI_ANY_NULLABLE_REF);
      EXCEPT(e);
    } else if (node->as.DEFARG.is_vararg) {
      if (!typ_has_same_generic_functor(mod, node->typ, TBI_VARARG)) {
        e = mk_except_type(mod, node,
                           "vararg argument must have type"
                           " (vararg `any_any_ref), not '%s'",
                           typ_pretty_name(mod, node->typ));
        THROW(e);
      }
    }
    break;
  case DEFGENARG:
  case SETGENARG:
    set_typ(&node->typ, node_subs_at(node, 1)->typ);
    node->flags |= NODE_IS_TYPE;
    break;
  case DEFFIELD:
    set_typ(&node->typ, node_subs_at(node, 1)->typ);
    break;
  case LET:
    if (node_has_tail_block(node)) {
      set_typ(&node->typ, node_subs_last(node)->typ);
    } else {
      set_typ(&node->typ, TBI_VOID);
    }
    break;
  case DELEGATE:
    set_typ(&node->typ, TBI_VOID);
    break;
  case EXAMPLE:
  case PRE:
  case POST:
  case INVARIANT:
  case PHI:
    set_typ(&node->typ, TBI_VOID);
    break;
  case WITHIN:
    if (node_subs_count_atleast(node, 1)
        && node_subs_first(node)->which != WITHIN) {
      e = type_inference_within(mod, node);
      EXCEPT(e);
    } else {
      set_typ(&node->typ, TBI_VOID);
    }
    break;
  case ISALIST:
  case GENARGS:
  case FUNARGS:
    node->typ = TBI__NOT_TYPEABLE;
    break;
  case IMPORT:
    node->typ = TBI__NOT_TYPEABLE;
    node->flags = NODE_IS_TYPE;
    break;
  case ISA:
    set_typ(&node->typ, node_subs_first(node)->typ);
    node->flags = node_subs_first(node)->flags & NODE__TRANSITIVE;
    break;
  case DIRECTDEF:
    set_typ(&node->typ, node->as.DIRECTDEF.typ);
    node->flags = node->as.DIRECTDEF.flags;
    break;
  case DEFCHOICE:
    ;const struct node *parent = node;
    do {
      parent = node_parent_const(parent);
    } while (parent->which == DEFCHOICE);
    set_typ(&node->typ, parent->typ);
    break;
  default:
    break;
  }

  if (node->typ != NULL) {
    record_topdep(mod, node->typ);
  }

  assert(node->typ != NULL
         || (node->which == IDENT
             && "tolerate when it's a CATCH label or WITHIN label"));
  return 0;
}

// FIXME: should we be removing these before inferring dyn?
STEP_FILTER(step_remove_typeconstraints,
            SF(TYPECONSTRAINT));
error step_remove_typeconstraints(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  DSTEP(mod, node);

  if (!node->as.TYPECONSTRAINT.in_pattern) {
    struct typ *saved = node->typ;
    struct node *sub = node_subs_first(node);
    node_move_content(node, sub);
    set_typ(&node->typ, saved);

    if (node->which == IDENT) {
      track_ident_use(mod, node, node->as.IDENT.def, false);
    }
  }

  return 0;
}

static STEP_FILTER(step_type_drop_excepts,
                   SF(TRY));
static error step_type_drop_excepts(struct module *mod, struct node *node,
                                    void *user, bool *stop) {
  DSTEP(mod, node);

  module_excepts_close_try(mod);

  return 0;
}

STEP_FILTER(step_gather_final_instantiations,
            SF(DEFTYPE) | SF(DEFINTF) | SF(DEFFUN) | SF(DEFMETHOD));
static error step_gather_final_instantiations(struct module *mod, struct node *node,
                                              void *user, bool *stop) {
  DSTEP(mod, node);

  struct toplevel *toplevel = node_toplevel(mod->state->top_state->top);
  if (toplevel->tentative_instantiations == NULL) {
    return 0;
  }

  if (typ_is_generic_functor(node->typ)) {
    return 0;
  }
  const struct node *parent = node_parent_const(node);
  if (parent->which != MODULE_BODY
      && typ_is_generic_functor(parent->typ)) {
    return 0;
  }

  for (size_t n = 0; n < toplevel->tentative_instantiations_count; ++n) {
    struct typ *t = toplevel->tentative_instantiations[n]->typ;
    if (typ_definition_const(t) == NULL) {
      // 't' was cleared in link_to_final()
      continue;
    }

    if (typ_generic_arity(t) == 0) {
      // For instance, a DEFINCOMPLETE that unified to a non-generic.
      continue;
    }

    if (typ_is_pseudo_builtin(t)) {
      continue;
    }

    if (typ_is_reference(t) && typ_definition_const(t)->which == DEFINTF) {
      continue;
    }

    if (typ_is_tentative(t)) {
      // By now, this instance should not be tentative anymore, as all its
      // generic arguments should have been linked to final types.
      for (size_t m = 0; m < typ_generic_arity(t); ++m) {
        struct typ *arg = typ_generic_arg(t, m);
        if (typ_is_weakly_concrete(arg)) {
          continue;
        }

        if (typ_is_tentative(arg)) {
          fprintf(g_env.stderr, "%s in %s\n",
                  typ_pretty_name(mod, arg), typ_pretty_name(mod, t));
          assert(!typ_is_tentative(arg)
                 && "FIXME: this should be an error, but how to explain it?");
        }
      }
    }

    struct typ *functor = typ_generic_functor(t);
    const size_t arity = typ_generic_arity(t);

    struct typ *existing = find_existing_final_for_tentative(mod, t);
    if (existing != NULL) {
      typ_link_to_existing_final(existing, t);
      record_topdep(mod, existing);
      continue;
    }

    struct typ **args = calloc(arity, sizeof(struct typ *));
    for (size_t m = 0; m < arity; ++m) {
      args[m] = typ_generic_arg(t, m);
    }

    struct node *i = NULL;
    error e = do_instantiate(&i, mod, functor, args, arity, false);
    EXCEPT(e);

    typ_declare_final__privileged(i->typ);
    typ_link_to_existing_final(i->typ, t);
    record_topdep(mod, i->typ);

    free(args);
  }

  if (node->which == DEFINTF) {
    struct node *isal = node_subs_at(node, IDX_ISALIST);
    FOREACH_SUB(isa, isal) {
      assert(!typ_is_tentative(isa->typ));
    }
  }

  free(toplevel->tentative_instantiations);
  toplevel->tentative_instantiations = NULL;
  toplevel->tentative_instantiations_count = 0;

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

static STEP_FILTER(step_check_no_literals_left,
                   SF(NUMBER) | SF(NUL));
static error step_check_no_literals_left(struct module *mod, struct node *node,
                                         void *user, bool *stop) {
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

static STEP_FILTER(step_check_no_incomplete_left,
                   -1);
static error step_check_no_incomplete_left(struct module *mod, struct node *node,
                                           void *user, bool *stop) {
  if (node->typ == NULL) {
    return 0;
  }

  const struct node *d = typ_definition_const(node->typ);
  if (d == NULL || d->which != DEFINCOMPLETE) {
    return 0;
  }

  char msg[2048] = { 0 };
  size_t pos = 0, len = ARRAY_SIZE(msg);
  pos += snprintf(msg+pos, len-pos, "incomplete type was never resolved:\n");
  pos += snprint_defincomplete(msg+pos, len-pos, mod, d);
  THROWF(EINVAL, "%s", msg);
}

static STEP_FILTER(step_ident_non_local_scope,
                   SF(IDENT));
static error step_ident_non_local_scope(struct module *mod, struct node *node,
                                        void *user, bool *stop) {
  struct scope *non_local_scope = node->as.IDENT.non_local_scope;
  const struct node *d = typ_definition_const(node->typ);

  if (non_local_scope != NULL
      && scope_node(non_local_scope)->which == DEFINCOMPLETE
      && d->which == DEFTYPE
      && d->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    struct node *def = NULL;
    error e = scope_lookup_ident_immediate(&def, node, mod, &d->scope,
                                           node_ident(node), false);
    assert(!e);
    node->as.IDENT.non_local_scope = def->scope.parent;
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

static STEP_FILTER(step_weak_literal_conversion,
                   SF(STRING) | SF(BOOL));
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

  struct scope *saved_parent = node->scope.parent;
  struct typ *saved_typ = node->typ;

  struct node *literal = node_new_subnode(mod, node);
  node_subs_remove(node, literal);
  node_move_content(literal, node);

  node_set_which(node, CALL);

  struct node *fun = mk_node(mod, node, DIRECTDEF);
  struct node *fund = node_get_member(mod, typ_definition(saved_typ), id);
  assert(fund != NULL);
  set_typ(&fun->as.DIRECTDEF.typ, fund->typ);
  fun->as.DIRECTDEF.flags = NODE_IS_TYPE;

  node_subs_append(node, literal);
  set_typ(&literal->typ, lit_typ);

  const struct node *except[] = { literal, NULL };
  error e = catchup(mod, except, node, saved_parent,
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
    assert(false);
    return 0;
  }
}

static error gen_operator_call(struct module *mod,
                               struct scope *saved_parent, struct node *node,
                               ident operator_name, struct node *left, struct node *right,
                               enum catchup_for catchup_for) {
  struct typ *tfun = node_get_member(mod, typ_definition(left->typ),
                                     operator_name)->typ;

  node_set_which(node, CALL);
  struct node *fun = mk_node(mod, node, DIRECTDEF);
  set_typ(&fun->as.DIRECTDEF.typ, tfun);
  fun->as.DIRECTDEF.flags = NODE_IS_TYPE;

  const struct node *except[3] = { NULL, NULL, NULL };
  except[0] = left;
  expr_ref(mod, node, operator_call_arg_refop(mod, tfun, 0), left);

  if (right != NULL) {
    except[1] = right;
    expr_ref(mod, node, operator_call_arg_refop(mod, tfun, 1), right);
  }

  error e = catchup(mod, except, node, saved_parent, catchup_for);
  EXCEPT(e);

  return 0;
}

static STEP_FILTER(step_operator_call_inference,
                   SF(UN) | SF(BIN));
static error step_operator_call_inference(struct module *mod, struct node *node,
                                          void *user, bool *stop) {
  DSTEP(mod, node);

  enum token_type op;
  struct node *left = NULL;
  struct node *right = NULL;

  switch (node->which) {
  case UN:
    op = node->as.UN.operator;
    left = node_subs_first(node);
    break;
  case BIN:
    op = node->as.BIN.operator;
    left = node_subs_first(node);
    right = node_subs_last(node);
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
  if (typ_isa(left->typ, TBI_NATIVE_INTEGER)
      || typ_isa(left->typ, TBI_NATIVE_BOOLEAN)
      || typ_isa(left->typ, TBI_NATIVE_FLOATING)
      || (l0 != NULL && typ_isa(l0, TBI_ENUM))) {
    return 0;
  }

  struct node *dleft = typ_definition(left->typ);
  if (dleft->which == DEFTYPE
      && dleft->as.DEFTYPE.kind == DEFTYPE_ENUM
      && typ_isa(dleft->as.DEFTYPE.tag_typ, TBI_NATIVE_INTEGER)) {
    return 0;
  }

  if (operator_ident[op] == 0) {
    return 0;
  }

  node_subs_remove(node, left);
  if (right != NULL) {
    node_subs_remove(node, right);
  }
  error e = gen_operator_call(mod, node->scope.parent, node,
                              operator_ident[op], left, right,
                              CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static error gen_operator_test_call(struct module *mod, struct node *node, size_t n) {
  struct node *expr = node_subs_at(node, n);
  if (typ_equal(expr->typ, TBI_BOOL)) {
    return 0;
  }

  struct node *test = node_new_subnode(mod, node);
  node_subs_remove(node, test);
  node_subs_replace(node, expr, test);
  error e = gen_operator_call(mod, node->scope.parent, test,
                              ID_OPERATOR_TEST, expr, NULL,
                              CATCHUP_BELOW_CURRENT);
  EXCEPT(e);
  return 0;
}

static STEP_FILTER(step_operator_test_call_inference,
                   SF(DEFNAME));
static error step_operator_test_call_inference(struct module *mod, struct node *node,
                                               void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->as.DEFNAME.is_excep) {
    error e = gen_operator_test_call(mod, node, IDX_DEFNAME_EXCEP_TEST);
    EXCEPT(e);
  }

  return 0;
}

static STEP_FILTER(step_ctor_call_inference,
                   SF(RETURN) | SF(BIN) | SF(DEFNAME) | SF(TYPECONSTRAINT) |
                   SF(CALL) | SF(INIT));
static error step_ctor_call_inference(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);
  return 0;
}

static STEP_FILTER(step_array_ctor_call_inference,
                   SF(INIT));
static error step_array_ctor_call_inference(struct module *mod, struct node *node,
                                            void *user, bool *stop) {
  DSTEP(mod, node);

  if (!node->as.INIT.is_array) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_TRIVIAL_ARRAY_CTOR)) {
    return 0;
  }

  struct scope *saved_parent = node->scope.parent;
  struct typ *saved_typ = node->typ;

  struct node *array = node_new_subnode(mod, node);
  node_subs_remove(node, array);
  node_move_content(array, node);

  node_set_which(node, CALL);

  struct node *fun = mk_node(mod, node, DIRECTDEF);
  set_typ(&fun->as.DIRECTDEF.typ,
          node_get_member(mod, typ_definition(saved_typ), ID_MKV)->typ);
  fun->as.DIRECTDEF.flags = NODE_IS_TYPE;

  struct node *ref_array = mk_node(mod, node, UN);
  ref_array->as.UN.operator = TREFDOT;

  node_subs_append(ref_array, array);
  set_typ(&array->typ,
          typ_generic_arg(typ_function_arg(fun->as.DIRECTDEF.typ, 0), 0));

  const struct node *except[] = { array, NULL };
  error e = catchup(mod, except, node, saved_parent,
                    CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static STEP_FILTER(step_dtor_call_inference,
                   SF(BLOCK));
static error step_dtor_call_inference(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);
  return 0;
}

static bool expr_is_literal_initializer(struct node **expr, struct module *mod, struct node *node) {
  if (node->which == INIT) {
    if (expr != NULL) {
      *expr = node;
    }
    return true;
  } else if (node->which == BLOCK) {
    return expr_is_literal_initializer(expr, mod, node_subs_last(node));
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
    return expr_is_return_through_ref(expr, mod, node_subs_last(node));
  } else {
    return false;
  }
}

static error assign_copy_call_inference(struct module *mod, struct node *node) {
  struct node *left = node_subs_first(node);
  struct node *right = node_subs_at(node, 1);

  node_subs_remove(node, left);
  node_subs_remove(node, right);

  error e = gen_operator_call(mod, node->scope.parent, node,
                              ID_COPY_CTOR, left, right,
                              CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);
  return 0;
}

static error defname_copy_call_inference(struct module *mod, struct node *node) {
  struct node *let = node_parent(node_parent(node));
  assert(let->which == LET);

  struct node *within;
  if (node_has_tail_block(let)) {
    within = node_subs_last(let);
  } else {
    within = mk_node(mod, let, BLOCK);
    struct node *to_remove = mk_node(mod, within, NOOP);
    error e = catchup(mod, NULL, within, &let->scope, CATCHUP_BEFORE_CURRENT);
    EXCEPT(e);
    node_subs_remove(within, to_remove);
  }

  struct node *left = mk_node(mod, within, IDENT);
  left->as.IDENT.name = node_ident(node->as.DEFNAME.pattern);
  set_typ(&left->typ, node->as.DEFNAME.pattern->typ);
  node_subs_remove(within, left);

  struct node *right = node->as.DEFNAME.expr;
  node->as.DEFNAME.expr = NULL; // Steal right.
  node_subs_remove(node_parent(right), right);

  struct scope *saved_parent = within->scope.parent;
  struct node *copycall = node_new_subnode(mod, within);
  error e = gen_operator_call(mod, saved_parent, copycall,
                              ID_COPY_CTOR, left, right,
                              CATCHUP_AFTER_CURRENT);
  EXCEPT(e);

  assert(typ_equal(copycall->typ, TBI_VOID)
         && "We used a PASS above to catchup() 'within', this only works "
         "if the return type is void.");

  return 0;
}

static STEP_FILTER(step_copy_call_inference,
                   SF(BIN) | SF(DEFNAME));
static error step_copy_call_inference(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);
  struct node *left;
  struct node *right;
  switch (node->which) {
  case BIN:
    if (node->as.BIN.operator == TASSIGN) {
      left = node_subs_first(node);
      right = node_subs_last(node);
      break;
    }
    return 0;
  case DEFNAME:
    if (node_parent(node)->as.DEFPATTERN.is_ssa_var) {
      return 0;
    }
    if (!(node->flags & NODE_IS_TYPE)) {
      left = node->as.DEFNAME.pattern;
      right = node->as.DEFNAME.expr;
      if (right != NULL) {
        break;
      }
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
    assert(false);
  }
  EXCEPT(e);
  return 0;
}

static error check_has_matching_member(struct module *mod,
                                       struct node *deft,
                                       const struct typ *intf,
                                       const struct node *mi) {
  error e;
  struct node *m = node_get_member(mod, deft, node_ident(mi));
  if (m == NULL) {
    e = mk_except_type(mod, deft,
                       "type '%s' isa '%s' but does not implement member '%s'",
                       typ_pretty_name(mod, deft->typ),
                       typ_pretty_name(mod, intf),
                       idents_value(mod->gctx, node_ident(mi)));
    THROW(e);
  }

  if (m->which != mi->which) {
    e = mk_except_type(mod, deft,
                       "in type '%s', member '%s' implemented from intf '%s'"
                       " is not the right kind of declaration",
                       typ_pretty_name(mod, deft->typ),
                       idents_value(mod->gctx, node_ident(m)),
                       typ_pretty_name(mod, intf));
  }

  if (m->which == DEFNAME
      || m->which == DEFFIELD) {
    // FIXME: if the type of mi is (lexically) 'final', we need to check
    // that it is *equal* to deft->typ.

    if (!typ_isa(m->typ, mi->typ)) {
      e = mk_except_type(mod, deft,
                         "in type '%s', member '%s' implemented from intf '%s'"
                         " has type '%s' but must be isa '%s'",
                         typ_pretty_name(mod, deft->typ),
                         idents_value(mod->gctx, node_ident(m)),
                         typ_pretty_name(mod, intf),
                         typ_pretty_name(mod, m->typ),
                         typ_pretty_name(mod, mi->typ));
      THROW(e);
    }
  } else if (mi->which == DEFFUN || mi->which == DEFMETHOD) {
    // FIXME check that the prototype is an exact match.
    // FIXME: handle (lexically) 'final' in mi properly.
  } else {
    assert(false && "Unreached");
  }

  switch (m->which) {
  case DEFNAME:
    m->as.DEFNAME.member_isa = mi;
    break;
  case DEFFIELD:
    m->as.DEFFIELD.member_isa = mi;
    break;
  case DEFFUN:
    m->as.DEFFUN.member_isa = mi;
    break;
  case DEFMETHOD:
    m->as.DEFMETHOD.member_isa = mi;
    break;
  default:
    assert(false && "Unreached");
    break;
  }

  return 0;
}

static error check_exhaustive_intf_impl_eachisalist(struct module *mod,
                                                    struct typ *t,
                                                    struct typ *intf,
                                                    bool *stop,
                                                    void *user) {
  (void) user;
  struct node *deft = typ_definition(t);
  const struct node *dintf = typ_definition_const(intf);
  error e = 0;

  if (typ_isa(t, TBI_ANY_TUPLE) && typ_equal(intf, TBI_COPYABLE)) {
    // FIXME: once we generate copy_ctor in the non-trivial_copy case,
    // remove this.
    return 0;
  }

  FOREACH_SUB_EVERY_CONST(mi, dintf, IDX_ISALIST + 1, 1) {
    if (mi->which == LET) {
      FOREACH_SUB_CONST(defp, mi) {
        FOREACH_SUB_CONST(d, defp) {
          if (d->which != DEFNAME) {
            continue;
          }

          ident id = node_ident(d);
          if (id == ID_FINAL || id == ID_THIS) {
            continue;
          }

          e = check_has_matching_member(mod, deft, intf, d);
          EXCEPT(e);
        }
      }
    } else {
      e = check_has_matching_member(mod, deft, intf, mi);
      EXCEPT(e);
    }
  }

  return 0;
}

static STEP_FILTER(step_check_exhaustive_intf_impl,
                   SF(DEFTYPE));
static error step_check_exhaustive_intf_impl(struct module *mod, struct node *node,
                                             void *user, bool *stop) {
  DSTEP(mod, node);

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
  error e = catchup(mod, except, d, &node->scope, CATCHUP_BELOW_CURRENT);
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

static STEP_FILTER(step_dyn_inference,
                   SF(RETURN) | SF(BIN) | SF(DEFNAME) | SF(TYPECONSTRAINT) |
                   SF(CALL) | SF(INIT));
static error step_dyn_inference(struct module *mod, struct node *node,
                                void *user, bool *stop) {
  DSTEP(mod, node);
  const struct node *target;
  struct node *src;

  error e;
  switch (node->which) {
  case RETURN:
    if (!node_subs_count_atleast(node, 1)) {
      return 0;
    }
    target = module_retval_get(mod);
    src = node_subs_first(node);

    e = try_insert_dyn(&src, mod, node, target->typ);
    EXCEPT(e);
    return 0;
  case BIN:
    if (node->as.BIN.operator == TASSIGN) {
      target = node_subs_first(node);
      src = node_subs_at(node, 1);
      e = try_insert_dyn(&src, mod, node, target->typ);
      EXCEPT(e);
    }
    return 0;
  case DEFNAME:
    if (!(node->flags & NODE_IS_TYPE)) {
      target = node->as.DEFNAME.pattern;
      src = node->as.DEFNAME.expr;
      if (src != NULL) {
        e = try_insert_dyn(&src, mod, node, target->typ);
        EXCEPT(e);
      }
    }
    return 0;
  case TYPECONSTRAINT:
    target = node_subs_first(node);
    src = node_subs_last(node);
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

    const struct node *funargs = node_subs_at_const(
      typ_definition_const(node_subs_first_const(node)->typ), IDX_FUNARGS);
    const struct node *target = node_subs_first_const(funargs);

    FOREACH_SUB_EVERY(src, node, 1, 1) {
      e = try_insert_dyn(&src, mod, node, GET_TYP(target));
      EXCEPT(e);

      if (!target->as.DEFARG.is_vararg) {
        target = node_next_const(target);
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

  struct node *last = node_subs_last(block);

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
  error e = catchup(mod, except, assign, &block->scope, CATCHUP_BELOW_CURRENT);
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
    if (last_elif != node_subs_last(node)) {
      struct node *els = node_subs_last(node);
      block_insert_value_assign(mod, els, target, target_name);
    }
    break;
  case TRY:
    block_insert_value_assign(mod, node_subs_first(node), target, target_name);
    block_insert_value_assign(mod, node_subs_at(node, 2), target, target_name);
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

static STEP_FILTER(step_move_assign_in_block_like,
                   SF(BIN));
static error step_move_assign_in_block_like(struct module *mod, struct node *node,
                                            void *user, bool *stop) {
  if (!OP_IS_ASSIGN(node->as.BIN.operator)) {
    return 0;
  }

  struct node *left = node_subs_first(node);
  struct node *right = node_subs_last(node);
  if (!is_block_like(right)) {
    return 0;
  }

  node_subs_remove(node, left);
  block_like_insert_value_assign(mod, right, left, 0);

  node_subs_remove(node, right);
  node_move_content(node, right);

  set_typ(&node->typ, TBI_VOID);

  return 0;
}

static const struct node *retval_name(struct module *mod) {
  const struct node *retval = module_retval_get(mod);
  assert(node_subs_count_atleast(retval, 1));
  return node_subs_first_const(retval);
}

static STEP_FILTER(step_store_return_through_ref_expr,
                   SF(RETURN) | SF(DEFPATTERN) | SF(BIN));
static error step_store_return_through_ref_expr(struct module *mod, struct node *node,
                                                void *user, bool *stop) {
  DSTEP(mod, node);

  // FIXME: this is crap, mostly rendered useless by the introduction of the
  // SSA step.

  struct node *expr = NULL;
  struct node *real_expr = NULL;

  switch (node->which) {
  case RETURN:
    if (!node_subs_count_atleast(node, 1)
        || typ_equal(node_subs_first(node)->typ, TBI_VOID)) {
      return 0;
    }

    expr = node_subs_first(node);
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
  case DEFPATTERN:
    REVERSE_FOREACH_SUB(d, node) {
      if (d->which != DEFNAME) {
        continue;
      }
      expr = d->as.DEFNAME.expr;
      if (expr == NULL) {
        // noop
      } else if (expr_is_literal_initializer(&real_expr, mod, expr)) {
        real_expr->as.INIT.target_expr = d->as.DEFNAME.pattern;
      } else if (expr_is_return_through_ref(&real_expr, mod, expr)) {
        real_expr->as.CALL.return_through_ref_expr = d->as.DEFNAME.pattern;
      }
    }
    return 0;
  case BIN:
    if (!OP_IS_ASSIGN(node->as.BIN.operator)) {
      return 0;
    }
    struct node *left = node_subs_first(node);
    struct node *right = node_subs_last(node);
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

static bool block_like_needs_temporary(struct module *mod,
                                       struct node *node) {
  assert(is_block_like(node));
  struct node *parent = node_parent(node);

  if (is_block_like(parent)) {
    return false;
  }

  if (parent->which == RETURN
      && !typ_isa(node->typ, TBI_RETURN_BY_COPY)) {
    return false;
  } else if (parent->which == DEFPATTERN) {
    return false;
  } else if (parent->which == BIN
             && OP_IS_ASSIGN(parent->as.BIN.operator)) {
    return false;
  } else {
    return true;
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

static STEP_FILTER(step_gather_temporary_rvalues,
                   SF(UN) | SF(INIT) | SF(CALL) | SF(TUPLEEXTRACT) |
                   SF(IF) | SF(TRY) | SF(MATCH) | SF(BLOCK));
static error step_gather_temporary_rvalues(struct module *mod, struct node *node,
                                           void *user, bool *stop) {
  DSTEP(mod, node);
  struct temporaries *temps = user;

  struct node *parent = node_parent(node);

  switch (node->which) {
  case UN:
    if (OP_KIND(node->as.UN.operator) == OP_UN_REFOF
        && node_is_rvalue(node_subs_first(node))) {
      temporaries_add(temps, node);
    }
    break;
  case INIT:
    if (parent->which == RETURN
        && !typ_isa(node->typ, TBI_RETURN_BY_COPY)) {
      break;
    }
    if (parent->which == DEFPATTERN) {
      break;
    }
    if (parent->which == BIN
        && OP_IS_ASSIGN(parent->as.BIN.operator)) {
      break;
    }
    if (parent->which == UN
        && OP_KIND(parent->as.UN.operator) == OP_UN_REFOF) {
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
    if (parent->which == RETURN) {
      break;
    }
    if (parent->which == BIN
        && OP_IS_ASSIGN(parent->as.BIN.operator)) {
      break;
    }
    if (parent->which == DEFPATTERN) {
      break;
    }
    temporaries_add(temps, node);
    break;
  case TUPLEEXTRACT:
    if (node_subs_last(node)->which != IDENT) {
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
    *stop = true;
    break;
  default:
    assert(false && "Unreached");
    return 0;
  }

  return 0;
}

static error finish_passbody1(struct module *mod, struct node *root,
                              void *user, ssize_t shallow_last_up);

static error declare_temporaries(struct module *mod, struct node *statement,
                                 struct temporaries *temps) {
  temps->gensyms = calloc(temps->count, sizeof(*temps->gensyms));

  struct node *let = NULL;
  if (statement->which == LET) {
    let = statement;
  } else {
    struct scope *saved_parent = statement->scope.parent;
    struct typ *saved = statement->typ;
    struct node *block = mk_node(mod, statement, BLOCK);
    struct node *new_statement = node_new_subnode(mod, block);
    node_subs_remove(statement, block);
    node_move_content(new_statement, statement);
    set_typ(&new_statement->typ, saved);
    node_set_which(statement, LET);
    node_subs_append(statement, block);

    const struct node *except[] = { new_statement, NULL };
    error e = catchup(mod, except, statement, saved_parent,
                      CATCHUP_REWRITING_CURRENT);
    assert(!e);

    // We moved the current stepping node down in the tree, and it was in
    // the catchup() except list and not processed by later steps in the
    // current pass. But these steps may be crucial as 'statement' could be
    // anything at all. So we must force these steps by hand.
    PUSH_STATE(mod->state->step_state);
    e = finish_passbody1(mod, statement, NULL, -1);
    EXCEPT(e);
    POP_STATE(mod->state->step_state);

    let = statement;
  }

  statement = NULL;

  struct node *prev = NULL;
  for (size_t n = 0; n < temps->count; ++n) {
    const struct node *rv = temps->rvalues[n];
    struct node *defp = mk_node(mod, let, DEFPATTERN);
    node_subs_remove(let, defp);
    if (prev != NULL) {
      node_subs_insert_after(let, prev, defp);
    } else {
      if (node_subs_count_atleast(let, 1)) {
        node_subs_insert_before(let, node_subs_first(let), defp);
      } else {
        node_subs_append(let, defp);
      }
    }

    struct node *typc = mk_node(mod, defp, TYPECONSTRAINT);

    struct node *tmp_name = mk_node(mod, typc, IDENT);
    const ident g = gensym(mod);
    tmp_name->as.IDENT.name = g;
    temps->gensyms[n] = g;

    struct node *typ = mk_node(mod, typc, DIRECTDEF);
    if (rv->which == UN && OP_KIND(rv->as.UN.operator) == OP_UN_REFOF) {
      set_typ(&typ->as.DIRECTDEF.typ, typ_generic_arg(rv->typ, 0));
    } else {
      set_typ(&typ->as.DIRECTDEF.typ, rv->typ);
    }
    typ->as.DIRECTDEF.flags = NODE_IS_TYPE;

    error e = catchup(mod, NULL, defp, &let->scope, CATCHUP_BELOW_CURRENT);
    assert(!e);

    prev = defp;
  }

  return 0;
}


static error pass_gather_temps(struct module *mod, struct node *root,
                               void *user, ssize_t shallow_last_up) {
  PASS(, UP_STEP(step_gather_temporary_rvalues));
  return 0;
}

static STEP_FILTER(step_define_temporary_rvalues,
                   -1);
static error step_define_temporary_rvalues(struct module *mod, struct node *node,
                                           void *user, bool *stop) {
  return 0;
  //FIXME
  //
  //
  //
  //
  DSTEP(mod, node);
  if (!node_is_statement(node)) {
    return 0;
  }

  struct temporaries temps = { 0 };

  PUSH_STATE(mod->state->step_state);
  error e = pass_gather_temps(mod, node, &temps, -1);
  POP_STATE(mod->state->step_state);
  EXCEPT(e);

  if (temps.count == 0) {
    return 0;
  }

  e = declare_temporaries(mod, node, &temps);
  EXCEPT(e);

  for (size_t n = 0; n < temps.count; ++n) {
    const ident g = temps.gensyms[n];
    struct node *rv = temps.rvalues[n];
    struct node *rv_parent = node_parent(rv);
    struct node saved = { 0 };
    node_move_content(&saved, rv);

    struct node *block = rv;
    rv = NULL;

    node_set_which(block, BLOCK);
    struct node *assign = mk_node(mod, block, BIN);
    assign->as.BIN.operator = TASSIGN;
    struct node *target = mk_node(mod, assign, IDENT);
    target->as.IDENT.name = g;

    const struct node *except[2];
    except[1] = NULL;

    if (saved.which == UN && OP_KIND(saved.as.UN.operator) == OP_UN_REFOF) {
      struct node *rv1 = node_subs_first(&saved);
      node_subs_remove(&saved, rv1);
      node_subs_append(assign, rv1);
      rv1->flags |= NODE_IS_TEMPORARY;
      except[0] = rv1;

      struct node *nvalue = mk_node(mod, block, UN);
      nvalue->as.UN.operator = saved.as.UN.operator;
      struct node *nvalue_name = mk_node(mod, nvalue, IDENT);
      nvalue_name->as.IDENT.name = g;

    } else if (saved.which == TUPLEEXTRACT) {
      struct node *tuple = node_subs_last(&saved);
      node_subs_remove(&saved, tuple);
      node_subs_append(assign, tuple);
      except[0] = tuple;

      struct node *extractor = mk_node(mod, block, TUPLEEXTRACT);
      struct node *nth = node_subs_first(&saved);
      struct node *next_nth = NULL;
      for (size_t n = 0; n < typ_generic_arity(tuple->typ); ++n) {
        // We want to reuse the original TUPLENTH from 'rv' as they may be
        // pointed to by nearby DEFNAME.expr, so their location in memory
        // cannot change.
        next_nth = node_next(nth);

        node_subs_remove(&saved, nth);
        node_subs_append(extractor, nth);
        unset_typ(&nth->typ);

        nth = next_nth;
      }
      struct node *nvalue = mk_node(mod, extractor, IDENT);
      nvalue->as.IDENT.name = g;

    } else {
      struct node *moved_rv = node_new_subnode(mod, assign);
      node_move_content(moved_rv, &saved);
      moved_rv->flags |= NODE_IS_TEMPORARY;
      except[0] = moved_rv;

      struct node *nvalue = mk_node(mod, block, IDENT);
      nvalue->as.IDENT.name = g;
    }

    e = catchup(mod, except, block, &rv_parent->scope, CATCHUP_BELOW_CURRENT);
    EXCEPT(e);
  }

  free(temps.rvalues);
  free(temps.gensyms);

  return 0;
}

error passbody0(struct module *mod, struct node *root,
                void *user, ssize_t shallow_last_up) {
  // first
  PASS(
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_stop_already_morningtypepass);
    DOWN_STEP(step_push_top_state);
    DOWN_STEP(step_push_fun_state);
    DOWN_STEP(step_set_topdep_mask);
    DOWN_STEP(step_detect_not_dyn_intf_down);
    DOWN_STEP(step_rewrite_wildcards);
    DOWN_STEP(step_type_destruct_mark);
    DOWN_STEP(step_type_mutability_mark);
    DOWN_STEP(step_type_gather_retval);
    DOWN_STEP(step_type_gather_excepts);
    DOWN_STEP(step_branch_down);
    DOWN_STEP(step_branch_block_down);
    DOWN_STEP(step_push_block_state);
    DOWN_STEP(step_record_current_statement);
    ,
    UP_STEP(step_excepts_store_label);
    UP_STEP(step_rewrite_defname_no_expr);
    UP_STEP(step_rewrite_union_constructors);
    UP_STEP(step_detect_not_dyn_intf_up);
    UP_STEP(step_ssa_convert);
    UP_STEP(step_type_inference);
    UP_STEP(step_constraint_inference);
    UP_STEP(step_remove_typeconstraints);
    UP_STEP(step_type_drop_excepts);
    UP_STEP(step_check_exhaustive_match);
    UP_STEP(step_branch_block_up);
    UP_STEP(step_branch_up);
    UP_STEP(step_pop_block_state);
    UP_STEP(step_gather_final_instantiations);
    UP_STEP(step_pop_fun_state);
    UP_STEP(step_pop_top_state);
    UP_STEP(step_complete_instantiation);
    );
    return 0;
}

static error finish_passbody1(struct module *mod, struct node *root,
                              void *user, ssize_t shallow_last_up) {
  PASS(
    ,
    // Must start after 'step_define_temporary_rvalues'
    UP_STEP(step_move_assign_in_block_like);
    UP_STEP(step_store_return_through_ref_expr);

    UP_STEP(step_pop_fun_state);
    UP_STEP(step_complete_instantiation);
    );
  return 0;
}

static error passbody1(struct module *mod, struct node *root,
                       void *user, ssize_t shallow_last_up) {
  // second
  PASS(
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_push_top_state);
    DOWN_STEP(step_push_fun_state);
    DOWN_STEP(step_type_gather_retval);
    DOWN_STEP(step_check_no_literals_left);
    DOWN_STEP(step_check_no_incomplete_left);
    DOWN_STEP(step_ident_non_local_scope);
    DOWN_STEP(step_branch_down);
    DOWN_STEP(step_branch_block_down);
    DOWN_STEP(step_push_block_state);
    DOWN_STEP(step_record_current_statement);
    ,
    UP_STEP(step_weak_literal_conversion);
    UP_STEP(step_operator_call_inference);
    UP_STEP(step_operator_test_call_inference);
    UP_STEP(step_ctor_call_inference);
    UP_STEP(step_array_ctor_call_inference);
    UP_STEP(step_dtor_call_inference);
    UP_STEP(step_copy_call_inference);
    UP_STEP(step_check_exhaustive_intf_impl);
    UP_STEP(step_dyn_inference);

    // Must be kept in sync with finish_passbody1().
    UP_STEP(step_define_temporary_rvalues);
    UP_STEP(step_move_assign_in_block_like);
    UP_STEP(step_store_return_through_ref_expr);

    UP_STEP(step_branch_block_up);
    UP_STEP(step_branch_up);
    UP_STEP(step_pop_block_state);
    UP_STEP(step_pop_fun_state);
    UP_STEP(step_pop_top_state);
    UP_STEP(step_complete_instantiation);
    );
    return 0;
}

a_pass passbody[] = { passbody0, passbody1 };
