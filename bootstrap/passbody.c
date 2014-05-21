#include "passbody.h"

#include "table.h"
#include "ssa.h"
#include "types.h"
#include "scope.h"
#include "unify.h"
#include "constraints.h"

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

STEP_NM(step_push_fun_state,
        NM(DEFFUN) | NM(DEFMETHOD) | NM(EXAMPLE));
error step_push_fun_state(struct module *mod, struct node *node,
                          void *user, bool *stop) {
  DSTEP(mod, node);

  PUSH_STATE(mod->state->fun_state);

  return 0;
}

STEP_NM(step_pop_fun_state,
        NM(DEFFUN) | NM(DEFMETHOD) | NM(EXAMPLE));
error step_pop_fun_state(struct module *mod, struct node *node,
                         void *user, bool *stop) {
  DSTEP(mod, node);

  POP_STATE(mod->state->fun_state);

  return 0;

}

STEP_NM(step_push_block_state,
        NM(BLOCK));
error step_push_block_state(struct module *mod, struct node *node,
                            void *user, bool *stop) {
  DSTEP(mod, node);
  PUSH_STATE(mod->state->fun_state->block_state);

  if (mod->state->fun_state->block_state->prev != NULL) {
    mod->state->fun_state->block_state->current_statement
      = mod->state->fun_state->block_state->prev->current_statement;
  }

  return 0;
}

STEP_NM(step_pop_block_state,
        NM(BLOCK));
error step_pop_block_state(struct module *mod, struct node *node,
                           void *user, bool *stop) {
  DSTEP(mod, node);
  POP_STATE(mod->state->fun_state->block_state);
  return 0;
}

STEP_NM(step_record_current_statement,
        -1);
error step_record_current_statement(struct module *mod, struct node *node,
                                    void *user, bool *stop) {
  DSTEP(mod, node);
  if (parent(node)->which == BLOCK) {
    mod->state->fun_state->block_state->current_statement = node;
  }
  return 0;
}

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

// Scopes are defined over an entire BLOCK, but a name becomes defined only
// in the statements following its DEFNAME or DEFALIAS within the block.
static bool local_name_is_scoped(struct module *mod, const struct node *node) {
  switch (node->which) {
  case DEFARG:
    return true;
  case DEFNAME:
    return node->as.DEFNAME.passed >= mod->stage->state->passing;
  case DEFALIAS:
    return node->as.DEFALIAS.passed >= mod->stage->state->passing;
  default:
    assert(false);
    return false;
  }
}

STEP_NM(step_set_topdep_mask,
        NM(BLOCK));
static error step_set_topdep_mask(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  DSTEP(mod, node);

  struct top_state *st = mod->state->top_state;
  switch (parent(node)->which) {
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

static STEP_NM(step_detect_not_dyn_intf_down,
               NM(DEFFUN) | NM(DEFMETHOD));
static error step_detect_not_dyn_intf_down(struct module *mod, struct node *node,
                                           void *user, bool *stop) {
  DSTEP(mod, node);

  mod->state->fun_state->fun_uses_final = false;

  return 0;
}

static STEP_NM(step_detect_not_dyn_intf_up,
               NM(DEFFUN) | NM(DEFMETHOD) | NM(IDENT) | NM(DEFARG));
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
        || subs_count_atleast(subs_at_const(node, IDX_GENARGS), 1)) {
      node_toplevel(node)->flags |= TOP_IS_NOT_DYN;
    }
    break;
  case IDENT:
    if (node_ident(node) == ID_FINAL) {
      mod->state->fun_state->fun_uses_final = true;
    }
    break;
  case DEFARG:
    if (subs_first(parent(node)) == node
        && parent(node)->which == DEFMETHOD) {
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

static STEP_NM(step_rewrite_wildcards,
               NM(DEFMETHOD) | NM(UN) | NM(BIN));
static error step_rewrite_wildcards(struct module *mod, struct node *node,
                                    void *user, bool *stop) {
  DSTEP(mod, node);

#define SET_UNLESS_ZERO(dst, src) if (src != 0) { dst = src; }

  bool is_self = false;

#define GET_WILDCARD(kind) \
    ( (is_self) ? mod->state->fun_state->self_wildcard.kind \
      : mod->state->fun_state->wildcard.kind )

  // FIXME: handle DEFFUN like in t02/fixme01.n
  switch (node->which) {
  case DEFMETHOD:
    if (!subs_count_atleast(subs_at_const(node, IDX_GENARGS), 1)) {
      break;
    }
    struct node *def = NULL;
    error e = scope_lookup_ident_immediate(&def, node, mod, &node->scope,
                                           ID_WILDCARD_REF_ARG, true);
    if (e) {
      break;
    }

    const bool is_shallow = node_toplevel_const(node)->flags & TOP_IS_SHALLOW;
    if (typ_equal(def->typ, TBI_ANY_REF)) {
      // noop
    } else if (typ_equal(def->typ, TBI_REF)) {
      mod->state->fun_state->wildcard.ref = TREFDOT;
      mod->state->fun_state->wildcard.nulref = TNULREFDOT;
      mod->state->fun_state->wildcard.deref = TDEREFDOT;
      mod->state->fun_state->wildcard.acc = TDOT;

      mod->state->fun_state->self_wildcard = mod->state->fun_state->wildcard;
    } else if (typ_equal(def->typ, TBI_MREF)) {
      mod->state->fun_state->wildcard.ref = is_shallow ? TREFSHARP : TREFBANG;
      mod->state->fun_state->wildcard.nulref = is_shallow ? TNULREFSHARP : TNULREFBANG;
      mod->state->fun_state->wildcard.deref = is_shallow ? TDEREFSHARP : TDEREFBANG;
      mod->state->fun_state->wildcard.acc = is_shallow ? TSHARP : TBANG;

      mod->state->fun_state->self_wildcard.ref = TREFBANG;
      mod->state->fun_state->self_wildcard.nulref = TNULREFBANG;
      mod->state->fun_state->self_wildcard.deref = TDEREFBANG;
      mod->state->fun_state->self_wildcard.acc = TBANG;
    } else if (typ_equal(def->typ, TBI_MMREF)) {
      mod->state->fun_state->wildcard.ref = TREFSHARP;
      mod->state->fun_state->wildcard.nulref = TNULREFSHARP;
      mod->state->fun_state->wildcard.deref = TDEREFSHARP;
      mod->state->fun_state->wildcard.acc = TSHARP;

      mod->state->fun_state->self_wildcard = mod->state->fun_state->wildcard;
    } else {
      assert(false);
    }
    break;

  case UN:
    is_self = node_ident(subs_first_const(node)) == ID_SELF;

    switch (node->as.UN.operator) {
    case TREFWILDCARD:
      SET_UNLESS_ZERO(node->as.UN.operator, GET_WILDCARD(ref));
      break;
    case TNULREFWILDCARD:
      SET_UNLESS_ZERO(node->as.UN.operator, GET_WILDCARD(nulref));
      break;
    case TDEREFWILDCARD:
      SET_UNLESS_ZERO(node->as.UN.operator, GET_WILDCARD(deref));
      break;
    default:
      break;
    }
    break;
  case BIN:
    is_self = node_ident(subs_first_const(node)) == ID_SELF;

    switch (node->as.BIN.operator) {
    case TWILDCARD:
      SET_UNLESS_ZERO(node->as.BIN.operator, GET_WILDCARD(acc));
      break;
    default:
      break;
    }
    break;
  default:
    assert(false && "Unreached");
    break;
  }

#undef GET_WILDCARD
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

      n = next(n);
    }
  }
}

static void inherit(struct module *mod, struct node *node) {
  if (node->typ == TBI__NOT_TYPEABLE) {
    mark_subs(mod, node, node->typ,
              subs_first(node), subs_last(node), 1);
  }
}

STEP_NM(step_type_destruct_mark,
        NM(BIN) | NM(CALL) | NM(INIT) | NM(DEFALIAS) | NM(DEFNAME) |
        NM(DEFFUN) | NM(DEFMETHOD) | NM(DEFTYPE) | NM(DEFINTF) | NM(DEFINCOMPLETE) |
        NM(DEFFIELD) | NM(DEFARG) | NM(DEFGENARG) | NM(SETGENARG) |
        NM(MODULE_BODY) | NM(DEFCHOICE) | NM(WITHIN) | NM(THROW));
error step_type_destruct_mark(struct module *mod, struct node *node,
                              void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which == MODULE) {
    return 0;
  }

  inherit(mod, node);

  struct typ *not_typeable = TBI__NOT_TYPEABLE;
  struct node *first = subs_first(node);
  struct node *last = subs_last(node);

  switch (node->which) {
  case BIN:
    if (OP_KIND(node->as.BIN.operator) == OP_BIN_ACC) {
      mark_subs(mod, node, not_typeable, next(first), last, 1);
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
    if (subs_count_atleast(node, 1)
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
  case DEFALIAS:
  case DEFNAME:
  case DEFTYPE:
  case DEFINTF:
  case DEFINCOMPLETE:
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
    if (subs_count_atleast(node, 2)) {
      first->typ = not_typeable;
    }
    break;
  default:
    assert(false && "Unreached");
    break;
  }

  return 0;
}

static STEP_NM(step_type_mutability_mark,
               NM(BIN) | NM(UN));
static error step_type_mutability_mark(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  DSTEP(mod, node);
  struct typ *mutable = TBI__MUTABLE;
  struct typ *mercurial = TBI__MERCURIAL;

  struct node *first = subs_first(node);

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
      break;
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

static STEP_NM(step_type_gather_retval,
               NM(DEFFUN) | NM(DEFMETHOD));
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
    p = parent_const(p);
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

static STEP_NM(step_type_gather_excepts,
               NM(TRY) | NM(THROW));
static error step_type_gather_excepts(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  DSTEP(mod, node);
  error e;
  switch (node->which) {
  case TRY:
    module_excepts_open_try(mod, node);
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

// Does not itself check that 'args' are valid types for instantiation.
// This is done in passfwd.c:validate_genarg_types().
static error do_instantiate(struct node **result,
                            struct module *mod,
                            const struct node *for_error, ssize_t for_error_offset,
                            struct typ *t,
                            struct typ **args, size_t arity,
                            bool tentative) {
  assert(arity == 0 || arity == typ_generic_arity(t));

  struct node *gendef = typ_definition(t);
  if (vecnode_count(&node_toplevel(gendef)->generic->instances) == 0) {
    if (for_error == NULL) {
      assert(false);
    } else {
      error e = mk_except_type(mod, for_error, "not a generic functor");
      EXCEPT(e);
    }
  }

  struct node *pristine = *vecnode_get(&node_toplevel(gendef)->generic->instances, 0);
  struct node *instance = add_instance_deepcopy_from_pristine(mod, gendef,
                                                              pristine, tentative);
  node_toplevel(instance)->generic->for_error = for_error;

  // Do not use set_typ()! With second-order generics, we actually do not
  // want to update this value when linking to another type during
  // unification. If the functor for instance->typ gets linked to something
  // else, then instance->typ itself must be linked to something else.
  // Making instance unreachable (through a typ).
  // See also types.c:link_generic_arg_update().
  node_toplevel(instance)->generic->our_generic_functor_typ = t;

  struct node *genargs = subs_at(instance, IDX_GENARGS);
  struct node *ga = subs_first(genargs);
  for (size_t n = 0; n < arity; ++n) {
    node_set_which(ga, SETGENARG);
    if (args[n] == NULL) {
      ga = next(ga);
      continue;
    }

    struct node *ga_arg = subs_last(ga);
    node_set_which(ga_arg, DIRECTDEF);
    set_typ(&ga_arg->as.DIRECTDEF.typ, args[n]);
    ga_arg->as.DIRECTDEF.flags = NODE_IS_TYPE;

    if (for_error != NULL) {
      ga->as.SETGENARG.for_error = for_error_offset >= 0
        ? subs_at_const(for_error, for_error_offset+n)
        : for_error;
    }

    ga = next(ga);
  }

  error e = catchup_instantiation(mod, node_module_owner(gendef),
                                  instance, tentative);
  if (e) {
    char *n = typ_pretty_name(mod, t);
    e = mk_except_type(mod, for_error, "while instantiating generic here '%s'", n);
    free(n);
    THROW(e);
  }

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
  for (size_t n = 1, count = vecnode_count(&toplevel->generic->instances); n < count; ++n) {
    struct typ *i = (*vecnode_get(&toplevel->generic->instances, n))->typ;
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
  for (size_t n = 1, count = vecnode_count(&toplevel->generic->instances); n < count; ++n) {
    struct typ *i = (*vecnode_get(&toplevel->generic->instances, n))->typ;
    if (!typ_is_tentative(i) && typ_equal(t, i)) {
      return i;
    }
  }

  return NULL;
}

// Allows self-referential instances, such as, in (slice t):
//  alias us = slice t
static struct typ *find_existing_identical(struct module *mod,
                                           struct typ *t,
                                           struct typ **args,
                                           size_t arity) {
  const size_t first_explicit = typ_generic_first_explicit_arg(t);

  const struct node *d = typ_definition_const(t);
  const struct toplevel *toplevel = node_toplevel_const(d);
  for (size_t n = 1, count = vecnode_count(&toplevel->generic->instances); n < count; ++n) {
    struct typ *i = (*vecnode_get(&toplevel->generic->instances, n))->typ;

    size_t a;
    for (a = first_explicit; a < arity; ++a) {
      // Use pointer comparison: we're looking for the exact same arguments
      // (same tentative bit, etc.).
      if (typ_generic_arg_const(i, a) != args[a - first_explicit]) {
        break;
      }
    }

    if (a == arity) {
      return i;
    }
  }

  return NULL;
}

static bool are_all_tentative(struct typ **args, size_t arity) {
  for (size_t n = 0; n < arity; ++n) {
    if (args[n] == NULL) {
      continue;
    }
    if (!typ_is_tentative(args[n])) {
      return false;
    }
  }
  return true;
}

static struct typ *tentative_generic_arg(struct typ *t, size_t n) {
  struct typ *ga = typ_generic_arg(t, n);
  const size_t arity = typ_generic_arity(ga);
  if (arity == 0 || typ_is_generic_functor(ga)) {
    assert(!typ_is_tentative(ga));
    return typ_create_tentative(ga);
  }

  // Otherwise, the generic argument is declared as c:(`container t), where
  // t is another generic argument. The typ will be created by the type
  // inference step.

  return NULL;
}

error instance(struct node **result,
               struct module *mod,
               const struct node *for_error, size_t for_error_offset,
               struct typ *t, struct typ **args, size_t arity) {
  assert(arity == typ_generic_arity(t));

  const bool already_tentative_args = are_all_tentative(args, arity);
  const bool tentative = instantiation_is_tentative(mod, t, args, arity);

  error e;
  if (tentative && !already_tentative_args) {
    struct typ **tentative_args = calloc(arity, sizeof(struct typ *));
    for (size_t n = 0; n < arity; ++n) {
      tentative_args[n] = tentative_generic_arg(t, n);
    }

    e = do_instantiate(result, mod, for_error, for_error_offset,
                       t, tentative_args, arity, true);
    EXCEPT(e);

    free(tentative_args);

    const struct node *fe = subs_at_const(for_error, for_error_offset);
    for (size_t n = 0; n < arity; ++n) {
      if (args[n] == NULL) {
        continue;
      }
      e = unify(mod, fe,
                typ_generic_arg((*result)->typ, n),
                args[n]);
      EXCEPT(e);

      fe = next_const(fe);
    }

  } else {
    struct typ *r = find_existing_identical(mod, t, args, arity);
    if (r != NULL) {
      if (result != NULL) {
        *result = typ_definition(r);
      }
      return 0;
    }

    if (!already_tentative_args) {
      struct typ *r = find_existing_final(mod, t, args, arity);
      if (r != NULL) {
        if (result != NULL) {
          *result = typ_definition(r);
        }
        return 0;
      }
    }

    e = do_instantiate(result, mod, for_error, for_error_offset,
                       t, args, arity, tentative);
    EXCEPT(e);
  }

  return 0;
}

static error typ_ref(struct node **result,
                     struct module *mod, struct node *for_error,
                     enum token_type op, struct typ *typ) {
  // FIXME: when op is a wildcard (not substituted -- i.e. in the
  // uninstantiated generic), we will not catch all possible
  // incompatibilities. They will get caught when the function is
  // instantiated on an actual reference, but we're supposed to catch
  // everything prior to that.

  error e = instance(result, mod, for_error, 0,
                     mod->gctx->builtin_typs_for_refop[op], &typ, 1);
  EXCEPT(e);

  return 0;
}

static error check_terms_not_types(struct module *mod, struct node *node) {
  error e;
  int nth = 1;
  FOREACH_SUB_CONST(s, node) {
    if (s->flags & NODE_IS_TYPE) {
      e = mk_except_type(mod, s, "term %d cannot be a type", nth);
      THROW(e);
    }
    nth += 1;
  }
  return 0;
}

static struct node *follow_ssa(struct node *node) {
  struct node *expr = node;
  if (expr->which == IDENT) {
    struct node *def = expr->as.IDENT.def;
    if (def->which == DEFNAME
        && def->as.DEFNAME.ssa_user == expr) {
      expr = subs_last(def);
    }
  }
  return expr;
}

static error try_insert_automagic_deref(struct module *mod,
                                        struct node *node) {
  if (!typ_is_reference(node->typ)) {
    return 0;
  }

  struct node *expr = follow_ssa(node);
  if (expr->which == UN
      && expr->as.UN.operator == TREFDOT
      && expr->as.UN.is_explicit) {
    error e = mk_except_type(mod, expr, "explicit '@' operators are not"
                             " allowed for unqualified const references");
    THROW(e);
  }

  struct node *last_block = NULL;
  struct node *target = node;
  while (target->which == BLOCK) {
    last_block = target;
    unset_typ(&last_block->typ);

    target = subs_last(target);
  }

  struct node *par = parent(target);
  struct node *deref = mk_node(mod, par, UN);
  deref->codeloc = target->codeloc;
  deref->as.UN.operator = TDEREFDOT;

  node_subs_remove(par, deref);
  node_subs_replace(par, target, deref);
  node_subs_append(deref, target);

  const struct node *except[] = { target, NULL };
  error e = catchup(mod, except, deref, CATCHUP_BELOW_CURRENT);
  assert(!e);

  if (last_block != NULL) {
    struct node *n = last_block;
    while (true) {
      assert(n->which == BLOCK);
      set_typ(&n->typ, subs_last(n)->typ);

      if (n == node) {
        break;
      }
      n = parent(n);
    }
  }

  return 0;
}

static error type_inference_un(struct module *mod, struct node *node) {
  assert(node->which == UN);
  error e;
  const enum token_type operator = node->as.UN.operator;
  struct node *term = subs_first(node);

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
    return 0;
  case OP_UN_DEREF:
    e = typ_check_can_deref(mod, term, term->typ, operator);
    EXCEPT(e);
    e = typ_check_deref_against_mark(mod, node, node->typ, operator);
    EXCEPT(e);
    set_typ(&node->typ, typ_generic_arg(term->typ, 0));
    node->flags |= term->flags & NODE__TRANSITIVE;
    return 0;

  case OP_UN_BOOL:
  case OP_UN_ARITH:
  case OP_UN_BW:
    break;
  default:
    assert(false);
  }

  e = check_terms_not_types(mod, node);
  EXCEPT(e);

  e = try_insert_automagic_deref(mod, term);
  EXCEPT(e);
  term = subs_first(node);

  switch (OP_KIND(operator)) {
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

static error type_inference_bin_sym(struct module *mod, struct node *node) {
  assert(node->which == BIN);

  const enum token_type operator = node->as.BIN.operator;

  struct node *left = subs_first(node);
  struct node *right = subs_last(node);
  error e;

  if (!OP_IS_ASSIGN(operator) && OP_KIND(operator) != OP_BIN_SYM_PTR) {
    e = try_insert_automagic_deref(mod, left);
    EXCEPT(e);
    e = try_insert_automagic_deref(mod, right);
    EXCEPT(e);

    left = subs_first(node);
    right = subs_last(node);
  }

  if (operator == TASSIGN) {
    if (typ_equal(right->typ, TBI_VOID)) {
      if (left->which == IDENT
          && left->as.IDENT.def->which == DEFNAME
          && left->as.IDENT.def->as.DEFNAME.ssa_user != NULL) {
        // noop
      } else {
        e = mk_except(mod, node, "cannot assign an expression of type 'void'");
        THROW(e);
      }
    }

    e = unify_refcompat(mod, node, left->typ, right->typ);
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

static size_t codeloc_pos_after(struct module *mod, struct node *node) {
  struct node *n = node;
  while (next(n) == NULL || next(n)->codeloc.pos == node->codeloc.pos) {
    if (parent(n) == NULL) {
      break;
    }
    n = parent(n);
  }

  n = next(n);
  if (n == NULL) {
    return mod->parser.len;
  } else {
    return n->codeloc.pos;
  }
}

static char *quote_code(const char *data, size_t start, size_t end) {
  int len = 0;
  for (size_t n = start; n < end; ++n) {
    switch (data[n]) {
    case '\n':
      goto done;
    case '"':
      len += 1;
      break;
    }
    len += 1;
  }

done:
  ;char *r = calloc(2 + len + 1, sizeof(char));
  sprintf(r, "\"%.*s\"", len, data + start);
  return r;
}

EXAMPLE(quote_code) {
  const char *code = "abcd\nefgh";
  assert(strcmp("\"abcd\"", quote_code(code, 0, 9)) == 0);
  assert(strcmp("\"efgh\"", quote_code(code, 5, 9)) == 0);
  assert(strcmp("\"d\"", quote_code(code, 3, 5)) == 0);
}

static void try_filling_codeloc(struct module *mod, struct node *named,
                                struct node *node) {
  if (node_ident(named) != ID_NCODELOC) {
    return;
  }

  node_subs_remove(named, subs_first(named));
  GSTART();
  G0(init, named, INIT,
     G_IDENT(wheren, "where");
     G(wheres, STRING);
     G_IDENT(exprn, "expression");
     G(exprs, STRING));

  const size_t len = 64 + strlen(mod->filename);
  char *buf = calloc(len, sizeof(char));
  snprintf(buf, len, "\"%s:%d:%d\"", mod->filename,
           node->codeloc.line, node->codeloc.column);
  wheres->as.STRING.value = buf;

  exprs->as.STRING.value = quote_code(mod->parser.data, node->codeloc.pos,
                                      codeloc_pos_after(mod, node));
}

static void insert_missing_optional_arg(struct module *mod, struct node *node,
                                        struct node *after_this, ident name) {
  assert(name != idents_add_string(mod->gctx, "v", 1));
  GSTART();
  G0(named, node, CALLNAMEDARG,
     named->as.CALLNAMEDARG.name = name;
     G(nul, NUL));
  node_subs_remove(node, named);
  node_subs_insert_after(node, after_this, named);

  try_filling_codeloc(mod, named, node);

  error e = catchup(mod, NULL, named, CATCHUP_BELOW_CURRENT);
  assert(!e);
}

static error fill_in_optional_args(struct module *mod, struct node *node,
                                   const struct typ *tfun) {
  const struct node *dfun = typ_definition_const(tfun);
  const size_t dmin = node_fun_min_args_count(dfun);
  const size_t dmax = node_fun_max_args_count(dfun);

  if (dmin == dmax) {
    return 0;
  }

  const struct node *funargs = subs_at_const(dfun, IDX_FUNARGS);
  const struct node *darg = subs_first_const(funargs);
  struct node *arg = next(subs_first(node));

  ssize_t n, code_pos = 0;
  error e;
  for (n = 0; n < dmin; ++n, ++code_pos) {
    if (arg == NULL) {
      e = mk_except(mod, arg, "missing positional argument '%s' at position %zd",
                    idents_value(mod->gctx, node_ident(darg)), code_pos);
      THROW(e);
    } else if (arg->which == CALLNAMEDARG) {
      if (node_ident(arg) != node_ident(darg)) {
        e = mk_except(mod, arg, "named argument '%s' has bad name"
                      " or appears out of order at position %zd",
                      idents_value(mod->gctx, node_ident(arg)), code_pos);
        THROW(e);
      }
    }

    darg = next_const(darg);
    arg = next(arg);
  }

  const ssize_t first_vararg = node_fun_first_vararg(dfun);
  for (n = dmin; n < dmax && (first_vararg == - 1 || n < first_vararg); ++n) {
    if (arg == NULL) {
      insert_missing_optional_arg(mod, node, subs_last(node), node_ident(darg));

    } else if (arg->which != CALLNAMEDARG) {
      // Assume this is the first vararg

      if (first_vararg == -1) {
        e = mk_except(mod, arg, "excessive positional argument"
                      " or optional argument lacks a name at position %zd", code_pos);
        THROW(e);
      }

      insert_missing_optional_arg(mod, node, prev(arg), node_ident(darg));

    } else if (arg->which == CALLNAMEDARG) {
      const ident name = node_ident(arg);

      while (node_ident(darg) != name) {
        insert_missing_optional_arg(mod, node, prev(arg), node_ident(darg));

        darg = next_const(darg);
        n += 1;
        if ((first_vararg != -1 && n >= first_vararg)
            || next_const(darg) == NULL) {
          e = mk_except(mod, arg, "named argument '%s' has bad name"
                        " or appears out of order at position %zd",
                        idents_value(mod->gctx, name), code_pos);
          THROW(e);
        }
      }

      arg = next(arg);
      code_pos += 1;
    }

    darg = next_const(darg);
  }

  assert(arg == NULL || first_vararg >= 0);
  while (arg != NULL) {
    if (arg->which == CALLNAMEDARG) {
      const ident name = node_ident(arg);
      e = mk_except(mod, arg, "excess named argument '%s'"
                    " or appears out of order at position %zd",
                    idents_value(mod->gctx, name), code_pos);
      THROW(e);
    }
    arg = next(arg);
    code_pos += 1;
  }

  return 0;
}

static error rewrite_unary_call(struct module *mod, struct node *node, struct typ *tfun) {
  struct node *fun = node_new_subnode(mod, node);
  node_subs_remove(node, fun);
  node_move_content(fun, node);
  node_subs_append(node, fun);
  node_set_which(node, CALL);
  set_typ(&fun->typ, tfun);

  error e = fill_in_optional_args(mod, node, tfun);
  assert(!e);

  const struct node *except[] = { fun, NULL };
  e = catchup(mod, except, node, CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

struct node *instance_fully_implicit(struct module *mod,
                                     const struct node *for_error,
                                     struct typ *t) {
  assert(typ_is_generic_functor(t));

  const size_t gen_arity = typ_generic_arity(t);
  struct typ **args = calloc(gen_arity, sizeof(struct typ *));
  for (size_t n = 0; n < gen_arity; ++n) {
    assert(!typ_is_tentative(typ_generic_arg(t, n)));
    args[n] = tentative_generic_arg(t, n);
  }

  struct node *i = NULL;
  error e = instance(&i, mod, for_error, -1,
                     t, args, gen_arity);
  assert(!e);
  free(args);

  return i;
}

static void bin_accessor_maybe_functor(struct module *mod, struct node *par) {
  // Something like the (hypothetical): vector.mk_filled 100 0:u8
  // 'vector' is a generic functor, and the instantiation will be done
  // through the call to the function 'vector.mk_filled'. We need to have a
  // fully tentative instance of 'vector' so that the unification of '0:u8'
  // with t:`copyable succeeds.
  if (typ_is_generic_functor(par->typ)) {
    struct node *i = instance_fully_implicit(mod, par, par->typ);
    unset_typ(&par->typ);
    set_typ(&par->typ, i->typ);
  }
}

static bool bin_accessor_maybe_ref(struct node **parent_scope,
                                   struct module *mod, struct node *par) {
  if (typ_is_reference(par->typ)) {
    *parent_scope = typ_definition(typ_generic_arg(par->typ, 0));
    return true;
  }
  return false;
}

static void bin_accessor_maybe_defchoice(struct node **parent_scope, struct node *for_error,
                                         struct module *mod, struct node *par) {
  if (par->flags & NODE_IS_DEFCHOICE) {
    struct node *defchoice = NULL;
    error e = scope_lookup_ident_immediate(&defchoice, for_error, mod,
                                           &typ_definition(par->typ)->scope,
                                           node_ident(subs_last(par)), false);
    assert(!e);
    assert(defchoice->which == DEFCHOICE);

    *parent_scope = defchoice;
  }
}

static error type_inference_bin_accessor(struct module *mod, struct node *node) {
  error e;

  enum token_type operator = node->as.BIN.operator;
  const struct typ *mark = node->typ;

  struct node *left = subs_first(node);
  bin_accessor_maybe_functor(mod, left);

  struct node *dcontainer = typ_definition(left->typ);
  if (!bin_accessor_maybe_ref(&dcontainer, mod, left)) {
    bin_accessor_maybe_defchoice(&dcontainer, node, mod, left);
  }
  struct scope *container_scope = &dcontainer->scope;

  const bool container_is_tentative = typ_is_tentative(scope_node(container_scope)->typ);

  struct node *name = subs_last(node);
  struct node *field = NULL;
  e = scope_lookup_ident_immediate(&field, name, mod, container_scope,
                                   node_ident(name), container_is_tentative);
  if (container_is_tentative && e == EINVAL) {
    struct node *dinc = defincomplete_create(mod, node);
    defincomplete_add_field(mod, node, dinc, node_ident(name), TBI_ANY);
    e = defincomplete_catchup(mod, dinc);
    EXCEPT(e);

    e = unify(mod, node, left->typ, dinc->typ);
    EXCEPT(e);

    e = scope_lookup_ident_immediate(&field, name, mod, &dinc->scope,
                                     node_ident(name), false);
    EXCEPT(e);
  } else {
    EXCEPT(e);
  }

  if (field->which == IMPORT && !field->as.IMPORT.intermediate_mark) {
    e = scope_lookup(&field, mod, &mod->gctx->modules_root.scope,
                     subs_first(field), false);
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
    if (operator == TWILDCARD && typ_is_reference(field->typ)) {
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
    if (!(node->flags & NODE_IS_TEMPORARY)
        && !(subs_first(node)->flags & NODE_IS_DEFCHOICE)) {
      e = typ_check_deref_against_mark(mod, node, mark, operator);
      EXCEPT(e);
    }
  }

  return 0;
}

static error type_inference_bin_rhs_unsigned(struct module *mod, struct node *node) {
  error e;
  struct node *left = subs_first(node);
  struct node *right = subs_last(node);

  e = try_insert_automagic_deref(mod, right);
  EXCEPT(e);
  right = subs_last(node);

  e = unify(mod, right, right->typ, TBI_U32);
  EXCEPT(e);

  set_typ(&node->typ, typ_create_tentative(TBI_BITWISE));
  e = unify(mod, node, left->typ, node->typ);
  EXCEPT(e);

  return 0;
}

static error type_inference_bin_rhs_type(struct module *mod, struct node *node) {
  error e;
  struct node *left = subs_first(node);
  struct node *right = subs_last(node);

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

  error e;
  switch (OP_KIND(node->as.BIN.operator)) {
  case OP_BIN_SYM:
  case OP_BIN_SYM_BOOL:
  case OP_BIN_SYM_ARITH:
  case OP_BIN_SYM_BW:
  case OP_BIN_SYM_PTR:
    e = check_terms_not_types(mod, node);
    EXCEPT(e);
    return type_inference_bin_sym(mod, node);
  case OP_BIN_BW_RHS_UNSIGNED:
    e = check_terms_not_types(mod, node);
    EXCEPT(e);
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
  const size_t arity = subs_count(node);
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

static void type_inference_init_named(struct module *mod, struct node *node) {
  struct node *dinc = defincomplete_create(mod, node);

  FOREACH_SUB_EVERY(s, node, 0, 2) {
    const struct node *left = s;
    const struct node *right = next(s);
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

static void type_inference_init_isalist_literal(struct module *mod, struct node *node) {
  struct node *dinc = defincomplete_create(mod, node);

  FOREACH_SUB(s, node) {
    defincomplete_add_isa(mod, s, dinc, s->typ);
  }

  error e = defincomplete_catchup(mod, dinc);
  assert(!e);
  set_typ(&node->typ, dinc->typ);

  node->flags |= NODE_IS_TYPE;
}

static error type_inference_init(struct module *mod, struct node *node) {
  assert(node->which == INIT);
  if (node->as.INIT.is_array) {
    if (typ_definition(subs_first(node)->typ)->which == DEFINTF) {
      type_inference_init_isalist_literal(mod, node);
      return 0;
    } else {
      return type_inference_init_array(mod, node);
    }
  } else {
    type_inference_init_named(mod, node);
    return 0;
  }
}

static error type_inference_return(struct module *mod, struct node *node) {
  assert(node->which == RETURN);

  if (subs_count_atleast(node, 1)) {
    struct node *arg = subs_first(node);
    error e = unify_refcompat(mod, arg, module_retval_get(mod)->typ, arg->typ);
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

static struct node *expr_ref(struct module *mod, struct node *par,
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

  struct node *n = mk_node(mod, par, UN);
  n->codeloc = node->codeloc;
  n->as.UN.operator = refop;
  node_subs_append(n, node);
  return n;
}

static error rewrite_self(struct module *mod, struct node *node,
                          struct node *fun) {
  assert(fun->which == BIN);

  struct node *old_self = subs_first(fun);
  struct node *self;
  if (typ_is_reference(old_self->typ)) {
    node_subs_remove(fun, old_self);
    node_subs_insert_after(node, subs_first(node), old_self);
    self = old_self;
  } else {
    node_subs_remove(fun, old_self);
    enum token_type access = refop_for_accop[fun->as.BIN.operator];
    struct node *s = expr_ref(mod, node, access, old_self);
    node_subs_remove(node, s);
    node_subs_insert_after(node, subs_first(node), s);
    self = s;
  }

  const struct node *except[] = { old_self, NULL };
  error e = catchup(mod, except, self, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  if (typ_is_reference(self->typ)) {
    e = typ_check_can_deref(mod, fun, self->typ,
                            derefop_for_accop[fun->as.BIN.operator]);
    EXCEPT(e);
  }

  return 0;
}

static bool compare_ref_depth(const struct typ *target, const struct typ *arg,
                              int diff) {
  assert(!typ_equal(target, TBI_ANY_ANY_REF));

  if (typ_equal(arg, TBI_LITERALS_NULL)) {
    return diff != 1;
  }

  int dtarget = 0;
  while (typ_is_reference(target)) {
    dtarget += 1;
    target = typ_generic_arg_const(target, 0);
  }

  int darg = 0;
  while (typ_is_reference(arg)) {
    darg += 1;
    arg = typ_generic_arg_const(arg, 0);

    if (typ_equal(arg, TBI_LITERALS_NULL)) {
      return diff != 1;
    }
  }

  return dtarget == darg + diff;
}

static error try_insert_const_ref(struct module *mod, struct node *node,
                                  const struct typ *target,
                                  enum token_type target_explicit_ref,
                                  struct node *arg) {
  if (!typ_is_reference(target)) {
    return 0;
  }

  const bool is_named = arg->which == CALLNAMEDARG;
  struct node *real_arg = is_named ? subs_first(arg) : arg;
  struct node *expr_arg = follow_ssa(real_arg);

  if (target_explicit_ref == TREFDOT || target_explicit_ref == TNULREFDOT) {
    if (compare_ref_depth(target, real_arg->typ, 1)) {
      if (!typ_isa(target, TBI_ANY_MUTABLE_REF)) {
        struct node *before = prev(real_arg);

        struct node *par = parent(real_arg);
        node_subs_remove(par, real_arg);
        struct node *ref_arg = expr_ref(mod, par, TREFDOT, real_arg);
        node_subs_remove(par, ref_arg);
        node_subs_insert_after(par, before, ref_arg);

        if (is_named) {
          unset_typ(&arg->typ);
        }

        const struct node *except[] = { real_arg, NULL };
        error e = catchup(mod, except,
                          is_named ? arg : ref_arg,
                          CATCHUP_BELOW_CURRENT);
        EXCEPT(e);
      }
    } else if (compare_ref_depth(target, real_arg->typ, 0)) {
      if (expr_arg->which == UN
          && expr_arg->as.UN.operator == TREFDOT
          && expr_arg->as.UN.is_explicit
          && !typ_equal(subs_first(expr_arg)->typ, TBI_LITERALS_NULL)) {
        error e = mk_except_type(mod, expr_arg, "explicit '@' operators are not"
                                 " allowed for unqualified const references");
        THROW(e);
      }
    }
  } else {
    // We do not automagically insert const ref operators when the
    // function does not always accept a reference, as in: (also see
    // t00/automagicref.n)
    //   s (method t:`any) foo p:t = void
    //     noop
    //   (s.foo @i32) self @1 -- '@' required on 1.
    //   (s.foo i32) self 1

    // noop -- this error will get caught by regular argument typing.
  }

  return 0;
}

static error try_insert_const_deref(struct module *mod, struct node *node,
                                    const struct typ *target,
                                    enum token_type target_explicit_ref,
                                    struct node *arg) {
  if (typ_is_reference(target)) {
    return 0;
  }

  const bool is_named = arg->which == CALLNAMEDARG;
  struct node *real_arg = is_named ? subs_first(arg) : arg;

  if (target_explicit_ref == 0 && typ_isa(arg->typ, TBI_ANY_REF)) {
    struct node *before = prev(real_arg);

    struct node *par = parent(real_arg);
    node_subs_remove(par, real_arg);
    struct node *deref_arg = mk_node(mod, par, UN);
    deref_arg->as.UN.operator = TDEREFDOT;
    node_subs_append(deref_arg, real_arg);
    node_subs_remove(par, deref_arg);
    node_subs_insert_after(par, before, deref_arg);

    if (is_named) {
      unset_typ(&arg->typ);
    }

    const struct node *except[] = { real_arg, NULL };
    error e = catchup(mod, except,
                      is_named ? arg : deref_arg,
                      CATCHUP_BELOW_CURRENT);
    EXCEPT(e);
  }

  return 0;
}

static enum token_type has_explicit_ref(const struct node *dfun, size_t n) {
  const struct node *funargs = subs_at_const(dfun, IDX_FUNARGS);
  const struct node *darg = subs_at_const(funargs, n);
  if (subs_last_const(darg)->which == UN) {
    return subs_last_const(darg)->as.UN.operator;
  }
  return 0;
}

static error process_automagic_call_arguments(struct module *mod,
                                              struct node *node,
                                              const struct typ *tfun) {
  if (!subs_count_atleast(node, 2)) {
    return 0;
  }

  const struct node *dfun = typ_definition_const(tfun);
  const ssize_t first_vararg = node_fun_first_vararg(dfun);

  error e;
  ssize_t n = 0;
  struct node *last = NULL;
  struct node *nxt = subs_at(node, 1);
  while (nxt != NULL) {
    if (n == first_vararg) {
      break;
    }

    // We record 'nxt' now as try_insert_const_{,de}ref() may move 'arg'.
    struct node *arg = nxt;
    nxt = next(nxt);

    const enum token_type explicit_ref = has_explicit_ref(dfun, n);

    e = try_insert_const_ref(mod, node,
                             typ_function_arg_const(tfun, n),
                             explicit_ref, arg);
    EXCEPT(e);

    e = try_insert_const_deref(mod, node,
                               typ_function_arg_const(tfun, n),
                               explicit_ref, arg);
    EXCEPT(e);

    n += 1;
    last = arg;
  }

  if (n == first_vararg) {
    const struct typ *target = typ_generic_arg_const(
      typ_function_arg_const(tfun, n), 0);

    struct node *nxt = last == NULL
      ? next(subs_first(node)) : next(last);
    while (nxt != NULL) {
      // We record 'nxt' now as try_insert_const_ref() may move 'arg'.
      struct node *arg = nxt;
      nxt = next(nxt);

      e = try_insert_const_ref(mod, node, target, TREFDOT, arg);
      EXCEPT(e);
    }
  }

  return 0;
}

static error prepare_call_arguments(struct module *mod, struct node *node) {
  error e;
  struct node *fun = subs_first(node);

  const struct node *dfun = typ_definition_const(fun->typ);
  const size_t dmin = node_fun_min_args_count(dfun);
  const size_t dmax = node_fun_max_args_count(dfun);

  const size_t args = subs_count(node) - 1;

  switch (dfun->which) {
  case DEFFUN:
    if (args < dmin || args > dmax) {
      e = mk_except_call_args_count(mod, node, dfun, false, args);
      THROW(e);
    }
    break;
  case DEFMETHOD:
    if (fun->which == BIN) {
      if ((subs_first(fun)->flags & NODE_IS_TYPE)) {
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

        e = catchup(mod, NULL, m, CATCHUP_BELOW_CURRENT);
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

  e = process_automagic_call_arguments(mod, node, fun->typ);
  EXCEPT(e);

  return 0;
}

static error explicit_instantiation(struct module *mod, struct node *node) {
  error e;
  struct node *what = subs_first(node);
  if (what->which == BIN && !(subs_first(what)->flags & NODE_IS_TYPE)) {
    e = mk_except_type(mod, what,
                       "explicit generic instantion must use a type as functor;"
                       " e.g. not 'self.method i32'"
                       " but '(my_struct.method i32) self' (FIXME: remove this restriction)");
    THROW(e);
  }

  struct typ *t = what->typ;
  const size_t given_arity = subs_count(node) - 1;

  const size_t arity = typ_generic_arity(t);
  const size_t first_explicit = typ_generic_first_explicit_arg(t);
  const size_t explicit_arity = arity - first_explicit;
  if (given_arity != explicit_arity) {
    e = mk_except_type(mod, node,
                       "invalid number of explicit generic arguments:"
                       " %zu expected, but %zu given",
                       explicit_arity, given_arity);
    THROW(e);
  }

  struct typ **args = calloc(arity, sizeof(struct typ *));
  size_t n;
  for (n = 0; n < first_explicit; ++n) {
    args[n] = tentative_generic_arg(t, n);
  }
  FOREACH_SUB_EVERY(s, node, 1, 1) {
    args[first_explicit + n] = s->typ;
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
  struct typ *tfun = subs_first(node)->typ;
  const size_t arity = subs_count(node) - 1;

  // Already checked in prepare_call_arguments().
  assert(arity == typ_function_arity(tfun));

  struct node *i = instance_fully_implicit(mod, node, tfun);

  size_t n = 0;
  FOREACH_SUB_EVERY(s, node, 1, 1) {
    e = unify_refcompat(mod, s, typ_function_arg(i->typ, n), s->typ);
    EXCEPT(e);

    if (n == 0) {
      const struct node *genargs = subs_at_const(i, IDX_GENARGS);
      if (i->which == DEFMETHOD
          && subs_count_atleast(genargs, 2)
          && node_ident(subs_first_const(genargs)) == ID_WILDCARD_REF_ARG_SELF) {
        const struct node *wildcard = subs_at_const(genargs, 1);
        if ((node_toplevel_const(i)->flags & TOP_IS_SHALLOW)
            && typ_equal(typ_generic_functor(s->typ), TBI_MREF)) {
          e = unify(mod, s, wildcard->typ, TBI_MMREF);
          EXCEPT(e);
        } else {
          assert(typ_is_reference(s->typ));

          e = unify(mod, s, wildcard->typ, typ_generic_functor(s->typ));
          EXCEPT(e);
        }
      }
    }

    n += 1;
  }

  set_typ(&subs_first(node)->typ, i->typ);
  record_topdep(mod, i->typ);
  set_typ(&node->typ, typ_function_return(i->typ));

  return 0;
}

static error function_instantiation(struct module *mod, struct node *node) {
  assert(subs_count_atleast(node, 2));

  error e;
  if (subs_at(node, 1)->flags & NODE_IS_TYPE) {
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
  for (struct node *s = arg0; s != NULL; s = next(s)) {
    if (s != arg0 && (flags & NODE_IS_TYPE) != (s->flags & NODE_IS_TYPE)) {
      error e = mk_except_type(mod, s, "expression combines types and values");
      THROW(e);
    }
    flags |= s->flags;
  }

  return 0;
}

static error type_inference_explicit_unary_call(struct module *mod, struct node *node, struct node *dfun) {
  const size_t count = subs_count(node);
  if (dfun->which == DEFFUN && count != 1) {
    error e = mk_except_call_args_count(mod, node, dfun, false, count - 1);
    THROW(e);
  } else if (dfun->which == DEFMETHOD && count != 2) {
    error e = mk_except_call_args_count(mod, node, dfun, false, count - 1);
    THROW(e);
  }

  if (dfun->which == DEFMETHOD) {
    struct node *self = subs_at(node, 1);
    error e = unify(mod, self, typ_function_arg(dfun->typ, 0), self->typ);
    EXCEPT(e);
  }

  set_typ(&node->typ, typ_function_return(dfun->typ));

  return 0;
}

static error type_inference_call(struct module *mod, struct node *node) {
  error e;
  struct node *fun = subs_first(node);
  struct typ *tfun = fun->typ;
  struct node *dfun = typ_definition(tfun);

  if (!node_is_fun(dfun)
      || (subs_count_atleast(node, 2)
          && (subs_at(node, 1)->flags & NODE_IS_TYPE))) {

    if (!node_is_fun(dfun)
        && (!node_can_have_genargs(dfun)
            || !subs_count_atleast(subs_at(dfun, IDX_GENARGS), 1))) {
      char *n = typ_pretty_name(mod, dfun->typ);
      e = mk_except_type(mod, fun, "'%s' not a generic type", n);
      free(n);
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

  if (subs_count_atleast(subs_at(dfun, IDX_GENARGS), 1)
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
    e = unify_refcompat(mod, arg, typ_function_arg(tfun, n), arg->typ);
    EXCEPT(e);
    n += 1;
  }

  if (n == first_vararg) {
    struct typ *target = typ_generic_arg(typ_function_arg(tfun, n), 0);

    FOREACH_SUB_EVERY(arg, node, 1 + n, 1) {
      e = unify_refcompat(mod, arg, target, arg->typ);
      EXCEPT(e);
    }
  }

  set_typ(&node->typ, typ_function_return(tfun));

  return 0;
}

static error type_inference_block(struct module *mod, struct node *node) {
  error e;

  struct node *last_typable = subs_last(node);
  while (typ_equal(last_typable->typ, TBI__NOT_TYPEABLE)) {
    last_typable = prev(last_typable);
  }

  FOREACH_SUB(s, node) {
    if ((s->flags & NODE_IS_TYPE)) {
      e = mk_except_type(mod, s, "block statements cannot be type names");
      THROW(e);
    }

    if (s->typ == TBI__NOT_TYPEABLE) {
      continue;
    }

    if (s == last_typable) {
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

  if (last_typable->which == RETURN) {
    // FIXME: should make sure there are no statements after a RETURN.
    set_typ(&node->typ, TBI_VOID);
  } else {
    set_typ(&node->typ, last_typable->typ);
  }

  return 0;
}

static error type_inference_if(struct module *mod, struct node *node) {
  error e;

  struct node *cond = subs_first(node);
  e = unify(mod, cond, cond->typ,
            typ_create_tentative(TBI_GENERALIZED_BOOLEAN));
  EXCEPT(e);

  struct node *yes = next(cond);
  struct node *els = subs_last(node);
  e = unify(mod, els, yes->typ, els->typ);
  EXCEPT(e);

  set_typ(&node->typ, yes->typ);

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

  e = unify(mod, pattern, pattern->typ, expr->typ);
  EXCEPT(e);

  return 0;
}

static error type_inference_match(struct module *mod, struct node *node) {
  error e;

  struct node *expr = subs_first(node);
  e = try_insert_automagic_deref(mod, expr);
  EXCEPT(e);
  expr = subs_first(node);

  FOREACH_SUB_EVERY(s, node, 1, 2) {
    e = unify_match_pattern(mod, expr, s);
    EXCEPT(e);
  }

  set_typ(&node->typ, subs_at(node, 2)->typ);
  if (subs_count_atleast(node, 4)) {
    FOREACH_SUB_EVERY(s, node, 4, 2) {
      e = unify(mod, s, s->typ, node->typ);
      EXCEPT(e);
    }
  }

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
    THROW(e);
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

struct phi_tracker_state *get_phi_tracker(struct node *def) {
  switch (def->which) {
  case DEFNAME:
    return def->as.DEFNAME.phi_state;
  case DEFARG:
    return def->as.DEFARG.phi_state;
  default:
    assert(false);
    return NULL;
  }
}

#define OR_ELSE(p, def) ( (p) != NULL ? (p) : (def) )

static struct node *scoped_block_surrounding(struct node *node) {
  struct node *n = node;
  do {
    n = parent(n);
  } while (n->which != BLOCK || n->as.BLOCK.is_scopeless);
  return n;
}

static error branching_block_foreach_scoped_ident(struct module *mod,
                                                  struct node *branching,
                                                  struct node *block,
                                                  scope_each each) {
  error e;
  struct node *fun = mod->state->top_state->top;
  assert(fun->which == DEFFUN || fun->which == DEFMETHOD);
  struct node *funargs = subs_at(fun, IDX_FUNARGS);
  FOREACH_SUB(arg, funargs) {
    e = each(mod, arg, block);
    EXCEPT(e);
  }

  struct node *top_block = subs_last(fun);
  struct node *b = branching;
  do {
    b = scoped_block_surrounding(b);

    e = scope_foreach(mod, &b->scope, each, block);
    EXCEPT(e);
  } while (b != top_block);

  return 0;
}

static struct node *insert_conditioned_phi(struct module *mod,
                                           struct branch_state *br_st,
                                           struct node *br_block,
                                           struct node *pre_branch_use) {
  if (br_st->branching->which == TRY) {
    // FIXME: unsupported in try/catch
    return pre_branch_use;
  }

  assert(br_block->which == BLOCK);
  struct node *phi = mk_node(mod, br_block, PHI);
  phi->as.PHI.is_conditioned = true;
  switch (pre_branch_use->which) {
  case PHI:
    phi->as.PHI.def = pre_branch_use->as.PHI.def;
    break;
  case IDENT:
    phi->as.PHI.def = pre_branch_use->as.IDENT.def;
    break;
  case DEFARG:
  case DEFNAME:
    phi->as.PHI.def = pre_branch_use;
    break;
  default:
    assert(false);
  }

  struct ancestor ancestor = { .prev = pre_branch_use, 0 };
  if (br_st->prev != NULL) {
    ancestor.cond = br_st->prev->cond;
    ancestor.reversed = br_st->prev->reversed;
  }
  vecancestor_push(&phi->as.PHI.ancestors, ancestor);

  node_subs_remove(br_block, phi);
  node_subs_insert_before(br_block, subs_first(br_block), phi);

  error e = catchup(mod, NULL, phi, CATCHUP_AFTER_CURRENT);
  assert(!e);

  return phi;
}

static error insert_all_possible_conditioned_phi_each(struct module *mod,
                                                      struct node *def,
                                                      void *user) {
  struct node *br_block = user;
  if (!local_name_is_scoped(mod, def)) {
    return 0;
  }
  assert(def->which == DEFNAME || def->which == DEFARG);

  struct branch_state *br_st = mod->state->branch_state;
  struct phi_tracker_state *phi_st = get_phi_tracker(def);

  struct node *prev = phi_st->prev != NULL
    ? OR_ELSE(phi_st->prev->last, def) : def;
  struct node *phi = insert_conditioned_phi(mod, br_st, br_block, prev);
  phi_st->last = phi;
  return 0;
}

static error insert_all_possible_conditioned_phi(struct module *mod,
                                                 struct node *block) {
  error e = branching_block_foreach_scoped_ident(
    mod, parent(block), block, insert_all_possible_conditioned_phi_each);
  EXCEPT(e);
  return 0;
}

static error init_phi_trackers_for_branching_each(struct module *mod,
                                                  struct node *def,
                                                  void *user) {
  struct node *br_block = user;
  assert(br_block == NULL);

  if (!local_name_is_scoped(mod, def)) {
    return 0;
  }
  assert(def->which == DEFNAME || def->which == DEFARG);

  switch (def->which) {
  case DEFNAME:
    PUSH_STATE(def->as.DEFNAME.phi_state);
    break;
  case DEFARG:
    PUSH_STATE(def->as.DEFARG.phi_state);
    break;
  default:
    assert(false);
  }

  return 0;
}

static void init_phi_trackers_for_branching(struct module *mod) {
  struct branch_state *br_st = mod->state->branch_state;
  struct node *branching = br_st->branching;

  error e = branching_block_foreach_scoped_ident(
    mod, branching, NULL, init_phi_trackers_for_branching_each);
  assert(!e);
}

static error uninit_phi_trackers_for_branching_each(struct module *mod,
                                                    struct node *def,
                                                    void *user) {
  struct node *br_block = user;
  assert(br_block == NULL);

  if (!local_name_is_scoped(mod, def)) {
    return 0;
  }
  assert(def->which == DEFNAME || def->which == DEFARG);

  switch (def->which) {
  case DEFNAME:
    POP_STATE(def->as.DEFNAME.phi_state);
    break;
  case DEFARG:
    POP_STATE(def->as.DEFARG.phi_state);
    break;
  default:
    assert(false);
  }

  return 0;
}

static void uninit_phi_trackers_for_branching(struct module *mod) {
  struct branch_state *br_st = mod->state->branch_state;
  struct node *branching = br_st->branching;

  error e = branching_block_foreach_scoped_ident(
    mod, branching, NULL, uninit_phi_trackers_for_branching_each);
  assert(!e);
}

static STEP_NM(step_branching_down,
               STEP_NM_BRANCHING);
static error step_branching_down(struct module *mod, struct node *node,
                                 void *user, bool *stop) {
  DSTEP(mod, node);

  PUSH_STATE(mod->state->branch_state);
  struct branch_state *st = mod->state->branch_state;
  st->branching = node;
  st->cond = NULL;
  st->reversed = false;

  return 0;
}

static size_t find_in_parent(const struct node *par, const struct node *node) {
  size_t n = 0;
  FOREACH_SUB_CONST(s, par) {
    if (s == node) {
      return n;
    }
    n += 1;
  }
  assert(false);
  return 0;
}

static STEP_NM(step_branching_block_down,
               NM(BLOCK));
static error step_branching_block_down(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  DSTEP(mod, node);
  struct node *par = parent(node);
  const size_t nth_sub = find_in_parent(par, node);
  struct branch_state *st = mod->state->branch_state;

  switch (par->which) {
  case WHILE:
    if (nth_sub == 0) {
      st->cond = NULL;
      return 0;
    } else {
      st->cond = subs_first(par);
    }
    break;
  case IF:
    if (nth_sub == 0) {
      st->cond = NULL;
      return 0;
    } else if (nth_sub == 1) {
      st->cond = subs_first(par);
      st->reversed = false;
    } else if (nth_sub == 2) {
      // Else branch.
      st->cond = subs_first(par);
      st->reversed = true;
    } else {
      assert(false);
    }
    break;
  case MATCH:
    if (nth_sub == 0) {
      st->cond = NULL;
    } else {
      st->cond = subs_first(par);
    }
    break;
  case TRY:
    return 0;
  default:
    return 0;
  }

  assert(st->branching == par);

  return 0;
}

static STEP_NM(step_branching_block_down_phi,
               NM(BLOCK));
static error step_branching_block_down_phi(struct module *mod, struct node *node,
                                        void *user, bool *stop) {
  DSTEP(mod, node);

  const struct node *par = parent_const(node);
  if (!(NM(par->which) & STEP_NM_BRANCHING)) {
    return 0;
  }

  struct branch_state *br_st = mod->state->branch_state;
  assert(br_st->branching == par);
  if (br_st->cond == NULL) {
    return 0;
  }

  init_phi_trackers_for_branching(mod);
  insert_all_possible_conditioned_phi(mod, node);

  return 0;
}

static void mark_conditioned_phi_chain_used(struct node *node) {
  if (node->which == PHI && !node->as.PHI.is_used) {
    node->as.PHI.is_used = true;

    if (node->as.PHI.is_conditioned) {
      struct node *ancestor = vecancestor_get(&node->as.PHI.ancestors, 0)->prev;
      mark_conditioned_phi_chain_used(ancestor);
    }
  }
}

static error track_ident_use(struct module *mod, struct node *node) {
  assert(node->which == IDENT);
  struct node *def = node->as.IDENT.def;

  switch (def->which) {
  case WITHIN:
    // FIXME: WITHIN should probably also have its own phi_tracker and be
    // handled like DEFNAME and DEFARG below.
  case DEFGENARG:
  case SETGENARG:
  case DEFFUN:
  case DEFMETHOD:
  case DEFTYPE:
  case DEFINTF:
  case DEFALIAS:
  case DEFCHOICE:
    node->as.IDENT.prev_use = def;
    assert(node->as.IDENT.prev_use != node);
    return 0;
  case IMPORT:
    return 0;

  case DEFNAME:
    if (parent_const(parent_const(def))->which == MODULE_BODY) {
      return 0;
    }
    break;
  case DEFARG:
    break;

  default:
    assert(false);
    return 0;
  }

  if (node->as.IDENT.non_local_scope == NULL && !local_name_is_scoped(mod, def)) {
    error e = mk_except(mod, node, "identifier '%s' used before its definition",
                        idents_value(mod->gctx, node_ident(node)));
    THROW(e);
  }

  struct phi_tracker_state *phi_st = get_phi_tracker(def);

  node->as.IDENT.prev_use = OR_ELSE(phi_st->last, def);
  assert(node->as.IDENT.prev_use != node);

  if (phi_st->last != NULL && phi_st->last->which == IDENT) {
    phi_st->last->as.IDENT.next_use = node;
  }

  mark_conditioned_phi_chain_used(node->as.IDENT.prev_use);

  phi_st->last = node;
  return 0;
}

static void phi_insertion(struct module *mod, struct node *br_block) {
  struct branch_state *br_st = mod->state->branch_state;
  struct node *branching = parent(br_block);
  assert(NM(branching->which) & STEP_NM_BRANCHING);
  struct node *where_phi = parent(branching);
  struct node *maybe_first_phi = next(branching);

  struct node *cphi = subs_first(br_block);
  while (cphi->which == PHI) {
    assert(cphi->as.PHI.is_conditioned);
    if (!cphi->as.PHI.is_used) {
      struct node *to_remove = cphi;
      cphi = next(cphi);
      node_subs_remove(br_block, to_remove);
      continue;
    }

    struct node *def = cphi->as.PHI.def;

    // Look for a suitable existing PHI. Slow-ish, quadratic-ish.
    struct node *phi = maybe_first_phi;
    while (phi != NULL && phi->which == PHI && node_ident(phi) != node_ident(cphi)) {
      phi = next(phi);
    }

    if (phi == NULL || phi->which != PHI) {
      struct node *nxt = phi;
      phi = mk_node(mod, where_phi, PHI);
      phi->as.PHI.def = cphi->as.PHI.def;
      phi->typ = TBI__NOT_TYPEABLE;

      if (nxt != NULL) {
        node_subs_remove(where_phi, phi);
        node_subs_insert_before(where_phi, nxt, phi);
      }

      struct ancestor *pre_branch = vecancestor_get(&cphi->as.PHI.ancestors, 0);
      vecancestor_push(&phi->as.PHI.ancestors, *pre_branch);
    }

    assert(phi->as.PHI.def == def);
    struct phi_tracker_state *phi_st = get_phi_tracker(def);

    struct ancestor ancestor = {
      .prev = phi_st->last,
      .cond = br_st->cond,
      .reversed = br_st->reversed,
    };
    vecancestor_push(&phi->as.PHI.ancestors, ancestor);

    cphi = next(cphi);
  }
}

static STEP_NM(step_branching_block_up_phi,
               NM(BLOCK));
static error step_branching_block_up_phi(struct module *mod, struct node *node,
                                         void *user, bool *stop) {
  DSTEP(mod, node);

  const struct node *par = parent_const(node);
  if (!(NM(par->which) & STEP_NM_BRANCHING)) {
    return 0;
  }

  struct branch_state *br_st = mod->state->branch_state;
  assert(br_st->branching == par);
  if (br_st->cond == NULL) {
    return 0;
  }

  phi_insertion(mod, node);
  uninit_phi_trackers_for_branching(mod);
  return 0;
}

static STEP_NM(step_branching_up,
               STEP_NM_BRANCHING);
static error step_branching_up(struct module *mod, struct node *node,
                               void *user, bool *stop) {
  DSTEP(mod, node);

  POP_STATE(mod->state->branch_state);
  return 0;
}

static bool is_name_of_globalenv(const struct node *node) {
  const struct node *par = parent_const(node);
  return par->which == DEFNAME && par->as.DEFNAME.is_globalenv;
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

  if (def->which == DEFFIELD) {
    e = mk_except(mod, node,
                  "fields identifiers must be accessed through 'self'");
    THROW(e);
  }

  if (def->which == CATCH) {
    // 'node' is a throw or except label.
    return 0;
  }

  if (def->typ == NULL) {
    e = mk_except(mod, node,
                  "identifier '%s' used before its definition",
                  idents_value(mod->gctx, node_ident(node)));
    THROW(e);
  }

  node->as.IDENT.def = def;
  if (parent_const(def)->which == MODULE_BODY
      || parent_const(def)->which == DEFTYPE
      || parent_const(def)->which == DEFINTF) {
    node->as.IDENT.non_local_scope = &parent(def)->scope;
  } else if (def->flags & NODE_IS_GLOBAL_LET) {
    node->as.IDENT.non_local_scope = &parent(parent(parent(def)))->scope;
  }

  if (typ_is_function(def->typ) && node->typ != TBI__CALL_FUNCTION_SLOT) {
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
  struct node *first = subs_first(node);

  if (node->which == WITHIN) {
    const struct node *modbody = NULL;
    if (first->which == BIN) {
      struct node *ffirst = subs_first(first);
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
                                     node_ident(subs_last_const(node)), false);
    EXCEPT(e);
  } else if (node->which == IDENT) {
    e = scope_lookup(&def, mod, &node->scope, node, false);
    EXCEPT(e);
  } else if (node->which == BIN) {
    e = type_inference_within(mod, first);
    EXCEPT(e);

    e = scope_lookup_ident_immediate(&def, node,
                                     mod, &typ_definition(first->typ)->scope,
                                     node_ident(subs_last_const(node)), false);
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

static error type_inference_try(struct module *mod, struct node *node) {
  struct node *eblock = subs_last(node);

  struct typ *u = NULL;
  FOREACH_SUB(b, eblock) {
    if (u == NULL) {
      u = b->typ;
    } else {
      error e = unify(mod, b, b->typ, u);
      EXCEPT(e);
    }
  }

  set_typ(&node->typ, u);

  return 0;
}

static error type_inference_defchoice_init(struct module *mod,
                                           struct node *node) {
  error e;
  struct node *left = subs_first(node);
  struct node *right = subs_last(node);
  struct node *dleft = typ_definition(left->typ);
  struct node *dright = typ_definition(right->typ);

  assert(left->which == INIT);
  assert(dleft->which == DEFINCOMPLETE);

  assert(right->flags & NODE_IS_DEFCHOICE);
  assert(right->which == BIN);
  const struct node *dleaf = node_get_member_const(mod, dright,
                                                   node_ident(subs_last(right)));
  assert(dleaf->which == DEFCHOICE);
  if (!dleaf->as.DEFCHOICE.is_leaf) {
    e = mk_except_type(mod, subs_last(right),
                       "only union leaf variants can be initialized");
    THROW(e);
  }

  left->as.INIT.for_tag = node_ident(dleaf);

  FOREACH_SUB_EVERY(name, left, 0, 2) {
    const struct node *d = dleaf;
    struct node *field = NULL;
    while (true) {
      e = scope_lookup_ident_immediate(&field, name, mod, &d->scope,
                                       node_ident(name), true);
      if (!e) {
        break;
      }

      if (d->which == DEFTYPE) {
        assert(false && "field names were checked by unify_with_defincomplete()");
      }

      d = parent_const(d);
    }

    typ_link_tentative(field->typ, next(name)->typ);
  }

  return 0;
}

static error type_inference_typeconstraint(struct module *mod, struct node *node) {
  if (node->as.TYPECONSTRAINT.is_constraint) {
    set_typ(&node->typ, subs_first(node)->typ);
    return 0;
  }

  error e;
  if (subs_first(node)->which == INIT
      && subs_last(node)->flags & NODE_IS_DEFCHOICE) {
    e = type_inference_defchoice_init(mod, node);
    EXCEPT(e);
  }

  set_typ(&node->typ, subs_first(node)->typ);
  e = unify(mod, subs_first(node),
            subs_first(node)->typ, subs_last(node)->typ);
  EXCEPT(e);

  node->flags |= subs_first(node)->flags;
  node->flags |= subs_last(node)->flags & NODE__ASSIGN_TRANSITIVE;
  // Copy flags back, as TYPECONSTRAINT are elided in
  // step_remove_typeconstraints().
  subs_first(node)->flags |= node->flags;

  return 0;
}

STEP_NM(step_type_inference,
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

  if (node->typ == NULL
      || node->typ == TBI__MUTABLE
      || node->typ == TBI__MERCURIAL
      || node->typ == TBI__CALL_FUNCTION_SLOT
      || node->which == DEFNAME
      || typ_definition_const(node->typ)->which == MODULE
      || typ_definition_const(node->typ)->which == ROOT_OF_ALL) {
    // noop
  } else {
    return 0;
  }
  //assert(node->typ == NULL
  //       || node->typ == TBI__MUTABLE
  //       || node->typ == TBI__MERCURIAL
  //       || node->typ == TBI__CALL_FUNCTION_SLOT
  //       || node->which == DEFNAME
  //       || typ_definition_const(node->typ)->which == MODULE
  //       || typ_definition_const(node->typ)->which == ROOT_OF_ALL);

  switch (node->which) {
  case NUL:
    set_typ(&node->typ, typ_create_tentative(TBI_LITERALS_NULL));
    break;
  case IDENT:
    e = type_inference_ident(mod, node);
    EXCEPT(e);
    break;
  case DEFNAME:
    PUSH_STATE(node->as.DEFNAME.phi_state);

    set_typ(&node->typ, subs_last(node)->typ);
    node->flags |= subs_last(node)->flags & NODE__TRANSITIVE;

    if (try_remove_unnecessary_ssa_defname(mod, node)) {
      set_typ(&node->typ, TBI_VOID);
      break;
    }

    if (node->flags & NODE_IS_TYPE) {
      e = mk_except(mod, node, "let cannot be used with a type name (use alias)");
      THROW(e);
    }

    if (typ_equal(node->typ, TBI_VOID)) {
      e = mk_except(mod, node, "cannot define a variable of type 'void'");
      THROW(e);
    }
    break;
  case DEFALIAS:
    set_typ(&node->typ, subs_last(node)->typ);
    node->flags |= subs_last(node)->flags & NODE__TRANSITIVE;
    if (!(node->flags & NODE_IS_TYPE)) {
      e = mk_except(mod, node, "alias cannot be used with a value (use let)");
      THROW(e);
    }
    break;
  case PHI:
    node->typ = TBI__NOT_TYPEABLE;
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
  case CALLNAMEDARG:
    set_typ(&node->typ, subs_first(node)->typ);
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
    set_typ(&node->typ, subs_last(node)->typ);
    break;
  case THROW:
    {
      struct node *tryy = module_excepts_get(mod)->tryy;
      struct node *err = subs_at(subs_first(subs_first(tryy)), 1);
      assert(err->which == DEFNAME);
      e = unify(mod, node, subs_last(node)->typ, err->typ);
      EXCEPT(e);
      set_typ(&node->typ, TBI_VOID);
      break;
    }
  case JUMP:
  case BREAK:
  case CONTINUE:
  case NOOP:
    set_typ(&node->typ, TBI_VOID);
    break;
  case IF:
    e = type_inference_if(mod, node);
    EXCEPT(e);
    break;
  case WHILE:
    set_typ(&node->typ, TBI_VOID);
    struct node *cond = subs_first(node);
    e = unify(mod, cond, cond->typ, typ_create_tentative(TBI_GENERALIZED_BOOLEAN));
    EXCEPT(e);
    struct node *block = subs_at(node, 1);
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
    assert(typ_is_reference(subs_first(node)->typ));
    set_typ(&node->typ, node->as.DYN.intf_typ);
    break;
  case TYPECONSTRAINT:
    e = type_inference_typeconstraint(mod, node);
    EXCEPT(e);
    break;
  case DEFARG:
    PUSH_STATE(node->as.DEFARG.phi_state);

    set_typ(&node->typ, subs_at(node, 1)->typ);
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
    set_typ(&node->typ, subs_at(node, 1)->typ);
    node->flags |= NODE_IS_TYPE;
    break;
  case DEFFIELD:
    set_typ(&node->typ, subs_at(node, 1)->typ);
    break;
  case LET:
    set_typ(&node->typ, TBI_VOID);
    break;
  case DELEGATE:
    set_typ(&node->typ, TBI_VOID);
    break;
  case EXAMPLE:
  case PRE:
  case POST:
  case INVARIANT:
    set_typ(&node->typ, TBI_VOID);
    break;
  case WITHIN:
    if (subs_count_atleast(node, 1) && subs_first(node)->which != WITHIN) {
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
    set_typ(&node->typ, subs_first(node)->typ);
    node->flags = subs_first(node)->flags & NODE__TRANSITIVE;
    break;
  case DIRECTDEF:
    set_typ(&node->typ, node->as.DIRECTDEF.typ);
    node->flags = node->as.DIRECTDEF.flags;
    break;
  case DEFCHOICE:
    {
      const struct node *par = node;
      do {
        par = parent_const(par);
      } while (par->which == DEFCHOICE);
      set_typ(&node->typ, par->typ);
    }
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

static STEP_NM(step_ident_non_local_scope,
               NM(IDENT));
static error step_ident_non_local_scope(struct module *mod, struct node *node,
                                        void *user, bool *stop) {
  struct scope *non_local_scope = node->as.IDENT.non_local_scope;
  const struct node *d = typ_definition_const(node->typ);

  if (non_local_scope != NULL
      && scope_node(non_local_scope)->which == DEFINCOMPLETE
      && d->which == DEFTYPE
      && (d->as.DEFTYPE.kind == DEFTYPE_ENUM || d->as.DEFTYPE.kind == DEFTYPE_UNION)) {
    struct node *def = NULL;
    error e = scope_lookup_ident_immediate(&def, node, mod, &d->scope,
                                           node_ident(node), false);
    assert(!e);

    node->as.IDENT.def = def;
    node->as.IDENT.non_local_scope = &parent(def)->scope;

    e = track_ident_use(mod, node);
    assert(!e);
  }
  return 0;
}

static STEP_NM(step_track_ident_use,
               NM(IDENT) | NM(PHI));
static error step_track_ident_use(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  DSTEP(mod, node);

  if (is_name_of_globalenv(node)
      || node_ident(node) == ID_OTHERWISE
      || (node->which == IDENT && typ_equal(node->typ, TBI__NOT_TYPEABLE))) {
    return 0;
  }

  error e;

  if (strcmp(mod->filename, "t03/fixme01.n") == 0)
  if (node_ident(node) == ID_C) __break();

  switch (node->which) {
  case IDENT:
    e = track_ident_use(mod, node);
    EXCEPT(e);
    break;
  case PHI:
    {
      struct node *def = node->as.PHI.def;
      switch (def->which) {
      case DEFNAME:
        def->as.DEFNAME.phi_state->last = node;
        break;
      case DEFARG:
        def->as.DEFARG.phi_state->last = node;
        break;
      default:
        break;
      }
    }
    break;
  default:
    break;
  }

  return 0;
}

// FIXME: should we be removing these before inferring dyn?
STEP_NM(step_remove_typeconstraints,
        NM(TYPECONSTRAINT));
error step_remove_typeconstraints(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  DSTEP(mod, node);

  struct typ *saved = node->typ;
  struct node *sub = subs_first(node);
  node_move_content(node, sub);
  set_typ(&node->typ, saved);

  if (node->which == IDENT) {
    error e = track_ident_use(mod, node);
    EXCEPT(e);
  }

  return 0;
}

static STEP_NM(step_type_drop_excepts,
               NM(TRY));
static error step_type_drop_excepts(struct module *mod, struct node *node,
                                    void *user, bool *stop) {
  DSTEP(mod, node);

  module_excepts_close_try(mod);

  return 0;
}

static error finalize_generic_instantiation(struct module *mod, const struct node *for_error,
                                            struct typ *t) {
  if (typ_definition_const(t) == NULL) {
    // 't' was cleared in link_to_final()
    return 0;
  }

  if (typ_generic_arity(t) == 0) {
    // For instance, a DEFINCOMPLETE that unified to a non-generic.
    return 0;
  }

  if (typ_is_pseudo_builtin(t)) {
    return 0;
  }

  if (typ_is_reference(t) && typ_definition_const(t)->which == DEFINTF) {
    return 0;
  }

  if (typ_is_tentative(t)) {
    // By now, this instance should not be tentative anymore, as all its
    // generic arguments should have been linked to final types.
    for (size_t m = 0; m < typ_generic_arity(t); ++m) {
      struct typ *arg = typ_generic_arg(t, m);
      if (typ_is_weakly_concrete(arg)) {
        return 0;
      }

      if (typ_is_tentative(arg)) {
        fprintf(g_env.stderr, "%s in %s\n",
                typ_pretty_name(mod, arg), typ_pretty_name(mod, t));
        error e = mk_except(mod, for_error, "FIXME: this should be an error,"
                            " but how to explain it?");
        THROW(e);
      }
    }
  }

  struct typ *functor = typ_generic_functor(t);
  const size_t arity = typ_generic_arity(t);

  struct typ *existing = find_existing_final_for_tentative(mod, t);
  if (existing != NULL) {
    typ_link_to_existing_final(existing, t);
    record_topdep(mod, existing);
    return 0;
  }

  struct typ **args = calloc(arity, sizeof(struct typ *));
  for (size_t m = 0; m < arity; ++m) {
    args[m] = typ_generic_arg(t, m);
  }

  const struct node *instantiating_for_error = node_toplevel_const(typ_definition_const(t))
    ->generic->for_error;
  struct node *i = NULL;
  error e = do_instantiate(&i, mod, instantiating_for_error, -1, functor, args, arity, false);
  EXCEPT(e);

  typ_declare_final__privileged(i->typ);
  typ_link_to_existing_final(i->typ, t);
  record_topdep(mod, i->typ);

  free(args);
  return 0;
}

// Typically, unify_with_defincomplete() finishes the job. But in some cases
// it's not enough.
//   fun foo = error
//     let e = {}:(definc {})
//     let a = {x=0}:(definc {x:`integer})
//     e:(definc {x:`integer}) = a -- a.typ is linked to e.typ
//     return e
// The typing of the return unifies 'e.typ' to 'error', which correctly
// updates 'a.typ' to 'error', but unify_with_defincomplete() only sees
// 'error' and 'e.typ', and doesn't know to find the definition 'a.typ' and
// unify the fields of *that* DEFINCOMPLETE (it does know about the
// different DEFINCOMPLETE behind 'e.typ').
//
// So we fix it after the fact.
static void finalize_defincomplete_unification(struct module *mod, struct node *dinc) {
  error e = unify_with_defincomplete_entrails(mod, dinc, dinc->typ,
                                              dinc->typ, dinc);
  assert(!e);
}

// See bootstrap/types.c for some ideas on how to get rid of this step.
STEP_NM(step_gather_final_instantiations,
        NM(DEFTYPE) | NM(DEFINTF) | NM(DEFFUN) | NM(DEFMETHOD));
static error step_gather_final_instantiations(struct module *mod, struct node *node,
                                              void *user, bool *stop) {
  DSTEP(mod, node);

  struct toplevel *toplevel = node_toplevel(mod->state->top_state->top);
  if (vecnode_count(&toplevel->tentative_instantiations) == 0) {
    return 0;
  }

  if (typ_is_generic_functor(node->typ)) {
    return 0;
  }
  const struct node *par = parent_const(node);
  if (par->which != MODULE_BODY
      && typ_is_generic_functor(par->typ)) {
    return 0;
  }

  for (size_t n = 0, count = vecnode_count(&toplevel->tentative_instantiations);
       n < count; ++n) {
    struct node *d = *vecnode_get(&toplevel->tentative_instantiations, n);
    struct typ *t = d->typ;
    if (d->which == DEFINCOMPLETE) {
      finalize_defincomplete_unification(mod, d);
    } else {
      error e = finalize_generic_instantiation(mod, node, t);
      EXCEPT(e);
    }
  }

  if (node->which == DEFINTF) {
    struct node *isal = subs_at(node, IDX_ISALIST);
    FOREACH_SUB(isa, isal) {
      assert(!typ_is_tentative(isa->typ));
    }
  }

  vecnode_destroy(&toplevel->tentative_instantiations);

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
    char *s = typ_pretty_name(mod, node->typ);
    error e = mk_except_type(mod, node,
                             "literal of type '%s' did not unify"
                             " to a concrete type", s);
    free(s);
    THROW(e);
  }

  return 0;
}

static STEP_NM(step_check_no_incomplete_left,
               -1);
static error step_check_no_incomplete_left(struct module *mod, struct node *node,
                                           void *user, bool *stop) {
  if (node->typ == NULL || node_is_at_top(node)) {
    return 0;
  }

  const struct node *d = typ_definition_const(node->typ);
  if (d == NULL
      || d->which != DEFINCOMPLETE
      || d->as.DEFINCOMPLETE.is_isalist_literal) {
    return 0;
  }

  char msg[2048] = { 0 };
  size_t pos = 0, len = ARRAY_SIZE(msg);
  pos += snprintf(msg+pos, len-pos, "incomplete type was never resolved:\n");
  pos += snprint_defincomplete(msg+pos, len-pos, mod, d);
  THROWF(EINVAL, "%s", msg);
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
  struct node *fund = node_get_member(mod, typ_definition(saved_typ), id);
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
  struct typ *tfun = node_get_member(mod, typ_definition(left->typ),
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

  struct typ *tfun = node_get_member(mod, typ_definition(saved_typ), ID_FROM_ARRAY)->typ;
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
      || m->which == DEFALIAS
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
  case DEFALIAS:
    m->as.DEFALIAS.member_isa = mi;
    break;
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
    if (mi->which == NOOP) {
      // noop
    } else if (mi->which == LET) {
      FOREACH_SUB_CONST(d, mi) {
        assert(NM(d->which) & (NM(DEFNAME) | NM(DEFALIAS)));

        ident id = node_ident(d);
        if (id == ID_FINAL || id == ID_THIS) {
          continue;
        }

        e = check_has_matching_member(mod, deft, intf, d);
        EXCEPT(e);
      }
    } else {
      e = check_has_matching_member(mod, deft, intf, mi);
      EXCEPT(e);
    }
  }

  return 0;
}

static STEP_NM(step_check_exhaustive_intf_impl,
               NM(DEFTYPE));
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

error passbody0(struct module *mod, struct node *root,
                void *user, ssize_t shallow_last_up) {
  // first
  PASS(
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_stop_already_morningtypepass);
    DOWN_STEP(step_push_top_state);
    DOWN_STEP(step_push_fun_state);
    DOWN_STEP(step_push_block_state);
    DOWN_STEP(step_record_current_statement);
    DOWN_STEP(step_set_topdep_mask);
    DOWN_STEP(step_detect_not_dyn_intf_down);
    DOWN_STEP(step_rewrite_wildcards);
    DOWN_STEP(step_type_destruct_mark);
    DOWN_STEP(step_type_mutability_mark);
    DOWN_STEP(step_type_gather_retval);
    DOWN_STEP(step_type_gather_excepts);
    ,
    UP_STEP(step_detect_not_dyn_intf_up);
    UP_STEP(step_type_inference);
    UP_STEP(step_remove_typeconstraints);
    UP_STEP(step_type_drop_excepts);
    UP_STEP(step_gather_final_instantiations);
    UP_STEP(step_pop_block_state);
    UP_STEP(step_pop_fun_state);
    UP_STEP(step_pop_top_state);
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
    DOWN_STEP(step_push_block_state);
    DOWN_STEP(step_record_current_statement);
    DOWN_STEP(step_type_gather_retval);
    DOWN_STEP(step_check_no_literals_left);
    DOWN_STEP(step_check_no_incomplete_left);
    DOWN_STEP(step_ident_non_local_scope);
    DOWN_STEP(step_branching_down);
    DOWN_STEP(step_branching_block_down);
    DOWN_STEP(step_branching_block_down_phi);
    ,
    UP_STEP(step_track_ident_use);
    UP_STEP(step_increment_def_name_passed);
    UP_STEP(step_weak_literal_conversion);
    UP_STEP(step_operator_call_inference);
    UP_STEP(step_ctor_call_inference);
    UP_STEP(step_array_ctor_call_inference);
    UP_STEP(step_dtor_call_inference);
    UP_STEP(step_copy_call_inference);
    UP_STEP(step_check_exhaustive_intf_impl);
    UP_STEP(step_dyn_inference);

    UP_STEP(step_store_return_through_ref_expr);

    UP_STEP(step_branching_block_up_phi);
    UP_STEP(step_branching_up);
    UP_STEP(step_pop_block_state);
    UP_STEP(step_pop_fun_state);
    UP_STEP(step_pop_top_state);
    UP_STEP(step_complete_instantiation);
    );
    return 0;
}

static error passbody2(struct module *mod, struct node *root,
                       void *user, ssize_t shallow_last_up) {
  // second
  PASS(
    DOWN_STEP(step_stop_submodules);
    DOWN_STEP(step_stop_marker_tbi);
    DOWN_STEP(step_push_top_state);
    DOWN_STEP(step_push_fun_state);
    DOWN_STEP(step_push_block_state);
    DOWN_STEP(step_type_gather_retval);
    DOWN_STEP(step_branching_down);
    DOWN_STEP(step_branching_block_down);
    ,
    UP_STEP(step_constraint_inference);
    UP_STEP(step_check_exhaustive_match);
    UP_STEP(step_branching_up);
    UP_STEP(step_pop_block_state);
    UP_STEP(step_pop_fun_state);
    UP_STEP(step_pop_top_state);
    UP_STEP(step_complete_instantiation);
    UP_STEP(step_increment_def_name_passed);
    );
    return 0;
}

a_pass passbody[] = { passbody0, passbody1, passbody2 };
