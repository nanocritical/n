#include "instantiate.h"

#include "types.h"
#include "topdeps.h"
#include "unify.h"

#include "passzero.h"

// Does not itself check that 'args' are valid types for instantiation.
// This is done in passfwd.c:validate_genarg_types().
static ERROR do_instantiate(struct typ **result,
                            struct module *mod,
                            const struct node *for_error, ssize_t for_error_offset,
                            struct typ *t,
                            struct typ **args, size_t arity) {
  BEGTIMEIT(TIMEIT_INSTANTIATE_TOTAL);
  BEGTIMEIT(TIMEIT_INSTANTIATE);
  BEGTIMEIT(TIMEIT_INSTANTIATE_INTF);
  BEGTIMEIT(TIMEIT_INSTANTIATE_REF);

  assert(arity == 0 || (typ_is_generic_functor(t) && arity == typ_generic_arity(t)));

  struct node *gendef = typ_definition_ignore_any_overlay(t);
  if (node_toplevel(gendef)->generic->pristine == NULL) {
    if (for_error == NULL) {
      assert(false && "Not supposed to fail when for_error is NULL");
    } else {
      error e = mk_except_type(mod, for_error, "not a generic functor");
      EXCEPT(e);
    }
  }

  struct node *pristine = node_toplevel(gendef)->generic->pristine;
  struct node *instance = create_instance_deepcopy_from_pristine(mod, gendef, pristine);
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
        && for_error_offset+n < subs_count(for_error)
        ? subs_at_const(for_error, for_error_offset+n)
        : for_error;
    }

    ga = next(ga);
  }

  error e = catchup_instantiation(mod, node_module_owner(gendef),
                                  instance, false);
  if (e) {
    char *n = pptyp(mod, t);
    e = mk_except_type(mod, for_error, "while instantiating generic here '%s'", n);
    free(n);
    THROW(e);
  }

  *result = instance->typ;

  ENDTIMEIT(typ_is_reference(instance->typ), TIMEIT_INSTANTIATE_REF);
  ENDTIMEIT(instance->which == DEFINTF, TIMEIT_INSTANTIATE_INTF);
  ENDTIMEIT(true, TIMEIT_INSTANTIATE);
  ENDTIMEIT(true, TIMEIT_INSTANTIATE_TOTAL);

  return 0;
}

static ERROR do_instantiate_tentative(struct typ **result,
                                      struct module *mod,
                                      const struct node *for_error, ssize_t for_error_offset,
                                      struct typ *t,
                                      struct typ **args, size_t arity) {
  BEGTIMEIT(TIMEIT_INSTANTIATE_TENTATIVE);
  BEGTIMEIT(TIMEIT_INSTANTIATE_TENTATIVE_INTF);
  BEGTIMEIT(TIMEIT_INSTANTIATE_TENTATIVE_REF);

  *result = typ_create_tentative(mod, t, args, arity);

  ENDTIMEIT(typ_is_reference(*result), TIMEIT_INSTANTIATE_TENTATIVE_REF);
  ENDTIMEIT(typ_definition_which(*result) == DEFINTF, TIMEIT_INSTANTIATE_TENTATIVE_INTF);
  ENDTIMEIT(true, TIMEIT_INSTANTIATE_TENTATIVE);
  return 0;
}

static bool instantiation_is_genarg_or_tentative(const struct module *mod,
                                                 struct typ *t, struct typ **args,
                                                 size_t arity) {
  if (mod->state->tentatively || mod->state->top_state != NULL) {
    if (arity == 0) {
      return true;
    }

    if (typ_is_genarg(t) || typ_is_tentative(t)) {
      return true;
    }

    for (size_t n = 0; n < arity; ++n) {
      if (args[n] == NULL) {
        return true;
      }
      if (typ_is_genarg(args[n]) || typ_is_tentative(args[n])) {
        return true;
      }
    }

    // Optimisation: immediately turn into a final instantiation.
    return false;
  } else {
    return false;
  }
}

error instantiate(struct typ **result,
                  struct module *mod,
                  const struct node *for_error, ssize_t for_error_offset,
                  struct typ *t, struct typ **args, size_t arity,
                  bool reject_identical) {
  assert(arity == typ_generic_arity(t));

  if (!reject_identical) {
    struct typ *r = instances_find_existing_identical(mod, t, args, arity);
    if (r != NULL) {
      if (result != NULL) {
        *result = r;
      }
      return 0;
    }
  }

  const bool tentative = instantiation_is_genarg_or_tentative(mod, t, args, arity);

  bool genarg = typ_is_genarg(t);
  for (size_t n = 0; n < arity; ++n) {
    genarg |= args[n] == NULL ? false : typ_is_genarg(args[n]);
  }

  if (!tentative && !genarg) {
    struct typ *r = instances_find_existing_final_with(t, args, arity);
    if (r != NULL) {
      if (result != NULL) {
        *result = r;
      }
      return 0;
    }
  }

  if (tentative) {
    error e = do_instantiate_tentative(result, mod, for_error, for_error_offset,
                                       t, args, arity);
    EXCEPT(e);
  } else {
    error e = do_instantiate(result, mod, for_error, for_error_offset,
                             t, args, arity);
    EXCEPT(e);
  }

  return 0;
}

struct typ *tentative_generic_arg(struct module *mod, const struct node *for_error,
                                  struct typ *t, size_t n) {
  struct typ *ga = typ_generic_arg(t, n);
  const size_t arity = typ_generic_arity(ga);

  if (arity == 0) {
    assert(!typ_is_tentative(ga));
    return instantiate_fully_implicit(mod, for_error, ga);
  } else if (typ_is_generic_functor(ga)) {
    return typ_create_tentative_functor(mod, ga);
  }

  // Otherwise, the generic argument is declared as c:(`container t), where
  // t is another generic argument. The typ will be created by the genarg
  // mapping in typ_create_tentative.
  return NULL;
}

struct typ *instantiate_fully_implicit(struct module *mod,
                                       const struct node *for_error,
                                       struct typ *t) {
  if (typ_generic_arity(t) == 0) {
    assert(typ_definition_which(t) == DEFINTF || typ_is_isalist_literal(t));
    return typ_create_tentative(mod, t, NULL, 0);
  }

  assert(typ_is_generic_functor(t) || typ_generic_arity(t) == 0);

  const size_t gen_arity = typ_generic_arity(t);
  struct typ **args = calloc(gen_arity, sizeof(struct typ *));
  for (size_t n = 0; n < gen_arity; ++n) {
    assert(!typ_is_tentative(typ_generic_arg(t, n)));
    args[n] = tentative_generic_arg(mod, for_error, t, n);
    assert(args[n] == NULL || typ_is_tentative(args[n]));
  }

  struct typ *i = NULL;
  error e = instantiate(&i, mod, for_error, -1,
                        t, args, gen_arity, true);
  // FIXME: Temporary workaround: it's actually legal for this code to
  // return an error, because of an error in the N code under consideration.
  // Some refactoring needs to be done so that we can raise the error
  // properly. In the meantime, we print it here when needed.
  if (e && g_env.running_example) {
    examples_destroy("<FIXME: unknown>");
  }
  assert(!e);

  if (gen_arity == 0) {
    assert(typ_is_tentative(i));
  } else {
    assert(typ_is_tentative(typ_generic_functor(i)) == typ_is_tentative(t));
  }
  for (size_t n = 0; n < gen_arity; ++n) {
    assert(typ_is_tentative(typ_generic_arg(i, n))
           && "FIXME: some generics cannot be instiated fully implicitly, e.g. Nonnull_cast");
  }

  free(args);

  return i;
}
