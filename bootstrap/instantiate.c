#include "instantiate.h"

#include "types.h"
#include "unify.h"

#include "passzero.h"

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
  const struct node *d = typ_definition_const(t);
  const struct toplevel *toplevel = node_toplevel_const(d);
  for (size_t n = 1, count = vecnode_count(&toplevel->generic->instances); n < count; ++n) {
    struct typ *i = (*vecnode_get(&toplevel->generic->instances, n))->typ;
    if (typ_is_tentative(i)) {
      continue;
    }

    size_t a;
    for (a = 0; a < arity; ++a) {
      if (!typ_equal(typ_generic_arg_const(i, a), args[a])) {
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
struct typ *find_existing_final_for_tentative(struct module *mod,
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
  const struct node *d = typ_definition_const(t);
  const struct toplevel *toplevel = node_toplevel_const(d);
  for (size_t n = 1, count = vecnode_count(&toplevel->generic->instances); n < count; ++n) {
    struct typ *i = (*vecnode_get(&toplevel->generic->instances, n))->typ;

    // Use pointer comparison: we're looking for the exact same arguments
    // (same tentative bit, etc.).

    if (typ_generic_functor_const(i) != t) {
      break;
    }

    size_t a;
    for (a = 0; a < arity; ++a) {
      if (typ_generic_arg_const(i, a) != args[a]) {
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

struct typ *tentative_generic_arg(struct typ *t, size_t n) {
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

static bool instantiation_is_tentative(const struct module *mod,
                                       struct typ *t, struct typ **args,
                                       size_t arity) {
  if (mod->state->tentatively || mod->state->top_state != NULL) {
    if (typ_is_tentative(t)) {
      return true;
    }

    for (size_t n = 0; n < arity; ++n) {
      if (args[n] == NULL) {
        continue;
      }
      if (typ_is_tentative(args[n])) {
        return true;
      }
    }

    // Optimisation: immediately turn into a final instantiation.
    return false;
  } else {
    return false;
  }
}

error instantiate(struct node **result,
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


error instantiate_no_reuse(struct node **result,
                           struct module *mod,
                           const struct node *for_error, size_t for_error_offset,
                           struct typ *t, struct typ **args, size_t arity) {

  assert(arity == typ_generic_arity(t));

  const bool already_tentative_args = are_all_tentative(args, arity);
  const bool tentative = instantiation_is_tentative(mod, t, args, arity);
  assert(!already_tentative_args && !tentative);

  error e = do_instantiate(result, mod, for_error, for_error_offset,
                           t, args, arity, false);
  EXCEPT(e);
  return 0;
}

struct node *instantiate_fully_implicit(struct module *mod,
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
  error e = instantiate(&i, mod, for_error, -1,
                        t, args, gen_arity);
  assert(!e);
  free(args);

  return i;
}
