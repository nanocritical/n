#include "passes.h"

#include <stdarg.h>
#include "types.h"
#include "mock.h"
#include "scope.h"

#include "passzero.h"
#include "passfwd.h"
#include "passbody.h"

const struct pass *passes(size_t p) {
  static __thread const struct pass *v[PASSZERO_COUNT + PASSFWD_COUNT + PASSBODY_COUNT + 1];

  if (v[0] == NULL) {
    v[0] = &passzero[0];
    v[PASSZERO_COUNT + 0] = &passfwd[0];
    v[PASSZERO_COUNT + 1] = &passfwd[1];
    v[PASSZERO_COUNT + 2] = &passfwd[2];
    v[PASSZERO_COUNT + 3] = &passfwd[3];
    v[PASSZERO_COUNT + 4] = &passfwd[4];
    v[PASSZERO_COUNT + 5] = &passfwd[5];
    v[PASSZERO_COUNT + 6] = &passfwd[6];
    v[PASSZERO_COUNT + 7] = &passfwd[7];
    v[PASSZERO_COUNT + 8] = &passfwd[8];
    v[PASSZERO_COUNT + PASSFWD_COUNT + 0] = &passbody[0];
    v[PASSZERO_COUNT + PASSFWD_COUNT + 1] = &passbody[1];
    v[PASSZERO_COUNT + PASSFWD_COUNT + PASSBODY_COUNT] = NULL;
  }

  assert(p < ARRAY_SIZE(v));
  return v[p];
};

error pass(struct module *mod, struct node *node,
           const step *down_steps, const step *up_steps, ssize_t shallow_last_up,
           void *user) {
  error e;
  if (node == NULL) {
    node = mod->root;
  }

  if (node->flags & NODE__EXCEPTED) {
    return 0;
  }

  bool stop = FALSE;
  for (size_t s = 0; down_steps[s] != NULL; ++s) {
    if (mod != NULL) {
      mod->state->step_state->upward = FALSE;
      mod->state->step_state->stepping = s;
    }

    bool stop = FALSE;
    e = down_steps[s](mod, node, user, &stop);
    EXCEPT(e);

    if (stop) {
      return 0;
    }
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *sub = node->subs[n];
    e = pass(mod, sub, down_steps, up_steps, -1, user);
    EXCEPT(e);
  }

  for (ssize_t s = 0; up_steps[s] != NULL
                      && (shallow_last_up == -1 || s <= shallow_last_up); ++s) {
    if (mod != NULL) {
      mod->state->step_state->upward = TRUE;
      mod->state->step_state->stepping = s;
    }

    e = up_steps[s](mod, node, user, &stop);
    EXCEPT(e);

    if (stop) {
      return 0;
    }
  }

  return 0;
}

error advance(struct module *mod) {
  const size_t p = mod->stage->state->passing;
  const struct pass *pa = passes(p);

  int module_depth = 0;
  error e = pass(mod, NULL,
                 pa->downs, pa->ups, -1,
                 &module_depth);
  EXCEPT(e);

  return 0;
}

static size_t last_pass(void) {
  static __thread size_t r;
  if (r == 0) {
    while (passes(r) != NULL) {
      r += 1;
    }
  }
  return r - 1;
}

static size_t last_tentative_instance_pass(void) {
  static __thread size_t r;
  if (r == 0) {
    while (passes(r)->kind != PASS_BODY) {
      r += 1;
    }
    r -= 1;
  }
  return r;
}

static void mark_excepted(const struct node **except) {
  if (except == NULL) {
    return;
  }

  for (size_t n = 0; except[n] != NULL; ++n) {
    ((struct node *) except[n])->flags |= NODE__EXCEPTED;
  }
}

static void unmark_excepted(const struct node **except) {
  if (except == NULL) {
    return;
  }

  for (size_t n = 0; except[n] != NULL; ++n) {
    ((struct node *) except[n])->flags &= ~NODE__EXCEPTED;
  }
}

// Rules for generated nodes:
// - No constraints when modifying anything below the current node (in
// node->subs, etc.);
// - Allowed to *rewrite* current node (change its kind, content, etc.);
// - Not allowed to modify the current node's parent->subs (including
// replacing the current node).
error catchup(struct module *mod,
              const struct node **except,
              struct node *node,
              struct scope *parent_scope,
              enum catchup_for how) {
  mark_excepted(except);

  ssize_t from = 0, goal;
  if (how == CATCHUP_NEW_INSTANCE) {
    goal = mod->done ? last_pass() : mod->stage->state->passing;
  } else if (how == CATCHUP_TENTATIVE_NEW_INSTANCE) {
    goal = min(size_t, mod->stage->state->passing, last_tentative_instance_pass());
  } else if (!mod->state->step_state->upward) {
    goal = mod->stage->state->passing - 1;
  } else if (how == CATCHUP_BELOW_CURRENT) {
    goal = mod->stage->state->passing;
  } else if (how == CATCHUP_REWRITING_CURRENT
             || how == CATCHUP_AFTER_CURRENT) {
    goal = mod->stage->state->passing - 1;
  } else {
    assert(FALSE && "Unreached");
  }

  const struct node *parent = parent_scope->node;
  const bool need_new_state =
    parent->which == MODULE_BODY
    || parent->which == DEFTYPE
    || parent->which == DEFINTF;

  PUSH_STATE(mod->stage->state);
  if (need_new_state) {
    PUSH_STATE(mod->state);
  }
  PUSH_STATE(mod->state->step_state);

  if (how == CATCHUP_TENTATIVE_NEW_INSTANCE) {
    mod->state->tentatively = TRUE;
  }

  for (ssize_t p = from; p <= goal; ++p) {
    const struct pass *pa = passes(p);
    mod->stage->state->passing = p;

    int module_depth = 0;
    error e = pass(mod, node,
                   pa->downs, pa->ups, -1,
                   &module_depth);
    EXCEPT(e);

    if (p == 0) {
      node->scope->parent = parent_scope;
    }
  }

  if (mod->state->step_state->upward && how == CATCHUP_REWRITING_CURRENT) {
    // Catch up to, and including, the current step.
    const struct pass *pa = passes(goal + 1);
    mod->stage->state->passing = goal + 1;

    int module_depth = 0;
    error e = pass(mod, node,
                   pa->downs, pa->ups, mod->state->step_state->prev->stepping,
                   &module_depth);
    EXCEPT(e);
  }

  POP_STATE(mod->state->step_state);
  if (need_new_state) {
    POP_STATE(mod->state);
  }
  POP_STATE(mod->stage->state);

  unmark_excepted(except);

  return 0;
}

bool instantiation_is_tentative(const struct module *mod,
                                struct typ *t, struct typ **args,
                                size_t arity) {
  if (mod->state->tentatively || mod->state->fun_state != NULL) {
    if (typ_is_tentative(t)) {
      return TRUE;
    }

    for (size_t n = 0; n < arity; ++n) {
      if (typ_is_tentative(args[n])) {
        return TRUE;
      }
    }

    // Optimisation: immediately turn into a final instantiation.
    return FALSE;
  } else {
    return FALSE;
  }
}

static void record_tentative_instantiation(struct module *mod, struct node *i) {
  struct module_state *st = mod->state;
  st->tentative_instantiations_count += 1;
  st->tentative_instantiations = realloc(
    st->tentative_instantiations,
    st->tentative_instantiations_count * sizeof(*st->tentative_instantiations));
  st->tentative_instantiations[st->tentative_instantiations_count - 1] = i;
}

error catchup_instantiation(struct module *instantiating_mod,
                            struct module *gendef_mod,
                            struct node *instance,
                            struct scope *parent_scope,
                            bool tentative) {
  enum catchup_for how = CATCHUP_NEW_INSTANCE;
  if (tentative) {
    how = CATCHUP_TENTATIVE_NEW_INSTANCE;
    record_tentative_instantiation(instantiating_mod, instance);
  }

  error e = catchup(gendef_mod, NULL, instance, parent_scope, how);
  EXCEPT(e);

  if (instance->which == DEFTYPE) {
    for (size_t n = 0; n < instance->as.DEFTYPE.members_count; ++n) {
      struct node *m = instance->as.DEFTYPE.members[n];
      if (node_toplevel_const(m)->builtingen != BG__NOT) {
        continue;
      }

      error e = catchup(gendef_mod, NULL, m, instance->scope, how);
      EXCEPT(e);
    }
  }

  return 0;
}

error step_stop_marker_tbi(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->which == DEFTYPE) {
    switch (node_ident(node)) {
    case ID_TBI__NOT_TYPEABLE:
    case ID_TBI__CALL_FUNCTION_SLOT:
      *stop = TRUE;
      return 0;
    }
  }

  if (node->which == IMPORT && node->typ != NULL) {
    *stop = TRUE;
    return 0;
  }

  return 0;
}

error step_stop_block(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  switch (node->which) {
  case BLOCK:
    *stop = TRUE;
    return 0;
  default:
    return 0;
  }
}

error step_stop_funblock(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case BLOCK:
    break;
  default:
    return 0;
  }

  switch (node_parent(node)->which) {
  case DEFFUN:
  case DEFMETHOD:
  case EXAMPLE:
    *stop = TRUE;
    break;
  default:
    break;
  }

  return 0;
}

static error do_complete_instantiation(struct module *mod, struct node *node) {
  const size_t goal = mod->stage->state->passing;

  PUSH_STATE(mod->stage->state);
  PUSH_STATE(mod->state->step_state);

  struct toplevel *toplevel = node_toplevel(node);
  for (ssize_t p = toplevel->yet_to_pass; p <= goal; ++p) {
    const struct pass *pa = passes(p);
    mod->stage->state->passing = p;

    int module_depth = 0;
    error e = pass(mod, node, pa->downs, pa->ups, -1, &module_depth);
    EXCEPT(e);

    toplevel->yet_to_pass = p + 1;

    if (node->which == DEFTYPE) {
      for (size_t n = 0; n < node->as.DEFTYPE.members_count; ++n) {
        struct node *m = node->as.DEFTYPE.members[n];
        if (node_toplevel_const(m)->builtingen != BG__NOT) {
          continue;
        }

        error e = pass(mod, m, pa->downs, pa->ups, -1, &module_depth);
        EXCEPT(e);

        node_toplevel(m)->yet_to_pass = p + 1;
      }
    }
  }

  POP_STATE(mod->state->step_state);
  POP_STATE(mod->stage->state);

  return 0;
}

error step_complete_instantiation(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  struct toplevel *toplevel = node_toplevel(node);
  if (toplevel == NULL) {
    return 0;
  }

  if (toplevel->yet_to_pass > mod->stage->state->passing) {
    return 0;
  }

  for (size_t n = 1; n < toplevel->instances_count; ++n) {
    struct node *i = toplevel->instances[n];
    error e = do_complete_instantiation(mod, i);
    EXCEPT(e);
  }

  toplevel->yet_to_pass = mod->stage->state->passing + 1;

  return 0;
}
