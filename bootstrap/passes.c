#include "passes.h"

#include <stdarg.h>
#include "types.h"
#include "mock.h"

#include "passzero.h"
#include "passfwd.h"
#include "passbody.h"

a_pass passes(size_t p) {
  static __thread a_pass v[PASSZERO_COUNT + PASSFWD_COUNT + PASSBODY_COUNT + 1]
    = { 0 };

  if (v[0] == NULL) {
    for (size_t n = 0; n < PASSZERO_COUNT; ++n) {
      v[n] = passzero[n];
    }
    for (size_t n = 0; n < PASSFWD_COUNT; ++n) {
      v[PASSZERO_COUNT + n] = passfwd[n];
    }
    for (size_t n = 0; n < PASSBODY_COUNT; ++n) {
      v[PASSZERO_COUNT + PASSFWD_COUNT + n] = passbody[n];
    }
  }

  assert(p < ARRAY_SIZE(v));
  return v[p];
};

error advance(struct module *mod) {
  mod->stage->state->passing_in_mod = mod;
  const size_t p = mod->stage->state->passing;
  a_pass pa = passes(p);

  mod->state->furthest_passing = max(size_t, mod->state->furthest_passing, p);

  int module_depth = 0;
  error e = pa(mod, NULL, &module_depth, -1);
  EXCEPT(e);

  return 0;
}

static size_t last_pass(void) {
  return PASSZERO_COUNT + PASSFWD_COUNT + PASSBODY_COUNT - 1;
}

static size_t last_tentative_instance_pass(void) {
  return PASSZERO_COUNT + PASSFWD_COUNT - 1;
}

static void mark_excepted(const struct node **except) {
  if (except == NULL) {
    return;
  }

  for (size_t n = 0; except[n] != NULL; ++n) {
    ((struct node *) except[n])->excepted += 1;
  }
}

static void unmark_excepted(const struct node **except) {
  if (except == NULL) {
    return;
  }

  for (size_t n = 0; except[n] != NULL; ++n) {
    ((struct node *) except[n])->excepted -= 1;
  }
}

// Rules for generated nodes:
// - No constraints when modifying anything below the current node (in
// node->subs, etc.);
// - Allowed to *rewrite* current node (change its kind, content, etc.);
// - Not allowed to modify the current node's par->subs (including
// replacing the current node).
error catchup(struct module *mod,
              const struct node **except,
              struct node *node,
              enum catchup_for how) {
  mark_excepted(except);

  ssize_t from = 0, goal;
  if (how == CATCHUP_NEW_INSTANCE) {
    goal = mod->done ? last_pass() : mod->stage->state->passing;
  } else if (how == CATCHUP_TENTATIVE_NEW_INSTANCE) {
    goal = min(size_t, mod->stage->state->passing, last_tentative_instance_pass());
  } else if (how == CATCHUP_BEFORE_CURRENT) {
    assert(mod->stage->state->passing == 0 && "Unsupported");
    goal = mod->state->furthest_passing;
  } else if (how == CATCHUP_AFTER_CURRENT) {
    goal = max(size_t, mod->stage->state->passing,
               mod->state->furthest_passing) - 1;
  } else if (!mod->state->step_state->upward) {
    goal = mod->stage->state->passing - 1;
  } else if (how == CATCHUP_BELOW_CURRENT) {
    goal = mod->stage->state->passing;
  } else if (how == CATCHUP_REWRITING_CURRENT) {
    goal = mod->stage->state->passing - 1;
  } else {
    assert(false && "Unreached");
  }

  const bool was_upward = mod->state->step_state->upward;
  struct node *saved_current_statement = NULL;
  if (mod->state->fun_state != NULL
      && mod->state->fun_state->block_state != NULL) {
    saved_current_statement = mod->state->fun_state->block_state->current_statement;
  }

  const struct node *par = parent(node);
  const bool need_new_state =
    par->which == MODULE_BODY
    || par->which == DEFTYPE
    || par->which == DEFINTF;

  PUSH_STATE(mod->stage->state);
  mod->stage->state->passing_in_mod = mod;

  if (need_new_state) {
    PUSH_STATE(mod->state);
  }
  PUSH_STATE(mod->state->step_state);

  bool tentatively_saved = mod->state->tentatively;
  if (how == CATCHUP_TENTATIVE_NEW_INSTANCE) {
    mod->state->tentatively = true;
  }

  for (ssize_t p = from; p <= goal; ++p) {
    a_pass pa = passes(p);
    mod->stage->state->passing = p;
    mod->state->furthest_passing = max(size_t, mod->state->furthest_passing, p);

    int module_depth = 0;
    error e = pa(mod, node, &module_depth, -1);
    EXCEPT(e);
  }

  if (was_upward && how == CATCHUP_REWRITING_CURRENT) {
    // Catch up to, and including, the current step.
    a_pass pa = passes(goal + 1);
    mod->stage->state->passing = goal + 1;
    mod->state->furthest_passing = max(size_t, mod->state->furthest_passing, goal + 1);

    int module_depth = 0;
    error e = pa(mod, node, &module_depth, mod->state->step_state->prev->stepping);
    EXCEPT(e);
  }

  mod->state->tentatively = tentatively_saved;
  POP_STATE(mod->state->step_state);
  if (need_new_state) {
    POP_STATE(mod->state);
  }
  POP_STATE(mod->stage->state);

  if (saved_current_statement != NULL) {
    mod->state->fun_state->block_state->current_statement = saved_current_statement;
  }

  unmark_excepted(except);
  return 0;
}

bool instantiation_is_tentative(const struct module *mod,
                                struct typ *t, struct typ **args,
                                size_t arity) {
  if (mod->state->tentatively || mod->state->top_state != NULL) {
    if (typ_is_tentative(t)) {
      return true;
    }

    for (size_t n = 0; n < arity; ++n) {
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

void record_tentative_instantiation(struct module *mod, struct node *i) {
  struct module *m = mod;
  while (m->state->top_state == NULL) {
    m = m->stage->state->prev->passing_in_mod;
  }

  struct toplevel *toplevel = node_toplevel(m->state->top_state->top);
  vecnode_push(&toplevel->tentative_instantiations, i);
}

error catchup_instantiation(struct module *instantiating_mod,
                            struct module *gendef_mod,
                            struct node *instance,
                            bool tentative) {
  enum catchup_for how = CATCHUP_NEW_INSTANCE;
  if (tentative) {
    how = CATCHUP_TENTATIVE_NEW_INSTANCE;
    record_tentative_instantiation(instantiating_mod, instance);
  }

  error e = catchup(gendef_mod, NULL, instance, how);
  EXCEPT(e);

  if (tentative) {
    typ_add_tentative_bit__privileged(&instance->typ);
  }

  return 0;
}

STEP_NM(step_stop_marker_tbi,
        NM(DEFTYPE) | NM(IMPORT));
error step_stop_marker_tbi(struct module *mod, struct node *node,
                           void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->which == DEFTYPE) {
    switch (node_ident(node)) {
    case ID_TBI__NOT_TYPEABLE:
    case ID_TBI__CALL_FUNCTION_SLOT:
      *stop = true;
      return 0;
    }
  } else if (node->which == IMPORT && node->typ != NULL) {
    *stop = true;
    return 0;
  } else {
    assert(false && "Unreached");
  }

  return 0;
}

STEP_NM(step_stop_block,
        NM(BLOCK));
error step_stop_block(struct module *mod, struct node *node,
                      void *user, bool *stop) {
  DSTEP(mod, node);

  *stop = true;

  return 0;
}

STEP_NM(step_stop_funblock,
        NM(BLOCK));
error step_stop_funblock(struct module *mod, struct node *node,
                         void *user, bool *stop) {
  DSTEP(mod, node);

  switch (parent(node)->which) {
  case DEFFUN:
  case DEFMETHOD:
  case EXAMPLE:
    *stop = true;
    break;
  default:
    break;
  }

  return 0;
}

STEP_NM(step_push_top_state, STEP_NM_HAS_TOPLEVEL);
error step_push_top_state(struct module *mod, struct node *node,
                          void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->which == LET
      && !(node_is_at_top(node) || node_is_at_top(parent_const(node)))) {
    return 0;
  }

  PUSH_STATE(mod->state->top_state);
  mod->state->top_state->top = node;

  const struct toplevel *toplevel = node_toplevel_const(node);
  mod->state->top_state->topdep_mask
    = toplevel->flags & (TOP_IS_EXPORT | TOP_IS_INLINE);

  return 0;
}

STEP_NM(step_pop_top_state, STEP_NM_HAS_TOPLEVEL);
error step_pop_top_state(struct module *mod, struct node *node,
                         void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->which == LET
      && !(node_is_at_top(node) || node_is_at_top(parent_const(node)))) {
    return 0;
  }

  POP_STATE(mod->state->top_state);
  return 0;
}

static error do_complete_instantiation(struct module *mod, struct node *node) {
  const size_t goal = mod->stage->state->passing;

  PUSH_STATE(mod->stage->state);
  PUSH_STATE(mod->state->step_state);

  const size_t saved_furthest_passing = mod->state->furthest_passing;

  struct toplevel *toplevel = node_toplevel(node);
  for (ssize_t p = toplevel->yet_to_pass; p <= goal; ++p) {
    a_pass pa = passes(p);
    mod->stage->state->passing = p;
    if (mod->state->top_state != NULL) {
      mod->state->furthest_passing = mod->stage->state->passing;
    }

    int module_depth = 0;
    error e = pa(mod, node, &module_depth, -1);
    EXCEPT(e);

    toplevel->yet_to_pass = p + 1;
  }

  mod->state->furthest_passing = max(size_t, saved_furthest_passing,
                                     mod->state->furthest_passing);

  POP_STATE(mod->state->step_state);
  POP_STATE(mod->stage->state);

  return 0;
}

STEP_NM(step_complete_instantiation,
        STEP_NM_HAS_TOPLEVEL);
error step_complete_instantiation(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  DSTEP(mod, node);

  struct toplevel *toplevel = node_toplevel(node);
  if (toplevel->yet_to_pass > mod->stage->state->passing) {
    return 0;
  }

  if (toplevel->generic == NULL) {
    return 0;
  }

  for (size_t n = 1, count = vecnode_count(&toplevel->generic->instances); n < count; ++n) {
    struct node *i = *vecnode_get(&toplevel->generic->instances, n);
    if (typ_is_tentative(i->typ)
        // Was linked to something else.
        || typ_definition_const(i->typ) != i) {
      continue;
    }

    error e = do_complete_instantiation(mod, i);
    if (e) {
      e = mk_except_type(mod, node_toplevel_const(i)->generic->for_error,
                         "while instantiating generic here");
      THROW(e);
    }
  }

  toplevel->yet_to_pass = mod->stage->state->passing + 1;

  return 0;
}

static STEP_NM(step_test_down,
               -1);
static error step_test_down(struct module *mod, struct node *node,
                            void *user, bool *stop) {
  size_t *u = user;
  *u += 1;
  return 0;
}

static STEP_NM(step_test_up,
               -1);
static error step_test_up(struct module *mod, struct node *node,
                          void *user, bool *stop) {
  size_t *u = user;
  *u -= 1;
  return 0;
}

static error passtest(struct module *mod, struct node *root,
                      void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_test_down);
    ,
    UP_STEP(step_test_up);
    );
  return 0;
}

EXAMPLE_NCC_EMPTY(as_many_up_down) {
  (void) mock_deftype(mod, "test");

  size_t u = 0;
  error e = passtest(mod, NULL, &u, -1);
  assert(!e);
  assert(u == 0);

  u = 0;
  e = passtest(mod, NULL, &u, -1);
  assert(!e);
  assert(u == 0);
}

static STEP_NM(step_test_stop_deftype_down,
               NM(DEFTYPE));
static error step_test_stop_deftype_down(struct module *mod, struct node *node,
                                         void *user, bool *stop) {
  if (node->which == DEFTYPE) {
    *stop = true;
  }
  return 0;
}

static STEP_NM(step_test_stop_deftype_up,
               NM(DEFTYPE));
static error step_test_stop_deftype_up(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  size_t *u = user;
  *u += 1;
  return 0;
}

static error passtest_stop_deftype(struct module *mod, struct node *root,
                                   void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_test_stop_deftype_down);
    ,
    UP_STEP(step_test_stop_deftype_up);
    );
  return 0;
}

EXAMPLE_NCC_EMPTY(down_stop) {
  (void) mock_deftype(mod, "test");

  size_t u = 0;
  error e = passtest_stop_deftype(mod, NULL, &u, -1);
  assert(!e);
  assert(u == 0);
}
