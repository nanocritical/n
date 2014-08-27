#include "passes.h"

#include "types.h"
#include "mock.h"
#include "topdeps.h"

#include "passzero.h"
#include "passfwd.h"
#include "passbody.h"
#include "passsem.h"

#include <stdarg.h>

a_pass passes(size_t p) {
  static __thread a_pass v[PASSZERO_COUNT + PASSFWD_COUNT + PASSBODY_COUNT
    + PASSSEMFWD_COUNT + PASSSEMBODY_COUNT + 1]
    = { 0 };

  if (v[0] == NULL) {
    size_t p = 0;
    for (size_t n = 0; n < PASSZERO_COUNT; ++n, ++p) {
      v[p] = passzero[n];
    }
    for (size_t n = 0; n < PASSFWD_COUNT; ++n, ++p) {
      v[p] = passfwd[n];
    }
    for (size_t n = 0; n < PASSBODY_COUNT; ++n, ++p) {
      v[p] = passbody[n];
    }
    for (size_t n = 0; n < PASSSEMFWD_COUNT; ++n, ++p) {
      v[p] = passsemfwd[n];
    }
    for (size_t n = 0; n < PASSSEMBODY_COUNT; ++n, ++p) {
      v[p] = passsembody[n];
    }
  }

  assert(p < ARRAY_SIZE(v));
  return v[p];
};

error advance(struct module *mod) {
  mod->stage->state->passing_in_mod = mod;
  const ssize_t p = mod->stage->state->passing;
  a_pass pa = passes(p);

  mod->state->furthest_passing = max(ssize_t, mod->state->furthest_passing, p);

  int module_depth = 0;
  error e = pa(mod, NULL, &module_depth, -1);
  EXCEPT(e);

  return 0;
}

static ssize_t first_fwd_pass(void) {
  return PASSZERO_COUNT;
}

static ssize_t last_body_pass(void) {
  return PASSZERO_COUNT + PASSFWD_COUNT + PASSBODY_COUNT - 1;
}

static ssize_t last_pass_with_tentative_body(void) {
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
    if (except[n]->excepted > 0) {
      // Can be 0 after a node_move_content().
      ((struct node *) except[n])->excepted -= 1;
    }
  }
}

static struct node *try_find_triggering_top(struct module *mod) {
  struct stage_state *st = mod->stage->state;
  struct module *m = st->passing_in_mod;

  while (st->prev != NULL
          && (m->state->top_state == NULL
              || (m->state->top_state->top->typ != NULL
                  && typ_is_tentative(m->state->top_state->top->typ)))) {
    st = st->prev;
    m = st->passing_in_mod;
  }

  if (m->state->top_state == NULL) {
    return NULL;
  } else {
    return m->state->top_state->top;
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

  struct node *triggering = try_find_triggering_top(mod);

  ssize_t goal;
  if (how == CATCHUP_NEW_INSTANCE || how == CATCHUP_TENTATIVE_NEW_INSTANCE) {
    if (mod->done) {
      if (triggering == NULL) {
        goal = last_body_pass();
      } else {
        goal = mod->stage->state->passing;
      }
    } else {
      if (triggering == NULL) {
        goal = mod->state->furthest_passing;
      } else {
        goal = mod->stage->state->passing;
      }
    }
    goal = min(size_t, last_pass_with_tentative_body(), goal);
  } else if (how == CATCHUP_BEFORE_CURRENT_SAME_TOP) {
    assert(mod->stage->state->passing == 0 && "Unsupported");

    if (mod->state->top_state == NULL) {
      goal = mod->state->furthest_passing;
    } else {
      goal = node_toplevel_const(mod->state->top_state->top)->passing;
    }
  } else if (how == CATCHUP_AFTER_CURRENT) {
    goal = max(ssize_t, mod->stage->state->passing,
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

  const bool was_upward =
    how != CATCHUP_TENTATIVE_NEW_INSTANCE
    && how != CATCHUP_NEW_INSTANCE
    && mod->state->step_state->upward;

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

  for (ssize_t p = 0; p <= goal; ++p) {
    BEGTIMEIT(TIMEIT_PRE_PASSBODY);
    BEGTIMEIT(TIMEIT_PASSBODY);
    BEGTIMEIT(TIMEIT_PASSSEM);

    a_pass pa = passes(p);
    mod->stage->state->passing = p;
    mod->state->furthest_passing = max(ssize_t, mod->state->furthest_passing, p);

    int module_depth = 0;
    error e = pa(mod, node, &module_depth, -1);
    EXCEPT(e);

    if (p == first_fwd_pass()) {
      if (how == CATCHUP_NEW_INSTANCE || how == CATCHUP_TENTATIVE_NEW_INSTANCE) {
        struct typ *functor = typ_generic_functor(node->typ);
        if (functor != NULL) {
          instances_add(functor, node);
        }
      }
      if (how == CATCHUP_TENTATIVE_NEW_INSTANCE) {
        typ_add_tentative_bit__privileged(&node->typ);
      }
    }

    bool other;
    ENDTIMEIT((other = p < PASSZERO_COUNT + PASSFWD_COUNT), TIMEIT_PRE_PASSBODY);
    ENDTIMEIT(!other && (other = p < PASSZERO_COUNT + PASSFWD_COUNT + PASSBODY_COUNT), TIMEIT_PASSBODY);
    ENDTIMEIT(!other, TIMEIT_PASSSEM);
  }

  if (was_upward && how == CATCHUP_REWRITING_CURRENT) {
    // Catch up to, and including, the current step.
    a_pass pa = passes(goal + 1);
    mod->stage->state->passing = goal + 1;
    mod->state->furthest_passing = max(ssize_t, mod->state->furthest_passing, goal + 1);

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

  unmark_excepted(except);
  return 0;
}

error catchup_instantiation(struct module *instantiating_mod,
                            struct module *gendef_mod,
                            struct node *instance,
                            bool tentative) {
  enum catchup_for how = CATCHUP_NEW_INSTANCE;
  if (tentative) {
    // FIXME Deprecate.
    how = CATCHUP_TENTATIVE_NEW_INSTANCE;
  }

  struct generic *generic = node_toplevel(instance)->generic;
  if (generic != NULL) {
    generic->trigger = try_find_triggering_top(instantiating_mod);
    generic->trigger_mod = instantiating_mod;
  }

  error e = catchup(gendef_mod, NULL, instance, how);
  EXCEPT(e);

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

static ERROR do_do_complete_instantiation(struct module *mod, struct node *node,
                                          ssize_t goal) {
  PUSH_STATE(mod->stage->state);
  PUSH_STATE(mod->state->step_state);

  mod->stage->state->passing_in_mod = mod;
  const ssize_t saved_furthest_passing = mod->state->furthest_passing;

  struct toplevel *toplevel = node_toplevel(node);

  for (ssize_t p = toplevel->passed; p <= goal; ++p) {
    mod->stage->state->passing = p;
    if (mod->state->top_state != NULL) {
      mod->state->furthest_passing = mod->stage->state->passing;
    }

    a_pass pa = passes(p);

    int module_depth = 0;
    error e = pa(mod, node, &module_depth, -1);
    EXCEPT(e);
  }

  mod->state->furthest_passing = max(ssize_t, saved_furthest_passing,
                                     mod->state->furthest_passing);

  POP_STATE(mod->state->step_state);
  POP_STATE(mod->stage->state);

  return 0;
}

static bool wont_go_further(struct node *node, ssize_t goal) {
  return goal > last_body_pass()
          && node->which == DEFINCOMPLETE
          && !node->as.DEFINCOMPLETE.is_isalist_literal;
}

static ERROR do_complete_instantiation(struct module *mod, struct node *node) {
  const ssize_t goal = mod->stage->state->passing;

  struct toplevel *toplevel = node_toplevel(node);

  if (node_is_at_top(node)) {
    FOREACH_SUB(s, node) {
      if (NM(s->which) & STEP_NM_HAS_TOPLEVEL) {
        if (wont_go_further(s, goal)) {
          continue;
        }

        const ssize_t p = node_toplevel_const(s)->passed;
        if (p + 1 == toplevel->passed) {
          error e = do_do_complete_instantiation(mod, s, p + 1);
          EXCEPT(e);
        } else {
          assert(p == toplevel->passed);
        }
      }
    }
  }

  error e = do_do_complete_instantiation(mod, node, goal);
  EXCEPT(e);
  return 0;
}

static ERROR advance_topdeps_each(struct module *mod, struct node *node,
                                  struct typ *t, uint32_t topdep_mask, void *user) {
  const ssize_t goal = *(ssize_t *) user;

  if (typ_was_zeroed(t) || typ_is_tentative(t)) {
    return 0;
  }

  struct node *d = typ_definition_nooverlay(t);
  if (wont_go_further(d, goal)) {
    return 0;
  }

  if (!node_is_at_top(d)) {
    error e = advance_topdeps_each(mod, node, parent(d)->typ, 0, user);
    EXCEPT(e);
  }

  if (node_toplevel(d)->passing >= goal) {
    return 0;
  }

  error e = do_complete_instantiation(node_module_owner(d), d);
  if (e) {
    e = mk_except_type(mod, node_toplevel_const(d)->generic->for_error,
                       "while instantiating generic here");
    THROW(e);
  }

  return 0;
}

#if 0
static struct vecnode debug_tops;

unused__
static void debug_print_tops(struct module *mod) {
  for (ssize_t n = vecnode_count(&debug_tops) - 1; n >= 0; --n) {
    const struct node *top = *vecnode_get(&debug_tops, n);
    fprintf(stderr, "%s @%p\n", pptyp(mod, top->typ), top->typ);
  }
}
#endif

STEP_NM(step_push_state,
        STEP_NM_HAS_TOPLEVEL | NM(BLOCK) | NM(DEFFIELD));
error step_push_state(struct module *mod, struct node *node,
                      void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->which == DEFFIELD) {
    mod->state->top_state->exportable = node;
    return 0;
  } else if (node->which == BLOCK) {
    if (NM(parent_const(node)->which) & (NM(DEFFUN) | NM(DEFMETHOD) | NM(EXAMPLE))) {
      mod->state->fun_state->in_block = true;
    }
    return 0;
  }

  ssize_t goal = mod->stage->state->passing;
  struct toplevel *toplevel = node_toplevel(node);

  if (node->which == LET
      && !node_is_at_top(node)
      && !node_is_at_top(parent_const(node))) {
    toplevel->passing = goal;
    return 0;
  }

//  vecnode_push(&debug_tops, node);
  PUSH_STATE(mod->state->top_state);
  mod->state->top_state->top = node;
  mod->state->top_state->exportable = node;

  if (NM(node->which) & (NM(DEFFUN) | NM(DEFMETHOD) | NM(EXAMPLE))) {
    PUSH_STATE(mod->state->fun_state);
  }

  if (wont_go_further(node, goal)) {
    *stop = true;
    return 0;
  }

  if (toplevel->passing >= goal) {
    *stop = true;
    return 0;
  }

  assert(node->which == IMPORT || goal == toplevel->passed + 1);
  toplevel->passing = goal;

  error e = topdeps_foreach(mod, node, advance_topdeps_each, &goal);
  EXCEPT(e);

  return 0;
}

STEP_NM(step_pop_state,
        STEP_NM_HAS_TOPLEVEL | NM(BLOCK) | NM(DEFFIELD));
error step_pop_state(struct module *mod, struct node *node,
                     void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->which == DEFFIELD) {
    mod->state->top_state->exportable = NULL;
    return 0;
  } else if (node->which == BLOCK) {
    if (NM(parent_const(node)->which) & (NM(DEFFUN) | NM(DEFMETHOD) | NM(EXAMPLE))) {
      mod->state->fun_state->in_block = false;
    }
    return 0;
  }

  struct toplevel *toplevel = node_toplevel(node);
  toplevel->passed = toplevel->passing;

  if (node->which == LET
      && !node_is_at_top(node)
      && !node_is_at_top(parent_const(node))) {
    return 0;
  }

  if (NM(node->which) & (NM(DEFFUN) | NM(DEFMETHOD) | NM(EXAMPLE))) {
    POP_STATE(mod->state->fun_state);
  }

  POP_STATE(mod->state->top_state);
//  assert(node == vecnode_pop(&debug_tops));

  return 0;
}

static STEP_NM(step_test_down,
               -1);
static ERROR step_test_down(struct module *mod, struct node *node,
                            void *user, bool *stop) {
  size_t *u = user;
  *u += 1;
  return 0;
}

static STEP_NM(step_test_up,
               -1);
static ERROR step_test_up(struct module *mod, struct node *node,
                          void *user, bool *stop) {
  size_t *u = user;
  *u -= 1;
  return 0;
}

static ERROR passtest(struct module *mod, struct node *root,
                      void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_test_down);
    ,
    UP_STEP(step_test_up);
    ,
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
static ERROR step_test_stop_deftype_down(struct module *mod, struct node *node,
                                         void *user, bool *stop) {
  if (node->which == DEFTYPE) {
    *stop = true;
  }
  return 0;
}

static STEP_NM(step_test_stop_deftype_up,
               NM(DEFTYPE));
static ERROR step_test_stop_deftype_up(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  size_t *u = user;
  *u += 1;
  return 0;
}

static ERROR passtest_stop_deftype(struct module *mod, struct node *root,
                                   void *user, ssize_t shallow_last_up) {
  PASS(
    DOWN_STEP(step_test_stop_deftype_down);
    ,
    UP_STEP(step_test_stop_deftype_up);
    ,
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
