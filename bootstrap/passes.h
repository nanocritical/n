#ifndef PASSES_H__
#define PASSES_H__

#include "parser.h"

typedef error (*step)(struct module *mod, struct node *node, void *user, bool *stop);

enum catchup_for {
  CATCHUP_BELOW_CURRENT = 0,
  CATCHUP_REWRITING_CURRENT,
  CATCHUP_BEFORE_CURRENT,
  CATCHUP_AFTER_CURRENT, // depth-first order in the tree of nodes
  CATCHUP_NEW_INSTANCE,
  CATCHUP_TENTATIVE_NEW_INSTANCE,
};

error catchup(struct module *mod,
              const struct node **except,
              struct node *node,
              enum catchup_for how);
void record_tentative_instantiation(struct module *mod, struct node *i);
error catchup_instantiation(struct module *instantiating_mod,
                            struct module *gendef_mod,
                            struct node *instance,
                            bool tentative);
bool instantiation_is_tentative(const struct module *mod,
                                struct typ *t, struct typ **args,
                                size_t arity);

typedef error (*a_pass)(struct module *mod, struct node *root,
                        void *user, ssize_t shallow_last_up);

#define PASSZERO_COUNT 2
#define PASSFWD_COUNT 9
#define PASSBODY_COUNT 3

a_pass passes(size_t p);
error advance(struct module *mod);

const uint64_t step_stop_marker_tbi_filter;
error step_stop_marker_tbi(struct module *mod, struct node *node, void *user, bool *stop);
const uint64_t step_stop_block_filter;
error step_stop_block(struct module *mod, struct node *node, void *user, bool *stop);
const uint64_t step_stop_funblock_filter;
error step_stop_funblock(struct module *mod, struct node *node, void *user, bool *stop);
const uint64_t step_push_top_state_filter;
error step_push_top_state(struct module *mod, struct node *node,
                          void *user, bool *stop);
const uint64_t step_pop_top_state_filter;
error step_pop_top_state(struct module *mod, struct node *node,
                         void *user, bool *stop);

const uint64_t step_complete_instantiation_filter;
error step_complete_instantiation(struct module *mod, struct node *node, void *user, bool *stop);


//#define DEBUG_PASS

#ifdef DEBUG_PASS
#define DSTEP(mod, node) do { \
  ident id = node_ident(node); \
  if (id != ID__NONE) { \
    fprintf(g_env.stderr, "%s %s\n", __func__, idents_value(mod->gctx, id)); \
  } else { \
    fprintf(g_env.stderr, "%s\n", __func__); \
  } \
} while (0)
#else
#define DSTEP(mod, node)
#endif

#define PASS(downs, ups) \
  size_t *stackp = &mod->state->stackp; \
  const size_t saved_stackp = *stackp; \
  struct stackel *stack = mod->state->stack + *stackp; \
  struct node *node = root != NULL ? root : mod->root; \
  if (node->excepted > 0) { \
    goto done; \
  } \
  struct node *sub = NULL; \
  \
start: \
  mod->state->step_state->upward = false; \
  mod->state->step_state->stepping = 0; \
  \
  downs; \
  \
  if (node->subs_first == NULL) { \
    goto skip_descend; \
  } \
  sub = node->subs_first; \
  \
descend: \
  if (sub->excepted > 0) { \
    goto next; \
  } \
  \
  *stackp += 1; \
  assert(*stackp < PASS_STACK_DEPTH); \
  stack += 1; \
  stack[0].node = node; \
  stack[0].sub = sub; \
  node = sub; \
  goto start; \
  \
next: \
  sub = sub->next; \
  if (sub != NULL) { \
    goto descend; \
  } \
  \
skip_descend: \
  mod->state->step_state->upward = true; \
  mod->state->step_state->stepping = 0; \
  \
  ups; \
  \
ascend: \
  if (*stackp == saved_stackp) { \
    goto done; \
  } \
  node = stack[0].node; \
  sub = stack[0].sub; \
  *stackp -= 1; \
  stack -= 1; \
  goto next; \
  \
done:

#define DOWN_STEP(step) do { \
  mod->state->step_state->stepping += 1; \
  \
  if (NM(node->which) & step##_filter) { \
    bool stop = false; \
    unused__ static const char *current_step = STRINGIFY(step); \
    error e = step(mod, node, user, &stop); \
    INVARIANT_NODE(node); \
    EXCEPT(e); \
    \
    if (stop) { \
      goto ascend; \
    } \
  } \
  \
} while (0)

#define UP_STEP(step) do { \
  mod->state->step_state->stepping += 1; \
  if (*stackp == saved_stackp \
      && mod->state->step_state->stepping > shallow_last_up) { \
    goto ascend; \
  } \
  \
  if (NM(node->which) & step##_filter) { \
    bool stop = false; \
    unused__ static const char *current_step = STRINGIFY(step); \
    error e = step(mod, node, user, &stop); \
    INVARIANT_NODE(node); \
    EXCEPT(e); \
    \
    if (stop) { \
      goto ascend; \
    } \
  } \
  \
} while (0)

#endif
