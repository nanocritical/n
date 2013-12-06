#ifndef PASSES_H__
#define PASSES_H__

#include "parser.h"

typedef error (*step)(struct module *mod, struct node *node, void *user, bool *stop);

error pass(struct module *mod, struct node *node,
           const step *down_steps, const step *up_steps, ssize_t shallow_last_up,
           void *user);

enum catchup_for {
  CATCHUP_BELOW_CURRENT = 0,
  CATCHUP_REWRITING_CURRENT,
  CATCHUP_AFTER_CURRENT, // depth-first order in the tree of nodes
  CATCHUP_NEW_INSTANCE,
  CATCHUP_TENTATIVE_NEW_INSTANCE,
};

error catchup(struct module *mod,
              const struct node **except,
              struct node *node,
              struct scope *parent_scope,
              enum catchup_for how);
error catchup_instantiation(struct module *instantiating_mod,
                            struct module *gendef_mod,
                            struct node *instance,
                            struct scope *parent_scope,
                            bool tentative);
bool instantiation_is_tentative(const struct module *mod,
                                struct typ *t, struct typ **args,
                                size_t arity);

enum pass_kind {
  PASS__NONE = 0,
  PASS_ZERO,
  PASS_FORWARD,
  PASS_BODY,
};

// Arbitrary, make it big enough.
#define MAX_PASS 32

struct pass {
  enum pass_kind kind;
  const char *name;
  step downs[MAX_PASS];
  step ups[MAX_PASS];
};

#define PASSZERO_COUNT 1
#define PASSFWD_COUNT 9
#define PASSBODY_COUNT 2

const struct pass *passes(size_t p);
error advance(struct module *mod);

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

error step_stop_marker_tbi(struct module *mod, struct node *node, void *user, bool *stop);
error step_stop_block(struct module *mod, struct node *node, void *user, bool *stop);
error step_stop_funblock(struct module *mod, struct node *node, void *user, bool *stop);
error step_complete_instantiation(struct module *mod, struct node *node, void *user, bool *stop);

#endif
