#ifndef FIRSTPASS_H__
#define FIRSTPASS_H__

#include "parser.h"

typedef error (*step)(struct module *mod, struct node *node, void *user, bool *stop);

error pass(struct module *mod, struct node *node,
           const step *down_steps, const step *up_steps, ssize_t last_up,
           void *user);

enum pass_kind {
  PASS_ZERO,
  PASS_FORWARD,
  PASS_BODY,
  PASS__NONE,
};

// Arbitrary, make it big enough.
#define MAX_PASS 32

struct pass {
  enum pass_kind kind;
  const char *name;
  step downs[MAX_PASS];
  step ups[MAX_PASS];
};

const struct pass *passes;

error advance(struct module *mod, size_t p);

#endif
