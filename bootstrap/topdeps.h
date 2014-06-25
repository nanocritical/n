#ifndef TOPDEPS_H__
#define TOPDEPS_H__

#include "nodes.h"

struct topdeps;

void topdeps_record(struct module *mod, struct typ *t);

typedef error (*topdeps_each)(struct module *mod, struct node *node,
                              struct typ *t, uint32_t topdep_mask, void *user);
ERROR topdeps_foreach(struct module *mod, struct node *node,
                      topdeps_each each, void *user);

void debug_print_topdeps(const struct module *mod, const struct node *node);

#endif
