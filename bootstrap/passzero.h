#ifndef PASSZERO_H__
#define PASSZERO_H__

#include "passes.h"

a_pass passzero[PASSZERO_COUNT];

extern const uint64_t step_add_scopes_filter;
error step_add_scopes(struct module *mod, struct node *node, void *user, bool *stop);
extern const uint64_t step_stop_submodules_filter;
error step_stop_submodules(struct module *mod, struct node *node, void *user, bool *stop);

struct node *add_instance_deepcopy_from_pristine(struct module *mod,
                                                 struct node *node,
                                                 struct node *pristine,
                                                 bool tentative);

#endif
