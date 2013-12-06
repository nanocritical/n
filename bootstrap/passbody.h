#ifndef PASSBODY_H__
#define PASSBODY_H__

#include "passes.h"

const struct pass passbody[PASSBODY_COUNT];

error step_type_inference(struct module *mod, struct node *node, void *user, bool *stop);
error step_type_destruct_mark(struct module *mod, struct node *node, void *user, bool *stop);
error step_gather_final_instantiations(struct module *mod, struct node *node,
                                       void *user, bool *stop);

#endif
