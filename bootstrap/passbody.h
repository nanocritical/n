#ifndef PASSBODY_H__
#define PASSBODY_H__

#include "passes.h"

a_pass passbody[PASSBODY_COUNT];

const uint64_t step_type_inference_filter;
error step_type_inference(struct module *mod, struct node *node, void *user, bool *stop);
const uint64_t step_type_destruct_mark_filter;
error step_type_destruct_mark(struct module *mod, struct node *node, void *user, bool *stop);

#endif
