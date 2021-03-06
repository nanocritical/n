#ifndef PASSBODY_H__
#define PASSBODY_H__

#include "passes.h"

a_pass passbody[PASSBODY_COUNT];

extern const uint64_t step_type_inference_filter;
ERROR step_type_inference(struct module *mod, struct node *node, void *user, bool *stop);
extern const uint64_t step_type_destruct_mark_filter;
ERROR step_type_destruct_mark(struct module *mod, struct node *node, void *user, bool *stop);

extern const uint64_t step_remove_typeconstraints_filter;
ERROR step_remove_typeconstraints(struct module *mod, struct node *node,
                                  void *user, bool *stop);

ERROR passbody0(struct module *mod, struct node *root,
                void *user, ssize_t shallow_last_up);

struct phi_tracker_state *get_phi_tracker(struct node *def);

ERROR insert_conv(struct node **src,
                  struct module *mod, struct node *node,
                  struct typ *target);
#endif
