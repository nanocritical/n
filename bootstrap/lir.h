#ifndef LIR_H__
#define LIR_H__

#include "parser.h"

extern const uint64_t step_add_sequence_points_filter;
error step_add_sequence_points(struct module *mod, struct node *node,
                               void *user, bool *stop);
extern const uint64_t step_lir_conversion_down_filter;
error step_lir_conversion_down(struct module *mod, struct node *node,
                               void *user, bool *stop);
extern const uint64_t step_lir_conversion_up_filter;
error step_lir_conversion_up(struct module *mod, struct node *node,
                             void *user, bool *stop);

#endif
