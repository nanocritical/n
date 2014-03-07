#ifndef SSA_H__
#define SSA_H__

#include "parser.h"

extern const uint64_t step_ssa_convert_filter;
error step_ssa_convert(struct module *mod, struct node *node,
                       void *user, bool *stop);

#endif
