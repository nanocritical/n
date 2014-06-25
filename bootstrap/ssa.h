#ifndef SSA_H__
#define SSA_H__

#include "nodes.h"

extern const uint64_t step_ssa_convert_shallow_catchup_filter;
ERROR step_ssa_convert_shallow_catchup(struct module *mod, struct node *node,
                                       void *user, bool *stop);

extern const uint64_t step_ssa_convert_filter;
ERROR step_ssa_convert(struct module *mod, struct node *node,
                       void *user, bool *stop);

extern const uint64_t step_insert_nullable_void_filter;
ERROR step_insert_nullable_void(struct module *mod, struct node *node,
                                void *user, bool *stop);

bool try_remove_unnecessary_ssa_defname(struct module *mod, struct node *defn);

#endif
