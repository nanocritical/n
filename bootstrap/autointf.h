#include "nodes.h"

const uint64_t step_autointf_enum_union_isalist_filter;
ERROR step_autointf_enum_union_isalist(struct module *mod, struct node *node,
                                       void *user, bool *stop);
const uint64_t step_autointf_enum_union_filter;
ERROR step_autointf_enum_union(struct module *mod, struct node *node,
                               void *user, bool *stop);
const uint64_t step_autointf_detect_default_ctor_dtor_filter;
ERROR step_autointf_detect_default_ctor_dtor(struct module *mod, struct node *node,
                                             void *user, bool *stop);
const uint64_t step_autointf_infer_intfs_filter;
ERROR step_autointf_infer_intfs(struct module *mod, struct node *node,
                                void *user, bool *stop);
const uint64_t step_autointf_isalist_literal_protos_filter;
ERROR step_autointf_isalist_literal_protos(struct module *mod, struct node *node,
                                           void *user, bool *stop);
const uint64_t step_autointf_inherit_filter;
ERROR step_autointf_inherit(struct module *mod, struct node *node,
                            void *user, bool *stop);
const uint64_t step_autointf_add_environment_builtins_filter;
ERROR step_autointf_add_environment_builtins(struct module *mod, struct node *node,
                                             void *user, bool *stop);
