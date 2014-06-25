#ifndef PHI_H__
#define PHI_H__

#include "nodes.h"

struct phi_tracker_state *get_phi_tracker(struct node *def);

const uint64_t step_branching_down_filter;
ERROR step_branching_down(struct module *mod, struct node *node,
                          void *user, bool *stop);
const uint64_t step_branching_block_down_filter;
ERROR step_branching_block_down(struct module *mod, struct node *node,
                                void *user, bool *stop);
const uint64_t step_branching_block_down_phi_filter;
ERROR step_branching_block_down_phi(struct module *mod, struct node *node,
                                    void *user, bool *stop);
const uint64_t step_branching_block_up_phi_filter;
ERROR step_branching_block_up_phi(struct module *mod, struct node *node,
                                  void *user, bool *stop);
const uint64_t step_branching_up_filter;
ERROR step_branching_up(struct module *mod, struct node *node,
                        void *user, bool *stop);

const uint64_t step_remove_typeconstraints_filter;
ERROR step_remove_typeconstraints(struct module *mod, struct node *node,
                                  void *user, bool *stop);
const uint64_t step_ident_non_local_scope_filter;
ERROR step_ident_non_local_scope(struct module *mod, struct node *node,
                                 void *user, bool *stop);
const uint64_t step_track_ident_use_filter;
ERROR step_track_ident_use(struct module *mod, struct node *node,
                                  void *user, bool *stop);

#endif
