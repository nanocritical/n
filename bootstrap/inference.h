#ifndef INFERENCE_H__
#define INFERENCE_H__

#include "nodes.h"

const uint64_t step_rewrite_wildcards_filter;
error step_rewrite_wildcards(struct module *mod, struct node *node,
                             void *user, bool *stop);

const uint64_t step_type_destruct_mark_filter;
error step_type_destruct_mark(struct module *mod, struct node *node,
                              void *user, bool *stop);

const uint64_t step_type_mutability_mark_filter;
error step_type_mutability_mark(struct module *mod, struct node *node,
                                void *user, bool *stop);

const uint64_t step_type_gather_retval_filter;
error step_type_gather_retval(struct module *mod, struct node *node,
                              void *user, bool *stop);

const uint64_t step_type_gather_excepts_filter;
error step_type_gather_excepts(struct module *mod, struct node *node,
                               void *user, bool *stop);

const uint64_t step_type_inference_filter;
error step_type_inference(struct module *mod, struct node *node,
                          void *user, bool *stop);

const uint64_t step_type_drop_excepts_filter;
error step_type_drop_excepts(struct module *mod, struct node *node,
                             void *user, bool *stop);

const uint64_t step_gather_final_instantiations_filter;
error step_gather_final_instantiations(struct module *mod, struct node *node,
                                       void *user, bool *stop);

struct node *expr_ref(struct module *mod, struct node *par,
                      enum token_type refop, struct node *node);

#endif
