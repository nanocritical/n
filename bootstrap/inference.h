#ifndef INFERENCE_H__
#define INFERENCE_H__

#include "nodes.h"

const uint64_t step_type_destruct_mark_filter;
ERROR step_type_destruct_mark(struct module *mod, struct node *node,
                              void *user, bool *stop);

const uint64_t step_type_mutability_mark_filter;
ERROR step_type_mutability_mark(struct module *mod, struct node *node,
                                void *user, bool *stop);

const uint64_t step_type_gather_retval_filter;
ERROR step_type_gather_retval(struct module *mod, struct node *node,
                              void *user, bool *stop);

const uint64_t step_type_gather_excepts_filter;
ERROR step_type_gather_excepts(struct module *mod, struct node *node,
                               void *user, bool *stop);

const uint64_t step_type_inference_filter;
ERROR step_type_inference(struct module *mod, struct node *node,
                          void *user, bool *stop);

const uint64_t step_type_drop_excepts_filter;
ERROR step_type_drop_excepts(struct module *mod, struct node *node,
                             void *user, bool *stop);

struct node *expr_ref(struct module *mod, struct node *par,
                      enum token_type refop, struct node *node);

ERROR reference(struct typ **result,
                struct module *mod, struct node *for_error,
                enum token_type op, struct typ *typ);

ERROR try_insert_automagic_de(struct module *mod, struct node *node);

void schedule_finalization(struct typ *t);
ERROR process_finalizations(void);

#endif
