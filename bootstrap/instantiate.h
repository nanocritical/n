#ifndef INSTANTIATE_H__
#define INSTANTIATE_H__

#include "nodes.h"

struct typ *tentative_generic_arg(struct module *mod, const struct node *for_error,
                                  struct typ *t, size_t n);

error instantiate(struct node **result,
                  struct module *mod,
                  const struct node *for_error, size_t for_error_offset,
                  struct typ *t, struct typ **args, size_t arity);

struct node *instantiate_fully_implicit(struct module *mod,
                                        const struct node *for_error,
                                        struct typ *t);

struct typ *find_existing_final_for_tentative(struct module *mod,
                                              const struct typ *t);

#endif
