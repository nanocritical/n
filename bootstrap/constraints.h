#ifndef CONSTRAINTS_H__
#define CONSTRAINTS_H__

#include "nodes.h"

struct constraint;
struct assumptions;

void constraint_invariant(const struct constraint *c);
#define INVARIANT_CONSTRAINT(c) INVARIANT(constraint_invariant(c))

int snprint_constraint(char *s, size_t len,
                       const struct module *mod, const struct constraint *c);

extern const uint64_t step_constraint_inference_filter;
ERROR step_constraint_inference(struct module *mod, struct node *node,
                                void *user, bool *stop);

extern const uint64_t step_check_exhaustive_match_filter;
ERROR step_check_exhaustive_match(struct module *mod, struct node *node,
                                  void *user, bool *stop);

extern const uint64_t step_stop_generic_functor_filter;
ERROR step_stop_generic_functor(struct module *mod, struct node *node,
                                void *user, bool *stop);

#endif
