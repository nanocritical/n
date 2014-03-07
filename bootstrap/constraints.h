#ifndef CONSTRAINTS_H__
#define CONSTRAINTS_H__

#include "parser.h"

struct constraint;

void constraint_invariant(const struct constraint *c);
#define INVARIANT_CONSTRAINT(c) INVARIANT(constraint_invariant(c))

int snprint_constraint(char *s, size_t len,
                       const struct module *mod, struct constraint *c);

bool constraint_has_common_root_tag(ident *tag,
                                    struct module *mod, struct node *node);

extern const uint64_t step_constraint_inference_filter;
error step_constraint_inference(struct module *mod, struct node *node,
                                void *user, bool *stop);

extern const uint64_t step_check_exhaustive_match_filter;
error step_check_exhaustive_match(struct module *mod, struct node *node,
                                  void *user, bool *stop);

#endif
