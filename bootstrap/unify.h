#ifndef UNIFY_H__
#define UNIFY_H__

#include "nodes.h"

error unify(struct module *mod, const struct node *for_error,
            struct typ *a, struct typ *b);

error unify_refcompat(struct module *mod, const struct node *for_error,
                      struct typ *a, struct typ *b);

error unify_with_defincomplete_entrails(struct module *mod,
                                        const struct node *for_error,
                                        struct typ *a,
                                        struct typ *inc, struct node *dinc);

void unify_with_new_parent(struct module *mod, const struct node *for_error,
                           struct typ *p, struct typ *t);

struct typ *unify_with_new_functor(struct module *mod, const struct node *for_error,
                                   struct typ *f, struct typ *t);

#endif
