#include "parser.h"

error unify(struct module *mod, const struct node *for_error,
            struct typ *a, struct typ *b);

error unify_refcompat(struct module *mod, const struct node *for_error,
                      struct typ *a, struct typ *b);

error unify_with_defincomplete_entrails(struct module *mod,
                                        const struct node *for_error,
                                        struct typ *a,
                                        struct typ *inc, struct node *dinc);
