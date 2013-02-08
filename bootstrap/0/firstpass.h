#ifndef FIRSTPASS_H__
#define FIRSTPASS_H__

#include "parser.h"

typedef error (*step)(struct module *mod, struct node *node, void *user, bool *stop);

error step_add_scopes(struct module *mod, struct node *node, void *, bool *);

error pass(struct module *mod, struct node *root, const step *steps, const step *up_steps,
           void *user);
error one_level_pass(struct module *mod, struct node *root, const step *down_steps,
                     const step *up_steps, void *user);

error zeropass(struct module *mod, struct node *node);
error firstpass(struct module *mod, struct node *node);
error secondpass(struct module *mod, struct node *node);

#endif
