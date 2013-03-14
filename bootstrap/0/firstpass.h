#ifndef FIRSTPASS_H__
#define FIRSTPASS_H__

#include "parser.h"

typedef error (*step)(struct module *mod, struct node *node, void *user, bool *stop);

error pass(struct module *mod, struct node *root, const step *steps, const step *up_steps,
           struct node **except, void *user);
error one_level_pass(struct module *mod, struct node *root, const step *down_steps,
                     const step *up_steps, void *user);

error zeropass(struct module *mod, struct node *node, struct node **except);
error forwardpass(struct module *mod, struct node *node, struct node **except);
error earlypass(struct module *mod, struct node *node, struct node **except);
error firstpass(struct module *mod, struct node *node, struct node **except);
error secondpass(struct module *mod, struct node *node, struct node **except);

#endif
