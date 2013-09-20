#ifndef FIRSTPASS_H__
#define FIRSTPASS_H__

#include "parser.h"

typedef error (*step)(struct module *mod, struct node *node, void *user, bool *stop);

error pass(struct module *mod, struct node *root, const step *steps, const step *up_steps,
           void *user);
error one_level_pass(struct module *mod, struct node *root, const step *down_steps,
                     const step *up_steps, void *user);

error zeropass(struct module *mod, struct node *node);

typedef error (*passfun)(struct module *mod, struct node *node);
const passfun *fwd_passes;
const passfun *body_passes;

#endif
