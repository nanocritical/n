#ifndef PASSFWD_H__
#define PASSFWD_H__

#include "passes.h"

const struct pass passfwd[PASSFWD_COUNT];

void fix_scopes_after_move(struct node *node);

error step_stop_already_morningtypepass(struct module *mod, struct node *node, void *user, bool *stop);

#endif