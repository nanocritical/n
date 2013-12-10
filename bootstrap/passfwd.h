#ifndef PASSFWD_H__
#define PASSFWD_H__

#include "passes.h"

a_pass passfwd[PASSFWD_COUNT];

void check_scopes_after_move(struct node *node);

error step_stop_already_morningtypepass(struct module *mod, struct node *node, void *user, bool *stop);

#endif
