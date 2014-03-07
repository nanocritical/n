#ifndef PASSFWD_H__
#define PASSFWD_H__

#include "passes.h"

a_pass passfwd[PASSFWD_COUNT];

void check_scopes_after_move(struct node *node);

extern const uint64_t step_stop_already_morningtypepass_filter;
error step_stop_already_morningtypepass(struct module *mod, struct node *node, void *user, bool *stop);

error passfwd_ssa(struct module *mod, struct node *root,
                  void *user, ssize_t shallow_last_up);

#endif
