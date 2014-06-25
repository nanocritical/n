#ifndef PASSFWD_H__
#define PASSFWD_H__

#include "passes.h"

a_pass passfwd[PASSFWD_COUNT];

void check_scopes_after_move(struct node *node);

extern const uint64_t step_stop_already_early_typing_filter;
ERROR step_stop_already_early_typing(struct module *mod, struct node *node, void *user, bool *stop);

#endif
