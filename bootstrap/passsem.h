#ifndef PASSSEM_H__
#define PASSSEM_H__

#include "passes.h"

a_pass passsemfwd[PASSSEMFWD_COUNT];
a_pass passsembody[PASSSEMBODY_COUNT];

extern const uint64_t step_stop_already_early_constraining_filter;
error step_stop_already_early_constraining(struct module *mod, struct node *node, void *user, bool *stop);

#endif
