#ifndef PRINTER_H__
#define PRINTER_H__

#include "common.h"
#include "parser.h"

error printer_scopes(int fd, const struct module *mod, const struct node *root);

error printer_pretty(int fd, const struct module *mod);
error printer_tree(int fd, const struct module *mod, const struct node *root);
error printer_c(int fd, const struct module *mod);
error printer_h(int fd, const struct module *mod);
void print_c_runexamples_name(FILE *out, const struct module *mod);

const char *tokens_string[TOKEN__NUM];

#endif
