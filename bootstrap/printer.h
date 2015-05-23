#ifndef PRINTER_H__
#define PRINTER_H__

#include "common.h"
#include "parser.h"

ERROR printer_scopes(int fd, const struct module *mod, const struct node *root);

ERROR printer_pretty(int fd, const struct module *mod);
error pptree(int fd, const struct module *mod, const struct node *root);
ERROR printer_dot(int fd, const struct module *mod, const struct node *root);
ERROR printer_c(int fd, int linemap_fd, const struct module *mod);
ERROR printer_h(int fd, const struct module *mod);
void print_c_runexamples_name(FILE *out, const struct module *mod);

const char *tokens_string[TOKEN__NUM];

#endif
