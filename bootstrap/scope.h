#ifndef SCOPE_H__
#define SCOPE_H__

#include "parser.h"

struct scope *scope_new(struct node *node);
error scope_define_ident(const struct module *mod, struct scope *scope, ident id, struct node *node);
error scope_define(const struct module *mod, struct scope *scope, struct node *id, struct node *node);
error scope_lookup_ident_wontimport(struct node **result, const struct node *for_error,
                                    const struct module *mod,
                                    const struct scope *scope, ident id, bool failure_ok);
error scope_lookup_ident_immediate(struct node **result, const struct node *for_error,
                                   const struct module *mod,
                                   const struct scope *scope, ident id,
                                   bool failure_ok);
error scope_lookup(struct node **result, const struct module *mod,
                   const struct scope *scope, const struct node *id,
                   bool failure_ok);
error scope_lookup_module(struct node **result, const struct module *mod,
                          const struct node *id, bool failure_ok);
error scope_lookup_abspath(struct node **result, const struct node *for_error,
                           const struct module *mod, const char *path);
char *scope_name(const struct module *mod, const struct scope *scope);
char *scope_definitions_name_list(const struct module *mod, const struct scope *scope);

void fix_scopes_after_move(struct node *node);

#endif
