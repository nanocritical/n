#ifndef SCOPE_H__
#define SCOPE_H__

#include "common.h"
#include "table.h"

struct node;
struct module;

typedef uint32_t ident;
uint32_t ident_hash(const ident *a);
int ident_cmp(const ident *a, const ident *b);

HTABLE_SPARSE(scope_map, struct node *, ident);

struct scope {
  struct scope_map map;
};

struct node *scope_node(struct scope *sc);
const struct node *scope_node_const(const struct scope *sc);

void scope_init(struct scope *scope);
void scope_undefine_ssa_var(struct scope *scope, ident id);
size_t scope_count(const struct scope *scope);
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
char *scope_name(const struct module *mod, const struct scope *scope);
char *scope_definitions_name_list(const struct module *mod, const struct scope *scope);

typedef error (*scope_each)(struct module *mod, struct node *node, void *user);
error scope_foreach(struct module *mod, struct scope *scope,
                    scope_each each, void *user);

#endif
