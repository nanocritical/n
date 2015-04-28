#ifndef TOPDEPS_H__
#define TOPDEPS_H__

#include "nodes.h"

enum topdep {
  TD_DYN_NEEDS_TYPE = 0x1,
  TD_TYPEBODY_NEEDS_TYPE = 0x2,
  TD_TYPEBODY_NEEDS_TYPEBODY = 0x4 | TD_TYPEBODY_NEEDS_TYPE,
  TD_FUN_NEEDS_TYPE = 0x8,
  TD_FUN_NEEDS_TYPEBODY = 0x10 | TD_FUN_NEEDS_TYPE,
  TD_FUNBODY_NEEDS_TYPE = 0x20,
  TD_FUNBODY_NEEDS_TYPEBODY = 0x40 | TD_FUNBODY_NEEDS_TYPE,
  TD_TYPEBODY_NEEDS_DYN = 0x80,
  TD_TYPEBODY_NEEDS_DYNBODY = 0x100,
  TD_FUNBODY_NEEDS_DYN = 0x200,
  TD_FUNBODY_NEEDS_DYNBODY = 0x400,
  TD_ANY_NEEDS_NODE = 0x800,
};

struct topdeps;

void topdeps_record(struct module *mod, struct typ *t);
void topdeps_record_newtype_actual(struct module *mod, struct typ *t);
void topdeps_record_dyn(struct module *mod, struct typ *t);
void topdeps_record_mkdyn(struct module *mod, struct typ *t);
void topdeps_record_global(struct module *mod, struct node *node);

typedef error (*topdeps_each)(struct module *mod, struct node *node,
                              struct typ *t, uint32_t topdep_mask, void *user);
ERROR topdeps_foreach(struct module *mod, struct node *node,
                      topdeps_each each, void *user);

typedef error (*topdeps_td_each)(struct module *mod, struct node *node,
                                 struct node *d, uint32_t topdep_mask, void *user);
ERROR topdeps_foreach_td(struct module *mod, struct node *node,
                         topdeps_td_each each, void *user);

void debug_print_topdeps(const struct module *mod, const struct node *node);
void debug_print_topdeps_td(const struct module *mod, const struct node *node);

#endif
