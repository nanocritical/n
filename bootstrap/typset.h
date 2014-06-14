#ifndef TYPSET_H__
#define TYPSET_H__

#include "types.h"

// Set that can handle the insertion of tentative typs that are later linked
// to other typs.
// This is similar to how topdeps works. In principle there could be sharing
// of code, but topdeps is actually a map and stores a bitmask with
// particular updates rules of the weak bits. It may be worth sharing the
// rest of the logic.

VECTOR(vectyp, struct typ *, 4);
DECLARE_VECTOR(vectyp, struct typ *);

struct typset {
  bool ready;

  struct vectyp list;
  struct fintypset set;

  struct vecnode tentatives;
};

void typset_init(struct typset *set);
bool typset_has(const struct typset *set, const struct typ *t);
void typset_add(struct typset *set, struct typ *t);

typedef error (*typset_each)(struct module *mod, struct typ *t,
                             bool *stop, void *user);
// Ordering guarantees:
// - tentative types are first;
// - final types are always in the same order, with respect to other final
// types in the typset.
error typset_foreach(struct module *mod, struct typset *set,
                     typset_each each, void *user);

void debug_print_typset(const struct module *mod, const struct typset *set);

#endif
