#include "topdeps.h"

#include "types.h"

VECTOR(vectyp, struct typ *, 4);
IMPLEMENT_VECTOR(unused__ static, vectyp, struct typ *);

struct topdeps {
  struct vectyp list;
  struct typset set;

  struct vecnode tentatives;
};

static void record_final(struct module *mod, struct typ *t) {
  struct top_state *st = mod->state->top_state;

  if (typ_is_pseudo_builtin(t)
      || (st->top->typ != NULL && typ_equal(st->top->typ, t))) {
    return;
  }

  struct node *top = st->top;
  struct toplevel *toplevel = node_toplevel(top);

  static const uint32_t TO_KEEP = TOP_IS_EXPORT | TOP_IS_INLINE
    | TOP_IS_FUNCTOR | TOP_IS_PREVENT_DYN;
  static const uint32_t WEAK = TOP_IS_FUNCTOR;

  uint32_t mask = toplevel->flags & TO_KEEP;

  if (!node_is_at_top(top)) {
    top = parent(top);
    toplevel = node_toplevel(top);
    mask |= toplevel->flags & TO_KEEP;
  }

  if (top->typ != NULL && typ_equal(top->typ, t)) {
    return;
  }

  if (toplevel->topdeps == NULL) {
    toplevel->topdeps = calloc(1, sizeof(*toplevel->topdeps));
    typset_fullinit(&toplevel->topdeps->set);
  }

  uint32_t *value = typset_get(&toplevel->topdeps->set, t);
  if (value == NULL) {
    typset_set(&toplevel->topdeps->set, t, mask);
    vectyp_push(&toplevel->topdeps->list, t);
  } else {
    if ((*value & WEAK) && !(mask & WEAK)) {
      *value = (*value & ~WEAK) | mask;
    } else {
      *value |= mask;
    }
  }
}

static void record_tentative(struct module *mod, struct node *node) {
  struct top_state *st = mod->state->top_state;
  struct node *top = st->top;
  if (typ_is_pseudo_builtin(node->typ)
      || top == node) {
    return;
  }
  struct toplevel *toplevel = node_toplevel(top);

  if (toplevel->topdeps == NULL) {
    toplevel->topdeps = calloc(1, sizeof(*toplevel->topdeps));
    typset_fullinit(&toplevel->topdeps->set);
  }

  vecnode_push(&toplevel->topdeps->tentatives, node);
}

void topdeps_record(struct module *mod, struct typ *t) {
  struct top_state *st = mod->state->top_state;
  if (st == NULL) {
    return;
  }

  if (typ_is_tentative(t) || !typ_hash_ready(t)) {
    record_tentative(mod, typ_definition(t));
  } else {
    record_final(mod, t);
  }
}

error topdeps_foreach(struct module *mod, struct node *node,
                      topdeps_each each, void *user) {
  struct toplevel *toplevel = node_toplevel(node);
  if (toplevel->topdeps == NULL) {
    return 0;
  }

  // Need to work when entries are being added to the topdeps while we are
  // iterating over it, so we have to get the current count every time.

  struct vecnode *tentatives = &toplevel->topdeps->tentatives;

  struct top_state *st = mod->state->top_state;
  // 'st' may be NULL when topdeps_foreach() is called from a non-passing
  // context, e.g. cprinter.

  for (size_t n = 0; n < vecnode_count(tentatives); ++n) {
    struct node **p = vecnode_get(tentatives, n);
    if (*p == NULL) {
      continue;
    }

    if (st != NULL) {
      struct typ *t = (*p)->typ;
      if (!typ_is_tentative(t) && typ_hash_ready(t)) {
        record_final(mod, t);
        *p = NULL;
        continue;
      }
    }

    error e = each(mod, node, (*p)->typ, 0, user);
    EXCEPT(e);
  }

  struct vectyp *list = &toplevel->topdeps->list;
  struct typset *set = &toplevel->topdeps->set;

  for (size_t n = 0; n < vectyp_count(list); ++n) {
    struct typ *t = *vectyp_get(list, n);
    const uint32_t mask = *typset_get(set, t);
    error e = each(mod, node, t, mask, user);
    EXCEPT(e);
  }

  return 0;
}

static error print_topdeps_each(struct module *mod, struct node *node,
                                struct typ *t, uint32_t topdep_mask, void *user) {
  fprintf(stderr, "\t%04x %d %zu %s @%p\n",
          topdep_mask,
          typ_is_tentative(t),
          node_toplevel_const(typ_definition_const(t))->passing,
          typ_pretty_name(mod, t), t);
  return 0;
}

void debug_print_topdeps(const struct module *mod, const struct node *node) {
  fprintf(stderr, "%s :%s @%p\n", scope_name(mod, &node->scope),
          typ_pretty_name(mod, node->typ), node->typ);
  topdeps_foreach(CONST_CAST(mod), CONST_CAST(node), print_topdeps_each, NULL);
}
