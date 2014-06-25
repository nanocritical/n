#include "typset.h"

void typset_init(struct typset *set) {
  fintypset_fullinit(&set->set);
}

static void add_final(struct typset *set, struct typ *t) {
  uint32_t *value = fintypset_get(&set->set, t);
  if (value == NULL) {
    fintypset_set(&set->set, t, 1);
    vectyp_push(&set->list, t);
  }
}

static void add_tentative(struct typset *set, struct node *def) {
  vecnode_push(&set->tentatives, def);
}

void typset_add(struct typset *set, struct typ *t) {
  if (typ_is_tentative(t) || !typ_hash_ready(t)) {
    add_tentative(set, typ_definition(t));
  } else {
    add_final(set, t);
  }
}

bool typset_has(const struct typset *_set, const struct typ *_t) {
  struct typset *set = CONST_CAST(_set);
  struct typ *t = CONST_CAST(_t);

  struct vecnode *tentatives = &set->tentatives;
  for (ssize_t n = 0; n < vecnode_count(tentatives); ++n) {
    struct node **p = vecnode_get(tentatives, n);

    struct typ *s = (*p)->typ;
    if (!typ_is_tentative(s) && typ_hash_ready(s)) {
      add_final(set, s);
      n += vecnode_remove_replace_with_last(tentatives, n);
    }

    if (typ_equal(t, s)) {
      return true;
    }
  }

  const uint32_t *existing = fintypset_get(&set->set, t);
  return existing != NULL && *existing;
}

error typset_foreach(struct module *mod, struct typset *set,
                     typset_each each, void *user) {
  struct vecnode *tentatives = &set->tentatives;
  for (ssize_t n = 0; n < vecnode_count(tentatives); ++n) {
    struct node **p = vecnode_get(tentatives, n);
    struct typ *s = (*p)->typ;
    if (!typ_is_tentative(s) && typ_hash_ready(s)) {
      add_final(set, s);
      n += vecnode_remove_replace_with_last(tentatives, n);
      continue;
    }

    bool stop = false;
    error e = each(mod, s, &stop, user);
    EXCEPT(e);

    if (stop) {
      return 0;
    }
  }

  struct vectyp *list = &set->list;
  for (size_t n = 0; n < vectyp_count(list); ++n) {
    struct typ *s = *vectyp_get(list, n);
    bool stop = false;
    error e = each(mod, s, &stop, user);
    EXCEPT(e);
    if (stop) {
      return 0;
    }
  }

  return 0;
}

static ERROR print_each(struct module *mod, struct typ *t,
                        bool *stop, void *user) {
  fprintf(stderr, "%s\n", pptyp(mod, t));
  return 0;
}

void debug_print_typset(const struct module *mod, const struct typset *set) {
  error never = typset_foreach(CONST_CAST(mod), CONST_CAST(set), print_each, NULL);
  assert(!never);
}

