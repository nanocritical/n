#include "topdeps.h"

#include "types.h"
#include "parser.h"
#include "inference.h"

// If the typ is tentative, we cannot record it yet, as its hash will
// change. We need to keep a snapshot of the context when the topdep was
// first recorded.
struct snapshot {
  struct typ **loc;

  struct node *top;
  struct node *exportable;
  bool in_fun_in_block;
  uint32_t for_dyn;
};

VECTOR(vecsnapshot, struct snapshot, 4);
IMPLEMENT_VECTOR(unused__ static, vecsnapshot, struct snapshot);

// Loose elements ordering.
static inline use_result__ ssize_t vecsnapshot_remove_replace_with_last(struct vecsnapshot *v, ssize_t n) {
  const size_t count = vecsnapshot_count(v);
  if (count == 1) {
    vecsnapshot_destroy(v);
    return 0;
  }

  struct snapshot last = vecsnapshot_pop(v);
  if (n + 1 == count) {
    (void) last;
    return 0;
  } else {
    struct snapshot *snap = vecsnapshot_get(v, n);
    *snap = last;
    return -1;
  }
}

struct topdeps {
  struct vectyp list;
  struct fintypset set;

  struct vectyp list_tds;
  struct fintypset set_tds;

  struct nodeset globals;

  struct vecsnapshot tentatives;
};

static void topdeps_init(struct topdeps **topdeps) {
  if (*topdeps == NULL) {
    *topdeps = calloc(1, sizeof(**topdeps));
    fintypset_fullinit(&(*topdeps)->set);
    fintypset_fullinit(&(*topdeps)->set_tds);
    nodeset_init(&(*topdeps)->globals, 0);
  }
}

static void record_final_td(struct module *mod, struct snapshot *snap) {
  struct typ *t = *snap->loc;
  struct node *top = snap->top;

  struct node *topmost = top;
  while (!node_is_at_top(topmost)) {
    topmost = parent(topmost);
  }

  if (typ_is_pseudo_builtin(t)
      // when a try_remove_unnecessary_ssa_defname() is used on a global let:
      || topmost->which == NOOP) {
    return;
  }

  uint32_t td = snap->for_dyn;

  struct toplevel *toplevel = node_toplevel(top);
  const uint32_t top_flags = toplevel->flags;

  const bool within_exportable_field
    = snap->exportable != NULL
    && snap->exportable->which == DEFFIELD;
  const bool is_intf = typ_definition_which(t) == DEFINTF;
  const bool is_ref = typ_is_reference(t);
  const bool is_dyn = typ_is_dyn(t);
  const bool is_fun = typ_is_function(t);
  const bool is_imported = typ_generic_arity(t) == 0 && typ_module_owner(t) != node_module_owner(top);

  switch (top->which) {
  case DEFTYPE:
    if (within_exportable_field && snap->exportable->typ == t) {
      td |= (is_ref || is_intf) ? TD_TYPEBODY_NEEDS_TYPE : TD_TYPEBODY_NEEDS_TYPEBODY;
      td |= is_dyn ? TD_TYPEBODY_NEEDS_DYN : 0;
    }
    break;
  case DEFINTF:
    break;
  case LET:
    if (subs_first_const(top)->which == DEFNAME) {
      td |= is_ref ? TD_FUNBODY_NEEDS_TYPE : TD_FUNBODY_NEEDS_TYPEBODY;
    }
    break;
  case DEFFUN:
  case DEFMETHOD:
    if (snap->in_fun_in_block) {
      if (snap->for_dyn == 0) {
        td |= (is_ref || is_fun) ? TD_FUNBODY_NEEDS_TYPE : TD_FUNBODY_NEEDS_TYPEBODY;
        td |= is_dyn ? TD_FUNBODY_NEEDS_DYN : 0;
      }
    } else {
      td |= (is_ref || is_fun) ? TD_FUN_NEEDS_TYPE : TD_FUN_NEEDS_TYPEBODY;
    }
    break;
  default:
    break;
  }

  if (td == 0) {
    return;
  }

  topdeps_init(&toplevel->topdeps);

  uint32_t *value = fintypset_get(&toplevel->topdeps->set_tds, t);
  if (value == NULL) {
    fintypset_set(&toplevel->topdeps->set_tds, t, td);
    vectyp_push(&toplevel->topdeps->list_tds, t);
  } else {
    *value |= td;
  }

  if (is_dyn) {
    struct typ *i = typ_generic_arg(t, 0);
    uint32_t *value = fintypset_get(&toplevel->topdeps->set_tds, i);
    if (value == NULL) {
      fintypset_set(&toplevel->topdeps->set_tds, i, td);
      vectyp_push(&toplevel->topdeps->list_tds, i);
    } else {
      *value |= td;
    }
  }
#if 0
  if ((NM(top->which) & (NM(DEFTYPE) | NM(DEFINTF))) && typ_definition_which(t) == DEFINTF) {
    struct tit *m = typ_definition_members(t, DEFFUN, DEFMETHOD, 0);
    while (tit_next(m)) {
      struct typ *mt = tit_typ(m);
     // fprintf(stderr, "%s -> %s\n", pptyp(NULL, top->typ), pptyp(NULL, mt));
      uint32_t *value = fintypset_get(&toplevel->topdeps->set_tds, mt);
      if (value == NULL) {
        fintypset_set(&toplevel->topdeps->set_tds, mt, td);
        vectyp_push(&toplevel->topdeps->list_tds, mt);
      } else {
        *value = td;
      }
    }
  }
#endif
}

static void record_final(struct module *mod, struct snapshot *snap) {
  record_final_td(mod, snap);

  struct typ *t = *snap->loc;
  struct node *top = snap->top;

  struct node *topmost = top;
  while (!node_is_at_top(topmost)) {
    topmost = parent(topmost);
  }

  if (typ_is_pseudo_builtin(t)
      // when a try_remove_unnecessary_ssa_defname() is used on a global let:
      || topmost->which == NOOP
      || (top->typ != NULL && typ_equal(top->typ, t))
      || (topmost->typ != NULL && typ_equal(topmost->typ, t))) {
    return;
  }

  struct toplevel *toplevel = node_toplevel(top);

  static const uint32_t TO_KEEP = TOP_IS_EXPORT | TOP_IS_INLINE
    | TOP_IS_FUNCTOR | TOP_IS_PREVENT_DYN;
  static const uint32_t WEAK = TOP_IS_FUNCTOR;

  uint32_t mask = toplevel->flags & TO_KEEP;

  const bool within_exportable_field = snap->exportable != NULL
    && snap->exportable->which == DEFFIELD;
  if (within_exportable_field) {
    if (!name_is_export(mod, snap->exportable)) {
      mask &= ~TOP_IS_EXPORT;
    }
  }

  if (snap->in_fun_in_block) {
    if (!(mask & TOP_IS_INLINE)) {
      mask &= ~(TOP_IS_EXPORT | TOP_IS_INLINE);
    }
  }

  if (!typ_is_reference(t)) {
    switch (top->which) {
    case DEFTYPE:
      if (within_exportable_field) {
        mask |= TOP__TOPDEP_INLINE_STRUCT;
      }
      break;
    case LET:
      if (subs_first_const(top)->which == DEFNAME) {
        mask |= TOP__TOPDEP_INLINE_STRUCT;
      }
      break;
    default:
      break;
    }
  }

  // This may prove to be too restrictive, but we want to make sure that
  // builtins appears first in generated code.
  const bool is_dependency_on_builtins = top->typ != NULL
    && strncmp(node_module_owner_const(top)->filename, "lib/n/builtins/",
               sizeof("lib/n/builtins/")-1) == 0;
  if (!is_dependency_on_builtins
      && typ_generic_arity(t) > 0 && !typ_is_generic_functor(t)) {
    // Inherit the inline struct dependencies from fields of a generic.
    struct tit *tit = typ_definition_members(t, DEFFIELD, 0);
    while (tit_next(tit)) {
      struct typ *fieldt = tit_typ(tit);
      if (fieldt == NULL) {
        continue;
      }
      struct module *owner = typ_module_owner(fieldt);
      if (owner != mod) {
        struct node *import = module_find_import(mod, owner);
        if (import != NULL) {
          node_toplevel(import)->flags |= TOP_IS_INLINE;
        }
      }
    }
  }

  // Attribute the dependency to the parent type (if any), unless 'top' is a
  // generic function.
  if (!node_is_at_top(top)
      && (!(NM(top->which) & (NM(DEFFUN) | NM(DEFMETHOD)))
          || typ_generic_arity(top->typ) == 0
          || typ_is_generic_functor(top->typ))) {
    const uint32_t member_mask = mask;

    top = parent(top);
    toplevel = node_toplevel(top);
    mask |= toplevel->flags & TO_KEEP;

    if (!(member_mask & TOP_IS_EXPORT) || !(toplevel->flags & TOP_IS_EXPORT)) {
      mask &= ~TOP_IS_EXPORT;
    }
  }

  if (top->typ != NULL && typ_equal(top->typ, t)) {
    return;
  }

  topdeps_init(&toplevel->topdeps);

  uint32_t *value = fintypset_get(&toplevel->topdeps->set, t);
  if (value == NULL) {
    fintypset_set(&toplevel->topdeps->set, t, mask);
    vectyp_push(&toplevel->topdeps->list, t);
  } else {
    // A weak flag can only be set by the initial add. Otherwise, it can
    // only be unset.
    if ((*value & WEAK) && (mask & WEAK)) {
      *value |= mask;
    } else {
      *value = (*value & ~WEAK) | (mask & ~WEAK);
    }
  }
}

static void record_tentative(struct module *mod, struct snapshot *snap) {
  struct typ **loc = snap->loc;
  if (typ_is_pseudo_builtin(*loc)
      || (!typ_is_tentative(*loc)
          && !typ_is_ungenarg(*loc)
          && typ_definition_nooverlay(*loc) == snap->top)) {
    return;
  }

  struct toplevel *toplevel = node_toplevel(snap->top);
  topdeps_init(&toplevel->topdeps);

  vecsnapshot_push(&toplevel->topdeps->tentatives, *snap);
}

static void do_topdeps_record(struct module *mod, struct typ *t, uint32_t for_dyn) {
  struct top_state *st = mod->state->top_state;
  if (st == NULL
      || typ_definition_which(t) == MODULE) {
    return;
  }

  struct fun_state *fun_st = mod->state->fun_state;
  struct snapshot snap = {
    .loc = typ_permanent_loc(t),
    .top = st->top,
    .exportable = st->exportable,
    .in_fun_in_block = fun_st != NULL && fun_st->in_block,
    .for_dyn = for_dyn,
  };

  if (typ_is_tentative(t) || !typ_hash_ready(t)) {
    record_tentative(mod, &snap);
  } else {
    record_final(mod, &snap);
  }
}

void topdeps_record(struct module *mod, struct typ *t) {
  do_topdeps_record(mod, t, 0);
}

static ERROR add_dyn_topdep_each(struct module *mod, struct typ *t, struct typ *intf,
                                 bool *stop, void *user) {
  do_topdeps_record(mod, intf, TD_DYN_NEEDS_TYPE);

  struct tit *m = typ_definition_members(t, DEFFUN, DEFMETHOD, 0);
  while (tit_next(m)) {
    struct typ *mt = tit_typ(m);
    do_topdeps_record(mod, mt, TD_DYN_NEEDS_TYPE);
  }

  struct typ *r = NULL;
  error e = reference(&r, mod, NULL, TREFDOT, intf);
  assert(!e);
  do_topdeps_record(mod, r, TD_DYN_NEEDS_TYPE);

  e = reference(&r, mod, NULL, TREFBANG, intf);
  assert(!e);
  do_topdeps_record(mod, r, TD_DYN_NEEDS_TYPE);

  e = reference(&r, mod, NULL, TREFSHARP, intf);
  assert(!e);
  do_topdeps_record(mod, r, TD_DYN_NEEDS_TYPE);
  return 0;
}

void topdeps_record_dyn(struct module *mod, struct typ *t) {
  if (typ_is_reference(t)) {
    t = typ_generic_arg(t, 0);

    do_topdeps_record(mod, t, TD_DYN_NEEDS_TYPE);
  }

  error never = typ_isalist_foreach(mod, t, ISALIST_FILTEROUT_PREVENT_DYN,
                                    add_dyn_topdep_each, NULL);
  assert(!never);
}

void topdeps_record_mkdyn(struct module *mod, struct typ *t) {
  do_topdeps_record(mod, t, TD_FUNBODY_NEEDS_DYNBODY);

  struct tit *m = typ_definition_members(t, DEFFUN, DEFMETHOD, 0);
  while (tit_next(m)) {
    struct typ *mt = tit_typ(m);
    do_topdeps_record(mod, mt, TD_TYPEBODY_NEEDS_DYNBODY);
  }
}

void topdeps_record_global(struct module *mod, struct node *node) {
  if (NM(node->which) & (NM(DEFALIAS) | NM(DEFCHOICE))) {
    return;
  }
  assert(node->which == DEFNAME);
  node = parent(node);
  struct top_state *st = mod->state->top_state;
  struct node *top = st->top;
  struct toplevel *toplevel = node_toplevel(top);
  topdeps_init(&toplevel->topdeps);
  nodeset_set(&toplevel->topdeps->globals, node, TD_ANY_NEEDS_NODE);
}

error topdeps_foreach(struct module *mod, struct node *node,
                      topdeps_each each, void *user) {
  struct toplevel *toplevel = node_toplevel(node);
  if (toplevel->topdeps == NULL) {
    return 0;
  }

  // Need to work when entries are being added to the topdeps while we are
  // iterating over it, so we have to get the current count every time.

  struct vecsnapshot *tentatives = &toplevel->topdeps->tentatives;

  struct top_state *st = mod->state->top_state;
  // 'st' may be NULL when topdeps_foreach() is called from a non-passing
  // context, e.g. cprinter.

  for (ssize_t n = 0; n < vecsnapshot_count(tentatives); ++n) {
    struct snapshot *snap = vecsnapshot_get(tentatives, n);
    struct typ *t = *snap->loc;
    if (st != NULL) {
      if (!typ_is_tentative(t) && typ_hash_ready(t)) {
        record_final(mod, snap);
        n += vecsnapshot_remove_replace_with_last(tentatives, n);
        continue;
      }
    }

    error e = each(mod, node, t, 0, user);
    EXCEPT(e);
  }

  struct vectyp *list = &toplevel->topdeps->list;
  struct fintypset *set = &toplevel->topdeps->set;

  for (size_t n = 0; n < vectyp_count(list); ++n) {
    struct typ *t = *vectyp_get(list, n);
    const uint32_t mask = *fintypset_get(set, t);
    error e = each(mod, node, t, mask, user);
    EXCEPT(e);
  }

  return 0;
}

struct global_each_state {
  struct module *mod;
  struct node *node;
  topdeps_td_each each;
  void *user;
};

static int global_each(const struct node **node, uint32_t *td, void *user) {
  struct global_each_state *st = user;
  return st->each(st->mod, st->node, CONST_CAST(*node), *td, st->user);
}

error topdeps_foreach_td(struct module *mod, struct node *node,
                         topdeps_td_each each, void *user) {
  struct toplevel *toplevel = node_toplevel(node);
  if (toplevel->topdeps == NULL) {
    return 0;
  }

  struct vectyp *list = &toplevel->topdeps->list_tds;
  struct fintypset *set = &toplevel->topdeps->set_tds;

  for (size_t n = 0; n < vectyp_count(list); ++n) {
    struct typ *t = *vectyp_get(list, n);
    const uint32_t mask = *fintypset_get(set, t);
    error e = each(mod, node, typ_definition_ignore_any_overlay(t), mask, user);
    EXCEPT(e);
  }

  struct global_each_state st = {
    .mod = mod,
    .node = node,
    .each = each,
    .user = user,
  };
  int never = nodeset_foreach(&toplevel->topdeps->globals, global_each, &st);
  assert(!never);

  return 0;
}

static ERROR print_topdeps_each(struct module *mod, struct node *node,
                                struct typ *t, uint32_t topdep_mask, void *user) {
  if (typ_was_zeroed(t)) {
    return 0;
  }

  fprintf(stderr, "\t%04x %d %zu %s @%p\n",
          topdep_mask,
          typ_is_tentative(t),
          node_toplevel_const(typ_definition_ignore_any_overlay_const(t))->passing,
          pptyp(mod, t), t);
  return 0;
}

void debug_print_topdeps(const struct module *mod, const struct node *node) {
  fprintf(stderr, "%s :%s @%p\n", scope_name(mod, node),
          pptyp(mod, node->typ), node->typ);
  error never = topdeps_foreach(CONST_CAST(mod), CONST_CAST(node), print_topdeps_each, NULL);
  assert(!never);
}

static ERROR print_topdeps_each_td(struct module *mod, struct node *node,
                                   struct node *d, uint32_t topdep_mask, void *user) {
  struct typ *t = d->typ;
  if (typ_was_zeroed(t)) {
    return 0;
  }

  if (d->which == LET) {
    t = subs_first(d)->typ;
  }

  fprintf(stderr, "\t%04x %d %zu %s :%s @%p\n",
          topdep_mask,
          typ_is_tentative(t),
          node_toplevel_const(d)->passing,
          d->which == LET ? scope_name(mod, subs_first(d)) : "",
          pptyp(mod, t), t);
  return 0;
}

void debug_print_topdeps_td(const struct module *mod, const struct node *node) {
  fprintf(stderr, "%s :%s @%p\n", scope_name(mod, node),
          pptyp(mod, node->typ), node->typ);
  error never = topdeps_foreach_td(CONST_CAST(mod), CONST_CAST(node), print_topdeps_each_td, NULL);
  assert(!never);
}
