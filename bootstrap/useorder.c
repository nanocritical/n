#include "useorder.h"

#include "topdeps.h"

const char *forward_guards[FORWARD__NUM] = {
  [FWD_DECLARE_TYPES] = "NLANG_DECLARE_TYPES",
  [FWD_DEFINE_DYNS] = "NLANG_DEFINE_DYNS",
  [FWD_DEFINE_TYPES] = "NLANG_DEFINE_TYPES",
  [FWD_DECLARE_FUNCTIONS] = "NLANG_DECLARE_FUNCTIONS",
  [FWD_DEFINE_FUNCTIONS] = "NLANG_DEFINE_FUNCTIONS",
};

enum {
  MARK_NEEDED = 0x10000000,
};

static void init(struct useorder *uorder, const struct module *mod) {
  memset(uorder, 0, sizeof(*uorder));
  fintypset_fullinit(&uorder->marks);
  nodeset_init(&uorder->globals, 0);
  uorder->mod = mod;
}

void useorder_destroy(struct useorder *uorder) {
  vecnode_destroy(&uorder->dependencies);
  fintypset_destroy(&uorder->marks);
  nodeset_destroy(&uorder->globals);
  memset(uorder, 0, sizeof(*uorder));
}

static bool xxx;
static const char spaces[320] =
  "                                                                                "
  "                                                                                "
  "                                                                                "
  "                                                                                ";

#define DEF(t) typ_definition_ignore_any_overlay(t)

static void mark(struct useorder *uorder, struct typ *t, uint32_t td) {
  bool already = fintypset_set(&uorder->marks, t, td);
  if (already) {
    *fintypset_get(&uorder->marks, t) |= td;
  }
}

static uint32_t get_mark(struct useorder *uorder, struct typ *t) {
  uint32_t *mk = fintypset_get(&uorder->marks, t);
  return mk != NULL ? *mk : 0;
}

static bool fully_marked(struct useorder *uorder, struct typ *t, uint32_t td) {
  uint32_t *mk = fintypset_get(&uorder->marks, t);
  return mk != NULL && ((*mk & MARK_NEEDED) || (*mk & td) == td);
}

static void need(struct useorder *uorder, struct node *d) {
  if(xxx) fprintf(stderr, "need %s %x\n", pptyp(NULL, d->typ), get_mark(uorder, d->typ));
  if (get_mark(uorder, d->typ) & MARK_NEEDED) {
    return;
  }
  mark(uorder, d->typ, MARK_NEEDED);
  vecnode_push(&uorder->dependencies, d);
}

static void need_global(struct useorder *uorder, struct node *node) {
  assert(node->which == LET);
  uint32_t *has = nodeset_get(&uorder->globals, node);
  if (has != NULL && *has) {
    return;
  }

  if(xxx) fprintf(stderr, "need_global %s\n", scope_name(node_module_owner_const(node), subs_first(node)));
  nodeset_set(&uorder->globals, node, 1);
  vecnode_push(&uorder->dependencies, node);
}

struct mask_state {
  struct mask_state *prev;

  uint32_t inv_mask;
  size_t max_depth;
  bool inline_typebody;
  bool deftype_only;
};

struct state {
  struct useorder *uorder;

  topdeps_td_each each;
  size_t depth;
  struct mask_state *mask_state;
};

static void descend(struct state *st, const struct node *node);

static bool at_top(struct state *st, struct node *d) {
  struct node *par = parent(d);
  return par == st->uorder->mod->body || parent(par) == st->uorder->mod->body;
}

static error fwd_declare_types_each(struct module *mod, struct node *node,
                                    struct node *d, uint32_t td, void *user) {
  struct state *st = user;
  const bool is_at_top = at_top(st, d);
  const enum node_which which = d->which;
  struct typ *t = d->typ;
  td &= ~st->mask_state->inv_mask;

  if (!is_at_top
      && !(td & (TD_DYN_NEEDS_TYPE | TD_TYPEBODY_NEEDS_TYPE
                 | TD_FUN_NEEDS_TYPE | TD_FUNBODY_NEEDS_TYPE))) {
    return 0;
  }

  if (typ_was_zeroed(t) || !typ_is_concrete(t)) {
    return 0;
  }

  if (which != LET) {
    if (fully_marked(st->uorder, t, td)) {
      return 0;
    }
    mark(st->uorder, t, td);
  }

  bool pop_state = false;
  switch (which) {
  case LET:
    if (is_at_top) {
      struct node *dd = DEF(subs_first(d)->typ);
      descend(st, dd);
      need(st->uorder, dd);
    }
    break;
  case DEFFUN:
  case DEFMETHOD:
    descend(st, d);
    break;
  case DEFTYPE:
    descend(st, d);
    break;
  case DEFINTF:
    if (td & TD_DYN_NEEDS_TYPE) {
      pop_state = true;
      PUSH_STATE(st->mask_state);
      st->mask_state->inv_mask = ~(TD_FUN_NEEDS_TYPE | TD_DYN_NEEDS_TYPE);
      st->mask_state->max_depth = st->depth + 2;
      descend(st, d);
    } else {
      descend(st, d);
    }
    break;
  default:
    descend(st, d);
    break;
  }

  if (pop_state) {
    POP_STATE(st->mask_state);
  }

  if (NM(which) & (NM(DEFTYPE) | NM(DEFINTF))) {
    need(st->uorder, d);
  }

  return 0;
}

static error fwd_define_dyns_each(struct module *mod, struct node *node,
                                  struct node *d, uint32_t td, void *user) {
  struct state *st = user;
  const bool is_at_top = at_top(st, d);
  struct typ *t = d->typ;

  if (!is_at_top
      && !(td & (TD_DYN_NEEDS_TYPE | TD_TYPEBODY_NEEDS_TYPE
                 | TD_FUN_NEEDS_TYPE | TD_FUNBODY_NEEDS_TYPE
                 | TD_TYPEBODY_NEEDS_DYN | TD_FUNBODY_NEEDS_DYN
                 | TD_FUNBODY_NEEDS_DYNBODY))) {

    return 0;
  }

  if (typ_was_zeroed(t) || !typ_is_concrete(t)) {
    return 0;
  }

  if (fully_marked(st->uorder, t, td)) {
    return 0;
  }
  mark(st->uorder, t, td);

  const enum node_which which = d->which;
  switch (which) {
  case DEFFUN:
  case DEFMETHOD:
    descend(st, DEF(t));
    break;
  case DEFTYPE:
    if (is_at_top) {
      descend(st, d);
    } else if (td & (TD_TYPEBODY_NEEDS_TYPEBODY | TD_FUN_NEEDS_TYPEBODY
                     | TD_FUNBODY_NEEDS_TYPEBODY | TD_FUNBODY_NEEDS_DYNBODY)) {
      descend(st, d);
    }
    break;
  default:
    descend(st, DEF(t));
    break;
  }

  if ((td & (TD_DYN_NEEDS_TYPE | TD_TYPEBODY_NEEDS_DYN | TD_FUNBODY_NEEDS_DYN | TD_FUNBODY_NEEDS_DYNBODY))
       && (NM(which) & (NM(DEFTYPE) | NM(DEFINTF)))) {
    need(st->uorder, d);
  }

  return 0;
}

static error fwd_define_types_each(struct module *mod, struct node *node,
                                   struct node *d, uint32_t td, void *user) {
  struct state *st = user;
  const bool is_at_top = at_top(st, d);
  const enum node_which which = d->which;
  struct typ *t = d->typ;

  if (st->mask_state->inline_typebody) {
    if (!is_at_top
        && !(td & (TD_TYPEBODY_NEEDS_TYPEBODY))) {
      return 0;
    }
  }

  if (typ_was_zeroed(t) || !typ_is_concrete(t)) {
    return 0;
  }

  bool pop_state = false;
#if 0
  uint32_t mk = get_mark(st->uorder, t);
  if (mk != 0) {
    if (!(td & TD_TYPEBODY_NEEDS_TYPEBODY)) {
      return 0;
    }
    pop_state = true;
    PUSH_STATE(st->mask_state);
    st->mask_state->inline_typebody = true;
  } else if (mk & TD_TYPEBODY_NEEDS_TYPEBODY) {
    assert(!(td & TD_TYPEBODY_NEEDS_TYPEBODY) && "cycle");
  }
#endif
  if (which != LET) {
    if (fully_marked(st->uorder, t, td)) {
      return 0;
    }
    mark(st->uorder, t, td);
  }

  if (td & TD_TYPEBODY_NEEDS_TYPEBODY) {
    // First we'll gather inline field dependencies.
    pop_state = true;
    PUSH_STATE(st->mask_state);
    st->mask_state->inline_typebody = true;
  }

again:
  switch (which) {
  case LET:
    if (is_at_top) {
      struct node *dd = DEF(subs_first(d)->typ);
      descend(st, dd);
      need(st->uorder, dd);
    }
    break;
  case DEFFUN:
  case DEFMETHOD:
    if (st->mask_state->inline_typebody) {
      // noop
    } else if (is_at_top || node_is_inline(d)) {
      descend(st, d);
    }
    break;
  case DEFTYPE:
    DEBUG_IF_IDENT(mod, "fmt.n", node, "Prf")
    DEBUG_IF_IDENT(mod, "fmt.n", d, "Stringbuf")__break();
    if (is_at_top) {
      descend(st, d);
      need(st->uorder, d);
    } else if (td & (TD_TYPEBODY_NEEDS_TYPEBODY | TD_FUN_NEEDS_TYPEBODY | TD_FUNBODY_NEEDS_TYPEBODY)) {
      descend(st, d);
      need(st->uorder, d);
    }
    break;
  case DEFINTF:
    need(st->uorder, d);
    break;
  default:
    descend(st, d);
    break;
  }

  if (pop_state) {
    POP_STATE(st->mask_state);
    pop_state = false;
    goto again;
  }

  return 0;
}

static error fwd_declare_functions_each(struct module *mod, struct node *node,
                                        struct node *d, uint32_t td, void *user) {
  struct state *st = user;
  const bool is_at_top = at_top(st, d);
  const enum node_which which = d->which;
  struct typ *t = d->typ;

  if (!(NM(which) & (NM(DEFFUN) | NM(DEFMETHOD) | NM(DEFTYPE) | NM(LET)))) {
    return 0;
  }

  if (st->mask_state->deftype_only && which != DEFTYPE) {
    return 0;
  }

  if (!is_at_top
      && !(td & (TD_FUNBODY_NEEDS_TYPE | TD_DYN_NEEDS_TYPE | TD_FUNBODY_NEEDS_DYNBODY
                 | TD_ANY_NEEDS_NODE))) {
    return 0;
  }

  if (typ_was_zeroed(t) || !typ_is_concrete(t)) {
    return 0;
  }

  if (which != LET) {
    if (fully_marked(st->uorder, t, td)) {
      return 0;
    }
    mark(st->uorder, t, td);
  }

  switch (which) {
  case DEFFUN:
  case DEFMETHOD:
    if (is_at_top
        || ((NM(node->which) & (NM(DEFFUN) | NM(DEFMETHOD)))
            && node_is_inline(d) && (td & TD_FUNBODY_NEEDS_TYPE))) {
      descend(st, d);
    } else if (td & (TD_DYN_NEEDS_TYPE | TD_FUNBODY_NEEDS_TYPE)) {
      PUSH_STATE(st->mask_state);
      st->mask_state->deftype_only = true;
      descend(st, d);
      POP_STATE(st->mask_state);
    }
    need(st->uorder, d);
    break;
  case DEFTYPE:
    if (td & TD_FUNBODY_NEEDS_DYNBODY) {
      descend(st, d);
      need(st->uorder, d);
    }
    break;
  case LET:
    if (subs_first(d)->which == DEFALIAS) {
      // noop
    } else if (is_at_top || (td & TD_ANY_NEEDS_NODE)) {
      descend(st, d);
      need_global(st->uorder, d);
    }
    break;
  default:
    break;
  }

  return 0;
}

static error fwd_define_functions_each(struct module *mod, struct node *node,
                                       struct node *d, uint32_t td, void *user) {
  struct state *st = user;
  const bool is_at_top = at_top(st, d);
  const enum node_which which = d->which;
  struct typ *t = d->typ;

  if (!(NM(which) & (NM(DEFFUN) | NM(DEFMETHOD) | NM(DEFTYPE) | NM(LET)))) {
    return 0;
  }

  if (!is_at_top
      && !(td & (TD_FUNBODY_NEEDS_TYPE | TD_TYPEBODY_NEEDS_TYPEBODY
                 | TD_FUNBODY_NEEDS_DYNBODY | TD_ANY_NEEDS_NODE))) {
    return 0;
  }

  if (typ_was_zeroed(t) || !typ_is_concrete(t)) {
    return 0;
  }

  if (which != LET) {
    if (fully_marked(st->uorder, t, td)) {
      return 0;
    }
    mark(st->uorder, t, td);
  }

  switch (which) {
  case DEFFUN:
  case DEFMETHOD:
    if (is_at_top
        || ((NM(node->which) & (NM(DEFFUN) | NM(DEFMETHOD)))
            && node_is_inline(d) && (td & TD_FUNBODY_NEEDS_TYPE))
        // Inline code is using a non-inline, non-exported function: so it's
        // opaque inline by induction.
        || ((NM(node->which) & (NM(DEFFUN) | NM(DEFMETHOD)))
            && !node_is_export(d) && (td & TD_FUNBODY_NEEDS_TYPE))) {
      descend(st, d);
      need(st->uorder, d);
    }
    break;
  case DEFTYPE:
    if (td & TD_FUNBODY_NEEDS_DYNBODY) {
      descend(st, d);
      need(st->uorder, d);
    }
    break;
  case LET:
    if (subs_first(d)->which == DEFALIAS) {
      // noop
    } else if (is_at_top || (td & TD_ANY_NEEDS_NODE)) {
      descend(st, d);
      need_global(st->uorder, d);
    }
    break;
  default:
    break;
  }

  return 0;
}

static void descend(struct state *st, const struct node *node) {
  if (st->mask_state->max_depth != 0 && st->depth >= st->mask_state->max_depth) {
    return;
  }

  if(xxx) fprintf(stderr, "%.*s:: %s\n", (int)st->depth, spaces, pptyp(NULL, node->typ));
  st->depth += 1;
  error e = topdeps_foreach_td(CONST_CAST(st->uorder->mod), CONST_CAST(node),
                               st->each, st);
  assert(!e);
  st->depth -= 1;
}

void useorder_build(struct useorder *uorder, const struct module *mod,
                    bool header, enum forward fwd) {
  xxx = strcmp(mod->filename, "lib/n/fmt/fmt.n")==0;

  init(uorder, mod);
  uorder->header = header;
  uorder->fwd = fwd;

  struct state st = { 0 };
  st.uorder = uorder;
  PUSH_STATE(st.mask_state);

  switch (fwd) {
  case FWD_DECLARE_TYPES:
    st.each = fwd_declare_types_each;
    break;
  case FWD_DEFINE_DYNS:
    st.each = fwd_define_dyns_each;
    break;
  case FWD_DEFINE_TYPES:
    st.each = fwd_define_types_each;
    break;
  case FWD_DECLARE_FUNCTIONS:
    st.each = fwd_declare_functions_each;
    break;
  case FWD_DEFINE_FUNCTIONS:
    st.each = fwd_define_functions_each;
    break;
  default:
    assert(false);
  }

  const uint64_t filter = NM(DEFTYPE) | NM(DEFINTF) | NM(DEFFUN) | NM(DEFMETHOD) | NM(LET);
  FOREACH_SUB(n, mod->body) {
    if ((NM(n->which) & filter) && typ_is_concrete(n->typ)) {
      (void) st.each(CONST_CAST(mod), n, n, 0, &st);

      if (NM(n->which) & NM(DEFTYPE)) {
        FOREACH_SUB(m, n) {
          if ((NM(m->which) & filter) && typ_is_concrete(m->typ)) {
            (void) st.each(CONST_CAST(mod), m, m, 0, &st);
          }
        }
      }
    }
  }
}

void debug_useorder_print(struct useorder *uorder) {
  for (size_t n = 0, count = vecnode_count(&uorder->dependencies); n < count; ++n) {
    struct node **node = vecnode_get(&uorder->dependencies, n);
    if (*node != NULL) {
      fprintf(stderr, "%s :%s\n",
              (*node)->which == LET ? scope_name(uorder->mod, subs_first(*node)) : "",
              pptyp(uorder->mod, (*node)->typ));
    }
  }
  fprintf(stderr, "-- %s %d %s %zu\n", uorder->mod->filename, uorder->header,
          forward_guards[uorder->fwd], vecnode_count(&uorder->dependencies));
}
