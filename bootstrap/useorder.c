#include "useorder.h"

#include "topdeps.h"

const char *forward_guards[FORWARD__NUM] = {
  [FWD_DECLARE_TYPES] = "NLANG_DECLARE_TYPES",
  [FWD_DEFINE_DYNS] = "NLANG_DEFINE_DYNS",
  [FWD_DEFINE_TYPES] = "NLANG_DEFINE_TYPES",
  [FWD_DECLARE_FUNCTIONS] = "NLANG_DECLARE_FUNCTIONS",
  [FWD_DEFINE_FUNCTIONS] = "NLANG_DEFINE_FUNCTIONS",
};

enum mark {
  NONE = 0,
  TEMP,
  TEMP2,
  PERM,
};

static void init(struct useorder *uorder, const struct module *mod) {
  memset(uorder, 0, sizeof(*uorder));
  fintypset_fullinit(&uorder->marks);
  uorder->mod = mod;
}

void useorder_destroy(struct useorder *uorder) {
  vecnode_destroy(&uorder->dependencies);
  fintypset_destroy(&uorder->marks);
  memset(uorder, 0, sizeof(*uorder));
}

static bool xxx;
static const char spaces[80] = "                                                                                ";

#define DEF(t) typ_definition_ignore_any_overlay(t)

static void need(struct useorder *uorder, struct typ *t) {
  if(xxx) fprintf(stderr, "%s\n", pptyp(NULL, t));
  enum mark *mk = fintypset_get(&uorder->marks, t);
  if (mk != NULL && *mk == PERM) {
    return;
  }
  vecnode_push(&uorder->dependencies, DEF(t));
}

static void need_global(struct useorder *uorder, struct node *node) {
  assert(node->which == LET);
  if(xxx) fprintf(stderr, "%s\n", scope_name(node_module_owner_const(node), subs_first(node)));
  vecnode_push(&uorder->dependencies, node);
}

static void mark(struct useorder *uorder, struct typ *t, enum mark mk) {
  bool already = fintypset_set(&uorder->marks, t, mk);
  if (already) {
    *fintypset_get(&uorder->marks, t) = mk;
  }
}

static enum mark get_mark(struct useorder *uorder, struct typ *t) {
  uint32_t *v = fintypset_get(&uorder->marks, t);
  return v != NULL ? *v : NONE;
}

struct mask_state {
  struct mask_state *prev;

  uint32_t inv_mask;
  size_t max_depth;
  bool inline_typebody;
};

struct state {
  struct useorder *uorder;

  topdeps_td_each each;
  size_t depth;
  struct mask_state *mask_state;
};

static void descend(struct state *st, const struct node *node);

static error fwd_declare_types_each(struct module *mod, struct node *node,
                                    struct node *d, uint32_t td, void *user) {
  struct typ *t = d->typ;
  struct state *st = user;
  td &= ~st->mask_state->inv_mask;

  if (st->depth > 0
      && !(td & (TD_DYN_NEEDS_TYPE | TD_TYPEBODY_NEEDS_TYPE
                 | TD_FUN_NEEDS_TYPE | TD_FUNBODY_NEEDS_TYPE))) {
    return 0;
  }

  if (typ_was_zeroed(t) || !typ_is_concrete(t)) {
    return 0;
  }

  const enum mark mk = get_mark(st->uorder, t);
  if (mk != NONE) {
    return 0;
  }

  mark(st->uorder, t, TEMP);

  bool pop_state = false;
  const enum node_which which = d->which;
  switch (which) {
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
    need(st->uorder, t);
  }
  mark(st->uorder, t, PERM);

  return 0;
}

static error fwd_define_dyns_each(struct module *mod, struct node *node,
                                  struct node *d, uint32_t td, void *user) {
  struct typ *t = d->typ;
  struct state *st = user;

  if (st->depth > 0
      && !(td & (TD_DYN_NEEDS_TYPE | TD_TYPEBODY_NEEDS_TYPE
                 | TD_FUN_NEEDS_TYPE | TD_FUNBODY_NEEDS_TYPE
                 | TD_TYPEBODY_NEEDS_DYN | TD_FUNBODY_NEEDS_DYN))) {

    return 0;
  }

  if (typ_was_zeroed(t) || !typ_is_concrete(t)) {
    return 0;
  }

  const enum mark mk = get_mark(st->uorder, t);
  if (mk != NONE) {
    return 0;
  }

  mark(st->uorder, t, TEMP);

  const enum node_which which = d->which;
  switch (which) {
  case DEFFUN:
  case DEFMETHOD:
    descend(st, DEF(t));
    break;
  case DEFTYPE:
    if (st->depth == 0) {
      descend(st, d);
    } else if (td & (TD_TYPEBODY_NEEDS_TYPEBODY | TD_FUN_NEEDS_TYPEBODY | TD_FUNBODY_NEEDS_TYPEBODY)) {
      descend(st, d);
    }
    break;
  default:
    descend(st, DEF(t));
    break;
  }

  if ((td & (TD_DYN_NEEDS_TYPE | TD_TYPEBODY_NEEDS_DYN | TD_FUNBODY_NEEDS_DYN))
       && (NM(which) & (NM(DEFTYPE) | NM(DEFINTF)))) {
    need(st->uorder, t);
  }
  mark(st->uorder, t, PERM);

  return 0;
}

static error fwd_define_types_each(struct module *mod, struct node *node,
                                   struct node *d, uint32_t td, void *user) {
  struct typ *t = d->typ;
  struct state *st = user;

    if(xxx)fprintf(stderr, "%.*s %x %s, %s\n", (int)st->depth, spaces, td, pptyp(NULL, t), pptyp(NULL, node->typ));

  if (st->mask_state->inline_typebody) {
    if (st->depth > 0
        && !(td & (TD_TYPEBODY_NEEDS_TYPEBODY))) {
      return 0;
    }
  }

  if (typ_was_zeroed(t) || !typ_is_concrete(t)) {
    return 0;
  }

  bool pop_state = false;
  const enum mark mk = get_mark(st->uorder, t);
  switch (mk) {
  case NONE:
    mark(st->uorder, t, TEMP);
    break;
  case TEMP:
    if (!(td & TD_TYPEBODY_NEEDS_TYPEBODY)) {
      return 0;
    }
    mark(st->uorder, t, TEMP2);
    pop_state = true;
    PUSH_STATE(st->mask_state);
    st->mask_state->inline_typebody = true;
    break;
  case TEMP2:
    assert(!(td & TD_TYPEBODY_NEEDS_TYPEBODY) && "cycle");
    return 0;
  case PERM:
    return 0;
  }

  const enum node_which which = d->which;
  switch (which) {
  case DEFFUN:
  case DEFMETHOD:
    if (st->depth == 0 || node_is_inline(d)) {
      descend(st, d);
    }
    break;
  case DEFTYPE:
    if (st->mask_state->inline_typebody) {
      if (st->depth == 0
          || (td & (TD_TYPEBODY_NEEDS_TYPEBODY))) {
        descend(st, d);
      }
    } else {
      if (st->depth == 0) {
        descend(st, d);
      } else if (td & (TD_TYPEBODY_NEEDS_TYPEBODY | TD_FUN_NEEDS_TYPEBODY | TD_FUNBODY_NEEDS_TYPEBODY)) {
        descend(st, d);
      }
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
    need(st->uorder, t);
  }
  mark(st->uorder, t, PERM);

  return 0;
}

static error fwd_declare_functions_each(struct module *mod, struct node *node,
                                        struct node *d, uint32_t td, void *user) {
  const enum node_which which = d->which;
  struct typ *t = d->typ;
  struct state *st = user;

  if (!(NM(which) & (NM(DEFFUN) | NM(DEFMETHOD) | NM(DEFTYPE) | NM(LET)))) {
    return 0;
  }

  if (st->depth > 0
      && !(td & (TD_FUNBODY_NEEDS_TYPE | TD_DYN_NEEDS_TYPE | TD_FUNBODY_NEEDS_DYNBODY
                 | TD_ANY_NEEDS_NODE))) {
    return 0;
  }

  if (typ_was_zeroed(t) || !typ_is_concrete(t)) {
    return 0;
  }

  if (which != LET) {
    const enum mark mk = get_mark(st->uorder, t);
    if (mk != NONE) {
      return 0;
    }
  }

  mark(st->uorder, t, TEMP);

  switch (which) {
  case DEFFUN:
  case DEFMETHOD:
    if (st->depth == 0 || (node_is_inline(d) && (td & TD_FUNBODY_NEEDS_TYPE))) {
      descend(st, d);
    } else if (td & TD_DYN_NEEDS_TYPE) {
      descend(st, d);
    }
    break;
  case DEFTYPE:
    if ((st->depth == 0 || node_is_inline(d))
        && (td & TD_FUNBODY_NEEDS_DYNBODY)) {
      descend(st, d);
    }
    break;
  case LET:
    if (subs_first(d)->which == DEFALIAS) {
      // noop
    } else if (st->depth == 0 || (td & TD_ANY_NEEDS_NODE)) {
      need_global(st->uorder, d);
    }
    break;
  default:
    break;
  }

  need(st->uorder, t);
  mark(st->uorder, t, PERM);

  return 0;
}

static error fwd_define_functions_each(struct module *mod, struct node *node,
                                       struct node *d, uint32_t td, void *user) {
  const enum node_which which = d->which;
  struct typ *t = d->typ;
  struct state *st = user;

  if (!(NM(which) & (NM(DEFFUN) | NM(DEFMETHOD) | NM(DEFTYPE)))) {
    return 0;
  }

  if (st->depth > 0
      && !(td & (TD_FUNBODY_NEEDS_TYPE | TD_TYPEBODY_NEEDS_TYPEBODY | TD_FUNBODY_NEEDS_DYNBODY))) {
    return 0;
  }

  if (typ_was_zeroed(t) || !typ_is_concrete(t)) {
    return 0;
  }

  const enum mark mk = get_mark(st->uorder, t);
  if (mk != NONE) {
    return 0;
  }

  mark(st->uorder, t, TEMP);

  switch (which) {
  case DEFFUN:
  case DEFMETHOD:
    if (st->depth == 0 || (node_is_inline(d) && (td & TD_FUNBODY_NEEDS_TYPE))) {
      descend(st, d);
      need(st->uorder, t);
    }
    break;
  case DEFTYPE:
    if ((st->depth == 0 || node_is_inline(d))
        && (td & TD_FUNBODY_NEEDS_DYNBODY)) {
      descend(st, d);
      need(st->uorder, t);
    }
    break;
  default:
    break;
  }

  mark(st->uorder, t, PERM);

  return 0;
}

static void descend(struct state *st, const struct node *node) {
  if (st->mask_state->max_depth != 0 && st->depth >= st->mask_state->max_depth) {
    return;
  }

  if(xxx) fprintf(stderr, "%.*s :: %s\n", (int)st->depth, spaces, pptyp(NULL, node->typ));
  st->depth += 1;
  error e = topdeps_foreach_td(CONST_CAST(st->uorder->mod), CONST_CAST(node),
                               st->each, st);
  assert(!e);
  st->depth -= 1;
}

void useorder_build(struct useorder *uorder, const struct module *mod,
                    bool header, enum forward fwd) {
  xxx = strcmp(mod->filename, "lib/n/crypto/rand/rand.n")==0;

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
    if (NM(n->which) & filter) {
      (void) st.each(CONST_CAST(mod), n, n, 0, &st);
    }

    if (NM(n->which) & NM(DEFTYPE)) {
      FOREACH_SUB(m, n) {
        if (NM(m->which) & filter) {
          (void) st.each(CONST_CAST(mod), m, m, 0, &st);
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
