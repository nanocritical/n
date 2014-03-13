#include "constraints.h"

#include "passes.h"
#include "types.h"
#include <stdarg.h>

typedef int8_t cbool;

enum cbool_values {
  U = 0,
  N = -1,
  Y = 1,
};

HTABLE_SPARSE(tagset, cbool, ident);
IMPLEMENT_HTABLE_SPARSE(unused__ static, tagset, cbool, ident,
                        ident_hash, ident_cmp);

struct constraint {
  cbool init;
  cbool nonnull;
  struct tagset tags;
  // FIXME: 'tags' is not normalized: it can be full of 'U' that while
  // considered to be deleted by the underlying table, do appear in
  // tagset_count().

  struct constraint *surround;
};

void constraint_invariant(const struct constraint *c) {
  assert(c->init >= N && c->init <= Y);
}

static struct constraint *new_constraint(struct module *mod) {
  struct constraint *c = mempool_calloc(mod, 1, sizeof(struct constraint));

  tagset_init(&c->tags, 0);
  tagset_set_delete_val(&c->tags, U);

  return c;
}

static int constraint_equal_tag_foreach(const ident *tag, cbool *value, void *user) {
  struct tagset *b = user;
  cbool *existing = tagset_get(b, *tag);
  if (*value == U) {
    return !(existing == NULL || *existing == U);
  } else {
    return *value != *existing;
  }
}

static bool constraint_equal(const struct constraint *_a, const struct constraint *_b) {
  struct constraint *a = (struct constraint *) _a;
  struct constraint *b = (struct constraint *) _b;

  INVARIANT_CONSTRAINT(a);
  INVARIANT_CONSTRAINT(b);

  if (a->init != b->init) {
    return false;
  }
  if (a->nonnull != b->nonnull) {
    return false;
  }
  if (tagset_count(&a->tags) != tagset_count(&b->tags)) {
    return false;
  }
  if (tagset_foreach(&a->tags, constraint_equal_tag_foreach, &b->tags)) {
    return false;
  }
  if (tagset_foreach(&b->tags, constraint_equal_tag_foreach, &a->tags)) {
    return false;
  }

  return true;
}

EXAMPLE_NCC_EMPTY(constraint_equal) {
  {
    struct constraint *a = new_constraint(mod), *b = new_constraint(mod);
    a->init = Y;
    b->init = Y;
    assert(constraint_equal(a, b));
  }
  {
    struct constraint *a = new_constraint(mod), *b = new_constraint(mod);
    a->init = Y;
    b->init = N;
    assert(!constraint_equal(a, b));
  }
  {
    struct constraint *a = new_constraint(mod), *b = new_constraint(mod);
    a->init = U;
    b->init = N;
    assert(!constraint_equal(a, b));
  }
  {
    struct constraint *a = new_constraint(mod), *b = new_constraint(mod);
    a->nonnull = U;
    b->nonnull = N;
    assert(!constraint_equal(a, b));
  }
  {
    struct constraint *a = new_constraint(mod), *b = new_constraint(mod);
    tagset_set(&a->tags, ID_C, Y);
    assert(!constraint_equal(a, b));
  }
  {
    struct constraint *a = new_constraint(mod), *b = new_constraint(mod);
    tagset_set(&b->tags, ID_C, Y);
    assert(!constraint_equal(a, b));
  }
  {
    struct constraint *a = new_constraint(mod), *b = new_constraint(mod);
    tagset_set(&a->tags, ID_C, Y);
    tagset_set(&b->tags, ID_C, Y);
    assert(constraint_equal(a, b));
  }
  {
    struct constraint *a = new_constraint(mod), *b = new_constraint(mod);
    tagset_set(&a->tags, ID_C, U);
    tagset_set(&b->tags, ID_C, Y);
    assert(!constraint_equal(a, b));
  }
}

static void constraint_set_init(struct module *mod, struct node *node, bool reversed) {
  struct constraint *c = node->constraint;
  INVARIANT_CONSTRAINT(c);

  c->init = reversed ? N : Y;
}

static void constraint_unset_init(struct module *mod, struct node *node) {
  struct constraint *c = node->constraint;
  INVARIANT_CONSTRAINT(c);

  c->init = U;
}

static void constraint_set_nonnull(struct module *mod, struct node *node, bool reversed) {
  struct constraint *c = node->constraint;
  INVARIANT_CONSTRAINT(c);

  c->nonnull = reversed ? N : Y;
}

static void constraint_unset_nonnull(struct module *mod, struct node *node) {
  struct constraint *c = node->constraint;
  INVARIANT_CONSTRAINT(c);

  c->nonnull = U;
}

static void constraint_set_tag(struct module *mod, struct node *node,
                               ident tag, bool reversed) {
  struct constraint *c = node->constraint;
  INVARIANT_CONSTRAINT(c);

  tagset_set(&c->tags, tag, reversed ? N : Y);
}

static void constraint_unset_tag(struct module *mod, struct node *node, ident tag) {
  struct constraint *c = node->constraint;
  INVARIANT_CONSTRAINT(c);

  tagset_set(&c->tags, tag, U);
}

struct snprint_constraint_state {
  const struct module *mod;
  char *s;
  size_t len;
  size_t pos;
  bool do_nos;
  bool has_yess;
  bool has_nos;
};

static int snprint_constraint_tag_foreach(const ident *tag, cbool *value, void *user) {
  struct snprint_constraint_state *st = user;
  if ((*value == Y && !st->do_nos)
      || (*value == N && st->do_nos)) {
    int n = snprintf(st->s, st->len,
                     "|%s", idents_value(st->mod->gctx, *tag));
    st->s += n;
    st->len -= n;
    st->has_yess = true;
  } else if (*value == N) {
    st->has_nos = true;
  }
  return 0;
}

int snprint_constraint(char *s, size_t len,
                       const struct module *mod, struct constraint *c) {
  size_t pos = 0;
  pos += snprintf(s+pos, len-pos, "(");

  if (c->init == Y) {
    pos += snprintf(s+pos, len-pos, "init");
  } else if (c->init == N) {
    pos += snprintf(s+pos, len-pos, "not init");
  }

  if (c->init != U && c->nonnull != U) {
    pos += snprintf(s+pos, len-pos, " and ");
  }

  if (c->nonnull == Y) {
    pos += snprintf(s+pos, len-pos, "nonnull");
  } else if (c->nonnull == N) {
    pos += snprintf(s+pos, len-pos, "not nonnull");
  }

  if (((c->init != U && c->nonnull == U) || (c->init == U && c->nonnull != U))
      && tagset_count(&c->tags) != 0) {
    pos += snprintf(s+pos, len-pos, " and ");
  }

  if (tagset_count(&c->tags) != 0) {
    struct snprint_constraint_state st = {
      .mod = mod,
      .s = s + pos,
      .len = len - pos,
      .do_nos = false,
      .has_yess = false,
      .has_nos = false,
    };

    tagset_foreach(&c->tags, snprint_constraint_tag_foreach, &st);

    if (st.has_nos) {
      if (st.has_yess) {
        int n = snprintf(st.s, st.len, " and not ");
        st.s += n;
        st.len -= n;
      }

      st.do_nos = true;
      tagset_foreach(&c->tags, snprint_constraint_tag_foreach, &st);
    }

    pos += len - pos - st.len;
  }

  pos += snprintf(s+pos, len-pos, ")");
  return 0;
}

EXAMPLE_NCC_EMPTY(snprint_constraint) {
  char s[2048] = { 0 };
  size_t len = ARRAY_SIZE(s);

  {
    struct node *na = node_new_subnode(mod, mod->body);
    na->constraint = new_constraint(mod);
    constraint_set_init(mod, na, false);
    snprint_constraint(s, len, mod, na->constraint);
    assert(strcmp(s, "(init)") == 0);
  }
  {
    struct node *na = node_new_subnode(mod, mod->body);
    na->constraint = new_constraint(mod);
    constraint_set_init(mod, na, true);
    snprint_constraint(s, len, mod, na->constraint);
    assert(strcmp(s, "(not init)") == 0);
  }
  {
    struct node *na = node_new_subnode(mod, mod->body);
    na->constraint = new_constraint(mod);
    constraint_set_init(mod, na, true);
    constraint_unset_init(mod, na);
    snprint_constraint(s, len, mod, na->constraint);
    assert(strcmp(s, "()") == 0);
  }
  {
    struct node *na = node_new_subnode(mod, mod->body);
    na->constraint = new_constraint(mod);
    constraint_set_nonnull(mod, na, false);
    constraint_unset_nonnull(mod, na);
    constraint_set_nonnull(mod, na, true);
    snprint_constraint(s, len, mod, na->constraint);
    assert(strcmp(s, "(not nonnull)") == 0);
  }
  {
    struct node *na = node_new_subnode(mod, mod->body);
    na->constraint = new_constraint(mod);
    constraint_set_init(mod, na, false);
    constraint_set_nonnull(mod, na, true);
    snprint_constraint(s, len, mod, na->constraint);
    assert(strcmp(s, "(init and not nonnull)") == 0);
  }
  {
    struct node *na = node_new_subnode(mod, mod->body);
    na->constraint = new_constraint(mod);
    constraint_set_tag(mod, na, ID_C, false);
    constraint_unset_tag(mod, na, ID_C);
    constraint_set_tag(mod, na, ID_C, false);
    snprint_constraint(s, len, mod, na->constraint);
    assert(strcmp(s, "(|c)") == 0);
  }
  {
    struct node *na = node_new_subnode(mod, mod->body);
    na->constraint = new_constraint(mod);
    constraint_set_tag(mod, na, ID_C, false);
    constraint_set_tag(mod, na, ID_CTOR, false);
    snprint_constraint(s, len, mod, na->constraint);
    assert(strcmp(s, "(|c|ctor)") == 0);
  }
  {
    struct node *na = node_new_subnode(mod, mod->body);
    na->constraint = new_constraint(mod);
    constraint_set_tag(mod, na, ID_C, false);
    constraint_set_tag(mod, na, ID_CTOR, true);
    snprint_constraint(s, len, mod, na->constraint);
    assert(strcmp(s, "(|c and not |ctor)") == 0);
  }
}

static error mk_except_constraint(const struct module *mod,
                                  const struct node *for_error,
                                  const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  char s[2048] = { 0 };
  size_t pos = 0, len = ARRAY_SIZE(s);

  pos += snprint_codeloc(s+pos, len-pos, mod, for_error);
  pos += snprintf(s+pos, len-pos, "constraint: ");
  pos += vsnprintf(s+pos, len-pos, fmt, ap);
  if (for_error != NULL) {
    pos += snprintf(s+pos, len-pos, " ");
    pos += snprint_constraint(s+pos, len-pos, mod, for_error->constraint);
  }

  error e = 0;
  GOTO_THROWF(EINVAL, "%s", s);

except:
  va_end(ap);
  return e;
}

static int constraint_get_single_tag_foreach(const ident *tag, cbool *value, void *user) {
  if (*value != U) {
    ident *result = user;
    if (*result != ID__NONE) {
      return 1;
    }
    *result = *tag;
  }
  return 0;
}

static error constraint_get_single_tag(ident *tag,
                                       const struct module *mod,
                                       const struct node *node) {
  struct constraint *c = node->constraint;
  INVARIANT_CONSTRAINT(c);

  *tag = ID__NONE;
  int too_many = tagset_foreach(&c->tags, constraint_get_single_tag_foreach, tag);
  if (too_many) {
    error e = mk_except_constraint(mod, node, "too many possible tags");
    EXCEPT(e);
  } else if (*tag == ID__NONE) {
    error e = mk_except_constraint(mod, node, "no known tag");
    EXCEPT(e);
  }

  return 0;
}

static void constraint_copy_direct(struct module *mod,
                                   struct constraint *cdst,
                                   const struct constraint *csrc) {
  cdst->init = csrc->init;
  cdst->nonnull = csrc->nonnull;

  tagset_rehash(&cdst->tags, (struct tagset *) &csrc->tags);
}

static void constraint_copy(struct module *mod, struct node *dst, const struct node *src) {
  struct constraint *cdst = dst->constraint;
  const struct constraint *csrc = src->constraint;

  constraint_copy_direct(mod, cdst, csrc);
}

static error constraint_check_init(struct module *mod, struct node *node) {
  if (node->constraint->init != Y) {
    error e = mk_except_constraint(mod, node, "'init' constraint not satisfied");
    EXCEPT(e);
  }
  return 0;
}

static error constraint_check_nonnull(struct module *mod, struct node *node) {
  if (node->constraint->nonnull != Y) {
    error e = mk_except_constraint(mod, node, "'nonnull' constraint not satisfied");
    EXCEPT(e);
  }
  return 0;
}

struct constraint_compatible_state {
  struct module *mod;
  struct constraint *target;
  const struct node *nc;
};

static int constraint_compatible_assign_tag_foreach(const ident *tag,
                                                    cbool *value, void *user) {
  struct constraint_compatible_state *st = user;

  cbool *existing = tagset_get(&st->target->tags, *tag);
  if (existing == NULL || *existing == U) {
    error e = mk_except_constraint(st->mod, st->nc, "constraint does not allow"
                                   " possible case '%s' from",
                                   idents_value(st->mod->gctx, *tag));
    THROW(e);
  }

  return 0;
}

static error constraint_check_compatible_assign(struct module *mod,
                                                const struct node *ntarget,
                                                const struct node *nc) {
  struct constraint *target = ntarget->constraint;
  struct constraint *c = (struct constraint *) nc->constraint;

  error e;
  if (target->init == Y && c->init != Y) {
    e = mk_except_constraint(mod, nc,
                             "'init' constraint not satisfied");
    EXCEPT(e);
  }
  if (target->init == N && c->init != N) {
    e = mk_except_constraint(mod, nc,
                             "'not init' constraint does not allow 'init'");
    EXCEPT(e);
  }

  if (target->nonnull == Y && c->nonnull != Y) {
    e = mk_except_constraint(mod, nc,
                             "'nonnull' constraint not satisfied");
    EXCEPT(e);
  }
  if (target->nonnull == N && c->nonnull != N) {
    e = mk_except_constraint(mod, nc,
                             "'not nonnull' constraint does not allow 'nonnull'");
    EXCEPT(e);
  }

  if (tagset_count(&target->tags) != 0) {
    if (tagset_count(&c->tags) == 0) {
      e = mk_except_constraint(mod, nc,
                               "constrained enum or union does not allow"
                               " unconstrained enum or union");
      THROW(e);
    }

    struct constraint_compatible_state st = {
      .mod = mod,
      .target = target,
      .nc = nc,
    };
    e = tagset_foreach(&c->tags, constraint_compatible_assign_tag_foreach, &st);
    EXCEPT(e);
  }

  return 0;
}

EXAMPLE_NCC_EMPTY(constraint) {
  {
    struct node *na = node_new_subnode(mod, mod->body);
    struct node *nb = node_new_subnode(mod, mod->body);
    na->constraint = new_constraint(mod);
    nb->constraint = new_constraint(mod);
    constraint_set_init(mod, na, false);
    constraint_set_init(mod, nb, false);
    assert(0 == constraint_check_compatible_assign(mod, na, nb));
  }
  {
    struct node *na = node_new_subnode(mod, mod->body);
    struct node *nb = node_new_subnode(mod, mod->body);
    na->constraint = new_constraint(mod);
    nb->constraint = new_constraint(mod);
    constraint_set_init(mod, na, false);
    should_fail(constraint_check_compatible_assign(mod, na, nb));
  }
  {
    struct node *na = node_new_subnode(mod, mod->body);
    struct node *nb = node_new_subnode(mod, mod->body);
    na->constraint = new_constraint(mod);
    nb->constraint = new_constraint(mod);
    constraint_set_init(mod, na, false);
    constraint_set_nonnull(mod, nb, false);
    should_fail(constraint_check_compatible_assign(mod, na, nb));
  }
  {
    struct node *na = node_new_subnode(mod, mod->body);
    struct node *nb = node_new_subnode(mod, mod->body);
    na->constraint = new_constraint(mod);
    nb->constraint = new_constraint(mod);
    constraint_set_tag(mod, na, ID_C, false);
    constraint_set_tag(mod, na, ID_CTOR, false);
    constraint_set_tag(mod, nb, ID_C, false);
    constraint_unset_tag(mod, na, ID_CTOR);
    constraint_set_tag(mod, na, ID_CTOR, false);
    assert(0 == constraint_check_compatible_assign(mod, na, nb));
  }
}

static error constraint_inference_ident(struct module *mod, struct node *node) {
  struct node *prev_use = node->as.IDENT.prev_use;
  if (prev_use != NULL) {
    if (prev_use->which == DEFNAME) {
      if (node == prev_use->as.DEFNAME.pattern) {
        // noop
      } else {
        constraint_copy(mod, node, prev_use->as.DEFNAME.expr);
      }
    } else {
      constraint_copy(mod, node, prev_use);
    }
  }
  return 0;
}

static error cond_descend_eval_bin(struct module *mod, struct node *conditioned,
                                   struct node *conditioned_def,
                                   struct node *node, bool reversed) {
  struct node *na = subs_first(node);
  struct node *nb = subs_last(node);
  struct constraint *a = na->constraint;
  struct constraint *b = nb->constraint;
  enum token_type op = node->as.BIN.operator;

  if (OP_KIND(op) != OP_BIN_SYM_PTR) {
    return 0;
  }

  error e;
  switch (op) {
  case TEQPTR:
    if (a->nonnull == b->nonnull) {
      // noop
    } else if (a->nonnull == N && b->nonnull == Y) {
      e = mk_except_constraint(mod, NULL, "unsatisfiable === comparison"
                               " lhs is 'not nonnull', rhs is 'nonnull'");
      THROW(e);
    } else if (a->nonnull == Y && b->nonnull == N) {
      e = mk_except_constraint(mod, NULL, "unsatisfiable === comparison"
                               " lhs is 'nonnull', rhs is 'not nonnull'");
      THROW(e);
    }
    break;
  case TNEPTR:
    if (a->nonnull != b->nonnull) {
      // noop
    } else if (a->nonnull == N && b->nonnull == N) {
      e = mk_except_constraint(mod, NULL, "unsatisfiable !== comparison"
                               " lhs is 'not nonnull', rhs is 'not nonnull'");
      THROW(e);
    } else if (a->nonnull == Y && b->nonnull == Y) {
      e = mk_except_constraint(mod, NULL, "unsatisfiable !== comparison"
                               " lhs is 'nonnull', rhs is 'nonnull'");
      THROW(e);
    }
    reversed = !reversed;
    break;
  default:
    assert(false);
    break;
  }

  if (b->nonnull == U) {
    SWAP(a, b);
    SWAP(na, nb);
  }

  if (a->nonnull == U) {
    if (na->which == IDENT && na->as.IDENT.def == conditioned_def) {
      struct constraint *c = conditioned->constraint;
      switch (c->nonnull) {
      case U:
        c->nonnull = reversed ? Y : N;
        break;
      case Y:
        assert(!reversed);
        break;
      case N:
        assert(reversed);
        break;
      }
    }
  }

  return 0;
}

static error cond_descend_eval_un(struct module *mod, struct node *conditioned,
                                  struct node *conditioned_def,
                                  struct node *node, bool reversed) {
  return 0;
}

static error cond_descend_eval(struct module *mod, struct node *conditioned,
                               struct node *conditioned_def,
                               struct node *node, bool reversed) {
  assert(conditioned->which == PHI);

  error e;
  switch (node->which) {
  case BIN:
    e = cond_descend_eval_bin(mod, conditioned, conditioned_def, node, reversed);
    EXCEPT(e);
    break;
  case UN:
    e = cond_descend_eval_un(mod, conditioned, conditioned_def, node, reversed);
    EXCEPT(e);
    break;
  default:
    break;
  }

  return 0;
}

static error constraint_inference_phi_conditioned(struct module *mod,
                                                  struct node *node) {
  assert(vecnode_count(&node->as.PHI.ancestors) == 1);
  struct node *ancestor = vecnode_get(&node->as.PHI.ancestors, 0);
  constraint_copy(mod, node, ancestor);

  struct node *def = ancestor->which == IDENT ? ancestor->as.IDENT.def : ancestor;

  error e;
  struct branch_state *br_st = mod->state->branch_state;
  if (br_st->branching->which == MATCH) {
    struct node *pattern = prev(node_parent(node));
    if (node_ident(pattern) == ID_OTHERWISE) {
      pattern = prev(prev(pattern));
      while (pattern != NULL) {
        constraint_set_tag(mod, node, node_ident(pattern), true);
        pattern = prev(prev(pattern));
      }
    } else {
      constraint_set_tag(mod, node, node_ident(pattern), false);
    }

  } else {
    for (size_t nth = 0; nth < br_st->nth_branch; ++nth) {
      e = cond_descend_eval(
        mod, node, def,
        node_branching_nth_cond(br_st->branching, nth), true);
      EXCEPT(e);
    }

    e = cond_descend_eval(
      mod, node, def,
      node_branching_nth_cond(br_st->branching, br_st->nth_branch), false);
    EXCEPT(e);
  }

  return 0;
}

static int least_common_tag_foreach(const ident *name, cbool *value, void *user) {
  if (*value == U) {
    return 0;
  }

  struct tagset *ctags = user;
  cbool *existing = tagset_get(ctags, *name);
  if (existing == NULL) {
    tagset_set(ctags, *name, *value);
  } else if (*existing == U) {
    *existing = *value;
  } else if (*existing != *value) {
    *existing = U;
  }
  return 0;
}

static error constraint_inference_phi(struct module *mod, struct node *node) {
  if (node->as.PHI.is_conditioned) {
    error e = constraint_inference_phi_conditioned(mod, node);
    EXCEPT(e);
    return 0;
  }

  struct constraint *c = node->constraint;
  for (size_t n = 0, count = vecnode_count(&node->as.PHI.ancestors);
       n < count; ++n) {
    struct node *na = vecnode_get(&node->as.PHI.ancestors, n);
    struct constraint *a = na->constraint;

    if (n == 0) {
      constraint_copy(mod, node, na);
      continue;
    }

    if (c->init == U) {
      c->init = a->init;
    } else if (c->init != a->init) {
      c->init = U;
    }

    if (c->nonnull == U) {
      c->nonnull = a->nonnull;
    } else if (c->nonnull != a->nonnull) {
      c->nonnull = U;
    }

    if (tagset_count(&c->tags) == 0) {
      tagset_rehash(&c->tags, &a->tags);
    } else if (tagset_count(&a->tags) == 0) {
      tagset_destroy(&c->tags);
      tagset_set_delete_val(&c->tags, U);
    } else {
      tagset_foreach(&a->tags, least_common_tag_foreach, &c->tags);
    }
  }
  return 0;
}

bool constraint_has_common_root_tag(ident *tag,
                                    const struct module *mod, const struct node *node) {
  size_t count = tagset_count(&node->constraint->tags);
  if (count == 0) {
    return false;
  } else if (count == 1) {
    error e = constraint_get_single_tag(tag, mod, node);
    assert(!e);
    return true;
  } else {
    return false;
  }
}

static void constraint_defchoice_container(const struct node **container,
                                           struct module *mod, struct node *node) {
  struct node *parent = subs_first(node);
  assert(parent->flags & NODE_IS_DEFCHOICE);

  ident common = ID__NONE;
  if (constraint_has_common_root_tag(&common, mod, parent)) {
    *container = node_get_member_const(mod, typ_definition_const(parent->typ),
                                       common);
  } else {
    *container = typ_definition_const(parent->typ);
  }
}

static error constraint_inference_bin_acc(struct module *mod,
                                          struct node *node) {
  struct node *parent = subs_first(node);
  struct node *name = subs_last(node);

  error e;
  if (!(parent->flags & NODE_IS_TYPE)) {
    e = constraint_check_init(mod, parent);
    EXCEPT(e);
  }

  if (parent->flags & NODE_IS_DEFCHOICE) {
    const struct node *container = NULL;
    constraint_defchoice_container(&container, mod, node);

    struct node *field = NULL;
    e = scope_lookup_ident_immediate(&field, name, mod, &container->scope,
                                     node_ident(name), false);
    EXCEPT(e);
  }

  if (node->flags & NODE_IS_DEFCHOICE) {
    constraint_set_tag(mod, node, node_ident(name), false);
  }

  constraint_set_init(mod, node, false);
  return 0;
}

static error constraint_inference_bin(struct module *mod,
                                      struct node *node) {
  error e;
  enum token_type op = node->as.BIN.operator;
  switch (OP_KIND(op)) {
  case OP_BIN:
  case OP_BIN_RHS_TYPE:
    return 0;
  case OP_BIN_SYM:
    if (OP_IS_ASSIGN(op)) {
      e = constraint_check_init(mod, subs_last(node));
      EXCEPT(e);
      constraint_copy(mod, subs_first(node), subs_last(node));
      constraint_set_init(mod, node, false);
      return 0;
    }
    // fallthrough
  case OP_BIN_SYM_BOOL:
  case OP_BIN_SYM_ARITH:
  case OP_BIN_SYM_BW:
  case OP_BIN_SYM_PTR:
  case OP_BIN_BW_RHS_UNSIGNED:
    e = constraint_check_init(mod, subs_first(node));
    EXCEPT(e);
    e = constraint_check_init(mod, subs_last(node));
    EXCEPT(e);
    constraint_set_init(mod, node, false);
    return 0;
  case OP_BIN_ACC:
    e = constraint_inference_bin_acc(mod, node);
    EXCEPT(e);
    return 0;
  default:
    assert(false);
    break;
  }
  return 0;
}

static error constraint_inference_un(struct module *mod,
                                     struct node *node) {
  error e;
  switch (OP_KIND(node->as.UN.operator)) {
  case OP_UN_REFOF:
    switch (node->as.UN.operator) {
    case TNULREFDOT:
    case TNULREFBANG:
    case TNULREFSHARP:
    case TNULREFWILDCARD:
      break;
    default:
      constraint_set_nonnull(mod, node, false);
      break;
    }
    constraint_set_init(mod, node, false);
    return 0;
  case OP_UN_DEREF:
    e = constraint_check_init(mod, subs_first(node));
    EXCEPT(e);
    e = constraint_check_nonnull(mod, subs_first(node));
    EXCEPT(e);
    constraint_set_init(mod, node, false);
    return 0;
  case OP_UN_BOOL:
  case OP_UN_ARITH:
  case OP_UN_BW:
    e = constraint_check_init(mod, subs_first(node));
    EXCEPT(e);
    constraint_set_init(mod, node, false);
    return 0;
  default:
    assert(false);
    break;
  }
  return 0;
}

static error constraint_inference_tuples(struct module *mod,
                                         struct node *node) {
  // FIXME
  return 0;
}

static error constraint_inference_call(struct module *mod,
                                       struct node *node) {
  if (node->flags & NODE_IS_TYPE) {
    constraint_copy(mod, node, typ_definition_const(node->typ));
    return 0;
  }

  error e;
  const struct node *fun = subs_first(node);
  const struct typ *tfun = fun->typ;
  const struct node *dfun = typ_definition_const(tfun);

  const struct node *funargs = subs_at_const(dfun, IDX_FUNARGS);
  const ssize_t first_vararg = node_fun_first_vararg(dfun);
  ssize_t n = 0;
  FOREACH_SUB_EVERY(arg, node, 1, 1) {
    if (n == first_vararg) {
      break;
    }
    const struct node *target = subs_at_const(funargs, n);
    e = constraint_check_compatible_assign(mod, target, arg);
    EXCEPT(e);
    n += 1;
  }

  if (n == first_vararg) {
    const struct node *target = subs_at_const(funargs, n);

    FOREACH_SUB_EVERY(arg, node, 1 + n, 1) {
      e = constraint_check_compatible_assign(mod, target, arg);
      EXCEPT(e);
    }
  }

  const struct node *ret = subs_at_const(funargs, n);
  constraint_copy(mod, node, ret);

  return 0;
}

static error constraint_inference_return(struct module *mod,
                                         struct node *node) {
  const struct node *ret = module_retval_get(mod);
  error e = constraint_check_compatible_assign(mod, ret, subs_first(node));
  EXCEPT(e);

  return 0;
}

static int check_weaker_tags_foreach(const ident *name, cbool *value, void *user) {
  struct tagset *right_tags = user;
  cbool *existing = tagset_get(right_tags, *name);
  if (existing != NULL && *existing == Y && (*value == N || *value == U)) {
    return *name;
  }
  return 0;
}

static int restrict_tags_foreach(const ident *name, cbool *value, void *user) {
  if (*value == U) {
    return 0;
  }

  struct tagset *ctags = user;
  cbool *existing = tagset_get(ctags, *name);
  if (existing == NULL) {
    tagset_set(ctags, *name, *value);
  } else if (*existing == U) {
    *existing = *value;
  } else if (*value == U) {
    // noop
  } else if (*existing != *value) {
    return *name;
  }
  return 0;
}

static error unify_defchoice_init(struct module *mod, struct node *node,
                                  struct node *nleft, struct constraint *left,
                                  struct node *nright, struct constraint *right) {
  error e;
  struct node *dleft = typ_definition(nleft->typ);
  struct node *dright = typ_definition(nright->typ);

  assert(tagset_count(&left->tags) == 0);
  assert(dleft->as.DEFINCOMPLETE.variant_of != NULL);
  assert(nright->flags & NODE_IS_DEFCHOICE);

  ident root = ID__NONE;
  const bool yes = constraint_has_common_root_tag(&root, mod, nright);
  assert(yes);

  struct node *dc = node_get_member(mod, dright, root);
  assert(dc->which == DEFCHOICE && dc->as.DEFCHOICE.is_leaf);

  // Below, we force the links that we declined to create in
  // unify_with_defincomplete() because we did not yet know of the
  // constraint.

  typ_link_tentative(nright->typ, nleft->typ);

  FOREACH_SUB_EVERY(name, nleft, 0, 2) {
    struct node *field = NULL;
    e = scope_lookup_ident_immediate(&field, name, mod, &dc->scope,
                                     node_ident(name), false);
    EXCEPT(e);

    typ_link_tentative(field->typ, next(name)->typ);
  }

  constraint_set_tag(mod, node, root, false);
  constraint_set_init(mod, node, false);
  constraint_copy(mod, subs_first(node), node);

  return 0;
}

static error constraint_inference_typeconstraint(struct module *mod,
                                                 struct node *node) {
  constraint_copy(mod, node, subs_first(node));

  struct constraint *c = node->constraint;
  struct node *nleft = subs_first(node);
  struct constraint *left = nleft->constraint;
  struct node *nright = subs_last(node);
  struct constraint *right = nright->constraint;
  error e;

  if (nleft->which == INIT
      && typ_definition_const(nleft->typ)->which == DEFINCOMPLETE
      && tagset_count(&right->tags) == 1) {
    // As an exception, a TYPECONSTRAINT with a constraint can narrow the
    // possible tag cases when the following form is used:
    //   { ... }:some_union.A
    e = unify_defchoice_init(mod, node, nleft, left, nright, right);
    EXCEPT(e);
    return 0;
  }

  if (c->init == U) {
    c->init = right->init;
  } else if (right->init == U) {
    // noop
  } else if (c->init != right->init) {
    e = mk_except_constraint(mod, nright,
                             "incompatible constraints regarding 'init'");
    THROW(e);
  }

  if (c->nonnull == U) {
    c->nonnull = right->nonnull;
  } else if (right->nonnull == U) {
    // noop
  } else if (c->nonnull != right->nonnull) {
    e = mk_except_constraint(mod, nright,
                             "incompatible constraints regarding 'nonnull'");
    THROW(e);
  }

  if (tagset_count(&right->tags) == 0) {
    // noop
  } else if (tagset_count(&c->tags) == 0) {
    e = mk_except_constraint(mod, nright,
                             "tag constraints attempt to restrict "
                             "unconstrained tags");
    EXCEPT(e);
  } else {
    int ret = tagset_foreach(&right->tags, check_weaker_tags_foreach, &c->tags);
    if (ret != 0) {
      ident failed = ret;
      e = mk_except_constraint(mod, nright,
                               "tag constraint on '%s' attempts to restrict "
                               "unconstrained tag",
                               idents_value(mod->gctx, failed));
      EXCEPT(e);
    }

    ret = tagset_foreach(&c->tags, restrict_tags_foreach, &right->tags);
    if (ret != 0) {
      ident failed = ret;
      e = mk_except_constraint(mod, nright,
                               "incompatible constraints regarding tag '%s'",
                               idents_value(mod->gctx, failed));
      EXCEPT(e);
    }
  }

  // Copy over to LHS as the TYPECONSTRAINT itself will get elided in
  // step_remove_typeconstraints().
  constraint_copy(mod, subs_first(node), node);
  return 0;
}

static error constraint_inference_defarg(struct module *mod,
                                         struct node *node) {
  constraint_copy(mod, node, subs_last(node));
  constraint_set_init(mod, node, false);
  return 0;
}

static error constraint_inference_deffield(struct module *mod,
                                           struct node *node) {
  return 0;
}

static error constraint_inference_definitions(struct module *mod,
                                              struct node *node) {
  if (typ_is_reference(node->typ) && !typ_is_nullable_reference(node->typ)) {
    constraint_set_nonnull(mod, node, false);
  }
  return 0;
}

static error constraint_inference_genarg(struct module *mod,
                                         struct node *node) {
  if (typ_is_reference(node->typ) && !typ_is_nullable_reference(node->typ)) {
    constraint_set_nonnull(mod, node, false);
  }
  return 0;
}

STEP_NM(step_constraint_inference,
        -1);
error step_constraint_inference(struct module *mod, struct node *node,
                                void *user, bool *stop) {
  DSTEP(mod, node);
  error e;

  if (node->typ == TBI__NOT_TYPEABLE) {
    return 0;
  }

  node->constraint = new_constraint(mod);

  switch (node->which) {
  case NUL:
    constraint_set_init(mod, node, false);
    constraint_set_nonnull(mod, node, true);
    break;
  case IDENT:
    e = constraint_inference_ident(mod, node);
    EXCEPT(e);
    break;
  case PHI:
    e = constraint_inference_phi(mod, node);
    EXCEPT(e);
    break;
  case NUMBER:
  case BOOL:
  case STRING:
  case SIZEOF:
  case ALIGNOF:
  case INIT:
    constraint_set_init(mod, node, false);
    break;
  case BIN:
    e = constraint_inference_bin(mod, node);
    EXCEPT(e);
    break;
  case UN:
    e = constraint_inference_un(mod, node);
    EXCEPT(e);
    break;
  case TUPLE:
  case TUPLEEXTRACT:
  case TUPLENTH:
    e = constraint_inference_tuples(mod, node);
    EXCEPT(e);
    break;
  case CALL:
    e = constraint_inference_call(mod, node);
    EXCEPT(e);
    break;
  case CALLNAMEDARG:
    constraint_copy(mod, node, subs_first(node));
    break;
  case RETURN:
    e = constraint_inference_return(mod, node);
    EXCEPT(e);
    break;
  case BLOCK:
  case CATCH:
    constraint_copy(mod, node, subs_last(node));
    break;
  case IF:
  case WHILE:
  case MATCH:
  case TRY:
    break;
  case TYPECONSTRAINT:
    e = constraint_inference_typeconstraint(mod, node);
    EXCEPT(e);
    break;
  case DYN:
    constraint_copy(mod, node, subs_first(node));
    break;
  case DEFNAME:
    if (node->as.DEFNAME.expr != NULL) {
      constraint_copy(mod, node, node->as.DEFNAME.expr);
    }
    break;
  case DEFPATTERN:
    FOREACH_SUB(d, node) {
      if (d->which == DEFNAME) {
        if (typ_isa(d->typ, TBI_DEFAULT_CTOR)) {
          constraint_set_init(mod, d, false);
        }

        constraint_copy(mod, d->as.DEFNAME.pattern, d);
      }
    }
    break;
  case DEFARG:
    e = constraint_inference_defarg(mod, node);
    EXCEPT(e);
    break;
  case DEFFIELD:
    e = constraint_inference_deffield(mod, node);
    EXCEPT(e);
    break;
  case DIRECTDEF:
    {
      struct node *def = typ_definition(node->as.DIRECTDEF.typ);
      if (def->constraint != NULL) {
        constraint_copy(mod, node, def);
      }
    }
    break;
  case DEFINTF:
  case DEFTYPE:
    e = constraint_inference_definitions(mod, node);
    EXCEPT(e);
    break;
  case DEFGENARG:
  case SETGENARG:
    e = constraint_inference_genarg(mod, node);
    EXCEPT(e);
    break;
  case FOR:
  case BREAK:
  case CONTINUE:
  case NOOP:
  case EXCEP:
  case THROW:
  case DEFFUN:
  case DEFMETHOD:
  case DEFINCOMPLETE:
  case FUNARGS:
  case GENARGS:
  case LET:
  case DELEGATE:
  case PRE:
  case POST:
  case INVARIANT:
  case EXAMPLE:
  case DEFCHOICE:
  case WITHIN:
  case ISALIST:
  case ISA:
  case IMPORT:
  case MODULE:
  case MODULE_BODY:
  case ROOT_OF_ALL:
    // noop
    break;
  default:
    assert(false);
    break;
  }

  return 0;
}

struct check_tag_state {
  struct module *mod;
  struct node *node;
  struct tagset *matched;
};

static int check_tag_is_matched_foreach(const ident *name, cbool *value,
                                        void *user) {
  struct check_tag_state *st = user;
  if (tagset_get(st->matched, *name) == NULL) {
    error e = mk_except_type(st->mod, st->node,
                             "non-exhaustive match, for example '%s'",
                             idents_value(st->mod->gctx, *name));
    EXCEPT(e);
  }
  return 0;
}

STEP_NM(step_check_exhaustive_match,
        NM(MATCH));
error step_check_exhaustive_match(struct module *mod, struct node *node,
                                  void *user, bool *stop) {
  struct node *expr = subs_first(node);
  struct node *dexpr = typ_definition(expr->typ);
  const bool enum_or_union = dexpr->as.DEFTYPE.kind == DEFTYPE_ENUM
    || dexpr->as.DEFTYPE.kind == DEFTYPE_UNION;

  if (!enum_or_union) {
    return 0;
  }

  struct tagset matched;
  tagset_init(&matched, 0);
  tagset_set_delete_val(&matched, U);

  error e = 0;
  FOREACH_SUB_EVERY(p, node, 1, 2) {
    ident id;
    switch (p->which) {
    case IDENT:
      id = node_ident(p);
      if (id == ID_OTHERWISE) {
        if (p != prev(subs_last(node))) {
          e = mk_except(mod, p, "default pattern '_' must be last");
          GOTO_THROW(e);
        }
        // No need to check further.
        goto ok;
      }
      break;
    case BIN:
      assert(OP_KIND(p->as.BIN.operator) == OP_BIN_ACC);
      id = node_ident(subs_at(p, 1));
      break;
    default:
      assert(false);
    }

    if (tagset_get(&matched, id) != NULL) {
      e = mk_except(mod, p, "duplicated match case");
      GOTO_THROW(e);
    }

    tagset_set(&matched, id, Y);
  }

  struct check_tag_state st = {
    .mod = mod,
    .node = node,
    .matched = &matched,
  };
  e = (error) tagset_foreach(&expr->constraint->tags,
                             check_tag_is_matched_foreach, &st);
  GOTO_EXCEPT(e);

ok:
except:
  tagset_destroy(&matched);
  return e;
}

