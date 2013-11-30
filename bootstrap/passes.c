#include "passes.h"

#include <stdarg.h>
#include "table.h"
#include "types.h"
#include "mock.h"

//#define DEBUG_PASS

#ifdef DEBUG_PASS
#define DSTEP(mod, node) do { \
  ident id = node_ident(node); \
  if (id != ID__NONE) { \
    fprintf(g_env.stderr, "%s %s\n", __func__, idents_value(mod->gctx, id)); \
  } else { \
    fprintf(g_env.stderr, "%s\n", __func__); \
  } \
} while (0)
#else
#define DSTEP(mod, node)
#endif

static bool is_excepted(const struct module *mod, struct node *node) {
  if (mod == NULL) {
    return FALSE;
  }

  for (struct except_list *ex = mod->excepts; ex != NULL; ex = ex->prev) {
    for (size_t n = 0; ex->list[n] != NULL; ++n) {
      if (ex->list[n] == node) {
        return TRUE;
      }
    }
  }
  return FALSE;
}

error pass(struct module *mod, struct node *node,
           const step *down_steps, const step *up_steps, ssize_t shallow_last_up,
           void *user) {
  error e;
  if (node == NULL) {
    node = mod->root;
  }

  if (is_excepted(mod, node)) {
    return 0;
  }

  bool stop = FALSE;
  for (size_t s = 0; down_steps[s] != NULL; ++s) {
    if (mod != NULL) {
      mod->state->step_state->upward = FALSE;
      mod->state->step_state->stepping = s;
    }

    bool stop = FALSE;
    e = down_steps[s](mod, node, user, &stop);
    EXCEPT(e);

    if (stop) {
      return 0;
    }
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *sub = node->subs[n];
    e = pass(mod, sub, down_steps, up_steps, -1, user);
    EXCEPT(e);
  }

  for (ssize_t s = 0; up_steps[s] != NULL
                      && (shallow_last_up == -1 || s <= shallow_last_up); ++s) {
    if (mod != NULL) {
      mod->state->step_state->upward = TRUE;
      mod->state->step_state->stepping = s;
    }

    e = up_steps[s](mod, node, user, &stop);
    EXCEPT(e);

    if (stop) {
      return 0;
    }
  }

  return 0;
}

error advance(struct module *mod, size_t p) {
  assert(p - mod->stage->state->passing <= 1);

  const struct pass *pa = &passes[p];

  int module_depth = 0;
  error e = pass(mod, NULL,
                 pa->downs, pa->ups, -1,
                 &module_depth);
  EXCEPT(e);

  return 0;
}

static size_t last_pass(void) {
  static __thread size_t r;
  if (r == 0) {
    do {
      r += 1;
    } while (passes[r].kind == PASS_FORWARD || passes[r].kind == PASS_BODY);
  }
  return r;
}

static size_t last_tentative_instance_pass(void) {
  static __thread size_t r;
  if (r == 0) {
    while (passes[r].kind != PASS_BODY) {
      r += 1;
    }
    r -= 1;
  }
  return r;
}

enum catchup_for {
  CATCHUP_BELOW_CURRENT = 0,
  CATCHUP_REWRITING_CURRENT,
  CATCHUP_AFTER_CURRENT, // depth-first order in the tree of nodes
  CATCHUP_NEW_INSTANCE,
  CATCHUP_TENTATIVE_NEW_INSTANCE,
};

// Rules for generated nodes:
// - No constraints when modifying anything below the current node (in
// node->subs, etc.);
// - Allowed to *rewrite* current node (change its kind, content, etc.);
// - Not allowed to modify the current node's parent->subs (including
// replacing the current node).
static error catchup(struct module *mod,
                     const struct node **except,
                     struct node *node,
                     struct scope *parent_scope,
                     enum catchup_for how) {
  if (except != NULL) {
    PUSH_STATE(mod->excepts);
    mod->excepts->list = except;
  }

  ssize_t from = 0, goal;
  if (how == CATCHUP_NEW_INSTANCE) {
    goal = mod->done ? last_pass() : mod->stage->state->passing;
  } else if (how == CATCHUP_TENTATIVE_NEW_INSTANCE) {
    goal = min(size_t, mod->stage->state->passing, last_tentative_instance_pass());
  } else if (!mod->state->step_state->upward) {
    goal = mod->stage->state->passing - 1;
  } else if (how == CATCHUP_BELOW_CURRENT) {
    goal = mod->stage->state->passing;
  } else if (how == CATCHUP_REWRITING_CURRENT
             || how == CATCHUP_AFTER_CURRENT) {
    goal = mod->stage->state->passing - 1;
  } else {
    assert(FALSE && "Unreached");
  }

  const bool need_new_state =
    how == CATCHUP_NEW_INSTANCE
    || how == CATCHUP_TENTATIVE_NEW_INSTANCE;

  PUSH_STATE(mod->stage->state);
  if (need_new_state) {
    PUSH_STATE(mod->state);
  }
  PUSH_STATE(mod->state->step_state);

  if (how == CATCHUP_TENTATIVE_NEW_INSTANCE) {
    mod->state->tentatively = TRUE;
  }

  for (ssize_t p = from; p <= goal; ++p) {
    const struct pass *pa = &passes[p];
    mod->stage->state->passing = p;

    int module_depth = 0;
    error e = pass(mod, node,
                   pa->downs, pa->ups, -1,
                   &module_depth);
    EXCEPT(e);

    if (p == 0) {
      node->scope->parent = parent_scope;
    }
  }

  if (mod->state->step_state->upward && how == CATCHUP_REWRITING_CURRENT) {
    // Catch up to, and including, the current step.
    const struct pass *pa = &passes[goal + 1];
    mod->stage->state->passing = goal + 1;

    int module_depth = 0;
    error e = pass(mod, node,
                   pa->downs, pa->ups, mod->state->step_state->prev->stepping,
                   &module_depth);
    EXCEPT(e);
  }

  POP_STATE(mod->state->step_state);
  if (need_new_state) {
    POP_STATE(mod->state);
  }
  POP_STATE(mod->stage->state);

  if (except != NULL) {
    POP_STATE(mod->excepts);
  }

  return 0;
}

static void record_tentative_instantiation(struct module *mod, struct node *i) {
  struct module_state *st = mod->state;
  st->tentative_instantiations_count += 1;
  st->tentative_instantiations = realloc(
    st->tentative_instantiations,
    st->tentative_instantiations_count * sizeof(*st->tentative_instantiations));
  st->tentative_instantiations[st->tentative_instantiations_count - 1] = i;
}

static error catchup_instantiation(struct module *instantiating_mod,
                                   struct module *gendef_mod,
                                   struct node *instance,
                                   struct scope *parent_scope,
                                   bool tentative) {
  enum catchup_for how = CATCHUP_NEW_INSTANCE;
  if (tentative) {
    how = CATCHUP_TENTATIVE_NEW_INSTANCE;
    record_tentative_instantiation(instantiating_mod, instance);
  }

  error e = catchup(gendef_mod, NULL, instance, parent_scope, how);
  EXCEPT(e);

  assert(typ_definition(instance->typ));

  if (instance->which == DEFTYPE) {
    for (size_t n = 0; n < instance->as.DEFTYPE.members_count; ++n) {
      struct node *m = instance->as.DEFTYPE.members[n];
      if (node_toplevel_const(m)->builtingen != BG__NOT) {
        continue;
      }

      error e = catchup(gendef_mod, NULL, m, instance->scope, how);
      EXCEPT(e);
    }
  }

  return 0;
}

static error step_do_rewrite_prototype_wildcards(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case UN:
    if (node->as.UN.operator == TREFWILDCARD
        || node->as.UN.operator == TNULREFWILDCARD) {
      // FIXME The proper solution is to use
      //   (intf t:Any) i_nullable r:(i_ref t) = (i_any_ref t)
      // instead of i_nullable_ref, i_nullable_mutable_ref, and i_nullable_mercurial_ref.
      // and use (i_nullable __wildcard_ref_arg__) here.
      assert(node->as.UN.operator != TNULREFWILDCARD && "Unsupported yet");

      node->which = CALL;
      struct node *d = mk_node(mod, node, IDENT);
      d->as.IDENT.name = ID_WILDCARD_REF_ARG;
      rew_insert_last_at(node, 0);
    }
    break;
  default:
    break;
  }
  return 0;
}

static error step_rewrite_prototype_wildcards(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  static const step down[] = {
    NULL,
  };

  static const step up[] = {
    step_do_rewrite_prototype_wildcards,
    NULL,
  };

  struct node *funargs;
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    funargs = node->subs[IDX_FUNARGS];
    for (size_t n = 0; n < funargs->subs_count; ++n) {
      struct node *arg = funargs->subs[n];
      if (arg->which == BLOCK) {
        break;
      }

      PUSH_STATE(mod->state->step_state);
      error e = pass(mod, arg, down, up, -1, NULL);
      EXCEPT(e);
      POP_STATE(mod->state->step_state);
    }
    break;
  default:
    break;
  }

  return 0;
}

static struct node *add_instance_deepcopy_from_pristine(struct module *mod,
                                                        struct node *node,
                                                        struct node *pristine,
                                                        bool tentative) {
  struct node *instance = calloc(1, sizeof(struct node));
  node_deepcopy(mod, instance, pristine);

  if (!tentative) {
    struct toplevel *toplevel = node_toplevel(node);
    const size_t idx = toplevel->instances_count;
    toplevel->instances_count += 1;
    toplevel->instances = realloc(toplevel->instances,
                                  toplevel->instances_count * sizeof(*toplevel->instances));
    toplevel->instances[idx] = instance;
  }

  if (instance->which == DEFTYPE) {
    instance->as.DEFTYPE.members_count = pristine->as.DEFTYPE.members_count,
      instance->as.DEFTYPE.members = calloc(instance->as.DEFTYPE.members_count,
                                            sizeof(*instance->as.DEFTYPE.members));

    for (size_t n = 0; n < pristine->as.DEFTYPE.members_count; ++n) {
      instance->as.DEFTYPE.members[n] = calloc(1, sizeof(**instance->as.DEFTYPE.members));
      node_deepcopy(mod, instance->as.DEFTYPE.members[n], pristine->as.DEFTYPE.members[n]);
    }
  }

  return instance;
}

static void append_member(struct node *deft, struct node *m) {
  assert(deft->which == DEFTYPE);

  deft->as.DEFTYPE.members_count += 1;
  deft->as.DEFTYPE.members = realloc(
    deft->as.DEFTYPE.members,
    deft->as.DEFTYPE.members_count * sizeof(*deft->as.DEFTYPE.members));

  deft->as.DEFTYPE.members[deft->as.DEFTYPE.members_count - 1] = m;
}

static void add_deftype_pristine_external_member(struct module *mod, struct node *deft,
                                                 struct node *member) {
  assert(deft->which == DEFTYPE);
  struct node *deft_pristine = node_toplevel(deft)->instances[0];
  struct node *member_pristine = node_toplevel(member)->instances[0];

  append_member(deft_pristine, member_pristine);
}

static error step_generics_pristine_copy(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
    if (node->subs[IDX_GENARGS]->subs_count > 0
        && node->subs[IDX_GENARGS]->subs[0]->which == DEFGENARG) {
      (void) add_instance_deepcopy_from_pristine(mod, node, node, FALSE);
    }
    break;
  case DEFFUN:
  case DEFMETHOD:
    // Always needed because the method/fun could be part of a generic
    // DEFTYPE, and we cannot know that yet.
    (void) add_instance_deepcopy_from_pristine(mod, node, node, FALSE);
    break;
  default:
    break;
  }

  return 0;
}

static error step_detect_prototypes(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  struct toplevel *toplevel = node_toplevel(node);
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    toplevel->is_prototype = toplevel->builtingen == BG__NOT
      && !node_has_tail_block(node);
    break;
  default:
    break;
  }
  return 0;
}

// Must be run before builtins are added.
static error step_detect_deftype_kind(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFTYPE) {
    return 0;
  }

  error e;
  struct node *f = NULL;
  enum deftype_kind k = DEFTYPE_PROTOTYPE;
  for (size_t n = 0; n < node->subs_count; ++n) {
    f = node->subs[n];

    switch (f->which) {
    case DEFFIELD:
      if (k == DEFTYPE_ENUM || k == DEFTYPE_SUM) {
        goto field_and_sum;
      }
      k = DEFTYPE_STRUCT;
      break;
    case DEFCHOICE:
      if (k == DEFTYPE_STRUCT) {
        goto field_and_sum;
      }
      if (k != DEFTYPE_SUM) {
        k = DEFTYPE_ENUM;
      }
      if ((!f->as.DEFCHOICE.has_value && node_ident(f->subs[IDX_CH_PAYLOAD-1]) != ID_TBI_VOID)
          || (f->as.DEFCHOICE.has_value && node_ident(f->subs[IDX_CH_PAYLOAD]) != ID_TBI_VOID)) {
        k = DEFTYPE_SUM;
      }
      break;
    default:
      break;
    }
  }

  node->as.DEFTYPE.kind = k;
  return 0;

field_and_sum:
  e = mk_except_type(mod, f, "type contains both fields and choices");
  THROW(e);
}

static error step_assign_deftype_which_values(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFTYPE
      || (node->as.DEFTYPE.kind != DEFTYPE_ENUM
          && node->as.DEFTYPE.kind != DEFTYPE_SUM)) {
    return 0;
  }

  struct node *prev = NULL;
  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *d = node->subs[n];
    if (d->which != DEFCHOICE) {
      continue;
    }

    if (d->as.DEFCHOICE.has_value) {
      prev = d;
      continue;
    }

    d->as.DEFCHOICE.has_value = TRUE;
    if (prev == NULL) {
      struct node *val = mk_node(mod, d, NUMBER);
      val->as.NUMBER.value = "0";
    } else {
      struct node *val = mk_node(mod, d, BIN);
      val->as.BIN.operator = TPLUS;
      struct node *left = node_new_subnode(mod, val);
      node_deepcopy(mod, left, prev->subs[IDX_CH_VALUE]);
      struct node *right = mk_node(mod, val, NUMBER);
      right->as.NUMBER.value = "1";
    }

    rew_insert_last_at(d, IDX_CH_VALUE);

    prev = d;
  }

  return 0;
}

static void fix_scopes_after_move(struct node *node) {
  node->scope->node = node;
  for (size_t n = 0; n < node->subs_count; ++n) {
    assert(node->subs[n]->scope->parent == node->scope);
  }
}

static error insert_tupleextract(struct module *mod, size_t arity, struct node *expr) {
  struct scope *parent_scope = expr->scope->parent;
  struct node copy = *expr;

  memset(expr, 0, sizeof(*expr));
  expr->which = TUPLEEXTRACT;
  for (size_t n = 0; n < arity; ++n) {
    struct node *nth = mk_node(mod, expr, TUPLENTH);
    nth->as.TUPLENTH.nth = n;
  }
  struct node *value = node_new_subnode(mod, expr);
  *value = copy;
  fix_scopes_after_move(value);

  const struct node *except[] = { value, NULL };
  error e = catchup(mod, except, expr, parent_scope, CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static error extract_defnames_in_pattern(struct module *mod, struct node *defpattern,
                                         struct node *pattern, struct node *expr) {
  struct node *def;
  error e;

  if (expr != NULL
      && pattern->which != expr->which
      && pattern->which != IDENT
      && pattern->which != EXCEP) {
    if (pattern->which == TUPLE) {
      e = insert_tupleextract(mod, pattern->subs_count, expr);
      EXCEPT(e);
    } else {
      e = mk_except(mod, pattern, "value destruct not supported");
      THROW(e);
    }
  }

#define UNLESS_NULL(n, sub) ( (n) != NULL ? (sub) : NULL )

  switch (pattern->which) {
  case EXCEP:
    {
      struct node *parent = node_parent(pattern);
      const size_t where = rew_find_subnode_in_parent(parent, pattern);

      struct node *label_ident = NULL;
      if (pattern->subs_count > 0) {
        label_ident = pattern->subs[0];
      }

      pattern = mk_node(mod, parent, IDENT);
      pattern->as.IDENT.name = gensym(mod);
      rew_move_last_over(parent, where, FALSE);

      e = catchup(mod, NULL, pattern, defpattern->scope, CATCHUP_BELOW_CURRENT);
      EXCEPT(e);

      def = mk_node(mod, defpattern, DEFNAME);
      def->as.DEFNAME.pattern = pattern;
      def->as.DEFNAME.expr = expr;
      def->as.DEFNAME.is_excep = TRUE;
      def->as.DEFNAME.excep_label_ident = label_ident;

      struct node *test = mk_node(mod, def, IDENT);
      test->as.IDENT.name = node_ident(pattern);

      e = catchup(mod, NULL, def, defpattern->scope, CATCHUP_BELOW_CURRENT);
      EXCEPT(e);

      return 0;
    }

  case IDENT:
    def = mk_node(mod, defpattern, DEFNAME);
    def->as.DEFNAME.pattern = pattern;
    def->as.DEFNAME.expr = expr;

    e = catchup(mod, NULL, def, defpattern->scope, CATCHUP_BELOW_CURRENT);
    EXCEPT(e);

    return 0;

  case UN:
    assert(FALSE && "Unsupported");
    e = extract_defnames_in_pattern(mod, defpattern, pattern->subs[0],
                                    UNLESS_NULL(expr, expr->subs[0]));
    EXCEPT(e);
    break;
  case TUPLE:
    for (size_t n = 0; n < pattern->subs_count; ++n) {
      e = extract_defnames_in_pattern(mod, defpattern, pattern->subs[n],
                                      UNLESS_NULL(expr, expr->subs[n]));
      EXCEPT(e);
    }
    break;
  case TYPECONSTRAINT:
    pattern->as.TYPECONSTRAINT.in_pattern = TRUE;
    e = extract_defnames_in_pattern(mod, defpattern, pattern->subs[0],
                                    UNLESS_NULL(expr, expr->subs[0]));
    EXCEPT(e);
    break;
  default:
    e = mk_except(mod, pattern, "invalid construct in pattern");
    THROW(e);
  }
#undef UNLESS_NULL

  return 0;
}

static error step_defpattern_extract_defname(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFPATTERN) {
    return 0;
  }

  struct node *expr = NULL;
  if (node->subs_count >= 2) {
    expr = node->subs[1];
  }

  error e = extract_defnames_in_pattern(mod, node, node->subs[0], expr);
  EXCEPT(e);

  return 0;
}

static error step_add_scopes(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != MODULE) {
    node->scope = scope_new(node);

    // In later passes, we may rewrite nodes that have been marked, we must
    // erase the marking. But not for module nodes that are:
    //   (i) never rewritten,
    //   (ii) are typed void when created, to allow global module lookup to
    //   use the 'typ' field when typing import nodes.
    if (node->which != SETGENARG) {
      node->typ = NULL;
      node->flags = 0;
    }
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *s = node->subs[n];
    if (s->scope != NULL) {
      s->scope->parent = node->scope;
    }
  }

  return 0;
}

static error step_stop_submodules(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != MODULE) {
    return 0;
  }

  int *module_depth = user;
  *module_depth += 1;

  if (*module_depth > 1) {
    *stop = TRUE;
  }
  return 0;
}

static error step_codeloc_for_generated(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->codeloc == 0
      && node->scope != NULL
      && node->scope->parent != NULL) {
    node->codeloc = node_parent(node)->codeloc;
  }

  return 0;
}

static void pass_import_mark(struct module *mod, struct node *mark,
                             struct scope *parent_scope) {
  static const step down[] = {
    NULL,
  };
  static const step up[] = {
    step_add_scopes,
    NULL,
  };

  PUSH_STATE(mod->state->step_state);
  error e = pass(mod, mark, down, up, -1, NULL);
  assert(!e);
  POP_STATE(mod->state->step_state);

  mark->scope->parent = parent_scope;
  // Special self-referencing typ; see type_inference_bin_accessor().
  mark->typ = typ_create(NULL, mark);
}

// Recursive, depth first; Will modify scope on the way back up.
static struct node *create_lexical_import_hierarchy(struct scope **scope,
                                                    struct module *mod,
                                                    struct node *original_import,
                                                    struct node *import_path,
                                                    struct node *import) {
  assert(import == NULL || import->which == IMPORT);

  error e;
  struct node *mark_ident = NULL;

  // Descent.
  switch (import_path->which) {
  case IDENT:
    mark_ident = import_path;
    break;
  case BIN:
    (void)create_lexical_import_hierarchy(scope, mod, original_import,
                                          import_path->subs[0], NULL);
    mark_ident = import_path->subs[1];
    break;
  default:
    assert(FALSE);
  }

  // Ascent.
  if (import != NULL) {
    // Back at the top: we're done.
    struct node *placeholder;
    ident name = node_ident(mark_ident);
    e = scope_lookup_ident_immediate(&placeholder, mark_ident, mod, *scope,
                                     name, TRUE);
    if (!e) {
      e = mk_except(mod, original_import,
                    "importing identifier '%s' more than once",
                    idents_value(mod->gctx, name));
      assert(!e);
    } else if (e == EINVAL) {
      e = scope_define_ident(mod, *scope, name, import);
      assert(!e);
      return import;
    } else if (e) {
      assert(!e && "Unreached");
      return NULL;
    }
  }

  ident name = node_ident(mark_ident);
  // Find existing import mark, if any.
  struct node *mark = NULL;
  e = scope_lookup_ident_immediate(&mark, mark_ident, mod, *scope, name, TRUE);
  if (e == EINVAL) {
    // We need to create an (intermediate) mark.
    struct node *anchor = (*scope == mod->body->scope)
      ? original_import
      : (*scope)->node;

    mark = mk_node(mod, anchor, IMPORT);
    mark->as.IMPORT.toplevel.is_export = original_import->as.IMPORT.toplevel.is_export;
    mark->as.IMPORT.intermediate_mark = TRUE;
    node_deepcopy(mod, node_new_subnode(mod, mark), import_path);
    pass_import_mark(mod, mark, anchor->scope);

    e = scope_define_ident(mod, *scope, name, mark);
    assert(!e);

  } else if (e) {
    assert(FALSE);
  } else {
    assert(mark->which == IMPORT);
  }

  assert(mark->scope);
  *scope = mark->scope;

  return NULL;
}

static error check_import_target_exists(struct module *mod,
                                        struct node *module_import_path,
                                        int points_inside_module) {
  struct node *mpath
    = points_inside_module == 1 ? module_import_path->subs[0] : module_import_path;

  struct node *def = NULL;
  error e = scope_lookup_module(&def, mod, mpath, TRUE);

  if (e == EINVAL && points_inside_module == -1 /* caller doesn't know */) {
    e = check_import_target_exists(mod, module_import_path, 1);
    EXCEPT(e);
    return 0;
  } else if (e) {
    EXCEPT(e);
  }

  if (points_inside_module == 1) {
    // We don't use target, but we check that it exists.
    struct node *target = NULL;
    struct node *id = module_import_path->subs[1];
    e = scope_lookup_ident_immediate(&target, id,
                                     mod, def->scope,
                                     node_ident(id), FALSE);
    EXCEPT(e);
  }

  return 0;
}

static error import_single_ident(struct scope *scope, struct module *mod,
                                 struct node *original_import,
                                 struct node *id, bool define) {
  struct node *id_full_import_path = id->subs[0];
  assert(id_full_import_path->which == BIN);
  assert(id_full_import_path->subs[1]->which == IDENT);

  struct scope *tmp = scope;
  struct node *id_import_marker = create_lexical_import_hierarchy(
    &tmp, mod, original_import,
    id_full_import_path, id);

  error e;
  if (define) {
    ident id_name = node_ident(id_full_import_path->subs[1]);
    e = scope_define_ident(mod, scope, id_name, id_import_marker);
    EXCEPT(e);
  } else {
    e = check_import_target_exists(mod, id_full_import_path, 1);
    EXCEPT(e);
  }

  return 0;
}

static error lexical_import_from_path(struct scope *scope, struct module *mod,
                                      struct node *original_import,
                                      struct node *import) {

  error e;
  // We take of copy of 'subs_count' now as 'import' will grow if
  // 'import == original_import'.
  for (size_t n = 1, count = import->subs_count; n < count; ++n) {
    struct node *id = import->subs[n];

    e = import_single_ident(scope, mod, original_import, id, TRUE);
    EXCEPT(e);
  }

  return 0;
}

static struct node *create_import_node_for_ex(struct module *mod,
                                              struct node *original_import,
                                              struct node *import,
                                              struct node *ex) {
  struct node *id = node_new_subnode(mod, original_import);
  id->which = IMPORT;
  id->as.IMPORT.toplevel.is_export = original_import->as.IMPORT.toplevel.is_export;

  struct token tok = { 0 };
  tok.t = TIDENT;
  tok.value = idents_value(mod->gctx, node_ident(ex));
  tok.len = strlen(tok.value);
  copy_and_extend_import_path(mod, id, import, &tok);

  pass_import_mark(mod, id, original_import->scope);

  return id;
}

static error lexical_import(struct scope *scope, struct module *mod,
                            struct node *original_import, struct node *import);

static error lexical_import_path(struct scope *scope, struct module *mod,
                                 struct node *original_import,
                                 struct node *import) {
  error e;
  struct node *target = NULL;
  e = scope_lookup_module(&target, mod, import->subs[0], FALSE);
  EXCEPT(e);

  bool need_expose_all = original_import->as.IMPORT.is_all
    || target->which == MODULE;

  if (!need_expose_all) {
    e = import_single_ident(scope, mod, original_import, import, FALSE);
    EXCEPT(e);
    return 0;
  }

  struct module *target_mod = target->as.MODULE.mod;
  struct node *target_body = target_mod->body;
  for (size_t n = 0; n < target_body->subs_count; ++n) {
    struct node *ex = target_body->subs[n];
    const struct toplevel *toplevel = node_toplevel_const(ex);
    if (toplevel == NULL
        || !toplevel->is_export
        || toplevel->scope_name != ID__NONE
        || toplevel->is_shadowed) {
      continue;
    }

    if (ex->which == IMPORT) {
      e = lexical_import(scope, mod, original_import, ex);
      EXCEPT(e);

      continue;
    }

    struct node *id = create_import_node_for_ex(mod, original_import,
                                                import, ex);

    e = import_single_ident(scope, mod, original_import, id,
                            original_import->as.IMPORT.is_all);
    EXCEPT(e);
  }

  return 0;
}

static error lexical_import(struct scope *scope, struct module *mod,
                            struct node *original_import, struct node *import) {
  assert(import->which == IMPORT);
  error e;

  if (import->as.IMPORT.is_all) {
    // from <path> (import|export) *
    e = lexical_import_path(scope, mod, original_import, import);
    EXCEPT(e);
  } else if (import->subs_count == 1) {
    // (import|export) <path>
    e = lexical_import_path(scope, mod, original_import, import);
    EXCEPT(e);
  } else {
    // from <path> (import|export) <a> <b> <c> ...
    e = lexical_import_from_path(scope, mod, original_import, import);
    EXCEPT(e);
  }

  import->typ = TBI__NOT_TYPEABLE;

  return 0;
}

static error step_lexical_import(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  error e;

  switch (node->which) {
  case IMPORT:
    if (node_is_at_top(node)) {
      e = lexical_import(mod->body->scope, mod, node, node);
      EXCEPT(e);
    }
    return 0;
  default:
    return 0;
  }
}

static error lexical_retval(struct module *mod, struct node *fun, struct node *retval) {
  error e;

  switch (retval->which) {
  case BIN:
  case UN:
  case IDENT:
  case CALL:
    break;
  case DEFARG:
    e = scope_define(mod, fun->scope, retval->subs[0], retval);
    EXCEPT(e);
    break;
  case TUPLE:
    for (size_t n = 0; n < retval->subs_count; ++n) {
      struct node *r = retval->subs[n];
      e = lexical_retval(mod, fun, r);
      EXCEPT(e);
    }
    break;
  default:
    e = mk_except(mod, retval, "return value type expression not supported");
    EXCEPT(e);
  }

  return 0;
}

static error step_lexical_scoping(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  struct node *id = NULL;
  struct scope *sc = NULL;
  error e;

  struct node *container = NULL;
  const struct toplevel *toplevel = NULL;

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    if (node->subs[0]->which == IDENT) {
      id = node->subs[0];
    } else {
      id = node->subs[0]->subs[1];
    }

    toplevel = node_toplevel_const(node);
    if (toplevel->our_generic_functor_typ != NULL) {
      // For generic instances, do not define the name, as we want the type
      // name to point to the generic functor (e.g. in (vector u8), allow
      // vector to be used in (vector u16). To get the current instance,
      // use this or final.
      sc = NULL;
    } else if (toplevel->scope_name == 0
        || node_parent(node)->which == DEFINTF) {
      sc = node->scope->parent;
    } else {
      if (node_parent(node)->which == DEFTYPE) {
        // Generic instance *members* already have the 'right' parent.
        container = node_parent(node);
        sc = container->scope;
      } else {
        e = scope_lookup_ident_wontimport(&container, node, mod, node->scope->parent,
                                          toplevel->scope_name, FALSE);
        EXCEPT(e);
        sc = container->scope;
        node->scope->parent = sc;
      }

      const struct toplevel *ctoplevel = node_toplevel_const(container);
      if (toplevel->builtingen == BG__NOT // otherwise, will be re-generated
          && ctoplevel != NULL
          && ctoplevel->instances != NULL
          && ctoplevel->our_generic_functor_typ == NULL
          && container->subs[IDX_GENARGS]->subs_count > 0
          && container->subs[IDX_GENARGS]->subs[0]->which == DEFGENARG) {
        add_deftype_pristine_external_member(mod, container, node);
      }
    }
    break;
  case DEFTYPE:
  case DEFINTF:
    if (node_toplevel_const(node)->our_generic_functor_typ != NULL) {
      sc = NULL;

      // For generic instances, define the name in its own scope, to make
      // sure lookups inside the instance resolve to the instance itself
      // (e.g. the definition of this).
      e = scope_define(mod, node->scope, node->subs[0], node);
      EXCEPT(e);
    } else {
      id = node->subs[0];
      sc = node->scope->parent;
    }
    break;
  case DEFFIELD:
  case DEFCHOICE:
    id = node->subs[0];
    sc = node->scope->parent;
    break;
  case DEFNAME:
    if (node_ident(node) != ID_OTHERWISE) {
      id = node->as.DEFNAME.pattern;
      sc = node->scope->parent->parent->parent;
    }
    break;
  case CATCH:
    break;
  default:
    return 0;
  }

  if (sc != NULL) {
    e = scope_define(mod, sc, id, node);
    EXCEPT(e);
  }

  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
    for (size_t n = 0; n < node->subs[IDX_GENARGS]->subs_count; ++n) {
      struct node *ga = node->subs[IDX_GENARGS]->subs[n];
      assert(ga->which == DEFGENARG || ga->which == SETGENARG);
      e = scope_define(mod, node->scope, ga->subs[0], ga);
      EXCEPT(e);
    }
    break;
  case DEFFUN:
  case DEFMETHOD:
    for (size_t n = 0; n < node->subs[IDX_GENARGS]->subs_count; ++n) {
      struct node *ga = node->subs[IDX_GENARGS]->subs[n];
      assert(ga->which == DEFGENARG || ga->which == SETGENARG);
      e = scope_define(mod, node->scope, ga->subs[0], ga);
      EXCEPT(e);
    }

    struct node *funargs = node->subs[IDX_FUNARGS];
    for (size_t n = 0; n < node_fun_all_args_count(node); ++n) {
      struct node *arg = funargs->subs[n];
      assert(arg->which == DEFARG);
      e = scope_define(mod, node->scope, arg->subs[0], arg);
      EXCEPT(e);
    }

    e = lexical_retval(mod, node, node_fun_retval(node));
    EXCEPT(e);
    break;
  case CATCH:
    if (node->as.CATCH.is_user_label) {
      e = scope_define_ident(mod, node->scope->parent, node->as.CATCH.label, node);
      EXCEPT(e);
    }
    break;
  default:
    break;
  }

  return 0;
}

static error step_add_builtin_members(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
    break;
  default:
    return 0;
  }

  if (typ_is_pseudo_builtin(node->typ)) {
    return 0;
  }

  {
    struct node *let = mk_node(mod, node, LET);
    struct node *defp = mk_node(mod, let, DEFPATTERN);
    defp->as.DEFPATTERN.is_alias = TRUE;
    struct node *name = mk_node(mod, defp, IDENT);
    name->as.IDENT.name = ID_THIS;
    struct node *expr = mk_node(mod, defp, DIRECTDEF);
    set_typ(&expr->as.DIRECTDEF.typ, node->typ);
    expr->as.DIRECTDEF.flags = NODE_IS_TYPE;

    rew_insert_last_at(node, 3);

    error e = catchup(mod, NULL, let, node->scope, CATCHUP_BELOW_CURRENT);
    EXCEPT(e);
  }

  {
    struct node *let = mk_node(mod, node, LET);
    struct node *defp = mk_node(mod, let, DEFPATTERN);
    defp->as.DEFPATTERN.is_alias = TRUE;
    struct node *name = mk_node(mod, defp, IDENT);
    name->as.IDENT.name = ID_FINAL;
    struct node *expr = mk_node(mod, defp, DIRECTDEF);
    set_typ(&expr->as.DIRECTDEF.typ, node->typ);
    expr->as.DIRECTDEF.flags = NODE_IS_TYPE;

    rew_insert_last_at(node, 4);

    error e = catchup(mod, NULL, let, node->scope, CATCHUP_BELOW_CURRENT);
    EXCEPT(e);
  }

  return 0;
}

static void mark_subs(struct module *mod, struct node *node, struct typ *mark,
                      size_t begin, size_t end, size_t incr) {
  for (size_t n = begin; n < end; n += incr){
    node->subs[n]->typ = mark;
  }
}

static void inherit(struct module *mod, struct node *node) {
  if (node->typ == TBI__NOT_TYPEABLE) {
    mark_subs(mod, node, node->typ, 0, node->subs_count, 1);
  }
}

static error step_stop_marker_tbi(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->which == DEFTYPE) {
    switch (node_ident(node)) {
    case ID_TBI__NOT_TYPEABLE:
    case ID_TBI__CALL_FUNCTION_SLOT:
      *stop = TRUE;
      return 0;
    }
  }

  if (node->which == IMPORT && node->typ != NULL) {
    *stop = TRUE;
    return 0;
  }

  return 0;
}

static error step_stop_block(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  switch (node->which) {
  case BLOCK:
    *stop = TRUE;
    return 0;
  default:
    return 0;
  }
}

static error step_stop_funblock(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case BLOCK:
    break;
  default:
    return 0;
  }

  switch (node_parent(node)->which) {
  case DEFFUN:
  case DEFMETHOD:
  case EXAMPLE:
    *stop = TRUE;
    break;
  default:
    break;
  }

  return 0;
}

static error step_push_fun_state(struct module *mod, struct node *node, void *user, bool *stop) {
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
  case EXAMPLE:
    PUSH_STATE(mod->state->fun_state);
    break;
  default:
    break;
  }
  return 0;
}

static error step_pop_fun_state(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
  case EXAMPLE:
    POP_STATE(mod->state->fun_state);
    break;
  default:
    break;
  }
  return 0;

}

static error step_detect_not_dyn_intf_down(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    mod->state->fun_state->fun_uses_final = FALSE;
    break;
  default:
    break;
  }
  return 0;
}

static error step_detect_not_dyn_intf_up(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  if (mod->state->fun_state == NULL) {
    // Not in a function.
    return 0;
  }

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    node_toplevel(node)->is_not_dyn = mod->state->fun_state->fun_uses_final
      || node->subs[IDX_GENARGS]->subs_count != 0;
    break;
  case IDENT:
    if (node_ident(node) == ID_FINAL) {
      mod->state->fun_state->fun_uses_final = TRUE;
    }
    break;
  case DEFARG:
    if (rew_find_subnode_in_parent(node_parent(node), node) == 0
        && node_parent(node)->which == DEFMETHOD) {
      // We just found self as a method argument on the way up, doesn't count.
      assert(mod->state->fun_state->fun_uses_final);
      mod->state->fun_state->fun_uses_final = FALSE;
    }
    break;
  default:
    break;
  }
  return 0;
}

static error step_rewrite_wildcards(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

#define SET_UNLESS_ZERO(dst, src) if (src != 0) { dst = src; }

  switch (node->which) {
  case DEFMETHOD:
    if (node->subs[IDX_GENARGS]->subs_count == 0) {
      break;
    }
    struct node *def = NULL;
    error e = scope_lookup_ident_immediate(&def, node, mod, node->scope,
                                           ID_WILDCARD_REF_ARG, TRUE);
    if (e) {
      break;
    }
    if (typ_equal(def->typ, TBI_ANY_REF)) {
      // noop
    } else if (typ_equal(def->typ, TBI_REF)) {
      mod->state->fun_state->ref_wildcard = TREFDOT;
      mod->state->fun_state->nulref_wildcard = TNULREFDOT;
      mod->state->fun_state->deref_wildcard = TDEREFDOT;
      mod->state->fun_state->wildcard = TDOT;
    } else if (typ_equal(def->typ, TBI_MREF)) {
      mod->state->fun_state->ref_wildcard = TREFBANG;
      mod->state->fun_state->nulref_wildcard = TNULREFBANG;
      mod->state->fun_state->deref_wildcard = TDEREFBANG;
      mod->state->fun_state->wildcard = TBANG;
    } else if (typ_equal(def->typ, TBI_MMREF)) {
      mod->state->fun_state->ref_wildcard = TREFSHARP;
      mod->state->fun_state->nulref_wildcard = TNULREFSHARP;
      mod->state->fun_state->deref_wildcard = TDEREFSHARP;
      mod->state->fun_state->wildcard = TSHARP;
    } else {
      assert(FALSE);
    }
    break;
  case UN:
    switch (node->as.UN.operator) {
    case TREFWILDCARD:
      SET_UNLESS_ZERO(node->as.UN.operator, mod->state->fun_state->ref_wildcard);
      break;
    case TNULREFWILDCARD:
      SET_UNLESS_ZERO(node->as.UN.operator, mod->state->fun_state->nulref_wildcard);
      break;
    case TDEREFWILDCARD:
      SET_UNLESS_ZERO(node->as.UN.operator, mod->state->fun_state->deref_wildcard);
      break;
    default:
      break;
    }
    break;
  case BIN:
    switch (node->as.BIN.operator) {
    case TWILDCARD:
      SET_UNLESS_ZERO(node->as.UN.operator, mod->state->fun_state->wildcard);
      break;
    default:
      break;
    }
    break;
  default:
    break;
  }

#undef SET_UNLESS_ZERO

  return 0;
}

static error step_type_destruct_mark(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which == MODULE) {
    return 0;
  }

  inherit(mod, node);

  struct typ *not_typeable = TBI__NOT_TYPEABLE;

  switch (node->which) {
  case BIN:
    if (OP_KIND(node->as.BIN.operator) == OP_BIN_ACC) {
      mark_subs(mod, node, not_typeable, 1, node->subs_count, 1);
    }
    break;
  case CALL:
    if (node->subs[0]->typ == NULL) {
      // Otherwise, we are rewriting this expression and we should not touch
      // subs[0].
      mark_subs(mod, node, TBI__CALL_FUNCTION_SLOT, 0, 1, 1);
    }
    break;
  case INIT:
    if (!node->as.INIT.is_array) {
      mark_subs(mod, node, TBI__NOT_TYPEABLE, 0, node->subs_count, 2);
    }
    break;
  case DEFFUN:
  case DEFMETHOD:
    node->subs[0]->typ = not_typeable;
    break;
  case DEFTYPE:
  case DEFINTF:
    node->subs[0]->typ = not_typeable;
    break;
  case DEFFIELD:
  case DEFARG:
  case DEFGENARG:
  case SETGENARG:
    node->subs[0]->typ = not_typeable;
    break;
  case MODULE_BODY:
    node->typ = not_typeable;
    break;
  case DEFCHOICE:
    node->subs[0]->typ = not_typeable;
    break;
  default:
    break;
  }

  return 0;
}

static error step_type_mutability_mark(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  struct typ *mutable = TBI__MUTABLE;
  struct typ *mercurial = TBI__MERCURIAL;

  switch (node->which) {
  case BIN:
    switch (node->as.BIN.operator) {
    case TASSIGN:
    case TPLUS_ASSIGN:
    case TMINUS_ASSIGN:
    case TTIMES_ASSIGN:
    case TDIVIDE_ASSIGN:
    case TMODULO_ASSIGN:
    case TBWAND_ASSIGN:
    case TBWOR_ASSIGN:
    case TBWXOR_ASSIGN:
    case TRSHIFT_ASSIGN:
    case TLSHIFT_ASSIGN:
      mark_subs(mod, node, mutable, 0, 1, 1);
      break;
    default:
      break;
    }
    break;
  case UN:
    if (OP_KIND(node->as.UN.operator) != OP_UN_REFOF) {
      return 0;
    }

    // Below, we're interested in catching cases like: @#(self!p)

    struct node *arg= node->subs[0];
    switch (node->as.UN.operator) {
    case TREFDOT:
      // no-op
      break;
    case TREFBANG:
      if (arg->typ != NULL) {
        if (arg->which == BIN && !(arg->flags & NODE_IS_TYPE)) {
          error e = typ_check_deref_against_mark(mod, arg, TBI__MUTABLE,
                                                 arg->as.BIN.operator);
          EXCEPT(e);
        }
      } else {
        mark_subs(mod, node, mutable, 0, 1, 1);
      }
      break;
    case TREFWILDCARD:
    case TREFSHARP:
      if (arg->typ != NULL) {
        if (arg->which == BIN && !(arg->flags & NODE_IS_TYPE)) {
          error e = typ_check_deref_against_mark(mod, arg, TBI__MERCURIAL,
                                                 arg->as.BIN.operator);
          EXCEPT(e);
        }
        return 0;
      } else {
        mark_subs(mod, node, mercurial, 0, 1, 1);
      }
      break;
    default:
      break;
    }
    break;
  default:
    break;
  }

  return 0;
}

static error step_stop_already_morningtypepass(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case LET:
    if (node_is_at_top(node) || node_is_at_top(node_parent_const(node))) {
      *stop = node->typ != NULL;
    }
    break;
  case ISA:
  case GENARGS:
  case FUNARGS:
  case DEFGENARG:
  case SETGENARG:
  case DEFFIELD:
  case DEFCHOICE:
    *stop = node->typ != NULL;
    break;
  default:
    break;
  }
  return 0;
}

static error step_type_inference(struct module *mod, struct node *node, void *user, bool *stop);

static error morningtypepass(struct module *mod, struct node *node) {
  static const step down[] = {
    step_stop_marker_tbi,
    step_stop_block,
    step_stop_already_morningtypepass,
    step_type_destruct_mark,
    NULL,
  };

  static const step up[] = {
    step_type_inference,
    NULL,
  };

  PUSH_STATE(mod->state->step_state);
  if (mod->state->prev != NULL) {
    mod->state->tentatively |= mod->state->prev->tentatively;
  }
  error e = pass(mod, node, down, up, -1, NULL);
  EXCEPT(e);

  POP_STATE(mod->state->step_state);

  return 0;
}

static error step_type_definitions(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
  case DEFNAMEDLITERAL:
  case DEFCONSTRAINTLITERAL:
  case DEFFUN:
  case DEFMETHOD:
    break;
  default:
    return 0;
  }

  ident id = node_ident(node->subs[0]);

  if (node->subs[IDX_GENARGS]->subs_count > 0
      && node_toplevel(node)->our_generic_functor_typ != NULL) {
    set_typ(&node->typ, typ_create(NULL, node));
  } else if (mod->path[0] == ID_NLANG
             && (id >= ID_TBI__FIRST && id <= ID_TBI__LAST)) {
    // FIXME Effectively reserving these idents for builtin types, but
    // that's a temporary trick to avoid having to look up the current
    // module path.
    set_typ(&node->typ, typ_create(mod->gctx->builtin_typs_by_name[id], node));
  } else {
    set_typ(&node->typ, typ_create(NULL, node));
  }
  node->flags = NODE_IS_TYPE;

  return 0;
}

static error step_type_inference_genargs(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  error e;

  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
  case DEFNAMEDLITERAL:
  case DEFCONSTRAINTLITERAL:
  case DEFFUN:
  case DEFMETHOD:
    break;
  default:
    return 0;
  }

  if (node->typ == TBI__NOT_TYPEABLE) {
    return 0;
  }

  struct node *genargs = node->subs[IDX_GENARGS];
  e = morningtypepass(mod, genargs);
  EXCEPT(e);

  return 0;
}

static error step_type_create_update(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
  case DEFNAMEDLITERAL:
  case DEFCONSTRAINTLITERAL:
  case DEFFUN:
  case DEFMETHOD:
    break;
  default:
    return 0;
  }

  typ_create_update_genargs(node->typ);
  typ_create_update_hash(node->typ);

  return 0;
}

static error step_type_inference_isalist(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  error e;

  switch (node->which) {
  case ISA:
    e = morningtypepass(mod, node);
    EXCEPT(e);
    break;
  default:
    break;
  }

  return 0;
}

static error step_type_update_quickisa(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
  case DEFNAMEDLITERAL:
  case DEFCONSTRAINTLITERAL:
    break;
  default:
    return 0;
  }

  typ_create_update_quickisa(node->typ);

  return 0;
}

static error step_type_lets(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *parent = node_parent(node);
  error e;
  switch (node->which) {
  case LET:
    if (node_is_at_top(node) || node_is_at_top(parent)) {
      e = morningtypepass(mod, node);
      EXCEPT(e);
    }
    return 0;
  default:
    return 0;
  }

  return 0;
}

static error step_type_deffields(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  error e;
  switch (node->which) {
  case DEFCHOICE:
  case DEFFIELD:
    e = morningtypepass(mod, node);
    EXCEPT(e);
    return 0;
  default:
    return 0;
  }

  return 0;
}

static error unify(struct module *mod, const struct node *for_error,
                   struct typ *a, struct typ *b);

static error step_type_defchoices(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  error e;
  switch (node->which) {
  case DEFTYPE:
    switch (node->as.DEFTYPE.kind) {
    case DEFTYPE_ENUM:
    case DEFTYPE_SUM:
      {
        struct typ *u = typ_create_tentative(TBI_LITERALS_INTEGER);
        for (size_t n = 0; n < node->subs_count; ++n) {
          struct node *ch = node->subs[n];
          if (ch->which != DEFCHOICE) {
            continue;
          }

          e = unify(mod, ch, u, ch->subs[IDX_CH_VALUE]->typ);
          EXCEPT(e);

          ch->flags |= NODE_IS_DEFCHOICE;
        }

        if (typ_equal(u, TBI_LITERALS_INTEGER)) {
          e = unify(mod, node, u, TBI_U32);
          EXCEPT(e);
        }

        set_typ(&node->as.DEFTYPE.choice_typ, u);
      }
      break;
    default:
      break;
    }
    break;
  default:
    break;
  }
  return 0;
}

static error step_type_deffuns(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  error e;
  switch (node->which) {
  case DEFMETHOD:
  case DEFFUN:
    e = morningtypepass(mod, node);
    EXCEPT(e);
    return 0;
  case DEFTYPE:
  case DEFINTF:
    break;
  default:
    return 0;
  }

  return 0;
}

static error step_type_gather_retval(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    module_retval_set(mod, node_fun_retval(node));
    break;
  default:
    break;
  }
  return 0;
}

// FIXME: This is O(depth * number_throw_except).
// Would be O(number_throw_except) if we remembered whether we're in the TRY
// or in one of the CATCH, when descending.
static error check_in_try(struct module *mod, struct node *node, const char *which) {
  error e;

  struct try_state *st = module_excepts_get(mod);
  if (st == NULL) {
    goto fail;
  }

  const struct node *p = node;
  do {
    p = node_parent_const(p);
    if (p->which == CATCH) {
      goto fail;
    }
  } while (p->which != TRY);

  goto ok;

fail:
  e = mk_except(mod, node, "%s not in try block", which);
  THROW(e);

ok:
  return 0;
}

static error step_type_gather_excepts(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  error e;
  switch (node->which) {
  case TRY:
    module_excepts_open_try(mod, node);
    break;
  case DEFNAME:
    if (node->as.DEFNAME.is_excep) {
      e = check_in_try(mod, node, "except");
      EXCEPT(e);
      module_excepts_push(mod, node);
    }
    break;
  case THROW:
    e = check_in_try(mod, node, "throw");
    EXCEPT(e);
    module_excepts_push(mod, node);
    break;
  default:
    break;
  }
  return 0;
}

static error step_excepts_store_label(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *label_ident = NULL;

  const char *which = NULL;
  switch (node->which) {
  case DEFNAME:
    if (node->as.DEFNAME.is_excep) {
      which = "except";
      label_ident = node->as.DEFNAME.excep_label_ident;
      break;
    }
    return 0;
  case THROW:
    which = "throw";
    if (node->subs_count == 2) {
      label_ident = node->subs[0];
    }
    break;
  default:
    return 0;
  }

  struct node *tryy = module_excepts_get(mod)->tryy;
  struct node *eblock = tryy->subs[0]->subs[1];

  ident label = ID__NONE;
  error e;

  if (label_ident == NULL) {
    if (eblock->subs_count != 2) {
      e = mk_except(mod, node,
                    "try block has multiple catch,"
                    " %s must use a label", which);
      THROW(e);
    }

    assert(eblock->subs[1]->which == CATCH);
    label = eblock->subs[1]->as.CATCH.label;

  } else {
    if (eblock->subs_count == 2) {
      assert(eblock->subs[1]->which == CATCH);
      if (!eblock->subs[1]->as.CATCH.is_user_label) {
        e = mk_except(mod, node,
                      "try block has a single catch without a label,"
                      " %s must not use a label",
                      which);
        THROW(e);
      }
    }

    struct node *def = NULL;
    e = scope_lookup(&def, mod, node->scope, label_ident);
    EXCEPT(e);

    if (def->which != CATCH || def->scope->parent->node != eblock) {
      e = mk_except(mod, label_ident,
                    "invalid label '%s'",
                    idents_value(mod->gctx, node_ident(label_ident)));
      THROW(e);
    }

    label = node_ident(label_ident);
  }

  switch (node->which) {
  case DEFNAME:
    node->as.DEFNAME.excep_label = label;
    node->as.DEFNAME.excep_error = tryy->as.TRY.error;
    break;
  case THROW:
    node->as.THROW.label = label;
    node->as.THROW.error = tryy->as.TRY.error;
    break;
  default:
    assert(FALSE);
  }

  return 0;
}

static error step_rewrite_defname_no_expr(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFNAME) {
    return 0;
  }

  return 0;
}

static error step_rewrite_sum_constructors(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != CALL) {
    return 0;
  }

  struct node *fun = node->subs[0];
  struct node *dfun = typ_definition(fun->typ);
  if (dfun->which != DEFTYPE
      || dfun->as.DEFTYPE.kind != DEFTYPE_SUM) {
    return 0;
  }

  struct node *member = NULL;
  error e = scope_lookup(&member, mod, fun->scope, fun);
  EXCEPT(e);
  if (member->which != DEFCHOICE) {
    return 0;
  }

  struct node *mk_fun = mk_node(mod, node, BIN);
  mk_fun->as.BIN.operator = TDOT;
  rew_append(mk_fun, fun);
  struct node *mk = mk_node(mod, mk_fun, IDENT);
  mk->as.IDENT.name = ID_MK;

  rew_move_last_over(node, 0, TRUE);

  const struct node *except[] = { fun, NULL };
  e = catchup(mod, except, mk_fun, node->scope, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  return 0;
}

static error do_instantiate(struct typ **result,
                            struct module *mod, struct typ *t,
                            struct typ **explicit_args, size_t arity,
                            bool tentative) {
  assert(arity == 0 || arity == typ_generic_arity(t));

  struct node *gendef = typ_definition(t);
  struct node *pristine = node_toplevel(gendef)->instances[0];
  struct node *instance = add_instance_deepcopy_from_pristine(mod, gendef,
                                                              pristine, tentative);
  set_typ(&node_toplevel(instance)->our_generic_functor_typ, t);

  struct node *genargs = instance->subs[IDX_GENARGS];
  const size_t first = typ_generic_first_explicit_arg(t);
  for (size_t n = 0; n < first; ++n) {
    struct node *ga = genargs->subs[n];
    ga->which = SETGENARG;
    // FIXME leaking ga->subs[1]
    ga->subs[1]->which = DIRECTDEF;
    set_typ(&ga->subs[1]->as.DIRECTDEF.typ,
            typ_create_tentative(typ_generic_arg(t, n)));
    ga->subs[1]->as.DIRECTDEF.flags = NODE_IS_TYPE;
  }

  for (size_t n = 0; n < arity; ++n) {
    struct node *ga = genargs->subs[first + n];
    ga->which = SETGENARG;
    // FIXME leaking ga->subs[1]
    ga->subs[1]->which = DIRECTDEF;
    set_typ(&ga->subs[1]->as.DIRECTDEF.typ, explicit_args[n]);
    ga->subs[1]->as.DIRECTDEF.flags = NODE_IS_TYPE;
  }

  error e = catchup_instantiation(mod, node_module_owner(gendef),
                                  instance, gendef->scope->parent,
                                  tentative);
  EXCEPT(e);

  *result = instance->typ;
  return 0;
}

static struct typ *find_existing_instance(struct module *mod,
                                          struct typ *t,
                                          struct typ **args,
                                          size_t arity) {
  const size_t first = typ_generic_first_explicit_arg(t);
  assert(typ_generic_arity(t) - first == arity);

  const struct node *d = typ_definition_const(t);
  const struct toplevel *toplevel = node_toplevel_const(d);
  for (size_t n = 1; n < toplevel->instances_count; ++n) {
    struct typ *i = toplevel->instances[n]->typ;

    size_t a;
    for (a = first; a < arity; ++a) {
      if (!typ_equal(typ_generic_arg_const(i, a), args[a - first])) {
        break;
      }
    }

    if (a == arity) {
      return i;
    }
  }

  return NULL;
}

// Same as find_existing_instance(), but with different arguments.
static struct typ *find_existing_instance_for_tentative(struct module *mod,
                                                        const struct typ *t) {
  const struct node *d = typ_definition_const(typ_generic_functor_const(t));

  const struct toplevel *toplevel = node_toplevel_const(d);
  for (size_t n = 1; n < toplevel->instances_count; ++n) {
    struct typ *i = toplevel->instances[n]->typ;

    if (typ_equal(t, i)) {
      return i;
    }
  }

  return NULL;
}

static bool is_tentative(const struct module *mod,
                         struct typ *t, struct typ **args, size_t arity) {
  if (mod->state->tentatively || mod->state->fun_state != NULL) {
    if (typ_is_tentative(t)) {
      return TRUE;
    }

    for (size_t n = 0; n < arity; ++n) {
      if (typ_is_tentative(args[n])) {
        return TRUE;
      }
    }

    // Optimisation: immediately turn into a final instantiation.
    return FALSE;
  } else {
    return FALSE;
  }
}

static error instance(struct typ **result,
                      struct module *mod,
                      struct node *for_error, size_t for_error_offset,
                      struct typ *t, struct typ **explicit_args, size_t arity) {
  const size_t first = typ_generic_first_explicit_arg(t);
  assert(arity == typ_generic_arity(t) - first);

  const bool tentative = is_tentative(mod, t, explicit_args, arity);
  struct typ *r = NULL;
  if (tentative) {
    struct typ **args = calloc(arity, sizeof(struct typ *));
    for (size_t n = 0; n < arity; ++n) {
      args[n] = typ_create_tentative(typ_generic_arg(t, n));
    }

    error e = do_instantiate(&r, mod, t, args, arity, TRUE);
    EXCEPT(e);

    for (size_t n = 0; n < arity; ++n) {
      e = unify(mod, for_error->subs[for_error_offset + n],
                typ_generic_arg(r, first + n),
                explicit_args[n]);
      EXCEPT(e);
    }

    free(args);
  } else {
    r = find_existing_instance(mod, t, explicit_args, arity);
    if (r == NULL) {
      error e = do_instantiate(&r, mod, t, explicit_args, arity, FALSE);
      EXCEPT(e);
    }
  }

  *result = r;

  return 0;
}

static error mk_except_type_unification(struct module *mod, const struct node *for_error,
                                        const struct typ *a, const struct typ *b) {
  char *sa = typ_pretty_name(mod, a);
  char *sb = typ_pretty_name(mod, b);
  error e = mk_except_type(mod, for_error,
                           "types '%s' and '%s' cannot be unified", sa, sb);
  free(sb);
  free(sa);
  THROW(e);
}

static error unify_two_non_generic(struct module *mod, const struct node *for_error,
                                   struct typ *a, struct typ *b) {
  if (typ_isa(a, b)) {
    // noop
  } else if (typ_isa(b, a)) {
    SWAP(a, b);
  } else if (typ_definition(a)->which == DEFINTF
             && typ_definition(b)->which == DEFINTF) {
    assert(FALSE && "FIXME Unsupported (e.g. i_arithmethic and i_bitwise)");
  } else {
    error e = mk_except_type_unification(mod, for_error, a, b);
    THROW(e);
  }

  typ_link_tentative(a, b);

  return 0;
}

static bool same_generic_functor(const struct module *mod,
                                 const struct typ *a, const struct typ *b) {
  if (typ_generic_arity(a) > 0 && typ_generic_arity(b) > 0) {
    return typ_equal(typ_generic_functor_const(a), typ_generic_functor_const(b));
  } else if (typ_generic_arity(a) == 0 && typ_generic_arity(b) > 0) {
    return typ_equal(a, typ_generic_functor_const(b));
  } else if (typ_generic_arity(a) > 0 && typ_generic_arity(b) == 0) {
    return typ_equal(typ_generic_functor_const(a), b);
  } else {
    return FALSE;
  }
}

static error unify_same_generic_functor(struct module *mod, const struct node *for_error,
                                        struct typ *a, struct typ *b) {
  assert(typ_generic_arity(a) != 0 || typ_generic_arity(b) != 0);

  if (typ_generic_arity(a) == 0 || typ_generic_arity(b) == 0) {
    error e = mk_except_type_unification(mod, for_error, a, b);
    THROW(e);
  }

  if (typ_equal(a, b)) {
    typ_link_tentative(a, b);

    return 0;
  }

  assert(typ_generic_arity(a) == typ_generic_arity(b));
  const size_t arity = typ_generic_arity(a);
  for (size_t n = 0; n < arity; ++n) {
    struct typ *arga = typ_generic_arg(a, n);
    struct typ *argb = typ_generic_arg(b, n);

    if (typ_isa(arga, argb)) {
      typ_link_tentative(arga, argb);
    } else if (typ_isa(argb, arga)) {
      typ_link_tentative(argb, arga);
    } else {
      error e = mk_except_type_unification(mod, for_error, a, b);
      THROW(e);
    }
  }

  return 0;
}

static error unify_non_generic(struct module *mod, const struct node *for_error,
                               struct typ *a, struct typ *b,
                               bool a_non_generic, bool b_non_generic) {
  error e;

  if (a_non_generic && b_non_generic) {
    e = unify_two_non_generic(mod, for_error, a, b);
    EXCEPT(e);
    return 0;
  }

  if (same_generic_functor(mod, a, b)) {
    e = unify_same_generic_functor(mod, for_error, a, b);
    EXCEPT(e);
    return 0;
  }

  if (a_non_generic) {
    SWAP(a, b);
    SWAP(a_non_generic, b_non_generic);
  }

  e = typ_check_isa(mod, for_error, a, b);
  EXCEPT(e);

  typ_link_tentative(a, b);

  return 0;
}

static error unify_literal(struct module *mod, const struct node *for_error,
                           struct typ *a, struct typ *b,
                           bool a_literal, bool b_literal) {
  error e;
  if (a_literal && b_literal) {
    bool a_floating = typ_equal(a, TBI_LITERALS_FLOATING);
    bool b_floating = typ_equal(b, TBI_LITERALS_FLOATING);
    if (a_floating || b_floating) {
      if (!a_floating && b_floating) {
        SWAP(a, b);
        SWAP(a_floating, b_floating);
      }
      if (!a_floating) {
        if (!typ_equal(a, TBI_LITERALS_INTEGER)) {
          e = mk_except_type_unification(mod, for_error, a, b);
          THROW(e);
        }
      }
    } else {
      e = typ_check_equal(mod, for_error, a, b);
      EXCEPT(e);
    }

    typ_link_tentative(a, b);

    return 0;
  }

  if (a_literal) {
    SWAP(a, b);
  }

  if (typ_equal(b, TBI_LITERALS_NULL)) {
    if (typ_generic_functor(a) != NULL
        && typ_equal(typ_generic_functor(a), TBI__REF_COMPATIBLE)) {
      struct typ *real_a = typ_generic_arg(a, 0);
      e = unify(mod, for_error, real_a, b);
      EXCEPT(e);
      return 0;
    } else {
      e = typ_check_isa(mod, for_error, a, TBI_ANY_NULLABLE_REF);
      EXCEPT(e);
    }
  } else if (typ_equal(b, TBI_LITERALS_INTEGER)) {
    if (typ_isa(TBI_INTEGER, a)) {
      SWAP(a, b);
    } else {
      e = typ_check_isa(mod, for_error, a, TBI_INTEGER);
      EXCEPT(e);
    }
  } else if (typ_equal(b, TBI_LITERALS_FLOATING)) {
    if (typ_isa(TBI_FLOATING, a)) {
      SWAP(a, b);
    } else {
      e = typ_check_isa(mod, for_error, a, TBI_FLOATING);
      EXCEPT(e);
    }
  } else {
    assert(FALSE);
  }

  typ_link_tentative(a, b);

  return 0;
}

static bool typ_check_compat_weakly_concrete(struct module *mod,
                                             const struct typ *a,
                                             const struct typ *weak) {
  if (typ_equal(weak, TBI_BOOL)) {
    return typ_isa(a, TBI_BOOL_COMPATIBLE);
  } else if (typ_equal(weak, TBI_STATIC_STRING)) {
    return typ_isa(a, TBI_STATIC_STRING_COMPATIBLE);
  } else {
    assert(FALSE && "Unreached.");
  }
}

static error unify_with_weakly_concrete(bool *success,
                                        struct module *mod,
                                        const struct node *for_error,
                                        struct typ *a, struct typ *b,
                                        bool a_weakly_concrete,
                                        bool b_weakly_concrete) {
  if (a_weakly_concrete && !b_weakly_concrete) {
    SWAP(a, b);
  }

  *success = typ_check_compat_weakly_concrete(mod, a, b);
  if (*success) {
    typ_link_tentative(a, b);
  }

  return 0;
}

static void insert_defnamedliterals(struct module *mod,
                                    struct node *littype_body,
                                    struct node *a_body) {
  for (size_t n = 0; n < a_body->subs_count; ++n) {
    struct node *of = a_body->subs[n];

    struct node *f = mk_node(mod, littype_body, DEFFIELD);
    struct node *name = mk_node(mod, f, IDENT);
    name->as.IDENT.name = node_ident(of->subs[0]);
    struct node *t = mk_node(mod, f, DIRECTDEF);
    set_typ(&t->as.DIRECTDEF.typ, of->subs[1]->typ);
    t->as.DIRECTDEF.flags = NODE_IS_TYPE;
  }
}

static struct typ *merge_defnamedliterals(struct module *mod,
                                          struct node *a_body, struct node *b_body) {
  // FIXME: Detached node, would have to be freed when releasing the
  // mod fun_state in which it is recorded below.
  //
  struct node *littype = calloc(1, sizeof(struct node));
  littype->which = DEFNAMEDLITERAL;
  struct node *littype_name = mk_node(mod, littype, IDENT);
  littype_name->as.IDENT.name = gensym(mod);
  struct node *littype_body = mk_node(mod, littype, BLOCK);

  insert_defnamedliterals(mod, littype_body, a_body);
  insert_defnamedliterals(mod, littype_body, b_body);

  const size_t arity = littype_body->subs_count;
  struct typ **args = calloc(arity, sizeof(struct typ *));
  for (size_t n = 0; n < littype_body->subs_count; ++n) {
    const struct node *t = littype_body->subs[n]->subs[1];
    assert(t->which == DIRECTDEF);
    args[n] = t->as.DIRECTDEF.typ;
  }

  const bool tentative = is_tentative(mod, littype->typ, args, arity);
  free(args);
  error e = catchup_instantiation(mod, mod,
                                  littype, mod->body->scope,
                                  tentative);
  assert(!e);

  return littype->typ;
}

HTABLE_SPARSE(ident_typ_map, struct typ *, ident);
implement_htable_sparse(__attribute__((unused)) static, ident_typ_map, struct typ *, ident);

static error unify_two_defnamedliterals(struct module *mod, const struct node *for_error,
                                        struct typ *a, struct typ *b) {
  struct ident_typ_map map;
  ident_typ_map_init(&map, 0);
  ident_typ_map_set_delete_val(&map, FALSE);
  ident_typ_map_set_custom_hashf(&map, ident_hash);
  ident_typ_map_set_custom_cmpf(&map, ident_cmp);

  struct node *a_body = typ_definition(a)->subs[1];
  for (size_t n = 0; n < a_body->subs_count; ++n) {
    struct node *f = a_body->subs[n];
    ident_typ_map_set(&map, node_ident(f->subs[0]), f->subs[1]->typ);
  }

  struct node *b_body = typ_definition(b)->subs[1];
  for (size_t n = 0; n < b_body->subs_count; ++n) {
    struct node *f = b_body->subs[n];
    struct typ **existing = ident_typ_map_get(&map, node_ident(f->subs[0]));
    if (existing != NULL) {
      error e = unify(mod, for_error, *existing, f->subs[1]->typ);
      EXCEPT(e);
    }
  }

  struct typ *t = merge_defnamedliterals(mod, a_body, b_body);
  typ_link_tentative(t, a);
  typ_link_tentative(t, b);

  ident_typ_map_destroy(&map);

  return 0;
}

static error unify_with_defnamedliteral(struct module *mod, const struct node *for_error,
                                         struct typ *a, struct typ *dnl) {
  error e;

  struct node *a_def = typ_definition(a);
  struct node *ddnl = typ_definition(dnl);
  struct node *dnl_body = ddnl->subs[ddnl->subs_count - 1];
  for (size_t n = 0; n < dnl_body->subs_count; ++n) {
    struct node *f = dnl_body->subs[n];
    ident f_name = node_ident(f);

    struct node *d = NULL;
    e = scope_lookup_ident_immediate(&d, a_def, mod, a_def->scope, f_name, FALSE);
    EXCEPT(e);

    e = unify(mod, for_error, f->typ, d->typ);
    EXCEPT(e);
  }

  typ_link_tentative(a, dnl);

  return 0;
}

static error unify_defnamedliterals(struct module *mod, const struct node *for_error,
                                    struct typ *a, struct typ *b,
                                    bool a_dnl, bool b_dnl) {
  error e;
  if (a_dnl && b_dnl) {
    e = unify_two_defnamedliterals(mod, for_error, a, b);
    EXCEPT(e);

    return 0;
  }

  if (a_dnl && !b_dnl) {
    SWAP(a, b);
    SWAP(a_dnl, b_dnl);
  }

  e = unify_with_defnamedliteral(mod, for_error, a, b);
  EXCEPT(e);

  return 0;
}

static error unify_with_equal(struct module *mod, const struct node *for_error,
                              struct typ *a, struct typ *b) {
  error e = typ_check_equal(mod, for_error, a, b);
  EXCEPT(e);

  if (!typ_is_tentative(b)) {
    SWAP(b, a);
  }

  if (typ_is_tentative(b)) {
    typ_link_tentative(a, b);
  } else {
    assert(a == b);
  }

  return 0;
}

static error check_reference_compatibility(struct module *mod,
                                           const struct node *for_error,
                                           const struct typ *a,
                                           const struct typ *target) {
  if (typ_equal(target, TBI_ANY_ANY_REF)) {
    return 0;
  }

  const struct typ *a0 = typ_generic_functor_const(a);
  const struct typ *target0 = typ_generic_functor_const(target);

  bool ok = FALSE;
  error e;
  if (typ_equal(target0, TBI_ANY_REF)) {
    ok = typ_isa(a0, TBI_ANY_REF);
  } else if (typ_equal(target0, TBI_ANY_MUTABLE_REF)) {
    ok = typ_isa(a0, TBI_ANY_MUTABLE_REF);
  } else if (typ_equal(target0, TBI_ANY_NULLABLE_REF)) {
    ok = typ_isa(a0, TBI_ANY_REF);
  } else if (typ_equal(target0, TBI_ANY_NULLABLE_MUTABLE_REF)) {
    ok = typ_isa(a0, TBI_ANY_MUTABLE_REF);

  } else if (typ_equal(target0, TBI_REF)) {
    ok = typ_isa(a0, TBI_ANY_REF);
  } else if (typ_equal(target0, TBI_MREF)) {
    ok = typ_isa(a0, TBI_ANY_MUTABLE_REF);
  } else if (typ_equal(target0, TBI_MMREF)) {
    ok = typ_equal(a0, TBI_MMREF);
  } else if (typ_equal(target0, TBI_NREF)) {
    ok = typ_isa(a0, TBI_ANY_NULLABLE_REF)
      || typ_isa(a0, TBI_ANY_REF);
  } else if (typ_equal(target0, TBI_NMREF)) {
    ok = typ_isa(a0, TBI_ANY_NULLABLE_MUTABLE_REF)
      || typ_isa(a0, TBI_ANY_MUTABLE_REF);
  } else if (typ_equal(target0, TBI_NMMREF)) {
    ok = typ_equal(a0, TBI_NMMREF)
      || typ_equal(a0, TBI_MMREF);
  }

  if (!ok) {
    e = mk_except_type_unification(mod, for_error, a, target);
    THROW(e);
  }

  return 0;
}

static error unify_with_reference_compatible(struct module *mod,
                                             const struct node *for_error,
                                             struct typ *a, struct typ *b,
                                             bool a_ref_compatible,
                                             bool b_ref_compatible) {
  if (a_ref_compatible) {
    SWAP(a, b);
    SWAP(a_ref_compatible, b_ref_compatible);
  }

  struct typ *real_b = typ_generic_arg(b, 0);

  error e = check_reference_compatibility(mod, for_error, a, real_b);
  EXCEPT(e);

  struct typ *real_b0 = typ_generic_functor(real_b);
  if (typ_definition_const(real_b0)->which == DEFINTF
      && typ_is_tentative(real_b0)) {
    typ_link_tentative(typ_generic_functor(a), real_b0);
  }

  return 0;
}

static error unify_reference(struct module *mod, const struct node *for_error,
                             struct typ *a, struct typ *b,
                             bool a_ref, bool b_ref) {
  error e;

  if (!a_ref || !b_ref) {
    e = mk_except_type_unification(mod, for_error, a, b);
    THROW(e);
  }

  if (typ_equal(a, TBI_ANY_ANY_REF)) {
    typ_link_tentative(b, a);
    return 0;
  }

  if (typ_equal(b, TBI_ANY_ANY_REF)) {
    typ_link_tentative(a, b);
    return 0;
  }

  struct typ *a0 = typ_generic_functor(a);
  struct typ *b0 = typ_generic_functor(b);

  const bool a_ref_compatible = typ_equal(a0, TBI__REF_COMPATIBLE);
  const bool b_ref_compatible = typ_equal(b0, TBI__REF_COMPATIBLE);
  if (a_ref_compatible || b_ref_compatible) {
    e = unify_with_reference_compatible(mod, for_error, a, b,
                                        a_ref_compatible, b_ref_compatible);
    EXCEPT(e);
    return 0;
  }

  if (typ_isa(b0, a0)) {
    SWAP(a, b);
    SWAP(a0, b0);
    SWAP(a_ref, b_ref);
  }

  if (!typ_isa(a0, b0)) {
    e = mk_except_type_unification(mod, for_error, a, b);
    THROW(e);
  }

  if (typ_definition_const(b0)->which == DEFINTF && typ_is_tentative(b0)) {
    typ_link_tentative(a0, b0);
  }

  if (typ_equal(a, b)) {
    e = unify_with_equal(mod, for_error, a, b);
    EXCEPT(e);
  } else {
    if (!typ_is_tentative(b)) {
      SWAP(a, b);
      SWAP(a0, b0);
      SWAP(a_ref, b_ref);
    }

    typ_link_tentative(a, b);
  }

  return 0;
}

//static error typ_ref(struct typ **result,
//                     struct module *mod, struct node *for_error,
//                     enum token_type op, struct typ *typ);
//
//EXAMPLE_NCC(unify_with_reference_literal) {
//  struct node *foo = mock_deffun(mod, "foo");
//  struct node *body = foo->subs[foo->subs_count-1];
//  mock_parse(mod, body, "return (@1).");
//
//  G(intlit, body, NUMBER);
//  intlit->as.NUMBER.value = "1";
//  intlit->typ = typ_create_tentative(TBI_LITERALS_INTEGER);
//  struct node *for_error = intlit;
//
//  struct typ *rintlit = typ_create(NULL);
//  error e = typ_ref(&rintlit, mod, for_error, TREFDOT, intlit->typ);
//  assert(!e);
//  assert(typ_is_tentative(rintlit));
//
//  struct typ *drintlit = typ_generic_arg(rintlit, 0);
//  assert(typ_equal(drintlit, TBI_LITERALS_INTEGER));
//  assert(typ_is_tentative(drintlit));
//
//  struct typ *i32 = TBI_I32;
//  struct typ *ri32 = typ_create(NULL);
//  e = typ_ref(&ri32, mod, for_error, TREFDOT, i32);
//  assert(!e);
//  assert(typ_is_tentative(ri32));
//
//  assert(typ_equal(ri32, rintlit));
//}

static error unify_with_any(struct module *mod, const struct node *for_error,
                            struct typ *a, struct typ *b,
                            bool a_is_any, bool b_is_any) {
  if (a_is_any) {
    SWAP(a, b);
    SWAP(a_is_any, b_is_any);
  }

  typ_link_tentative(a, b);

  return 0;
}

struct instance_of {
  struct typ *functor;
  struct typ *result;
};

static error find_instance_of(struct module *mod, struct typ *t,
                              struct typ *intf, bool *stop, void *user) {
  struct instance_of *r = user;

  struct typ *intf0 = typ_generic_functor(intf);
  if (intf0 != NULL && typ_equal(intf0, r->functor)) {
    r->result = intf;
    *stop = TRUE;
  }

  return 0;
}

static error unify(struct module *mod, const struct node *for_error,
                   struct typ *a, struct typ *b) {
  error e;

  bool a_tentative = typ_is_tentative(a);
  bool b_tentative = typ_is_tentative(b);

  if (!a_tentative && !b_tentative) {
    e = typ_check_equal(mod, for_error, a, b);
    EXCEPT(e);
    return 0;
  }

  if (a_tentative && !b_tentative) {
    SWAP(a, b);
    SWAP(a_tentative, b_tentative);
  }

  if (typ_equal(a, b)) {
    e = unify_with_equal(mod, for_error, a, b);
    EXCEPT(e);
    return 0;
  }

  bool a_is_any = typ_equal(a, TBI_ANY);
  bool b_is_any = typ_equal(b, TBI_ANY);
  if (a_is_any || b_is_any) {
    unify_with_any(mod, for_error, a, b, a_is_any, b_is_any);
    return 0;
  }

  const bool a_dnl = typ_definition(a)->which == DEFNAMEDLITERAL;
  const bool b_dnl = typ_definition(b)->which == DEFNAMEDLITERAL;
  if (a_dnl || b_dnl) {
    e = unify_defnamedliterals(mod, for_error, a, b, a_dnl, b_dnl);
    EXCEPT(e);
    return 0;
  }

  const bool a_literal = typ_is_literal(a);
  const bool b_literal = typ_is_literal(b);
  if (a_literal || b_literal) {
    e = unify_literal(mod, for_error, a, b, a_literal, b_literal);
    EXCEPT(e);
    return 0;
  }

  const bool a_ref = typ_is_reference(a);
  const bool b_ref = typ_is_reference(b);
  if (a_ref || b_ref) {
    e = unify_reference(mod, for_error, a, b, a_ref, b_ref);
    EXCEPT(e);
    return 0;
  }

  const bool a_weakly_concrete = typ_is_weakly_concrete(a);
  const bool b_weakly_concrete = typ_is_weakly_concrete(b);
  if (a_weakly_concrete || b_weakly_concrete) {
    bool success = FALSE;
    e = unify_with_weakly_concrete(&success,
                                   mod, for_error, a, b,
                                   a_weakly_concrete, b_weakly_concrete);
    EXCEPT(e);

    if (success) {
      return 0;
    }
  }

  const bool a_non_generic = typ_generic_arity(a) == 0;
  const bool b_non_generic = typ_generic_arity(b) == 0;
  if (a_non_generic || b_non_generic) {
    e = unify_non_generic(mod, for_error, a, b, a_non_generic, b_non_generic);
    EXCEPT(e);
    return 0;
  }

  struct typ *a0 = typ_generic_functor(a);
  struct typ *b0 = typ_generic_functor(b);

  if (typ_isa(a0, b0)) {
    // noop
  } else if (typ_isa(b0, a0)) {
    SWAP(a, b);
    SWAP(a0, b0);
  } else if (typ_definition(a)->which == DEFINTF
             && typ_definition(b)->which == DEFINTF) {
    assert(FALSE && "FIXME Unsupported (e.g. combining constraints i_arithmethic and i_bitwise)");
  } else {
    e = mk_except_type_unification(mod, for_error, a, b);
    THROW(e);
  }

  if (!typ_equal(b, TBI_ANY)) {
    struct typ *b_in_a = NULL;

    if (typ_equal(a0, b0)) {
      b_in_a = a;
    } else {
      struct instance_of user = { .functor = b0, .result = NULL };
      typ_isalist_foreach(mod, a, 0, find_instance_of, &user);
      assert(user.result != NULL);
      b_in_a = user.result;
    }
    assert(b_in_a != b && "FIXME What does that mean?");

    e = unify_same_generic_functor(mod, for_error, b_in_a, b);
    EXCEPT(e);
  }

  typ_link_tentative(a, b);

  return 0;
}

EXAMPLE_NCC(unify) {
//  struct node *for_error = calloc(1, sizeof(struct node));
//  error e;
//  {
//    struct typ *a = TBI_I32;
//    struct typ *b = NULL;
//    set_typ(&b, typ_create_tentative(TBI_INTEGER));
//    e = unify(mod, for_error, a, b);
//    assert(!e);
//  }
//  {
//    struct typ *a = TBI_I32;
//    struct typ *b = TBI_U32;
//    e = unify(mod, for_error, a, b);
//    assert(e);
//  }
}

static error typ_ref(struct typ **result,
                     struct module *mod, struct node *for_error,
                     enum token_type op, struct typ *typ) {
  error e = instance(result, mod, for_error, 0,
                     mod->gctx->builtin_typs_for_refop[op], &typ, 1);
  EXCEPT(e);

  return 0;
}

static error type_inference_un(struct module *mod, struct node *node) {
  assert(node->which == UN);
  error e;
  const enum token_type operator = node->as.UN.operator;
  struct node *term = node->subs[0];

  switch (OP_KIND(operator)) {
  case OP_UN_REFOF:
    // FIXME: it's not OK to take a mutable reference of:
    //   fun foo p:@t = void
    //     let mut = @!(p.)
    e = typ_ref(&node->typ, mod, node, operator, term->typ);
    EXCEPT(e);
    node->flags |= term->flags & NODE__TRANSITIVE;
    break;
  case OP_UN_DEREF:
    e = typ_check_can_deref(mod, term, term->typ, operator);
    EXCEPT(e);
    e = typ_check_deref_against_mark(mod, node, node->typ, operator);
    EXCEPT(e);
    set_typ(&node->typ, typ_generic_arg(term->typ, 0));
    node->flags |= term->flags & NODE__TRANSITIVE;
    break;
  case OP_UN_BOOL:
    set_typ(&node->typ, typ_create_tentative(TBI_BOOL));
    e = unify(mod, node, node->typ, term->typ);
    EXCEPT(e);
    break;
  case OP_UN_NUM:
    set_typ(&node->typ, typ_create_tentative(TBI_ARITHMETIC));
    e = unify(mod, node, node->typ, term->typ);
    EXCEPT(e);
    break;
  default:
    assert(FALSE);
  }

  return 0;
}

// for_error and for_error_offset are not actually used, as this call should
// never fail, but we need something to pass down.
static struct typ *try_wrap_ref_compatible(struct module *mod,
                                           struct node *for_error,
                                           size_t for_error_offset,
                                           struct typ *t) {
  if (!typ_is_reference(t)) {
    return t;
  }

  struct typ *r = NULL;
  error e = instance(&r, mod, for_error, for_error_offset,
                     TBI__REF_COMPATIBLE, &t, 1);
  assert(!e);

  return typ_create_tentative(r);
}

static error check_assign_not_types(struct module *mod, struct node *left,
                                    struct node *right) {
  error e;
  if ((left->flags & NODE_IS_TYPE)) {
    e = mk_except_type(mod, left, "cannot assign to a type variable");
    THROW(e);
  }
  if ((right->flags & NODE_IS_TYPE)) {
    e = mk_except_type(mod, right, "cannot assign a type");
    THROW(e);
  }
  return 0;
}

static error type_inference_bin_sym(struct module *mod, struct node *node) {
  assert(node->which == BIN);

  struct node *left = node->subs[0];
  struct node *right = node->subs[1];
  const enum token_type operator = node->as.BIN.operator;

  error e;
  if (operator == TASSIGN) {
    e = check_assign_not_types(mod, left, right);
    EXCEPT(e);

    e = unify(mod, node,
              try_wrap_ref_compatible(mod, node, 0, left->typ), right->typ);
    EXCEPT(e);
  } else {
    e = unify(mod, node, left->typ, right->typ);
    EXCEPT(e);
  }

  switch (OP_KIND(operator)) {
  case OP_BIN_SYM_BOOL:
    set_typ(&node->typ, left->typ);
    break;
  case OP_BIN_SYM_NUM:
    switch (operator) {
    case TPLUS_ASSIGN:
    case TMINUS_ASSIGN:
    case TTIMES_ASSIGN:
    case TDIVIDE_ASSIGN:
    case TMODULO_ASSIGN:
    case TBWAND_ASSIGN:
    case TBWOR_ASSIGN:
    case TBWXOR_ASSIGN:
    case TRSHIFT_ASSIGN:
    case TLSHIFT_ASSIGN:
      e = check_assign_not_types(mod, left, right);
      EXCEPT(e);

      e = typ_check_isa(mod, node, left->typ, TBI_ARITHMETIC);
      EXCEPT(e);

      set_typ(&node->typ, TBI_VOID);
      left->flags |= right->flags & NODE__TRANSITIVE;
      break;
    default:
      set_typ(&node->typ, typ_create_tentative(TBI_ARITHMETIC));
      e = unify(mod, node, node->typ, left->typ);
      EXCEPT(e);
      break;
    }
    break;
  case OP_BIN_SYM_PTR:
    e = typ_check_isa(mod, node, left->typ, TBI_ANY_ANY_REF);
    EXCEPT(e);
    e = typ_check_isa(mod, node, right->typ, TBI_ANY_ANY_REF);
    EXCEPT(e);
    set_typ(&node->typ, TBI_BOOL);
    break;
  case OP_BIN_SYM:
    switch (operator) {
    case TLE:
    case TLT:
    case TGT:
    case TGE:
    case TEQ:
    case TNE:
      if (typ_equal(left->typ, TBI_BOOL)) {
        // We want to propagate the link status of the terms when used as a
        // weakly concrete.
        set_typ(&node->typ, left->typ);
      } else {
        set_typ(&node->typ, TBI_BOOL);
      }
      break;
    default:
      set_typ(&node->typ, TBI_VOID);
      break;
    }
    break;
  default:
    set_typ(&node->typ, TBI_VOID);
    break;
  }

  return 0;
}

static void bin_accessor_maybe_ref(struct scope **parent_scope,
                                   struct module *mod, struct node *parent) {
  if (typ_is_reference(parent->typ)) {
    *parent_scope = typ_definition(typ_generic_arg(parent->typ, 0))->scope;
  }
}

static void bin_accessor_maybe_defchoice(struct scope **parent_scope, struct node *for_error,
                                         struct module *mod, struct node *parent) {
  if (parent->flags & NODE_IS_DEFCHOICE) {
    assert(parent->which == BIN);

    struct node *defchoice = NULL;
    error e = scope_lookup_ident_immediate(&defchoice, for_error, mod,
                                           typ_definition(parent->typ)->scope,
                                           node_ident(parent->subs[1]), FALSE);
    assert(!e);
    assert(defchoice->which == DEFCHOICE);

    *parent_scope = defchoice->scope;
  }
}

static error rewrite_unary_call(struct module *mod, struct node *node, struct typ *tfun) {
  struct scope *parent_scope = node->scope->parent;

  struct node *fun = calloc(1, sizeof(struct node));
  memcpy(fun, node, sizeof(*fun));
  set_typ(&fun->typ, tfun);
  fun->scope->node = fun;

  memset(node, 0, sizeof(*node));
  node->which = CALL;
  rew_append(node, fun);

  const struct node *except[] = { fun, NULL };
  error e = catchup(mod, except, node, parent_scope, CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);
  return 0;
}

static error type_inference_bin_accessor(struct module *mod, struct node *node) {
  error e;

  enum token_type operator = node->as.BIN.operator;
  const struct typ *mark = node->typ;

  struct node *container = node->subs[0];
  struct scope *container_scope = typ_definition(container->typ)->scope;
  bin_accessor_maybe_ref(&container_scope, mod, container);
  bin_accessor_maybe_defchoice(&container_scope, node, mod, container);

  struct node *for_error = node->subs[1];
  struct node *field = NULL;
  e = scope_lookup_ident_immediate(&field, for_error, mod, container_scope,
                                   node_ident(node->subs[1]), FALSE);
  EXCEPT(e);

  if (field->which == IMPORT && !field->as.IMPORT.intermediate_mark) {
    e = scope_lookup(&field, mod, mod->gctx->modules_root.scope,
                     field->subs[0]);
    assert(!e);
  }

  if (typ_is_function(field->typ) && mark != TBI__CALL_FUNCTION_SLOT) {
    if (node_fun_explicit_args_count(field) != 0) {
      e = mk_except_call_args_count(mod, node, field, 0, 0);
      THROW(e);
    }

    e = rewrite_unary_call(mod, node, field->typ);
    EXCEPT(e);
  } else {
    struct typ *t = field->typ;

    if (operator == TWILDCARD
        && typ_is_reference(field->typ)) {
      assert(typ_is_reference(field->typ));
      e = typ_ref(&t, mod, node, TREFWILDCARD,
                  typ_generic_arg(field->typ, 0));
      assert(!e);
    }

    set_typ(&node->typ, t);
    assert(field->which != BIN || field->flags != 0);
    node->flags = field->flags;
  }

  if (!(node->flags & NODE_IS_TYPE)) {
    e = typ_check_deref_against_mark(mod, node, mark, operator);
    EXCEPT(e);
  }

  return 0;
}

static error type_inference_bin_rhs_unsigned(struct module *mod, struct node *node) {
  error e;

  e = unify(mod, node->subs[1], node->subs[1]->typ, TBI_U32);
  EXCEPT(e);

  set_typ(&node->typ, typ_create_tentative(TBI_ARITHMETIC));
  e = unify(mod, node, node->subs[0]->typ, node->typ);
  EXCEPT(e);

  return 0;
}

static error type_inference_bin_rhs_type(struct module *mod, struct node *node) {
  error e;

  if (!(node->subs[1]->flags & NODE_IS_TYPE)) {
    e = mk_except_type(mod, node->subs[1], "right-hand side not a type");
    THROW(e);
  }

  e = unify(mod, node, node->subs[0]->typ, node->subs[1]->typ);
  EXCEPT(e);

  return 0;
}

static error type_inference_bin(struct module *mod, struct node *node) {
  assert(node->which == BIN);

  switch (OP_KIND(node->as.BIN.operator)) {
  case OP_BIN_SYM:
  case OP_BIN_SYM_BOOL:
  case OP_BIN_SYM_NUM:
  case OP_BIN_SYM_PTR:
    return type_inference_bin_sym(mod, node);
  case OP_BIN_NUM_RHS_UNSIGNED:
    return type_inference_bin_rhs_unsigned(mod, node);
  case OP_BIN_ACC:
    return type_inference_bin_accessor(mod, node);
  case OP_BIN_RHS_TYPE:
    return type_inference_bin_rhs_type(mod, node);
  default:
    assert(FALSE);
    return 0;
  }
}

static error typ_tuple(struct typ **result, struct module *mod, struct node *node) {
  const size_t arity = node->subs_count;
  struct typ **args = calloc(arity, sizeof(struct typ *));
  for (size_t n = 0; n < arity; ++n) {
    args[n] = node->subs[n]->typ;
  }

  error e = instance(result, mod, node, 0,
                     typ_lookup_builtin_tuple(mod, arity), args, arity);
  EXCEPT(e);

  free(args);

  return 0;
}

static error type_inference_tuple(struct module *mod, struct node *node) {
  for (size_t n = 0; n < typ_generic_arity(node->typ); ++n) {
    if (n > 0 && (node->flags & NODE_IS_TYPE) != (node->subs[n]->flags & NODE_IS_TYPE)) {
      error e = mk_except_type(mod, node->subs[n], "tuple combines values and types");
      THROW(e);
    }
    node->flags |= (node->subs[n]->flags & NODE__TRANSITIVE);
  }

  error e = typ_tuple(&node->typ, mod, node);
  EXCEPT(e);

  return 0;
}

static error type_inference_tupleextract(struct module *mod, struct node *node) {
  struct node *expr = node->subs[node->subs_count - 1];
  assert(node->subs_count == typ_generic_arity(expr->typ) + 1
         && typ_isa(expr->typ, TBI_ANY_TUPLE));

  for (size_t n = 0; n < node->subs_count - 1; ++n) {
    set_typ(&node->subs[n]->typ, typ_generic_arg(expr->typ, n));
  }

  set_typ(&node->typ, node->subs[node->subs_count - 1]->typ);
  node->flags = node->subs[node->subs_count - 1]->flags; // Copy all flags, transparent node.

  return 0;
}

static void type_inference_init_named(struct module *mod, struct node *node) {
  // FIXME: Detached node, would have to be freed when releasing the
  // mod fun_state in which it is recorded below.
  //
  struct node *littype = calloc(1, sizeof(struct node));
  littype->which = DEFNAMEDLITERAL;
  struct node *littype_name = mk_node(mod, littype, IDENT);
  littype_name->as.IDENT.name = gensym(mod);
  (void)mk_node(mod, littype, GENARGS);
  struct node *littype_body = mk_node(mod, littype, BLOCK);

  const size_t arity = node->subs_count / 2;
  struct typ **args = calloc(arity, sizeof(struct typ *));
  for (size_t n = 0; n < node->subs_count; n += 2) {
    const struct node *left = node->subs[n];
    const struct node *right = node->subs[n+1];

    struct node *f = mk_node(mod, littype_body, DEFFIELD);
    struct node *name = mk_node(mod, f, IDENT);
    name->as.IDENT.name = node_ident(left);
    struct node *t = mk_node(mod, f, DIRECTDEF);
    set_typ(&t->as.DIRECTDEF.typ, right->typ);
    t->as.DIRECTDEF.flags = NODE_IS_TYPE;

    args[n / 2] = right->typ;
  }

  const bool tentative = TRUE;
  free(args);
  error e = catchup_instantiation(mod, mod,
                                  littype, node->scope,
                                  tentative);
  assert(!e);

  set_typ(&node->typ, typ_create_tentative(littype->typ));
}

static error type_inference_init_array(struct module *mod, struct node *node) {
  struct typ *el = typ_create_tentative(typ_generic_arg(TBI_STATIC_ARRAY, 0));

  for (size_t n = 0; n < node->subs_count; n += 1) {
    error e = unify(mod, node->subs[n], node->subs[n]->typ, el);
    EXCEPT(e);
  }

  error e = instance(&node->typ, mod, node, 0,
                     TBI_STATIC_ARRAY, &el, 1);
  EXCEPT(e);

  return 0;
}

static error type_inference_init(struct module *mod, struct node *node) {
  assert(node->which == INIT);
  if (node->as.INIT.is_array) {
    return type_inference_init_array(mod, node);
  } else {
    type_inference_init_named(mod, node);
    return 0;
  }
}

static error type_inference_return(struct module *mod, struct node *node) {
  assert(node->which == RETURN);

  if (node->subs_count > 0) {
    error e = unify(mod, node->subs[0], node->subs[0]->typ,
                    try_wrap_ref_compatible(mod, node, 1,
                                            module_retval_get(mod)->typ));
    EXCEPT(e);
  }

  set_typ(&node->typ, TBI_VOID);

  return 0;
}

static enum token_type refop_for_accop[] = {
  [TDOT] = TREFDOT,
  [TBANG] = TREFBANG,
  [TSHARP] = TREFSHARP,
  [TWILDCARD] = TREFWILDCARD,
};

static enum token_type accop_for_refop[] = {
  [TREFDOT] = TDOT,
  [TREFBANG] = TBANG,
  [TREFSHARP] = TSHARP,
  [TREFWILDCARD] = TWILDCARD,
};

static enum token_type derefop_for_accop[] = {
  [TDOT] = TDEREFDOT,
  [TBANG] = TDEREFBANG,
  [TSHARP] = TDEREFSHARP,
  [TWILDCARD] = TDEREFWILDCARD,
};

static struct node *expr_ref(enum token_type refop, struct node *node) {
  if (node->which == BIN && OP_KIND(node->as.BIN.operator) == OP_BIN_ACC) {
    // Of the form
    //   self.x.y!method args
    // which was transformed to
    //   type.method @!self.x.y args
    // We actually need
    //   type.method @!self.x!y args
    // This is assuming that typing has checked the transformation below is
    // legal.
    node->as.BIN.operator = accop_for_refop[refop];
  }

  struct node *n = calloc(1, sizeof(struct node));
  n->which = UN;
  n->as.UN.operator = refop;
  n->subs_count = 1;
  n->subs = calloc(n->subs_count, sizeof(struct node *));
  n->subs[0] = node;
  return n;
}

static struct node *self_ref_if_value(struct module *mod,
                                      enum token_type access, struct node *node) {
  if (typ_is_reference(node->typ)) {
    return node;
  } else {
    return expr_ref(access, node);
  }
}

static error prepare_call_arguments(struct module *mod, struct node *node) {
  struct node *fun = node->subs[0];

  if (node->subs_count > 1 && (node->subs[1]->flags & NODE_IS_TYPE)) {
    // Explicit generic function instantiation.
    return 0;
  }

  struct node *dfun = typ_definition(fun->typ);
  switch (dfun->which) {
  case DEFFUN:
    if (node_fun_explicit_args_count(dfun) != node->subs_count - 1) {
      error e = mk_except_call_args_count(mod, node, dfun, 0,
                                          node->subs_count - 1);
      THROW(e);
    }
    break;
  case DEFMETHOD:
    if (fun->which == BIN) {
      if ((fun->subs[0]->flags & NODE_IS_TYPE)) {
        // Form (type.method self ...).
        if (1 + node_fun_explicit_args_count(dfun) != node->subs_count - 1) {
          error e = mk_except_call_args_count(mod, node, dfun, 1,
                                              node->subs_count - 1);
          THROW(e);
        }
      } else {
        // Form (self.method ...); rewrite as (type.method self ...).
        if (node_fun_explicit_args_count(dfun) != node->subs_count - 1) {
          error e = mk_except_call_args_count(mod, node, dfun, 0,
                                              node->subs_count - 1);
          THROW(e);
        }

        struct node *m = mk_node(mod, node, DIRECTDEF);
        set_typ(&m->as.DIRECTDEF.typ, fun->typ);
        m->as.DIRECTDEF.flags = NODE_IS_TYPE;
        rew_move_last_over(node, 0, TRUE);

        struct node *self = self_ref_if_value(mod,
                                              refop_for_accop[fun->as.BIN.operator],
                                              fun->subs[0]);
        rew_append(node, self);
        rew_insert_last_at(node, 1);

        const struct node *except[] = { fun->subs[0], NULL };
        error e = catchup(mod, except, self, node->scope, CATCHUP_BELOW_CURRENT);
        EXCEPT(e);

        e = typ_check_can_deref(mod, fun, self->typ,
                                derefop_for_accop[fun->as.BIN.operator]);
        EXCEPT(e);

        e = catchup(mod, NULL, m, node->scope, CATCHUP_BELOW_CURRENT);
        EXCEPT(e);
      }
    } else if ((fun->flags & NODE_IS_TYPE) && fun->which == CALL) {
      // Generic method instantiation: (type.method u32 i32) self
      if (1 + node_fun_explicit_args_count(dfun) != node->subs_count - 1) {
        error e = mk_except_call_args_count(mod, node, dfun, 1,
                                            node->subs_count - 1);
        THROW(e);
      }
    }
    break;
  default:
    assert(FALSE);
  }

  return 0;
}

static error explicit_instantiation(struct module *mod, struct node *node) {
  error e;
  struct typ *t = node->subs[0]->typ;
  const size_t arity = node->subs_count - 1;

  const size_t first = typ_generic_first_explicit_arg(t);
  const size_t explicit_arity = typ_generic_arity(t) - first;
  if (arity != explicit_arity) {
    e = mk_except_type(mod, node,
                       "invalid number of explicit generic arguments:"
                       " %zu expected, but %zu given",
                       explicit_arity, arity);
    THROW(e);
  }

  struct typ **args = calloc(node->subs_count - 1, sizeof(struct typ *));
  for (size_t n = 0; n < arity; ++n) {
    args[n] = node->subs[1 + n]->typ;
  }

  e = instance(&node->typ, mod, node, 1,
               t, args, arity);
  EXCEPT(e);
  free(args);

  node->flags |= NODE_IS_TYPE;

  return 0;
}

static error implicit_function_instantiation(struct module *mod, struct node *node) {
  error e;
  struct typ *tfun = node->subs[0]->typ;
  const size_t arity = node->subs_count - 1;

  // Already checked in prepare_call_arguments().
  assert(arity == typ_function_arity(tfun));

  const size_t gen_arity = typ_generic_arity(tfun);
  struct typ **args = calloc(gen_arity, sizeof(struct typ *));
  for (size_t n = 0; n < typ_generic_arity(tfun); ++n) {
    args[n] = typ_create_tentative(typ_generic_arg(tfun, n));
  }

  struct typ *i = NULL;
  e = instance(&i, mod, node, 0, tfun, args, gen_arity);
  assert(!e);

  for (size_t n = 0; n < typ_function_arity(i); ++n) {
    e = unify(mod, node->subs[1 + n],
              try_wrap_ref_compatible(mod, node, 1,
                                      typ_function_arg(i, n)),
              node->subs[1 + n]->typ);
    EXCEPT(e);
  }

  free(args);

  set_typ(&node->subs[0]->typ, i);
  set_typ(&node->typ, typ_function_return(i));

  return 0;
}

static error function_instantiation(struct module *mod, struct node *node) {
  assert(node->subs_count >= 2);

  if (node->subs[1]->flags & NODE_IS_TYPE) {
    return explicit_instantiation(mod, node);
  } else {
    return implicit_function_instantiation(mod, node);
  }
}

static error check_consistent_either_types_or_values(struct module *mod,
                                                     struct node **subs,
                                                     size_t count) {
  uint32_t flags = 0;
  for (size_t n = 0; n < count; ++n) {
    struct node *s = subs[n];
    if (n > 0 && (flags & NODE_IS_TYPE) != (s->flags & NODE_IS_TYPE)) {
      error e = mk_except_type(mod, s, "expression combines types and values");
      THROW(e);
    }
    flags |= s->flags;
  }

  return 0;
}

static error type_inference_explicit_unary_call(struct module *mod, struct node *node, struct node *dfun) {
  if (dfun->which == DEFFUN && node->subs_count != 1) {
    error e = mk_except_call_args_count(mod, node, dfun, 0, node->subs_count - 1);
    THROW(e);
  } else if (dfun->which == DEFMETHOD && node->subs_count != 2) {
    error e = mk_except_call_args_count(mod, node, dfun, 1, node->subs_count - 1);
    THROW(e);
  }

  if (dfun->which == DEFMETHOD) {
    error e = unify(mod, node->subs[1],
                    try_wrap_ref_compatible(mod, node, 1,
                                            typ_function_arg(dfun->typ, 0)),
                    node->subs[1]->typ);
    EXCEPT(e);
  }

  set_typ(&node->typ, typ_function_return(dfun->typ));

  return 0;
}

static error type_inference_call(struct module *mod, struct node *node) {
  error e;
  struct node *fun = node->subs[0];
  struct typ *tfun = fun->typ;
  struct node *dfun = typ_definition(tfun);

  if (!node_is_fun(dfun)) {
    if (!node_can_have_genargs(dfun)
        || dfun->subs[IDX_GENARGS]->subs_count == 0) {
      e = mk_except_type(mod, fun, "not a generic type");
      THROW(e);
    }

    e = explicit_instantiation(mod, node);
    EXCEPT(e);

    return 0;
  }

  e = prepare_call_arguments(mod, node);
  EXCEPT(e);

  e = check_consistent_either_types_or_values(mod,
                                              node->subs + 1,
                                              node->subs_count - 1);
  EXCEPT(e);

  if (dfun->subs[IDX_GENARGS]->subs_count > 0
      && node_toplevel_const(dfun)->our_generic_functor_typ == NULL) {
    e = function_instantiation(mod, node);
    EXCEPT(e);

    return 0;
  }

  if (node_fun_explicit_args_count(dfun) == 0) {
    return type_inference_explicit_unary_call(mod, node, dfun);
  }

  for (size_t n = 1; n < node->subs_count; ++n) {
    e = unify(mod, node->subs[n],
              try_wrap_ref_compatible(mod, node, 1,
                                      typ_function_arg(tfun, n-1)),
              node->subs[n]->typ);
    EXCEPT(e);
  }

  set_typ(&node->typ, typ_function_return(tfun));

  return 0;
}

static error type_inference_block(struct module *mod, struct node *node) {
  error e;

  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *s = node->subs[n];
    if ((s->flags & NODE_IS_TYPE)) {
      e = mk_except_type(mod, s, "block statements cannot be type names");
      THROW(e);
    }
  }

  if (node->subs_count > 0) {
    for (size_t n = 0; n < node->subs_count - 1; ++n) {
      struct node *s = node->subs[n];
      if (!typ_equal(s->typ, TBI_VOID)) {
        e = mk_except_type(mod, s,
                           "intermediate statements in a block must be of type void"
                           " (except the last one), not '%s'",
                           typ_pretty_name(mod, s->typ));
        THROW(e);
      }
    }
    if (node->subs[node->subs_count - 1]->which == RETURN) {
      set_typ(&node->typ, TBI_VOID);
    } else {
      set_typ(&node->typ, node->subs[node->subs_count - 1]->typ);
    }
  } else {
    set_typ(&node->typ, TBI_VOID);
  }

  return 0;
}

static error type_inference_if(struct module *mod, struct node *node) {
  error e;

  for (size_t n = 0; n < node->subs_count-1; n += 2) {
    e = unify(mod, node->subs[n], node->subs[n]->typ,
              typ_create_tentative(TBI_GENERALIZED_BOOLEAN));
    EXCEPT(e);
  }

  set_typ(&node->typ, node->subs[1]->typ);

  for (size_t n = 3; n < node->subs_count; n += 2) {
    struct node *elif = node->subs[n];
    e = unify(mod, elif, node->typ, elif->typ);
    EXCEPT(e);
  }

  if (node->subs_count % 2 == 1) {
    struct node *els = node->subs[node->subs_count-1];
    e = unify(mod, els, node->typ, els->typ);
    EXCEPT(e);
  } else {
    if (!typ_equal(node->typ, TBI_VOID)) {
      e = mk_except_type(mod, node,
                         "if statement is not of type void but is missing an else branch");
      THROW(e);
    }
  }

  return 0;
}

static error unify_match_pattern(struct module *mod, struct node *expr, struct node *pattern) {
  struct node *d = typ_definition(expr->typ);
  assert(d->which == DEFTYPE);
  const bool enum_or_sum = d->as.DEFTYPE.kind == DEFTYPE_ENUM
    || d->as.DEFTYPE.kind == DEFTYPE_SUM;

  error e;
  if (!enum_or_sum) {
    e = mk_except_type(mod, expr,
                       "must match over an enum or sum type (FIXME: for now)");
    THROW(e);
  }

  if (node_ident(pattern) == ID_OTHERWISE) {
    set_typ(&pattern->typ, expr->typ);
    return 0;
  }

  if (d->which == DEFTYPE
      && enum_or_sum
      && pattern->which == IDENT) {
    struct node *field = NULL;
    e = scope_lookup_ident_immediate(&field, pattern, mod,
                                     d->scope,
                                     node_ident(pattern),
                                     TRUE);
    if (!e) {
      e = typ_check_equal(mod, pattern, expr->typ, field->typ);
      EXCEPT(e);
      set_typ(&pattern->typ, field->typ);
      pattern->flags = field->flags;
      return 0;
    } else {
      // drop e
    }
  }

  e = unify(mod, pattern, pattern->typ, expr->typ);
  EXCEPT(e);

  return 0;
}

static error type_inference_match(struct module *mod, struct node *node) {
  error e;

  struct node *expr = node->subs[0];
  for (size_t n = 1; n < node->subs_count; n += 2) {
    e = unify_match_pattern(mod, expr, node->subs[n]);
    EXCEPT(e);
  }

  set_typ(&node->typ, node->subs[2]->typ);
  for (size_t n = 4; n < node->subs_count; n += 2) {
    e = unify(mod, node->subs[n], node->subs[n]->typ, node->typ);
    EXCEPT(e);
  }

  return 0;
}

static error unify_try_errors(struct typ **exu, struct module *mod,
                              struct try_state *st) {
  for (size_t n = 0; n < st->count; ++n) {
    struct node *exc = st->excepts[n];

    switch (exc->which) {
    case THROW:
      if (exc->subs_count == 2) {
        exc = exc->subs[1];
      } else {
        exc = exc->subs[0];
      }
      break;
    case DEFNAME:
      exc = exc->as.DEFNAME.expr;
      break;
    default:
      assert(FALSE);
    }

    if (*exu == NULL) {
      *exu = exc->typ;
    } else {
      error e = unify(mod, exc, *exu, exc->typ);
      EXCEPT(e);
    }
  }

  return 0;
}

static error type_inference_try(struct module *mod, struct node *node) {
  node->typ = NULL;

  error e;
  struct try_state *st = module_excepts_get(mod);

  if (st->count == 0) {
    e = mk_except(mod, node,
                  "try block has no except or throw statement,"
                  " catch is unreachable");
    THROW(e);
  }

  struct typ *exu = NULL;
  e = unify_try_errors(&exu, mod, st);
  EXCEPT(e);

  struct node *elet = node->subs[0];
  struct node *edefp = elet->subs[0];
  struct node *eident = edefp->subs[0];
  set_typ(&eident->typ, exu);

  struct node *eblock = elet->subs[1];
  struct node *main_block = eblock->subs[0];
  struct typ *u = main_block->typ;

  for (size_t n = 1; n < eblock->subs_count; ++n) {
    struct node *catch = eblock->subs[n];
    struct node *let = catch->subs[0];
    struct node *defp = let->subs[0];
    struct node *error = defp->subs[1];
    struct node *block = catch->subs[1];

    e = unify(mod, error, error->typ, exu);
    EXCEPT(e);

    e = unify(mod, block, block->typ, u);
    EXCEPT(e);
  }

  set_typ(&node->typ, u);

  return 0;
}

static error type_inference_ident(struct module *mod, struct node *node) {
  if (node_ident(node) == ID_OTHERWISE) {
    node->typ = typ_create_tentative(TBI_ANY);
    return 0;
  }

  struct node *def = NULL;
  error e = scope_lookup(&def, mod, node->scope, node);
  EXCEPT(e);

  if (def->which == DEFNAME && def->typ == NULL) {
    // This happens when typing an IDENT in the pattern of a DEFPATTERN:
    // 'def' is the corresponding DEFNAME and not yet typed (as it appears
    // later in the tree).
    set_typ(&def->typ, typ_create_tentative(TBI_ANY));

    // FIXME: Cannot detect if an ident is used before its definition, e.g.:
    //   block
    //     x = a
    //     let a = 0
  }

  if (typ_is_function(def->typ)
      && node->typ != TBI__CALL_FUNCTION_SLOT) {
    if (node_fun_explicit_args_count(typ_definition(def->typ)) != 0) {
      e = mk_except_call_args_count(mod, node, typ_definition(def->typ), 0, 0);
      THROW(e);
    }
    e = rewrite_unary_call(mod, node, def->typ);
    EXCEPT(e);
  } else {
    set_typ(&node->typ, def->typ);
    node->flags = def->flags;
  }

  return 0;
}

static struct typ* number_literal_typ(struct module *mod, struct node *node) {
  assert(node->which == NUMBER);
  if (strchr(node->as.NUMBER.value, '.') != NULL) {
    return TBI_LITERALS_FLOATING;
  } else {
    return TBI_LITERALS_INTEGER;
  }
}

static bool string_literal_has_length_one(const char *s) {
  const size_t len = strlen(s);
  if (s == NULL) {
    return FALSE;
  } else if (len <= 2) {
    return FALSE;
  } else if (s[1] == '\\') {
    return len == 4;
  } else {
    return len == 3;
  }
}

static error step_type_inference(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  error e;

  if (node->typ == TBI__NOT_TYPEABLE) {
    return 0;
  }

  switch (node->which) {
  case DEFINTF:
  case DEFTYPE:
  case DEFFUN:
  case DEFMETHOD:
  case DEFCONSTRAINTLITERAL:
  case DEFNAMEDLITERAL:
    assert(node->typ != NULL);
    // Already typed.
    return 0;
  case IMPORT:
    if (node->typ != NULL) {
      // Already typed.
      return 0;
    }
    break;
  default:
    break;
  }

  assert(node->typ == NULL
         || node->typ == TBI__MUTABLE
         || node->typ == TBI__MERCURIAL
         || node->typ == TBI__CALL_FUNCTION_SLOT
         || node->which == DEFNAME
         || typ_definition_const(node->typ)->which == MODULE
         || typ_definition_const(node->typ)->which == ROOT_OF_ALL);

  switch (node->which) {
  case NUL:
    set_typ(&node->typ, typ_create_tentative(TBI_LITERALS_NULL));
    break;
  case IDENT:
    e = type_inference_ident(mod, node);
    EXCEPT(e);
    break;
  case DEFNAME:
    if (node->typ == NULL && node_ident(node) == ID_OTHERWISE) {
      set_typ(&node->typ, typ_create_tentative(TBI_ANY));
      set_typ(&node->as.DEFNAME.pattern->typ, node->typ);
    }
    assert(node->typ == node->as.DEFNAME.pattern->typ);

    if (node->as.DEFNAME.expr != NULL) {
      e = unify(mod, node,
                node->as.DEFNAME.expr->typ,
                node->as.DEFNAME.pattern->typ);
      EXCEPT(e);

      node->as.DEFNAME.pattern->flags |= node->as.DEFNAME.expr->flags;
    }

    node->flags = node->as.DEFNAME.pattern->flags;
    break;
  case NUMBER:
    set_typ(&node->typ, typ_create_tentative(number_literal_typ(mod, node)));
    break;
  case BOOL:
    set_typ(&node->typ, typ_create_tentative(TBI_BOOL));
    break;
  case STRING:
    set_typ(&node->typ, typ_create_tentative(TBI_STATIC_STRING));
    break;
  case SIZEOF:
    set_typ(&node->typ, TBI_SIZE);
    break;
  case BIN:
    e = type_inference_bin(mod, node);
    EXCEPT(e);
    break;
  case UN:
    e = type_inference_un(mod, node);
    EXCEPT(e);
    break;
  case TUPLE:
    e = type_inference_tuple(mod, node);
    EXCEPT(e);
    break;
  case TUPLEEXTRACT:
    e = type_inference_tupleextract(mod, node);
    EXCEPT(e);
    break;
  case CALL:
    e = type_inference_call(mod, node);
    EXCEPT(e);
    break;
  case INIT:
    e = type_inference_init(mod, node);
    EXCEPT(e);
    break;
  case RETURN:
    e = type_inference_return(mod, node);
    EXCEPT(e);
    break;
  case BLOCK:
    e = type_inference_block(mod, node);
    EXCEPT(e);
    break;
  case CATCH:
    set_typ(&node->typ, node->subs[node->subs_count - 1]->typ);
    break;
  case THROW:
  case BREAK:
  case CONTINUE:
  case NOOP:
    set_typ(&node->typ, TBI_VOID);
    break;
  case IF:
    e = type_inference_if(mod, node);
    EXCEPT(e);
    break;
  case FOR:
    set_typ(&node->typ, TBI_VOID);
    struct node *it = node->subs[IDX_FOR_IT]
      ->subs[IDX_FOR_IT_DEFP]
      ->subs[IDX_FOR_IT_DEFP_DEFN];
    e = unify(mod, it, it->typ, typ_create_tentative(TBI_ITERATOR));
    EXCEPT(e);
    e = typ_check_equal(mod, node_for_block(node),
                        node_for_block(node)->typ,
                        TBI_VOID);
    EXCEPT(e);
    break;
  case WHILE:
    set_typ(&node->typ, TBI_VOID);
    struct node *cond = node->subs[0];
    e = unify(mod, cond, cond->typ, typ_create_tentative(TBI_GENERALIZED_BOOLEAN));
    EXCEPT(e);
    struct node *block = node->subs[1];
    e = typ_check_equal(mod, block, block->typ, TBI_VOID);
    EXCEPT(e);
    break;
  case MATCH:
    e = type_inference_match(mod, node);
    EXCEPT(e);
    break;
  case TRY:
    e = type_inference_try(mod, node);
    EXCEPT(e);
    break;
  case DYN:
    assert(typ_is_reference(node->subs[0]->typ));
    set_typ(&node->typ, node->as.DYN.intf_typ);
    break;
  case TYPECONSTRAINT:
    set_typ(&node->typ, node->subs[1]->typ);
    e = unify(mod, node->subs[0], node->subs[0]->typ, node->typ);
    EXCEPT(e);
    break;
  case DEFARG:
    set_typ(&node->typ, node->subs[1]->typ);
    break;
  case DEFGENARG:
  case SETGENARG:
    set_typ(&node->typ, node->subs[1]->typ);
    node->flags |= NODE_IS_TYPE;
    break;
  case DEFPATTERN:
    set_typ(&node->typ, TBI_VOID);
    break;
  case DEFFIELD:
    set_typ(&node->typ, node->subs[1]->typ);
    break;
  case EXAMPLE:
    e = unify(mod, node->subs[0], node->subs[0]->typ,
              typ_create_tentative(TBI_BOOL));
    EXCEPT(e);
    set_typ(&node->typ, TBI_VOID);
    break;
  case LET:
    if (node_has_tail_block(node)) {
      set_typ(&node->typ, node->subs[node->subs_count - 1]->typ);
    } else {
      set_typ(&node->typ, TBI_VOID);
    }
    break;
  case DELEGATE:
  case PRE:
  case POST:
  case INVARIANT:
    set_typ(&node->typ, TBI_VOID);
    break;
  case ISALIST:
  case GENARGS:
  case FUNARGS:
  case IMPORT:
    node->typ = TBI__NOT_TYPEABLE;
    break;
  case ISA:
    set_typ(&node->typ, node->subs[0]->typ);
    node->flags = node->subs[0]->flags & NODE__TRANSITIVE;
    break;
  case DIRECTDEF:
    set_typ(&node->typ, node->as.DIRECTDEF.typ);
    node->flags = node->as.DIRECTDEF.flags;
    break;
  case DEFCHOICE:
    set_typ(&node->typ, node_parent(node)->typ);
    break;
  default:
    break;
  }

  assert(node->typ != NULL
         || (node->which == IDENT && "tolerate when its DEFNAME not yet typed"));
  return 0;
}

static error step_remove_typeconstraints(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->which == TYPECONSTRAINT && !node->as.TYPECONSTRAINT.in_pattern) {
    struct node **subs = node->subs;
    struct node *sub = node->subs[0];
    struct scope *parent = node->scope->parent;

    memset(node, 0, sizeof(*node));
    *node = *sub;
    node->scope->parent = parent;
    node->scope->node = node;

    free(sub);
    free(subs);
  }

  return 0;
}

static error step_type_drop_excepts(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case TRY:
    module_excepts_close_try(mod);
    return 0;
  default:
    return 0;
  }
}

HTABLE_SPARSE(idents_set, bool, ident);
implement_htable_sparse(__attribute__((unused)) static, idents_set, bool, ident);

static size_t defchoice_count(struct node *deft) {
  assert(deft->which == DEFTYPE);

  size_t r = 0;
  for (size_t n = 0; n < deft->subs_count; ++n) {
    struct node *d = deft->subs[n];
    if (d->which == DEFCHOICE) {
      r += 1;
    }
  }
  return r;
}

static error step_check_exhaustive_match(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which != MATCH) {
    return 0;
  }

  struct node *expr = node->subs[0];
  struct node *dexpr = typ_definition(expr->typ);
  const bool enum_or_sum = dexpr->as.DEFTYPE.kind == DEFTYPE_ENUM
    || dexpr->as.DEFTYPE.kind == DEFTYPE_SUM;

  if (!enum_or_sum) {
    return 0;
  }

  struct idents_set set;
  idents_set_init(&set, 0);
  idents_set_set_delete_val(&set, FALSE);
  idents_set_set_custom_hashf(&set, ident_hash);
  idents_set_set_custom_cmpf(&set, ident_cmp);

  error e = 0;
  for (size_t n = 1; n < node->subs_count; n += 2) {
    struct node *p = node->subs[n];
    ident id;
    switch (p->which) {
    case IDENT:
      id = node_ident(p);
      if (id == ID_OTHERWISE) {
        if (n != node->subs_count - 2) {
          e = mk_except(mod, p, "default pattern '_' must be last");
          GOTO_THROW(e);
        }
        // No need to check further.
        goto ok;
      }
      break;
    case BIN:
      assert(OP_KIND(p->as.BIN.operator) == OP_BIN_ACC);
      id = node_ident(p->subs[1]);
      break;
    default:
      assert(FALSE);
    }

    if (idents_set_get(&set, id) != NULL) {
      e = mk_except(mod, p, "duplicated match case");
      GOTO_THROW(e);
    }

    idents_set_set(&set, id, TRUE);
  }

  if (idents_set_count(&set) != defchoice_count(dexpr)) {
    e = mk_except_type(mod, node, "non-exhaustive match");
    GOTO_THROW(e);
  }

ok:
except:
  idents_set_destroy(&set);
  return e;
}

static error step_gather_final_instantiations(struct module *mod, struct node *node,
                                              void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    break;
  default:
    return 0;
  }

  struct module_state *st = mod->state;
  if (st->tentative_instantiations == NULL) {
    return 0;
  }

  for (size_t n = 0; n < st->tentative_instantiations_count; ++n) {
    struct typ *t = st->tentative_instantiations[n]->typ;

    if (typ_generic_arity(t) == 0) {
      // For instance, a DEFNAMEDLITERAL that unified to a non-generic.
      continue;
    }

    if (typ_is_reference(t)) {
      continue;
    }

    if (typ_is_tentative(t)) {
      // By now, this instance should not be tentative anymore, as all its
      // generic arguments should have been linked to final types.
      for (size_t m = 0; m < typ_generic_arity(t); ++m) {
        struct typ *arg = typ_generic_arg(t, m);
        assert(!typ_is_tentative(arg));
      }
    }

    struct typ *functor = typ_generic_functor(t);
    const size_t arity = typ_generic_arity(t);

    if (typ_definition_const(functor)->which == DEFINTF) {
      continue;
    }
    for (size_t m = 0; m < arity; ++m) {
      if (typ_definition_const(typ_generic_arg_const(t, m))->which == DEFINTF) {
        continue;
      }
    }

    struct typ *existing = find_existing_instance_for_tentative(mod, t);
    if (existing != NULL) {
      typ_link_to_existing_final(existing, t);
      continue;
    }

    struct typ **args = calloc(arity, sizeof(struct typ *));
    for (size_t m = 0; m < arity; ++m) {
      args[m] = typ_generic_arg(t, m);
    }

    struct typ *i = NULL;
    error e = do_instantiate(&i, mod, functor, args, arity, FALSE);
    EXCEPT(e);

    free(args);
  }

  free(st->tentative_instantiations);
  st->tentative_instantiations = NULL;
  st->tentative_instantiations_count = 0;

  return 0;
}

static void do_mk_expr_abspath(struct module *mod, struct node *node, const char *path, ssize_t len) {
  assert(node->which == BIN);
  node->as.BIN.operator = TDOT;

  for (ssize_t i = len-1; i >= 0; --i) {
    if (i == 0) {
      assert(len > 1);
      ident id = idents_add_string(mod->gctx, path, len - i);

      struct node *root = mk_node(mod, node, DIRECTDEF);
      set_typ(&root->as.DIRECTDEF.typ, mod->gctx->modules_root.typ);
      root->as.DIRECTDEF.flags = NODE_IS_TYPE;
      struct node *name = mk_node(mod, node, IDENT);
      name->as.IDENT.name = id;

      break;
    } else if (path[i] == '.') {
      assert(len - i > 1);
      ident id = idents_add_string(mod->gctx, path + i + 1, len - i - 1);

      struct node *down = mk_node(mod, node, BIN);
      struct node *name = mk_node(mod, node, IDENT);
      name->as.IDENT.name = id;

      do_mk_expr_abspath(mod, down, path, i);
      break;
    }
  }
}

static struct node *mk_expr_abspath(struct module *mod, struct node *node, const char *path) {
  if (strstr(path, ".") == NULL) {
    struct node *n = mk_node(mod, node, IDENT);
    n->as.IDENT.name = idents_add_string(mod->gctx, path, strlen(path));
    return n;
  }

  struct node *n = mk_node(mod, node, BIN);
  do_mk_expr_abspath(mod, n, path, strlen(path));
  return n;
}

static void add_inferred_isa(struct module *mod, struct node *deft, const char *path) {
  struct node *isalist = deft->subs[IDX_ISALIST];
  assert(isalist->which == ISALIST);
  struct node *isa = mk_node(mod, isalist, ISA);
  isa->as.ISA.is_export = node_toplevel(deft)->is_inline;
  (void)mk_expr_abspath(mod, isa, path);

  error e = catchup(mod, NULL, isa, isalist->scope, CATCHUP_BELOW_CURRENT);
  assert(!e);

  typ_create_update_quickisa(deft->typ);
}

static error step_add_builtin_enum_intf(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFTYPE
      || node->as.DEFTYPE.kind != DEFTYPE_ENUM) {
    return 0;
  }

  add_inferred_isa(mod, node, "nlang.builtins.i_trivial_copy");
  add_inferred_isa(mod, node, "nlang.builtins.i_trivial_dtor");

  return 0;
}

static error step_add_builtin_detect_ctor_intf(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFTYPE) {
    return 0;
  }
  if (node_toplevel_const(node)->is_extern) {
    return 0;
  }
  if (node->as.DEFTYPE.kind == DEFTYPE_ENUM
      || node->as.DEFTYPE.kind == DEFTYPE_SUM) {
    return 0;
  }

  struct node *proxy = node;
  struct node *ctor = node_get_member(mod, proxy, ID_CTOR);
  if (ctor != NULL) {
    if (node_fun_explicit_args_count(ctor) == 0) {
      add_inferred_isa(mod, node, "nlang.builtins.i_default_ctor");
    } else if (node_fun_explicit_args_count(ctor) == 1) {
      add_inferred_isa(mod, node, "nlang.builtins.i_ctor_with");
    }
  } else {
    add_inferred_isa(mod, node, "nlang.builtins.i_trivial_ctor");
  }

  return 0;
}

static error step_rewrite_final_this(struct module *mod, struct node *node, void *user, bool *stop) {
  struct typ *thi = user;
  if (node->which == IDENT) {
    ident id = node_ident(node);
    if (id == ID_THIS) {
      node->which = DIRECTDEF;
      set_typ(&node->as.DIRECTDEF.typ, thi);
      node->as.DIRECTDEF.flags = NODE_IS_TYPE;
    }
  }
  return 0;
}

static void intf_proto_deepcopy(struct module *mod, struct typ *thi,
                                struct node *dst, struct node *src) {
  node_deepcopy(mod, dst, src);

  static const step down[] = {
    step_rewrite_final_this,
    NULL,
  };

  static const step up[] = {
    NULL,
  };

  PUSH_STATE(mod->state->step_state);
  error e = pass(mod, dst, down, up, -1, thi);
  assert(!e);
  POP_STATE(mod->state->step_state);

  if (node_toplevel(dst) != NULL) {
    node_toplevel(dst)->yet_to_pass = 0;
  }
}

static void define_builtin(struct module *mod, struct node *deft,
                           enum builtingen bg) {
  struct node *modbody;
  ssize_t insert_pos;
  if (deft->subs[IDX_GENARGS]->subs_count > 0) {
    modbody = NULL;
    insert_pos = -1;
  } else {
    modbody = node_parent(deft);
    insert_pos = rew_find_subnode_in_parent(modbody, deft) + 1;
  }

  struct node *proto = NULL;
  error e = scope_lookup_abspath(&proto, deft, mod, builtingen_abspath[bg]);
  assert(!e);

  struct node *existing = node_get_member(mod, deft, node_ident(proto));
  if (existing != NULL) {
    return;
  }

  struct node *d;
  if (insert_pos >= 0) {
    d = node_new_subnode(mod, modbody);
  } else {
    d = calloc(1, sizeof(*d));
  }
  intf_proto_deepcopy(mod, node_parent(proto)->typ, d, proto);
  mk_expr_abspath(mod, d, builtingen_abspath[bg]);
  rew_move_last_over(d, 0, FALSE);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->scope_name = node_ident(deft);
  toplevel->is_prototype = FALSE;
  toplevel->builtingen = bg;
  toplevel->is_export = node_toplevel(deft)->is_export;
  toplevel->is_inline = node_toplevel(deft)->is_inline;

  enum catchup_for how;
  if (insert_pos >= 0) {
    rew_insert_last_at(modbody, insert_pos);
    how = CATCHUP_AFTER_CURRENT;
  } else {
    append_member(deft, d);
    how = CATCHUP_BELOW_CURRENT;
  }

  e = catchup(mod, NULL, d, deft->scope, how);
  assert(!e);
}

static void define_defchoice_builtin(struct module *mod, struct node *ch,
                                     enum builtingen bg, enum node_which which) {
  struct node *deft = node_parent(ch);

  struct node *proto = NULL;
  error e = scope_lookup_abspath(&proto, ch, mod, builtingen_abspath[bg]);
  assert(!e);

  struct node *d = mk_node(mod, ch, which);
  intf_proto_deepcopy(mod, node_parent(proto)->typ, d, proto);
  mk_expr_abspath(mod, d, builtingen_abspath[bg]);
  rew_move_last_over(d, 0, FALSE);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->scope_name = node_ident(ch);
  toplevel->is_prototype = FALSE;
  toplevel->builtingen = bg;
  toplevel->is_export = node_toplevel(deft)->is_export;
  toplevel->is_inline = node_toplevel(deft)->is_inline;

  if (bg == BG_SUM_CTOR_WITH_CTOR
      || bg == BG_SUM_CTOR_WITH_MK
      || bg == BG_SUM_CTOR_WITH_NEW) {
    struct node *funargs = d->subs[IDX_FUNARGS];
    struct node *arg = mk_node(mod, funargs, DEFARG);
    struct node *name = mk_node(mod, arg, IDENT);
    name->as.IDENT.name = ID_C;
    struct node *typename = mk_node(mod, arg, DIRECTDEF);
    set_typ(&typename->as.DIRECTDEF.typ, ch->subs[IDX_CH_PAYLOAD]->typ);
    typename->as.DIRECTDEF.flags = NODE_IS_TYPE;

    rew_insert_last_at(funargs, (d->which == DEFMETHOD) ? 1 : 0);
  }

  e = catchup(mod, NULL, d, ch->scope, CATCHUP_BELOW_CURRENT);
  assert(!e);
}

static error step_add_builtin_defchoice_constructors(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFCHOICE) {
    return 0;
  }

  const struct node *deft = node_parent(node);
  if (deft->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    return 0;
  }

  const struct typ *targ = node->subs[IDX_CH_PAYLOAD]->typ;
  error e = typ_check_isa(mod, node->subs[IDX_CH_PAYLOAD],
                          targ, TBI_COPYABLE);
  EXCEPT(e);

  define_defchoice_builtin(
    mod, node, BG_SUM_CTOR_WITH_CTOR, DEFMETHOD);

  return 0;
}

static error step_add_builtin_ctor(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFTYPE) {
    return 0;
  }
  if (node_toplevel_const(node)->is_extern) {
    return 0;
  }

  return 0;
}

static error step_add_builtin_dtor(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFTYPE) {
    return 0;
  }
  if (node_toplevel_const(node)->is_extern) {
    return 0;
  }

  return 0;
}

static error define_auto(struct module *mod, struct node *deft,
                         enum builtingen bg) {
  struct node *proto = NULL;
  error e = scope_lookup_abspath(&proto, deft, mod, builtingen_abspath[bg]);
  assert(!e);

  struct node *existing = node_get_member(mod, deft, node_ident(proto));
  if (existing != NULL) {
    return 0;
  }

  struct node *ctor = NULL;
  e = scope_lookup_ident_immediate(&ctor, deft, mod, deft->scope,
                                   ID_CTOR, TRUE);
  if (e) {
    // FIXME This should be narrower and only in the case the type cannot be
    // given an automatically generated ctor.
    e = mk_except_type(mod, deft, "type '%s' is not i_trivial_ctor and has no 'ctor'",
                       typ_pretty_name(mod, deft->typ));
    THROW(e);
  }

  struct node *modbody;
  ssize_t insert_pos;
  if (deft->subs[IDX_GENARGS]->subs_count > 0) {
    modbody = NULL;
    insert_pos = -1;
  } else {
    modbody = node_parent(deft);
    insert_pos = rew_find_subnode_in_parent(modbody, deft) + 1;
  }

  struct node *d;
  if (insert_pos >= 0) {
    d = node_new_subnode(mod, modbody);
  } else {
    d = calloc(1, sizeof(*d));
  }
  intf_proto_deepcopy(mod, node_parent(proto)->typ, d, proto);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->scope_name = node_ident(deft);
  toplevel->is_prototype = FALSE;
  toplevel->builtingen = bg;
  toplevel->is_export = node_toplevel(ctor)->is_export;
  toplevel->is_inline = node_toplevel(ctor)->is_inline;

  struct node *ctor_funargs = ctor->subs[IDX_FUNARGS];
  struct node *d_funargs = d->subs[IDX_FUNARGS];
  // (Skip self.)
  for (size_t n = 1; n < node_fun_all_args_count(ctor); ++n) {
    struct node *arg = ctor_funargs->subs[n];
    struct node *cpy = node_new_subnode(mod, d_funargs);
    intf_proto_deepcopy(mod, node_parent(proto)->typ, cpy, arg);
    rew_insert_last_at(d_funargs, n-1);
  }

  enum catchup_for how;
  if (insert_pos >= 0) {
    rew_insert_last_at(modbody, insert_pos);
    how = CATCHUP_AFTER_CURRENT;
  } else {
    append_member(deft, d);
    how = CATCHUP_BELOW_CURRENT;
  }

  assert(node_toplevel(d)->yet_to_pass == 0);
  e = catchup(mod, NULL, d, deft->scope, how);
  assert(!e);

  return 0;
}

static error step_add_builtin_mk_new(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFTYPE) {
    return 0;
  }
  if (node_toplevel_const(node)->is_extern) {
    return 0;
  }
  if (node->as.DEFTYPE.kind == DEFTYPE_SUM
      || node->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_TRIVIAL_CTOR)) {
    define_builtin(mod, node, BG_TRIVIAL_CTOR_CTOR);
    define_builtin(mod, node, BG_TRIVIAL_CTOR_MK);
    define_builtin(mod, node, BG_TRIVIAL_CTOR_NEW);
  } else if (typ_isa(node->typ, TBI_DEFAULT_CTOR)) {
    define_builtin(mod, node, BG_DEFAULT_CTOR_MK);
    define_builtin(mod, node, BG_DEFAULT_CTOR_NEW);
  } else if (typ_isa(node->typ, TBI_CTOR_WITH)) {
    define_builtin(mod, node, BG_CTOR_WITH_MK);
    define_builtin(mod, node, BG_CTOR_WITH_NEW);
  } else {
    error e = define_auto(mod, node, BG_AUTO_MK);
    EXCEPT(e);
    e = define_auto(mod, node, BG_AUTO_NEW);
    EXCEPT(e);
  }

  return 0;
}

static error step_add_builtin_mkv_newv(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFTYPE) {
    return 0;
  }
  if (node_toplevel_const(node)->is_extern) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_TRIVIAL_ARRAY_CTOR)) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_ARRAY_CTOR)) {
    define_builtin(mod, node, BG_AUTO_MKV);
    define_builtin(mod, node, BG_AUTO_NEWV);
  }

  return 0;
}

static error step_add_builtin_defchoice_mk_new(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFCHOICE) {
    return 0;
  }

  struct node *deft = node_parent(node);
  assert(deft->which == DEFTYPE);
  if (deft->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    define_defchoice_builtin(mod, node, BG_DEFAULT_CTOR_MK, DEFFUN);
    define_defchoice_builtin(mod, node, BG_DEFAULT_CTOR_NEW, DEFFUN);
  } else if (deft->as.DEFTYPE.kind == DEFTYPE_SUM) {
    define_defchoice_builtin(mod, node, BG_SUM_CTOR_WITH_MK, DEFFUN);
    define_defchoice_builtin(mod, node, BG_SUM_CTOR_WITH_NEW, DEFFUN);
  }

  return 0;
}

static error step_add_builtin_operators(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFTYPE) {
    return 0;
  }
  if (node_toplevel_const(node)->is_extern) {
    return 0;
  }

  switch (node->as.DEFTYPE.kind) {
  case DEFTYPE_PROTOTYPE:
    break;
  case DEFTYPE_STRUCT:
    break;
  case DEFTYPE_SUM:
    break;
  case DEFTYPE_ENUM:
    if (!typ_isa(node->as.DEFTYPE.choice_typ, TBI_NATIVE_INTEGER)) {
      define_builtin(mod, node, BG_ENUM_EQ);
      define_builtin(mod, node, BG_ENUM_NE);
    }
    break;
  }

  return 0;
}

static error step_add_trivials(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFTYPE) {
    return 0;
  }

  // FIXME: We should check that the fields/defchoice do indeed support
  // these trivial interfaces. It must be safe to declare them.
  // Same thing for trivial ctor, dtor.

  if (typ_is_pseudo_builtin(node->typ)
      || typ_is_reference(node->typ)) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_TRIVIAL_COPY)) {
    define_builtin(mod, node, BG_TRIVIAL_COPY_COPY_CTOR);
  }
  if (typ_isa(node->typ, TBI_TRIVIAL_EQUALITY)) {
    define_builtin(mod, node, BG_TRIVIAL_EQUALITY_OPERATOR_EQ);
    define_builtin(mod, node, BG_TRIVIAL_EQUALITY_OPERATOR_NE);
  }

  return 0;
}

static void define_dispatch(struct module *mod, struct node *deft, struct typ *tintf) {
  struct node *intf = typ_definition(tintf);

  struct node *modbody = node_parent(deft);
  size_t insert_pos = rew_find_subnode_in_parent(modbody, deft) + 1;

  for (size_t n = 0; n < intf->subs_count; ++n) {
    struct node *proto = intf->subs[n];
    if (proto->which != DEFMETHOD && proto->which != DEFFUN) {
      continue;
    }
    if (node_toplevel_const(proto)->is_not_dyn) {
      continue;
    }

    struct node *existing = node_get_member(mod, deft, node_ident(proto));
    if (existing != NULL) {
      return;
    }

    struct node *d = mk_node(mod, modbody, proto->which);
    intf_proto_deepcopy(mod, node_parent(proto)->typ, d, proto);
    char *abspath = scope_name(mod, proto->scope);
    mk_expr_abspath(mod, d, abspath);
    rew_move_last_over(d, 0, FALSE);

    struct toplevel *toplevel = node_toplevel(d);
    toplevel->scope_name = node_ident(deft);
    toplevel->builtingen = BG_SUM_DISPATCH;
    toplevel->is_prototype = FALSE;
    toplevel->is_export = node_toplevel(deft)->is_export;
    toplevel->is_inline = node_toplevel(deft)->is_inline;

    rew_insert_last_at(modbody, insert_pos);

    error e = catchup(mod, NULL, d, deft->scope, CATCHUP_BELOW_CURRENT);
    assert(!e);
  }
}

static error sum_choice_with_intf(struct module *mod, struct typ *t,
                                  struct typ *intf, bool *stop, void *user) {
  struct node *node = user;

  struct typ *to_check = intf;
  if (typ_equal(intf, TBI_SUM_COPY)) {
    to_check = TBI_COPYABLE;
  } else if (typ_equal(intf, TBI_SUM_EQUALITY)) {
    to_check = TBI_HAS_EQUALITY;
  } else if (typ_equal(intf, TBI_SUM_ORDER)) {
    to_check = TBI_ORDERED;
  }

  for (size_t c = 0; c < node->subs_count; ++c) {
    struct node *ch = node->subs[c];
    if (ch->which != DEFCHOICE) {
      continue;
    }

    struct typ *tch = NULL;
    if (typ_equal(ch->subs[IDX_CH_PAYLOAD]->typ, TBI_VOID)) {
      tch = node->as.DEFTYPE.choice_typ;
    } else {
      tch = ch->subs[IDX_CH_PAYLOAD]->typ;
    }

    error e = typ_check_isa(mod, ch, tch, to_check);
    EXCEPT(e);
  }

  if (typ_equal(intf, TBI_SUM_COPY)) {
    if (!typ_isa(node->typ, TBI_TRIVIAL_COPY)) {
      define_builtin(mod, node, BG_SUM_COPY);
    }
  } else if (typ_equal(intf, TBI_SUM_EQUALITY)) {
    if (!typ_isa(node->typ, TBI_TRIVIAL_EQUALITY)) {
      define_builtin(mod, node, BG_SUM_EQUALITY_EQ);
      define_builtin(mod, node, BG_SUM_EQUALITY_NE);
    }
  } else if (typ_equal(intf, TBI_SUM_ORDER)) {
    define_builtin(mod, node, BG_SUM_ORDER_LE);
    define_builtin(mod, node, BG_SUM_ORDER_LT);
    define_builtin(mod, node, BG_SUM_ORDER_GT);
    define_builtin(mod, node, BG_SUM_ORDER_GE);
  } else {
    define_dispatch(mod, node, intf);
  }

  return 0;
}

static error step_add_sum_dispatch(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case DEFTYPE:
    break;
  default:
    return 0;
  }

  switch (node->as.DEFTYPE.kind) {
  case DEFTYPE_PROTOTYPE:
  case DEFTYPE_ENUM:
  case DEFTYPE_STRUCT:
    return 0;
  case DEFTYPE_SUM:
    break;
  }

  error e = typ_isalist_foreach(mod, node->typ, ISALIST_FILTER_TRIVIAL_ISALIST,
                          sum_choice_with_intf, node);
  EXCEPT(e);

  return 0;
}

static error step_rewrite_def_return_through_ref(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFFUN && node->which != DEFMETHOD) {
    return 0;
  }

  struct node *retval = node_fun_retval(node);
  if (typ_isa(retval->typ, TBI_RETURN_BY_COPY)) {
    return 0;
  }

  if (retval->which == DEFARG) {
    return 0;
  }

  struct node *funargs = node->subs[IDX_FUNARGS];
  const size_t where = rew_find_subnode_in_parent(funargs, retval);
  struct node *named = mk_node(mod, funargs, DEFARG);
  named->as.DEFARG.is_retval = TRUE;
  struct node *name = mk_node(mod, named, IDENT);
  name->as.IDENT.name = ID_NRETVAL;
  rew_append(named, retval);
  rew_move_last_over(funargs, where, TRUE);

  error e = lexical_retval(mod, node, named);
  EXCEPT(e);

  const struct node *except[] = { retval, NULL };
  e = catchup(mod, except, named, node->scope, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  // We need to force the typing of 'named' by hand.
  e = step_type_inference(mod, named, NULL, NULL);
  EXCEPT(e);

  return 0;
}

static const ident operator_ident[TOKEN__NUM] = {
  [Tor] = ID_OPERATOR_OR,
  [Tand] = ID_OPERATOR_AND,
  [Tnot] = ID_OPERATOR_NOT,
  [TLE] = ID_OPERATOR_LE,
  [TLT] = ID_OPERATOR_LT,
  [TGT] = ID_OPERATOR_GT,
  [TGE] = ID_OPERATOR_GE,
  [TEQ] = ID_OPERATOR_EQ,
  [TNE] = ID_OPERATOR_NE,
  [TBWOR] = ID_OPERATOR_BWOR,
  [TBWXOR] = ID_OPERATOR_BWXOR,
  [TBWAND] = ID_OPERATOR_BWAND,
  [TLSHIFT] = ID_OPERATOR_LSHIFT,
  [TRSHIFT] = ID_OPERATOR_RSHIFT,
  [TBWOR_ASSIGN] = ID_OPERATOR_ASSIGN_BWOR,
  [TBWXOR_ASSIGN] = ID_OPERATOR_ASSIGN_BWXOR,
  [TBWAND_ASSIGN] = ID_OPERATOR_ASSIGN_BWAND,
  [TLSHIFT_ASSIGN] = ID_OPERATOR_ASSIGN_LSHIFT,
  [TRSHIFT_ASSIGN] = ID_OPERATOR_ASSIGN_RSHIFT,
  [TPLUS] = ID_OPERATOR_PLUS,
  [TMINUS] = ID_OPERATOR_MINUS,
  [TDIVIDE] = ID_OPERATOR_DIVIDE,
  [TMODULO] = ID_OPERATOR_MODULO,
  [TTIMES] = ID_OPERATOR_TIMES,
  [TPLUS_ASSIGN] = ID_OPERATOR_ASSIGN_PLUS,
  [TMINUS_ASSIGN] = ID_OPERATOR_ASSIGN_MINUS,
  [TDIVIDE_ASSIGN] = ID_OPERATOR_ASSIGN_DIVIDE,
  [TMODULO_ASSIGN] = ID_OPERATOR_ASSIGN_MODULO,
  [TTIMES_ASSIGN] = ID_OPERATOR_ASSIGN_TIMES,
  [TUMINUS] = ID_OPERATOR_UMINUS,
  [TBWNOT] = ID_OPERATOR_BWNOT,
};

static error step_literal_conversion(struct module *mod, struct node *node,
                                     void *user, bool *stop) {
  DSTEP(mod, node);

  ident id;
  struct typ *lit_typ;

  switch (node->which) {
  case STRING:
    if (typ_equal(node->typ, TBI_STATIC_STRING)) {
      return 0;
    }

    if (typ_equal(node->typ, TBI_CHAR)) {
      if (!string_literal_has_length_one(node->as.STRING.value)) {
        error e = mk_except_type(mod, node,
                                 "string literal '%s' does not have length 1,"
                                 " cannot coerce to char",
                                 node->as.STRING.value);
        THROW(e);
      }
      return 0;
    }

    id = ID_FROM_STATIC_STRING;
    lit_typ = TBI_STATIC_STRING;
    break;
  case BOOL:
    if (typ_equal(node->typ, TBI_BOOL)) {
      return 0;
    }
    id = ID_FROM_BOOL;
    lit_typ = TBI_BOOL;
    break;
  default:
    return 0;
  }

  struct node copy = *node;

  memset(node, 0, sizeof(*node));
  node->which = CALL;

  struct node *fun = mk_node(mod, node, DIRECTDEF);
  struct node *fund = node_get_member(mod, typ_definition(copy.typ), id);
  assert(fund != NULL);
  set_typ(&fun->as.DIRECTDEF.typ, fund->typ);
  fun->as.DIRECTDEF.flags = NODE_IS_TYPE;

  struct node *literal = node_new_subnode(mod, node);
  *literal = copy;
  fix_scopes_after_move(literal);
  set_typ(&literal->typ, lit_typ);

  const struct node *except[] = { literal, NULL };
  error e = catchup(mod, except, node, copy.scope->parent,
                    CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static enum token_type operator_call_arg_refop(const struct module *mod,
                                               const struct typ *tfun, size_t n) {
  const struct typ *arg0 = typ_generic_functor_const(typ_function_arg_const(tfun, n));
  assert(arg0 != NULL && typ_is_reference(arg0));

  if (typ_equal(arg0, TBI_REF) || typ_equal(arg0, TBI_NREF)) {
    return TREFDOT;
  } else if (typ_equal(arg0, TBI_MREF) || typ_equal(arg0, TBI_NMREF)) {
    return TREFBANG;
  } else if (typ_equal(arg0, TBI_MMREF) || typ_equal(arg0, TBI_NMMREF)) {
    return TREFSHARP;
  } else {
    assert(FALSE);
    return 0;
  }
}

static error gen_operator_call(struct module *mod,
                               struct scope *saved_parent, struct node *node,
                               ident operator_name, struct node *left, struct node *right,
                               enum catchup_for catchup_for) {
  struct typ *tfun = node_get_member(mod, typ_definition(left->typ),
                                     operator_name)->typ;

  memset(node, 0, sizeof(*node));
  node->which = CALL;
  struct node *fun = mk_node(mod, node, DIRECTDEF);
  set_typ(&fun->as.DIRECTDEF.typ, tfun);
  fun->as.DIRECTDEF.flags = NODE_IS_TYPE;

  const struct node *except[3] = { NULL, NULL, NULL };
  except[0] = left;
  rew_append(node, expr_ref(operator_call_arg_refop(mod, tfun, 0), left));

  if (right != NULL) {
    except[1] = right;
    rew_append(node, expr_ref(operator_call_arg_refop(mod, tfun, 1), right));
  }

  error e = catchup(mod, except, node, saved_parent, catchup_for);
  EXCEPT(e);

  return 0;
}

static error step_operator_call_inference(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  enum token_type op;
  struct node *left = NULL;
  struct node *right = NULL;

  switch (node->which) {
  case UN:
    op = node->as.UN.operator;
    left = node->subs[0];
    break;
  case BIN:
    op = node->as.BIN.operator;
    left = node->subs[0];
    right = node->subs[1];
    break;
  default:
    return 0;
  }

  switch (OP_KIND(op)) {
  case OP_BIN_SYM_PTR:
    return 0;
  case OP_UN_BOOL:
  case OP_UN_NUM:
  case OP_BIN:
  case OP_BIN_SYM:
  case OP_BIN_SYM_BOOL:
  case OP_BIN_SYM_NUM:
  case OP_BIN_NUM_RHS_UNSIGNED:
    break;
  default:
    return 0;
  }

  if (typ_isa(left->typ, TBI_NATIVE_INTEGER)
      || typ_isa(left->typ, TBI_NATIVE_BOOLEAN)
      || typ_isa(left->typ, TBI_NATIVE_FLOATING)) {
    return 0;
  }

  struct node *dleft = typ_definition(left->typ);
  if (dleft->which == DEFTYPE
      && dleft->as.DEFTYPE.kind == DEFTYPE_ENUM
      && typ_isa(dleft->as.DEFTYPE.choice_typ, TBI_NATIVE_INTEGER)) {
    return 0;
  }

  if (operator_ident[op] == 0) {
    return 0;
  }

  error e = gen_operator_call(mod, node->scope->parent, node,
                              operator_ident[op], left, right,
                              CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static error gen_operator_test_call(struct module *mod, struct node *node, size_t n) {
  struct node *expr = node->subs[n];
  if (typ_equal(expr->typ, TBI_BOOL)) {
    return 0;
  }

  struct node *test = node_new_subnode(mod, node);
  rew_move_last_over(node, n, TRUE);
  error e = gen_operator_call(mod, node->scope->parent, test,
                              ID_OPERATOR_TEST, expr, NULL,
                              CATCHUP_BELOW_CURRENT);
  EXCEPT(e);
  return 0;
}

static error step_operator_test_call_inference(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  error e;

  switch (node->which) {
  case DEFNAME:
    if (node->as.DEFNAME.is_excep) {
      e = gen_operator_test_call(mod, node, IDX_DEFNAME_EXCEP_TEST);
      EXCEPT(e);
    }
    break;
  case IF:
    for (size_t n = 0; n < node->subs_count - 1; n += 2) {
      e = gen_operator_test_call(mod, node, n);
      EXCEPT(e);
    }
    break;
  case WHILE:
    e = gen_operator_test_call(mod, node, 0);
    EXCEPT(e);
    break;
  default:
    break;
  }

  return 0;
}

static error step_ctor_call_inference(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  return 0;
}

static error step_array_ctor_call_inference(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->which != INIT || !node->as.INIT.is_array) {
    return 0;
  }

  if (typ_isa(node->typ, TBI_TRIVIAL_ARRAY_CTOR)) {
    return 0;
  }

  struct node copy = *node;

  memset(node, 0, sizeof(*node));
  node->which = CALL;
  struct node *fun = mk_node(mod, node, DIRECTDEF);
  set_typ(&fun->as.DIRECTDEF.typ,
          node_get_member(mod, typ_definition(copy.typ), ID_MKV)->typ);
  fun->as.DIRECTDEF.flags = NODE_IS_TYPE;

  struct node *ref_array = mk_node(mod, node, UN);
  ref_array->as.UN.operator = TREFDOT;
  struct node *array = node_new_subnode(mod, ref_array);
  *array = copy;
  fix_scopes_after_move(array);
  set_typ(&array->typ,
          typ_generic_arg(typ_function_arg(fun->as.DIRECTDEF.typ, 0), 0));

  const struct node *except[] = { array, NULL };
  error e = catchup(mod, except, node, copy.scope->parent,
                    CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static error step_dtor_call_inference(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  return 0;
}

static bool expr_is_literal_initializer(struct node **init, struct module *mod, struct node *expr) {
  if (expr->which == INIT) {
    if (init != NULL) {
      *init = expr;
    }
    return TRUE;
  } else {
    return expr->which == TYPECONSTRAINT
      && expr_is_literal_initializer(init, mod, expr->subs[0]);
  }
}

static bool expr_is_return_through_ref(struct node **init, struct module *mod, struct node *expr) {
  return (expr_is_literal_initializer(init, mod, expr) || expr->which == CALL)
    && !typ_isa(expr->typ, TBI_RETURN_BY_COPY);
}

static error assign_copy_call_inference(struct module *mod, struct node *node) {
  error e = gen_operator_call(mod, node->scope->parent, node,
                              ID_COPY_CTOR, node->subs[0], node->subs[1],
                              CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);
  return 0;
}

static error defname_copy_call_inference(struct module *mod, struct node *node) {
  struct node *let = node_parent(node_parent(node));
  assert(let->which == LET);

  struct node *within;
  if (node_has_tail_block(let)) {
    within = let->subs[let->subs_count-1];
  } else {
    within = mk_node(mod, let, BLOCK);
    error e = catchup(mod, NULL, within, let->scope, CATCHUP_BELOW_CURRENT);
    EXCEPT(e);
  }

  struct node *left = node->as.DEFNAME.pattern;
  struct node *right = node->as.DEFNAME.expr;
  node->as.DEFNAME.expr = NULL; // Steal right.
  struct scope *saved_parent = within->scope->parent;

  struct node *copycall = node_new_subnode(mod, within);
  error e = gen_operator_call(mod, saved_parent, copycall,
                              ID_COPY_CTOR, left, right,
                              CATCHUP_AFTER_CURRENT);
  EXCEPT(e);
  return 0;
}

static error step_copy_call_inference(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  struct node *left;
  struct node *right;
  switch (node->which) {
  case BIN:
    if (node->as.BIN.operator == TASSIGN) {
      left = node->subs[0];
      right = node->subs[1];
      break;
    }
    return 0;
  case DEFNAME:
    if (!(node->flags & NODE_IS_TYPE)) {
      left = node->as.DEFNAME.pattern;
      right = node->as.DEFNAME.expr;
      if (right != NULL) {
        break;
      }
    }
    return 0;
  default:
    return 0;
  }

  if ((right->flags & NODE_IS_TEMPORARY)) {
    // It's OK to trivial copy temporaries: it's a move.
    return 0;
  }

  if (typ_isa(left->typ, TBI_TRIVIAL_COPY)) {
    return 0;
  }

  if (expr_is_return_through_ref(NULL, mod, right)) {
    return 0;
  }

  error e = typ_check_isa(mod, left, left->typ, TBI_COPYABLE);
  EXCEPT(e);

  switch (node->which) {
  case BIN:
    e = assign_copy_call_inference(mod, node);
    break;
  case DEFNAME:
    e = defname_copy_call_inference(mod, node);
    break;
  default:
    assert(FALSE);
  }
  EXCEPT(e);
  return 0;
}

static error check_exhaustive_intf_impl_eachisalist(struct module *mod,
                                                    struct typ *t,
                                                    struct typ *intf,
                                                    bool *stop,
                                                    void *user) {
  (void) user;
  const struct node *deft = typ_definition_const(t);

  // FIXME: Remove
  if (typ_isa(t, TBI_ANY_TUPLE)) {
    return 0;
  }

  const struct node *dintf = typ_definition_const(intf);

  for (size_t m = 0; m < dintf->subs_count; ++m) {
    const struct node *f = dintf->subs[m];
    if (f->which != DEFFUN && f->which != DEFMETHOD) {
      continue;
    }

    if (node_get_member_const(mod, deft, node_ident(f)) == NULL) {
      error e = mk_except_type(mod, deft,
                               "type '%s' isa '%s' but does not implement '%s'",
                               typ_pretty_name(mod, deft->typ),
                               typ_pretty_name(mod, intf),
                               idents_value(mod->gctx, node_ident(f)));
      THROW(e);
    }

    // FIXME check that the prototype is an exact match.
  }

  return 0;
}

static error step_check_exhaustive_intf_impl(struct module *mod, struct node *node,
                                             void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFTYPE) {
    return 0;
  }

  if (typ_is_pseudo_builtin(node->typ)) {
    return 0;
  }

  error e = typ_isalist_foreach(mod, node->typ, ISALIST_FILTER_TRIVIAL_ISALIST,
                                check_exhaustive_intf_impl_eachisalist, NULL);
  EXCEPT(e);

  return 0;
}

static bool need_insert_dyn(struct module *mod,
                            const struct typ *intf,
                            const struct typ *concrete) {
  return
    typ_is_reference(intf)
    && typ_generic_arity(intf) > 0
    && typ_definition_const(typ_generic_arg_const(intf, 0))->which == DEFINTF
    && typ_is_reference(concrete)
    && typ_definition_const(typ_generic_arg_const(concrete, 0))->which != DEFINTF;
}

static error insert_dyn(struct module *mod, struct node *node,
                        const struct node *target, struct node *src) {
  struct node *d = mk_node(mod, node, DYN);
  set_typ(&d->as.DYN.intf_typ, target->typ);

  const size_t where = rew_find_subnode_in_parent(node, src);
  rew_move_last_over(node, where, TRUE);
  rew_append(d, src);

  const struct node *except[] = { target, src, NULL };
  error e = catchup(mod, except, d, node->scope, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  return 0;
}

static error try_insert_dyn(struct module *mod, struct node *node,
                            const struct node *target, struct node *src) {
  if (!need_insert_dyn(mod, target->typ, src->typ)) {
    return 0;
  }

  error e = insert_dyn(mod, node, target, src);
  EXCEPT(e);
  return 0;
}

static error step_dyn_inference(struct module *mod, struct node *node,
                                void *user, bool *stop) {
  DSTEP(mod, node);
  const struct node *target;
  struct node *src;

  error e;
  switch (node->which) {
  case RETURN:
    if (node->subs_count == 0) {
      return 0;
    }
    target = module_retval_get(mod);
    src = node->subs[0];

    e = try_insert_dyn(mod, node, target, src);
    EXCEPT(e);
    return 0;
  case BIN:
    if (node->as.BIN.operator == TASSIGN) {
      target = node->subs[0];
      src = node->subs[1];
      e = try_insert_dyn(mod, node, target, src);
      EXCEPT(e);
    }
    return 0;
  case DEFNAME:
    if (!(node->flags & NODE_IS_TYPE)) {
      target = node->as.DEFNAME.pattern;
      src = node->as.DEFNAME.expr;
      if (src != NULL) {
        e = try_insert_dyn(mod, node, target, src);
        EXCEPT(e);
      }
    }
    return 0;
  case TYPECONSTRAINT:
    target = node->subs[0];
    src = node->subs[1];
    e = try_insert_dyn(mod, node, target, src);
    EXCEPT(e);
    return 0;
  case CALL:
    if (node->flags & NODE_IS_TYPE) {
      return 0;
    }

    struct node *funargs = typ_definition(node->subs[0]->typ)->subs[IDX_FUNARGS];
    for (size_t n = 1; n < node->subs_count; ++n) {
      struct node *arg = node->subs[n];
      if (arg->which == BLOCK) {
        break;
      }
      target = funargs->subs[n - 1];
      src = arg;
      e = try_insert_dyn(mod, node, target, src);
      EXCEPT(e);
    }
    return 0;
  default:
    return 0;
  }
}

static bool is_block_like(struct node *node) {
  switch (node->which) {
  case IF:
  case TRY:
  case MATCH:
  case BLOCK:
    return TRUE;
  default:
    return FALSE;
  }
}

static void block_insert_value_assign(struct module *mod, struct node *block,
                                      struct node *target, ident target_name) {
  assert(block->which == BLOCK);

  const size_t where = block->subs_count - 1;
  struct node *last = block->subs[where];

  struct node *assign = mk_node(mod, block, BIN);
  assign->as.BIN.operator = TASSIGN;
  if (target != NULL) {
    rew_append(assign, target);
  } else {
    struct node *left = mk_node(mod, assign, IDENT);
    left->as.IDENT.name = target_name;
  }
  rew_move_last_over(block, where, TRUE);

  rew_append(assign, last);
  set_typ(&block->typ, TBI_VOID);

  const struct node *except[] = { last, NULL };
  error e = catchup(mod, except, assign, block->scope, CATCHUP_BELOW_CURRENT);
  assert(!e);
}

static void block_like_insert_value_assign(struct module *mod, struct node *node,
                                           struct node *target, ident target_name) {
  switch (node->which) {
  case IF:
    for (size_t n = 1; n < node->subs_count; n += 2) {
      block_insert_value_assign(mod, node->subs[n], target, target_name);
    }
    if (node->subs_count % 2 == 1) {
      struct node *els = node->subs[node->subs_count-1];
      block_insert_value_assign(mod, els, target, target_name);
    }
    break;
  case TRY:
    block_insert_value_assign(mod, node->subs[0], target, target_name);
    block_insert_value_assign(mod, node->subs[2], target, target_name);
    break;
  case MATCH:
    for (size_t n = 2; n < node->subs_count; n += 2) {
      block_insert_value_assign(mod, node->subs[n], target, target_name);
    }
    break;
  case BLOCK:
    block_insert_value_assign(mod, node, target, target_name);
    break;
  default:
    assert(FALSE);
    break;
  }
}

static error step_move_assign_in_block_like(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which != BIN || !OP_ASSIGN(node->as.BIN.operator)) {
    return 0;
  }

  struct node *left = node->subs[0];
  struct node *right = node->subs[1];
  if (!is_block_like(right)) {
    return 0;
  }

  block_like_insert_value_assign(mod, right, left, 0);

  struct scope *saved_parent = node->scope->parent;
  free(node->scope);
  memset(node, 0, sizeof(*node));
  *node = *right;
  fix_scopes_after_move(node);
  node->scope->parent = saved_parent;
  set_typ(&node->typ, TBI_VOID);

  return 0;
}

static error step_move_defname_expr_in_let_block(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which != DEFPATTERN) {
    return 0;
  }

  // Need to process them backwards for cases like:
  //   let x, y = block -> 0;;, block -> 1;;
  // where we prepend the blocks to the let-block, such that:
  //   let x, y
  //     block -> x = 0
  //     block -> y = 0
  struct node *defp_block = NULL;
  for (ssize_t n = node->subs_count - 1; n >= 0; --n) {
    struct node *d = node->subs[n];
    if (d->which != DEFNAME) {
      continue;
    }

    struct node *expr = d->as.DEFNAME.expr;
    if (expr == NULL) {
      continue;
    } else if (is_block_like(expr)) {
      block_like_insert_value_assign(mod, expr, d->as.DEFNAME.pattern, 0);

      if (defp_block == NULL) {
        struct node *let = node_parent(node);
        struct node *target_let_block = NULL;

        if (node_has_tail_block(let)) {
          const bool first_defpattern_in_let = rew_find_subnode_in_parent(let, node) == 0;
          if (first_defpattern_in_let) {
            struct node *let_block = let->subs[let->subs_count-1];
            target_let_block = mk_node(mod, let, BLOCK);
            rew_move_last_over(let, let->subs_count - 2, TRUE);
            rew_append(target_let_block, let_block);

            const struct node *except[] = { let_block, NULL };
            error e = catchup(mod, except, target_let_block, let->scope,
                              CATCHUP_AFTER_CURRENT);
            assert(!e);
          } else {
            target_let_block = let->subs[let->subs_count - 1];
          }
        } else {
          target_let_block = mk_node(mod, let, BLOCK);
          error e = catchup(mod, NULL, target_let_block, let->scope,
                            CATCHUP_AFTER_CURRENT);
          assert(!e);
        }

        defp_block = mk_node(mod, target_let_block, BLOCK);
        error e = catchup(mod, NULL, defp_block, target_let_block->scope,
                          CATCHUP_AFTER_CURRENT);
        assert(!e);
      }

      d->as.DEFNAME.expr = NULL;
      rew_prepend(defp_block, expr);
      expr->scope->parent = defp_block->scope;
    }
  }

  return 0;
}

static const struct node *retval_name(struct module *mod) {
  const struct node *retval = module_retval_get(mod);
  assert(retval->subs_count > 0);
  return retval->subs[0];
}

static error step_store_return_through_ref_expr(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *expr = NULL;
  struct node *init_expr = NULL;

  switch (node->which) {
  case RETURN:
    if (node->subs_count == 0
        || typ_equal(node->subs[0]->typ, TBI_VOID)) {
      return 0;
    }

    expr = node->subs[0];
    if (expr_is_return_through_ref(&init_expr, mod, expr)) {
      // Keep node->as.RETURN.return_through_ref_expr null as the
      // subexpression CALL or INIT will directly write to it.
      if (init_expr != NULL) {
        init_expr->as.INIT.target_expr = retval_name(mod);
        node->as.RETURN.forced_return_through_ref = TRUE;
      } else if (expr->which == CALL) {
        expr->as.CALL.return_through_ref_expr = retval_name(mod);
      } else if (is_block_like(expr)) {
        block_like_insert_value_assign(mod, expr, NULL, node_ident(retval_name(mod)));
      } else {
        assert(FALSE);
      }
    } else if (!typ_isa(expr->typ, TBI_RETURN_BY_COPY)
               && typ_isa(expr->typ, TBI_COPYABLE)) {
      // FIXME need to insert copy_ctor

      if (node->subs[0]->which == IDENT
          && node_ident(retval_name(mod)) == node_ident(node->subs[0])) {
        // noop
      } else if (is_block_like(expr)) {
        block_like_insert_value_assign(mod, expr, NULL, node_ident(retval_name(mod)));

        node->as.RETURN.return_through_ref_expr = retval_name(mod);
      } else {
        node->as.RETURN.return_through_ref_expr = retval_name(mod);
      }
    }
    return 0;
  case DEFPATTERN:
    for (ssize_t n = node->subs_count - 1; n >= 0; --n) {
      struct node *d = node->subs[n];
      if (d->which != DEFNAME) {
        continue;
      }
      expr = d->as.DEFNAME.expr;
      init_expr = NULL;
      if (expr == NULL) {
        // noop
      } else if (expr_is_literal_initializer(&init_expr, mod, expr)) {
        init_expr->as.INIT.target_expr = d->as.DEFNAME.pattern;
      } else if (expr->which == CALL && expr_is_return_through_ref(NULL, mod, expr)) {
        expr->as.CALL.return_through_ref_expr = d->as.DEFNAME.pattern;
      }
    }
    return 0;
  case BIN:
    if (!OP_ASSIGN(node->as.BIN.operator)) {
      return 0;
    }
    struct node *left = node->subs[0];
    struct node *right = node->subs[1];
    init_expr = NULL;
    if (expr_is_literal_initializer(&init_expr, mod, right)) {
      init_expr->as.INIT.target_expr = left;
    } else if (right->which == CALL && expr_is_return_through_ref(NULL, mod, right)) {
      right->as.CALL.return_through_ref_expr = left;
    }
    return 0;
  default:
    return 0;
  }
}

static bool is_significant(const struct node *node) {
  return node->which != TYPECONSTRAINT;
}

static void closest_significant_parent(struct node **parent, struct node *node) {
  struct node *n = node;
  do {
    n = node_parent(n);
  } while (!is_significant(n));
  *parent = n;
}

static bool block_like_needs_temporary(struct module *mod,
                                       struct node *node) {
  assert(is_block_like(node));
  struct node *significant_parent = NULL;
  closest_significant_parent(&significant_parent, node);

  if (is_block_like(significant_parent)) {
    return FALSE;
  }

  if (significant_parent->which == RETURN
      && !typ_isa(node->typ, TBI_RETURN_BY_COPY)) {
    return FALSE;
  } else if (significant_parent->which == DEFPATTERN) {
    return FALSE;
  } else if (significant_parent->which == BIN
             && OP_ASSIGN(significant_parent->as.BIN.operator)) {
    return FALSE;
  } else {
    return TRUE;
  }
}

struct temporaries {
  struct node **rvalues;
  ident *gensyms;
  size_t count;
};

static void temporaries_add(struct temporaries *temps, struct node *node) {
  temps->count += 1;
  temps->rvalues = realloc(temps->rvalues, temps->count * sizeof(*temps->rvalues));
  temps->rvalues[temps->count - 1] = node;
}

static error step_gather_temporary_rvalues(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  struct temporaries *temps = user;

  if (!is_significant(node)) {
    return 0;
  }

  struct node *significant_parent = NULL;
  closest_significant_parent(&significant_parent, node);

  switch (node->which) {
  case UN:
    if (OP_KIND(node->as.UN.operator) == OP_UN_REFOF
        && node_is_rvalue(node->subs[0])) {
      if (node->as.UN.operator != TREFDOT) {
        error e = mk_except(mod, node, "Cannot take a mutating reference of a rvalue");
        THROW(e);
      }

      temporaries_add(temps, node);
    }
    break;
  case INIT:
    if (significant_parent->which == RETURN
        && !typ_isa(node->typ, TBI_RETURN_BY_COPY)) {
      break;
    }
    if (significant_parent->which == DEFPATTERN) {
      break;
    }
    if (significant_parent->which == BIN
        && OP_ASSIGN(significant_parent->as.BIN.operator)) {
      break;
    }
    if (significant_parent->which == UN
        && OP_KIND(significant_parent->as.UN.operator) == OP_UN_REFOF) {
      break;
    }
    temporaries_add(temps, node);
    break;
  case CALL:
    if ((node->flags & NODE_IS_TYPE)) {
      break;
    }
    if (typ_isa(node->typ, TBI_RETURN_BY_COPY)) {
      break;
    }
    if (significant_parent->which == RETURN) {
      break;
    }
    if (significant_parent->which == BIN
        && OP_ASSIGN(significant_parent->as.BIN.operator)) {
      break;
    }
    if (significant_parent->which == UN
        && OP_KIND(significant_parent->as.UN.operator) == OP_UN_REFOF) {
      break;
    }
    if (significant_parent->which == DEFPATTERN) {
      break;
    }
    temporaries_add(temps, node);
    break;
  case TUPLEEXTRACT:
    if (node->subs[node->subs_count - 1]->which != IDENT) {
      temporaries_add(temps, node);
    }
    break;
  case IF:
  case TRY:
  case MATCH:
    if (typ_equal(node->typ, TBI_VOID)) {
      break;
    }

    if (!block_like_needs_temporary(mod, node)) {
      break;
    }

    temporaries_add(temps, node);
    break;
  case BLOCK:
    *stop = TRUE;
    break;
  default:
    return 0;
  }

  return 0;
}

static void declare_temporaries(struct module *mod, struct node *statement,
                                struct temporaries *temps) {
  temps->gensyms = calloc(temps->count, sizeof(*temps->gensyms));

  struct node *let = NULL;
  if (statement->which == LET) {
    let = statement;
  } else {
    // We are going to move the current stepping node down in the tree, and
    // it would normally be in the except list and not be processed by later
    // steps in the current pass. But these steps may be crucial as
    // 'statement' could be anything at all. So we must force these steps by
    // hand. Yes, it's hacky at best.

    struct node copy;
    copy = *statement;

    let = statement;
    memset(let, 0, sizeof(*let));
    let->which = LET;
    struct node *block = mk_node(mod, let, BLOCK);
    struct node *new_statement = node_new_subnode(mod, block);
    *new_statement = copy;
    fix_scopes_after_move(new_statement);

    const struct node *except[] = { new_statement, NULL };
    error e = catchup(mod, except, let, copy.scope->parent, CATCHUP_REWRITING_CURRENT);
    assert(!e);

    const struct pass *pa = &passes[mod->stage->state->passing];
    PUSH_STATE(mod->state->step_state);
    for (size_t s = mod->state->step_state->prev->stepping + 1; pa->ups[s] != NULL; ++s) {
      mod->state->step_state->upward = TRUE;
      mod->state->step_state->stepping = s;

      bool stop = FALSE;
      e = pa->ups[s](mod, new_statement, NULL, &stop);
      assert(!e);
      assert(!stop);
    }
    POP_STATE(mod->state->step_state);
  }

  statement = NULL;

  for (size_t n = 0; n < temps->count; ++n) {
    const struct node *rv = temps->rvalues[n];
    struct node *defp = mk_node(mod, let, DEFPATTERN);
    rew_insert_last_at(let, n);

    struct node *typc = mk_node(mod, defp, TYPECONSTRAINT);

    struct node *tmp_name = mk_node(mod, typc, IDENT);
    const ident g = gensym(mod);
    tmp_name->as.IDENT.name = g;
    temps->gensyms[n] = g;

    struct node *typ = mk_node(mod, typc, DIRECTDEF);
    if (rv->which == UN && OP_KIND(rv->as.UN.operator) == OP_UN_REFOF) {
      assert(typ_generic_arity(rv->typ) == 1);
      set_typ(&typ->as.DIRECTDEF.typ, typ_generic_arg(rv->typ, 0));
    } else {
      set_typ(&typ->as.DIRECTDEF.typ, rv->typ);
    }
    typ->as.DIRECTDEF.flags = NODE_IS_TYPE;

    error e = catchup(mod, NULL, let->subs[n], let->scope, CATCHUP_BELOW_CURRENT);
    assert(!e);
  }
}

static error step_define_temporary_rvalues(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (!node_is_statement(node)) {
    return 0;
  }

  static const step temprvalue_down[] = {
    NULL,
  };

  static const step temprvalue_up[] = {
    step_gather_temporary_rvalues,
    NULL,
  };

  struct temporaries temps = { 0 };

  PUSH_STATE(mod->state->step_state);
  error e = pass(mod, node, temprvalue_down, temprvalue_up, -1, &temps);
  EXCEPT(e);
  POP_STATE(mod->state->step_state);

  if (temps.count == 0) {
    return 0;
  }

  declare_temporaries(mod, node, &temps);

  for (size_t n = 0; n < temps.count; ++n) {
    const ident g = temps.gensyms[n];
    struct node *rv = temps.rvalues[n];

    struct node *rv_parent = NULL;
    closest_significant_parent(&rv_parent, rv);
    const size_t rv_where = rew_find_subnode_in_parent(rv_parent, rv);

    struct node *nrv = mk_node(mod, rv_parent, BLOCK);
    rew_move_last_over(rv_parent, rv_where, TRUE);
    struct node *assign = mk_node(mod, nrv, BIN);
    assign->as.BIN.operator = TASSIGN;

    struct node *target = mk_node(mod, assign, IDENT);
    target->as.IDENT.name = g;

    const struct node *except[2];
    except[1] = NULL;

    if (rv->which == UN && OP_KIND(rv->as.UN.operator) == OP_UN_REFOF) {
      rv->subs[0]->flags |= NODE_IS_TEMPORARY;
      rew_append(assign, rv->subs[0]);
      except[0] = rv->subs[0];

      struct node *nvalue = mk_node(mod, nrv, UN);
      nvalue->as.UN.operator = rv->as.UN.operator;
      struct node *nvalue_name = mk_node(mod, nvalue, IDENT);
      nvalue_name->as.IDENT.name = g;
    } else if (rv->which == TUPLEEXTRACT) {
      struct node *tuple = rv->subs[rv->subs_count - 1];
      rew_pop(rv, TRUE);
      rew_append(assign, tuple);
      except[0] = tuple;

      struct node *extractor = mk_node(mod, nrv, TUPLEEXTRACT);
      for (size_t n = 0; n < typ_generic_arity(tuple->typ); ++n) {
        // We want to reuse the original TUPLENTH from 'rv' as they may be
        // pointed to by nearby DEFNAME.expr, so their location in memory
        // cannot change.
        struct node *nth = rv->subs[n];
        memset(nth, 0, sizeof(*nth));
        nth->which = TUPLENTH;
        nth->as.TUPLENTH.nth = n;
        rew_append(extractor, nth);
      }
      struct node *nvalue = mk_node(mod, extractor, IDENT);
      nvalue->as.IDENT.name = g;

    } else {
      rv->flags |= NODE_IS_TEMPORARY;
      rew_append(assign, rv);
      except[0] = rv;

      struct node *nvalue = mk_node(mod, nrv, IDENT);
      nvalue->as.IDENT.name = g;
    }

    e = catchup(mod, except, nrv, rv_parent->scope, CATCHUP_BELOW_CURRENT);
    EXCEPT(e);
  }

  free(temps.rvalues);
  free(temps.gensyms);

  return 0;
}

static error do_complete_instantiation(struct module *mod, struct node *node) {
  const size_t goal = mod->stage->state->passing;

  PUSH_STATE(mod->stage->state);
  PUSH_STATE(mod->state->step_state);

  struct toplevel *toplevel = node_toplevel(node);
  for (ssize_t p = toplevel->yet_to_pass; p <= goal; ++p) {
    const struct pass *pa = &passes[p];
    mod->stage->state->passing = p;

    int module_depth = 0;
    error e = pass(mod, node, pa->downs, pa->ups, -1, &module_depth);
    EXCEPT(e);

    toplevel->yet_to_pass = p + 1;

    if (node->which == DEFTYPE) {
      for (size_t n = 0; n < node->as.DEFTYPE.members_count; ++n) {
        struct node *m = node->as.DEFTYPE.members[n];
        if (node_toplevel_const(m)->builtingen != BG__NOT) {
          continue;
        }

        error e = pass(mod, m, pa->downs, pa->ups, -1, &module_depth);
        EXCEPT(e);

        node_toplevel(m)->yet_to_pass = p + 1;
      }
    }
  }

  POP_STATE(mod->state->step_state);
  POP_STATE(mod->stage->state);

  return 0;
}

static error step_complete_instantiation(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  struct toplevel *toplevel = node_toplevel(node);
  if (toplevel == NULL) {
    return 0;
  }

  if (toplevel->yet_to_pass > mod->stage->state->passing) {
    return 0;
  }

  for (size_t n = 1; n < toplevel->instances_count; ++n) {
    struct node *i = toplevel->instances[n];
    error e = do_complete_instantiation(mod, i);
    EXCEPT(e);
  }

  toplevel->yet_to_pass = mod->stage->state->passing + 1;

  return 0;
}

static const struct pass _passes[] = {
  {
    PASS_ZERO, "zero",
    {
      step_rewrite_prototype_wildcards,
      step_generics_pristine_copy,
      step_detect_prototypes,
      step_detect_deftype_kind,
      step_assign_deftype_which_values,
      NULL,
    },
    {
      step_add_scopes,
      NULL,
    }
  },

  {
    PASS_FORWARD, "scoping_deftypes",
    {
      step_stop_submodules,
      step_codeloc_for_generated,
      step_defpattern_extract_defname,
      step_type_gather_excepts,
      step_lexical_scoping,
      NULL,
    },
    {
      step_type_definitions,
      step_type_drop_excepts,
      step_gather_final_instantiations,
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_FORWARD, "imports",
    {
      step_stop_submodules,
      step_lexical_import,
      step_add_builtin_members,
      NULL,
    },
    {
      step_gather_final_instantiations,
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_FORWARD, "type_genargs",
    {
      step_stop_submodules,
      step_stop_marker_tbi,
      step_stop_funblock,
      step_type_inference_genargs,
      NULL,
    },
    {
      step_gather_final_instantiations,
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_FORWARD, "type_isalist",
    {
      step_stop_submodules,
      step_type_create_update,
      step_stop_marker_tbi,
      step_stop_funblock,
      NULL,
    },
    {
      step_type_inference_isalist,
      step_gather_final_instantiations,
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_FORWARD, "type_complete_create",
    {
      step_stop_submodules,
      step_type_update_quickisa,
      step_stop_marker_tbi,
      step_stop_funblock,
      NULL,
    },
    {
      NULL,
    }
  },

  {
    PASS_FORWARD, "type_add_builtin_intf",
    {
      step_stop_submodules,
      step_stop_marker_tbi,
      step_stop_funblock,
      NULL,
    },
    {
      step_add_builtin_enum_intf,
      step_add_builtin_detect_ctor_intf,
      step_gather_final_instantiations,
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_FORWARD, "type_lets",
    {
      step_stop_submodules,
      step_stop_marker_tbi,
      step_stop_funblock,
      NULL,
    },
    {
      step_type_lets,
      step_gather_final_instantiations,
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_FORWARD, "type_deffields",
    {
      step_stop_submodules,
      step_stop_marker_tbi,
      step_stop_funblock,
      NULL,
    },
    {
      step_type_deffields,
      step_type_defchoices,
      step_gather_final_instantiations,
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_FORWARD, "type_deffuns",
    {
      step_stop_submodules,
      step_stop_marker_tbi,
      step_stop_funblock,
      NULL,
    },
    {
      step_type_deffuns,
      step_add_builtin_defchoice_mk_new,
      step_add_builtin_defchoice_constructors,
      step_add_builtin_ctor,
      step_add_builtin_dtor,
      step_add_builtin_mk_new,
      step_add_builtin_mkv_newv,
      step_add_builtin_operators,
      step_add_trivials,
      step_add_sum_dispatch,
      step_rewrite_def_return_through_ref,
      step_gather_final_instantiations,
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_BODY, "first",
    {
      step_stop_submodules,
      step_stop_marker_tbi,
      step_stop_already_morningtypepass,
      step_push_fun_state,
      step_detect_not_dyn_intf_down,
      step_rewrite_wildcards,
      step_type_destruct_mark,
      step_type_mutability_mark,
      step_type_gather_retval,
      step_type_gather_excepts,
      NULL,
    },
    {
      step_excepts_store_label,
      step_rewrite_defname_no_expr,
      step_rewrite_sum_constructors,
      step_detect_not_dyn_intf_up,
      step_type_inference,
      step_remove_typeconstraints,
      step_type_drop_excepts,
      step_check_exhaustive_match,
      step_gather_final_instantiations,
      step_pop_fun_state,
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_BODY, "second",
    {
      step_stop_submodules,
      step_stop_marker_tbi,
      step_push_fun_state,
      step_type_gather_retval,
      NULL,
    },
    {
      step_literal_conversion,
      step_operator_call_inference,
      step_operator_test_call_inference,
      step_ctor_call_inference,
      step_array_ctor_call_inference,
      step_dtor_call_inference,
      step_copy_call_inference,
      step_check_exhaustive_intf_impl,
      step_dyn_inference,

      step_define_temporary_rvalues,
      step_move_assign_in_block_like,
      step_move_defname_expr_in_let_block,
      step_store_return_through_ref_expr,

      step_pop_fun_state,
      step_complete_instantiation,
      NULL,
    }
  },

  { PASS__NONE, NULL, { NULL }, { NULL } },
};

const struct pass *passes = _passes;
