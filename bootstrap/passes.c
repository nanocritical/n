#include "passes.h"
#include "table.h"

//#define DEBUG_PASS

#ifdef DEBUG_PASS
#define DSTEP(mod, node) do { \
  ident id = node_ident(node); \
  if (id != ID__NONE) { \
    fprintf(stderr, "%s %s\n", __func__, idents_value(mod->gctx, id)); \
  } else { \
    fprintf(stderr, "%s\n", __func__); \
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
      mod->state->upward = FALSE;
      mod->state->stepping = s;
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
      mod->state->upward = TRUE;
      mod->state->stepping = s;
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

enum catchup_for {
  CATCHUP_BELOW_CURRENT = 0,
  CATCHUP_REWRITING_CURRENT,
  CATCHUP_AFTER_CURRENT, // depth-first order in the tree of nodes
  CATCHUP_NEW_INSTANCE,
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

  ssize_t goal;
  if (how == CATCHUP_NEW_INSTANCE) {
    goal = mod->stage->state->passing;
  } else if (!mod->state->upward) {
    goal = mod->stage->state->passing - 1;
  } else if (how != CATCHUP_BELOW_CURRENT) {
    goal = mod->stage->state->passing - 1;
  } else {
    goal = mod->stage->state->passing;
  }

  PUSH_STATE(mod->stage->state);
  PUSH_STATE(mod->state);

  for (ssize_t p = 0; p <= goal; ++p) {
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

  if (mod->state->upward && how == CATCHUP_REWRITING_CURRENT) {
    // Catch up to, and including, the current step.
    const struct pass *pa = &passes[goal + 1];
    mod->stage->state->passing = goal + 1;

    int module_depth = 0;
    error e = pass(mod, node,
                   pa->downs, pa->ups, mod->state->prev->stepping,
                   &module_depth);
    EXCEPT(e);
  }

  POP_STATE(mod->state);
  POP_STATE(mod->stage->state);

  if (except != NULL) {
    POP_STATE(mod->excepts);
  }

  return 0;
}

static error catchup_instantiation(struct module *gendef_mod,
                                   struct node *instance,
                                   struct scope *parent_scope) {
  error e = catchup(gendef_mod, NULL, instance, parent_scope, CATCHUP_NEW_INSTANCE);
  EXCEPT(e);

  if (instance->which == DEFTYPE) {
    for (size_t n = 0; n < instance->as.DEFTYPE.members_count; ++n) {
      struct node *m = instance->as.DEFTYPE.members[n];
      if (node_toplevel_const(m)->builtingen != BG__NOT) {
        continue;
      }

      error e = catchup(gendef_mod, NULL, m, instance->scope, CATCHUP_NEW_INSTANCE);
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

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    for (size_t n = IDX_FUN_FIRSTARG; n < node->subs_count; ++n) {
      struct node *arg = node->subs[n];
      if (arg->which == BLOCK) {
        break;
      }

      PUSH_STATE(mod->state);
      error e = pass(mod, arg, down, up, -1, NULL);
      EXCEPT(e);
      POP_STATE(mod->state);
    }
    break;
  default:
    break;
  }

  return 0;
}

static struct node *add_instance_deepcopy_from_pristine(struct module *mod,
                                                        struct node *node,
                                                        struct node *pristine) {
  struct node *instance = calloc(1, sizeof(struct node));
  node_deepcopy(mod, instance, pristine);

  struct toplevel *toplevel = node_toplevel(node);
  const size_t idx = toplevel->instances_count;
  toplevel->instances_count += 1;
  toplevel->instances = realloc(toplevel->instances,
                                toplevel->instances_count * sizeof(*toplevel->instances));
  toplevel->instances[idx] = instance;

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
      (void) add_instance_deepcopy_from_pristine(mod, node, node);
    }
    break;
  case DEFFUN:
  case DEFMETHOD:
    // Always needed because the method/fun could be part of a generic
    // DEFTYPE, and we cannot know that yet.
    (void) add_instance_deepcopy_from_pristine(mod, node, node);
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
  EXCEPT(e);
  return 0;
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

static void do_mk_expr_abspath(struct module *mod, struct node *node, const char *path, ssize_t len) {
  assert(node->which == BIN);
  node->as.BIN.operator = TDOT;

  for (ssize_t i = len-1; i >= 0; --i) {
    if (i == 0) {
      assert(len > 1);
      ident id = idents_add_string(mod->gctx, path, len - i);

      struct node *root = mk_node(mod, node, DIRECTDEF);
      root->as.DIRECTDEF.definition = &mod->gctx->modules_root;
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

static error step_add_builtin_members(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
    break;
  default:
    return 0;
  }

  {
    struct node *let = mk_node(mod, node, LET);
    struct node *defp = mk_node(mod, let, DEFPATTERN);
    defp->as.DEFPATTERN.is_alias = TRUE;
    struct node *name = mk_node(mod, defp, IDENT);
    name->as.IDENT.name = ID_THIS;
    struct node *expr = mk_node(mod, defp, IDENT);
    expr->as.IDENT.name = node_ident(node);

    rew_insert_last_at(node, 3);
  }

  {
    struct node *let = mk_node(mod, node, LET);
    struct node *defp = mk_node(mod, let, DEFPATTERN);
    defp->as.DEFPATTERN.is_alias = TRUE;
    struct node *name = mk_node(mod, defp, IDENT);
    name->as.IDENT.name = ID_FINAL;
    struct node *expr = mk_node(mod, defp, IDENT);
    expr->as.IDENT.name = node_ident(node);

    rew_insert_last_at(node, 4);
  }

  return 0;
}

static error extract_defnames_in_pattern(struct module *mod, struct node *defpattern,
                                         struct node *pattern, struct node *expr) {
  struct node *defn;
  error e;

  if (expr != NULL
      && pattern->which != expr->which
      && pattern->which != IDENT) {
    // FIXME
    e = mk_except(mod, pattern, "value destruct not yet supported");
    EXCEPT(e);
  }

#define UNLESS_NULL(n, sub) ( (n) != NULL ? (sub) : NULL )

  switch (pattern->which) {
  case IDENT:
    if (node_ident(pattern) == ID_OTHERWISE) {
      return 0;
    }
    defn = mk_node(mod, defpattern, DEFNAME);

    defn->as.DEFNAME.pattern = pattern;
    defn->as.DEFNAME.expr = expr;

    e = catchup(mod, NULL, defn, defpattern->scope, CATCHUP_BELOW_CURRENT);
    EXCEPT(e);
    return 0;
  case UN:
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
  case EXCEP:
    e = extract_defnames_in_pattern(mod, defpattern, pattern->subs[0],
                                    UNLESS_NULL(expr, expr->subs[0]));
    EXCEPT(e);
    break;
  case TYPECONSTRAINT:
    pattern->as.TYPECONSTRAINT.in_pattern = TRUE;
    e = extract_defnames_in_pattern(mod, defpattern, pattern->subs[0],
                                    UNLESS_NULL(expr, expr->subs[0]));
    EXCEPT(e);
    break;
  default:
    e = mk_except(mod, pattern, "invalid construct in pattern");
    EXCEPT(e);
    break;
  }
#undef UNLESS_NULL

  return 0;
}

static error step_defpattern_extract_defname(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFPATTERN) {
    return 0;
  }

  if (node_ident(node) == ID_OTHERWISE) {
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

// Recursive, depth first; Will modify scope on the way back up.
static error lexical_import_path(struct scope **scope, struct module *mod,
                                 struct node *import_path, struct node *import) {
  assert(import == NULL || import->which == IMPORT);
  error e;
  struct node *i = NULL;

  switch (import_path->which) {
  case IDENT:
    i = import_path;
    break;
  case BIN:
    assert(import_path->as.BIN.operator == TDOT);
    e = lexical_import_path(scope, mod, import_path->subs[0], NULL);
    EXCEPT(e);
    i = import_path->subs[1];
    break;
  default:
    assert(FALSE);
  }

  assert((*scope)->parent != NULL);
  if (import != NULL) {
    e = scope_define_ident(mod, *scope, i->as.IDENT.name, import);
    EXCEPT(e);
    return 0;
  }

  struct node *n = NULL;
  e = scope_lookup_ident_wontimport(&n, i, mod, *scope, i->as.IDENT.name, TRUE);
  if (e == EINVAL) {
    n = import_path;
    e = scope_define_ident(mod, *scope, i->as.IDENT.name, n);
    EXCEPT(e);
  } else if (e) {
    // Repeat bound-to-fail lookup to get the error message right.
    e = scope_lookup_ident_wontimport(&n, i, mod, *scope, i->as.IDENT.name, FALSE);
    EXCEPT(e);
  }

  *scope = n->scope;

  return 0;
}

static error lexical_import_from_path(struct scope *scope, struct module *mod,
                                      struct node *import) {
  error e;
  for (size_t n = 1; n < import->subs_count; ++n) {
    struct node *full_import_path = import->subs[n]->subs[0];
    assert(full_import_path->which == BIN);
    assert(full_import_path->subs[1]->which == IDENT);
    ident id = node_ident(full_import_path->subs[1]);

    // We don't use target, but we check it exists.
    struct node *def = NULL;
    struct node *target = NULL;
    e = scope_lookup_module(&def, mod, full_import_path->subs[0], FALSE);
    EXCEPT(e);
    e = scope_lookup_ident_immediate(&target, full_import_path->subs[1],
                                     mod, def->scope, id, FALSE);
    EXCEPT(e);

    e = scope_define_ident(mod, scope, id, import->subs[n]);
    EXCEPT(e);
  }

  return 0;
}

static error lexical_import(struct scope *scope, struct module *mod, struct node *import);

static bool is_forward_declaration(struct module *mod, struct node *node) {
  struct node *d = NULL;
  error e = scope_lookup_ident_wontimport(&d, node, mod, mod->body->scope,
                                          node_ident(node), FALSE);
  assert(!e);

  return d != node;
}

static error lexical_import_all_from_path(struct scope *scope, struct module *mod,
                                          struct node *import) {
  error e;
  struct node *target = NULL;
  e = scope_lookup_module(&target, mod, import->subs[0], FALSE);
  EXCEPT(e);

  if (target->which != MODULE) {
    e = mk_except_type(mod, import, "cannot import * from a non-module");
    EXCEPT(e);
  }

  static const step down[] = {
    NULL,
  };
  static const step up[] = {
    step_add_scopes,
    NULL,
  };

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
      e = lexical_import(scope, target_mod, ex);
      EXCEPT(e);

      continue;
    }

    if (is_forward_declaration(target_mod, ex)) {
      continue;
    }

    struct node *imported = node_new_subnode(mod, import);
    imported->which = IMPORT;
    imported->as.IMPORT.toplevel.is_export = import->as.IMPORT.toplevel.is_export;

    const ident id = node_ident(ex);
    struct token tok = { 0 };
    tok.t = TIDENT;
    tok.value = idents_value(mod->gctx, id);
    tok.len = strlen(tok.value);

    copy_and_extend_import_path(mod, imported, import, &tok);

    e = scope_define_ident(mod, scope, id, imported);
    EXCEPT(e);

    PUSH_STATE(mod->state);
    e = pass(mod, imported, down, up, -1, NULL);
    EXCEPT(e);
    imported->scope->parent = import->scope;
    POP_STATE(mod->state);
  }

  return 0;
}

static error lexical_import(struct scope *scope, struct module *mod, struct node *import) {
  assert(import->which == IMPORT);
  error e;

  struct node *import_path = import->subs[0];

  if (import->as.IMPORT.is_all) {
    // from <path> (import|export) *
    e = lexical_import_all_from_path(scope, mod, import);
    EXCEPT(e);
  } else if (import->subs_count == 1) {
    // (import|export) <path>
    struct scope *tmp = scope;
    e = lexical_import_path(&tmp, mod, import_path, import);
    EXCEPT(e);
  } else {
    // from <path> (import|export) <a> <b> <c> ...
    e = lexical_import_from_path(scope, mod, import);
    EXCEPT(e);
  }

  return 0;
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
  default:
    e = mk_except(mod, retval, "return value type expression not supported");
    EXCEPT(e);
    break;
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
    if (toplevel->generic_definition != NULL) {
      sc = NULL;

      // For generic instances, define the name in its own scope, to make
      // sure lookups inside the instance resolve to the instance itself
      // (e.g. the definition of this).
      e = scope_define(mod, node->scope, id, node);
      EXCEPT(e);
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
          && ctoplevel->generic_definition == NULL
          && container->subs[IDX_GENARGS]->subs_count > 0
          && container->subs[IDX_GENARGS]->subs[0]->which == DEFGENARG) {
        add_deftype_pristine_external_member(mod, container, node);
      }
    }
    break;
  case DEFTYPE:
  case DEFINTF:
    if (node_toplevel_const(node)->generic_definition != NULL) {
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
    id = node->as.DEFNAME.pattern;
    sc = node->scope->parent->parent->parent;
    break;
  case TRY:
    id = node->subs[1];
    sc = node->scope;
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

    size_t args_count = node_fun_all_args_count(node);
    for (size_t n = 0; n < args_count; ++n) {
      struct node *arg = node->subs[IDX_FUN_FIRSTARG+n];
      assert(arg->which == DEFARG);
      e = scope_define(mod, node->scope, arg->subs[0], arg);
      EXCEPT(e);
    }

    e = lexical_retval(mod, node, node_fun_retval(node));
    EXCEPT(e);
    break;
  default:
    break;
  }

  return 0;
}

static error step_lexical_import(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  error e;

  switch (node->which) {
  case IMPORT:
    if (node_is_at_top(node)) {
      e = lexical_import(node->scope->parent, mod, node);
      EXCEPT(e);
    }
    return 0;
  default:
    return 0;
  }
}

static void mark_subs(struct module *mod, struct node *node, const struct typ *mark,
                      size_t begin, size_t end, size_t incr) {
  for (size_t n = begin; n < end; n += incr){
    node->subs[n]->typ = mark;
  }
}

static void inherit(struct module *mod, struct node *node) {
  const struct typ *pending = typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT);
  const struct typ *not_typeable = typ_lookup_builtin(mod, TBI__NOT_TYPEABLE);
  if (node->typ == pending || node->typ == not_typeable) {
    mark_subs(mod, node, node->typ, 0, node->subs_count, 1);
  }
}

static error step_stop_marker_tbi(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node_ident(node)) {
  case ID_TBI__PENDING_DESTRUCT:
  case ID_TBI__NOT_TYPEABLE:
  case ID_TBI__CALL_FUNCTION_SLOT:
    *stop = TRUE;
    return 0;
  default:
    return 0;
  }
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
    PUSH_STATE(mod->fun_state);
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
    POP_STATE(mod->fun_state);
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
    mod->fun_state->fun_uses_final = FALSE;
    break;
  default:
    break;
  }
  return 0;
}

static error step_detect_not_dyn_intf_up(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  if (mod->fun_state == NULL) {
    // Not in a function.
    return 0;
  }

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    node_toplevel(node)->is_not_dyn = mod->fun_state->fun_uses_final
      || node->subs[IDX_GENARGS]->subs_count != 0;
    break;
  case IDENT:
    if (node_ident(node) == ID_FINAL) {
      mod->fun_state->fun_uses_final = TRUE;
    }
    break;
  case DEFARG:
    if (rew_find_subnode_in_parent(node_parent(node), node) == IDX_FUN_FIRSTARG
        && node_parent(node)->which == DEFMETHOD) {
      // We just found self as a method argument on the way up, doesn't count.
      assert(mod->fun_state->fun_uses_final);
      mod->fun_state->fun_uses_final = FALSE;
    }
    break;
  default:
    break;
  }
  return 0;
}

static error step_rewrite_wildcards(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
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
    if (typ_equal(mod, def->typ, typ_lookup_builtin(mod, TBI_REF))) {
      mod->fun_state->ref_wildcard = TREFDOT;
      mod->fun_state->nulref_wildcard = TNULREFDOT;
      mod->fun_state->deref_wildcard = TDEREFDOT;
      mod->fun_state->wildcard = TDOT;
    } else if (typ_equal(mod, def->typ, typ_lookup_builtin(mod, TBI_MREF))) {
      mod->fun_state->ref_wildcard = TREFBANG;
      mod->fun_state->nulref_wildcard = TNULREFBANG;
      mod->fun_state->deref_wildcard = TDEREFBANG;
      mod->fun_state->wildcard = TBANG;
    } else if (typ_equal(mod, def->typ, typ_lookup_builtin(mod, TBI_MMREF))) {
      mod->fun_state->ref_wildcard = TREFSHARP;
      mod->fun_state->nulref_wildcard = TNULREFSHARP;
      mod->fun_state->deref_wildcard = TDEREFSHARP;
      mod->fun_state->wildcard = TSHARP;
    } else {
      assert(FALSE);
    }
    break;
  case UN:
    switch (node->as.UN.operator) {
    case TREFWILDCARD:
      node->as.UN.operator = mod->fun_state->ref_wildcard;
      break;
    case TNULREFWILDCARD:
      node->as.UN.operator = mod->fun_state->nulref_wildcard;
      break;
    case TDEREFWILDCARD:
      node->as.UN.operator = mod->fun_state->deref_wildcard;
      break;
    default:
      break;
    }
    break;
  case BIN:
    switch (node->as.BIN.operator) {
    case TWILDCARD:
      node->as.UN.operator = mod->fun_state->wildcard;
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

static error step_type_destruct_mark(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which == MODULE) {
    return 0;
  }

  inherit(mod, node);

  const struct typ *pending = typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT);
  const struct typ *not_typeable = typ_lookup_builtin(mod, TBI__NOT_TYPEABLE);

  switch (node->which) {
  case DEFARG:
  case DEFGENARG:
  case SETGENARG:
  case TYPECONSTRAINT:
    mark_subs(mod, node, pending, 0, 1, 1);
    break;
  case TRY:
    mark_subs(mod, node, pending, 1, 2, 1);
    break;
  case MATCH:
    mark_subs(mod, node, pending, 1, node->subs_count, 2);
    break;
  case BIN:
    switch (OP_KIND(node->as.BIN.operator)) {
    case OP_BIN_RHS_TYPE:
      mark_subs(mod, node, pending, 0, 1, 1);
      break;
    case OP_BIN_ACC:
      mark_subs(mod, node, not_typeable, 1, node->subs_count, 1);
      break;
    case OP_BIN_SYM:
    case OP_BIN_SYM_BOOL:
    case OP_BIN_SYM_NUM:
    case OP_BIN_SYM_PTR:
    case OP_BIN_NUM_RHS_UNSIGNED:
      break;
    default:
      assert(FALSE);
      break;
    }
    break;
  case INIT:
    if (!node->as.INIT.is_array) {
      mark_subs(mod, node, pending, 0, node->subs_count, 2);
    }
    break;
  case CALL:
    if (node->subs[0]->typ == NULL) {
      // Otherwise, we are rewriting this expression and we should not touch
      // subs[0].
      mark_subs(mod, node, typ_lookup_builtin(mod, TBI__CALL_FUNCTION_SLOT), 0, 1, 1);
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
    node->subs[0]->typ = not_typeable;
    break;
  case DEFPATTERN:
    if (node->subs_count > 1 && node->subs[1]->which != DEFNAME) {
      node->subs[0]->typ = pending;
      mark_subs(mod, node, pending, 2, node->subs_count, 1);
    } else {
      mark_subs(mod, node, pending, 1, node->subs_count, 1);
    }
    break;
  case MODULE_BODY:
    node->typ = not_typeable;
    break;
  case DEFCHOICE:
    node->subs[0]->typ = not_typeable;
    break;
  case IMPORT:
    mark_subs(mod, node, pending, 0, node->subs_count, 1);
    break;
  default:
    break;
  }

  return 0;
}

static error step_type_mutability_mark(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  const struct typ *mutable = typ_lookup_builtin(mod, TBI__MUTABLE);
  const struct typ *mercurial = typ_lookup_builtin(mod, TBI__MERCURIAL);

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
    struct node *arg= node->subs[0];
    switch (node->as.UN.operator) {
    case TREFDOT:
      // no-op
      break;
    case TREFBANG:
      if (arg->typ != NULL) {
        if (arg->which == BIN && !(arg->flags & NODE_IS_TYPE)) {
          error e = typ_check_deref_against_mark(mod, arg,
                                                 typ_lookup_builtin(mod, TBI__MUTABLE),
                                                 arg->as.BIN.operator);
          EXCEPT(e);
        }
      } else {
        mark_subs(mod, node, mutable, 0, 1, 1);
      }
      break;
    case TREFSHARP:
      if (arg->typ != NULL) {
        if (arg->which == BIN && !(arg->flags & NODE_IS_TYPE)) {
          error e = typ_check_deref_against_mark(mod, arg,
                                                 typ_lookup_builtin(mod, TBI__MERCURIAL),
                                                 arg->as.BIN.operator);
          EXCEPT(e);
        }
        return 0;
      } else {
        mark_subs(mod, node, mercurial, 0, 1, 1);
      }
      break;
    case TREFWILDCARD:
      assert(FALSE);
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
  case ISA:
  case DEFGENARG:
  case SETGENARG:
  case DEFFIELD:
  case DEFCHOICE:
    *stop = TRUE;
    return 0;
  default:
    return 0;
  }
}

static error step_type_inference(struct module *mod, struct node *node, void *user, bool *stop);

static error morningtypepass(struct module *mod, struct node *node) {
  static const step down[] = {
    step_stop_marker_tbi,
    step_stop_block,
    step_type_destruct_mark,
    NULL,
  };

  static const step up[] = {
    step_type_inference,
    NULL,
  };

  PUSH_STATE(mod->state);
  error e = pass(mod, node, down, up, -1, NULL);
  EXCEPT(e);
  POP_STATE(mod->state);

  return 0;
}

static error step_type_deftypes_defintfs(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
    break;
  default:
    return 0;
  }

  assert(node->subs[0]->which == IDENT);
  ident id = node_ident(node->subs[0]);

  if (node->subs[IDX_GENARGS]->subs_count > 0
      && node_toplevel(node)->generic_definition != NULL) {
    struct typ *mutable_typ = typ_new(node_toplevel(node)->generic_definition,
                                      TYPE_DEF, node->subs[IDX_GENARGS]->subs_count, 0);
    mutable_typ->gen_args[0] = node_toplevel(node)->generic_definition->typ;
    mutable_typ->definition = node;
    node->typ = mutable_typ;

    for (size_t n = 0; n < node->typ->gen_arity; ++n) {
      assert(node->subs[IDX_GENARGS]->subs[n]->typ != NULL);
      node->typ->gen_args[1 + n] = node->subs[IDX_GENARGS]->subs[n]->typ;
    }
  } else if (mod->path[0] == ID_NLANG
             && (id >= ID_TBI__FIRST && id <= ID_TBI__LAST)) {
    // FIXME Effectively reserving these idents for builtin types, but
    // that's a temporary trick to avoid having to look up the current
    // module path.
    struct typ *t = mod->gctx->builtin_typs_by_name[id];
    t->definition = node;
    node->typ = t;
  } else {
    node->typ = typ_new(node, TYPE_DEF, 0, 0);
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
  case DEFFUN:
  case DEFMETHOD:
    break;
  default:
    return 0;
  }

  if (node->typ == typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT)
      || node->typ == typ_lookup_builtin(mod, TBI__NOT_TYPEABLE)) {
    return 0;
  }

  struct node *genargs = node->subs[IDX_GENARGS];
  e = morningtypepass(mod, genargs);
  EXCEPT(e);

  return 0;
}

static error step_type_inference_isalist(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  error e;
  struct isalist *tisalist = NULL;

  switch (node->which) {
  case ISA:
    e = morningtypepass(mod, node);
    EXCEPT(e);
    return 0;
  case DEFTYPE:
    tisalist = &node->as.DEFTYPE.isalist;
    break;
  case DEFINTF:
    tisalist = &node->as.DEFINTF.isalist;
    break;
  default:
    return 0;
  }

  if (node->typ == typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT)
      || node->typ == typ_lookup_builtin(mod, TBI__NOT_TYPEABLE)) {
    return 0;
  }

  struct node *isalist = node->subs[IDX_ISALIST];

  // FIXME: Check for duplicates?

  tisalist->count = isalist->subs_count;
  tisalist->list = calloc(isalist->subs_count, sizeof(*tisalist->list));
  tisalist->exported = calloc(isalist->subs_count, sizeof(*tisalist->exported));

  for (size_t n = 0; n < isalist->subs_count; ++n) {
    struct node *isa = isalist->subs[n];
    assert(isa->which == ISA);

    if (isa->typ->definition->which != DEFINTF) {
      e = mk_except_type(mod, isa, "not an intf");
      EXCEPT(e);
    }

    tisalist->list[n] = isa->typ;
    tisalist->exported[n] = isa->as.ISA.is_export;
  }

  return 0;
}

static error step_type_aliases(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  struct node *parent = node_parent(node);
  error e;
  switch (node->which) {
  case IMPORT:
    if (node_is_at_top(node)) {
      e = morningtypepass(mod, node);
      EXCEPT(e);
    }
    return 0;
  case LET:
    if (node->subs[0]->as.DEFPATTERN.is_alias
        && (node_is_at_top(node) || node_is_at_top(parent))) {
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

static error type_destruct(struct module *mod, struct node *node, const struct typ *constraint);

static error step_type_defchoices(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  error e;
  switch (node->which) {
  case DEFTYPE:
    switch (node->as.DEFTYPE.kind) {
    case DEFTYPE_ENUM:
    case DEFTYPE_SUM:
      {
        const struct typ *u = typ_lookup_builtin(mod, TBI_LITERALS_INTEGER);
        for (size_t n = 0; n < node->subs_count; ++n) {
          struct node *ch = node->subs[n];
          if (ch->which != DEFCHOICE) {
            continue;
          }
          e = typ_unify(&u, mod, ch,
                        u, ch->subs[IDX_CH_VALUE]->typ);
          EXCEPT(e);
          ch->flags |= NODE_IS_DEFCHOICE;
        }

        if (typ_equal(mod, u, typ_lookup_builtin(mod, TBI_LITERALS_INTEGER))) {
          u = typ_lookup_builtin(mod, TBI_U32);
        }
        node->as.DEFTYPE.choice_typ = u;

        for (size_t n = 0; n < node->subs_count; ++n) {
          struct node *ch = node->subs[n];
          if (ch->which != DEFCHOICE) {
            continue;
          }
          e = type_destruct(mod, ch->subs[IDX_CH_VALUE], u);
          EXCEPT(e);
        }
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
  case IMPORT:
    if (node_is_at_top(node)) {
      e = morningtypepass(mod, node);
      EXCEPT(e);
    }
    return 0;
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

static error step_type_gather_excepts(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case TRY:
    module_excepts_open_try(mod);
    return 0;
  case EXCEP:
    module_excepts_push(mod, node);
    break;
  default:
    break;
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
  if (fun->typ->which != TYPE_DEF
      || fun->typ->definition->as.DEFTYPE.kind != DEFTYPE_SUM) {
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

static error type_inference_explicit_unary_call(struct module *mod, struct node *node, struct node *def) {
  if (def->which == DEFFUN && node->subs_count != 1) {
    error e = mk_except_call_args_count(mod, node, def, 0, node->subs_count - 1);
    EXCEPT(e);
  } else if (def->which == DEFMETHOD && node->subs_count != 2) {
    error e = mk_except_call_args_count(mod, node, def, 1, node->subs_count - 1);
    EXCEPT(e);
  }

  switch (def->which) {
  case DEFFUN:
  case DEFMETHOD:
    node->typ = def->typ->fun_args[def->typ->fun_arity];
    break;
  default:
    assert(FALSE);
  }

  return 0;
}

static const uint32_t tbi_for_ref[TOKEN__NUM] = {
  [TREFDOT] = TBI_REF,
  [TREFBANG] = TBI_MREF,
  [TREFSHARP] = TBI_MMREF,
  [TREFWILDCARD] = TBI_MMREF,
  [TDEREFDOT] = TBI_REF,
  [TDEREFBANG] = TBI_MREF,
  [TDEREFSHARP] = TBI_MMREF,
  [TDEREFWILDCARD] = TBI_MMREF,
  [TNULREFDOT] = TBI_NREF,
  [TNULREFBANG] = TBI_NMREF,
  [TNULREFSHARP] = TBI_NMMREF,
  [TNULREFWILDCARD] = TBI_NMMREF,
};

static const struct typ *typ_ref(struct module *mod, enum token_type op, const struct typ *typ) {
  struct node *gendef = typ_lookup_builtin(mod, tbi_for_ref[op])->definition;

  struct toplevel *toplevel = node_toplevel(gendef);
  for (size_t n = 1; n < toplevel->instances_count; ++n) {
    struct node *i = toplevel->instances[n];
    if (typ_equal(mod, i->typ->gen_args[1], typ)) {
      return i->typ;
    }
  }

  struct node *pristine = toplevel->instances[0];
  struct node *instance = add_instance_deepcopy_from_pristine(mod, gendef, pristine);
  node_toplevel(instance)->generic_definition = gendef;

  struct node *ga = instance->subs[IDX_GENARGS]->subs[0];
  ga->which = SETGENARG;
  // FIXME leaking ga->subs[1]
  ga->subs[1]->which = DIRECTDEF;
  ga->subs[1]->as.DIRECTDEF.definition = typ->definition;
  ga->typ = typ;
  ga->flags = NODE_IS_TYPE;

  error e = catchup_instantiation(node_module_owner(gendef),
                                  instance, gendef->scope->parent);
  assert(!e);

  return instance->typ;
}

static error type_inference_un(struct module *mod, struct node *node) {
  assert(node->which == UN);
  error e;

  const enum token_type op = node->as.UN.operator;
  switch (OP_KIND(op)) {
  case OP_UN_REFOF:
    node->typ = typ_ref(mod, op, node->subs[0]->typ);
    node->flags |= node->subs[0]->flags & NODE__TRANSITIVE;
    break;
  case OP_UN_DEREF:
    e = typ_check_can_deref(mod, node->subs[0], node->subs[0]->typ, node->as.UN.operator);
    EXCEPT(e);
    e = typ_check_deref_against_mark(mod, node, node->typ, node->as.UN.operator);
    EXCEPT(e);
    node->typ = node->subs[0]->typ->gen_args[1];
    node->flags |= node->subs[0]->flags & NODE__TRANSITIVE;
    break;
  case OP_UN_BOOL:
    if (typ_is_concrete(mod, node->subs[0]->typ)) {
      e = typ_compatible(mod, node, node->subs[0]->typ, typ_lookup_builtin(mod, TBI_BOOL));
      EXCEPT(e);
      node->typ = node->subs[0]->typ;
    } else {
      e = type_destruct(mod, node, typ_lookup_builtin(mod, TBI_BOOL));
      EXCEPT(e);
    }
    break;
  case OP_UN_NUM:
    e = typ_compatible_numeric(mod, node, node->subs[0]->typ);
    EXCEPT(e);
    node->typ = node->subs[0]->typ;
    break;
  default:
    assert(FALSE);
  }

  return 0;
}

static error type_inference_bin_sym(struct module *mod, struct node *node) {
  assert(node->which == BIN);

  error e;
  if (typ_is_concrete(mod, node->subs[0]->typ)
      && typ_is_concrete(mod, node->subs[1]->typ)) {
    e = typ_compatible(mod, node, node->subs[0]->typ, node->subs[1]->typ);
    EXCEPT(e);
  } else if (typ_is_concrete(mod, node->subs[0]->typ)) {
    e = type_destruct(mod, node->subs[1], node->subs[0]->typ);
    EXCEPT(e);
  } else if (typ_is_concrete(mod, node->subs[1]->typ)) {
    e = type_destruct(mod, node->subs[0], node->subs[1]->typ);
    EXCEPT(e);
  }

  e = typ_unify(&node->typ, mod, node,
                node->subs[0]->typ, node->subs[1]->typ);
  EXCEPT(e);

  switch (OP_KIND(node->as.BIN.operator)) {
  case OP_BIN_SYM_BOOL:
    e = typ_compatible(mod, node, node->typ, typ_lookup_builtin(mod, TBI_BOOL));
    EXCEPT(e);
    node->typ = typ_lookup_builtin(mod, TBI_BOOL);
    break;
  case OP_BIN_SYM_NUM:
    e = typ_compatible_numeric(mod, node, node->typ);
    EXCEPT(e);
    switch (node->as.BIN.operator) {
    case TLE:
    case TLT:
    case TGT:
    case TGE:
      node->typ = typ_lookup_builtin(mod, TBI_BOOL);
      break;
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
      node->typ = typ_lookup_builtin(mod, TBI_VOID);
      break;
    default:
      break;
    }
    break;
  case OP_BIN_SYM_PTR:
    e = typ_check_is_reference_instance(mod, node, node->subs[0]->typ);
    EXCEPT(e);
    e = typ_check_is_reference_instance(mod, node, node->subs[1]->typ);
    EXCEPT(e);
    node->typ = typ_lookup_builtin(mod, TBI_BOOL);
    break;
  case OP_BIN_SYM:
    switch (node->as.BIN.operator) {
    case TLE:
    case TLT:
    case TGT:
    case TGE:
    case TEQ:
    case TNE:
      node->typ = typ_lookup_builtin(mod, TBI_BOOL);
      break;
    default:
      break;
    }
  default:
    break;
  }

  if (OP_ASSIGN(node->as.BIN.operator)) {
    if ((node->subs[0]->flags & NODE_IS_TYPE)) {
      e = mk_except_type(mod, node->subs[0], "cannot assign to a type variable");
      EXCEPT(e);
    }
    if ((node->subs[1]->flags & NODE_IS_TYPE)) {
      e = mk_except_type(mod, node->subs[1], "cannot assign a type");
      EXCEPT(e);
    }

    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    node->subs[0]->flags |= (node->subs[1]->flags & NODE__TRANSITIVE);
  }
  node->flags |= (node->subs[0]->flags & NODE__TRANSITIVE);

  return 0;
}

static error type_inference_call(struct module *mod, struct node *node);

static error bin_accessor_maybe_ref(struct scope **parent_scope,
                                    struct module *mod, struct node *parent) {
  if (typ_is_reference_instance(mod, parent->typ)) {
    *parent_scope = parent->typ->gen_args[1]->definition->scope;
  }
  return 0;
}

static error bin_accessor_maybe_defchoice(struct scope **parent_scope, struct node *for_error,
                                          struct module *mod, struct node *parent) {
  if (parent->flags & NODE_IS_DEFCHOICE) {
    assert(parent->which == BIN);

    struct node *defchoice = NULL;
    error e = scope_lookup_ident_immediate(&defchoice, for_error, mod,
                                           parent->typ->definition->scope,
                                           node_ident(parent->subs[1]), FALSE);
    EXCEPT(e);
    assert(defchoice->which == DEFCHOICE);

    *parent_scope = defchoice->scope;
  }
  return 0;
}

static error rewrite_unary_call(struct module *mod, struct node *node, const struct typ *tfun) {
  struct scope *parent_scope = node->scope->parent;

  struct node *fun = calloc(1, sizeof(struct node));
  memcpy(fun, node, sizeof(*fun));
  fun->typ = tfun;
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

  const struct typ *mark = node->typ;

  struct node *parent = node->subs[0];
  if (typ_equal(mod, parent->typ, typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT))) {
    node->typ = NULL;
    parent->typ = NULL;

    // Force a new pass of the first BODY pass, clearing the pending
    // destruct marker first to type this node in isolation.
    PUSH_STATE(mod->state);
    const struct pass *pa = &passes[mod->stage->state->passing];
    int module_depth = 0;
    e = pass(mod, node, pa->downs, pa->ups, -1, &module_depth);
    EXCEPT(e);
    POP_STATE(mod->state);

    return 0;
  }

  struct scope *parent_scope = parent->typ->definition->scope;
  e = bin_accessor_maybe_ref(&parent_scope, mod, parent);
  EXCEPT(e);
  e = bin_accessor_maybe_defchoice(&parent_scope, node, mod, parent);
  EXCEPT(e);

  struct node *field = NULL;
  e = scope_lookup_ident_immediate(&field, node->subs[1], mod, parent_scope,
                                   node_ident(node->subs[1]), FALSE);
  EXCEPT(e);

  if (field->typ->which == TYPE_FUNCTION
      && node->typ != typ_lookup_builtin(mod, TBI__CALL_FUNCTION_SLOT)) {
    if (node_fun_explicit_args_count(field) != 0) {
      e = mk_except_call_args_count(mod, node, field, 0, 0);
      EXCEPT(e);
    }

    e = rewrite_unary_call(mod, node, field->typ);
    EXCEPT(e);
  } else {
    node->typ = field->typ;
    assert(field->which != BIN || field->flags != 0);
    node->flags = field->flags;
  }

  if (!(node->flags & NODE_IS_TYPE)) {
    e = typ_check_deref_against_mark(mod, node, mark, node->as.BIN.operator);
    EXCEPT(e);
  }

  return 0;
}

static error type_inference_bin_rhs_unsigned(struct module *mod, struct node *node) {
  error e;
  e = typ_compatible_numeric(mod, node->subs[0], node->subs[0]->typ);
  EXCEPT(e);
  e = typ_check_isa(mod, node->subs[1], node->subs[1]->typ,
                    typ_lookup_builtin(mod, TBI_UNSIGNED_INTEGER));
  EXCEPT(e);
  if (node->subs[1]->typ->definition->which == DEFINTF) {
    node->subs[1]->typ = typ_lookup_builtin(mod, TBI_UNSIGNED_INTEGER);
  } else {
    // noop
  }
  node->typ = node->subs[0]->typ;
  return 0;
}

static error type_inference_bin_rhs_type(struct module *mod, struct node *node) {
  error e;
  if (!(node->subs[1]->flags & NODE_IS_TYPE)) {
    e = mk_except_type(mod, node->subs[1], "right-hand side not a type");
    EXCEPT(e);
  }
  e = type_destruct(mod, node->subs[0], node->subs[1]->typ);
  EXCEPT(e);
  node->typ = node->subs[1]->typ;
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

static error type_inference_tuple(struct module *mod, struct node *node) {
  node->typ = typ_new(typ_lookup_builtin(mod, TBI_PSEUDO_TUPLE)->definition,
                      TYPE_TUPLE, node->subs_count, 0);
  for (size_t n = 0; n < node->typ->gen_arity; ++n) {
    node->typ->gen_args[1+n] = node->subs[n]->typ;

    if (n > 0 && (node->flags & NODE_IS_TYPE) != (node->subs[n]->flags & NODE_IS_TYPE)) {
      error e = mk_except_type(mod, node->subs[n], "tuple combines values and types");
      EXCEPT(e);
    }
    node->flags |= (node->subs[n]->flags & NODE__TRANSITIVE);
  }
  error e = need_instance(mod, node, node->typ);
  EXCEPT(e);
  return 0;
}

static error type_destruct_init_named(struct module *mod, struct node *node,
                                      const struct typ *constraint) {
  struct node *def = constraint->definition;

  for (size_t n = 0; n < node->subs_count; n += 2) {
    struct node *field_name = node->subs[n];
    struct node *field = NULL;
    error e = scope_lookup_ident_immediate(&field, field_name, mod, def->scope,
                                           node_ident(field_name), FALSE);
    EXCEPT(e);
    e = type_destruct(mod, node->subs[n+1], field->typ);
    EXCEPT(e);
  }

  node->typ = constraint;
  return 0;
}

static error find_array_ctor_isalisteach(struct module *mod,
                                         const struct typ *t,
                                         const struct typ *intf, void *user) {
  const struct typ **result = user;

  if (intf->gen_arity > 0
      && typ_equal(mod, intf->gen_args[0],
                   typ_lookup_builtin(mod, TBI_ARRAY_CTOR))) {
    *result = intf;
  }

  return 0;
}

static error type_destruct_init_array(struct module *mod, struct node *node,
                                      const struct typ *constraint) {
  const struct typ *array_ctor_intf = NULL;
  error e = typ_isalist_foreach(mod, constraint, 0,
                                find_array_ctor_isalisteach, &array_ctor_intf);
  EXCEPT(e);

  if (array_ctor_intf == NULL) {
    e = mk_except_type(mod, node,
                       "type '%s' not isa i_array_ctor, cannot use array initializer",
                       typ_pretty_name(mod, constraint));
    EXCEPT(e);
  }

  const struct typ *t_el = array_ctor_intf->gen_args[1];
  for (size_t n = 0; n < node->subs_count; n += 1) {
    e = type_destruct(mod, node->subs[n], t_el);
    EXCEPT(e);
  }

  node->typ = constraint;
  return 0;
}

static error type_destruct_init(struct module *mod, struct node *node,
                                const struct typ *constraint) {
  assert(node->which == INIT);
  if (node->as.INIT.is_array) {
    return type_destruct_init_array(mod, node, constraint);
  } else {
    return type_destruct_init_named(mod, node, constraint);
  }
}

static enum token_type ref_op_for_acc_op[] = {
  [TDOT] = TREFDOT,
  [TBANG] = TREFBANG,
  [TSHARP] = TREFSHARP,
  [TWILDCARD] = TREFWILDCARD,
};

static enum token_type acc_op_for_ref_op[] = {
  [TREFDOT] = TDOT,
  [TREFBANG] = TBANG,
  [TREFSHARP] = TSHARP,
  [TREFWILDCARD] = TWILDCARD,
};

static struct node *generated_expr_ref(enum token_type ref_op, struct node *node) {
  if (node->which == BIN && OP_KIND(node->as.BIN.operator) == OP_BIN_ACC) {
    // Of the form
    //   self.x.y!method args
    // which was transformed to
    //   type.method @!self.x.y args
    // We actually need
    //   type.method @!self.x!y args
    // This is assuming that typing has checked the transformation below is
    // legal.
    node->as.BIN.operator = acc_op_for_ref_op[ref_op];
  }

  struct node *n = calloc(1, sizeof(struct node));
  n->which = UN;
  n->as.UN.operator = ref_op;
  n->subs_count = 1;
  n->subs = calloc(n->subs_count, sizeof(struct node *));
  n->subs[0] = node;
  return n;
}

static struct node *self_ref_if_value(struct module *mod,
                                      enum token_type access, struct node *node) {
  if (typ_is_reference_instance(mod, node->typ)) {
    return node;
  } else {
    return generated_expr_ref(access, node);
  }
}

static error prepare_call_arguments(struct module *mod, struct node *node) {
  struct node *fun = node->subs[0];

  if (node->subs_count > 1 && (node->subs[1]->flags & NODE_IS_TYPE)) {
    // Explicit generic function instantiation.
    return 0;
  }

  switch (fun->typ->definition->which) {
  case DEFFUN:
    if (node_fun_explicit_args_count(fun->typ->definition) != node->subs_count - 1) {
      error e = mk_except_call_args_count(mod, node, fun->typ->definition, 0,
                                          node->subs_count - 1);
      EXCEPT(e);
    }
    break;
  case DEFMETHOD:
    if (fun->which == BIN) {
      if ((fun->subs[0]->flags & NODE_IS_TYPE)) {
        // Form (type.method self ...).
        if (1 + node_fun_explicit_args_count(fun->typ->definition) != node->subs_count - 1) {
          error e = mk_except_call_args_count(mod, node, fun->typ->definition, 1,
                                              node->subs_count - 1);
          EXCEPT(e);
        }
      } else {
        // Form (self.method ...); rewrite as (type.method self ...).
        if (node_fun_explicit_args_count(fun->typ->definition) != node->subs_count - 1) {
          error e = mk_except_call_args_count(mod, node, fun->typ->definition, 0,
                                              node->subs_count - 1);
          EXCEPT(e);
        }

        struct node *m = mk_node(mod, node, DIRECTDEF);
        m->as.DIRECTDEF.definition = fun->typ->definition;
        rew_move_last_over(node, 0, TRUE);

        assert(fun->which == BIN);
        struct node *self = self_ref_if_value(
          mod, ref_op_for_acc_op[fun->as.BIN.operator], fun->subs[0]);
        rew_append(node, self);
        rew_insert_last_at(node, 1);

        const struct node *except[] = { fun->subs[0], NULL };
        error e = catchup(mod, except, self, node->scope, CATCHUP_BELOW_CURRENT);
        EXCEPT(e);

        e = catchup(mod, NULL, m, node->scope, CATCHUP_BELOW_CURRENT);
        EXCEPT(e);
      }
    } else if ((fun->flags & NODE_IS_TYPE) && fun->which == CALL) {
      // Generic method instantiation: (type.method u32 i32) self
      if (1 + node_fun_explicit_args_count(fun->typ->definition) != node->subs_count - 1) {
        error e = mk_except_call_args_count(mod, node, fun->typ->definition, 1,
                                            node->subs_count - 1);
        EXCEPT(e);
      }
    }
    break;
  default:
    assert(FALSE);
  }

  return 0;
}

static bool is_instance_for_explicit(struct module *mod, struct node *i, struct node *node) {
  for (size_t n = 1; n < node->subs_count; ++n) {
    if (!typ_equal(mod, i->typ->gen_args[n], node->subs[n]->typ)) {
      return FALSE;
    }
  }

  return TRUE;
}

static bool is_instance_for_implicit(struct module *mod, struct node *i, struct node *node) {
  for (size_t n = 0; n < node->subs_count; ++n) {
    if (!typ_equal(mod, i->typ->gen_args[1+n], node->subs[n]->typ)) {
      return FALSE;
    }
  }

  return TRUE;
}

static error genarg_destruct(struct module *mod, const struct node *gendef,
                             struct node *genargs, struct node *ga, bool destructing_genarg,
                             struct node *for_error, const struct typ *constraint) {
  error e;
  struct node *def = NULL;

  size_t offset;
  switch (ga->which) {
  case DEFARG:
    e = genarg_destruct(mod, gendef, genargs, ga->subs[1], FALSE, for_error, constraint);
    EXCEPT(e);
    break;
  case DEFGENARG:
    if (ga->typ != NULL) {
      e = typ_unify(&ga->typ, mod, for_error, ga->typ, constraint);
      EXCEPT(e);
    } else {
      ga->typ = constraint;
      ga->flags = NODE_IS_TYPE;
      ga->subs[0]->typ = ga->typ;
      ga->subs[0]->flags = ga->flags;
    }
    e = genarg_destruct(mod, gendef, genargs, ga->subs[1], TRUE, for_error, constraint);
    EXCEPT(e);
    break;
  case UN:
  case CALL:
    offset = ga->which == CALL ? 0 : 1;
    if (destructing_genarg) {
      const struct typ *concrete = NULL;
      e = typ_find_matching_concrete_isa(&concrete, mod, ga, ga->typ, constraint);
      EXCEPT(e);
      ga->typ = concrete;
      ga->flags = NODE_IS_TYPE;
      assert(concrete->gen_arity == ga->subs_count + offset - 1);
      for (size_t n = 0; n < ga->subs_count; ++n) {
        e = genarg_destruct(mod, gendef, genargs, ga->subs[n], TRUE,
                            for_error, concrete->gen_args[n + offset]);
        EXCEPT(e);
      }
    } else {
      e = typ_check_isa(mod, ga, constraint, ga->typ);
      EXCEPT(e);
      assert(constraint->gen_arity == ga->subs_count + offset - 1);
      ga->typ = constraint;
      ga->flags = NODE_IS_TYPE;
      for (size_t n = 0; n < ga->subs_count; ++n) {
        e = genarg_destruct(mod, gendef, genargs, ga->subs[n], FALSE,
                            for_error, constraint->gen_args[n + offset]);
        EXCEPT(e);
      }
    }
    break;
  case IDENT:
    e = scope_lookup(&def, mod, gendef->scope, ga);
    EXCEPT(e);
    if (def->which == DEFGENARG) {
      // This is the DEFGENARG from the generic definition itself.
      // Look up again to get it from the instantiating genargs this time.
      e = scope_lookup_ident_immediate(&def, ga, mod, genargs->scope,
                                       node_ident(ga), FALSE);
      EXCEPT(e);
      assert(def->which == DEFGENARG);
      e = genarg_destruct(mod, gendef, genargs, def, TRUE, for_error, constraint);
      EXCEPT(e);
    } else {
      if (def->typ->definition->which == DEFINTF) {
        e = typ_check_isa(mod, for_error, constraint, def->typ);
        EXCEPT(e);
      } else {
        e = typ_compatible(mod, for_error, constraint, def->typ);
        EXCEPT(e);
      }
    }
    break;
  default:
    assert(FALSE);
    break;
  }

  return 0;
}

static error rewrite_deftype_instance_genargs(struct module *mod, const struct node *for_error,
                                              struct node *instance, struct node *expr) {
  struct node *genargs = instance->subs[IDX_GENARGS];
  size_t offset;
  switch (expr->which) {
  case GENARGS:
    offset = 0;
    break;
  case CALL:
    offset = 1;
    break;
  default:
    assert(0);
    break;
  }

  const struct typ *gendeft = node_toplevel(instance)->generic_definition->typ;
  const bool gendef_is_ref = typ_isa(mod, gendeft, typ_lookup_builtin(mod, TBI_ANY_ANY_REF));;

  for (size_t n = 0; n < genargs->subs_count; ++n) {
    struct node *ex = expr->subs[offset + n];
    if (!(ex->flags & NODE_IS_TYPE)) {
      char *ngendeft = typ_pretty_name(mod, gendeft);
      error e = mk_except_type(mod, for_error,
                               "generic argument %d of generic '%s' is not a type expression",
                               n + 1, ngendeft);
      free(ngendeft);
      EXCEPT(e);
    }

    if (!gendef_is_ref && !typ_is_concrete(mod, ex->typ)) {
      char *ngendeft = typ_pretty_name(mod, gendeft);
      char *nex = typ_pretty_name(mod, ex->typ);
      error e = mk_except_type(mod, for_error,
                               "generic argument %d '%s' of generic '%s' is not concrete (it is a literal)",
                               n + 1, nex, ngendeft);
      free(nex);
      free(ngendeft);
      EXCEPT(e);
    }

    struct node *ga = genargs->subs[n];
    ga->which = SETGENARG;
    // FIXME leaking ga->subs[1]
    ga->subs[1]->which = DIRECTDEF;
    assert(ex->typ->definition != NULL);
    ga->subs[1]->as.DIRECTDEF.definition = ex->typ->definition;

    // For the benefit of step_type_deftypes_defintfs
    ga->typ = ex->typ;
    ga->flags = NODE_IS_TYPE;
  }

  return 0;
}

static void found_existing_instance_explicit(struct module *mod, struct node *node,
                                             struct node *fun, struct node *i) {
  if (fun->typ->is_abstract_genarg) {
    node->typ = typ_genarg_mark_as_abstract(i->typ);
  } else {
    node->typ = i->typ;
  }
  node->flags = NODE_IS_TYPE;
}

static error found_existing_instance_implicit_deffun(struct module *mod, struct node *node,
                                                     struct node *fun, struct node *i) {
  if (fun->typ->is_abstract_genarg) {
    fun->typ = typ_genarg_mark_as_abstract(i->typ);
  } else {
    fun->typ = i->typ;
  }
  for (size_t n = 1; n < node->subs_count; ++n) {
    error e = type_destruct(mod, node->subs[n], fun->typ->fun_args[n-1]);
    EXCEPT(e);
  }
  node->typ = i->typ->fun_args[i->typ->fun_arity];
  return 0;
}

static error type_inference_generic_instantiation(struct module *mod, struct node *node) {
  struct node *fun = node->subs[0];
  struct node *gendef = fun->typ->definition;
  struct node *genargs = gendef->subs[IDX_GENARGS];
  struct toplevel *toplevel = node_toplevel(gendef);

  struct node *expr = NULL;
  bool is_explicit;

  assert(node->subs_count != 0);
  if ((node->subs[1]->flags & NODE_IS_TYPE)) {
    is_explicit = TRUE;
    if (genargs->subs_count != node->subs_count - 1) {
      error e = mk_except_type(mod, node, "wrong number of generic type arguments\n");
      EXCEPT(e);
    }

    for (size_t n = 0; n < genargs->subs_count; ++n) {
      struct node *arg = node->subs[1+n];
      error e = typ_check_isa(mod, arg, arg->typ, genargs->subs[n]->typ);
      EXCEPT(e);
    }

    for (size_t n = 1; n < toplevel->instances_count; ++n) {
      struct node *i = toplevel->instances[n];
      if (is_instance_for_explicit(mod, i, node)) {
        found_existing_instance_explicit(mod, node, fun, i);
        return 0;
      }
    }

    expr = node;

  } else {
    is_explicit = FALSE;
    assert(gendef->typ->which == TYPE_FUNCTION);

    struct node *instantiation = calloc(1, sizeof(struct node));
    instantiation->which = GENARGS;
    for (size_t n = 0; n < genargs->subs_count; ++n) {
      struct node *ga = node_new_subnode(mod, instantiation);
      node_deepcopy(mod, ga, genargs->subs[n]);
    }

    PUSH_STATE(mod->stage->state);
    PUSH_STATE(mod->state);
    const struct pass *pa = &passes[0];
    int module_depth = 0;
    error e = pass(mod, instantiation, pa->downs, pa->ups, -1, &module_depth);
    EXCEPT(e);
    POP_STATE(mod->state);
    POP_STATE(mod->stage->state);

    for (size_t n = 0; n < instantiation->subs_count; ++n) {
      struct node *ga = instantiation->subs[n];
      e = scope_define_ident(mod, instantiation->scope, node_ident(ga->subs[0]), ga);
      EXCEPT(e);
    }

    for (size_t n = 0; n < node->subs_count-1; ++n) {
      error e = genarg_destruct(mod, gendef, instantiation,
                                gendef->subs[IDX_FUN_FIRSTARG+n], FALSE,
                                node->subs[1+n], node->subs[1+n]->typ);
      EXCEPT(e);
    }

    for (size_t n = 1; n < toplevel->instances_count; ++n) {
      struct node *i = toplevel->instances[n];
      if (is_instance_for_implicit(mod, i, instantiation)) {
        error e = found_existing_instance_implicit_deffun(mod, node, fun, i);
        EXCEPT(e);
        return 0;
      }
    }

    expr = instantiation;
  }

  struct node *pristine = toplevel->instances[0];
  struct node *instance = add_instance_deepcopy_from_pristine(mod, gendef, pristine);
  node_toplevel(instance)->generic_definition = gendef;

  error e = rewrite_deftype_instance_genargs(mod, node, instance, expr);
  EXCEPT(e);


  e = catchup_instantiation(node_module_owner(gendef),
                            instance, gendef->scope->parent);
  EXCEPT(e);

  if (is_explicit) {
    if (fun->typ->is_abstract_genarg) {
      node->typ = typ_genarg_mark_as_abstract(instance->typ);
    } else {
      node->typ = instance->typ;
    }
    node->flags = NODE_IS_TYPE;
  } else {
    assert(gendef->typ->which == TYPE_FUNCTION && "only for functions, for now");
    fun->typ = instance->typ;
    for (size_t n = 1; n < node->subs_count; ++n) {
      e = type_destruct(mod, node->subs[n], fun->typ->fun_args[n-1]);
      EXCEPT(e);
    }
    node->typ = instance->typ->fun_args[instance->typ->fun_arity];
    free(expr);
  }

  assert(node->typ != NULL);

  return 0;
}

static error type_inference_call(struct module *mod, struct node *node) {
  struct node *fun = node->subs[0];

  if (fun->typ->which == TYPE_DEF) {
    if (!node_can_have_genargs(fun->typ->definition)
        || fun->typ->definition->subs[IDX_GENARGS]->subs_count == 0) {
      error e = mk_except_type(mod, fun, "not a generic type");
      EXCEPT(e);
    }

    error e = type_inference_generic_instantiation(mod, node);
    EXCEPT(e);
    return 0;
  }

  if (fun->typ->which != TYPE_FUNCTION) {
    error e = mk_except_type(mod, fun, "not a function or sum type constructor");
    EXCEPT(e);
  }

  error e = prepare_call_arguments(mod, node);
  EXCEPT(e);

  if (fun->typ->which == TYPE_FUNCTION
      && fun->typ->definition->subs[IDX_GENARGS]->subs_count > 0
      && node_toplevel_const(fun->typ->definition)->generic_definition == NULL) {
    error e = type_inference_generic_instantiation(mod, node);
    EXCEPT(e);

    return 0;
  }

  if (node_fun_explicit_args_count(fun->typ->definition) == 0) {
    return type_inference_explicit_unary_call(mod, node, fun->typ->definition);
  }

  if (node->subs_count >= 2) {
    node->flags = (node->subs[1]->flags & NODE__TRANSITIVE);
    for (size_t n = 2; n < node->subs_count; ++n) {
      if (n > 0 && (node->flags & NODE_IS_TYPE) != (node->subs[n]->flags & NODE_IS_TYPE)) {
        e = mk_except_type(mod, node->subs[n], "call combines value and type arguments");
        EXCEPT(e);
      }
      node->flags |= (node->subs[n]->flags & NODE__TRANSITIVE);
    }
  }
  assert(!(node->flags & NODE_IS_TYPE));

  for (size_t n = 1; n < node->subs_count; ++n) {
    e = type_destruct(mod, node->subs[n], fun->typ->fun_args[n-1]);
    EXCEPT(e);
  }
  node->typ = fun->typ->fun_args[fun->typ->fun_arity];

  return 0;
}

static error type_inference_block(struct module *mod, struct node *node) {
  error e;

  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *s = node->subs[n];
    if ((s->flags & NODE_IS_TYPE)) {
      e = mk_except_type(mod, s, "block statements cannot be type names");
      EXCEPT(e);
    }
  }
  if (node->subs_count > 0) {
    for (size_t n = 0; n < node->subs_count - 1; ++n) {
      struct node *s = node->subs[n];
      if (!typ_equal(mod, s->typ, typ_lookup_builtin(mod, TBI_VOID))) {
        e = mk_except_type(mod, s, "intermediate statements in a block must be of type void (except the last one), not '%s'",
                           typ_pretty_name(mod, s->typ));
        EXCEPT(e);
      }
    }
    if (node->subs[node->subs_count - 1]->which == RETURN) {
      node->typ = typ_lookup_builtin(mod, TBI_VOID);
    } else {
      node->typ = node->subs[node->subs_count - 1]->typ;
    }
  } else {
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
  }

  return 0;
}

static error type_inference_if(struct module *mod, struct node *node) {
  error e;

  for (size_t n = 0; n < node->subs_count-1; n += 2) {
    e = typ_compatible(mod, node->subs[n], node->subs[n]->typ,
                       typ_lookup_builtin(mod, TBI_BOOL));
    EXCEPT(e);
  }

  const struct typ *u = node->subs[1]->typ;
  for (size_t n = 3; n < node->subs_count; n += 2) {
    struct node *elif = node->subs[n];
    e = typ_unify(&u, mod, elif, u, elif->typ);
    EXCEPT(e);
  }

  if (node->subs_count % 2 == 1) {
    struct node *els = node->subs[node->subs_count-1];
    e = typ_unify(&u, mod, els, u, els->typ);
    EXCEPT(e);
  } else {
    if (!typ_equal(mod, u, typ_lookup_builtin(mod, TBI_VOID))) {
      e = mk_except_type(mod, node,
                         "if statement is not of type void but is missing an else branch");
      EXCEPT(e);
    }
  }

  node->typ = u;

  return 0;
}

static error type_destruct_match_pattern(struct module *mod, struct node *match, size_t n) {
  assert(n % 2 == 1);

  struct node *expr = match->subs[0];
  struct node *p = match->subs[n];

  assert(expr->typ->definition->which == DEFTYPE);
  const bool enum_or_sum = expr->typ->definition->as.DEFTYPE.kind == DEFTYPE_ENUM
    || expr->typ->definition->as.DEFTYPE.kind == DEFTYPE_SUM;

  error e;
  if (!enum_or_sum) {
    e = mk_except_type(mod, expr, "must match over an enum or sum type");
    EXCEPT(e);
  }

  if (node_ident(p) == ID_OTHERWISE) {
    p->typ = expr->typ;
    return 0;
  }

  if (expr->typ->definition->which == DEFTYPE
      && enum_or_sum
      && p->which == IDENT) {
    struct node *field = NULL;
    e = scope_lookup_ident_immediate(&field, p, mod,
                                     expr->typ->definition->scope,
                                     node_ident(p),
                                     TRUE);
    if (!e) {
      e = typ_check_equal(mod, p, expr->typ, field->typ);
      EXCEPT(e);
      p->typ = field->typ;
      p->flags = field->flags;
      return 0;
    }
  }

  e = type_destruct(mod, p, expr->typ);
  EXCEPT(e);
  return 0;
}

static error type_inference_match(struct module *mod, struct node *node) {
  error e;

  for (size_t n = 1; n < node->subs_count; n += 2) {
    e = type_destruct_match_pattern(mod, node, n);
    EXCEPT(e);
  }

  for (size_t n = 4; n < node->subs_count; n += 2) {
    e = typ_compatible(mod, node->subs[n], node->subs[n]->typ,
                       node->subs[2]->typ);
    EXCEPT(e);
  }

  node->typ = node->subs[1]->typ;

  return 0;
}

static error type_inference_try(struct module *mod, struct node *node) {
  node->typ = NULL;

  error e;
  struct try_state *st = module_excepts_get(mod);

  if (st->count == 0) {
    e = mk_except(mod, node, "try block has no except statement, catch is unreachable");
    EXCEPT(e);
  }

  const struct typ *u = st->excepts[0]->typ;
  for (size_t n = 1; n < st->count; ++n) {
    struct node *exc = st->excepts[n];
    e = typ_unify(&u, mod, exc, u, exc->typ);
    EXCEPT(e);
  }

  e = type_destruct(mod, node->subs[1], u);
  EXCEPT(e);

  e = typ_compatible(mod, node->subs[1], node->subs[1]->typ,
                     node->subs[0]->typ);
  EXCEPT(e);
  node->typ = node->subs[0]->typ;

  return 0;
}

static error type_destruct_import_path(struct module *mod, struct node *node) {
  struct node *target_mod = NULL;
  struct node *def = NULL;
  error e = 0;

  if (node->which == BIN) {
    struct node *parent = node_parent(node);
    struct node *pparent = node_parent(parent);

    if (pparent->which == IMPORT) {
      e = scope_lookup_module(&target_mod, mod, node->subs[0], FALSE);
      EXCEPT(e);
      e = scope_lookup_ident_immediate(&def, node->subs[1], mod,
                                       target_mod->scope, node_ident(node->subs[1]), FALSE);
      EXCEPT(e);
      if (def->typ == NULL) {
        // Not yet "passed" in the other module, skip.
        return 0;
      }
    }
  }

  if (def == NULL) {
    e = scope_lookup_module(&def, mod, node, FALSE);
    EXCEPT(e);
  }

  node->typ = def->typ;
  node->flags = def->flags;

  if (node->which == BIN) {
    assert(node->as.BIN.operator == TDOT);
    e = type_destruct_import_path(mod, node->subs[0]);
    EXCEPT(e);
  }

  assert(node->typ != NULL);

  return 0;
}

static struct typ* number_literal_typ(struct module *mod, struct node *node) {
  assert(node->which == NUMBER);
  if (strchr(node->as.NUMBER.value, '.') != NULL) {
    return typ_lookup_builtin(mod, TBI_LITERALS_FLOATING);
  } else {
    return typ_lookup_builtin(mod, TBI_LITERALS_INTEGER);
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

static error type_destruct(struct module *mod, struct node *node, const struct typ *constraint) {
  error e;
  struct node *def = NULL;

  assert(node->typ != typ_lookup_builtin(mod, TBI__NOT_TYPEABLE));

  if (node->typ != NULL
      && node->typ != typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT)
      && node->typ != typ_lookup_builtin(mod, TBI__MUTABLE)
      && node->typ != typ_lookup_builtin(mod, TBI__MERCURIAL)
      && typ_is_concrete(mod, node->typ)) {
    e = typ_unify(&node->typ, mod, node, node->typ, constraint);
    EXCEPT(e);
    return 0;
  }

  switch (node->which) {
  case NUL:
    e = typ_unify(&node->typ, mod, node, typ_lookup_builtin(mod, TBI_LITERALS_NULL), constraint);
    EXCEPT(e);
    break;
  case NUMBER:
    e = typ_unify(&node->typ, mod, node, number_literal_typ(mod, node), constraint);
    EXCEPT(e);
    break;
  case BOOL:
    e = typ_unify(&node->typ, mod, node, typ_lookup_builtin(mod, TBI_BOOL), constraint);
    EXCEPT(e);
    break;
  case STRING:
    if (typ_equal(mod, constraint, typ_lookup_builtin(mod, TBI_CHAR))) {
      if (!string_literal_has_length_one(node->as.STRING.value)) {
        e = mk_except_type(mod, node,
                           "string literal '%s' does not have length 1, cannot coerce to char",
                           node->as.STRING.value);
        EXCEPT(e);
      }
    }

    e = typ_unify(&node->typ, mod, node, typ_lookup_builtin(mod, TBI_STATIC_STRING), constraint);
    EXCEPT(e);
    break;
  case SIZEOF:
    e = typ_unify(&node->typ, mod, node, typ_lookup_builtin(mod, TBI_SIZE), constraint);
    EXCEPT(e);
    break;
  case IDENT:
    // FIXME make sure ident not used before definition.
    // FIXME let x, (y, z) = i32, (i32, i32)
    // In this case, y (for instance), will not have NODE_IS_TYPE set properly.
    // NODE_IS_TYPE needs to be set recursively when descending via
    // type_destruct.
    e = scope_lookup_ident_wontimport(&def, node, mod, node->scope,
                                      node_ident(node), FALSE);
    EXCEPT(e);
    if (def->which == DEFNAME) {
      e = type_destruct(mod, def, constraint);
      EXCEPT(e);
    }
    node->typ = def->typ;
    node->flags |= (def->flags & NODE__TRANSITIVE);
    break;
  case DEFNAME:
    if (node->typ == typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT)) {
      node->typ = constraint;
    } else {
      e = typ_unify(&node->typ, mod, node, node->typ, constraint);
      EXCEPT(e);
      node->as.DEFNAME.pattern->typ = node->typ;
      EXCEPT(e);
    }
    if (node->as.DEFNAME.expr != NULL) {
      e = type_destruct(mod, node->as.DEFNAME.expr, node->typ);
      EXCEPT(e);
    }
    break;
  case UN:
    switch (OP_KIND(node->as.UN.operator)) {
    case OP_UN_BOOL:
      e = typ_compatible(mod, node, constraint, typ_lookup_builtin(mod, TBI_BOOL));
      break;
    case OP_UN_NUM:
      e = typ_compatible_numeric(mod, node, constraint);
      break;
    case OP_UN_REFOF:
      e = 0;
      break;
    case OP_UN_DEREF:
      e = typ_check_reference_compatible(mod, node, node->as.UN.operator, constraint);
      break;
    default:
      assert(FALSE);
    }
    EXCEPT(e);

    struct node *sub_node = node->subs[0];
    const struct typ *sub_constraint = NULL;

    switch (OP_KIND(node->as.UN.operator)) {
    case OP_UN_REFOF:
      sub_constraint = constraint->gen_args[1];
      break;
    case OP_UN_DEREF:
      sub_constraint = typ_ref(mod, node->as.UN.operator, constraint);
      break;
    default:
      sub_constraint = constraint;
      node->flags = (node->subs[0]->flags & NODE__TRANSITIVE);
      break;
    }

    e = type_destruct(mod, sub_node, sub_constraint);
    EXCEPT(e);

    switch (OP_KIND(node->as.UN.operator)) {
    case OP_UN_REFOF:
      node->typ = typ_new(constraint->definition, TYPE_DEF, 1, 0);
      node->typ->gen_args[1] = node->subs[0]->typ;
      break;
    default:
      node->typ = node->subs[0]->typ;
      break;
    }
    break;
  case BIN:
    if (OP_KIND(node->as.BIN.operator) == OP_BIN_ACC) {
      e = type_inference_bin_accessor(mod, node);
      EXCEPT(e);
      e = typ_unify(&node->typ, mod, node, node->typ, constraint);
      EXCEPT(e);
      break;
    }

    const struct typ *left_constraint = constraint;
    const struct typ *right_constraint = constraint;

    switch (OP_KIND(node->as.BIN.operator)) {
    case OP_BIN_SYM_BOOL:
      e = typ_compatible(mod, node, constraint, typ_lookup_builtin(mod, TBI_BOOL));
      EXCEPT(e);
      break;
    case OP_BIN_SYM_NUM:
      e = typ_compatible_numeric(mod, node, constraint);
      EXCEPT(e);
      break;
    case OP_BIN_NUM_RHS_UNSIGNED:
      e = typ_compatible_numeric(mod, node, left_constraint);
      EXCEPT(e);
      e = type_inference_bin_rhs_unsigned(mod, node);
      EXCEPT(e);
      right_constraint = NULL;
      break;
    case OP_BIN_RHS_TYPE:
      if (!(node->subs[1]->flags & NODE_IS_TYPE)) {
        e = mk_except_type(mod, node->subs[1], "right-hand side of type constraint is not a type");
        EXCEPT(e);
      }
      if (node->as.BIN.operator == TCOLON) {
        right_constraint = NULL;
      }
      break;
    default:
      break;
    }

    e = type_destruct(mod, node->subs[0], left_constraint);
    EXCEPT(e);
    if (right_constraint != NULL) {
      e = type_destruct(mod, node->subs[1], right_constraint);
      EXCEPT(e);
    }

    switch (OP_KIND(node->as.BIN.operator)) {
    case OP_BIN_SYM:
    case OP_BIN_SYM_BOOL:
    case OP_BIN_SYM_NUM:
      e = typ_unify(&node->typ, mod, node, node->subs[0]->typ, node->subs[1]->typ);
      EXCEPT(e);
      break;
    case OP_BIN_NUM_RHS_UNSIGNED:
      node->typ = node->subs[0]->typ;
      break;
    case OP_BIN_RHS_TYPE:
      if (node->as.BIN.operator == Tisa) {
        node->typ = typ_lookup_builtin(mod, TBI_BOOL);
      } else if (node->as.BIN.operator == TCOLON) {
        node->typ = node->subs[0]->typ;
      } else {
        assert(FALSE);
      }
      break;
    default:
      assert(FALSE);
    }

    if (OP_ASSIGN(node->as.BIN.operator)) {
      if ((node->subs[0]->flags & NODE_IS_TYPE)) {
        e = mk_except_type(mod, node->subs[0], "cannot assign to a type variable");
        EXCEPT(e);
      }
      if ((node->subs[1]->flags & NODE_IS_TYPE)) {
        e = mk_except_type(mod, node->subs[1], "cannot assign a type");
        EXCEPT(e);
      }

      node->subs[0]->flags |= node->subs[1]->flags & NODE__TRANSITIVE;
    }
    node->flags |= node->subs[0]->flags;

    break;
  case TYPECONSTRAINT:
    e = type_destruct(mod, node->subs[1], constraint);
    EXCEPT(e);
    e = type_destruct(mod, node->subs[0], node->subs[1]->typ);
    EXCEPT(e);
    node->typ = constraint;
    break;
  case TUPLE:
    e = type_inference_tuple(mod, node);
    EXCEPT(e);
    e = typ_unify(&node->typ, mod, node, node->typ, constraint);
    EXCEPT(e);
    for (size_t n = 0; n < node->subs_count; ++n) {
      e = type_destruct(mod, node->subs[n], node->typ->gen_args[1+n]);
      EXCEPT(e);
    }
    break;
  case INIT:
    e = type_destruct_init(mod, node, constraint);
    EXCEPT(e);
    break;
  case CALL:
    e = type_inference_call(mod, node);
    EXCEPT(e);
    e = typ_unify(&node->typ, mod, node, node->typ, constraint);
    EXCEPT(e);
    break;
  case BLOCK:
    e = type_destruct(mod, node->subs[node->subs_count-1], constraint);
    EXCEPT(e);
    e = type_inference_block(mod, node);
    EXCEPT(e);
    break;
  case IF:
    for (size_t n = 1; n < node->subs_count-1; n += 2) {
      e = typ_compatible(mod, node->subs[n], node->subs[n]->typ,
                         constraint);
      EXCEPT(e);
    }
    if (node->subs_count % 2 == 1) {
      struct node *els = node->subs[node->subs_count-1];
      e = typ_compatible(mod, els, els->typ,
                         constraint);
      EXCEPT(e);
    }
    e = type_inference_if(mod, node);
    EXCEPT(e);
    break;
  case TRY:
    e = type_destruct(mod, node->subs[0], constraint);
    EXCEPT(e);
    e = type_destruct(mod, node->subs[2], constraint);
    EXCEPT(e);
    e = type_inference_try(mod, node);
    EXCEPT(e);
    break;
  case MATCH:
    for (size_t n = 2; n < node->subs_count; n += 2) {
      e = typ_compatible(mod, node->subs[n], node->subs[n]->typ,
                         constraint);
      EXCEPT(e);
    }
    e = type_inference_match(mod, node);
    EXCEPT(e);
    break;
  default:
    assert(FALSE);
  }

  assert(node->typ != typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT));
  assert(node->typ != NULL);
  return 0;
}

static error step_type_inference(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  error e;
  struct node *def = NULL;

  if (node->typ == typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT)
      || node->typ == typ_lookup_builtin(mod, TBI__NOT_TYPEABLE)) {
    return 0;
  }

  switch (node->which) {
  case NUL:
    node->typ = typ_lookup_builtin(mod, TBI_LITERALS_NULL);
    goto ok;
  case IDENT:
    e = scope_lookup(&def, mod, node->scope, node);
    EXCEPT(e);

    if (def->typ == NULL) {
      e = mk_except(mod, node, "'%s' is used before its definition in this scope",
                    idents_value(mod->gctx, node_ident(node)));
      EXCEPT(e);
    }

    if (def->typ->which == TYPE_FUNCTION
        && node->typ != typ_lookup_builtin(mod, TBI__CALL_FUNCTION_SLOT)) {
      if (node_fun_explicit_args_count(def->typ->definition) != 0) {
        e = mk_except_call_args_count(mod, node, def->typ->definition, 0, 0);
        EXCEPT(e);
      }
      e = rewrite_unary_call(mod, node, def->typ);
      EXCEPT(e);
    } else {
      node->typ = def->typ;
      node->flags = def->flags;
    }
    goto ok;
  case IMPORT:
    e = scope_lookup_module(&def, mod, node->subs[0], FALSE);
    EXCEPT(e);
    node->typ = def->typ;
    node->flags = def->flags;
    e = type_destruct_import_path(mod, node->subs[0]);
    EXCEPT(e);
    for (size_t n = 1; n < node->subs_count; ++n) {
      struct node *s = node->subs[n]->subs[0];
      e = type_destruct_import_path(mod, s);
      EXCEPT(e);

      node->subs[n]->typ = s->typ;
      node->subs[n]->flags = s->flags;
    }
    goto ok;
  case NUMBER:
    node->typ = number_literal_typ(mod, node);
    goto ok;
  case BOOL:
    node->typ = typ_lookup_builtin(mod, TBI_BOOL);
    goto ok;
  case STRING:
    node->typ = typ_lookup_builtin(mod, TBI_STATIC_STRING);
    goto ok;
  case SIZEOF:
    node->typ = typ_lookup_builtin(mod, TBI_SIZE);
    goto ok;
  case BIN:
    e = type_inference_bin(mod, node);
    EXCEPT(e);
    goto ok;
  case UN:
    e = type_inference_un(mod, node);
    EXCEPT(e);
    goto ok;
  case TUPLE:
    e = type_inference_tuple(mod, node);
    EXCEPT(e);
    goto ok;
  case CALL:
    e = type_inference_call(mod, node);
    EXCEPT(e);
    goto ok;
  case INIT:
    if (node->as.INIT.is_array) {
      node->typ = typ_lookup_builtin(mod, TBI_LITERALS_INIT_ARRAY);
    } else {
      node->typ = typ_lookup_builtin(mod, TBI_LITERALS_INIT);
    }
    goto ok;
  case RETURN:
    if (node->subs_count > 0) {
      e = type_destruct(mod, node->subs[0], module_retval_get(mod)->typ);
      EXCEPT(e);
      node->typ = node->subs[0]->typ;
      node->flags |= node->subs[0]->flags & NODE__TRANSITIVE;
    } else {
      node->typ = typ_lookup_builtin(mod, TBI_VOID);
    }
    goto ok;
  case BLOCK:
    e = type_inference_block(mod, node);
    EXCEPT(e);
    goto ok;
  case EXCEP:
  case BREAK:
  case CONTINUE:
  case NOOP:
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    goto ok;
  case IF:
    e = type_inference_if(mod, node);
    EXCEPT(e);
    goto ok;
  case FOR:
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    struct node *it = node->subs[IDX_FOR_IT]
      ->subs[IDX_FOR_IT_DEFP]
      ->subs[IDX_FOR_IT_DEFP_DEFN];
    e = typ_check_isa(mod, it, it->typ,
                      typ_lookup_builtin(mod, TBI_ITERATOR));
    EXCEPT(e);
    e = typ_compatible(mod, node_for_block(node), node_for_block(node)->typ,
                       typ_lookup_builtin(mod, TBI_VOID));
    EXCEPT(e);
    goto ok;
  case WHILE:
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    e = typ_compatible(mod, node->subs[0], node->subs[0]->typ,
                       typ_lookup_builtin(mod, TBI_BOOL));
    EXCEPT(e);
    e = typ_compatible(mod, node->subs[1], node->subs[1]->typ,
                       typ_lookup_builtin(mod, TBI_VOID));
    EXCEPT(e);
    EXCEPT(e);
    goto ok;
  case MATCH:
    e = type_inference_match(mod, node);
    EXCEPT(e);
    goto ok;
  case TRY:
    e = type_inference_try(mod, node);
    EXCEPT(e);
    goto ok;
  case DYN:
    assert(typ_is_reference_instance(mod, node->subs[0]->typ));
    node->typ = node->as.DYN.intf;
    goto ok;
  case DEFARG:
  case TYPECONSTRAINT:
    node->typ = node->subs[1]->typ;
    e = type_destruct(mod, node->subs[0], node->typ);
    EXCEPT(e);
    goto ok;
  case DEFGENARG:
    node->typ = typ_genarg_mark_as_abstract(node->subs[1]->typ);
    e = type_destruct(mod, node->subs[0], node->typ);
    EXCEPT(e);
    node->flags |= NODE_IS_TYPE;
    goto ok;
  case SETGENARG:
    node->typ = node->subs[1]->typ;
    e = type_destruct(mod, node->subs[0], node->typ);
    EXCEPT(e);
    node->flags |= NODE_IS_TYPE;
    goto ok;
  case DEFFUN:
  case DEFMETHOD:
    if (node->subs[IDX_GENARGS]->subs_count > 0
        && node_toplevel(node)->generic_definition != NULL) {
      struct typ *mutable_typ = typ_new(node_toplevel(node)->generic_definition,
                                        TYPE_FUNCTION, node->subs[IDX_GENARGS]->subs_count,
                                        node_fun_all_args_count(node));
      mutable_typ->gen_args[0] = node_toplevel(node)->generic_definition->typ;
      mutable_typ->definition = node;
      node->typ = mutable_typ;

      for (size_t n = 0; n < node->typ->gen_arity; ++n) {
        assert(node->subs[IDX_GENARGS]->subs[n]->typ != NULL);
        node->typ->gen_args[1 + n] = node->subs[IDX_GENARGS]->subs[n]->typ;
      }
    } else {
      node->typ = typ_new(node, TYPE_FUNCTION, 0,
                          node_fun_all_args_count(node));
    }

    for (size_t n = 0; n < node->typ->fun_arity; ++n) {
      node->typ->fun_args[n] = node->subs[n + IDX_FUN_FIRSTARG]->typ;
    }
    node->typ->fun_args[node->typ->fun_arity] = node_fun_retval(node)->typ;
    node->flags |= NODE_IS_TYPE;

    goto ok;
  case DEFINTF:
    goto ok;
  case DEFTYPE:
    goto ok;
  case DEFPATTERN:
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    if (node_ident(node->subs[0]) == ID_OTHERWISE) {
      goto ok;
    }
    bool has_expr = node->subs_count > 1 && node->subs[1]->which != DEFNAME;
    if (has_expr) {
      e = type_destruct(mod, node->subs[0], node->subs[1]->typ);
      EXCEPT(e);
    }

    for (size_t n = 0; n < node->subs_count; ++n) {
      struct node *d = node->subs[n];
      if (d->which == DEFNAME) {
        d->as.DEFNAME.pattern->typ = d->typ;

        if (has_expr) {
          d->flags |= d->as.DEFNAME.expr->flags & NODE__TRANSITIVE;
        }
      }
    }
    goto ok;
  case DEFFIELD:
    node->typ = node->subs[1]->typ;
    goto ok;
  case EXAMPLE:
    e = type_destruct(mod, node->subs[0],
                      typ_lookup_builtin(mod, TBI_BOOL));
    EXCEPT(e);
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    goto ok;
  case LET:
  case DELEGATE:
  case PRE:
  case POST:
  case INVARIANT:
  case ISALIST:
  case GENARGS:
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    goto ok;
  case ISA:
    node->typ = node->subs[0]->typ;
    node->flags = node->subs[0]->flags & NODE__TRANSITIVE;
    goto ok;
  case DIRECTDEF:
    node->typ = node->as.DIRECTDEF.definition->typ;
    node->flags = node->as.DIRECTDEF.definition->flags;
    goto ok;
  case DEFCHOICE:
    node->typ = node_parent(node)->typ;
    goto ok;
  default:
    goto ok;
  }

ok:
  assert(node->typ != typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT));
  assert(node->typ != NULL);
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

static error step_type_drop_retval(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    module_retval_clear(mod);
    return 0;
  default:
    return 0;
  }
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

static uint32_t ident_hash(const ident *id) {
  return *id;
}

static int ident_cmp(const ident *a, const ident *b) {
  return memcmp(a, b, sizeof(*a));
}

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
  assert(expr->typ->definition->which == DEFTYPE);
  const bool enum_or_sum = expr->typ->definition->as.DEFTYPE.kind == DEFTYPE_ENUM
    || expr->typ->definition->as.DEFTYPE.kind == DEFTYPE_SUM;

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
          GOTO_EXCEPT(e);
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
      GOTO_EXCEPT(e);
    }

    idents_set_set(&set, id, TRUE);
  }

  if (idents_set_count(&set) != defchoice_count(expr->typ->definition)) {
    e = mk_except_type(mod, node, "non-exhaustive match");
    GOTO_EXCEPT(e);
  }

ok:
except:
  idents_set_destroy(&set);
  return e;
}

static void add_inferred_isa(struct module *mod, struct node *deft, const char *path) {
  struct node *isalist = deft->subs[IDX_ISALIST];
  assert(isalist->which == ISALIST);
  struct node *isa = mk_node(mod, isalist, ISA);
  isa->as.ISA.is_export = node_toplevel(deft)->is_inline;
  mk_expr_abspath(mod, isa, path);

  error e = catchup(mod, NULL, isa, isalist->scope, CATCHUP_BELOW_CURRENT);
  assert(!e);
  e = morningtypepass(mod, isa);
  assert(!e);

  // isalist are typed in snackpass, but add_inferred_isa() is called later. We
  // forcibly add the new typ to it if it's not already there.
  if (typ_isa(mod, deft->typ, isa->typ)) {
    return;
  }

  struct isalist *tisalist = NULL;
  switch (deft->which) {
  case DEFTYPE:
    tisalist = &deft->as.DEFTYPE.isalist;
    break;
  case DEFINTF:
    tisalist = &deft->as.DEFINTF.isalist;
    break;
  default:
    assert(FALSE);
  }

  const size_t last = tisalist->count;
  tisalist->count += 1;
  tisalist->list = realloc(tisalist->list,
                           tisalist->count * sizeof(*tisalist->list));
  tisalist->exported = realloc(tisalist->exported,
                               tisalist->count * sizeof(*tisalist->exported));

  tisalist->list[last] = isa->typ;
  tisalist->exported[last] = isa->as.ISA.is_export;
}

static error step_add_builtin_enum_isalist(struct module *mod, struct node *node, void *user, bool *stop) {
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
  struct node *defi = user;
  if (node->which == IDENT) {
    ident id = node_ident(node);
    if (id == ID_THIS) {
      node->which = DIRECTDEF;
      node->as.DIRECTDEF.definition = defi;
    }
  }
  return 0;
}

static void intf_proto_deepcopy(struct module *mod, struct node *defi,
                                struct node *dst, struct node *src) {
  node_deepcopy(mod, dst, src);

  static const step down[] = {
    step_rewrite_final_this,
    NULL,
  };

  static const step up[] = {
    NULL,
  };

  PUSH_STATE(mod->state);
  error e = pass(mod, dst, down, up, -1, defi);
  assert(!e);
  POP_STATE(mod->state);
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
  intf_proto_deepcopy(mod, node_parent(proto), d, proto);
  mk_expr_abspath(mod, d, builtingen_abspath[bg]);
  rew_move_last_over(d, 0, FALSE);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->scope_name = node_ident(deft);
  toplevel->is_prototype = FALSE;
  toplevel->builtingen = bg;
  toplevel->is_export = node_toplevel(deft)->is_export;
  toplevel->is_inline = node_toplevel(deft)->is_inline;

  if (insert_pos >= 0) {
    rew_insert_last_at(modbody, insert_pos);
  } else {
    append_member(deft, d);
  }

  e = catchup(mod, NULL, d, deft->scope, CATCHUP_BELOW_CURRENT);
  assert(!e);
}

static void define_defchoice_builtin(struct module *mod, struct node *ch,
                                     enum builtingen bg, enum node_which which) {
  struct node *deft = node_parent(ch);

  struct node *proto = NULL;
  error e = scope_lookup_abspath(&proto, ch, mod, builtingen_abspath[bg]);
  assert(!e);

  struct node *d = mk_node(mod, ch, which);
  intf_proto_deepcopy(mod, node_parent(proto), d, proto);
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
    struct node *arg = mk_node(mod, d, DEFARG);
    struct node *name = mk_node(mod, arg, IDENT);
    name->as.IDENT.name = ID_C;
    struct node *typename = mk_node(mod, arg, DIRECTDEF);
    typename->as.DIRECTDEF.definition = ch->subs[IDX_CH_PAYLOAD]->typ->definition;

    rew_insert_last_at(d, IDX_FUN_FIRSTARG + ((d->which == DEFMETHOD) ? 1 : 0));
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
                          targ, typ_lookup_builtin(mod, TBI_COPYABLE));
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
    EXCEPT(e);
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
  intf_proto_deepcopy(mod, node_parent(proto), d, proto);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->scope_name = node_ident(deft);
  toplevel->is_prototype = FALSE;
  toplevel->builtingen = bg;
  toplevel->is_export = node_toplevel(ctor)->is_export;
  toplevel->is_inline = node_toplevel(ctor)->is_inline;

  for (size_t n = IDX_FUN_FIRSTARG + 1; n < IDX_FUN_FIRSTARG + node_fun_all_args_count(ctor); ++n) {
    struct node *arg = ctor->subs[n];
    assert(arg->which == DEFARG);
    struct node *cpy = node_new_subnode(mod, d);
    intf_proto_deepcopy(mod, node_parent(proto), cpy, arg);
    rew_insert_last_at(d, n - 1);
  }

  if (insert_pos >= 0) {
    rew_insert_last_at(modbody, insert_pos);
  } else {
    append_member(deft, d);
  }

  e = catchup(mod, NULL, d, deft->scope, CATCHUP_BELOW_CURRENT);
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

  if (typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_TRIVIAL_CTOR))) {
    define_builtin(mod, node, BG_TRIVIAL_CTOR_CTOR);
    define_builtin(mod, node, BG_TRIVIAL_CTOR_MK);
    define_builtin(mod, node, BG_TRIVIAL_CTOR_NEW);
  } else if (typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_DEFAULT_CTOR))) {
    define_builtin(mod, node, BG_DEFAULT_CTOR_MK);
    define_builtin(mod, node, BG_DEFAULT_CTOR_NEW);
  } else if (typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_CTOR_WITH))) {
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

  if (typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_TRIVIAL_ARRAY_CTOR))) {
    return 0;
  }

  if (typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_ARRAY_CTOR))) {
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
    if (!typ_isa(mod, node->as.DEFTYPE.choice_typ,
                 typ_lookup_builtin(mod, TBI_NATIVE_INTEGER))) {
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

  if (!typ_is_concrete(mod, node->typ)
      || typ_is_reference_instance(mod, node->typ)) {
    return 0;
  }

  if (typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_TRIVIAL_COPY))) {
    define_builtin(mod, node, BG_TRIVIAL_COPY_COPY_CTOR);
  }
  if (typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_TRIVIAL_EQUALITY))) {
    define_builtin(mod, node, BG_TRIVIAL_EQUALITY_OPERATOR_EQ);
    define_builtin(mod, node, BG_TRIVIAL_EQUALITY_OPERATOR_NE);
  }

  return 0;
}

static void define_dispatch(struct module *mod, struct node *deft, const struct typ *tintf) {
  struct node *intf = tintf->definition;

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
    intf_proto_deepcopy(mod, node_parent(proto), d, proto);
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

  for (size_t n = 0; n < typ_isalist_count(node->typ); ++n) {
    assert(node->subs[IDX_ISALIST]->which == ISALIST);
    assert(node->subs[IDX_ISALIST]->subs[n]->which == ISA);
    if (!node->subs[IDX_ISALIST]->subs[n]->as.ISA.is_explicit) {
      continue;
    }

    const struct typ *intf = typ_isalist(node->typ)[n];
    const struct typ *check_intf = intf;
    if (typ_equal(mod, intf, typ_lookup_builtin(mod, TBI_SUM_COPY))) {
      check_intf = typ_lookup_builtin(mod, TBI_COPYABLE);
    } else if (typ_equal(mod, intf, typ_lookup_builtin(mod, TBI_SUM_EQUALITY))) {
      check_intf = typ_lookup_builtin(mod, TBI_HAS_EQUALITY);
    } else if (typ_equal(mod, intf, typ_lookup_builtin(mod, TBI_SUM_ORDER))) {
      check_intf = typ_lookup_builtin(mod, TBI_ORDERED);
    }

    for (size_t c = 0; c < node->subs_count; ++c) {
      struct node *ch = node->subs[c];
      if (ch->which != DEFCHOICE) {
        continue;
      }

      const struct typ *tch = NULL;
      if (typ_equal(mod, ch->subs[IDX_CH_PAYLOAD]->typ, typ_lookup_builtin(mod, TBI_VOID))) {
        tch = node->as.DEFTYPE.choice_typ;
      } else {
        tch = ch->subs[IDX_CH_PAYLOAD]->typ;
      }

      error e = typ_check_isa(mod, ch, tch, check_intf);
      EXCEPT(e);
    }

    if (typ_equal(mod, intf, typ_lookup_builtin(mod, TBI_SUM_COPY))) {
      if (!typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_TRIVIAL_COPY))) {
        define_builtin(mod, node, BG_SUM_COPY);
      }
    } else if (typ_equal(mod, intf, typ_lookup_builtin(mod, TBI_SUM_EQUALITY))) {
      if (!typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_TRIVIAL_EQUALITY))) {
        define_builtin(mod, node, BG_SUM_EQUALITY_EQ);
        define_builtin(mod, node, BG_SUM_EQUALITY_NE);
      }
    } else if (typ_equal(mod, intf, typ_lookup_builtin(mod, TBI_SUM_ORDER))) {
      define_builtin(mod, node, BG_SUM_ORDER_LE);
      define_builtin(mod, node, BG_SUM_ORDER_LT);
      define_builtin(mod, node, BG_SUM_ORDER_GT);
      define_builtin(mod, node, BG_SUM_ORDER_GE);
    } else {
      define_dispatch(mod, node, intf);
    }
  }

  return 0;
}

static error step_rewrite_def_return_through_ref(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which != DEFFUN && node->which != DEFMETHOD) {
    return 0;
  }

  struct node *retval = node_fun_retval(node);
  if (typ_isa(mod, retval->typ, typ_lookup_builtin(mod, TBI_RETURN_BY_COPY))) {
    return 0;
  }

  if (retval->which == DEFARG) {
    return 0;
  }

  const size_t where = rew_find_subnode_in_parent(node, retval);
  struct node *named = mk_node(mod, node, DEFARG);
  named->as.DEFARG.is_retval = TRUE;
  struct node *name = mk_node(mod, named, IDENT);
  name->as.IDENT.name = ID_NRETVAL;
  rew_append(named, retval);
  rew_move_last_over(node, where, TRUE);

  error e = lexical_retval(mod, node, named);
  EXCEPT(e);

  const struct node *except[] = { retval, NULL };
  e = catchup(mod, except, named, node->scope, CATCHUP_BELOW_CURRENT);
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

static void fix_scopes_after_move(struct node *node) {
  node->scope->node = node;
  for (size_t n = 0; n < node->subs_count; ++n) {
    node->subs[n]->scope->parent = node->scope;
  }
}

static error step_string_literal_conversion(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->which != STRING
      || typ_equal(mod, node->typ, typ_lookup_builtin(mod, TBI_STATIC_STRING))) {
    return 0;
  }

  struct node copy = *node;

  memset(node, 0, sizeof(*node));
  node->which = CALL;

  struct node *fun = mk_node(mod, node, DIRECTDEF);
  struct node *fund = node_get_member(mod, copy.typ->definition, ID_FROM_LITERAL_STRING);
  assert(fund != NULL);
  fun->as.DIRECTDEF.definition = fund;

  struct node *literal = node_new_subnode(mod, node);
  *literal = copy;
  fix_scopes_after_move(literal);
  literal->typ = typ_lookup_builtin(mod, TBI_STATIC_STRING);

  const struct node *except[] = { literal, NULL };
  error e = catchup(mod, except, node, copy.scope->parent, CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static error step_bool_literal_conversion(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  if (node->which != BOOL
      || typ_equal(mod, node->typ, typ_lookup_builtin(mod, TBI_BOOL))) {
    return 0;
  }

  struct node copy = *node;

  memset(node, 0, sizeof(*node));
  node->which = CALL;

  struct node *fun = mk_node(mod, node, DIRECTDEF);
  struct node *fund = node_get_member(mod, copy.typ->definition, ID_FROM_LITERAL_BOOL);
  assert(fund != NULL);
  fun->as.DIRECTDEF.definition = fund;

  struct node *literal = node_new_subnode(mod, node);
  *literal = copy;
  fix_scopes_after_move(literal);
  literal->typ = typ_lookup_builtin(mod, TBI_BOOL);

  const struct node *except[] = { literal, NULL };
  error e = catchup(mod, except, node, copy.scope->parent, CATCHUP_REWRITING_CURRENT);
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

  if (typ_isa(mod, left->typ, typ_lookup_builtin(mod, TBI_NATIVE_INTEGER))
      || typ_isa(mod, left->typ, typ_lookup_builtin(mod, TBI_NATIVE_BOOLEAN))
      || typ_isa(mod, left->typ, typ_lookup_builtin(mod, TBI_NATIVE_FLOATING))) {
    return 0;
  }

  struct node *dleft = left->typ->definition;
  if (dleft->which == DEFTYPE
      && dleft->as.DEFTYPE.kind == DEFTYPE_ENUM
      && typ_isa(mod, dleft->as.DEFTYPE.choice_typ,
                 typ_lookup_builtin(mod, TBI_NATIVE_INTEGER))) {
    return 0;
  }

  if (operator_ident[op] == 0) {
    return 0;
  }

  struct scope *saved_parent = node->scope->parent;

  memset(node, 0, sizeof(*node));
  node->which = CALL;
  struct node *fun = mk_node(mod, node, BIN);
  fun->as.BIN.operator = TDOT;
  struct node *base = mk_node(mod, fun, DIRECTDEF);
  base->as.DIRECTDEF.definition = dleft;
  struct node *member = mk_node(mod, fun, IDENT);
  member->as.IDENT.name = operator_ident[op];

  const struct node *except[3] = { NULL, NULL, NULL };
  except[0] = left;
  rew_append(node, generated_expr_ref(TREFDOT, left));

  if (node->subs_count == 2) {
    except[1] = right;
    rew_append(node, generated_expr_ref(TREFDOT, right));
  }

  error e = catchup(mod, except, node, saved_parent, CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

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

  if (typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_TRIVIAL_ARRAY_CTOR))) {
    return 0;
  }

  struct node copy = *node;

  memset(node, 0, sizeof(*node));
  node->which = CALL;
  struct node *fun = mk_node(mod, node, DIRECTDEF);
  fun->as.DIRECTDEF.definition = node_get_member(mod, copy.typ->definition, ID_MKV);

  struct node *ref_array = mk_node(mod, node, UN);
  ref_array->as.UN.operator = TREFDOT;
  struct node *array = node_new_subnode(mod, ref_array);
  *array = copy;
  fix_scopes_after_move(array);
  array->typ = fun->as.DIRECTDEF.definition->subs[IDX_FUN_FIRSTARG]->typ->gen_args[1];

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
    && !typ_isa(mod, expr->typ, typ_lookup_builtin(mod, TBI_RETURN_BY_COPY));
}

static error assign_copy_call_inference(struct module *mod, struct node *node) {
  struct node *left = node->subs[0];
  struct node *right = node->subs[1];
  struct scope *saved_parent = node->scope->parent;

  memset(node, 0, sizeof(*node));
  node->which = CALL;
  struct node *fun = mk_node(mod, node, BIN);
  fun->as.BIN.operator = TDOT;
  struct node *base = mk_node(mod, fun, DIRECTDEF);
  base->as.DIRECTDEF.definition = left->typ->definition;
  struct node *member = mk_node(mod, fun, IDENT);
  member->as.IDENT.name = ID_COPY_CTOR;

  rew_append(node, generated_expr_ref(TREFSHARP, left));
  rew_append(node, generated_expr_ref(TREFDOT, right));

  const struct node *except[] = { left, right, NULL };
  error e = catchup(mod, except, node, saved_parent, CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static error defname_copy_call_inference(struct module *mod, struct node *node) {
  struct node *let = node->scope->parent->parent->node;
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
  struct scope *saved_parent = within->scope->parent;

  struct node *copycall = mk_node(mod, within, CALL);
  struct node *fun = mk_node(mod, copycall, BIN);
  fun->as.BIN.operator = TDOT;
  struct node *base = mk_node(mod, fun, DIRECTDEF);
  base->as.DIRECTDEF.definition = left->typ->definition;
  struct node *member = mk_node(mod, fun, IDENT);
  member->as.IDENT.name = ID_COPY_CTOR;

  struct node *copyleft = calloc(1, sizeof(struct node));
  node_deepcopy(mod, copyleft, left);

  rew_append(copycall, generated_expr_ref(TREFSHARP, copyleft));
  // Steal right:
  node->as.DEFNAME.expr = NULL;
  rew_append(copycall, generated_expr_ref(TREFDOT, right));

  error e = catchup(mod, NULL, copycall, saved_parent, CATCHUP_AFTER_CURRENT);
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

  if (typ_isa(mod, left->typ, typ_lookup_builtin(mod, TBI_TRIVIAL_COPY))) {
    return 0;
  }

  if (expr_is_return_through_ref(NULL, mod, right)) {
    return 0;
  }

  error e = typ_check_isa(mod, left, left->typ, typ_lookup_builtin(mod, TBI_COPYABLE));
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

static error check_exhaustive_intf_impl_eachisalist(struct module *mod, const struct typ *t,
                                                    const struct typ *intf, void *user) {
  (void) user;
  struct node *deft = t->definition;
  const struct node *dintf = intf->definition;

  for (size_t m = 0; m < dintf->subs_count; ++m) {
    const struct node *f = dintf->subs[m];
    if (f->which != DEFFUN && f->which != DEFMETHOD) {
      continue;
    }

    if (node_get_member(mod, deft, node_ident(f)) == NULL) {
      error e = mk_except_type(mod, deft, "type '%s' isa '%s' but does not implement '%s'",
                               typ_pretty_name(mod, deft->typ), typ_pretty_name(mod, intf),
                               idents_value(mod->gctx, node_ident(f)));
      EXCEPT(e);
    }

    // FIXME check that the prototype is an exact match.
  }

  return 0;
}

static error step_check_exhaustive_intf_impl(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which != DEFTYPE) {
    return 0;
  }

  if (typ_is_pseudo_builtin(mod, node->typ)) {
    return 0;
  }

  error e = typ_isalist_foreach(mod, node->typ, ISALIST_FILTER_TRIVIAL_ISALIST,
                                check_exhaustive_intf_impl_eachisalist, NULL);
  EXCEPT(e);

  return 0;
}

static bool need_insert_dyn(struct module *mod, const struct typ *intf,
                            const struct typ *concrete) {
  return
    typ_is_reference_instance(mod, intf)
    && intf->gen_args[1]->definition->which == DEFINTF
    && typ_is_reference_instance(mod, concrete)
    && concrete->gen_args[1]->definition->which != DEFINTF;
}

static error insert_dyn(struct module *mod, struct node *node,
                        const struct node *target, struct node *src) {
  struct node *d = mk_node(mod, node, DYN);
  d->as.DYN.intf = target->typ;

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

static error step_dyn_inference(struct module *mod, struct node *node, void *user, bool *stop) {
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
    for (size_t n = 1; n < node->subs_count; ++n) {
      struct node *arg = node->subs[n];
      if (arg->which == BLOCK) {
        break;
      }
      target = node->subs[0]->typ->definition->subs[IDX_FUN_FIRSTARG+n-1];
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
  block->typ = typ_lookup_builtin(mod, TBI_VOID);

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
  node->typ = typ_lookup_builtin(mod, TBI_VOID);

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
        || typ_equal(mod, node->subs[0]->typ, typ_lookup_builtin(mod, TBI_VOID))) {
      return 0;
    }

    expr = node->subs[0];
    if (expr_is_return_through_ref(&init_expr, mod, expr)) {
      // Keep node->as.RETURN.return_through_ref_expr null as the
      // subexpression CALL or INIT will directly write to it.
      if (expr->which == CALL) {
        expr->as.CALL.return_through_ref_expr = retval_name(mod);
      } else if (expr->which == INIT) {
        init_expr->as.INIT.target_expr = retval_name(mod);
      } else if (is_block_like(expr)) {
        block_like_insert_value_assign(mod, expr, NULL, node_ident(retval_name(mod)));
      } else {
        assert(FALSE);
      }
    } else if (!typ_isa(mod, expr->typ, typ_lookup_builtin(mod, TBI_RETURN_BY_COPY))
               && typ_isa(mod, expr->typ, typ_lookup_builtin(mod, TBI_COPYABLE))) {
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
      && !typ_isa(mod, node->typ,
                  typ_lookup_builtin(mod, TBI_RETURN_BY_COPY))) {
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
        EXCEPT(e);
      }

      temporaries_add(temps, node);
    }
    break;
  case INIT:
    if (significant_parent->which == RETURN
        && !typ_isa(mod, node->typ,
                    typ_lookup_builtin(mod, TBI_RETURN_BY_COPY))) {
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
    if (typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_RETURN_BY_COPY))) {
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
  case IF:
  case TRY:
  case MATCH:
    if (typ_equal(mod, node->typ, typ_lookup_builtin(mod, TBI_VOID))) {
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
    PUSH_STATE(mod->state);
    for (size_t s = mod->state->prev->stepping + 1; pa->ups[s] != NULL; ++s) {
      mod->state->upward = TRUE;
      mod->state->stepping = s;

      bool stop = FALSE;
      e = pa->ups[s](mod, new_statement, NULL, &stop);
      assert(!e);
      assert(!stop);
    }
    POP_STATE(mod->state);
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
      assert(rv->typ->gen_arity == 1);
      typ->as.DIRECTDEF.definition = rv->typ->gen_args[1]->definition;
    } else {
      typ->as.DIRECTDEF.definition = rv->typ->definition;
    }

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

  PUSH_STATE(mod->state);
  error e = pass(mod, node, temprvalue_down, temprvalue_up, -1, &temps);
  EXCEPT(e);
  POP_STATE(mod->state);

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

    struct node *nvalue;
    if (rv->which == UN && OP_KIND(rv->as.UN.operator) == OP_UN_REFOF) {
      rv->subs[0]->flags |= NODE_IS_TEMPORARY;
      rew_append(assign, rv->subs[0]);
      except[0] = rv->subs[0];

      nvalue = mk_node(mod, nrv, UN);
      nvalue->as.UN.operator = rv->as.UN.operator;
      struct node *nvalue_name = mk_node(mod, nvalue, IDENT);
      nvalue_name->as.IDENT.name = g;
    } else {
      rv->flags |= NODE_IS_TEMPORARY;
      rew_append(assign, rv);
      except[0] = rv;

      nvalue = mk_node(mod, nrv, IDENT);
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
  PUSH_STATE(mod->state);

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

  POP_STATE(mod->state);
  POP_STATE(mod->stage->state);

  return 0;
}

static error step_complete_instantiation(struct module *mod, struct node *node, void *user, bool *stop) {
  DSTEP(mod, node);

  struct toplevel *toplevel = node_toplevel(node);
  if (toplevel == NULL) {
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
      step_add_builtin_members,
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
      step_lexical_scoping,
      NULL,
    },
    {
      step_type_deftypes_defintfs,
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_FORWARD, "imports",
    {
      step_stop_submodules,
      step_lexical_import,
      NULL,
    },
    {
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_FORWARD, "genargs",
    {
      step_stop_submodules,
      step_stop_marker_tbi,
      step_stop_funblock,
      step_type_inference_genargs,
      NULL,
    },
    {
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_FORWARD, "isalist",
    {
      step_stop_submodules,
      step_stop_marker_tbi,
      step_stop_funblock,
      NULL,
    },
    {
      step_add_builtin_enum_isalist,
      step_add_builtin_detect_ctor_intf,
      step_type_inference_isalist,
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_FORWARD, "aliases",
    {
      step_stop_submodules,
      step_stop_marker_tbi,
      step_stop_funblock,
      NULL,
    },
    {
      step_type_aliases,
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_FORWARD, "deffields",
    {
      step_stop_submodules,
      step_stop_marker_tbi,
      step_stop_funblock,
      NULL,
    },
    {
      step_type_deffields,
      step_type_defchoices,
      step_complete_instantiation,
      NULL,
    }
  },

  {
    PASS_FORWARD, "deffuns",
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
      step_rewrite_defname_no_expr,
      step_rewrite_sum_constructors,
      step_detect_not_dyn_intf_up,
      step_type_inference,
      step_remove_typeconstraints,
      step_type_drop_retval,
      step_type_drop_excepts,
      step_check_exhaustive_match,
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
      step_string_literal_conversion,
      step_bool_literal_conversion,
      step_operator_call_inference,
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

      step_type_drop_retval,
      step_pop_fun_state,
      step_complete_instantiation,
      NULL,
    }
  },

  { PASS__NONE, NULL, { NULL }, { NULL } },
};

const struct pass *passes = _passes;
