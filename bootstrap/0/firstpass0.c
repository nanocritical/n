#include "firstpass.h"

error pass(struct module *mod, struct node *root, const step *down_steps, const step *up_steps,
           struct node **except, void *user) {
  error e;
  if (root == NULL) {
    root = mod->root;
  }

  if (except != NULL) {
    for (size_t n = 0; except[n] != NULL; ++n) {
      if (except[n] == root) {
        return 0;
      }
    }
  }

  bool stop = FALSE;
  for (size_t s = 0; down_steps[s] != NULL; ++s) {
    bool stop = FALSE;
    e = down_steps[s](mod, root, user, &stop);
    EXCEPT(e);
    if (stop) {
      return 0;
    }
  }

  for (size_t n = 0; n < root->subs_count; ++n) {
    struct node *node = root->subs[n];
    e = pass(mod, node, down_steps, up_steps, except, user);
    EXCEPT(e);
  }

  for (size_t s = 0; up_steps[s] != NULL; ++s) {
    e = up_steps[s](mod, root, user, &stop);
    EXCEPT(e);

    if (stop) {
      return 0;
    }
  }

  return 0;
}

error one_level_pass(struct module *mod, struct node *root, const step *down_steps, const step *up_steps,
                     void *user) {
  error e;

  bool stop = FALSE;
  for (size_t s = 0; down_steps[s] != NULL; ++s) {
    bool stop = FALSE;
    e = down_steps[s](mod, root, user, &stop);
    EXCEPT(e);
    if (stop) {
      return 0;
    }
  }

  for (size_t s = 0; up_steps[s] != NULL; ++s) {
    e = up_steps[s](mod, root, user, &stop);
    EXCEPT(e);

    if (stop) {
      return 0;
    }
  }

  return 0;
}

static error zero_to_forward_for_generated(struct module *mod, struct node *node,
                                           struct scope *parent_scope);
static error zero_to_first_for_generated(struct module *mod, struct node *node,
                                         struct node **except, struct scope *parent_scope);
static error zero_to_second_for_generated(struct module *mod, struct node *node,
                                          struct node **except, struct scope *parent_scope);

static struct node *add_instance_deepcopy_from_pristine(struct module *mod,
                                                        struct node *node,
                                                        struct node *pristine) {
  struct toplevel *toplevel = node_toplevel(node);
  const size_t idx = toplevel->instances_count;
  toplevel->instances_count += 1;
  toplevel->instances = realloc(toplevel->instances,
                                toplevel->instances_count * sizeof(*toplevel->instances));
  struct node *instance = calloc(1, sizeof(**toplevel->instances));
  toplevel->instances[idx] = instance;

  node_deepcopy(mod, instance, pristine);
  node_toplevel(instance)->instances = NULL;
  node_toplevel(instance)->instances_count = 0;

  instance->as.DEFTYPE.members_count = pristine->as.DEFTYPE.members_count,
  instance->as.DEFTYPE.members = calloc(instance->as.DEFTYPE.members_count,
                                        sizeof(*instance->as.DEFTYPE.members));
  for (size_t n = 0; n < pristine->as.DEFTYPE.members_count; ++n) {
    instance->as.DEFTYPE.members[n] = calloc(1, sizeof(**instance->as.DEFTYPE.members));
    node_deepcopy(mod, instance->as.DEFTYPE.members[n], pristine->as.DEFTYPE.members[n]);
  }

  return instance;
}

static void add_deftype_pristine_external_member(struct module *mod, struct node *deft,
                                                 struct node *member) {
  assert(deft->which == DEFTYPE);
  struct node *pristine = node_toplevel(deft)->instances[0];

  pristine->as.DEFTYPE.members_count += 1;
  pristine->as.DEFTYPE.members = realloc(
    pristine->as.DEFTYPE.members,
    pristine->as.DEFTYPE.members_count * sizeof(*pristine->as.DEFTYPE.members));

  pristine->as.DEFTYPE.members[pristine->as.DEFTYPE.members_count - 1] = member;
}

static error step_generics_pristine_copy(struct module *mod, struct node *node, void *user, bool *stop) {
  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
  case DEFFUN:
  case DEFMETHOD:
    if (node->subs[IDX_GENARGS]->subs_count > 0) {
      (void) add_instance_deepcopy_from_pristine(mod, node, node);
    }
    return 0;
  default:
    return 0;
  }
}

static error step_detect_prototypes(struct module *mod, struct node *node, void *user, bool *stop) {
  struct toplevel *toplevel = node_toplevel(node);
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    toplevel->is_prototype = node->subs[node->subs_count - 1]->which != BLOCK;
    break;
  case DEFTYPE:
  case DEFINTF:
    toplevel->is_prototype = node->subs_count <= 3;
    break;
  default:
    break;
  }
  return 0;
}

static error step_detect_generic_interfaces_down(struct module *mod, struct node *node, void *user, bool *stop) {
  switch (node->which) {
  case DEFINTF:
    mod->intf_uses_this = FALSE;
    break;
  case IDENT:
    if (node->as.IDENT.name == ID_THIS) {
      mod->intf_uses_this = TRUE;
    }
    break;
  default:
    break;
  }
  return 0;
}

static error step_detect_generic_interfaces_up(struct module *mod, struct node *node, void *user, bool *stop) {
  switch (node->which) {
  case DEFINTF:
    node->as.DEFINTF.is_implied_generic = mod->intf_uses_this;
    break;
  default:
    break;
  }
  return 0;
}

// Must be run before builtins are added.
static error step_detect_deftype_kind(struct module *mod, struct node *node, void *user, bool *stop) {
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

void do_mk_expr_abspath(struct module *mod, struct node *node, const char *path, ssize_t len) {
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

struct node *mk_expr_abspath(struct module *mod, struct node *node, const char *path) {
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
  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
    break;
  default:
    return 0;
  }

  struct node *let = mk_node(mod, node, LET);
  struct node *defp = mk_node(mod, let, DEFPATTERN);
  struct node *name = mk_node(mod, defp, IDENT);
  name->as.IDENT.name = ID_THIS;
  struct node *expr = mk_node(mod, defp, IDENT);
  expr->as.IDENT.name = node_ident(node);

  rew_insert_last_at(node, 3);

  return 0;
}

static error step_add_codegen_variables(struct module *mod, struct node *node, void *user, bool *stop) {
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
  case FOR:
  case MATCH:
  case TRY:
  case DEFTYPE:
  case DEFINTF:
  case LET:
    break;
  default:
    return 0;
  }

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
  default:
    break;
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
    defn = mk_node(mod, defpattern, DEFNAME);
    defn->as.DEFNAME.pattern = pattern;
    defn->as.DEFNAME.expr = expr;
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
  e = scope_lookup_ident_wontimport(&n, mod, *scope, i->as.IDENT.name, TRUE);
  if (e == EINVAL) {
    n = import_path;
    e = scope_define_ident(mod, *scope, i->as.IDENT.name, n);
    EXCEPT(e);
  } else if (e) {
    // Repeat bound-to-fail lookup to get the error message right.
    e = scope_lookup_ident_wontimport(&n, mod, *scope, i->as.IDENT.name, FALSE);
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
    struct node *target = NULL;
    e = scope_lookup(&target, mod, mod->gctx->modules_root.scope, full_import_path);
    EXCEPT(e);

    assert(full_import_path->which == BIN);
    assert(full_import_path->subs[1]->which == IDENT);
    e = scope_define_ident(mod, scope, full_import_path->subs[1]->as.IDENT.name, import->subs[n]);
    EXCEPT(e);
  }

  return 0;
}

static error lexical_import(struct scope *scope, struct module *mod, struct node *import);

static bool is_forward_declaration(struct module *mod, struct node *node) {
  struct node *d = NULL;
  error e = scope_lookup_ident_wontimport(&d, mod, mod->body->scope, node_ident(node), FALSE);
  assert(!e);

  return d != node;
}

static error lexical_import_all_from_path(struct scope *scope, struct module *mod,
                                          struct node *import) {
  error e;
  struct node *target = NULL;
  e = scope_lookup(&target, mod, mod->gctx->modules_root.scope, import->subs[0]);
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
    struct token tok;
    tok.t = TIDENT;
    tok.value = idents_value(mod->gctx, id);
    tok.len = strlen(tok.value);

    copy_and_extend_import_path(mod, imported, import, &tok);

    e = scope_define_ident(mod, scope, id, imported);
    EXCEPT(e);

    e = pass(mod, imported, down, up, NULL, NULL);
    EXCEPT(e);
    imported->scope->parent = import->scope;
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
  case UN:
  case IDENT:
  case CALL:
    break;
  case DEFARG:
    e = scope_define(mod, fun->scope, retval->subs[0], retval);
    EXCEPT(e);
    break;
  default:
    e = mk_except(mod, retval, "return value type structure not supported");
    EXCEPT(e);
    break;
  }

  return 0;
}

static error step_lexical_scoping(struct module *mod, struct node *node, void *user, bool *stop) {
  struct node *id = NULL;
  struct scope *sc = NULL;
  error e;

  struct node *container = NULL;
  const struct toplevel *toplevel = NULL;

  switch (node->which) {
  case IMPORT:
    e = lexical_import(node->scope->parent, mod, node);
    EXCEPT(e);
    return 0;
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
        || node->scope->parent->node->which == DEFINTF) {
      sc = node->scope->parent;
    } else {
      if (node->scope->parent->node->which == DEFTYPE) {
        // Generic instance *members* already have the 'right' parent.
        container = node->scope->parent->node;
        sc = container->scope;
      } else {
        e = scope_lookup_ident_wontimport(&container, mod, node->scope->parent,
                                          toplevel->scope_name, FALSE);
        EXCEPT(e);
        sc = container->scope;
        node->scope->parent = sc;
      }

      const struct toplevel *toplevel = node_toplevel_const(container);
      if (toplevel != NULL
          && toplevel->instances != NULL
          && toplevel->generic_definition == NULL) {
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

static error step_type_destruct_mark(struct module *mod, struct node *node, void *user, bool *stop) {
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
    case OP_BIN_NUM_RHS_U16:
      break;
    default:
      assert(FALSE);
      break;
    }
    break;
  case INIT:
    mark_subs(mod, node, pending, 1, node->subs_count, 1);
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
    node->typ = pending;
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

static error step_type_inference(struct module *mod, struct node *node, void *user, bool *stop);

error earlytypepass(struct module *mod, struct node *node) {
  static const step down[] = {
    step_stop_marker_tbi,
    step_type_destruct_mark,
    NULL,
  };

  static const step up[] = {
    step_type_inference,
    NULL,
  };

  error e = pass(mod, node, down, up, NULL, NULL);
  EXCEPT(e);

  return 0;
}

static error step_type_definitions(struct module *mod, struct node *node, void *user, bool *stop) {
  error e;
  switch (node->which) {
  case IMPORT:
    e = earlytypepass(mod, node);
    EXCEPT(e);
    return 0;
  case DEFTYPE:
  case DEFINTF:
    break;
  default:
    return 0;
  }

  assert(node->subs[0]->which == IDENT);
  ident id = node->subs[0]->as.IDENT.name;
  if (id >= ID_TBI__FIRST && id <= ID_TBI__LAST) {
    // FIXME Effectively reserving these idents for builtin types, but
    // that's a temporary trick to avoid having to look up the current
    // module path.
    mod->gctx->builtin_typs_by_name[id]->definition = node;
    node->typ = mod->gctx->builtin_typs_by_name[id];
  } else if (node->subs[IDX_GENARGS]->subs_count > 0
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
  } else {
    node->typ = typ_new(node, TYPE_DEF, 0, 0);
  }
  node->flags = NODE_IS_TYPE;

  return 0;
}

static error step_type_inference_isalist(struct module *mod, struct node *node, void *user, bool *stop) {
  error e;

  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
    break;
  default:
    return 0;
  }

  if (node->typ == typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT)
      || node->typ == typ_lookup_builtin(mod, TBI__NOT_TYPEABLE)) {
    return 0;
  }

  struct node *isalist = node->subs[IDX_ISALIST];
  e = earlytypepass(mod, isalist);
  EXCEPT(e);
  struct node *genargs = node->subs[IDX_GENARGS];
  e = earlytypepass(mod, genargs);
  EXCEPT(e);

  struct typ *mutable_typ = (struct typ *) node->typ;

  // FIXME: Check for duplicates?

  mutable_typ->isalist_count = isalist->subs_count;
  mutable_typ->isalist = calloc(isalist->subs_count, sizeof(*node->typ->isalist));
  mutable_typ->isalist_exported = calloc(isalist->subs_count, sizeof(bool));

  bool uses_implied_generic = FALSE;
  for (size_t n = 0; n < isalist->subs_count; ++n) {
    struct node *isa = isalist->subs[n];
    assert(isa->which == ISA);

    if (isa->typ->definition->which != DEFINTF) {
      e = mk_except_type(mod, isa, "not an intf");
      EXCEPT(e);
    }

    mutable_typ->isalist[n] = isa->typ;
    mutable_typ->isalist_exported[n] = isa->as.ISA.is_export;

    uses_implied_generic |= isa->typ->definition->as.DEFINTF.is_implied_generic;
  }

  if (node->which == DEFINTF) {
    node->as.DEFINTF.is_implied_generic = uses_implied_generic;
  }

  return 0;
}

static error step_type_gather_returns(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which == MODULE) {
    return 0;
  }

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    module_return_push(mod, node_fun_retval(node));
    break;
  default:
    break;
  }
  return 0;
}

static error step_type_gather_excepts(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which == MODULE) {
    return 0;
  }

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
  if (node->which != DEFNAME) {
    return 0;
  }

  return 0;
}

static error step_rewrite_sum_constructors(struct module *mod, struct node *node, void *user, bool *stop) {
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

  struct node *except[] = { fun, NULL };
  e = zero_to_first_for_generated(mod, mk_fun, except, node->scope);
  EXCEPT(e);

  return 0;
}

static error type_destruct(struct module *mod, struct node *node, const struct typ *constraint);

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
  [TDEREFDOT] = TBI_REF,
  [TDEREFBANG] = TBI_MREF,
  [TDEREFSHARP] = TBI_MMREF,
  [TNULREFDOT] = TBI_NREF,
  [TNULREFBANG] = TBI_NMREF,
  [TNULREFSHARP] = TBI_NMMREF,
};

static const struct typ *typ_ref(struct module *mod, enum token_type op, const struct typ *typ) {
  struct typ *t;
  t = typ_new(typ_lookup_builtin(mod, tbi_for_ref[op])->definition, TYPE_DEF, 1, 0);
  t->gen_args[1] = typ;
  return t;
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
    e = typ_can_deref(mod, node->subs[0],node->subs[0]->typ, node->as.UN.operator);
    EXCEPT(e);
    node->typ = node->subs[0]->typ;
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
    default:
      break;
    }
    break;
  case OP_BIN_SYM:
    switch (node->as.BIN.operator) {
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

static error bin_accessor_maybe_defchoice(struct scope **parent_scope,
                                          struct module *mod, struct node *parent) {
  if (parent->flags & NODE_IS_DEFCHOICE) {
    assert(parent->which == BIN);

    struct node *defchoice = NULL;
    error e = scope_lookup_ident_immediate(&defchoice, mod, parent->typ->definition->scope,
                                           node_ident(parent->subs[1]), FALSE);
    EXCEPT(e);
    assert(defchoice->which == DEFCHOICE);

    *parent_scope = defchoice->scope;
  }
  return 0;
}

static error rewrite_unary_call(struct module *mod, struct node *node, const struct typ *tfun) {
  struct scope *parent_scope = node->scope->parent;

  // It would generally be a bad idea to create a detached node, but there
  // is no natural place to attach it.
  struct node *fun = calloc(1, sizeof(struct node));
  memcpy(fun, node, sizeof(*fun));
  fun->typ = tfun;

  memset(node, 0, sizeof(*node));
  node->which = CALL;
  rew_append(node, fun);

  struct node *except[] = { fun, NULL };
  error e = zero_to_first_for_generated(mod, node, except, parent_scope);
  EXCEPT(e);
  return 0;
}

static error type_inference_bin_accessor(struct module *mod, struct node *node) {
  error e;
  struct node *parent = node->subs[0];
  struct scope *parent_scope = parent->typ->definition->scope;
  e = bin_accessor_maybe_ref(&parent_scope, mod, parent);
  EXCEPT(e);
  e = bin_accessor_maybe_defchoice(&parent_scope, mod, parent);
  EXCEPT(e);

  struct node *field = NULL;
  e = scope_lookup_ident_immediate(&field, mod, parent_scope, node_ident(node->subs[1]), FALSE);
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
  }
  assert(field->which != BIN || field->flags != 0);
  node->flags = field->flags;

  return 0;
}

static error type_inference_bin_rhs_u16(struct module *mod, struct node *node) {
  error e;
  e = typ_compatible_numeric(mod, node->subs[0], node->subs[0]->typ);
  EXCEPT(e);
  // FIXME handle the generic number case.
  e = type_destruct(mod, node->subs[1], typ_lookup_builtin(mod, TBI_U16));
  EXCEPT(e);
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
    return type_inference_bin_sym(mod, node);
  case OP_BIN_NUM_RHS_U16:
    return type_inference_bin_rhs_u16(mod, node);
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

static error type_inference_init(struct module *mod, struct node *node) {
  struct node *def = node->subs[0]->typ->definition;

  for (size_t n = 1; n < node->subs_count; n += 2) {
    struct node *field_name = node->subs[n];
    struct node *field = NULL;
    error e = scope_lookup_ident_immediate(&field, mod, def->scope,
                                           node_ident(field_name), FALSE);
    EXCEPT(e);
    e = type_destruct(mod, node->subs[n+1], field->typ);
    EXCEPT(e);
  }

  node->typ = node->subs[0]->typ;
  return 0;
}

static struct node *expr_ref(enum token_type ref_op, struct node *node) {
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
    return expr_ref(access, node);
  }
}

static error prepare_call_arguments(struct module *mod, struct node *node) {
  struct node *fun = node->subs[0];

  switch (fun->typ->definition->which) {
  case DEFFUN:
    if (node_fun_explicit_args_count(fun->typ->definition) != node->subs_count - 1) {
      error e = mk_except_call_args_count(mod, node, fun->typ->definition, 0,
                                          node->subs_count - 1);
      EXCEPT(e);
    }
    break;
  case DEFMETHOD:
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

      struct node *self = self_ref_if_value(
        mod, fun->typ->definition->as.DEFMETHOD.access, fun->subs[0]);
      rew_append(node, self);
      rew_insert_last_at(node, 1);

      struct node *except[] = { fun->subs[0], NULL };
      error e = zero_to_first_for_generated(mod, self, except, node->scope);
      EXCEPT(e);
      e = zero_to_first_for_generated(mod, m, NULL, node->scope);
      EXCEPT(e);
    }
    break;
  default:
    assert(FALSE);
  }

  return 0;
}

static bool is_instance_for(struct module *mod, struct node *i, struct node *node) {
  if (node->subs_count - 1 != i->typ->gen_arity) {
    return FALSE;
  }

  for (size_t n = 1; n < node->subs_count; ++n) {
    if (!typ_equal(mod, i->typ->gen_args[n], node->subs[n]->typ)) {
      return FALSE;
    }
  }

  return TRUE;
}

static error rewrite_instance_genargs(struct module *mod,
                                      struct node *instance, struct node *expr) {
  struct node *genargs = instance->subs[IDX_GENARGS];
  for (size_t n = 0; n < genargs->subs_count; ++n) {
    if (!(expr->subs[1+n]->flags & NODE_IS_TYPE)) {
      error e = mk_except_type(mod, expr->subs[1+n],
                               "generic type argument is not a type expression");
      EXCEPT(e);
    }

    struct node *ga = genargs->subs[n];
    ga->which = SETGENARG;
    // FIXME leaking ga->subs[1]
    ga->subs[1]->which = DIRECTDEF;
    assert(expr->subs[1+n]->typ->definition != NULL);
    ga->subs[1]->as.DIRECTDEF.definition = expr->subs[1+n]->typ->definition;

    // For the benefit of step_type_definitions
    ga->typ = expr->subs[1+n]->typ;
    ga->flags = NODE_IS_TYPE;
  }

  return 0;
}

static error type_inference_generic_instantiation(struct module *mod, struct node *node) {
  struct node *fun = node->subs[0];
  struct node *gendef = fun->typ->definition;

  struct node *genargs = gendef->subs[IDX_GENARGS];
  if (genargs->subs_count != node->subs_count - 1) {
    error e = mk_except_type(mod, node, "wrong number of generic type arguments\n");
    EXCEPT(e);
  }

  for (size_t n = 0; n < genargs->subs_count; ++n) {
    struct node *arg = node->subs[1+n];
    error e = typ_check_isa(mod, arg, arg->typ, genargs->subs[n]->typ);
    EXCEPT(e);
  }

  for (size_t n = 0; n < gendef->subs[IDX_GENARGS]->subs_count; ++n) {
    struct node *i = gendef->subs[IDX_GENARGS]->subs[n];
    if (is_instance_for(mod, i, node)) {
      node->typ = i->typ;
      return 0;
    }
  }

  struct node *pristine = node_toplevel(gendef)->instances[0];
  struct node *instance = add_instance_deepcopy_from_pristine(mod, gendef, pristine);
  node_toplevel(instance)->generic_definition = gendef;

  error e = rewrite_instance_genargs(mod, instance, node);
  EXCEPT(e);

  e = zero_to_second_for_generated(mod, instance, NULL, gendef->scope->parent);
  EXCEPT(e);

  if (instance->which == DEFTYPE) {
    for (size_t n = 0; n < instance->as.DEFTYPE.members_count; ++n) {
      e = zero_to_second_for_generated(mod, instance->as.DEFTYPE.members[n],
                                       NULL, instance->scope);
      EXCEPT(e);
    }
  }

  node->typ = instance->typ;

  return 0;
}

static error type_inference_call(struct module *mod, struct node *node) {
  struct node *fun = node->subs[0];

  if (fun->typ->which == TYPE_DEF) {
    if (fun->typ->definition->subs[IDX_GENARGS]->subs_count == 0) {
      error e = mk_except_type(mod, fun, "not a generic type");
      EXCEPT(e);
    }

    error e = type_inference_generic_instantiation(mod, node);
    EXCEPT(e);
    return 0;
  } else if (fun->typ->which == TYPE_FUNCTION
             && fun->typ->definition->subs[IDX_GENARGS]->subs_count > 0
             && node_toplevel_const(fun->typ->definition)->generic_definition == NULL) {
    // FIXME force explicit instantiation for now.
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

static error type_inference_try(struct module *mod, struct node *node) {
  node->typ = NULL;

  error e;
  struct try_excepts *t = &mod->trys[mod->trys_count - 1];

  if (t->count == 0) {
    e = mk_except(mod, node, "try block has no except statement, catch is unreachable");
    EXCEPT(e);
  }

  const struct typ *u = t->excepts[0]->typ;
  for (size_t n = 1; n < t->count; ++n) {
    struct node *exc = t->excepts[n];
    e = typ_unify(&u, mod, exc, u, exc->typ);
    EXCEPT(e);
  }

  e = type_destruct(mod, node->subs[1], u);
  EXCEPT(e);

  module_excepts_close_try(mod);
  return 0;
}

static error type_destruct_import_path(struct module *mod, struct node *node) {
  struct node *def = NULL;
  error e = scope_lookup(&def, mod, mod->gctx->modules_root.scope, node);
  EXCEPT(e);

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

static error type_destruct(struct module *mod, struct node *node, const struct typ *constraint) {
  error e;
  struct node *def = NULL;

  assert(node->typ != typ_lookup_builtin(mod, TBI__NOT_TYPEABLE));

  if (node->typ != NULL
      && node->typ != typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT)
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
    e = typ_unify(&node->typ, mod, node, typ_lookup_builtin(mod, TBI_LITERALS_INTEGER), constraint);
    EXCEPT(e);
    break;
  case BOOL:
    e = typ_unify(&node->typ, mod, node, typ_lookup_builtin(mod, TBI_LITERALS_BOOLEAN), constraint);
    EXCEPT(e);
    break;
  case STRING:
    e = typ_unify(&node->typ, mod, node, typ_lookup_builtin(mod, TBI_STRING), constraint);
    EXCEPT(e);
    break;
  case IDENT:
    // FIXME make sure ident not used before definition.
    // FIXME let x, (y, z) = i32, (i32, i32)
    // In this case, y (for instance), will not have NODE_IS_TYPE set properly.
    // NODE_IS_TYPE needs to be set recursively when descending via
    // type_destruct.
    e = scope_lookup_ident_wontimport(&def, mod, node->scope, node_ident(node), FALSE);
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
      e = typ_compatible_reference(mod, node, node->as.UN.operator, constraint);
      break;
    case OP_UN_DEREF:
      e = 0;
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
      return 0;
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
    case OP_BIN_NUM_RHS_U16:
      e = typ_compatible_numeric(mod, node, left_constraint);
      EXCEPT(e);
      right_constraint = typ_lookup_builtin(mod, TBI_U16);
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
    case OP_BIN_NUM_RHS_U16:
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
    e = type_inference_init(mod, node);
    EXCEPT(e);
    e = typ_unify(&node->typ, mod, node, node->typ, constraint);
    EXCEPT(e);
    break;
  case CALL:
    e = type_inference_call(mod, node);
    EXCEPT(e);
    e = typ_unify(&node->typ, mod, node, node->typ, constraint);
    EXCEPT(e);
    break;
  default:
    assert(FALSE);
  }

  assert(node->typ != typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT));
  assert(node->typ != NULL);
  return 0;
}

static struct typ *genarg_mark_as_uninstantiated(const struct typ *t) {
  assert(t->definition->which == DEFINTF);
  struct typ *r = calloc(1, sizeof(struct typ));
  memcpy(r, t, sizeof(*r));
  r->is_uninstantiated_genarg = TRUE;
  return r;
}

static error step_type_inference(struct module *mod, struct node *node, void *user, bool *stop) {
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
    if (def->typ->which == TYPE_FUNCTION
        && node->typ != typ_lookup_builtin(mod, TBI__CALL_FUNCTION_SLOT)) {
      if (node_fun_explicit_args_count(def) != 0) {
        e = mk_except_call_args_count(mod, node, def, 0, 0);
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
    e = scope_lookup(&def, mod, mod->gctx->modules_root.scope, node->subs[0]);
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
    node->typ = typ_lookup_builtin(mod, TBI_LITERALS_INTEGER);
    goto ok;
  case BOOL:
    node->typ = typ_lookup_builtin(mod, TBI_LITERALS_BOOLEAN);
    goto ok;
  case STRING:
    node->typ = typ_lookup_builtin(mod, TBI_STRING);
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
    e = type_inference_init(mod, node);
    EXCEPT(e);
    goto ok;
  case RETURN:
    if (node->subs_count > 0) {
      e = type_destruct(mod, node->subs[0], module_return_get(mod)->typ);
      EXCEPT(e);
      node->typ = node->subs[0]->typ;
    } else {
      node->typ = typ_lookup_builtin(mod, TBI_VOID);
    }
    goto ok;
  case EXCEP:
  case BLOCK:
  case BREAK:
  case CONTINUE:
  case PASS:
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    goto ok;
  case FOR:
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    struct node *it = node->subs[IDX_FOR_IT]->subs[IDX_FOR_IT_DEFP];
    e = typ_check_isa(mod, it, it->typ,
                      typ_lookup_builtin(mod, TBI_ITERATOR));
    EXCEPT(e);
    goto ok;
  case WHILE:
  case IF:
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    for (size_t n = 0; n < node->subs_count-1; n += 2) {
      e = typ_compatible(mod, node->subs[n], node->subs[n]->typ, typ_lookup_builtin(mod, TBI_BOOL));
      EXCEPT(e);
    }
    goto ok;
  case MATCH:
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    for (size_t n = 1; n < node->subs_count; n += 2) {
      e = type_destruct(mod, node->subs[n], node->subs[0]->typ);
      EXCEPT(e);
    }
    goto ok;
  case TRY:
    e = type_inference_try(mod, node);
    EXCEPT(e);
    goto ok;
  case SETGENARG:
    node->typ = node->subs[1]->typ;
    e = type_destruct(mod, node->subs[0], node->typ);
    EXCEPT(e);
    goto ok;
  case DEFARG:
  case TYPECONSTRAINT:
    node->typ = node->subs[1]->typ;
    e = type_destruct(mod, node->subs[0], node->typ);
    EXCEPT(e);
    goto ok;
  case DEFGENARG:
    node->typ = genarg_mark_as_uninstantiated(node->subs[1]->typ);
    e = type_destruct(mod, node->subs[0], node->typ);
    EXCEPT(e);
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
    module_return_pop(mod);

    goto ok;
  case DEFINTF:
    goto ok;
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

          ch->typ = node->typ;
        }
      }
      break;
    default:
      break;
    }
    goto ok;
  case DEFPATTERN: {
    bool has_expr = node->subs_count > 1 && node->subs[1]->which != DEFNAME;
    if (has_expr) {
      e = type_destruct(mod, node->subs[0], node->subs[1]->typ);
      EXCEPT(e);
    }
    node->typ = node->subs[0]->typ;
    if (has_expr) {
      node->flags = node->subs[1]->flags;
      for (size_t n = 0; n < node->subs_count; ++n) {
        if (node->subs[n]->which == DEFNAME) {
          node->subs[n]->flags |= (node->subs[n]->as.DEFNAME.expr->flags & NODE__TRANSITIVE);
        }
      }
    }
    goto ok;
  }
  case DEFFIELD:
    node->typ = node->subs[1]->typ;
    goto ok;
  case LET:
  case DELEGATE:
  case PRE:
  case POST:
  case INVARIANT:
  case EXAMPLE:
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
  default:
    goto ok;
  }

ok:
  assert(node->typ != typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT));
  assert(node->typ != NULL);
  return 0;
}

static error step_toplevel_secondpass(struct module *mod, struct node *node, void *user, bool *stop) {
  const struct node *parent = node->scope->parent->node;
  if (parent->which != MODULE_BODY) {
    return 0;
  }

  error e = secondpass(mod, node, NULL);
  EXCEPT(e);

  return 0;
}

static struct node *get_member(struct module *mod, struct node *node, ident id) {
  assert(node->which == DEFTYPE || node->which == DEFCHOICE);
  struct node *m = NULL;
  (void)scope_lookup_ident_immediate(&m, mod, node->scope, id, TRUE);
  return m;
}

static void define_builtin(struct module *mod, struct node *tdef,
                           enum builtingen bg) {
  struct node *modbody = tdef->scope->parent->node;
  size_t insert_pos = rew_find_subnode_in_parent(modbody, tdef) + 1;

  struct node *proto = NULL;
  error e = scope_lookup_abspath(&proto, mod, builtingen_abspath[bg]);
  assert(!e);

  struct node *existing = get_member(mod, tdef, node_ident(proto));
  if (existing != NULL) {
    return;
  }

  struct node *d = node_new_subnode(mod, modbody);
  node_deepcopy(mod, d, proto);
  mk_expr_abspath(mod, d, builtingen_abspath[bg]);
  rew_move_last_over(d, 0, FALSE);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->scope_name = node_ident(tdef);
  toplevel->builtingen = bg;
  toplevel->is_export = node_toplevel(tdef)->is_export;
  toplevel->is_inline = node_toplevel(tdef)->is_inline;

  rew_insert_last_at(modbody, insert_pos);
  e = zero_to_forward_for_generated(mod, d, tdef->scope);
  assert(!e);

  node_toplevel(d)->is_prototype = FALSE;
}

static void define_defchoice_builtin(struct module *mod, struct node *ch,
                                     enum builtingen bg, enum node_which which) {
  struct node *tdef = ch->scope->parent->node;

  struct node *proto = NULL;
  error e = scope_lookup_abspath(&proto, mod, builtingen_abspath[bg]);
  assert(!e);

  struct node *d = mk_node(mod, ch, which);
  node_deepcopy(mod, d, proto);
  mk_expr_abspath(mod, d, builtingen_abspath[bg]);
  rew_move_last_over(d, 0, FALSE);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->scope_name = node_ident(ch);
  toplevel->builtingen = bg;
  toplevel->is_export = node_toplevel(tdef)->is_export;
  toplevel->is_inline = node_toplevel(tdef)->is_inline;

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

  e = zero_to_first_for_generated(mod, d, NULL, ch->scope);
  assert(!e);
  node_toplevel(d)->is_prototype = FALSE;
}

static error step_add_builtin_defchoice_constructors(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which != DEFCHOICE) {
    return 0;
  }

  const struct node *tdef = node->scope->parent->node;
  if (tdef->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    return 0;
  }

  const struct typ *targ = node->subs[IDX_CH_PAYLOAD]->typ;
  bool has_ctor = FALSE;
  if (typ_isa(mod, targ, typ_lookup_builtin(mod, TBI_COPYABLE))) {
    has_ctor |= TRUE;

    define_defchoice_builtin(
      mod, node, BG_SUM_CTOR_WITH_CTOR, DEFMETHOD);
  }

  if (typ_isa(mod, targ, typ_lookup_builtin(mod, TBI_DEFAULT_CTOR))) {
    has_ctor |= TRUE;

    define_defchoice_builtin(
      mod, node, BG_DEFAULT_CTOR_CTOR, DEFMETHOD);
  }

  if (!has_ctor) {
    error e = mk_except(mod, node, "cannot use type in sum type, neither Copyable nor DefaultCtor");
    EXCEPT(e);
  }

  return 0;
}

static void add_isa(struct module *mod, struct node *tdef, const char *path) {
  struct node *isalist = tdef->subs[IDX_ISALIST];
  assert(isalist->which == ISALIST);
  struct node *isa = mk_node(mod, isalist, ISA);
  isa->as.ISA.is_export = node_toplevel(tdef)->is_export;
  mk_expr_abspath(mod, isa, path);
  error e = zero_to_first_for_generated(mod, isa, NULL, isalist->scope);
  assert(!e);
}

static error step_add_builtin_detect_ctor_intf(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which != DEFTYPE) {
    return 0;
  }
  if (node_toplevel_const(node)->is_extern) {
    return 0;
  }
  if (node->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    return 0;
  }

  struct node *proxy = node;
  if (node->as.DEFTYPE.kind == DEFTYPE_SUM) {
    // Find first DEFCHOICE with an non-void argument type.
    for (size_t n = 0; n < node->subs_count; ++n) {
      struct node *ch = node->subs[n];
      if (ch->which == DEFCHOICE
          && ch->subs[IDX_CH_PAYLOAD]->typ != typ_lookup_builtin(mod, TBI_VOID)) {
        proxy = ch;
        break;
      }
    }
  }

  struct node *ctor = get_member(mod, proxy, ID_CTOR);
  if (ctor != NULL) {
    if (node_fun_explicit_args_count(ctor) == 0) {
      add_isa(mod, node, "nlang.builtins.DefaultCtor");
    } else if (node_fun_explicit_args_count(ctor) == 1) {
      add_isa(mod, node, "nlang.builtins.CtorWith");
    }
  } else {
    bool zero = TRUE;
    for (size_t n = 0; n < node->subs_count; ++n) {
      struct node *f = node->subs[n];
      if (f->which == DEFFIELD
          && !typ_isa(mod, f->typ, typ_lookup_builtin(mod, TBI_TRIVIAL_CTOR))) {
        zero = FALSE;
      }
    }

    if (zero) {
      add_isa(mod, node, "nlang.builtins.TrivialCtor");
    }
  }

  return 0;
}

static error step_add_builtin_ctor(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which != DEFTYPE) {
    return 0;
  }
  if (node_toplevel_const(node)->is_extern) {
    return 0;
  }

  return 0;
}

static error step_add_builtin_dtor(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which != DEFTYPE) {
    return 0;
  }
  if (node_toplevel_const(node)->is_extern) {
    return 0;
  }

  return 0;
}

static error step_add_builtin_mk_new(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which != DEFTYPE) {
    return 0;
  }
  if (node_toplevel_const(node)->is_extern) {
    return 0;
  }
  if (node->as.DEFTYPE.kind == DEFTYPE_SUM) {
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
  }

  return 0;
}

static error step_add_builtin_defchoice_mk_new(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which != DEFCHOICE) {
    return 0;
  }

  struct node *tdef = node->scope->parent->node;
  assert(tdef->which == DEFTYPE);
  if (tdef->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    define_defchoice_builtin(mod, node, BG_DEFAULT_CTOR_MK, DEFFUN);
    define_defchoice_builtin(mod, node, BG_DEFAULT_CTOR_NEW, DEFFUN);
  } else if (tdef->as.DEFTYPE.kind == DEFTYPE_SUM) {
    define_defchoice_builtin(mod, node, BG_SUM_CTOR_WITH_MK, DEFFUN);
    define_defchoice_builtin(mod, node, BG_SUM_CTOR_WITH_NEW, DEFFUN);
  }

  return 0;
}

static error step_add_builtin_operators(struct module *mod, struct node *node, void *user, bool *stop) {
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

static error step_implement_trivials(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which != DEFTYPE) {
    return 0;
  }

  // FIXME: We should check that the fields/defchoice do indeed support
  // these trivial interfaces. It must be safe to declare them.
  // Same thing for trivial ctor, dtor.

  if (typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_TRIVIAL_COPY))) {
    define_builtin(mod, node, BG_TRIVIAL_COPY_OPERATOR_COPY);
  } else if (typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_TRIVIAL_EQUALITY))) {
    define_builtin(mod, node, BG_TRIVIAL_EQUALITY_OPERATOR_EQ);
    define_builtin(mod, node, BG_TRIVIAL_EQUALITY_OPERATOR_NE);
  }

  return 0;
}

static void define_dispatch(struct module *mod, struct node *tdef, const struct typ *tintf) {
  struct node *intf = tintf->definition;

  struct node *modbody = tdef->scope->parent->node;
  size_t insert_pos = rew_find_subnode_in_parent(modbody, tdef) + 1;

  for (size_t n = 0; n < intf->subs_count; ++n) {
    struct node *proto = intf->subs[n];
    if (proto->which != DEFMETHOD) {
      continue;
    }

    struct node *existing = get_member(mod, tdef, node_ident(proto));
    if (existing != NULL) {
      return;
    }

    struct node *d = mk_node(mod, modbody, DEFMETHOD);
    node_deepcopy(mod, d, proto);
    char *abspath = scope_name(mod, proto->scope);
    mk_expr_abspath(mod, d, abspath);
    rew_move_last_over(d, 0, FALSE);

    struct toplevel *toplevel = node_toplevel(d);
    toplevel->scope_name = node_ident(tdef);
    toplevel->builtingen = BG_SUM_DISPATCH;
    toplevel->is_export = node_toplevel(tdef)->is_export;
    toplevel->is_inline = node_toplevel(tdef)->is_inline;

    rew_insert_last_at(modbody, insert_pos);

    error e = zero_to_forward_for_generated(mod, d, tdef->scope);
    assert(!e);
    node_toplevel(d)->is_prototype = FALSE;
  }
}

static error step_add_sum_dispatch(struct module *mod, struct node *node, void *user, bool *stop) {
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

  for (size_t n = 0; n < node->typ->isalist_count; ++n) {
    assert(node->subs[IDX_ISALIST]->which == ISALIST);
    assert(node->subs[IDX_ISALIST]->subs[n]->which == ISA);
    if (!node->subs[IDX_ISALIST]->subs[n]->as.ISA.is_explicit) {
      continue;
    }

    const struct typ *intf = node->typ->isalist[n];
    const struct typ *check_intf = intf;
    if (typ_equal(mod, intf, typ_lookup_builtin(mod, TBI_SUM_COPY))) {
      check_intf = typ_lookup_builtin(mod, TBI_COPYABLE);
    } else if (typ_equal(mod, intf, typ_lookup_builtin(mod, TBI_SUM_EQUALITY))) {
      check_intf = typ_lookup_builtin(mod, TBI_HAS_EQUALITY);
    } else if (typ_equal(mod, intf, typ_lookup_builtin(mod, TBI_SUM_ORDER))) {
      check_intf = typ_lookup_builtin(mod, TBI_ORDERED);
    } else {
      assert(intf->definition->which == DEFINTF);
      if (intf->definition->as.DEFINTF.is_implied_generic) {
        error e = mk_except_type(mod, node->subs[IDX_ISALIST]->subs[n],
                                 "intf is an implied generic (uses 'this') and cannot be dispatched over");
        EXCEPT(e);
      }
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
  if (node->which != DEFFUN && node->which != DEFMETHOD) {
    return 0;
  }

  const struct node *retval = node_fun_retval(node);
  if (typ_equal(mod, retval->typ, typ_lookup_builtin(mod, TBI_VOID))) {
    return 0;
  }
  if (typ_isa(mod, retval->typ, typ_lookup_builtin(mod, TBI_RETURN_BY_COPY))) {
    return 0;
  }

  error e = mk_except(mod, retval, "Return through ref not yet supported\n");
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
  [TPLUS] = ID_OPERATOR_PLUS,
  [TMINUS] = ID_OPERATOR_MINUS,
  [TDIVIDE] = ID_OPERATOR_DIVIDE,
  [TMODULO] = ID_OPERATOR_MODULO,
  [TTIMES] = ID_OPERATOR_TIMES,
  [TUMINUS] = ID_OPERATOR_UMINUS,
  [TBWNOT] = ID_OPERATOR_BWNOT,
};

static error step_operator_call_inference(struct module *mod, struct node *node, void *user, bool *stop) {
  enum token_type op;
  switch (node->which) {
  case UN:
    op = node->as.UN.operator;
    break;
  case BIN:
    op = node->as.BIN.operator;
    break;
  default:
    return 0;
  }

  switch (OP_KIND(op)) {
  case OP_UN_BOOL:
  case OP_UN_NUM:
  case OP_BIN:
  case OP_BIN_SYM:
  case OP_BIN_SYM_BOOL:
  case OP_BIN_SYM_NUM:
  case OP_BIN_NUM_RHS_U16:
    break;
  default:
    return 0;
  }

  struct node *left = node->subs[0];
  if (typ_isa(mod, left->typ, typ_lookup_builtin(mod, TBI_NATIVE_INTEGER))
      || typ_isa(mod, left->typ, typ_lookup_builtin(mod, TBI_NATIVE_BOOLEAN))) {
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

  node->which = CALL;
  struct node *fun = mk_node(mod, node, BIN);
  fun->as.BIN.operator = TDOT;
  struct node *base = mk_node(mod, fun, DIRECTDEF);
  base->as.DIRECTDEF.definition = node->subs[0]->typ->definition;
  struct node *member = mk_node(mod, fun, IDENT);
  member->as.IDENT.name = operator_ident[op];

  struct node *except[] = { node->subs[0], node->subs[1], NULL };

  rew_insert_last_at(node, 0);
  node->subs[1] = expr_ref(TREFDOT, node->subs[1]);
  node->subs[2] = expr_ref(TREFDOT, node->subs[2]);

  error e = zero_to_second_for_generated(mod, node, except, saved_parent);
  EXCEPT(e);

  return 0;
}

static error step_ctor_call_inference(struct module *mod, struct node *node, void *user, bool *stop) {
  return 0;
}

static error step_dtor_call_inference(struct module *mod, struct node *node, void *user, bool *stop) {
  return 0;
}

static error step_copy_call_inference(struct module *mod, struct node *node, void *user, bool *stop) {
  return 0;
}

struct temporaries {
  struct node **rvalues;
  size_t count;
};

static error step_gather_temporary_rvalues(struct module *mod, struct node *node, void *user, bool *stop) {
  struct temporaries *temps = user;

  switch (node->which) {
  case UN:
    if (OP_KIND(node->as.UN.operator) == OP_UN_REFOF
        && node_is_rvalue(node->subs[0])) {
      temps->count += 1;
      temps->rvalues = realloc(temps->rvalues, temps->count * sizeof(*temps->rvalues));
      temps->rvalues[temps->count - 1] = node->subs[0];
    }
    break;
  case BLOCK:
    *stop = TRUE;
    break;
  default:
    return 0;
  }

  return 0;
}

static error step_define_temporary_rvalues(struct module *mod, struct node *node, void *user, bool *stop) {
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

  struct temporaries temps;
  memset(&temps, 0, sizeof(temps));

  error e = pass(mod, node, temprvalue_down, temprvalue_up, NULL, &temps);
  EXCEPT(e);

  struct node *parent = node->scope->parent->node;
  size_t where = rew_find_subnode_in_parent(parent, node);
  for (size_t n = 0; n < temps.count; ++n) {
    struct node *rval = temps.rvalues[n];
    struct node *rval_parent = rval->scope->parent->node;
    const size_t rval_where = rew_find_subnode_in_parent(rval_parent, rval);

    struct node *let = mk_node(mod, parent, LET);
    struct node *defp = mk_node(mod, let, DEFPATTERN);
    struct node *tmpvar = mk_node(mod, defp, IDENT);
    tmpvar->as.IDENT.name = gensym(mod);
    rew_append(defp, rval);

    struct node *except[] = { rval, NULL };
    e = zero_to_first_for_generated(mod, let, except, node->scope);
    EXCEPT(e);

    struct node *rval_replacement = mk_node(mod, rval_parent, IDENT);
    node_deepcopy(mod, rval_replacement, tmpvar);
    rew_move_last_over(rval_parent, rval_where, TRUE);

    e = zero_to_first_for_generated(mod, rval_replacement, NULL, node->scope);
    EXCEPT(e);

    rew_insert_last_at(parent, where);
    where += 1;
  }

  free(temps.rvalues);

  return 0;
}

static const step zeropass_down[] = {
  step_generics_pristine_copy,
  step_detect_prototypes,
  step_detect_generic_interfaces_down,
  step_detect_deftype_kind,
  step_assign_deftype_which_values,
  step_add_builtin_members,
  step_add_codegen_variables,
  step_defpattern_extract_defname,
  NULL,
};

static const step zeropass_up[] = {
  step_add_scopes,
  step_detect_generic_interfaces_up,
  NULL,
};

error zeropass(struct module *mod, struct node *node, struct node **except) {
  error e = pass(mod, node, zeropass_down, zeropass_up, except, NULL);
  EXCEPT(e);

  return 0;
}

// The "forward" pass is necessary to handle forward declarations.
// By running step_lexical_scoping over the whole module first, we ensure
// (full) definitions take over forward declarations.
//
// The two "up" passes happen to make sure that the (full) definitions are
// given (i) a typ, (ii) their isalist is typ'd, so that code that in
// principle can only see the forward declaration, can use the typ and the
// isalist typs (which is what a forward declaration is for).
//
// FIXME: We're not checking if the forward declaration isalist is a subset
// of the definition isalist.
static const step forwardpass_down[] = {
  step_stop_submodules,
  step_lexical_scoping,
  NULL,
};

static const step forwardpass_up[] = {
  step_type_definitions,
  NULL,
};

error forwardpass(struct module *mod, struct node *node, struct node **except) {
  int module_depth = 0;
  error e = pass(mod, node, forwardpass_down, forwardpass_up, except, &module_depth);
  EXCEPT(e);

  return 0;
}

static const step firstpass_down[] = {
  step_stop_submodules,
  step_stop_marker_tbi,
  step_type_inference_isalist,
  step_type_destruct_mark,
  step_type_gather_returns,
  step_type_gather_excepts,
  NULL,
};

static const step firstpass_up[] = {
  step_add_builtin_defchoice_constructors,
  step_add_builtin_detect_ctor_intf,
  step_rewrite_defname_no_expr,
  step_rewrite_sum_constructors,
  step_type_inference,
  step_toplevel_secondpass,
  NULL,
};

error firstpass(struct module *mod, struct node *node, struct node **except) {
  int module_depth = 0;
  error e = pass(mod, node, firstpass_down, firstpass_up, except, &module_depth);
  EXCEPT(e);

  return 0;
}

static const step secondpass_down[] = {
  step_add_builtin_ctor,
  step_add_builtin_dtor,
  step_add_builtin_defchoice_mk_new,
  step_add_builtin_mk_new,
  step_add_builtin_operators,
  step_add_sum_dispatch,
  step_implement_trivials,
  step_rewrite_def_return_through_ref,
  NULL,
};

static const step secondpass_up[] = {
  step_operator_call_inference,
  step_ctor_call_inference,
  step_dtor_call_inference,
  step_copy_call_inference,
  step_define_temporary_rvalues,
  NULL,
};

error secondpass(struct module *mod, struct node *node, struct node **except) {
  error e = pass(mod, node, secondpass_down, secondpass_up, except, NULL);
  EXCEPT(e);

  return 0;
}

static error zero_to_forward_for_generated(struct module *mod, struct node *node,
                                           struct scope *parent_scope) {
  error e = zeropass(mod, node, NULL);
  EXCEPT(e);
  node->scope->parent = parent_scope;

  e = forwardpass(mod, node, NULL);
  EXCEPT(e);

  return 0;
}

static error zero_to_first_for_generated(struct module *mod, struct node *node,
                                         struct node **except,
                                         struct scope *parent_scope) {
  error e = zeropass(mod, node, except);
  EXCEPT(e);
  node->scope->parent = parent_scope;

  e = forwardpass(mod, node, except);
  EXCEPT(e);

  e = firstpass(mod, node, except);
  EXCEPT(e);

  return 0;
}

static error zero_to_second_for_generated(struct module *mod, struct node *node,
                                          struct node **except,
                                          struct scope *parent_scope) {
  error e = zeropass(mod, node, except);
  EXCEPT(e);
  node->scope->parent = parent_scope;

  e = forwardpass(mod, node, except);
  EXCEPT(e);

  e = firstpass(mod, node, except);
  EXCEPT(e);

  e = secondpass(mod, node, except);
  EXCEPT(e);

  return 0;
}
