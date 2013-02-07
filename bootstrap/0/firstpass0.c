#include "firstpass.h"

error pass(struct module *mod, struct node *root, const step *down_steps, const step *up_steps,
           void *user) {
  error e;
  if (root == NULL) {
    root = mod->root;
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
    e = pass(mod, node, down_steps, up_steps, user);
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

static void insert_last_at(struct node *node, size_t pos) {
  struct node *tmp = node->subs[pos];
  node->subs[pos] = node->subs[node->subs_count - 1];
  for (size_t n = pos + 1; n < node->subs_count - 1; ++n) {
    struct node *tmptmp = node->subs[n];
    node->subs[n] = tmp;
    tmp = tmptmp;
  }
  node->subs[node->subs_count - 1] = tmp;
}

static void move_last_over(struct node *node, size_t pos) {
  struct node *would_leak = node->subs[pos];
  assert(would_leak->which == IDENT);
  node->subs[pos] = node->subs[node->subs_count - 1];
  node->subs_count -= 1;
  node->subs = realloc(node->subs, node->subs_count * sizeof(node->subs[0]));
}

// Must be run before builtins are added.
error step_detect_deftype_kind(struct module *mod, struct node *node, void *user, bool *stop) {
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
      if ((!f->as.DEFCHOICE.has_value && f->subs_count == 2)
          || (f->as.DEFCHOICE.has_value && f->subs_count == 3)) {
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

error step_assign_deftype_which_values(struct module *mod, struct node *node, void *user, bool *stop) {
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
      node_deepcopy(mod, left, prev->subs[1]);
      struct node *right = mk_node(mod, val, NUMBER);
      right->as.NUMBER.value = "1";
    }

    insert_last_at(d, 1);

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
      struct token tok;
      tok.t = TIDENT;
      tok.value = path;
      tok.len = len - i;
      ident id = idents_add(mod->gctx, &tok);

      struct node *root = mk_node(mod, node, DIRECTDEF);
      root->as.DIRECTDEF.definition = &mod->gctx->modules_root;
      struct node *name = mk_node(mod, node, IDENT);
      name->as.IDENT.name = id;

      break;
    } else if (path[i] == '.') {
      assert(len - i > 1);
      struct token tok;
      tok.t = TIDENT;
      tok.value = path + i + 1;
      tok.len = len - i - 1;
      ident id = idents_add(mod->gctx, &tok);

      struct node *down = mk_node(mod, node, BIN);
      struct node *name = mk_node(mod, node, IDENT);
      name->as.IDENT.name = id;

      do_mk_expr_abspath(mod, down, path, i);
      break;
    }
  }
}

struct node *mk_expr_abspath(struct module *mod, struct node *node, const char *path) {
  struct node *n = mk_node(mod, node, BIN);
  do_mk_expr_abspath(mod, n, path, strlen(path));
  return n;
}

error step_extend_deftype_builtin_isalist(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which != DEFTYPE) {
    return 0;
  }

  struct node *isalist = NULL;
  struct node *isa = NULL;

  switch (node->as.DEFTYPE.kind) {
  case DEFTYPE_PROTOTYPE:
    break;
  case DEFTYPE_STRUCT:
    break;
  case DEFTYPE_SUM:
    isalist = node->subs[2];
    isa = mk_node(mod, isalist, ISA);
    isa->as.ISA.toplevel.is_export = TRUE;
    mk_expr_abspath(mod, isa, "nlang.builtins.BuiltinSum");
    break;
  case DEFTYPE_ENUM:
    isalist = node->subs[2];
    isa = mk_node(mod, isalist, ISA);
    isa->as.ISA.toplevel.is_export = TRUE;
    mk_expr_abspath(mod, isa, "nlang.builtins.BuiltinEnum");
    break;
  }

  return 0;
}

error step_add_builtin_members(struct module *mod, struct node *node, void *user, bool *stop) {
  switch (node->which) {
  case DEFTYPE:
  case DEFINTF:
    break;
  default:
    return 0;
  }

  struct node *let = mk_node(mod, node, LET);
  struct node *defn = mk_node(mod, let, DEFNAME);
  struct node *name = mk_node(mod, defn, IDENT);
  name->as.IDENT.name = ID_THIS;
  struct node *expr = mk_node(mod, defn, IDENT);
  expr->as.IDENT.name = node_ident(node);

  insert_last_at(node, 2);

  return 0;
}

error step_add_builtin_constructors(struct module *mod, struct node *node, void *user, bool *stop) {
  switch (node->which) {
  case DEFTYPE:
    break;
  default:
    return 0;
  }

  return 0;
}

error step_add_codegen_variables(struct module *mod, struct node *node, void *user, bool *stop) {
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

error step_add_scopes(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->scope == NULL) {
    node->scope = scope_new(node);
  } else {
    assert(node->which == MODULE);
  }

  if (node->which != MODULE) {
    // In later passes, we may rewrite nodes that have been marked, we must
    // erase the marking. But not for module nodes that are:
    //   (i) never rewritten,
    //   (ii) are typed void when created, to allow global module lookup to
    //   use the 'typ' field when typing import nodes.
    node->typ = NULL;
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *s = node->subs[n];
    if (s->scope != NULL) {
      s->scope->parent = node->scope;
    }
  }

  return 0;
}

error step_stop_submodules(struct module *mod, struct node *node, void *user, bool *stop) {
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
  e = scope_lookup_ident_noimport(&n, mod, *scope, i->as.IDENT.name, TRUE);
  if (e == EINVAL) {
    n = import_path;
    e = scope_define_ident(mod, *scope, i->as.IDENT.name, n);
    EXCEPT(e);
  } else if (e) {
    // Repeat bound-to-fail lookup to get the error message right.
    e = scope_lookup_ident_noimport(&n, mod, *scope, i->as.IDENT.name, FALSE);
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
    if (toplevel == NULL || !toplevel->is_export || toplevel->scope_name != ID__NONE) {
      continue;
    }

    if (ex->which == IMPORT) {
      e = lexical_import(scope, target_mod, ex);
      EXCEPT(e);

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

    e = pass(mod, imported, down, up, NULL);
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

error step_lexical_scoping(struct module *mod, struct node *node, void *user, bool *stop) {
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
  case FOR:
    id = node->subs[0];
    sc = node->scope;
    break;
  case DEFFUN:
  case DEFMETHOD:
    if (node->subs[0]->which == IDENT) {
      id = node->subs[0];
    } else {
      id = node->subs[0]->subs[1];
    }
    toplevel = node_toplevel_const(node);
    if (toplevel->scope_name == 0) {
      sc = node->scope->parent;
    } else {
      e = scope_lookup_ident_noimport(&container, mod, node->scope->parent,
                                      toplevel->scope_name,
                                      FALSE);
      EXCEPT(e);
      sc = container->scope;
      node->scope->parent = sc;
    }
    break;
  case DEFTYPE:
  case DEFINTF:
  case DEFFIELD:
  case DEFCHOICE:
    id = node->subs[0];
    sc = node->scope->parent;
    break;
  case DEFNAME:
    id = node->subs[0];
    sc = node->scope->parent->parent;
    break;
  case TRY:
    id = node->subs[1];
    sc = node->scope;
    break;
  default:
    return 0;
  }

  e = scope_define(mod, sc, id, node);
  EXCEPT(e);

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    {
      for (size_t n = 0; n < node_fun_args_count(node); ++n) {
        assert(node->subs[n+1]->which == TYPECONSTRAINT);
        e = scope_define(mod, node->scope, node->subs[n+1]->subs[0],
                         node->subs[n+1]);
        EXCEPT(e);
      }
    }
    break;
  default:
    break;
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
  struct typ *pending = typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT);
  struct typ *not_typeable = typ_lookup_builtin(mod, TBI__NOT_TYPEABLE);
  if (node->typ == pending || node->typ == not_typeable) {
    mark_subs(mod, node, node->typ, 0, node->subs_count, 1);
  }
}

error step_type_destruct_mark(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which == MODULE) {
    return 0;
  }

  inherit(mod, node);

  struct typ *pending = typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT);
  struct typ *not_typeable = typ_lookup_builtin(mod, TBI__NOT_TYPEABLE);

  switch (node->which) {
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
    mark_subs(mod, node, pending, 0, node->subs_count, 1);
    break;
  case DEFFUN:
  case DEFMETHOD:
    // The BIN case implies that's this method comes from an intf
    // and we will type the full path.
    if (node->subs[0]->which == IDENT) {
      node->subs[0]->typ = not_typeable;
    }
    break;
  case DEFTYPE:
  case DEFINTF:
  case DEFFIELD:
  case DEFNAME:
    node->subs[0]->typ = not_typeable;
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

error step_type_definitions(struct module *mod, struct node *node, void *user, bool *stop) {
  switch (node->which) {
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
    node->typ = mod->gctx->builtin_typs_by_name[id];
    node->typ->definition = node;
  } else {
    node->typ = typ_new(node, TYPE_DEF, 0, 0);
  }
  node->is_type = TRUE;

  return 0;
}

static struct node *node_fun_retval(struct node *def) {
  assert(def->which == DEFFUN || def->which == DEFMETHOD);
  if (def->subs[def->subs_count-1]->which == BLOCK) {
    return def->subs[def->subs_count-2];
  } else {
    return def->subs[def->subs_count-1];
  }
}

error step_type_gather_returns(struct module *mod, struct node *node, void *user, bool *stop) {
  if (node->which == MODULE) {
    return 0;
  }

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    module_return_set(mod, node_fun_retval(node));
    break;
  default:
    break;
  }
  return 0;
}

error step_type_gather_excepts(struct module *mod, struct node *node, void *user, bool *stop) {
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

static error type_destruct(struct module *mod, struct node *node, struct typ *constraint);

static error type_inference_unary_call(struct module *mod, struct node *node, struct node *def) {
  if (node_fun_args_count(def) != 0) {
    error e = mk_except_call_arg_count(mod, node, def, 0);
    EXCEPT(e);
  }

  switch (def->which) {
  case DEFFUN:
  case DEFMETHOD:
    node->typ = node_fun_retval(def)->typ;
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
  [TNULREFDOT] = TBI_NREF,
  [TNULREFBANG] = TBI_NMREF,
  [TNULREFSHARP] = TBI_NMMREF,
};

static error type_inference_un(struct module *mod, struct node *node) {
  assert(node->which == UN);
  error e;

  const enum token_type op = node->as.UN.operator;
  switch (OP_KIND(op)) {
  case OP_UN_REFOF:
    node->typ = typ_new(typ_lookup_builtin(mod, tbi_for_ref[op])->definition, TYPE_DEF, 1, 0);
    node->typ->gen_args[1] = node->subs[0]->typ;
    node->is_type = node->subs[0]->is_type;
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
  case OP_UN_DYN:
    e = typ_compatible_reference(mod, node, node->subs[0]->typ);
    EXCEPT(e);
    node->typ = node->subs[0]->typ;
    node->is_type = node->subs[0]->is_type;
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

static error type_inference_bin_accessor(struct module *mod, struct node *node) {
  error e;
  struct node *field = NULL;
  e = scope_lookup(&field, mod, node->scope, node);
  EXCEPT(e);

  node->typ = field->typ;
  node->is_type = field->is_type;

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
  if (!node->subs[1]->is_type) {
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

    if (n > 0 && node->is_type != node->subs[n]->is_type) {
      error e = mk_except_type(mod, node->subs[n], "tuple combines values and types");
      EXCEPT(e);
    }
    node->is_type |= node->subs[n]->is_type;
  }
  module_needs_instance(mod, node->typ);
  return 0;
}

static error type_inference_init(struct module *mod, struct node *node) {
  struct node *def = node->subs[0]->typ->definition;

  for (size_t n = 1; n < node->subs_count; n += 2) {
    struct node *field_name = node->subs[n];
    struct node *field = NULL;
    error e = scope_lookup(&field, mod, def->scope, field_name);
    EXCEPT(e);
    e = type_destruct(mod, node->subs[n+1], field->typ);
    EXCEPT(e);
  }

  node->typ = node->subs[0]->typ;
  return 0;
}

static error type_inference_call(struct module *mod, struct node *node) {
  struct node *fun = node->subs[0];
  struct node *def = NULL;

  error e = scope_lookup(&def, mod, fun->scope, fun);
  EXCEPT(e);

  if (def->typ->which != TYPE_FUNCTION) {
    e = mk_except_type(mod, fun, "not a function");
    EXCEPT(e);
  }

  fun->typ = def->typ;

  if (node->subs_count == 1) {
    return type_inference_unary_call(mod, node, fun->typ->definition);
  }

  if (node_fun_args_count(fun->typ->definition) != node->subs_count - 1) {
    error e = mk_except_call_arg_count(mod, node, def, node->subs_count - 1);
    EXCEPT(e);
  }

  for (size_t n = 1; n < node->subs_count; ++n) {
    if (n > 0 && node->is_type != node->subs[n]->is_type) {
      e = mk_except_type(mod, node->subs[n], "call combines value and type arguments");
      EXCEPT(e);
    }
    node->is_type |= node->subs[n]->is_type;
  }

  if (node->is_type) {
    assert(FALSE);
    //switch (fun->typ->definition->typ->which) {
    //case TYPE_FUNCTION:
    //  instance = instantiate_function(mod, fun, node);
    //  node->typ = instance->typ;
    //  break;
    //case TYPE_DEF:
    //  instance = instantiate_type(mod, fun, node);
    //  node->typ = instance->typ;
    //  break;
    //default:
    //  assert(FALSE);
    //}
    //
    //module_needs_instance(mod, node->typ);
  } else {
    if (node->subs_count - 1 != fun->typ->fun_arity) {
      e = mk_except_type(mod, node, "");
      EXCEPT(e);
    }
    for (size_t n = 1; n < node->subs_count; ++n) {
      e = type_destruct(mod, node->subs[n], fun->typ->fun_args[n-1]);
      EXCEPT(e);
    }
    node->typ = node_fun_retval(fun->typ->definition)->typ;
  }

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

  struct typ *u = t->excepts[0]->typ;
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
  node->is_type = def->is_type;

  if (node->which == BIN) {
    assert(node->as.BIN.operator == TDOT);
    e = type_destruct_import_path(mod, node->subs[0]);
    EXCEPT(e);
  }

  assert(node->typ != NULL);

  return 0;
}

static error type_destruct(struct module *mod, struct node *node, struct typ *constraint) {
  error e;
  struct node *def = NULL;

  assert(node->typ != typ_lookup_builtin(mod, TBI__NOT_TYPEABLE));

  if (node->typ != NULL
      && node->typ != typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT)
      && typ_is_concrete(mod, node->typ)) {

    for (size_t n = 0; n < node->subs_count; ++n) {
      assert(node->subs[n]->typ != NULL
             && node->subs[n]->typ != typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT));
    }

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
  case STRING:
    e = typ_unify(&node->typ, mod, node, typ_lookup_builtin(mod, TBI_STRING), constraint);
    EXCEPT(e);
    break;
  case IDENT:
    // FIXME make sure ident not used before definition.
    // FIXME let x, (y, z) = i32, (i32, i32)
    // In this case, y (for instance), will not have is_type set properly.
    // is_type needs to be set recursively when descending via
    // type_destruct.
    e = scope_lookup(&def, mod, node->scope, node);
    EXCEPT(e);

    if (def->which == DEFNAME) {
      e = type_destruct(mod, def, constraint);
      EXCEPT(e);
    }
    node->typ = def->typ;
    node->is_type = def->is_type;
    break;
  case DEFNAME:
    e = typ_unify(&node->typ, mod, node, node->typ, constraint);
    EXCEPT(e);
    e = type_destruct(mod, node->subs[1], node->typ);
    EXCEPT(e);
    node->is_type = node->subs[1]->is_type;
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
      e = typ_compatible_reference(mod, node, constraint);
      break;
    case OP_UN_DYN:
      e = typ_compatible(mod, node, constraint, typ_lookup_builtin(mod, TBI_DYN));
      break;
    default:
      assert(FALSE);
    }
    assert(!e);
    EXCEPT(e);

    struct node *sub_node = node->subs[0];
    struct typ *sub_constraint = NULL;

    switch (OP_KIND(node->as.UN.operator)) {
    case OP_UN_REFOF:
      sub_constraint = constraint->gen_args[1];
      break;
    default:
      sub_constraint = constraint;
      node->is_type = node->subs[0]->is_type;
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

    struct typ *left_constraint = constraint;
    struct typ *right_constraint = constraint;

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
      if (!node->subs[1]->is_type) {
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

error step_type_inference(struct module *mod, struct node *node, void *user, bool *stop) {
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
    node->typ = def->typ;
    node->is_type = def->is_type;
    assert(node->typ->which != TYPE__MARKER);
    goto ok;
  case IMPORT:
    e = scope_lookup(&def, mod, mod->gctx->modules_root.scope, node->subs[0]);
    EXCEPT(e);
    node->typ = def->typ;
    node->is_type = def->is_type;
    for (size_t n = 1; n < node->subs_count; ++n) {
      struct node *s = node->subs[n]->subs[0];
      e = type_destruct_import_path(mod, s);
      EXCEPT(e);

      assert(s->typ->which != TYPE__MARKER);
      node->subs[n]->typ = s->typ;
      node->subs[n]->is_type = s->is_type;
    }
    assert(node->typ->which != TYPE__MARKER);
    goto ok;
  case NUMBER:
    node->typ = typ_lookup_builtin(mod, TBI_LITERALS_INTEGER);
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
    e = type_destruct(mod, node->subs[0], node->subs[1]->typ);
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
  case TYPECONSTRAINT:
    node->typ = node->subs[1]->typ;
    e = type_destruct(mod, node->subs[0], node->typ);
    EXCEPT(e);
    assert(!node->subs[0]->is_type);
    goto ok;
  case DEFFUN:
  case DEFMETHOD:
    node->typ = typ_new(node, TYPE_FUNCTION, 0, node_fun_args_count(node));
    for (size_t n = 0; n < node->typ->fun_arity; ++n) {
      node->typ->fun_args[n] = node->subs[n+1]->typ;
    }
    node->is_type = TRUE;
    module_return_set(mod, NULL);
    goto ok;
  case DEFINTF:
    goto ok;
  case DEFTYPE:
    switch (node->as.DEFTYPE.kind) {
    case DEFTYPE_ENUM:
    case DEFTYPE_SUM:
      {
        struct typ *u = typ_lookup_builtin(mod, TBI_LITERALS_INTEGER);
        for (size_t n = 0; n < node->subs_count; ++n) {
          if (node->subs[n]->which != DEFCHOICE) {
            continue;
          }
          e = typ_unify(&u, mod, node->subs[n],
                        u, node->subs[n]->subs[1]->typ);
          EXCEPT(e);
        }

        if (u == typ_lookup_builtin(mod, TBI_LITERALS_INTEGER)) {
          u = typ_lookup_builtin(mod, TBI_U32);
        }

        for (size_t n = 0; n < node->subs_count; ++n) {
          if (node->subs[n]->which != DEFCHOICE) {
            continue;
          }
          e = type_destruct(mod, node->subs[n]->subs[1], u);
          EXCEPT(e);

          node->subs[n]->typ = node->typ;
        }
      }
      break;
    default:
      break;
    }
    goto ok;
  case DEFNAME:
    // FIXME handle case where there is a constraint on the DEFNAME;
    node->typ = node->subs[1]->typ;
    node->is_type = node->subs[1]->is_type;
    goto ok;
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
    node->typ = typ_lookup_builtin(mod, TBI_VOID);
    goto ok;
  case ISA:
    node->typ = node->subs[0]->typ;
    goto ok;
  case DIRECTDEF:
    node->typ = node->as.DIRECTDEF.definition->typ;
    node->is_type = node->as.DIRECTDEF.definition->is_type;
    goto ok;
  default:
    goto ok;
  }

ok:
  assert(node->typ != typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT));
  assert(node->typ != NULL);
  return 0;
}

error step_type_inference_isalist(struct module *mod, struct node *node, void *user, bool *stop) {
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

  struct node *isalist = node->subs[1];
  assert(isalist->which == ISALIST);

  node->typ->isalist_count = isalist->subs_count;
  node->typ->isalist = calloc(isalist->subs_count, sizeof(*node->typ->isalist));
  node->typ->isalist_exported = calloc(isalist->subs_count, sizeof(bool));
  for (size_t n = 0; n < isalist->subs_count; ++n) {
    struct node *isa = isalist->subs[n];
    assert(isa->which == ISA);

    struct node *def = NULL;
    e = scope_lookup(&def, mod, node->scope->parent, isa->subs[0]);
    EXCEPT(e);

    isa->typ = def->typ;
    node->typ->isalist[n] = def->typ;
    node->typ->isalist_exported[n] = isa->as.ISA.toplevel.is_export;
  }

  return 0;
}

static size_t find_subnode_in_parent(struct node *parent, struct node *node) {
  for (size_t n = 0; n < parent->subs_count; ++n) {
    if (parent->subs[n] == node) {
      return n;
    }
  }

  assert(FALSE);
  return 0;
}

static error zero_for_generated(struct module *mod, struct node *node,
                                struct scope *parent_scope) {
  error e = zeropass(mod, node);
  EXCEPT(e);
  node->scope->parent = parent_scope;

  return 0;
}

static error zero_and_first_for_generated(struct module *mod, struct node *node,
                                          struct scope *parent_scope) {
  error e = zero_for_generated(mod, node, parent_scope);
  EXCEPT(e);

  e = firstpass(mod, node);
  EXCEPT(e);

  return 0;
}

static void define_builtin(struct module *mod, struct node *tdef,
                           enum builtingen bg) {
  struct node *modbody = tdef->scope->parent->node;
  size_t insert_pos = find_subnode_in_parent(modbody, tdef) + 1;

  struct node *proto = NULL;
  error e = scope_lookup_abspath(&proto, mod, builtingen_abspath[bg]);
  assert(!e);

  struct node *d = node_new_subnode(mod, modbody);
  node_deepcopy(mod, d, proto);

  struct toplevel *toplevel = node_toplevel(d);
  toplevel->scope_name = node_ident(tdef);
  toplevel->builtingen = bg;
  toplevel->is_export = TRUE;

  mk_expr_abspath(mod, d, builtingen_abspath[bg]);
  move_last_over(d, 0);

  insert_last_at(modbody, insert_pos);
  e = zero_for_generated(mod, d, modbody->scope);
  assert(!e);
}

error step_add_builtin_operators(struct module *mod, struct node *node, void *user, bool *stop) {
  switch (node->which) {
  case DEFTYPE:
    break;
  default:
    return 0;
  }

  switch (node->as.DEFTYPE.kind) {
  case DEFTYPE_PROTOTYPE:
    break;
  case DEFTYPE_STRUCT:
    break;
  case DEFTYPE_SUM:
    if (typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_COMPARABLE))) {
      define_builtin(mod, node, BG_SUM_EQ);
      define_builtin(mod, node, BG_SUM_NE);
    }
    break;
  case DEFTYPE_ENUM:
    fprintf(stderr, "bi op %s\n", idents_value(mod->gctx, node_ident(node)));
    define_builtin(mod, node, BG_ENUM_EQ);
    define_builtin(mod, node, BG_ENUM_NE);
    break;
  }

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
  [TUBWNOT] = ID_OPERATOR_UBWNOT,
};

static struct node *expr_ref(enum token_type ref_op, struct node *node) {
  struct node *n = calloc(1, sizeof(struct node));
  n->which = UN;
  n->as.UN.operator = ref_op;
  n->subs_count = 1;
  n->subs = calloc(n->subs_count, sizeof(struct node *));
  n->subs[0] = node;
  return n;
}

error step_operator_call_inference(struct module *mod, struct node *node, void *user, bool *stop) {
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

  if (typ_isa(mod, node->subs[0]->typ, typ_lookup_builtin(mod, TBI_NATIVE_INTEGER))) {
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

  insert_last_at(node, 0);
  node->subs[1] = expr_ref(TREFDOT, node->subs[1]);
  node->subs[2] = expr_ref(TREFDOT, node->subs[2]);

  error e = zero_and_first_for_generated(mod, node, saved_parent);
  EXCEPT(e);

  return 0;
}

error step_unary_call_inference(struct module *mod, struct node *node, void *user, bool *stop) {
  return 0;
}

error step_ctor_call_inference(struct module *mod, struct node *node, void *user, bool *stop) {
  return 0;
}

error step_gather_generics(struct module *mod, struct node *node, void *user, bool *stop) {
  return 0;
}

error step_call_arguments_prepare(struct module *mod, struct node *node, void *user, bool *stop) {
  return 0;
}

error step_temporary_inference(struct module *mod, struct node *node, void *user, bool *stop) {
  return 0;
}

error zeropass(struct module *mod, struct node *node) {
  static const step zeropass_down[] = {
    step_detect_deftype_kind,
    step_assign_deftype_which_values,
    step_extend_deftype_builtin_isalist,
    step_add_builtin_members,
    step_add_codegen_variables,
    NULL,
  };
  static const step zeropass_up[] = {
    step_add_scopes,
    NULL,
  };

  error e = pass(mod, node, zeropass_down, zeropass_up, NULL);
  EXCEPT(e);

  return 0;
}

error firstpass(struct module *mod, struct node *node) {
  static const step firstpass_down[] = {
    step_stop_submodules,
    step_lexical_scoping,
    step_type_definitions,
    step_type_destruct_mark,
    step_type_gather_returns,
    step_type_gather_excepts,
    NULL,
  };
  static const step firstpass_up[] = {
    step_type_inference,
    step_type_inference_isalist,
    step_add_builtin_constructors,
    step_add_builtin_operators,
    NULL,
  };

  int module_depth = 0;
  error e = pass(mod, node, firstpass_down, firstpass_up, &module_depth);
  EXCEPT(e);

  return 0;
}

error secondpass(struct module *mod, struct node *node) {
  static const step secondpass_down[] = {
    step_operator_call_inference,
    step_unary_call_inference,
    step_ctor_call_inference,
    step_gather_generics,
    step_call_arguments_prepare,
    step_temporary_inference,
    NULL,
  };
  static const step secondpass_up[] = {
    NULL,
  };

  int module_depth = 0;
  error e = pass(mod, node, secondpass_down, secondpass_up, &module_depth);
  EXCEPT(e);

  return 0;
}
