#include "import.h"

#include "types.h"
#include "scope.h"
#include "passes.h"

static void pass_import_mark(struct module *mod, struct node *mark) {
  // Special self-referencing typ; see type_inference_bin_accessor().
  mark->typ = typ_create(NULL, mark);
  mark->flags = NODE_IS_TYPE;
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
                                          subs_first(import_path), NULL);
    mark_ident = subs_last(import_path);
    break;
  default:
    assert(false);
  }

  // Ascent.
  if (import != NULL) {
    // Back at the top: we're done.
    struct node *placeholder;
    ident name = node_ident(mark_ident);
    e = scope_lookup_ident_immediate(&placeholder, mark_ident, mod, *scope,
                                     name, true);
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
  e = scope_lookup_ident_immediate(&mark, mark_ident, mod, *scope, name, true);
  if (e == EINVAL) {
    // We need to create an (intermediate) mark.
    struct node *anchor = (*scope == &mod->body->scope)
      ? original_import
      : scope_node((*scope));

    mark = mk_node(mod, anchor, IMPORT);
    mark->as.IMPORT.toplevel.flags |=
      original_import->as.IMPORT.toplevel.flags & TOP_IS_EXPORT;
    mark->as.IMPORT.intermediate_mark = true;
    node_deepcopy(mod, node_new_subnode(mod, mark), import_path);
    pass_import_mark(mod, mark);

    e = scope_define_ident(mod, *scope, name, mark);
    assert(!e);

  } else if (e) {
    assert(false);
  } else {
    assert(mark->which == IMPORT);
  }

  *scope = &mark->scope;

  return NULL;
}

static error check_import_target_exists(struct module *mod,
                                        struct node *module_import_path,
                                        int points_inside_module) {
  struct node *mpath
    = points_inside_module == 1 ? subs_first(module_import_path) : module_import_path;

  struct node *def = NULL;
  error e = scope_lookup_module(&def, mod, mpath, true);

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
    struct node *id = subs_last(module_import_path);
    e = scope_lookup_ident_immediate(&target, id,
                                     mod, &def->scope,
                                     node_ident(id), false);
    EXCEPT(e);
  }

  return 0;
}

static error import_single_ident(struct scope *scope, struct module *mod,
                                 struct node *original_import,
                                 struct node *id, bool define) {
  struct node *id_full_import_path = subs_first(id);
  assert(id_full_import_path->which == BIN);
  assert(subs_last(id_full_import_path)->which == IDENT);

  struct scope *tmp = scope;
  struct node *id_import_marker = create_lexical_import_hierarchy(
    &tmp, mod, original_import,
    id_full_import_path, id);

  error e;
  if (define) {
    ident id_name = node_ident(subs_last(id_full_import_path));
    e = scope_define_ident(mod, scope, id_name, id_import_marker);
    EXCEPT(e);

    e = check_import_target_exists(mod, id_full_import_path, -1);
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
  // We take of copy of 'last' now as 'import' will grow if
  // 'import == original_import'.
  struct node *last = subs_last(import);
  FOREACH_SUB_EVERY(id, import, 1, 1) {
    e = import_single_ident(scope, mod, original_import, id, true);
    EXCEPT(e);

    if (id == last) {
      break;
    }
  }

  return 0;
}

static struct node *create_import_node_for_ex(struct module *mod,
                                              struct node *original_import,
                                              struct node *import,
                                              struct node *ex) {
  struct node *id = node_new_subnode(mod, original_import);
  node_set_which(id, IMPORT);
  id->as.IMPORT.toplevel.flags |=
    original_import->as.IMPORT.toplevel.flags & TOP_IS_EXPORT;

  struct token tok = { 0 };
  tok.t = TIDENT;
  tok.value = idents_value(mod->gctx, node_ident(ex));
  tok.len = strlen(tok.value);
  copy_and_extend_import_path(mod, id, import, &tok);

  pass_import_mark(mod, id);

  return id;
}

static error lexical_import_path(struct scope *scope, struct module *mod,
                                 struct node *original_import,
                                 struct node *import) {
  error e;
  struct node *target = NULL;
  e = scope_lookup_module(&target, mod, subs_first(import), false);
  EXCEPT(e);

  bool need_expose_all = original_import->as.IMPORT.is_all
    || target->which == MODULE;

  if (!need_expose_all) {
    e = import_single_ident(scope, mod, original_import, import, false);
    EXCEPT(e);
    return 0;
  }

  struct module *target_mod = target->as.MODULE.mod;
  struct node *target_body = target_mod->body;
  FOREACH_SUB(ex, target_body) {
    const struct toplevel *toplevel = node_toplevel_const(ex);
    if (toplevel == NULL
        || !(toplevel->flags & TOP_IS_EXPORT)
        || toplevel->scope_name != ID__NONE
        || (toplevel->flags & TOP_IS_SHADOWED)) {
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

error lexical_import(struct scope *scope, struct module *mod,
                     struct node *original_import, struct node *import) {
  assert(import->which == IMPORT);
  error e;

  if (import->as.IMPORT.is_all) {
    // from <path> (import|export) *
    e = lexical_import_path(scope, mod, original_import, import);
    EXCEPT(e);
  } else if (next(subs_first(import)) == NULL) {
    // (import|export) <path>
    e = lexical_import_path(scope, mod, original_import, import);
    EXCEPT(e);
  } else {
    // from <path> (import|export) <a> <b> <c> ...
    e = lexical_import_from_path(scope, mod, original_import, import);
    EXCEPT(e);
  }

  import->typ = TBI__NOT_TYPEABLE;
  import->flags = NODE_IS_TYPE;

  return 0;
}
