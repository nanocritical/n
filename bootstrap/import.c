#include "import.h"

#include "types.h"
#include "scope.h"
#include "passes.h"

#include "passzero.h"

static error do_pass_import_mark(struct module *mod, struct node *root,
                                 void *user, size_t shallow_last_up) {
  PASS(, UP_STEP(step_add_scopes));
  return 0;
}

static void pass_import_mark(struct module *mod, struct node *mark,
                             struct scope *parent_scope) {
  PUSH_STATE(mod->state->step_state);
  error e = do_pass_import_mark(mod, mark, NULL, -1);
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

error lexical_import(struct scope *scope, struct module *mod,
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
