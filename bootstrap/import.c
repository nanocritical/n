#include "import.h"

#include "types.h"
#include "scope.h"
#include "passes.h"
#include "parser.h"

static void pass_import_mark(struct module *mod, struct node *mark) {
  // Special self-referencing typ; see type_inference_bin_accessor().
  mark->typ = typ_create(NULL, mark);
  mark->flags = NODE_IS_TYPE;
}

static ERROR check_import_target_exists(bool *is_globalenv, struct module *mod,
                                        struct node *module_import_path,
                                        bool points_inside_module) {
  struct node *mpath
    = points_inside_module ? subs_first(module_import_path) : module_import_path;

  struct node *def = NULL;
  error e = scope_lookup_module(&def, mod, mpath, true);

  if (e == EINVAL && !points_inside_module /* really, the caller doesn't know */) {
    e = check_import_target_exists(is_globalenv, mod, module_import_path, true);
    EXCEPT(e);
    return 0;
  } else if (e) {
    // Repeat bound-to-fail call to get error message right.
    e = scope_lookup_module(&def, mod, mpath, false);
    THROW(e);
  }

  if (points_inside_module) {
    const bool could_be_globalenv = def->which == MODULE;
    struct node *target = NULL;
    struct node *id = subs_last(module_import_path);
    e = scope_lookup_ident_immediate(&target, id,
                                     mod, def,
                                     node_ident(id), could_be_globalenv);

    if (e && could_be_globalenv) {
      e = scope_lookup_ident_immediate(&target, id,
                                       mod, def->as.MODULE.mod->body->as.MODULE_BODY.globalenv_scoper,
                                       node_ident(id), true);

      if (e) {
        // Give up: repeat bound-to-fail call to get error message right.
        e = scope_lookup_ident_immediate(&target, id,
                                         mod, def,
                                         node_ident(id), false);
        THROW(e);
      } else {
        *is_globalenv = true;
      }
    } else if (e) {
      THROW(e);
    }
  }

  return 0;
}

static ERROR import_single_ident(struct module *mod, struct node *original_import,
                                 struct node *import, bool inside_module) {
  struct node *import_path = subs_first(import);
  bool is_globalenv = false;
  error e = check_import_target_exists(&is_globalenv, mod, import_path, inside_module);
  EXCEPT(e);

  struct node *name = NULL;
  switch (import_path->which) {
  case IDENT:
    name = import_path;
    break;
  case BIN:
    name = subs_last(import_path);
    break;
  default:
    assert(false);
  }

  struct node *scoper = mod->body;
  if (is_globalenv) {
    scoper = mod->body->as.MODULE_BODY.globalenv_scoper;
  }

  e = scope_define_ident(mod, scoper, node_ident(name), import);
  assert(!e);
  return 0;
}

static ERROR lookup_target_mod(struct module **target_mod,
                               struct module *mod,
                               struct node *import_path) {
  struct node *target = NULL;
  error e = scope_lookup_module(&target, mod, import_path, false);
  EXCEPT(e);

  *target_mod = target->as.MODULE.mod;
  return 0;
}

static ERROR lexical_import_from_path(struct module *mod,
                                      struct node *original_import,
                                      struct node *import) {
  struct module *target_mod = NULL;
  error e = lookup_target_mod(&target_mod, mod, subs_first(import));

  bool keep_existing = importmap_set(&mod->importmap, target_mod, original_import);
  (void) keep_existing;

  // We take of copy of 'last' now as 'import' will grow if
  // 'import == original_import'.
  struct node *last = subs_last(import);
  FOREACH_SUB_EVERY(id_import, import, 1, 1) {
    e = import_single_ident(mod, original_import, id_import, true);
    EXCEPT(e);

    if (id_import == last) {
      break;
    }
  }

  return 0;
}

static struct node *create_import_mark_for_ex(struct module *mod,
                                              struct node *original_import,
                                              struct node *import,
                                              struct node *ex) {
  struct node *import_id = mk_node(mod, original_import, IMPORT);
  import_id->as.IMPORT.toplevel.flags |=
    original_import->as.IMPORT.toplevel.flags & TOP_IS_EXPORT;

  struct token tok = { 0 };
  tok.t = TIDENT;
  tok.value = idents_value(mod->gctx, ex->which == LET
                           ? node_ident(subs_first(ex)) : node_ident(ex));
  tok.len = strlen(tok.value);
  copy_and_extend_import_path(mod, import_id, import, &tok);

  pass_import_mark(mod, import_id);
  return import_id;
}

static ERROR lexical_import_path(struct module *mod,
                                 struct node *original_import,
                                 struct node *import) {
  struct module *target_mod = NULL;
  error e = lookup_target_mod(&target_mod, mod, subs_first(import));

  bool keep_existing = importmap_set(&mod->importmap, target_mod, original_import);
  (void) keep_existing;

  if (target_mod->is_builtins) {
    node_toplevel(original_import)->flags |= TOP_IS_INLINE;
  }

  if (!original_import->as.IMPORT.is_all) {
    e = import_single_ident(mod, original_import, import, false);
    return 0;
  }

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
      e = lexical_import(mod, original_import, ex);
      EXCEPT(e);

      continue;
    }

    struct node *id_import = create_import_mark_for_ex(mod, original_import,
                                                       import, ex);
    e = import_single_ident(mod, original_import, id_import, true);
    EXCEPT(e);
  }

  return 0;
}

error lexical_import(struct module *mod,
                     struct node *original_import, struct node *import) {
  assert(import->which == IMPORT);
  error e;

  if (import->as.IMPORT.is_all) {
    // from <path> (import|export) *
    // Exposes all the identifiers defined in module at 'path'.
    e = lexical_import_path(mod, original_import, import);
    EXCEPT(e);
  } else if (next(subs_first(import)) == NULL) {
    // (import|export) <path>
    // Exposes the module at 'path' under the name subs_last(path).
    e = lexical_import_path(mod, original_import, import);
    EXCEPT(e);
  } else {
    // from <path> (import|export) <a> <b> <c> ...
    // Exposes the identifiers at 'path.a', 'path.b', 'path.c' under the
    // names 'a', 'b' and 'c'.
    e = lexical_import_from_path(mod, original_import, import);
    EXCEPT(e);
  }

  import->typ = TBI__NOT_TYPEABLE;
  import->flags = NODE_IS_TYPE;

  return 0;
}
