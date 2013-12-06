#include "scope.h"

#include "table.h"
#include "types.h"

HTABLE_SPARSE(scope_map, struct node *, ident);
implement_htable_sparse(__attribute__((unused)) static, scope_map, struct node *, ident);

uint32_t ident_hash(const ident *a) {
  return hash32_hsieh(a, sizeof(*a));
}

int ident_cmp(const ident *a, const ident *b) {
  return memcmp(a, b, sizeof(*a));
}

struct scope *scope_new(struct node *node) {
  assert(node != NULL);

  struct scope *s = calloc(1, sizeof(struct scope));
  s->map = calloc(1, sizeof(struct scope_map));
  scope_map_init(s->map, 0);
  scope_map_set_delete_val(s->map, NULL);
  scope_map_set_custom_hashf(s->map, ident_hash);
  scope_map_set_custom_cmpf(s->map, ident_cmp);

  s->node = node;
  return s;
}

// Return value must be freed by caller.
char *scope_name(const struct module *mod, const struct scope *scope) {
  if (scope->node == &mod->gctx->modules_root) {
    const char *name = idents_value(mod->gctx, node_ident(scope->node));
    char *r = calloc(strlen(name) + 1, sizeof(char));
    strcpy(r, name);
    return r;
  }

  size_t len = 0;
  const struct scope *root = scope;
  while (root->parent != NULL) {
    ident id = node_ident(root->node);
    if (id != ID_ANONYMOUS) {
      len += strlen(idents_value(mod->gctx, id)) + 1;
    }
    root = root->parent;
  }
  len -= 1;

  char *r = calloc(len + 1, sizeof(char));
  root = scope;
  while (root->parent != NULL) {
    ident id = node_ident(root->node);
    if (id != ID_ANONYMOUS) {
      const char *name = idents_value(mod->gctx, id);
      size_t name_len = strlen(name);
      if (root->parent->parent == NULL) {
        memcpy(r, name, name_len);
      } else {
        len -= name_len + 1;
        r[len] = '.';
        memcpy(r + len + 1, name, name_len);
      }
    }
    root = root->parent;
  }

  return r;
}

struct scope_definitions_name_list_data {
  const struct module *mod;
  char *s;
  size_t len;
};

static int scope_definitions_name_list_iter(const ident *key,
                                            struct node **val,
                                            void *user) {
  struct scope_definitions_name_list_data *d = user;
  const char *n = idents_value(d->mod->gctx, *key);
  const size_t nlen = strlen(n);

  const bool first = d->s == NULL;

  d->s = realloc(d->s, d->len + nlen + (first ? 1 : 3));
  char *p = d->s + d->len;

  if (!first) {
    strcpy(p, ", ");
    p += 2;
    d->len += 2;
  }

  strcpy(p, n);
  d->len += nlen;

  return 0;
}

char *scope_definitions_name_list(const struct module *mod,
                                  const struct scope *scope) {
  struct scope_definitions_name_list_data d = {
    .mod = mod,
    .s = NULL,
    .len = 0,
  };

  scope_map_foreach(scope->map, scope_definitions_name_list_iter, &d);

  return d.s;
}

error scope_define_ident(const struct module *mod, struct scope *scope,
                         ident id, struct node *node) {
  assert(id != ID__NONE);
  struct node **existing = scope_map_get(scope->map, id);

  // If existing is prototype, we just replace it with full definition.
  // If not, it's an error:
  if (existing != NULL
      && (!node_is_prototype(*existing)
          || node->which != (*existing)->which)) {
    const struct module *existing_mod = try_node_module_owner_const(mod, *existing);
    struct token existing_tok = { 0 };
    existing_tok.t = TIDENT;
    existing_tok.value = existing_mod->parser.data + (*existing)->codeloc;
    existing_tok.len = 0;
    char *scname = scope_name(mod, scope);
    error e = mk_except(try_node_module_owner_const(mod, node), node,
                        "in scope %s: identifier '%s' already defined at %s:%d:%d",
                        scname, idents_value(mod->gctx, id), existing_mod->filename,
                        parser_line(&existing_mod->parser, &existing_tok),
                        parser_column(&existing_mod->parser, &existing_tok));
    free(scname);
    THROW(e);
  } else if (existing != NULL) {
    struct toplevel *toplevel = node_toplevel(*existing);
    if (toplevel != NULL) {
      // FIXME? Should it only be possible to shadow an imported name from
      // an from xxx import *
      toplevel->is_shadowed = TRUE;
    }
    *existing = node;
  } else {
    scope_map_set(scope->map, id, node);
  }

  return 0;
}

error scope_define(const struct module *mod, struct scope *scope,
                   struct node *id, struct node *node) {
  assert(id->which == IDENT);
  error e = scope_define_ident(mod, scope, id->as.IDENT.name, node);
  EXCEPT(e);
  return 0;
}

struct use_isalist_state {
  struct node *result;
  const struct node *for_error;
  ident id;
};

static error do_scope_lookup_ident_immediate(struct node **result,
                                             const struct node *for_error,
                                             const struct module *mod,
                                             const struct scope *scope, ident id,
                                             bool allow_isalist, bool failure_ok);

static error use_isalist_scope_lookup(struct module *mod,
                                      struct typ *t, struct typ *intf,
                                      bool *stop, void *user) {
  struct use_isalist_state *st = user;

  struct node *result = NULL;
  error e = do_scope_lookup_ident_immediate(&result, st->for_error, mod,
                                            typ_definition_const(intf)->scope,
                                            st->id, FALSE, TRUE);

  if (!e) {
    if (result->which == DEFFUN || result->which == DEFMETHOD) {
      st->result = result;
      *stop = TRUE;
    }
  }

  return 0;
}

static error do_scope_lookup_ident_immediate(struct node **result,
                                             const struct node *for_error,
                                             const struct module *mod,
                                             const struct scope *scope, ident id,
                                             bool allow_isalist, bool failure_ok) {
  const bool use_isalist = allow_isalist
    && scope->node->which == DEFINTF
    && scope->node->typ != NULL;

  assert(id != ID__NONE);
  struct node **r = scope_map_get(scope->map, id);
  if (r != NULL) {
    *result = *r;
    return 0;
  }

  if (scope->node->which == MODULE && scope->node->subs_count >= 1) {
    struct node *body = scope->node->subs[0];
    error e = do_scope_lookup_ident_immediate(result, for_error, mod, body->scope,
                                              id, allow_isalist, failure_ok);
    if (!e) {
      return 0;
    } else if (failure_ok) {
      return e;
    } else {
      EXCEPT(e);
    }
  }

  if (use_isalist) {
    const struct node *parent = scope->node;
    // FIXME: We should statically detect when this search would return
    // different results depending on order. And we should force the caller
    // to specificy which intf is being called if it's ambiguous.
    // FIXME: Should prevent lookups from another module to use non-exported
    // interfaces.
    enum isalist_filter filter = 0;
    struct use_isalist_state st = {
      .result = NULL,
      .for_error = for_error,
      .id = id,
    };

    error e = typ_isalist_foreach((struct module *) mod, parent->typ, filter,
                                  use_isalist_scope_lookup, &st);
    EXCEPT(e);

    if (st.result != NULL) {
      *result = st.result;
      return 0;
    }
  }

  if (failure_ok) {
    return EINVAL;
  } else {
    char *scname = scope_name(mod, scope);
    char *escname = scope_name(mod, for_error->scope);
    error e = mk_except(try_node_module_owner_const(mod, for_error), for_error,
                        "from scope %s: in scope %s: unknown identifier '%s'",
                        escname, scname, idents_value(mod->gctx, id));
    free(escname);
    free(scname);
    THROW(e);
  }

  return 0;
}

error scope_lookup_ident_immediate(struct node **result, const struct node *for_error,
                                   const struct module *mod,
                                   const struct scope *scope, ident id,
                                   bool failure_ok) {
  return do_scope_lookup_ident_immediate(result, for_error, mod, scope, id,
                                         TRUE, failure_ok);
}

static error do_scope_lookup_ident_wontimport(struct node **result, const struct node *for_error,
                                              const struct module *mod,
                                              const struct scope *scope, ident id,
                                              bool failure_ok) {
  char *scname = NULL;
  char *escname = NULL;
  error e;

skip:
  e = do_scope_lookup_ident_immediate(result, for_error, mod, scope, id, FALSE, TRUE);
  if (!e) {
    if (scope->node->which == DEFTYPE
        && ((*result)->which == DEFFUN || (*result)->which == DEFMETHOD)) {
      // Skip scope: id is a bare identifier, cannot reference functions or
      // methods in the scope of a DEFTYPE. Must use the form 'this.name'.
      scope = scope->parent;
      goto skip;
    }
    return 0;
  }

  // Will not go up in modules_root past a MODULE_BODY node as there is no
  // permission to access these scopes unless descending from
  // gctx->modules_root.
  if (scope->parent == NULL || scope->node->which == MODULE_BODY) {
    if (failure_ok) {
      return e;
    } else {
      scname = scope_name(mod, scope);
      escname = scope_name(mod, for_error->scope);
      e = mk_except(try_node_module_owner_const(mod, for_error), for_error,
                    "from scope %s: in scope %s: unknown identifier '%s'",
                    escname, scname, idents_value(mod->gctx, id));
      free(escname);
      free(scname);
      THROW(e);
    }
  }

  return do_scope_lookup_ident_wontimport(result, for_error, mod, scope->parent,
                                          id, failure_ok);
}

error scope_lookup_ident_wontimport(struct node **result, const struct node *for_error,
                                    const struct module *mod,
                                    const struct scope *scope, ident id, bool failure_ok) {
  return do_scope_lookup_ident_wontimport(result, for_error, mod, scope, id, failure_ok);
}

#define EXCEPT_UNLESS(e, failure_ok) do { \
  if (e) {\
    if (failure_ok) { \
      return e; \
    } else { \
      EXCEPT(e); \
    } \
  } \
} while (0)

static error do_scope_lookup(struct node **result, const struct node *for_error,
                             const struct module *mod,
                             const struct scope *scope, const struct node *id,
                             bool failure_ok) {
  error e;
  char *scname = NULL;
  char *escname = NULL;
  struct node *parent = NULL, *r = NULL;

  switch (id->which) {
  case IDENT:
    e = do_scope_lookup_ident_wontimport(&r, for_error, mod, scope, id->as.IDENT.name, failure_ok);
    EXCEPT_UNLESS(e, failure_ok);

    break;
  case BIN:
    if (id->as.BIN.operator != TDOT
        && id->as.BIN.operator != TBANG
        && id->as.BIN.operator != TSHARP) {
      GOTO_EXCEPT_TYPE(try_node_module_owner_const(mod, id), id, "malformed name");
    }
    if (id->subs[1]->which != IDENT) {
      GOTO_EXCEPT_TYPE(try_node_module_owner_const(mod, id), id, "malformed name");
    }

    e = do_scope_lookup(&parent, for_error, mod, scope, id->subs[0], failure_ok);
    EXCEPT_UNLESS(e, failure_ok);

    e = do_scope_lookup_ident_immediate(&r, for_error, mod, parent->scope,
                                        id->subs[1]->as.IDENT.name, FALSE, failure_ok);
    EXCEPT_UNLESS(e, failure_ok);

    break;
  case DIRECTDEF:
    r = typ_definition(id->as.DIRECTDEF.typ);
    break;
  default:
    assert(FALSE);
    return 0;
  }

  if (r->which == IMPORT) {
    struct node *mark = r;
    if (!mark->as.IMPORT.intermediate_mark) {
      e = do_scope_lookup(&r, for_error, mod, mod->gctx->modules_root.scope,
                          r->subs[0], failure_ok);
      EXCEPT_UNLESS(e, failure_ok);
    }
  }

  *result = r;
  return 0;

except:
  free(escname);
  free(scname);
  return e;
}

error scope_lookup(struct node **result, const struct module *mod,
                   const struct scope *scope, const struct node *id,
                   bool failure_ok) {
  return do_scope_lookup(result, id, mod, scope, id, failure_ok);
}

error scope_lookup_module(struct node **result, const struct module *mod,
                          const struct node *id, bool failure_ok) {
  error e = 0;
  struct node *parent = NULL, *r = NULL;
  struct scope *scope = mod->gctx->modules_root.scope;
  const struct node *for_error = id;

  switch (id->which) {
  case IDENT:
    e = do_scope_lookup_ident_wontimport(&r, for_error, mod, scope, id->as.IDENT.name, TRUE);
    break;
  case BIN:
    if (id->as.BIN.operator != TDOT) {
      e = mk_except_type(try_node_module_owner_const(mod, id), id,
                         "malformed module path name");
      THROW(e);
    }
    e = do_scope_lookup(&parent, for_error, mod, scope, id->subs[0], TRUE);
    if (e) {
      break;
    }
    e = do_scope_lookup(&r, for_error, mod, parent->scope, id->subs[1], TRUE);
    if (e) {
      break;
    }
    break;
  default:
    assert(FALSE);
    return 0;
  }

  if (e || r->which != MODULE) {
    goto err;
  }

  *result = r;
  return 0;

err:
  if (failure_ok) {
    return EINVAL;
  } else {
    if (r != NULL && r->which != MODULE) {
      e = mk_except(try_node_module_owner_const(mod, id), id,
                    "not the name of a module");
      THROW(e);
    } else {
      e = mk_except(try_node_module_owner_const(mod, id), id,
                    "module not found");
      THROW(e);
    }
  }

  assert(FALSE && "Unreached.");
  return 0;
}

static error do_scope_lookup_abspath(struct node **result, const struct node *for_error,
                                     const struct module *mod,
                                     const char *path, ssize_t len, ssize_t full_len) {
  ssize_t i;
  ident id = ID__NONE;
  for (i = len-1; i >= 0; --i) {
    if (i == 0) {
      assert(len > 1);
      id = idents_add_string(mod->gctx, path, len - i);
      break;
    } else if (path[i] == '.') {
      assert(len - i > 1);
      id = idents_add_string(mod->gctx, path + i + 1, len - i - 1);
      break;
    }
  }
  assert(id != ID__NONE);

  error e;
  if (i == 0) {
    e = do_scope_lookup_ident_immediate(result, for_error, mod,
                                        mod->gctx->modules_root.scope, id,
                                        FALSE, TRUE);
  } else {
    struct node *parent = NULL;
    e = do_scope_lookup_abspath(&parent, for_error, mod, path, i, full_len);
    EXCEPT(e);
    e = do_scope_lookup_ident_immediate(result, for_error, mod, parent->scope, id,
                                        len == full_len, TRUE);
  }

  if (e) {
    char *escname = scope_name(mod, for_error->scope);
    e = mk_except(try_node_module_owner_const(mod, for_error), for_error,
                  "from scope %s: in global scope, unknown identifier '%s'",
                  escname, path);
    free(escname);
    THROW(e);
  }

  return 0;
}

error scope_lookup_abspath(struct node **result, const struct node *for_error,
                           const struct module *mod, const char *path) {
  const ssize_t len = strlen(path);
  return do_scope_lookup_abspath(result, for_error, mod, path, len, len);
}