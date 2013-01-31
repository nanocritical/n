#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "parser.h"
#include "table.h"

const char *node_which_strings[] = {
  [NUL] = "NUL",
  [IDENT] = "IDENT",
  [NUMBER] = "NUMBER",
  [STRING] = "STRING",
  [BIN] = "BIN",
  [UN] = "UN",
  [TUPLE] = "TUPLE",
  [CALL] = "CALL",
  [INIT] = "INIT",
  [RETURN] = "RETURN",
  [EXCEP] = "EXCEP",
  [BLOCK] = "BLOCK",
  [FUTURE] = "FUTURE",
  [LAMBDA] = "LAMBDA",
  [FOR] = "FOR",
  [WHILE] = "WHILE",
  [BREAK] = "BREAK",
  [CONTINUE] = "CONTINUE",
  [PASS] = "PASS",
  [IF] = "IF",
  [MATCH] = "MATCH",
  [TRY] = "TRY",
  [TYPECONSTRAINT] = "TYPECONSTRAINT",
  [DEFFUN] = "DEFFUN",
  [DEFTYPE] = "DEFTYPE",
  [DEFMETHOD] = "DEFMETHOD",
  [DEFINTF] = "DEFINTF",
  [DEFNAME] = "DEFNAME",
  [LET] = "LET",
  [DEFFIELD] = "DEFFIELD",
  [DEFCHOICE] = "DEFCHOICE",
  [DELEGATE] = "DELEGATE",
  [PRE] = "PRE",
  [POST] = "POST",
  [INVARIANT] = "INVARIANT",
  [EXAMPLE] = "EXAMPLE",
  [ISALIST] = "ISALIST",
  [IMPORT] = "IMPORT",
  [MODULE] = "MODULE",
  [ROOT_OF_ALL] = "ROOT_OF_ALL",
  [DIRECTDEF] = "DIRECTDEF",
};

static const char *predefined_idents_strings[ID__NUM] = {
  [ID__NONE] = "<NONE>",
  [ID_ANONYMOUS] = "<anonymous>",
  [ID_ROOT_OF_ALL] = "<root>",
  [ID_FOR] = "<for>",
  [ID_WHILE] = "<while>",
  [ID_MATCH] = "<match>",
  [ID_TRY] = "<try_catch>",
  [ID_LET] = "<let>",
  [ID_PRE] = "<pre>",
  [ID_POST] = "<post>",
  [ID_INVARIANT] = "<invariant>",
  [ID_EXAMPLE] = "<example>",
  [ID_THIS] = "this",
  [ID_SELF] = "self",
  [ID_MAIN] = "main",
  [ID_WHICH] = "which__",
  [ID_AS] = "as__",
  [ID_WHICH_TYPE] = "which_type__",
  [ID_AS_TYPE] = "as_type__",
  [ID_TBI_VOID] = "void",
  [ID_TBI_LITERALS_NULL] = "__null__",
  [ID_TBI_LITERALS_INTEGER] = "integer",
  [ID_TBI_PSEUDO_TUPLE] = "__pseudo_tuple__",
  [ID_TBI_BOOL] = "bool",
  [ID_TBI_I8] = "i8",
  [ID_TBI_U8] = "u8",
  [ID_TBI_I16] = "i16",
  [ID_TBI_U16] = "u16",
  [ID_TBI_I32] = "i32",
  [ID_TBI_U32] = "u32",
  [ID_TBI_I64] = "i64",
  [ID_TBI_U64] = "u64",
  [ID_TBI_SIZE] = "size",
  [ID_TBI_SSIZE] = "ssize",
  [ID_TBI_STRING] = "string",
  [ID_TBI_REF] = "ref",
  [ID_TBI_MREF] = "mref",
  [ID_TBI_MMREF] = "mmref",
  [ID_TBI_NREF] = "nref",
  [ID_TBI_NMREF] = "nmref",
  [ID_TBI_NMMREF] = "nmmref",
  [ID_TBI_DYN] = "__internal_dyn__",
  [ID_TBI_NATIVE_INTEGER] = "NativeInteger",
  [ID_TBI_BUILTIN_ENUM] = "BuiltinEnum",
  [ID_TBI__PENDING_DESTRUCT] = "__internal_pending_destruct__",
  [ID_TBI__NOT_TYPEABLE] = "__internal_not_typeable__",
  [ID_OPERATOR_OR] = "operator_or",
  [ID_OPERATOR_AND] = "operator_and",
  [ID_OPERATOR_NOT] = "operator_not",
  [ID_OPERATOR_LE] = "operator_le",
  [ID_OPERATOR_LT] = "operator_lt",
  [ID_OPERATOR_GT] = "operator_gt",
  [ID_OPERATOR_GE] = "operator_ge",
  [ID_OPERATOR_EQ] = "operator_eq",
  [ID_OPERATOR_NE] = "operator_ne",
  [ID_OPERATOR_BWOR] = "operator_bwor",
  [ID_OPERATOR_BWXOR] = "operator_bwxor",
  [ID_OPERATOR_BWAND] = "operator_bwand",
  [ID_OPERATOR_LSHIFT] = "operator_lshift",
  [ID_OPERATOR_RSHIFT] = "operator_rshift",
  [ID_OPERATOR_PLUS] = "operator_plus",
  [ID_OPERATOR_MINUS] = "operator_minus",
  [ID_OPERATOR_DIVIDE] = "operator_divide",
  [ID_OPERATOR_MODULO] = "operator_modulo",
  [ID_OPERATOR_TIMES] = "operator_times",
  [ID_OPERATOR_UMINUS] = "operator_uminus",
  [ID_OPERATOR_UBWNOT] = "operator_ubwnot",
};

HTABLE_SPARSE(idents_map, ident, struct token);
implement_htable_sparse(__attribute__((unused)) static, idents_map, ident, struct token);

uint32_t token_hash(const struct token *tok) {
  return hash32_hsieh(tok->value, tok->len);
}

int token_cmp(const struct token *a, const struct token *b) {
  if (a->len != b->len) {
    return a->len - b->len;
  } else {
    return memcmp(a->value, b->value, min(size_t, a->len, b->len));
  }
}

const char *idents_value(const struct globalctx *gctx, ident id) {
  assert(id <= gctx->idents.count);
  return gctx->idents.values[id];
}

ident idents_add(struct globalctx *gctx, const struct token *tok) {
  assert(tok->t == TIDENT);

  struct idents *idents = &gctx->idents;

  ident *existing_id = idents_map_get(idents->map, *tok);
  if (existing_id != NULL) {
    return *existing_id;
  }

  if (idents->count >= idents->capacity) {
    if (idents->capacity <= 1) {
      idents->capacity = 256;
    }

    size_t old_capacity = idents->capacity;
    idents->capacity *= 2;
    idents->values = realloc(idents->values, idents->capacity * sizeof(*idents->values));
    memset(idents->values + old_capacity, 0, idents->capacity - old_capacity);
  }

  char *cpy = malloc(tok->len + 1);
  memcpy(cpy, tok->value, tok->len);
  cpy[tok->len] = '\0';

  const ident id = idents->count;
  idents->values[id] = cpy;
  idents->count += 1;

  idents_map_set(idents->map, *tok, id);

  return id;
}

static int line(const struct parser *parser, const struct token *tok) {
  const size_t pos = tok->value != NULL ? tok->value - parser->data : parser->pos;
  int count = 1;
  for (size_t p = 0; p < pos; ++p) {
    if (parser->data[p] == '\n') {
      count += 1;
    }
  }
  return count;
}

static int column(const struct parser *parser, const struct token *tok) {
  const size_t pos = tok->value != NULL ? tok->value - parser->data : parser->pos;
  int count = 1;
  for (size_t p = 0; p < pos; ++p) {
    if (parser->data[p] == '\n') {
      count = 1;
    } else {
      count += 1;
    }
  }
  return count;
}

#define EXCEPT_SYNTAX(mod, tok, fmt, ...) do { \
  EXCEPTF(EINVAL, "%s:%d:%d: syntax: " fmt, \
          mod->filename, line(&mod->parser, tok), column(&mod->parser, tok), ##__VA_ARGS__); \
} while (0)

#define UNEXPECTED(mod, tok) do { \
  EXCEPT_SYNTAX(mod, tok, "unexpected token '%.*s'", (int)(tok)->len, (tok)->value); \
} while (0)

#define EXCEPT_PARSE(mod, codeloc, fmt, ...) do { \
  struct token tok; \
  tok.value = mod->parser.data + codeloc; \
  EXCEPTF(EINVAL, "%s:%d:%d: parse: " fmt, \
          mod->filename, line(&mod->parser, &tok), column(&mod->parser, &tok), ##__VA_ARGS__); \
} while (0)

#define EXCEPT_TYPE(mod, node, fmt, ...) do { \
  struct token tok; \
  tok.value = mod->parser.data + node->codeloc; \
  EXCEPTF(EINVAL, "%s:%d:%d: type: " fmt, \
          mod->filename, line(&mod->parser, &tok), column(&mod->parser, &tok), ##__VA_ARGS__); \
} while (0)

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

static struct node *do_node_module_owner(struct node *node) {
  if (node->which == MODULE) {
    return node;
  } else {
    return do_node_module_owner(node->scope->parent->node);
  }
}

struct module *node_module_owner(struct node *node) {
  struct node *n = do_node_module_owner(node);
  assert(n != NULL);
  assert(n->which == MODULE);
  return n->as.MODULE.mod;
}

const struct module *node_module_owner_const(const struct node *node) {
  return node_module_owner((struct node *) node);
}

ident node_ident(const struct node *node) {
  switch (node->which) {
  case IDENT:
    return node->as.IDENT.name;
  case FOR:
    return ID_FOR;
  case WHILE:
    return ID_WHILE;
  case MATCH:
    return ID_MATCH;
  case TRY:
    return ID_TRY;
  case PRE:
    return ID_PRE;
  case POST:
    return ID_POST;
  case INVARIANT:
    return ID_INVARIANT;
  case EXAMPLE:
    return ID_EXAMPLE;
  case DEFFUN:
  case DEFTYPE:
  case DEFMETHOD:
  case DEFFIELD:
  case DEFCHOICE:
  case DEFINTF:
    assert(node->subs[0]->which == IDENT);
    return node->subs[0]->as.IDENT.name;
  case LET:
    return ID_LET;
  case MODULE:
    return node->as.MODULE.name;
  case ROOT_OF_ALL:
    return ID_ROOT_OF_ALL;
  default:
    return ID_ANONYMOUS;
  }
}

const struct toplevel *node_toplevel(const struct node *node) {
  const struct toplevel *toplevel = NULL;

  switch (node->which) {
  case DEFFUN:
    toplevel = &node->as.DEFFUN.toplevel;
    break;
  case DEFTYPE:
    toplevel = &node->as.DEFTYPE.toplevel;
    break;
  case DEFMETHOD:
    toplevel = &node->as.DEFMETHOD.toplevel;
    break;
  case DEFINTF:
    toplevel = &node->as.DEFINTF.toplevel;
    break;
  case LET:
    toplevel = &node->as.LET.toplevel;
    break;
  case IMPORT:
    toplevel = &node->as.IMPORT.toplevel;
    break;
  default:
    break;
  }

  return toplevel;
}

bool node_is_prototype(const struct node *node) {
  const struct toplevel *toplevel = node_toplevel(node);
  if (toplevel == NULL) {
    return FALSE;
  } else {
    return toplevel->is_prototype;
  }
}

bool node_is_inline(const struct node *node) {
  const struct toplevel *toplevel = node_toplevel(node);
  if (toplevel == NULL) {
    return FALSE;
  } else {
    return toplevel->is_inline;
  }
}

bool node_is_export(const struct node *node) {
  const struct toplevel *toplevel = node_toplevel(node);
  if (toplevel == NULL) {
    return FALSE;
  } else {
    return toplevel->is_export;
  }
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
    len += strlen(idents_value(mod->gctx, node_ident(root->node))) + 1;
    root = root->parent;
  }
  len -= 1;

  char *r = calloc(len + 1, sizeof(char));
  root = scope;
  while (root->parent != NULL) {
    const char *name = idents_value(mod->gctx, node_ident(root->node));
    size_t name_len = strlen(name);
    if (root->parent->parent == NULL) {
      memcpy(r, name, name_len);
    } else {
      len -= name_len + 1;
      r[len] = '.';
      memcpy(r + len + 1, name, name_len);
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

char *scope_definitions_name_list(const struct module *mod, const struct scope *scope) {
  struct scope_definitions_name_list_data d = {
    .mod = mod,
    .s = NULL,
    .len = 0,
  };

  scope_map_foreach(scope->map, scope_definitions_name_list_iter, &d);

  return d.s;
}

error scope_define_ident(const struct module *mod, struct scope *scope, ident id, struct node *node) {
  assert(id != ID__NONE);
  struct node **existing = scope_map_get(scope->map, id);

  // If existing is prototype, we just replace it with full definition.
  // If not, it's an error:
  if (existing != NULL && !node_is_prototype(*existing)) {
    struct token existing_tok;
    existing_tok.t = TIDENT;
    existing_tok.value = mod->parser.data + (*existing)->codeloc;
    existing_tok.len = 0;
    const char *scname = scope_name(mod, scope);
    EXCEPT_PARSE(node_module_owner_const(node), node->codeloc,
                 "in scope %s: identifier '%s' already defined at %s:%d:%d",
                 scname, idents_value(mod->gctx, id), mod->filename,
                 line(&mod->parser, &existing_tok), column(&mod->parser, &existing_tok));
    // FIXME: leaking scname.
  }

  scope_map_set(scope->map, id, node);
  return 0;
}

error scope_define(const struct module *mod, struct scope *scope, struct node *id, struct node *node) {
  assert(id->which == IDENT);
  error e = scope_define_ident(mod, scope, id->as.IDENT.name, node);
  EXCEPT(e);
  return 0;
}

static error do_scope_lookup_ident_immediate(struct node **result, const struct module *mod,
                                             const struct scope *scope, ident id,
                                             const struct scope *within, bool failure_ok,
                                             bool use_isalist) {
  assert(id != ID__NONE);
  struct node **r = scope_map_get(scope->map, id);
  if (r != NULL) {
    *result = *r;
    return 0;
  }

  if (use_isalist) {
    // Depth-first.
    // FIXME: We should statically detect when this search would return
    // different results depending on order. And we should force the caller
    // to specificy which intf is being called.
    const struct node *parent = scope->node;
    for (size_t n = 0; n < parent->typ->isalist_count; ++n) {
      const struct typ *t = parent->typ->isalist[n];
      error e = do_scope_lookup_ident_immediate(result, mod, t->definition->scope, id,
                                          within, TRUE, TRUE);
      if (!e) {
        return 0;
      }
    }
  }

  if (failure_ok) {
    return EINVAL;
  } else {
    const char *scname = scope_name(mod, scope);
    const char *wscname = scope_name(mod, within);
    EXCEPT_PARSE(node_module_owner_const(within->node), scope->node->codeloc,
                 "in scope %s: from scope %s: unknown identifier '%s'",
                 scname, wscname, idents_value(mod->gctx, id));
    // FIXME: leaking scname.
  }

  return 0;
}

static error do_scope_lookup_ident(struct node **result, const struct module *mod,
                                   const struct scope *scope, ident id,
                                   const struct scope *within, bool failure_ok) {
  error e = do_scope_lookup_ident_immediate(result, mod, scope, id, within, TRUE, FALSE);
  if (!e) {
    return 0;
  }

  // Will not go up in modules_root past a MODULE node as there is no
  // permission to access these scopes unless descending from
  // gctx->modules_root.
  if (scope->parent == NULL || scope->node->which == MODULE) {
    if (failure_ok) {
      return e;
    } else {
      const char *scname = scope_name(mod, scope);
      const char *wscname = scope_name(mod, within);
      EXCEPT_PARSE(node_module_owner_const(within->node), scope->node->codeloc,
                   "in scope %s: from scope %s: unknown identifier '%s'",
                   scname, wscname, idents_value(mod->gctx, id));
      // FIXME: leaking scname.
    }
  }

  return do_scope_lookup_ident(result, mod, scope->parent, id, within, failure_ok);
}

error scope_lookup_ident(struct node **result, const struct module *mod,
                         const struct scope *scope, ident id, bool failure_ok) {
  return do_scope_lookup_ident(result, mod, scope, id, scope, failure_ok);
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

static error do_scope_lookup(struct node **result, const struct module *mod,
                             const struct scope *scope, const struct node *id,
                             const struct scope *within, bool failure_ok) {
  error e;
  struct node *parent = NULL, *r = NULL;

  switch (id->which) {
  case IDENT:
    e = do_scope_lookup_ident(&r, mod, scope, id->as.IDENT.name, within, failure_ok);
    EXCEPT_UNLESS(e, failure_ok);

    if (r->which == IMPORT) {
      e = do_scope_lookup(&r, mod, mod->gctx->modules_root.scope,
                          r->subs[0], within, failure_ok);
      EXCEPT_UNLESS(e, failure_ok);
    }

    break;
  case BIN:
    if (id->as.BIN.operator != TDOT
           && id->as.BIN.operator != TBANG
           && id->as.BIN.operator != TSHARP) {
      EXCEPT_TYPE(node_module_owner_const(id), id, "malformed name");
    }
    if (id->subs[1]->which != IDENT) {
      EXCEPT_TYPE(node_module_owner_const(id), id, "malformed name");
    }

    e = do_scope_lookup(&parent, mod, scope, id->subs[0], within, failure_ok);
    EXCEPT_UNLESS(e, failure_ok);

    bool fully_resolved = FALSE;
    if (parent->which == IMPORT) {
      while (parent->which == IMPORT) {
        // This may be a case like
        //   import os
        //   import os.path
        // and 'os' resolves to the 'import os' (that's the parent). So we
        // first search for 'path' id->subs[1] in parent->scope (note: it
        // would shadow a 'path' declaration in the module os, but that's
        // OK). If that fails, we will resolve parent to find the module
        // it's importing, and use the usual lookup path.
        e = do_scope_lookup_ident_immediate(&parent, mod, parent->scope,
                                            id->subs[1]->as.IDENT.name, within, TRUE, FALSE);
        if (!e) {
          fully_resolved = TRUE;
        }

        assert(parent->which == IMPORT);
        const struct node *path = parent->subs[0];
        e = do_scope_lookup(&parent, mod, mod->gctx->modules_root.scope,
                            path, within, failure_ok);
        EXCEPT_UNLESS(e, failure_ok);
      }
    }

    if (!fully_resolved) {
      e = do_scope_lookup_ident_immediate(&r, mod, parent->scope,
                                          id->subs[1]->as.IDENT.name, within, failure_ok,
                                          TRUE);
      EXCEPT_UNLESS(e, failure_ok);
    }

    if (r->which == IMPORT
        && node_module_owner_const(r) != node_module_owner_const(id)
        && !r->as.IMPORT.toplevel.is_export) {
      const char *scname = scope_name(mod, scope);
      const char *wscname = scope_name(mod, within);
      EXCEPT_PARSE(node_module_owner(scope->node), scope->node->codeloc,
                   "in scope %s: imported from scope %s: not exported",
                   scname, wscname);
      // FIXME: leaking scname.
    }
    break;
  case DIRECTDEF:
    r = id->as.DIRECTDEF.definition;
    break;
  default:
    assert(FALSE);
    return 0;
  }

  *result = r;

  return 0;
}

error scope_lookup(struct node **result, const struct module *mod,
                   const struct scope *scope, const struct node *id) {
  return do_scope_lookup(result, mod, scope, id, scope, FALSE);
}

error scope_lookup_module(struct node **result, const struct module *mod,
                          const struct node *id) {
  error e;
  struct node *parent = NULL, *r = NULL;
  struct scope *scope = mod->gctx->modules_root.scope;
  struct scope *within = scope;

  switch (id->which) {
  case IDENT:
    e = do_scope_lookup_ident(&r, mod, scope, id->as.IDENT.name, within, TRUE);
    break;
  case BIN:
    if (id->as.BIN.operator != TDOT) {
      EXCEPT_TYPE(node_module_owner_const(id), id, "malformed module path name");
    }
    e = do_scope_lookup(&parent, mod, scope, id->subs[0], within, TRUE);
    if (e) {
      break;
    }
    e = do_scope_lookup(&r, mod, parent->scope, id->subs[1], within, TRUE);
    if (e) {
      break;
    }
    break;
  default:
    assert(FALSE);
    return 0;
  }

  if (e || r->which != MODULE) {
    return EINVAL;
  }

  *result = r;

  return 0;
}

static error parse_modpath(struct module *mod, const char *raw_fn) {
  const char *fn = raw_fn;
  while (fn[0] == '/' || fn[0] == '.') {
    fn += 1;
  }

  mod->path_len = 0;
  bool must_be_last = FALSE;
  for (size_t n = 0, last = 0, p = 0; fn[p] != '\0'; ++p) {
    if (fn[p] == '_') {
      EXCEPTF(EINVAL, "module path element cannot contain '_' in '%s'", fn);
    }

    if (fn[p] == '/' || fn[p] == '.' || fn[p + 1] == '\0') {
      struct token tok;
      tok.t = TIDENT;
      tok.value = fn + last;
      tok.len = p - last;

      if (strncmp(tok.value, "module", tok.len) == 0) {
        must_be_last = TRUE;
      } else {
        mod->path[n] = idents_add(mod->gctx, &tok);
        mod->path_len += 1;

        last = p + 1;
        n += 1;
        if (n >= ARRAY_SIZE(mod->path)) {
          EXCEPTF(EINVAL, "module path '%s' has too many elements", fn);
        }
      }

      if (fn[p] == '.') {
        // Skip anything after a dot (allows for things like versioning to
        // be present in filenames or dirnames after the dot, without
        // changing the module name).
        while (fn[p] != '/' && fn[p+1] != '\0') {
          p += 1;
        }
      }

      if (must_be_last && fn[p+1] != '\0') {
        EXCEPTF(EINVAL, "module path '%s' contains the"
                " element 'module' in an illegal position", fn);
      }
    }
  }
  return 0;
}

static struct typ *typ_new_builtin(struct node *definition,
                                   enum typ_which which, size_t gen_arity,
                                   size_t fun_arity);

void globalctx_init(struct globalctx *gctx) {
  memset(gctx, 0, sizeof(*gctx));

  gctx->idents.map = calloc(1, sizeof(struct idents_map));
  idents_map_init(gctx->idents.map, 0);
  idents_map_set_delete_val(gctx->idents.map, -1);
  idents_map_set_custom_hashf(gctx->idents.map, token_hash);
  idents_map_set_custom_cmpf(gctx->idents.map, token_cmp);

  assert(ID_TBI__LAST - ID_TBI__FIRST + 2 == TBI__NUM);

  gctx->idents.count = ID__NUM;
  gctx->idents.capacity = ID__NUM;
  gctx->idents.values = calloc(ID__NUM, sizeof(char *));
  for (int i = 0; i < ID__NUM; ++i) {
    gctx->idents.values[i] = predefined_idents_strings[i];

    struct token tok;
    tok.t = TIDENT;
    tok.value = predefined_idents_strings[i];
    tok.len = strlen(predefined_idents_strings[i]);
    idents_map_set(gctx->idents.map, tok, i);

    if (i >= ID_TBI__FIRST && i <= ID_TBI__LAST) {
      struct typ *t = calloc(1, sizeof(struct typ));
      if (i < ID_TBI__FIRST_MARKER) {
        t = typ_new_builtin(NULL, TYPE_DEF, 0, 0);
      } else {
        t = typ_new_builtin(NULL, TYPE__MARKER, 0, 0);
      }

      gctx->builtin_typs[i - ID_TBI__FIRST + 1] = t;

      gctx->builtin_typs_by_name[i] = t;
    }
  }

  gctx->modules_root.scope = scope_new(&gctx->modules_root);
  gctx->modules_root.which = ROOT_OF_ALL;
}

static error module_read(struct module *mod, const char *prefix, const char *fn) {
  char *fullpath = NULL;
  if (prefix != NULL) {
    fullpath = calloc(strlen(prefix) + 1 + strlen(fn) + 1, sizeof(char));
    strcpy(fullpath, prefix);
    fullpath[strlen(prefix)] = '/';
    strcpy(fullpath + strlen(prefix) + 1, fn);
  } else {
    fullpath = calloc(strlen(fn) + 1, sizeof(char));
    strcpy(fullpath, fn);
  }
  mod->filename = fullpath;

  int fd = open(fullpath, O_RDONLY);
  if (fd < 0) {
    EXCEPTF(errno, "Cannot open module '%s'", fullpath);
  }

  struct stat st;
  memset(&st, 0, sizeof(st));
  error e = fstat(fd, &st);
  if (e < 0) {
    EXCEPTF(errno, "Cannot stat module '%s'", fullpath);
  }

  char *data = malloc(st.st_size + 1);
  ssize_t count = read(fd, data, st.st_size);
  if (count < 0) {
    EXCEPTF(errno, "Error reading module '%s'", fullpath);
  } else if (count != (ssize_t) st.st_size) {
    EXCEPTF(errno, "Reading module '%s': Partial read not supported by parser", fullpath);
  }
  data[st.st_size] = '\0';

  mod->parser.data = data;
  mod->parser.len = st.st_size;

  return 0;
}

static bool eof(struct parser *parser) {
  return parser->pos >= parser->len;
}

static error scan(struct token *tok, struct module *mod) {
  memset(tok, 0, sizeof(*tok));

  error e = lexer_scan(tok, &mod->parser);
  if (e == EINVAL) {
    EXCEPT_SYNTAX(mod, tok, "%s", mod->parser.error_message);
  } else {
    EXCEPT(e);
  }
  return 0;
}

static void back(struct module *mod, const struct token *tok) {
  lexer_back(&mod->parser, tok);
}

static error scan_expected(struct module *mod, enum token_type t) {
  struct token tok;
  error e = scan(&tok, mod);
  EXCEPT(e);

  if (tok.t != t) {
    UNEXPECTED(mod, &tok);
  }

  return 0;
}

static error scan_oneof(struct token *tok, struct module *mod, ...) {
  error e = scan(tok, mod);
  EXCEPT(e);

  va_list ap;
  va_start(ap, mod);

  while (TRUE) {
    enum token_type t = va_arg(ap, enum token_type);
    if (t == 0) {
      break;
    }
    if (tok->t == t) {
      va_end(ap);
      return 0;
    }
  }

  va_end(ap);
  UNEXPECTED(mod, tok);

  return 0;

}

struct node *node_new_subnode(const struct module *mod, struct node *node) {
  const size_t last = node->subs_count;
  node->subs_count = last + 1;
  node->subs = realloc(node->subs, node->subs_count * sizeof(struct node *));

  struct node **r = node->subs + last;
  *r = calloc(1, sizeof(**r));

  if (mod->parser.pos >= mod->parser.len) {
    // It's a node inserted after parsing.
    (*r)->codeloc = node->codeloc;
  } else {
    (*r)->codeloc = mod->parser.pos;
  }

  return *r;
}

size_t node_fun_args_count(const struct node *def) {
  assert(def->which == DEFFUN || def->which == DEFMETHOD);
  if (def->subs[def->subs_count-1]->which == BLOCK) {
    return def->subs_count-3;
  } else {
    return def->subs_count-2;
  }
}

struct node *mk_node(struct module *mod, struct node *parent, enum node_which kind) {
  struct node *n = node_new_subnode(mod, parent);
  n->which = kind;
  return n;
}

static error p_expr(struct node *node, struct module *mod, uint32_t parent_op);
static error p_block(struct node *node, struct module *mod);

static error p_ident(struct node *node, struct module *mod) {
  struct token tok;
  error e = scan_oneof(&tok, mod, TIDENT, 0);
  EXCEPT(e);

  node->which = IDENT;
  node->as.IDENT.name = idents_add(mod->gctx, &tok);

  return 0;
}

static error p_number(struct node *node, struct module *mod) {
  struct token tok;
  error e = scan_oneof(&tok, mod, TNUMBER, 0);
  EXCEPT(e);

  char *cpy = malloc(tok.len + 1);
  memcpy(cpy, tok.value, tok.len);
  cpy[tok.len] = '\0';

  node->which = NUMBER;
  node->as.NUMBER.value = cpy;

  return 0;
}

static error p_string(struct node *node, struct module *mod) {
  struct token tok;
  error e = scan_oneof(&tok, mod, TSTRING, 0);
  EXCEPT(e);

  char *cpy = malloc(tok.len + 1);
  memcpy(cpy, tok.value, tok.len);
  cpy[tok.len] = '\0';

  node->which = STRING;
  node->as.STRING.value = cpy;

  return 0;
}

static error p_typeexpr(struct node *node, struct module *mod) {
  error e = p_expr(node, mod, T__CALL);
  EXCEPT(e);
  return 0;
}

static error p_typeconstraint(struct node *node, struct module *mod) {
  node->which = TYPECONSTRAINT;
  error e = p_ident(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  e = scan_expected(mod, TCOLON);
  EXCEPT(e);

  e = p_typeexpr(node_new_subnode(mod, node), mod);
  EXCEPT(e);
  return 0;
}

static error p_deffield(struct node *node, struct module *mod) {
  node->which = DEFFIELD;
  error e = p_ident(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  e = scan_expected(mod, TCOLON);
  EXCEPT(e);

  e = p_typeexpr(node_new_subnode(mod, node), mod);
  EXCEPT(e);
  return 0;
}

static error p_defchoice(struct node *node, struct module *mod) {
  node->which = DEFCHOICE;
  error e = p_ident(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  struct token tok;
  e = scan(&tok, mod);
  EXCEPT(e);

  if (tok.t == TASSIGN) {
    node->as.DEFCHOICE.has_value = TRUE;

    e = p_expr(node_new_subnode(mod, node), mod, T__NONE);
    EXCEPT(e);

    e = scan(&tok, mod);
    EXCEPT(e);
  }

  if (tok.t == TSOB) {
    e = p_typeexpr(node_new_subnode(mod, node), mod);
    EXCEPT(e);
    e = scan_expected(mod, TEOB);
    EXCEPT(e);
  } else {
    back(mod, &tok);
  }
  return 0;
}

static error p_expr_unary(struct node *node, struct module *mod) {
  struct token tok;
  error e = scan(&tok, mod);
  EXCEPT(e);

  uint32_t op;
  switch (tok.t) {
  case TMINUS:
    op = TUMINUS;
    break;
  case TPLUS:
    op = TUPLUS;
    break;
  default:
    op = tok.t;
    break;
  }

  node->which = UN;
  node->as.UN.operator = op;

  e = p_expr(node_new_subnode(mod, node), mod, op);
  EXCEPT(e);

  return 0;
}

static error p_expr_init(struct node *node, const struct node *first,
                         struct module *mod) {
  node->which = INIT;

  error e = scan_expected(mod, TLINIT);
  EXCEPT(e);

  struct token tok;

  struct node *fst = node_new_subnode(mod, node);
  *fst = *first;

  while (TRUE) {
    e = scan(&tok, mod);
    EXCEPT(e);

    if (tok.t == TRINIT) {
      return 0;
    }
    back(mod, &tok);

    e = p_ident(node_new_subnode(mod, node), mod);
    EXCEPT(e);

    e = scan_expected(mod, TASSIGN);
    EXCEPT(e);

    e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
    EXCEPT(e);
  }
}

static error p_expr_tuple(struct node *node, const struct node *first,
                          struct module *mod) {
  node->which = TUPLE;
  error e;
  struct token tok;

  struct node *fst = node_new_subnode(mod, node);
  *fst = *first;

  while (TRUE) {
    e = scan(&tok, mod);
    EXCEPT(e);

    if (tok.t != TCOMMA) {
      back(mod, &tok);
      return 0;
    }

    e = p_expr(node_new_subnode(mod, node), mod, TCOMMA);
    EXCEPT(e);
  }
}

static error p_expr_call(struct node *node, const struct node *first,
                         struct module *mod) {
  node->which = CALL;
  error e;
  struct token tok;

  struct node *function = node_new_subnode(mod, node);
  *function = *first;

  while (TRUE) {
    e = scan(&tok, mod);
    EXCEPT(e);
    back(mod, &tok);

    if (expr_terminators[tok.t]) {
      return 0;
    }

    e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
    EXCEPT(e);
  }
}

static error p_expr_binary(struct node *node, const struct node *first,
                           struct module *mod) {
  struct token tok;
  error e = scan(&tok, mod);
  EXCEPT(e);

  assert(tok.t != TCOMMA);
  node->which = BIN;
  node->as.BIN.operator = tok.t;

  struct node *left = node_new_subnode(mod, node);
  *left = *first;

  e = p_expr(node_new_subnode(mod, node), mod, tok.t);
  EXCEPT(e);

  return 0;
}

static error p_expr(struct node *node, struct module *mod, uint32_t parent_op) {
  assert(parent_op < TOKEN__NUM && IS_OP(parent_op));

  error e;
  struct token tok;
  bool first_iteration = TRUE;
  bool topmost = parent_op == T__NONE;

  e = scan(&tok, mod);
  EXCEPT(e);

  struct node first, second;
  memset(&first, 0, sizeof(first));
  memset(&second, 0, sizeof(second));
  first.codeloc = mod->parser.pos;
  second.codeloc = mod->parser.pos;

  if (tok.t == TLPAR) {
    e = p_expr(&first, mod, T__NONE);
    EXCEPT(e);
    e = scan_expected(mod, TRPAR);
    EXCEPT(e);
  } else {
    back(mod, &tok);

    if (tok.t == Tnull) {
      e = scan(&tok, mod);
      EXCEPT(e);
      first.which = NUL;
    } else if (tok.t == TIDENT) {
      e = p_ident(&first, mod);
    } else if (tok.t == TNUMBER) {
      e = p_number(&first, mod);
    } else if (tok.t == TSTRING) {
      e = p_string(&first, mod);
    } else if ((IS_OP(tok.t) && OP_UNARY(tok.t))
               || tok.t == TMINUS || tok.t == TPLUS) { // Unary versions.
      e = p_expr_unary(&first, mod);
    } else {
      UNEXPECTED(mod, &tok);
    }
    EXCEPT(e);
  }

shift:
  e = scan(&tok, mod);
  EXCEPT(e);
  back(mod, &tok);

  if (IS_OP(tok.t) && OP_BINARY(tok.t)
      && OP_PREC(tok.t) == OP_PREC(parent_op)
      && OP_ASSOC(tok.t) == ASSOC_NON) {
    EXCEPT_SYNTAX(mod, &tok, "Operator '%.*s' is non-associative", (int)tok.len, tok.value);
  }

  if (first_iteration) {
    first_iteration = FALSE;
  } else {
    first = second;
    memset(&second, 0, sizeof(second));
    second.codeloc = mod->parser.pos;
  }

  if (expr_terminators[tok.t]) {
    goto done;
  } else if (IS_OP(tok.t) && OP_BINARY(tok.t)) {
    if (tok.t == TCOMMA) {
      if (OP_PREC(tok.t) < OP_PREC(parent_op)
          || topmost) {
        e = p_expr_tuple(&second, &first, mod);
        EXCEPT(e);

        goto shift;
      } else {
        goto done;
      }
    } else if (tok.t == TLINIT) {
      if (OP_PREC(tok.t) < OP_PREC(parent_op)
          || topmost) {
        e = p_expr_init(&second, &first, mod);
        EXCEPT(e);

        goto shift;
      } else {
        goto done;
      }
    } else if (OP_PREC(tok.t) < OP_PREC(parent_op)
        || (OP_PREC(tok.t) == OP_PREC(parent_op)
            && OP_ASSOC(tok.t) == ASSOC_RIGHT)
        || topmost) {
      e = p_expr_binary(&second, &first, mod);
      EXCEPT(e);

      if (topmost) {
        parent_op = tok.t;
      }

      goto shift;
    } else {
      goto done;
    }
  } else if (OP_PREC(T__CALL) < OP_PREC(parent_op) || topmost) {
    e = p_expr_call(&second, &first, mod);
    if (topmost) {
      parent_op = T__CALL;
    }
    EXCEPT(e);

    goto shift;
  } else {
    goto done;
  }

done:
  *node = first;
  return 0;
}

static error p_return(struct node *node, struct module *mod) {
  node->which = RETURN;

  struct token tok;
  error e = scan(&tok, mod);
  EXCEPT(e);
  back(mod, &tok);

  if (!expr_terminators[tok.t]) {
    e = p_expr(node_new_subnode(mod, node), mod, T__NONE);
    EXCEPT(e);
  }
  return 0;
}

static error p_except(struct node *node, struct module *mod) {
  node->which = EXCEP;
  struct token tok;
  error e = scan(&tok, mod);
  EXCEPT(e);
  back(mod, &tok);

  if (!expr_terminators[tok.t]) {
    e = p_expr(node_new_subnode(mod, node), mod, T__NONE);
    EXCEPT(e);
  }
  return 0;
}

static error p_pattern(struct node *node, struct module *mod) {
  error e = p_ident(node, mod);
  EXCEPT(e);
  return 0;
}

static error p_defname(struct node *node, struct module *mod) {
  node->which = DEFNAME;

  error e = p_pattern(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  e = scan_expected(mod, TASSIGN);
  EXCEPT(e);

  e = p_expr(node_new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);

  struct token tok;
  e = scan(&tok, mod);
  EXCEPT(e);
  if (tok.t != TSOB) {
    back(mod, &tok);
    return 0;
  }

  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_let(struct node *node, struct module *mod, const struct toplevel *toplevel) {
  node->which = LET;
  if (toplevel != NULL) {
    node->as.LET.toplevel = *toplevel;
  }

  return p_defname(node_new_subnode(mod, node), mod);
}

static error p_if(struct node *node, struct module *mod) {
  node->which = IF;

  struct token eol;

  error e = p_expr(node_new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);
  e = scan(&eol, mod);
  EXCEPT(e);
  if (eol.t != TEOL) {
    UNEXPECTED(mod, &eol);
  }

  struct token tok;

again:
  e = scan(&tok, mod);
  EXCEPT(e);

  switch (tok.t) {
  case Telif:
    e = p_expr(node_new_subnode(mod, node), mod, T__NONE);
    EXCEPT(e);
    e = scan_expected(mod, TSOB);
    EXCEPT(e);
    e = p_block(node_new_subnode(mod, node), mod);
    EXCEPT(e);
    e = scan(&eol, mod);
    EXCEPT(e);
    if (eol.t != TEOL) {
      UNEXPECTED(mod, &eol);
    }
    goto again;
  case Telse:
    e = scan_expected(mod, TSOB);
    EXCEPT(e);
    e = p_block(node_new_subnode(mod, node), mod);
    EXCEPT(e);
    e = scan(&tok, mod);
    EXCEPT(e);
    if (eol.t != TEOL) {
      UNEXPECTED(mod, &eol);
    }
    break;
  default:
    back(mod, &tok);
    break;
  }

  // Need to reinject the eol after the last block.
  mod->parser.inject_eol_after_eob = TRUE;

  return 0;
}

static error p_for(struct node *node, struct module *mod) {
  node->which = FOR;

  error e = p_pattern(node_new_subnode(mod, node), mod);
  EXCEPT(e);
  e = scan_expected(mod, Tin);
  EXCEPT(e);
  e = p_expr(node_new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_while(struct node *node, struct module *mod) {
  node->which = WHILE;

  error e = p_expr(node_new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_try(struct node *node, struct module *mod) {
  node->which = TRY;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);
  e = scan_expected(mod, TEOL);
  EXCEPT(e);
  e = scan_expected(mod, Tcatch);
  EXCEPT(e);

  e = p_expr(node_new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);

  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_break(struct node *node, struct module *mod) {
  node->which = BREAK;
  return 0;
}

static error p_continue(struct node *node, struct module *mod) {
  node->which = CONTINUE;
  return 0;
}

static error p_pass(struct node *node, struct module *mod) {
  node->which = PASS;
  return 0;
}

static error p_match(struct node *node, struct module *mod) {
  node->which = MATCH;

  error e = p_expr(node_new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TEOL);
  EXCEPT(e);

  struct token tok;
again:
  e = scan(&tok, mod);
  EXCEPT(e);
  if (tok.t != TBWOR) {
    back(mod, &tok);
    mod->parser.inject_eol_after_eob = TRUE;
    return 0;
  }

  e = p_expr(node_new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);
  e = scan_expected(mod, TEOL);
  EXCEPT(e);
  goto again;
}

static error p_pre(struct node *node, struct module *mod) {
  node->which = PRE;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_post(struct node *node, struct module *mod) {
  node->which = POST;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_invariant(struct node *node, struct module *mod) {
  node->which = INVARIANT;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_example(struct node *node, struct module *mod) {
  node->which = EXAMPLE;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_statement(struct node *node, struct module *mod) {
  error e;
  struct token tok;

  e = scan(&tok, mod);
  EXCEPT(e);

  switch (tok.t) {
  case Treturn:
    e = p_return(node, mod);
    break;
  case Texcept:
    e = p_except(node, mod);
    break;
  case Tlet:
    e = p_let(node, mod, NULL);
    break;
  case Tif:
    e = p_if(node, mod);
    break;
  case Tfor:
    e = p_for(node, mod);
    break;
  case Twhile:
    e = p_while(node, mod);
    break;
  case Ttry:
    e = p_try(node, mod);
    break;
  case Tbreak:
    e = p_break(node, mod);
    break;
  case Tcontinue:
    e = p_continue(node, mod);
    break;
  case Tpass:
    e = p_pass(node, mod);
    break;
  case Tmatch:
    e = p_match(node, mod);
    break;
  case Tpre:
    e = p_pre(node, mod);
    break;
  case Tpost:
    e = p_post(node, mod);
    break;
  case Tinvariant:
    e = p_invariant(node, mod);
    break;
  case Texample:
    e = p_example(node, mod);
    break;
  case TLPAR:
  case TIDENT:
    back(mod, &tok);
    e = p_expr(node, mod, T__NONE);
    break;
  default:
    UNEXPECTED(mod, &tok);
  }
  EXCEPT(e);

  return 0;
}

static error p_block(struct node *node, struct module *mod) {
  node->which = BLOCK;
  error e;
  struct token tok;
  bool first = TRUE;

  e = scan(&tok, mod);
  EXCEPT(e);

again:
  if (tok.t == TEOB) {
    if (first) {
      EXCEPT_SYNTAX(mod, &tok, "block cannot be empty (use 'pass' instead)");;
    } else {
      return 0;
    }
  } else {
    back(mod, &tok);
    e = p_statement(node_new_subnode(mod, node), mod);
    EXCEPT(e);

    e = scan_oneof(&tok, mod, TEOL, TEOB, 0);
    EXCEPT(e);

    if (tok.t == TEOB) {
      return 0;
    }
  }

  first = FALSE;
  e = scan(&tok, mod);
  EXCEPT(e);

  goto again;
}

static error p_deffun(struct node *node, struct module *mod, const struct toplevel *toplevel,
                      enum node_which fun_or_method) {
  error e;
  struct token tok;
  struct toplevel *node_toplevel;

  node->which = fun_or_method;
  switch (fun_or_method) {
  case DEFFUN:
    node->as.DEFFUN.toplevel = *toplevel;
    node_toplevel = &node->as.DEFFUN.toplevel;
    break;
  case DEFMETHOD:
    node->as.DEFMETHOD.toplevel = *toplevel;
    node->as.DEFMETHOD.access = TREFDOT;
    node_toplevel = &node->as.DEFMETHOD.toplevel;

    e = scan(&tok, mod);
    EXCEPT(e);
    switch (tok.t) {
    case TBANG:
      node->as.DEFMETHOD.access = TREFBANG;
      break;
    case TSHARP:
      node->as.DEFMETHOD.access = TREFSHARP;
      break;
    default:
      back(mod, &tok);
      break;
    }
    break;
  default:
    assert(FALSE);
  }

  e = p_ident(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  if (fun_or_method == DEFMETHOD) {
    struct node *arg = mk_node(mod, node, TYPECONSTRAINT);
    arg->as.TYPECONSTRAINT.is_arg = TRUE;
    struct node *name = mk_node(mod, arg, IDENT);
    name->as.IDENT.name = ID_SELF;

    struct node *ref = mk_node(mod, arg, UN);
    ref->as.UN.operator = node->as.DEFMETHOD.access;
    struct node *typename = mk_node(mod, ref, IDENT);
    typename->as.IDENT.name = ID_THIS;
  }

again:
  e = scan_oneof(&tok, mod, TASSIGN, TIDENT, 0);
  EXCEPT(e);

  switch (tok.t) {
  case TASSIGN:
    goto retval;
  case TIDENT:
    back(mod, &tok);

    struct node *arg = node_new_subnode(mod, node);
    e = p_typeconstraint(arg, mod);
    EXCEPT(e);

    assert(arg->which == TYPECONSTRAINT);
    arg->as.TYPECONSTRAINT.is_arg = TRUE;

    goto again;
  default:
    assert(FALSE);
  }

retval:
  e = p_expr(node_new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);

  e = scan_oneof(&tok, mod, TEOL, TSOB, TEOB, 0);
  EXCEPT(e);

  if (tok.t == TEOL || tok.t == TEOB) {
    back(mod, &tok);
    node_toplevel->is_prototype = TRUE;
    return 0;
  }

  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_isalist(struct node *node, struct module *mod) {
  node->which = ISALIST;

  error e;
  struct token tok;

again:
  e = scan(&tok, mod);
  EXCEPT(e);
  back(mod, &tok);

  switch (tok.t) {
  case TEOL:
  case TSOB:
    return 0;
  default:
    e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
    EXCEPT(e);
    goto again;
  }
}

static error p_delegate(struct node *node, struct module *mod) {
  node->which = DELEGATE;

  error e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
  EXCEPT(e);

  struct token tok;

again:
  e = scan(&tok, mod);
  EXCEPT(e);
  back(mod, &tok);

  if (tok.t == TEOL) {
    return 0;
  }

  e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
  EXCEPT(e);

  goto again;
}

static error p_deftype_statement(struct node *node, struct module *mod) {
  error e;
  struct token tok;
  struct toplevel toplevel;
  memset(&toplevel, 0, sizeof(toplevel));

  e = scan(&tok, mod);
  EXCEPT(e);

  switch (tok.t) {
  case Tlet:
    e = p_let(node, mod, &toplevel);
    break;
  case Tdelegate:
    e = p_delegate(node, mod);
    break;
  case Tinvariant:
    e = p_invariant(node, mod);
    break;
  case Texample:
    e = p_example(node, mod);
    break;
  case TIDENT:
    back(mod, &tok);
    e = p_deffield(node, mod);
    break;
  case TBWOR:
    e = p_defchoice(node, mod);
    break;
  default:
    UNEXPECTED(mod, &tok);
  }
  EXCEPT(e);

  return 0;
}

static error p_deftype_block(struct node *node, struct module *mod) {
  error e;
  struct token tok;
  bool first = TRUE;

  e = scan(&tok, mod);
  EXCEPT(e);

again:
  if (tok.t == TEOB) {
    if (first) {
      EXCEPT_SYNTAX(mod, &tok, "block cannot be empty (use 'pass' instead)");;
    } else {
      return 0;
    }
  } else {
    back(mod, &tok);
    e = p_deftype_statement(node_new_subnode(mod, node), mod);
    EXCEPT(e);

    e = scan_oneof(&tok, mod, TEOL, TEOB, 0);
    EXCEPT(e);

    if (tok.t == TEOB) {
      return 0;
    }
  }

  first = FALSE;
  e = scan(&tok, mod);
  EXCEPT(e);

  goto again;
}

static error p_deftype(struct node *node, struct module *mod, const struct toplevel *toplevel) {
  node->which = DEFTYPE;
  node->as.DEFTYPE.toplevel = *toplevel;

  error e = p_ident(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  e = scan_expected(mod, TASSIGN);
  EXCEPT(e);

  e = p_isalist(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  struct token tok;
  e = scan_oneof(&tok, mod, TEOL, TSOB, 0);
  EXCEPT(e);

  if (tok.t == TEOL) {
    back(mod, &tok);
    node->as.DEFTYPE.toplevel.is_prototype = TRUE;
    return 0;
  }

  e = p_deftype_block(node, mod);
  EXCEPT(e);

  return 0;
}

static error p_defintf_statement(struct node *node, struct module *mod) {
  error e;
  struct token tok;
  struct toplevel toplevel;
  memset(&toplevel, 0, sizeof(toplevel));

  e = scan(&tok, mod);
  EXCEPT(e);

  switch (tok.t) {
  case Tfun:
    e = p_deffun(node, mod, &toplevel, DEFFUN);
    break;
  case Tmethod:
    e = p_deffun(node, mod, &toplevel, DEFMETHOD);
    break;
  case Tlet:
    e = p_let(node, mod, &toplevel);
    break;
  case Tinvariant:
    e = p_invariant(node, mod);
    break;
  case Texample:
    e = p_example(node, mod);
    break;
  case TIDENT:
    back(mod, &tok);
    e = p_deffield(node, mod);
    break;
  default:
    UNEXPECTED(mod, &tok);
  }
  EXCEPT(e);

  return 0;
}

static error p_defintf_block(struct node *node, struct module *mod) {
  error e;
  struct token tok;
  bool first = TRUE;

  e = scan(&tok, mod);
  EXCEPT(e);

again:
  if (tok.t == TEOB) {
    if (first) {
      EXCEPT_SYNTAX(mod, &tok, "block cannot be empty (use 'pass' instead)");;
    } else {
      return 0;
    }
  } else {
    back(mod, &tok);
    e = p_defintf_statement(node_new_subnode(mod, node), mod);
    EXCEPT(e);

    e = scan_oneof(&tok, mod, TEOL, TEOB, 0);
    EXCEPT(e);

    if (tok.t == TEOB) {
      return 0;
    }
  }

  first = FALSE;
  e = scan(&tok, mod);
  EXCEPT(e);

  goto again;
}

static error p_defintf(struct node *node, struct module *mod, const struct toplevel *toplevel) {
  node->which = DEFINTF;
  node->as.DEFINTF.toplevel = *toplevel;

  struct token tok;
  error e = p_ident(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  e = scan_expected(mod, TASSIGN);
  EXCEPT(e);

  e = p_isalist(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  e = scan_oneof(&tok, mod, TEOL, TSOB, 0);
  EXCEPT(e);

  if (tok.t == TEOL) {
    back(mod, &tok);
    node->as.DEFINTF.toplevel.is_prototype = TRUE;
    return 0;
  }

  e = p_defintf_block(node, mod);
  EXCEPT(e);

  return 0;
}

void node_deepcopy(struct module *mod, struct node *dst,
                   const struct node *src) {
  dst->which = src->which;
  memcpy(&dst->as, &src->as, sizeof(&dst->as));

  for (size_t s = 0; s < src->subs_count; ++s) {
    struct node *cpy = node_new_subnode(mod, dst);
    node_deepcopy(mod, cpy, src->subs[s]);
  }
}

void copy_and_extend_import_path(struct module *mod, struct node *imported,
                                 const struct node *import, const struct token *tok) {
  struct node *n = node_new_subnode(mod, imported);
  n->which = BIN;
  n->as.BIN.operator = TDOT;

  const struct node *path = import->subs[0];
  node_deepcopy(mod, node_new_subnode(mod, n), path);

  struct node *i = node_new_subnode(mod, n);
  i->which = IDENT;
  i->as.IDENT.name = idents_add(mod->gctx, tok);
}

static error p_import(struct node *node, struct module *mod, const struct toplevel *toplevel,
                      bool from) {
  node->which = IMPORT;
  node->as.IMPORT.toplevel = *toplevel;

  error e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
  EXCEPT(e);

  int import_export_count = 0;
  int inline_count = 0;
  int ident_count = 0;
  struct token tok;
again:
  e = scan(&tok, mod);
  EXCEPT(e);

  if (tok.t == Tinline) {
    if (inline_count > 0 || import_export_count > 0 || ident_count > 0) {
      UNEXPECTED(mod, &tok);
    }
    inline_count += 1;
    node->as.IMPORT.toplevel.is_inline = TRUE;
    goto again;
  } else if (tok.t == Timport || tok.t == Texport) {
    if (!from || import_export_count > 0 || ident_count > 0) {
      UNEXPECTED(mod, &tok);
    }
    import_export_count += 1;
    node->as.IMPORT.toplevel.is_export = tok.t == Texport;
    goto again;
  } else if (tok.t == TTIMES) {
    if (ident_count > 0) {
      UNEXPECTED(mod, &tok);
    }
    node->as.IMPORT.is_all = TRUE;
    return 0;
  } else if (tok.t == TIDENT) {
    ident_count += 1;
    struct node *imported = node_new_subnode(mod, node);
    imported->which = IMPORT;
    imported->as.IMPORT.toplevel.is_export = toplevel->is_export;

    copy_and_extend_import_path(mod, imported, node, &tok);

    goto again;
  } else {
    back(mod, &tok);
    return 0;
  }
}

static error p_toplevel(struct module *mod) {
  struct node *node = node_new_subnode(mod, mod->root);

  struct toplevel toplevel;
  memset(&toplevel, 0, sizeof(toplevel));

  bool is_scoped = FALSE;
  error e;
  struct token tok;
again:
  e = scan(&tok, mod);
  EXCEPT(e);

  if (is_scoped && tok.t != Tmethod && tok.t != Tfun) {
    UNEXPECTED(mod, &tok);
  }

  switch (tok.t) {
  case Ttype:
    e = p_deftype(node, mod, &toplevel);
    break;
  case Texport:
    toplevel.is_export = TRUE;
    goto again;
  case Textern:
    toplevel.is_extern = TRUE;
    goto again;
  case Tinline:
    toplevel.is_inline = TRUE;
    goto again;
  case Tfun:
    e = p_deffun(node, mod, &toplevel, DEFFUN);
    break;
  case Tmethod:
    if (!is_scoped) {
      UNEXPECTED(mod, &tok);
    }
    e = p_deffun(node, mod, &toplevel, DEFMETHOD);
    break;
  case Tintf:
    e = p_defintf(node, mod, &toplevel);
    break;
  case Tlet:
    e = p_let(node, mod, &toplevel);
    break;
  case TIDENT:
    toplevel.scope_name = idents_add(mod->gctx, &tok);
    is_scoped = TRUE;
    goto again;
  case Tfrom:
  case Timport:
    e = p_import(node, mod, &toplevel, tok.t == Tfrom);
    break;
  default:
    EXCEPT_SYNTAX(mod, &tok, "malformed top-level statement at '%.*s'", (int)tok.len, tok.value);
    break;
  }

  EXCEPT(e);
  return 0;
}

static void rec_subnode_counts(struct node *node,
                               size_t *node_count, size_t *sub_count) {
  size_t n;
  for (n = 0; n < node->subs_count; ++n) {
    *sub_count += 1;
    rec_subnode_counts(node->subs[n], node_count, sub_count);
  }

  *node_count += !!n;
}

__attribute__((unused))
static float subnode_count_avg(struct module *mod) {
  size_t sub_count = 0, node_count = 0;
  rec_subnode_counts(mod->root, &node_count, &sub_count);
  return (float) sub_count / node_count;
}

static error module_parse(struct module *mod) {
  error e;

  do {
    e = p_toplevel(mod);
    EXCEPT(e);
    e = scan_expected(mod, TEOL);
    EXCEPT(e);
  } while (!eof(&mod->parser));

  return 0;
}

static void module_init(struct globalctx *gctx, struct module *mod) {
  memset(mod, 0, sizeof(*mod));

  mod->gctx = gctx;
}

static error register_module(struct node **parent,
                             struct globalctx *gctx, struct module *mod,
                             const char *filename) {
  const size_t last = mod->path_len - 1;
  struct node *root = &gctx->modules_root;

  for (size_t p = 0; p <= last; ++p) {
    ident i = mod->path[p];
    struct node *m = NULL;
    error e = scope_lookup_ident(&m, mod, root->scope, i, TRUE);
    if (e == EINVAL) {
      m = node_new_subnode(mod, root);
      m->which = MODULE;
      m->as.MODULE.name = i;
      m->as.MODULE.is_placeholder = p != last;
      m->as.MODULE.mod = p == last ? mod : NULL;
      m->scope = scope_new(m);
      m->scope->parent = root->scope;
      m->typ = typ_lookup_builtin(mod, TBI_VOID);

      e = scope_define_ident(mod, root->scope, i, m);
      EXCEPT(e);
    } else if (e) {
      // Repeat bound-to-fail lookup to get the error message right.
      e = scope_lookup_ident(&m, mod, root->scope, i, FALSE);
      EXCEPT(e);
    } else {
      if (p == last) {
        assert(m->which == MODULE);
        if (!m->as.MODULE.is_placeholder) {
          EXCEPTF(EINVAL, "Cannot load_module module '%s' more than once",
                  filename);
        } else {
          assert(mod->root->which == MODULE);
          mod->root->as.MODULE.mod = mod;

          for (size_t s = 0; s < m->subs_count; ++s) {
            struct node *to_save = m->subs[s];
            assert(to_save->which == MODULE);
            e = scope_define_ident(mod, mod->root->scope,
                                   to_save->as.MODULE.name, to_save);
            EXCEPT(e);
          }

          // Accepts to overwrite because it is a placeholder.
          e = scope_define_ident(mod, root->scope, i, mod->root);
          EXCEPT(e);
        }
      }
    }

    *parent = root;
    root = m;
  }

  mod->root = root;
  assert(mod->root->which == MODULE);
  mod->root->as.MODULE.mod = mod;

  return 0;
}

error module_open(struct globalctx *gctx, struct module *mod,
                  const char *prefix, const char *fn) {
  module_init(gctx, mod);

  error e = parse_modpath(mod, fn);
  EXCEPT(e);

  struct node *parent = NULL;
  e = register_module(&parent, gctx, mod, fn);
  EXCEPT(e);

  e = module_read(mod, prefix, fn);
  EXCEPT(e);

  if (mod->path_len >= 1) {
    mod->root->as.MODULE.name = mod->path[mod->path_len - 1];
  } else {
    mod->root->as.MODULE.name = ID_ANONYMOUS;
  }

  e = module_parse(mod);
  EXCEPT(e);

  return 0;
}

ident gensym(struct module *mod) {
  size_t g = mod->next_gensym;
  mod->next_gensym += 1;

  char name[64];
  int cnt = snprintf(name, ARRAY_SIZE(name), "__gensym%zx", g);
  assert(cnt < ARRAY_SIZE(name));

  struct token tok;
  tok.t = TIDENT;
  tok.value = name;
  tok.len = cnt;

  return idents_add(mod->gctx, &tok);
}

void module_needs_instance(struct module *mod, struct typ *typ) {
}

void module_return_set(struct module *mod, struct node *return_node) {
  mod->return_node = return_node;
}

struct node *module_return_get(struct module *mod) {
  return mod->return_node;
}

void module_excepts_open_try(struct module *mod) {
  mod->trys_count += 1;
  mod->trys = realloc(mod->trys, mod->trys_count * sizeof(*mod->trys));
  memset(mod->trys + mod->trys_count - 1, 0, sizeof(*mod->trys));
}

void module_excepts_push(struct module *mod, struct node *return_node) {
  struct try_excepts *t = &mod->trys[mod->trys_count - 1];
  t->count += 1;
  t->excepts = realloc(t->excepts, t->count * sizeof(*t->excepts));
  memset(t->excepts + t->count - 1, 0, sizeof(*t->excepts));
  t->excepts[t->count - 1] = return_node;
}

void module_excepts_close_try(struct module *mod) {
  assert(mod->trys_count > 0);
  free(mod->trys[mod->trys_count - 1].excepts);
  mod->trys_count -= 1;
}

static struct typ *typ_new_builtin(struct node *definition,
                                   enum typ_which which, size_t gen_arity,
                                   size_t fun_arity) {
  struct typ *r = calloc(1, sizeof(struct typ));
  r->definition = definition;
  r->which = which;
  r->gen_arity = gen_arity;
  if (gen_arity > 0) {
    r->gen_args = calloc(gen_arity, sizeof(struct typ *));
  }
  r->fun_arity = fun_arity;
  if (fun_arity > 0) {
    r->fun_args = calloc(fun_arity + 1, sizeof(struct typ *));
  }

  return r;
}

struct typ *typ_new(struct node *definition,
                    enum typ_which which, size_t gen_arity,
                    size_t fun_arity) {
  assert(which == TYPE__MARKER || definition != NULL);
  return typ_new_builtin(definition, which, gen_arity, fun_arity);
}

// Return value must be freed by caller.
char *typ_name(const struct module *mod, const struct typ *t) {
  assert(t->which != TYPE__MARKER);
  if (t->definition != NULL) {
    return scope_name(mod, t->definition->scope);
  } else {
    for (size_t n = ID_TBI__FIRST; n < ID_TBI__LAST; ++n) {
      if (mod->gctx->builtin_typs_by_name[n] == t) {
        return strdup(predefined_idents_strings[n]);
      }
    }
  }
  assert(FALSE);
  return NULL;
}

struct typ *typ_lookup_builtin(const struct module *mod, enum typ_builtin id) {
  return mod->gctx->builtin_typs[id];
}

error typ_check(const struct module *mod, const struct node *for_error,
                const struct typ *a, const struct typ *constraint) {
  if (constraint == a) {
    return 0;
  }

  if (a == typ_lookup_builtin(mod, TBI_LITERALS_INTEGER)) {
    if (constraint == typ_lookup_builtin(mod, TBI_U8)
        || constraint == typ_lookup_builtin(mod, TBI_U16)
        || constraint == typ_lookup_builtin(mod, TBI_U32)
        || constraint == typ_lookup_builtin(mod, TBI_U64)
        || constraint == typ_lookup_builtin(mod, TBI_I8)
        || constraint == typ_lookup_builtin(mod, TBI_I16)
        || constraint == typ_lookup_builtin(mod, TBI_I32)
        || constraint == typ_lookup_builtin(mod, TBI_I64)
        || constraint == typ_lookup_builtin(mod, TBI_SIZE)
        || constraint == typ_lookup_builtin(mod, TBI_SSIZE)) {
      return 0;
    }
  }

  if (a == typ_lookup_builtin(mod, TBI_LITERALS_NULL)) {
    if (constraint == typ_lookup_builtin(mod, TBI_NREF)
        || constraint == typ_lookup_builtin(mod, TBI_NMREF)
        || constraint == typ_lookup_builtin(mod, TBI_NMMREF)
        || constraint == typ_lookup_builtin(mod, TBI_DYN)) {
      return 0;
    }
  }

  EXCEPT_TYPE(node_module_owner_const(for_error), for_error,
              "'%s' not compatible with constraint '%s'",
              typ_name(mod, a), typ_name(mod, constraint));
  // FIXME: leaking typ_names.
}

error typ_check_numeric(const struct module *mod, const struct node *for_error,
                        const struct typ *a) {
  if (a == typ_lookup_builtin(mod, TBI_U8)
      || a == typ_lookup_builtin(mod, TBI_U16)
      || a == typ_lookup_builtin(mod, TBI_U32)
      || a == typ_lookup_builtin(mod, TBI_U64)
      || a == typ_lookup_builtin(mod, TBI_I8)
      || a == typ_lookup_builtin(mod, TBI_I16)
      || a == typ_lookup_builtin(mod, TBI_I32)
      || a == typ_lookup_builtin(mod, TBI_I64)
      || a == typ_lookup_builtin(mod, TBI_SIZE)
      || a == typ_lookup_builtin(mod, TBI_SSIZE)
      || a == typ_lookup_builtin(mod, TBI_LITERALS_INTEGER)) {
    return 0;
  }

  EXCEPT_TYPE(node_module_owner_const(for_error), for_error, "'%s' type is not numeric",
              typ_name(mod, a));
  // FIXME: leaking typ_names.
}

error typ_is_reference(const struct module *mod, const struct typ *a) {
  return a == typ_lookup_builtin(mod, TBI_REF)
    || a == typ_lookup_builtin(mod, TBI_MREF)
    || a == typ_lookup_builtin(mod, TBI_MMREF);
}

error typ_check_reference(const struct module *mod, const struct node *for_error,
                          const struct typ *a) {
  if (typ_is_reference(mod, a)) {
    return 0;
  }

  EXCEPT_TYPE(node_module_owner_const(for_error), for_error, "'%s' type is not numeric",
              typ_name(mod, a));
  // FIXME: leaking typ_names.
}

bool typ_is_concrete(const struct module *mod, const struct typ *a) {
  if (a == typ_lookup_builtin(mod, TBI_LITERALS_NULL)
      || a == typ_lookup_builtin(mod, TBI_LITERALS_INTEGER)) {
    return FALSE;
  }

  if (a->which == TYPE_TUPLE) {
    for (size_t n = 0; n < a->gen_arity; ++n) {
      if (!typ_is_concrete(mod, a->gen_args[n])) {
        return FALSE;
      }
    }
  }

  return TRUE;
}

bool typ_is_builtin(const struct module *mod, const struct typ *t) {
  for (size_t n = 0; n < TBI__NUM; ++n) {
    if (t == mod->gctx->builtin_typs[n]) {
      return TRUE;
    }
  }
  return FALSE;
}

error typ_unify(struct typ **u, const struct module *mod, const struct node *for_error,
                struct typ *a, struct typ *b) {
  error e = typ_check(mod, for_error, a, b);
  EXCEPT(e);

  // Choose the concrete typ if there is one.
  if (a->which == TYPE_TUPLE) {
    for (size_t n = 0; n < a->gen_arity; ++n) {
      if (typ_is_concrete(mod, a->gen_args[n])) {
        (*u)->gen_args[n] = a->gen_args[n];
      } else {
        (*u)->gen_args[n] = b->gen_args[n];
      }
    }
  } else {
    if (typ_is_concrete(mod, a)) {
      *u = a;
    } else {
      *u = b;
    }
  }

  return 0;
}

bool typ_isa(const struct module *mod, const struct typ *a, const struct typ *intf) {
  for (size_t n = 0; n < a->isalist_count; ++n) {
    if (a->isalist[n] == intf) {
      return TRUE;
    }
  }

  for (size_t n = 0; n < a->isalist_count; ++n) {
    if (typ_isa(mod, a->isalist[n], intf)) {
      return TRUE;
    }
  }

  return FALSE;
}

error mk_except(const struct module *mod, const struct node *node, const char *fmt) {
  struct token tok;
  tok.value = mod->parser.data + node->codeloc;
  EXCEPTF(EINVAL, "%s:%d:%d: type: %s",
          mod->filename, line(&mod->parser, &tok),
          column(&mod->parser, &tok), fmt);
  return 0;
}

error mk_except_type(const struct module *mod, const struct node *node, const char *fmt) {
  EXCEPT_TYPE(node_module_owner_const(node), node, "%s", fmt);
  return 0;
}

error mk_except_call_arg_count(const struct module *mod, const struct node *node,
                               const struct node *definition, size_t given) {
  EXCEPT_TYPE(node_module_owner_const(node), node, "invalid number of arguments: %zd expected, but %zd given",
              node_fun_args_count(definition), given);
  return 0;
}
