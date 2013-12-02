#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "parser.h"
#include "table.h"
#include "types.h"

EXAMPLE(data_structure_size_stats) {
  // It is a good idea to keep track of what is responsible for the size of
  // 'node_as'. In other words, where to look first to shrink 'struct node'.
  assert(sizeof(struct node_deftype) == sizeof(union node_as));
}

const char *node_which_strings[] = {
  [NUL] = "NUL",
  [IDENT] = "IDENT",
  [NUMBER] = "NUMBER",
  [BOOL] = "BOOL",
  [STRING] = "STRING",
  [SIZEOF] = "SIZEOF",
  [BIN] = "BIN",
  [UN] = "UN",
  [TUPLE] = "TUPLE",
  [TUPLEEXTRACT] = "TUPLEEXTRACT",
  [TUPLENTH] = "TUPLENTH",
  [CALL] = "CALL",
  [INIT] = "INIT",
  [RETURN] = "RETURN",
  [BLOCK] = "BLOCK",
  [FUTURE] = "FUTURE",
  [LAMBDA] = "LAMBDA",
  [FOR] = "FOR",
  [WHILE] = "WHILE",
  [BREAK] = "BREAK",
  [CONTINUE] = "CONTINUE",
  [NOOP] = "NOOP",
  [IF] = "IF",
  [MATCH] = "MATCH",
  [TRY] = "TRY",
  [CATCH] = "CATCH",
  [EXCEP] = "EXCEP",
  [THROW] = "THROW",
  [TYPECONSTRAINT] = "TYPECONSTRAINT",
  [DYN] = "DYN",
  [DEFFUN] = "DEFFUN",
  [DEFTYPE] = "DEFTYPE",
  [DEFNAMEDLITERAL] = "DEFNAMEDLITERAL",
  [DEFCONSTRAINTLITERAL] = "DEFCONSTRAINTLITERAL",
  [DEFUNKNOWNIDENT] = "DEFUNKNOWNIDENT",
  [DEFMETHOD] = "DEFMETHOD",
  [DEFINTF] = "DEFINTF",
  [DEFNAME] = "DEFNAME",
  [DEFPATTERN] = "DEFPATTERN",
  [FUNARGS] = "FUNARGS",
  [DEFARG] = "DEFARG",
  [GENARGS] = "GENARGS",
  [DEFGENARG] = "DEFGENARG",
  [SETGENARG] = "SETGENARG",
  [LET] = "LET",
  [DEFFIELD] = "DEFFIELD",
  [DEFCHOICE] = "DEFCHOICE",
  [DELEGATE] = "DELEGATE",
  [PRE] = "PRE",
  [POST] = "POST",
  [INVARIANT] = "INVARIANT",
  [EXAMPLE] = "EXAMPLE",
  [ISALIST] = "ISALIST",
  [ISA] = "ISA",
  [IMPORT] = "IMPORT",
  [MODULE] = "MODULE",
  [MODULE_BODY] = "MODULE_BODY",
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
  [ID_FINAL] = "final",
  [ID_SELF] = "self",
  [ID_OTHERWISE] = "_",
  [ID_THROW] = "throw",
  [ID_MAIN] = "main",
  [ID_WHICH] = "which__",
  [ID_AS] = "as__",
  [ID_WHICH_TYPE] = "which_type__",
  [ID_AS_TYPE] = "as_type__",
  [ID_NEXT] = "next",
  [ID_GET] = "get",
  [ID_IS_VALID] = "is_valid",
  [ID_CAST] = "cast",
  [ID_WILDCARD_REF_ARG] = "__wildcard_ref_arg__",
  [ID_LIKELY] = "likely",
  [ID_UNLIKELY] = "unlikely",
  [ID_NLANG] = "nlang",
  [ID_TBI_VOID] = "void",
  [ID_TBI_LITERALS_NULL] = "__null__",
  [ID_TBI_LITERALS_INTEGER] = "integer",
  [ID_TBI_LITERALS_FLOATING] = "floating",
  [ID_TBI_ANY] = "i_any",
  [ID_TBI_ANY_TUPLE] = "i_any_tuple",
  [ID_TBI_TUPLE_2] = "tuple_2",
  [ID_TBI_TUPLE_3] = "tuple_3",
  [ID_TBI_TUPLE_4] = "tuple_4",
  [ID_TBI_TUPLE_5] = "tuple_5",
  [ID_TBI_TUPLE_6] = "tuple_6",
  [ID_TBI_TUPLE_7] = "tuple_7",
  [ID_TBI_TUPLE_8] = "tuple_8",
  [ID_TBI_TUPLE_9] = "tuple_9",
  [ID_TBI_TUPLE_10] = "tuple_10",
  [ID_TBI_TUPLE_11] = "tuple_11",
  [ID_TBI_TUPLE_12] = "tuple_12",
  [ID_TBI_TUPLE_13] = "tuple_13",
  [ID_TBI_TUPLE_14] = "tuple_14",
  [ID_TBI_TUPLE_15] = "tuple_15",
  [ID_TBI_TUPLE_16] = "tuple_16",
  [ID_TBI_BOOL] = "bool",
  [ID_TBI_BOOL_COMPATIBLE] = "i_bool_compatible",
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
  [ID_TBI_FLOAT] = "float",
  [ID_TBI_DOUBLE] = "double",
  [ID_TBI_CHAR] = "char",
  [ID_TBI_STRING] = "string",
  [ID_TBI_STATIC_STRING] = "static_string",
  [ID_TBI_STATIC_STRING_COMPATIBLE] = "i_static_string_compatible",
  [ID_TBI_STATIC_ARRAY] = "static_array",
  [ID_TBI__REF_COMPATIBLE] = "__i_ref_compatible",
  [ID_TBI_ANY_ANY_REF] = "i_any_any_ref",
  [ID_TBI_ANY_REF] = "i_any_ref",
  [ID_TBI_ANY_MUTABLE_REF] = "i_any_mutable_ref",
  [ID_TBI_ANY_NULLABLE_REF] = "i_any_nullable_ref",
  [ID_TBI_ANY_NULLABLE_MUTABLE_REF] = "i_any_nullable_mutable_ref",
  [ID_TBI_REF] = "ref",
  [ID_TBI_MREF] = "mutable_ref",
  [ID_TBI_MMREF] = "mercurial_ref",
  [ID_TBI_NREF] = "nullable_ref",
  [ID_TBI_NMREF] = "nullable_mutable_ref",
  [ID_TBI_NMMREF] = "nullable_mercurial_ref",
  [ID_TBI_ARITHMETIC] = "i_arithmetic",
  [ID_TBI_INTEGER] = "i_integer",
  [ID_TBI_UNSIGNED_INTEGER] = "i_unsigned_integer",
  [ID_TBI_NATIVE_INTEGER] = "i_native_integer",
  [ID_TBI_NATIVE_ANYSIGN_INTEGER] = "i_native_anysign_integer",
  [ID_TBI_GENERALIZED_BOOLEAN] = "i_generalized_boolean",
  [ID_TBI_NATIVE_BOOLEAN] = "i_native_boolean",
  [ID_TBI_FLOATING] = "i_floating",
  [ID_TBI_NATIVE_FLOATING] = "i_native_floating",
  [ID_TBI_HAS_EQUALITY] = "i_has_equality",
  [ID_TBI_ORDERED] = "i_ordered",
  [ID_TBI_ORDERED_BY_COMPARE] = "i_ordered_by_compare",
  [ID_TBI_COPYABLE] = "i_copyable",
  [ID_TBI_DEFAULT_CTOR] = "i_default_ctor",
  [ID_TBI_CTOR_WITH] = "i_ctor_with",
  [ID_TBI_ARRAY_CTOR] = "i_array_ctor",
  [ID_TBI_TRIVIAL_COPY] = "i_trivial_copy",
  [ID_TBI_TRIVIAL_CTOR] = "i_trivial_ctor",
  [ID_TBI_TRIVIAL_ARRAY_CTOR] = "i_trivial_array_ctor",
  [ID_TBI_TRIVIAL_DTOR] = "i_trivial_dtor",
  [ID_TBI_TRIVIAL_EQUALITY] = "i_trivial_equality",
  [ID_TBI_TRIVIAL_ORDER] = "i_trivial_order",
  [ID_TBI_RETURN_BY_COPY] = "i_return_by_copy",
  [ID_TBI_SUM_COPY] = "i_sum_copy",
  [ID_TBI_SUM_EQUALITY] = "i_sum_equality",
  [ID_TBI_SUM_ORDER] = "i_sum_order",
  [ID_TBI_ITERATOR] = "i_iterator",
  [ID_TBI__NOT_TYPEABLE] = "__internal_not_typeable__",
  [ID_TBI__CALL_FUNCTION_SLOT] = "__call_function_slot__",
  [ID_TBI__MUTABLE] = "__mutable__",
  [ID_TBI__MERCURIAL] = "__mercurial__",
  [ID_MK] = "mk",
  [ID_NEW] = "new",
  [ID_CTOR] = "ctor",
  [ID_COPY_CTOR] = "copy_ctor",
  [ID_MKV] = "mkv",
  [ID_NEWV] = "newv",
  [ID_C] = "c",
  [ID_FROM_STATIC_STRING] = "from_static_string",
  [ID_FROM_BOOL] = "from_bool",
  [ID_NRETVAL] = "_nretval",
  [ID_OPERATOR_OR] = "operator_or",
  [ID_OPERATOR_AND] = "operator_and",
  [ID_OPERATOR_NOT] = "operator_not",
  [ID_OPERATOR_TEST] = "operator_test",
  [ID_OPERATOR_LE] = "operator_le",
  [ID_OPERATOR_LT] = "operator_lt",
  [ID_OPERATOR_GT] = "operator_gt",
  [ID_OPERATOR_GE] = "operator_ge",
  [ID_OPERATOR_EQ] = "operator_eq",
  [ID_OPERATOR_NE] = "operator_ne",
  [ID_OPERATOR_MATCH] = "operator_match",
  [ID_OPERATOR_BWOR] = "operator_bwor",
  [ID_OPERATOR_BWXOR] = "operator_bwxor",
  [ID_OPERATOR_BWAND] = "operator_bwand",
  [ID_OPERATOR_LSHIFT] = "operator_lshift",
  [ID_OPERATOR_RSHIFT] = "operator_rshift",
  [ID_OPERATOR_ASSIGN_BWOR] = "operator_assign_bwor",
  [ID_OPERATOR_ASSIGN_BWXOR] = "operator_assign_bwxor",
  [ID_OPERATOR_ASSIGN_BWAND] = "operator_assign_bwand",
  [ID_OPERATOR_ASSIGN_LSHIFT] = "operator_assign_lshift",
  [ID_OPERATOR_ASSIGN_RSHIFT] = "operator_assign_rshift",
  [ID_OPERATOR_PLUS] = "operator_plus",
  [ID_OPERATOR_MINUS] = "operator_minus",
  [ID_OPERATOR_DIVIDE] = "operator_divide",
  [ID_OPERATOR_MODULO] = "operator_modulo",
  [ID_OPERATOR_TIMES] = "operator_times",
  [ID_OPERATOR_ASSIGN_PLUS] = "operator_assign_plus",
  [ID_OPERATOR_ASSIGN_MINUS] = "operator_assign_minus",
  [ID_OPERATOR_ASSIGN_DIVIDE] = "operator_assign_divide",
  [ID_OPERATOR_ASSIGN_MODULO] = "operator_assign_modulo",
  [ID_OPERATOR_ASSIGN_TIMES] = "operator_assign_times",
  [ID_OPERATOR_UMINUS] = "operator_uminus",
  [ID_OPERATOR_BWNOT] = "operator_bwnot",
};

const char *builtingen_abspath[BG__NUM] = {
  [BG_DEFAULT_CTOR_CTOR] = "nlang.builtins.i_default_ctor.ctor",
  [BG_DEFAULT_CTOR_MK] = "nlang.builtins.i_default_ctor.mk",
  [BG_DEFAULT_CTOR_NEW] = "nlang.builtins.i_default_ctor.new",
  [BG_TRIVIAL_CTOR_CTOR] = "nlang.builtins.i_trivial_ctor.ctor",
  [BG_TRIVIAL_CTOR_MK] = "nlang.builtins.i_trivial_ctor.mk",
  [BG_TRIVIAL_CTOR_NEW] = "nlang.builtins.i_trivial_ctor.new",
  [BG_AUTO_MK] = "nlang.builtins.i_auto_ctor.mk",
  [BG_AUTO_NEW] = "nlang.builtins.i_auto_ctor.new",
  // These should be templates of i_ctor_with.
  [BG_CTOR_WITH_MK] = "nlang.builtins.i_ctor_with.mk",
  [BG_CTOR_WITH_NEW] = "nlang.builtins.i_ctor_with.new",
  [BG_AUTO_MKV] = "nlang.builtins.i_array_ctor.mkv",
  [BG_AUTO_NEWV] = "nlang.builtins.i_array_ctor.newv",
  [BG_SUM_CTOR_WITH_CTOR] = "nlang.builtins.i_sum_ctor_with.ctor",
  [BG_SUM_CTOR_WITH_MK] = "nlang.builtins.i_sum_ctor_with.mk",
  [BG_SUM_CTOR_WITH_NEW] = "nlang.builtins.i_sum_ctor_with.new",

  [BG_ENUM_EQ] = "nlang.builtins.i_has_equality.operator_eq",
  [BG_ENUM_NE] = "nlang.builtins.i_has_equality.operator_ne",
  [BG_ENUM_MATCH] = "nlang.builtins.i_matchable.operator_match",
  [BG_SUM_MATCH] = "nlang.builtins.i_matchable.operator_match",
  [BG_SUM_DISPATCH] = NULL,
  [BG_SUM_COPY] = "nlang.builtins.i_copyable.copy_ctor",
  [BG_SUM_EQUALITY_EQ] = "nlang.builtins.i_has_equality.operator_eq",
  [BG_SUM_EQUALITY_NE] = "nlang.builtins.i_has_equality.operator_ne",
  [BG_SUM_ORDER_LE] = "nlang.builtins.i_ordered.operator_le",
  [BG_SUM_ORDER_LT] = "nlang.builtins.i_ordered.operator_lt",
  [BG_SUM_ORDER_GT] = "nlang.builtins.i_ordered.operator_gt",
  [BG_SUM_ORDER_GE] = "nlang.builtins.i_ordered.operator_ge",
  [BG_TRIVIAL_COPY_COPY_CTOR] = "nlang.builtins.i_copyable.copy_ctor",
  [BG_TRIVIAL_EQUALITY_OPERATOR_EQ] = "nlang.builtins.i_has_equality.operator_eq",
  [BG_TRIVIAL_EQUALITY_OPERATOR_NE] = "nlang.builtins.i_has_equality.operator_ne",
};

HTABLE_SPARSE(idents_map, ident, struct token);
implement_htable_sparse(__attribute__((unused)) static, idents_map, ident, struct token);

static uint32_t token_hash(const struct token *tok) {
  return hash32_hsieh(tok->value, tok->len);
}

static int token_cmp(const struct token *a, const struct token *b) {
  if (a->len != b->len) {
    return a->len - b->len;
  } else {
    return memcmp(a->value, b->value, min(size_t, a->len, b->len));
  }
}

const char *idents_value(const struct globalctx *gctx, ident id) {
  assert(id < gctx->idents.count);
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

  char *cpy = calloc(tok->len + 1, sizeof(char));
  memcpy(cpy, tok->value, tok->len);

  const ident id = idents->count;
  idents->values[id] = cpy;
  idents->count += 1;

  struct token tokcpy = *tok;
  tokcpy.value = cpy;

  idents_map_set(idents->map, tokcpy, id);

  return id;
}

ident idents_add_string(struct globalctx *gctx, const char *name, size_t len) {
  struct token tok = { 0 };
  tok.t = TIDENT;
  tok.value = name;
  tok.len = len;
  return idents_add(gctx, &tok);
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

#define GOTO_EXCEPT_PARSE(mod, codeloc, fmt, ...) do { \
  struct token tok = { 0 }; \
  tok.value = mod->parser.data + codeloc; \
  GOTO_EXCEPTF(EINVAL, "%s:%d:%d: parse: " fmt, \
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
  assert(node->which != ROOT_OF_ALL);
  if (node->which == MODULE) {
    return node;
  } else {
    if (node->scope == NULL || node->scope->parent == NULL) {
      return NULL;
    }
    return do_node_module_owner(node_parent(node));
  }
}

struct module *node_module_owner(struct node *node) {
  struct node *n = do_node_module_owner(node);
  assert(n != NULL);
  assert(n->which == MODULE);
  return n->as.MODULE.mod;
}

const struct module *node_module_owner_const(const struct node *node) {
  struct node *n = do_node_module_owner((struct node *)node);
  assert(n != NULL);
  assert(n->which == MODULE);
  return n->as.MODULE.mod;
}

const struct module *try_node_module_owner_const(const struct module *mod,
                                                 const struct node *node) {
  struct node *n = do_node_module_owner((struct node *)node);
  if (n == NULL) {
    return mod;
  } else {
    return n->as.MODULE.mod;
  }
}

struct node *node_toplevel_owner(struct node *node) {
  if (node_toplevel(node) != NULL) {
    return node;
  } else {
    return node_toplevel_owner(node_parent(node));
  }
}

struct node *node_statement_owner(struct node *node) {
  if (node_is_statement(node)) {
    return node;
  } else {
    return node_statement_owner(node_parent(node));
  }
}

bool node_is_prototype(const struct node *node) {
  const struct toplevel *toplevel = node_toplevel_const(node);
  if (node->which == MODULE) {
    return node->as.MODULE.is_placeholder;
  } else if (toplevel == NULL) {
    return FALSE;
  } else {
    return toplevel->is_prototype;
  }
}

bool node_is_inline(const struct node *node) {
  const struct toplevel *toplevel = node_toplevel_const(node);
  if (toplevel == NULL) {
    return FALSE;
  } else {
    return toplevel->is_inline;
  }
}

bool node_is_export(const struct node *node) {
  const struct toplevel *toplevel = node_toplevel_const(node);
  if (toplevel == NULL) {
    return FALSE;
  } else {
    return toplevel->is_export;
  }
}

bool node_is_def(const struct node *node) {
  switch (node->which) {
  case MODULE:
  case DEFNAME:
  case DEFARG:
  case DEFFUN:
  case DEFMETHOD:
  case DEFTYPE:
  case DEFFIELD:
  case DEFINTF:
  case DEFCHOICE:
    return TRUE;
  default:
    return FALSE;
  }
}

bool node_is_statement(const struct node *node) {
  return node->scope->parent != NULL
    && node_parent_const(node)->which == BLOCK;
}

bool node_is_at_top(const struct node *node) {
  if (node->scope->parent == NULL) {
    return FALSE;
  } else {
    return node_parent_const(node)->which == MODULE_BODY;
  }
}

bool node_is_rvalue(const struct node *node) {
  switch (node->which) {
  case BIN:
    if (OP_KIND(node->as.BIN.operator) == OP_BIN_ACC) {
      return FALSE;
    }
    return TRUE;
  case UN:
    if (OP_KIND(node->as.UN.operator) == OP_UN_DEREF) {
      return FALSE;
    }
    return TRUE;
  case BOOL:
  case STRING:
  case NUMBER:
  case NUL:
  case TUPLE:
  case INIT:
  case CALL:
    return TRUE;
  case TYPECONSTRAINT:
    return node_is_rvalue(node->subs[0]);
  case BLOCK:
    return node->subs_count > 0 && node_is_rvalue(node->subs[node->subs_count - 1]);
  case IF:
  case TRY:
  case MATCH:
    // Actually not considered to be rvalues, as we always insert a
    // temporary such that
    //   let tmp
    //     tmp = block-like
    // and we then move the 'tmp =' within the block-like, and then we
    // consider issues of rvalues/temporaries on these assignments within
    // the block-like.

    // Fallthrough.
  default:
    return FALSE;
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
    error e = 0;
    GOTO_EXCEPT_PARSE(try_node_module_owner_const(mod, node), node->codeloc,
                      "in scope %s: identifier '%s' already defined at %s:%d:%d",
                      scname, idents_value(mod->gctx, id), existing_mod->filename,
                      line(&existing_mod->parser, &existing_tok),
                      column(&existing_mod->parser, &existing_tok));
except:
    free(scname);
    return e;
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
    error e = 0;
    char *scname = scope_name(mod, scope);
    char *escname = scope_name(mod, for_error->scope);
    GOTO_EXCEPT_PARSE(try_node_module_owner_const(mod, for_error), for_error->codeloc,
                      "from scope %s: in scope %s: unknown identifier '%s'",
                      escname, scname, idents_value(mod->gctx, id));
except:
    free(escname);
    free(scname);
    return e;
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
      GOTO_EXCEPT_PARSE(try_node_module_owner_const(mod, for_error), for_error->codeloc,
                        "from scope %s: in scope %s: unknown identifier '%s'",
                        escname, scname, idents_value(mod->gctx, id));
    }
  }

  return do_scope_lookup_ident_wontimport(result, for_error, mod, scope->parent,
                                          id, failure_ok);

except:
  free(escname);
  free(scname);
  return e;
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
      GOTO_EXCEPT_TYPE(try_node_module_owner_const(mod, id), id, "malformed module path name");
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
      GOTO_EXCEPT_PARSE(try_node_module_owner_const(mod, id), id->codeloc,
                        "not the name of a module");
    } else {
      GOTO_EXCEPT_PARSE(try_node_module_owner_const(mod, id), id->codeloc,
                        "module not found");
    }
  }

except:
  return EINVAL;
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
    GOTO_EXCEPT_PARSE(try_node_module_owner_const(mod, for_error), for_error->codeloc,
                      "from scope %s: in global scope, unknown identifier '%s'",
                      escname, path);
except:
    free(escname);
    return e;
  }

  return 0;
}

error scope_lookup_abspath(struct node **result, const struct node *for_error,
                           const struct module *mod, const char *path) {
  const ssize_t len = strlen(path);
  return do_scope_lookup_abspath(result, for_error, mod, path, len, len);
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
      struct token tok = { 0 };
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

static void init_tbis(struct globalctx *gctx) {
  TBI_VOID = gctx->builtin_typs_by_name[ID_TBI_VOID];
  TBI_LITERALS_NULL = gctx->builtin_typs_by_name[ID_TBI_LITERALS_NULL];
  TBI_LITERALS_INTEGER = gctx->builtin_typs_by_name[ID_TBI_LITERALS_INTEGER];
  TBI_LITERALS_FLOATING = gctx->builtin_typs_by_name[ID_TBI_LITERALS_FLOATING];
  TBI_ANY_TUPLE = gctx->builtin_typs_by_name[ID_TBI_ANY_TUPLE];
  TBI_TUPLE_2 = gctx->builtin_typs_by_name[ID_TBI_TUPLE_2];
  TBI_TUPLE_3 = gctx->builtin_typs_by_name[ID_TBI_TUPLE_3];
  TBI_TUPLE_4 = gctx->builtin_typs_by_name[ID_TBI_TUPLE_4];
  TBI_TUPLE_5 = gctx->builtin_typs_by_name[ID_TBI_TUPLE_5];
  TBI_TUPLE_6 = gctx->builtin_typs_by_name[ID_TBI_TUPLE_6];
  TBI_TUPLE_7 = gctx->builtin_typs_by_name[ID_TBI_TUPLE_7];
  TBI_TUPLE_8 = gctx->builtin_typs_by_name[ID_TBI_TUPLE_8];
  TBI_TUPLE_9 = gctx->builtin_typs_by_name[ID_TBI_TUPLE_9];
  TBI_TUPLE_10 = gctx->builtin_typs_by_name[ID_TBI_TUPLE_10];
  TBI_TUPLE_11 = gctx->builtin_typs_by_name[ID_TBI_TUPLE_11];
  TBI_TUPLE_12 = gctx->builtin_typs_by_name[ID_TBI_TUPLE_12];
  TBI_TUPLE_13 = gctx->builtin_typs_by_name[ID_TBI_TUPLE_13];
  TBI_TUPLE_14 = gctx->builtin_typs_by_name[ID_TBI_TUPLE_14];
  TBI_TUPLE_15 = gctx->builtin_typs_by_name[ID_TBI_TUPLE_15];
  TBI_TUPLE_16 = gctx->builtin_typs_by_name[ID_TBI_TUPLE_16];
  TBI_ANY = gctx->builtin_typs_by_name[ID_TBI_ANY];
  TBI_BOOL = gctx->builtin_typs_by_name[ID_TBI_BOOL];
  TBI_BOOL_COMPATIBLE = gctx->builtin_typs_by_name[ID_TBI_BOOL_COMPATIBLE];
  TBI_I8 = gctx->builtin_typs_by_name[ID_TBI_I8];
  TBI_U8 = gctx->builtin_typs_by_name[ID_TBI_U8];
  TBI_I16 = gctx->builtin_typs_by_name[ID_TBI_I16];
  TBI_U16 = gctx->builtin_typs_by_name[ID_TBI_U16];
  TBI_I32 = gctx->builtin_typs_by_name[ID_TBI_I32];
  TBI_U32 = gctx->builtin_typs_by_name[ID_TBI_U32];
  TBI_I64 = gctx->builtin_typs_by_name[ID_TBI_I64];
  TBI_U64 = gctx->builtin_typs_by_name[ID_TBI_U64];
  TBI_SIZE = gctx->builtin_typs_by_name[ID_TBI_SIZE];
  TBI_SSIZE = gctx->builtin_typs_by_name[ID_TBI_SSIZE];
  TBI_FLOAT = gctx->builtin_typs_by_name[ID_TBI_FLOAT];
  TBI_DOUBLE = gctx->builtin_typs_by_name[ID_TBI_DOUBLE];
  TBI_CHAR = gctx->builtin_typs_by_name[ID_TBI_CHAR];
  TBI_STRING = gctx->builtin_typs_by_name[ID_TBI_STRING];
  TBI_STATIC_STRING = gctx->builtin_typs_by_name[ID_TBI_STATIC_STRING];
  TBI_STATIC_STRING_COMPATIBLE = gctx->builtin_typs_by_name[ID_TBI_STATIC_STRING_COMPATIBLE];
  TBI_STATIC_ARRAY = gctx->builtin_typs_by_name[ID_TBI_STATIC_ARRAY];
  TBI__REF_COMPATIBLE = gctx->builtin_typs_by_name[ID_TBI__REF_COMPATIBLE];
  TBI_ANY_ANY_REF = gctx->builtin_typs_by_name[ID_TBI_ANY_ANY_REF];
  TBI_ANY_REF = gctx->builtin_typs_by_name[ID_TBI_ANY_REF];
  TBI_ANY_MUTABLE_REF = gctx->builtin_typs_by_name[ID_TBI_ANY_MUTABLE_REF];
  TBI_ANY_NULLABLE_REF = gctx->builtin_typs_by_name[ID_TBI_ANY_NULLABLE_REF];
  TBI_ANY_NULLABLE_MUTABLE_REF = gctx->builtin_typs_by_name[ID_TBI_ANY_NULLABLE_MUTABLE_REF];
  TBI_REF = gctx->builtin_typs_by_name[ID_TBI_REF]; // @
  TBI_MREF = gctx->builtin_typs_by_name[ID_TBI_MREF]; // @!
  TBI_MMREF = gctx->builtin_typs_by_name[ID_TBI_MMREF]; // @#
  TBI_NREF = gctx->builtin_typs_by_name[ID_TBI_NREF]; // ?@
  TBI_NMREF = gctx->builtin_typs_by_name[ID_TBI_NMREF]; // ?@!
  TBI_NMMREF = gctx->builtin_typs_by_name[ID_TBI_NMMREF]; // ?@#
  TBI_ARITHMETIC = gctx->builtin_typs_by_name[ID_TBI_ARITHMETIC];
  TBI_INTEGER = gctx->builtin_typs_by_name[ID_TBI_INTEGER];
  TBI_UNSIGNED_INTEGER = gctx->builtin_typs_by_name[ID_TBI_UNSIGNED_INTEGER];
  TBI_NATIVE_INTEGER = gctx->builtin_typs_by_name[ID_TBI_NATIVE_INTEGER];
  TBI_NATIVE_ANYSIGN_INTEGER = gctx->builtin_typs_by_name[ID_TBI_NATIVE_ANYSIGN_INTEGER];
  TBI_GENERALIZED_BOOLEAN = gctx->builtin_typs_by_name[ID_TBI_GENERALIZED_BOOLEAN];
  TBI_NATIVE_BOOLEAN = gctx->builtin_typs_by_name[ID_TBI_NATIVE_BOOLEAN];
  TBI_FLOATING = gctx->builtin_typs_by_name[ID_TBI_FLOATING];
  TBI_NATIVE_FLOATING = gctx->builtin_typs_by_name[ID_TBI_NATIVE_FLOATING];
  TBI_HAS_EQUALITY = gctx->builtin_typs_by_name[ID_TBI_HAS_EQUALITY];
  TBI_ORDERED = gctx->builtin_typs_by_name[ID_TBI_ORDERED];
  TBI_ORDERED_BY_COMPARE = gctx->builtin_typs_by_name[ID_TBI_ORDERED_BY_COMPARE];
  TBI_COPYABLE = gctx->builtin_typs_by_name[ID_TBI_COPYABLE];
  TBI_DEFAULT_CTOR = gctx->builtin_typs_by_name[ID_TBI_DEFAULT_CTOR];
  TBI_CTOR_WITH = gctx->builtin_typs_by_name[ID_TBI_CTOR_WITH];
  TBI_ARRAY_CTOR = gctx->builtin_typs_by_name[ID_TBI_ARRAY_CTOR];
  TBI_TRIVIAL_COPY = gctx->builtin_typs_by_name[ID_TBI_TRIVIAL_COPY];
  TBI_TRIVIAL_CTOR = gctx->builtin_typs_by_name[ID_TBI_TRIVIAL_CTOR];
  TBI_TRIVIAL_ARRAY_CTOR = gctx->builtin_typs_by_name[ID_TBI_TRIVIAL_ARRAY_CTOR];
  TBI_TRIVIAL_DTOR = gctx->builtin_typs_by_name[ID_TBI_TRIVIAL_DTOR];
  TBI_TRIVIAL_EQUALITY = gctx->builtin_typs_by_name[ID_TBI_TRIVIAL_EQUALITY];
  TBI_TRIVIAL_ORDER = gctx->builtin_typs_by_name[ID_TBI_TRIVIAL_ORDER];
  TBI_RETURN_BY_COPY = gctx->builtin_typs_by_name[ID_TBI_RETURN_BY_COPY];
  TBI_SUM_COPY = gctx->builtin_typs_by_name[ID_TBI_SUM_COPY];
  TBI_SUM_EQUALITY = gctx->builtin_typs_by_name[ID_TBI_SUM_EQUALITY];
  TBI_SUM_ORDER = gctx->builtin_typs_by_name[ID_TBI_SUM_ORDER];
  TBI_ITERATOR = gctx->builtin_typs_by_name[ID_TBI_ITERATOR];
  TBI__NOT_TYPEABLE = gctx->builtin_typs_by_name[ID_TBI__NOT_TYPEABLE];
  TBI__CALL_FUNCTION_SLOT = gctx->builtin_typs_by_name[ID_TBI__CALL_FUNCTION_SLOT];
  TBI__MUTABLE = gctx->builtin_typs_by_name[ID_TBI__MUTABLE];
  TBI__MERCURIAL = gctx->builtin_typs_by_name[ID_TBI__MERCURIAL];

  gctx->builtin_typs_for_refop[TREFDOT] = TBI_REF;
  gctx->builtin_typs_for_refop[TREFBANG] = TBI_MREF;
  gctx->builtin_typs_for_refop[TREFSHARP] = TBI_MMREF;
  gctx->builtin_typs_for_refop[TREFWILDCARD] = TBI_ANY_REF;
  gctx->builtin_typs_for_refop[TDEREFDOT] = TBI_REF;
  gctx->builtin_typs_for_refop[TDEREFBANG] = TBI_MREF;
  gctx->builtin_typs_for_refop[TDEREFSHARP] = TBI_MMREF;
  gctx->builtin_typs_for_refop[TDEREFWILDCARD] = TBI_ANY_REF;
  gctx->builtin_typs_for_refop[TNULREFDOT] = TBI_NREF;
  gctx->builtin_typs_for_refop[TNULREFBANG] = TBI_NMREF;
  gctx->builtin_typs_for_refop[TNULREFSHARP] = TBI_NMMREF;
  gctx->builtin_typs_for_refop[TNULREFWILDCARD] = TBI_ANY_NULLABLE_REF;
}

void globalctx_init(struct globalctx *gctx) {
  memset(gctx, 0, sizeof(*gctx));

  gctx->idents.map = calloc(1, sizeof(struct idents_map));
  idents_map_init(gctx->idents.map, 100*1000);
  idents_map_set_delete_val(gctx->idents.map, -1);
  idents_map_set_custom_hashf(gctx->idents.map, token_hash);
  idents_map_set_custom_cmpf(gctx->idents.map, token_cmp);

  gctx->idents.count = ID__NUM;
  gctx->idents.capacity = ID__NUM;
  gctx->idents.values = calloc(ID__NUM, sizeof(char *));
  for (int i = 0; i < ID__NUM; ++i) {
    gctx->idents.values[i] = predefined_idents_strings[i];

    struct token tok = { 0 };
    tok.t = TIDENT;
    tok.value = predefined_idents_strings[i];
    tok.len = strlen(predefined_idents_strings[i]);
    idents_map_set(gctx->idents.map, tok, i);

    if (i >= ID_TBI__FIRST && i <= ID_TBI__LAST) {
      gctx->builtin_typs_by_name[i] = typ_create(NULL, NULL);
    }
  }

  init_tbis(gctx);

  gctx->modules_root.scope = scope_new(&gctx->modules_root);
  gctx->modules_root.which = ROOT_OF_ALL;
  gctx->modules_root.typ = typ_create(NULL, &gctx->modules_root);
}

static error module_read(struct module *mod, const char *prefix, const char *fn) {
  char *fullpath = NULL;
  if (prefix != NULL && strlen(prefix) > 0) {
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

  struct stat st = { 0 };
  error e = fstat(fd, &st);
  if (e < 0) {
    EXCEPTF(errno, "Cannot stat module '%s'", fullpath);
  }

  char *data = calloc(st.st_size + 1, sizeof(char));
  ssize_t count = read(fd, data, st.st_size);
  if (count < 0) {
    EXCEPTF(errno, "Error reading module '%s'", fullpath);
  } else if (count != (ssize_t) st.st_size) {
    EXCEPTF(errno, "Reading module '%s': Partial read not supported by parser", fullpath);
  }

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
  struct token tok = { 0 };
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

bool node_has_tail_block(const struct node *node) {
  return node->subs_count > 0 && node->subs[node->subs_count - 1]->which == BLOCK;
}

bool node_is_fun(const struct node *node) {
  return node->which == DEFFUN || node->which == DEFMETHOD;
}

size_t node_fun_all_args_count(const struct node *def) {
  return def->subs[IDX_FUNARGS]->subs_count - 1;
}

size_t node_fun_explicit_args_count(const struct node *def) {
  size_t minus = 0;
  switch(def->which) {
  case DEFFUN:
    break;
  case DEFMETHOD:
    minus = 1;
    break;
  default:
    assert(FALSE);
    return -1;
  }

  return node_fun_all_args_count(def) - minus;
}

const struct node *node_fun_retval_const(const struct node *def) {
  assert(def->which == DEFFUN || def->which == DEFMETHOD);
  struct node *funargs = def->subs[IDX_FUNARGS];
  return funargs->subs[funargs->subs_count-1];
}

struct node *node_fun_retval(struct node *def) {
  return (struct node *) node_fun_retval_const(def);
}

struct node *node_get_member(struct module *mod, struct node *node, ident id) {
  assert(node->which == DEFTYPE || node->which == DEFCHOICE || node->which == DEFINTF);
  struct node *m = NULL;
  (void)scope_lookup_ident_immediate(&m, node, mod, node->scope, id, TRUE);
  return m;
}

const struct node *node_get_member_const(const struct module *mod, const struct node *node, ident id) {
  return node_get_member((struct module *)mod, (struct node *)node, id);
}

struct node *mk_node(struct module *mod, struct node *parent, enum node_which kind) {
  struct node *n = node_new_subnode(mod, parent);
  n->which = kind;
  return n;
}

static error p_expr(struct node *node, struct module *mod, uint32_t parent_op);
static error p_block(struct node *node, struct module *mod);

static error p_ident(struct node *node, struct module *mod) {
  struct token tok = { 0 };
  error e = scan_oneof(&tok, mod, TIDENT, 0);
  EXCEPT(e);

  node->which = IDENT;
  node->as.IDENT.name = idents_add(mod->gctx, &tok);

  return 0;
}

static error p_number(struct node *node, struct module *mod) {
  struct token tok = { 0 };
  error e = scan_oneof(&tok, mod, TNUMBER, 0);
  EXCEPT(e);

  char *cpy = calloc(tok.len + 1, sizeof(char));
  memcpy(cpy, tok.value, tok.len);

  node->which = NUMBER;
  node->as.NUMBER.value = cpy;

  return 0;
}

static error p_bool(struct node *node, struct module *mod) {
  struct token tok = { 0 };
  error e = scan_oneof(&tok, mod, Tfalse, Ttrue, 0);
  EXCEPT(e);

  node->which = BOOL;
  node->as.BOOL.value = tok.t == Ttrue;
  return 0;
}

static error p_except(struct node *node, struct module *mod) {
  node->which = EXCEP;
  struct token tok = { 0 };
  error e = scan(&tok, mod);
  EXCEPT(e);
  back(mod, &tok);

  if (tok.t == TIDENT) {
    error e = p_ident(node_new_subnode(mod, node), mod);
    EXCEPT(e);
  }

  return 0;
}

static error p_string(struct node *node, struct module *mod) {
  struct token tok = { 0 };
  error e = scan_oneof(&tok, mod, TSTRING, 0);
  EXCEPT(e);

  char *cpy = calloc(tok.len + 1, sizeof(char));
  memcpy(cpy, tok.value, tok.len);

  node->which = STRING;
  node->as.STRING.value = cpy;

  return 0;
}

static error p_typeexpr(struct node *node, struct module *mod) {
  error e = p_expr(node, mod, T__CALL);
  EXCEPT(e);
  return 0;
}

__attribute((unused))
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

  struct token tok = { 0 };
  e = scan(&tok, mod);
  EXCEPT(e);

  if (tok.t == TASSIGN) {
    node->as.DEFCHOICE.has_value = TRUE;

    e = p_expr(node_new_subnode(mod, node), mod, T__NOT_STATEMENT);
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
    struct node *v = mk_node(mod, node, IDENT);
    v->as.IDENT.name = ID_TBI_VOID;
  }
  return 0;
}

static error p_expr_unary(struct node *node, struct module *mod) {
  struct token tok = { 0 };
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

static error p_expr_init(struct node *node, struct module *mod) {
  node->which = INIT;

  error e = scan_expected(mod, TLCBRA);
  EXCEPT(e);

  struct token tok = { 0 };

  while (TRUE) {
    e = scan(&tok, mod);
    EXCEPT(e);

    if (tok.t == TRCBRA) {
      return 0;
    }
    back(mod, &tok);

    e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
    EXCEPT(e);

    struct token assign = { 0 };
    e = scan(&assign, mod);
    EXCEPT(e);

    if (assign.t != TASSIGN) {
      node->as.INIT.is_array = TRUE;
      back(mod, &assign);
    } else {
      if (node->as.INIT.is_array) {
        EXCEPT_SYNTAX(mod, &assign, "array initializer should not contain named pairs");
      }

      e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
      EXCEPT(e);
    }
  }
}

static error p_expr_tuple(struct node *node, const struct node *first,
                          struct module *mod) {
  node->which = TUPLE;
  error e;
  struct token tok = { 0 };

  struct node *fst = node_new_subnode(mod, node);
  *fst = *first;

  size_t count = 1;
  while (TRUE) {
    e = scan(&tok, mod);
    EXCEPT(e);

    if (tok.t != TCOMMA) {
      back(mod, &tok);

      if (count > 16) {
        EXCEPT_SYNTAX(mod, &tok, "tuples can have no more than 16 elements");
      }

      return 0;
    }

    e = p_expr(node_new_subnode(mod, node), mod, TCOMMA);
    EXCEPT(e);

    count += 1;
  }
}

static error p_expr_call(struct node *node, const struct node *first,
                         struct module *mod) {
  node->which = CALL;
  error e;
  struct token tok = { 0 };

  struct node *function = node_new_subnode(mod, node);
  *function = *first;

  while (TRUE) {
    e = scan(&tok, mod);
    EXCEPT(e);
    back(mod, &tok);

    if (expr_terminators[tok.t]) {
      return 0;
    } else if (OP_PREC(tok.t) > OP_PREC(T__CALL)) {
      return 0;
    }

    e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
    EXCEPT(e);
  }
}

static error p_expr_binary(struct node *node, const struct node *first,
                           struct module *mod) {
  struct token tok = { 0 };
  error e = scan(&tok, mod);
  EXCEPT(e);

  assert(tok.t != TCOMMA);
  node->which = tok.t == TCOLON ? TYPECONSTRAINT : BIN;
  node->as.BIN.operator = tok.t;

  struct node *left = node_new_subnode(mod, node);
  *left = *first;

  if (tok.t == TCOLON) {
    e = p_typeexpr(node_new_subnode(mod, node), mod);
    EXCEPT(e);
  } else {
    e = p_expr(node_new_subnode(mod, node), mod, tok.t);
    EXCEPT(e);
  }

  return 0;
}

static error p_expr_post_unary(struct node *node, const struct node *first,
                               struct module *mod) {
  struct token tok = { 0 };
  error e = scan(&tok, mod);
  EXCEPT(e);

  assert(OP_KIND(tok.t) == OP_UN_DEREF);
  node->which = UN;
  node->as.UN.operator = tok.t;

  struct node *left = node_new_subnode(mod, node);
  *left = *first;

  return 0;
}

static error p_if(struct node *node, struct module *mod) {
  node->which = IF;

  struct token eol = { 0 };

  error e = p_expr(node_new_subnode(mod, node), mod, T__NOT_STATEMENT);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);
  e = scan(&eol, mod);
  EXCEPT(e);
  if (eol.t != TEOL) {
    back(mod, &eol);
  }

  struct token tok = { 0 };

again:
  e = scan(&tok, mod);
  EXCEPT(e);

  switch (tok.t) {
  case Telif:
    e = p_expr(node_new_subnode(mod, node), mod, T__NOT_STATEMENT);
    EXCEPT(e);
    e = scan_expected(mod, TSOB);
    EXCEPT(e);
    e = p_block(node_new_subnode(mod, node), mod);
    EXCEPT(e);
    e = scan(&eol, mod);
    EXCEPT(e);
    if (eol.t != TEOL) {
      back(mod, &eol);
    }
    goto again;
  case Telse:
    e = scan_expected(mod, TSOB);
    EXCEPT(e);
    e = p_block(node_new_subnode(mod, node), mod);
    EXCEPT(e);
    e = scan(&eol, mod);
    EXCEPT(e);
    if (eol.t != TEOL) {
      back(mod, &eol);
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

  node->as.FOR.pattern = calloc(1, sizeof(*node->as.FOR.pattern));
  error e = p_expr(node->as.FOR.pattern, mod, T__NOT_STATEMENT);
  EXCEPT(e);

  e = scan_expected(mod, Tin);
  EXCEPT(e);

  struct node *let_it = mk_node(mod, node, LET);
  struct node *it = mk_node(mod, let_it, DEFPATTERN);
  struct node *it_var = mk_node(mod, it, IDENT);
  it_var->as.IDENT.name = gensym(mod);

  e = p_expr(node_new_subnode(mod, it), mod, T__NOT_STATEMENT);
  EXCEPT(e);

  struct node *let_it_block = mk_node(mod, let_it, BLOCK);
  struct node *loop = mk_node(mod, let_it_block, WHILE);
  struct node *v = mk_node(mod, loop, CALL);
  struct node *vm = mk_node(mod, v, BIN);
  vm->as.BIN.operator = TDOT;
  struct node *vmi = mk_node(mod, vm, IDENT);
  vmi->as.IDENT.name = node_ident(it_var);
  struct node *vmm = mk_node(mod, vm, IDENT);
  vmm->as.IDENT.name = ID_IS_VALID;

  struct node *loop_block = mk_node(mod, loop, BLOCK);
  struct node *let_var = mk_node(mod, loop_block, LET);
  struct node *var = mk_node(mod, let_var, DEFPATTERN);
  rew_append(var, node->as.FOR.pattern);
  struct node *g = mk_node(mod, var, CALL);
  struct node *gm = mk_node(mod, g, BIN);
  gm->as.BIN.operator = TDOT;
  struct node *gmi = mk_node(mod, gm, IDENT);
  gmi->as.IDENT.name = node_ident(it_var);
  struct node *gmm = mk_node(mod, gm, IDENT);
  gmm->as.IDENT.name = ID_GET;

  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  node->as.FOR.block = mk_node(mod, let_var, BLOCK);
  e = p_block(node->as.FOR.block, mod);
  EXCEPT(e);

  struct node *n = mk_node(mod, loop_block, CALL);
  struct node *nm = mk_node(mod, n, BIN);
  nm->as.BIN.operator = TBANG;
  struct node *nmi = mk_node(mod, nm, IDENT);
  nmi->as.IDENT.name = node_ident(it_var);
  struct node *nmm = mk_node(mod, nm, IDENT);
  nmm->as.IDENT.name = ID_NEXT;

  return 0;
}

struct node *node_for_block(struct node *node) {
 assert(node->which == FOR);
 return node->subs[IDX_FOR_IT]
   ->subs[IDX_FOR_IT_BLOCK]
   ->subs[IDX_FOR_IT_BLOCK_WHILE]
   ->subs[IDX_FOR_IT_BLOCK_WHILE_BLOCK];
}

static error p_while(struct node *node, struct module *mod) {
  node->which = WHILE;

  error e = p_expr(node_new_subnode(mod, node), mod, T__NOT_STATEMENT);
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

  struct node *elet = mk_node(mod, node, LET);
  struct node *edefp = mk_node(mod, elet, DEFPATTERN);
  struct node *eident = mk_node(mod, edefp, IDENT);
  eident->as.IDENT.name = gensym(mod);
  node->as.TRY.error = node_ident(eident);
  struct node *eblock = mk_node(mod, elet, BLOCK);

  e = p_block(node_new_subnode(mod, eblock), mod);
  EXCEPT(e);
  e = scan_expected(mod, TEOL);
  EXCEPT(e);

  bool first = TRUE;
  bool has_label = FALSE;
  struct token tok = { 0 }, label = { 0 };
  struct node *catch;

again:
  e = scan(&tok, mod);
  EXCEPT(e);

  if (tok.t != Tcatch) {
    if (first) {
      UNEXPECTED(mod, &tok);
    }

    if (!has_label) {
      assert(eblock->subs_count == 2);
      catch->as.CATCH.label = gensym(mod);
    }

    back(mod, &tok);
    return 0;
  }

  if (!first && !has_label) {
    goto missing_label;
  }

  catch = mk_node(mod, eblock, CATCH);

  e = scan(&label, mod);
  EXCEPT(e);
  if (label.t != TIDENT) {
    UNEXPECTED(mod, &label);
  }

  e = scan(&tok, mod);
  EXCEPT(e);
  if (tok.t == TIDENT) {
    has_label = TRUE;
    catch->as.CATCH.label = idents_add(mod->gctx, &label);
    catch->as.CATCH.is_user_label = TRUE;
  } else if (tok.t == TSOB) {
    if (!first) {
      goto missing_label;
    }

    back(mod, &tok);
    tok = label;
  } else {
    UNEXPECTED(mod, &tok);
  }

  struct node *let = mk_node(mod, catch, LET);
  struct node *defp = mk_node(mod, let, DEFPATTERN);
  struct node *var = mk_node(mod, defp, IDENT);
  var->as.IDENT.name = idents_add(mod->gctx, &tok);
  struct node *expr = mk_node(mod, defp, IDENT);
  expr->as.IDENT.name = node_ident(eident);

  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(node_new_subnode(mod, catch), mod);
  EXCEPT(e);
  e = scan_expected(mod, TEOL);
  EXCEPT(e);

  first = FALSE;
  goto again;

missing_label:
  EXCEPT_SYNTAX(mod, &tok, "to use multiple catch in a try block, each must have a label");
  return 0;
}

static error p_match(struct node *node, struct module *mod) {
  node->which = MATCH;

  error e = p_expr(node_new_subnode(mod, node), mod, T__NOT_STATEMENT);
  EXCEPT(e);
  e = scan_expected(mod, TEOL);
  EXCEPT(e);

  struct token tok = { 0 };
again:
  e = scan(&tok, mod);
  EXCEPT(e);
  if (tok.t != TBWOR) {
    back(mod, &tok);
    mod->parser.inject_eol_after_eob = TRUE;
    return 0;
  }

  e = p_expr(node_new_subnode(mod, node), mod, T__NOT_STATEMENT);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);
  e = scan_expected(mod, TEOL);
  EXCEPT(e);
  goto again;
}

static error p_expr(struct node *node, struct module *mod, uint32_t parent_op) {
  assert(parent_op < TOKEN__NUM && IS_OP(parent_op));

  error e;
  struct token tok = { 0 };
  bool first_iteration = TRUE;
  bool topmost = parent_op == T__STATEMENT;

  e = scan(&tok, mod);
  EXCEPT(e);

  struct node first = { 0 }, second = { 0 };
  first.codeloc = mod->parser.pos;
  second.codeloc = mod->parser.pos;

  if (tok.t == TBARROW) {
    e = p_expr(&first, mod, T__NOT_STATEMENT);
    EXCEPT(e);
  } else if (tok.t == TLPAR) {
    e = p_expr(&first, mod, T__NOT_STATEMENT);
    EXCEPT(e);
    e = scan_expected(mod, TRPAR);
    EXCEPT(e);
  } else {

    switch (tok.t) {
    case Tnull:
      first.which = NUL;
      break;
    case Tsizeof:
      first.which = SIZEOF;
      e = p_expr(node_new_subnode(mod, &first), mod, T__CALL);
      break;
    case Texcept:
      e = p_except(&first, mod);
      break;
    case TIDENT:
      back(mod, &tok);
      e = p_ident(&first, mod);
      break;
    case TNUMBER:
      back(mod, &tok);
      e = p_number(&first, mod);
      break;
    case Tfalse:
    case Ttrue:
      back(mod, &tok);
      e = p_bool(&first, mod);
      break;
    case TSTRING:
      back(mod, &tok);
      e = p_string(&first, mod);
      break;
    case TLCBRA:
      back(mod, &tok);
      e = p_expr_init(&first, mod);
      break;
    case Tblock:
      e = scan_expected(mod, TSOB);
      EXCEPT(e);
      e = p_block(&first, mod);
      break;
    case Tif:
      e = p_if(&first, mod);
      break;
    case Ttry:
      e = p_try(&first, mod);
      break;
    case Tmatch:
      e = p_match(&first, mod);
      break;
    default:
      back(mod, &tok);
      if ((IS_OP(tok.t) && OP_UNARY(tok.t))
          || tok.t == TMINUS || tok.t == TPLUS) { // Unary versions.
        e = p_expr_unary(&first, mod);
      } else {
        UNEXPECTED(mod, &tok);
      }
      break;
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
  } else if (tok.t != TBARROW && IS_OP(tok.t) && OP_BINARY(tok.t)) {
    if (tok.t == TCOMMA) {
      if (OP_PREC(tok.t) < OP_PREC(parent_op)
          || topmost) {
        e = p_expr_tuple(&second, &first, mod);
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
  } else if (IS_OP(tok.t) && OP_UNARY(tok.t) && OP_KIND(tok.t) == OP_UN_DEREF) {
    e = p_expr_post_unary(&second, &first, mod);
    EXCEPT(e);

    if (topmost) {
      parent_op = tok.t;
    }

    goto shift;
  } else if (OP_PREC(T__CALL) < OP_PREC(parent_op)
             || topmost) {
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

  struct token tok = { 0 };
  error e = scan(&tok, mod);
  EXCEPT(e);
  back(mod, &tok);

  if (!expr_terminators[tok.t]) {
    e = p_expr(node_new_subnode(mod, node), mod, T__NOT_STATEMENT);
    EXCEPT(e);
  }
  return 0;
}

static error p_throw(struct node *node, struct module *mod) {
  node->which = THROW;

  error e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
  EXCEPT(e);

  struct token tok = { 0 };
  e = scan(&tok, mod);
  EXCEPT(e);
  back(mod, &tok);

  if (!expr_terminators[tok.t]) {
    e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
    EXCEPT(e);
  }
  return 0;
}

static error p_defpattern(struct node *node, struct module *mod,
                          enum token_type let_alias) {
  node->which = DEFPATTERN;
  node->as.DEFPATTERN.is_alias = let_alias == Talias;

  error e = p_expr(node_new_subnode(mod, node), mod, T__NOT_STATEMENT);
  EXCEPT(e);

  struct token tok = { 0 };
  e = scan(&tok, mod);
  EXCEPT(e);
  if (tok.t != TASSIGN) {
    back(mod, &tok);
    return 0;
  }

  e = p_expr(node_new_subnode(mod, node), mod, T__NOT_STATEMENT);
  EXCEPT(e);

  return 0;
}

// When let_and_alias_such == Tand, node is previous LET we are appending
// ourselves to. Same idea with Tsuch.
static error p_let(struct node *node, struct module *mod, const struct toplevel *toplevel,
                   enum token_type let_and_alias_such) {
  if (let_and_alias_such == Tsuch) {
    assert(node->which == LET);
    assert(toplevel == NULL);

    struct token tok = { 0 };
    error e = scan(&tok, mod);
    EXCEPT(e);

    if (tok.t != TSOB) {
      UNEXPECTED(mod, &tok);
    }

    e = p_block(node_new_subnode(mod, node), mod);
    EXCEPT(e);

    return 0;

  } else if (let_and_alias_such == Tand) {
    assert(node->which == LET);
    assert(toplevel == NULL);
  } else {
    node->which = LET;
    if (toplevel != NULL) {
      node->as.LET.toplevel = *toplevel;
      node->flags |= NODE_IS_GLOBAL_LET;
    }
  }

  error e = p_defpattern(node_new_subnode(mod, node), mod, let_and_alias_such);
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

static error p_noop(struct node *node, struct module *mod) {
  node->which = NOOP;
  return 0;
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

static error p_invariant(struct node *node, struct module *mod,
                         struct toplevel *toplevel) {
  node->which = INVARIANT;
  node->as.INVARIANT.toplevel = *toplevel;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_example(struct node *node, struct module *mod) {
  node->which = EXAMPLE;
  node->as.EXAMPLE.name = mod->next_example;
  mod->next_example += 1;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_statement(struct node *parent, struct module *mod) {
  error e;
  struct token tok = { 0 };

#define NEW node_new_subnode(mod, parent)

  e = scan(&tok, mod);
  EXCEPT(e);

  switch (tok.t) {
  case Treturn:
    e = p_return(NEW, mod);
    break;
  case Tthrow:
    e = p_throw(NEW, mod);
    break;
  case Tlet:
  case Tand:
  case Tsuch:
  case Talias:
    e = p_let((tok.t == Tlet || tok.t == Talias)
              ? NEW
              : parent->subs[parent->subs_count-1],
              mod, NULL, tok.t);
    break;
  case Tfor:
    e = p_for(NEW, mod);
    break;
  case Twhile:
    e = p_while(NEW, mod);
    break;
  case Tbreak:
    e = p_break(NEW, mod);
    break;
  case Tcontinue:
    e = p_continue(NEW, mod);
    break;
  case Tnoop:
    e = p_noop(NEW, mod);
    break;
  case Tpre:
    e = p_pre(NEW, mod);
    break;
  case Tpost:
    e = p_post(NEW, mod);
    break;
  case Tinvariant:
    e = p_invariant(NEW, mod, NULL);
    break;
  default:
    back(mod, &tok);
    e = p_expr(NEW, mod, T__STATEMENT);
    break;
  }
  EXCEPT(e);

#undef NEW

  return 0;
}

static error p_block(struct node *node, struct module *mod) {
  node->which = BLOCK;
  error e;
  struct token tok = { 0 };
  bool first = TRUE;

  e = scan(&tok, mod);
  EXCEPT(e);

again:
  if (tok.t == TEOB) {
    if (first) {
      EXCEPT_SYNTAX(mod, &tok, "block cannot be empty (use 'noop' instead)");;
    } else {
      return 0;
    }
  } else {
    back(mod, &tok);
    e = p_statement(node, mod);
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

static error p_defarg(struct node *node, struct module *mod, bool is_optional) {
  node->which = DEFARG;
  node->as.DEFARG.is_optional = is_optional;

  error e = p_expr(node_new_subnode(mod, node), mod, TCOLON);
  EXCEPT(e);

  e = scan_expected(mod, TCOLON);
  EXCEPT(e);

  e = p_typeexpr(node_new_subnode(mod, node), mod);
  EXCEPT(e);
  return 0;
}

// FIXME: Avoid these forward declarations.
typedef error (*step)(struct module *mod, struct node *node, void *user, bool *stop);

error pass(struct module *mod, struct node *node,
           const step *down_steps, const step *up_steps, ssize_t shallow_last_up,
           void *user);

static error step_rewrite_into_defarg(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  if (node->which == TYPECONSTRAINT) {
    node->which = DEFARG;
  }
  return 0;
}

static error p_defret(struct node *node, struct module *mod) {
  error e = p_expr(node, mod, T__STATEMENT);
  EXCEPT(e);

  static const step downs[] = { NULL };
  static const step ups[] = { step_rewrite_into_defarg, NULL };

  e = pass(mod, node, downs, ups, -1, NULL);
  EXCEPT(e);

  return 0;
}

static void add_self_arg(struct module *mod, struct node *node,
                         struct node *funargs) {
  struct node *arg = mk_node(mod, funargs, DEFARG);
  struct node *name = mk_node(mod, arg, IDENT);
  name->as.IDENT.name = ID_SELF;

  if (node->as.DEFMETHOD.access == TREFWILDCARD) {
    struct node *genargs = node->subs[IDX_GENARGS];
    struct node *ga = mk_node(mod, genargs, DEFGENARG);
    struct node *gan = mk_node(mod, ga, IDENT);
    gan->as.IDENT.name = ID_WILDCARD_REF_ARG;
    struct node *gat = mk_node(mod, ga, IDENT);
    gat->as.IDENT.name = ID_TBI_ANY_REF;

    struct node *argt = mk_node(mod, arg, CALL);
    struct node *ref = mk_node(mod, argt, IDENT);
    ref->as.IDENT.name = ID_WILDCARD_REF_ARG;
    struct node *reft = mk_node(mod, argt, IDENT);
    reft->as.IDENT.name = ID_FINAL;
  } else {
    struct node *ref = mk_node(mod, arg, UN);
    ref->as.UN.operator = node->as.DEFMETHOD.access;
    struct node *typename = mk_node(mod, ref, IDENT);
    typename->as.IDENT.name = ID_FINAL;
  }
}

static error p_defmethod_access(struct node *node, struct module *mod) {
  node->as.DEFMETHOD.access = TREFDOT;

  struct token tok = { 0 };
  error e = scan(&tok, mod);
  EXCEPT(e);
  switch (tok.t) {
  case TDEREFBANG:
    node->as.DEFMETHOD.access = TREFBANG;
    break;
  case TDEREFSHARP:
    node->as.DEFMETHOD.access = TREFSHARP;
    break;
  case TDEREFWILDCARD:
    node->as.DEFMETHOD.access = TREFWILDCARD;
    break;
  default:
    back(mod, &tok);
    break;
  }

  return 0;
}

static error p_deffun(struct node *node, struct module *mod, const struct toplevel *toplevel,
                      enum node_which fun_or_method) {
  error e;
  struct token tok = { 0 };

  node->which = fun_or_method;
  switch (node->which) {
  case DEFFUN:
    node->as.DEFFUN.toplevel = *toplevel;
    break;
  case DEFMETHOD:
    node->as.DEFMETHOD.toplevel = *toplevel;
    e = p_defmethod_access(node, mod);
    EXCEPT(e);
    break;
  default:
    assert(FALSE);
  }

  struct node *name = node->subs[0];
  e = p_expr(name, mod, T__CALL);
  EXCEPT(e);

  struct node *funargs = mk_node(mod, node, FUNARGS);
  if (fun_or_method == DEFMETHOD) {
    if (name->which != IDENT && name->which != BIN) {
      EXCEPT_SYNTAX(mod, &tok, "malformed method name");
    }

    add_self_arg(mod, node, funargs);
  } else {
    if (name->which != IDENT) {
      EXCEPT_SYNTAX(mod, &tok, "malformed fun name");
    }
  }

again:
  e = scan_oneof(&tok, mod, TASSIGN, TIDENT, TQMARK, 0);
  EXCEPT(e);

  switch (tok.t) {
  case TASSIGN:
    goto retval;
  case TIDENT:
    back(mod, &tok);
    // Fallthrough
  case TQMARK:
    {
      struct node *arg = node_new_subnode(mod, funargs);
      e = p_defarg(arg, mod, tok.t == TQMARK);
      EXCEPT(e);
    }
    goto again;
  default:
    assert(FALSE);
  }

retval:
  e = p_defret(node_new_subnode(mod, funargs), mod);
  EXCEPT(e);

  e = scan_oneof(&tok, mod, TEOL, TSOB, TEOB, 0);
  EXCEPT(e);

  if (tok.t == TEOL || tok.t == TEOB) {
    back(mod, &tok);
    node_toplevel(node)->is_prototype = TRUE;
    return 0;
  }

  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_isa(struct node *node, struct module *mod, bool is_export) {
  node->which = ISA;
  node->as.ISA.is_export = is_export;
  node->as.ISA.is_explicit = TRUE;

  error e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
  EXCEPT(e);
  return 0;
}

static error p_isalist(struct node *node, struct module *mod) {
  node->which = ISALIST;

  error e;
  struct token tok = { 0 };
  bool is_export = FALSE;

again:
  e = scan(&tok, mod);
  EXCEPT(e);

  switch (tok.t) {
  case TEOL:
  case TSOB:
    back(mod, &tok);
    return 0;
  case Texport:
    is_export = TRUE;
    goto again;
  default:
    back(mod, &tok);
    e = p_isa(node_new_subnode(mod, node), mod, is_export);
    EXCEPT(e);
    is_export = FALSE;
    goto again;
  }
}

static error p_delegate(struct node *node, struct module *mod,
                        struct toplevel *toplevel) {
  node->which = DELEGATE;
  node->as.DELEGATE.toplevel = *toplevel;

  error e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
  EXCEPT(e);

  struct token tok = { 0 };

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
  error e = 0;
  struct token tok = { 0 };
  struct toplevel toplevel = { 0 };

again:
  e = scan(&tok, mod);
  EXCEPT(e);

  switch (tok.t) {
  case Tnoop:
    e = p_noop(node, mod);
    break;
  case Tlet:
  case Talias:
    e = p_let(node, mod, &toplevel, tok.t);
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
  case Tdelegate:
    e = p_delegate(node, mod, &toplevel);
    break;
  case Tinvariant:
    e = p_invariant(node, mod, &toplevel);
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
  struct token tok = { 0 };
  bool first = TRUE;

  e = scan(&tok, mod);
  EXCEPT(e);

again:
  if (tok.t == TEOB) {
    if (first) {
      EXCEPT_SYNTAX(mod, &tok, "block cannot be empty (use 'noop' instead)");;
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

static error p_defgenarg(struct node *node, struct module *mod, bool explicit) {
  node->which = DEFGENARG;
  node->as.DEFGENARG.is_explicit = explicit;
  error e = p_expr(node_new_subnode(mod, node), mod, TCOLON);
  EXCEPT(e);

  e = scan_expected(mod, TCOLON);
  EXCEPT(e);

  e = p_typeexpr(node_new_subnode(mod, node), mod);
  EXCEPT(e);
  return 0;
}

static error p_genargs(struct node *node, struct module *mod,
                       enum token_type terminator, bool explicit) {
  node->which = GENARGS;

  error e;
  struct token tok = { 0 };
again:
  e = scan(&tok, mod);
  EXCEPT(e);
  if (tok.t == terminator) {
    if (!explicit && node->subs_count == 0) {
      UNEXPECTED(mod, &tok);
    }
    return 0;
  }
  back(mod, &tok);

  e = p_defgenarg(node_new_subnode(mod, node), mod, explicit);
  EXCEPT(e);
  goto again;

  return 0;
}

static error p_deftype(struct node *node, struct module *mod,
                       struct node *some_genargs, struct toplevel *toplevel) {
  if (some_genargs != NULL) {
    toplevel->first_explicit_genarg = some_genargs->subs_count;
  }

  node->which = DEFTYPE;
  node->as.DEFTYPE.toplevel = *toplevel;

  error e = p_ident(node->subs[0], mod);
  EXCEPT(e);

  e = p_genargs(node->subs[IDX_GENARGS], mod, TASSIGN, TRUE);
  EXCEPT(e);

  e = p_isalist(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  struct token tok = { 0 };
  e = scan_oneof(&tok, mod, TEOL, TSOB, 0);
  EXCEPT(e);

  if (tok.t == TEOL) {
    back(mod, &tok);
    return 0;
  }

  e = p_deftype_block(node, mod);
  EXCEPT(e);

  return 0;
}

static error p_implicit_genargs(struct node *genargs, struct module *mod) {
  error e = p_genargs(genargs, mod, TRPAR, FALSE);
  EXCEPT(e);

  return 0;
}

static error p_defintf_statement(struct node *node, struct module *mod, ident intf_name) {
  error e;
  struct token tok = { 0 }, tok2 = { 0 };
  struct toplevel toplevel = { 0 };
  toplevel.scope_name = intf_name;
  struct node *genargs = NULL;

  e = scan(&tok, mod);
  EXCEPT(e);

again:
  switch (tok.t) {
  case TLPAR:
    e = scan_oneof(&tok2, mod, Tfun, Tmethod, 0);
    EXCEPT(e);
    if (tok2.t == Tmethod) {
      e = p_defmethod_access(node, mod);
      EXCEPT(e);
    }
    (void)node_new_subnode(mod, node);
    genargs = node_new_subnode(mod, node);
    e = p_implicit_genargs(genargs, mod);
    EXCEPT(e);
    tok = tok2;
    goto again;
  case Tfun:
    if (node->subs_count == 0) {
      (void)node_new_subnode(mod, node);
      (void)mk_node(mod, node, GENARGS);
    }
    e = p_deffun(node, mod, &toplevel, DEFFUN);
    break;
  case Tmethod:
    if (node->subs_count == 0) {
      (void)node_new_subnode(mod, node);
      (void)mk_node(mod, node, GENARGS);
    }
    e = p_deffun(node, mod, &toplevel, DEFMETHOD);
    break;
  case Tlet:
  case Talias:
    e = p_let(node, mod, &toplevel, tok.t);
    break;
  case Tinvariant:
    e = p_invariant(node, mod, &toplevel);
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

static error p_defintf_block(struct node *node, struct module *mod, ident intf_name) {
  error e;
  struct token tok = { 0 };
  bool first = TRUE;

  e = scan(&tok, mod);
  EXCEPT(e);

again:
  if (tok.t == TEOB) {
    if (first) {
      EXCEPT_SYNTAX(mod, &tok, "block cannot be empty (use 'noop' instead)");;
    } else {
      return 0;
    }
  } else {
    back(mod, &tok);
    e = p_defintf_statement(node_new_subnode(mod, node), mod, intf_name);
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

static error p_defintf(struct node *node, struct module *mod,
                       struct node *some_genargs, struct toplevel *toplevel) {
  if (some_genargs != NULL) {
    toplevel->first_explicit_genarg = some_genargs->subs_count;
  }

  node->which = DEFINTF;
  node->as.DEFINTF.toplevel = *toplevel;

  struct token tok = { 0 };
  error e = p_ident(node->subs[0], mod);
  EXCEPT(e);

  e = p_genargs(node->subs[IDX_GENARGS], mod, TASSIGN, TRUE);
  EXCEPT(e);

  e = p_isalist(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  e = scan_oneof(&tok, mod, TEOL, TSOB, 0);
  EXCEPT(e);

  if (tok.t == TEOL) {
    back(mod, &tok);
    return 0;
  }

  e = p_defintf_block(node, mod, node_ident(node));
  EXCEPT(e);

  return 0;
}

void node_deepcopy(struct module *mod, struct node *dst,
                   const struct node *src) {
  dst->which = src->which;
  memcpy(dst, src, sizeof(*dst));
  dst->scope = NULL;
  dst->subs = NULL;
  dst->subs_count = 0;
  dst->typ = NULL;

  struct toplevel *dtoplevel = node_toplevel(dst);
  if (dtoplevel != NULL) {
    dtoplevel->instances = NULL;
    dtoplevel->instances_count = 0;
  }
  if (dst->which == DEFTYPE) {
    dst->as.DEFTYPE.members = NULL;
    dst->as.DEFTYPE.members_count = 0;
  }

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

static error p_import(struct node *node, struct module *mod,
                      const struct toplevel *toplevel,
                      bool from) {
  node->which = IMPORT;
  node->as.IMPORT.toplevel = *toplevel;

  error e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
  EXCEPT(e);

  if (!from) {
    return 0;
  }

  int import_export_count = 0;
  int inline_count = 0;
  int ident_count = 0;
  struct token tok = { 0 };
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
    *node_toplevel(imported) = *toplevel;

    copy_and_extend_import_path(mod, imported, node, &tok);

    goto again;
  } else {
    back(mod, &tok);
    return 0;
  }
}

static error p_toplevel(struct module *mod) {
  struct toplevel toplevel = { 0 };
  struct node *genargs = NULL;

  bool is_scoped = FALSE;
  error e;
  struct token tok = { 0 }, tok2 = { 0 };
  struct node *node = NULL;

again:
  e = scan(&tok, mod);
  EXCEPT(e);

bypass:
  if (is_scoped && tok.t != Tmethod && tok.t != Tfun && tok.t != TLPAR) {
    UNEXPECTED(mod, &tok);
  }

#define NEW(mod, node) ( (node != NULL) ? node : node_new_subnode(mod, mod->body) )

  switch (tok.t) {
  case TLPAR:
    e = scan_oneof(&tok2, mod, Ttype, Tintf, Tfun, Tmethod, 0);
    EXCEPT(e);
    node = NEW(mod, node);
    if (tok2.t == Tmethod) {
      e = p_defmethod_access(node, mod);
      EXCEPT(e);
    }
    /* name */ (void)node_new_subnode(mod, node);
    genargs = node_new_subnode(mod, node);
    e = p_implicit_genargs(genargs, mod);
    EXCEPT(e);
    tok = tok2;
    goto bypass;
  case Ttype:
    node = NEW(mod, node);
    if (node->subs_count == 0) {
      (void)node_new_subnode(mod, node);
      (void)mk_node(mod, node, GENARGS);
    }
    e = p_deftype(node, mod, genargs, &toplevel);
    break;
  case Tintf:
    node = NEW(mod, node);
    if (node->subs_count == 0) {
      (void)node_new_subnode(mod, node);
      (void)mk_node(mod, node, GENARGS);
    }
    e = p_defintf(node, mod, genargs, &toplevel);
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
    node = NEW(mod, node);
    if (node->subs_count == 0) {
      (void)node_new_subnode(mod, node);
      (void)mk_node(mod, node, GENARGS);
    }
    e = p_deffun(node, mod, &toplevel, DEFFUN);
    break;
  case Tmethod:
    if (!is_scoped) {
      UNEXPECTED(mod, &tok);
    }
    node = NEW(mod, node);
    if (node->subs_count == 0) {
      (void)node_new_subnode(mod, node);
      (void)mk_node(mod, node, GENARGS);
    }
    e = p_deffun(node, mod, &toplevel, DEFMETHOD);
    break;
  case Tlet:
  case Talias:
    e = p_let(NEW(mod, node), mod, &toplevel, tok.t);
    break;
  case TIDENT:
    toplevel.scope_name = idents_add(mod->gctx, &tok);
    is_scoped = TRUE;
    goto again;
  case Tfrom:
  case Timport:
    e = p_import(NEW(mod, node), mod, &toplevel, tok.t == Tfrom);
    break;
  case Texample:
    e = p_example(NEW(mod, node), mod);
    break;
  case TEOL:
    break;
  default:
    EXCEPT_SYNTAX(mod, &tok, "malformed top-level statement at '%.*s'", (int)tok.len, tok.value);
    break;
  }

#undef NEW

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
  mod->body = mk_node(mod, mod->root, MODULE_BODY);

  do {
    e = p_toplevel(mod);
    EXCEPT(e);

    if (eof(&mod->parser)) {
      // Only hit with source file containing just comments.
      break;
    }

    e = scan_expected(mod, TEOL);
    EXCEPT(e);
  } while (!eof(&mod->parser));

  return 0;
}

static void module_init(struct globalctx *gctx, struct stage *stage,
                        struct module *mod) {
  memset(mod, 0, sizeof(*mod));

  mod->gctx = gctx;
  mod->stage = stage;

  PUSH_STATE(mod->state);
  PUSH_STATE(mod->state->step_state);
}

EXAMPLE(parse_modpath) {
  {
    struct globalctx gctx = { 0 };
    globalctx_init(&gctx);
    struct stage stage = { 0 };
    struct module m = { 0 };
    module_init(&gctx, &stage, &m);
    parse_modpath(&m, "test.n");
    assert(m.path_len == 1);
    const char *p = "test";
    assert(m.path[0] == idents_add_string(&gctx, p, strlen(p)));
  }
  {
    struct globalctx gctx = { 0 };
    globalctx_init(&gctx);
    struct stage stage = { 0 };
    struct module m = { 0 };
    module_init(&gctx, &stage, &m);
    parse_modpath(&m, "bootstrap/module.n");
    assert(m.path_len == 1);
    const char *p = "bootstrap";
    assert(m.path[0] == idents_add_string(&gctx, p, strlen(p)));
  }
  {
    struct globalctx gctx = { 0 };
    globalctx_init(&gctx);
    struct stage stage = { 0 };
    struct module m = { 0 };
    module_init(&gctx, &stage, &m);
    parse_modpath(&m, "bootstrap/test.n");
    assert(m.path_len == 2);
    const char *p1 = "bootstrap";
    const char *p2 = "test";
    assert(m.path[0] == idents_add_string(&gctx, p1, strlen(p1)));
    assert(m.path[1] == idents_add_string(&gctx, p2, strlen(p2)));
  }
}

static struct node *create_module_node(struct node *parent, ident basename,
                                       bool is_placeholder,
                                       struct module *non_placeholder_mod) {
  struct node *m = node_new_subnode(non_placeholder_mod, parent);
  m->which = MODULE;
  m->as.MODULE.name = basename;
  m->as.MODULE.is_placeholder = is_placeholder;
  m->as.MODULE.mod = is_placeholder ? NULL : non_placeholder_mod;
  m->scope = scope_new(m);
  m->scope->parent = parent->scope;
  m->typ = typ_create(NULL, m);
  m->flags = NODE_IS_TYPE;

  if (!is_placeholder) {
    non_placeholder_mod->root = m;
  }

  return m;
}

static error register_module(struct globalctx *gctx, struct module *to_register,
                             const char *filename) {
  // A number of the calls below need access to global state, but don't care
  // about 'to_register' specifically. This variable makes this distinction
  // clearer.
  struct module *some_module = to_register;

  const size_t last = to_register->path_len - 1;
  struct node *parent = &gctx->modules_root;

  for (size_t p = 0; p <= last; ++p) {
    ident i = to_register->path[p];
    struct node *m = NULL;
    error e = scope_lookup_ident_wontimport(&m, parent, some_module, parent->scope,
                                            i, TRUE);
    if (e == EINVAL) {
      m = create_module_node(parent, i, p != last, to_register);

      e = scope_define_ident(some_module, parent->scope, i, m);
      EXCEPT(e);

    } else if (e) {
      // Repeat bound-to-fail lookup to get the error message right.
      e = scope_lookup_ident_wontimport(&m, parent, some_module, parent->scope,
                                        i, FALSE);
      THROW(e);
    } else {
      assert(m->which == MODULE);
      if (p == last) {
        if (!m->as.MODULE.is_placeholder) {
          EXCEPTF(EINVAL, "Cannot load_module module '%s' more than once",
                  filename);
        } else {
          assert(to_register->root == NULL);
          to_register->root = create_module_node(parent, i, FALSE, to_register);

          for (size_t s = 0; s < m->subs_count; ++s) {
            struct node *to_save = m->subs[s];
            assert(to_save->which == MODULE);
            e = scope_define_ident(some_module, to_register->root->scope,
                                   node_ident(to_save), to_save);
            EXCEPT(e);
          }

          // scope_define_ident() knows to accept to overwrite because it
          // was a placeholder.
          e = scope_define_ident(some_module, parent->scope, i, to_register->root);
          EXCEPT(e);
        }

        break;
      }
    }

    parent = m;
  }

  return 0;
}

error module_open(struct globalctx *gctx, struct stage *stage,
                  struct module *mod, const char *prefix, const char *fn) {
  module_init(gctx, stage, mod);

  error e = parse_modpath(mod, fn);
  EXCEPT(e);

  e = register_module(gctx, mod, fn);
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

  char name[64] = { 0 };
  int cnt = snprintf(name, ARRAY_SIZE(name), "_Ngensym%zx", g);

  return idents_add_string(mod->gctx, name, cnt);
}

void module_retval_set(struct module *mod, const struct node *retval) {
  mod->state->fun_state->retval = retval;
}

const struct node *module_retval_get(struct module *mod) {
  return mod->state->fun_state->retval;
}

void module_excepts_open_try(struct module *mod, struct node *tryy) {
  PUSH_STATE(mod->state->try_state);
  mod->state->try_state->tryy = tryy;
}

void module_excepts_push(struct module *mod, struct node *excep_node) {
  struct try_state *st = mod->state->try_state;
  st->count += 1;
  st->excepts = realloc(st->excepts, st->count * sizeof(*st->excepts));
  st->excepts[st->count - 1] = excep_node;
}

struct try_state *module_excepts_get(struct module *mod) {
  return mod->state->try_state;
}

void module_excepts_close_try(struct module *mod) {
  free(mod->state->try_state->excepts);
  POP_STATE(mod->state->try_state);
}

char *typ_name(const struct module *mod, const struct typ *t) {
  if (typ_generic_arity(t) > 0 && !typ_is_generic_functor(t)) {
    return typ_name(mod, typ_generic_functor_const(t));
  } else if (typ_definition_const(t) != NULL) {
    return scope_name(mod, typ_definition_const(t)->scope);
  } else {
    for (size_t n = ID_TBI__FIRST; n < ID_TBI__LAST; ++n) {
      if (mod->gctx->builtin_typs_by_name[n] == t) {
        return strdup(predefined_idents_strings[n]);
      }
    }
  }
  return NULL;
}

extern char *stpcpy(char *dest, const char *src);
char *typ_pretty_name(const struct module *mod, const struct typ *t) {
  char *r = calloc(2048, sizeof(char));
  char *s = r;

  if (typ_generic_arity(t) == 0) {
    s = stpcpy(s, typ_name(mod, t));
  } else if (typ_isa(t, TBI_ANY_TUPLE)) {
    if (typ_is_generic_functor(t)) {
      s = stpcpy(s, "functor ");
    }

    for (size_t n = 0; n < typ_generic_arity(t); ++n) {
      if (n > 0) {
        s = stpcpy(s, ", ");
      }
      char *s2 = typ_pretty_name(mod, typ_generic_arg_const(t, n));
      s = stpcpy(s, s2);
      free(s2);
    }
  } else {
    s = stpcpy(s, "(");
    if (typ_is_generic_functor(t)) {
      s = stpcpy(s, "functor ");
      s = stpcpy(s, typ_name(mod, t));
    } else {
      const struct typ *f = typ_generic_functor_const(t);
      s = stpcpy(s, typ_name(mod, f));
    }

    for (size_t n = 0; n < typ_generic_arity(t); ++n) {
      const struct typ *ga = typ_generic_arg_const(t, n);
      if (ga == NULL) {
        s = stpcpy(s, " null");
      } else {
        char *s2 = typ_pretty_name(mod, ga);
        s = stpcpy(s, " ");
s = stpcpy(s, s2);
        free(s2);
      }
    }
    s = stpcpy(s, ")");
  }

  return r;
}

error mk_except(const struct module *mod, const struct node *node,
                const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  char s[2048];
  vsnprintf(s, ARRAY_SIZE(s), fmt, ap);

  const struct module *actual_mod = try_node_module_owner_const(mod, node);

  struct token tok = { 0 };
  tok.value = mod->parser.data + node->codeloc;

  error e = 0;
  GOTO_EXCEPTF(EINVAL, "%s:%d:%d: %s",
               actual_mod->filename, line(&actual_mod->parser, &tok),
               column(&actual_mod->parser, &tok), s);

except:
  va_end(ap);
  return e;
}

error mk_except_type(const struct module *mod, const struct node *node,
                     const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  char s[2048];
  vsnprintf(s, ARRAY_SIZE(s), fmt, ap);

  const struct module *actual_mod = try_node_module_owner_const(mod, node);

  struct token tok = { 0 };
  tok.value = mod->parser.data + node->codeloc;

  error e = 0;
  GOTO_EXCEPTF(EINVAL, "%s:%d:%d: type: %s",
               actual_mod->filename, line(&actual_mod->parser, &tok),
               column(&actual_mod->parser, &tok), s);

except:
  va_end(ap);
  return e;
}

error mk_except_call_args_count(const struct module *mod, const struct node *node,
                                const struct node *definition, size_t extra, size_t given) {
  error e = mk_except_type(mod, node,
                           "invalid number of arguments: %zu expected, but %zu given",
                           node_fun_explicit_args_count(definition) + extra, given);
  THROW(e);
}

void rew_insert_last_at(struct node *node, size_t pos) {
  if (pos == node->subs_count - 1) {
    return;
  }

  struct node *tmp = node->subs[pos];
  node->subs[pos] = node->subs[node->subs_count - 1];
  for (size_t n = pos + 1; n < node->subs_count - 1; ++n) {
    SWAP(node->subs[n], tmp);
  }
  node->subs[node->subs_count - 1] = tmp;
}

void rew_pop(struct node *node, bool saved_it) {
  struct node *would_leak = node->subs[node->subs_count - 1];
  // Ensures there is nothing to leak.
  assert(saved_it || (would_leak->which == IDENT
                      || would_leak->which == DIRECTDEF
                      || would_leak->which == BLOCK));
  node->subs_count -= 1;
  node->subs = realloc(node->subs, node->subs_count * sizeof(node->subs[0]));
}

void rew_move_last_over(struct node *node, size_t pos, bool saved_it) {
  struct node *would_leak = node->subs[pos];
  // Ensures there is nothing to leak.
  assert(saved_it || (would_leak->which == IDENT
                      || would_leak->which == EXCEP
                      || would_leak->which == DIRECTDEF
                      || would_leak->which == BLOCK));
  node->subs[pos] = node->subs[node->subs_count - 1];
  node->subs_count -= 1;
  node->subs = realloc(node->subs, node->subs_count * sizeof(node->subs[0]));
}

void rew_prepend(struct node *node, struct node *sub) {
  rew_append(node, sub);
  rew_insert_last_at(node, 0);
}

void rew_append(struct node *node, struct node *sub) {
  node->subs_count += 1;
  node->subs = realloc(node->subs, node->subs_count * sizeof(node->subs[0]));
  node->subs[node->subs_count - 1] = sub;
}

size_t rew_find_subnode_in_parent(const struct node *parent, const struct node *node) {
  for (size_t n = 0; n < parent->subs_count; ++n) {
    if (parent->subs[n] == node) {
      return n;
    }
  }

  assert(FALSE);
  return 0;
}
