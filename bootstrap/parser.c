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
#include "scope.h"
#include "passes.h"

#define MEMPOOL_CHUNK (64*1024)

#ifdef CONFIG_MEMPOOL_JUST_MALLOC
noinline__ void *mempool_calloc(struct module *mod, size_t nmemb, size_t size) {
  (void) mod;
  return calloc(nmemb, size);
}
#else
noinline__ void *mempool_calloc(struct module *mod, size_t nmemb, size_t size) {
  struct mempool *mempool = mod->mempool;

  const size_t sz = nmemb * size;
  assert(likely(sz <= MEMPOOL_CHUNK && sz != 0));

  void *f = mempool->free;
  mempool->free += sz;
  if (likely((uintptr_t)mempool->free <= (uintptr_t)mempool->end)) {
    return f;
  }

  mempool->free -= sz;

  PUSH_STATE(mod->mempool);
  uint8_t *g = calloc(1, MEMPOOL_CHUNK);
  mod->mempool->free = g + sz;
  mod->mempool->end = mod->mempool->free + (MEMPOOL_CHUNK - sz);

  return g;
}
#endif

EXAMPLE(data_structure_size_stats) {
  // It is a good idea to keep track of what is responsible for the size of
  // 'node_as'. In other words, where to look first to shrink 'struct node'.
  assert(sizeof(struct node_defmethod) == sizeof(union node_as));

  // If not, we'll need to store step masks differently.
  assert(NODE__NUM <= 64);
}

const char *node_which_strings[] = {
  [NUL] = "NUL",
  [IDENT] = "IDENT",
  [NUMBER] = "NUMBER",
  [BOOL] = "BOOL",
  [STRING] = "STRING",
  [SIZEOF] = "SIZEOF",
  [ALIGNOF] = "ALIGNOF",
  [BIN] = "BIN",
  [UN] = "UN",
  [TUPLE] = "TUPLE",
  [TUPLEEXTRACT] = "TUPLEEXTRACT",
  [TUPLENTH] = "TUPLENTH",
  [CALL] = "CALL",
  [CALLNAMEDARG] = "CALLNAMEDARG",
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
  [DEFINCOMPLETE] = "DEFINCOMPLETE",
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
  [WITHIN] = "WITHIN",
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
  [ID_PRE] = "__pre__",
  [ID_POST] = "__post__",
  [ID_INVARIANT] = "__invariant__",
  [ID_EXAMPLE] = "__example__",
  [ID_THIS] = "this",
  [ID_FINAL] = "final",
  [ID_SELF] = "self",
  [ID_OTHERWISE] = "_",
  [ID_THROW] = "throw",
  [ID_MAIN] = "main",
  [ID_TAG] = "tag",
  [ID_FIRST_TAG] = "first_tag",
  [ID_LAST_TAG] = "last_tag",
  [ID_AS] = "as",
  [ID_TAG_TYPE] = "tag_type",
  [ID_AS_TYPE] = "as_type",
  [ID_HAS_NEXT] = "has_next",
  [ID_NEXT] = "next",
  [ID_CAST] = "cast",
  [ID_WILDCARD_REF_ARG] = "__wildcard_ref_arg__",
  [ID_LIKELY] = "likely",
  [ID_UNLIKELY] = "unlikely",
  [ID_NLANG] = "nlang",
  [ID_TBI_VOID] = "void",
  [ID_TBI_LITERALS_NULL] = "__literal_null__",
  [ID_TBI_LITERALS_INTEGER] = "__literal_integer__",
  [ID_TBI_LITERALS_FLOATING] = "__literal_floating__",
  [ID_TBI_ANY] = "`any",
  [ID_TBI_ANY_TUPLE] = "`any_tuple",
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
  [ID_TBI_BOOL_COMPATIBLE] = "`bool_compatible",
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
  [ID_TBI_STATIC_STRING_COMPATIBLE] = "`static_string_compatible",
  [ID_TBI_STATIC_ARRAY] = "static_array",
  [ID_TBI__REF_COMPATIBLE] = "`__ref_compatible",
  [ID_TBI_ANY_ANY_REF] = "`any_any_ref",
  [ID_TBI_ANY_REF] = "`any_ref",
  [ID_TBI_ANY_MUTABLE_REF] = "`any_mutable_ref",
  [ID_TBI_ANY_NULLABLE_REF] = "`any_nullable_ref",
  [ID_TBI_ANY_NULLABLE_MUTABLE_REF] = "`any_nullable_mutable_ref",
  [ID_TBI_REF] = "ref",
  [ID_TBI_MREF] = "mutable_ref",
  [ID_TBI_MMREF] = "mercurial_ref",
  [ID_TBI_NREF] = "nullable_ref",
  [ID_TBI_NMREF] = "nullable_mutable_ref",
  [ID_TBI_NMMREF] = "nullable_mercurial_ref",
  [ID_TBI_VARARG] = "vararg",
  [ID_TBI_ARITHMETIC] = "`arithmetic",
  [ID_TBI_BITWISE] = "`bitwise",
  [ID_TBI_INTEGER] = "`integer",
  [ID_TBI_UNSIGNED_INTEGER] = "`unsigned_integer",
  [ID_TBI_NATIVE_INTEGER] = "`native_integer",
  [ID_TBI_NATIVE_ANYSIGN_INTEGER] = "`native_anysign_integer",
  [ID_TBI_GENERALIZED_BOOLEAN] = "`generalized_boolean",
  [ID_TBI_NATIVE_BOOLEAN] = "`native_boolean",
  [ID_TBI_FLOATING] = "`floating",
  [ID_TBI_NATIVE_FLOATING] = "`native_floating",
  [ID_TBI_HAS_EQUALITY] = "`has_equality",
  [ID_TBI_ORDERED] = "`ordered",
  [ID_TBI_ORDERED_BY_COMPARE] = "`ordered_by_compare",
  [ID_TBI_COPYABLE] = "`copyable",
  [ID_TBI_DEFAULT_CTOR] = "`default_ctor",
  [ID_TBI_CTOR_WITH] = "`ctor_with",
  [ID_TBI_ARRAY_CTOR] = "`array_ctor",
  [ID_TBI_TRIVIAL_COPY] = "`trivial_copy",
  [ID_TBI_TRIVIAL_CTOR] = "`trivial_ctor",
  [ID_TBI_TRIVIAL_ARRAY_CTOR] = "`trivial_array_ctor",
  [ID_TBI_TRIVIAL_DTOR] = "`trivial_dtor",
  [ID_TBI_TRIVIAL_EQUALITY] = "`trivial_equality",
  [ID_TBI_TRIVIAL_ORDER] = "`trivial_order",
  [ID_TBI_RETURN_BY_COPY] = "`return_by_copy",
  [ID_TBI_ENUM] = "`enum",
  [ID_TBI_ITERATOR] = "`iterator",
  [ID_TBI_ENVIRONMENT] = "`environment",
  [ID_TBI_ANY_ENVIRONMENT] = "`any_environment",
  [ID_TBI_PREVENT_DYN] = "`prevent_dyn",
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
  [BG_DEFAULT_CTOR_CTOR] = "nlang.builtins.`default_ctor.ctor",
  [BG_DEFAULT_CTOR_MK] = "nlang.builtins.`default_ctor.mk",
  [BG_DEFAULT_CTOR_NEW] = "nlang.builtins.`default_ctor.new",
  [BG_TRIVIAL_CTOR_CTOR] = "nlang.builtins.`trivial_ctor.ctor",
  [BG_TRIVIAL_CTOR_MK] = "nlang.builtins.`trivial_ctor.mk",
  [BG_TRIVIAL_CTOR_NEW] = "nlang.builtins.`trivial_ctor.new",
  [BG_AUTO_MK] = "nlang.builtins.`auto_ctor.mk",
  [BG_AUTO_NEW] = "nlang.builtins.`auto_ctor.new",
  // These should be templates of `ctor_with.
  [BG_CTOR_WITH_MK] = "nlang.builtins.`ctor_with.mk",
  [BG_CTOR_WITH_NEW] = "nlang.builtins.`ctor_with.new",
  [BG_AUTO_MKV] = "nlang.builtins.`array_ctor.mkv",
  [BG_AUTO_NEWV] = "nlang.builtins.`array_ctor.newv",

  [BG_TRIVIAL_COPY_COPY_CTOR] = "nlang.builtins.`copyable.copy_ctor",
  [BG_TRIVIAL_EQUALITY_OPERATOR_EQ] = "nlang.builtins.`has_equality.operator_eq",
  [BG_TRIVIAL_EQUALITY_OPERATOR_NE] = "nlang.builtins.`has_equality.operator_ne",
  [BG_ENVIRONMENT_PARENT] = "parent",
  [BG_ENVIRONMENT_INSTALL] = "install",
  [BG_ENVIRONMENT_UNINSTALL] = "uninstall",
};

HTABLE_SPARSE(idents_map, ident, struct token);
implement_htable_sparse(unused__ static, idents_map, ident, struct token);

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

int parser_line(const struct parser *parser, const struct token *tok) {
  const size_t pos = tok->value != NULL ? tok->value - parser->data : parser->pos;
  int count = 1;
  for (size_t p = 0; p < pos; ++p) {
    if (parser->data[p] == '\n') {
      count += 1;
    }
  }
  return count;
}

int parser_column(const struct parser *parser, const struct token *tok) {
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

#define THROW_SYNTAX(mod, tok, fmt, ...) do { \
  THROWF(EINVAL, "%s:%d:%d: syntax: " fmt, \
         mod->filename, parser_line(&mod->parser, tok), parser_column(&mod->parser, tok), ##__VA_ARGS__); \
} while (0)

#define UNEXPECTED(mod, tok) do { \
  THROW_SYNTAX(mod, tok, "unexpected token '%.*s'", (int)(tok)->len, (tok)->value); \
} while (0)

static struct node *do_node_module_owner(struct node *node) {
  assert(node->which != ROOT_OF_ALL);
  if (node->which == MODULE) {
    return node;
  } else {
    if (node->scope.parent == NULL) {
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
    return toplevel->flags & TOP_IS_PROTOTYPE;
  }
}

bool node_is_inline(const struct node *node) {
  const struct toplevel *toplevel = node_toplevel_const(node);
  if (toplevel == NULL) {
    return FALSE;
  } else {
    return toplevel->flags & TOP_IS_INLINE;
  }
}

bool node_is_export(const struct node *node) {
  const struct toplevel *toplevel = node_toplevel_const(node);
  if (toplevel == NULL) {
    return FALSE;
  } else {
    return toplevel->flags & TOP_IS_EXPORT;
  }
}

bool node_is_extern(const struct node *node) {
  const struct toplevel *toplevel = node_toplevel_const(node);
  if (toplevel == NULL) {
    return FALSE;
  } else {
    return toplevel->flags & TOP_IS_EXTERN;
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
  return node->scope.parent != NULL
    && node_parent_const(node)->which == BLOCK;
}

bool node_is_at_top(const struct node *node) {
  if (node->scope.parent == NULL) {
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
    return node_is_rvalue(node_subs_first_const(node));
  case BLOCK:
    return node_subs_count_atleast(node, 1)
      && node_is_rvalue(node_subs_last_const(node));
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

static error parse_modpath(struct module *mod, const char *raw_fn) {
  const char *fn = raw_fn;
  while (fn[0] == '/' || fn[0] == '.') {
    fn += 1;
  }

  mod->path_len = 0;
  bool must_be_last = FALSE;
  for (size_t n = 0, last = 0, p = 0; fn[p] != '\0'; ++p) {
    if (fn[p] == '_') {
      THROWF(EINVAL, "module path element cannot contain '_' in '%s'", fn);
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
          THROWF(EINVAL, "module path '%s' has too many elements", fn);
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
        THROWF(EINVAL, "module path '%s' contains the"
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
  TBI_VARARG = gctx->builtin_typs_by_name[ID_TBI_VARARG]; // ?@#
  TBI_ARITHMETIC = gctx->builtin_typs_by_name[ID_TBI_ARITHMETIC];
  TBI_BITWISE = gctx->builtin_typs_by_name[ID_TBI_BITWISE];
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
  TBI_ENUM = gctx->builtin_typs_by_name[ID_TBI_ENUM];
  TBI_ITERATOR = gctx->builtin_typs_by_name[ID_TBI_ITERATOR];
  TBI_ENVIRONMENT = gctx->builtin_typs_by_name[ID_TBI_ENVIRONMENT];
  TBI_ANY_ENVIRONMENT = gctx->builtin_typs_by_name[ID_TBI_ANY_ENVIRONMENT];
  TBI_PREVENT_DYN = gctx->builtin_typs_by_name[ID_TBI_PREVENT_DYN];
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

  scope_init(&gctx->modules_root.scope);
  node_set_which(&gctx->modules_root, ROOT_OF_ALL);
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
    THROWF(errno, "Cannot open module '%s'", fullpath);
  }

  struct stat st = { 0 };
  error e = fstat(fd, &st);
  if (e < 0) {
    THROWF(errno, "Cannot stat module '%s'", fullpath);
  }

  char *data = calloc(st.st_size + 1, sizeof(char));
  ssize_t count = read(fd, data, st.st_size);
  if (count < 0) {
    THROWF(errno, "Error reading module '%s'", fullpath);
  } else if (count != (ssize_t) st.st_size) {
    THROWF(errno, "Reading module '%s': Partial read not supported by parser", fullpath);
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
    THROW_SYNTAX(mod, tok, "%s", mod->parser.error_message);
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

void node_invariant(const struct node *node) {
  assert(node_subs_count(node) != 1 || node->subs_first == node->subs_last);
  assert(node->subs_first != node && node->subs_last != node);

  ssize_t n = 0;
  FOREACH_SUB_CONST(s, node) {
    if (n == 0) {
      assert(s->prev == NULL);
      assert(s == node->subs_first);
    } else {
      assert(s->prev != NULL);
      assert(s != node->subs_first);
    }
    if (s->next == NULL) {
      assert(s == node->subs_last);
    } else {
      assert(s != node->subs_last);
    }
    n += 1;
  }

  switch (node->which) {
  case IF:
    assert(node_subs_count_atleast(node, 2));
    break;
  case DEFNAME:
    assert(node->as.DEFNAME.pattern == NULL
           || node->as.DEFNAME.pattern->which == IDENT);
    break;
  default:
    break;
  }

  assert(node_parent_const(node) != node);
}

static void fix_scopes_after_move(struct node *node) {
  FOREACH_SUB(s, node) {
    s->scope.parent = &node->scope;
  }
}

void node_move_content(struct node *dst, struct node *src) {
  struct node copy = *src;
  struct scope *saved_dst_parent = dst->scope.parent;

  unset_typ(&src->typ);
  unset_typ(&dst->typ);

  memset(src, 0, sizeof(*src));

  src->prev = node_prev(&copy);
  src->next = node_next(&copy);

  struct node *prev = node_prev(dst);
  struct node *next = node_next(dst);

  *dst = copy;
  dst->scope.parent = saved_dst_parent;

  dst->prev = prev;
  dst->next = next;
  dst->subs_first = node_subs_first(&copy);
  dst->subs_last = node_subs_last(&copy);

  fix_scopes_after_move(dst);
}

struct node *node_new_subnode(struct module *mod, struct node *node) {
  struct node *r = mempool_calloc(mod, 1, sizeof(struct node));
  node_subs_append(node, r);

  if (mod->parser.pos >= mod->parser.len) {
    // It's a node inserted after parsing.
    r->codeloc = node->codeloc;
  } else {
    r->codeloc = mod->parser.pos;
  }

  return r;
}

EXAMPLE(node_subs) {
  struct node n = { 0 };
  struct node a = { 0 };
  assert(node_subs_count_atleast(&n, 0));
  assert(!node_subs_count_atleast(&n, 1));
  node_subs_append(&n, &a);
  assert(node_subs_count(&n) == 1);
  assert(node_subs_count_atleast(&n, 0));
  assert(node_subs_count_atleast(&n, 1));
  assert(!node_subs_count_atleast(&n, 2));
  assert(&a == node_subs_first(&n));
  assert(&a == node_subs_last(&n));
  assert(NULL == node_next(&a));
  assert(NULL == node_prev(&a));

  struct node b = { 0 };
  node_subs_append(&n, &b);
  assert(node_subs_count(&n) == 2);
  assert(node_subs_count_atleast(&n, 0));
  assert(node_subs_count_atleast(&n, 1));
  assert(node_subs_count_atleast(&n, 2));
  assert(!node_subs_count_atleast(&n, 3));
  assert(&a == node_subs_first(&n));
  assert(&b == node_subs_last(&n));
  assert(&b == node_next(&a));
  assert(NULL == node_prev(&a));
  assert(NULL == node_next(&b));
  assert(&a == node_prev(&b));

  struct node c = { 0 };
  node_subs_replace(&n, &a, &c);
  assert(node_next(&a) == NULL);
  assert(node_prev(&a) == NULL);
  assert(node_subs_count(&n) == 2);
  assert(node_subs_count_atleast(&n, 0));
  assert(node_subs_count_atleast(&n, 1));
  assert(node_subs_count_atleast(&n, 2));
  assert(!node_subs_count_atleast(&n, 3));
  assert(&c == node_subs_first(&n));
  assert(&b == node_subs_last(&n));
  assert(&b == node_next(&c));
  assert(NULL == node_prev(&c));
  assert(NULL == node_next(&b));
  assert(&c == node_prev(&b));

  node_subs_insert_after(&n, &c, &a);
  assert(node_subs_count(&n) == 3);
  assert(&c == node_subs_first(&n));
  assert(&b == node_subs_last(&n));
  assert(&a == node_next(&c));
  assert(&b == node_next(&a));
  assert(NULL == node_next(&b));
  assert(NULL == node_prev(&c));
  assert(&c == node_prev(&a));
  assert(&a == node_prev(&b));

  node_subs_remove(&n, &b);
  assert(node_subs_count(&n) == 2);
  assert(&c == node_subs_first(&n));
  assert(&a == node_subs_last(&n));
  assert(&a == node_next(&c));
  assert(NULL == node_prev(&c));
  assert(NULL == node_next(&a));
  assert(&c == node_prev(&a));

  node_subs_insert_before(&n, &a, &b);
  assert(node_subs_count(&n) == 3);
  assert(&c == node_subs_first(&n));
  assert(&a == node_subs_last(&n));
  assert(&b == node_next(&c));
  assert(&a == node_next(&b));
  assert(NULL == node_next(&a));
  assert(NULL == node_prev(&c));
  assert(&c == node_prev(&b));
  assert(&b == node_prev(&a));
}

bool node_has_tail_block(const struct node *node) {
  return node_subs_count_atleast(node, 1)
    && node_subs_last_const(node)->which == BLOCK;
}

bool node_is_fun(const struct node *node) {
  return node->which == DEFFUN || node->which == DEFMETHOD;
}

size_t node_fun_all_args_count(const struct node *def) {
  return node_subs_count(node_subs_at_const(def, IDX_FUNARGS)) - 1;
}

size_t node_fun_min_args_count(const struct node *def) {
  switch(def->which) {
  case DEFFUN:
    return def->as.DEFFUN.min_args;
  case DEFMETHOD:
    return def->as.DEFMETHOD.min_args;
  default:
    assert(FALSE);
    return -1;
  }
}

size_t node_fun_max_args_count(const struct node *def) {
  switch(def->which) {
  case DEFFUN:
    return def->as.DEFFUN.max_args;
  case DEFMETHOD:
    return def->as.DEFMETHOD.max_args;
  default:
    assert(FALSE);
    return -1;
  }
}

ssize_t node_fun_first_vararg(const struct node *def) {
  switch(def->which) {
  case DEFFUN:
    return def->as.DEFFUN.first_vararg;
  case DEFMETHOD:
    return def->as.DEFMETHOD.first_vararg;
  default:
    assert(FALSE);
    return -1;
  }
}

const struct node *node_fun_retval_const(const struct node *def) {
  assert(def->which == DEFFUN || def->which == DEFMETHOD);
  const struct node *funargs = node_subs_at_const(def, IDX_FUNARGS);
  return node_subs_last_const(funargs);
}

struct node *node_fun_retval(struct node *def) {
  return (struct node *) node_fun_retval_const(def);
}

struct node *node_get_member(struct module *mod, struct node *node, ident id) {
  assert(node->which == DEFTYPE || node->which == DEFCHOICE || node->which == DEFINTF);
  struct node *m = NULL;
  (void)scope_lookup_ident_immediate(&m, node, mod, &node->scope, id, TRUE);
  return m;
}

const struct node *node_get_member_const(const struct module *mod, const struct node *node, ident id) {
  return node_get_member((struct module *)mod, (struct node *)node, id);
}

struct node *mk_node(struct module *mod, struct node *parent, enum node_which kind) {
  struct node *n = node_new_subnode(mod, parent);
  node_set_which(n, kind);
  return n;
}

static error p_expr(struct node *node, struct module *mod, uint32_t parent_op);
static error p_block(struct node *node, struct module *mod);

static error p_ident(struct node *node, struct module *mod) {
  struct token tok = { 0 };
  error e = scan_oneof(&tok, mod, TIDENT, 0);
  EXCEPT(e);

  node_set_which(node, IDENT);
  node->as.IDENT.name = idents_add(mod->gctx, &tok);

  return 0;
}

static error p_number(struct node *node, struct module *mod) {
  struct token tok = { 0 };
  error e = scan_oneof(&tok, mod, TNUMBER, 0);
  EXCEPT(e);

  char *cpy = calloc(tok.len + 1, sizeof(char));
  memcpy(cpy, tok.value, tok.len);

  node_set_which(node, NUMBER);
  node->as.NUMBER.value = cpy;

  return 0;
}

static error p_bool(struct node *node, struct module *mod) {
  struct token tok = { 0 };
  error e = scan_oneof(&tok, mod, Tfalse, Ttrue, 0);
  EXCEPT(e);

  node_set_which(node, BOOL);
  node->as.BOOL.value = tok.t == Ttrue;
  return 0;
}

static error p_except(struct node *node, struct module *mod) {
  node_set_which(node, EXCEP);
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

  node_set_which(node, STRING);
  node->as.STRING.value = cpy;

  return 0;
}

static error p_typeexpr(struct node *node, struct module *mod) {
  error e = p_expr(node, mod, T__CALL);
  EXCEPT(e);
  return 0;
}

static error p_deffield(struct node *node, struct module *mod) {
  node_set_which(node, DEFFIELD);
  error e = p_ident(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  e = scan_expected(mod, TCOLON);
  EXCEPT(e);

  e = p_typeexpr(node_new_subnode(mod, node), mod);
  EXCEPT(e);
  return 0;
}

static error p_deftype_block(struct node *node, struct module *mod);

static error p_defchoice(struct node *node, struct module *mod) {
  node_set_which(node, DEFCHOICE);
  error e = p_ident(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  struct token tok = { 0 };
  e = scan(&tok, mod);
  EXCEPT(e);

  if (tok.t == TASSIGN) {
    node->as.DEFCHOICE.has_tag = TRUE;

    e = p_expr(node_new_subnode(mod, node), mod, T__NOT_STATEMENT);
    EXCEPT(e);

    e = scan(&tok, mod);
    EXCEPT(e);
  }

  if (tok.t == TSOB) {
    node->as.DEFCHOICE.has_payload = TRUE;

    e = p_deftype_block(node, mod);
    EXCEPT(e);
  } else {
    back(mod, &tok);
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

  node_set_which(node, UN);
  node->as.UN.operator = op;

  e = p_expr(node_new_subnode(mod, node), mod, op);
  EXCEPT(e);

  return 0;
}

static error p_expr_init(struct node *node, struct module *mod) {
  node_set_which(node, INIT);

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
        THROW_SYNTAX(mod, &assign, "array initializer should not contain named pairs");
      }

      e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
      EXCEPT(e);
    }
  }
}

static error p_expr_tuple(struct node *node, struct module *mod) {
  node_set_which(node, TUPLE);
  error e;
  struct token tok = { 0 };

  size_t count = 1;
  while (TRUE) {
    e = scan(&tok, mod);
    EXCEPT(e);

    if (tok.t != TCOMMA) {
      back(mod, &tok);

      if (count > 16) {
        THROW_SYNTAX(mod, &tok, "tuples can have no more than 16 elements");
      }

      return 0;
    }

    e = p_expr(node_new_subnode(mod, node), mod, TCOMMA);
    EXCEPT(e);

    count += 1;
  }
}

static error p_expr_call(struct node *node, struct module *mod) {
  node_set_which(node, CALL);
  error e;
  struct token tok = { 0 };

  while (TRUE) {
    e = scan(&tok, mod);
    EXCEPT(e);
    back(mod, &tok);

    if (expr_terminators[tok.t]) {
      return 0;
    } else if (OP_PREC(tok.t) > OP_PREC(T__CALL)) {
      return 0;
    }

    struct node *tentative = node_new_subnode(mod, node);
    e = p_expr(tentative, mod, T__CALL);
    EXCEPT(e);

    if (tentative->which == IDENT) {
      e = scan(&tok, mod);
      EXCEPT(e);

      if (tok.t == TASSIGN) {
        const ident name = node_ident(tentative);
        node_set_which(tentative, CALLNAMEDARG);
        tentative->as.CALLNAMEDARG.name = name;

        e = p_expr(node_new_subnode(mod, tentative), mod, T__CALL);
        EXCEPT(e);
      } else {
        back(mod, &tok);
      }
    }
  }
}

static error p_expr_binary(struct node *node, struct module *mod) {
  struct token tok = { 0 };
  error e = scan(&tok, mod);
  EXCEPT(e);

  assert(tok.t != TCOMMA);
  if (tok.t == TCOLON) {
    node_set_which(node, TYPECONSTRAINT);
  } else {
    node_set_which(node, BIN);
    node->as.BIN.operator = tok.t;
  }

  if (tok.t == TCOLON) {
    e = p_typeexpr(node_new_subnode(mod, node), mod);
    EXCEPT(e);
  } else {
    e = p_expr(node_new_subnode(mod, node), mod, tok.t);
    EXCEPT(e);
  }

  return 0;
}

static error p_expr_post_unary(struct node *node, struct module *mod) {
  struct token tok = { 0 };
  error e = scan(&tok, mod);
  EXCEPT(e);

  assert(OP_KIND(tok.t) == OP_UN_DEREF);
  node_set_which(node, UN);
  node->as.UN.operator = tok.t;

  return 0;
}

static error p_if(struct node *node, struct module *mod) {
  node_set_which(node, IF);

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
  node_set_which(node, FOR);

  node->as.FOR.pattern = calloc(1, sizeof(*node->as.FOR.pattern));
  error e = p_expr(node->as.FOR.pattern, mod, T__NOT_STATEMENT);
  EXCEPT(e);

  e = scan_expected(mod, Tin);
  EXCEPT(e);

  struct node *let_it = mk_node(mod, node, LET);
  struct node *it = mk_node(mod, let_it, DEFPATTERN);
  struct node *it_var = mk_node(mod, it, IDENT);
  it_var->as.IDENT.name = gensym(mod);
  struct node *it_expr = mk_node(mod, it, UN);
  it_expr->as.UN.operator = TREFBANG;

  e = p_expr(node_new_subnode(mod, it_expr), mod, T__NOT_STATEMENT);
  EXCEPT(e);

  struct node *let_it_block = mk_node(mod, let_it, BLOCK);
  struct node *loop = mk_node(mod, let_it_block, WHILE);
  struct node *v = mk_node(mod, loop, CALL);
  struct node *vm = mk_node(mod, v, BIN);
  vm->as.BIN.operator = TDOT;
  struct node *vmi = mk_node(mod, vm, IDENT);
  vmi->as.IDENT.name = node_ident(it_var);
  struct node *vmm = mk_node(mod, vm, IDENT);
  vmm->as.IDENT.name = ID_HAS_NEXT;

  struct node *loop_block = mk_node(mod, loop, BLOCK);
  struct node *let_var = mk_node(mod, loop_block, LET);
  struct node *var = mk_node(mod, let_var, DEFPATTERN);
  node_subs_append(var, node->as.FOR.pattern);
  struct node *g = mk_node(mod, var, CALL);
  struct node *gm = mk_node(mod, g, BIN);
  gm->as.BIN.operator = TBANG;
  struct node *gmi = mk_node(mod, gm, IDENT);
  gmi->as.IDENT.name = node_ident(it_var);
  struct node *gmm = mk_node(mod, gm, IDENT);
  gmm->as.IDENT.name = ID_NEXT;

  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  node->as.FOR.block = mk_node(mod, let_var, BLOCK);
  e = p_block(node->as.FOR.block, mod);
  EXCEPT(e);

  return 0;
}

struct node *node_for_block(struct node *node) {
 assert(node->which == FOR);
 return node_subs_at(node_subs_at(node_subs_at(node_subs_at(
         node, IDX_FOR_IT),
       IDX_FOR_IT_BLOCK),
     IDX_FOR_IT_BLOCK_WHILE),
   IDX_FOR_IT_BLOCK_WHILE_BLOCK);
}

static error p_while(struct node *node, struct module *mod) {
  node_set_which(node, WHILE);

  error e = p_expr(node_new_subnode(mod, node), mod, T__NOT_STATEMENT);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_try(struct node *node, struct module *mod) {
  node_set_which(node, TRY);

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
  THROW_SYNTAX(mod, &tok, "to use multiple catch in a try block, each must have a label");
  return 0;
}

static error p_match(struct node *node, struct module *mod) {
  node_set_which(node, MATCH);

  error e = p_expr(node_new_subnode(mod, node), mod, T__NOT_STATEMENT);
  EXCEPT(e);
  e = scan_expected(mod, TEOL);
  EXCEPT(e);

  struct token tok = { 0 };
again:
  e = scan(&tok, mod);
  EXCEPT(e);
  if (tok.t != TPATTERNOR) {
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

static bool mixing_arith_and_bw(enum token_type a, enum token_type b) {
  return (OP_KIND(a) == OP_BIN_SYM_ARITH
          && (OP_KIND(b) == OP_BIN_SYM_BW
              || OP_KIND(b) == OP_BIN_BW_RHS_UNSIGNED))
    || (OP_KIND(b) == OP_BIN_SYM_ARITH
          && (OP_KIND(a) == OP_BIN_SYM_BW
              || OP_KIND(a) == OP_BIN_BW_RHS_UNSIGNED));
}

EXAMPLE(mixing_arith_and_bw) {
  assert(mixing_arith_and_bw(TPLUS, TBWOR));
  assert(!mixing_arith_and_bw(TPLUS, TMINUS));
}

static void shift(struct module *mod, struct node *node) {
  struct node *first = node_new_subnode(mod, node);
  node_subs_remove(node, first);

  *first = *node;

  node_set_which(node, 0);
  memset(&node->as, 0, sizeof(node->as));
  node->subs_first = first;
  node->subs_last = first;
  first->next = NULL;
  first->prev = NULL;
}

static error p_expr(struct node *node, struct module *mod, uint32_t parent_op) {
  assert(parent_op < TOKEN__NUM && IS_OP(parent_op));

  error e;
  struct token tok = { 0 };
  bool topmost = parent_op == T__STATEMENT;

  e = scan(&tok, mod);
  EXCEPT(e);

  if (tok.t == TBARROW) {
    e = p_expr(node, mod, T__NOT_STATEMENT);
    EXCEPT(e);
  } else if (tok.t == TLPAR) {
    e = p_expr(node, mod, T__NOT_STATEMENT);
    EXCEPT(e);
    e = scan_expected(mod, TRPAR);
    EXCEPT(e);
  } else {

    switch (tok.t) {
    case Tnull:
      node_set_which(node, NUL);
      break;
    case Tsizeof:
      node_set_which(node, SIZEOF);
      e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
      break;
    case Talignof:
      node_set_which(node, ALIGNOF);
      e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
      break;
    case Texcept:
      e = p_except(node, mod);
      break;
    case TIDENT:
      back(mod, &tok);
      e = p_ident(node, mod);
      break;
    case TNUMBER:
      back(mod, &tok);
      e = p_number(node, mod);
      break;
    case Tfalse:
    case Ttrue:
      back(mod, &tok);
      e = p_bool(node, mod);
      break;
    case TSTRING:
      back(mod, &tok);
      e = p_string(node, mod);
      break;
    case TLCBRA:
      back(mod, &tok);
      e = p_expr_init(node, mod);
      break;
    case Tblock:
      e = scan_expected(mod, TSOB);
      EXCEPT(e);
      e = p_block(node, mod);
      break;
    case Tif:
      e = p_if(node, mod);
      break;
    case Ttry:
      e = p_try(node, mod);
      break;
    case Tmatch:
      e = p_match(node, mod);
      break;
    default:
      back(mod, &tok);
      if ((IS_OP(tok.t) && OP_IS_UNARY(tok.t))
          || tok.t == TMINUS || tok.t == TPLUS) { // Unary versions.
        e = p_expr_unary(node, mod);
      } else {
        UNEXPECTED(mod, &tok);
      }
      break;
    }
    EXCEPT(e);
  }

shifted:
  e = scan(&tok, mod);
  EXCEPT(e);
  back(mod, &tok);

  if (parent_op != 0) {
    if (IS_OP(tok.t) && OP_IS_BINARY(tok.t)
        && OP_PREC(tok.t) == OP_PREC(parent_op)) {
      if (OP_ASSOC(tok.t) == ASSOC_LEFT_SAME && tok.t != parent_op) {
        THROW_SYNTAX(mod, &tok,
                      "Operator '%.*s' must be parenthesized when combined"
                      " with operators of the same precedence",
                      (int)tok.len, tok.value);
      } else if (OP_ASSOC(tok.t) == ASSOC_NON) {
        THROW_SYNTAX(mod, &tok,
                      "Operator '%.*s' is non-associative, use parentheses",
                      (int)tok.len, tok.value);
      }
    } else if (mixing_arith_and_bw(tok.t, parent_op)) {
      THROW_SYNTAX(mod, &tok, "Combining arithmetic and bitwise"
                    " operators requires parentheses");
    }
  }

  if (expr_terminators[tok.t]) {
    goto done;
  } else if (tok.t != TBARROW && IS_OP(tok.t) && OP_IS_BINARY(tok.t)) {
    if (tok.t == TCOMMA) {
      if (OP_PREC(tok.t) < OP_PREC(parent_op)
          || topmost) {
        shift(mod, node);
        e = p_expr_tuple(node, mod);
        EXCEPT(e);

        goto shifted;
      } else {
        goto done;
      }
    } else if (OP_PREC(tok.t) < OP_PREC(parent_op)
               || (OP_PREC(tok.t) == OP_PREC(parent_op)
                   && OP_ASSOC(tok.t) == ASSOC_RIGHT)
               || topmost) {
      shift(mod, node);
      e = p_expr_binary(node, mod);
      EXCEPT(e);

      if (topmost) {
        parent_op = tok.t;
      }

      goto shifted;
    } else {
      goto done;
    }
  } else if (IS_OP(tok.t) && OP_IS_UNARY(tok.t) && OP_KIND(tok.t) == OP_UN_DEREF) {
    shift(mod, node);
    e = p_expr_post_unary(node, mod);
    EXCEPT(e);

    if (topmost) {
      parent_op = tok.t;
    }

    goto shifted;
  } else if (OP_PREC(T__CALL) < OP_PREC(parent_op)
             || topmost) {
    shift(mod, node);
    e = p_expr_call(node, mod);
    if (topmost) {
      parent_op = T__CALL;
    }
    EXCEPT(e);

    goto shifted;
  } else {
    goto done;
  }

done:
  return 0;
}

static error p_return(struct node *node, struct module *mod) {
  node_set_which(node, RETURN);

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
  node_set_which(node, THROW);

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
                          enum token_type let_alias_globalenv) {
  node_set_which(node, DEFPATTERN);
  node->as.DEFPATTERN.is_alias = let_alias_globalenv == Talias;
  node->as.DEFPATTERN.is_globalenv = let_alias_globalenv == Tglobalenv;

  error e;
  struct node *prev = node_prev(node);
  if (prev != NULL
      && (prev->as.DEFPATTERN.is_alias || prev->as.DEFPATTERN.is_globalenv)) {
    e = mk_except(mod, node, "cannot use 'and' or 'such'"
                  " after an 'alias' or 'globalenv'");
    THROW(e);
  }

  e = p_expr(node_new_subnode(mod, node), mod, T__NOT_STATEMENT);
  EXCEPT(e);

  if (node->as.DEFPATTERN.is_globalenv) {
    struct node *n = node_subs_first(node);
    if (n->which != TYPECONSTRAINT) {
      e = mk_except(mod, node, "malformed expression, must be:"
                    " globalenv name:type");
      THROW(e);
    }
    return 0;
  }

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

// When let_and_alias_such_globalenv == Tand, node is previous LET we are
// appending ourselves to. Same idea with Tsuch.
static error p_let(struct node *node, struct module *mod, const struct toplevel *toplevel,
                   enum token_type let_and_alias_such_globalenv) {
  if (let_and_alias_such_globalenv == Tsuch) {
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

  } else if (let_and_alias_such_globalenv == Tand) {
    assert(node->which == LET);
    assert(toplevel == NULL);
  } else {
    node_set_which(node, LET);
    if (toplevel != NULL) {
      node->as.LET.toplevel = *toplevel;
      node->flags |= NODE_IS_GLOBAL_LET;
    }
  }

  error e = p_defpattern(node_new_subnode(mod, node), mod,
                         let_and_alias_such_globalenv);
  EXCEPT(e);

  return 0;
}

static error p_break(struct node *node, struct module *mod) {
  node_set_which(node, BREAK);
  return 0;
}

static error p_continue(struct node *node, struct module *mod) {
  node_set_which(node, CONTINUE);
  return 0;
}

static error p_noop(struct node *node, struct module *mod) {
  node_set_which(node, NOOP);
  return 0;
}

static error p_pre(struct node *node, struct module *mod) {
  node_set_which(node, PRE);

  G(block, node, BLOCK,
    G(call, block, CALL,
      G(name, call, IDENT,
        name->as.IDENT.name = ID_PRE)));
  error e = p_expr(node_new_subnode(mod, call), mod, T__CALL);
  EXCEPT(e);

  return 0;
}

static error p_post(struct node *node, struct module *mod) {
  node_set_which(node, POST);

  G(block, node, BLOCK,
    G(call, block, CALL,
      G(name, call, IDENT,
        name->as.IDENT.name = ID_POST)));
  error e = p_expr(node_new_subnode(mod, call), mod, T__CALL);
  EXCEPT(e);

  return 0;
}

static error p_invariant(struct node *node, struct module *mod) {
  node_set_which(node, INVARIANT);

  G(block, node, BLOCK,
    G(call, block, CALL,
      G(name, call, IDENT,
        name->as.IDENT.name = ID_INVARIANT)));
  error e = p_expr(node_new_subnode(mod, call), mod, T__CALL);
  EXCEPT(e);

  return 0;
}

static error p_example(struct node *node, struct module *mod) {
  node_set_which(node, EXAMPLE);
  node->as.EXAMPLE.name = mod->next_example;
  mod->next_example += 1;

  G(block, node, BLOCK,
    G(call, block, CALL,
      G(name, call, IDENT,
        name->as.IDENT.name = ID_EXAMPLE)));
  error e = p_expr(node_new_subnode(mod, call), mod, T__CALL);
  EXCEPT(e);

  return 0;
}

static error p_within(struct node *node, struct module *mod, bool is_list) {
  node_set_which(node, WITHIN);

  struct token tok = { 0 };
  error e;

  if (!is_list) {
    e = p_expr(node_new_subnode(mod, node), mod, T__CALL);
    EXCEPT(e);
    return 0;
  }

  while (TRUE) {
    e = scan(&tok, mod);
    EXCEPT(e);
    back(mod, &tok);

    if (tok.t != TIDENT) {
      break;
    }

    e = p_within(node_new_subnode(mod, node), mod, FALSE);
    EXCEPT(e);
  }

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
              : node_subs_last(parent),
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
    e = p_invariant(NEW, mod);
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
  node_set_which(node, BLOCK);
  error e;
  struct token tok = { 0 };
  bool first = TRUE;

  e = scan(&tok, mod);
  EXCEPT(e);

again:
  if (tok.t == TEOB) {
    if (first) {
      THROW_SYNTAX(mod, &tok, "block cannot be empty (use 'noop' instead)");;
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

static error p_defarg(struct node *node, struct module *mod, enum token_type tokt) {
  node_set_which(node, DEFARG);
  node->as.DEFARG.is_optional = tokt == TQMARK;
  node->as.DEFARG.is_vararg = tokt == TDOTDOTDOT;

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

static STEP_FILTER(step_rewrite_into_defarg,
                   SF(TYPECONSTRAINT));
static error step_rewrite_into_defarg(struct module *mod, struct node *node,
                                      void *user, bool *stop) {
  node_set_which(node, DEFARG);
  return 0;
}

static error pass_rewrite_into_defarg(struct module *mod, struct node *root,
                                      void *user, size_t shallow_last_up) {
  PASS(, UP_STEP(step_rewrite_into_defarg));
  return 0;
}

static error p_defret(struct node *node, struct module *mod) {
  error e = p_expr(node, mod, T__STATEMENT);
  EXCEPT(e);

  e = pass_rewrite_into_defarg(mod, node, NULL, -1);
  EXCEPT(e);

  return 0;
}

static void add_self_arg(struct module *mod, struct node *node,
                         struct node *funargs) {
  struct node *arg = mk_node(mod, funargs, DEFARG);
  struct node *name = mk_node(mod, arg, IDENT);
  name->as.IDENT.name = ID_SELF;

  if (node->as.DEFMETHOD.access == TREFWILDCARD) {
    struct node *genargs = node_subs_at(node, IDX_GENARGS);
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
  struct token tok = { 0 };
  error e = scan(&tok, mod);
  EXCEPT(e);
  switch (tok.t) {
  case TREFDOT:
  case TREFBANG:
  case TREFSHARP:
  case TREFWILDCARD:
    node->as.DEFMETHOD.access = tok.t;
    break;
  default:
    // In 't (method@ u:`any) foobar x:@u = void', p_defmethod_access() is
    // called twice (once for when parsing the genargs, once in p_deffun).
    if (node->as.DEFMETHOD.access == 0) {
      THROW_SYNTAX(mod, &tok, "passing self by value is not supported,"
                    " use one of method{@,@!,@#,@$}");
    }
    back(mod, &tok);
    break;
  }

  return 0;
}

static void count_args(struct node *def) {
  ssize_t *min, *max, *first_va;
  switch(def->which) {
  case DEFFUN:
    min = &def->as.DEFFUN.min_args;
    max = &def->as.DEFFUN.max_args;
    first_va = &def->as.DEFFUN.first_vararg;
    break;
  case DEFMETHOD:
    min = &def->as.DEFMETHOD.min_args;
    max = &def->as.DEFMETHOD.max_args;
    first_va = &def->as.DEFMETHOD.first_vararg;
    break;
  default:
    assert(FALSE);
    return;
  }

  const struct node *funargs = node_subs_at_const(def, IDX_FUNARGS);
  *max = node_fun_all_args_count(def);
  *min = *max;

  *first_va = -1;
  bool last = TRUE;
  REVERSE_FOREACH_SUB_CONST(arg, funargs) {
    if (last) {
      last = FALSE;
      continue;
    }

    assert(arg->which == DEFARG);
    if (arg->as.DEFARG.is_optional) {
      *min -= 1;
    } else if (arg->as.DEFARG.is_vararg) {
      *min -= 1;
      *first_va = *min;
      *max = SSIZE_MAX;
    } else {
      break;
    }
  }
}

static error p_deffun(struct node *node, struct module *mod, const struct toplevel *toplevel,
                      enum node_which fun_or_method) {
  error e;
  struct token tok = { 0 };

  node_set_which(node, fun_or_method);
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

  struct node *name = node_subs_first(node);
  e = p_expr(name, mod, T__CALL);
  EXCEPT(e);

  struct node *funargs = mk_node(mod, node, FUNARGS);
  if (fun_or_method == DEFMETHOD) {
    if (name->which != IDENT && name->which != BIN) {
      THROW_SYNTAX(mod, &tok, "malformed method name");
    }

    add_self_arg(mod, node, funargs);
  } else {
    if (name->which != IDENT) {
      THROW_SYNTAX(mod, &tok, "malformed fun name");
    }
  }

  bool must_be_last = FALSE;
again:
  e = scan_oneof(&tok, mod, TASSIGN, TIDENT, TQMARK, TDOTDOTDOT, 0);
  EXCEPT(e);

  if (must_be_last && tok.t != TASSIGN) {
    THROW_SYNTAX(mod, &tok, "vararg must be last");
  }

  switch (tok.t) {
  case TASSIGN:
    goto retval;
  case TIDENT:
    back(mod, &tok);
    // Fallthrough
  case TQMARK:
  case TDOTDOTDOT:
    {
      struct node *arg = node_new_subnode(mod, funargs);
      e = p_defarg(arg, mod, tok.t);
      EXCEPT(e);
    }
    must_be_last = tok.t == TDOTDOTDOT;
    goto again;
  default:
    assert(FALSE);
  }

retval:
  e = p_defret(node_new_subnode(mod, funargs), mod);
  EXCEPT(e);

  count_args(node);

  e = scan(&tok, mod);
  EXCEPT(e);
  if (tok.t == Twithin) {
    e = p_within(node_new_subnode(mod, node), mod, TRUE);
    EXCEPT(e);
  } else {
    back(mod, &tok);
    (void) mk_node(mod, node, WITHIN);
  }

  e = scan_oneof(&tok, mod, TEOL, TSOB, TEOB, 0);
  EXCEPT(e);

  if (tok.t == TEOL || tok.t == TEOB) {
    back(mod, &tok);
    node_toplevel(node)->flags |= TOP_IS_PROTOTYPE;
    return 0;
  }

  e = p_block(node_new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_isalist(struct node *parent, struct module *mod, bool is_export) {
  struct node *isalist = node_subs_at(parent, IDX_ISALIST);

  error e;
  struct token tok = { 0 };

again:
  e = scan(&tok, mod);
  EXCEPT(e);

  switch (tok.t) {
  case TEOB:
  case TEOL:
    back(mod, &tok);
    return 0;
  default:
    back(mod, &tok);

    struct node *isa = node_new_subnode(mod, isalist);
    node_set_which(isa, ISA);
    isa->as.ISA.is_export = is_export;
    isa->as.ISA.is_explicit = TRUE;

    e = p_expr(node_new_subnode(mod, isa), mod, T__CALL);
    EXCEPT(e);
    goto again;
  }
}

static error p_delegate(struct node *node, struct module *mod,
                        struct toplevel *toplevel) {
  node_set_which(node, DELEGATE);
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

static error p_deftype_statement(struct node *node, struct module *mod,
                                 struct node *deft) {
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
    toplevel.flags |= TOP_IS_EXPORT;
    goto again;
  case Textern:
    toplevel.flags |= TOP_IS_EXTERN;
    goto again;
  case Topaque:
    toplevel.flags |= TOP_IS_OPAQUE;
    goto again;
  case Tinline:
    toplevel.flags |= TOP_IS_INLINE;
    goto again;
  case Tisa:
    node_subs_remove(deft, node);
    e = p_isalist(deft, mod, toplevel.flags & TOP_IS_EXPORT);
    break;
  case Tdelegate:
    e = p_delegate(node, mod, &toplevel);
    break;
  case Tinvariant:
    e = p_invariant(node, mod);
    break;
  case TIDENT:
    back(mod, &tok);
    e = p_deffield(node, mod);
    break;
  case TPATTERNOR:
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
      THROW_SYNTAX(mod, &tok, "block cannot be empty (use 'noop' instead)");;
    } else {
      return 0;
    }
  } else {
    back(mod, &tok);
    e = p_deftype_statement(node_new_subnode(mod, node), mod, node);
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
  node_set_which(node, DEFGENARG);
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
  node_set_which(node, GENARGS);

  error e;
  struct token tok = { 0 };
again:
  e = scan(&tok, mod);
  EXCEPT(e);
  if (tok.t == terminator) {
    if (!explicit && node_subs_first(node) == NULL) {
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
                       struct node *some_genargs, struct toplevel *toplevel,
                       enum token_type decl_tok) {
  if (some_genargs != NULL) {
    toplevel->generic->first_explicit_genarg = node_subs_count(some_genargs);
  }

  node_set_which(node, DEFTYPE);
  node->as.DEFTYPE.toplevel = *toplevel;
  switch (decl_tok) {
  case Tstruct:
    node->as.DEFTYPE.kind = DEFTYPE_STRUCT;
    break;
  case Tenum:
    node->as.DEFTYPE.kind = DEFTYPE_ENUM;
    break;
  case Tunion:
    node->as.DEFTYPE.kind = DEFTYPE_UNION;
    break;
  default:
    assert(FALSE && "Unreached");
    break;
  }

  error e = p_ident(node_subs_first(node), mod);
  EXCEPT(e);

  e = p_genargs(node_subs_at(node, IDX_GENARGS), mod, TASSIGN, TRUE);
  EXCEPT(e);

  (void)mk_node(mod, node, ISALIST);

  struct token tok = { 0 };
  e = scan_oneof(&tok, mod, TEOL, TSOB, 0);
  EXCEPT(e);

  if (tok.t == TEOL) {
    back(mod, &tok);
    goto done;
  }

  e = p_deftype_block(node, mod);
  EXCEPT(e);

done:
  return 0;
}

static error p_implicit_genargs(struct node *genargs, struct module *mod) {
  error e = p_genargs(genargs, mod, TRPAR, FALSE);
  EXCEPT(e);

  return 0;
}

static error p_defintf_statement(struct node *node, struct module *mod,
                                 struct node *intf) {
  error e;
  struct token tok = { 0 }, tok2 = { 0 };
  struct toplevel toplevel = { 0 };
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
    if (node_subs_first(node) == NULL) {
      (void)node_new_subnode(mod, node);
      (void)mk_node(mod, node, GENARGS);
    }
    e = p_deffun(node, mod, &toplevel, DEFFUN);
    break;
  case Tmethod:
    if (node_subs_first(node) == NULL) {
      (void)node_new_subnode(mod, node);
      (void)mk_node(mod, node, GENARGS);
    }
    e = p_deffun(node, mod, &toplevel, DEFMETHOD);
    break;
  case Tisa:
    node_subs_remove(intf, node);
    e = p_isalist(intf, mod, node_is_export(intf));
    EXCEPT(e);
    break;
  case Tlet:
  case Talias:
    e = p_let(node, mod, &toplevel, tok.t);
    break;
  case Tinvariant:
    e = p_invariant(node, mod);
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
  struct token tok = { 0 };
  bool first = TRUE;

  e = scan(&tok, mod);
  EXCEPT(e);

again:
  if (tok.t == TEOB) {
    if (first) {
      THROW_SYNTAX(mod, &tok, "block cannot be empty (use 'noop' instead)");;
    } else {
      return 0;
    }
  } else {
    back(mod, &tok);
    e = p_defintf_statement(node_new_subnode(mod, node), mod, node);
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
    toplevel->generic->first_explicit_genarg = node_subs_count(some_genargs);
  }

  node_set_which(node, DEFINTF);
  node->as.DEFINTF.toplevel = *toplevel;

  struct token tok = { 0 };
  error e = p_ident(node_subs_first(node), mod);
  EXCEPT(e);

  if (idents_value(mod->gctx, node_ident(node_subs_first(node)))[0] != '`') {
    e = mk_except(mod, node_subs_first(node), "intf name doesn't start with '`'");
    THROW(e);
  }

  e = p_genargs(node_subs_at(node, IDX_GENARGS), mod, TASSIGN, TRUE);
  EXCEPT(e);

  (void)mk_node(mod, node, ISALIST);

  e = scan_oneof(&tok, mod, TEOL, TSOB, 0);
  EXCEPT(e);

  if (tok.t == TEOL) {
    back(mod, &tok);
    goto done;
  }

  e = p_defintf_block(node, mod);
  EXCEPT(e);

done:
  return 0;
}

struct node *defincomplete_create(struct module *mod, const struct node *for_error) {
  // FIXME: Detached node, would have to be freed when releasing the
  // mod fun_state in which it is recorded below.
  //
  struct node *dinc = mempool_calloc(mod, 1, sizeof(struct node));
  dinc->codeloc = for_error->codeloc;
  node_set_which(dinc, DEFINCOMPLETE);
  struct node *dinc_name = mk_node(mod, dinc, IDENT);
  dinc_name->as.IDENT.name = gensym(mod);
  (void)mk_node(mod, dinc, GENARGS);
  (void)mk_node(mod, dinc, ISALIST);
  return dinc;
}

void defincomplete_set_ident(struct module *mod, const struct node *for_error,
                             struct node *dinc, ident name) {
  assert(dinc->which == DEFINCOMPLETE);
  dinc->as.DEFINCOMPLETE.ident = name;
  dinc->as.DEFINCOMPLETE.ident_for_error = for_error;
}

void defincomplete_add_field(struct module *mod, const struct node *for_error,
                             struct node *dinc, ident field, struct typ *t) {
  assert(dinc->which == DEFINCOMPLETE);
  struct node *f = mk_node(mod, dinc, DEFFIELD);
  f->codeloc = for_error->codeloc;
  struct node *n = mk_node(mod, f, IDENT);
  n->as.IDENT.name = field;
  struct node *d = mk_node(mod, f, DIRECTDEF);
  set_typ(&d->as.DIRECTDEF.typ, t);
  d->as.DIRECTDEF.flags = NODE_IS_TYPE;
}

void defincomplete_add_isa(struct module *mod, const struct node *for_error,
                           struct node *dinc, struct typ *tisa) {
  struct node *isalist = node_subs_at(dinc, IDX_ISALIST);
  struct node *isa = mk_node(mod, isalist, ISA);
  isa->codeloc = for_error->codeloc;
  struct node *dd = mk_node(mod, isa, DIRECTDEF);
  set_typ(&dd->as.DIRECTDEF.typ, tisa);
}

error defincomplete_catchup(struct module *mod, struct node *dinc) {
  assert(dinc->which == DEFINCOMPLETE);
  error e = catchup_instantiation(mod, mod, dinc, &mod->body->scope,
                                  TRUE);
  EXCEPT(e);
  return 0;
}

int snprint_defincomplete(char *s, size_t len,
                          const struct module *mod, const struct node *dinc) {
  assert(dinc->which == DEFINCOMPLETE);
  size_t pos = 0;

  pos += snprint_codeloc(s+pos, len-pos, mod, dinc);
  pos += snprintf(s+pos, len-pos, "\n");

  const ident name = dinc->as.DEFINCOMPLETE.ident;
  if (name != ID__NONE) {
    pos += snprintf(s+pos, len-pos, "  ");
    pos += snprint_codeloc(s+pos, len-pos, mod, dinc->as.DEFINCOMPLETE.ident_for_error);
    pos += snprintf(s+pos, len-pos,
                    "for ident '%s'\n",
                    idents_value(mod->gctx, dinc->as.DEFINCOMPLETE.ident));
  }

  const struct node *isalist = node_subs_at_const(dinc, IDX_ISALIST);
  if (node_subs_count_atleast(isalist, 1)) {
    FOREACH_SUB_CONST(isa, isalist) {
      pos += snprintf(s+pos, len-pos, "  ");
      pos += snprint_codeloc(s+pos, len-pos, mod, isa);
      pos += snprintf(s+pos, len-pos,
                      "with constraint '%s'\n",
                      typ_pretty_name(mod, isa->typ));
    }
  }

  FOREACH_SUB_CONST(f, dinc) {
    if (f->which == DEFFIELD) {
      pos += snprintf(s+pos, len-pos, "  ");
      pos += snprint_codeloc(s+pos, len-pos, mod, f);
      pos += snprintf(s+pos, len-pos,
                      "with field '%s', constrained by '%s'\n",
                      idents_value(mod->gctx, node_ident(f)),
                      typ_pretty_name(mod, f->typ));
    }
  }

  return pos;
}

void node_deepcopy(struct module *mod, struct node *dst,
                   const struct node *src) {
  INVARIANT_NODE(src);

  struct node *prev = dst->prev;
  struct node *next = dst->next;

  node_set_which(dst, src->which);
  memcpy(dst, src, sizeof(*dst));
  memset(&dst->scope, 0, sizeof(dst->scope));
  dst->prev = prev;
  dst->next = next;
  dst->subs_first = NULL;
  dst->subs_last = NULL;
  dst->typ = NULL;

  struct toplevel *dtoplevel = node_toplevel(dst);
  if (dtoplevel != NULL) {
    dtoplevel->generic = NULL;
    dtoplevel->topdeps = NULL;
  }

  FOREACH_SUB_CONST(s, src) {
    struct node *cpy = node_new_subnode(mod, dst);
    node_deepcopy(mod, cpy, s);
  }

  INVARIANT_NODE(dst);
}

void copy_and_extend_import_path(struct module *mod, struct node *imported,
                                 const struct node *import, const struct token *tok) {
  struct node *n = node_new_subnode(mod, imported);
  node_set_which(n, BIN);
  n->as.BIN.operator = TDOT;

  const struct node *path = node_subs_first_const(import);
  node_deepcopy(mod, node_new_subnode(mod, n), path);

  struct node *i = node_new_subnode(mod, n);
  node_set_which(i, IDENT);
  i->as.IDENT.name = idents_add(mod->gctx, tok);
}

static error p_import(struct node *node, struct module *mod,
                      const struct toplevel *toplevel,
                      bool from) {
  node_set_which(node, IMPORT);
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
    node->as.IMPORT.toplevel.flags |= TOP_IS_INLINE;
    goto again;
  } else if (tok.t == Timport || tok.t == Texport) {
    if (!from || import_export_count > 0 || ident_count > 0) {
      UNEXPECTED(mod, &tok);
    }
    import_export_count += 1;
    if (tok.t == Texport) {
      node->as.IMPORT.toplevel.flags |= TOP_IS_EXPORT;
    }
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
    node_set_which(imported, IMPORT);
    *node_toplevel(imported) = *toplevel;

    copy_and_extend_import_path(mod, imported, node, &tok);

    goto again;
  } else {
    back(mod, &tok);
    return 0;
  }
}

static error p_toplevel(struct module *mod) {
  if (mod->parser.len == 0) {
    return 0;
  }

  struct toplevel toplevel = { 0 };
  struct node *genargs = NULL;

  bool is_scoped = FALSE;
  error e;
  struct token tok = { 0 }, tok2 = { 0 };
  struct node *node = NULL;

  bool first = TRUE;
  goto start;

again:
  first = FALSE;

start:
  e = scan(&tok, mod);
  EXCEPT(e);

bypass:
  if (is_scoped && tok.t != Tmethod && tok.t != Tfun && tok.t != TLPAR) {
    UNEXPECTED(mod, &tok);
  }

#define NEW(mod, node) ( (node != NULL) ? node : node_new_subnode(mod, mod->body) )

  switch (tok.t) {
  case TLPAR:
    e = scan_oneof(&tok2, mod, Tstruct, Tenum, Tunion, Tintf, Tfun, Tmethod, 0);
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
  case Tstruct:
  case Tenum:
  case Tunion:
    node = NEW(mod, node);
    if (!node_subs_count_atleast(node, 1)) {
      (void)node_new_subnode(mod, node);
      (void)mk_node(mod, node, GENARGS);
    }
    e = p_deftype(node, mod, genargs, &toplevel, tok.t);
    break;
  case Tintf:
    node = NEW(mod, node);
    if (!node_subs_count_atleast(node, 1)) {
      (void)node_new_subnode(mod, node);
      (void)mk_node(mod, node, GENARGS);
    }
    e = p_defintf(node, mod, genargs, &toplevel);
    break;
  case Texport:
    toplevel.flags |= TOP_IS_EXPORT;
    goto again;
  case Textern:
    toplevel.flags |= TOP_IS_EXTERN;
    goto again;
  case Topaque:
    toplevel.flags |= TOP_IS_OPAQUE;
    goto again;
  case Tinline:
    toplevel.flags |= TOP_IS_INLINE;
    goto again;
  case Tfun:
    node = NEW(mod, node);
    if (!node_subs_count_atleast(node, 1)) {
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
    if (!node_subs_count_atleast(node, 1)) {
      (void)node_new_subnode(mod, node);
      (void)mk_node(mod, node, GENARGS);
    }
    e = p_deffun(node, mod, &toplevel, DEFMETHOD);
    break;
  case Tlet:
  case Talias:
  case Tglobalenv:
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
  case Twithin:
    e = p_within(NEW(mod, node), mod, TRUE);
    break;
  case TEOL:
    if (first) {
      // This happens when a file starts with comments.
      back(mod, &tok);
    }
    break;
  default:
    THROW_SYNTAX(mod, &tok, "malformed top-level statement at '%.*s'", (int)tok.len, tok.value);
    break;
  }

#undef NEW

  EXCEPT(e);
  return 0;
}

static void rec_subnode_counts(struct node *node,
                               size_t *node_count, size_t *sub_count,
                               size_t *hist) {
  size_t n;
  FOREACH_SUB(s, node) {
    *sub_count += 1;
    n += 1;
    rec_subnode_counts(s, node_count, sub_count, hist);
  }

  hist[min(size_t, 10, n)] += 1;
  *node_count += !!n;
}

unused__
static void subnode_count_avg(struct module *mod) {
  size_t sub_count = 0, node_count = 0;
  size_t hist[10] = { 0 };
  rec_subnode_counts(mod->root, &node_count, &sub_count, hist);
  for (size_t n = 0; n < ARRAY_SIZE(hist); ++n) {
    fprintf(stderr, "%zu:%zu\n", n, hist[n]);
  }

  fprintf(stderr, "%s: %f\n", mod->filename, (float) sub_count / node_count);
}

static error module_parse(struct module *mod) {
  error e;
  mod->body = mk_node(mod, mod->root, MODULE_BODY);
  scope_init(&mod->body->as.MODULE_BODY.globalenv_scope);

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
  PUSH_STATE(mod->mempool);
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
  node_set_which(m, MODULE);
  m->as.MODULE.name = basename;
  m->as.MODULE.is_placeholder = is_placeholder;
  m->as.MODULE.mod = is_placeholder ? NULL : non_placeholder_mod;
  scope_init(&m->scope);
  m->scope.parent = &parent->scope;
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
    error e = scope_lookup_ident_wontimport(&m, parent, some_module, &parent->scope,
                                            i, TRUE);
    if (e == EINVAL) {
      m = create_module_node(parent, i, p != last, to_register);

      e = scope_define_ident(some_module, &parent->scope, i, m);
      EXCEPT(e);

    } else if (e) {
      // Repeat bound-to-fail lookup to get the error message right.
      e = scope_lookup_ident_wontimport(&m, parent, some_module, &parent->scope,
                                        i, FALSE);
      THROW(e);
    } else {
      assert(m->which == MODULE);
      if (p == last) {
        if (!m->as.MODULE.is_placeholder) {
          THROWF(EINVAL, "Cannot load_module module '%s' more than once",
                 filename);
        } else {
          assert(to_register->root == NULL);
          to_register->root = create_module_node(parent, i, FALSE, to_register);

          FOREACH_SUB(to_save, m) {
            assert(to_save->which == MODULE);
            e = scope_define_ident(some_module, &to_register->root->scope,
                                   node_ident(to_save), to_save);
            EXCEPT(e);
          }

          // scope_define_ident() knows to accept to overwrite because it
          // was a placeholder.
          e = scope_define_ident(some_module, &parent->scope, i, to_register->root);
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
    return scope_name(mod, &typ_definition_const(t)->scope);
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

static int print_topdeps_foreach(const struct typ **t, uint32_t *value, void *user) {
  const struct module *mod = user;
  fprintf(stderr, "\t%04x %s\n", *value, typ_pretty_name(mod, *t));
  return 0;
}

void debug_print_topdeps(const struct module *mod, const struct node *node) {
  const struct toplevel *toplevel = node_toplevel_const(node);
  struct typset *topdeps = toplevel->topdeps;
  if (topdeps == NULL) {
    return;
  }

  fprintf(stderr, "%s\n", scope_name(mod, &node->scope));
  typset_foreach(topdeps, print_topdeps_foreach, (void *)mod);
}

int snprint_codeloc(char *s, size_t len,
                    const struct module *mod, const struct node *node) {
  const struct module *actual_mod = try_node_module_owner_const(mod, node);

  struct token tok = { 0 };
  tok.value = mod->parser.data + node->codeloc;

  return snprintf(s, len, "%s:%d:%d: ",
                  actual_mod->filename, parser_line(&actual_mod->parser, &tok),
                  parser_column(&actual_mod->parser, &tok));
}

error mk_except(const struct module *mod, const struct node *node,
                const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  char s[2048] = { 0 };
  size_t pos = 0, len = ARRAY_SIZE(s);

  pos += snprint_codeloc(s+pos, len-pos, mod, node);
  pos += vsnprintf(s+pos, len-pos, fmt, ap);

  error e = 0;
  GOTO_THROWF(EINVAL, "%s", s);

except:
  va_end(ap);
  return e;
}

error mk_except_type(const struct module *mod, const struct node *node,
                     const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  char s[2048] = { 0 };
  size_t pos = 0, len = ARRAY_SIZE(s);

  pos += snprint_codeloc(s+pos, len-pos, mod, node);
  pos += snprintf(s+pos, len-pos, "type: ");
  pos += vsnprintf(s+pos, len-pos, fmt, ap);

  error e = 0;
  GOTO_THROWF(EINVAL, "%s", s);

except:
  va_end(ap);
  return e;
}

error mk_except_call_args_count(const struct module *mod, const struct node *node,
                                const struct node *definition, bool implicit_self,
                                size_t given) {
  const size_t minus = implicit_self ? 1 : 0;
  error e = mk_except_type(mod, node,
                           "invalid number of arguments:"
                           " between %zu and %zu expected, but %zu given",
                           node_fun_min_args_count(definition) - minus,
                           node_fun_max_args_count(definition) - minus,
                           given);
  THROW(e);
}
