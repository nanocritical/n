#ifndef PARSER_H__
#define PARSER_H__

#include "common.h"
#include "lexer.h"
#include "scope.h"

#define MODULE_PATH_MAXLEN 16

struct typ;

#define SF(which) ( (uint64_t)1 << (which) )
#define STEP_FILTER(step, m) const uint64_t step##_filter = (m)

#define node_whichmask(node) SF(node->which)

enum node_which {
  NUL = 1,
  IDENT,
  NUMBER,
  BOOL,
  STRING,
  SIZEOF,
  BIN,
  UN,
  TUPLE,
  TUPLEEXTRACT,
  TUPLENTH,
  CALL,
  INIT,
  RETURN,
  BLOCK,
  FUTURE,
  LAMBDA,
  FOR,
  WHILE,
  BREAK,
  CONTINUE,
  NOOP,
  IF,
  MATCH,
  TRY,
  CATCH,
  EXCEP,
  THROW,
  TYPECONSTRAINT,
  DYN,
  DEFFUN,
  DEFTYPE,
  DEFNAMEDLITERAL,
  DEFCONSTRAINTLITERAL,
  DEFUNKNOWNIDENT,
  DEFMETHOD,
  DEFINTF,
  DEFNAME,
  DEFPATTERN,
  FUNARGS,
  DEFARG,
  GENARGS,
  DEFGENARG,
  SETGENARG,
  LET,
  DEFFIELD,
  DEFCHOICE,
  DELEGATE,
  PRE,
  POST,
  INVARIANT,
  EXAMPLE,
  ISALIST,
  ISA,
  IMPORT,
  MODULE,
  MODULE_BODY,
  ROOT_OF_ALL,
  DIRECTDEF,
  NODE__NUM,
};

const char *node_which_strings[NODE__NUM];

enum builtingen {
  BG__NOT,
  BG_DEFAULT_CTOR_CTOR,
  BG_DEFAULT_CTOR_MK,
  BG_DEFAULT_CTOR_NEW,
  BG_TRIVIAL_CTOR_CTOR,
  BG_TRIVIAL_CTOR_MK,
  BG_TRIVIAL_CTOR_NEW,
  BG_AUTO_MK,
  BG_AUTO_NEW,
  BG_CTOR_WITH_MK,
  BG_CTOR_WITH_NEW,
  BG_AUTO_MKV,
  BG_AUTO_NEWV,
  BG_UNION_CTOR_WITH_CTOR,
  BG_UNION_CTOR_WITH_MK,
  BG_UNION_CTOR_WITH_NEW,
  BG_ENUM_EQ,
  BG_ENUM_NE,
  BG_ENUM_MATCH,
  BG_UNION_MATCH,
  BG_UNION_DISPATCH,
  BG_UNION_COPY,
  BG_UNION_EQUALITY_EQ,
  BG_UNION_EQUALITY_NE,
  BG_UNION_ORDER_LE,
  BG_UNION_ORDER_LT,
  BG_UNION_ORDER_GT,
  BG_UNION_ORDER_GE,
  BG_TRIVIAL_COPY_COPY_CTOR,
  BG_TRIVIAL_EQUALITY_OPERATOR_EQ,
  BG_TRIVIAL_EQUALITY_OPERATOR_NE,
  BG__NUM,
};

const char *builtingen_abspath[BG__NUM];

struct toplevel {
  bool is_export;
  bool is_extern;
  bool is_inline;
  ident scope_name;
  bool is_prototype;
  bool is_shadowed;
  bool is_not_dyn;
  enum builtingen builtingen;

  size_t first_explicit_genarg;
  struct node **instances;
  size_t instances_count;
  struct typ *our_generic_functor_typ;

  ssize_t yet_to_pass;
};

struct node_nul {};
struct node_ident {
  ident name;

  struct scope *non_local_scope;
};
struct node_number {
  const char *value;
};
struct node_bool {
  bool value;
};
struct node_string {
  const char *value;
};
struct node_bin {
  enum token_type operator;
};
struct node_un {
  enum token_type operator;
};
struct node_tuple {};
struct node_tuplenth {
  size_t nth;
};
struct node_call {
  const struct node *return_through_ref_expr;
};
struct node_future {};
struct node_lambda {};
struct node_init {
  const struct node *target_expr;
  bool is_array;
};
struct node_return {
  const struct node *return_through_ref_expr;
  bool forced_return_through_ref;
};
struct node_for {
  struct node *pattern;
  struct node *block;
};
struct node_while {};
struct node_break {};
struct node_continue {};
struct node_pass {};
struct node_if {};
struct node_match {};
struct node_try {
  ident error;
};
struct node_catch {
  bool is_user_label;
  ident label;
};
struct node_throw {
  ident label;
  ident error;
};
struct node_typeconstraint {
  bool in_pattern;
};
struct node_dyn {
  struct typ *intf_typ;
};
struct node_deffun {
  struct toplevel toplevel;
};

enum deftype_kind {
  DEFTYPE_PROTOTYPE = 0,
  DEFTYPE_STRUCT,
  DEFTYPE_ENUM,
  DEFTYPE_UNION,
};

struct node_deftype {
  struct toplevel toplevel;

  enum deftype_kind kind;
  struct typ *choice_typ;
};
struct node_defmethod {
  struct toplevel toplevel;
  enum token_type access;
};
struct node_defintf {
  struct toplevel toplevel;
};
struct node_defnamedliteral {
  struct toplevel toplevel;
};
struct node_defconstraintliteral {
  struct toplevel toplevel;
};
struct node_defunknownident {
  struct toplevel toplevel;
};
struct node_defname {
  struct node *pattern;
  struct node *expr;

  bool is_excep;
  struct node *excep_label_ident;
  ident excep_label;
  ident excep_error;
};
struct node_defpattern {
  bool is_alias;
};
struct node_defarg {
  bool is_optional;
  bool is_retval;
};
struct node_defgenarg {
  bool is_explicit;
};
struct node_setgenarg {
};
struct node_let {
  struct toplevel toplevel;
};
struct node_deffield {};
struct node_defchoice {
  bool has_value;
};
struct node_isa {
  bool is_export;
  bool is_explicit;
};
struct node_delegate {
  struct toplevel toplevel;
};
struct node_pre {};
struct node_post {};
struct node_invariant {
  struct toplevel toplevel;
};
struct node_example {
  struct toplevel toplevel;
  size_t name;
};
struct node_import {
  struct toplevel toplevel;
  bool is_all;
  bool intermediate_mark;
};

struct module;
struct node_module {
  ident name;
  bool is_placeholder;

  struct module *mod;
};
struct node_module_body {
};

struct node_directdef {
  struct typ *typ;
  uint32_t flags;
};

union node_as {
  struct node_nul NUL;
  struct node_ident IDENT;
  struct node_number NUMBER;
  struct node_bool BOOL;
  struct node_string STRING;
  struct node_bin BIN;
  struct node_un UN;
  struct node_tuple TUPLE;
  struct node_tuplenth TUPLENTH;
  struct node_call CALL;
  struct node_future FUTURE;
  struct node_init INIT;
  struct node_return RETURN;
  struct node_for FOR;
  struct node_break BREAK;
  struct node_continue CONTINUE;
  struct node_if IF;
  struct node_match MATCH;
  struct node_try TRY;
  struct node_catch CATCH;
  struct node_throw THROW;
  struct node_typeconstraint TYPECONSTRAINT;
  struct node_dyn DYN;
  struct node_deffun DEFFUN;
  struct node_deftype DEFTYPE;
  struct node_defnamedliteral DEFNAMEDLITERAL;
  struct node_defconstraintliteral DEFCONSTRAINTLITERAL;
  struct node_defunknownident DEFUNKNOWNIDENT;
  struct node_defmethod DEFMETHOD;
  struct node_defintf DEFINTF;
  struct node_defname DEFNAME;
  struct node_defpattern DEFPATTERN;
  struct node_defarg DEFARG;
  struct node_defgenarg DEFGENARG;
  struct node_setgenarg SETGENARG;
  struct node_let LET;
  struct node_deffield DEFFIELD;
  struct node_defchoice DEFCHOICE;
  struct node_delegate DELEGATE;
  struct node_pre PRE;
  struct node_post POST;
  struct node_invariant INVARIANT;
  struct node_example EXAMPLE;
  struct node_isa ISA;
  struct node_import IMPORT;
  struct node_module MODULE;
  struct node_module_body MODULE_BODY;
  struct node_directdef DIRECTDEF;
};

struct scope;

enum node_flags {
  NODE_IS_TYPE = 0x1,
  NODE_IS_DEFCHOICE = 0x2,
  NODE_IS_TEMPORARY = 0x4,
  NODE_IS_GLOBAL_LET = 0x8,
  NODE__TRANSITIVE = NODE_IS_TYPE,
  NODE__EXCEPTED = 0x8000,
};

struct node {
  enum node_which which;
  uint32_t flags;

  struct node *next;
  struct node *prev;
  struct node *subs_first;
  struct node *subs_last;

  struct typ *typ;
  struct scope scope;

  union node_as as;
  size_t codeloc;
};

extern void unset_typ(struct typ **loc);

// This is a tricky operation when 'node->which' is already set: we may
// be destroying all sorts of state. We try to do it cleanly.
static inline void node_set_which(struct node *node, enum node_which which) {
  if (node->which == 0) {
    node->which = which;
    return;
  }

  switch (node->which) {
  case DIRECTDEF:
    unset_typ(&node->as.DIRECTDEF.typ);
    break;
  case DYN:
    unset_typ(&node->as.DYN.intf_typ);
    break;
  case DEFTYPE:
  case DEFINTF:
  case DEFFUN:
  case DEFMETHOD:
  case DEFNAMEDLITERAL:
  case DEFCONSTRAINTLITERAL:
  case DEFUNKNOWNIDENT:
    assert(FALSE && "Don't do that");
    break;
  default:
    break;
  }

  unset_typ(&node->typ);

  assert(scope_count(&node->scope) == 0 && "Not handled");

  memset(&node->as, 0, sizeof(node->as));
  node->which = which;
}

// Does not move src->typ, clears it in both 'dst' and 'src'.
void node_move_content(struct node *dst, struct node *src);

void node_invariant(const struct node *node);

#define INVARIANT_NODE(node) INVARIANT(node_invariant(node))

#define FOREACH_SUB(s, n) \
  for (struct node *__p_##s = (n), *s = __p_##s->subs_first; \
       s != NULL; s = s->next)

#define REVERSE_FOREACH_SUB(s, n) \
  for (struct node *__p_##s = (n), *s = __p_##s->subs_last; \
       s != NULL; s = s->prev)

#define NODE_NEXTTH(n, repeat) ({ \
  size_t count = repeat; \
  __typeof__(n) r = n; \
  while (count > 0) { \
    count -= 1; \
    r = r->next; \
    if (r == NULL) { \
      break; \
    } \
  } \
  r; })

#define FOREACH_SUB_EVERY(s, n, from, every) \
  for (struct node *__p_##s = (n), *s = try_node_subs_at(__p_##s, from); \
       s != NULL; s = NODE_NEXTTH(s, every))

#define FOREACH_SUB_CONST(s, n) \
  for (const struct node *__p_##s = (n), *s = __p_##s->subs_first; \
       s != NULL; s = s->next)

#define FOREACH_SUB_EVERY_CONST(s, n, from, every) \
  for (const struct node *__p_##s = (n), *s = try_node_subs_at_const(__p_##s, from); \
       s != NULL; s = NODE_NEXTTH(s, every))

static inline size_t node_subs_count(const struct node *node) {
  size_t n = 0;
  FOREACH_SUB_CONST(s, node) {
    n += 1;
  }
  return n;
}

// Prefer whenever possible.
static inline bool node_subs_count_atleast(const struct node *node, size_t min) {
  if (min == 0) {
    return TRUE;
  }

  size_t n = 0;
  FOREACH_SUB_CONST(s, node) {
    n += 1;
    if (n >= min) {
      return TRUE;
    }
  }
  return FALSE;
}

static inline const struct node *node_subs_first_const(const struct node *node) {
  return node->subs_first;
}

static inline const struct node *node_subs_last_const(const struct node *node) {
  return node->subs_last;
}

static inline const struct node *node_next_const(const struct node *node) {
  return node->next;
}

static inline const struct node *node_prev_const(const struct node *node) {
  return node->prev;
}

static inline const struct node *node_subs_at_const(const struct node *node, size_t n) {
  const struct node *r = node->subs_first;
  size_t i;
  for (i = 0; r != NULL && i < n; ++i) {
    r = r->next;
  }
  assert(r != NULL && i == n && "Index out of bound");
  return r;
}

static inline const struct node *try_node_subs_at_const(const struct node *node, size_t n) {
  const struct node *r = node->subs_first;
  size_t i;
  for (i = 0; r != NULL && i < n; ++i) {
    r = r->next;
  }
  return r;
}

static inline struct node *node_subs_first(struct node *node) {
  return (struct node *)node_subs_first_const(node);
}

static inline struct node *node_subs_last(struct node *node) {
  return (struct node *)node_subs_last_const(node);
}

static inline struct node *node_next(struct node *node) {
  return (struct node *)node_next_const(node);
}

static inline struct node *node_prev(struct node *node) {
  return (struct node *)node_prev_const(node);
}

static inline struct node *node_subs_at(struct node *node, size_t n) {
  return (struct node *)node_subs_at_const(node, n);
}

static inline struct node *try_node_subs_at(struct node *node, size_t n) {
  return (struct node *)try_node_subs_at_const(node, n);
}

static inline void node_subs_remove(struct node *node, struct node *sub) {
  struct node *prev = sub->prev;
  struct node *next = sub->next;
  if (prev == NULL) {
    assert(node->subs_first == sub);
    node->subs_first = next;
  } else {
    prev->next = next;
  }
  if (next == NULL) {
    assert(node->subs_last == sub);
    node->subs_last = prev;
  } else {
    next->prev = prev;
  }

  sub->prev = NULL;
  sub->next = NULL;
}

static inline void node_subs_append(struct node *node, struct node *sub) {
  assert(sub->prev == NULL && sub->next == NULL);
  struct node *last = node->subs_last;
  if (last == NULL) {
    node->subs_first = sub;
    node->subs_last = sub;
    return;
  }
  last->next = sub;
  sub->prev = last;
  sub->next = NULL;
  node->subs_last = sub;
}

static inline void node_subs_prepend(struct node *node, struct node *sub) {
  assert(sub->prev == NULL && sub->next == NULL);
  struct node *first = node->subs_first;
  if (first == NULL) {
    node->subs_first = sub;
    node->subs_last = sub;
    return;
  }
  sub->next = first;
  sub->prev = NULL;
  first->prev = sub;
  node->subs_first = sub;
}

static inline void node_subs_insert_before(struct node *node, struct node *where,
                                           struct node *sub) {
  assert(sub->prev == NULL && sub->next == NULL);
  if (where == NULL) {
    assert(node->subs_first == NULL && node->subs_last == NULL);
    node->subs_first = sub;
    node->subs_last = sub;
    return;
  }

  struct node *prev = where->prev;
  if (prev == sub) {
    return;
  }
  sub->prev = prev;
  if (prev == NULL) {
    node->subs_first = sub;
  } else {
    prev->next = sub;
  }

  sub->next = where;
  where->prev = sub;
}

static inline void node_subs_insert_after(struct node *node, struct node *where,
                                          struct node *sub) {
  assert(sub->prev == NULL && sub->next == NULL);
  if (where == NULL) {
    assert(node->subs_first == NULL && node->subs_last == NULL);
    node->subs_first = sub;
    node->subs_last = sub;
    return;
  }

  struct node *next = where->next;
  if (next == sub) {
    return;
  }
  sub->next = next;
  if (next == NULL) {
    node->subs_last = sub;
  } else {
    next->prev = sub;
  }

  sub->prev = where;
  where->next = sub;
}

static inline void node_subs_replace(struct node *node, struct node *where,
                                     struct node *sub) {
  assert(sub->prev == NULL && sub->next == NULL);
  struct node *prev = where->prev;
  sub->prev = prev;
  if (prev == NULL) {
    assert(node->subs_first == where);
    node->subs_first = sub;
  } else {
    prev->next = sub;
  }

  struct node *next = where->next;
  sub->next = next;
  if (next == NULL) {
    assert(node->subs_last == where);
    node->subs_last = sub;
  } else {
    next->prev = sub;
  }

  where->prev = NULL;
  where->next = NULL;
}

enum subnode_idx {
  IDX_GENARGS = 1,
  IDX_ISALIST = 2,
  IDX_CH_VALUE = 1,
  IDX_CH_PAYLOAD = 2,
  IDX_FOR_IT = 0,
  IDX_FOR_IT_DEFP = 0,
  IDX_FOR_IT_DEFP_DEFN = 0,
  IDX_FOR_IT_DEFP_EXPR = 1,
  IDX_FOR_IT_BLOCK = 1,
  IDX_FOR_IT_BLOCK_WHILE = 0,
  IDX_FOR_IT_BLOCK_WHILE_BLOCK = 1,
  IDX_FUNARGS = 2,
  IDX_DEFNAME_EXCEP_TEST = 0,
  IDX_UNKNOWN_IDENT = 2,
};

struct idents_map;

struct idents {
  const char **values;
  size_t capacity;
  size_t count;

  struct idents_map *map;
};

enum predefined_idents {
  ID__NONE = 0,
  ID_ANONYMOUS,
  ID_ROOT_OF_ALL,
  ID_FOR,
  ID_WHILE,
  ID_MATCH,
  ID_TRY,
  ID_LET,
  ID_PRE,
  ID_POST,
  ID_INVARIANT,
  ID_EXAMPLE,
  ID_THIS,
  ID_FINAL,
  ID_SELF,
  ID_OTHERWISE,
  ID_THROW,

  ID_MAIN,
  ID_WHICH,
  ID_AS,
  ID_WHICH_TYPE,
  ID_AS_TYPE,
  ID_NEXT,
  ID_GET,
  ID_IS_VALID,
  ID_CAST,
  ID_WILDCARD_REF_ARG,
  ID_LIKELY,
  ID_UNLIKELY,
  ID_NLANG,

  ID_TBI_VOID,
  ID_TBI__FIRST = ID_TBI_VOID,
  ID_TBI_LITERALS_NULL,
  ID_TBI_LITERALS_INTEGER,
  ID_TBI_LITERALS_FLOATING,
  ID_TBI_ANY_TUPLE,
  ID_TBI_TUPLE_2,
  ID_TBI_TUPLE_3,
  ID_TBI_TUPLE_4,
  ID_TBI_TUPLE_5,
  ID_TBI_TUPLE_6,
  ID_TBI_TUPLE_7,
  ID_TBI_TUPLE_8,
  ID_TBI_TUPLE_9,
  ID_TBI_TUPLE_10,
  ID_TBI_TUPLE_11,
  ID_TBI_TUPLE_12,
  ID_TBI_TUPLE_13,
  ID_TBI_TUPLE_14,
  ID_TBI_TUPLE_15,
  ID_TBI_TUPLE_16,
  ID_TBI_ANY,
  ID_TBI_BOOL,
  ID_TBI_BOOL_COMPATIBLE,
  ID_TBI_I8,
  ID_TBI_U8,
  ID_TBI_I16,
  ID_TBI_U16,
  ID_TBI_I32,
  ID_TBI_U32,
  ID_TBI_I64,
  ID_TBI_U64,
  ID_TBI_SIZE,
  ID_TBI_SSIZE,
  ID_TBI_FLOAT,
  ID_TBI_DOUBLE,
  ID_TBI_CHAR,
  ID_TBI_STRING,
  ID_TBI_STATIC_STRING,
  ID_TBI_STATIC_STRING_COMPATIBLE,
  ID_TBI_STATIC_ARRAY,
  ID_TBI__REF_COMPATIBLE,
  ID_TBI_ANY_ANY_REF,
  ID_TBI_ANY_REF,
  ID_TBI_ANY_MUTABLE_REF,
  ID_TBI_ANY_NULLABLE_REF,
  ID_TBI_ANY_NULLABLE_MUTABLE_REF,
  ID_TBI_REF,
  ID_TBI_MREF,
  ID_TBI_MMREF,
  ID_TBI_NREF,
  ID_TBI_NMREF,
  ID_TBI_NMMREF,
  ID_TBI_ARITHMETIC,
  ID_TBI_BITWISE,
  ID_TBI_INTEGER,
  ID_TBI_UNSIGNED_INTEGER,
  ID_TBI_NATIVE_INTEGER,
  ID_TBI_NATIVE_ANYSIGN_INTEGER,
  ID_TBI_GENERALIZED_BOOLEAN,
  ID_TBI_NATIVE_BOOLEAN,
  ID_TBI_FLOATING,
  ID_TBI_NATIVE_FLOATING,
  ID_TBI_HAS_EQUALITY,
  ID_TBI_ORDERED,
  ID_TBI_ORDERED_BY_COMPARE,
  ID_TBI_COPYABLE,
  ID_TBI_DEFAULT_CTOR,
  ID_TBI_CTOR_WITH,
  ID_TBI_ARRAY_CTOR,
  ID_TBI_TRIVIAL_COPY,
  ID_TBI_TRIVIAL_CTOR,
  ID_TBI_TRIVIAL_ARRAY_CTOR,
  ID_TBI_TRIVIAL_DTOR,
  ID_TBI_TRIVIAL_EQUALITY,
  ID_TBI_TRIVIAL_ORDER,
  ID_TBI_RETURN_BY_COPY,
  ID_TBI_UNION_COPY,
  ID_TBI_UNION_EQUALITY,
  ID_TBI_UNION_ORDER,
  ID_TBI_ITERATOR,
  ID_TBI__NOT_TYPEABLE,
  ID_TBI__FIRST_MARKER = ID_TBI__NOT_TYPEABLE,
  ID_TBI__CALL_FUNCTION_SLOT,
  ID_TBI__MUTABLE,
  ID_TBI__MERCURIAL,
  ID_TBI__LAST = ID_TBI__MERCURIAL,

  ID_MK,
  ID_NEW,
  ID_CTOR,
  ID_COPY_CTOR,
  ID_MKV,
  ID_NEWV,
  ID_C,
  ID_FROM_STATIC_STRING,
  ID_FROM_BOOL,
  ID_NRETVAL,
  ID_OPERATOR_OR,
  ID_OPERATOR_AND,
  ID_OPERATOR_NOT,
  ID_OPERATOR_TEST,
  ID_OPERATOR_LE,
  ID_OPERATOR_LT,
  ID_OPERATOR_GT,
  ID_OPERATOR_GE,
  ID_OPERATOR_EQ,
  ID_OPERATOR_NE,
  ID_OPERATOR_MATCH,
  ID_OPERATOR_BWOR,
  ID_OPERATOR_BWXOR,
  ID_OPERATOR_BWAND,
  ID_OPERATOR_LSHIFT,
  ID_OPERATOR_RSHIFT,
  ID_OPERATOR_ASSIGN_BWOR,
  ID_OPERATOR_ASSIGN_BWXOR,
  ID_OPERATOR_ASSIGN_BWAND,
  ID_OPERATOR_ASSIGN_LSHIFT,
  ID_OPERATOR_ASSIGN_RSHIFT,
  ID_OPERATOR_PLUS,
  ID_OPERATOR_MINUS,
  ID_OPERATOR_DIVIDE,
  ID_OPERATOR_MODULO,
  ID_OPERATOR_TIMES,
  ID_OPERATOR_ASSIGN_PLUS,
  ID_OPERATOR_ASSIGN_MINUS,
  ID_OPERATOR_ASSIGN_DIVIDE,
  ID_OPERATOR_ASSIGN_MODULO,
  ID_OPERATOR_ASSIGN_TIMES,
  ID_OPERATOR_UMINUS,
  ID_OPERATOR_BWNOT,

  ID__NUM,
};

struct fun_state {
  struct fun_state *prev;

  bool fun_uses_final;
  const struct node *retval;
  enum token_type ref_wildcard;
  enum token_type nulref_wildcard;
  enum token_type deref_wildcard;
  enum token_type wildcard;

  struct node **tentative_instantiations;
  size_t tentative_instantiations_count;
};

struct try_state {
  struct try_state *prev;

  struct node *tryy;
  struct node **excepts;
  size_t count;
};

struct step_state {
  struct step_state *prev;

  bool upward;
  size_t stepping;
};

struct stackel {
  struct node *node;
  struct node *sub;
  struct node *next_sub;
};

struct module_state {
  struct module_state *prev;

  bool tentatively;

  struct fun_state *fun_state;
  struct try_state *try_state;

  struct step_state *step_state;

#define PASS_STACK_DEPTH 512
  struct stackel stack[PASS_STACK_DEPTH];
  size_t stackp;
};

#define PUSH_STATE(st) do { \
  __typeof__(st) nst = calloc(1, sizeof(*st)); \
  nst->prev = st; \
  st = nst; \
} while (0)

#define POP_STATE(st) do { \
  assert(st != NULL); \
  __typeof__(st) ost = st; \
  st = ost->prev; \
  free(ost); \
} while (0)

struct globalctx {
  struct idents idents;
  // This node hierarchy is used to park loaded modules using their
  // absolute name. It is not used for lexical lookup.
  struct node modules_root;

  struct typ *builtin_typs_by_name[ID__NUM];
  struct typ *builtin_typs_for_refop[TOKEN__NUM];
};

struct stage_state {
  struct stage_state *prev;

  ssize_t passing;
};

// A compilation stage is a group of modules that are loaded and compiled
// together and can be mutually interdependent. Typically, it would be a
// whole program or a library. Compilation stages can be compiled separately
// and between compilation stages, however, (inline) dependencies must be
// cycle-free.
struct stage {
  const char **prefixes;

  struct node **loaded;
  size_t loaded_count;

  struct module **sorted;
  size_t sorted_count;

  struct module *entry_point;

  struct stage_state *state;
};

struct mempool {
  struct mempool *prev;

  uint8_t *free;
  uint8_t *end;
};

struct module {
  struct globalctx *gctx;
  struct stage *stage;

  const char *filename;

  ident path[MODULE_PATH_MAXLEN];
  size_t path_len;

  struct parser parser;
  struct node *root;
  struct node *body;

  // State
  size_t next_gensym;
  size_t next_example;
  size_t next_pre;
  size_t next_post;
  size_t next_invariant;

  bool done;

  struct module_state *state;

  struct mempool *mempool;
};

void *mempool_calloc(struct module *mod, size_t nmemb, size_t size);

int parser_line(const struct parser *parser, const struct token *tok);
int parser_column(const struct parser *parser, const struct token *tok);

void globalctx_init(struct globalctx *gctx);

error module_open(struct globalctx *gctx, struct stage *stage, struct module *mod,
                  const char *prefix, const char *fn);
void module_retval_set(struct module *mod, const struct node *retval);
const struct node *module_retval_get(struct module *mod);
void module_excepts_open_try(struct module *mod, struct node *tryy);
void module_excepts_push(struct module *mod, struct node *excep_node);
struct try_state *module_excepts_get(struct module *mod);
void module_excepts_close_try(struct module *mod);

struct typ *instances_register(struct module *mod, struct typ *t);
struct typ *instances_find_existing_for_tentative(struct module *mod,
                                                  struct typ *t);

ident gensym(struct module *mod);

const char *idents_value(const struct globalctx *gctx, ident id);
ident idents_add(struct globalctx *gctx, const struct token *tok);
ident idents_add_string(struct globalctx *gctx, const char *name, size_t len);

void copy_and_extend_import_path(struct module *mod, struct node *imported,
                                 const struct node *import, const struct token *tok);

const struct module *try_node_module_owner_const(const struct module *mod,
                                                 const struct node *node);
struct module *node_module_owner(struct node *node);
const struct module *node_module_owner_const(const struct node *node);
struct node *node_toplevel_owner(struct node *node);
struct node *node_statement_owner(struct node *node);

static inline ident node_ident(const struct node *node) {
  switch (node->which) {
  case IDENT:
    return node->as.IDENT.name;
  case DEFNAME:
    return node_ident(node->as.DEFNAME.pattern);
  case DEFARG:
    return node_ident(node_subs_first_const(node));
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
  case DEFMETHOD:
    if (node_subs_first_const(node)->which == IDENT) {
      return node_subs_first_const(node)->as.IDENT.name;
    } else {
      return node_subs_at_const(node_subs_first_const(node), 1)->as.IDENT.name;
    }
    break;
  case DEFTYPE:
  case DEFFIELD:
  case DEFCHOICE:
  case DEFINTF:
  case DEFNAMEDLITERAL:
  case DEFCONSTRAINTLITERAL:
    assert(node_subs_first_const(node)->which == IDENT);
    return node_subs_first_const(node)->as.IDENT.name;
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

static inline struct node *node_parent(struct node *node) {
  if (node->scope.parent == NULL) {
    return NULL;
  } else {
    return scope_node(node->scope.parent);
  }
}

static inline const struct node *node_parent_const(const struct node *node) {
  return node_parent((struct node *) node);
}

bool node_is_prototype(const struct node *node);
bool node_is_inline(const struct node *node);
bool node_is_export(const struct node *node);
bool node_is_def(const struct node *node);
bool node_is_statement(const struct node *node);
bool node_is_rvalue(const struct node *node);
bool node_is_at_top(const struct node *node);

static inline bool node_can_have_genargs(const struct node *node) {
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
  case DEFTYPE:
  case DEFINTF:
    return TRUE;
  default:
    return FALSE;
  }
}

struct node *node_new_subnode(struct module *mod, struct node *node);
bool node_has_tail_block(const struct node *node);
bool node_is_fun(const struct node *node);
size_t node_fun_all_args_count(const struct node *def);
size_t node_fun_explicit_args_count(const struct node *def);
struct node *node_fun_retval(struct node *def);
const struct node *node_fun_retval_const(const struct node *def);
struct node *node_for_block(struct node *node);

#define STEP_FILTER_DEFS_NO_FUNS \
  (SF(DEFTYPE) | SF(DEFINTF) | SF(DEFNAMEDLITERAL) \
   | SF(DEFCONSTRAINTLITERAL) | SF(DEFUNKNOWNIDENT))

#define STEP_FILTER_DEFS \
   (STEP_FILTER_DEFS_NO_FUNS | SF(DEFFUN) | SF(DEFMETHOD))

#define STEP_FILTER_HAS_TOPLEVEL \
  (STEP_FILTER_DEFS | SF(LET) | SF(INVARIANT) | SF(DELEGATE) \
   | SF(IMPORT) | SF(EXAMPLE))

static inline const struct toplevel *node_toplevel_const(const struct node *node) {
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
  case DEFNAMEDLITERAL:
    toplevel = &node->as.DEFNAMEDLITERAL.toplevel;
    break;
  case DEFCONSTRAINTLITERAL:
    toplevel = &node->as.DEFCONSTRAINTLITERAL.toplevel;
    break;
  case DEFUNKNOWNIDENT:
    toplevel = &node->as.DEFUNKNOWNIDENT.toplevel;
    break;
  case LET:
    toplevel = &node->as.LET.toplevel;
    break;
  case INVARIANT:
    toplevel = &node->as.INVARIANT.toplevel;
    break;
  case DELEGATE:
    toplevel = &node->as.DELEGATE.toplevel;
    break;
  case EXAMPLE:
    toplevel = &node->as.EXAMPLE.toplevel;
    break;
  case IMPORT:
    toplevel = &node->as.IMPORT.toplevel;
    break;
  default:
    break;
  }

  return toplevel;
}

static inline struct toplevel *node_toplevel(struct node *node) {
  return (struct toplevel *) node_toplevel_const(node);
}

struct node *node_get_member(struct module *mod, struct node *node, ident id);
const struct node *node_get_member_const(const struct module *mod, const struct node *node, ident id);
struct node *mk_node(struct module *mod, struct node *parent, enum node_which kind);
void node_deepcopy(struct module *mod, struct node *dst,
                   const struct node *src);

#define G(var, parent, which, ...) \
  __attribute__((unused)) \
  struct node *var = mk_node(mod, parent, which); \
  __VA_ARGS__

#define G_IDENT(var, parent, _name, ...) \
  G(var, parent, IDENT, \
     { \
       const char *__name = (_name); \
       var->as.IDENT.name = idents_add_string(mod->gctx, __name, strlen(__name)); \
     } \
     __VA_ARGS__)

// Return value must be freed by caller.
char *typ_name(const struct module *mod, const struct typ *t);
// Return value must be freed by caller.
char *typ_pretty_name(const struct module *mod, const struct typ *t);

error mk_except(const struct module *mod, const struct node *node, const char *fmt, ...)
  __attribute__((__format__(__printf__, 3, 4)));
error mk_except_type(const struct module *mod, const struct node *node, const char *fmt, ...)
  __attribute__((__format__(__printf__, 3, 4)));
error mk_except_call_args_count(const struct module *mod, const struct node *node,
                                const struct node *definition, size_t extra, size_t given);

#define GOTO_EXCEPT_TYPE(mod, node, fmt, ...) do { \
  e = mk_except_type(mod, node, fmt, ##__VA_ARGS__); \
  GOTO_EXCEPT(e); \
} while (0)

#endif
