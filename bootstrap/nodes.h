#ifndef NODES_H__
#define NODES_H__

#include "common.h"
#include "scope.h"
#include "vector.h"
#include "table.h"
#include "lexer.h"

#define MODULE_PATH_MAXLEN 16

struct typ;
struct constraint;
struct constraint_resolve_state;

#define NM(which) ( (uint64_t)1LL << (which) )
#define STEP_NM(step, m) const uint64_t step##_filter = (m)

enum node_which {
  NUL = 1,
  IDENT,
  NUMBER,
  BOOL,
  STRING,
  SIZEOF,
  ALIGNOF,
  BIN,
  UN,
  TUPLE,
  CALL,
  CALLNAMEDARG,
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
  JUMP,
  PHI,
  TYPECONSTRAINT,
  DYN,
  DEFFUN,
  DEFTYPE,
  DEFINCOMPLETE,
  DEFMETHOD,
  DEFINTF,
  DEFALIAS,
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
  WITHIN,
  ISALIST,
  ISA,
  IMPORT,
  MODULE,
  MODULE_BODY,
  ROOT_OF_ALL,
  DIRECTDEF,
  NODE__NUM,
};

#define NMASK_HIR_ONLY \
  ( NM(FUTURE) | NM(FOR) | NM(BREAK) \
    | NM(EXCEP) | NM(THROW) \
    | NM(DEFPATTERN) )

#define NMASK_LIR_ONLY ( NM(JUMP) )

const char *node_which_strings[NODE__NUM];

enum builtingen {
  BG__NOT,
  BG_TRIVIAL_CTOR_CTOR,
  BG_TRIVIAL_DTOR_DTOR,
  BG_TRIVIAL_COPY_COPY_CTOR,
  BG_TRIVIAL_COMPARE_OPERATOR_COMPARE,
  BG_ENUM_FROM_TAG,
  BG_ENUM_TAG,
  BG_ENVIRONMENT_PARENT,
  BG_ENVIRONMENT_INSTALL,
  BG_ENVIRONMENT_UNINSTALL,
  BG__NUM,
};

enum toplevel_flags {
  TOP_IS_EXPORT = 0x1,
  TOP_IS_EXTERN = 0x2,
  TOP_IS_INLINE = 0x4,
  TOP_IS_OPAQUE = 0x8 | TOP_IS_INLINE,
  TOP_IS_PROTOTYPE = 0x10,
  TOP_IS_SHADOWED = 0x20,
  TOP_IS_NOT_DYN = 0x40,
  TOP_IS_SHALLOW = 0x80,
};

VECTOR(vecbool, bool, 4);
IMPLEMENT_VECTOR(static inline, vecbool, bool);

VECTOR(vecnode, struct node *, 4);
IMPLEMENT_VECTOR(static inline, vecnode, struct node *);

struct generic {
  size_t first_explicit_genarg;
  struct vecnode instances;
  struct typ *our_generic_functor_typ;

  const struct node *for_error;
};

struct toplevel {
  ident scope_name;
  uint32_t flags;
  enum builtingen builtingen;

  struct generic *generic;
  struct typset *topdeps;

  struct vecnode tentative_instantiations;

  ssize_t yet_to_pass;
};

struct phi_tracker_state {
  struct phi_tracker_state *prev;

  struct node *last;
};

struct node_nul {};
struct node_ident {
  ident name;

  struct scope *non_local_scope;

  struct node *def;
  struct node *prev_use;
  struct node *next_use;
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
  bool is_explicit;
};
struct node_tuple {};
struct node_tuplenth {
  size_t nth;
};
struct node_call {
  const struct node *return_through_ref_expr;
};
struct node_callnamedarg {
  ident name;
};
struct node_future {};
struct node_lambda {};
struct node_init {
  bool is_array;
  bool is_range;
  bool is_bounds;
  struct node *defchoice;
  ident for_tag;
  const struct node *target_expr;
};
struct node_return {
  const struct node *return_through_ref_expr;
  bool forced_return_through_ref;
};
struct node_block {
  bool is_scopeless;
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
struct node_excep {
  ident label;
};
struct node_throw {
  ident label;
};
struct node_jump {
  struct node *to;
  bool is_break;
  bool is_continue;
  ident label;
};

struct ancestor {
  struct node *prev;
  struct node *cond;
  bool reversed;
};

VECTOR(vecancestor, struct ancestor, 2);
DECLARE_VECTOR(vecancestor, struct ancestor);

struct node_phi {
  struct node *def;

  struct vecancestor ancestors;

  bool is_conditioned;
  bool is_used;
};
struct node_typeconstraint {
  bool is_constraint;
};
struct node_dyn {
  struct typ *intf_typ;
};
struct node_deffun {
  struct toplevel toplevel;
  ssize_t min_args, max_args, first_vararg;
  const struct node *member_isa;
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
  struct typ *tag_typ;
  struct node *default_choice;
};
struct node_defmethod {
  struct toplevel toplevel;
  enum token_type access;
  ssize_t min_args, max_args, first_vararg;
  const struct node *member_isa;
};
struct node_defintf {
  struct toplevel toplevel;
};
struct node_defincomplete {
  struct toplevel toplevel;
  ident ident;
  const struct node *ident_for_error;
  bool is_isalist_literal;
};
struct node_defalias {
  uint32_t passed;

  const struct node *member_isa;
};
struct node_defname {
  struct node *ssa_user;
  bool is_globalenv;
  uint32_t passed;

  const struct node *member_isa;

  struct phi_tracker_state *phi_state;
};
struct node_defpattern {
  bool is_alias;
  bool is_globalenv;
};
struct node_defarg {
  bool is_optional;
  bool is_vararg;
  bool is_retval;
  bool is_explicit;

  struct phi_tracker_state *phi_state;
};
struct node_defgenarg {
  bool is_explicit;
};
struct node_setgenarg {
  const struct node *for_error;
};
struct node_let {
  struct toplevel toplevel;
};
struct node_deffield {
  const struct node *member_isa;
};
struct node_defchoice {
  bool is_leaf;
  bool has_tag;
  bool has_payload;
};
struct node_isa {
  bool is_export;
  bool is_explicit;
};
struct node_delegate {
  struct toplevel toplevel;
};
struct node_pre {
  struct toplevel toplevel;
};
struct node_post {
  struct toplevel toplevel;
};
struct node_invariant {
  struct toplevel toplevel;
};
struct node_example {
  struct toplevel toplevel;
  size_t name;
};
struct node_within {
  struct toplevel toplevel;
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
  struct scope globalenv_scope;
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
  struct node_call CALL;
  struct node_callnamedarg CALLNAMEDARG;
  struct node_future FUTURE;
  struct node_init INIT;
  struct node_return RETURN;
  struct node_block BLOCK;
  struct node_for FOR;
  struct node_break BREAK;
  struct node_continue CONTINUE;
  struct node_if IF;
  struct node_match MATCH;
  struct node_try TRY;
  struct node_catch CATCH;
  struct node_excep EXCEP;
  struct node_throw THROW;
  struct node_jump JUMP;
  struct node_phi PHI;
  struct node_typeconstraint TYPECONSTRAINT;
  struct node_dyn DYN;
  struct node_deffun DEFFUN;
  struct node_deftype DEFTYPE;
  struct node_defincomplete DEFINCOMPLETE;
  struct node_defmethod DEFMETHOD;
  struct node_defintf DEFINTF;
  struct node_defalias DEFALIAS;
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
  struct node_within WITHIN;
  struct node_isa ISA;
  struct node_import IMPORT;
  struct node_module MODULE;
  struct node_module_body MODULE_BODY;
  struct node_directdef DIRECTDEF;
};

enum node_flags {
  NODE_IS_TYPE = 0x1,
  NODE_IS_DEFCHOICE = 0x2,
  NODE_IS_TEMPORARY = 0x4,
  NODE_IS_GLOBAL_LET = 0x8,
  NODE__TRANSITIVE = NODE_IS_TYPE,
  NODE__ASSIGN_TRANSITIVE = NODE_IS_DEFCHOICE,
  NODE__DETACHED = 0x10,
};

struct node {
  enum node_which which;
  uint32_t flags;
  uint32_t excepted;

  struct node *parent;
  struct node *next;
  struct node *prev;
  struct node *subs_first;
  struct node *subs_last;

  struct typ *typ;
  struct scope scope;
  struct constraint *constraint;

  union node_as as;
  struct codeloc codeloc;
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
  case DEFINCOMPLETE:
    assert(false && "Don't do that");
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

#define REVERSE_FOREACH_SUB_CONST(s, n) \
  for (const struct node *__p_##s = (n), *s = __p_##s->subs_last; \
       s != NULL; s = s->prev)

#define FOREACH_SUB_EVERY_CONST(s, n, from, every) \
  for (const struct node *__p_##s = (n), *s = try_node_subs_at_const(__p_##s, from); \
       s != NULL; s = NODE_NEXTTH(s, every))

static inline size_t subs_count(const struct node *node) {
  size_t n = 0;
  FOREACH_SUB_CONST(s, node) {
    n += 1;
  }
  return n;
}

// Prefer whenever possible.
static inline bool subs_count_atleast(const struct node *node, size_t min) {
  if (min == 0) {
    return true;
  }

  size_t n = 0;
  FOREACH_SUB_CONST(s, node) {
    n += 1;
    if (n >= min) {
      return true;
    }
  }
  return false;
}

static inline const struct node *subs_first_const(const struct node *node) {
  return node->subs_first;
}

static inline const struct node *subs_last_const(const struct node *node) {
  return node->subs_last;
}

static inline const struct node *next_const(const struct node *node) {
  return node->next;
}

static inline const struct node *prev_const(const struct node *node) {
  return node->prev;
}

static inline const struct node *subs_at_const(const struct node *node, size_t n) {
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

static inline struct node *subs_first(struct node *node) {
  return CONST_CAST(subs_first_const(node));
}

static inline struct node *subs_last(struct node *node) {
  return CONST_CAST(subs_last_const(node));
}

static inline struct node *next(struct node *node) {
  return CONST_CAST(next_const(node));
}

static inline struct node *prev(struct node *node) {
  return CONST_CAST(prev_const(node));
}

static inline struct node *subs_at(struct node *node, size_t n) {
  return CONST_CAST(subs_at_const(node, n));
}

static inline struct node *try_node_subs_at(struct node *node, size_t n) {
  return CONST_CAST(try_node_subs_at_const(node, n));
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

  sub->parent = NULL;
  sub->prev = NULL;
  sub->next = NULL;
}

static inline void node_subs_append(struct node *node, struct node *sub) {
  assert(sub->prev == NULL && sub->next == NULL && sub->parent == NULL);
  sub->parent = node;
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
  assert(sub->prev == NULL && sub->next == NULL && sub->parent == NULL);
  sub->parent = node;
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
  assert(sub->prev == NULL && sub->next == NULL && sub->parent == NULL);
  sub->parent = node;
  if (where == NULL) {
    assert(node->subs_first == NULL && node->subs_last == NULL);
    node->subs_first = sub;
    node->subs_last = sub;
    return;
  }

  assert(where->parent == node);
  struct node *prev = where->prev;
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
  assert(sub->prev == NULL && sub->next == NULL && sub->parent == NULL);
  sub->parent = node;
  if (where == NULL) {
    assert(node->subs_first == NULL && node->subs_last == NULL);
    node->subs_first = sub;
    node->subs_last = sub;
    return;
  }
  assert(where->parent == node);
  struct node *next = where->next;
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
  assert(sub->prev == NULL && sub->next == NULL && sub->parent == NULL);
  sub->parent = node;
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

  where->parent = NULL;
  where->prev = NULL;
  where->next = NULL;
}

enum subnode_idx {
  IDX_GENARGS = 1,
  IDX_ISALIST = 2,
  IDX_CH_TAG_FIRST = 1,
  IDX_CH_TAG_LAST = 2,
  IDX_CH_FIRST_PAYLOAD = 3,
  IDX_FOR_IT = 0,
  IDX_FOR_IT_DEFP = 0,
  IDX_FOR_IT_DEFP_DEFN = 1,
  IDX_FOR_IT_DEFP_EXPR = 0,
  IDX_FOR_IT_BLOCK = 1,
  IDX_FOR_IT_BLOCK_WHILE = 0,
  IDX_FOR_IT_BLOCK_WHILE_BLOCK = 1,
  IDX_FUNARGS = 2,
  IDX_WITHIN = 3,
  IDX_DEFNAME_EXCEP_TEST = 0,
};

struct top_state {
  struct top_state *prev;

  struct node *top;
  uint32_t topdep_mask;
};

struct block_state {
  struct block_state *prev;

  struct node *current_statement;
};

struct wildcard {
  enum token_type ref;
  enum token_type nulref;
  enum token_type deref;
  enum token_type acc;
};

struct fun_state {
  struct fun_state *prev;

  bool fun_uses_final;
  const struct node *retval;
  struct wildcard wildcard;
  struct wildcard self_wildcard;

  struct block_state *block_state;
};

struct try_state {
  struct try_state *prev;

  struct node *tryy;
  struct vecnode excepts;
};

struct branch_state {
  struct branch_state *prev;

  struct node *branching;
  struct node *cond;
  bool reversed;
};

struct step_state {
  struct step_state *prev;

  bool upward;
  size_t stepping;
};

struct stackel {
  struct node *node;
  struct node *sub;
};

struct lir_state;

struct module_state {
  struct module_state *prev;

  bool tentatively;

  struct lir_state *lir_state;

  struct top_state *top_state;
  struct fun_state *fun_state;
  struct try_state *try_state;
  struct branch_state *branch_state;

  size_t furthest_passing;
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
  ID_TAG,
  ID_FIRST_TAG,
  ID_LAST_TAG,
  ID_AS,
  ID_TAG_TYPE,
  ID_AS_TYPE,
  ID_HAS_NEXT,
  ID_NEXT,
  ID_CAST,
  ID_WILDCARD_REF_ARG,
  ID_WILDCARD_REF_ARG_SELF,
  ID_LIKELY,
  ID_UNLIKELY,
  ID_NLANG,
  ID_NCODELOC,

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
  ID_TBI_ANY_ANY_SLICE,
  ID_TBI_ANY_SLICE,
  ID_TBI_ANY_MUTABLE_SLICE,
  ID_TBI_SLICE,
  ID_TBI_MSLICE,
  ID_TBI_SLICE_IMPL,
  ID_TBI_VARARG,
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
  ID_TBI_NOT_HAS_EQUALITY,
  ID_TBI_ORDERED,
  ID_TBI_NOT_ORDERED,
  ID_TBI_EQUALITY_BY_COMPARE,
  ID_TBI_ORDERED_BY_COMPARE,
  ID_TBI_COPYABLE,
  ID_TBI_NOT_COPYABLE,
  ID_TBI_DEFAULT_CTOR,
  ID_TBI_NON_DEFAULT_CTOR,
  ID_TBI_DEFAULT_DTOR,
  ID_TBI_ARRAY_CTOR,
  ID_TBI_TRIVIAL_COPY,
  ID_TBI_TRIVIAL_COPY_BUT_OWNED,
  ID_TBI_TRIVIAL_CTOR,
  ID_TBI_TRIVIAL_ARRAY_CTOR,
  ID_TBI_TRIVIAL_DTOR,
  ID_TBI_TRIVIAL_COMPARE,
  ID_TBI_TRIVIAL_EQUALITY,
  ID_TBI_TRIVIAL_ORDER,
  ID_TBI_RETURN_BY_COPY,
  ID_TBI_NOT_RETURN_BY_COPY,
  ID_TBI_ENUM,
  ID_TBI_UNION,
  ID_TBI_UNION_TRIVIAL_CTOR,
  ID_TBI_RANGE,
  ID_TBI_INDEX_BOUNDS,
  ID_TBI_ITERATOR,
  ID_TBI_ENVIRONMENT,
  ID_TBI_ANY_ENVIRONMENT,
  ID_TBI_PREVENT_DYN,
  ID_TBI__NOT_TYPEABLE,
  ID_TBI__FIRST_MARKER = ID_TBI__NOT_TYPEABLE,
  ID_TBI__CALL_FUNCTION_SLOT,
  ID_TBI__MUTABLE,
  ID_TBI__MERCURIAL,
  ID_TBI__LAST = ID_TBI__MERCURIAL,

  ID_CTOR,
  ID_DTOR,
  ID_COPY_CTOR,
  ID_C,
  ID_OTHER,
  ID_FROM_STATIC_STRING,
  ID_FROM_BOOL,
  ID_FROM_ARRAY,
  ID_FROM_TAG,
  ID_NRETVAL,
  ID_OPERATOR_OR,
  ID_OPERATOR_AND,
  ID_OPERATOR_NOT,
  ID_OPERATOR_TEST,
  ID_OPERATOR_COMPARE,
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
  ID_OPERATOR_AT,
  ID_OPERATOR_SUB,
  ID_PARENT,
  ID_INSTALL,
  ID_UNINSTALL,
  ID_SHOW,

  ID__NUM,
};

HTABLE_SPARSE(idents_map, ident, struct token);
struct idents_map;
DECLARE_HTABLE_SPARSE(idents_map, ident, struct token);

struct idents {
  const char **values;
  size_t capacity;
  size_t count;

  struct idents_map *map;
};

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
  struct module *passing_in_mod;
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

void module_retval_set(struct module *mod, const struct node *retval);
const struct node *module_retval_get(struct module *mod);
void module_excepts_open_try(struct module *mod, struct node *tryy);
void module_excepts_push(struct module *mod, struct node *excep_node);
struct try_state *module_excepts_get(struct module *mod);
void module_excepts_close_try(struct module *mod);

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
  case DEFALIAS:
  case DEFNAME:
  case DEFARG:
  case DEFGENARG:
  case SETGENARG:
    return node_ident(subs_first_const(node));
  case PHI:
    if (vecancestor_count(CONST_CAST(&node->as.PHI.ancestors))
        == 0) {
      return ID__NONE;
    } else {
      struct ancestor *ancestor = vecancestor_get(
        CONST_CAST(&node->as.PHI.ancestors), 0);
      return node_ident(ancestor->prev);
    }
  case CALLNAMEDARG:
    return node->as.CALLNAMEDARG.name;
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
    if (subs_first_const(node)->which == IDENT) {
      return subs_first_const(node)->as.IDENT.name;
    } else {
      return subs_at_const(subs_first_const(node), 1)->as.IDENT.name;
    }
    break;
  case DEFTYPE:
  case DEFFIELD:
  case DEFCHOICE:
  case DEFINTF:
  case DEFINCOMPLETE:
    assert(subs_first_const(node)->which == IDENT);
    return subs_first_const(node)->as.IDENT.name;
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

static inline struct node *parent(struct node *node) {
  return node->parent;
}

static inline const struct node *parent_const(const struct node *node) {
  return node->parent;
}

bool node_is_prototype(const struct node *node);
bool node_is_inline(const struct node *node);
bool node_is_opaque(const struct node *node);
bool node_is_export(const struct node *node);
bool node_is_extern(const struct node *node);
bool node_is_def(const struct node *node);
bool node_is_statement(const struct node *node);
bool node_is_rvalue(const struct node *node);
bool node_is_at_top(const struct node *node);
bool node_is_name_of_globalenv(const struct node *node);

static inline bool node_can_have_genargs(const struct node *node) {
  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
  case DEFTYPE:
  case DEFINTF:
    return true;
  default:
    return false;
  }
}

struct node *node_new_subnode(struct module *mod, struct node *node);
bool node_has_tail_block(const struct node *node);
bool node_is_fun(const struct node *node);
size_t node_fun_all_args_count(const struct node *def);
size_t node_fun_min_args_count(const struct node *def);
size_t node_fun_max_args_count(const struct node *def);
ssize_t node_fun_first_vararg(const struct node *def);
struct node *node_fun_retval(struct node *def);
const struct node *node_fun_retval_const(const struct node *def);
size_t node_branching_exhaustive_branch_count(struct node *node);

#define STEP_NM_DEFS_NO_FUNS \
  (NM(DEFTYPE) | NM(DEFINTF) | NM(DEFINCOMPLETE))

#define STEP_NM_DEFS \
   (STEP_NM_DEFS_NO_FUNS | NM(DEFFUN) | NM(DEFMETHOD))

#define STEP_NM_HAS_TOPLEVEL \
  (STEP_NM_DEFS | NM(LET) | NM(INVARIANT) | NM(DELEGATE) \
   | NM(IMPORT) | NM(EXAMPLE) | NM(WITHIN))

#define STEP_NM_BRANCHING \
  (NM(IF) | NM(FOR) | NM(WHILE) | NM(MATCH) | NM(TRY))

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
  case DEFINCOMPLETE:
    toplevel = &node->as.DEFINCOMPLETE.toplevel;
    break;
  case LET:
    toplevel = &node->as.LET.toplevel;
    break;
  case DELEGATE:
    toplevel = &node->as.DELEGATE.toplevel;
    break;
  case PRE:
    toplevel = &node->as.PRE.toplevel;
    break;
  case POST:
    toplevel = &node->as.POST.toplevel;
    break;
  case INVARIANT:
    toplevel = &node->as.INVARIANT.toplevel;
    break;
  case EXAMPLE:
    toplevel = &node->as.EXAMPLE.toplevel;
    break;
  case WITHIN:
    toplevel = &node->as.WITHIN.toplevel;
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
  return CONST_CAST(node_toplevel_const(node));
}

struct node *node_get_member(struct module *mod, struct node *node, ident id);
const struct node *node_get_member_const(const struct module *mod, const struct node *node, ident id);
struct node *mk_node(struct module *mod, struct node *parent, enum node_which kind);
void node_deepcopy(struct module *mod, struct node *dst,
                   const struct node *src);

struct node *defincomplete_create(struct module *mod, const struct node *trigger);
void defincomplete_set_ident(struct module *mod, const struct node *for_error,
                             struct node *dinc, ident name);
void defincomplete_add_field(struct module *mod, const struct node *for_error,
                             struct node *dinc, ident field, struct typ *t);
void defincomplete_add_isa(struct module *mod, const struct node *for_error,
                           struct node *dinc, struct typ *tisa);
error defincomplete_catchup(struct module *mod, struct node *dinc);
int snprint_defincomplete(char *s, size_t len,
                          const struct module *mod, const struct node *dinc);

#define GSTART() \
  unused__ struct node *gparent = NULL

#define G0(var, parent, which, ...) \
  unused__ \
  struct node *var = mk_node(mod, parent, which); \
  gparent = var; \
  __VA_ARGS__

#define G(var, which, ...) \
  unused__ \
  struct node *var = mk_node(mod, gparent, which); \
  struct node *gparent_##var = gparent; \
  gparent = var; \
  __VA_ARGS__; \
  gparent = gparent_##var; \

#define G_IDENT(var, _name, ...) \
  G(var, IDENT, \
     { \
       const char *__name = (_name); \
       var->as.IDENT.name = idents_add_string(mod->gctx, __name, strlen(__name)); \
     } \
     __VA_ARGS__)

// Return value must be freed by caller.
char *typ_name(const struct module *mod, const struct typ *t);
// Return value must be freed by caller.
char *typ_pretty_name(const struct module *mod, const struct typ *t);
void debug_print_topdeps(const struct module *mod, const struct node *node);

int snprint_codeloc(char *s, size_t len,
                    const struct module *mod, const struct node *node);
error mk_except(const struct module *mod, const struct node *node, const char *fmt, ...)
  __attribute__((__format__(__printf__, 3, 4)));
error mk_except_type(const struct module *mod, const struct node *node, const char *fmt, ...)
  __attribute__((__format__(__printf__, 3, 4)));
error mk_except_call_args_count(const struct module *mod, const struct node *node,
                                const struct node *definition, bool implicit_self, size_t given);

#define GOTO_EXCEPT_TYPE(mod, node, fmt, ...) do { \
  e = mk_except_type(mod, node, fmt, ##__VA_ARGS__); \
  GOTO_EXCEPT(e); \
} while (0)

#endif