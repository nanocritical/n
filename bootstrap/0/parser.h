#ifndef PARSER_H__
#define PARSER_H__

#include "common.h"
#include "lexer.h"

#define MODULE_PATH_MAXLEN 16

typedef uint32_t ident;

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
  CALL,
  INIT,
  RETURN,
  EXCEP,
  BLOCK,
  FUTURE,
  LAMBDA,
  FOR,
  WHILE,
  BREAK,
  CONTINUE,
  PASS,
  IF,
  MATCH,
  TRY,
  TYPECONSTRAINT,
  DEFFUN,
  DEFTYPE,
  DEFMETHOD,
  DEFINTF,
  DEFNAME,
  DEFPATTERN,
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
  BG_CTOR_WITH_MK,
  BG_CTOR_WITH_NEW,
  BG_SUM_CTOR_WITH_CTOR,
  BG_SUM_CTOR_WITH_MK,
  BG_SUM_CTOR_WITH_NEW,
  BG_AUTO_MK,
  BG_AUTO_NEW,
  BG_ENUM_EQ,
  BG_ENUM_NE,
  BG_ENUM_MATCH,
  BG_SUM_MATCH,
  BG_SUM_DISPATCH,
  BG_SUM_COPY,
  BG_SUM_EQUALITY_EQ,
  BG_SUM_EQUALITY_NE,
  BG_SUM_ORDER_LE,
  BG_SUM_ORDER_LT,
  BG_SUM_ORDER_GT,
  BG_SUM_ORDER_GE,
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
  struct node *forward_declaration;
  struct node *full_definition;
  enum builtingen builtingen;

  struct node **instances;
  size_t instances_count;
  struct node *generic_definition;
};

struct node_nul {};
struct node_ident {
  ident name;
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
struct node_call {
  const struct node *return_through_ref_expr;
};
struct node_future {};
struct node_lambda {};
struct node_init {
  const struct node *target_expr;
};
struct node_return {
  const struct node *return_through_ref_expr;
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
struct node_try {};
struct node_typeconstraint {};
struct node_deffun {
  struct toplevel toplevel;
};

enum deftype_kind {
  DEFTYPE_PROTOTYPE = 0,
  DEFTYPE_STRUCT,
  DEFTYPE_ENUM,
  DEFTYPE_SUM,
};

struct typ;

struct node_deftype {
  struct toplevel toplevel;

  enum deftype_kind kind;
  const struct typ *choice_typ;

  struct node **members;
  size_t members_count;
};
struct node_defmethod {
  struct toplevel toplevel;
  enum token_type access;
};
struct node_defintf {
  struct toplevel toplevel;
  bool is_implied_generic;
};
struct node_defname {
  struct node *pattern;
  struct node *expr;
};
struct node_defpattern {
  bool is_alias;
};
struct node_defarg {
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
struct node_isalist {};
struct node_isa {
  bool is_export;
  bool is_explicit;
};
struct node_delegate {};
struct node_pre {};
struct node_post {};
struct node_invariant {};
struct node_example {
  size_t name;
};
struct node_import {
  struct toplevel toplevel;
  bool is_all;
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
  struct node *definition;
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
  struct node_future FUTURE;
  struct node_init INIT;
  struct node_return RETURN;
  struct node_for FOR;
  struct node_break BREAK;
  struct node_continue CONTINUE;
  struct node_if IF;
  struct node_match MATCH;
  struct node_try TRY;
  struct node_typeconstraint TYPECONSTRAINT;
  struct node_deffun DEFFUN;
  struct node_deftype DEFTYPE;
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
  struct node_isalist ISALIST;
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
  NODE__TRANSITIVE = NODE_IS_TYPE,
  NODE_IS_TEMPORARY = 0x4,
};

struct node {
  enum node_which which;
  union node_as as;

  size_t codeloc;

  size_t subs_count;
  struct node **subs;

  struct scope *scope;
  const struct typ *typ;
  uint32_t flags;
};

enum subnode_idx {
  IDX_GENARGS = 1,
  IDX_ISALIST = 2,
  IDX_BLOCK = 3,
  IDX_CH_VALUE = 1,
  IDX_CH_PAYLOAD = 2,
  IDX_FOR_IT = 0,
  IDX_FOR_IT_DEFP = 0,
  IDX_FOR_IT_DEFP_DEFN = 0,
  IDX_FOR_IT_DEFP_EXPR = 1,
  IDX_FUN_FIRSTARG = 2,
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
  ID_SELF,
  ID_OTHERWISE,

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

  ID_TBI_VOID,
  ID_TBI__FIRST = ID_TBI_VOID,
  ID_TBI_LITERALS_NULL,
  ID_TBI_LITERALS_INTEGER,
  ID_TBI_LITERALS_BOOLEAN,
  ID_TBI_LITERALS_FLOATING,
  ID_TBI_PSEUDO_TUPLE,
  ID_TBI_ANY,
  ID_TBI_BOOL,
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
  ID_TBI_STRING,
  ID_TBI_ANY_REF,
  ID_TBI_ANY_ANY_REF,
  ID_TBI_REF,
  ID_TBI_MREF,
  ID_TBI_MMREF,
  ID_TBI_NREF,
  ID_TBI_NMREF,
  ID_TBI_NMMREF,
  ID_TBI_ARITHMETIC,
  ID_TBI_INTEGER,
  ID_TBI_NATIVE_INTEGER,
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
  ID_TBI_TRIVIAL_COPY,
  ID_TBI_TRIVIAL_CTOR,
  ID_TBI_TRIVIAL_DTOR,
  ID_TBI_TRIVIAL_EQUALITY,
  ID_TBI_RETURN_BY_COPY,
  ID_TBI_SUM_COPY,
  ID_TBI_SUM_EQUALITY,
  ID_TBI_SUM_ORDER,
  ID_TBI_ITERATOR,
  ID_TBI__PENDING_DESTRUCT,
  ID_TBI__FIRST_MARKER = ID_TBI__PENDING_DESTRUCT,
  ID_TBI__NOT_TYPEABLE,
  ID_TBI__CALL_FUNCTION_SLOT,
  ID_TBI__MUTABLE,
  ID_TBI__MERCURIAL,
  ID_TBI__LAST = ID_TBI__MERCURIAL,

  ID_MK,
  ID_NEW,
  ID_CTOR,
  ID_C,
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
  ID_COPY_CTOR,

  ID__NUM,
};

struct scope_map;

struct scope {
  struct scope_map *map;
  struct scope *parent;
  struct node *node;
};

enum typ_builtin {
  TBI__NONE = 0,
  TBI_VOID,
  TBI_LITERALS_NULL,
  TBI_LITERALS_INTEGER,
  TBI_LITERALS_BOOLEAN,
  TBI_LITERALS_FLOATING,
  TBI_PSEUDO_TUPLE,
  TBI_ANY,
  TBI_BOOL,
  TBI_I8,
  TBI_U8,
  TBI_I16,
  TBI_U16,
  TBI_I32,
  TBI_U32,
  TBI_I64,
  TBI_U64,
  TBI_SIZE,
  TBI_SSIZE,
  TBI_FLOAT,
  TBI_DOUBLE,
  TBI_STRING,
  TBI_ANY_REF,
  TBI_ANY_ANY_REF,
  TBI_REF, // @
  TBI_MREF, // @!
  TBI_MMREF, // @#
  TBI_NREF, // ?@
  TBI_NMREF, // ?@!
  TBI_NMMREF, // ?@#
  TBI_ARITHMETIC,
  TBI_INTEGER,
  TBI_NATIVE_INTEGER,
  TBI_GENERALIZED_BOOLEAN,
  TBI_NATIVE_BOOLEAN,
  TBI_FLOATING,
  TBI_NATIVE_FLOATING,
  TBI_HAS_EQUALITY,
  TBI_ORDERED,
  TBI_ORDERED_BY_COMPARE,
  TBI_COPYABLE,
  TBI_DEFAULT_CTOR,
  TBI_CTOR_WITH,
  TBI_TRIVIAL_COPY,
  TBI_TRIVIAL_CTOR,
  TBI_TRIVIAL_DTOR,
  TBI_TRIVIAL_EQUALITY,
  TBI_RETURN_BY_COPY,
  TBI_SUM_COPY,
  TBI_SUM_EQUALITY,
  TBI_SUM_ORDER,
  TBI_ITERATOR,
  TBI__PENDING_DESTRUCT,
  TBI__NOT_TYPEABLE,
  TBI__CALL_FUNCTION_SLOT,
  TBI__MUTABLE,
  TBI__MERCURIAL,
  TBI__NUM,
};

enum typ_which {
  TYPE_DEF,
  TYPE_TUPLE,
  TYPE_FUNCTION,
  TYPE__MARKER,
};

struct typ {
  struct node *definition;
  enum typ_which which;
  size_t gen_arity;
  const struct typ **gen_args; // length gen_arity + 1

  size_t fun_arity;
  const struct typ **fun_args; // length fun_arity + 1

  size_t isalist_count;
  const struct typ **isalist;
  bool *isalist_exported;

  bool is_abstract_genarg;
};

struct try_excepts {
  struct node **excepts;
  size_t count;
};

struct globalctx {
  struct idents idents;
  // This node hierarchy is used to park loaded modules using their
  // absolute name. It is not used for lexical lookup.
  struct node modules_root;

  struct node **loaded;
  size_t loaded_count;

  struct typ *builtin_typs[TBI__NUM];
  struct typ *builtin_typs_by_name[ID__NUM];
};

struct module {
  struct globalctx *gctx;

  const char *filename;

  ident path[MODULE_PATH_MAXLEN];
  size_t path_len;

  struct parser parser;
  struct node *root;
  struct node *body;

  // State
  size_t next_gensym;
  bool afternoon;
  bool intf_uses_this;
  const struct node **return_nodes;
  size_t return_nodes_count;
  struct try_excepts *trys;
  size_t trys_count;
  enum token_type ref_wildcard;
  enum token_type nulref_wildcard;
  enum token_type deref_wildcard;
  enum token_type wildcard;
  size_t next_example;
  size_t next_pre;
  size_t next_post;
  size_t next_invariant;
};

void globalctx_init(struct globalctx *gctx);

error module_open(struct globalctx *gctx, struct module *mod,
                  const char *prefix, const char *fn);
error need_instance(struct module *mod, struct node *needer, const struct typ *typ);
void module_retval_push(struct module *mod, const struct node *return_node);
const struct node *module_retval_get(struct module *mod);
void module_retval_pop(struct module *mod);
void module_excepts_open_try(struct module *mod);
void module_excepts_push(struct module *mod, struct node *return_node);
void module_excepts_close_try(struct module *mod);

ident gensym(struct module *mod);

const char *idents_value(const struct globalctx *gctx, ident id);
ident idents_add(struct globalctx *gctx, const struct token *tok);
ident idents_add_string(struct globalctx *gctx, const char *name, size_t len);

struct scope *scope_new(struct node *node);
error scope_define_ident(const struct module *mod, struct scope *scope, ident id, struct node *node);
error scope_define(const struct module *mod, struct scope *scope, struct node *id, struct node *node);
error scope_lookup_ident_wontimport(struct node **result, const struct node *for_error,
                                    const struct module *mod,
                                    const struct scope *scope, ident id, bool failure_ok);
error scope_lookup_ident_immediate(struct node **result, const struct node *for_error,
                                   const struct module *mod,
                                   const struct scope *scope, ident id,
                                   bool failure_ok);
error scope_lookup(struct node **result, const struct module *mod,
                   const struct scope *scope, const struct node *id);
error scope_lookup_module(struct node **result, const struct module *mod,
                          const struct node *id, bool failure_ok);
error scope_lookup_abspath(struct node **result, const struct node *for_error,
                           const struct module *mod, const char *path);
char *scope_name(const struct module *mod, const struct scope *scope);
char *scope_definitions_name_list(const struct module *mod, const struct scope *scope);

void copy_and_extend_import_path(struct module *mod, struct node *imported,
                                 const struct node *import, const struct token *tok);

struct module *node_module_owner(struct node *node);
struct node *node_toplevel_owner(struct node *node);
struct node *node_statement_owner(struct node *node);

ident node_ident(const struct node *node);
bool node_is_prototype(const struct node *node);
bool node_is_inline(const struct node *node);
bool node_is_export(const struct node *node);
bool node_is_def(const struct node *node);
bool node_is_statement(const struct node *node);
bool node_is_rvalue(const struct node *node);
bool node_is_at_top(const struct node *node);
bool node_can_have_genargs(const struct node *node);
struct node *node_new_subnode(const struct module *mod, struct node *node);
size_t node_fun_all_args_count(const struct node *def);
size_t node_fun_explicit_args_count(const struct node *def);
struct node *node_fun_retval(struct node *def);
const struct node *node_fun_retval_const(const struct node *def);
struct toplevel *node_toplevel(struct node *node);
const struct toplevel *node_toplevel_const(const struct node *node);
struct node *mk_node(struct module *mod, struct node *parent, enum node_which kind);
struct node *node_typ_member(const struct typ *typ, const char *member);
void node_deepcopy(struct module *mod, struct node *dst,
                   const struct node *src);

struct typ *typ_new(struct node *definition,
                    enum typ_which which, size_t gen_arity, size_t fun_arity);
struct typ *typ_genarg_mark_as_abstract(const struct typ *t);
struct typ *typ_lookup_builtin(const struct module *mod, enum typ_builtin id);
bool typ_equal(const struct module *mod, const struct typ *a, const struct typ *b);
error typ_check_equal(const struct module *mod, const struct node *for_error,
                      const struct typ *a, const struct typ *b);
error typ_compatible(const struct module *mod, const struct node *for_error,
                     const struct typ *a, const struct typ *constraint);
error typ_compatible_numeric(const struct module *mod, const struct node *for_error, const struct typ *a);
error typ_check_reference_compatible(const struct module *mod, const struct node *for_error,
                                     enum token_type operator, const struct typ *a);
error typ_check_can_deref(const struct module *mod, const struct node *for_error,
                          const struct typ *a, enum token_type operator);
error typ_check_deref_against_mark(const struct module *mod, const struct node *for_error,
                                   const struct typ *t, enum token_type operator);
bool typ_is_reference_instance(const struct module *mod, const struct typ *a);
error typ_check_is_reference_instance(const struct module *mod, const struct node *for_error,
                                      const struct typ *a);
bool typ_is_concrete(const struct module *mod, const struct typ *a);
bool typ_is_abstract_instance(const struct module *mod, const struct typ *a);
error typ_unify(const struct typ **u, const struct module *mod, const struct node *for_error,
                const struct typ *a, const struct typ *b);
bool typ_isa(const struct module *mod, const struct typ *a, const struct typ *intf);
error typ_check_isa(const struct module *mod, const struct node *for_error,
                    const struct typ *a, const struct typ *intf);
error typ_find_matching_concrete_isa(const struct typ **concrete,
                                     const struct module *mod, const struct node *for_error,
                                     const struct typ *a, const struct typ *intf);
error mk_except(const struct module *mod, const struct node *node, const char *fmt, ...);
error mk_except_type(const struct module *mod, const struct node *node, const char *fmt, ...);
error mk_except_call_args_count(const struct module *mod, const struct node *node,
                                const struct node *definition, size_t extra, size_t given);
char *typ_name(const struct module *mod, const struct typ *t);
char *typ_pretty_name(const struct module *mod, const struct typ *t);
bool typ_is_builtin(const struct module *mod, const struct typ *t);

void rew_insert_last_at(struct node *node, size_t pos);
void rew_move_last_over(struct node *node, size_t pos, bool saved_it);
void rew_append(struct node *node, struct node *sub);
size_t rew_find_subnode_in_parent(struct node *parent, struct node *node);

#endif
