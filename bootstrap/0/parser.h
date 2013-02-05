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
  STRING,
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
  ROOT_OF_ALL,
  DIRECTDEF,
  BUILTINGEN,
  NODE__NUM,
};

const char *node_which_strings[NODE__NUM];

struct toplevel {
  bool is_export;
  bool is_extern;
  bool is_inline;
  ident scope_name;
  bool is_prototype;
};

struct node_nul {};
struct node_ident {
  ident name;
};
struct node_number {
  const char *value;
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
struct node_call {};
struct node_future {};
struct node_lambda {};
struct node_init {};
struct node_return {};
struct node_for {};
struct node_while {};
struct node_break {};
struct node_continue {};
struct node_pass {};
struct node_if {};
struct node_match {};
struct node_try {};
struct node_typeconstraint {
  bool is_arg;
};
struct node_deffun {
  struct toplevel toplevel;
};

enum deftype_kind {
  DEFTYPE_PROTOTYPE = 0,
  DEFTYPE_STRUCT,
  DEFTYPE_ENUM,
  DEFTYPE_SUM,
};

struct node_deftype {
  struct toplevel toplevel;

  enum deftype_kind kind;
};
struct node_defmethod {
  struct toplevel toplevel;
  enum token_type access;
};
struct node_defintf {
  struct toplevel toplevel;
};
struct node_defname {};
struct node_let {
  struct toplevel toplevel;
};
struct node_deffield {};
struct node_defchoice {
  bool has_value;
};
struct node_isalist {};
struct node_isa {
  struct toplevel toplevel;
};
struct node_delegate {};
struct node_pre {};
struct node_post {};
struct node_invariant {};
struct node_example {};
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

struct node_directdef {
  struct node *definition;
};

enum builtingen {
  BG_ENUM_EQ,
  BG_ENUM_NE,
  BG_SUM_EQ,
  BG_SUM_NE,
};

struct node_builtingen {
  enum builtingen gen;
};

union node_as {
  struct node_nul NUL;
  struct node_ident IDENT;
  struct node_number NUMBER;
  struct node_string STRING;
  struct node_bin BIN;
  struct node_un UN;
  struct node_tuple TUPLE;
  struct node_call CALL;
  struct node_future FUTURE;
  struct node_init INIT;
  struct node_return RETURN;
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
  struct node_directdef DIRECTDEF;
  struct node_builtingen BUILTINGEN;
};

struct scope;
struct typ;

struct node {
  enum node_which which;
  union node_as as;

  size_t codeloc;

  size_t subs_count;
  struct node **subs;

  struct scope *scope;
  struct typ *typ;
  bool is_type;
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

  ID_MAIN,
  ID_WHICH,
  ID_AS,
  ID_WHICH_TYPE,
  ID_AS_TYPE,

  ID_TBI_VOID,
  ID_TBI__FIRST = ID_TBI_VOID,
  ID_TBI_LITERALS_NULL,
  ID_TBI_LITERALS_INTEGER,
  ID_TBI_PSEUDO_TUPLE,
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
  ID_TBI_STRING,
  ID_TBI_REF,
  ID_TBI_MREF,
  ID_TBI_MMREF,
  ID_TBI_NREF,
  ID_TBI_NMREF,
  ID_TBI_NMMREF,
  ID_TBI_DYN,
  ID_TBI_NATIVE_INTEGER,
  ID_TBI_BUILTIN_ENUM,
  ID_TBI__PENDING_DESTRUCT,
  ID_TBI__FIRST_MARKER = ID_TBI__PENDING_DESTRUCT,
  ID_TBI__NOT_TYPEABLE,
  ID_TBI__LAST = ID_TBI__NOT_TYPEABLE,

  ID_OPERATOR_OR,
  ID_OPERATOR_AND,
  ID_OPERATOR_NOT,
  ID_OPERATOR_LE,
  ID_OPERATOR_LT,
  ID_OPERATOR_GT,
  ID_OPERATOR_GE,
  ID_OPERATOR_EQ,
  ID_OPERATOR_NE,
  ID_OPERATOR_BWOR,
  ID_OPERATOR_BWXOR,
  ID_OPERATOR_BWAND,
  ID_OPERATOR_LSHIFT,
  ID_OPERATOR_RSHIFT,
  ID_OPERATOR_PLUS,
  ID_OPERATOR_MINUS,
  ID_OPERATOR_DIVIDE,
  ID_OPERATOR_MODULO,
  ID_OPERATOR_TIMES,
  ID_OPERATOR_UMINUS,
  ID_OPERATOR_UBWNOT,

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
  TBI_PSEUDO_TUPLE,
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
  TBI_STRING,
  TBI_REF, // @
  TBI_MREF, // @!
  TBI_MMREF, // @#
  TBI_NREF, // ?@
  TBI_NMREF, // ?@!
  TBI_NMMREF, // ?@#
  TBI_DYN,
  TBI_NATIVE_INTEGER,
  TBI_BUILTIN_ENUM,
  TBI__PENDING_DESTRUCT,
  TBI__NOT_TYPEABLE,
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
  struct typ **gen_args; // length gen_arity + 1

  size_t fun_arity;
  struct typ **fun_args; // length fun_arity + 1

  size_t isalist_count;
  struct typ **isalist;
  bool *isalist_exported;
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

  struct typ *builtin_typs[TBI__NUM];
  struct typ *builtin_typs_by_name[ID__NUM];
};

struct module {
  struct globalctx *gctx;

  const char *filename;

  struct parser parser;
  struct node *root;
  size_t next_gensym;

  struct node *return_node;
  struct try_excepts *trys;
  size_t trys_count;

  ident path[MODULE_PATH_MAXLEN];
  size_t path_len;
};

void globalctx_init(struct globalctx *gctx);

error module_open(struct globalctx *gctx, struct module *mod,
                  const char *prefix, const char *fn);
void module_needs_instance(struct module *mod, struct typ *typ);
void module_return_set(struct module *mod, struct node *return_node);
struct node *module_return_get(struct module *mod);
void module_excepts_open_try(struct module *mod);
void module_excepts_push(struct module *mod, struct node *return_node);
void module_excepts_close_try(struct module *mod);

ident gensym(struct module *mod);

const char *idents_value(const struct globalctx *gctx, ident id);
ident idents_add(struct globalctx *gctx, const struct token *tok);

struct scope *scope_new(struct node *node);
error scope_define_ident(const struct module *mod, struct scope *scope, ident id, struct node *node);
error scope_define(const struct module *mod, struct scope *scope, struct node *id, struct node *node);
// Does not follow imports; use scope_lookup().
error scope_lookup_ident(struct node **result, const struct module *mod,
                         const struct scope *scope, ident id, bool failure_ok);
error scope_lookup(struct node **result, const struct module *mod,
                   const struct scope *scope, const struct node *id);
error scope_lookup_module(struct node **result, const struct module *mod,
                          const struct node *id);
char *scope_name(const struct module *mod, const struct scope *scope);
char *scope_definitions_name_list(const struct module *mod, const struct scope *scope);

void copy_and_extend_import_path(struct module *mod, struct node *imported,
                                 const struct node *import, const struct token *tok);

const struct module *node_module_owner_const(const struct node *node);
struct module *node_module_owner(struct node *node);

ident node_ident(const struct node *node);
bool node_is_prototype(const struct node *node);
bool node_is_inline(const struct node *node);
bool node_is_export(const struct node *node);
struct node *node_new_subnode(const struct module *mod, struct node *node);
size_t node_fun_args_count(const struct node *def);
const struct toplevel *node_toplevel(const struct node *node);
struct node *mk_node(struct module *mod, struct node *parent, enum node_which kind);
struct node *node_typ_member(struct typ *typ, const char *member);
void node_deepcopy(struct module *mod, struct node *dst,
                   const struct node *src);

struct typ *typ_new(struct node *definition,
                    enum typ_which which, size_t gen_arity, size_t fun_arity);
struct typ *typ_lookup_builtin(const struct module *mod, enum typ_builtin id);
error typ_compatible(const struct module *mod, const struct node *for_error,
                const struct typ *a, const struct typ *constraint);
error typ_compatible_numeric(const struct module *mod, const struct node *for_error, const struct typ *a);
error typ_compatible_reference(const struct module *mod, const struct node *for_error, const struct typ *a);
bool typ_is_reference_instance(const struct module *mod, const struct typ *a);
bool typ_is_concrete(const struct module *mod, const struct typ *a);
error typ_unify(struct typ **u, const struct module *mod, const struct node *for_error,
                struct typ *a, struct typ *b);
bool typ_isa(const struct module *mod, const struct typ *a, const struct typ *intf);
error mk_except(const struct module *mod, const struct node *node, const char *fmt);
error mk_except_type(const struct module *mod, const struct node *node, const char *fmt);
error mk_except_call_arg_count(const struct module *mod, const struct node *node,
                               const struct node *definition, size_t given);
char *typ_name(const struct module *mod, const struct typ *t);
bool typ_is_builtin(const struct module *mod, const struct typ *t);

#endif
