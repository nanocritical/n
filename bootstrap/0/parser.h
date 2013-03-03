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
  DEFPATTERN,
  DEFARG,
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
  BG__NUM,
};

const char *builtingen_abspath[BG__NUM];

struct toplevel {
  bool is_export;
  bool is_extern;
  bool is_inline;
  ident scope_name;
  bool is_prototype;
  struct node *forward_declaration;
  struct node *full_definition;
  enum builtingen builtingen;

  const struct typ **instances;
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
};
struct node_defmethod {
  struct toplevel toplevel;
  enum token_type access;
};
struct node_defintf {
  struct toplevel toplevel;
};
struct node_defname {
  struct node *pattern;
  struct node *expr;
};
struct node_defpattern {};
struct node_defarg {};
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
struct node_module_body {
};

struct node_directdef {
  struct node *definition;
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
  struct node_defpattern DEFPATTERN;
  struct node_defarg DEFARG;
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
  ID_TBI_NATIVE_INTEGER,
  ID_TBI_COMPARABLE,
  ID_TBI_COPYABLE,
  ID_TBI_DEFAULT_CTOR,
  ID_TBI_TRIVIAL_CTOR,
  ID_TBI_CTOR_WITH,
  ID_TBI_RETURN_BY_COPY,
  ID_TBI__PENDING_DESTRUCT,
  ID_TBI__FIRST_MARKER = ID_TBI__PENDING_DESTRUCT,
  ID_TBI__NOT_TYPEABLE,
  ID_TBI__LAST = ID_TBI__NOT_TYPEABLE,

  ID_MK,
  ID_NEW,
  ID_CTOR,
  ID_C,
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
  TBI_NATIVE_INTEGER,
  TBI_COMPARABLE,
  TBI_COPYABLE,
  TBI_DEFAULT_CTOR,
  TBI_TRIVIAL_CTOR,
  TBI_CTOR_WITH,
  TBI_RETURN_BY_COPY,
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
  const struct typ **gen_args; // length gen_arity + 1

  size_t fun_arity;
  const struct typ **fun_args; // length fun_arity + 1

  size_t isalist_count;
  const struct typ **isalist;
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
  struct node *body;
  size_t next_gensym;

  const struct node *return_node;
  struct try_excepts *trys;
  size_t trys_count;

  ident path[MODULE_PATH_MAXLEN];
  size_t path_len;
};

void globalctx_init(struct globalctx *gctx);

error module_open(struct globalctx *gctx, struct module *mod,
                  const char *prefix, const char *fn);
error need_instance(struct module *mod, struct node *needer, const struct typ *typ);
void module_return_set(struct module *mod, const struct node *return_node);
const struct node *module_return_get(struct module *mod);
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
error scope_lookup_ident_wontimport(struct node **result, const struct module *mod,
                                    const struct scope *scope, ident id, bool failure_ok);
error scope_lookup_ident_immediate(struct node **result, const struct module *mod,
                                   const struct scope *scope, ident id,
                                   bool failure_ok);
error scope_lookup(struct node **result, const struct module *mod,
                   const struct scope *scope, const struct node *id);
error scope_lookup_module(struct node **result, const struct module *mod,
                          const struct node *id);
error scope_lookup_abspath(struct node **result, const struct module *mod,
                           const char *path);
char *scope_name(const struct module *mod, const struct scope *scope);
char *scope_definitions_name_list(const struct module *mod, const struct scope *scope);

void copy_and_extend_import_path(struct module *mod, struct node *imported,
                                 const struct node *import, const struct token *tok);

const struct module *node_module_owner_const(const struct node *node);
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
struct node *node_new_subnode(const struct module *mod, struct node *node);
size_t node_fun_explicit_args_count(const struct node *def);
const struct node *node_fun_retval(const struct node *def);
struct toplevel *node_toplevel(struct node *node);
const struct toplevel *node_toplevel_const(const struct node *node);
struct node *mk_node(struct module *mod, struct node *parent, enum node_which kind);
struct node *node_typ_member(const struct typ *typ, const char *member);
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
error typ_unify(const struct typ **u, const struct module *mod, const struct node *for_error,
                const struct typ *a, const struct typ *b);
bool typ_isa(const struct module *mod, const struct typ *a, const struct typ *intf);
error typ_check_isa(const struct module *mod, const struct node *for_error,
                    const struct typ *a, const struct typ *intf);
error mk_except(const struct module *mod, const struct node *node, const char *fmt);
error mk_except_type(const struct module *mod, const struct node *node, const char *fmt);
error mk_except_call_arg_count(const struct module *mod, const struct node *node,
                               const struct node *definition, size_t given);
char *typ_name(const struct module *mod, const struct typ *t);
bool typ_is_builtin(const struct module *mod, const struct typ *t);

#endif
