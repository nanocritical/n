#ifndef PARSER_H__
#define PARSER_H__

#include "common.h"
#include "lexer.h"

typedef uint32_t ident;

enum type_node {
  NUL = 1,
  IDENT,
  NUMBER,
  STRING,
  BIN,
  UN,
  TUPLE,
  REF,
  DEREF,
  CALL,
  INIT,
  RETURN,
  EXCEP,
  BLOCK,
  FOR,
  WHILE,
  BREAK,
  CONTINUE,
  PASS,
  IF,
  MATCH,
  TRY,
  TYPEEXPR,
  TYPECONSTRAINT,
  DEFFUN,
  DEFTYPE,
  DEFMETHOD,
  DEFINTF,
  DEFLET,
  DELEGATE,
  PRE,
  POST,
  INVARIANT,
  EXAMPLE,
  ISALIST,
  IMPORT,
  IMPORT_PATH,
  MODULE,
};

struct toplevel {
  bool is_inline;
  bool is_extern;
  ident scope;
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
struct node_ref {
  enum token_type access;
};
struct node_deref {
  enum token_type access;
};
struct node_member {
  enum token_type access;
};
struct node_call {};
struct node_block {};
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
struct node_typeexpr {};
struct node_deffun {
  struct toplevel toplevel;
};
struct node_deftype {
  struct toplevel toplevel;
  size_t isa_count;
};
struct node_defmethod {
  struct toplevel toplevel;
};
struct node_defintf {
  struct toplevel toplevel;
  size_t isa_count;
};
struct node_deflet {
  struct toplevel toplevel;
};
struct node_isalist {};
struct node_delegate {};
struct node_pre {};
struct node_post {};
struct node_invariant {};
struct node_example {};
struct node_import {
  struct toplevel toplevel;
  bool is_export;
  bool is_all;
};
struct node_import_path {};
struct node_module {
  ident name;
};

union choice_node {
  struct node_nul NUL;
  struct node_ident IDENT;
  struct node_number NUMBER;
  struct node_string STRING;
  struct node_bin BIN;
  struct node_un UN;
  struct node_tuple TUPLE;
  struct node_ref REF;
  struct node_deref DEREF;
  struct node_call CALL;
  struct node_init INIT;
  struct node_return RETURN;
  struct node_break BREAK;
  struct node_continue CONTINUE;
  struct node_if IF;
  struct node_match MATCH;
  struct node_try TRY;
  struct node_typeconstraint TYPECONSTRAINT;
  struct node_typeexpr TYPEEXPR;
  struct node_deffun DEFFUN;
  struct node_deftype DEFTYPE;
  struct node_defmethod DEFMETHOD;
  struct node_defintf DEFINTF;
  struct node_deflet DEFLET;
  struct node_delegate DELEGATE;
  struct node_pre PRE;
  struct node_post POST;
  struct node_invariant INVARIANT;
  struct node_example EXAMPLE;
  struct node_import IMPORT;
  struct node_import_path IMPORT_PATH;
  struct node_module MODULE;
};

struct node {
  enum type_node which;
  union choice_node as;

  size_t codeloc;

  size_t subs_count;
  struct node *subs;

  struct scope *scope;
};

struct idents_map;

struct idents {
  const char **values;
  size_t capacity;
  size_t count;

  struct idents_map *map;
};

struct scope_map;

struct scope {
  struct scope_map *map;
  struct scope *parent;
  struct node *node;
};

struct module {
  const char *filename;

  struct parser parser;
  struct idents idents;
  struct node node;
  struct scope *scopes;
  size_t scopes_count;

  ident path[128];
  size_t path_len;
};

error module_open(struct module *mod, const char *fn);

const char *idents_value(const struct module *mod, ident id);
ident idents_add(struct module *mod, const struct token *tok);

struct scope *scope_new(struct node *node);
error scope_get(struct node **node,
                const struct module *mod, const struct scope *scope, ident id);
error scope_add_ident(const struct module *mod, struct scope *scope, ident id, struct node *node);
error scope_add(const struct module *mod, struct scope *scope, struct node *id, struct node *node);

#endif
