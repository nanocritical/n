#ifndef PARSER_H__
#define PARSER_H__

#include "common.h"
#include "lexer.h"

typedef uint32_t ident;

enum type_node {
  NUL,
  SIZEOF,
  IDENT,
  NUMBER,
  STRING,
  ASSIGN,
  ASSIGNOP,
  BIN,
  UN,
  TUPLE,
  REF,
  DEREF,
  MEMBER,
  CALL,
  BLOCK,
  INIT,
  RETURN,
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
  ISALIST,
  FIELD,
  IMPORT,
  EXPORT,
};

struct toplevel {
  bool is_inline;
  bool is_extern;
  ident scope;
};

struct node_nul {};
struct node_sizeof {};
struct node_ident {
  ident value;
};
struct node_number {
  const char *value;
};
struct node_string {
  const char *value;
};
struct node_assign {};
struct node_assignop {
  enum token_type operator;
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
struct node_field {};
struct node_import {
};
struct node_export {
};

union choice_node {
  struct node_nul NUL;
  struct node_sizeof SIZEOF;
  struct node_ident IDENT;
  struct node_number NUMBER;
  struct node_string STRING;
  struct node_assign ASSIGN;
  struct node_assignop ASSIGNOP;
  struct node_bin BIN;
  struct node_un UN;
  struct node_tuple TUPLE;
  struct node_ref REF;
  struct node_deref DEREF;
  struct node_member MEMBER;
  struct node_call CALL;
  struct node_block BLOCK;
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
  struct node_field FIELD;
  struct node_import IMPORT;
  struct node_export EXPORT;
};

struct node {
  enum type_node which;
  union choice_node as;

  size_t subs_count;
  struct node *subs;
};

struct idents {
  size_t count;
  size_t capacity;
  const char **values;
};

struct module {
  const char *filename;

  struct parser parser;
  struct idents idents;
  struct node node;

  ident path[128];
};

error module_open(struct module *mod, const char *fn);

#endif
