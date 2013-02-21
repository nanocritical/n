#ifndef _POSIX_SOURCE
# define _POSIX_SOURCE
#endif
#include <stdio.h>

#include "printer.h"

const char *c_token_strings[TOKEN__NUM] = {
  [TASSIGN] = " = ",
  [TEQ] = " == ",
  [TNE] = " != ",
  [TLE] = " <= ",
  [TLT] = " < ",
  [TGT] = " > ",
  [TGE] = " >= ",
  [TPLUS] = " + ",
  [TMINUS] = " - ",
  [TUPLUS] = "+",
  [TUMINUS] = "-",
  [TTIMES] = " * ",
  [TDIVIDE] = " / ",
  [TMODULO] = " % ",
  [TBWAND] = " & ",
  [TBWOR] = " | ",
  [TBWXOR] = " ^ ",
  [TRSHIFT] = " >> ",
  [TLSHIFT] = " << ",
  [TPLUS_ASSIGN] = " += ",
  [TMINUS_ASSIGN] = " -= ",
  [TTIMES_ASSIGN] = " *= ",
  [TDIVIDE_ASSIGN] = " /= ",
  [TMODULO_ASSIGN] = " %= ",
  [TBWAND_ASSIGN] = " &= ",
  [TBWOR_ASSIGN] = " |= ",
  [TBWXOR_ASSIGN] = " ^= ",
  [TRSHIFT_ASSIGN] = " >>= ",
  [TLSHIFT_ASSIGN] = " <<= ",
  [TUBWNOT] = " ~",
  [TARROW] = " -> ",
  [TCOLON] = ":",
  [TCOMMA] = ", ",
  [TSEMICOLON] = "; ",
  [TDOT] = ".",
  [TBANG] = "!",
  [TSHARP] = "#",
  [TREFDOT] = "@",
  [TREFBANG] = "@!",
  [TREFSHARP] = "@#",
  [TNULREFDOT] = "?@",
  [TNULREFBANG] = "?@!",
  [TNULREFSHARP] = "?@#",
  [TDOTDOTDOT] = "...",
  [TSLICEBRAKETS] = "[]",
  [TLINIT] = "{{",
  [TRINIT] = "}}",
  [TLPAR] = "(",
  [TRPAR] = ")",
};

static char *escape_string(const char *s) {
  char *r = calloc(2 * strlen(s) + 1, sizeof(char));
  char delim = s[0];
  if (s[1] == delim) {
    r[0] = '\0';
    return r;
  }

  char *v = r;
  for (const char *p = s + 1; p[1] != '\0'; ++p, ++v) {
    if (p[0] == delim) {
      v[0] = '\0';
    } else if (p[0] == '"') {
      v[0] = '\\';
      v[1] = '"';
      v += 1;
    } else if (p[0] == '\\') {
      if (p[1] == delim) {
        if (delim == '"') {
          v[0] = '\\';
          v[1] = '"';
          v += 1;
        } else {
          v[0] = delim;
        }
        p += 1;
      } else {
        v[0] = p[0];
      }
    } else {
      v[0] = p[0];
    }
  }

  return r;
}

static void print_token(FILE *out, enum token_type t) {
  fprintf(out, "%s", c_token_strings[t]);
}

static void print_expr(FILE *out, bool header, const struct module *mod, const struct node *node, uint32_t parent_op);
static void print_block(FILE *out, bool header, const struct module *mod, const struct node *node);
static void print_typ(FILE *out, const struct module *mod, const struct typ *typ);
static void print_typeconstraint(FILE *out, bool header, const struct module *mod, const struct node *node);
static void print_deftype_name(FILE *out, const struct module *mod, const struct node *node);
static void print_deffun_name(FILE *out, const struct module *mod, const struct node *node);
static void print_deffield_name(FILE *out, const struct module *mod, const struct node *node);

static void print_pattern(FILE *out, bool header, const struct module *mod, const struct node *node) {
  print_expr(out, header, mod, node, T__STATEMENT);
}

static void print_bin_sym(FILE *out, bool header, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.BIN.operator;

  print_expr(out, header, mod, node->subs[0], op);
  print_token(out, op);
  print_expr(out, header, mod, node->subs[1], op);
}

static void print_bin_acc(FILE *out, bool header, const struct module *mod, const struct node *node, uint32_t parent_op) {
  assert(node->subs[1]->which == IDENT);
  const uint32_t op = node->as.BIN.operator;
  const struct node *left = node->subs[0];
  const struct node *right = node->subs[1];

  const struct typ *tleft = left->typ;

  struct node *n = NULL;
  error e = scope_lookup(&n, mod, tleft->definition->scope, right);
  assert(!e);

  switch (n->which) {
  case DEFCHOICE:
    if (tleft->definition->as.DEFTYPE.kind == DEFTYPE_ENUM) {
      print_typ(out, mod, tleft);
      fprintf(out, "_");
      print_deffield_name(out, mod, n);
    } else {
      assert(FALSE && "FIXME Unsupported");
      print_deftype_name(out, mod, tleft->definition);
      fprintf(out, "_");
      print_deffield_name(out, mod, n);
      fprintf(out, "_%s", idents_value(mod->gctx, ID_WHICH));
    }
    break;
  case DEFFUN:
  case DEFMETHOD:
    print_deffun_name(out, mod, n);
    break;
  case DEFTYPE:
    print_deftype_name(out, mod, n);
    break;
  default: {
    const char *deref = ".";
    if (typ_is_reference_instance(mod, tleft)) {
      deref = "->";
    }
    print_expr(out, header, mod, left, op);
    fprintf(out, "%s%s", deref, idents_value(mod->gctx, right->as.IDENT.name));
    break;
  }}
}

static void print_bin(FILE *out, bool header, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.BIN.operator;
  const uint32_t prec = OP_PREC(op);
  const uint32_t parent_prec = OP_PREC(parent_op);

  if (prec > parent_prec) {
    fprintf(out, "(");
  }

  switch (OP_KIND(op)) {
  case OP_BIN:
  case OP_BIN_SYM:
  case OP_BIN_SYM_BOOL:
  case OP_BIN_SYM_NUM:
  case OP_BIN_NUM_RHS_U16:
    print_bin_sym(out, header, mod, node, parent_op);
    break;
  case OP_BIN_ACC:
    print_bin_acc(out, header, mod, node, parent_op);
    break;
  case OP_BIN_RHS_TYPE:
    print_typeconstraint(out, header, mod, node);
    break;
  }

  if (prec > parent_prec) {
    fprintf(out, ")");
  }
}

static void print_un(FILE *out, bool header, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.UN.operator;
  const uint32_t prec = OP_PREC(op);
  const uint32_t parent_prec = OP_PREC(parent_op);

  if (prec > parent_prec) {
    fprintf(out, "(");
  }

  switch (OP_KIND(op)) {
  case OP_UN_REFOF:
    if (node->is_type) {
      print_typ(out, mod, node->typ);
    } else {
      fprintf(out, "&");
      print_expr(out, header, mod, node->subs[0], op);
    }
    break;
  case OP_UN_BOOL:
  case OP_UN_NUM:
    print_token(out, op);
    print_expr(out, header, mod, node->subs[0], op);
    break;
  case OP_UN_DYN:
    break;
  }

  if (prec > parent_prec) {
    fprintf(out, ")");
  }
}

static void print_tuple(FILE *out, bool header, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t prec = OP_PREC(TCOMMA);
  const uint32_t parent_prec = OP_PREC(parent_op);

  if (prec >= parent_prec) {
    fprintf(out, "(");
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    if (n > 0) {
      fprintf(out, ", ");
    }
    print_expr(out, header, mod, node->subs[n], TCOMMA);
  }

  if (prec >= parent_prec) {
    fprintf(out, ")");
  }
}

static char *replace_dots(char *n) {
  char *p = n;
  while (p[0] != '\0') {
    if (p[0] == '.') {
      p[0] = '_';
    }
    p += 1;
  }
  return n;
}

static void print_call(FILE *out, bool header, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const struct typ *ftyp = node->subs[0]->typ;

  fprintf(out, "%s(", replace_dots(typ_name(mod, ftyp)));

  for (size_t n = 0; n < ftyp->fun_arity; ++n) {
    if (n > 0) {
      fprintf(out, ", ");
    }
    print_expr(out, header, mod, node->subs[1 + n], T__CALL);
  }
  fprintf(out, ")");
}

static void print_init(FILE *out, bool header, const struct module *mod, const struct node *node) {
  print_expr(out, header, mod, node->subs[0], T__STATEMENT);
  fprintf(out, "{{ ");

  for (size_t n = 1; n < node->subs_count; n += 2) {
    print_expr(out, header, mod, node->subs[n], T__STATEMENT);
    fprintf(out, "=");
    print_expr(out, header, mod, node->subs[n + 1], T__CALL);
    fprintf(out, " ");
  }

  fprintf(out, "}}");
}

static error is_local_name(bool *is_local, const struct module *mod, const struct node *node) {
  if (node->which == DEFNAME) {
    *is_local = TRUE;
    return 0;
  }

  struct node *def = NULL;
  error e = scope_lookup(&def, mod, node->scope, node);
  EXCEPT(e);
  enum node_which parent_which = def->scope->parent->node->which;
  *is_local = parent_which != MODULE && parent_which != DEFTYPE && parent_which != DEFINTF;
  return 0;
}

static void print_deftype_name(FILE *out, const struct module *mod, const struct node *node) {
  char *n = replace_dots(scope_name(mod, node->scope));
  fprintf(out, "%s", n);
  free(n);
}

static void print_deffun_name(FILE *out, const struct module *mod, const struct node *node) {
  const ident id = node_ident(node);
  if (id == ID_MAIN) {
    fprintf(out, "main");
  } else {
    char *n = replace_dots(scope_name(mod, node->scope));
    fprintf(out, "%s", n);
    free(n);
  }
}

static void print_deffield_name(FILE *out, const struct module *mod, const struct node *node) {
  const ident id = node_ident(node);
  fprintf(out, "%s", idents_value(mod->gctx, id));
}

static void print_ident(FILE *out, const struct module *mod, const struct node *node) {
  bool is_local = FALSE;
  error e = is_local_name(&is_local, mod, node);
  assert(!e);

  const ident id = node_ident(node);
  if (id == ID_MAIN) {
    fprintf(out, "main");
  } else if (is_local) {
    fprintf(out, "%s", idents_value(mod->gctx, id));
  } else {
    fprintf(out, "%s", scope_name(mod, node->scope));
  }
}

static void print_expr(FILE *out, bool header, const struct module *mod, const struct node *node, uint32_t parent_op) {
  if (node->is_type) {
    print_typ(out, mod, node->typ);
    return;
  }

  switch (node->which) {
  case NUL:
    fprintf(out, "NULL");
    break;
  case IDENT:
    print_ident(out, mod, node);
    break;
  case NUMBER:
    fprintf(out, "(");
    print_typ(out, mod, node->typ);
    fprintf(out, ")");
    fprintf(out, "%s", node->as.NUMBER.value);
    break;
  case STRING:
    {
      char *s = escape_string(node->as.STRING.value);
      fprintf(out, "\"%s\"", s);
      free(s);
      break;
    }
  case BIN:
    print_bin(out, header, mod, node, parent_op);
    break;
  case UN:
    print_un(out, header, mod, node, parent_op);
    break;
  case TYPECONSTRAINT:
    print_typeconstraint(out, header, mod, node);
    break;
  case CALL:
    print_call(out, header, mod, node, parent_op);
    break;
  case TUPLE:
    print_tuple(out, header, mod, node, parent_op);
    break;
  case INIT:
    print_init(out, header, mod, node);
    break;
  default:
    fprintf(stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_for(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "for ");
  print_pattern(out, header, mod, node->subs[0]);
  fprintf(out, " in ");
  print_expr(out, header, mod, node->subs[1], T__STATEMENT);
  print_block(out, header, mod, node->subs[2]);
}

static void print_while(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "while (");
  print_expr(out, header, mod, node->subs[0], T__STATEMENT);
  fprintf(out, ")");
  print_block(out, header, mod, node->subs[1]);
}

static void print_if(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "if (");
  print_expr(out, header, mod, node->subs[0], T__STATEMENT);
  fprintf(out, ")");
  print_block(out, header, mod, node->subs[1]);

  size_t p = 2;
  size_t br_count = node->subs_count - 2;
  while (br_count >= 2) {
    fprintf(out, "\n");
    fprintf(out, "else if (");
    print_expr(out, header, mod, node->subs[p], T__STATEMENT);
    fprintf(out, ") ");
    print_block(out, header, mod, node->subs[p+1]);
    p += 2;
    br_count -= 2;
  }

  if (br_count == 1) {
    fprintf(out, "\n");
    fprintf(out, "else ");
    print_block(out, header, mod, node->subs[p]);
  }
}

static void print_match(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "switch (");
  print_expr(out, header, mod, node->subs[0], T__STATEMENT);
  fprintf(out, ") {");

  for (size_t n = 1; n < node->subs_count; n += 2) {
    fprintf(out, "case ");
    print_expr(out, header, mod, node->subs[n], T__STATEMENT);
    fprintf(out, ":");
    print_block(out, header, mod, node->subs[n + 1]);
  }
}

static void print_try(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "try");
  print_block(out, header, mod, node->subs[0]);
  fprintf(out, "\n");
  fprintf(out, "catch ");
  print_expr(out, header, mod, node->subs[1], T__STATEMENT);
  print_block(out, header, mod, node->subs[2]);
}

static void print_pre(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "pre");
  print_block(out, header, mod, node->subs[0]);
}

static void print_post(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "post");
  print_block(out, header, mod, node->subs[0]);
}

static void print_invariant(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "invariant");
  print_block(out, header, mod, node->subs[0]);
}

static void print_example(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "example");
  print_block(out, header, mod, node->subs[0]);
}

static void print_toplevel(FILE *out, bool header, const struct node *node) {
  if (header && !node_is_export(node)) {
    return;
  }

  const struct toplevel *toplevel = node_toplevel_const(node);
  if (toplevel->is_extern) {
    fprintf(out, "extern ");
  }
  if (toplevel->is_inline) {
    fprintf(out, "inline ");
  }
}

static void print_typ(FILE *out, const struct module *mod, const struct typ *typ) {
  switch (typ->which) {
  case TYPE_DEF:
    if (typ->gen_arity == 0) {
      fprintf(out, "%s", replace_dots(typ_name(mod, typ)));
    } else {
      fprintf(out, "_ngen_%s", replace_dots(typ_name(mod, typ)));
      for (size_t n = 1; n < typ->gen_arity + 1; ++n) {
        fprintf(out, "__%s", replace_dots(typ_name(mod, typ->gen_args[n])));
      }
    }
    break;
  case TYPE_TUPLE:
    fprintf(out, "_ntup_%s", replace_dots(typ_name(mod, typ)));
    break;
  case TYPE_FUNCTION:
    fprintf(out, "_nfun_%s", replace_dots(typ_name(mod, typ)));
    break;
  default:
    break;
  }
}

static void print_defname(FILE *out, bool header, const struct module *mod, const struct node *node,
                          const struct node *pattern) {
  assert(node->which == DEFNAME);
  if (node->is_type) {
    const ident id = node_ident(node->as.DEFNAME.pattern);
    if (id != ID_THIS) {
      fprintf(out, "typedef ");
      print_typ(out, mod, node->typ);
      fprintf(out, " ");
      print_pattern(out, header, mod, node->as.DEFNAME.expr);
    }
  } else {
    fprintf(stderr, "%s\n", typ_name(mod, node->typ));
    print_typ(out, mod, node->typ);
    fprintf(out, " ");
    print_pattern(out, header, mod, node->as.DEFNAME.pattern);

    if (!header || node_is_inline(pattern)) {
      if (node->as.DEFNAME.expr != NULL) {
        fprintf(out, " = ");
        print_expr(out, header, mod, node->as.DEFNAME.expr, T__STATEMENT);
      }
    }
  }
}

static void print_defpattern(FILE *out, bool header, const struct module *mod, const struct node *node) {
  assert(node->which == DEFPATTERN);
  if (header && !node_is_export(node)) {
    return;
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    if (node->subs[n]->which != DEFNAME) {
      continue;
    }

    print_defname(out, header, mod, node->subs[n], node);
  }
}

static void print_let(FILE *out, bool header, const struct module *mod, const struct node *node) {
  print_toplevel(out, header, node);
  print_defpattern(out, header, mod, node->subs[0]);

  if (node->subs_count > 1) {
    assert(!node_is_inline(node));
    print_block(out, header, mod, node->subs[1]);
  }
}

static void print_statement(FILE *out, bool header, const struct module *mod, const struct node *node) {
  switch (node->which) {
  case RETURN:
    fprintf(out, "return");
    if (node->subs_count > 0) {
      fprintf(out, " ");
      print_expr(out, header, mod, node->subs[0], T__STATEMENT);
    }
    break;
  case FOR:
    print_for(out, header, mod, node);
    break;
  case WHILE:
    print_while(out, header, mod, node);
    break;
  case BREAK:
    fprintf(out, "break");
    break;
  case CONTINUE:
    fprintf(out, "continue");
    break;
  case PASS:
    fprintf(out, ";");
    break;
  case IF:
    print_if(out, header, mod, node);
    break;
  case MATCH:
    print_match(out, header, mod, node);
    break;
  case TRY:
    print_try(out, header, mod, node);
    break;
  case PRE:
    print_pre(out, header, mod, node);
    break;
  case POST:
    print_post(out, header, mod, node);
    break;
  case INVARIANT:
    print_invariant(out, header, mod, node);
    break;
  case EXAMPLE:
    print_example(out, header, mod, node);
    break;
  case LET:
    print_let(out, header, mod, node);
    break;
  case IDENT:
  case BIN:
  case UN:
  case CALL:
    print_expr(out, header, mod, node, T__STATEMENT);
    break;
  default:
    fprintf(stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_block(FILE *out, bool header, const struct module *mod, const struct node *node) {
  assert(node->which == BLOCK);
  fprintf(out, " {\n");
  for (size_t n = 0; n < node->subs_count; ++n) {
    const struct node *statement = node->subs[n];
    print_statement(out, header, mod, statement);
    fprintf(out, ";\n");
  }
  fprintf(out, "}");
}

static void print_typeexpr(FILE *out, bool header, const struct module *mod, const struct node *node) {
  print_expr(out, header, mod, node, T__STATEMENT);
}

static void print_typeconstraint(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "(");
  print_typeexpr(out, header, mod, node->subs[1]);
  fprintf(out, ")");
  print_expr(out, header, mod, node->subs[0], T__STATEMENT);
}

static void print_defarg(FILE *out, bool header, const struct module *mod, const struct node *node) {
  print_typeexpr(out, header, mod, node->subs[1]);
  fprintf(out, " ");
  print_expr(out, header, mod, node->subs[0], T__STATEMENT);
}

static void print_retval(FILE *out, bool header, const struct module *mod, const struct node *node) {
  if (node->which == DEFARG) {
    print_expr(out, header, mod, node->subs[1], T__STATEMENT);
  } else {
    print_expr(out, header, mod, node, T__STATEMENT);
  }
}

static void print_fun_prototype(FILE *out, bool header, const struct module *mod,
                                const struct node *node) {
  const size_t arg_count = node_fun_explicit_args_count(node);
  const struct node *retval = node_fun_retval(node);

  print_toplevel(out, header, node);

  print_retval(out, header, mod, retval);
  fprintf(out, " ");
  print_deffun_name(out, mod, node);
  fprintf(out, "(");

  if (arg_count == 0) {
    fprintf(out, "void");
  }

  for (size_t n = 0; n < arg_count; ++n) {
    if (n > 0) {
      fprintf(out, ", ");
    }
    const struct node *arg = node->subs[1 + n];
    print_defarg(out, header, mod, arg);
  }
  fprintf(out, ")");
}

static bool prototype_only(bool header, const struct node *node) {
  return (header && !(node_is_export(node) && node_is_inline(node))) || node_is_prototype(node);
}

static void print_deffun_builtingen(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, " {\n");
  switch (node_toplevel_const(node)->builtingen) {
  case BG_ZERO_CTOR_CTOR:
    fprintf(out, "memset(self, 0, sizeof(*self));");
    break;
  case BG_DEFAULT_CTOR_CTOR:
    fprintf(out, "memset(self, 0, sizeof(*self));");
    break;
  case BG_DEFAULT_CTOR_MK:
  //     ... must handle return through ref
  //       ... this is a C gen question, must be handled here, N doesn't care
  //       ... we should have facilities to handle this nicely,
  //     ... generating prototypes for us,
  //     ... generating return types, initializing return values, setting them?
  //       ... can we do this transparently so that the generating code can be agnostic
  //       ... to this stuff?
    fprintf(out, "_ctor(&r);");
    break;
  case BG_DEFAULT_CTOR_NEW:
    fprintf(out, "memset(self, 0, sizeof(*self));");
    break;
  case BG_CTOR_WITH_CTOR_WITH:
    fprintf(out, "memset(self, 0, sizeof(*self));");
    break;
  case BG_CTOR_WITH_MK_WITH:
    fprintf(out, "memset(self, 0, sizeof(*self));");
    break;
  case BG_CTOR_WITH_NEW_WITH:
    fprintf(out, "memset(self, 0, sizeof(*self));");
    break;
  case BG_AUTO_MK:
    fprintf(out, "memset(self, 0, sizeof(*self));");
    break;
  case BG_AUTO_NEW:
    fprintf(out, "memset(self, 0, sizeof(*self));");
    break;
  case BG_ENUM_EQ:
    fprintf(out, "return *self == *other;\n");
    break;
  case BG_ENUM_NE:
    fprintf(out, "return *self != *other;\n");
    break;
  case BG_ENUM_MATCH:
    fprintf(out, "return *self == *other;\n");
    break;
  case BG_SUM_MATCH:
    fprintf(out, "memset(self, 0, sizeof(*self));");
    break;
  case BG_SUM_DISPATCH:
    fprintf(out, "memset(self, 0, sizeof(*self));");
    break;
  default:
    assert(FALSE);
    break;
  }
  fprintf(out, "}\n");
}

static void print_deffun(FILE *out, bool header, const struct module *mod, const struct node *node) {
  if (header && !node_is_export(node)) {
    return;
  }

  if (prototype_only(header, node)) {
    print_fun_prototype(out, header, mod, node);
    fprintf(out, ";\n");
  } else if (node_toplevel_const(node)->builtingen != BG__NOT) {
    print_fun_prototype(out, header, mod, node);
    print_deffun_builtingen(out, header, mod, node);
  } else {
    print_fun_prototype(out, header, mod, node);

    const struct node *retval = node_fun_retval(node);
    const bool named_retval = retval->which == DEFARG;
    const bool retval_bycopy = typ_isa(mod, retval->typ, typ_lookup_builtin(mod, TBI_RETURN_BY_COPY));

    if (named_retval && retval_bycopy) {
      fprintf(out, "{\n__attribute__((__unused__))");
      print_defarg(out, header, mod, retval);
      fprintf(out, ";\n");
    }

    const size_t arg_count = node_fun_explicit_args_count(node);
    const struct node *block = node->subs[1 + arg_count + 1];
    print_block(out, header, mod, block);

    fprintf(out, "\n");

    if (named_retval) {
      fprintf(out, "}\n");
    }
  }
}

static void print_deffield(FILE *out, bool header, const struct module *mod, const struct node *node) {
  print_typeexpr(out, header, mod, node->subs[1]);
  fprintf(out, " ");
  print_deffield_name(out, mod, node);
}

static void print_delegate(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "delegate ");
  print_expr(out, header, mod, node->subs[0], T__CALL);

  for (size_t n = 1; n < node->subs_count; ++n) {
    fprintf(out, " ");
    print_expr(out, header, mod, node->subs[n], T__CALL);
  }
}

static void print_deftype_statement(FILE *out, bool header, const struct module *mod, const struct node *node,
                                    bool do_static) {
  if (do_static) {
    switch (node->which) {
    case LET:
      print_let(out, header, mod, node);
      break;
    case DELEGATE:
      print_delegate(out, header, mod, node);
      break;
    case INVARIANT:
      print_invariant(out, header, mod, node);
      break;
    case EXAMPLE:
      print_example(out, header, mod, node);
      break;
    default:
      break;
    }
  } else {
    switch (node->which) {
    case DEFFIELD:
      print_deffield(out, header, mod, node);
      break;
    default:
      break;
    }
  }
}

static void print_deftype_block(FILE *out, bool header, const struct module *mod,
                                const struct node *node, size_t first, bool do_static) {
  if (!do_static) {
    fprintf(out, " {\n");
  }
  for (size_t n = first; n < node->subs_count; ++n) {
    ssize_t prev_pos = ftell(out);

    const struct node *statement = node->subs[n];
    print_deftype_statement(out, header, mod, statement, do_static);

    // Hack to prevent isolated ';' when statement does not print anything.
    if (ftell(out) != prev_pos) {
      fprintf(out, ";\n");
    }
  }

  if (!do_static && (node->as.DEFTYPE.kind == DEFTYPE_SUM)) {
    print_deftype_name(out, mod, node);
    fprintf(out, "_%s %s;\n",
            idents_value(mod->gctx, ID_WHICH_TYPE),
            idents_value(mod->gctx, ID_WHICH));

    print_deftype_name(out, mod, node);
    fprintf(out, "_%s %s;\n",
            idents_value(mod->gctx, ID_AS_TYPE),
            idents_value(mod->gctx, ID_AS));
  }

  if (!do_static) {
    fprintf(out, "}\n");
  }
}

static void print_deftype_typedefs(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "typedef const ");
  print_deftype_name(out, mod, node);
  fprintf(out, "* _ngen_nlang_builtins_ref__");
  print_deftype_name(out, mod, node);
  fprintf(out, ";\n");

  fprintf(out, "typedef _ngen_nlang_builtins_ref__");
  print_deftype_name(out, mod, node);
  fprintf(out, " _ngen_nlang_builtins_nref__");
  print_deftype_name(out, mod, node);
  fprintf(out, ";\n");

  fprintf(out, "typedef ");
  print_deftype_name(out, mod, node);
  fprintf(out, "* _ngen_nlang_builtins_mref__");
  print_deftype_name(out, mod, node);
  fprintf(out, ";\n");

  fprintf(out, "typedef _ngen_nlang_builtins_mref__");
  print_deftype_name(out, mod, node);
  fprintf(out, " _ngen_nlang_builtins_mmref__");
  print_deftype_name(out, mod, node);
  fprintf(out, ";\n");

  fprintf(out, "typedef _ngen_nlang_builtins_mref__");
  print_deftype_name(out, mod, node);
  fprintf(out, " _ngen_nlang_builtins_nmref__");
  print_deftype_name(out, mod, node);
  fprintf(out, ";\n");

  fprintf(out, "typedef _ngen_nlang_builtins_mref__");
  print_deftype_name(out, mod, node);
  fprintf(out, " _ngen_nlang_builtins_nmmref__");
  print_deftype_name(out, mod, node);
  fprintf(out, ";\n");
}

static void print_deftype_choices(FILE *out, bool header, const struct module *mod, const struct node *node) {
  if (header && !(node_is_export(node) && node_is_inline(node))) {
    return;
  }

  const struct typ *choice_typ = NULL;
  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *s = node->subs[n];
    if (s->which == DEFCHOICE) {
      choice_typ = s->subs[1]->typ;
      break;
    }
  }
  assert(choice_typ != NULL);

  fprintf(out, "typedef ");
  print_typ(out, mod, choice_typ);
  fprintf(out, " ");
  print_deftype_name(out, mod, node);
  fprintf(out, "_%s;\n", idents_value(mod->gctx, ID_WHICH_TYPE));

  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *s = node->subs[n];
    if (s->which != DEFCHOICE) {
      continue;
    }

    fprintf(out, "static const ");
    print_deftype_name(out, mod, node);
    fprintf(out, "_%s", idents_value(mod->gctx, ID_WHICH_TYPE));
    fprintf(out, " ");
    print_deftype_name(out, mod, node);
    fprintf(out, "_");
    print_deffield_name(out, mod, s);
    fprintf(out, "_%s = ", idents_value(mod->gctx, ID_WHICH));
    print_expr(out, header, mod, s->subs[1], T__STATEMENT);
    fprintf(out, ";\n");
  }

  if (node->as.DEFTYPE.kind != DEFTYPE_SUM) {
    return;
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *s = node->subs[n];
    if (s->which != DEFCHOICE) {
      continue;
    }

    fprintf(out, "typedef ");
    if (s->subs_count > 2) {
      print_typ(out, mod, s->subs[2]->typ);
    } else {
      print_typ(out, mod, typ_lookup_builtin(mod, TBI_U8));
    }
    fprintf(out, " ");
    print_deftype_name(out, mod, node);
    fprintf(out, "_");
    print_deffield_name(out, mod, s);
    fprintf(out, ";\n");
  }

  fprintf(out, "union ");
  print_deftype_name(out, mod, node);
  fprintf(out, "_%s {\n", idents_value(mod->gctx, ID_AS_TYPE));
  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *s = node->subs[n];
    if (s->which != DEFCHOICE) {
      continue;
    }

    print_deftype_name(out, mod, node);
    fprintf(out, "_");
    print_deffield_name(out, mod, s);
    fprintf(out, " ");
    print_deffield_name(out, mod, s);
    fprintf(out, ";\n");
  }
  fprintf(out, "};\n");

  fprintf(out, "typedef union ");
  print_deftype_name(out, mod, node);
  fprintf(out, "_%s ", idents_value(mod->gctx, ID_AS_TYPE));
  print_deftype_name(out, mod, node);
  fprintf(out, "_%s;\n", idents_value(mod->gctx, ID_AS_TYPE));

  print_deftype_typedefs(out, header, mod, node);
}

static void print_deftype_enum(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "typedef ");
  print_typ(out, mod, node->as.DEFTYPE.choice_typ);
  fprintf(out, " ");
  print_deftype_name(out, mod, node);
  fprintf(out, ";\n");

  print_deftype_block(out, header, mod, node, 2, TRUE);

  if (!prototype_only(header, node)) {
    for (size_t n = 0; n < node->subs_count; ++n) {
      struct node *s = node->subs[n];
      if (s->which != DEFCHOICE) {
        continue;
      }

      print_deftype_name(out, mod, node);
      fprintf(out, " ");
      print_deftype_name(out, mod, node);
      fprintf(out, "_");
      print_deffield_name(out, mod, s);
      fprintf(out, " = ");
      print_expr(out, header, mod, s->subs[1], T__CALL);
      fprintf(out, ";\n");
    }
  }

  print_deftype_typedefs(out, header, mod, node);
}
static void print_deftype(FILE *out, bool header, const struct module *mod, const struct node *node) {
  if (header && !node_is_export(node)) {
    return;
  }

  if (typ_is_builtin(mod, node->typ)) {
    return;
  }

  if (node->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    print_deftype_enum(out, header, mod, node);
    return;
  }

  fprintf(out, "struct ");
  print_deftype_name(out, mod, node);
  fprintf(out, ";\n");
  fprintf(out, "typedef struct ");
  print_deftype_name(out, mod, node);
  fprintf(out, " ");
  print_deftype_name(out, mod, node);
  fprintf(out, ";\n");

  if (node->as.DEFTYPE.kind == DEFTYPE_SUM) {
    print_deftype_choices(out, header, mod, node);
  }

  print_deftype_block(out, header, mod, node, 2, TRUE);

  if (!prototype_only(header, node)) {
    fprintf(out, "struct ");
    print_deftype_name(out, mod, node);

    print_deftype_block(out, header, mod, node, 2, FALSE);

    fprintf(out, ";\n");
  }

  print_deftype_typedefs(out, header, mod, node);
}

static void print_defintf(FILE *out, bool header, const struct module *mod, const struct node *node) {
}

static void print_import(FILE *out, bool header, const struct module *mod, const struct node *node) {
  struct node *target = NULL;
  error e = scope_lookup(&target, mod, mod->gctx->modules_root.scope, node->subs[0]);
  assert(!e);
  if (target->which == MODULE) {
    fprintf(out, "#include \"%s.h.out\"", target->as.MODULE.mod->filename);
  }
}

static void print_module(FILE *out, bool header, const struct module *mod) {
  if (header) {
    const char *guard = replace_dots(scope_name(mod, mod->root->scope));
    fprintf(out, "#ifndef %s\n#define %s\n\n", guard, guard);
  }

  fprintf(out, "#include <lib/nlang/runtime.h>\n");

  const struct node *top = mod->body;

  for (size_t n = 0; n < top->subs_count; ++n) {
    const struct node *node = top->subs[n];
    switch (node->which) {
    case DEFFUN:
      print_deffun(out, header, mod, node);
      break;
    case DEFTYPE:
      print_deftype(out, header, mod, node);
      break;
    case DEFMETHOD:
      print_deffun(out, header, mod, node);
      break;
    case DEFINTF:
      print_defintf(out, header, mod, node);
      break;
    case LET:
      print_let(out, header, mod, node);
      break;
    case IMPORT:
      print_import(out, header, mod, node);
      break;
    default:
      fprintf(stderr, "Unsupported node: %d\n", node->which);
      assert(FALSE);
    }

    if (n < top->subs_count - 1) {
      fprintf(out, "\n");
    }
  }

  if (header) {
    fprintf(out, "\n#endif\n");
  }
}

error printer_c(int fd, const struct module *mod) {
  FILE *out = fdopen(fd, "w");
  if (out == NULL) {
    EXCEPTF(errno, "Invalid output file descriptor '%d'", fd);
  }

  print_module(out, FALSE, mod);
  fflush(out);

  return 0;
}

error printer_h(int fd, const struct module *mod) {
  FILE *out = fdopen(fd, "w");
  if (out == NULL) {
    EXCEPTF(errno, "Invalid output file descriptor '%d'", fd);
  }

  print_module(out, TRUE, mod);
  fflush(out);

  return 0;
}
