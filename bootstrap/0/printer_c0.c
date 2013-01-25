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
  char *r = malloc(2 * strlen(s) + 1);
  char delim = s[0];
  if (s[1] == delim) {
    r[0] = '\0';
    return r;
  }

  char *v = r;
  for (const char *p = s + 1; p[1] != '\0'; ++p, ++v) {
    switch (p[0]) {
    case '"':
      v[0] = '\\';
      v[1] = '"';
      v += 1;
      break;
    case '\\':
      if (p[1] == delim) {
        if (delim == '"') {
          v[0] = '\\';
          v[1] = '"';
          v += 1;
        } else {
          v[0] = '\'';
        }
        p += 1;
      } else {
        v[0] = p[0];
      }
    }
  }

  return r;
}

static void print_token(FILE *out, enum token_type t) {
  fprintf(out, "%s", c_token_strings[t]);
}

static void print_expr(FILE *out, bool header, const struct module *mod, const struct node *node, uint32_t parent_op);
static void print_block(FILE *out, bool header, const struct module *mod, const struct node *node);
static void print_typ(FILE *out, const struct module *mod, struct typ *typ);

static void print_pattern(FILE *out, bool header, const struct module *mod, const struct node *node) {
  print_expr(out, header, mod, node, T__NONE);
}

static void print_bin(FILE *out, bool header, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.BIN.operator;
  const uint32_t prec = OP_PREC(op);
  const uint32_t parent_prec = OP_PREC(parent_op);

  if (prec > parent_prec) {
    fprintf(out, "(");
  }

  print_expr(out, header, mod, node->subs[0], op);
  print_token(out, op);
  print_expr(out, header, mod, node->subs[1], op);

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

  print_token(out, op);
  print_expr(out, header, mod, node->subs[0], op);

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

static void print_call(FILE *out, bool header, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t prec = OP_PREC(T__CALL);
  const uint32_t parent_prec = OP_PREC(parent_op);

  if (prec >= parent_prec) {
    fprintf(out, "(");
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    if (n > 0) {
      fprintf(out, " ");
    }
    print_expr(out, header, mod, node->subs[n], T__CALL);
  }

  if (prec >= parent_prec) {
    fprintf(out, ")");
  }
}

static void print_init(FILE *out, bool header, const struct module *mod, const struct node *node) {
  print_expr(out, header, mod, node->subs[0], T__NONE);
  fprintf(out, "{{ ");

  for (size_t n = 1; n < node->subs_count; n += 2) {
    print_expr(out, header, mod, node->subs[n], T__NONE);
    fprintf(out, "=");
    print_expr(out, header, mod, node->subs[n + 1], T__CALL);
    fprintf(out, " ");
  }

  fprintf(out, "}}");
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
    fprintf(out, "%s", idents_value(mod->gctx, node->as.IDENT.name));
    break;
  case NUMBER:
    fprintf(out, "%s", node->as.NUMBER.value);
    break;
  case STRING:
    {
      char *s = escape_string(node->as.STRING.value);
      fprintf(out, "\"%s\"", escape_string(s));
      free(s);
      break;
    }
  case BIN:
    print_bin(out, header, mod, node, parent_op);
    break;
  case UN:
    print_un(out, header, mod, node, parent_op);
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
  print_expr(out, header, mod, node->subs[1], T__NONE);
  print_block(out, header, mod, node->subs[2]);
}

static void print_while(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "while (");
  print_expr(out, header, mod, node->subs[0], T__NONE);
  fprintf(out, ")");
  print_block(out, header, mod, node->subs[1]);
}

static void print_if(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "if ");
  print_expr(out, header, mod, node->subs[0], T__NONE);
  print_block(out, header, mod, node->subs[1]);

  size_t p = 2;
  size_t br_count = node->subs_count - 2;
  while (br_count >= 2) {
    fprintf(out, "\n");
    fprintf(out, "elif ");
    print_expr(out, header, mod, node->subs[p], T__NONE);
    print_block(out, header, mod, node->subs[p+1]);
    p += 2;
    br_count -= 2;
  }

  if (br_count == 1) {
    fprintf(out, "\n");
    fprintf(out, "else");
    print_block(out, header, mod, node->subs[p]);
  }
}

static void print_match(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "match ");
  print_expr(out, header, mod, node->subs[0], T__NONE);

  for (size_t n = 1; n < node->subs_count; n += 2) {
    fprintf(out, "\n");
    fprintf(out, "| ");
    print_expr(out, header, mod, node->subs[n], T__NONE);
    print_block(out, header, mod, node->subs[n + 1]);
  }
}

static void print_try(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "try");
  print_block(out, header, mod, node->subs[0]);
  fprintf(out, "\n");
  fprintf(out, "catch ");
  print_expr(out, header, mod, node->subs[1], T__NONE);
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

static void print_toplevel(FILE *out, const struct toplevel *toplevel) {
  if (toplevel->is_extern) {
    fprintf(out, "extern ");
  }
  if (toplevel->is_inline) {
    fprintf(out, "inline ");
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

static void print_typ(FILE *out, const struct module *mod, struct typ *typ) {
  switch (typ->which) {
  case TYPE_DEF:
    fprintf(out, "%s", replace_dots(typ_name(mod, typ)));
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

static void print_defname(FILE *out, bool header, const struct module *mod, const struct node *node) {
  print_typ(out, mod, node->typ);
  fprintf(out, " ");
  print_pattern(out, header, mod, node->subs[0]);
  fprintf(out, " = ");
  print_expr(out, header, mod, node->subs[1], T__NONE);

  if (node->subs_count > 2) {
    print_block(out, header, mod, node->subs[2]);
  }
}

static void print_let(FILE *out, bool header, const struct module *mod, const struct node *node) {
  print_toplevel(out, &node->as.LET.toplevel);
  print_defname(out, header, mod, node->subs[0]);
}

static void print_statement(FILE *out, bool header, const struct module *mod, const struct node *node) {
  switch (node->which) {
  case RETURN:
    fprintf(out, "return");
    if (node->subs_count > 0) {
      fprintf(out, " ");
      print_expr(out, header, mod, node->subs[0], T__NONE);
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
    fprintf(out, "pass");
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
    print_expr(out, header, mod, node, T__NONE);
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
  print_expr(out, header, mod, node, T__NONE);
}

static void print_typeconstraint(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "(");
  print_typeexpr(out, header, mod, node->subs[1]);
  fprintf(out, ")");
  print_expr(out, header, mod, node->subs[0], T__NONE);
}

static void print_fun_prototype(FILE *out, bool header, const struct module *mod,
                                const struct node *node) {
  const size_t arg_count = node->subs_count - (node->as.DEFFUN.toplevel.is_prototype ? 2 : 3);
  const struct node *name = node->subs[0];
  const struct node *retval = node->subs[1 + arg_count];

  print_toplevel(out, &node->as.DEFFUN.toplevel);

  print_expr(out, header, mod, retval, T__NONE);
  fprintf(out, " ");
  print_expr(out, header, mod, name, T__NONE);
  fprintf(out, "(");

  if (arg_count == 0) {
    fprintf(out, "void");
  }

  for (size_t n = 0; n < arg_count; ++n) {
    if (n > 0) {
      fprintf(out, ", ");
    }
    const struct node *arg = node->subs[1 + n];
    print_typeconstraint(out, header, mod, arg);
  }
  fprintf(out, ")");
}

static bool prototype_only(bool header, const struct node *node) {
  return (header && !node_is_inline(node)) || node_is_prototype(node);
}

static void print_deffun(FILE *out, bool header, const struct module *mod, const struct node *node) {
  if (prototype_only(header, node)) {
    print_fun_prototype(out, header, mod, node);
    fprintf(out, ";\n");
  } else {
    const size_t arg_count = node->subs_count - (node->as.DEFFUN.toplevel.is_prototype ? 2 : 3);
    print_toplevel(out, &node->as.DEFFUN.toplevel);
    print_fun_prototype(out, header, mod, node);

    const struct node *block = node->subs[1 + arg_count + 1];
    print_block(out, header, mod, block);
    fprintf(out, "\n");
  }
}

static void print_deffield(FILE *out, bool header, const struct module *mod, const struct node *node) {
  print_expr(out, header, mod, node->subs[0], T__NONE);
  fprintf(out, ":");
  print_typeexpr(out, header, mod, node->subs[1]);
}

static void print_defchoice(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "| ");
  print_expr(out, header, mod, node->subs[0], T__NONE);
  switch (node->subs_count) {
  case 1:
    return;
  case 2:
    if (node->as.DEFCHOICE.has_value) {
      fprintf(out, " = ");
      print_expr(out, header, mod, node->subs[1], T__NONE);
    } else {
      fprintf(out, " -> ");
      print_expr(out, header, mod, node->subs[1], T__NONE);
    }
    return;
  case 3:
    fprintf(out, " = ");
    print_expr(out, header, mod, node->subs[1], T__NONE);
    fprintf(out, " -> ");
    print_expr(out, header, mod, node->subs[2], T__NONE);
    return;
  }
}

static void print_delegate(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "delegate ");
  print_expr(out, header, mod, node->subs[0], T__CALL);

  for (size_t n = 1; n < node->subs_count; ++n) {
    fprintf(out, " ");
    print_expr(out, header, mod, node->subs[n], T__CALL);
  }
}

static void print_deftype_statement(FILE *out, bool header, const struct module *mod, const struct node *node) {
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
  case DEFFIELD:
    print_deffield(out, header, mod, node);
    break;
  case DEFCHOICE:
    print_defchoice(out, header, mod, node);
    break;
  default:
    fprintf(stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_deftype_block(FILE *out, bool header, const struct module *mod,
                                const struct node *node, size_t first) {
  fprintf(out, " {\n");
  for (size_t n = first; n < node->subs_count; ++n) {
    const struct node *statement = node->subs[n];
    print_deftype_statement(out, header, mod, statement);
    fprintf(out, ";\n");
  }
  fprintf(out, "}\n");
}

static void print_deftype(FILE *out, bool header, const struct module *mod, const struct node *node) {
  if (typ_is_builtin(mod, node->typ)) {
    return;
  }

  const struct node *name = node->subs[0];
  fprintf(out, "struct ");
  print_expr(out, header, mod, name, T__NONE);

  if (prototype_only(header, node)) {
    fprintf(out, ";\n");
  } else {
    const bool has_isalist = node_is_prototype(node)
      ? node->subs_count > 1 : node->subs_count > 2;

    print_toplevel(out, &node->as.DEFTYPE.toplevel);

    if (!node_is_prototype(node)) {
      print_deftype_block(out, header, mod, node, has_isalist ? 2 : 1);
    }

    fprintf(out, "\n");
  }

  fprintf(out, "typedef struct ");
  print_expr(out, header, mod, name, T__NONE);
  fprintf(out, " ");
  print_expr(out, header, mod, name, T__NONE);
  fprintf(out, ";\n");
}

static void print_defmethod(FILE *out, bool header, const struct module *mod, const struct node *node) {
  const size_t arg_count = node->subs_count - (node_is_prototype(node) ? 2 : 3);
  const struct node *name = node->subs[0];
  const struct node *retval = node->subs[1 + arg_count];

  print_toplevel(out, &node->as.DEFMETHOD.toplevel);

  const char *scope = idents_value(mod->gctx, node->as.DEFMETHOD.toplevel.scope_name);
  fprintf(out, "%s method ", scope);
  print_expr(out, header, mod, name, T__NONE);

  for (size_t n = 0; n < arg_count; ++n) {
    fprintf(out, " ");
    const struct node *arg = node->subs[1 + n];
    print_typeconstraint(out, header, mod, arg);
  }

  fprintf(out, " = ");
  print_expr(out, header, mod, retval, T__NONE);

  if (!node_is_prototype(node)) {
    const struct node *block = node->subs[1 + arg_count + 1];
    print_block(out, header, mod, block);
  }

  fprintf(out, "\n");
}

static void print_defintf(FILE *out, bool header, const struct module *mod, const struct node *node) {
  print_toplevel(out, &node->as.DEFINTF.toplevel);
}

unused__ static void print_import_path(FILE *out, bool header, const struct module *mod, const struct node *node) {
  for (size_t n = 0; n < node->subs_count; ++n) {
    if (n > 0) {
      fprintf(out, "/");
    }

    print_expr(out, header, mod, node->subs[n], T__CALL);
  }
}

static void print_import(FILE *out, bool header, const struct module *mod, const struct node *node) {
}

static void print_module(FILE *out, bool header, const struct module *mod) {
  fprintf(out, "#include <nlang/runtime.h>\n");

  const struct node *top = mod->root;

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
      print_defmethod(out, header, mod, node);
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
