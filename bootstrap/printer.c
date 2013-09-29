#ifndef _POSIX_SOURCE
# define _POSIX_SOURCE
#endif
#include <stdio.h>

#include "printer.h"

const char *token_strings[TOKEN__NUM] = {
  [Timport] = "import",
  [Texport] = "export",
  [Tfrom] = "from",
  [Ttype] = "type",
  [Textern] = "extern",
  [Tfun] = "fun",
  [Tmethod] = "method",
  [Tintf] = "intf",
  [Tinline] = "inline",
  [Tlet] = "let",
  [Tsuch] = "such",
  [Tif] = "if",
  [Telif] = "elif",
  [Telse] = "else",
  [Tfor] = "for",
  [Tin] = "in",
  [Twhile] = "while",
  [Tcontinue] = "continue",
  [Tbreak] = "break",
  [Tmatch] = "match",
  [Treturn] = "return",
  [Ttry] = "try",
  [Tcatch] = "catch",
  [Texcept] = "except",
  [Tthrow] = "throw",
  [Tblock] = "block",
  [Tdelegate] = "delegate",
  [Tdeclare] = "declare",
  [Tand] = " and ",
  [Tor] = " or ",
  [Tnot] = "not ",
  [Tfalse] = "false",
  [Ttrue] = "true",
  [Tisa] = " isa ",
  [Tnull] = "null",
  [Tnoop] = "noop",
  [Tpre] = "pre",
  [Tpost] = "post",
  [Tinvariant] = "invariant",
  [Texample] = "example",

  [TASSIGN] = " = ",
  [TEQ] = " == ",
  [TNE] = " != ",
  [TEQPTR] = " === ",
  [TNEPTR] = " !== ",
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
  [TBWXOR] = " bwxor ",
  [TRSHIFT] = " >> ",
  [TLSHIFT] = " << ",
  [TPLUS_ASSIGN] = " += ",
  [TMINUS_ASSIGN] = " -= ",
  [TTIMES_ASSIGN] = " *= ",
  [TDIVIDE_ASSIGN] = " /= ",
  [TMODULO_ASSIGN] = " %= ",
  [TBWAND_ASSIGN] = " &= ",
  [TBWOR_ASSIGN] = " |= ",
  [TBWXOR_ASSIGN] = " bwxor= ",
  [TRSHIFT_ASSIGN] = " >>= ",
  [TLSHIFT_ASSIGN] = " <<= ",
  [TBWNOT] = " ~",
  [TARROW] = " -> ",
  [TCOLON] = ":",
  [TCOMMA] = ", ",
  [TSEMICOLON] = "; ",
  [TDOT] = ".",
  [TBANG] = "!",
  [TSHARP] = "#",
  [TWILDCARD] = "$",
  [TDEREFDOT] = ".",
  [TDEREFBANG] = "!",
  [TDEREFSHARP] = "#",
  [TDEREFWILDCARD] = "$",
  [TREFDOT] = "@",
  [TREFBANG] = "@!",
  [TREFSHARP] = "@#",
  [TREFWILDCARD] = "@$",
  [TNULREFDOT] = "?@",
  [TNULREFBANG] = "?@!",
  [TNULREFSHARP] = "?@#",
  [TNULREFWILDCARD] = "?@$",
  [TDOTDOTDOT] = "...",
  [TSLICEBRAKETS] = "[]",
  [TLSBRA] = "[",
  [TRSBRA] = "]",
  [TLCBRA] = "{",
  [TRCBRA] = "}",
  [TLPAR] = "(",
  [TRPAR] = ")",
};

static void print_token(FILE *out, enum token_type t) {
  fprintf(out, "%s", token_strings[t]);
}

static void spaces(FILE *out, int indent) {
  assert(indent % 2 == 0);
  for (int i = 0; i < indent; i += 2) {
    fprintf(out, "  ");
  }
}

static void do_printer_scopes(FILE *out, const struct module *mod, const struct node *root, int indent) {
  if (root->scope != NULL) {
    spaces(out, indent);
    char *n = scope_name(mod, root->scope);
    char *l = scope_definitions_name_list(mod, root->scope);
    fprintf(out, "%s:%s %s\n", n, node_which_strings[root->which], l);
    free(l);
    free(n);
  }

  for (size_t n = 0; n < root->subs_count; ++n) {
    do_printer_scopes(out, mod, root->subs[n], indent+2);
  }
}

error printer_scopes(int fd, const struct module *mod, const struct node *root) {
  FILE *out = fdopen(fd, "w");
  if (out == NULL) {
    EXCEPTF(errno, "Invalid output file descriptor '%d'", fd);
  }

  do_printer_scopes(out, mod, root, 0);
  fflush(out);

  return 0;
}

static void print_expr(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op);
static void print_block(FILE *out, const struct module *mod, int indent, const struct node *node);

static void print_pattern(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, node, T__STATEMENT);
}

static void print_bin(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.BIN.operator;
  const uint32_t prec = OP_PREC(op);
  const uint32_t parent_prec = OP_PREC(parent_op);

  if (prec > parent_prec) {
    fprintf(out, "(");
  }

  print_expr(out, mod, node->subs[0], op);
  print_token(out, op);
  print_expr(out, mod, node->subs[1], op);

  if (prec > parent_prec) {
    fprintf(out, ")");
  }
}

static void print_un(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.UN.operator;
  const uint32_t prec = OP_PREC(op);
  const uint32_t parent_prec = OP_PREC(parent_op);

  if (prec > parent_prec) {
    fprintf(out, "(");
  }

  print_token(out, op);
  print_expr(out, mod, node->subs[0], op);

  if (prec > parent_prec) {
    fprintf(out, ")");
  }
}

static void print_tuple(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t prec = OP_PREC(TCOMMA);
  const uint32_t parent_prec = OP_PREC(parent_op);

  if (prec >= parent_prec) {
    fprintf(out, "(");
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    if (n > 0) {
      fprintf(out, ", ");
    }
    print_expr(out, mod, node->subs[n], TCOMMA);
  }

  if (prec >= parent_prec) {
    fprintf(out, ")");
  }
}

static void print_call(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t prec = OP_PREC(T__CALL);
  const uint32_t parent_prec = OP_PREC(parent_op);

  if (prec >= parent_prec) {
    fprintf(out, "(");
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    if (n > 0) {
      fprintf(out, " ");
    }
    print_expr(out, mod, node->subs[n], T__CALL);
  }

  if (prec >= parent_prec) {
    fprintf(out, ")");
  }
}

static void print_init(FILE *out, const struct module *mod, const struct node *node) {
  if (node->as.INIT.is_array) {
    fprintf(out, "{ ");

    for (size_t n = 0; n < node->subs_count; n += 1) {
      print_expr(out, mod, node->subs[n], T__CALL);
      fprintf(out, " ");
    }

    fprintf(out, "}");
  } else {
    fprintf(out, "{ ");

    for (size_t n = 0; n < node->subs_count; n += 2) {
      print_expr(out, mod, node->subs[n], T__STATEMENT);
      fprintf(out, "=");
      print_expr(out, mod, node->subs[n + 1], T__CALL);
      fprintf(out, " ");
    }

    fprintf(out, "}");
  }
}

static void print_expr(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const char *val = NULL;

  switch (node->which) {
  case NUL:
    fprintf(out, "null");
    break;
  case IDENT:
    val = idents_value(mod->gctx, node->as.IDENT.name);
    fprintf(out, "%s", val);
    break;
  case NUMBER:
    fprintf(out, "%s", node->as.NUMBER.value);
    break;
  case BOOL:
    fprintf(out, "%s", node->as.BOOL.value ? "true" : "false");
    break;
  case STRING:
    fprintf(out, "%s", node->as.STRING.value);
    break;
  case SIZEOF:
    fprintf(out, "(sizeof ");
    print_expr(out, mod, node->subs[0], T__CALL);
    fprintf(out, ")");
    break;
  case BIN:
    print_bin(out, mod, node, parent_op);
    break;
  case TYPECONSTRAINT:
    print_expr(out, mod, node->subs[0], parent_op);
    fprintf(out, ":");
    print_expr(out, mod, node->subs[1], parent_op);
    break;
  case UN:
    print_un(out, mod, node, parent_op);
    break;
  case CALL:
    print_call(out, mod, node, parent_op);
    break;
  case TUPLE:
    print_tuple(out, mod, node, parent_op);
    break;
  case INIT:
    print_init(out, mod, node);
    break;
  case ISA:
    fprintf(out, "%s", node->as.ISA.is_export ? "export " : "");
    print_expr(out, mod, node->subs[0], parent_op);
    break;
  case DIRECTDEF:
    fprintf(out, "%s", scope_name(mod, node->as.DIRECTDEF.definition->scope));
    break;
  case DYN:
    print_expr(out, mod, node->subs[0], parent_op);
    break;
  case BLOCK:
    fprintf(out, "block ");
    print_block(out, mod, 0, node);
    fprintf(out, ";;");
    break;
  default:
    fprintf(stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_for(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "for ");
  print_pattern(out, mod, node->as.FOR.pattern);
  fprintf(out, " in ");
  struct node *expr = node->subs[IDX_FOR_IT]->subs[IDX_FOR_IT_DEFP]->subs[IDX_FOR_IT_DEFP_EXPR];
  print_expr(out, mod, expr, T__STATEMENT);
  print_block(out, mod, indent, node->as.FOR.block);
}

static void print_while(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "while ");
  print_expr(out, mod, node->subs[0], T__STATEMENT);
  print_block(out, mod, indent, node->subs[node->subs_count - 1]);
}

static void print_if(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "if ");
  print_expr(out, mod, node->subs[0], T__STATEMENT);
  print_block(out, mod, indent, node->subs[1]);

  size_t p = 2;
  size_t br_count = node->subs_count - 2;
  while (br_count >= 2) {
    fprintf(out, "\n");
    spaces(out, indent);
    fprintf(out, "elif ");
    print_expr(out, mod, node->subs[p], T__STATEMENT);
    print_block(out, mod, indent, node->subs[p+1]);
    p += 2;
    br_count -= 2;
  }

  if (br_count == 1) {
    fprintf(out, "\n");
    spaces(out, indent);
    fprintf(out, "else");
    print_block(out, mod, indent, node->subs[p]);
  }
}

static void print_match(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "match ");
  print_expr(out, mod, node->subs[0], T__STATEMENT);

  for (size_t n = 1; n < node->subs_count; n += 2) {
    fprintf(out, "\n");
    spaces(out, indent);
    fprintf(out, "| ");
    print_expr(out, mod, node->subs[n], T__STATEMENT);
    print_block(out, mod, indent, node->subs[n + 1]);
  }
}

static void print_try(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "try");
  print_block(out, mod, indent, node->subs[0]);
  fprintf(out, "\n");
  spaces(out, indent);
  fprintf(out, "catch ");
  print_expr(out, mod, node->subs[1], T__STATEMENT);
  print_block(out, mod, indent, node->subs[2]);
}

static void print_pre(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "pre -> ");
  print_expr(out, mod, node->subs[0], T__STATEMENT);
}

static void print_post(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "post -> ");
  print_expr(out, mod, node->subs[0], T__STATEMENT);
}

static void print_invariant(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "invariant -> ");
  print_expr(out, mod, node->subs[0], T__STATEMENT);
}

static void print_example(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "example");
  print_block(out, mod, indent, node->subs[0]);
}

static void print_toplevel(FILE *out, const struct toplevel *toplevel) {
  if (toplevel->is_export) {
    fprintf(out, "export ");
  }
  if (toplevel->is_extern) {
    fprintf(out, "extern ");
  }
  if (toplevel->is_inline) {
    fprintf(out, "inline ");
  }
}

static void print_defpattern(FILE *out, const struct module *mod, int indent, const struct node *node,
                             bool first_defp) {
  bool first_defn = TRUE;
  for (size_t n = 0; n < node->subs_count; ++n) {
    const struct node *d = node->subs[n];
    if (d->which != DEFNAME) {
      continue;
    }

    fprintf(out, (first_defp && first_defn) ? "let " : "and ");
    print_pattern(out, mod, node->subs[0]);

    first_defn = FALSE;
  }
}

static void print_let(FILE *out, const struct module *mod, int indent, const struct node *node) {
  print_toplevel(out, &node->as.LET.toplevel);
  const size_t last_def = node_has_tail_block(node) ? node->subs_count-2 : node->subs_count-1;

  for (size_t i = 0; i <= last_def; ++i) {
    if (i > 0) {
      fprintf(out, "\n");
      spaces(out, indent);
    }
    print_defpattern(out, mod, indent, node->subs[i], i == 0);
  }

  if (node_has_tail_block(node)) {
    spaces(out, indent);
    fprintf(out, "such");
    print_block(out, mod, indent, node->subs[node->subs_count-1]);
  }
}

static void print_statement(FILE *out, const struct module *mod, int indent, const struct node *node) {
  switch (node->which) {
  case RETURN:
    fprintf(out, "return");
    if (node->subs_count > 0) {
      fprintf(out, " ");
      print_expr(out, mod, node->subs[0], T__STATEMENT);
    }
    break;
  case FOR:
    print_for(out, mod, indent, node);
    break;
  case WHILE:
    print_while(out, mod, indent, node);
    break;
  case BREAK:
    fprintf(out, "break");
    break;
  case CONTINUE:
    fprintf(out, "continue");
    break;
  case NOOP:
    fprintf(out, "noop");
    break;
  case BLOCK:
    fprintf(out, "block");
    print_block(out, mod, indent, node);
    break;
  case IF:
    print_if(out, mod, indent, node);
    break;
  case MATCH:
    print_match(out, mod, indent, node);
    break;
  case TRY:
    print_try(out, mod, indent, node);
    break;
  case PRE:
    print_pre(out, mod, indent, node);
    break;
  case POST:
    print_post(out, mod, indent, node);
    break;
  case INVARIANT:
    print_invariant(out, mod, indent, node);
    break;
  case EXAMPLE:
    print_example(out, mod, indent, node);
    break;
  case LET:
    print_let(out, mod, indent, node);
    break;
  case IDENT:
  case NUMBER:
  case BOOL:
  case NUL:
  case BIN:
  case UN:
  case CALL:
  case TYPECONSTRAINT:
    print_expr(out, mod, node, T__STATEMENT);
    break;
  default:
    fprintf(stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_block(FILE *out, const struct module *mod, int indent, const struct node *node) {
  assert(node->which == BLOCK);
  fprintf(out, "\n");
  for (size_t n = 0; n < node->subs_count; ++n) {
    const struct node *statement = node->subs[n];
    spaces(out, indent + 2);
    print_statement(out, mod, indent + 2, statement);
    if (n < node->subs_count - 1) {
      fprintf(out, "\n");
    }
  }
}

static void print_typeexpr(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, node, T__STATEMENT);
}

static void print_typeconstraint(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, node->subs[0], T__STATEMENT);
  fprintf(out, ":");
  print_typeexpr(out, mod, node->subs[1]);
}

static void print_defarg(FILE *out, const struct module *mod, const struct node *node) {
  assert(node->which == DEFARG);
  print_expr(out, mod, node->subs[0], T__STATEMENT);
  fprintf(out, ":");
  print_typeexpr(out, mod, node->subs[1]);
}

static void print_deffun(FILE *out, const struct module *mod, int indent, const struct node *node) {
  const size_t args_count = node_fun_explicit_args_count(node);
  const struct node *name = node->subs[0];
  const struct node *retval = node_fun_retval_const(node);

  print_toplevel(out, &node->as.DEFFUN.toplevel);

  fprintf(out, "fun ");
  print_expr(out, mod, name, T__STATEMENT);

  for (size_t n = 0; n < args_count; ++n) {
    fprintf(out, " ");
    const struct node *arg = node->subs[IDX_FUN_FIRSTARG + n];
    print_typeconstraint(out, mod, arg);
  }

  fprintf(out, " = ");
  if (retval->which == DEFARG) {
    print_defarg(out, mod, retval);
  } else {
    print_expr(out, mod, retval, T__STATEMENT);
  }

  if (!node_toplevel_const(node)->is_prototype
      && node_toplevel_const(node)->builtingen == BG__NOT) {
    const struct node *block = node->subs[node->subs_count-1];
    print_block(out, mod, 0, block);
  }

  fprintf(out, "\n");
}

static void print_deffield(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, node->subs[0], T__STATEMENT);
  fprintf(out, ":");
  print_typeexpr(out, mod, node->subs[1]);
}

static void print_defchoice(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "| ");
  print_expr(out, mod, node->subs[0], T__STATEMENT);
  fprintf(out, " = ");
  print_expr(out, mod, node->subs[IDX_CH_VALUE], T__STATEMENT);
  if (node->subs[IDX_CH_PAYLOAD]->typ != typ_lookup_builtin(mod, TBI_VOID)) {
    fprintf(out, " -> ");
    print_expr(out, mod, node->subs[IDX_CH_PAYLOAD], T__STATEMENT);
  }
  return;
}

static void print_delegate(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "delegate ");
  print_expr(out, mod, node->subs[0], T__CALL);

  for (size_t n = 1; n < node->subs_count; ++n) {
    fprintf(out, " ");
    print_expr(out, mod, node->subs[n], T__CALL);
  }
}

static void print_deftype_statement(FILE *out, const struct module *mod, int indent, const struct node *node) {
  switch (node->which) {
  case LET:
    print_let(out, mod, indent, node);
    break;
  case DELEGATE:
    print_delegate(out, mod, node);
    break;
  case INVARIANT:
    print_invariant(out, mod, indent, node);
    break;
  case DEFFIELD:
    print_deffield(out, mod, node);
    break;
  case DEFCHOICE:
    print_defchoice(out, mod, node);
    break;
  case NOOP:
    spaces(out, indent + 2);
    fprintf(out, "noop");
    break;
  default:
    fprintf(stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_deftype_block(FILE *out, const struct module *mod, int indent,
                                const struct node *node, size_t first) {
  fprintf(out, "\n");
  for (size_t n = first; n < node->subs_count; ++n) {
    const struct node *statement = node->subs[n];
    spaces(out, indent + 2);
    print_deftype_statement(out, mod, indent + 2, statement);
    if (n < node->subs_count - 1) {
      fprintf(out, "\n");
    }
  }
}

static void print_isalist(FILE *out, const struct module *mod, const struct node *node) {
  for (size_t n = 0; n < node->subs_count; ++n) {
    fprintf(out, " ");
    print_expr(out, mod, node->subs[n], T__CALL);
  }
}

static void print_deftype(FILE *out, const struct module *mod, int indent, const struct node *node) {
  const struct node *name = node->subs[0];

  print_toplevel(out, &node->as.DEFTYPE.toplevel);

  fprintf(out, "type ");
  print_expr(out, mod, name, T__STATEMENT);
  fprintf(out, " =");

  const struct node *isalist = node->subs[IDX_ISALIST];
  print_isalist(out, mod, isalist);

  if (!node_is_prototype(node)) {
    print_deftype_block(out, mod, 0, node, IDX_ISALIST+1);
  }

  fprintf(out, "\n");
}

static void print_defmethod(FILE *out, const struct module *mod, int indent, const struct node *node) {
  const size_t args_count = node_fun_explicit_args_count(node);
  const struct node *name = node->subs[0];
  const struct node *retval = node_fun_retval_const(node);

  print_toplevel(out, &node->as.DEFMETHOD.toplevel);

  const char *scope = idents_value(mod->gctx, node->as.DEFMETHOD.toplevel.scope_name);
  fprintf(out, "%s method ", scope);
  print_expr(out, mod, name, T__STATEMENT);

  for (size_t n = 0; n < args_count; ++n) {
    fprintf(out, " ");
    const struct node *arg = node->subs[IDX_FUN_FIRSTARG + n];
    print_typeconstraint(out, mod, arg);
  }

  fprintf(out, " = ");
  if (retval->which == DEFARG) {
    print_defarg(out, mod, retval);
  } else {
    print_expr(out, mod, retval, T__STATEMENT);
  }

  if (!node_is_prototype(node) && node_toplevel_const(node)->builtingen == BG__NOT) {
    const struct node *block = node->subs[node->subs_count - 1];
    print_block(out, mod, 0, block);
  }

  fprintf(out, "\n");
}

static void print_defintf(FILE *out, const struct module *mod, int indent, const struct node *node) {
  print_toplevel(out, &node->as.DEFINTF.toplevel);
}

static void print_import_path(FILE *out, const struct module *mod, const struct node *node) {
  switch (node->which) {
  case IDENT:
    print_expr(out, mod, node, T__CALL);
    break;
  case BIN:
    print_import_path(out, mod, node->subs[0]);
    fprintf(out, ".");
    print_expr(out, mod, node->subs[1], T__CALL);
    break;
  default:
    assert(FALSE);
    break;
  }
}

static void print_import(FILE *out, const struct module *mod, int indent, const struct node *node) {
  const char *kind;
  if (node->as.IMPORT.toplevel.is_export) {
    kind = "export"; // export already printed by print_toplevel
  } else {
    kind = "import";
  }

  if (node->as.IMPORT.is_all || node->subs_count > 1) {
    fprintf(out, "from ");
  } else {
    fprintf(out, "%s ", kind);
  }

  print_import_path(out, mod, node->subs[0]);

  if (node->as.IMPORT.is_all) {
    fprintf(out, " %s *", kind);
  } else if (node->subs_count > 1) {
    fprintf(out, " %s ", kind);

    for (size_t n = 1; n < node->subs_count; ++n) {
      print_expr(out, mod, node->subs[n]->subs[0]->subs[1], T__CALL);
    }
  }
}

static void print_module(FILE *out, const struct module *mod) {
  const struct node *top = mod->body;

  for (size_t n = 0; n < top->subs_count; ++n) {
    const struct node *node = top->subs[n];

    switch (node->which) {
    case DEFFUN:
      print_deffun(out, mod, 0, node);
      break;
    case DEFTYPE:
      print_deftype(out, mod, 0, node);
      break;
    case DEFMETHOD:
      print_defmethod(out, mod, 0, node);
      break;
    case DEFINTF:
      print_defintf(out, mod, 0, node);
      break;
    case LET:
      print_let(out, mod, 0, node);
      break;
    case IMPORT:
      print_import(out, mod, 0, node);
      break;
    case EXAMPLE:
      print_example(out, mod, 0, node);
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

error printer_pretty(int fd, const struct module *mod) {
  FILE *out = fdopen(fd, "w");
  if (out == NULL) {
    EXCEPTF(errno, "Invalid output file descriptor '%d'", fd);
  }

  print_module(out, mod);
  fflush(out);

  return 0;
}

static void print_tree_node(FILE *out, const struct module *mod,
                            const struct node *node, int depth) {
  for (int i = 0; i < depth; ++i) {
    fprintf(out, " ");
  }

  fprintf(out, "%s", node_which_strings[node->which]);
  switch (node->which) {
  case IDENT:
    fprintf(out, "(%s)", idents_value(mod->gctx, node->as.IDENT.name));
    break;
  case NUMBER:
    fprintf(out, "(%s)", node->as.NUMBER.value);
    break;
  case STRING:
    fprintf(out, "(%s)", node->as.STRING.value);
    break;
  case UN:
    fprintf(out, "(%s)", token_strings[node->as.UN.operator]);
    break;
  case BIN:
    fprintf(out, "(%s)", token_strings[node->as.BIN.operator]);
    break;
  case DEFNAME:
    assert(node->as.DEFNAME.pattern->which == IDENT);
    fprintf(out, "(%s)", idents_value(mod->gctx, node_ident(node->as.DEFNAME.pattern)));
    break;
  default:
    break;
  }

  if (node->typ != NULL) {
    char *typn = typ_pretty_name(mod, node->typ);
    fprintf(out, " :%s", typn);
    free(typn);
  }

  fprintf(out, "\n");

  for (size_t n = 0; n < node->subs_count; ++n) {
    print_tree_node(out, mod, node->subs[n], depth+1);
  }
}

error printer_tree(int fd, const struct module *mod, const struct node *root) {
  FILE *out = fdopen(fd, "w");
  if (out == NULL) {
    EXCEPTF(errno, "Invalid output file descriptor '%d'", fd);
  }

  print_tree_node(out, mod, root != NULL ? root : mod->body, 0);
  fflush(out);

  return 0;
}
