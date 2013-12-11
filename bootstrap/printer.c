#ifndef _POSIX_SOURCE
# define _POSIX_SOURCE
#endif
#include <stdio.h>

#include "printer.h"
#include "types.h"
#include "scope.h"

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
  spaces(out, indent);
  char *n = scope_name(mod, &root->scope);
  char *l = scope_definitions_name_list(mod, &root->scope);
  fprintf(out, "%s:%s %s\n", n, node_which_strings[root->which], l);
  free(l);
  free(n);

  FOREACH_SUB_CONST(s, root) {
    do_printer_scopes(out, mod, s, indent+2);
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

  print_expr(out, mod, node_subs_first_const(node), op);
  print_token(out, op);
  print_expr(out, mod, node_subs_last_const(node), op);

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
  print_expr(out, mod, node_subs_first_const(node), op);

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

  size_t n = 0;
  FOREACH_SUB_CONST(s, node) {
    if (n++ > 0) {
      fprintf(out, ", ");
    }
    print_expr(out, mod, s, TCOMMA);
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

  size_t n = 0;
  FOREACH_SUB_CONST(s, node) {
    if (n++ > 0) {
      fprintf(out, " ");
    }
    print_expr(out, mod, s, T__CALL);
  }

  if (prec >= parent_prec) {
    fprintf(out, ")");
  }
}

static void print_init(FILE *out, const struct module *mod, const struct node *node) {
  if (node->as.INIT.is_array) {
    fprintf(out, "{ ");

    FOREACH_SUB_CONST(s, node) {
      print_expr(out, mod, s, T__CALL);
      fprintf(out, " ");
    }

    fprintf(out, "}");
  } else {
    fprintf(out, "{ ");

    FOREACH_SUB_EVERY_CONST(s, node, 0, 2) {
      print_expr(out, mod, s, T__STATEMENT);
      fprintf(out, "=");
      print_expr(out, mod, node_next_const(s), T__CALL);
      fprintf(out, " ");
    }

    fprintf(out, "}");
  }
}

static void print_statement(FILE *out, const struct module *mod, int indent, const struct node *node);

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
  case EXCEP:
    fprintf(out, "except");
    break;
  case SIZEOF:
    fprintf(out, "(sizeof ");
    print_expr(out, mod, node_subs_first_const(node), T__CALL);
    fprintf(out, ")");
    break;
  case BIN:
    print_bin(out, mod, node, parent_op);
    break;
  case DEFARG:
  case TYPECONSTRAINT:
    print_expr(out, mod, node_subs_first_const(node), parent_op);
    fprintf(out, ":");
    print_expr(out, mod, node_subs_last_const(node), parent_op);
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
  case TUPLEEXTRACT:
    print_expr(out, mod, node_subs_last_const(node), parent_op);
    break;
  case INIT:
    print_init(out, mod, node);
    break;
  case ISA:
    fprintf(out, "%s", node->as.ISA.is_export ? "export " : "");
    print_expr(out, mod, node_subs_first_const(node), parent_op);
    break;
  case DIRECTDEF:
    fprintf(out, "%s",
            scope_name(mod, &typ_definition_const(node->as.DIRECTDEF.typ)->scope));
    break;
  case DYN:
    print_expr(out, mod, node_subs_first_const(node), parent_op);
    break;
  case BLOCK:
    fprintf(out, "block ");
    print_block(out, mod, 0, node);
    fprintf(out, ";;");
    break;
  case IF:
  case TRY:
  case MATCH:
  case LET:
    print_statement(out, mod, 0, node);
    break;
  default:
    fprintf(g_env.stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_for(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "for ");
  print_pattern(out, mod, node->as.FOR.pattern);
  fprintf(out, " in ");
  const struct node *expr = node_subs_at_const(node_subs_at_const(node_subs_at_const(
        node, IDX_FOR_IT), IDX_FOR_IT_DEFP), IDX_FOR_IT_DEFP_EXPR);
  print_expr(out, mod, expr, T__STATEMENT);
  print_block(out, mod, indent, node->as.FOR.block);
}

static void print_while(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "while ");
  print_expr(out, mod, node_subs_first_const(node), T__STATEMENT);
  print_block(out, mod, indent, node_subs_last_const(node));
}

static void print_if(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "if ");
  print_expr(out, mod, node_subs_first_const(node), T__STATEMENT);
  print_block(out, mod, indent, node_subs_at_const(node, 1));

  const struct node *els = NULL;
  FOREACH_SUB_EVERY_CONST(cond, node, 2, 2) {
    if (node_next_const(cond) == NULL) {
      els = cond;
      break;
    }
    fprintf(out, "\n");
    spaces(out, indent);
    fprintf(out, "elif ");
    print_expr(out, mod, cond, T__STATEMENT);
    print_block(out, mod, indent, node_next_const(cond));
  }

  if (els != NULL) {
    fprintf(out, "\n");
    spaces(out, indent);
    fprintf(out, "else");
    print_block(out, mod, indent, els);
  }
}

static void print_match(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "match ");
  print_expr(out, mod, node_subs_first_const(node), T__STATEMENT);

  FOREACH_SUB_EVERY_CONST(p, node, 1, 2) {
    fprintf(out, "\n");
    spaces(out, indent);
    fprintf(out, "| ");
    print_expr(out, mod, p, T__STATEMENT);
    print_block(out, mod, indent, node_next_const(p));
  }
}

static void print_try(FILE *out, const struct module *mod, int indent, const struct node *node) {
  const struct node *eblock = node_subs_at_const(node_subs_first_const(node), 1);

  fprintf(out, "try");
  print_block(out, mod, indent, node_subs_first_const(eblock));
  fprintf(out, "\n");

  FOREACH_SUB_EVERY_CONST(catch, eblock, 1, 1) {
    spaces(out, indent);
    fprintf(out, "catch ");
    if (catch->as.CATCH.label != ID__NONE) {
      fprintf(out, "%s ", idents_value(mod->gctx, catch->as.CATCH.label));
    }
    print_expr(out, mod,
               node_subs_first_const(node_subs_first_const(node_subs_first_const(catch))),
               T__STATEMENT);
    fprintf(out, "\n");
    print_block(out, mod, indent, node_subs_at_const(catch, 1));
  }
}

static void print_pre(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "pre -> ");
  print_expr(out, mod, node_subs_first_const(node), T__STATEMENT);
}

static void print_post(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "post -> ");
  print_expr(out, mod, node_subs_first_const(node), T__STATEMENT);
}

static void print_invariant(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "invariant -> ");
  print_expr(out, mod, node_subs_first_const(node), T__STATEMENT);
}

static void print_example(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "example");
  print_block(out, mod, indent, node_subs_first_const(node));
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
  fprintf(out, first_defp ? "let " : "and ");
  print_pattern(out, mod, node_subs_first_const(node));

  if (node_subs_at_const(node, 1)->which != DEFNAME) {
    fprintf(out, " = ");
    print_expr(out, mod, node_subs_at_const(node, 1), T__STATEMENT);
  }
}

static void print_let(FILE *out, const struct module *mod, int indent, const struct node *node) {
  print_toplevel(out, &node->as.LET.toplevel);

  size_t i = 0;
  FOREACH_SUB_CONST(d, node) {
    if (d->which == BLOCK) {
      break;
    }
    if (i++ > 0) {
      fprintf(out, "\n");
      spaces(out, indent);
    }
    print_defpattern(out, mod, indent, d, i == 0);
  }

  if (node_has_tail_block(node)) {
    spaces(out, indent);
    fprintf(out, "such");
    print_block(out, mod, indent, node_subs_last_const(node));
  }
}

static void print_statement(FILE *out, const struct module *mod, int indent, const struct node *node) {
  switch (node->which) {
  case RETURN:
    fprintf(out, "return");
    if (node_subs_count_atleast(node, 1)) {
      fprintf(out, " ");
      print_expr(out, mod, node_subs_first_const(node), T__STATEMENT);
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
  case THROW:
    fprintf(out, "throw ");
    FOREACH_SUB_CONST(s, node) {
      print_expr(out, mod, s, T__CALL);
    }
    break;
  case EXCEP:
    fprintf(out, "except ");
    FOREACH_SUB_CONST(s, node) {
      print_expr(out, mod, s, T__CALL);
    }
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
  case TUPLEEXTRACT:
  case TYPECONSTRAINT:
    print_expr(out, mod, node, T__STATEMENT);
    break;
  default:
    fprintf(g_env.stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_block(FILE *out, const struct module *mod, int indent, const struct node *node) {
  assert(node->which == BLOCK);
  fprintf(out, "\n");
  FOREACH_SUB_CONST(statement, node) {
    spaces(out, indent + 2);
    print_statement(out, mod, indent + 2, statement);
    if (node_next_const(statement) != NULL) {
      fprintf(out, "\n");
    }
  }
}

static void print_typeexpr(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, node, T__STATEMENT);
}

static void print_typeconstraint(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, node_subs_first_const(node), T__STATEMENT);
  fprintf(out, ":");
  print_typeexpr(out, mod, node_subs_last_const(node));
}

static void print_deffun(FILE *out, const struct module *mod, int indent, const struct node *node) {
  const struct node *name = node_subs_first_const(node);
  const struct node *retval = node_fun_retval_const(node);

  print_toplevel(out, &node->as.DEFFUN.toplevel);

  fprintf(out, "fun ");
  print_expr(out, mod, name, T__STATEMENT);

  const struct node *funargs = node_subs_at_const(node, IDX_FUNARGS);
  FOREACH_SUB_CONST(arg, funargs) {
    if (node_next_const(arg) == NULL) {
      break;
    }
    fprintf(out, " ");
    print_typeconstraint(out, mod, arg);
  }

  fprintf(out, " = ");
  print_expr(out, mod, retval, T__STATEMENT);

  if (!node_toplevel_const(node)->is_prototype
      && node_toplevel_const(node)->builtingen == BG__NOT) {
    const struct node *block = node_subs_last_const(node);
    print_block(out, mod, 0, block);
  }

  fprintf(out, "\n");
}

static void print_deffield(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, node_subs_first_const(node), T__STATEMENT);
  fprintf(out, ":");
  print_typeexpr(out, mod, node_subs_at_const(node, 1));
}

static void print_defchoice(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "| ");
  print_expr(out, mod, node_subs_first_const(node), T__STATEMENT);
  fprintf(out, " = ");
  print_expr(out, mod, node_subs_at_const(node, IDX_CH_VALUE), T__STATEMENT);
  if (node_subs_at_const(node, IDX_CH_PAYLOAD)->typ != TBI_VOID) {
    fprintf(out, " -> ");
    print_expr(out, mod, node_subs_at_const(node, IDX_CH_PAYLOAD), T__STATEMENT);
  }
  return;
}

static void print_delegate(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "delegate ");
  print_expr(out, mod, node_subs_first_const(node), T__CALL);

  FOREACH_SUB_EVERY_CONST(s, node, 1, 1) {
    fprintf(out, " ");
    print_expr(out, mod, s, T__CALL);
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
    fprintf(g_env.stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_deftype_block(FILE *out, const struct module *mod, int indent,
                                const struct node *node, size_t first) {
  fprintf(out, "\n");
  FOREACH_SUB_EVERY_CONST(statement, node, first, 1) {
    spaces(out, indent + 2);
    print_deftype_statement(out, mod, indent + 2, statement);
    if (node_next_const(statement) != NULL) {
      fprintf(out, "\n");
    }
  }
}

static void print_isalist(FILE *out, const struct module *mod, const struct node *node) {
  FOREACH_SUB_CONST(s, node) {
    fprintf(out, " ");
    print_expr(out, mod, s, T__CALL);
  }
}

static void print_deftype(FILE *out, const struct module *mod, int indent, const struct node *node) {
  const struct node *name = node_subs_first_const(node);

  print_toplevel(out, &node->as.DEFTYPE.toplevel);

  fprintf(out, "type ");
  print_expr(out, mod, name, T__STATEMENT);
  fprintf(out, " =");

  const struct node *isalist = node_subs_at_const(node, IDX_ISALIST);
  print_isalist(out, mod, isalist);

  if (!node_is_prototype(node)) {
    print_deftype_block(out, mod, 0, node, IDX_ISALIST+1);
  }

  fprintf(out, "\n");
}

static void print_defmethod(FILE *out, const struct module *mod, int indent, const struct node *node) {
  const struct node *name = node_subs_first_const(node);
  const struct node *retval = node_fun_retval_const(node);

  print_toplevel(out, &node->as.DEFMETHOD.toplevel);

  const char *scope = idents_value(mod->gctx, node->as.DEFMETHOD.toplevel.scope_name);
  fprintf(out, "%s method ", scope);
  print_expr(out, mod, name, T__STATEMENT);

  const struct node *funargs = node_subs_at_const(node, IDX_FUNARGS);
  FOREACH_SUB_EVERY_CONST(arg, funargs, 1, 1) {
    if (node_next_const(arg) == NULL) {
      break;
    }
    fprintf(out, " ");
    print_typeconstraint(out, mod, arg);
  }

  fprintf(out, " = ");
  print_expr(out, mod, retval, T__STATEMENT);

  if (!node_is_prototype(node) && node_toplevel_const(node)->builtingen == BG__NOT) {
    const struct node *block = node_subs_last_const(node);
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
    print_import_path(out, mod, node_subs_first_const(node));
    fprintf(out, ".");
    print_expr(out, mod, node_subs_last_const(node), T__CALL);
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

  if (node->as.IMPORT.is_all || node_subs_count_atleast(node, 2)) {
    fprintf(out, "from ");
  } else {
    fprintf(out, "%s ", kind);
  }

  print_import_path(out, mod, node_subs_first_const(node));

  if (node->as.IMPORT.is_all) {
    fprintf(out, " %s *", kind);
  } else if (node_subs_count_atleast(node, 2)) {
    fprintf(out, " %s ", kind);

    FOREACH_SUB_EVERY_CONST(i, node, 1, 1) {
      if (i->typ == NULL) {
        print_expr(out, mod, node_subs_at_const(node_subs_first_const(i), 1), T__CALL);
      }
    }
  }
}

static void print_module(FILE *out, const struct module *mod) {
  const struct node *top = mod->body;

  FOREACH_SUB_CONST(node, top) {
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
      fprintf(g_env.stderr, "Unsupported node: %d\n", node->which);
      assert(FALSE);
    }

    if (node_next_const(node) != NULL) {
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
    if (node->as.DEFNAME.is_excep) {
      fprintf(out, "(excep %s %s)",
              idents_value(mod->gctx, node->as.DEFNAME.excep_label),
              idents_value(mod->gctx, node_ident(node->as.DEFNAME.pattern)));
    } else {
      fprintf(out, "(%s)", idents_value(mod->gctx, node_ident(node->as.DEFNAME.pattern)));
    }
    break;
  default:
    break;
  }

  if (node->typ != NULL) {
    const struct node *def = typ_definition_const(node->typ);
    if (def->which == IMPORT) {
      fprintf(out, " :<import>");
    } else {
      char *typn = typ_pretty_name(mod, node->typ);
      fprintf(out, " :%s", typn);
      free(typn);
    }
  }

  fprintf(out, "\n");

  FOREACH_SUB_CONST(s, node) {
    print_tree_node(out, mod, s, depth+1);
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
