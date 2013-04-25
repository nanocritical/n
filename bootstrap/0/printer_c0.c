#ifndef _POSIX_SOURCE
# define _POSIX_SOURCE
#endif
#include <stdio.h>

#include "printer.h"

enum forward {
  FWDTYPES,
  DEFTYPES,
  FWDFUNS,
  DEFFUNS,
};

static const char *c_token_strings[TOKEN__NUM] = {
  [TASSIGN] = " = ",
  [TEQ] = " == ",
  [TNE] = " != ",
  [TEQPTR] = " == ",
  [TNEPTR] = " != ",
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
  [Tor] = " || ",
  [Tand] = " && ",
  [Tnot] = "!",
  [Tfalse] = "false",
  [Ttrue] = "true",
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

static size_t c_fun_args_count(const struct node *node) {
  return node_fun_all_args_count(node);
}

static void print_token(FILE *out, enum token_type t) {
  fprintf(out, "%s", c_token_strings[t]);
}

static void print_expr(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op);
static void print_block(FILE *out, const struct module *mod, const struct node *node);
static void print_typ(FILE *out, const struct module *mod, const struct typ *typ);
static void print_typeconstraint(FILE *out, const struct module *mod, const struct node *node);
static void print_ident(FILE *out, const struct module *mod, const struct node *node);
static void print_statement(FILE *out, const struct module *mod, const struct node *node);

static void print_pattern(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, node, T__STATEMENT);
}

static void print_bin_sym(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.BIN.operator;
  const struct node *expr = node->subs[1];
  if (OP_ASSIGN(op)
      && expr->which == CALL
      && !typ_isa(mod, expr->typ, typ_lookup_builtin(mod, TBI_RETURN_BY_COPY))) {
    print_expr(out, mod, expr, T__STATEMENT);
  } else {
    print_expr(out, mod, node->subs[0], op);
    print_token(out, op);
    print_expr(out, mod, node->subs[1], op);
  }
}

static void print_bin_acc(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  assert(node->subs[1]->which == IDENT);
  const uint32_t op = node->as.BIN.operator;
  const struct node *left = node->subs[0];
  const struct node *right = node->subs[1];

  if (node->flags & NODE_IS_DEFCHOICE) {
    print_typ(out, mod, left->typ);
    fprintf(out, "_%s", idents_value(mod->gctx, node_ident(right)));
  } else if (node->flags & NODE_IS_TYPE) {
    print_typ(out, mod, node->typ);
  } else {
    const char *deref = ".";
    if (typ_is_reference_instance(mod, left->typ)) {
      deref = "->";
    }
    print_expr(out, mod, left, op);
    fprintf(out, "%s%s", deref, idents_value(mod->gctx, right->as.IDENT.name));
  }
}

static void print_bin(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
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
  case OP_BIN_SYM_PTR:
  case OP_BIN_NUM_RHS_U16:
    print_bin_sym(out, mod, node, parent_op);
    break;
  case OP_BIN_ACC:
    print_bin_acc(out, mod, node, parent_op);
    break;
  case OP_BIN_RHS_TYPE:
    print_typeconstraint(out, mod, node);
    break;
  }

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

  switch (OP_KIND(op)) {
  case OP_UN_REFOF:
    if (node->flags & NODE_IS_TYPE) {
      print_typ(out, mod, node->typ);
    } else {
      fprintf(out, "(&");
      print_expr(out, mod, node->subs[0], op);
      fprintf(out, ")");
    }
    break;
  case OP_UN_DEREF:
    if (node->flags & NODE_IS_TYPE) {
      print_typ(out, mod, node->subs[0]->typ);
    } else {
      fprintf(out, "(*");
      print_expr(out, mod, node->subs[0], op);
      fprintf(out, ")");
    }
    break;
  case OP_UN_BOOL:
  case OP_UN_NUM:
    print_token(out, op);
    print_expr(out, mod, node->subs[0], op);
    break;
  }

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

static void print_call(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  if (node_ident(node->subs[0]->typ->definition) == ID_CAST) {
    fprintf(out, "(");
    print_typ(out, mod, node->typ);
    fprintf(out, ")(");
    print_expr(out, mod, node->subs[1], T__CALL);
    fprintf(out, ")");
    return;
  }

  const struct typ *ftyp = node->subs[0]->typ;
  print_typ(out, mod, ftyp);
  fprintf(out, "(");

  size_t n;
  for (n = 0; n < c_fun_args_count(ftyp->definition); ++n) {
    if (n > 0) {
      fprintf(out, ", ");
    }
    print_expr(out, mod, node->subs[1 + n], T__CALL);
  }

  const bool retval_throughref = !typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_RETURN_BY_COPY));
  if (retval_throughref) {
    if (n > 0) {
      fprintf(out, ", ");
    }

    fprintf(out, "&(");
    print_expr(out, mod, node->as.CALL.return_through_ref_expr, T__CALL);
    fprintf(out, ")");
  }

  fprintf(out, ")");
}

static void print_init_toplevel(FILE *out, const struct module *mod, const struct node *node) {
  if (node->subs_count == 1) {
    fprintf(out, " = { 0 }");
    return;
  }

  fprintf(out, " = {\n");
  for (size_t n = 1; n < node->subs_count; n += 2) {
    fprintf(out, ".%s = ",
            idents_value(mod->gctx, node_ident(node->subs[n])));
    print_expr(out, mod, node->subs[n + 1], T__NOT_STATEMENT);
    fprintf(out, ",\n");
  }
  fprintf(out, " }\n");
}

static void print_init(FILE *out, const struct module *mod, const struct node *node) {
  switch (node->scope->parent->parent->parent->node->which) {
  case MODULE_BODY:
  case DEFTYPE:
    print_init_toplevel(out, mod, node);
    return;
  default:
    break;
  }

  const struct node *target = node->as.INIT.target_expr;

  fprintf(out, "; memset(&(");
  print_expr(out, mod, target, T__STATEMENT);
  fprintf(out, "), 0, sizeof(");
  print_typ(out, mod, node->typ);
  fprintf(out, "));\n");

  for (size_t n = 1; n < node->subs_count; n += 2) {
    print_expr(out, mod, target, TDOT);
    fprintf(out, ".%s = ",
            idents_value(mod->gctx, node_ident(node->subs[n])));
    print_expr(out, mod, node->subs[n + 1], T__NOT_STATEMENT);
    fprintf(out, ";\n");
  }
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
  print_typ(out, mod, node->typ);
}

static void print_deffun_name(FILE *out, const struct module *mod, const struct node *node) {
  const ident id = node_ident(node);
  if (id == ID_MAIN) {
    fprintf(out, "main");
  } else {
    print_typ(out, mod, node->typ);
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

static void print_expr(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
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
  case BOOL:
    fprintf(out, "(");
    print_typ(out, mod, node->typ);
    fprintf(out, ")");
    fprintf(out, "%s", node->as.BOOL.value ? "1" : "0");
    break;
  case STRING:
    {
      char *s = escape_string(node->as.STRING.value);
      fprintf(out, "\"%s\"", s);
      free(s);
      break;
    }
  case SIZEOF:
    fprintf(out, "sizeof(");
    print_typ(out, mod, node->subs[0]->typ);
    fprintf(out, ")");
    break;
  case BIN:
    print_bin(out, mod, node, parent_op);
    break;
  case UN:
    print_un(out, mod, node, parent_op);
    break;
  case TYPECONSTRAINT:
    print_typeconstraint(out, mod, node);
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
  default:
    fprintf(stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_for(FILE *out, const struct module *mod, const struct node *node) {
  print_statement(out, mod, node->subs[0]);
}

static void print_while(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "while (");
  print_expr(out, mod, node->subs[0], T__STATEMENT);
  fprintf(out, ")");
  print_block(out, mod, node->subs[1]);
}

static void print_if(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "if (");
  print_expr(out, mod, node->subs[0], T__STATEMENT);
  fprintf(out, ")");
  print_block(out, mod, node->subs[1]);

  size_t p = 2;
  size_t br_count = node->subs_count - 2;
  while (br_count >= 2) {
    fprintf(out, "\n");
    fprintf(out, "else if (");
    print_expr(out, mod, node->subs[p], T__STATEMENT);
    fprintf(out, ") ");
    print_block(out, mod, node->subs[p+1]);
    p += 2;
    br_count -= 2;
  }

  if (br_count == 1) {
    fprintf(out, "\n");
    fprintf(out, "else ");
    print_block(out, mod, node->subs[p]);
  }
}

static void print_match(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "switch (");
  print_expr(out, mod, node->subs[0], T__STATEMENT);
  fprintf(out, ") {\n");

  for (size_t n = 1; n < node->subs_count; n += 2) {
    const struct node *p = node->subs[n];
    if (node_ident(p) != ID_OTHERWISE) {
      fprintf(out, "case ");
      print_typ(out, mod, p->typ);
      const struct node *id = p;
      if (p->which == BIN) {
        id = node->subs[n]->subs[1];
      }
      fprintf(out, "_%s_label__", idents_value(mod->gctx, node_ident(id)));
      fprintf(out, ":\n");
    }

    if (n == node->subs_count - 2) {
      fprintf(out, "default:\n");
    }

    print_block(out, mod, node->subs[n + 1]);
    fprintf(out, "\n");
  }
  fprintf(out, "}");
}

static void print_try(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "try");
  print_block(out, mod, node->subs[0]);
  fprintf(out, "\n");
  fprintf(out, "catch ");
  print_expr(out, mod, node->subs[1], T__STATEMENT);
  print_block(out, mod, node->subs[2]);
}

static void print_pre(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "pre");
  print_block(out, mod, node->subs[0]);
}

static void print_post(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "post");
  print_block(out, mod, node->subs[0]);
}

static void print_invariant(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "invariant");
  print_block(out, mod, node->subs[0]);
}

static void print_example(FILE *out, bool header, const struct module *mod, const struct node *node) {
  fprintf(out, "example");
  print_block(out, mod, node->subs[0]);
}

static void print_toplevel(FILE *out, bool header, const struct node *node) {
  if (header && !node_is_export(node)) {
    return;
  }

  const struct toplevel *toplevel = node_toplevel_const(node);
  if (toplevel->is_extern) {
    fprintf(out, "extern ");
  } else if (toplevel->is_inline && node->which != LET) {
    fprintf(out, "static inline ");
  } else if (node_is_at_top(node) && !toplevel->is_export) {
    fprintf(out, "static ");
  }
}

static void print_typ(FILE *out, const struct module *mod, const struct typ *typ) {
  switch (typ->which) {
  case TYPE_DEF:
    if (typ->gen_arity > 0) {
      fprintf(out, "_Ngen_");
    }

    fprintf(out, "%s", replace_dots(typ_name(mod, typ)));

    if (typ->gen_arity > 0) {
      for (size_t n = 1; n < typ->gen_arity + 1; ++n) {
        fprintf(out, "__");
        print_typ(out, mod, typ->gen_args[n]);
      }
      fprintf(out, "_genN_");
    }
    break;
  case TYPE_FUNCTION:
    if (typ->gen_arity > 0) {
      fprintf(out, "_Ngen_");
    }

    if (node_is_at_top(typ->definition)) {
      fprintf(out, "%s", replace_dots(typ_name(mod, typ)));
    } else {
      const struct node *parent = typ->definition->scope->parent->node;
      const struct typ *tparent = parent->typ;
      print_typ(out, mod, tparent);
      if (parent->which == DEFCHOICE) {
        fprintf(out, "_%s", idents_value(mod->gctx, node_ident(parent)));
      }
      fprintf(out, "_%s", idents_value(mod->gctx, node_ident(typ->definition)));
    }

    if (typ->gen_arity > 0) {
      for (size_t n = 1; n < typ->gen_arity + 1; ++n) {
        fprintf(out, "__");
        print_typ(out, mod, typ->gen_args[n]);
      }
      fprintf(out, "_genN_");
    }
    break;
  case TYPE_TUPLE:
    fprintf(out, "_Ntup_");
    for (size_t n = 1; n < typ->gen_arity + 1; ++n) {
      if (n > 1) {
        fprintf(out, "__");
      }
      print_typ(out, mod, typ->gen_args[n]);
    }
    fprintf(out, "_putN_");
    break;
  default:
    break;
  }
}

static void print_defname(FILE *out, bool header, enum forward fwd,
                          const struct module *mod, const struct node *node,
                          const struct node *pattern) {
  assert(node->which == DEFNAME);
  if (fwd == FWDTYPES) {
    if ((node->flags & NODE_IS_TYPE)) {
      struct node *pp = node->scope->parent->parent->node;
      if (pp->which == DEFTYPE) {
        const ident id = node_ident(node->as.DEFNAME.pattern);
        if (id != ID_THIS) {
          fprintf(out, "typedef ");
          print_typ(out, mod, node->typ);
          fprintf(out, " ");
          print_typ(out, mod, pp->typ);
          fprintf(out, "_");
          print_expr(out, mod, node->as.DEFNAME.pattern, T__STATEMENT);
          fprintf(out, ";\n");
        }
      }
    }
  } else if (!(node->flags & NODE_IS_TYPE)) {
    print_toplevel(out, header, node->scope->parent->parent->node);

    print_typ(out, mod, node->typ);
    fprintf(out, " ");
    print_pattern(out, mod, node->as.DEFNAME.pattern);

    if (fwd == DEFFUNS && (!header || node_is_inline(pattern))) {
      const struct node *expr = node->as.DEFNAME.expr;
      if (expr != NULL) {
        if (expr->which == INIT) {
          print_init(out, mod, expr);
        } else if (expr->which == CALL && !typ_isa(mod, expr->typ, typ_lookup_builtin(mod, TBI_RETURN_BY_COPY))) {
          fprintf(out, " = { 0 };\n");
          print_expr(out, mod, expr, T__STATEMENT);
        } else {
          fprintf(out, " = ");
          print_expr(out, mod, expr, T__STATEMENT);
        }
        fprintf(out, ";\n");
      } else {
        fprintf(out, " = { 0 };\n");
      }
    } else {
      fprintf(out, ";\n");
    }
  }
}

static void print_defpattern(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node) {
  assert(node->which == DEFPATTERN);
  struct node *let = node->scope->parent->node;
  if (node_is_at_top(let) && header && !node_is_export(let)) {
    return;
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    if (node->subs[n]->which != DEFNAME) {
      continue;
    }

    print_defname(out, header, fwd, mod, node->subs[n], node);
  }
}

static void print_let(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node) {
  print_defpattern(out, header, fwd, mod, node->subs[0]);

  if (fwd == DEFFUNS && node->subs_count > 1) {
    assert(!node_is_inline(node));
    print_block(out, mod, node->subs[1]);
  }
}

static void print_return(FILE *out, const struct module *mod, const struct node *node) {
  if (node->subs_count > 0) {
    const struct node *expr = node->subs[0];
    if (typ_isa(mod, node->typ, typ_lookup_builtin(mod, TBI_RETURN_BY_COPY))) {
      fprintf(out, "return ");
      print_expr(out, mod, node->subs[0], T__STATEMENT);
    } else if (node->as.RETURN.return_through_ref_expr != NULL) {
      print_expr(out, mod, node->as.RETURN.return_through_ref_expr, T__STATEMENT);
      fprintf(out, " = ");
      print_expr(out, mod, node->subs[0], T__STATEMENT);
      fprintf(out, ";\nreturn");
    } else {
      if (expr->which != IDENT) {
        print_expr(out, mod, expr, T__STATEMENT);
      }
      fprintf(out, ";\nreturn");
    }
  } else {
    fprintf(out, "return");
  }
}

static void print_statement(FILE *out, const struct module *mod, const struct node *node) {
  switch (node->which) {
  case RETURN:
    print_return(out, mod, node);
    break;
  case FOR:
    print_for(out, mod, node);
    break;
  case WHILE:
    print_while(out, mod, node);
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
    print_if(out, mod, node);
    break;
  case MATCH:
    print_match(out, mod, node);
    break;
  case TRY:
    print_try(out, mod, node);
    break;
  case PRE:
    print_pre(out, mod, node);
    break;
  case POST:
    print_post(out, mod, node);
    break;
  case INVARIANT:
    print_invariant(out, mod, node);
    break;
  case EXAMPLE:
    print_example(out, FALSE, mod, node);
    break;
  case LET:
    print_let(out, FALSE, DEFFUNS, mod, node);
    break;
  case IDENT:
  case BIN:
  case UN:
  case CALL:
    print_expr(out, mod, node, T__STATEMENT);
    break;
  default:
    fprintf(stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_block(FILE *out, const struct module *mod, const struct node *node) {
  assert(node->which == BLOCK);
  fprintf(out, " {\n");
  for (size_t n = 0; n < node->subs_count; ++n) {
    const struct node *statement = node->subs[n];
    print_statement(out, mod, statement);
    fprintf(out, ";\n");
  }
  fprintf(out, "}");
}

static void print_typeconstraint(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "(");
  print_typ(out, mod, node->typ);
  fprintf(out, ")");
  print_expr(out, mod, node->subs[0], T__STATEMENT);
}

static void print_defarg(FILE *out, const struct module *mod, const struct node *node,
                         bool return_through_ref) {
  assert(node->which == DEFARG);
  print_typ(out, mod, node->typ);
  fprintf(out, " ");
  if (return_through_ref) {
    fprintf(out, "*_nrtr_");
  }
  print_expr(out, mod, node->subs[0], T__STATEMENT);
}

static void print_fun_prototype(FILE *out, bool header, const struct module *mod,
                                const struct node *node) {
  const size_t args_count = c_fun_args_count(node);
  const struct node *retval = node_fun_retval_const(node);
  const bool retval_throughref = !typ_isa(mod, retval->typ, typ_lookup_builtin(mod, TBI_RETURN_BY_COPY));

  print_toplevel(out, header, node);

  if (retval_throughref) {
    fprintf(out, "void");
  } else {
    print_typ(out, mod, retval->typ);
  }

  fprintf(out, " ");
  print_deffun_name(out, mod, node);
  fprintf(out, "(");

  if (!retval_throughref && args_count == 0) {
    fprintf(out, "void");
  }

  size_t n;
  for (n = 0; n < args_count; ++n) {
    if (n > 0) {
      fprintf(out, ", ");
    }
    const struct node *arg = node->subs[IDX_FUN_FIRSTARG + n];
    print_defarg(out, mod, arg, FALSE);
  }

  if (retval_throughref) {
    if (n > 0) {
      fprintf(out, ", ");
    }

    print_defarg(out, mod, retval, TRUE);
  }

  fprintf(out, ")");
}

static bool prototype_only(bool header, const struct node *node) {
  return (header && !(node_is_export(node) && node_is_inline(node))) || node_is_prototype(node);
}

static const struct node *get_defchoice_member(const struct module *mod,
                                               const struct node *ch,
                                               ident member_id) {
  assert(ch->which == DEFCHOICE);
  struct node *m = NULL;
  error e = scope_lookup_ident_immediate(
    &m, mod, ch->subs[IDX_CH_PAYLOAD]->typ->definition->scope, member_id, FALSE);
  assert(!e);
  return m;
}

static const char *returns_something(const struct module *mod, const struct node *m) {
  return node_fun_retval_const(m)->typ == typ_lookup_builtin(mod, TBI_VOID) ? "" : "return";
}

static void rtr_helpers(FILE *out, const struct module *mod,
                        const struct node *node, bool start) {
  const struct node *retval = node_fun_retval_const(node);
  const bool retval_bycopy = typ_isa(mod, retval->typ, typ_lookup_builtin(mod, TBI_RETURN_BY_COPY));
  if (retval_bycopy) {
    return;
  }

  if (start) {
    fprintf(out, "#define ");
    print_expr(out, mod, retval->subs[0], T__STATEMENT);
    fprintf(out, " (*_nrtr_");
    print_expr(out, mod, retval->subs[0], T__STATEMENT);
    fprintf(out, ")\n");
  } else {
    fprintf(out, "#undef ");
    print_expr(out, mod, retval->subs[0], T__STATEMENT);
    fprintf(out, "\n");
  }
}

static void bg_return_if_by_copy(FILE *out, const struct module *mod, const struct node *node,
                                 const char *what) {
  const struct node *retval = node_fun_retval_const(node);
  const bool retval_bycopy = typ_isa(mod, retval->typ, typ_lookup_builtin(mod, TBI_RETURN_BY_COPY));
  if (!retval_bycopy) {
    return;
  }

  fprintf(out, "return %s;\n", what);
}

static void print_deffun_builtingen(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, " {\n");
  if (node->scope->parent->node->which == DEFTYPE
      || node->scope->parent->node->which == DEFCHOICE) {
    fprintf(out, "#define THIS(x) ");
    print_typ(out, mod, node->scope->parent->node->typ);
    fprintf(out, "##x\n");
  }
  switch (node_toplevel_const(node)->builtingen) {
  case BG_TRIVIAL_CTOR_CTOR:
    break;
  case BG_TRIVIAL_CTOR_MK:
    rtr_helpers(out, mod, node, TRUE);
    bg_return_if_by_copy(out, mod, node, "(THIS()){ 0 }");
    rtr_helpers(out, mod, node, FALSE);
    break;
  case BG_TRIVIAL_CTOR_NEW:
    fprintf(out, "return calloc(1, sizeof(THIS()));\n");
    break;
  case BG_DEFAULT_CTOR_MK:
    rtr_helpers(out, mod, node, TRUE);
    fprintf(out, "THIS(_ctor)(&r);\n");
    bg_return_if_by_copy(out, mod, node, "r");
    rtr_helpers(out, mod, node, FALSE);
    break;
  case BG_DEFAULT_CTOR_NEW:
    fprintf(out, "THIS() *self = calloc(1, sizeof(sizeof(THIS())));\n");
    fprintf(out, "THIS(_ctor)(self);\n");
    fprintf(out, "return self;\n");
    break;
  case BG_CTOR_WITH_MK:
    rtr_helpers(out, mod, node, TRUE);
    fprintf(out, "THIS(_ctor)(&r, c);\n");
    bg_return_if_by_copy(out, mod, node, "r");
    rtr_helpers(out, mod, node, FALSE);
    break;
  case BG_CTOR_WITH_NEW:
    fprintf(out, "THIS() *self = calloc(1, sizeof(sizeof(THIS())));\n");
    fprintf(out, "THIS(_ctor)(self, c);\n");
    fprintf(out, "return self;\n");
    break;
  case BG_SUM_CTOR_WITH_CTOR:
    fprintf(out, "self->which__ = %s_which__;\n", replace_dots(scope_name(mod, node->scope->parent)));
    fprintf(out, "self->as__.%s = c;\n", idents_value(mod->gctx, node_ident(node->scope->parent->node)));
    break;
  case BG_SUM_CTOR_WITH_MK:
    rtr_helpers(out, mod, node, TRUE);
    fprintf(out, "THIS(_%s_ctor)(&r, c);\n", idents_value(mod->gctx, node_ident(node->scope->parent->node)));
    bg_return_if_by_copy(out, mod, node, "r");
    rtr_helpers(out, mod, node, FALSE);
    break;
  case BG_SUM_CTOR_WITH_NEW:
    fprintf(out, "THIS() *self = calloc(1, sizeof(sizeof(THIS())));\n");
    fprintf(out, "THIS(_%s_ctor)(self, c);\n", idents_value(mod->gctx, node_ident(node->scope->parent->node)));
    fprintf(out, "return self;\n");
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
    fprintf(out, "\n");
    break;
  case BG_SUM_DISPATCH:
    fprintf(out, "switch (self->which__) {\n");
    for (size_t n = 0; n < node->scope->parent->node->subs_count; ++n) {
      struct node *ch = node->scope->parent->node->subs[n];
      if (ch->which == DEFCHOICE) {
        const char *nch = idents_value(mod->gctx, node_ident(ch));
        const struct node *m = get_defchoice_member(mod, ch, node_ident(node));
        char *nm = replace_dots(scope_name(mod, m->scope));
        fprintf(out, "case THIS(_%s_which___label__): %s %s(", nch, returns_something(mod, m), nm);
        free(nm);

        for (size_t a = 0; a < c_fun_args_count(node); ++a) {
          struct node *arg = node->subs[a + IDX_FUN_FIRSTARG];
          if (a > 0) {
            fprintf(out, ", ");
          }
          fprintf(out, "%s", idents_value(mod->gctx, node_ident(arg)));
        }
        fprintf(out, ");\n");
      }
    }
    fprintf(out, "}\n");
    break;
  case BG_SUM_COPY:
    fprintf(out, "self->which__ = other->which__;\n");
    fprintf(out, "switch (self->which__) {\n");
    for (size_t n = 0; n < node->scope->parent->node->subs_count; ++n) {
      struct node *ch = node->scope->parent->node->subs[n];
      if (ch->which == DEFCHOICE) {
        const char *nch = idents_value(mod->gctx, node_ident(ch));
        const struct node *m = get_defchoice_member(mod, ch, node_ident(node));
        char *nm = replace_dots(scope_name(mod, m->scope));
        fprintf(out, "case THIS(_%s_which___label__): %s %s(&self->as__.%s, &other->as__.%s);\n",
                nch, returns_something(mod, m), nm, nch, nch);
        free(nm);
      }
    }
    fprintf(out, "}\n");
    break;
  case BG_SUM_EQUALITY_EQ:
    fprintf(out, "if (self->which__ != other->which__) { return 0; }\n");
    fprintf(out, "switch (self->which__) {\n");
    for (size_t n = 0; n < node->scope->parent->node->subs_count; ++n) {
      struct node *ch = node->scope->parent->node->subs[n];
      if (ch->which == DEFCHOICE) {
        const char *nch = idents_value(mod->gctx, node_ident(ch));
        const struct node *m = get_defchoice_member(mod, ch, node_ident(node));
        char *nm = replace_dots(scope_name(mod, m->scope));
        fprintf(out, "case THIS(_%s_which___label__): %s %s(&self->as__.%s, &other->as__.%s);\n",
                nch, returns_something(mod, m), nm, nch, nch);
        free(nm);
      }
    }
    fprintf(out, "}\nreturn 0;\n");
    break;
  case BG_SUM_EQUALITY_NE:
    fprintf(out, "if (self->which__ != other->which__) { return 1; }\n");
    fprintf(out, "switch (self->which__) {\n");
    for (size_t n = 0; n < node->scope->parent->node->subs_count; ++n) {
      struct node *ch = node->scope->parent->node->subs[n];
      if (ch->which == DEFCHOICE) {
        const char *nch = idents_value(mod->gctx, node_ident(ch));
        const struct node *m = get_defchoice_member(mod, ch, node_ident(node));
        char *nm = replace_dots(scope_name(mod, m->scope));
        fprintf(out, "case THIS(_%s_which___label__): %s %s(&self->as__.%s, &other->as__.%s);\n",
                nch, returns_something(mod, m), nm, nch, nch);
        free(nm);
      }
    }
    fprintf(out, "}\nreturn 0;\n");
    break;
  case BG_SUM_ORDER_LE:
    fprintf(out, "if (self->which__ > other->which__) { return 0; }\n");
    fprintf(out, "switch (self->which__) {\n");
    for (size_t n = 0; n < node->scope->parent->node->subs_count; ++n) {
      struct node *ch = node->scope->parent->node->subs[n];
      if (ch->which == DEFCHOICE) {
        const char *nch = idents_value(mod->gctx, node_ident(ch));
        const struct node *m = get_defchoice_member(mod, ch, node_ident(node));
        char *nm = replace_dots(scope_name(mod, m->scope));
        fprintf(out, "case THIS(_%s_which___label__): %s %s(&self->as__.%s, &other->as__.%s);\n",
                nch, returns_something(mod, m), nm, nch, nch);
        free(nm);
      }
    }
    fprintf(out, "}\nreturn 0;\n");
    break;
  case BG_SUM_ORDER_LT:
    fprintf(out, "if (self->which__ >= other->which__) { return 0; }\n");
    fprintf(out, "switch (self->which__) {\n");
    for (size_t n = 0; n < node->scope->parent->node->subs_count; ++n) {
      struct node *ch = node->scope->parent->node->subs[n];
      if (ch->which == DEFCHOICE) {
        const char *nch = idents_value(mod->gctx, node_ident(ch));
        const struct node *m = get_defchoice_member(mod, ch, node_ident(node));
        char *nm = replace_dots(scope_name(mod, m->scope));
        fprintf(out, "case THIS(_%s_which___label__): %s %s(&self->as__.%s, &other->as__.%s);\n",
                nch, returns_something(mod, m), nm, nch, nch);
        free(nm);
      }
    }
    fprintf(out, "}\n");
    fprintf(out, "}\nreturn 0;\n");
    break;
  case BG_SUM_ORDER_GT:
    fprintf(out, "if (self->which__ <= other->which__) { return 0; }\n");
    fprintf(out, "switch (self->which__) {\n");
    for (size_t n = 0; n < node->scope->parent->node->subs_count; ++n) {
      struct node *ch = node->scope->parent->node->subs[n];
      if (ch->which == DEFCHOICE) {
        const char *nch = idents_value(mod->gctx, node_ident(ch));
        const struct node *m = get_defchoice_member(mod, ch, node_ident(node));
        char *nm = replace_dots(scope_name(mod, m->scope));
        fprintf(out, "case THIS(_%s_which___label__): %s %s(&self->as__.%s, &other->as__.%s);\n",
                nch, returns_something(mod, m), nm, nch, nch);
        free(nm);
      }
    }
    fprintf(out, "}\nreturn 0;\n");
    break;
  case BG_SUM_ORDER_GE:
    fprintf(out, "if (self->which__ < other->which__) { return 0; }\n");
    fprintf(out, "switch (self->which__) {\n");
    for (size_t n = 0; n < node->scope->parent->node->subs_count; ++n) {
      struct node *ch = node->scope->parent->node->subs[n];
      if (ch->which == DEFCHOICE) {
        const char *nch = idents_value(mod->gctx, node_ident(ch));
        const struct node *m = get_defchoice_member(mod, ch, node_ident(node));
        char *nm = replace_dots(scope_name(mod, m->scope));
        fprintf(out, "case THIS(_%s_which___label__): %s %s(&self->as__.%s, &other->as__.%s);\n",
                nch, returns_something(mod, m), nm, nch, nch);
        free(nm);
      }
    }
    fprintf(out, "}\nreturn 0;\n");
    break;
  case BG_TRIVIAL_COPY_COPY_CTOR:
    fprintf(out, "memcpy(self, other, sizeof(*self));\n");
    break;
  case BG_TRIVIAL_EQUALITY_OPERATOR_EQ:
    fprintf(out, "memcmp(self, other, sizeof(*self)) == 0;\n");
    break;
  case BG_TRIVIAL_EQUALITY_OPERATOR_NE:
    fprintf(out, "memcmp(self, other, sizeof(*self)) != 0;\n");
    break;
  default:
    assert(FALSE);
    break;
  }
  if (node->scope->parent->node->which == DEFTYPE
      || node->scope->parent->node->which == DEFCHOICE) {
    fprintf(out, "#undef THIS\n");
  }
  fprintf(out, "}\n");
}

static void print_deffun(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node) {
  if (fwd == FWDTYPES || fwd == DEFTYPES) {
    return;
  }
  if (header && !node_is_export(node)) {
    return;
  }
  if (!header) {
    if (node_is_export(node) && fwd == FWDFUNS) {
      return;
    } else if (node_is_export(node) && node_is_inline(node) && fwd == DEFFUNS) {
      return;
    }
  }
  if (node_ident(node) == ID_CAST) {
    return;
  }

  if (fwd == FWDFUNS) {
    print_fun_prototype(out, header, mod, node);
    fprintf(out, ";\n");
  } else if (prototype_only(header, node)) {
    // noop
  } else if (node_toplevel_const(node)->builtingen != BG__NOT) {
    print_fun_prototype(out, header, mod, node);
    print_deffun_builtingen(out, mod, node);
  } else {
    print_fun_prototype(out, header, mod, node);

    fprintf(out, "{\n");
    if (node->scope->parent->node->which == DEFTYPE) {
      fprintf(out, "#define THIS(x) ");
      print_typ(out, mod, node->scope->parent->node->typ);
      fprintf(out, "##x\n");
    }

    const struct node *retval = node_fun_retval_const(node);
    const bool named_retval = retval->which == DEFARG;
    const bool retval_bycopy = typ_isa(mod, retval->typ, typ_lookup_builtin(mod, TBI_RETURN_BY_COPY));

    if (named_retval && retval_bycopy) {
      fprintf(out, "__attribute__((__unused__)) ");
      print_defarg(out, mod, retval, FALSE);
      fprintf(out, " = { 0 };\n");
    }

    rtr_helpers(out, mod, node, TRUE);

    const struct node *block = node->subs[node->subs_count - 1];
    print_block(out, mod, block);

    fprintf(out, "\n");

    rtr_helpers(out, mod, node, FALSE);

    if (node->scope->parent->node->which == DEFTYPE) {
      fprintf(out, "#undef THIS\n");
    }
    fprintf(out, "}\n");
  }
}

static void print_deffield(FILE *out, const struct module *mod, const struct node *node) {
  print_typ(out, mod, node->typ);
  fprintf(out, " ");
  print_deffield_name(out, mod, node);
}

static void print_delegate(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "delegate ");
  print_expr(out, mod, node->subs[0], T__CALL);

  for (size_t n = 1; n < node->subs_count; ++n) {
    fprintf(out, " ");
    print_expr(out, mod, node->subs[n], T__CALL);
  }
}

static void print_deftype_statement(FILE *out, bool header, enum forward fwd,
                                    const struct module *mod, const struct node *node,
                                    bool do_static) {
  if (do_static) {
    switch (node->which) {
    case LET:
      print_let(out, header, fwd, mod, node);
      break;
    case DELEGATE:
      if (fwd == DEFTYPES) {
        print_delegate(out, mod, node);
      }
      break;
    case INVARIANT:
      if (fwd == DEFTYPES) {
        print_invariant(out, mod, node);
      }
      break;
    case EXAMPLE:
      if (fwd == DEFTYPES) {
        print_example(out, header, mod, node);
      }
      break;
    default:
      break;
    }
  } else {
    switch (node->which) {
    case DEFFIELD:
      if (fwd == DEFTYPES) {
        print_deffield(out, mod, node);
      }
      break;
    default:
      break;
    }
  }
}

static void print_deftype_block(FILE *out, bool header, enum forward fwd, const struct module *mod,
                                const struct node *node, size_t first, bool do_static) {
  if (!do_static) {
    fprintf(out, " {\n");
  }
  for (size_t n = first; n < node->subs_count; ++n) {
    ssize_t prev_pos = ftell(out);

    const struct node *statement = node->subs[n];
    print_deftype_statement(out, header, fwd, mod, statement, do_static);

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

static void print_deftype_typedefs(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "typedef const ");
  print_deftype_name(out, mod, node);
  fprintf(out, "* _Ngen_nlang_builtins_i_ref__");
  print_deftype_name(out, mod, node);
  fprintf(out, "_genN_;\n");

  fprintf(out, "typedef _Ngen_nlang_builtins_i_ref__");
  print_deftype_name(out, mod, node);
  fprintf(out, "_genN_ _Ngen_nlang_builtins_i_nullable_ref__");
  print_deftype_name(out, mod, node);
  fprintf(out, "_genN_;\n");

  fprintf(out, "typedef ");
  print_deftype_name(out, mod, node);
  fprintf(out, "* _Ngen_nlang_builtins_i_mutable_ref__");
  print_deftype_name(out, mod, node);
  fprintf(out, "_genN_;\n");

  fprintf(out, "typedef _Ngen_nlang_builtins_i_mutable_ref__");
  print_deftype_name(out, mod, node);
  fprintf(out, "_genN_ _Ngen_nlang_builtins_i_mercurial_ref__");
  print_deftype_name(out, mod, node);
  fprintf(out, "_genN_;\n");

  fprintf(out, "typedef _Ngen_nlang_builtins_i_mutable_ref__");
  print_deftype_name(out, mod, node);
  fprintf(out, "_genN_ _Ngen_nlang_builtins_i_nullable_mutable_ref__");
  print_deftype_name(out, mod, node);
  fprintf(out, "_genN_;\n");

  fprintf(out, "typedef _Ngen_nlang_builtins_i_mercurial_ref__");
  print_deftype_name(out, mod, node);
  fprintf(out, "_genN_ _Ngen_nlang_builtins_i_nullable_mercurial_ref__");
  print_deftype_name(out, mod, node);
  fprintf(out, "_genN_;\n");
}

static void print_deftype_sum_choices_fwdtypes(FILE *out, bool header, const struct module *mod, const struct node *node) {
  if (header && !(node_is_export(node) && node_is_inline(node))) {
    return;
  }

  const struct typ *choice_typ = NULL;
  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *ch = node->subs[n];
    if (ch->which == DEFCHOICE) {
      choice_typ = ch->subs[IDX_CH_VALUE]->typ;
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
    struct node *ch = node->subs[n];
    if (ch->which != DEFCHOICE) {
      continue;
    }

    fprintf(out, "#define ");
    print_deftype_name(out, mod, node);
    fprintf(out, "_");
    print_deffield_name(out, mod, ch);
    fprintf(out, "_%s_label__ (", idents_value(mod->gctx, ID_WHICH));
    print_expr(out, mod, ch->subs[IDX_CH_VALUE], T__NOT_STATEMENT);
    fprintf(out, ")\n");

    fprintf(out, "static const ");
    print_deftype_name(out, mod, node);
    fprintf(out, "_%s", idents_value(mod->gctx, ID_WHICH_TYPE));
    fprintf(out, " ");
    print_deftype_name(out, mod, node);
    fprintf(out, "_");
    print_deffield_name(out, mod, ch);
    fprintf(out, "_%s = ", idents_value(mod->gctx, ID_WHICH));
    print_expr(out, mod, ch->subs[IDX_CH_VALUE], T__NOT_STATEMENT);
    fprintf(out, ";\n");
  }

  if (node->as.DEFTYPE.kind != DEFTYPE_SUM) {
    return;
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *ch = node->subs[n];
    if (ch->which != DEFCHOICE) {
      continue;
    }

    fprintf(out, "typedef ");
    print_typ(out, mod, ch->subs[IDX_CH_PAYLOAD]->typ);
    fprintf(out, " ");
    print_deftype_name(out, mod, node);
    fprintf(out, "_");
    print_deffield_name(out, mod, ch);
    fprintf(out, ";\n");
  }
}

static void print_deftype_sum_choices_deftypes(FILE *out, bool header, const struct module *mod, const struct node *node) {
  if (header && !(node_is_export(node) && node_is_inline(node))) {
    return;
  }

  fprintf(out, "union ");
  print_deftype_name(out, mod, node);
  fprintf(out, "_%s {\n", idents_value(mod->gctx, ID_AS_TYPE));
  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *ch = node->subs[n];
    if (ch->which != DEFCHOICE) {
      continue;
    }

    print_deftype_name(out, mod, node);
    fprintf(out, "_");
    print_deffield_name(out, mod, ch);
    fprintf(out, " ");
    print_deffield_name(out, mod, ch);
    fprintf(out, ";\n");
  }
  fprintf(out, "};\n");

  fprintf(out, "typedef union ");
  print_deftype_name(out, mod, node);
  fprintf(out, "_%s ", idents_value(mod->gctx, ID_AS_TYPE));
  print_deftype_name(out, mod, node);
  fprintf(out, "_%s;\n", idents_value(mod->gctx, ID_AS_TYPE));
}

static void print_deftype_sum_members(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node) {
  if (header && !(node_is_export(node) && node_is_inline(node))) {
    return;
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *s = node->subs[n];
    if (s->which != DEFCHOICE) {
      continue;
    }

    for (size_t m = 0; m < s->subs_count; ++m) {
      struct node *cm = s->subs[m];
      if (cm->which == DEFFUN || cm->which == DEFMETHOD) {
        print_deffun(out, header, fwd, mod, cm);
      }
    }
  }
}

static void print_deftype_enum(FILE *out, bool header, enum forward fwd,
                               const struct module *mod, const struct node *node) {
  if (fwd == FWDTYPES) {
    fprintf(out, "typedef ");
    print_typ(out, mod, node->as.DEFTYPE.choice_typ);
    fprintf(out, " ");
    print_deftype_name(out, mod, node);
    fprintf(out, ";\n");

    print_deftype_typedefs(out, mod, node);
  }

  print_deftype_block(out, header, fwd, mod, node, 2, TRUE);

  if (fwd == DEFTYPES) {
    if (!prototype_only(header, node)) {
      for (size_t n = 0; n < node->subs_count; ++n) {
        struct node *s = node->subs[n];
        if (s->which != DEFCHOICE) {
          continue;
        }

        fprintf(out, "#define ");
        print_deftype_name(out, mod, node);
        fprintf(out, "_");
        print_deffield_name(out, mod, s);
        fprintf(out, "_label__ (");
        print_expr(out, mod, s->subs[IDX_CH_VALUE], T__NOT_STATEMENT);
        fprintf(out, ")\n");

        fprintf(out, "static const ");
        print_deftype_name(out, mod, node);
        fprintf(out, " ");
        print_deftype_name(out, mod, node);
        fprintf(out, "_");
        print_deffield_name(out, mod, s);
        fprintf(out, " = ");
        print_expr(out, mod, s->subs[IDX_CH_VALUE], T__NOT_STATEMENT);
        fprintf(out, ";\n");
      }
    }
  }
}

static bool is_pseudo_tbi(const struct module *mod, const struct typ *t) {
  if (typ_equal(mod, t, typ_lookup_builtin(mod, TBI_LITERALS_NULL))
      || typ_equal(mod, t, typ_lookup_builtin(mod, TBI_LITERALS_INTEGER))
      || typ_equal(mod, t, typ_lookup_builtin(mod, TBI_LITERALS_BOOLEAN))
      || typ_equal(mod, t, typ_lookup_builtin(mod, TBI_LITERALS_FLOATING))
      || typ_equal(mod, t, typ_lookup_builtin(mod, TBI__PENDING_DESTRUCT))
      || typ_equal(mod, t, typ_lookup_builtin(mod, TBI__NOT_TYPEABLE))
      || typ_equal(mod, t, typ_lookup_builtin(mod, TBI__CALL_FUNCTION_SLOT))
      || typ_equal(mod, t, typ_lookup_builtin(mod, TBI__MUTABLE))
      || typ_equal(mod, t, typ_lookup_builtin(mod, TBI__MERCURIAL))
      || typ_equal(mod, t, typ_lookup_builtin(mod, TBI_PSEUDO_TUPLE))) {
    return TRUE;
  }
  return FALSE;
}

static void print_deftype(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node) {
  if (header && !node_is_export(node)) {
    return;
  }
  if (!header) {
    if (node_is_export(node) && fwd == FWDTYPES) {
      return;
    } else if (node_is_export(node) && node_is_inline(node) && fwd == DEFTYPES) {
      return;
    }
  }

  if (typ_is_builtin(mod, node->typ) || is_pseudo_tbi(mod, node->typ)) {
    if (!is_pseudo_tbi(mod, node->typ)) {
      if (fwd == FWDTYPES) {
        print_deftype_typedefs(out, mod, node);
      }
    }
    return;
  }

  if (node->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    print_deftype_enum(out, header, fwd, mod, node);
    return;
  }

  if (fwd == FWDTYPES) {
    fprintf(out, "struct ");
    print_deftype_name(out, mod, node);
    fprintf(out, ";\n");
    fprintf(out, "typedef struct ");
    print_deftype_name(out, mod, node);
    fprintf(out, " ");
    print_deftype_name(out, mod, node);
    fprintf(out, ";\n");

    print_deftype_typedefs(out, mod, node);

    if (node->as.DEFTYPE.kind == DEFTYPE_SUM) {
      print_deftype_sum_members(out, header, fwd, mod, node);
      print_deftype_sum_choices_fwdtypes(out, header, mod, node);
    }

    print_deftype_block(out, header, fwd, mod, node, 2, TRUE);

  } else if (fwd == FWDFUNS || fwd == DEFFUNS) {
    if (node->as.DEFTYPE.kind == DEFTYPE_SUM) {
      print_deftype_sum_members(out, header, fwd, mod, node);
    }

  } else if (fwd == DEFTYPES) {
    if (node->as.DEFTYPE.kind == DEFTYPE_SUM) {
      print_deftype_sum_choices_deftypes(out, header, mod, node);
    }

    print_deftype_block(out, header, fwd, mod, node, 2, TRUE);

    if (!prototype_only(header, node)) {
      fprintf(out, "struct ");
      print_deftype_name(out, mod, node);

      print_deftype_block(out, header, fwd, mod, node, 2, FALSE);

      fprintf(out, ";\n");
    }
  }
}

static void print_import(FILE *out, bool header, const struct module *mod, const struct node *node) {
  struct node *target = NULL;
  error e = scope_lookup(&target, mod, mod->gctx->modules_root.scope, node->subs[0]);
  assert(!e);
  if (target->which == MODULE) {
    fprintf(out, "#include \"%s.h.out\"", target->as.MODULE.mod->filename);
  }
}

static bool file_exists(const char *base, const char *postfix) {
  char *fn = calloc(strlen(base) + strlen(postfix) + 1, sizeof(char));
  strcpy(fn, base);
  strcpy(fn + strlen(base), postfix);
  FILE *f = fopen(fn, "r");
  const bool r = f != NULL;
  if (f != NULL) {
    fclose(f);
  }
  free(fn);
  return r;
}

static void print_top(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node) {
  const struct toplevel *toplevel = node_toplevel_const(node);
  if (node_can_have_genargs(node)
      && node->subs[IDX_GENARGS]->subs_count > 0
      && node->subs[IDX_GENARGS]->subs[0]->which == DEFGENARG) {

    if (toplevel->instances_count > 1) {
      for (size_t n = 1; n < toplevel->instances_count; ++n) {
        const struct node *instance = toplevel->instances[n];
        if (typ_is_abstract_instance(mod, instance->typ)) {
          continue;
        }
        print_top(out, header, fwd, mod, instance);

        if (instance->which == DEFTYPE) {
          for (size_t n = 0; n < instance->as.DEFTYPE.members_count; ++n) {
            print_top(out, header, fwd, mod, instance->as.DEFTYPE.members[n]);
          }
        }
      }
    }

    return;
  }

  switch (node->which) {
  case DEFFUN:
  case DEFMETHOD:
    if (toplevel->scope_name != 0
        && node_toplevel_const(node->scope->parent->node)->instances_count >= 1) {
      // noop
    } else {
      print_deffun(out, header, fwd, mod, node);
    }
    break;
  case DEFTYPE:
    print_deftype(out, header, fwd, mod, node);
    break;
  case DEFINTF:
    // noop
    break;
  case LET:
    print_let(out, header, fwd, mod, node);
    break;
  case IMPORT:
    if (fwd == FWDTYPES) {
      print_import(out, header, mod, node);
    }
    break;
  default:
    fprintf(stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_module(FILE *out, bool header, const struct module *mod) {
  if (header) {
    const char *guard = replace_dots(scope_name(mod, mod->root->scope));
    fprintf(out, "#ifndef __%s\n#define __%s\n\n", guard, guard);
  }

  fprintf(out, "#include <lib/nlang/runtime.h>\n");

  const struct node *top = mod->body;

  size_t n;
  for (n = 0; n < top->subs_count; ++n) {
    const struct node *node = top->subs[n];
    if (node->which != IMPORT) {
      break;
    }
    print_top(out, header, FWDTYPES, mod, node);
    fprintf(out, "\n");
  }
  const size_t first_non_import = n;

  if (header) {
    if (file_exists(mod->filename, ".h")) {
      fprintf(out, "#include \"%s.h\"\n", mod->filename);
    }
  } else {
    if (file_exists(mod->filename, ".c")) {
      fprintf(out, "#include \"%s.c\"\n", mod->filename);
    }
    fprintf(out, "#include \"%s.h.out\"\n", mod->filename);
  }

  enum forward fwd_passes[] = { FWDTYPES, DEFTYPES, FWDFUNS, DEFFUNS };
  for (int i = 0; i < ARRAY_SIZE(fwd_passes); ++i) {
    enum forward fwd = fwd_passes[i];
    for (size_t n = first_non_import; n < top->subs_count; ++n) {
      const struct node *node = top->subs[n];
      print_top(out, header, fwd, mod, node);

      if (n < top->subs_count - 1) {
        fprintf(out, "\n");
      }
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
