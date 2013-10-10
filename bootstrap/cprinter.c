#ifndef _POSIX_SOURCE
# define _POSIX_SOURCE
#endif
#include <stdio.h>

#include "printer.h"

enum forward {
  FWD_DECLARE_TYPES,
  FWD_DEFINE_TYPES,
  FWD_DECLARE_FUNCTIONS,
  FWD_DEFINE_FUNCTIONS,
  FORWARD__NUM,
};

static const char *forward_guards[FORWARD__NUM] = {
  [FWD_DECLARE_TYPES] = "NLANG_DECLARE_TYPES",
  [FWD_DEFINE_TYPES] = "NLANG_DEFINE_TYPES",
  [FWD_DECLARE_FUNCTIONS] = "NLANG_DECLARE_FUNCTIONS",
  [FWD_DEFINE_FUNCTIONS] = "NLANG_DEFINE_FUNCTIONS",
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
  [TLSBRA] = "[",
  [TRSBRA] = "]",
  [TLCBRA] = "{",
  [TRCBRA] = "}",
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
static void print_block(FILE *out, const struct module *mod, const struct node *node, bool no_braces);
static void print_typ(FILE *out, const struct module *mod, const struct typ *typ);
static void print_typeconstraint(FILE *out, const struct module *mod, const struct node *node);
static void print_ident(FILE *out, const struct module *mod, const struct node *node);
static void print_statement(FILE *out, const struct module *mod, const struct node *node);
static void print_let(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node);
static void print_defpattern(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node);

static void print_pattern(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, node, T__STATEMENT);
}

static void print_bin_sym(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.BIN.operator;
  const struct node *right = node->subs[1];
  if (OP_ASSIGN(op)
      && (right->which == INIT
          || (right->which == CALL
              && !typ_isa(mod, right->typ, TBI_RETURN_BY_COPY)))) {
    print_expr(out, mod, right, T__STATEMENT);
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

  if (!OP_ASSIGN(op)) {
    fprintf(out, "(");
  }

  switch (OP_KIND(op)) {
  case OP_BIN:
  case OP_BIN_SYM:
  case OP_BIN_SYM_BOOL:
  case OP_BIN_SYM_NUM:
  case OP_BIN_SYM_PTR:
  case OP_BIN_NUM_RHS_UNSIGNED:
    print_bin_sym(out, mod, node, parent_op);
    break;
  case OP_BIN_ACC:
    print_bin_acc(out, mod, node, parent_op);
    break;
  case OP_BIN_RHS_TYPE:
    print_typeconstraint(out, mod, node);
    break;
  }

  if (!OP_ASSIGN(op)) {
    fprintf(out, ")");
  }
}

static void print_un(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.UN.operator;

  fprintf(out, "(");

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

  fprintf(out, ")");
}

static void print_tuple(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  if (node->flags & NODE_IS_TYPE) {
    print_typ(out, mod, node->typ);
    return;
  }

  fprintf(out, "(");
  print_typ(out, mod, node->typ);
  fprintf(out, "){");
  for (size_t n = 0; n < node->subs_count; ++n) {
    if (n > 0) {
      fprintf(out, ", ");
    }
    print_expr(out, mod, node->subs[n], TCOMMA);
  }
  fprintf(out, "}");
}

static void print_tuplenth(FILE *out, const struct module *mod, const struct node *node) {
  const struct node *parent = node_parent_const(node);
  assert(parent->which == TUPLEEXTRACT);
  const struct node *target = parent->subs[parent->subs_count - 1];
  assert(target != node);
  fprintf(out, "((");
  print_expr(out, mod, target, T__STATEMENT);
  fprintf(out, ")%sx%zu",
          typ_is_reference_instance(mod, target->typ) ? "->" : ".",
          node->as.TUPLENTH.nth);
  fprintf(out, ")");
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
  const struct node *parentd = node_parent_const(ftyp->definition);
  print_typ(out, mod, ftyp);
  fprintf(out, "(");

  bool force_comma = FALSE;
  if (ftyp->definition->which == DEFFUN
      && parentd->which == DEFINTF) {
    fprintf(out, "*(");
    print_typ(out, mod, node->subs[0]->subs[0]->typ);
    fprintf(out, " *)&");
    print_expr(out, mod, node->subs[0]->subs[0], T__CALL);
    force_comma = TRUE;
  }

  size_t n;
  for (n = 0; n < c_fun_args_count(ftyp->definition); ++n) {
    if (force_comma || n > 0) {
      fprintf(out, ", ");
    }
    const struct node *arg = node->subs[1 + n];
    if (n == 0
        && ftyp->definition->which == DEFMETHOD
        && parentd->which == DEFINTF) {
      assert(typ_is_reference_instance(mod, arg->typ));
      if (!typ_equal(mod, parentd->typ, arg->typ->gen_args[1])) {
        fprintf(out, "*(");
        print_typ(out, mod, parentd->typ);
        fprintf(out, " *)&");
      }
    }
    print_expr(out, mod, arg, T__CALL);
  }

  const bool retval_throughref = !typ_isa_return_by_copy(mod, node->typ);
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

static void print_init_array(FILE *out, const struct module *mod, const struct node *node) {
  const struct node *parent = node_parent_const(node);
  if (parent->which == BIN && OP_ASSIGN(parent->as.BIN.operator)) {
    const struct node *target = node->as.INIT.target_expr;
    print_expr(out, mod, target, T__STATEMENT);
  }
  fprintf(out, "= ");

  if (node->subs_count == 1) {
    fprintf(out, "{ 0 }");
    return;
  }

  assert(typ_isa(mod, node->subs[1]->typ, TBI_TRIVIAL_COPY) && "not yet supported");

  fprintf(out, "(const ");
  print_typ(out, mod, node->typ);
  fprintf(out, "){ (");
  print_typ(out, mod, node->subs[1]->typ);
  fprintf(out, "[]){ ");

  for (size_t n = 0; n < node->subs_count; n += 1) {
    print_expr(out, mod, node->subs[n], T__NOT_STATEMENT);
    fprintf(out, ", ");
  }
  fprintf(out, " }, %zu }\n", node->subs_count);
}

static void print_init_toplevel(FILE *out, const struct module *mod, const struct node *node) {
  if (node->subs_count == 0) {
    fprintf(out, " = { 0 }");
    return;
  }

  fprintf(out, " = {\n");
  for (size_t n = 0; n < node->subs_count; n += 2) {
    fprintf(out, ".%s = ",
            idents_value(mod->gctx, node_ident(node->subs[n])));
    print_expr(out, mod, node->subs[n + 1], T__NOT_STATEMENT);
    fprintf(out, ",\n");
  }
  fprintf(out, " }\n");
}

static void print_init(FILE *out, const struct module *mod, const struct node *node) {
  if (node->as.INIT.is_array) {
    print_init_array(out, mod, node);
    return;
  }

  if (node->scope->parent->parent->parent->node->which == LET) {
    switch (node->scope->parent->parent->parent->parent->node->which) {
    case MODULE_BODY:
    case DEFTYPE:
      print_init_toplevel(out, mod, node);
      return;
    default:
      break;
    }
  }

  if (node_parent_const(node)->which == DEFPATTERN) {
    fprintf(out, "= { 0 };\n");
  }

  const struct node *target = node->as.INIT.target_expr;

  for (size_t n = 0; n < node->subs_count; n += 2) {
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
  enum node_which parent_which = node_parent_const(def)->which;
  *is_local = parent_which != MODULE && parent_which != DEFTYPE && parent_which != DEFINTF;
  return 0;
}

static void print_deftype_name(FILE *out, const struct module *mod, const struct node *node) {
  print_typ(out, mod, node->typ);
}

static void print_deffun_name(FILE *out, const struct module *mod, const struct node *node,
                              const struct typ *intf_final_typ) {
  const ident id = node_ident(node);
  if (id == ID_MAIN) {
    fprintf(out, "_Nmain");
  } else if (intf_final_typ != NULL) {
    print_typ(out, mod, intf_final_typ);
    fprintf(out, "_%s", idents_value(mod->gctx, node_ident(node)));
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

static void print_dyn(FILE *out, const struct module *mod, const struct node *node) {
  const struct typ *intf = node->typ->gen_args[1];
  const struct typ *concrete = node->subs[0]->typ->gen_args[1];

  print_typ(out, mod, concrete);
  fprintf(out, "_mkdyn__");
  print_typ(out, mod, intf);
  fprintf(out, "((void *)");
  print_expr(out, mod, node->subs[0], T__CALL);
  fprintf(out, ")");
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
      if (typ_equal(mod, node->typ, TBI_STATIC_STRING)) {
        fprintf(out, "nlang_chars_static_string_mk((const nlang_builtins_u8 *)\"%s\", sizeof(\"%s\")-1)", s, s);
      } else if (typ_equal(mod, node->typ, TBI_CHAR)) {
        fprintf(out, "nlang_chars_char_from_ascii('%s')", s);
      } else {
        assert(FALSE);
      }
      free(s);
    }
    break;
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
  case TUPLEEXTRACT:
    print_expr(out, mod, node->subs[node->subs_count - 1], parent_op);
    break;
  case TUPLENTH:
    print_tuplenth(out, mod, node);
    break;
  case INIT:
    print_init(out, mod, node);
    break;
  case DYN:
    print_dyn(out, mod, node);
    break;
  case BLOCK:
    fprintf(out, "({ ");
    print_block(out, mod, node, TRUE);
    fprintf(out, "; })");
    break;
  case IF:
  case TRY:
  case MATCH:
  case LET:
    fprintf(out, "({ ");
    print_statement(out, mod, node);
    fprintf(out, "; })");
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
  print_block(out, mod, node->subs[1], FALSE);
}

static void print_if(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "if (");
  print_expr(out, mod, node->subs[0], T__STATEMENT);
  fprintf(out, ")");
  print_block(out, mod, node->subs[1], FALSE);

  size_t p = 2;
  size_t br_count = node->subs_count - 2;
  while (br_count >= 2) {
    fprintf(out, "\n");
    fprintf(out, "else if (");
    print_expr(out, mod, node->subs[p], T__STATEMENT);
    fprintf(out, ") ");
    print_block(out, mod, node->subs[p+1], FALSE);
    p += 2;
    br_count -= 2;
  }

  if (br_count == 1) {
    fprintf(out, "\n");
    fprintf(out, "else ");
    print_block(out, mod, node->subs[p], FALSE);
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

    print_block(out, mod, node->subs[n + 1], FALSE);
    fprintf(out, "\n");
  }
  fprintf(out, "}");
}

static void print_try(FILE *out, const struct module *mod, const struct node *node) {
  const struct node *elet = node->subs[0];
  const struct node *edefp = elet->subs[0];
  const struct node *eblock = elet->subs[1];

  print_defpattern(out, FALSE, FWD_DEFINE_FUNCTIONS, mod, edefp);
  print_block(out, mod, eblock->subs[0], FALSE);

  for (size_t n = 1; n < eblock->subs_count; ++n) {
    struct node *catch = eblock->subs[n];

    fprintf(out, "\n%s: {\n", idents_value(mod->gctx, catch->as.CATCH.label));

    print_let(out, FALSE, FWD_DEFINE_FUNCTIONS, mod, catch->subs[0]);
    print_block(out, mod, catch->subs[1], FALSE);

    fprintf(out, "\n}\n");
  }
}

static void print_pre(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "nlang_builtins_assert(");
  print_expr(out, mod, node->subs[0], T__CALL);
  fprintf(out, ")");
}

static void print_post(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "nlang_builtins_assert(");
  print_expr(out, mod, node->subs[0], T__CALL);
  fprintf(out, ")");
}

static void print_invariant(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "nlang_builtins_assert(");
  print_expr(out, mod, node->subs[0], T__CALL);
  fprintf(out, ")");
}

#define ATTR_SECTION_EXAMPLES "__attribute__((section(\".text.nlang.examples\")))"

static void print_example(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node) {
  if (header) {
    return;
  }
  if (fwd == FWD_DECLARE_FUNCTIONS) {
    fprintf(out, "void %s__Nexample%zu(void) " ATTR_SECTION_EXAMPLES ";",
            replace_dots(scope_name(mod, mod->root->scope)), node->as.EXAMPLE.name);
  } else if (fwd == FWD_DEFINE_FUNCTIONS) {
    fprintf(out, "void %s__Nexample%zu(void) {\n",
            replace_dots(scope_name(mod, mod->root->scope)), node->as.EXAMPLE.name);
    const struct node *block = node->subs[0];
    fprintf(out, "nlang_builtins_assert(");
    print_expr(out, mod, block, T__STATEMENT);
    fprintf(out, ");\n}");
  }
}

static void print_toplevel(FILE *out, bool header, const struct node *node) {
  if (header && !node_is_export(node)) {
    return;
  }

  const struct toplevel *toplevel = node_toplevel_const(node);
  if ((node->which == DEFFUN || node->which == DEFMETHOD)
    && toplevel->is_extern && toplevel->is_inline) {
    fprintf(out, "static inline ");
  } else if (toplevel->is_extern) {
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
    if (typ_is_reference_instance(mod, typ)
        && typ->gen_args[1]->definition->which == DEFINTF) {
      print_typ(out, mod, typ->gen_args[1]);
      break;
    }

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
      const struct node *parent = node_parent_const(typ->definition);
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
  default:
    assert(FALSE);
  }
}

static void print_defname_excep(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "%s = ", idents_value(mod->gctx, node->as.DEFNAME.excep_error));
  print_expr(out, mod, node->as.DEFNAME.pattern, TASSIGN);
  fprintf(out, "; if (");
  print_expr(out, mod, node->subs[IDX_DEFNAME_EXCEP_TEST], T__CALL);
  fprintf(out, ") { goto %s; }", idents_value(mod->gctx, node->as.DEFNAME.excep_label));
}

static const struct node *significant_expr_or_null(const struct node *node) {
  if (node == NULL) {
    return NULL;
  }

  while (node->which == TYPECONSTRAINT) {
    node = node->subs[0];
  }
  return node;
}

static void print_defname(FILE *out, bool header, enum forward fwd,
                          const struct module *mod, const struct node *node,
                          const struct node *pattern) {
  assert(node->which == DEFNAME);
  if (fwd == FWD_DECLARE_TYPES) {
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
    if (node_ident(node) == ID_OTHERWISE
        || node->as.DEFNAME.pattern->which == EXCEP) {
      return;
    }

    print_toplevel(out, header, node->scope->parent->parent->node);

    print_typ(out, mod, node->typ);
    fprintf(out, " ");
    print_pattern(out, mod, node->as.DEFNAME.pattern);

    if (fwd == FWD_DEFINE_FUNCTIONS && (!header || node_is_inline(pattern))) {
      const struct node *expr = significant_expr_or_null(node->as.DEFNAME.expr);
      if (expr != NULL) {
        if (expr->which == INIT) {
          print_init(out, mod, expr);
        } else if (expr->which == CALL
                   && !typ_isa(mod, expr->typ, TBI_RETURN_BY_COPY)) {
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
  const struct node *let = node_parent_const(node);
  if (node_is_at_top(let) && header && !node_is_export(let)) {
    return;
  }

  bool defname_to_subexpr = FALSE;
  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *d = node->subs[n];
    if (d->which != DEFNAME) {
      continue;
    }

    defname_to_subexpr |= d->as.DEFNAME.expr != NULL
      && d->as.DEFNAME.expr->which == TUPLENTH;
  }

  if (defname_to_subexpr
      || node_ident(node->subs[0]) == ID_OTHERWISE) {
    fprintf(out, "(void) (");
    print_expr(out, mod, node->subs[1], T__STATEMENT);
    fprintf(out, ");\n");
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *d = node->subs[n];
    if (d->which != DEFNAME) {
      continue;
    }

    print_defname(out, header, fwd, mod, d, node);

    if (d->as.DEFNAME.is_excep) {
      print_defname_excep(out, mod, d);
    }
  }
}

static void print_let(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node) {
  const size_t last_def = node_has_tail_block(node) ? node->subs_count-2 : node->subs_count-1;

  for (size_t i = 0; i <= last_def; ++i) {
    // FIXME: not handling multiple definitions chained with 'and' that
    // refer to each others in their expressions.
    print_defpattern(out, header, fwd, mod, node->subs[i]);
  }

  if (fwd == FWD_DEFINE_FUNCTIONS && last_def != node->subs_count-1) {
    assert(!node_is_inline(node));
    struct node *block = node->subs[node->subs_count-1];
    print_block(out, mod, block, TRUE);
  }
}

static void print_return(FILE *out, const struct module *mod, const struct node *node) {
  if (node->subs_count > 0) {
    const struct node *expr = node->subs[0];
    if (typ_isa(mod, node->typ, TBI_RETURN_BY_COPY)) {
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
  case NOOP:
    fprintf(out, ";");
    break;
  case SPIT:
    fprintf(out, "%s = ", idents_value(mod->gctx, node->as.SPIT.error));
    print_expr(out, mod, node->subs_count == 1 ? node->subs[0] : node->subs[1], TASSIGN);
    fprintf(out, "; goto %s", idents_value(mod->gctx, node->as.SPIT.label));
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
  case LET:
    print_let(out, FALSE, FWD_DEFINE_FUNCTIONS, mod, node);
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
  case BLOCK:
    print_block(out, mod, node, FALSE);
    break;
  default:
    fprintf(stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_block(FILE *out, const struct module *mod, const struct node *node, bool no_braces) {
  assert(node->which == BLOCK);
  if (!no_braces) {
    fprintf(out, " {\n");
  }
  for (size_t n = 0; n < node->subs_count; ++n) {
    const struct node *statement = node->subs[n];
    print_statement(out, mod, statement);
    fprintf(out, ";\n");
  }
  if (!no_braces) {
    fprintf(out, "}");
  }
}

static void print_typeconstraint(FILE *out, const struct module *mod, const struct node *node) {
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
                                const struct node *node,
                                bool as_fun_pointer, bool named_fun_pointer, bool as_dyn_fun_pointer,
                                const struct typ *intf_final_typ) {
  assert(!as_fun_pointer || (named_fun_pointer || as_dyn_fun_pointer));

  const size_t args_count = c_fun_args_count(node);
  const struct node *retval = node_fun_retval_const(node);
  const bool retval_throughref = !typ_isa_return_by_copy(mod, retval->typ);

  if (!as_fun_pointer) {
    print_toplevel(out, header, node);
  }

  if (retval_throughref) {
    fprintf(out, "void");
  } else {
    print_typ(out, mod, retval->typ);
  }

  fprintf(out, " ");
  if (as_fun_pointer) {
    fprintf(out, "(*");
    if (named_fun_pointer) {
      fprintf(out, "%s", idents_value(mod->gctx, node_ident(node)));
    }
    fprintf(out, ")");
  } else {
    print_deffun_name(out, mod, node, intf_final_typ);
  }
  fprintf(out, "(");

  bool no_args_at_all = TRUE;
  bool force_comma = FALSE;

  if (node->which == DEFFUN && intf_final_typ != NULL) {
    print_typ(out, mod, intf_final_typ);
    fprintf(out, " self");
    no_args_at_all = FALSE;
    force_comma = TRUE;
  }

  size_t n;
  for (n = 0; n < args_count; ++n) {
    no_args_at_all = FALSE;
    if (force_comma || n > 0) {
      fprintf(out, ", ");
    }

    if (n == 0 && node->which == DEFMETHOD && as_dyn_fun_pointer) {
      fprintf(out, "void *self");
      continue;
    }

    const struct node *arg = node->subs[IDX_FUN_FIRSTARG + n];
    print_defarg(out, mod, arg, FALSE);
  }

  if (retval_throughref) {
    no_args_at_all = FALSE;
    if (force_comma || n > 0) {
      fprintf(out, ", ");
    }

    print_defarg(out, mod, retval, TRUE);
  }

  if (no_args_at_all) {
    fprintf(out, "void");
  }

  fprintf(out, ")");
}

static bool prototype_only(bool header, const struct node *node) {
  if (node->which == DEFINTF) {
    return (header && !node_is_export(node))
      || node_is_prototype(node);
  } else {
    return (header && !(node_is_export(node) && node_is_inline(node)))
      || node_is_prototype(node);
  }
}

static const struct node *get_defchoice_member(const struct module *mod,
                                               const struct node *ch,
                                               ident member_id) {
  assert(ch->which == DEFCHOICE);
  struct node *m = NULL;
  error e = scope_lookup_ident_immediate(
    &m, ch, mod,
    ch->subs[IDX_CH_PAYLOAD]->typ->definition->scope, member_id, FALSE);
  assert(!e);
  return m;
}

static const char *returns_something(const struct module *mod, const struct node *m) {
  return node_fun_retval_const(m)->typ == TBI_VOID ? "" : "return";
}

static const struct node *parent_in_tuple(const struct node *n) {
  const struct node *p = node_parent_const(n);
  if (p->which == DEFARG) {
    return parent_in_tuple(p);
  } else if (p->which == TUPLE) {
    return n;
  } else {
    return NULL;
  }
}

static void print_rtr_helpers_tuple_accessor(FILE *out, const struct module *mod,
                                             const struct node *retval) {
  if (retval->which == DEFARG) {
    print_rtr_helpers_tuple_accessor(out, mod, retval->subs[1]);
    return;
  }

  const struct node *p = parent_in_tuple(retval);

  if (parent_in_tuple(p) != NULL) {
    print_rtr_helpers_tuple_accessor(out, mod, p);
  }

  const size_t where = rew_find_subnode_in_parent(node_parent_const(p), p);
  fprintf(out, ".x%zu", where);
}

static void print_rtr_helpers_fully_named_tuple(FILE *out,
                                                const struct module *mod,
                                                const struct node *retval) {
  if (retval->which == DEFARG) {
    print_expr(out, mod, retval->subs[0], TCOMMA);
  } else if (retval->which == TUPLE) {
    fprintf(out, "(");
    print_typ(out, mod, retval->typ);
    fprintf(out, "){");
    for (size_t n = 0; n < retval->subs_count; ++n) {
      if (n > 0) {
        fprintf(out, ", ");
      }

      print_rtr_helpers_fully_named_tuple(out, mod, retval->subs[n]);
    }
    fprintf(out, "}");
  } else {
    assert(FALSE && "in a function returning a tuple,"
           " either all return values are named, or none are");
  }
}

static void print_rtr_helpers(FILE *out, const struct module *mod,
                              const struct node *retval, bool start) {
  const bool named_retval = retval->which == DEFARG;
  const bool bycopy = typ_isa(mod, retval->typ, TBI_RETURN_BY_COPY);

  const bool parent_tuple = node_parent_const(retval)->which == TUPLE;
  const bool is_tuple = retval->which == TUPLE
    || (named_retval && retval->subs[1]->which == TUPLE);

  if (start) {
    if (named_retval) {
      if (bycopy) {
        fprintf(out, "__attribute__((__unused__)) ");
        print_defarg(out, mod, retval, FALSE);
        fprintf(out, " = { 0 };\n");
      } else {
        fprintf(out, "#define ");
        print_expr(out, mod, retval->subs[0], T__STATEMENT);
        fprintf(out, " (*_nrtr_");
        print_expr(out, mod, retval->subs[0], T__STATEMENT);
        if (parent_tuple) {
          print_rtr_helpers_tuple_accessor(out, mod, retval);
        }
        fprintf(out, ")\n");
      }
    }
  } else {
    if (named_retval) {
      if (bycopy) {
        fprintf(out, "return ");
        print_expr(out, mod, retval->subs[0], T__STATEMENT);
        fprintf(out, ";\n");
      } else {
        fprintf(out, "#undef ");
        print_expr(out, mod, retval->subs[0], T__STATEMENT);
        fprintf(out, "\n");
      }
    } else {
      if (bycopy && is_tuple && retval->subs[0]->which == DEFARG) {
        fprintf(out, "return ");
        print_rtr_helpers_fully_named_tuple(out, mod, retval);
        fprintf(out, ";\n");
      }
    }
  }

  if (is_tuple) {
    if (!start && bycopy) {
      return;
    }

    if (named_retval) {
      print_rtr_helpers(out, mod, retval->subs[1], start);
    } else {
      for (size_t n = 0; n < retval->subs_count; ++n) {
        print_rtr_helpers(out, mod, retval->subs[n], start);
      }
    }
  }
}

static void rtr_helpers(FILE *out, const struct module *mod,
                        const struct node *node, bool start) {
  const struct node *retval = node_fun_retval_const(node);
  print_rtr_helpers(out, mod, retval, start);
}

static void bg_return_if_by_copy(FILE *out, const struct module *mod, const struct node *node,
                                 const char *what) {
  const struct node *retval = node_fun_retval_const(node);
  const bool retval_bycopy = typ_isa(mod, retval->typ, TBI_RETURN_BY_COPY);
  if (!retval_bycopy) {
    return;
  }

  fprintf(out, "return %s;\n", what);
}

static void print_deffun_builtingen(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, " {\n");
  if (node_parent_const(node)->which == DEFTYPE
      || node_parent_const(node)->which == DEFCHOICE) {
    fprintf(out, "#define THIS(x) ");
    print_typ(out, mod, node_parent_const(node)->typ);
    fprintf(out, "##x\n");
  }

  rtr_helpers(out, mod, node, TRUE);

  switch (node_toplevel_const(node)->builtingen) {
  case BG_TRIVIAL_CTOR_CTOR:
    break;
  case BG_TRIVIAL_CTOR_MK:
    bg_return_if_by_copy(out, mod, node, "(THIS()){ 0 }");
    break;
  case BG_TRIVIAL_CTOR_NEW:
    fprintf(out, "return calloc(1, sizeof(THIS()));\n");
    break;
  case BG_DEFAULT_CTOR_MK:
    fprintf(out, "THIS(_ctor)(&r);\n");
    bg_return_if_by_copy(out, mod, node, "r");
    break;
  case BG_DEFAULT_CTOR_NEW:
    fprintf(out, "THIS() *self = calloc(1, sizeof(sizeof(THIS())));\n");
    fprintf(out, "THIS(_ctor)(self);\n");
    fprintf(out, "return self;\n");
    break;
  case BG_AUTO_MK:
    fprintf(out, "THIS(_ctor)(&r, ");
    for (size_t a = 0; a < c_fun_args_count(node); ++a) {
      struct node *arg = node->subs[a + IDX_FUN_FIRSTARG];
      if (a > 0) {
        fprintf(out, ", ");
      }
      fprintf(out, "%s", idents_value(mod->gctx, node_ident(arg)));
    }
    fprintf(out, ");\n");
    break;
  case BG_AUTO_NEW:
    fprintf(out, "THIS() *self = calloc(1, sizeof(sizeof(THIS())));\n");
    fprintf(out, "THIS(_ctor)(self, ");
    for (size_t a = 0; a < c_fun_args_count(node); ++a) {
      struct node *arg = node->subs[a + IDX_FUN_FIRSTARG];
      if (a > 0) {
        fprintf(out, ", ");
      }
      fprintf(out, "%s", idents_value(mod->gctx, node_ident(arg)));
    }
    fprintf(out, ");\n");
    fprintf(out, "return self;\n");
    break;
  case BG_CTOR_WITH_MK:
    fprintf(out, "THIS(_ctor)(&r, c);\n");
    bg_return_if_by_copy(out, mod, node, "r");
    break;
  case BG_CTOR_WITH_NEW:
    fprintf(out, "THIS() *self = calloc(1, sizeof(sizeof(THIS())));\n");
    fprintf(out, "THIS(_ctor)(self, c);\n");
    fprintf(out, "return self;\n");
    break;
  case BG_AUTO_MKV:
    fprintf(out, "THIS(_ctorv)(&r, c);\n");
    bg_return_if_by_copy(out, mod, node, "r");
    break;
  case BG_AUTO_NEWV:
    fprintf(out, "THIS() *self = calloc(1, sizeof(sizeof(THIS())));\n");
    fprintf(out, "THIS(_ctorv)(self, c);\n");
    fprintf(out, "return self;\n");
    break;
  case BG_SUM_CTOR_WITH_CTOR:
    fprintf(out, "self->which__ = %s_which__;\n", replace_dots(scope_name(mod, node->scope->parent)));
    fprintf(out, "self->as__.%s = c;\n", idents_value(mod->gctx, node_ident(node_parent_const(node))));
    break;
  case BG_SUM_CTOR_WITH_MK:
    fprintf(out, "THIS(_%s_ctor)(&r, c);\n", idents_value(mod->gctx, node_ident(node_parent_const(node))));
    bg_return_if_by_copy(out, mod, node, "r");
    break;
  case BG_SUM_CTOR_WITH_NEW:
    fprintf(out, "THIS() *self = calloc(1, sizeof(sizeof(THIS())));\n");
    fprintf(out, "THIS(_%s_ctor)(self, c);\n", idents_value(mod->gctx, node_ident(node_parent_const(node))));
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
    for (size_t n = 0; n < node_parent_const(node)->subs_count; ++n) {
      struct node *ch = node_parent_const(node)->subs[n];
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
    for (size_t n = 0; n < node_parent_const(node)->subs_count; ++n) {
      struct node *ch = node_parent_const(node)->subs[n];
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
    for (size_t n = 0; n < node_parent_const(node)->subs_count; ++n) {
      struct node *ch = node_parent_const(node)->subs[n];
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
    for (size_t n = 0; n < node_parent_const(node)->subs_count; ++n) {
      struct node *ch = node_parent_const(node)->subs[n];
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
    for (size_t n = 0; n < node_parent_const(node)->subs_count; ++n) {
      struct node *ch = node_parent_const(node)->subs[n];
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
    for (size_t n = 0; n < node_parent_const(node)->subs_count; ++n) {
      struct node *ch = node_parent_const(node)->subs[n];
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
    for (size_t n = 0; n < node_parent_const(node)->subs_count; ++n) {
      struct node *ch = node_parent_const(node)->subs[n];
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
    for (size_t n = 0; n < node_parent_const(node)->subs_count; ++n) {
      struct node *ch = node_parent_const(node)->subs[n];
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
    fprintf(out, "return memcmp(self, other, sizeof(*self)) == 0;\n");
    break;
  case BG_TRIVIAL_EQUALITY_OPERATOR_NE:
    fprintf(out, "return memcmp(self, other, sizeof(*self)) != 0;\n");
    break;
  default:
    assert(FALSE);
    break;
  }

  rtr_helpers(out, mod, node, FALSE);

  if (node_parent_const(node)->which == DEFTYPE
      || node_parent_const(node)->which == DEFCHOICE) {
    fprintf(out, "#undef THIS\n");
  }
  fprintf(out, "}\n");
}

static void print_deffun(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node) {
  if (fwd == FWD_DECLARE_TYPES || fwd == FWD_DEFINE_TYPES) {
    return;
  }
  if (header && !node_is_export(node)) {
    return;
  }
  if (!header) {
    if (node_is_export(node) && fwd == FWD_DECLARE_FUNCTIONS) {
      return;
    } else if (node_is_export(node) && node_is_inline(node) && fwd == FWD_DEFINE_FUNCTIONS) {
      return;
    }
  }
  const ident id = node_ident(node);
  if (id == ID_CAST
      || id == ID_LIKELY
      || id == ID_UNLIKELY) {
    return;
  }

  if (fwd == FWD_DECLARE_FUNCTIONS) {
    print_fun_prototype(out, header, mod, node, FALSE, FALSE, FALSE, NULL);
    fprintf(out, ";\n");
  } else if (prototype_only(header, node)) {
    // noop
  } else if (node_toplevel_const(node)->builtingen != BG__NOT) {
    print_fun_prototype(out, header, mod, node, FALSE, FALSE, FALSE, NULL);
    print_deffun_builtingen(out, mod, node);
  } else {
    print_fun_prototype(out, header, mod, node, FALSE, FALSE, FALSE, NULL);

    fprintf(out, "{\n");
    if (node_parent_const(node)->which == DEFTYPE) {
      fprintf(out, "#define THIS(x) ");
      print_typ(out, mod, node_parent_const(node)->typ);
      fprintf(out, "##x\n");
    }

    rtr_helpers(out, mod, node, TRUE);

    const struct node *block = node->subs[node->subs_count - 1];
    print_block(out, mod, block, FALSE);

    fprintf(out, "\n");

    rtr_helpers(out, mod, node, FALSE);

    if (node_parent_const(node)->which == DEFTYPE) {
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
      if (fwd == FWD_DEFINE_TYPES) {
        print_delegate(out, mod, node);
      }
      break;
    case INVARIANT:
      if (fwd == FWD_DEFINE_FUNCTIONS) {
        print_invariant(out, mod, node);
      }
      break;
    default:
      break;
    }
  } else {
    switch (node->which) {
    case DEFFIELD:
      if (fwd == FWD_DEFINE_TYPES) {
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
    print_deftype_name(out, mod, node);
    fprintf(out, "_");
    print_deffield_name(out, mod, ch);
    fprintf(out, "_%s_label__;\n", idents_value(mod->gctx, ID_WHICH));
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
  if (fwd == FWD_DECLARE_TYPES) {
    fprintf(out, "typedef ");
    print_typ(out, mod, node->as.DEFTYPE.choice_typ);
    fprintf(out, " ");
    print_deftype_name(out, mod, node);
    fprintf(out, ";\n");

    print_deftype_typedefs(out, mod, node);
  }

  print_deftype_block(out, header, fwd, mod, node, 2, TRUE);

  if (fwd == FWD_DEFINE_TYPES) {
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
        print_deftype_name(out, mod, node);
        fprintf(out, "_");
        print_deffield_name(out, mod, s);
        fprintf(out, "_label__;\n");
      }
    }
  }
}

struct printer_state {
  FILE *out;
  bool header;
  enum forward fwd;
  size_t printed;
  void *user;
};

static void print_deftype_mkdyn_proto(FILE *out, const struct module *mod,
                                      const struct node *node, const struct typ *intf) {
  fprintf(out, "static inline ");
  print_typ(out, mod, intf);
  fprintf(out, " ");
  print_typ(out, mod, node->typ);
  fprintf(out, "_mkdyn__");
  print_typ(out, mod, intf);
  fprintf(out, "(");
  print_typ(out, mod, node->typ);
  fprintf(out, " *obj");
  fprintf(out, ")");
}

static error print_deftype_mkdyn_proto_eachisalist(struct module *mod, const struct typ *t,
                                                   const struct typ *intf, void *user) {
  struct printer_state *st = user;

  print_deftype_mkdyn_proto(st->out, mod, t->definition, intf);
  fprintf(st->out, ";\n");
  return 0;
}

static error print_deftype_dyn_field_eachisalist(struct module *mod, const struct typ *ignored,
                                                 const struct typ *intf, void *user) {
  struct printer_state *st = user;
  struct typ *t = st->user;

  const struct node *dintf = intf->definition;
  for (size_t m = 0; m < dintf->subs_count; ++m) {
    const struct node *f = dintf->subs[m];
    if (f->which != DEFFUN && f->which != DEFMETHOD) {
      continue;
    }
    if (node_toplevel_const(f)->is_not_dyn) {
      continue;
    }
    if (f->typ->definition->subs[IDX_GENARGS]->subs_count != 0) {
      continue;
    }

    st->printed += 1;
    const struct node *thisf = node_get_member_const(mod, t->definition, node_ident(f));
    fprintf(st->out, ".%s = (", idents_value(mod->gctx, node_ident(thisf)));
    print_fun_prototype(st->out, st->header, mod, thisf, TRUE, FALSE, TRUE, NULL);
    fprintf(st->out, ")");
    print_typ(st->out, mod, thisf->typ);
    fprintf(st->out, ",\n");
  }

  return 0;
}

static error print_deftype_mkdyn_eachisalist(struct module *mod, const struct typ *t,
                                             const struct typ *intf, void *user) {
  struct printer_state *st = user;

  print_deftype_mkdyn_proto(st->out, mod, t->definition, intf);
  fprintf(st->out, " {\n");
  fprintf(st->out, "static const struct _Ndyn_");
  print_typ(st->out, mod, intf);
  fprintf(st->out, " vtable = {\n");

  struct printer_state st2 = *st;
  st2.printed = 0;
  st2.user = (void *)t;

  const uint32_t filter = ISALIST_FILTER_TRIVIAL_ISALIST
    | (st->header ? ISALIST_FILTER_NOT_EXPORTED : ISALIST_FILTER_EXPORTED);
  error e = typ_isalist_foreach((struct module *)mod, intf, filter,
                                print_deftype_dyn_field_eachisalist,
                                &st2);
  assert(!e);
  e = print_deftype_dyn_field_eachisalist((struct module *)mod,
                                          NULL, intf, &st2);
  assert(!e);

  if (st2.printed == 0) {
    fprintf(st->out, "0,\n");
  }
  fprintf(st->out, "};\n");

  fprintf(st->out, "return (");
  print_typ(st->out, mod, intf);
  fprintf(st->out, "){ .vptr = &vtable, .obj = obj };\n");

  fprintf(st->out, "}\n");
  return 0;
}

static void print_deftype_mkdyn(FILE *out, bool header, enum forward fwd,
                                const struct module *mod, const struct node *node) {
  const uint32_t filter = ISALIST_FILTER_TRIVIAL_ISALIST
    | (header ? ISALIST_FILTER_NOT_EXPORTED : ISALIST_FILTER_EXPORTED);
  if (fwd == FWD_DECLARE_FUNCTIONS) {
    struct printer_state st = { .out = out, .header = header, .fwd = fwd, .printed = 0, .user = NULL };
    error e = typ_isalist_foreach((struct module *)mod, node->typ, filter,
                                  print_deftype_mkdyn_proto_eachisalist,
                                  &st);
    assert(!e);
  } else if (fwd == FWD_DEFINE_FUNCTIONS) {
    struct printer_state st = { .out = out, .header = header, .fwd = fwd, .printed = 0, .user = NULL };
    error e = typ_isalist_foreach((struct module *)mod, node->typ, filter,
                                  print_deftype_mkdyn_eachisalist,
                                  &st);
    assert(!e);
  }
}

static void print_deftype(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node) {
  if (header && !node_is_export(node)) {
    return;
  }
  if (!header) {
    if (node_is_export(node) && fwd == FWD_DECLARE_TYPES) {
      return;
    } else if (node_is_export(node) && node_is_inline(node) && fwd == FWD_DEFINE_TYPES) {
      return;
    }
  }

  if (typ_is_pseudo_builtin(mod, node->typ)) {
    return;
  }
  if (typ_is_builtin(mod, node->typ) && node_toplevel_const(node)->is_extern) {
    if (fwd == FWD_DECLARE_TYPES) {
      print_deftype_typedefs(out, mod, node);
    }
    print_deftype_mkdyn(out, header, fwd, mod, node);
    return;
  }

  if (node->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    print_deftype_enum(out, header, fwd, mod, node);
    return;
  }

  if (fwd == FWD_DECLARE_TYPES) {
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

  } else if (fwd == FWD_DEFINE_TYPES) {
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

  if (fwd == FWD_DECLARE_FUNCTIONS || fwd == FWD_DEFINE_FUNCTIONS) {
    if (node->as.DEFTYPE.kind == DEFTYPE_SUM) {
      print_deftype_sum_members(out, header, fwd, mod, node);
    }
  }

  print_deftype_mkdyn(out, header, fwd, mod, node);
}

static error print_defintf_dyn_field_eachisalist(struct module *mod, const struct typ *t,
                                                 const struct typ *intf, void *user) {
  struct printer_state *st = user;

  for (size_t n = 0; n < intf->definition->subs_count; ++n) {
    const struct node *d = intf->definition->subs[n];
    if (d->which != DEFFUN && d->which != DEFMETHOD) {
      continue;
    }
    if (node_toplevel_const(d)->is_not_dyn) {
      continue;
    }
    if (d->typ->definition->subs[IDX_GENARGS]->subs_count != 0) {
      continue;
    }
    print_fun_prototype(st->out, st->header, mod, d, TRUE, TRUE, TRUE, NULL);
    fprintf(st->out, ";\n");
    st->printed += 1;
  }

  return 0;
}

static error print_defintf_member_proto_eachisalist(struct module *mod, const struct typ *t, const struct typ *intf, void *user) {
  struct printer_state *st = user;

  for (size_t n = 0; n < intf->definition->subs_count; ++n) {
    const struct node *d = intf->definition->subs[n];
    if (d->which != DEFFUN && d->which != DEFMETHOD) {
      continue;
    }
    if (node_toplevel_const(d)->is_not_dyn) {
      continue;
    }
    if (d->typ->definition->subs[IDX_GENARGS]->subs_count != 0) {
      continue;
    }
    print_fun_prototype(st->out, st->header, mod, d, FALSE, FALSE, FALSE, t);
    fprintf(st->out, ";\n");
  }
  return 0;
}

static error print_defintf_member_eachisalist(struct module *mod, const struct typ *t, const struct typ *intf, void *user) {
  struct printer_state *st = user;

  for (size_t n = 0; n < intf->definition->subs_count; ++n) {
    const struct node *d = intf->definition->subs[n];
    if (d->which != DEFFUN && d->which != DEFMETHOD) {
      continue;
    }
    if (node_toplevel_const(d)->is_not_dyn) {
      continue;
    }
    if (d->typ->definition->subs[IDX_GENARGS]->subs_count != 0) {
      continue;
    }

    print_fun_prototype(st->out, st->header, mod, d, FALSE, FALSE, FALSE, t);
    fprintf(st->out, " {\n");

    const bool retval_throughref = !typ_isa_return_by_copy(mod, node_fun_retval_const(d)->typ);
    if (!retval_throughref
        && !typ_equal(mod, node_fun_retval_const(d)->typ, TBI_VOID)) {
      fprintf(st->out, "return ");
    }
    fprintf(st->out, "self.vptr->%s(", idents_value(mod->gctx, node_ident(d)));
    bool force_comma = FALSE;
    if (d->which == DEFMETHOD) {
      force_comma = TRUE;
      fprintf(st->out, "self.obj");
    }
    for (size_t a = IDX_FUN_FIRSTARG; a < IDX_FUN_FIRSTARG + node_fun_all_args_count(d); ++a) {
      if (a == IDX_FUN_FIRSTARG && d->which == DEFMETHOD) {
        // skip self
        continue;
      }
      if (force_comma || a > IDX_FUN_FIRSTARG) {
        fprintf(st->out, ", ");
      }
      fprintf(st->out, "%s",
              idents_value(mod->gctx, node_ident(d->subs[a])));
    }

    if (retval_throughref) {
      if (n > 0) {
        fprintf(st->out, ", ");
      }

      fprintf(st->out, "_nrtr_");
      print_expr(st->out, mod, node_fun_retval_const(d)->subs[0], T__STATEMENT);
    }

    fprintf(st->out, ");\n}\n");
  }
  return 0;
}

static void print_defintf(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node) {
  if (header && !node_is_export(node)) {
    return;
  }
  if (!header) {
    if (node_is_export(node) && fwd == FWD_DECLARE_TYPES) {
      return;
    } else if (node_is_export(node) && fwd == FWD_DEFINE_TYPES) {
      return;
    }
  }

  if (typ_is_pseudo_builtin(mod, node->typ)) {
    return;
  }
  if (typ_is_reference_instance(mod, node->typ)) {
    return;
  }

  if (fwd == FWD_DECLARE_TYPES) {
    fprintf(out, "struct _Ndyn_");
    print_deftype_name(out, mod, node);
    fprintf(out, ";\n");

    fprintf(out, "struct ");
    print_deftype_name(out, mod, node);
    fprintf(out, ";\n");
    fprintf(out, "typedef struct ");
    print_deftype_name(out, mod, node);
    fprintf(out, " ");
    print_deftype_name(out, mod, node);
    fprintf(out, ";\n");

  } else if (fwd == FWD_DEFINE_TYPES) {
    if (!prototype_only(header, node)) {
      fprintf(out, "struct _Ndyn_");
      print_deftype_name(out, mod, node);
      fprintf(out, " {\n");

      struct printer_state st = {
        .out = out, .header = header, .fwd = fwd, .printed = 0,
      };
      error e = typ_isalist_foreach((struct module *)mod, node->typ, 0,
                                    print_defintf_dyn_field_eachisalist,
                                    &st);
      assert(!e);
      e = print_defintf_dyn_field_eachisalist((struct module *)mod, node->typ,
                                              node->typ, &st);
      assert(!e);

      if (st.printed == 0) {
        // Needed if all the members of the intf are *themselves* generics,
        // that form is indeed legal.
        fprintf(out, "nlang_builtins_u8 __Nfiller;\n");
      }
      fprintf(out, "};\n");

      fprintf(out, "struct ");
      print_deftype_name(out, mod, node);
      fprintf(out, " {\n");
      fprintf(out, "const struct _Ndyn_");
      print_deftype_name(out, mod, node);
      fprintf(out, " *vptr;\n");
      fprintf(out, "void *obj;\n");
      fprintf(out, "};\n");
    }
  } else if (fwd == FWD_DECLARE_FUNCTIONS) {
    struct printer_state st = { .out = out, .header = header, .fwd = fwd, .printed = 0, .user = NULL };
    error e = print_defintf_member_proto_eachisalist((struct module *)mod, node->typ,
                                                     node->typ, &st);
    assert(!e);
  } else if (fwd == FWD_DEFINE_FUNCTIONS && !header) {
    struct printer_state st = { .out = out, .header = header, .fwd = fwd, .printed = 0, .user = NULL };
    error e = print_defintf_member_eachisalist((struct module *)mod, node->typ,
                                               node->typ, &st);
    assert(!e);
  }
}

static void print_guarded_include(FILE *out, bool header, enum forward fwd,
                                  const char *filename, const char *postfix) {
  fprintf(out, "#define %s\n", forward_guards[fwd]);
  fprintf(out, "# include \"%s%s\"\n", filename, postfix);
  fprintf(out, "#undef %s\n", forward_guards[fwd]);
}

static void print_import(FILE *out, bool header, enum forward fwd,
                         const struct module *mod, const struct node *node) {
  struct node *target = NULL;
  error e = scope_lookup(&target, mod, mod->gctx->modules_root.scope, node->subs[0]);
  assert(!e);
  if (target->which == MODULE) {
    print_guarded_include(out, header, fwd, target->as.MODULE.mod->filename, ".h.out");
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
        if (typ_is_abstract_instance(mod, instance->typ)
            // It's possible for a non-concrete instance to be created,
            // because of the incremental nature of the typing unification
            // (e.g. in 'let x, y = 1, 1' the tuples will be first typed
            // nlang.literal.integer, nlang.literal.integer). Eventually
            // however, no non-concrete type must remain in use.
            || !typ_is_concrete(mod, instance->typ)) {
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
        && node_toplevel_const(node_parent_const(node))->instances_count >= 1) {
      // noop
    } else {
      print_deffun(out, header, fwd, mod, node);
    }
    break;
  case DEFTYPE:
    print_deftype(out, header, fwd, mod, node);
    break;
  case DEFINTF:
    print_defintf(out, header, fwd, mod, node);
    break;
  case LET:
    print_let(out, header, fwd, mod, node);
    break;
  case EXAMPLE:
    print_example(out, header, fwd, mod, node);
    break;
  case IMPORT:
    print_import(out, header, fwd, mod, node);
    break;
  default:
    fprintf(stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_module(FILE *out, bool header, const struct module *mod) {
  fprintf(out, "#include <lib/nlang/runtime.h>\n");

  const struct node *top = mod->body;

  size_t n;
  for (n = 0; n < top->subs_count; ++n) {
    const struct node *node = top->subs[n];
    if (node->which != IMPORT) {
      break;
    }
  }
  const size_t first_non_import = n;

  enum forward fwd_passes[] = {
    FWD_DECLARE_TYPES, FWD_DEFINE_TYPES, FWD_DECLARE_FUNCTIONS, FWD_DEFINE_FUNCTIONS };
  for (int i = 0; i < ARRAY_SIZE(fwd_passes); ++i) {
    enum forward fwd = fwd_passes[i];

    if (header) {
      fprintf(out, "#ifdef %s\n", forward_guards[fwd]);
      const char *guard = replace_dots(scope_name(mod, mod->root->scope));
      fprintf(out, "#ifndef %s__%s\n#define %s__%s\n\n",
              forward_guards[fwd], guard, forward_guards[fwd], guard);
    }

    for (size_t n = 0; n < first_non_import; ++n) {
      const struct node *node = top->subs[n];
      print_top(out, header, fwd, mod, node);

      if (n < top->subs_count - 1) {
        fprintf(out, "\n");
      }
    }

    if (header) {
      if (file_exists(mod->filename, ".h")) {
        print_guarded_include(out, header, fwd, mod->filename, ".h");
      }
    } else {
      print_guarded_include(out, header, fwd, mod->filename, ".h.out");
      if (file_exists(mod->filename, ".c")) {
        print_guarded_include(out, header, fwd, mod->filename, ".c");
      }
    }

    for (size_t n = first_non_import; n < top->subs_count; ++n) {
      const struct node *node = top->subs[n];
      print_top(out, header, fwd, mod, node);

      if (n < top->subs_count - 1) {
        fprintf(out, "\n");
      }
    }

    if (header) {
      fprintf(out, "\n#endif\n");
      fprintf(out, "#endif // %s\n", forward_guards[fwd]);
    }
  }
}

static error print_runexamples(FILE *out, const struct module *mod) {
  fprintf(out, "void %s(void) " ATTR_SECTION_EXAMPLES ";\n", printer_c_runexamples_name(mod));
  fprintf(out, "void %s(void) {\n", printer_c_runexamples_name(mod));
  for (size_t n = 0; n < mod->next_example; ++n) {
    fprintf(out, "%s__Nexample%zu();\n",
            replace_dots(scope_name(mod, mod->root->scope)), n);
  }
  fprintf(out, "}\n");
  return 0;
}

error printer_c(int fd, const struct module *mod) {
  FILE *out = fdopen(fd, "w");
  if (out == NULL) {
    EXCEPTF(errno, "Invalid output file descriptor '%d'", fd);
  }

  print_module(out, FALSE, mod);
  print_runexamples(out, mod);
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

char *printer_c_runexamples_name(const struct module *mod) {
  static const char runexamples[] = "_Nrunexamples";
  char *sc = replace_dots(scope_name(mod, mod->root->scope));
  char *r = calloc(strlen(sc) + sizeof(runexamples), sizeof(char));
  sprintf(r, "%s%s", sc, runexamples);
  free(sc);
  return r;
}
