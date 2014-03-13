#ifndef _POSIX_SOURCE
# define _POSIX_SOURCE
#endif
#include <stdio.h>

#include "printer.h"
#include "types.h"
#include "scope.h"
#include "constraints.h"

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

static void print_scope_name(FILE *out, const struct module *mod,
                             const struct scope *scope);

static void print_scope_last_name(FILE *out, const struct module *mod,
                                  const struct scope *scope) {
  const ident id = node_ident(scope_node(scope));
  const char *name = idents_value(mod->gctx, id);
  if (id == ID_ANONYMOUS) {
    return;
  }

  if (name[0] == '`') {
    fprintf(out, "_Ni_%s", name + 1);
  } else {
    fprintf(out, "%s", name);
  }
}

static void print_scope_name(FILE *out, const struct module *mod,
                             const struct scope *scope) {
  if (scope->parent->parent != NULL) {
    print_scope_name(out, mod, scope->parent);
    if (node_ident(scope_node(scope)) != ID_ANONYMOUS) {
      fprintf(out, "_");
    }
  }

  print_scope_last_name(out, mod, scope);
}

static size_t c_fun_args_count(const struct node *node) {
  return node_fun_all_args_count(node);
}

static void print_token(FILE *out, enum token_type t) {
  fprintf(out, "%s", c_token_strings[t]);
}

static void print_expr(FILE *out, const struct module *mod,
                       const struct node *node, uint32_t parent_op);
static void print_block(FILE *out, const struct module *mod,
                        const struct node *node, bool no_braces);
static void print_typ(FILE *out, const struct module *mod, const struct typ *typ);
static void print_typeconstraint(FILE *out, const struct module *mod,
                                 const struct node *node);
static void print_ident(FILE *out, const struct module *mod,
                        const struct node *node);
static void print_statement(FILE *out, const struct module *mod,
                            const struct node *node);
static void print_let(FILE *out, bool header, enum forward fwd,
                      const struct module *mod, const struct node *node);
static void print_defpattern(FILE *out, bool header, enum forward fwd,
                             const struct module *mod, const struct node *node);
static void print_top(FILE *out, bool header, enum forward fwd,
                      const struct module *mod, const struct node *node,
                      struct typset *printed);
static void print_defchoice_path(FILE *out,
                                 const struct module *mod,
                                 const struct node *deft,
                                 const struct node *ch);

static void print_pattern(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, node, T__STATEMENT);
}

static void print_bin_sym(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.BIN.operator;
  const struct node *left = node_subs_first_const(node);
  const struct node *right = node_subs_last_const(node);
  if (OP_IS_ASSIGN(op)
      && (right->which == INIT
          || (right->which == CALL
              && !typ_isa(right->typ, TBI_RETURN_BY_COPY)))) {
    print_expr(out, mod, right, T__STATEMENT);
  } else {
    print_expr(out, mod, left, op);
    print_token(out, op);
    print_expr(out, mod, right, op);
  }
}

static void print_defchoice_access(FILE *out, const struct module *mod,
                                   const struct node *node) {
  const struct node *parent = node_parent_const(node);
  if (parent->which == DEFCHOICE) {
    print_defchoice_access(out, mod, parent);
  }

  fprintf(out, "as.%s.", idents_value(mod->gctx, node_ident(node)));
}

static void print_union_access_path(FILE *out, const struct module *mod,
                                const struct node *node) {
  const struct node *d = typ_definition_const(node->typ);
  if (d->which != DEFTYPE || d->as.DEFTYPE.kind != DEFTYPE_UNION) {
    return;
  }

  ident tag = ID__NONE;
  const bool yes = constraint_has_common_root_tag(&tag, mod, node);
  assert(yes);

  const struct node *dc = node_get_member_const(mod, d, tag);
  print_defchoice_access(out, mod, dc);
}

static void print_bin_acc(FILE *out, const struct module *mod,
                          const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.BIN.operator;
  const struct node *base = node_subs_first_const(node);
  const struct node *name = node_subs_last_const(node);
  const char *name_s = idents_value(mod->gctx, node_ident(name));

  if ((node->flags & NODE_IS_DEFCHOICE)
      || (node->flags & NODE_IS_GLOBAL_LET)) {
    print_typ(out, mod, base->typ);
    fprintf(out, "_%s", name_s);
  } else if (node->flags & NODE_IS_TYPE) {
    print_typ(out, mod, node->typ);
  } else {
    const char *deref = ".";
    if (typ_is_reference(base->typ)) {
      deref = "->";
    }
    print_expr(out, mod, base, op);
    fprintf(out, "%s", deref);
    print_union_access_path(out, mod, base);
    fprintf(out, "%s", name_s);
  }
}

static void print_bin(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.BIN.operator;

  if (!OP_IS_ASSIGN(op)) {
    fprintf(out, "(");
  }

  switch (OP_KIND(op)) {
  case OP_BIN:
  case OP_BIN_SYM:
  case OP_BIN_SYM_BOOL:
  case OP_BIN_SYM_ARITH:
  case OP_BIN_SYM_BW:
  case OP_BIN_SYM_PTR:
  case OP_BIN_BW_RHS_UNSIGNED:
    print_bin_sym(out, mod, node, parent_op);
    break;
  case OP_BIN_ACC:
    print_bin_acc(out, mod, node, parent_op);
    break;
  case OP_BIN_RHS_TYPE:
    print_typeconstraint(out, mod, node);
    break;
  }

  if (!OP_IS_ASSIGN(op)) {
    fprintf(out, ")");
  }
}

static void print_un(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.UN.operator;
  const struct node *term = node_subs_first_const(node);

  fprintf(out, "(");

  switch (OP_KIND(op)) {
  case OP_UN_REFOF:
    if (node->flags & NODE_IS_TYPE) {
      print_typ(out, mod, node->typ);
    } else {
      fprintf(out, "(&");
      print_expr(out, mod, term, op);
      fprintf(out, ")");
    }
    break;
  case OP_UN_DEREF:
    if (node->flags & NODE_IS_TYPE) {
      print_typ(out, mod, term->typ);
    } else {
      fprintf(out, "(*");
      print_expr(out, mod, term, op);
      fprintf(out, ")");
    }
    break;
  case OP_UN_BOOL:
  case OP_UN_ARITH:
  case OP_UN_BW:
    print_token(out, op);
    print_expr(out, mod, term, op);
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

  size_t n = 0;
  FOREACH_SUB_CONST(s, node) {
    if (n > 0) {
      fprintf(out, ", ");
    }
    print_expr(out, mod, s, TCOMMA);
    n += 1;
  }
  fprintf(out, "}");
}

static void print_tuplenth(FILE *out, const struct module *mod, const struct node *node) {
  const struct node *parent = node_parent_const(node);
  assert(parent->which == TUPLEEXTRACT);
  const struct node *target = node_subs_last_const(parent);
  assert(target != node);
  fprintf(out, "((");
  print_expr(out, mod, target, T__STATEMENT);
  fprintf(out, ")%sx%zu",
          typ_is_reference(target->typ) ? "->" : ".",
          node->as.TUPLENTH.nth);
  fprintf(out, ")");
}

static void print_call_vararg_count(FILE *out, const struct node *dfun,
                                    const struct node *node, size_t n) {
  size_t first_vararg;
  switch (dfun->which) {
  case DEFFUN:
    first_vararg = dfun->as.DEFFUN.first_vararg;
    break;
  case DEFMETHOD:
    first_vararg = dfun->as.DEFMETHOD.first_vararg;
    break;
  default:
    assert(false);
  }

  if (n != first_vararg) {
    return;
  }

  const size_t count = node_subs_count(node) - 1 - first_vararg;
  fprintf(out, "%zu", count);
  if (count > 0) {
    fprintf(out, ", ");
  }
}

static void print_call(FILE *out, const struct module *mod,
                       const struct node *node, uint32_t parent_op) {
  const struct node *fun = node_subs_first_const(node);
  const struct typ *ftyp = fun->typ;
  const struct node *fdef = typ_definition_const(ftyp);
  const struct node *parentd = node_parent_const(fdef);

  if (node_ident(fdef) == ID_CAST) {
    fprintf(out, "(");
    print_typ(out, mod, node->typ);
    fprintf(out, ")(");
    print_expr(out, mod, node_subs_at_const(node, 1), T__CALL);
    fprintf(out, ")");
    return;
  } else if (node_ident(fdef) == ID_NEXT && typ_isa(parentd->typ, TBI_VARARG)) {
    const struct node *self = node_subs_at_const(node, 1);
    fprintf(out, "NLANG_BUILTINS_VARARG_NEXT(");
    print_typ(out, mod, typ_generic_arg_const(parentd->typ, 0));
    fprintf(out, ", ");
    if (typ_is_reference(self->typ)) {
      fprintf(out, "*");
    }
    fprintf(out, "(");
    print_expr(out, mod, self, T__CALL);
    fprintf(out, "))");
    return;
  }

  print_typ(out, mod, ftyp);
  fprintf(out, "(");

  bool force_comma = false;
  if (fdef->which == DEFFUN && parentd->which == DEFINTF) {
    fprintf(out, "*(");
    print_typ(out, mod, node_subs_first_const(fun)->typ);
    fprintf(out, " *)&");
    print_expr(out, mod, node_subs_first_const(fun), T__CALL);
    force_comma = true;
  }

  size_t n = 1;
  FOREACH_SUB_EVERY_CONST(arg, node, 1, 1) {
    if (force_comma || n > 1) {
      fprintf(out, ", ");
    }

    print_call_vararg_count(out, fdef, node, n - 1);

    if (n == 1
        && fdef->which == DEFMETHOD
        && parentd->which == DEFINTF) {
      assert(typ_is_reference(arg->typ));
      if (!typ_equal(parentd->typ, typ_generic_arg_const(arg->typ, 0))) {
        fprintf(out, "*(");
        print_typ(out, mod, parentd->typ);
        fprintf(out, " *)&");
      }
    }
    print_expr(out, mod, arg, T__CALL);
    n += 1;
  }

  if (node->as.CALL.return_through_ref_expr != NULL) {
    if (n > 1) {
      fprintf(out, ", ");
    }

    fprintf(out, "&(");
    print_expr(out, mod, node->as.CALL.return_through_ref_expr, T__CALL);
    fprintf(out, ")");
  } else {
    assert(typ_isa(node->typ, TBI_RETURN_BY_COPY));
  }

  fprintf(out, ")");
}

static void print_init_array(FILE *out, const struct module *mod, const struct node *node) {
  const struct node *parent = node_parent_const(node);
  if (parent->which == BIN && OP_IS_ASSIGN(parent->as.BIN.operator)) {
    const struct node *target = node->as.INIT.target_expr;
    print_expr(out, mod, target, T__STATEMENT);
  }
  fprintf(out, "= ");

  if (!node_subs_count_atleast(node, 1)) {
    fprintf(out, "{ 0 }");
    return;
  }

  const struct node *el = node_subs_first_const(node);
  assert(typ_isa(el->typ, TBI_TRIVIAL_COPY) && "not yet supported");

  fprintf(out, "(const ");
  print_typ(out, mod, node->typ);
  fprintf(out, "){ (");
  print_typ(out, mod, el->typ);
  fprintf(out, "[]){ ");

  FOREACH_SUB_CONST(s, node) {
    print_expr(out, mod, s, T__NOT_STATEMENT);
    fprintf(out, ", ");
  }
  fprintf(out, " }, %zu }\n", node_subs_count(node));
}

static void print_tag_init(FILE *out, const struct module *mod,
                           const struct node *node, bool is_inline) {
  const struct node *d = typ_definition_const(node->typ);
  if (d->which != DEFTYPE || d->as.DEFTYPE.kind != DEFTYPE_UNION) {
    return;
  }

  ident tag = ID__NONE;
  const bool yes = constraint_has_common_root_tag(&tag, mod, node);
  assert(yes);
  const struct node *ch = node_get_member_const(mod, d, tag);

  if (!is_inline) {
    print_expr(out, mod, node, TDOT);
  }
  fprintf(out, ".%s = ", idents_value(mod->gctx, ID_TAG));
  print_defchoice_path(out, mod, d, ch);
  fprintf(out, "_%s", idents_value(mod->gctx, ID_TAG));
  if (!is_inline) {
    fprintf(out, ";\n");
  }
}

static void print_init_toplevel(FILE *out, const struct module *mod,
                                const struct node *node) {
  if (!node_subs_count_atleast(node, 1)) {
    fprintf(out, " = { 0 }");
    return;
  }

  fprintf(out, " = {\n");
  print_tag_init(out, mod, node->as.INIT.target_expr, true);
  FOREACH_SUB_EVERY_CONST(s, node, 0, 2) {
    fprintf(out, ".");
    print_union_access_path(out, mod, node->as.INIT.target_expr);
    fprintf(out, "%s", idents_value(mod->gctx, node_ident(s)));
    fprintf(out, " = ");
    print_expr(out, mod, node_next_const(s), T__NOT_STATEMENT);
    fprintf(out, ",\n");
  }
  fprintf(out, " }\n");
}

static void print_init(FILE *out, const struct module *mod,
                       const struct node *node) {
  if (node->as.INIT.is_array) {
    print_init_array(out, mod, node);
    return;
  }

  const struct node *parent = node_parent_const(node);
  const struct node *context = node_parent_const(parent);
  if (context->which == LET) {
    switch (context->which) {
    case MODULE_BODY:
    case DEFTYPE:
      print_init_toplevel(out, mod, node);
      return;
    default:
      break;
    }
  }

  if (parent->which == DEFPATTERN || parent->which == BLOCK) {
    fprintf(out, "= { 0 };\n");
  }

  print_tag_init(out, mod, node->as.INIT.target_expr, false);

  FOREACH_SUB_EVERY_CONST(s, node, 0, 2) {
    print_expr(out, mod, node->as.INIT.target_expr, TDOT);
    fprintf(out, ".");
    print_union_access_path(out, mod, node->as.INIT.target_expr);
    fprintf(out, "%s", idents_value(mod->gctx, node_ident(s)));
    fprintf(out, " = ");
    print_expr(out, mod, node_next_const(s), T__NOT_STATEMENT);
    fprintf(out, ";\n");
  }
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
  assert(node->which == IDENT);
  assert(node_ident(node) != ID_ANONYMOUS);

  if (node->as.IDENT.non_local_scope != NULL) {
    print_scope_name(out, mod, node->as.IDENT.non_local_scope);
    fprintf(out, "_");
  }

  print_scope_last_name(out, mod, &node->scope);
}

static void print_dyn(FILE *out, const struct module *mod, const struct node *node) {
  const struct node *arg = node_subs_first_const(node);
  const struct typ *intf = typ_generic_arg_const(node->typ, 0);
  const struct typ *concrete = typ_generic_arg_const(arg->typ, 0);

  print_typ(out, mod, concrete);
  fprintf(out, "_mkdyn__");
  print_typ(out, mod, intf);
  fprintf(out, "((void *)");
  print_expr(out, mod, arg, T__CALL);
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
      if (typ_equal(node->typ, TBI_STATIC_STRING)) {
        fprintf(out, "nlang_chars_static_string_mk((const nlang_builtins_u8 *)\"%s\", sizeof(\"%s\")-1)", s, s);
      } else if (typ_equal(node->typ, TBI_CHAR)) {
        fprintf(out, "nlang_chars_char_from_ascii('%s')", s);
      } else {
        assert(false);
      }
      free(s);
    }
    break;
  case SIZEOF:
    fprintf(out, "sizeof(");
    print_typ(out, mod, node_subs_first_const(node)->typ);
    fprintf(out, ")");
    break;
  case ALIGNOF:
    fprintf(out, "__alignof__(");
    print_typ(out, mod, node_subs_first_const(node)->typ);
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
  case CALLNAMEDARG:
    print_expr(out, mod, node_subs_first_const(node), parent_op);
    break;
  case TUPLE:
    print_tuple(out, mod, node, parent_op);
    break;
  case TUPLEEXTRACT:
    print_expr(out, mod, node_subs_last_const(node), parent_op);
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
    {
      const bool atleast2 = node_subs_count_atleast(node, 2);
      if (atleast2) {
        fprintf(out, "({ ");
      }
      print_block(out, mod, node, true);
      if (atleast2) {
        fprintf(out, "; })");
      }
    }
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
    fprintf(g_env.stderr, "Unsupported node: %d\n", node->which);
    assert(false);
  }
}

static void print_for(FILE *out, const struct module *mod, const struct node *node) {
  print_statement(out, mod, node_subs_first_const(node));
}

static void print_while(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "while (");
  print_expr(out, mod, node_subs_first_const(node), T__STATEMENT);
  fprintf(out, ")");
  print_block(out, mod, node_subs_at_const(node, 1), false);
}

static void repeat(FILE *out, const char *s, size_t n) {
  while (n > 0) {
    fprintf(out, "%s", s);
    n -= 1;
  }
}

static void print_if(FILE *out, const struct module *mod, const struct node *node) {
  const struct node *n = node_subs_first_const(node);
  const size_t subs_count = node_subs_count(node);
  const size_t br_count = subs_count / 2 + subs_count % 2;

  fprintf(out, "( (");
  print_expr(out, mod, n, T__STATEMENT);
  fprintf(out, ") ? (");
  n = node_next_const(n);
  print_block(out, mod, n, false);
  fprintf(out, ")");

  if (br_count == 1) {
    fprintf(out, " : ({;}) )");
    return;
  }

  n = node_next_const(n);
  while (n != NULL && node_next_const(n) != NULL) {
    fprintf(out, "\n");
    fprintf(out, " : ( (");
    print_expr(out, mod, n, T__STATEMENT);
    fprintf(out, ") ? (");
    n = node_next_const(n);
    print_block(out, mod, n, false);
    fprintf(out, ")");
    n = node_next_const(n);
  }

  if (n != NULL) {
    fprintf(out, "\n");
    fprintf(out, " : (");
    print_block(out, mod, n, false);
    fprintf(out, ")");
  } else {
    fprintf(out, " : ({;})");
  }

  repeat(out, ")", br_count - 1);
}

static void print_defchoice_path(FILE *out,
                                 const struct module *mod,
                                 const struct node *deft,
                                 const struct node *ch) {
  print_deftype_name(out, mod, deft);

  if (ch == deft) {
    return;
  }

  fprintf(out, "_");
  print_deffield_name(out, mod, ch);
}

static void print_match_label(FILE *out, const struct module *mod,
                              const struct node *label) {
  if (node_ident(label) == ID_OTHERWISE) {
    return;
  }

  const struct node *id = label;
  if (id->which == BIN) {
    id = node_subs_last_const(id);
  }
  const struct node *deft = typ_definition_const(label->typ);
  const struct node *ch = node_get_member_const(
    mod, deft, node_ident(id));

  assert(ch->which == DEFCHOICE);
  if (ch->as.DEFCHOICE.is_leaf) {
    fprintf(out, "case ");
    print_defchoice_path(out, mod, deft, ch);
    fprintf(out, "_%s_label__", idents_value(mod->gctx, ID_TAG));
    fprintf(out, ":\n");
    return;
  }

  FOREACH_SUB_CONST(s, ch) {
    if (s->which == DEFCHOICE) {
      print_match_label(out, mod, s);
    }
  }
}

static void print_match(FILE *out, const struct module *mod,
                        const struct node *node) {
  const struct node *expr = node_subs_first_const(node);
  const struct node *dexpr = typ_definition_const(expr->typ);
  fprintf(out, "switch (");
  print_expr(out, mod, expr, T__STATEMENT);
  if (dexpr->which == DEFTYPE && dexpr->as.DEFTYPE.kind == DEFTYPE_UNION) {
    fprintf(out, ".tag");
  }
  fprintf(out, ") {\n");

  const struct node *n = node_next_const(expr);
  while (n != NULL) {
    const struct node *p = n;
    const struct node *block = node_next_const(n);

    print_match_label(out, mod, p);

    if (node_next_const(block) == NULL) {
      fprintf(out, "default:\n");
    }

    print_block(out, mod, block, false);
    fprintf(out, "\nbreak;\n");

    n = node_next_const(block);
  }
  fprintf(out, "}");
}

static void print_try(FILE *out, const struct module *mod, const struct node *node) {
  const struct node *elet = node_subs_first_const(node);
  const struct node *edefp = node_subs_first_const(elet);
  const struct node *eblock = node_next_const(edefp);

  print_defpattern(out, false, FWD_DEFINE_FUNCTIONS, mod, edefp);
  print_block(out, mod, node_subs_first_const(eblock), false);

  fprintf(out, "while (0) {\n");
  size_t n = 0;
  FOREACH_SUB_CONST(catch, eblock) {
    if (n++ == 0) {
      continue;
    }

    fprintf(out, "\n%s: {\n", idents_value(mod->gctx, catch->as.CATCH.label));

    print_let(out, false, FWD_DEFINE_FUNCTIONS, mod, node_subs_first_const(catch));
    print_block(out, mod, node_subs_at_const(catch, 1), false);

    fprintf(out, "\n}\n");
  }
  fprintf(out, "}\n");
}

static void print_pre(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, node_subs_first_const(node), T__CALL);
}

static void print_post(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, node_subs_first_const(node), T__CALL);
}

static void print_invariant(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, node_subs_first_const(node), T__CALL);
}

#define ATTR_SECTION_EXAMPLES "__attribute__((section(\".text.nlang.examples\")))"

static void print_example(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node) {
  if (header) {
    return;
  }

  if (fwd == FWD_DECLARE_FUNCTIONS
      || fwd == FWD_DEFINE_FUNCTIONS) {
    fprintf(out, "void ");
    print_scope_name(out, mod, &mod->root->scope);
    fprintf(out, "__Nexample%zu(void) ", node->as.EXAMPLE.name);

    if (fwd == FWD_DECLARE_FUNCTIONS) {
      fprintf(out, ATTR_SECTION_EXAMPLES ";");
    } else if (fwd == FWD_DEFINE_FUNCTIONS) {
      fprintf(out, "{\n");
      const struct node *block = node_subs_first_const(node);
      print_expr(out, mod, block, T__STATEMENT);
      fprintf(out, ";\n}");
    }
  }
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

static void print_linkage(FILE *out, bool header, const struct node *node) {
  if (header && !node_is_export(node)) {
    return;
  }

  const struct toplevel *toplevel = node_toplevel_const(node);
  const uint32_t flags = toplevel->flags;
  if ((node->which == DEFFUN || node->which == DEFMETHOD)
      && (flags & TOP_IS_EXTERN) && (flags & TOP_IS_INLINE)) {
    fprintf(out, "static inline ");
  } else if (flags & TOP_IS_EXTERN) {
    fprintf(out, "extern ");
  } else if ((flags & TOP_IS_INLINE) && node->which != LET) {
    fprintf(out, "static inline ");
  } else if (node_is_at_top(node) && !(flags & TOP_IS_EXPORT)) {
    fprintf(out, "static ");
  }
}

static void print_typ_name(FILE *out, const struct module *mod,
                           const struct typ *t) {
  if (typ_generic_arity(t) > 0 && !typ_is_generic_functor(t)) {
    print_typ_name(out, mod, typ_generic_functor_const(t));
    return;
  }

  const struct scope *scope = &typ_definition_const(t)->scope;
  print_scope_name(out, mod, scope);
}

static void print_typ_function(FILE *out, const struct module *mod, const struct typ *typ) {
  const struct node *def = typ_definition_const(typ);

  if (typ_generic_arity(typ) > 0) {
    fprintf(out, "_Ngen_");
  }

  if (node_is_at_top(def)) {
    print_typ_name(out, mod, typ);
  } else {
    const struct node *parent = node_parent_const(def);
    const struct typ *tparent = parent->typ;
    print_typ(out, mod, tparent);
    if (parent->which == DEFCHOICE) {
      fprintf(out, "_%s", idents_value(mod->gctx, node_ident(parent)));
    }
    fprintf(out, "_%s", idents_value(mod->gctx, node_ident(def)));
  }

  if (typ_generic_arity(typ) > 0) {
    for (size_t n = 0; n < typ_generic_arity(typ); ++n) {
      fprintf(out, "__");
      print_typ(out, mod, typ_generic_arg_const(typ, n));
    }
    fprintf(out, "_genN_");
  }
}

static void print_typ_data(FILE *out, const struct module *mod, const struct typ *typ) {
  if (typ_is_generic_functor(typ)) {
    print_typ_name(out, mod, typ);
    return;
  } else if (typ_is_reference(typ)
             && !typ_equal(typ, TBI_ANY_ANY_REF)
             && typ_definition_const(typ_generic_arg_const(typ, 0))->which == DEFINTF) {
    // dyn
    print_typ(out, mod, typ_generic_arg_const(typ, 0));
    return;
  }

  if (typ_generic_arity(typ) > 0) {
    fprintf(out, "_Ngen_");
  }

  print_typ_name(out, mod, typ);

  if (typ_generic_arity(typ) > 0) {
    for (size_t n = 0; n < typ_generic_arity(typ); ++n) {
      fprintf(out, "__");
      print_typ(out, mod, typ_generic_arg_const(typ, n));
    }
    fprintf(out, "_genN_");
  }
}

static void print_typ(FILE *out, const struct module *mod, const struct typ *typ) {
  if (typ_is_function(typ)) {
    print_typ_function(out, mod, typ);
  } else {
    print_typ_data(out, mod, typ);
  }
}

static void print_defname_excep(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "%s = ", idents_value(mod->gctx, node->as.DEFNAME.excep_error));
  print_expr(out, mod, node->as.DEFNAME.pattern, TASSIGN);
  fprintf(out, "; if (");
  print_expr(out, mod, node_subs_at_const(node, IDX_DEFNAME_EXCEP_TEST), T__CALL);
  fprintf(out, ") { goto %s; }", idents_value(mod->gctx, node->as.DEFNAME.excep_label));
}

static void print_defname(FILE *out, bool header, enum forward fwd,
                          const struct module *mod, const struct node *node,
                          const struct node *let) {
  assert(node->which == DEFNAME);
  if (fwd == FWD_DECLARE_TYPES) {
    if ((node->flags & NODE_IS_TYPE)) {
      const struct node *pp = node_parent_const(node_parent_const(node));
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
    return;
  }

  if (node->flags & NODE_IS_TYPE) {
    return;
  }

  if (node_ident(node) == ID_OTHERWISE
      || node->as.DEFNAME.pattern->which == EXCEP) {
    return;
  }

  if (!typ_equal(node->typ, TBI_VOID)) {
    print_linkage(out, header, node_parent_const(node_parent_const(node)));

    print_typ(out, mod, node->typ);
    fprintf(out, " ");

    print_pattern(out, mod, node->as.DEFNAME.pattern);
  }

  if (fwd == FWD_DEFINE_FUNCTIONS && (!header || node_is_inline(let))) {
    const struct node *expr = node->as.DEFNAME.expr;
    if (expr != NULL) {
      if (expr->which == BLOCK && !node_subs_count_atleast(expr, 2)) {
        expr = node_subs_first_const(expr);
      }

      if (expr->which == INIT) {
        print_init(out, mod, expr);
      } else if (expr->which == CALL
                 && !typ_isa(expr->typ, TBI_RETURN_BY_COPY)) {
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

static void print_defpattern(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node) {
  assert(node->which == DEFPATTERN);

  if (fwd != FWD_DECLARE_FUNCTIONS && fwd != FWD_DEFINE_FUNCTIONS) {
    return;
  }

  const struct node *let = node_parent_const(node);
  if (let->flags & NODE_IS_GLOBAL_LET) {
    if (header && !node_is_export(let)) {
      return;
    } else if (header && fwd == FWD_DEFINE_FUNCTIONS) {
      // Even if it is inline, the value is set in the .c
      // This is a lost optimization opportunity for the C compiler, but
      // we're not sure of the implications of making the value visible in
      // the header (using a #define).
      return;
    } else if (!header && fwd == FWD_DECLARE_FUNCTIONS && node_is_export(let)) {
      return;
    }
  }

  bool defname_to_subexpr = false;
  FOREACH_SUB_CONST(d, node) {
    if (d->which != DEFNAME) {
      continue;
    }

    defname_to_subexpr |= d->as.DEFNAME.expr != NULL
      && d->as.DEFNAME.expr->which == TUPLENTH;
  }

  if (defname_to_subexpr
      || node_ident(node_subs_last_const(node)) == ID_OTHERWISE) {
    fprintf(out, "(void) (");
    print_expr(out, mod, node_subs_first_const(node), T__STATEMENT);
    fprintf(out, ");\n");
  }

  FOREACH_SUB_CONST(d, node) {
    if (d->which != DEFNAME) {
      continue;
    }

    print_defname(out, header, fwd, mod, d, let);

    if (d->as.DEFNAME.is_excep) {
      print_defname_excep(out, mod, d);
    }
  }
}

static void print_let(FILE *out, bool header, enum forward fwd, const struct module *mod, const struct node *node) {
  const struct node *last_def = node_has_tail_block(node)
    ? node_prev_const(node_subs_last_const(node)) : node_subs_last_const(node);

  FOREACH_SUB_CONST(s, node) {
    // FIXME: not handling multiple definitions chained with 'and' that
    // refer to each others in their expressions.
    print_defpattern(out, header, fwd, mod, s);

    if (s == last_def) {
      break;
    }
  }

  if (fwd == FWD_DEFINE_FUNCTIONS && node_has_tail_block(node)) {
    assert(!node_is_inline(node));
    const struct node *block = node_subs_last_const(node);
    print_block(out, mod, block, true);
  }
}

static void print_return(FILE *out, const struct module *mod, const struct node *node) {
  if (node_subs_count_atleast(node, 1)) {
    const struct node *expr = node_subs_first_const(node);
    if (node->as.RETURN.return_through_ref_expr != NULL) {
      print_expr(out, mod, node->as.RETURN.return_through_ref_expr, T__STATEMENT);
      fprintf(out, " = ");
      print_expr(out, mod, expr, T__STATEMENT);
      fprintf(out, ";\nreturn");
    } else if (!node->as.RETURN.forced_return_through_ref
               && typ_isa(expr->typ, TBI_RETURN_BY_COPY)) {
      fprintf(out, "return ");
      print_expr(out, mod, expr, T__STATEMENT);
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
  case THROW:
    fprintf(out, "%s = ", idents_value(mod->gctx, node->as.THROW.error));
    print_expr(out, mod, node_subs_at_const(node, node_subs_count(node) == 1 ? 0 : 1), TASSIGN);
    fprintf(out, "; goto %s", idents_value(mod->gctx, node->as.THROW.label));
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
    print_let(out, false, FWD_DEFINE_FUNCTIONS, mod, node);
    break;
  case IDENT:
  case NUMBER:
  case BOOL:
  case STRING:
  case NUL:
  case BIN:
  case UN:
  case CALL:
  case TUPLEEXTRACT:
  case TYPECONSTRAINT:
  case SIZEOF:
  case ALIGNOF:
  case INIT:
    print_expr(out, mod, node, T__STATEMENT);
    break;
  case BLOCK:
    print_block(out, mod, node, false);
    break;
  case PHI:
    // noop
    break;
  default:
    fprintf(g_env.stderr, "Unsupported node: %d\n", node->which);
    assert(false);
  }
}

static void print_block(FILE *out, const struct module *mod, const struct node *node, bool no_braces) {
  assert(node->which == BLOCK);
  if (!no_braces) {
    fprintf(out, " {\n");
  }
  FOREACH_SUB_CONST(statement, node) {
    print_statement(out, mod, statement);
    fprintf(out, ";\n");
  }
  if (!no_braces) {
    fprintf(out, "}");
  }
}

static void print_typeconstraint(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, node_subs_first_const(node), T__STATEMENT);
}

static void print_defarg(FILE *out, const struct module *mod, const struct node *node,
                         bool return_through_ref) {
  assert(node->which == DEFARG);
  print_typ(out, mod, node->typ);
  fprintf(out, " ");
  if (return_through_ref) {
    fprintf(out, "*_nrtr_");
  }
  print_expr(out, mod, node_subs_first_const(node), T__STATEMENT);
}

static bool print_call_vararg_proto(FILE *out, const struct node *dfun, size_t n) {
  size_t first_vararg;
  switch (dfun->which) {
  case DEFFUN:
    first_vararg = dfun->as.DEFFUN.first_vararg;
    break;
  case DEFMETHOD:
    first_vararg = dfun->as.DEFMETHOD.first_vararg;
    break;
  default:
    assert(false);
  }

  if (n != first_vararg) {
    return false;
  }

  fprintf(out, "nlang_builtins_size _Nvarargcount, ...");
  return true;
}

static void print_fun_prototype(FILE *out, bool header, const struct module *mod,
                                const struct node *node,
                                bool as_fun_pointer, bool named_fun_pointer,
                                bool as_dyn_fun_pointer,
                                const struct typ *intf_final_typ) {
  assert(!as_fun_pointer || (named_fun_pointer || as_dyn_fun_pointer));

  const size_t args_count = c_fun_args_count(node);
  const struct node *retval = node_fun_retval_const(node);
  const bool retval_throughref = !typ_isa_return_by_copy(retval->typ);

  if (!as_fun_pointer) {
    print_linkage(out, header, node);
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

  bool no_args_at_all = true;
  bool force_comma = false;

  if (node->which == DEFFUN && intf_final_typ != NULL) {
    print_typ(out, mod, intf_final_typ);
    fprintf(out, " self");
    no_args_at_all = false;
    force_comma = true;
  }

  const struct node *funargs = node_subs_at_const(node, IDX_FUNARGS);
  size_t n;
  for (n = 0; n < args_count; ++n) {
    no_args_at_all = false;
    if (force_comma || n > 0) {
      fprintf(out, ", ");
    }

    if (print_call_vararg_proto(out, node, n)) {
      break;
    }

    if (n == 0 && node->which == DEFMETHOD && as_dyn_fun_pointer) {
      fprintf(out, "void *self");
      continue;
    }

    const struct node *arg = node_subs_at_const(funargs, n);
    print_defarg(out, mod, arg, false);
  }

  if (retval_throughref) {
    no_args_at_all = false;
    if (force_comma || n > 0) {
      fprintf(out, ", ");
    }

    print_defarg(out, mod, retval, true);
  }

  if (no_args_at_all) {
    fprintf(out, "void");
  }

  fprintf(out, ")");
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
    print_rtr_helpers_tuple_accessor(out, mod, node_subs_at_const(retval, 1));
    return;
  }

  const struct node *p = parent_in_tuple(retval);

  if (parent_in_tuple(p) != NULL) {
    print_rtr_helpers_tuple_accessor(out, mod, p);
  }

  size_t where = 0;
  bool found = false;
  FOREACH_SUB_CONST(s, node_parent_const(p)) {
    if (s == p) {
      found = true;
      break;
    }
    where += 1;
  }
  assert(!found);

  fprintf(out, ".x%zu", where);
}

static void print_rtr_helpers_fully_named_tuple(FILE *out,
                                                const struct module *mod,
                                                const struct node *retval) {
  if (retval->which == DEFARG) {
    print_expr(out, mod, node_subs_first_const(retval), TCOMMA);
  } else if (retval->which == TUPLE) {
    fprintf(out, "(");
    print_typ(out, mod, retval->typ);
    fprintf(out, "){");

    size_t n = 0;
    FOREACH_SUB_CONST(r, retval) {
      if (n++ > 0) {
        fprintf(out, ", ");
      }

      print_rtr_helpers_fully_named_tuple(out, mod, r);
    }
    fprintf(out, "}");
  } else {
    assert(false && "in a function returning a tuple,"
           " either all return values are named, or none are");
  }
}

static void print_rtr_helpers(FILE *out, const struct module *mod,
                              const struct node *retval, bool start) {
  assert(retval->which == DEFARG);
  const bool named_retval = !typ_equal(retval->typ, TBI_VOID);
  const bool bycopy = typ_isa(retval->typ, TBI_RETURN_BY_COPY);

  const bool parent_tuple = node_parent_const(retval)->which == TUPLE;
  const bool is_tuple = retval->which == TUPLE
    || (named_retval && node_subs_at_const(retval, 1)->which == TUPLE);

  if (start) {
    if (named_retval) {
      if (bycopy) {
        fprintf(out, "__attribute__((__unused__)) ");
        print_defarg(out, mod, retval, false);
        fprintf(out, " = { 0 };\n");
      } else {
        fprintf(out, "#define ");
        print_expr(out, mod, node_subs_first_const(retval), T__STATEMENT);
        fprintf(out, " (*_nrtr_");
        print_expr(out, mod, node_subs_first_const(retval), T__STATEMENT);
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
        print_expr(out, mod, node_subs_first_const(retval), T__STATEMENT);
        fprintf(out, ";\n");
      } else {
        fprintf(out, "#undef ");
        print_expr(out, mod, node_subs_first_const(retval), T__STATEMENT);
        fprintf(out, "\n");
      }
    } else {
      if (bycopy && is_tuple && node_subs_first_const(retval)->which == DEFARG) {
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
      print_rtr_helpers(out, mod, node_subs_at_const(retval, 1), start);
    } else {
      FOREACH_SUB_CONST(r, retval) {
        print_rtr_helpers(out, mod, r, start);
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
  const bool retval_bycopy = typ_isa(retval->typ, TBI_RETURN_BY_COPY);
  if (!retval_bycopy) {
    return;
  }

  fprintf(out, "return %s;\n", what);
}

static void print_deffun_builtingen(FILE *out, const struct module *mod, const struct node *node) {
  const struct node *parent = node_parent_const(node);

  fprintf(out, " {\n");
  if (parent->which == DEFTYPE || parent->which == DEFCHOICE) {
    fprintf(out, "#define THIS(x) ");
    print_typ(out, mod, parent->typ);
    fprintf(out, "##x\n");
  }

  rtr_helpers(out, mod, node, true);

  size_t a;
  const struct node *funargs = NULL;
  const enum builtingen bg = node_toplevel_const(node)->builtingen;
  switch (bg) {
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
    funargs = node_subs_at_const(node, IDX_FUNARGS);

    a = 0;
    FOREACH_SUB_CONST(arg, funargs) {
      if (node_next_const(arg) == NULL) {
        break;
      }
      if (a++ > 0) {
        fprintf(out, ", ");
      }
      fprintf(out, "%s", idents_value(mod->gctx, node_ident(arg)));
    }
    fprintf(out, ");\n");
    break;
  case BG_AUTO_NEW:
    fprintf(out, "THIS() *self = calloc(1, sizeof(sizeof(THIS())));\n");
    fprintf(out, "THIS(_ctor)(self, ");
    funargs = node_subs_at_const(node, IDX_FUNARGS);

    a = 0;
    FOREACH_SUB_CONST(arg, funargs) {
      if (node_next_const(arg) == NULL) {
        break;
      }
      if (a++ > 0) {
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
  case BG_TRIVIAL_COPY_COPY_CTOR:
    fprintf(out, "memcpy(self, other, sizeof(*self));\n");
    break;
  case BG_TRIVIAL_EQUALITY_OPERATOR_EQ:
    fprintf(out, "return memcmp(self, other, sizeof(*self)) == 0;\n");
    break;
  case BG_TRIVIAL_EQUALITY_OPERATOR_NE:
    fprintf(out, "return memcmp(self, other, sizeof(*self)) != 0;\n");
    break;
  case BG_ENVIRONMENT_PARENT:
    assert(node->which == DEFMETHOD);
    fprintf(out, "NLANG_BUILTINS_BG_ENVIRONMENT_PARENT(");
    print_typ(out, mod,
              typ_generic_arg_const(
                node_parent_const(node->as.DEFMETHOD.member_isa)->typ, 0));
    fprintf(out, ");\n");
    break;
  case BG_ENVIRONMENT_INSTALL:
  case BG_ENVIRONMENT_UNINSTALL:
    funargs = node_subs_at_const(node, IDX_FUNARGS);
    fprintf(out, "NLANG_BUILTINS_BG_ENVIRONMENT_%sINSTALL(",
            bg == BG_ENVIRONMENT_UNINSTALL ? "UN" : "");
    print_typ(out, mod,
              typ_generic_arg_const(node_subs_at_const(funargs, 1)->typ, 0));
    fprintf(out, ");\n");
    break;
  default:
    assert(false);
    break;
  }

  rtr_helpers(out, mod, node, false);

  if (parent->which == DEFTYPE || parent->which == DEFCHOICE) {
    fprintf(out, "#undef THIS\n");
  }
  fprintf(out, "}\n");
}

static bool do_fun_nonnull_attribute(FILE *out, const struct typ *tfun) {
  if (out != NULL) {
    fprintf(out, " __attribute__((__nonnull__(");
  }
  size_t count = 0;
  for (size_t n = 0, arity = typ_function_arity(tfun); n < arity; ++n) {
    const struct typ *a = typ_function_arg_const(tfun, n);
    if (typ_is_reference(a)
        && !typ_isa(a, TBI_ANY_NULLABLE_REF)
        && !typ_is_dyn(a)) {
      if (count > 0) {
        if (out != NULL) {
          fprintf(out, ", ");
        }
      }
      if (out != NULL) {
        fprintf(out, "%zu", 1 + n);
      }
      count += 1;
    }
  }
  if (out != NULL) {
    fprintf(out, ")))");
  }

  return count > 0;
}

static void fun_nonnull_attribute(FILE *out, bool header,
                                  const struct module *mod,
                                  const struct node *node) {
  const struct typ *tfun = node->typ;
  const size_t arity = typ_function_arity(tfun);
  if (arity == 0) {
    return;
  }

  if (do_fun_nonnull_attribute(NULL, tfun)) {
    (void) do_fun_nonnull_attribute(out, tfun);
  }
}

static uint32_t track_id(bool header, enum forward fwd) {
  return (!!header << 4) | (uint8_t)fwd;
}

static void guard_generic(FILE *out, bool header, enum forward fwd,
                          const struct module *mod,
                          const struct node *node, const char *postfix,
                          bool begin) {
  const struct typ *t = node->typ;
  const struct node *parent = node_parent_const(node);
  if (typ_generic_arity(t) == 0 && typ_generic_arity(parent->typ) == 0) {
    return;
  }

  if (begin) {
    fprintf(out, "#ifndef HAS%x_", fwd);
    print_typ(out, mod, t);
    fprintf(out, "%s\n#define HAS%x_", postfix, fwd);
    print_typ(out, mod, t);
    fprintf(out, "%s\n", postfix);
  } else {
    fprintf(out, "#endif // HAS%x_", fwd);
    print_typ(out, mod, t);
    fprintf(out, "%s\n", postfix);
  }
}

static void print_deffun(FILE *out, bool header, enum forward fwd,
                         const struct module *mod, const struct node *node,
                         struct typset *printed) {
  if (fwd == FWD_DECLARE_TYPES || fwd == FWD_DEFINE_TYPES) {
    return;
  }
  if (node_is_extern(node) && fwd == FWD_DEFINE_FUNCTIONS) {
    return;
  }
  if (node_ident(node) == ID_NEXT
      && typ_generic_functor_const(node_parent_const(node)->typ) != NULL
      && typ_equal(typ_generic_functor_const(node_parent_const(node)->typ), TBI_VARARG)) {
    // This is a builtin and does not have a real function prototype.
    return;
  }

  const struct node *parent = node_parent_const(node);
  const bool is_gen = typ_generic_arity(node->typ) > 0
    || typ_generic_arity(parent->typ) > 0;
  if (!is_gen) {
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
  }

  const ident id = node_ident(node);
  if (id == ID_CAST
      || id == ID_LIKELY
      || id == ID_UNLIKELY) {
    return;
  }

  guard_generic(out, header, fwd, mod, node, "", true);

  if (fwd == FWD_DECLARE_FUNCTIONS) {
    print_fun_prototype(out, header, mod, node, false, false, false, NULL);
    fun_nonnull_attribute(out, header, mod, node);
    fprintf(out, ";\n");
  } else if (prototype_only(header, node)) {
    // noop
  } else if (node_toplevel_const(node)->builtingen != BG__NOT) {
    print_fun_prototype(out, header, mod, node, false, false, false, NULL);
    print_deffun_builtingen(out, mod, node);
  } else {
    print_fun_prototype(out, header, mod, node, false, false, false, NULL);

    fprintf(out, " {\n");
    if (parent->which == DEFTYPE) {
      fprintf(out, "#define THIS(x) ");
      print_typ(out, mod, parent->typ);
      fprintf(out, "##x\n");
    }

    rtr_helpers(out, mod, node, true);

    const struct node *funargs = node_subs_at_const(node, IDX_FUNARGS);
    const ssize_t first_vararg = node_fun_first_vararg(node);
    ident id_ap = ID__NONE;
    if (first_vararg >= 0) {
      const struct node *ap = node_subs_at_const(funargs, first_vararg);
      id_ap = node_ident(ap);
      print_typ(out, mod, ap->typ);
      fprintf(out, " %s = { 0 };\nNLANG_BUILTINS_VARARG_START(%s);\n",
              idents_value(mod->gctx, id_ap),
              idents_value(mod->gctx, id_ap));
    }

    fprintf(out, "#if 0\n");
    fflush(out);
    printer_tree(fileno(out), mod, node);
    fflush(out);
    fprintf(out, "#endif\n");

    const struct node *block = node_subs_last_const(node);
    print_block(out, mod, block, false);

    fprintf(out, "\n");

    if (first_vararg >= 0) {
      fprintf(out, "NLANG_BUILTINS_VARARG_END(%s);\n",
              idents_value(mod->gctx, id_ap));
    }

    rtr_helpers(out, mod, node, false);

    if (parent->which == DEFTYPE) {
      fprintf(out, "#undef THIS\n");
    }
    fprintf(out, "}\n");
  }

  guard_generic(out, header, fwd, mod, node, "", false);
}

static void print_deffield(FILE *out, const struct module *mod, const struct node *node) {
  print_typ(out, mod, node->typ);
  fprintf(out, " ");
  print_deffield_name(out, mod, node);
}

static void print_delegate(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "delegate ");
  print_expr(out, mod, node_subs_first_const(node), T__CALL);

  FOREACH_SUB_EVERY_CONST(s, node, 1, 1) {
    fprintf(out, " ");
    print_expr(out, mod, s, T__CALL);
  }
}

static void print_deftype_statement(FILE *out, bool header, enum forward fwd,
                                    const struct module *mod, const struct node *node,
                                    bool do_static, struct typset *printed) {
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
    case DEFFUN:
    case DEFMETHOD:
      if (do_static) {
        if (!typ_is_generic_functor(node->typ)) {
          print_top(out, header, fwd, mod, node, printed);
        }
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

struct cprinter_state {
  FILE *out;
  bool header;
  enum forward fwd;
  const struct module *mod;
  size_t printed;
  void *user;
};

static error print_deftype_envparent_eachisalist(struct module *mod,
                                                 struct typ *t, struct typ *intf,
                                                 bool *stop, void *user) {
  if (typ_generic_arity(intf) == 0
      || !typ_equal(typ_generic_functor_const(intf), TBI_ENVIRONMENT)) {
    return 0;
  }

  struct cprinter_state *st = user;
  fprintf(st->out, "NLANG_BUILTINS_DEFINE_ENVPARENT(");
  print_typ(st->out, st->mod, typ_generic_arg_const(intf, 0));
  fprintf(st->out, ");\n");

  return 0;
}

static void print_deftype_block(FILE *out, bool header, enum forward fwd, const struct module *mod,
                                const struct node *node, bool do_static, struct typset *printed) {
  assert(node->which == DEFTYPE && node->as.DEFTYPE.kind == DEFTYPE_STRUCT);

  if (!do_static) {
    fprintf(out, " {\n");
  }

  FOREACH_SUB_EVERY_CONST(statement, node, 2, 1) {
    const ssize_t prev_pos = ftell(out);
    print_deftype_statement(out, header, fwd, mod, statement, do_static, printed);
    // Hack to prevent isolated ';' when statement does not print anything.
    if (ftell(out) != prev_pos) {
      fprintf(out, ";\n");
    }
  }

  if (!do_static && typ_isa(node->typ, TBI_ENVIRONMENT)) {
    struct cprinter_state st = {
      .out = out, .header = header, .fwd = fwd,
      .mod = mod, .printed = 0, .user = NULL,
    };
    error e = typ_isalist_foreach((struct module *)mod, node->typ, 0,
                                  print_deftype_envparent_eachisalist,
                                  &st);
    assert(!e);
  }

  if (!do_static) {
    fprintf(out, "}\n");
  }
}

static void print_mkdyn_proto(FILE *out, const struct module *mod,
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

static error print_mkdyn_proto_eachisalist(struct module *mod, struct typ *t,
                                           struct typ *intf,
                                           bool *stop, void *user) {
  struct cprinter_state *st = user;

  print_mkdyn_proto(st->out, mod, typ_definition_const(t), intf);
  fprintf(st->out, ";\n");
  return 0;
}

static error print_dyn_field_eachisalist(struct module *mod, struct typ *ignored,
                                         struct typ *intf,
                                         bool *stop, void *user) {
  struct cprinter_state *st = user;
  struct typ *t = st->user;

  const struct node *dintf = typ_definition_const(intf);
  FOREACH_SUB_CONST(f, dintf) {
    if (f->which != DEFFUN && f->which != DEFMETHOD) {
      continue;
    }
    if (node_toplevel_const(f)->flags & TOP_IS_NOT_DYN) {
      continue;
    }
    if (node_subs_count_atleast(node_subs_at_const(typ_definition_const(f->typ), IDX_GENARGS), 1)) {
      continue;
    }

    st->printed += 1;
    const struct node *thisf = node_get_member_const(mod, typ_definition_const(t),
                                                     node_ident(f));
    fprintf(st->out, ".%s = (", idents_value(mod->gctx, node_ident(thisf)));
    print_fun_prototype(st->out, st->header, mod, f, true, false, true, NULL);
    fprintf(st->out, ")");
    print_typ(st->out, mod, thisf->typ);
    fprintf(st->out, ",\n");
  }

  return 0;
}

static error print_mkdyn_eachisalist(struct module *mod, struct typ *t,
                                     struct typ *intf,
                                     bool *stop, void *user) {
  struct cprinter_state *st = user;

  print_mkdyn_proto(st->out, mod, typ_definition_const(t), intf);
  fprintf(st->out, " {\n");
  fprintf(st->out, "static const struct _Ndyn_");
  print_typ(st->out, mod, intf);
  fprintf(st->out, " vtable = {\n");

  struct cprinter_state st2 = *st;
  st2.printed = 0;
  st2.user = (void *)t;

  // FIXME: Shouldn't filter out trivial intf, but we don't yet have
  // builtingen for all of them.
  const uint32_t filter = ISALIST_FILTER_TRIVIAL_ISALIST
    | ISALIST_FILTER_PREVENT_DYN
    | (st->header ? ISALIST_FILTER_NOT_EXPORTED : ISALIST_FILTER_EXPORTED);
  error e = typ_isalist_foreach((struct module *)mod, intf, filter,
                                print_dyn_field_eachisalist,
                                &st2);
  assert(!e);
  e = print_dyn_field_eachisalist((struct module *)mod,
                                  NULL, intf, NULL, &st2);
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

static void print_mkdyn(FILE *out, bool header, enum forward fwd,
                        const struct module *mod, const struct node *node) {
  // FIXME: Shouldn't filter out trivial intf, but we don't yet have
  // builtingen for all of them.
  const uint32_t filter = ISALIST_FILTER_TRIVIAL_ISALIST
    | ISALIST_FILTER_PREVENT_DYN
    | (header ? ISALIST_FILTER_NOT_EXPORTED : ISALIST_FILTER_EXPORTED);
  if (fwd == FWD_DECLARE_FUNCTIONS) {
    struct cprinter_state st = { .out = out, .header = header, .fwd = fwd,
      .mod = NULL, .printed = 0, .user = NULL };
    error e = typ_isalist_foreach((struct module *)mod, node->typ, filter,
                                  print_mkdyn_proto_eachisalist,
                                  &st);
    assert(!e);
  } else if (fwd == FWD_DEFINE_FUNCTIONS) {
    struct cprinter_state st = { .out = out, .header = header, .fwd = fwd,
      .mod = NULL, .printed = 0, .user = NULL };
    error e = typ_isalist_foreach((struct module *)mod, node->typ, filter,
                                  print_mkdyn_eachisalist,
                                  &st);
    assert(!e);
  }
}

static void print_defchoice_payload(FILE *out,
                                    enum forward fwd,
                                    const struct module *mod,
                                    const struct node *deft,
                                    const struct node *ch) {
  fprintf(out, "struct ");
  print_defchoice_path(out, mod, deft, ch);

  if (fwd == FWD_DECLARE_TYPES) {
    fprintf(out, ";\ntypedef struct ");
    print_defchoice_path(out, mod, deft, ch);
    fprintf(out, " ");
    print_defchoice_path(out, mod, deft, ch);
    fprintf(out, ";\n");
    return;
  }

  fprintf(out, " {\n");

  if (!node_subs_count_atleast(ch, IDX_CH_FIRST_PAYLOAD+1)) {
    print_typ(out, mod, TBI_U8);
    fprintf(out, " _Nfiller;\n};\n");
    return;
  }

  FOREACH_SUB_EVERY_CONST(m, ch, IDX_CH_FIRST_PAYLOAD, 1) {
    if (m->which == DEFFIELD) {
      print_deffield(out, mod, m);
      fprintf(out, ";\n");
    }
  }

  if (ch == deft) {
    print_defchoice_path(out, mod, deft, deft);
    fprintf(out, "_%s %s;\n",
            idents_value(mod->gctx, ID_TAG_TYPE),
            idents_value(mod->gctx, ID_TAG));
  }

  if (!ch->as.DEFCHOICE.is_leaf) {
    fprintf(out, "union ");
    print_defchoice_path(out, mod, deft, ch);
    fprintf(out, "_%s %s;\n",
            idents_value(mod->gctx, ID_AS_TYPE),
            idents_value(mod->gctx, ID_AS));
  }

  fprintf(out, "};\n");
}

static void print_defchoice_leaf(FILE *out,
                                 const struct module *mod,
                                 const struct node *deft,
                                 const struct node *ch) {
  fprintf(out, "#define ");
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "_%s_label__ (", idents_value(mod->gctx, ID_TAG));
  print_expr(out, mod, node_subs_at_const(ch, IDX_CH_TAG_FIRST), T__NOT_STATEMENT);
  fprintf(out, ")\n");

  fprintf(out, "static const ");
  print_deftype_name(out, mod, deft);
  fprintf(out, "_%s", idents_value(mod->gctx, ID_TAG_TYPE));
  fprintf(out, " ");
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "_%s = ", idents_value(mod->gctx, ID_TAG));
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "_%s_label__;\n", idents_value(mod->gctx, ID_TAG));
}

static void print_defchoice(FILE *out,
                            const struct module *mod,
                            const struct node *deft,
                            const struct node *ch) {
  if (ch->as.DEFCHOICE.is_leaf) {
    print_defchoice_leaf(out, mod, deft, ch);
    return;
  }

  fprintf(out, "#define ");
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "_%s_label__ (", idents_value(mod->gctx, ID_FIRST_TAG));
  print_expr(out, mod, node_subs_at_const(ch, IDX_CH_TAG_FIRST), T__NOT_STATEMENT);
  fprintf(out, ")\n");

  fprintf(out, "static const ");
  print_deftype_name(out, mod, deft);
  fprintf(out, "_%s", idents_value(mod->gctx, ID_TAG_TYPE));
  fprintf(out, " ");
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "_%s = ", idents_value(mod->gctx, ID_FIRST_TAG));
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "_%s_label__;\n", idents_value(mod->gctx, ID_FIRST_TAG));

  fprintf(out, "#define ");
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "_%s_label__ (", idents_value(mod->gctx, ID_LAST_TAG));
  print_expr(out, mod, node_subs_at_const(ch, IDX_CH_TAG_FIRST), T__NOT_STATEMENT);
  fprintf(out, ")\n");

  fprintf(out, "static const ");
  print_deftype_name(out, mod, deft);
  fprintf(out, "_%s", idents_value(mod->gctx, ID_TAG_TYPE));
  fprintf(out, " ");
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "_%s = ", idents_value(mod->gctx, ID_LAST_TAG));
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "_%s_label__;\n", idents_value(mod->gctx, ID_LAST_TAG));
}

static void print_union_functions(FILE *out, bool header, enum forward fwd,
                                  const struct module *mod,
                                  const struct node *deft,
                                  const struct node *ch,
                                  struct typset *printed) {
  if (header && !(node_is_export(deft) && node_is_inline(deft))) {
    return;
  }

  FOREACH_SUB_CONST(m, ch) {
    switch (m->which) {
    case DEFFUN:
    case DEFMETHOD:
      print_top(out, header, fwd, mod, m, printed);
      break;
    case DEFCHOICE:
      print_union_functions(out, header, fwd, mod, deft, m, printed);
      break;
    default:
      break;
    }
  }
}

static void print_union_types(FILE *out, bool header, enum forward fwd,
                              const struct module *mod, const struct node *deft,
                              const struct node *ch) {
  if (fwd == FWD_DECLARE_TYPES) {
    if (header && !node_is_export(deft)) {
      return;
    } else if (!header && node_is_export(deft)) {
      return;
    }

    if (ch != deft) {
      print_defchoice(out, mod, deft, ch);
    }
  } else if (fwd == FWD_DEFINE_TYPES) {
    if (header && !(node_is_export(deft) && node_is_inline(deft))) {
      return;
    } else if (!header && (node_is_export(deft) && node_is_inline(deft))) {
      return;
    }
  }

  if (ch->which != DEFCHOICE || !ch->as.DEFCHOICE.is_leaf) {
    fprintf(out, "union ");
    print_defchoice_path(out, mod, deft, ch);
    fprintf(out, "_%s" , idents_value(mod->gctx, ID_AS_TYPE));
    if (fwd == FWD_DEFINE_TYPES) {
      fprintf(out, " {\n");
      FOREACH_SUB_CONST(m, ch) {
        if (m->which == DEFCHOICE) {
          print_defchoice_path(out, mod, deft, m);
          fprintf(out, " %s;\n",
                  idents_value(mod->gctx, node_ident(m)));
        }
      }
      fprintf(out, "}");
    }
    fprintf(out, ";\n");
  }

  print_defchoice_payload(out, fwd, mod, deft, ch);
}

static void print_union(FILE *out, bool header, enum forward fwd,
                        const struct module *mod, const struct node *deft,
                        const struct node *node, struct typset *printed) {
  switch (fwd) {
  case FWD_DECLARE_TYPES:
    if (node == deft) {
      fprintf(out, "typedef ");
      print_typ(out, mod, deft->as.DEFTYPE.tag_typ);
      fprintf(out, " ");
      print_deftype_name(out, mod, deft);
      fprintf(out, "_%s" , idents_value(mod->gctx, ID_TAG_TYPE));
      fprintf(out, ";\n");
    }

    // fallthrough
  case FWD_DEFINE_TYPES:
    FOREACH_SUB_CONST(ch, node) {
      if (ch->which == DEFCHOICE) {
        print_union(out, header, fwd, mod, deft, ch, printed);
      }
    }
    print_union_types(out, header, fwd, mod, deft, node);
    break;
  case FWD_DECLARE_FUNCTIONS:
  case FWD_DEFINE_FUNCTIONS:
    FOREACH_SUB_CONST(ch, node) {
      if (ch->which == DEFCHOICE) {
        print_union(out, header, fwd, mod, deft, ch, printed);
      }
    }

    print_union_functions(out, header, fwd, mod, deft, node, printed);
    print_mkdyn(out, header, fwd, mod, node);
    break;
  default:
    assert(false);
    break;
  }
}

static void print_enum(FILE *out, bool header, enum forward fwd,
                       const struct module *mod, const struct node *deft,
                       const struct node *node, struct typset *printed) {
  if (fwd == FWD_DECLARE_TYPES) {
    if (deft == node) {
      fprintf(out, "typedef ");
      print_typ(out, mod, node->as.DEFTYPE.tag_typ);
      fprintf(out, " ");
      print_deftype_name(out, mod, node);
      fprintf(out, ";\n");
    }
    return;
  }

  if (fwd == FWD_DEFINE_TYPES) {
    if (prototype_only(header, node)) {
      return;
    }
    FOREACH_SUB_CONST(s, node) {
      if (s->which != DEFCHOICE) {
        continue;
      }

      if (s->as.DEFCHOICE.is_leaf) {
        fprintf(out, "#define ");
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, "_label__ (");
        print_expr(out, mod, node_subs_at_const(s, IDX_CH_TAG_FIRST), T__NOT_STATEMENT);
        fprintf(out, ")\n");

        fprintf(out, "static const ");
        print_deftype_name(out, mod, node);
        fprintf(out, " ");
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, " = ");
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, "_label__;\n");
      } else {
        fprintf(out, "#define ");
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, "_%s_label__ (", idents_value(mod->gctx, ID_FIRST_TAG));
        print_expr(out, mod, node_subs_at_const(s, IDX_CH_TAG_FIRST), T__NOT_STATEMENT);
        fprintf(out, ")\n");

        fprintf(out, "static const ");
        print_deftype_name(out, mod, node);
        fprintf(out, " ");
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, "_%s = ", idents_value(mod->gctx, ID_FIRST_TAG));
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, "_%s_label__;\n", idents_value(mod->gctx, ID_FIRST_TAG));

        fprintf(out, "#define ");
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, "_%s_label__ (", idents_value(mod->gctx, ID_LAST_TAG));
        print_expr(out, mod, node_subs_at_const(s, IDX_CH_TAG_LAST), T__NOT_STATEMENT);
        fprintf(out, ")\n");

        fprintf(out, "static const ");
        print_deftype_name(out, mod, node);
        fprintf(out, " ");
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, "_%s = ", idents_value(mod->gctx, ID_LAST_TAG));
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, "_%s_label__;\n", idents_value(mod->gctx, ID_LAST_TAG));

        print_enum(out, header, fwd, mod, deft, s, printed);
      }
    }
  }
}

static void print_deftype_reference(FILE *out, bool header, enum forward fwd,
                                    const struct module *mod,
                                    const struct node *node) {
  const struct typ *r = typ_generic_functor_const(node->typ);
  const struct node *d = typ_definition_const(typ_generic_arg_const(node->typ, 0));

  if (d->which == DEFINTF) {
    return;
  }

  const char *prefix = "";
  if (!(typ_is_builtin(mod, d->typ) && node_is_extern(d))
      && !(d->which == DEFTYPE && d->as.DEFTYPE.kind == DEFTYPE_ENUM)) {
    prefix = "struct ";
    fprintf(out, "struct ");
    print_deftype_name(out, mod, d);
    fprintf(out, ";\n");
  }

  if (typ_equal(r, TBI_REF)) {
    fprintf(out, "typedef const %s", prefix);
    print_deftype_name(out, mod, d);
    fprintf(out, "* _Ngen_nlang_builtins_ref__");
    print_deftype_name(out, mod, d);
    fprintf(out, "_genN_;\n");
  } else if (typ_equal(r, TBI_NREF)) {
    fprintf(out, "typedef const %s", prefix);
    print_deftype_name(out, mod, d);
    fprintf(out, "* _Ngen_nlang_builtins_nullable_ref__");
    print_deftype_name(out, mod, d);
    fprintf(out, "_genN_;\n");
  } else if (typ_equal(r, TBI_MREF)) {
    fprintf(out, "typedef %s", prefix);
    print_deftype_name(out, mod, d);
    fprintf(out, "* _Ngen_nlang_builtins_mutable_ref__");
    print_deftype_name(out, mod, d);
    fprintf(out, "_genN_;\n");
  } else if (typ_equal(r, TBI_MMREF)) {
    fprintf(out, "typedef %s", prefix);
    print_deftype_name(out, mod, d);
    fprintf(out, "* _Ngen_nlang_builtins_mercurial_ref__");
    print_deftype_name(out, mod, d);
    fprintf(out, "_genN_;\n");
  } else if (typ_equal(r, TBI_NMREF)) {
    fprintf(out, "typedef %s", prefix);
    print_deftype_name(out, mod, d);
    fprintf(out, "* _Ngen_nlang_builtins_nullable_mutable_ref__");
    print_deftype_name(out, mod, d);
    fprintf(out, "_genN_;\n");
  } else if (typ_equal(r, TBI_NMMREF)) {
    fprintf(out, "typedef %s", prefix);
    print_deftype_name(out, mod, d);
    fprintf(out, "* _Ngen_nlang_builtins_nullable_mercurial_ref__");
    print_deftype_name(out, mod, d);
    fprintf(out, "_genN_;\n");
  }
}

static void print_deftype(FILE *out, bool header, enum forward fwd,
                          const struct module *mod, const struct node *node,
                          struct typset *printed) {
  const bool is_gen = typ_generic_arity(node->typ) > 0;
  if (!is_gen) {
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
  }

  if (fwd == FWD_DEFINE_TYPES && node_is_extern(node)) {
    return;
  }

  if (typ_is_pseudo_builtin(node->typ)) {
    return;
  }

  guard_generic(out, header, fwd, mod, node, "", true);

  if (typ_is_reference(node->typ)) {
    print_deftype_reference(out, header, fwd, mod, node);
    goto done;
  }

  if (typ_is_builtin(mod, node->typ) && node_is_extern(node)) {
    if (fwd == FWD_DECLARE_FUNCTIONS) {
      print_deftype_block(out, header, fwd, mod, node, true, printed);
    }
    print_mkdyn(out, header, fwd, mod, node);
    goto done;
  }

  if (node->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    print_enum(out, header, fwd, mod, node, node, printed);
    goto done;
  } else if (node->as.DEFTYPE.kind == DEFTYPE_UNION) {
    print_union(out, header, fwd, mod, node, node, printed);
    goto done;
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

    print_deftype_block(out, header, fwd, mod, node, true, printed);

  } else if (fwd == FWD_DEFINE_TYPES) {
    print_deftype_block(out, header, fwd, mod, node, true, printed);

    if (!prototype_only(header, node)) {
      fprintf(out, "struct ");
      print_deftype_name(out, mod, node);
      print_deftype_block(out, header, fwd, mod, node, false, printed);
      fprintf(out, ";\n");
    }
  } else if (fwd == FWD_DECLARE_FUNCTIONS || fwd == FWD_DEFINE_FUNCTIONS) {
    print_deftype_block(out, header, fwd, mod, node, true, printed);
  }

  print_mkdyn(out, header, fwd, mod, node);

done:
  guard_generic(out, header, fwd, mod, node, "", false);
}

static error print_defintf_dyn_field_eachisalist(struct module *mod, struct typ *t,
                                                 struct typ *intf,
                                                 bool *stop, void *user) {
  struct cprinter_state *st = user;

  const struct node *dintf = typ_definition_const(intf);
  FOREACH_SUB_CONST(d, dintf) {
    if (d->which != DEFFUN && d->which != DEFMETHOD) {
      continue;
    }
    if (node_toplevel_const(d)->flags & TOP_IS_NOT_DYN) {
      continue;
    }
    if (node_subs_count_atleast(node_subs_at_const(typ_definition_const(d->typ), IDX_GENARGS), 1)) {
      continue;
    }
    print_fun_prototype(st->out, st->header, mod, d, true, true, true, NULL);
    fprintf(st->out, ";\n");
    st->printed += 1;
  }

  return 0;
}

static error print_defintf_member_proto_eachisalist(struct module *mod, struct typ *t,
                                                    struct typ *intf, void *user) {
  struct cprinter_state *st = user;

  const struct node *dintf = typ_definition_const(intf);
  FOREACH_SUB_CONST(d, dintf) {
    if (d->which != DEFFUN && d->which != DEFMETHOD) {
      continue;
    }
    if (node_toplevel_const(d)->flags & TOP_IS_NOT_DYN) {
      continue;
    }
    if (node_subs_count_atleast(node_subs_at_const(typ_definition_const(d->typ), IDX_GENARGS), 1)) {
      continue;
    }
    print_fun_prototype(st->out, st->header, mod, d, false, false, false, t);
    fprintf(st->out, ";\n");
  }
  return 0;
}

static error print_defintf_member_eachisalist(struct module *mod, struct typ *t,
                                              struct typ *intf, void *user) {
  struct cprinter_state *st = user;

  const struct node *dintf = typ_definition_const(intf);
  FOREACH_SUB_CONST(d, dintf) {
    if (d->which != DEFFUN && d->which != DEFMETHOD) {
      continue;
    }
    if (node_toplevel_const(d)->flags & TOP_IS_NOT_DYN) {
      continue;
    }
    if (node_subs_count_atleast(node_subs_at_const(typ_definition_const(d->typ), IDX_GENARGS), 1)) {
      continue;
    }

    print_fun_prototype(st->out, st->header, mod, d, false, false, false, t);
    fprintf(st->out, " {\n");

    const bool retval_throughref = !typ_isa_return_by_copy(node_fun_retval_const(d)->typ);
    if (!retval_throughref
        && !typ_equal(node_fun_retval_const(d)->typ, TBI_VOID)) {
      fprintf(st->out, "return ");
    }
    fprintf(st->out, "self.vptr->%s(", idents_value(mod->gctx, node_ident(d)));
    bool need_comma = false;
    if (d->which == DEFMETHOD) {
      need_comma = true;
      fprintf(st->out, "self.obj");
    }

    const struct node *funargs = node_subs_at_const(d, IDX_FUNARGS);
    FOREACH_SUB_EVERY_CONST(arg, funargs, d->which == DEFMETHOD ? 1 : 0, 1) {
      if (node_next_const(arg) == NULL) {
        break;
      }
      if (need_comma) {
        fprintf(st->out, ", ");
      }
      fprintf(st->out, "%s",
              idents_value(mod->gctx, node_ident(arg)));

      need_comma = true;
    }

    if (retval_throughref) {
      if (need_comma) {
        fprintf(st->out, ", ");
      }

      fprintf(st->out, "_nrtr_");
      print_expr(st->out, mod, node_subs_first_const(node_fun_retval_const(d)), T__STATEMENT);
    }

    fprintf(st->out, ");\n}\n");
  }
  return 0;
}

static void print_defintf(FILE *out, bool header, enum forward fwd,
                          const struct module *mod, const struct node *node) {
  const bool is_gen = typ_generic_arity(node->typ) > 0;
  if (!is_gen) {
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
  }

  if (typ_is_pseudo_builtin(node->typ)) {
    return;
  }

  if (typ_is_reference(node->typ)) {
    return;
  }

  guard_generic(out, header, fwd, mod, node, "", true);

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

      struct cprinter_state st = {
        .out = out, .header = header, .fwd = fwd,
        .mod = NULL, .printed = 0, .user = NULL,
      };
      error e = typ_isalist_foreach((struct module *)mod, node->typ, 0,
                                    // filter is 0 because all members of an
                                    // intf inherit the exported status from
                                    // the intf itself.
                                    print_defintf_dyn_field_eachisalist,
                                    &st);
      assert(!e);
      e = print_defintf_dyn_field_eachisalist((struct module *)mod, node->typ,
                                              node->typ, NULL, &st);
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
    struct cprinter_state st = { .out = out, .header = header, .fwd = fwd,
      .mod = NULL, .printed = 0, .user = NULL };
    error e = print_defintf_member_proto_eachisalist((struct module *)mod, node->typ,
                                                     node->typ, &st);
    assert(!e);
  } else if (fwd == FWD_DEFINE_FUNCTIONS && !header) {
    struct cprinter_state st = { .out = out, .header = header, .fwd = fwd,
      .mod = NULL, .printed = 0, .user = NULL };
    error e = print_defintf_member_eachisalist((struct module *)mod, node->typ,
                                               node->typ, &st);
    assert(!e);
  }

  guard_generic(out, header, fwd, mod, node, "", false);
}

static void print_guarded_include(FILE *out, bool header, enum forward fwd,
                                  const char *filename, const char *postfix) {
  fprintf(out, "#define %s\n", forward_guards[fwd]);
  fprintf(out, "# include \"%s%s\"\n", filename, postfix);
  fprintf(out, "#undef %s\n", forward_guards[fwd]);
}

static void print_module(FILE *out, bool header, const struct module *mod);

static void print_import(FILE *out, bool header, enum forward fwd,
                         const struct module *mod, const struct node *node) {
  struct node *target = NULL;
  error e = scope_lookup(&target, mod, &mod->gctx->modules_root.scope,
                         node_subs_first_const(node), false);
  assert(!e);
  if (target->which != MODULE) {
    return;
  }

  print_guarded_include(out, header, fwd, target->as.MODULE.mod->filename, ".o.h");
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

static bool is_concrete(const struct node *instance) {
  if (!node_can_have_genargs(instance)) {
    return true;
  }

  const struct node *genargs = node_subs_at_const(instance, IDX_GENARGS);
  FOREACH_SUB_CONST(g, genargs) {
    if (g->which == DEFGENARG) {
      return false;
    }
  }
  return true;
}

static bool is_printed(struct typset *printed,
                       bool header, enum forward fwd,
                       const struct typ *t) {
  uint32_t *at = typset_get(printed, t);

  if (at != NULL) {
    const uint32_t at_fwd = (*at) & 0x0f;
    const uint32_t at_header = (*at) >> 4;
    if ((!at_header && header) || at_fwd >= fwd) {
      return true;
    }
  }

  return false;
}

static void track_printed(struct typset *printed,
                          bool header, enum forward fwd,
                          const struct typ *t) {
  const uint32_t update = track_id(header, fwd);
  uint32_t *at = typset_get(printed, t);
  if (at != NULL) {
    *at = update;
  } else {
    typset_set(printed, t, update);
  }
}

static int print_topdeps_foreach(const struct typ **t, uint32_t *value, void *user) {
  struct cprinter_state *st = user;
  struct typset *printed = st->user;
  enum toplevel_flags mask = *value;

  if (st->header && !(mask & (TOP_IS_EXPORT | TOP_IS_INLINE))) {
    return 0;
  }

  if (is_printed(printed, st->header, st->fwd, *t)) {
    return 0;
  }

  track_printed(printed, st->header, st->fwd, *t);

  const struct node *d = typ_definition_const(*t);
  const struct module *dmod = node_module_owner_const(d);
  print_top(st->out, st->header, st->fwd, dmod, d, printed);

  return 0;
}

static void print_topdeps(FILE *out, bool header, enum forward fwd,
                          const struct module *mod, const struct node *node,
                          struct typset *printed) {
  const struct toplevel *toplevel = node_toplevel_const(node);
  struct typset *topdeps = toplevel->topdeps;
  if (topdeps == NULL) {
    return;
  }

  if (header && !(toplevel->flags & TOP_IS_EXPORT)) {
    return;
  }

  struct cprinter_state st = {
    .out = out,
    .header = header,
    .fwd = fwd,
    .mod = mod,
    .printed = 0,
    .user = printed,
  };
  (void) typset_foreach(topdeps, print_topdeps_foreach, &st);
}

static void print_top(FILE *out, bool header, enum forward fwd,
                      const struct module *mod, const struct node *node,
                      struct typset *printed) {
  if (!is_concrete(node)) {
    return;
  }

  print_topdeps(out, header, fwd, mod, node, printed);

  switch (node->which) {
  case DEFMETHOD:
  case DEFFUN:
    print_deffun(out, header, fwd, mod, node, printed);
    break;
  case DEFTYPE:
    print_deftype(out, header, fwd, mod, node, printed);
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
  case WITHIN:
    break;
  default:
    fprintf(g_env.stderr, "Unsupported node: %d\n", node->which);
    assert(false);
  }
}

static void print_module(FILE *out, bool header, const struct module *mod) {
  fprintf(out, "#include <lib/nlang/runtime.h>\n");

  struct typset printed;
  typset_fullinit(&printed);

  const struct node *top = mod->body;

  const struct node *first_non_import = node_subs_first_const(top);
  FOREACH_SUB_CONST(node, top) {
    if (node->which != IMPORT) {
      first_non_import = node;
      break;
    }
  }

  enum forward fwd_passes[] = {
    FWD_DECLARE_TYPES, FWD_DEFINE_TYPES, FWD_DECLARE_FUNCTIONS, FWD_DEFINE_FUNCTIONS };
  for (int i = 0; i < ARRAY_SIZE(fwd_passes); ++i) {
    enum forward fwd = fwd_passes[i];

    if (header) {
      fprintf(out, "#ifdef %s\n", forward_guards[fwd]);
      fprintf(out, "#ifndef %s__", forward_guards[fwd]);
      print_scope_name(out, mod, &mod->root->scope);
      fprintf(out, "\n#define %s__", forward_guards[fwd]);
      print_scope_name(out, mod, &mod->root->scope);
      fprintf(out, "\n\n");
    }

    for (const struct node *node = node_subs_first_const(top);
         node != first_non_import; node = node_next_const(node)) {
      print_top(out, header, fwd, mod, node, &printed);

      if (node_next_const(node) != NULL) {
        fprintf(out, "\n");
      }
    }

    if (header) {
      if (file_exists(mod->filename, ".h")) {
        print_guarded_include(out, header, fwd, mod->filename, ".h");
      }
    } else {
      print_guarded_include(out, header, fwd, mod->filename, ".o.h");
      if (file_exists(mod->filename, ".c")) {
        print_guarded_include(out, header, fwd, mod->filename, ".c");
      }
    }

    for (const struct node *node = first_non_import; node != NULL;
         node = node_next_const(node)) {
      print_top(out, header, fwd, mod, node, &printed);

      if (node_next_const(node) != NULL) {
        fprintf(out, "\n");
      }
    }

    if (header) {
      fprintf(out, "\n#endif\n");
      fprintf(out, "#endif // %s\n", forward_guards[fwd]);
    }
  }

  typset_destroy(&printed);
}

static error print_runexamples(FILE *out, const struct module *mod) {
  fprintf(out, "void ");
  print_c_runexamples_name(out, mod);
  fprintf(out, "(void) " ATTR_SECTION_EXAMPLES ";\n");

  fprintf(out, "void ");
  print_c_runexamples_name(out, mod);
  fprintf(out, "(void) {\n");
  for (size_t n = 0; n < mod->next_example; ++n) {
    print_scope_name(out, mod, &mod->root->scope);
    fprintf(out, "__Nexample%zu();\n", n);
  }
  fprintf(out, "}\n");
  return 0;
}

error printer_c(int fd, const struct module *mod) {
  FILE *out = fdopen(fd, "w");
  if (out == NULL) {
    THROWF(errno, "Invalid output file descriptor '%d'", fd);
  }

  print_module(out, false, mod);
  print_runexamples(out, mod);
  fflush(out);

  return 0;
}

error printer_h(int fd, const struct module *mod) {
  FILE *out = fdopen(fd, "w");
  if (out == NULL) {
    THROWF(errno, "Invalid output file descriptor '%d'", fd);
  }

  print_module(out, true, mod);
  fflush(out);

  return 0;
}

void print_c_runexamples_name(FILE *out, const struct module *mod) {
  print_scope_name(out, mod, &mod->root->scope);
  fprintf(out, "_Nrunexamples");
}
