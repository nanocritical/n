#ifndef _POSIX_SOURCE
# define _POSIX_SOURCE
#endif
#include <stdio.h>

#include "printer.h"
#include "types.h"
#include "topdeps.h"
#include "scope.h"
#include "constraints.h"
#include "reflect.h"

#define DEF(t) typ_definition_ignore_any_overlay_const(t)

enum forward {
  FWD_DECLARE_TYPES,
  FWD_DEFINE_DYNS,
  FWD_DEFINE_TYPES,
  FWD_DECLARE_FUNCTIONS,
  FWD_DEFINE_FUNCTIONS,
  FORWARD__NUM,
};

static const char *forward_guards[FORWARD__NUM] = {
  [FWD_DECLARE_TYPES] = "NLANG_DECLARE_TYPES",
  [FWD_DEFINE_DYNS] = "NLANG_DEFINE_DYNS",
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
  [TOVPLUS] = " + ",
  [TOVMINUS] = " - ",
  [TOVUPLUS] = "+",
  [TOVUMINUS] = "-",
  [TOVTIMES] = " * ",
  [TOVDIVIDE] = " / ",
  [TOVMODULO] = " % ",
  [TBWAND] = " & ",
  [TBWOR] = " | ",
  [TBWXOR] = " ^ ",
  [TRSHIFT] = " >> ",
  [TOVLSHIFT] = " << ",
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
  [TOVPLUS_ASSIGN] = " += ",
  [TOVMINUS_ASSIGN] = " -= ",
  [TOVTIMES_ASSIGN] = " *= ",
  [TOVDIVIDE_ASSIGN] = " /= ",
  [TOVMODULO_ASSIGN] = " %= ",
  [TBWAND_ASSIGN] = " &= ",
  [TBWOR_ASSIGN] = " |= ",
  [TBWXOR_ASSIGN] = " ^= ",
  [TRSHIFT_ASSIGN] = " >>= ",
  [TOVLSHIFT_ASSIGN] = " <<= ",
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
  [TDOTDOT] = "..",
  [TBEGDOTDOT] = "..",
  [TENDDOTDOT] = "..",
  [TDOTDOTDOT] = "...",
  [TSLICEBRAKETS] = "[]",
  [TRSBRA] = "]",
  [TLCBRA] = "{",
  [TRCBRA] = "}",
  [TLPAR] = "(",
  [TRPAR] = ")",
};

static char *escape_string(const char *s) {
  char *r = calloc(2 * strlen(s) + 1, sizeof(char));

  char delim = s[0];
  assert(delim == '\'' || delim == '"');

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

EXAMPLE(escape_string) {
  assert(strcmp("abc", escape_string("\"abc\"")) == 0);
  assert(strcmp("ab'c", escape_string("\"ab'c\"")) == 0);
  assert(strcmp("abc", escape_string("'abc'")) == 0);
  assert(strcmp("", escape_string("\"\"")) == 0);
  assert(strcmp("", escape_string("''")) == 0);
  assert(strcmp("'", escape_string("'\\''")) == 0);
  assert(strcmp("\\'", escape_string("\"\\'\"")) == 0);
  assert(strcmp("\\\"", escape_string("\"\\\"\"")) == 0);
  assert(strcmp("\n", escape_string("'\n'")) == 0);
  assert(strcmp("t00/automagicref.n:76:10",
                escape_string("\"t00/automagicref.n:76:10\"")) == 0);
}

static void print_scope_name(FILE *out, const struct module *mod,
                             const struct scope *scope);

static void print_scope_last_name(FILE *out, const struct module *mod,
                                  const struct scope *scope) {
  const ident id = node_ident(scope_node_const(scope));
  const char *name = idents_value(mod->gctx, id);
  if (id == ID_ANONYMOUS) {
    return;
  }

  if (name[0] == '`') {
    fprintf(out, "_$Ni_%s", name + 1);
  } else {
    fprintf(out, "%s", name);
  }
}

static void print_scope_name(FILE *out, const struct module *mod,
                             const struct scope *scope) {
  if (parent_const(parent_const(scope_node_const(scope))) != NULL) {
    print_scope_name(out, mod, &parent_const(scope_node_const(scope))->scope);
    if (node_ident(scope_node_const(scope)) != ID_ANONYMOUS) {
      fprintf(out, "$");
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

static bool is_in_topmost_module(const struct typ *t) {
  const struct module *mod = node_module_owner_const(DEF(t));
  return mod->stage->printing_mod == mod;
}

static void print_expr(FILE *out, const struct module *mod,
                       const struct node *node, uint32_t parent_op);
static void print_block(FILE *out, const struct module *mod,
                        const struct node *node);
static void print_typ(FILE *out, const struct module *mod, const struct typ *typ);
static void print_typeconstraint(FILE *out, const struct module *mod,
                                 const struct node *node);
static void print_ident(FILE *out, const struct module *mod,
                        const struct node *node);
static void print_defname(FILE *out, bool header, enum forward fwd,
                          const struct module *mod, const struct node *node);
static void print_statement(FILE *out, const struct module *mod,
                            const struct node *node);
static void print_top(FILE *out, bool header, enum forward fwd,
                      const struct module *mod, const struct node *node,
                      struct fintypset *printed, bool force);
static void print_defchoice_path(FILE *out,
                                 const struct module *mod,
                                 const struct node *deft,
                                 const struct node *ch);
static void print_defintf(FILE *out, bool header, enum forward fwd,
                          const struct module *mod, const struct node *node);

static bool ident_is_spurious_ssa_var(const struct node *node) {
  assert(node->which == IDENT);
  return typ_equal(node->typ, TBI_VOID)
    && node->as.IDENT.def->which == DEFNAME
    && node->as.IDENT.def->as.DEFNAME.ssa_user != NULL;
}

static void print_bin_sym(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.BIN.operator;
  const struct node *left = subs_first_const(node);
  const struct node *right = subs_last_const(node);
  if (op == TASSIGN && typ_equal(left->typ, TBI_VOID)) {
    assert(ident_is_spurious_ssa_var(left));

    if (right->which == IDENT) {
      assert(ident_is_spurious_ssa_var(right));
    } else {
      print_expr(out, mod, right, T__STATEMENT);
    }
  } else if (OP_IS_ASSIGN(op)
             && (right->which == INIT
                 || (right->which == CALL
                     && !typ_isa(right->typ, TBI_RETURN_BY_COPY)))) {
    print_expr(out, mod, right, T__STATEMENT);
  } else if (op == TEQMATCH || op == TNEMATCH) {
    const char *cop = op == TEQMATCH ? "==" : "!=";

    fprintf(out, "(");
    print_expr(out, mod, left, T__STATEMENT);
    fprintf(out, ").%s %s ", idents_value(mod->gctx, ID_TAG), cop);

    const struct node *d = DEF(left->typ);
    const struct node *ch = node_get_member_const(d, node_ident(right));
    print_defchoice_path(out, mod, d, ch);
    fprintf(out, "$%s", idents_value(mod->gctx, ID_TAG));
  } else if (op == TEQPTR || op == TNEPTR) {
    if (typ_is_dyn(left->typ) && left->which != NUL) {
      fprintf(out, "(");
      print_expr(out, mod, left, op);
      fprintf(out, ").obj");
    } else {
      print_expr(out, mod, left, op);
    }
    print_token(out, op);
    if (typ_is_dyn(right->typ) && right->which != NUL) {
      fprintf(out, "(");
      print_expr(out, mod, right, op);
      fprintf(out, ").obj");
    } else {
      print_expr(out, mod, right, op);
    }
  } else {
    const bool is_assign = OP_IS_ASSIGN(op);
    if (is_assign) {
      fprintf(out, "(void) ( ");
    }
    if (node_ident(left) != ID_OTHERWISE) {
      print_expr(out, mod, left, op);
      print_token(out, op);
    }
    print_expr(out, mod, right, op);
    if (is_assign) {
      fprintf(out, " )");
    }
  }
}

static void print_union_access_path(FILE *out, const struct module *mod,
                                    const struct typ *t, ident tag) {
  if (typ_is_reference(t)) {
    t = typ_generic_arg_const(t, 0);
  }

  const struct node *d = DEF(t);
  const struct node *defch = node_get_member_const(d, tag);

  struct node *field = NULL;
  while (true) {
    error e = scope_lookup_ident_immediate(&field, NULL, mod, &defch->scope,
                                           tag, true);
    if (!e) {
      break;
    }

    assert(defch->which != DEFTYPE);

    defch = parent_const(defch);
  }

  struct vecnode stack = { 0 };
  const struct node *p = field;
  while (p->which != DEFTYPE) {
    vecnode_push(&stack, CONST_CAST(p));
    p = parent_const(p);
  }

  for (size_t n = vecnode_count(&stack); n > 0; --n) {
    p = *vecnode_get(&stack, n - 1);
    fprintf(out, "as.%s", idents_value(mod->gctx, node_ident(p)));
  }
  vecnode_destroy(&stack);
}

static void print_union_init_access_path(FILE *out, const struct module *mod,
                                         const struct node *node) {
  assert(node->which == INIT);

  const ident tag = node->as.INIT.for_tag;
  if (tag == ID__NONE) {
    return;
  }

  print_union_access_path(out, mod, node->typ, tag);
  fprintf(out, ".");
}

static void print_bin_acc(FILE *out, const struct module *mod,
                          const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.BIN.operator;
  const struct node *base = subs_first_const(node);
  const struct node *name = subs_last_const(node);
  const char *name_s = idents_value(mod->gctx, node_ident(name));

  const struct node *d = DEF(node->typ);
  const bool is_enum = d->which == DEFTYPE && d->as.DEFTYPE.kind == DEFTYPE_ENUM;

  if ((is_enum && (node->flags & NODE_IS_DEFCHOICE))
      || (node->flags & NODE_IS_GLOBAL_LET)) {
    print_typ(out, mod, base->typ);
    fprintf(out, "$%s", name_s);
  } else if (node->flags & NODE_IS_TYPE) {
    print_typ(out, mod, node->typ);
  } else {
    const char *deref = ".";
    if (typ_is_reference(base->typ)) {
      deref = "->";
    }
    print_expr(out, mod, base, op);
    fprintf(out, "%s", deref);
    if (node->flags & NODE_IS_DEFCHOICE) {
      print_union_access_path(out, mod, base->typ, node_ident(name));
    } else {
      fprintf(out, "%s", name_s);
    }
  }
}

static void print_bin_isa(FILE *out, const struct module *mod, const struct node *node) {
  const struct node *left = subs_first_const(node);
  const struct node *right = subs_last_const(node);

  fprintf(out, "n$reflect$Isa((void *)(");
  if (left->flags & NODE_IS_TYPE) {
    const struct typ *t = typ_is_dyn(left->typ) ? typ_generic_arg(left->typ, 0) : left->typ;
    fprintf(out, "&");
    print_typ(out, mod, t);
    fprintf(out, "$Reflect_type)");
  } else {
    print_expr(out, mod, left, T__STATEMENT);
    fprintf(out, ").dyntable");
  }
  fprintf(out, ", (void *)&");
  print_typ(out, mod, right->typ);
  fprintf(out, "$Reflect_type)");
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
  case OP_BIN_SYM_INTARITH:
  case OP_BIN_SYM_OVARITH:
  case OP_BIN_SYM_BW:
  case OP_BIN_SYM_PTR:
  case OP_BIN_BW_RHS_UNSIGNED:
  case OP_BIN_OVBW_RHS_UNSIGNED:
    print_bin_sym(out, mod, node, parent_op);
    break;
  case OP_BIN_ACC:
    print_bin_acc(out, mod, node, parent_op);
    break;
  case OP_BIN_RHS_TYPE:
    if (op == Tisa) {
      print_bin_isa(out, mod, node);
    } else {
      print_typeconstraint(out, mod, node);
    }
    break;
  }

  if (!OP_IS_ASSIGN(op)) {
    fprintf(out, ")");
  }
}

static void print_un(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.UN.operator;
  const struct node *term = subs_first_const(node);

  fprintf(out, "(");

  switch (OP_KIND(op)) {
  case OP_UN_NULLABLE:
    print_expr(out, mod, term, parent_op);
    break;
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
  case OP_UN_OVARITH:
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

static void print_call_vararg_count(FILE *out, const struct node *dfun,
                                    const struct node *node, const struct node *arg,
                                    size_t n) {
  const ssize_t first_vararg = node_fun_first_vararg(dfun);
  if (n != first_vararg) {
    return;
  }

  const ssize_t count = subs_count(node) - 1 - first_vararg;
  if (count == 1 && arg->which == CALLNAMEDARG && arg->as.CALLNAMEDARG.is_slice_vararg) {
    bool passdown = false;
    if (typ_is_reference(arg->typ)) {
      const struct typ *va = typ_generic_arg_const(arg->typ, 0);
      if (typ_generic_arity(va) > 0
          && typ_equal(typ_generic_functor_const(va), TBI_VARARG)) {
        passdown = true;
        fprintf(out, "NLANG_BUILTINS_VACOUNT_VARARGREF");
      }
    }

    if (!passdown) {
      fprintf(out, "NLANG_BUILTINS_VACOUNT_SLICE");
    }
  } else {
    fprintf(out, "%zd", count);
  }

  if (count != 0) {
    fprintf(out, ", ");
  }
}

static const struct node *call_dyn_expr(const struct node *node) {
  const struct node *fun = subs_first_const(node);
  const struct typ *tfun = fun->typ;
  if (typ_definition_which(tfun) == DEFFUN) {
    return subs_first_const(subs_first_const(node));
  } else {
    return subs_at_const(node, 1);
  }
}

static void print_call_fun(FILE *out, const struct module *mod,
                           const struct node *node) {
  const struct node *fun = subs_first_const(node);
  const struct typ *tfun = fun->typ;
  struct tit *it = typ_definition_parent(tfun);
  if (tit_which(it) != DEFINTF) {
    // Normal case.
    tit_next(it);
    print_typ(out, mod, tfun);
    return;
  }

  // Dyn case.
  tit_next(it);
  print_expr(out, mod, call_dyn_expr(node), T__CALL);
  fprintf(out, ".dyntable->%s", idents_value(mod->gctx, typ_definition_ident(tfun)));
}

static void print_call(FILE *out, const struct module *mod,
                       const struct node *node, uint32_t parent_op) {
  const struct node *fun = subs_first_const(node);
  const struct typ *tfun = fun->typ;
  assert(typ_is_concrete(tfun));
  const struct node *dfun = DEF(tfun);
  const struct node *parentd = parent_const(dfun);

  if (node_ident(dfun) == ID_CAST) {
    fprintf(out, "(");
    print_typ(out, mod, node->typ);
    fprintf(out, ")(");
    print_expr(out, mod, subs_at_const(node, 1), T__CALL);
    fprintf(out, ")");
    return;
  } else if (node_ident(dfun) == ID_DYNCAST) {
    const struct typ *i = typ_generic_arg_const(tfun, 0);
    fprintf(out, "NLANG_MKDYN(_$Ndyn_");
    print_typ(out, mod, i);
    fprintf(out, ", n$reflect$Get_dyntable_for((void *)(");
    print_expr(out, mod, subs_at_const(node, 1), T__CALL);
    fprintf(out, ").dyntable, (void *)&");
    print_typ(out, mod, i);
    fprintf(out, "$Reflect_type), (");
    print_expr(out, mod, subs_at_const(node, 1), T__CALL);
    fprintf(out, ").obj)");
    return;
  } else if (node_ident(dfun) == ID_NEXT && typ_isa(parentd->typ, TBI_VARARG)) {
    const struct node *self = subs_at_const(node, 1);
    if (typ_is_dyn(typ_generic_arg(parentd->typ, 0))) {
      fprintf(out, "NLANG_BUILTINS_VARARG_NEXT_DYN(");
    } else {
      fprintf(out, "NLANG_BUILTINS_VARARG_NEXT(");
    }
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

  const ssize_t first_vararg = node_fun_first_vararg(dfun);

  print_call_fun(out, mod, node);
  fprintf(out, "(");

  size_t n = 1;
  bool force_comma = false;

  if (parentd->which == DEFINTF) {
    if (dfun->which == DEFMETHOD) {
      print_expr(out, mod, call_dyn_expr(node), T__CALL);
      fprintf(out, ".obj");
      n = 2;
    }
  }

  FOREACH_SUB_EVERY_CONST(arg, node, n, 1) {
    if (force_comma || n > 1) {
      fprintf(out, ", ");
    }

    print_call_vararg_count(out, dfun, node, arg, n - 1);

    print_expr(out, mod, arg, T__CALL);
    n += 1;
  }

  if ((ssize_t)subs_count(node) - 1 <= first_vararg) {
    if (n > 1) {
      fprintf(out, ", ");
    }
    fprintf(out, "0");
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
  const struct node *par = parent_const(node);
  if (par->which == BIN && OP_IS_ASSIGN(par->as.BIN.operator)) {
    const struct node *target = node->as.INIT.target_expr;
    print_expr(out, mod, target, T__STATEMENT);
  }

  if (!subs_count_atleast(node, 1)) {
    fprintf(out, "{ 0 }");
    return;
  }

  const struct node *el = subs_first_const(node);
  assert(typ_isa(el->typ, TBI_TRIVIAL_COPY) && "not yet supported");

  fprintf(out, " = (const ");
  print_typ(out, mod, node->typ);
  fprintf(out, "){ (");
  print_typ(out, mod, el->typ);
  fprintf(out, "[]){ ");

  FOREACH_SUB_CONST(s, node) {
    print_expr(out, mod, s, T__NOT_STATEMENT);
    fprintf(out, ", ");
  }
  fprintf(out, " }, %zu, %zu }\n", subs_count(node), subs_count(node));
}

static void print_tag_init(FILE *out, const struct module *mod,
                           const struct node *node, bool is_inline) {
  assert(node->which == INIT);
  const struct node *d = DEF(node->typ);
  if (d->which != DEFTYPE || d->as.DEFTYPE.kind != DEFTYPE_UNION) {
    return;
  }

  const ident tag = node->as.INIT.for_tag;
  if (tag == ID__NONE) {
    // We rely on constraints to catch the cases where this would create a
    // badly initialized enum/union. But in a case like:
    //  let what such
    //    if ...
    //      what = A
    //    else
    //      what = B
    // This is overall well-formed.
    return;
  }

  const struct node *ch = node_get_member_const(d, tag);

  if (!is_inline) {
    print_expr(out, mod, node->as.INIT.target_expr, TDOT);
  }
  fprintf(out, ".%s = ", idents_value(mod->gctx, ID_TAG));
  print_defchoice_path(out, mod, d, ch);
  fprintf(out, "$%s", idents_value(mod->gctx, ID_TAG));
  if (!is_inline) {
    fprintf(out, ";\n");
  }
}

static void print_init_toplevel(FILE *out, const struct module *mod,
                                const struct node *node) {
  if (!subs_count_atleast(node, 1)) {
    fprintf(out, " = { 0 }");
    return;
  }

  // FIXME: unions unsupported (except trivial init)
  assert(subs_count(node) == 0 || node->as.INIT.for_tag == ID__NONE);

  fprintf(out, " = {\n");
  print_tag_init(out, mod, node, true);
  FOREACH_SUB_EVERY_CONST(s, node, 0, 2) {
    fprintf(out, ".");
    fprintf(out, "%s", idents_value(mod->gctx, node_ident(s)));
    fprintf(out, " = ");
    print_expr(out, mod, next_const(s), T__NOT_STATEMENT);
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

  const struct node *par = parent_const(node);
  const struct node *context = parent_const(parent_const(par));
  if (par->which == DEFNAME) {
    switch (context->which) {
    case MODULE_BODY:
    case DEFTYPE:
      print_init_toplevel(out, mod, node);
      return;
    default:
      break;
    }
  }

  if (typ_equal(node->typ, TBI_VOID)) {
    return;
  }

  if (par->which == DEFNAME || par->which == BLOCK) {
    fprintf(out, "= { 0 };\n");
  }

  print_tag_init(out, mod, node, false);

  // FIXME: hierarchical unions unsupported

  if (node->as.INIT.is_defchoice_external_payload_constraint) {
    print_expr(out, mod, node->as.INIT.target_expr, TDOT);
    fprintf(out, ".");
    const ident tag = node->as.INIT.for_tag;
    print_union_access_path(out, mod, node->typ, tag);
    fprintf(out, " = ");
    print_expr(out, mod, subs_first_const(node), T__NOT_STATEMENT);
    fprintf(out, ";\n");
    return;
  }

  FOREACH_SUB_EVERY_CONST(s, node, 0, 2) {
    print_expr(out, mod, node->as.INIT.target_expr, TDOT);
    fprintf(out, ".");
    print_union_init_access_path(out, mod, node);
    fprintf(out, "%s", idents_value(mod->gctx, node_ident(s)));
    fprintf(out, " = ");
    print_expr(out, mod, next_const(s), T__NOT_STATEMENT);
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
    fprintf(out, "_$Nmain");
  } else if (intf_final_typ != NULL) {
    print_typ(out, mod, intf_final_typ);
    fprintf(out, "$%s", idents_value(mod->gctx, node_ident(node)));
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
    fprintf(out, "$");
  }

  print_scope_last_name(out, mod, &node->scope);
}

static void print_dyn(FILE *out, const struct module *mod, const struct node *node) {
  const struct node *arg = subs_first_const(node);
  const struct typ *intf = typ_generic_arg_const(node->typ, 0);
  const struct typ *concrete = typ_generic_arg_const(arg->typ, 0);

  fprintf(out, "NLANG_MKDYN(_$Ndyn_");
  print_typ(out, mod, intf);
  fprintf(out, ", &");
  print_typ(out, mod, concrete);
  fprintf(out, "$Dyntable__");
  print_typ(out, mod, intf);
  fprintf(out, ", (void *)");
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
      if (typ_equal(node->typ, TBI_STRING)) {
        fprintf(out, "NLANG_STRING_LITERAL(\"%s\")", s);
      } else {
        assert(false);
      }
      free(s);
    }
    break;
  case SIZEOF:
    fprintf(out, "sizeof(");
    print_typ(out, mod, subs_first_const(node)->typ);
    fprintf(out, ")");
    break;
  case ALIGNOF:
    fprintf(out, "__alignof__(");
    print_typ(out, mod, subs_first_const(node)->typ);
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
    print_expr(out, mod, subs_first_const(node), parent_op);
    break;
  case TUPLE:
    print_tuple(out, mod, node, parent_op);
    break;
  case INIT:
    print_init(out, mod, node);
    break;
  case DYN:
    print_dyn(out, mod, node);
    break;
  case BLOCK:
    {
      print_block(out, mod, node);
    }
    break;
  case IF:
  case TRY:
  case MATCH:
  case ASSERT:
  case PRE:
  case POST:
  case INVARIANT:
    fprintf(out, "({ ");
    print_statement(out, mod, node);
    fprintf(out, "; })");
    break;
  default:
    fprintf(g_env.stderr, "Unsupported node: %d\n", node->which);
    assert(false);
  }
}

static void print_while(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "while (");
  print_expr(out, mod, subs_first_const(node), T__STATEMENT);
  fprintf(out, ")");
  print_block(out, mod, subs_at_const(node, 1));
}

static void print_if(FILE *out, const struct module *mod, const struct node *node) {
  const struct node *n = subs_first_const(node);
  const size_t count = subs_count(node);
  const size_t br_count = count / 2 + count % 2;

  print_block(out, mod, n);
  fprintf(out, " ? ");
  n = next_const(n);
  print_block(out, mod, n);

  if (br_count == 1) {
    fprintf(out, " : ({;}) )");
    return;
  }

  n = next_const(n);
  while (n != NULL && next_const(n) != NULL) {
    fprintf(out, "\n");
    fprintf(out, " : ");
    print_expr(out, mod, n, T__STATEMENT);
    fprintf(out, " ? ");
    n = next_const(n);
    print_block(out, mod, n);
    n = next_const(n);
  }

  if (n != NULL) {
    fprintf(out, "\n");
    fprintf(out, " : ");
    print_block(out, mod, n);
  } else {
    fprintf(out, " : ({;})");
  }
}

static void print_defchoice_path(FILE *out,
                                 const struct module *mod,
                                 const struct node *deft,
                                 const struct node *ch) {
  print_deftype_name(out, mod, deft);

  if (ch == deft) {
    return;
  }

  fprintf(out, "$");
  print_deffield_name(out, mod, ch);
}

static void print_match_label(FILE *out, const struct module *mod,
                              const struct node *label) {
  if (node_ident(label) == ID_OTHERWISE) {
    return;
  }

  const struct node *id = label;
  if (id->which == BIN) {
    id = subs_last_const(id);
  }
  const struct node *deft = DEF(label->typ);
  const struct node *ch = node_get_member_const(deft, node_ident(id));

  assert(ch->which == DEFCHOICE);
  if (ch->as.DEFCHOICE.is_leaf) {
    fprintf(out, "case ");
    print_defchoice_path(out, mod, deft, ch);
    fprintf(out, "$%s_label__", idents_value(mod->gctx, ID_TAG));
    fprintf(out, ":\n");
    return;
  }

  FOREACH_SUB_CONST(s, ch) {
    if (s->which == DEFCHOICE) {
      print_match_label(out, mod, s);
    }
  }
}

static void print_match_expr(FILE *out, const struct module *mod,
                             const struct node *node) {
  const struct node *expr = subs_first_const(node);

  fprintf(out, "if (0) {\n");
  const struct node *n = next_const(expr);
  while (n != NULL) {
    const struct node *p = n;
    const struct node *block = next_const(n);

    if (next_const(block) == NULL) {
      fprintf(out, "} else {");
    } else {
      fprintf(out, "} else if (");
      print_expr(out, mod, p, T__CALL);
      fprintf(out, ") {");
    }

    print_block(out, mod, block);
    n = next_const(block);
  }
  fprintf(out, "}");
}

static void print_match(FILE *out, const struct module *mod,
                        const struct node *node) {
  const struct node *expr = subs_first_const(node);
  if (typ_definition_deftype_kind(expr->typ) == DEFTYPE_STRUCT) {
    print_match_expr(out, mod, node);
    return;
  }

  const struct node *dexpr = DEF(expr->typ);
  fprintf(out, "switch (");
  print_expr(out, mod, expr, T__STATEMENT);
  if (dexpr->which == DEFTYPE && dexpr->as.DEFTYPE.kind == DEFTYPE_UNION) {
    fprintf(out, ".tag");
  }
  fprintf(out, ") {\n");

  const struct node *n = next_const(expr);
  while (n != NULL) {
    const struct node *p = n;
    const struct node *block = next_const(n);

    print_match_label(out, mod, p);

    if (next_const(block) == NULL) {
      fprintf(out, "default:\n");
    }

    print_block(out, mod, block);
    fprintf(out, "break;\n");

    n = next_const(block);
  }
  fprintf(out, "}");
}

static void print_try(FILE *out, const struct module *mod, const struct node *node) {
  const struct node *err = subs_at_const(node, 1);
  print_statement(out, mod, err);

  const struct node *eblock = subs_last_const(node);
  print_block(out, mod, subs_first_const(eblock));

  fprintf(out, "while (0) {\n");
  FOREACH_SUB_EVERY_CONST(catch, eblock, 1, 1) {
    fprintf(out, "\n%s: {\n", idents_value(mod->gctx, catch->as.CATCH.label));
    print_block(out, mod, subs_first_const(catch));
    fprintf(out, "\n}\n");
  }
  fprintf(out, "}\n");
}

static void print_assert(FILE *out, const struct module *mod, const struct node *node) {
  print_block(out, mod, subs_first_const(node));
}

static void print_pre(FILE *out, const struct module *mod, const struct node *node) {
  print_block(out, mod, subs_first_const(node));
}

static void print_post(FILE *out, const struct module *mod, const struct node *node) {
  print_block(out, mod, subs_first_const(node));
}

static void print_invariant(FILE *out, const struct module *mod, const struct node *node) {
  print_block(out, mod, subs_first_const(node));
}

#define ATTR_SECTION_EXAMPLES "__attribute__((section(\".text.n.examples\")))"

static void print_example(FILE *out, bool header, enum forward fwd,
                          const struct module *mod, const struct node *node) {
  if (header) {
    return;
  }

  if (fwd == FWD_DECLARE_FUNCTIONS
      || fwd == FWD_DEFINE_FUNCTIONS) {
    fprintf(out, "void ");
    print_scope_name(out, mod, &mod->root->scope);
    fprintf(out, "_$Nexample%zu(void) ", node->as.EXAMPLE.name);

    if (fwd == FWD_DECLARE_FUNCTIONS) {
      fprintf(out, ATTR_SECTION_EXAMPLES ";");
    } else if (fwd == FWD_DEFINE_FUNCTIONS) {
      fprintf(out, "{\n");
      const struct node *block = subs_first_const(node);
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
    return
      (header && !(node_is_export(node) && node_is_inline(node)))
      || node_is_prototype(node);
  }
}

static void print_generic_linkage(FILE *out, bool header, enum forward fwd,
                                  const struct node *at_top,
                                  const struct node *node) {
  if (NM(node->which) & (NM(DEFFUN) | NM(DEFMETHOD))) {
    if (node_is_inline(node)) {
      fprintf(out, "static inline ");
    } else {
      if (fwd == FWD_DECLARE_FUNCTIONS) {
        fprintf(out, "__attribute__((__weak__)) ");
      }
    }
  }
}

static void print_linkage(FILE *out, bool header, enum forward fwd,
                          const struct node *at_top,
                          const struct node *node) {
  if (typ_generic_arity(node->typ) > 0
      || (!node_is_at_top(node) && typ_generic_arity(parent_const(node)->typ) > 0)) {
    print_generic_linkage(out, header, fwd, at_top, node);
    return;
  }

  const struct toplevel *toplevel = node_toplevel_const(at_top);
  const uint32_t flags = toplevel->flags;
  if ((NM(node->which) & (NM(DEFFUN) | NM(DEFMETHOD)))) {
    if (((flags & TOP_IS_EXPORT) && (flags & TOP_IS_INLINE))
        || ((flags & TOP_IS_EXTERN) && (flags & TOP_IS_INLINE))) {
      fprintf(out, "static inline ");
    } else if ((flags & TOP_IS_EXTERN) && (flags & TOP_IS_EXPORT)) {
        fprintf(out, "extern ");
    } else if (flags & TOP_IS_EXPORT) {
      // noop
    } else {
      fprintf(out, "static ");
    }
  } else if (flags & TOP_IS_EXTERN) {
    fprintf(out, "extern ");
  } else if ((flags & TOP_IS_INLINE) && node->which != DEFNAME) {
    fprintf(out, "static inline ");
  } else if (node_is_at_top(node) && !(flags & TOP_IS_EXPORT)) {
    fprintf(out, "static ");
  }
}

static const struct typ *intercept_slices(const struct module *mod, const struct typ *t) {
  const struct node *d = DEF(t);

  if (node_is_at_top(d) && typ_generic_arity(t) == 0) {
    return t;
  }

  if (!node_is_at_top(d) && NM(d->which) & (NM(DEFFUN) | NM(DEFMETHOD))) {
    const struct node *pd = parent_const(d);
    const struct typ *pt = intercept_slices(mod, pd->typ);
    if (pt == pd->typ) {
      return t;
    }

    const struct node *m = node_get_member_const(DEF(pt),
                                                 node_ident(d));
    const struct typ *mt = m->typ;
    if (typ_generic_arity(mt) == 0) {
      return mt;
    }

    const size_t arity = typ_generic_arity(t);
    struct typ **args = calloc(arity, sizeof(struct typ *));
    for (size_t a = 0; a < arity; ++a) {
      args[a] = typ_generic_arg(CONST_CAST(t), a);
    }

    struct typ *r = instances_find_existing_final_with(CONST_CAST(m)->typ, args, arity);
    assert(r);
    free(args);
    return r;
  }

  if (!typ_is_slice(t)) {
    return t;
  }

  if (d->which != DEFTYPE) {
    return t;
  }

  if (typ_is_generic_functor(t)) {
    return TBI_SLICE_IMPL;
  } else {
    const char name[] = "create_impl_instance";
    const ident nameid = idents_add_string(mod->gctx, name, ARRAY_SIZE(name)-1);
    const struct node *m = node_get_member_const(d, nameid);
    return node_fun_retval_const(m)->typ;
  }
}

static void print_typ_name(FILE *out, const struct module *mod,
                           const struct typ *t) {
  if (typ_generic_arity(t) > 0 && !typ_is_generic_functor(t)) {
    print_typ_name(out, mod, typ_generic_functor_const(t));
    return;
  }

  t = intercept_slices(mod, t);

  const struct scope *scope = &DEF(t)->scope;
  print_scope_name(out, mod, scope);
}

static void print_typ_function(FILE *out, const struct module *mod, const struct typ *typ) {
  const struct node *def = DEF(typ);

  if (typ_generic_arity(typ) > 0) {
    fprintf(out, "_$Ngen_");
  }

  if (node_is_at_top(def)) {
    print_typ_name(out, mod, typ);
  } else {
    const struct node *par = parent_const(def);
    const struct typ *tparent = par->typ;
    print_typ(out, mod, tparent);
    if (par->which == DEFCHOICE) {
      fprintf(out, "$%s", idents_value(mod->gctx, node_ident(par)));
    }
    fprintf(out, "$%s", idents_value(mod->gctx, node_ident(def)));
  }

  if (typ_generic_arity(typ) > 0) {
    for (size_t n = 0; n < typ_generic_arity(typ); ++n) {
      fprintf(out, "$$");
      print_typ(out, mod, typ_generic_arg_const(typ, n));
    }
    fprintf(out, "_genN$_");
  }
}

static void print_typ_data(FILE *out, const struct module *mod, const struct typ *typ) {
  if (typ_is_generic_functor(typ)) {
    print_typ_name(out, mod, typ);
    return;
  } else if (typ_is_dyn(typ)) {
    fprintf(out, "_$Ndyn_");
    print_typ(out, mod, typ_generic_arg_const(typ, 0));
    return;
  } else if (typ_is_reference(typ)) {
    fprintf(out, "_$Nref_");
    print_typ(out, mod, typ_generic_arg_const(typ, 0));
    return;
  }

  if (typ_generic_arity(typ) > 0) {
    fprintf(out, "_$Ngen_");
  }

  print_typ_name(out, mod, typ);

  if (typ_generic_arity(typ) > 0) {
    for (size_t n = 0; n < typ_generic_arity(typ); ++n) {
      fprintf(out, "$$");
      print_typ(out, mod, typ_generic_arg_const(typ, n));
    }
    fprintf(out, "_genN$_");
  }
}

static void print_import_path(FILE *out, const struct module *mod,
                              const struct node *node) {
  switch (node->which) {
  case IMPORT:
    print_import_path(out, mod, subs_first_const(node));
    break;
  case BIN:
    print_import_path(out, mod, subs_first_const(node));
    fprintf(out, "$");
    print_import_path(out, mod, subs_last_const(node));
    break;
  case IDENT:
    print_ident(out, mod, node);
    break;
  default:
    assert(false);
  }
}

static void print_typ(FILE *out, const struct module *mod, const struct typ *typ) {
  const struct node *d = DEF(typ);
  if (d->which == IMPORT) {
    print_import_path(out, mod, d);
    return;
  }

  if (typ_is_function(typ)) {
    print_typ_function(out, mod, typ);
  } else {
    print_typ_data(out, mod, typ);
  }
}

static void print_defname(FILE *out, bool header, enum forward fwd,
                          const struct module *mod, const struct node *node) {
  if (node->which == DEFALIAS) {
    return;
  }

  if (fwd != FWD_DECLARE_FUNCTIONS && fwd != FWD_DEFINE_FUNCTIONS) {
    return;
  }

  const struct node *expr = subs_last_const(node);
  const struct node *par = parent_const(node);

  if (node->flags & NODE_IS_GLOBAL_LET) {
    if (header && !node_is_export(par)) {
      return;
    } else if (header && fwd == FWD_DEFINE_FUNCTIONS) {
      // Even if it is inline, the value is set in the .c
      // This is a lost optimization opportunity for the C compiler, but
      // we're not sure of the implications of making the value visible in
      // the header (using a #define).
      return;
    } else if (!header && fwd == FWD_DECLARE_FUNCTIONS
               && node_is_export(par)) {
      return;
    } else if (!header && node_is_extern(par)) {
      return;
    }
  }

  assert(node->which == DEFNAME);

  if (node_ident(node) == ID_OTHERWISE) {
    if (expr != NULL) {
      if (expr->which == BLOCK && !subs_count_atleast(expr, 2)) {
        expr = subs_first_const(expr);
      }
      fprintf(out, "(void) ");
      print_expr(out, mod, expr, T__STATEMENT);
    }
    return;
  }

  const bool is_void = typ_equal(node->typ, TBI_VOID);
  if (!is_void) {
    if (node->flags & NODE_IS_GLOBAL_LET) {
      print_linkage(out, header, fwd, par, node);
    }

    if (node->as.DEFNAME.may_be_unused) {
      fprintf(out, "__attribute__((__unused__)) ");
    }

    print_typ(out, mod, node->typ);
    fprintf(out, " ");

    if (node->flags & NODE_IS_GLOBAL_LET) {
      print_scope_name(out, mod, &parent_const(parent_const(node))->scope);
      fprintf(out, "$");
    }
    print_expr(out, mod, subs_first_const(node), T__STATEMENT);
  }

  if (fwd == FWD_DEFINE_FUNCTIONS
      && (!header || node_is_inline(node))
      && !(node_toplevel_const(par)->flags & TOP_IS_EXTERN)) {
    if (expr != NULL) {
      if (expr->which == BLOCK && !subs_count_atleast(expr, 2)) {
        expr = subs_first_const(expr);
      }

      if (expr->which == INIT) {
        print_init(out, mod, expr);
      } else if (expr->which == CALL
                 && !typ_isa(expr->typ, TBI_RETURN_BY_COPY)) {
        if (!is_void) {
          fprintf(out, " = { 0 };\n");
        }
        print_expr(out, mod, expr, T__STATEMENT);
      } else {
        if (!is_void) {
          fprintf(out, " = ");
        }
        print_expr(out, mod, expr, T__STATEMENT);
      }
      fprintf(out, ";\n");
    } else {
      if (!is_void) {
        fprintf(out, " = { 0 };\n");
      }
    }
  } else {
    fprintf(out, ";\n");
  }
}

static void print_return(FILE *out, const struct module *mod, const struct node *node) {
  if (subs_count_atleast(node, 1)) {
    const struct node *expr = subs_first_const(node);
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
  case LET:
    print_defname(out, false, FWD_DEFINE_FUNCTIONS, mod, subs_first_const(node));
    break;
  case RETURN:
    print_return(out, mod, node);
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
  case IF:
    print_if(out, mod, node);
    break;
  case MATCH:
    print_match(out, mod, node);
    break;
  case TRY:
    print_try(out, mod, node);
    break;
  case ASSERT:
    print_assert(out, mod, node);
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
  case IDENT:
    if (ident_is_spurious_ssa_var(node)) {
      break;
    }
    // fallthrough
  case NUMBER:
  case BOOL:
  case STRING:
  case NUL:
  case BIN:
  case UN:
  case CALL:
  case TYPECONSTRAINT:
  case SIZEOF:
  case ALIGNOF:
  case INIT:
    print_expr(out, mod, node, T__STATEMENT);
    break;
  case BLOCK:
    print_block(out, mod, node);
    break;
  case PHI:
    // noop
    break;
  case JUMP:
    if (node->as.JUMP.is_break) {
      fprintf(out, "break");
    } else if (node->as.JUMP.is_continue) {
      fprintf(out, "continue");
    } else {
      fprintf(out, "goto %s", idents_value(mod->gctx, node->as.JUMP.label));
    }
    break;
  default:
    fprintf(g_env.stderr, "Unsupported node: %d\n", node->which);
    assert(false);
  }
}

static void print_block(FILE *out, const struct module *mod, const struct node *node) {
  assert(node->which == BLOCK);
  const bool value_braces = !typ_equal(node->typ, TBI_VOID) || parent_const(node)->which == IF;
  const bool braces = !node->as.BLOCK.is_scopeless;

  if (value_braces) {
    fprintf(out, "({\n");
  } else if (braces) {
    fprintf(out, "{\n");
  }

  if (node->as.BLOCK.is_scopeless && !subs_count_atleast(node, 2)) {
    print_statement(out, mod, subs_first_const(node));
  } else {
    FOREACH_SUB_CONST(statement, node) {
      print_statement(out, mod, statement);
      fprintf(out, ";\n");
    }
  }

  if (value_braces) {
    fprintf(out, "})\n");
  } else if (braces) {
    fprintf(out, "}\n");
  }
}

static void print_typeconstraint(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, subs_first_const(node), T__STATEMENT);
}

static void print_defarg(FILE *out, const struct module *mod, const struct node *node,
                         bool return_through_ref) {
  if (return_through_ref) {
    if (DEF(node->typ)->which == DEFINTF) {
      fprintf(out, "_$Ndyn_");
    }
  }
  print_typ(out, mod, node->typ);
  fprintf(out, " ");
  if (return_through_ref) {
    fprintf(out, "*_$Nrtr_");
  }
  print_expr(out, mod, subs_first_const(node), T__STATEMENT);
}

static bool print_call_vararg_proto(FILE *out, const struct node *dfun, size_t n) {
  const ssize_t first_vararg = node_fun_first_vararg(dfun);
  if (n != first_vararg) {
    return false;
  }

  fprintf(out, "n$builtins$Int _$Nvacount, ...");
  return true;
}

static void print_fun_prototype(FILE *out, bool header, enum forward fwd,
                                const struct module *mod,
                                const struct node *node,
                                bool as_fun_pointer, bool named_fun_pointer,
                                bool as_dyn_fun_pointer,
                                const struct typ *intf_final_typ) {
  assert(!as_fun_pointer || (named_fun_pointer || as_dyn_fun_pointer));

  const size_t args_count = c_fun_args_count(node);
  const struct node *retval = node_fun_retval_const(node);
  const bool retval_throughref = !typ_isa_return_by_copy(retval->typ);

  if (!as_fun_pointer) {
    print_linkage(out, header, fwd, node, node);
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
    fprintf(out, "_$Ndyn_");
    print_typ(out, mod, intf_final_typ);
    fprintf(out, " self");
    no_args_at_all = false;
    force_comma = true;
  }

  const struct node *funargs = subs_at_const(node, IDX_FUNARGS);
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

    const struct node *arg = subs_at_const(funargs, n);
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

static void print_rtr_helpers_start(FILE *out, const struct module *mod,
                                    const struct node *retval, bool bycopy) {
  const struct node *first = subs_first_const(retval);
  const struct node *last = subs_last_const(retval);
  if (bycopy) {
    fprintf(out, "__attribute__((__unused__)) ");
    print_defarg(out, mod, retval, false);
    fprintf(out, " = { 0 };\n");
  } else {
    fprintf(out, "#define ");
    print_expr(out, mod, first, T__STATEMENT);
    fprintf(out, " (*_$Nrtr_");
    print_expr(out, mod, first, T__STATEMENT);
    fprintf(out, ")\n");
  }

  if (last->which == TUPLE && subs_first_const(last)->which == DEFARG) {
    size_t n = 0;
    FOREACH_SUB_CONST(x, last) {
      assert(x->which == DEFARG && "FIXME(catch in parser): either all named ");

      fprintf(out, "#define ");
      print_expr(out, mod, subs_first_const(x), T__STATEMENT);
      fprintf(out, " ((");
      print_expr(out, mod, first, T__STATEMENT);
      fprintf(out, ").X%zu)\n", n);
      n += 1;
    }
  }
}

static void print_rtr_helpers_end(FILE *out, const struct module *mod,
                                  const struct node *retval, bool bycopy) {
  const struct node *first = subs_first_const(retval);
  const struct node *last = subs_last_const(retval);
  if (bycopy) {
    fprintf(out, "return ");
    print_expr(out, mod, first, T__STATEMENT);
    fprintf(out, ";\n");
  } else {
    fprintf(out, "#undef ");
    print_expr(out, mod, first, T__STATEMENT);
    fprintf(out, "\n");
  }

  if (last->which == TUPLE && subs_first_const(last)->which == DEFARG) {
    FOREACH_SUB_CONST(x, last) {
      fprintf(out, "#undef ");
      print_expr(out, mod, subs_first_const(x), T__STATEMENT);
      fprintf(out, "\n");
    }
  }
}

// Possible forms
//   DEFARG
//    IDENT
//    type
//
//  DEFARG
//   TUPLE
//    TYPE
//    TYPE
//
//  TUPLE
//   DEFARG
//    IDENT
//    type
//   DEFARG
//    IDENT
//    type
static void print_rtr_helpers(FILE *out, const struct module *mod,
                              const struct node *retval, bool start) {
  if (typ_equal(retval->typ, TBI_VOID)) {
    return;
  }

  const bool bycopy = typ_isa(retval->typ, TBI_RETURN_BY_COPY);
  if (start) {
    print_rtr_helpers_start(out, mod, retval, bycopy);
  } else {
    print_rtr_helpers_end(out, mod, retval, bycopy);
  }
}

static void rtr_helpers(FILE *out, const struct module *mod,
                        const struct node *node, bool start) {
  const struct node *retval = node_fun_retval_const(node);
  print_rtr_helpers(out, mod, retval, start);
}

static void print_deffun_builtingen(FILE *out, const struct module *mod, const struct node *node) {
  const struct node *par = parent_const(node);

  fprintf(out, " {\n");
  if (par->which == DEFTYPE || par->which == DEFCHOICE) {
    fprintf(out, "#define THIS(x) ");
    print_typ(out, mod, par->typ);
    fprintf(out, "##x\n");
  }

  rtr_helpers(out, mod, node, true);

  const struct node *funargs = NULL;
  const enum builtingen bg = node_toplevel_const(node)->builtingen;
  switch (bg) {
  case BG_TRIVIAL_CTOR_CTOR:
    break;
  case BG_TRIVIAL_DTOR_DTOR:
    break;
  case BG_TRIVIAL_COPY_COPY_CTOR:
    fprintf(out, "memmove(self, other, sizeof(*self));\n");
    break;
  case BG_TRIVIAL_COMPARE_OPERATOR_COMPARE:
    fprintf(out, "return memcmp(self, other, sizeof(*self));\n");
    break;
  case BG_ENUM_FROM_TAG:
    assert(par->which == DEFTYPE);
    if (par->as.DEFTYPE.kind == DEFTYPE_ENUM) {
      fprintf(out, "return value;\n");
    } else {
      fprintf(out, "return (");
      print_typ(out, mod, par->typ);
      fprintf(out, "){ .tag = value };\n");
    }
    break;
  case BG_ENUM_TAG:
    assert(par->which == DEFTYPE);
    if (par->as.DEFTYPE.kind == DEFTYPE_ENUM) {
      fprintf(out, "return *self;\n");
    } else {
      fprintf(out, "return self->tag;\n");
    }
    break;
  case BG_ENVIRONMENT_PARENT:
    assert(node->which == DEFMETHOD);
    fprintf(out, "NLANG_BUILTINS_BG_ENVIRONMENT_PARENT(");
    print_typ(out, mod,
              typ_generic_arg_const(
                node->as.DEFMETHOD.member_from_intf, 0));
    fprintf(out, ");\n");
    break;
  case BG_ENVIRONMENT_INSTALL:
  case BG_ENVIRONMENT_UNINSTALL:
    funargs = subs_at_const(node, IDX_FUNARGS);
    fprintf(out, "NLANG_BUILTINS_BG_ENVIRONMENT_%sINSTALL(",
            bg == BG_ENVIRONMENT_UNINSTALL ? "UN" : "");
    print_typ(out, mod,
              typ_generic_arg_const(
                typ_generic_arg_const(
                  subs_at_const(funargs, 1)->typ, 0), 0));
    fprintf(out, ");\n");
    break;
  case BG__NOT:
  case BG__NUM:
    assert(false);
  }

  rtr_helpers(out, mod, node, false);

  if (par->which == DEFTYPE || par->which == DEFCHOICE) {
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

static void guard_generic(FILE *out, bool header, enum forward fwd,
                          const struct module *mod,
                          const struct node *node,
                          bool begin) {
  const struct typ *t = node->typ;
  const struct node *par = parent_const(node);

  const char *prefix = "";
  if (node->which == DEFINTF) {
    prefix = "_$Ndyn_";
  } else if (typ_generic_arity(t) == 0 && typ_generic_arity(par->typ) == 0) {
    return;
  }

  if (begin) {
    fprintf(out, "#ifndef HAS%x_%s", fwd, prefix);
    print_typ(out, mod, t);
    fprintf(out, "\n#define HAS%x_%s", fwd, prefix);
    print_typ(out, mod, t);
    fprintf(out, "\n");
  } else {
    fprintf(out, "#endif // HAS%x_%s", fwd, prefix);
    print_typ(out, mod, t);
    fprintf(out, "\n");
  }
}

static void print_deffun(FILE *out, bool header, enum forward fwd,
                         const struct module *mod, const struct node *node,
                         struct fintypset *printed) {
  if (fwd != FWD_DECLARE_FUNCTIONS && fwd != FWD_DEFINE_FUNCTIONS) {
    return;
  }
  if (node_is_extern(node) && fwd == FWD_DEFINE_FUNCTIONS) {
    return;
  }
  if (header && !node_is_export(node)) {
    return;
  }
  if (node_ident(node) == ID_NEXT
      && typ_generic_functor_const(parent_const(node)->typ) != NULL
      && typ_equal(typ_generic_functor_const(parent_const(node)->typ), TBI_VARARG)) {
    // This is a builtin and does not have a real function prototype.
    return;
  }

  const struct node *par = parent_const(node);
  const bool is_gen = typ_generic_arity(node->typ) > 0
    || typ_generic_arity(par->typ) > 0;
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
      || id == ID_DYNCAST
      || id == ID_LIKELY
      || id == ID_UNLIKELY) {
    return;
  }

  if (fwd != FWD_DECLARE_FUNCTIONS && prototype_only(header, node)) {
    return;
  }

  guard_generic(out, header, fwd, mod, node, true);

  if (fwd == FWD_DECLARE_FUNCTIONS) {
    print_fun_prototype(out, header, fwd, mod, node, false, false, false, NULL);
    fun_nonnull_attribute(out, header, mod, node);
    fprintf(out, ";\n");
  } else if (node_toplevel_const(node)->builtingen != BG__NOT) {
    print_fun_prototype(out, header, fwd, mod, node, false, false, false, NULL);
    print_deffun_builtingen(out, mod, node);
  } else {
    print_fun_prototype(out, header, fwd, mod, node, false, false, false, NULL);

    fprintf(out, " {\n");
    if (par->which == DEFTYPE) {
      fprintf(out, "#define THIS(x) ");
      print_typ(out, mod, par->typ);
      fprintf(out, "##x\n");
    }

    rtr_helpers(out, mod, node, true);

    const struct node *funargs = subs_at_const(node, IDX_FUNARGS);
    const ssize_t first_vararg = node_fun_first_vararg(node);
    ident id_ap = ID__NONE;
    if (first_vararg >= 0) {
      const struct node *ap = subs_at_const(funargs, first_vararg);
      id_ap = node_ident(ap);
      print_typ(out, mod, ap->typ);
      fprintf(out, " %s = { 0 };\nNLANG_BUILTINS_VARARG_START(%s);\n",
              idents_value(mod->gctx, id_ap),
              idents_value(mod->gctx, id_ap));
    }

    const struct node *block = subs_last_const(node);
    print_block(out, mod, block);

    fprintf(out, "\n");

    if (first_vararg >= 0) {
      fprintf(out, "NLANG_BUILTINS_VARARG_END(%s);\n",
              idents_value(mod->gctx, id_ap));
    }

    rtr_helpers(out, mod, node, false);

    if (par->which == DEFTYPE) {
      fprintf(out, "#undef THIS\n");
    }
    fprintf(out, "}\n");
  }

  guard_generic(out, header, fwd, mod, node, false);
}

static void print_deffield(FILE *out, const struct module *mod, const struct node *node) {
  print_typ(out, mod, node->typ);
  fprintf(out, " ");
  print_deffield_name(out, mod, node);
}

static void print_delegate(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "delegate ");
  print_expr(out, mod, subs_first_const(node), T__CALL);

  FOREACH_SUB_EVERY_CONST(s, node, 1, 1) {
    fprintf(out, " ");
    print_expr(out, mod, s, T__CALL);
  }
}

static void print_deftype_statement(FILE *out, bool header, enum forward fwd,
                                    const struct module *mod, const struct node *node,
                                    bool do_static, struct fintypset *printed) {
  if (do_static) {
    switch (node->which) {
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
      if (!typ_is_generic_functor(node->typ)) {
        print_top(out, header, fwd, mod, node, printed, false);
      }
      break;
    case LET:
      print_defname(out, header, fwd, mod, subs_first_const(node));
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

static ERROR print_deftype_envparent_eachisalist(struct module *mod,
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
                                const struct node *node, bool do_static, struct fintypset *printed) {
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

static ERROR print_dyntable_proto_eachisalist(struct module *mod, struct typ *t,
                                              struct typ *intf,
                                              bool *stop, void *user) {
  struct cprinter_state *st = user;
  if (typ_is_reference(intf)) {
    return 0;
  }

  fprintf(st->out, "static const struct _$Ndyntable_");
  print_typ(st->out, mod, intf);
  fprintf(st->out, " ");
  print_typ(st->out, mod, t);
  fprintf(st->out, "$Dyntable__");
  print_typ(st->out, mod, intf);
  fprintf(st->out, ";\n");
  return 0;
}

static ERROR print_dyntable_field_eachisalist(struct module *mod, struct typ *ignored,
                                              struct typ *intf,
                                              bool *stop, void *user) {
  struct cprinter_state *st = user;
  struct typ *t = st->user;
  if (typ_is_reference(intf)) {
    return 0;
  }

  const struct node *dintf = DEF(intf);
  FOREACH_SUB_CONST(f, dintf) {
    if (f->which != DEFFUN && f->which != DEFMETHOD) {
      continue;
    }
    if (node_toplevel_const(f)->flags & TOP_IS_NOT_DYN) {
      continue;
    }
    if (subs_count_atleast(subs_at_const(DEF(f->typ), IDX_GENARGS), 1)) {
      continue;
    }

    st->printed += 1;
    const struct node *thisf = node_get_member_const(DEF(t),
                                                     node_ident(f));
    if (!typ_is_concrete(thisf->typ)) {
      return 0;
    }

    fprintf(st->out, ".%s = (", idents_value(mod->gctx, node_ident(thisf)));
    print_fun_prototype(st->out, st->header, st->fwd, mod, f, true, false, true, NULL);
    fprintf(st->out, ")");
    print_typ(st->out, mod, thisf->typ);
    fprintf(st->out, ",\n");
  }

  return 0;
}

static ERROR print_dyntable_eachisalist(struct module *mod, struct typ *t,
                                        struct typ *intf,
                                        bool *stop, void *user) {
  struct cprinter_state *st = user;
  if (typ_is_reference(intf)) {
    return 0;
  }

  fprintf(st->out, "static const struct _$Ndyntable_");
  print_typ(st->out, mod, intf);
  fprintf(st->out, " ");
  print_typ(st->out, mod, t);
  fprintf(st->out, "$Dyntable__");
  print_typ(st->out, mod, intf);
  fprintf(st->out, " = {\n");
  fprintf(st->out, ".type = &");
  print_typ(st->out, mod, t);
  fprintf(st->out, "$Reflect_type,\n");

  struct cprinter_state st2 = *st;
  st2.printed = 0;
  st2.user = (void *)t;


  uint32_t filter = ISALIST_FILTEROUT_PREVENT_DYN;
  const bool is_gen = typ_generic_arity(t) > 0;
  if (!is_gen) {
    filter |= st->header ? ISALIST_FILTEROUT_NOT_EXPORTED : ISALIST_FILTEROUT_EXPORTED;
  }

  error e = typ_isalist_foreach((struct module *)mod, intf, filter,
                                print_dyntable_field_eachisalist,
                                &st2);
  assert(!e);
  e = print_dyntable_field_eachisalist((struct module *)mod,
                                       NULL, intf, NULL, &st2);
  assert(!e);

  if (st2.printed == 0) {
    fprintf(st->out, "0,\n");
  }
  fprintf(st->out, "};\n");
  return 0;
}

static void print_dyntable(FILE *out, bool header, enum forward fwd,
                           const struct module *mod, const struct node *node) {
  if (typ_generic_functor_const(node->typ) == TBI_SLICE_IMPL) {
    return;
  }
  if (typ_isa(node->typ, TBI_ANY_TUPLE)) {
    return;
  }

  uint32_t filter = ISALIST_FILTEROUT_PREVENT_DYN;
  const bool is_gen = typ_generic_arity(node->typ) > 0;
  if (!is_gen) {
    filter |= header ? ISALIST_FILTEROUT_NOT_EXPORTED : ISALIST_FILTEROUT_EXPORTED;
  }

  if (fwd == FWD_DECLARE_FUNCTIONS) {
    struct cprinter_state st = { .out = out, .header = header, .fwd = fwd,
      .mod = NULL, .printed = 0, .user = NULL };
    error e = typ_isalist_foreach((struct module *)mod, node->typ, filter,
                                  print_dyntable_proto_eachisalist,
                                  &st);
    assert(!e);
  } else if (fwd == FWD_DEFINE_FUNCTIONS) {
    struct cprinter_state st = { .out = out, .header = header, .fwd = fwd,
      .mod = NULL, .printed = 0, .user = NULL };
    error e = typ_isalist_foreach((struct module *)mod, node->typ, filter,
                                  print_dyntable_eachisalist,
                                  &st);
    assert(!e);
  }
}

static void print_reflect_type(FILE *out, bool header, enum forward fwd,
                               const struct module *mod, const struct node *node) {
  if (typ_is_reference(node->typ)) {
    return;
  }
  if (typ_generic_functor_const(node->typ) == TBI_SLICE_IMPL) {
    return;
  }
  if (typ_isa(node->typ, TBI_ANY_TUPLE)) {
    return;
  }

  if (header && fwd == FWD_DECLARE_TYPES) {
    fprintf(out, "extern const struct __Type ");
    print_typ(out, mod, node->typ);
    fprintf(out, "$Reflect_type;\n");
    return;
  } else if (header || fwd != FWD_DEFINE_FUNCTIONS) {
    return;
  }

  if (node->which == DEFINTF) {
    struct __Type *type = node->as.DEFINTF.reflect_type;
    fprintf(out, "__attribute__((__weak__)) ");
    fprintf(out, "const struct __Type ");
    print_typ(out, mod, node->typ);
    fprintf(out, "$Reflect_type = {\n");
    fprintf(out, ".typename_hash32 = 0x%x,\n", type->typename_hash32);
    fprintf(out, ".Typename = NLANG_STRING_LITERAL(\"%.*s\"),\n",
            (int)type->Typename.bytes.cnt, type->Typename.bytes.dat);
    fprintf(out, "0 };\n");
    return;
  }

  struct __Type *type = node->as.DEFTYPE.reflect_type;
  fprintf(out, "static uint16_t ");
  print_typ(out, mod, node->typ);
  fprintf(out, "$Reflect_type__hashmap[] = {\n");
  for (size_t n = 0; n < type->dynisalist.hashmap.cnt; n += 10) {
    for (size_t i = n; i < n + 10 && i < type->dynisalist.hashmap.cnt; ++i) {
      fprintf(out, "0x%hx,", type->dynisalist.hashmap.dat[i]);
    }
    fprintf(out, "\n");
  }
  fprintf(out, "};\n");

  fprintf(out, "static struct __entry ");
  print_typ(out, mod, node->typ);
  fprintf(out, "$Reflect_type__entries[] = {\n");
  for (size_t n = 0; n < type->dynisalist.entries.cnt; ++n) {
    struct __entry *e = &type->dynisalist.entries.dat[n];
    fprintf(out, "{ .typename_hash32 = 0x%x, .Typename = NLANG_STRING_LITERAL(\"%.*s\"), ",
            e->typename_hash32, (int)e->Typename.bytes.cnt, e->Typename.bytes.dat);

    const struct typ *intf = e->dyntable;
    fprintf(out, "&");
    print_typ(out, mod, node->typ);
    fprintf(out, "$Dyntable__");
    print_typ(out, mod, intf);
    fprintf(out, " },\n");
  }
  fprintf(out, "};\n");

    fprintf(out, "__attribute__((__weak__)) ");
  fprintf(out, "const struct __Type ");
  print_typ(out, mod, node->typ);
  fprintf(out, "$Reflect_type = {\n");
  fprintf(out, ".typename_hash32 = 0x%x,\n", type->typename_hash32);
  fprintf(out, ".Typename = NLANG_STRING_LITERAL(\"%.*s\"),\n",
          (int)type->Typename.bytes.cnt, type->Typename.bytes.dat);
  fprintf(out, ".dynisalist = {\n");

  fprintf(out, ".hashmap = {\n");
  fprintf(out, ".dat = ");
  print_typ(out, mod, node->typ);
  fprintf(out, "$Reflect_type__hashmap,\n");
  fprintf(out, ".cnt = %zu,\n.cap = %zu,\n},",
          type->dynisalist.hashmap.cnt, type->dynisalist.hashmap.cap);

  fprintf(out, ".entries = {\n");
  fprintf(out, ".dat = ");
  print_typ(out, mod, node->typ);
  fprintf(out, "$Reflect_type__entries,\n");
  fprintf(out, ".cnt = %zu,\n.cap = %zu,\n},",
          type->dynisalist.entries.cnt, type->dynisalist.entries.cap);

  fprintf(out, "},\n};\n");
}

static void print_defchoice_payload(FILE *out,
                                    enum forward fwd,
                                    const struct module *mod,
                                    const struct node *deft,
                                    const struct node *ch) {
  if (ch->which == DEFCHOICE) {
    const struct node *ext = node_defchoice_external_payload(ch);
    if (ext != NULL) {
      if (fwd == FWD_DECLARE_TYPES) {
        fprintf(out, "typedef ");
        print_typ(out, mod, ext->typ);
        fprintf(out, " ");
        print_defchoice_path(out, mod, deft, ch);
        fprintf(out, ";\n");
      }
      return;
    }
  }

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

  if (!subs_count_atleast(ch, IDX_CH_FIRST_PAYLOAD+1)) {
    print_typ(out, mod, TBI_U8);
    fprintf(out, " _$Nfiller;\n};\n");
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
    fprintf(out, "$%s %s;\n",
            idents_value(mod->gctx, ID_TAG_TYPE),
            idents_value(mod->gctx, ID_TAG));
  }

  if (!ch->as.DEFCHOICE.is_leaf) {
    fprintf(out, "union ");
    print_defchoice_path(out, mod, deft, ch);
    fprintf(out, "$%s %s;\n",
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
  fprintf(out, "$%s_label__ (", idents_value(mod->gctx, ID_TAG));
  print_expr(out, mod, subs_at_const(ch, IDX_CH_TAG_FIRST), T__NOT_STATEMENT);
  fprintf(out, ")\n");

  fprintf(out, "static const ");
  print_deftype_name(out, mod, deft);
  fprintf(out, "$%s", idents_value(mod->gctx, ID_TAG_TYPE));
  fprintf(out, " ");
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "$%s = ", idents_value(mod->gctx, ID_TAG));
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "$%s_label__;\n", idents_value(mod->gctx, ID_TAG));
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
  fprintf(out, "$%s_label__ (", idents_value(mod->gctx, ID_FIRST_TAG));
  print_expr(out, mod, subs_at_const(ch, IDX_CH_TAG_FIRST), T__NOT_STATEMENT);
  fprintf(out, ")\n");

  fprintf(out, "static const ");
  print_deftype_name(out, mod, deft);
  fprintf(out, "$%s", idents_value(mod->gctx, ID_TAG_TYPE));
  fprintf(out, " ");
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "$%s = ", idents_value(mod->gctx, ID_FIRST_TAG));
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "$%s_label__;\n", idents_value(mod->gctx, ID_FIRST_TAG));

  fprintf(out, "#define ");
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "$%s_label__ (", idents_value(mod->gctx, ID_LAST_TAG));
  print_expr(out, mod, subs_at_const(ch, IDX_CH_TAG_FIRST), T__NOT_STATEMENT);
  fprintf(out, ")\n");

  fprintf(out, "static const ");
  print_deftype_name(out, mod, deft);
  fprintf(out, "$%s", idents_value(mod->gctx, ID_TAG_TYPE));
  fprintf(out, " ");
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "$%s = ", idents_value(mod->gctx, ID_LAST_TAG));
  print_defchoice_path(out, mod, deft, ch);
  fprintf(out, "$%s_label__;\n", idents_value(mod->gctx, ID_LAST_TAG));
}

static void print_enumunion_functions(FILE *out, bool header, enum forward fwd,
                                  const struct module *mod,
                                  const struct node *deft,
                                  const struct node *ch,
                                  struct fintypset *printed) {
  if (header && !(node_is_export(deft) && node_is_inline(deft))) {
    return;
  }

  FOREACH_SUB_CONST(m, ch) {
    switch (m->which) {
    case DEFFUN:
    case DEFMETHOD:
      print_top(out, header, fwd, mod, m, printed, false);
      break;
    case DEFCHOICE:
      print_enumunion_functions(out, header, fwd, mod, deft, m, printed);
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
    fprintf(out, "$%s" , idents_value(mod->gctx, ID_AS_TYPE));
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
                        const struct node *node, struct fintypset *printed) {
  switch (fwd) {
  case FWD_DECLARE_TYPES:
    if (node == deft) {
      fprintf(out, "typedef ");
      print_typ(out, mod, deft->as.DEFTYPE.tag_typ);
      fprintf(out, " ");
      print_deftype_name(out, mod, deft);
      fprintf(out, "$%s" , idents_value(mod->gctx, ID_TAG_TYPE));
      fprintf(out, ";\n");

      print_reflect_type(out, header, fwd, mod, deft);
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

    if (node == deft) {
      print_enumunion_functions(out, header, fwd, mod, deft, node, printed);
      print_reflect_type(out, header, fwd, mod, deft);
      print_dyntable(out, header, fwd, mod, deft);
    }
    break;
  default:
    assert(false);
    break;
  }
}

static void print_enum(FILE *out, bool header, enum forward fwd,
                       const struct module *mod, const struct node *deft,
                       const struct node *node, struct fintypset *printed) {
  if (fwd == FWD_DECLARE_TYPES) {
    if (deft == node) {
      fprintf(out, "typedef ");
      print_typ(out, mod, node->as.DEFTYPE.tag_typ);
      fprintf(out, " ");
      print_deftype_name(out, mod, node);
      fprintf(out, ";\n");

      print_reflect_type(out, header, fwd, mod, node);
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
        fprintf(out, "$%s_label__ (", idents_value(mod->gctx, ID_TAG));
        print_expr(out, mod, subs_at_const(s, IDX_CH_TAG_FIRST), T__NOT_STATEMENT);
        fprintf(out, ")\n");

        fprintf(out, "static const ");
        print_deftype_name(out, mod, node);
        fprintf(out, " ");
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, " = ");
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, "$%s_label__;\n", idents_value(mod->gctx, ID_TAG));
      } else {
        fprintf(out, "#define ");
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, "$%s_label__ (", idents_value(mod->gctx, ID_FIRST_TAG));
        print_expr(out, mod, subs_at_const(s, IDX_CH_TAG_FIRST), T__NOT_STATEMENT);
        fprintf(out, ")\n");

        fprintf(out, "static const ");
        print_deftype_name(out, mod, node);
        fprintf(out, " ");
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, "$%s = ", idents_value(mod->gctx, ID_FIRST_TAG));
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, "$%s_label__;\n", idents_value(mod->gctx, ID_FIRST_TAG));

        fprintf(out, "#define ");
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, "$%s_label__ (", idents_value(mod->gctx, ID_LAST_TAG));
        print_expr(out, mod, subs_at_const(s, IDX_CH_TAG_LAST), T__NOT_STATEMENT);
        fprintf(out, ")\n");

        fprintf(out, "static const ");
        print_deftype_name(out, mod, node);
        fprintf(out, " ");
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, "$%s = ", idents_value(mod->gctx, ID_LAST_TAG));
        print_defchoice_path(out, mod, deft, s);
        fprintf(out, "$%s_label__;\n", idents_value(mod->gctx, ID_LAST_TAG));

        print_enum(out, header, fwd, mod, deft, s, printed);
      }
    }
  }

  if (fwd == FWD_DECLARE_FUNCTIONS || fwd == FWD_DEFINE_FUNCTIONS) {
    if (deft == node) {
      print_enumunion_functions(out, header, fwd, mod, deft, node, printed);
      print_reflect_type(out, header, fwd, mod, node);
      print_dyntable(out, header, fwd, mod, node);
    }
  }
}

static void print_deftype_reference(FILE *out, bool header, enum forward fwd,
                                    const struct module *mod,
                                    const struct node *node) {
  if (fwd != FWD_DECLARE_TYPES) {
    return;
  }

  const struct node *d = DEF(typ_generic_arg_const(node->typ, 0));

  if (header && !node_is_export(d)) {
    return;
  }

  if (d->which == DEFINTF) {
    if (typ_generic_arity(d->typ) > 0) {
      print_defintf(out, header, fwd, mod, node);
    }
    return;
  }

  guard_generic(out, header, fwd, mod, node, true);

  const char *prefix = "";
  if (typ_is_dyn(d->typ)) {
    const struct node *dd = DEF(typ_generic_arg_const(d->typ, 0));
    fprintf(out, "struct _$Ndyn_");
    print_deftype_name(out, mod, dd);
    fprintf(out, ";\n");
    fprintf(out, "typedef struct _$Ndyn_");
    print_deftype_name(out, mod, dd);
    fprintf(out, " _$Ndyn_");
    print_deftype_name(out, mod, dd);
    fprintf(out, ";\n");
  } else if (d->which == DEFTYPE && d->as.DEFTYPE.kind == DEFTYPE_ENUM) {
    if (is_in_topmost_module(d->typ)) {
      print_enum(out, false, FWD_DECLARE_TYPES, mod, d, d, NULL);
    }
  } else if (!(typ_is_builtin(mod, d->typ) && node_is_extern(d))) {
    prefix = "struct ";
    fprintf(out, "struct ");
    print_deftype_name(out, mod, d);
    fprintf(out, ";\n");
    fprintf(out, "typedef struct ");
    print_deftype_name(out, mod, d);
    fprintf(out, " ");
    print_deftype_name(out, mod, d);
    fprintf(out, ";\n");
  }

  fprintf(out, "typedef %s", prefix);
  print_deftype_name(out, mod, d);
  fprintf(out, "* _$Nref_");
  print_deftype_name(out, mod, d);
  fprintf(out, ";\n");

  guard_generic(out, header, fwd, mod, node, false);
}

static void print_deftype(FILE *out, bool header, enum forward fwd,
                          const struct module *mod, const struct node *node,
                          struct fintypset *printed) {
  if (header && !node_is_export(node)) {
    return;
  }
  const bool is_gen = typ_generic_arity(node->typ) > 0;
  if (!is_gen && !header) {
    if (node_is_export(node) && fwd == FWD_DECLARE_TYPES) {
      return;
    } else if (node_is_export(node) && node_is_inline(node) && fwd == FWD_DEFINE_TYPES) {
      return;
    }
  }

  if (typ_is_pseudo_builtin(node->typ)) {
    return;
  }

  if (typ_is_reference(node->typ)) {
    print_deftype_reference(out, header, fwd, mod, node);
    return;
  }
  if (fwd == FWD_DEFINE_DYNS) {
    return;
  }

  guard_generic(out, header, fwd, mod, node, true);

  if (fwd == FWD_DEFINE_TYPES && node_is_extern(node)) {
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
    if (typ_is_builtin(mod, node->typ) && node_is_extern(node) && node_is_inline(node)) {
      // noop
    } else {
      fprintf(out, "struct ");
      print_deftype_name(out, mod, node);
      fprintf(out, ";\n");
      fprintf(out, "typedef struct ");
      print_deftype_name(out, mod, node);
      fprintf(out, " ");
      print_deftype_name(out, mod, node);
      fprintf(out, ";\n");

      print_deftype_block(out, header, fwd, mod, node, true, printed);
    }

  } else if (fwd == FWD_DEFINE_TYPES) {
    if (typ_is_builtin(mod, node->typ) && node_is_extern(node) && node_is_inline(node)) {
      // noop
    } else {
      print_deftype_block(out, header, fwd, mod, node, true, printed);

      if (!prototype_only(header, node)) {
        fprintf(out, "struct ");
        print_deftype_name(out, mod, node);
        print_deftype_block(out, header, fwd, mod, node, false, printed);
        fprintf(out, ";\n");
      }
    }
  } else if (fwd == FWD_DECLARE_FUNCTIONS || fwd == FWD_DEFINE_FUNCTIONS) {
    print_deftype_block(out, header, fwd, mod, node, true, printed);
  }

  print_reflect_type(out, header, fwd, mod, node);
  print_dyntable(out, header, fwd, mod, node);

done:
  guard_generic(out, header, fwd, mod, node, false);
}

static ERROR print_defintf_dyntable_field_eachisalist(struct module *mod, struct typ *t,
                                                      struct typ *intf,
                                                      bool *stop, void *user) {
  struct cprinter_state *st = user;

  const struct node *dintf = DEF(intf);
  FOREACH_SUB_CONST(d, dintf) {
    if (d->which != DEFFUN && d->which != DEFMETHOD) {
      continue;
    }
    if (node_toplevel_const(d)->flags & TOP_IS_NOT_DYN) {
      continue;
    }
    if (subs_count_atleast(subs_at_const(DEF(d->typ), IDX_GENARGS), 1)) {
      continue;
    }
    print_fun_prototype(st->out, st->header, st->fwd, mod, d, true, true, true, NULL);
    fprintf(st->out, ";\n");
    st->printed += 1;
  }

  return 0;
}

static void print_defintf(FILE *out, bool header, enum forward fwd,
                          const struct module *mod, const struct node *node) {
  if (typ_is_generic_functor(node->typ)) {
    return;
  }
  const struct node *isalist = subs_at_const(node, IDX_ISALIST);
  if (subs_count_atleast(isalist, 1)
      && typ_equal(subs_first_const(isalist)->typ, TBI_PREVENT_DYN)) {
    return;
  }

  if (header && !node_is_export(node)) {
    return;
  }
  if (header && fwd == FWD_DEFINE_FUNCTIONS) {
    return;
  }

  const bool is_gen = typ_generic_arity(node->typ) > 0;
  if (!is_gen && !header) {
    if (node_is_export(node) && fwd == FWD_DECLARE_TYPES) {
      return;
    } else if (node_is_export(node) && node_is_inline(node) && fwd == FWD_DEFINE_DYNS) {
      return;
    }
  }

  if (typ_is_pseudo_builtin(node->typ)) {
    return;
  }

  if (typ_is_reference(node->typ)) {
    return;
  }

  guard_generic(out, header, fwd, mod, node, true);

  if (fwd == FWD_DECLARE_TYPES) {
    fprintf(out, "struct _$Ndyntable_");
    print_deftype_name(out, mod, node);
    fprintf(out, ";\n");

    fprintf(out, "struct _$Ndyn_");
    print_deftype_name(out, mod, node);
    fprintf(out, ";\n");
    fprintf(out, "typedef struct _$Ndyn_");
    print_deftype_name(out, mod, node);
    fprintf(out, " _$Ndyn_");
    print_deftype_name(out, mod, node);
    fprintf(out, ";\n");

  } else if (fwd == FWD_DEFINE_DYNS) {
    if (!prototype_only(header, node)) {
      fprintf(out, "struct _$Ndyntable_");
      print_deftype_name(out, mod, node);
      fprintf(out, " {\n");

      // Must be first: see lib/n/reflect.n.h
      fprintf(out, "const void *type;\n");

      struct cprinter_state st = {
        .out = out, .header = header, .fwd = fwd,
        .mod = NULL, .printed = 0, .user = NULL,
      };
      error e = typ_isalist_foreach((struct module *)mod, node->typ,
                                    ISALIST_FILTEROUT_PREVENT_DYN,
                                    // filter is 0 because all members of an
                                    // intf inherit the exported status from
                                    // the intf itself.
                                    print_defintf_dyntable_field_eachisalist,
                                    &st);
      assert(!e);
      e = print_defintf_dyntable_field_eachisalist((struct module *)mod, node->typ,
                                                   node->typ, NULL, &st);
      assert(!e);

      if (st.printed == 0) {
        // Needed if all the members of the intf are *themselves* generics,
        // that form is indeed legal.
        fprintf(out, "n$builtins$U8 _$Nfiller;\n");
      }
      fprintf(out, "};\n");

      fprintf(out, "struct _$Ndyn_");
      print_deftype_name(out, mod, node);
      fprintf(out, " {\n");
      fprintf(out, "const struct _$Ndyntable_");
      print_deftype_name(out, mod, node);
      fprintf(out, " *dyntable;\n");
      fprintf(out, "void *obj;\n");
      fprintf(out, "};\n");
    }
  }

  print_reflect_type(out, header, fwd, mod, node);

  guard_generic(out, header, fwd, mod, node, false);
}

static void print_include(FILE *out, const char *filename, const char *postfix) {
  fprintf(out, "# include \"%s%s\"\n", filename, postfix);
}

static void print_import(FILE *out, bool header, enum forward fwd,
                         const struct module *mod, const struct node *node,
                         bool non_inline_deps) {
  struct node *target = NULL;
  error e = scope_lookup(&target, mod, &mod->gctx->modules_root.scope,
                         subs_first_const(node), false);
  assert(!e);
  if (target->which != MODULE) {
    return;
  }

  if (fwd == FWD_DEFINE_TYPES) {
    if (!non_inline_deps
        && !(node_toplevel_const(node)->flags & TOP_IS_INLINE)) {
      return;
    } else if (non_inline_deps
               && (node_toplevel_const(node)->flags & TOP_IS_INLINE)) {
      return;
    }
  }

  print_include(out, target->as.MODULE.mod->filename, ".o.h");
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

static uint32_t track_id(bool header, enum forward fwd,
                         bool struct_body_written) {
  return (!!struct_body_written << 9) | (!!header << 8) | (uint8_t)fwd;
}

static bool is_printed(struct fintypset *printed,
                       bool header, enum forward fwd,
                       const struct typ *t,
                       uint32_t topdep_mask) {
  if (typ_equal(t, TBI__NOT_TYPEABLE)) {
    return false;
  }

  uint32_t *at = fintypset_get(printed, t);

  if (at != NULL) {
    const uint32_t at_fwd = (*at) & 0xff;
    const uint32_t at_header = ((*at) >> 8) & 0x1;
    const uint32_t at_struct_body_written = ((*at) >> 9) & 0x1;

    if (fwd == FWD_DEFINE_TYPES
        && (topdep_mask & TOP__TOPDEP_INLINE_STRUCT)
        && !at_struct_body_written) {
      return false;
    }

    if ((!at_header && header) || at_fwd >= fwd) {
      return true;
    }
  }

  return false;
}

static void track_printed(const struct module *mod,
                          struct fintypset *printed,
                          bool header, enum forward fwd,
                          const struct typ *t,
                          bool struct_body_written) {
  if (typ_equal(t, TBI__NOT_TYPEABLE)) {
    return;
  }

  const uint32_t update = track_id(header, fwd, struct_body_written);
  uint32_t *at = fintypset_get(printed, t);
  if (at != NULL) {
    *at = update;
  } else {
    fintypset_set(printed, t, update);
  }
}

static ERROR print_topdeps_each(struct module *mod, struct node *node,
                                struct typ *_t, uint32_t topdep_mask, void *user) {
  struct cprinter_state *st = user;
  struct fintypset *printed = st->user;
  if (topdep_mask & (TOP_IS_FUNCTOR | TOP_IS_PREVENT_DYN)) {
    return 0;
  }

  if ((!typ_is_concrete(node->typ) && node->which != DEFINTF)
      || (!node_is_at_top(node) && !typ_is_concrete(parent_const(node)->typ))
      || typ_was_zeroed(_t)
      || (typ_is_reference(_t) && DEF(_t)->which == DEFINTF)
      || typ_is_generic_functor(_t)
      || (typ_generic_arity(_t) == 0 && !is_in_topmost_module(_t))
      || typ_is_tentative(_t)) {
    return 0;
  }

  const struct typ *t = intercept_slices(st->mod, _t);

  if (st->header
      && ((typ_is_function(t) && !(topdep_mask & TOP_IS_INLINE))
          || (!(topdep_mask & TOP_IS_INLINE) && !(topdep_mask & TOP_IS_EXPORT)))) {
    return 0;
  }

  if (is_printed(printed, st->header, st->fwd, t, topdep_mask)) {
    return 0;
  }

  const struct node *d = DEF(t);
  const struct module *dmod = node_module_owner_const(d);
  print_top(st->out, st->header, st->fwd, dmod, d, printed,
            st->fwd == FWD_DEFINE_TYPES
            && (topdep_mask & TOP__TOPDEP_INLINE_STRUCT));

  return 0;
}

static void print_topdeps(FILE *out, bool header, enum forward fwd,
                          const struct module *mod, const struct node *node,
                          struct fintypset *printed) {
  const struct toplevel *toplevel = node_toplevel_const(node);
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

  error e = topdeps_foreach(CONST_CAST(mod), CONST_CAST(node),
                            print_topdeps_each, &st);
  assert(!e);
}

static void print_top(FILE *out, bool header, enum forward fwd,
                      const struct module *mod, const struct node *node,
                      struct fintypset *printed, bool force) {
  if ((!typ_is_concrete(node->typ) && node->which != DEFINTF)
      || (!node_is_at_top(node) && !typ_is_concrete(parent_const(node)->typ))) {
    return;
  }

  if (node->which == NOOP) {
    return;
  }

  if (NM(node->which) & (NM(EXAMPLE) | NM(LET))) {
    // not tracked
  } else {
    if (!force && is_printed(printed, header, fwd, node->typ, 0)) {
      return;
    }
    track_printed(mod, printed, header, fwd, node->typ, false);
  }

  if (!node_is_at_top(node)
      && node_ident(parent_const(node)) == node_ident(DEF(TBI_SLICE))) {
    return;
  }

#if 0
  fprintf(out, "/*\n");
    stderr=out;
    debug_print_topdeps(mod, node);
  fprintf(out, "\n*/\n");
#endif

  static __thread size_t prevent_infinite;
  assert(++prevent_infinite < 1000 && "FIXME When force==true, see t00/fixme10");

  print_topdeps(out, header, fwd, mod, node, printed);

  switch (node->which) {
  case DEFMETHOD:
  case DEFFUN:
    print_deffun(out, header, fwd, mod, node, printed);
    break;
  case DEFTYPE:
    if (fwd != FWD_DEFINE_TYPES ||
        !is_printed(printed, header, fwd, node->typ, TOP__TOPDEP_INLINE_STRUCT)) {
      print_deftype(out, header, fwd, mod, node, printed);
      if (fwd == FWD_DEFINE_TYPES) {
        track_printed(mod, printed, header, fwd, node->typ, true);
      }
    }
    break;
  case DEFINTF:
    print_defintf(out, header, fwd, mod, node);
    break;
  case LET:
    if (fwd != FWD_DEFINE_TYPES
        || !is_printed(printed, header, fwd, node->typ, TOP__TOPDEP_INLINE_STRUCT)) {
      print_defname(out, header, fwd, mod, subs_first_const(node));
      if (fwd == FWD_DEFINE_TYPES) {
        track_printed(mod, printed, header, fwd, node->typ, true);
      }
    }
    break;
  case EXAMPLE:
    print_example(out, header, fwd, mod, node);
    break;
  case IMPORT:
    print_import(out, header, fwd, mod, node, force);
    break;
  case WITHIN:
  case DEFINCOMPLETE:
    break;
  default:
    fprintf(g_env.stderr, "Unsupported node: %d\n", node->which);
    assert(false);
  }

  --prevent_infinite;
}

static void print_module(FILE *out, bool header, const struct module *mod) {
  mod->stage->printing_mod = mod;

  fprintf(out, "#include <lib/n/runtime.h>\n");

  struct fintypset printed;
  fintypset_fullinit(&printed);

  const struct node *top = mod->body;

  const struct node *first_non_import = subs_first_const(top);
  FOREACH_SUB_CONST(node, top) {
    if (node->which != IMPORT) {
      first_non_import = node;
      break;
    }
  }

  const enum forward fwd_passes[] = {
    FWD_DECLARE_TYPES, FWD_DEFINE_DYNS, FWD_DEFINE_TYPES,
    FWD_DECLARE_FUNCTIONS, FWD_DEFINE_FUNCTIONS };
  for (int i = 0; i < ARRAY_SIZE(fwd_passes); ++i) {
    enum forward fwd = fwd_passes[i];

    if (header) {
      fprintf(out, "#ifdef %s\n", forward_guards[fwd]);
      fprintf(out, "#ifndef %s__", forward_guards[fwd]);
      print_scope_name(out, mod, &mod->root->scope);
      fprintf(out, "\n#define %s__", forward_guards[fwd]);
      print_scope_name(out, mod, &mod->root->scope);
      fprintf(out, "\n\n");
    } else {
      fprintf(out, "#define %s\n", forward_guards[fwd]);
    }

    for (const struct node *node = subs_first_const(top);
         node != first_non_import; node = next_const(node)) {
      print_top(out, header, fwd, mod, node, &printed, false);

      if (next_const(node) != NULL) {
        fprintf(out, "\n");
      }
    }

    if (header) {
      if (file_exists(mod->filename, ".h")) {
        print_include(out, mod->filename, ".h");
      }
    } else {
      print_include(out, mod->filename, ".o.h");
      if (file_exists(mod->filename, ".c")) {
        print_include(out, mod->filename, ".c");
      }
    }

    for (const struct node *node = first_non_import; node != NULL;
         node = next_const(node)) {
      print_top(out, header, fwd, mod, node, &printed, false);

      if (next_const(node) != NULL) {
        fprintf(out, "\n");
      }
    }

    if (fwd == FWD_DEFINE_TYPES) {
      // Again, this time to print imports to non-inline imports.
      for (const struct node *node = subs_first_const(top);
           node != first_non_import; node = next_const(node)) {
        print_top(out, header, fwd, mod, node, &printed, true);

        if (next_const(node) != NULL) {
          fprintf(out, "\n");
        }
      }
    }

    if (header) {
      fprintf(out, "\n#endif\n");
      fprintf(out, "#endif // %s\n", forward_guards[fwd]);

      if (fwd == FWD_DEFINE_TYPES) {
        // When fwd==FWD_DEFINE_TYPES, to support more dependency orders,
        // print_import() will skip modules to which the importer has no
        // inline dependency. The module must be imported under
        // FWD_DEFINE_TYPES at some point, though (at least for the benefit
        // of the function definitions in itself). So we import it here if
        // it hadn't been done before.
        fprintf(out, "\n#ifdef %s\n", forward_guards[FWD_DEFINE_FUNCTIONS]);
        fprintf(out, "# undef %s\n", forward_guards[FWD_DEFINE_FUNCTIONS]);
        fprintf(out, "# ifndef %s__", forward_guards[fwd]);
        print_scope_name(out, mod, &mod->root->scope);
        fprintf(out, "\n#  define %s\n", forward_guards[fwd]);
        print_include(out, mod->filename, ".o.h");
        fprintf(out, "\n#  undef %s\n# endif\n", forward_guards[fwd]);
        fprintf(out, "# define %s\n#endif\n\n", forward_guards[FWD_DEFINE_FUNCTIONS]);
      }
    } else {
      fprintf(out, "#undef %s\n", forward_guards[fwd]);
    }
  }

  fintypset_destroy(&printed);
}

static void print_runexamples(FILE *out, const struct module *mod) {
  fprintf(out, "void ");
  print_c_runexamples_name(out, mod);
  fprintf(out, "(void) " ATTR_SECTION_EXAMPLES ";\n");

  fprintf(out, "void ");
  print_c_runexamples_name(out, mod);
  fprintf(out, "(void) {\n");
  for (size_t n = 0; n < mod->next_example; ++n) {
    print_scope_name(out, mod, &mod->root->scope);
    fprintf(out, "_$Nexample%zu();\n", n);
  }
  fprintf(out, "}\n");
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
  fprintf(out, "_$Nrunexamples");
}
