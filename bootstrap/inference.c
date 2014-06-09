#include "inference.h"

#include "passes.h"
#include "ssa.h"
#include "types.h"
#include "unify.h"
#include "scope.h"
#include "parser.h"
#include "instantiate.h"
#include "topdeps.h"

static struct typ *create_tentative(struct module *mod, const struct node *for_error,
                                    struct typ *functor) {
  return instantiate_fully_implicit(mod, for_error, functor)->typ;
}

STEP_NM(step_rewrite_wildcards,
        NM(DEFMETHOD) | NM(UN) | NM(BIN));
error step_rewrite_wildcards(struct module *mod, struct node *node,
                             void *user, bool *stop) {
  DSTEP(mod, node);

#define SET_UNLESS_ZERO(dst, src) if (src != 0) { dst = src; }

  bool is_self = false;

#define GET_WILDCARD(kind) \
  ( (is_self) ? mod->state->fun_state->self_wildcard.kind \
    : mod->state->fun_state->wildcard.kind )

  // FIXME: handle DEFFUN like in t02/fixme01.n
  switch (node->which) {
  case DEFMETHOD:
    if (!subs_count_atleast(subs_at_const(node, IDX_GENARGS), 1)) {
      break;
    }
    struct node *def = NULL;
    error e = scope_lookup_ident_immediate(&def, node, mod, &node->scope,
                                           ID_WILDCARD_REF_ARG, true);
    if (e) {
      break;
    }

    const bool is_shallow = node_toplevel_const(node)->flags & TOP_IS_SHALLOW;
    if (typ_equal(def->typ, TBI_ANY_REF)
        || typ_equal(def->typ, TBI_REF)) {
      mod->state->fun_state->wildcard.ref = TREFDOT;
      mod->state->fun_state->wildcard.nulref = TNULREFDOT;
      mod->state->fun_state->wildcard.deref = TDEREFDOT;
      mod->state->fun_state->wildcard.acc = TDOT;

      mod->state->fun_state->self_wildcard = mod->state->fun_state->wildcard;
    } else if (typ_equal(def->typ, TBI_MREF)) {
      mod->state->fun_state->wildcard.ref = is_shallow ? TREFSHARP : TREFBANG;
      mod->state->fun_state->wildcard.nulref = is_shallow ? TNULREFSHARP : TNULREFBANG;
      mod->state->fun_state->wildcard.deref = is_shallow ? TDEREFSHARP : TDEREFBANG;
      mod->state->fun_state->wildcard.acc = is_shallow ? TSHARP : TBANG;

      mod->state->fun_state->self_wildcard.ref = TREFBANG;
      mod->state->fun_state->self_wildcard.nulref = TNULREFBANG;
      mod->state->fun_state->self_wildcard.deref = TDEREFBANG;
      mod->state->fun_state->self_wildcard.acc = TBANG;
    } else if (typ_equal(def->typ, TBI_MMREF)) {
      mod->state->fun_state->wildcard.ref = TREFSHARP;
      mod->state->fun_state->wildcard.nulref = TNULREFSHARP;
      mod->state->fun_state->wildcard.deref = TDEREFSHARP;
      mod->state->fun_state->wildcard.acc = TSHARP;

      mod->state->fun_state->self_wildcard = mod->state->fun_state->wildcard;
    } else {
      assert(false);
    }
    break;

  case UN:
    is_self = node_ident(subs_first_const(node)) == ID_SELF;

    switch (node->as.UN.operator) {
    case TREFWILDCARD:
      SET_UNLESS_ZERO(node->as.UN.operator, GET_WILDCARD(ref));
      break;
    case TNULREFWILDCARD:
      SET_UNLESS_ZERO(node->as.UN.operator, GET_WILDCARD(nulref));
      break;
    case TDEREFWILDCARD:
      SET_UNLESS_ZERO(node->as.UN.operator, GET_WILDCARD(deref));
      break;
    default:
      break;
    }
    break;
  case BIN:
    is_self = node_ident(subs_first_const(node)) == ID_SELF;

    switch (node->as.BIN.operator) {
    case TWILDCARD:
      SET_UNLESS_ZERO(node->as.BIN.operator, GET_WILDCARD(acc));
      break;
    default:
      break;
    }
    break;
  default:
    assert(false && "Unreached");
    break;
  }

#undef GET_WILDCARD
#undef SET_UNLESS_ZERO

  return 0;
}

static void mark_subs(struct module *mod, struct node *node, struct typ *mark,
                      struct node *first, struct node *last, size_t incr) {
  if (first == NULL) {
    return;
  }

  struct node *n = first;
  while (true) {
    n->typ = mark;

    for (size_t i = 0; i < incr; ++i) {
      if (n == last) {
        return;
      }

      n = next(n);
    }
  }
}

static void inherit(struct module *mod, struct node *node) {
  if (node->typ == TBI__NOT_TYPEABLE) {
    mark_subs(mod, node, node->typ,
              subs_first(node), subs_last(node), 1);
  }
}

STEP_NM(step_type_destruct_mark,
        NM(BIN) | NM(CALL) | NM(INIT) | NM(DEFALIAS) | NM(DEFNAME) |
        NM(DEFFUN) | NM(DEFMETHOD) | NM(DEFTYPE) | NM(DEFINTF) | NM(DEFINCOMPLETE) |
        NM(DEFFIELD) | NM(DEFARG) | NM(DEFGENARG) | NM(SETGENARG) |
        NM(MODULE_BODY) | NM(DEFCHOICE) | NM(WITHIN) | NM(THROW));
error step_type_destruct_mark(struct module *mod, struct node *node,
                              void *user, bool *stop) {
  DSTEP(mod, node);
  if (node->which == MODULE) {
    return 0;
  }

  inherit(mod, node);

  struct typ *not_typeable = TBI__NOT_TYPEABLE;
  struct node *first = subs_first(node);
  struct node *last = subs_last(node);

  switch (node->which) {
  case BIN:
    if (OP_KIND(node->as.BIN.operator) == OP_BIN_ACC) {
      mark_subs(mod, node, not_typeable, next(first), last, 1);
    }
    break;
  case CALL:
    if (first->typ == NULL) {
      // Otherwise, we are rewriting this expression and we should not touch
      // first.
      first->typ = TBI__CALL_FUNCTION_SLOT;
    }
    break;
  case INIT:
    if (!node->as.INIT.is_array) {
      mark_subs(mod, node, TBI__NOT_TYPEABLE, first, last, 2);
    }
    break;
  case WITHIN:
    if (subs_count_atleast(node, 1)
        && first->which != WITHIN) {
      // So it will not resolve in type_inference_ident(), but through
      // type_inference_within().
      first->typ = not_typeable;
    }
    break;
  case DEFFUN:
  case DEFMETHOD:
    first->typ = not_typeable;
    break;
  case DEFALIAS:
  case DEFNAME:
  case DEFTYPE:
  case DEFINTF:
  case DEFINCOMPLETE:
  case DEFFIELD:
  case DEFARG:
  case DEFGENARG:
  case SETGENARG:
    first->typ = not_typeable;
    break;
  case MODULE_BODY:
    node->typ = not_typeable;
    break;
  case DEFCHOICE:
    first->typ = not_typeable;
    break;
  case THROW:
    if (subs_count_atleast(node, 2)) {
      first->typ = not_typeable;
    }
    break;
  default:
    assert(false && "Unreached");
    break;
  }

  return 0;
}

STEP_NM(step_type_mutability_mark,
        NM(BIN) | NM(UN));
error step_type_mutability_mark(struct module *mod, struct node *node,
                                void *user, bool *stop) {
  DSTEP(mod, node);
  struct typ *mutable = TBI__MUTABLE;
  struct typ *mercurial = TBI__MERCURIAL;

  struct node *first = subs_first(node);

  switch (node->which) {
  case BIN:
    switch (node->as.BIN.operator) {
    case TASSIGN:
    case TPLUS_ASSIGN:
    case TMINUS_ASSIGN:
    case TTIMES_ASSIGN:
    case TDIVIDE_ASSIGN:
    case TMODULO_ASSIGN:
    case TBWAND_ASSIGN:
    case TBWOR_ASSIGN:
    case TBWXOR_ASSIGN:
    case TRSHIFT_ASSIGN:
    case TLSHIFT_ASSIGN:
      first->typ = mutable;
      break;
    default:
      break;
    }
    break;
  case UN:
    if (OP_KIND(node->as.UN.operator) != OP_UN_REFOF) {
      break;
    }

    // Below, we're interested in catching cases like: @#(self!p)

    switch (node->as.UN.operator) {
    case TREFDOT:
      // no-op
      break;
    case TREFBANG:
      if (first->typ != NULL) {
        if (first->which == BIN && !(first->flags & NODE_IS_TYPE)) {
          error e = typ_check_deref_against_mark(mod, first, TBI__MUTABLE,
                                                 first->as.BIN.operator);
          EXCEPT(e);
        }
      } else {
        first->typ = mutable;
      }
      break;
    case TREFWILDCARD:
    case TREFSHARP:
      if (first->typ != NULL) {
        if (first->which == BIN && !(first->flags & NODE_IS_TYPE)) {
          error e = typ_check_deref_against_mark(mod, first, TBI__MERCURIAL,
                                                 first->as.BIN.operator);
          EXCEPT(e);
        }
      } else {
        first->typ = mercurial;
      }
      break;
    default:
      break;
    }
    break;
  default:
    assert(false && "Unreached");
    break;
  }

  return 0;
}

STEP_NM(step_type_gather_retval,
        NM(DEFFUN) | NM(DEFMETHOD));
error step_type_gather_retval(struct module *mod, struct node *node,
                              void *user, bool *stop) {
  DSTEP(mod, node);

  module_retval_set(mod, node_fun_retval(node));

  return 0;
}

// FIXME: This is O(depth * number_throw_except).
// Would be O(number_throw_except) if we remembered whether we're in the TRY
// or in one of the CATCH, when descending.
static error check_in_try(struct module *mod, struct node *node, const char *which) {
  error e;

  struct try_state *st = module_excepts_get(mod);
  if (st == NULL) {
    goto fail;
  }

  const struct node *p = node;
  do {
    p = parent_const(p);
    if (p->which == CATCH) {
      goto fail;
    }
  } while (p->which != TRY);

  goto ok;

fail:
  e = mk_except(mod, node, "%s not in try block", which);
  THROW(e);

ok:
  return 0;
}

STEP_NM(step_type_gather_excepts,
        NM(TRY) | NM(THROW));
error step_type_gather_excepts(struct module *mod, struct node *node,
                               void *user, bool *stop) {
  DSTEP(mod, node);
  error e;
  switch (node->which) {
  case TRY:
    module_excepts_open_try(mod, node);
    break;
  case THROW:
    e = check_in_try(mod, node, "throw");
    EXCEPT(e);
    module_excepts_push(mod, node);
    break;
  default:
    assert(false && "Unreached");
    break;
  }
  return 0;
}

error reference(struct node **result,
                struct module *mod, struct node *for_error,
                enum token_type op, struct typ *typ) {
  struct typ *f = mod->gctx->builtin_typs_for_refop[op];
  switch (op) {
  case TREFWILDCARD:
  case TNULREFWILDCARD:
    // FIXME: when op is a wildcard (not substituted -- i.e. in the
    // uninstantiated generic), we will not catch all possible
    // incompatibilities. They will get caught when the function is
    // instantiated on an actual reference, but we're supposed to catch
    // everything prior to that.
    f = typ_create_tentative_functor(f);
    break;
  default:
    break;
  }

  error e = instantiate(result, mod, for_error, 0, f, &typ, 1);
  EXCEPT(e);

  topdeps_record(mod, (*result)->typ);

  return 0;
}

static error check_terms_not_types(struct module *mod, struct node *node) {
  error e;
  int nth = 1;
  FOREACH_SUB_CONST(s, node) {
    if (s->flags & NODE_IS_TYPE) {
      e = mk_except_type(mod, s, "term %d cannot be a type", nth);
      THROW(e);
    }
    nth += 1;
  }
  return 0;
}

static struct node *follow_ssa(struct node *node) {
  struct node *expr = node;
  if (expr->which == IDENT) {
    struct node *def = expr->as.IDENT.def;
    if (def->which == DEFNAME
        && def->as.DEFNAME.ssa_user == expr) {
      expr = subs_last(def);
    }
  }
  return expr;
}

static error try_insert_automagic_deref(struct module *mod,
                                        struct node *node) {
  if (!typ_is_reference(node->typ)) {
    return 0;
  }

  struct node *expr = follow_ssa(node);
  if (expr->which == UN
      && expr->as.UN.operator == TREFDOT
      && expr->as.UN.is_explicit) {
    error e = mk_except_type(mod, expr, "explicit '@' operators are not"
                             " allowed for unqualified const references");
    THROW(e);
  }

  struct node *last_block = NULL;
  struct node *target = node;
  while (target->which == BLOCK) {
    last_block = target;
    unset_typ(&last_block->typ);

    target = subs_last(target);
  }

  struct node *par = parent(target);
  struct node *deref = mk_node(mod, par, UN);
  deref->codeloc = target->codeloc;
  deref->as.UN.operator = TDEREFDOT;

  node_subs_remove(par, deref);
  node_subs_replace(par, target, deref);
  node_subs_append(deref, target);

  const struct node *except[] = { target, NULL };
  error e = catchup(mod, except, deref, CATCHUP_BELOW_CURRENT);
  assert(!e);

  if (last_block != NULL) {
    struct node *n = last_block;
    while (true) {
      assert(n->which == BLOCK);
      set_typ(&n->typ, subs_last(n)->typ);

      if (n == node) {
        break;
      }
      n = parent(n);
    }
  }

  return 0;
}

static error type_inference_un(struct module *mod, struct node *node) {
  assert(node->which == UN);
  error e;
  const enum token_type operator = node->as.UN.operator;
  struct node *term = subs_first(node);

  struct node *i = NULL;
  switch (OP_KIND(operator)) {
  case OP_UN_SLICE:
    if (!(term->flags & NODE_IS_TYPE)) {
      e = mk_except_type(mod, node, "slice specifier must be applied to a type");
      THROW(e);
    }
    // fallthrough
  case OP_UN_REFOF:
    // FIXME: it's not OK to take a mutable reference of:
    //   fun foo p:@t = void
    //     let mut = @!(p.)
    e = reference(&i, mod, node, operator, term->typ);
    EXCEPT(e);
    set_typ(&node->typ, i->typ);
    node->flags |= term->flags & NODE__TRANSITIVE;
    return 0;
  case OP_UN_DEREF:
    e = typ_check_can_deref(mod, term, term->typ, operator);
    EXCEPT(e);
    e = typ_check_deref_against_mark(mod, node, node->typ, operator);
    EXCEPT(e);
    set_typ(&node->typ, typ_generic_arg(term->typ, 0));
    node->flags |= term->flags & NODE__TRANSITIVE;
    return 0;

  case OP_UN_BOOL:
  case OP_UN_ARITH:
  case OP_UN_BW:
    break;
  default:
    assert(false);
  }

  e = check_terms_not_types(mod, node);
  EXCEPT(e);

  e = try_insert_automagic_deref(mod, term);
  EXCEPT(e);
  term = subs_first(node);

  switch (OP_KIND(operator)) {
  case OP_UN_BOOL:
    set_typ(&node->typ, TBI_BOOL);
    e = unify(mod, node, node->typ, term->typ);
    EXCEPT(e);
    break;
  case OP_UN_ARITH:
    set_typ(&node->typ, create_tentative(mod, node, TBI_ARITHMETIC));
    e = unify(mod, node, node->typ, term->typ);
    EXCEPT(e);
    break;
  case OP_UN_BW:
    set_typ(&node->typ, create_tentative(mod, node, TBI_BITWISE));
    e = unify(mod, node, node->typ, term->typ);
    EXCEPT(e);
    break;
  default:
    assert(false);
  }

  return 0;
}

static error type_inference_bin_sym(struct module *mod, struct node *node) {
  assert(node->which == BIN);

  const enum token_type operator = node->as.BIN.operator;

  struct node *left = subs_first(node);
  struct node *right = subs_last(node);
  error e;

  if (!OP_IS_ASSIGN(operator) && OP_KIND(operator) != OP_BIN_SYM_PTR) {
    e = try_insert_automagic_deref(mod, left);
    EXCEPT(e);
    e = try_insert_automagic_deref(mod, right);
    EXCEPT(e);

    left = subs_first(node);
    right = subs_last(node);
  }

  if (operator == TASSIGN) {
    if (typ_equal(right->typ, TBI_VOID)) {
      if (left->which == IDENT
          && left->as.IDENT.def->which == DEFNAME
          && left->as.IDENT.def->as.DEFNAME.ssa_user != NULL) {
        // noop
      } else {
        e = mk_except(mod, node, "cannot assign an expression of type 'void'");
        THROW(e);
      }
    }

    e = unify_refcompat(mod, node, left->typ, right->typ);
    EXCEPT(e);

    left->flags |= right->flags & NODE__ASSIGN_TRANSITIVE;
  } else {
    e = unify(mod, node, left->typ, right->typ);
    EXCEPT(e);
  }

  switch (OP_KIND(operator)) {
  case OP_BIN_SYM_BOOL:
    set_typ(&node->typ, left->typ);
    break;
  case OP_BIN_SYM_ARITH:
    switch (operator) {
    case TPLUS_ASSIGN:
    case TMINUS_ASSIGN:
    case TTIMES_ASSIGN:
    case TDIVIDE_ASSIGN:
    case TMODULO_ASSIGN:
      e = typ_check_isa(mod, node, left->typ, TBI_ARITHMETIC);
      EXCEPT(e);

      set_typ(&node->typ, TBI_VOID);
      left->flags |= right->flags & NODE__TRANSITIVE;
      break;
    default:
      set_typ(&node->typ, create_tentative(mod, node, TBI_ARITHMETIC));
      e = unify(mod, node, node->typ, left->typ);
      EXCEPT(e);
      break;
    }
    break;
  case OP_BIN_SYM_BW:
    switch (operator) {
    case TBWAND_ASSIGN:
    case TBWOR_ASSIGN:
    case TBWXOR_ASSIGN:
    case TRSHIFT_ASSIGN:
    case TLSHIFT_ASSIGN:
      e = typ_check_isa(mod, node, left->typ, TBI_BITWISE);
      EXCEPT(e);

      set_typ(&node->typ, TBI_VOID);
      left->flags |= right->flags & NODE__TRANSITIVE;
      break;
    default:
      set_typ(&node->typ, create_tentative(mod, node, TBI_BITWISE));
      e = unify(mod, node, node->typ, left->typ);
      EXCEPT(e);
      break;
    }
    break;
  case OP_BIN_SYM_PTR:
    e = typ_check_isa(mod, node, left->typ, TBI_ANY_ANY_REF);
    EXCEPT(e);
    e = typ_check_isa(mod, node, right->typ, TBI_ANY_ANY_REF);
    EXCEPT(e);
    set_typ(&node->typ, TBI_BOOL);
    break;
  case OP_BIN_SYM:
    switch (operator) {
    case TLE:
    case TLT:
    case TGT:
    case TGE:
    case TEQ:
    case TNE:
      if (typ_equal(left->typ, TBI_BOOL)) {
        // We want to propagate the link status of the terms when used as a
        // weakly concrete.
        set_typ(&node->typ, left->typ);
      } else {
        set_typ(&node->typ, TBI_BOOL);
      }
      break;
    default:
      set_typ(&node->typ, TBI_VOID);
      break;
    }
    break;
  default:
    set_typ(&node->typ, TBI_VOID);
    break;
  }

  return 0;
}

static size_t codeloc_pos_after(struct module *mod, struct node *node) {
  struct node *n = node;
  while (next(n) == NULL || next(n)->codeloc.pos == node->codeloc.pos) {
    if (parent(n) == NULL) {
      break;
    }
    n = parent(n);
  }

  n = next(n);
  if (n == NULL) {
    return mod->parser.len;
  } else {
    return n->codeloc.pos;
  }
}

static char *quote_code(const char *data, size_t start, size_t end) {
  int len = 0;
  for (size_t n = start; n < end; ++n) {
    switch (data[n]) {
    case '\n':
      goto done;
    case '"':
      len += 1;
      break;
    }
    len += 1;
  }

done:
  ;char *r = calloc(2 + len + 1, sizeof(char));
  sprintf(r, "\"%.*s\"", len, data + start);
  return r;
}

EXAMPLE(quote_code) {
  const char *code = "abcd\nefgh";
  assert(strcmp("\"abcd\"", quote_code(code, 0, 9)) == 0);
  assert(strcmp("\"efgh\"", quote_code(code, 5, 9)) == 0);
  assert(strcmp("\"d\"", quote_code(code, 3, 5)) == 0);
}

static void try_filling_codeloc(struct module *mod, struct node *named,
                                struct node *node) {
  if (node_ident(named) != ID_NCODELOC) {
    return;
  }

  node_subs_remove(named, subs_first(named));
  GSTART();
  G0(init, named, INIT,
     G_IDENT(wheren, "where");
     G(wheres, STRING);
     G_IDENT(exprn, "expression");
     G(exprs, STRING));

  const size_t len = 64 + strlen(mod->filename);
  char *buf = calloc(len, sizeof(char));
  snprintf(buf, len, "\"%s:%d:%d\"", mod->filename,
           node->codeloc.line, node->codeloc.column);
  wheres->as.STRING.value = buf;

  exprs->as.STRING.value = quote_code(mod->parser.data, node->codeloc.pos,
                                      codeloc_pos_after(mod, node));
}

static void insert_missing_optional_arg(struct module *mod, struct node *node,
                                        struct node *after_this, ident name) {
  assert(name != idents_add_string(mod->gctx, "v", 1));
  GSTART();
  G0(named, node, CALLNAMEDARG,
     named->as.CALLNAMEDARG.name = name;
     G(nul, NUL));
  node_subs_remove(node, named);
  node_subs_insert_after(node, after_this, named);

  try_filling_codeloc(mod, named, node);

  error e = catchup(mod, NULL, named, CATCHUP_BELOW_CURRENT);
  assert(!e);
}

static error fill_in_optional_args(struct module *mod, struct node *node,
                                   const struct typ *tfun) {
  const struct node *dfun = typ_definition_const(tfun);
  const size_t dmin = node_fun_min_args_count(dfun);
  const size_t dmax = node_fun_max_args_count(dfun);

  if (dmin == dmax) {
    return 0;
  }

  const struct node *funargs = subs_at_const(dfun, IDX_FUNARGS);
  const struct node *darg = subs_first_const(funargs);
  struct node *arg = next(subs_first(node));

  ssize_t n, code_pos = 0;
  error e;
  for (n = 0; n < dmin; ++n, ++code_pos) {
    if (arg == NULL) {
      e = mk_except(mod, arg, "missing positional argument '%s' at position %zd",
                    idents_value(mod->gctx, node_ident(darg)), code_pos);
      THROW(e);
    } else if (arg->which == CALLNAMEDARG) {
      if (node_ident(arg) != node_ident(darg)) {
        e = mk_except(mod, arg, "named argument '%s' has bad name"
                      " or appears out of order at position %zd",
                      idents_value(mod->gctx, node_ident(arg)), code_pos);
        THROW(e);
      }
    }

    darg = next_const(darg);
    arg = next(arg);
  }

  const ssize_t first_vararg = node_fun_first_vararg(dfun);
  for (n = dmin; n < dmax && (first_vararg == - 1 || n < first_vararg); ++n) {
    if (arg == NULL) {
      insert_missing_optional_arg(mod, node, subs_last(node), node_ident(darg));

    } else if (arg->which != CALLNAMEDARG) {
      // Assume this is the first vararg

      if (first_vararg == -1) {
        e = mk_except(mod, arg, "excessive positional argument"
                      " or optional argument lacks a name at position %zd", code_pos);
        THROW(e);
      }

      insert_missing_optional_arg(mod, node, prev(arg), node_ident(darg));

    } else if (arg->which == CALLNAMEDARG) {
      const ident name = node_ident(arg);

      while (node_ident(darg) != name) {
        insert_missing_optional_arg(mod, node, prev(arg), node_ident(darg));

        darg = next_const(darg);
        n += 1;
        if ((first_vararg != -1 && n >= first_vararg)
            || next_const(darg) == NULL) {
          e = mk_except(mod, arg, "named argument '%s' has bad name"
                        " or appears out of order at position %zd",
                        idents_value(mod->gctx, name), code_pos);
          THROW(e);
        }
      }

      arg = next(arg);
      code_pos += 1;
    }

    darg = next_const(darg);
  }

  assert(arg == NULL || first_vararg >= 0);
  while (arg != NULL) {
    if (arg->which == CALLNAMEDARG) {
      const ident name = node_ident(arg);
      e = mk_except(mod, arg, "excess named argument '%s'"
                    " or appears out of order at position %zd",
                    idents_value(mod->gctx, name), code_pos);
      THROW(e);
    }
    arg = next(arg);
    code_pos += 1;
  }

  return 0;
}

static error rewrite_unary_call(struct module *mod, struct node *node, struct typ *tfun) {
  struct node *fun = node_new_subnode(mod, node);
  node_subs_remove(node, fun);
  node_move_content(fun, node);
  node_subs_append(node, fun);
  node_set_which(node, CALL);
  set_typ(&fun->typ, tfun);

  error e = fill_in_optional_args(mod, node, tfun);
  assert(!e);

  const struct node *except[] = { fun, NULL };
  e = catchup(mod, except, node, CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static void bin_accessor_maybe_functor(struct module *mod, struct node *par) {
  // Something like the (hypothetical): vector.mk_filled 100 0:u8
  // 'vector' is a generic functor, and the instantiation will be done
  // through the call to the function 'vector.mk_filled'. We need to have a
  // fully tentative instance of 'vector' so that the unification of '0:u8'
  // with t:`copyable succeeds.
  if (typ_is_generic_functor(par->typ)) {
    struct node *i = instantiate_fully_implicit(mod, par, par->typ);
    unset_typ(&par->typ);
    set_typ(&par->typ, i->typ);
  }
}

static bool bin_accessor_maybe_ref(struct node **parent_scope,
                                   struct module *mod, struct node *par) {
  if (typ_is_reference(par->typ)) {
    *parent_scope = typ_definition(typ_generic_arg(par->typ, 0));
    return true;
  }
  return false;
}

static void bin_accessor_maybe_defchoice(struct node **parent_scope, struct node *for_error,
                                         struct module *mod, struct node *par) {
  if (par->flags & NODE_IS_DEFCHOICE) {
    struct node *defchoice = NULL;
    error e = scope_lookup_ident_immediate(&defchoice, for_error, mod,
                                           &typ_definition(par->typ)->scope,
                                           node_ident(subs_last(par)), false);
    assert(!e);
    assert(defchoice->which == DEFCHOICE);

    *parent_scope = defchoice;
  }
}

static error type_inference_bin_accessor(struct module *mod, struct node *node) {
  error e;

  enum token_type operator = node->as.BIN.operator;
  const struct typ *mark = node->typ;

  struct node *left = subs_first(node);
  bin_accessor_maybe_functor(mod, left);

  struct node *dcontainer = typ_definition(left->typ);
  if (!bin_accessor_maybe_ref(&dcontainer, mod, left)) {
    bin_accessor_maybe_defchoice(&dcontainer, node, mod, left);
  }
  struct scope *container_scope = &dcontainer->scope;

  const bool container_is_tentative = typ_is_tentative(scope_node(container_scope)->typ);

  struct node *name = subs_last(node);
  struct node *field = NULL;
  e = scope_lookup_ident_immediate(&field, name, mod, container_scope,
                                   node_ident(name), container_is_tentative);
  if (container_is_tentative && e == EINVAL) {
    struct node *dinc = defincomplete_create(mod, node);
    defincomplete_add_field(mod, node, dinc, node_ident(name), TBI_ANY);
    e = defincomplete_catchup(mod, dinc);
    EXCEPT(e);

    e = unify(mod, node, left->typ, dinc->typ);
    EXCEPT(e);

    e = scope_lookup_ident_immediate(&field, name, mod, &dinc->scope,
                                     node_ident(name), false);
    EXCEPT(e);
  } else {
    EXCEPT(e);
  }

  if (field->which == IMPORT && !field->as.IMPORT.intermediate_mark) {
    e = scope_lookup(&field, mod, &mod->gctx->modules_root.scope,
                     subs_first(field), false);
    assert(!e);
  }

  if (typ_is_function(field->typ) && mark != TBI__CALL_FUNCTION_SLOT) {
    const bool is_method = typ_definition_const(field->typ)->which == DEFMETHOD;
    if (node_fun_min_args_count(field) != (is_method ? 1 : 0)) {
      e = mk_except_call_args_count(mod, node, field, is_method, 0);
      THROW(e);
    }

    e = rewrite_unary_call(mod, node, field->typ);
    EXCEPT(e);
  } else {
    if (operator == TWILDCARD && typ_is_reference(field->typ)) {
      assert(typ_is_reference(field->typ));
      struct node *i = NULL;
      e = reference(&i, mod, node, TREFWILDCARD,
                    typ_generic_arg(field->typ, 0));
      assert(!e);
      set_typ(&node->typ, i->typ);
    } else {
      set_typ(&node->typ, field->typ);
    }
    assert(field->which != BIN || field->flags != 0);
    node->flags = field->flags;
  }

  if (!(node->flags & NODE_IS_TYPE)) {
    if (!(node->flags & NODE_IS_TEMPORARY)
        && !(subs_first(node)->flags & NODE_IS_DEFCHOICE)) {
      e = typ_check_deref_against_mark(mod, node, mark, operator);
      EXCEPT(e);
    }
  }

  return 0;
}

static error type_inference_bin_rhs_unsigned(struct module *mod, struct node *node) {
  error e;
  struct node *left = subs_first(node);
  struct node *right = subs_last(node);

  e = try_insert_automagic_deref(mod, right);
  EXCEPT(e);
  right = subs_last(node);

  e = unify(mod, right, right->typ, TBI_U32);
  EXCEPT(e);

  set_typ(&node->typ, create_tentative(mod, node, TBI_BITWISE));
  e = unify(mod, node, left->typ, node->typ);
  EXCEPT(e);

  return 0;
}

static error type_inference_bin_rhs_type(struct module *mod, struct node *node) {
  error e;
  struct node *left = subs_first(node);
  struct node *right = subs_last(node);

  if (!(right->flags & NODE_IS_TYPE)) {
    e = mk_except_type(mod, right, "right-hand side not a type");
    THROW(e);
  }

  e = unify(mod, node, left->typ, right->typ);
  EXCEPT(e);

  set_typ(&node->typ, left->typ);

  return 0;
}

static error type_inference_bin(struct module *mod, struct node *node) {
  assert(node->which == BIN);

  error e;
  switch (OP_KIND(node->as.BIN.operator)) {
  case OP_BIN_SYM:
  case OP_BIN_SYM_BOOL:
  case OP_BIN_SYM_ARITH:
  case OP_BIN_SYM_BW:
  case OP_BIN_SYM_PTR:
    e = check_terms_not_types(mod, node);
    EXCEPT(e);
    return type_inference_bin_sym(mod, node);
  case OP_BIN_BW_RHS_UNSIGNED:
    e = check_terms_not_types(mod, node);
    EXCEPT(e);
    return type_inference_bin_rhs_unsigned(mod, node);
  case OP_BIN_ACC:
    return type_inference_bin_accessor(mod, node);
  case OP_BIN_RHS_TYPE:
    return type_inference_bin_rhs_type(mod, node);
  default:
    assert(false);
    return 0;
  }
}

static error typ_tuple(struct node **result, struct module *mod, struct node *node) {
  const size_t arity = subs_count(node);
  struct typ **args = calloc(arity, sizeof(struct typ *));
  size_t n = 0;
  FOREACH_SUB(s, node) {
    args[n] = s->typ;
    n += 1;
  }

  error e = instantiate(result, mod, node, 0,
                        typ_lookup_builtin_tuple(mod, arity), args, arity);
  EXCEPT(e);

  free(args);

  return 0;
}

static error type_inference_tuple(struct module *mod, struct node *node) {
  size_t n = 0;
  FOREACH_SUB(s, node) {
    if (n > 0 && (node->flags & NODE_IS_TYPE) != (s->flags & NODE_IS_TYPE)) {
      error e = mk_except_type(mod, s, "tuple combines values and types");
      THROW(e);
    }
    node->flags |= (s->flags & NODE__TRANSITIVE);
    n += 1;
  }

  struct node *i = NULL;
  error e = typ_tuple(&i, mod, node);
  EXCEPT(e);

  set_typ(&node->typ, i->typ);

  return 0;
}

static void type_inference_init_named(struct module *mod, struct node *node) {
  struct node *dinc = defincomplete_create(mod, node);

  FOREACH_SUB_EVERY(s, node, 0, 2) {
    const struct node *left = s;
    const struct node *right = next(s);
    defincomplete_add_field(mod, s, dinc, node_ident(left), right->typ);
  }

  error e = defincomplete_catchup(mod, dinc);
  assert(!e);
  set_typ(&node->typ, dinc->typ);

  if (node->as.INIT.is_range) {
    e = unify(mod, node, node->typ, TBI_INDEX_RANGE);
    assert(!e);
  } else if (node->as.INIT.is_bounds) {
    e = unify(mod, node, node->typ, TBI_INDEX_BOUNDS);
    assert(!e);
  }
}

static error type_inference_init_array(struct module *mod, struct node *node) {
  set_typ(&node->typ, create_tentative(mod, node, TBI_STATIC_ARRAY));

  FOREACH_SUB(s, node) {
    error e = unify(mod, s, s->typ,
                    typ_generic_arg(node->typ, 0));
    EXCEPT(e);
  }

  return 0;
}

static void type_inference_init_isalist_literal(struct module *mod, struct node *node) {
  struct node *dinc = defincomplete_create(mod, node);

  FOREACH_SUB(s, node) {
    defincomplete_add_isa(mod, s, dinc, s->typ);
  }

  error e = defincomplete_catchup(mod, dinc);
  assert(!e);
  set_typ(&node->typ, dinc->typ);

  node->flags |= NODE_IS_TYPE;
}

static error type_inference_init(struct module *mod, struct node *node) {
  assert(node->which == INIT);
  if (node->as.INIT.is_array) {
    if (!typ_is_literal(subs_first(node)->typ)
        && typ_definition(subs_first(node)->typ)->which == DEFINTF) {
      type_inference_init_isalist_literal(mod, node);
      return 0;
    } else {
      return type_inference_init_array(mod, node);
    }
  } else {
    type_inference_init_named(mod, node);
    return 0;
  }
}

static error type_inference_return(struct module *mod, struct node *node) {
  assert(node->which == RETURN);

  if (subs_count_atleast(node, 1)) {
    struct node *arg = subs_first(node);
    error e = unify_refcompat(mod, arg, module_retval_get(mod)->typ, arg->typ);
    EXCEPT(e);
  }

  set_typ(&node->typ, TBI_VOID);

  return 0;
}

static enum token_type refop_for_accop[] = {
  [TDOT] = TREFDOT,
  [TBANG] = TREFBANG,
  [TSHARP] = TREFSHARP,
  [TWILDCARD] = TREFWILDCARD,
};

static enum token_type accop_for_refop[] = {
  [TREFDOT] = TDOT,
  [TREFBANG] = TBANG,
  [TREFSHARP] = TSHARP,
  [TREFWILDCARD] = TWILDCARD,
};

static enum token_type derefop_for_accop[] = {
  [TDOT] = TDEREFDOT,
  [TBANG] = TDEREFBANG,
  [TSHARP] = TDEREFSHARP,
  [TWILDCARD] = TDEREFWILDCARD,
};

struct node *expr_ref(struct module *mod, struct node *par,
                      enum token_type refop, struct node *node) {
  if (node->which == BIN && OP_KIND(node->as.BIN.operator) == OP_BIN_ACC) {
    // Of the form
    //   self.x.y!method args
    // which was transformed to
    //   type.method @!self.x.y args
    // We actually need
    //   type.method @!self.x!y args
    // This is assuming that typing has checked the transformation below is
    // legal.
    node->as.BIN.operator = accop_for_refop[refop];
  }

  struct node *n = mk_node(mod, par, UN);
  n->codeloc = node->codeloc;
  n->as.UN.operator = refop;
  node_subs_append(n, node);
  return n;
}

static error rewrite_self(struct module *mod, struct node *node,
                          struct node *fun) {
  assert(fun->which == BIN);

  struct node *old_self = subs_first(fun);
  struct node *self;
  if (typ_is_reference(old_self->typ)) {
    node_subs_remove(fun, old_self);
    node_subs_insert_after(node, subs_first(node), old_self);
    self = old_self;
  } else {
    node_subs_remove(fun, old_self);
    enum token_type access = refop_for_accop[fun->as.BIN.operator];
    struct node *s = expr_ref(mod, node, access, old_self);
    node_subs_remove(node, s);
    node_subs_insert_after(node, subs_first(node), s);
    self = s;
  }

  const struct node *except[] = { old_self, NULL };
  error e = catchup(mod, except, self, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  if (typ_is_reference(self->typ)) {
    e = typ_check_can_deref(mod, fun, self->typ,
                            derefop_for_accop[fun->as.BIN.operator]);
    EXCEPT(e);
  }

  return 0;
}

static bool compare_ref_depth(const struct typ *target, const struct typ *arg,
                              int diff) {
  assert(!typ_equal(target, TBI_ANY_ANY_REF));

  if (typ_equal(arg, TBI_LITERALS_NULL)) {
    return diff != 1;
  }

  int dtarget = 0;
  while (typ_is_reference(target)) {
    dtarget += 1;
    target = typ_generic_arg_const(target, 0);
  }

  int darg = 0;
  while (typ_is_reference(arg)) {
    darg += 1;
    arg = typ_generic_arg_const(arg, 0);

    if (typ_equal(arg, TBI_LITERALS_NULL)) {
      return diff != 1;
    }
  }

  return dtarget == darg + diff;
}

static error try_insert_const_ref(struct module *mod, struct node *node,
                                  const struct typ *target,
                                  enum token_type target_explicit_ref,
                                  struct node *arg) {
  if (!typ_is_reference(target)) {
    return 0;
  }

  const bool is_named = arg->which == CALLNAMEDARG;
  struct node *real_arg = is_named ? subs_first(arg) : arg;
  struct node *expr_arg = follow_ssa(real_arg);

  if (target_explicit_ref == TREFDOT || target_explicit_ref == TNULREFDOT) {
    if (compare_ref_depth(target, real_arg->typ, 1)) {
      if (!typ_isa(target, TBI_ANY_MUTABLE_REF)) {
        struct node *before = prev(real_arg);

        struct node *par = parent(real_arg);
        node_subs_remove(par, real_arg);
        struct node *ref_arg = expr_ref(mod, par, TREFDOT, real_arg);
        node_subs_remove(par, ref_arg);
        node_subs_insert_after(par, before, ref_arg);

        if (is_named) {
          unset_typ(&arg->typ);
        }

        const struct node *except[] = { real_arg, NULL };
        error e = catchup(mod, except,
                          is_named ? arg : ref_arg,
                          CATCHUP_BELOW_CURRENT);
        EXCEPT(e);
      }
    } else if (compare_ref_depth(target, real_arg->typ, 0)) {
      if (expr_arg->which == UN
          && expr_arg->as.UN.operator == TREFDOT
          && expr_arg->as.UN.is_explicit
          && !typ_equal(subs_first(expr_arg)->typ, TBI_LITERALS_NULL)) {
        error e = mk_except_type(mod, expr_arg, "explicit '@' operators are not"
                                 " allowed for unqualified const references");
        THROW(e);
      }
    }
  } else {
    // We do not automagically insert const ref operators when the
    // function does not always accept a reference, as in: (also see
    // t00/automagicref.n)
    //   s (method t:`any) foo p:t = void
    //     noop
    //   (s.foo @i32) self @1 -- '@' required on 1.
    //   (s.foo i32) self 1

    // noop -- this error will get caught by regular argument typing.
  }

  return 0;
}

static error try_insert_const_deref(struct module *mod, struct node *node,
                                    const struct typ *target,
                                    enum token_type target_explicit_ref,
                                    struct node *arg) {
  if (typ_is_reference(target)) {
    return 0;
  }

  const bool is_named = arg->which == CALLNAMEDARG;
  struct node *real_arg = is_named ? subs_first(arg) : arg;

  if (target_explicit_ref == 0 && typ_isa(arg->typ, TBI_ANY_REF)) {
    struct node *before = prev(real_arg);

    struct node *par = parent(real_arg);
    node_subs_remove(par, real_arg);
    struct node *deref_arg = mk_node(mod, par, UN);
    deref_arg->as.UN.operator = TDEREFDOT;
    node_subs_append(deref_arg, real_arg);
    node_subs_remove(par, deref_arg);
    node_subs_insert_after(par, before, deref_arg);

    if (is_named) {
      unset_typ(&arg->typ);
    }

    const struct node *except[] = { real_arg, NULL };
    error e = catchup(mod, except,
                      is_named ? arg : deref_arg,
                      CATCHUP_BELOW_CURRENT);
    EXCEPT(e);
  }

  return 0;
}

static enum token_type has_explicit_ref(const struct node *dfun, size_t n) {
  const struct node *funargs = subs_at_const(dfun, IDX_FUNARGS);
  const struct node *darg = subs_at_const(funargs, n);
  if (subs_last_const(darg)->which == UN) {
    return subs_last_const(darg)->as.UN.operator;
  }
  return 0;
}

static error process_automagic_call_arguments(struct module *mod,
                                              struct node *node,
                                              const struct typ *tfun) {
  if (!subs_count_atleast(node, 2)) {
    return 0;
  }

  const struct node *dfun = typ_definition_const(tfun);
  const ssize_t first_vararg = node_fun_first_vararg(dfun);

  error e;
  ssize_t n = 0;
  struct node *last = NULL;
  struct node *nxt = subs_at(node, 1);
  while (nxt != NULL) {
    if (n == first_vararg) {
      break;
    }

    // We record 'nxt' now as try_insert_const_{,de}ref() may move 'arg'.
    struct node *arg = nxt;
    nxt = next(nxt);

    const enum token_type explicit_ref = has_explicit_ref(dfun, n);

    e = try_insert_const_ref(mod, node,
                             typ_function_arg_const(tfun, n),
                             explicit_ref, arg);
    EXCEPT(e);

    e = try_insert_const_deref(mod, node,
                               typ_function_arg_const(tfun, n),
                               explicit_ref, arg);
    EXCEPT(e);

    n += 1;
    last = arg;
  }

  if (n == first_vararg) {
    const struct typ *target = typ_generic_arg_const(
      typ_function_arg_const(tfun, n), 0);

    struct node *nxt = last == NULL
      ? next(subs_first(node)) : next(last);
    while (nxt != NULL) {
      // We record 'nxt' now as try_insert_const_ref() may move 'arg'.
      struct node *arg = nxt;
      nxt = next(nxt);

      e = try_insert_const_ref(mod, node, target, TREFDOT, arg);
      EXCEPT(e);
    }
  }

  return 0;
}

static error prepare_call_arguments(struct module *mod, struct node *node) {
  error e;
  struct node *fun = subs_first(node);

  const struct node *dfun = typ_definition_const(fun->typ);
  const size_t dmin = node_fun_min_args_count(dfun);
  const size_t dmax = node_fun_max_args_count(dfun);

  const size_t args = subs_count(node) - 1;

  switch (dfun->which) {
  case DEFFUN:
    if (args < dmin || args > dmax) {
      e = mk_except_call_args_count(mod, node, dfun, false, args);
      THROW(e);
    }
    break;
  case DEFMETHOD:
    if (fun->which == BIN) {
      if ((subs_first(fun)->flags & NODE_IS_TYPE)) {
        // Form (type.method self ...).
        if (args < dmin || args > dmax) {
          e = mk_except_call_args_count(mod, node, dfun, false, args);
          THROW(e);
        }
      } else {
        // Form (self.method ...); rewrite as (type.method self ...).
        if (args+1 < dmin || args+1 > dmax) {
          e = mk_except_call_args_count(mod, node, dfun, true, args);
          THROW(e);
        }

        struct node *m = mk_node(mod, node, DIRECTDEF);
        set_typ(&m->as.DIRECTDEF.typ, fun->typ);
        m->as.DIRECTDEF.flags = NODE_IS_TYPE;
        node_subs_remove(node, m);
        node_subs_replace(node, fun, m);

        e = rewrite_self(mod, node, fun);
        EXCEPT(e);

        e = catchup(mod, NULL, m, CATCHUP_BELOW_CURRENT);
        EXCEPT(e);
      }
    } else if ((fun->flags & NODE_IS_TYPE)) {
      assert(fun->which == CALL || fun->which == DIRECTDEF);
      // Generic method instantiation: (type.method u32 i32) self
      // or DIRECTDEF.
      if (args < dmin || args > dmax) {
        e = mk_except_call_args_count(mod, node, dfun, false, args);
        THROW(e);
      }
    } else {
      assert(false && "Unreached");
    }
    break;
  default:
    assert(false);
  }

  e = fill_in_optional_args(mod, node, fun->typ);
  EXCEPT(e);

  e = process_automagic_call_arguments(mod, node, fun->typ);
  EXCEPT(e);

  return 0;
}

static error explicit_instantiation(struct module *mod, struct node *node) {
  error e;
  struct node *what = subs_first(node);
  if (what->which == BIN && !(subs_first(what)->flags & NODE_IS_TYPE)) {
    e = mk_except_type(mod, what,
                       "explicit generic instantion must use a type as functor;"
                       " e.g. not 'self.method i32'"
                       " but '(my_struct.method i32) self' (FIXME: remove this restriction)");
    THROW(e);
  }

  struct typ *t = what->typ;
  if (mod->state->top_state->is_setgenarg) {
    t = typ_create_tentative_functor(t);
  }

  const size_t given_arity = subs_count(node) - 1;

  const size_t arity = typ_generic_arity(t);
  const size_t first_explicit = typ_generic_first_explicit_arg(t);
  const size_t explicit_arity = arity - first_explicit;
  if (given_arity != explicit_arity) {
    e = mk_except_type(mod, node,
                       "invalid number of explicit generic arguments:"
                       " %zu expected, but %zu given",
                       explicit_arity, given_arity);
    THROW(e);
  }

  struct typ **args = calloc(arity, sizeof(struct typ *));
  size_t n;
  for (n = 0; n < first_explicit; ++n) {
    args[n] = tentative_generic_arg(mod, node, t, n);
  }
  FOREACH_SUB_EVERY(s, node, 1, 1) {
    args[n] = s->typ;
    n += 1;
  }

  struct node *i = NULL;
  e = instantiate(&i, mod, node, 1, t, args, arity);
  EXCEPT(e);

  n = first_explicit;
  FOREACH_SUB_EVERY(s, node, 1, 1) {
    if (typ_generic_arity(typ_generic_arg(i->typ, n)) > 0) {
      assert(typ_is_tentative(typ_generic_functor(typ_generic_arg(i->typ, n))) == typ_is_tentative(typ_generic_functor(s->typ)));
    }
    n += 1;
  }

  free(args);

  set_typ(&node->typ, i->typ);
  topdeps_record(mod, i->typ);
  node->flags |= NODE_IS_TYPE;

  return 0;
}

static error implicit_function_instantiation(struct module *mod, struct node *node) {
  error e;
  struct typ *tfun = subs_first(node)->typ;
  const size_t arity = subs_count(node) - 1;

  // Already checked in prepare_call_arguments().
  assert(arity == typ_function_arity(tfun));

  struct node *i = instantiate_fully_implicit(mod, node, tfun);

  size_t n = 0;
  FOREACH_SUB_EVERY(s, node, 1, 1) {
    e = unify_refcompat(mod, s, typ_function_arg(i->typ, n), s->typ);
    EXCEPT(e);

    if (n == 0) {
      const struct node *genargs = subs_at_const(i, IDX_GENARGS);
      if (i->which == DEFMETHOD
          && subs_count_atleast(genargs, 2)
          && node_ident(subs_first_const(genargs)) == ID_WILDCARD_REF_ARG_SELF) {
        const struct node *wildcard = subs_at_const(genargs, 1);

        if ((node_toplevel_const(i)->flags & TOP_IS_SHALLOW)
            && typ_equal(typ_generic_functor(s->typ), TBI_MREF)) {
          e = unify(mod, s, wildcard->typ, TBI_MMREF);
          EXCEPT(e);
        } else {
          assert(typ_is_reference(s->typ));

          e = unify(mod, s, wildcard->typ, typ_generic_functor(s->typ));
          EXCEPT(e);
        }
      }
    }

    n += 1;
  }

  set_typ(&subs_first(node)->typ, i->typ);
  topdeps_record(mod, i->typ);
  set_typ(&node->typ, typ_function_return(i->typ));

  return 0;
}

static error function_instantiation(struct module *mod, struct node *node) {
  assert(subs_count_atleast(node, 2));

  error e;
  if (subs_at(node, 1)->flags & NODE_IS_TYPE) {
    e = explicit_instantiation(mod, node);
    EXCEPT(e);
  } else {
    e = implicit_function_instantiation(mod, node);
    EXCEPT(e);
  }

  return 0;
}

static error check_consistent_either_types_or_values(struct module *mod,
                                                     struct node *arg0) {
  uint32_t flags = 0;
  for (struct node *s = arg0; s != NULL; s = next(s)) {
    if (s != arg0 && (flags & NODE_IS_TYPE) != (s->flags & NODE_IS_TYPE)) {
      error e = mk_except_type(mod, s, "expression combines types and values");
      THROW(e);
    }
    flags |= s->flags;
  }

  return 0;
}

static error type_inference_explicit_unary_call(struct module *mod, struct node *node, struct node *dfun) {
  const size_t count = subs_count(node);
  if (dfun->which == DEFFUN && count != 1) {
    error e = mk_except_call_args_count(mod, node, dfun, false, count - 1);
    THROW(e);
  } else if (dfun->which == DEFMETHOD && count != 2) {
    error e = mk_except_call_args_count(mod, node, dfun, false, count - 1);
    THROW(e);
  }

  if (dfun->which == DEFMETHOD) {
    struct node *self = subs_at(node, 1);
    error e = unify(mod, self, typ_function_arg(dfun->typ, 0), self->typ);
    EXCEPT(e);
  }

  set_typ(&node->typ, typ_function_return(dfun->typ));

  return 0;
}

static error try_rewrite_operator_sub(struct module *mod, struct node *node) {
  if (!subs_count_atleast(node, 3)) {
    return 0;
  }

  struct node *fun = subs_first(node);
  struct node *self = subs_at(node, 1);
  struct node *arg = subs_at(node, 2);

  if (!(typ_equal(arg->typ, TBI_INDEX_RANGE) || typ_equal(arg->typ, TBI_INDEX_BOUNDS))
      || node_ident(typ_definition_const(fun->typ)) != ID_OPERATOR_AT) {
    return 0;
  }

  assert(typ_is_reference(self->typ));
  struct node *dfun = typ_definition(typ_generic_arg(self->typ, 0));
  struct node *m = node_get_member(dfun, ID_OPERATOR_SUB);
  if (m == NULL) {
    error e = mk_except_type(mod, node, "type '%s' does not have 'operator_sub'",
                             typ_pretty_name(mod, self->typ));
    THROW(e);
  }

  unset_typ(&fun->typ);
  set_typ(&fun->as.DIRECTDEF.typ, m->typ);

  error e = catchup(mod, NULL, fun, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  if (typ_equal(arg->typ, TBI_INDEX_BOUNDS)) {
    // Magically convert v.[10...] to v.[(10...).Range_of v].
    node_subs_remove(node, arg);
    GSTART();
    G0(call, node, CALL,
       G(b, BIN,
         b->as.BIN.operator = TDOT;
         node_subs_append(b, arg);
         G_IDENT(f, "Range_of"));
       G(vn, IDENT,
         vn->as.IDENT.name = node_ident(self)));

    const struct node *except[] = { arg, NULL };
    error e = catchup(mod, except, call, CATCHUP_BELOW_CURRENT);
    EXCEPT(e);
  }

  return 0;
}

static error type_inference_call(struct module *mod, struct node *node) {
  error e;
  struct node *fun = subs_first(node);
  struct typ *tfun = fun->typ;
  struct node *dfun = typ_definition(tfun);

  if (!node_is_fun(dfun)
      || (subs_count_atleast(node, 2)
          && (subs_at(node, 1)->flags & NODE_IS_TYPE))) {

    if (!node_is_fun(dfun)
        && (!node_can_have_genargs(dfun)
            || !subs_count_atleast(subs_at(dfun, IDX_GENARGS), 1))) {
      char *n = typ_pretty_name(mod, dfun->typ);
      e = mk_except_type(mod, fun, "'%s' not a function or a generic", n);
      free(n);
      THROW(e);
    }

    e = explicit_instantiation(mod, node);
    EXCEPT(e);

    return 0;
  }

  e = prepare_call_arguments(mod, node);
  EXCEPT(e);

  // Assumes that operator_at and operator_sub have the same arguments.
  e = try_rewrite_operator_sub(mod, node);
  EXCEPT(e);

  e = check_consistent_either_types_or_values(mod, try_node_subs_at(node, 1));
  EXCEPT(e);

  node->flags |= NODE_IS_TEMPORARY;

  if (subs_count_atleast(subs_at(dfun, IDX_GENARGS), 1)
      && node_toplevel_const(dfun)->generic->our_generic_functor_typ == NULL) {
    e = function_instantiation(mod, node);
    EXCEPT(e);

    return 0;
  }

  if (node_fun_max_args_count(dfun) == 0) {
    return type_inference_explicit_unary_call(mod, node, dfun);
  }

  const ssize_t first_vararg = node_fun_first_vararg(dfun);
  ssize_t n = 0;
  FOREACH_SUB_EVERY(arg, node, 1, 1) {
    if (n == first_vararg) {
      break;
    }
    e = unify_refcompat(mod, arg, typ_function_arg(tfun, n), arg->typ);
    EXCEPT(e);
    n += 1;
  }

  if (n == first_vararg) {
    struct typ *target = typ_generic_arg(typ_function_arg(tfun, n), 0);

    FOREACH_SUB_EVERY(arg, node, 1 + n, 1) {
      e = unify_refcompat(mod, arg, target, arg->typ);
      EXCEPT(e);
    }
  }

  set_typ(&node->typ, typ_function_return(tfun));

  return 0;
}

static error type_inference_block(struct module *mod, struct node *node) {
  error e;

  struct node *last_typable = subs_last(node);
  while (typ_equal(last_typable->typ, TBI__NOT_TYPEABLE)) {
    last_typable = prev(last_typable);
  }

  FOREACH_SUB(s, node) {
    if ((s->flags & NODE_IS_TYPE)) {
      e = mk_except_type(mod, s, "block statements cannot be type names");
      THROW(e);
    }

    if (s->typ == TBI__NOT_TYPEABLE) {
      continue;
    }

    if (s == last_typable) {
      break;
    }

    if (!typ_equal(s->typ, TBI_VOID)) {
      e = mk_except_type(mod, s,
                         "intermediate statements in a block must be of type void"
                         " (except the last one), not '%s'",
                         typ_pretty_name(mod, s->typ));
      THROW(e);
    }
  }

  if (last_typable->which == RETURN) {
    // FIXME: should make sure there are no statements after a RETURN.
    set_typ(&node->typ, TBI_VOID);
  } else {
    set_typ(&node->typ, last_typable->typ);
  }

  return 0;
}

static error type_inference_if(struct module *mod, struct node *node) {
  error e;

  struct node *cond = subs_first(node);
  e = unify(mod, cond, cond->typ,
            create_tentative(mod, node, TBI_GENERALIZED_BOOLEAN));
  EXCEPT(e);

  struct node *yes = next(cond);
  struct node *els = subs_last(node);
  e = unify(mod, els, yes->typ, els->typ);
  EXCEPT(e);

  set_typ(&node->typ, yes->typ);

  return 0;
}

static error unify_match_pattern(struct module *mod, struct node *expr, struct node *pattern) {
  struct node *d = typ_definition(expr->typ);
  assert(d->which == DEFTYPE);
  const bool enum_or_union = d->as.DEFTYPE.kind == DEFTYPE_ENUM
    || d->as.DEFTYPE.kind == DEFTYPE_UNION;

  error e;
  if (!enum_or_union) {
    e = mk_except_type(mod, expr,
                       "must match over an enum or sum type (FIXME: for now)");
    THROW(e);
  }

  e = unify(mod, pattern, pattern->typ, expr->typ);
  EXCEPT(e);

  return 0;
}

static error type_inference_match(struct module *mod, struct node *node) {
  error e;

  struct node *expr = subs_first(node);
  e = try_insert_automagic_deref(mod, expr);
  EXCEPT(e);
  expr = subs_first(node);

  FOREACH_SUB_EVERY(s, node, 1, 2) {
    e = unify_match_pattern(mod, expr, s);
    EXCEPT(e);
  }

  set_typ(&node->typ, subs_at(node, 2)->typ);
  if (subs_count_atleast(node, 4)) {
    FOREACH_SUB_EVERY(s, node, 4, 2) {
      e = unify(mod, s, s->typ, node->typ);
      EXCEPT(e);
    }
  }

  return 0;
}

static bool in_a_body_pass(struct module *mod) {
  return mod->stage->state->passing >= PASSZERO_COUNT + PASSFWD_COUNT;
}

static error type_inference_ident_unknown(struct module *mod, struct node *node) {
  error e;
  if (!in_a_body_pass(mod)) {
    e = mk_except(mod, node, "unknown ident '%s'",
                  idents_value(mod->gctx, node_ident(node)));
    THROW(e);
  }

  struct node *unk = defincomplete_create(mod, node);
  defincomplete_set_ident(mod, node, unk, node_ident(node));
  e = defincomplete_catchup(mod, unk);
  EXCEPT(e);

  // Special marker, so we can rewrite it with the final enum or sum scope
  // in step_check_no_unknown_ident_left().
  node->as.IDENT.non_local_scope = &unk->scope;

  set_typ(&node->typ, unk->typ);
  return 0;
}

static error type_inference_ident(struct module *mod, struct node *node) {
  if (node_is_name_of_globalenv(node)) {
    set_typ(&node->typ, create_tentative(mod, node, TBI_ANY));
    return 0;
  }

  if (node_ident(node) == ID_OTHERWISE) {
    set_typ(&node->typ, create_tentative(mod, node, TBI_ANY));
    return 0;
  }

  struct node *def = NULL;
  error e = scope_lookup(&def, mod, &node->scope, node, true);
  if (e == EINVAL) {
    e = type_inference_ident_unknown(mod, node);
    EXCEPT(e);
    return 0;
  }

  if (def->which == DEFFIELD) {
    e = mk_except(mod, node,
                  "fields identifiers must be accessed through 'self'");
    THROW(e);
  }

  if (def->which == CATCH) {
    // 'node' is a throw or except label.
    return 0;
  }

  if (def->typ == NULL) {
    e = mk_except(mod, node,
                  "identifier '%s' used before its definition",
                  idents_value(mod->gctx, node_ident(node)));
    THROW(e);
  }

  node->as.IDENT.def = def;
  if (parent_const(def)->which == MODULE_BODY
      || parent_const(def)->which == DEFTYPE
      || parent_const(def)->which == DEFINTF) {
    node->as.IDENT.non_local_scope = &parent(def)->scope;
  } else if (def->flags & NODE_IS_GLOBAL_LET) {
    node->as.IDENT.non_local_scope = &parent(parent(parent(def)))->scope;
  }

  if (typ_is_function(def->typ) && node->typ != TBI__CALL_FUNCTION_SLOT) {
    if (node_fun_min_args_count(typ_definition(def->typ)) != 0) {
      e = mk_except_call_args_count(mod, node, typ_definition(def->typ), false, 0);
      THROW(e);
    }

    e = rewrite_unary_call(mod, node, def->typ);
    EXCEPT(e);
  } else {
    set_typ(&node->typ, def->typ);
    node->flags = def->flags;
  }

  return 0;
}

static struct typ* number_literal_typ(struct module *mod, struct node *node) {
  assert(node->which == NUMBER);
  if (strchr(node->as.NUMBER.value, '.') != NULL) {
    return TBI_LITERALS_FLOATING;
  } else {
    return TBI_LITERALS_INTEGER;
  }
}

static error type_inference_within(struct module *mod, struct node *node) {
  node->typ = NULL;

  error e;
  struct node *def = NULL;
  struct node *first = subs_first(node);

  if (node->which == WITHIN) {
    const struct node *modbody = NULL;
    if (first->which == BIN) {
      struct node *ffirst = subs_first(first);
      e = type_inference_within(mod, ffirst);
      EXCEPT(e);

      modbody = typ_definition_const(ffirst->typ);

      if (modbody->which != MODULE_BODY) {
        e = mk_except(mod, node, "invalid within expression,"
                      " must point to a globalenv declaration");
        THROW(e);
      }
    } else if (first->which == IDENT) {
      modbody = node_module_owner_const(node)->body;
    } else {
      goto malformed;
    }

    e = scope_lookup_ident_immediate(&def, node, mod,
                                     &modbody->as.MODULE_BODY.globalenv_scope,
                                     node_ident(subs_last_const(node)), false);
    EXCEPT(e);
  } else if (node->which == IDENT) {
    e = scope_lookup(&def, mod, &node->scope, node, false);
    EXCEPT(e);
  } else if (node->which == BIN) {
    e = type_inference_within(mod, first);
    EXCEPT(e);

    e = scope_lookup_ident_immediate(&def, node,
                                     mod, &typ_definition(first->typ)->scope,
                                     node_ident(subs_last_const(node)), false);
    EXCEPT(e);
  } else {
    goto malformed;
  }

  node->typ = def->typ;
  node->flags |= def->flags;
  return 0;

malformed:
  e = mk_except(mod, node, "malformed within expression");
  THROW(e);
}

static error type_inference_try(struct module *mod, struct node *node) {
  struct node *eblock = subs_last(node);

  struct typ *u = NULL;
  FOREACH_SUB(b, eblock) {
    if (u == NULL) {
      u = b->typ;
    } else {
      error e = unify(mod, b, b->typ, u);
      EXCEPT(e);
    }
  }

  set_typ(&node->typ, u);

  return 0;
}

static error type_inference_defchoice_init(struct module *mod,
                                           struct node *node) {
  error e;
  struct node *left = subs_first(node);
  struct node *right = subs_last(node);
  struct node *dleft = typ_definition(left->typ);
  struct node *dright = typ_definition(right->typ);

  assert(left->which == INIT);
  assert(dleft->which == DEFINCOMPLETE);

  assert(right->flags & NODE_IS_DEFCHOICE);
  assert(right->which == BIN);
  const struct node *dleaf = node_get_member_const(dright,
                                                   node_ident(subs_last(right)));
  assert(dleaf->which == DEFCHOICE);
  if (!dleaf->as.DEFCHOICE.is_leaf) {
    e = mk_except_type(mod, subs_last(right),
                       "only union leaf variants can be initialized");
    THROW(e);
  }

  left->as.INIT.for_tag = node_ident(dleaf);

  FOREACH_SUB_EVERY(name, left, 0, 2) {
    const struct node *d = dleaf;
    struct node *field = NULL;
    while (true) {
      e = scope_lookup_ident_immediate(&field, name, mod, &d->scope,
                                       node_ident(name), true);
      if (!e) {
        break;
      }

      if (d->which == DEFTYPE) {
        assert(false && "field names were checked by unify_with_defincomplete()");
      }

      d = parent_const(d);
    }

    typ_link_tentative(field->typ, next(name)->typ);
  }

  return 0;
}

static error type_inference_typeconstraint(struct module *mod, struct node *node) {
  if (node->as.TYPECONSTRAINT.is_constraint) {
    set_typ(&node->typ, subs_first(node)->typ);
    return 0;
  }

  error e;
  if (subs_first(node)->which == INIT
      && subs_last(node)->flags & NODE_IS_DEFCHOICE) {
    e = type_inference_defchoice_init(mod, node);
    EXCEPT(e);
  }

  set_typ(&node->typ, subs_first(node)->typ);
  e = unify(mod, subs_first(node),
            subs_first(node)->typ, subs_last(node)->typ);
  EXCEPT(e);

  node->flags |= subs_first(node)->flags;
  node->flags |= subs_last(node)->flags & NODE__ASSIGN_TRANSITIVE;
  // Copy flags back, as TYPECONSTRAINT are elided in
  // step_remove_typeconstraints().
  subs_first(node)->flags |= node->flags;

  return 0;
}

STEP_NM(step_type_inference,
        -1);
error step_type_inference(struct module *mod, struct node *node,
                          void *user, bool *stop) {
  DSTEP(mod, node);
  error e;

  if (node->typ == TBI__NOT_TYPEABLE) {
    return 0;
  }

  switch (node->which) {
  case DEFINTF:
  case DEFTYPE:
  case DEFFUN:
  case DEFMETHOD:
  case DEFINCOMPLETE:
    assert(node->typ != NULL);
    // Already typed.
    topdeps_record(mod, node->typ);
    return 0;
  case IMPORT:
    if (node->typ != NULL) {
      // Already typed.
      topdeps_record(mod, node->typ);
      return 0;
    }
    break;
  default:
    break;
  }

  if (node->typ == NULL
      || node->typ == TBI__MUTABLE
      || node->typ == TBI__MERCURIAL
      || node->typ == TBI__CALL_FUNCTION_SLOT
      || node->which == DEFNAME
      || typ_definition_const(node->typ)->which == MODULE
      || typ_definition_const(node->typ)->which == ROOT_OF_ALL) {
    // noop
  } else {
    return 0;
  }
  //assert(node->typ == NULL
  //       || node->typ == TBI__MUTABLE
  //       || node->typ == TBI__MERCURIAL
  //       || node->typ == TBI__CALL_FUNCTION_SLOT
  //       || node->which == DEFNAME
  //       || typ_definition_const(node->typ)->which == MODULE
  //       || typ_definition_const(node->typ)->which == ROOT_OF_ALL);

  switch (node->which) {
  case NUL:
    set_typ(&node->typ, create_tentative(mod, node, TBI_LITERALS_NULL));
    break;
  case IDENT:
    e = type_inference_ident(mod, node);
    EXCEPT(e);
    break;
  case DEFNAME:
    PUSH_STATE(node->as.DEFNAME.phi_state);

    set_typ(&node->typ, subs_last(node)->typ);
    node->flags |= subs_last(node)->flags & NODE__TRANSITIVE;

    if (try_remove_unnecessary_ssa_defname(mod, node)) {
      set_typ(&node->typ, TBI_VOID);
      break;
    }

    if (node->flags & NODE_IS_TYPE) {
      e = mk_except(mod, node, "let cannot be used with a type name (use alias)");
      THROW(e);
    }

    if (typ_equal(node->typ, TBI_VOID)) {
      e = mk_except(mod, node, "cannot define a variable of type 'void'");
      THROW(e);
    }
    break;
  case DEFALIAS:
    set_typ(&node->typ, subs_last(node)->typ);
    node->flags |= subs_last(node)->flags & NODE__TRANSITIVE;
    if (!(node->flags & NODE_IS_TYPE)) {
      e = mk_except(mod, node, "alias cannot be used with a value (use let)");
      THROW(e);
    }
    break;
  case PHI:
    node->typ = TBI__NOT_TYPEABLE;
    break;
  case NUMBER:
    set_typ(&node->typ, create_tentative(mod, node, number_literal_typ(mod, node)));
    break;
  case BOOL:
    set_typ(&node->typ, create_tentative(mod, node, TBI_BOOL));
    break;
  case STRING:
    set_typ(&node->typ, create_tentative(mod, node, TBI_STATIC_STRING));
    break;
  case SIZEOF:
    set_typ(&node->typ, TBI_SIZE);
    break;
  case ALIGNOF:
    set_typ(&node->typ, TBI_SIZE);
    break;
  case BIN:
    e = type_inference_bin(mod, node);
    EXCEPT(e);
    break;
  case UN:
    e = type_inference_un(mod, node);
    EXCEPT(e);
    break;
  case TUPLE:
    e = type_inference_tuple(mod, node);
    EXCEPT(e);
    break;
  case CALLNAMEDARG:
    set_typ(&node->typ, subs_first(node)->typ);
    break;
  case CALL:
    e = type_inference_call(mod, node);
    EXCEPT(e);
    break;
  case INIT:
    e = type_inference_init(mod, node);
    EXCEPT(e);
    break;
  case RETURN:
    e = type_inference_return(mod, node);
    EXCEPT(e);
    break;
  case BLOCK:
    e = type_inference_block(mod, node);
    EXCEPT(e);
    break;
  case CATCH:
    set_typ(&node->typ, subs_last(node)->typ);
    break;
  case THROW:
    {
      struct node *tryy = module_excepts_get(mod)->tryy;
      struct node *err = subs_at(subs_first(subs_first(tryy)), 1);
      assert(err->which == DEFNAME);
      e = unify(mod, node, subs_last(node)->typ, err->typ);
      EXCEPT(e);
      set_typ(&node->typ, TBI_VOID);
      break;
    }
  case JUMP:
  case BREAK:
  case CONTINUE:
  case NOOP:
    set_typ(&node->typ, TBI_VOID);
    break;
  case IF:
    e = type_inference_if(mod, node);
    EXCEPT(e);
    break;
  case WHILE:
    set_typ(&node->typ, TBI_VOID);
    struct node *cond = subs_first(node);
    e = unify(mod, cond, cond->typ, create_tentative(mod, node, TBI_GENERALIZED_BOOLEAN));
    EXCEPT(e);
    struct node *block = subs_at(node, 1);
    e = typ_check_equal(mod, block, block->typ, TBI_VOID);
    EXCEPT(e);
    break;
  case MATCH:
    e = type_inference_match(mod, node);
    EXCEPT(e);
    break;
  case TRY:
    e = type_inference_try(mod, node);
    EXCEPT(e);
    break;
  case DYN:
    assert(typ_is_reference(subs_first(node)->typ));
    set_typ(&node->typ, node->as.DYN.intf_typ);
    break;
  case TYPECONSTRAINT:
    e = type_inference_typeconstraint(mod, node);
    EXCEPT(e);
    break;
  case DEFARG:
    PUSH_STATE(node->as.DEFARG.phi_state);

    set_typ(&node->typ, subs_at(node, 1)->typ);
    if (node->as.DEFARG.is_optional) {
      e = typ_check_isa(mod, node, node->typ, TBI_ANY_NULLABLE_REF);
      EXCEPT(e);
    } else if (node->as.DEFARG.is_vararg) {
      if (!typ_has_same_generic_functor(mod, node->typ, TBI_VARARG)) {
        e = mk_except_type(mod, node,
                           "vararg argument must have type"
                           " (vararg `any_any_ref), not '%s'",
                           typ_pretty_name(mod, node->typ));
        THROW(e);
      }
    }
    break;
  case DEFGENARG:
  case SETGENARG:
    set_typ(&node->typ, subs_at(node, 1)->typ);
    node->flags |= NODE_IS_TYPE;
    {
      struct node *def = nparent(node, 2);
      if (node->which == DEFGENARG) {
        node_toplevel(def)->flags |= TOP_IS_FUNCTOR;
      } else {
        node_toplevel(def)->flags &= ~TOP_IS_FUNCTOR;
      }
    }
    break;
  case DEFFIELD:
    set_typ(&node->typ, subs_at(node, 1)->typ);
    break;
  case LET:
    set_typ(&node->typ, TBI_VOID);
    break;
  case DELEGATE:
    set_typ(&node->typ, TBI_VOID);
    break;
  case EXAMPLE:
  case PRE:
  case POST:
  case INVARIANT:
    set_typ(&node->typ, TBI_VOID);
    break;
  case WITHIN:
    if (subs_count_atleast(node, 1) && subs_first(node)->which != WITHIN) {
      e = type_inference_within(mod, node);
      EXCEPT(e);
    } else {
      set_typ(&node->typ, TBI_VOID);
    }
    break;
  case ISALIST:
  case GENARGS:
  case FUNARGS:
    node->typ = TBI__NOT_TYPEABLE;
    break;
  case IMPORT:
    node->typ = TBI__NOT_TYPEABLE;
    node->flags = NODE_IS_TYPE;
    break;
  case ISA:
    set_typ(&node->typ, subs_first(node)->typ);
    node->flags = subs_first(node)->flags & NODE__TRANSITIVE;
    break;
  case DIRECTDEF:
    set_typ(&node->typ, node->as.DIRECTDEF.typ);
    node->flags = node->as.DIRECTDEF.flags;
    break;
  case DEFCHOICE:
    {
      const struct node *par = node;
      do {
        par = parent_const(par);
      } while (par->which == DEFCHOICE);
      set_typ(&node->typ, par->typ);
    }
    break;
  default:
    break;
  }

  if (node->typ != NULL) {
    topdeps_record(mod, node->typ);
  }

  assert(node->typ != NULL
         || (node->which == IDENT
             && "tolerate when it's a CATCH label or WITHIN label"));
  return 0;
}

STEP_NM(step_type_drop_excepts,
        NM(TRY));
error step_type_drop_excepts(struct module *mod, struct node *node,
                             void *user, bool *stop) {
  DSTEP(mod, node);

  module_excepts_close_try(mod);

  return 0;
}

static void finalize_weakly_concrete(struct module *mod, struct typ *t) {
  assert(typ_is_weakly_concrete(t));
  struct node *d = typ_definition(t);
  struct typ *concrete = node_toplevel(d)->generic->our_generic_functor_typ;
  if (typ_equal(concrete, TBI_BOOL)) {
    typ_link_tentative(TBI_BOOL, t);
  } else if (typ_equal(concrete, TBI_STATIC_STRING)) {
    typ_link_tentative(TBI_STATIC_STRING, t);
  } else if (typ_equal(concrete, TBI_STATIC_ARRAY)) {
    typ_link_tentative(TBI_STATIC_ARRAY, t);
  } else {
    assert(false);
  }
}

static error finalize_generic_instantiation(size_t *remaining,
                                            struct module *mod,
                                            const struct node *for_error,
                                            struct typ *t) {
  if (typ_definition_const(t) == NULL) {
    // 't' was cleared in link_to_final()
    return 0;
  }

  if (typ_generic_arity(t) == 0) {
    // For instance, a DEFINCOMPLETE that unified to a non-generic.
    if (typ_is_weakly_concrete(t)) {
      finalize_weakly_concrete(mod, t);
    }
    return 0;
  }

  if (typ_is_pseudo_builtin(t)) {
    return 0;
  }

  struct typ *functor = typ_generic_functor(t);

  if (typ_is_tentative(t)) {
    if (typ_is_tentative(functor)) {
      *remaining += 1;
      return 0;
    }

    for (size_t m = 0; m < typ_generic_arity(t); ++m) {
      struct typ *arg = typ_generic_arg(t, m);
      if (typ_is_weakly_concrete(arg)) {
        finalize_weakly_concrete(mod, arg);
        continue;
      }

      if (typ_is_tentative(arg)) {
        *remaining += 1;
        return 0;
      }
    }
  }

  const size_t arity = typ_generic_arity(t);

  struct typ *existing = find_existing_final_for_tentative(mod, t);
  if (existing != NULL) {
    typ_link_to_existing_final(existing, t);
    topdeps_record(mod, existing);
    return 0;
  }

  struct typ **args = calloc(arity, sizeof(struct typ *));
  for (size_t m = 0; m < arity; ++m) {
    args[m] = typ_generic_arg(t, m);
  }

  const struct node *instantiating_for_error = node_toplevel_const(typ_definition_const(t))
    ->generic->for_error;
  struct node *i = NULL;
  error e = instantiate(&i, mod, instantiating_for_error, -1, functor, args, arity);
  EXCEPT(e);

  typ_declare_final__privileged(i->typ);
  if (NM(i->which) & STEP_NM_DEFS_NO_FUNS) {
    FOREACH_SUB(m, i) {
      if (NM(m->which) & (NM(DEFFUN) | NM(DEFMETHOD))) {
        typ_declare_final__privileged(m->typ);
      }
    }
  }

  typ_link_to_existing_final(i->typ, t);
  topdeps_record(mod, i->typ);

  free(args);
  return 0;
}

// Typically, unify_with_defincomplete() finishes the job. But in some cases
// it's not enough.
//   fun foo = error
//     let e = {}:(definc {})
//     let a = {x=0}:(definc {x:`integer})
//     e:(definc {x:`integer}) = a -- a.typ is linked to e.typ
//     return e
// The typing of the return unifies 'e.typ' to 'error', which correctly
// updates 'a.typ' to 'error', but unify_with_defincomplete() only sees
// 'error' and 'e.typ', and doesn't know to find the definition 'a.typ' and
// unify the fields of *that* DEFINCOMPLETE (it does know about the
// different DEFINCOMPLETE behind 'e.typ').
//
// So we fix it after the fact.
static void finalize_defincomplete_unification(struct module *mod, struct node *dinc) {
  error e = unify_with_defincomplete_entrails(mod, dinc, dinc->typ,
                                              dinc->typ, dinc);
  assert(!e);
}

// See bootstrap/types.c for some ideas on how to get rid of this step.
STEP_NM(step_gather_final_instantiations,
        NM(DEFTYPE) | NM(DEFINTF) | NM(DEFFUN) | NM(DEFMETHOD) | NM(EXAMPLE));
error step_gather_final_instantiations(struct module *mod, struct node *node,
                                       void *user, bool *stop) {
  DSTEP(mod, node);

  struct toplevel *toplevel = node_toplevel(mod->state->top_state->top);
  if (vecnode_count(&toplevel->triggered_instantiations) == 0) {
    return 0;
  }

  if (typ_is_tentative(node->typ)) {
    return 0;
  }

  const struct node *par = parent_const(node);
  if (par->which != MODULE_BODY
      && typ_is_generic_functor(par->typ)) {
    return 0;
  }

  size_t prev_remaining, remaining = 0;
again:
  prev_remaining = remaining;
  remaining = 0;

  for (size_t n = 0; n < vecnode_count(&toplevel->triggered_instantiations); ++n) {
    struct node *d = *vecnode_get(&toplevel->triggered_instantiations, n);
    struct typ *t = d->typ;
    if (d->which == DEFINCOMPLETE) {
      finalize_defincomplete_unification(mod, d);
    } else {
      error e = finalize_generic_instantiation(&remaining, mod, node, t);
      EXCEPT(e);
    }
  }

  if (remaining > 0) {
    if (remaining != prev_remaining) {
      // Need to loop over, as the tentative instances may depend on each
      // others, and they may appear in an arbitrary order in the vector.
      // TODO: sort them first so we can avoid this quadratic behavior.
      goto again;
    } else {
      // FIXME: this sometimes is an error (under-constrained generic)
      // that gets caught in cprinter or gcc. We don't know how to tell
      // that case apart from a well-constrained generic in a
      // non-instantiated generic. There is probably a bug somewhere.
    }
  }

  if (node->which == DEFINTF) {
    struct node *isal = subs_at(node, IDX_ISALIST);
    FOREACH_SUB(isa, isal) {
      assert(!typ_is_tentative(isa->typ));
    }
  }

  vecnode_destroy(&toplevel->triggered_instantiations);

  return 0;
}
