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
  return instantiate_fully_implicit(mod, for_error, functor);
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
        NM(DEFCHOICE) | NM(WITHIN) | NM(THROW));
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
      last->typ = not_typeable;

      const struct node *par = parent_const(node);
      if (NM(par->which) & (NM(DEFFUN) | NM(DEFMETHOD))
          && subs_first_const(par) == node) {
        first->typ = NULL;
      }
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
    case TOVLSHIFT_ASSIGN:
      first->typ = TBI__MUTABLE;
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
    case TREFWILDCARD:
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
        first->typ = TBI__MUTABLE;
      }
      break;
    case TREFSHARP:
      if (first->typ != NULL) {
        if (first->which == BIN && !(first->flags & NODE_IS_TYPE)) {
          error e = typ_check_deref_against_mark(mod, first, TBI__MERCURIAL,
                                                 first->as.BIN.operator);
          EXCEPT(e);
        }
      } else {
        first->typ = TBI__MERCURIAL;
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
static ERROR check_in_try(struct module *mod, struct node *node, const char *which) {
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

static ERROR optional(struct typ **result,
                      struct module *mod, struct node *for_error,
                      struct typ *typ) {
  if (typ_equal(typ, TBI_LITERALS_NIL)) {
    return 0;
  }
  error e = instantiate(result, mod, for_error, 0, TBI_OPTIONAL, &typ, 1, false);
  EXCEPT(e);

  topdeps_record(mod, *result);
  return 0;
}

static ERROR reference_functor(struct typ **result,
                               struct module *mod, struct node *for_error,
                               struct typ *functor, struct typ *typ) {
  error e = instantiate(result, mod, for_error, 0, functor, &typ, 1, false);
  EXCEPT(e);

  topdeps_record(mod, *result);
  return 0;
}

error reference(struct typ **result,
                struct module *mod, struct node *for_error,
                enum token_type op, struct typ *typ) {
  struct typ *f = mod->gctx->builtin_typs_for_refop[op];
  error e = reference_functor(result, mod, for_error, f, typ);
  EXCEPT(e);
  return 0;
}

static struct typ *nullable_functor(struct typ *t) {
  struct typ *t0 = typ_generic_functor(t);
  if (typ_isa(t0, TBI_ANY_NULLABLE_REF)) {
    return t0;
  }

  if (typ_equal(t0, TBI_ANY_REF)) {
    return TBI_ANY_NULLABLE_REF;
  } else if (typ_equal(t0, TBI_ANY_MUTABLE_REF)) {
    return TBI_ANY_NULLABLE_MUTABLE_REF;
  } else if (typ_equal(t0, TBI_REF)) {
    return TBI_NREF;
  } else if (typ_isa(t0, TBI_NREF)) {
    return TBI_NMREF;
  } else if (typ_isa(t0, TBI_MMREF)) {
    return TBI_NMMREF;
  } else {
    assert(false);
    return NULL;
  }
}

struct wildcards {
  enum token_type ref;
  enum token_type nulref;
  enum token_type deref;
  enum token_type acc;
};

static void fill_wildcards(struct wildcards *w, struct typ *r0) {
  if (typ_equal(r0, TBI_REF)
      || typ_equal(r0, TBI_NREF)
      || typ_equal(r0, TBI_ANY_REF)
      || typ_equal(r0, TBI_ANY_NULLABLE_REF)) {
    w->ref = TREFDOT;
    w->nulref = TNULREFDOT;
    w->deref = TDEREFDOT;
    w->acc = TDOT;
  } else if (typ_equal(r0, TBI_MREF) || typ_equal(r0, TBI_NMREF)) {
    w->ref = TREFBANG;
    w->nulref = TNULREFBANG;
    w->deref = TDEREFBANG;
    w->acc = TBANG;
  } else if (typ_equal(r0, TBI_MMREF)
             || typ_equal(r0, TBI_NMMREF)) {
    w->ref = TREFSHARP;
    w->nulref = TNULREFSHARP;
    w->deref = TDEREFSHARP;
    w->acc = TSHARP;
  } else {
    assert(false);
  }
}

static ERROR try_wildcard_op(struct typ **r0, enum token_type *rop,
                             struct module *mod, struct node *node) {
  error e;
  struct typ *w0 = NULL, *self0 = NULL;
  struct wildcards w = { 0 }, selfw = { 0 };

  struct node *top = mod->state->top_state->top;

  struct wildcards *ww = NULL;
  if (top->which == DEFFUN) {
    w0 = typ_definition_deffun_wildcard_functor(top->typ);
    if (w0 == NULL) {
      goto not_wildcard_fun;
    }

    fill_wildcards(&w, w0);

    *r0 = w0;
    ww = &w;
  } else if (top->which == DEFMETHOD) {
    w0 = typ_definition_defmethod_wildcard_functor(top->typ);
    if (w0 == NULL) {
      goto not_wildcard_fun;
    }
    self0 = typ_definition_defmethod_self_wildcard_functor(top->typ);

    fill_wildcards(&w, w0);
    fill_wildcards(&selfw, self0);

    if (node_ident(subs_first_const(node)) == ID_SELF) {
      *r0 = self0;
      ww = &selfw;
    } else {
      *r0 = w0;
      ww = &w;
    }
  }

  bool reject_wildcard = false;
  if (false) {
not_wildcard_fun:
    reject_wildcard = true;
  }

  bool is_wildcard = true;
  switch (node->which) {
  case UN:
    switch (node->as.UN.operator) {
    case TREFWILDCARD:
      *rop = ww->ref;
      break;
    case TNULREFWILDCARD:
      *rop = ww->nulref;
      *r0 = nullable_functor(*r0);
      break;
    case TDEREFWILDCARD:
      *rop = ww->deref;
      break;
    default:
      *r0 = mod->gctx->builtin_typs_for_refop[node->as.UN.operator];
      *rop = node->as.UN.operator;
      is_wildcard = false;
      break;
    }
    break;
  case BIN:
    switch (node->as.BIN.operator) {
    case TWILDCARD:
      *rop = ww->acc;
      break;
    default:
      *r0 = mod->gctx->builtin_typs_for_refop[node->as.BIN.operator];
      *rop = node->as.BIN.operator;
      is_wildcard = false;
      break;
    }
    break;
  default:
    assert(false);
  }

  if (reject_wildcard && is_wildcard
      && (!(NM(top->which) & (NM(DEFFUN) | NM(DEFMETHOD)))
          || typ_generic_arity(node->typ) == 0)) {
    e = mk_except_type(mod, node, "cannot use wildcards outside"
                       " of a wildcard function or method");
    THROW(e);
  }

  return 0;
}

static ERROR wrap_arg_unary_op(struct node **r, struct module *mod,
                               enum token_type op) {
  struct node *arg = *r;
  const bool is_named = arg->which == CALLNAMEDARG;
  struct node *real_arg = is_named ? subs_first(arg) : arg;

  struct node *before = prev(real_arg);
  struct node *par = parent(real_arg);
  node_subs_remove(par, real_arg);
  struct node *deref_arg = mk_node(mod, par, UN);
  deref_arg->as.UN.operator = op;
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

  if (!is_named) {
    // arg was changed
    *r = deref_arg;
  }
  return 0;
}

static ERROR check_terms_not_types(struct module *mod, struct node *node) {
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
    if (def == NULL) {
      return NULL;
    }

    if (def->which == DEFNAME
        && def->as.DEFNAME.ssa_user == expr) {
      expr = subs_last(def);
    }
  }
  return expr;
}

static ERROR insert_automagic_de(struct node **r,
                                 struct module *mod,
                                 enum token_type op) {
  struct node *node = *r;
  struct node *expr = follow_ssa(node);
  if (expr != NULL
      && expr->which == UN
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
  deref->as.UN.operator = op;

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

  *r = deref;
  return 0;
}

// May modifies node. Get the new one after calling this (same location in
// the tree).
static ERROR try_insert_automagic_de(struct module *mod,
                                     struct node *node) {
  if (typ_is_optional(node->typ)) {
    error e = insert_automagic_de(&node, mod, T__DEOPT);
    EXCEPT(e);
  }
  if (typ_is_reference(node->typ)) {
    error e = insert_automagic_de(&node, mod, TDEREFDOT);
    EXCEPT(e);
  }
  if (typ_is_optional(node->typ)) {
    error e = insert_automagic_de(&node, mod, T__DEOPT);
    EXCEPT(e);
  }
  return 0;
}

static ERROR nullable_op(enum token_type *r,
                         struct module *mod, const struct node *for_error,
                         struct typ *t) {
  if (typ_isa(t, TBI_ANY_NULLABLE_REF)) {
    *r = 0;
    return 0;
  }

  struct typ *t0 = typ_generic_functor(t);
  if (t0 == NULL) {
    error e = mk_except_type(mod, for_error, "Nullable expects a reference, not '%s'",
                             pptyp(mod, t));
    THROW(e);
  }

  if (typ_equal(t0, TBI_MMREF)) {
    *r = TNULREFSHARP;
  } else if (typ_isa(t0, TBI_ANY_MUTABLE_REF)) {
    *r = TNULREFBANG;
  } else if (typ_isa(t0, TBI_ANY_REF)) {
    *r = TNULREFDOT;
  } else {
    assert(false);
  }

  return 0;
}

static ERROR type_inference_un(struct module *mod, struct node *node) {
  assert(node->which == UN);
  error e;

  enum token_type rop = 0;
  struct typ *rfunctor = NULL;
  e = try_wildcard_op(&rfunctor, &rop, mod, node);
  EXCEPT(e);

  struct node *term = subs_first(node);
  struct typ *i = NULL;

rewrote_op:
  switch (OP_KIND(rop)) {
  case OP_UN_PRIMITIVES:
    switch (rop) {
    case T__NULLABLE:
      {
        enum token_type nop = 0;
        e = nullable_op(&nop, mod, node, term->typ);
        EXCEPT(e);
        if (nop == 0) {
          set_typ(&node->typ, term->typ);
        } else {
          e = reference(&i, mod, node, nop, typ_generic_arg(term->typ, 0));
          EXCEPT(e);
          set_typ(&node->typ, i);
          node->flags |= term->flags & NODE__TRANSITIVE;
        }
      }
      break;
    default:
      assert(false);
    }
    return 0;
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
    e = reference_functor(&i, mod, node, rfunctor, term->typ);
    EXCEPT(e);
    set_typ(&node->typ, i);
    node->flags |= term->flags & NODE__TRANSITIVE;
    return 0;
  case OP_UN_DEREF:
    e = typ_check_can_deref(mod, term, term->typ, rop);
    EXCEPT(e);
    e = typ_check_deref_against_mark(mod, node, node->typ, rop);
    EXCEPT(e);
    set_typ(&node->typ, typ_generic_arg(term->typ, 0));
    node->flags |= term->flags & NODE__TRANSITIVE;
    return 0;
  case OP_UN_OPT:
    switch (rop) {
    case TPOSTQMARK:
      set_typ(&node->typ, TBI_BOOL);
      break;
    case TPREQMARK:
      e = optional(&i, mod, node, term->typ);
      EXCEPT(e);
      set_typ(&node->typ, i);
      node->flags |= term->flags & NODE__TRANSITIVE;
      break;
    case T__DEOPT_DEREFDOT:
      if (typ_is_reference(term->typ)) {
        rop = TDEREFDOT;
        node->as.UN.operator = rop;
        goto rewrote_op;
      }
      rop = T__DEOPT;
      node->as.UN.operator = rop;
      // fallthrough
    case T__DEOPT:
      assert(typ_is_optional(term->typ));
      set_typ(&node->typ, typ_generic_arg(term->typ, 0));
      node->flags |= term->flags & NODE__TRANSITIVE;
      break;
    default:
      assert(false);
    }
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

  e = try_insert_automagic_de(mod, term);
  EXCEPT(e);
  // may have been modified, get the new one
  term = subs_first(node);

  switch (OP_KIND(rop)) {
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
  case OP_UN_OVARITH:
    set_typ(&node->typ, create_tentative(mod, node, TBI_OVERFLOW_ARITHMETIC));
    e = unify(mod, node, node->typ, term->typ);
    EXCEPT(e);
    break;
  case OP_UN_BW:
    set_typ(&node->typ, create_tentative(mod, node, TBI_HAS_BITWISE_OPERATORS));
    e = unify(mod, node, node->typ, term->typ);
    EXCEPT(e);
    break;
  default:
    assert(false);
  }

  return 0;
}

static ERROR type_inference_bin_sym(struct module *mod, struct node *node) {
  assert(node->which == BIN);

  const enum token_type operator = node->as.BIN.operator;

  struct node *left = subs_first(node);
  struct node *right = subs_last(node);
  error e;

  if (!OP_IS_ASSIGN(operator) && OP_KIND(operator) != OP_BIN_SYM_PTR) {
    e = try_insert_automagic_de(mod, left);
    EXCEPT(e);
    e = try_insert_automagic_de(mod, right);
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

    const bool lisnil = typ_equal(left->typ, TBI_LITERALS_NIL);
    const bool lisdefinc = typ_definition_which(left->typ) == DEFINCOMPLETE;
    const bool lisref = typ_is_reference(left->typ);
    const bool lisopt = typ_is_optional(left->typ);
    const bool risnil = typ_equal(right->typ, TBI_LITERALS_NIL);
    const bool risref = typ_is_reference(right->typ);
    const bool risopt = typ_is_optional(right->typ);
    if (!lisdefinc && !lisref && !lisopt && !risnil && (risref || risopt)) {
      e = try_insert_automagic_de(mod, right);
      EXCEPT(e);
      right = subs_last(node);
    }
    if (!lisnil && lisopt && !risopt) {
      e = wrap_arg_unary_op(&right, mod, TPREQMARK);
      EXCEPT(e);
      right = subs_last(node);
    }

    e = unify_refcompat(mod, node, left->typ, right->typ);
    EXCEPT(e);

    left->flags |= right->flags & NODE__ASSIGN_TRANSITIVE;
  } else if (operator == TEQMATCH || operator == TNEMATCH) {
    // noop
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
      e = typ_check_isa(mod, node, left->typ,
                        operator == TMODULO_ASSIGN ? TBI_INTEGER_ARITHMETIC : TBI_ARITHMETIC);
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
  case OP_BIN_SYM_OVARITH:
    switch (operator) {
    case TOVPLUS_ASSIGN:
    case TOVMINUS_ASSIGN:
    case TOVTIMES_ASSIGN:
    case TOVDIVIDE_ASSIGN:
    case TOVMODULO_ASSIGN:
    case TOVLSHIFT_ASSIGN:
      e = typ_check_isa(mod, node, left->typ, TBI_OVERFLOW_ARITHMETIC);
      EXCEPT(e);

      set_typ(&node->typ, TBI_VOID);
      left->flags |= right->flags & NODE__TRANSITIVE;
      break;
    default:
      set_typ(&node->typ, create_tentative(mod, node, TBI_OVERFLOW_ARITHMETIC));
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
      e = typ_check_isa(mod, node, left->typ, TBI_HAS_BITWISE_OPERATORS);
      EXCEPT(e);

      set_typ(&node->typ, TBI_VOID);
      left->flags |= right->flags & NODE__TRANSITIVE;
      break;
    default:
      set_typ(&node->typ, create_tentative(mod, node, TBI_HAS_BITWISE_OPERATORS));
      e = unify(mod, node, node->typ, left->typ);
      EXCEPT(e);
      break;
    }
    break;
  case OP_BIN_SYM_INTARITH:
    switch (operator) {
    case TMODULO_ASSIGN:
    case TRSHIFT_ASSIGN:
      e = typ_check_isa(mod, node, left->typ, TBI_INTEGER_ARITHMETIC);
      EXCEPT(e);

      set_typ(&node->typ, TBI_VOID);
      left->flags |= right->flags & NODE__TRANSITIVE;
      break;
    default:
      set_typ(&node->typ, create_tentative(mod, node, TBI_INTEGER_ARITHMETIC));
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
      if (operator == TEQ || operator == TNE) {
        e = typ_check_isa(mod, left, left->typ, TBI_HAS_EQUALITY);
        EXCEPT(e);
      } else {
        e = typ_check_isa(mod, left, left->typ, TBI_ORDERED);
        EXCEPT(e);
      }

      set_typ(&node->typ, TBI_BOOL);
      break;
    case TEQMATCH:
    case TNEMATCH:
      {
        if (typ_definition_which(left->typ) != DEFTYPE
            || typ_definition_deftype_kind(left->typ) != DEFTYPE_UNION) {
          e = mk_except_type(mod, left, "match operator must be used on a union,"
                             " not on type '%s'", pptyp(mod, left->typ));
          THROW(e);
        }
        if (typ_member(left->typ, node_ident(right)) == NULL) {
          e = mk_except_type(mod, right, "unknown ident '%s' in match"
                             " operator pattern",
                             idents_value(mod->gctx, node_ident(right)));
          THROW(e);
        }
      }
      e = unify(mod, right, right->typ, left->typ);
      EXCEPT(e);
      set_typ(&node->typ, TBI_BOOL);
      break;
    default:
      set_typ(&node->typ, TBI_VOID);
      break;
    }
    break;
  case OP_BIN:
    set_typ(&node->typ, TBI_VOID);
    break;
  default:
    assert(false);
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
     G_IDENT(filen, "File");
     G(file, STRING);
     G_IDENT(linen, "Line");
     G(line, NUMBER);
     G_IDENT(coln, "Col");
     G(col, NUMBER);
     G_IDENT(exprn, "Expr");
     G(expr, STRING));

  const char *fn = module_component_filename_at(mod, node->codeloc.pos);
  char *vfn = calloc(strlen(fn) + 3, sizeof(char));
  sprintf(vfn, "\"%s\"", fn);
  file->as.STRING.value = vfn;

  char *vl = calloc(16, sizeof(char));
  snprintf(vl, 16, "%d", node->codeloc.line);
  line->as.NUMBER.value = vl;

  char *vc = calloc(16, sizeof(char));
  snprintf(vc, 16, "%d", node->codeloc.column);
  col->as.NUMBER.value = vc;

  expr->as.STRING.value = quote_code(mod->parser.data, node->codeloc.pos,
                                     codeloc_pos_after(mod, node));
}

static void insert_missing_optional_arg(struct module *mod, struct node *node,
                                        struct node *after_this, ident name) {
  assert(name != idents_add_string(mod->gctx, "v", 1));
  GSTART();
  G0(named, node, CALLNAMEDARG,
     named->as.CALLNAMEDARG.name = name;
     G(nil, NIL));
  node_subs_remove(node, named);
  node_subs_insert_after(node, after_this, named);

  try_filling_codeloc(mod, named, node);

  error e = catchup(mod, NULL, named, CATCHUP_BELOW_CURRENT);
  assert(!e);
}

static ERROR fill_in_optional_args(struct module *mod, struct node *node,
                                   const struct typ *tfun) {
  const size_t tmin = typ_function_min_arity(tfun);
  const size_t tmax = typ_function_max_arity(tfun);

  if (tmin == tmax) {
    return 0;
  }

  error e;
  struct node *arg = next(subs_first(node));
  ssize_t n = 0, code_pos = 0;
  for (; n < tmin; ++n, ++code_pos) {
    if (arg == NULL) {
      e = mk_except(mod, arg, "missing positional argument '%s' at position %zd",
                    idents_value(mod->gctx, node_ident(arg)), code_pos);
      THROW(e);
    } else if (arg->which == CALLNAMEDARG && !arg->as.CALLNAMEDARG.is_slice_vararg) {
      if (node_ident(arg) != typ_function_arg_ident(tfun, n)) {
        e = mk_except(mod, arg, "named argument '%s' has bad name"
                      " or appears out of order at position %zd",
                      idents_value(mod->gctx, node_ident(arg)), code_pos);
        THROW(e);
      }
    }

    arg = next(arg);
  }

  const ssize_t first_vararg = typ_function_first_vararg(tfun);
  for (n = tmin; n < tmax && (first_vararg == - 1 || n < first_vararg); ++n) {
    const ident targ_name = typ_function_arg_ident(tfun, n);

    if (arg == NULL) {
      insert_missing_optional_arg(mod, node, subs_last(node), targ_name);

    } else if (arg->which != CALLNAMEDARG || arg->as.CALLNAMEDARG.is_slice_vararg) {
      // Assume this is the first vararg

      if (first_vararg == -1) {
        e = mk_except(mod, arg, "excessive positional argument"
                      " or optional argument lacks a name at position %zd", code_pos);
        THROW(e);
      }

      insert_missing_optional_arg(mod, node, prev(arg), targ_name);

    } else if (arg->which == CALLNAMEDARG && !arg->as.CALLNAMEDARG.is_slice_vararg) {
      const ident name = node_ident(arg);

      while (typ_function_arg_ident(tfun, n) != name) {
        insert_missing_optional_arg(mod, node, prev(arg),
                                    typ_function_arg_ident(tfun, n));

        n += 1;
        if ((first_vararg != -1 && n >= first_vararg) || n == tmax) {
          e = mk_except(mod, arg, "named argument '%s' has bad name"
                        " or appears out of order at position %zd",
                        idents_value(mod->gctx, name), code_pos);
          THROW(e);
        }
      }

      arg = next(arg);
      code_pos += 1;
    }
  }

  assert(arg == NULL || first_vararg >= 0);
  while (arg != NULL) {
    if (arg->which == CALLNAMEDARG && !arg->as.CALLNAMEDARG.is_slice_vararg) {
      const ident name = node_ident(arg);
      e = mk_except(mod, arg, "excess named argument '%s'"
                    " or appears out of order at position %zd",
                    idents_value(mod->gctx, name), code_pos);
      THROW(e);
    } else if (arg->which == CALLNAMEDARG && arg->as.CALLNAMEDARG.is_slice_vararg) {
      if (next(arg) != NULL) {
        e = mk_except(mod, next(arg), "excess argument appearing after a"
                      " slice or Vararg is passed as vararg,"
                      " at position %zd", code_pos);
        THROW(e);
      }
      break;
    }

    arg = next(arg);
    code_pos += 1;
  }

  return 0;
}

static ERROR rewrite_unary_call(struct module *mod, struct node *node, struct typ *tfun) {
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

static ERROR type_inference_bin_accessor(struct module *mod, struct node *node) {
  const struct typ *mark = node->typ;

  enum token_type rop = 0;
  struct typ *rfunctor = NULL;
  error e = try_wildcard_op(&rfunctor, &rop, mod, node);
  EXCEPT(e);
  node->as.BIN.operator = rop;

  bool container_is_tentative = false;
  struct tit *field = typ_resolve_accessor__has_effect(&e, &container_is_tentative,
                                                       mod, node);
  // e handled below.

  if (container_is_tentative && e == EINVAL) {
    assert(field == NULL);
    struct node *left = subs_first(node);
    struct node *name = subs_last(node);

    struct node *dinc = defincomplete_create(mod, node);
    defincomplete_add_field(mod, node, dinc, node_ident(name),
                            create_tentative(mod, node, TBI_ANY));
    e = defincomplete_catchup(mod, dinc);
    EXCEPT(e);

    e = unify(mod, node, left->typ, dinc->typ);
    EXCEPT(e);

    field = typ_definition_one_member(dinc->typ, node_ident(name));
  } else {
    EXCEPT(e);
  }

  struct typ *tfield = tit_typ(field);

  if (typ_is_function(tfield) && mark != TBI__CALL_FUNCTION_SLOT) {
    const bool is_method = typ_definition_which(tfield) == DEFMETHOD;
    if (typ_function_min_arity(tfield) != (is_method ? 1 : 0)) {
      e = mk_except_call_args_count(mod, node, tfield, is_method, 0);
      THROW(e);
    }

    e = rewrite_unary_call(mod, node, tfield);
    EXCEPT(e);
  } else {
    set_typ(&node->typ, tfield);
    node->flags = tit_node_flags(field);
  }

  if (!(node->flags & NODE_IS_TYPE)) {
    if (!(node->flags & NODE_IS_TEMPORARY)
        && !(subs_first(node)->flags & NODE_IS_DEFCHOICE)) {
      e = typ_check_deref_against_mark(mod, node, mark, rop);
      EXCEPT(e);
    }
  }

  tit_next(field);
  return 0;
}

static ERROR type_inference_bin_rhs_unsigned(struct module *mod, struct node *node) {
  error e;
  struct node *left = subs_first(node);
  struct node *right = subs_last(node);

  e = try_insert_automagic_de(mod, right);
  EXCEPT(e);
  right = subs_last(node);

  e = unify(mod, right, right->typ, TBI_UINT);
  EXCEPT(e);

  switch (node->as.BIN.operator) {
  case TRSHIFT:
    set_typ(&node->typ, create_tentative(mod, node, TBI_INTEGER_ARITHMETIC));
    e = unify(mod, node, left->typ, node->typ);
    EXCEPT(e);
    break;
  case TOVLSHIFT:
    set_typ(&node->typ, create_tentative(mod, node, TBI_OVERFLOW_ARITHMETIC));
    e = unify(mod, node, left->typ, node->typ);
    EXCEPT(e);
    break;
  case TRSHIFT_ASSIGN:
    e = unify(mod, node, left->typ, create_tentative(mod, node, TBI_INTEGER_ARITHMETIC));
    EXCEPT(e);
    set_typ(&node->typ, TBI_VOID);
    break;
  case TOVLSHIFT_ASSIGN:
    e = unify(mod, node, left->typ, create_tentative(mod, node, TBI_OVERFLOW_ARITHMETIC));
    EXCEPT(e);
    set_typ(&node->typ, TBI_VOID);
    break;
  default:
    assert(false);
    break;
  }

  return 0;
}

static ERROR type_inference_bin_isa(struct module *mod, struct node *node) {
  error e;
  struct node *left = subs_first(node);
  struct node *right = subs_last(node);
  if (typ_is_dyn(left->typ)) {
    set_typ(&node->typ, TBI_BOOL);
    return 0;
  }

  node_subs_remove(node, left);
  node_subs_remove(node, right);

  if (!(node->flags & NODE_IS_TYPE)) {
    if (left->which == IDENT && left->as.IDENT.def->which == DEFNAME) {
      left->as.IDENT.def->as.DEFNAME.may_be_unused = true;
    } else {
      // By SSA, left is side-effect free. If it's not an IDENT (e.g. a
      // field bin acc), we can ignore it.
    }
  }

  const bool isa = typ_isa(left->typ, right->typ);

  if (parent(node)->which == BLOCK && nparent(node, 2)->which == IF) {
    // ssa.c does not replace the result of 'isa' in this case:
    //   if a isa b
    // to allow for implementation selection in generics.
    struct node *par = parent(node);
    struct node *xif = parent(par);
    assert(subs_first(xif) == par && "We should be in the condition block");

    struct node *to_disable = subs_at(xif, isa ? 2 : 1);
    assert(to_disable->which == BLOCK);

    struct node *s = subs_first(to_disable);
    while (s != NULL) {
      struct node *nxt = next(s);
      node_subs_remove(to_disable, s);
      s = nxt;
    }

    (void) mk_node(mod, to_disable, INIT);
  }

  node_set_which(node, BOOL);
  node->as.BOOL.value = isa;

  e = catchup(mod, NULL, node, CATCHUP_REWRITING_CURRENT);
  EXCEPT(e);

  return 0;
}

static ERROR type_inference_bin_rhs_type(struct module *mod, struct node *node) {
  error e;
  struct node *left = subs_first(node);
  struct node *right = subs_last(node);

  if (!(right->flags & NODE_IS_TYPE)) {
    e = mk_except_type(mod, right, "right-hand side not a type");
    THROW(e);
  }

  if (node->as.BIN.operator == Tisa) {
    e = type_inference_bin_isa(mod, node);
    EXCEPT(e);
  } else {
    e = unify(mod, node, left->typ, right->typ);
    EXCEPT(e);

    set_typ(&node->typ, left->typ);
  }

  return 0;
}

static ERROR type_inference_bin(struct module *mod, struct node *node) {
  assert(node->which == BIN);

  error e;
  switch (OP_KIND(node->as.BIN.operator)) {
  case OP_BIN_SYM:
  case OP_BIN_SYM_BOOL:
  case OP_BIN_SYM_ARITH:
  case OP_BIN_SYM_INTARITH:
  case OP_BIN_SYM_OVARITH:
  case OP_BIN_SYM_BW:
  case OP_BIN_SYM_PTR:
    e = check_terms_not_types(mod, node);
    EXCEPT(e);
    return type_inference_bin_sym(mod, node);
  case OP_BIN_BW_RHS_UNSIGNED:
  case OP_BIN_OVBW_RHS_UNSIGNED:
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

static ERROR typ_tuple(struct typ **result, struct module *mod, struct node *node) {
  const size_t arity = subs_count(node);
  struct typ **args = calloc(arity, sizeof(struct typ *));
  size_t n = 0;
  FOREACH_SUB(s, node) {
    args[n] = s->typ;
    n += 1;
  }

  error e = instantiate(result, mod, node, 0,
                        typ_lookup_builtin_tuple(mod, arity), args, arity, false);
  EXCEPT(e);

  free(args);

  return 0;
}

static ERROR type_inference_tuple(struct module *mod, struct node *node) {
  size_t n = 0;
  FOREACH_SUB(s, node) {
    if (n > 0 && (node->flags & NODE_IS_TYPE) != (s->flags & NODE_IS_TYPE)) {
      error e = mk_except_type(mod, s, "tuple combines values and types");
      THROW(e);
    }
    node->flags |= (s->flags & NODE__TRANSITIVE);
    n += 1;
  }

  struct typ *i = NULL;
  error e = typ_tuple(&i, mod, node);
  EXCEPT(e);

  set_typ(&node->typ, i);

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
    e = unify(mod, node, node->typ, TBI_RANGE);
    assert(!e);
  } else if (node->as.INIT.is_bounds) {
    e = unify(mod, node, node->typ, TBI_BOUNDS);
    assert(!e);
  }
}

static ERROR type_inference_init_array(struct module *mod, struct node *node) {
  set_typ(&node->typ, create_tentative(mod, node, TBI_LITERALS_SLICE));

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

static ERROR type_inference_init(struct module *mod, struct node *node) {
  assert(node->which == INIT);
  if (node->as.INIT.is_array) {
    if (!typ_is_literal(subs_first(node)->typ)
        && typ_definition_which(subs_first(node)->typ) == DEFINTF) {
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

static ERROR type_inference_return(struct module *mod, struct node *node) {
  assert(node->which == RETURN);

  if (subs_count_atleast(node, 1)) {
    struct typ *ret = module_retval_get(mod)->typ;
    struct node *arg = subs_first(node);

    if (typ_is_optional(ret) && !typ_is_optional(arg->typ)) {
      error e = wrap_arg_unary_op(&arg, mod, TPREQMARK);
      EXCEPT(e);
    } else if (!typ_is_optional(ret) && typ_is_optional(arg->typ)) {
      error e = wrap_arg_unary_op(&arg, mod, T__DEOPT);
      EXCEPT(e);
    }

    error e = unify_refcompat(mod, arg, ret, arg->typ);
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

static ERROR rewrite_self(struct module *mod, struct node *node,
                          struct node *old_fun) {
  assert(old_fun->which == BIN);

  struct node *old_self = subs_first(old_fun);
  struct node *self;
  if (typ_is_reference(old_self->typ)) {
    node_subs_remove(old_fun, old_self);
    node_subs_insert_after(node, subs_first(node), old_self);
    self = old_self;
  } else {
    node_subs_remove(old_fun, old_self);
    enum token_type access = refop_for_accop[old_fun->as.BIN.operator];
    struct node *s = expr_ref(mod, node, access, old_self);
    node_subs_remove(node, s);
    node_subs_insert_after(node, subs_first(node), s);
    self = s;
  }

  const struct node *except[] = { old_self, NULL };
  error e = catchup(mod, except, self, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  if (typ_is_reference(self->typ)) {
    e = typ_check_can_deref(mod, old_fun, self->typ,
                            derefop_for_accop[old_fun->as.BIN.operator]);
    EXCEPT(e);
  }

  return 0;
}

static bool compare_ref_depth(const struct typ *target, const struct typ *arg,
                              int diff) {
  assert(!typ_equal(target, TBI_ANY_ANY_REF));

  if (typ_equal(arg, TBI_LITERALS_NIL)) {
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

    if (typ_equal(arg, TBI_LITERALS_NIL)) {
      return diff != 1;
    }
  }

  return dtarget == darg + diff;
}

static ERROR try_insert_automagic_arg_ref(struct module *mod, struct node *node,
                                          const struct typ *target,
                                          enum token_type target_explicit_ref,
                                          struct node *arg) {
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
      if (expr_arg != NULL
          && expr_arg->which == UN
          && expr_arg->as.UN.operator == TREFDOT
          && expr_arg->as.UN.is_explicit
          && !typ_equal(subs_first(expr_arg)->typ, TBI_LITERALS_NIL)) {
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

static ERROR try_insert_automagic_arg(struct module *mod, struct node *node,
                                      const struct typ *target,
                                      enum token_type target_explicit_ref,
                                      struct node *arg) {
  if (typ_equal(arg->typ, TBI_LITERALS_NIL)) {
    return 0;
  }

  if (typ_is_reference(target)) {
    error e = try_insert_automagic_arg_ref(mod, node, target, target_explicit_ref, arg);
    EXCEPT(e);
  } else if (typ_is_optional(target)) {
    struct node *r = arg;
    error e = wrap_arg_unary_op(&r, mod, TPREQMARK);
    EXCEPT(e);
  }
  return 0;
}

static ERROR try_insert_automagic_arg_de(struct module *mod,
                                         const struct typ *target,
                                         enum token_type target_explicit_ref,
                                         struct node *arg) {
  if (target_explicit_ref != 0 || typ_is_reference(target)) {
    return 0;
  }

  // target can be either t or ?t
  // arg can be any of: ?t, @t, @?t
  // Don't deref more than once.
  struct node *r = arg;
  if (!typ_is_optional(target) && typ_is_optional(r->typ)) {
    error e = wrap_arg_unary_op(&r, mod, T__DEOPT);
    EXCEPT(e);
    arg = r;
  }
  if (!typ_is_reference(target) && typ_is_reference(r->typ)) {
    error e = wrap_arg_unary_op(&r, mod, TDEREFDOT);
    EXCEPT(e);
    arg = r;
  }
  if (!typ_is_optional(target) && typ_is_optional(r->typ)) {
    error e = wrap_arg_unary_op(&r, mod, T__DEOPT);
    EXCEPT(e);
    arg = r;
  }
  return 0;
}

static ERROR process_automagic_call_arguments(struct module *mod,
                                              struct node *node,
                                              const struct typ *tfun) {
  if (!subs_count_atleast(node, 2)) {
    return 0;
  }

  const ssize_t first_vararg = typ_function_first_vararg(tfun);

  error e;
  ssize_t n = 0;
  struct node *last = NULL;
  struct node *nxt = subs_at(node, 1);
  while (nxt != NULL) {
    if (n == first_vararg) {
      break;
    }

    // We record 'nxt' now as try_insert_automagic_arg{,_de}() may move 'arg'.
    struct node *arg = nxt;
    nxt = next(nxt);

    const enum token_type explicit_ref = typ_function_arg_explicit_ref(tfun, n);
    e = try_insert_automagic_arg(mod, node,
                                 typ_function_arg_const(tfun, n),
                                 explicit_ref, arg);
    EXCEPT(e);

    e = try_insert_automagic_arg_de(mod, typ_function_arg_const(tfun, n),
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
      // We record 'nxt' now as try_insert_automagic_arg() may move 'arg'.
      struct node *arg = nxt;
      nxt = next(nxt);

      e = try_insert_automagic_arg(mod, node, target, TREFDOT, arg);
      EXCEPT(e);
    }
  }

  return 0;
}

static ERROR prepare_call_arguments(struct module *mod, struct node *node) {
  error e;
  struct node *fun = subs_first(node);
  struct typ *tfun = fun->typ;
  const size_t tmin = typ_function_min_arity(tfun);
  const size_t tmax = typ_function_max_arity(tfun);

  const size_t args = subs_count(node) - 1;

  switch (typ_definition_which(tfun)) {
  case DEFFUN:
    if (args < tmin || args > tmax) {
      e = mk_except_call_args_count(mod, node, tfun, false, args);
      THROW(e);
    }
    break;
  case DEFMETHOD:
    if (fun->which == BIN) {
      if ((subs_first(fun)->flags & NODE_IS_TYPE)) {
        // Form (type.method self ...).
        if (args < tmin || args > tmax) {
          e = mk_except_call_args_count(mod, node, tfun, false, args);
          THROW(e);
        }
      } else {
        // Form (self.method ...); rewrite as (type.method self ...).
        if (args+1 < tmin || args+1 > tmax) {
          e = mk_except_call_args_count(mod, node, tfun, true, args);
          THROW(e);
        }

        struct node *m = mk_node(mod, node, DIRECTDEF);
        set_typ(&m->as.DIRECTDEF.typ, tfun);
        m->as.DIRECTDEF.flags = NODE_IS_TYPE;
        node_subs_remove(node, m);
        node_subs_replace(node, fun, m);
        e = catchup(mod, NULL, m, CATCHUP_BELOW_CURRENT);
        EXCEPT(e);

        e = rewrite_self(mod, node, fun);
        EXCEPT(e);

        e = catchup(mod, NULL, m, CATCHUP_BELOW_CURRENT);
        EXCEPT(e);
      }
    } else if ((fun->flags & NODE_IS_TYPE)) {
      assert(fun->which == CALL || fun->which == DIRECTDEF);
      // Generic method instantiation: (type.method u32 i32) self
      // or DIRECTDEF.
      if (args < tmin || args > tmax) {
        e = mk_except_call_args_count(mod, node, tfun, false, args);
        THROW(e);
      }
    } else {
      assert(false && "Unreached");
    }
    break;
  default:
    assert(false);
  }

  e = fill_in_optional_args(mod, node, tfun);
  EXCEPT(e);

  e = process_automagic_call_arguments(mod, node, tfun);
  EXCEPT(e);

  return 0;
}

static ERROR explicit_instantiation(struct module *mod, struct node *node) {
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
    t = typ_create_tentative_functor(mod, t);
  }

  const size_t given_arity = subs_count(node) - 1;

  const size_t arity = typ_generic_arity(t);
  const size_t first_explicit = typ_generic_first_explicit_arg(t);
  const size_t explicit_arity = arity - first_explicit;
  if (given_arity > explicit_arity) {
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
  // Ommitted generic arguments.
  for (; n < arity; ++n) {
    args[n] = tentative_generic_arg(mod, node, t, n);
  }

  struct typ *i = NULL;
  e = instantiate(&i, mod, node, 1, t, args, arity, false);
  EXCEPT(e);

  free(args);

  set_typ(&node->typ, i);
  topdeps_record(mod, i);
  node->flags |= NODE_IS_TYPE;

  return 0;
}

static ERROR implicit_function_instantiation(struct module *mod, struct node *node) {
  error e;
  struct node *fun = subs_first(node);
  struct typ *tfun = fun->typ;
  const size_t arity = subs_count(node) - 1;

  // Already checked in prepare_call_arguments().
  assert(arity == typ_function_arity(tfun));

  struct typ **i = typ_permanent_loc(instantiate_fully_implicit(mod, node, tfun));

  size_t n = 0;
  FOREACH_SUB_EVERY(s, node, 1, 1) {
    e = unify_refcompat(mod, s, typ_function_arg(*i, n), s->typ);
    EXCEPT(e);

    if (n == 0 /* self */
        && typ_definition_which(*i) == DEFMETHOD
        && typ_definition_defmethod_access(*i) == TREFWILDCARD) {
      // This code enforces the relationship between the 2 wildcard generic
      // arguments, for wildcard methods.

      struct typ *self_wildcard = typ_definition_defmethod_self_wildcard_functor(*i);
      struct typ *wildcard = typ_definition_defmethod_wildcard_functor(*i);
      if ((typ_toplevel_flags(*i) & TOP_IS_SHALLOW)
          && typ_equal(typ_generic_functor(s->typ), TBI_MREF)) {
        e = unify(mod, s, wildcard, TBI_MMREF);
        EXCEPT(e);
      } else {
        assert(typ_is_reference(s->typ));

        e = unify(mod, s, wildcard, self_wildcard);
        EXCEPT(e);
      }
    }

    n += 1;
  }

  // We're turning 'fun' into something different: it was a
  // functor, now it's an instantiated function. We are, in essence,
  // removing the node and replacing with a different one. We don't actually
  // bother to do that, but we need to be careful:
  unset_typ(&fun->typ);
  set_typ(&fun->typ, *i);
  if (fun->which == DIRECTDEF) {
    unset_typ(&fun->as.DIRECTDEF.typ);
    set_typ(&fun->as.DIRECTDEF.typ, *i);
  }

  topdeps_record(mod, *i);
  set_typ(&node->typ, typ_function_return(*i));

  return 0;
}

static ERROR function_instantiation(struct module *mod, struct node *node) {
  assert(subs_count_atleast(node, 2));

  error e = implicit_function_instantiation(mod, node);
  EXCEPT(e);

  return 0;
}

static ERROR check_consistent_either_types_or_values(struct module *mod,
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

static ERROR type_inference_explicit_unary_call(struct module *mod, struct node *node,
                                                struct typ *tfun) {
  const size_t count = subs_count(node);
  if (typ_definition_which(tfun) == DEFFUN && count != 1) {
    error e = mk_except_call_args_count(mod, node, tfun, false, count - 1);
    THROW(e);
  } else if (typ_definition_which(tfun)== DEFMETHOD && count != 2) {
    error e = mk_except_call_args_count(mod, node, tfun, false, count - 1);
    THROW(e);
  }

  if (typ_definition_which(tfun) == DEFMETHOD) {
    struct node *self = subs_at(node, 1);
    error e = unify(mod, self, typ_function_arg(tfun, 0), self->typ);
    EXCEPT(e);
  }

  set_typ(&node->typ, typ_function_return(tfun));

  return 0;
}

static ERROR try_rewrite_operator_sub_bounds(struct module *mod, struct node *node) {
  if (!subs_count_atleast(node, 3)) {
    return 0;
  }

  struct node *fun = subs_first(node);
  struct node *self = subs_at(node, 1);
  struct node *arg = subs_at(node, 2);

  if (!typ_equal(arg->typ, TBI_BOUNDS)
      || typ_definition_ident(fun->typ) != ID_OPERATOR_SUB) {
    return 0;
  }

  assert(typ_is_reference(self->typ));
  struct tit *m = typ_definition_one_member(typ_generic_arg(self->typ, 0),
                                            ID_OPERATOR_SUB);
  if (m == NULL) {
    error e = mk_except_type(mod, node, "type '%s' does not have '%s'",
                             pptyp(mod, self->typ),
                             idents_value(mod->gctx, ID_OPERATOR_SUB));
    THROW(e);
  }

  unset_typ(&fun->typ);
  unset_typ(&fun->as.DIRECTDEF.typ);
  set_typ(&fun->as.DIRECTDEF.typ, tit_typ(m));
  tit_next(m);

  error e = catchup(mod, NULL, fun, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

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
  e = catchup(mod, except, call, CATCHUP_BELOW_CURRENT);
  EXCEPT(e);

  return 0;
}

static bool is_vararg_passdown(struct typ *arg) {
  if (!typ_is_reference(arg)) {
    return false;
  }
  struct typ *va = typ_generic_arg(arg, 0);
  return typ_generic_arity(va) > 0
    && typ_equal(typ_generic_functor(va), TBI_VARARG);
}

static ERROR type_inference_call(struct module *mod, struct node *node) {
  error e;
  struct node *fun = subs_first(node);

  if (!typ_is_function(fun->typ)
      || (subs_count_atleast(node, 2) && (subs_at(node, 1)->flags & NODE_IS_TYPE))) {
    // Uninstantiated generic, called on types.

    if (!typ_is_function(fun->typ) && typ_generic_arity(fun->typ) == 0) {
      char *n = pptyp(mod, fun->typ);
      e = mk_except_type(mod, fun, "'%s' not a function or a generic", n);
      free(n);
      THROW(e);
    }

    e = explicit_instantiation(mod, node);
    EXCEPT(e);

    if (typ_is_function(node->typ) && typ_function_min_arity(node->typ) == 0
        && parent_const(node)->which != CALL) {
      // Combined explicit instantiation and unary call:
      //   let p = Alloc I32
      struct node *moved = node_new_subnode(mod, node);
      node_subs_remove(node, moved);
      node_move_content(moved, node);
      node_subs_append(node, moved);
      node->which = CALL;

      e = type_inference_call(mod, node);
      EXCEPT(e);
    }

    return 0;
  }

  e = prepare_call_arguments(mod, node);
  EXCEPT(e);

  // Assumes that operator_at and operator_sub have the same arguments.
  e = try_rewrite_operator_sub_bounds(mod, node);
  EXCEPT(e);

  e = check_consistent_either_types_or_values(mod, try_node_subs_at(node, 1));
  EXCEPT(e);

  node->flags |= NODE_IS_TEMPORARY;

  if (typ_is_generic_functor(fun->typ)) {
    // Uninstantiated generic, called on values.

    e = function_instantiation(mod, node);
    EXCEPT(e);

    return 0;
  }

  if (typ_function_max_arity(fun->typ) == 0) {
    return type_inference_explicit_unary_call(mod, node, fun->typ);
  }

  const ssize_t first_vararg = typ_function_first_vararg(fun->typ);
  ssize_t n = 0;
  FOREACH_SUB_EVERY(arg, node, 1, 1) {
    if (n == first_vararg) {
      break;
    }
    e = unify_refcompat(mod, arg, typ_function_arg(fun->typ, n), arg->typ);
    EXCEPT(e);
    n += 1;
  }

  if (n == first_vararg) {
    FOREACH_SUB_EVERY(arg, node, 1 + n, 1) {
      struct typ *target = typ_generic_arg(typ_function_arg(fun->typ, n), 0);

      if (arg->which == CALLNAMEDARG && arg->as.CALLNAMEDARG.is_slice_vararg) {
        if (is_vararg_passdown(arg->typ)) {
          e = unify_refcompat(mod, arg, target, typ_generic_arg(typ_generic_arg(arg->typ, 0), 0));
          EXCEPT(e);
        } else {
          struct typ *target_arg = typ_generic_arg(target, 0);
          assert(typ_definition_which(target_arg) != DEFINTF && "Unsupported: needs dyn slice support");

          struct typ *slice_target = NULL;
          e = instantiate(&slice_target, mod, arg, -1, TBI_SLICE, &target_arg, 1, false);
          EXCEPT(e);

          e = unify_refcompat(mod, arg, slice_target, typ_generic_arg(arg->typ, 0));
          EXCEPT(e);
        }
        break;
      }

      e = unify_refcompat(mod, arg, target, arg->typ);
      EXCEPT(e);
    }
  }

  set_typ(&node->typ, typ_function_return(fun->typ));

  return 0;
}

static ERROR type_inference_block(struct module *mod, struct node *node) {
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
                         pptyp(mod, s->typ));
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

static ERROR type_inference_if(struct module *mod, struct node *node) {
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

static ERROR unify_match_pattern(struct module *mod, struct node *expr, struct node *pattern) {
  error e = unify(mod, pattern, pattern->typ, expr->typ);
  EXCEPT(e);

  return 0;
}

static ERROR type_inference_match(struct module *mod, struct node *node) {
  error e;

  struct node *expr = subs_first(node);
  e = try_insert_automagic_de(mod, expr);
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

static ERROR type_inference_ident_unknown(struct module *mod, struct node *node) {
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
  // in step_ident_non_local_scope().
  node->as.IDENT.non_local_scope = &unk->scope;

  set_typ(&node->typ, unk->typ);
  return 0;
}

static ERROR type_inference_ident(struct module *mod, struct node *node) {
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
  } else if (def->which == WITHIN) {
    node->as.IDENT.non_local_scope = &def->as.WITHIN.globalenv_scope->scope;
  }

  if (typ_is_function(def->typ) && node->typ != TBI__CALL_FUNCTION_SLOT) {
    if (typ_function_min_arity(def->typ) != 0) {
      e = mk_except_call_args_count(mod, node, def->typ, false, 0);
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

static ERROR type_inference_within(struct module *mod, struct node *node) {
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

      modbody = typ_definition_nooverlay_const(ffirst->typ);

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
                                     &modbody->as.MODULE_BODY.globalenv_scope->scope,
                                     node_ident(subs_last_const(node)), false);
    if (e) {
      e = mk_except(mod, node, "in within declaration");
      THROW(e);
    }

    node->as.WITHIN.globalenv_scope = modbody->as.MODULE_BODY.globalenv_scope;
  } else if (node->which == IDENT) {
    e = scope_lookup(&def, mod, &node->scope, node, false);
    EXCEPT(e);
  } else if (node->which == BIN) {
    e = type_inference_within(mod, first);
    EXCEPT(e);

    e = scope_lookup_ident_immediate(&def, node,
                                     mod, &typ_definition_nooverlay(first->typ)->scope,
                                     node_ident(subs_last_const(node)), false);
    EXCEPT(e);
  } else {
    goto malformed;
  }

  set_typ(&node->typ, def->typ);
  node->flags |= def->flags;
  return 0;

malformed:
  e = mk_except(mod, node, "malformed within expression");
  THROW(e);
}

static ERROR type_inference_try(struct module *mod, struct node *node) {
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

static ERROR type_inference_typeconstraint_defchoice_init(struct module *mod,
                                                          struct node *node) {
  error e;
  struct node *left = subs_first(node);
  struct node *right = subs_last(node);

  assert(left->which == INIT);
  assert(typ_definition_which(left->typ) == DEFINCOMPLETE);

  assert(right->flags & NODE_IS_DEFCHOICE);
  assert(right->which == BIN);
  struct tit *leaf = typ_definition_one_member(subs_first(right)->typ, node_ident(subs_last(right)));
  assert(tit_which(leaf) == DEFCHOICE);
  if (!tit_defchoice_is_leaf(leaf)) {
    e = mk_except_type(mod, subs_last(right),
                       "only union leaf variants can be initialized");
    THROW(e);
  }

  // With an initializer, 'un.A' is of type 'un', unlike for an accessor
  // (with external payload). So here we fix the typing of 'right' as bin
  // acc.
  unset_typ(&right->typ);
  set_typ(&right->typ, subs_first(right)->typ);

  left->as.INIT.for_tag = tit_ident(leaf);

  FOREACH_SUB_EVERY(name, left, 0, 2) {
    struct tit *f = tit_defchoice_lookup_field(leaf, node_ident(name));
    if (f == NULL) {
      e = mk_except_type(mod, name, "field '%s' not found",
                         idents_value(mod->gctx, node_ident(name)));
      THROW(e);
    }

    typ_link_tentative(tit_typ(f), next(name)->typ);
    tit_next(f);
  }

  tit_next(leaf);
  return 0;
}

static ERROR type_inference_typeconstraint_defchoice_convert(struct module *mod,
                                                             struct node *node) {
  error e;
  struct node *left = subs_first(node);
  struct node *right = subs_last(node);

  assert(right->which == BIN);
  struct tit *leaf = typ_definition_one_member(subs_first(right)->typ, node_ident(subs_last(right)));
  assert(tit_which(leaf) == DEFCHOICE);
  if (!tit_defchoice_is_leaf(leaf)
      || !(right->flags & NODE_IS_DEFCHOICE_HAS_EXTERNAL_PAYLOAD)) {
    e = mk_except_type(mod, subs_last(right),
                       "only union leaf variants with external payload"
                       " can constraint a value");
    THROW(e);
  }

  e = unify(mod, subs_first(node),
            left->typ, right->typ);
  EXCEPT(e);

  node_set_which(node, INIT);
  node->as.INIT.is_defchoice_external_payload_constraint = true;
  node->as.INIT.for_tag = tit_ident(leaf);

  node_subs_remove(node, right);
  set_typ(&node->typ, subs_first(right)->typ);
  node->flags |= subs_last(node)->flags;

  return 0;
}

static ERROR type_inference_typeconstraint(struct module *mod, struct node *node) {
  if (node->as.TYPECONSTRAINT.is_constraint) {
    set_typ(&node->typ, subs_first(node)->typ);
    return 0;
  }

  error e;
  if (subs_first(node)->which == INIT
      && subs_last(node)->flags & NODE_IS_DEFCHOICE) {
    e = type_inference_typeconstraint_defchoice_init(mod, node);
    EXCEPT(e);
  } else if (subs_last(node)->flags & NODE_IS_DEFCHOICE) {
    e = type_inference_typeconstraint_defchoice_convert(mod, node);
    EXCEPT(e);
    return 0;
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

static ERROR check_void_body(struct module *mod, const struct node *node) {
  if (!node_has_tail_block(node)) {
    return 0;
  }
  const struct node *body = subs_last_const(node);
  if (body->typ != NULL && !typ_equal(body->typ, TBI_VOID)) {
    error e = mk_except_type(mod, body,
                             "the body of a function must be a block of type"
                             " Void (use return), not '%s'",
                             pptyp(mod, body->typ));
    THROW(e);
  }
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
  case DEFFUN:
  case DEFMETHOD:
    e = check_void_body(mod, node);
    EXCEPT(e);
    // Fallthrough
  case DEFINTF:
  case DEFTYPE:
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
      || typ_definition_which(node->typ) == MODULE
      || typ_definition_which(node->typ) == ROOT_OF_ALL) {
    // noop
  } else {
    return 0;
  }

  BEGTIMEIT(TIMEIT_TYPE_INFERENCE);
  BEGTIMEIT(TIMEIT_TYPE_INFERENCE_PREBODYPASS);
  BEGTIMEIT(TIMEIT_TYPE_INFERENCE_IN_FUNS_BLOCK);

  switch (node->which) {
  case NIL:
    set_typ(&node->typ, create_tentative(mod, node, TBI_LITERALS_NIL));
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
    if (nparent(node, 2)->which == DEFINTF) {
      set_typ(&node->typ, typ_create_ungenarg(subs_last(node)->typ));
    } else {
      set_typ(&node->typ, subs_last(node)->typ);
    }
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
    set_typ(&node->typ, TBI_BOOL);
    break;
  case STRING:
    set_typ(&node->typ, create_tentative(mod, node, TBI_LITERALS_STRING));
    break;
  case SIZEOF:
    set_typ(&node->typ, TBI_UINT);
    break;
  case ALIGNOF:
    set_typ(&node->typ, TBI_UINT);
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
      if (!typ_is_nullable_reference(node->typ) && !typ_is_optional(node->typ)) {
        e = mk_except_type(mod, node, "optional argument '%s' must be an optional"
                           " value or a nullable reference, not '%s'",
                           idents_value(mod->gctx, node_ident(node)),
                           pptyp(mod, node->typ));
        THROW(e);
      }
    } else if (node->as.DEFARG.is_vararg) {
      if (!typ_has_same_generic_functor(mod, node->typ, TBI_VARARG)) {
        e = mk_except_type(mod, node,
                           "vararg argument '%s' must have type"
                           " (vararg `any_any_ref), not '%s'",
                           idents_value(mod->gctx, node_ident(node)),
                           pptyp(mod, node->typ));
        THROW(e);
      }
    }
    break;
  case DEFGENARG:
    node->typ = typ_create_ungenarg(subs_at(node, 1)->typ);
    node->flags |= NODE_IS_TYPE;
    node_toplevel(nparent(node, 2))->flags |= TOP_IS_FUNCTOR;
    break;
  case SETGENARG:
    set_typ(&node->typ, subs_at(node, 1)->typ);
    node->flags |= NODE_IS_TYPE;
    node_toplevel(nparent(node, 2))->flags &= ~TOP_IS_FUNCTOR;
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
  case ASSERT:
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
  case MODULE_BODY:
    node->typ = TBI__NOT_TYPEABLE;
    break;
  default:
    break;
  }

  if (node->typ != NULL) {
    topdeps_record(mod, node->typ);
  }

  ENDTIMEIT(mod->state->fun_state != NULL && mod->state->fun_state->in_block,
            TIMEIT_TYPE_INFERENCE_IN_FUNS_BLOCK);
  ENDTIMEIT(mod->stage->state->passing < PASSZERO_COUNT + PASSFWD_COUNT,
            TIMEIT_TYPE_INFERENCE_PREBODYPASS);
  ENDTIMEIT(true, TIMEIT_TYPE_INFERENCE);

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

static ERROR finalize_generic_instantiation(struct typ *t) {
  struct module *mod = typ_module_owner(t);

  if (typ_was_zeroed(t)) {
    // 't' was cleared in link_to_final()
    return 0;
  }

  if (typ_generic_arity(t) == 0) {
    // For instance, a DEFINCOMPLETE that unified to a non-generic.
    return 0;
  }

  if (typ_is_pseudo_builtin(t)) {
    return 0;
  }

  struct typ *functor = typ_generic_functor(t);

  if (typ_is_tentative(t)) {
    assert(!typ_is_tentative(functor));

    for (size_t m = 0; m < typ_generic_arity(t); ++m) {
      struct typ *arg = typ_generic_arg(t, m);
      assert(!typ_is_tentative(arg));
    }
  } else {
    // While t is already not tentative anymore, the instantiation is still
    // partial, and needs to be completed (bodypass). It's easier to simply
    // create a final instance from scratch.
  }

  const size_t arity = typ_generic_arity(t);

  struct typ *existing = instances_find_existing_final_like(t);
  if (existing != NULL) {
    typ_link_to_existing_final(existing, t);
    topdeps_record(mod, existing);
    return 0;
  }

  struct typ **args = calloc(arity, sizeof(struct typ *));
  for (size_t m = 0; m < arity; ++m) {
    args[m] = typ_generic_arg(t, m);
  }

  struct typ *i = NULL;
  error e = instantiate(&i, mod, typ_for_error(t), -1, functor, args, arity,
                        true);
  EXCEPT(e);

  assert(!typ_is_tentative(i));
  if (!typ_was_zeroed(t)) {
    // Otherwise it's already linked.
    typ_link_to_existing_final(i, t);
  }

  topdeps_record(mod, i);

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
static void finalize_defincomplete_unification(struct typ *inc) {
  error e = unify_with_defincomplete_entrails(typ_module_owner(inc),
                                              typ_for_error(inc), inc, inc);
  assert(!e);
}

static __thread struct vectyp scheduleq;

// Called from typ_link_*() operations. Linking is triggered by unify*().
// unify*() works directly on typs. When a typ is linked, the source is
// zeroed. In some cases (currently, on link_finalize()), we have to wait
// until after unify*() is done to proceed.
//
// Why only link_finalize()?
// unify*() respects this invariant: during unification, a tentative typ may
// be linked to another tentative typ or final typ; once *explicitly* linked
// to something else (explicitly means passed directly to typ_link_*()) by
// unify*()), tentative typs are not used again by the current unification.
// So there is no need to delay further work, they can be linked and zeroed
// immediately.
//
// link_finalize(), on the other hand, operates on tentative typs that have
// just lost their tentative status but were not directly passed to
// typ_link_*(). They lost their tentative status as a side effect of some
// other typ being linked to something final. The current unification
// invokation has no way of knowing all the typs that may loose their
// tentative status indirectly. If link_finalize() were to do the link
// immediately, unify*() would end up using zeroed typs by mistake. Instead,
// the work is delayed until the current unification is over.
void schedule_finalization(struct typ *t) {
  vectyp_push(&scheduleq, t);
}

error process_finalizations(void) {
  static __thread bool in_progress;
  if (in_progress) {
    return 0;
  }
  in_progress = true;

  // More work may be scheduled as we're iterating.
  for (size_t n = 0; n < vectyp_count(&scheduleq); ++n) {
    struct typ *t = *vectyp_get(&scheduleq, n);
    if (typ_was_zeroed(t)) {
      continue;
    }

    if (typ_definition_which(t) == DEFINCOMPLETE) {
      finalize_defincomplete_unification(t);
    } else {
      error e = finalize_generic_instantiation(t);
      EXCEPT(e);
    }
  }
  vectyp_destroy(&scheduleq);

  in_progress = false;
  return 0;
}
