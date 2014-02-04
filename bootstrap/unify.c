#include "unify.h"

#include "table.h"
#include "types.h"
#include "scope.h"
#include "passes.h"

static error mk_except_type_unification(struct module *mod, const struct node *for_error,
                                        const struct typ *a, const struct typ *b) {
  char *sa = typ_pretty_name(mod, a);
  char *sb = typ_pretty_name(mod, b);
  error e = mk_except_type(mod, for_error,
                           "types '%s' and '%s' cannot be unified", sa, sb);
  free(sb);
  free(sa);
  THROW(e);
}

static error unify_two_non_generic(struct module *mod, const struct node *for_error,
                                   struct typ *a, struct typ *b) {
  if (typ_isa(a, b)) {
    // noop
  } else if (typ_isa(b, a)) {
    SWAP(a, b);
  } else if (typ_definition(a)->which == DEFINTF
             && typ_definition(b)->which == DEFINTF) {
    assert(FALSE && "FIXME Unsupported (e.g. `arithmethic and `bitwise)");
  } else {
    error e = mk_except_type_unification(mod, for_error, a, b);
    THROW(e);
  }

  typ_link_tentative(a, b);

  return 0;
}

static bool same_generic_functor(const struct module *mod,
                                 const struct typ *a, const struct typ *b) {
  if (typ_generic_arity(a) > 0 && typ_generic_arity(b) > 0) {
    return typ_equal(typ_generic_functor_const(a), typ_generic_functor_const(b));
  } else {
    return FALSE;
  }
}

static error unify_with_equal(struct module *mod, const struct node *for_error,
                              struct typ *a, struct typ *b);

static error unify_same_generic_functor(struct module *mod, const struct node *for_error,
                                        struct typ *a, struct typ *b) {
  assert(typ_generic_arity(a) != 0 && typ_generic_arity(b) != 0);

  if (typ_equal(a, b)) {
    error e = unify_with_equal(mod, for_error, a, b);
    EXCEPT(e);
    return 0;
  }

  assert(typ_generic_arity(a) == typ_generic_arity(b));
  const size_t arity = typ_generic_arity(a);
  for (size_t n = 0; n < arity; ++n) {
    struct typ *arga = typ_generic_arg(a, n);
    struct typ *argb = typ_generic_arg(b, n);

    error e = unify(mod, for_error, arga, argb);
    if (e) {
      e = mk_except_type(mod, for_error,
                         "unifying generic argument at position %zu", 1 + n);
      THROW(e);
    }
  }

  return 0;
}

struct instance_of {
  struct typ *functor;
  struct typ *result;
};

static error find_instance_of(struct module *mod, struct typ *t,
                              struct typ *intf, bool *stop, void *user) {
  struct instance_of *r = user;

  struct typ *intf0 = typ_generic_functor(intf);
  if (intf0 != NULL && typ_equal(intf0, r->functor)) {
    r->result = intf;
    *stop = TRUE;
  }

  return 0;
}

static error unify_generics(struct module *mod, const struct node *for_error,
                            struct typ *a, struct typ *b,
                            bool a_tentative, bool b_tentative) {
  struct typ *a0 = typ_generic_functor(a);
  struct typ *b0 = typ_generic_functor(b);
  bool a_genf = typ_is_generic_functor(a);
  bool b_genf = typ_is_generic_functor(b);

  error e;
  if (typ_isa(a0, b0)) {
    // noop
  } else if (a_tentative && typ_isa(b0, a0)) {
    SWAP(a, b);
    SWAP(a0, b0);
    SWAP(a_genf, b_genf);
  } else if (typ_definition(a)->which == DEFINTF
             && typ_definition(b)->which == DEFINTF) {
    assert(FALSE && "FIXME Unsupported (e.g. combining constraints `arithmethic and `bitwise)");
  } else {
    e = mk_except_type_unification(mod, for_error, a, b);
    THROW(e);
  }

  if (!b_genf && !typ_equal(b, TBI_ANY)) {
    struct typ *b_in_a = NULL;

    if (typ_equal(a0, b0)) {
      b_in_a = a;
    } else {
      struct instance_of user = { .functor = b0, .result = NULL };
      typ_isalist_foreach(mod, a, 0, find_instance_of, &user);
      assert(user.result != NULL);
      b_in_a = user.result;
    }
    assert(b_in_a != b && "FIXME What does that mean?");

    e = unify_same_generic_functor(mod, for_error, b_in_a, b);
    EXCEPT(e);
  }

  typ_link_tentative(a, b);

  return 0;
}

static error unify_non_generic(struct module *mod, const struct node *for_error,
                               struct typ *a, struct typ *b,
                               bool a_non_generic, bool b_non_generic) {
  error e;

  if (a_non_generic && b_non_generic) {
    e = unify_two_non_generic(mod, for_error, a, b);
    EXCEPT(e);
    return 0;
  }

  if (!typ_is_tentative(b)) {
    SWAP(a, b);
  }

  e = typ_check_isa(mod, for_error, a, b);
  EXCEPT(e);

  typ_link_tentative(a, b);

  return 0;
}

static error unify_literal(struct module *mod, const struct node *for_error,
                           struct typ *a, struct typ *b,
                           bool a_literal, bool b_literal) {
  error e;
  if (a_literal && b_literal) {
    bool a_floating = typ_equal(a, TBI_LITERALS_FLOATING);
    bool b_floating = typ_equal(b, TBI_LITERALS_FLOATING);
    if (a_floating || b_floating) {
      if (!a_floating && b_floating) {
        SWAP(a, b);
        SWAP(a_floating, b_floating);
      }
      if (!a_floating) {
        if (!typ_equal(a, TBI_LITERALS_INTEGER)) {
          e = mk_except_type_unification(mod, for_error, a, b);
          THROW(e);
        }
      }
    } else {
      e = typ_check_equal(mod, for_error, a, b);
      EXCEPT(e);
    }

    typ_link_tentative(a, b);

    return 0;
  }

  if (a_literal) {
    SWAP(a, b);
  }

  if (typ_equal(b, TBI_LITERALS_NULL)) {
    if (typ_generic_functor(a) != NULL
        && typ_equal(typ_generic_functor(a), TBI__REF_COMPATIBLE)) {
      struct typ *real_a = typ_generic_arg(a, 0);
      e = unify(mod, for_error, real_a, b);
      EXCEPT(e);
      return 0;
    } else {
      e = typ_check_isa(mod, for_error, a, TBI_ANY_NULLABLE_REF);
      EXCEPT(e);
    }
  } else if (typ_equal(b, TBI_LITERALS_INTEGER)) {
    if (typ_is_tentative(a) && typ_isa(TBI_INTEGER, a)) {
      SWAP(a, b);
    } else {
      e = typ_check_isa(mod, for_error, a, TBI_INTEGER);
      EXCEPT(e);
    }
  } else if (typ_equal(b, TBI_LITERALS_FLOATING)) {
    if (typ_is_tentative(a) && typ_isa(TBI_FLOATING, a)) {
      SWAP(a, b);
    } else {
      e = typ_check_isa(mod, for_error, a, TBI_FLOATING);
      EXCEPT(e);
    }
  } else {
    assert(FALSE);
  }

  typ_link_tentative(a, b);

  return 0;
}

static error unify_with_weakly_concrete(bool *success,
                                        struct module *mod,
                                        const struct node *for_error,
                                        struct typ *a, struct typ *b,
                                        bool a_weakly_concrete,
                                        bool b_weakly_concrete) {
  if (a_weakly_concrete && !b_weakly_concrete) {
    SWAP(a, b);
  }

  if (typ_equal(b, TBI_BOOL)) {
    if (typ_isa(a, TBI_BOOL_COMPATIBLE)) {
      typ_link_tentative(a, b);
      *success = TRUE;
    }
  } else if (typ_equal(b, TBI_STATIC_STRING)) {
    if (typ_isa(a, TBI_STATIC_STRING_COMPATIBLE)) {
      typ_link_tentative(a, b);
      *success = TRUE;
    }
  } else if (same_generic_functor(mod, b, TBI_STATIC_ARRAY)) {
    if (typ_isa(a, TBI_ARRAY_CTOR)) {
      typ_link_tentative(typ_generic_arg(a, 0), typ_generic_arg(b, 0));
      typ_link_tentative(a, b);
      *success = TRUE;
    }
  }

  return 0;
}

static size_t insert_defnamedliterals(struct module *mod,
                                      struct node *littype,
                                      struct node *a) {
  size_t count = 0;
  FOREACH_SUB_CONST(of, a) {
    if (of->which != DEFFIELD) {
      continue;
    }
    struct node *f = mk_node(mod, littype, DEFFIELD);
    struct node *name = mk_node(mod, f, IDENT);
    name->as.IDENT.name = node_ident(node_subs_first_const(of));
    struct node *t = mk_node(mod, f, DIRECTDEF);
    set_typ(&t->as.DIRECTDEF.typ, node_subs_at_const(of, 1)->typ);
    t->as.DIRECTDEF.flags = NODE_IS_TYPE;
    count += 1;
  }
  return count;
}

static struct typ *merge_defnamedliterals(struct module *mod,
                                          struct node *a, struct node *b) {
  // FIXME: Detached node, would have to be freed when releasing the
  // mod fun_state in which it is recorded below.
  //
  struct node *littype = mempool_calloc(mod, 1, sizeof(struct node));
  node_set_which(littype, DEFNAMEDLITERAL);
  struct node *littype_name = mk_node(mod, littype, IDENT);
  littype_name->as.IDENT.name = gensym(mod);
  (void)mk_node(mod, littype, GENARGS);

  size_t arity = 0;
  arity += insert_defnamedliterals(mod, littype, a);
  arity += insert_defnamedliterals(mod, littype, b);

  struct typ **args = mempool_calloc(mod, arity, sizeof(struct typ *));
  size_t n = 0;
  FOREACH_SUB_CONST(f, littype) {
    if (f->which != DEFFIELD) {
      continue;
    }
    const struct node *t = node_subs_at_const(f, 1);
    assert(t->which == DIRECTDEF);
    args[n] = t->as.DIRECTDEF.typ;
    n += 1;
  }

  const bool tentative = instantiation_is_tentative(mod, littype->typ,
                                                    args, arity);
  free(args);
  error e = catchup_instantiation(mod, mod,
                                  littype, &mod->body->scope,
                                  tentative);
  assert(!e);

  return littype->typ;
}

HTABLE_SPARSE(ident_typ_map, struct typ *, ident);
implement_htable_sparse(unused__ static, ident_typ_map, struct typ *, ident);

static error unify_two_defnamedliterals(struct module *mod, const struct node *for_error,
                                        struct typ *a, struct typ *b) {
  struct ident_typ_map map;
  ident_typ_map_init(&map, 0);
  ident_typ_map_set_delete_val(&map, FALSE);
  ident_typ_map_set_custom_hashf(&map, ident_hash);
  ident_typ_map_set_custom_cmpf(&map, ident_cmp);

  struct node *da = typ_definition(a);
  FOREACH_SUB_CONST(f, da) {
    if (f->which != DEFFIELD) {
      continue;
    }
    ident_typ_map_set(&map, node_ident(node_subs_first_const(f)),
                      node_subs_at_const(f, 1)->typ);
  }

  struct node *db = typ_definition(b);
  FOREACH_SUB_CONST(f, db) {
    if (f->which != DEFFIELD) {
      continue;
    }
    struct typ **existing = ident_typ_map_get(&map, node_ident(node_subs_first_const(f)));
    if (existing != NULL) {
      error e = unify(mod, for_error, *existing, node_subs_at_const(f, 1)->typ);
      EXCEPT(e);
    }
  }

  struct typ *t = merge_defnamedliterals(mod, da, db);
  typ_link_tentative(t, a);
  typ_link_tentative(t, b);

  ident_typ_map_destroy(&map);

  return 0;
}

static error unify_with_defnamedliteral(struct module *mod, const struct node *for_error,
                                         struct typ *a, struct typ *dnl) {
  error e;

  struct node *a_def = typ_definition(a);
  struct node *ddnl = typ_definition(dnl);
  FOREACH_SUB_CONST(f, ddnl) {
    if (f->which != DEFFIELD) {
      continue;
    }
    ident f_name = node_ident(f);
    struct node *d = NULL;
    e = scope_lookup_ident_immediate(&d, a_def, mod, &a_def->scope, f_name, FALSE);
    EXCEPT(e);

    e = unify(mod, for_error, f->typ, d->typ);
    EXCEPT(e);
  }

  typ_link_tentative(a, dnl);

  return 0;
}

static error unify_defnamedliterals(struct module *mod, const struct node *for_error,
                                    struct typ *a, struct typ *b,
                                    bool a_dnl, bool b_dnl) {
  error e;
  if (a_dnl && b_dnl) {
    e = unify_two_defnamedliterals(mod, for_error, a, b);
    EXCEPT(e);

    return 0;
  }

  if (a_dnl && !b_dnl) {
    SWAP(a, b);
    SWAP(a_dnl, b_dnl);
  }

  e = unify_with_defnamedliteral(mod, for_error, a, b);
  EXCEPT(e);

  return 0;
}

static error unify_two_defunknownidents(struct module *mod, const struct node *for_error,
                                        struct typ *a, struct typ *b) {
  const struct node *a_unk = node_subs_at_const(typ_definition_const(a),
                                                IDX_UNKNOWN_IDENT);
  const struct node *b_unk = node_subs_at_const(typ_definition_const(b),
                                                IDX_UNKNOWN_IDENT);
  assert(a_unk->which == IDENT && b_unk->which == IDENT);

  if (node_ident(a_unk) != node_ident(b_unk)) {
    error e = mk_except_type(mod, for_error,
                             "unresolved idents '%s' and '%s' cannot be unified",
                             idents_value(mod->gctx, node_ident(a_unk)),
                             idents_value(mod->gctx, node_ident(b_unk)));
    THROW(e);
  }

  typ_link_tentative(a, b);
  return 0;
}

static error unify_with_defunknownident(struct module *mod, const struct node *for_error,
                                        struct typ *a, struct typ *dui) {
  const struct node *unk = node_subs_at_const(typ_definition_const(dui),
                                              IDX_UNKNOWN_IDENT);
  const struct node *da = typ_definition_const(a);

  error e;
  if (da->which != DEFTYPE
      || da->as.DEFTYPE.kind != DEFTYPE_ENUM) {
    char *s = typ_pretty_name(mod, a);
    e = mk_except_type(mod, for_error,
                       "ident '%s' cannot be resolved in type '%s'"
                       " (not an enum)",
                       idents_value(mod->gctx, node_ident(unk)), s);
    free(s);
    THROW(e);
  }

  struct node *def = NULL;
  e = scope_lookup_ident_immediate(&def, for_error, mod,
                                   &da->scope,
                                   node_ident(unk), FALSE);
  EXCEPT(e);

  typ_link_tentative(a, dui);

  return 0;
}

static error unify_defunknownidents(struct module *mod, const struct node *for_error,
                                    struct typ *a, struct typ *b,
                                    bool a_dui, bool b_dui) {
  error e;
  if (a_dui && b_dui) {
    e = unify_two_defunknownidents(mod, for_error, a, b);
    EXCEPT(e);

    return 0;
  }

  if (a_dui && !b_dui) {
    SWAP(a, b);
    SWAP(a_dui, b_dui);
  }

  e = unify_with_defunknownident(mod, for_error, a, b);
  EXCEPT(e);

  return 0;
}

static error unify_with_equal(struct module *mod, const struct node *for_error,
                              struct typ *a, struct typ *b) {
  error e = typ_check_equal(mod, for_error, a, b);
  EXCEPT(e);

  if (!typ_is_tentative(b)) {
    SWAP(b, a);
  }

  assert(typ_generic_arity(a) == typ_generic_arity(b));
  const size_t arity = typ_generic_arity(a);
  for (size_t n = 0; n < arity; ++n) {
    struct typ *arga = typ_generic_arg(a, n);
    struct typ *argb = typ_generic_arg(b, n);

    error e = unify_with_equal(mod, for_error, arga, argb);
    assert(!e);
  }

  if (typ_is_tentative(b)) {
    typ_link_tentative(a, b);
  } else {
    assert(a == b);
  }

  return 0;
}

static error check_reference_compatibility(struct module *mod,
                                           const struct node *for_error,
                                           const struct typ *a,
                                           const struct typ *target) {
  if (typ_equal(target, TBI_ANY_ANY_REF)) {
    return 0;
  }

  const struct typ *a0 = typ_generic_functor_const(a);
  const struct typ *target0 = typ_generic_functor_const(target);

  bool ok = FALSE;
  error e;
  if (typ_equal(target0, TBI_ANY_REF)) {
    ok = typ_isa(a0, TBI_ANY_REF);
  } else if (typ_equal(target0, TBI_ANY_MUTABLE_REF)) {
    ok = typ_isa(a0, TBI_ANY_MUTABLE_REF);
  } else if (typ_equal(target0, TBI_ANY_NULLABLE_REF)) {
    ok = typ_isa(a0, TBI_ANY_REF)
      || typ_isa(a0, TBI_ANY_NULLABLE_REF);
  } else if (typ_equal(target0, TBI_ANY_NULLABLE_MUTABLE_REF)) {
    ok = typ_isa(a0, TBI_ANY_MUTABLE_REF)
      || typ_isa(a0, TBI_ANY_NULLABLE_MUTABLE_REF);

  } else if (typ_equal(target0, TBI_REF)) {
    ok = typ_isa(a0, TBI_ANY_REF);
  } else if (typ_equal(target0, TBI_MREF)) {
    ok = typ_isa(a0, TBI_ANY_MUTABLE_REF);
  } else if (typ_equal(target0, TBI_MMREF)) {
    ok = typ_equal(a0, TBI_MMREF);
  } else if (typ_equal(target0, TBI_NREF)) {
    ok = typ_isa(a0, TBI_ANY_NULLABLE_REF)
      || typ_isa(a0, TBI_ANY_REF);
  } else if (typ_equal(target0, TBI_NMREF)) {
    ok = typ_isa(a0, TBI_ANY_NULLABLE_MUTABLE_REF)
      || typ_isa(a0, TBI_ANY_MUTABLE_REF);
  } else if (typ_equal(target0, TBI_NMMREF)) {
    ok = typ_equal(a0, TBI_NMMREF)
      || typ_equal(a0, TBI_MMREF);
  }

  if (!ok) {
    e = mk_except_type_unification(mod, for_error, a, target);
    THROW(e);
  }

  return 0;
}

static error unify_dyn(struct module *mod, const struct node *for_error,
                       struct typ *a, struct typ *b,
                       bool a_intf, bool b_intf) {
  if (!b_intf) {
    SWAP(a, b);
    SWAP(a_intf, b_intf);
  }

  error e = typ_check_isa(mod, for_error, a, b);
  EXCEPT(e);

  return 0;
}

static error unify_reference_arg(struct module *mod,
                                 const struct node *for_error,
                                 struct typ *a, struct typ *b) {
  struct typ *arg_a = typ_generic_arg(a, 0);
  struct typ *arg_b = typ_generic_arg(b, 0);
  const bool arg_a_intf = typ_definition_const(arg_a)->which == DEFINTF;
  const bool arg_b_intf = typ_definition_const(arg_b)->which == DEFINTF;
  const bool arg_a_tentative = typ_is_tentative(arg_a);
  const bool arg_b_tentative = typ_is_tentative(arg_b);
  const bool arg_a_weak = typ_is_weakly_concrete(arg_a);
  const bool arg_b_weak = typ_is_weakly_concrete(arg_b);

  error e;
  if ((arg_a_intf || arg_b_intf)
      && ((!arg_a_tentative && !arg_b_tentative)
          || (!arg_a_tentative && arg_b_weak)
          || (!arg_b_tentative && arg_a_weak))) {
    e = unify_dyn(mod, for_error, arg_a, arg_b, arg_a_intf, arg_b_intf);
    EXCEPT(e);
  } else {
    e = unify(mod, for_error, arg_a, arg_b);
    EXCEPT(e);
  }

  return 0;
}

static error unify_with_reference_compatible(struct module *mod,
                                             const struct node *for_error,
                                             struct typ *a, struct typ *b,
                                             bool a_ref_compatible,
                                             bool b_ref_compatible) {
  if (a_ref_compatible) {
    SWAP(a, b);
    SWAP(a_ref_compatible, b_ref_compatible);
  }

  struct typ *real_b = typ_generic_arg(b, 0);

  error e = check_reference_compatibility(mod, for_error, a, real_b);
  EXCEPT(e);

  if (!typ_equal(real_b, TBI_ANY_ANY_REF)) {
    struct typ *real_b0 = typ_generic_functor(real_b);
    if (typ_definition_const(real_b0)->which == DEFINTF && typ_is_tentative(real_b0)) {
      typ_link_tentative(typ_generic_functor(a), real_b0);
    }
  }

  if (typ_equal(a, real_b)) {
    e = unify_with_equal(mod, for_error, a, real_b);
    EXCEPT(e);
    return 0;
  }

  e = unify_reference_arg(mod, for_error, a, real_b);
  EXCEPT(e);

  return 0;
}

static error unify_reference(struct module *mod, const struct node *for_error,
                             struct typ *a, struct typ *b,
                             bool a_ref, bool b_ref) {
  error e;

  if (!a_ref) {
    SWAP(a, b);
    SWAP(a_ref, b_ref);
  }

  if (!typ_is_reference(b) && typ_is_tentative(b) && typ_isa(a, b)) {
    typ_link_tentative(a, b);
    return 0;
  }

  struct typ *a0 = typ_generic_functor(a);
  struct typ *b0 = typ_generic_functor(b);

  const bool a_ref_compatible = a0 != NULL && typ_equal(a0, TBI__REF_COMPATIBLE);
  const bool b_ref_compatible = b0 != NULL && typ_equal(b0, TBI__REF_COMPATIBLE);
  if (a_ref_compatible || b_ref_compatible) {
    e = unify_with_reference_compatible(mod, for_error, a, b,
                                        a_ref_compatible, b_ref_compatible);
    EXCEPT(e);
    return 0;
  }

  if (typ_equal(a, TBI_ANY_ANY_REF)) {
    typ_link_tentative(b, a);
    return 0;
  }

  if (typ_equal(b, TBI_ANY_ANY_REF)) {
    typ_link_tentative(a, b);
    return 0;
  }

  if (typ_isa(b0, a0)) {
    SWAP(a, b);
    SWAP(a0, b0);
    SWAP(a_ref, b_ref);
  }

  if (!typ_isa(a0, b0)) {
    e = mk_except_type_unification(mod, for_error, a, b);
    THROW(e);
  }

  if (typ_definition_const(b0)->which == DEFINTF && typ_is_tentative(b0)) {
    typ_link_tentative(a0, b0);
  }

  if (typ_equal(a, b)) {
    e = unify_with_equal(mod, for_error, a, b);
    EXCEPT(e);
    return 0;
  }

  e = unify_reference_arg(mod, for_error, a, b);
  EXCEPT(e);

  return 0;
}

//static error typ_ref(struct typ **result,
//                     struct module *mod, struct node *for_error,
//                     enum token_type op, struct typ *typ);
//
//EXAMPLE_NCC(unify_with_reference_literal) {
//  struct node *foo = mock_deffun(mod, "foo");
//  struct node *body = foo->subs[foo->subs_count-1];
//  mock_parse(mod, body, "return (@1).");
//
//  G(intlit, body, NUMBER);
//  intlit->as.NUMBER.value = "1";
//  intlit->typ = typ_create_tentative(TBI_LITERALS_INTEGER);
//  struct node *for_error = intlit;
//
//  struct typ *rintlit = typ_create(NULL);
//  error e = typ_ref(&rintlit, mod, for_error, TREFDOT, intlit->typ);
//  assert(!e);
//  assert(typ_is_tentative(rintlit));
//
//  struct typ *drintlit = typ_generic_arg(rintlit, 0);
//  assert(typ_equal(drintlit, TBI_LITERALS_INTEGER));
//  assert(typ_is_tentative(drintlit));
//
//  struct typ *i32 = TBI_I32;
//  struct typ *ri32 = typ_create(NULL);
//  e = typ_ref(&ri32, mod, for_error, TREFDOT, i32);
//  assert(!e);
//  assert(typ_is_tentative(ri32));
//
//  assert(typ_equal(ri32, rintlit));
//}

static error unify_with_any(struct module *mod, const struct node *for_error,
                            struct typ *a, struct typ *b,
                            bool a_is_any, bool b_is_any) {
  if (a_is_any) {
    SWAP(a, b);
    SWAP(a_is_any, b_is_any);
  }

  typ_link_tentative(a, b);

  return 0;
}

error unify(struct module *mod, const struct node *for_error,
            struct typ *a, struct typ *b) {
  error e;

  bool a_tentative = typ_is_tentative(a);
  bool b_tentative = typ_is_tentative(b);

  if (!a_tentative && !b_tentative) {
    e = typ_check_equal(mod, for_error, a, b);
    EXCEPT(e);
    return 0;
  }

  if (a_tentative && !b_tentative) {
    SWAP(a, b);
    SWAP(a_tentative, b_tentative);
  }

  if (typ_equal(a, b)) {
    e = unify_with_equal(mod, for_error, a, b);
    EXCEPT(e);
    return 0;
  }

  const bool a_is_any = typ_equal(a, TBI_ANY);
  const bool b_is_any = typ_equal(b, TBI_ANY);
  if (a_is_any || b_is_any) {
    unify_with_any(mod, for_error, a, b, a_is_any, b_is_any);
    return 0;
  }

  const struct node *da = typ_definition_const(a);
  const struct node *db = typ_definition_const(b);

  const bool a_dui = da->which == DEFUNKNOWNIDENT;
  const bool b_dui = db->which == DEFUNKNOWNIDENT;
  if (a_dui || b_dui) {
    e = unify_defunknownidents(mod, for_error, a, b, a_dui, b_dui);
    EXCEPT(e);
    return 0;
  }


  const bool a_dnl = da->which == DEFNAMEDLITERAL;
  const bool b_dnl = db->which == DEFNAMEDLITERAL;
  if (a_dnl || b_dnl) {
    e = unify_defnamedliterals(mod, for_error, a, b, a_dnl, b_dnl);
    EXCEPT(e);
    return 0;
  }

  const bool a_literal = typ_is_literal(a);
  const bool b_literal = typ_is_literal(b);
  if (a_literal || b_literal) {
    e = unify_literal(mod, for_error, a, b, a_literal, b_literal);
    EXCEPT(e);
    return 0;
  }

  const bool a_ref = typ_is_reference(a);
  const bool b_ref = typ_is_reference(b);
  if (a_ref || b_ref) {
    e = unify_reference(mod, for_error, a, b, a_ref, b_ref);
    EXCEPT(e);
    return 0;
  }

  const bool a_weakly_concrete = typ_is_weakly_concrete(a);
  const bool b_weakly_concrete = typ_is_weakly_concrete(b);
  if (a_weakly_concrete || b_weakly_concrete) {
    bool success = FALSE;
    e = unify_with_weakly_concrete(&success,
                                   mod, for_error, a, b,
                                   a_weakly_concrete, b_weakly_concrete);
    EXCEPT(e);

    if (success) {
      return 0;
    }
  }

  const bool a_non_generic = typ_generic_arity(a) == 0;
  const bool b_non_generic = typ_generic_arity(b) == 0;
  if (a_non_generic || b_non_generic) {
    e = unify_non_generic(mod, for_error, a, b, a_non_generic, b_non_generic);
    EXCEPT(e);
    return 0;
  }

  e = unify_generics(mod, for_error, a, b, a_tentative, b_tentative);
  EXCEPT(e);

  return 0;
}

EXAMPLE_NCC(unify) {
//  struct node *for_error = calloc(1, sizeof(struct node));
//  error e;
//  {
//    struct typ *a = TBI_I32;
//    struct typ *b = NULL;
//    set_typ(&b, typ_create_tentative(TBI_INTEGER));
//    e = unify(mod, for_error, a, b);
//    assert(!e);
//  }
//  {
//    struct typ *a = TBI_I32;
//    struct typ *b = TBI_U32;
//    e = unify(mod, for_error, a, b);
//    assert(e);
//  }
}
