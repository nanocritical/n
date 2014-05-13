#include "unify.h"

#include "table.h"
#include "types.h"
#include "scope.h"
#include "passes.h"
#include "passbody.h"

enum unify_flags {
  REFCOMPAT_LEFT = 0x1,
  REFCOMPAT_RIGHT = 0x2,
  REFCOMPAT__SWAPPABLES = REFCOMPAT_LEFT | REFCOMPAT_RIGHT,
};

#define SWAP_FLAGS(flags) do { \
  uint32_t tmp = (flags); \
  flags &= ~REFCOMPAT__SWAPPABLES; \
  if (tmp & REFCOMPAT_LEFT) { \
    flags |= REFCOMPAT_RIGHT; \
  } \
  if (tmp & REFCOMPAT_RIGHT) { \
    flags |= REFCOMPAT_LEFT; \
  } \
} while (0)

static error do_unify(struct module *mod, uint32_t flags,
                      const struct node *for_error,
                      struct typ *a, struct typ *b);

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
    struct node *dinc = defincomplete_create(mod, for_error);

    defincomplete_add_isa(mod, for_error, dinc, a);
    defincomplete_add_isa(mod, for_error, dinc, b);

    error e = defincomplete_catchup(mod, dinc);
    assert(!e);

    typ_link_tentative(dinc->typ, a);
    typ_link_tentative(dinc->typ, b);
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
    return false;
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
    *stop = true;
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
    struct node *dinc = defincomplete_create(mod, for_error);
    defincomplete_add_isa(mod, for_error, dinc, a);
    defincomplete_add_isa(mod, for_error, dinc, b);
    e = defincomplete_catchup(mod, dinc);
    THROW(e);
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

    assert(!typ_is_tentative(a0) && "FIXME handle it");

    e = unify_same_generic_functor(mod, for_error, b_in_a, b);
    EXCEPT(e);
  }

  // FIXME: there are holes here: 2nd order generics, etc.

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

static struct typ *nullable(struct module *mod, const struct node *for_error,
                            struct typ *t) {
  if (typ_isa(t, TBI_ANY_NULLABLE_REF)) {
    return t;
  }

  struct typ *r0 = NULL;
  const struct typ *t0 = typ_generic_functor_const(t);
  if (typ_equal(t0, TBI_REF)) {
    r0 = TBI_NREF;
  } else if (typ_equal(t0, TBI_MREF)) {
    r0 = TBI_NMREF;
  } else if (typ_equal(t0, TBI_MMREF)) {
    r0 = TBI_NMMREF;
  } else {
    assert(false);
  }

  struct typ *a = typ_generic_arg(t, 0);

  struct node *i = NULL;
  error e = instance(&i, mod, for_error, 0, r0, &a, 1);
  assert(!e);
  return i->typ;
}

static error unify_literal(struct module *mod, uint32_t flags,
                           const struct node *for_error,
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
        SWAP_FLAGS(flags);
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
    if (typ_generic_functor(a) != NULL && (flags & REFCOMPAT_LEFT)) {
      e = do_unify(mod, flags & ~REFCOMPAT_LEFT, for_error, a, b);
      EXCEPT(e);
      return 0;
    } else if (flags & REFCOMPAT_RIGHT) {
      e = typ_check_isa(mod, for_error, a, TBI_ANY_REF);
      EXCEPT(e);

      typ_link_tentative(nullable(mod, for_error, a), b);
    } else {
      e = typ_check_isa(mod, for_error, a, TBI_ANY_NULLABLE_REF);
      EXCEPT(e);

      typ_link_tentative(a, b);
    }
  } else if (typ_equal(b, TBI_LITERALS_INTEGER)) {
    if (!typ_is_tentative(a) && typ_isa(TBI_INTEGER, a) && !typ_equal(a, TBI_INTEGER)) {
      // noop
    } else if (typ_is_tentative(a) && typ_isa(b, a)) {
      typ_link_tentative(b, a);
    } else {
      e = typ_check_isa(mod, for_error, a, TBI_INTEGER);
      EXCEPT(e);

      typ_link_tentative(a, b);
    }
  } else if (typ_equal(b, TBI_LITERALS_FLOATING) && !typ_equal(a, TBI_FLOATING)) {
    if (!typ_is_tentative(a) && typ_isa(TBI_FLOATING, a)) {
      // noop
    } else if (typ_is_tentative(a) && typ_isa(b, a)) {
      typ_link_tentative(b, a);
    } else {
      e = typ_check_isa(mod, for_error, a, TBI_FLOATING);
      EXCEPT(e);

      typ_link_tentative(a, b);
    }
  } else {
    assert(false);
  }

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
      *success = true;
    }
  } else if (typ_equal(b, TBI_STATIC_STRING)) {
    if (typ_isa(a, TBI_STATIC_STRING_COMPATIBLE)) {
      typ_link_tentative(a, b);
      *success = true;
    }
  } else if (same_generic_functor(mod, b, TBI_STATIC_ARRAY)) {
    if (typ_isa(a, TBI_ARRAY_CTOR)) {
      typ_link_tentative(typ_generic_arg(a, 0), typ_generic_arg(b, 0));
      typ_link_tentative(a, b);
      *success = true;
    }
  }

  return 0;
}

static struct typ *merge_defincomplete(struct module *mod, const struct node *for_error,
                                       const struct node *a, const struct node *b) {
  struct node *dinc = defincomplete_create(mod, for_error);

  if (a->as.DEFINCOMPLETE.ident != ID__NONE) {
    defincomplete_set_ident(mod, a->as.DEFINCOMPLETE.ident_for_error,
                            dinc, a->as.DEFINCOMPLETE.ident);
  } else if (b->as.DEFINCOMPLETE.ident != ID__NONE) {
    defincomplete_set_ident(mod, b->as.DEFINCOMPLETE.ident_for_error,
                            dinc, b->as.DEFINCOMPLETE.ident);
  }

  const struct node *a_isalist = subs_at_const(a, IDX_ISALIST);
  FOREACH_SUB_CONST(isa, a_isalist) {
    defincomplete_add_isa(mod, isa, dinc, isa->typ);
  }
  const struct node *b_isalist = subs_at_const(b, IDX_ISALIST);
  FOREACH_SUB_CONST(isa, b_isalist) {
    defincomplete_add_isa(mod, isa, dinc, isa->typ);
  }

  FOREACH_SUB_CONST(f, a) {
    if (f->which == DEFFIELD) {
      defincomplete_add_field(mod, f, dinc, node_ident(f), f->typ);
    }
  }
  FOREACH_SUB_CONST(f, b) {
    if (f->which == DEFFIELD) {
      defincomplete_add_field(mod, f, dinc, node_ident(f), f->typ);
    }
  }

  error e = defincomplete_catchup(mod, dinc);
  assert(!e);
  return dinc->typ;
}

HTABLE_SPARSE(ident_typ_map, struct typ *, ident);
IMPLEMENT_HTABLE_SPARSE(unused__ static, ident_typ_map, struct typ *, ident,
                        ident_hash, ident_cmp);

static error unify_two_defincomplete(struct module *mod,
                                     const struct node *for_error,
                                     struct typ *a, struct typ *b) {
  error e;
  const char *reason;

  struct node *da = typ_definition(a);
  struct node *db = typ_definition(b);
  if (da->as.DEFINCOMPLETE.ident != ID__NONE
      && db->as.DEFINCOMPLETE.ident != ID__NONE) {
    reason = "conflicting idents";
    goto except;
  }

  struct ident_typ_map map;
  ident_typ_map_init(&map, 0);
  ident_typ_map_set_delete_val(&map, false);

  FOREACH_SUB_CONST(f, da) {
    if (f->which != DEFFIELD) {
      continue;
    }
    ident_typ_map_set(&map, node_ident(subs_first_const(f)),
                      subs_at_const(f, 1)->typ);
  }

  FOREACH_SUB_CONST(f, db) {
    if (f->which != DEFFIELD) {
      continue;
    }
    struct typ **existing = ident_typ_map_get(&map, node_ident(subs_first_const(f)));
    if (existing != NULL) {
      reason = "conflicting fields";
      e = unify(mod, for_error, *existing, subs_at_const(f, 1)->typ);
      GOTO_EXCEPT(e);
    }
  }

  struct typ *r = merge_defincomplete(mod, for_error, da, db);
  typ_link_tentative(r, a);
  typ_link_tentative(r, b);

  ident_typ_map_destroy(&map);
  return 0;

except:
  assert(e == EINVAL);
  char sa[2048], sb[2048];
  snprint_defincomplete(sa, ARRAY_SIZE(sa), mod, da);
  snprint_defincomplete(sb, ARRAY_SIZE(sb), mod, db);

  e = mk_except_type(mod, for_error,
                     "%s for incomplete types\n%sand\n%s",
                     reason, sa, sb);
  THROW(e);
}

static error unify_with_defunknownident(struct module *mod, const struct node *for_error,
                                        struct node *da, struct node *dinc) {
  assert(dinc->which == DEFINCOMPLETE);
  ident unk = dinc->as.DEFINCOMPLETE.ident;

  error e;
  if (da->which != DEFTYPE
      || (da->as.DEFTYPE.kind != DEFTYPE_ENUM
          && da->as.DEFTYPE.kind != DEFTYPE_UNION)) {
    char *s = typ_pretty_name(mod, da->typ);
    e = mk_except_type(mod, for_error,
                       "ident '%s' cannot be resolved in type '%s'"
                       " (not an enum)",
                       idents_value(mod->gctx, unk), s);
    free(s);
    THROW(e);
  }

  struct node *r = NULL;
  e = scope_lookup_ident_immediate(&r, for_error, mod, &da->scope,
                                   unk, false);
  EXCEPT(e);

  // Will typ_link_tentative() in unify_with_defincomplete().

  return 0;
}

static bool has_variant_with_field(struct module *mod,
                                   const struct node *for_error,
                                   const struct node *d, ident f) {
  FOREACH_SUB_CONST(dc, d) {
    if (dc->which != DEFCHOICE) {
      continue;
    }

    struct node *r = NULL;
    error e = scope_lookup_ident_immediate(&r, for_error, mod, &dc->scope,
                                           f, true);
    if (!e) {
      return true;
    }

    if (has_variant_with_field(mod, for_error, dc, f)) {
      return true;
    }
  }

  return false;
}

// Exported for the sole benefit of finalize_defincomplete_unification().
error unify_with_defincomplete_entrails(struct module *mod,
                                        const struct node *for_error,
                                        struct typ *a,
                                        struct typ *inc, struct node *dinc) {
  error e;

  struct node *da = typ_definition(a);
  if (dinc->as.DEFINCOMPLETE.ident != ID__NONE) {
    e = unify_with_defunknownident(mod, for_error, da, dinc);
    EXCEPT(e);
  }

  const bool is_union = da->which == DEFTYPE && da->as.DEFTYPE.kind == DEFTYPE_UNION;
  if (is_union) {
    FOREACH_SUB_CONST(f, dinc) {
      if (f->which != DEFFIELD) {
        continue;
      }

      if (!has_variant_with_field(mod, for_error, da, node_ident(f))) {
        e = mk_except_type(mod, for_error, "cannot resolve field '%s' "
                           "in any variant of the union",
                           idents_value(mod->gctx, node_ident(f)));
        (void) e;
        e = mk_except_type_unification(mod, for_error, a, inc);
        THROW(e);
      }
    }

    return 0;
  }

  // FIXME: if 'a' is a tentative intf, we should be adding that intf as a
  // restriction on 'dinc'.

  FOREACH_SUB_CONST(f, dinc) {
    if (f->which != DEFFIELD) {
      continue;
    }

    ident f_name = node_ident(f);
    struct node *d = NULL;
    e = scope_lookup_ident_immediate(&d, for_error, mod, &da->scope, f_name, false);
    EXCEPT(e);

    e = unify(mod, for_error, f->typ, d->typ);
    EXCEPT(e);
  }

  return 0;
}

static error unify_with_defincomplete(struct module *mod,
                                      const struct node *for_error,
                                      struct typ *a, struct typ *inc) {
  struct node *dinc = typ_definition(inc);
  error e = unify_with_defincomplete_entrails(mod, for_error, a, inc, dinc);
  EXCEPT(e);

  typ_link_tentative(a, inc);

  return 0;
}

static error unify_defincomplete(struct module *mod,
                                 const struct node *for_error,
                                 struct typ *a, struct typ *b,
                                 bool a_inc, bool b_inc) {
  error e;
  if (a_inc && b_inc) {
    e = unify_two_defincomplete(mod, for_error, a, b);
    EXCEPT(e);

    return 0;
  }

  if (a_inc && !b_inc) {
    SWAP(a, b);
    SWAP(a_inc, b_inc);
  }

  e = unify_with_defincomplete(mod, for_error, a, b);
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
    if (typ_is_generic_functor(b)) {
      typ_link_tentative_functor(mod, a, b);
    } else {
      typ_link_tentative(a, b);
    }
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

  bool ok = false;
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

static error unify_reference_arg(struct module *mod, uint32_t flags,
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
    e = do_unify(mod, flags, for_error, arg_a, arg_b);
    EXCEPT(e);
  }

  return 0;
}

struct typ *unify_with_new_functor(struct module *mod, const struct node *for_error,
                                   struct typ *f, struct typ *t) {
  assert(typ_is_generic_functor(f));
  assert(!typ_is_generic_functor(t));
  assert(typ_generic_arity(t) == typ_generic_arity(f));

  struct typ *t0 = typ_generic_functor(t);
  assert(typ_is_tentative(t));
  assert(typ_is_tentative(t0));

  struct node *i = instance_fully_implicit(mod, for_error, f);

  for (size_t n = 0, arity = typ_generic_arity(t); n < arity; ++n) {
    typ_link_tentative(typ_generic_arg(t, n), typ_generic_arg(i->typ, n));
  }

  typ_link_tentative(i->typ, t);

  return i->typ;
}

static error unify_with_reference_compatible(struct module *mod, uint32_t flags,
                                             const struct node *for_error,
                                             struct typ *a, struct typ *b,
                                             bool a_ref_compatible,
                                             bool b_ref_compatible) {
  if (a_ref_compatible) {
    SWAP(a, b);
    SWAP(a_ref_compatible, b_ref_compatible);
    SWAP_FLAGS(flags);
  }

  error e = check_reference_compatibility(mod, for_error, a, b);
  EXCEPT(e);

  if (typ_equal(b, TBI_ANY_ANY_REF)) {
    typ_link_tentative(a, b);
    return 0;
  }

  if (typ_equal(a, b)) {
    e = unify_with_equal(mod, for_error, a, b);
    EXCEPT(e);
    return 0;
  }

  e = unify_reference_arg(mod, flags, for_error, a, b);
  EXCEPT(e);

  struct typ *b0 = typ_generic_functor(b);
  if (typ_definition_const(b0)->which == DEFINTF && typ_is_tentative(b0)) {
    struct typ *a0 = typ_generic_functor(a);
    typ_link_tentative_functor(mod, a0, b0);
  }

  return 0;
}

static error unify_reference(struct module *mod, uint32_t flags,
                             const struct node *for_error,
                             struct typ *a, struct typ *b,
                             bool a_ref, bool b_ref) {
  error e;

  if (!a_ref) {
    SWAP(a, b);
    SWAP(a_ref, b_ref);
    SWAP_FLAGS(flags);
  }

  if (!b_ref && typ_is_tentative(b) && typ_isa(a, b)) {
    typ_link_tentative(a, b);
    return 0;
  }

  if (!b_ref) {
    e = mk_except_type_unification(mod, for_error, a, b);
    THROW(e);
  }

  struct typ *a0 = typ_generic_functor(a);
  struct typ *b0 = typ_generic_functor(b);

  const bool a_ref_compatible = flags & REFCOMPAT_LEFT;
  const bool b_ref_compatible = flags & REFCOMPAT_RIGHT;
  if (a_ref_compatible || b_ref_compatible) {
    e = unify_with_reference_compatible(mod, flags, for_error, a, b,
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
    SWAP_FLAGS(flags);
  }

  if (!typ_isa(a0, b0)) {
    e = mk_except_type_unification(mod, for_error, a, b);
    THROW(e);
  }

  e = unify_reference_arg(mod, flags, for_error, a, b);
  EXCEPT(e);

  if (typ_definition_const(b0)->which == DEFINTF && typ_is_tentative(b0)) {
    typ_link_tentative_functor(mod, a0, b0);
  }

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

static error do_unify(struct module *mod, uint32_t flags,
                      const struct node *for_error,
                      struct typ *a, struct typ *b) {
  error e;

  bool a_tentative = typ_is_tentative(a);
  bool b_tentative = typ_is_tentative(b);

  if (!(flags & (REFCOMPAT_LEFT | REFCOMPAT_RIGHT))
      && !a_tentative && !b_tentative) {
    e = typ_check_equal(mod, for_error, a, b);
    EXCEPT(e);
    return 0;
  }

  if (a_tentative && !b_tentative) {
    SWAP(a, b);
    SWAP(a_tentative, b_tentative);
    SWAP_FLAGS(flags);
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

  const bool a_inc = da->which == DEFINCOMPLETE;
  const bool b_inc = db->which == DEFINCOMPLETE;
  if (a_inc || b_inc) {
    e = unify_defincomplete(mod, for_error, a, b, a_inc, b_inc);
    EXCEPT(e);
    return 0;
  }

  const bool a_literal = typ_is_literal(a);
  const bool b_literal = typ_is_literal(b);
  if (a_literal || b_literal) {
    e = unify_literal(mod, flags, for_error, a, b, a_literal, b_literal);
    EXCEPT(e);
    return 0;
  }

  const bool a_ref = typ_is_reference(a);
  const bool b_ref = typ_is_reference(b);
  if (a_ref || b_ref) {
    e = unify_reference(mod, flags, for_error, a, b, a_ref, b_ref);
    EXCEPT(e);
    return 0;
  }

  const bool a_weakly_concrete = typ_is_weakly_concrete(a);
  const bool b_weakly_concrete = typ_is_weakly_concrete(b);
  if (a_weakly_concrete || b_weakly_concrete) {
    bool success = false;
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

error unify(struct module *mod, const struct node *for_error,
            struct typ *a, struct typ *b) {
  return do_unify(mod, 0, for_error, a, b);
}

// Be tolerant of acceptable differences in reference functors.
error unify_refcompat(struct module *mod, const struct node *for_error,
                      struct typ *target, struct typ *b) {
  error e;

  const bool target_ref = typ_is_reference(target);
  const bool b_ref = typ_is_reference(b);
  if (target_ref || b_ref) {
    e = do_unify(mod, REFCOMPAT_LEFT, for_error, target, b);
    EXCEPT(e);
    return 0;
  }

  e = unify(mod, for_error, target, b);
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
