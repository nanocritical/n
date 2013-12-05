#include "types.h"

#include <stdio.h>
#include "table.h"
#include "mock.h"

#define BACKLINKS_LEN 7 // arbitrary
#define USERS_LEN 7 // arbitrary

struct backlinks {
  size_t count;
  struct typ **links[BACKLINKS_LEN];
  struct backlinks *more;
};

struct users {
  size_t count;
  struct typ *users[USERS_LEN];
  struct users *more;
};

HTABLE_SPARSE(typs_set, bool, struct typ *);
implement_htable_sparse(__attribute__((unused)) static, typs_set, bool, struct typ *);

enum typ_flags {
  TYPF_TENTATIVE = 0x1,
  TYPF_BUILTIN = 0x2,
  TYPF_PSEUDO_BUILTIN = 0x4,
  TYPF_TRIVIAL = 0x8,
  TYPF_REF = 0x10,
  TYPF_LITERAL = 0x20,
  TYPF_WEAKLY_CONCRETE = 0x40,
  TYPF__INHERIT_FROM_FUNCTOR = TYPF_TENTATIVE
    | TYPF_TRIVIAL | TYPF_REF | TYPF_WEAKLY_CONCRETE,
  TYPF__MASK_HASH = 0xffff & ~TYPF_TENTATIVE,
};

struct typ {
  uint32_t flags;
  uint32_t hash;

  struct node *definition;

  struct typs_set quickisa;

  struct backlinks backlinks;
  struct users users;
};

#define FOREACH_BACKLINK(back, t, what) do { \
  struct backlinks *backlinks = &t->backlinks; \
  do { \
    for (size_t n = 0; n < backlinks->count; ++n) { \
      struct typ **back = backlinks->links[n]; \
      if (back != NULL) { \
        what; \
      } \
    } \
    backlinks = backlinks->more; \
  } while (backlinks != NULL); \
} while (0)

#define FOREACH_USER(user, t, what) do { \
  struct users *users = &t->users; \
  do { \
    for (size_t n = 0; n < users->count; ++n) { \
      struct typ *user = users->users[n]; \
      what; \
    } \
    users = users->more; \
  } while (users != NULL); \
} while (0)

static void remove_backlink(struct typ *t, struct typ **loc) {
  FOREACH_BACKLINK(back, t,
                   if (back == loc) { backlinks->links[n] = NULL; });
}

size_t typ_debug_backlinks_count(const struct typ *t) {
  const struct backlinks *backlinks = &t->backlinks;
  size_t c = 0;
  while (backlinks->more != NULL) {
    backlinks = backlinks->more;
    c += 7;
  }

  return c + backlinks->count;
}

void set_typ(struct typ **loc, struct typ *t) {
  if (*loc != NULL) {
    if (*loc == t) {
      return;
    }

    remove_backlink(t, loc);
  }

  *loc = t;

  if (typ_is_tentative(t)) {
    struct backlinks *backlinks = &t->backlinks;
    while (backlinks->more != NULL) {
      backlinks = backlinks->more;
    }

    if (backlinks->count == BACKLINKS_LEN) {
      backlinks->more = calloc(1, sizeof(*backlinks->more));
      backlinks = backlinks->more;
    }

    backlinks->links[backlinks->count] = loc;
    backlinks->count += 1;
  }
}

static void clear_backlinks(struct typ *t) {
  struct backlinks *b = t->backlinks.more;

  while (b != NULL) {
    struct backlinks *m = b->more;
    free(b);
    b = m;
  }

  memset(&t->backlinks, 0, sizeof(t->backlinks));
}

static void add_user(struct typ *arg, struct typ *user) {
  if (!typ_is_tentative(arg)) {
    return;
  }

  assert(typ_is_tentative(user));

  struct users *users = &arg->users;
  while (users->more != NULL) {
    users = users->more;
  }

  if (users->count == USERS_LEN) {
    users->more = calloc(1, sizeof(*users->more));
    users = users->more;
  }

  users->users[users->count] = user;
  users->count += 1;
}

static void clear_users(struct typ *t) {
  struct users *b = t->users.more;

  while (b != NULL) {
    struct users *m = b->more;
    free(b);
    b = m;
  }

  memset(&t->users, 0, sizeof(t->users));
}

static uint32_t typ_hash(const struct typ **a) {
  return (*a)->hash;
}

static int typ_cmp(const struct typ **a, const struct typ **b) {
  return !typ_equal(*a, *b);
}

static void quickisa_init(struct typ *t) {
  typs_set_init(&t->quickisa, 0);
  typs_set_set_delete_val(&t->quickisa, NULL);
  typs_set_set_custom_hashf(&t->quickisa, typ_hash);
  typs_set_set_custom_cmpf(&t->quickisa, typ_cmp);
}

static void create_flags(struct typ *t, struct typ *tbi) {
  if (typ_definition_const(t) == NULL) {
    // This is very early in the global init.
    return;
  }

  const struct toplevel *toplevel = node_toplevel_const(typ_definition_const(t));
  const struct typ *functor = NULL;
  if (toplevel != NULL) {
    functor = toplevel->our_generic_functor_typ;
  }

  if (t == TBI_LITERALS_NULL
      || t == TBI_ANY_ANY_REF) {
    t->flags |= TYPF_REF;
  }

  const struct typ *maybe_ref = tbi;
  if (functor != NULL) {
    maybe_ref = functor;

    t->flags |= functor->flags & TYPF__INHERIT_FROM_FUNCTOR;
  }

  if (maybe_ref == TBI_ANY_REF
      || maybe_ref == TBI_ANY_MUTABLE_REF
      || maybe_ref == TBI_ANY_NULLABLE_REF
      || maybe_ref == TBI_ANY_NULLABLE_MUTABLE_REF
      || maybe_ref == TBI_REF
      || maybe_ref == TBI_MREF
      || maybe_ref == TBI_MMREF
      || maybe_ref == TBI_NREF
      || maybe_ref == TBI_NMREF
      || maybe_ref == TBI_NMMREF
      || maybe_ref == TBI__REF_COMPATIBLE) {
    t->flags |= TYPF_REF;
  }

  if (tbi == NULL) {
    return;
  }

  t->flags |= TYPF_BUILTIN;

  if (tbi == TBI_LITERALS_NULL
      || tbi == TBI_LITERALS_INTEGER
      || tbi == TBI_LITERALS_FLOATING
      || tbi == TBI__REF_COMPATIBLE
      || tbi == TBI__NOT_TYPEABLE
      || tbi == TBI__CALL_FUNCTION_SLOT
      || tbi == TBI__MUTABLE
      || tbi == TBI__MERCURIAL) {
    t->flags |= TYPF_PSEUDO_BUILTIN;
  }

  if (functor != NULL && typ_equal(functor, TBI_TRIVIAL_ARRAY_CTOR)) {
    t->flags |= TYPF_TRIVIAL;
  }
  if (t->flags & TYPF_REF) {
    t->flags |= TYPF_TRIVIAL;
  }
  if (tbi == TBI_TRIVIAL_CTOR
      || tbi == TBI_TRIVIAL_COPY
      || tbi == TBI_TRIVIAL_EQUALITY
      || tbi == TBI_TRIVIAL_ORDER
      || tbi == TBI_TRIVIAL_DTOR
      || tbi == TBI_TRIVIAL_ARRAY_CTOR) {
    t->flags |= TYPF_TRIVIAL;
  }

  if (tbi == TBI_LITERALS_NULL
      || tbi == TBI_LITERALS_INTEGER
      || tbi == TBI_LITERALS_FLOATING) {
    t->flags |= TYPF_LITERAL;
  }

  if (tbi == TBI_BOOL
      || tbi == TBI_STATIC_STRING
      || tbi == TBI_STATIC_ARRAY) {
    t->flags |= TYPF_WEAKLY_CONCRETE;
  }
}

struct typ *typ_create(struct typ *tbi, struct node *definition) {
  struct typ *r = tbi != NULL ? tbi : calloc(1, sizeof(struct typ));

  r->definition = definition;
  create_flags(r, tbi);
  quickisa_init(r);

  return r;
}

void typ_create_update_genargs(struct typ *t) {
  if (typ_generic_arity(t) == 0) {
    return;
  }

  struct typ *t0 = typ_generic_functor(t);
  if (typ_is_tentative(t0)) {
    t->flags |= TYPF_TENTATIVE;
    add_user(t0, t);
  }
  for (size_t n = 0, count = typ_generic_arity(t); n < count; ++n) {
    struct typ *arg = typ_generic_arg(t, n);
    if (typ_is_tentative(arg)) {
      t->flags |= TYPF_TENTATIVE;
      add_user(arg, t);
    }
  }

}

void typ_create_update_hash(struct typ *t) {
  const struct node *d = typ_definition_const(t);
  const struct node *genargs = d->subs[IDX_GENARGS];

#define LEN 8 // arbitrary, at least 4
  uint32_t buf[LEN] = { 0 };
  buf[0] = t->flags & TYPF__MASK_HASH;
  buf[1] = node_ident(d);
  buf[2] = genargs->subs_count;
  size_t i = 3;
  for (size_t n = 0; n < genargs->subs_count && i < LEN; ++n, ++i) {
    buf[i] = node_ident(typ_definition_const(genargs->subs[n]->typ));
  }
#undef LEN

  t->hash = hash32_hsieh(buf, sizeof(buf));
}

static error update_quickisa_isalist_each(struct module *mod,
                                          struct typ *t, struct typ *intf,
                                          bool *stop, void *user) {
  typs_set_set(&t->quickisa, intf, TRUE);
  return 0;
}

void typ_create_update_quickisa(struct typ *t) {
  if (typs_set_count(&t->quickisa) > 0) {
    typs_set_destroy(&t->quickisa);
    quickisa_init(t);
  }

  typ_isalist_foreach(NULL, t, 0, update_quickisa_isalist_each, NULL);
}

bool typ_is_tentative(const struct typ *t) {
  return t->flags & TYPF_TENTATIVE;
}

static bool check_can_be_tentative(const struct typ *t) {
  const struct node *d = typ_definition_const(t);
  if (d->which == DEFINTF
      || d->which == DEFNAMEDLITERAL
      || d->which == DEFCONSTRAINTLITERAL
      || d->which == DEFUNKNOWNIDENT) {
    return TRUE;
  }

  if (typ_is_literal(t) || (t->flags & TYPF_WEAKLY_CONCRETE)) {
    return TRUE;
  }

  for (size_t n = 0, count = typ_generic_arity(t); n < count; ++n) {
    const struct node *da = typ_definition_const(typ_generic_arg_const(t, n));
    if (da->which == DEFINTF) {
      return TRUE;
    }
  }

  return FALSE;
}

struct typ *typ_create_tentative(struct typ *target) {
  assert(target != NULL);
  assert(target->hash != 0);

  if (typ_is_tentative(target)) {
    return target;
  }

  assert(check_can_be_tentative(target));

  struct typ *r = calloc(1, sizeof(struct typ));
  r->flags = target->flags | TYPF_TENTATIVE;
  r->hash = target->hash;
  r->definition = target->definition;
  quickisa_init(r);

  typs_set_rehash(&r->quickisa, (struct typs_set *) &target->quickisa);

  return r;
}

static void link_generic_arg_update(struct typ *user) {
  assert(typ_is_tentative(user));

  // It is possible that now that 'arg' is not tentative, 'user' itself
  // should loose its tentative status. This will be handled in
  // step_gather_final_instantiations().

  typ_create_update_hash(user);
  typ_create_update_quickisa(user);
}

static void link_to_final(struct typ *dst, struct typ *src) {
  FOREACH_BACKLINK(back, src, set_typ(back, dst));

  FOREACH_USER(user, src, link_generic_arg_update(user));

  // Noone should be referring to 'src' anymore; let's make sure.
  memset(src, 0, sizeof(*src));
}

static void link_to_tentative(struct typ *dst, struct typ *src) {
  FOREACH_BACKLINK(back, src, set_typ(back, dst));

  FOREACH_USER(user, src, add_user(dst, user));

  clear_backlinks(src);
  clear_users(src);

  // Noone should be referring to 'src' anymore; let's make sure.
  memset(src, 0, sizeof(*src));
}

void typ_link_tentative(struct typ *dst, struct typ *src) {
  assert(typ_is_tentative(src));

  if (dst == src) {
    return;
  }

  if (!typ_is_tentative(dst)) {
    link_to_final(dst, src);
  } else {
    link_to_tentative(dst, src);
  }
}

void typ_link_to_existing_final(struct typ *dst, struct typ *src) {
  assert(!typ_is_tentative(dst));

  if (dst == src) {
    return;
  }

  link_to_final(dst, src);
}

void typ_debug_check_in_backlinks(struct typ **u) {
  struct typ *t = *u;
  if (t == NULL || !typ_is_tentative(t)) {
    return;
  }
  bool r = FALSE;
  FOREACH_BACKLINK(b, t, if (b != NULL) { r |= b == u; });
  if (!r) __break();
}

struct node *typ_definition(struct typ *t) {
  return t->definition;
}

bool typ_is_function(const struct typ *t) {
  const struct node *def = typ_definition_const(t);
  return def->which == DEFFUN || def->which == DEFMETHOD;
}

struct typ *typ_generic_functor(struct typ *t) {
  if (typ_generic_arity(t) == 0) {
    return NULL;
  }

  const struct toplevel *toplevel = node_toplevel_const(typ_definition_const(t));
  if (toplevel->our_generic_functor_typ != NULL) {
    return toplevel->our_generic_functor_typ;
  } else {
    return t;
  }
}

EXAMPLE_NCC(typ_generic_functor) {
//  struct node *test = mock_deftype(mod, "test");
//  struct typ ttest = { 0 };
//  ttest.definition = test;
//  assert(typ_generic_functor(&ttest) == NULL);
}

size_t typ_generic_arity(const struct typ *t) {
  const struct node *d = typ_definition_const(t);
  if (node_can_have_genargs(d)) {
    return d->subs[IDX_GENARGS]->subs_count;
  } else {
    return 0;
  }
}

EXAMPLE_NCC(typ_generic_arity) {
//  {
//    struct node *test = mock_deftype(mod, "test");
//    struct typ ttest = { 0 };
//    ttest.definition = test;
//    assert(typ_generic_arity(&ttest) == 0);
//  }
//  {
//    struct node *test = mock_deftype(mod, "test2");
//    struct node *genargs = test->subs[IDX_GENARGS];
//    G(g1, genargs, DEFGENARG,
//       G_IDENT(name_g1, g1, "g1"));
//    G(g2, genargs, DEFGENARG,
//       G_IDENT(name_g2, g2, "g2"));
//
//    struct typ ttest = { 0 };
//    ttest.definition = test;
//    assert(typ_generic_arity(&ttest) == 2);
//  }
}

size_t typ_generic_first_explicit_arg(const struct typ *t) {
  return node_toplevel_const(typ_definition_const(t))->first_explicit_genarg;
}

struct typ *typ_generic_arg(struct typ *t, size_t n) {
  assert(n < typ_generic_arity(t));
  return typ_definition_const(t)->subs[IDX_GENARGS]->subs[n]->typ;
}

size_t typ_function_arity(const struct typ *t) {
  assert(typ_definition_const(t)->which == DEFFUN
         || typ_definition_const(t)->which == DEFMETHOD);
  return node_fun_all_args_count(typ_definition_const(t));
}

struct typ *typ_function_arg(struct typ *t, size_t n) {
  assert(n < typ_function_arity(t));
  return typ_definition_const(t)->subs[IDX_FUNARGS]->subs[n]->typ;
}

struct typ *typ_function_return(struct typ *t) {
  assert(typ_definition_const(t)->which == DEFFUN
         || typ_definition_const(t)->which == DEFMETHOD);
  return node_fun_retval_const(typ_definition_const(t))->typ;
}

const struct node *typ_definition_const(const struct typ *t) {
  return typ_definition((struct typ *) t);
}

const struct typ *typ_generic_functor_const(const struct typ *t) {
  return typ_generic_functor((struct typ *) t);
}

const struct typ *typ_generic_arg_const(const struct typ *t, size_t n) {
  return typ_generic_arg((struct typ *) t, n);
}

const struct typ *typ_function_arg_const(const struct typ *t, size_t n) {
  return typ_function_arg((struct typ *) t, n);
}

const struct typ *typ_function_return_const(const struct typ *t) {
  return typ_function_return((struct typ *) t);
}

static size_t direct_isalist_count(const struct typ *t) {
  const struct node *def = typ_definition_const(t);
  switch (def->which) {
  case DEFTYPE:
  case DEFINTF:
    return def->subs[IDX_ISALIST]->subs_count;
  default:
    return 0;
  }
}

static struct typ *direct_isalist(struct typ *t, size_t n) {
  const struct node *def = typ_definition_const(t);
  switch (def->which) {
  case DEFTYPE:
  case DEFINTF:
    assert(n < def->subs[IDX_ISALIST]->subs_count);
    return def->subs[IDX_ISALIST]->subs[n]->typ;
  default:
    assert(FALSE);
    return 0;
  }
}

static const struct typ *direct_isalist_const(const struct typ *t, size_t n) {
  return direct_isalist((struct typ *) t, n);
}

static bool direct_isalist_exported(const struct typ *t, size_t n) {
  const struct node *def = typ_definition_const(t);
  switch (def->which) {
  case DEFTYPE:
  case DEFINTF:
    return def->subs[IDX_ISALIST]->subs[n]->as.ISA.is_export;
  default:
    return 0;
  }
}

static error do_typ_isalist_foreach(struct module *mod, struct typ *t, struct typ *base,
                                    uint32_t filter, isalist_each iter, void *user,
                                    bool *stop, struct typs_set *set) {
  for (size_t n = 0; n < direct_isalist_count(base); ++n) {
    struct typ *intf = direct_isalist(base, n);
    if (typs_set_get(set, intf) != NULL) {
      continue;
    }

    const bool filter_not_exported = filter & ISALIST_FILTER_NOT_EXPORTED;
    const bool filter_exported = filter & ISALIST_FILTER_EXPORTED;
    const bool filter_trivial_isalist = filter & ISALIST_FILTER_TRIVIAL_ISALIST;
    const bool exported = direct_isalist_exported(base, n);
    if (filter_not_exported && !exported) {
      continue;
    }
    if (filter_exported && exported) {
      continue;
    }
    if (filter_trivial_isalist && typ_is_trivial(intf)) {
      continue;
    }

    typs_set_set(set, intf, TRUE);

    error e = do_typ_isalist_foreach(mod, t, intf, filter, iter, user, stop, set);
    EXCEPT(e);

    if (*stop) {
      return 0;
    }

    e = iter(mod, t, intf, stop, user);
    EXCEPT(e);

    if (*stop) {
      return 0;
    }
  }

  return 0;
}

error typ_isalist_foreach(struct module *mod, struct typ *t, uint32_t filter,
                          isalist_each iter, void *user) {
  struct typs_set set;
  typs_set_init(&set, 0);
  typs_set_set_delete_val(&set, NULL);
  typs_set_set_custom_hashf(&set, typ_hash);
  typs_set_set_custom_cmpf(&set, typ_cmp);

  bool stop = FALSE;
  error e = do_typ_isalist_foreach(mod, t, t, filter, iter, user, &stop, &set);
  typs_set_destroy(&set);
  EXCEPT(e);

  return 0;
}

static error example_isalist_each(struct module *mod, struct typ *base,
                                  struct typ *intf, bool *stop, void *user) {
  assert(base != intf);
  assert(!typ_equal(base, intf));
  assert(typ_isa(base, intf));

  int *count = user;
  *count += 1;

  return 0;
}

EXAMPLE_NCC(direct_isalist_foreach) {
//  struct node *i_test = mock_defintf(mod, "i_test");
//  struct node *test = mock_deftype(mod, "test");
//  G(isa, test->subs[IDX_ISALIST], ISA,
//     G_IDENT(isa_name, isa, "i_test"));
//
//  i_test->typ = typ_create(NULL, i_test);
//  test->typ = typ_create(NULL, test);
//  set_typ(&isa_name->typ, i_test->typ);
//  set_typ(&isa->typ, isa_name->typ);
//
//  int count = 0;
//  error e = typ_isalist_foreach(mod, test->typ, 0, example_isalist_each, &count);
//  assert(!e);
//  assert(count == 1);
}

struct typ *TBI_VOID;
struct typ *TBI_LITERALS_NULL;
struct typ *TBI_LITERALS_INTEGER;
struct typ *TBI_LITERALS_FLOATING;
struct typ *TBI_ANY_TUPLE;
struct typ *TBI_TUPLE_2;
struct typ *TBI_TUPLE_3;
struct typ *TBI_TUPLE_4;
struct typ *TBI_TUPLE_5;
struct typ *TBI_TUPLE_6;
struct typ *TBI_TUPLE_7;
struct typ *TBI_TUPLE_8;
struct typ *TBI_TUPLE_9;
struct typ *TBI_TUPLE_10;
struct typ *TBI_TUPLE_11;
struct typ *TBI_TUPLE_12;
struct typ *TBI_TUPLE_13;
struct typ *TBI_TUPLE_14;
struct typ *TBI_TUPLE_15;
struct typ *TBI_TUPLE_16;
struct typ *TBI_ANY;
struct typ *TBI_BOOL;
struct typ *TBI_BOOL_COMPATIBLE;
struct typ *TBI_I8;
struct typ *TBI_U8;
struct typ *TBI_I16;
struct typ *TBI_U16;
struct typ *TBI_I32;
struct typ *TBI_U32;
struct typ *TBI_I64;
struct typ *TBI_U64;
struct typ *TBI_SIZE;
struct typ *TBI_SSIZE;
struct typ *TBI_FLOAT;
struct typ *TBI_DOUBLE;
struct typ *TBI_CHAR;
struct typ *TBI_STRING;
struct typ *TBI_STATIC_STRING;
struct typ *TBI_STATIC_STRING_COMPATIBLE;
struct typ *TBI_STATIC_ARRAY;
struct typ *TBI__REF_COMPATIBLE;
struct typ *TBI_ANY_ANY_REF;
struct typ *TBI_ANY_REF;
struct typ *TBI_ANY_MUTABLE_REF;
struct typ *TBI_ANY_NULLABLE_REF;
struct typ *TBI_ANY_NULLABLE_MUTABLE_REF;
struct typ *TBI_REF; // @
struct typ *TBI_MREF; // @!
struct typ *TBI_MMREF; // @#
struct typ *TBI_NREF; // ?@
struct typ *TBI_NMREF; // ?@!
struct typ *TBI_NMMREF; // ?@#
struct typ *TBI_ARITHMETIC;
struct typ *TBI_BITWISE;
struct typ *TBI_INTEGER;
struct typ *TBI_UNSIGNED_INTEGER;
struct typ *TBI_NATIVE_INTEGER;
struct typ *TBI_NATIVE_ANYSIGN_INTEGER;
struct typ *TBI_GENERALIZED_BOOLEAN;
struct typ *TBI_NATIVE_BOOLEAN;
struct typ *TBI_FLOATING;
struct typ *TBI_NATIVE_FLOATING;
struct typ *TBI_HAS_EQUALITY;
struct typ *TBI_ORDERED;
struct typ *TBI_ORDERED_BY_COMPARE;
struct typ *TBI_COPYABLE;
struct typ *TBI_DEFAULT_CTOR;
struct typ *TBI_CTOR_WITH;
struct typ *TBI_ARRAY_CTOR;
struct typ *TBI_TRIVIAL_COPY;
struct typ *TBI_TRIVIAL_CTOR;
struct typ *TBI_TRIVIAL_ARRAY_CTOR;
struct typ *TBI_TRIVIAL_DTOR;
struct typ *TBI_TRIVIAL_EQUALITY;
struct typ *TBI_TRIVIAL_ORDER;
struct typ *TBI_RETURN_BY_COPY;
struct typ *TBI_SUM_COPY;
struct typ *TBI_SUM_EQUALITY;
struct typ *TBI_SUM_ORDER;
struct typ *TBI_ITERATOR;
struct typ *TBI__NOT_TYPEABLE;
struct typ *TBI__CALL_FUNCTION_SLOT;
struct typ *TBI__MUTABLE;
struct typ *TBI__MERCURIAL;

static bool __typ_equal(const struct typ *a, const struct typ *b) {
  assert(a->hash != 0 && b->hash != 0);

  const struct node *da = typ_definition_const(a);
  const struct node *db = typ_definition_const(b);
  if (da == db) {
    return TRUE;
  }

  const size_t a_ga = typ_generic_arity(a);
  if (a_ga == 0) {
    return da == db;
  } else {
    if (a_ga != typ_generic_arity(b)) {
      return FALSE;
    }

    const struct typ *a0 = typ_generic_functor_const(a);
    const struct typ *b0 = typ_generic_functor_const(b);
    if (a0 != b0) {
      return FALSE;
    }

    const bool a_functor = typ_is_generic_functor(a);
    const bool b_functor = typ_is_generic_functor(b);
    if (a_functor && b_functor) {
      return TRUE;
    } else if (a_functor || b_functor) {
      return FALSE;
    }

    for (size_t n = 0; n < a_ga; ++n) {
      if (!typ_equal(typ_generic_arg_const(a, n),
                     typ_generic_arg_const(b, n))) {
        return FALSE;
      }
    }

    return TRUE;
  }
}

bool typ_equal(const struct typ *a, const struct typ *b) {
  if (a->hash != b->hash) {
    return FALSE;
  }

  return __typ_equal(a, b);
}

EXAMPLE_NCC(typ_equal) {
//  struct node *di = mock_defintf(mod, "i");
//  struct typ *i = typ_create(NULL, di);
//  di->typ = i;
//
//  struct node *da = mock_deftype(mod, "a");
//  struct node *db = mock_deftype(mod, "b");
//
//  da->typ = typ_create(NULL, da);
//  struct typ *alt_a = typ_create(NULL, da);
//  db->typ = typ_create(NULL, db);
//
//  assert(typ_equal(da->typ, da->typ));
//  assert(typ_equal(da->typ, alt_a));
//  assert(typ_equal(alt_a, da->typ));
//
//  assert(!typ_equal(da->typ, db->typ));
//  assert(!typ_equal(db->typ, da->typ));
//
//  struct node *dc = mock_deftype(mod, "dc");
//  {
//    G(g, dc->subs[IDX_GENARGS], DEFGENARG,
//       G_IDENT(g_name, g, "g");
//       G_IDENT(g_type, g, "i"));
//    dc->typ = typ_create(NULL, dc);
//    g_type->typ = i;
//    g->typ = i;
//    assert(typ_generic_arity(dc->typ) == 1);
//  }
//
//  struct node *dd = mock_deftype(mod, "dd");
//  {
//    G(g, dd->subs[IDX_GENARGS], SETGENARG,
//       G_IDENT(g_name, g, "g");
//       G_IDENT(g_type, g, "i"));
//    dd->typ = typ_create(NULL, dd);
//    node_toplevel(dd)->our_generic_functor_typ = dc->typ;
//    g_type->typ = i;
//    g->typ = i;
//    assert(typ_generic_arity(dd->typ) == 1);
//  }
//
//  assert(typ_equal(typ_generic_functor(dc->typ), typ_generic_functor(dd->typ)));
//  assert(!typ_equal(dc->typ, dd->typ));
}

error typ_check_equal(const struct module *mod, const struct node *for_error,
                      const struct typ *a, const struct typ *b) {
  if (typ_equal(a, b)) {
    return 0;
  }

  error e = 0;
  char *na = typ_pretty_name(mod, a);
  char *nb = typ_pretty_name(mod, b);
  GOTO_EXCEPT_TYPE(try_node_module_owner_const(mod, for_error), for_error,
                   "'%s' not equal to type '%s'", na, nb);
except:
  free(nb);
  free(na);
  return e;
}

struct typ *typ_lookup_builtin_tuple(struct module *mod, size_t arity) {
  switch (arity) {
  case 2: return TBI_TUPLE_2;
  case 3: return TBI_TUPLE_3;
  case 4: return TBI_TUPLE_4;
  case 5: return TBI_TUPLE_5;
  case 6: return TBI_TUPLE_6;
  case 7: return TBI_TUPLE_7;
  case 8: return TBI_TUPLE_8;
  case 9: return TBI_TUPLE_9;
  case 10: return TBI_TUPLE_10;
  case 11: return TBI_TUPLE_11;
  case 12: return TBI_TUPLE_12;
  case 13: return TBI_TUPLE_13;
  case 14: return TBI_TUPLE_14;
  case 15: return TBI_TUPLE_15;
  case 16: return TBI_TUPLE_16;
  default: assert(FALSE); return NULL;
  }
}

bool typ_has_same_generic_functor(const struct module *mod,
                                  const struct typ *a, const struct typ *b) {
  const size_t a_arity = typ_generic_arity(a);
  const size_t b_arity = typ_generic_arity(b);

  if (a_arity > 0 && b_arity > 0) {
    return typ_equal(typ_generic_functor_const(a), typ_generic_functor_const(b));
  } else if (a_arity == 0 && b_arity > 0) {
    return typ_equal(a, typ_generic_functor_const(b));
  } else if (a_arity > 0 && b_arity == 0) {
    return typ_equal(typ_generic_functor_const(a), b);
  } else {
    return FALSE;
  }
}

bool typ_is_generic_functor(const struct typ *t) {
  return typ_generic_arity(t) > 0
    && typ_generic_functor_const(t) == t;
}

bool typ_isa(const struct typ *a, const struct typ *intf) {
  if (typ_equal(intf, TBI_ANY)) {
    return TRUE;
  }

  if (typ_equal(a, intf)) {
    return TRUE;
  }

  const size_t a_ga = typ_generic_arity(a);
  if (a_ga > 0
      && !typ_is_generic_functor(a)
      && typ_is_generic_functor(intf)) {
    if (typ_isa(typ_generic_functor_const(a), intf)) {
      return TRUE;
    }
  }

  if (a_ga > 0
      && !typ_equal(intf, TBI_ANY_TUPLE)
      && typ_isa(a, TBI_ANY_TUPLE)) {
    // FIXME: only valid for certain builtin interfaces (copy, trivial...)
    size_t n;
    for (n = 0; n < a_ga; ++n) {
      if (!typ_isa(typ_generic_arg_const(a, n), intf)) {
        break;
      }
    }
    if (n == a_ga) {
      return TRUE;
    }
  }

  if (a_ga > 0
      && a_ga == typ_generic_arity(intf)
      && typ_equal(typ_generic_functor_const(a),
                   typ_generic_functor_const(intf))) {
    size_t n = 0;
    for (n = 0; n < a_ga; ++n) {
      if (!typ_isa(typ_generic_arg_const(a, n),
                   typ_generic_arg_const(intf, n))) {
        break;
      }
    }
    if (n == a_ga) {
      return TRUE;
    }
  }

  // Literals types do not have a isalist. FIXME: Why not?
  if (typ_equal(a, TBI_LITERALS_INTEGER)) {
    return typ_isa(TBI_INTEGER, intf);
  } else if (typ_equal(a, TBI_LITERALS_NULL)) {
    return typ_isa(TBI_NREF, intf);
  } else if (typ_equal(a, TBI_LITERALS_FLOATING)) {
    return typ_isa(TBI_FLOATING, intf);
  }

  for (size_t n = 0; n < direct_isalist_count(a); ++n) {
    if (typ_equal(direct_isalist_const(a, n), intf)) {
      return TRUE;
    }
  }

  for (size_t n = 0; n < direct_isalist_count(a); ++n) {
    if (typ_isa(direct_isalist_const(a, n), intf)) {
      return TRUE;
    }
  }

  return FALSE;
}

error typ_check_isa(const struct module *mod, const struct node *for_error,
                    const struct typ *a, const struct typ *intf) {
  if (typ_isa(a, intf)) {
    return 0;
  }

  error e = 0;
  char *na = typ_pretty_name(mod, a);
  char *nintf = typ_pretty_name(mod, intf);
  GOTO_EXCEPT_TYPE(try_node_module_owner_const(mod, for_error), for_error,
                   "'%s' not isa intf '%s'", na, nintf);
except:
  free(nintf);
  free(na);
  return e;
}

bool typ_is_reference(const struct typ *t) {
  return t->flags & TYPF_REF;
}

bool typ_is_dyn(const struct typ *t) {
  return !typ_is_tentative(t)
    && typ_is_reference(t)
    && typ_generic_arity(t) > 0
    && typ_definition_const(typ_generic_arg_const(t, 0))->which == DEFINTF;
}

error typ_check_is_reference(const struct module *mod, const struct node *for_error,
                                      const struct typ *a) {
  if (typ_is_reference(a)) {
    return 0;
  }

  error e = 0;
  char *na = typ_pretty_name(mod, a);
  GOTO_EXCEPT_TYPE(try_node_module_owner_const(mod, for_error), for_error,
                   "'%s' type is not a reference", na);
except:
  free(na);
  return e;
}

static const char *string_for_ref[TOKEN__NUM] = {
  [TREFDOT] = "@",
  [TREFBANG] = "@!",
  [TREFSHARP] = "@#",
  [TDEREFDOT] = ".",
  [TDEREFBANG] = "!",
  [TDEREFSHARP] = "#",
  [TNULREFDOT] = "?@",
  [TNULREFBANG] = "?@!",
  [TNULREFSHARP] = "?@#",
};

error typ_check_can_deref(const struct module *mod, const struct node *for_error,
                          const struct typ *a, enum token_type operator) {
  error e = 0;
  char *na = NULL;
  if (!typ_is_reference(a)) {
    na = typ_pretty_name(mod, a);
    GOTO_EXCEPT_TYPE(try_node_module_owner_const(mod, for_error), for_error,
                     "'%s' type is not a reference", na);
  }

  bool ok = FALSE;
  switch (operator) {
  case TDEREFDOT:
  case TDEREFWILDCARD:
    ok = TRUE;
    break;
  case TDEREFBANG:
    ok = typ_isa(a, TBI_ANY_MUTABLE_REF);
    break;
  case TDEREFSHARP:
    ok = typ_has_same_generic_functor(mod, a, TBI_MMREF);
    break;
  default:
    assert(FALSE);
  }
  if (ok) {
    return 0;
  }

  na = typ_pretty_name(mod, a);
  GOTO_EXCEPT_TYPE(try_node_module_owner_const(mod, for_error), for_error,
                   "'%s' type cannot be dereferenced with '%s'",
                   na, string_for_ref[operator]);
except:
  free(na);
  return e;
}

error typ_check_deref_against_mark(const struct module *mod, const struct node *for_error,
                                   const struct typ *t, enum token_type operator) {
  if (t == NULL) {
    return 0;
  }

  const char *kind = NULL;
  if (typ_equal(t, TBI__MUTABLE)) {
    if (operator != TDOT && operator != TDEREFDOT && operator != TDEREFWILDCARD) {
      return 0;
    }
    kind = "mutable";
  } else if (typ_equal(t, TBI__MERCURIAL)) {
    if (operator == TSHARP || operator == TDEREFSHARP
        || operator == TWILDCARD || operator == TDEREFWILDCARD) {
      return 0;
    }
    kind = "mercurial";
  } else {
    return 0;
  }

  error e = 0;
  char *nt = typ_pretty_name(mod, t);
  GOTO_EXCEPT_TYPE(try_node_module_owner_const(mod, for_error), for_error,
                   "'%s' type cannot be dereferenced with '%s' in a %s context",
                   nt, token_strings[operator], kind);
except:
  free(nt);
  return e;
}

bool typ_is_builtin(const struct module *mod, const struct typ *t) {
  return t->flags & TYPF_BUILTIN;
}

bool typ_is_pseudo_builtin(const struct typ *t) {
  return t->flags & TYPF_PSEUDO_BUILTIN;
}

bool typ_is_trivial(const struct typ *t) {
  return t->flags & TYPF_TRIVIAL;
}

bool typ_is_literal(const struct typ *t) {
  return t->flags & TYPF_LITERAL;
}

bool typ_is_weakly_concrete(const struct typ *t) {
  return (t->flags & TYPF_WEAKLY_CONCRETE) && typ_is_tentative(t);
}

bool typ_isa_return_by_copy(const struct typ *t) {
  return typ_isa(t, TBI_RETURN_BY_COPY);
}
