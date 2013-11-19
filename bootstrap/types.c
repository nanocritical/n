#include "types.h"

#include <stdio.h>
#include "table.h"
#include "mock.h"

struct typ {
  struct typ *link;
  struct node *definition;
};

void typ_pprint(const struct module *mod, const struct typ *t) {
  fprintf(g_env.stderr, "%s\n", typ_pretty_name(mod, t));
}

struct typ *typ_create(struct node *definition) {
  struct typ *r = calloc(1, sizeof(struct typ));
  r->definition = definition;
  return r;
}

EXAMPLE(typ_create) {
  assert(typ_create(NULL)->definition == NULL);
  struct node n = { 0 };
  assert(typ_create(&n)->definition == &n);
}

struct typ *typ_init_tbi(struct typ *tbi, struct node *definition) {
  tbi->definition = definition;
  return tbi;
}

EXAMPLE(typ_create_tbi) {
  struct typ t = { 0 };
  struct node n = { 0 };
  struct typ *tt = typ_init_tbi(&t, &n);
  assert(tt == &t && tt->definition == &n);
}

bool typ_is_link(const struct typ *t) {
  return t->link != NULL;
}

EXAMPLE(typ_is_link) {
  struct typ t = { .link = NULL };
  assert(!typ_is_link(&t));
  struct typ s = { .link = &t };
  assert(typ_is_link(&s));
}

struct typ *typ_create_link(struct typ *dst) {
  assert(dst != NULL);
  struct typ *r = calloc(1, sizeof(struct typ));
  r->link = dst;
  return r;
}

EXAMPLE(typ_create_link) {
  struct node n = { 0 };
  struct typ t = { .definition = &n };
  struct typ *l = typ_create_link(&t);
  assert(typ_is_link(l));
  assert(l->link == &t);
  assert(typ_follow(l) == &t);
  assert(typ_definition(l) == &n);
}

void typ_link(struct typ *dst, struct typ *src) {
  if (dst == src
      || (dst->definition != NULL
          && src->definition != NULL
          && dst->definition == src->definition)) {
    return;
  }

  assert(typ_is_link(src));

  struct typ *d = dst;
  while (d->link != NULL && d->link->link != NULL) {
    d = d->link;
    if (d == src || d->link == src) {
      return;
    }
  }

  while (src->link != NULL && src->link->link != NULL) {
    src = src->link;
    if (src == d) {
      return;
    }
  }

  src->link = dst;

  // FIXME: remove; used to detect circular links.
  (void) typ_follow(src);
}

EXAMPLE(typ_link) {
  struct node d1 = { 0 }, d2 = { 0 };
  struct typ *a = typ_create(&d1);
  struct typ *b = typ_create(&d2);
  struct typ *lnk_a = typ_create_link(a);
  struct typ *lnk_b = typ_create_link(b);
  assert(typ_follow(lnk_a) != typ_follow(lnk_b));
  typ_link(lnk_a, lnk_b);
  assert(typ_is_link(lnk_a));
  assert(typ_is_link(lnk_b));
  assert(typ_follow(lnk_a) == typ_follow(lnk_b));
}

struct typ *typ_follow(struct typ *t) {
  if (t == NULL) {
    return NULL;
  }

  while (t->link != NULL) {
    t = t->link;
  }
  return t;
}

EXAMPLE(typ_follow) {
  assert(NULL == typ_follow(NULL));
  struct node n = { 0 };
  struct typ t = { .definition = &n };
  assert(&t == typ_follow(&t));
  struct typ s = { .link = &t };
  assert(&t == typ_follow(&s));
}

size_t typ_link_length(const struct typ *t) {
  if (t == NULL) {
    return 0;
  }

  size_t len = 0;
  while (t->link != NULL) {
    t = t->link;
    len += 1;
  }
  return len;
}

EXAMPLE(typ_link_length) {
  assert(0 == typ_link_length(NULL));
  struct typ t = { 0 };
  assert(0 == typ_link_length(&t));
  struct typ s = { .link = &t };
  assert(1 == typ_link_length(&s));
}

void typ_lock(struct typ *t) {
  t->definition = typ_follow_const(t)->definition;
  t->link = NULL;
}

EXAMPLE(typ_lock) {
  struct node n = { 0 };
  struct typ s = { .definition = &n };
  struct typ t = { .link = &s };
  typ_lock(&t);
  assert(!typ_is_link(&t));
  assert(typ_definition_const(&t) == typ_definition_const(&s));
}

struct node *typ_definition(struct typ *t) {
  t = typ_follow(t);

  return t->definition;
}

bool typ_is_function(const struct typ *t) {
  const struct node *def = typ_definition_const(t);
  return def->which == DEFFUN || def->which == DEFMETHOD;
}

struct typ *typ_generic_functor(struct typ *t) {
  t = typ_follow(t);

  if (typ_generic_arity(t) == 0) {
    return NULL;
  }

  const struct toplevel *toplevel = node_toplevel_const(t->definition);
  if (toplevel->our_generic_functor != NULL) {
    return toplevel->our_generic_functor;
  } else {
    return t;
  }
}

EXAMPLE_NCC(typ_generic_functor) {
  struct node *test = mock_deftype(mod, "test");
  struct typ ttest = { .definition = test };
  assert(typ_generic_functor(&ttest) == NULL);
}

size_t typ_generic_arity(const struct typ *t) {
  t = typ_follow_const(t);

  if (node_can_have_genargs(t->definition)) {
    return t->definition->subs[IDX_GENARGS]->subs_count;
  } else {
    return 0;
  }
}

EXAMPLE_NCC(typ_generic_arity) {
  {
    struct node *test = mock_deftype(mod, "test");
    struct typ ttest = { .definition = test };
    assert(typ_generic_arity(&ttest) == 0);
  }
  {
    struct node *test = mock_deftype(mod, "test2");
    struct node *genargs = test->subs[IDX_GENARGS];
    MK(g1, genargs, DEFGENARG,
       MK_IDENT(name_g1, g1, "g1"));
    MK(g2, genargs, DEFGENARG,
       MK_IDENT(name_g2, g2, "g2"));

    struct typ ttest = { .definition = test };
    assert(typ_generic_arity(&ttest) == 2);
  }
}

size_t typ_generic_first_explicit_arg(const struct typ *t) {
  t = typ_follow_const(t);

  return node_toplevel_const(t->definition)->first_explicit_genarg;
}

struct typ *typ_generic_arg(struct typ *t, size_t n) {
  t = typ_follow(t);

  assert(n < typ_generic_arity(t));
  return t->definition->subs[IDX_GENARGS]->subs[n]->typ;
}

size_t typ_function_arity(const struct typ *t) {
  t = typ_follow_const(t);

  assert(t->definition->which == DEFFUN || t->definition->which == DEFMETHOD);
  return node_fun_all_args_count(t->definition);
}

struct typ *typ_function_arg(struct typ *t, size_t n) {
  t = typ_follow(t);

  assert(n < typ_function_arity(t));
  return t->definition->subs[IDX_FUNARGS]->subs[n]->typ;
}

struct typ *typ_function_return(struct typ *t) {
  t = typ_follow(t);

  assert(t->definition->which == DEFFUN || t->definition->which == DEFMETHOD);
  return node_fun_retval(t->definition)->typ;
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

const struct typ *typ_follow_const(const struct typ *t) {
  return typ_follow((struct typ *) t);
}

size_t typ_isalist_count(const struct typ *t) {
  t = typ_follow_const(t);

  struct node *def = t->definition;
  if (def->which == DEFTYPE) {
    return def->as.DEFTYPE.isalist.count;
  } else if (def->which == DEFINTF) {
    return def->as.DEFINTF.isalist.count;
  } else {
    return 0;
  }
}

struct typ *typ_isalist(struct typ *t, size_t n) {
  t = typ_follow(t);

  struct node *def = t->definition;
  struct typ **list = NULL;
  if (def->which == DEFTYPE) {
    list = def->as.DEFTYPE.isalist.list;
  } else if (def->which == DEFINTF) {
    list = def->as.DEFINTF.isalist.list;
  } else {
    assert(FALSE);
  }

  assert(n < typ_isalist_count(t));
  return list[n];
}

const struct typ *typ_isalist_const(const struct typ *t, size_t n) {
  return typ_isalist((struct typ *) t, n);
}

const bool *typ_isalist_exported(const struct typ *t) {
  t = typ_follow_const(t);

  struct node *def = t->definition;
  if (def->which == DEFTYPE) {
    return def->as.DEFTYPE.isalist.exported;
  } else if (def->which == DEFINTF) {
    return def->as.DEFINTF.isalist.exported;
  } else {
    return NULL;
  }
}

HTABLE_SPARSE(typs_set, bool, struct typ *);
implement_htable_sparse(__attribute__((unused)) static, typs_set, bool, struct typ *);

static uint32_t typ_hash(const struct typ **a) {
  return node_ident(typ_follow_const(*a)->definition);;
}

static int typ_cmp(const struct typ **a, const struct typ **b) {
  const struct typ *aa = typ_follow_const(*a);
  const struct typ *bb = typ_follow_const(*b);
  return !typ_equal(aa, bb);
}

static error do_typ_isalist_foreach(struct module *mod, struct typ *t, struct typ *base,
                                    uint32_t filter, isalist_each iter, void *user,
                                    bool *stop, struct typs_set *set) {
  t = typ_follow(t);
  base = typ_follow(base);

  for (size_t n = 0; n < typ_isalist_count(base); ++n) {
    struct typ *intf = typ_follow(typ_isalist(base, n));
    if (typs_set_get(set, intf) != NULL) {
      continue;
    }

    const bool filter_not_exported = filter & ISALIST_FILTER_NOT_EXPORTED;
    const bool filter_exported = filter & ISALIST_FILTER_EXPORTED;
    const bool filter_trivial_isalist = filter & ISALIST_FILTER_TRIVIAL_ISALIST;
    const bool exported = typ_isalist_exported(base)[n];
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

EXAMPLE_NCC(typ_isalist_foreach) {
  struct node *i_test = mock_defintf(mod, "i_test");
  struct node *test = mock_deftype(mod, "test");
  MK(isa, test->subs[IDX_ISALIST], ISA,
     MK_IDENT(isa_name, isa, "i_test"));

  i_test->typ = typ_create(i_test);
  test->typ = typ_create(test);
  isa_name->typ = typ_create_link(i_test->typ);
  isa->typ = isa_name->typ;

  test->as.DEFTYPE.isalist.count = 1;
  test->as.DEFTYPE.isalist.list = &i_test->typ;
  bool no = FALSE;
  test->as.DEFTYPE.isalist.exported = &no;

  int count = 0;
  error e = typ_isalist_foreach(mod, test->typ, 0, example_isalist_each, &count);
  assert(!e);
  assert(count == 1);
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
struct typ *TBI_REF_COMPATIBLE;
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

bool typ_equal(const struct typ *a, const struct typ *b) {
  a = typ_follow_const(a);
  b = typ_follow_const(b);

  if (typ_generic_arity(a) == 0) {
    return typ_definition_const(typ_follow_const(a))
      == typ_definition_const(typ_follow_const(b));
  } else {
    if (typ_generic_arity(a) != typ_generic_arity(b)) {
      return FALSE;
    }

    if (typ_definition_const(typ_generic_functor_const(a))
        != typ_definition_const(typ_generic_functor_const(b))) {
      return FALSE;
    }

    const bool a_functor = typ_is_generic_functor(a);
    const bool b_functor = typ_is_generic_functor(b);
    if (a_functor && b_functor) {
      return TRUE;
    } else if (a_functor || b_functor) {
      return FALSE;
    }

    for (size_t n = 0; n < typ_generic_arity(a); ++n) {
      if (!typ_equal(typ_generic_arg_const(a, n),
                     typ_generic_arg_const(b, n))) {
        return FALSE;
      }
    }

    return TRUE;
  }
}

EXAMPLE_NCC(typ_equal) {
  struct node *di = mock_defintf(mod, "i");
  struct typ *i = typ_create(di);
  di->typ = i;

  struct node *da = mock_deftype(mod, "a");
  struct node *db = mock_deftype(mod, "b");

  da->typ = typ_create(da);
  struct typ *alt_a = typ_create(da);
  db->typ = typ_create(db);

  assert(typ_equal(da->typ, da->typ));
  assert(typ_equal(da->typ, alt_a));
  assert(typ_equal(alt_a, da->typ));
  struct typ *a = typ_create_link(da->typ);
  assert(typ_equal(da->typ, a));
  assert(typ_equal(a, da->typ));

  assert(!typ_equal(da->typ, db->typ));
  assert(!typ_equal(db->typ, da->typ));

  struct node *dc = mock_deftype(mod, "dc");
  {
    MK(g, dc->subs[IDX_GENARGS], DEFGENARG,
       MK_IDENT(g_name, g, "g");
       MK_IDENT(g_type, g, "i"));
    dc->typ = typ_create(dc);
    g_type->typ = i;
    g->typ = i;
    assert(typ_generic_arity(dc->typ) == 1);
  }

  struct node *dd = mock_deftype(mod, "dd");
  {
    MK(g, dd->subs[IDX_GENARGS], SETGENARG,
       MK_IDENT(g_name, g, "g");
       MK_IDENT(g_type, g, "i"));
    dd->typ = typ_create(dd);
    node_toplevel(dd)->our_generic_functor = dc->typ;
    g_type->typ = i;
    g->typ = i;
    assert(typ_generic_arity(dd->typ) == 1);
  }

  assert(typ_equal(typ_generic_functor(dc->typ), typ_generic_functor(dd->typ)));
  assert(!typ_equal(dc->typ, dd->typ));
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
  a = typ_follow_const(a);
  b = typ_follow_const(b);

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
  t = typ_follow_const(t);
  return typ_generic_arity(t) > 0
    && typ_generic_functor_const(t) == t;
}

bool typ_isa(const struct typ *a, const struct typ *intf) {
  a = typ_follow_const(a);
  intf = typ_follow_const(intf);

  if (typ_equal(intf, TBI_ANY)) {
    return TRUE;
  }

  if (typ_equal(a, intf)) {
    return TRUE;
  }

  if (typ_generic_arity(a) > 0
      && !typ_is_generic_functor(a)
      && typ_is_generic_functor(intf)) {
    if (typ_isa(typ_generic_functor_const(a), intf)) {
      return TRUE;
    }
  }

  if (typ_generic_arity(a) > 0
      && !typ_equal(intf, TBI_ANY_TUPLE)
      && typ_isa(a, TBI_ANY_TUPLE)) {
    // FIXME: only valid for certain builtin interfaces (copy, trivial...)
    size_t n;
    for (n = 0; n < typ_generic_arity(a); ++n) {
      if (!typ_isa(typ_generic_arg_const(a, n), intf)) {
        break;
      }
    }
    if (n == typ_generic_arity(a)) {
      return TRUE;
    }
  }

  if (typ_generic_arity(a) > 0
      && typ_generic_arity(a) == typ_generic_arity(intf)
      && typ_equal(typ_generic_functor_const(a),
                   typ_generic_functor_const(intf))) {
    size_t n = 0;
    for (n = 0; n < typ_generic_arity(a); ++n) {
      if (!typ_isa(typ_generic_arg_const(a, n),
                   typ_generic_arg_const(intf, n))) {
        break;
      }
    }
    if (n == typ_generic_arity(a)) {
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

  for (size_t n = 0; n < typ_isalist_count(a); ++n) {
    if (typ_equal(typ_isalist_const(a, n), intf)) {
      return TRUE;
    }
  }

  for (size_t n = 0; n < typ_isalist_count(a); ++n) {
    if (typ_isa(typ_isalist_const(a, n), intf)) {
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

bool typ_is_reference_instance(const struct typ *a) {
  a = typ_follow_const(a);

  return typ_isa(a, TBI_ANY_ANY_REF)
    || (typ_generic_arity(a) > 0
        && typ_equal(typ_generic_functor_const(a), TBI_REF_COMPATIBLE));
}

error typ_check_is_reference_instance(const struct module *mod, const struct node *for_error,
                                      const struct typ *a) {
  a = typ_follow_const(a);

  if (typ_is_reference_instance(a)) {
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
  a = typ_follow_const(a);

  error e = 0;
  char *na = NULL;
  if (!typ_is_reference_instance(a)) {
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

  t = typ_follow_const(t);

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
  t = typ_follow_const(t);

  for (size_t n = ID_TBI__FIRST; n <= ID_TBI__LAST; ++n) {
    if (typ_equal(t, mod->gctx->builtin_typs_by_name[n])) {
      return TRUE;
    }
  }
  return FALSE;
}

bool typ_is_pseudo_builtin(const struct typ *t) {
  t = typ_follow_const(t);

  return typ_equal(t, TBI_LITERALS_NULL)
      || typ_equal(t, TBI_LITERALS_INTEGER)
      || typ_equal(t, TBI_LITERALS_FLOATING)
      || typ_equal(t, TBI_REF_COMPATIBLE)
      || typ_equal(t, TBI__NOT_TYPEABLE)
      || typ_equal(t, TBI__CALL_FUNCTION_SLOT)
      || typ_equal(t, TBI__MUTABLE)
      || typ_equal(t, TBI__MERCURIAL);
}

bool typ_is_trivial(const struct typ *t) {
  t = typ_follow_const(t);

  const struct typ *t0 = typ_generic_functor_const(t);

  return typ_equal(t, TBI_TRIVIAL_CTOR)
      || (t0 != NULL && typ_equal(t0, TBI_TRIVIAL_ARRAY_CTOR))
      || typ_equal(t, TBI_TRIVIAL_COPY)
      || typ_equal(t, TBI_TRIVIAL_EQUALITY)
      || typ_equal(t, TBI_TRIVIAL_ORDER)
      || typ_equal(t, TBI_TRIVIAL_DTOR)
      || typ_is_reference_instance(t);
}

bool typ_isa_return_by_copy(const struct typ *t) {
  return typ_isa(t, TBI_RETURN_BY_COPY);
}
