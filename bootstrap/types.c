#include "types.h"

#include "table.h"
#include "mock.h"
#include "typset.h"
#include "unify.h"
#include "inference.h"
#include "instantiate.h"
#include "parser.h"

#include <stdio.h>
#include <stdarg.h>

static size_t def_generic_arity(const struct typ *t);
static struct typ *def_generic_arg(struct typ *t, size_t n);

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

enum typ_flags {
  TYPF_TENTATIVE = 0x1,
  TYPF_BUILTIN = 0x2,
  TYPF_PSEUDO_BUILTIN = 0x4,
  TYPF_TRIVIAL = 0x8,
  TYPF_REF = 0x10,
  TYPF_NREF = 0x20,
  TYPF_SLICE = 0x40,
  TYPF_OPTIONAL = 0x80,
  TYPF_LITERAL = 0x100,
  TYPF_CONCRETE = 0x200,
  TYPF_GENARG = 0x400,
  TYPF_TUPLE = 0x800,
  TYPF__INHERIT_FROM_FUNCTOR = TYPF_TENTATIVE
    | TYPF_BUILTIN | TYPF_PSEUDO_BUILTIN
    | TYPF_TRIVIAL | TYPF_REF | TYPF_NREF | TYPF_OPTIONAL
    | TYPF_LITERAL | TYPF_CONCRETE | TYPF_GENARG | TYPF_TUPLE,
  TYPF__INHERIT_FROM_GENARG = TYPF_TENTATIVE | TYPF_GENARG,
  TYPF__MASK_HASH = 0xffff & ~(TYPF_TENTATIVE | TYPF_CONCRETE | TYPF_GENARG),
};

enum rdy_flags {
  RDY_HASH = 0x1,
  RDY_GEN = 0x2,
};

static uint32_t typ_ptr_hash(const struct typ **a) {
  return hash32_hsieh(a, sizeof(*a));
}

static int typ_ptr_cmp(const struct typ **a, const struct typ **b) {
  return memcmp(a, b, sizeof(*a));
}

HTABLE_SPARSE(typ2loc, struct typ **, struct typ *);
IMPLEMENT_HTABLE_SPARSE(unused__ static, typ2loc, struct typ **, struct typ *,
                        typ_ptr_hash, typ_ptr_cmp);

struct overlay {
  struct typ2loc map;
  struct module *trigger_mod;
};

struct typ {
  uint32_t flags;
  uint32_t hash;

  struct node *definition;
  struct overlay *overlay;

  uint8_t rdy;

  struct typ *perm;

  struct typ *gen0;
  size_t gen_arity;
  struct typ **gen_args; // of length gen_arity (generics)

  struct typset quickisa;

  struct backlinks backlinks;
  struct users users;
};

static struct node *definition(struct typ *t) {
  return t->definition;
}

static const struct node *definition_const(const struct typ *t) {
  return definition(CONST_CAST(t));
}

#define FOREACH_BACKLINK(idx, back, t, what) do { \
  struct backlinks *backlinks = &t->backlinks; \
  do { \
    for (size_t idx = 0; idx < backlinks->count; ++idx) { \
      struct typ **back = backlinks->links[idx]; \
      if (back != NULL) { \
        what; \
      } \
    } \
    backlinks = backlinks->more; \
  } while (backlinks != NULL); \
} while (0)

#define FOREACH_USER(idx, user, t, what) do { \
  struct users *users = &t->users; \
  do { \
    for (size_t idx = 0; idx < users->count; ++idx) { \
      struct typ *user = users->users[idx]; \
      if (user != NULL) { \
        what; \
      } \
    } \
    users = users->more; \
  } while (users != NULL); \
} while (0)

bool typ_is_ungenarg(const struct typ *t) {
  return t->flags & TYPF_GENARG;
}

static void overlay_init(struct typ *t, struct module *trigger_mod, struct typ *cpy) {
  t->overlay = calloc(1, sizeof(*t->overlay));
  typ2loc_init(&t->overlay->map, 0);
  if (cpy != NULL && cpy->overlay != NULL) {
    typ2loc_copy(&t->overlay->map, &cpy->overlay->map);
  }
  t->overlay->trigger_mod = trigger_mod;
}

static void overlay_map(struct typ *t, struct typ **dst, struct typ *src) {
  if (*dst == src) {
    return;
  }
  const bool ignore = typ2loc_set(&t->overlay->map, src, dst);
  (void) ignore;
}

static int ppoverlay_each(const struct typ **t, struct typ ***loc, void *user) {
  fprintf(stderr, "\t%s %p\n\t-> %s %p\n", pptyp(NULL, *t), *t, pptyp(NULL, **loc), **loc);
  return 0;
}

void ppoverlay(struct typ *t) {
  fprintf(stderr, "%s\n", pptyp(NULL, t));
  if (t->overlay == NULL) {
    return;
  }
  int ret = typ2loc_foreach(&t->overlay->map, ppoverlay_each, NULL);
  assert(!ret);
}

static struct typ *olay(const struct typ *t, struct typ *src);

static struct typ *overlay_translate(const struct typ *t, struct typ *src) {
  if (src == NULL) {
    return NULL;
  }

  struct typ ***existing = typ2loc_get(&CONST_CAST(t)->overlay->map, src);
  if (existing != NULL) {
    return **existing;
  } else {
    if (parent_const(t->definition)->typ == src) {
      // Already trying to translate the parent typ. Prevent recursion:
      return src;
    }

    struct tit *par = typ_definition_parent(t);
    if (par != NULL) {
      struct typ *p = tit_typ(par);
      tit_next(par);
      assert(p!=t);
      return olay(p, src);
    } else {
      return src;
    }
  }
}

static struct typ *olay(const struct typ *t, struct typ *src) {
  if (t->overlay != NULL) {
    return overlay_translate(t, src);
  } else {
    return src;
  }
}

bool typ_hash_ready(const struct typ *t) {
  return t->rdy & RDY_HASH;
}

static uint32_t typ_hash(const struct typ **a) {
  // The 'hash' in a tentative typ is maintained as the typ gets linked to a
  // new typs. So 'hash' can be used in typ_equal(), etc. But it cannot be
  // used in a hash table *IF* elements of the table are re-linked while the
  // table exists. fintypset can be used in typ_isalist_foreach() with
  // tentative types when typs are not actively being linked. E.g.
  // unify_generics() typ_isalist_each(find_instance_of) is fine. So we
  // cannot assert(!typ_is_tentative(*a)) in general.
  //
  // FIXME: this is a difficult condition to respect.
  assert(typ_hash_ready(*a));
  const uint32_t h = (*a)->hash;
  return h;
}

static int typ_cmp(const struct typ **a, const struct typ **b) {
  return !typ_equal(*a, *b);
}

IMPLEMENT_HTABLE_SPARSE(, fintypset, uint32_t, struct typ *,
                        typ_hash, typ_cmp);

static void remove_backlink(struct typ *t, struct typ **loc) {
  FOREACH_BACKLINK(idx, back, t,
                   if (back == loc) { backlinks->links[idx] = NULL; });
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

void unset_typ(struct typ **loc) {
  assert(*loc != NULL);

  remove_backlink(*loc, loc);
  *loc = NULL;
}

static void add_backlink(struct typ *t, struct typ **loc) {
  if (!typ_is_tentative(t)) {
    return;
  }

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

void set_typ(struct typ **loc, struct typ *t) {
  assert(t != NULL);
  assert(*loc == NULL
         || *loc == t
         || *loc == TBI__NOT_TYPEABLE
         || *loc == TBI__CALL_FUNCTION_SLOT
         || *loc == TBI__MERCURIAL || *loc == TBI__MUTABLE);

  *loc = t;

  add_backlink(t, loc);
}

void typ_add_tentative_bit__privileged(struct typ **loc) {
  if (!typ_is_tentative(*loc)) {
    (*loc)->flags |= TYPF_TENTATIVE;
    add_backlink(*loc, loc);
  }
}

void typ_declare_final__privileged(struct typ *t) {
  for (size_t n = 0, arity = typ_generic_arity(t); n < arity; ++n) {
    struct typ *arg = typ_generic_arg(t, n);
    typ_declare_final__privileged(arg);
  }
  t->flags &= ~TYPF_TENTATIVE;
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
  if (arg == user) {
    return;
  }

  if (typ_is_generic_functor(user)) {
    return;
  }

  if (((!typ_is_generic_functor(arg) && !typ_is_ungenarg(arg))
       || !typ_is_ungenarg(user))
      && !typ_is_tentative(arg)) {
    return;
  }

  struct users *users = &arg->users;
  while (users->more != NULL) {
    users = users->more;
  }

  if (users->count == USERS_LEN) {
    users->more = calloc(1, sizeof(*users->more));
    users = users->more;
  }

  // Do not use set_typ(): that would create ordering problems in
  // link_to_final()/link_to_tentative() between the processing of regular
  // backlinks, and users.
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

void fintypset_fullinit(struct fintypset *set) {
  fintypset_init(set, 0);
  fintypset_set_delete_val(set, -1);
}

static void quickisa_init(struct typ *t) {
  typset_init(&t->quickisa);
}

// Examples of non-concrete types: uninstantiated generics or instances
// within an instantiated generic which are instantiated over non-tentative
// interfaces.
static void create_update_concrete_flag(struct typ *t) {
  if (typ_is_ungenarg(t)) {
    t->flags &= ~TYPF_CONCRETE;
    return;
  }

  if (typ_is_generic_functor(t)) {
    t->flags &= ~TYPF_CONCRETE;
  } else if (typ_generic_arity(t) == 0) {
    t->flags |= TYPF_CONCRETE;
  } else if (typ_is_reference(t)) {
    if (definition_const(t)->which == DEFINTF) {
      t->flags &= ~TYPF_CONCRETE;
    } else {
      const struct typ *arg = typ_generic_arg_const(t, 0);
      if (typ_generic_arity(arg) == 0) {
        // Even if the generic argument is a DEFINTF, then t is a dyn, which
        // is concrete.
        t->flags |= TYPF_CONCRETE;
      } else {
        if (typ_is_concrete(arg)) {
          t->flags |= TYPF_CONCRETE;
        } else {
          t->flags &= ~TYPF_CONCRETE;
        }
      }
    }
  } else {
    t->flags |= TYPF_CONCRETE;

    for (size_t n = 0, arity = typ_generic_arity(t); n < arity; ++n) {
      const struct typ *arg = typ_generic_arg_const(t, n);
      if (arg == NULL) {
        // Too early to tell
        break;
      }
      if (typ_is_generic_functor(arg)
          && definition_const(arg)->which != DEFINTF) {
        continue;
      }
      if (!typ_is_concrete(arg)) {
        t->flags &= ~TYPF_CONCRETE;
        break;
      }
    }
  }
}

static void create_flags(struct typ *t, struct typ *tbi) {
  const struct node *d = definition_const(t);
  if (d == NULL) {
    // This is very early in the global init.
    return;
  }

  const struct toplevel *toplevel = node_toplevel_const(d);
  const struct typ *functor = NULL;
  if (toplevel != NULL && toplevel->generic != NULL) {
    functor = toplevel->generic->our_generic_functor_typ;
  }

  if (t == TBI_LITERALS_NIL
      || t == TBI_ANY_ANY_REF) {
    t->flags |= TYPF_REF;
  }

  if (t == TBI_LITERALS_NIL) {
    t->flags |= TYPF_NREF | TYPF_OPTIONAL;
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
      || maybe_ref == TBI_NMMREF) {
    t->flags |= TYPF_REF;
  }

  if (maybe_ref == TBI_ANY_NULLABLE_REF
      || maybe_ref == TBI_ANY_NULLABLE_MUTABLE_REF
      || maybe_ref == TBI_NREF
      || maybe_ref == TBI_NMREF
      || maybe_ref == TBI_NMMREF) {
    t->flags |= TYPF_NREF;
  }

  if (maybe_ref == TBI_ANY_ANY_SLICE
      || maybe_ref == TBI_ANY_SLICE
      || maybe_ref == TBI_ANY_MUTABLE_SLICE
      || maybe_ref == TBI_SLICE
      || maybe_ref == TBI_MSLICE) {
    t->flags |= TYPF_SLICE;
  }

  if (maybe_ref == TBI_OPTIONAL) {
    t->flags |= TYPF_OPTIONAL;
  }

  if (d->which == IMPORT) {
    t->flags |= TYPF_PSEUDO_BUILTIN;
  }

  if (tbi == NULL) {
    return;
  }

  t->flags |= TYPF_BUILTIN;

  if (tbi == TBI_LITERALS_NIL
      || tbi == TBI_LITERALS_INTEGER
      || tbi == TBI_LITERALS_FLOATING
      || tbi == TBI_LITERALS_STRING
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
      || tbi == TBI_TRIVIAL_COPY_BUT_OWNED
      || tbi == TBI_TRIVIAL_COMPARE
      || tbi == TBI_TRIVIAL_EQUALITY
      || tbi == TBI_TRIVIAL_ORDER
      || tbi == TBI_TRIVIAL_DTOR
      || tbi == TBI_TRIVIAL_ARRAY_CTOR) {
    t->flags |= TYPF_TRIVIAL;
  }

  if (tbi == TBI_LITERALS_NIL
      || tbi == TBI_LITERALS_INTEGER
      || tbi == TBI_LITERALS_FLOATING
      || tbi == TBI_LITERALS_STRING
      || tbi == TBI_LITERALS_SLICE) {
    t->flags |= TYPF_LITERAL;
  }

  if (tbi == TBI_TUPLE_2
      || tbi == TBI_TUPLE_3
      || tbi == TBI_TUPLE_4
      || tbi == TBI_TUPLE_5
      || tbi == TBI_TUPLE_6
      || tbi == TBI_TUPLE_7
      || tbi == TBI_TUPLE_8
      || tbi == TBI_TUPLE_9
      || tbi == TBI_TUPLE_10
      || tbi == TBI_TUPLE_11
      || tbi == TBI_TUPLE_12
      || tbi == TBI_TUPLE_13
      || tbi == TBI_TUPLE_14
      || tbi == TBI_TUPLE_15
      || tbi == TBI_TUPLE_16) {
    t->flags |= TYPF_TUPLE;
  }
}

bool typ_was_zeroed(const struct typ *t) {
  return definition_const(t) == NULL;
}

struct typ *typ_create(struct typ *tbi, struct node *definition) {
  struct typ *r = tbi != NULL ? tbi : calloc(1, sizeof(struct typ));

  r->definition = definition;
  quickisa_init(r);
  set_typ(&r->perm, r);
  create_flags(r, tbi);

  return r;
}

// Only for passfwd.
struct typ *typ_create_ungenarg(struct typ *t) {
  if (typ_is_ungenarg(t)) {
    // Already prepared by instantiate() in tentative/ungenarg mode.
    return t;
  }

  struct typ *r = calloc(1, sizeof(struct typ));
  quickisa_init(r);
  r->definition = definition(t);
  r->flags = t->flags | TYPF_GENARG;
  // Store 't' to here remember it. Properly set later in
  // do_typ_create_ungenarg_update_genargs().
  set_typ(&r->gen0, t);
  set_typ(&r->perm, r);
  return r;
}

void typ_create_update_genargs(struct typ *t) {
  if (t->rdy & RDY_GEN) {
    return;
  }

  create_update_concrete_flag(t);

  struct node *dpar = parent(definition(t));
  if (NM(dpar->which) & (NM(DEFTYPE) | NM(DEFINTF))) {
    if (typ_is_tentative(dpar->typ)) {
      typ_add_tentative_bit__privileged(&t);
    }
    if (typ_is_ungenarg(dpar->typ)) {
      t->flags |= TYPF_GENARG;
    }
    add_user(dpar->typ, t);
  }

  const size_t arity = typ_generic_arity(t);
  t->gen_arity = arity;

  if (arity == 0) {
    t->rdy |= RDY_GEN;
    return;
  }

  t->gen_args = calloc(arity, sizeof(*t->gen_args));

  if (t->gen0 != NULL) {
    unset_typ(&t->gen0);
  }
  struct typ *t0 = typ_generic_functor(t);
  set_typ(&t->gen0, t0);
  if (typ_is_tentative(t0)) {
    typ_add_tentative_bit__privileged(&t);
  }
  if (typ_is_ungenarg(t0)) {
    t->flags |= TYPF_GENARG;
  }
  add_user(t0, t);

  for (size_t n = 0, count = typ_generic_arity(t); n < count; ++n) {
    struct typ *arg = typ_generic_arg(t, n);
    set_typ(&t->gen_args[n], arg);
    if (typ_is_tentative(arg)) {
      typ_add_tentative_bit__privileged(&t);
    }
    if (typ_is_ungenarg(arg) && !typ_is_generic_functor(t)) {
      t->flags |= TYPF_GENARG;
    }
    add_user(arg, t);
  }

  t->rdy |= RDY_GEN;
}

void typ_create_update_hash(struct typ *t) {
  if (typ_hash_ready(t) && typ_is_ungenarg(t)) {
    return;
  }
  assert(t->rdy & RDY_GEN);

#define LEN 8 // arbitrary, at least 4
  uint32_t buf[LEN] = { 0 };
  buf[0] = t->flags & TYPF__MASK_HASH;
  buf[1] = node_ident(definition_const(t));
  buf[2] = t->gen_arity;
  size_t i = 3;
  for (size_t n = 0; n < t->gen_arity; ++n) {
    if (i == LEN) {
      break;
    }
    buf[i] = node_ident(definition_const(t->gen_args[n]));
    i += 1;
  }
#undef LEN

  t->hash = hash32_hsieh(buf, sizeof(buf));
  t->rdy |= RDY_HASH;
}

static ERROR update_quickisa_isalist_each(struct module *mod,
                                          struct typ *t, struct typ *intf,
                                          bool *stop, void *user) {

  if (typset_has(&t->quickisa, intf)) {
    return 0;
  }

  typset_add(&t->quickisa, intf);

  struct typ *intf0 = typ_generic_functor(intf);
  if (intf0 != NULL) {
    typset_add(&t->quickisa, intf0);
  }

  if (typ_is_generic_functor(t)) {
    FOREACH_USER(idx, user, t, {
      if (typ_generic_functor_const(user) == t) {
        typ_create_update_quickisa(user);
      }
    });
  }

  return 0;
}

void typ_create_update_quickisa(struct typ *t) {
  error never = typ_isalist_foreach(NULL, t, 0, update_quickisa_isalist_each, NULL);
  assert(!never);

  struct typ *t0 = typ_generic_functor(t);
  if (t0 != NULL) {
    error never = update_quickisa_isalist_each(NULL, t, t0, NULL, NULL);
    assert(!never);
  }

  t->quickisa.ready = true;
}

bool typ_is_tentative(const struct typ *t) {
  return t->flags & TYPF_TENTATIVE;
}

static struct module *trigger_mod_for(const struct typ *t) {
  if (t->overlay != NULL) {
    return t->overlay->trigger_mod;
  } else {
    return node_toplevel_const(definition_const(t))->generic->trigger_mod;
  }
}

HTABLE_SPARSE(typptrset, uint32_t, struct typ *);
IMPLEMENT_HTABLE_SPARSE(unused__ static, typptrset, uint32_t, struct typ *,
                        typ_ptr_hash, typ_ptr_cmp);

static bool track_ungenarg(struct typptrset *set, struct typ *t) {
  uint32_t *v = typptrset_get(set, t);
  if (v != NULL && *v > 0) {
    return true;
  } else if (v != NULL) {
    *v += 1;
  } else {
    typptrset_set(set, t, 1);
  }
  return false;
}

static struct typ *do_typ_create_tentative(struct module *trigger_mod,
                                           struct typ *t, struct typ **args, size_t arity,
                                           struct typptrset *set, bool reject_identical);
static void map_ungenarg_users(struct module *trigger_mod,
                               struct typ *t, struct typ *src, struct typptrset *set);

static void map_ungenarg_user(struct module *trigger_mod,
                              struct typ *t, struct typ *user, struct typptrset *set) {
  if (typ_generic_arity(user) == 0) {
    return;
  }

  if (track_ungenarg(set, user)) {
    return;
  }

  struct typ *user0 = typ_generic_functor(user);
  const size_t arity = typ_generic_arity(user);

  struct typ **args = calloc(arity, sizeof(struct typ *));
  struct typ *i0 = olay(t, user0);
  if (!typ_is_generic_functor(i0)) {
    // Already mapped to an instance.
    return;
  }

  bool diff = i0 != user0;
  for (size_t n = 0; n < arity; ++n) {
    struct typ *ga = typ_generic_arg(user, n);
    struct typ *mga = olay(t, ga);
    if (mga != ga) {
      args[n] = mga;
      diff |= true;
    } else if (typ_generic_arg_has_dependent_spec(user, n)) {
      args[n] = NULL;
    } else {
      args[n] = ga;
    }
  }

  if (!diff) {
    free(args);
    return;
  }

  struct typ *i = do_typ_create_tentative(trigger_mod, i0, args, arity, set, false);
  overlay_map(t, typ_permanent_loc(i), user);
  free(args);

  map_ungenarg_users(trigger_mod, t, user, set);
}

static void map_ungenarg_users(struct module *trigger_mod,
                               struct typ *t, struct typ *src, struct typptrset *set) {
  if (!typ_is_ungenarg(src)) {
    return;
  }
  FOREACH_USER(idx, user, src, map_ungenarg_user(trigger_mod, t, user, set));
}

static void map_ungenarg(struct typ *t, struct typ **dst, struct typ *src) {
  overlay_map(t, dst, src);
}

static void map_children(struct module *trigger_mod, struct typ *par) {
  struct tit *tit = typ_definition_members(par, DEFMETHOD, DEFFUN, 0);
  while (tit_next(tit)) {
    struct typ *m = tit_typ(tit);
    struct typ *mm;
    if (typ_is_generic_functor(m)) {
      mm = typ_create_tentative_functor(trigger_mod, m);
    } else {
      mm = typ_create_tentative(trigger_mod, m, NULL, 0);
    }

    map_ungenarg(par, &mm->perm, m);
    add_user(par, mm);

    map_ungenarg(mm, &par->perm, tit_parent_definition_typ(tit));
  }
}

struct typ *typ_create_tentative_functor(struct module *trigger_mod, struct typ *t) {
  assert(typ_is_generic_functor(t));
  assert(typ_hash_ready(t));

  if (typ_is_tentative(t)) {
    return t;
  }

  struct typ *r = calloc(1, sizeof(struct typ));
  quickisa_init(r);
  r->flags = (t->flags & ~TYPF_GENARG) | TYPF_TENTATIVE;
  r->rdy = t->rdy;
  r->hash = t->hash;
  r->definition = definition(t);
  set_typ(&r->perm, r);

  assert(t->rdy & RDY_GEN);
  r->gen_arity = t->gen_arity;
  overlay_init(r, trigger_mod, t);
  r->gen_args = calloc(t->gen_arity, sizeof(*r->gen_args));
  struct typ *final = typ_definition_which(t) == DEFINTF ? typ_member(t, ID_FINAL) : t;
  set_typ(&r->gen0, r /* tentative functors are their own functor */);
  if (typ_is_ungenarg(final)) {
    map_ungenarg(r, &r->gen0, final);
  }

  for (size_t n = 0; n < t->gen_arity; ++n) {
    set_typ(&r->gen_args[n], t->gen_args[n]);
    map_ungenarg(r, &r->gen_args[n], t->gen_args[n]);
  }

  struct typptrset set = { 0 };
  typptrset_init(&set, 0);
  for (size_t n = 0; n < t->gen_arity; ++n) {
    map_ungenarg_users(trigger_mod, r, t->gen_args[n], &set);
  }
  typptrset_destroy(&set);

  typ_create_update_quickisa(r);

  map_children(trigger_mod, r);

  assert(typ_is_generic_functor(r));
  return r;
}

static void do_typ_create_ungenarg_update_genargs(struct module *trigger_mod,
                                                  struct typ *r, struct typptrset *set) {
  const bool not_ready_ungenarg = r->gen_args == NULL && typ_is_ungenarg(r);

  struct typ *t = r->gen0;
  if (not_ready_ungenarg) {
    if (typ_generic_arity(r) == 0) {
      goto nothing_to_do;
    }
  } else if (r->gen_arity == 0) {
    if (!typ_is_tentative(r)) {
      goto nothing_to_do;
    }
    goto children;
  }

  assert(t->rdy & RDY_GEN);

  if (not_ready_ungenarg) {
    // They were not set in typ_create_ungenarg(), as the info was not yet
    // available.
    unset_typ(&r->gen0);
    if (typ_is_generic_functor(t)) {
      set_typ(&r->gen0, r);
    } else if (typ_generic_arity(t) > 0) {
      set_typ(&r->gen0, typ_generic_functor(t));
    } else {
      set_typ(&r->gen0, t);
    }

    r->gen_arity = t->gen_arity;
    r->gen_args = calloc(r->gen_arity, sizeof(struct typ *));
    for (size_t n = 0; n < r->gen_arity; ++n) {
      set_typ(&r->gen_args[n], t->gen_args[n]);
    }

    overlay_init(r, trigger_mod, t);
  }

  assert(t->gen_arity == r->gen_arity);

  for (size_t n = 0; n < r->gen_arity; ++n) {
    if (r->gen_args[n] != NULL) {
      map_ungenarg(r, &r->gen_args[n], t->gen_args[n]);
    }
  }

  // The call map_children() at the very end handle the case where the
  // DEFTYPE/DEFINTF is the one being create as tentative. But if a
  // DEFMETHOD is created as tentative (because it is itself also a
  // generic), we need to add ourself as user of par, and map in our overlay
  // the users of par's genargs.
  struct tit *titpar = typ_definition_parent(r);
  if (titpar != NULL) {
    struct typ *par = tit_typ(titpar);
    tit_next(titpar);

    add_user(par, r);

    if (typ_generic_arity(par) > 0) {
      for (size_t n = 0, arity = typ_generic_arity(par); n < arity; ++n) {
        map_ungenarg_users(trigger_mod, r, typ_generic_functor(par)->gen_args[n], set);
      }
    }
  }

  bool no = track_ungenarg(set, r);
  assert(!no);
  for (size_t n = 0; n < t->gen_arity; ++n) {
    if (r->gen_args[n] == NULL) {
      // We find it in the overlay, put there by map_ungenarg() in a previous
      // iteration of this loop.
      set_typ(&r->gen_args[n], olay(r, typ_generic_arg(t, n)));
    }
    map_ungenarg_users(trigger_mod, r, t->gen_args[n], set);
  }

  add_user(r->gen0, r);
  for (size_t n = 0; n < r->gen_arity; ++n) {
    add_user(r->gen_args[n], r);
  }

children:
  map_children(trigger_mod, r);

nothing_to_do:
  r->rdy |= RDY_GEN;
}

void typ_create_ungenarg_update_genargs(struct module *trigger_mod, struct typ *r) {
  struct typptrset set = { 0 };
  typptrset_init(&set, 0);
  do_typ_create_ungenarg_update_genargs(trigger_mod, r, &set);
  typptrset_destroy(&set);
}

static void is_it_ungenarg_or_tentative(bool *is_ungenarg, bool *is_tentative,
                                   struct typ *t, struct typ **args, size_t arity) {
  *is_ungenarg = typ_is_ungenarg(t);
  for (size_t n = 0; n < arity; ++n) {
    *is_ungenarg |= args[n] != NULL ? typ_is_ungenarg(args[n]) : false;
  }

  *is_tentative = typ_generic_arity(t) == 0 || !*is_ungenarg;
  *is_tentative |= typ_is_tentative(t);
  for (size_t n = 0; n < arity; ++n) {
    *is_tentative |= args[n] != NULL ? typ_is_tentative(args[n]) : false;
  }

  *is_ungenarg &= !*is_tentative;
}

static struct typ *do_typ_create_tentative(struct module *trigger_mod,
                                           struct typ *t, struct typ **args, size_t arity,
                                           struct typptrset *set, bool reject_identical) {
  assert(typ_is_generic_functor(t) || typ_generic_arity(t) == 0);

  struct typ *r = NULL;
  if (arity > 0 && !reject_identical) {
    r = instances_find_existing_identical(trigger_mod, t, args, arity);
    if (r != NULL) {
      return r;
    }
  }

  bool is_ungenarg = false, is_tentative = false;
  is_it_ungenarg_or_tentative(&is_ungenarg, &is_tentative, t, args, arity);

  r = calloc(1, sizeof(struct typ));
  quickisa_init(r);
  r->flags = t->flags & ~TYPF_GENARG;
  r->flags |= is_ungenarg ? TYPF_GENARG : 0;
  r->flags |= is_tentative ? TYPF_TENTATIVE : 0;
  r->definition = definition(t);
  set_typ(&r->perm, r);

  struct typ *final = is_ungenarg ? t
    : (NM(typ_definition_which(t)) & (NM(DEFINTF) | NM(DEFINCOMPLETE))) ? typ_member(t, ID_FINAL) : t;
  set_typ(&r->gen0, final);

  r->gen_arity = arity;
  r->gen_args = calloc(arity, sizeof(*r->gen_args));
  for (size_t n = 0; n < r->gen_arity; ++n) {
    if (args[n] != NULL) {
      set_typ(&r->gen_args[n], args[n]);
    }
  }

  overlay_init(r, trigger_mod, t);

  instances_add(trigger_mod, t, typ_permanent_loc(r));

  if (is_ungenarg && !(t->rdy & RDY_GEN)) {
    // For genarg, there are updates in passfwd.
    return r;
  }

  do_typ_create_ungenarg_update_genargs(trigger_mod, r, set);

  if (is_ungenarg && !(t->rdy & RDY_HASH)) {
    // For genarg, there are updates in passfwd.
    return r;
  }

  typ_create_update_hash(r);

  if (is_ungenarg && !t->quickisa.ready) {
    // For genarg, there are updates in passfwd.
    return r;
  }

  typ_create_update_quickisa(r);

  return r;
}

struct typ *typ_create_tentative(struct module *trigger_mod,
                                 struct typ *t, struct typ **args, size_t arity) {
  struct typptrset set = { 0 };
  typptrset_init(&set, 0);
  struct typ *r = do_typ_create_tentative(trigger_mod, t, args, arity, &set, true);
  typptrset_destroy(&set);
  return r;
}

static bool is_actually_still_tentative(const struct typ *user) {
  bool r;
  struct tit *par = typ_definition_parent(user);
  if ((NM(tit_which(par)) & (NM(DEFTYPE) | NM(DEFINTF)))
      && typ_is_tentative(tit_typ(par))) {
    r = true;
    goto out;
  }
  if (typ_generic_arity(user) > 0) {
    const struct typ *user0 = typ_generic_functor_const(user);
    if (typ_is_tentative(typ_generic_functor_const(user))
        || typ_definition_which(user0) == DEFINTF) {
      r = true;
      goto out;
    }
    for (size_t n = 0, arity = typ_generic_arity(user); n < arity; ++n) {
      if (typ_is_tentative(typ_generic_arg_const(user, n))) {
        r = true;
        goto out;
      }
    }
  }

  r = false;
out:
  tit_next(par);
  return r;
}

static void link_finalize(struct typ *user) {
  if (typ_was_zeroed(user)) {
    // Was zeroed in link_generic_functor_update().
    return;
  }

  if (typ_is_tentative(user) && is_actually_still_tentative(user)) {
    return;
  }

  schedule_finalization(user);
}

static void link_generic_functor_update(struct typ *user, struct typ *dst, struct typ *src) {
  if (typ_was_zeroed(user)) {
    return;
  }

  assert(typ_is_tentative(user));

  struct typ *user0 = typ_generic_functor(user);
  if (user0 == src) {
    // 'src' was used by 'user' as generic functor. Linking to the new
    // functor means creating a new instance, and linking 'user' to it.

    assert(typ_is_generic_functor(src));
    assert(typ_is_generic_functor(dst));

    struct module *trigger_mod = trigger_mod_for(user);
    const bool need_state = !trigger_mod->state->tentatively
      && trigger_mod->state->top_state == NULL;
    if (need_state) {
      PUSH_STATE(trigger_mod->state);
      trigger_mod->state->tentatively = true;
    }

    struct typ *new_user = unify_with_new_functor(trigger_mod, NULL, dst, user);
    assert(typ_was_zeroed(user) && "must have been zeroed");

    if (need_state) {
      POP_STATE(trigger_mod->state);
    }

    typ_create_update_hash(new_user);
    create_update_concrete_flag(new_user);
  }
}

static void link_generic_arg_update(struct typ *user, struct typ *dst, struct typ *src) {
  if (typ_was_zeroed(user)) {
    return;
  }

  assert(typ_is_tentative(user));
  assert(!typ_is_generic_functor(src));

  // If 'src' was used by 'user' as a generic arg, then the
  // FOREACH_BACKLINK() pass already updated the SETGENARG in
  // definition(user).

  for (size_t n = 0, arity = typ_generic_arity(user); n < arity; ++n) {
    struct typ *ga = typ_generic_arg(user, n);
    if (ga == dst) {
      // We do have to do it by hand, add_user() doesn't use set_typ().
      add_user(dst, user);
    }
  }

  typ_create_update_hash(user);
  create_update_concrete_flag(user);
}

static void link_parent_update(struct typ *user, struct typ *dst, struct typ *src) {
  if (typ_was_zeroed(user)) {
    return;
  }
  if (typ_is_generic_functor(user)) {
    return;
  }

  assert(typ_is_tentative(user));

  struct tit *par = typ_definition_parent(user);
  if (tit_typ(par) == src) {
    unify_with_new_parent(trigger_mod_for(user), NULL, dst, user);
  }
  tit_next(par);
}

static void link_to_final(struct module *mod, struct typ *dst, struct typ *src) {
  FOREACH_USER(idx, user, src, link_parent_update(user, dst, src));

  FOREACH_USER(idx, user, src, link_generic_functor_update(user, dst, src));

  // May have been zeroed in link_generic_functor_update().
  FOREACH_BACKLINK(idx, back, src, if (*back != NULL) { unset_typ(back); set_typ(back, dst); });

  FOREACH_USER(idx, user, src, link_generic_arg_update(user, dst, src));

  FOREACH_USER(idx, user, src, link_finalize(user));

  // Noone should be referring to 'src' anymore, except perm; let's make sure.
  struct typ *perm = src->perm;
  memset(src, 0, sizeof(*src));
  src->perm = perm;
}

// When linking src to a tentative dst, and when src is a generic, each of
// the generic arguments of src have gained a user (dst) and lost a user
// (src).
noinline__ // Expensive -- we want it to show up in profiles.
static void remove_as_user_of_generic_args(struct typ *t) {
  for (size_t n = 0, arity = typ_generic_arity(t); n < arity; ++n) {
    struct typ *arg = typ_generic_arg(t, n);
    FOREACH_USER(idx, user, arg,
                 if (user == t) { users->users[idx] = NULL; });
  }
}

static void link_to_tentative(struct module *mod, struct typ *dst, struct typ *src) {
  FOREACH_USER(idx, user, src, link_parent_update(user, dst, src));

  FOREACH_USER(idx, user, src, link_generic_functor_update(user, dst, src));

  // May have been zeroed in link_generic_functor_update().
  FOREACH_BACKLINK(idx, back, src, if (*back != NULL) { unset_typ(back); set_typ(back, dst); });

  FOREACH_USER(idx, user, src, link_generic_arg_update(user, dst, src));

  remove_as_user_of_generic_args(src);

  clear_backlinks(src);
  clear_users(src);

  // Noone should be referring to 'src' anymore, except perm; let's make sure.
  struct typ *perm = src->perm;
  memset(src, 0, sizeof(*src));
  src->perm = perm;
}

void typ_link_tentative(struct typ *dst, struct typ *src) {
  if (typ_is_ungenarg(src)) {
    return;
  }

  assert(typ_is_tentative(src));
  assert(!typ_is_generic_functor(src) && "use typ_link_tentative_functor()");

  if (dst == src) {
    return;
  }

  if (!typ_is_tentative(dst)) {
    link_to_final(NULL, dst, src);
  } else {
    link_to_tentative(NULL, dst, src);
  }
}

void typ_link_tentative_functor(struct module *mod, struct typ *dst, struct typ *src) {
  if (typ_is_ungenarg(src)) {
    return;
  }

  assert(mod != NULL);
  assert(typ_is_tentative(src));
  assert(typ_is_generic_functor(src));
  assert(typ_is_generic_functor(dst));

  if (dst == src) {
    return;
  }

  if (!typ_is_tentative(dst)) {
    link_to_final(NULL, dst, src);
  } else {
    link_to_tentative(NULL, dst, src);
  }
}

void typ_link_to_existing_final(struct typ *dst, struct typ *src) {
  assert(!typ_is_tentative(dst));
  assert(typ_is_tentative(src));

  if (dst == src) {
    return;
  }

  link_to_final(NULL, dst, src);
}

void typ_debug_check_in_backlinks(struct typ **u) {
  struct typ *t = *u;
  if (t == NULL || !typ_is_tentative(t)) {
    return;
  }
  bool r = false;
  FOREACH_BACKLINK(idx, b, t, if (b != NULL) { r |= b == u; });
}

struct typ **typ_permanent_loc(struct typ *t) {
  return &t->perm;
}

struct node *typ_definition_nooverlay(struct typ *t) {
  assert(t->overlay == NULL || typ2loc_count(&t->overlay->map) == 0);
  return t->definition;
}

const struct node *typ_definition_nooverlay_const(const struct typ *t) {
  assert(t->overlay == NULL || typ2loc_count(&t->overlay->map) == 0);
  return t->definition;
}

struct node *typ_definition_ignore_any_overlay(struct typ *t) {
  return t->definition;
}

const struct node *typ_definition_ignore_any_overlay_const(const struct typ *t) {
  return t->definition;
}

const struct node *typ_for_error(const struct typ *t) {
  return node_toplevel_const(definition_const(t))->generic->for_error;
}

bool typ_is_function(const struct typ *t) {
  const struct node *def = definition_const(t);
  return def->which == DEFFUN || def->which == DEFMETHOD;
}

uint32_t typ_toplevel_flags(const struct typ *t) {
  return node_toplevel_const(definition_const(t))->flags;
}

enum node_which typ_definition_which(const struct typ *t) {
  return definition_const(t)->which;
}

enum deftype_kind typ_definition_deftype_kind(const struct typ *t) {
  const struct node *d = definition_const(t);
  assert(d->which == DEFTYPE);
  return d->as.DEFTYPE.kind;
}

struct typ *typ_definition_tag_type(const struct typ *t) {
  const struct node *d = definition_const(t);
  assert(d->which == DEFTYPE);
  return olay(t, d->as.DEFTYPE.tag_typ);
}

enum token_type typ_definition_defmethod_access(const struct typ *t) {
  const struct node *d = definition_const(t);
  assert(d->which == DEFMETHOD);
  return d->as.DEFMETHOD.access;
}

struct typ *typ_definition_defmethod_self_wildcard_functor(const struct typ *t) {
  assert(t->definition->which == DEFMETHOD);
  if (typ_definition_defmethod_access(t) != TREFWILDCARD) {
    return NULL;
  }
  const struct node *genargs = subs_at_const(definition_const(t), IDX_GENARGS);
  return olay(t, subs_at_const(genargs, t->definition->as.DEFMETHOD.first_wildcard_genarg)->typ);
}

struct typ *typ_definition_defmethod_wildcard_functor(const struct typ *t) {
  assert(t->definition->which == DEFMETHOD);
  if (typ_definition_defmethod_access(t) != TREFWILDCARD) {
    return NULL;
  }
  const struct node *genargs = subs_at_const(definition_const(t), IDX_GENARGS);
  return olay(t, subs_at_const(genargs, t->definition->as.DEFMETHOD.first_wildcard_genarg+1)->typ);
}

struct typ *typ_definition_deffun_wildcard_functor(const struct typ *t) {
  assert(t->definition->which == DEFFUN);
  if (t->definition->as.DEFFUN.access != TWILDCARD) {
    return NULL;
  }
  const struct node *genargs = subs_at_const(definition_const(t), IDX_GENARGS);
  return olay(t, subs_last_const(genargs)->typ);
}

ident typ_definition_ident(const struct typ *t) {
  return node_ident(definition_const(t));
}

struct module *typ_module_owner(const struct typ *t) {
  if (t->definition->which == DEFINCOMPLETE) {
    return t->definition->as.DEFINCOMPLETE.trigger_mod;
  }
  return node_module_owner(CONST_CAST(t)->definition);
}

struct typ *typ_member(struct typ *t, ident name) {
  struct node *d = definition(t);
  struct node *m = node_get_member(d, name);
  return m != NULL ? olay(t, m->typ) : NULL;
}

struct tit {
  const struct typ *t;

  struct node *definition;
  struct node *pos;
  uint64_t node_filter;
  bool just_one;
};

bool tit_next(struct tit *tit) {
  struct node *pos = tit->pos;
  if (pos == NULL) {
    // We're being called on a brand new tit.
    pos = subs_first(tit->definition);
  }

  tit->pos = NULL;

  if (tit->just_one) {
    goto done;
  }

  if (pos->which == DEFCHOICE) {
    struct node *f = subs_first(pos);
    if (f != NULL) {
      tit->pos = f;
      goto done;
    }
  }

  struct node *n = next(pos);
  if (n != NULL) {
    tit->pos = n;
    goto done;
  }

  struct node *p = parent(pos);
  if (p->which == DEFCHOICE) {
    tit->pos = next(p);
    goto done;
  }

done:
  if (tit->pos == NULL) {
    free(tit);
    return false;
  }

  if (tit->node_filter != 0 && (tit->node_filter & NM(tit->pos->which))) {
    return true;
  }

  return tit_next(tit);
}

struct tit *typ_definition_parent(const struct typ *t) {
  struct tit *tit = calloc(1, sizeof(struct tit));
  tit->definition = parent(definition(CONST_CAST(t)));
  tit->t = t;
  tit->just_one = true;
  tit->pos = tit->definition;
  return tit;
}

struct tit *typ_definition_one_member(const struct typ *t, ident name) {
  struct tit *tit = calloc(1, sizeof(struct tit));
  tit->t = t;
  tit->definition = definition(CONST_CAST(t));
  tit->just_one = true;

  error e = scope_lookup_ident_immediate(&tit->pos, tit->definition,
                                         typ_module_owner(t),
                                         &tit->definition->scope,
                                         name, true);
  if (e) {
    free(tit);
    return NULL;
  }

  return tit;
}

struct tit *typ_definition_members(const struct typ *t, ...) {
  struct tit *tit = calloc(1, sizeof(struct tit));
  tit->t = t;
  tit->definition = definition(CONST_CAST(t));

  va_list ap;
  va_start(ap, t);

  uint64_t f = 0;
  enum node_which n;
  while ((n = va_arg(ap, enum node_which)) != 0) {
    f |= NM(n);
  }
  tit->node_filter = f;

  return tit;
}

// TODO: Ideally, we shouldn't be setting any 'typ' from within types.c. This
// should be coded in inference.c. The next few functions have that kind of
// side effect "__has_effect", however.
//

static void bin_accessor_maybe_literal_string__has_effect(struct module *mod, struct node *par) {
  if (typ_isa(par->typ, TBI_LITERALS_STRING)) {
    error ok = unify(mod, par, par->typ, TBI_STRING);
    assert(!ok);
  }
}

static void bin_accessor_maybe_literal_slice__has_effect(struct module *mod, struct node *par) {
  if (typ_isa(par->typ, TBI_LITERALS_SLICE)) {
    struct typ *i = instantiate_fully_implicit(mod, par, TBI_SLICE);
    error ok = unify(mod, par, par->typ, i);
    assert(!ok);
  }
}

static void bin_accessor_maybe_functor__has_effect(struct module *mod, struct node *par) {
  // Something like the (hypothetical): vector.mk_filled 100 0:u8
  // 'vector' is a generic functor, and the instantiation will be done
  // through the call to the function 'vector.mk_filled'. We need to have a
  // fully tentative instance of 'vector' so that the unification of '0:u8'
  // with t:`copyable succeeds.
  if ((par->flags & NODE_IS_TYPE)
      && typ_is_generic_functor(par->typ)
      && node_ident(par) != ID_THIS) {

    struct typ *i = instantiate_fully_implicit(mod, par, par->typ);
    unset_typ(&par->typ);
    set_typ(&par->typ, i);
  }
}

static bool bin_accessor_maybe_ref(struct node **parent_scope,
                                   struct module *mod, struct node *par) {
  if (typ_is_reference(par->typ)) {
    *parent_scope = definition(typ_generic_arg(par->typ, 0));
    return true;
  }
  return false;
}

static void bin_accessor_maybe_defchoice_field(struct node **parent_scope, struct node *for_error,
                                               struct module *mod, struct node *par) {
  if ((par->flags & NODE_IS_DEFCHOICE)
      && !(par->flags & NODE_IS_DEFCHOICE_HAS_EXTERNAL_PAYLOAD)) {
    struct node *defchoice = NULL;
    error e = scope_lookup_ident_immediate(&defchoice, for_error, mod,
                                           &definition(par->typ)->scope,
                                           node_ident(subs_last(par)), false);
    assert(!e);
    assert(defchoice->which == DEFCHOICE);

    const struct node *ext = node_defchoice_external_payload(defchoice);
    *parent_scope = ext != NULL ? definition(ext->typ) : defchoice;
  }
}

struct tit *typ_resolve_accessor__has_effect(error *e,
                                             bool *container_is_tentative,
                                             struct module *mod,
                                             struct node *node) {
  assert(node->which == BIN && OP_KIND(node->as.BIN.operator) == OP_BIN_ACC);

  struct node *left = subs_first(node);
  bin_accessor_maybe_literal_string__has_effect(mod, left);
  bin_accessor_maybe_literal_slice__has_effect(mod, left);
  bin_accessor_maybe_functor__has_effect(mod, left);

  struct node *dcontainer = definition(left->typ);
  if (!bin_accessor_maybe_ref(&dcontainer, mod, left)) {
    bin_accessor_maybe_defchoice_field(&dcontainer, node, mod, left);
  }
  struct scope *container_scope = &dcontainer->scope;
  *container_is_tentative = typ_is_tentative(scope_node(container_scope)->typ);

  struct node *name = subs_last(node);
  struct node *field = NULL;
  *e = scope_lookup_ident_immediate(&field, name, mod, container_scope,
                                    node_ident(name), *container_is_tentative);
  if (*e) {
    return NULL;
  }

  if (field->which == IMPORT && !field->as.IMPORT.intermediate_mark) {
    error none = scope_lookup(&field, mod, &mod->gctx->modules_root.scope,
                              subs_first(field), false);
    assert(!none);
  }

  struct tit *r = calloc(1, sizeof(struct tit));
  r->t = typ_is_reference(left->typ) ? typ_generic_arg(left->typ, 0) : left->typ;
  r->definition = dcontainer;
  r->just_one = true;
  r->pos = field;
  return r;
}

enum node_which tit_which(const struct tit *tit) {
  return tit->pos->which;
}

ident tit_ident(const struct tit *tit) {
  return node_ident(tit->pos);
}

struct typ *tit_typ(const struct tit *tit) {
  struct typ *r = tit->pos->typ;
  if (tit->pos->which == DEFCHOICE) {
    const struct node *ext = node_defchoice_external_payload(tit->pos);
    if (ext != NULL) {
      r = ext->typ;
    }
  }
  return olay(tit->t, r);
}

struct typ *tit_parent_definition_typ(const struct tit *tit) {
  struct node *p = tit->pos;
  if (NM(p->which) & (NM(MODULE) | NM(MODULE_BODY) | NM(IMPORT))) {
    return olay(tit->t, parent(p)->typ);
  }

  while (!node_is_at_top(p)) {
    p = parent(p);
  }
  return olay(tit->t, p->typ);
}

uint32_t tit_node_flags(const struct tit *tit) {
  return tit->pos->flags;
}

struct tit *tit_let_def(const struct tit *tit) {
  assert(tit->pos->which == LET);
  struct tit *r = calloc(1, sizeof(struct tit));
  r->t = tit->t;
  r->definition = tit->definition;
  r->just_one = true;
  r->pos = subs_first(tit->pos);
  assert(NM(r->pos->which) & (NM(DEFNAME) | NM(DEFALIAS)));
  return r;
}

struct node *tit_defname_expr(const struct tit *tit) {
  assert(tit->pos->which == DEFNAME);
  return subs_last(tit->pos);
}

bool tit_defchoice_is_leaf(const struct tit *tit) {
  assert(tit->pos->which == DEFCHOICE);
  return tit->pos->as.DEFCHOICE.is_leaf;
}

bool tit_defchoice_is_external_payload(const struct tit *tit) {
  assert(tit->pos->which == DEFCHOICE);
  return node_defchoice_external_payload(tit->pos) != NULL;
}

struct tit *tit_defchoice_lookup_field(const struct tit *variant, ident name) {
  const struct node *d = variant->pos;
  assert(d->which == DEFCHOICE);

  const struct typ *container = NULL;
  struct node *field = NULL;
  while (true) {
    container = variant->t;

    const struct scope *sc = &d->scope;
    if (d->which == DEFCHOICE) {
      const struct node *ext = node_defchoice_external_payload(d);
      if (ext != NULL) {
        container = olay(variant->t, ext->typ);
        sc = &definition_const(ext->typ)->scope;
      }
    }

    error e = scope_lookup_ident_immediate(&field, NULL, NULL, sc,
                                           name, true);
    if (!e) {
      break;
    }

    if (d->which == DEFTYPE) {
      return NULL;
    }

    d = parent_const(d);
  }

  struct tit *r = calloc(1, sizeof(struct tit));
  r->t = container;
  r->definition = container->definition;
  r->just_one = true;
  r->pos = field;
  return r;
}


static struct typ *def_generic_functor(struct typ *t) {
  if (typ_generic_arity(t) == 0) {
    return NULL;
  }

  const struct toplevel *toplevel = node_toplevel_const(definition_const(t));
  if (toplevel->generic->our_generic_functor_typ != NULL) {
    return toplevel->generic->our_generic_functor_typ;
  } else {
    return t;
  }
}

struct node *tit_node_ignore_any_overlay(const struct tit *tit) {
  return tit->pos;
}

const struct node *tit_for_error(const struct tit *tit) {
  return tit_node_ignore_any_overlay(tit);
}

struct typ *typ_generic_functor(struct typ *t) {
  struct typ *r = NULL;
  if (t->rdy & RDY_GEN) {
    if (t->gen_arity == 0) {
      return NULL;
    } else {
      r = t->gen0;
    }
  } else {
    r = def_generic_functor(t);
  }
  return r;
}

EXAMPLE_NCC(typ_generic_functor) {
//  struct node *test = mock_deftype(mod, "test");
//  struct typ ttest = { 0 };
//  ttest.definition = test;
//  assert(typ_generic_functor(&ttest) == NULL);
}

static size_t def_generic_arity(const struct typ *t) {
  const struct node *d = definition_const(t);
  if (node_can_have_genargs(d)) {
    return subs_count(subs_at_const(d, IDX_GENARGS));
  } else {
    return 0;
  }
}

size_t typ_generic_arity(const struct typ *t) {
  if (t->rdy & RDY_GEN) {
    return t->gen_arity;
  } else {
    return def_generic_arity(t);
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
  return node_toplevel_const(definition_const(t))
    ->generic->first_explicit_genarg;
}

static struct typ *def_generic_arg(struct typ *t, size_t n) {
  assert(n < typ_generic_arity(t));
  return subs_at_const(subs_at_const(definition_const(t), IDX_GENARGS), n)->typ;
}

struct typ *typ_generic_arg(struct typ *t, size_t n) {
  struct typ *r;
  if (t->rdy & RDY_GEN) {
    assert(n < t->gen_arity);
    r = t->gen_args[n];
  } else {
    r = def_generic_arg(t, n);
  }
  return olay(t, r);
}

bool typ_generic_arg_has_dependent_spec(const struct typ *t, size_t n) {
  const struct node *genargs = subs_at_const(typ_generic_functor_const(t)->definition, IDX_GENARGS);
  const struct node *ga = subs_at_const(genargs, n);
  assert(ga->which == DEFGENARG);
  return ga->as.DEFGENARG.has_dependent_spec;
}

struct typ *typ_as_non_tentative(const struct typ *t) {
  const size_t arity = typ_generic_arity(t);
  if (arity == 0) {
    return definition_const(t)->typ;
  } else if (typ_is_generic_functor(t)) {
    return definition_const(t)->typ;
  } else {
    struct typ **args = calloc(arity, sizeof(struct typ *));
    for (size_t n = 0; n < arity; ++n) {
      args[n] = typ_as_non_tentative(typ_generic_arg_const(t, n));
    }
    struct typ *i = NULL;
    error e = instantiate(&i, typ_module_owner(t), typ_for_error(t), -1,
                          typ_as_non_tentative(typ_generic_functor_const(t)),
                          args, arity, false);
    assert(!e);
    free(args);
    return i;
  }
}

size_t typ_function_arity(const struct typ *t) {
  assert(typ_is_function(t));
  return node_fun_all_args_count(definition_const(t));
}

size_t typ_function_min_arity(const struct typ *t) {
  assert(typ_is_function(t));
  return node_fun_min_args_count(definition_const(t));
}

size_t typ_function_max_arity(const struct typ *t) {
  assert(typ_is_function(t));
  return node_fun_max_args_count(definition_const(t));
}

ssize_t typ_function_first_vararg(const struct typ *t) {
  assert(typ_is_function(t));
  return node_fun_first_vararg(definition_const(t));
}

struct typ *typ_function_arg(struct typ *t, size_t n) {
  assert(n < typ_function_arity(t));
  return olay(t, subs_at_const(subs_at_const(definition_const(t), IDX_FUNARGS), n)->typ);
}

ident typ_function_arg_ident(const struct typ *t, size_t n) {
  assert(n < typ_function_arity(t));
  return node_ident(subs_at_const(subs_at_const(definition_const(t), IDX_FUNARGS), n));
}

enum token_type typ_function_arg_explicit_ref(const struct typ *t, size_t n) {
  assert(n < typ_function_arity(t));
  const struct node *dfun = t->definition;
  const struct node *funargs = subs_at_const(dfun, IDX_FUNARGS);
  const struct node *darg = subs_at_const(funargs, n);
  if (subs_last_const(darg)->which == UN) {
    return subs_last_const(darg)->as.UN.operator;
  }
  return 0;
}

struct typ *typ_function_return(struct typ *t) {
  assert(definition_const(t)->which == DEFFUN
         || definition_const(t)->which == DEFMETHOD);
  return olay(t, node_fun_retval_const(definition_const(t))->typ);
}

const struct typ *typ_generic_functor_const(const struct typ *t) {
  return typ_generic_functor(CONST_CAST(t));
}

const struct typ *typ_generic_arg_const(const struct typ *t, size_t n) {
  return typ_generic_arg(CONST_CAST(t), n);
}

const struct typ *typ_function_arg_const(const struct typ *t, size_t n) {
  return typ_function_arg(CONST_CAST(t), n);
}

const struct typ *typ_function_return_const(const struct typ *t) {
  return typ_function_return(CONST_CAST(t));
}

static size_t direct_isalist_count(const struct typ *t) {
  const struct node *def = definition_const(t);
  switch (def->which) {
  case DEFTYPE:
  case DEFINTF:
  case DEFINCOMPLETE:
    return subs_count(subs_at_const(def, IDX_ISALIST));
  default:
    return 0;
  }
}

static struct typ *direct_isalist(struct typ *t, size_t n) {
  const struct node *def = definition_const(t);
  switch (def->which) {
  case DEFTYPE:
  case DEFINTF:
  case DEFINCOMPLETE:
    return olay(t, subs_at_const(subs_at_const(def, IDX_ISALIST), n)->typ);
  default:
    assert(false);
    return 0;
  }
}

static const struct typ *direct_isalist_const(const struct typ *t, size_t n) {
  return direct_isalist(CONST_CAST(t), n);
}

static bool direct_isalist_exported(const struct typ *t, size_t n) {
  const struct node *def = definition_const(t);
  switch (def->which) {
  case DEFTYPE:
  case DEFINTF:
  case DEFINCOMPLETE:
    return subs_at_const(subs_at_const(def, IDX_ISALIST), n)->as.ISA.is_export;
  default:
    return 0;
  }
}

static ERROR do_typ_isalist_foreach(struct module *mod, struct typ *t, struct typ *base,
                                    uint32_t filter, isalist_each iter, void *user,
                                    bool *stop, struct fintypset *set) {
  const bool filter_not_exported = filter & ISALIST_FILTEROUT_NOT_EXPORTED;
  const bool filter_exported = filter & ISALIST_FILTEROUT_EXPORTED;
  const bool filter_trivial_isalist = filter & ISALIST_FILTEROUT_TRIVIAL_ISALIST;
  bool filter_nontrivial_isalist = filter & ISALIST_FILTEROUT_NONTRIVIAL_ISALIST;
  const bool filter_prevent_dyn = filter & ISALIST_FILTEROUT_PREVENT_DYN;

  if (filter_trivial_isalist && typ_is_trivial(base)) {
    return 0;
  }
  if (filter_nontrivial_isalist && !typ_is_trivial(base)) {
    return 0;
  }
  if (filter_prevent_dyn && typ_isa(base, TBI_PREVENT_DYN)) {
    return 0;
  }

  if (typ_is_trivial(base)) {
    // Trivial interfaces are often empty but refer to definitions in
    // nontrivial interfaces.
    filter &= ~ISALIST_FILTEROUT_NONTRIVIAL_ISALIST;
  }

  filter_nontrivial_isalist = filter & ISALIST_FILTEROUT_NONTRIVIAL_ISALIST;

  for (size_t n = 0; n < direct_isalist_count(base); ++n) {
    struct typ *intf = olay(t, direct_isalist(base, n));
    if (fintypset_get(set, intf) != NULL) {
      continue;
    }

    if (filter_prevent_dyn && typ_equal(intf, TBI_PREVENT_DYN)) {
      return 0;
    }

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
    if (filter_nontrivial_isalist && !typ_is_trivial(intf)) {
      continue;
    }

    if (exported) {
      filter &= ~ISALIST_FILTEROUT_NOT_EXPORTED;
    } else {
      filter &= ~ISALIST_FILTEROUT_EXPORTED;
    }

    fintypset_set(set, intf, true);

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
  bool stop = false;
  struct fintypset set;
  fintypset_fullinit(&set);

  const bool is_export = node_toplevel_const(definition_const(t))->flags & TOP_IS_EXPORT;
  const bool filter_exported = filter & ISALIST_FILTEROUT_EXPORTED;
  const bool filter_not_exported = filter & ISALIST_FILTEROUT_NOT_EXPORTED;
  if (!(filter_exported && is_export) && !(filter_not_exported && !is_export)) {
    error e = iter(mod, t, TBI_ANY, &stop, user);
    EXCEPT(e);

    if (stop) {
      return 0;
    }
    fintypset_set(&set, TBI_ANY, true);
  }

  error e = do_typ_isalist_foreach(mod, t, t, filter, iter, user, &stop, &set);
  fintypset_destroy(&set);
  EXCEPT(e);

  return 0;
}

EXAMPLE_NCC(direct_isalist_foreach) {
//  struct node *i_test = mock_defintf(mod, "`test");
//  struct node *test = mock_deftype(mod, "test");
//  G(isa, test->subs[IDX_ISALIST], ISA,
//     G_IDENT(isa_name, isa, "`test"));
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
struct typ *TBI_LITERALS_NIL;
struct typ *TBI_LITERALS_INTEGER;
struct typ *TBI_LITERALS_FLOATING;
struct typ *TBI_LITERALS_SLICE;
struct typ *TBI_LITERALS_STRING;
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
struct typ *TBI_UINT;
struct typ *TBI_INT;
struct typ *TBI_UINTPTR;
struct typ *TBI_INTPTR;
struct typ *TBI_FLOAT;
struct typ *TBI_DOUBLE;
struct typ *TBI_RUNE;
struct typ *TBI_STRING;
struct typ *TBI_STRING_COMPATIBLE;
struct typ *TBI_STATIC_ARRAY;
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
struct typ *TBI_VOIDREF;
struct typ *TBI_ANY_ANY_SLICE;
struct typ *TBI_ANY_SLICE;
struct typ *TBI_ANY_MUTABLE_SLICE;
struct typ *TBI_SLICE;
struct typ *TBI_MSLICE;
struct typ *TBI_SLICE_IMPL;
struct typ *TBI_SLICE_COMPATIBLE;
struct typ *TBI_OPTIONAL;
struct typ *TBI_VARARG;
struct typ *TBI_ARITHMETIC;
struct typ *TBI_HAS_BITWISE_OPERATORS;
struct typ *TBI_INTEGER_ARITHMETIC;
struct typ *TBI_OVERFLOW_ARITHMETIC;
struct typ *TBI_INTEGER_LITERAL_COMPATIBLE;
struct typ *TBI_INTEGER;
struct typ *TBI_UNSIGNED_INTEGER;
struct typ *TBI_NATIVE_INTEGER;
struct typ *TBI_NATIVE_SIZED_UNSIGNED_INTEGER;
struct typ *TBI_GENERALIZED_BOOLEAN;
struct typ *TBI_NATIVE_BOOLEAN;
struct typ *TBI_FLOATING;
struct typ *TBI_NATIVE_FLOATING;
struct typ *TBI_HAS_EQUALITY;
struct typ *TBI_NOT_HAS_EQUALITY;
struct typ *TBI_ORDERED;
struct typ *TBI_NOT_ORDERED;
struct typ *TBI_EQUALITY_BY_COMPARE;
struct typ *TBI_ORDERED_BY_COMPARE;
struct typ *TBI_COPYABLE;
struct typ *TBI_NOT_COPYABLE;
struct typ *TBI_DEFAULT_CTOR;
struct typ *TBI_NON_DEFAULT_CTOR;
struct typ *TBI_DEFAULT_DTOR;
struct typ *TBI_ARRAY_CTOR;
struct typ *TBI_TRIVIAL_COPY;
struct typ *TBI_TRIVIAL_COPY_BUT_OWNED;
struct typ *TBI_TRIVIAL_CTOR;
struct typ *TBI_TRIVIAL_ARRAY_CTOR;
struct typ *TBI_TRIVIAL_DTOR;
struct typ *TBI_TRIVIAL_COMPARE;
struct typ *TBI_TRIVIAL_EQUALITY;
struct typ *TBI_TRIVIAL_ORDER;
struct typ *TBI_RETURN_BY_COPY;
struct typ *TBI_NOT_RETURN_BY_COPY;
struct typ *TBI_ENUM;
struct typ *TBI_UNION;
struct typ *TBI_UNION_TRIVIAL_CTOR;
struct typ *TBI_RANGE;
struct typ *TBI_BOUNDS;
struct typ *TBI_COLLECTION;
struct typ *TBI_ITERATOR;
struct typ *TBI_ENVIRONMENT;
struct typ *TBI_ANY_ENVIRONMENT;
struct typ *TBI_PREVENT_DYN;
struct typ *TBI_INHERIT;
struct typ *TBI_ERROR;
struct typ *TBI__NOT_TYPEABLE;
struct typ *TBI__CALL_FUNCTION_SLOT;
struct typ *TBI__MUTABLE;
struct typ *TBI__MERCURIAL;

static bool __typ_equal(const struct typ *a, const struct typ *b) {
  const struct node *da = definition_const(a);
  const struct node *db = definition_const(b);
  if (da != db) {
    return false;
  }

  const size_t a_ga = typ_generic_arity(a);
  const size_t b_ga = typ_generic_arity(b);
  if (a_ga != b_ga) {
    return false;
  }

  if (a_ga == 0) {
    const bool a_tentative = typ_is_tentative(a);
    const bool b_tentative = typ_is_tentative(b);
    if (!a_tentative && !b_tentative) {
      return da == db;
    }

    return typ_as_non_tentative(a) == typ_as_non_tentative(b);
  } else {
    const bool a_functor = typ_is_generic_functor(a);
    const bool b_functor = typ_is_generic_functor(b);
    if (a_functor && b_functor) {
      return true;
    } else if (a_functor || b_functor) {
      return false;
    }

    const struct typ *a0 = typ_generic_functor_const(a);
    const struct typ *b0 = typ_generic_functor_const(b);
    if (!typ_equal(a0, b0)) {
      return false;
    }

    for (size_t n = 0; n < a_ga; ++n) {
      if (!typ_equal(typ_generic_arg_const(a, n),
                     typ_generic_arg_const(b, n))) {
        return false;
      }
    }

    return true;
  }
}

bool typ_equal(const struct typ *a, const struct typ *b) {
  if (a == b) {
    return true;
  }

  if ((a->rdy & RDY_HASH) && (b->rdy & RDY_HASH) && a->hash != b->hash) {
    return false;
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
  char *na = pptyp(mod, a);
  char *nb = pptyp(mod, b);
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
  default: assert(false); return NULL;
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
    return false;
  }
}

bool typ_is_generic_functor(const struct typ *t) {
  if (t->rdy & RDY_GEN) {
    if (t->gen_arity == 0) {
      return false;
    } else {
      return t->gen0 == t;
    }
  } else if (t->gen0 != NULL) {
    return t->gen0 == t;
  } else {
    return typ_generic_arity(t) > 0
      && t == def_generic_functor(CONST_CAST(t));
  }
}

bool typ_is_isalist_literal(const struct typ *t) {
  const struct node *d = definition_const(t);
  return d->which == DEFINCOMPLETE && d->as.DEFINCOMPLETE.is_isalist_literal;
}

// Rarely, for certains (a, intf), we don't have enough information in
// quickisa to answer. We could reduce that frequency further by adding more
// information in quickisa.
// There are however cases that are just inherently too numerous to record
// explicitly, such as
//  (Ref I32) isa (`Any_ref `Arithmetic)
// We do record that I32 isa `Arithmetic, though, so the recursive calls to
// typ_isa() don't generally go very deep.
//
// We could also cache the result of typ_isa(). Right now, the overall cost
// of typ_isa() is negligible.
static bool can_use_quickisa(const struct typ *a, const struct typ *intf) {
  return (typ_generic_functor_const(intf) == NULL
          || typ_is_generic_functor(intf)
          || typ_is_concrete(intf))
    && !(a->flags & TYPF_TUPLE)
    && !typ_is_isalist_literal(a)
    && !typ_is_isalist_literal(intf);
}

static bool __typ_isa(bool *quickisa_used, bool *quickisa_ret,
                      const struct typ *a, const struct typ *intf) {
  if (typ_equal(intf, TBI_ANY)) {
    return true;
  }

  if (typ_equal(a, intf)) {
    return true;
  }

  if (a->quickisa.ready && can_use_quickisa(a, intf)) {
#if CONFIG_DEBUG_QUICKISA
    *quickisa_used = true;
    *quickisa_ret = typset_has(&a->quickisa, intf);
#else
    return typset_has(&a->quickisa, intf);
#endif
  }

  const size_t a_ga = typ_generic_arity(a);
  if (a_ga > 0
      && !typ_is_generic_functor(a)
      && typ_is_generic_functor(intf)) {
    if (typ_isa(typ_generic_functor_const(a), intf)) {
      return true;
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
      return true;
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
      return true;
    }
  }

  for (size_t n = 0; n < direct_isalist_count(a); ++n) {
    if (typ_equal(direct_isalist_const(a, n), intf)) {
      return true;
    }
  }

  if (typ_is_isalist_literal(a)) {
    for (size_t n = 0; n < direct_isalist_count(a); ++n) {
      if (typ_isa(direct_isalist_const(a, n), intf)) {
        return true;
      }
    }
    return false;
  }

  if (typ_is_isalist_literal(intf)) {
    for (size_t n = 0; n < direct_isalist_count(intf); ++n) {
      if (!typ_isa(a, direct_isalist_const(intf, n))) {
        return false;
      }
    }
    return true;
  }

  for (size_t n = 0; n < direct_isalist_count(a); ++n) {
    if (typ_isa(direct_isalist_const(a, n), intf)) {
      return true;
    }
  }

  return false;
}

bool typ_isa(const struct typ *a, const struct typ *intf) {
#if CONFIG_DEBUG_QUICKISA
  bool quickisa_used = false, quickisa_ret = false;
  const bool ret = __typ_isa(&quickisa_used, &quickisa_ret, a, intf);
  assert(!quickisa_used || quickisa_ret == ret);
  return ret;
#else
  return __typ_isa(NULL, NULL, a, intf);
#endif
}

error typ_check_isa(const struct module *mod, const struct node *for_error,
                    const struct typ *a, const struct typ *intf) {
  if (typ_isa(a, intf)) {
    return 0;
  }

  error e = 0;
  char *na = pptyp(mod, a);
  char *nintf = pptyp(mod, intf);
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

bool typ_is_nullable_reference(const struct typ *t) {
  return t->flags & TYPF_NREF;
}

bool typ_is_slice(const struct typ *t) {
  return t->flags & TYPF_SLICE;
}

bool typ_is_optional(const struct typ *t) {
  return t->flags & TYPF_OPTIONAL;
}

bool typ_is_dyn(const struct typ *t) {
  if (typ_is_tentative(t)
      || !typ_is_reference(t)
      || typ_generic_arity(t) == 0) {
    return false;
  }

  const struct typ *a = typ_generic_arg_const(t, 0);
  return definition_const(a)->which == DEFINTF;
}

bool typ_is_dyn_compatible(const struct typ *t) {
  if (!typ_is_reference(t)
      || typ_generic_arity(t) == 0) {
    return false;
  }

  const struct typ *a = typ_generic_arg_const(t, 0);
  return definition_const(a)->which != DEFINTF;
}

error typ_check_is_reference(const struct module *mod, const struct node *for_error,
                             const struct typ *a) {
  if (typ_is_reference(a)) {
    return 0;
  }

  error e = 0;
  char *na = pptyp(mod, a);
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
    na = pptyp(mod, a);
    GOTO_EXCEPT_TYPE(try_node_module_owner_const(mod, for_error), for_error,
                     "'%s' type is not a reference", na);
  }

  if (typ_equal(a, TBI_ANY_ANY_REF)) {
    na = pptyp(mod, a);
    GOTO_EXCEPT_TYPE(try_node_module_owner_const(mod, for_error), for_error,
                     "cannot dereference type '%s'", na);
  }

  bool ok = false;
  switch (operator) {
  case TDEREFDOT:
  case TDEREFWILDCARD:
    ok = true;
    break;
  case TDEREFBANG:
    ok = typ_isa(a, TBI_ANY_MUTABLE_REF);
    break;
  case TDEREFSHARP:
    ok = typ_has_same_generic_functor(mod, a, TBI_MMREF)
      || typ_has_same_generic_functor(mod, a, TBI_NMMREF);
    break;
  default:
    assert(false);
  }
  if (ok) {
    return 0;
  }

  na = pptyp(mod, a);
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
  char *nt = pptyp(mod, t);
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

bool typ_is_concrete(const struct typ *t) {
  return t->flags & TYPF_CONCRETE;
}

bool typ_isa_return_by_copy(const struct typ *t) {
  return typ_isa(t, TBI_RETURN_BY_COPY);
}


static uint32_t instance_hash(const struct instance *i) {
  switch (i->which) {
  case INST_ENTRY:
    return (*i->as.ENTRY)->hash;
  case INST_QUERY_FINAL:
  case INST_QUERY_IDENTICAL:
    return i->as.QUERY->hash;
  default:
    assert(false);
    return 0;
  }
}

static int instance_cmp(const struct instance *a, const struct instance *b) {
  if (a->which == INST_ENTRY && b->which == INST_ENTRY
      && !typ_is_ungenarg(*a->as.ENTRY) && !typ_is_ungenarg(*b->as.ENTRY)) {
    return !typ_equal(*a->as.ENTRY, *b->as.ENTRY);
  }

  if (b->which != INST_ENTRY) {
    SWAP(a, b);
  }

  struct typ *ta = a->which == INST_ENTRY ? *a->as.ENTRY : a->as.QUERY;
  struct typ *tb = *b->as.ENTRY;

  switch (a->which) {
  case INST_QUERY_FINAL:
    if (typ_is_tentative(tb)) {
      return 1;
    }

    for (size_t n = 0; n < typ_generic_arity(tb); ++n) {
      const struct typ *ga = typ_generic_arg_const(ta, n);
      const struct typ *gb = typ_generic_arg_const(tb, n);
      if (!typ_equal(ga, gb)) {
        return 1;
      }
    }
    return 0;
  case INST_ENTRY:
  case INST_QUERY_IDENTICAL:
    // Use pointer comparison: we're looking for the exact same arguments
    // (same tentative bit, etc.).

    if (typ_is_tentative(ta) != typ_is_tentative(tb)) {
      return 1;
    }

    if (typ_generic_functor_const(ta) != typ_generic_functor_const(tb)) {
      return 1;
    }
    for (size_t n = 0; n < typ_generic_arity(tb); ++n) {
      const struct typ *ga = typ_generic_arg_const(ta, n);
      const struct typ *gb = typ_generic_arg_const(tb, n);
      if (ga != gb) {
        return 1;
      }
    }
    return 0;
  }

  return 1;
}

static struct vectyploc *get_ungenargs(struct module *mod) {
  if (mod == NULL
      || mod->state == NULL
      || mod->state->top_state == NULL) {
    return NULL;
  }
  struct node *top = mod->state->top_state->top;
  if (!node_is_at_top(top)) {
    top = parent(top);
  }
  struct generic *generic = node_toplevel(top)->generic;
  return generic == NULL ? NULL : &generic->ungenargs;
}

static struct vectyploc *get_tentatives(struct module *mod) {
  if (mod == NULL
      || mod->state == NULL
      || mod->state->top_state == NULL) {
    return NULL;
  }
  return &mod->state->top_state->tentatives;
}

IMPLEMENT_HTABLE_SPARSE(, instanceset, struct typ **, struct instance,
                        instance_hash, instance_cmp);

void instances_init(struct node *gendef) {
  struct generic *generic = node_toplevel(gendef)->generic;
  instanceset_init(&generic->finals, 0);
}

static void do_instances_add(struct module *mod, struct node *gendef,
                             struct typ **instance,
                             bool maintaining_tentatives);

void instances_maintain(struct module *mod, struct typ *genf) {
  struct node *gendef = definition(genf);
  struct generic *generic = node_toplevel(gendef)->generic;

  for (ssize_t n = 0; n < vectyploc_count(&generic->finals_nohash); ++n) {
    struct typ ***i = vectyploc_get(&generic->finals_nohash, n);
    if (typ_hash_ready(**i)) {
      do_instances_add(mod, gendef, *i, true);
      n += vectyploc_remove_replace_with_last(&generic->finals_nohash, n);
    }
  }

  struct vectyploc *tentatives = get_tentatives(mod);
  if (tentatives == NULL) {
    return;
  }

  for (ssize_t n = 0; n < vectyploc_count(tentatives); ++n) {
    struct typ ***i = vectyploc_get(tentatives, n);
    if (!typ_is_tentative(**i)) {
      do_instances_add(mod, gendef, *i, true);
      n += vectyploc_remove_replace_with_last(tentatives, n);
    }
  }
}

static void do_instances_add(struct module *mod, struct node *gendef,
                             struct typ **instance,
                             bool maintaining_tentatives) {
  struct generic *generic = node_toplevel(gendef)->generic;

  if (!maintaining_tentatives) {
    instances_maintain(mod, gendef->typ);
  }

  struct vectyploc *ungenargs = get_ungenargs(mod);

  if (ungenargs != NULL && typ_is_ungenarg(*instance)) {
    vectyploc_push(get_ungenargs(mod), instance);
    return;
  }

  struct vectyploc *tentatives = get_tentatives(mod);

  if (tentatives != NULL && typ_is_tentative(*instance)) {
    vectyploc_push(get_tentatives(mod), instance);
    return;
  }

  if (!typ_hash_ready(*instance)) {
    vectyploc_push(&generic->finals_nohash, instance);
    return;
  }

  struct instance i = { 0 };
  i.which = INST_ENTRY;
  i.as.ENTRY = instance;

  const bool already = instanceset_set(&generic->finals, i, instance);
  // When maintaining_tentatives, instance->typ has changed -- it was linked
  // -- since we last looked, so maybe its actual final value was already in
  // generic->finals all along.
  assert(maintaining_tentatives || !already);
}

void instances_add(struct module *mod, struct typ *genf, struct typ **instance) {
  assert(mod != NULL || !typ_is_tentative(*instance));

  struct node *gendef = definition(genf);
  do_instances_add(mod, gendef, instance, false);
}

static void prepare_query_tmp(struct typ *tmp, struct typ *functor,
                              struct typ **args, size_t arity,
                              bool is_ungenarg, bool tentative) {
  tmp->definition = definition(functor);
  tmp->gen_arity = arity;
  tmp->gen_args = calloc(arity, sizeof(*tmp->gen_args));
  // Not using set_typ(), 'tmp' doesn't live long enough to see linking.
  set_typ(&tmp->gen0, functor);
  memcpy(tmp->gen_args, args, arity * sizeof(*tmp->gen_args));
  tmp->rdy = RDY_GEN;

  create_flags(tmp, (functor->flags & TYPF_BUILTIN) ? functor : NULL);
  tmp->flags |= is_ungenarg ? TYPF_GENARG : 0;
  tmp->flags |= tentative ? TYPF_TENTATIVE : 0;

  typ_create_update_hash(tmp);
}

static bool has_null_arg(struct typ **args, size_t arity) {
  for (size_t n = 0; n < arity; ++n) {
    if (args[n] == NULL) {
      return true;
    }
  }
  return false;
}

struct typ *instances_find_existing_final_with(struct typ *genf,
                                               struct typ **args, size_t arity) {
  if (arity == 0) {
    return NULL;
  }
  if (has_null_arg(args, arity)) {
    return NULL;
  }

  struct node *gendef = definition(genf);
  struct typ *functor = typ_generic_functor(genf);
  assert(arity == typ_generic_arity(genf));

  if (!typ_hash_ready(functor)) {
    return NULL;
  }

  struct generic *generic = node_toplevel(CONST_CAST(gendef))->generic;

  instances_maintain(node_module_owner(gendef), genf);

  struct typ tmp = { 0 };
  prepare_query_tmp(&tmp, functor, args, arity, false, false);

  struct instance q = { 0 };
  q.which = INST_QUERY_FINAL;
  q.as.QUERY = &tmp;

  struct typ ***i = instanceset_get(&generic->finals, q);
  free(tmp.gen_args);
  unset_typ(&tmp.gen0);

  return i == NULL ? NULL : **i;
}

struct typ *instances_find_existing_final_like(const struct typ *_t) {
  struct typ *t = CONST_CAST(_t);
  if (!typ_hash_ready(t)) {
    return NULL;
  }

  struct node *gendef = definition(t);
  struct generic *generic = node_toplevel(CONST_CAST(gendef))->generic;

  instances_maintain(node_module_owner(gendef), t);

  struct instance q = { 0 };
  q.which = INST_QUERY_FINAL;
  q.as.QUERY = t;

  struct typ ***i = instanceset_get(&generic->finals, q);
  return i == NULL ? NULL : **i;
}

struct typ *instances_find_existing_identical(struct module *mod,
                                              struct typ *functor,
                                              struct typ **args, size_t arity) {
  if (has_null_arg(args, arity)) {
    return NULL;
  }

  bool is_ungenarg = false, is_tentative = false;
  is_it_ungenarg_or_tentative(&is_ungenarg, &is_tentative, functor, args, arity);

  BEGTIMEIT(TIMEIT_INSTANTIATE_FIND_EXISTING_IDENTICAL);

  struct node *gendef = definition(functor);
  struct generic *generic = node_toplevel(CONST_CAST(gendef))->generic;

  struct typ tmp = { 0 };
  prepare_query_tmp(&tmp, functor, args, arity, is_ungenarg, is_tentative);

  struct instance q = { 0 };
  q.which = INST_QUERY_IDENTICAL;
  q.as.QUERY = &tmp;

  struct vectyploc *ungenargs = get_ungenargs(mod);
  struct vectyploc *tentatives = get_tentatives(mod);

#if 0
  fprintf(stderr, "nohash:%zu finals:%u ungenargs:%zu tentatives:%zu ::: %s\n",
          vectyploc_count(&generic->finals_nohash),
          instanceset_count(&generic->finals),
          ungenargs ? vectyploc_count(ungenargs) : 0,
          tentatives ? vectyploc_count(tentatives) : 0,
          pptyp(NULL, &tmp));
#endif

  struct typ *ret = NULL;
  for (ssize_t n = 0;
       ungenargs != NULL && is_ungenarg
       && n < vectyploc_count(ungenargs);
       ++n) {
    struct typ ***i = vectyploc_get(ungenargs, n);

    struct instance f = { 0 };
    f.which = INST_ENTRY;
    f.as.ENTRY = *i;
    if (0 == instance_cmp(&f, &q)) {
      ret = **i;
      goto out;
    }
  }

  for (ssize_t n = 0;
       tentatives != NULL && is_tentative
       && n < vectyploc_count(tentatives);
       ++n) {
    struct typ ***i = vectyploc_get(tentatives, n);
    if (!typ_is_tentative(**i)) {
      // Also maintaining invariant: move finals out.
      do_instances_add(mod, gendef, *i, true);
      n += vectyploc_remove_replace_with_last(tentatives, n);
      continue;
    }

    struct instance f = { 0 };
    f.which = INST_ENTRY;
    f.as.ENTRY = *i;
    if (0 == instance_cmp(&f, &q)) {
      ret = **i;
      goto out;
    }
  }

  struct typ ***i = instanceset_get(&generic->finals, q);
  ret = i == NULL ? NULL : **i;

out:
  free(tmp.gen_args);
  unset_typ(&tmp.gen0);
  ENDTIMEIT(true, TIMEIT_INSTANTIATE_FIND_EXISTING_IDENTICAL);
  return ret;
}

char *typ_name(const struct module *mod, const struct typ *t) {
  if (typ_generic_arity(t) > 0 && !typ_is_generic_functor(t)) {
    return typ_name(mod, typ_generic_functor_const(t));
  } else if (definition_const(t) != NULL) {
    return scope_name(mod, &definition_const(t)->scope);
  } else {
    for (size_t n = ID_TBI__FIRST; n < ID_TBI__LAST; ++n) {
      if (mod->gctx->builtin_typs_by_name[n] == t) {
        return strdup(predefined_idents_strings[n]);
      }
    }
  }
  return NULL;
}

extern char *stpcpy(char *dest, const char *src);

static char *pptyp_defincomplete(char *r, const struct module *mod,
                                 const struct node *d) {
  char *s = r;
  s = stpcpy(s, idents_value(mod->gctx, node_ident(d)));
  s = stpcpy(s, "{");
  if (vecident_count(&d->as.DEFINCOMPLETE.idents) > 0) {
    struct node *md = CONST_CAST(d);
    for (size_t n = 0, count = vecident_count(&md->as.DEFINCOMPLETE.idents); n < count; ++n) {
      s = stpcpy(s, "\"");
      s = stpcpy(s, idents_value(mod->gctx,
                                 *vecident_get(&md->as.DEFINCOMPLETE.idents, n)));
      s = stpcpy(s, "\" ");
    }
  }
  const struct node *isalist = subs_at_const(d, IDX_ISALIST);
  FOREACH_SUB_CONST(i, isalist) {
    s = stpcpy(s, "isa ");
    char *n = pptyp(mod, i->typ);
    s = stpcpy(s, n);
    free(n);
    s = stpcpy(s, " ");
  }
  FOREACH_SUB_CONST(f, d) {
    if (f->which == DEFFIELD) {
      s = stpcpy(s, idents_value(mod->gctx, node_ident(f)));
      s = stpcpy(s, ":");
      char *n = pptyp(mod, f->typ);
      s = stpcpy(s, n);
      free(n);
      s = stpcpy(s, " ");
    }
  }
  s = stpcpy(s, "}");
  return r;
}

char *pptyp(const struct module *mod, const struct typ *t) {
  if (t == NULL) {
    return strdup("(null)");
  }

  if (mod == NULL) {
    mod = typ_module_owner(t);
  }

  char *r = calloc(2048, sizeof(char));
  char *s = r;

  s = stpcpy(s, typ_is_tentative(t) ? "*" : typ_is_ungenarg(t) ? "+" : "");

  const struct node *d = definition_const(t);
  if (d == NULL) {
    s = stpcpy(s, "<ZEROED>");
  } else if (d->which == IMPORT) {
    s = stpcpy(s, "<import>");
  } else if (d->which == DEFINCOMPLETE) {
    s = pptyp_defincomplete(s, mod, d);
  } else if (typ_generic_arity(t) == 0) {
    s = stpcpy(s, typ_name(mod, t));
  } else {
    s = stpcpy(s, "(");
    if (typ_is_generic_functor(t)) {
      s = stpcpy(s, "functor ");
      s = stpcpy(s, typ_name(mod, t));
    } else {
      const struct typ *f = typ_generic_functor_const(t);
      s = stpcpy(s, typ_is_tentative(f) ? "*" : "");
      s = stpcpy(s, typ_name(mod, f));
    }

    for (size_t n = 0; n < typ_generic_arity(t); ++n) {
      const struct typ *ga = typ_generic_arg_const(t, n);
      if (ga == NULL) {
        s = stpcpy(s, " null");
      } else {
        char *s2 = pptyp(mod, ga);
        s = stpcpy(s, " ");
        s = stpcpy(s, s2);
        free(s2);
      }
    }
    s = stpcpy(s, ")");
  }

  return r;
}

static ERROR ppisalist_each(struct module *mod, struct typ *t, struct typ *intf,
                            bool *stop, void *user) {
  fprintf(stderr, "\t%s\n", pptyp(mod, intf));
  return 0;
}

void ppisalist(const struct typ *t) {
  fprintf(stderr, "isalist: %s %p\n", pptyp(NULL, t), t);
  error e = typ_isalist_foreach(typ_module_owner(t), CONST_CAST(t), 0, ppisalist_each, NULL);
  assert(!e);
}

void pptypptrs(const struct typ *t) {
  fprintf(stderr, "%p (", t);
  if (typ_generic_arity(t) > 0) {
    fprintf(stderr, "%p ", typ_generic_functor_const(t));
    for (size_t n = 0, arity = typ_generic_arity(t); n < arity; ++n) {
      fprintf(stderr, "%p", typ_generic_arg_const(t, n));
    }
  }
  fprintf(stderr, ")\n");
}

void ppusers(const struct typ *t) {
  fprintf(stderr, "users: %s %p\n", pptyp(NULL, t), t);
  FOREACH_USER(idx, user, CONST_CAST(t), {
    fprintf(stderr, "\t%s ", pptyp(NULL, user));
    pptypptrs(user);
  });
}

void ppvectyp(struct vectyp *v) {
  for (size_t n = 0, arity = vectyp_count(v); n < arity; ++n) {
    struct typ **t = vectyp_get(v, n);
    fprintf(stderr, "%s ", pptyp(NULL, *t));
    pptypptrs(*t);
  }
}

void ppvectyploc(struct vectyploc *v) {
  for (size_t n = 0, arity = vectyploc_count(v); n < arity; ++n) {
    struct typ ***t = vectyploc_get(v, n);
    fprintf(stderr, "%s ", pptyp(NULL, **t));
    pptypptrs(**t);
  }
}
