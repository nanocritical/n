#ifndef TABLE_H__
#define TABLE_H__

#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "common.h"
#include "bitops.h"
#include "hash.h"

/*
 * The sparse/dense table and sparse hashtable follow the specifications of
 * Google's own C++ sparsetable sparse_hash_set.
 */

/*
 * Note: we often use memcpy(3) for assignment: it is necessary to go
 * around const restrictions when they go in our way.
 */

/*
 * Group size, same as the bitmap word size.
 */
#define SPTABLE_M ( BITS_PER_WORD )
#define SPTABLE_MIN_SIZE 2*SPTABLE_M

#define SPTABLE_NUM_GROUPS(tbl) ( div_ceil(unsigned, (tbl)->size, SPTABLE_M) )
#define SPTABLE_GROUP(i) ( (i) / SPTABLE_M )
#define SPTABLE_GROUP_POS(i) ( (i) % SPTABLE_M )
#define SPTABLE_GROUP_BITMAP(tbl, gr) \
    ( (tbl)->bitmap[SPTABLE_M/BITS_PER_WORD * (gr)] )

#define SPTABLE(name, type) \
    struct name { \
      unsigned size; \
      type **groups; \
      uint32_t *bitmap; \
      unsigned count; \
    }

#define declare_sptable(storage, name, type) \
    storage declare_sptable_init(name, type); \
storage declare_sptable_destroy(name, type); \
storage declare_sptable_count(name, type); \
storage declare_sptable_get(name, type); \
storage declare_sptable_set(name, type); \
storage declare_sptable_unset(name, type); \
storage declare_sptable_foreach(name, type)

#define implement_sptable(storage, name, type) \
    storage implement_sptable_init(name, type) \
storage implement_sptable_destroy(name, type) \
storage implement_sptable_count(name, type) \
storage implement_sptable_get(name, type) \
storage implement_sptable_set(name, type) \
storage implement_sptable_unset(name, type) \
storage implement_sptable_foreach(name, type)


#define declare_sptable_init(name, type) \
    void name ## _init(struct name *tbl, unsigned size)

#define declare_sptable_destroy(name, type) \
    void name ## _destroy(struct name *tbl)

#define declare_sptable_count(name, type) \
    unsigned name ## _count(struct name *tbl)

#define declare_sptable_get(name, type) \
    type *name ## _get(struct name *tbl, unsigned i)

#define declare_sptable_set(name, type) \
    void name ## _set(struct name *tbl, unsigned i, type el)

#define declare_sptable_unset(name, type) \
    void name ## _unset(struct name *tbl, unsigned i)

#define declare_sptable_foreach(name, type) \
    int name ## _foreach(struct name *ht, \
                         int (*iter)(type *val, \
                                     void *user), \
                         void *user);


#define implement_sptable_init(name, type) \
    void name ## _init(struct name *tbl, unsigned size) \
{ \
  tbl->size = max(unsigned, size, SPTABLE_MIN_SIZE); \
  tbl->groups = (type **)calloc(SPTABLE_NUM_GROUPS(tbl), \
                                sizeof(type *)); \
  tbl->bitmap = (uint32_t *)calloc(BITMAP_SIZE(tbl->size), 1); \
  tbl->count = 0; \
}

#define implement_sptable_destroy(name, type) \
    void name ## _destroy(struct name *tbl) \
{ \
  unsigned i, nb = SPTABLE_NUM_GROUPS(tbl); \
  type **g = tbl->groups; \
  \
  for (i = 0; i < nb; ++i, ++g) \
  if (*g) \
  free(*g); \
  free(tbl->groups); \
  tbl->groups = NULL; \
  free(tbl->bitmap); \
  tbl->bitmap = NULL; \
  tbl->size = 0; \
}

#define SPTABLE_GET__(tbl, gr, i) \
    ( (tbl)->groups[gr] \
     + bit_popcount(SPTABLE_GROUP_BITMAP(tbl, gr) \
                    & BITMAP_MASK_LAST(i+1)) \
     - 1 )

#define implement_sptable_count(name, type) \
    unsigned name ## _count(struct name *tbl) \
{ \
  return tbl->count; \
}

#define implement_sptable_get(name, type) \
    type *name ## _get(struct name *tbl, unsigned i) \
{ \
  if (bitmap_test(tbl->bitmap, i)) { \
    unsigned gr = SPTABLE_GROUP(i); \
    return SPTABLE_GET__(tbl, gr, i); \
  } else \
  return NULL; \
}

#define SPTABLE_GROUP_GROW__(type, tbl, gr, sz) \
    realloc((tbl)->groups[gr], \
            ((sz) + 1) * sizeof(type))

#define SPTABLE_GROUP_SHRINK__(type, tbl, gr, sz) \
    realloc((tbl)->groups[gr], \
            ((sz) - 1) * sizeof(type))

#define implement_sptable_set(name, type) \
    void name ## _set(struct name *tbl, unsigned i, type el) \
{ \
  unsigned gr = SPTABLE_GROUP(i); \
  if (bitmap_test(tbl->bitmap, i)) { \
    type *r = SPTABLE_GET__(tbl, gr, i); \
    memcpy(r, &el, sizeof(el)); \
  } else { \
    unsigned pos = SPTABLE_GROUP_POS(i); \
    uint32_t b = SPTABLE_GROUP_BITMAP(tbl, gr); \
    unsigned szb = bit_popcount(b & BITMAP_MASK_LAST(pos+1)); \
    unsigned sz = bit_popcount(b); \
    tbl->groups[gr] = (type *)SPTABLE_GROUP_GROW__(type, tbl, gr, sz); \
    memmove(tbl->groups[gr] + szb + 1, \
            tbl->groups[gr] + szb, \
            (sz - szb) * sizeof(type)); \
    memcpy(tbl->groups[gr] + szb, &el, sizeof(el)); \
    bitmap_set(tbl->bitmap, i); \
    tbl->count += 1; \
  } \
}

#define implement_sptable_unset(name, type) \
    void name ## _unset(struct name *tbl, unsigned i) \
{ \
  if (!bitmap_test(tbl->bitmap, i)) \
  return; \
  \
  unsigned gr = SPTABLE_GROUP(i); \
  unsigned pos = SPTABLE_GROUP_POS(i); \
  uint32_t b = SPTABLE_GROUP_BITMAP(tbl, gr); \
  unsigned szb = bit_popcount(b & BITMAP_MASK_LAST(pos+1)); \
  unsigned sz = bit_popcount(b); \
  memmove(tbl->groups[gr] + szb, \
          tbl->groups[gr] + szb + 1, \
          (sz - szb - 1) * sizeof(type)); \
  tbl->groups[gr] = (type *)SPTABLE_GROUP_SHRINK__(type, tbl, gr, sz); \
  bitmap_clear(tbl->bitmap, i); \
  tbl->count -= 1; \
}

#define implement_sptable_foreach(name, type) \
    int name ## _foreach(struct name *table, \
                         int (*iter)(type *val, \
                                     void *user), \
                         void *user) \
{ \
  unsigned g, bsize = SPTABLE_NUM_GROUPS(table); \
  int r; \
  \
  for (g = 0; g < bsize; ++g) { \
    uint32_t b = SPTABLE_GROUP_BITMAP(table, g); \
    int n; \
    \
    while ((n = bit_ffs(b))) { \
      type *u = SPTABLE_GET__(table, g, n-1); \
      r = iter(u, user); \
      if (r) \
      return r; \
      bitmap_clear(&b, n-1); \
    } \
  } \
  return 0; \
}



#define DNTABLE(name, type) \
    struct name { \
      unsigned size; \
      type *array; \
      uint32_t *bitmap; \
      unsigned count; \
    }

#define DNTABLE_MIN_SIZE SPTABLE_MIN_SIZE

#define declare_dntable(storage, name, type) \
    storage declare_dntable_init(name, type); \
storage declare_dntable_destroy(name, type); \
storage declare_dntable_count(name, type); \
storage declare_dntable_get(name, type); \
storage declare_dntable_set(name, type); \
storage declare_dntable_foreach(name, type)

#define implement_dntable(storage, name, type) \
    storage implement_dntable_init(name, type) \
storage implement_dntable_destroy(name, type) \
storage implement_dntable_count(name, type) \
storage implement_dntable_get(name, type) \
storage implement_dntable_set(name, type) \
storage implement_dntable_foreach(name, type)


#define declare_dntable_init(name, type) \
    void name ## _init(struct name *tbl, unsigned size)

#define declare_dntable_destroy(name, type) \
    void name ## _destroy(struct name *tbl)

#define declare_dntable_count(name, type) \
    unsigned name ## _count(struct name *tbl)

#define declare_dntable_get(name, type) \
    type *name ## _get(struct name *tbl, unsigned i)

#define declare_dntable_set(name, type) \
    void name ## _set(struct name *tbl, unsigned i, type el)

#define declare_dntable_foreach(name, type) \
    int name ## _foreach(struct name *ht, \
                         int (*iter)(type *val, \
                                     void *user), \
                         void *user);

#define implement_dntable_init(name, type) \
    void name ## _init(struct name *tbl, unsigned size) \
{ \
  tbl->size = max(unsigned, size, DNTABLE_MIN_SIZE); \
  tbl->array = (type *)calloc(tbl->size, sizeof(type)); \
  tbl->bitmap = (uint32_t *)calloc(BITMAP_SIZE(tbl->size), 1); \
  tbl->count = 0; \
}

#define implement_dntable_destroy(name, type) \
    void name ## _destroy(struct name *tbl) \
{ \
  free(tbl->array); \
  tbl->array = NULL; \
  free(tbl->bitmap); \
  tbl->bitmap = NULL; \
  tbl->size = 0; \
}

#define implement_dntable_count(name, type) \
    unsigned name ## _count(struct name *tbl) \
{ \
  return tbl->count; \
}

#define implement_dntable_get(name, type) \
    type *name ## _get(struct name *tbl, unsigned i) \
{ \
  if (bitmap_test(tbl->bitmap, i)) \
  return &tbl->array[i]; \
  else \
  return NULL; \
}

#define implement_dntable_set(name, type) \
    void name ## _set(struct name *tbl, unsigned i, type el) \
{ \
  memcpy(tbl->array + i, &el, sizeof(el)); \
  if (!bitmap_test(tbl->bitmap, i)) { \
    tbl->count += 1; \
  } \
  bitmap_set(tbl->bitmap, i); \
}

#define implement_dntable_foreach(name, type) \
    int name ## _foreach(struct name *table, \
                         int (*iter)(type *val, \
                                     void *user), \
                         void *user) \
{ \
  unsigned g, blast = BITMAP_WORD(table->size - 1); \
  int r; \
  for (g = 0; g <= blast; ++g) { \
    uint32_t b = table->bitmap[g]; \
    int n; \
    \
    while ((n = bit_ffs(b))) { \
      type *u = &table->array[g*BITS_PER_WORD + n-1]; \
      r = iter(u, user); \
      if (r) \
      return r; \
      bitmap_clear(&b, n-1); \
    } \
  } \
  return 0; \
}



enum _HtableFlag {
  HTABLE_HAS_DELETE = 1,
  HTABLE_DONT_SHRINK = 2,
};

#define HTABLE_SPARSE(name, type, key_type) \
    struct name ## _unit__ { \
      type val; \
      const key_type key; \
      uint32_t hkey; \
    } packed__; \
SPTABLE(name ## _table__, struct name ## _unit__); \
struct name { \
  struct name ## _table__ table; \
  unsigned hweight; \
  unsigned flag; \
  type delete_val; \
  uint32_t (*hashf)(const key_type *); \
  int (*cmpf)(const key_type *, const key_type *); \
}

#define declare_htable_sparse(storage, name, type, key_type) \
    declare_sptable(storage, name ## _table__, struct name ## _unit__); \
storage declare_htable_init(name, type, key_type); \
storage declare_htable_destroy(name, type, key_type); \
storage declare_htable_set_delete_val(name, type, key_type); \
storage declare_htable_set_custom_hashf(name, type, key_type); \
storage declare_htable_set_custom_cmpf(name, type, key_type); \
storage declare_htable_count(name, type, key_type); \
storage declare_htable_get(name, type, key_type); \
storage declare_htable_set(name, type, key_type); \
storage declare_htable_foreach(name, type, key_type); \
storage declare_htable_rehash(name, type, key_type)

#define implement_htable_sparse(storage, name, type, key_type) \
    implement_sptable(storage, name ## _table__, struct name ## _unit__); \
storage implement_htable_init(name, type, key_type); \
storage implement_htable_destroy(name, type, key_type); \
storage implement_htable_set_delete_val(name, type, key_type); \
storage implement_htable_set_custom_hashf(name, type, key_type);\
storage implement_htable_set_custom_cmpf(name, type, key_type); \
storage implement_htable_count(name, type, key_type); \
storage implement_htable_get(name, type, key_type); \
storage implement_htable_set(storage, name, type, key_type); \
implement_htable_foreach(storage, name, type, key_type); \
implement_htable_rehash(storage, name, type, key_type)


/*
 * A unit holds the object put in the table, the key and its full hash (as
 * opposed to the hash modulo the size of the table, used as the first element
 * of the quadratic sequence), which will be used when growing/shrinking the
 * table.
 */
#define HTABLE_DENSE(name, type, key_type) \
    struct name ## _unit__ { \
      type val; \
      const key_type key; \
      uint32_t hkey; \
    } packed__; \
DNTABLE(name ## _table__, struct name ## _unit__); \
struct name { \
  struct name ## _table__ table; \
  unsigned hweight; \
  unsigned flag; \
  type delete_val; \
  uint32_t (*hashf)(const key_type *); \
  int (*cmpf)(const key_type *, const key_type *); \
}

#define declare_htable_dense(storage, name, type, key_type) \
    declare_dntable(storage, name ## _table__, struct name ## _unit__); \
storage declare_htable_init(name, type, key_type); \
storage declare_htable_destroy(name, type, key_type); \
storage declare_htable_set_delete_val(name, type, key_type); \
storage declare_htable_set_custom_hashf(name, type, key_type); \
storage declare_htable_set_custom_cmpf(name, type, key_type); \
storage declare_htable_count(name, type, key_type); \
storage declare_htable_get(name, type, key_type); \
storage declare_htable_set(name, type, key_type); \
storage declare_htable_foreach(name, type, key_type); \
storage declare_htable_rehash(name, type, key_type)

#define implement_htable_dense(storage, name, type, key_type) \
    implement_dntable(storage, name ## _table__, struct name ## _unit__); \
storage implement_htable_init(name, type, key_type); \
storage implement_htable_destroy(name, type, key_type); \
storage implement_htable_set_delete_val(name, type, key_type); \
storage implement_htable_set_custom_hashf(name, type, key_type);\
storage implement_htable_set_custom_cmpf(name, type, key_type); \
storage implement_htable_count(name, type, key_type); \
storage implement_htable_get(name, type, key_type); \
storage implement_htable_set(storage, name, type, key_type); \
implement_htable_foreach(storage, name, type, key_type); \
implement_htable_rehash(storage, name, type, key_type)



#define declare_htable_init(name, type, key_type) \
    void name ## _init(struct name *ht, unsigned size)

#define declare_htable_destroy(name, type, key_type) \
    void name ## _destroy(struct name *ht)

#define declare_htable_set_delete_val(name, type, key_type) \
    void name ## _set_delete_val(struct name *ht, type d)

#define declare_htable_set_custom_hashf(name, type, key_type) \
    void name ## _set_custom_hashf(struct name *ht, \
                                   uint32_t (*hashf)(const key_type *))

#define declare_htable_set_custom_cmpf(name, type, key_type) \
    void name ## _set_custom_cmpf(struct name *ht, \
                                  int (*cmpf)(const key_type *, \
                                              const key_type *))

#define declare_htable_count(name, type, key_type) \
    unsigned name ## _count(struct name *ht)

#define declare_htable_get(name, type, key_type) \
    type *name ## _get(struct name *ht, const key_type k)

#define declare_htable_set(name, type, key_type) \
    int name ## _set(struct name *ht, const key_type k, type v)

#define declare_htable_foreach(name, type, key_type) \
    int name ## _foreach(struct name *ht, \
                         int (*iter)(const key_type *key, \
                                     type *val, \
                                     void *user), \
                         void *user);

#define declare_htable_rehash(name, type, key_type) \
    void name ## _rehash(struct name *hta, struct name *htb)


#define HTABLE_TOOFULL(ht) \
    ( (uint64_t)(ht)->hweight * 32 / (ht)->table.size > 25 )
#define HTABLE_TOOEMPTY(ht) \
    ( ( (uint64_t)(ht)->hweight * 32 / (ht)->table.size < 3 ) \
     && (ht)->table.size > SPTABLE_MIN_SIZE )



/* Size is a power of 2, should be less than UINT_MAX/2 */
#define implement_htable_init(name, type, key_type) \
    void name ## _init(struct name *ht, unsigned size) \
{ \
  assert(size <= UINT_MAX/2); \
  name ## _table___init(&(ht->table), \
                        roundup_pow2(size)); \
  ht->hweight = 0; \
  ht->flag = size > 0 ? HTABLE_DONT_SHRINK : 0; \
  ht->hashf = NULL; \
  ht->cmpf = NULL; \
}

#define implement_htable_destroy(name, type, key_type) \
    void name ## _destroy(struct name *ht) \
{ \
  name ## _table___destroy(&(ht->table)); \
}

#define implement_htable_set_delete_val(name, type, key_type) \
    void name ## _set_delete_val(struct name *ht, type d) \
{ \
  ht->delete_val = d; \
  ht->flag |= HTABLE_HAS_DELETE; \
}

#define implement_htable_set_custom_hashf(name, type, key_type) \
    void name ## _set_custom_hashf(struct name *ht, \
                                   uint32_t (*hashf)(const key_type *)) \
{ \
  ht->hashf = hashf; \
}

#define implement_htable_set_custom_cmpf(name, type, key_type) \
    void name ## _set_custom_cmpf(struct name *ht, \
                                  int (*cmpf)(const key_type *, \
                                              const key_type *)) \
{ \
  ht->cmpf = cmpf; \
}

#define implement_htable_count(name, type, key_type) \
    unsigned name ## _count(struct name *ht) \
{ \
  return name ## _table___count(&((ht)->table)); \
}

/* Quadratic open addressing (see Knuth TAOCP 6.4 exercise 20) */
#define __htable_idx(ht, hash, n) \
    ( ((hash) + (n)*((n)+1)/2) % (ht)->table.size )

#define HTABLE_GET__(name, ht, hash, n) \
    name ## _table___get(&((ht)->table), __htable_idx(ht, hash, n))

#define HTABLE_CMP_KEYS__(ht, k1, k2) \
    ( (ht)->cmpf ? (ht)->cmpf(k1, k2) \
     : memcmp(k1, k2, sizeof(*(k1))) )

#include <stdio.h>

#define implement_htable_get(name, type, key_type) \
    type *name ## _get(struct name *ht, const key_type k) \
{ \
  const key_type *pk = &k; \
  struct name ## _unit__ *b; \
  uint32_t hash; \
  unsigned n = 0; \
  \
  if (ht->hashf) \
  hash = ht->hashf(pk); \
  else \
  hash = hash32((const char *)pk, sizeof(*pk)); \
  do { \
    b = HTABLE_GET__(name, ht, hash, n); \
    n += 1; \
  } while (b && (hash != b->hkey \
                 || HTABLE_CMP_KEYS__(ht, (const key_type *) \
                                      &(b->key), &k) != 0)); \
  \
  return b ? &(b->val) : NULL; \
}

#define HTABLE_GROW__(name, ht) do { \
  struct name htmp; \
  name ## _init(&htmp, 2 * (ht)->table.size - 1); \
  htmp.cmpf = ht->cmpf; \
  htmp.hashf = ht->hashf; \
  htmp.delete_val = ht->delete_val; \
  htmp.flag = ht->flag; \
  int oldflag = htmp.flag; \
  htmp.flag |= HTABLE_DONT_SHRINK; \
  name ## _rehash(&htmp, ht); \
  name ## _destroy(ht); \
  htmp.flag = oldflag; \
  *ht = htmp; \
} while (0)

#define HTABLE_SHRINK__(name, ht) /* TODO not implemented */

#define HTABLE_SET__(name, ht, hk, n, b) \
    name ## _table___set(&((ht)->table), \
                         __htable_idx(ht, hk, n), b)

/*
 * We do not overwrite, as that can potentially lead to memory leakages:
 * the overwritten object can become dangling. _set() returns 1 when an
 * object with a matching key is already present and the new object wasn't
 * added to the table.
 *
 * The unit is allocated by the underlying sptable. Below, bnew is just a
 * temporary.
 */
#define implement_htable_set(storage, name, type, key_type) \
    void name ## _rehash(struct name *hta, struct name *htb); \
storage int name ## _set(struct name *ht, const key_type k, type v) \
{ \
  const key_type *pk = &k; \
  struct name ## _unit__ *b; \
  uint32_t hash; \
  unsigned n = 0; \
  int cmph = 1, cmp = 0; \
  \
  if (HTABLE_TOOFULL(ht)) \
  HTABLE_GROW__(name, ht); \
  else if (HTABLE_TOOEMPTY(ht) \
           && !(ht->flag & HTABLE_DONT_SHRINK)) \
  HTABLE_SHRINK__(name, ht); \
  \
  if (ht->hashf) \
  hash = ht->hashf(pk); \
  else \
  hash = hash32((const char *)pk, sizeof(*pk)); \
  \
  do { \
    b = HTABLE_GET__(name, ht, hash, n); \
    if (b && ((cmph = b->hkey != hash) \
              || (cmp = HTABLE_CMP_KEYS__(ht, \
                                          (const key_type *) &b->key, \
                                          (const key_type *) &k)) != 0)) \
    n += 1; \
    else \
    break; \
  } while (1); \
  \
  if (!cmph && cmp == 0) \
  return 1; \
  else { \
    struct name ## _unit__ bnew = { \
      v, \
      k, \
      hash, \
    }; \
    HTABLE_SET__(name, ht, hash, n, bnew); \
    ht->hweight += 1; \
    return 0; \
  } \
}

#define implement_htable_foreach(storage, name, type, key_type) \
    storage int name ## _foreach_iter__(struct name ## _unit__ *u, \
                                        void *user) \
{ \
  struct { \
    int (*iter)(const key_type *, type *, void *); \
    void *user; \
  } *s; \
  s = (__typeof__(s))user; \
  \
  return s->iter(&u->key, &u->val, s->user); \
} \
storage int name ## _foreach(struct name *ht, \
                             int (*iter)(const key_type *key, \
                                         type *val, \
                                         void *user), \
                             void *user) \
{ \
  struct name ## _table__ *table = &ht->table; \
  struct { \
    int (*iter)(const key_type *, type *, void *); \
    void *user; \
  } s = { iter, user }; \
  \
  return name ## _table___foreach(table, \
                                  name ## _foreach_iter__, &s); \
}

/*
 * Note: rehashing will not preserve the structure of the sptable in any way.
 */
#define implement_htable_rehash(storage, name, type, key_type) \
    storage int name ## _rehash_iter__(const key_type *key, \
                                       type *val, \
                                       void *user) \
{ \
  struct name *ht = (struct name *)user; \
  name ## _set(ht, *key, *val); \
  return 0; \
} \
storage void name ## _rehash(struct name *hta, struct name *htb)\
{ \
  name ## _foreach(htb, name ## _rehash_iter__, hta); \
}

#endif
