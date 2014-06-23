#ifndef VECTOR_H__
#define VECTOR_H__

#include "common.h"

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#if CONFIG_VECTOR_BOUND_CHECKS == 1
# define VECTOR_ASSERT(c) assert(c)
#else
# define VECTOR_ASSERT(c)
#endif

// The size of the 'large' storage is roundup_pow2(v->count).
#define VECTOR(name, type, small_count) \
  struct name { \
    size_t count; \
    type small[small_count]; \
    type *large; \
  }

#define VECTOR_SMALL_COUNT(name) ARRAY_SIZE(((struct name *) NULL)->small)

#define DECLARE_VECTOR(name, type) \
  void name ## _destroy(struct name *v); \
  size_t name ## _count(const struct name *v); \
  type *name ## _array(struct name *v); \
  type *name ## _set(struct name *v, size_t i, type el); \
  type *name ## _push(struct name *v, type el); \
  type *name ## _get(struct name *v, size_t i); \
  type name ## _pop(struct name *v); \
  void name ## _resize__(struct name *v, size_t new_count, bool skip_copy); \
  void name ## _resize(struct name *v, size_t new_count); \
  void name ## _copy(struct name *d, const struct name *s);

// Implementation note: the internal call name##_resize__(v, cnt,
// skip_copy=true) avoids some of the copies that name##_resize() would do
// when resizing a vector for name##_copy(), but it's still using realloc()
// which may not be optimal.

#define IMPLEMENT_VECTOR(storage, name, type) \
  \
storage void name ## _destroy(struct name *v) { \
  v->count = 0; \
  free(v->large); \
  v->large = NULL; \
  memset(v->small, 0, VECTOR_SMALL_COUNT(name) * sizeof(type)); \
} \
\
storage size_t name ## _count(const struct name *v) { \
  return v->count; \
} \
\
storage type *name ## _array(struct name *v) { \
  if (v->count <= VECTOR_SMALL_COUNT(name)) { \
    return v->small; \
  } else { \
    return v->large; \
  } \
} \
\
storage void name ## _resize__(struct name *v, size_t new_count, \
                               bool skip_copy) { \
  if (new_count == v->count) { \
    return; \
    \
  } else if (new_count > v->count) { \
    if (v->count <= VECTOR_SMALL_COUNT(name)) { \
      if (new_count > VECTOR_SMALL_COUNT(name)) { \
        v->large = calloc(roundup_pow2(new_count), sizeof(type)); \
        if (!skip_copy) { \
          memcpy(v->large, v->small, new_count * sizeof(type)); \
        } \
        memset(v->small, 0, VECTOR_SMALL_COUNT(name) * sizeof(type)); \
      } \
    } else { \
      const size_t grown_size = roundup_pow2(new_count); \
      if (grown_size > roundup_pow2(v->count)) { \
        v->large = realloc(v->large, grown_size * sizeof(type)); \
        memset(v->large + v->count, 0, (new_count - v->count) * sizeof(type)); \
      } \
    } \
    \
  } else { \
    if (v->count <= VECTOR_SMALL_COUNT(name)) { \
      memset(v->small + new_count, 0, \
             (VECTOR_SMALL_COUNT(name) - new_count) * sizeof(type)); \
    } else { \
      if (new_count <= VECTOR_SMALL_COUNT(name)) { \
        if (!skip_copy) { \
          memcpy(v->small, v->large, new_count * sizeof(type)); \
        } \
        free(v->large); \
        v->large = NULL; \
      } else { \
        const size_t shrunk_size = roundup_pow2(new_count); \
        if (shrunk_size < roundup_pow2(v->count)) { \
          v->large = realloc(v->large, shrunk_size * sizeof(type)); \
          memset(v->large + new_count, 0, \
                 (shrunk_size - new_count) * sizeof(type)); \
        } \
      } \
    } \
  } \
  \
  v->count = new_count; \
} \
\
storage void name ## _resize(struct name *v, size_t new_count) {\
  name##_resize__(v, new_count, false); \
} \
\
storage type *name ## _set(struct name *v, size_t i, type el) { \
  VECTOR_ASSERT(i < v->count); \
  if (v->count <= VECTOR_SMALL_COUNT(name)) { \
    v->small[i] = el; \
    return &v->small[i]; \
  } else { \
    v->large[i] = el; \
    return &v->large[i]; \
  } \
} \
\
storage type *name ## _push(struct name *v, type el) { \
  name##_resize(v, v->count + 1); \
  return name##_set(v, v->count - 1, el); \
} \
\
storage type *name ## _get(struct name *v, size_t i) { \
  VECTOR_ASSERT(i < v->count); \
  if (v->count <= VECTOR_SMALL_COUNT(name)) { \
    return &v->small[i]; \
  } else { \
    return &v->large[i]; \
  } \
} \
\
storage type name ## _pop(struct name *v) { \
  VECTOR_ASSERT(v->count > 0); \
  type el = *name##_get(v, v->count - 1); \
  name##_resize(v, v->count - 1); \
  return el; \
} \
\
storage void name ## _copy(struct name *d, const struct name *s) { \
  name##_resize__(d, s->count, true); \
  \
  if (s->count <= VECTOR_SMALL_COUNT(name)) { \
    memcpy(d->small, s->small, s->count * sizeof(type)); \
  } else { \
    memcpy(d->large, s->large, s->count * sizeof(type)); \
  } \
}

#endif
