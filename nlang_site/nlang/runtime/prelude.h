#ifndef RUNTIME_PRELUDE_H_
#define RUNTIME_PRELUDE_H_

#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

#define NLANG_RUNTIME_ERROR() abort()

#define NLANG_RUNTIME_ERROR_ON(cond) do { \
  if (cond) { \
    NLANG_RUNTIME_ERROR(); \
  } \
} while (0)

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef size_t size;
typedef ssize_t ssize;
typedef _Bool bool;

typedef uint8_t* nlangp__u8;
typedef const uint8_t* nlangcp__u8;

#define null NULL

static inline nlangp__u8 nlang_unsafe_realloc(nlangp__u8 data, size oldlen, size newlen) {
  nlangp__u8 p = realloc(data, newlen);
  if (newlen > oldlen) {
    memset(p + oldlen, 0, newlen - oldlen);
  }
  return p;
}

static inline nlangp__u8 nlang_unsafe_malloc(size len) {
  return calloc(len, 1);
}

static inline void nlang_unsafe_free(nlangp__u8 p, size len) {
  (void) len;
  return free(p);
}

static inline void nlang_unsafe_memcpy(__restrict__ nlangp__u8 dst, __restrict__ nlangcp__u8 src, size elsize, size n) {
  memcpy(dst, src, n * elsize);
}

static inline void nlang_slice_unsafe_slice_clear(nlangp__u8 data, size elsize,
                                                  size start, size last) {
  memset(data + (start * elsize), 0, elsize * (last - start + 1));
}

static inline nlangp__u8 nlang_slice_unsafe_slice_addr(nlangp__u8 data, size elsize,
                                                       size n) {
  return data + elsize * n;
}

#define NLANG_UNREACHED() { abort(); }

#endif // RUNTIME_PRELUDE_H_
