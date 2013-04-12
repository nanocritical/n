#ifndef COMMON_H__
#define COMMON_H__

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

#define packed__ __attribute__((__packed__))
#define unused__ __attribute__((__unused__))
#define pure__ __attribute__((__pure__))
#define noinline__ __attribute__((noinline))
#define warn_unused_result__ __attribute__((__warn_unused_result__))

#define likely(x) __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)

#define _STRINGIFY(x) #x
#define STRINGIFY(x) _STRINGIFY(x)

typedef _Bool bool;
#define TRUE 1
#define FALSE 0

typedef int error;

#define EXCEPT(e) do { \
  if (e) { \
    fprintf(stderr, "\tE: %s:%d: %s()\n", __FILE__, __LINE__, __func__); \
    return e; \
  } \
} while (0)

#define GOTO_EXCEPT(_e) do { \
  if (_e) { \
    e = _e; \
    fprintf(stderr, "\tE: %s:%d: %s()\n", __FILE__, __LINE__, __func__); \
    goto except; \
  } \
} while (0)

#define EXCEPTF(e, fmt, ...) do { \
  if (e) { \
    fprintf(stderr, fmt "\n", ##__VA_ARGS__); \
    fprintf(stderr, "\tE: %s:%d: %s(): %s\n", __FILE__, __LINE__, __func__, strerror(e)); \
    return e; \
  } \
} while (0)

#define GOTO_EXCEPTF(_e, fmt, ...) do { \
  if (_e) { \
    e = _e; \
    fprintf(stderr, fmt "\n", ##__VA_ARGS__); \
    fprintf(stderr, "\tE: %s:%d: %s(): %s\n", __FILE__, __LINE__, __func__, strerror(e)); \
    goto except; \
  } \
} while (0)

#define ARRAY_SIZE(a) ( sizeof(a) / sizeof(a[0]) )

#define min(type, a, b) ({ \
  type __min_a = a; \
  type __min_b = b; \
  __min_a <= __min_b ? __min_a : __min_b; })

#define max(type, a, b) ({ \
  type __max_a = a; \
  type __max_b = b; \
  __max_a >= __max_b ? __max_a : __max_b; })


#define SLICE(v, hi, lo) ( ((v) >> (lo)) & ((1ULL << ((hi) - (lo) + 1)) - 1) )

/* sizeof() doesn't evaluate it's argument */
#define bit_ffs(x) \
    ( sizeof(x) == sizeof(int) ? __builtin_ffs(x) : \
     sizeof(x) == sizeof(long) ? __builtin_ffsl(x) : \
     sizeof(x) == sizeof(long long) ? __builtin_ffsll(x) : \
     ({ assert(0); 0; }) )
// Undefined for [x == 0].
#define bit_clz(x) \
    ( sizeof(x) == sizeof(int) ? __builtin_clz(x) : \
     sizeof(x) == sizeof(long) ? __builtin_clzl(x) : \
     sizeof(x) == sizeof(long long) ? __builtin_clzll(x) : \
     ({ assert(0); 0; }) )
// Undefined for [x == 0].
#define bit_ctz(x) \
    ( sizeof(x) == sizeof(int) ? __builtin_ctz(x) : \
     sizeof(x) == sizeof(long) ? __builtin_ctzl(x) : \
     sizeof(x) == sizeof(long long) ? __builtin_ctzll(x) : \
     ({ assert(0); 0; }) )

/*
 * fls "find last set"
 * Returns one plus the index of the most significant 1-bit of X, or
 * if X is zero, returns zero (which is non-sensical but predictable).
 */
#define bit_fls(x) ( likely(x != 0) ? sizeof(x)*8 - 1 - bit_clz(x) : 0 )

/*
 * GCC __builtin_popcount, at least on x86 and x86_64, is implemented with
 * a table and a loop, which is relatively fast but has a higher
 * instruction fetch count (and data) than the implementation here. Other
 * architectures, like IA64, will have an instruction for popcount.
 * (see gcc-4.3/gcc/libgcc2.c)
 */
#if 0
#define bit_popcount(x) \
    ( sizeof(x) == sizeof(int) ? __builtin_popcount(x) : \
     sizeof(x) == sizeof(long) ? __builtin_popcountl(x) : \
     sizeof(x) == sizeof(long long) ? __builtin_popcountll(x) : \
     ({ assert(0); 0; }) )
#else
#define bit_popcount(x) \
    ( sizeof(x) == sizeof(uint32_t) ? hweight32(x) : \
     sizeof(x) == sizeof(uint64_t) ? hweight64(x) : \
     ({ assert(0); 0; }) )
#endif

static inline unsigned hweight32(uint32_t x) {
  uint32_t w = x - ((x >> 1) & 0x55555555);
  w = (w & 0x33333333) + ((w >> 2) & 0x33333333);
  w = (w + (w >> 4)) & 0x0F0F0F0F;
  w = w + (w >> 8);
  return (w + (w >> 16)) & 0x000000FF;
}

static inline unsigned hweight64(uint64_t x) {
  uint64_t w = x - ((x >> 1) & 0x5555555555555555ULL);
  w = (w & 0x3333333333333333ULL) + ((w >> 2) & 0x3333333333333333ULL);
  w = (w + (w >> 4)) & 0x0f0f0f0f0f0f0f0fULL;
  return (w * 0x0101010101010101ULL) >> 56;
}

#define log2_ceil(x) ( likely(x > 1) ? bit_fls(x-1) + 1 : 0 )

static inline uint16_t bitreverse16(uint16_t x) {
  x = ((x >> 1) & 0x5555) | ((x & 0x5555) << 1);
  x = ((x >> 2) & 0x3333) | ((x & 0x3333) << 2);
  x = ((x >> 4) & 0x0f0f) | ((x & 0x0f0f) << 4);
  x = (x >> 8) | (x << 8);
  return x;
}

static inline uint32_t bitreverse32(uint32_t x) {
  x = ((x >> 1) & 0x55555555) | ((x & 0x55555555) << 1);
  x = ((x >> 2) & 0x33333333) | ((x & 0x33333333) << 2);
  x = ((x >> 4) & 0x0f0f0f0f) | ((x & 0x0f0f0f0f) << 4);
  x = ((x >> 8) & 0x00ff00ff) | ((x & 0x00ff00ff) << 8);
  x = (x >> 16) | (x << 16);
  return x;
}

#define div_ceil(type, a, b) ({ \
  type __div_a = a; \
  type __div_b = b; \
  (__div_a == 0) ? 0 : (__div_a - 1) / __div_b + 1; })

#define roundup_pow2(x) ( 1ULL << log2_ceil(x) )

#define roundup_mult_pow2(x, pow2) ( (x + pow2 - 1) & ~(pow2 - 1) )

#define rounddown_mult_pow2(x, pow2) ( x & ~(pow2 - 1) )

#define roundup_mult(x, mult) ({ \
  typeof(x) rem = x % mult; \
  rem ? x + mult - rem : x \
})

static inline char *strdup(const char *s) {
  char *r = calloc(strlen(s) + 1, sizeof(char));
  strcpy(r, s);
  return r;
}

// Must free return value
static inline char *xdirname(const char *s) {
  if (s == NULL) {
    return strdup(".");
  } else if (strcmp(s, "/") == 0) {
    return strdup("/");
  } else {
    const size_t len = strlen(s);
    ssize_t n;
    for (n = len - 1; n >= 0; --n) {
      if (s[n] == '/') {
        break;
      }
    }
    if (n < 0) {
      return strdup(s);
    } else {
      char *r = calloc(n + 1, sizeof(char));
      memcpy(r, s, n);
      return r;
    }
  }
}

#endif
