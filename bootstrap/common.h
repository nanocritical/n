#ifndef COMMON_H__
#define COMMON_H__

#define CONFIG_MEMPOOL_JUST_MALLOC 0
#define CONFIG_VECTOR_BOUND_CHECKS 1

#define _XOPEN_SOURCE 700 // fmemopen(3)

#include <stdint.h>
#define  __USE_POSIX 1 // for SSIZE_MAX
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

#define offsetof(type, member) __builtin_offsetof(type, member)
#define container_of(ptr, type, member) \
  ({ const __typeof__( ((type *)0)->member ) *__mptr = (ptr); \
   (type *)( (char *)__mptr - offsetof(type, member) );})

#define packed__ __attribute__((__packed__))
#define unused__ __attribute__((__unused__))
#define pure__ __attribute__((__pure__))
#define noinline__ __attribute__((noinline))
#define use_result__ __attribute__((__warn_unused_result__))
#define sentinel__ __attribute__((__sentinel__))

#define likely(x) __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)

#define _STRINGIFY(x) #x
#define STRINGIFY(x) _STRINGIFY(x)

#define STATIC_ASSERT(x) ( ({ unused__ char __failed[-(!(x))]; }), (void) 0 )

#define CONST_CAST(x) \
  __builtin_choose_expr( \
    __builtin_types_compatible_p(__typeof__(x), const struct node *), \
    (struct node *) (x), \
    __builtin_choose_expr( \
      __builtin_types_compatible_p(__typeof__(x), const struct module *), \
      (struct module *) (x), \
      __builtin_choose_expr( \
        __builtin_types_compatible_p(__typeof__(x), const struct typ *), \
        (struct typ *) (x), \
        __builtin_choose_expr( \
          __builtin_types_compatible_p(__typeof__(x), const struct toplevel *), \
          (struct toplevel *) (x), \
          __builtin_choose_expr( \
            __builtin_types_compatible_p(__typeof__(x), const struct scope *), \
            (struct scope *) (x), \
            __builtin_choose_expr( \
              __builtin_types_compatible_p(__typeof__(x), const struct typset *), \
              (struct typset *) (x), \
              __builtin_choose_expr( \
                __builtin_types_compatible_p(__typeof__(x), const struct constraint *), \
                (struct constraint *) (x), \
                __builtin_choose_expr( \
                  __builtin_types_compatible_p(__typeof__(x), const struct vecancestor *), \
                  (struct vecancestor *) (x), \
                  (x) ))))))))

typedef _Bool bool;
#define true 1
#define false 0

typedef int error;
#define ERROR use_result__ error

void __break(void);

#define EXCEPT(e) do { \
  if (e) { \
    __break(); \
    fprintf(g_env.stderr, "\tE: %s:%d: %s()\n", __FILE__, __LINE__, __func__); \
    return e; \
  } \
} while (0)

#define THROW(e) do { \
  assert(e); \
  EXCEPT(e); \
  return e; \
} while (0)

#define GOTO_EXCEPT(_e) do { \
  if (_e) { \
    __break(); \
    e = _e; \
    fprintf(g_env.stderr, "\tE: %s:%d: %s()\n", __FILE__, __LINE__, __func__); \
    goto except; \
  } \
} while (0)

#define GOTO_THROW(e) do { \
  assert(e); \
  GOTO_EXCEPT(e); \
  goto except; \
} while (0)

#define EXCEPTF(e, fmt, ...) if (e) { THROWF(e, fmt, ##__VA_ARGS__); }

#define THROWF(e, fmt, ...) do { \
  assert(e); \
  __break(); \
  fprintf(g_env.stderr, fmt "\n", ##__VA_ARGS__); \
  fprintf(g_env.stderr, "\tE: %s:%d: %s(): %s\n", __FILE__, __LINE__, __func__, strerror(e)); \
  return e; \
} while (0)

#define GOTO_THROWF(_e, fmt, ...) do { \
  assert(_e); \
  __break(); \
  e = _e; \
  fprintf(g_env.stderr, fmt "\n", ##__VA_ARGS__); \
  fprintf(g_env.stderr, "\tE: %s:%d: %s(): %s\n", __FILE__, __LINE__, __func__, strerror(e)); \
  goto except; \
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

#define SWAP(a, b) ({ \
  __typeof__(a) __tmp = a; \
  a = b; \
  b = __tmp; })

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

#define rounddown_pow2(x) ( roundup_pow2(x) / 2 )

#define roundup_mult_pow2(x, pow2) ( (x + pow2 - 1) & ~(pow2 - 1) )

#define rounddown_mult_pow2(x, pow2) ( x & ~(pow2 - 1) )

#define roundup_mult(x, mult) ({ \
  __typeof__(x) rem = x % mult; \
  rem ? x + mult - rem : x \
})

char *strdup(const char *s);

// Must free return value
char *xdirname(const char *s);
char *xbasename(const char *s);

#define ENV_BUF_SIZE 1024
struct env {
  char *stderr_mem;
  FILE *stderr;
  bool running_example;
};

struct env g_env;

void env_init(void);

void should_fail(error e);
void should_fail_with(error e, const char *err);

#define EXAMPLES_DECLS
#define EXAMPLES_INIT_ARGS
#define EXAMPLES_PROTO void
#define EXAMPLES_ARGS
void examples_init(const char *name);
void examples_destroy(const char *name);

#define EXAMPLE(name) \
  __attribute__((section(".text.examples"))) \
  void example__##name(void); \
  void example__##name(void)

struct module;
#define EXAMPLES_DECLS_NCC_EMPTY \
  struct module *mod
#define EXAMPLES_INIT_ARGS_NCC_EMPTY , &mod
#define EXAMPLES_PROTO_NCC_EMPTY struct module *mod
#define EXAMPLES_ARGS_NCC_EMPTY mod
void examples_init_NCC_EMPTY(const char *name, struct module **mod);
void examples_destroy_NCC_EMPTY(const char *name, struct module **mod);

#define EXAMPLE_NCC_EMPTY(name) \
  __attribute__((section(".text.examples._NCC_EMPTY"))) \
  void example_NCC_EMPTY__##name(struct module *mod); \
  void example_NCC_EMPTY__##name(struct module *mod)

#define EXAMPLES_DECLS_NCC \
  struct module *mod
#define EXAMPLES_INIT_ARGS_NCC , &mod
#define EXAMPLES_PROTO_NCC struct module *mod
#define EXAMPLES_ARGS_NCC mod
void examples_init_NCC(const char *name, struct module **mod);
void examples_destroy_NCC(const char *name, struct module **mod);

#define EXAMPLE_NCC(name) \
  __attribute__((section(".text.examples._NCC"))) \
  void example_NCC__##name(struct module *mod); \
  void example_NCC__##name(struct module *mod)


#ifndef INVARIANTS
#define INVARIANT(invs)
#else
extern uint32_t g_invariants_counter;
#define CHECK_INVARIANTS() (++g_invariants_counter % INVARIANTS == 0)
#define INVARIANT(invs) if (CHECK_INVARIANTS()) { invs; }
#endif


struct node;

void check_structure(struct node *node, ...) sentinel__;

enum timeits {
  TIMEIT_MAIN,
  TIMEIT_PARSER,
  TIMEIT_PRE_PASSBODY,
  TIMEIT_PASSBODY,
  TIMEIT_PASSSEM,
  TIMEIT_CREATE_INSTANCE_DEEPCOPY,
  TIMEIT_INSTANTIATE_DEEPCOPY,
  TIMEIT_INSTANTIATE_TOTAL,
  TIMEIT_INSTANTIATE,
  TIMEIT_INSTANTIATE_INTF,
  TIMEIT_INSTANTIATE_REF,
  TIMEIT_INSTANTIATE_TENTATIVE,
  TIMEIT_INSTANTIATE_TENTATIVE_INTF,
  TIMEIT_INSTANTIATE_TENTATIVE_REF,
  TIMEIT_TYPE_INFERENCE,
  TIMEIT_TYPE_INFERENCE_PREBODYPASS,
  TIMEIT_TYPE_INFERENCE_IN_FUNS_BLOCK,
  TIMEIT_UNIFY,
  TIMEIT_GENERATE,
  TIMEIT_GENERATE_C,
  TIMEIT__NUM,
};

struct timeit {
  size_t depth;
  size_t count;
  double time;
};

extern struct timeit timeits[TIMEIT__NUM];

double time(void);
void timeit_print(FILE *out);

#define BEGTIMEIT(what) \
  double timeit_##what = timeits[what].depth != 0 ? 0 : time(); \
  timeits[what].depth += 1

#define ENDTIMEIT(cond, what) \
  if (cond) { \
    timeits[what].count += 1; \
    if (timeits[what].depth == 1) { timeits[what].time += time() - timeit_##what; } \
  } \
  (void) timeit_##what; \
  timeits[what].depth -= 1

#endif
