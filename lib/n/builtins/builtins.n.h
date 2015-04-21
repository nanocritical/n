#include <stdarg.h>
#include <float.h>

#define NB(n) n$builtins$##n

#ifdef NLANG_DEFINE_TYPES

struct n$builtins$cleaner {
  void *p;
  size_t sz;
};

typedef struct _$Ngen_n$builtins$Slice_impl$$n$builtins$U8_genN$_ NB(byteslice);

union NB(Varargintunion) {
  struct NB(Varargint) *ref;
  va_list valist;
  NB(byteslice) *s;
};

struct NB(Varargint) {
  NB(Int) n; // Must be first, see Count_left below.
  NB(I32) vacount;
  union NB(Varargintunion) u;
};

#endif

#ifdef NLANG_DECLARE_FUNCTIONS

#define NLANG_DEFER(dtor) __attribute__((__cleanup__(dtor)))

#define NLANG_CLEANUP_ZERO(x) \
  __attribute__((__cleanup__(n$builtins$clean_zero))) \
  struct n$builtins$cleaner __Ncleaner_##x = { .p = (x), .sz = sizeof(*(x)) }

static inline void n$builtins$clean_zero(struct n$builtins$cleaner *c);

#define NLANG_STRING_LITERAL(s) \
{ .bytes = { \
  .dat = (n$builtins$U8 *)s, \
  .cnt = sizeof(s)-1, \
  .cap = sizeof(s) } }

#define NLANG_BYTE_SLICE(b, cnt) \
{ .dat = (n$builtins$U8 *)b, \
  .cnt = cnt, \
  .cap = cnt }

#define NLANG_BUILTINS_VARARG_SLICE_CNT(ap) ( (ap)->u.s->cnt )
#define NLANG_BUILTINS_VARARG_SLICE_NTH(ap, t, nth) \
  ({ \
   n$builtins$Assert__((ap)->n != 0, NULL); \
   __attribute__((__unused__)) static t __dummy; \
   (void *) &(ap)->u.s->dat[(nth) * sizeof(*__dummy)]; })

#define NLANG_BUILTINS_VACOUNT_SLICE (-1)
#define NLANG_BUILTINS_VACOUNT_VARARGREF (-2)

#define NLANG_BUILTINS_VARARG_START(va) do { \
  va_start((va).ap.u.valist, _$Nvacount); \
  (va).ap.vacount = _$Nvacount; \
  if (_$Nvacount >= 0) { \
    (va).ap.n = _$Nvacount; \
  } else if (_$Nvacount == NLANG_BUILTINS_VACOUNT_SLICE) { \
    (va).ap.u.s = va_arg((va).ap.u.valist, void *); \
    (va).ap.n = NLANG_BUILTINS_VARARG_SLICE_CNT(&(va).ap); \
    va_end((va).ap.u.valist); \
  } else { \
    struct n$builtins$Varargint *ref = va_arg((va).ap.u.valist, struct n$builtins$Varargint *); \
    va_end((va).ap.u.valist); \
    if (ref->vacount == NLANG_BUILTINS_VACOUNT_VARARGREF) { \
      ref = ref->u.ref; \
    } \
    (va).ap.u.ref = ref; \
  } \
} while (0)

#define NLANG_BUILTINS_VARARG_AP(va) \
  (((va).ap.vacount == NLANG_BUILTINS_VACOUNT_VARARGREF) \
   ? (va).ap.u.ref : &(va).ap)

#define NLANG_BUILTINS_VARARG_END(va) do { \
  struct n$builtins$Varargint *ap = NLANG_BUILTINS_VARARG_AP(va); \
  if (ap->vacount >= 0) { \
    va_end(ap->u.valist); \
  } \
} while (0)

#define NLANG_BUILTINS_VARARG_NEXT_(t, ap) \
  ({ \
   n$builtins$Assert__((ap)->n != 0, NULL); \
   (ap)->n -= 1; \
   va_arg((ap)->u.valist, t); \
   })

#define NLANG_BUILTINS_VARARG_NEXT(t, va) \
  ({ \
   struct n$builtins$Varargint *ap = NLANG_BUILTINS_VARARG_AP(va); \
   (ap->vacount == NLANG_BUILTINS_VACOUNT_SLICE) ? \
   NLANG_BUILTINS_VARARG_SLICE_NTH(ap, t, \
                                   NLANG_BUILTINS_VARARG_SLICE_CNT(ap) \
                                   - ap->n--) \
   : NLANG_BUILTINS_VARARG_NEXT_(t, ap); })

// FIXME: dyn slices support is missing
#define NLANG_BUILTINS_VARARG_NEXT_DYN(t, va) \
  NLANG_BUILTINS_VARARG_NEXT_(t, NLANG_BUILTINS_VARARG_AP(va))

#define NLANG_MKDYN(dyn_type, _dyntable, _obj) \
  (dyn_type){ .obj = (_obj), .dyntable = (void *)(_dyntable) }

struct n$reflect$Type;
n$builtins$Bool n$reflect$Isa(void *dyntable, struct n$reflect$Type *i);

struct n$bench$Example;
n$builtins$Void n$bench$Example$Dtor(struct n$bench$Example *);

#endif

#ifdef NLANG_DEFINE_FUNCTIONS

static inline void n$builtins$clean_zero(struct n$builtins$cleaner *c) {
  memset(c->p, 0, c->sz);
}

static inline NB(Uint) NB(Varargint$Count_left)(struct NB(Varargint) *self) {
  if (self->vacount == NLANG_BUILTINS_VACOUNT_VARARGREF) {
    return self->u.ref->n;
  } else {
    return self->n;
  }
}

static inline NB(U8) *NB(Slice_at_byte)(NB(U8) *p, NB(Uint) off) {
  return p + off;
}

static inline NB(Void) NB(Slice_memmove)(NB(U8) *dst, NB(U8) *src, NB(Uint) cnt) {
  memmove(dst, src, cnt);
}

static inline NB(Int) NB(Slice_memcmp)(NB(U8) *a, NB(U8) *b, NB(Uint) cnt) {
  return memcmp(a, b, cnt);
}

static inline NB(Void) NB(Slice_memset)(NB(U8) *a, NB(U8) c, NB(Uint) cnt) {
  memset(a, c, cnt);
}

#define define_native_boolean(t) \
  static inline NB(Bool) t##$Operator_eq(t *self, t *other) { return *self == *other; } \
  static inline NB(Bool) t##$Operator_ne(t *self, t *other) { return *self != *other; } \
  \
  static inline NB(Bool) t##$Operator_le(t *self, t *other) { return *self <= * other; } \
  static inline NB(Bool) t##$Operator_lt(t *self, t *other) { return *self < * other; } \
  static inline NB(Bool) t##$Operator_gt(t *self, t *other) { return *self > * other; } \
  static inline NB(Bool) t##$Operator_ge(t *self, t *other) { return *self >= * other; } \
  \
  static inline t t##$Operator_test(t *self) { return *self; } \
  static inline t t##$Operator_not(t *self) { return ! *self; }

#define define_native_integer(t) \
  static inline NB(Bool) t##$Operator_eq(t *self, t *other) { return *self == *other; } \
  static inline NB(Bool) t##$Operator_ne(t *self, t *other) { return *self != *other; } \
  \
  static inline NB(Bool) t##$Operator_le(t *self, t *other) { return *self <= * other; } \
  static inline NB(Bool) t##$Operator_lt(t *self, t *other) { return *self < * other; } \
  static inline NB(Bool) t##$Operator_gt(t *self, t *other) { return *self > * other; } \
  static inline NB(Bool) t##$Operator_ge(t *self, t *other) { return *self >= * other; } \
  \
  static inline t t##$Operator_plus(t *self, t *other) { return *self + *other; } \
  static inline t t##$Operator_minus(t *self, t *other) { return *self - *other; } \
  static inline t t##$Operator_divide(t *self, t *other) { return *self / *other; } \
  static inline t t##$Operator_modulo(t *self, t *other) { return *self % *other; } \
  static inline t t##$Operator_times(t *self, t *other) { return *self * *other; } \
  static inline t t##$Operator_uminus(t *self) { return - *self; } \
  static inline t t##$Operator_rshift(t *self, NB(Uint) by) { return *self >> by; } \
  static inline NB(Void) t##$Operator_assign_plus(t *self, t *other) { *self += *other; } \
  static inline NB(Void) t##$Operator_assign_minus(t *self, t *other) { *self -= *other; } \
  static inline NB(Void) t##$Operator_assign_divide(t *self, t *other) { *self /= *other; } \
  static inline NB(Void) t##$Operator_assign_modulo(t *self, t *other) { *self %= *other; } \
  static inline NB(Void) t##$Operator_assign_times(t *self, t *other) { *self *= *other; } \
  static inline NB(Void) t##$Operator_assign_rshift(t *self, NB(Uint) by) { *self >>= by; } \
  \
  static inline t t##$Operator_bwor(t *self, t *other) { return *self | *other; } \
  static inline t t##$Operator_bwxor(t *self, t *other) { return *self ^ *other; } \
  static inline t t##$Operator_bwand(t *self, t *other) { return *self & *other; } \
  static inline t t##$Operator_bwnot(t *self) { return ~ *self; } \
  static inline NB(Void) t##$Operator_assign_bwor(t *self, t *other) { *self |= *other; } \
  static inline NB(Void) t##$Operator_assign_bwxor(t *self, t *other) { *self ^= *other; } \
  static inline NB(Void) t##$Operator_assign_bwand(t *self, t *other) { *self &= *other; }

#define define_native_sized_integer(t) \
  define_native_integer(t) \
  static inline t t##$Operator_ovplus(t *self, t *other) { return *self + *other; } \
  static inline t t##$Operator_ovminus(t *self, t *other) { return *self - *other; } \
  static inline t t##$Operator_ovdivide(t *self, t *other) { return *self / *other; } \
  static inline t t##$Operator_ovmodulo(t *self, t *other) { return *self % *other; } \
  static inline t t##$Operator_ovtimes(t *self, t *other) { return *self * *other; } \
  static inline t t##$Operator_ovuminus(t *self) { return - *self; } \
  static inline t t##$Operator_ovlshift(t *self, NB(Uint) by) { return *self << by; } \
  static inline NB(Void) t##$Operator_assign_ovplus(t *self, t *other) { *self += *other; } \
  static inline NB(Void) t##$Operator_assign_ovminus(t *self, t *other) { *self -= *other; } \
  static inline NB(Void) t##$Operator_assign_ovdivide(t *self, t *other) { *self /= *other; } \
  static inline NB(Void) t##$Operator_assign_ovmodulo(t *self, t *other) { *self %= *other; } \
  static inline NB(Void) t##$Operator_assign_ovtimes(t *self, t *other) { *self *= *other; } \
  static inline NB(Void) t##$Operator_assign_ovlshift(t *self, NB(Uint) by) { *self <<= by; } \

#define define_native_sized_signed_integer(t, uns) \
  define_native_sized_integer(t) \
  static inline uns t##$Bitwise_unsigned(t *self) { return (uns) *self; }

#define define_native_sized_unsigned_integer(t, sig) \
  define_native_sized_integer(t) \
  static inline sig t##$Bitwise_signed(t *self) { return (sig) *self; }

#define define_native_floating(t) \
  static inline NB(Bool) t##$Operator_eq(t *self, t *other) { return *self == *other; } \
  static inline NB(Bool) t##$Operator_ne(t *self, t *other) { return *self != *other; } \
  \
  static inline NB(Bool) t##$Operator_le(t *self, t *other) { return *self <= * other; } \
  static inline NB(Bool) t##$Operator_lt(t *self, t *other) { return *self < * other; } \
  static inline NB(Bool) t##$Operator_gt(t *self, t *other) { return *self > * other; } \
  static inline NB(Bool) t##$Operator_ge(t *self, t *other) { return *self >= * other; } \
  \
  static inline t t##$Operator_plus(t *self, t *other) { return *self + *other; } \
  static inline t t##$Operator_minus(t *self, t *other) { return *self - *other; } \
  static inline t t##$Operator_divide(t *self, t *other) { return *self / *other; } \
  static inline t t##$Operator_times(t *self, t *other) { return *self * *other; } \
  static inline t t##$Operator_uminus(t *self) { return - *self; } \
  static inline NB(Void) t##$Operator_assign_plus(t *self, t *other) { *self += *other; } \
  static inline NB(Void) t##$Operator_assign_minus(t *self, t *other) { *self -= *other; } \
  static inline NB(Void) t##$Operator_assign_divide(t *self, t *other) { *self /= *other; } \
  static inline NB(Void) t##$Operator_assign_times(t *self, t *other) { *self *= *other; } \

define_native_boolean(n$builtins$Bool)
define_native_sized_signed_integer(n$builtins$I8, NB(U8))
define_native_sized_signed_integer(n$builtins$I16, NB(U16))
define_native_sized_signed_integer(n$builtins$I32, NB(U32))
define_native_sized_signed_integer(n$builtins$I64, NB(U64))
define_native_sized_unsigned_integer(n$builtins$U8, NB(I8))
define_native_sized_unsigned_integer(n$builtins$U16, NB(I16))
define_native_sized_unsigned_integer(n$builtins$U32, NB(I32))
define_native_sized_unsigned_integer(n$builtins$U64, NB(I64))
define_native_integer(n$builtins$Uint)
define_native_integer(n$builtins$Int)
define_native_integer(n$builtins$Uintptr)
define_native_integer(n$builtins$Intptr)
define_native_floating(n$builtins$Float)
define_native_floating(n$builtins$Double)

static inline NB(U8) NB(Bool$U8)(NB(Bool) *self) { return (NB(U8)) *self; }
static inline NB(U16) NB(Bool$U16)(NB(Bool) *self) { return (NB(U16)) *self; }
static inline NB(U32) NB(Bool$U32)(NB(Bool) *self) { return (NB(U32)) *self; }
static inline NB(U64) NB(Bool$U64)(NB(Bool) *self) { return (NB(U64)) *self; }
static inline NB(Uint) NB(Bool$Uint)(NB(Bool) *self) { return (NB(Uint)) *self; }
static inline NB(I8) NB(Bool$I8)(NB(Bool) *self) { return (NB(I8)) *self; }
static inline NB(I16) NB(Bool$I16)(NB(Bool) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(Bool$I32)(NB(Bool) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(Bool$I64)(NB(Bool) *self) { return (NB(I64)) *self; }
static inline NB(Int) NB(Bool$Int)(NB(Bool) *self) { return (NB(Int)) *self; }

static inline NB(I16) NB(I8$I16)(NB(I8) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(I8$I32)(NB(I8) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(I8$I64)(NB(I8) *self) { return (NB(I64)) *self; }
static inline NB(Int) NB(I8$Int)(NB(I8) *self) { return (NB(Int)) *self; }
static inline NB(U16) NB(I8$U16)(NB(I8) *self) { return (NB(U16)) *self; }
static inline NB(U32) NB(I8$U32)(NB(I8) *self) { return (NB(U32)) *self; }
static inline NB(U64) NB(I8$U64)(NB(I8) *self) { return (NB(U64)) *self; }
static inline NB(Uint) NB(I8$Uint)(NB(I8) *self) { return (NB(Uint)) *self; }
static inline NB(Float) NB(I8$Exact_float)(NB(I8) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(I8$Exact_double)(NB(I8) *self) { return (NB(Double)) *self; }

static inline NB(I8) NB(I16$Trim_i8)(NB(I16) *self) { return (NB(I8)) *self; }
static inline NB(I32) NB(I16$I32)(NB(I16) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(I16$I64)(NB(I16) *self) { return (NB(I64)) *self; }
static inline NB(Int) NB(I16$Int)(NB(I16) *self) { return (NB(Int)) *self; }
static inline NB(U32) NB(I16$U32)(NB(I16) *self) { return (NB(U32)) *self; }
static inline NB(U64) NB(I16$U64)(NB(I16) *self) { return (NB(U64)) *self; }
static inline NB(Uint) NB(I16$Uint)(NB(I16) *self) { return (NB(Uint)) *self; }
static inline NB(Float) NB(I16$Exact_float)(NB(I16) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(I16$Exact_double)(NB(I16) *self) { return (NB(Double)) *self; }

static inline NB(I8) NB(I32$Trim_i8)(NB(I32) *self) { return (NB(I8)) *self; }
static inline NB(I16) NB(I32$Trim_i16)(NB(I32) *self) { return (NB(I16)) *self; }
static inline NB(I64) NB(I32$I64)(NB(I32) *self) { return (NB(I64)) *self; }
static inline NB(Int) NB(I32$Int)(NB(I32) *self) { return (NB(Int)) *self; }
static inline NB(U64) NB(I32$U64)(NB(I32) *self) { return (NB(U64)) *self; }
static inline NB(Uint) NB(I32$Uint)(NB(I32) *self) { return (NB(Uint)) *self; }
static inline NB(Float) NB(I32$Round_float)(NB(I32) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(I32$Exact_double)(NB(I32) *self) { return (NB(Double)) *self; }

static inline NB(Int) NB(I64$force_int)(NB(I64) *self) { return (NB(Int)) *self; }
static inline NB(Intptr) NB(I64$force_intptr)(NB(I64) *self) { return (NB(Intptr)) *self; }
static inline NB(I8) NB(I64$Trim_i8)(NB(I64) *self) { return (NB(I8)) *self; }
static inline NB(I16) NB(I64$Trim_i16)(NB(I64) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(I64$Trim_i32)(NB(I64) *self) { return (NB(I32)) *self; }
static inline NB(Float) NB(I64$Round_float)(NB(I64) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(I64$Round_double)(NB(I64) *self) { return (NB(Double)) *self; }

static inline NB(U16) NB(U8$U16)(NB(U8) *self) { return (NB(U16)) *self; }
static inline NB(U32) NB(U8$U32)(NB(U8) *self) { return (NB(U32)) *self; }
static inline NB(U64) NB(U8$U64)(NB(U8) *self) { return (NB(U64)) *self; }
static inline NB(Uint) NB(U8$Uint)(NB(U8) *self) { return (NB(Uint)) *self; }
static inline NB(I16) NB(U8$I16)(NB(U8) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(U8$I32)(NB(U8) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(U8$I64)(NB(U8) *self) { return (NB(I64)) *self; }
static inline NB(Int) NB(U8$Int)(NB(U8) *self) { return (NB(Int)) *self; }
static inline NB(Float) NB(U8$Exact_float)(NB(U8) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(U8$Exact_double)(NB(U8) *self) { return (NB(Double)) *self; }

static inline NB(U8) NB(U16$Trim_u8)(NB(U16) *self) { return (NB(U8)) *self; }
static inline NB(U32) NB(U16$U32)(NB(U16) *self) { return (NB(U32)) *self; }
static inline NB(U64) NB(U16$U64)(NB(U16) *self) { return (NB(U64)) *self; }
static inline NB(Uint) NB(U16$Uint)(NB(U16) *self) { return (NB(Uint)) *self; }
static inline NB(I32) NB(U16$I32)(NB(U16) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(U16$I64)(NB(U16) *self) { return (NB(I64)) *self; }
static inline NB(Int) NB(U16$Int)(NB(U16) *self) { return (NB(Int)) *self; }
static inline NB(Float) NB(U16$Exact_float)(NB(U16) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(U16$Exact_double)(NB(U16) *self) { return (NB(Double)) *self; }

static inline NB(U8) NB(U32$Trim_u8)(NB(U32) *self) { return (NB(U8)) *self; }
static inline NB(U16) NB(U32$Trim_u16)(NB(U32) *self) { return (NB(U16)) *self; }
static inline NB(U64) NB(U32$U64)(NB(U32) *self) { return (NB(U64)) *self; }
static inline NB(I64) NB(U32$I64)(NB(U32) *self) { return (NB(I64)) *self; }
static inline NB(Uint) NB(U32$Uint)(NB(U32) *self) { return (NB(Uint)) *self; }
static inline NB(Int) NB(U32$Int)(NB(U32) *self) { return (NB(Int)) *self; }
static inline NB(Float) NB(U32$Round_float)(NB(U32) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(U32$Exact_double)(NB(U32) *self) { return (NB(Double)) *self; }

static inline NB(Uint) NB(U64$force_uint)(NB(U64) *self) { return (NB(Uint)) *self; }
static inline NB(Uintptr) NB(U64$force_uintptr)(NB(U64) *self) { return (NB(Uintptr)) *self; }
static inline NB(U8) NB(U64$Trim_u8)(NB(U64) *self) { return (NB(U8)) *self; }
static inline NB(U16) NB(U64$Trim_u16)(NB(U64) *self) { return (NB(U16)) *self; }
static inline NB(U32) NB(U64$Trim_u32)(NB(U64) *self) { return (NB(U32)) *self; }
static inline NB(Float) NB(U64$Round_float)(NB(U64) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(U64$Round_double)(NB(U64) *self) { return (NB(Double)) *self; }

static inline NB(Uint) NB(Int$force_unsigned)(NB(Int) *self) { return (NB(Uint)) *self; }
static inline NB(I8) NB(Int$Trim_i8)(NB(Int) *self) { return (NB(I8)) *self; }
static inline NB(I16) NB(Int$Trim_i16)(NB(Int) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(Int$Trim_i32)(NB(Int) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(Int$I64)(NB(Int) *self) { return (NB(I64)) *self; }
static inline NB(Float) NB(Int$Round_float)(NB(Int) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(Int$Round_double)(NB(Int) *self) { return (NB(Double)) *self; }

static inline NB(Int) NB(Uint$force_signed)(NB(Uint) *self) { return (NB(Int)) *self; }
static inline NB(U8) NB(Uint$Trim_u8)(NB(Uint) *self) { return (NB(U8)) *self; }
static inline NB(U16) NB(Uint$Trim_u16)(NB(Uint) *self) { return (NB(U16)) *self; }
static inline NB(U32) NB(Uint$Trim_u32)(NB(Uint) *self) { return (NB(U32)) *self; }
static inline NB(U64) NB(Uint$U64)(NB(Uint) *self) { return (NB(U64)) *self; }
static inline NB(Float) NB(Uint$Round_float)(NB(Uint) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(Uint$Round_double)(NB(Uint) *self) { return (NB(Double)) *self; }

static inline NB(Uintptr) NB(Intptr$force_unsigned)(NB(Intptr) *self) { return (NB(Uintptr)) *self; }
static inline NB(I64) NB(Intptr$I64)(NB(Intptr) *self) { return (NB(I64)) *self; }

static inline NB(Intptr) NB(Uintptr$force_signed)(NB(Uintptr) *self) { return (NB(Intptr)) *self; }
static inline NB(U64) NB(Uintptr$U64)(NB(Uintptr) *self) { return (NB(U64)) *self; }

static inline NB(I8) NB(Float$Round0_i8)(NB(Float) *self) { return (NB(I8)) *self; }
static inline NB(I16) NB(Float$Round0_i16)(NB(Float) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(Float$Round0_i32)(NB(Float) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(Float$Round0_i64)(NB(Float) *self) { return (NB(I64)) *self; }
static inline NB(Double) NB(Float$Double)(NB(Float) *self) { return (NB(Double)) *self; }
static inline NB(Int) NB(Float$Round0_int)(NB(Float) *self) { return (NB(Int)) *self; }
static inline NB(U32) NB(Float$Bitwise_u32)(NB(Float) *self) { return *(NB(U32) *) self; }
static inline NB(Float) NB(Float$MIN)(void) { return -FLT_MAX; }
static inline NB(Float) NB(Float$MAX)(void) { return FLT_MAX; }
static inline NB(Float) NB(Float$EPSILON)(void) { return FLT_EPSILON; }
static inline NB(Float) NB(Float$SMALLEST_POSITIVE_NORMAL)(void) { return FLT_MIN; }
static inline NB(Uint) NB(Float$RADIX)(void) { return FLT_RADIX; }
static inline NB(Uint) NB(Float$MANTISSA_WIDTH)(void) { return FLT_MANT_DIG; }
static inline NB(Uint) NB(Float$MAX_DECIMAL_DIGIT_EXACT)(void) { return FLT_DIG; }
static inline NB(Int) NB(Float$MIN_EXP)(void) { return FLT_MIN_EXP; }
static inline NB(Int) NB(Float$MAX_EXP)(void) { return FLT_MAX_EXP; }
static inline NB(Int) NB(Float$MIN_10_EXP)(void) { return FLT_MIN_10_EXP; }
static inline NB(Int) NB(Float$MAX_10_EXP)(void) { return FLT_MAX_10_EXP; }

static inline NB(I8) NB(Double$Round0_i8)(NB(Double) *self) { return (NB(I8)) *self; }
static inline NB(I16) NB(Double$Round0_i16)(NB(Double) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(Double$Round0_i32)(NB(Double) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(Double$Round0_i64)(NB(Double) *self) { return (NB(I64)) *self; }
static inline NB(Float) NB(Double$Round_float)(NB(Double) *self) { return (NB(Float)) *self; }
static inline NB(Int) NB(Double$Round0_int)(NB(Double) *self) { return (NB(Int)) *self; }
static inline NB(U64) NB(Double$Bitwise_u64)(NB(Double) *self) { return *(NB(U64) *) self; }
static inline NB(Double) NB(Double$MIN)(void) { return -DBL_MAX; }
static inline NB(Double) NB(Double$MAX)(void) { return DBL_MAX; }
static inline NB(Double) NB(Double$EPSILON)(void) { return DBL_EPSILON; }
static inline NB(Double) NB(Double$SMALLEST_POSITIVE_NORMAL)(void) { return DBL_MIN; }
static inline NB(Uint) NB(Double$RADIX)(void) { return FLT_RADIX; }
static inline NB(Uint) NB(Double$MANTISSA_WIDTH)(void) { return DBL_MANT_DIG; }
static inline NB(Uint) NB(Double$MAX_DECIMAL_DIGIT_EXACT)(void) { return DBL_DIG; }
static inline NB(Int) NB(Double$MIN_EXP)(void) { return DBL_MIN_EXP; }
static inline NB(Int) NB(Double$MAX_EXP)(void) { return DBL_MAX_EXP; }
static inline NB(Int) NB(Double$MIN_10_EXP)(void) { return DBL_MIN_10_EXP; }
static inline NB(Int) NB(Double$MAX_10_EXP)(void) { return DBL_MAX_10_EXP; }

#define n$builtins$Likely(x) __builtin_expect(!!(x), 1)
#define n$builtins$Unlikely(x) __builtin_expect(!!(x), 0)

static inline NB(U8) *NB(Static_array_at_byte)(NB(U8) *p, NB(Uint) off) {
  return p + off;
}

#endif

#ifdef NLANG_DECLARE_FUNCTIONS

NB(Void) *NB(Nonnil_void)(void);

#endif

#undef NB
