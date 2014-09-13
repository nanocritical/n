#define NB(n) n$builtins$##n

#ifdef NLANG_DEFINE_TYPES
#include <stdarg.h>

struct NB(Valist) {
  va_list ap;
};

#define NLANG_BUILTINS_DEFINE_ENVPARENT(envt) _$Ndyn_##envt _$Nenvparent_##envt

#endif

#ifdef NLANG_DECLARE_FUNCTIONS

#define NLANG_STRING_LITERAL(s) \
{ .bytes = { \
  .dat = (n$builtins$U8 *)s, \
  .cnt = sizeof(s)-1, \
  .cap = sizeof(s) } }

#define NLANG_BYTE_SLICE(b, cnt) \
{ .dat = (n$builtins$U8 *)b, \
  .cnt = cnt, \
  .cap = cnt }

#define NLANG_BUILTINS_VARARG_START(va) do { \
  va_start((va).ap.ap, _$Nvarargcount); \
  (va).n = _$Nvarargcount; \
} while (0)

#define NLANG_BUILTINS_VARARG_END(va) do { \
  va_end((va).ap.ap); \
} while (0)

#define NLANG_BUILTINS_VARARG_NEXT(t, va) \
  ({ n$builtins$Assert__((va).n > 0, NULL); \
   (va).n -= 1; \
   va_arg((va).ap.ap, t); })

#define NLANG_MKDYN(dyn_type, _dyntable, _obj) \
  (dyn_type){ .dyntable = (void *)(_dyntable), .obj = (_obj) }

#define NLANG_BUILTINS_BG_ENVIRONMENT_PARENT(envt) do { \
  return self->_$Nenvparent_##envt; \
} while (0)

#define NLANG_BUILTINS_BG_ENVIRONMENT_INSTALL(envt) do { \
  self->_$Nenvparent_##envt = *where; \
  *where = NLANG_MKDYN(_$Ndyn_##envt, &THIS($Dyntable__##envt), self); \
} while (0)

#define NLANG_BUILTINS_BG_ENVIRONMENT_UNINSTALL(envt) do { \
  *where = where->dyntable->Parent(where->obj); \
} while (0)

#endif

#ifdef NLANG_DEFINE_FUNCTIONS

static inline NB(U8) *NB(Slice_at_byte)(NB(U8) *p, NB(Uint) off) {
  return p + off;
}

static inline NB(Void) NB(Slice_memmove)(NB(U8) *dst, NB(U8) *src, NB(Uint) cnt) {
  memmove(dst, src, cnt);
}

static inline NB(Int) NB(Slice_memcmp)(NB(U8) *a, NB(U8) *b, NB(Uint) cnt) {
  return memcmp(a, b, cnt);
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


static inline NB(I16) NB(I8$To_i16)(NB(I8) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(I8$To_i32)(NB(I8) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(I8$To_i64)(NB(I8) *self) { return (NB(I64)) *self; }
static inline NB(Int) NB(I8$To_int)(NB(I8) *self) { return (NB(Int)) *self; }
static inline NB(Float) NB(I8$Exact_float)(NB(I8) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(I8$Exact_double)(NB(I8) *self) { return (NB(Double)) *self; }

static inline NB(I8) NB(I16$Trim_i8)(NB(I16) *self) { return (NB(I8)) *self; }
static inline NB(I32) NB(I16$To_i32)(NB(I16) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(I16$To_i64)(NB(I16) *self) { return (NB(I64)) *self; }
static inline NB(Int) NB(I16$To_int)(NB(I16) *self) { return (NB(Int)) *self; }
static inline NB(Float) NB(I16$Exact_float)(NB(I16) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(I16$Exact_double)(NB(I16) *self) { return (NB(Double)) *self; }

static inline NB(I8) NB(I32$Trim_i8)(NB(I32) *self) { return (NB(I8)) *self; }
static inline NB(I16) NB(I32$Trim_i16)(NB(I32) *self) { return (NB(I16)) *self; }
static inline NB(I64) NB(I32$To_i64)(NB(I32) *self) { return (NB(I64)) *self; }
static inline NB(Int) NB(I32$To_int)(NB(I32) *self) { return (NB(Int)) *self; }
static inline NB(Float) NB(I32$Round_float)(NB(I32) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(I32$Exact_double)(NB(I32) *self) { return (NB(Double)) *self; }

static inline NB(Int) NB(I64$force_int)(NB(I64) *self) { return (NB(Int)) *self; }
static inline NB(Intptr) NB(I64$force_intptr)(NB(I64) *self) { return (NB(Intptr)) *self; }
static inline NB(I8) NB(I64$Trim_i8)(NB(I64) *self) { return (NB(I8)) *self; }
static inline NB(I16) NB(I64$Trim_i16)(NB(I64) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(I64$Trim_i32)(NB(I64) *self) { return (NB(I32)) *self; }
static inline NB(Float) NB(I64$Round_float)(NB(I64) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(I64$Round_double)(NB(I64) *self) { return (NB(Double)) *self; }

static inline NB(U16) NB(U8$To_u16)(NB(U8) *self) { return (NB(U16)) *self; }
static inline NB(U32) NB(U8$To_u32)(NB(U8) *self) { return (NB(U32)) *self; }
static inline NB(U64) NB(U8$To_u64)(NB(U8) *self) { return (NB(U64)) *self; }
static inline NB(Uint) NB(U8$To_uint)(NB(U8) *self) { return (NB(Uint)) *self; }
static inline NB(Float) NB(U8$Exact_float)(NB(U8) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(U8$Exact_double)(NB(U8) *self) { return (NB(Double)) *self; }

static inline NB(U8) NB(U16$Trim_u8)(NB(U16) *self) { return (NB(U8)) *self; }
static inline NB(U32) NB(U16$To_u32)(NB(U16) *self) { return (NB(U32)) *self; }
static inline NB(U64) NB(U16$To_u64)(NB(U16) *self) { return (NB(U64)) *self; }
static inline NB(Uint) NB(U16$To_uint)(NB(U16) *self) { return (NB(Uint)) *self; }
static inline NB(Float) NB(U16$Exact_float)(NB(U16) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(U16$Exact_double)(NB(U16) *self) { return (NB(Double)) *self; }

static inline NB(U8) NB(U32$Trim_u8)(NB(U32) *self) { return (NB(U8)) *self; }
static inline NB(U16) NB(U32$Trim_u16)(NB(U32) *self) { return (NB(U16)) *self; }
static inline NB(U64) NB(U32$To_u64)(NB(U32) *self) { return (NB(U64)) *self; }
static inline NB(Uint) NB(U32$To_uint)(NB(U32) *self) { return (NB(Uint)) *self; }
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
static inline NB(I64) NB(Int$To_i64)(NB(Int) *self) { return (NB(I64)) *self; }
static inline NB(Float) NB(Int$Round_float)(NB(Int) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(Int$Round_double)(NB(Int) *self) { return (NB(Double)) *self; }

static inline NB(Int) NB(Uint$force_signed)(NB(Uint) *self) { return (NB(Int)) *self; }
static inline NB(U8) NB(Uint$Trim_u8)(NB(Uint) *self) { return (NB(U8)) *self; }
static inline NB(U16) NB(Uint$Trim_u16)(NB(Uint) *self) { return (NB(U16)) *self; }
static inline NB(U32) NB(Uint$Trim_u32)(NB(Uint) *self) { return (NB(U32)) *self; }
static inline NB(U64) NB(Uint$To_u64)(NB(Uint) *self) { return (NB(U64)) *self; }
static inline NB(Float) NB(Uint$Round_float)(NB(Uint) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(Uint$Round_double)(NB(Uint) *self) { return (NB(Double)) *self; }

static inline NB(Uintptr) NB(Intptr$force_unsigned)(NB(Intptr) *self) { return (NB(Uintptr)) *self; }
static inline NB(I64) NB(Intptr$To_i64)(NB(Intptr) *self) { return (NB(I64)) *self; }

static inline NB(Intptr) NB(Uintptr$force_signed)(NB(Uintptr) *self) { return (NB(Intptr)) *self; }
static inline NB(U64) NB(Uintptr$To_u64)(NB(Uintptr) *self) { return (NB(U64)) *self; }

static inline NB(I8) NB(Float$Trim_i8)(NB(Float) *self) { return (NB(I8)) *self; }
static inline NB(I16) NB(Float$Trim_i16)(NB(Float) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(Float$To_i32)(NB(Float) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(Float$To_i64)(NB(Float) *self) { return (NB(I64)) *self; }
static inline NB(Double) NB(Float$Exact_double)(NB(Float) *self) { return (NB(Double)) *self; }
static inline NB(Int) NB(Float$force_round_int)(NB(Float) *self) { return (NB(Int)) *self; }
static inline NB(Uint) NB(Float$force_round_uint)(NB(Float) *self) { return (NB(Uint)) *self; }

static inline NB(I8) NB(Double$Trim_i8)(NB(Double) *self) { return (NB(I8)) *self; }
static inline NB(I16) NB(Double$Trim_i16)(NB(Double) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(Double$Trim_i32)(NB(Double) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(Double$To_i64)(NB(Double) *self) { return (NB(I64)) *self; }
static inline NB(Float) NB(Double$Round_float)(NB(Double) *self) { return (NB(Float)) *self; }
static inline NB(Int) NB(Double$force_round_int)(NB(Double) *self) { return (NB(Int)) *self; }
static inline NB(Uint) NB(Double$force_round_uint)(NB(Double) *self) { return (NB(Uint)) *self; }

#define n$builtins$Likely(x) __builtin_expect(!!(x), 1)
#define n$builtins$Unlikely(x) __builtin_expect(!!(x), 0)

static inline NB(U8) *NB(Static_array_at_byte)(NB(U8) *p, NB(Uint) off) {
  return p + off;
}

#endif

#ifdef NLANG_DECLARE_FUNCTIONS

NB(Void) *NB(Nonnull_void)(void);

#endif

#ifdef NLANG_DEFINE_FUNCTIONS

static inline NB(U8) *n$builtins$Internal_realloc0(NB(U8) *ap, NB(Uint) old_bsz, NB(Uint) bsz) {
  if (old_bsz == bsz) {
    return ap;
  }

  if (bsz == 0) {
    free(ap);
    return NULL;
  }

  NB(U8) *r;
  if (ap == NULL) {
    // Use calloc(3) in the hope that on occasion it is able to obtain
    // memory already zeroed.
    r = calloc(bsz, 1);
    if (r == NULL) {
      NB(Abort)();
    }
  } else {
    r = realloc(ap, bsz);
    if (r == NULL) {
      NB(Abort)();
    }
    memset(r + old_bsz, 0, bsz - old_bsz);
  }
  return r;
}

static inline void n$builtins$Internal_free(NB(U8) *ap, NB(Uint) bsz) {
  free(ap);
}

#endif

#undef NB
