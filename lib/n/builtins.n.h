#define NB(n) n$builtins$##n

#ifdef NLANG_DEFINE_TYPES
#include <stdarg.h>

struct NB(Valist) {
  va_list ap;
};

#define NLANG_BUILTINS_DEFINE_ENVPARENT(envt) envt _$Nenvparent_##envt

#endif

#ifdef NLANG_DEFINE_FUNCTIONS

static inline NB(U8) *NB(Slice_at_byte)(NB(U8) *p, NB(Uint) off) {
  return p + off;
}

static inline NB(Void) NB(Slice_memcpy)(NB(U8) *dst, const NB(U8) *src, NB(Uint) count) {
  memcpy(dst, src, count);
}

#define NLANG_BUILTINS_VARARG_START(va) do { \
  va_start((va).ap.ap, _$Nvarargcount); \
  (va).n = _$Nvarargcount; \
} while (0)

#define NLANG_BUILTINS_VARARG_END(va) do { \
  va_end((va).ap.ap); \
} while (0)

#define NLANG_BUILTINS_VARARG_NEXT(t, va) \
  ({ n$builtins$Assert((va).n > 0, NULL); \
   (va).n -= 1; \
   va_arg((va).ap.ap, t); })

#define NLANG_BUILTINS_BG_ENVIRONMENT_PARENT(envt) do { \
  return self->_$Nenvparent_##envt; \
} while (0)

#define NLANG_BUILTINS_BG_ENVIRONMENT_INSTALL(envt) do { \
  self->_$Nenvparent_##envt = *where; \
  *where = THIS(_$Nmkdyn__##envt)(self); \
} while (0)

#define NLANG_BUILTINS_BG_ENVIRONMENT_UNINSTALL(envt) do { \
  *where = where->vptr->parent(where->obj); \
} while (0)


#define define_native_boolean(t) \
  static inline NB(Bool) t##$Operator_eq(const t *self, const t *other) { return *self == *other; } \
  static inline NB(Bool) t##$Operator_ne(const t *self, const t *other) { return *self != *other; } \
  \
  static inline NB(Bool) t##$Operator_le(const t *self, const t *other) { return *self <= * other; } \
  static inline NB(Bool) t##$Operator_lt(const t *self, const t *other) { return *self < * other; } \
  static inline NB(Bool) t##$Operator_gt(const t *self, const t *other) { return *self > * other; } \
  static inline NB(Bool) t##$Operator_ge(const t *self, const t *other) { return *self >= * other; } \
  \
  static inline t t##$Operator_test(const t *self) { return *self; } \
  static inline t t##$Operator_not(const t *self) { return ! *self; }

#define define_native_integer(t) \
  static inline NB(Bool) t##$Operator_eq(const t *self, const t *other) { return *self == *other; } \
  static inline NB(Bool) t##$Operator_ne(const t *self, const t *other) { return *self != *other; } \
  \
  static inline NB(Bool) t##$Operator_le(const t *self, const t *other) { return *self <= * other; } \
  static inline NB(Bool) t##$Operator_lt(const t *self, const t *other) { return *self < * other; } \
  static inline NB(Bool) t##$Operator_gt(const t *self, const t *other) { return *self > * other; } \
  static inline NB(Bool) t##$Operator_ge(const t *self, const t *other) { return *self >= * other; } \
  \
  static inline t t##$Operator_plus(const t *self, const t *other) { return *self + *other; } \
  static inline t t##$Operator_minus(const t *self, const t *other) { return *self - *other; } \
  static inline t t##$Operator_divide(const t *self, const t *other) { return *self / *other; } \
  static inline t t##$Operator_modulo(const t *self, const t *other) { return *self % *other; } \
  static inline t t##$Operator_times(const t *self, const t *other) { return *self * *other; } \
  static inline t t##$Operator_uminus(const t *self) { return - *self; } \
  static inline NB(Void) t##$Operator_assign_plus(t *self, const t *other) { *self += *other; } \
  static inline NB(Void) t##$Operator_assign_minus(t *self, const t *other) { *self -= *other; } \
  static inline NB(Void) t##$Operator_assign_divide(t *self, const t *other) { *self /= *other; } \
  static inline NB(Void) t##$Operator_assign_modulo(t *self, const t *other) { *self %= *other; } \
  static inline NB(Void) t##$Operator_assign_times(t *self, const t *other) { *self *= *other; } \

#define define_native_sized_integer(t) \
  define_native_integer(t) \
  static inline t t##$Operator_bwor(const t *self, const t *other) { return *self | *other; } \
  static inline t t##$Operator_bwxor(const t *self, const t *other) { return *self ^ *other; } \
  static inline t t##$Operator_bwand(const t *self, const t *other) { return *self & *other; } \
  static inline t t##$Operator_lshift(const t *self, const NB(U32) by) { return *self << by; } \
  static inline t t##$Operator_rshift(const t *self, const NB(U32) by) { return *self >> by; } \
  static inline t t##$Operator_bwnot(const t *self) { return ~ *self; } \
  static inline NB(Void) t##$Operator_assign_bwor(t *self, const t *other) { *self |= *other; } \
  static inline NB(Void) t##$Operator_assign_bwxor(t *self, const t *other) { *self ^= *other; } \
  static inline NB(Void) t##$Operator_assign_bwand(t *self, const t *other) { *self &= *other; } \
  static inline NB(Void) t##$Operator_assign_lshift(t *self, const NB(U32) by) { *self <<= by; } \
  static inline NB(Void) t##$Operator_assign_rshift(t *self, const NB(U32) by) { *self >>= by; }

#define define_native_sized_signed_integer(t, uns) \
  define_native_sized_integer(t) \
  static inline uns t##$Bitwise_unsigned(const t *self) { return (uns) *self; }

#define define_native_sized_unsigned_integer(t, sig) \
  define_native_sized_integer(t) \
  static inline sig t##$Bitwise_signed(const t *self) { return (sig) *self; }

#define define_native_floating(t) \
  static inline NB(Bool) t##$Operator_eq(const t *self, const t *other) { return *self == *other; } \
  static inline NB(Bool) t##$Operator_ne(const t *self, const t *other) { return *self != *other; } \
  \
  static inline NB(Bool) t##$Operator_le(const t *self, const t *other) { return *self <= * other; } \
  static inline NB(Bool) t##$Operator_lt(const t *self, const t *other) { return *self < * other; } \
  static inline NB(Bool) t##$Operator_gt(const t *self, const t *other) { return *self > * other; } \
  static inline NB(Bool) t##$Operator_ge(const t *self, const t *other) { return *self >= * other; } \
  \
  static inline t t##$Operator_plus(const t *self, const t *other) { return *self + *other; } \
  static inline t t##$Operator_minus(const t *self, const t *other) { return *self - *other; } \
  static inline t t##$Operator_divide(const t *self, const t *other) { return *self / *other; } \
  static inline t t##$Operator_times(const t *self, const t *other) { return *self * *other; } \
  static inline t t##$Operator_uminus(const t *self) { return - *self; } \
  static inline NB(Void) t##$Operator_assign_plus(t *self, const t *other) { *self += *other; } \
  static inline NB(Void) t##$Operator_assign_minus(t *self, const t *other) { *self -= *other; } \
  static inline NB(Void) t##$Operator_assign_divide(t *self, const t *other) { *self /= *other; } \
  static inline NB(Void) t##$Operator_assign_times(t *self, const t *other) { *self *= *other; } \

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
define_native_floating(n$builtins$Float)
define_native_floating(n$builtins$Double)


static inline NB(I16) NB(I8$To_i16)(const NB(I8) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(I8$To_i32)(const NB(I8) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(I8$To_i64)(const NB(I8) *self) { return (NB(I64)) *self; }
static inline NB(Int) NB(I8$To_int)(const NB(I8) *self) { return (NB(Int)) *self; }
static inline NB(Float) NB(I8$Exact_float)(const NB(I8) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(I8$Exact_double)(const NB(I8) *self) { return (NB(Double)) *self; }

static inline NB(I8) NB(I16$Trim_i8)(const NB(I16) *self) { return (NB(I8)) *self; }
static inline NB(I32) NB(I16$To_i32)(const NB(I16) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(I16$To_i64)(const NB(I16) *self) { return (NB(I64)) *self; }
static inline NB(Int) NB(I16$To_int)(const NB(I16) *self) { return (NB(Int)) *self; }
static inline NB(Float) NB(I16$Exact_float)(const NB(I16) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(I16$Exact_double)(const NB(I16) *self) { return (NB(Double)) *self; }

static inline NB(I8) NB(I32$Trim_i8)(const NB(I32) *self) { return (NB(I8)) *self; }
static inline NB(I16) NB(I32$Trim_i16)(const NB(I32) *self) { return (NB(I16)) *self; }
static inline NB(I64) NB(I32$To_i64)(const NB(I32) *self) { return (NB(I64)) *self; }
static inline NB(Int) NB(I32$To_int)(const NB(I32) *self) { return (NB(Int)) *self; }
static inline NB(Float) NB(I32$Round_float)(const NB(I32) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(I32$Exact_double)(const NB(I32) *self) { return (NB(Double)) *self; }

static inline NB(Int) NB(I64$force_int)(const NB(I64) *self) { return (NB(Int)) *self; }
static inline NB(I8) NB(I64$Trim_i8)(const NB(I64) *self) { return (NB(I8)) *self; }
static inline NB(I16) NB(I64$Trim_i16)(const NB(I64) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(I64$Trim_i32)(const NB(I64) *self) { return (NB(I32)) *self; }
static inline NB(Float) NB(I64$Round_float)(const NB(I64) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(I64$Round_double)(const NB(I64) *self) { return (NB(Double)) *self; }

static inline NB(U16) NB(U8$To_u16)(const NB(U8) *self) { return (NB(U16)) *self; }
static inline NB(U32) NB(U8$To_u32)(const NB(U8) *self) { return (NB(U32)) *self; }
static inline NB(U64) NB(U8$To_u64)(const NB(U8) *self) { return (NB(U64)) *self; }
static inline NB(Uint) NB(U8$To_uint)(const NB(U8) *self) { return (NB(Uint)) *self; }
static inline NB(Float) NB(U8$Exact_float)(const NB(U8) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(U8$Exact_double)(const NB(U8) *self) { return (NB(Double)) *self; }

static inline NB(U8) NB(U16$Trim_u8)(const NB(U16) *self) { return (NB(U8)) *self; }
static inline NB(U32) NB(U16$To_u32)(const NB(U16) *self) { return (NB(U32)) *self; }
static inline NB(U64) NB(U16$To_u64)(const NB(U16) *self) { return (NB(U64)) *self; }
static inline NB(Uint) NB(U16$To_uint)(const NB(U16) *self) { return (NB(Uint)) *self; }
static inline NB(Float) NB(U16$Exact_float)(const NB(U16) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(U16$Exact_double)(const NB(U16) *self) { return (NB(Double)) *self; }

static inline NB(U8) NB(U32$Trim_u8)(const NB(U32) *self) { return (NB(U8)) *self; }
static inline NB(U16) NB(U32$Trim_u16)(const NB(U32) *self) { return (NB(U16)) *self; }
static inline NB(U64) NB(U32$To_u64)(const NB(U32) *self) { return (NB(U64)) *self; }
static inline NB(Uint) NB(U32$To_uint)(const NB(U32) *self) { return (NB(Uint)) *self; }
static inline NB(Float) NB(U32$Round_float)(const NB(U32) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(U32$Exact_double)(const NB(U32) *self) { return (NB(Double)) *self; }

static inline NB(Uint) NB(U64$force_uint)(const NB(U64) *self) { return (NB(Uint)) *self; }
static inline NB(U8) NB(U64$Trim_u8)(const NB(U64) *self) { return (NB(U8)) *self; }
static inline NB(U16) NB(U64$Trim_u16)(const NB(U64) *self) { return (NB(U16)) *self; }
static inline NB(U32) NB(U64$Trim_u32)(const NB(U64) *self) { return (NB(U32)) *self; }
static inline NB(Float) NB(U64$Round_float)(const NB(U64) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(U64$Round_double)(const NB(U64) *self) { return (NB(Double)) *self; }

static inline NB(Uint) NB(Int$force_unsigned)(const NB(Int) *self) { return (NB(Uint)) *self; }
static inline NB(I8) NB(Int$Trim_i8)(const NB(Int) *self) { return (NB(I8)) *self; }
static inline NB(I16) NB(Int$Trim_i16)(const NB(Int) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(Int$Trim_i32)(const NB(Int) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(Int$To_i64)(const NB(Int) *self) { return (NB(I64)) *self; }
static inline NB(Float) NB(Int$Round_float)(const NB(Int) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(Int$Round_double)(const NB(Int) *self) { return (NB(Double)) *self; }

static inline NB(Int) NB(Uint$force_signed)(const NB(Uint) *self) { return (NB(Int)) *self; }
static inline NB(U8) NB(Uint$Trim_u8)(const NB(Uint) *self) { return (NB(U8)) *self; }
static inline NB(U16) NB(Uint$Trim_u16)(const NB(Uint) *self) { return (NB(U16)) *self; }
static inline NB(U32) NB(Uint$Trim_u32)(const NB(Uint) *self) { return (NB(U32)) *self; }
static inline NB(U64) NB(Uint$To_u64)(const NB(Uint) *self) { return (NB(U64)) *self; }
static inline NB(Float) NB(Uint$Round_float)(const NB(Uint) *self) { return (NB(Float)) *self; }
static inline NB(Double) NB(Uint$Round_double)(const NB(Uint) *self) { return (NB(Double)) *self; }

static inline NB(I8) NB(Float$Trim_i8)(const NB(Float) *self) { return (NB(I8)) *self; }
static inline NB(I16) NB(Float$Trim_i16)(const NB(Float) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(Float$To_i32)(const NB(Float) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(Float$To_i64)(const NB(Float) *self) { return (NB(I64)) *self; }
static inline NB(Double) NB(Float$Exact_double)(const NB(Float) *self) { return (NB(Double)) *self; }
static inline NB(Int) NB(Float$force_round_int)(const NB(Float) *self) { return (NB(Int)) *self; }
static inline NB(Uint) NB(Float$force_round_uint)(const NB(Float) *self) { return (NB(Uint)) *self; }

static inline NB(I8) NB(Double$Trim_i8)(const NB(Double) *self) { return (NB(I8)) *self; }
static inline NB(I16) NB(Double$Trim_i16)(const NB(Double) *self) { return (NB(I16)) *self; }
static inline NB(I32) NB(Double$Trim_i32)(const NB(Double) *self) { return (NB(I32)) *self; }
static inline NB(I64) NB(Double$To_i64)(const NB(Double) *self) { return (NB(I64)) *self; }
static inline NB(Float) NB(Double$Round_float)(const NB(Double) *self) { return (NB(Float)) *self; }
static inline NB(Int) NB(Double$force_round_int)(const NB(Double) *self) { return (NB(Int)) *self; }
static inline NB(Uint) NB(Double$force_round_uint)(const NB(Double) *self) { return (NB(Uint)) *self; }

#define n$builtins$Likely(x) __builtin_expect(!!(x), 1)
#define n$builtins$Unlikely(x) __builtin_expect(!!(x), 0)

static inline NB(U8) *NB(Static_array_at_byte)(NB(U8) *p, NB(Uint) off) {
  return p + off;
}

#endif

#ifdef NLANG_DECLARE_FUNCTIONS

const NB(Void) *NB(Nonnull_void)(void);

#endif

#undef NB
