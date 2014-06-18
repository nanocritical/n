#include <inttypes.h>
#include <stdio.h>

#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(n) n$builtins$##n

const NB(Void) *NB(Nonnull_void)(void) {
  static NB(U32) dummy;
  return &dummy;
}

static void native_write_buffer(_$Ndyn_n$chars$_$Ni_String_buffer buf, const char *s) {
  for (int i = 0; s[i] != '\0'; ++i) {
    n$chars$_$Ni_String_buffer$Push(buf, n$chars$Char$From_ascii((NB(U8)) s[i]));
  }
}

#define define_native_boolean(t) \
  NB(Bool) t##$Operator_eq(const t *self, const t *other) { return *self == *other; } \
  NB(Bool) t##$Operator_ne(const t *self, const t *other) { return *self != *other; } \
  \
  NB(Bool) t##$Operator_le(const t *self, const t *other) { return *self <= * other; } \
  NB(Bool) t##$Operator_lt(const t *self, const t *other) { return *self < * other; } \
  NB(Bool) t##$Operator_gt(const t *self, const t *other) { return *self > * other; } \
  NB(Bool) t##$Operator_ge(const t *self, const t *other) { return *self >= * other; } \
  \
  t t##$Operator_test(const t *self) { return *self; } \
  t t##$Operator_or(const t *self, const t *other) { return *self || *other; } \
  t t##$Operator_and(const t *self, const t *other) { return *self && *other; } \
  t t##$Operator_not(const t *self) { return ! *self; } \
  \
  void t##$Show(const t *self, _$Ndyn_n$chars$_$Ni_String_buffer buf) { \
    native_write_buffer(buf, *self ? "true" : "false"); \
  }

// ln(2^64)/ln(10) = 19.27
#define define_show_number(t, fmt) \
  void t##$Show(const t *self, _$Ndyn_n$chars$_$Ni_String_buffer buf) { \
    char s[32]; \
    snprintf(s, 32, fmt, *self); \
    native_write_buffer(buf, s); \
  }

#define define_native_integer(t) \
  NB(Bool) t##$Operator_eq(const t *self, const t *other) { return *self == *other; } \
  NB(Bool) t##$Operator_ne(const t *self, const t *other) { return *self != *other; } \
  \
  NB(Bool) t##$Operator_le(const t *self, const t *other) { return *self <= * other; } \
  NB(Bool) t##$Operator_lt(const t *self, const t *other) { return *self < * other; } \
  NB(Bool) t##$Operator_gt(const t *self, const t *other) { return *self > * other; } \
  NB(Bool) t##$Operator_ge(const t *self, const t *other) { return *self >= * other; } \
  \
  t t##$Operator_plus(const t *self, const t *other) { return *self + *other; } \
  t t##$Operator_minus(const t *self, const t *other) { return *self - *other; } \
  t t##$Operator_divide(const t *self, const t *other) { return *self / *other; } \
  t t##$Operator_modulo(const t *self, const t *other) { return *self % *other; } \
  t t##$Operator_times(const t *self, const t *other) { return *self * *other; } \
  t t##$Operator_uminus(const t *self) { return - *self; } \
  NB(Void) t##$Operator_assign_plus(t *self, const t *other) { *self += *other; } \
  NB(Void) t##$Operator_assign_minus(t *self, const t *other) { *self -= *other; } \
  NB(Void) t##$Operator_assign_divide(t *self, const t *other) { *self /= *other; } \
  NB(Void) t##$Operator_assign_modulo(t *self, const t *other) { *self %= *other; } \
  NB(Void) t##$Operator_assign_times(t *self, const t *other) { *self *= *other; } \
  \
  t t##$Operator_bwor(const t *self, const t *other) { return *self | *other; } \
  t t##$Operator_bwxor(const t *self, const t *other) { return *self ^ *other; } \
  t t##$Operator_bwand(const t *self, const t *other) { return *self & *other; } \
  t t##$Operator_lshift(const t *self, const NB(U32) by) { return *self << by; } \
  t t##$Operator_rshift(const t *self, const NB(U32) by) { return *self >> by; } \
  t t##$Operator_bwnot(const t *self) { return ~ *self; } \
  NB(Void) t##$Operator_assign_bwor(t *self, const t *other) { *self |= *other; } \
  NB(Void) t##$Operator_assign_bwxor(t *self, const t *other) { *self ^= *other; } \
  NB(Void) t##$Operator_assign_bwand(t *self, const t *other) { *self &= *other; } \
  NB(Void) t##$Operator_assign_lshift(t *self, const NB(U32) by) { *self <<= by; } \
  NB(Void) t##$Operator_assign_rshift(t *self, const NB(U32) by) { *self >>= by; }

#define define_native_floating(t) \
  NB(Bool) t##$Operator_eq(const t *self, const t *other) { return *self == *other; } \
  NB(Bool) t##$Operator_ne(const t *self, const t *other) { return *self != *other; } \
  \
  NB(Bool) t##$Operator_le(const t *self, const t *other) { return *self <= * other; } \
  NB(Bool) t##$Operator_lt(const t *self, const t *other) { return *self < * other; } \
  NB(Bool) t##$Operator_gt(const t *self, const t *other) { return *self > * other; } \
  NB(Bool) t##$Operator_ge(const t *self, const t *other) { return *self >= * other; } \
  \
  t t##$Operator_plus(const t *self, const t *other) { return *self + *other; } \
  t t##$Operator_minus(const t *self, const t *other) { return *self - *other; } \
  t t##$Operator_divide(const t *self, const t *other) { return *self / *other; } \
  t t##$Operator_times(const t *self, const t *other) { return *self * *other; } \
  t t##$Operator_uminus(const t *self) { return - *self; }

define_native_boolean(n$builtins$Bool)
define_native_integer(n$builtins$I8)
define_native_integer(n$builtins$I16)
define_native_integer(n$builtins$I32)
define_native_integer(n$builtins$I64)
define_native_integer(n$builtins$U8)
define_native_integer(n$builtins$U16)
define_native_integer(n$builtins$U32)
define_native_integer(n$builtins$U64)
define_native_integer(n$builtins$Uint)
define_native_integer(n$builtins$Int)
define_native_floating(n$builtins$Float)
define_native_floating(n$builtins$Double)

define_show_number(n$builtins$I8, "%"PRId8)
define_show_number(n$builtins$I16, "%"PRId16)
define_show_number(n$builtins$I32, "%"PRId32)
define_show_number(n$builtins$I64, "%"PRId64)
define_show_number(n$builtins$U8, "%"PRIu8)
define_show_number(n$builtins$U16, "%"PRIu16)
define_show_number(n$builtins$U32, "%"PRIu32)
define_show_number(n$builtins$U64, "%"PRIu64)
define_show_number(n$builtins$Uint, "%zu")
define_show_number(n$builtins$Int, "%zd")
define_show_number(n$builtins$Float, "%f")
define_show_number(n$builtins$Double, "%f")

#undef define_show_number
#undef define_native_floating
#undef define_native_boolean
#undef define_native_integer

void NB(Abort)(void) {
  abort();
}

#undef NB

#endif
