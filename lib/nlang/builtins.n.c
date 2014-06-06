#include <inttypes.h>
#include <stdio.h>

#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(n) nlang$builtins$##n

static void native_write_buffer(_$Ndyn_nlang$chars$_$Ni_string_buffer buf, const char *s) {
  for (int i = 0; s[i] != '\0'; ++i) {
    nlang$chars$_$Ni_string_buffer$push(buf, nlang$chars$char$from_ascii((NB(u8)) s[i]));
  }
}

#define define_native_boolean(t) \
  NB(bool) t##$operator_eq(const t *self, const t *other) { return *self == *other; } \
  NB(bool) t##$operator_ne(const t *self, const t *other) { return *self != *other; } \
  \
  NB(bool) t##$operator_le(const t *self, const t *other) { return *self <= * other; } \
  NB(bool) t##$operator_lt(const t *self, const t *other) { return *self < * other; } \
  NB(bool) t##$operator_gt(const t *self, const t *other) { return *self > * other; } \
  NB(bool) t##$operator_ge(const t *self, const t *other) { return *self >= * other; } \
  \
  t t##$operator_test(const t *self) { return *self; } \
  t t##$operator_or(const t *self, const t *other) { return *self || *other; } \
  t t##$operator_and(const t *self, const t *other) { return *self && *other; } \
  t t##$operator_not(const t *self) { return ! *self; } \
  \
  void t##$show(const t *self, _$Ndyn_nlang$chars$_$Ni_string_buffer buf) { \
    native_write_buffer(buf, *self ? "true" : "false"); \
  }

// ln(2^64)/ln(10) = 19.27
#define define_show_number(t, fmt) \
  void t##$show(const t *self, _$Ndyn_nlang$chars$_$Ni_string_buffer buf) { \
    char s[32]; \
    snprintf(s, 32, fmt, *self); \
    native_write_buffer(buf, s); \
  }

#define define_native_integer(t) \
  NB(bool) t##$operator_eq(const t *self, const t *other) { return *self == *other; } \
  NB(bool) t##$operator_ne(const t *self, const t *other) { return *self != *other; } \
  \
  NB(bool) t##$operator_le(const t *self, const t *other) { return *self <= * other; } \
  NB(bool) t##$operator_lt(const t *self, const t *other) { return *self < * other; } \
  NB(bool) t##$operator_gt(const t *self, const t *other) { return *self > * other; } \
  NB(bool) t##$operator_ge(const t *self, const t *other) { return *self >= * other; } \
  \
  t t##$operator_plus(const t *self, const t *other) { return *self + *other; } \
  t t##$operator_minus(const t *self, const t *other) { return *self - *other; } \
  t t##$operator_divide(const t *self, const t *other) { return *self / *other; } \
  t t##$operator_modulo(const t *self, const t *other) { return *self % *other; } \
  t t##$operator_times(const t *self, const t *other) { return *self * *other; } \
  t t##$operator_uminus(const t *self) { return - *self; } \
  NB(void) t##$operator_assign_plus(t *self, const t *other) { *self += *other; } \
  NB(void) t##$operator_assign_minus(t *self, const t *other) { *self -= *other; } \
  NB(void) t##$operator_assign_divide(t *self, const t *other) { *self /= *other; } \
  NB(void) t##$operator_assign_modulo(t *self, const t *other) { *self %= *other; } \
  NB(void) t##$operator_assign_times(t *self, const t *other) { *self *= *other; } \
  \
  t t##$operator_bwor(const t *self, const t *other) { return *self | *other; } \
  t t##$operator_bwxor(const t *self, const t *other) { return *self ^ *other; } \
  t t##$operator_bwand(const t *self, const t *other) { return *self & *other; } \
  t t##$operator_lshift(const t *self, const NB(u32) by) { return *self << by; } \
  t t##$operator_rshift(const t *self, const NB(u32) by) { return *self >> by; } \
  t t##$operator_bwnot(const t *self) { return ~ *self; } \
  NB(void) t##$operator_assign_bwor(t *self, const t *other) { *self |= *other; } \
  NB(void) t##$operator_assign_bwxor(t *self, const t *other) { *self ^= *other; } \
  NB(void) t##$operator_assign_bwand(t *self, const t *other) { *self &= *other; } \
  NB(void) t##$operator_assign_lshift(t *self, const NB(u32) by) { *self <<= by; } \
  NB(void) t##$operator_assign_rshift(t *self, const NB(u32) by) { *self >>= by; }

#define define_native_floating(t) \
  NB(bool) t##$operator_eq(const t *self, const t *other) { return *self == *other; } \
  NB(bool) t##$operator_ne(const t *self, const t *other) { return *self != *other; } \
  \
  NB(bool) t##$operator_le(const t *self, const t *other) { return *self <= * other; } \
  NB(bool) t##$operator_lt(const t *self, const t *other) { return *self < * other; } \
  NB(bool) t##$operator_gt(const t *self, const t *other) { return *self > * other; } \
  NB(bool) t##$operator_ge(const t *self, const t *other) { return *self >= * other; } \
  \
  t t##$operator_plus(const t *self, const t *other) { return *self + *other; } \
  t t##$operator_minus(const t *self, const t *other) { return *self - *other; } \
  t t##$operator_divide(const t *self, const t *other) { return *self / *other; } \
  t t##$operator_times(const t *self, const t *other) { return *self * *other; } \
  t t##$operator_uminus(const t *self) { return - *self; }

define_native_boolean(nlang$builtins$bool)
define_native_integer(nlang$builtins$i8)
define_native_integer(nlang$builtins$i16)
define_native_integer(nlang$builtins$i32)
define_native_integer(nlang$builtins$i64)
define_native_integer(nlang$builtins$u8)
define_native_integer(nlang$builtins$u16)
define_native_integer(nlang$builtins$u32)
define_native_integer(nlang$builtins$u64)
define_native_integer(nlang$builtins$size)
define_native_integer(nlang$builtins$ssize)
define_native_floating(nlang$builtins$float)
define_native_floating(nlang$builtins$double)

define_show_number(nlang$builtins$i8, "%"PRId8)
define_show_number(nlang$builtins$i16, "%"PRId16)
define_show_number(nlang$builtins$i32, "%"PRId32)
define_show_number(nlang$builtins$i64, "%"PRId64)
define_show_number(nlang$builtins$u8, "%"PRIu8)
define_show_number(nlang$builtins$u16, "%"PRIu16)
define_show_number(nlang$builtins$u32, "%"PRIu32)
define_show_number(nlang$builtins$u64, "%"PRIu64)
define_show_number(nlang$builtins$size, "%zu")
define_show_number(nlang$builtins$ssize, "%zd")
define_show_number(nlang$builtins$float, "%f")
define_show_number(nlang$builtins$double, "%f")

#undef define_show_number
#undef define_native_floating
#undef define_native_boolean
#undef define_native_integer

void NB(abort)(void) {
  abort();
}

#undef NB

#endif
