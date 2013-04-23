#define NB(n) nlang_builtins_##n

#define define_native_boolean(t) \
  NB(bool) t##_operator_eq(const t *self, const t *other) { return *self == *other; } \
  NB(bool) t##_operator_ne(const t *self, const t *other) { return *self != *other; } \
  \
  NB(i32) t##_operator_compare(const t *self, const t *other) { return memcmp(self, other, sizeof(*self)); } \
  \
  NB(bool) t##_operator_le(const t *self, const t *other) { return *self <= * other; } \
  NB(bool) t##_operator_lt(const t *self, const t *other) { return *self < * other; } \
  NB(bool) t##_operator_gt(const t *self, const t *other) { return *self > * other; } \
  NB(bool) t##_operator_ge(const t *self, const t *other) { return *self >= * other; } \
  \
  t t##_operator_or(const t *self, const t *other) { return *self || *other; } \
  t t##_operator_and(const t *self, const t *other) { return *self && *other; } \
  t t##_operator_not(const t *self) { return ! *self; } \
  \
  void t##_copy_ctor(t *self, const t *other) { *self = *other; }

#define define_native_integer(t) \
  NB(bool) t##_operator_eq(const t *self, const t *other) { return *self == *other; } \
  NB(bool) t##_operator_ne(const t *self, const t *other) { return *self != *other; } \
  \
  NB(i32) t##_operator_compare(const t *self, const t *other) { return memcmp(self, other, sizeof(*self)); } \
  \
  NB(bool) t##_operator_le(const t *self, const t *other) { return *self <= * other; } \
  NB(bool) t##_operator_lt(const t *self, const t *other) { return *self < * other; } \
  NB(bool) t##_operator_gt(const t *self, const t *other) { return *self > * other; } \
  NB(bool) t##_operator_ge(const t *self, const t *other) { return *self >= * other; } \
  \
  t t##_operator_plus(const t *self, const t *other) { return *self + *other; } \
  t t##_operator_minus(const t *self, const t *other) { return *self - *other; } \
  t t##_operator_divide(const t *self, const t *other) { return *self / *other; } \
  t t##_operator_modulo(const t *self, const t *other) { return *self % *other; } \
  t t##_operator_times(const t *self, const t *other) { return *self * *other; } \
  t t##_operator_uminus(const t *self) { return - *self; } \
  NB(void) t##_operator_assign_plus(t *self, const t *other) { *self += *other; } \
  NB(void) t##_operator_assign_minus(t *self, const t *other) { *self -= *other; } \
  NB(void) t##_operator_assign_divide(t *self, const t *other) { *self /= *other; } \
  NB(void) t##_operator_assign_modulo(t *self, const t *other) { *self %= *other; } \
  NB(void) t##_operator_assign_times(t *self, const t *other) { *self *= *other; } \
  \
  t t##_operator_bwor(const t *self, const t *other) { return *self | *other; } \
  t t##_operator_bwxor(const t *self, const t *other) { return *self ^ *other; } \
  t t##_operator_bwand(const t *self, const t *other) { return *self & *other; } \
  t t##_operator_lshift(const t *self, const NB(u32) by) { return *self << by; } \
  t t##_operator_rshift(const t *self, const NB(u32) by) { return *self >> by; } \
  t t##_operator_bwnot(const t *self) { return ~ *self; } \
  NB(void) t##_operator_assign_bwor(t *self, const t *other) { *self |= *other; } \
  NB(void) t##_operator_assign_bwxor(t *self, const t *other) { *self ^= *other; } \
  NB(void) t##_operator_assign_bwand(t *self, const t *other) { *self &= *other; } \
  NB(void) t##_operator_assign_lshift(t *self, const NB(u32) by) { *self <<= by; } \
  NB(void) t##_operator_assign_rshift(t *self, const NB(u32) by) { *self >>= by; } \
  \
  void t##_copy_ctor(t *self, const t *other) { *self = * other; }

#define define_native_floating(t) \
  NB(bool) t##_operator_eq(const t *self, const t *other) { return *self == *other; } \
  NB(bool) t##_operator_ne(const t *self, const t *other) { return *self != *other; } \
  \
  NB(i32) t##_operator_compare(const t *self, const t *other) { return memcmp(self, other, sizeof(*self)); } \
  \
  NB(bool) t##_operator_le(const t *self, const t *other) { return *self <= * other; } \
  NB(bool) t##_operator_lt(const t *self, const t *other) { return *self < * other; } \
  NB(bool) t##_operator_gt(const t *self, const t *other) { return *self > * other; } \
  NB(bool) t##_operator_ge(const t *self, const t *other) { return *self >= * other; } \
  \
  t t##_operator_plus(const t *self, const t *other) { return *self + *other; } \
  t t##_operator_minus(const t *self, const t *other) { return *self - *other; } \
  t t##_operator_divide(const t *self, const t *other) { return *self / *other; } \
  t t##_operator_times(const t *self, const t *other) { return *self * *other; } \
  t t##_operator_uminus(const t *self) { return - *self; } \
  \
  void t##_copy_ctor(t *self, const t *other) { *self = * other; }

define_native_boolean(nlang_builtins_bool)
define_native_integer(nlang_builtins_i8)
define_native_integer(nlang_builtins_i16)
define_native_integer(nlang_builtins_i32)
define_native_integer(nlang_builtins_i64)
define_native_integer(nlang_builtins_u8)
define_native_integer(nlang_builtins_u16)
define_native_integer(nlang_builtins_u32)
define_native_integer(nlang_builtins_u64)
define_native_integer(nlang_builtins_size)
define_native_integer(nlang_builtins_ssize)
define_native_floating(nlang_builtins_float)
define_native_floating(nlang_builtins_double)

#undef define_native_floating
#undef define_native_boolean
#undef define_native_integer

void NB(abort)(void) {
  abort();
}

#undef NB
