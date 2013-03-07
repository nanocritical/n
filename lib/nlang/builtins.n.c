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
  t t##_operator_bwor(const t *self, const t *other) { return *self | *other; } \
  t t##_operator_bwxor(const t *self, const t *other) { return *self ^ *other; } \
  t t##_operator_bwand(const t *self, const t *other) { return *self & *other; } \
  t t##_operator_lshift(const t *self, const NB(u32) by) { return *self << by; } \
  t t##_operator_rshift(const t *self, const NB(u32) by) { return *self >> by; } \
  t t##_operator_bwnot(const t *self) { return ~ *self; } \
  \
  t t##_operator_or(const t *self, const t *other) { return *self || *other; } \
  t t##_operator_and(const t *self, const t *other) { return *self && *other; } \
  t t##_operator_not(const t *self) { return ! *self; } \
  \
  void t##_operator_copy(t *self, const t *other) { *self = *other; }

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
  \
  t t##_operator_bwor(const t *self, const t *other) { return *self | *other; } \
  t t##_operator_bwxor(const t *self, const t *other) { return *self ^ *other; } \
  t t##_operator_bwand(const t *self, const t *other) { return *self & *other; } \
  t t##_operator_lshift(const t *self, const NB(u32) by) { return *self << by; } \
  t t##_operator_rshift(const t *self, const NB(u32) by) { return *self >> by; } \
  t t##_operator_bwnot(const t *self) { return ~ *self; } \
  \
  void t##_operator_copy(t *self, const t *other) { *self = * other; }

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

#undef define_native_boolean
#undef define_native_integer
#undef NB
