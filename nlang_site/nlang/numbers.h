static const u8 u8_MAX = 0xff;
static const u16 u16_MAX = 0xffff;
static const u32 u32_MAX = 0xffffffff;
static const u64 u64_MAX = 0xffffffffffffffff;
static const i8 i8_MAX = 0x7f;
static const i8 i8_MIN = -0x80;
static const i16 i16_MAX = 0x7fff;
static const i16 i16_MIN = -0x8000;
static const i32 i32_MAX = 0x7fffffff;
static const i32 i32_MIN = -0x80000000;
static const i64 i64_MAX = 0x7fffffffffffffff;
static const i64 i64_MIN = -0x8000000000000000;
static const size size_MAX = SIZE_MAX;
static const ssize ssize_MAX = SIZE_MAX / 2;
static const ssize ssize_MIN = - (SIZE_MAX / 2) - 1;


i8 i16_trim_i8(nlangcp__i16 self) {
  return (i8) *self;
}

u16 i16_reinterpret_unsigned(nlangcp__i16 self) {
  return (u16) *self;
}

i32 i16_to_i32(nlangcp__i16 self) {
  return (i32) *self;
}

i64 i16_to_i64(nlangcp__i16 self) {
  return (i64) *self;
}

u8 size_trim_u8(nlangcp__size self) {
  return (u8) *self;
}

u16 size_trim_u16(nlangcp__size self) {
  return (u16) *self;
}

u32 size_trim_u32(nlangcp__size self) {
  return (u32) *self;
}

u64 size_trim_u64(nlangcp__size self) {
  return (u64) *self;
}

ssize size_reinterpret_signed(nlangcp__size self) {
  return (ssize) *self;
}

i8 i64_trim_i8(nlangcp__i64 self) {
  return (i8) *self;
}

i16 i64_trim_i16(nlangcp__i64 self) {
  return (i16) *self;
}

i32 i64_trim_i32(nlangcp__i64 self) {
  return (i32) *self;
}

ssize i64_trim_ssize(nlangcp__i64 self) {
  return (ssize) *self;
}

u64 i64_reinterpret_unsigned(nlangcp__i64 self) {
  return (u64) *self;
}

u8 u16_trim_u8(nlangcp__u16 self) {
  return (u8) *self;
}

i16 u16_reinterpret_signed(nlangcp__u16 self) {
  return (i16) *self;
}

u32 u16_to_u32(nlangcp__u16 self) {
  return (u32) *self;
}

u64 u16_to_u64(nlangcp__u16 self) {
  return (u64) *self;
}

i8 ssize_trim_i8(nlangcp__ssize self) {
  return (i8) *self;
}

i16 ssize_trim_i16(nlangcp__ssize self) {
  return (i16) *self;
}

i32 ssize_trim_i32(nlangcp__ssize self) {
  return (i32) *self;
}

i64 ssize_trim_i64(nlangcp__ssize self) {
  return (i64) *self;
}

size ssize_reinterpret_unsigned(nlangcp__ssize self) {
  return (size) *self;
}

u8 u32_trim_u8(nlangcp__u32 self) {
  return (u8) *self;
}

u16 u32_trim_u16(nlangcp__u32 self) {
  return (u16) *self;
}

i32 u32_reinterpret_signed(nlangcp__u32 self) {
  return (i32) *self;
}

u64 u32_to_u64(nlangcp__u32 self) {
  return (u64) *self;
}

i8 u8_reinterpret_signed(nlangcp__u8 self) {
  return (i8) *self;
}

u16 u8_to_u16(nlangcp__u8 self) {
  return (u16) *self;
}

u32 u8_to_u32(nlangcp__u8 self) {
  return (u32) *self;
}

u64 u8_to_u64(nlangcp__u8 self) {
  return (u64) *self;
}

i8 i32_trim_i8(nlangcp__i32 self) {
  return (i8) *self;
}

i16 i32_trim_i16(nlangcp__i32 self) {
  return (i16) *self;
}

u32 i32_reinterpret_unsigned(nlangcp__i32 self) {
  return (u32) *self;
}

i64 i32_to_i64(nlangcp__i32 self) {
  return (i64) *self;
}

u8 i8_reinterpret_unsigned(nlangcp__i8 self) {
  return (u8) *self;
}

i16 i8_to_i16(nlangcp__i8 self) {
  return (i16) *self;
}

i32 i8_to_i32(nlangcp__i8 self) {
  return (i32) *self;
}

i64 i8_to_i64(nlangcp__i8 self) {
  return (i64) *self;
}

u8 u64_trim_u8(nlangcp__u64 self) {
  return (u8) *self;
}

u16 u64_trim_u16(nlangcp__u64 self) {
  return (u16) *self;
}

u32 u64_trim_u32(nlangcp__u64 self) {
  return (u32) *self;
}

size u64_trim_size(nlangcp__u64 self) {
  return (size) *self;
}

i64 u64_reinterpret_signed(nlangcp__u64 self) {
  return (i64) *self;
}


size u8_operator_hash__(nlangcp__u8 self) { return *self; }
size u16_operator_hash__(nlangcp__u16 self) { return *self; }
size u32_operator_hash__(nlangcp__u32 self) { return *self; }
size u64_operator_hash__(nlangcp__u64 self) { return *self; }
size size_operator_hash__(nlangcp__size self) { return *self; }

size i8_operator_hash__(nlangcp__i8 self) { return *self; }
size i16_operator_hash__(nlangcp__i16 self) { return *self; }
size i32_operator_hash__(nlangcp__i32 self) { return *self; }
size i64_operator_hash__(nlangcp__i64 self) { return *self; }
size ssize_operator_hash__(nlangcp__ssize self) { return *self; }

u8 u8_operator_plus__(nlangcp__u8 self, nlangcp__u8 other) { return *self + *other; }
u8 u8_operator_minus__(nlangcp__u8 self, nlangcp__u8 other) { return *self - *other; }
u8 u8_operator_times__(nlangcp__u8 self, nlangcp__u8 other) { return *self * *other; }
u8 u8_operator_divide__(nlangcp__u8 self, nlangcp__u8 other) { return *self / *other; }
u8 u8_operator_modulo__(nlangcp__u8 self, nlangcp__u8 other) { return *self % *other; }
u8 u8_operator_neg__(nlangcp__u8 self) { return - *self; }
u8 u8_operator_rshift__(nlangcp__u8 self, size n) { return *self >> n; }
u8 u8_operator_lshift__(nlangcp__u8 self, size n) { return *self << n; }
u8 u8_operator_bwand__(nlangcp__u8 self, nlangcp__u8 other) { return *self & *other; }
u8 u8_operator_bwor__(nlangcp__u8 self, nlangcp__u8 other) { return *self | *other; }
u8 u8_operator_bwxor__(nlangcp__u8 self, nlangcp__u8 other) { return *self ^ *other; }
u8 u8_operator_bwnot__(nlangcp__u8 self) { return ~ *self; }
bool u8_operator_eq__(nlangcp__u8 self, nlangcp__u8 other) { return *self == *other; }
bool u8_operator_ne__(nlangcp__u8 self, nlangcp__u8 other) { return *self != *other; }
bool u8_operator_lt__(nlangcp__u8 self, nlangcp__u8 other) { return *self < *other; }
bool u8_operator_gt__(nlangcp__u8 self, nlangcp__u8 other) { return *self > *other; }
bool u8_operator_le__(nlangcp__u8 self, nlangcp__u8 other) { return *self <= *other; }
bool u8_operator_ge__(nlangcp__u8 self, nlangcp__u8 other) { return *self >= *other; }

u16 u16_operator_plus__(nlangcp__u16 self, nlangcp__u16 other) { return *self + *other; }
u16 u16_operator_minus__(nlangcp__u16 self, nlangcp__u16 other) { return *self - *other; }
u16 u16_operator_times__(nlangcp__u16 self, nlangcp__u16 other) { return *self * *other; }
u16 u16_operator_divide__(nlangcp__u16 self, nlangcp__u16 other) { return *self / *other; }
u16 u16_operator_modulo__(nlangcp__u16 self, nlangcp__u16 other) { return *self % *other; }
u16 u16_operator_neg__(nlangcp__u16 self) { return - *self; }
u16 u16_operator_rshift__(nlangcp__u16 self, size n) { return *self >> n; }
u16 u16_operator_lshift__(nlangcp__u16 self, size n) { return *self << n; }
u16 u16_operator_bwand__(nlangcp__u16 self, nlangcp__u16 other) { return *self & *other; }
u16 u16_operator_bwor__(nlangcp__u16 self, nlangcp__u16 other) { return *self | *other; }
u16 u16_operator_bwxor__(nlangcp__u16 self, nlangcp__u16 other) { return *self ^ *other; }
u16 u16_operator_bwnot__(nlangcp__u16 self) { return ~ *self; }
bool u16_operator_eq__(nlangcp__u16 self, nlangcp__u16 other) { return *self == *other; }
bool u16_operator_ne__(nlangcp__u16 self, nlangcp__u16 other) { return *self != *other; }
bool u16_operator_lt__(nlangcp__u16 self, nlangcp__u16 other) { return *self < *other; }
bool u16_operator_gt__(nlangcp__u16 self, nlangcp__u16 other) { return *self > *other; }
bool u16_operator_le__(nlangcp__u16 self, nlangcp__u16 other) { return *self <= *other; }
bool u16_operator_ge__(nlangcp__u16 self, nlangcp__u16 other) { return *self >= *other; }

u32 u32_operator_plus__(nlangcp__u32 self, nlangcp__u32 other) { return *self + *other; }
u32 u32_operator_minus__(nlangcp__u32 self, nlangcp__u32 other) { return *self - *other; }
u32 u32_operator_times__(nlangcp__u32 self, nlangcp__u32 other) { return *self * *other; }
u32 u32_operator_divide__(nlangcp__u32 self, nlangcp__u32 other) { return *self / *other; }
u32 u32_operator_modulo__(nlangcp__u32 self, nlangcp__u32 other) { return *self % *other; }
u32 u32_operator_neg__(nlangcp__u32 self) { return - *self; }
u32 u32_operator_rshift__(nlangcp__u32 self, size n) { return *self >> n; }
u32 u32_operator_lshift__(nlangcp__u32 self, size n) { return *self << n; }
u32 u32_operator_bwand__(nlangcp__u32 self, nlangcp__u32 other) { return *self & *other; }
u32 u32_operator_bwor__(nlangcp__u32 self, nlangcp__u32 other) { return *self | *other; }
u32 u32_operator_bwxor__(nlangcp__u32 self, nlangcp__u32 other) { return *self ^ *other; }
u32 u32_operator_bwnot__(nlangcp__u32 self) { return ~ *self; }
bool u32_operator_eq__(nlangcp__u32 self, nlangcp__u32 other) { return *self == *other; }
bool u32_operator_ne__(nlangcp__u32 self, nlangcp__u32 other) { return *self != *other; }
bool u32_operator_lt__(nlangcp__u32 self, nlangcp__u32 other) { return *self < *other; }
bool u32_operator_gt__(nlangcp__u32 self, nlangcp__u32 other) { return *self > *other; }
bool u32_operator_le__(nlangcp__u32 self, nlangcp__u32 other) { return *self <= *other; }
bool u32_operator_ge__(nlangcp__u32 self, nlangcp__u32 other) { return *self >= *other; }

u64 u64_operator_plus__(nlangcp__u64 self, nlangcp__u64 other) { return *self + *other; }
u64 u64_operator_minus__(nlangcp__u64 self, nlangcp__u64 other) { return *self - *other; }
u64 u64_operator_times__(nlangcp__u64 self, nlangcp__u64 other) { return *self * *other; }
u64 u64_operator_divide__(nlangcp__u64 self, nlangcp__u64 other) { return *self / *other; }
u64 u64_operator_modulo__(nlangcp__u64 self, nlangcp__u64 other) { return *self % *other; }
u64 u64_operator_neg__(nlangcp__u64 self) { return - *self; }
u64 u64_operator_rshift__(nlangcp__u64 self, size n) { return *self >> n; }
u64 u64_operator_lshift__(nlangcp__u64 self, size n) { return *self << n; }
u64 u64_operator_bwand__(nlangcp__u64 self, nlangcp__u64 other) { return *self & *other; }
u64 u64_operator_bwor__(nlangcp__u64 self, nlangcp__u64 other) { return *self | *other; }
u64 u64_operator_bwxor__(nlangcp__u64 self, nlangcp__u64 other) { return *self ^ *other; }
u64 u64_operator_bwnot__(nlangcp__u64 self) { return ~ *self; }
bool u64_operator_eq__(nlangcp__u64 self, nlangcp__u64 other) { return *self == *other; }
bool u64_operator_ne__(nlangcp__u64 self, nlangcp__u64 other) { return *self != *other; }
bool u64_operator_lt__(nlangcp__u64 self, nlangcp__u64 other) { return *self < *other; }
bool u64_operator_gt__(nlangcp__u64 self, nlangcp__u64 other) { return *self > *other; }
bool u64_operator_le__(nlangcp__u64 self, nlangcp__u64 other) { return *self <= *other; }
bool u64_operator_ge__(nlangcp__u64 self, nlangcp__u64 other) { return *self >= *other; }

i8 i8_operator_plus__(nlangcp__i8 self, nlangcp__i8 other) { return *self + *other; }
i8 i8_operator_minus__(nlangcp__i8 self, nlangcp__i8 other) { return *self - *other; }
i8 i8_operator_times__(nlangcp__i8 self, nlangcp__i8 other) { return *self * *other; }
i8 i8_operator_divide__(nlangcp__i8 self, nlangcp__i8 other) { return *self / *other; }
i8 i8_operator_modulo__(nlangcp__i8 self, nlangcp__i8 other) { return *self % *other; }
i8 i8_operator_neg__(nlangcp__i8 self) { return - *self; }
i8 i8_operator_rshift__(nlangcp__i8 self, size n) { return *self >> n; }
i8 i8_operator_lshift__(nlangcp__i8 self, size n) { return *self << n; }
i8 i8_operator_bwand__(nlangcp__i8 self, nlangcp__i8 other) { return *self & *other; }
i8 i8_operator_bwor__(nlangcp__i8 self, nlangcp__i8 other) { return *self | *other; }
i8 i8_operator_bwxor__(nlangcp__i8 self, nlangcp__i8 other) { return *self ^ *other; }
i8 i8_operator_bwnot__(nlangcp__i8 self) { return ~ *self; }
bool i8_operator_eq__(nlangcp__i8 self, nlangcp__i8 other) { return *self == *other; }
bool i8_operator_ne__(nlangcp__i8 self, nlangcp__i8 other) { return *self != *other; }
bool i8_operator_lt__(nlangcp__i8 self, nlangcp__i8 other) { return *self < *other; }
bool i8_operator_gt__(nlangcp__i8 self, nlangcp__i8 other) { return *self > *other; }
bool i8_operator_le__(nlangcp__i8 self, nlangcp__i8 other) { return *self <= *other; }
bool i8_operator_ge__(nlangcp__i8 self, nlangcp__i8 other) { return *self >= *other; }

i16 i16_operator_plus__(nlangcp__i16 self, nlangcp__i16 other) { return *self + *other; }
i16 i16_operator_minus__(nlangcp__i16 self, nlangcp__i16 other) { return *self - *other; }
i16 i16_operator_times__(nlangcp__i16 self, nlangcp__i16 other) { return *self * *other; }
i16 i16_operator_divide__(nlangcp__i16 self, nlangcp__i16 other) { return *self / *other; }
i16 i16_operator_modulo__(nlangcp__i16 self, nlangcp__i16 other) { return *self % *other; }
i16 i16_operator_neg__(nlangcp__i16 self) { return - *self; }
i16 i16_operator_rshift__(nlangcp__i16 self, size n) { return *self >> n; }
i16 i16_operator_lshift__(nlangcp__i16 self, size n) { return *self << n; }
i16 i16_operator_bwand__(nlangcp__i16 self, nlangcp__i16 other) { return *self & *other; }
i16 i16_operator_bwor__(nlangcp__i16 self, nlangcp__i16 other) { return *self | *other; }
i16 i16_operator_bwxor__(nlangcp__i16 self, nlangcp__i16 other) { return *self ^ *other; }
i16 i16_operator_bwnot__(nlangcp__i16 self) { return ~ *self; }
bool i16_operator_eq__(nlangcp__i16 self, nlangcp__i16 other) { return *self == *other; }
bool i16_operator_ne__(nlangcp__i16 self, nlangcp__i16 other) { return *self != *other; }
bool i16_operator_lt__(nlangcp__i16 self, nlangcp__i16 other) { return *self < *other; }
bool i16_operator_gt__(nlangcp__i16 self, nlangcp__i16 other) { return *self > *other; }
bool i16_operator_le__(nlangcp__i16 self, nlangcp__i16 other) { return *self <= *other; }
bool i16_operator_ge__(nlangcp__i16 self, nlangcp__i16 other) { return *self >= *other; }

i32 i32_operator_plus__(nlangcp__i32 self, nlangcp__i32 other) { return *self + *other; }
i32 i32_operator_minus__(nlangcp__i32 self, nlangcp__i32 other) { return *self - *other; }
i32 i32_operator_times__(nlangcp__i32 self, nlangcp__i32 other) { return *self * *other; }
i32 i32_operator_divide__(nlangcp__i32 self, nlangcp__i32 other) { return *self / *other; }
i32 i32_operator_modulo__(nlangcp__i32 self, nlangcp__i32 other) { return *self % *other; }
i32 i32_operator_neg__(nlangcp__i32 self) { return - *self; }
i32 i32_operator_rshift__(nlangcp__i32 self, size n) { return *self >> n; }
i32 i32_operator_lshift__(nlangcp__i32 self, size n) { return *self << n; }
i32 i32_operator_bwand__(nlangcp__i32 self, nlangcp__i32 other) { return *self & *other; }
i32 i32_operator_bwor__(nlangcp__i32 self, nlangcp__i32 other) { return *self | *other; }
i32 i32_operator_bwxor__(nlangcp__i32 self, nlangcp__i32 other) { return *self ^ *other; }
i32 i32_operator_bwnot__(nlangcp__i32 self) { return ~ *self; }
bool i32_operator_eq__(nlangcp__i32 self, nlangcp__i32 other) { return *self == *other; }
bool i32_operator_ne__(nlangcp__i32 self, nlangcp__i32 other) { return *self != *other; }
bool i32_operator_lt__(nlangcp__i32 self, nlangcp__i32 other) { return *self < *other; }
bool i32_operator_gt__(nlangcp__i32 self, nlangcp__i32 other) { return *self > *other; }
bool i32_operator_le__(nlangcp__i32 self, nlangcp__i32 other) { return *self <= *other; }
bool i32_operator_ge__(nlangcp__i32 self, nlangcp__i32 other) { return *self >= *other; }

i64 i64_operator_plus__(nlangcp__i64 self, nlangcp__i64 other) { return *self + *other; }
i64 i64_operator_minus__(nlangcp__i64 self, nlangcp__i64 other) { return *self - *other; }
i64 i64_operator_times__(nlangcp__i64 self, nlangcp__i64 other) { return *self * *other; }
i64 i64_operator_divide__(nlangcp__i64 self, nlangcp__i64 other) { return *self / *other; }
i64 i64_operator_modulo__(nlangcp__i64 self, nlangcp__i64 other) { return *self % *other; }
i64 i64_operator_neg__(nlangcp__i64 self) { return - *self; }
i64 i64_operator_rshift__(nlangcp__i64 self, size n) { return *self >> n; }
i64 i64_operator_lshift__(nlangcp__i64 self, size n) { return *self << n; }
i64 i64_operator_bwand__(nlangcp__i64 self, nlangcp__i64 other) { return *self & *other; }
i64 i64_operator_bwor__(nlangcp__i64 self, nlangcp__i64 other) { return *self | *other; }
i64 i64_operator_bwxor__(nlangcp__i64 self, nlangcp__i64 other) { return *self ^ *other; }
i64 i64_operator_bwnot__(nlangcp__i64 self) { return ~ *self; }
bool i64_operator_eq__(nlangcp__i64 self, nlangcp__i64 other) { return *self == *other; }
bool i64_operator_ne__(nlangcp__i64 self, nlangcp__i64 other) { return *self != *other; }
bool i64_operator_lt__(nlangcp__i64 self, nlangcp__i64 other) { return *self < *other; }
bool i64_operator_gt__(nlangcp__i64 self, nlangcp__i64 other) { return *self > *other; }
bool i64_operator_le__(nlangcp__i64 self, nlangcp__i64 other) { return *self <= *other; }
bool i64_operator_ge__(nlangcp__i64 self, nlangcp__i64 other) { return *self >= *other; }

size size_operator_plus__(nlangcp__size self, nlangcp__size other) { return *self + *other; }
size size_operator_minus__(nlangcp__size self, nlangcp__size other) { return *self - *other; }
size size_operator_times__(nlangcp__size self, nlangcp__size other) { return *self * *other; }
size size_operator_divide__(nlangcp__size self, nlangcp__size other) { return *self / *other; }
size size_operator_modulo__(nlangcp__size self, nlangcp__size other) { return *self % *other; }
size size_operator_neg__(nlangcp__size self) { return - *self; }
size size_operator_rshift__(nlangcp__size self, size n) { return *self >> n; }
size size_operator_lshift__(nlangcp__size self, size n) { return *self << n; }
size size_operator_bwand__(nlangcp__size self, nlangcp__size other) { return *self & *other; }
size size_operator_bwor__(nlangcp__size self, nlangcp__size other) { return *self | *other; }
size size_operator_bwxor__(nlangcp__size self, nlangcp__size other) { return *self ^ *other; }
size size_operator_bwnot__(nlangcp__size self) { return ~ *self; }
bool size_operator_eq__(nlangcp__size self, nlangcp__size other) { return *self == *other; }
bool size_operator_ne__(nlangcp__size self, nlangcp__size other) { return *self != *other; }
bool size_operator_lt__(nlangcp__size self, nlangcp__size other) { return *self < *other; }
bool size_operator_gt__(nlangcp__size self, nlangcp__size other) { return *self > *other; }
bool size_operator_le__(nlangcp__size self, nlangcp__size other) { return *self <= *other; }
bool size_operator_ge__(nlangcp__size self, nlangcp__size other) { return *self >= *other; }

ssize ssize_operator_plus__(nlangcp__ssize self, nlangcp__ssize other) { return *self + *other; }
ssize ssize_operator_minus__(nlangcp__ssize self, nlangcp__ssize other) { return *self - *other; }
ssize ssize_operator_times__(nlangcp__ssize self, nlangcp__ssize other) { return *self * *other; }
ssize ssize_operator_divide__(nlangcp__ssize self, nlangcp__ssize other) { return *self / *other; }
ssize ssize_operator_modulo__(nlangcp__ssize self, nlangcp__ssize other) { return *self % *other; }
ssize ssize_operator_neg__(nlangcp__ssize self) { return - *self; }
ssize ssize_operator_rshift__(nlangcp__ssize self, size n) { return *self >> n; }
ssize ssize_operator_lshift__(nlangcp__ssize self, size n) { return *self << n; }
ssize ssize_operator_bwand__(nlangcp__ssize self, nlangcp__ssize other) { return *self & *other; }
ssize ssize_operator_bwor__(nlangcp__ssize self, nlangcp__ssize other) { return *self | *other; }
ssize ssize_operator_bwxor__(nlangcp__ssize self, nlangcp__ssize other) { return *self ^ *other; }
ssize ssize_operator_bwnot__(nlangcp__ssize self) { return ~ *self; }
bool ssize_operator_eq__(nlangcp__ssize self, nlangcp__ssize other) { return *self == *other; }
bool ssize_operator_ne__(nlangcp__ssize self, nlangcp__ssize other) { return *self != *other; }
bool ssize_operator_lt__(nlangcp__ssize self, nlangcp__ssize other) { return *self < *other; }
bool ssize_operator_gt__(nlangcp__ssize self, nlangcp__ssize other) { return *self > *other; }
bool ssize_operator_le__(nlangcp__ssize self, nlangcp__ssize other) { return *self <= *other; }
bool ssize_operator_ge__(nlangcp__ssize self, nlangcp__ssize other) { return *self >= *other; }

bool bool_operator_eq__(nlangcp__bool self, nlangcp__bool other) { return *self == *other; }
bool bool_operator_ne__(nlangcp__bool self, nlangcp__bool other) { return *self != *other; }
bool bool_operator_lt__(nlangcp__bool self, nlangcp__bool other) { return *self < *other; }
bool bool_operator_gt__(nlangcp__bool self, nlangcp__bool other) { return *self > *other; }
bool bool_operator_le__(nlangcp__bool self, nlangcp__bool other) { return *self <= *other; }
bool bool_operator_ge__(nlangcp__bool self, nlangcp__bool other) { return *self >= *other; }
bool bool_operator_and__(nlangcp__bool self, nlangcp__bool other) { return *self && *other; }
bool bool_operator_or__(nlangcp__bool self, nlangcp__bool other) { return *self || *other; }
bool bool_operator_not__(nlangcp__bool self) { return ! *self; }
