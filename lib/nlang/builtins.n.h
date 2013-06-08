#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(n) nlang_builtins_##n

static inline NB(u8) NB(i8_reinterpret_unsigned)(const NB(i8) *self) {
  return (NB(u8)) *self;
}

static inline NB(i8) NB(i8_reinterpret_signed)(const NB(i8) *self) {
  return *self;
}

static inline NB(i16) NB(i8_to_i16)(const NB(i8) *self) {
  return (NB(i16)) *self;
}

static inline NB(i32) NB(i8_to_i32)(const NB(i8) *self) {
  return (NB(i32)) *self;
}

static inline NB(i64) NB(i8_to_i64)(const NB(i8) *self) {
  return (NB(i64)) *self;
}

static inline NB(ssize) NB(i8_as_ssize)(const NB(i8) *self) {
  return (NB(ssize)) *self;
}

static inline NB(float) NB(i8_to_float)(const NB(i8) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(i8_to_double)(const NB(i8) *self) {
  return (NB(double)) *self;
}


static inline NB(u16) NB(i16_reinterpret_unsigned)(const NB(i16) *self) {
  return (NB(u16)) *self;
}

static inline NB(i16) NB(i16_reinterpret_signed)(const NB(i16) *self) {
  return *self;
}

static inline NB(i8) NB(i16_trim_i8)(const NB(i16) *self) {
  return (NB(i8)) *self;
}

static inline NB(i32) NB(i16_to_i32)(const NB(i16) *self) {
  return (NB(i32)) *self;
}

static inline NB(i64) NB(i16_to_i64)(const NB(i16) *self) {
  return (NB(i64)) *self;
}

static inline NB(ssize) NB(i16_as_ssize)(const NB(i16) *self) {
  return (NB(ssize)) *self;
}

static inline NB(float) NB(i16_to_float)(const NB(i16) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(i16_to_double)(const NB(i16) *self) {
  return (NB(double)) *self;
}


static inline NB(u32) NB(i32_reinterpret_unsigned)(const NB(i32) *self) {
  return (NB(u32)) *self;
}

static inline NB(i32) NB(i32_reinterpret_signed)(const NB(i32) *self) {
  return *self;
}

static inline NB(i8) NB(i32_trim_i8)(const NB(i32) *self) {
  return (NB(i8)) *self;
}

static inline NB(i16) NB(i32_trim_i16)(const NB(i32) *self) {
  return (NB(i16)) *self;
}

static inline NB(i64) NB(i32_to_i64)(const NB(i32) *self) {
  return (NB(i64)) *self;
}

static inline NB(ssize) NB(i32_as_ssize)(const NB(i32) *self) {
  return (NB(ssize)) *self;
}

static inline NB(float) NB(i32_trim_float)(const NB(i32) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(i32_to_double)(const NB(i32) *self) {
  return (NB(double)) *self;
}


static inline NB(u64) NB(i64_reinterpret_unsigned)(const NB(i64) *self) {
  return (NB(u64)) *self;
}

static inline NB(i64) NB(i64_reinterpret_signed)(const NB(i64) *self) {
  return *self;
}

static inline NB(i8) NB(i64_trim_i8)(const NB(i64) *self) {
  return (NB(i8)) *self;
}

static inline NB(i16) NB(i64_trim_i16)(const NB(i64) *self) {
  return (NB(i16)) *self;
}

static inline NB(i32) NB(i64_trim_i32)(const NB(i64) *self) {
  return (NB(i32)) *self;
}

static inline NB(ssize) NB(i64_as_ssize)(const NB(i64) *self) {
  return (NB(ssize)) *self;
}

static inline NB(float) NB(i64_trim_float)(const NB(i64) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(i64_trim_double)(const NB(i64) *self) {
  return (NB(double)) *self;
}


static inline NB(u8) NB(u8_reinterpret_unsigned)(const NB(u8) *self) {
  return *self;
}

static inline NB(i8) NB(u8_reinterpret_signed)(const NB(u8) *self) {
  return (NB(i8)) *self;
}

static inline NB(u16) NB(u8_to_u16)(const NB(u8) *self) {
  return (NB(u16)) *self;
}

static inline NB(u32) NB(u8_to_u32)(const NB(u8) *self) {
  return (NB(u32)) *self;
}

static inline NB(u64) NB(u8_to_u64)(const NB(u8) *self) {
  return (NB(u64)) *self;
}

static inline NB(size) NB(u8_as_size)(const NB(u8) *self) {
  return (NB(size)) *self;
}

static inline NB(float) NB(u8_to_float)(const NB(u8) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(u8_to_double)(const NB(u8) *self) {
  return (NB(double)) *self;
}


static inline NB(u16) NB(u16_reinterpret_unsigned)(const NB(u16) *self) {
  return *self;
}

static inline NB(i16) NB(u16_reinterpret_signed)(const NB(u16) *self) {
  return (NB(i16)) *self;
}

static inline NB(u8) NB(u16_trim_u8)(const NB(u16) *self) {
  return (NB(u8)) *self;
}

static inline NB(u32) NB(u16_to_u32)(const NB(u16) *self) {
  return (NB(u32)) *self;
}

static inline NB(u64) NB(u16_to_u64)(const NB(u16) *self) {
  return (NB(u64)) *self;
}

static inline NB(size) NB(u16_as_size)(const NB(u16) *self) {
  return (NB(size)) *self;
}

static inline NB(float) NB(u16_to_float)(const NB(u16) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(u16_to_double)(const NB(u16) *self) {
  return (NB(double)) *self;
}


static inline NB(u32) NB(u32_reinterpret_unsigned)(const NB(u32) *self) {
  return *self;
}

static inline NB(i32) NB(u32_reinterpret_signed)(const NB(u32) *self) {
  return (NB(i32)) *self;
}

static inline NB(u8) NB(u32_trim_u8)(const NB(u32) *self) {
  return (NB(u8)) *self;
}

static inline NB(u16) NB(u32_trim_u16)(const NB(u32) *self) {
  return (NB(u16)) *self;
}

static inline NB(u64) NB(u32_to_u64)(const NB(u32) *self) {
  return (NB(u64)) *self;
}

static inline NB(size) NB(u32_as_size)(const NB(u32) *self) {
  return (NB(size)) *self;
}

static inline NB(float) NB(u32_trim_float)(const NB(u32) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(u32_to_double)(const NB(u32) *self) {
  return (NB(double)) *self;
}


static inline NB(u64) NB(u64_reinterpret_unsigned)(const NB(u64) *self) {
  return *self;
}

static inline NB(i64) NB(u64_reinterpret_signed)(const NB(u64) *self) {
  return (NB(i64)) *self;
}

static inline NB(u8) NB(u64_trim_u8)(const NB(u64) *self) {
  return (NB(u8)) *self;
}

static inline NB(u16) NB(u64_trim_u16)(const NB(u64) *self) {
  return (NB(u16)) *self;
}

static inline NB(u32) NB(u64_trim_u32)(const NB(u64) *self) {
  return (NB(u32)) *self;
}

static inline NB(size) NB(u64_as_size)(const NB(u64) *self) {
  return (NB(size)) *self;
}

static inline NB(float) NB(u64_trim_float)(const NB(u64) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(u64_trim_double)(const NB(u64) *self) {
  return (NB(double)) *self;
}


static inline NB(ssize) NB(ssize_reinterpret_signed)(const NB(ssize) *self) {
  return *self;
}

static inline NB(size) NB(ssize_reinterpret_unsigned)(const NB(ssize) *self) {
  return (NB(size)) *self;
}

static inline NB(i8) NB(ssize_trim_i8)(const NB(ssize) *self) {
  return (NB(i8)) *self;
}

static inline NB(i16) NB(ssize_trim_i16)(const NB(ssize) *self) {
  return (NB(i16)) *self;
}

static inline NB(i32) NB(ssize_trim_i32)(const NB(ssize) *self) {
  return (NB(i32)) *self;
}

static inline NB(i64) NB(ssize_trim_i64)(const NB(ssize) *self) {
  return (NB(i64)) *self;
}

static inline NB(float) NB(ssize_trim_float)(const NB(ssize) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(ssize_trim_double)(const NB(ssize) *self) {
  return (NB(double)) *self;
}

static inline NB(size) NB(size_reinterpret_unsigned)(const NB(size) *self) {
  return *self;
}

static inline NB(ssize) NB(size_reinterpret_signed)(const NB(size) *self) {
  return (NB(ssize)) *self;
}

static inline NB(u8) NB(size_trim_u8)(const NB(size) *self) {
  return (NB(u8)) *self;
}

static inline NB(u16) NB(size_trim_u16)(const NB(size) *self) {
  return (NB(u16)) *self;
}

static inline NB(u32) NB(size_trim_u32)(const NB(size) *self) {
  return (NB(u32)) *self;
}

static inline NB(u64) NB(size_trim_u64)(const NB(size) *self) {
  return (NB(u64)) *self;
}

static inline NB(float) NB(size_trim_float)(const NB(size) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(size_trim_double)(const NB(size) *self) {
  return (NB(double)) *self;
}


static inline NB(i8) NB(float_trim_i8)(const NB(float) *self) {
  return (NB(i8)) *self;
}

static inline NB(i16) NB(float_trim_i16)(const NB(float) *self) {
  return (NB(i16)) *self;
}

static inline NB(i32) NB(float_to_i32)(const NB(float) *self) {
  return (NB(i32)) *self;
}

static inline NB(i64) NB(float_to_i64)(const NB(float) *self) {
  return (NB(i64)) *self;
}

static inline NB(double) NB(float_to_double)(const NB(float) *self) {
  return (NB(double)) *self;
}

static inline NB(ssize) NB(float_as_ssize)(const NB(float) *self) {
  return (NB(ssize)) *self;
}


static inline NB(i8) NB(double_trim_i8)(const NB(double) *self) {
  return (NB(i8)) *self;
}

static inline NB(i16) NB(double_trim_i16)(const NB(double) *self) {
  return (NB(i16)) *self;
}

static inline NB(i32) NB(double_trim_i32)(const NB(double) *self) {
  return (NB(i32)) *self;
}

static inline NB(i64) NB(double_to_i64)(const NB(double) *self) {
  return (NB(i64)) *self;
}

static inline NB(float) NB(double_trim_float)(const NB(double) *self) {
  return (NB(float)) *self;
}

static inline NB(ssize) NB(double_as_ssize)(const NB(double) *self) {
  return (NB(ssize)) *self;
}

#define nlang_builtins_likely(x) __builtin_expect(!!(x), 1)
#define nlang_builtins_unlikely(x) __builtin_expect(!!(x), 0)

static inline NB(u8) *NB(static_array_at_byte)(NB(u8) *p, NB(size) off) {
  return p + off;
}

#undef NB

#endif
