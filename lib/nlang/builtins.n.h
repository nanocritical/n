#define NB(n) nlang$builtins$##n

#ifdef NLANG_DEFINE_TYPES
#include <stdarg.h>

struct NB(valist) {
  va_list ap;
};

#define NLANG_BUILTINS_DEFINE_ENVPARENT(envt) envt _$Nenvparent_##envt

#endif

#ifdef NLANG_DEFINE_FUNCTIONS

static inline NB(u8) *NB(slice_at_byte)(NB(u8) *p, NB(size) off) {
  return p + off;
}

static inline NB(void) NB(slice_memcpy)(NB(u8) *dst, const NB(u8) *src, NB(size) count) {
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
  ({ nlang$builtins$assert((va).n > 0, NULL); \
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

static inline NB(u8) NB(i8$reinterpret_unsigned)(const NB(i8) *self) {
  return (NB(u8)) *self;
}

static inline NB(i8) NB(i8$reinterpret_signed)(const NB(i8) *self) {
  return *self;
}

static inline NB(i16) NB(i8$to_i16)(const NB(i8) *self) {
  return (NB(i16)) *self;
}

static inline NB(i32) NB(i8$to_i32)(const NB(i8) *self) {
  return (NB(i32)) *self;
}

static inline NB(i64) NB(i8$to_i64)(const NB(i8) *self) {
  return (NB(i64)) *self;
}

static inline NB(ssize) NB(i8$as_ssize)(const NB(i8) *self) {
  return (NB(ssize)) *self;
}

static inline NB(float) NB(i8$to_float)(const NB(i8) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(i8$to_double)(const NB(i8) *self) {
  return (NB(double)) *self;
}


static inline NB(u16) NB(i16$reinterpret_unsigned)(const NB(i16) *self) {
  return (NB(u16)) *self;
}

static inline NB(i16) NB(i16$reinterpret_signed)(const NB(i16) *self) {
  return *self;
}

static inline NB(i8) NB(i16$trim_i8)(const NB(i16) *self) {
  return (NB(i8)) *self;
}

static inline NB(i32) NB(i16$to_i32)(const NB(i16) *self) {
  return (NB(i32)) *self;
}

static inline NB(i64) NB(i16$to_i64)(const NB(i16) *self) {
  return (NB(i64)) *self;
}

static inline NB(ssize) NB(i16$as_ssize)(const NB(i16) *self) {
  return (NB(ssize)) *self;
}

static inline NB(float) NB(i16$to_float)(const NB(i16) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(i16$to_double)(const NB(i16) *self) {
  return (NB(double)) *self;
}


static inline NB(u32) NB(i32$reinterpret_unsigned)(const NB(i32) *self) {
  return (NB(u32)) *self;
}

static inline NB(i32) NB(i32$reinterpret_signed)(const NB(i32) *self) {
  return *self;
}

static inline NB(i8) NB(i32$trim_i8)(const NB(i32) *self) {
  return (NB(i8)) *self;
}

static inline NB(i16) NB(i32$trim_i16)(const NB(i32) *self) {
  return (NB(i16)) *self;
}

static inline NB(i64) NB(i32$to_i64)(const NB(i32) *self) {
  return (NB(i64)) *self;
}

static inline NB(ssize) NB(i32$as_ssize)(const NB(i32) *self) {
  return (NB(ssize)) *self;
}

static inline NB(float) NB(i32$trim_float)(const NB(i32) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(i32$to_double)(const NB(i32) *self) {
  return (NB(double)) *self;
}


static inline NB(u64) NB(i64$reinterpret_unsigned)(const NB(i64) *self) {
  return (NB(u64)) *self;
}

static inline NB(i64) NB(i64$reinterpret_signed)(const NB(i64) *self) {
  return *self;
}

static inline NB(i8) NB(i64$trim_i8)(const NB(i64) *self) {
  return (NB(i8)) *self;
}

static inline NB(i16) NB(i64$trim_i16)(const NB(i64) *self) {
  return (NB(i16)) *self;
}

static inline NB(i32) NB(i64$trim_i32)(const NB(i64) *self) {
  return (NB(i32)) *self;
}

static inline NB(ssize) NB(i64$as_ssize)(const NB(i64) *self) {
  return (NB(ssize)) *self;
}

static inline NB(float) NB(i64$trim_float)(const NB(i64) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(i64$trim_double)(const NB(i64) *self) {
  return (NB(double)) *self;
}


static inline NB(u8) NB(u8$reinterpret_unsigned)(const NB(u8) *self) {
  return *self;
}

static inline NB(i8) NB(u8$reinterpret_signed)(const NB(u8) *self) {
  return (NB(i8)) *self;
}

static inline NB(u16) NB(u8$to_u16)(const NB(u8) *self) {
  return (NB(u16)) *self;
}

static inline NB(u32) NB(u8$to_u32)(const NB(u8) *self) {
  return (NB(u32)) *self;
}

static inline NB(u64) NB(u8$to_u64)(const NB(u8) *self) {
  return (NB(u64)) *self;
}

static inline NB(size) NB(u8$as_size)(const NB(u8) *self) {
  return (NB(size)) *self;
}

static inline NB(float) NB(u8$to_float)(const NB(u8) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(u8$to_double)(const NB(u8) *self) {
  return (NB(double)) *self;
}


static inline NB(u16) NB(u16$reinterpret_unsigned)(const NB(u16) *self) {
  return *self;
}

static inline NB(i16) NB(u16$reinterpret_signed)(const NB(u16) *self) {
  return (NB(i16)) *self;
}

static inline NB(u8) NB(u16$trim_u8)(const NB(u16) *self) {
  return (NB(u8)) *self;
}

static inline NB(u32) NB(u16$to_u32)(const NB(u16) *self) {
  return (NB(u32)) *self;
}

static inline NB(u64) NB(u16$to_u64)(const NB(u16) *self) {
  return (NB(u64)) *self;
}

static inline NB(size) NB(u16$as_size)(const NB(u16) *self) {
  return (NB(size)) *self;
}

static inline NB(float) NB(u16$to_float)(const NB(u16) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(u16$to_double)(const NB(u16) *self) {
  return (NB(double)) *self;
}


static inline NB(u32) NB(u32$reinterpret_unsigned)(const NB(u32) *self) {
  return *self;
}

static inline NB(i32) NB(u32$reinterpret_signed)(const NB(u32) *self) {
  return (NB(i32)) *self;
}

static inline NB(u8) NB(u32$trim_u8)(const NB(u32) *self) {
  return (NB(u8)) *self;
}

static inline NB(u16) NB(u32$trim_u16)(const NB(u32) *self) {
  return (NB(u16)) *self;
}

static inline NB(u64) NB(u32$to_u64)(const NB(u32) *self) {
  return (NB(u64)) *self;
}

static inline NB(size) NB(u32$as_size)(const NB(u32) *self) {
  return (NB(size)) *self;
}

static inline NB(float) NB(u32$trim_float)(const NB(u32) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(u32$to_double)(const NB(u32) *self) {
  return (NB(double)) *self;
}


static inline NB(u64) NB(u64$reinterpret_unsigned)(const NB(u64) *self) {
  return *self;
}

static inline NB(i64) NB(u64$reinterpret_signed)(const NB(u64) *self) {
  return (NB(i64)) *self;
}

static inline NB(u8) NB(u64$trim_u8)(const NB(u64) *self) {
  return (NB(u8)) *self;
}

static inline NB(u16) NB(u64$trim_u16)(const NB(u64) *self) {
  return (NB(u16)) *self;
}

static inline NB(u32) NB(u64$trim_u32)(const NB(u64) *self) {
  return (NB(u32)) *self;
}

static inline NB(size) NB(u64$as_size)(const NB(u64) *self) {
  return (NB(size)) *self;
}

static inline NB(float) NB(u64$trim_float)(const NB(u64) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(u64$trim_double)(const NB(u64) *self) {
  return (NB(double)) *self;
}


static inline NB(ssize) NB(ssize$reinterpret_signed)(const NB(ssize) *self) {
  return *self;
}

static inline NB(size) NB(ssize$reinterpret_unsigned)(const NB(ssize) *self) {
  return (NB(size)) *self;
}

static inline NB(i8) NB(ssize$trim_i8)(const NB(ssize) *self) {
  return (NB(i8)) *self;
}

static inline NB(i16) NB(ssize$trim_i16)(const NB(ssize) *self) {
  return (NB(i16)) *self;
}

static inline NB(i32) NB(ssize$trim_i32)(const NB(ssize) *self) {
  return (NB(i32)) *self;
}

static inline NB(i64) NB(ssize$trim_i64)(const NB(ssize) *self) {
  return (NB(i64)) *self;
}

static inline NB(float) NB(ssize$trim_float)(const NB(ssize) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(ssize$trim_double)(const NB(ssize) *self) {
  return (NB(double)) *self;
}

static inline NB(size) NB(size$reinterpret_unsigned)(const NB(size) *self) {
  return *self;
}

static inline NB(ssize) NB(size$reinterpret_signed)(const NB(size) *self) {
  return (NB(ssize)) *self;
}

static inline NB(u8) NB(size$trim_u8)(const NB(size) *self) {
  return (NB(u8)) *self;
}

static inline NB(u16) NB(size$trim_u16)(const NB(size) *self) {
  return (NB(u16)) *self;
}

static inline NB(u32) NB(size$trim_u32)(const NB(size) *self) {
  return (NB(u32)) *self;
}

static inline NB(u64) NB(size$trim_u64)(const NB(size) *self) {
  return (NB(u64)) *self;
}

static inline NB(float) NB(size$trim_float)(const NB(size) *self) {
  return (NB(float)) *self;
}

static inline NB(double) NB(size$trim_double)(const NB(size) *self) {
  return (NB(double)) *self;
}


static inline NB(i8) NB(float$trim_i8)(const NB(float) *self) {
  return (NB(i8)) *self;
}

static inline NB(i16) NB(float$trim_i16)(const NB(float) *self) {
  return (NB(i16)) *self;
}

static inline NB(i32) NB(float$to_i32)(const NB(float) *self) {
  return (NB(i32)) *self;
}

static inline NB(i64) NB(float$to_i64)(const NB(float) *self) {
  return (NB(i64)) *self;
}

static inline NB(double) NB(float$to_double)(const NB(float) *self) {
  return (NB(double)) *self;
}

static inline NB(ssize) NB(float$as_ssize)(const NB(float) *self) {
  return (NB(ssize)) *self;
}


static inline NB(i8) NB(double$trim_i8)(const NB(double) *self) {
  return (NB(i8)) *self;
}

static inline NB(i16) NB(double$trim_i16)(const NB(double) *self) {
  return (NB(i16)) *self;
}

static inline NB(i32) NB(double$trim_i32)(const NB(double) *self) {
  return (NB(i32)) *self;
}

static inline NB(i64) NB(double$to_i64)(const NB(double) *self) {
  return (NB(i64)) *self;
}

static inline NB(float) NB(double$trim_float)(const NB(double) *self) {
  return (NB(float)) *self;
}

static inline NB(ssize) NB(double$as_ssize)(const NB(double) *self) {
  return (NB(ssize)) *self;
}

#define nlang$builtins$likely(x) __builtin_expect(!!(x), 1)
#define nlang$builtins$unlikely(x) __builtin_expect(!!(x), 0)

static inline NB(u8) *NB(static_array_at_byte)(NB(u8) *p, NB(size) off) {
  return p + off;
}

#endif

#undef NB
