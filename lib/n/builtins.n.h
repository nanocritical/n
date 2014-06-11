#define NB(n) n$builtins$##n

#ifdef NLANG_DEFINE_TYPES
#include <stdarg.h>

struct NB(Valist) {
  va_list ap;
};

#define NLANG_BUILTINS_DEFINE_ENVPARENT(envt) envt _$Nenvparent_##envt

#endif

#ifdef NLANG_DEFINE_FUNCTIONS

static inline NB(U8) *NB(Slice_at_byte)(NB(U8) *p, NB(Size) off) {
  return p + off;
}

static inline NB(Void) NB(Slice_memcpy)(NB(U8) *dst, const NB(U8) *src, NB(Size) count) {
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

static inline NB(U8) NB(I8$Reinterpret_unsigned)(const NB(I8) *self) {
  return (NB(U8)) *self;
}

static inline NB(I8) NB(I8$Reinterpret_signed)(const NB(I8) *self) {
  return *self;
}

static inline NB(I16) NB(I8$To_i16)(const NB(I8) *self) {
  return (NB(I16)) *self;
}

static inline NB(I32) NB(I8$To_i32)(const NB(I8) *self) {
  return (NB(I32)) *self;
}

static inline NB(I64) NB(I8$To_i64)(const NB(I8) *self) {
  return (NB(I64)) *self;
}

static inline NB(Ssize) NB(I8$As_ssize)(const NB(I8) *self) {
  return (NB(Ssize)) *self;
}

static inline NB(Float) NB(I8$To_float)(const NB(I8) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(I8$To_double)(const NB(I8) *self) {
  return (NB(Double)) *self;
}


static inline NB(U16) NB(I16$Reinterpret_unsigned)(const NB(I16) *self) {
  return (NB(U16)) *self;
}

static inline NB(I16) NB(I16$Reinterpret_signed)(const NB(I16) *self) {
  return *self;
}

static inline NB(I8) NB(I16$Trim_i8)(const NB(I16) *self) {
  return (NB(I8)) *self;
}

static inline NB(I32) NB(I16$To_i32)(const NB(I16) *self) {
  return (NB(I32)) *self;
}

static inline NB(I64) NB(I16$To_i64)(const NB(I16) *self) {
  return (NB(I64)) *self;
}

static inline NB(Ssize) NB(I16$As_ssize)(const NB(I16) *self) {
  return (NB(Ssize)) *self;
}

static inline NB(Float) NB(I16$To_float)(const NB(I16) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(I16$To_double)(const NB(I16) *self) {
  return (NB(Double)) *self;
}


static inline NB(U32) NB(I32$Reinterpret_unsigned)(const NB(I32) *self) {
  return (NB(U32)) *self;
}

static inline NB(I32) NB(I32$Reinterpret_signed)(const NB(I32) *self) {
  return *self;
}

static inline NB(I8) NB(I32$Trim_i8)(const NB(I32) *self) {
  return (NB(I8)) *self;
}

static inline NB(I16) NB(I32$Trim_i16)(const NB(I32) *self) {
  return (NB(I16)) *self;
}

static inline NB(I64) NB(I32$To_i64)(const NB(I32) *self) {
  return (NB(I64)) *self;
}

static inline NB(Ssize) NB(I32$As_ssize)(const NB(I32) *self) {
  return (NB(Ssize)) *self;
}

static inline NB(Float) NB(I32$Trim_float)(const NB(I32) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(I32$To_double)(const NB(I32) *self) {
  return (NB(Double)) *self;
}


static inline NB(U64) NB(I64$Reinterpret_unsigned)(const NB(I64) *self) {
  return (NB(U64)) *self;
}

static inline NB(I64) NB(I64$Reinterpret_signed)(const NB(I64) *self) {
  return *self;
}

static inline NB(I8) NB(I64$Trim_i8)(const NB(I64) *self) {
  return (NB(I8)) *self;
}

static inline NB(I16) NB(I64$Trim_i16)(const NB(I64) *self) {
  return (NB(I16)) *self;
}

static inline NB(I32) NB(I64$Trim_i32)(const NB(I64) *self) {
  return (NB(I32)) *self;
}

static inline NB(Ssize) NB(I64$As_ssize)(const NB(I64) *self) {
  return (NB(Ssize)) *self;
}

static inline NB(Float) NB(I64$Trim_float)(const NB(I64) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(I64$Trim_double)(const NB(I64) *self) {
  return (NB(Double)) *self;
}


static inline NB(U8) NB(U8$Reinterpret_unsigned)(const NB(U8) *self) {
  return *self;
}

static inline NB(I8) NB(U8$Reinterpret_signed)(const NB(U8) *self) {
  return (NB(I8)) *self;
}

static inline NB(U16) NB(U8$To_u16)(const NB(U8) *self) {
  return (NB(U16)) *self;
}

static inline NB(U32) NB(U8$To_u32)(const NB(U8) *self) {
  return (NB(U32)) *self;
}

static inline NB(U64) NB(U8$To_u64)(const NB(U8) *self) {
  return (NB(U64)) *self;
}

static inline NB(Size) NB(U8$As_size)(const NB(U8) *self) {
  return (NB(Size)) *self;
}

static inline NB(Float) NB(U8$To_float)(const NB(U8) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(U8$To_double)(const NB(U8) *self) {
  return (NB(Double)) *self;
}


static inline NB(U16) NB(U16$Reinterpret_unsigned)(const NB(U16) *self) {
  return *self;
}

static inline NB(I16) NB(U16$Reinterpret_signed)(const NB(U16) *self) {
  return (NB(I16)) *self;
}

static inline NB(U8) NB(U16$Trim_u8)(const NB(U16) *self) {
  return (NB(U8)) *self;
}

static inline NB(U32) NB(U16$To_u32)(const NB(U16) *self) {
  return (NB(U32)) *self;
}

static inline NB(U64) NB(U16$To_u64)(const NB(U16) *self) {
  return (NB(U64)) *self;
}

static inline NB(Size) NB(U16$As_size)(const NB(U16) *self) {
  return (NB(Size)) *self;
}

static inline NB(Float) NB(U16$To_float)(const NB(U16) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(U16$To_double)(const NB(U16) *self) {
  return (NB(Double)) *self;
}


static inline NB(U32) NB(U32$Reinterpret_unsigned)(const NB(U32) *self) {
  return *self;
}

static inline NB(I32) NB(U32$Reinterpret_signed)(const NB(U32) *self) {
  return (NB(I32)) *self;
}

static inline NB(U8) NB(U32$Trim_u8)(const NB(U32) *self) {
  return (NB(U8)) *self;
}

static inline NB(U16) NB(U32$Trim_u16)(const NB(U32) *self) {
  return (NB(U16)) *self;
}

static inline NB(U64) NB(U32$To_u64)(const NB(U32) *self) {
  return (NB(U64)) *self;
}

static inline NB(Size) NB(U32$As_size)(const NB(U32) *self) {
  return (NB(Size)) *self;
}

static inline NB(Float) NB(U32$Trim_float)(const NB(U32) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(U32$To_double)(const NB(U32) *self) {
  return (NB(Double)) *self;
}


static inline NB(U64) NB(U64$Reinterpret_unsigned)(const NB(U64) *self) {
  return *self;
}

static inline NB(I64) NB(U64$Reinterpret_signed)(const NB(U64) *self) {
  return (NB(I64)) *self;
}

static inline NB(U8) NB(U64$Trim_u8)(const NB(U64) *self) {
  return (NB(U8)) *self;
}

static inline NB(U16) NB(U64$Trim_u16)(const NB(U64) *self) {
  return (NB(U16)) *self;
}

static inline NB(U32) NB(U64$Trim_u32)(const NB(U64) *self) {
  return (NB(U32)) *self;
}

static inline NB(Size) NB(U64$As_size)(const NB(U64) *self) {
  return (NB(Size)) *self;
}

static inline NB(Float) NB(U64$Trim_float)(const NB(U64) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(U64$Trim_double)(const NB(U64) *self) {
  return (NB(Double)) *self;
}


static inline NB(Ssize) NB(Ssize$Reinterpret_signed)(const NB(Ssize) *self) {
  return *self;
}

static inline NB(Size) NB(Ssize$Reinterpret_unsigned)(const NB(Ssize) *self) {
  return (NB(Size)) *self;
}

static inline NB(I8) NB(Ssize$Trim_i8)(const NB(Ssize) *self) {
  return (NB(I8)) *self;
}

static inline NB(I16) NB(Ssize$Trim_i16)(const NB(Ssize) *self) {
  return (NB(I16)) *self;
}

static inline NB(I32) NB(Ssize$Trim_i32)(const NB(Ssize) *self) {
  return (NB(I32)) *self;
}

static inline NB(I64) NB(Ssize$Trim_i64)(const NB(Ssize) *self) {
  return (NB(I64)) *self;
}

static inline NB(Float) NB(Ssize$Trim_float)(const NB(Ssize) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(Ssize$Trim_double)(const NB(Ssize) *self) {
  return (NB(Double)) *self;
}

static inline NB(Size) NB(Size$Reinterpret_unsigned)(const NB(Size) *self) {
  return *self;
}

static inline NB(Ssize) NB(Size$Reinterpret_signed)(const NB(Size) *self) {
  return (NB(Ssize)) *self;
}

static inline NB(U8) NB(Size$Trim_u8)(const NB(Size) *self) {
  return (NB(U8)) *self;
}

static inline NB(U16) NB(Size$Trim_u16)(const NB(Size) *self) {
  return (NB(U16)) *self;
}

static inline NB(U32) NB(Size$Trim_u32)(const NB(Size) *self) {
  return (NB(U32)) *self;
}

static inline NB(U64) NB(Size$Trim_u64)(const NB(Size) *self) {
  return (NB(U64)) *self;
}

static inline NB(Float) NB(Size$Trim_float)(const NB(Size) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(Size$Trim_double)(const NB(Size) *self) {
  return (NB(Double)) *self;
}


static inline NB(I8) NB(Float$Trim_i8)(const NB(Float) *self) {
  return (NB(I8)) *self;
}

static inline NB(I16) NB(Float$Trim_i16)(const NB(Float) *self) {
  return (NB(I16)) *self;
}

static inline NB(I32) NB(Float$To_i32)(const NB(Float) *self) {
  return (NB(I32)) *self;
}

static inline NB(I64) NB(Float$To_i64)(const NB(Float) *self) {
  return (NB(I64)) *self;
}

static inline NB(Double) NB(Float$To_double)(const NB(Float) *self) {
  return (NB(Double)) *self;
}

static inline NB(Ssize) NB(Float$As_ssize)(const NB(Float) *self) {
  return (NB(Ssize)) *self;
}


static inline NB(I8) NB(Double$Trim_i8)(const NB(Double) *self) {
  return (NB(I8)) *self;
}

static inline NB(I16) NB(Double$Trim_i16)(const NB(Double) *self) {
  return (NB(I16)) *self;
}

static inline NB(I32) NB(Double$Trim_i32)(const NB(Double) *self) {
  return (NB(I32)) *self;
}

static inline NB(I64) NB(Double$To_i64)(const NB(Double) *self) {
  return (NB(I64)) *self;
}

static inline NB(Float) NB(Double$Trim_float)(const NB(Double) *self) {
  return (NB(Float)) *self;
}

static inline NB(Ssize) NB(Double$As_ssize)(const NB(Double) *self) {
  return (NB(Ssize)) *self;
}

#define n$builtins$likely(x) __builtin_expect(!!(x), 1)
#define n$builtins$unlikely(x) __builtin_expect(!!(x), 0)

static inline NB(U8) *NB(Static_array_at_byte)(NB(U8) *p, NB(Size) off) {
  return p + off;
}

#endif

#undef NB
