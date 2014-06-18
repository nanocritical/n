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

static inline NB(U8) NB(I8$As_unsigned)(const NB(I8) *self) {
  return (NB(U8)) *self;
}

static inline NB(I8) NB(I8$As_signed)(const NB(I8) *self) {
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

static inline NB(Int) NB(I8$As_int)(const NB(I8) *self) {
  return (NB(Int)) *self;
}

static inline NB(Float) NB(I8$To_float)(const NB(I8) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(I8$To_double)(const NB(I8) *self) {
  return (NB(Double)) *self;
}


static inline NB(U16) NB(I16$As_unsigned)(const NB(I16) *self) {
  return (NB(U16)) *self;
}

static inline NB(I16) NB(I16$As_signed)(const NB(I16) *self) {
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

static inline NB(Int) NB(I16$As_int)(const NB(I16) *self) {
  return (NB(Int)) *self;
}

static inline NB(Float) NB(I16$To_float)(const NB(I16) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(I16$To_double)(const NB(I16) *self) {
  return (NB(Double)) *self;
}


static inline NB(U32) NB(I32$As_unsigned)(const NB(I32) *self) {
  return (NB(U32)) *self;
}

static inline NB(I32) NB(I32$As_signed)(const NB(I32) *self) {
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

static inline NB(Int) NB(I32$As_int)(const NB(I32) *self) {
  return (NB(Int)) *self;
}

static inline NB(Float) NB(I32$Round_float)(const NB(I32) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(I32$To_double)(const NB(I32) *self) {
  return (NB(Double)) *self;
}


static inline NB(U64) NB(I64$As_unsigned)(const NB(I64) *self) {
  return (NB(U64)) *self;
}

static inline NB(I64) NB(I64$As_signed)(const NB(I64) *self) {
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

static inline NB(Int) NB(I64$As_int)(const NB(I64) *self) {
  return (NB(Int)) *self;
}

static inline NB(Float) NB(I64$Round_float)(const NB(I64) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(I64$Round_double)(const NB(I64) *self) {
  return (NB(Double)) *self;
}


static inline NB(U8) NB(U8$As_unsigned)(const NB(U8) *self) {
  return *self;
}

static inline NB(I8) NB(U8$As_signed)(const NB(U8) *self) {
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

static inline NB(Uint) NB(U8$As_uint)(const NB(U8) *self) {
  return (NB(Uint)) *self;
}

static inline NB(Float) NB(U8$To_float)(const NB(U8) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(U8$To_double)(const NB(U8) *self) {
  return (NB(Double)) *self;
}


static inline NB(U16) NB(U16$As_unsigned)(const NB(U16) *self) {
  return *self;
}

static inline NB(I16) NB(U16$As_signed)(const NB(U16) *self) {
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

static inline NB(Uint) NB(U16$As_uint)(const NB(U16) *self) {
  return (NB(Uint)) *self;
}

static inline NB(Float) NB(U16$To_float)(const NB(U16) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(U16$To_double)(const NB(U16) *self) {
  return (NB(Double)) *self;
}


static inline NB(U32) NB(U32$As_unsigned)(const NB(U32) *self) {
  return *self;
}

static inline NB(I32) NB(U32$As_signed)(const NB(U32) *self) {
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

static inline NB(Uint) NB(U32$As_uint)(const NB(U32) *self) {
  return (NB(Uint)) *self;
}

static inline NB(Float) NB(U32$Round_float)(const NB(U32) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(U32$To_double)(const NB(U32) *self) {
  return (NB(Double)) *self;
}


static inline NB(U64) NB(U64$As_unsigned)(const NB(U64) *self) {
  return *self;
}

static inline NB(I64) NB(U64$As_signed)(const NB(U64) *self) {
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

static inline NB(Uint) NB(U64$As_uint)(const NB(U64) *self) {
  return (NB(Uint)) *self;
}

static inline NB(Float) NB(U64$Round_float)(const NB(U64) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(U64$Round_double)(const NB(U64) *self) {
  return (NB(Double)) *self;
}


static inline NB(Int) NB(Int$As_signed)(const NB(Int) *self) {
  return *self;
}

static inline NB(Uint) NB(Int$As_unsigned)(const NB(Int) *self) {
  return (NB(Uint)) *self;
}

static inline NB(I8) NB(Int$Trim_i8)(const NB(Int) *self) {
  return (NB(I8)) *self;
}

static inline NB(I16) NB(Int$Trim_i16)(const NB(Int) *self) {
  return (NB(I16)) *self;
}

static inline NB(I32) NB(Int$Trim_i32)(const NB(Int) *self) {
  return (NB(I32)) *self;
}

static inline NB(I64) NB(Int$Trim_i64)(const NB(Int) *self) {
  return (NB(I64)) *self;
}

static inline NB(Float) NB(Int$Round_float)(const NB(Int) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(Int$Round_double)(const NB(Int) *self) {
  return (NB(Double)) *self;
}

static inline NB(Uint) NB(Uint$As_unsigned)(const NB(Uint) *self) {
  return *self;
}

static inline NB(Int) NB(Uint$As_signed)(const NB(Uint) *self) {
  return (NB(Int)) *self;
}

static inline NB(U8) NB(Uint$Trim_u8)(const NB(Uint) *self) {
  return (NB(U8)) *self;
}

static inline NB(U16) NB(Uint$Trim_u16)(const NB(Uint) *self) {
  return (NB(U16)) *self;
}

static inline NB(U32) NB(Uint$Trim_u32)(const NB(Uint) *self) {
  return (NB(U32)) *self;
}

static inline NB(U64) NB(Uint$Trim_u64)(const NB(Uint) *self) {
  return (NB(U64)) *self;
}

static inline NB(Float) NB(Uint$Round_float)(const NB(Uint) *self) {
  return (NB(Float)) *self;
}

static inline NB(Double) NB(Uint$Round_double)(const NB(Uint) *self) {
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

static inline NB(Int) NB(Float$As_int)(const NB(Float) *self) {
  return (NB(Int)) *self;
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

static inline NB(Float) NB(Double$Round_float)(const NB(Double) *self) {
  return (NB(Float)) *self;
}

static inline NB(Int) NB(Double$As_int)(const NB(Double) *self) {
  return (NB(Int)) *self;
}

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
