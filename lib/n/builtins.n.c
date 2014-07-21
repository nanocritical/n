#include <inttypes.h>
#include <stdio.h>

#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(n) n$builtins$##n

const NB(Void) *NB(Nonnull_void)(void) {
  static NB(U32) dummy;
  return &dummy;
}

static void native_write_buffer(_$Ndyn_n$chars$_$Ni_State st, const char *s, int cnt) {
  n$fmt_$Ni_State$Write(st, NLANG_BYTE_SLICE(s, cnt));
}

void n$builtins$Bool$Show(const NB(Bool) *self, _$Ndyn_n$fmt$_$Ni_State st) { \
  native_write_buffer(st, *self ? "true" : "false"); \
}

// ln(2^64)/ln(10) = 19.27
#define define_show_number(t, fmt) \
  void t##$Show(const t *self, _$Ndyn_n$fmt$_$Ni_State st) { \
    char s[32]; \
    const int cnt = snprintf(s, 32, fmt, *self); \
    native_write_buffer(st, s, cnt); \
  }

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
