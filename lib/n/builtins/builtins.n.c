#include <inttypes.h>
#include <stdio.h>
#include <ctype.h>

#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(n) n$builtins$##n
#define NBDYN(t) _$Ndyn_n$builtins$_$Ni_##t

#define heap_header _$Ngen_n$builtins$Envheader$$n$builtins$_$Ni_Heap$$_$Ndyn_n$builtins$_$Ni_Heap_genN$_

static struct heap_header sysheap_header;
extern const struct _$Ndyntable_n$builtins$_$Ni_Heap n$mem$Sysheap$Dyntable__n$builtins$_$Ni_Heap;

extern void n$env$Install_sys(NB(Uint) argc, NB(U8) **argv);
extern void n$time$Install_sys(void);
extern void n$fs$Install_sys(void);
extern void n$crypto$rand$Install_sys(void);

void _$Nprelude(int *argc, char ***argv, char ***env) {
  sysheap_header.Env = NLANG_MKDYN(struct _$Ndyn_n$builtins$_$Ni_Heap,
                                   &n$mem$Sysheap$Dyntable__n$builtins$_$Ni_Heap,
                                   (void *)&n$builtins$sysheap);
  sysheap_header.Parent = NULL;
  n$builtins$Install_sysheap(&sysheap_header);

  n$env$Install_sys(*argc, (NB(U8) **) *argv);
  n$time$Install_sys();
  n$fs$Install_sys();
  n$crypto$rand$Install_sys();
}

void _$Npostlude(int *ret) {
}

NB(Void) *NB(Nonnull_void)(void) {
  static NB(U32) dummy;
  return &dummy;
}

#define strto(t, nptr) \
  __builtin_choose_expr( \
    __builtin_types_compatible_p(t *, int8_t *), \
    strtol(nptr, NULL, 0), \
  __builtin_choose_expr( \
    __builtin_types_compatible_p(t *, int16_t *), \
    strtol(nptr, NULL, 0), \
  __builtin_choose_expr( \
    __builtin_types_compatible_p(t *, int *), \
    strtol(nptr, NULL, 0), \
  __builtin_choose_expr( \
    __builtin_types_compatible_p(t *, long int *), \
    strtol(nptr, NULL, 0), \
  __builtin_choose_expr( \
    __builtin_types_compatible_p(t *, long long int *), \
    strtoll(nptr, NULL, 0), \
  __builtin_choose_expr( \
    __builtin_types_compatible_p(t *, uint8_t *), \
    strtol(nptr, NULL, 0), \
  __builtin_choose_expr( \
    __builtin_types_compatible_p(t *, uint16_t *), \
    strtol(nptr, NULL, 0), \
  __builtin_choose_expr( \
    __builtin_types_compatible_p(t *, unsigned int *), \
    strtol(nptr, NULL, 0), \
  __builtin_choose_expr( \
    __builtin_types_compatible_p(t *, unsigned long int *), \
    strtoul(nptr, NULL, 0), \
  __builtin_choose_expr( \
    __builtin_types_compatible_p(t *, unsigned long long int *), \
    strtoull(nptr, NULL, 0), \
  __builtin_choose_expr( \
    __builtin_types_compatible_p(t *, float *), \
    strtof(nptr, NULL), \
  __builtin_choose_expr( \
    __builtin_types_compatible_p(t *, double *), \
    strtod(nptr, NULL), \
  __builtin_choose_expr( \
    __builtin_types_compatible_p(t *, long double *), \
    strtold(nptr, NULL), \
    0)))))))))))))

#define define_from_number_literal(t) \
  t t##$From_number_literal(struct NB(String) v) { \
    return strto(t, (char *) v.bytes.dat); \
  }

define_from_number_literal(n$builtins$I8)
define_from_number_literal(n$builtins$I16)
define_from_number_literal(n$builtins$I32)
define_from_number_literal(n$builtins$I64)
define_from_number_literal(n$builtins$U8)
define_from_number_literal(n$builtins$U16)
define_from_number_literal(n$builtins$U32)
define_from_number_literal(n$builtins$U64)
define_from_number_literal(n$builtins$Uint)
define_from_number_literal(n$builtins$Int)
define_from_number_literal(n$builtins$Uintptr)
define_from_number_literal(n$builtins$Intptr)
define_from_number_literal(n$builtins$Float)
define_from_number_literal(n$builtins$Double)


static void native_write_buffer(struct _$Ndyn_n$fmt$_$Ni_State st, char *s, int cnt) {
  const struct _$Ngen_n$builtins$Slice_impl$$n$builtins$U8_genN$_ bytes =
    NLANG_BYTE_SLICE(s, cnt);
  st.dyntable->Write(st.obj, bytes);
}

void n$builtins$Bool$Show(NB(Bool) *self, struct _$Ndyn_n$fmt$_$Ni_State st) {
  native_write_buffer(st, *self ? "true" : "false", *self ? 4 : 5);
}

// ln(2^64)/ln(10) = 19.27
#define define_show_number(t, fmt) \
  void t##$Show(t *self, struct _$Ndyn_n$fmt$_$Ni_State st) { \
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
define_show_number(n$builtins$Uintptr, "%"PRIxPTR)
define_show_number(n$builtins$Intptr, "%"PRIxPTR)
define_show_number(n$builtins$Float, "%f")
define_show_number(n$builtins$Double, "%f")

#undef define_show_number
#undef define_native_floating
#undef define_native_boolean
#undef define_native_integer

void NB(abort)(void) {
  abort();
}

extern void *memchr(const void *s, int c, size_t n);
extern void *memrchr(const void *s, int c, size_t n);

NB(Int) NB(String$Index_byte)(struct NB(String) *self, NB(U8) sep) {
  void *r = memchr(self->bytes.dat, sep, self->bytes.cnt);
  if (r == NULL) {
    return -1;
  } else {
    return (uintptr_t)r - (uintptr_t)self->bytes.dat;
  }
}

NB(Int) NB(String$Last_index_byte)(struct NB(String) *self, NB(U8) sep) {
  void *r = memrchr(self->bytes.dat, sep, self->bytes.cnt);
  if (r == NULL) {
    return -1;
  } else {
    return (uintptr_t)r - (uintptr_t)self->bytes.dat;
  }
}

extern void *memmem(const void *, size_t, const void *, size_t);

NB(Int) NB(String$memmem_index)(struct NB(String) *self, struct NB(String) sep) {
  void *r = memmem(self->bytes.dat, self->bytes.cnt, sep.bytes.dat, sep.bytes.cnt);
  if (r == NULL) {
    return -1;
  }
  return (uintptr_t)r - (uintptr_t)self->bytes.dat;
}

NB(U8) NB(Byte_to_lower)(NB(U8) c) {
  return tolower(c);
}

NB(U8) NB(Byte_to_upper)(NB(U8) c) {
  return toupper(c);
}

/*
 * On platforms that support it well, use unaligned access.
 * (The test needs improvements.)
 */
#if (defined(__GNUC__) && defined(__i386__)) || defined(__WATCOMC__) \
|| defined(_MSC_VER) || defined (__BORLANDC__) || defined (__TURBOC__)
#define get16bits(d) (*((const uint16_t *) (d)))
#endif

#if !defined (get16bits)
#define get16bits(d) ((((uint32_t)(((const uint8_t *)(d))[1])) << 8)\
                      +(uint32_t)(((const uint8_t *)(d))[0]) )
#endif

uint32_t n$builtins$hash32_hsieh(NB(U8) *vdata, size_t ulen) {
  const char *data = (const char *)vdata;
  ssize_t len = ulen;
  uint32_t hash = len, tmp;
  int rem;

  if (!data)
    return 0;

  rem = len & 3;
  len >>= 2;

  /* Main loop */
  for (; len > 0; len--) {
    hash += get16bits (data);
    tmp = (get16bits (data+2) << 11) ^ hash;
    hash = (hash << 16) ^ tmp;
    data += 2*sizeof(uint16_t);
    hash += hash >> 11;
  }

  /* Handle end cases */
  switch (rem) {
  case 3:
    hash += get16bits(data);
    hash ^= hash << 16;
    hash ^= data[sizeof(uint16_t)] << 18;
    hash += hash >> 11;
    break;
  case 2:
    hash += get16bits(data);
    hash ^= hash << 11;
    hash += hash >> 17;
    break;
  case 1:
    hash += *data;
    hash ^= hash << 10;
    hash += hash >> 1;
  }

  /* Force "avalanching" of final 127 bits */
  hash ^= hash << 3;
  hash += hash >> 5;
  hash ^= hash << 4;
  hash += hash >> 17;
  hash ^= hash << 25;
  hash += hash >> 6;

  return hash;
}

#undef get16bits

#undef NBDYN
#undef NB

#endif
