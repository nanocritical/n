#ifndef RUNTIME_PRELUDE_H_
#define RUNTIME_PRELUDE_H_

#ifndef __USE_XOPEN2K8
# define __USE_XOPEN2K8
#endif
#ifndef __USE_GNU
# define __USE_GNU
#endif
#ifndef _BSD_SOURCE
# define _BSD_SOURCE
#endif
#ifndef _XOPEN_SOURCE
# define _XOPEN_SOURCE 500
#endif

#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

#define NLANG_RUNTIME_ERROR() abort()

#define NLANG_RUNTIME_ERROR_ON(cond) do { \
  if (cond) { \
    NLANG_RUNTIME_ERROR(); \
  } \
} while (0)

#define NLANG_POINTER_ALIASES(t) \
  typedef t* nlangp__##t; \
  typedef const t* nlangcp__##t

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef size_t size;
typedef ssize_t ssize;
typedef _Bool bool;

NLANG_POINTER_ALIASES(u8);
NLANG_POINTER_ALIASES(u16);
NLANG_POINTER_ALIASES(u32);
NLANG_POINTER_ALIASES(u64);
NLANG_POINTER_ALIASES(i8);
NLANG_POINTER_ALIASES(i16);
NLANG_POINTER_ALIASES(i32);
NLANG_POINTER_ALIASES(i64);
NLANG_POINTER_ALIASES(size);
NLANG_POINTER_ALIASES(ssize);
NLANG_POINTER_ALIASES(bool);

#define null NULL

#define NLANG_UNREACHED() { abort(); }

#endif // RUNTIME_PRELUDE_H_
