#ifndef RUNTIME_PRELUDE_H_
#define RUNTIME_PRELUDE_H_

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

typedef uint8_t* nlangp__u8;
typedef const uint8_t* nlangcp__u8;

#define null NULL

#define NLANG_UNREACHED() { abort(); }

#endif // RUNTIME_PRELUDE_H_
