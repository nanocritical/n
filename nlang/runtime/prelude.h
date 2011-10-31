#ifndef RUNTIME_PRELUDE_H_
#define RUNTIME_PRELUDE_H_

#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>

#define NLANG_RUNTIME_ERROR() abort()

#define NLANG_RUNTIME_ERROR_ON(cond) do { \
  if (cond) { \
    NLANG_RUNTIME_ERROR(); \
  } \
} while (0)

typedef void Void;
typedef uint8_t U8;
typedef uint16_t U16;
typedef uint32_t U32;
typedef uint64_t U64;
typedef int8_t I8;
typedef int16_t I16;
typedef int32_t I32;
typedef int64_t I64;
typedef char Char;
typedef size_t Size;
typedef ssize_t SSize;

typedef void* nlangp__Void;
typedef uint8_t* nlangp__U8;
typedef uint16_t* nlangp__U16;
typedef uint32_t* nlangp__U32;
typedef uint64_t* nlangp__U64;
typedef int8_t* nlangp__I8;
typedef int16_t* nlangp__I16;
typedef int32_t* nlangp__I32;
typedef int64_t* nlangp__I64;
typedef char* nlangp__Char;
typedef size_t* nlangp__Size;
typedef ssize_t* nlangp__SSize;

typedef const void* nlangcp__Void;
typedef const uint8_t* nlangcp__U8;
typedef const uint16_t* nlangcp__U16;
typedef const uint32_t* nlangcp__U32;
typedef const uint64_t* nlangcp__U64;
typedef const int8_t* nlangcp__I8;
typedef const int16_t* nlangcp__I16;
typedef const int32_t* nlangcp__I32;
typedef const int64_t* nlangcp__I64;
typedef const char* nlangcp__Char;
typedef const size_t* nlangcp__Size;
typedef const ssize_t* nlangcp__SSize;

#define null NULL

#endif // RUNTIME_PRELUDE_H_
