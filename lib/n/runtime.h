#ifndef NLANG_RUNTIME_H__
#define NLANG_RUNTIME_H__

#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

typedef void n$builtins$Void;
typedef _Bool n$builtins$Bool;
typedef int8_t n$builtins$I8;
typedef int16_t n$builtins$I16;
typedef int32_t n$builtins$I32;
typedef int64_t n$builtins$I64;
typedef uint8_t n$builtins$U8;
typedef uint16_t n$builtins$U16;
typedef uint32_t n$builtins$U32;
typedef uint64_t n$builtins$U64;
typedef uintptr_t n$builtins$Uintptr;
typedef intptr_t n$builtins$Intptr;
typedef size_t n$builtins$Uint;
typedef ssize_t n$builtins$Int;
typedef float n$builtins$Float;
typedef double n$builtins$Double;

#include <lib/n/reflect.h>

#endif
