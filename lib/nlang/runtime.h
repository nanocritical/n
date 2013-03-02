#ifndef NLANG_RUNTIME_H__
#define NLANG_RUNTIME_H__

#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

typedef void nlang_builtins_void;
typedef _Bool nlang_builtins_bool;
typedef int8_t nlang_builtins_i8;
typedef int16_t nlang_builtins_i16;
typedef int32_t nlang_builtins_i32;
typedef int64_t nlang_builtins_i64;
typedef uint8_t nlang_builtins_u8;
typedef uint16_t nlang_builtins_u16;
typedef uint32_t nlang_builtins_u32;
typedef uint64_t nlang_builtins_u64;
typedef size_t nlang_builtins_size;
typedef ssize_t nlang_builtins_ssize;

#endif
