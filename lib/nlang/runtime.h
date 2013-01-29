#ifndef NLANG_RUNTIME_H__
#define NLANG_RUNTIME_H__

#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>

typedef void nlang_literals_void;
typedef _Bool nlang_integers_bool;
typedef int8_t nlang_integers_i8;
typedef int16_t nlang_integers_i16;
typedef int32_t nlang_integers_i32;
typedef int64_t nlang_integers_i64;
typedef uint8_t nlang_integers_u8;
typedef uint16_t nlang_integers_u16;
typedef uint32_t nlang_integers_u32;
typedef uint64_t nlang_integers_u64;
typedef size_t nlang_integers_size;
typedef ssize_t nlang_integers_ssize;

#endif
