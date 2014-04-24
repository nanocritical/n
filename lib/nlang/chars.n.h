#include <string.h>

#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(t) nlang$builtins$##t

static inline NB(i32) nlang$chars$memcmp(const NB(u8) *a, const NB(u8) *b, NB(size) count) {
  return memcmp(a, b, count);
}

#undef NB

#endif
