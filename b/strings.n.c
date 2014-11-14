#include <string.h>

#define NB(t) n$builtins$##t

#ifdef NLANG_DEFINE_FUNCTIONS

static NB(Int) b$strings$c_index_memmem(NB(U8) *data, NB(Uint) len, NB(U8) *pattern, NB(Uint) pattern_len) {
  void *r = memmem(data, len, pattern, pattern_len);
  if (r == NULL) {
    return -1;
  }
  return (uintptr_t)r - (uintptr_t)data;
}

#endif

#undef NB
