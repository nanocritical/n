#include <unistd.h>

#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(x) nlang$builtins$##x

static void nlang$write_buf(NB(I32) fd, const NB(U8) *s, NB(Size) count) {
  write(fd, s, count);
}

#undef NB

#endif
