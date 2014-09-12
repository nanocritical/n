#include <unistd.h>

#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(x) n$builtins$##x

static void n$write_buf(NB(I32) fd, NB(U8) *s, NB(Uint) count) {
  write(fd, s, count);
}

#undef NB

#endif
