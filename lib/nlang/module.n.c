#include <unistd.h>

#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(x) nlang_builtins_##x

static void nlang_write_buf(NB(i32) fd, const NB(u8) *s, NB(size) count) {
  write(fd, s, count);
}

#undef NB

#endif
