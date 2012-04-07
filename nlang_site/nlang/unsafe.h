#include <stdlib.h>
#include <string.h>

inline nlangp__u8 nlang_unsafe_realloc(nlangp__u8 data, size oldlen, size newlen) {
  nlangp__u8 p = realloc(data, newlen);
  if (newlen > oldlen) {
    memset(p + oldlen, 0, newlen - oldlen);
  }
  return p;
}

inline nlangp__u8 nlang_unsafe_malloc(size len) {
  return calloc(len, 1);
}

inline void nlang_unsafe_free(nlangp__u8 p, size len) {
  (void) len;
  return free(p);
}

inline void nlang_unsafe_memcpy(__restrict__ nlangp__u8 dst, __restrict__ nlangcp__u8 src, size elsize, size n) {
  memcpy(dst, src, n * elsize);
}
