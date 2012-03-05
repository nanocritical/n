#include <nlang/runtime/prelude.h>

n_Slice n_Slice_alloc(size_t sz) {
  n_Slice s;
  s.data = malloc(sz);
  s.off = 0;
  s.size = sz;
  return s;
}

void n_Slice_realloc(n_Slice *s, size_t sz) {
#ifdef NLANG_BOOTSTRAP
  U8 *n = malloc(sz);
  memcpy(n, s.data, sz < s.size ? sz : s.size);
  s.data = n;
#else
  s.data = realloc(s.data, sz);
  s.size = sz;
#endif
}

void n_Slice_free(n_Slice *s) {
#ifndef NLANG_BOOTSTRAP
  free(s.data);
  memset(s, 0, sizeof(*s));
#endif
}
