#define NB(x) nlang_builtins_##x

static inline NB(u8) *nlang_memory_realloc(NB(u8) *ap, NB(size) oldbsz, NB(size) bsz) {
  NB(u8) *r;
  if (ap == NULL) {
    r = calloc(1, bsz);
    if (r == NULL) {
      NB(abort)();
    }
  } else {
    r = realloc(ap, bsz);
    if (r == NULL) {
      NB(abort)();
    }
    memset(r + oldbsz, 0, bsz - oldbsz);
  }
  return r;
}

static inline void nlang_memory_free(NB(u8) *ap, NB(size) bsz) {
  free(ap);
}

static inline NB(u8) *nlang_memory_slice_at_byte(NB(u8) *p, NB(size) off) {
  return p + off;
}

static inline NB(void) nlang_memory_slice_memcpy(NB(u8) *dst, NB(size) at,
                                                 const NB(u8) *src, NB(size) off, NB(size) count) {
  memcpy(dst + at, src + off, count);
}

#undef NB
