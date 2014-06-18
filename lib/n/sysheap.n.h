#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(x) n$builtins$##x

static inline NB(U8) *n$sysheap$Realloc(NB(U8) *ap, NB(Uint) oldbsz, NB(Uint) bsz) {
  NB(U8) *r;
  if (ap == NULL) {
    r = calloc(1, bsz);
    if (r == NULL) {
      NB(Abort)();
    }
  } else {
    r = realloc(ap, bsz);
    if (r == NULL) {
      NB(Abort)();
    }
    memset(r + oldbsz, 0, bsz - oldbsz);
  }
  return r;
}

static inline void n$sysheap$Free(NB(U8) *ap, NB(Uint) bsz) {
  free(ap);
}

#undef NB

#endif
