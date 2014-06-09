#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(x) nlang$builtins$##x

static inline NB(U8) *nlang$sysheap$Realloc(NB(U8) *ap, NB(Size) oldbsz, NB(Size) bsz) {
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

static inline void nlang$sysheap$Free(NB(U8) *ap, NB(Size) bsz) {
  free(ap);
}

#undef NB

#endif
