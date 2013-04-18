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

static inline NB(u8) *nlang_memory_slice_dataptr(NB(u8) *ap) {
  return ap + sizeof(NB(size));
}

static inline NB(u8) *nlang_memory_slice_allocptr(NB(u8) *p) {
  if (p == NULL) {
    return NULL;
  } else {
    return p - sizeof(NB(size));
  }
}

static inline NB(size) nlang_memory_slice_elcount(const NB(u8) *p) {
  if (p == NULL) {
    return 0;
  } else {
    return *(NB(size) *) nlang_memory_slice_allocptr((NB(u8) *) p);
  }
}

static inline NB(void) nlang_memory_slice_set_elcount(NB(u8) *ap, size_t elcount) {
  *((NB(size) *)ap) = elcount;
}

static inline NB(u8) *nlang_memory_slice_at_byte(NB(u8) *p, NB(size) off) {
  return p + off;
}

#undef NB
