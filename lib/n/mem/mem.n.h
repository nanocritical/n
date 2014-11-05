#include <stdlib.h>

#ifdef NLANG_DEFINE_FUNCTIONS

n$builtins$U8 *n$mem$internal_realloc0(n$builtins$U8 *ap,
                                       n$builtins$Uint old_bsz,
                                       n$builtins$Uint bsz);

static inline n$builtins$U8 *n$mem$Internal_realloc0(n$builtins$U8 *ap,
                                                     n$builtins$Uint old_bsz,
                                                     n$builtins$Uint bsz) {
  if (old_bsz == bsz) {
    return ap;
  }

  return n$mem$internal_realloc0(ap, old_bsz, bsz);
}

static inline void n$mem$Internal_free(n$builtins$U8 *ap, n$builtins$Uint bsz) {
  free(ap);
}

#endif
