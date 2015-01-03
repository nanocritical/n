#include <stdlib.h>

#ifdef NLANG_DEFINE_FUNCTIONS

n$builtins$U8 *n$mem$_realloc0(n$builtins$U8 *ap,
                                       n$builtins$Uint old_bsz,
                                       n$builtins$Uint bsz);

static inline n$builtins$U8 *n$mem$realloc0(n$builtins$U8 *ap,
                                            n$builtins$Uint old_bsz,
                                            n$builtins$Uint bsz) {
  if (old_bsz == bsz) {
    return ap;
  }

  return n$mem$_realloc0(ap, old_bsz, bsz);
}

static inline void n$mem$free(n$builtins$U8 *ap, n$builtins$Uint bsz) {
  free(ap);
}

#endif
