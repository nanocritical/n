#include <inttypes.h>
#include <stdio.h>

#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(x) n$builtins$##x

NB(U8) *n$mem$_realloc0(NB(U8) *ap, NB(Uint) old_bsz, NB(Uint) bsz) {
  if (old_bsz == bsz) {
    return ap;
  }

  if (bsz == 0) {
    free(ap);
    return NULL;
  }

  NB(U8) *r;
  if (ap == NULL) {
    // Use calloc(3) in the hope that on occasion it is able to obtain
    // memory already zeroed.
    r = calloc(bsz, 1);
    if (r == NULL) {
      NB(Abort)();
    }
  } else {
    r = realloc(ap, bsz);
    if (r == NULL) {
      NB(Abort)();
    }
    if (bsz > old_bsz) {
      memset(r + old_bsz, 0, bsz - old_bsz);
    }
  }
  return r;
}

#undef NB

#endif
