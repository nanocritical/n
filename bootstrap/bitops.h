#ifndef BITOPS_H__
#define BITOPS_H__

#include <stdint.h>
#include <string.h>
#include "common.h"


/*
 * The following bitmap functions work best when nr is known at compile
 * time. On most architectures, they can have a better implementation in
 * asm.
 *
 * asm inline implementations were tested on x86_64, following the
 * approach of the Linux kernel (arch/x86/include/asm/bitops.h). The
 * implementation below is actually quite a bit faster than using the
 * special purpose Intel instructions (btc, bts, ...). The difference
 * can be up to 30%. The tests were standalone, in a tight loop. icache
 * effects or register scheduling could make a difference on more large
 * scale tests.
 */
#define BITS_PER_WORD 32U

#define BITMAP_MASK(nr) ( 1U << ((nr) % BITS_PER_WORD) )
#define BITMAP_WORD(nr) ( (nr) / (BITS_PER_WORD) )
#define BITMAP_SIZE(nr) ( div_ceil(__typeof__(nr), nr, BITS_PER_WORD) * sizeof(uint32_t) )
#define BITMAP_MASK_LAST(nr) \
    ({ int _mask_lm = (nr) % BITS_PER_WORD; \
     _mask_lm ? (1U << _mask_lm) - 1 : ~0; })

static inline int bitmap_test(const uint32_t *bitmap, unsigned nr) {
  unsigned i = BITMAP_WORD(nr);
  const uint32_t mask = BITMAP_MASK(nr);

  return (bitmap[i] & mask) != 0;
}

static inline void bitmap_set(uint32_t *bitmap, unsigned nr) {
  unsigned i = BITMAP_WORD(nr);
  uint32_t mask = BITMAP_MASK(nr);

  bitmap[i] |= mask;
}

static inline void bitmap_toggle(uint32_t *bitmap, unsigned nr) {
  unsigned i = BITMAP_WORD(nr);
  uint32_t mask = BITMAP_MASK(nr);

  bitmap[i] ^= mask;
}

static inline void bitmap_clear(uint32_t *bitmap, unsigned nr) {
  unsigned i = BITMAP_WORD(nr);
  uint32_t mask = BITMAP_MASK(nr);

  bitmap[i] &= ~mask;
}

void bitmap_or(uint32_t *c, const uint32_t *a, const uint32_t *b,
               unsigned len);
void bitmap_xor(uint32_t *c, const uint32_t *a, const uint32_t *b,
                unsigned len);
void bitmap_and(uint32_t *c, const uint32_t *a, const uint32_t *b,
                unsigned len);
void bitmap_not(uint32_t *c, const uint32_t *a,
                unsigned len);

/*
 * lo - left inclusive bound
 * hi - right inclusive bound
 */
static inline void bitmap_set_region(uint32_t *b, unsigned lo, unsigned hi) {
  const uint32_t mask = ~0U;
  int wlo = BITMAP_WORD(lo), whi = BITMAP_WORD(hi), n;
  uint32_t r, m;

  if ((r = lo % BITS_PER_WORD)) {
    m = SLICE(mask, min(unsigned, BITS_PER_WORD, hi - lo), r) << r;
    b[0] |= m;
    wlo += 1;
  }

  for (n = wlo; n <= whi-1; ++n)
    b[n] |= mask;

  if ((r = hi % BITS_PER_WORD)) {
    unsigned low_bound = hi - lo > r ? 0 : hi - lo;
    m = SLICE(mask, r, low_bound) << low_bound;
    b[whi] |= m;
  }
}

static inline void bitmap_clear_region(uint32_t *b, unsigned lo, unsigned hi) {
  const uint32_t mask = 0U;
  unsigned wlo = BITMAP_WORD(lo), whi = BITMAP_WORD(hi), n;
  uint32_t r, m;

  if ((r = lo % BITS_PER_WORD)) {
    m = SLICE(mask, min(unsigned, BITS_PER_WORD, hi-lo), lo) << lo;
    b[0] &= m;
    wlo += 1;
  }

  for (n = wlo; n <= whi-1; ++n)
    b[n] &= mask;

  if ((r = hi % BITS_PER_WORD)) {
    unsigned low_bound = hi - lo > r ? 0 : hi - lo;
    m = SLICE(mask, r, low_bound) << low_bound;
    b[whi] &= m;
  }
}

/*
 * Count the number of bits set between position first and last-1
 * in the bitmap.
 */
unsigned bitmap_hweight(const uint32_t *bitmap, unsigned len);

// Return [size] if none are set
unsigned bitmap_ffs(const uint32_t *bitmap, unsigned from, unsigned size);
// Return [size] if none are set
unsigned bitmap_fls(const uint32_t *bitmap, unsigned from, unsigned size);

#endif
