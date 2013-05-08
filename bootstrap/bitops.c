// Copyright 2011, Nanocritical Corp. and the nntools Project contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Bit-level operations, and bitmaps.

#include "bitops.h"

#define implement_bitmap_bitwiseop(name, op) \
    void bitmap_##name(uint32_t *c, \
                       const uint32_t *a, const uint32_t *b, \
                       unsigned nr) { \
      unsigned i, n = BITMAP_WORD(nr); \
      /* Can do the full last word, trailing bits remain zero. */ \
      for (i = 0; i <= n; ++i) \
      c[i] = a[i] op b[i]; \
    }

#define implement_bitmap_bitwiseop_unary(name, op) \
    void bitmap_##name(uint32_t *c, const uint32_t *a, \
                       unsigned nr) { \
      unsigned i, n = BITMAP_WORD(nr); \
      for (i = 0; i <= n; ++i) \
      c[i] = op a[i]; \
      /* Ensure trailing bits are still zero. */ \
      c[i] &= ~BITMAP_MASK_LAST(nr); \
    }

implement_bitmap_bitwiseop(or, |);
implement_bitmap_bitwiseop(xor, ^);
implement_bitmap_bitwiseop(and, &);
implement_bitmap_bitwiseop_unary(not, ~);

unsigned bitmap_hweight(const uint32_t *bitmap, unsigned nr) {
  unsigned i, n = nr / BITS_PER_WORD, w = 0;

  for (i = 0; i < n; ++i)
    w += bit_popcount(bitmap[i]);

  if (nr % BITS_PER_WORD != 0)
    w += bit_popcount(bitmap[i] & BITMAP_MASK_LAST(nr));

  return w;
}

unsigned bitmap_ffs(const uint32_t *bitmap, unsigned from, unsigned size) {
  unsigned index_last = BITMAP_WORD(size - 1);
  unsigned from_word = BITMAP_WORD(from);

  if (from_word == index_last) {
    uint32_t mask = SLICE(~0u, (size - 1) % BITS_PER_WORD, from % BITS_PER_WORD)
        << (from % BITS_PER_WORD);
    uint32_t word = bitmap[from_word] & mask;
    if (word != 0)
      return from_word * BITS_PER_WORD + bit_ffs(word) - 1;
    else
      return size;
  }

  if (bitmap[from_word] != 0) {
    uint32_t first_mask = SLICE(~0u, BITS_PER_WORD - 1, from % BITS_PER_WORD)
        << (from % BITS_PER_WORD);
    uint32_t first_word = bitmap[from_word] & first_mask;
    if (first_word != 0)
      return from_word * BITS_PER_WORD + bit_ffs(first_word) - 1;
  }

  for (unsigned w = from_word + 1; w < index_last; ++w) {
    if (bitmap[w] != 0)
      return w * BITS_PER_WORD + bit_ffs(bitmap[w]) - 1;
  }

  if (index_last != from_word) {
    uint32_t last = bitmap[index_last] & BITMAP_MASK_LAST(size);
    if (last != 0)
      return index_last * BITS_PER_WORD + bit_ffs(last) - 1;
  }

  return size;
}

// Find the last non-zero word, last bit set in word has index
// [bit_fls(word)].
unsigned bitmap_fls(const uint32_t *bitmap, unsigned from, unsigned size) {
  unsigned from_word = BITMAP_WORD(from);
  unsigned index_last = BITMAP_WORD(size - 1);
  uint32_t last = bitmap[index_last] & BITMAP_MASK_LAST(size);

  if (index_last == from_word) {
    uint32_t mask = SLICE(~0u, (size - 1) % BITS_PER_WORD, from % BITS_PER_WORD)
        << (from % BITS_PER_WORD);
    last = last & mask;
  }

  if (last != 0)
    return index_last * BITS_PER_WORD + bit_fls(last);

  for (int w = index_last - 1; w >= (int)from_word; --w) {
    if (bitmap[w] != 0)
      return w * BITS_PER_WORD + bit_fls(bitmap[w]);
  }

  if (index_last != from_word && bitmap[from_word] != 0) {
    uint32_t first_mask = SLICE(~0u, BITS_PER_WORD - 1, from % BITS_PER_WORD)
        << (from % BITS_PER_WORD);
    uint32_t first_word = bitmap[from_word] & first_mask;
    if (first_word != 0)
      return from_word * BITS_PER_WORD + bit_fls(first_word);
  }

  return size;
}
