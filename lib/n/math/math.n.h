#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(n) n$builtins$##n
#define GEN1(t, f) _$Ngen_n$math$##f##$$##t##_genN$_

#define proto(name, rt, t) static inline NB(rt) GEN1(n$builtins$##t, name)(NB(t) x)

#define likely(x) __builtin_expect(!!(x), 1)

proto(Bit_ffs, Uint, U8) { return __builtin_ffs(x); }
proto(Bit_ffs, Uint, U16) { return __builtin_ffs(x); }
proto(Bit_ffs, Uint, U32) { return __builtin_ffs(x); }
proto(Bit_ffs, Uint, U64) { return __builtin_ffsll(x); }

proto(Bit_clz, Uint, U8) { return likely(x != 0) ? __builtin_clz(x) - 24 : 8; }
proto(Bit_clz, Uint, U16) { return likely(x != 0) ? __builtin_clz(x) - 16 : 16; }
proto(Bit_clz, Uint, U32) { return likely(x != 0) ? __builtin_clz(x) : 32; }
proto(Bit_clz, Uint, U64) { return likely(x != 0) ? __builtin_clzll(x) : 64; }

proto(Bit_ctz, Uint, U8) { return likely(x != 0) ? __builtin_ctz(x) : 8; }
proto(Bit_ctz, Uint, U16) { return likely(x != 0) ? __builtin_ctz(x) : 16; }
proto(Bit_ctz, Uint, U32) { return likely(x != 0) ? __builtin_ctz(x) : 32; }
proto(Bit_ctz, Uint, U64) { return likely(x != 0) ? __builtin_ctzll(x) : 64; }

proto(Bit_fls, Uint, U8) { return likely(x != 0) ? 32 - __builtin_clz(x) : 0; }
proto(Bit_fls, Uint, U16) { return likely(x != 0) ? 32 - __builtin_clz(x) : 0; }
proto(Bit_fls, Uint, U32) { return likely(x != 0) ? 32 - __builtin_clz(x) : 0; }
proto(Bit_fls, Uint, U64) { return likely(x != 0) ? 64 - __builtin_clzll(x) : 0; }

// On x84_64, GCC's __builtin_popcount() is actually very slow. This is
// better.

static inline unsigned hweight32(uint32_t x) {
  uint32_t w = x - ((x >> 1) & 0x55555555);
  w = (w & 0x33333333) + ((w >> 2) & 0x33333333);
  w = (w + (w >> 4)) & 0x0F0F0F0F;
  w = w + (w >> 8);
  return (w + (w >> 16)) & 0x000000FF;
}

static inline unsigned hweight64(uint64_t x) {
  uint64_t w = x - ((x >> 1) & 0x5555555555555555ULL);
  w = (w & 0x3333333333333333ULL) + ((w >> 2) & 0x3333333333333333ULL);
  w = (w + (w >> 4)) & 0x0f0f0f0f0f0f0f0fULL;
  return (w * 0x0101010101010101ULL) >> 56;
}

proto(Bit_popcount, Uint, U8) { return hweight32(x); }
proto(Bit_popcount, Uint, U16) { return hweight32(x); }
proto(Bit_popcount, Uint, U32) { return hweight32(x); }
proto(Bit_popcount, Uint, U64) { return hweight64(x); }

proto(Bit_parity, Bool, U8) { return __builtin_parity(x); }
proto(Bit_parity, Bool, U16) { return __builtin_parity(x); }
proto(Bit_parity, Bool, U32) { return __builtin_parity(x); }
proto(Bit_parity, Bool, U64) { return __builtin_parityll(x); }

#undef likely
#undef proto
#undef GEN1
#undef NB

#endif
