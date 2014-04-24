#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(n) nlang$builtins$##n
#define GEN1(t, f) _Ngen_nlang$math$##f##__##t##_genN_

#define proto(name, rt, t) NB(rt) GEN1(NB(t), name)(NB(t) x)

proto(bit_ffs, u8, u8) { return __builtin_ffs(x); }
proto(bit_ffs, u16, u16) { return __builtin_ffs(x); }
proto(bit_ffs, u32, u32) { return __builtin_ffs(x); }
proto(bit_ffs, u64, u64) { return __builtin_ffsll(x); }

proto(bit_clz, u8, u8) { return x != 0 ? __builtin_clz(x) - 24 : 8; }
proto(bit_clz, u16, u16) { return x != 0 ? __builtin_clz(x) - 16 : 16; }
proto(bit_clz, u32, u32) { return x != 0 ? __builtin_clz(x) : 32; }
proto(bit_clz, u64, u64) { return x != 0 ? __builtin_clzll(x) : 64; }

proto(bit_ctz, u8, u8) { return x != 0 ? __builtin_ctz(x) : 8; }
proto(bit_ctz, u16, u16) { return x != 0 ? __builtin_ctz(x) : 16; }
proto(bit_ctz, u32, u32) { return x != 0 ? __builtin_ctz(x) : 32; }
proto(bit_ctz, u64, u64) { return x != 0 ? __builtin_ctzll(x) : 64; }

proto(bit_fls, u8, u8) { return x != 0 ? 32 - __builtin_clz(x) : 0; }
proto(bit_fls, u16, u16) { return x != 0 ? 32 - __builtin_clz(x) : 0; }
proto(bit_fls, u32, u32) { return x != 0 ? 32 - __builtin_clz(x) : 0; }
proto(bit_fls, u64, u64) { return x != 0 ? 64 - __builtin_clzll(x) : 0; }

proto(bit_popcount, u8, u8) { return __builtin_popcount(x); }
proto(bit_popcount, u16, u16) { return __builtin_popcount(x); }
proto(bit_popcount, u32, u32) { return __builtin_popcount(x); }
proto(bit_popcount, u64, u64) { return __builtin_popcountll(x); }

proto(bit_parity, bool, u8) { return __builtin_parity(x); }
proto(bit_parity, bool, u16) { return __builtin_parity(x); }
proto(bit_parity, bool, u32) { return __builtin_parity(x); }
proto(bit_parity, bool, u64) { return __builtin_parityll(x); }

#undef GEN1
#undef NB

#endif
