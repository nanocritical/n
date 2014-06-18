#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(n) n$builtins$##n
#define GEN1(t, f) _Ngen_n$math$##f##__##t##_genN_

#define proto(name, rt, t) NB(rt) GEN1(NB(t), name)(NB(t) x)

proto(Bit_ffs, u8, u8) { return __builtin_ffs(x); }
proto(Bit_ffs, u16, u16) { return __builtin_ffs(x); }
proto(Bit_ffs, u32, u32) { return __builtin_ffs(x); }
proto(Bit_ffs, u64, u64) { return __builtin_ffsll(x); }

proto(Bit_clz, u8, u8) { return x != 0 ? __builtin_clz(x) - 24 : 8; }
proto(Bit_clz, u16, u16) { return x != 0 ? __builtin_clz(x) - 16 : 16; }
proto(Bit_clz, u32, u32) { return x != 0 ? __builtin_clz(x) : 32; }
proto(Bit_clz, u64, u64) { return x != 0 ? __builtin_clzll(x) : 64; }

proto(Bit_ctz, u8, u8) { return x != 0 ? __builtin_ctz(x) : 8; }
proto(Bit_ctz, u16, u16) { return x != 0 ? __builtin_ctz(x) : 16; }
proto(Bit_ctz, u32, u32) { return x != 0 ? __builtin_ctz(x) : 32; }
proto(Bit_ctz, u64, u64) { return x != 0 ? __builtin_ctzll(x) : 64; }

proto(Bit_fls, u8, u8) { return x != 0 ? 32 - __builtin_clz(x) : 0; }
proto(Bit_fls, u16, u16) { return x != 0 ? 32 - __builtin_clz(x) : 0; }
proto(Bit_fls, u32, u32) { return x != 0 ? 32 - __builtin_clz(x) : 0; }
proto(Bit_fls, u64, u64) { return x != 0 ? 64 - __builtin_clzll(x) : 0; }

proto(Bit_popcount, u8, u8) { return __builtin_popcount(x); }
proto(Bit_popcount, u16, u16) { return __builtin_popcount(x); }
proto(Bit_popcount, u32, u32) { return __builtin_popcount(x); }
proto(Bit_popcount, u64, u64) { return __builtin_popcountll(x); }

proto(Bit_parity, bool, u8) { return __builtin_parity(x); }
proto(Bit_parity, bool, u16) { return __builtin_parity(x); }
proto(Bit_parity, bool, u32) { return __builtin_parity(x); }
proto(Bit_parity, bool, u64) { return __builtin_parityll(x); }

#undef GEN1
#undef NB

#endif
