#ifndef nlang_math_h
#define nlang_math_h

#define NB(n) nlang_builtins_##n
#define GEN1(t, f) _Ngen_lib_nlang_math_##f##__##t##_genN_

#define proto(name, t) NB(t) GEN1(nlang_builtins_##t, name)(NB(t) x)

proto(bit_ffs, u8) { return __builtin_ffs(x); }
proto(bit_ffs, u16) { return __builtin_ffs(x); }
proto(bit_ffs, u32) { return __builtin_ffs(x); }
proto(bit_ffs, u64) { return __builtin_ffsll(x); }

proto(bit_clz, u8) { return __builtin_clz(x) - 24; }
proto(bit_clz, u16) { return __builtin_clz(x) - 16; }
proto(bit_clz, u32) { return __builtin_clz(x); }
proto(bit_clz, u64) { return __builtin_clzll(x); }

proto(bit_ctz, u8) { return __builtin_ctz(x); }
proto(bit_ctz, u16) { return __builtin_ctz(x); }
proto(bit_ctz, u32) { return __builtin_ctz(x); }
proto(bit_ctz, u64) { return __builtin_ctzll(x); }

proto(bit_fls, u8) { return x != 0 ? 32 - __builtin_clz(x) : 0; }
proto(bit_fls, u16) { return x != 0 ? 32 - __builtin_clz(x) : 0; }
proto(bit_fls, u32) { return x != 0 ? 32 - __builtin_clz(x) : 0; }
proto(bit_fls, u64) { return x != 0 ? 64 - __builtin_clzll(x) : 0; }

proto(bit_popcount, u8) { return __builtin_popcount(x); }
proto(bit_popcount, u16) { return __builtin_popcount(x); }
proto(bit_popcount, u32) { return __builtin_popcount(x); }
proto(bit_popcount, u64) { return __builtin_popcountll(x); }

proto(bit_parity, u8) { return __builtin_parity(x); }
proto(bit_parity, u16) { return __builtin_parity(x); }
proto(bit_parity, u32) { return __builtin_parity(x); }
proto(bit_parity, u64) { return __builtin_parityll(x); }

#undef GEN1
#undef NB

#endif
