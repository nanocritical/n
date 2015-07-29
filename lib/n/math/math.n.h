#include <math.h>

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

static inline NB(Double) n$math$Acos(NB(Double) x) { return acos(x); }
static inline NB(Float) n$math$Acosf(NB(Float) x) { return acosf(x); }
static inline NB(Double) n$math$Acosh(NB(Double) x) { return acosh(x); }
static inline NB(Float) n$math$Acoshf(NB(Float) x) { return acoshf(x); }
static inline NB(Double) n$math$Asin(NB(Double) x) { return asin(x); }
static inline NB(Float) n$math$Asinf(NB(Float) x) { return asinf(x); }
static inline NB(Double) n$math$Asinh(NB(Double) x) { return asinh(x); }
static inline NB(Float) n$math$Asinhf(NB(Float) x) { return asinhf(x); }
static inline NB(Double) n$math$Atan(NB(Double) x) { return atan(x); }
static inline NB(Double) n$math$Atan2(NB(Double) x, NB(Double) y) { return atan2(x, y); }
static inline NB(Float) n$math$Atan2f(NB(Float) x, NB(Float) y) { return atan2f(x, y); }
static inline NB(Float) n$math$Atanf(NB(Float) x) { return atanf(x); }
static inline NB(Double) n$math$Atanh(NB(Double) x) { return atanh(x); }
static inline NB(Float) n$math$Atanhf(NB(Float) x) { return atanhf(x); }
static inline NB(Double) n$math$Cbrt(NB(Double) x) { return cbrt(x); }
static inline NB(Float) n$math$Cbrtf(NB(Float) x) { return cbrtf(x); }
static inline NB(Double) n$math$Ceil(NB(Double) x) { return ceil(x); }
static inline NB(Float) n$math$Ceilf(NB(Float) x) { return ceilf(x); }
static inline NB(Double) n$math$Copysign(NB(Double) x, NB(Double) y) { return copysign(x, y); }
static inline NB(Float) n$math$Copysignf(NB(Float) x, NB(Float) y) { return copysignf(x, y); }
static inline NB(Double) n$math$Cos(NB(Double) x) { return cos(x); }
static inline NB(Float) n$math$Cosf(NB(Float) x) { return cosf(x); }
static inline NB(Double) n$math$Cosh(NB(Double) x) { return cosh(x); }
static inline NB(Float) n$math$Coshf(NB(Float) x) { return coshf(x); }
static inline NB(Double) n$math$Erf(NB(Double) x) { return erf(x); }
static inline NB(Double) n$math$Erfc(NB(Double) x) { return erfc(x); }
static inline NB(Float) n$math$Erfcf(NB(Float) x) { return erfcf(x); }
static inline NB(Float) n$math$Erff(NB(Float) x) { return erff(x); }
static inline NB(Double) n$math$Exp(NB(Double) x) { return exp(x); }
static inline NB(Double) n$math$Exp2(NB(Double) x) { return exp2(x); }
static inline NB(Float) n$math$Exp2f(NB(Float) x) { return exp2f(x); }
static inline NB(Float) n$math$Expf(NB(Float) x) { return expf(x); }
static inline NB(Double) n$math$Expm1(NB(Double) x) { return expm1(x); }
static inline NB(Float) n$math$Expm1f(NB(Float) x) { return expm1f(x); }
static inline NB(Double) n$math$Fabs(NB(Double) x) { return fabs(x); }
static inline NB(Float) n$math$Fabsf(NB(Float) x) { return fabsf(x); }
static inline NB(Double) n$math$Fdim(NB(Double) x, NB(Double) y) { return fdim(x, y); }
static inline NB(Float) n$math$Fdimf(NB(Float) x, NB(Float) y) { return fdimf(x, y); }
static inline NB(Double) n$math$Floor(NB(Double) x) { return floor(x); }
static inline NB(Float) n$math$Floorf(NB(Float) x) { return floorf(x); }
static inline NB(Double) n$math$Fma(NB(Double) x, NB(Double) y, NB(Double) z) { return fma(x, y, z); }
static inline NB(Float) n$math$Fmaf(NB(Float) x, NB(Float) y, NB(Float) z) { return fmaf(x, y, z); }
static inline NB(Double) n$math$Fmax(NB(Double) x, NB(Double) y) { return fmax(x, y); }
static inline NB(Float) n$math$Fmaxf(NB(Float) x, NB(Float) y) { return fmaxf(x, y); }
static inline NB(Double) n$math$Fmin(NB(Double) x, NB(Double) y) { return fmin(x, y); }
static inline NB(Float) n$math$Fminf(NB(Float) x, NB(Float) y) { return fminf(x, y); }
static inline NB(Double) n$math$Fmod(NB(Double) x, NB(Double) y) { return fmod(x, y); }
static inline NB(Float) n$math$Fmodf(NB(Float) x, NB(Float) y) { return fmodf(x, y); }
static inline NB(Double) n$math$Frexp(NB(Double) x, NB(I32) *exp) { return frexp(x, exp); }
static inline NB(Float) n$math$Frexpf(NB(Float) x, NB(I32) *exp) { return frexpf(x, exp); }
static inline NB(Double) n$math$Hypot(NB(Double) x, NB(Double) y) { return hypot(x, y); }
static inline NB(Float) n$math$Hypotf(NB(Float) x, NB(Float) y) { return hypotf(x, y); }
static inline NB(I32) n$math$Ilogb(NB(Double) x) { return ilogb(x); }
static inline NB(I32) n$math$Ilogbf(NB(Float) x) { return ilogbf(x); }
static inline NB(Double) n$math$J0(NB(Double) x) { return j0(x); }
static inline NB(Double) n$math$J1(NB(Double) x) { return j1(x); }
static inline NB(Double) n$math$Jn(NB(I32) x, NB(Double) y) { return jn(x, y); }
static inline NB(Double) n$math$Ldexp(NB(Double) x, NB(I32) y) { return ldexp(x, y); }
static inline NB(Float) n$math$Ldexpf(NB(Float) x, NB(I32) y) { return ldexpf(x, y); }
static inline NB(Double) n$math$Lgamma(NB(Double) x) { return lgamma(x); }
static inline NB(Float) n$math$Lgammaf(NB(Float) x) { return lgammaf(x); }
static inline NB(Double) n$math$Log(NB(Double) x) { return log(x); }
static inline NB(Double) n$math$Log10(NB(Double) x) { return log10(x); }
static inline NB(Float) n$math$Log10f(NB(Float) x) { return log10f(x); }
static inline NB(Double) n$math$Log1p(NB(Double) x) { return log1p(x); }
static inline NB(Float) n$math$Log1pf(NB(Float) x) { return log1pf(x); }
static inline NB(Double) n$math$Log2(NB(Double) x) { return log2(x); }
static inline NB(Float) n$math$Log2f(NB(Float) x) { return log2f(x); }
static inline NB(Double) n$math$Logb(NB(Double) x) { return logb(x); }
static inline NB(Float) n$math$Logbf(NB(Float) x) { return logbf(x); }
static inline NB(Float) n$math$Logf(NB(Float) x) { return logf(x); }
static inline NB(I64) n$math$Lrint(NB(Double) x) { return lrint(x); }
static inline NB(I64) n$math$Lrintf(NB(Float) x) { return lrintf(x); }
static inline NB(I64) n$math$Lround(NB(Double) x) { return lround(x); }
static inline NB(I64) n$math$Lroundf(NB(Float) x) { return lroundf(x); }
static inline NB(Double) n$math$Modf(NB(Double) x, NB(Double) *iptr) { return modf(x, iptr); }
static inline NB(Float) n$math$Modff(NB(Float) x, NB(Float) *iptr) { return modff(x, iptr); }
static inline NB(Double) n$math$Nearbyint(NB(Double) x) { return nearbyint(x); }
static inline NB(Float) n$math$Nearbyintf(NB(Float) x) { return nearbyintf(x); }
static inline NB(Double) n$math$Nextafter(NB(Double) x, NB(Double) y) { return nextafter(x, y); }
static inline NB(Float) n$math$Nextafterf(NB(Float) x, NB(Float) y) { return nextafterf(x, y); }
static inline NB(Double) n$math$Pow(NB(Double) x, NB(Double) y) { return pow(x, y); }
static inline NB(Float) n$math$Powf(NB(Float) x, NB(Float) y) { return powf(x, y); }
static inline NB(Double) n$math$Remainder(NB(Double) x, NB(Double) y) { return remainder(x, y); }
static inline NB(Float) n$math$Remainderf(NB(Float) x, NB(Float) y) { return remainderf(x, y); }
static inline NB(Double) n$math$Remquo(NB(Double) x, NB(Double) y, NB(I32) *quo) { return remquo(x, y, quo); }
static inline NB(Float) n$math$Remquof(NB(Float) x, NB(Float) y, NB(I32) *quo) { return remquof(x, y, quo); }
static inline NB(Double) n$math$Rint(NB(Double) x) { return rint(x); }
static inline NB(Float) n$math$Rintf(NB(Float) x) { return rintf(x); }
static inline NB(Double) n$math$Round(NB(Double) x) { return round(x); }
static inline NB(Float) n$math$Roundf(NB(Float) x) { return roundf(x); }
static inline NB(Double) n$math$Scalbln(NB(Double) x, NB(I64) y) { return scalbln(x, y); }
static inline NB(Float) n$math$Scalblnf(NB(Float) x, NB(I64) y) { return scalblnf(x, y); }
static inline NB(Double) n$math$Scalbn(NB(Double) x, NB(I32) y) { return scalbn(x, y); }
static inline NB(Float) n$math$Scalbnf(NB(Float) x, NB(I32) y) { return scalbnf(x, y); }
static inline NB(Double) n$math$Sin(NB(Double) x) { return sin(x); }
static inline NB(Float) n$math$Sinf(NB(Float) x) { return sinf(x); }
static inline NB(Double) n$math$Sinh(NB(Double) x) { return sinh(x); }
static inline NB(Float) n$math$Sinhf(NB(Float) x) { return sinhf(x); }
static inline NB(Double) n$math$Sqrt(NB(Double) x) { return sqrt(x); }
static inline NB(Float) n$math$Sqrtf(NB(Float) x) { return sqrtf(x); }
static inline NB(Double) n$math$Tan(NB(Double) x) { return tan(x); }
static inline NB(Float) n$math$Tanf(NB(Float) x) { return tanf(x); }
static inline NB(Double) n$math$Tanh(NB(Double) x) { return tanh(x); }
static inline NB(Float) n$math$Tanhf(NB(Float) x) { return tanhf(x); }
static inline NB(Double) n$math$Tgamma(NB(Double) x) { return tgamma(x); }
static inline NB(Float) n$math$Tgammaf(NB(Float) x) { return tgammaf(x); }
static inline NB(Double) n$math$Trunc(NB(Double) x) { return trunc(x); }
static inline NB(Float) n$math$Truncf(NB(Float) x) { return truncf(x); }
static inline NB(Double) n$math$Y0(NB(Double) x) { return y0(x); }
static inline NB(Double) n$math$Y1(NB(Double) x) { return y1(x); }
static inline NB(Double) n$math$Yn(NB(I32) x, NB(Double) y) { return yn(x, y); }

#undef likely
#undef proto
#undef GEN1
#undef NB

#endif
