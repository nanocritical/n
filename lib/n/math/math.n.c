#define NB(n) n$builtins$##n

#ifdef NLANG_DEFINE_FUNCTIONS

NB(Double) n$math$E = M_E;
NB(Double) n$math$LOG2E = M_LOG2E;
NB(Double) n$math$LOG10E = M_LOG10E;
NB(Double) n$math$LN2 = M_LN2;
NB(Double) n$math$LN10 = M_LN10;
NB(Double) n$math$PI = M_PI;
NB(Double) n$math$RATIO_PI_2 = M_PI_2;
NB(Double) n$math$RATIO_PI_4 = M_PI_4;
NB(Double) n$math$RATIO_1_PI = M_1_PI;
NB(Double) n$math$RATIO_2_PI = M_2_PI;
NB(Double) n$math$RATIO_2_SQRTPI = M_2_SQRTPI;
NB(Double) n$math$SQRT2 = M_SQRT2;
NB(Double) n$math$RATIO_SQRT1_2 = M_SQRT1_2;

#endif

#undef NB
