#define NB(t) n$builtins$##t
#define NU(t) n$unsafe$##t

#ifdef NLANG_DECLARE_TYPES

typedef const void *NU(Voidref);

#endif

#ifdef NLANG_DECLARE_FUNCTIONS

static inline NU(Voidref) NU(Voidref$From_uintptr)(NB(Uintptr) p) {
  return (NU(Voidref)) (uintptr_t) p;
}

static inline NB(Uintptr) NU(Voidref$To_uintptr)(const NU(Voidref) *self) {
  return (uintptr_t) *self;
}

#endif

#undef NU
#undef NB
