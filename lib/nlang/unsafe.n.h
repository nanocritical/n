#ifdef NLANG_DECLARE_TYPES
struct nlang$unsafe$voidref;
struct nlang$unsafe$voidmref;
struct nlang$unsafe$voidmmref;
#endif

#ifdef NLANG_DEFINE_TYPES

struct nlang$unsafe$voidref {
  const void *p;
};

struct nlang$unsafe$voidmref {
  void *p;
};

struct nlang$unsafe$voidmmref {
  void *p;
};

#endif
