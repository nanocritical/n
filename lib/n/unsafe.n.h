#ifdef NLANG_DECLARE_TYPES
struct n$unsafe$voidref;
struct n$unsafe$voidmref;
struct n$unsafe$voidmmref;
#endif

#ifdef NLANG_DEFINE_TYPES

struct n$unsafe$voidref {
  const void *p;
};

struct n$unsafe$voidmref {
  void *p;
};

struct n$unsafe$voidmmref {
  void *p;
};

#endif
