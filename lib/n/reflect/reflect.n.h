#ifdef NLANG_DECLARE_FUNCTIONS

static inline _$Nref_n$reflect$Type n$reflect$Get_type_from_dyntable(
    n$unsafe$Voidref dyntable) {
  // .type is in first position in dyntable.
  return *(void **)dyntable;
}

#endif
