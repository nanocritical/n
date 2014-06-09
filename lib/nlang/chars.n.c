#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(t) nlang$builtins$##t

NB(I32) nlang$chars$Static_string$Operator_compare(const nlang$chars$Static_string *self,
                                                   const nlang$chars$Static_string *other) {
  return strcmp((const char *)self->bytes.dat, (const char *)other->bytes.dat);
}

#undef NB

#endif
