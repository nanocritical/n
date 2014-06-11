#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(t) n$builtins$##t

NB(I32) n$chars$Static_string$Operator_compare(const n$chars$Static_string *self,
                                                   const n$chars$Static_string *other) {
  return strcmp((const char *)self->bytes.dat, (const char *)other->bytes.dat);
}

#undef NB

#endif
