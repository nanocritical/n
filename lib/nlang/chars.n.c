#ifdef NLANG_DEFINE_FUNCTIONS

#define NB(t) nlang$builtins$##t

NB(i32) nlang$chars$static_string$operator_compare(const nlang$chars$static_string *self,
                                                   const nlang$chars$static_string *other) {
  return strcmp((const char *)self->bytes.dat, (const char *)other->bytes.dat);
}

#undef NB

#endif
