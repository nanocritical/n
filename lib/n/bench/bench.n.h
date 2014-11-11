#define NLANG_EXAMPLE_PRE \
  struct n$bench$Example ex = { 0 }

#define NLANG_EXAMPLE_POST \
  n$bench$Example$Dtor(&ex)
