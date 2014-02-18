#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/barrow.n.o.h"
#undef NLANG_DECLARE_TYPES


#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t00/barrow.n.o.h"
#undef NLANG_DEFINE_TYPES


#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/barrow.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS
static nlang_builtins_i32 t00_barrow_foo(nlang_builtins_i32 x);

static nlang_builtins_i32 t00_barrow_bar(nlang_builtins_i32 x, nlang_builtins_i32 y);

#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/barrow.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS
static nlang_builtins_i32 t00_barrow_foo(nlang_builtins_i32 x) {
 {
return x;
}
}

static nlang_builtins_i32 t00_barrow_bar(nlang_builtins_i32 x, nlang_builtins_i32 y) {
 {
return (x + y);
}
}

nlang_builtins_i32 _Nmain(void) {
 {
nlang_builtins_i32 z = t00_barrow_foo((nlang_builtins_i32)0);
;
nlang_builtins_i32 x = (z + t00_barrow_foo((nlang_builtins_i32)1));
;
return (t00_barrow_foo(t00_barrow_bar((nlang_builtins_i32)0, (nlang_builtins_i32)1)) - x);
}
}
void t00_barrow_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_barrow_Nrunexamples(void) {
}
