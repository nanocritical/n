#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/bool.n.o.h"
#undef NLANG_DECLARE_TYPES
#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t00/bool.n.o.h"
#undef NLANG_DEFINE_TYPES
#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/bool.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS
#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/bool.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS
nlang_builtins_i32 _Nmain(void) {
 {
nlang_builtins_bool x = (nlang_builtins_bool)0;
;
( ((x && (nlang_builtins_bool)1)) ? ( {
return (nlang_builtins_i32)1;
})
 : ( {
return (nlang_builtins_i32)0;
}));
}
}
void t00_bool_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_bool_Nrunexamples(void) {
}
