#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t01/genericfun.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t01/genericfun.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t01/genericfun.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#ifndef HAS2__Ngen_t01_genericfun_decr__nlang_builtins_i32_genN_
#define HAS2__Ngen_t01_genericfun_decr__nlang_builtins_i32_genN_
static nlang_builtins_i32 _Ngen_t01_genericfun_decr__nlang_builtins_i32_genN_(nlang_builtins_i32 x);
#endif // HAS2__Ngen_t01_genericfun_decr__nlang_builtins_i32_genN_
#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t01/genericfun.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#ifndef HAS3__Ngen_t01_genericfun_decr__nlang_builtins_i32_genN_
#define HAS3__Ngen_t01_genericfun_decr__nlang_builtins_i32_genN_
static nlang_builtins_i32 _Ngen_t01_genericfun_decr__nlang_builtins_i32_genN_(nlang_builtins_i32 x) {
 {
return (x - (nlang_builtins_i32)1);
}
}
#endif // HAS3__Ngen_t01_genericfun_decr__nlang_builtins_i32_genN_
nlang_builtins_i32 _Nmain(void) {
 {
return _Ngen_t01_genericfun_decr__nlang_builtins_i32_genN_((nlang_builtins_i32)1);
}
}
void t01_genericfun_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t01_genericfun_Nrunexamples(void) {
}