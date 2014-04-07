#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/alignof.n.o.h"
#undef NLANG_DECLARE_TYPES
#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t00/alignof.n.o.h"
#undef NLANG_DEFINE_TYPES
#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/alignof.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS
#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/alignof.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS
nlang_builtins_i32 _Nmain(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_builtins_size _Ngensym0 = __alignof__(nlang_builtins_i32);
;
nlang_builtins_size _Ngensym1 = __alignof__(nlang_builtins_u32);
;
nlang_builtins_bool _Ngensym2 = (_Ngensym0 == _Ngensym1);
;
;;
nlang_builtins_assert(_Ngensym2);
;
nlang_builtins_i32 _Ngensym4 = (nlang_builtins_i32)0;
;
return _Ngensym4;
}
return _nretval;
}
void t00_alignof_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_alignof_Nrunexamples(void) {
}
