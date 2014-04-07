#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/arith.n.o.h"
#undef NLANG_DECLARE_TYPES
#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t00/arith.n.o.h"
#undef NLANG_DEFINE_TYPES
#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/arith.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS
#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/arith.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS
nlang_builtins_i32 _Nmain(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
;;
nlang_builtins_i32 m = (nlang_builtins_i32)0xffff;
;
;
;;
nlang_builtins_i32 _Ngensym0 = (m | m);
;
nlang_builtins_i32 _Ngensym1 = (nlang_builtins_i32)0;
;
nlang_builtins_u32 _Ngensym2 = (nlang_builtins_u32)1;
;
nlang_builtins_i32 _Ngensym3 = (_Ngensym1 << _Ngensym2);
;
nlang_builtins_i32 y = (_Ngensym0 + _Ngensym3);
;
;;
nlang_builtins_i32 _Ngensym4 = (nlang_builtins_i32)2;
;
nlang_builtins_i32 _Ngensym5 = (m | y);
;
nlang_builtins_i32 x = (_Ngensym4 & _Ngensym5);
;
nlang_builtins_i32 _Ngensym6 = (nlang_builtins_i32)2;
;
nlang_builtins_i32 _Ngensym7 = (x - _Ngensym6);
;
return _Ngensym7;
}
return _nretval;
}
void t00_arith_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_arith_Nrunexamples(void) {
}
