#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/float.n.o.h"
#undef NLANG_DECLARE_TYPES
#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t00/float.n.o.h"
#undef NLANG_DEFINE_TYPES
#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/float.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS
#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/float.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS
nlang_builtins_i32 _Nmain(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
;;
nlang_builtins_float f = (nlang_builtins_float)0.0213;
;
;
;;
nlang_builtins_float _Ngensym0 = (nlang_builtins_float)10.2;
;
nlang_builtins_float _Ngensym1 = (nlang_builtins_float).2;
;
nlang_builtins_float _Ngensym2 = (_Ngensym0 * _Ngensym1);
;
nlang_builtins_float g = (f + _Ngensym2);
;
( (({ nlang_builtins_float _Ngensym3 = (nlang_builtins_float)0.1;
;
nlang_builtins_bool _Ngensym4 = (g < _Ngensym3);
;
_Ngensym4;
; })) ? ( {
nlang_builtins_i32 _Ngensym5 = (nlang_builtins_i32)1;
;
return _Ngensym5;
})
 : ( {
nlang_builtins_i32 _Ngensym6 = (nlang_builtins_i32)0;
;
return _Ngensym6;
}));
}
return _nretval;
}
void t00_float_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_float_Nrunexamples(void) {
}
