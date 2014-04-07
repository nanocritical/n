#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/while.n.o.h"
#undef NLANG_DECLARE_TYPES
#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t00/while.n.o.h"
#undef NLANG_DEFINE_TYPES
#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/while.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS
#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/while.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS
nlang_builtins_i32 _Nmain(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
;;
nlang_builtins_i32 j = (nlang_builtins_i32)0;
;
 {
;;
nlang_builtins_i32 i = (nlang_builtins_i32)0;
;
 {
while (({ nlang_builtins_i32 _Ngensym0 = (nlang_builtins_i32)10;
;
nlang_builtins_bool _Ngensym1 = (i < _Ngensym0);
;
_Ngensym1;
; })) {
;
;
nlang_builtins_i32 _Ngensym2 = (nlang_builtins_i32)1;
;
i += _Ngensym2;
;
nlang_builtins_i32 _Ngensym3 = (nlang_builtins_i32)1;
;
j += _Ngensym3;
};
;
;
};
};
;
nlang_builtins_i32 _Ngensym4 = (nlang_builtins_i32)10;
;
nlang_builtins_i32 _Ngensym5 = (j - _Ngensym4);
;
return _Ngensym5;
}
return _nretval;
}
void t00_while_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_while_Nrunexamples(void) {
}
