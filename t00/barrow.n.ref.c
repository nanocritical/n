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
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
return x;
}
return _nretval;
}

static nlang_builtins_i32 t00_barrow_bar(nlang_builtins_i32 x, nlang_builtins_i32 y) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_builtins_i32 _Ngensym0 = (x + y);
;
return _Ngensym0;
}
return _nretval;
}

nlang_builtins_i32 _Nmain(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
;;
nlang_builtins_i32 _Ngensym1 = (nlang_builtins_i32)0;
;
nlang_builtins_i32 z = t00_barrow_foo(_Ngensym1);
;
;
;;
nlang_builtins_i32 _Ngensym2 = (nlang_builtins_i32)1;
;
nlang_builtins_i32 _Ngensym3 = t00_barrow_foo(_Ngensym2);
;
nlang_builtins_i32 x = (z + _Ngensym3);
;
nlang_builtins_i32 _Ngensym4 = (nlang_builtins_i32)0;
;
nlang_builtins_i32 _Ngensym5 = (nlang_builtins_i32)1;
;
nlang_builtins_i32 _Ngensym6 = t00_barrow_bar(_Ngensym4, _Ngensym5);
;
nlang_builtins_i32 _Ngensym7 = t00_barrow_foo(_Ngensym6);
;
nlang_builtins_i32 _Ngensym8 = (_Ngensym7 - x);
;
return _Ngensym8;
}
return _nretval;
}
void t00_barrow_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_barrow_Nrunexamples(void) {
}
