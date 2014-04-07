#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/example.n.o.h"
#undef NLANG_DECLARE_TYPES




#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t00/example.n.o.h"
#undef NLANG_DEFINE_TYPES




#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/example.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS
static nlang_builtins_i32 t00_example_foo(nlang_builtins_i32 x);

void t00_example__Nexample0(void) __attribute__((section(".text.nlang.examples")));
void t00_example__Nexample1(void) __attribute__((section(".text.nlang.examples")));
void t00_example__Nexample2(void) __attribute__((section(".text.nlang.examples")));
#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/example.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS
static nlang_builtins_i32 t00_example_foo(nlang_builtins_i32 x) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_builtins_i32 _Ngensym0 = (nlang_builtins_i32)0;
;
return _Ngensym0;
}
return _nretval;
}

void t00_example__Nexample0(void) {
({ nlang_builtins_i32 _Ngensym1 = (nlang_builtins_i32)0;
;
nlang_builtins_i32 _Ngensym2 = (nlang_builtins_i32)0;
;
nlang_builtins_i32 _Ngensym3 = t00_example_foo(_Ngensym2);
;
nlang_builtins_bool _Ngensym4 = (_Ngensym1 == _Ngensym3);
;
;;
nlang_builtins___example__(_Ngensym4);
; });
}
void t00_example__Nexample1(void) {
({ nlang_builtins_i32 _Ngensym6 = (nlang_builtins_i32)1;
;
nlang_builtins_i32 _Ngensym7 = t00_example_foo(_Ngensym6);
;
nlang_builtins_i32 _Ngensym8 = (nlang_builtins_i32)0;
;
nlang_builtins_bool _Ngensym9 = (_Ngensym7 == _Ngensym8);
;
;;
nlang_builtins___example__(_Ngensym9);
; });
}
void t00_example__Nexample2(void) {
({ nlang_builtins_bool _Ngensym10= { 0 };
;
;
 {
nlang_builtins_i32 _Ngensymb = (nlang_builtins_i32)1;
;
nlang_builtins_i32 _Ngensymc = (-_Ngensymb);
;
nlang_builtins_i32 _Ngensymd = t00_example_foo(_Ngensymc);
;
nlang_builtins_i32 _Ngensyme = (nlang_builtins_i32)0;
;
nlang_builtins_bool _Ngensymf = (_Ngensymd == _Ngensyme);
;
_Ngensym10 = _Ngensymf;
};
;;
nlang_builtins___example__(_Ngensym10);
; });
}
nlang_builtins_i32 _Nmain(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_builtins_i32 _Ngensym12 = (nlang_builtins_i32)1;
;
nlang_builtins_i32 _Ngensym13 = t00_example_foo(_Ngensym12);
;
return _Ngensym13;
}
return _nretval;
}
void t00_example_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_example_Nrunexamples(void) {
t00_example__Nexample0();
t00_example__Nexample1();
t00_example__Nexample2();
}
