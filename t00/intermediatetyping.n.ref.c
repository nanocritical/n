#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/intermediatetyping.n.o.h"
#undef NLANG_DECLARE_TYPES
#ifndef HAS0__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
#define HAS0__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
struct nlang_builtins_error;
typedef const struct nlang_builtins_error* _Ngen_nlang_builtins_ref__nlang_builtins_error_genN_;
#endif // HAS0__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_

#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t00/intermediatetyping.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/intermediatetyping.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS
#ifndef HAS2__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
#define HAS2__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
struct nlang_builtins_error;
typedef const struct nlang_builtins_error* _Ngen_nlang_builtins_ref__nlang_builtins_error_genN_;
#endif // HAS2__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
static nlang_builtins_i32 t00_intermediatetyping_noerror(void);

#ifndef HAS2__Ngen_nlang_builtins_max__nlang_builtins_i32_genN_
#define HAS2__Ngen_nlang_builtins_max__nlang_builtins_i32_genN_
static inline nlang_builtins_i32 _Ngen_nlang_builtins_max__nlang_builtins_i32_genN_(nlang_builtins_i32 a, nlang_builtins_i32 b);
#endif // HAS2__Ngen_nlang_builtins_max__nlang_builtins_i32_genN_
#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/intermediatetyping.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS
#ifndef HAS3__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
#define HAS3__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
struct nlang_builtins_error;
typedef const struct nlang_builtins_error* _Ngen_nlang_builtins_ref__nlang_builtins_error_genN_;
#endif // HAS3__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
static nlang_builtins_i32 t00_intermediatetyping_noerror(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
;;
nlang_builtins_i32 x= { 0 };
;
;
 {
nlang_builtins_error _Ngensym0= { 0 };
;
 {
;;
nlang_builtins_i32 _Ngensym3 = (nlang_builtins_i32)0;
;
nlang_builtins_error _Ngensym2= { 0 };
_Ngensym2.code = _Ngensym3;
;
;
( (({ nlang_builtins_bool _Ngensym5= { 0 };
;
;
 {
_Ngen_nlang_builtins_ref__nlang_builtins_error_genN_ _Ngensym15 = ((&_Ngensym2));
;
nlang_builtins_bool _Ngensym4 = nlang_builtins_error_operator_test(_Ngensym15);
;
_Ngensym5 = _Ngensym4;
};
_Ngensym5;
; })) ? ( {
;
;
 {
_Ngensym0 = _Ngensym2;
goto _Ngensym1;
};
})
 : ( {
;;
}));
;
;
;
nlang_builtins_i32 _Ngensym6 = (nlang_builtins_i32)0;
;
x = _Ngensym6;
}while (0) {

_Ngensym1: {
 {
;;
(void) _Ngensym0;
;
 {
nlang_builtins_i32 _Ngensym7 = (nlang_builtins_i32)1;
;
return _Ngensym7;
};
}
}
}
;
};
;
return x;
}
return _nretval;
}

#ifndef HAS3__Ngen_nlang_builtins_max__nlang_builtins_i32_genN_
#define HAS3__Ngen_nlang_builtins_max__nlang_builtins_i32_genN_
static inline nlang_builtins_i32 _Ngen_nlang_builtins_max__nlang_builtins_i32_genN_(nlang_builtins_i32 a, nlang_builtins_i32 b) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
( (({ nlang_builtins_bool _Ngensymfe = (a >= b);
;
_Ngensymfe;
; })) ? ( {
;
return a;
})
 : ( {
;
return b;
}));
;
;
}
return _nretval;
}
#endif // HAS3__Ngen_nlang_builtins_max__nlang_builtins_i32_genN_
nlang_builtins_i32 _Nmain(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
;;
nlang_builtins_i32 _Ngensym8 = (nlang_builtins_i32)2;
;
nlang_builtins_i32 _Ngensym16 = t00_intermediatetyping_noerror();
;
nlang_builtins_i32 x = (_Ngensym8 + _Ngensym16);
;
 {
nlang_builtins_i32 _Ngensym9 = (nlang_builtins_i32)1;
;
nlang_builtins_i32 _Ngensyma = (x + _Ngensym9);
;
x = _Ngensyma;
;
nlang_builtins_i32 _Ngensymb = (nlang_builtins_i32)0;
;
nlang_builtins_i32 _Ngensymc = _Ngen_nlang_builtins_max__nlang_builtins_i32_genN_(x, _Ngensymb);
;
nlang_builtins_i32 _Ngensymd = (nlang_builtins_i32)1;
;
nlang_builtins_i32 _Ngensyme = (_Ngensymc + _Ngensymd);
;
x = _Ngensyme;
};
;
nlang_builtins_i32 _Ngensymf = (nlang_builtins_i32)4;
;
nlang_builtins_i32 _Ngensym10 = (x - _Ngensymf);
;
return _Ngensym10;
}
return _nretval;
}
void t00_intermediatetyping_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_intermediatetyping_Nrunexamples(void) {
}
