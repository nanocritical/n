#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/try.n.o.h"
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
# include "t00/try.n.o.h"
#undef NLANG_DEFINE_TYPES




#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/try.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS
static void t00_try_inferred(nlang_builtins_error *_nrtr__nretval);

static nlang_builtins_i32 t00_try_foo(void);

#ifndef HAS2__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
#define HAS2__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
struct nlang_builtins_error;
typedef const struct nlang_builtins_error* _Ngen_nlang_builtins_ref__nlang_builtins_error_genN_;
#endif // HAS2__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
static nlang_builtins_i32 t00_try_bar(void);

static nlang_builtins_i32 t00_try_noerror(void);

#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/try.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS
static void t00_try_inferred(nlang_builtins_error *_nrtr__nretval) {
#define _nretval (*_nrtr__nretval)
 {
nlang_builtins_error _Ngensym0= { 0 };
;
 {
 {
nlang_builtins_i32 _Ngensyma = (nlang_builtins_i32)1;
;
nlang_builtins_error _Ngensymb= { 0 };
_Ngensymb.code = _Ngensyma;
;
;
_Ngensym0 = _Ngensymb;
;
goto _Ngensym1;
};
}while (0) {

_Ngensym1: {
 {
;;
nlang_builtins_error err = _Ngensym0;
;
;
 {
_nretval = err;
return;
};
}
}
}
;
}
#undef _nretval
}

static nlang_builtins_i32 t00_try_foo(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_builtins_error _Ngensym2= { 0 };
;
 {
 {
nlang_builtins_i32 _Ngensymc = (nlang_builtins_i32)1;
;
nlang_builtins_error _Ngensymd= { 0 };
_Ngensymd.code = _Ngensymc;
;
;
_Ngensym2 = _Ngensymd;
;
goto ONE;
};
 {
nlang_builtins_i32 _Ngensyme = (nlang_builtins_i32)0;
;
nlang_builtins_error _Ngensymf= { 0 };
_Ngensymf.code = _Ngensyme;
;
;
_Ngensym2 = _Ngensymf;
;
goto ZERO;
};
nlang_builtins_i32 _Ngensym10 = (nlang_builtins_i32)2;
;
return _Ngensym10;
}while (0) {

ZERO: {
 {
;;
(void) _Ngensym2;
;
 {
nlang_builtins_i32 _Ngensym11 = (nlang_builtins_i32)0;
;
return _Ngensym11;
};
}
}

ONE: {
 {
;;
(void) _Ngensym2;
;
 {
nlang_builtins_i32 _Ngensym12 = (nlang_builtins_i32)1;
;
return _Ngensym12;
};
}
}
}
;
}
return _nretval;
}

#ifndef HAS3__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
#define HAS3__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
struct nlang_builtins_error;
typedef const struct nlang_builtins_error* _Ngen_nlang_builtins_ref__nlang_builtins_error_genN_;
#endif // HAS3__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
static nlang_builtins_i32 t00_try_bar(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_builtins_error _Ngensym3= { 0 };
;
 {
;;
nlang_builtins_i32 _Ngensym14 = (nlang_builtins_i32)1;
;
nlang_builtins_error _Ngensym13= { 0 };
_Ngensym13.code = _Ngensym14;
;
;
( (({ nlang_builtins_bool _Ngensym16= { 0 };
;
;
 {
_Ngen_nlang_builtins_ref__nlang_builtins_error_genN_ _Ngensym44 = ((&_Ngensym13));
;
nlang_builtins_bool _Ngensym15 = nlang_builtins_error_operator_test(_Ngensym44);
;
_Ngensym16 = _Ngensym15;
};
_Ngensym16;
; })) ? ( {
;
;
 {
_Ngensym3 = _Ngensym13;
goto _Ngensym5;
};
})
 : ( {
;;
}));
;
;
;
nlang_builtins_i32 _Ngensym17 = (nlang_builtins_i32)0;
;
return _Ngensym17;
}while (0) {

_Ngensym5: {
 {
;;
nlang_builtins_error err = _Ngensym3;
;
;
 {
nlang_builtins_error _Ngensym4= { 0 };
;
 {
;;
;;
nlang_builtins_i32 _Ngensym1a = (nlang_builtins_i32)1;
;
nlang_builtins_i32 _Ngensym1b = ((err.code) + _Ngensym1a);
;
nlang_builtins_error _Ngensym18= { 0 };
_Ngensym18.code = _Ngensym1b;
;
;
( (({ nlang_builtins_bool _Ngensym1d= { 0 };
;
;
 {
_Ngen_nlang_builtins_ref__nlang_builtins_error_genN_ _Ngensym48 = ((&_Ngensym18));
;
nlang_builtins_bool _Ngensym1c = nlang_builtins_error_operator_test(_Ngensym48);
;
_Ngensym1d = _Ngensym1c;
};
_Ngensym1d;
; })) ? ( {
;
;
 {
_Ngensym4 = _Ngensym18;
goto ONE;
};
})
 : ( {
;;
}));
;
;
;
;;
nlang_builtins_i32 _Ngensym1f = (nlang_builtins_i32)0;
;
nlang_builtins_error _Ngensym1e= { 0 };
_Ngensym1e.code = _Ngensym1f;
;
;
nlang_builtins_i32 _Ngensym20 = (nlang_builtins_i32)0;
;
return _Ngensym20;
( (({ nlang_builtins_bool _Ngensym22= { 0 };
;
;
 {
_Ngen_nlang_builtins_ref__nlang_builtins_error_genN_ _Ngensym4b = ((&_Ngensym1e));
;
nlang_builtins_bool _Ngensym21 = nlang_builtins_error_operator_test(_Ngensym4b);
;
_Ngensym22 = _Ngensym21;
};
_Ngensym22;
; })) ? ( {
;
;
 {
_Ngensym4 = _Ngensym1e;
goto ZERO;
};
})
 : ( {
;;
}));
;
;
}while (0) {

ONE: {
 {
;;
nlang_builtins_error err = _Ngensym4;
;
;
 {
;;
nlang_builtins_i32 _Ngensym24 = (nlang_builtins_i32)1;
;
nlang_builtins_i32 _Ngensym25 = ((err.code) - _Ngensym24);
;
return _Ngensym25;
};
}
}

ZERO: {
 {
;;
(void) _Ngensym4;
;
 {
nlang_builtins_i32 _Ngensym26 = (nlang_builtins_i32)0;
;
return _Ngensym26;
};
}
}
}
;
};
}
}
}
;
}
return _nretval;
}

static nlang_builtins_i32 t00_try_noerror(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_builtins_error _Ngensym6= { 0 };
;
 {
;;
nlang_builtins_i32 _Ngensym28 = (nlang_builtins_i32)0;
;
nlang_builtins_error _Ngensym27= { 0 };
_Ngensym27.code = _Ngensym28;
;
;
( (({ nlang_builtins_bool _Ngensym2a= { 0 };
;
;
 {
_Ngen_nlang_builtins_ref__nlang_builtins_error_genN_ _Ngensym4f = ((&_Ngensym27));
;
nlang_builtins_bool _Ngensym29 = nlang_builtins_error_operator_test(_Ngensym4f);
;
_Ngensym2a = _Ngensym29;
};
_Ngensym2a;
; })) ? ( {
;
;
 {
_Ngensym6 = _Ngensym27;
goto _Ngensym7;
};
})
 : ( {
;;
}));
;
;
;
nlang_builtins_i32 _Ngensym2b = (nlang_builtins_i32)0;
;
return _Ngensym2b;
}while (0) {

_Ngensym7: {
 {
;;
(void) _Ngensym6;
;
 {
nlang_builtins_i32 _Ngensym2c = (nlang_builtins_i32)1;
;
return _Ngensym2c;
};
}
}
}
;
}
return _nretval;
}

nlang_builtins_i32 _Nmain(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
;;
nlang_builtins_error _Ngensym50 = { 0 };
t00_try_inferred(&(_Ngensym50));
;
_Ngen_nlang_builtins_ref__nlang_builtins_error_genN_ _Ngensym57 = ((&_Ngensym50));
;
_Ngen_nlang_builtins_ref__nlang_builtins_error_genN_ _Ngensym58 = ((&(nlang_builtins_error_OK)));
;
nlang_builtins_bool _Ngensym2e = nlang_builtins_error_operator_ne(_Ngensym57, _Ngensym58);
;
;;
nlang_builtins_assert(_Ngensym2e);
;
nlang_builtins_i32 _Ngensym30 = (nlang_builtins_i32)1;
;
nlang_builtins_i32 _Ngensym51 = t00_try_foo();
;
nlang_builtins_bool _Ngensym31 = (_Ngensym51 == _Ngensym30);
;
nlang_builtins_assert(_Ngensym31);
nlang_builtins_i32 _Ngensym32 = (nlang_builtins_i32)1;
;
nlang_builtins_i32 _Ngensym52 = t00_try_bar();
;
nlang_builtins_bool _Ngensym33 = (_Ngensym52 == _Ngensym32);
;
nlang_builtins_assert(_Ngensym33);
nlang_builtins_i32 _Ngensym34 = (nlang_builtins_i32)0;
;
nlang_builtins_i32 _Ngensym53 = t00_try_noerror();
;
nlang_builtins_bool _Ngensym35 = (_Ngensym53 == _Ngensym34);
;
nlang_builtins_assert(_Ngensym35);
nlang_builtins_error _Ngensym8= { 0 };
;
 {
 {
nlang_builtins_i32 _Ngensym55 = t00_try_bar();
;
nlang_builtins_error _Ngensym36= { 0 };
_Ngensym36.code = _Ngensym55;
;
;
_Ngensym8 = _Ngensym36;
;
goto _Ngensym9;
};
nlang_builtins_i32 _Ngensym37 = (nlang_builtins_i32)1;
;
return _Ngensym37;
}while (0) {

_Ngensym9: {
 {
;;
nlang_builtins_error err = _Ngensym8;
;
;
 {
;;
nlang_builtins_i32 _Ngensym39 = (nlang_builtins_i32)1;
;
nlang_builtins_i32 _Ngensym3a = ((err.code) - _Ngensym39);
;
return _Ngensym3a;
};
}
}
}
;
}
return _nretval;
}
void t00_try_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_try_Nrunexamples(void) {
}
