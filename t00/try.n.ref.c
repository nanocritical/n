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
nlang_builtins_error _Ngensym0 = { 0 };
 {
nlang_builtins_error _Ngensym16 = { 0 };
_Ngensym0 = ({ _Ngensym16.code = (nlang_builtins_i32)1;
;
_Ngensym16;
; }); goto _Ngensym1;
;
}while (0) {

_Ngensym1: {
nlang_builtins_error err = _Ngensym0;
 {
_nretval = err;
return;
}
}
}
;
}
#undef _nretval
}

static nlang_builtins_i32 t00_try_foo(void) {
 {
nlang_builtins_error _Ngensym2 = { 0 };
 {
nlang_builtins_error _Ngensym17 = { 0 };
_Ngensym2 = ({ _Ngensym17.code = (nlang_builtins_i32)1;
;
_Ngensym17;
; }); goto ONE;
;
nlang_builtins_error _Ngensym18 = { 0 };
_Ngensym2 = ({ _Ngensym18.code = (nlang_builtins_i32)0;
;
_Ngensym18;
; }); goto ZERO;
;
}while (0) {

ZERO: {
(void) (_Ngensym2);
 {
return (nlang_builtins_i32)0;
}
}

ONE: {
(void) (_Ngensym2);
 {
return (nlang_builtins_i32)1;
}
}
}
;
}
}

#ifndef HAS3__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
#define HAS3__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
struct nlang_builtins_error;
typedef const struct nlang_builtins_error* _Ngen_nlang_builtins_ref__nlang_builtins_error_genN_;
#endif // HAS3__Ngen_nlang_builtins_ref__nlang_builtins_error_genN_
static nlang_builtins_i32 t00_try_bar(void) {
 {
nlang_builtins_error _Ngensym3 = { 0 };
 {
nlang_builtins_error _Ngensyma= { 0 };
_Ngensyma.code = (nlang_builtins_i32)1;
;
_Ngensym3 = _Ngensyma; if (nlang_builtins_error_operator_test(((&_Ngensyma)))) { goto _Ngensym5; };
return (nlang_builtins_i32)0;
}while (0) {

_Ngensym5: {
nlang_builtins_error err = _Ngensym3;
 {
nlang_builtins_error _Ngensym4 = { 0 };
 {
nlang_builtins_error _Ngensymb= { 0 };
_Ngensymb.code = ((err.code) + (nlang_builtins_i32)1);
;
_Ngensym4 = _Ngensymb; if (nlang_builtins_error_operator_test(((&_Ngensymb)))) { goto ONE; };
nlang_builtins_error _Ngensymc= { 0 };
_Ngensymc.code = (nlang_builtins_i32)0;
;
_Ngensym4 = _Ngensymc; if (nlang_builtins_error_operator_test(((&_Ngensymc)))) { goto ZERO; };
return (nlang_builtins_i32)0;
}while (0) {

ONE: {
nlang_builtins_error err = _Ngensym4;
 {
return ((err.code) - (nlang_builtins_i32)1);
}
}

ZERO: {
(void) (_Ngensym4);
 {
return (nlang_builtins_i32)0;
}
}
}
;
}
}
}
;
}
}

static nlang_builtins_i32 t00_try_noerror(void) {
 {
nlang_builtins_error _Ngensym6 = { 0 };
 {
nlang_builtins_error _Ngensymd= { 0 };
_Ngensymd.code = (nlang_builtins_i32)0;
;
_Ngensym6 = _Ngensymd; if (nlang_builtins_error_operator_test(((&_Ngensymd)))) { goto _Ngensym7; };
return (nlang_builtins_i32)0;
}while (0) {

_Ngensym7: {
(void) (_Ngensym6);
 {
return (nlang_builtins_i32)1;
}
}
}
;
}
}

nlang_builtins_i32 _Nmain(void) {
 {
nlang_builtins_error _Ngensym19 = { 0 };
nlang_builtins_error _Ngensym1a = { 0 };
nlang_builtins_assert(nlang_builtins_error_operator_ne(({  {
t00_try_inferred(&(_Ngensym19));
_Ngensym1a = _Ngensym19;
};
((&_Ngensym1a));
; }), ((&(nlang_builtins_error_OK)))));
;
nlang_builtins_assert((t00_try_foo() == (nlang_builtins_i32)1));
nlang_builtins_assert((t00_try_bar() == (nlang_builtins_i32)1));
nlang_builtins_assert((t00_try_noerror() == (nlang_builtins_i32)0));
nlang_builtins_error _Ngensym8 = { 0 };
 {
nlang_builtins_error _Ngensym1b = { 0 };
_Ngensym8 = ({ _Ngensym1b.code = t00_try_bar();
;
_Ngensym1b;
; }); goto _Ngensym9;
;
return (nlang_builtins_i32)1;
}while (0) {

_Ngensym9: {
nlang_builtins_error err = _Ngensym8;
 {
return ((err.code) - (nlang_builtins_i32)1);
}
}
}
;
}
}
void t00_try_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_try_Nrunexamples(void) {
}