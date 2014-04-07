#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/match.n.o.h"
#undef NLANG_DECLARE_TYPES
typedef nlang_builtins_u32 t00_match_enu;



struct t00_match_t;
typedef struct t00_match_t t00_match_t;
#ifndef HAS0__Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_
#define HAS0__Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_
struct t00_match_t;
typedef struct t00_match_t* _Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_;
#endif // HAS0__Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_
;

#ifndef HAS0__Ngen_nlang_builtins_ref__t00_match_t_genN_
#define HAS0__Ngen_nlang_builtins_ref__t00_match_t_genN_
struct t00_match_t;
typedef const struct t00_match_t* _Ngen_nlang_builtins_ref__t00_match_t_genN_;
#endif // HAS0__Ngen_nlang_builtins_ref__t00_match_t_genN_

#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t00/match.n.o.h"
#undef NLANG_DEFINE_TYPES
#define t00_match_enu_A_tag_label__ ((nlang_builtins_u32)0)
static const t00_match_enu t00_match_enu_A = t00_match_enu_A_tag_label__;
#define t00_match_enu_B_tag_label__ (((nlang_builtins_u32)0 + (nlang_builtins_u32)1))
static const t00_match_enu t00_match_enu_B = t00_match_enu_B_tag_label__;



struct t00_match_t {
t00_match_enu x;
}
;


#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/match.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

static nlang_builtins_i32 t00_match_foo(t00_match_enu n);

static nlang_builtins_i32 t00_match_bar(t00_match_enu n);

#ifndef HAS2__Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_
#define HAS2__Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_
struct t00_match_t;
typedef struct t00_match_t* _Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_;
#endif // HAS2__Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_
nlang_builtins_void t00_match_t_ctor(_Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_ self) __attribute__((__nonnull__(1)));
;
void t00_match_t_mk(t00_match_t *_nrtr_r);
;
_Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_ t00_match_t_new(void);
;

#ifndef HAS2__Ngen_nlang_builtins_ref__t00_match_t_genN_
#define HAS2__Ngen_nlang_builtins_ref__t00_match_t_genN_
struct t00_match_t;
typedef const struct t00_match_t* _Ngen_nlang_builtins_ref__t00_match_t_genN_;
#endif // HAS2__Ngen_nlang_builtins_ref__t00_match_t_genN_
static nlang_builtins_bool t00_match_bor(_Ngen_nlang_builtins_ref__t00_match_t_genN_ xt) __attribute__((__nonnull__(1)));

#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/match.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

static nlang_builtins_i32 t00_match_foo(t00_match_enu n) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
switch (n) {
case t00_match_enu_A_tag_label__:
 {
nlang_builtins_i32 _Ngensym0 = (nlang_builtins_i32)0;
;
return _Ngensym0;
}
break;
case t00_match_enu_B_tag_label__:
default:
 {
nlang_builtins_i32 _Ngensym1 = (nlang_builtins_i32)1;
;
return _Ngensym1;
}
break;
};
}
return _nretval;
}

static nlang_builtins_i32 t00_match_bar(t00_match_enu n) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
switch (n) {
case t00_match_enu_A_tag_label__:
 {
nlang_builtins_i32 _Ngensym2 = (nlang_builtins_i32)0;
;
return _Ngensym2;
}
break;
default:
 {
nlang_builtins_i32 _Ngensym3 = (nlang_builtins_i32)1;
;
return _Ngensym3;
}
break;
};
}
return _nretval;
}

#ifndef HAS3__Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_
#define HAS3__Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_
struct t00_match_t;
typedef struct t00_match_t* _Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_;
#endif // HAS3__Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_
nlang_builtins_void t00_match_t_ctor(_Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_ self) {
#define THIS(x) t00_match_t##x
#undef THIS
}
;
void t00_match_t_mk(t00_match_t *_nrtr_r) {
#define THIS(x) t00_match_t##x
#define r (*_nrtr_r)
#undef r
#undef THIS
}
;
_Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_ t00_match_t_new(void) {
#define THIS(x) t00_match_t##x
__attribute__((__unused__)) _Ngen_nlang_builtins_mercurial_ref__t00_match_t_genN_ _nretval = { 0 };
return calloc(1, sizeof(THIS()));
return _nretval;
#undef THIS
}
;

#ifndef HAS3__Ngen_nlang_builtins_ref__t00_match_t_genN_
#define HAS3__Ngen_nlang_builtins_ref__t00_match_t_genN_
struct t00_match_t;
typedef const struct t00_match_t* _Ngen_nlang_builtins_ref__t00_match_t_genN_;
#endif // HAS3__Ngen_nlang_builtins_ref__t00_match_t_genN_
static nlang_builtins_bool t00_match_bor(_Ngen_nlang_builtins_ref__t00_match_t_genN_ xt) {
__attribute__((__unused__)) nlang_builtins_bool _nretval = { 0 };
 {
;;
nlang_builtins_bool _Ngensym5 = ((xt->x) == t00_match_enu_A);
;
return _Ngensym5;
}
return _nretval;
}

nlang_builtins_i32 _Nmain(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
;;
t00_match_t xt= { 0 };
xt.x = t00_match_enu_A;
;
;
;
( (({ _Ngen_nlang_builtins_ref__t00_match_t_genN_ _Ngensym14 = ((&xt));
;
nlang_builtins_bool _Ngensym6 = t00_match_bor(_Ngensym14);
;
nlang_builtins_bool _Ngensym7 = (!_Ngensym6);
;
_Ngensym7;
; })) ? ( {
nlang_builtins_i32 _Ngensym8 = (nlang_builtins_i32)1;
;
return _Ngensym8;
})
 : ( {
;;
}));
;;
t00_match_enu n = (t00_match_enu_A);
;
( (({ ;;
nlang_builtins_bool _Ngensyma = (n != (xt.x));
;
_Ngensyma;
; })) ? ( {
nlang_builtins_i32 _Ngensymb = (nlang_builtins_i32)1;
;
return _Ngensymb;
})
 : ( {
;;
}));
;;
(void) t00_match_foo(n);
;;
(void) t00_match_bar(t00_match_enu_A);
switch (n) {
case t00_match_enu_A_tag_label__:
 {
nlang_builtins_i32 _Ngensymc = (nlang_builtins_i32)0;
;
return _Ngensymc;
}
break;
case t00_match_enu_B_tag_label__:
default:
 {
nlang_builtins_i32 _Ngensymd = (nlang_builtins_i32)1;
;
return _Ngensymd;
}
break;
};
}
return _nretval;
}
void t00_match_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_match_Nrunexamples(void) {
}
