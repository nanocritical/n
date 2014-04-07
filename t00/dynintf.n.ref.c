#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/dynintf.n.o.h"
#undef NLANG_DECLARE_TYPES
struct _Ndyn_t00_dynintf__Ni_d;
struct t00_dynintf__Ni_d;
typedef struct t00_dynintf__Ni_d t00_dynintf__Ni_d;

struct t00_dynintf_d;
typedef struct t00_dynintf_d t00_dynintf_d;
#ifndef HAS0__Ngen_nlang_builtins_ref__t00_dynintf_d_genN_
#define HAS0__Ngen_nlang_builtins_ref__t00_dynintf_d_genN_
struct t00_dynintf_d;
typedef const struct t00_dynintf_d* _Ngen_nlang_builtins_ref__t00_dynintf_d_genN_;
#endif // HAS0__Ngen_nlang_builtins_ref__t00_dynintf_d_genN_
;
#ifndef HAS0__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_
#define HAS0__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_
struct t00_dynintf_d;
typedef struct t00_dynintf_d* _Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_;
#endif // HAS0__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_
;

struct t00_dynintf_dc;
typedef struct t00_dynintf_dc t00_dynintf_dc;
#ifndef HAS0__Ngen_nlang_builtins_ref__t00_dynintf_dc_genN_
#define HAS0__Ngen_nlang_builtins_ref__t00_dynintf_dc_genN_
struct t00_dynintf_dc;
typedef const struct t00_dynintf_dc* _Ngen_nlang_builtins_ref__t00_dynintf_dc_genN_;
#endif // HAS0__Ngen_nlang_builtins_ref__t00_dynintf_dc_genN_
;
#ifndef HAS0__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_
#define HAS0__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_
struct t00_dynintf_dc;
typedef struct t00_dynintf_dc* _Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_;
#endif // HAS0__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_
;

#ifndef HAS0_t00_dynintf__Ni_d
#define HAS0_t00_dynintf__Ni_d
#endif // HAS0_t00_dynintf__Ni_d




#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t00/dynintf.n.o.h"
#undef NLANG_DEFINE_TYPES
struct _Ndyn_t00_dynintf__Ni_d {
nlang_builtins_i32 (*answer)(void);
nlang_builtins_i32 (*get)(void *self);
};
struct t00_dynintf__Ni_d {
const struct _Ndyn_t00_dynintf__Ni_d *vptr;
void *obj;
};

struct t00_dynintf_d {
nlang_builtins_i32 x;
}
;

struct t00_dynintf_dc {
nlang_builtins_i32 dummy;
}
;





#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/dynintf.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS
nlang_builtins_i32 t00_dynintf__Ni_d_answer(t00_dynintf__Ni_d self);
nlang_builtins_i32 t00_dynintf__Ni_d_get(t00_dynintf__Ni_d self);

nlang_builtins_i32 t00_dynintf_d_answer(void);
;
#ifndef HAS2__Ngen_nlang_builtins_ref__t00_dynintf_d_genN_
#define HAS2__Ngen_nlang_builtins_ref__t00_dynintf_d_genN_
struct t00_dynintf_d;
typedef const struct t00_dynintf_d* _Ngen_nlang_builtins_ref__t00_dynintf_d_genN_;
#endif // HAS2__Ngen_nlang_builtins_ref__t00_dynintf_d_genN_
nlang_builtins_i32 t00_dynintf_d_get(_Ngen_nlang_builtins_ref__t00_dynintf_d_genN_ self) __attribute__((__nonnull__(1)));
;
#ifndef HAS2__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_
#define HAS2__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_
struct t00_dynintf_d;
typedef struct t00_dynintf_d* _Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_;
#endif // HAS2__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_
nlang_builtins_void t00_dynintf_d_ctor(_Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_ self) __attribute__((__nonnull__(1)));
;
void t00_dynintf_d_mk(t00_dynintf_d *_nrtr_r);
;
_Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_ t00_dynintf_d_new(void);
;
static inline t00_dynintf__Ni_d t00_dynintf_d_mkdyn__t00_dynintf__Ni_d(t00_dynintf_d *obj);

nlang_builtins_i32 t00_dynintf_dc_answer(void);
;
#ifndef HAS2__Ngen_nlang_builtins_ref__t00_dynintf_dc_genN_
#define HAS2__Ngen_nlang_builtins_ref__t00_dynintf_dc_genN_
struct t00_dynintf_dc;
typedef const struct t00_dynintf_dc* _Ngen_nlang_builtins_ref__t00_dynintf_dc_genN_;
#endif // HAS2__Ngen_nlang_builtins_ref__t00_dynintf_dc_genN_
nlang_builtins_i32 t00_dynintf_dc_get(_Ngen_nlang_builtins_ref__t00_dynintf_dc_genN_ self) __attribute__((__nonnull__(1)));
;
#ifndef HAS2__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_
#define HAS2__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_
struct t00_dynintf_dc;
typedef struct t00_dynintf_dc* _Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_;
#endif // HAS2__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_
nlang_builtins_void t00_dynintf_dc_ctor(_Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_ self) __attribute__((__nonnull__(1)));
;
void t00_dynintf_dc_mk(t00_dynintf_dc *_nrtr_r);
;
_Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_ t00_dynintf_dc_new(void);
;
static inline t00_dynintf__Ni_d t00_dynintf_dc_mkdyn__t00_dynintf__Ni_d(t00_dynintf_dc *obj);

#ifndef HAS2_t00_dynintf__Ni_d
#define HAS2_t00_dynintf__Ni_d
#endif // HAS2_t00_dynintf__Ni_d
static nlang_builtins_i32 t00_dynintf_foo(t00_dynintf__Ni_d v);

void t00_dynintf__Nexample0(void) __attribute__((section(".text.nlang.examples")));
static nlang_builtins_i32 t00_dynintf_rfoo(t00_dynintf__Ni_d pv);

static nlang_builtins_i32 t00_dynintf_bar(_Ngen_nlang_builtins_ref__t00_dynintf_d_genN_ pv) __attribute__((__nonnull__(1)));

#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/dynintf.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS
nlang_builtins_i32 t00_dynintf__Ni_d_answer(t00_dynintf__Ni_d self) {
return self.vptr->answer();
}
nlang_builtins_i32 t00_dynintf__Ni_d_get(t00_dynintf__Ni_d self) {
return self.vptr->get(self.obj);
}

nlang_builtins_i32 t00_dynintf_d_answer(void) {
#define THIS(x) t00_dynintf_d##x
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_builtins_i32 _Ngensym0 = (nlang_builtins_i32)42;
;
return _Ngensym0;
}
return _nretval;
#undef THIS
}
;
#ifndef HAS3__Ngen_nlang_builtins_ref__t00_dynintf_d_genN_
#define HAS3__Ngen_nlang_builtins_ref__t00_dynintf_d_genN_
struct t00_dynintf_d;
typedef const struct t00_dynintf_d* _Ngen_nlang_builtins_ref__t00_dynintf_d_genN_;
#endif // HAS3__Ngen_nlang_builtins_ref__t00_dynintf_d_genN_
nlang_builtins_i32 t00_dynintf_d_get(_Ngen_nlang_builtins_ref__t00_dynintf_d_genN_ self) {
#define THIS(x) t00_dynintf_d##x
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
;;
return (self->x);
}
return _nretval;
#undef THIS
}
;
#ifndef HAS3__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_
#define HAS3__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_
struct t00_dynintf_d;
typedef struct t00_dynintf_d* _Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_;
#endif // HAS3__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_
nlang_builtins_void t00_dynintf_d_ctor(_Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_ self) {
#define THIS(x) t00_dynintf_d##x
#undef THIS
}
;
void t00_dynintf_d_mk(t00_dynintf_d *_nrtr_r) {
#define THIS(x) t00_dynintf_d##x
#define r (*_nrtr_r)
#undef r
#undef THIS
}
;
_Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_ t00_dynintf_d_new(void) {
#define THIS(x) t00_dynintf_d##x
__attribute__((__unused__)) _Ngen_nlang_builtins_mercurial_ref__t00_dynintf_d_genN_ _nretval = { 0 };
return calloc(1, sizeof(THIS()));
return _nretval;
#undef THIS
}
;
static inline t00_dynintf__Ni_d t00_dynintf_d_mkdyn__t00_dynintf__Ni_d(t00_dynintf_d *obj) {
static const struct _Ndyn_t00_dynintf__Ni_d vtable = {
.answer = (nlang_builtins_i32 (*)(void))t00_dynintf_d_answer,
.get = (nlang_builtins_i32 (*)(void *self))t00_dynintf_d_get,
};
return (t00_dynintf__Ni_d){ .vptr = &vtable, .obj = obj };
}

nlang_builtins_i32 t00_dynintf_dc_answer(void) {
#define THIS(x) t00_dynintf_dc##x
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_builtins_i32 _Ngensym2 = (nlang_builtins_i32)42;
;
return _Ngensym2;
}
return _nretval;
#undef THIS
}
;
#ifndef HAS3__Ngen_nlang_builtins_ref__t00_dynintf_dc_genN_
#define HAS3__Ngen_nlang_builtins_ref__t00_dynintf_dc_genN_
struct t00_dynintf_dc;
typedef const struct t00_dynintf_dc* _Ngen_nlang_builtins_ref__t00_dynintf_dc_genN_;
#endif // HAS3__Ngen_nlang_builtins_ref__t00_dynintf_dc_genN_
nlang_builtins_i32 t00_dynintf_dc_get(_Ngen_nlang_builtins_ref__t00_dynintf_dc_genN_ self) {
#define THIS(x) t00_dynintf_dc##x
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_builtins_i32 _Ngensym3 = (nlang_builtins_i32)42;
;
return _Ngensym3;
}
return _nretval;
#undef THIS
}
;
#ifndef HAS3__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_
#define HAS3__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_
struct t00_dynintf_dc;
typedef struct t00_dynintf_dc* _Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_;
#endif // HAS3__Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_
nlang_builtins_void t00_dynintf_dc_ctor(_Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_ self) {
#define THIS(x) t00_dynintf_dc##x
#undef THIS
}
;
void t00_dynintf_dc_mk(t00_dynintf_dc *_nrtr_r) {
#define THIS(x) t00_dynintf_dc##x
#define r (*_nrtr_r)
#undef r
#undef THIS
}
;
_Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_ t00_dynintf_dc_new(void) {
#define THIS(x) t00_dynintf_dc##x
__attribute__((__unused__)) _Ngen_nlang_builtins_mercurial_ref__t00_dynintf_dc_genN_ _nretval = { 0 };
return calloc(1, sizeof(THIS()));
return _nretval;
#undef THIS
}
;
static inline t00_dynintf__Ni_d t00_dynintf_dc_mkdyn__t00_dynintf__Ni_d(t00_dynintf_dc *obj) {
static const struct _Ndyn_t00_dynintf__Ni_d vtable = {
.answer = (nlang_builtins_i32 (*)(void))t00_dynintf_dc_answer,
.get = (nlang_builtins_i32 (*)(void *self))t00_dynintf_dc_get,
};
return (t00_dynintf__Ni_d){ .vptr = &vtable, .obj = obj };
}

#ifndef HAS3_t00_dynintf__Ni_d
#define HAS3_t00_dynintf__Ni_d
#endif // HAS3_t00_dynintf__Ni_d
static nlang_builtins_i32 t00_dynintf_foo(t00_dynintf__Ni_d v) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_builtins_i32 _Ngensym4 = t00_dynintf__Ni_d_get(v);
;
return _Ngensym4;
}
return _nretval;
}

void t00_dynintf__Nexample0(void) {
({ nlang_builtins_bool _Ngensym9= { 0 };
;
;
 {
;;
nlang_builtins_i32 _Ngensym5 = (nlang_builtins_i32)42;
;
t00_dynintf_d x= { 0 };
x.x = _Ngensym5;
;
;
;
nlang_builtins_i32 _Ngensym6 = (nlang_builtins_i32)42;
;
_Ngen_nlang_builtins_ref__t00_dynintf_d_genN_ _Ngensym19 = ((&x));
;
t00_dynintf__Ni_d _Ngensym1e = t00_dynintf_d_mkdyn__t00_dynintf__Ni_d((void *)_Ngensym19);
;
nlang_builtins_i32 _Ngensym7 = t00_dynintf_foo(_Ngensym1e);
;
nlang_builtins_bool _Ngensym8 = (_Ngensym6 == _Ngensym7);
;
_Ngensym9 = _Ngensym8;
};
;;
nlang_builtins___example__(_Ngensym9);
; });
}
static nlang_builtins_i32 t00_dynintf_rfoo(t00_dynintf__Ni_d pv) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_builtins_i32 _Ngensymb = t00_dynintf__Ni_d_answer(*(t00_dynintf__Ni_d *)&pv);
;
return _Ngensymb;
}
return _nretval;
}

static nlang_builtins_i32 t00_dynintf_bar(_Ngen_nlang_builtins_ref__t00_dynintf_d_genN_ pv) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
t00_dynintf__Ni_d _Ngensym1f = t00_dynintf_d_mkdyn__t00_dynintf__Ni_d((void *)pv);
;
nlang_builtins_i32 _Ngensymc = t00_dynintf_foo(_Ngensym1f);
;
return _Ngensymc;
}
return _nretval;
}

nlang_builtins_i32 _Nmain(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
;;
nlang_builtins_i32 _Ngensymd = (nlang_builtins_i32)42;
;
t00_dynintf_d xd= { 0 };
xd.x = _Ngensymd;
;
;
;
;;
t00_dynintf_dc xdc= { 0 };
;
;
( (({ _Ngen_nlang_builtins_ref__t00_dynintf_d_genN_ _Ngensym1c = ((&xd));
;
t00_dynintf__Ni_d _Ngensym20 = t00_dynintf_d_mkdyn__t00_dynintf__Ni_d((void *)_Ngensym1c);
;
nlang_builtins_i32 _Ngensyme = t00_dynintf_foo(_Ngensym20);
;
_Ngen_nlang_builtins_ref__t00_dynintf_dc_genN_ _Ngensym1d = ((&xdc));
;
t00_dynintf__Ni_d _Ngensym21 = t00_dynintf_dc_mkdyn__t00_dynintf__Ni_d((void *)_Ngensym1d);
;
nlang_builtins_i32 _Ngensymf = t00_dynintf_foo(_Ngensym21);
;
nlang_builtins_bool _Ngensym10 = (_Ngensyme != _Ngensymf);
;
_Ngensym10;
; })) ? ( {
nlang_builtins_i32 _Ngensym11 = (nlang_builtins_i32)1;
;
return _Ngensym11;
})
 : ( {
;;
}));
;;
_Ngen_nlang_builtins_ref__t00_dynintf_d_genN_ pxd = ((&xd));
;
( (({ nlang_builtins_i32 _Ngensym12 = t00_dynintf_bar(pxd);
;
t00_dynintf__Ni_d _Ngensym22 = t00_dynintf_d_mkdyn__t00_dynintf__Ni_d((void *)pxd);
;
nlang_builtins_i32 _Ngensym13 = t00_dynintf_rfoo(_Ngensym22);
;
nlang_builtins_bool _Ngensym14 = (_Ngensym12 != _Ngensym13);
;
_Ngensym14;
; })) ? ( {
nlang_builtins_i32 _Ngensym15 = (nlang_builtins_i32)1;
;
return _Ngensym15;
})
 : ( {
;;
}));
nlang_builtins_i32 _Ngensym16 = (nlang_builtins_i32)0;
;
return _Ngensym16;
}
return _nretval;
}
void t00_dynintf_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_dynintf_Nrunexamples(void) {
t00_dynintf__Nexample0();
}
