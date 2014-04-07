#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t03/constraints.n.o.h"
#undef NLANG_DECLARE_TYPES
typedef nlang_builtins_u32 t03_constraints_u_tag_type;
#define t03_constraints_u_A_tag_label__ ((nlang_builtins_u32)0)
static const t03_constraints_u_tag_type t03_constraints_u_A_tag = t03_constraints_u_A_tag_label__;
struct t03_constraints_u_A;
typedef struct t03_constraints_u_A t03_constraints_u_A;
#define t03_constraints_u_B_tag_label__ (((nlang_builtins_u32)0 + (nlang_builtins_u32)1))
static const t03_constraints_u_tag_type t03_constraints_u_B_tag = t03_constraints_u_B_tag_label__;
struct t03_constraints_u_B;
typedef struct t03_constraints_u_B t03_constraints_u_B;
#define t03_constraints_u_C_tag_label__ ((((nlang_builtins_u32)0 + (nlang_builtins_u32)1) + (nlang_builtins_u32)1))
static const t03_constraints_u_tag_type t03_constraints_u_C_tag = t03_constraints_u_C_tag_label__;
struct t03_constraints_u_C;
typedef struct t03_constraints_u_C t03_constraints_u_C;
union t03_constraints_u_as_type;
struct t03_constraints_u;
typedef struct t03_constraints_u t03_constraints_u;


#ifndef HAS0__Ngen_nlang_builtins_mutable_ref__t03_constraints_u_genN_
#define HAS0__Ngen_nlang_builtins_mutable_ref__t03_constraints_u_genN_
struct t03_constraints_u;
typedef struct t03_constraints_u* _Ngen_nlang_builtins_mutable_ref__t03_constraints_u_genN_;
#endif // HAS0__Ngen_nlang_builtins_mutable_ref__t03_constraints_u_genN_

#ifndef HAS0_nlang_chars__Ni_show
#define HAS0_nlang_chars__Ni_show
#endif // HAS0_nlang_chars__Ni_show
#ifndef HAS0__Ngen_nlang_builtins_ref__nlang_builtins_i32_genN_
#define HAS0__Ngen_nlang_builtins_ref__nlang_builtins_i32_genN_
typedef const nlang_builtins_i32* _Ngen_nlang_builtins_ref__nlang_builtins_i32_genN_;
#endif // HAS0__Ngen_nlang_builtins_ref__nlang_builtins_i32_genN_
#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t03/constraints.n.o.h"
#undef NLANG_DEFINE_TYPES
struct t03_constraints_u_A {
nlang_builtins_u8 _Nfiller;
};
struct t03_constraints_u_B {
nlang_builtins_i32 x;
};
struct t03_constraints_u_C {
nlang_builtins_u32 x;
};
union t03_constraints_u_as_type {
t03_constraints_u_A A;
t03_constraints_u_B B;
t03_constraints_u_C C;
};
struct t03_constraints_u {
t03_constraints_u_tag_type tag;
union t03_constraints_u_as_type as;
};



#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t03/constraints.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

static nlang_builtins_i32 t03_constraints_aux(t03_constraints_u e);

#ifndef HAS2__Ngen_nlang_builtins_mutable_ref__t03_constraints_u_genN_
#define HAS2__Ngen_nlang_builtins_mutable_ref__t03_constraints_u_genN_
struct t03_constraints_u;
typedef struct t03_constraints_u* _Ngen_nlang_builtins_mutable_ref__t03_constraints_u_genN_;
#endif // HAS2__Ngen_nlang_builtins_mutable_ref__t03_constraints_u_genN_
static nlang_builtins_void t03_constraints_set1(_Ngen_nlang_builtins_mutable_ref__t03_constraints_u_genN_ e) __attribute__((__nonnull__(1)));

#ifndef HAS2_nlang_chars__Ni_show
#define HAS2_nlang_chars__Ni_show
#endif // HAS2_nlang_chars__Ni_show
#ifndef HAS2__Ngen_nlang_builtins_ref__nlang_builtins_i32_genN_
#define HAS2__Ngen_nlang_builtins_ref__nlang_builtins_i32_genN_
typedef const nlang_builtins_i32* _Ngen_nlang_builtins_ref__nlang_builtins_i32_genN_;
#endif // HAS2__Ngen_nlang_builtins_ref__nlang_builtins_i32_genN_
#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t03/constraints.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

static nlang_builtins_i32 t03_constraints_aux(t03_constraints_u e) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
switch (e.tag) {
case t03_constraints_u_B_tag_label__:
 {
;
;;
;;
return ((e.as.B).x);
}
break;
default:
 {
nlang_builtins_i32 _Ngensym2 = (nlang_builtins_i32)1;
;
return _Ngensym2;
}
break;
};
;
}
return _nretval;
}

#ifndef HAS3__Ngen_nlang_builtins_mutable_ref__t03_constraints_u_genN_
#define HAS3__Ngen_nlang_builtins_mutable_ref__t03_constraints_u_genN_
struct t03_constraints_u;
typedef struct t03_constraints_u* _Ngen_nlang_builtins_mutable_ref__t03_constraints_u_genN_;
#endif // HAS3__Ngen_nlang_builtins_mutable_ref__t03_constraints_u_genN_
static nlang_builtins_void t03_constraints_set1(_Ngen_nlang_builtins_mutable_ref__t03_constraints_u_genN_ e) {
 {
switch (({ t03_constraints_u _Ngensym3 = ((*e));
;
_Ngensym3;
; }).tag) {
case t03_constraints_u_B_tag_label__:
 {
;
;;
;;
nlang_builtins_i32 _Ngensym6 = (nlang_builtins_i32)1;
;
((e->as.B).x) = _Ngensym6;
}
break;
default:
 {
;;
}
break;
};
;
}
}

#ifndef HAS3_nlang_chars__Ni_show
#define HAS3_nlang_chars__Ni_show
#endif // HAS3_nlang_chars__Ni_show
#ifndef HAS3__Ngen_nlang_builtins_ref__nlang_builtins_i32_genN_
#define HAS3__Ngen_nlang_builtins_ref__nlang_builtins_i32_genN_
typedef const nlang_builtins_i32* _Ngen_nlang_builtins_ref__nlang_builtins_i32_genN_;
#endif // HAS3__Ngen_nlang_builtins_ref__nlang_builtins_i32_genN_
nlang_builtins_i32 _Nmain(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
;;
t03_constraints_u g= { 0 };
g.tag = t03_constraints_u_A_tag;
;
;
;
switch (g.tag) {
case t03_constraints_u_B_tag_label__:
 {
nlang_builtins_i32 _Ngensym7 = (nlang_builtins_i32)1;
;
return _Ngensym7;
}
break;
case t03_constraints_u_C_tag_label__:
 {
nlang_builtins_i32 _Ngensym8 = (nlang_builtins_i32)1;
;
return _Ngensym8;
}
break;
default:
 {
;;
}
break;
};
;;
t03_constraints_u f= { 0 };
f.tag = t03_constraints_u_B_tag;
;
;
;;
;;
_Ngen_nlang_builtins_nullable_ref__nlang_strings_string_genN_ _Ngensym1b = NULL;
;
_Ngen_nlang_builtins_ref__nlang_builtins_i32_genN_ _Ngensym1c = ((&((f.as.B).x)));
;
nlang_chars__Ni_show _Ngensym21 = nlang_builtins_i32_mkdyn__nlang_chars__Ni_show((void *)_Ngensym1c);
;
nlang_say(_Ngensym1b, 1, _Ngensym21);
_Ngen_nlang_builtins_mutable_ref__t03_constraints_u_genN_ _Ngensymb = ((&f));
;
t03_constraints_set1(_Ngensymb);
;;
;;
_Ngen_nlang_builtins_nullable_ref__nlang_strings_string_genN_ _Ngensym1d = NULL;
;
_Ngen_nlang_builtins_ref__nlang_builtins_i32_genN_ _Ngensym1e = ((&((f.as.B).x)));
;
nlang_chars__Ni_show _Ngensym22 = nlang_builtins_i32_mkdyn__nlang_chars__Ni_show((void *)_Ngensym1e);
;
nlang_say(_Ngensym1d, 1, _Ngensym22);
;;
nlang_builtins_i32 _Ngensyme = (nlang_builtins_i32)1;
;
t03_constraints_u e= { 0 };
e.tag = t03_constraints_u_B_tag;
e.as.B.x = _Ngensyme;
;
;
switch (e.tag) {
case t03_constraints_u_B_tag_label__:
default:
 {
;
;;
;;
nlang_builtins_i32 _Ngensym11 = (nlang_builtins_i32)0;
;
nlang_builtins_bool _Ngensym12 = (((e.as.B).x) == _Ngensym11);
;
;;
nlang_builtins_assert(_Ngensym12);
}
break;
};
;
nlang_builtins_i32 _Ngensym14 = t03_constraints_aux(e);
;
return _Ngensym14;
}
return _nretval;
}
void t03_constraints_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t03_constraints_Nrunexamples(void) {
}
