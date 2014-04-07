#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/return.n.o.h"
#undef NLANG_DECLARE_TYPES
struct t00_return_copy;
typedef struct t00_return_copy t00_return_copy;
#ifndef HAS0__Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_
#define HAS0__Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_
struct t00_return_copy;
typedef struct t00_return_copy* _Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_;
#endif // HAS0__Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_
;



#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t00/return.n.o.h"
#undef NLANG_DEFINE_TYPES
struct t00_return_copy {
nlang_builtins_i32 x;
}
;



#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/return.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS
#ifndef HAS2__Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_
#define HAS2__Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_
struct t00_return_copy;
typedef struct t00_return_copy* _Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_;
#endif // HAS2__Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_
nlang_builtins_void t00_return_copy_ctor(_Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_ self) __attribute__((__nonnull__(1)));
;
t00_return_copy t00_return_copy_mk(void);
;
_Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_ t00_return_copy_new(void);
;
static inline nlang_builtins__Ni_return_by_copy t00_return_copy_mkdyn__nlang_builtins__Ni_return_by_copy(t00_return_copy *obj);

static t00_return_copy t00_return_foo(nlang_builtins_i32 x);

static t00_return_copy t00_return_chained(nlang_builtins_i32 x);

#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/return.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS
#ifndef HAS3__Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_
#define HAS3__Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_
struct t00_return_copy;
typedef struct t00_return_copy* _Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_;
#endif // HAS3__Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_
nlang_builtins_void t00_return_copy_ctor(_Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_ self) {
#define THIS(x) t00_return_copy##x
#undef THIS
}
;
t00_return_copy t00_return_copy_mk(void) {
#define THIS(x) t00_return_copy##x
__attribute__((__unused__)) t00_return_copy r = { 0 };
return (THIS()){ 0 };
return r;
#undef THIS
}
;
_Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_ t00_return_copy_new(void) {
#define THIS(x) t00_return_copy##x
__attribute__((__unused__)) _Ngen_nlang_builtins_mercurial_ref__t00_return_copy_genN_ _nretval = { 0 };
return calloc(1, sizeof(THIS()));
return _nretval;
#undef THIS
}
;
static inline nlang_builtins__Ni_return_by_copy t00_return_copy_mkdyn__nlang_builtins__Ni_return_by_copy(t00_return_copy *obj) {
static const struct _Ndyn_nlang_builtins__Ni_return_by_copy vtable = {
0,
};
return (nlang_builtins__Ni_return_by_copy){ .vptr = &vtable, .obj = obj };
}

static t00_return_copy t00_return_foo(nlang_builtins_i32 x) {
__attribute__((__unused__)) t00_return_copy n = { 0 };
 {
;;
(n.x) = x;
;
return n;
}
return n;
}

static t00_return_copy t00_return_chained(nlang_builtins_i32 x) {
__attribute__((__unused__)) t00_return_copy n = { 0 };
 {
t00_return_copy _Ngensym1 = t00_return_foo(x);
;
return _Ngensym1;
}
return n;
}

nlang_builtins_i32 _Nmain(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_builtins_i32 _Ngensym2 = (nlang_builtins_i32)1;
;
t00_return_copy _Ngensym3 = t00_return_chained(_Ngensym2);
;
;;
nlang_builtins_i32 _Ngensym5 = (nlang_builtins_i32)1;
;
nlang_builtins_i32 _Ngensym6 = ((_Ngensym3.x) - _Ngensym5);
;
return _Ngensym6;
}
return _nretval;
}
void t00_return_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_return_Nrunexamples(void) {
}
