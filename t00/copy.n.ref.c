#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/copy.n.o.h"
#undef NLANG_DECLARE_TYPES
struct t00_copy_copy;
typedef struct t00_copy_copy t00_copy_copy;
#ifndef HAS0__Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_
#define HAS0__Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_
struct t00_copy_copy;
typedef struct t00_copy_copy* _Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_;
#endif // HAS0__Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_
#ifndef HAS0__Ngen_nlang_builtins_ref__t00_copy_copy_genN_
#define HAS0__Ngen_nlang_builtins_ref__t00_copy_copy_genN_
struct t00_copy_copy;
typedef const struct t00_copy_copy* _Ngen_nlang_builtins_ref__t00_copy_copy_genN_;
#endif // HAS0__Ngen_nlang_builtins_ref__t00_copy_copy_genN_
;


#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t00/copy.n.o.h"
#undef NLANG_DEFINE_TYPES
struct t00_copy_copy {
nlang_builtins_i32 x;
}
;


#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/copy.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS
#ifndef HAS2__Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_
#define HAS2__Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_
struct t00_copy_copy;
typedef struct t00_copy_copy* _Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_;
#endif // HAS2__Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_
#ifndef HAS2__Ngen_nlang_builtins_ref__t00_copy_copy_genN_
#define HAS2__Ngen_nlang_builtins_ref__t00_copy_copy_genN_
struct t00_copy_copy;
typedef const struct t00_copy_copy* _Ngen_nlang_builtins_ref__t00_copy_copy_genN_;
#endif // HAS2__Ngen_nlang_builtins_ref__t00_copy_copy_genN_
nlang_builtins_void t00_copy_copy_copy_ctor(_Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_ self, _Ngen_nlang_builtins_ref__t00_copy_copy_genN_ other) __attribute__((__nonnull__(1, 2)));
;
nlang_builtins_void t00_copy_copy_ctor(_Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_ self) __attribute__((__nonnull__(1)));
;
void t00_copy_copy_mk(t00_copy_copy *_nrtr_r);
;
_Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_ t00_copy_copy_new(void);
;
static inline nlang_builtins__Ni_copyable t00_copy_copy_mkdyn__nlang_builtins__Ni_copyable(t00_copy_copy *obj);

static void t00_copy_foo(t00_copy_copy *_nrtr__nretval);

#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/copy.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS
#ifndef HAS3__Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_
#define HAS3__Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_
struct t00_copy_copy;
typedef struct t00_copy_copy* _Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_;
#endif // HAS3__Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_
#ifndef HAS3__Ngen_nlang_builtins_ref__t00_copy_copy_genN_
#define HAS3__Ngen_nlang_builtins_ref__t00_copy_copy_genN_
struct t00_copy_copy;
typedef const struct t00_copy_copy* _Ngen_nlang_builtins_ref__t00_copy_copy_genN_;
#endif // HAS3__Ngen_nlang_builtins_ref__t00_copy_copy_genN_
nlang_builtins_void t00_copy_copy_copy_ctor(_Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_ self, _Ngen_nlang_builtins_ref__t00_copy_copy_genN_ other) {
#define THIS(x) t00_copy_copy##x
 {
(self->x) = (other->x);
}
#undef THIS
}
;
nlang_builtins_void t00_copy_copy_ctor(_Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_ self) {
#define THIS(x) t00_copy_copy##x
#undef THIS
}
;
void t00_copy_copy_mk(t00_copy_copy *_nrtr_r) {
#define THIS(x) t00_copy_copy##x
#define r (*_nrtr_r)
#undef r
#undef THIS
}
;
_Ngen_nlang_builtins_mercurial_ref__t00_copy_copy_genN_ t00_copy_copy_new(void) {
#define THIS(x) t00_copy_copy##x
return calloc(1, sizeof(THIS()));
#undef THIS
}
;
static inline nlang_builtins__Ni_copyable t00_copy_copy_mkdyn__nlang_builtins__Ni_copyable(t00_copy_copy *obj) {
static const struct _Ndyn_nlang_builtins__Ni_copyable vtable = {
.copy_ctor = (nlang_builtins_void (*)(void *self, nlang_builtins__Ni_copyable other))t00_copy_copy_copy_ctor,
};
return (nlang_builtins__Ni_copyable){ .vptr = &vtable, .obj = obj };
}

static void t00_copy_foo(t00_copy_copy *_nrtr__nretval) {
#define _nretval (*_nrtr__nretval)
 {
_nretval.x = (nlang_builtins_i32)0;
;
return;
}
#undef _nretval
}

nlang_builtins_i32 _Nmain(void) {
 {
t00_copy_copy w = { 0 };
t00_copy_foo(&(w));
;
t00_copy_copy y= { 0 };
y.x = (-(nlang_builtins_i32)1);
;
;
t00_copy_copy z = { 0 };
t00_copy_copy_copy_ctor(((&z)), ((&y)));
;
t00_copy_copy zz = { 0 };
t00_copy_copy_copy_ctor(((&zz)), ((&z)));
;
return ((z.x) + (nlang_builtins_i32)1);
}
}
void t00_copy_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_copy_Nrunexamples(void) {
}
