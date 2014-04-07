#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "lib/nlang/strings.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t02/string.n.o.h"
#undef NLANG_DECLARE_TYPES

#ifndef HAS0__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
#define HAS0__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
struct nlang_strings_string;
typedef const struct nlang_strings_string* _Ngen_nlang_builtins_ref__nlang_strings_string_genN_;
#endif // HAS0__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
#ifndef HAS0_nlang_chars__Ni_show
#define HAS0_nlang_chars__Ni_show
#endif // HAS0_nlang_chars__Ni_show
#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "lib/nlang/strings.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t02/string.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/strings.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t02/string.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS
static void t02_string_foo(nlang_strings_string *_nrtr__nretval);

#ifndef HAS2__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
#define HAS2__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
struct nlang_strings_string;
typedef const struct nlang_strings_string* _Ngen_nlang_builtins_ref__nlang_strings_string_genN_;
#endif // HAS2__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
#ifndef HAS2_nlang_chars__Ni_show
#define HAS2_nlang_chars__Ni_show
#endif // HAS2_nlang_chars__Ni_show
#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/strings.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t02/string.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS
static void t02_string_foo(nlang_strings_string *_nrtr__nretval) {
#define _nretval (*_nrtr__nretval)
 {
nlang_strings_string _Ngensym0 = { 0 };
nlang_strings_string_from_static_string(nlang_chars_static_string_mk((const nlang_builtins_u8 *)"test", sizeof("test")-1), &(_Ngensym0));
;
_nretval = _Ngensym0;
return;
}
#undef _nretval
}

#ifndef HAS3__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
#define HAS3__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
struct nlang_strings_string;
typedef const struct nlang_strings_string* _Ngen_nlang_builtins_ref__nlang_strings_string_genN_;
#endif // HAS3__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
#ifndef HAS3_nlang_chars__Ni_show
#define HAS3_nlang_chars__Ni_show
#endif // HAS3_nlang_chars__Ni_show
nlang_builtins_i32 _Nmain(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_chars_static_string _Ngensym1 = nlang_chars_static_string_mk((const nlang_builtins_u8 *)"test", sizeof("test")-1);
;
_Ngen_nlang_builtins_nullable_ref__nlang_strings_string_genN_ _Ngensym4 = NULL;
;
_Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_ _Ngensym5 = ((&_Ngensym1));
;
;;
nlang_chars__Ni_show _Ngensym9 = nlang_chars_static_string_mkdyn__nlang_chars__Ni_show((void *)_Ngensym5);
;
nlang_say(_Ngensym4, 1, _Ngensym9);
;
nlang_strings_string _Ngensym6 = { 0 };
t02_string_foo(&(_Ngensym6));
;
_Ngen_nlang_builtins_nullable_ref__nlang_strings_string_genN_ _Ngensym7 = NULL;
;
_Ngen_nlang_builtins_ref__nlang_strings_string_genN_ _Ngensym8 = ((&_Ngensym6));
;
nlang_chars__Ni_show _Ngensyma = nlang_strings_string_mkdyn__nlang_chars__Ni_show((void *)_Ngensym8);
;
nlang_say(_Ngensym7, 1, _Ngensyma);
nlang_builtins_i32 _Ngensym3 = (nlang_builtins_i32)0;
;
return _Ngensym3;
}
return _nretval;
}
void t02_string_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t02_string_Nrunexamples(void) {
}
