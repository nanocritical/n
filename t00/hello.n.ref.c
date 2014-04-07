#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/hello.n.o.h"
#undef NLANG_DECLARE_TYPES
#ifndef HAS0__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
#define HAS0__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
struct nlang_strings_string;
typedef const struct nlang_strings_string* _Ngen_nlang_builtins_ref__nlang_strings_string_genN_;
#endif // HAS0__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
#ifndef HAS0__Ngen_nlang_builtins_ref__nlang_builtins_float_genN_
#define HAS0__Ngen_nlang_builtins_ref__nlang_builtins_float_genN_
typedef const nlang_builtins_float* _Ngen_nlang_builtins_ref__nlang_builtins_float_genN_;
#endif // HAS0__Ngen_nlang_builtins_ref__nlang_builtins_float_genN_
#ifndef HAS0_nlang_chars__Ni_show
#define HAS0_nlang_chars__Ni_show
#endif // HAS0_nlang_chars__Ni_show
#ifndef HAS0__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
#define HAS0__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
typedef const nlang_builtins_u32* _Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_;
#endif // HAS0__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t00/hello.n.o.h"
#undef NLANG_DEFINE_TYPES
#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/hello.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS
#ifndef HAS2__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
#define HAS2__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
struct nlang_strings_string;
typedef const struct nlang_strings_string* _Ngen_nlang_builtins_ref__nlang_strings_string_genN_;
#endif // HAS2__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
#ifndef HAS2__Ngen_nlang_builtins_ref__nlang_builtins_float_genN_
#define HAS2__Ngen_nlang_builtins_ref__nlang_builtins_float_genN_
typedef const nlang_builtins_float* _Ngen_nlang_builtins_ref__nlang_builtins_float_genN_;
#endif // HAS2__Ngen_nlang_builtins_ref__nlang_builtins_float_genN_
#ifndef HAS2_nlang_chars__Ni_show
#define HAS2_nlang_chars__Ni_show
#endif // HAS2_nlang_chars__Ni_show
#ifndef HAS2__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
#define HAS2__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
typedef const nlang_builtins_u32* _Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_;
#endif // HAS2__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/hello.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS
#ifndef HAS3__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
#define HAS3__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
struct nlang_strings_string;
typedef const struct nlang_strings_string* _Ngen_nlang_builtins_ref__nlang_strings_string_genN_;
#endif // HAS3__Ngen_nlang_builtins_ref__nlang_strings_string_genN_
#ifndef HAS3__Ngen_nlang_builtins_ref__nlang_builtins_float_genN_
#define HAS3__Ngen_nlang_builtins_ref__nlang_builtins_float_genN_
typedef const nlang_builtins_float* _Ngen_nlang_builtins_ref__nlang_builtins_float_genN_;
#endif // HAS3__Ngen_nlang_builtins_ref__nlang_builtins_float_genN_
#ifndef HAS3_nlang_chars__Ni_show
#define HAS3_nlang_chars__Ni_show
#endif // HAS3_nlang_chars__Ni_show
#ifndef HAS3__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
#define HAS3__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
typedef const nlang_builtins_u32* _Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_;
#endif // HAS3__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
nlang_builtins_i32 _Nmain(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_chars_static_string _Ngensym0 = nlang_chars_static_string_mk((const nlang_builtins_u8 *)"Hello, World!", sizeof("Hello, World!")-1);
;
_Ngen_nlang_builtins_nullable_ref__nlang_strings_string_genN_ _Ngensym9 = NULL;
;
_Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_ _Ngensyma = ((&_Ngensym0));
;
;;
nlang_chars__Ni_show _Ngensym14 = nlang_chars_static_string_mkdyn__nlang_chars__Ni_show((void *)_Ngensyma);
;
nlang_say(_Ngensym9, 1, _Ngensym14);
;
nlang_strings_string _Ngensym2 = { 0 };
nlang_strings_string_from_static_string(nlang_chars_static_string_mk((const nlang_builtins_u8 *)", ", sizeof(", ")-1), &(_Ngensym2));
;
nlang_chars_static_string _Ngensym3 = nlang_chars_static_string_mk((const nlang_builtins_u8 *)"Hello", sizeof("Hello")-1);
;
nlang_chars_static_string _Ngensym4 = nlang_chars_static_string_mk((const nlang_builtins_u8 *)"World!", sizeof("World!")-1);
;
_Ngen_nlang_builtins_ref__nlang_strings_string_genN_ _Ngensymb = ((&_Ngensym2));
;
_Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_ _Ngensymc = ((&_Ngensym3));
;
_Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_ _Ngensymd = ((&_Ngensym4));
;
nlang_chars__Ni_show _Ngensym15 = nlang_chars_static_string_mkdyn__nlang_chars__Ni_show((void *)_Ngensymc);
;
nlang_chars__Ni_show _Ngensym16 = nlang_chars_static_string_mkdyn__nlang_chars__Ni_show((void *)_Ngensymd);
;
nlang_say(_Ngensymb, 2, _Ngensym15, _Ngensym16);
nlang_builtins_bool _Ngensym5 = (nlang_builtins_bool)1;
;
_Ngen_nlang_builtins_nullable_ref__nlang_strings_string_genN_ _Ngensyme = NULL;
;
_Ngen_nlang_builtins_ref__nlang_builtins_bool_genN_ _Ngensymf = ((&_Ngensym5));
;
nlang_chars__Ni_show _Ngensym17 = nlang_builtins_bool_mkdyn__nlang_chars__Ni_show((void *)_Ngensymf);
;
nlang_say(_Ngensyme, 1, _Ngensym17);
nlang_builtins_u32 _Ngensym6 = (nlang_builtins_u32)1;
;
_Ngen_nlang_builtins_nullable_ref__nlang_strings_string_genN_ _Ngensym10 = NULL;
;
_Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_ _Ngensym11 = ((&_Ngensym6));
;
nlang_chars__Ni_show _Ngensym18 = nlang_builtins_u32_mkdyn__nlang_chars__Ni_show((void *)_Ngensym11);
;
nlang_say(_Ngensym10, 1, _Ngensym18);
nlang_builtins_float _Ngensym7 = (nlang_builtins_float)1.0;
;
_Ngen_nlang_builtins_nullable_ref__nlang_strings_string_genN_ _Ngensym12 = NULL;
;
_Ngen_nlang_builtins_ref__nlang_builtins_float_genN_ _Ngensym13 = ((&_Ngensym7));
;
nlang_chars__Ni_show _Ngensym19 = nlang_builtins_float_mkdyn__nlang_chars__Ni_show((void *)_Ngensym13);
;
nlang_say(_Ngensym12, 1, _Ngensym19);
nlang_builtins_i32 _Ngensym8 = (nlang_builtins_i32)0;
;
return _Ngensym8;
}
return _nretval;
}
void t00_hello_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_hello_Nrunexamples(void) {
}
