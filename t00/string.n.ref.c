#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/string.n.o.h"
#undef NLANG_DECLARE_TYPES
#ifndef HAS0__Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_
#define HAS0__Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_
struct nlang_chars_static_string;
typedef const struct nlang_chars_static_string* _Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_;
#endif // HAS0__Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_
#ifndef HAS0__Ngen_nlang_builtins_ref__nlang_builtins_size_genN_
#define HAS0__Ngen_nlang_builtins_ref__nlang_builtins_size_genN_
typedef const nlang_builtins_size* _Ngen_nlang_builtins_ref__nlang_builtins_size_genN_;
#endif // HAS0__Ngen_nlang_builtins_ref__nlang_builtins_size_genN_
#ifndef HAS0__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
#define HAS0__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
typedef const nlang_builtins_u32* _Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_;
#endif // HAS0__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t00/string.n.o.h"
#undef NLANG_DEFINE_TYPES
#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/string.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS
#ifndef HAS2__Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_
#define HAS2__Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_
struct nlang_chars_static_string;
typedef const struct nlang_chars_static_string* _Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_;
#endif // HAS2__Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_
#ifndef HAS2__Ngen_nlang_builtins_ref__nlang_builtins_size_genN_
#define HAS2__Ngen_nlang_builtins_ref__nlang_builtins_size_genN_
typedef const nlang_builtins_size* _Ngen_nlang_builtins_ref__nlang_builtins_size_genN_;
#endif // HAS2__Ngen_nlang_builtins_ref__nlang_builtins_size_genN_
#ifndef HAS2__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
#define HAS2__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
typedef const nlang_builtins_u32* _Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_;
#endif // HAS2__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/string.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS
#ifndef HAS3__Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_
#define HAS3__Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_
struct nlang_chars_static_string;
typedef const struct nlang_chars_static_string* _Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_;
#endif // HAS3__Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_
#ifndef HAS3__Ngen_nlang_builtins_ref__nlang_builtins_size_genN_
#define HAS3__Ngen_nlang_builtins_ref__nlang_builtins_size_genN_
typedef const nlang_builtins_size* _Ngen_nlang_builtins_ref__nlang_builtins_size_genN_;
#endif // HAS3__Ngen_nlang_builtins_ref__nlang_builtins_size_genN_
#ifndef HAS3__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
#define HAS3__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
typedef const nlang_builtins_u32* _Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_;
#endif // HAS3__Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_
nlang_builtins_i32 _Nmain(void) {
__attribute__((__unused__)) nlang_builtins_i32 _nretval = { 0 };
 {
nlang_chars_static_string _Ngensym0 = nlang_chars_static_string_mk((const nlang_builtins_u8 *)"123", sizeof("123")-1);
;
_Ngen_nlang_builtins_ref__nlang_chars_static_string_genN_ _Ngensym6 = ((&_Ngensym0));
;
nlang_builtins_size _Ngensym1 = nlang_chars_static_string_count(_Ngensym6);
;
_Ngen_nlang_builtins_ref__nlang_builtins_size_genN_ _Ngensym7 = ((&_Ngensym1));
;
nlang_builtins_u32 _Ngensym2 = nlang_builtins_size_trim_u32(_Ngensym7);
;
_Ngen_nlang_builtins_ref__nlang_builtins_u32_genN_ _Ngensym8 = ((&_Ngensym2));
;
nlang_builtins_i32 _Ngensym3 = nlang_builtins_u32_reinterpret_signed(_Ngensym8);
;
nlang_builtins_i32 _Ngensym4 = (nlang_builtins_i32)3;
;
nlang_builtins_i32 _Ngensym5 = (_Ngensym3 - _Ngensym4);
;
return _Ngensym5;
}
return _nretval;
}
void t00_string_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_string_Nrunexamples(void) {
}
