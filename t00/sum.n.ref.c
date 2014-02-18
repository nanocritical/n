#include <lib/nlang/runtime.h>
#define NLANG_DECLARE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_TYPES

#define NLANG_DECLARE_TYPES
# include "t00/sum.n.o.h"
#undef NLANG_DECLARE_TYPES
typedef nlang_builtins_u32 t00_sum_en;

typedef nlang_builtins_u32 t00_sum_hen;

typedef nlang_builtins_u8 t00_sum_un_tag_type;
#define t00_sum_un_A_tag_label__ ((nlang_builtins_u8)0)
static const t00_sum_un_tag_type t00_sum_un_A_tag = t00_sum_un_A_tag_label__;
struct t00_sum_un_A;
typedef struct t00_sum_un_A t00_sum_un_A;
#define t00_sum_un_B_tag_label__ (((nlang_builtins_u8)0 + (nlang_builtins_u8)1))
static const t00_sum_un_tag_type t00_sum_un_B_tag = t00_sum_un_B_tag_label__;
struct t00_sum_un_B;
typedef struct t00_sum_un_B t00_sum_un_B;
#define t00_sum_un_C_tag_label__ ((nlang_builtins_u8)2)
static const t00_sum_un_tag_type t00_sum_un_C_tag = t00_sum_un_C_tag_label__;
struct t00_sum_un_C;
typedef struct t00_sum_un_C t00_sum_un_C;
union t00_sum_un_as_type;
struct t00_sum_un;
typedef struct t00_sum_un t00_sum_un;

typedef nlang_builtins_u32 t00_sum_hun_tag_type;
#define t00_sum_hun_A_tag_label__ ((nlang_builtins_u32)0)
static const t00_sum_hun_tag_type t00_sum_hun_A_tag = t00_sum_hun_A_tag_label__;
struct t00_sum_hun_A;
typedef struct t00_sum_hun_A t00_sum_hun_A;
#define t00_sum_hun_BA_tag_label__ (((nlang_builtins_u32)0 + (nlang_builtins_u32)1))
static const t00_sum_hun_tag_type t00_sum_hun_BA_tag = t00_sum_hun_BA_tag_label__;
struct t00_sum_hun_BA;
typedef struct t00_sum_hun_BA t00_sum_hun_BA;
#define t00_sum_hun_BB_tag_label__ ((((nlang_builtins_u32)0 + (nlang_builtins_u32)1) + (nlang_builtins_u32)1))
static const t00_sum_hun_tag_type t00_sum_hun_BB_tag = t00_sum_hun_BB_tag_label__;
struct t00_sum_hun_BB;
typedef struct t00_sum_hun_BB t00_sum_hun_BB;
#define t00_sum_hun_B_first_tag_label__ (((nlang_builtins_u32)0 + (nlang_builtins_u32)1))
static const t00_sum_hun_tag_type t00_sum_hun_B_first_tag = t00_sum_hun_B_first_tag_label__;
#define t00_sum_hun_B_last_tag_label__ (((nlang_builtins_u32)0 + (nlang_builtins_u32)1))
static const t00_sum_hun_tag_type t00_sum_hun_B_last_tag = t00_sum_hun_B_last_tag_label__;
union t00_sum_hun_B_as_type;
struct t00_sum_hun_B;
typedef struct t00_sum_hun_B t00_sum_hun_B;
union t00_sum_hun_as_type;
struct t00_sum_hun;
typedef struct t00_sum_hun t00_sum_hun;

#define NLANG_DEFINE_TYPES
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_TYPES

#define NLANG_DEFINE_TYPES
# include "t00/sum.n.o.h"
#undef NLANG_DEFINE_TYPES
#define t00_sum_en_A_label__ ((nlang_builtins_u32)0)
static const t00_sum_en t00_sum_en_A = t00_sum_en_A_label__;
#define t00_sum_en_B_label__ ((nlang_builtins_u32)3)
static const t00_sum_en t00_sum_en_B = t00_sum_en_B_label__;
#define t00_sum_en_C_label__ (((nlang_builtins_u32)3 + (nlang_builtins_u32)1))
static const t00_sum_en t00_sum_en_C = t00_sum_en_C_label__;

#define t00_sum_hen_A_label__ ((nlang_builtins_u32)0)
static const t00_sum_hen t00_sum_hen_A = t00_sum_hen_A_label__;
#define t00_sum_hen_B_first_tag_label__ (((nlang_builtins_u32)0 + (nlang_builtins_u32)1))
static const t00_sum_hen t00_sum_hen_B_first_tag = t00_sum_hen_B_first_tag_label__;
#define t00_sum_hen_B_last_tag_label__ (((((nlang_builtins_u32)0 + (nlang_builtins_u32)1) + (nlang_builtins_u32)1) + (nlang_builtins_u32)1))
static const t00_sum_hen t00_sum_hen_B_last_tag = t00_sum_hen_B_last_tag_label__;
#define t00_sum_hen_BA_label__ (((nlang_builtins_u32)0 + (nlang_builtins_u32)1))
static const t00_sum_hen t00_sum_hen_BA = t00_sum_hen_BA_label__;
#define t00_sum_hen_BB_label__ ((((nlang_builtins_u32)0 + (nlang_builtins_u32)1) + (nlang_builtins_u32)1))
static const t00_sum_hen t00_sum_hen_BB = t00_sum_hen_BB_label__;
#define t00_sum_hen_BC_label__ (((((nlang_builtins_u32)0 + (nlang_builtins_u32)1) + (nlang_builtins_u32)1) + (nlang_builtins_u32)1))
static const t00_sum_hen t00_sum_hen_BC = t00_sum_hen_BC_label__;
#define t00_sum_hen_C_label__ ((((nlang_builtins_u32)0 + (nlang_builtins_u32)1) + (nlang_builtins_u32)1))
static const t00_sum_hen t00_sum_hen_C = t00_sum_hen_C_label__;

struct t00_sum_un_A {
nlang_builtins_u8 _Nfiller;
};
struct t00_sum_un_B {
nlang_builtins_u32 x;
};
struct t00_sum_un_C {
t00_sum_en x;
};
union t00_sum_un_as_type {
t00_sum_un_A A;
t00_sum_un_B B;
t00_sum_un_C C;
};
struct t00_sum_un {
t00_sum_un_tag_type tag;
union t00_sum_un_as_type as;
};

struct t00_sum_hun_A {
nlang_builtins_u8 _Nfiller;
};
struct t00_sum_hun_BA {
nlang_builtins_u32 y;
nlang_builtins_u32 z;
};
struct t00_sum_hun_BB {
nlang_builtins_i64 z;
};
union t00_sum_hun_B_as_type {
t00_sum_hun_BA BA;
t00_sum_hun_BB BB;
};
struct t00_sum_hun_B {
nlang_builtins_u32 x;
union t00_sum_hun_B_as_type as;
};
union t00_sum_hun_as_type {
t00_sum_hun_A A;
t00_sum_hun_B B;
};
struct t00_sum_hun {
t00_sum_hun_tag_type tag;
union t00_sum_hun_as_type as;
};

#define NLANG_DECLARE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS

#define NLANG_DECLARE_FUNCTIONS
# include "t00/sum.n.o.h"
#undef NLANG_DECLARE_FUNCTIONS




#define NLANG_DEFINE_FUNCTIONS
# include "lib/nlang/module.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS

#define NLANG_DEFINE_FUNCTIONS
# include "t00/sum.n.o.h"
#undef NLANG_DEFINE_FUNCTIONS




nlang_builtins_i32 _Nmain(void) {
 {
t00_sum_en e = (t00_sum_en_A);
;
switch (e) {
case t00_sum_en_A_label__:
 {
;;
}
break;
default:
 {
return (nlang_builtins_i32)1;
}
break;
};
t00_sum_hen f = (t00_sum_hen_BA);
;
switch (f) {
case t00_sum_hen_BA_label__:
case t00_sum_hen_BB_label__:
case t00_sum_hen_BC_label__:
 {
;;
}
break;
default:
 {
return (nlang_builtins_i32)1;
}
break;
};
switch (f) {
case t00_sum_hen_BA_label__:
 {
;;
}
break;
default:
 {
return (nlang_builtins_i32)1;
}
break;
};
return (nlang_builtins_i32)0;
}
}
void t00_sum_Nrunexamples(void) __attribute__((section(".text.nlang.examples")));
void t00_sum_Nrunexamples(void) {
}
