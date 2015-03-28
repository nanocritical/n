#ifndef TYPES_H__
#define TYPES_H__

#include "nodes.h"

void print_statit_typs(void);

struct typ;

// Can only be used with final typs.
VECTOR(vectyp, struct typ *, 4);
IMPLEMENT_VECTOR(static inline, vectyp, struct typ *);

// Loose elements ordering.
static inline use_result__ ssize_t vectyploc_remove_replace_with_last(struct vectyploc *v, ssize_t n) {
  const size_t count = vectyploc_count(v);
  if (count == 1) {
    vectyploc_destroy(v);
    return 0;
  }

  struct typ **last = vectyploc_pop(v);
  if (n + 1 == count) {
    (void) last;
    return 0;
  } else {
    struct typ ***loc = vectyploc_get(v, n);
    *loc = last;
    return -1;
  }
}

HTABLE_SPARSE(fintypset, uint32_t, struct typ *);
DECLARE_HTABLE_SPARSE(fintypset, uint32_t, struct typ *);
void fintypset_fullinit(struct fintypset *set);

size_t typ_debug_backlinks_count(const struct typ *t);
void typ_debug_check_in_backlinks(struct typ **u);

struct typ *typ_create(struct typ *tbi, struct node *definition);
void typ_create_update_hash(struct typ *t);
void typ_create_update_genargs(struct typ *t);
void typ_create_update_quickisa(struct typ *t);
void typ_create_update_map_children(struct module *mod, struct typ *par);
struct typ *typ_create_ungenarg(struct typ *t);
void typ_create_ungenarg_update_genargs(struct module *trigger_mod, struct typ *t);

bool typ_hash_ready(const struct typ *t);

bool typ_was_zeroed(const struct typ *t);

struct typ **typ_permanent_loc(struct typ *t);
struct node *typ_definition_nooverlay(struct typ *t);
const struct node *typ_definition_nooverlay_const(const struct typ *t);
struct node *typ_definition_ignore_any_overlay(struct typ *t);
const struct node *typ_definition_ignore_any_overlay_const(const struct typ *t);
const struct node *typ_for_error(const struct typ *t);

uint32_t typ_toplevel_flags(const struct typ *t);
enum node_which typ_definition_which(const struct typ *t);
enum deftype_kind typ_definition_deftype_kind(const struct typ *t);
bool typ_definition_is_member(const struct typ *t);
struct typ *typ_definition_tag_type(const struct typ *t);
enum token_type typ_definition_deffun_access(const struct typ *t);
enum token_type typ_definition_defmethod_access(const struct typ *t);
struct typ *typ_definition_defmethod_self_wildcard_functor(const struct typ *t);
struct typ *typ_definition_defmethod_wildcard_functor(const struct typ *t);
struct typ *typ_definition_defmethod_nullable_wildcard_functor(const struct typ *t);
struct typ *typ_definition_deffun_wildcard_functor(const struct typ *t);
struct typ *typ_definition_deffun_nullable_wildcard_functor(const struct typ *t);
ident typ_definition_ident(const struct typ *t);
struct module *typ_module_owner(const struct typ *t);

struct typ *typ_member(const struct typ *t, ident name);

struct tit;
// End with 0.
struct tit *typ_definition_parent(const struct typ *t);
struct tit *typ_definition_members(const struct typ *t, ...);
struct tit *typ_definition_one_member(const struct typ *t, ident name);
struct tit *typ_resolve_accessor__has_effect(error *e,
                                             bool *container_is_tentative,
                                             struct module *mod,
                                             struct node *node);

// Frees the iterator when end is reached.
bool tit_next(struct tit *tit);
enum node_which tit_which(const struct tit *tit);
ident tit_ident(const struct tit *tit);
struct typ *tit_typ(const struct tit *tit);
struct typ *tit_parent_definition_typ(const struct tit *tit);
uint32_t tit_node_flags(const struct tit *tit);
struct node *tit_node_ignore_any_overlay(const struct tit *tit);
const struct node *tit_for_error(const struct tit *tit);

struct tit *tit_let_def(const struct tit *tit);
struct node *tit_defname_expr(const struct tit *tit);
bool tit_defchoice_is_leaf(const struct tit *tit);
bool tit_defchoice_is_external_payload(const struct tit *tit);
const struct node *tit_defchoice_tag_expr(const struct tit *tit);
struct tit *tit_defchoice_lookup_field(const struct tit *tit, ident name);

bool typ_is_generic_functor(const struct typ *t);
struct typ *typ_generic_functor(struct typ *t);
size_t typ_generic_arity(const struct typ *t);
size_t typ_generic_first_explicit_arg(const struct typ *t);
struct typ *typ_generic_arg(struct typ *t, size_t n);
bool typ_generic_arg_has_dependent_spec(const struct typ *t, size_t n);
bool typ_is_isalist_literal(const struct typ *t);

struct typ *typ_as_non_tentative(const struct typ *t);

bool typ_is_function(const struct typ *t);
size_t typ_function_arity(const struct typ *t);
size_t typ_function_min_arity(const struct typ *t);
size_t typ_function_max_arity(const struct typ *t);
ssize_t typ_function_first_vararg(const struct typ *t);
struct typ *typ_function_arg(struct typ *t, size_t n);
ident typ_function_arg_ident(const struct typ *t, size_t n);
enum token_type typ_function_arg_explicit_ref(const struct typ *t, size_t n);
struct typ *typ_function_return(struct typ *t);

void unset_typ(struct typ **loc);
void set_typ(struct typ **loc, struct typ *t);
void set_typ_defgenarg(struct typ **loc, struct typ *t);

// Don't call these; they're privileged.
void typ_add_tentative_bit__privileged(struct typ **loc);
void typ_declare_final__privileged(struct typ *t);

bool typ_is_ungenarg(const struct typ *t);
bool typ_is_tentative(const struct typ *t);
struct typ *typ_create_tentative_functor(struct module *trigger_mod, struct typ *target);
struct typ *typ_create_tentative(struct module *trigger_mod,
                                 struct typ *t, struct typ **args, size_t arity);

void typ_link_tentative(struct typ *dst, struct typ *src);
void typ_link_tentative_functor(struct module *mod, struct typ *dst, struct typ *src);
void typ_link_to_existing_final(struct typ *dst, struct typ *src);

struct typ *typ_lookup_builtin_tuple(struct module *mod, size_t arity);

bool typ_equal(const struct typ *a, const struct typ *b);
ERROR typ_check_equal(const struct module *mod, const struct node *for_error,
                      const struct typ *a, const struct typ *b);
bool typ_has_same_generic_functor(const struct module *mod,
                                  const struct typ *a, const struct typ *b);
bool typ_isa(const struct typ *a, const struct typ *intf);
ERROR typ_check_isa(const struct module *mod, const struct node *for_error,
                    const struct typ *a, const struct typ *intf);

bool typ_is_reference(const struct typ *t);
bool typ_is_nullable_reference(const struct typ *t);
ERROR typ_check_is_reference(const struct module *mod, const struct node *for_error,
                             const struct typ *a);
bool typ_is_slice(const struct typ *t);
bool typ_is_tuple(const struct typ *t);
bool typ_is_optional(const struct typ *t);

bool typ_is_dyn(const struct typ *t);
bool typ_is_dyn_compatible(const struct typ *t);
ERROR typ_check_can_deref(const struct module *mod, const struct node *for_error,
                          const struct typ *a, enum token_type operator);
ERROR typ_check_deref_against_mark(const struct module *mod, const struct node *for_error,
                                   const struct typ *t, enum token_type operator);

bool typ_is_builtin(const struct module *mod, const struct typ *t);
bool typ_is_pseudo_builtin(const struct typ *t);
bool typ_is_trivial(const struct typ *t);
bool typ_is_literal(const struct typ *t);
bool typ_is_concrete(const struct typ *t);
bool typ_isa_return_by_copy(const struct typ *t);

enum isalist_filter {
  ISALIST_FILTEROUT_NOT_EXPORTED = 0x1,
  ISALIST_FILTEROUT_EXPORTED = 0x2,
  ISALIST_FILTEROUT_TRIVIAL_ISALIST = 0x4,
  ISALIST_FILTEROUT_NONTRIVIAL_ISALIST = 0x8,
  ISALIST_FILTEROUT_PREVENT_DYN = 0x10,
};
typedef error (*isalist_each)(struct module *mod, struct typ *t, struct typ *intf,
                              bool *stop, void *user);
ERROR typ_isalist_foreach(struct module *mod, struct typ *t, uint32_t filter,
                          isalist_each iter, void *user);

// const interface
const struct typ *typ_generic_functor_const(const struct typ *t);
const struct typ *typ_generic_arg_const(const struct typ *t, size_t n);
const struct typ *typ_function_arg_const(const struct typ *t, size_t n);
const struct typ *typ_function_return_const(const struct typ *t);

void instances_init(struct node *gendef);
void instances_add(struct module *mod, struct typ *genf, struct typ **instance);
void instances_maintain(struct module *mod, struct typ *genf);
struct typ *instances_find_existing_final_with(struct typ *genf,
                                               struct typ **args, size_t arity);
struct typ *instances_find_existing_final_like(const struct typ *_t);
struct typ *instances_find_existing_identical(struct module *mod, struct typ *functor,
                                              struct typ **args, size_t arity);

// Return value must be freed by caller.
char *typ_name(const struct module *mod, const struct typ *t);
// Return value must be freed by caller.
char *pptyp(const struct module *mod, const struct typ *t);
void ppoverlay(struct typ *t);
void ppisalist(const struct typ *t);
void pptypptrs(const struct typ *t);
void ppusers(const struct typ *t);
void ppvectyp(struct vectyp *v);
void ppvectyploc(struct vectyploc *v);

extern struct typ *TBI_VOID;
extern struct typ *TBI_LITERALS_NIL;
extern struct typ *TBI_LITERALS_INTEGER;
extern struct typ *TBI_LITERALS_FLOATING;
extern struct typ *TBI_LITERALS_SLICE;
extern struct typ *TBI_LITERALS_STRING;
extern struct typ *TBI_ANY_TUPLE;
extern struct typ *TBI_TUPLE_2;
extern struct typ *TBI_TUPLE_3;
extern struct typ *TBI_TUPLE_4;
extern struct typ *TBI_TUPLE_5;
extern struct typ *TBI_TUPLE_6;
extern struct typ *TBI_TUPLE_7;
extern struct typ *TBI_TUPLE_8;
extern struct typ *TBI_TUPLE_9;
extern struct typ *TBI_TUPLE_10;
extern struct typ *TBI_TUPLE_11;
extern struct typ *TBI_TUPLE_12;
extern struct typ *TBI_TUPLE_13;
extern struct typ *TBI_TUPLE_14;
extern struct typ *TBI_TUPLE_15;
extern struct typ *TBI_TUPLE_16;
extern struct typ *TBI_ANY;
extern struct typ *TBI_BOOL;
extern struct typ *TBI_BOOL_COMPATIBLE;
extern struct typ *TBI_I8;
extern struct typ *TBI_U8;
extern struct typ *TBI_I16;
extern struct typ *TBI_U16;
extern struct typ *TBI_I32;
extern struct typ *TBI_U32;
extern struct typ *TBI_I64;
extern struct typ *TBI_U64;
extern struct typ *TBI_UINT;
extern struct typ *TBI_INT;
extern struct typ *TBI_UINTPTR;
extern struct typ *TBI_INTPTR;
extern struct typ *TBI_FLOAT;
extern struct typ *TBI_DOUBLE;
extern struct typ *TBI_RUNE;
extern struct typ *TBI_STRING;
extern struct typ *TBI_STRING_COMPATIBLE;
extern struct typ *TBI_ANY_ANY_REF;
extern struct typ *TBI_ANY_REF;
extern struct typ *TBI_ANY_MUTABLE_REF;
extern struct typ *TBI_ANY_NULLABLE_REF;
extern struct typ *TBI_ANY_NULLABLE_MUTABLE_REF;
extern struct typ *TBI_REF; // @
extern struct typ *TBI_MREF; // @!
extern struct typ *TBI_MMREF; // @#
extern struct typ *TBI_NREF; // ?@
extern struct typ *TBI_NMREF; // ?@!
extern struct typ *TBI_NMMREF; // ?@#
extern struct typ *TBI_VOIDREF;
extern struct typ *TBI_ANY_ANY_SLICE;
extern struct typ *TBI_ANY_SLICE;
extern struct typ *TBI_ANY_MUTABLE_SLICE;
extern struct typ *TBI_SLICE;
extern struct typ *TBI_MSLICE;
extern struct typ *TBI_SLICE_IMPL;
extern struct typ *TBI_SLICE_COMPATIBLE;
extern struct typ *TBI_OPTIONAL;
extern struct typ *TBI_VARARG;
extern struct typ *TBI_ADDITIVE_ARITHMETIC;
extern struct typ *TBI_ADDITIVE_ARITHMETIC_ASSIGN;
extern struct typ *TBI_ARITHMETIC;
extern struct typ *TBI_ARITHMETIC_ASSIGN;
extern struct typ *TBI_HAS_BITWISE_OPERATORS;
extern struct typ *TBI_HAS_BITWISE_OPERATORS_ASSIGN;
extern struct typ *TBI_INTEGER_ARITHMETIC;
extern struct typ *TBI_OVERFLOW_ARITHMETIC;
extern struct typ *TBI_NUMBER_LITERAL_COMPATIBLE;
extern struct typ *TBI_INTEGER;
extern struct typ *TBI_UNSIGNED_INTEGER;
extern struct typ *TBI_NATIVE;
extern struct typ *TBI_NATIVE_INTEGER;
extern struct typ *TBI_NATIVE_SIZED_UNSIGNED_INTEGER;
extern struct typ *TBI_GENERALIZED_BOOLEAN;
extern struct typ *TBI_NATIVE_BOOLEAN;
extern struct typ *TBI_FLOATING;
extern struct typ *TBI_NATIVE_FLOATING;
extern struct typ *TBI_HAS_EQUALITY;
extern struct typ *TBI_NOT_HAS_EQUALITY;
extern struct typ *TBI_ORDERED;
extern struct typ *TBI_NOT_ORDERED;
extern struct typ *TBI_PARTIALLY_ORDERED;
extern struct typ *TBI_EQUALITY_BY_COMPARE;
extern struct typ *TBI_ORDERED_BY_COMPARE;
extern struct typ *TBI_COPYABLE;
extern struct typ *TBI_NOT_COPYABLE;
extern struct typ *TBI_MOVEABLE;
extern struct typ *TBI_NOT_MOVEABLE;
extern struct typ *TBI_DEFAULT_CTOR;
extern struct typ *TBI_NON_DEFAULT_CTOR;
extern struct typ *TBI_DEFAULT_DTOR;
extern struct typ *TBI_ERROR_DTOR;
extern struct typ *TBI_ARRAY_CTOR;
extern struct typ *TBI_TRIVIAL_COPY;
extern struct typ *TBI_TRIVIAL_COPY_BUT_OWNED;
extern struct typ *TBI_TRIVIAL_MOVE;
extern struct typ *TBI_TRIVIAL_CTOR;
extern struct typ *TBI_TRIVIAL_DTOR;
extern struct typ *TBI_TRIVIAL_COMPARE;
extern struct typ *TBI_TRIVIAL_EQUALITY;
extern struct typ *TBI_TRIVIAL_ORDER;
extern struct typ *TBI_RETURN_BY_COPY;
extern struct typ *TBI_NOT_RETURN_BY_COPY;
extern struct typ *TBI_ENUM;
extern struct typ *TBI_UNION;
extern struct typ *TBI_UNION_TRIVIAL_CTOR;
extern struct typ *TBI_RANGE;
extern struct typ *TBI_BOUNDS;
extern struct typ *TBI_COLLECTION;
extern struct typ *TBI_ITERATOR;
extern struct typ *TBI_ENVIRONMENT;
extern struct typ *TBI_ANY_ENVIRONMENT;
extern struct typ *TBI_PREVENT_DYN;
extern struct typ *TBI_INHERIT;
extern struct typ *TBI_ERROR;
extern struct typ *TBI_GLOBALENV_INSTALLED;
extern struct typ *TBI_GLOBALENV_PARENT;
extern struct typ *TBI_GLOBALENV_INSTALL;
extern struct typ *TBI_GLOBALENV_UNINSTALL;
extern struct typ *TBI_CODELOC;
extern struct typ *TBI__NOT_TYPEABLE;
extern struct typ *TBI__CALL_FUNCTION_SLOT;
extern struct typ *TBI__MUTABLE;
extern struct typ *TBI__MERCURIAL;

#endif
