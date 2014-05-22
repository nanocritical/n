#ifndef TYPES_H__
#define TYPES_H__

#include "nodes.h"

struct typ;

HTABLE_SPARSE(typset, uint32_t, struct typ *);
DECLARE_HTABLE_SPARSE(typset, uint32_t, struct typ *);
void typset_fullinit(struct typset *set);

size_t typ_debug_backlinks_count(const struct typ *t);
void typ_debug_check_in_backlinks(struct typ **u);

struct typ *typ_create(struct typ *tbi, struct node *definition);
void typ_create_update_hash(struct typ *t);
void typ_create_update_genargs(struct typ *t);
void typ_create_update_quickisa(struct typ *t);

struct node *typ_definition(struct typ *t);

bool typ_is_generic_functor(const struct typ *t);
struct typ *typ_generic_functor(struct typ *t);
size_t typ_generic_arity(const struct typ *t);
size_t typ_generic_first_explicit_arg(const struct typ *t);
struct typ *typ_generic_arg(struct typ *t, size_t n);

bool typ_is_function(const struct typ *t);
size_t typ_function_arity(const struct typ *t);
struct typ *typ_function_arg(struct typ *t, size_t n);
struct typ *typ_function_return(struct typ *t);

void unset_typ(struct typ **loc);
void set_typ(struct typ **loc, struct typ *t);

// Don't call these; they're privileged.
void typ_add_tentative_bit__privileged(struct typ **loc);
void typ_declare_final__privileged(struct typ *t);

struct typ *typ_create_tentative(struct typ *target);
bool typ_is_tentative(const struct typ *t);

void typ_link_tentative(struct typ *dst, struct typ *src);
void typ_link_tentative_functor(struct module *mod, struct typ *dst, struct typ *src);
void typ_link_to_existing_final(struct typ *dst, struct typ *src);

struct typ *typ_lookup_builtin_tuple(struct module *mod, size_t arity);

bool typ_equal(const struct typ *a, const struct typ *b);
error typ_check_equal(const struct module *mod, const struct node *for_error,
                      const struct typ *a, const struct typ *b);
bool typ_has_same_generic_functor(const struct module *mod,
                                  const struct typ *a, const struct typ *b);
bool typ_isa(const struct typ *a, const struct typ *intf);
error typ_check_isa(const struct module *mod, const struct node *for_error,
                    const struct typ *a, const struct typ *intf);

bool typ_is_reference(const struct typ *t);
bool typ_is_nullable_reference(const struct typ *t);
error typ_check_is_reference(const struct module *mod, const struct node *for_error,
                             const struct typ *a);
bool typ_is_slice(const struct typ *t);

bool typ_is_dyn(const struct typ *t);
bool typ_is_dyn_compatible(const struct typ *t);
error typ_check_can_deref(const struct module *mod, const struct node *for_error,
                          const struct typ *a, enum token_type operator);
error typ_check_deref_against_mark(const struct module *mod, const struct node *for_error,
                                   const struct typ *t, enum token_type operator);

bool typ_is_builtin(const struct module *mod, const struct typ *t);
bool typ_is_pseudo_builtin(const struct typ *t);
bool typ_is_trivial(const struct typ *t);
bool typ_is_literal(const struct typ *t);
bool typ_is_weakly_concrete(const struct typ *t);
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
error typ_isalist_foreach(struct module *mod, struct typ *t, uint32_t filter,
                          isalist_each iter, void *user);

// const interface
const struct node *typ_definition_const(const struct typ *t);
const struct typ *typ_generic_functor_const(const struct typ *t);
const struct typ *typ_generic_arg_const(const struct typ *t, size_t n);
const struct typ *typ_function_arg_const(const struct typ *t, size_t n);
const struct typ *typ_function_return_const(const struct typ *t);

extern struct typ *TBI_VOID;
extern struct typ *TBI_LITERALS_NULL;
extern struct typ *TBI_LITERALS_INTEGER;
extern struct typ *TBI_LITERALS_FLOATING;
extern struct typ *TBI_LITERALS_INIT;
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
extern struct typ *TBI_SIZE;
extern struct typ *TBI_SSIZE;
extern struct typ *TBI_FLOAT;
extern struct typ *TBI_DOUBLE;
extern struct typ *TBI_CHAR;
extern struct typ *TBI_STRING;
extern struct typ *TBI_STATIC_STRING;
extern struct typ *TBI_STATIC_STRING_COMPATIBLE;
extern struct typ *TBI_STATIC_ARRAY;
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
extern struct typ *TBI_ANY_ANY_SLICE;
extern struct typ *TBI_ANY_SLICE;
extern struct typ *TBI_ANY_MUTABLE_SLICE;
extern struct typ *TBI_SLICE;
extern struct typ *TBI_MSLICE;
extern struct typ *TBI_SLICE_IMPL;
extern struct typ *TBI_VARARG;
extern struct typ *TBI_ARITHMETIC;
extern struct typ *TBI_BITWISE;
extern struct typ *TBI_INTEGER;
extern struct typ *TBI_UNSIGNED_INTEGER;
extern struct typ *TBI_NATIVE_INTEGER;
extern struct typ *TBI_NATIVE_ANYSIGN_INTEGER;
extern struct typ *TBI_GENERALIZED_BOOLEAN;
extern struct typ *TBI_NATIVE_BOOLEAN;
extern struct typ *TBI_FLOATING;
extern struct typ *TBI_NATIVE_FLOATING;
extern struct typ *TBI_HAS_EQUALITY;
extern struct typ *TBI_NOT_HAS_EQUALITY;
extern struct typ *TBI_ORDERED;
extern struct typ *TBI_NOT_ORDERED;
extern struct typ *TBI_EQUALITY_BY_COMPARE;
extern struct typ *TBI_ORDERED_BY_COMPARE;
extern struct typ *TBI_COPYABLE;
extern struct typ *TBI_NOT_COPYABLE;
extern struct typ *TBI_DEFAULT_CTOR;
extern struct typ *TBI_NON_DEFAULT_CTOR;
extern struct typ *TBI_DEFAULT_DTOR;
extern struct typ *TBI_ARRAY_CTOR;
extern struct typ *TBI_TRIVIAL_COPY;
extern struct typ *TBI_TRIVIAL_COPY_BUT_OWNED;
extern struct typ *TBI_TRIVIAL_CTOR;
extern struct typ *TBI_TRIVIAL_ARRAY_CTOR;
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
extern struct typ *TBI_INDEX_BOUNDS;
extern struct typ *TBI_ITERATOR;
extern struct typ *TBI_ENVIRONMENT;
extern struct typ *TBI_ANY_ENVIRONMENT;
extern struct typ *TBI_PREVENT_DYN;
extern struct typ *TBI__NOT_TYPEABLE;
extern struct typ *TBI__CALL_FUNCTION_SLOT;
extern struct typ *TBI__MUTABLE;
extern struct typ *TBI__MERCURIAL;

#endif
