//#include "based.h"
//
//#include "passes.h"
//#include "types.h"
//
//struct phi {
//  struct vecnode ancestors;
//};
//
//struct based_field {
//  struct node *base;
//  ident field;
//};
//
//struct based_element {
//  struct node *base;
//
//  bool is_constant;
//  uint64_t constant;
//
//  struct node *dynamic_index;
//};
//
//// Related to C99 "based" concept.
//enum based_how {
//  PHI_OF,
//  REF_OF,
//  FIELD_OF,
//  ELEMENT_OF,
//  // cast of a ref, or somehow derived from a ref's numerical address
//  MEMALIAS_OF,
//};
//
//union based_as {
//  struct phi PHI_OF;
//  struct node *REF_OF; // DEFNAME or DEFARG
//  struct based_field FIELD_OF;
//  struct based_element ELEMENT_OF;
//  struct node *MEMALIAS_OF; // DEFNAME or DEFARG
//};
//
//struct based {
//  enum based_how how;
//  union based_as as;
//
//  struct based *next;
//};
//
