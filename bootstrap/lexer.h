#ifndef LEXER_H__
#define LEXER_H__

#include <stdint.h>
#include <stdlib.h>

#include "common.h"

enum token_type {
  Timport = 1,
  Texport,
  Tfrom,
  Ttype,
  Textern,
  Tfun,
  Tmethod,
  Tintf,
  Tinline,
  Tlet,
  Tsuch,
  Talias,
  Tif,
  Telif,
  Telse,
  Tfor,
  Tin,
  Twhile,
  Tcontinue,
  Tbreak,
  Tmatch,
  Treturn,
  Ttry,
  Tcatch,
  Texcept,
  Tthrow,
  Tblock,
  Tdelegate,
  Tdeclare,
  Tand,
  Tor,
  Tnot,
  Tfalse,
  Ttrue,
  Tisa,
  Tnull,
  Tnoop,
  Tpre,
  Tpost,
  Tinvariant,
  Texample,
  Tsizeof,

  TNUMBER,
  TSTRING,
  TIDENT,
  TASSIGN,
  TEQ,
  TNE,
  TEQPTR,
  TNEPTR,
  TLE,
  TLT,
  TGT,
  TGE,
  TPLUS,
  TMINUS,
  TUPLUS,
  TUMINUS,
  TTIMES,
  TDIVIDE,
  TMODULO,
  TBWAND,
  TBWOR,
  TBWXOR,
  TRSHIFT,
  TLSHIFT,
  TPLUS_ASSIGN,
  TMINUS_ASSIGN,
  TTIMES_ASSIGN,
  TDIVIDE_ASSIGN,
  TMODULO_ASSIGN,
  TBWAND_ASSIGN,
  TBWOR_ASSIGN,
  TBWXOR_ASSIGN,
  TRSHIFT_ASSIGN,
  TLSHIFT_ASSIGN,
  TBWNOT,
  TARROW,
  TEOL,
  TSOB,
  TEOB,
  TCOLON,
  TCOMMA,
  TSEMICOLON,
  TDOT,
  TBANG,
  TSHARP,
  TWILDCARD,
  TDEREFDOT,
  TDEREFBANG,
  TDEREFSHARP,
  TDEREFWILDCARD,
  TREFDOT,
  TREFBANG,
  TREFSHARP,
  TREFWILDCARD,
  TNULREFDOT,
  TNULREFBANG,
  TNULREFSHARP,
  TNULREFWILDCARD,
  TQMARK,
  TDOTDOTDOT,
  TSLICEBRAKETS,
  TLSBRA,
  TRSBRA,
  TLCBRA,
  TRCBRA,
  TLPAR,
  TRPAR,

  T__CALL,
  T__NOT_STATEMENT,
  T__STATEMENT,

  TOKEN__NUM,
};

const char *token_strings[TOKEN__NUM];

enum operator_associativity {
  ASSOC_NON = 0x10000,
  ASSOC_LEFT = 0x20000,
  ASSOC_RIGHT = 0x30000,
};

enum operator_kind {
  OP_UN_REFOF = 0x1,
  OP_UN_DEREF = 0x2,
  OP_UN_BOOL = 0x3,
  OP_UN_NUM = 0x4,
  OP_UN_DYN = 0x5,
  OP_BIN = 0x10,
  OP_BIN_SYM = 0x20,
  OP_BIN_SYM_BOOL = 0x30,
  OP_BIN_SYM_NUM = 0x40,
  OP_BIN_SYM_PTR = 0x50,
  OP_BIN_ACC = 0x60,
  OP_BIN_NUM_RHS_UNSIGNED = 0x70,
  OP_BIN_RHS_TYPE = 0x80,
  OP_ASSIGN = 0x1,
  OP_UN__MASK = 0x0f,
  OP_BIN__MASK = 0xf0,
  OP_KIND__MASK = 0xff,
  OP_ASSIGN__MASK = 0x1,
};

#define OP(assign, kind, assoc, precedence) ((assign << 26) | (kind << 18) | assoc | precedence)
#define IS_OP(t) (operators[t] != 0)
#define OP_UNARY(t) ((operators[t] >> 18) & OP_UN__MASK)
#define OP_BINARY(t) ((operators[t] >> 18) & OP_BIN__MASK)
#define OP_ASSOC(t) (operators[t] & 0x30000)
#define OP_PREC(t) (operators[t] & 0xffff)
#define OP_KIND(t) ((operators[t] >> 18) & OP_KIND__MASK)
#define OP_ASSIGN(t) ((operators[t] >> 26) & OP_ASSIGN__MASK)

static const uint32_t operators[TOKEN__NUM] = {
  [T__STATEMENT] = OP(0, 0, 0, 0xffff),
  [TASSIGN] = OP(OP_ASSIGN, OP_BIN_SYM, ASSOC_NON, 0x140),
  [TPLUS_ASSIGN] = OP(0, OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TMINUS_ASSIGN] = OP(0, OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TTIMES_ASSIGN] = OP(0, OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TDIVIDE_ASSIGN] = OP(0, OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TMODULO_ASSIGN] = OP(0, OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TBWAND_ASSIGN] = OP(0, OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TBWOR_ASSIGN] = OP(0, OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TBWXOR_ASSIGN] = OP(0, OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TRSHIFT_ASSIGN] = OP(0, OP_BIN_NUM_RHS_UNSIGNED, ASSOC_NON, 0x140),
  [TLSHIFT_ASSIGN] = OP(0, OP_BIN_NUM_RHS_UNSIGNED, ASSOC_NON, 0x140),
  [Tin] = OP(0, OP_BIN_SYM, ASSOC_NON, 0x140),
  [T__NOT_STATEMENT] = OP(0, OP_BIN, ASSOC_NON, 0x139),
  [TCOMMA] = OP(0, OP_BIN, ASSOC_LEFT, 0x130),
  [Tor] = OP(0, OP_BIN_SYM_BOOL, ASSOC_LEFT, 0x120),
  [Tand] = OP(0, OP_BIN_SYM_BOOL, ASSOC_LEFT, 0x110),
  [Tnot] = OP(0, OP_UN_BOOL, ASSOC_RIGHT, 0x100),
  [Tisa] = OP(0, OP_BIN_RHS_TYPE, ASSOC_NON, 0xf0),
  [TLE] = OP(0, OP_BIN_SYM, ASSOC_NON, 0xe0),
  [TLT] = OP(0, OP_BIN_SYM, ASSOC_NON, 0xe0),
  [TGT] = OP(0, OP_BIN_SYM, ASSOC_NON, 0xe0),
  [TGE] = OP(0, OP_BIN_SYM, ASSOC_NON, 0xe0),
  [TEQ] = OP(0, OP_BIN_SYM, ASSOC_NON, 0xe0),
  [TNE] = OP(0, OP_BIN_SYM, ASSOC_NON, 0xe0),
  [TEQPTR] = OP(0, OP_BIN_SYM_PTR, ASSOC_NON, 0xe0),
  [TNEPTR] = OP(0, OP_BIN_SYM_PTR, ASSOC_NON, 0xe0),
  [T__CALL] = OP(0, OP_BIN, ASSOC_NON, 0xd0),
  [TBWOR] = OP(0, OP_BIN_SYM_NUM, ASSOC_LEFT, 0xc0),
  [TBWXOR] = OP(0, OP_BIN_SYM_NUM, ASSOC_LEFT, 0xb0),
  [TBWAND] = OP(0, OP_BIN_SYM_NUM, ASSOC_LEFT, 0xa0),
  [TLSHIFT] = OP(0, OP_BIN_NUM_RHS_UNSIGNED, ASSOC_NON, 0x90),
  [TRSHIFT] = OP(0, OP_BIN_NUM_RHS_UNSIGNED, ASSOC_NON, 0x90),
  [TPLUS] = OP(0, OP_BIN_SYM_NUM, ASSOC_LEFT, 0x80),
  [TMINUS] = OP(0, OP_BIN_SYM_NUM, ASSOC_LEFT, 0x80),
  [TDIVIDE] = OP(0, OP_BIN_SYM_NUM, ASSOC_LEFT, 0x60),
  [TMODULO] = OP(0, OP_BIN_SYM_NUM, ASSOC_LEFT, 0x60),
  [TTIMES] = OP(0, OP_BIN_SYM_NUM, ASSOC_LEFT, 0x60),
  [TUPLUS] = OP(0, OP_UN_NUM, ASSOC_RIGHT, 0x50),
  [TUMINUS] = OP(0, OP_UN_NUM, ASSOC_RIGHT, 0x50),
  [TBWNOT] = OP(0, OP_UN_NUM, ASSOC_RIGHT, 0x50),
  [TREFDOT] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TREFBANG] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TREFSHARP] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TREFWILDCARD] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TNULREFDOT] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TNULREFBANG] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TNULREFSHARP] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TNULREFWILDCARD] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TCOLON] = OP(0, OP_BIN_RHS_TYPE, ASSOC_LEFT, 0x30),
  [TDOT] = OP(0, OP_BIN_ACC, ASSOC_LEFT, 0x20),
  [TBANG] = OP(0, OP_BIN_ACC, ASSOC_LEFT, 0x20),
  [TSHARP] = OP(0, OP_BIN_ACC, ASSOC_LEFT, 0x20),
  [TWILDCARD] = OP(0, OP_BIN_ACC, ASSOC_LEFT, 0x20),
  [TDEREFDOT] = OP(0, OP_UN_DEREF, ASSOC_LEFT, 0x1f),
  [TDEREFBANG] = OP(0, OP_UN_DEREF, ASSOC_LEFT, 0x1f),
  [TDEREFSHARP] = OP(0, OP_UN_DEREF, ASSOC_LEFT, 0x1f),
  [TDEREFWILDCARD] = OP(0, OP_UN_DEREF, ASSOC_LEFT, 0x1f),
};

static const bool expr_terminators[TOKEN__NUM] = {
  [TEOL] = TRUE,
  [TSOB] = TRUE,
  [TEOB] = TRUE,
  [TRPAR] = TRUE,
  [TRSBRA] = TRUE,
  [TRCBRA] = TRUE,
};

struct parser {
  const char *data;
  size_t len;
  size_t pos;

  size_t indent;
  size_t block_depth;
  size_t no_block_depth;
  bool block_style[1024];

  size_t backs_count;

  bool tok_was_injected;
  bool inject_eol_after_eob;

  char error_message[1024];
};

struct token {
  enum token_type t;
  const char *value;
  size_t len;
};

error lexer_scan(struct token *tok, struct parser *parser);
void lexer_back(struct parser *parser, const struct token *tok);

#endif
