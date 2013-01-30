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
  Tdyn,
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
  Tisa,
  Tnull,
  Tpass,
  Tpre,
  Tpost,
  Tinvariant,
  Texample,

  TNUMBER,
  TSTRING,
  TIDENT,
  TASSIGN,
  TEQ,
  TNE,
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
  TUBWNOT,
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
  TREFDOT,
  TREFBANG,
  TREFSHARP,
  TNULREFDOT,
  TNULREFBANG,
  TNULREFSHARP,
  TDOTDOTDOT,
  TSLICEBRAKETS,
  TLINIT,
  TRINIT,
  TLPAR,
  TRPAR,

  T__CALL,
  T__NONE,

  TOKEN__NUM,
};

enum operator_associativity {
  ASSOC_NON = 0x10000,
  ASSOC_LEFT = 0x20000,
  ASSOC_RIGHT = 0x30000,
};

enum operator_kind {
  OP_UN_REFOF = 0x1,
  OP_UN_BOOL = 0x2,
  OP_UN_NUM = 0x3,
  OP_UN_DYN = 0x4,
  OP_BIN = 0x10,
  OP_BIN_SYM = 0x20,
  OP_BIN_SYM_BOOL = 0x30,
  OP_BIN_SYM_NUM = 0x40,
  OP_BIN_ACC = 0x50,
  OP_BIN_NUM_RHS_U16 = 0x60,
  OP_BIN_RHS_TYPE = 0x70,
  OP_UN__MASK = 0x0f,
  OP_BIN__MASK = 0xf0,
  OP_KIND__MASK = 0xff,
};

#define OP(kind, assoc, precedence) ((kind << 18) | assoc | precedence)
#define IS_OP(t) (operators[t] != 0)
#define OP_UNARY(t) ((operators[t] >> 18) & OP_UN__MASK)
#define OP_BINARY(t) ((operators[t] >> 18) & OP_BIN__MASK)
#define OP_ASSOC(t) (operators[t] & 0x30000)
#define OP_PREC(t) (operators[t] & 0xffff)
#define OP_KIND(t) ((operators[t] >> 18) & OP_KIND__MASK)

static const uint32_t operators[TOKEN__NUM] = {
  [T__NONE] = OP(0, 0, 0xffff),
  [TASSIGN] = OP(OP_BIN_SYM, ASSOC_NON, 0x140),
  [TPLUS_ASSIGN] = OP(OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TMINUS_ASSIGN] = OP(OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TTIMES_ASSIGN] = OP(OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TDIVIDE_ASSIGN] = OP(OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TMODULO_ASSIGN] = OP(OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TBWAND_ASSIGN] = OP(OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TBWOR_ASSIGN] = OP(OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TBWXOR_ASSIGN] = OP(OP_BIN_SYM_NUM, ASSOC_NON, 0x140),
  [TRSHIFT_ASSIGN] = OP(OP_BIN_NUM_RHS_U16, ASSOC_NON, 0x140),
  [TLSHIFT_ASSIGN] = OP(OP_BIN_NUM_RHS_U16, ASSOC_NON, 0x140),
  [TCOMMA] = OP(OP_BIN, ASSOC_LEFT, 0x130),
  [Tor] = OP(OP_BIN_SYM_BOOL, ASSOC_LEFT, 0x120),
  [Tand] = OP(OP_BIN_SYM_BOOL, ASSOC_LEFT, 0x110),
  [Tnot] = OP(OP_UN_BOOL, ASSOC_RIGHT, 0x100),
  [Tisa] = OP(OP_BIN_RHS_TYPE, ASSOC_NON, 0xf0),
  [TLE] = OP(OP_BIN_SYM_NUM, ASSOC_NON, 0xe0),
  [TLT] = OP(OP_BIN_SYM_NUM, ASSOC_NON, 0xe0),
  [TGT] = OP(OP_BIN_SYM_NUM, ASSOC_NON, 0xe0),
  [TGE] = OP(OP_BIN_SYM_NUM, ASSOC_NON, 0xe0),
  [TEQ] = OP(OP_BIN_SYM, ASSOC_NON, 0xe0),
  [TNE] = OP(OP_BIN_SYM, ASSOC_NON, 0xe0),
  [T__CALL] = OP(OP_BIN, ASSOC_NON, 0xd0),
  [TBWOR] = OP(OP_BIN_SYM_NUM, ASSOC_LEFT, 0xc0),
  [TBWXOR] = OP(OP_BIN_SYM_NUM, ASSOC_LEFT, 0xb0),
  [TBWAND] = OP(OP_BIN_SYM_NUM, ASSOC_LEFT, 0xa0),
  [TLSHIFT] = OP(OP_BIN_NUM_RHS_U16, ASSOC_NON, 0x90),
  [TRSHIFT] = OP(OP_BIN_NUM_RHS_U16, ASSOC_NON, 0x90),
  [TPLUS] = OP(OP_BIN_SYM_NUM, ASSOC_LEFT, 0x80),
  [TMINUS] = OP(OP_BIN_SYM_NUM, ASSOC_LEFT, 0x80),
  [TDIVIDE] = OP(OP_BIN_SYM_NUM, ASSOC_LEFT, 0x60),
  [TMODULO] = OP(OP_BIN_SYM_NUM, ASSOC_LEFT, 0x60),
  [TTIMES] = OP(OP_BIN_SYM_NUM, ASSOC_LEFT, 0x60),
  [TUPLUS] = OP(OP_UN_NUM, ASSOC_RIGHT, 0x50),
  [TUMINUS] = OP(OP_UN_NUM, ASSOC_RIGHT, 0x50),
  [TUBWNOT] = OP(OP_UN_NUM, ASSOC_RIGHT, 0x50),
  [TREFDOT] = OP(OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TREFBANG] = OP(OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TREFSHARP] = OP(OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TNULREFDOT] = OP(OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TNULREFBANG] = OP(OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TNULREFSHARP] = OP(OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TCOLON] = OP(OP_BIN_RHS_TYPE, ASSOC_LEFT, 0x30),
  [Tdyn] = OP(OP_UN_DYN, ASSOC_RIGHT, 0x25),
  [TLINIT] = OP(OP_BIN, ASSOC_RIGHT, 0x21),
  [TDOT] = OP(OP_BIN_ACC, ASSOC_LEFT, 0x20),
  [TBANG] = OP(OP_BIN_ACC, ASSOC_LEFT, 0x20),
  [TSHARP] = OP(OP_BIN_ACC, ASSOC_LEFT, 0x20),
};

static const bool expr_terminators[TOKEN__NUM] = {
  [TEOL] = TRUE,
  [TSOB] = TRUE,
  [TEOB] = TRUE,
  [TRPAR] = TRUE,
};

struct parser {
  const char *data;
  size_t len;
  size_t pos;

  size_t indent;
  size_t block_depth;
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
