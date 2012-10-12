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
  Tunion,
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

#define OP(unary, assoc, precedence) ((unary << 18) | assoc | precedence)
#define IS_OP(t) (operators[t] != 0)
#define OP_UNARY(t) ((operators[t] >> 18) & 0x1)
#define OP_BINARY(t) (!OP_UNARY(t))
#define OP_ASSOC(t) (operators[t] & 0x30000)
#define OP_PREC(t) (operators[t] & 0xffff)

enum associativity {
  ASSOC_NON = 0x10000,
  ASSOC_LEFT = 0x20000,
  ASSOC_RIGHT = 0x30000,
};

static const uint32_t operators[TOKEN__NUM] = {
  [T__NONE] = OP(FALSE, 0, 0xffff),
  [TASSIGN] = OP(FALSE, ASSOC_LEFT, 0x400),
  [TCOMMA] = OP(FALSE, ASSOC_LEFT, 0x300),
  [Tand] = OP(FALSE, ASSOC_LEFT, 0x200),
  [Tor] = OP(FALSE, ASSOC_LEFT, 0x200),
  [Tnot] = OP(TRUE, ASSOC_RIGHT, 0x100),
  [Tisa] = OP(FALSE, ASSOC_NON, 0xf0),
  [TLE] = OP(FALSE, ASSOC_NON, 0xe0),
  [TLT] = OP(FALSE, ASSOC_NON, 0xe0),
  [TGT] = OP(FALSE, ASSOC_NON, 0xe0),
  [TGE] = OP(FALSE, ASSOC_NON, 0xe0),
  [TEQ] = OP(FALSE, ASSOC_NON, 0xe0),
  [TNE] = OP(FALSE, ASSOC_NON, 0xe0),
  [T__CALL] = OP(FALSE, ASSOC_NON, 0xd0),
  [TBWOR] = OP(FALSE, ASSOC_LEFT, 0xc0),
  [TBWXOR] = OP(FALSE, ASSOC_LEFT, 0xb0),
  [TBWAND] = OP(FALSE, ASSOC_LEFT, 0xa0),
  [TLSHIFT] = OP(FALSE, ASSOC_NON, 0x90),
  [TRSHIFT] = OP(FALSE, ASSOC_NON, 0x90),
  [TPLUS] = OP(FALSE, ASSOC_LEFT, 0x80),
  [TMINUS] = OP(FALSE, ASSOC_LEFT, 0x80),
  [TUPLUS] = OP(FALSE, ASSOC_LEFT, 0x7f),
  [TUMINUS] = OP(FALSE, ASSOC_LEFT, 0x7f),
  [TDIVIDE] = OP(FALSE, ASSOC_NON, 0x70),
  [TMODULO] = OP(FALSE, ASSOC_NON, 0x70),
  [TTIMES] = OP(FALSE, ASSOC_LEFT, 0x60),
  [TUBWNOT] = OP(TRUE, ASSOC_RIGHT, 0x50),
  [TREFDOT] = OP(TRUE, ASSOC_RIGHT, 0x40),
  [TREFBANG] = OP(TRUE, ASSOC_RIGHT, 0x40),
  [TREFSHARP] = OP(TRUE, ASSOC_RIGHT, 0x40),
  [TNULREFDOT] = OP(TRUE, ASSOC_RIGHT, 0x40),
  [TNULREFBANG] = OP(TRUE, ASSOC_RIGHT, 0x40),
  [TNULREFSHARP] = OP(TRUE, ASSOC_RIGHT, 0x40),
  [TCOLON] = OP(FALSE, ASSOC_LEFT, 0x30),
  [TDOT] = OP(FALSE, ASSOC_LEFT, 0x20),
  [TBANG] = OP(FALSE, ASSOC_LEFT, 0x20),
  [TSHARP] = OP(FALSE, ASSOC_LEFT, 0x20),
};

#undef OP

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
