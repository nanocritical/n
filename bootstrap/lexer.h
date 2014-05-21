#ifndef LEXER_H__
#define LEXER_H__

#include <stdint.h>
#include <stdlib.h>

#include "common.h"

enum token_type {
  Timport = 1,
  Texport,
  Tfrom,
  Tstruct,
  Tenum,
  Tunion,
  Textern,
  Topaque,
  Tfun,
  Tmethod,
  Tshallow,
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
  Talignof,
  Twithin,
  Tglobalenv,

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
  TPATTERNOR,
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
  TBARROW,
  TEOL,
  TSOB,
  TEOB,
  TCOLON,
  TDCOLON,
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
  T__NOT_COLON,
  T__NOT_COMMA,

  TOKEN__NUM,
};

const char *token_strings[TOKEN__NUM];

enum operator_associativity {
  ASSOC_NON = 0x10000,
  ASSOC_LEFT = 0x20000,
  ASSOC_LEFT_SAME = 0x30000,
  ASSOC_RIGHT = 0x40000,
};

enum operator_kind {
  OP_UN_REFOF = 0x1,
  OP_UN_DEREF = 0x2,
  OP_UN_BOOL = 0x3,
  OP_UN_ARITH = 0x4,
  OP_UN_BW = 0x5,
  OP_BIN = 0x10,
  OP_BIN_SYM = 0x20,
  OP_BIN_SYM_BOOL = 0x30,
  OP_BIN_SYM_ARITH = 0x40,
  OP_BIN_SYM_BW = 0x50,
  OP_BIN_SYM_PTR = 0x60,
  OP_BIN_ACC = 0x70,
  OP_BIN_BW_RHS_UNSIGNED = 0x80,
  OP_BIN_RHS_TYPE = 0x90,
  OP_ASSIGN = 0x1,
  OP_UN__MASK = 0x0f,
  OP_BIN__MASK = 0xf0,
  OP_KIND__MASK = 0xff,
  OP_ASSIGN__MASK = 0x1,
};

#define OP(assign, kind, assoc, precedence) ((assign << 27) | (kind << 19) | assoc | precedence)
#define IS_OP(t) (operators[t] != 0)
#define OP_IS_UNARY(t) (!!((operators[t] >> 19) & OP_UN__MASK))
#define OP_IS_BINARY(t) (!!((operators[t] >> 19) & OP_BIN__MASK))
#define OP_ASSOC(t) (operators[t] & 0x30000)
#define OP_PREC(t) (operators[t] & 0xffff)
#define OP_KIND(t) ((operators[t] >> 19) & OP_KIND__MASK)
#define OP_IS_ASSIGN(t) (!!((operators[t] >> 27) & OP_ASSIGN__MASK))

static const uint32_t operators[TOKEN__NUM] = {
  [T__STATEMENT] = OP(0, 0, 0, 0xffff),
  [TASSIGN] = OP(OP_ASSIGN, OP_BIN_SYM, ASSOC_NON, 0x140),
  [TPLUS_ASSIGN] = OP(OP_ASSIGN, OP_BIN_SYM_ARITH, ASSOC_NON, 0x140),
  [TMINUS_ASSIGN] = OP(OP_ASSIGN, OP_BIN_SYM_ARITH, ASSOC_NON, 0x140),
  [TTIMES_ASSIGN] = OP(OP_ASSIGN, OP_BIN_SYM_ARITH, ASSOC_NON, 0x140),
  [TDIVIDE_ASSIGN] = OP(OP_ASSIGN, OP_BIN_SYM_ARITH, ASSOC_NON, 0x140),
  [TMODULO_ASSIGN] = OP(OP_ASSIGN, OP_BIN_SYM_ARITH, ASSOC_NON, 0x140),
  [TBWAND_ASSIGN] = OP(OP_ASSIGN, OP_BIN_SYM_BW, ASSOC_NON, 0x140),
  [TBWOR_ASSIGN] = OP(OP_ASSIGN, OP_BIN_SYM_BW, ASSOC_NON, 0x140),
  [TBWXOR_ASSIGN] = OP(OP_ASSIGN, OP_BIN_SYM_BW, ASSOC_NON, 0x140),
  [TRSHIFT_ASSIGN] = OP(OP_ASSIGN, OP_BIN_BW_RHS_UNSIGNED, ASSOC_NON, 0x140),
  [TLSHIFT_ASSIGN] = OP(OP_ASSIGN, OP_BIN_BW_RHS_UNSIGNED, ASSOC_NON, 0x140),
  [Tin] = OP(0, OP_BIN_SYM, ASSOC_NON, 0x140),
  [T__NOT_STATEMENT] = OP(0, OP_BIN, ASSOC_NON, 0x139),
  [TCOMMA] = OP(0, OP_BIN, ASSOC_LEFT, 0x130),
  [T__NOT_COMMA] = OP(0, OP_BIN, ASSOC_NON, 0x129),
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
  [TBARROW] = OP(0, OP_BIN, ASSOC_RIGHT, 0xcf),
  [TBWOR] = OP(0, OP_BIN_SYM_BW, ASSOC_LEFT_SAME, 0x90),
  [TBWXOR] = OP(0, OP_BIN_SYM_BW, ASSOC_LEFT_SAME, 0x90),
  [TBWAND] = OP(0, OP_BIN_SYM_BW, ASSOC_LEFT_SAME, 0x90),
  [TLSHIFT] = OP(0, OP_BIN_BW_RHS_UNSIGNED, ASSOC_NON, 0x90),
  [TRSHIFT] = OP(0, OP_BIN_BW_RHS_UNSIGNED, ASSOC_NON, 0x90),
  [TPLUS] = OP(0, OP_BIN_SYM_ARITH, ASSOC_LEFT, 0x80),
  [TMINUS] = OP(0, OP_BIN_SYM_ARITH, ASSOC_LEFT, 0x80),
  [TDIVIDE] = OP(0, OP_BIN_SYM_ARITH, ASSOC_LEFT, 0x60),
  [TMODULO] = OP(0, OP_BIN_SYM_ARITH, ASSOC_LEFT, 0x60),
  [TTIMES] = OP(0, OP_BIN_SYM_ARITH, ASSOC_LEFT, 0x60),
  [TUPLUS] = OP(0, OP_UN_ARITH, ASSOC_NON, 0x50),
  [TUMINUS] = OP(0, OP_UN_ARITH, ASSOC_NON, 0x50),
  [TBWNOT] = OP(0, OP_UN_BW, ASSOC_NON, 0x50),
  [TREFDOT] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TREFBANG] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TREFSHARP] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TREFWILDCARD] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TNULREFDOT] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TNULREFBANG] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TNULREFSHARP] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TNULREFWILDCARD] = OP(0, OP_UN_REFOF, ASSOC_RIGHT, 0x40),
  [TDCOLON] = OP(0, OP_BIN_RHS_TYPE, ASSOC_LEFT, 0x31),
  [TCOLON] = OP(0, OP_BIN_RHS_TYPE, ASSOC_LEFT, 0x30),
  [T__NOT_COLON] = OP(0, OP_BIN, ASSOC_NON, 0x29),
  [TDEREFDOT] = OP(0, OP_UN_DEREF, ASSOC_LEFT, 0x20),
  [TDEREFBANG] = OP(0, OP_UN_DEREF, ASSOC_LEFT, 0x20),
  [TDEREFSHARP] = OP(0, OP_UN_DEREF, ASSOC_LEFT, 0x20),
  [TDEREFWILDCARD] = OP(0, OP_UN_DEREF, ASSOC_LEFT, 0x20),
  [TDOT] = OP(0, OP_BIN_ACC, ASSOC_LEFT, 0x1f),
  [TBANG] = OP(0, OP_BIN_ACC, ASSOC_LEFT, 0x1f),
  [TSHARP] = OP(0, OP_BIN_ACC, ASSOC_LEFT, 0x1f),
  [TWILDCARD] = OP(0, OP_BIN_ACC, ASSOC_LEFT, 0x1f),
};

static const bool expr_terminators[TOKEN__NUM] = {
  [TEOL] = true,
  [TSOB] = true,
  [TEOB] = true,
  [TRPAR] = true,
  [TRSBRA] = true,
  [TRCBRA] = true,
  [Twithin] = true,
  [Tsuch] = true,
};

struct codeloc {
  size_t pos;
  int line;
  int column;
};

struct parser {
  const char *data;
  size_t len;
  struct codeloc codeloc;

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
