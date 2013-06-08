#include "common.h"
#include "lexer.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

static const char *first_eol_back(const char *start, const char *cur) {
  while (cur >= start) {
    if (cur[0] == '\n') {
      return cur;
    }
    cur -= 1;
  }
  return start;
}

static error block_down(struct parser *parser, bool oneline) {
  parser->block_style[parser->block_depth] = oneline;
  parser->block_depth += 1;
  if (parser->block_depth >= ARRAY_SIZE(parser->block_style)) {
    return EINVAL;
  }
  return 0;
}

static void block_up(struct parser *parser, bool oneline) {
  assert(parser->block_depth > 0);
  assert(parser->block_style[parser->block_depth - 1] == oneline);

  // Don't erase value, lexer_back() needs it.
  parser->block_depth -= 1;
}

static bool block_style(struct parser *parser) {
  return parser->block_style[parser->block_depth - 1];
}

error lexer_scan(struct token *tok, struct parser *parser) {
  typedef char YYCTYPE;
  const char *YYCURSOR = parser->data + parser->pos;
  const char *YYLIMIT = parser->data + parser->len;
  const char *YYMARKER = NULL;
  const char *start;
  char opening;

  size_t spaces = 0;

#define ERROR(e, fmt, ...) do { \
  tok->value = start; \
  snprintf(parser->error_message, sizeof(parser->error_message), fmt, ##__VA_ARGS__); \
  return e; \
} while (0)

#define YYFILL(n) do { \
  if (YYCURSOR >= YYLIMIT + 1) { \
    ERROR(EINVAL, "unexpected end-of-file while reading token"); \
  } \
} while (0)

#define R(type) do { \
  parser->pos = YYCURSOR - parser->data; \
  tok->t = type; \
  tok->value = start; \
  tok->len = YYCURSOR - start; \
  return 0; \
} while (0)

  if (parser->inject_eol_after_eob) {
    parser->inject_eol_after_eob = FALSE;
    parser->tok_was_injected = TRUE;
    tok->t = TEOL;
    tok->value = first_eol_back(parser->data, YYCURSOR);
    tok->len = 1;
    return 0;
  }

  parser->tok_was_injected = FALSE;

normal:
  start = YYCURSOR;

/*!re2c
  ANY = [\000-\377];

  "--" { goto comment; }
  [ \t\r] { goto normal; }

  "0."[0-9]+ { R(TNUMBER); }
  "."[0-9]+ { R(TNUMBER); }
  [1-9][0-9]*"."[0-9]+ { R(TNUMBER); }
  [1-9][0-9]* { R(TNUMBER); }
  "0x" [0-9a-fA-F][0-9a-fA-F]* { R(TNUMBER); }
  "0" [0-7]+ { R(TNUMBER); }
  "0" { R(TNUMBER); }

  ["'] { opening = *(YYCURSOR-1); goto string; }

  "type" { R(Ttype); }
  "extern" { R(Textern); }
  "fun" { R(Tfun); }
  "method" { R(Tmethod); }
  "intf" { R(Tintf); }
  "inline" { R(Tinline); }
  "let" { R(Tlet); }
  "alias" { R(Talias); }
  "if" { R(Tif); }
  "elif" { R(Telif); }
  "else" { R(Telse); }
  "for" { R(Tfor); }
  "in" { R(Tin); }
  "while" { R(Twhile); }
  "continue" { R(Tcontinue); }
  "break" { R(Tbreak); }
  "match" { R(Tmatch); }
  "return" { R(Treturn); }
  "try" { R(Ttry); }
  "catch" { R(Tcatch); }
  "except" { R(Texcept); }
  "throw" { R(Tthrow); }
  "block" { R(Tblock); }
  "import" { R(Timport); }
  "export" { R(Texport); }
  "from" { R(Tfrom); }
  "delegate" { R(Tdelegate); }
  "declare" { R(Tdeclare); }
  "and" { R(Tand); }
  "or" { R(Tor); }
  "not" { R(Tnot); }
  "false" { R(Tfalse); }
  "true" { R(Ttrue); }
  "isa" { R(Tisa); }
  "null" { R(Tnull); }
  "pass" { R(Tpass); }
  "pre" { R(Tpre); }
  "post" { R(Tpost); }
  "invariant" { R(Tinvariant); }
  "example" { R(Texample); }
  "sizeof" { R(Tsizeof); }
  "->" {
    error e = block_down(parser, TRUE);
    if (e) {
      ERROR(e, "lexer: too many block levels");
    }

    parser->indent += 2;
    R(TSOB);
  }
  ">>=" { R(TRSHIFT_ASSIGN); }
  "<<=" { R(TLSHIFT_ASSIGN); }
  ">>" { R(TRSHIFT); }
  "<<" { R(TLSHIFT); }
  "+=" { R(TPLUS_ASSIGN); }
  "-=" { R(TMINUS_ASSIGN); }
  "*=" { R(TTIMES_ASSIGN); }
  "/=" { R(TDIVIDE_ASSIGN); }
  "%=" { R(TMODULO_ASSIGN); }
  "&=" { R(TBWAND_ASSIGN); }
  "|=" { R(TBWOR_ASSIGN); }
  "^=" { R(TBWXOR_ASSIGN); }
  "==" { R(TEQ); }
  "!=" { R(TNE); }
  "===" { R(TEQPTR); }
  "!==" { R(TNEPTR); }
  "<=" { R(TLE); }
  "<" { R(TLT); }
  ">=" { R(TGE); }
  ">" { R(TGT); }
  "=" { R(TASSIGN); }
  "+" { R(TPLUS); }
  "-" { R(TMINUS); }
  "*" { R(TTIMES); }
  "/" { R(TDIVIDE); }
  "%" { R(TMODULO); }
  "&" { R(TBWAND); }
  "|" { R(TBWOR); }
  "^" { R(TBWXOR); }
  "~" { R(TBWNOT); }
  ":" { R(TCOLON); }
  "," { R(TCOMMA); }
  ";" {
    if (block_style(parser)) {
      R(TEOL);
    } else {
      ERROR(EINVAL, "lexer: unexpected semicolon in multi-line block");
    }
  }
  "."[^a-zA-Z_] { YYCURSOR -= 1; R(TDEREFDOT); }
  "!"[^a-zA-Z_] { YYCURSOR -= 1; R(TDEREFBANG); }
  "#"[^a-zA-Z_] { YYCURSOR -= 1; R(TDEREFSHARP); }
  "$"[^a-zA-Z_] { YYCURSOR -= 1; R(TDEREFWILDCARD); }
  "." { R(TDOT); }
  "!" { R(TBANG); }
  "#" { R(TSHARP); }
  "$" { R(TWILDCARD); }
  "@!" { R(TREFBANG); }
  "@#" { R(TREFSHARP); }
  "@$" { R(TREFWILDCARD); }
  "@" { R(TREFDOT); }
  "?@!" { R(TNULREFBANG); }
  "?@#" { R(TNULREFSHARP); }
  "?@$" { R(TNULREFWILDCARD); }
  "?@" { R(TNULREFDOT); }
  "?" { R(TQMARK); }
  "..." { R(TDOTDOTDOT); }
  "[]" { R(TSLICEBRAKETS); }
  "[" { parser->no_block_depth += 1; R(TLSBRA); }
  "]" { parser->no_block_depth -= 1; R(TRSBRA); }
  "{" { parser->no_block_depth += 1; R(TLCBRA); }
  "}" { parser->no_block_depth -= 1; R(TRCBRA); }
  "(" { parser->no_block_depth += 1; R(TLPAR); }
  ")" { parser->no_block_depth -= 1; R(TRPAR); }

  "\n" { 
    spaces = 0;
    goto eol;
  }

  "\\" {
    if (YYCURSOR >= YYLIMIT) {
      ERROR(EINVAL, "escape character at the EOF");
    }
    if (*YYCURSOR == '\n') {
      YYCURSOR += 1;
      goto normal;
    } else {
      ERROR(EINVAL, "invalid character following '\\'");
    }
  }

  [a-zA-Z_][a-zA-Z_0-9]* { R(TIDENT); }

  ANY { ERROR(EINVAL, "lexer: illegal char '\\0%hho'", *(YYCURSOR - 1)); }
 */

eol:
/*!re2c
  " " { spaces += 1; goto eol; }
  "\t" { spaces += 8; goto eol; }
  "\r" { goto eol; }
  "\n" { spaces = 0; goto eol; }
  "-" {
    YYFILL(1);
    if (*YYCURSOR == '-') {
      YYCURSOR += 1;
      goto comment_while_eol;
    } else {
      goto eol_any;
    }
  }
  ANY { goto eol_any; }
 */

eol_any:
  YYCURSOR -= 1;
  if (parser->no_block_depth > 0) {
    goto normal;
  }

  if (spaces == parser->indent) {
    if (block_style(parser)) {
      parser->inject_eol_after_eob = TRUE;
      block_up(parser, TRUE);
      R(TEOB);
    } else {
      R(TEOL);
    }
  } else if (spaces == parser->indent + 2) {
    parser->indent = spaces;

    error e = block_down(parser, FALSE);
    if (e) {
      ERROR(e, "lexer: too many block levels");
    }

    R(TSOB);
  } else if (spaces < parser->indent) {
    if (spaces % 2 != 0) {
      ERROR(EINVAL, "lexer: indentation must be a multiple of 2 spaces"
              " (one tab counts for 8 spaces), not %zu", spaces);
    }

    parser->indent -= 2;

    if (spaces != parser->indent) {
      // Closing several blocks at once, return token for the others.
      YYCURSOR = start;
      parser->tok_was_injected = TRUE;
    }

    // EOB must be followed by an EOL.
    parser->inject_eol_after_eob = TRUE;

    block_up(parser, block_style(parser));
    R(TEOB);
  } else {
    ERROR(EINVAL, "indentation must be a multiple of 2 spaces"
            " (one tab counts for 8 spaces), not %zu", spaces);
  }

string:
/*!re2c
  [\\] {
    if (YYCURSOR >= YYLIMIT) {
      ERROR(EINVAL, "non-terminated string");
    }
    if (*YYCURSOR == opening) {
      YYCURSOR += 1; goto string;
    } else {
      goto string;
    }
  }
  [\n] {
    ERROR(EINVAL, "string literal contains a newline");
  }
  ["'] {
    if (*(YYCURSOR - 1) != opening) {
      goto string;
    } else {
      R(TSTRING);
    }
  }
  ANY { goto string; }
 */

comment:
/*!re2c
  "\n" { YYCURSOR -= 1; goto normal; }
  ANY { goto comment; }
 */

comment_while_eol:
/*!re2c
  "\n" { YYCURSOR -= 1; goto eol; }
  ANY { goto comment; }
 */
}

void lexer_back(struct parser *parser, const struct token *tok) {
  parser->inject_eol_after_eob = FALSE;

  if (tok->t == TLSBRA) {
    parser->no_block_depth -= 1;
  } else if (tok->t == TRSBRA) {
    parser->no_block_depth += 1;
  } else if (tok->t == TLCBRA) {
    parser->no_block_depth -= 1;
  } else if (tok->t == TRCBRA) {
    parser->no_block_depth += 1;
  } else if (tok->t == TLPAR) {
    parser->no_block_depth -= 1;
  } else if (tok->t == TRPAR) {
    parser->no_block_depth += 1;
  } else if (tok->t == TSOB) {
    parser->indent -= 2;
    parser->block_depth -= 1;
  } else if (tok->t == TEOB) {
    parser->block_depth += 1;
    parser->indent += 2;
  }

  if (parser->tok_was_injected) {
    return;
  }

  assert(tok->len < parser->pos);
  parser->pos -= tok->len;
}
