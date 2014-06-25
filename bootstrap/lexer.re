#include "lexer.h"
#include "parser.h"

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

static ERROR block_down(struct parser *parser, enum block_style style) {
  parser->block_style[parser->block_depth] = style;
  parser->block_depth += 1;
  if (parser->block_depth >= ARRAY_SIZE(parser->block_style)) {
    return EINVAL;
  }
  return 0;
}

static void block_up(struct parser *parser, enum block_style style) {
  assert(parser->block_depth > 0);
  assert(parser->block_style[parser->block_depth - 1] == style);

  // Don't erase value, lexer_back() needs it.
  parser->block_depth -= 1;
}

static enum block_style block_style(struct parser *parser) {
  return parser->block_style[parser->block_depth - 1];
}

static void update_codeloc_incr(struct parser *parser, size_t new_pos) {
  for (ssize_t p = parser->codeloc.pos; p != new_pos; ++p) {
    if (parser->data[p] == '\n') {
      parser->codeloc.line += 1;
      parser->codeloc.column = 1;
    } else {
      parser->codeloc.column += 1;
    }
  }

  parser->codeloc.pos = new_pos;
}

static int column(struct parser *parser, size_t new_pos) {
  int c = 0;
  if (parser->data[new_pos] == '\n') {
    new_pos -= 1;
    c = 1;
  }
  for (ssize_t p = new_pos; p >= 0; --p) {
    if (parser->data[p] == '\n') {
      break;
    }
    c += 1;
  }
  return c;
}

static void update_codeloc_decr(struct parser *parser, size_t new_pos) {
  bool recompute_col = parser->data[parser->codeloc.pos] == '\n';

  parser->codeloc.column -= 1;
  for (ssize_t p = parser->codeloc.pos - 1; p != new_pos; --p) {
    if (parser->data[p] == '\n') {
      parser->codeloc.line -= 1;
      recompute_col = true;
    } else {
      parser->codeloc.column -= 1;
    }
  }

  if (parser->data[new_pos] == '\n') {
    parser->codeloc.line -= 1;
    recompute_col = true;
  }

  if (recompute_col) {
    parser->codeloc.column = column(parser, new_pos);
  }

  parser->codeloc.pos = new_pos;
}

static void update_codeloc(struct parser *parser, size_t new_pos) {
  if (new_pos == parser->codeloc.pos) {
    return;
  } else if (new_pos > parser->codeloc.pos) {
    update_codeloc_incr(parser, new_pos);
  } else {
    update_codeloc_decr(parser, new_pos);
  }
}

EXAMPLE(update_codeloc) {
  struct parser parser = { 0 };
  const struct codeloc ZERO = { .pos = 0, .line = 1, .column = 1, };
  parser.codeloc = ZERO;

#define CHECK() do { \
  struct token tok = { \
    .t = 0, \
    .value = parser.data + parser.codeloc.pos, \
    .len = 0, \
  }; \
  assert(parser.codeloc.line == parser_line(&parser, &tok)); \
  assert(parser.codeloc.column == parser_column(&parser, &tok)); \
} while (0)

#define ALL(incr) do { \
  parser.codeloc = ZERO; \
  for (ssize_t n = 0, count = strlen(parser.data); n < count; n += incr) { \
    update_codeloc(&parser, n); \
    CHECK(); \
  } \
  for (ssize_t n = strlen(parser.data) - 1; n >= 0; n -= incr) { \
    update_codeloc(&parser, n); \
    CHECK(); \
  } \
} while (0)

  parser.data = "\n2\n3";
  ALL(1);
  ALL(2);
  ALL(3);

  parser.codeloc = ZERO;
  update_codeloc(&parser, 0);
  CHECK();
  update_codeloc(&parser, 3);
  CHECK();
  update_codeloc(&parser, 0);
  CHECK();

  update_codeloc(&parser, 3);
  CHECK();
  update_codeloc(&parser, 1);
  CHECK();

  parser.data = "test1\ntest2\ntest3";
  ALL(1);
  ALL(2);
  ALL(3);

#undef ALL
#undef CHECK
}

static bool is_space(const char *cursor) {
  const int c = cursor[0];
  return c == ' ' || c == '\t' || c == '\n';
}

error lexer_scan(struct token *tok, struct parser *parser) {
  typedef char YYCTYPE;
  const char *YYCURSOR = parser->data + parser->codeloc.pos;
  const char *YYLIMIT = parser->data + parser->len;
  const char *YYMARKER = NULL;
  const char *start;
  char opening = '\0';
  bool escaped;

  size_t spaces = 0;

#define FAIL(e, fmt, ...) do { \
  __break(); \
  tok->value = start; \
  snprintf(parser->error_message, sizeof(parser->error_message), fmt, ##__VA_ARGS__); \
  return e; \
} while (0)

#define YYFILL(n) do { \
  if (YYCURSOR >= YYLIMIT + 1) { \
    FAIL(EINVAL, "unexpected end-of-file while reading token"); \
  } \
} while (0)

#define R(type) do { \
  update_codeloc(parser, YYCURSOR - parser->data); \
  tok->t = type; \
  tok->value = start; \
  tok->len = YYCURSOR - start; \
  return 0; \
} while (0)

#define RUNORBIN(token_len, un_pre, un_post, bin) do { \
  const bool before = is_space(YYCURSOR - token_len - 1); \
  const bool after = is_space(YYCURSOR); \
  if (before == after) { \
    if (bin == 0) { \
      FAIL(EINVAL, "not a binary operator"); \
    } \
    R(bin); \
  } else if (before && !after) { \
    if (un_pre == 0) { \
      FAIL(EINVAL, "not a prefix operator"); \
    } \
    R(un_pre); \
  } else if (!before && after) { \
    if (un_post == 0) { \
      FAIL(EINVAL, "not a postfix operator"); \
    } \
    R(un_post); \
  } \
} while (0)

  if (parser->inject_eol_after_eob) {
    parser->inject_eol_after_eob = false;
    parser->tok_was_injected = true;
    tok->t = TEOL;
    tok->value = first_eol_back(parser->data, YYCURSOR);
    tok->len = 1;
    return 0;
  }

  parser->tok_was_injected = false;

normal:
  escaped = false;
  start = YYCURSOR;

/*!re2c
  ANY = [\000-\377];

  "--" { goto comment; }
  [ \t\r] { goto normal; }

  "0."[0-9_]+ { R(TNUMBER); }
  "."[0-9_]+ { R(TNUMBER); }
  [1-9][0-9_]*"."[0-9]+ { R(TNUMBER); }
  [1-9][0-9_]* { R(TNUMBER); }
  "0x" [0-9a-fA-F_][0-9a-fA-F_]* { R(TNUMBER); }
  "0" [0-7_]+ { R(TNUMBER); }
  "0" { R(TNUMBER); }

  ["'] { opening = *(YYCURSOR-1); goto string; }

  "struct" { R(Tstruct); }
  "enum" { R(Tenum); }
  "union" { R(Tunion); }
  "extern" { R(Textern); }
  "opaque" { R(Topaque); }
  "fun" { R(Tfun); }
  "met" { R(Tmethod); }
  "shallow" { R(Tshallow); }
  "intf" { R(Tintf); }
  "inline" { R(Tinline); }
  "let" { R(Tlet); }
  "such" { R(Tsuch); }
  "alias" { R(Talias); }
  "if" { R(Tif); }
  "elif" { R(Telif); }
  "else" { R(Telse); }
  "foreach" { R(Tforeach); }
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
  "noop" { R(Tnoop); }
  "pre" { R(Tpre); }
  "post" { R(Tpost); }
  "invariant" { R(Tinvariant); }
  "example" { R(Texample); }
  "sizeof" { R(Tsizeof); }
  "alignof" { R(Talignof); }
  "within" { R(Twithin); }
  "globalenv" { R(Tglobalenv); }
  "<-" { R(TBARROW); }
  "->" {
    error e = block_down(parser, BLOCK_SINGLE);
    if (e) {
      FAIL(e, "lexer: too many block levels");
    }

    parser->indent += 8;
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
  "ov+=" { R(TOVPLUS_ASSIGN); }
  "ov-=" { R(TOVMINUS_ASSIGN); }
  "ov*=" { R(TOVTIMES_ASSIGN); }
  "ov/=" { R(TOVDIVIDE_ASSIGN); }
  "ov%=" { R(TOVMODULO_ASSIGN); }
  "bw&=" { R(TBWAND_ASSIGN); }
  "bw|=" { R(TBWOR_ASSIGN); }
  "bw^=" { R(TBWXOR_ASSIGN); }
  "==" { R(TEQ); }
  "!=" { R(TNE); }
  "===" { R(TEQPTR); }
  "!==" { R(TNEPTR); }
  "<=" { R(TLE); }
  "<" { R(TLT); }
  ">=" { R(TGE); }
  ">" { R(TGT); }
  "=" { R(TASSIGN); }
  "+" { RUNORBIN(1, TUPLUS, 0, TPLUS); }
  "-" { RUNORBIN(1, TUMINUS, 0, TMINUS); }
  "*" { R(TTIMES); }
  "/" { R(TDIVIDE); }
  "%" { R(TMODULO); }
  "|" { R(TPATTERNOR); }
  "ov+" { RUNORBIN(3, TOVUPLUS, 0, TOVPLUS); }
  "ov-" { RUNORBIN(3, TOVUMINUS, 0, TOVMINUS); }
  "ov*" { R(TTIMES); }
  "ov/" { R(TDIVIDE); }
  "ov%" { R(TMODULO); }
  "bw&" { R(TBWAND); }
  "bw|" { R(TBWOR); }
  "bw^" { R(TBWXOR); }
  "bw~" { R(TBWNOT); }
  "::" { R(TDCOLON); }
  ":" { R(TCOLON); }
  "," { R(TCOMMA); }
  ";;" {
    if (block_style(parser) == BLOCK_SINGLE) {
      parser->indent -= 8;
      block_up(parser, BLOCK_SINGLE);
      R(TEOB);
    } else {
      FAIL(EINVAL, "lexer: unexpected double-semicolon in multi-line block");
    }
  }
  ";" {
    if (block_style(parser) == BLOCK_SINGLE) {
      R(TEOL);
    } else {
      FAIL(EINVAL, "lexer: unexpected semicolon in multi-line block");
    }
  }
  "..." { R(TDOTDOTDOT); }
  ".." { R(TDOTDOT); }
  "?.[" { R(TOPTATDOT); }
  "?![" { R(TOPTATBANG); }
  "?$[" { R(TOPTATWILDCARD); }
  "?." { RUNORBIN(2, 0, 0, TOPTDOT); }
  "?!" { RUNORBIN(2, 0, 0, TOPTBANG); }
  "?#" { RUNORBIN(2, 0, 0, TOPTSHARP); }
  "?$" { RUNORBIN(2, 0, 0, TOPTWILDCARD); }
  ".[" { R(TATDOT); }
  "![" { R(TATBANG); }
  "$[" { R(TATWILDCARD); }
  "." { RUNORBIN(1, 0, TDEREFDOT, TDOT); }
  "!" { RUNORBIN(1, 0, TDEREFBANG, TBANG); }
  "#" { RUNORBIN(1, 0, TDEREFSHARP, TSHARP); }
  "$" { RUNORBIN(1, 0, TDEREFWILDCARD, TWILDCARD); }
  "@!" { R(TREFBANG); }
  "@#" { R(TREFSHARP); }
  "@$" { R(TREFWILDCARD); }
  "@" { R(TREFDOT); }
  "?@!" { R(TNULREFBANG); }
  "?@#" { R(TNULREFSHARP); }
  "?@$" { R(TNULREFWILDCARD); }
  "?@" { R(TNULREFDOT); }
  "?" { RUNORBIN(1, TPREQMARK, TPOSTQMARK, 0); }
  "[]" { R(TSLICEBRAKETS); }
  "[]!" { R(TMSLICEBRAKETS); }
  "[]$" { R(TWSLICEBRAKETS); }
  "]" { R(TRSBRA); }
  "{" { R(TLCBRA); }
  "}" { R(TRCBRA); }
  "(" { R(TLPAR); }
  ")" { R(TRPAR); }

  "\n" {
    YYCURSOR -= 1;
    goto eol;
  }

  "\\" {
    if (YYCURSOR >= YYLIMIT) {
      FAIL(EINVAL, "escape character at the EOF");
    }
    FAIL(EINVAL, "invalid character following '\\'");
  }

  [`a-zA-Z_][a-zA-Z_0-9]* { R(TIDENT); }

  ANY { FAIL(EINVAL, "lexer: illegal char \\0%hho '%c'", *(YYCURSOR - 1), *(YYCURSOR - 1)); }
 */

eol:
/*!re2c
  " " {
    FAIL(EINVAL, "indentation must be made of tabs, not spaces");
  }
  "\t" { spaces += 8; goto eol; }
  "\r" { goto eol; }
  "\r\n"[\t]*"\\" {
    spaces = 8 * (YYCURSOR - start - 3);
    escaped = true;
    goto escaped_eol_any;
  }
  "\n"[\t]*"\\" {
    spaces = 8 * (YYCURSOR - start - 2);
    escaped = true;
    goto escaped_eol_any;
  }
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

escaped_eol_any:
  if (spaces == parser->indent) {
    switch (block_style(parser)) {
    case BLOCK_MULTI:
      if (escaped) {
        goto normal;
      } else {
        R(TEOL);
      }
      break;
    case BLOCK_MULTI_ESCAPED:
      if (!escaped) {
        FAIL(EINVAL, "once in escape mode, further lines must be escaped");
      }
      goto normal;
    case BLOCK_SINGLE:
      if (escaped) {
        FAIL(EINVAL, "escaped lines must be indented");
      }
      parser->inject_eol_after_eob = true;
      block_up(parser, BLOCK_SINGLE);
      R(TEOB);
      break;
    case BLOCK_SINGLE_ESCAPED:
      if (!escaped) {
        FAIL(EINVAL, "reduce indent to close an escaped line");
      }
      goto normal;
    }
  } else if (spaces > parser->indent) {
    if (spaces != parser->indent + 8) {
      FAIL(EINVAL, "cannot increase indentation by more than one tab");
    }

    parser->indent = spaces;

    enum block_style starting = 0;
    switch (block_style(parser)) {
    case BLOCK_MULTI:
    case BLOCK_MULTI_ESCAPED:
      starting = escaped ? BLOCK_MULTI_ESCAPED : BLOCK_MULTI;
      break;
    case BLOCK_SINGLE:
      if (escaped) {
        goto normal;
      } else {
        FAIL(EINVAL, "in single line mode, further indented lines must be escaped");
      }
      break;
    case BLOCK_SINGLE_ESCAPED:
      FAIL(EINVAL, "in single line escaped mode, cannot have further indented lines");
      break;
    }

    error e = block_down(parser, starting);
    if (e) {
      FAIL(e, "lexer: too many block levels");
    }

    if (starting == BLOCK_MULTI) {
      R(TSOB);
    } else {
      goto normal;
    }
  } else if (spaces < parser->indent) {
    assert(spaces % 8 == 0);

    parser->indent -= 8;

    switch (block_style(parser)) {
    case BLOCK_MULTI:
    case BLOCK_SINGLE:
      if (escaped || spaces != parser->indent) {
        // Closing several blocks at once, return token for the others.
        YYCURSOR = start;
        parser->tok_was_injected = true;
      }

      // EOB must be followed by an EOL.
      parser->inject_eol_after_eob = !escaped || spaces != parser->indent;
      block_up(parser, block_style(parser));
      R(TEOB);
      break;
    case BLOCK_MULTI_ESCAPED:
      if (spaces != parser->indent) {
        // Closing several blocks at once, return token for the others.
        YYCURSOR = start;
      }
      block_up(parser, block_style(parser));
      if (spaces == parser->indent) {
        R(TEOL);
      }
      goto normal;
    default:
      break;
    }
  } else {
    assert(false);
  }

string:
/*!re2c
  [\\] {
    if (YYCURSOR >= YYLIMIT) {
      FAIL(EINVAL, "non-terminated string");
    }
    if (*YYCURSOR == opening) {
      YYCURSOR += 1; goto string;
    } else {
      goto string;
    }
  }
  [\n] {
    FAIL(EINVAL, "string literal contains a newline");
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
  if (tok->t == TSOB) {
    parser->indent -= 8;
    parser->block_depth -= 1;
  } else if (tok->t == TEOB) {
    parser->block_depth += 1;
    parser->indent += 8;
  }

  if (parser->tok_was_injected) {
    parser->inject_eol_after_eob = tok->t == TEOL;
    return;
  }

  parser->inject_eol_after_eob = false;

  assert(tok->len < parser->codeloc.pos);
  update_codeloc(parser, parser->codeloc.pos - tok->len);
}
