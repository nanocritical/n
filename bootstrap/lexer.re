// vi: ft=c

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

error lexer_open_implicit_single_block(struct parser *parser) {
  error e = block_down(parser, BLOCK_SINGLE);
  if (e) {
    EXCEPTF(e, "lexer: too many block levels");
  }

  parser->indent += 8;
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

static bool update_codeloc_incr(struct parser *parser, size_t new_pos) {
  bool across = false;
  for (ssize_t p = parser->codeloc.pos; p != new_pos; ++p) {
    if (parser->data[p] == '\n') {
      parser->codeloc.line += 1;
      parser->codeloc.column = 1;

      if (parser->codeloc.pos < parser->next_component_first_pos
          && (p == parser->next_component_first_pos - 1
              || p == parser->next_component_first_pos)) {
        parser->codeloc.line = 1;
        // Module files end with a newline. And not in the middle of a token.
        across = new_pos > parser->next_component_first_pos;
      }
    } else {
      parser->codeloc.column += 1;
    }
  }

  parser->codeloc.pos = new_pos;
  return across;
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

// Returns true if we just moved to a new module component file since the
// last update.
static bool update_codeloc(struct parser *parser, size_t new_pos) {
  if (new_pos == parser->codeloc.pos) {
    return false;
  } else if (new_pos > parser->codeloc.pos) {
    return update_codeloc_incr(parser, new_pos);
  } else {
    update_codeloc_decr(parser, new_pos);
    return false;
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
  struct module mod = { .parser = parser, 0 }; \
  assert(parser.codeloc.line == parser_line(&mod, &tok)); \
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

static bool is_pre_space(bool *deep, const char *data, const char *cursor) {
  static const bool lut[256] = {
    [' '] = true,
    ['\t'] = true,
    ['\n'] = true,
    [':'] = true,
    ['('] = true,
    ['['] = true,
    ['{'] = true,
    ['='] = true,
    [','] = true,
  };
  static const bool lut_deeper[256] = {
    ['*'] = true,
    ['!'] = true,
    ['#'] = true,
    ['$'] = true,
    ['?'] = true,
    [']'] = true,
  };

  int c = cursor[0];
  if (lut[c]) {
    return true;
  }

  while (cursor != data && lut_deeper[c]) {
    cursor -= 1;
    c = cursor[0];
    if (lut[c]) {
      *deep = true;
      return false;
    }
  }
  return false;
}

static bool is_post_space(bool *deep, const char *limit, const char *cursor) {
  static const bool lut[256] = {
    [' '] = true,
    ['\t'] = true,
    ['\n'] = true,
    [':'] = true,
    [')'] = true,
    [']'] = true,
    ['}'] = true,
    ['='] = true,
    [','] = true,
  };
  static const bool lut_deeper[256] = {
    ['*'] = true,
    ['!'] = true,
    ['#'] = true,
    ['$'] = true,
    ['?'] = true,
  };

  int c = cursor[0];
  if (lut[c]) {
    return true;
  }

  while (cursor != limit && lut_deeper[c]) {
    cursor += 1;
    c = cursor[0];
    if (lut[c]) {
      *deep = true;
      return false;
    }
  }
  return false;
}

error lexer_scan(struct token *tok, struct parser *parser) {
  typedef char YYCTYPE;
  const char *YYCURSOR = parser->data + parser->codeloc.pos;
  const char *YYLIMIT = parser->data + parser->len;
  const char *YYMARKER = NULL;
  const char *start;
  char opening = '\0';

  size_t spaces = 0;
  bool escaped = false;

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
  bool before_deep = false, after_deep = false; \
  const bool before = is_pre_space(&before_deep, parser->data, YYCURSOR - token_len - 1); \
  const bool after = is_post_space(&after_deep, YYLIMIT, YYCURSOR); \
  if (before == after && !before_deep && !after_deep) { \
    if (bin == 0) { \
      FAIL(EINVAL, "not a binary operator"); \
    } \
    R(bin); \
  } else if ((before || before_deep) && !after) { \
    if (un_pre == 0) { \
      FAIL(EINVAL, "not a prefix operator"); \
    } \
    R(un_pre); \
  } else if (!before && (after || after_deep)) { \
    if (un_post == 0) { \
      FAIL(EINVAL, "not a postfix operator"); \
    } \
    R(un_post); \
  } else { \
    assert(false); \
  } \
} while (0)

#define RATUNORBIN(token_len, bin, pre_un) do { \
  bool before_deep = false; \
  const bool before = is_pre_space(&before_deep, parser->data, YYCURSOR - token_len - 1); \
  if (before) { \
    YYCURSOR -= 1; \
    R(pre_un); \
  } else { \
    R(bin); \
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
  start = YYCURSOR;
  escaped = false;

/*!re2c
  ANY = [\000-\377];

  "--(" { goto long_comment; }
  "--" { goto comment; }
  [ \t\r] { goto normal; }

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
  "newtype" { R(Tnewtype); }
  "inline" { R(Tinline); }
  "let" { R(Tlet); }
  "var" { R(Tlet); }
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
  "drop" { R(Tdrop); }
  "fatal" { R(Tfatal); }
  "never" { R(Tnever); }
  "throw" { R(Tthrow); }
  "block" { R(Tblock); }
  "import" { R(Timport); }
  "export" { R(Texport); }
  "from" { R(Tfrom); }
  "build" { R(Tbuild); }
  "declare" { R(Tdeclare); }
  "and" { R(Tand); }
  "or" { R(Tor); }
  "not" { R(Tnot); }
  "false" { R(Tfalse); }
  "true" { R(Ttrue); }
  "isa" { R(Tisa); }
  "nil" { R(Tnil); }
  "noop" { R(Tnoop); }
  "assert" { R(Tassert); }
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
  "ov<<=" { R(TOVLSHIFT_ASSIGN); }
  ">>" { R(TRSHIFT); }
  "ov<<" { R(TOVLSHIFT); }
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
  "&=" { R(TBWAND_ASSIGN); }
  "|=" { R(TBWOR_ASSIGN); }
  "^=" { R(TBWXOR_ASSIGN); }
  "==" { R(TEQ); }
  "!=" { R(TNE); }
  "===" { R(TEQPTR); }
  "!==" { R(TNEPTR); }
  "==|" { R(TEQMATCH); }
  "!=|" { R(TNEMATCH); }
  "<=" { R(TLE); }
  "<" { R(TLT); }
  ">=" { R(TGE); }
  ">" { R(TGT); }
  "=" { R(TASSIGN); }
  "+" { RUNORBIN(1, TUPLUS, 0, TPLUS); }
  "-" { RUNORBIN(1, TUMINUS, 0, TMINUS); }
  "/" { R(TDIVIDE); }
  "%" { R(TMODULO); }
  "ov+" { RUNORBIN(3, TOVUPLUS, 0, TOVPLUS); }
  "ov-" { RUNORBIN(3, TOVUMINUS, 0, TOVMINUS); }
  "ov*" { R(TTIMES); }
  "ov/" { R(TDIVIDE); }
  "ov%" { R(TMODULO); }
  "&" { R(TBWAND); }
  "|" { R(TBWOR); }
  "^" { R(TBWXOR); }
  "~" { R(TBWNOT); }
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
  "?![" { RATUNORBIN(3, TOPTATBANG, TNULREFBANG); }
  "?$[" { RATUNORBIN(3, TOPTATWILDCARD, TNULREFWILDCARD); }
  ".[" { R(TATDOT); }
  "![" { RATUNORBIN(2, TATBANG, TREFBANG); }
  "$[" { RATUNORBIN(2, TATWILDCARD, TREFWILDCARD); }
  "?*" { R(TNULREFDOT); }
  "?@" { R(TNULREFDOT); }
  "weak ?@" { R(TNULREFDOT); }
  "?." { R(TOPTDOT); }
  "?!" { RUNORBIN(2, TNULREFBANG, 0, TOPTBANG); }
  "?@!" { RUNORBIN(3, TNULREFBANG, 0, 0); }
  "weak ?@!" { RUNORBIN(7, TNULREFBANG, 0, 0); }
  "?#" { RUNORBIN(2, TNULREFSHARP, 0, TOPTSHARP); }
  "?@#" { RUNORBIN(3, TNULREFSHARP, 0, 0); }
  "weak ?@#" { RUNORBIN(7, TNULREFSHARP, 0, 0); }
  "?$" { RUNORBIN(2, TNULREFWILDCARD, 0, TOPTWILDCARD); }
  "*" { RUNORBIN(1, TREFDOT, TDEREFDOT, TTIMES); }
  "@" { RUNORBIN(1, TREFDOT, 0, 0); }
  "." { RUNORBIN(1, TPREDOT, 0, TDOT); }
  "!" { RUNORBIN(1, TREFBANG, TDEREFBANG, TBANG); }
  "@!" { RUNORBIN(2, TREFBANG, 0, 0); }
  "#" { RUNORBIN(1, TREFSHARP, TDEREFSHARP, TSHARP); }
  "@#" { RUNORBIN(2, TREFSHARP, 0, 0); }
  "$" { RUNORBIN(1, TREFWILDCARD, TDEREFWILDCARD, TWILDCARD); }
  "?" { RUNORBIN(1, TPREQMARK, TPOSTQMARK, 0); }
  "[]" { R(TSLICEBRAKETS); }
  "[!]" { R(TMSLICEBRAKETS); }
  "[$]" { R(TWSLICEBRAKETS); }
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

  "0."[0-9_]+ { R(TNUMBER); }
  "."[0-9_]+ { R(TNUMBER); }
  [1-9][0-9_]*"."[0-9]+ { R(TNUMBER); }
  [1-9][0-9_]* { R(TNUMBER); }
  "0x" [0-9a-fA-F_][0-9a-fA-F_]* { R(TNUMBER); }
  "0" [0-7_]+ { R(TNUMBER); }
  "0" { R(TNUMBER); }

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
    goto escaped_eol_any;
  }
  "\n"[\t]*"\\" {
    spaces = 8 * (YYCURSOR - start - 2);
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

escaped_eol_any:
  escaped = true;

  switch (block_style(parser)) {
  case BLOCK_MULTI:
    spaces -= 8; // the escape makes extra tabs insignificant
    break;
  case BLOCK_SINGLE:
    // noop: parser->indent already includes an artificial +8, added when
    // opening a BLOCK_SINGLE.
    break;
  }

  if (spaces >= parser->indent) {
    goto normal;
  } else if (spaces < parser->indent) {
    if (spaces == parser->indent - 8) {
      // consume the escape char
      YYCURSOR += 1;
    }
    goto eol_any;
  } else {
    assert(false);
  }

eol_any:
  YYCURSOR -= 1;

  if (spaces == parser->indent) {
    switch (block_style(parser)) {
    case BLOCK_MULTI:
      R(TEOL);
      break;
    case BLOCK_SINGLE:
      parser->inject_eol_after_eob = true;
      block_up(parser, BLOCK_SINGLE);
      R(TEOB);
      break;
    }
  } else if (spaces > parser->indent) {
    if (spaces != parser->indent + 8) {
      FAIL(EINVAL, "cannot increase indentation by more than one tab");
    }
    parser->indent = spaces;

    error e = block_down(parser, BLOCK_MULTI);
    if (e) {
      FAIL(e, "lexer: too many block levels");
    }
    R(TSOB);
  } else if (spaces < parser->indent) {
    assert(spaces % 8 == 0);
    parser->indent -= 8;

    if (spaces != parser->indent) {
      // Closing several blocks at once, return token for the others.
      YYCURSOR = start;
      parser->tok_was_injected = true;
    }

    // EOB must be followed by an EOL.
    parser->inject_eol_after_eob = !escaped || spaces + 8 < parser->indent;
    block_up(parser, block_style(parser));
    R(TEOB);
  } else {
    assert(false);
  }

string:
/*!re2c
  [\\] {
    if (YYCURSOR >= YYLIMIT) {
      FAIL(EINVAL, "non-terminated string");
    }
    YYCURSOR += 1; goto string;
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

long_comment:
/*!re2c
  ")--" { goto normal; }
  ANY { goto long_comment; }
 */

comment:
/*!re2c
  "\n" { YYCURSOR -= 1; goto normal; }
  ANY { goto comment; }
 */

comment_while_eol:
/*!re2c
  "(" { goto long_comment; }
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

  const bool possible_injection = tok->t == TEOL || tok->t == TEOB;
  if (possible_injection && parser->tok_was_injected) {
    parser->inject_eol_after_eob = tok->t == TEOL;
    return;
  }

  parser->inject_eol_after_eob = false;

  assert(tok->len < parser->codeloc.pos);

  // Cannot use tok->len to compute the new position. When lexer_back() is
  // called several times in a row, we have to rewind from at least
  // tok->len, but it could be more as the toke could be separated by
  // whitespaces that are not accounted for in tok->len. By using
  // tok->value, we know we go back far enough.
  const size_t new_pos = tok->value - parser->data;
  bool no = update_codeloc(parser, new_pos);
  assert(!no);

  assert(parser->data + parser->codeloc.pos == tok->value);
}
