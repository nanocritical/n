#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "parser.h"

static error idents_value(const char **value, struct module *mod, ident id) {
  if (id > mod->idents.count) {
    EXCEPTF(EINVAL, "Unknown identifier id %d", id);
  }
  *value = mod->idents.values[id];
  return 0;
}

static ident idents_add(struct module *mod, const struct token *tok) {
  assert(tok->t == TIDENT);

  struct idents *idents = &mod->idents;

  for (size_t n = 0; n < idents->count; ++n) {
    if (strncmp(idents->values[n], tok->value, tok->len) == 0) {
      return n;
    }
  }

  if (idents->count >= idents->capacity) {
    if (idents->capacity == 0) {
      idents->capacity = 256;
    }
    idents->capacity *= 2;
    idents->values = realloc(idents->values, idents->capacity * sizeof(*idents->values));
  }

  char *cpy = malloc(tok->len + 1);
  memcpy(cpy, tok->value, tok->len);
  cpy[tok->len] = '\0';

  const ident id = idents->count;
  idents->values[id] = cpy;
  idents->count += 1;

  return id;
}

static error parse_modpath(struct module *mod, const char *fn) {
  for (size_t n = 0, last = 0, p = 0; fn[p] != '\0'; ++p) {
    if (fn[p] == '_') {
      EXCEPTF(EINVAL, "module path element cannot contain '_' in '%s'", fn);
    }

    if (fn[p] == '/') {
      struct token tok;
      tok.t = TIDENT;
      tok.value = fn + last;
      tok.len = p - last;
      mod->path[n] = idents_add(mod, &tok);

      last = p;
      n += 1;
      if (n >= ARRAY_SIZE(mod->path)) {
        EXCEPTF(EINVAL, "module path '%s' has too many elements", fn);
      }
    }
  }
  return 0;
}

static error module_read(struct module *mod, const char *fn) {
  mod->filename = fn;
  error e = parse_modpath(mod, fn);
  EXCEPT(e);

  int fd = open(fn, O_RDONLY);
  if (fd < 0) {
    EXCEPTF(errno, "open: %s", fn);
  }

  struct stat st;
  memset(&st, 0, sizeof(st));
  e = fstat(fd, &st);
  if (e < 0) {
    EXCEPTF(errno, "fstat: %s", fn);
  }

  char *data = malloc(st.st_size + 1);
  ssize_t count = read(fd, data, st.st_size);
  if (count < 0) {
    EXCEPTF(errno, "read: %s", fn);
  } else if (count != (ssize_t) st.st_size) {
    EXCEPTF(errno, "read: %s: Partial read not supported by parser", fn);
  }
  data[st.st_size] = '\0';

  mod->parser.data = data;
  mod->parser.len = st.st_size;

  return 0;
}

static int line(const struct parser *parser, const struct token *tok) {
  const size_t pos = tok->value != NULL ? tok->value - parser->data : parser->pos;
  int count = 1;
  for (size_t p = 0; p < pos; ++p) {
    if (parser->data[p] == '\n') {
      count += 1;
    }
  }
  return count;
}

static int column(const struct parser *parser, const struct token *tok) {
  const size_t pos = tok->value != NULL ? tok->value - parser->data : parser->pos;
  int count = 1;
  for (size_t p = 0; p < pos; ++p) {
    if (parser->data[p] == '\n') {
      count = 1;
    } else {
      count += 1;
    }
  }
  return count;
}

#define EXCEPT_SYNTAX(mod, tok, fmt, ...) do { \
  EXCEPTF(EINVAL, "%s:%d:%d: syntax: " fmt, \
          mod->filename, line(&mod->parser, tok), column(&mod->parser, tok), ##__VA_ARGS__); \
} while (0)

#define UNEXPECTED(mod, tok) do { \
  EXCEPT_SYNTAX(mod, tok, "unexpected token '%.*s'", (int)(tok)->len, (tok)->value); \
} while (0)

static bool eof(struct parser *parser) {
  return parser->pos >= parser->len;
}

static error scan(struct token *tok, struct module *mod) {
  memset(tok, 0, sizeof(*tok));

  error e = lexer_scan(tok, &mod->parser);
  if (e == EINVAL) {
    EXCEPT_SYNTAX(mod, tok, "%s", mod->parser.error_message);
  } else {
    EXCEPT(e);
  }
  return 0;
}

static void back(struct module *mod, const struct token *tok) {
  lexer_back(&mod->parser, tok);
}

static error scan_expected(struct module *mod, enum token_type t) {
  struct token tok;
  error e = scan(&tok, mod);
  EXCEPT(e);

  if (tok.t != t) {
    UNEXPECTED(mod, &tok);
  }

  return 0;
}

static error scan_oneof(struct token *tok, struct module *mod, ...) {
  error e = scan(tok, mod);
  EXCEPT(e);

  va_list ap;
  va_start(ap, mod);

  while (TRUE) {
    enum token_type t = va_arg(ap, enum token_type);
    if (t == 0) {
      break;
    }
    if (tok->t == t) {
      va_end(ap);
      return 0;
    }
  }

  va_end(ap);
  UNEXPECTED(mod, tok);

  return 0;

}

static struct node *new_subnode(struct node *node) {
  const size_t last = node->subs_count;
  node->subs_count = last + 1;
  node->subs = realloc(node->subs, node->subs_count * sizeof(struct node));

  struct node *r = node->subs + last;
  memset(r, 0, sizeof(*r));
  return r;
}

static error p_ident(struct node *node, struct module *mod) {
  struct token tok;
  error e = scan_oneof(&tok, mod, TIDENT, 0);
  EXCEPT(e);

  node->which = IDENT;
  node->as.IDENT.value = idents_add(mod, &tok);

  return 0;
}

static error p_number(struct node *node, struct module *mod) {
  struct token tok;
  error e = scan_oneof(&tok, mod, TNUMBER, 0);
  EXCEPT(e);

  char *cpy = malloc(tok.len + 1);
  memcpy(cpy, tok.value, tok.len);
  cpy[tok.len] = '\0';

  node->which = STRING;
  node->as.STRING.value = cpy;

  return 0;
}

static error p_string(struct node *node, struct module *mod) {
  struct token tok;
  error e = scan_oneof(&tok, mod, TSTRING, 0);
  EXCEPT(e);

  char *cpy = malloc(tok.len + 1);
  memcpy(cpy, tok.value, tok.len);
  cpy[tok.len] = '\0';

  node->which = STRING;
  node->as.STRING.value = cpy;

  return 0;
}

static error p_typeexpr(struct node *node, struct module *mod) {
  node->which = TYPEEXPR;
  error e = p_ident(new_subnode(node), mod);
  EXCEPT(e);
  return 0;
}

static error p_typeconstraint(struct node *node, struct module *mod) {
  node->which = TYPECONSTRAINT;
  error e = p_ident(new_subnode(node), mod);
  EXCEPT(e);

  e = scan_expected(mod, TCOLON);
  EXCEPT(e);

  e = p_typeexpr(new_subnode(node), mod);
  EXCEPT(e);
  return 0;
}

static error p_expr(struct node *node, struct module *mod, uint32_t parent_op);
static error p_block(struct node *node, struct module *mod);
static error p_deflet(struct node *node, struct module *mod, const struct toplevel *toplevel);

static error p_unary_expr(struct node *node, struct module *mod) {
  struct token tok;
  error e = scan(&tok, mod);
  EXCEPT(e);

  node->which = UN;
  node->as.UN.operator = tok.t;

  uint32_t prec = OP_PREC(tok.t);
  switch (tok.t) {
  case TMINUS:
    prec = OP_PREC(TUMINUS);
    break;
  case TPLUS:
    prec = OP_PREC(TUPLUS);
    break;
  default:
    break;
  }

  e = p_expr(new_subnode(node), mod, prec);
  EXCEPT(e);

  return 0;
}

static error p_expr_call(struct node *node, const struct node *first,
                         struct module *mod) {
  node->which = CALL;
  error e;
  struct token tok;

  while (TRUE) {
    e = scan(&tok, mod);
    EXCEPT(e);
    back(mod, &tok);

    if (tok.t == TEOL || tok.t == TEOB || tok.t == TSOB) {
      return 0;
    }

    e = p_expr(new_subnode(node), mod, T__NONE);
    EXCEPT(e);
  }
}

static error p_expr_binary(struct node *node, const struct node *first,
                           struct module *mod) {
  struct token tok;
  error e = scan(&tok, mod);
  EXCEPT(e);

  node->which = BIN;
  node->as.BIN.operator = tok.t;

  e = p_expr(new_subnode(node), mod, OP_PREC(tok.t));
  EXCEPT(e);

  return 0;
}

static error p_expr(struct node *node, struct module *mod, uint32_t parent_op) {
  error e;
  struct token tok;

  e = scan(&tok, mod);
  EXCEPT(e);

  struct node first, second;
  memset(&first, 0, sizeof(first));
  memset(&second, 0, sizeof(second));

  if (tok.t == TLPAR) {
    e = p_expr(&first, mod, T__NONE);
    EXCEPT(e);
    e = scan_expected(mod, TRPAR);
    EXCEPT(e);
  } else {
    back(mod, &tok);

    if (tok.t == Tnull) {
      e = scan(&tok, mod);
      EXCEPT(e);
      node->which = NUL;
      return 0;
    } else if (tok.t == TIDENT) {
      e = p_ident(&first, mod);
    } else if (tok.t == TNUMBER) {
      e = p_number(&first, mod);
    } else if (tok.t == TSTRING) {
      e = p_string(&first, mod);
    } else if ((IS_OP(tok.t) && OP_UNARY(tok.t))
               || tok.t == TMINUS || tok.t == TPLUS) { // Unary versions.
      e = p_unary_expr(&first, mod);
    } else {
      UNEXPECTED(mod, &tok);
    }
    EXCEPT(e);
  }

  e = scan(&tok, mod);
  EXCEPT(e);
  back(mod, &tok);

  if (IS_OP(tok.t) && OP_BINARY(tok.t)
      && OP_PREC(tok.t) == OP_PREC(parent_op)
      && OP_ASSOC(tok.t) == ASSOC_NON) {
    EXCEPT_SYNTAX(mod, &tok, "Operator '%.*s' is non-associative", (int)tok.len, tok.value);
  }

  if (IS_OP(tok.t) && OP_BINARY(tok.t)) {
    if (OP_PREC(tok.t) < OP_PREC(parent_op)
        || (OP_PREC(tok.t) == OP_PREC(parent_op)
            && OP_ASSOC(tok.t) == ASSOC_RIGHT)) {
      e = p_expr_binary(&second, &first, mod);
      EXCEPT(e);
      goto even_more;
    }
  } else if (!expr_terminators[tok.t]) {
    if (OP_PREC(T__CALL) < OP_PREC(parent_op)) {
      e = p_expr_call(&second, &first, mod);
      EXCEPT(e);
      goto even_more;
    }
  } else {
    *node = first;
    return 0;
  }

  EXCEPT(e);

even_more:
  first = second;
  memset(&second, 0, sizeof(second));

  e = scan(&tok, mod);
  EXCEPT(e);
  back(mod, &tok);

  if (IS_OP(tok.t) && OP_BINARY(tok.t)
      && (OP_PREC(tok.t) < OP_PREC(parent_op)
          || (OP_PREC(tok.t) == OP_PREC(parent_op)
              && OP_ASSOC(tok.t) == ASSOC_RIGHT))) {
    e = p_expr_binary(&first, &second, mod);
    EXCEPT(e);

    goto even_more;
  } else {
    *node = first;
    return 0;
  }
}

static error p_return(struct node *node, struct module *mod) {
  node->which = RETURN;
  error e = p_expr(new_subnode(node), mod, T__NONE);
  EXCEPT(e);
  return 0;
}

static error p_pattern(struct node *node, struct module *mod) {
  error e = p_ident(node, mod);
  EXCEPT(e);
  return 0;
}

static error p_let(struct node *node, struct module *mod) {
  node->which = DEFLET;

  error e = p_pattern(new_subnode(node), mod);
  EXCEPT(e);

  e = scan_expected(mod, TASSIGN);
  EXCEPT(e);

  e = p_expr(new_subnode(node), mod, T__NONE);
  EXCEPT(e);

  struct token tok;
  e = scan(&tok, mod);
  EXCEPT(e);
  if (tok.t != TSOB) {
    back(mod, &tok);
    return 0;
  }

  e = p_block(new_subnode(node), mod);
  EXCEPT(e);

  return 0;
}

static error p_if(struct node *node, struct module *mod) {
  node->which = IF;

  struct token eol;

  error e = p_expr(new_subnode(node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(node), mod);
  EXCEPT(e);
  e = scan(&eol, mod);
  EXCEPT(e);
  if (eol.t != TEOL) {
    UNEXPECTED(mod, &eol);
  }

  struct token tok;

again:
  e = scan(&tok, mod);
  EXCEPT(e);

  switch (tok.t) {
  case Telif:
    e = p_expr(new_subnode(node), mod, T__NONE);
    EXCEPT(e);
    e = scan_expected(mod, TSOB);
    EXCEPT(e);
    e = p_block(new_subnode(node), mod);
    EXCEPT(e);
    e = scan(&eol, mod);
    EXCEPT(e);
    if (eol.t != TEOL) {
      UNEXPECTED(mod, &eol);
    }
    goto again;
  case Telse:
    e = scan_expected(mod, TSOB);
    EXCEPT(e);
    e = p_block(new_subnode(node), mod);
    EXCEPT(e);
    e = scan(&tok, mod);
    EXCEPT(e);
    if (eol.t != TEOL) {
      UNEXPECTED(mod, &eol);
    }
    break;
  default:
    back(mod, &tok);
    break;
  }

  // Need to reinject the eol after the last block.
  mod->parser.inject_eol_after_eob = TRUE;

  return 0;
}

static error p_for(struct node *node, struct module *mod) {
  node->which = FOR;

  error e = p_pattern(new_subnode(node), mod);
  EXCEPT(e);
  e = scan_expected(mod, Tin);
  EXCEPT(e);
  e = p_expr(new_subnode(node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(node), mod);
  EXCEPT(e);

  return 0;
}

static error p_while(struct node *node, struct module *mod) {
  node->which = WHILE;

  error e = p_expr(new_subnode(node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(node), mod);
  EXCEPT(e);

  return 0;
}

static error p_try(struct node *node, struct module *mod) {
  node->which = TRY;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(node), mod);
  EXCEPT(e);
  e = scan_expected(mod, TEOL);
  EXCEPT(e);
  e = scan_expected(mod, Tcatch);
  EXCEPT(e);
  e = p_expr(new_subnode(node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(node), mod);
  EXCEPT(e);

  return 0;
}

static error p_break(struct node *node, struct module *mod) {
  node->which = BREAK;
  return 0;
}

static error p_continue(struct node *node, struct module *mod) {
  node->which = CONTINUE;
  return 0;
}

static error p_pass(struct node *node, struct module *mod) {
  node->which = PASS;
  return 0;
}

static error p_match(struct node *node, struct module *mod) {
  node->which = MATCH;

  error e = p_expr(new_subnode(node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TEOL);
  EXCEPT(e);

  struct token tok;
again:
  e = scan(&tok, mod);
  EXCEPT(e);
  if (tok.t != TBWOR) {
    mod->parser.inject_eol_after_eob = TRUE;
    return 0;
  }

  e = p_expr(new_subnode(node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(node), mod);
  EXCEPT(e);
  e = scan_expected(mod, TEOL);
  EXCEPT(e);
  goto again;
}

static error p_pre(struct node *node, struct module *mod) {
  node->which = PRE;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(node), mod);
  EXCEPT(e);

  return 0;
}

static error p_post(struct node *node, struct module *mod) {
  node->which = POST;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(node), mod);
  EXCEPT(e);

  return 0;
}

static error p_invariant(struct node *node, struct module *mod) {
  node->which = INVARIANT;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(node), mod);
  EXCEPT(e);

  return 0;
}

static error p_example(struct node *node, struct module *mod) {
  node->which = EXAMPLE;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(node), mod);
  EXCEPT(e);

  return 0;
}

static error p_statement(struct node *node, struct module *mod) {
  error e;
  struct token tok;

  e = scan(&tok, mod);
  EXCEPT(e);

  switch (tok.t) {
  case Treturn:
    e = p_return(node, mod);
    break;
  case Tlet:
    e = p_let(node, mod);
    break;
  case Tif:
    e = p_if(node, mod);
    break;
  case Tfor:
    e = p_for(node, mod);
    break;
  case Twhile:
    e = p_while(node, mod);
    break;
  case Ttry:
    e = p_try(node, mod);
    break;
  case Tbreak:
    e = p_break(node, mod);
    break;
  case Tcontinue:
    e = p_continue(node, mod);
    break;
  case Tpass:
    e = p_pass(node, mod);
    break;
  case Tmatch:
    e = p_match(node, mod);
    break;
  case Tpre:
    e = p_pre(node, mod);
    break;
  case Tpost:
    e = p_post(node, mod);
    break;
  case Tinvariant:
    e = p_invariant(node, mod);
    break;
  case Texample:
    e = p_example(node, mod);
    break;
  case TLPAR:
  case TIDENT:
    back(mod, &tok);
    e = p_expr(node, mod, T__NONE);
    break;
  default:
    UNEXPECTED(mod, &tok);
  }
  EXCEPT(e);

  return 0;
}

static error p_block(struct node *node, struct module *mod) {
  error e;
  struct token tok;
  bool first = TRUE;

  e = scan(&tok, mod);
  EXCEPT(e);

again:
  if (tok.t == TEOB) {
    if (first) {
      EXCEPT_SYNTAX(mod, &tok, "block cannot be empty (use 'pass' instead)");;
    } else {
      return 0;
    }
  } else {
    back(mod, &tok);
    e = p_statement(new_subnode(node), mod);
    EXCEPT(e);

    e = scan_oneof(&tok, mod, TEOL, TEOB, 0);
    EXCEPT(e);

    if (tok.t == TEOB) {
      return 0;
    }
  }

  first = FALSE;
  e = scan(&tok, mod);
  EXCEPT(e);

  goto again;
}

static error p_deffun(struct node *node, struct module *mod, const struct toplevel *toplevel,
                      enum type_node fun_or_method) {
  node->which = fun_or_method;
  switch (fun_or_method) {
  case DEFFUN:
    node->as.DEFFUN.toplevel = *toplevel;
    break;
  case DEFMETHOD:
    node->as.DEFMETHOD.toplevel = *toplevel;
    break;
  default:
    assert(FALSE);
  }

  error e = p_ident(new_subnode(node), mod);
  EXCEPT(e);

  struct token tok;

again:
  e = scan_oneof(&tok, mod, TASSIGN, TIDENT, 0);
  EXCEPT(e);

  switch (tok.t) {
  case TASSIGN:
    goto retval;
  case TIDENT:
    back(mod, &tok);

    e = p_typeconstraint(new_subnode(node), mod);
    EXCEPT(e);
    goto again;
  default:
    assert(FALSE);
  }

retval:
  e = p_expr(new_subnode(node), mod, T__NONE);
  EXCEPT(e);

  e = scan_oneof(&tok, mod, TEOL, TSOB, TEOB, 0);
  EXCEPT(e);

  if (tok.t == TEOL || tok.t == TEOB) {
    back(mod, &tok);
    return 0;
  }

  e = p_block(new_subnode(node), mod);
  EXCEPT(e);

  return 0;
}

static error p_isalist(struct node *node, struct module *mod) {
  node->which = ISALIST;

  error e;
  struct token tok;

again:
  e = scan(&tok, mod);
  EXCEPT(e);
  back(mod, &tok);

  switch (tok.t) {
  case TEOL:
  case TSOB:
    return 0;
  default:
    e = p_expr(new_subnode(node), mod, T__CALL);
    EXCEPT(e);
    goto again;
  }
}

static error p_delegate(struct node *node, struct module *mod) {
  node->which = DELEGATE;

  error e = p_expr(new_subnode(node), mod, T__NONE);
  EXCEPT(e);

  struct token tok;

again:
  e = scan(&tok, mod);
  EXCEPT(e);
  back(mod, &tok);

  if (tok.t == TEOL) {
    return 0;
  }

  e = p_expr(new_subnode(node), mod, T__NONE);
  EXCEPT(e);

  goto again;
}

static error p_deftype_statement(struct node *node, struct module *mod) {
  error e;
  struct token tok;
  struct toplevel toplevel;

  e = scan(&tok, mod);
  EXCEPT(e);

  switch (tok.t) {
  case Tlet:
    memset(&toplevel, 0, sizeof(toplevel));
    e = p_deflet(node, mod, &toplevel);
    break;
  case Tdelegate:
    e = p_delegate(node, mod);
    break;
  case Tinvariant:
    e = p_invariant(node, mod);
    break;
  case Texample:
    e = p_example(node, mod);
    break;
  case TIDENT:
    back(mod, &tok);
    e = p_typeconstraint(node, mod);
    break;
  default:
    UNEXPECTED(mod, &tok);
  }
  EXCEPT(e);

  return 0;
}

static error p_deftype_block(struct node *node, struct module *mod) {
  error e;
  struct token tok;
  bool first = TRUE;

  e = scan(&tok, mod);
  EXCEPT(e);

again:
  if (tok.t == TEOB) {
    if (first) {
      EXCEPT_SYNTAX(mod, &tok, "block cannot be empty (use 'pass' instead)");;
    } else {
      return 0;
    }
  } else {
    back(mod, &tok);
    e = p_deftype_statement(new_subnode(node), mod);
    EXCEPT(e);

    e = scan_oneof(&tok, mod, TEOL, TEOB, 0);
    EXCEPT(e);

    if (tok.t == TEOB) {
      return 0;
    }
  }

  first = FALSE;
  e = scan(&tok, mod);
  EXCEPT(e);

  goto again;
}

static error p_deftype(struct node *node, struct module *mod, const struct toplevel *toplevel) {
  node->which = DEFTYPE;
  node->as.DEFTYPE.toplevel = *toplevel;

  struct token tok;
  error e = p_ident(new_subnode(node), mod);
  EXCEPT(e);

  e = scan_expected(mod, TASSIGN);
  EXCEPT(e);

  e = p_isalist(new_subnode(node), mod);
  EXCEPT(e);

  e = scan_oneof(&tok, mod, TEOL, TSOB, 0);
  EXCEPT(e);

  if (tok.t == TEOL) {
    back(mod, &tok);
    return 0;
  }

  e = p_deftype_block(new_subnode(node), mod);
  EXCEPT(e);

  return 0;
}

static error p_defunion(struct node *node, struct module *mod, const struct toplevel *toplevel) {
  return 0;
}

static error p_defintf(struct node *node, struct module *mod, const struct toplevel *toplevel) {
  return 0;
}

static error p_deflet(struct node *node, struct module *mod, const struct toplevel *toplevel) {
  error e = p_let(node, mod);
  EXCEPT(e);
  node->as.DEFLET.toplevel = *toplevel;
  return 0;
}

static error p_toplevel(struct module *mod) {
  struct node *node = new_subnode(&mod->node);

  struct toplevel toplevel;
  memset(&toplevel, 0, sizeof(toplevel));

  error e;
  struct token tok;
  bool is_method = FALSE;
again:
  e = scan(&tok, mod);
  EXCEPT(e);

  if (is_method && tok.t != Tmethod) {
    UNEXPECTED(mod, &tok);
  }

  switch (tok.t) {
  case Ttype:
    e = p_deftype(node, mod, &toplevel);
    break;
  case Textern:
    toplevel.is_extern = TRUE;
    goto again;
  case Tinline:
    toplevel.is_inline = TRUE;
    goto again;
  case Tfun:
    e = p_deffun(node, mod, &toplevel, DEFFUN);
    break;
  case Tmethod:
    if (!is_method) {
      UNEXPECTED(mod, &tok);
    }
    e = p_deffun(node, mod, &toplevel, DEFMETHOD);
    break;
  case Tunion:
    e = p_defunion(node, mod, &toplevel);
    break;
  case Tintf:
    e = p_defintf(node, mod, &toplevel);
    break;
  case Tlet:
    e = p_deflet(node, mod, &toplevel);
    break;
  case TIDENT:
    toplevel.scope = idents_add(mod, &tok);
    is_method = TRUE;
    goto again;
  default:
    EXCEPT_SYNTAX(mod, &tok, "malformed top-level statement at '%.*s'", (int)tok.len, tok.value);
    break;
  }

  EXCEPT(e);
  return 0;
}

static void rec_subnode_counts(struct node *node,
                               size_t *node_count, size_t *sub_count) {
  size_t n;
  for (n = 0; n < node->subs_count; ++n) {
    *sub_count += 1;
    rec_subnode_counts(node->subs + n, node_count, sub_count);
  }

  *node_count += !!n;
}

__attribute__((unused))
static float subnode_count_avg(struct module *mod) {
  size_t sub_count = 0, node_count = 0;
  rec_subnode_counts(&mod->node, &node_count, &sub_count);
  return (float) sub_count / node_count;
}

static error module_parse(struct module *mod) {
  error e;

  do {
    e = p_toplevel(mod);
    EXCEPT(e);
    e = scan_expected(mod, TEOL);
    EXCEPT(e);
  } while (!eof(&mod->parser));

  return 0;
}

static void module_init(struct module *mod) {
  memset(mod, 0, sizeof(*mod));
}

error module_open(struct module *mod, const char *fn) {
  module_init(mod);

  error e = module_read(mod, fn);
  EXCEPT(e);

  e = module_parse(mod);
  EXCEPT(e);

  return 0;
}
