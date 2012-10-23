#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "parser.h"
#include "table.h"

enum predefined_idents {
  ID_ANONYMOUS = 0,
  ID_FOR,
  ID_WHILE,
  ID_MATCH,
  ID_TRY,
  ID_DEFLET,
  ID__NUM,
};

static const char *predefined_idents_strings[] = {
  [ID_ANONYMOUS] = "<anonymous>",
  [ID_FOR] = "<for>",
  [ID_WHILE] = "<while>",
  [ID_MATCH] = "<match>",
  [ID_TRY] = "<try_catch>",
  [ID_DEFLET] = "<deflet>",
};

HTABLE_SPARSE(idents_map, ident, struct token);
implement_htable_sparse(__attribute__((unused)) static, idents_map, ident, struct token);

uint32_t token_hash(const struct token *tok) {
  return hash32_hsieh(tok->value, tok->len);
}

int token_cmp(const struct token *a, const struct token *b) {
  return memcmp(a->value, b->value, min(size_t, a->len, b->len));
}

const char *idents_value(const struct module *mod, ident id) {
  assert(id <= mod->idents.count);
  return mod->idents.values[id];
}

ident idents_add(struct module *mod, const struct token *tok) {
  assert(tok->t == TIDENT);

  struct idents *idents = &mod->idents;

  ident *existing_id = idents_map_get(idents->map, *tok);
  if (existing_id != NULL) {
    return *existing_id;
  }

  if (idents->count >= idents->capacity) {
    if (idents->capacity <= 1) {
      idents->capacity = 256;
    }

    size_t old_capacity = idents->capacity;
    idents->capacity *= 2;
    idents->values = realloc(idents->values, idents->capacity * sizeof(*idents->values));
    memset(idents->values + old_capacity, 0, idents->capacity - old_capacity);
  }

  char *cpy = malloc(tok->len + 1);
  memcpy(cpy, tok->value, tok->len);
  cpy[tok->len] = '\0';

  const ident id = idents->count;
  idents->values[id] = cpy;
  idents->count += 1;

  idents_map_set(idents->map, *tok, id);

  return id;
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

#define EXCEPT_PARSE(mod, codeloc, fmt, ...) do { \
  struct token tok; \
  tok.value = mod->parser.data + codeloc; \
  EXCEPTF(EINVAL, "%s:%d:%d: parse: " fmt, \
          mod->filename, line(&mod->parser, &tok), column(&mod->parser, &tok), ##__VA_ARGS__); \
} while (0)

#define UNEXPECTED(mod, tok) do { \
  EXCEPT_SYNTAX(mod, tok, "unexpected token '%.*s'", (int)(tok)->len, (tok)->value); \
} while (0)

HTABLE_SPARSE(scope_map, struct node *, ident);
implement_htable_sparse(__attribute__((unused)) static, scope_map, struct node *, ident);

uint32_t ident_hash(const ident *a) {
  return hash32_hsieh(a, sizeof(*a));
}

int ident_cmp(const ident *a, const ident *b) {
  return memcmp(a, b, sizeof(*a));
}

struct scope *scope_new(struct node *node) {
  struct scope *s = calloc(1, sizeof(struct scope));
  s->map = calloc(1, sizeof(struct scope_map));
  scope_map_init(s->map, 0);
  scope_map_set_delete_val(s->map, NULL);
  scope_map_set_custom_hashf(s->map, ident_hash);
  scope_map_set_custom_cmpf(s->map, ident_cmp);

  s->node = node;
  return s;
}

static ident node_ident(const struct node *node) {
  switch (node->which) {
  case IDENT:
    return node->as.IDENT.name;
  case FOR:
    return ID_FOR;
  case WHILE:
    return ID_WHILE;
  case MATCH:
    return ID_MATCH;
  case TRY:
    return ID_TRY;
  case DEFFUN:
  case DEFTYPE:
  case DEFMETHOD:
  case DEFINTF:
    assert(node->subs[0].which == IDENT);
    return node->subs[0].as.IDENT.name;
  case DEFLET:
    return ID_DEFLET;
  case MODULE:
    return node->as.MODULE.name;
  default:
    fprintf(stderr, "%d\n", node->which);
    return ID_ANONYMOUS;
  }
}

// Must free return value.
const char *scope_name(const struct module *mod, const struct scope *scope) {
  size_t len = 0;
  const struct scope *root = scope;
  while (root != NULL) {
    len += strlen(idents_value(mod, node_ident(root->node))) + 1;
    root = root->parent;
  }
  len -= 1;

  char *r = calloc(len + 1, sizeof(char));
  root = scope;
  while (root != NULL) {
    const char *name = idents_value(mod, node_ident(root->node));
    size_t name_len = strlen(name);
    if (root->parent == NULL) {
      memcpy(r, name, name_len);
    } else {
      len -= name_len + 1;
      r[len] = '.';
      memcpy(r + len + 1, name, name_len);
    }
    root = root->parent;
  }

  return r;
}

error scope_get(struct node **node,
                const struct module *mod, const struct scope *scope, ident id) {
  struct node **n = scope_map_get(scope->map, id);
  if (n == NULL) {
    const char *scname = scope_name(mod, scope);
    EXCEPT_PARSE(mod, scope->node->codeloc, "in scope %s: unknown identifier '%s'",
                 scname, idents_value(mod, id));
    // FIXME: leaking scname.
  }

  *node = *n;
  return 0;
}

error scope_add_ident(const struct module *mod, struct scope *scope, ident id, struct node *node) {
  struct node **existing = scope_map_get(scope->map, id);
  if (existing != NULL) {
    if (*existing == node) {
      return 0;
    }

    struct token existing_tok;
    existing_tok.value = mod->parser.data + (*existing)->codeloc;
    const char *scname = scope_name(mod, scope);
    EXCEPT_PARSE(mod, node->codeloc,
                 "in scope %s: identifier '%s' already defined at %s:%d:%d",
                 scname, idents_value(mod, id), mod->filename,
                 line(&mod->parser, &existing_tok), column(&mod->parser, &existing_tok));
    // FIXME: leaking scname.
  }

  scope_map_set(scope->map, id, node);
  return 0;
}

error scope_add(const struct module *mod, struct scope *scope, struct node *id, struct node *node) {
  assert(id->which == IDENT);
  return scope_add_ident(mod, scope, id->as.IDENT.name, node);
}

static error parse_modpath(struct module *mod, const char *fn) {
  mod->path_len = 0;
  for (size_t n = 0, last = 0, p = 0; fn[p] != '\0'; ++p) {
    if (fn[p] == '_') {
      EXCEPTF(EINVAL, "Module path element cannot contain '_' in '%s'", fn);
    }

    if (fn[p] == '/' || fn[p] == '.' || fn[p + 1] == '\0') {
      struct token tok;
      tok.t = TIDENT;
      tok.value = fn + last;
      tok.len = p - last;
      mod->path[n] = idents_add(mod, &tok);
      mod->path_len += 1;

      last = p;
      n += 1;
      if (n >= ARRAY_SIZE(mod->path)) {
        EXCEPTF(EINVAL, "Module path '%s' has too many elements", fn);
      }

      if (fn[p] == '.') {
        // Skip anything after a dot (allows for things like versioning to
        // be present in filename or dirname after the dot, without
        // changing the module name).
        while (fn[p] != '/' && fn[p+1] != '\0') {
          p += 1;
        }
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
    EXCEPTF(errno, "Cannot open module '%s'", fn);
  }

  struct stat st;
  memset(&st, 0, sizeof(st));
  e = fstat(fd, &st);
  if (e < 0) {
    EXCEPTF(errno, "Cannot stat module '%s'", fn);
  }

  char *data = malloc(st.st_size + 1);
  ssize_t count = read(fd, data, st.st_size);
  if (count < 0) {
    EXCEPTF(errno, "Error reading module '%s'", fn);
  } else if (count != (ssize_t) st.st_size) {
    EXCEPTF(errno, "Reading module '%s': Partial read not supported by parser", fn);
  }
  data[st.st_size] = '\0';

  mod->parser.data = data;
  mod->parser.len = st.st_size;

  return 0;
}

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

static struct node *new_subnode(const struct module *mod, struct node *node) {
  const size_t last = node->subs_count;
  node->subs_count = last + 1;
  node->subs = realloc(node->subs, node->subs_count * sizeof(struct node));

  struct node *r = node->subs + last;
  memset(r, 0, sizeof(*r));

  if (mod->parser.pos == mod->parser.len) {
    // That's a node inserted after parsing.
    r->codeloc = node->codeloc;
  } else {
    r->codeloc = mod->parser.pos;
  }

  return r;
}

static error p_ident(struct node *node, struct module *mod) {
  struct token tok;
  error e = scan_oneof(&tok, mod, TIDENT, 0);
  EXCEPT(e);

  node->which = IDENT;
  node->as.IDENT.name = idents_add(mod, &tok);

  return 0;
}

static error p_number(struct node *node, struct module *mod) {
  struct token tok;
  error e = scan_oneof(&tok, mod, TNUMBER, 0);
  EXCEPT(e);

  char *cpy = malloc(tok.len + 1);
  memcpy(cpy, tok.value, tok.len);
  cpy[tok.len] = '\0';

  node->which = NUMBER;
  node->as.NUMBER.value = cpy;

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
  error e = p_ident(new_subnode(mod, node), mod);
  EXCEPT(e);
  return 0;
}

static error p_typeconstraint(struct node *node, struct module *mod) {
  node->which = TYPECONSTRAINT;
  error e = p_ident(new_subnode(mod, node), mod);
  EXCEPT(e);

  e = scan_expected(mod, TCOLON);
  EXCEPT(e);

  e = p_typeexpr(new_subnode(mod, node), mod);
  EXCEPT(e);
  return 0;
}

static error p_expr(struct node *node, struct module *mod, uint32_t parent_op);
static error p_block(struct node *node, struct module *mod);
static error p_deflet(struct node *node, struct module *mod, const struct toplevel *toplevel);

static error p_expr_unary(struct node *node, struct module *mod) {
  struct token tok;
  error e = scan(&tok, mod);
  EXCEPT(e);

  uint32_t op;
  switch (tok.t) {
  case TMINUS:
    op = TUMINUS;
    break;
  case TPLUS:
    op = TUPLUS;
    break;
  default:
    op = tok.t;
    break;
  }

  node->which = UN;
  node->as.UN.operator = op;

  e = p_expr(new_subnode(mod, node), mod, op);
  EXCEPT(e);

  return 0;
}

static error p_expr_init(struct node *node, const struct node *first,
                         struct module *mod) {
  node->which = INIT;

  error e = scan_expected(mod, TLINIT);
  EXCEPT(e);

  struct token tok;

  struct node *fst = new_subnode(mod, node);
  *fst = *first;

  while (TRUE) {
    e = scan(&tok, mod);
    EXCEPT(e);

    if (tok.t == TRINIT) {
      return 0;
    }
    back(mod, &tok);

    e = p_ident(new_subnode(mod, node), mod);
    EXCEPT(e);

    e = scan_expected(mod, TASSIGN);
    EXCEPT(e);

    e = p_expr(new_subnode(mod, node), mod, T__CALL);
    EXCEPT(e);
  }
}

static error p_expr_tuple(struct node *node, const struct node *first,
                          struct module *mod) {
  node->which = TUPLE;
  error e;
  struct token tok;

  struct node *fst = new_subnode(mod, node);
  *fst = *first;

  while (TRUE) {
    e = scan(&tok, mod);
    EXCEPT(e);

    if (tok.t != TCOMMA) {
      back(mod, &tok);
      return 0;
    }

    e = p_expr(new_subnode(mod, node), mod, TCOMMA);
    EXCEPT(e);
  }
}

static error p_expr_call(struct node *node, const struct node *first,
                         struct module *mod) {
  node->which = CALL;
  error e;
  struct token tok;

  struct node *function = new_subnode(mod, node);
  *function = *first;

  while (TRUE) {
    e = scan(&tok, mod);
    EXCEPT(e);
    back(mod, &tok);

    if (expr_terminators[tok.t]) {
      return 0;
    }

    e = p_expr(new_subnode(mod, node), mod, T__CALL);
    EXCEPT(e);
  }
}

static error p_expr_binary(struct node *node, const struct node *first,
                           struct module *mod) {
  struct token tok;
  error e = scan(&tok, mod);
  EXCEPT(e);

  assert(tok.t != TCOMMA);
  node->which = BIN;
  node->as.BIN.operator = tok.t;

  struct node *left = new_subnode(mod, node);
  *left = *first;

  e = p_expr(new_subnode(mod, node), mod, tok.t);
  EXCEPT(e);

  return 0;
}

static error p_expr(struct node *node, struct module *mod, uint32_t parent_op) {
  assert(parent_op < TOKEN__NUM && IS_OP(parent_op));

  error e;
  struct token tok;
  bool first_iteration = TRUE;
  bool topmost = parent_op == T__NONE;

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
      first.which = NUL;
    } else if (tok.t == TIDENT) {
      e = p_ident(&first, mod);
    } else if (tok.t == TNUMBER) {
      e = p_number(&first, mod);
    } else if (tok.t == TSTRING) {
      e = p_string(&first, mod);
    } else if ((IS_OP(tok.t) && OP_UNARY(tok.t))
               || tok.t == TMINUS || tok.t == TPLUS) { // Unary versions.
      e = p_expr_unary(&first, mod);
    } else {
      UNEXPECTED(mod, &tok);
    }
    EXCEPT(e);
  }

even_more:
  e = scan(&tok, mod);
  EXCEPT(e);
  back(mod, &tok);

  if (IS_OP(tok.t) && OP_BINARY(tok.t)
      && OP_PREC(tok.t) == OP_PREC(parent_op)
      && OP_ASSOC(tok.t) == ASSOC_NON) {
    EXCEPT_SYNTAX(mod, &tok, "Operator '%.*s' is non-associative", (int)tok.len, tok.value);
  }

  if (first_iteration) {
    first_iteration = FALSE;
  } else {
    first = second;
    memset(&second, 0, sizeof(second));
  }

  if (expr_terminators[tok.t]) {
    goto done;
  } else if (IS_OP(tok.t) && OP_BINARY(tok.t)) {
    if (tok.t == TCOMMA) {
      if (OP_PREC(tok.t) < OP_PREC(parent_op)
          || topmost) {
        e = p_expr_tuple(&second, &first, mod);
        EXCEPT(e);

        goto even_more;
      } else {
        goto done;
      }
    } else if (tok.t == TLINIT) {
      if (OP_PREC(tok.t) < OP_PREC(parent_op)
          || topmost) {
        e = p_expr_init(&second, &first, mod);
        EXCEPT(e);

        goto even_more;
      } else {
        goto done;
      }
    } else if (OP_PREC(tok.t) < OP_PREC(parent_op)
        || (OP_PREC(tok.t) == OP_PREC(parent_op)
            && OP_ASSOC(tok.t) == ASSOC_RIGHT)
        || topmost) {
      e = p_expr_binary(&second, &first, mod);
      EXCEPT(e);

      if (topmost) {
        parent_op = tok.t;
      }

      goto even_more;
    } else {
      goto done;
    }
  } else if (OP_PREC(T__CALL) < OP_PREC(parent_op) || topmost) {
    e = p_expr_call(&second, &first, mod);
    if (topmost) {
      parent_op = T__CALL;
    }
    EXCEPT(e);

    goto even_more;
  } else {
    goto done;
  }

done:
  *node = first;
  return 0;
}

static error p_return(struct node *node, struct module *mod) {
  node->which = RETURN;
  error e = p_expr(new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);
  return 0;
}

static error p_except(struct node *node, struct module *mod) {
  node->which = EXCEP;
  struct token tok;
  scan(&tok, mod);
  if (!expr_terminators[tok.t]) {
    error e = p_expr(new_subnode(mod, node), mod, T__NONE);
    EXCEPT(e);
  }
  return 0;
}

static error p_pattern(struct node *node, struct module *mod) {
  error e = p_ident(node, mod);
  EXCEPT(e);
  return 0;
}

static error p_let(struct node *node, struct module *mod) {
  node->which = DEFLET;

  error e = p_pattern(new_subnode(mod, node), mod);
  EXCEPT(e);

  e = scan_expected(mod, TASSIGN);
  EXCEPT(e);

  e = p_expr(new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);

  struct token tok;
  e = scan(&tok, mod);
  EXCEPT(e);
  if (tok.t != TSOB) {
    back(mod, &tok);
    return 0;
  }

  e = p_block(new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_if(struct node *node, struct module *mod) {
  node->which = IF;

  struct token eol;

  error e = p_expr(new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(mod, node), mod);
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
    e = p_expr(new_subnode(mod, node), mod, T__NONE);
    EXCEPT(e);
    e = scan_expected(mod, TSOB);
    EXCEPT(e);
    e = p_block(new_subnode(mod, node), mod);
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
    e = p_block(new_subnode(mod, node), mod);
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

  error e = p_pattern(new_subnode(mod, node), mod);
  EXCEPT(e);
  e = scan_expected(mod, Tin);
  EXCEPT(e);
  e = p_expr(new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_while(struct node *node, struct module *mod) {
  node->which = WHILE;

  error e = p_expr(new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_try(struct node *node, struct module *mod) {
  node->which = TRY;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(mod, node), mod);
  EXCEPT(e);
  e = scan_expected(mod, TEOL);
  EXCEPT(e);
  e = scan_expected(mod, Tcatch);
  EXCEPT(e);

  struct token tok;
  scan(&tok, mod);
  if (tok.t != TSOB) {
    back(mod, &tok);
    e = p_expr(new_subnode(mod, node), mod, T__NONE);
    EXCEPT(e);
    e = scan_expected(mod, TSOB);
    EXCEPT(e);
  }

  e = p_block(new_subnode(mod, node), mod);
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

  error e = p_expr(new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TEOL);
  EXCEPT(e);

  struct token tok;
again:
  e = scan(&tok, mod);
  EXCEPT(e);
  if (tok.t != TBWOR) {
    back(mod, &tok);
    mod->parser.inject_eol_after_eob = TRUE;
    return 0;
  }

  e = p_expr(new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);
  e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(mod, node), mod);
  EXCEPT(e);
  e = scan_expected(mod, TEOL);
  EXCEPT(e);
  goto again;
}

static error p_pre(struct node *node, struct module *mod) {
  node->which = PRE;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_post(struct node *node, struct module *mod) {
  node->which = POST;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_invariant(struct node *node, struct module *mod) {
  node->which = INVARIANT;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(mod, node), mod);
  EXCEPT(e);

  return 0;
}

static error p_example(struct node *node, struct module *mod) {
  node->which = EXAMPLE;

  error e = scan_expected(mod, TSOB);
  EXCEPT(e);
  e = p_block(new_subnode(mod, node), mod);
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
  case Texcept:
    e = p_except(node, mod);
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
  node->which = BLOCK;
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
    e = p_statement(new_subnode(mod, node), mod);
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
  struct toplevel *node_toplevel;
  node->which = fun_or_method;
  switch (fun_or_method) {
  case DEFFUN:
    node->as.DEFFUN.toplevel = *toplevel;
    node_toplevel = &node->as.DEFFUN.toplevel;
    break;
  case DEFMETHOD:
    node->as.DEFMETHOD.toplevel = *toplevel;
    node_toplevel = &node->as.DEFMETHOD.toplevel;
    break;
  default:
    assert(FALSE);
  }

  error e = p_ident(new_subnode(mod, node), mod);
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

    e = p_typeconstraint(new_subnode(mod, node), mod);
    EXCEPT(e);
    goto again;
  default:
    assert(FALSE);
  }

retval:
  e = p_expr(new_subnode(mod, node), mod, T__NONE);
  EXCEPT(e);

  e = scan_oneof(&tok, mod, TEOL, TSOB, TEOB, 0);
  EXCEPT(e);

  if (tok.t == TEOL || tok.t == TEOB) {
    back(mod, &tok);
    node_toplevel->is_prototype = TRUE;
    return 0;
  }

  e = p_block(new_subnode(mod, node), mod);
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
    e = p_expr(new_subnode(mod, node), mod, T__CALL);
    EXCEPT(e);
    goto again;
  }
}

static error p_delegate(struct node *node, struct module *mod) {
  node->which = DELEGATE;

  error e = p_expr(new_subnode(mod, node), mod, T__CALL);
  EXCEPT(e);

  struct token tok;

again:
  e = scan(&tok, mod);
  EXCEPT(e);
  back(mod, &tok);

  if (tok.t == TEOL) {
    return 0;
  }

  e = p_expr(new_subnode(mod, node), mod, T__CALL);
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
    e = p_deftype_statement(new_subnode(mod, node), mod);
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
  error e = p_ident(new_subnode(mod, node), mod);
  EXCEPT(e);

  e = scan_expected(mod, TASSIGN);
  EXCEPT(e);

  e = p_isalist(new_subnode(mod, node), mod);
  EXCEPT(e);

  e = scan_oneof(&tok, mod, TEOL, TSOB, 0);
  EXCEPT(e);

  if (tok.t == TEOL) {
    back(mod, &tok);
    node->as.DEFTYPE.toplevel.is_prototype = TRUE;
    return 0;
  }

  e = p_deftype_block(new_subnode(mod, node), mod);
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

static error p_import_path(struct node *node, struct module *mod) {
  node->which = IMPORT_PATH;

  error e;
  struct token tok;

  e = p_ident(new_subnode(mod, node), mod);
  EXCEPT(e);

again:
  e = scan(&tok, mod);
  EXCEPT(e);

  if (tok.t != TDOT) {
    back(mod, &tok);
    return 0;
  }

  p_ident(new_subnode(mod, node), mod);

  goto again;
}

static error p_import(struct node *node, struct module *mod, const struct toplevel *toplevel,
                      bool is_export) {
  node->which = IMPORT;
  node->as.IMPORT.toplevel = *toplevel;
  node->as.IMPORT.is_export = is_export;

  error e = p_import_path(new_subnode(mod, node), mod);
  EXCEPT(e);

  struct token tok;
  e = scan(&tok, mod);
  EXCEPT(e);

  if (tok.t != Timport && tok.t != Texport) {
    back(mod, &tok);
    return 0;
  }

again:
  e = scan(&tok, mod);
  EXCEPT(e);

  if (tok.t == TTIMES) {
    node->as.IMPORT.is_all = TRUE;
    goto again;
  } else if (tok.t == TIDENT) {
    e = p_ident(new_subnode(mod, node), mod);
    EXCEPT(e);

    goto again;
  } else {
    back(mod, &tok);
    return 0;
  }
}

static error p_toplevel(struct module *mod) {
  struct node *node = new_subnode(mod, &mod->node);

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
  case Tfrom:
  case Timport:
  case Texport:
    e = p_import(node, mod, &toplevel, tok.t == Texport);
    break;
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

  mod->idents.map = calloc(1, sizeof(struct idents_map));
  idents_map_init(mod->idents.map, 0);
  idents_map_set_delete_val(mod->idents.map, -1);
  idents_map_set_custom_hashf(mod->idents.map, token_hash);
  idents_map_set_custom_cmpf(mod->idents.map, token_cmp);

  mod->idents.count = ID__NUM;
  mod->idents.capacity = ID__NUM;
  mod->idents.values = calloc(ID__NUM, sizeof(char *));
  for (int i = 0; i < ID__NUM; ++i) {
    mod->idents.values[i] = predefined_idents_strings[i];

    struct token tok;
    tok.value = predefined_idents_strings[i];
    tok.len = strlen(predefined_idents_strings[i]);
    idents_map_set(mod->idents.map, tok, i);
  }

  mod->node.which = MODULE;
}

error module_open(struct module *mod, const char *fn) {
  module_init(mod);

  error e = module_read(mod, fn);
  EXCEPT(e);

  if (mod->path_len >= 1) {
    mod->node.as.MODULE.name = mod->path[mod->path_len - 1];
  } else {
    mod->node.as.MODULE.name = ID_ANONYMOUS;
  }

  e = module_parse(mod);
  EXCEPT(e);

  return 0;
}
