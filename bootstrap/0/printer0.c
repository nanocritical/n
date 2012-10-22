#ifndef _POSIX_SOURCE
# define _POSIX_SOURCE
#endif
#include <stdio.h>

#include "printer.h"

static void print_token(FILE *out, enum token_type t) {
  fprintf(out, "%s", tokens_string[t]);
}

static void spaces(FILE *out, int indent) {
  assert(indent % 2 == 0);
  for (int i = 0; i < indent; i += 2) {
    fprintf(out, "  ");
  }
}

static void print_expr(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op);
static void print_block(FILE *out, const struct module *mod, int indent, const struct node *node);

static void print_pattern(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, node, T__NONE);
}

static void print_bin(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.BIN.operator;
  const uint32_t prec = OP_PREC(op);
  const uint32_t parent_prec = OP_PREC(parent_op);

  if (prec > parent_prec) {
    fprintf(out, "(");
  }

  print_expr(out, mod, &node->subs[0], op);
  print_token(out, op);
  print_expr(out, mod, &node->subs[1], op);

  if (prec > parent_prec) {
    fprintf(out, ")");
  }
}

static void print_un(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t op = node->as.UN.operator;
  const uint32_t prec = OP_PREC(op);
  const uint32_t parent_prec = OP_PREC(parent_op);

  if (prec > parent_prec) {
    fprintf(out, "(");
  }

  print_token(out, op);
  print_expr(out, mod, &node->subs[0], op);

  if (prec > parent_prec) {
    fprintf(out, ")");
  }
}

static void print_tuple(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t prec = OP_PREC(TCOMMA);
  const uint32_t parent_prec = OP_PREC(parent_op);

  if (prec >= parent_prec) {
    fprintf(out, "(");
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    if (n > 0) {
      fprintf(out, ", ");
    }
    print_expr(out, mod, &node->subs[n], TCOMMA);
  }

  if (prec >= parent_prec) {
    fprintf(out, ")");
  }
}

static void print_call(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const uint32_t prec = OP_PREC(T__CALL);
  const uint32_t parent_prec = OP_PREC(parent_op);

  if (prec >= parent_prec) {
    fprintf(out, "(");
  }

  for (size_t n = 0; n < node->subs_count; ++n) {
    if (n > 0) {
      fprintf(out, " ");
    }
    print_expr(out, mod, &node->subs[n], T__CALL);
  }

  if (prec >= parent_prec) {
    fprintf(out, ")");
  }
}

static void print_init(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, &node->subs[0], T__NONE);
  fprintf(out, "{{ ");

  for (size_t n = 1; n < node->subs_count; n += 2) {
    print_expr(out, mod, &node->subs[n], T__NONE);
    fprintf(out, "=");
    print_expr(out, mod, &node->subs[n + 1], T__CALL);
    fprintf(out, " ");
  }

  fprintf(out, "}}");
}

static void print_expr(FILE *out, const struct module *mod, const struct node *node, uint32_t parent_op) {
  const char *val = NULL;
  error e;

  switch (node->which) {
  case NUL:
    abort();
    fprintf(out, "null");
    break;
  case IDENT:
    e = idents_value(&val, mod, node->as.IDENT.value);
    assert(e == 0);
    fprintf(out, "%s", val);
    break;
  case NUMBER:
    fprintf(out, "%s", node->as.NUMBER.value);
    break;
  case STRING:
    fprintf(out, "%s", node->as.STRING.value);
    break;
  case BIN:
    print_bin(out, mod, node, parent_op);
    break;
  case UN:
    print_un(out, mod, node, parent_op);
    break;
  case CALL:
    print_call(out, mod, node, parent_op);
    break;
  case TUPLE:
    print_tuple(out, mod, node, parent_op);
    break;
  case INIT:
    print_init(out, mod, node);
    break;
  default:
    fprintf(stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_for(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "for ");
  print_pattern(out, mod, &node->subs[0]);
  fprintf(out, " in ");
  print_expr(out, mod, &node->subs[1], T__NONE);
  print_block(out, mod, indent, &node->subs[2]);
}

static void print_while(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "while ");
  print_expr(out, mod, &node->subs[0], T__NONE);
  print_block(out, mod, indent, &node->subs[1]);
}

static void print_if(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "if ");
  print_expr(out, mod, &node->subs[0], T__NONE);
  print_block(out, mod, indent, &node->subs[1]);

  size_t p = 2;
  size_t br_count = node->subs_count - 2;
  while (br_count >= 2) {
    fprintf(out, "\n");
    spaces(out, indent);
    fprintf(out, "elif ");
    print_expr(out, mod, &node->subs[p], T__NONE);
    print_block(out, mod, indent, &node->subs[p+1]);
    p += 2;
    br_count -= 2;
  }

  if (br_count == 1) {
    fprintf(out, "\n");
    spaces(out, indent);
    fprintf(out, "else ");
    print_block(out, mod, indent, &node->subs[p]);
  }
}

static void print_match(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "match ");
  print_expr(out, mod, &node->subs[0], T__NONE);

  for (size_t n = 1; n < node->subs_count; n += 2) {
    fprintf(out, "\n");
    spaces(out, indent);
    fprintf(out, "| ");
    print_expr(out, mod, &node->subs[n], T__NONE);
    print_block(out, mod, indent, &node->subs[n + 1]);
  }
}

static void print_try(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "try");
  print_block(out, mod, indent, &node->subs[0]);
  fprintf(out, "\n");
  spaces(out, indent);
  fprintf(out, "catch ");
  print_expr(out, mod, &node->subs[1], T__NONE);
  print_block(out, mod, indent, &node->subs[2]);
}

static void print_pre(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "pre");
  print_block(out, mod, indent, &node->subs[0]);
}

static void print_post(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "post");
  print_block(out, mod, indent, &node->subs[0]);
}

static void print_invariant(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "invariant");
  print_block(out, mod, indent, &node->subs[0]);
}

static void print_example(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "example");
  print_block(out, mod, indent, &node->subs[0]);
}

static void print_deflet(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "let ");
  print_pattern(out, mod, &node->subs[0]);
  fprintf(out, " = ");
  print_expr(out, mod, &node->subs[1], T__NONE);

  if (node->subs_count > 2) {
    print_block(out, mod, indent, &node->subs[2]);
  }
}

static void print_statement(FILE *out, const struct module *mod, int indent, const struct node *node) {
  switch (node->which) {
  case RETURN:
    fprintf(out, "return");
    if (node->subs_count > 0) {
      fprintf(out, " ");
      print_expr(out, mod, &node->subs[0], T__NONE);
    }
    break;
  case FOR:
    print_for(out, mod, indent, node);
    break;
  case WHILE:
    print_while(out, mod, indent, node);
    break;
  case BREAK:
    fprintf(out, "break");
    break;
  case CONTINUE:
    fprintf(out, "continue");
    break;
  case PASS:
    fprintf(out, "pass");
    break;
  case IF:
    print_if(out, mod, indent, node);
    break;
  case MATCH:
    print_match(out, mod, indent, node);
    break;
  case TRY:
    print_try(out, mod, indent, node);
    break;
  case PRE:
    print_pre(out, mod, indent, node);
    break;
  case POST:
    print_post(out, mod, indent, node);
    break;
  case INVARIANT:
    print_invariant(out, mod, indent, node);
    break;
  case EXAMPLE:
    print_example(out, mod, indent, node);
    break;
  case DEFLET:
    print_deflet(out, mod, indent, node);
    break;
  case IDENT:
  case BIN:
  case UN:
  case CALL:
    print_expr(out, mod, node, T__NONE);
    break;
  default:
    fprintf(stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_block(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "\n");
  for (size_t n = 0; n < node->subs_count; ++n) {
    const struct node *statement = &node->subs[n];
    spaces(out, indent + 2);
    print_statement(out, mod, indent + 2, statement);
    if (n < node->subs_count - 1) {
      fprintf(out, "\n");
    }
  }
}

static void print_typeexpr(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, &node->subs[0], T__NONE);
}

static void print_typeconstraint(FILE *out, const struct module *mod, const struct node *node) {
  print_expr(out, mod, &node->subs[0], T__NONE);
  fprintf(out, ":");
  print_typeexpr(out, mod, &node->subs[1]);
}

static void print_deffun(FILE *out, const struct module *mod, int indent, const struct node *node) {
  const size_t arg_count = node->subs_count - (node->as.DEFFUN.toplevel.is_prototype ? 2 : 3);
  const struct node *name = &node->subs[0];
  const struct node *retval = &node->subs[1 + arg_count];

  fprintf(out, "fun ");
  print_expr(out, mod, name, T__NONE);

  for (size_t n = 0; n < arg_count; ++n) {
    fprintf(out, " ");
    const struct node *arg = &node->subs[1 + n];
    print_typeconstraint(out, mod, arg);
  }

  fprintf(out, " = ");
  print_expr(out, mod, retval, T__NONE);

  if (!node->as.DEFFUN.toplevel.is_prototype) {
    const struct node *block = &node->subs[1 + arg_count + 1];
    print_block(out, mod, 0, block);
  }

  fprintf(out, "\n");
}

static void print_delegate(FILE *out, const struct module *mod, const struct node *node) {
  fprintf(out, "delegate ");
  print_expr(out, mod, &node->subs[0], T__CALL);

  for (size_t n = 1; n < node->subs_count; ++n) {
    fprintf(out, " ");
    print_expr(out, mod, &node->subs[n], T__CALL);
  }
}

static void print_deftype_statement(FILE *out, const struct module *mod, int indent, const struct node *node) {
  switch (node->which) {
  case DEFLET:
    print_deflet(out, mod, indent, node);
    break;
  case DELEGATE:
    print_delegate(out, mod, node);
    break;
  case INVARIANT:
    print_invariant(out, mod, indent, node);
    break;
  case EXAMPLE:
    print_example(out, mod, indent, node);
    break;
  case TYPECONSTRAINT:
    print_typeconstraint(out, mod, node);
    break;
  default:
    fprintf(stderr, "Unsupported node: %d\n", node->which);
    assert(FALSE);
  }
}

static void print_deftype_block(FILE *out, const struct module *mod, int indent, const struct node *node) {
  fprintf(out, "\n");
  for (size_t n = 0; n < node->subs_count; ++n) {
    const struct node *statement = &node->subs[n];
    spaces(out, indent + 2);
    print_deftype_statement(out, mod, indent + 2, statement);
    if (n < node->subs_count - 1) {
      fprintf(out, "\n");
    }
  }
}

static void print_isalist(FILE *out, const struct module *mod, const struct node *node) {
  for (size_t n = 0; n < node->subs_count; ++n) {
    fprintf(out, " ");
    print_expr(out, mod, &node->subs[n], T__CALL);
  }
}

static void print_deftype(FILE *out, const struct module *mod, int indent, const struct node *node) {
  const struct node *name = &node->subs[0];
  const bool has_isalist = node->as.DEFFUN.toplevel.is_prototype
    ? node->subs_count > 1 : node->subs_count > 2;

  fprintf(out, "type ");
  print_expr(out, mod, name, T__NONE);
  fprintf(out, " =");

  if (has_isalist) {
    const struct node *isalist = &node->subs[1];
    print_isalist(out, mod, isalist);
  }

  if (!node->as.DEFFUN.toplevel.is_prototype) {
    const struct node *block = &node->subs[has_isalist ? 2 : 1];
    print_deftype_block(out, mod, 0, block);
  }

  fprintf(out, "\n");
}

static void print_defmethod(FILE *out, const struct module *mod, int indent, const struct node *node) {
  const size_t arg_count = node->subs_count - (node->as.DEFFUN.toplevel.is_prototype ? 2 : 3);
  const struct node *name = &node->subs[0];
  const struct node *retval = &node->subs[1 + arg_count];

  const char *scope = NULL;
  error e = idents_value(&scope, mod, node->as.DEFMETHOD.toplevel.scope);
  assert(e == 0);

  fprintf(out, "%s method ", scope);
  print_expr(out, mod, name, T__NONE);

  for (size_t n = 0; n < arg_count; ++n) {
    fprintf(out, " ");
    const struct node *arg = &node->subs[1 + n];
    print_typeconstraint(out, mod, arg);
  }

  fprintf(out, " = ");
  print_expr(out, mod, retval, T__NONE);

  if (!node->as.DEFMETHOD.toplevel.is_prototype) {
    const struct node *block = &node->subs[1 + arg_count + 1];
    print_block(out, mod, 0, block);
  }

  fprintf(out, "\n");
}

static void print_defintf(FILE *out, const struct module *mod, int indent, const struct node *node) {
}

static void print_import_path(FILE *out, const struct module *mod, const struct node *node) {
  for (size_t n = 0; n < node->subs_count; ++n) {
    if (n > 0) {
      fprintf(out, ".");
    }

    print_expr(out, mod, &node->subs[n], T__CALL);
  }
}

static void print_import(FILE *out, const struct module *mod, int indent, const struct node *node) {
  const char *kind;
  if (node->as.IMPORT.is_export) {
    kind = "export";
  } else {
    kind = "import";
  }

  if (node->as.IMPORT.is_all || node->subs_count > 1) {
    fprintf(out, "from ");
  } else {
    fprintf(out, "%s ", kind);
  }

  print_import_path(out, mod, &node->subs[0]);

  if (node->as.IMPORT.is_all) {
    fprintf(out, " %s *", kind);
  } else if (node->subs_count > 1) {
    fprintf(out, " %s ", kind);

    for (size_t n = 1; n < node->subs_count; ++n) {
      print_expr(out, mod, &node->subs[n], T__CALL);
    }
  }
}

static void print_module(FILE *out, const struct module *mod) {
  const struct node *top = &mod->node;

  for (size_t n = 0; n < top->subs_count; ++n) {
    const struct node *node = &top->subs[n];

    switch (node->which) {
    case DEFFUN:
      print_deffun(out, mod, 0, node);
      break;
    case DEFTYPE:
      print_deftype(out, mod, 0, node);
      break;
    case DEFMETHOD:
      print_defmethod(out, mod, 0, node);
      break;
    case DEFINTF:
      print_defintf(out, mod, 0, node);
      break;
    case DEFLET:
      print_deflet(out, mod, 0, node);
      break;
    case IMPORT:
      print_import(out, mod, 0, node);
      break;
    default:
      fprintf(stderr, "Unsupported node: %d\n", node->which);
      assert(FALSE);
    }

    fprintf(out, "\n");
  }
}

error printer_pretty(int fd, const struct module *mod) {
  FILE *out = fdopen(fd, "w");
  if (out == NULL) {
    EXCEPTF(errno, "Invalid output file descriptor '%d'", fd);
  }

  print_module(out, mod);
  return 0;
}
