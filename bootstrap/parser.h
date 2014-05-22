#ifndef PARSER_H__
#define PARSER_H__

#include "nodes.h"

int parser_line(const struct parser *parser, const struct token *tok);
int parser_column(const struct parser *parser, const struct token *tok);

void globalctx_init(struct globalctx *gctx);
error module_open(struct globalctx *gctx, struct stage *stage, struct module *mod,
                  const char *prefix, const char *fn);

ident gensym(struct module *mod);

const char *predefined_idents_strings[ID__NUM];
const char *idents_value(const struct globalctx *gctx, ident id);
ident idents_add(struct globalctx *gctx, const struct token *tok);
ident idents_add_string(struct globalctx *gctx, const char *name, size_t len);

void copy_and_extend_import_path(struct module *mod, struct node *imported,
                                 const struct node *import, const struct token *tok);

#endif
