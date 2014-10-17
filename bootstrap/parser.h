#ifndef PARSER_H__
#define PARSER_H__

#include "nodes.h"

int parser_line(const struct module *mod, const struct token *tok);
int parser_column(const struct parser *parser, const struct token *tok);

void globalctx_init(struct globalctx *gctx);
ERROR module_open(struct globalctx *gctx, struct stage *stage, struct module *mod,
                  const char *prefix, const char *fn);
const char *module_component_filename_at(const struct module *mod, size_t pos);

ident gensym(struct module *mod);

const char *predefined_idents_strings[ID__NUM];
const char *idents_value(const struct globalctx *gctx, ident id);
ident idents_add(struct globalctx *gctx, const struct token *tok);
ident idents_add_string(struct globalctx *gctx, const char *name, size_t len);

void deffun_count_args(struct node *def);
bool name_is_export(const struct module *mod, const struct node *name);

void copy_and_extend_import_path(struct module *mod, struct node *imported,
                                 const struct node *import, const struct token *tok);

#endif
