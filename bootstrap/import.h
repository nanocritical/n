#ifndef IMPORT_H__
#define IMPORT_H__

#include "parser.h"

error lexical_import(struct scope *scope, struct module *mod,
                     struct node *original_import, struct node *import);

#endif