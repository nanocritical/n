#ifndef MOCK_H__
#define MOCK_H__

#include "parser.h"

struct node *mock_deftype(struct module *mod, const char *name);
struct node *mock_defintf(struct module *mod, const char *name);
struct node *mock_deffun(struct module *mod, const char *name);

#endif
