#ifndef REFLECT_H__
#define REFLECT_H__

#include "nodes.h"

#include <lib/n/reflect/reflect.h>

void reflect_fill_type(struct __Type *typ, const struct module *mod,
                       const struct typ *t);

#endif
