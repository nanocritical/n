#ifndef STAGE_H__
#define STAGE_H__

#include "parser.h"

ERROR stage_load(struct globalctx *gctx,
                 struct stage *stage, const char *entry_point_fn);

#endif
