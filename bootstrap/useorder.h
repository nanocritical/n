#ifndef USEORDER_H__
#define USEORDER_H__

#include "types.h"

enum forward {
  FWD_DECLARE_TYPES,
  FWD_DEFINE_DYNS,
  FWD_DEFINE_TYPES,
  FWD_DECLARE_FUNCTIONS,
  FWD_DEFINE_FUNCTIONS,
  FORWARD__NUM,
};

extern const char *forward_guards[];

struct useorder {
  struct vectyp dependencies;
  struct fintypset marks;
  const struct module *mod;
  bool header;
  enum forward fwd;
};

void useorder_build(struct useorder *uorder, const struct module *mod,
                    bool header, enum forward fwd);
void debug_useorder_print(struct useorder *uorder);
void useorder_destroy(struct useorder *uorder);

#endif
