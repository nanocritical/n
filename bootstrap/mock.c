#include "mock.h"

struct node *mock_deftype(struct module *mod, const char *name) {
  MK(test, mod->body, DEFTYPE,
     MK_IDENT(_name, test, name);
     MK(genargs, test, GENARGS);
     MK(isalist, test, ISALIST));
  return test;
}

struct node *mock_defintf(struct module *mod, const char *name) {
  MK(test, mod->body, DEFTYPE,
     MK_IDENT(_name, test, name);
     MK(genargs, test, GENARGS);
     MK(isalist, test, ISALIST));
  return test;
}

struct node *mock_deffun(struct module *mod, const char *name) {
  MK(foo, mod->body, DEFFUN,
     MK_IDENT(_name, foo, name);
     MK(genargs, foo, GENARGS);
     MK(funargs, foo, FUNARGS,
        MK_IDENT(retval, funargs, "void"));
     MK(body, foo, BLOCK));
  return foo;
}
