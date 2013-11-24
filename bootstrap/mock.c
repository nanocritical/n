#include "mock.h"

struct node *mock_deftype(struct module *mod, const char *name) {
  G(test, mod->body, DEFTYPE,
     G_IDENT(_name, test, name);
     G(genargs, test, GENARGS);
     G(isalist, test, ISALIST));
  return test;
}

struct node *mock_defintf(struct module *mod, const char *name) {
  G(test, mod->body, DEFTYPE,
     G_IDENT(_name, test, name);
     G(genargs, test, GENARGS);
     G(isalist, test, ISALIST));
  return test;
}

struct node *mock_deffun(struct module *mod, const char *name) {
  G(foo, mod->body, DEFFUN,
     G_IDENT(_name, foo, name);
     G(genargs, foo, GENARGS);
     G(funargs, foo, FUNARGS,
        G_IDENT(retval, funargs, "void"));
     G(body, foo, BLOCK));
  return foo;
}
