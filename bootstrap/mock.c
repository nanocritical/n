#include "mock.h"

#include "parser.h"

struct node *mock_deftype(struct module *mod, const char *name) {
  GSTART();
  G0(test, mod->body, DEFTYPE,
     G_IDENT(_name, name);
     G(genargs, GENARGS);
     G(isalist, ISALIST));
  return test;
}

struct node *mock_defintf(struct module *mod, const char *name) {
  GSTART();
  G0(test, mod->body, DEFTYPE,
     G_IDENT(_name, name);
     G(genargs, GENARGS);
     G(isalist, ISALIST));
  return test;
}

struct node *mock_deffun(struct module *mod, const char *name) {
  GSTART();
  G0(foo, mod->body, DEFFUN,
     G_IDENT(_name, name);
     G(genargs, GENARGS);
     G(funargs, FUNARGS,
        G_IDENT(retval, "void"));
     G(body, BLOCK));
  return foo;
}
