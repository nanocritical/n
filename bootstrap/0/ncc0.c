#include "parser.h"
#include "printer.h"
#include "firstpass.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

static error cc(const struct module *mod, const char *c_fn, const char *h_fn) {
  static const char *fmt = "gcc -Wall -std=c99 -pedantic -Inlang_site -xc %s";
  char *cmd = calloc(strlen(fmt) + strlen(c_fn) + 1, sizeof(char));
  sprintf(cmd, fmt, c_fn);

  int status = system(cmd);
  if (status == -1) {
    EXCEPTF(errno, "system(3) failed");
  }
  if (WIFSIGNALED(status)) {
    EXCEPTF(ECHILD, "command terminated by signal %d: %s", WTERMSIG(status), cmd);
  } else if (WEXITSTATUS(status) != 0) {
    EXCEPTF(ECHILD, "command exited with %d: %s", WEXITSTATUS(status), cmd);
  }

  return 0;
}

static error register_module(struct globalctx *gctx, struct module *mod) {
  const size_t last = mod->path_len - 1;
  struct node *root = &gctx->absolute_root;
  for (size_t p = 0; p <= last; ++p) {
    ident i = mod->path[p];
    struct node *m = NULL;
    error e = scope_lookup_ident(&m, mod, root->scope, i, TRUE);
    if (e == EINVAL) {
      m = node_new_subnode(mod, root);
      m->which = MODULE;
      m->as.MODULE.name = i;
      m->as.MODULE.is_placeholder = TRUE;
      m->as.MODULE.mod = mod;
      m->scope = scope_new(m);

      e = scope_define_ident(mod, root->scope, i, m);
      EXCEPT(e);
    } else if (e) {
      // Repeat bound-to-fail lookup to get the error message right.
      e = scope_lookup_ident(&m, mod, root->scope, i, FALSE);
      EXCEPT(e);
    } else {
      if (p == last) {
        assert(m->which == MODULE);
        if (!m->as.MODULE.is_placeholder) {
          EXCEPTF(EINVAL, "Cannot load_module module '%s' more than once",
                  mod->filename);
        } else {
          for (size_t s = 0; s < m->subs_count; ++s) {
            struct node *to_save = m->subs[s];
            assert(to_save->which == MODULE);
            e = scope_define_ident(mod, mod->root.scope,
                                   to_save->as.MODULE.name, to_save);
            EXCEPT(e);
          }

          e = scope_define_ident(mod, root->scope, i, &mod->root);
          EXCEPT(e);
        }
      }
    }

    root = m;
  }

  return 0;
}

static error zero(struct globalctx *gctx, struct module *mod, const char *fn) {
  error e = module_open(gctx, mod, fn);
  EXCEPT(e);

  step zeropass_down[] = {
    step_detect_deftype_kind,
    step_add_builtin_members,
    step_add_builtin_functions,
    step_add_builtin_methods,
    step_add_builtin_self,
    step_add_codegen_variables,
    NULL,
  };
  step zeropass_up[] = {
    step_add_scopes,
    NULL,
  };

  e = pass(mod, NULL, zeropass_down, zeropass_up, NULL);
  EXCEPT(e);

  return 0;
}

static error first(struct node *node, void *ignore, bool *ignore2) {
  assert(node->which == MODULE);
  struct module *mod = node->as.MODULE.mod;

  step firstpass_down[] = {
    step_lexical_scoping,
    step_type_definitions,
    step_type_destruct_mark,
    step_type_gather_returns,
    step_type_gather_excepts,
    NULL,
  };
  step firstpass_up[] = {
    step_type_inference,
    step_operator_call_inference,
    step_unary_call_inference,
    step_ctor_call_inference,
    step_call_arguments_prepare,
    step_temporary_inference,
    NULL,
  };

  error e = pass(mod, NULL, firstpass_down, firstpass_up, NULL);
  EXCEPT(e);

  return 0;
}

error generate(struct node *node, void *ignore, bool *ignore2) {
  assert(node->which == MODULE);
  struct module *mod = node->as.MODULE.mod;

  const char *fn = mod->filename;
  error e;

  char *out_fn = malloc(strlen(fn) + sizeof(".tree.out"));
  sprintf(out_fn, "%s.tree.out", fn);

  int fd = creat(out_fn, 00600);
  if (fd < 0) {
    EXCEPTF(errno, "Cannot open output file '%s'", out_fn);
  }
  free(out_fn);

  e = printer_tree(fd, mod, NULL);
  EXCEPT(e);
  close(fd);

  out_fn = malloc(strlen(fn) + sizeof(".pretty.out"));
  sprintf(out_fn, "%s.pretty.out", fn);

  fd = creat(out_fn, 00600);
  if (fd < 0) {
    EXCEPTF(errno, "Cannot open output file '%s'", out_fn);
  }
  free(out_fn);

  e = printer_pretty(fd, mod);
  EXCEPT(e);
  close(fd);

  char *c_fn = malloc(strlen(fn) + sizeof(".c.out"));
  sprintf(c_fn, "%s.c.out", fn);

  fd = creat(c_fn, 00600);
  if (fd < 0) {
    EXCEPTF(errno, "Cannot open output file '%s'", c_fn);
  }

  e = printer_c(fd, mod);
  EXCEPT(e);
  close(fd);

  char *h_fn = malloc(strlen(fn) + sizeof(".h.out"));
  sprintf(h_fn, "%s.h.out", fn);

  fd = creat(h_fn, 00600);
  if (fd < 0) {
    EXCEPTF(errno, "Cannot open output file '%s'", h_fn);
  }

  e = printer_h(fd, mod);
  EXCEPT(e);
  close(fd);

  e = cc(mod, c_fn, h_fn);
  EXCEPT(e);

  free(c_fn);
  free(h_fn);

  return 0;
}

struct node_op {
  enum node_which filter;
  error (*fun)(struct node *node, void *user, bool *stop);
  void *user;
};

static error step_for_all_nodes(struct module *mod, struct node *node,
                                void *user, bool *stop) {
  struct node_op *op = user;

  if (node->which == op->filter) {
    error e = op->fun(node, op->user, stop);
    EXCEPT(e);
  }

  return 0;
}

static error for_all_nodes(struct node *root,
                           enum node_which filter,
                           error (*node_fun)(struct node *node, void *user, bool *stop),
                           void *user) {
  step downsteps[] = {
    step_for_all_nodes,
    NULL,
  };
  step upsteps[] = {
    NULL,
  };

  struct node_op op = {
    .filter = filter,
    .fun = node_fun,
    .user = user,
  };

  error e = pass(NULL, root, downsteps, upsteps, &op);
  EXCEPT(e);

  return 0;
}

static error load_module(struct globalctx *gctx, const char *fn) {
  struct module *mod = calloc(1, sizeof(struct module));

  error e = zero(gctx, mod, fn);
  EXCEPT(e);

  e = register_module(gctx, mod);
  EXCEPT(e);

  return 0;
}

static error lookup_import(char **fn, struct module *mod, struct node *node) {
  assert(node->which == IMPORT);

  *fn = calloc(1, sizeof(char));

  for (size_t n = 0; n < node->subs_count; ++n) {
    struct node *s = node->subs[n];
    assert(s->which == IDENT);
    const char *ident = idents_value(mod, s->as.IDENT.name);

    const size_t len = 1 + strlen(ident);
    *fn = realloc(*fn, strlen(*fn) + len + 1);
    strcpy(*fn, ".");
    strcpy(*fn + 1, ident);
    *fn += len;
  }

  return 0;
}

static error load_import(struct node *node, void *user, bool *stop) {
  struct module *mod = user;

  char *fn = NULL;
  error e = lookup_import(&fn, mod, node);
  EXCEPT(e);

  e = load_module(mod->gctx, fn);
  free(fn);
  EXCEPT(e);

  *stop = TRUE;

  return 0;
}

static error load_imports(struct node *node, void *user, bool *stop) {
  assert(node->which == MODULE);
  struct globalctx *gctx = user;

  error e = for_all_nodes(&gctx->absolute_root, IMPORT, load_import,
                          node->as.MODULE.mod);
  EXCEPT(e);

  return 0;
}

int main(int argc, char **argv) {
  struct globalctx gctx;
  globalctx_init(&gctx);

  error e;
  for (int i = 1; i < argc; ++i) {
    e = load_module(&gctx, argv[i]);
    EXCEPT(e);
  }

  e = for_all_nodes(&gctx.absolute_root, MODULE, load_imports, &gctx);
  EXCEPT(e);

  e = for_all_nodes(&gctx.absolute_root, MODULE, first, NULL);
  EXCEPT(e);

  e = for_all_nodes(&gctx.absolute_root, MODULE, generate, NULL);
  EXCEPT(e);

  return 0;
}
