#include "parser.h"
#include "printer.h"
#include "passes.h"
#include "table.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

#define DIR_MODULE_NAME "module.n"
#define CFLAGS "-Wall -Wno-missing-braces -std=c99 -pedantic -I. -g"
#define LDFLAGS CFLAGS " -Wl,--gc-sections"

static error sh(const char *cmd) {
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

static char *o_filename(const char *filename) {
  char *o_fn = calloc(strlen(filename) + sizeof(".o"), sizeof(char));
  sprintf(o_fn, "%s.o", filename);
  return o_fn;
}

static error cc(const struct module *mod, const char *o_fn,
                const char *c_fn, const char *h_fn) {
  static const char *fmt = "gcc " CFLAGS " -xc %s -c -o %s";
  char *cmd = calloc(strlen(fmt) + strlen(c_fn) + strlen(o_fn) - 4 + 1, sizeof(char));
  sprintf(cmd, fmt, c_fn, o_fn);

  error e = sh(cmd);
  free(cmd);
  EXCEPT(e);
  return 0;
}

static char *file_list(const struct module **modules, size_t count,
                       char *(*process)(const char *)) {
  size_t len = 0;
  char *list = NULL;

  for (size_t n = 0; n < count; ++n) {
    const struct module *mod = modules[n];
    const size_t old_len = len;
    char *o_fn = process(mod->filename);
    len += strlen(o_fn) + 1;
    list = realloc(list, (len + 1) * sizeof(char));
    strcpy(list + old_len, " ");
    strcpy(list + old_len + 1, o_fn);
    free(o_fn);
  }

  return list;
}

static error clink(const char *out_fn, const char *inputs, const char *extra) {
  static const char fmt[] = "gcc " LDFLAGS " %s %s -o %s";
  size_t len = strlen(fmt) + strlen(inputs) + strlen(extra) + strlen(out_fn) - 6;
  char *cmd = calloc(len + 1, sizeof(char));
  sprintf(cmd, fmt, inputs, extra, out_fn);

  error e = sh(cmd);
  free(cmd);
  EXCEPT(e);
  return 0;
}

static error generate(struct node *node) {
  assert(node->which == MODULE);
  struct module *mod = node->as.MODULE.mod;

  const char *fn = mod->filename;
  error e;

  char *out_fn = calloc(strlen(fn) + sizeof(".tree.out"), sizeof(char));
  sprintf(out_fn, "%s.tree.out", fn);

  int fd = creat(out_fn, 00600);
  if (fd < 0) {
    EXCEPTF(errno, "Cannot open output file '%s'", out_fn);
  }
  free(out_fn);

  e = printer_tree(fd, mod, NULL);
  EXCEPT(e);
  close(fd);

  out_fn = calloc(strlen(fn) + sizeof(".pretty.out"), sizeof(char));
  sprintf(out_fn, "%s.pretty.out", fn);

  fd = creat(out_fn, 00600);
  if (fd < 0) {
    EXCEPTF(errno, "Cannot open output file '%s'", out_fn);
  }
  free(out_fn);

  e = printer_pretty(fd, mod);
  EXCEPT(e);
  close(fd);

  char *c_fn = calloc(strlen(fn) + sizeof(".c.out"), sizeof(char));
  sprintf(c_fn, "%s.c.out", fn);

  fd = creat(c_fn, 00600);
  if (fd < 0) {
    EXCEPTF(errno, "Cannot open output file '%s'", c_fn);
  }

  e = printer_c(fd, mod);
  EXCEPT(e);
  close(fd);

  char *h_fn = calloc(strlen(fn) + sizeof(".h.out"), sizeof(char));
  sprintf(h_fn, "%s.h.out", fn);

  fd = creat(h_fn, 00600);
  if (fd < 0) {
    EXCEPTF(errno, "Cannot open output file '%s'", h_fn);
  }

  e = printer_h(fd, mod);
  EXCEPT(e);
  close(fd);

  char *o_fn = o_filename(mod->filename);
  e = cc(mod, o_fn, c_fn, h_fn);
  EXCEPT(e);

  free(o_fn);
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

  if (op->filter == 0 || node->which == op->filter) {
    error e = op->fun(node, op->user, stop);
    EXCEPT(e);
  }

  return 0;
}

static error for_all_nodes(struct node *root,
                           enum node_which filter, // 0 to get all nodes
                           error (*node_fun)(struct node *node, void *user, bool *stop_descent),
                           void *user) {
  static const step downsteps[] = {
    step_for_all_nodes,
    NULL,
  };
  static const step upsteps[] = {
    NULL,
  };

  struct node_op op = {
    .filter = filter,
    .fun = node_fun,
    .user = user,
  };

  error e = pass(NULL, root, downsteps, upsteps, NULL, &op);
  EXCEPT(e);

  return 0;
}

static error load_module(struct module **main_mod,
                         struct globalctx *gctx, const char *prefix, const char *fn) {
  struct module *mod = calloc(1, sizeof(struct module));

  error e = module_open(gctx, mod, prefix, fn);
  EXCEPT(e);

  gctx->loaded = realloc(gctx->loaded, (gctx->loaded_count + 1) * sizeof(struct node *));
  gctx->loaded[gctx->loaded_count] = mod->root;
  gctx->loaded_count += 1;

  e = zeropass(mod, NULL, NULL);
  EXCEPT(e);

  if (main_mod != NULL) {
    *main_mod = mod;
  }

  return 0;
}

static void import_filename(char **fn, size_t *len,
                            struct module *mod, struct node *import) {
  struct node *to_append = NULL;

  switch (import->which) {
  case BIN:
    import_filename(fn, len, mod, import->subs[0]);
    to_append = import->subs[1];
    break;
  case IDENT:
    to_append = import;
    break;
  default:
    assert(FALSE);
  }

  assert(to_append->which == IDENT);
  const char *app = idents_value(mod->gctx, to_append->as.IDENT.name);
  const size_t applen = strlen(app);

  if (*len == 0) {
    *fn = realloc(*fn, applen + 1);
    strcpy(*fn, app);
    *len += applen;
  } else {
    *fn = realloc(*fn, *len + 1 + applen + 1);
    (*fn)[*len] = '/';
    strcpy(*fn + *len + 1, app);
    *len += applen + 1;
  }
}

static void import_module_path(char **module_path, size_t *mplen,
                               struct module *mod, struct node *import) {
  import_filename(module_path, mplen, mod, import);
  char *mp = *module_path;
  for (size_t n = 0; mp[n] != '\0'; ++n) {
    if (mp[n] == '/') {
      mp[n] = '.';
    }
  }
}


static error try_import(char **fn, struct module *mod, struct node *import,
                        const char *prefix) {
  assert(import->which == IMPORT);
  struct node *import_path = import->subs[0];

  *fn = NULL;

  size_t prefix_len = strlen(prefix);
  size_t len = prefix_len;
  char *base = calloc(len + 1, sizeof(char));
  strcpy(base, prefix);

  import_filename(&base, &len, mod, import_path);

  struct stat st = { 0 };

  char *file = calloc(len + 2 + 1, sizeof(char));
  strcpy(file, base);
  strcpy(file + len, ".n");
  int ret = stat(file, &st);
  if (ret == 0 && S_ISREG(st.st_mode)) {
    prefix_len += prefix_len > 0 ? 1 : 0;
    *fn = strdup(file + prefix_len);
    free(file);
    free(base);
    return 0;
  }
  free(file);

  char *dir = calloc(len + 1 + strlen(DIR_MODULE_NAME) + 1, sizeof(char));
  strcpy(dir, base);
  strcpy(dir + len, "/");
  strcpy(dir + len + 1, DIR_MODULE_NAME);
  memset(&st, 0, sizeof(st));
  ret = stat(dir, &st);
  if (ret == 0 && S_ISREG(st.st_mode)) {
    prefix_len += prefix_len > 0 ? 1 : 0;
    *fn = strdup(dir + prefix_len);
    free(dir);
    free(base);
    return 0;
  }
  free(dir);
  free(base);

  return EINVAL;
}

static error lookup_import(const char **prefix, char **fn,
                           struct module *mod, struct node *import,
                           const char **prefixes) {
  char *mod_dirname = xdirname(mod->filename);
  error e = try_import(fn, mod, import, mod_dirname);
  if (!e) {
    *prefix = strdup(mod_dirname);
    return 0;
  }

  for (size_t n = 0; prefixes[n] != NULL; ++n) {
    error e = try_import(fn, mod, import, prefixes[n]);
    if (!e) {
      *prefix = prefixes[n];
      return 0;
    }
  }

  char *module_path = NULL;
  size_t module_path_len = 0;
  struct node *import_path = import->subs[0];
  import_module_path(&module_path, &module_path_len, mod, import_path);

  fprintf(stderr, "After looking up in the directories:\n");
  for (size_t n = 0; prefixes[n] != NULL; ++n) {
    fprintf(stderr, "\t'%s'\n", prefixes[n]);
  }

  e = mk_except(mod, import, "module '%s' not found", module_path);
  free(module_path);
  EXCEPT(e);
  return 0;
}

static error load_import(struct node *node, void *user, bool *stop) {
  *stop = TRUE;

  assert(node->which == IMPORT);
  const char **prefixes = user;
  struct module *mod = node_module_owner(node);

  struct node *existing = NULL;
  error e = scope_lookup_module(&existing, mod, node->subs[0], TRUE);
  if (!e && !existing->as.MODULE.is_placeholder) {
    return 0;
  }

  const char *prefix = NULL;
  char *fn = NULL;
  e = lookup_import(&prefix, &fn, mod, node, prefixes);
  EXCEPT(e);

  e = load_module(NULL, mod->gctx, prefix, fn);
  free(fn);
  EXCEPT(e);

  return 0;
}

static error load_imports(struct node *node, const char **prefixes) {
  assert(node->which == MODULE);

  if (node->as.MODULE.is_placeholder) {
    return 0;
  }

  assert(node->as.MODULE.mod != NULL);
  error e = for_all_nodes(node->as.MODULE.mod->root, IMPORT, load_import,
                          prefixes);
  EXCEPT(e);

  return 0;
}

struct dependencies {
  struct module **tmp;
  size_t tmp_count;
  struct module **modules;
  size_t modules_count;
  struct globalctx *gctx;
};

static error gather_dependencies(struct node *node, struct dependencies *deps);

static error step_gather_dependencies_in_module(struct module *mod, struct node *node, void *user, bool *stop) {
  struct dependencies *deps = user;

  assert(node->which != MODULE);
  if (node->which != IMPORT) {
    return 0;
  }

  *stop = TRUE;

  struct node *nmod = NULL;
  error e = scope_lookup_module(&nmod, node_module_owner(node), node->subs[0], FALSE);
  EXCEPT(e);

  deps->tmp_count += 1;
  deps->tmp = realloc(deps->tmp, deps->tmp_count * sizeof(*deps->tmp));
  deps->tmp[deps->tmp_count - 1] = node_module_owner(nmod);

  e = gather_dependencies(nmod, deps);
  EXCEPT(e);

  return 0;
}

static error gather_dependencies(struct node *node, struct dependencies *deps) {
  assert(node->which == MODULE);

  if (node->as.MODULE.is_placeholder) {
    return 0;
  }

  deps->tmp_count += 1;
  deps->tmp = realloc(deps->tmp, deps->tmp_count * sizeof(*deps->tmp));
  deps->tmp[deps->tmp_count - 1] = node_module_owner(node);

  static const step down[] = {
    step_gather_dependencies_in_module,
    NULL,
  };

  static const step up[] = {
    NULL,
  };

  error e = pass(node_module_owner(node), node_module_owner(node)->body,
                 down, up, NULL, deps);
  EXCEPT(e);

  return 0;
}

HTABLE_SPARSE(dependencies_map, int, struct module *);
implement_htable_sparse(__attribute__((unused)) static, dependencies_map, int, struct module *);

static uint32_t module_pointer_hash(const struct module **mod) {
  return hash32_hsieh(mod, sizeof(*mod));
}

static int module_pointer_cmp(const struct module **a, const struct module **b) {
  return memcmp(a, b, sizeof(*a));
}

static error calculate_dependencies(struct dependencies *deps) {
  struct dependencies_map map;
  dependencies_map_init(&map, 0);
  dependencies_map_set_delete_val(&map, 0);
  dependencies_map_set_custom_hashf(&map, module_pointer_hash);
  dependencies_map_set_custom_cmpf(&map, module_pointer_cmp);

  for (ssize_t n = deps->tmp_count - 1; n >= 0; --n) {
    struct module *m = deps->tmp[n];
    int already = dependencies_map_set(&map, m, 1);
    if (already) {
      continue;
    }

    deps->modules_count += 1;
    deps->modules = realloc(deps->modules, deps->modules_count * sizeof(*deps->modules));
    deps->modules[deps->modules_count - 1] = m;
  }

  dependencies_map_destroy(&map);

  free(deps->tmp);
  deps->tmp = NULL;
  deps->tmp_count = 0;

  return 0;
}

static error run_examples(const struct dependencies *deps) {
  static const char *out_fn = "a.out.examples";
  static const char *main_fn = "a.out.examples.c";

  FILE *run = fopen(main_fn, "w");
  if (run == NULL) {
    EXCEPTF(errno, "Cannot open output file '%s'", main_fn);
  }

  for (size_t n = 0; n < deps->modules_count; ++n) {
    const struct module *mod = deps->modules[n];
    fprintf(run, "void %s(void);\n", printer_c_runexamples_name(mod));
  }

  fprintf(run, "int main(void) {\n");
  for (size_t n = 0; n < deps->modules_count; ++n) {
    const struct module *mod = deps->modules[n];
    fprintf(run, "%s();\n", printer_c_runexamples_name(mod));
  }
    fprintf(run, "}\n");
  fclose(run);

  char *inputs = file_list((const struct module **)deps->modules,
                           deps->modules_count, o_filename);
  error e = clink(out_fn, inputs, main_fn);
  free(inputs);
  EXCEPT(e);

  static const char *fmt = "./%s";
  char *cmd = calloc(strlen(fmt) + strlen(main_fn) - 2 + 1, sizeof(char));
  sprintf(cmd, fmt, out_fn);
  e = sh(cmd);
  free(cmd);
  EXCEPTF(e, "examples failed");

  return 0;
}

static error program_link(const struct dependencies *deps) {
  const char *out_fn = "a.out";
  const char *main_fn = "a.out.c";

  FILE *run = fopen(main_fn, "w");
  if (run == NULL) {
    EXCEPTF(errno, "Cannot open output file '%s'", main_fn);
  }
  fprintf(run, "int __Nmain();\n");
  fprintf(run, "int main(int argc, char **argv, char **env) {\nreturn __Nmain(argc, argv, env);\n}\n");
  fclose(run);

  char *inputs = file_list((const struct module **)deps->modules,
                           deps->modules_count, o_filename);
  error e = clink(out_fn, inputs, main_fn);
  free(inputs);
  EXCEPT(e);

  return 0;
}

int main(int argc, char **argv) {
  struct globalctx gctx;
  globalctx_init(&gctx);

  if (argc != 2) {
    fprintf(stderr, "Usage: %s <main.n>\n", argv[0]);
    exit(1);
  }

  struct module *main_mod = NULL;
  error e = load_module(&main_mod, &gctx, NULL, argv[1]);
  EXCEPT(e);

  const char *prefixes[] = { "", "lib", NULL };

  size_t processed = 0;
  while (processed < gctx.loaded_count) {
    e = load_imports(gctx.loaded[processed], prefixes);
    EXCEPT(e);
    processed += 1;
  }

  struct dependencies deps = { 0 };
  deps.gctx = &gctx;

  e = gather_dependencies(main_mod->root, &deps);
  EXCEPT(e);

  e = calculate_dependencies(&deps);
  EXCEPT(e);

  for (size_t s = 0; fwd_passes[s] != NULL; ++s) {
    for (size_t n = 0; n < deps.modules_count; ++n) {
      struct module *mod = deps.modules[n];
      e = fwd_passes[s](mod, NULL, NULL);
      EXCEPT(e);
    }
  }

  for (size_t n = 0; n < deps.modules_count; ++n) {
    struct module *mod = deps.modules[n];

    for (size_t s = 0; body_passes[s] != NULL; ++s) {
      e = body_passes[s](mod, NULL, NULL);
      EXCEPT(e);
    }
  }

  for (size_t n = 0; n < deps.modules_count; ++n) {
    struct module *mod = deps.modules[n];

    e = generate(mod->root);
    EXCEPT(e);
  }

  e = run_examples(&deps);
  EXCEPT(e);

  e = program_link(&deps);
  EXCEPT(e);

  return 0;
}
