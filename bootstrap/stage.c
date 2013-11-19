#include "stage.h"

#include "parser.h"
#include "printer.h"
#include "passes.h"
#include "table.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#define DIR_MODULE_NAME "module.n"

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

  error e = pass(NULL, root, downsteps, upsteps, -1, &op);
  EXCEPT(e);

  return 0;
}

static error load_module(struct module **main_mod,
                         struct globalctx *gctx,
                         struct stage *stage,
                         const char *prefix, const char *fn) {
  struct module *mod = calloc(1, sizeof(struct module));

  error e = module_open(gctx, stage, mod, prefix, fn);
  EXCEPT(e);

  stage->loaded = realloc(stage->loaded,
                         (stage->loaded_count + 1) * sizeof(*stage->loaded));
  stage->loaded[stage->loaded_count] = mod->root;
  stage->loaded_count += 1;

  e = advance(mod, 0);
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

  fprintf(g_env.stderr, "After looking up in the directories:\n");
  for (size_t n = 0; prefixes[n] != NULL; ++n) {
    fprintf(g_env.stderr, "\t'%s'\n", prefixes[n]);
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

  e = load_module(NULL, mod->gctx, mod->stage, prefix, fn);
  free(fn);
  EXCEPT(e);

  return 0;
}

static error load_imports(struct stage *stage, struct node *node) {
  assert(node->which == MODULE);

  if (node->as.MODULE.is_placeholder) {
    return 0;
  }

  assert(node->as.MODULE.mod != NULL);
  error e = for_all_nodes(node->as.MODULE.mod->root, IMPORT, load_import,
                          stage->prefixes);
  EXCEPT(e);

  return 0;
}

HTABLE_SPARSE(modules_set, bool, struct module *);
implement_htable_sparse(__attribute__((unused)) static, modules_set, bool, struct module *);

static uint32_t module_pointer_hash(const struct module **mod) {
  return hash32_hsieh(mod, sizeof(*mod));
}

static int module_pointer_cmp(const struct module **a, const struct module **b) {
  return memcmp(a, b, sizeof(*a));
}

struct dependencies {
  struct modules_set added;
  struct module **tmp;
  size_t tmp_count;
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

  assert(nmod->which == MODULE);
  const bool already = modules_set_set(&deps->added, nmod->as.MODULE.mod, TRUE);
  if (already && !node_toplevel_const(node)->is_inline) {
    return 0;
  }

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

  struct module *mod = node_module_owner(node);
  PUSH_STATE(mod->state);
  PUSH_STATE(mod->state->step_state);
  error e = pass(mod, mod->body,
                 down, up, -1, deps);
  EXCEPT(e);
  POP_STATE(mod->state->step_state);
  POP_STATE(mod->state);

  return 0;
}

static error calculate_dependencies(struct dependencies *deps) {
  struct modules_set pushed;
  modules_set_init(&pushed, 0);
  modules_set_set_delete_val(&pushed, 0);
  modules_set_set_custom_hashf(&pushed, module_pointer_hash);
  modules_set_set_custom_cmpf(&pushed, module_pointer_cmp);

  for (ssize_t n = deps->tmp_count - 1; n >= 0; --n) {
    struct module *m = deps->tmp[n];
    int already = modules_set_set(&pushed, m, 1);
    if (already) {
      continue;
    }

    struct stage *stage = m->stage;
    stage->sorted_count += 1;
    stage->sorted = realloc(stage->sorted, stage->sorted_count * sizeof(*stage->sorted));
    stage->sorted[stage->sorted_count - 1] = m;
  }

  modules_set_destroy(&pushed);

  free(deps->tmp);
  deps->tmp = NULL;
  deps->tmp_count = 0;

  return 0;
}

error stage_load(struct stage *stage, const char *entry_point_fn) {
  struct globalctx gctx;
  globalctx_init(&gctx);

  PUSH_STATE(stage->state);

  error e = load_module(&stage->entry_point, &gctx, stage, NULL, entry_point_fn);
  EXCEPT(e);

  const char *prefixes[] = { "", "lib", NULL };
  stage->prefixes = prefixes;

  size_t processed = 0;
  while (processed < stage->loaded_count) {
    e = load_imports(stage, stage->loaded[processed]);
    EXCEPT(e);
    processed += 1;
  }

  struct dependencies deps = { 0 };
  deps.gctx = &gctx;
  modules_set_init(&deps.added, 0);
  modules_set_set_delete_val(&deps.added, FALSE);
  modules_set_set_custom_hashf(&deps.added, module_pointer_hash);
  modules_set_set_custom_cmpf(&deps.added, module_pointer_cmp);

  e = gather_dependencies(stage->entry_point->root, &deps);
  EXCEPT(e);

  e = calculate_dependencies(&deps);
  EXCEPT(e);

  size_t p;
  for (p = 1; passes[p].kind == PASS_FORWARD; ++p) {
    stage->state->passing = p;

    for (size_t n = 0; n < stage->sorted_count; ++n) {
      struct module *mod = stage->sorted[n];

      PUSH_STATE(mod->state->step_state);

      e = advance(mod, p);
      EXCEPT(e);

      POP_STATE(mod->state->step_state);
    }
  }

  for (size_t n = 0; n < stage->sorted_count; ++n) {
    struct module *mod = stage->sorted[n];

    PUSH_STATE(mod->state->step_state);

    for (size_t pp = p; passes[pp].kind == PASS_BODY; ++pp) {
      stage->state->passing = pp;

      e = advance(mod, pp);
      EXCEPT(e);
    }

    mod->done = TRUE;

    POP_STATE(mod->state->step_state);
  }

  return 0;
}
