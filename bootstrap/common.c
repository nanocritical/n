#include "common.h"

#include "parser.h"
#include "stage.h"

void __break(void) {
  static volatile int dummy;
  dummy += 1;
}

char *strdup(const char *s) {
  char *r = calloc(strlen(s) + 1, sizeof(char));
  strcpy(r, s);
  return r;
}

EXAMPLE(strdup) {
  assert(strcmp(strdup("abc"), "abc") == 0);
}

char *xdirname(const char *s) {
  if (s == NULL) {
    return strdup(".");
  } else if (strcmp(s, "/") == 0) {
    return strdup("/");
  } else {
    const size_t len = strlen(s);
    ssize_t n;
    for (n = len - 1; n >= 0; --n) {
      if (s[n] == '/') {
        break;
      }
    }

    while (n > 1 && s[n-1] == '/') {
      n -= 1;
    }

    if (n < 0) {
      return strdup(s);
    } else {
      char *r = calloc(n + 1, sizeof(char));
      memcpy(r, s, n);
      return r;
    }
  }
}

EXAMPLE(xdirname) {
  assert(strcmp(xdirname(NULL), ".") == 0);
  assert(strcmp(xdirname(""), "") == 0);
  assert(strcmp(xdirname("/"), "/") == 0);
  assert(strcmp(xdirname("///"), "/") == 0);
  assert(strcmp(xdirname("a"), "a") == 0);
  assert(strcmp(xdirname("a/b"), "a") == 0);
  assert(strcmp(xdirname("a/b/c/d"), "a/b/c") == 0);
  assert(strcmp(xdirname("a/b/c/"), "a/b/c") == 0);
  assert(strcmp(xdirname("a/b/c//"), "a/b/c") == 0);
}

void env_init(void) {
  g_env.stderr = stderr;
}

void examples_init(const char *name) {
  if (g_env.stderr_mem == NULL) {
    g_env.stderr_mem = calloc(ENV_BUF_SIZE, sizeof(*g_env.stderr_mem));
  } else {
    memset(g_env.stderr_mem, 0, ENV_BUF_SIZE * sizeof(*g_env.stderr_mem));
  }

  g_env.stderr = fmemopen(g_env.stderr_mem, ENV_BUF_SIZE * sizeof(*g_env.stderr_mem), "w");
}

void examples_destroy(const char *name) {
  if (ftell(g_env.stderr) != 0) {
    fflush(g_env.stderr);
    fprintf(stderr, "Example '%s' non-empty stderr:\n%s\n", name, g_env.stderr_mem);
    assert(FALSE);
  }

  fclose(g_env.stderr);
  g_env.stderr = NULL;
}

static void module_prepare_empty_mocks(struct module **mod) {
  struct globalctx *gctx = calloc(1, sizeof(struct globalctx));
  globalctx_init(gctx);

  struct stage *stage = calloc(1, sizeof(struct stage));
  error e = stage_load(gctx, stage, "bootstrap/mockempty.n");
  assert(!e);

  *mod = stage->entry_point;
}

void examples_init_NCC_EMPTY(const char *name, struct module **mod) {
  examples_init(name);

  module_prepare_empty_mocks(mod);
}

void examples_destroy_NCC_EMPTY(const char *name, struct module **mod) {
  examples_destroy(name);
}

static void module_prepare_mocks(struct module **mod) {
  struct globalctx *gctx = calloc(1, sizeof(struct globalctx));
  globalctx_init(gctx);

  struct stage *stage = calloc(1, sizeof(struct stage));
  error e = stage_load(gctx, stage, "bootstrap/mockbasic.n");
  if (e && g_env.stderr != stderr) {
    fflush(g_env.stderr);
    fprintf(stderr, "%s", g_env.stderr_mem);
  }
  assert(!e);

  *mod = stage->entry_point;
}

void examples_init_NCC(const char *name, struct module **mod) {
  examples_init(name);

  module_prepare_mocks(mod);
}

void examples_destroy_NCC(const char *name, struct module **mod) {
  examples_destroy(name);
}

uint32_t g_invariants_counter = 0;
