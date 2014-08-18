#include "common.h"

#include "parser.h"
#include "stage.h"

#include <stdarg.h>
#include <sys/time.h>

void __break(void) {
  static volatile int dummy;
  dummy += 1;
}

EXAMPLE(log2_ceil) {
  assert(log2_ceil(0) == 0);
  assert(log2_ceil(1) == 0);
  assert(log2_ceil(2) == 1);
  assert(log2_ceil(4) == 2);
  assert(log2_ceil(0xfff) == 12);
  assert(log2_ceil(0x1000) == 12);
  assert(log2_ceil(0x7ff) == 11);
  assert(log2_ceil(0x800) == 11);
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

    if (n < 0) {
      return strdup(".");
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
  assert(strcmp(xdirname(""), ".") == 0);
  assert(strcmp(xdirname("/"), "/") == 0);
  assert(strcmp(xdirname("///"), "/") == 0);
  assert(strcmp(xdirname("a"), ".") == 0);
  assert(strcmp(xdirname("a/b"), "a") == 0);
  assert(strcmp(xdirname("a/b/c/d"), "a/b/c") == 0);
  assert(strcmp(xdirname("a/b/c/"), "a/b/c") == 0);
  assert(strcmp(xdirname("a/b/c//"), "a/b/c") == 0);
}

char *xbasename(const char *s) {
  assert(s != NULL);

  const size_t len = strlen(s);
  ssize_t n;
  for (n = len - 1; n >= 0; --n) {
    if (s[n] == '/') {
      break;
    }
  }

  if (n < 0) {
    return strdup(s);
  } else {
    char *r = calloc(len - n, sizeof(char));
    memcpy(r, s + n + 1, len - n);
    return r;
  }
}

EXAMPLE(xbasename) {
  assert(strcmp(xbasename(""), "") == 0);
  assert(strcmp(xbasename("/"), "") == 0);
  assert(strcmp(xbasename("///"), "") == 0);
  assert(strcmp(xbasename("a"), "a") == 0);
  assert(strcmp(xbasename("a/b"), "b") == 0);
  assert(strcmp(xbasename("a/b/c/def"), "def") == 0);
  assert(strcmp(xbasename("a/b/c/"), "") == 0);
  assert(strcmp(xbasename("a/b/c//"), "") == 0);
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

  g_env.running_example = true;
}

void examples_destroy(const char *name) {
  if (ftell(g_env.stderr) != 0) {
    fflush(g_env.stderr);
    fprintf(stderr, "Example '%s' non-empty stderr:\n%s\n", name, g_env.stderr_mem);
    assert(false);
  }

  fclose(g_env.stderr);
  g_env.stderr = NULL;

  g_env.running_example = false;
}

void should_fail(error e) {
  assert(e);
  fflush(g_env.stderr);
  rewind(g_env.stderr);
  memset(g_env.stderr_mem, 0, ENV_BUF_SIZE * sizeof(*g_env.stderr_mem));
}

void should_fail_with(error e, const char *err) {
  assert(e);
  fflush(g_env.stderr);
  assert(strcmp(g_env.stderr_mem, err) == 0);
  rewind(g_env.stderr);
  memset(g_env.stderr_mem, 0, ENV_BUF_SIZE * sizeof(*g_env.stderr_mem));
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

enum spec_move {
  DOWN,
  UP,
  NEXT,
};

static void init_read_spec_map(struct idents_map *map) {
  if (idents_map_count(map) != 0) {
    return;
  }

  idents_map_init(map, 100);
  idents_map_set_delete_val(map, -1);

  for (size_t n = 1; n < NODE__NUM; ++n) {
    struct token tok = {
      .t = IDENT,
      .value = node_which_strings[n],
      .len = strlen(node_which_strings[n]),
    };

    idents_map_set(map, tok, (ident) n);
  }
}

static enum node_which read_spec(enum spec_move *move, size_t *repeat,
                                 size_t *indent, const char *s) {
  static __thread struct idents_map map;
  init_read_spec_map(&map);

  size_t whites = 0;
  while (s[0] == ' ') {
    whites += 1;
    s += 1;
  }

  ssize_t d = whites - *indent;
  if (d == +1) {
    *move = DOWN;
    *repeat = 1;
  } else if (d == 0) {
    *move = NEXT;
    *repeat = 1;
  } else if (d < 0) {
    *move = UP;
    *repeat = - d;
  }

  *indent = whites;

  const char *end = s;
  while (end[0] != '\0') {
    end += 1;
  }

  struct token tok = { .t = IDENT, .value = s, .len = end - s, };
  ident *code = idents_map_get(&map, tok);
  assert(code != NULL);
  return (enum node_which) *code;
}

void check_structure(struct node *node, ...) {
  va_list ap;
  va_start(ap, node);

  struct node *n = node;
  size_t indent = 0;
  bool first = true;

  while (true) {
    const char *s = va_arg(ap, const char *);
    if (s == NULL) {
      break;
    }

    enum spec_move move = 0;
    size_t repeat = 0;
    enum node_which which = read_spec(&move, &repeat, &indent, s);

    if (!first) {
      switch (move) {
      case DOWN:
        n = subs_first(n);
        break;
      case UP:
        while (repeat > 0) {
          n = parent(n);
          repeat -= 1;
        }
        n = next(n);
        break;
      case NEXT:
        n = next(n);
        break;
      }
    }
    first = false;

    assert(n != NULL && which == n->which && "Failed structure check");
  }

  va_end(ap);
}

EXAMPLE_NCC_EMPTY(check_structure) {
  struct node *let = mk_node(mod, mod->body, LET);
  check_structure(let, "LET", NULL);

  struct node *defn = mk_node(mod, let, DEFNAME);
  check_structure(defn, "DEFNAME", NULL);
  check_structure(let,
                  "LET",
                  " DEFNAME", NULL);

  struct node *ident = mk_node(mod, defn, IDENT);
  check_structure(ident, "IDENT", NULL);
  check_structure(defn,
                  "DEFNAME",
                  " IDENT", NULL);
  check_structure(let,
                  "LET",
                  " DEFNAME",
                  "  IDENT", NULL);

  struct node *deft = mk_node(mod, mod->body, DEFTYPE);
  check_structure(deft, "DEFTYPE", NULL);

  check_structure(ident, "IDENT", NULL);
  check_structure(defn,
                  "DEFNAME",
                  " IDENT", NULL);
  check_structure(let,
                  "LET",
                  " DEFNAME",
                  "  IDENT", NULL);

  check_structure(mod->body,
                  "MODULE_BODY",
                  " LET",
                  "  DEFNAME",
                  "   IDENT",
                  " DEFTYPE", NULL);
}

double time(void) {
  struct timeval tv = { 0 };
  int ret = gettimeofday(&tv, NULL);
  assert(ret == 0);
  return tv.tv_sec + tv.tv_usec * 1e-6;
}

struct timeit timeits[TIMEIT__NUM];

static const char *timeits_name[TIMEIT__NUM] = {
  [TIMEIT_MAIN] = "main",
  [TIMEIT_PARSER] = "parser",
  [TIMEIT_PRE_PASSBODY] = "pre_passbody",
  [TIMEIT_PASSBODY] = "passbody",
  [TIMEIT_PASSSEM] = "passsem",
  [TIMEIT_CREATE_INSTANCE_DEEPCOPY] = "create_instance_deepcopy",
  [TIMEIT_INSTANTIATE_DEEPCOPY] = "instantiate_deepcopy",
  [TIMEIT_INSTANTIATE_TOTAL] = "instantiate_total",
  [TIMEIT_INSTANTIATE] = "instantiate",
  [TIMEIT_INSTANTIATE_INTF] = "instantiate_intf",
  [TIMEIT_INSTANTIATE_REF] = "instantiate_ref",
  [TIMEIT_INSTANTIATE_TENTATIVE] = "instantiate_tentative",
  [TIMEIT_INSTANTIATE_TENTATIVE_INTF] = "instantiate_tentative_intf",
  [TIMEIT_INSTANTIATE_TENTATIVE_REF] = "instantiate_tentative_ref",
  [TIMEIT_INSTANTIATE_TENTATIVE_WEAKLY_CONCRETE] = "instantiate_tentative_weakly_concrete",
  [TIMEIT_TYPE_INFERENCE] = "type_inference",
  [TIMEIT_TYPE_INFERENCE_PREBODYPASS] = "type_inference_prebodypass",
  [TIMEIT_TYPE_INFERENCE_IN_FUNS_BLOCK] = "type_inference_in_funs_block",
  [TIMEIT_UNIFY] = "unify",
  [TIMEIT_GENERATE] = "generate",
  [TIMEIT_GENERATE_C] = "generate_c",
};

void timeit_print(FILE *out) {
  for (size_t n = 0; n < TIMEIT__NUM; ++n) {
    fprintf(out, "%8zu\t%.3f\t%s\n", timeits[n].count, timeits[n].time, timeits_name[n]);
  }
};
