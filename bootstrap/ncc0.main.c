#include "parser.h"
#include "stage.h"
#include "printer.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

#define CC "gcc"
//#define CC "clang"
#define CFLAGS "-Wall -Wno-missing-braces -ffunction-sections -fdata-sections -std=c99 -I. -g"
#define LDFLAGS CFLAGS " -Wl,--gc-sections"

static error sh(const char *cmd) {
  int status = system(cmd);
  if (status == -1) {
    THROWF(errno, "system(3) failed");
  }
  if (WIFSIGNALED(status)) {
    THROWF(ECHILD, "command terminated by signal %d: %s", WTERMSIG(status), cmd);
  } else if (WEXITSTATUS(status) != 0) {
    THROWF(ECHILD, "command exited with %d: %s", WEXITSTATUS(status), cmd);
  }

  return 0;
}

static char *o_filename(const char *filename) {
  char *o_fn = calloc(strlen(filename) + sizeof(".o"), sizeof(char));
  sprintf(o_fn, "%s.o", filename);
  return o_fn;
}

static error cc(const struct module *mod, const char *o_fn,
                const char *c_fn) {
  static const char *fmt = CC " " CFLAGS " -xc %s -c -o %s";
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
  static const char fmt[] = CC " " LDFLAGS " %s %s -o %s";
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
    THROWF(errno, "Cannot open output file '%s'", out_fn);
  }
  free(out_fn);

  e = printer_tree(fd, mod, NULL);
  EXCEPT(e);
  close(fd);

  out_fn = calloc(strlen(fn) + sizeof(".pretty.out"), sizeof(char));
  sprintf(out_fn, "%s.pretty.out", fn);

  fd = creat(out_fn, 00600);
  if (fd < 0) {
    THROWF(errno, "Cannot open output file '%s'", out_fn);
  }
  free(out_fn);

  e = printer_pretty(fd, mod);
  EXCEPT(e);
  close(fd);

  char *c_fn = calloc(strlen(fn) + sizeof(".c.out"), sizeof(char));
  sprintf(c_fn, "%s.c.out", fn);

  fd = creat(c_fn, 00600);
  if (fd < 0) {
    THROWF(errno, "Cannot open output file '%s'", c_fn);
  }

  e = printer_c(fd, mod);
  EXCEPT(e);
  close(fd);

  char *h_fn = calloc(strlen(fn) + sizeof(".h.out"), sizeof(char));
  sprintf(h_fn, "%s.h.out", fn);

  fd = creat(h_fn, 00600);
  if (fd < 0) {
    THROWF(errno, "Cannot open output file '%s'", h_fn);
  }

  e = printer_h(fd, mod);
  EXCEPT(e);
  close(fd);

  free(c_fn);
  free(h_fn);

  return 0;
}

static error compile(struct node *node) {
  assert(node->which == MODULE);
  struct module *mod = node->as.MODULE.mod;

  const char *fn = mod->filename;
  error e;

  char *c_fn = calloc(strlen(fn) + sizeof(".c.out"), sizeof(char));
  sprintf(c_fn, "%s.c.out", fn);

  char *o_fn = o_filename(mod->filename);
  e = cc(mod, o_fn, c_fn);
  EXCEPT(e);

  free(o_fn);
  free(c_fn);

  return 0;
}

static error run_examples(const struct stage *stage) {
  static const char *out_fn = "a.out.examples";
  static const char *main_fn = "a.out.examples.c";

  FILE *run = fopen(main_fn, "w");
  if (run == NULL) {
    THROWF(errno, "Cannot open output file '%s'", main_fn);
  }

  for (size_t n = 0; n < stage->sorted_count; ++n) {
    const struct module *mod = stage->sorted[n];
    fprintf(run, "void ");
    print_c_runexamples_name(run, mod);
    fprintf(run, "(void);\n");
  }

  fprintf(run, "int main(void) {\n");
  for (size_t n = 0; n < stage->sorted_count; ++n) {
    const struct module *mod = stage->sorted[n];
    print_c_runexamples_name(run, mod);
    fprintf(run, "();\n");
  }
    fprintf(run, "}\n");
  fclose(run);

  char *inputs = file_list((const struct module **)stage->sorted,
                           stage->sorted_count, o_filename);
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

static error program_link(const struct stage *stage) {
  const char *out_fn = "a.out";
  const char *main_fn = "a.out.c";

  FILE *run = fopen(main_fn, "w");
  if (run == NULL) {
    THROWF(errno, "Cannot open output file '%s'", main_fn);
  }
  fprintf(run, "int _Nmain();\n");
  fprintf(run, "int main(int argc, char **argv, char **env) {\nreturn _Nmain(argc, argv, env);\n}\n");
  fclose(run);

  char *inputs = file_list((const struct module **)stage->sorted,
                           stage->sorted_count, o_filename);
  error e = clink(out_fn, inputs, main_fn);
  free(inputs);
  EXCEPT(e);

  return 0;
}

int main(int argc, char **argv) {
  env_init();

  if (argc != 2) {
    fprintf(g_env.stderr, "Usage: %s <main.n>\n", argv[0]);
    exit(1);
  }

  bool opt_compile = TRUE;
  if (getenv("NCC_GEN_ONLY")) {
    opt_compile = FALSE;
  }

  struct globalctx gctx;
  globalctx_init(&gctx);

  struct stage stage = { 0 };
  error e = stage_load(&gctx, &stage, argv[1]);
  EXCEPT(e);

  for (size_t n = 0; n < stage.sorted_count; ++n) {
    const struct module *mod = stage.sorted[n];

    e = generate(mod->root);
    EXCEPT(e);
  }

  if (!opt_compile) {
    return 0;
  }

  for (size_t n = 0; n < stage.sorted_count; ++n) {
    const struct module *mod = stage.sorted[n];

    e = compile(mod->root);
    EXCEPT(e);
  }

  e = run_examples(&stage);
  EXCEPT(e);

  e = program_link(&stage);
  EXCEPT(e);

  return 0;
}
