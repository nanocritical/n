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

struct opt {
  bool verbose;
  bool compile;
  const char *compiler;
  const char *cflags;
};

static struct opt g_opt;

#define CFLAGS "-Wall -Wno-missing-braces -ffunction-sections -fdata-sections -std=c99 -I. -g"
#define LDFLAGS CFLAGS " -Wl,--gc-sections"

static ERROR sh(const char *cmd) {
  if (g_opt.verbose) {
    fprintf(stdout, "%s\n", cmd);
  }

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

static ERROR clang_cleanup(const char *c_fn) {
  static const char *fmt = "perl -i -e 'undef $/; $_=<>; s/(;[\\s\\n]*)*;/;/mg; print' %s";
  char *cmd = calloc(strlen(fmt) + strlen(c_fn) + 1, sizeof(char));
  sprintf(cmd, fmt, c_fn);

  error e = sh(cmd);
  free(cmd);
  EXCEPT(e);
  return 0;
}

static ERROR cc(const char *o_fn, const char *c_fn) {
  if (strcmp(g_opt.compiler, "clang") == 0 || strcmp(g_opt.compiler, "emcc") == 0) {
    error e = clang_cleanup(c_fn);
    EXCEPT(e);
  }

  static const char *fmt = "%s " CFLAGS " %s -xc %s -c -o %s";
  char *cmd = calloc(strlen(fmt) + strlen(g_opt.compiler) + strlen(g_opt.cflags)
                     + strlen(c_fn) + strlen(o_fn) - 5 + 1, sizeof(char));
  sprintf(cmd, fmt, g_opt.compiler, g_opt.cflags, c_fn, o_fn);

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

static ERROR clink(const char *out_fn, const char *inputs, const char *extra) {
  static const char fmt[] = "%s " LDFLAGS " %s %s %s -o %s%s";
  const char *ext = "";
  if (strcmp(g_opt.compiler, "emcc") == 0) {
    ext = ".js";
  }
  size_t len = strlen(fmt) + strlen(g_opt.compiler) + strlen(g_opt.cflags)
    + strlen(inputs) + strlen(extra) + strlen(out_fn) + strlen(ext) - 7;
  char *cmd = calloc(len + 1, sizeof(char));
  sprintf(cmd, fmt, g_opt.compiler, g_opt.cflags, inputs, extra, out_fn, ext);

  error e = sh(cmd);
  free(cmd);
  EXCEPT(e);
  return 0;
}

static ERROR generate(struct node *node) {
  BEGTIMEIT(TIMEIT_GENERATE);

  assert(node->which == MODULE);
  struct module *mod = node->as.MODULE.mod;

  const char *fn = mod->filename;
  error e;

  char *out_fn = calloc(strlen(fn) + sizeof(".o.tree"), sizeof(char));
  sprintf(out_fn, "%s.o.tree", fn);

  int fd = creat(out_fn, 00600);
  if (fd < 0) {
    THROWF(errno, "Cannot open output file '%s'", out_fn);
  }
  free(out_fn);

  e = pptree(fd, mod, NULL);
  EXCEPT(e);
  close(fd);

  out_fn = calloc(strlen(fn) + sizeof(".o.pretty"), sizeof(char));
  sprintf(out_fn, "%s.o.pretty", fn);

  fd = creat(out_fn, 00600);
  if (fd < 0) {
    THROWF(errno, "Cannot open output file '%s'", out_fn);
  }
  free(out_fn);

  e = printer_pretty(fd, mod);
  EXCEPT(e);
  close(fd);

  char *c_fn = calloc(strlen(fn) + sizeof(".o.c"), sizeof(char));
  sprintf(c_fn, "%s.o.c", fn);

  fd = creat(c_fn, 00600);
  if (fd < 0) {
    THROWF(errno, "Cannot open output file '%s'", c_fn);
  }

  BEGTIMEIT(TIMEIT_GENERATE_C);
  e = printer_c(fd, mod);
  EXCEPT(e);
  ENDTIMEIT(true, TIMEIT_GENERATE_C);

  close(fd);

  char *h_fn = calloc(strlen(fn) + sizeof(".o.h"), sizeof(char));
  sprintf(h_fn, "%s.o.h", fn);

  fd = creat(h_fn, 00600);
  if (fd < 0) {
    THROWF(errno, "Cannot open output file '%s'", h_fn);
  }

  e = printer_h(fd, mod);
  EXCEPT(e);
  close(fd);

  free(c_fn);
  free(h_fn);

  ENDTIMEIT(true, TIMEIT_GENERATE);
  return 0;
}

static ERROR compile(struct node *node) {
  assert(node->which == MODULE);
  struct module *mod = node->as.MODULE.mod;

  const char *fn = mod->filename;
  error e;

  char *c_fn = calloc(strlen(fn) + sizeof(".o.c"), sizeof(char));
  sprintf(c_fn, "%s.o.c", fn);

  char *o_fn = o_filename(mod->filename);
  e = cc(o_fn, c_fn);
  EXCEPT(e);

  free(o_fn);
  free(c_fn);

  return 0;
}

static ERROR run_examples(const struct stage *stage) {
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

  fprintf(run, "void _$Nprelude(int *argc, char ***argv, char ***env);\n"
          "void _$Npostlude(int *ret);\n");
  fprintf(run, "int main(int argc, char **argv, char **env) {\n"
          "_$Nprelude(&argc, &argv, &env);\n");
  for (size_t n = 0; n < stage->sorted_count; ++n) {
    const struct module *mod = stage->sorted[n];
    print_c_runexamples_name(run, mod);
    fprintf(run, "();\n");
  }
  fprintf(run, "int ret = 0;\n"
          "_$Npostlude(&ret);\n"
          "return ret;\n"
          "}\n");
  fclose(run);

  char *inputs = file_list((const struct module **)stage->sorted,
                           stage->sorted_count, o_filename);
  error e = clink(out_fn, inputs, main_fn);
  free(inputs);
  EXCEPT(e);

  static const char *fmt = "%s ./%s%s";
  const char *runner = "";
  const char *ext = "";
  if (strcmp(g_opt.compiler, "emcc") == 0) {
    runner = "d8";
    ext = ".js";
  }
  char *cmd = calloc(strlen(fmt) + strlen(runner) + strlen(main_fn) + strlen(ext) - 4 + 1, sizeof(char));
  sprintf(cmd, fmt, runner, out_fn, ext);
  e = sh(cmd);
  free(cmd);
  EXCEPTF(e, "examples failed");

  return 0;
}

static ERROR program_link(const struct stage *stage) {
  const char *out_fn = "a.out";
  const char *main_fn = "a.out.c";

  FILE *run = fopen(main_fn, "w");
  if (run == NULL) {
    THROWF(errno, "Cannot open output file '%s'", main_fn);
  }
  fprintf(run, "int _$Nmain(void);\n"
          "void _$Nprelude(int *argc, char ***argv, char ***env);\n"
          "void _$Npostlude(int *ret);\n");
  fprintf(run, "int main(int argc, char **argv, char **env) {\n"
          "_$Nprelude(&argc, &argv, &env);\n"
          "int ret = _$Nmain();\n"
          "_$Npostlude(&ret);\n"
          "return ret;\n"
          "}\n");
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

  BEGTIMEIT(TIMEIT_MAIN);

  if (argc != 2) {
    fprintf(g_env.stderr, "Usage: %s <main.n>\n", argv[0]);
    exit(1);
  }

  g_opt.verbose = false;
  g_opt.compile = true;
  g_opt.compiler = "gcc";
  g_opt.cflags = "";

  if (getenv("NCC_VERBOSE")) {
    g_opt.verbose = atoi(getenv("NCC_VERBOSE")) != 0;
  }
  if (getenv("NCC_COMPILE")) {
    g_opt.compile = atoi(getenv("NCC_COMPILE")) != 0;
  }
  if (getenv("NCC_COMPILER")) {
    g_opt.compiler = strdup(getenv("NCC_COMPILER"));
  }
  if (getenv("NCC_CFLAGS")) {
    g_opt.cflags = strdup(getenv("NCC_CFLAGS"));
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

  ENDTIMEIT(true, TIMEIT_MAIN);

  if (getenv("NCC_TIMEIT")) {
    timeit_print(g_env.stderr);
    fprintf(g_env.stderr, "\n"
            "%.0f k nodes (approx)\n"
            "%.0f k nodes / sec (approx)\n",
            timeits[TIMEIT_TYPE_INFERENCE].count * .001,
            timeits[TIMEIT_TYPE_INFERENCE].count * .001 / timeits[TIMEIT_MAIN].time);
  }

  if (!g_opt.compile) {
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
