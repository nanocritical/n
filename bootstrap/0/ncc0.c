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

int main(int argc, char **argv) {
  for (int i = 1; i < argc; ++i) {
    struct module mod;
    error e = module_open(&mod, argv[i]);
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

    e = pass(&mod, NULL, zeropass_down, zeropass_up);
    EXCEPT(e);

    e = pass(&mod, NULL, firstpass_down, firstpass_up);
    EXCEPT(e);

    char *out_fn = malloc(strlen(argv[i]) + sizeof(".tree.out"));
    sprintf(out_fn, "%s.tree.out", argv[i]);

    int fd = creat(out_fn, 00600);
    if (fd < 0) {
      EXCEPTF(errno, "Cannot open output file '%s'", out_fn);
    }
    free(out_fn);

    e = printer_tree(fd, &mod, NULL);
    EXCEPT(e);
    close(fd);

    out_fn = malloc(strlen(argv[i]) + sizeof(".pretty.out"));
    sprintf(out_fn, "%s.pretty.out", argv[i]);

    fd = creat(out_fn, 00600);
    if (fd < 0) {
      EXCEPTF(errno, "Cannot open output file '%s'", out_fn);
    }
    free(out_fn);

    e = printer_pretty(fd, &mod);
    EXCEPT(e);
    close(fd);

    char *c_fn = malloc(strlen(argv[i]) + sizeof(".c.out"));
    sprintf(c_fn, "%s.c.out", argv[i]);

    fd = creat(c_fn, 00600);
    if (fd < 0) {
      EXCEPTF(errno, "Cannot open output file '%s'", c_fn);
    }

    e = printer_c(fd, &mod);
    EXCEPT(e);
    close(fd);

    char *h_fn = malloc(strlen(argv[i]) + sizeof(".h.out"));
    sprintf(h_fn, "%s.h.out", argv[i]);

    fd = creat(h_fn, 00600);
    if (fd < 0) {
      EXCEPTF(errno, "Cannot open output file '%s'", h_fn);
    }

    e = printer_h(fd, &mod);
    EXCEPT(e);
    close(fd);

    e = cc(&mod, c_fn, h_fn);
    EXCEPT(e);

    free(c_fn);
    free(h_fn);
  }

  return 0;
}
