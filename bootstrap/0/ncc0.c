#include "parser.h"
#include "printer.h"
#include "firstpass.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int main(int argc, char **argv) {
  for (int i = 1; i < argc; ++i) {
    struct module mod;
    error e = module_open(&mod, argv[i]);
    EXCEPT(e);

    step zeropass_down[] = {
      NULL,
    };
    step zeropass_up[] = {
      step_add_scopes,
      NULL,
    };

    step firstpass_down[] = {
      step_lexical_scoping,
      step_add_builtins,
      step_add_implicit_variables,
      NULL,
    };
    step firstpass_up[] = {
      step_type_destructuring,
      step_type_inference,
      step_operator_call_inference,
      step_unary_call_inference,
      step_ctor_call_inference,
      step_call_arguments_prepare,
      step_temporary_inference,
      step_validation,
      NULL,
    };

    e = pass(&mod, NULL, zeropass_down, zeropass_up);
    EXCEPT(e);

    e = pass(&mod, NULL, firstpass_down, firstpass_up);
    EXCEPT(e);

    char *out_fn = malloc(strlen(argv[i]) + 4 + 1);
    sprintf(out_fn, "%s.out", argv[i]);

    int fd = creat(out_fn, 00600);
    if (fd < 0) {
      EXCEPTF(errno, "Cannot open output file '%s'", out_fn);
    }
    free(out_fn);

    printer_pretty(fd, &mod);
  }
}
