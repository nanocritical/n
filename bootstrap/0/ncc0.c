#include "parser.h"
#include "printer.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int main(int argc, char **argv) {
  for (int i = 1; i < argc; ++i) {
    struct module mod;
    error e = module_open(&mod, argv[i]);
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
