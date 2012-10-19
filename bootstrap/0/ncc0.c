#include "parser.h"

int main(int argc, char **argv) {
  for (int i = 1; i < argc; ++i) {
    struct module mod;
    error e = module_open(&mod, argv[i]);
    EXCEPT(e);
  }
}
