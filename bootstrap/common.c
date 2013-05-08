#include "common.h"

void __break(void) {
  volatile int dummy;
  dummy += 1;
}
