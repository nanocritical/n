#include <unistd.h>

void nlang_postlude_abort() {
  abort();
}

void nlang_postlude___print(nlangcp__nlang_stringmod_string s) {
  const char *cstr = (const char *) nlang_stringmod_string_cstr(s);
  write(2, cstr, strlen(cstr));
}
