#define NLANG_VARARGMOD_VARARG_NEXT(self, type) ({ \
  bool __vararg_next_ret; \
  if ((self)->_left == 0) { \
    __vararg_next_ret = 0; \
  } else { \
    (self)->_current = va_arg((self)->_ap, type); \
    (self)->_left -= 1; \
    __vararg_next_ret = 1; \
  } \
  __vararg_next_ret; })


void nlang_varargmod___error_reset(void) {
  const char *cstr = "FATAL: Cannot iterate more than once in a vararg\n";
  write(2, cstr, strlen(cstr));
  abort();
}
