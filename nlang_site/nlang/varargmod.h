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
