#define NLANG_VARARGMOD_VARARG_ITERATOR_NEXT(self, type) ({ \
  bool __vararg_next_ret; \
  if ((self)->_v->_left == 0) { \
    __vararg_next_ret = 0; \
  } else { \
    (self)->_v->_current = va_arg((self)->_v->_ap, type); \
    (self)->_v->_left -= 1; \
    __vararg_next_ret = 1; \
  } \
  __vararg_next_ret; })
