#include <sys/epoll.h>
#include <signal.h>

#ifdef NLANG_DEFINE_TYPES

// Must have the same layout as struct epoll_event.
struct n$syscall$Epoll_event {
  struct epoll_event sys;
};

struct n$syscall$Sigaction {
  struct sigaction act;
};

#endif

#ifdef NLANG_DEFINE_FUNCTIONS

static inline void n$syscall$Epoll_event$Mk(n$builtins$U32 events, n$builtins$U8 *rawdata,
                                            struct n$syscall$Epoll_event *r) {
  r->sys.events = events;
  r->sys.data.ptr = rawdata;
}

static inline n$builtins$U32 n$syscall$Epoll_event$Events(struct n$syscall$Epoll_event *self) {
  return self->sys.events;
}

static inline n$builtins$U8 *n$syscall$Epoll_event$Rawdata(struct n$syscall$Epoll_event *self) {
  return self->sys.data.ptr;
}

#endif
