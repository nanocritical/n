#include <sys/epoll.h>
#include <sys/signalfd.h>
#include <signal.h>

#ifdef NLANG_DEFINE_TYPES

// Must have the same layout as struct epoll_event.
struct n$syscall$Epoll_event {
  struct epoll_event sys;
};

struct n$syscall$Sigaction {
  struct sigaction act;
};

struct n$syscall$Sigset {
  sigset_t set;
};

struct n$syscall$Signalfd_siginfo {
  struct signalfd_siginfo ssi;
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

static inline n$builtins$U32 n$syscall$Signalfd_siginfo$Signo(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_signo; }
static inline n$builtins$I32 n$syscall$Signalfd_siginfo$Errno(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_errno; }
static inline n$builtins$I32 n$syscall$Signalfd_siginfo$Code(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_code; }
static inline n$builtins$U32 n$syscall$Signalfd_siginfo$Pid(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_pid; }
static inline n$builtins$U32 n$syscall$Signalfd_siginfo$Uid(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_uid; }
static inline n$builtins$I32 n$syscall$Signalfd_siginfo$Fd(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_fd; }
static inline n$builtins$U32 n$syscall$Signalfd_siginfo$Tid(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_tid; }
static inline n$builtins$U32 n$syscall$Signalfd_siginfo$Band(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_band; }
static inline n$builtins$U32 n$syscall$Signalfd_siginfo$Overrun(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_overrun; }
static inline n$builtins$U32 n$syscall$Signalfd_siginfo$Trapno(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_trapno; }
static inline n$builtins$I32 n$syscall$Signalfd_siginfo$Status(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_status; }
static inline n$builtins$I32 n$syscall$Signalfd_siginfo$Int(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_int; }
static inline n$builtins$U64 n$syscall$Signalfd_siginfo$Ptr(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_ptr; }
static inline n$builtins$U64 n$syscall$Signalfd_siginfo$Utime(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_utime; }
static inline n$builtins$U64 n$syscall$Signalfd_siginfo$Stime(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_stime; }
static inline n$builtins$U64 n$syscall$Signalfd_siginfo$Addr(struct n$syscall$Signalfd_siginfo *self) { return self->ssi.ssi_addr; }

#endif
