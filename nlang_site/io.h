#include <sys/types.h>
#include <sys/stat.h>
#ifndef __USE_XOPEN2K8
# define __USE_XOPEN2K8
#endif
#ifndef __USE_GNU
# define __USE_GNU
#endif
#include <fcntl.h>
#include <errno.h>

bool io_file_mode_isreg(nlangcp__io_file_mode self) {
  return S_ISREG((mode_t) self->_bits);
}

bool io_file_mode_isdir(nlangcp__io_file_mode self) {
  return S_ISDIR((mode_t) self->_bits);
}

bool io_file_mode_ischr(nlangcp__io_file_mode self) {
  return S_ISCHR((mode_t) self->_bits);
}

bool io_file_mode_isblk(nlangcp__io_file_mode self) {
  return S_ISBLK((mode_t) self->_bits);
}

bool io_file_mode_isfifo(nlangcp__io_file_mode self) {
  return S_ISFIFO((mode_t) self->_bits);
}

bool io_file_mode_islnk(nlangcp__io_file_mode self) {
  return S_ISLNK((mode_t) self->_bits);
}

bool io_file_mode_issock(nlangcp__io_file_mode self) {
  return S_ISSOCK((mode_t) self->_bits);
}


static const i32 io__O_RDONLY = O_RDONLY;
static const i32 io__O_WRONLY = O_WRONLY;
static const i32 io__O_RDWR = O_RDWR;
static const i32 io__O_APPEND = O_APPEND;
static const i32 io__O_ASYNC = O_ASYNC;
static const i32 io__O_CLOEXEC = O_CLOEXEC;
static const i32 io__O_CREAT = O_CREAT;
static const i32 io__O_DIRECT = O_DIRECT;
static const i32 io__O_DIRECTORY = O_DIRECTORY;
static const i32 io__O_EXCL = O_EXCL;
static const i32 io__O_NOATIME = O_NOATIME;
static const i32 io__O_NOCTTY = O_NOCTTY;
static const i32 io__O_NOFOLLOW = O_NOFOLLOW;
static const i32 io__O_NONBLOCK = O_NONBLOCK;
static const i32 io__O_NDELAY = O_NDELAY;
static const i32 io__O_SYNC = O_SYNC;
static const i32 io__O_TRUNC = O_TRUNC;
