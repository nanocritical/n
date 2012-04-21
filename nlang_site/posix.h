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

i32 posix_errno(void) {
  return errno;
}

static const i32 posix__O_RDONLY = O_RDONLY;
static const i32 posix__O_WRONLY = O_WRONLY;
static const i32 posix__O_RDWR = O_RDWR;
static const i32 posix__O_APPEND = O_APPEND;
static const i32 posix__O_ASYNC = O_ASYNC;
static const i32 posix__O_CLOEXEC = O_CLOEXEC;
static const i32 posix__O_CREAT = O_CREAT;
static const i32 posix__O_DIRECT = O_DIRECT;
static const i32 posix__O_DIRECTORY = O_DIRECTORY;
static const i32 posix__O_EXCL = O_EXCL;
static const i32 posix__O_NOATIME = O_NOATIME;
static const i32 posix__O_NOCTTY = O_NOCTTY;
static const i32 posix__O_NOFOLLOW = O_NOFOLLOW;
static const i32 posix__O_NONBLOCK = O_NONBLOCK;
static const i32 posix__O_NDELAY = O_NDELAY;
static const i32 posix__O_SYNC = O_SYNC;
static const i32 posix__O_TRUNC = O_TRUNC;

i32 posix_open(const u8 *pathname, posix_open_flags flags, const posix_open_mode *mode) {
  mode_t m = 0;
  if (mode != null) {
    m = posix_open_mode_to_bits(mode);
  }

  return open((const char *) pathname, posix_open_flags_to_bits(&flags), m);
}

i32 posix_close(i32 fd) {
  return close(fd);
}

i32 posix_unlink(const u8 *pathname) {
  return unlink((const char *) pathname);
}

ssize posix_read(i32 fd, u8 *buf, size count) {
  return read(fd, buf, count);
}

ssize posix_write(i32 fd, const u8 *buf, size count) {
  return write(fd, buf, count);
}

i32 posix_rand(void) {
  return rand();
}

void posix_srand(u32 seed) {
  srand(seed);
}
