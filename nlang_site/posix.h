#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#ifndef __USE_XOPEN2K8
# define __USE_XOPEN2K8
#endif
#ifndef __USE_GNU
# define __USE_GNU
#endif
#include <fcntl.h>
#include <errno.h>
#include <time.h>


#define SL8(f) nlangapp__nlang_slicemod_slice__u8##f
#define NERROR_OK nlang_errormod_nerror_OK_mk()
#define NERROR(eno) nlang_errormod_nerror_from_errno(eno)
#define TUP2(ta, tb) nlangtuple__##ta##__##tb
#define TUP2VAL(ta, tb, va, vb) (nlangtuple__##ta##__##tb){ va, vb }


static const i32 posix_O_RDONLY = O_RDONLY;
static const i32 posix_O_WRONLY = O_WRONLY;
static const i32 posix_O_RDWR = O_RDWR;
static const i32 posix_O_APPEND = O_APPEND;
static const i32 posix_O_ASYNC = O_ASYNC;
static const i32 posix_O_CLOEXEC = O_CLOEXEC;
static const i32 posix_O_CREAT = O_CREAT;
static const i32 posix_O_DIRECT = O_DIRECT;
static const i32 posix_O_DIRECTORY = O_DIRECTORY;
static const i32 posix_O_EXCL = O_EXCL;
static const i32 posix_O_NOATIME = O_NOATIME;
static const i32 posix_O_NOCTTY = O_NOCTTY;
static const i32 posix_O_NOFOLLOW = O_NOFOLLOW;
static const i32 posix_O_NONBLOCK = O_NONBLOCK;
static const i32 posix_O_NDELAY = O_NDELAY;
static const i32 posix_O_SYNC = O_SYNC;
static const i32 posix_O_TRUNC = O_TRUNC;


i32 posix___errno(void) {
  return errno;
}


u64 posix_stat_t_dev(nlangcp__posix_stat_t self) { return self->st_dev; }
u64 posix_stat_t_ino(nlangcp__posix_stat_t self) { return self->st_ino; }
u32 posix_stat_t_mode(nlangcp__posix_stat_t self) { return self->st_mode; }
u64 posix_stat_t_nlink(nlangcp__posix_stat_t self) { return self->st_nlink; }
u64 posix_stat_t_uid(nlangcp__posix_stat_t self) { return self->st_uid; }
u64 posix_stat_t_gid(nlangcp__posix_stat_t self) { return self->st_gid; }
u64 posix_stat_t_rdev(nlangcp__posix_stat_t self) { return self->st_rdev; }
size posix_stat_t_size(nlangcp__posix_stat_t self) { return self->st_size; }
size posix_stat_t_blksize(nlangcp__posix_stat_t self) { return self->st_blksize; }
size posix_stat_t_blocks(nlangcp__posix_stat_t self) { return self->st_blocks; }
i64 posix_stat_t_atime(nlangcp__posix_stat_t self) { return self->st_atime; }
i64 posix_stat_t_mtime(nlangcp__posix_stat_t self) { return self->st_mtime; }
i64 posix_stat_t_ctime(nlangcp__posix_stat_t self) { return self->st_ctime; }


nlang_errormod_nerror posix_stat(nlangcp__u8 pathname, nlangp__posix_stat_t buf) {
  if (stat((const char *) pathname, buf) < 0) {
    return NERROR(errno);
  } else {
    return NERROR_OK;
  }
}

nlang_errormod_nerror posix_fstat(i32 fd, nlangp__posix_stat_t buf) {
  if (fstat(fd, buf) < 0) {
    return NERROR(errno);
  } else {
    return NERROR_OK;
  }
}


TUP2(i32, nlang_errormod_nerror) posix_open(const u8 *pathname, i32 flags, u32 mode) {
  int fd = open((const char *) pathname, flags, mode);
  if (fd < 0) {
    return TUP2VAL(i32, nlang_errormod_nerror, fd, NERROR(errno));
  } else {
    return TUP2VAL(i32, nlang_errormod_nerror, fd, NERROR_OK);
  }
}

nlang_errormod_nerror posix_close(i32 fd) {
  if (close(fd) < 0) {
    return NERROR(errno);
  } else {
    return NERROR_OK;
  }
}

nlang_errormod_nerror posix_unlink(const u8 *pathname) {
  if (unlink((const char *) pathname) < 0) {
    return NERROR(errno);
  } else {
    return NERROR_OK;
  }
}


TUP2(size, nlang_errormod_nerror) posix_read(i32 fd, SL8() *buf, size count) {
  size blen = SL8(_len)(buf);
  if (count > blen) {
    count = blen;
  }

  ssize rd = read(fd, SL8(_unsafe_mutable_rawdata)(buf), count);
  if (rd < 0) {
    return TUP2VAL(size, nlang_errormod_nerror, 0, NERROR(errno));
  } else {
    return TUP2VAL(size, nlang_errormod_nerror, rd, NERROR_OK);
  }
}

TUP2(size, nlang_errormod_nerror) posix_write(i32 fd, const SL8() *buf, size count) {
  size blen = SL8(_len)(buf);
  if (count > blen) {
    count = blen;
  }

  ssize wr = write(fd, buf, count);
  if (wr < 0) {
    return TUP2VAL(size, nlang_errormod_nerror, 0, NERROR(errno));
  } else {
    return TUP2VAL(size, nlang_errormod_nerror, wr, NERROR_OK);
  }
}

nlang_errormod_nerror posix_fsync(i32 fd) {
  if (fsync(fd) < 0) {
    return NERROR(errno);
  } else {
    return NERROR_OK;
  }
}

nlang_errormod_nerror posix_fdatasync(i32 fd) {
  if (fdatasync(fd) < 0) {
    return NERROR(errno);
  } else {
    return NERROR_OK;
  }
}


i32 posix_rand(void) {
  return rand();
}

void posix_srand(u32 seed) {
  srand(seed);
}


i64 posix_timeval_sec(nlangcp__posix_timeval self) { return self->tv_sec; }
i64 posix_timeval_usec(nlangcp__posix_timeval self) { return self->tv_usec; }

nlang_errormod_nerror posix_gettimeofday(nlangp__posix_timeval tv) {
  if (gettimeofday(tv, NULL) < 0) {
    return NERROR(errno);
  } else {
    return NERROR_OK;
  }
}

i64 posix_time() {
  return time(NULL);
}


#undef TUP2VAL
#undef TUP2
#undef NERROR
#undef NERROR_OK
#undef SL8
