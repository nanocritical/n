#include <sys/types.h>
#include <sys/stat.h>
#include <sys/xattr.h>
#include <sys/syscall.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <time.h>

#define NB(t) n$builtins$##t
#define SY(t) n$syscall$##t

#ifdef NLANG_DEFINE_FUNCTIONS

NB(I32) SY(_EPERM) = EPERM;
NB(I32) SY(_ENOENT) = ENOENT;
NB(I32) SY(_ESRCH) = ESRCH;
NB(I32) SY(_EINTR) = EINTR;
NB(I32) SY(_EIO) = EIO;
NB(I32) SY(_ENXIO) = ENXIO;
NB(I32) SY(_E2BIG) = E2BIG;
NB(I32) SY(_ENOEXEC) = ENOEXEC;
NB(I32) SY(_EBADF) = EBADF;
NB(I32) SY(_ECHILD) = ECHILD;
NB(I32) SY(_EAGAIN) = EAGAIN;
NB(I32) SY(_ENOMEM) = ENOMEM;
NB(I32) SY(_EACCES) = EACCES;
NB(I32) SY(_EFAULT) = EFAULT;
NB(I32) SY(_ENOTBLK) = ENOTBLK;
NB(I32) SY(_EBUSY) = EBUSY;
NB(I32) SY(_EEXIST) = EEXIST;
NB(I32) SY(_EXDEV) = EXDEV;
NB(I32) SY(_ENODEV) = ENODEV;
NB(I32) SY(_ENOTDIR) = ENOTDIR;
NB(I32) SY(_EISDIR) = EISDIR;
NB(I32) SY(_EINVAL) = EINVAL;
NB(I32) SY(_ENFILE) = ENFILE;
NB(I32) SY(_EMFILE) = EMFILE;
NB(I32) SY(_ENOTTY) = ENOTTY;
NB(I32) SY(_ETXTBSY) = ETXTBSY;
NB(I32) SY(_EFBIG) = EFBIG;
NB(I32) SY(_ENOSPC) = ENOSPC;
NB(I32) SY(_ESPIPE) = ESPIPE;
NB(I32) SY(_EROFS) = EROFS;
NB(I32) SY(_EMLINK) = EMLINK;
NB(I32) SY(_EPIPE) = EPIPE;
NB(I32) SY(_EDOM) = EDOM;
NB(I32) SY(_ERANGE) = ERANGE;
NB(I32) SY(_EDEADLK) = EDEADLK;
NB(I32) SY(_ENAMETOOLONG) = ENAMETOOLONG;
NB(I32) SY(_ENOLCK) = ENOLCK;
NB(I32) SY(_ENOSYS) = ENOSYS;
NB(I32) SY(_ENOTEMPTY) = ENOTEMPTY;
NB(I32) SY(_ELOOP) = ELOOP;
NB(I32) SY(_EWOULDBLOCK) = EWOULDBLOCK;
NB(I32) SY(_ENOMSG) = ENOMSG;
NB(I32) SY(_EIDRM) = EIDRM;
NB(I32) SY(_ECHRNG) = ECHRNG;
NB(I32) SY(_EL2NSYNC) = EL2NSYNC;
NB(I32) SY(_EL3HLT) = EL3HLT;
NB(I32) SY(_EL3RST) = EL3RST;
NB(I32) SY(_ELNRNG) = ELNRNG;
NB(I32) SY(_EUNATCH) = EUNATCH;
NB(I32) SY(_ENOCSI) = ENOCSI;
NB(I32) SY(_EL2HLT) = EL2HLT;
NB(I32) SY(_EBADE) = EBADE;
NB(I32) SY(_EBADR) = EBADR;
NB(I32) SY(_EXFULL) = EXFULL;
NB(I32) SY(_ENOANO) = ENOANO;
NB(I32) SY(_EBADRQC) = EBADRQC;
NB(I32) SY(_EBADSLT) = EBADSLT;
NB(I32) SY(_EDEADLOCK) = EDEADLOCK;
NB(I32) SY(_EBFONT) = EBFONT;
NB(I32) SY(_ENOSTR) = ENOSTR;
NB(I32) SY(_ENODATA) = ENODATA;
NB(I32) SY(_ETIME) = ETIME;
NB(I32) SY(_ENOSR) = ENOSR;
NB(I32) SY(_ENONET) = ENONET;
NB(I32) SY(_ENOPKG) = ENOPKG;
NB(I32) SY(_EREMOTE) = EREMOTE;
NB(I32) SY(_ENOLINK) = ENOLINK;
NB(I32) SY(_EADV) = EADV;
NB(I32) SY(_ESRMNT) = ESRMNT;
NB(I32) SY(_ECOMM) = ECOMM;
NB(I32) SY(_EPROTO) = EPROTO;
NB(I32) SY(_EMULTIHOP) = EMULTIHOP;
NB(I32) SY(_EDOTDOT) = EDOTDOT;
NB(I32) SY(_EBADMSG) = EBADMSG;
NB(I32) SY(_EOVERFLOW) = EOVERFLOW;
NB(I32) SY(_ENOTUNIQ) = ENOTUNIQ;
NB(I32) SY(_EBADFD) = EBADFD;
NB(I32) SY(_EREMCHG) = EREMCHG;
NB(I32) SY(_ELIBACC) = ELIBACC;
NB(I32) SY(_ELIBBAD) = ELIBBAD;
NB(I32) SY(_ELIBSCN) = ELIBSCN;
NB(I32) SY(_ELIBMAX) = ELIBMAX;
NB(I32) SY(_ELIBEXEC) = ELIBEXEC;
NB(I32) SY(_EILSEQ) = EILSEQ;
NB(I32) SY(_ERESTART) = ERESTART;
NB(I32) SY(_ESTRPIPE) = ESTRPIPE;
NB(I32) SY(_EUSERS) = EUSERS;
NB(I32) SY(_ENOTSOCK) = ENOTSOCK;
NB(I32) SY(_EDESTADDRREQ) = EDESTADDRREQ;
NB(I32) SY(_EMSGSIZE) = EMSGSIZE;
NB(I32) SY(_EPROTOTYPE) = EPROTOTYPE;
NB(I32) SY(_ENOPROTOOPT) = ENOPROTOOPT;
NB(I32) SY(_EPROTONOSUPPORT) = EPROTONOSUPPORT;
NB(I32) SY(_ESOCKTNOSUPPORT) = ESOCKTNOSUPPORT;
NB(I32) SY(_EOPNOTSUPP) = EOPNOTSUPP;
NB(I32) SY(_EPFNOSUPPORT) = EPFNOSUPPORT;
NB(I32) SY(_EAFNOSUPPORT) = EAFNOSUPPORT;
NB(I32) SY(_EADDRINUSE) = EADDRINUSE;
NB(I32) SY(_EADDRNOTAVAIL) = EADDRNOTAVAIL;
NB(I32) SY(_ENETDOWN) = ENETDOWN;
NB(I32) SY(_ENETUNREACH) = ENETUNREACH;
NB(I32) SY(_ENETRESET) = ENETRESET;
NB(I32) SY(_ECONNABORTED) = ECONNABORTED;
NB(I32) SY(_ECONNRESET) = ECONNRESET;
NB(I32) SY(_ENOBUFS) = ENOBUFS;
NB(I32) SY(_EISCONN) = EISCONN;
NB(I32) SY(_ENOTCONN) = ENOTCONN;
NB(I32) SY(_ESHUTDOWN) = ESHUTDOWN;
NB(I32) SY(_ETOOMANYREFS) = ETOOMANYREFS;
NB(I32) SY(_ETIMEDOUT) = ETIMEDOUT;
NB(I32) SY(_ECONNREFUSED) = ECONNREFUSED;
NB(I32) SY(_EHOSTDOWN) = EHOSTDOWN;
NB(I32) SY(_EHOSTUNREACH) = EHOSTUNREACH;
NB(I32) SY(_EALREADY) = EALREADY;
NB(I32) SY(_EINPROGRESS) = EINPROGRESS;
NB(I32) SY(_ESTALE) = ESTALE;
NB(I32) SY(_EUCLEAN) = EUCLEAN;
NB(I32) SY(_ENOTNAM) = ENOTNAM;
NB(I32) SY(_ENAVAIL) = ENAVAIL;
NB(I32) SY(_EISNAM) = EISNAM;
NB(I32) SY(_EREMOTEIO) = EREMOTEIO;
NB(I32) SY(_EDQUOT) = EDQUOT;
NB(I32) SY(_ENOMEDIUM) = ENOMEDIUM;
NB(I32) SY(_EMEDIUMTYPE) = EMEDIUMTYPE;
NB(I32) SY(_ECANCELED) = ECANCELED;
NB(I32) SY(_ENOKEY) = ENOKEY;
NB(I32) SY(_EKEYEXPIRED) = EKEYEXPIRED;
NB(I32) SY(_EKEYREVOKED) = EKEYREVOKED;
NB(I32) SY(_EKEYREJECTED) = EKEYREJECTED;
NB(I32) SY(_EOWNERDEAD) = EOWNERDEAD;
NB(I32) SY(_ENOTRECOVERABLE) = ENOTRECOVERABLE;
NB(I32) SY(_ERFKILL) = ERFKILL;
NB(I32) SY(_EHWPOISON) = EHWPOISON;

NB(I32) SY(O_RDONLY) = O_RDONLY;
NB(I32) SY(O_WRONLY) = O_WRONLY;
NB(I32) SY(O_RDWR) = O_RDWR;
NB(I32) SY(O_APPEND) = O_APPEND;
NB(I32) SY(O_ASYNC) = O_ASYNC;
NB(I32) SY(O_CLOEXEC) = O_CLOEXEC;
NB(I32) SY(O_CREAT) = O_CREAT;
NB(I32) SY(O_DIRECT) = O_DIRECT;
NB(I32) SY(O_DIRECTORY) = O_DIRECTORY;
NB(I32) SY(O_DSYNC) = O_DSYNC;
NB(I32) SY(O_EXCL) = O_EXCL;
NB(I32) SY(O_LARGEFILE) = O_LARGEFILE;
NB(I32) SY(O_NOATIME) = O_NOATIME;
NB(I32) SY(O_NOCTTY) = O_NOCTTY;
NB(I32) SY(O_NOFOLLOW) = O_NOFOLLOW;
NB(I32) SY(O_NONBLOCK) = O_NONBLOCK;
NB(I32) SY(O_NDELAY) = O_NDELAY;
NB(I32) SY(O_PATH) = O_PATH;
NB(I32) SY(O_SYNC) = O_SYNC;
NB(I32) SY(O_TMPFILE) = O_TMPFILE;
NB(I32) SY(O_TRUNC) = O_TRUNC;

NB(Int) SY(AT_FDCWD) = AT_FDCWD;
NB(I32) SY(AT_SYMLINK_FOLLOW) = AT_SYMLINK_FOLLOW;
NB(I32) SY(AT_SYMLINK_NOFOLLOW) = AT_SYMLINK_NOFOLLOW;
NB(I32) SY(AT_EMPTY_PATH) = AT_EMPTY_PATH;
NB(I32) SY(AT_REMOVEDIR) = AT_REMOVEDIR;

NB(I32) SY(SEEK_SET) = SEEK_SET;
NB(I32) SY(SEEK_CUR) = SEEK_CUR;
NB(I32) SY(SEEK_END) = SEEK_END;
NB(I32) SY(SEEK_DATA) = SEEK_DATA;
NB(I32) SY(SEEK_HOLE) = SEEK_HOLE;

NB(I32) SY(XATTR_CREATE) = XATTR_CREATE;
NB(I32) SY(XATTR_REPLACE) = XATTR_REPLACE;

NB(I32) SY(CLOCK_REALTIME) = CLOCK_REALTIME;
NB(I32) SY(CLOCK_REALTIME_COARSE) = CLOCK_REALTIME_COARSE;
NB(I32) SY(CLOCK_MONOTONIC) = CLOCK_MONOTONIC;
NB(I32) SY(CLOCK_MONOTONIC_COARSE) = CLOCK_MONOTONIC_COARSE;
NB(I32) SY(CLOCK_MONOTONIC_RAW) = CLOCK_MONOTONIC_RAW;
NB(I32) SY(CLOCK_BOOTTIME) = CLOCK_BOOTTIME;
NB(I32) SY(CLOCK_PROCESS_CPUTIME_ID) = CLOCK_PROCESS_CPUTIME_ID;
NB(I32) SY(CLOCK_THREAD_CPUTIME_ID) = CLOCK_THREAD_CPUTIME_ID;

NB(I32) SY(PROT_EXEC) = PROT_EXEC;
NB(I32) SY(PROT_READ) = PROT_READ;
NB(I32) SY(PROT_WRITE) = PROT_WRITE;
NB(I32) SY(PROT_NONE) = PROT_NONE;

NB(I32) SY(MAP_SHARED) = MAP_SHARED;
NB(I32) SY(MAP_PRIVATE) = MAP_PRIVATE;
NB(I32) SY(MAP_ANONYMOUS) = MAP_ANONYMOUS;

NB(U8) *SY(MAP_FAILED) = MAP_FAILED;

// Some of these functions should explicitly set errno to 0 before
// performing the underlying call (e.g. sysconf), as -1 can be a valid
// return value and on its own -1 is not sufficient to determine whether an
// error occured.

static __thread int _$Nlatestsyscallerrno;

static NB(I32) SY(errno)(void) {
  return _$Nlatestsyscallerrno;
}

static NB(Int) SY(unlink)(NB(U8) *pathname) {
  int ret = unlink((char *) pathname);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(close)(NB(Int) fd) {
  int ret = close(fd);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(open)(NB(U8) *pathname, NB(I32) flags, NB(U32) mode) {
  int ret = open((char *) pathname, flags, mode);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(openat)(NB(Int) dirfd, NB(U8) *pathname, NB(I32) flags, NB(U32) mode) {
  // openat() wrapper is borked on glibc < 2.21, see
  // https://sourceware.org/bugzilla/show_bug.cgi?id=17523
  int ret = syscall(SYS_openat, dirfd, (char *) pathname, flags, mode);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(mkfifoat)(NB(Int) dirfd, NB(U8) *pathname, NB(U32) mode) {
  int ret = mkfifoat(dirfd, (char *) pathname, mode);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(linkat)(NB(Int) olddirfd, NB(U8) *oldpath,
                          NB(Int) newdirfd, NB(U8) *newpath, NB(I32) flags) {
  int ret = linkat(olddirfd, (char *) oldpath, newdirfd, (char *) newpath, flags);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(unlinkat)(NB(Int) dirfd, NB(U8) *pathname, NB(I32) flags) {
  int ret = unlinkat(dirfd, (char *) pathname, flags);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(mkdirat)(NB(Int) dirfd, NB(U8) *pathname, NB(U32) mode) {
  int ret = mkdirat(dirfd, (char *) pathname, mode);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(fchdir)(NB(Int) fd) {
  int ret = fchdir(fd);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(symlinkat)(NB(U8) *target, NB(Int) newdirfd, NB(U8) *linkpath) {
  int ret = symlinkat((char *) target, newdirfd, (char *) linkpath);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(readlinkat)(NB(Int) dirfd, NB(U8) *pathname, NB(U8) *buf, NB(Uint) bufsiz) {
  ssize_t ret = readlinkat(dirfd, (char *) pathname, (char *) buf, bufsiz);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(renameat)(NB(Int) olddirfd, NB(U8) *oldpath,
                            NB(Int) newdirfd, NB(U8) *newpath, NB(U32) flags) {
  (void) flags;
  int ret = renameat(olddirfd, (char *) oldpath, newdirfd, (char *) newpath);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(write)(NB(Int) fd, NB(U8) *buf, NB(Uint) count) {
  ssize_t ret = write(fd, buf, count);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(pwrite)(NB(Int) fd, NB(U8) *buf, NB(Uint) count, NB(Uint) off) {
  ssize_t ret = pwrite(fd, buf, count, off);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(read)(NB(Int) fd, NB(U8) *buf, NB(Uint) count) {
  ssize_t ret = read(fd, buf, count);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(pread)(NB(Int) fd, NB(U8) *buf, NB(Uint) count, NB(Uint) off) {
  ssize_t ret = pread(fd, buf, count, off);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(lseek)(NB(Int) fd, NB(Int) off, NB(I32) whence) {
  off_t ret = lseek(fd, off, whence);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(sysconf)(NB(Int) name) {
  errno = 0;
  long ret = sysconf(name);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(fstatat)(NB(Int) dirfd, NB(U8) *pathname, struct SY(Stat_t) *buf, NB(I32) flags) {
  errno = 0;
  struct stat st = { 0 };
  int ret = fstatat(dirfd, (char *) pathname, &st, flags);
  _$Nlatestsyscallerrno = errno;

  if (ret >= 0) {
    buf->Size = st.st_size;
    buf->Mtime_sec = st.st_mtim.tv_sec;
    buf->Mtime_nsec = st.st_mtim.tv_nsec;
    buf->Mode = st.st_mode;
    buf->Owner = st.st_uid;
    buf->Group = st.st_gid;
  }

  return ret;
}

static NB(Int) SY(fdatasync)(NB(Int) fd) {
  errno = 0;
  int ret = fdatasync(fd);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(fsync)(NB(Int) fd) {
  errno = 0;
  int ret = fsync(fd);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(ftruncate)(NB(Int) fd, NB(Uint) size) {
  errno = 0;
  int ret = ftruncate(fd, size);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(fchown)(NB(Int) fd, NB(Int) uid, NB(Int) gid) {
  errno = 0;
  int ret = fchown(fd, uid, gid);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(fsetxattr)(NB(Int) fd, NB(U8) *name, NB(U8) *value, NB(Uint) size, NB(I32) flags) {
  errno = 0;
  ssize_t ret = fsetxattr(fd, (char *) name, (char *) value, size, flags);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(fgetxattr)(NB(Int) fd, NB(U8) *name, NB(U8) *value, NB(Uint) size) {
  errno = 0;
  ssize_t ret = fgetxattr(fd, (char *) name, (char *) value, size);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(flistxattr)(NB(Int) fd, NB(U8) *list, NB(Uint) size) {
  errno = 0;
  ssize_t ret = flistxattr(fd, (char *) list, size);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(clock_getres)(NB(I32) clk_id, NB(Int) *s, NB(Int) *ns) {
  struct timespec res = { 0 };
  int ret = clock_getres((clockid_t) clk_id, &res);
  *s = res.tv_sec;
  *ns = res.tv_nsec;
  return ret;
}

static NB(Int) SY(clock_gettime)(NB(I32) clk_id, NB(Int) *s, NB(Int) *ns) {
  struct timespec res = { 0 };
  int ret = clock_gettime((clockid_t) clk_id, &res);
  *s = res.tv_sec;
  *ns = res.tv_nsec;
  return ret;
}

NB(Uint) SY(Strlen)(NB(U8) *s) {
  return strlen((char *) s);
}

static NB(U8) *SY(getenv)(NB(U8) *name) {
  return (NB(U8) *) secure_getenv((char *) name);
}

static NB(Int) SY(setenv)(NB(U8) *name, NB(U8) *value, NB(I32) overwrite) {
  return setenv((char *) name, (char *) value, overwrite);
}

static NB(U8) *SY(mmap)(NB(Uintptr) addr, NB(Uint) length, NB(I32) prot, NB(I32) flags,
                        NB(Int) fd, NB(Uint) offset) {
  return mmap((void *) addr, length, prot, flags, fd, offset);
}

static NB(Int) SY(munmap)(NB(U8) *addr, NB(Uint) length) {
  return munmap((void *) addr, length);
}

#endif

#undef SY
#undef NB
