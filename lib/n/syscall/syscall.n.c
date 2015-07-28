#include <sys/types.h>
#include <sys/stat.h>
#include <sys/xattr.h>
#include <sys/syscall.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/epoll.h>
#include <netdb.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <time.h>
#include <signal.h>
#include <arpa/inet.h>

#define NB(t) n$builtins$##t
#define SY(t) n$syscall$##t

#define SY_int n$builtins$I32
#define SY_clockid_t n$builtins$I32

#ifdef NLANG_DEFINE_FUNCTIONS

NB(Uint) SY(Strlen)(NB(U8) *s) {
  return strlen((char *) s);
}

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

NB(I32) SY(AT_FDCWD) = AT_FDCWD;
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

static SY_int SY(unlink)(NB(U8) *pathname) {
  int ret = unlink((char *) pathname);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(close)(SY_int fd) {
  int ret = close(fd);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(open)(NB(U8) *pathname, NB(I32) flags, NB(U32) mode) {
  int ret = open((char *) pathname, flags, mode);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(openat)(SY_int dirfd, NB(U8) *pathname, NB(I32) flags, NB(U32) mode) {
  // openat() wrapper is borked on glibc < 2.21, see
  // https://sourceware.org/bugzilla/show_bug.cgi?id=17523
  int ret = syscall(SYS_openat, dirfd, (char *) pathname, flags, mode);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

NB(I32) SY(F_DUPFD) = F_DUPFD;
NB(I32) SY(F_GETFD) = F_GETFD;
NB(I32) SY(F_SETFD) = F_SETFD;
NB(I32) SY(F_GETFL) = F_GETFL;
NB(I32) SY(F_SETFL) = F_SETFL;

static SY_int SY(fcntl_void)(SY_int fd, SY_int cmd) {
  int ret = fcntl(fd, cmd);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(fcntl_i32)(SY_int fd, SY_int cmd, SY_int arg) {
  int ret = fcntl(fd, cmd, arg);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(fcntl_ref)(SY_int fd, SY_int cmd, NB(U8) *arg) {
  int ret = fcntl(fd, cmd, arg);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(mkfifoat)(SY_int dirfd, NB(U8) *pathname, NB(U32) mode) {
  int ret = mkfifoat(dirfd, (char *) pathname, mode);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(linkat)(SY_int olddirfd, NB(U8) *oldpath,
                          SY_int newdirfd, NB(U8) *newpath, NB(I32) flags) {
  int ret = linkat(olddirfd, (char *) oldpath, newdirfd, (char *) newpath, flags);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(unlinkat)(SY_int dirfd, NB(U8) *pathname, NB(I32) flags) {
  int ret = unlinkat(dirfd, (char *) pathname, flags);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(mkdirat)(SY_int dirfd, NB(U8) *pathname, NB(U32) mode) {
  int ret = mkdirat(dirfd, (char *) pathname, mode);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(fchdir)(SY_int fd) {
  int ret = fchdir(fd);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(symlinkat)(NB(U8) *target, SY_int newdirfd, NB(U8) *linkpath) {
  int ret = symlinkat((char *) target, newdirfd, (char *) linkpath);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(readlinkat)(SY_int dirfd, NB(U8) *pathname, NB(U8) *buf, NB(Uint) bufsiz) {
  ssize_t ret = readlinkat(dirfd, (char *) pathname, (char *) buf, bufsiz);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(renameat)(SY_int olddirfd, NB(U8) *oldpath,
                            SY_int newdirfd, NB(U8) *newpath, NB(U32) flags) {
  (void) flags;
  int ret = renameat(olddirfd, (char *) oldpath, newdirfd, (char *) newpath);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(write)(SY_int fd, NB(U8) *buf, NB(Uint) count) {
  // write(2) says that on a short write, it *may* report an error. We hope
  // it sets errno whenever there is a short write, even if no error has
  // been detected. But we don't trust it.
  errno = 0;
  ssize_t ret = write(fd, buf, count);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(pwrite)(SY_int fd, NB(U8) *buf, NB(Uint) count, NB(Uint) off) {
  // pwrite(2) says that on a short write, it *may* report an error. We hope
  // it sets errno whenever there is a short write, even if no error has
  // been detected. But we don't trust it.
  errno = 0;
  ssize_t ret = pwrite(fd, buf, count, off);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(read)(SY_int fd, NB(U8) *buf, NB(Uint) count) {
  ssize_t ret = read(fd, buf, count);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(pread)(SY_int fd, NB(U8) *buf, NB(Uint) count, NB(Uint) off) {
  ssize_t ret = pread(fd, buf, count, off);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(lseek)(SY_int fd, NB(Int) off, NB(I32) whence) {
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

static SY_int SY(fstatat)(SY_int dirfd, NB(U8) *pathname, struct SY(Stat_t) *buf, NB(I32) flags) {
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

static SY_int SY(fdatasync)(SY_int fd) {
  errno = 0;
  int ret = fdatasync(fd);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(fsync)(SY_int fd) {
  errno = 0;
  int ret = fsync(fd);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(ftruncate)(SY_int fd, NB(Uint) size) {
  errno = 0;
  int ret = ftruncate(fd, size);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(fchown)(SY_int fd, NB(Int) uid, NB(Int) gid) {
  errno = 0;
  int ret = fchown(fd, uid, gid);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(fsetxattr)(SY_int fd, NB(U8) *name, NB(U8) *value, NB(Uint) size, NB(I32) flags) {
  errno = 0;
  int ret = fsetxattr(fd, (char *) name, (char *) value, size, flags);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(fgetxattr)(SY_int fd, NB(U8) *name, NB(U8) *value, NB(Uint) size) {
  errno = 0;
  ssize_t ret = fgetxattr(fd, (char *) name, (char *) value, size);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static NB(Int) SY(flistxattr)(SY_int fd, NB(U8) *list, NB(Uint) size) {
  errno = 0;
  ssize_t ret = flistxattr(fd, (char *) list, size);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(clock_getres)(SY_clockid_t clk_id, NB(Int) *s, NB(Int) *ns) {
  struct timespec res = { 0 };
  int ret = clock_getres((clockid_t) clk_id, &res);
  *s = res.tv_sec;
  *ns = res.tv_nsec;
  return ret;
}

static SY_int SY(clock_gettime)(SY_clockid_t clk_id, NB(Int) *s, NB(Int) *ns) {
  struct timespec res = { 0 };
  int ret = clock_gettime((clockid_t) clk_id, &res);
  *s = res.tv_sec;
  *ns = res.tv_nsec;
  return ret;
}

static NB(U8) *SY(getenv)(NB(U8) *name) {
  return (NB(U8) *) secure_getenv((char *) name);
}

static SY_int SY(setenv)(NB(U8) *name, NB(U8) *value, NB(I32) overwrite) {
  return setenv((char *) name, (char *) value, overwrite);
}

static NB(U8) *SY(mmap)(NB(Uintptr) addr, NB(Uint) length, NB(I32) prot, NB(I32) flags,
                        SY_int fd, NB(Uint) offset) {
  void *ret = mmap((void *) addr, length, prot, flags, fd, offset);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(munmap)(NB(U8) *addr, NB(Uint) length) {
  int ret = munmap((void *) addr, length);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(dup)(SY_int oldfd) {
  int ret = dup(oldfd);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(dup3)(SY_int oldfd, SY_int newfd, SY_int flags) {
  int ret = dup3(oldfd, newfd, flags);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

SY_int SY(_EAI_ADDRFAMILY) = EAI_ADDRFAMILY;
SY_int SY(_EAI_AGAIN) = EAI_AGAIN;
SY_int SY(_EAI_BADFLAGS) = EAI_BADFLAGS;
SY_int SY(_EAI_FAIL) = EAI_FAIL;
SY_int SY(_EAI_FAMILY) = EAI_FAMILY;
SY_int SY(_EAI_MEMORY) = EAI_MEMORY;
SY_int SY(_EAI_NODATA) = EAI_NODATA;
SY_int SY(_EAI_NONAME) = EAI_NONAME;
SY_int SY(_EAI_SERVICE) = EAI_SERVICE;
SY_int SY(_EAI_SOCKTYPE) = EAI_SOCKTYPE;

NB(I32) SY(AF_INET) = AF_INET;
NB(I32) SY(AF_INET6) = AF_INET6;
NB(I32) SY(AF_UNSPEC) = AF_UNSPEC;
NB(I32) SY(AI_V4MAPPED) = AI_V4MAPPED;
NB(I32) SY(AI_ADDRCONFIG) = AI_ADDRCONFIG;
NB(I32) SY(AI_ALL) = AI_ALL;
NB(I32) SY(AI_CANONNAME) = AI_CANONNAME;
NB(I32) SY(AI_NUMERICHOST) = AI_NUMERICHOST;
NB(I32) SY(AI_PASSIVE) = AI_PASSIVE;
NB(I32) SY(SOCK_STREAM) = SOCK_STREAM;
NB(I32) SY(SOCK_DGRAM) = SOCK_DGRAM;
NB(I32) SY(SOCK_NONBLOCK) = SOCK_NONBLOCK;
NB(I32) SY(SOCK_CLOEXEC) = SOCK_CLOEXEC;
NB(I32) SY(IPPROTO_TCP) = IPPROTO_TCP;
NB(I32) SY(IPPROTO_UDP) = IPPROTO_UDP;

struct SY(Addrinfo) SY(Addrinfo$From_raw)(NB(U8) *raw) {
  struct SY(Addrinfo) r = { 0 };
  struct addrinfo *ai = (void *) raw;
  r.Flags = ai->ai_flags;
  r.Family = ai->ai_family;
  r.Socktype = ai->ai_socktype;
  r.Protocol = ai->ai_protocol;
  r.Raw_addrlen = ai->ai_addrlen;
  r.Raw_addr = (NB(U8) *) ai->ai_addr;
  r.Canonname.bytes.dat = (NB(U8) *) ai->ai_canonname;
  if (ai->ai_canonname != NULL) {
    r.Canonname.bytes.cnt = strlen(ai->ai_canonname);
    r.Canonname.bytes.cap = r.Canonname.bytes.cnt + 1;
  }
  r.Raw_next = (NB(U8) *) ai->ai_next;
  return r;
}

NB(byteslice) SY(Sockaddr_ip_bytes)(NB(I32) family, NB(U8) *raw_addr, NB(Uint) raw_addrlen) {
  NB(byteslice) ret = { 0 };

  if (family == SY(AF_INET)) {
    struct sockaddr_in *addr = (void *) raw_addr;
    ret.dat = (NB(U8) *) &addr->sin_addr;
    ret.cnt = 4;
    ret.cap = 4;
  } else if (family == SY(AF_INET6)) {
    struct sockaddr_in6 *addr = (void *) raw_addr;
    ret.dat = (NB(U8) *) &addr->sin6_addr;
    ret.cnt = 16;
    ret.cap = 16;
  }

  return ret;
}

NB(Uint) SY(Sockaddr_ip_port)(NB(I32) family, NB(U8) *raw_addr, NB(Uint) raw_addrlen) {
  NB(Uint) ret = 0;

  if (family == SY(AF_INET)) {
    struct sockaddr_in *addr = (void *) raw_addr;
    ret = ntohs(addr->sin_port);
  } else if (family == SY(AF_INET6)) {
    struct sockaddr_in6 *addr = (void *) raw_addr;
    ret = ntohs(addr->sin6_port);
  }

  return ret;
}

NB(Uint) SY(Sockaddr_ip_sizeof)(NB(I32) family) {
  if (family == SY(AF_INET)) {
    return sizeof(struct sockaddr_in);
  } else if (family == SY(AF_INET6)) {
    return sizeof(struct sockaddr_in6);
  } else {
    return 0;
  }
}

NB(Void) SY(Sockaddr_ip)(NB(byteslice) *buf, NB(I32) family, NB(byteslice) ip6, NB(Uint) port) {
  if (family == SY(AF_INET)) {
    // The IP is always passed as IPv4 mapped to IPv6.
    struct sockaddr_in addr = { 0 };
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    memcpy(&addr.sin_addr, ip6.dat + 12, 4);

    buf->cnt = sizeof(struct sockaddr_in);
    memcpy(buf->dat, &addr, buf->cnt);
  } else if (family == SY(AF_INET6)) {
    struct sockaddr_in6 addr = { 0 };
    addr.sin6_family = AF_INET6;
    addr.sin6_port = htons(port);
    memcpy(&addr.sin6_addr, ip6.dat, ip6.cnt);

    buf->cnt = sizeof(struct sockaddr_in6);
    memcpy(buf->dat, &addr, buf->cnt);
  }
}

static SY_int SY(getaddrinfo)(NB(U8) *node, NB(U8) *service, struct SY(Addrinfo) *hints,
                               NB(U8) **res) {
  struct addrinfo _hints = {
    .ai_flags = hints->Flags,
    .ai_family = hints->Family,
    .ai_socktype = hints->Socktype,
    .ai_protocol = hints->Protocol,
    0,
  };

  int ret = getaddrinfo((char *) node, (char *) service, &_hints, (struct addrinfo **) res);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static void SY(freeaddrinfo)(NB(U8) *res) {
  freeaddrinfo((struct addrinfo *) res);
}


static inline NB(U32) SY(Htonl)(NB(U32) hostlong) {
  return htonl(hostlong);
}

static inline NB(U16) SY(Htons)(NB(U16) hostshort) {
  return htons(hostshort);
}

static inline NB(U32) SY(Ntohl)(NB(U32) netlong) {
  return ntohl(netlong);
}

static inline NB(U16) SY(Ntohs)(NB(U16) netshort) {
  return ntohs(netshort);
}


static SY_int SY(socket)(NB(I32) domain, NB(I32) type, NB(I32) protocol) {
  int ret = socket(domain, type, protocol);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(bind)(SY_int sockfd, NB(U8) *raw_addr, NB(Uint) raw_addrlen) {
  int ret = bind(sockfd, (const struct sockaddr *) raw_addr, (socklen_t) raw_addrlen);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(listen)(SY_int sockfd, NB(Int) backlog) {
  int ret = listen(sockfd, backlog);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(accept4)(SY_int sockfd, NB(U8) *raw_addr, NB(Uint) *raw_addrlen, NB(I32) flags) {
  socklen_t addrlen = *raw_addrlen;
  int ret = accept4(sockfd, (struct sockaddr *) raw_addr, &addrlen, flags);
  _$Nlatestsyscallerrno = errno;
  *raw_addrlen = addrlen;
  return ret;
}

static SY_int SY(connect)(SY_int sockfd, NB(U8) *raw_addr, NB(Uint) raw_addrlen) {
  socklen_t addrlen = raw_addrlen;
  int ret = connect(sockfd, (struct sockaddr *) raw_addr, addrlen);
  _$Nlatestsyscallerrno = errno;
  return ret;
}


NB(I32) SY(SOL_SOCKET) = SOL_SOCKET;
NB(I32) SY(SO_REUSEADDR) = SO_REUSEADDR;
NB(I32) SY(SO_ERROR) = SO_ERROR;

static SY_int SY(getsockopt)(SY_int sockfd, NB(I32) level, NB(I32) optname,
                              NB(U8) *optval, NB(Uint) *optlen) {
  socklen_t _optlen = *optlen;
  int ret = getsockopt(sockfd, level, optname, optval, &_optlen);
  *optlen = _optlen;
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(setsockopt)(SY_int sockfd, NB(I32) level, NB(I32) optname,
                              NB(U8) *optval, NB(Uint) optlen) {
  socklen_t _optlen = optlen;
  int ret = setsockopt(sockfd, level, optname, optval, _optlen);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(getsockname)(SY_int sockfd, NB(U8) *raw_addr, NB(Uint) *raw_addrlen) {
  socklen_t addrlen = *raw_addrlen;
  int ret = getsockname(sockfd, (struct sockaddr *) raw_addr, &addrlen);
  _$Nlatestsyscallerrno = errno;
  *raw_addrlen = addrlen;
  return ret;
}

NB(I32) SY(SHUT_RD) = SHUT_RD;
NB(I32) SY(SHUT_WR) = SHUT_WR;
NB(I32) SY(SHUT_RDWR) = SHUT_RDWR;

static SY_int SY(shutdown)(SY_int sockfd, NB(I32) how) {
  int ret = shutdown(sockfd, how);
  _$Nlatestsyscallerrno = errno;
  return ret;
}


NB(I32) SY(EPOLL_CLOEXEC) = EPOLL_CLOEXEC;

NB(I32) SY(EPOLL_CTL_ADD) = EPOLL_CTL_ADD;
NB(I32) SY(EPOLL_CTL_MOD) = EPOLL_CTL_MOD;
NB(I32) SY(EPOLL_CTL_DEL) = EPOLL_CTL_DEL;

NB(U32) SY(EPOLLIN) = EPOLLIN;
NB(U32) SY(EPOLLOUT) = EPOLLOUT;
NB(U32) SY(EPOLLRDHUP) = EPOLLRDHUP;
NB(U32) SY(EPOLLPRI) = EPOLLPRI;
NB(U32) SY(EPOLLERR) = EPOLLERR;
NB(U32) SY(EPOLLHUP) = EPOLLHUP;
NB(U32) SY(EPOLLET) = EPOLLET;
NB(U32) SY(EPOLLONESHOT) = EPOLLONESHOT;
NB(U32) SY(EPOLLWAKEUP) = EPOLLWAKEUP;

static SY_int SY(epoll_create1)(SY_int flags) {
  int ret = epoll_create1(flags);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(epoll_ctl)(SY_int epfd, NB(I32) op, SY_int fd, NB(U8) *raw_event) {
  int ret = epoll_ctl(epfd, op, fd, (struct epoll_event *) raw_event);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(epoll_pwait)(SY_int epfd, NB(U8) *raw_events, NB(Uint) maxevents, NB(Int) timeout,
                               NB(U8) *raw_sigmask) {
  int ret = epoll_pwait(epfd, (struct epoll_event *) raw_events, maxevents, timeout, (const sigset_t *) raw_sigmask);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

NB(I32) SY(SIGHUP) = SIGHUP;
NB(I32) SY(SIGINT) = SIGINT;
NB(I32) SY(SIGQUIT) = SIGQUIT;
NB(I32) SY(SIGILL) = SIGILL;
NB(I32) SY(SIGABRT) = SIGABRT;
NB(I32) SY(SIGFPE) = SIGFPE;
NB(I32) SY(SIGKILL) = SIGKILL;
NB(I32) SY(SIGSEGV) = SIGSEGV;
NB(I32) SY(SIGPIPE) = SIGPIPE;
NB(I32) SY(SIGALRM) = SIGALRM;
NB(I32) SY(SIGTERM) = SIGTERM;

NB(I32) SY(SIGUSR1) = SIGUSR1;
NB(I32) SY(SIGUSR2) = SIGUSR2;
NB(I32) SY(SIGCHLD) = SIGCHLD;
NB(I32) SY(SIGCONT) = SIGCONT;
NB(I32) SY(SIGSTOP) = SIGSTOP;
NB(I32) SY(SIGTSTP) = SIGTSTP;
NB(I32) SY(SIGTTIN) = SIGTTIN;
NB(I32) SY(SIGTTOU) = SIGTTOU;

NB(I32) SY(SIGTRAP) = SIGTRAP;

static void SY(sigemptyset)(NB(U8) *raw_set) {
  sigemptyset((sigset_t *)raw_set);
}

static SY_int SY(sigaddset)(NB(U8) *raw_set, SY_int signum) {
  int ret = sigaddset((sigset_t *)raw_set, signum);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

NB(I32) SY(SFD_CLOEXEC) = SFD_CLOEXEC;
NB(I32) SY(SFD_NONBLOCK) = SFD_NONBLOCK;

static SY_int SY(signalfd)(SY_int fd, NB(U8) *raw_mask, SY_int flags) {
  int ret = signalfd(fd, (const sigset_t *)raw_mask, flags);
  _$Nlatestsyscallerrno = errno;
  return ret;
}


NB(U8) *n$syscall$SIGACTION_DFL(void) {
  static __thread struct n$syscall$Sigaction act = { 0 };
  act.act.sa_handler = SIG_DFL;
  return (void *)&act;
}

NB(U8) *n$syscall$SIGACTION_IGN(void) {
  static __thread struct n$syscall$Sigaction act = { 0 };
  act.act.sa_handler = SIG_IGN;
  return (void *)&act;
}

static SY_int SY(sigaction)(NB(I32) signum, NB(U8) *raw_act, NB(U8) *raw_oldact) {
  int ret = sigaction(signum, (const struct sigaction *)raw_act, (struct sigaction *)raw_oldact);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

NB(I32) SY(SIG_BLOCK) = SIG_BLOCK;
NB(I32) SY(SIG_UNBLOCK) = SIG_UNBLOCK;
NB(I32) SY(SIG_SETMASK) = SIG_SETMASK;

static SY_int SY(sigprocmask)(SY_int how, NB(U8) *raw_set, NB(U8) *raw_oldset) {
  int ret = sigprocmask(how, (const sigset_t *)raw_set, (sigset_t *)raw_oldset);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(execve)(NB(U8) *filename, NB(U8) **argv, NB(U8) **envp) {
  int ret = execve((char *) filename, (char **) argv, (char **) envp);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(pipe2)(SY_int *pipefd, SY_int flags) {
  int ret = pipe2(pipefd, flags);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static SY_int SY(fork)(void) {
  int ret = fork();
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static void SY(_exit)(SY_int status) {
  _exit(status);
}

NB(I32) SY(WNOHANG) = WNOHANG;
NB(I32) SY(WUNTRACED) = WUNTRACED;
NB(I32) SY(WCONTINUED) = WCONTINUED;

static SY_int SY(waitpid)(SY_int pid, SY_int *status, SY_int options) {
  int ret = waitpid(pid, status, options);
  _$Nlatestsyscallerrno = errno;
  return ret;
}

static void tm_sys(struct tm *sys, const struct SY(Tm) *tm) {
  sys->tm_sec = tm->Sec;
  sys->tm_min = tm->Min;
  sys->tm_hour = tm->Hour;
  sys->tm_mday = tm->Mday;
  sys->tm_mon = tm->Mon;
  sys->tm_year = tm->Year;
  sys->tm_wday = tm->Wday;
  sys->tm_yday = tm->Yday;
  sys->tm_isdst = tm->Isdst;
}

static void sys_tm(struct SY(Tm) *tm, const struct tm *sys) {
  tm->Sec = sys->tm_sec;
  tm->Min = sys->tm_min;
  tm->Hour = sys->tm_hour;
  tm->Mday = sys->tm_mday;
  tm->Mon = sys->tm_mon;
  tm->Year = sys->tm_year;
  tm->Wday = sys->tm_wday;
  tm->Yday = sys->tm_yday;
  tm->Isdst = sys->tm_isdst;
}

struct SY(Tm) *SY(Gmtime_r)(NB(I64) *timep, struct SY(Tm) *result) {
  struct tm sys = { 0 };
  if (gmtime_r(timep, &sys) == NULL) {
    return NULL;
  }
  sys_tm(result, &sys);
  return result;
}

struct SY(Tm) *SY(Localtime_r)(NB(I64) *timep, struct SY(Tm) *result) {
  struct tm sys = { 0 };
  if (localtime_r(timep, &sys) == NULL) {
    return NULL;
  }
  sys_tm(result, &sys);
  return result;
}

NB(Uint) SY(strftime)(NB(U8) *s, NB(Uint) max, NB(U8) *format, struct SY(Tm) *tm) {
  struct tm sys = { 0 };
  tm_sys(&sys, tm);
  return strftime((char *)s, max, (const char *)format, &sys);
}

#endif

#undef SY
#undef NB
