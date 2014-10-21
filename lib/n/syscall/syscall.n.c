#include <sys/types.h>
#include <sys/stat.h>
#include <sys/xattr.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

#define NB(t) n$builtins$##t
#define SY(t) n$syscall$##t

#ifdef NLANG_DEFINE_FUNCTIONS

static NB(I32) SY(_EPERM) = EPERM;
static NB(I32) SY(_ENOENT) = ENOENT;
static NB(I32) SY(_ESRCH) = ESRCH;
static NB(I32) SY(_EINTR) = EINTR;
static NB(I32) SY(_EIO) = EIO;
static NB(I32) SY(_ENXIO) = ENXIO;
static NB(I32) SY(_E2BIG) = E2BIG;
static NB(I32) SY(_ENOEXEC) = ENOEXEC;
static NB(I32) SY(_EBADF) = EBADF;
static NB(I32) SY(_ECHILD) = ECHILD;
static NB(I32) SY(_EAGAIN) = EAGAIN;
static NB(I32) SY(_ENOMEM) = ENOMEM;
static NB(I32) SY(_EACCES) = EACCES;
static NB(I32) SY(_EFAULT) = EFAULT;
static NB(I32) SY(_ENOTBLK) = ENOTBLK;
static NB(I32) SY(_EBUSY) = EBUSY;
static NB(I32) SY(_EEXIST) = EEXIST;
static NB(I32) SY(_EXDEV) = EXDEV;
static NB(I32) SY(_ENODEV) = ENODEV;
static NB(I32) SY(_ENOTDIR) = ENOTDIR;
static NB(I32) SY(_EISDIR) = EISDIR;
static NB(I32) SY(_EINVAL) = EINVAL;
static NB(I32) SY(_ENFILE) = ENFILE;
static NB(I32) SY(_EMFILE) = EMFILE;
static NB(I32) SY(_ENOTTY) = ENOTTY;
static NB(I32) SY(_ETXTBSY) = ETXTBSY;
static NB(I32) SY(_EFBIG) = EFBIG;
static NB(I32) SY(_ENOSPC) = ENOSPC;
static NB(I32) SY(_ESPIPE) = ESPIPE;
static NB(I32) SY(_EROFS) = EROFS;
static NB(I32) SY(_EMLINK) = EMLINK;
static NB(I32) SY(_EPIPE) = EPIPE;
static NB(I32) SY(_EDOM) = EDOM;
static NB(I32) SY(_ERANGE) = ERANGE;
static NB(I32) SY(_EDEADLK) = EDEADLK;
static NB(I32) SY(_ENAMETOOLONG) = ENAMETOOLONG;
static NB(I32) SY(_ENOLCK) = ENOLCK;
static NB(I32) SY(_ENOSYS) = ENOSYS;
static NB(I32) SY(_ENOTEMPTY) = ENOTEMPTY;
static NB(I32) SY(_ELOOP) = ELOOP;
static NB(I32) SY(_EWOULDBLOCK) = EWOULDBLOCK;
static NB(I32) SY(_ENOMSG) = ENOMSG;
static NB(I32) SY(_EIDRM) = EIDRM;
static NB(I32) SY(_ECHRNG) = ECHRNG;
static NB(I32) SY(_EL2NSYNC) = EL2NSYNC;
static NB(I32) SY(_EL3HLT) = EL3HLT;
static NB(I32) SY(_EL3RST) = EL3RST;
static NB(I32) SY(_ELNRNG) = ELNRNG;
static NB(I32) SY(_EUNATCH) = EUNATCH;
static NB(I32) SY(_ENOCSI) = ENOCSI;
static NB(I32) SY(_EL2HLT) = EL2HLT;
static NB(I32) SY(_EBADE) = EBADE;
static NB(I32) SY(_EBADR) = EBADR;
static NB(I32) SY(_EXFULL) = EXFULL;
static NB(I32) SY(_ENOANO) = ENOANO;
static NB(I32) SY(_EBADRQC) = EBADRQC;
static NB(I32) SY(_EBADSLT) = EBADSLT;
static NB(I32) SY(_EDEADLOCK) = EDEADLOCK;
static NB(I32) SY(_EBFONT) = EBFONT;
static NB(I32) SY(_ENOSTR) = ENOSTR;
static NB(I32) SY(_ENODATA) = ENODATA;
static NB(I32) SY(_ETIME) = ETIME;
static NB(I32) SY(_ENOSR) = ENOSR;
static NB(I32) SY(_ENONET) = ENONET;
static NB(I32) SY(_ENOPKG) = ENOPKG;
static NB(I32) SY(_EREMOTE) = EREMOTE;
static NB(I32) SY(_ENOLINK) = ENOLINK;
static NB(I32) SY(_EADV) = EADV;
static NB(I32) SY(_ESRMNT) = ESRMNT;
static NB(I32) SY(_ECOMM) = ECOMM;
static NB(I32) SY(_EPROTO) = EPROTO;
static NB(I32) SY(_EMULTIHOP) = EMULTIHOP;
static NB(I32) SY(_EDOTDOT) = EDOTDOT;
static NB(I32) SY(_EBADMSG) = EBADMSG;
static NB(I32) SY(_EOVERFLOW) = EOVERFLOW;
static NB(I32) SY(_ENOTUNIQ) = ENOTUNIQ;
static NB(I32) SY(_EBADFD) = EBADFD;
static NB(I32) SY(_EREMCHG) = EREMCHG;
static NB(I32) SY(_ELIBACC) = ELIBACC;
static NB(I32) SY(_ELIBBAD) = ELIBBAD;
static NB(I32) SY(_ELIBSCN) = ELIBSCN;
static NB(I32) SY(_ELIBMAX) = ELIBMAX;
static NB(I32) SY(_ELIBEXEC) = ELIBEXEC;
static NB(I32) SY(_EILSEQ) = EILSEQ;
static NB(I32) SY(_ERESTART) = ERESTART;
static NB(I32) SY(_ESTRPIPE) = ESTRPIPE;
static NB(I32) SY(_EUSERS) = EUSERS;
static NB(I32) SY(_ENOTSOCK) = ENOTSOCK;
static NB(I32) SY(_EDESTADDRREQ) = EDESTADDRREQ;
static NB(I32) SY(_EMSGSIZE) = EMSGSIZE;
static NB(I32) SY(_EPROTOTYPE) = EPROTOTYPE;
static NB(I32) SY(_ENOPROTOOPT) = ENOPROTOOPT;
static NB(I32) SY(_EPROTONOSUPPORT) = EPROTONOSUPPORT;
static NB(I32) SY(_ESOCKTNOSUPPORT) = ESOCKTNOSUPPORT;
static NB(I32) SY(_EOPNOTSUPP) = EOPNOTSUPP;
static NB(I32) SY(_EPFNOSUPPORT) = EPFNOSUPPORT;
static NB(I32) SY(_EAFNOSUPPORT) = EAFNOSUPPORT;
static NB(I32) SY(_EADDRINUSE) = EADDRINUSE;
static NB(I32) SY(_EADDRNOTAVAIL) = EADDRNOTAVAIL;
static NB(I32) SY(_ENETDOWN) = ENETDOWN;
static NB(I32) SY(_ENETUNREACH) = ENETUNREACH;
static NB(I32) SY(_ENETRESET) = ENETRESET;
static NB(I32) SY(_ECONNABORTED) = ECONNABORTED;
static NB(I32) SY(_ECONNRESET) = ECONNRESET;
static NB(I32) SY(_ENOBUFS) = ENOBUFS;
static NB(I32) SY(_EISCONN) = EISCONN;
static NB(I32) SY(_ENOTCONN) = ENOTCONN;
static NB(I32) SY(_ESHUTDOWN) = ESHUTDOWN;
static NB(I32) SY(_ETOOMANYREFS) = ETOOMANYREFS;
static NB(I32) SY(_ETIMEDOUT) = ETIMEDOUT;
static NB(I32) SY(_ECONNREFUSED) = ECONNREFUSED;
static NB(I32) SY(_EHOSTDOWN) = EHOSTDOWN;
static NB(I32) SY(_EHOSTUNREACH) = EHOSTUNREACH;
static NB(I32) SY(_EALREADY) = EALREADY;
static NB(I32) SY(_EINPROGRESS) = EINPROGRESS;
static NB(I32) SY(_ESTALE) = ESTALE;
static NB(I32) SY(_EUCLEAN) = EUCLEAN;
static NB(I32) SY(_ENOTNAM) = ENOTNAM;
static NB(I32) SY(_ENAVAIL) = ENAVAIL;
static NB(I32) SY(_EISNAM) = EISNAM;
static NB(I32) SY(_EREMOTEIO) = EREMOTEIO;
static NB(I32) SY(_EDQUOT) = EDQUOT;
static NB(I32) SY(_ENOMEDIUM) = ENOMEDIUM;
static NB(I32) SY(_EMEDIUMTYPE) = EMEDIUMTYPE;
static NB(I32) SY(_ECANCELED) = ECANCELED;
static NB(I32) SY(_ENOKEY) = ENOKEY;
static NB(I32) SY(_EKEYEXPIRED) = EKEYEXPIRED;
static NB(I32) SY(_EKEYREVOKED) = EKEYREVOKED;
static NB(I32) SY(_EKEYREJECTED) = EKEYREJECTED;
static NB(I32) SY(_EOWNERDEAD) = EOWNERDEAD;
static NB(I32) SY(_ENOTRECOVERABLE) = ENOTRECOVERABLE;
static NB(I32) SY(_ERFKILL) = ERFKILL;
static NB(I32) SY(_EHWPOISON) = EHWPOISON;

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
  int ret = openat(dirfd, (char *) pathname, flags, mode);
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

static NB(Int) SY(mkdirat)(NB(Int) dirfd, NB(U8) *pathname, SY(Mode) mode) {
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

static NB(Int) SY(fstatat)(NB(Int) dirfd, NB(U8) *pathname, SY(Stat_t) *buf, NB(I32) flags) {
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

#endif

#undef SY
#undef NB
