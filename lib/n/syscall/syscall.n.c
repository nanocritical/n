#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
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

NB(U32) SY(O_RDONLY) = O_RDONLY;
NB(U32) SY(O_WRONLY) = O_WRONLY;
NB(U32) SY(O_RDWR) = O_RDWR;
NB(U32) SY(O_APPEND) = O_APPEND;
NB(U32) SY(O_ASYNC) = O_ASYNC;
NB(U32) SY(O_CLOEXEC) = O_CLOEXEC;
NB(U32) SY(O_CREAT) = O_CREAT;
NB(U32) SY(O_DIRECT) = O_DIRECT;
NB(U32) SY(O_DIRECTORY) = O_DIRECTORY;
NB(U32) SY(O_DSYNC) = O_DSYNC;
NB(U32) SY(O_EXCL) = O_EXCL;
NB(U32) SY(O_LARGEFILE) = O_LARGEFILE;
NB(U32) SY(O_NOATIME) = O_NOATIME;
NB(U32) SY(O_NOCTTY) = O_NOCTTY;
NB(U32) SY(O_NOFOLLOW) = O_NOFOLLOW;
NB(U32) SY(O_NONBLOCK) = O_NONBLOCK;
NB(U32) SY(O_NDELAY) = O_NDELAY;
NB(U32) SY(O_PATH) = O_PATH;
NB(U32) SY(O_SYNC) = O_SYNC;
NB(U32) SY(O_TMPFILE) = O_TMPFILE;
NB(U32) SY(O_TRUNC) = O_TRUNC;

NB(Int) SY(SEEK_SET) = SEEK_SET;
NB(Int) SY(SEEK_CUR) = SEEK_CUR;
NB(Int) SY(SEEK_END) = SEEK_END;
NB(Int) SY(SEEK_DATA) = SEEK_DATA;
NB(Int) SY(SEEK_HOLE) = SEEK_HOLE;

static NB(I32) SY(errno)(void) {
  return errno;
}

static NB(Int) SY(close)(NB(Int) fd) {
  return close(fd);
}

static NB(Int) SY(open)(NB(U8) *pathname, NB(U32) flags, NB(U32) mode) {
  return open((char *) pathname, flags, mode);
}

static NB(Int) SY(openat)(NB(Int) dirfd, NB(U8) *pathname, NB(U32) flags, NB(U32) mode) {
  return openat(dirfd, (char *) pathname, flags, mode);
}

static NB(Int) SY(mkfifoat)(NB(Int) dirfd, NB(U8) *pathname, NB(U32) mode) {
  return mkfifoat(dirfd, (char *) pathname, mode);
}

static NB(Int) SY(write)(NB(Int) fd, NB(U8) *buf, NB(Uint) count) {
  return write(fd, buf, count);
}

static NB(Int) SY(read)(NB(Int) fd, NB(U8) *buf, NB(Uint) count) {
  return read(fd, buf, count);
}

static NB(Int) SY(lseek)(NB(Int) fd, NB(Int) off, NB(Int) whence) {
  return lseek(fd, off, whence);
}

#endif

#undef SY
#undef NB
