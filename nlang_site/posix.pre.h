#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

typedef struct stat posix_stat_t;
NLANG_POINTER_ALIASES(posix_stat_t);

typedef struct timeval posix_timeval;
NLANG_POINTER_ALIASES(posix_timeval);
