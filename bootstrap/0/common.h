#ifndef COMMON_H__
#define COMMON_H__

#include <stdint.h>
#include <stdio.h>
#include <errno.h>
#include <assert.h>

typedef _Bool bool;
#define TRUE 1
#define FALSE 0

typedef int error;

#define EXCEPT(e) do { \
  if (e) { \
    fprintf(stderr, "\tE: %s:%d:%s()\n", __FILE__, __LINE__, __func__); \
    return e; \
  } \
} while (0)

#define EXCEPTF(e, fmt, ...) do { \
  if (e) { \
    fprintf(stderr, fmt "\n", ##__VA_ARGS__); \
    fprintf(stderr, "\tE: %s:%d:%s(): %s\n", __FILE__, __LINE__, __func__, strerror(e)); \
    return e; \
  } \
} while (0)

#define ARRAY_SIZE(a) ( sizeof(a) / sizeof(a[0]) )

#endif
