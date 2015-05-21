#include "../../bootstrap/table.h"
#include "../../bootstrap/hash.h"

#include <stdint.h>

HTABLE_SPARSE(strmap, uint32_t, char *);

static inline uint32_t strhashf(const char **a) {
  return hash32(*a, strlen(*a));
}

static inline int strcmpf(const char **a, const char **b) {
  return strcmp(*a, *b);
}

IMPLEMENT_HTABLE_SPARSE(static inline, strmap, uint32_t, char *, strhashf, strcmpf);

struct linemap_entry {
  int32_t dst_line;
  uint32_t dst_file_id;
};

struct linemap {
  char *data;
  size_t len;

  struct linemap_entry *entries; // length: max_src_line + 1
  size_t max_src_line;

  struct strmap file_ids; // keys points in `data`
  uint32_t file_ids_count;
  char **file_names; // points in `data`
};

int linemap_read(struct linemap *linemap, const char *fn);
void linemap_destroy(struct linemap *linemap);
