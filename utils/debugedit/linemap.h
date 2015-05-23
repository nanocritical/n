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
  uint32_t next_file_id;
  char **file_names; // points in `data`
  size_t file_names_needed_space;
};

int linemap_read(struct linemap *linemap, const char *dir, const char *base_fn,
                 uint32_t primary_file, uint32_t first_additional_n_file);
void linemap_destroy(struct linemap *linemap);
int32_t linemap_lookup(uint32_t *file, const struct linemap *linemap, int32_t src_line);
