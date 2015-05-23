#include "linemap.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int linemap_read(struct linemap *linemap, const char *dir, const char *base_fn,
                 uint32_t primary_file, uint32_t first_additional_n_file) {
  if (base_fn == NULL) {
    return -1;
  }

  assert(primary_file != first_additional_n_file);

  memset(linemap, 0, sizeof(*linemap));

  dir = dir != NULL ? dir : ".";
  char *fn = calloc(strlen(dir) + 1 + strlen(base_fn) + sizeof(".linemap"), sizeof(char));
  if (strlen(dir) > 0) {
    sprintf(fn, "%s/%s.linemap", dir, base_fn);
  } else {
    sprintf(fn, "%s.linemap", base_fn);
  }

  int fd = open(fn, O_RDONLY);
  if (fd == -1) {
    fprintf(stderr, "Warning: error opening linemap file '%s'\n", fn);
    return -errno;
  }

  struct stat st = { 0 };
  int ret = fstat(fd, &st);
  if (ret == -1) {
    fprintf(stderr, "Warning: error stating linemap file '%s'\n", fn);
    close(fd);
    return -errno;
  }

  linemap->len = st.st_size;
  linemap->data = calloc(linemap->len + 1, sizeof(char));

  ssize_t len = read(fd, linemap->data, linemap->len);
  if (len != linemap->len) {
    close(fd);
    linemap_destroy(linemap);
    free(fn);
    return -errno;
  }

  linemap->max_src_line = 8192;
  linemap->entries = calloc(1 + linemap->max_src_line, sizeof(*linemap->entries));

  linemap->file_names = calloc(first_additional_n_file, sizeof(*linemap->file_names));

  strmap_init(&linemap->file_ids, 20);

  char *p = linemap->data;
  while (p != linemap->data + linemap->len) {
    int32_t src_line = 0;
    struct linemap_entry entry = { 0 };

    int start_name = 0, entry_len = 0;
    int ret = sscanf(p, "%d %d %n%*s%n\n", &src_line, &entry.dst_line, &start_name, &entry_len);
    if (ret != 2) {
      fprintf(stderr, "Warning: format error in linemap file '%s'\n", fn);
      linemap_destroy(linemap);
      free(fn);
      return -EINVAL;
    }

    char *dst_file = p + start_name;
    assert(p[entry_len] == '\n');
    p[entry_len] = '\0';

    uint32_t *dst_file_id = strmap_get(&linemap->file_ids, dst_file);
    if (dst_file_id == NULL) {
      if (linemap->next_file_id == 0) {
        // First line, first file to appear is the primary N source.
        // Line of the form "1 1 primary.n"
        assert(src_line == 1 && entry.dst_line == 1);
        entry.dst_file_id = primary_file;
        linemap->next_file_id = first_additional_n_file;
      } else {
        entry.dst_file_id = linemap->next_file_id;
        linemap->next_file_id += 1;
      }

      strmap_set(&linemap->file_ids, dst_file, entry.dst_file_id);
      linemap->file_names = realloc(linemap->file_names,
                                    linemap->next_file_id * sizeof(*linemap->file_names));
      linemap->file_names[entry.dst_file_id] = dst_file;
      linemap->file_names_needed_space += strlen(dst_file) + 1 + 3*4 + 1;

    } else {
      entry.dst_file_id = *dst_file_id;
    }

    if (src_line >= linemap->max_src_line) {
      const size_t old_max_src_line = linemap->max_src_line;
      linemap->max_src_line = 2 * src_line;
      linemap->entries = realloc(linemap->entries, (1+linemap->max_src_line) * sizeof(*linemap->entries));
      memset(linemap->entries + old_max_src_line, 0,
             (1 + linemap->max_src_line - old_max_src_line) * sizeof(*linemap->entries));
    }

    linemap->entries[src_line] = entry;

    p += entry_len + 1;
  }

  free(fn);
  return 0;
}

void linemap_destroy(struct linemap *linemap) {
  free(linemap->data);
  free(linemap->entries);
  free(linemap->file_names);
  strmap_destroy(&linemap->file_ids);
  memset(linemap, 0, sizeof(*linemap));
}

int32_t linemap_lookup(uint32_t *file, const struct linemap *linemap, int32_t src_line) {
  if (linemap->data == NULL) {
    return src_line;
  }

  if (src_line > linemap->max_src_line) {
    src_line = linemap->max_src_line;
  }

  do {
    struct linemap_entry *entry = &linemap->entries[src_line];
    if (entry->dst_line != 0) {
      *file = entry->dst_file_id;
      return entry->dst_line;
    }
  } while (--src_line >= 0);

  assert(false && "Unreached, there is at least one entry for line 1");
}

#ifdef LINEMAP_STANDALONE

int main(int argc, char **argv) {
  if (argc != 2) {
    return 1;
  }

  struct linemap linemap = { 0 };
  int ret = linemap_read(&linemap, NULL, argv[1], 1, 2);
  if (ret < 0) {
    return -ret;
  }

  for (size_t src_line = 0; src_line <= linemap.max_src_line; ++src_line) {
    struct linemap_entry *entry = &linemap.entries[src_line];
    if (entry->dst_line == 0) {
      continue;
    }

    fprintf(stdout, "%zd %d %s\n", src_line, entry->dst_line, linemap.file_names[entry->dst_file_id]);
  }

  return 0;
}

#endif
