#include "linemap.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int linemap_read(struct linemap *linemap, const char *fn) {
  memset(linemap, 0, sizeof(*linemap));

  int fd = open(fn, O_RDONLY);
  if (fd == -1) {
    return -errno;
  }

  struct stat st = { 0 };
  int ret = fstat(fd, &st);
  if (ret == -1) {
    close(fd);
    return -errno;
  }

  linemap->len = st.st_size;
  linemap->data = calloc(linemap->len, sizeof(char));

  ssize_t len = read(fd, linemap->data, linemap->len);
  if (len != linemap->len) {
    close(fd);
    linemap_destroy(linemap);
    return -errno;
  }

  linemap->max_src_line = 8192;
  linemap->entries = calloc(linemap->max_src_line, sizeof(struct linemap_entry));

  strmap_init(&linemap->file_ids, 20);

  char *p = linemap->data;
  while (p != linemap->data + linemap->len) {
    int32_t src_line = 0;
    struct linemap_entry entry = { 0 };

    int start_name = 0, entry_len = 0;
    int ret = sscanf(p, "%d %d %n%*s%n\n", &src_line, &entry.dst_line, &start_name, &entry_len);
    if (ret != 2) {
      fprintf(stderr, "format error in linemap file '%s'\n", fn);
      linemap_destroy(linemap);
      return -EINVAL;
    }

    char *dst_file = p + start_name;
    assert(p[entry_len] == '\n');
    p[entry_len] = '\0';

    uint32_t *dst_file_id = strmap_get(&linemap->file_ids, dst_file);
    if (dst_file_id == NULL) {
      entry.dst_file_id = linemap->file_ids_count;
      strmap_set(&linemap->file_ids, dst_file, entry.dst_file_id);
      linemap->file_ids_count += 1;
      linemap->file_names = realloc(linemap->file_names,
                                    linemap->file_ids_count * sizeof(*linemap->file_names));
      linemap->file_names[entry.dst_file_id] = dst_file;

    } else {
      entry.dst_file_id = *dst_file_id;
    }

    if (src_line > linemap->max_src_line) {
      const size_t old_max_src_line = linemap->max_src_line;
      linemap->max_src_line = 2 * src_line;
      linemap->entries = realloc(linemap->entries, linemap->max_src_line * sizeof(struct linemap_entry));
      memset(linemap->entries + old_max_src_line, 0, linemap->max_src_line - old_max_src_line);
    }

    linemap->entries[src_line] = entry;

    p += entry_len + 1;
  }

  return 0;
}

void linemap_destroy(struct linemap *linemap) {
  free(linemap->data);
  free(linemap->entries);
  strmap_destroy(&linemap->file_ids);
  memset(linemap, 0, sizeof(*linemap));
}

#ifdef LINEMAP_STANDALONE

int main(int argc, char **argv) {
  struct linemap linemap = { 0 };
  int ret = linemap_read(&linemap, argv[1]);
  if (ret < 0) {
    return ret;
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
