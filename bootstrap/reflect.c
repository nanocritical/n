#include "reflect.h"
#include "types.h"

static size_t Dynisalist_hashmap_addr(struct __Dynisalist *self, uint32_t hash, size_t nth) {
  return (hash + (nth * (nth + 1)/2)) % self->hashmap.cnt;
}

static size_t Dynisalist_find_entry(struct __Dynisalist *self, struct __Type *rintf) {
  uint32_t hash = rintf->typename_hash32;
  size_t nth = 0, addr = 0;
  while (true) {
    addr = Dynisalist_hashmap_addr(self, hash, nth);
    uint16_t pos = self->hashmap.dat[addr];
    if (pos == 0) {
      break;
    }

    struct __entry *e = &self->entries.dat[pos - 1];
    if (e->typename_hash32 != hash) {
      nth += 1;
      continue;
    }

    if (e->Typename.bytes.cnt == rintf->Typename.bytes.cnt
        && memcmp(e->Typename.bytes.dat, rintf->Typename.bytes.dat,
                  e->Typename.bytes.cnt) == 0) {
      assert(false && "conflicting entry");
    }

    nth += 1;
  }

  return addr;
}

static void Dynisalist_Init(struct __Dynisalist *self, size_t count) {
  size_t table_size = roundup_pow2(count);
  if (3*table_size < 4*count) {
    table_size *= 2;
  }
  self->hashmap = (struct __Slice_u16){
    .dat = calloc(table_size, sizeof(uint16_t)),
    .cnt = table_size,
    .cap = table_size,
  };
  self->entries = (struct __Slice_entry){
    .dat = calloc(count, sizeof(struct __entry)),
    .cnt = 0,
    .cap = count,
  };
}

static void Dynisalist_Add_entry(struct __Dynisalist *self, struct __Type *rintf,
                                 void *dyntable) {
  const size_t addr = Dynisalist_find_entry(self, rintf);
  self->entries.cnt += 1;
  const size_t pos = self->entries.cnt;
  self->hashmap.dat[addr] = pos;
  struct __entry *e = &self->entries.dat[pos - 1];
  e->typename_hash32 = rintf->typename_hash32;
  e->Typename = rintf->Typename;
  e->dyntable = dyntable;
}

static struct __Type mk_minimal_type(const struct module *mod, const struct typ *t) {
  struct __Type type = { 0 };
  char *tn = pptyp(mod, t);
  type.typename_hash32 = hash32_n(tn, strlen(tn));
  type.Typename = (struct __String){ .bytes = (struct __Slice_u8){
    .dat = (uint8_t *)tn, .cnt = strlen(tn), .cap = strlen(tn) } };
  return type;
}

struct state {
  const struct module *mod;
  size_t count;
  struct __Type *type;
};

static ERROR count_isa_each(struct module *mod, struct typ *t, struct typ *intf,
                            bool *stop, void *user) {
  struct state *st = user;
  if (typ_is_reference(intf)) {
    return 0;
  }
  st->count += 1;
  return 0;
}

static ERROR add_entry_each(struct module *mod, struct typ *t, struct typ *intf,
                            bool *stop, void *user) {
  struct state *st = user;
  if (typ_is_reference(intf)) {
    return 0;
  }
  struct __Type rintf = mk_minimal_type(mod, intf);
  Dynisalist_Add_entry(&st->type->dynisalist, &rintf, intf);
  return 0;
}

void reflect_fill_type(struct __Type *type,
                       const struct module *mod, const struct typ *t) {
  *type = mk_minimal_type(mod, t);

  struct state st = {
    .mod = mod,
    .count = 0,
    .type = type,
  };

  const bool zero = typ_isalist_foreach(CONST_CAST(mod), CONST_CAST(t),
                                        ISALIST_FILTEROUT_PREVENT_DYN,
                                        count_isa_each, &st);
  assert(!zero);

  Dynisalist_Init(&type->dynisalist, st.count);

  const bool zero2 = typ_isalist_foreach(CONST_CAST(mod), CONST_CAST(t),
                                         ISALIST_FILTEROUT_PREVENT_DYN,
                                         add_entry_each, &st);
  assert(!zero2);
}
