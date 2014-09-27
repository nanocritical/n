#ifndef NLANG_REFLECT_H__
#define NLANG_REFLECT_H__

// These definitions are for the benefit of bootstrap/reflect.h (compiler),
// and runtime.h and the C printer (codegen).
//
// They must match those in lib/n/reflect.n.
// Codegen doesn't use reflect.n output directly to avoid a dependency of
// all generated code on reflect.n; and since the compiler needs to have a C
// version of these definitions anyway...

struct __Slice_u8 {
  uint8_t *dat;
  size_t cnt;
  size_t cap;
};

struct __Slice_u16 {
  uint16_t *dat;
  size_t cnt;
  size_t cap;
};

struct __String {
  struct __Slice_u8 bytes;
};

struct __entry {
  uint32_t typename_hash32;
  struct __String Typename;
  const void *dyntable; // In the compiler, used to store a struct typ * to the intf.
};

struct __Slice_entry {
  struct __entry *dat;
  size_t cnt;
  size_t cap;
};

struct __Dynisalist {
  struct __Slice_u16 hashmap;
  struct __Slice_entry entries;
};

struct __Type {
  uint32_t typename_hash32;
  struct __String Typename;
  struct __Dynisalist dynisalist;
};

#endif
