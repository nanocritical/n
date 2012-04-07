inline void nlang_slice__unsafe_slice_clear(nlangp__u8 data, size elsize,
                                                  size start, size last) {
  memset(data + (start * elsize), 0, elsize * (last - start + 1));
}

inline nlangp__u8 nlang_slice__unsafe_slice_addr(nlangp__u8 data, size elsize,
                                                       size n) {
  return data + elsize * n;
}
