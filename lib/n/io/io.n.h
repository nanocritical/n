#ifdef NLANG_DEFINE_FUNCTIONS

static inline void n$io$write_le_u8(n$builtins$U8 *b, n$builtins$U8 n) { memcpy(b, &n, sizeof(n)); }
static inline void n$io$write_le_u16(n$builtins$U8 *b, n$builtins$U16 n) { memcpy(b, &n, sizeof(n)); }
static inline void n$io$write_le_u32(n$builtins$U8 *b, n$builtins$U32 n) { memcpy(b, &n, sizeof(n)); }
static inline void n$io$write_le_u64(n$builtins$U8 *b, n$builtins$U64 n) { memcpy(b, &n, sizeof(n)); }

static inline n$builtins$U8 n$io$read_le_u8(n$builtins$U8 *b) { n$builtins$U8 n; memcpy(&n, b, sizeof(n)); return n; }
static inline n$builtins$U16 n$io$read_le_u16(n$builtins$U8 *b) { n$builtins$U16 n; memcpy(&n, b, sizeof(n)); return n; }
static inline n$builtins$U32 n$io$read_le_u32(n$builtins$U8 *b) { n$builtins$U32 n; memcpy(&n, b, sizeof(n)); return n; }
static inline n$builtins$U64 n$io$read_le_u64(n$builtins$U8 *b) { n$builtins$U64 n; memcpy(&n, b, sizeof(n)); return n; }

#endif
