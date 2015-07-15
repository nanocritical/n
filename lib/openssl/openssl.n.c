#ifdef NLANG_DEFINE_FUNCTIONS

n$builtins$Uint openssl$EVP_MAX_MD_SIZE = EVP_MAX_MD_SIZE;

struct openssl$EVP_MD *openssl$EVP_md_null(void) { return (EVP_MD *) EVP_md_null(); }
#ifndef OPENSSL_NO_MD2
struct openssl$EVP_MD *openssl$EVP_md2(void) { return (EVP_MD *) EVP_md2(); }
#else
struct openssl$EVP_MD *openssl$EVP_md2(void) { return NULL; }
#endif

#ifndef OPENSSL_NO_MD4
struct openssl$EVP_MD *openssl$EVP_md4(void) { return (EVP_MD *) EVP_md4(); }
#else
struct openssl$EVP_MD *openssl$EVP_md4(void) { return NULL; }
#endif

#ifndef OPENSSL_NO_MD5
struct openssl$EVP_MD *openssl$EVP_md5(void) { return (EVP_MD *) EVP_md5(); }
#else
struct openssl$EVP_MD *openssl$EVP_md5(void) { return NULL; }
#endif

#ifndef OPENSSL_NO_SHA
struct openssl$EVP_MD *openssl$EVP_sha(void) { return (EVP_MD *) EVP_sha(); }
struct openssl$EVP_MD *openssl$EVP_sha1(void) { return (EVP_MD *) EVP_sha1(); }
struct openssl$EVP_MD *openssl$EVP_dss(void) { return (EVP_MD *) EVP_dss(); }
struct openssl$EVP_MD *openssl$EVP_dss1(void) { return (EVP_MD *) EVP_dss1(); }
struct openssl$EVP_MD *openssl$EVP_ecdsa(void) { return (EVP_MD *) EVP_ecdsa(); }
#else
struct openssl$EVP_MD *openssl$EVP_sha(void) { return NULL; }
struct openssl$EVP_MD *openssl$EVP_sha1(void) { return NULL; }
struct openssl$EVP_MD *openssl$EVP_dss(void) { return NULL; }
struct openssl$EVP_MD *openssl$EVP_dss1(void) { return NULL; }
struct openssl$EVP_MD *openssl$EVP_ecdsa(void) { return NULL; }
#endif

#ifndef OPENSSL_NO_SHA256
struct openssl$EVP_MD *openssl$EVP_sha224(void) { return (EVP_MD *) EVP_sha224(); }
struct openssl$EVP_MD *openssl$EVP_sha256(void) { return (EVP_MD *) EVP_sha256(); }
#else
struct openssl$EVP_MD *openssl$EVP_sha224(void) { return NULL; }
struct openssl$EVP_MD *openssl$EVP_sha256(void) { return NULL; }
#endif

#ifndef OPENSSL_NO_SHA512
struct openssl$EVP_MD *openssl$EVP_sha384(void) { return (EVP_MD *) EVP_sha384(); }
struct openssl$EVP_MD *openssl$EVP_sha512(void) { return (EVP_MD *) EVP_sha512(); }
#else
struct openssl$EVP_MD *openssl$EVP_sha384(void) { return NULL; }
struct openssl$EVP_MD *openssl$EVP_sha512(void) { return NULL; }
#endif

#ifndef OPENSSL_NO_MDC2
struct openssl$EVP_MD *openssl$EVP_mdc2(void) { return (EVP_MD *) EVP_mdc2(); }
#else
struct openssl$EVP_MD *openssl$EVP_mdc2(void) { return NULL; }
#endif

#ifndef OPENSSL_NO_RIPEMD
struct openssl$EVP_MD *openssl$EVP_ripemd160(void) { return (EVP_MD *) EVP_ripemd160(); }
#else
struct openssl$EVP_MD *openssl$EVP_ripemd160(void) { return NULL; }
#endif

#ifndef OPENSSL_NO_WHIRLPOOL
struct openssl$EVP_MD *openssl$EVP_whirlpool(void) { return (EVP_MD *) EVP_whirlpool(); }
#else
struct openssl$EVP_MD *openssl$EVP_ripemd160(void) { return NULL; }
#endif

static struct openssl$EVP_MD *openssl$_EVP_get_digestbyname(n$builtins$U8 *name) {
  return (EVP_MD *) EVP_get_digestbyname((char *) name);
}

// defines can only be used for non-exported N declarations (directly mapped
// to openssl definitions), which in practice is when we need to write a N
// wrapper. For exported symbols that need no wrapper, we still need a
// openssl$xxx symbol to be emitted (for outside modules that wish to link
// with this module), so we define it here.

void openssl$EVP_MD_CTX_init(EVP_MD_CTX *ctx) { EVP_MD_CTX_init(ctx); }
EVP_MD_CTX *openssl$EVP_MD_CTX_create(void) { return EVP_MD_CTX_create(); }
#define openssl$_EVP_DigestInit_ex EVP_DigestInit_ex
#define openssl$_EVP_DigestUpdate EVP_DigestUpdate
#define openssl$_EVP_DigestFinal_ex EVP_DigestFinal_ex
#define openssl$_EVP_MD_CTX_cleanup EVP_MD_CTX_cleanup
void openssl$EVP_MD_CTX_destroy(EVP_MD_CTX *ctx) { EVP_MD_CTX_destroy(ctx); }
#define openssl$_EVP_MD_CTX_copy_ex EVP_MD_CTX_copy_ex
#define openssl$_EVP_DigestInit EVP_DigestInit
#define openssl$_EVP_DigestFinal EVP_DigestFinal
#define openssl$_EVP_MD_CTX_copy EVP_MD_CTX_copy
int openssl$EVP_MD_type(EVP_MD *md) { return EVP_MD_type(md); }
int openssl$EVP_MD_pkey_type(EVP_MD *md) { return EVP_MD_pkey_type(md); }
#define openssl$_EVP_MD_size EVP_MD_size
#define openssl$_EVP_MD_block_size EVP_MD_block_size

struct openssl$EVP_MD *openssl$EVP_MD_CTX_md(EVP_MD_CTX *md) {
  return (EVP_MD *) EVP_MD_CTX_md(md);
}

static int openssl$_EVP_MD_CTX_size(EVP_MD_CTX *ctx) { return EVP_MD_CTX_size(ctx); }
static int openssl$_EVP_MD_CTX_block_size(EVP_MD_CTX *ctx) { return EVP_MD_CTX_block_size(ctx); }
int openssl$EVP_MD_CTX_type(EVP_MD_CTX *ctx) { return EVP_MD_CTX_type(ctx); }

#define openssl$_HMAC HMAC
#define openssl$_HMAC_CTX_init HMAC_CTX_init
#define openssl$_HMAC_Init_ex HMAC_Init_ex
#define openssl$_HMAC_Update HMAC_Update
#define openssl$_HMAC_Final HMAC_Final
#define openssl$_HMAC_CTX_cleanup HMAC_CTX_cleanup

#endif
