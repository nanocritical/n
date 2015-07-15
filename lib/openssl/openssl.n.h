#include <openssl/evp.h>
#include <openssl/hmac.h>

#ifdef NLANG_DEFINE_TYPES

#define openssl$EVP_MD env_md_st
#define openssl$HMAC_CTX hmac_ctx_st
#define openssl$ENGINE engine_st

#endif
