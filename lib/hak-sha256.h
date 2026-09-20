/*
    Copyright (c) 2016-2018 Chung, Hyung-Hwan. All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:
    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.
    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
    IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
    OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
    IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
    NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
    THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef HAK_SHA256_H
#define HAK_SHA256_H

#include <hak.h>

#define HAK_SHA256_DIGEST_LEN (32)

struct hak_sha256_ctx_t
{
    hak_uint8_t data[64];
    hak_uint8_t datalen;

    hak_uint32_t bitlen_lo; /* for systems that don't have 64 bit integer types */
    hak_uint32_t bitlen_hi;

    hak_uint32_t state[8];
};

typedef struct hak_sha256_ctx_t hak_sha256_ctx_t;

#if defined(__cplusplus)
extern "C" {
#endif

HAK_EXPORT void hak_sha256_init (
	hak_sha256_ctx_t* ctx
);

HAK_EXPORT void hak_sha256_update (
	hak_sha256_ctx_t*  ctx,
	const void*        data,
	hak_oow_t          len
);

HAK_EXPORT void hak_sha256_final (
	hak_sha256_ctx_t* ctx,
	hak_uint8_t       hash[HAK_SHA256_DIGEST_LEN]
);

/* convenience function for quick digestion */
HAK_EXPORT void hak_sha256_digest (
	hak_uint8_t       hash[HAK_SHA256_DIGEST_LEN],
	const void*       data,
	hak_oow_t         dlen
);

#if defined(__cplusplus)
}
#endif

#endif

