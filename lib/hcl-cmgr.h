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

#ifndef _HCL_CMGR_H_
#define _HCL_CMGR_H_

#include <hcl-cmn.h>

enum hcl_cmgr_id_t
{
	HCL_CMGR_UTF8,
	HCL_CMGR_UTF16,
	HCL_CMGR_MB8
};
typedef enum hcl_cmgr_id_t hcl_cmgr_id_t;


#if defined(__cplusplus)
extern "C" {
#endif

HCL_EXPORT hcl_cmgr_t* hcl_get_utf8_cmgr (
	void
);

HCL_EXPORT hcl_cmgr_t* hcl_get_cmgr_by_id (
	hcl_cmgr_id_t id
);

HCL_EXPORT hcl_cmgr_t* hcl_get_cmgr_by_bcstr (
	const hcl_bch_t* name
);

HCL_EXPORT hcl_cmgr_t* hcl_get_cmgr_by_ucstr (
	const hcl_uch_t* name
);

#if defined(HCL_OOCH_IS_UCH)
#	define hcl_get_cmgr_by_name(name) hcl_get_cmgr_by_ucstr(name)
#else
#	define hcl_get_cmgr_by_name(name) hcl_get_cmgr_by_bcstr(name)
#endif

#define hcl_get_utf8_cmgr() hcl_get_cmgr_by_id(HCL_CMGR_UTF8)
#define hcl_get_utf16_cmgr() hcl_get_cmgr_by_id(HCL_CMGR_UTF16)
#define hcl_get_mb8_cmgr() hcl_get_cmgr_by_id(HCL_CMGR_MB8)


/* ------------------------------------------------------------------------- */

/**
 * The hcl_conv_uchars_to_utf8() function converts a unicode character string \a ucs
 * to a UTF8 string and writes it into the buffer pointed to by \a bcs, but
 * not more than \a bcslen bytes including the terminating null.
 *
 * Upon return, \a bcslen is modified to the actual number of bytes written to
 * \a bcs excluding the terminating null; \a ucslen is modified to the number of
 * wide characters converted.
 *
 * You may pass #HCL_NULL for \a bcs to dry-run conversion or to get the
 * required buffer size for conversion. -2 is never returned in this case.
 *
 * \return
 * - 0 on full conversion,
 * - -1 on no or partial conversion for an illegal character encountered,
 * - -2 on no or partial conversion for a small buffer.
 *
 * \code
 *   const hcl_uch_t ucs[] = { 'H', 'e', 'l', 'l', 'o' };
 *   hcl_bch_t bcs[10];
 *   hcl_oow_t ucslen = 5;
 *   hcl_oow_t bcslen = HCL_COUNTOF(bcs);
 *   n = hcl_conv_uchars_to_utf8 (ucs, &ucslen, bcs, &bcslen);
 *   if (n <= -1)
 *   {
 *      // conversion error
 *   }
 * \endcode
 */
HCL_EXPORT int hcl_conv_uchars_to_utf8 (
	const hcl_uch_t*    ucs,
	hcl_oow_t*          ucslen,
	hcl_bch_t*          bcs,
	hcl_oow_t*          bcslen
);

/**
 * The hcl_conv_utf8_to_uchars() function converts a UTF8 string to a uncide string.
 *
 * It never returns -2 if \a ucs is #HCL_NULL.
 *
 * \code
 *  const hcl_bch_t* bcs = "test string";
 *  hcl_uch_t ucs[100];
 *  hcl_oow_t ucslen = HCL_COUNTOF(buf), n;
 *  hcl_oow_t bcslen = 11;
 *  int n;
 *  n = hcl_conv_utf8_to_uchars (bcs, &bcslen, ucs, &ucslen);
 *  if (n <= -1) { invalid/incomplenete sequence or buffer to small }
 * \endcode
 *
 * The resulting \a ucslen can still be greater than 0 even if the return
 * value is negative. The value indiates the number of characters converted
 * before the error has occurred.
 *
 * \return 0 on success.
 *         -1 if \a bcs contains an illegal character.
 *         -2 if the wide-character string buffer is too small.
 *         -3 if \a bcs is not a complete sequence.
 */
HCL_EXPORT int hcl_conv_utf8_to_uchars (
	const hcl_bch_t*   bcs,
	hcl_oow_t*         bcslen,
	hcl_uch_t*         ucs,
	hcl_oow_t*         ucslen
);


HCL_EXPORT int hcl_conv_ucstr_to_utf8 (
	const hcl_uch_t*    ucs,
	hcl_oow_t*          ucslen,
	hcl_bch_t*          bcs,
	hcl_oow_t*          bcslen
);

HCL_EXPORT int hcl_conv_utf8_to_ucstr (
	const hcl_bch_t*   bcs,
	hcl_oow_t*         bcslen,
	hcl_uch_t*         ucs,
	hcl_oow_t*         ucslen
);
#if defined(__cplusplus)
}
#endif

#endif
