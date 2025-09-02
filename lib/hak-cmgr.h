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

#ifndef _HAK_CMGR_H_
#define _HAK_CMGR_H_

#include <hak-cmn.h>

enum hak_cmgr_id_t
{
	HAK_CMGR_UTF8,
	HAK_CMGR_UTF16,
	HAK_CMGR_MB8
};
typedef enum hak_cmgr_id_t hak_cmgr_id_t;


#if defined(__cplusplus)
extern "C" {
#endif

HAK_EXPORT hak_cmgr_t* hak_get_utf8_cmgr (
	void
);

HAK_EXPORT hak_cmgr_t* hak_get_cmgr_by_id (
	hak_cmgr_id_t id
);

HAK_EXPORT hak_cmgr_t* hak_get_cmgr_by_bcstr (
	const hak_bch_t* name
);

HAK_EXPORT hak_cmgr_t* hak_get_cmgr_by_ucstr (
	const hak_uch_t* name
);

#if defined(HAK_OOCH_IS_UCH)
#	define hak_get_cmgr_by_name(name) hak_get_cmgr_by_ucstr(name)
#else
#	define hak_get_cmgr_by_name(name) hak_get_cmgr_by_bcstr(name)
#endif

#define hak_get_utf8_cmgr() hak_get_cmgr_by_id(HAK_CMGR_UTF8)
#define hak_get_utf16_cmgr() hak_get_cmgr_by_id(HAK_CMGR_UTF16)
#define hak_get_mb8_cmgr() hak_get_cmgr_by_id(HAK_CMGR_MB8)


/* ------------------------------------------------------------------------- */

/**
 * The hak_conv_uchars_to_utf8() function converts a unicode character string \a ucs
 * to a UTF8 string and writes it into the buffer pointed to by \a bcs, but
 * not more than \a bcslen bytes including the terminating null.
 *
 * Upon return, \a bcslen is modified to the actual number of bytes written to
 * \a bcs excluding the terminating null; \a ucslen is modified to the number of
 * wide characters converted.
 *
 * You may pass #HAK_NULL for \a bcs to dry-run conversion or to get the
 * required buffer size for conversion. -2 is never returned in this case.
 *
 * \return
 * - 0 on full conversion,
 * - -1 on no or partial conversion for an illegal character encountered,
 * - -2 on no or partial conversion for a small buffer.
 *
 * \code
 *   const hak_uch_t ucs[] = { 'H', 'e', 'l', 'l', 'o' };
 *   hak_bch_t bcs[10];
 *   hak_oow_t ucslen = 5;
 *   hak_oow_t bcslen = HAK_COUNTOF(bcs);
 *   n = hak_conv_uchars_to_utf8 (ucs, &ucslen, bcs, &bcslen);
 *   if (n <= -1)
 *   {
 *      // conversion error
 *   }
 * \endcode
 */
HAK_EXPORT int hak_conv_uchars_to_utf8 (
	const hak_uch_t*    ucs,
	hak_oow_t*          ucslen,
	hak_bch_t*          bcs,
	hak_oow_t*          bcslen
);

/**
 * The hak_conv_utf8_to_uchars() function converts a UTF8 string to a uncide string.
 *
 * It never returns -2 if \a ucs is #HAK_NULL.
 *
 * \code
 *  const hak_bch_t* bcs = "test string";
 *  hak_uch_t ucs[100];
 *  hak_oow_t ucslen = HAK_COUNTOF(buf), n;
 *  hak_oow_t bcslen = 11;
 *  int n;
 *  n = hak_conv_utf8_to_uchars (bcs, &bcslen, ucs, &ucslen);
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
HAK_EXPORT int hak_conv_utf8_to_uchars (
	const hak_bch_t*   bcs,
	hak_oow_t*         bcslen,
	hak_uch_t*         ucs,
	hak_oow_t*         ucslen
);


HAK_EXPORT int hak_conv_ucstr_to_utf8 (
	const hak_uch_t*    ucs,
	hak_oow_t*          ucslen,
	hak_bch_t*          bcs,
	hak_oow_t*          bcslen
);

HAK_EXPORT int hak_conv_utf8_to_ucstr (
	const hak_bch_t*   bcs,
	hak_oow_t*         bcslen,
	hak_uch_t*         ucs,
	hak_oow_t*         ucslen
);
#if defined(__cplusplus)
}
#endif

#endif
