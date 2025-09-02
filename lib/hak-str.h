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

#ifndef _HAK_STR_H_
#define _HAK_STR_H_

#include <hak-cmn.h>

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * The hak_equal_uchars() function determines equality of two strings
 * of the same length \a len.
 */
HAK_EXPORT int hak_equal_uchars (
	const hak_uch_t* str1,
	const hak_uch_t* str2,
	hak_oow_t        len
);

HAK_EXPORT int hak_equal_bchars (
	const hak_bch_t* str1,
	const hak_bch_t* str2,
	hak_oow_t        len
);

/* ------------------------------ */

HAK_EXPORT int hak_comp_uchars (
	const hak_uch_t* str1,
	hak_oow_t        len1,
	const hak_uch_t* str2,
	hak_oow_t        len2
);

HAK_EXPORT int hak_comp_bchars (
	const hak_bch_t* str1,
	hak_oow_t        len1,
	const hak_bch_t* str2,
	hak_oow_t        len2
);

HAK_EXPORT int hak_comp_ucstr (
	const hak_uch_t* str1,
	const hak_uch_t* str2
);

HAK_EXPORT int hak_comp_bcstr (
	const hak_bch_t* str1,
	const hak_bch_t* str2
);

HAK_EXPORT int hak_comp_ucstr_bcstr (
	const hak_uch_t* str1,
	const hak_bch_t* str2
);

HAK_EXPORT int hak_comp_uchars_ucstr (
	const hak_uch_t* str1,
	hak_oow_t        len,
	const hak_uch_t* str2
);

HAK_EXPORT int hak_comp_uchars_bcstr (
	const hak_uch_t* str1,
	hak_oow_t        len,
	const hak_bch_t* str2
);

HAK_EXPORT int hak_comp_bchars_bcstr (
	const hak_bch_t* str1,
	hak_oow_t        len,
	const hak_bch_t* str2
);

HAK_EXPORT int hak_comp_bchars_ucstr (
	const hak_bch_t* str1,
	hak_oow_t        len,
	const hak_uch_t* str2
);

/* ------------------------------ */

HAK_EXPORT void hak_copy_uchars (
	hak_uch_t*       dst,
	const hak_uch_t* src,
	hak_oow_t        len
);

HAK_EXPORT void hak_copy_bchars (
	hak_bch_t*       dst,
	const hak_bch_t* src,
	hak_oow_t        len
);

HAK_EXPORT void hak_copy_bchars_to_uchars (
	hak_uch_t*       dst,
	const hak_bch_t* src,
	hak_oow_t        len
);

HAK_EXPORT void hak_copy_uchars_to_bchars (
	hak_bch_t*       dst,
	const hak_uch_t* src,
	hak_oow_t        len
);

HAK_EXPORT hak_oow_t hak_copy_bcstr_to_ucstr (
	hak_uch_t*       dst,
	hak_oow_t        len,
	const hak_bch_t* src
);

HAK_EXPORT hak_oow_t hak_copy_ucstr_to_bcstr (
	hak_bch_t*       dst,
	hak_oow_t        len,
	const hak_uch_t* src
);

HAK_EXPORT hak_oow_t hak_copy_uchars_to_ucstr_unlimited (
	hak_uch_t*       dst,
	const hak_uch_t* src,
	hak_oow_t        len
);

HAK_EXPORT hak_oow_t hak_copy_bchars_to_bcstr_unlimited (
	hak_bch_t*       dst,
	const hak_bch_t* src,
	hak_oow_t        len
);

HAK_EXPORT hak_oow_t hak_copy_ucstr (
	hak_uch_t*       dst,
	hak_oow_t        len,
	const hak_uch_t* src
);

HAK_EXPORT hak_oow_t hak_copy_bcstr (
	hak_bch_t*       dst,
	hak_oow_t        len,
	const hak_bch_t* src
);

HAK_EXPORT hak_oow_t hak_copy_uchars_to_ucstr (
	hak_uch_t*       dst,
	hak_oow_t        dlen,
	const hak_uch_t* src,
	hak_oow_t        slen
);

HAK_EXPORT hak_oow_t hak_copy_bchars_to_bcstr (
	hak_bch_t*       dst,
	hak_oow_t        dlen,
	const hak_bch_t* src,
	hak_oow_t        slen
);

HAK_EXPORT hak_oow_t hak_copy_ucstr_unlimited (
	hak_uch_t*       dst,
	const hak_uch_t* src
);

HAK_EXPORT hak_oow_t hak_copy_bcstr_unlimited (
	hak_bch_t*       dst,
	const hak_bch_t* src
);

/* ------------------------------ */

HAK_EXPORT void hak_fill_uchars (
	hak_uch_t*       dst,
	hak_uch_t        ch,
	hak_oow_t        len
);

HAK_EXPORT void hak_fill_bchars (
	hak_bch_t*       dst,
	hak_bch_t        ch,
	hak_oow_t        len
);

HAK_EXPORT hak_uch_t* hak_find_uchar (
	const hak_uch_t* ptr,
	hak_oow_t        len,
	hak_uch_t        c
);

HAK_EXPORT hak_bch_t* hak_find_bchar (
	const hak_bch_t* ptr,
	hak_oow_t        len,
	hak_bch_t        c
);

HAK_EXPORT hak_uch_t* hak_rfind_uchar (
	const hak_uch_t* ptr,
	hak_oow_t        len,
	hak_uch_t        c
);

HAK_EXPORT hak_bch_t* hak_rfind_bchar (
	const hak_bch_t* ptr,
	hak_oow_t        len,
	hak_bch_t        c
);

HAK_EXPORT hak_uch_t* hak_find_uchar_in_ucstr (
	const hak_uch_t* ptr,
	hak_uch_t        c
);

HAK_EXPORT hak_bch_t* hak_find_bchar_in_bcstr (
	const hak_bch_t* ptr,
	hak_bch_t        c
);

HAK_EXPORT hak_oow_t hak_rotate_uchars (
	hak_uch_t*       str,
	hak_oow_t        len,
	int              dir,
	hak_oow_t        n
);

HAK_EXPORT hak_oow_t hak_rotate_bchars (
	hak_bch_t*       str,
	hak_oow_t        len,
	int              dir,
	hak_oow_t        n
);

HAK_EXPORT hak_oow_t hak_count_ucstr (
	const hak_uch_t* str
);

HAK_EXPORT hak_oow_t hak_count_bcstr (
	const hak_bch_t* str
);

#if defined(HAK_OOCH_IS_UCH)
#	define hak_equal_oochars(str1,str2,len) hak_equal_uchars(str1,str2,len)
#	define hak_comp_oochars(str1,len1,str2,len2) hak_comp_uchars(str1,len1,str2,len2)
#	define hak_comp_oocstr_bcstr(str1,str2) hak_comp_ucstr_bcstr(str1,str2)
#	define hak_comp_oochars_bcstr(str1,len1,str2) hak_comp_uchars_bcstr(str1,len1,str2)
#	define hak_comp_oochars_ucstr(str1,len1,str2) hak_comp_uchars_ucstr(str1,len1,str2)
#	define hak_comp_oochars_oocstr(str1,len1,str2) hak_comp_uchars_ucstr(str1,len1,str2)
#	define hak_comp_oocstr(str1,str2) hak_comp_ucstr(str1,str2)

#	define hak_copy_oochars(dst,src,len) hak_copy_uchars(dst,src,len)
#	define hak_copy_bchars_to_oochars(dst,src,len) hak_copy_bchars_to_uchars(dst,src,len)
#	define hak_copy_oochars_to_bchars(dst,src,len) hak_copy_uchars_to_bchars(dst,src,len)
#	define hak_copy_uchars_to_oochars(dst,src,len) hak_copy_uchars(dst,src,len)
#	define hak_copy_oochars_to_uchars(dst,src,len) hak_copy_uchars(dst,src,len)

#	define hak_copy_oochars_to_oocstr(dst,dlen,src,slen) hak_copy_uchars_to_ucstr(dst,dlen,src,slen)
#	define hak_copy_oochars_to_oocstr_unlimited(dst,src,len) hak_copy_uchars_to_ucstr_unlimited(dst,src,len)
#	define hak_copy_oocstr(dst,len,src) hak_copy_ucstr(dst,len,src)
#	define hak_copy_oocstr_unlimited(dst,src) hak_copy_ucstr_unlimited(dst,src)

#	define hak_fill_oochars hak_fill_uchars
#	define hak_find_oochar hak_find_uchar
#	define hak_rfind_oochar hak_rfind_uchar
#	define hak_find_oochar_in_oocstr hak_find_uchar_in_ucstr
#	define hak_rotate_oochars hak_rotate_uchars
#	define hak_count_oocstr hak_count_ucstr
#else
#	define hak_equal_oochars(str1,str2,len) hak_equal_bchars(str1,str2,len)
#	define hak_comp_oochars(str1,len1,str2,len2) hak_comp_bchars(str1,len1,str2,len2)
#	define hak_comp_oocstr_bcstr(str1,str2) hak_comp_bcstr(str1,str2)
#	define hak_comp_oochars_bcstr(str1,len1,str2) hak_comp_bchars_bcstr(str1,len1,str2)
#	define hak_comp_oochars_ucstr(str1,len1,str2) hak_comp_bchars_ucstr(str1,len1,str2)
#	define hak_comp_oochars_oocstr(str1,len1,str2) hak_comp_bchars_bcstr(str1,len1,str2)
#	define hak_comp_oocstr(str1,str2) hak_comp_bcstr(str1,str2)

#	define hak_copy_oochars(dst,src,len) hak_copy_bchars(dst,src,len)
#	define hak_copy_bchars_to_oochars(dst,src,len) hak_copy_bchars(dst,src,len)
#	define hak_copy_oochars_to_bchars(dst,src,len) hak_copy_bchars(dst,src,len)
#	define hak_copy_uchars_to_oochars(dst,src,len) hak_copy_uchars_to_bchars(dst,src,len)
#	define hak_copy_oochars_to_uchars(dst,src,len) hak_copy_bchars_to_uchars(dst,src,len)

#	define hak_copy_oochars_to_oocstr(dst,dlen,src,slen) hak_copy_bchars_to_bcstr(dst,dlen,src,slen)
#	define hak_copy_oochars_to_oocstr_unlimited(dst,src,len) hak_copy_bchars_to_bcstr_unlimited(dst,src,len)
#	define hak_copy_oocstr(dst,len,src) hak_copy_bcstr(dst,len,src)
#	define hak_copy_oocstr_unlimited(dst,src) hak_copy_bcstr_unlimited(dst,src)

#	define hak_fill_oochars hak_fill_bchars
#	define hak_find_oochar hak_find_bchar
#	define hak_rfind_oochar hak_rfind_bchar
#	define hak_find_oochar_in_oocstr hak_find_bchar_in_bcstr
#	define hak_rotate_oochars hak_rotate_bchars
#	define hak_count_oocstr hak_count_bcstr
#endif

#define HAK_BYTE_TO_BCSTR_RADIXMASK (0xFF)
#define HAK_BYTE_TO_BCSTR_LOWERCASE (1 << 8)

hak_oow_t hak_byte_to_bcstr (
	hak_uint8_t   byte,
	hak_bch_t*    buf,
	hak_oow_t     size,
	int           flagged_radix,
	hak_bch_t     fill
);


HAK_EXPORT int hak_conv_bcstr_to_ucstr_with_cmgr (
	const hak_bch_t* bcs,
	hak_oow_t*       bcslen,
	hak_uch_t*       ucs,
	hak_oow_t*       ucslen,
	hak_cmgr_t*      cmgr,
	int              all
);

HAK_EXPORT int hak_conv_bchars_to_uchars_with_cmgr (
	const hak_bch_t* bcs,
	hak_oow_t*       bcslen,
	hak_uch_t*       ucs,
	hak_oow_t*       ucslen,
	hak_cmgr_t*      cmgr,
	int              all
);

HAK_EXPORT int hak_conv_ucstr_to_bcstr_with_cmgr (
	const hak_uch_t* ucs,
	hak_oow_t*       ucslen,
	hak_bch_t*       bcs,
	hak_oow_t*       bcslen,
	hak_cmgr_t*      cmgr
);

HAK_EXPORT int hak_conv_uchars_to_bchars_with_cmgr (
	const hak_uch_t* ucs,
	hak_oow_t*       ucslen,
	hak_bch_t*       bcs,
	hak_oow_t*       bcslen,
	hak_cmgr_t*      cmgr
);

#if defined(HAK_OOCH_IS_UCH)
#	define hak_conv_oocstr_to_bcstr_with_cmgr(oocs,oocslen,bcs,bcslen,cmgr) hak_conv_ucstr_to_bcstr_with_cmgr(oocs,oocslen,bcs,bcslen,cmgr)
#	define hak_conv_oochars_to_bchars_with_cmgr(oocs,oocslen,bcs,bcslen,cmgr) hak_conv_uchars_to_bchars_with_cmgr(oocs,oocslen,bcs,bcslen,cmgr)
#else
#	define hak_conv_oocstr_to_ucstr_with_cmgr(oocs,oocslen,ucs,ucslen,cmgr) hak_conv_bcstr_to_ucstr_with_cmgr(oocs,oocslen,ucs,ucslen,cmgr,0)
#	define hak_conv_oochars_to_uchars_with_cmgr(oocs,oocslen,ucs,ucslen,cmgr) hak_conv_bchars_to_uchars_with_cmgr(oocs,oocslen,ucs,ucslen,cmgr,0)
#endif

#if defined(__cplusplus)
}
#endif

#endif
