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

#ifndef _HCL_STR_H_
#define _HCL_STR_H_

#include <hcl-cmn.h>

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * The hcl_equal_uchars() function determines equality of two strings
 * of the same length \a len.
 */
HCL_EXPORT int hcl_equal_uchars (
	const hcl_uch_t* str1,
	const hcl_uch_t* str2,
	hcl_oow_t        len
);

HCL_EXPORT int hcl_equal_bchars (
	const hcl_bch_t* str1,
	const hcl_bch_t* str2,
	hcl_oow_t        len
);

/* ------------------------------ */

HCL_EXPORT int hcl_comp_uchars (
	const hcl_uch_t* str1,
	hcl_oow_t        len1,
	const hcl_uch_t* str2,
	hcl_oow_t        len2
);

HCL_EXPORT int hcl_comp_bchars (
	const hcl_bch_t* str1,
	hcl_oow_t        len1,
	const hcl_bch_t* str2,
	hcl_oow_t        len2
);

HCL_EXPORT int hcl_comp_ucstr (
	const hcl_uch_t* str1,
	const hcl_uch_t* str2
);

HCL_EXPORT int hcl_comp_bcstr (
	const hcl_bch_t* str1,
	const hcl_bch_t* str2
);

HCL_EXPORT int hcl_comp_ucstr_bcstr (
	const hcl_uch_t* str1,
	const hcl_bch_t* str2
);

HCL_EXPORT int hcl_comp_uchars_ucstr (
	const hcl_uch_t* str1,
	hcl_oow_t        len,
	const hcl_uch_t* str2
);

HCL_EXPORT int hcl_comp_uchars_bcstr (
	const hcl_uch_t* str1,
	hcl_oow_t        len,
	const hcl_bch_t* str2
);

HCL_EXPORT int hcl_comp_bchars_bcstr (
	const hcl_bch_t* str1,
	hcl_oow_t        len,
	const hcl_bch_t* str2
);

HCL_EXPORT int hcl_comp_bchars_ucstr (
	const hcl_bch_t* str1,
	hcl_oow_t        len,
	const hcl_uch_t* str2
);

/* ------------------------------ */

HCL_EXPORT void hcl_copy_uchars (
	hcl_uch_t*       dst,
	const hcl_uch_t* src,
	hcl_oow_t        len
);

HCL_EXPORT void hcl_copy_bchars (
	hcl_bch_t*       dst,
	const hcl_bch_t* src,
	hcl_oow_t        len
);

HCL_EXPORT void hcl_copy_bchars_to_uchars (
	hcl_uch_t*       dst,
	const hcl_bch_t* src,
	hcl_oow_t        len
);

HCL_EXPORT void hcl_copy_uchars_to_bchars (
	hcl_bch_t*       dst,
	const hcl_uch_t* src,
	hcl_oow_t        len
);

HCL_EXPORT hcl_oow_t hcl_copy_bcstr_to_ucstr (
	hcl_uch_t*       dst,
	hcl_oow_t        len,
	const hcl_bch_t* src
);

HCL_EXPORT hcl_oow_t hcl_copy_ucstr_to_bcstr (
	hcl_bch_t*       dst,
	hcl_oow_t        len,
	const hcl_uch_t* src
);

HCL_EXPORT hcl_oow_t hcl_copy_uchars_to_ucstr_unlimited (
	hcl_uch_t*       dst,
	const hcl_uch_t* src,
	hcl_oow_t        len
);

HCL_EXPORT hcl_oow_t hcl_copy_bchars_to_bcstr_unlimited (
	hcl_bch_t*       dst,
	const hcl_bch_t* src,
	hcl_oow_t        len
);

HCL_EXPORT hcl_oow_t hcl_copy_ucstr (
	hcl_uch_t*       dst,
	hcl_oow_t        len,
	const hcl_uch_t* src
);

HCL_EXPORT hcl_oow_t hcl_copy_bcstr (
	hcl_bch_t*       dst,
	hcl_oow_t        len,
	const hcl_bch_t* src
);

HCL_EXPORT hcl_oow_t hcl_copy_uchars_to_ucstr (
	hcl_uch_t*       dst,
	hcl_oow_t        dlen,
	const hcl_uch_t* src,
	hcl_oow_t        slen
);

HCL_EXPORT hcl_oow_t hcl_copy_bchars_to_bcstr (
	hcl_bch_t*       dst,
	hcl_oow_t        dlen,
	const hcl_bch_t* src,
	hcl_oow_t        slen
);

HCL_EXPORT hcl_oow_t hcl_copy_ucstr_unlimited (
	hcl_uch_t*       dst,
	const hcl_uch_t* src
);

HCL_EXPORT hcl_oow_t hcl_copy_bcstr_unlimited (
	hcl_bch_t*       dst,
	const hcl_bch_t* src
);

/* ------------------------------ */

HCL_EXPORT void hcl_fill_uchars (
	hcl_uch_t*       dst,
	hcl_uch_t        ch,
	hcl_oow_t        len
);

HCL_EXPORT void hcl_fill_bchars (
	hcl_bch_t*       dst,
	hcl_bch_t        ch,
	hcl_oow_t        len
);

HCL_EXPORT hcl_uch_t* hcl_find_uchar (
	const hcl_uch_t* ptr,
	hcl_oow_t        len,
	hcl_uch_t        c
);

HCL_EXPORT hcl_bch_t* hcl_find_bchar (
	const hcl_bch_t* ptr,
	hcl_oow_t        len,
	hcl_bch_t        c
);

HCL_EXPORT hcl_uch_t* hcl_rfind_uchar (
	const hcl_uch_t* ptr,
	hcl_oow_t        len,
	hcl_uch_t        c
);

HCL_EXPORT hcl_bch_t* hcl_rfind_bchar (
	const hcl_bch_t* ptr,
	hcl_oow_t        len,
	hcl_bch_t        c
);

HCL_EXPORT hcl_uch_t* hcl_find_uchar_in_ucstr (
	const hcl_uch_t* ptr,
	hcl_uch_t        c
);

HCL_EXPORT hcl_bch_t* hcl_find_bchar_in_bcstr (
	const hcl_bch_t* ptr,
	hcl_bch_t        c
);

HCL_EXPORT hcl_oow_t hcl_rotate_uchars (
	hcl_uch_t*       str,
	hcl_oow_t        len,
	int              dir,
	hcl_oow_t        n
);

HCL_EXPORT hcl_oow_t hcl_rotate_bchars (
	hcl_bch_t*       str,
	hcl_oow_t        len,
	int              dir,
	hcl_oow_t        n
);

HCL_EXPORT hcl_oow_t hcl_count_ucstr (
	const hcl_uch_t* str
);

HCL_EXPORT hcl_oow_t hcl_count_bcstr (
	const hcl_bch_t* str
);

#if defined(HCL_OOCH_IS_UCH)
#	define hcl_equal_oochars(str1,str2,len) hcl_equal_uchars(str1,str2,len)
#	define hcl_comp_oochars(str1,len1,str2,len2) hcl_comp_uchars(str1,len1,str2,len2)
#	define hcl_comp_oocstr_bcstr(str1,str2) hcl_comp_ucstr_bcstr(str1,str2)
#	define hcl_comp_oochars_bcstr(str1,len1,str2) hcl_comp_uchars_bcstr(str1,len1,str2)
#	define hcl_comp_oochars_ucstr(str1,len1,str2) hcl_comp_uchars_ucstr(str1,len1,str2)
#	define hcl_comp_oochars_oocstr(str1,len1,str2) hcl_comp_uchars_ucstr(str1,len1,str2)
#	define hcl_comp_oocstr(str1,str2) hcl_comp_ucstr(str1,str2)

#	define hcl_copy_oochars(dst,src,len) hcl_copy_uchars(dst,src,len)
#	define hcl_copy_bchars_to_oochars(dst,src,len) hcl_copy_bchars_to_uchars(dst,src,len)
#	define hcl_copy_oochars_to_bchars(dst,src,len) hcl_copy_uchars_to_bchars(dst,src,len)
#	define hcl_copy_uchars_to_oochars(dst,src,len) hcl_copy_uchars(dst,src,len)
#	define hcl_copy_oochars_to_uchars(dst,src,len) hcl_copy_uchars(dst,src,len)

#	define hcl_copy_oochars_to_oocstr(dst,dlen,src,slen) hcl_copy_uchars_to_ucstr(dst,dlen,src,slen)
#	define hcl_copy_oochars_to_oocstr_unlimited(dst,src,len) hcl_copy_uchars_to_ucstr_unlimited(dst,src,len)
#	define hcl_copy_oocstr(dst,len,src) hcl_copy_ucstr(dst,len,src)
#	define hcl_copy_oocstr_unlimited(dst,src) hcl_copy_ucstr_unlimited(dst,src)

#	define hcl_fill_oochars hcl_fill_uchars
#	define hcl_find_oochar hcl_find_uchar
#	define hcl_rfind_oochar hcl_rfind_uchar
#	define hcl_find_oochar_in_oocstr hcl_find_uchar_in_ucstr
#	define hcl_rotate_oochars hcl_rotate_uchars
#	define hcl_count_oocstr hcl_count_ucstr
#else
#	define hcl_equal_oochars(str1,str2,len) hcl_equal_bchars(str1,str2,len)
#	define hcl_comp_oochars(str1,len1,str2,len2) hcl_comp_bchars(str1,len1,str2,len2)
#	define hcl_comp_oocstr_bcstr(str1,str2) hcl_comp_bcstr(str1,str2)
#	define hcl_comp_oochars_bcstr(str1,len1,str2) hcl_comp_bchars_bcstr(str1,len1,str2)
#	define hcl_comp_oochars_ucstr(str1,len1,str2) hcl_comp_bchars_ucstr(str1,len1,str2)
#	define hcl_comp_oochars_oocstr(str1,len1,str2) hcl_comp_bchars_bcstr(str1,len1,str2)
#	define hcl_comp_oocstr(str1,str2) hcl_comp_bcstr(str1,str2)

#	define hcl_copy_oochars(dst,src,len) hcl_copy_bchars(dst,src,len)
#	define hcl_copy_bchars_to_oochars(dst,src,len) hcl_copy_bchars(dst,src,len)
#	define hcl_copy_oochars_to_bchars(dst,src,len) hcl_copy_bchars(dst,src,len)
#	define hcl_copy_uchars_to_oochars(dst,src,len) hcl_copy_uchars_to_bchars(dst,src,len)
#	define hcl_copy_oochars_to_uchars(dst,src,len) hcl_copy_bchars_to_uchars(dst,src,len)

#	define hcl_copy_oochars_to_oocstr(dst,dlen,src,slen) hcl_copy_bchars_to_bcstr(dst,dlen,src,slen)
#	define hcl_copy_oochars_to_oocstr_unlimited(dst,src,len) hcl_copy_bchars_to_bcstr_unlimited(dst,src,len)
#	define hcl_copy_oocstr(dst,len,src) hcl_copy_bcstr(dst,len,src)
#	define hcl_copy_oocstr_unlimited(dst,src) hcl_copy_bcstr_unlimited(dst,src)

#	define hcl_fill_oochars hcl_fill_bchars
#	define hcl_find_oochar hcl_find_bchar
#	define hcl_rfind_oochar hcl_rfind_bchar
#	define hcl_find_oochar_in_oocstr hcl_find_bchar_in_bcstr
#	define hcl_rotate_oochars hcl_rotate_bchars
#	define hcl_count_oocstr hcl_count_bcstr
#endif

#define HCL_BYTE_TO_BCSTR_RADIXMASK (0xFF)
#define HCL_BYTE_TO_BCSTR_LOWERCASE (1 << 8)

hcl_oow_t hcl_byte_to_bcstr (
	hcl_uint8_t   byte,
	hcl_bch_t*    buf,
	hcl_oow_t     size,
	int           flagged_radix,
	hcl_bch_t     fill
);


HCL_EXPORT int hcl_conv_bcstr_to_ucstr_with_cmgr (
	const hcl_bch_t* bcs,
	hcl_oow_t*       bcslen,
	hcl_uch_t*       ucs,
	hcl_oow_t*       ucslen,
	hcl_cmgr_t*      cmgr,
	int              all
);

HCL_EXPORT int hcl_conv_bchars_to_uchars_with_cmgr (
	const hcl_bch_t* bcs,
	hcl_oow_t*       bcslen,
	hcl_uch_t*       ucs,
	hcl_oow_t*       ucslen,
	hcl_cmgr_t*      cmgr,
	int              all
);

HCL_EXPORT int hcl_conv_ucstr_to_bcstr_with_cmgr (
	const hcl_uch_t* ucs,
	hcl_oow_t*       ucslen,
	hcl_bch_t*       bcs,
	hcl_oow_t*       bcslen,
	hcl_cmgr_t*      cmgr
);

HCL_EXPORT int hcl_conv_uchars_to_bchars_with_cmgr (
	const hcl_uch_t* ucs,
	hcl_oow_t*       ucslen,
	hcl_bch_t*       bcs,
	hcl_oow_t*       bcslen,
	hcl_cmgr_t*      cmgr
);

#if defined(HCL_OOCH_IS_UCH)
#	define hcl_conv_oocstr_to_bcstr_with_cmgr(oocs,oocslen,bcs,bcslen,cmgr) hcl_conv_ucstr_to_bcstr_with_cmgr(oocs,oocslen,bcs,bcslen,cmgr)
#	define hcl_conv_oochars_to_bchars_with_cmgr(oocs,oocslen,bcs,bcslen,cmgr) hcl_conv_uchars_to_bchars_with_cmgr(oocs,oocslen,bcs,bcslen,cmgr)
#else
#	define hcl_conv_oocstr_to_ucstr_with_cmgr(oocs,oocslen,ucs,ucslen,cmgr) hcl_conv_bcstr_to_ucstr_with_cmgr(oocs,oocslen,ucs,ucslen,cmgr,0)
#	define hcl_conv_oochars_to_uchars_with_cmgr(oocs,oocslen,ucs,ucslen,cmgr) hcl_conv_bchars_to_uchars_with_cmgr(oocs,oocslen,ucs,ucslen,cmgr,0)
#endif

#if defined(__cplusplus)
}
#endif

#endif
