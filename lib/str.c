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

#include <hak-str.h>

int hak_equal_uchars (const hak_uch_t* str1, const hak_uch_t* str2, hak_oow_t len)
{
	hak_oow_t i;

	/* NOTE: you should call this function after having ensured that
	 *       str1 and str2 are in the same length */

	for (i = 0; i < len; i++)
	{
		if (str1[i] != str2[i]) return 0;
	}

	return 1;
}

int hak_equal_bchars (const hak_bch_t* str1, const hak_bch_t* str2, hak_oow_t len)
{
	hak_oow_t i;

	/* NOTE: you should call this function after having ensured that
	 *       str1 and str2 are in the same length */

	for (i = 0; i < len; i++)
	{
		if (str1[i] != str2[i]) return 0;
	}

	return 1;
}

int hak_comp_uchars (const hak_uch_t* str1, hak_oow_t len1, const hak_uch_t* str2, hak_oow_t len2)
{
	hak_uchu_t c1, c2;
	const hak_uch_t* end1 = str1 + len1;
	const hak_uch_t* end2 = str2 + len2;

	while (str1 < end1)
	{
		c1 = *str1;
		if (str2 < end2)
		{
			c2 = *str2;
			if (c1 > c2) return 1;
			if (c1 < c2) return -1;
		}
		else return 1;
		str1++; str2++;
	}

	return (str2 < end2)? -1: 0;
}

int hak_comp_bchars (const hak_bch_t* str1, hak_oow_t len1, const hak_bch_t* str2, hak_oow_t len2)
{
	hak_bchu_t c1, c2;
	const hak_bch_t* end1 = str1 + len1;
	const hak_bch_t* end2 = str2 + len2;

	while (str1 < end1)
	{
		c1 = *str1;
		if (str2 < end2)
		{
			c2 = *str2;
			if (c1 > c2) return 1;
			if (c1 < c2) return -1;
		}
		else return 1;
		str1++; str2++;
	}

	return (str2 < end2)? -1: 0;
}

int hak_comp_ucstr (const hak_uch_t* str1, const hak_uch_t* str2)
{
	while (*str1 == *str2)
	{
		if (*str1 == '\0') return 0;
		str1++; str2++;
	}

	return ((hak_uchu_t)*str1 > (hak_uchu_t)*str2)? 1: -1;
}

int hak_comp_bcstr (const hak_bch_t* str1, const hak_bch_t* str2)
{
	while (*str1 == *str2)
	{
		if (*str1 == '\0') return 0;
		str1++; str2++;
	}

	return ((hak_bchu_t)*str1 > (hak_bchu_t)*str2)? 1: -1;
}

int hak_comp_ucstr_bcstr (const hak_uch_t* str1, const hak_bch_t* str2)
{
	while (*str1 == *str2)
	{
		if (*str1 == '\0') return 0;
		str1++; str2++;
	}

	return ((hak_uchu_t)*str1 > (hak_bchu_t)*str2)? 1: -1;
}

int hak_comp_uchars_ucstr (const hak_uch_t* str1, hak_oow_t len, const hak_uch_t* str2)
{
	/* for "abc\0" of length 4 vs "abc", the fourth character
	 * of the first string is equal to the terminating null of
	 * the second string. the first string is still considered
	 * bigger */
	const hak_uch_t* end = str1 + len;
	while (str1 < end && *str2 != '\0')
	{
		if (*str1 != *str2) return ((hak_uchu_t)*str1 > (hak_uchu_t)*str2)? 1: -1;
		str1++; str2++;
	}
	return (str1 < end)? 1: (*str2 == '\0'? 0: -1);
}

int hak_comp_uchars_bcstr (const hak_uch_t* str1, hak_oow_t len, const hak_bch_t* str2)
{
	const hak_uch_t* end = str1 + len;
	while (str1 < end && *str2 != '\0')
	{
		if (*str1 != *str2) return ((hak_uchu_t)*str1 > (hak_bchu_t)*str2)? 1: -1;
		str1++; str2++;
	}
	return (str1 < end)? 1: (*str2 == '\0'? 0: -1);
}

int hak_comp_bchars_bcstr (const hak_bch_t* str1, hak_oow_t len, const hak_bch_t* str2)
{
	const hak_bch_t* end = str1 + len;
	while (str1 < end && *str2 != '\0')
	{
		if (*str1 != *str2) return ((hak_bchu_t)*str1 > (hak_bchu_t)*str2)? 1: -1;
		str1++; str2++;
	}
	return (str1 < end)? 1: (*str2 == '\0'? 0: -1);
}

int hak_comp_bchars_ucstr (const hak_bch_t* str1, hak_oow_t len, const hak_uch_t* str2)
{
	const hak_bch_t* end = str1 + len;
	while (str1 < end && *str2 != '\0')
	{
		if (*str1 != *str2) return ((hak_bchu_t)*str1 > (hak_uchu_t)*str2)? 1: -1;
		str1++; str2++;
	}
	return (str1 < end)? 1: (*str2 == '\0'? 0: -1);
}

void hak_copy_uchars (hak_uch_t* dst, const hak_uch_t* src, hak_oow_t len)
{
	/* take note of no forced null termination */
	hak_oow_t i;
	for (i = 0; i < len; i++) dst[i] = src[i];
}

void hak_copy_bchars (hak_bch_t* dst, const hak_bch_t* src, hak_oow_t len)
{
	/* take note of no forced null termination */
	hak_oow_t i;
	for (i = 0; i < len; i++) dst[i] = src[i];
}

void hak_copy_bchars_to_uchars (hak_uch_t* dst, const hak_bch_t* src, hak_oow_t len)
{
	/* copy without conversions.
	 * use hak_convbtouchars() for conversion encoding */
	hak_oow_t i;
	for (i = 0; i < len; i++) dst[i] = src[i];
}

void hak_copy_uchars_to_bchars (hak_bch_t* dst, const hak_uch_t* src, hak_oow_t len)
{
	/* copy without conversions.
	 * use hak_convutobchars() for conversion encoding */
	hak_oow_t i;
	for (i = 0; i < len; i++) dst[i] = src[i];
}

hak_oow_t hak_copy_bcstr_to_ucstr (hak_uch_t* dst, hak_oow_t len, const hak_bch_t* src)
{
	/* copy without conversions.
	 * the code is the same as hak_copy_bcstr() except type of src */
	hak_uch_t* p, * p2;

	p = dst; p2 = dst + len - 1;

	while (p < p2)
	{
		 if (*src == '\0') break;
		 *p++ = *src++;
	}

	if (len > 0) *p = '\0';
	return p - dst;
}

hak_oow_t hak_copy_ucstr_to_bcstr (hak_bch_t* dst, hak_oow_t len, const hak_uch_t* src)
{
	/* copy without conversions */
	hak_bch_t* p, * p2;

	p = dst; p2 = dst + len - 1;

	while (p < p2)
	{
		 if (*src == '\0') break;
		 *p++ = *src++;
	}

	if (len > 0) *p = '\0';
	return p - dst;
}


hak_oow_t hak_copy_uchars_to_ucstr (hak_uch_t* dst, hak_oow_t dlen, const hak_uch_t* src, hak_oow_t slen)
{
	hak_oow_t i;
	if (dlen <= 0) return 0;
	if (dlen <= slen) slen = dlen - 1;
	for (i = 0; i < slen; i++) dst[i] = src[i];
	dst[i] = '\0';
	return i;
}

hak_oow_t hak_copy_bchars_to_bcstr (hak_bch_t* dst, hak_oow_t dlen, const hak_bch_t* src, hak_oow_t slen)
{
	hak_oow_t i;
	if (dlen <= 0) return 0;
	if (dlen <= slen) slen = dlen - 1;
	for (i = 0; i < slen; i++) dst[i] = src[i];
	dst[i] = '\0';
	return i;
}

hak_oow_t hak_copy_uchars_to_ucstr_unlimited (hak_uch_t* dst, const hak_uch_t* src, hak_oow_t len)
{
	hak_oow_t i;
	for (i = 0; i < len; i++) dst[i] = src[i];
	dst[i] = '\0';
	return i;
}

hak_oow_t hak_copy_bchars_to_bcstr_unlimited (hak_bch_t* dst, const hak_bch_t* src, hak_oow_t len)
{
	hak_oow_t i;
	for (i = 0; i < len; i++) dst[i] = src[i];
	dst[i] = '\0';
	return i;
}

hak_oow_t hak_copy_ucstr (hak_uch_t* dst, hak_oow_t len, const hak_uch_t* src)
{
	hak_uch_t* p, * p2;

	p = dst; p2 = dst + len - 1;

	while (p < p2)
	{
		 if (*src == '\0') break;
		 *p++ = *src++;
	}

	if (len > 0) *p = '\0';
	return p - dst;
}

hak_oow_t hak_copy_bcstr (hak_bch_t* dst, hak_oow_t len, const hak_bch_t* src)
{
	hak_bch_t* p, * p2;

	p = dst; p2 = dst + len - 1;

	while (p < p2)
	{
		 if (*src == '\0') break;
		 *p++ = *src++;
	}

	if (len > 0) *p = '\0';
	return p - dst;
}


hak_oow_t hak_copy_ucstr_unlimited (hak_uch_t* dst, const hak_uch_t* src)
{
	hak_uch_t* org = dst;
	while ((*dst++ = *src++) != '\0');
	return dst - org - 1;
}

hak_oow_t hak_copy_bcstr_unlimited (hak_bch_t* dst, const hak_bch_t* src)
{
	hak_bch_t* org = dst;
	while ((*dst++ = *src++) != '\0');
	return dst - org - 1;
}

void hak_fill_uchars (hak_uch_t* dst, hak_uch_t ch, hak_oow_t len)
{
	hak_oow_t i;
	for (i = 0; i < len; i++) dst[i] = ch;
}

void hak_fill_bchars (hak_bch_t* dst, hak_bch_t ch, hak_oow_t len)
{
	hak_oow_t i;
	for (i = 0; i < len; i++) dst[i] = ch;
}

hak_oow_t hak_count_ucstr (const hak_uch_t* str)
{
	const hak_uch_t* ptr = str;
	while (*ptr != '\0') ptr++;
	return ptr - str;
}

hak_oow_t hak_count_bcstr (const hak_bch_t* str)
{
	const hak_bch_t* ptr = str;
	while (*ptr != '\0') ptr++;
	return ptr - str;
}

hak_uch_t* hak_find_uchar (const hak_uch_t* ptr, hak_oow_t len, hak_uch_t c)
{
	const hak_uch_t* end;

	end = ptr + len;
	while (ptr < end)
	{
		if (*ptr == c) return (hak_uch_t*)ptr;
		ptr++;
	}

	return HAK_NULL;
}

hak_bch_t* hak_find_bchar (const hak_bch_t* ptr, hak_oow_t len, hak_bch_t c)
{
	const hak_bch_t* end;

	end = ptr + len;
	while (ptr < end)
	{
		if (*ptr == c) return (hak_bch_t*)ptr;
		ptr++;
	}

	return HAK_NULL;
}

hak_uch_t* hak_rfind_uchar (const hak_uch_t* ptr, hak_oow_t len, hak_uch_t c)
{
	const hak_uch_t* cur;

	cur = ptr + len;
	while (cur > ptr)
	{
		--cur;
		if (*cur == c) return (hak_uch_t*)cur;
	}

	return HAK_NULL;
}

hak_bch_t* hak_rfind_bchar (const hak_bch_t* ptr, hak_oow_t len, hak_bch_t c)
{
	const hak_bch_t* cur;

	cur = ptr + len;
	while (cur > ptr)
	{
		--cur;
		if (*cur == c) return (hak_bch_t*)cur;
	}

	return HAK_NULL;
}

hak_uch_t* hak_find_uchar_in_ucstr (const hak_uch_t* ptr, hak_uch_t c)
{
	while (*ptr != '\0')
	{
		if (*ptr == c) return (hak_uch_t*)ptr;
		ptr++;
	}

	return HAK_NULL;
}

hak_bch_t* hak_find_bchar_in_bcstr (const hak_bch_t* ptr, hak_bch_t c)
{
	while (*ptr != '\0')
	{
		if (*ptr == c) return (hak_bch_t*)ptr;
		ptr++;
	}

	return HAK_NULL;
}

/* ----------------------------------------------------------------------- */

hak_oow_t hak_rotate_uchars (hak_uch_t* str, hak_oow_t len, int dir, hak_oow_t n)
{
	hak_oow_t first, last, count, index, nk;
	hak_uch_t c;

	if (dir == 0 || len == 0) return len;
	if ((n %= len) == 0) return len;

	if (dir > 0) n = len - n;
	first = 0; nk = len - n; count = 0;

	while (count < n)
	{
		last = first + nk;
		index = first;
		c = str[first];
		do
		{
			count++;
			while (index < nk)
			{
				str[index] = str[index + n];
				index += n;
			}
			if (index == last) break;
			str[index] = str[index - nk];
			index -= nk;
		}
		while (1);
		str[last] = c; first++;
	}
	return len;
}

hak_oow_t hak_rotate_bchars (hak_bch_t* str, hak_oow_t len, int dir, hak_oow_t n)
{
	hak_oow_t first, last, count, index, nk;
	hak_bch_t c;

	if (dir == 0 || len == 0) return len;
	if ((n %= len) == 0) return len;

	if (dir > 0) n = len - n;
	first = 0; nk = len - n; count = 0;

	while (count < n)
	{
		last = first + nk;
		index = first;
		c = str[first];
		do
		{
			count++;
			while (index < nk)
			{
				str[index] = str[index + n];
				index += n;
			}
			if (index == last) break;
			str[index] = str[index - nk];
			index -= nk;
		}
		while (1);
		str[last] = c; first++;
	}
	return len;
}

/* ----------------------------------------------------------------------- */

hak_oow_t hak_byte_to_bcstr (hak_uint8_t byte, hak_bch_t* buf, hak_oow_t size, int flagged_radix, hak_bch_t fill)
{
	hak_bch_t tmp[(HAK_SIZEOF(hak_uint8_t) * HAK_BITS_PER_BYTE)];
	hak_bch_t* p = tmp, * bp = buf, * be = buf + size - 1;
	int radix;
	hak_bch_t radix_char;

	radix = (flagged_radix & HAK_BYTE_TO_BCSTR_RADIXMASK);
	radix_char = (flagged_radix & HAK_BYTE_TO_BCSTR_LOWERCASE)? 'a': 'A';
	if (radix < 2 || radix > 36 || size <= 0) return 0;

	do
	{
		hak_uint8_t digit = byte % radix;
		if (digit < 10) *p++ = digit + '0';
		else *p++ = digit + radix_char - 10;
		byte /= radix;
	}
	while (byte > 0);

	if (fill != '\0')
	{
		while (size - 1 > p - tmp)
		{
			*bp++ = fill;
			size--;
		}
	}

	while (p > tmp && bp < be) *bp++ = *--p;
	*bp = '\0';
	return bp - buf;
}

/* ----------------------------------------------------------------------- */

int hak_conv_bchars_to_uchars_with_cmgr (const hak_bch_t* bcs, hak_oow_t* bcslen, hak_uch_t* ucs, hak_oow_t* ucslen, hak_cmgr_t* cmgr, int all)
{
	const hak_bch_t* p;
	int ret = 0;
	hak_oow_t mlen;

	if (ucs)
	{
		/* destination buffer is specified.
		 * copy the conversion result to the buffer */

		hak_uch_t* q, * qend;

		p = bcs;
		q = ucs;
		qend = ucs + *ucslen;
		mlen = *bcslen;

		while (mlen > 0)
		{
			hak_oow_t n;

			if (q >= qend)
			{
				/* buffer too small */
				ret = -2;
				break;
			}

			n = cmgr->bctouc(p, mlen, q);
			if (n == 0)
			{
				/* invalid sequence */
				if (all)
				{
					n = 1;
					*q = '?';
				}
				else
				{
					ret = -1;
					break;
				}
			}
			if (n > mlen)
			{
				/* incomplete sequence */
				if (all)
				{
					n = 1;
					*q = '?';
				}
				else
				{
					ret = -3;
					break;
				}
			}

			q++;
			p += n;
			mlen -= n;
		}

		*ucslen = q - ucs;
		*bcslen = p - bcs;
	}
	else
	{
		/* no destination buffer is specified. perform conversion
		 * but don't copy the result. the caller can call this function
		 * without a buffer to find the required buffer size, allocate
		 * a buffer with the size and call this function again with
		 * the buffer. */

		hak_uch_t w;
		hak_oow_t wlen = 0;

		p = bcs;
		mlen = *bcslen;

		while (mlen > 0)
		{
			hak_oow_t n;

			n = cmgr->bctouc(p, mlen, &w);
			if (n == 0)
			{
				/* invalid sequence */
				if (all) n = 1;
				else
				{
					ret = -1;
					break;
				}
			}
			if (n > mlen)
			{
				/* incomplete sequence */
				if (all) n = 1;
				else
				{
					ret = -3;
					break;
				}
			}

			p += n;
			mlen -= n;
			wlen += 1;
		}

		*ucslen = wlen;
		*bcslen = p - bcs;
	}

	return ret;
}

int hak_conv_bcstr_to_ucstr_with_cmgr (const hak_bch_t* bcs, hak_oow_t* bcslen, hak_uch_t* ucs, hak_oow_t* ucslen, hak_cmgr_t* cmgr, int all)
{
	const hak_bch_t* bp;
	hak_oow_t mlen, wlen;
	int n;

	for (bp = bcs; *bp != '\0'; bp++) /* nothing */ ;

	mlen = bp - bcs; wlen = *ucslen;
	n = hak_conv_bchars_to_uchars_with_cmgr(bcs, &mlen, ucs, &wlen, cmgr, all);
	if (ucs)
	{
		/* null-terminate the target buffer if it has room for it. */
		if (wlen < *ucslen) ucs[wlen] = '\0';
		else n = -2; /* buffer too small */
	}
	*bcslen = mlen; *ucslen = wlen;

	return n;
}

int hak_conv_uchars_to_bchars_with_cmgr (const hak_uch_t* ucs, hak_oow_t* ucslen, hak_bch_t* bcs, hak_oow_t* bcslen, hak_cmgr_t* cmgr)
{
	const hak_uch_t* p = ucs;
	const hak_uch_t* end = ucs + *ucslen;
	int ret = 0;

	if (bcs)
	{
		hak_oow_t rem = *bcslen;

		while (p < end)
		{
			hak_oow_t n;

			if (rem <= 0)
			{
				ret = -2; /* buffer too small */
				break;
			}

			n = cmgr->uctobc(*p, bcs, rem);
			if (n == 0)
			{
				ret = -1;
				break; /* illegal character */
			}
			if (n > rem)
			{
				ret = -2; /* buffer too small */
				break;
			}
			bcs += n; rem -= n; p++;
		}

		*bcslen -= rem;
	}
	else
	{
		hak_bch_t bcsbuf[HAK_BCSIZE_MAX];
		hak_oow_t mlen = 0;

		while (p < end)
		{
			hak_oow_t n;

			n = cmgr->uctobc(*p, bcsbuf, HAK_COUNTOF(bcsbuf));
			if (n == 0)
			{
				ret = -1;
				break; /* illegal character */
			}

			/* it assumes that bcsbuf is large enough to hold a character */
			/*HAK_ASSERT(hak, n <= HAK_COUNTOF(bcsbuf));*/

			p++; mlen += n;
		}

		/* this length excludes the terminating null character.
		 * this function doesn't even null-terminate the result. */
		*bcslen = mlen;
	}

	*ucslen = p - ucs;
	return ret;
}

int hak_conv_ucstr_to_bcstr_with_cmgr (const hak_uch_t* ucs, hak_oow_t* ucslen, hak_bch_t* bcs, hak_oow_t* bcslen, hak_cmgr_t* cmgr)
{
	const hak_uch_t* p = ucs;
	int ret = 0;

	if (bcs)
	{
		hak_oow_t rem = *bcslen;

		while (*p != '\0')
		{
			hak_oow_t n;

			if (rem <= 0)
			{
				ret = -2;
				break;
			}

			n = cmgr->uctobc(*p, bcs, rem);
			if (n == 0)
			{
				ret = -1;
				break; /* illegal character */
			}
			if (n > rem)
			{
				ret = -2;
				break; /* buffer too small */
			}

			bcs += n; rem -= n; p++;
		}

		/* update bcslen to the length of the bcs string converted excluding
		 * terminating null */
		*bcslen -= rem;

		/* null-terminate the multibyte sequence if it has sufficient space */
		if (rem > 0) *bcs = '\0';
		else
		{
			/* if ret is -2 and cs[cslen] == '\0',
			 * this means that the bcs buffer was lacking one
			 * slot for the terminating null */
			ret = -2; /* buffer too small */
		}
	}
	else
	{
		hak_bch_t bcsbuf[HAK_BCSIZE_MAX];
		hak_oow_t mlen = 0;

		while (*p != '\0')
		{
			hak_oow_t n;

			n = cmgr->uctobc(*p, bcsbuf, HAK_COUNTOF(bcsbuf));
			if (n == 0)
			{
				ret = -1;
				break; /* illegal character */
			}

			/* it assumes that bcs is large enough to hold a character */
			/*HAK_ASSERT(hak, n <= HAK_COUNTOF(bcs));*/

			p++; mlen += n;
		}

		/* this length holds the number of resulting multi-byte characters
		 * excluding the terminating null character */
		*bcslen = mlen;
	}

	*ucslen = p - ucs;  /* the number of wide characters handled. */
	return ret;
}
