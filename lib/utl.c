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

#include <hak-utl.h>
#include <hak-str.h>
#include <hak.h>

/* some naming conventions
 *  bchars, uchars -> pointer and length
 *  bcstr, ucstr -> null-terminated string pointer
 *  btouchars -> bchars to uchars
 *  utobchars -> uchars to bchars
 *  btoucstr -> bcstr to ucstr
 *  utobcstr -> ucstr to bcstr
 */

hak_oow_t hak_hash_bytes_ (const hak_oob_t* ptr, hak_oow_t len)
{
	hak_oow_t hv;
	HAK_HASH_BYTES (hv, ptr, len);
	/* constrain the hash value to be representable in a small integer
	 * for convenience sake */
	return hv % ((hak_oow_t)HAK_SMOOI_MAX + 1);
}

/* ----------------------------------------------------------------------- */

int hak_convbtouchars (hak_t* hak, const hak_bch_t* bcs, hak_oow_t* bcslen, hak_uch_t* ucs, hak_oow_t* ucslen)
{
	/* length bound */
	int n;

	n = hak_conv_bchars_to_uchars_with_cmgr(bcs, bcslen, ucs, ucslen, HAK_CMGR(hak), 0);

	if (n <= -1)
	{
		/* -1: illegal character, -2: buffer too small, -3: incomplete sequence */
		hak_seterrnum(hak, (n == -2)? HAK_EBUFFULL: HAK_EECERR);
	}

	return n;
}

int hak_convutobchars (hak_t* hak, const hak_uch_t* ucs, hak_oow_t* ucslen, hak_bch_t* bcs, hak_oow_t* bcslen)
{
	/* length bound */
	int n;

	n = hak_conv_uchars_to_bchars_with_cmgr(ucs, ucslen, bcs, bcslen, HAK_CMGR(hak));

	if (n <= -1)
	{
		hak_seterrnum(hak, (n == -2)? HAK_EBUFFULL: HAK_EECERR);
	}

	return n;
}

int hak_convbtoucstr (hak_t* hak, const hak_bch_t* bcs, hak_oow_t* bcslen, hak_uch_t* ucs, hak_oow_t* ucslen)
{
	/* null-terminated. */
	int n;

	n = hak_conv_bcstr_to_ucstr_with_cmgr(bcs, bcslen, ucs, ucslen, HAK_CMGR(hak), 0);

	if (n <= -1)
	{
		hak_seterrnum(hak, (n == -2)? HAK_EBUFFULL: HAK_EECERR);
	}

	return n;
}

int hak_convutobcstr (hak_t* hak, const hak_uch_t* ucs, hak_oow_t* ucslen, hak_bch_t* bcs, hak_oow_t* bcslen)
{
	/* null-terminated */
	int n;

	n = hak_conv_ucstr_to_bcstr_with_cmgr(ucs, ucslen, bcs, bcslen, HAK_CMGR(hak));

	if (n <= -1)
	{
		hak_seterrnum(hak, (n == -2)? HAK_EBUFFULL: HAK_EECERR);
	}

	return n;
}

/* ----------------------------------------------------------------------- */

hak_uch_t* hak_dupbtoucharswithheadroom (hak_t* hak, hak_oow_t headroom_bytes, const hak_bch_t* bcs, hak_oow_t bcslen, hak_oow_t* ucslen)
{
	hak_oow_t inlen, outlen;
	hak_uch_t* ptr;

	inlen = bcslen;
	if (hak_convbtouchars(hak, bcs, &inlen, HAK_NULL, &outlen) <= -1)
	{
		/* note it's also an error if no full conversion is made in this function */
		return HAK_NULL;
	}

	ptr = (hak_uch_t*)hak_allocmem(hak, headroom_bytes + ((outlen + 1) * HAK_SIZEOF(hak_uch_t)));
	if (HAK_UNLIKELY(!ptr)) return HAK_NULL;

	inlen = bcslen;

	ptr = (hak_uch_t*)((hak_oob_t*)ptr + headroom_bytes);
	hak_convbtouchars(hak, bcs, &inlen, ptr, &outlen);

	/* hak_convbtouchars() doesn't null-terminate the target.
	 * but in hak_dupbtouchars(), i allocate space. so i don't mind
	 * null-terminating it with 1 extra character overhead */
	ptr[outlen] = '\0';
	if (ucslen) *ucslen = outlen;
	return ptr;
}

hak_uch_t* hak_dupbtouchars (hak_t* hak, const hak_bch_t* bcs, hak_oow_t bcslen, hak_oow_t* ucslen)
{
	return hak_dupbtoucharswithheadroom(hak, 0, bcs, bcslen, ucslen);
}

hak_bch_t* hak_duputobcharswithheadroom (hak_t* hak, hak_oow_t headroom_bytes, const hak_uch_t* ucs, hak_oow_t ucslen, hak_oow_t* bcslen)
{
	hak_oow_t inlen, outlen;
	hak_bch_t* ptr;

	inlen = ucslen;
	if (hak_convutobchars(hak, ucs, &inlen, HAK_NULL, &outlen) <= -1)
	{
		/* note it's also an error if no full conversion is made in this function */
		return HAK_NULL;
	}

	ptr = (hak_bch_t*)hak_allocmem(hak, headroom_bytes + ((outlen + 1) * HAK_SIZEOF(hak_bch_t)));
	if (HAK_UNLIKELY(!ptr)) return HAK_NULL;

	inlen = ucslen;
	ptr = (hak_bch_t*)((hak_oob_t*)ptr + headroom_bytes);
	hak_convutobchars(hak, ucs, &inlen, ptr, &outlen);

	ptr[outlen] = '\0';
	if (bcslen) *bcslen = outlen;
	return ptr;
}

hak_bch_t* hak_duputobchars (hak_t* hak, const hak_uch_t* ucs, hak_oow_t ucslen, hak_oow_t* bcslen)
{
	return hak_duputobcharswithheadroom(hak, 0, ucs, ucslen, bcslen);
}


/* ----------------------------------------------------------------------- */

hak_uch_t* hak_dupbtoucstrwithheadroom (hak_t* hak, hak_oow_t headroom_bytes, const hak_bch_t* bcs, hak_oow_t* ucslen)
{
	hak_oow_t inlen, outlen;
	hak_uch_t* ptr;

	if (hak_convbtoucstr(hak, bcs, &inlen, HAK_NULL, &outlen) <= -1)
	{
		/* note it's also an error if no full conversion is made in this function */
		return HAK_NULL;
	}

	outlen++;
	ptr = (hak_uch_t*)hak_allocmem(hak, headroom_bytes + (outlen * HAK_SIZEOF(hak_uch_t)));
	if (HAK_UNLIKELY(!ptr)) return HAK_NULL;

	hak_convbtoucstr(hak, bcs, &inlen, ptr, &outlen);
	if (ucslen) *ucslen = outlen;
	return ptr;
}

hak_uch_t* hak_dupbtoucstr (hak_t* hak, const hak_bch_t* bcs, hak_oow_t* ucslen)
{
	return hak_dupbtoucstrwithheadroom(hak, 0, bcs, ucslen);
}

hak_bch_t* hak_duputobcstrwithheadroom (hak_t* hak, hak_oow_t headroom_bytes, const hak_uch_t* ucs, hak_oow_t* bcslen)
{
	hak_oow_t inlen, outlen;
	hak_bch_t* ptr;

	if (hak_convutobcstr(hak, ucs, &inlen, HAK_NULL, &outlen) <= -1)
	{
		/* note it's also an error if no full conversion is made in this function */
		return HAK_NULL;
	}

	outlen++;
	ptr = (hak_bch_t*)hak_allocmem(hak, headroom_bytes + (outlen * HAK_SIZEOF(hak_bch_t)));
	if (HAK_UNLIKELY(!ptr)) return HAK_NULL;

	ptr = (hak_bch_t*)((hak_oob_t*)ptr + headroom_bytes);

	hak_convutobcstr(hak, ucs, &inlen, ptr, &outlen);
	if (bcslen) *bcslen = outlen;
	return ptr;
}

hak_bch_t* hak_duputobcstr (hak_t* hak, const hak_uch_t* ucs, hak_oow_t* bcslen)
{
	return hak_duputobcstrwithheadroom(hak, 0, ucs, bcslen);
}
/* ----------------------------------------------------------------------- */

hak_uch_t* hak_dupuchars (hak_t* hak, const hak_uch_t* ucs, hak_oow_t ucslen)
{
	hak_uch_t* ptr;

	ptr = (hak_uch_t*)hak_allocmem(hak, (ucslen + 1) * HAK_SIZEOF(hak_uch_t));
	if (HAK_UNLIKELY(!ptr)) return HAK_NULL;

	hak_copy_uchars (ptr, ucs, ucslen);
	ptr[ucslen] = '\0';
	return ptr;
}

hak_bch_t* hak_dupbchars (hak_t* hak, const hak_bch_t* bcs, hak_oow_t bcslen)
{
	hak_bch_t* ptr;

	ptr = (hak_bch_t*)hak_allocmem(hak, (bcslen + 1) * HAK_SIZEOF(hak_bch_t));
	if (HAK_UNLIKELY(!ptr)) return HAK_NULL;

	hak_copy_bchars (ptr, bcs, bcslen);
	ptr[bcslen] = '\0';
	return ptr;
}

hak_uch_t* hak_dupucstr (hak_t* hak, const hak_uch_t* ucs, hak_oow_t* ucslen)
{
	hak_oow_t len;
	hak_uch_t* ptr;

	len = hak_count_ucstr(ucs);
	ptr = hak_dupuchars(hak, ucs, len);
	if (HAK_UNLIKELY(!ptr)) return HAK_NULL;

	if (ucslen) *ucslen = len;
	return ptr;
}

hak_bch_t* hak_dupbcstr (hak_t* hak, const hak_bch_t* bcs, hak_oow_t* bcslen)
{
	hak_oow_t len;
	hak_bch_t* ptr;

	len = hak_count_bcstr(bcs);
	ptr = hak_dupbchars(hak, bcs, len);
	if (HAK_UNLIKELY(!ptr)) return HAK_NULL;

	if (bcslen) *bcslen = len;
	return ptr;
}

/* ----------------------------------------------------------------------- */

void hak_add_ntime (hak_ntime_t* z, const hak_ntime_t* x, const hak_ntime_t* y)
{
	hak_ntime_sec_t xs, ys;
	hak_ntime_nsec_t ns;

	/*HAK_ASSERT(x->nsec >= 0 && x->nsec < HAK_NSECS_PER_SEC);
	HAK_ASSERT(y->nsec >= 0 && y->nsec < HAK_NSECS_PER_SEC);*/

	ns = x->nsec + y->nsec;
	if (ns >= HAK_NSECS_PER_SEC)
	{
		ns = ns - HAK_NSECS_PER_SEC;
		if (x->sec == HAK_TYPE_MAX(hak_ntime_sec_t))
		{
			if (y->sec >= 0) goto overflow;
			xs = x->sec;
			ys = y->sec + 1; /* this won't overflow */
		}
		else
		{
			xs = x->sec + 1; /* this won't overflow */
			ys = y->sec;
		}
	}
	else
	{
		xs = x->sec;
		ys = y->sec;
	}

	if ((ys >= 1 && xs > HAK_TYPE_MAX(hak_ntime_sec_t) - ys) ||
	    (ys <= -1 && xs < HAK_TYPE_MIN(hak_ntime_sec_t) - ys))
	{
		if (xs >= 0)
		{
		overflow:
			xs = HAK_TYPE_MAX(hak_ntime_sec_t);
			ns = HAK_NSECS_PER_SEC - 1;
		}
		else
		{
			xs = HAK_TYPE_MIN(hak_ntime_sec_t);
			ns = 0;
		}
	}
	else
	{
		xs = xs + ys;
	}

	z->sec = xs;
	z->nsec = ns;
}

void hak_sub_ntime (hak_ntime_t* z, const hak_ntime_t* x, const hak_ntime_t* y)
{
	hak_ntime_sec_t xs, ys;
	hak_ntime_nsec_t ns;

	/*HAK_ASSERT(x->nsec >= 0 && x->nsec < HAK_NSECS_PER_SEC);
	HAK_ASSERT(y->nsec >= 0 && y->nsec < HAK_NSECS_PER_SEC);*/

	ns = x->nsec - y->nsec;
	if (ns < 0)
	{
		ns = ns + HAK_NSECS_PER_SEC;
		if (x->sec == HAK_TYPE_MIN(hak_ntime_sec_t))
		{
			if (y->sec <= 0) goto underflow;
			xs = x->sec;
			ys = y->sec - 1; /* this won't underflow */
		}
		else
		{
			xs = x->sec - 1; /* this won't underflow */
			ys = y->sec;
		}
	}
	else
	{
		xs = x->sec;
		ys = y->sec;
	}

	if ((ys >= 1 && xs < HAK_TYPE_MIN(hak_ntime_sec_t) + ys) ||
	    (ys <= -1 && xs > HAK_TYPE_MAX(hak_ntime_sec_t) + ys))
	{
		if (xs >= 0)
		{
			xs = HAK_TYPE_MAX(hak_ntime_sec_t);
			ns = HAK_NSECS_PER_SEC - 1;
		}
		else
		{
		underflow:
			xs = HAK_TYPE_MIN(hak_ntime_sec_t);
			ns = 0;
		}
	}
	else
	{
		xs = xs - ys;
	}

	z->sec = xs;
	z->nsec = ns;
}

/* ----------------------------------------------------------------------- */

const hak_bch_t* hak_get_base_name_from_bcstr_path (const hak_bch_t* path)
{
	const hak_bch_t* p, * last = HAK_NULL;

	for (p = path; *p != '\0'; p++)
	{
		if (HAK_IS_PATH_SEP(*p)) last = p;
	}

	return (last == HAK_NULL)? path: (last + 1);
}

const hak_uch_t* hak_get_base_name_from_ucstr_path (const hak_uch_t* path)
{
	const hak_uch_t* p, * last = HAK_NULL;

	for (p = path; *p != '\0'; p++)
	{
		if (HAK_IS_PATH_SEP(*p)) last = p;
	}

	return (last == HAK_NULL)? path: (last + 1);
}
