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

#include <hcl-utl.h>
#include <hcl.h>

/* some naming conventions
 *  bchars, uchars -> pointer and length
 *  bcstr, ucstr -> null-terminated string pointer
 *  btouchars -> bchars to uchars
 *  utobchars -> uchars to bchars
 *  btoucstr -> bcstr to ucstr
 *  utobcstr -> ucstr to bcstr
 */

hcl_oow_t hcl_hash_bytes_ (const hcl_oob_t* ptr, hcl_oow_t len)
{
	hcl_oow_t hv;
	HCL_HASH_BYTES (hv, ptr, len);
	/* constrain the hash value to be representable in a small integer
	 * for convenience sake */
	return hv % ((hcl_oow_t)HCL_SMOOI_MAX + 1);
}

/* ----------------------------------------------------------------------- */

int hcl_convbtouchars (hcl_t* hcl, const hcl_bch_t* bcs, hcl_oow_t* bcslen, hcl_uch_t* ucs, hcl_oow_t* ucslen)
{
	/* length bound */
	int n;

	n = hcl_conv_bchars_to_uchars_with_cmgr(bcs, bcslen, ucs, ucslen, HCL_CMGR(hcl), 0);

	if (n <= -1)
	{
		/* -1: illegal character, -2: buffer too small, -3: incomplete sequence */
		hcl_seterrnum (hcl, (n == -2)? HCL_EBUFFULL: HCL_EECERR);
	}

	return n;
}

int hcl_convutobchars (hcl_t* hcl, const hcl_uch_t* ucs, hcl_oow_t* ucslen, hcl_bch_t* bcs, hcl_oow_t* bcslen)
{
	/* length bound */
	int n;

	n = hcl_conv_uchars_to_bchars_with_cmgr(ucs, ucslen, bcs, bcslen, HCL_CMGR(hcl));

	if (n <= -1)
	{
		hcl_seterrnum (hcl, (n == -2)? HCL_EBUFFULL: HCL_EECERR);
	}

	return n;
}

int hcl_convbtoucstr (hcl_t* hcl, const hcl_bch_t* bcs, hcl_oow_t* bcslen, hcl_uch_t* ucs, hcl_oow_t* ucslen)
{
	/* null-terminated. */
	int n;

	n = hcl_conv_bcstr_to_ucstr_with_cmgr(bcs, bcslen, ucs, ucslen, HCL_CMGR(hcl), 0);

	if (n <= -1)
	{
		hcl_seterrnum (hcl, (n == -2)? HCL_EBUFFULL: HCL_EECERR);
	}

	return n;
}

int hcl_convutobcstr (hcl_t* hcl, const hcl_uch_t* ucs, hcl_oow_t* ucslen, hcl_bch_t* bcs, hcl_oow_t* bcslen)
{
	/* null-terminated */
	int n;

	n = hcl_conv_ucstr_to_bcstr_with_cmgr(ucs, ucslen, bcs, bcslen, HCL_CMGR(hcl));

	if (n <= -1)
	{
		hcl_seterrnum (hcl, (n == -2)? HCL_EBUFFULL: HCL_EECERR);
	}

	return n;
}

/* ----------------------------------------------------------------------- */

hcl_uch_t* hcl_dupbtoucharswithheadroom (hcl_t* hcl, hcl_oow_t headroom_bytes, const hcl_bch_t* bcs, hcl_oow_t bcslen, hcl_oow_t* ucslen)
{
	hcl_oow_t inlen, outlen;
	hcl_uch_t* ptr;

	inlen = bcslen;
	if (hcl_convbtouchars (hcl, bcs, &inlen, HCL_NULL, &outlen) <= -1)
	{
		/* note it's also an error if no full conversion is made in this function */
		return HCL_NULL;
	}

	ptr = (hcl_uch_t*)hcl_allocmem(hcl, headroom_bytes + ((outlen + 1) * HCL_SIZEOF(hcl_uch_t)));
	if (HCL_UNLIKELY(!ptr)) return HCL_NULL;

	inlen = bcslen;

	ptr = (hcl_uch_t*)((hcl_oob_t*)ptr + headroom_bytes);
	hcl_convbtouchars (hcl, bcs, &inlen, ptr, &outlen);

	/* hcl_convbtouchars() doesn't null-terminate the target.
	 * but in hcl_dupbtouchars(), i allocate space. so i don't mind
	 * null-terminating it with 1 extra character overhead */
	ptr[outlen] = '\0';
	if (ucslen) *ucslen = outlen;
	return ptr;
}

hcl_uch_t* hcl_dupbtouchars (hcl_t* hcl, const hcl_bch_t* bcs, hcl_oow_t bcslen, hcl_oow_t* ucslen)
{
	return hcl_dupbtoucharswithheadroom (hcl, 0, bcs, bcslen, ucslen);
}

hcl_bch_t* hcl_duputobcharswithheadroom (hcl_t* hcl, hcl_oow_t headroom_bytes, const hcl_uch_t* ucs, hcl_oow_t ucslen, hcl_oow_t* bcslen)
{
	hcl_oow_t inlen, outlen;
	hcl_bch_t* ptr;

	inlen = ucslen;
	if (hcl_convutobchars(hcl, ucs, &inlen, HCL_NULL, &outlen) <= -1)
	{
		/* note it's also an error if no full conversion is made in this function */
		return HCL_NULL;
	}

	ptr = (hcl_bch_t*)hcl_allocmem(hcl, headroom_bytes + ((outlen + 1) * HCL_SIZEOF(hcl_bch_t)));
	if (HCL_UNLIKELY(!ptr)) return HCL_NULL;

	inlen = ucslen;
	ptr = (hcl_bch_t*)((hcl_oob_t*)ptr + headroom_bytes);
	hcl_convutobchars (hcl, ucs, &inlen, ptr, &outlen);

	ptr[outlen] = '\0';
	if (bcslen) *bcslen = outlen;
	return ptr;
}

hcl_bch_t* hcl_duputobchars (hcl_t* hcl, const hcl_uch_t* ucs, hcl_oow_t ucslen, hcl_oow_t* bcslen)
{
	return hcl_duputobcharswithheadroom (hcl, 0, ucs, ucslen, bcslen);
}


/* ----------------------------------------------------------------------- */

hcl_uch_t* hcl_dupbtoucstrwithheadroom (hcl_t* hcl, hcl_oow_t headroom_bytes, const hcl_bch_t* bcs, hcl_oow_t* ucslen)
{
	hcl_oow_t inlen, outlen;
	hcl_uch_t* ptr;

	if (hcl_convbtoucstr(hcl, bcs, &inlen, HCL_NULL, &outlen) <= -1)
	{
		/* note it's also an error if no full conversion is made in this function */
		return HCL_NULL;
	}

	outlen++;
	ptr = (hcl_uch_t*)hcl_allocmem(hcl, headroom_bytes + (outlen * HCL_SIZEOF(hcl_uch_t)));
	if (HCL_UNLIKELY(!ptr)) return HCL_NULL;

	hcl_convbtoucstr (hcl, bcs, &inlen, ptr, &outlen);
	if (ucslen) *ucslen = outlen;
	return ptr;
}

hcl_uch_t* hcl_dupbtoucstr (hcl_t* hcl, const hcl_bch_t* bcs, hcl_oow_t* ucslen)
{
	return hcl_dupbtoucstrwithheadroom (hcl, 0, bcs, ucslen);
}

hcl_bch_t* hcl_duputobcstrwithheadroom (hcl_t* hcl, hcl_oow_t headroom_bytes, const hcl_uch_t* ucs, hcl_oow_t* bcslen)
{
	hcl_oow_t inlen, outlen;
	hcl_bch_t* ptr;

	if (hcl_convutobcstr (hcl, ucs, &inlen, HCL_NULL, &outlen) <= -1)
	{
		/* note it's also an error if no full conversion is made in this function */
		return HCL_NULL;
	}

	outlen++;
	ptr = (hcl_bch_t*)hcl_allocmem(hcl, headroom_bytes + (outlen * HCL_SIZEOF(hcl_bch_t)));
	if (HCL_UNLIKELY(!ptr)) return HCL_NULL;

	ptr = (hcl_bch_t*)((hcl_oob_t*)ptr + headroom_bytes);

	hcl_convutobcstr (hcl, ucs, &inlen, ptr, &outlen);
	if (bcslen) *bcslen = outlen;
	return ptr;
}

hcl_bch_t* hcl_duputobcstr (hcl_t* hcl, const hcl_uch_t* ucs, hcl_oow_t* bcslen)
{
	return hcl_duputobcstrwithheadroom (hcl, 0, ucs, bcslen);
}
/* ----------------------------------------------------------------------- */

hcl_uch_t* hcl_dupuchars (hcl_t* hcl, const hcl_uch_t* ucs, hcl_oow_t ucslen)
{
	hcl_uch_t* ptr;

	ptr = (hcl_uch_t*)hcl_allocmem(hcl, (ucslen + 1) * HCL_SIZEOF(hcl_uch_t));
	if (HCL_UNLIKELY(!ptr)) return HCL_NULL;

	hcl_copy_uchars (ptr, ucs, ucslen);
	ptr[ucslen] = '\0';
	return ptr;
}

hcl_bch_t* hcl_dupbchars (hcl_t* hcl, const hcl_bch_t* bcs, hcl_oow_t bcslen)
{
	hcl_bch_t* ptr;

	ptr = (hcl_bch_t*)hcl_allocmem(hcl, (bcslen + 1) * HCL_SIZEOF(hcl_bch_t));
	if (HCL_UNLIKELY(!ptr)) return HCL_NULL;

	hcl_copy_bchars (ptr, bcs, bcslen);
	ptr[bcslen] = '\0';
	return ptr;
}

hcl_uch_t* hcl_dupucstr (hcl_t* hcl, const hcl_uch_t* ucs, hcl_oow_t* ucslen)
{
	hcl_oow_t len;
	hcl_uch_t* ptr;

	len = hcl_count_ucstr(ucs);
	ptr = hcl_dupuchars(hcl, ucs, len);
	if (HCL_UNLIKELY(!ptr)) return HCL_NULL;

	if (ucslen) *ucslen = len;
	return ptr;
}

hcl_bch_t* hcl_dupbcstr (hcl_t* hcl, const hcl_bch_t* bcs, hcl_oow_t* bcslen)
{
	hcl_oow_t len;
	hcl_bch_t* ptr;

	len = hcl_count_bcstr(bcs);
	ptr = hcl_dupbchars(hcl, bcs, len);
	if (HCL_UNLIKELY(!ptr)) return HCL_NULL;

	if (bcslen) *bcslen = len;
	return ptr;
}

/* ----------------------------------------------------------------------- */

void hcl_add_ntime (hcl_ntime_t* z, const hcl_ntime_t* x, const hcl_ntime_t* y)
{
	hcl_ntime_sec_t xs, ys;
	hcl_ntime_nsec_t ns;

	/*HCL_ASSERT (x->nsec >= 0 && x->nsec < HCL_NSECS_PER_SEC);
	HCL_ASSERT (y->nsec >= 0 && y->nsec < HCL_NSECS_PER_SEC);*/

	ns = x->nsec + y->nsec;
	if (ns >= HCL_NSECS_PER_SEC)
	{
		ns = ns - HCL_NSECS_PER_SEC;
		if (x->sec == HCL_TYPE_MAX(hcl_ntime_sec_t))
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

	if ((ys >= 1 && xs > HCL_TYPE_MAX(hcl_ntime_sec_t) - ys) ||
	    (ys <= -1 && xs < HCL_TYPE_MIN(hcl_ntime_sec_t) - ys))
	{
		if (xs >= 0)
		{
		overflow:
			xs = HCL_TYPE_MAX(hcl_ntime_sec_t);
			ns = HCL_NSECS_PER_SEC - 1;
		}
		else
		{
			xs = HCL_TYPE_MIN(hcl_ntime_sec_t);
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

void hcl_sub_ntime (hcl_ntime_t* z, const hcl_ntime_t* x, const hcl_ntime_t* y)
{
	hcl_ntime_sec_t xs, ys;
	hcl_ntime_nsec_t ns;

	/*HCL_ASSERT (x->nsec >= 0 && x->nsec < HCL_NSECS_PER_SEC);
	HCL_ASSERT (y->nsec >= 0 && y->nsec < HCL_NSECS_PER_SEC);*/

	ns = x->nsec - y->nsec;
	if (ns < 0)
	{
		ns = ns + HCL_NSECS_PER_SEC;
		if (x->sec == HCL_TYPE_MIN(hcl_ntime_sec_t))
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

	if ((ys >= 1 && xs < HCL_TYPE_MIN(hcl_ntime_sec_t) + ys) ||
	    (ys <= -1 && xs > HCL_TYPE_MAX(hcl_ntime_sec_t) + ys))
	{
		if (xs >= 0)
		{
			xs = HCL_TYPE_MAX(hcl_ntime_sec_t);
			ns = HCL_NSECS_PER_SEC - 1;
		}
		else
		{
		underflow:
			xs = HCL_TYPE_MIN(hcl_ntime_sec_t);
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

const hcl_bch_t* hcl_get_base_name_from_bcstr_path (const hcl_bch_t* path)
{
	const hcl_bch_t* p, * last = HCL_NULL;

	for (p = path; *p != '\0'; p++)
	{
		if (HCL_IS_PATH_SEP(*p)) last = p;
	}

	return (last == HCL_NULL)? path: (last + 1);
}

const hcl_uch_t* hcl_get_base_name_from_ucstr_path (const hcl_uch_t* path)
{
	const hcl_uch_t* p, * last = HCL_NULL;

	for (p = path; *p != '\0'; p++)
	{
		if (HCL_IS_PATH_SEP(*p)) last = p;
	}

	return (last == HCL_NULL)? path: (last + 1);
}
