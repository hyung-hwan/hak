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

#include "hcl-prv.h"

/* compiler's literal representation */

#include <hcl-pac1.h>
struct hcl_clit_hdr_t
{
	hcl_uint8_t ver;
	hcl_uint8_t oow_size;
};
typedef struct hcl_clit_hdr_t hcl_clit_hdr_t;
#include <hcl-upac.h>

enum hcl_clit_type_t
{
	HCL_CLIT_STRING = 0x00,
	HCL_CLIT_SYMBOL, /* contained in a cons cell */
	HCL_CLIT_PBIGINT,
	HCL_CLIT_NBIGINT,
	HCL_CLIT_FPDEC_1, /* smooi + smooi */
	HCL_CLIT_FPDEC_2, /* pbigint + smooi */
	HCL_CLIT_FPDEC_3, /* nbigint + smooi */
	HCL_CLIT_PRIM,

	HCL_CLIT_END =0xFF /* end marker. not a real literal type */
};
typedef enum hcl_clit_type_t hcl_clit_type_t;

#define HCL_CLIT_HEADER \
	hcl_oow_t _type: 3; \
	hcl_oow_t _size: (HCL_OOW_BITS - 3)

#define HCL_CLIT_SIZE_MAX ((((hcl_oow_t)1) << 3) >> 3)
	
/* TODO: should clit be chained? these fields in the header?
	hcl_clit_t* _prev;
	hcl_clit_t* _next
*/

struct hcl_clit_t
{
	HCL_CLIT_HEADER;
};
typedef struct hcl_clit_t hcl_clit_t;

/*
struct hcl_clit_string_t
{
	HCL_CLIT_HEADER;
};
typedef struct hcl_clit_string_t hcl_clit_string_t;

struct hcl_clit_symbol_t
{
	HCL_CLIT_HEADER;
};
typedef struct hcl_clit_symbol_t hcl_clit_symbol_t;
*/

struct hcl_clit_fpdec_t
{
	HCL_CLIT_HEADER;
#if 0
	hcl_oop_t value; /* smooi or bigint */
	hcl_oop_t scale; /* smooi, positiv
#else
	/* TODO: how to represent value?? */
	hcl_ooi_t scale;
#endif
};
typedef struct hcl_clit_fpdec_t hcl_clit_fpdec_t;

struct hcl_clit_prim_t
{
	HCL_CLIT_HEADER;
};
typedef struct hcl_clit_prim_t hcl_clit_prim_t;

static hcl_clit_t* alloc_clit (hcl_t* hcl, hcl_clit_type_t type, const void* data, hcl_oow_t size)
{
	hcl_clit_t* clit;

	if (size > HCL_CLIT_SIZE_MAX)
	{
		hcl_seterrnum (hcl, HCL_EINVAL); /* TODO: more specific error messagae... */
		return HCL_NULL;
	}

	clit = (hcl_clit_t*)hcl_allocmem(hcl, HCL_SIZEOF(*clit) + size);
	if (HCL_UNLIKELY(!clit)) return HCL_NULL;

	clit->_type = type;
	clit->_size = size;
	if (size > 0 && data) HCL_MEMCPY (clit + 1, data, size);
	/* if size is greater than 0 and data is HCL_NULL, the allocated space is left uninitialized */

	return clit;
}

hcl_clit_t* hcl_makestringclit (hcl_t* hcl, const hcl_ooch_t* ptr, hcl_oow_t len)
{
	return alloc_clit(hcl, HCL_CLIT_STRING, ptr, len * HCL_SIZEOF(*ptr));
}

hcl_clit_t* hcl_makesymbolclit (hcl_t* hcl, const hcl_ooch_t* ptr, hcl_oow_t len)
{
	return alloc_clit(hcl, HCL_CLIT_SYMBOL, ptr, len * HCL_SIZEOF(*ptr));
}

hcl_clit_t* hcl_makefpdecclit (hcl_t* hcl)
{
	hcl_clit_fpdec_t* fpdec;

	fpdec = (hcl_clit_fpdec_t*)alloc_clit(hcl, HCL_CLIT_FPDEC_1, HCL_NULL, HCL_SIZEOF(*fpdec) - HCL_SIZEOF(hcl_clit_t));
	if (HCL_UNLIKELY(!fpdec)) return HCL_NULL;

	//fpdec-> = argss;
	return (hcl_clit_t*)fpdec;
}

hcl_clit_t* hcl_makeprimclit (hcl_t* hcl)
{
	hcl_clit_prim_t* prim;

	prim = (hcl_clit_prim_t*)alloc_clit(hcl, HCL_CLIT_PRIM, HCL_NULL, HCL_SIZEOF(*prim) - HCL_SIZEOF(hcl_clit_t));
	if (HCL_UNLIKELY(!prim)) return HCL_NULL;

	//prim-> = argss;
	return (hcl_clit_t*)prim;
}

void hcl_freeclit (hcl_t* hcl, hcl_clit_t* clit)
{
	hcl_freemem(hcl, clit);
}

#if 0
static HCL_INLINE hcl_oop_t alloc_numeric_array (hcl_t* hcl, int brand, const void* ptr, hcl_oow_t len, hcl_obj_type_t type, hcl_oow_t unit, int extra, int ngc)
{
	/* allocate a variable object */

	hcl_oop_t hdr;
	hcl_oow_t xbytes, nbytes, nbytes_aligned;

	xbytes = len * unit;
	/* 'extra' indicates an extra unit to append at the end.
	 * it's useful to store a string with a terminating null */
	nbytes = extra? xbytes + unit: xbytes;
	nbytes_aligned = HCL_ALIGN(nbytes, HCL_SIZEOF(hcl_oop_t));
/* TODO: check overflow in size calculation*/

	/* making the number of bytes to allocate a multiple of
	 * HCL_SIZEOF(hcl_oop_t) will guarantee the starting address
	 * of the allocated space to be an even number.
	 * see HCL_OOP_IS_NUMERIC() and HCL_OOP_IS_POINTER() */
	if (HCL_UNLIKELY(ngc))
		hdr = (hcl_oop_t)hcl_callocmem(hcl, HCL_SIZEOF(hcl_obj_t) + nbytes_aligned);
	else
		hdr = (hcl_oop_t)hcl_allocbytes(hcl, HCL_SIZEOF(hcl_obj_t) + nbytes_aligned);
	if (HCL_UNLIKELY(!hdr)) return HCL_NULL;

	hdr->_flags = HCL_OBJ_MAKE_FLAGS(type, unit, extra, 0, 0, ngc, 0, 0);
	hdr->_size = len;
	HCL_OBJ_SET_SIZE (hdr, len);
	/*HCL_OBJ_SET_CLASS (hdr, hcl->_nil);*/
	HCL_OBJ_SET_FLAGS_BRAND (hdr, brand);

	if (ptr)
	{
		/* copy data */
		HCL_MEMCPY (hdr + 1, ptr, xbytes);
		HCL_MEMSET ((hcl_uint8_t*)(hdr + 1) + xbytes, 0, nbytes_aligned - xbytes);
	}
	else
	{
		/* initialize with zeros when the string pointer is not given */
		HCL_MEMSET ((hdr + 1), 0, nbytes_aligned);
	}

	return hdr;
}

hcl_oop_t hcl_alloccharobj (hcl_t* hcl, int brand, const hcl_ooch_t* ptr, hcl_oow_t len)
{
	return alloc_numeric_array(hcl, brand, ptr, len, HCL_OBJ_TYPE_CHAR, HCL_SIZEOF(hcl_ooch_t), 1, 0);
}

hcl_oop_t hcl_allocbyteobj (hcl_t* hcl, int brand, const hcl_oob_t* ptr, hcl_oow_t len)
{
	return alloc_numeric_array(hcl, brand, ptr, len, HCL_OBJ_TYPE_BYTE, HCL_SIZEOF(hcl_oob_t), 0, 0);
}

hcl_oop_t hcl_allochalfwordobj (hcl_t* hcl, int brand, const hcl_oohw_t* ptr, hcl_oow_t len)
{
	return alloc_numeric_array(hcl, brand, ptr, len, HCL_OBJ_TYPE_HALFWORD, HCL_SIZEOF(hcl_oohw_t), 0, 0);
}

hcl_oop_t hcl_allocwordobj (hcl_t* hcl, int brand, const hcl_oow_t* ptr, hcl_oow_t len)
{
	return alloc_numeric_array(hcl, brand, ptr, len, HCL_OBJ_TYPE_WORD, HCL_SIZEOF(hcl_oow_t), 0, 0);
}
#endif

#if 0
static int add_literal (hcl_t* hcl, hcl_oop_t obj, hcl_oow_t* index)
{
	hcl_oow_t capa, i, lfbase = 0;
	hcl_oop_t tmp;

	lfbase = (hcl->option.trait & HCL_TRAIT_INTERACTIVE)? hcl->c->fnblk.info[hcl->c->fnblk.depth].lfbase: 0;

	/* TODO: speed up the following duplicate check loop */
	for (i = lfbase; i < hcl->code.lit.len; i++)
	{
		tmp = ((hcl_oop_oop_t)hcl->code.lit.arr)->slot[i];

		if (tmp == obj)
		{
			/* this removes redundancy of symbols, characters, and integers. */
			*index = i - lfbase;
			return 0;
		}
		else if (HCL_IS_STRING(hcl, obj) && HCL_IS_STRING(hcl, tmp) && hcl_equalobjs(hcl, obj, tmp))
		{
			/* a string object requires equality check. however, the string created to the literal frame
			 * must be made immutable. non-immutable string literals are source of various problems */
			*index = i - lfbase;
			return 0;
		}
	}

	capa = HCL_OBJ_GET_SIZE(hcl->code.lit.arr);
	if (hcl->code.lit.len >= capa)
	{
		hcl_oop_t tmp;
		hcl_oow_t newcapa;

		newcapa = HCL_ALIGN(capa + 1, HCL_LIT_BUFFER_ALIGN);
		tmp = hcl_remakengcarray(hcl, (hcl_oop_t)hcl->code.lit.arr, newcapa);
		if (HCL_UNLIKELY(!tmp)) return -1;

		hcl->code.lit.arr = (hcl_oop_oop_t)tmp;
	}

	*index = hcl->code.lit.len - lfbase;

	((hcl_oop_oop_t)hcl->code.lit.arr)->slot[hcl->code.lit.len++] = obj;
	/* TODO: RDONLY? */
	/*if (HCL_IS_OOP_POINTER(obj)) HCL_OBJ_SET_FLAGS_RDONLY(obj, 1); */
	return 0;
}
#endif


struct hcl_clit_frame_t
{
	int x;
};

typedef struct hcl_clit_frame_t hcl_clit_frame_t;

/* 
 * B(1) | LEN(8) | DATA |
 * DATA is B-specific.
 */

typedef int (*hcl_clit_reader_t) (
	hcl_t*      hcl,
	void*       buf,
	hcl_oow_t*  len,
	void*       ctx
);

typedef int (*hcl_clit_writer_t) (
	hcl_t*      hcl,
	const void* ptr,
	hcl_oow_t   len,
	void*       ctx
);

int hcl_writeclits (hcl_t* hcl, hcl_clit_writer_t wrtr, void* ctx)
{
	hcl_oow_t i, lfbase = 0;
	hcl_oop_t tmp;
	int brand;
	hcl_oow_t tsize;
	hcl_uint8_t b;
	hcl_oow_t w;
	hcl_clit_hdr_t h;
	
	lfbase = (hcl->option.trait & HCL_TRAIT_INTERACTIVE)? hcl->c->fnblk.info[hcl->c->fnblk.depth].lfbase: 0;

	/* start with a header */
	h.ver = 1;
	h.oow_size = (hcl_uint8_t)HCL_SIZEOF(hcl_oow_t); /* the size must not exceed 256 */
	if (wrtr(hcl, &h, HCL_SIZEOF(h), ctx) <= -1) goto oops;

	/* write actual literals */
	for (i = lfbase; i < hcl->code.lit.len; i++)
	{
		tmp = ((hcl_oop_oop_t)hcl->code.lit.arr)->slot[i];
		brand = HCL_OBJ_GET_FLAGS_BRAND(tmp);
		tsize = HCL_OBJ_GET_SIZE(tmp);

		switch (brand)
		{
			case HCL_BRAND_PBIGINT:
			case HCL_BRAND_NBIGINT:
			{
				hcl_oow_t nbytes;
				hcl_oow_t j;
				hcl_liw_t liw;

				/* write the brand */
				b = (brand == HCL_BRAND_PBIGINT ? HCL_CLIT_PBIGINT : HCL_CLIT_NBIGINT);
				if (wrtr(hcl, &b, 1, ctx) <= -1) goto oops;

				/* write the number of bytes in the little-endian */
				nbytes = tsize * HCL_SIZEOF(hcl_liw_t);
				w = hcl_htoleoow(nbytes);
				if (wrtr(hcl, &w, HCL_SIZEOF(w), ctx) <= -1) goto oops;

				for (j = 0; j < tsize; j++)
				{
					liw = HCL_OBJ_GET_LIWORD_VAL(tmp, j);
					liw = hcl_htoleliw(liw);
					if (wrtr(hcl, &liw, HCL_SIZEOF(liw), ctx) <= -1) goto oops;
				}
				break;
			}

			case HCL_BRAND_CONS:
			{
				/* write 1-byte brand */
				b = (hcl_uint8_t)HCL_CLIT_SYMBOL;
				if (wrtr(hcl, &b, 1, ctx) <= -1) goto oops;

				/* get the symbol at CAR and make it as if it is the current object processed.*/
				tmp = HCL_CONS_CAR(tmp);
				brand = HCL_OBJ_GET_FLAGS_BRAND(tmp);
				tsize = HCL_OBJ_GET_SIZE(tmp);

				HCL_ASSERT(hcl, brand == HCL_BRAND_SYMBOL);
				goto string_body;
			}

			case HCL_BRAND_FPDEC:
			{
				hcl_oop_fpdec_t f;

				f = (hcl_oop_fpdec_t)tmp;

				HCL_ASSERT (hcl, HCL_OOP_IS_SMOOI(f->scale));
				HCL_ASSERT(hcl, HCL_OOP_IS_SMOOI(f->value) || HCL_OOP_IS_POINTER(f->value));

				/* write 1-byte brand */
				if (HCL_OOP_IS_SMOOI(f->value)) b = HCL_CLIT_FPDEC_1;
				else if (HCL_IS_PBIGINT(hcl, f->value)) b = HCL_CLIT_FPDEC_2;
				else
				{
					HCL_ASSERT(hcl, HCL_IS_NBIGINT(hcl, f->value));
					b = HCL_CLIT_FPDEC_2;
				}
				if (wrtr(hcl, &b, 1, ctx) <= -1) goto oops;

				/* cast the scale part from hcl_ooi_t to hcl_oow_t */
				w = hcl_htoleoow((hcl_oow_t)f->scale);
				if (wrtr(hcl, &w, HCL_SIZEOF(w), ctx) <= -1) goto oops;

				/* TODO: write the value part */

				break;
			}

			case HCL_BRAND_STRING:
			{
			#if defined(HCL_OOCH_IS_UCH)
				hcl_uch_t* ucsptr;
				hcl_oow_t ucspos, ucslen;
				hcl_bch_t bcsbuf[128];
				hcl_oow_t bcslen;
				int n;

				/* write 1-byte brand */
				b = (hcl_uint8_t)HCL_CLIT_STRING;
				if (wrtr(hcl, &b, 1, ctx) <= -1) goto oops;

			string_body:
				ucsptr = HCL_OBJ_GET_CHAR_SLOT(tmp);
				ucslen = tsize;
				if (hcl_convutobchars(hcl, ucsptr, &ucslen, HCL_NULL, &bcslen) <= -1) goto oops;

				/* write the number of bytes in the little-endian */
				w = hcl_htoleoow(bcslen);
				if (wrtr(hcl, &w, HCL_SIZEOF(w), ctx) <= -1) goto oops;

				/* write string in bytess and write to the callback.*/
				ucspos = 0;
				while (ucspos < tsize)
				{
					bcslen = HCL_COUNTOF(bcsbuf);
					ucslen = tsize - ucspos;
					n = hcl_convutobchars(hcl, &ucsptr[ucspos], &ucslen, bcsbuf, &bcslen);
					if (n <= -1 && bcslen == 0) goto oops;
					if (wrtr(hcl, bcsbuf, bcslen, ctx) <= -1) goto oops;
					ucspos += ucslen;
				}
			#else
				w = hcl_htoleoow(tsize);
				if (wrtr(hcl, &w, HCL_SIZEOF(w), ctx) <= -1) goto oops;
				if (wrtr(hcl, HCL_OBJ_GET_CHAR_SLOT(tmp), tsize, ctx) <= -1) goto oops;
			#endif
				break;
			}

			case HCL_BRAND_PRIM:
				break;
		}
	}

	b = HCL_CLIT_END;
	if (wrtr(hcl, &b, 1, ctx) <= -1) goto oops;
	return 0;

oops:
	return -1;
}

int hcl_restoreclits(hcl_t* hcl, hcl_clit_reader_t rdr, void* ctx)
{
	int n;
	hcl_uint8_t buf[128];
	hcl_oow_t len, i;

	while (1)
	{
		len = HCL_COUNTOF(buf);
		n = rdr(hcl, buf, &len, ctx);
		if (n <= -1) goto oops;
		if (n == 0) break;

		for (i = 0; i < len; i++)
		{
			switch (buf[i])
			{
				case HCL_BRAND_PBIGINT:
				case HCL_BRAND_NBIGINT:
					break;
				case HCL_BRAND_STRING:
					break;
			}
		}
	}

/* TODO: have i SEEN HCL_CLIT_END??? otherwise, incomplete... */
	return 0;

oops:
	return -1;
}