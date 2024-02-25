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
struct hcl_xchg_hdr_t
{
	hcl_uint8_t ver;
	hcl_uint8_t oow_size;
};
typedef struct hcl_xchg_hdr_t hcl_xchg_hdr_t;
#include <hcl-upac.h>

enum hcl_xchg_type_t
{
	/* byte code */
	HCL_XCHG_BC = 0x00,

	/* literals */
	HCL_XCHG_STRING_U,
	HCL_XCHG_STRING_B,
	HCL_XCHG_SYMBOL_U, /* contained in a cons cell */
	HCL_XCHG_SYMBOL_B, /* contained in a cons cell */
	HCL_XCHG_SMOOI,
	HCL_XCHG_PBIGINT,
	HCL_XCHG_NBIGINT,
	HCL_XCHG_FPDEC_1, /* smooi + smooi */
	HCL_XCHG_FPDEC_2, /* pbigint + smooi */
	HCL_XCHG_FPDEC_3, /* nbigint + smooi */
	HCL_XCHG_PRIM,

	/* end marker */
	HCL_XCHG_END =0xFF /* end marker. not a real literal type */
};
typedef enum hcl_xchg_type_t hcl_xchg_type_t;

/* -------------------------------------------------------------------- */

int hcl_marshalcode (hcl_t* hcl, const hcl_code_t* code, hcl_xchg_writer_t wrtr, void* ctx)
{
	hcl_oow_t i, lfbase = 0;
	hcl_oop_t tmp;
	int brand;
	hcl_oow_t tsize;
	hcl_uint8_t b;
	hcl_oow_t w;
	hcl_xchg_hdr_t h;
	
	lfbase = (hcl->option.trait & HCL_TRAIT_INTERACTIVE)? hcl->c->fnblk.info[hcl->c->fnblk.depth].lfbase: 0;

	/* start with a header */
	h.ver = 1;
	h.oow_size = (hcl_uint8_t)HCL_SIZEOF(hcl_oow_t); /* the size must not exceed 256 */
	if (wrtr(hcl, &h, HCL_SIZEOF(h), ctx) <= -1) goto oops;

	/* write the byte-code */
	b = HCL_XCHG_BC;
	if (wrtr(hcl, &b, HCL_SIZEOF(b), ctx) <= -1) goto oops;
	w = hcl_htoleoow(code->bc.len);
	if (wrtr(hcl, &w, HCL_SIZEOF(w), ctx) <= -1) goto oops;
	if (wrtr(hcl, code->bc.ptr, code->bc.len, ctx) <= -1) goto oops;

	/* write actual literals */
	for (i = lfbase; i < code->lit.len; i++)
	{
		tmp = ((hcl_oop_oop_t)code->lit.arr)->slot[i];
		if (HCL_OOP_IS_SMOOI(tmp))
		{
			b = HCL_XCHG_SMOOI;
			if (wrtr(hcl, &b, HCL_SIZEOF(b), ctx) <= -1) goto oops;
			w = hcl_htoleoow((hcl_oow_t)HCL_OOP_TO_SMOOI(tmp));
			if (wrtr(hcl, &w, HCL_SIZEOF(w), ctx) <= -1) goto oops;
			continue;
		}

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
				b = (brand == HCL_BRAND_PBIGINT ? HCL_XCHG_PBIGINT : HCL_XCHG_NBIGINT);
				if (wrtr(hcl, &b, HCL_SIZEOF(b), ctx) <= -1) goto oops;

			bigint_body:
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

			case HCL_BRAND_FPDEC:
			{
				hcl_oop_fpdec_t f;

				f = (hcl_oop_fpdec_t)tmp;
				HCL_ASSERT (hcl, HCL_OOP_IS_SMOOI(f->scale));
				HCL_ASSERT(hcl, HCL_OOP_IS_SMOOI(f->value) || HCL_OOP_IS_POINTER(f->value));

				/* write 1-byte brand */
				if (HCL_OOP_IS_SMOOI(f->value)) b = HCL_XCHG_FPDEC_1;
				else if (HCL_IS_PBIGINT(hcl, f->value)) b = HCL_XCHG_FPDEC_2;
				else
				{
					HCL_ASSERT(hcl, HCL_IS_NBIGINT(hcl, f->value));
					b = HCL_XCHG_FPDEC_2;
				}
				if (wrtr(hcl, &b, HCL_SIZEOF(b), ctx) <= -1) goto oops;

				/* cast the scale part from hcl_ooi_t to hcl_oow_t and write it */
				w = hcl_htoleoow((hcl_oow_t)HCL_OOP_TO_SMOOI(f->scale));
				if (wrtr(hcl, &w, HCL_SIZEOF(w), ctx) <= -1) goto oops;

				/* write the value part */
				if (b == HCL_XCHG_FPDEC_1)
				{
					w = hcl_htoleoow((hcl_oow_t)HCL_OOP_TO_SMOOI(f->value));
					if (wrtr(hcl, &w, HCL_SIZEOF(w), ctx) <= -1) goto oops;
				}
				else
				{
					tmp = f->value;
					brand = HCL_OBJ_GET_FLAGS_BRAND(tmp);
					tsize = HCL_OBJ_GET_SIZE(tmp);
					goto bigint_body;
				}
				break;
			}

			case HCL_BRAND_CONS:
			{
				/* write 1-byte brand */
			#if defined(HCL_OOCH_IS_UCH)
				b = (hcl_uint8_t)HCL_XCHG_SYMBOL_U;
			#else
				b = (hcl_uint8_t)HCL_XCHG_SYMBOL_B;
			#endif
				if (wrtr(hcl, &b, HCL_SIZEOF(b), ctx) <= -1) goto oops;

				/* get the symbol at CAR and make it as if it is the current object processed.*/
				tmp = HCL_CONS_CAR(tmp);
				brand = HCL_OBJ_GET_FLAGS_BRAND(tmp);
				tsize = HCL_OBJ_GET_SIZE(tmp);

				HCL_ASSERT(hcl, brand == HCL_BRAND_SYMBOL);
				goto string_body;
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
				b = (hcl_uint8_t)HCL_XCHG_STRING_U;
				if (wrtr(hcl, &b, HCL_SIZEOF(b), ctx) <= -1) goto oops;

			string_body:
				ucsptr = HCL_OBJ_GET_CHAR_SLOT(tmp);
				ucslen = tsize;
				if (hcl_convutobchars(hcl, ucsptr, &ucslen, HCL_NULL, &bcslen) <= -1) goto oops;

				/* write the number of characters in the little endian */
				w = hcl_htoleoow(tsize);
				if (wrtr(hcl, &w, HCL_SIZEOF(w), ctx) <= -1) goto oops;

				/* write the number of bytes in the little endian */
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
				/* write 1-byte brand */
				b = (hcl_uint8_t)HCL_XCHG_BSTRING;
				if (wrtr(hcl, &b, HCL_SIZEOF(b), ctx) <= -1) goto oops;

			string_body:
				w = hcl_htoleoow(tsize);
				if (wrtr(hcl, &w, HCL_SIZEOF(w), ctx) <= -1) goto oops;

				if (wrtr(hcl, HCL_OBJ_GET_CHAR_SLOT(tmp), tsize, ctx) <= -1) goto oops;
			#endif
				break;
			}

			case HCL_BRAND_PRIM:
				/* TODO: can't have resolved pointer... need module name and the functio name??? */
				break;
		}
	}

	b = HCL_XCHG_END;
	if (wrtr(hcl, &b, HCL_SIZEOF(b), ctx) <= -1) goto oops;
	return 0;

oops:
	return -1;
}

/* -------------------------------------------------------------------- */

static void set_rdr_ioerr (hcl_t* hcl, const hcl_bch_t* msg)
{
	const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
	hcl_seterrbfmt(hcl, HCL_EIOERR, "%hs - %js", orgmsg);
}

int hcl_unmarshalcode (hcl_t* hcl, hcl_code_t* code, hcl_xchg_reader_t rdr, void* ctx)
{
	int n;
	hcl_xchg_hdr_t h;
	hcl_uint8_t b;
	hcl_oow_t w;

	/* [NOTE]
	 *  this function may pollute the code data when it fails because it doesn't
	 *  roll back changed made to the memory pointed to by 'code'. the caller side
	 *  may use two code structs. and switch between them for each call to hcl_unmarshalcode()
	 *  to avoid this issue.
	 */

	if (hcl_brewcode(hcl, code) <= -1) goto oops;

	n = rdr(hcl, &h, HCL_SIZEOF(h), ctx);
	if (n <= -1)
	{
		set_rdr_ioerr(hcl, "erroneous or insufficient header");
		goto oops;
	}

	if (h.ver != 1)
	{
		hcl_seterrbfmt(hcl, HCL_EIOERR, "unsupported header version %d", (int)h.ver);
		goto oops;
	}

	if (h.oow_size != HCL_SIZEOF(hcl_oow_t))
	{
		/* no support for cross-architecture exchange yet */
		hcl_seterrbfmt(hcl, HCL_EIOERR, "unsupported word size %d", (int)h.oow_size);
		goto oops;
	}

	while (1)
	{
		/* read 1-byte brand */
		n = rdr(hcl, &b, HCL_SIZEOF(b), ctx);
		if (n <= -1)
		{
			set_rdr_ioerr(hcl, "erroneous or insufficient record type");
			goto oops;
		}

		if (b == HCL_XCHG_END) break;

		switch (b)
		{
			case HCL_XCHG_BC:
			{
				hcl_oow_t nbytes;

				/* this must appear only once but never mind about multiple occurrences */
				n = rdr(hcl, &w, HCL_SIZEOF(w), ctx);
				if (n <= -1)
				{
					set_rdr_ioerr(hcl, "erroneous or insufficient record length");
					goto oops;
				}
				nbytes = hcl_leoowtoh(w);

				if (nbytes > code->bc.capa)
				{
					/* grow the buffer */
					hcl_oow_t newcapa;
					hcl_oob_t* newptr;

					newcapa = nbytes;
					if (HCL_UNLIKELY(newcapa <= 0)) newcapa++;
					newcapa = HCL_ALIGN(newcapa, HCL_BC_BUFFER_ALIGN);
					newptr = hcl_reallocmem(hcl, code->bc.ptr, newcapa);
					if (!newptr) goto oops;

					code->bc.ptr = newptr;
					code->bc.capa = newcapa;
				}

				n = rdr(hcl, code->bc.ptr, nbytes, ctx);
				if (n <= -1) goto oops;

				code->bc.len = nbytes;
				break;
			}

			case HCL_XCHG_STRING_U:
			case HCL_XCHG_SYMBOL_U:
			{
				hcl_bch_t bcsbuf[64];
				hcl_oow_t bcslen, bcsres, ucslen, ucspos;
				hcl_oow_t nbytes, nchars;
				hcl_oop_t ns;

				n = rdr(hcl, &w, HCL_SIZEOF(w), ctx);
				if (n <= -1)
				{
					set_rdr_ioerr(hcl, "erroneous or insufficient record length");
					goto oops;
				}
				nchars = hcl_leoowtoh(w);

				n = rdr(hcl, &w, HCL_SIZEOF(w), ctx);
				if (n <= -1)
				{
					set_rdr_ioerr(hcl, "erroneous or insufficient record length");
					goto oops;
				}
				nbytes = hcl_leoowtoh(w);

				ns = hcl_makestring(hcl, HCL_NULL, nchars, 0); 
				if (HCL_UNLIKELY(!ns)) goto oops;

				ucspos = 0;
				bcsres = 0;
				while (nbytes > 0)
				{
					bcslen = nbytes <= HCL_SIZEOF(bcsbuf)? nbytes : HCL_SIZEOF(bcsbuf);
					n = rdr(hcl, &bcsbuf[bcsres], bcslen - bcsres, ctx);
					if (n <= -1)
					{
						set_rdr_ioerr(hcl, "erroneous or insufficient record data");
						goto oops;
					}

					HCL_ASSERT(hcl, ucspos < nchars);
					bcsres = bcslen;
					ucslen = nchars - ucspos;
					if (hcl_convbtouchars(hcl, bcsbuf, &bcslen, HCL_OBJ_GET_CHAR_PTR(ns, ucspos), &ucslen) <= -1 && bcslen <= 0)
					{
						goto oops;
					}

					ucspos += ucslen;
					nbytes -= bcslen;
					bcsres -= bcslen;
					if (bcsres > 0) HCL_MEMMOVE(bcsbuf, &bcsbuf[bcslen], bcsres);
				}

				HCL_ASSERT(hcl, ucspos == nchars);

				if (b == HCL_XCHG_SYMBOL_U)
				{
					/* form a cons cell */
					hcl_oop_t nc;
					hcl_pushvolat(hcl, &ns);
					nc = hcl_makecons(hcl, ns, hcl->_nil);
					hcl_popvolat(hcl);
					ns = nc;
				}

				if (hcl_addliteraltocode(hcl, code, ns, 0, HCL_NULL) <= -1) goto oops;
				break;
			}

			case HCL_XCHG_STRING_B:
			case HCL_XCHG_SYMBOL_B:
				/* TODO */
				break;

			case HCL_XCHG_SMOOI:
			{
				hcl_oop_t ns;
				if (rdr(hcl, &w, HCL_SIZEOF(w), ctx) <= -1) goto oops;
				w = hcl_leoowtoh(w);
				ns = HCL_SMOOI_TO_OOP((hcl_ooi_t)w);
				if (hcl_addliteraltocode(hcl, code, ns, 0, HCL_NULL) <= -1) goto oops;
				break;
			}

			case HCL_XCHG_PBIGINT:
			case HCL_XCHG_NBIGINT:
			{
				hcl_oow_t nbytes, nwords, j;
				hcl_liw_t liw;
				hcl_oop_t ns;

				n = rdr(hcl, &w, HCL_SIZEOF(w), ctx);
				if (n <= -1)
				{
					set_rdr_ioerr(hcl, "erroneous or insufficient bigint length");
					goto oops;
				}
				nbytes = hcl_leoowtoh(w);

				if (nbytes % HCL_SIZEOF(hcl_liw_t)) goto oops; /* not the right number of bytes */
				nwords = nbytes / HCL_SIZEOF(hcl_liw_t);

				ns = hcl_makebigint(hcl, ((b == HCL_XCHG_PBIGINT)? HCL_BRAND_PBIGINT: HCL_BRAND_NBIGINT), HCL_NULL, nwords);
				if (HCL_UNLIKELY(!ns)) goto oops;

				for (j = 0; j < nwords; j ++)
				{
					if (rdr(hcl, &liw, HCL_SIZEOF(liw), ctx) <= -1) goto oops;
					liw = hcl_leliwtoh(liw);
					HCL_OBJ_SET_LIWORD_VAL(ns, j, liw);
				}

				if (hcl_addliteraltocode(hcl, code, ns, 0, HCL_NULL) <= -1) goto oops;
				break;
			}

			case HCL_XCHG_FPDEC_1:
			case HCL_XCHG_FPDEC_2:
			case HCL_XCHG_FPDEC_3:
			{
				hcl_ooi_t scale;
				hcl_oop_t ns;

				/* read scale */
				n = rdr(hcl, &w, HCL_SIZEOF(w), ctx);
				if (n <= -1)
				{
					set_rdr_ioerr(hcl, "erroneous or insufficient record length");
					goto oops;
				}
				scale = (hcl_ooi_t)hcl_leoowtoh(w);

				if (b == HCL_XCHG_FPDEC_1)
				{
					hcl_ooi_t value;
					n = rdr(hcl, &w, HCL_SIZEOF(w), ctx);
					if (n <= -1)
					{
						set_rdr_ioerr(hcl, "erroneous or insufficient record length");
						goto oops;
					}
					value = (hcl_ooi_t)hcl_leoowtoh(w);
					ns = hcl_makefpdec(hcl, HCL_SMOOI_TO_OOP(value), scale);
					if (HCL_UNLIKELY(!ns)) goto oops;
				}
				else
				{
					hcl_oow_t j, nbytes, nwords;
					hcl_liw_t liw;
					hcl_oop_t v;

					n = rdr(hcl, &w, HCL_SIZEOF(w), ctx);
					if (n <= -1)
					{
						set_rdr_ioerr(hcl, "erroneous or insufficient record length");
						goto oops;
					}
					nbytes = hcl_leoowtoh(w);

					if (nbytes % HCL_SIZEOF(hcl_liw_t)) goto oops; /* not the right number of bytes */
					nwords = nbytes / HCL_SIZEOF(hcl_liw_t);

					v = hcl_makebigint(hcl, ((b == HCL_XCHG_FPDEC_2) ? HCL_BRAND_PBIGINT : HCL_BRAND_NBIGINT), HCL_NULL, nwords);
					if (HCL_UNLIKELY(!v)) goto oops;

					for (j = 0; j < nwords; j++)
					{
						if (rdr(hcl, &liw, HCL_SIZEOF(liw), ctx) <= -1) goto oops;
						liw = hcl_leliwtoh(liw);
						HCL_OBJ_SET_LIWORD_VAL(v, j, liw);
					}
					hcl_pushvolat (hcl, &v);
					ns = hcl_makefpdec(hcl, v, scale);
					hcl_popvolat (hcl);
					if (HCL_UNLIKELY(!ns)) goto oops;
				}
				if (hcl_addliteraltocode(hcl, code, ns, 0, HCL_NULL) <= -1) goto oops;
				break;
			}

			case HCL_XCHG_PRIM:
				/* TODO: */
				break;
		}
	}

	return 0;

oops:
	return -1;
}
/* -------------------------------------------------------------------- */

static int mem_code_writer (hcl_t* hcl, const void* ptr, hcl_oow_t len, void* ctx)
{
	hcl_ptlc_t* dst = (hcl_ptlc_t*)ctx;
	const hcl_uint8_t* p = (const hcl_uint8_t*)ptr;
	const hcl_uint8_t* e = p + len;

	if (dst->capa - dst->len < len)
	{
		hcl_oow_t newcapa;
		hcl_uint8_t* newptr;

		newcapa = dst->len + len;
		newcapa = HCL_ALIGN_POW2(newcapa, 64);
		newptr = hcl_reallocmem(hcl, dst->ptr, newcapa);
		if (HCL_UNLIKELY(!newptr)) return -1;

		dst->ptr = newptr;
		dst->capa = newcapa;
	}

	while (p < e) ((hcl_uint8_t*)dst->ptr)[dst->len++] = *p++;
	return 0;
}

int hcl_marshalcodetomem (hcl_t* hcl, const hcl_code_t* code, hcl_ptlc_t* dst)
{
	return hcl_marshalcode(hcl, code, mem_code_writer, dst);
}

/* -------------------------------------------------------------------- */

struct cmr_t
{
	const hcl_ptl_t* src;
	hcl_oow_t pos;
};

static int mem_code_reader(hcl_t* hcl, void* ptr, hcl_oow_t len, void* ctx)
{
	struct cmr_t* cmr = (struct cmr_t*)ctx;
	hcl_uint8_t* p = (hcl_uint8_t*)ptr;
	hcl_uint8_t* e = p + len;

	HCL_ASSERT (hcl, cmr->pos <= cmr->src->len);

	if (cmr->src->len - cmr->pos < len)
	{
		hcl_seterrbfmt (hcl, HCL_ENOENT, "no more data");
		return -1;
	}

	while (p < e) *p++ = ((const hcl_uint8_t*)cmr->src->ptr)[cmr->pos++];
	return 0;
}

int hcl_unmarshalcodefrommem (hcl_t* hcl, hcl_code_t* code, const hcl_ptl_t* src)
{
	struct cmr_t cmr;
	cmr.src = src;
	cmr.pos = 0;
	return hcl_unmarshalcode(hcl, code, mem_code_reader, &cmr);
}

/* -------------------------------------------------------------------- */

int hcl_brewcode (hcl_t* hcl, hcl_code_t* code)
{
	/* create space to hold byte code and debug information */

	if (!code->bc.ptr)
	{
		code->bc.ptr = (hcl_oob_t*)hcl_allocmem(hcl, HCL_SIZEOF(*code->bc.ptr) * HCL_BC_BUFFER_INIT); /* TODO: set a proper intial size */
		if (HCL_UNLIKELY(!code->bc.ptr)) return -1;
		HCL_ASSERT (hcl, code->bc.len == 0);
		code->bc.capa = HCL_BC_BUFFER_INIT;
	}

	if (!code->dbgi)
	{
		code->dbgi = (hcl_dbgi_t*)hcl_allocmem(hcl, HCL_SIZEOF(*code->dbgi) * HCL_BC_BUFFER_INIT);
		if (HCL_UNLIKELY(!code->dbgi))
		{
			/* bc.ptr and dbgi go together. so free bc.ptr if dbgi allocation fails */
			hcl_freemem (hcl, code->bc.ptr);
			code->bc.ptr = HCL_NULL;
			code->bc.len = 0;
			code->bc.capa = 0;
			return -1;
		}

		HCL_MEMSET (code->dbgi, 0, HCL_SIZEOF(*code->dbgi) * HCL_BC_BUFFER_INIT);
	}

	/* TODO: move code.lit.arr creation to hcl_init() after swithching to hcl_allocmem? */
        if (!code->lit.arr)
        {
                code->lit.arr = (hcl_oop_oop_t)hcl_makengcarray(hcl, HCL_LIT_BUFFER_INIT); /* TOOD: set a proper initial size */
                if (HCL_UNLIKELY(!code->lit.arr)) return -1;
                HCL_ASSERT (hcl, code->lit.len == 0);
        }

	return 0;
}

void hcl_purgecode (hcl_t* hcl, hcl_code_t* code)
{
	if (code->dbgi)
        {
		hcl_freemem (hcl, code->dbgi);
		code->dbgi = HCL_NULL;
        }

	if (code->bc.ptr)
	{
		hcl_freemem (hcl, code->bc.ptr);
		code->bc.ptr = HCL_NULL;
		code->bc.len = 0;
		code->bc.capa = 0;
	}

	if (code->lit.arr)
	{
		hcl_freengcobj (hcl, (hcl_oop_t)code->lit.arr);
		code->lit.arr = HCL_NULL;
		code->lit.len = 0;
	}

	HCL_MEMSET (&code, 0, HCL_SIZEOF(code));
}

/* -------------------------------------------------------------------- */

int hcl_addliteraltocode (hcl_t* hcl, hcl_code_t* code, hcl_oop_t obj, hcl_oow_t lfbase, hcl_oow_t* index)
{
	hcl_oow_t capa, i;
	hcl_oop_t tmp;

	/* TODO: speed up the following duplicate check loop */
	for (i = lfbase; i < code->lit.len; i++)
	{
		tmp = ((hcl_oop_oop_t)code->lit.arr)->slot[i];

		if (tmp == obj)
		{
			/* this removes redundancy of symbols, characters, and integers. */
			if (index) *index = i - lfbase;
			return 0;
		}
		else if (HCL_IS_STRING(hcl, obj) && HCL_IS_STRING(hcl, tmp) && hcl_equalobjs(hcl, obj, tmp))
		{
			/* a string object requires equality check. however, the string created to the literal frame
			 * must be made immutable. non-immutable string literals are source of various problems */
			if (index) *index = i - lfbase;
			return 0;
		}
	}

	capa = HCL_OBJ_GET_SIZE(code->lit.arr);
	if (code->lit.len >= capa)
	{
		hcl_oop_t tmp;
		hcl_oow_t newcapa;

		newcapa = HCL_ALIGN(capa + 1, HCL_LIT_BUFFER_ALIGN);
		tmp = hcl_remakengcarray(hcl, (hcl_oop_t)code->lit.arr, newcapa);
		if (HCL_UNLIKELY(!tmp)) return -1;

		code->lit.arr = (hcl_oop_oop_t)tmp;
	}

	if (index) *index = code->lit.len - lfbase;

	((hcl_oop_oop_t)code->lit.arr)->slot[code->lit.len++] = obj;
	/* TODO: RDONLY? */
	/*if (HCL_IS_OOP_POINTER(obj)) HCL_OBJ_SET_FLAGS_RDONLY(obj, 1); */
	return 0;
}
