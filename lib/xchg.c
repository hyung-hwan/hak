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

#include "hak-prv.h"

/* compiler's literal representation */

#include <hak-pac1.h>
struct hak_xchg_hdr_t
{
	hak_uint8_t ver;
	hak_uint8_t oow_size;
};
typedef struct hak_xchg_hdr_t hak_xchg_hdr_t;
#include <hak-upac.h>

enum hak_xchg_type_t
{
	/* byte code */
	HAK_XCHG_BC = 0x00,

	/* literals */
	HAK_XCHG_STRING_U,
	HAK_XCHG_STRING_B,
	HAK_XCHG_SYMLIT_U, /* literal symbol */
	HAK_XCHG_SYMLIT_B, /* literal symbol */
	HAK_XCHG_SYMBOL_U, /* contained in a cons cell */
	HAK_XCHG_SYMBOL_B, /* contained in a cons cell */
	HAK_XCHG_SMOOI,
	HAK_XCHG_PBIGINT,
	HAK_XCHG_NBIGINT,
	HAK_XCHG_FPDEC_1, /* smooi + smooi */
	HAK_XCHG_FPDEC_2, /* pbigint + smooi */
	HAK_XCHG_FPDEC_3, /* nbigint + smooi */
	HAK_XCHG_PRIM,

	/* end marker */
	HAK_XCHG_END =0xFF /* end marker. not a real literal type */
};
typedef enum hak_xchg_type_t hak_xchg_type_t;

/* -------------------------------------------------------------------- */

int hak_marshalcode (hak_t* hak, const hak_code_t* code, hak_xchg_writer_t wrtr, void* ctx)
{
	hak_oow_t i, lfbase = 0;
	hak_oop_t tmp;
	hak_oop_class_t _class;
	int brand;
	hak_oow_t tsize;
	hak_uint8_t b;
	hak_oow_t w;
	hak_xchg_hdr_t h;

	lfbase = (hak->option.trait & HAK_TRAIT_INTERACTIVE)? hak->c->funblk.info[hak->c->funblk.depth].lfbase: 0;

	/* start with a header */
	h.ver = 1;
	h.oow_size = (hak_uint8_t)HAK_SIZEOF(hak_oow_t); /* the size must not exceed 256 */
	if (wrtr(hak, &h, HAK_SIZEOF(h), ctx) <= -1) goto oops;

	/* write the byte-code */
	b = HAK_XCHG_BC;
	if (wrtr(hak, &b, HAK_SIZEOF(b), ctx) <= -1) goto oops;
	w = hak_htoleoow(code->bc.len);
	if (wrtr(hak, &w, HAK_SIZEOF(w), ctx) <= -1) goto oops;
	if (wrtr(hak, code->bc.ptr, code->bc.len, ctx) <= -1) goto oops;

	/* write actual literals */
	for (i = lfbase; i < code->lit.len; i++)
	{
		tmp = ((hak_oop_oop_t)code->lit.arr)->slot[i];
		if (HAK_OOP_IS_SMOOI(tmp))
		{
			b = HAK_XCHG_SMOOI;
			if (wrtr(hak, &b, HAK_SIZEOF(b), ctx) <= -1) goto oops;
			w = hak_htoleoow((hak_oow_t)HAK_OOP_TO_SMOOI(tmp));
			if (wrtr(hak, &w, HAK_SIZEOF(w), ctx) <= -1) goto oops;
			continue;
		}

		_class = (hak_oop_class_t)HAK_CLASSOF(hak, tmp);
		brand = HAK_OOP_TO_SMOOI(_class->ibrand);
		tsize = HAK_OBJ_GET_SIZE(tmp);

		switch (brand)
		{
			case HAK_BRAND_PBIGINT:
			case HAK_BRAND_NBIGINT:
			{
				hak_oow_t nbytes;
				hak_oow_t j;
				hak_liw_t liw;

				/* write the brand */
				b = (brand == HAK_BRAND_PBIGINT ? HAK_XCHG_PBIGINT : HAK_XCHG_NBIGINT);
				if (wrtr(hak, &b, HAK_SIZEOF(b), ctx) <= -1) goto oops;

			bigint_body:
				/* write the number of bytes in the little-endian */
				nbytes = tsize * HAK_SIZEOF(hak_liw_t);
				w = hak_htoleoow(nbytes);
				if (wrtr(hak, &w, HAK_SIZEOF(w), ctx) <= -1) goto oops;

				for (j = 0; j < tsize; j++)
				{
					liw = HAK_OBJ_GET_LIWORD_VAL(tmp, j);
					liw = hak_htoleliw(liw);
					if (wrtr(hak, &liw, HAK_SIZEOF(liw), ctx) <= -1) goto oops;
				}
				break;
			}

			case HAK_BRAND_FPDEC:
			{
				hak_oop_fpdec_t f;

				f = (hak_oop_fpdec_t)tmp;
				HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(f->scale));
				HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(f->value) || HAK_OOP_IS_POINTER(f->value));

				/* write 1-byte brand */
				if (HAK_OOP_IS_SMOOI(f->value)) b = HAK_XCHG_FPDEC_1;
				else if (HAK_IS_PBIGINT(hak, f->value)) b = HAK_XCHG_FPDEC_2;
				else
				{
					HAK_ASSERT(hak, HAK_IS_NBIGINT(hak, f->value));
					b = HAK_XCHG_FPDEC_2;
				}
				if (wrtr(hak, &b, HAK_SIZEOF(b), ctx) <= -1) goto oops;

				/* cast the scale part from hak_ooi_t to hak_oow_t and write it */
				w = hak_htoleoow((hak_oow_t)HAK_OOP_TO_SMOOI(f->scale));
				if (wrtr(hak, &w, HAK_SIZEOF(w), ctx) <= -1) goto oops;

				/* write the value part */
				if (b == HAK_XCHG_FPDEC_1)
				{
					w = hak_htoleoow((hak_oow_t)HAK_OOP_TO_SMOOI(f->value));
					if (wrtr(hak, &w, HAK_SIZEOF(w), ctx) <= -1) goto oops;
				}
				else
				{
					tmp = f->value;
					tsize = HAK_OBJ_GET_SIZE(tmp);
					goto bigint_body;
				}
				break;
			}

			case HAK_BRAND_CONS:
			{
				/* write 1-byte brand */
			#if defined(HAK_OOCH_IS_UCH)
				b = (hak_uint8_t)HAK_XCHG_SYMBOL_U;
			#else
				b = (hak_uint8_t)HAK_XCHG_SYMBOL_B;
			#endif
				if (wrtr(hak, &b, HAK_SIZEOF(b), ctx) <= -1) goto oops;

				/* get the symbol at CAR and make it as if it is the current object processed.*/
				tmp = HAK_CONS_CAR(tmp);
				tsize = HAK_OBJ_GET_SIZE(tmp);

				HAK_ASSERT(hak, HAK_CLASSOF(hak, tmp) == (hak_oop_t)hak->c_symbol);
				goto string_body;
			}

			case HAK_BRAND_STRING:
			case HAK_BRAND_SYMBOL:
			{
			#if defined(HAK_OOCH_IS_UCH)
				hak_uch_t* ucsptr;
				hak_oow_t ucspos, ucslen;
				hak_bch_t bcsbuf[128];
				hak_oow_t bcslen;
				int n;

				/* write 1-byte brand */
				b = (hak_uint8_t)(brand == HAK_BRAND_STRING? HAK_XCHG_STRING_U: HAK_XCHG_SYMLIT_U);
				if (wrtr(hak, &b, HAK_SIZEOF(b), ctx) <= -1) goto oops;

			string_body:
				ucsptr = HAK_OBJ_GET_CHAR_SLOT(tmp);
				ucslen = tsize;
				if (hak_convutobchars(hak, ucsptr, &ucslen, HAK_NULL, &bcslen) <= -1) goto oops;

				/* write the number of characters in the little endian */
				w = hak_htoleoow(tsize);
				if (wrtr(hak, &w, HAK_SIZEOF(w), ctx) <= -1) goto oops;

				/* write the number of bytes in the little endian */
				w = hak_htoleoow(bcslen);
				if (wrtr(hak, &w, HAK_SIZEOF(w), ctx) <= -1) goto oops;

				/* write string in bytess and write to the callback.*/
				ucspos = 0;
				while (ucspos < tsize)
				{
					bcslen = HAK_COUNTOF(bcsbuf);
					ucslen = tsize - ucspos;
					n = hak_convutobchars(hak, &ucsptr[ucspos], &ucslen, bcsbuf, &bcslen);
					if (n <= -1 && bcslen == 0) goto oops;
					if (wrtr(hak, bcsbuf, bcslen, ctx) <= -1) goto oops;
					ucspos += ucslen;
				}
			#else
				/* write 1-byte brand */
				if (wrtr(hak, &b, HAK_SIZEOF(b), ctx) <= -1) goto oops;

			string_body:
				w = hak_htoleoow(tsize);
				if (wrtr(hak, &w, HAK_SIZEOF(w), ctx) <= -1) goto oops;

				if (wrtr(hak, HAK_OBJ_GET_CHAR_SLOT(tmp), tsize, ctx) <= -1) goto oops;
			#endif
				break;
			}

			case HAK_BRAND_PRIM:
				/* TODO: can't have resolved pointer... need module name and the functio name??? */
				break;
		}
	}

	b = HAK_XCHG_END;
	if (wrtr(hak, &b, HAK_SIZEOF(b), ctx) <= -1) goto oops;
	return 0;

oops:
	return -1;
}

/* -------------------------------------------------------------------- */

static void set_rdr_ioerr (hak_t* hak, const hak_bch_t* msg)
{
	const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
	hak_seterrbfmt(hak, HAK_EIOERR, "%hs - %js", orgmsg);
}

int hak_unmarshalcode (hak_t* hak, hak_code_t* code, hak_xchg_reader_t rdr, void* ctx)
{
	int n;
	hak_xchg_hdr_t h;
	hak_uint8_t b;
	hak_oow_t w;

	hak_uch_t* usym_buf = HAK_NULL;
	hak_oow_t usym_buf_capa = 0;

	/* [NOTE]
	 *  this function may pollute the code data when it fails because it doesn't
	 *  roll back changed made to the memory pointed to by 'code'. the caller side
	 *  may use two code structs. and switch between them for each call to hak_unmarshalcode()
	 *  to avoid this issue.
	 */

	if (hak_brewcode(hak, code) <= -1) goto oops;

	n = rdr(hak, &h, HAK_SIZEOF(h), ctx);
	if (n <= -1)
	{
		set_rdr_ioerr(hak, "erroneous or insufficient header");
		goto oops;
	}

	if (h.ver != 1)
	{
		hak_seterrbfmt(hak, HAK_EIOERR, "unsupported header version %d", (int)h.ver);
		goto oops;
	}

	if (h.oow_size != HAK_SIZEOF(hak_oow_t))
	{
		/* no support for cross-architecture exchange yet */
		hak_seterrbfmt(hak, HAK_EIOERR, "unsupported word size %d", (int)h.oow_size);
		goto oops;
	}

	while (1)
	{
		/* read 1-byte brand */
		n = rdr(hak, &b, HAK_SIZEOF(b), ctx);
		if (n <= -1)
		{
			set_rdr_ioerr(hak, "erroneous or insufficient record type");
			goto oops;
		}

		if (b == HAK_XCHG_END) break;

		switch (b)
		{
			case HAK_XCHG_BC:
			{
				hak_oow_t nbytes;

				/* this must appear only once but never mind about multiple occurrences */
				n = rdr(hak, &w, HAK_SIZEOF(w), ctx);
				if (n <= -1)
				{
					set_rdr_ioerr(hak, "erroneous or insufficient record length");
					goto oops;
				}
				nbytes = hak_leoowtoh(w);

				if (nbytes > code->bc.capa)
				{
					/* grow the buffer */
					hak_oow_t newcapa;
					hak_oob_t* newptr;

					newcapa = nbytes;
					if (HAK_UNLIKELY(newcapa <= 0)) newcapa++;
					newcapa = HAK_ALIGN(newcapa, HAK_BC_BUFFER_ALIGN);
					newptr = (hak_oob_t*)hak_reallocmem(hak, code->bc.ptr, newcapa);
					if (HAK_UNLIKELY(!newptr)) goto oops;

					code->bc.ptr = newptr;
					code->bc.capa = newcapa;
				}

				n = rdr(hak, code->bc.ptr, nbytes, ctx);
				if (n <= -1) goto oops;

				code->bc.len = nbytes;
				break;
			}

			case HAK_XCHG_STRING_U:
			case HAK_XCHG_SYMLIT_U:
			case HAK_XCHG_SYMBOL_U:
			{
				hak_bch_t bcsbuf[64];
				hak_uch_t* ucsptr;
				hak_oow_t bcslen, bcsres, ucslen, ucspos;
				hak_oow_t nbytes, nchars;
				hak_oop_t ns;

				n = rdr(hak, &w, HAK_SIZEOF(w), ctx);
				if (n <= -1)
				{
					set_rdr_ioerr(hak, "erroneous or insufficient record length");
					goto oops;
				}
				nchars = hak_leoowtoh(w);

				n = rdr(hak, &w, HAK_SIZEOF(w), ctx);
				if (n <= -1)
				{
					set_rdr_ioerr(hak, "erroneous or insufficient record length");
					goto oops;
				}
				nbytes = hak_leoowtoh(w);

				if (nchars > usym_buf_capa)
				{
					usym_buf_capa = nchars * HAK_SIZEOF(usym_buf[0]);
					usym_buf = (hak_uch_t*)hak_allocmem(hak, usym_buf_capa);
					if (HAK_UNLIKELY(!usym_buf)) goto oops;
				}
				ucsptr = usym_buf;

				ucspos = 0;
				bcsres = 0;
				while (nbytes > 0)
				{
					bcslen = nbytes <= HAK_SIZEOF(bcsbuf)? nbytes : HAK_SIZEOF(bcsbuf);
					n = rdr(hak, &bcsbuf[bcsres], bcslen - bcsres, ctx);
					if (n <= -1)
					{
						set_rdr_ioerr(hak, "erroneous or insufficient record data");
						goto oops;
					}

					HAK_ASSERT(hak, ucspos < nchars);
					bcsres = bcslen;
					ucslen = nchars - ucspos;
					if (hak_convbtouchars(hak, bcsbuf, &bcslen, &ucsptr[ucspos], &ucslen) <= -1 && bcslen <= 0)
					{
						goto oops;
					}

					ucspos += ucslen;
					nbytes -= bcslen;
					bcsres -= bcslen;
					if (bcsres > 0) HAK_MEMMOVE(bcsbuf, &bcsbuf[bcslen], bcsres);
				}

				HAK_ASSERT(hak, ucspos == nchars);

				if (b == HAK_XCHG_STRING_U)
				{
					ns = hak_makestringwithuchars(hak, usym_buf, nchars);
					if (HAK_UNLIKELY(!ns)) goto oops;
				}
				else
				{
					/* symlit or symbol */
					ns = hak_makesymbolwithuchars(hak, usym_buf, nchars);
					if (HAK_UNLIKELY(!ns)) goto oops;

					if (b == HAK_XCHG_SYMBOL_U)
					{
						/* form a cons cell */
						hak_oop_t nc;
						hak_pushvolat(hak, &ns);
						nc = hak_makecons(hak, ns, hak->_nil);
						hak_popvolat(hak);
						if (HAK_UNLIKELY(!nc)) goto oops;
						ns = nc;
					}
				}

				if (hak_addliteraltocode(hak, code, ns, 0, HAK_NULL) <= -1) goto oops;
				break;
			}

			case HAK_XCHG_STRING_B:
			case HAK_XCHG_SYMBOL_B:
				/* TODO */
				break;

			case HAK_XCHG_SMOOI:
			{
				hak_oop_t ns;
				if (rdr(hak, &w, HAK_SIZEOF(w), ctx) <= -1) goto oops;
				w = hak_leoowtoh(w);
				ns = HAK_SMOOI_TO_OOP((hak_ooi_t)w);
				if (hak_addliteraltocode(hak, code, ns, 0, HAK_NULL) <= -1) goto oops;
				break;
			}

			case HAK_XCHG_PBIGINT:
			case HAK_XCHG_NBIGINT:
			{
				hak_oow_t nbytes, nwords, j;
				hak_liw_t liw;
				hak_oop_t ns;

				n = rdr(hak, &w, HAK_SIZEOF(w), ctx);
				if (n <= -1)
				{
					set_rdr_ioerr(hak, "erroneous or insufficient bigint length");
					goto oops;
				}
				nbytes = hak_leoowtoh(w);

				if (nbytes % HAK_SIZEOF(hak_liw_t)) goto oops; /* not the right number of bytes */
				nwords = nbytes / HAK_SIZEOF(hak_liw_t);

				ns = hak_instantiate(hak, ((b == HAK_XCHG_PBIGINT)? hak->c_large_positive_integer: hak->c_large_negative_integer), HAK_NULL, nwords);
				if (HAK_UNLIKELY(!ns)) goto oops;

				for (j = 0; j < nwords; j ++)
				{
					if (rdr(hak, &liw, HAK_SIZEOF(liw), ctx) <= -1) goto oops;
					liw = hak_leliwtoh(liw);
					HAK_OBJ_SET_LIWORD_VAL(ns, j, liw);
				}

				if (hak_addliteraltocode(hak, code, ns, 0, HAK_NULL) <= -1) goto oops;
				break;
			}

			case HAK_XCHG_FPDEC_1:
			case HAK_XCHG_FPDEC_2:
			case HAK_XCHG_FPDEC_3:
			{
				hak_ooi_t scale;
				hak_oop_t ns;

				/* read scale */
				n = rdr(hak, &w, HAK_SIZEOF(w), ctx);
				if (n <= -1)
				{
					set_rdr_ioerr(hak, "erroneous or insufficient record length");
					goto oops;
				}
				scale = (hak_ooi_t)hak_leoowtoh(w);

				if (b == HAK_XCHG_FPDEC_1)
				{
					hak_ooi_t value;
					n = rdr(hak, &w, HAK_SIZEOF(w), ctx);
					if (n <= -1)
					{
						set_rdr_ioerr(hak, "erroneous or insufficient record length");
						goto oops;
					}
					value = (hak_ooi_t)hak_leoowtoh(w);
					ns = hak_makefpdec(hak, HAK_SMOOI_TO_OOP(value), scale);
					if (HAK_UNLIKELY(!ns)) goto oops;
				}
				else
				{
					hak_oow_t j, nbytes, nwords;
					hak_liw_t liw;
					hak_oop_t v;

					n = rdr(hak, &w, HAK_SIZEOF(w), ctx);
					if (n <= -1)
					{
						set_rdr_ioerr(hak, "erroneous or insufficient record length");
						goto oops;
					}
					nbytes = hak_leoowtoh(w);

					if (nbytes % HAK_SIZEOF(hak_liw_t)) goto oops; /* not the right number of bytes */
					nwords = nbytes / HAK_SIZEOF(hak_liw_t);

					v = hak_instantiate(hak, ((b == HAK_XCHG_FPDEC_2) ? hak->c_large_positive_integer : hak->c_large_negative_integer), HAK_NULL, nwords);
					if (HAK_UNLIKELY(!v)) goto oops;

					for (j = 0; j < nwords; j++)
					{
						if (rdr(hak, &liw, HAK_SIZEOF(liw), ctx) <= -1) goto oops;
						liw = hak_leliwtoh(liw);
						HAK_OBJ_SET_LIWORD_VAL(v, j, liw);
					}
					hak_pushvolat(hak, &v);
					ns = hak_makefpdec(hak, v, scale);
					hak_popvolat(hak);
					if (HAK_UNLIKELY(!ns)) goto oops;
				}
				if (hak_addliteraltocode(hak, code, ns, 0, HAK_NULL) <= -1) goto oops;
				break;
			}

			case HAK_XCHG_PRIM:
				/* TODO: */
				break;
		}
	}

	return 0;

oops:
	if (usym_buf) hak_freemem(hak, usym_buf);
	return -1;
}
/* -------------------------------------------------------------------- */

static int mem_code_writer (hak_t* hak, const void* ptr, hak_oow_t len, void* ctx)
{
	hak_ptlc_t* dst = (hak_ptlc_t*)ctx;
	const hak_uint8_t* p = (const hak_uint8_t*)ptr;
	const hak_uint8_t* e = p + len;

	if (dst->capa - dst->len < len)
	{
		hak_oow_t newcapa;
		hak_uint8_t* newptr;

		newcapa = dst->len + len;
		newcapa = HAK_ALIGN_POW2(newcapa, 64);
		newptr = (hak_uint8_t*)hak_reallocmem(hak, dst->ptr, newcapa);
		if (HAK_UNLIKELY(!newptr)) return -1;

		dst->ptr = newptr;
		dst->capa = newcapa;
	}

	while (p < e) ((hak_uint8_t*)dst->ptr)[dst->len++] = *p++;
	return 0;
}

int hak_marshalcodetomem (hak_t* hak, const hak_code_t* code, hak_ptlc_t* dst)
{
	return hak_marshalcode(hak, code, mem_code_writer, dst);
}

/* -------------------------------------------------------------------- */

struct cmr_t
{
	const hak_ptl_t* src;
	hak_oow_t pos;
};

static int mem_code_reader(hak_t* hak, void* ptr, hak_oow_t len, void* ctx)
{
	struct cmr_t* cmr = (struct cmr_t*)ctx;
	hak_uint8_t* p = (hak_uint8_t*)ptr;
	hak_uint8_t* e = p + len;

	HAK_ASSERT(hak, cmr->pos <= cmr->src->len);

	if (cmr->src->len - cmr->pos < len)
	{
		hak_seterrbfmt(hak, HAK_ENOENT, "no more data");
		return -1;
	}

	while (p < e) *p++ = ((const hak_uint8_t*)cmr->src->ptr)[cmr->pos++];
	return 0;
}

int hak_unmarshalcodefrommem (hak_t* hak, hak_code_t* code, const hak_ptl_t* src)
{
	struct cmr_t cmr;
	cmr.src = src;
	cmr.pos = 0;
	return hak_unmarshalcode(hak, code, mem_code_reader, &cmr);
}

/* -------------------------------------------------------------------- */

int hak_brewcode (hak_t* hak, hak_code_t* code)
{
	/* create space to hold byte code and debug information */

	if (!code->bc.ptr)
	{
		code->bc.ptr = (hak_oob_t*)hak_allocmem(hak, HAK_SIZEOF(*code->bc.ptr) * HAK_BC_BUFFER_INIT); /* TODO: set a proper intial size */
		if (HAK_UNLIKELY(!code->bc.ptr))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to allocate code buffer - %js", orgmsg);
			return -1;
		}
		HAK_ASSERT(hak, code->bc.len == 0);
		code->bc.capa = HAK_BC_BUFFER_INIT;
	}

	if (!code->dbgi)
	{
		code->dbgi = (hak_dbgi_t*)hak_allocmem(hak, HAK_SIZEOF(*code->dbgi) * HAK_BC_BUFFER_INIT);
		if (HAK_UNLIKELY(!code->dbgi))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to allocate debug info buffer - %js", orgmsg);

			/* bc.ptr and dbgi go together. so free bc.ptr if dbgi allocation fails */
			hak_freemem(hak, code->bc.ptr);
			code->bc.ptr = HAK_NULL;
			code->bc.len = 0;
			code->bc.capa = 0;

			return -1;
		}

		HAK_MEMSET(code->dbgi, 0, HAK_SIZEOF(*code->dbgi) * HAK_BC_BUFFER_INIT);
	}

	/* TODO: move code.lit.arr creation to hak_init() after swithching to hak_allocmem? */
	if (!code->lit.arr)
	{
		code->lit.arr = (hak_oop_oop_t)hak_makengcarray(hak, HAK_LIT_BUFFER_INIT); /* TOOD: set a proper initial size */
		if (HAK_UNLIKELY(!code->lit.arr))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to allocate literal frame - %js", orgmsg);
			return -1;
		}
		HAK_ASSERT(hak, code->lit.len == 0);
	}

	return 0;
}

void hak_purgecode (hak_t* hak, hak_code_t* code)
{
	if (code->dbgi)
	{
		hak_freemem(hak, code->dbgi);
		code->dbgi = HAK_NULL;
	}

	if (code->bc.ptr)
	{
		hak_freemem(hak, code->bc.ptr);
		code->bc.ptr = HAK_NULL;
		code->bc.len = 0;
		code->bc.capa = 0;
	}

	if (code->lit.arr)
	{
		hak_freengcobj(hak, (hak_oop_t)code->lit.arr);
		code->lit.arr = HAK_NULL;
		code->lit.len = 0;
	}

	HAK_MEMSET(&code, 0, HAK_SIZEOF(code));
}

/* -------------------------------------------------------------------- */

int hak_addliteraltocode (hak_t* hak, hak_code_t* code, hak_oop_t obj, hak_oow_t lfbase, hak_oow_t* index)
{
	hak_oow_t capa, i;
	hak_oop_t tmp;

	/* TODO: speed up the following duplicate check loop */
	for (i = lfbase; i < code->lit.len; i++)
	{
		tmp = ((hak_oop_oop_t)code->lit.arr)->slot[i];

		if (tmp == obj || hak_equalobjs(hak, obj, tmp))
		{
			/* this removes redundancy of symbols, characters, and integers.
			 * a string object requires equality check. however,
			 * the string created to the literal frame
			 * must be made immutable. non-immutable string literals are
			 * source of various problems */
			if (index) *index = i - lfbase;
			return 0;
		}
	}

	capa = HAK_OBJ_GET_SIZE(code->lit.arr);
	if (code->lit.len >= capa)
	{
		hak_oop_t tmp;
		hak_oow_t newcapa;

		newcapa = HAK_ALIGN(capa + 1, HAK_LIT_BUFFER_ALIGN);
		tmp = hak_remakengcarray(hak, (hak_oop_t)code->lit.arr, newcapa);
		if (HAK_UNLIKELY(!tmp))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to resize literal frame - %js", orgmsg);
			return -1;
		}

		code->lit.arr = (hak_oop_oop_t)tmp;
	}

	if (index) *index = code->lit.len - lfbase;

	((hak_oop_oop_t)code->lit.arr)->slot[code->lit.len++] = obj;
	/* make read-only an object in the literal table.
	 * some immutable objects(e.g. literal symbol) don't need this part
	 * but we just execute it regardless */
	if (HAK_OOP_IS_POINTER(obj)) HAK_OBJ_SET_FLAGS_RDONLY(obj, 1);
	return 0;
}
