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

struct pf_t
{
	hcl_oow_t minargs;
	hcl_oow_t maxargs;
	hcl_pfimpl_t impl;

	hcl_oow_t namelen;
	hcl_ooch_t name[32];
};
typedef struct pf_t pf_t;

/* ------------------------------------------------------------------------- */

hcl_oop_t hcl_makeprim (hcl_t* hcl, hcl_pfimpl_t primimpl, hcl_oow_t minargs, hcl_oow_t maxargs, hcl_mod_t* mod)
{
#if 0
	hcl_oop_prim_t obj; /* in principle, hcl_oop_word_t with HCL_PRIM_NAMED_INSTVARS elements */

	obj = (hcl_oop_prim_t)hcl_allocwordobj(hcl, HCL_BRAND_PRIM, HCL_NULL, HCL_PRIM_NAMED_INSTVARS);
	if (HCL_LIKELY(obj))
	{
		obj->impl = (hcl_oow_t)primimpl;
		obj->min_nargs = minargs;
		obj->max_nargs = maxargs;
		obj->mod = (hcl_oow_t)mod;
	}

	return (hcl_oop_t)obj;
#else
	hcl_oop_prim_t v; /* in principle, hcl_oop_word_t with HCL_PRIM_NUM_WORDS elements */

	v = (hcl_oop_prim_t)hcl_instantiate(hcl, hcl->c_primitive, HCL_NULL, 0);
	if (HCL_UNLIKELY(!v))
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to make primitive - %js", orgmsg);
	}
	else
	{
		v->impl = (hcl_oow_t)primimpl;
		v->min_nargs = minargs;
		v->max_nargs = maxargs;
		v->mod = (hcl_oow_t)mod;
	}

	return (hcl_oop_t)v;
#endif
}

/* ------------------------------------------------------------------------- */

static void log_char_object (hcl_t* hcl, hcl_bitmask_t mask, hcl_oop_char_t msg)
{
	hcl_ooi_t n;
	hcl_oow_t rem;
	const hcl_ooch_t* ptr;

	HCL_ASSERT (hcl, HCL_OBJ_GET_FLAGS_TYPE(msg) == HCL_OBJ_TYPE_CHAR);

	rem = HCL_OBJ_GET_SIZE(msg);
	ptr = msg->slot;

start_over:
	while (rem > 0)
	{
		if (*ptr == '\0')
		{
			n = hcl_logbfmt (hcl, mask, "%jc", *ptr);
			HCL_ASSERT (hcl, n == 1);
			rem -= n;
			ptr += n;
			goto start_over;
		}

		n = hcl_logbfmt (hcl, mask, "%.*js", rem, ptr);
		if (n <= -1) break;
		if (n == 0)
		{
			/* to skip the unprinted character.
			 * actually, this check is not needed because of '\0' skipping
			 * at the beginning  of the loop */
			n = hcl_logbfmt (hcl, mask, "%jc", *ptr);
			HCL_ASSERT (hcl, n == 1);
		}
		rem -= n;
		ptr += n;
	}
}

static hcl_pfrc_t pf_log (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
/* TODO: accept log level */
	hcl_oop_t msg;
	hcl_bitmask_t mask;
	hcl_ooi_t k;

	/*level = HCL_STACK_GET(hcl, hcl->sp - nargs + 1);
	if (!HCL_OOP_IS_SMOOI(level)) mask = HCL_LOG_APP | HCL_LOG_INFO;
	else mask = HCL_LOG_APP | HCL_OOP_TO_SMOOI(level);*/
	mask = HCL_LOG_APP | HCL_LOG_FATAL; /* TODO: accept logging level .. */

	for (k = 0; k < nargs; k++)
	{
		msg = HCL_STACK_GETARG (hcl, nargs, k);

		if (msg == hcl->_nil || msg == hcl->_true || msg == hcl->_false)
		{
			goto dump_object;
		}
		else if (HCL_OOP_IS_CHAR(msg))
		{
			hcl_logbfmt (hcl, mask, "%jc", HCL_OOP_TO_CHAR(msg));
		}
		else if (HCL_OOP_IS_POINTER(msg))
		{
			if (HCL_OBJ_GET_FLAGS_TYPE(msg) == HCL_OBJ_TYPE_CHAR)
			{
				log_char_object (hcl, mask, (hcl_oop_char_t)msg);
			}
			else if (HCL_OBJ_GET_FLAGS_TYPE(msg) == HCL_OBJ_TYPE_OOP)
			{
				/* visit only 1-level down into an array-like object */
				hcl_oop_t inner;
				hcl_oow_t i;
				int brand;

				brand = HCL_OBJ_GET_FLAGS_BRAND(msg);
				if (brand != HCL_BRAND_ARRAY) goto dump_object;

				for (i = 0; i < HCL_OBJ_GET_SIZE(msg); i++)
				{
					inner = ((hcl_oop_oop_t)msg)->slot[i];

					if (i > 0) hcl_logbfmt (hcl, mask, " ");
					if (HCL_OOP_IS_CHAR(inner))
					{
						hcl_logbfmt (hcl, mask, "%jc", HCL_OOP_TO_CHAR(inner));
					}
					else if (HCL_OOP_IS_POINTER(inner) && HCL_OBJ_GET_FLAGS_TYPE(inner) == HCL_OBJ_TYPE_CHAR)
					{
						log_char_object (hcl, mask, (hcl_oop_char_t)inner);
					}
					else
					{
						hcl_logbfmt (hcl, mask, "%O", inner);
					}
				}
			}
			else goto dump_object;
		}
		else
		{
		dump_object:
			hcl_logbfmt (hcl, mask, "%O", msg);
		}
	}

	HCL_STACK_SETRET (hcl, nargs, hcl->_nil);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_logf (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	if (hcl_logfmtcallstack(hcl, nargs) <= -1)
	{
		HCL_STACK_SETRETTOERRNUM (hcl, nargs);
	}
	else
	{
/* TODO: better return code? */
		HCL_STACK_SETRET (hcl, nargs, hcl->_nil);
	}

	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_printf (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	if (hcl_prfmtcallstack(hcl, nargs) <= -1)
	{
		HCL_STACK_SETRETTOERRNUM (hcl, nargs);
	}
	else
	{
/* TODO: better return code? */
		HCL_STACK_SETRET (hcl, nargs, hcl->_nil);
	}

	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_sprintf (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	if (hcl_strfmtcallstack(hcl, nargs) <= -1)
	{
		HCL_STACK_SETRETTOERRNUM (hcl, nargs);
	}
	else
	{
		hcl_oop_t str;
		str = hcl_makestring(hcl, hcl->sprintf.xbuf.ptr, hcl->sprintf.xbuf.len);
		if (!str) return HCL_PF_FAILURE;

		HCL_STACK_SETRET (hcl, nargs, str);
	}

	return HCL_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */

static hcl_oow_t move_udi_residue_bytes (hcl_io_udiarg_t* curinp)
{
	hcl_oow_t cpl;

	cpl = HCL_COUNTOF(curinp->rsd.buf) - curinp->rsd.len;
	if (cpl > 0)
	{
		hcl_oow_t avail;
		avail = curinp->b.len - curinp->b.pos; /* available in the read buffer */
		if (cpl > avail) cpl = avail;
		HCL_MEMCPY(&curinp->rsd.buf[curinp->rsd.len], &curinp->buf.b[curinp->b.pos], cpl);
		curinp->rsd.len += cpl;
		curinp->b.pos += cpl; /* advance the position because the bytes moved to the residue buffer */
	}
	return curinp->rsd.len;
}

static int get_udi_char (hcl_t* hcl, hcl_ooch_t* ch)
{
	hcl_io_udiarg_t* curinp;
	hcl_ooch_t c;
	hcl_oow_t taken;
	int x;

	curinp = &hcl->io.udi_arg;

#if defined(HCL_OOCH_IS_UCH)
	if (curinp->byte_oriented)
	{
		hcl_cmgr_t* cmgr;
		const hcl_uint8_t* inpptr;
		hcl_oow_t inplen, n;

		cmgr = HCL_CMGR(hcl);

	start_over:
		if (curinp->b.pos >= curinp->b.len)
		{
			x = hcl->io.udi_rdr(hcl, HCL_IO_READ_BYTES, curinp);
			if (x <= -1)
			{
				const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
				hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to read bytes from input stream - %js", orgmsg);
				return -1;
			}

			if (curinp->xlen <= 0)
			{
				/* got EOF from an included stream */
				if (curinp->rsd.len > 0)
				{
					hcl_seterrbfmt (hcl, HCL_EECERR, "incomplete byte sequence in input stream");
					return -1;
				}
				curinp->eof_reached = 1;
				return 0;
			}

			curinp->b.pos = 0;
			curinp->b.len = curinp->xlen;
		}

		if (curinp->rsd.len > 0)
		{
			/* there is data in the residue buffer. use the residue buffer to
			 * locate a proper multi-byte sequence */
			HCL_ASSERT (hcl, curinp->b.pos == 0);
			inplen = move_udi_residue_bytes(curinp);
			inpptr = &curinp->rsd.buf[0];
		}
		else
		{
			inplen = curinp->b.len - curinp->b.pos;
			inpptr = &curinp->buf.b[curinp->b.pos];
		}

		n = cmgr->bctouc((const hcl_bch_t*)inpptr, inplen, &c);
		if (n == 0) /* invalid sequence */
		{
			hcl_seterrbfmt (hcl, HCL_EECERR, "invalid byte sequence in input stream");
			return -1;
		}
		if (n > inplen) /* incomplete sequence */
		{
			HCL_ASSERT (hcl, curinp->rsd.len < HCL_COUNTOF(curinp->rsd.buf));
			move_udi_residue_bytes (curinp);
			goto start_over;
		}

		if (curinp->rsd.len > 0)
		{
			/* move_cci_residue_bytes() advanced curinp->b.pos without checking
			 * the needed number of bytes to form a character. it must backoff by
			 * the number of excessive bytes moved to the residue buffer */
			curinp->b.pos -= curinp->rsd.len - n;
			taken = 0;  /* treat it as if no bytes are taken in this case */
		}
		else
		{
			taken = n;
		}
	}
	else
	{
#endif
		if (curinp->b.pos >= curinp->b.len)
		{
			x = hcl->io.udi_rdr(hcl, HCL_IO_READ, curinp);
			if (x <= -1)
			{
				const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
				hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to read input stream - %js", orgmsg);
				return -1;
			}
			if (curinp->xlen <= 0)
			{
				/* got EOF from an included stream */
				curinp->eof_reached++;
				return 0;
			}

			curinp->b.pos = 0;
			curinp->b.len = curinp->xlen;
		}

		c = curinp->buf.c[curinp->b.pos];
		taken = 1;
#if defined(HCL_OOCH_IS_UCH)
	}
#endif

	curinp->b.pos += taken;
#if defined(HCL_OOCH_IS_UCH)
	curinp->rsd.len = 0; /* clear up the residue byte buffer. needed for byte reading only */
#endif

	*ch = c;
	return 1;
}

static int get_udi_byte (hcl_t* hcl, hcl_uint8_t* bt)
{
	hcl_io_udiarg_t* curinp;
	int x;

#if defined(HCL_OOCH_IS_UCH)
	if (!hcl->io.udi_arg.byte_oriented)
	{
/* TODO: convert characters to bytes? but do we know the original encoding? */
		hcl_seterrbfmt (hcl, HCL_EPERM, "byte-oriented input prohibited on character-oriented stream");
		return -1;
	}
#endif

	curinp = &hcl->io.udi_arg;
	if (curinp->b.pos >= curinp->b.len)
	{
		x = hcl->io.udi_rdr(hcl, HCL_IO_READ_BYTES, curinp);
		if (x <= -1)
		{
			const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
			hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to read input stream - %js", orgmsg);
			return -1;
		}
		if (curinp->xlen <= 0)
		{
			/* got EOF from an included stream */
			curinp->eof_reached++;
			return 0;
		}

		curinp->b.pos = 0;
		curinp->b.len = curinp->xlen;
	}

	*bt = curinp->buf.b[curinp->b.pos++];
	return 1;
}

static hcl_pfrc_t pf_getbyte (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t v;
	hcl_uint8_t bt;
	int n;

	n = get_udi_byte(hcl, &bt);
	if (n <= -1) return HCL_PF_FAILURE;

	/* return nil on EOF, or the actual character read */
	v = (n == 0)? hcl->_nil: HCL_SMOOI_TO_OOP(bt);
	HCL_STACK_SETRET (hcl, nargs, v);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_getch (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t v;
	hcl_ooch_t ch;
	int n;

	n = get_udi_char(hcl, &ch);
	if (n <= -1) return HCL_PF_FAILURE;

	/* return nil on EOF, or the actual character read */
	v = (n == 0)? hcl->_nil: HCL_CHAR_TO_OOP(ch);
	HCL_STACK_SETRET (hcl, nargs, v);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_gets (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_io_udiarg_t* curinp;
	hcl_oop_t v;

	curinp = &hcl->io.udi_arg;
	if (curinp->eof_reached)
	{
		v = hcl->_nil;
	}
	else
	{
		int n;
		hcl_ooch_t ch;
		hcl_ooch_t buf[10];
		hcl_ooch_t* ptr;
		hcl_oow_t len, capa;

		ptr = buf;
		len = 0;
		capa = HCL_COUNTOF(buf);
		while (1)
		{
			n = get_udi_char(hcl, &ch);
			if (n <= -1) return HCL_PF_FAILURE;
			if (n == 0) break;

			if (len >= capa)
			{
				hcl_ooch_t* tmp;
				hcl_oow_t newcapa;

				newcapa = capa + HCL_COUNTOF(buf);
				if (ptr == buf)
				{
					tmp = hcl_allocmem(hcl, HCL_SIZEOF(*ptr) * newcapa);
					if (HCL_UNLIKELY(!tmp)) return HCL_PF_FAILURE;
					HCL_MEMCPY (tmp, buf, HCL_SIZEOF(buf));
				}
				else
				{
					tmp = hcl_reallocmem(hcl, ptr, HCL_SIZEOF(*ptr) * newcapa);
					if (HCL_UNLIKELY(!tmp))
					{
						hcl_freemem(hcl, ptr);
						return HCL_PF_FAILURE;
					}
				}

				ptr = tmp;
				capa = newcapa;
			}
			ptr[len++] = ch;
			if (ch == '\n') break; /* TODO: don't hardcode EOL */
		}

		if (len <= 0)
		{
			HCL_ASSERT (hcl, ptr == buf);
			v = hcl->_nil;
		}
		else
		{
			v = hcl_makestring(hcl, ptr, len);
			if (ptr != buf) hcl_freemem(hcl, ptr);
			if (HCL_UNLIKELY(!v)) return HCL_PF_FAILURE;
		}

	}

	HCL_STACK_SETRET (hcl, nargs, v);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_scanf (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	if (hcl_scfmtcallstack(hcl, nargs) <= -1)
	{
		HCL_STACK_SETRETTOERRNUM (hcl, nargs);
	}
	else
	{
/* TODO: better return code? */
		HCL_STACK_SETRET (hcl, nargs, hcl->_nil);
	}

	return HCL_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */

static hcl_pfrc_t pf_gc (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_gc (hcl, 1);
	HCL_STACK_SETRET (hcl, nargs, hcl->_nil);
	return HCL_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */

static hcl_pfrc_t pf_eqv (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t a0, a1, rv;

	a0 = HCL_STACK_GETARG(hcl, nargs, 0);
	a1 = HCL_STACK_GETARG(hcl, nargs, 1);

	rv = (a0 == a1? hcl->_true: hcl->_false);

	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_eql (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	int n;
	n = hcl_equalobjs(hcl, HCL_STACK_GETARG(hcl, nargs, 0), HCL_STACK_GETARG(hcl, nargs, 1));
	if (n <= -1) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, (n? hcl->_true: hcl->_false));
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_eqk (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	/* equal kind? */
	hcl_oop_t a0, a1, rv;

	a0 = HCL_STACK_GETARG(hcl, nargs, 0);
	a1 = HCL_STACK_GETARG(hcl, nargs, 1);

	rv = (HCL_BRANDOF(hcl, a0) == HCL_BRANDOF(hcl, a1)? hcl->_true: hcl->_false);

	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_nqv (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t a0, a1, rv;

	a0 = HCL_STACK_GETARG(hcl, nargs, 0);
	a1 = HCL_STACK_GETARG(hcl, nargs, 1);

	rv = (a0 != a1? hcl->_true: hcl->_false);

	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_nql (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	int n;
	n = hcl_equalobjs(hcl, HCL_STACK_GETARG(hcl, nargs, 0), HCL_STACK_GETARG(hcl, nargs, 1));
	if (n <= -1) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, (!n? hcl->_true: hcl->_false));
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_nqk (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	/* not equal kind? */
	hcl_oop_t a0, a1, rv;

	a0 = HCL_STACK_GETARG(hcl, nargs, 0);
	a1 = HCL_STACK_GETARG(hcl, nargs, 1);

	rv = (HCL_BRANDOF(hcl, a0) != HCL_BRANDOF(hcl, a1)? hcl->_true: hcl->_false);

	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */

static hcl_pfrc_t pf_is_nil (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t rv;
	rv = (HCL_STACK_GETARG(hcl, nargs, 0) == hcl->_nil)? hcl->_true: hcl->_false;
	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}


static hcl_pfrc_t pf_is_boolean (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t rv, x;
	x = HCL_STACK_GETARG(hcl, nargs, 0);
	rv = (HCL_IS_TRUE(hcl, x) || HCL_IS_FALSE(hcl, x))? hcl->_true: hcl->_false;
	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_is_character (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t rv, x;
	x = HCL_STACK_GETARG(hcl, nargs, 0);
	rv = (HCL_OOP_IS_CHAR(x))? hcl->_true: hcl->_false;
	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_is_error (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t rv, x;
	x = HCL_STACK_GETARG(hcl, nargs, 0);
	rv = (HCL_OOP_IS_ERROR(x))? hcl->_true: hcl->_false;
	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_is_smptr (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t rv, x;
	x = HCL_STACK_GETARG(hcl, nargs, 0);
	rv = (HCL_OOP_IS_SMPTR(x))? hcl->_true: hcl->_false;
	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_is_integer (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t rv, x;
	x = HCL_STACK_GETARG(hcl, nargs, 0);
	rv = (hcl_isint(hcl, x))? hcl->_true: hcl->_false;
	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_is_numeric(hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t rv, x;
	x = HCL_STACK_GETARG(hcl, nargs, 0);
	rv = (hcl_isint(hcl, x) || HCL_IS_FPDEC(hcl, x))? hcl->_true: hcl->_false;
	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_is_string (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t rv, x;
	x = HCL_STACK_GETARG(hcl, nargs, 0);
	rv = (HCL_IS_STRING(hcl, x))? hcl->_true: hcl->_false;
	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_is_array (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t rv, x;
	x = HCL_STACK_GETARG(hcl, nargs, 0);
	rv = (HCL_IS_ARRAY(hcl, x))? hcl->_true: hcl->_false;
	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_is_bytearray (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t rv, x;
	x = HCL_STACK_GETARG(hcl, nargs, 0);
	rv = (HCL_IS_BYTEARRAY(hcl, x))? hcl->_true: hcl->_false;
	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_is_dictionary (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t rv, x;
	x = HCL_STACK_GETARG(hcl, nargs, 0);
	rv = (HCL_IS_DIC(hcl, x))? hcl->_true: hcl->_false;
	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_is_block (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t rv, x;
	x = HCL_STACK_GETARG(hcl, nargs, 0);
	rv = (HCL_IS_BLOCK(hcl, x))? hcl->_true: hcl->_false;
	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_is_class (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t rv, x;
	x = HCL_STACK_GETARG(hcl, nargs, 0);
	rv = (HCL_IS_CLASS(hcl, x))? hcl->_true: hcl->_false;
	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_is_object (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t rv, x;
	x = HCL_STACK_GETARG(hcl, nargs, 0);
	rv = (HCL_IS_INSTANCE(hcl, x))? hcl->_true: hcl->_false;
	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */

static hcl_pfrc_t pf_not (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t arg, rv;

	arg = HCL_STACK_GETARG(hcl, nargs, 0);
	if (arg == hcl->_true) rv = hcl->_false;
	else if (arg == hcl->_false) rv = hcl->_true;
	else
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "boolean parameter expected - %O", arg);
		return HCL_PF_FAILURE;
	}

	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_and (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t arg, rv;
	hcl_ooi_t i;

	rv = hcl->_true;
	for (i = 1; i < nargs; i++)
	{
		arg = HCL_STACK_GETARG(hcl, nargs, i);
		if (arg == hcl->_true)
		{
			/* do nothing */
		}
		else if (arg == hcl->_false)
		{
			rv = hcl->_false;
			break;
		}
		else
		{
			hcl_seterrbfmt (hcl, HCL_EINVAL, "boolean parameter expected - %O", arg);
			return HCL_PF_FAILURE;
		}
	}

	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_or (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t arg, rv;
	hcl_ooi_t i;

	rv = hcl->_false;
	for (i = 1; i < nargs; i++)
	{
			arg = HCL_STACK_GETARG(hcl, nargs, i);
		if (arg == hcl->_true)
		{
			rv = hcl->_true;
			break;
		}
		else if (arg == hcl->_false)
		{
			/* do nothing */
		}
		else
		{
			hcl_seterrbfmt (hcl, HCL_EINVAL, "boolean parameter expected - %O", arg);
			return HCL_PF_FAILURE;
		}
	}

	HCL_STACK_SETRET (hcl, nargs, rv);
	return HCL_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */

static hcl_pfrc_t pf_number_add (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_ooi_t i;
	hcl_oop_t arg, ret;

	ret = HCL_STACK_GETARG(hcl, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HCL_STACK_GETARG(hcl, nargs, i);
		/*ret = hcl_addints(hcl, ret, arg);*/
		ret = hcl_addnums(hcl, ret, arg);
		if (!ret) return HCL_PF_FAILURE;
	}

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_number_sub (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_ooi_t i;
	hcl_oop_t arg, ret;

	ret = HCL_STACK_GETARG(hcl, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HCL_STACK_GETARG(hcl, nargs, i);
		/*ret = hcl_subints(hcl, ret, arg);*/
		ret = hcl_subnums(hcl, ret, arg);
		if (!ret) return HCL_PF_FAILURE;
	}

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_number_mul (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_ooi_t i;
	hcl_oop_t arg, ret;

	ret = HCL_STACK_GETARG(hcl, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HCL_STACK_GETARG(hcl, nargs, i);
		/*ret = hcl_mulints(hcl, ret, arg);*/
		ret = hcl_mulnums(hcl, ret, arg);
		if (!ret) return HCL_PF_FAILURE;
	}

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_number_mlt (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_ooi_t i;
	hcl_oop_t arg, ret;

	ret = HCL_STACK_GETARG(hcl, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HCL_STACK_GETARG(hcl, nargs, i);
		ret = hcl_mltnums(hcl, ret, arg);
		if (!ret) return HCL_PF_FAILURE;
	}

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_number_div (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_ooi_t i;
	hcl_oop_t arg, ret;

	ret = HCL_STACK_GETARG(hcl, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HCL_STACK_GETARG(hcl, nargs, i);
		ret = hcl_divnums(hcl, ret, arg);
		if (!ret) return HCL_PF_FAILURE;
	}

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_integer_quo (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_ooi_t i;
	hcl_oop_t arg, ret;

	ret = HCL_STACK_GETARG(hcl, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HCL_STACK_GETARG(hcl, nargs, i);
		ret = hcl_divints(hcl, ret, arg, 0, HCL_NULL);
		if (!ret) return HCL_PF_FAILURE;
	}

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_integer_rem (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_ooi_t i;
	hcl_oop_t arg, ret, rem;

	ret = HCL_STACK_GETARG(hcl, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HCL_STACK_GETARG(hcl, nargs, i);
		ret = hcl_divints(hcl, ret, arg, 0, &rem);
		if (!ret) return HCL_PF_FAILURE;
		ret = rem;
	}

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_integer_mquo (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_ooi_t i;
	hcl_oop_t arg, ret;

	ret = HCL_STACK_GETARG(hcl, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HCL_STACK_GETARG(hcl, nargs, i);
		ret = hcl_divints(hcl, ret, arg, 1, HCL_NULL);
		if (!ret) return HCL_PF_FAILURE;
	}

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_integer_mod (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_ooi_t i;
	hcl_oop_t arg, ret, rem;

	ret = HCL_STACK_GETARG(hcl, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HCL_STACK_GETARG(hcl, nargs, i);
		ret = hcl_divints(hcl, ret, arg, 1, &rem);
		if (!ret) return HCL_PF_FAILURE;
		ret = rem;
	}

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_number_sqrt (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t ret;
	ret = hcl_sqrtnum(hcl, HCL_STACK_GETARG(hcl, nargs, 0));
	if (!ret) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_number_abs (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t ret;
	ret = hcl_absnum(hcl, HCL_STACK_GETARG(hcl, nargs, 0));
	if (!ret) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_number_gt (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t ret;
	ret = hcl_gtnums(hcl, HCL_STACK_GETARG(hcl, nargs, 0), HCL_STACK_GETARG(hcl, nargs, 1));
	if (!ret) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}


static hcl_pfrc_t pf_number_ge (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t ret;
	ret = hcl_genums(hcl, HCL_STACK_GETARG(hcl, nargs, 0), HCL_STACK_GETARG(hcl, nargs, 1));
	if (!ret) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_number_lt (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t ret;
	ret = hcl_ltnums(hcl, HCL_STACK_GETARG(hcl, nargs, 0), HCL_STACK_GETARG(hcl, nargs, 1));
	if (!ret) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}
static hcl_pfrc_t pf_number_le (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t ret;
	ret = hcl_lenums(hcl, HCL_STACK_GETARG(hcl, nargs, 0), HCL_STACK_GETARG(hcl, nargs, 1));
	if (!ret) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}
static hcl_pfrc_t pf_number_eq (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t ret;
	ret = hcl_eqnums(hcl, HCL_STACK_GETARG(hcl, nargs, 0), HCL_STACK_GETARG(hcl, nargs, 1));
	if (!ret) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}
static hcl_pfrc_t pf_number_ne (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t ret;
	ret = hcl_nenums(hcl, HCL_STACK_GETARG(hcl, nargs, 0), HCL_STACK_GETARG(hcl, nargs, 1));
	if (!ret) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}



static hcl_pfrc_t pf_integer_band (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t ret;
	ret = hcl_bitandints(hcl, HCL_STACK_GETARG(hcl, nargs, 0), HCL_STACK_GETARG(hcl, nargs, 1));
	if (!ret) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_integer_bor (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t ret;
	ret = hcl_bitorints(hcl, HCL_STACK_GETARG(hcl, nargs, 0), HCL_STACK_GETARG(hcl, nargs, 1));
	if (!ret) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}
static hcl_pfrc_t pf_integer_bxor (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t ret;
	ret = hcl_bitxorints(hcl, HCL_STACK_GETARG(hcl, nargs, 0), HCL_STACK_GETARG(hcl, nargs, 1));
	if (!ret) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}
static hcl_pfrc_t pf_integer_bnot (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t ret;
	ret = hcl_bitinvint(hcl, HCL_STACK_GETARG(hcl, nargs, 0));
	if (!ret) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}
static hcl_pfrc_t pf_integer_bshift (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t ret;
	ret = hcl_bitshiftint(hcl, HCL_STACK_GETARG(hcl, nargs, 0), HCL_STACK_GETARG(hcl, nargs, 1));
	if (!ret) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, ret);
	return HCL_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */
static hcl_pfrc_t pf_va_context (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	HCL_STACK_SETRET (hcl, nargs, (hcl_oop_t)hcl->active_context);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_va_count (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_context_t ctx;
	hcl_ooi_t attr_mask, /*va,*/ fixed_nargs, nrvars, nlvars, nvaargs;

	if (nargs >= 1)
	{
		ctx = (hcl_oop_context_t)HCL_STACK_GETARG(hcl, nargs, 0);
		if (!HCL_IS_CONTEXT(hcl, ctx))
		{
			hcl_seterrbfmt (hcl, HCL_EINVAL, "not a proper va context - %O", ctx);
			return HCL_PF_FAILURE;
		}
	}
	else
	{
		ctx = hcl->active_context;
	}

	attr_mask = HCL_OOP_TO_SMOOI(ctx->attr_mask);

	/*va = GET_BLK_MASK_VA(attr_mask);*/
	fixed_nargs = GET_BLK_MASK_NARGS(attr_mask);
	nrvars = GET_BLK_MASK_NRVARS(attr_mask);
	nlvars = GET_BLK_MASK_NLVARS(attr_mask);

	/*if (!va) TODO: need this check?
	{
	}*/

	nvaargs = HCL_OBJ_GET_SIZE(ctx) - fixed_nargs - nrvars - nlvars - HCL_CONTEXT_NAMED_INSTVARS;
	HCL_STACK_SETRET (hcl, nargs, HCL_SMOOI_TO_OOP(nvaargs));
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_va_get (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_context_t ctx;
	hcl_ooi_t attr_mask, /*va,*/ fixed_nargs, nrvars, nlvars, nvaargs;
	hcl_oow_t index;
	hcl_oop_t idx;
	int n;

	if (nargs >= 2)
	{
		ctx = (hcl_oop_context_t)HCL_STACK_GETARG(hcl, nargs, 1);
		if (!HCL_IS_CONTEXT(hcl, ctx))
		{
			hcl_seterrbfmt (hcl, HCL_EINVAL, "not a proper va context - %O", ctx);
			return HCL_PF_FAILURE;
		}
	}
	else
	{
		ctx = hcl->active_context;
	}
	attr_mask = HCL_OOP_TO_SMOOI(ctx->attr_mask);

	/*va = GET_BLK_MASK_VA(attr_mask);*/
	fixed_nargs = GET_BLK_MASK_NARGS(attr_mask);
	nrvars = GET_BLK_MASK_NRVARS(attr_mask);
	nlvars = GET_BLK_MASK_NLVARS(attr_mask);

	idx = HCL_STACK_GETARG(hcl, nargs, 0);
	n = hcl_inttooow(hcl, idx, &index);
	if (n <= 0)
	{
		if (n <= -1) hcl_seterrbfmt (hcl, HCL_EINVAL, "invalid index - %O", idx);
		return HCL_PF_FAILURE;
	}

	/*
	if (!va) TODO: need this check?
	{
	}*/

	nvaargs = HCL_OBJ_GET_SIZE(ctx) - fixed_nargs - nrvars - nlvars - HCL_CONTEXT_NAMED_INSTVARS;
	if (index >= nvaargs)
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "va index(%zu) out of bounds for va of size %zd", index, nvaargs);
		return HCL_PF_FAILURE;
	}

	HCL_STACK_SETRET (hcl, nargs, ctx->slot[fixed_nargs + nrvars + nlvars + index]);
	return HCL_PF_SUCCESS;
}


static hcl_pfrc_t pf_object_new (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t obj;
	hcl_oop_t _class;
	hcl_oow_t size = 0;

	_class = HCL_STACK_GETARG(hcl, nargs, 0);
	if (!HCL_IS_CLASS(hcl, _class))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "not a class - %O", _class);
		return HCL_PF_FAILURE;
	}

	if (nargs >= 1)
	{
		int n;
		hcl_oop_t sz;

		sz = HCL_STACK_GETARG(hcl, nargs, 1);
		n = hcl_inttooow(hcl, sz, &size);
		if (n == 0) return HCL_PF_FAILURE;
		if (n <= -1)
		{
			hcl_seterrbfmt (hcl, HCL_EINVAL, "invalid size - %O", sz);
			return HCL_PF_FAILURE;
		}
	}

	obj = hcl_instantiate(hcl, (hcl_oop_class_t)_class, HCL_NULL, size);
	if (HCL_UNLIKELY(!obj)) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, obj);
	return HCL_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */

static pf_t builtin_prims[] =
{
	/* TODO: move these primitives to modules... */

	{ 0, 0,                       pf_getbyte,         7,  { 'g','e','t','b','y','t','e' } },
	{ 0, 0,                       pf_getch,           5,  { 'g','e','t','c','h' } },
	{ 0, 0,                       pf_gets,            4,  { 'g','e','t','s' } },
	{ 0, HCL_TYPE_MAX(hcl_oow_t), pf_log,             3,  { 'l','o','g' } },
	{ 1, HCL_TYPE_MAX(hcl_oow_t), pf_logf,            4,  { 'l','o','g','f' } },
	{ 1, HCL_TYPE_MAX(hcl_oow_t), pf_printf,          6,  { 'p','r','i','n','t','f' } },
	{ 1, HCL_TYPE_MAX(hcl_oow_t), pf_scanf,           5,  { 's','c','a','n','f' } },
	{ 1, HCL_TYPE_MAX(hcl_oow_t), pf_sprintf,         7,  { 's','p','r','i','n','t','f' } },

	{ 0, 0,                       pf_gc,              2,  { 'g','c' } },

	{ 1, 1,                       pf_not,             3,  { 'n','o','t' } },
	/* this is a long-circuit logical and the short-curcuit 'and' is treated as a special form */
	{ 2, HCL_TYPE_MAX(hcl_oow_t), pf_and,             4,  { '_','a','n','d' } },
	/* this is a long-cirtuit logical or. the short-circuit 'or' is treated as a special form */
	{ 2, HCL_TYPE_MAX(hcl_oow_t), pf_or,              3,  { '_','o','r' } },

	{ 2, 2,                       pf_eqv,             4,  { 'e','q','v','?' } },
	{ 2, 2,                       pf_eql,             4,  { 'e','q','l','?' } },
	{ 2, 2,                       pf_eqk,             4,  { 'e','q','k','?' } },
	{ 2, 2,                       pf_nqv,             4,  { 'n','q','v','?' } },
	{ 2, 2,                       pf_nql,             4,  { 'n','q','l','?' } },
	{ 2, 2,                       pf_nqk,             4,  { 'n','q','k','?' } },

	{ 1, 1,                       pf_is_nil,          4,  { 'n','i','l','?' } },
	{ 1, 1,                       pf_is_boolean,      8,  { 'b','o','o','l','e','a','n','?' } },
	{ 1, 1,                       pf_is_character,   10,  { 'c','h','a','r','a','c','t','e','r','?' } },
	{ 1, 1,                       pf_is_error,        6,  { 'e','r','r','o','r','?' } },
	{ 1, 1,                       pf_is_smptr,        6,  { 's','m','p','t','r','?' } },
	{ 1, 1,                       pf_is_integer,      8,  { 'i','n','t','e','g','e','r','?' } },
	{ 1, 1,                       pf_is_numeric,      8,  { 'n','u','m','e','r','i','c','?' } },
	{ 1, 1,                       pf_is_string,       7,  { 's','t','r','i','n','g','?' } },
	{ 1, 1,                       pf_is_array,        6,  { 'a','r','r','a','y','?' } },
	{ 1, 1,                       pf_is_bytearray,   10,  { 'b','y','t','e','a','r','r','a','y','?' } },
	{ 1, 1,                       pf_is_dictionary,  11,  { 'd','i','c','t','i','o','n','a','r','y','?' } },
	{ 1, 1,                       pf_is_block,        7,  { 'l','a','m','b','d','a','?' } },
	{ 1, 1,                       pf_is_block,        4,  { 'f','u','n','?' } },
	{ 1, 1,                       pf_is_class,        6,  { 'c','l','a','s','s','?' } },
	{ 1, 1,                       pf_is_object,       7,  { 'o','b','j','e','c','t','?' } },


	{ 1, HCL_TYPE_MAX(hcl_oow_t), pf_number_add,      1,  { '+' } },
	{ 1, HCL_TYPE_MAX(hcl_oow_t), pf_number_sub,      1,  { '-' } },
	{ 1, HCL_TYPE_MAX(hcl_oow_t), pf_number_mul,      1,  { '*' } },
	{ 1, HCL_TYPE_MAX(hcl_oow_t), pf_number_mlt,      3,  { 'm','l','t' } },
	{ 1, HCL_TYPE_MAX(hcl_oow_t), pf_number_div,      1,  { '/' } },
	{ 1, 1,                       pf_number_sqrt,     4,  { 's','q','r','t' } },
	{ 1, 1,                       pf_number_abs,      3,  { 'a','b','s' } },

	{ 2, 2,                       pf_number_gt,       1,  { '>' } },
	{ 2, 2,                       pf_number_ge,       2,  { '>','=' } },
	{ 2, 2,                       pf_number_lt,       1,  { '<' } },
	{ 2, 2,                       pf_number_le,       2,  { '<','=' } },
	{ 2, 2,                       pf_number_eq,       1,  { '=' } },
	{ 2, 2,                       pf_number_eq,       2,  { '=', '=' } },
	{ 2, 2,                       pf_number_ne,       2,  { '~','=' } },

	/* bitwise operations are supported for integers only */
	{ 2, 2,                       pf_integer_band,    7,  { 'b','i','t','-','a','n','d' } },
	{ 2, 2,                       pf_integer_bor,     6,  { 'b','i','t','-','o','r' } },
	{ 2, 2,                       pf_integer_bxor,    7,  { 'b','i','t','-','x','o','r' } },
	{ 1, 1,                       pf_integer_bnot,    7,  { 'b','i','t','-','n','o','t' } },
	{ 2, 2,                       pf_integer_bshift,  9,  { 'b','i','t','-','s','h','i','f','t'  } },

	{ 1, HCL_TYPE_MAX(hcl_oow_t), pf_integer_quo,     3,  { 'd','i','v' } },
	{ 2, HCL_TYPE_MAX(hcl_oow_t), pf_integer_rem,     3,  { 'r','e','m' } },
	{ 1, HCL_TYPE_MAX(hcl_oow_t), pf_integer_mquo,    4,  { 'm','d','i','v' } },
	{ 2, HCL_TYPE_MAX(hcl_oow_t), pf_integer_mod,     3,  { 'm','o','d' } },

	{ 0, 0,                       pf_va_context,      10, { 'v','a','-','c','o','n','t','e','x','t' } },
	{ 0, 1,                       pf_va_count,        8,  { 'v','a','-','c','o','u','n','t' } },
	{ 1, 2,                       pf_va_get,          6,  { 'v','a','-','g','e','t' } },

	{ 1, 2,                       pf_object_new,                           10, { 'o','b','j','e','c','t','-','n','e','w' } },

	{ 0, 0,                       hcl_pf_process_current,                  15, { 'c','u','r','r','e','n','t','-','p','r','o','c','e','s','s'} },
	{ 1, HCL_TYPE_MAX(hcl_oow_t), hcl_pf_process_fork,                      4, { 'f','o','r','k'} },
	{ 1, 1,                       hcl_pf_process_resume,                    6, { 'r','e','s','u','m','e' } },
	{ 0, 1,                       hcl_pf_process_suspend,                   7, { 's','u','s','p','e','n','d' } },
	{ 0, 1,                       hcl_pf_process_terminate,                 9, { 't','e','r','m','i','n','a','t','e' } },
	{ 0, 0,                       hcl_pf_process_terminate_all,            13, { 't','e','r','m','i','n','a','t','e','-','a','l','l' } },
	{ 0, 0,                       hcl_pf_process_yield,                     5, { 'y','i','e','l','d'} },


	{ 0, 0,                       hcl_pf_semaphore_new,                     7, { 's','e','m','-','n','e','w'} },
	{ 1, 1,                       hcl_pf_semaphore_wait,                    8, { 's','e','m','-','w','a','i','t'} },
	{ 1, 3,                       hcl_pf_semaphore_signal,                 10, { 's','e','m','-','s','i','g','n','a','l'} },
	{ 2, 2,                       hcl_pf_semaphore_signal_on_input,        19, { 's','e','m','-','s','i','g','n','a','l','-','o','n','-','i','n','p','u','t'} },
	{ 2, 2,                       hcl_pf_semaphore_signal_on_output,       20, { 's','e','m','-','s','i','g','n','a','l','-','o','n','-','o','u','t','p','u','t'} },
	{ 1, 1,                       hcl_pf_semaphore_unsignal,               12, { 's','e','m','-','u','n','s','i','g','n','a','l'} },

	{ 0, 0,                       hcl_pf_semaphore_group_new,               9, { 's','e','m','g','r','-','n','e','w'} },
	{ 1, 2,                       hcl_pf_semaphore_group_add_semaphore,     9, { 's','e','m','g','r','-','a','d','d'} },
	{ 1, 2,                       hcl_pf_semaphore_group_remove_semaphore, 12, { 's','e','m','g','r','-','r','e','m','o','v','e'} },
	{ 1, 1,                       hcl_pf_semaphore_group_wait,             10, { 's','e','m','g','r','-','w','a','i','t'} }
};

int hcl_addbuiltinprims (hcl_t* hcl)
{
	hcl_oow_t i;
	hcl_oop_t prim, name;
	hcl_oop_cons_t cons;

	for (i = 0; i < HCL_COUNTOF(builtin_prims); i++)
	{
		prim = hcl_makeprim(hcl, builtin_prims[i].impl, builtin_prims[i].minargs, builtin_prims[i].maxargs, HCL_NULL);
		if (HCL_UNLIKELY(!prim)) return -1;

		hcl_pushvolat (hcl, &prim);
		name = hcl_makesymbol(hcl, builtin_prims[i].name, builtin_prims[i].namelen);
		hcl_popvolat (hcl);
		if (HCL_UNLIKELY(!name)) return -1;

		hcl_pushvolat (hcl, &name);
		cons = hcl_putatsysdic(hcl, name, prim);
		hcl_popvolat (hcl);
		if (HCL_UNLIKELY(!cons)) return -1;

		/* turn on the kernel bit in the symbol associated with a primitive
		 * function. 'set' prevents this symbol from being used as a variable
		 * name */
		HCL_OBJ_SET_FLAGS_KERNEL (name, 2);
	}

	return 0;
}

