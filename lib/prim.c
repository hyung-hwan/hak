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

struct pf_t
{
	hak_oow_t minargs;
	hak_oow_t maxargs;
	hak_pfimpl_t impl;

	hak_oow_t namelen;
	hak_ooch_t name[32];
};
typedef struct pf_t pf_t;

/* ------------------------------------------------------------------------- */

hak_oop_t hak_makeprim (hak_t* hak, hak_pfimpl_t primimpl, hak_oow_t minargs, hak_oow_t maxargs, hak_mod_t* mod)
{
	hak_oop_prim_t v; /* in principle, hak_oop_word_t with HAK_PRIM_NUM_WORDS elements */

	v = (hak_oop_prim_t)hak_instantiate(hak, hak->c_primitive, HAK_NULL, 0);
	if (HAK_UNLIKELY(!v))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt(hak, HAK_ERRNUM(hak),
			"unable to instantiate %O - %js", hak->c_primitive->name, orgmsg);
	}
	else
	{
		v->impl = (hak_oow_t)primimpl;
		v->min_nargs = minargs;
		v->max_nargs = maxargs;
		v->mod = (hak_oow_t)mod;
	}

	return (hak_oop_t)v;
}

/* ------------------------------------------------------------------------- */

static void log_char_object (hak_t* hak, hak_bitmask_t mask, hak_oop_char_t msg)
{
	hak_ooi_t n;
	hak_oow_t rem;
	const hak_ooch_t* ptr;

	HAK_ASSERT(hak, HAK_OBJ_GET_FLAGS_TYPE(msg) == HAK_OBJ_TYPE_CHAR);

	rem = HAK_OBJ_GET_SIZE(msg);
	ptr = msg->slot;

start_over:
	while (rem > 0)
	{
		if (*ptr == '\0')
		{
			n = hak_logbfmt(hak, mask, "%jc", *ptr);
			HAK_ASSERT(hak, n == 1);
			rem -= n;
			ptr += n;
			goto start_over;
		}

		n = hak_logbfmt(hak, mask, "%.*js", rem, ptr);
		if (n <= -1) break;
		if (n == 0)
		{
			/* to skip the unprinted character.
			 * actually, this check is not needed because of '\0' skipping
			 * at the beginning  of the loop */
			n = hak_logbfmt(hak, mask, "%jc", *ptr);
			HAK_ASSERT(hak, n == 1);
		}
		rem -= n;
		ptr += n;
	}
}

static hak_pfrc_t pf_log (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
/* TODO: accept log level */
	hak_oop_t msg;
	hak_bitmask_t mask;
	hak_ooi_t k;

	/*level = HAK_STACK_GET(hak, hak->sp - nargs + 1);
	if (!HAK_OOP_IS_SMOOI(level)) mask = HAK_LOG_APP | HAK_LOG_INFO;
	else mask = HAK_LOG_APP | HAK_OOP_TO_SMOOI(level);*/
	mask = HAK_LOG_APP | HAK_LOG_FATAL; /* TODO: accept logging level .. */

	for (k = 0; k < nargs; k++)
	{
		msg = HAK_STACK_GETARG(hak, nargs, k);

		if (msg == hak->_nil || msg == hak->_true || msg == hak->_false)
		{
			goto dump_object;
		}
		else if (HAK_OOP_IS_CHAR(msg))
		{
			hak_logbfmt(hak, mask, "%jc", HAK_OOP_TO_CHAR(msg));
		}
		else if (HAK_OOP_IS_POINTER(msg))
		{
			if (HAK_OBJ_GET_FLAGS_TYPE(msg) == HAK_OBJ_TYPE_CHAR)
			{
				log_char_object(hak, mask, (hak_oop_char_t)msg);
			}
			else if (HAK_OBJ_GET_FLAGS_TYPE(msg) == HAK_OBJ_TYPE_OOP)
			{
				/* visit only 1-level down into an array-like object */
				hak_oop_t inner;
				hak_oow_t i;

				if (HAK_OBJ_GET_CLASS(msg) != (hak_oop_t)hak->c_array) goto dump_object;

				for (i = 0; i < HAK_OBJ_GET_SIZE(msg); i++)
				{
					inner = ((hak_oop_oop_t)msg)->slot[i];

					if (i > 0) hak_logbfmt(hak, mask, " ");
					if (HAK_OOP_IS_CHAR(inner))
					{
						hak_logbfmt(hak, mask, "%jc", HAK_OOP_TO_CHAR(inner));
					}
					else if (HAK_OOP_IS_POINTER(inner) && HAK_OBJ_GET_FLAGS_TYPE(inner) == HAK_OBJ_TYPE_CHAR)
					{
						log_char_object(hak, mask, (hak_oop_char_t)inner);
					}
					else
					{
						hak_logbfmt(hak, mask, "%O", inner);
					}
				}
			}
			else goto dump_object;
		}
		else
		{
		dump_object:
			hak_logbfmt(hak, mask, "%O", msg);
		}
	}

	HAK_STACK_SETRET(hak, nargs, hak->_nil);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_logf (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	if (hak_logfmtcallstack(hak, nargs) <= -1)
	{
		HAK_STACK_SETRETTOERRNUM(hak, nargs);
	}
	else
	{
/* TODO: better return code? */
		HAK_STACK_SETRET(hak, nargs, hak->_nil);
	}

	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_printf (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	if (hak_prfmtcallstack(hak, nargs) <= -1)
	{
		HAK_STACK_SETRETTOERRNUM(hak, nargs);
	}
	else
	{
/* TODO: better return code? */
		HAK_STACK_SETRET(hak, nargs, hak->_nil);
	}

	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_sprintf (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	if (hak_strfmtcallstack(hak, nargs) <= -1)
	{
		HAK_STACK_SETRETTOERRNUM(hak, nargs);
	}
	else
	{
		hak_oop_t str;
		str = hak_makestring(hak, hak->sprintf.xbuf.ptr, hak->sprintf.xbuf.len);
		if (!str) return HAK_PF_FAILURE;

		HAK_STACK_SETRET(hak, nargs, str);
	}

	return HAK_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */

static hak_oow_t move_udi_residue_bytes (hak_io_udiarg_t* curinp)
{
	hak_oow_t cpl;

	cpl = HAK_COUNTOF(curinp->rsd.buf) - curinp->rsd.len;
	if (cpl > 0)
	{
		hak_oow_t avail;
		avail = curinp->b.len - curinp->b.pos; /* available in the read buffer */
		if (cpl > avail) cpl = avail;
		HAK_MEMCPY(&curinp->rsd.buf[curinp->rsd.len], &curinp->buf.b[curinp->b.pos], cpl);
		curinp->rsd.len += cpl;
		curinp->b.pos += cpl; /* advance the position because the bytes moved to the residue buffer */
	}
	return curinp->rsd.len;
}

static int get_udi_char (hak_t* hak, hak_ooch_t* ch)
{
	hak_io_udiarg_t* curinp;
	hak_ooch_t c;
	hak_oow_t taken;
	int x;

	curinp = &hak->io.udi_arg;

#if defined(HAK_OOCH_IS_UCH)
	if (curinp->byte_oriented)
	{
		hak_cmgr_t* cmgr;
		const hak_uint8_t* inpptr;
		hak_oow_t inplen, n;

		cmgr = HAK_CMGR(hak);

	start_over:
		if (curinp->b.pos >= curinp->b.len)
		{
			x = hak->io.udi_rdr(hak, HAK_IO_READ_BYTES, curinp);
			if (x <= -1)
			{
				const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
				hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to read bytes from input stream - %js", orgmsg);
				return -1;
			}

			if (curinp->xlen <= 0)
			{
				/* got EOF from an included stream */
				if (curinp->rsd.len > 0)
				{
					hak_seterrbfmt(hak, HAK_EECERR, "incomplete byte sequence in input stream");
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
			HAK_ASSERT(hak, curinp->b.pos == 0);
			inplen = move_udi_residue_bytes(curinp);
			inpptr = &curinp->rsd.buf[0];
		}
		else
		{
			inplen = curinp->b.len - curinp->b.pos;
			inpptr = &curinp->buf.b[curinp->b.pos];
		}

		n = cmgr->bctouc((const hak_bch_t*)inpptr, inplen, &c);
		if (n == 0) /* invalid sequence */
		{
			hak_seterrbfmt(hak, HAK_EECERR, "invalid byte sequence in input stream");
			return -1;
		}
		if (n > inplen) /* incomplete sequence */
		{
			HAK_ASSERT(hak, curinp->rsd.len < HAK_COUNTOF(curinp->rsd.buf));
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
			x = hak->io.udi_rdr(hak, HAK_IO_READ, curinp);
			if (x <= -1)
			{
				const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
				hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to read input stream - %js", orgmsg);
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
#if defined(HAK_OOCH_IS_UCH)
	}
#endif

	curinp->b.pos += taken;
#if defined(HAK_OOCH_IS_UCH)
	curinp->rsd.len = 0; /* clear up the residue byte buffer. needed for byte reading only */
#endif

	*ch = c;
	return 1;
}

static int get_udi_byte (hak_t* hak, hak_uint8_t* bt)
{
	hak_io_udiarg_t* curinp;
	int x;

#if defined(HAK_OOCH_IS_UCH)
	if (!hak->io.udi_arg.byte_oriented)
	{
/* TODO: convert characters to bytes? but do we know the original encoding? */
		hak_seterrbfmt(hak, HAK_EPERM, "byte-oriented input prohibited on character-oriented stream");
		return -1;
	}
#endif

	curinp = &hak->io.udi_arg;
	if (curinp->b.pos >= curinp->b.len)
	{
		x = hak->io.udi_rdr(hak, HAK_IO_READ_BYTES, curinp);
		if (x <= -1)
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to read input stream - %js", orgmsg);
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

static hak_pfrc_t pf_getbyte (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t v;
	hak_uint8_t bt;
	int n;

	n = get_udi_byte(hak, &bt);
	if (n <= -1) return HAK_PF_FAILURE;

	/* return nil on EOF, or the actual character read */
	v = (n == 0)? hak->_nil: HAK_SMOOI_TO_OOP(bt);
	HAK_STACK_SETRET(hak, nargs, v);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_getch (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t v;
	hak_ooch_t ch;
	int n;

	n = get_udi_char(hak, &ch);
	if (n <= -1) return HAK_PF_FAILURE;

	/* return nil on EOF, or the actual character read */
	v = (n == 0)? hak->_nil: HAK_CHAR_TO_OOP(ch);
	HAK_STACK_SETRET(hak, nargs, v);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_gets (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_io_udiarg_t* curinp;
	hak_oop_t v;

	curinp = &hak->io.udi_arg;
	if (curinp->eof_reached)
	{
		v = hak->_nil;
	}
	else
	{
		int n;
		hak_ooch_t ch;
		hak_ooch_t buf[10];
		hak_ooch_t* ptr;
		hak_oow_t len, capa;

		ptr = buf;
		len = 0;
		capa = HAK_COUNTOF(buf);
		while (1)
		{
			n = get_udi_char(hak, &ch);
			if (n <= -1) return HAK_PF_FAILURE;
			if (n == 0) break;

			if (len >= capa)
			{
				hak_ooch_t* tmp;
				hak_oow_t newcapa;

				newcapa = capa + HAK_COUNTOF(buf);
				if (ptr == buf)
				{
					tmp = (hak_ooch_t*)hak_allocmem(hak, HAK_SIZEOF(*ptr) * newcapa);
					if (HAK_UNLIKELY(!tmp)) return HAK_PF_FAILURE;
					HAK_MEMCPY(tmp, buf, HAK_SIZEOF(buf));
				}
				else
				{
					tmp = (hak_ooch_t*)hak_reallocmem(hak, ptr, HAK_SIZEOF(*ptr) * newcapa);
					if (HAK_UNLIKELY(!tmp))
					{
						hak_freemem(hak, ptr);
						return HAK_PF_FAILURE;
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
			HAK_ASSERT(hak, ptr == buf);
			v = hak->_nil;
		}
		else
		{
			v = hak_makestring(hak, ptr, len);
			if (ptr != buf) hak_freemem(hak, ptr);
			if (HAK_UNLIKELY(!v)) return HAK_PF_FAILURE;
		}

	}

	HAK_STACK_SETRET(hak, nargs, v);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_scanf (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	if (hak_scfmtcallstack(hak, nargs) <= -1)
	{
		HAK_STACK_SETRETTOERRNUM(hak, nargs);
	}
	else
	{
/* TODO: better return code? */
		HAK_STACK_SETRET(hak, nargs, hak->_nil);
	}

	return HAK_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */

static hak_pfrc_t pf_gc (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_gc(hak, 1);
	HAK_STACK_SETRET(hak, nargs, hak->_nil);
	return HAK_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */

static hak_pfrc_t pf_eqv (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t a0, a1, rv;

	a0 = HAK_STACK_GETARG(hak, nargs, 0);
	a1 = HAK_STACK_GETARG(hak, nargs, 1);

	rv = (a0 == a1? hak->_true: hak->_false);

	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_eql (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	int n;
	n = hak_equalobjs(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_STACK_GETARG(hak, nargs, 1));
	if (n <= -1) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, (n? hak->_true: hak->_false));
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_eqk (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	/* equal kind? */
	hak_oop_t a0, a1, rv;

	a0 = HAK_STACK_GETARG(hak, nargs, 0);
	a1 = HAK_STACK_GETARG(hak, nargs, 1);

	rv = (HAK_CLASSOF(hak, a0) == HAK_CLASSOF(hak, a1)? hak->_true: hak->_false);

	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_nqv (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t a0, a1, rv;

	a0 = HAK_STACK_GETARG(hak, nargs, 0);
	a1 = HAK_STACK_GETARG(hak, nargs, 1);

	rv = (a0 != a1? hak->_true: hak->_false);

	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_nql (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	int n;
	n = hak_equalobjs(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_STACK_GETARG(hak, nargs, 1));
	if (n <= -1) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, (!n? hak->_true: hak->_false));
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_nqk (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	/* not equal kind? */
	hak_oop_t a0, a1, rv;

	a0 = HAK_STACK_GETARG(hak, nargs, 0);
	a1 = HAK_STACK_GETARG(hak, nargs, 1);

	rv = (HAK_CLASSOF(hak, a0) != HAK_CLASSOF(hak, a1)? hak->_true: hak->_false);

	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */

static hak_pfrc_t pf_is_nil (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rv;
	rv = (HAK_STACK_GETARG(hak, nargs, 0) == hak->_nil)? hak->_true: hak->_false;
	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}


static hak_pfrc_t pf_is_boolean (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rv, x;
	x = HAK_STACK_GETARG(hak, nargs, 0);
	rv = (HAK_IS_TRUE(hak, x) || HAK_IS_FALSE(hak, x))? hak->_true: hak->_false;
	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_is_character (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rv, x;
	x = HAK_STACK_GETARG(hak, nargs, 0);
	rv = (HAK_OOP_IS_CHAR(x))? hak->_true: hak->_false;
	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_is_error (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rv, x;
	x = HAK_STACK_GETARG(hak, nargs, 0);
	rv = (HAK_OOP_IS_ERROR(x))? hak->_true: hak->_false;
	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_is_smptr (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rv, x;
	x = HAK_STACK_GETARG(hak, nargs, 0);
	rv = (HAK_OOP_IS_SMPTR(x))? hak->_true: hak->_false;
	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_is_integer (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rv, x;
	x = HAK_STACK_GETARG(hak, nargs, 0);
	rv = (hak_isint(hak, x))? hak->_true: hak->_false;
	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_is_numeric(hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rv, x;
	x = HAK_STACK_GETARG(hak, nargs, 0);
	rv = (hak_isint(hak, x) || HAK_IS_FPDEC(hak, x))? hak->_true: hak->_false;
	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_is_string (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rv, x;
	x = HAK_STACK_GETARG(hak, nargs, 0);
	rv = (HAK_IS_STRING(hak, x))? hak->_true: hak->_false;
	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_is_array (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rv, x;
	x = HAK_STACK_GETARG(hak, nargs, 0);
	rv = (HAK_IS_ARRAY(hak, x))? hak->_true: hak->_false;
	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_is_bytearray (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rv, x;
	x = HAK_STACK_GETARG(hak, nargs, 0);
	rv = (HAK_IS_BYTEARRAY(hak, x))? hak->_true: hak->_false;
	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_is_dictionary (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rv, x;
	x = HAK_STACK_GETARG(hak, nargs, 0);
	rv = (HAK_IS_DIC(hak, x))? hak->_true: hak->_false;
	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_is_compiled_block (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rv, x;
	x = HAK_STACK_GETARG(hak, nargs, 0);
	rv = (HAK_IS_COMPILED_BLOCK(hak, x))? hak->_true: hak->_false;
	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_is_class (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rv, x;
	x = HAK_STACK_GETARG(hak, nargs, 0);
	rv = (HAK_IS_CLASS(hak, x))? hak->_true: hak->_false;
	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_is_object (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rv, x;
	x = HAK_STACK_GETARG(hak, nargs, 0);
	/*rv = (HAK_IS_INSTANCE(hak, x))? hak->_true: hak->_false;*/
	rv = (!HAK_IS_CLASS(hak, x))? hak->_true: hak->_false; /* true if not a class object itself */
	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */

static hak_pfrc_t pf_not (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t arg, rv;

	arg = HAK_STACK_GETARG(hak, nargs, 0);
	if (arg == hak->_true) rv = hak->_false;
	else if (arg == hak->_false) rv = hak->_true;
	else
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "boolean parameter expected - %O", arg);
		return HAK_PF_FAILURE;
	}

	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_and (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t arg, rv;
	hak_ooi_t i;

	rv = hak->_true;
	for (i = 1; i < nargs; i++)
	{
		arg = HAK_STACK_GETARG(hak, nargs, i);
		if (arg == hak->_true)
		{
			/* do nothing */
		}
		else if (arg == hak->_false)
		{
			rv = hak->_false;
			break;
		}
		else
		{
			hak_seterrbfmt(hak, HAK_EINVAL, "boolean parameter expected - %O", arg);
			return HAK_PF_FAILURE;
		}
	}

	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_or (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t arg, rv;
	hak_ooi_t i;

	rv = hak->_false;
	for (i = 1; i < nargs; i++)
	{
		arg = HAK_STACK_GETARG(hak, nargs, i);
		if (arg == hak->_true)
		{
			rv = hak->_true;
			break;
		}
		else if (arg == hak->_false)
		{
			/* do nothing */
		}
		else
		{
			hak_seterrbfmt(hak, HAK_EINVAL, "boolean parameter expected - %O", arg);
			return HAK_PF_FAILURE;
		}
	}

	HAK_STACK_SETRET(hak, nargs, rv);
	return HAK_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */

hak_pfrc_t hak_pf_number_add (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_ooi_t i;
	hak_oop_t arg, ret;

	ret = HAK_STACK_GETARG(hak, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HAK_STACK_GETARG(hak, nargs, i);
		/*ret = hak_addints(hak, ret, arg);*/
		ret = hak_addnums(hak, ret, arg);
		if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;
	}

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_number_sub (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_ooi_t i;
	hak_oop_t arg, ret;

	ret = HAK_STACK_GETARG(hak, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HAK_STACK_GETARG(hak, nargs, i);
		/*ret = hak_subints(hak, ret, arg);*/
		ret = hak_subnums(hak, ret, arg);
		if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;
	}

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_number_mul (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_ooi_t i;
	hak_oop_t arg, ret;

	ret = HAK_STACK_GETARG(hak, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HAK_STACK_GETARG(hak, nargs, i);
		/*ret = hak_mulints(hak, ret, arg);*/
		ret = hak_mulnums(hak, ret, arg);
		if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;
	}

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_number_mlt (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_ooi_t i;
	hak_oop_t arg, ret;

	ret = HAK_STACK_GETARG(hak, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HAK_STACK_GETARG(hak, nargs, i);
		ret = hak_mltnums(hak, ret, arg);
		if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;
	}

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_number_div (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_ooi_t i;
	hak_oop_t arg, ret;

	ret = HAK_STACK_GETARG(hak, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HAK_STACK_GETARG(hak, nargs, i);
		ret = hak_divnums(hak, ret, arg);
		if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;
	}

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_integer_quo (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_ooi_t i;
	hak_oop_t arg, ret;

	ret = HAK_STACK_GETARG(hak, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HAK_STACK_GETARG(hak, nargs, i);
		ret = hak_divints(hak, ret, arg, 0, HAK_NULL);
		if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;
	}

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_integer_rem (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_ooi_t i;
	hak_oop_t arg, ret, rem;

	ret = HAK_STACK_GETARG(hak, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HAK_STACK_GETARG(hak, nargs, i);
		ret = hak_divints(hak, ret, arg, 0, &rem);
		if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;
		ret = rem;
	}

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_integer_mquo (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_ooi_t i;
	hak_oop_t arg, ret;

	ret = HAK_STACK_GETARG(hak, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HAK_STACK_GETARG(hak, nargs, i);
		ret = hak_divints(hak, ret, arg, 1, HAK_NULL);
		if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;
	}

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_integer_mod (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_ooi_t i;
	hak_oop_t arg, ret, rem;

	ret = HAK_STACK_GETARG(hak, nargs, 0);
	for (i = 1; i < nargs; i++)
	{
		arg = HAK_STACK_GETARG(hak, nargs, i);
		ret = hak_divints(hak, ret, arg, 1, &rem);
		if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;
		ret = rem;
	}

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_number_sqrt (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t ret;
	ret = hak_sqrtnum(hak, HAK_STACK_GETARG(hak, nargs, 0));
	if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_number_abs (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t ret;
	ret = hak_absnum(hak, HAK_STACK_GETARG(hak, nargs, 0));
	if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_number_gt (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t ret;
	ret = hak_gtnums(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_STACK_GETARG(hak, nargs, 1));
	if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}


hak_pfrc_t hak_pf_number_ge (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t ret;
	ret = hak_genums(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_STACK_GETARG(hak, nargs, 1));
	if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_number_lt (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t ret;
	ret = hak_ltnums(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_STACK_GETARG(hak, nargs, 1));
	if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}
hak_pfrc_t hak_pf_number_le (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t ret;
	ret = hak_lenums(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_STACK_GETARG(hak, nargs, 1));
	if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}
hak_pfrc_t hak_pf_number_eq (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t ret;
	ret = hak_eqnums(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_STACK_GETARG(hak, nargs, 1));
	if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}
hak_pfrc_t hak_pf_number_ne (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t ret;
	ret = hak_nenums(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_STACK_GETARG(hak, nargs, 1));
	if (HAK_UNLIKELY(!ret)) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}



static hak_pfrc_t pf_integer_band (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t ret;
	ret = hak_bitandints(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_STACK_GETARG(hak, nargs, 1));
	if (!ret) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_integer_bor (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t ret;
	ret = hak_bitorints(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_STACK_GETARG(hak, nargs, 1));
	if (!ret) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}
static hak_pfrc_t pf_integer_bxor (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t ret;
	ret = hak_bitxorints(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_STACK_GETARG(hak, nargs, 1));
	if (!ret) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}
static hak_pfrc_t pf_integer_bnot (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t ret;
	ret = hak_bitinvint(hak, HAK_STACK_GETARG(hak, nargs, 0));
	if (!ret) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}
static hak_pfrc_t pf_integer_bshift (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t ret;
	ret = hak_bitshiftint(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_STACK_GETARG(hak, nargs, 1));
	if (!ret) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, ret);
	return HAK_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */
static hak_pfrc_t pf_va_context (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	HAK_STACK_SETRET(hak, nargs, (hak_oop_t)hak->active_context);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_va_count (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_context_t ctx;
	hak_ooi_t attr_mask, /*va,*/ fixed_nargs, nrvars, nlvars, nvaargs;

	if (nargs >= 1)
	{
		ctx = (hak_oop_context_t)HAK_STACK_GETARG(hak, nargs, 0);
		if (!HAK_IS_CONTEXT(hak, ctx))
		{
			hak_seterrbfmt(hak, HAK_EINVAL, "not a proper va context - %O", ctx);
			return HAK_PF_FAILURE;
		}
	}
	else
	{
		ctx = hak->active_context;
	}

	attr_mask = HAK_OOP_TO_SMOOI(ctx->attr_mask);

	/*va = GET_BLK_MASK_VA(attr_mask);*/
	fixed_nargs = GET_BLK_MASK_NARGS(attr_mask);
	nrvars = GET_BLK_MASK_NRVARS(attr_mask);
	nlvars = GET_BLK_MASK_NLVARS(attr_mask);

	/*if (!va) TODO: need this check?
	{
	}*/

	nvaargs = HAK_OBJ_GET_SIZE(ctx) - fixed_nargs - nrvars - nlvars - HAK_CONTEXT_NAMED_INSTVARS;
	HAK_STACK_SETRET(hak, nargs, HAK_SMOOI_TO_OOP(nvaargs));
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_va_get (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_context_t ctx;
	hak_ooi_t attr_mask, /*va,*/ fixed_nargs, nrvars, nlvars, nvaargs;
	hak_oow_t index;
	hak_oop_t idx;
	int n;

	if (nargs >= 2)
	{
		ctx = (hak_oop_context_t)HAK_STACK_GETARG(hak, nargs, 1);
		if (!HAK_IS_CONTEXT(hak, ctx))
		{
			hak_seterrbfmt(hak, HAK_EINVAL, "not a proper va context - %O", ctx);
			return HAK_PF_FAILURE;
		}
	}
	else
	{
		ctx = hak->active_context;
	}
	attr_mask = HAK_OOP_TO_SMOOI(ctx->attr_mask);

	/*va = GET_BLK_MASK_VA(attr_mask);*/
	fixed_nargs = GET_BLK_MASK_NARGS(attr_mask);
	nrvars = GET_BLK_MASK_NRVARS(attr_mask);
	nlvars = GET_BLK_MASK_NLVARS(attr_mask);

	idx = HAK_STACK_GETARG(hak, nargs, 0);
	n = hak_inttooow_noseterr(hak, idx, &index);
	if (n <= 0)
	{
		if (n <= -1) hak_seterrbfmt(hak, HAK_EINVAL, "invalid index - %O", idx);
		return HAK_PF_FAILURE;
	}

	/*
	if (!va) TODO: need this check?
	{
	}*/

	nvaargs = HAK_OBJ_GET_SIZE(ctx) - fixed_nargs - nrvars - nlvars - HAK_CONTEXT_NAMED_INSTVARS;
	if (index >= nvaargs)
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "va index(%zu) out of bounds for va of size %zd", index, nvaargs);
		return HAK_PF_FAILURE;
	}

	HAK_STACK_SETRET(hak, nargs, ctx->slot[fixed_nargs + nrvars + nlvars + index]);
	return HAK_PF_SUCCESS;
}


static hak_pfrc_t pf_object_new (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t obj;
	hak_oop_t _class;
	hak_oow_t size = 0;

	_class = HAK_STACK_GETARG(hak, nargs, 0);
	if (!HAK_IS_CLASS(hak, _class))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "not a class - %O", _class);
		return HAK_PF_FAILURE;
	}

	if (nargs >= 2)
	{
		int n;
		hak_oop_t sz;

		sz = HAK_STACK_GETARG(hak, nargs, 1);
		n = hak_inttooow_noseterr(hak, sz, &size);
		if (n <= 0)
		{
			hak_seterrbfmt(hak, HAK_EINVAL, "invalid size - %O", sz);
			return HAK_PF_FAILURE;
		}
	}

	obj = hak_instantiate(hak, (hak_oop_class_t)_class, HAK_NULL, size);
	if (HAK_UNLIKELY(!obj)) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, obj);
	return HAK_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */

static hak_pfrc_t pf_system_get_sigfd (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_ooi_t fd;
	fd = hak->vmprim.vm_getsigfd(hak);
	HAK_STACK_SETRET(hak, nargs, HAK_SMOOI_TO_OOP(fd));
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_system_get_sig (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_uint8_t sig;
	int n;

	n = hak->vmprim.vm_getsig(hak, &sig);
	if (n <= -1) return HAK_PF_FAILURE;

	if (n == 0) HAK_STACK_SETRETTOERROR(hak, nargs, HAK_ENOENT);
	else HAK_STACK_SETRET(hak, nargs, HAK_SMOOI_TO_OOP((hak_ooi_t)sig));

	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_system_set_sig (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t tmp;
	hak_uint8_t sig;
	int n;

	tmp = HAK_STACK_GETARG(hak, nargs, 0);
	HAK_PF_CHECK_ARGS(hak, nargs, HAK_OOP_IS_SMOOI(tmp));

	sig = (hak_uint8_t)HAK_OOP_TO_SMOOI(tmp);
	n = hak->vmprim.vm_setsig(hak, sig);
	if (n <= -1) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, HAK_SMOOI_TO_OOP((hak_ooi_t)sig));

	return HAK_PF_SUCCESS;
}

/* ------------------------------------------------------------------------- */

static pf_t builtin_prims[] =
{
	/* TODO: move these primitives to modules... */

	{ 0, 0,                       pf_getbyte,         7,  { 'g','e','t','b','y','t','e' } },
	{ 0, 0,                       pf_getch,           5,  { 'g','e','t','c','h' } },
	{ 0, 0,                       pf_gets,            4,  { 'g','e','t','s' } },
	{ 0, HAK_TYPE_MAX(hak_oow_t), pf_log,             3,  { 'l','o','g' } },
	{ 1, HAK_TYPE_MAX(hak_oow_t), pf_logf,            4,  { 'l','o','g','f' } },
	{ 1, HAK_TYPE_MAX(hak_oow_t), pf_printf,          6,  { 'p','r','i','n','t','f' } },
	{ 1, HAK_TYPE_MAX(hak_oow_t), pf_scanf,           5,  { 's','c','a','n','f' } },
	{ 1, HAK_TYPE_MAX(hak_oow_t), pf_sprintf,         7,  { 's','p','r','i','n','t','f' } },

	{ 0, 0,                       pf_system_get_sigfd,16, { 's','y','s','t','e','m','-','g','e','t','-','s','i','g','f','d' } },
	{ 0, 0,                       pf_system_get_sig,  14,  { 's','y','s','t','e','m','-','g','e','t','-','s','i','g' } },
	{ 1, 1,                       pf_system_set_sig,  14,  { 's','y','s','t','e','m','-','s','e','t','-','s','i','g' } },

	{ 0, 0,                       pf_gc,              2,  { 'g','c' } },

	{ 1, 1,                       pf_not,             3,  { 'n','o','t' } },
	/* this is a long-circuit logical and the short-curcuit 'and' is treated as a special form */
	{ 2, HAK_TYPE_MAX(hak_oow_t), pf_and,             4,  { '_','a','n','d' } },
	/* this is a long-cirtuit logical or. the short-circuit 'or' is treated as a special form */
	{ 2, HAK_TYPE_MAX(hak_oow_t), pf_or,              3,  { '_','o','r' } },

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
	{ 1, 1,                       pf_is_compiled_block, 4,  { 'f','u','n','?' } },
	{ 1, 1,                       pf_is_class,        6,  { 'c','l','a','s','s','?' } },
	{ 1, 1,                       pf_is_object,       7,  { 'o','b','j','e','c','t','?' } },


	{ 1, HAK_TYPE_MAX(hak_oow_t), hak_pf_number_add,  1,  { '+' } },
	{ 1, HAK_TYPE_MAX(hak_oow_t), hak_pf_number_sub,  1,  { '-' } },
	{ 1, HAK_TYPE_MAX(hak_oow_t), hak_pf_number_mul,  1,  { '*' } },
	{ 1, HAK_TYPE_MAX(hak_oow_t), pf_number_mlt,      3,  { 'm','l','t' } },
	{ 1, HAK_TYPE_MAX(hak_oow_t), hak_pf_number_div,  1,  { '/' } },
	{ 1, 1,                       hak_pf_number_sqrt, 4,  { 's','q','r','t' } },
	{ 1, 1,                       hak_pf_number_abs,  3,  { 'a','b','s' } },

	{ 2, 2,                       hak_pf_number_gt,   1,  { '>' } },
	{ 2, 2,                       hak_pf_number_ge,   2,  { '>','=' } },
	{ 2, 2,                       hak_pf_number_lt,   1,  { '<' } },
	{ 2, 2,                       hak_pf_number_le,   2,  { '<','=' } },
	{ 2, 2,                       hak_pf_number_eq,   1,  { '=' } },
	{ 2, 2,                       hak_pf_number_eq,   2,  { '=', '=' } },
	{ 2, 2,                       hak_pf_number_ne,   2,  { '~','=' } },

	/* bitwise operations are supported for integers only */
	{ 2, 2,                       pf_integer_band,    7,  { 'b','i','t','-','a','n','d' } },
	{ 2, 2,                       pf_integer_bor,     6,  { 'b','i','t','-','o','r' } },
	{ 2, 2,                       pf_integer_bxor,    7,  { 'b','i','t','-','x','o','r' } },
	{ 1, 1,                       pf_integer_bnot,    7,  { 'b','i','t','-','n','o','t' } },
	{ 2, 2,                       pf_integer_bshift,  9,  { 'b','i','t','-','s','h','i','f','t'  } },

	{ 1, HAK_TYPE_MAX(hak_oow_t), pf_integer_quo,     3,  { 'd','i','v' } },
	{ 2, HAK_TYPE_MAX(hak_oow_t), pf_integer_rem,     3,  { 'r','e','m' } },
	{ 1, HAK_TYPE_MAX(hak_oow_t), pf_integer_mquo,    4,  { 'm','d','i','v' } },
	{ 2, HAK_TYPE_MAX(hak_oow_t), pf_integer_mod,     3,  { 'm','o','d' } },

	{ 0, 0,                       pf_va_context,      10, { 'v','a','-','c','o','n','t','e','x','t' } },
	{ 0, 1,                       pf_va_count,        8,  { 'v','a','-','c','o','u','n','t' } },
	{ 1, 2,                       pf_va_get,          6,  { 'v','a','-','g','e','t' } },

	{ 1, 2,                       pf_object_new,                           10, { 'o','b','j','e','c','t','-','n','e','w' } },

	{ 0, 0,                       hak_pf_process_current,                  15, { 'c','u','r','r','e','n','t','-','p','r','o','c','e','s','s'} },
	{ 1, HAK_TYPE_MAX(hak_oow_t), hak_pf_process_fork,                      4, { 'f','o','r','k'} },
	{ 1, 1,                       hak_pf_process_resume,                    6, { 'r','e','s','u','m','e' } },
	{ 0, 1,                       hak_pf_process_suspend,                   7, { 's','u','s','p','e','n','d' } },
	{ 0, 1,                       hak_pf_process_terminate,                 9, { 't','e','r','m','i','n','a','t','e' } },
	{ 0, 0,                       hak_pf_process_terminate_all,            13, { 't','e','r','m','i','n','a','t','e','-','a','l','l' } },
	{ 0, 0,                       hak_pf_process_yield,                     5, { 'y','i','e','l','d'} },


	{ 0, 0,                       hak_pf_semaphore_new,                     7, { 's','e','m','-','n','e','w'} },
	{ 1, 1,                       hak_pf_semaphore_wait,                    8, { 's','e','m','-','w','a','i','t'} },
	{ 1, 3,                       hak_pf_semaphore_signal,                 10, { 's','e','m','-','s','i','g','n','a','l'} },
	{ 2, 2,                       hak_pf_semaphore_signal_on_input,        19, { 's','e','m','-','s','i','g','n','a','l','-','o','n','-','i','n','p','u','t'} },
	{ 2, 2,                       hak_pf_semaphore_signal_on_output,       20, { 's','e','m','-','s','i','g','n','a','l','-','o','n','-','o','u','t','p','u','t'} },
	{ 1, 1,                       hak_pf_semaphore_unsignal,               12, { 's','e','m','-','u','n','s','i','g','n','a','l'} },

	{ 0, 0,                       hak_pf_semaphore_group_new,               9, { 's','e','m','g','r','-','n','e','w'} },
	{ 1, 2,                       hak_pf_semaphore_group_add_semaphore,     9, { 's','e','m','g','r','-','a','d','d'} },
	{ 1, 2,                       hak_pf_semaphore_group_remove_semaphore, 12, { 's','e','m','g','r','-','r','e','m','o','v','e'} },
	{ 1, 1,                       hak_pf_semaphore_group_wait,             10, { 's','e','m','g','r','-','w','a','i','t'} }
};

int hak_addbuiltinprims (hak_t* hak)
{
	hak_oow_t i;
	hak_oop_t prim, name;
	hak_oop_cons_t cons;

	for (i = 0; i < HAK_COUNTOF(builtin_prims); i++)
	{
		prim = hak_makeprim(hak, builtin_prims[i].impl, builtin_prims[i].minargs, builtin_prims[i].maxargs, HAK_NULL);
		if (HAK_UNLIKELY(!prim))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to make primitive '%.*js' - %js",
				builtin_prims[i].namelen, builtin_prims[i].name, orgmsg);
			return -1;
		}

		hak_pushvolat(hak, &prim);
		name = hak_makesymbol(hak, builtin_prims[i].name, builtin_prims[i].namelen);
		hak_popvolat(hak);
		if (HAK_UNLIKELY(!name))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to make primitive name '%.*js' - %js",
				builtin_prims[i].namelen, builtin_prims[i].name, orgmsg);
			return -1;
		}

		hak_pushvolat(hak, &name);
		cons = hak_putatsysdic(hak, name, prim);
		hak_popvolat(hak);
		if (HAK_UNLIKELY(!cons))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to add primitive '%.*js' to system dictionary - %js",
				builtin_prims[i].namelen, builtin_prims[i].name, orgmsg);
			return -1;
		}

		/* turn on the kernel bit in the symbol associated with a primitive
		 * function. 'set' prevents this symbol from being used as a variable
		 * name */
		HAK_OBJ_SET_FLAGS_KERNEL (name, 2);
	}

	return 0;
}

