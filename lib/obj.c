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

#if defined(HCL_PROFILE_VM)
#include <sys/time.h>
#include <sys/resource.h> /* getrusage */
#endif

void* hcl_allocbytes (hcl_t* hcl, hcl_oow_t size)
{
	hcl_gchdr_t* gch;
	hcl_oow_t allocsize;
	int gc_called = 0;
#if defined(HCL_PROFILE_VM)
	struct rusage ru;
	hcl_ntime_t rut;
#endif

#if defined(HCL_BUILD_DEBUG)
	if ((hcl->option.trait & HCL_TRAIT_DEBUG_GC) && !(hcl->option.trait & HCL_TRAIT_NOGC)) hcl_gc (hcl, 1);
#endif

#if defined(HCL_PROFILE_VM)
	getrusage(RUSAGE_SELF, &ru);
	HCL_INIT_NTIME (&rut,  ru.ru_utime.tv_sec, HCL_USEC_TO_NSEC(ru.ru_utime.tv_usec));
#endif

	allocsize = HCL_SIZEOF(*gch) + size;

	if (hcl->gci.bsz >= hcl->gci.threshold)
	{
		hcl_gc (hcl, 0);
		hcl->gci.threshold = hcl->gci.bsz + 100000; /* TODO: change this fomula */
		gc_called = 1;
	}

	if (hcl->gci.lazy_sweep) hcl_gc_ms_sweep_lazy (hcl, allocsize);

	gch = (hcl_gchdr_t*)hcl_callocheapmem_noseterr(hcl, hcl->heap, allocsize);
	if (!gch)
	{
		if (HCL_UNLIKELY(hcl->option.trait & HCL_TRAIT_NOGC)) goto calloc_heapmem_fail;
		if (gc_called) goto sweep_the_rest;

		hcl_gc (hcl, 0);
		if (hcl->gci.lazy_sweep) hcl_gc_ms_sweep_lazy (hcl, allocsize);

		gch = (hcl_gchdr_t*)hcl_callocheapmem_noseterr(hcl, hcl->heap, allocsize);
		if (HCL_UNLIKELY(!gch))
		{
		sweep_the_rest:
			if (hcl->gci.lazy_sweep)
			{
				hcl_gc_ms_sweep_lazy (hcl, HCL_TYPE_MAX(hcl_oow_t)); /* sweep the rest */
				gch = (hcl_gchdr_t*)hcl_callocheapmem(hcl, hcl->heap, allocsize);
				if (HCL_UNLIKELY(!gch)) return HCL_NULL;
			}
			else
			{
			calloc_heapmem_fail:
				hcl_seterrnum (hcl, HCL_EOOMEM);
				return HCL_NULL;
			}
		}
	}

	if (hcl->gci.lazy_sweep && hcl->gci.ls.curr == hcl->gci.b)
	{
		/* if the lazy sweeping point is at the beginning of the allocation block,
		 * hcl->gc.ls.prev must get updated */
		HCL_ASSERT (hcl, hcl->gci.ls.prev == HCL_NULL);
		hcl->gci.ls.prev = gch;
	}

	gch->next = hcl->gci.b;
	hcl->gci.b = gch;
	hcl->gci.bsz += size;


#if defined(HCL_PROFILE_VM)
	getrusage(RUSAGE_SELF, &ru);
	HCL_SUB_NTIME_SNS (&rut, &rut, ru.ru_utime.tv_sec, HCL_USEC_TO_NSEC(ru.ru_utime.tv_usec));
	HCL_SUB_NTIME (&hcl->gci.stat.alloc, &hcl->gci.stat.alloc, &rut); /* do subtraction because rut is negative */
#endif
	return (hcl_uint8_t*)(gch + 1);

}

static HCL_INLINE hcl_oop_t alloc_oop_array (hcl_t* hcl, int brand, hcl_oow_t size, int ngc)
{
	hcl_oop_oop_t hdr;
	hcl_oow_t nbytes, nbytes_aligned;

	nbytes = size * HCL_SIZEOF(hcl_oop_t);

	/* this isn't really necessary since nbytes must be
	 * aligned already. */
	nbytes_aligned = HCL_ALIGN(nbytes, HCL_SIZEOF(hcl_oop_t));

	if (HCL_UNLIKELY(ngc))
	{
		hdr = (hcl_oop_oop_t)hcl_callocmem(hcl, HCL_SIZEOF(hcl_obj_t) + nbytes_aligned);
	}
	else
	{
		/* making the number of bytes to allocate a multiple of
		 * HCL_SIZEOF(hcl_oop_t) will guarantee the starting address
		 * of the allocated space to be an even number.
		 * see HCL_OOP_IS_NUMERIC() and HCL_OOP_IS_POINTER() */
		hdr = (hcl_oop_oop_t)hcl_allocbytes(hcl, HCL_SIZEOF(hcl_obj_t) + nbytes_aligned);
	}
	if (!hdr) return HCL_NULL;

	hdr->_flags = HCL_OBJ_MAKE_FLAGS(HCL_OBJ_TYPE_OOP, HCL_SIZEOF(hcl_oop_t), 0, 0, 0, ngc, 0, 0);
	HCL_OBJ_SET_SIZE (hdr, size);
	/*HCL_OBJ_SET_CLASS (hdr, hcl->_nil);*/
	HCL_OBJ_SET_FLAGS_BRAND (hdr, brand);

	while (size > 0) hdr->slot[--size] = hcl->_nil;

	return (hcl_oop_t)hdr;
}


hcl_oop_t hcl_allocoopobj (hcl_t* hcl, int brand, hcl_oow_t size)
{
	return alloc_oop_array(hcl, brand, size, 0);
}

hcl_oop_t hcl_allocoopobjwithtrailer (hcl_t* hcl, int brand, hcl_oow_t size, const hcl_oob_t* bptr, hcl_oow_t blen)
{
	hcl_oop_oop_t hdr;
	hcl_oow_t nbytes, nbytes_aligned;
	hcl_oow_t i;

	/* +1 for the trailer size of the hcl_oow_t type */
	nbytes = (size + 1) * HCL_SIZEOF(hcl_oop_t) + blen;
	nbytes_aligned = HCL_ALIGN(nbytes, HCL_SIZEOF(hcl_oop_t));

	hdr = (hcl_oop_oop_t)hcl_allocbytes(hcl, HCL_SIZEOF(hcl_obj_t) + nbytes_aligned);
	if (HCL_UNLIKELY(!hdr)) return HCL_NULL;

	hdr->_flags = HCL_OBJ_MAKE_FLAGS(HCL_OBJ_TYPE_OOP, HCL_SIZEOF(hcl_oop_t), 0, 0, 0, 0, 1, 0);
	HCL_OBJ_SET_SIZE (hdr, size);
	/*HCL_OBJ_SET_CLASS (hdr, hcl->_nil);*/
	HCL_OBJ_SET_FLAGS_BRAND (hdr, brand);

	for (i = 0; i < size; i++) hdr->slot[i] = hcl->_nil;

	/* [NOTE] this is not converted to a SMOOI object */
	hdr->slot[size] = (hcl_oop_t)blen;

	if (bptr)
	{
		HCL_MEMCPY (&hdr->slot[size + 1], bptr, blen);
	}
	else
	{
		HCL_MEMSET (&hdr->slot[size + 1], 0, blen);
	}

	return (hcl_oop_t)hdr;
}

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

/* ------------------------------------------------------------------------ *
 * COMMON OBJECTS
 * ------------------------------------------------------------------------ */

hcl_oop_t hcl_hatchundef (hcl_t* hcl)
{
	/* create the undef object for bootstrapping.
	 * this function doesn't set the class field */

	hcl_oop_t o;
	o = hcl_allocoopobj(hcl, HCL_BRAND_UNDEF, 0);
	if (HCL_LIKELY(o))
	{
		HCL_OBJ_SET_FLAGS_KERNEL(o, 1);
	}
	else
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to make undef - %js", orgmsg);
	}
	return o;
}

hcl_oop_t hcl_hatchnil (hcl_t* hcl)
{
	/* create the nil object for bootstrapping.
	 * this function doesn't set the class field */

	hcl_oop_t o;
	o = hcl_allocoopobj(hcl, HCL_BRAND_NIL, 0);
	if (HCL_LIKELY(o))
	{
		HCL_OBJ_SET_FLAGS_KERNEL(o, 1);
	}
	else
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to make nil - %js", orgmsg);
	}
	return o;
}

hcl_oop_t hcl_makebigint (hcl_t* hcl, int brand, const hcl_liw_t* ptr, hcl_oow_t len)
{
	hcl_oop_t oop;

	HCL_ASSERT (hcl, brand == HCL_BRAND_PBIGINT || brand == HCL_BRAND_NBIGINT);

#if (HCL_LIW_BITS == HCL_OOW_BITS)
	oop = hcl_allocwordobj(hcl, brand, ptr, len);
#elif (HCL_LIW_BITS == HCL_OOHW_BITS)
	oop = hcl_allochalfwordobj(hcl, brand, ptr, len);
#else
#	error UNSUPPORTED LIW BIT SIZE
#endif
	if (HCL_UNLIKELY(oop))
	{
		hcl_oop_class_t _class = (brand == HCL_BRAND_PBIGINT)?
			hcl->c_large_positive_integer: hcl->c_large_negative_integer;
		HCL_OBJ_SET_FLAGS_BRAND (oop, brand);
		HCL_OBJ_SET_CLASS (oop, (hcl_oop_t)_class);
	}
	return oop;
}

hcl_oop_t hcl_makecons (hcl_t* hcl, hcl_oop_t car, hcl_oop_t cdr)
{
/* TODO: use hcl_instantiate() */
	hcl_oop_cons_t cons;

	hcl_pushvolat (hcl, &car);
	hcl_pushvolat (hcl, &cdr);

	cons = (hcl_oop_cons_t)hcl_allocoopobj(hcl, HCL_BRAND_CONS, 2);
	if (HCL_LIKELY(cons))
	{
		cons->car = car;
		cons->cdr = cdr;
		HCL_OBJ_SET_CLASS (cons, (hcl_oop_t)hcl->c_cons);
	}

	hcl_popvolats (hcl, 2);

	return (hcl_oop_t)cons;
}

hcl_oop_t hcl_makearray (hcl_t* hcl, hcl_oow_t size, int ngc)
{
/* TODO: use hcl_instantiate() */
	hcl_oop_t v;
	v = hcl_allocoopobj(hcl, HCL_BRAND_ARRAY, size);
	if (HCL_LIKELY(v)) HCL_OBJ_SET_CLASS (v, (hcl_oop_t)hcl->c_array);
	return v;
}

hcl_oop_t hcl_makebytearray (hcl_t* hcl, const hcl_oob_t* ptr, hcl_oow_t size)
{
/* TODO: use hcl_instantiate() */
	hcl_oop_t v;
	v = hcl_allocbyteobj(hcl, HCL_BRAND_BYTE_ARRAY, ptr, size);
	if (HCL_LIKELY(v)) HCL_OBJ_SET_CLASS (v, (hcl_oop_t)hcl->c_byte_array);
	return v;
}

hcl_oop_t hcl_makebytestringwithbytes (hcl_t* hcl, const hcl_oob_t* ptr, hcl_oow_t len, int ngc)
{
	hcl_oop_byte_t b;
	hcl_oow_t i;
	hcl_oob_t v;

	b = (hcl_oop_byte_t)alloc_numeric_array(hcl, HCL_BRAND_BYTE_ARRAY, ptr, len, HCL_OBJ_TYPE_BYTE, HCL_SIZEOF(hcl_oob_t), 1, ngc);
	if (HCL_UNLIKELY(!b))
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to make bytestring - %js", orgmsg);
	}
	else
	{
		for (i = 0; i < len; i++)
		{
			v = ptr[i];
			HCL_OBJ_SET_BYTE_VAL(b, i, v);
		}

		HCL_OBJ_SET_CLASS (b, (hcl_oop_t)hcl->c_byte_array);
	}

	return (hcl_oop_t)b;
}

hcl_oop_t hcl_makebytestring (hcl_t* hcl, const hcl_ooch_t* ptr, hcl_oow_t len, int ngc)
{
	/* a byte string is a byte array with an extra null at the back.
	 * the input to this function, however, is the pointer to hcl_ooch_t data
	 * because this function is mainly used to convert a token to a byte string.
	 * the token in the compiler is stored as a hcl_ooch_t string. */

	hcl_oop_byte_t b;
	hcl_oow_t i;
	hcl_oob_t v;

	b = (hcl_oop_byte_t)alloc_numeric_array(hcl, HCL_BRAND_BYTE_ARRAY, HCL_NULL, len, HCL_OBJ_TYPE_BYTE, HCL_SIZEOF(hcl_oob_t), 1, ngc);
	if (HCL_UNLIKELY(!b))
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to make bytestring - %js", orgmsg);
	}
	else
	{
		for (i = 0; i < len; i++)
		{
			v = ptr[i] & 0xFF;
			HCL_OBJ_SET_BYTE_VAL(b, i, v);
		}

		HCL_OBJ_SET_CLASS (b, (hcl_oop_t)hcl->c_byte_array);
	}

	return (hcl_oop_t)b;
}

hcl_oop_t hcl_makestring (hcl_t* hcl, const hcl_ooch_t* ptr, hcl_oow_t len, int ngc)
{
	hcl_oop_char_t c;
	/*c = hcl_alloccharobj(hcl, HCL_BRAND_STRING, ptr, len);*/
	c = (hcl_oop_char_t)alloc_numeric_array(hcl, HCL_BRAND_STRING, ptr, len, HCL_OBJ_TYPE_CHAR, HCL_SIZEOF(hcl_ooch_t), 1, ngc);
	if (HCL_UNLIKELY(!c))
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to make string - %js", orgmsg);
	}
	else
	{
		HCL_OBJ_SET_CLASS (c, (hcl_oop_t)hcl->c_string);
	}
	return (hcl_oop_t)c;
}

hcl_oop_t hcl_makefpdec (hcl_t* hcl, hcl_oop_t value, hcl_ooi_t scale)
{
	hcl_oop_fpdec_t f;

	HCL_ASSERT (hcl, hcl_isint(hcl, value));

	if (scale <= 0) return value; /* if scale is 0 or less, return the value as it it */

	if (scale > HCL_SMOOI_MAX)
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "fpdec scale too large - %zd", scale);
		return HCL_NULL;
	}

	hcl_pushvolat (hcl, &value);
	f = (hcl_oop_fpdec_t)hcl_allocoopobj(hcl, HCL_BRAND_FPDEC, HCL_FPDEC_NAMED_INSTVARS);
	hcl_popvolat (hcl);

	if (HCL_UNLIKELY(!f))
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to make fpdec - %js", orgmsg);
	}
	else
	{
		f->value = value;
		f->scale = HCL_SMOOI_TO_OOP(scale);
		HCL_OBJ_SET_CLASS (f, (hcl_oop_t)hcl->c_fixed_point_decimal);
	}

	return (hcl_oop_t)f;
}

hcl_oop_t hcl_makeclass (hcl_t* hcl, hcl_oop_t superclass, hcl_ooi_t nivars, hcl_ooi_t ncvars, hcl_oop_t ivars_str, hcl_oop_t cvars_str)
{
	hcl_oop_class_t c;

	hcl_pushvolat (hcl, &superclass);
	hcl_pushvolat (hcl, &ivars_str);
	hcl_pushvolat (hcl, &cvars_str);
#if 0
	c = (hcl_oop_class_t)hcl_allocoopobj(hcl, HCL_BRAND_CLASS, HCL_CLASS_NAMED_INSTVARS + ncvars);
#else
	c = (hcl_oop_class_t)hcl_instantiate(hcl, hcl->c_class, HCL_NULL, ncvars);
#endif
	hcl_popvolats (hcl, 3);
	if (HCL_UNLIKELY(!c))
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to make class - %js", orgmsg);
		return HCL_NULL;
	}
	HCL_OBJ_SET_CLASS (c, (hcl_oop_t)hcl->c_class);

	c->spec = HCL_SMOOI_TO_OOP(0); /* TODO: fix this - encode nivars and nivars_super to spec??? */
	c->selfspec = HCL_SMOOI_TO_OOP(0); /* TODO: fix  this - encode ncvars to selfspec??? */
	c->superclass = superclass;
	c->nivars = HCL_SMOOI_TO_OOP(nivars);
	c->ncvars = HCL_SMOOI_TO_OOP(ncvars);
	c->ibrand = HCL_SMOOI_TO_OOP(HCL_BRAND_INSTANCE); /* TODO: really need ibrand??? */

	if ((hcl_oop_t)superclass != hcl->_nil)
	{
		hcl_ooi_t nivars_super;
		nivars_super = HCL_OOP_TO_SMOOI(((hcl_oop_class_t)superclass)->nivars_super) + HCL_OOP_TO_SMOOI(((hcl_oop_class_t)superclass)->nivars);
		c->nivars_super =  HCL_SMOOI_TO_OOP(nivars_super);
	}
	else
	{
		c->nivars_super = HCL_SMOOI_TO_OOP(0);
	}

	/* TODO: remember ivars_str and vars_str? */
	/* duplicate ivars_str and cvars_str and set it to c->ivarnames and c->cvarnames???? */

	return (hcl_oop_t)c;
}

#if 0
static HCL_INLINE int decode_spec (hcl_t* hcl, hcl_oop_class_t _class, hcl_obj_type_t* type, hcl_oow_t* outlen)
{
	/* TODO: */

	*type = HCL_OBJ_TYPE_OOP;
	*outlen = HCL_OOP_TO_SMOOI(_class->nivars_super) + HCL_OOP_TO_SMOOI(_class->nivars);
	return 0;
}
#else

static HCL_INLINE int decode_spec (hcl_t* hcl, hcl_oop_class_t _class, hcl_oow_t num_flexi_fields, hcl_obj_type_t* type, hcl_oow_t* outlen)
{
	hcl_oow_t spec;
	hcl_oow_t num_fixed_fields;
	hcl_obj_type_t indexed_type;

	HCL_ASSERT (hcl, HCL_OOP_IS_POINTER(_class));
	HCL_ASSERT (hcl, HCL_CLASSOF(hcl, _class) == (hcl_oop_t)hcl->c_class);

	HCL_ASSERT (hcl, HCL_OOP_IS_SMOOI(_class->spec));
	spec = HCL_OOP_TO_SMOOI(_class->spec);

	num_fixed_fields = HCL_CLASS_SPEC_NAMED_INSTVARS(spec);
	HCL_ASSERT (hcl, num_fixed_fields <= HCL_MAX_NAMED_INSTVARS);

	if (HCL_CLASS_SPEC_IS_INDEXED(spec))
	{
		indexed_type = (hcl_obj_type_t)HCL_CLASS_SPEC_INDEXED_TYPE(spec);

		/* the number of the fixed fields for a non-pointer object are supported.
		 * the fixed fields of a pointer object holds named instance variables
		 * and a non-pointer object is facilitated with the fixed fields of the size
		 * specified in the class description like #byte(5), #word(10).
		 *
		 * when it comes to spec decoding, there is no difference between a pointer
		 * object and a non-pointer object */

		if (num_flexi_fields > HCL_MAX_INDEXED_INSTVARS(num_fixed_fields))
		{
			hcl_seterrbfmt (hcl, HCL_EINVAL, "number of flexi-fields(%zu) too big for a class %O", num_flexi_fields, _class);
			return -1;
		}
	}
	else
	{
		/* named instance variables only. treat it as if it is an
		 * indexable class with no variable data */
		indexed_type = HCL_OBJ_TYPE_OOP;

		if (num_flexi_fields > 0)
		{
			hcl_seterrbfmt (hcl, HCL_EPERM, "flexi-fields(%zu) disallowed for a class %O", num_flexi_fields, _class);
			return -1;
		}
	}

	HCL_ASSERT (hcl, num_fixed_fields + num_flexi_fields <= HCL_OBJ_SIZE_MAX);
	*type = indexed_type;

	/* TODO: THIS PART IS WRONG.. nivars_super and nivars should be encoded to the spec.... */
	*outlen = num_fixed_fields + num_flexi_fields + HCL_OOP_TO_SMOOI(_class->nivars_super) + HCL_OOP_TO_SMOOI(_class->nivars);
	return 0;
}
#endif

hcl_oop_t hcl_instantiate (hcl_t* hcl, hcl_oop_class_t _class, const void* vptr, hcl_oow_t vlen)
{
	hcl_oop_t oop;
	hcl_obj_type_t type;
	hcl_oow_t alloclen;
	hcl_oow_t tmp_count = 0;

	HCL_ASSERT (hcl, hcl->_nil != HCL_NULL);

	if (decode_spec(hcl, _class, vlen, &type, &alloclen) <= -1) return HCL_NULL;

	hcl_pushvolat (hcl, (hcl_oop_t*)&_class); tmp_count++;

	switch (type)
	{
		case HCL_OBJ_TYPE_OOP:
			/* both the fixed part(named instance variables) and
			 * the variable part(indexed instance variables) are allowed. */
			oop = hcl_allocoopobj(hcl, HCL_BRAND_INSTANCE, alloclen);
			if (HCL_LIKELY(oop))
			{
		#if 0
				/* initialize named instance variables with default values */
				if (_class->initv[0] != hcl->_nil)
				{
					hcl_oow_t i = HCL_OBJ_GET_SIZE(_class->initv[0]);

					/* [NOTE] i don't deep-copy initial values.
					 *   if you change the contents of compound values like arrays,
					 *   it affects subsequent instantiation of the class.
					 *   it's important that the compiler should mark compound initial
					 *   values read-only. */
					while (i > 0)
					{
						--i;
						HCL_OBJ_SET_OOP_VAL (oop, i, HCL_OBJ_GET_OOP_VAL(_class->initv[0], i));
					}
				}
		#endif
			}
			HCL_ASSERT (hcl, vptr == HCL_NULL);
			/*
			This function is not GC-safe. so i don't want to initialize
			the payload of a pointer object. The caller can call this
			function and initialize payloads then.
			if (oop && vptr && vlen > 0)
			{
				hcl_oop_oop_t hdr = (hcl_oop_oop_t)oop;
				HCL_MEMCPY (&hdr->slot[named_ivar], vptr, vlen * HCL_SIZEOF(hcl_oop_t));
			}

			For the above code to work, it should protect the elements of
			the vptr array with hcl_pushvolat(). So it might be better
			to disallow a non-NULL vptr when indexed_type is OOP. See
			the assertion above this comment block.
			*/
			break;

		case HCL_OBJ_TYPE_CHAR:
			oop = hcl_alloccharobj(hcl, HCL_BRAND_INSTANCE, (const hcl_ooch_t*)vptr, alloclen);
			break;

		case HCL_OBJ_TYPE_BYTE:
			oop = hcl_allocbyteobj(hcl, HCL_BRAND_INSTANCE, (const hcl_oob_t*)vptr, alloclen);
			break;

		case HCL_OBJ_TYPE_HALFWORD:
			oop = hcl_allochalfwordobj(hcl, HCL_BRAND_INSTANCE, (const hcl_oohw_t*)vptr, alloclen);
			break;

		case HCL_OBJ_TYPE_WORD:
			oop = hcl_allocwordobj(hcl, HCL_BRAND_INSTANCE, (const hcl_oow_t*)vptr, alloclen);
			break;

		/* TODO: more types... HCL_OBJ_TYPE_INT... HCL_OBJ_TYPE_FLOAT, HCL_OBJ_TYPE_UINT16, etc*/
		default:
			hcl_seterrnum (hcl, HCL_EINTERN);
			oop = HCL_NULL;
			break;
	}

	if (HCL_LIKELY(oop))
	{
	#if 0
		hcl_ooi_t spec;
	#endif
		HCL_OBJ_SET_CLASS (oop, (hcl_oop_t)_class);
	#if 0 /* TODO: revive this part */
		spec = HCL_OOP_TO_SMOOI(_class->spec);
		if (HCL_CLASS_SPEC_IS_IMMUTABLE(spec)) HCL_OBJ_SET_FLAGS_RDONLY (oop, 1);
		if (HCL_CLASS_SPEC_IS_UNCOPYABLE(spec)) HCL_OBJ_SET_FLAGS_UNCOPYABLE (oop, 1);
	#endif
		HCL_OBJ_SET_FLAGS_BRAND(oop, HCL_OOP_TO_SMOOI(_class->ibrand));
	}
	hcl_popvolats (hcl, tmp_count);
	return oop;
}

hcl_oop_t hcl_instantiatewithtrailer (hcl_t* hcl, hcl_oop_class_t _class, hcl_oow_t vlen, const hcl_oob_t* trptr, hcl_oow_t trlen)
{
	hcl_oop_t oop;
	hcl_obj_type_t type;
	hcl_oow_t alloclen;
	hcl_oow_t tmp_count = 0;

	HCL_ASSERT (hcl, hcl->_nil != HCL_NULL);

	if (decode_spec(hcl, _class, vlen, &type, &alloclen) <= -1) return HCL_NULL;

	hcl_pushvolat (hcl, (hcl_oop_t*)&_class); tmp_count++;

	switch (type)
	{
		case HCL_OBJ_TYPE_OOP:
			oop = hcl_allocoopobjwithtrailer(hcl, HCL_BRAND_INSTANCE, alloclen, trptr, trlen);
			if (HCL_LIKELY(oop))
			{
				/* initialize named instance variables with default values */
			#if 0 /* TODO: revive this part */
				if (_class->initv[0] != hcl->_nil)
				{
					hcl_oow_t i = HCL_OBJ_GET_SIZE(_class->initv[0]);

					/* [NOTE] i don't deep-copy initial values.
					 *   if you change the contents of compound values like arrays,
					 *   it affects subsequent instantiation of the class.
					 *   it's important that the compiler should mark compound initial
					 *   values read-only. */
					while (i > 0)
					{
						--i;
						HCL_STORE_OOP (hcl, HCL_OBJ_GET_OOP_PTR(oop, i), HCL_OBJ_GET_OOP_VAL(_class->initv[0], i));
					}
				}
			#endif
			}

			break;

		default:
		#if 0
			HCL_DEBUG3 (hcl, "Not allowed to instantiate a non-pointer object of the %.*js class with trailer %zu\n",
				HCL_OBJ_GET_SIZE(_class->name),
				HCL_OBJ_GET_CHAR_SLOT(_class->name),
				trlen);
		#endif

			hcl_seterrnum (hcl, HCL_EPERM);
			oop = HCL_NULL;
			break;
	}

	if (oop)
	{
	#if 0
		hcl_ooi_t spec;
	#endif
		HCL_OBJ_SET_CLASS (oop, (hcl_oop_t)_class);
	#if 0 /* TODO: revive this part */
		spec = HCL_OOP_TO_SMOOI(_class->spec);
		if (HCL_CLASS_SPEC_IS_IMMUTABLE(spec)) HCL_OBJ_SET_FLAGS_RDONLY (oop, 1);
		/* the object with trailer is to to uncopyable in hcl_allocoopobjwithtrailer() so no need to check/set it again here
		if (HCL_CLASS_SPEC_IS_UNCOPYABLE(spec)) HCL_OBJ_SET_FLAGS_UNCOPYABLE (oop, 1);
		*/
	#endif
		HCL_OBJ_SET_FLAGS_BRAND(oop, HCL_OOP_TO_SMOOI(_class->ibrand));
	}
	hcl_popvolats (hcl, tmp_count);
	return oop;
}

/* ------------------------------------------------------------------------ *
 * NGC HANDLING
 * ------------------------------------------------------------------------ */

void hcl_freengcobj (hcl_t* hcl, hcl_oop_t obj)
{
	if (HCL_OOP_IS_POINTER(obj) && HCL_OBJ_GET_FLAGS_NGC(obj)) hcl_freemem (hcl, obj);
}

hcl_oop_t hcl_makengcbytearray (hcl_t* hcl, const hcl_oob_t* ptr, hcl_oow_t len)
{
	return alloc_numeric_array(hcl, HCL_BRAND_BYTE_ARRAY, ptr, len, HCL_OBJ_TYPE_BYTE, HCL_SIZEOF(hcl_oob_t), 0, 1);
}

hcl_oop_t hcl_remakengcbytearray (hcl_t* hcl, hcl_oop_t obj, hcl_oow_t newsize)
{
	hcl_oop_t tmp;

	HCL_ASSERT (hcl, !obj || (HCL_OOP_IS_POINTER(obj) && HCL_OBJ_GET_FLAGS_NGC(obj)));

	/* no hcl_pushvolat() is needed because 'obj' is a non-GC object. */
	/* TODO: improve this by using realloc */

	tmp = hcl_makengcbytearray(hcl, HCL_NULL, newsize);
	if (HCL_LIKELY(tmp))
	{
		if (obj)
		{
			hcl_oow_t cpsize;
			cpsize =  (newsize > HCL_OBJ_GET_SIZE(obj))? HCL_OBJ_GET_SIZE(obj): newsize;
			HCL_MEMCPY (((hcl_oop_byte_t)tmp)->slot, ((hcl_oop_byte_t)obj)->slot, cpsize * HCL_SIZEOF(hcl_oob_t));
		}
		hcl_freengcobj (hcl, obj);
	}
	return tmp;
}

hcl_oop_t hcl_makengcarray (hcl_t* hcl, hcl_oow_t len)
{
	return alloc_numeric_array(hcl, HCL_BRAND_ARRAY, HCL_NULL, len, HCL_OBJ_TYPE_OOP, HCL_SIZEOF(hcl_oop_t), 0, 1);
}

hcl_oop_t hcl_remakengcarray (hcl_t* hcl, hcl_oop_t obj, hcl_oow_t newsize)
{
	hcl_oop_t tmp;

	HCL_ASSERT (hcl, !obj || (HCL_OOP_IS_POINTER(obj) && HCL_OBJ_GET_FLAGS_NGC(obj)));

	/* no hcl_pushvolat() is needed because 'obj' is a non-GC object. */
	/* TODO: improve this by using realloc */

	tmp = hcl_makengcarray(hcl, newsize);
	if (HCL_LIKELY(tmp))
	{
		if (obj)
		{
			hcl_oow_t cpsize;
			cpsize =  (newsize > HCL_OBJ_GET_SIZE(obj))? HCL_OBJ_GET_SIZE(obj): newsize;
			HCL_MEMCPY (((hcl_oop_oop_t)tmp)->slot, ((hcl_oop_oop_t)obj)->slot, cpsize * HCL_SIZEOF(hcl_oop_t));
		}
		hcl_freengcobj (hcl, obj);
	}
	return tmp;
}

/* ------------------------------------------------------------------------ *
 * CONS
 * ------------------------------------------------------------------------ */
hcl_oow_t hcl_countcons (hcl_t* hcl, hcl_oop_t cons)
{
	/* this function ignores the last cdr */
	hcl_oow_t count = 1;

	HCL_ASSERT (hcl, HCL_IS_CONS(hcl, cons));
	do
	{
		cons = HCL_CONS_CDR(cons);
		if (!HCL_IS_CONS(hcl, cons)) break;
		count++;
	}
	while (1);

	return count;
}

hcl_oop_t hcl_getlastconscdr (hcl_t* hcl, hcl_oop_t cons)
{
	HCL_ASSERT (hcl, HCL_IS_CONS(hcl, cons));
	do
	{
		cons = HCL_CONS_CDR(cons);
		if (!HCL_IS_CONS(hcl, cons)) break;
	}
	while (1);

	return cons;
}

hcl_oop_t hcl_reversecons (hcl_t* hcl, hcl_oop_t cons)
{
	hcl_oop_t ptr, prev, next;

	/* Note: The non-nil cdr in the last cons cell gets lost.
	 *  e.g.) Reversing (1 2 3 . 4) results in (3 2 1) */

	HCL_ASSERT (hcl, HCL_IS_CONS(hcl, cons));

	prev = hcl->_nil;
	ptr = cons;

	do
	{
		next = HCL_CONS_CDR(ptr);
		HCL_CONS_CDR(ptr) = prev;
		prev = ptr;
		if (!HCL_IS_CONS(hcl, next)) break;
		ptr = next;
	}
	while (1);

	return ptr;
}


/* ------------------------------------------------------------------------ *
 * OBJECT HASHING
 * ------------------------------------------------------------------------ */
int hcl_hashobj (hcl_t* hcl, hcl_oop_t obj, hcl_oow_t* xhv)
{
	hcl_oow_t hv;

	if (obj == hcl->_nil)
	{
		*xhv = 0;
		return 0;
	}
	else if (obj == hcl->_true)
	{
		*xhv = 1;
		return 0;
	}
	else if (obj == hcl->_false)
	{
		*xhv = 2;
		return 0;
	}

	switch (HCL_OOP_GET_TAG(obj))
	{
		case HCL_OOP_TAG_SMOOI:
			hv = HCL_OOP_TO_SMOOI(obj);
			break;

/*
		case HCL_OOP_TAG_SMPTR:
			hv = (hcl_oow_t)HCL_OOP_TO_SMPTR(obj);
			break;
*/

		case HCL_OOP_TAG_CHAR:
			hv = HCL_OOP_TO_CHAR(obj);
			break;

/*
		case HCL_OOP_TAG_ERROR:
			hv = HCL_OOP_TO_ERROR(obj);
			break;
*/

		default:
		{
			int type;

			HCL_ASSERT (hcl, HCL_OOP_IS_POINTER(obj));
			type = HCL_OBJ_GET_FLAGS_TYPE(obj);
			switch (type)
			{
				case HCL_OBJ_TYPE_BYTE:
					hv = hcl_hash_bytes(((hcl_oop_byte_t)obj)->slot, HCL_OBJ_GET_SIZE(obj));
					break;

				case HCL_OBJ_TYPE_CHAR:
					hv = hcl_hash_oochars(((hcl_oop_char_t)obj)->slot, HCL_OBJ_GET_SIZE(obj));
					break;

				case HCL_OBJ_TYPE_HALFWORD:
					hv = hcl_hash_halfwords(((hcl_oop_halfword_t)obj)->slot, HCL_OBJ_GET_SIZE(obj));
					break;

				case HCL_OBJ_TYPE_WORD:
					hv = hcl_hash_words(((hcl_oop_word_t)obj)->slot, HCL_OBJ_GET_SIZE(obj));
					break;

				default:
					/* HCL_OBJ_TYPE_OOP, ... */
					hcl_seterrbfmt(hcl, HCL_ENOIMPL, "no builtin hash implemented for %O", obj); /* TODO: better error code? */
					return -1;
			}
			break;
		}
	}

	/* i assume that hcl_hashxxx() functions limits the return value to fall
	 * between 0 and HCL_SMOOI_MAX inclusive */
	HCL_ASSERT (hcl, hv >= 0 && hv <= HCL_SMOOI_MAX);
	*xhv = hv;
	return 0;
}

/* ------------------------------------------------------------------------ *
 * OBJECT EQUALITY
 * ------------------------------------------------------------------------ */
int hcl_equalobjs (hcl_t* hcl, hcl_oop_t rcv, hcl_oop_t arg)
{
	int rtag;

	if (rcv == arg) return 1; /* identical. so equal */

	rtag = HCL_OOP_GET_TAG(rcv);
	if (rtag != HCL_OOP_GET_TAG(arg)) return 0;

	switch (rtag)
	{
		case HCL_OOP_TAG_SMOOI:
			return HCL_OOP_TO_SMOOI(rcv) == HCL_OOP_TO_SMOOI(arg)? 1: 0;

		case HCL_OOP_TAG_SMPTR:
			return HCL_OOP_TO_SMPTR(rcv) == HCL_OOP_TO_SMPTR(arg)? 1: 0;

		case HCL_OOP_TAG_CHAR:
			return HCL_OOP_TO_CHAR(rcv) == HCL_OOP_TO_CHAR(arg)? 1: 0;

		case HCL_OOP_TAG_ERROR:
			return HCL_OOP_TO_ERROR(rcv) == HCL_OOP_TO_ERROR(arg)? 1: 0;

		default:
		{
			HCL_ASSERT (hcl, HCL_OOP_IS_POINTER(rcv));

			if (HCL_OBJ_GET_FLAGS_BRAND(rcv) != HCL_OBJ_GET_FLAGS_BRAND(arg)) return 0; /* different class, not equal */
			HCL_ASSERT (hcl, HCL_OBJ_GET_FLAGS_TYPE(rcv) == HCL_OBJ_GET_FLAGS_TYPE(arg));

			if (HCL_OBJ_GET_SIZE(rcv) != HCL_OBJ_GET_SIZE(arg)) return 0; /* different size, not equal */

			switch (HCL_OBJ_GET_FLAGS_TYPE(rcv))
			{
				case HCL_OBJ_TYPE_BYTE:
				case HCL_OBJ_TYPE_CHAR:
				case HCL_OBJ_TYPE_HALFWORD:
				case HCL_OBJ_TYPE_WORD:
					return (HCL_MEMCMP(HCL_OBJ_GET_BYTE_SLOT(rcv), HCL_OBJ_GET_BYTE_SLOT(arg), HCL_BYTESOF(hcl,rcv)) == 0)? 1: 0;

				default:
				{
					hcl_oow_t i, size;

					if (rcv == hcl->_nil) return arg == hcl->_nil? 1: 0;
					if (rcv == hcl->_true) return arg == hcl->_true? 1: 0;
					if (rcv == hcl->_false) return arg == hcl->_false? 1: 0;

					/* HCL_OBJ_TYPE_OOP, ... */
					HCL_ASSERT (hcl, HCL_OBJ_GET_FLAGS_TYPE(rcv) == HCL_OBJ_TYPE_OOP);

				#if 0
					hcl_seterrbfmt (hcl, HCL_ENOIMPL, "no builtin comparison implemented for %O and %O", rcv, arg); /* TODO: better error code */
					return -1;
				#else

					if (HCL_IS_PROCESS(hcl,rcv))
					{
						/* the stack in a process object doesn't need to be
						 * scanned in full. the slots above the stack pointer
						 * are garbages. */
						size = HCL_PROCESS_NAMED_INSTVARS +
							  HCL_OOP_TO_SMOOI(((hcl_oop_process_t)rcv)->sp) + 1;
						HCL_ASSERT (hcl, size <= HCL_OBJ_GET_SIZE(rcv));
					}
					else
					{
						size = HCL_OBJ_GET_SIZE(rcv);
					}
					for (i = 0; i < size; i++)
					{
						int n;
						/* TODO: remove recursion */
						/* NOTE: even if the object implements the equality method,
						 * this primitive method doesn't honor it. */
						n = hcl_equalobjs(hcl, ((hcl_oop_oop_t)rcv)->slot[i], ((hcl_oop_oop_t)arg)->slot[i]);
						if (n <= 0) return n;
					}

					/* the default implementation doesn't take the trailer space into account */
					return 1;
				#endif
				}
			}
		}
	}
}
