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

#if defined(HAK_PROFILE_VM)
#include <sys/time.h>
#include <sys/resource.h> /* getrusage */
#endif

void* hak_allocbytes (hak_t* hak, hak_oow_t size)
{
	hak_gchdr_t* gch;
	hak_oow_t allocsize;
	int gc_called = 0;
#if defined(HAK_PROFILE_VM)
	struct rusage ru;
	hak_ntime_t rut;
#endif

#if defined(HAK_BUILD_DEBUG)
	if ((hak->option.trait & HAK_TRAIT_DEBUG_GC) && !(hak->option.trait & HAK_TRAIT_NOGC)) hak_gc (hak, 1);
#endif

#if defined(HAK_PROFILE_VM)
	getrusage(RUSAGE_SELF, &ru);
	HAK_INIT_NTIME (&rut,  ru.ru_utime.tv_sec, HAK_USEC_TO_NSEC(ru.ru_utime.tv_usec));
#endif

	allocsize = HAK_SIZEOF(*gch) + size;

	if (hak->gci.bsz >= hak->gci.threshold)
	{
		hak_gc (hak, 0);
		hak->gci.threshold = hak->gci.bsz + 100000; /* TODO: change this fomula */
		gc_called = 1;
	}

	if (hak->gci.lazy_sweep) hak_gc_ms_sweep_lazy (hak, allocsize);

	gch = (hak_gchdr_t*)hak_callocheapmem_noseterr(hak, hak->heap, allocsize);
	if (!gch)
	{
		if (HAK_UNLIKELY(hak->option.trait & HAK_TRAIT_NOGC)) goto calloc_heapmem_fail;
		if (gc_called) goto sweep_the_rest;

		hak_gc (hak, 0);
		if (hak->gci.lazy_sweep) hak_gc_ms_sweep_lazy (hak, allocsize);

		gch = (hak_gchdr_t*)hak_callocheapmem_noseterr(hak, hak->heap, allocsize);
		if (HAK_UNLIKELY(!gch))
		{
		sweep_the_rest:
			if (hak->gci.lazy_sweep)
			{
				hak_gc_ms_sweep_lazy (hak, HAK_TYPE_MAX(hak_oow_t)); /* sweep the rest */
				gch = (hak_gchdr_t*)hak_callocheapmem(hak, hak->heap, allocsize);
				if (HAK_UNLIKELY(!gch)) return HAK_NULL;
			}
			else
			{
			calloc_heapmem_fail:
				hak_seterrnum (hak, HAK_EOOMEM);
				return HAK_NULL;
			}
		}
	}

	if (hak->gci.lazy_sweep && hak->gci.ls.curr == hak->gci.b)
	{
		/* if the lazy sweeping point is at the beginning of the allocation block,
		 * hak->gc.ls.prev must get updated */
		HAK_ASSERT (hak, hak->gci.ls.prev == HAK_NULL);
		hak->gci.ls.prev = gch;
	}

	gch->next = hak->gci.b;
	hak->gci.b = gch;
	hak->gci.bsz += size;


#if defined(HAK_PROFILE_VM)
	getrusage(RUSAGE_SELF, &ru);
	HAK_SUB_NTIME_SNS (&rut, &rut, ru.ru_utime.tv_sec, HAK_USEC_TO_NSEC(ru.ru_utime.tv_usec));
	HAK_SUB_NTIME (&hak->gci.stat.alloc, &hak->gci.stat.alloc, &rut); /* do subtraction because rut is negative */
#endif
	return (hak_uint8_t*)(gch + 1);

}

static HAK_INLINE hak_oop_t alloc_oop_array (hak_t* hak, hak_oow_t size, int ngc)
{
	hak_oop_oop_t hdr;
	hak_oow_t nbytes, nbytes_aligned;

	nbytes = size * HAK_SIZEOF(hak_oop_t);

	/* this isn't really necessary since nbytes must be
	 * aligned already. */
	nbytes_aligned = HAK_ALIGN(nbytes, HAK_SIZEOF(hak_oop_t));

	if (HAK_UNLIKELY(ngc))
	{
		hdr = (hak_oop_oop_t)hak_callocmem(hak, HAK_SIZEOF(hak_obj_t) + nbytes_aligned);
	}
	else
	{
		/* making the number of bytes to allocate a multiple of
		 * HAK_SIZEOF(hak_oop_t) will guarantee the starting address
		 * of the allocated space to be an even number.
		 * see HAK_OOP_IS_NUMERIC() and HAK_OOP_IS_POINTER() */
		hdr = (hak_oop_oop_t)hak_allocbytes(hak, HAK_SIZEOF(hak_obj_t) + nbytes_aligned);
	}
	if (!hdr) return HAK_NULL;

	hdr->_flags = HAK_OBJ_MAKE_FLAGS(HAK_OBJ_TYPE_OOP, HAK_SIZEOF(hak_oop_t), 0, 0, 0, ngc, 0);
	HAK_OBJ_SET_SIZE (hdr, size);
	/*HAK_OBJ_SET_CLASS (hdr, hak->_nil);*/

	while (size > 0) hdr->slot[--size] = hak->_nil;

	return (hak_oop_t)hdr;
}


hak_oop_t hak_allocoopobj (hak_t* hak, hak_oow_t size)
{
	return alloc_oop_array(hak, size, 0);
}

hak_oop_t hak_allocoopobjwithtrailer (hak_t* hak, hak_oow_t size, const hak_oob_t* bptr, hak_oow_t blen)
{
	hak_oop_oop_t hdr;
	hak_oow_t nbytes, nbytes_aligned;
	hak_oow_t i;

	/* +1 for the trailer size of the hak_oow_t type */
	nbytes = (size + 1) * HAK_SIZEOF(hak_oop_t) + blen;
	nbytes_aligned = HAK_ALIGN(nbytes, HAK_SIZEOF(hak_oop_t));

	hdr = (hak_oop_oop_t)hak_allocbytes(hak, HAK_SIZEOF(hak_obj_t) + nbytes_aligned);
	if (HAK_UNLIKELY(!hdr)) return HAK_NULL;

	hdr->_flags = HAK_OBJ_MAKE_FLAGS(HAK_OBJ_TYPE_OOP, HAK_SIZEOF(hak_oop_t), 0, 0, 0, 0, 1);
	HAK_OBJ_SET_SIZE (hdr, size);
	/*HAK_OBJ_SET_CLASS (hdr, hak->_nil);*/

	for (i = 0; i < size; i++) hdr->slot[i] = hak->_nil;

	/* [NOTE] this is not converted to a SMOOI object */
	hdr->slot[size] = (hak_oop_t)blen;

	if (bptr) HAK_MEMCPY (&hdr->slot[size + 1], bptr, blen);
	else HAK_MEMSET (&hdr->slot[size + 1], 0, blen);

	return (hak_oop_t)hdr;
}

static HAK_INLINE hak_oop_t alloc_numeric_array (hak_t* hak, const void* ptr, hak_oow_t len, hak_obj_type_t type, hak_oow_t unit, int extra, int ngc)
{
	/* allocate a variable object */

	hak_oop_t hdr;
	hak_oow_t xbytes, nbytes, nbytes_aligned;

	xbytes = len * unit;
	/* 'extra' indicates an extra unit to append at the end.
	 * it's useful to store a string with a terminating null */
	nbytes = extra? xbytes + unit: xbytes;
	nbytes_aligned = HAK_ALIGN(nbytes, HAK_SIZEOF(hak_oop_t));
/* TODO: check overflow in size calculation*/

	/* making the number of bytes to allocate a multiple of
	 * HAK_SIZEOF(hak_oop_t) will guarantee the starting address
	 * of the allocated space to be an even number.
	 * see HAK_OOP_IS_NUMERIC() and HAK_OOP_IS_POINTER() */
	if (HAK_UNLIKELY(ngc))
		hdr = (hak_oop_t)hak_callocmem(hak, HAK_SIZEOF(hak_obj_t) + nbytes_aligned);
	else
		hdr = (hak_oop_t)hak_allocbytes(hak, HAK_SIZEOF(hak_obj_t) + nbytes_aligned);
	if (HAK_UNLIKELY(!hdr)) return HAK_NULL;

	hdr->_flags = HAK_OBJ_MAKE_FLAGS(type, unit, extra, 0, 0, ngc, 0);
	hdr->_size = len;
	HAK_OBJ_SET_SIZE (hdr, len);
	/*HAK_OBJ_SET_CLASS (hdr, hak->_nil);*/

	if (ptr)
	{
		/* copy data */
		HAK_MEMCPY (hdr + 1, ptr, xbytes);
		HAK_MEMSET ((hak_uint8_t*)(hdr + 1) + xbytes, 0, nbytes_aligned - xbytes);
	}
	else
	{
		/* initialize with zeros when the string pointer is not given */
		HAK_MEMSET (hdr + 1, 0, nbytes_aligned);
	}

	return hdr;
}

hak_oop_t hak_alloccharobj (hak_t* hak, const hak_ooch_t* ptr, hak_oow_t len)
{
	return alloc_numeric_array(hak, ptr, len, HAK_OBJ_TYPE_CHAR, HAK_SIZEOF(hak_ooch_t), 1, 0);
}

hak_oop_t hak_allocbyteobj (hak_t* hak, const hak_oob_t* ptr, hak_oow_t len)
{
	return alloc_numeric_array(hak, ptr, len, HAK_OBJ_TYPE_BYTE, HAK_SIZEOF(hak_oob_t), 0, 0);
}

hak_oop_t hak_allochalfwordobj (hak_t* hak, const hak_oohw_t* ptr, hak_oow_t len)
{
	return alloc_numeric_array(hak, ptr, len, HAK_OBJ_TYPE_HALFWORD, HAK_SIZEOF(hak_oohw_t), 0, 0);
}

hak_oop_t hak_allocwordobj (hak_t* hak, const hak_oow_t* ptr, hak_oow_t len)
{
	return alloc_numeric_array(hak, ptr, len, HAK_OBJ_TYPE_WORD, HAK_SIZEOF(hak_oow_t), 0, 0);
}

/* ------------------------------------------------------------------------ *
 * COMMON OBJECTS
 * ------------------------------------------------------------------------ */

hak_oop_t hak_hatchundef (hak_t* hak)
{
	/* create the undef object for bootstrapping.
	 * this function doesn't set the class field */

	hak_oop_t v;
	v = hak_allocoopobj(hak, 0);
	if (HAK_UNLIKELY(!v))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt (hak, HAK_ERRNUM(hak), "unable to make undef - %js", orgmsg);
	}
	else
	{
		HAK_OBJ_SET_FLAGS_KERNEL(v, 1);
	}
	return v;
}

hak_oop_t hak_hatchnil (hak_t* hak)
{
	/* create the nil object for bootstrapping.
	 * this function doesn't set the class field */

	hak_oop_t v;
	v = hak_allocoopobj(hak, 0);
	if (HAK_UNLIKELY(!v))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt (hak, HAK_ERRNUM(hak), "unable to make nil - %js", orgmsg);
	}
	else
	{
		HAK_OBJ_SET_FLAGS_KERNEL(v, 1);
	}
	return v;
}

hak_oop_t hak_makecons (hak_t* hak, hak_oop_t car, hak_oop_t cdr)
{
/* TODO: use hak_instantiate() */
#if 0
	hak_oop_cons_t cons;

	hak_pushvolat (hak, &car);
	hak_pushvolat (hak, &cdr);

	cons = (hak_oop_cons_t)hak_allocoopobj(hak, HAK_BRAND_CONS, 2);
	if (HAK_LIKELY(cons))
	{
		cons->car = car;
		cons->cdr = cdr;
		HAK_OBJ_SET_CLASS (cons, (hak_oop_t)hak->c_cons);
	}

	hak_popvolats (hak, 2);

	return (hak_oop_t)cons;
#else
	hak_oop_cons_t v;
	hak_pushvolat (hak, &car);
	hak_pushvolat (hak, &cdr);
	v = (hak_oop_cons_t)hak_instantiate(hak, hak->c_cons, HAK_NULL, 0);
	hak_popvolats (hak, 2);
	if (HAK_UNLIKELY(!v))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt (hak, HAK_ERRNUM(hak), "unable to instantiate %O - %js", hak->c_cons->name, orgmsg);
	}
	else
	{
		v->car = car;
		v->cdr = cdr;
	}
	return (hak_oop_t)v;
#endif
}

hak_oop_t hak_makearray (hak_t* hak, hak_oow_t len)
{
#if 0
	hak_oop_t v;
	v = hak_allocoopobj(hak, HAK_BRAND_ARRAY, size);
	if (HAK_LIKELY(v)) HAK_OBJ_SET_CLASS (v, (hak_oop_t)hak->c_array);
	return v;
#else
	hak_oop_t v;
	v = hak_instantiate(hak, hak->c_array, HAK_NULL, len);
	if (HAK_UNLIKELY(!v))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt (hak, HAK_ERRNUM(hak),
			"unable to instantiate %O - %js", hak->c_array->name, orgmsg);
	}
	return v;
#endif
}

hak_oop_t hak_makechararray (hak_t* hak, const hak_ooch_t* ptr, hak_oow_t len)
{
	hak_oop_t v;
	v = hak_instantiate(hak, hak->c_character_array, ptr, len);
	if (HAK_UNLIKELY(!v))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt (hak, HAK_ERRNUM(hak),
			"unable to instantiate %O - %js", hak->c_character_array->name, orgmsg);
	}
	return v;
}

hak_oop_t hak_makebytearray (hak_t* hak, const hak_oob_t* ptr, hak_oow_t len)
{
#if 0
	hak_oop_t v;
	v = hak_allocbyteobj(hak, HAK_BRAND_BYTE_ARRAY, ptr, size);
	if (HAK_LIKELY(v)) HAK_OBJ_SET_CLASS (v, (hak_oop_t)hak->c_byte_array);
	return v;
#else
	hak_oop_t v;
	v = hak_instantiate(hak, hak->c_byte_array, ptr, len);
	if (HAK_UNLIKELY(!v))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt (hak, HAK_ERRNUM(hak),
			"unable to instantiate %O - %js", hak->c_byte_array->name, orgmsg);
	}
	return v;
#endif
}

hak_oop_t hak_makebytestringwithbytes (hak_t* hak, const hak_oob_t* ptr, hak_oow_t len)
{
	hak_oop_byte_t v;
	v = (hak_oop_byte_t)hak_instantiate(hak, hak->c_byte_string, ptr, len);
	if (HAK_UNLIKELY(!v))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt (hak, HAK_ERRNUM(hak),
			"unable to instantiate %O with bytes - %js", hak->c_byte_string->name, orgmsg);
	}
	return (hak_oop_t)v;
}

hak_oop_t hak_makebytestring (hak_t* hak, const hak_ooch_t* ptr, hak_oow_t len)
{
	/* a byte string is a byte array with an extra null at the back.
	 * the input to this function, however, is the pointer to hak_ooch_t data
	 * because this function is mainly used to convert a token to a byte string.
	 * the token in the compiler is stored as a hak_ooch_t string. */

	hak_oop_byte_t v;

	v = (hak_oop_byte_t)hak_instantiate(hak, hak->c_byte_string, HAK_NULL, len);
	if (HAK_UNLIKELY(!v))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt (hak, HAK_ERRNUM(hak),
			"unable to instantiate %O - %js", hak->c_byte_string->name, orgmsg);
	}
	else
	{
		hak_oow_t i;
		hak_oob_t b;
		for (i = 0; i < len; i++)
		{
			b = ptr[i] & 0xFF;
			HAK_OBJ_SET_BYTE_VAL(v, i, b);
		}
	}

	return (hak_oop_t)v;
}

hak_oop_t hak_makestring (hak_t* hak, const hak_ooch_t* ptr, hak_oow_t len)
{
	hak_oop_t v;
	v = hak_instantiate(hak, hak->c_string, ptr, len);
	if (HAK_UNLIKELY(!v))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt (hak, HAK_ERRNUM(hak),
			"unable to instantiate %O - %js", hak->c_string->name, orgmsg);
	}
	return v;
}

hak_oop_t hak_makefpdec (hak_t* hak, hak_oop_t value, hak_ooi_t scale)
{
	hak_oop_fpdec_t f;

	HAK_ASSERT (hak, hak_isint(hak, value));

	if (scale <= 0) return value; /* if scale is 0 or less, return the value as it it */

	if (scale > HAK_SMOOI_MAX)
	{
		hak_seterrbfmt (hak, HAK_EINVAL, "fpdec scale too large - %zd", scale);
		return HAK_NULL;
	}

	hak_pushvolat (hak, &value);
	f = (hak_oop_fpdec_t)hak_instantiate(hak, hak->c_fixed_point_decimal, HAK_NULL, 0);
	hak_popvolat (hak);

	if (HAK_UNLIKELY(!f))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt (
			hak, HAK_ERRNUM(hak), "unable to instantiate %O - %js",
			hak->c_fixed_point_decimal->name, orgmsg);
	}
	else
	{
		f->value = value;
		f->scale = HAK_SMOOI_TO_OOP(scale);
	}

	return (hak_oop_t)f;
}

hak_oop_t hak_makeclass (hak_t* hak, hak_oop_t class_name, hak_oop_t superclass, hak_ooi_t spec, hak_ooi_t selfspec, hak_oop_t ivars_str, hak_oop_t cvars_str)
{
	hak_oop_class_t c;

	hak_pushvolat (hak, &class_name);
	hak_pushvolat (hak, &superclass);
	hak_pushvolat (hak, &ivars_str);
	hak_pushvolat (hak, &cvars_str);
	c = (hak_oop_class_t)hak_instantiate(hak, hak->c_class, HAK_NULL, HAK_CLASS_SELFSPEC_CLASSVARS(selfspec));
	hak_popvolats (hak, 4);
	if (HAK_UNLIKELY(!c))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt (hak, HAK_ERRNUM(hak),
			"unable to instantiate class %O - %js", class_name, orgmsg);
	}
	else
	{
		hak_ooi_t nivars_super;

		if (!HAK_IS_NIL(hak, superclass))
		{
			hak_ooi_t superspec;
			superspec = HAK_OOP_TO_SMOOI(((hak_oop_class_t)superclass)->spec);
			nivars_super = HAK_OOP_TO_SMOOI(((hak_oop_class_t)superclass)->nivars_super) + HAK_CLASS_SPEC_NAMED_INSTVARS(superspec);
		}
		else
		{
			nivars_super = 0;
		}

		c->spec = HAK_SMOOI_TO_OOP(spec);
		c->selfspec = HAK_SMOOI_TO_OOP(selfspec);
		c->name = class_name;
		c->superclass = superclass;
		c->nivars_super = HAK_SMOOI_TO_OOP(nivars_super);
		c->ibrand = HAK_SMOOI_TO_OOP(HAK_BRAND_INSTANCE); /* TODO: really need ibrand??? */

		/* TODO: remember ivars_str and vars_str? */
		/* duplicate ivars_str and cvars_str and set it to c->ivarnames and c->cvarnames???? */
	}

	return (hak_oop_t)c;
}

struct decoded_spec_t
{
	hak_obj_type_t type;
	hak_oow_t alloclen;
	int flexi;
};
typedef struct decoded_spec_t decoded_spec_t;

static HAK_INLINE int decode_spec (hak_t* hak, hak_oop_class_t _class, hak_oow_t num_flexi_fields, decoded_spec_t* dspec)
{
	hak_oow_t spec;
	hak_oow_t num_fixed_fields;
	hak_obj_type_t indexed_type;

	HAK_ASSERT (hak, HAK_OOP_IS_POINTER(_class));
	HAK_ASSERT (hak, HAK_CLASSOF(hak, _class) == (hak_oop_t)hak->c_class);

	HAK_ASSERT (hak, HAK_OOP_IS_SMOOI(_class->spec));
	spec = HAK_OOP_TO_SMOOI(_class->spec);

	num_fixed_fields = HAK_CLASS_SPEC_NAMED_INSTVARS(spec);
	HAK_ASSERT (hak, num_fixed_fields <= HAK_MAX_NAMED_INSTVARS);

	if (HAK_CLASS_SPEC_IS_INDEXED(spec))
	{
		indexed_type = (hak_obj_type_t)HAK_CLASS_SPEC_INDEXED_TYPE(spec);

		/* the number of the fixed fields for a non-pointer object are supported.
		 * the fixed fields of a pointer object holds named instance variables
		 * and a non-pointer object is facilitated with the fixed fields of the size
		 * specified in the class description like #byte(5), #word(10).
		 *
		 * when it comes to spec decoding, there is no difference between a pointer
		 * object and a non-pointer object */

		if (num_flexi_fields > HAK_MAX_INDEXED_INSTVARS(num_fixed_fields))
		{
			hak_seterrbfmt (hak, HAK_EINVAL, "number of flexi-fields(%zu) too big for a class %O", num_flexi_fields, _class);
			return -1;
		}
	}
	else
	{
		/* named instance variables only. treat it as if it is an
		 * indexable class with no variable data */

		/* for an object composed of non-oop fields,
		 * the field can be accessed using a instance variable name.
		 * the instructions for instance variable access must cater for this.
		 * for example, the Primitive class is HAK_OBJ_TYPE_WORD and not variable */
		/*indexed_type = HAK_OBJ_TYPE_OOP; <- no more fixed to OOP fields only. */
		indexed_type = (hak_obj_type_t)HAK_CLASS_SPEC_INDEXED_TYPE(spec);

		if (num_flexi_fields > 0)
		{
			hak_seterrbfmt (hak, HAK_EPERM, "flexi-fields(%zu) disallowed for a class %O", num_flexi_fields, _class);
			return -1;
		}
	}

	HAK_ASSERT (hak, num_fixed_fields + num_flexi_fields <= HAK_OBJ_SIZE_MAX);
	dspec->flexi = !!HAK_CLASS_SPEC_IS_INDEXED(spec);
	dspec->type = indexed_type;
	dspec->alloclen = num_fixed_fields + num_flexi_fields + HAK_OOP_TO_SMOOI(_class->nivars_super);
	return 0;
}

hak_oop_t hak_instantiate (hak_t* hak, hak_oop_class_t _class, const void* vptr, hak_oow_t vlen)
{
	hak_oop_t oop;
	decoded_spec_t dspec;
	hak_oow_t tmp_count = 0;

	HAK_ASSERT (hak, hak->_nil != HAK_NULL);

	if (decode_spec(hak, _class, vlen, &dspec) <= -1) return HAK_NULL;

	hak_pushvolat (hak, (hak_oop_t*)&_class); tmp_count++;

	switch (dspec.type)
	{
		case HAK_OBJ_TYPE_OOP:
			/* both the fixed part(named instance variables) and
			 * the variable part(indexed instance variables) are allowed. */
			oop = hak_allocoopobj(hak, dspec.alloclen);
			if (HAK_LIKELY(oop))
			{
		#if 0
				/* initialize named instance variables with default values */
				if (_class->initv[0] != hak->_nil)
				{
					hak_oow_t i = HAK_OBJ_GET_SIZE(_class->initv[0]);

					/* [NOTE] i don't deep-copy initial values.
					 *   if you change the contents of compound values like arrays,
					 *   it affects subsequent instantiation of the class.
					 *   it's important that the compiler should mark compound initial
					 *   values read-only. */
					while (i > 0)
					{
						--i;
						HAK_OBJ_SET_OOP_VAL (oop, i, HAK_OBJ_GET_OOP_VAL(_class->initv[0], i));
					}
				}
		#endif
			}
			HAK_ASSERT (hak, vptr == HAK_NULL);
			/*
			This function is not GC-safe. so i don't want to initialize
			the payload of a pointer object. The caller can call this
			function and initialize payloads then.
			if (oop && vptr && vlen > 0)
			{
				hak_oop_oop_t hdr = (hak_oop_oop_t)oop;
				HAK_MEMCPY (&hdr->slot[named_ivar], vptr, vlen * HAK_SIZEOF(hak_oop_t));
			}

			For the above code to work, it should protect the elements of
			the vptr array with hak_pushvolat(). So it might be better
			to disallow a non-NULL vptr when indexed_type is OOP. See
			the assertion above this comment block.
			*/
			break;

		case HAK_OBJ_TYPE_CHAR:
			oop = hak_alloccharobj(hak, (const hak_ooch_t*)vptr, dspec.alloclen);
			break;

		case HAK_OBJ_TYPE_BYTE:
			oop = hak_allocbyteobj(hak, (const hak_oob_t*)vptr, dspec.alloclen);
			break;

		case HAK_OBJ_TYPE_HALFWORD:
			oop = hak_allochalfwordobj(hak, (const hak_oohw_t*)vptr, dspec.alloclen);
			break;

		case HAK_OBJ_TYPE_WORD:
			oop = hak_allocwordobj(hak, (const hak_oow_t*)vptr, dspec.alloclen);
			break;

		/* TODO: more types... HAK_OBJ_TYPE_INT... HAK_OBJ_TYPE_FLOAT, HAK_OBJ_TYPE_UINT16, etc*/
		default:
			hak_seterrnum (hak, HAK_EINTERN);
			oop = HAK_NULL;
			break;
	}

	if (HAK_LIKELY(oop))
	{
		hak_ooi_t spec;
		HAK_OBJ_SET_CLASS (oop, (hak_oop_t)_class);
		spec = HAK_OOP_TO_SMOOI(_class->spec);
		if (HAK_CLASS_SPEC_IS_IMMUTABLE(spec)) HAK_OBJ_SET_FLAGS_RDONLY (oop, 1);
	#if 0 /* TODO: revive this part */
		if (HAK_CLASS_SPEC_IS_UNCOPYABLE(spec)) HAK_OBJ_SET_FLAGS_UNCOPYABLE (oop, 1);
	#endif
		HAK_OBJ_SET_FLAGS_FLEXI(oop, dspec.flexi);
	}
	hak_popvolats (hak, tmp_count);
	return oop;
}

hak_oop_t hak_instantiatewithtrailer (hak_t* hak, hak_oop_class_t _class, hak_oow_t vlen, const hak_oob_t* trptr, hak_oow_t trlen)
{
	hak_oop_t oop;
	decoded_spec_t dspec;
	hak_oow_t tmp_count = 0;

	HAK_ASSERT (hak, hak->_nil != HAK_NULL);

	if (decode_spec(hak, _class, vlen, &dspec) <= -1) return HAK_NULL;

	hak_pushvolat (hak, (hak_oop_t*)&_class); tmp_count++;

	switch (dspec.type)
	{
		case HAK_OBJ_TYPE_OOP:
			oop = hak_allocoopobjwithtrailer(hak, dspec.alloclen, trptr, trlen);
			if (HAK_LIKELY(oop))
			{
				/* initialize named instance variables with default values */
			#if 0 /* TODO: revive this part */
				if (_class->initv[0] != hak->_nil)
				{
					hak_oow_t i = HAK_OBJ_GET_SIZE(_class->initv[0]);

					/* [NOTE] i don't deep-copy initial values.
					 *   if you change the contents of compound values like arrays,
					 *   it affects subsequent instantiation of the class.
					 *   it's important that the compiler should mark compound initial
					 *   values read-only. */
					while (i > 0)
					{
						--i;
						HAK_STORE_OOP (hak, HAK_OBJ_GET_OOP_PTR(oop, i), HAK_OBJ_GET_OOP_VAL(_class->initv[0], i));
					}
				}
			#endif
			}

			break;

		default:
		#if 0
			HAK_DEBUG3 (hak, "Not allowed to instantiate a non-pointer object of the %.*js class with trailer %zu\n",
				HAK_OBJ_GET_SIZE(_class->name),
				HAK_OBJ_GET_CHAR_SLOT(_class->name),
				trlen);
		#endif

			hak_seterrnum (hak, HAK_EPERM);
			oop = HAK_NULL;
			break;
	}

	if (HAK_LIKELY(oop))
	{
		hak_ooi_t spec;
		HAK_OBJ_SET_CLASS (oop, (hak_oop_t)_class);
		spec = HAK_OOP_TO_SMOOI(_class->spec);
		if (HAK_CLASS_SPEC_IS_IMMUTABLE(spec)) HAK_OBJ_SET_FLAGS_RDONLY (oop, 1);
	#if 0 /* TODO: revive this part */
		/* the object with trailer is to to uncopyable in hak_allocoopobjwithtrailer() so no need to check/set it again here
		if (HAK_CLASS_SPEC_IS_UNCOPYABLE(spec)) HAK_OBJ_SET_FLAGS_UNCOPYABLE (oop, 1);
		*/
	#endif
		HAK_OBJ_SET_FLAGS_FLEXI(oop, dspec.flexi);
	}
	hak_popvolats (hak, tmp_count);
	return oop;
}

/* ------------------------------------------------------------------------ *
 * NGC HANDLING
 * ------------------------------------------------------------------------ */

void hak_freengcobj (hak_t* hak, hak_oop_t obj)
{
	if (HAK_OOP_IS_POINTER(obj) && HAK_OBJ_GET_FLAGS_NGC(obj)) hak_freemem (hak, obj);
}

hak_oop_t hak_makengcbytearray (hak_t* hak, const hak_oob_t* ptr, hak_oow_t len)
{
	return alloc_numeric_array(hak, ptr, len, HAK_OBJ_TYPE_BYTE, HAK_SIZEOF(hak_oob_t), 0, 1);
}

hak_oop_t hak_remakengcbytearray (hak_t* hak, hak_oop_t obj, hak_oow_t newsize)
{
	hak_oop_t tmp;

	HAK_ASSERT (hak, !obj || (HAK_OOP_IS_POINTER(obj) && HAK_OBJ_GET_FLAGS_NGC(obj)));

	/* no hak_pushvolat() is needed because 'obj' is a non-GC object. */
	/* TODO: improve this by using realloc */

	tmp = hak_makengcbytearray(hak, HAK_NULL, newsize);
	if (HAK_LIKELY(tmp))
	{
		if (obj)
		{
			hak_oow_t cpsize;
			cpsize =  (newsize > HAK_OBJ_GET_SIZE(obj))? HAK_OBJ_GET_SIZE(obj): newsize;
			HAK_MEMCPY (((hak_oop_byte_t)tmp)->slot, ((hak_oop_byte_t)obj)->slot, cpsize * HAK_SIZEOF(hak_oob_t));
		}
		hak_freengcobj (hak, obj);
	}
	return tmp;
}

hak_oop_t hak_makengcarray (hak_t* hak, hak_oow_t len)
{
	return alloc_numeric_array(hak, HAK_NULL, len, HAK_OBJ_TYPE_OOP, HAK_SIZEOF(hak_oop_t), 0, 1);
}

hak_oop_t hak_remakengcarray (hak_t* hak, hak_oop_t obj, hak_oow_t newsize)
{
	hak_oop_t tmp;

	HAK_ASSERT (hak, !obj || (HAK_OOP_IS_POINTER(obj) && HAK_OBJ_GET_FLAGS_NGC(obj)));

	/* no hak_pushvolat() is needed because 'obj' is a non-GC object. */
	/* TODO: improve this by using realloc */

	tmp = hak_makengcarray(hak, newsize);
	if (HAK_LIKELY(tmp))
	{
		if (obj)
		{
			hak_oow_t cpsize;
			cpsize =  (newsize > HAK_OBJ_GET_SIZE(obj))? HAK_OBJ_GET_SIZE(obj): newsize;
			HAK_MEMCPY (((hak_oop_oop_t)tmp)->slot, ((hak_oop_oop_t)obj)->slot, cpsize * HAK_SIZEOF(hak_oop_t));
		}
		hak_freengcobj (hak, obj);
	}
	return tmp;
}

/* ------------------------------------------------------------------------ *
 * CONS
 * ------------------------------------------------------------------------ */
hak_oow_t hak_countcons (hak_t* hak, hak_oop_t cons)
{
	/* this function ignores the last cdr */
	hak_oow_t count = 1;

	HAK_ASSERT (hak, HAK_IS_CONS(hak, cons));
	do
	{
		cons = HAK_CONS_CDR(cons);
		if (!HAK_IS_CONS(hak, cons)) break;
		count++;
	}
	while (1);

	return count;
}

hak_oop_t hak_getlastconscdr (hak_t* hak, hak_oop_t cons)
{
	HAK_ASSERT (hak, HAK_IS_CONS(hak, cons));
	do
	{
		cons = HAK_CONS_CDR(cons);
		if (!HAK_IS_CONS(hak, cons)) break;
	}
	while (1);

	return cons;
}

hak_oop_t hak_reversecons (hak_t* hak, hak_oop_t cons)
{
	hak_oop_t ptr, prev, next;

	/* Note: The non-nil cdr in the last cons cell gets lost.
	 *  e.g.) Reversing (1 2 3 . 4) results in (3 2 1) */

	HAK_ASSERT (hak, HAK_IS_CONS(hak, cons));

	prev = hak->_nil;
	ptr = cons;

	do
	{
		next = HAK_CONS_CDR(ptr);
		HAK_CONS_CDR(ptr) = prev;
		prev = ptr;
		if (!HAK_IS_CONS(hak, next)) break;
		ptr = next;
	}
	while (1);

	return ptr;
}


/* ------------------------------------------------------------------------ *
 * OBJECT HASHING
 * ------------------------------------------------------------------------ */
int hak_hashobj (hak_t* hak, hak_oop_t obj, hak_oow_t* xhv)
{
	hak_oow_t hv;

	if (obj == hak->_nil)
	{
		*xhv = 0;
		return 0;
	}
	else if (obj == hak->_true)
	{
		*xhv = 1;
		return 0;
	}
	else if (obj == hak->_false)
	{
		*xhv = 2;
		return 0;
	}

	switch (HAK_OOP_GET_TAG(obj))
	{
		case HAK_OOP_TAG_SMOOI:
			hv = HAK_OOP_TO_SMOOI(obj);
			break;

/*
		case HAK_OOP_TAG_SMPTR:
			hv = (hak_oow_t)HAK_OOP_TO_SMPTR(obj);
			break;
*/

		case HAK_OOP_TAG_CHAR:
			hv = HAK_OOP_TO_CHAR(obj);
			break;

/*
		case HAK_OOP_TAG_ERROR:
			hv = HAK_OOP_TO_ERROR(obj);
			break;
*/

		default:
		{
			int type;

			HAK_ASSERT (hak, HAK_OOP_IS_POINTER(obj));
			type = HAK_OBJ_GET_FLAGS_TYPE(obj);
			switch (type)
			{
				case HAK_OBJ_TYPE_BYTE:
					hv = hak_hash_bytes(((hak_oop_byte_t)obj)->slot, HAK_OBJ_GET_SIZE(obj));
					break;

				case HAK_OBJ_TYPE_CHAR:
					hv = hak_hash_oochars(((hak_oop_char_t)obj)->slot, HAK_OBJ_GET_SIZE(obj));
					break;

				case HAK_OBJ_TYPE_HALFWORD:
					hv = hak_hash_halfwords(((hak_oop_halfword_t)obj)->slot, HAK_OBJ_GET_SIZE(obj));
					break;

				case HAK_OBJ_TYPE_WORD:
					hv = hak_hash_words(((hak_oop_word_t)obj)->slot, HAK_OBJ_GET_SIZE(obj));
					break;

				default:
					/* HAK_OBJ_TYPE_OOP, ... */
					hak_seterrbfmt(hak, HAK_ENOIMPL, "no builtin hash implemented for %O", obj); /* TODO: better error code? */
					return -1;
			}
			break;
		}
	}

	/* i assume that hak_hashxxx() functions limits the return value to fall
	 * between 0 and HAK_SMOOI_MAX inclusive */
	HAK_ASSERT (hak, hv >= 0 && hv <= HAK_SMOOI_MAX);
	*xhv = hv;
	return 0;
}

/* ------------------------------------------------------------------------ *
 * OBJECT EQUALITY
 * ------------------------------------------------------------------------ */
int hak_equalobjs (hak_t* hak, hak_oop_t rcv, hak_oop_t arg)
{
	int rtag;

	if (rcv == arg) return 1; /* identical. so equal */

	rtag = HAK_OOP_GET_TAG(rcv);
	if (rtag != HAK_OOP_GET_TAG(arg)) return 0;

	switch (rtag)
	{
		case HAK_OOP_TAG_SMOOI:
			return HAK_OOP_TO_SMOOI(rcv) == HAK_OOP_TO_SMOOI(arg)? 1: 0;

		case HAK_OOP_TAG_SMPTR:
			return HAK_OOP_TO_SMPTR(rcv) == HAK_OOP_TO_SMPTR(arg)? 1: 0;

		case HAK_OOP_TAG_CHAR:
			return HAK_OOP_TO_CHAR(rcv) == HAK_OOP_TO_CHAR(arg)? 1: 0;

		case HAK_OOP_TAG_ERROR:
			return HAK_OOP_TO_ERROR(rcv) == HAK_OOP_TO_ERROR(arg)? 1: 0;

		default:
		{
			HAK_ASSERT (hak, HAK_OOP_IS_POINTER(rcv));

			if (HAK_OBJ_GET_CLASS(rcv) != HAK_OBJ_GET_CLASS(arg)) return 0; /* different class, not equal */
			HAK_ASSERT (hak, HAK_OBJ_GET_FLAGS_TYPE(rcv) == HAK_OBJ_GET_FLAGS_TYPE(arg));

			if (HAK_OBJ_GET_SIZE(rcv) != HAK_OBJ_GET_SIZE(arg)) return 0; /* different size, not equal */

			switch (HAK_OBJ_GET_FLAGS_TYPE(rcv))
			{
				case HAK_OBJ_TYPE_BYTE:
				case HAK_OBJ_TYPE_CHAR:
				case HAK_OBJ_TYPE_HALFWORD:
				case HAK_OBJ_TYPE_WORD:
					return (HAK_MEMCMP(HAK_OBJ_GET_BYTE_SLOT(rcv), HAK_OBJ_GET_BYTE_SLOT(arg), HAK_BYTESOF(hak,rcv)) == 0)? 1: 0;

				default:
				{
					hak_oow_t i, size;

					if (rcv == hak->_nil) return arg == hak->_nil? 1: 0;
					if (rcv == hak->_true) return arg == hak->_true? 1: 0;
					if (rcv == hak->_false) return arg == hak->_false? 1: 0;

					/* HAK_OBJ_TYPE_OOP, ... */
					HAK_ASSERT (hak, HAK_OBJ_GET_FLAGS_TYPE(rcv) == HAK_OBJ_TYPE_OOP);

				#if 0
					hak_seterrbfmt (hak, HAK_ENOIMPL, "no builtin comparison implemented for %O and %O", rcv, arg); /* TODO: better error code */
					return -1;
				#else

					if (HAK_IS_PROCESS(hak,rcv))
					{
						/* the stack in a process object doesn't need to be
						 * scanned in full. the slots above the stack pointer
						 * are garbages. */
						size = HAK_PROCESS_NAMED_INSTVARS +
							  HAK_OOP_TO_SMOOI(((hak_oop_process_t)rcv)->sp) + 1;
						HAK_ASSERT (hak, size <= HAK_OBJ_GET_SIZE(rcv));
					}
					else
					{
						size = HAK_OBJ_GET_SIZE(rcv);
					}
					for (i = 0; i < size; i++)
					{
						int n;
						/* TODO: remove recursion */
						/* NOTE: even if the object implements the equality method,
						 * this primitive method doesn't honor it. */
						n = hak_equalobjs(hak, ((hak_oop_oop_t)rcv)->slot[i], ((hak_oop_oop_t)arg)->slot[i]);
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
