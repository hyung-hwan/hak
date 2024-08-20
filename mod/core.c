/*
 * $Id$
 *
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


#include "_core.h"
#include "../lib/hcl-prv.h"

static hcl_pfrc_t pf_core_basic_new (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t obj, size, inst;
	hcl_ooi_t nsize;

	obj = HCL_STACK_GETARG(hcl, nargs, 0);
	if (!HCL_IS_CLASS(hcl, obj))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "object not class - %O", obj);
		return HCL_PF_FAILURE;
	}

	size = HCL_STACK_GETARG(hcl, nargs, 1);
	if (!HCL_OOP_IS_SMOOI(size))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "size not numeric - %O", size);
		return HCL_PF_FAILURE;
	}

	nsize = HCL_OOP_TO_SMOOI(size);
	if (nsize < 0)
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "size not valid - %zd", nsize);
		return HCL_PF_FAILURE;
	}

	inst = hcl_instantiate(hcl, (hcl_oop_class_t)obj, HCL_NULL, nsize);
	if (HCL_UNLIKELY(!inst)) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, inst);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t __basic_at (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs, int span_fixed)
{
	hcl_oop_t obj, val;
	hcl_oop_t pos;
	hcl_oow_t index;
	hcl_oop_class_t _class;

	obj = HCL_STACK_GETARG(hcl, nargs, 0);
	pos = HCL_STACK_GETARG(hcl, nargs, 1);

	if (!HCL_OOP_IS_POINTER(obj) || !HCL_OBJ_GET_FLAGS_FLEXI(obj))
	{
	unindexable:
		/* the receiver is a special numeric object or a non-indexable object */
		hcl_seterrbfmt (hcl, HCL_EINVAL, "receiver not indexable - %O", obj);
		return HCL_PF_FAILURE;
	}

	if (hcl_inttooow_noseterr(hcl, pos, &index) <= 0)
	{
		/* negative integer or not integer */
		hcl_seterrbfmt (hcl, HCL_EINVAL, "position not valid - %O", pos);
		return HCL_PF_FAILURE;
	}

	_class = (hcl_oop_class_t)HCL_CLASSOF(hcl, obj);

	if (span_fixed)
	{
		hcl_oow_t size;
		size = HCL_OBJ_GET_SIZE(obj);
		if (index >= size)
		{
			hcl_seterrbfmt (hcl, HCL_EINVAL, "position(%zd) out of range - negative or greater than or equal to %zu", index, (hcl_ooi_t)size);
			return HCL_PF_FAILURE;
		}
	}
	else
	{
		hcl_oow_t fixed, flexi;

		fixed = HCL_CLASS_SPEC_NAMED_INSTVARS(_class->spec);
		flexi = HCL_OBJ_GET_SIZE(obj) - fixed;
		if (index >= flexi)
		{
			hcl_seterrbfmt (hcl, HCL_EINVAL, "position(%zd) out of range - negative or greater than or equal to %zu", index, (hcl_ooi_t)flexi);
			return HCL_PF_FAILURE;
		}
		index += fixed;
	}

	switch (HCL_OBJ_GET_FLAGS_TYPE(obj))
	{
		case HCL_OBJ_TYPE_OOP:
			val = HCL_OBJ_GET_OOP_VAL(obj, index);
			break;

		case HCL_OBJ_TYPE_CHAR:
		{
			hcl_ooch_t c;
			c = HCL_OBJ_GET_CHAR_VAL(obj, index);
			val = HCL_CHAR_TO_OOP(c);
			break;
		}

		case HCL_OBJ_TYPE_BYTE:
		{
			hcl_ooi_t b;
			b = HCL_OBJ_GET_BYTE_VAL(obj, index);
			val = HCL_SMOOI_TO_OOP(b);
			break;
		}

		case HCL_OBJ_TYPE_HALFWORD:
			val = hcl_oowtoint(hcl, HCL_OBJ_GET_HALFWORD_VAL(obj, index));
			if (HCL_UNLIKELY(!val)) return HCL_PF_FAILURE;
			break;

		case HCL_OBJ_TYPE_WORD:
			val = hcl_oowtoint(hcl, HCL_OBJ_GET_WORD_VAL(obj, index));
			if (HCL_UNLIKELY(!val)) return HCL_PF_FAILURE;
			break;

		default:
			goto unindexable;
	}


	HCL_STACK_SETRET (hcl, nargs, val);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_core_basic_at (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	return __basic_at(hcl, mod, nargs, 0);
}

static hcl_pfrc_t pf_core_prim_at (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	return __basic_at(hcl, mod, nargs, 1);
}

static hcl_pfrc_t __basic_at_put (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs, int span_fixed)
{
	hcl_oop_t obj, val;
	hcl_oop_t pos;
	hcl_oow_t index;
	hcl_oop_class_t _class;

	obj = HCL_STACK_GETARG(hcl, nargs, 0);
	pos = HCL_STACK_GETARG(hcl, nargs, 1);
	val = HCL_STACK_GETARG(hcl, nargs, 2);

	if (!HCL_OOP_IS_POINTER(obj) || !HCL_OBJ_GET_FLAGS_FLEXI(obj))
	{
	unindexable:
		hcl_seterrbfmt (hcl, HCL_EINVAL, "receiver not indexable - %O", obj);
		return HCL_PF_FAILURE;
	}

	if (hcl_inttooow_noseterr(hcl, pos, &index) <= 0)
	{
		/* negative integer or not integer */
		hcl_seterrbfmt (hcl, HCL_EINVAL, "position not valid - %O", pos);
		return HCL_PF_FAILURE;
	}

	_class = (hcl_oop_class_t)HCL_CLASSOF(hcl, obj);
	if (span_fixed)
	{
		hcl_oow_t size;
		size = HCL_OBJ_GET_SIZE(obj);
		if (index >= size)
		{
			hcl_seterrbfmt (hcl, HCL_EINVAL, "position(%zd) out of range - negative or greater than or equal to %zu", index, (hcl_ooi_t)size);
			return HCL_PF_FAILURE;
		}
	}
	else
	{
		hcl_oow_t fixed, flexi;

		fixed = HCL_CLASS_SPEC_NAMED_INSTVARS(_class->spec);
		flexi = HCL_OBJ_GET_SIZE(obj) - fixed;
		if (index >= flexi)
		{
			hcl_seterrbfmt (hcl, HCL_EINVAL, "position(%zd) out of range - negative or greater than or equal to %zu", index, (hcl_ooi_t)HCL_OBJ_GET_SIZE(obj));
			return HCL_PF_FAILURE;
		}
		index += fixed;
	}

	switch (HCL_OBJ_GET_FLAGS_TYPE(obj))
	{
		case HCL_OBJ_TYPE_OOP:
			HCL_OBJ_SET_OOP_VAL(obj, index, val);
			break;

		case HCL_OBJ_TYPE_CHAR:
		{
			hcl_ooch_t c;
			if (!HCL_OOP_IS_CHAR(val))
			{
				hcl_seterrbfmt (hcl, HCL_EINVAL, "value not character - %O", val);
				return HCL_PF_FAILURE;
			}
			c = HCL_OOP_TO_CHAR(val);
			HCL_OBJ_SET_CHAR_VAL(obj, index, c);
			break;
		}

		case HCL_OBJ_TYPE_BYTE:
		{
			hcl_ooi_t b;
			if (!HCL_OOP_IS_SMOOI(val))
			{
				hcl_seterrbfmt (hcl, HCL_EINVAL, "value not byte - %O", val);
				return HCL_PF_FAILURE;
			}
			b = HCL_OOP_TO_SMOOI(val);
			HCL_OBJ_SET_BYTE_VAL(obj, index, b);
			break;
		}

		case HCL_OBJ_TYPE_HALFWORD:
		{
			hcl_oow_t w;
			if (hcl_inttooow(hcl, val, &w) <= -1) return HCL_PF_FAILURE;
			HCL_OBJ_SET_HALFWORD_VAL(obj, index, w);
			break;
		}

		case HCL_OBJ_TYPE_WORD:
		{
			hcl_oow_t w;
			if (hcl_inttooow(hcl, val, &w) <= -1) return HCL_PF_FAILURE;
			HCL_OBJ_SET_WORD_VAL(obj, index, w);
			break;
		}

		default:
			goto unindexable;
	}


	HCL_STACK_SETRET (hcl, nargs, val);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_core_basic_at_put (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	return __basic_at_put(hcl, mod, nargs, 0);
}

static hcl_pfrc_t pf_core_prim_at_put (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	return __basic_at_put(hcl, mod, nargs, 1);
}

static hcl_pfrc_t pf_core_basic_size (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_oop_t src;
	hcl_oop_t size;

	src = (hcl_oop_oop_t)HCL_STACK_GETARG(hcl, nargs, 0);

	if (!HCL_OOP_IS_POINTER(src))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "source not sizable - %O", src);
		return HCL_PF_FAILURE;
	}

	size = hcl_oowtoint(hcl, HCL_OBJ_GET_SIZE(src));
	if (!size) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, size);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_core_class_name (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t obj;

	obj = HCL_STACK_GETARG(hcl, nargs, 0);

	if (!HCL_IS_CLASS(hcl, obj))
	{
	#if 0
		hcl_seterrbfmt (hcl, HCL_EINVAL, "receiver not class - %O", obj);
		return HCL_PF_FAILURE;
	#else
		obj = (hcl_oop_t)HCL_CLASSOF(hcl, obj);
		HCL_ASSERT (hcl, HCL_IS_CLASS(hcl, obj));
	#endif
	}

	HCL_STACK_SETRET (hcl, nargs, ((hcl_oop_class_t)obj)->name);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_core_class_responds_to (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t obj;
	hcl_oop_t msg;
	int x;

	obj = HCL_STACK_GETARG(hcl, nargs, 0);
	msg = HCL_STACK_GETARG(hcl, nargs, 1);
	if (!HCL_IS_CLASS(hcl, obj))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "receiver not class - %O", msg);
		return HCL_PF_FAILURE;
	}
	if (!HCL_OBJ_IS_CHAR_POINTER(msg))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "invalid message - %O", msg);
		return HCL_PF_FAILURE;
	}

	x = hcl_class_responds_to(hcl, obj, msg);
	HCL_STACK_SETRET (hcl, nargs, (x? hcl->_true: hcl->_false));
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_core_inst_responds_to (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t obj;
	hcl_oop_t msg;
	int x;

	obj = HCL_STACK_GETARG(hcl, nargs, 0);
	msg = HCL_STACK_GETARG(hcl, nargs, 1);
	if (!HCL_OBJ_IS_CHAR_POINTER(msg))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "invalid message - %O", msg);
		return HCL_PF_FAILURE;
	}

	x = hcl_inst_responds_to(hcl, obj, msg);
	HCL_STACK_SETRET (hcl, nargs, (x? hcl->_true: hcl->_false));
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_core_slice (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t src, slice, a1, a2;
	hcl_ooi_t size;
	hcl_ooi_t pos;
	hcl_ooi_t len;
	hcl_ooi_t i;

	src = HCL_STACK_GETARG(hcl, nargs, 0);
	a1 = HCL_STACK_GETARG(hcl, nargs, 1);
	a2 = HCL_STACK_GETARG(hcl, nargs, 2);

	if (!HCL_OOP_IS_POINTER(src))
	{
	unsliceable:
		hcl_seterrbfmt (hcl, HCL_EINVAL, "source not sliceable - %O", src);
		return HCL_PF_FAILURE;
	}

	if (!HCL_OOP_IS_SMOOI(a1))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "position not numeric - %O", a1);
		return HCL_PF_FAILURE;
	}
	if (!HCL_OOP_IS_SMOOI(a2))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "length not numeric - %O", a2);
		return HCL_PF_FAILURE;
	}

	size = HCL_OBJ_GET_SIZE(src);
	pos = HCL_OOP_TO_SMOOI(a1);
	len = HCL_OOP_TO_SMOOI(a2);

	if (pos < 0) pos = 0;
	else if (pos >= size) pos = size;
	if (len >= size - pos) len = size - pos;

/* TODO: check if the object is an indexable object from the class spec... */
	/* use HCL_OBJ_GET_CLASS() instead of HCL_CLASSOF() as we know it's an object */
	slice = hcl_instantiate(hcl, (hcl_oop_class_t)HCL_OBJ_GET_CLASS(src), HCL_NULL, len);
	if (HCL_UNLIKELY(!slice)) return HCL_PF_FAILURE;

/* OR if add by the number of fixed fields??? */
	switch (HCL_OBJ_GET_FLAGS_TYPE(src))
	{
		case HCL_OBJ_TYPE_OOP:
			for (i = 0; i < len; i++) HCL_OBJ_GET_OOP_VAL(slice, i) = HCL_OBJ_GET_OOP_VAL(src, pos + i);
			break;

		case HCL_OBJ_TYPE_CHAR:
			for (i = 0; i < len; i++) HCL_OBJ_GET_CHAR_VAL(slice, i) = HCL_OBJ_GET_CHAR_VAL(src, pos + i);
			break;

		case HCL_OBJ_TYPE_BYTE:
			for (i = 0; i < len; i++) HCL_OBJ_GET_BYTE_VAL(slice, i) = HCL_OBJ_GET_BYTE_VAL(src, pos + i);
			break;

		case HCL_OBJ_TYPE_HALFWORD:
			for (i = 0; i < len; i++) HCL_OBJ_GET_HALFWORD_VAL(slice, i) = HCL_OBJ_GET_HALFWORD_VAL(src, pos + i);
			break;

		case HCL_OBJ_TYPE_WORD:
			for (i = 0; i < len; i++) HCL_OBJ_GET_WORD_VAL(slice, i) = HCL_OBJ_GET_WORD_VAL(src, pos + i);
			break;

		default:
			goto unsliceable;
			break;
	}

	HCL_STACK_SETRET (hcl, nargs, slice);
	return HCL_PF_SUCCESS;
}


static hcl_pfinfo_t pfinfos[] =
{
	{ "basicAt",            { HCL_PFBASE_FUNC, pf_core_basic_at,              2,  2 } },
	{ "basicAtPut",         { HCL_PFBASE_FUNC, pf_core_basic_at_put,          3,  3 } },
	{ "basicNew",           { HCL_PFBASE_FUNC, pf_core_basic_new,             2,  2 } },
	{ "basicSize",          { HCL_PFBASE_FUNC, pf_core_basic_size,            1,  1 } },
	{ "className",          { HCL_PFBASE_FUNC, pf_core_class_name,            1,  1 } },
	{ "classRespondsTo",    { HCL_PFBASE_FUNC, pf_core_class_responds_to,     2,  2 } },
	{ "instRespondsTo",     { HCL_PFBASE_FUNC, pf_core_inst_responds_to,      2,  2 } },
	{ "primAt",             { HCL_PFBASE_FUNC, pf_core_prim_at,               2,  2 } },
	{ "primAtPut",          { HCL_PFBASE_FUNC, pf_core_prim_at_put,           3,  3 } },
	{ "slice",              { HCL_PFBASE_FUNC, pf_core_slice,                 3,  3 } }
};

/* ------------------------------------------------------------------------ */

static hcl_pfbase_t* query (hcl_t* hcl, hcl_mod_t* mod, const hcl_ooch_t* name, hcl_oow_t namelen)
{
	return hcl_findpfbase(hcl, pfinfos, HCL_COUNTOF(pfinfos), name, namelen);
}

static void unload (hcl_t* hcl, hcl_mod_t* mod)
{
}

int hcl_mod_core (hcl_t* hcl, hcl_mod_t* mod)
{
	mod->query = query;
	mod->unload = unload; 
	mod->ctx = HCL_NULL;
	return 0;
}
