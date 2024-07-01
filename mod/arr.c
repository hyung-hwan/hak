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


#include "_arr.h"

static hcl_pfrc_t pf_arr_new (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t sz, arr;
	hcl_oow_t size;

	sz = (hcl_oop_t)HCL_STACK_GETARG(hcl, nargs, 0);
	if (hcl_inttooow(hcl, sz, &size) == 0) return HCL_PF_FAILURE;

	arr = hcl_makearray(hcl, size, 0);
	if (HCL_UNLIKELY(!arr)) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, arr);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_arr_get (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_oop_t arr;
	hcl_oop_t idx;
	hcl_oow_t index;

	arr = (hcl_oop_oop_t)HCL_STACK_GETARG(hcl, nargs, 0);
	idx = HCL_STACK_GETARG(hcl, nargs, 1);

	if (!HCL_IS_ARRAY(hcl,arr))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "parameter not an array - %O", arr);
		return HCL_PF_FAILURE;
	}

	if (hcl_inttooow(hcl, idx, &index) == 0) return HCL_PF_FAILURE;

	if (index >= HCL_OBJ_GET_SIZE(arr))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "array index %zu out of bounds for array of size %zu", index, HCL_OBJ_GET_SIZE(arr));
		return HCL_PF_FAILURE;
	}

	HCL_STACK_SETRET (hcl, nargs, arr->slot[index]);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_arr_put (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_oop_t arr;
	hcl_oop_t idx, val;
	hcl_oow_t index;

	arr = (hcl_oop_oop_t)HCL_STACK_GETARG(hcl, nargs, 0);
	idx = HCL_STACK_GETARG(hcl, nargs, 1);
	val = HCL_STACK_GETARG(hcl, nargs, 2);

	if (!HCL_IS_ARRAY(hcl,arr))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "parameter not an array - %O", arr);
		return HCL_PF_FAILURE;
	}

	if (hcl_inttooow(hcl, idx, &index) == 0) return HCL_PF_FAILURE;

	if (index >= HCL_OBJ_GET_SIZE(arr))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "array index %zu out of bounds for array of size %zu", index, HCL_OBJ_GET_SIZE(arr));
		return HCL_PF_FAILURE;
	}

	arr->slot[index] = val;
	HCL_STACK_SETRET (hcl, nargs, val);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_arr_size (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
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

static hcl_pfrc_t pf_arr_slice (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
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
	{ { 'g','e','t','\0' },              { HCL_PFBASE_FUNC, pf_arr_get,     2,  2 } },
	{ { 'l','e','n','g','t','h','\0' },  { HCL_PFBASE_FUNC, pf_arr_size,    1,  1 } },
	{ { 'n','e','w','\0' },              { HCL_PFBASE_FUNC, pf_arr_new,     1,  1 } },
	{ { 'p','u','t','\0' },              { HCL_PFBASE_FUNC, pf_arr_put,     3,  3 } },
	{ { 's','i','z','e','\0' },          { HCL_PFBASE_FUNC, pf_arr_size,    1,  1 } },
	{ { 's','l','i','c','e','\0' },      { HCL_PFBASE_FUNC, pf_arr_slice,   3,  3 } }
};

/* ------------------------------------------------------------------------ */

static hcl_pfbase_t* query (hcl_t* hcl, hcl_mod_t* mod, const hcl_ooch_t* name, hcl_oow_t namelen)
{
	return hcl_findpfbase(hcl, pfinfos, HCL_COUNTOF(pfinfos), name, namelen);
}


static void unload (hcl_t* hcl, hcl_mod_t* mod)
{
}

int hcl_mod_arr (hcl_t* hcl, hcl_mod_t* mod)
{
	mod->query = query;
	mod->unload = unload; 
	mod->ctx = HCL_NULL;
	return 0;
}
