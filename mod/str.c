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


#include "_str.h"

static hcl_pfrc_t pf_str_at (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t str, pos;
	hcl_ooch_t cv;
	hcl_ooi_t size;
	hcl_ooi_t idx;

	str = HCL_STACK_GETARG(hcl, nargs, 0);
	pos = HCL_STACK_GETARG(hcl, nargs, 1);

	if (!HCL_IS_STRING(hcl, str))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "parameter not string - %O", str);
		return HCL_PF_FAILURE;
	}

	if (!HCL_OOP_IS_SMOOI(pos))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "parameter not smooi - %O", pos);
		return HCL_PF_FAILURE;
	}

	idx = HCL_OOP_TO_SMOOI(pos);
	size = HCL_OBJ_GET_SIZE(str);
	if (idx <= -1 || idx >= size)
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "parameter out of range - %O", pos);
		return HCL_PF_FAILURE;
	}
	cv = HCL_OBJ_GET_CHAR_VAL(str, idx);
	HCL_STACK_SETRET (hcl, nargs, HCL_CHAR_TO_OOP(cv));
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_str_at_put (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t str, pos, val;
	hcl_ooch_t cv;
	hcl_ooi_t size;
	hcl_ooi_t idx;

	str = HCL_STACK_GETARG(hcl, nargs, 0);
	pos = HCL_STACK_GETARG(hcl, nargs, 1);
	val = HCL_STACK_GETARG(hcl, nargs, 2);

	if (!HCL_IS_STRING(hcl, str))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "parameter not string - %O", str);
		return HCL_PF_FAILURE;
	}

	if (!HCL_OOP_IS_SMOOI(pos))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "parameter not integer - %O", pos);
		return HCL_PF_FAILURE;
	}

	if (!HCL_OOP_IS_CHAR(val))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "parameter not character - %O", pos);
		return HCL_PF_FAILURE;
	}

	idx = HCL_OOP_TO_SMOOI(pos);
	size = HCL_OBJ_GET_SIZE(str);
	if (idx <= -1 || idx >= size)
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "parameter out of range - %O", pos);
		return HCL_PF_FAILURE;
	}

	cv = HCL_OOP_TO_CHAR(val);
	HCL_OBJ_SET_CHAR_VAL(str, idx, cv);
	HCL_STACK_SETRET (hcl, nargs, val);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_str_length (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t str;
	hcl_ooi_t size;

	str = HCL_STACK_GETARG(hcl, nargs, 0);

	if (!HCL_IS_STRING(hcl, str))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "parameter not string - %O", str);
		return HCL_PF_FAILURE;
	}

	size = HCL_OBJ_GET_SIZE(str);
	HCL_STACK_SETRET (hcl, nargs, HCL_SMOOI_TO_OOP(size));
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_str_slice (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t str, slice, a1, a2;
	hcl_ooi_t size;
	hcl_ooi_t pos;
	hcl_ooi_t len;

	str = HCL_STACK_GETARG(hcl, nargs, 0);
	a1 = HCL_STACK_GETARG(hcl, nargs, 1);
	a2 = HCL_STACK_GETARG(hcl, nargs, 2);

	if (!HCL_IS_STRING(hcl, str))
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "parameter not string - %O", str);
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

	size = HCL_OBJ_GET_SIZE(str);
	pos = HCL_OOP_TO_SMOOI(a1);
	len = HCL_OOP_TO_SMOOI(a2);

	if (pos < 0) pos = 0;
	else if (pos >= size) pos = size;
	if (len >= size - pos) len = size - pos;
	slice = hcl_makestring(hcl, HCL_OBJ_GET_CHAR_PTR(str, pos), len, 0);
	if (HCL_UNLIKELY(!slice)) return HCL_PF_FAILURE;

	HCL_STACK_SETRET (hcl, nargs, slice);
	return HCL_PF_SUCCESS;
}

static hcl_pfinfo_t pfinfos[] =
{
	/*{ { 'V','A','R','\0' },                  { HCL_PFBASE_VAR,  HCL_NULL,           0,  0 } },*/
	{ { 'a','t','\0' },                      { HCL_PFBASE_FUNC,  pf_str_at,         2,  2 } },
	{ { 'a','t','P','u','t','\0' },          { HCL_PFBASE_FUNC,  pf_str_at_put,     3,  3 } },
	{ { 'l','e','n','g','t','h','\0' },      { HCL_PFBASE_FUNC,  pf_str_length,     1,  1 } },
	{ { 's','i','z','e'},                    { HCL_PFBASE_FUNC,  pf_str_length,     1,  1 } },
	{ { 's','l','i','c','e'},                { HCL_PFBASE_FUNC,  pf_str_slice,      3,  3 } },
	{ { 's','u','b','s','t','r'},            { HCL_PFBASE_FUNC,  pf_str_slice,      3,  3 } }
};

/* ------------------------------------------------------------------------ */

static hcl_pfbase_t* query (hcl_t* hcl, hcl_mod_t* mod, const hcl_ooch_t* name, hcl_oow_t namelen)
{
	return hcl_findpfbase(hcl, pfinfos, HCL_COUNTOF(pfinfos), name, namelen);
}

static void unload (hcl_t* hcl, hcl_mod_t* mod)
{
}

int hcl_mod_str (hcl_t* hcl, hcl_mod_t* mod)
{
	mod->query = query;
	mod->unload = unload; 
	mod->ctx = HCL_NULL;
	return 0;
}
