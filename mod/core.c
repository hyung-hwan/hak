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

static hcl_pfrc_t pf_core_get_class_name (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t obj;

	obj = HCL_STACK_GETARG(hcl, nargs, 0);

	if (!HCL_IS_CLASS(hcl, obj))
	{
	#if 0
		hcl_seterrbfmt (hcl, HCL_EINVAL, "parameter not a class - %O", obj);
		return HCL_PF_FAILURE;
	#else
		obj = (hcl_oop_t)HCL_CLASSOF(hcl, obj);
		HCL_ASSERT (hcl, HCL_IS_CLASS(hcl, obj));
	#endif
	}

	HCL_STACK_SETRET (hcl, nargs, ((hcl_oop_class_t)obj)->name);
	return HCL_PF_SUCCESS;
}

static hcl_pfinfo_t pfinfos[] =
{
	{ { 'c','l','a','s','s','_','n','a','m','e','\0' },  { HCL_PFBASE_FUNC, pf_core_get_class_name, 1,  1 } },
/*
	{ { 'l','e','n','g','t','h','\0' },  { HCL_PFBASE_FUNC, pf_core_size,    1,  1 } },
	{ { 'n','e','w','\0' },              { HCL_PFBASE_FUNC, pf_core_new,     1,  1 } },
	{ { 'p','u','t','\0' },              { HCL_PFBASE_FUNC, pf_core_put,     3,  3 } },
	{ { 's','i','z','e','\0' },          { HCL_PFBASE_FUNC, pf_core_size,    1,  1 } },
	{ { 's','l','i','c','e','\0' },      { HCL_PFBASE_FUNC, pf_core_slice,   3,  3 } }
*/
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
