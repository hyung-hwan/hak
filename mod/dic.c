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


#include "_dic.h"

static hak_pfrc_t pf_dic_get (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t dic;
	hak_oop_t key;
	hak_oop_cons_t pair;

	dic = HAK_STACK_GETARG(hak, nargs, 0);
	key = HAK_STACK_GETARG(hak, nargs, 1);

	if (!HAK_IS_DIC(hak,dic))
	{
		hak_seterrbfmt (hak, HAK_EINVAL, "parameter not an dictionary - %O", dic);
		return HAK_PF_FAILURE;
	}

	pair = hak_getatdic(hak, (hak_oop_dic_t)dic, key);
	if (!pair)
	{
		HAK_STACK_SETRETTOERROR (hak, nargs, HAK_ENOENT);
		return HAK_PF_SUCCESS;
	}

	HAK_STACK_SETRET (hak, nargs, HAK_CONS_CDR(pair));
	return HAK_PF_SUCCESS;
}


static hak_pfrc_t pf_dic_put (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t dic;
	hak_oop_t key, val;
	hak_oop_cons_t pair;

	dic = HAK_STACK_GETARG(hak, nargs, 0);
	key = HAK_STACK_GETARG(hak, nargs, 1);
	val = HAK_STACK_GETARG(hak, nargs, 2);

	if (!HAK_IS_DIC(hak,dic))
	{
		hak_seterrbfmt (hak, HAK_EINVAL, "parameter not an dictionary - %O", dic);
		return HAK_PF_FAILURE;
	}

	pair = hak_putatdic(hak, (hak_oop_dic_t)dic, key, val);
	if (!pair)
	{
		HAK_STACK_SETRETTOERRNUM (hak, nargs);
		return HAK_PF_SUCCESS;
	}

	HAK_STACK_SETRET (hak, nargs, HAK_CONS_CDR(pair));
	return HAK_PF_SUCCESS;
}


static int walker (hak_t* hak, hak_oop_dic_t dic, hak_oop_cons_t pair, void* ctx)
{
	HAK_DEBUG2 (hak, "walker ===> %O  =====> %O\n", HAK_CONS_CAR(pair), HAK_CONS_CDR(pair));
	return 0;
}

static hak_pfrc_t pf_dic_walk (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
/* TODO: write a proper function 
 * (dic.apply #{ ... } callable-or-lambda)
 */
	hak_oop_t arg;

	arg = HAK_STACK_GETARG(hak, nargs, 0);
	if (!HAK_IS_DIC(hak,arg))
	{
		hak_seterrbfmt (hak, HAK_EINVAL, "parameter not a dictionary - %O", arg);
		return HAK_PF_FAILURE;
	}

	hak_walkdic (hak, (hak_oop_dic_t)arg, walker, HAK_NULL);
	HAK_STACK_SETRET (hak, nargs, hak->_true);
	return HAK_PF_SUCCESS;
}


static hak_pfinfo_t pfinfos[] =
{
	{ "get",   { HAK_PFBASE_FUNC, pf_dic_get,     2,  2 } },
/*	{ "make",  { HAK_PFBASE_FUNC, pf_dic_make,    1,  1 } }, */
	{ "put",   { HAK_PFBASE_FUNC, pf_dic_put,     3,  3 } },
	{ "walk",  { HAK_PFBASE_FUNC, pf_dic_walk,    2,  2 } },
};

/* ------------------------------------------------------------------------ */

static hak_pfbase_t* query (hak_t* hak, hak_mod_t* mod, const hak_ooch_t* name, hak_oow_t namelen)
{
	return hak_findpfbase(hak, pfinfos, HAK_COUNTOF(pfinfos), name, namelen);
}


static void unload (hak_t* hak, hak_mod_t* mod)
{
}

int hak_mod_dic (hak_t* hak, hak_mod_t* mod)
{
	mod->query = query;
	mod->unload = unload; 
	mod->ctx = HAK_NULL;
	return 0;
}
