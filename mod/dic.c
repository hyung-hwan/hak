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
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not an dictionary - %O", dic);
		return HAK_PF_FAILURE;
	}

	pair = hak_getatdic(hak, (hak_oop_dic_t)dic, key);
	if (!pair)
	{
		HAK_STACK_SETRETTOERROR(hak, nargs, HAK_ENOENT);
		return HAK_PF_SUCCESS;
	}

	HAK_STACK_SETRET(hak, nargs, HAK_CONS_CDR(pair));
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
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not an dictionary - %O", dic);
		return HAK_PF_FAILURE;
	}

	pair = hak_putatdic(hak, (hak_oop_dic_t)dic, key, val);
	if (!pair)
	{
		HAK_STACK_SETRETTOERRNUM(hak, nargs);
		return HAK_PF_SUCCESS;
	}

	HAK_STACK_SETRET(hak, nargs, HAK_CONS_CDR(pair));
	return HAK_PF_SUCCESS;
}


/* ------------------------------------------------------------------------ *
 * INSPECTION
 * ------------------------------------------------------------------------ */

/* resolve and validate the dictionary argument shared by everything here */
static hak_oop_dic_t arg_to_dic (hak_t* hak, hak_ooi_t nargs, hak_ooi_t idx)
{
	hak_oop_t d = HAK_STACK_GETARG(hak, nargs, idx);
	if (!HAK_IS_DIC(hak, d))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not a dictionary - %O", d);
		return HAK_NULL;
	}
	return (hak_oop_dic_t)d;
}

/* (dic.make [bucket-size]) -> a new dictionary
 * The literal #{} covers the common case; this is for choosing an initial
 * bucket size when the eventual population is known. */
static hak_pfrc_t pf_dic_make (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oow_t inisize = 16;
	hak_oop_t d;

	if (nargs >= 1)
	{
		hak_oop_t t = HAK_STACK_GETARG(hak, nargs, 0);
		hak_ooi_t v;
		if (hak_inttoooi(hak, t, &v) == 0) return HAK_PF_FAILURE;
		if (v <= 0)
		{
			hak_seterrbfmt(hak, HAK_EINVAL, "bucket size not positive - %O", t);
			return HAK_PF_FAILURE;
		}
		inisize = (hak_oow_t)v;
	}

	d = hak_makedic(hak, inisize);
	if (HAK_UNLIKELY(!d)) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, d);
	return HAK_PF_SUCCESS;
}

/* (dic.size d) -> how many pairs it holds */
static hak_pfrc_t pf_dic_size (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_dic_t dic = arg_to_dic(hak, nargs, 0);
	if (HAK_UNLIKELY(!dic)) return HAK_PF_FAILURE;
	HAK_STACK_SETRET(hak, nargs, dic->tally);
	return HAK_PF_SUCCESS;
}

/* (dic.has? d k) -> true or false
 * dic.get answers with an error object for a missing key, which is awkward to
 * test when the stored value could itself be an error. */
static hak_pfrc_t pf_dic_has (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_dic_t dic = arg_to_dic(hak, nargs, 0);
	hak_oop_t key;

	if (HAK_UNLIKELY(!dic)) return HAK_PF_FAILURE;
	key = HAK_STACK_GETARG(hak, nargs, 1);

	HAK_STACK_SETRET(hak, nargs, hak_getatdic(hak, dic, key)? hak->_true: hak->_false);
	return HAK_PF_SUCCESS;
}

/* (dic.delete d k) -> true if a pair went away, false if there was none */
static hak_pfrc_t pf_dic_delete (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_dic_t dic = arg_to_dic(hak, nargs, 0);
	hak_oop_t key;

	if (HAK_UNLIKELY(!dic)) return HAK_PF_FAILURE;
	key = HAK_STACK_GETARG(hak, nargs, 1);

	HAK_STACK_SETRET(hak, nargs, (hak_zapatdic(hak, dic, key) >= 0)? hak->_true: hak->_false);
	return HAK_PF_SUCCESS;
}

/* ------------------------------------------------------------------------ *
 * ITERATION
 *
 * A dictionary is iterated by asking for its keys or values as an array and
 * walking that from hak code. There is deliberately no callback form: calling
 * a hak block from inside a primitive would need the virtual machine to be
 * re-entrant, which it is not, and collecting first also keeps the caller
 * clear of the question of what happens when the dictionary is modified
 * during a walk.
 * ------------------------------------------------------------------------ */

struct collect_t
{
	hak_oop_t arr;
	hak_oow_t idx;
	hak_oow_t capa;
	int       want;       /* WANT_KEY, WANT_VALUE or WANT_PAIR */
};

#define WANT_KEY   0
#define WANT_VALUE 1
#define WANT_PAIR  2
typedef struct collect_t collect_t;

static int collect_walker (hak_t* hak, hak_oop_dic_t dic, hak_oop_cons_t pair, void* ctx)
{
	collect_t* c = (collect_t*)ctx;

	/* tally and the number of pairs actually walked should agree, but never
	 * write past the array we sized from it */
	if (c->idx >= c->capa) return -1;

	HAK_OBJ_SET_OOP_VAL(c->arr, c->idx,
		(c->want == WANT_PAIR)? (hak_oop_t)pair:
		(c->want == WANT_VALUE)? HAK_CONS_CDR(pair): HAK_CONS_CAR(pair));
	c->idx++;
	return 0;
}

static hak_pfrc_t collect (hak_t* hak, hak_ooi_t nargs, int want)
{
	hak_oop_t dic;
	hak_oop_t arr;
	hak_ooi_t n;
	collect_t c;

	if (!arg_to_dic(hak, nargs, 0)) return HAK_PF_FAILURE;
	dic = HAK_STACK_GETARG(hak, nargs, 0);

	n = HAK_OOP_TO_SMOOI(((hak_oop_dic_t)dic)->tally);

	/* Keep the dictionary rooted across the allocation. hak's collector is
	 * mark-sweep and does not relocate, and the argument stack roots the
	 * dictionary anyway, so this is belt-and-braces today; it is what would
	 * keep the code correct if the compacting collector in gc.c were ever
	 * turned on. */
	hak_pushvolat(hak, &dic);
	arr = hak_makearray(hak, (hak_oow_t)n);
	hak_popvolat(hak);
	if (HAK_UNLIKELY(!arr)) return HAK_PF_FAILURE;

	/* nothing below allocates, so raw slot writes are safe from here */
	c.arr = arr;
	c.idx = 0;
	c.capa = (hak_oow_t)n;
	c.want = want;
	hak_walkdic(hak, (hak_oop_dic_t)dic, collect_walker, &c);

	HAK_STACK_SETRET(hak, nargs, arr);
	return HAK_PF_SUCCESS;
}

/* (dic.keys d) -> an array of the keys */
static hak_pfrc_t pf_dic_keys (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	return collect(hak, nargs, WANT_KEY);
}

/* (dic.values d) -> an array of the values, in the same order as dic.keys */
static hak_pfrc_t pf_dic_values (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	return collect(hak, nargs, WANT_VALUE);
}

/* (dic.pairs d) -> an array of the associations themselves
 *
 * One array instead of the two that dic.keys plus dic.values costs; read each
 * association with core.car and core.cdr. */
static hak_pfrc_t pf_dic_pairs (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	return collect(hak, nargs, WANT_PAIR);
}

/* ------------------------------------------------------------------------ *
 * ALLOCATION-FREE TRAVERSAL
 *
 * dic.keys, dic.values and dic.pairs each build an array. These two allocate
 * nothing at all: walk the slots from 0 to dic.bucketSize and read whichever
 * are occupied, taking key and value from the association with core.car and
 * core.cdr.
 *
 * The cost is that the traversal is live rather than a snapshot - inserting
 * during one may grow the bucket and rearrange everything, so a pair can be
 * seen twice or missed. Removing is safe, since a removal never grows the
 * bucket. Use dic.keys where a snapshot matters; use these where the traversal
 * is hot and nothing is being inserted.
 *
 * These do expose that a bucket exists and has gaps. The bucket doubles once it
 * is nearly full, so for a dictionary of any size the load runs between about
 * 57% and 96% - a quarter of the slots walked are empty on average. A small
 * dictionary still inside its initial bucket of 26 is far sparser than that, so
 * the slot walk is relatively worst exactly where it matters least.
 * ------------------------------------------------------------------------ */

/* (dic.bucketSize d) -> how many slots to walk */
static hak_pfrc_t pf_dic_bucket_size (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_dic_t dic = arg_to_dic(hak, nargs, 0);
	if (HAK_UNLIKELY(!dic)) return HAK_PF_FAILURE;
	HAK_STACK_SETRET(hak, nargs, HAK_SMOOI_TO_OOP((hak_ooi_t)HAK_OBJ_GET_SIZE(dic->bucket)));
	return HAK_PF_SUCCESS;
}

/* (dic.pairAt d index) -> the association in that slot, or nil if it is empty */
static hak_pfrc_t pf_dic_pair_at (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_dic_t dic = arg_to_dic(hak, nargs, 0);
	hak_oop_t t;
	hak_ooi_t i;

	if (HAK_UNLIKELY(!dic)) return HAK_PF_FAILURE;

	t = HAK_STACK_GETARG(hak, nargs, 1);
	if (hak_inttoooi(hak, t, &i) == 0) return HAK_PF_FAILURE;

	if (i < 0 || i >= (hak_ooi_t)HAK_OBJ_GET_SIZE(dic->bucket))
	{
		hak_seterrbfmt(hak, HAK_ERANGE, "slot %zd out of range - the bucket holds %zu", i, HAK_OBJ_GET_SIZE(dic->bucket));
		return HAK_PF_FAILURE;
	}

	t = dic->bucket->slot[i];
	HAK_STACK_SETRET(hak, nargs, HAK_IS_CONS(hak, t)? t: hak->_nil);
	return HAK_PF_SUCCESS;
}

/* (dic.clear d) -> the number of pairs removed */
static hak_pfrc_t pf_dic_clear (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t dic;
	hak_oop_t keys;
	hak_ooi_t n, i, gone = 0;
	collect_t c;

	if (!arg_to_dic(hak, nargs, 0)) return HAK_PF_FAILURE;
	dic = HAK_STACK_GETARG(hak, nargs, 0);

	n = HAK_OOP_TO_SMOOI(((hak_oop_dic_t)dic)->tally);

	/* collect the keys before removing any: zapping during a walk would
	 * disturb the buckets the walk is traversing. see collect() on the guard. */
	hak_pushvolat(hak, &dic);
	keys = hak_makearray(hak, (hak_oow_t)n);
	hak_popvolat(hak);
	if (HAK_UNLIKELY(!keys)) return HAK_PF_FAILURE;

	c.arr = keys;
	c.idx = 0;
	c.capa = (hak_oow_t)n;
	c.want = WANT_KEY;
	hak_walkdic(hak, (hak_oop_dic_t)dic, collect_walker, &c);

	for (i = 0; i < n; i++)
	{
		if (hak_zapatdic(hak, (hak_oop_dic_t)dic, HAK_OBJ_GET_OOP_VAL(keys, i)) >= 0) gone++;
	}

	HAK_STACK_SETRET(hak, nargs, HAK_SMOOI_TO_OOP(gone));
	return HAK_PF_SUCCESS;
}


/* sorted: hak_findpfbase() binary-searches this table */
static hak_pfinfo_t pfinfos[] =
{
	{ "bucketSize", { HAK_PFBASE_FUNC, pf_dic_bucket_size, 1,  1 } },
	{ "clear",  { HAK_PFBASE_FUNC, pf_dic_clear,   1,  1 } },
	{ "delete", { HAK_PFBASE_FUNC, pf_dic_delete,  2,  2 } },
	{ "get",    { HAK_PFBASE_FUNC, pf_dic_get,     2,  2 } },
	{ "has?",   { HAK_PFBASE_FUNC, pf_dic_has,     2,  2 } },
	{ "keys",   { HAK_PFBASE_FUNC, pf_dic_keys,    1,  1 } },
	{ "make",   { HAK_PFBASE_FUNC, pf_dic_make,    0,  1 } },
	{ "pairAt", { HAK_PFBASE_FUNC, pf_dic_pair_at, 2,  2 } },
	{ "pairs",  { HAK_PFBASE_FUNC, pf_dic_pairs,   1,  1 } },
	{ "put",    { HAK_PFBASE_FUNC, pf_dic_put,     3,  3 } },
	{ "size",   { HAK_PFBASE_FUNC, pf_dic_size,    1,  1 } },
	{ "values", { HAK_PFBASE_FUNC, pf_dic_values,  1,  1 } }
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
