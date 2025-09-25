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
#include "../lib/hak-prv.h"

static hak_pfrc_t pf_core_basic_new (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t obj, inst;
	hak_ooi_t nsize;

	obj = HAK_STACK_GETARG(hak, nargs, 0);
	if (!HAK_IS_CLASS(hak, obj))
	{
		hak_seterrbfmt (hak, HAK_EINVAL, "object not class - %O", obj);
		return HAK_PF_FAILURE;
	}

	nsize = 0;
	if (nargs >= 2)
	{
		hak_oop_t size;

		size = HAK_STACK_GETARG(hak, nargs, 1);
		if (!HAK_OOP_IS_SMOOI(size))
		{
			hak_seterrbfmt (hak, HAK_EINVAL, "size not numeric - %O", size);
			return HAK_PF_FAILURE;
		}

		nsize = HAK_OOP_TO_SMOOI(size);
		if (nsize < 0)
		{
			hak_seterrbfmt (hak, HAK_EINVAL, "size not valid - %zd", nsize);
			return HAK_PF_FAILURE;
		}
	}

	inst = hak_instantiate(hak, (hak_oop_class_t)obj, HAK_NULL, nsize);
	if (HAK_UNLIKELY(!inst)) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, inst);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t __basic_at (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs, int span_fixed)
{
	hak_oop_t obj, val;
	hak_oop_t pos;
	hak_oow_t index;
	hak_oop_class_t _class;

	obj = HAK_STACK_GETARG(hak, nargs, 0);
	pos = HAK_STACK_GETARG(hak, nargs, 1);

	if (!HAK_OOP_IS_POINTER(obj) || !HAK_OBJ_GET_FLAGS_FLEXI(obj))
	{
	unindexable:
		/* the receiver is a special numeric object or a non-indexable object */
		hak_seterrbfmt (hak, HAK_EINVAL, "receiver not indexable - %O", obj);
		return HAK_PF_FAILURE;
	}

	if (hak_inttooow_noseterr(hak, pos, &index) <= 0)
	{
		/* negative integer or not integer */
		hak_seterrbfmt (hak, HAK_EINVAL, "position not valid - %O", pos);
		return HAK_PF_FAILURE;
	}

	_class = (hak_oop_class_t)HAK_CLASSOF(hak, obj);

	if (span_fixed)
	{
		hak_oow_t size;
		size = HAK_OBJ_GET_SIZE(obj);
		if (index >= size)
		{
			hak_seterrbfmt (hak, HAK_EINVAL, "position(%zd) out of range - negative or greater than or equal to %zu", index, (hak_ooi_t)size);
			return HAK_PF_FAILURE;
		}
	}
	else
	{
		hak_oow_t fixed, flexi;

		fixed = HAK_CLASS_SPEC_NAMED_INSTVARS(HAK_OOP_TO_SMOOI(_class->spec));
		flexi = HAK_OBJ_GET_SIZE(obj) - fixed;
		if (index >= flexi)
		{
			hak_seterrbfmt (hak, HAK_EINVAL, "position(%zd) out of range - negative or greater than or equal to %zu", index, (hak_ooi_t)flexi);
			return HAK_PF_FAILURE;
		}
		index += fixed;
	}

	switch (HAK_OBJ_GET_FLAGS_TYPE(obj))
	{
		case HAK_OBJ_TYPE_OOP:
			val = HAK_OBJ_GET_OOP_VAL(obj, index);
			break;

		case HAK_OBJ_TYPE_CHAR:
		{
			hak_ooch_t c;
			c = HAK_OBJ_GET_CHAR_VAL(obj, index);
			val = HAK_CHAR_TO_OOP(c);
			break;
		}

		case HAK_OBJ_TYPE_BYTE:
		{
			hak_ooi_t b;
			b = HAK_OBJ_GET_BYTE_VAL(obj, index);
			val = HAK_SMOOI_TO_OOP(b);
			break;
		}

		case HAK_OBJ_TYPE_HALFWORD:
			val = hak_oowtoint(hak, HAK_OBJ_GET_HALFWORD_VAL(obj, index));
			if (HAK_UNLIKELY(!val)) return HAK_PF_FAILURE;
			break;

		case HAK_OBJ_TYPE_WORD:
			val = hak_oowtoint(hak, HAK_OBJ_GET_WORD_VAL(obj, index));
			if (HAK_UNLIKELY(!val)) return HAK_PF_FAILURE;
			break;

		default:
			goto unindexable;
	}


	HAK_STACK_SETRET(hak, nargs, val);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_core_basic_at (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	return __basic_at(hak, mod, nargs, 0);
}

static hak_pfrc_t pf_core_prim_at (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	return __basic_at(hak, mod, nargs, 1);
}

static hak_pfrc_t __basic_at_put (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs, int span_fixed)
{
	hak_oop_t obj, val;
	hak_oop_t pos;
	hak_oow_t index;
	hak_oop_class_t _class;

	obj = HAK_STACK_GETARG(hak, nargs, 0);
	pos = HAK_STACK_GETARG(hak, nargs, 1);
	val = HAK_STACK_GETARG(hak, nargs, 2);

	if (!HAK_OOP_IS_POINTER(obj) || !HAK_OBJ_GET_FLAGS_FLEXI(obj))
	{
	unindexable:
		hak_seterrbfmt (hak, HAK_EINVAL, "receiver not indexable - %O", obj);
		return HAK_PF_FAILURE;
	}

	if (HAK_OBJ_GET_FLAGS_RDONLY(obj))
	{
		hak_seterrbfmt (hak, HAK_EINVAL, "receiver immutable - %O", obj);
		return HAK_PF_FAILURE;
	}

	if (hak_inttooow_noseterr(hak, pos, &index) <= 0)
	{
		/* negative integer or not integer */
		hak_seterrbfmt (hak, HAK_EINVAL, "position not valid - %O", pos);
		return HAK_PF_FAILURE;
	}

	_class = (hak_oop_class_t)HAK_CLASSOF(hak, obj);
	if (span_fixed) /* include the fixed part in positioning */
	{
		hak_oow_t size;

		size = HAK_OBJ_GET_SIZE(obj);
		if (index >= size)
		{
			hak_seterrbfmt (hak, HAK_EINVAL, "position(%zd) out of range - negative or greater than or equal to %zu", index, (hak_ooi_t)size);
			return HAK_PF_FAILURE;
		}
	}
	else
	{
		hak_oow_t fixed, flexi;

		fixed = HAK_CLASS_SPEC_NAMED_INSTVARS(HAK_OOP_TO_SMOOI(_class->spec));
		flexi = HAK_OBJ_GET_SIZE(obj) - fixed;
		if (index >= flexi)
		{
			hak_seterrbfmt (hak, HAK_EINVAL, "position(%zd) out of range - negative or greater than or equal to %zu", index, (hak_ooi_t)HAK_OBJ_GET_SIZE(obj));
			return HAK_PF_FAILURE;
		}
		index += fixed;
	}

	switch (HAK_OBJ_GET_FLAGS_TYPE(obj))
	{
		case HAK_OBJ_TYPE_OOP:
			HAK_OBJ_SET_OOP_VAL(obj, index, val);
			break;

		case HAK_OBJ_TYPE_CHAR:
		{
			hak_ooch_t c;
			if (!HAK_OOP_IS_CHAR(val))
			{
				hak_seterrbfmt (hak, HAK_EINVAL, "value not character - %O", val);
				return HAK_PF_FAILURE;
			}
			c = HAK_OOP_TO_CHAR(val);
			HAK_OBJ_SET_CHAR_VAL(obj, index, c);
			break;
		}

		case HAK_OBJ_TYPE_BYTE:
		{
			hak_ooi_t b;
			if (!HAK_OOP_IS_SMOOI(val))
			{
				hak_seterrbfmt (hak, HAK_EINVAL, "value not byte - %O", val);
				return HAK_PF_FAILURE;
			}
			b = HAK_OOP_TO_SMOOI(val);
			HAK_OBJ_SET_BYTE_VAL(obj, index, b);
			break;
		}

		case HAK_OBJ_TYPE_HALFWORD:
		{
			hak_oow_t w;
			if (hak_inttooow(hak, val, &w) <= -1) return HAK_PF_FAILURE;
			HAK_OBJ_SET_HALFWORD_VAL(obj, index, w);
			break;
		}

		case HAK_OBJ_TYPE_WORD:
		{
			hak_oow_t w;
			if (hak_inttooow(hak, val, &w) <= -1) return HAK_PF_FAILURE;
			HAK_OBJ_SET_WORD_VAL(obj, index, w);
			break;
		}

		default:
			goto unindexable;
	}


	HAK_STACK_SETRET(hak, nargs, val);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_core_basic_at_put (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	return __basic_at_put(hak, mod, nargs, 0);
}

static hak_pfrc_t pf_core_prim_at_put (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	return __basic_at_put(hak, mod, nargs, 1);
}

static hak_pfrc_t pf_core_basic_size (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_oop_t src;
	hak_oop_t size;

	src = (hak_oop_oop_t)HAK_STACK_GETARG(hak, nargs, 0);

	if (!HAK_OOP_IS_POINTER(src))
	{
		hak_seterrbfmt (hak, HAK_EINVAL, "source not sizable - %O", src);
		return HAK_PF_FAILURE;
	}

	size = hak_oowtoint(hak, HAK_OBJ_GET_SIZE(src));
	if (!size) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, size);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_core_class_name (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t obj;

	obj = HAK_STACK_GETARG(hak, nargs, 0);

	if (!HAK_IS_CLASS(hak, obj))
	{
	#if 0
		hak_seterrbfmt (hak, HAK_EINVAL, "receiver not class - %O", obj);
		return HAK_PF_FAILURE;
	#else
		obj = (hak_oop_t)HAK_CLASSOF(hak, obj);
		HAK_ASSERT (hak, HAK_IS_CLASS(hak, obj));
	#endif
	}

	HAK_STACK_SETRET(hak, nargs, ((hak_oop_class_t)obj)->name);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_core_class_responds_to (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t obj;
	hak_oop_t msg;
	int x;

	obj = HAK_STACK_GETARG(hak, nargs, 0);
	msg = HAK_STACK_GETARG(hak, nargs, 1);
	if (!HAK_IS_CLASS(hak, obj))
	{
		hak_seterrbfmt (hak, HAK_EINVAL, "receiver not class - %O", msg);
		return HAK_PF_FAILURE;
	}
	if (!HAK_OBJ_IS_CHAR_POINTER(msg))
	{
		hak_seterrbfmt (hak, HAK_EINVAL, "invalid message - %O", msg);
		return HAK_PF_FAILURE;
	}

	x = hak_class_responds_to(hak, obj, msg);
	HAK_STACK_SETRET(hak, nargs, (x? hak->_true: hak->_false));
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_core_inst_responds_to (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t obj;
	hak_oop_t msg;
	int x;

	obj = HAK_STACK_GETARG(hak, nargs, 0);
	msg = HAK_STACK_GETARG(hak, nargs, 1);
	if (!HAK_OBJ_IS_CHAR_POINTER(msg))
	{
		hak_seterrbfmt (hak, HAK_EINVAL, "invalid message - %O", msg);
		return HAK_PF_FAILURE;
	}

	x = hak_inst_responds_to(hak, obj, msg);
	HAK_STACK_SETRET(hak, nargs, (x? hak->_true: hak->_false));
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_core_slice (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t src, slice, a1, a2;
	hak_ooi_t size;
	hak_ooi_t pos;
	hak_ooi_t len;
	hak_ooi_t i;

	src = HAK_STACK_GETARG(hak, nargs, 0);
	a1 = HAK_STACK_GETARG(hak, nargs, 1);
	a2 = HAK_STACK_GETARG(hak, nargs, 2);

	if (!HAK_OOP_IS_POINTER(src))
	{
	unsliceable:
		hak_seterrbfmt (hak, HAK_EINVAL, "source not sliceable - %O", src);
		return HAK_PF_FAILURE;
	}

	if (!HAK_OOP_IS_SMOOI(a1))
	{
		hak_seterrbfmt (hak, HAK_EINVAL, "position not numeric - %O", a1);
		return HAK_PF_FAILURE;
	}
	if (!HAK_OOP_IS_SMOOI(a2))
	{
		hak_seterrbfmt (hak, HAK_EINVAL, "length not numeric - %O", a2);
		return HAK_PF_FAILURE;
	}

	size = HAK_OBJ_GET_SIZE(src);
	pos = HAK_OOP_TO_SMOOI(a1);
	len = HAK_OOP_TO_SMOOI(a2);

	if (pos < 0) pos = 0;
	else if (pos >= size) pos = size;
	if (len >= size - pos) len = size - pos;

/* TODO: check if the object is an indexable object from the class spec... */
	/* use HAK_OBJ_GET_CLASS() instead of HAK_CLASSOF() as we know it's an object */
	slice = hak_instantiate(hak, (hak_oop_class_t)HAK_OBJ_GET_CLASS(src), HAK_NULL, len);
	if (HAK_UNLIKELY(!slice)) return HAK_PF_FAILURE;

/* OR if add by the number of fixed fields??? */
	switch (HAK_OBJ_GET_FLAGS_TYPE(src))
	{
		case HAK_OBJ_TYPE_OOP:
			for (i = 0; i < len; i++) HAK_OBJ_GET_OOP_VAL(slice, i) = HAK_OBJ_GET_OOP_VAL(src, pos + i);
			break;

		case HAK_OBJ_TYPE_CHAR:
			for (i = 0; i < len; i++) HAK_OBJ_GET_CHAR_VAL(slice, i) = HAK_OBJ_GET_CHAR_VAL(src, pos + i);
			break;

		case HAK_OBJ_TYPE_BYTE:
			for (i = 0; i < len; i++) HAK_OBJ_GET_BYTE_VAL(slice, i) = HAK_OBJ_GET_BYTE_VAL(src, pos + i);
			break;

		case HAK_OBJ_TYPE_HALFWORD:
			for (i = 0; i < len; i++) HAK_OBJ_GET_HALFWORD_VAL(slice, i) = HAK_OBJ_GET_HALFWORD_VAL(src, pos + i);
			break;

		case HAK_OBJ_TYPE_WORD:
			for (i = 0; i < len; i++) HAK_OBJ_GET_WORD_VAL(slice, i) = HAK_OBJ_GET_WORD_VAL(src, pos + i);
			break;

		default:
			goto unsliceable;
	}

	HAK_STACK_SETRET(hak, nargs, slice);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_core_char_to_smooi (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rcv, out;
	hak_ooi_t code;

	rcv = HAK_STACK_GETARG(hak, nargs, 0);
	if (!HAK_OOP_IS_CHAR(rcv))
	{
		hak_seterrbfmt (hak, HAK_EINVAL, "receiver not Character - %O", rcv);
		return HAK_PF_FAILURE;
	}

	code = HAK_OOP_TO_CHAR(rcv);
	out = HAK_SMOOI_TO_OOP(code);
	HAK_STACK_SETRET(hak, nargs, out);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_core_smooi_to_char (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t rcv, out;
	hak_ooi_t code;

	rcv = HAK_STACK_GETARG(hak, nargs, 0);
	if (!HAK_OOP_IS_SMOOI(rcv))
	{
		hak_seterrbfmt (hak, HAK_EINVAL, "receiver not SmallInteger - %O", rcv);
		return HAK_PF_FAILURE;
	}

	code = HAK_OOP_TO_SMOOI(rcv);
	out = HAK_CHAR_TO_OOP(code);
	HAK_STACK_SETRET(hak, nargs, out);
	return HAK_PF_SUCCESS;
}

static hak_pfinfo_t pfinfos[] =
{
	{ "*",                  { HAK_PFBASE_FUNC, hak_pf_number_mul,             1, HAK_TYPE_MAX(hak_oow_t) } },
	{ "+",                  { HAK_PFBASE_FUNC, hak_pf_number_add,             1, HAK_TYPE_MAX(hak_oow_t) } },
	{ "-",                  { HAK_PFBASE_FUNC, hak_pf_number_sub,             1, HAK_TYPE_MAX(hak_oow_t) } },
	{ "/",                  { HAK_PFBASE_FUNC, hak_pf_number_div,             1, HAK_TYPE_MAX(hak_oow_t) } },
	{ "<",                  { HAK_PFBASE_FUNC, hak_pf_number_lt,              2, 2 } },
	{ "<=",                 { HAK_PFBASE_FUNC, hak_pf_number_le,              2, 2 } },
	{ "=",                  { HAK_PFBASE_FUNC, hak_pf_number_eq,              2, 2 } },
	{ "==",                 { HAK_PFBASE_FUNC, hak_pf_number_eq,              2, 2 } },
	{ ">",                  { HAK_PFBASE_FUNC, hak_pf_number_gt,              2, 2 } },
	{ ">=",                 { HAK_PFBASE_FUNC, hak_pf_number_ge,              2, 2 } },

/* TODO: add more builtin primitives here... */
	{ "abs",                { HAK_PFBASE_FUNC, hak_pf_number_abs,             1,  1 } },
	{ "basicAt",            { HAK_PFBASE_FUNC, pf_core_basic_at,              2,  2 } },
	{ "basicAtPut",         { HAK_PFBASE_FUNC, pf_core_basic_at_put,          3,  3 } },
	{ "basicNew",           { HAK_PFBASE_FUNC, pf_core_basic_new,             1,  2 } },
	{ "basicSize",          { HAK_PFBASE_FUNC, pf_core_basic_size,            1,  1 } },

	{ "bit-and",            { HAK_PFBASE_FUNC, hak_pf_integer_band,           2,  2 } },
	{ "bit-left-shift",     { HAK_PFBASE_FUNC, hak_pf_integer_blshift,        2,  2 } },
	{ "bit-not",            { HAK_PFBASE_FUNC, hak_pf_integer_bnot,           1,  1 } },
	{ "bit-or",             { HAK_PFBASE_FUNC, hak_pf_integer_bor,            2,  2 } },
	{ "bit-right-shift",    { HAK_PFBASE_FUNC, hak_pf_integer_brshift,        2,  2 } },
	{ "bit-shift",          { HAK_PFBASE_FUNC, hak_pf_integer_bshift,         2,  2 } },
	{ "bit-xor",            { HAK_PFBASE_FUNC, hak_pf_integer_bxor,           2,  2 } },

	{ "charToSmooi",        { HAK_PFBASE_FUNC, pf_core_char_to_smooi,         1,  1 } },
	{ "className",          { HAK_PFBASE_FUNC, pf_core_class_name,            1,  1 } },
	{ "classRespondsTo",    { HAK_PFBASE_FUNC, pf_core_class_responds_to,     2,  2 } },

	{ "eqk?",               { HAK_PFBASE_FUNC, hak_pf_eqk,                    2,  2 } },
	{ "eql?",               { HAK_PFBASE_FUNC, hak_pf_eql,                    2,  2 } },
	{ "eqv?",               { HAK_PFBASE_FUNC, hak_pf_eqv,                    2,  2 } },

	{ "instRespondsTo",     { HAK_PFBASE_FUNC, pf_core_inst_responds_to,      2,  2 } },

	{ "nqk?",               { HAK_PFBASE_FUNC, hak_pf_nqk,                    2,  2 } },
	{ "nql?",               { HAK_PFBASE_FUNC, hak_pf_nql,                    2,  2 } },
	{ "nqv?",               { HAK_PFBASE_FUNC, hak_pf_nqv,                    2,  2 } },

	{ "primAt",             { HAK_PFBASE_FUNC, pf_core_prim_at,               2,  2 } },
	{ "primAtPut",          { HAK_PFBASE_FUNC, pf_core_prim_at_put,           3,  3 } },
	{ "slice",              { HAK_PFBASE_FUNC, pf_core_slice,                 3,  3 } },
	{ "smooiToChar",        { HAK_PFBASE_FUNC, pf_core_smooi_to_char,         1,  1 } },
	{ "sqrt",               { HAK_PFBASE_FUNC, hak_pf_number_sqrt,            1,  1 } },
	{ "~=",                 { HAK_PFBASE_FUNC, hak_pf_number_ne,              2,  2 } },
};

/* ------------------------------------------------------------------------ */

static hak_pfbase_t* query (hak_t* hak, hak_mod_t* mod, const hak_ooch_t* name, hak_oow_t namelen)
{
	return hak_findpfbase(hak, pfinfos, HAK_COUNTOF(pfinfos), name, namelen);
}

static void unload (hak_t* hak, hak_mod_t* mod)
{
}

int hak_mod_core (hak_t* hak, hak_mod_t* mod)
{
	mod->query = query;
	mod->unload = unload; 
	mod->ctx = HAK_NULL;
	return 0;
}
