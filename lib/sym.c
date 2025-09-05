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

static hak_oop_oop_t expand_bucket (hak_t* hak, hak_oop_oop_t oldbuc)
{
	hak_oop_oop_t newbuc;
	hak_oow_t oldsz, newsz, index;
	hak_oop_char_t symbol;

	oldsz = HAK_OBJ_GET_SIZE(oldbuc);

/* TODO: better growth policy? */
	if (oldsz < 5000) newsz = oldsz + oldsz;
	else if (oldsz < 50000) newsz = oldsz + (oldsz / 2);
	else if (oldsz < 100000) newsz = oldsz + (oldsz / 4);
	else if (oldsz < 200000) newsz = oldsz + (oldsz / 8);
	else if (oldsz < 400000) newsz = oldsz + (oldsz / 16);
	else if (oldsz < 800000) newsz = oldsz + (oldsz / 32);
	else if (oldsz < 1600000) newsz = oldsz + (oldsz / 64);
	else
	{
		hak_oow_t inc, inc_max;

		inc = oldsz / 128;
		inc_max = HAK_OBJ_SIZE_MAX - oldsz;
		if (inc > inc_max)
		{
			if (inc_max > 0) inc = inc_max;
			else
			{
				hak_seterrnum(hak, HAK_EOOMEM);
				return HAK_NULL;
			}
		}
		newsz = oldsz + inc;
	}

	hak_pushvolat(hak, (hak_oop_t*)&oldbuc);
	newbuc = (hak_oop_oop_t)hak_makearray(hak, newsz);
	hak_popvolat (hak);
	if (!newbuc) return HAK_NULL;

	while (oldsz > 0)
	{
		symbol = (hak_oop_char_t)oldbuc->slot[--oldsz];
		if ((hak_oop_t)symbol != hak->_nil)
		{
			HAK_ASSERT(hak, HAK_IS_SYMBOL(hak, symbol));
			/*HAK_ASSERT(hak, sym->size > 0);*/

			index = hak_hash_oochars(symbol->slot, HAK_OBJ_GET_SIZE(symbol)) % newsz;
			while (newbuc->slot[index] != hak->_nil) index = (index + 1) % newsz;
			newbuc->slot[index] = (hak_oop_t)symbol;
		}
	}

	return newbuc;
}

static hak_oop_t find_or_make_symbol (hak_t* hak, const hak_ooch_t* ptr, hak_oow_t len, int create)
{
	hak_ooi_t tally;
	hak_oow_t index;
	hak_oop_char_t sym;

	HAK_ASSERT(hak, len > 0);
	if (len <= 0)
	{
		/* i don't allow an empty symbol name */
		hak_seterrnum(hak, HAK_EINVAL);
		return HAK_NULL;
	}

	HAK_ASSERT(hak, HAK_IS_ARRAY(hak, hak->symtab->bucket));
	index = hak_hash_oochars(ptr, len) % HAK_OBJ_GET_SIZE(hak->symtab->bucket);

	/* find a matching symbol in the open-addressed symbol table */
	while (hak->symtab->bucket->slot[index] != hak->_nil)
	{
		sym = (hak_oop_char_t)hak->symtab->bucket->slot[index];
		HAK_ASSERT(hak, HAK_IS_SYMBOL(hak, sym));

		if (len == HAK_OBJ_GET_SIZE(sym) &&
		    hak_equal_oochars(ptr, sym->slot, len))
		{
			return (hak_oop_t)sym;
		}

		index = (index + 1) % HAK_OBJ_GET_SIZE(hak->symtab->bucket);
	}

	if (!create)
	{
		hak_seterrnum(hak, HAK_ENOENT);
		return HAK_NULL;
	}

	/* make a new symbol and insert it */
	HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(hak->symtab->tally));
	tally = HAK_OOP_TO_SMOOI(hak->symtab->tally);
	if (tally >= HAK_SMOOI_MAX)
	{
		/* this built-in table is not allowed to hold more than
		 * HAK_SMOOI_MAX items for efficiency sake */
		hak_seterrnum(hak, HAK_EDFULL);
		return HAK_NULL;
	}

	/* no conversion to hak_oow_t is necessary for tally + 1.
	 * the maximum value of tally is checked to be HAK_SMOOI_MAX - 1.
	 * tally + 1 can produce at most HAK_SMOOI_MAX. above all,
	 * HAK_SMOOI_MAX is way smaller than HAK_TYPE_MAX(hak_ooi_t). */
	if (tally + 1 >= HAK_OBJ_GET_SIZE(hak->symtab->bucket))
	{
		hak_oop_oop_t bucket;

		/* TODO: make the growth policy configurable instead of growing
		         it just before it gets full. The polcy can be grow it
		         if it's 70% full */

		/* enlarge the symbol table before it gets full to
		 * make sure that it has at least one free slot left
		 * after having added a new symbol. this is to help
		 * traversal end at a _nil slot if no entry is found. */
		bucket = expand_bucket(hak, hak->symtab->bucket);
		if (!bucket) return HAK_NULL;

		hak->symtab->bucket = bucket;

		/* recalculate the index for the expanded bucket */
		index = hak_hash_oochars(ptr, len) % HAK_OBJ_GET_SIZE(hak->symtab->bucket);

		while (hak->symtab->bucket->slot[index] != hak->_nil)
			index = (index + 1) % HAK_OBJ_GET_SIZE(hak->symtab->bucket);
	}

	/* create a new symbol since it isn't found in the symbol table */
	/*sym = (hak_oop_char_t)hak_alloccharobj(hak, HAK_BRAND_SYMBOL, ptr, len);*/
	sym = (hak_oop_char_t)hak_instantiate(hak, hak->c_symbol, ptr, len);
	if (HAK_LIKELY(sym))
	{
		HAK_ASSERT(hak, tally < HAK_SMOOI_MAX);
		hak->symtab->tally = HAK_SMOOI_TO_OOP(tally + 1);
		hak->symtab->bucket->slot[index] = (hak_oop_t)sym;
	}
	else
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt(hak, HAK_ERRNUM(hak),
			"unable to instantiate %O with %.*js - %js", hak->c_symbol->name, len, ptr, orgmsg);
	}
	return (hak_oop_t)sym;
}

hak_oop_t hak_makesymbol (hak_t* hak, const hak_ooch_t* ptr, hak_oow_t len)
{
	return find_or_make_symbol(hak, ptr, len, 1);
}

hak_oop_t hak_findsymbol (hak_t* hak, const hak_ooch_t* ptr, hak_oow_t len)
{
	return find_or_make_symbol(hak, ptr, len, 0);
}

hak_oop_t hak_makesymbolwithbcstr (hak_t* hak, const hak_bch_t* ptr)
{
#if defined(HAK_OOCH_IS_UCH)
	hak_uch_t* ucsptr;
	hak_oow_t ucslen;
	hak_oop_t v;
/* TODO: no duplication? */
	ucsptr = hak_dupbtoucstr(hak, ptr, &ucslen);
	if (HAK_UNLIKELY(!ucsptr)) return HAK_NULL;
	v = hak_makesymbol(hak, ucsptr, ucslen);
	hak_freemem(hak, ucsptr);
	return v;
#else
	return hak_makesymbol(hak, ptr, hak_count_bcstr(ptr));
#endif
}

hak_oop_t hak_makesymbolwithucstr (hak_t* hak, const hak_uch_t* ptr)
{
#if defined(HAK_OOCH_IS_UCH)
	return hak_makesymbol(hak, ptr, hak_count_ucstr(ptr));
#else
	hak_uch_t* bcsptr;
	hak_oow_t bcslen;
	hak_oop_t v;
/* TODO: no duplication? */
	bcsptr = hak_duputobcstr(hak, ptr, &bcslen);
	if (HAK_UNLIKELY(!bcsptr)) return HAK_NULL;
	v = hak_makesymbol(hak, bcsptr, bcslen);
	hak_freemem(hak, bcsptr);
	return v;
#endif
}
