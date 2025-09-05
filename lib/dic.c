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

/* The dictionary functions in this file are used for storing
 * a dictionary object enclosed in {}. So putting a non-symbol
 * key is allowed like { 1 2 3 4 } where 1 and 3 are keys.
 * so SYMBOL_ONLY_KEY must not be defined */
/*#define SYMBOL_ONLY_KEY*/

static hak_oop_oop_t expand_bucket (hak_t* hak, hak_oop_oop_t oldbuc)
{
	hak_oop_oop_t newbuc;
	hak_oow_t oldsz, newsz, index;
	hak_oop_cons_t ass;

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
		ass = (hak_oop_cons_t)oldbuc->slot[--oldsz];
		if ((hak_oop_t)ass != hak->_nil)
		{
		#if defined(SYMBOL_ONLY_KEY)
			hak_oop_char_t key;
			HAK_ASSERT(hak, HAK_IS_CONS(hak,ass));
			key = (hak_oop_char_t)ass->car;
			HAK_ASSERT(hak, HAK_IS_SYMBOL(hak,key));
			index = hak_hash_oochars(key->slot, HAK_OBJ_GET_SIZE(key)) % newsz;
		#else
			int n;
			HAK_ASSERT(hak, HAK_IS_CONS(hak,ass));
			n = hak_hashobj(hak, ass->car, &index);
			HAK_ASSERT(hak, n == 0); /* since it's expanding, the existing one in the bucket should always be hashable */
			index %= newsz;
		#endif
			while (newbuc->slot[index] != hak->_nil) index = (index + 1) % newsz;
			newbuc->slot[index] = (hak_oop_t)ass;
		}
	}

	return newbuc;
}

static hak_oop_cons_t find_or_upsert (hak_t* hak, hak_oop_dic_t dic, hak_oop_t key, hak_oop_t value, int is_method)
{
	hak_ooi_t tally;
	hak_oow_t index;
	hak_oop_cons_t ass;
	hak_oow_t tmp_count = 0;

	/* the system dictionary is not a generic dictionary.
	 * it accepts only a symbol as a key. */
#if defined(SYMBOL_ONLY_KEY)
	HAK_ASSERT(hak, HAK_IS_SYMBOL(hak,key));
#endif
	HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(dic->tally));
	HAK_ASSERT(hak, HAK_IS_ARRAY(hak,dic->bucket));

#if defined(SYMBOL_ONLY_KEY)
	index = hak_hash_oochars(HAK_OBJ_GET_CHAR_SLOT(key), HAK_OBJ_GET_SIZE(key)) % HAK_OBJ_GET_SIZE(dic->bucket);
#else
	if (hak_hashobj(hak, key, &index) <= -1) return HAK_NULL;
	index %= HAK_OBJ_GET_SIZE(dic->bucket);
#endif

	/* find */
	while (dic->bucket->slot[index] != hak->_nil)
	{
#if defined(SYMBOL_ONLY_KEY)
		/* nothing */
#else
		int n;
#endif

		ass = (hak_oop_cons_t)dic->bucket->slot[index];
		HAK_ASSERT(hak, HAK_IS_CONS(hak,ass));

#if defined(SYMBOL_ONLY_KEY)
		HAK_ASSERT(hak, HAK_IS_SYMBOL(hak,ass->car));
		if (HAK_OBJ_GET_SIZE(key) == HAK_OBJ_GET_SIZE(ass->car) &&
		    hak_equal_oochars(HAK_OBJ_GET_CHAR_SLOT(key), HAK_OBJ_GET_CHAR_SLOT(ass->car), HAK_OBJ_GET_SIZE(key)))
#else
		n = hak_equalobjs(hak, key, ass->car);
		if (n <= -1) return HAK_NULL;
		if (n >= 1)
#endif
		{
			/* the value of HAK_NULL indicates no insertion or update. */
			if (value)
			{
				if (is_method)
				{
					hak_oop_cons_t pair;
					pair = (hak_oop_cons_t)ass->cdr; /* once found, this must be a pair of method pointers  */
					HAK_ASSERT(hak, HAK_IS_CONS(hak, pair));
					HAK_ASSERT(hak, HAK_IS_COMPILED_BLOCK(hak, value));
					if (is_method & 1) pair->car = value; /* class method */
					if (is_method & 2) pair->cdr = value; /* instance method */
					/* the class instantiation method goes to both cells.
					 * you can't define a class method or an instance method with the name of
					 * a class instantiation method */
				}
				else ass->cdr = value; /* normal update */
			}
			return ass;
		}

		index = (index + 1) % HAK_OBJ_GET_SIZE(dic->bucket);
	}

	if (!value)
	{
		/* when value is HAK_NULL, perform no insertion.
		 * the value of HAK_NULL indicates no insertion or update. */
		hak_seterrbfmt(hak, HAK_ENOENT, "key not found - %O", key);
		return HAK_NULL;
	}

	/* the key is not found. insert it. */
	HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(dic->tally));
	tally = HAK_OOP_TO_SMOOI(dic->tally);
	if (tally >= HAK_SMOOI_MAX)
	{
		/* this built-in dictionary is not allowed to hold more than
		 * HAK_SMOOI_MAX items for efficiency sake */
		hak_seterrnum(hak, HAK_EDFULL);
		return HAK_NULL;
	}

	hak_pushvolat(hak, (hak_oop_t*)&dic); tmp_count++;
	hak_pushvolat(hak, (hak_oop_t*)&key); tmp_count++;
	hak_pushvolat(hak, &value); tmp_count++;

	/* no conversion to hak_oow_t is necessary for tally + 1.
	 * the maximum value of tally is checked to be HAK_SMOOI_MAX - 1.
	 * tally + 1 can produce at most HAK_SMOOI_MAX. above all,
	 * HAK_SMOOI_MAX is way smaller than HAK_TYPE_MAX(hak_ooi_t). */
	if (tally + 1 >= HAK_OBJ_GET_SIZE(dic->bucket))
	{
		hak_oop_oop_t bucket;

		/* TODO: make the growth policy configurable instead of growing
		         it just before it gets full. The polcy can be grow it
		         if it's 70% full */

		/* enlarge the bucket before it gets full to
		 * make sure that it has at least one free slot left
		 * after having added a new symbol. this is to help
		 * traversal end at a _nil slot if no entry is found. */
		bucket = expand_bucket(hak, dic->bucket);
		if (!bucket) goto oops;

		dic->bucket = bucket;

#if defined(SYMBOL_ONLY_KEY)
		/* recalculate the index for the expanded bucket */
		index = hak_hash_oochars(HAK_OBJ_GET_CHAR_SLOT(key), HAK_OBJ_GET_SIZE(key)) % HAK_OBJ_GET_SIZE(dic->bucket);
#else
		hak_hashobj(hak, key, &index); /* this must succeed as i know 'key' is hashable */
		index %= HAK_OBJ_GET_SIZE(dic->bucket);
#endif
		while (dic->bucket->slot[index] != hak->_nil)
			index = (index + 1) % HAK_OBJ_GET_SIZE(dic->bucket);
	}

	if (is_method)
	{
		/* create a new pair that holds a class method at the first cell and an instance method at the second cell */
		hak_oop_t pair;
		HAK_ASSERT(hak, HAK_IS_COMPILED_BLOCK(hak, value));
		hak_pushvolat(hak, &key);
		pair = hak_makecons(hak, (is_method & 1? value: hak->_nil), (is_method & 2? value: hak->_nil));
		hak_popvolat (hak);
		if (HAK_UNLIKELY(!pair)) goto oops;
		value = pair;
	}

	/* create a new assocation of a key and a value since
	 * the key isn't found in the root dictionary */
	ass = (hak_oop_cons_t)hak_makecons(hak, (hak_oop_t)key, value);
	if (HAK_UNLIKELY(!ass)) goto oops;

	/* the current tally must be less than the maximum value. otherwise,
	 * it overflows after increment below */
	HAK_ASSERT(hak, tally < HAK_SMOOI_MAX);
	dic->tally = HAK_SMOOI_TO_OOP(tally + 1);
	dic->bucket->slot[index] = (hak_oop_t)ass;

	hak_popvolats(hak, tmp_count);
	return ass;

oops:
	hak_popvolats(hak, tmp_count);
	return HAK_NULL;
}

static hak_oop_cons_t lookupdic_noseterr (hak_t* hak, hak_oop_dic_t dic, const hak_oocs_t* name)
{
	/* this is special version of hak_getatsysdic() that performs
	 * lookup using a plain symbol specified */

	hak_oow_t index;
	hak_oop_cons_t ass;

	HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(dic->tally));
	HAK_ASSERT(hak, HAK_IS_ARRAY(hak,dic->bucket));

	index = hak_hash_oochars(name->ptr, name->len) % HAK_OBJ_GET_SIZE(dic->bucket);

	while ((hak_oop_t)(ass = (hak_oop_cons_t)HAK_OBJ_GET_OOP_VAL(dic->bucket, index)) != hak->_nil)
	{
		HAK_ASSERT(hak, HAK_IS_CONS(hak,ass));
		if (HAK_IS_SYMBOL(hak, ass->car))
		{
			if (name->len == HAK_OBJ_GET_SIZE(ass->car) &&
			    hak_equal_oochars(name->ptr, HAK_OBJ_GET_CHAR_SLOT(ass->car), name->len))
			{
				return ass;
			}
		}

		index = (index + 1) % HAK_OBJ_GET_SIZE(dic->bucket);
	}


	/* when value is HAK_NULL, perform no insertion */

	/* hak_seterrXXX() is not called here. the dictionary lookup is very frequent
	 * and so is lookup failure. for instance, hak_findmethod() calls this over
	 * a class chain. there might be a failure at each class level. it's waste to
	 * set the error information whenever the failure occurs.
	 * the caller of this function must set the error information upon failure */
	return HAK_NULL;
}

static HAK_INLINE hak_oop_cons_t lookupdic (hak_t* hak, hak_oop_dic_t dic, const hak_oocs_t* name)
{
	hak_oop_cons_t ass = lookupdic_noseterr(hak, dic, name);
	if (!ass) hak_seterrbfmt(hak, HAK_ENOENT, "unable to find %.*js in a dictionary", name->len, name->ptr);
	return ass;
}

hak_oop_cons_t hak_lookupdicforsymbol_noseterr (hak_t* hak, hak_oop_dic_t dic, const hak_oocs_t* name)
{
	return lookupdic_noseterr(hak, dic, name);
}

hak_oop_cons_t hak_lookupdicforsymbol (hak_t* hak, hak_oop_dic_t dic, const hak_oocs_t* name)
{
	return lookupdic(hak, dic, name);
}


hak_oop_cons_t hak_putatsysdic (hak_t* hak, hak_oop_t key, hak_oop_t value)
{
#if defined(SYMBOL_ONLY_KEY)
	HAK_ASSERT(hak, HAK_IS_SYMBOL(hak,key));
#endif
	return find_or_upsert(hak, hak->sysdic, key, value, 0);
}

hak_oop_cons_t hak_getatsysdic (hak_t* hak, hak_oop_t key)
{
#if defined(SYMBOL_ONLY_KEY)
	HAK_ASSERT(hak, HAK_IS_SYMBOL(hak,key));
#endif
	return find_or_upsert(hak, hak->sysdic, key, HAK_NULL, 0);
}

hak_oop_cons_t hak_lookupsysdicforsymbol_noseterr (hak_t* hak, const hak_oocs_t* name)
{
	return lookupdic_noseterr(hak, hak->sysdic, name);
}

hak_oop_cons_t hak_lookupsysdicforsymbol (hak_t* hak, const hak_oocs_t* name)
{
	return lookupdic(hak, hak->sysdic, name);
}

int hak_zapatsysdic (hak_t* hak, hak_oop_t key)
{
#if defined(SYMBOL_ONLY_KEY)
	HAK_ASSERT(hak, HAK_IS_SYMBOL(hak,key));
#endif
	return hak_zapatdic(hak, hak->sysdic, key);
}

hak_oop_cons_t hak_putatdic (hak_t* hak, hak_oop_dic_t dic, hak_oop_t key, hak_oop_t value)
{
#if defined(SYMBOL_ONLY_KEY)
	HAK_ASSERT(hak, HAK_IS_SYMBOL(hak,key));
#endif
	return find_or_upsert(hak, dic, key, value, 0);
}

hak_oop_cons_t hak_putatdic_method (hak_t* hak, hak_oop_dic_t dic, hak_oop_t key, hak_oop_t value, int mtype)
{
#if defined(SYMBOL_ONLY_KEY)
	HAK_ASSERT(hak, HAK_IS_SYMBOL(hak,key));
#endif
	return find_or_upsert(hak, dic, key, value, mtype);
}

hak_oop_cons_t hak_getatdic (hak_t* hak, hak_oop_dic_t dic, hak_oop_t key)
{
#if defined(SYMBOL_ONLY_KEY)
	HAK_ASSERT(hak, HAK_IS_SYMBOL(hak,key));
#endif
	return find_or_upsert(hak, dic, key, HAK_NULL, 0);
}

int hak_zapatdic (hak_t* hak, hak_oop_dic_t dic, hak_oop_t key)
{
	hak_ooi_t tally;
	hak_oow_t index, bs, i, x, y, z;
	hak_oop_cons_t ass;

	tally = HAK_OOP_TO_SMOOI(dic->tally);
	bs = HAK_OBJ_GET_SIZE(dic->bucket);

	/* the system dictionary is not a generic dictionary.
	 * it accepts only a symbol as a key. */
#if defined(SYMBOL_ONLY_KEY)
	HAK_ASSERT(hak, HAK_IS_SYMBOL(hak,key));
#endif
	HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(dic->tally));
	HAK_ASSERT(hak, HAK_IS_ARRAY(hak,dic->bucket));

#if defined(SYMBOL_ONLY_KEY)
	index = hak_hash_oochars(HAK_OBJ_GET_CHAR_SLOT(key), HAK_OBJ_GET_SIZE(key)) % bs;
#else
	if (hak_hashobj(hak, key, &index) <= -1) return -1;
	index %= bs;
#endif

	/* find */
	while (dic->bucket->slot[index] != hak->_nil)
	{
	#if defined(SYMBOL_ONLY_KEY)
		ass = (hak_oop_cons_t)dic->bucket->slot[index];
		HAK_ASSERT(hak, HAK_IS_CONS(hak,ass));
		HAK_ASSERT(hak, HAK_IS_SYMBOL(hak,ass->car));

		if (HAK_OBJ_GET_SIZE(key) == HAK_OBJ_GET_SIZE(ass->car) &&
		    hak_equal_oochars(HAK_OBJ_GET_CHAR_SLOT(key), HAK_OBJ_GET_CHAR_SLOT(ass->car), HAK_OBJ_GET_SIZE(key)))
		{
			/* the value of HAK_NULL indicates no insertion or update. */
			goto found;
		}
	#else
		int n;

		ass = (hak_oop_cons_t)dic->bucket->slot[index];
		HAK_ASSERT(hak, HAK_IS_CONS(hak,ass));

		n = hak_equalobjs(hak, (hak_oop_t)key, ass->car);
		if (n <= -1) return -1;
		if (n >= 1) goto found;
	#endif

		index = (index + 1) % bs;
	}

	hak_seterrnum(hak, HAK_ENOENT);
	return -1;

found:
	/* compact the cluster */
	for (i = 0, x = index, y = index; i < tally; i++)
	{
		y = (y + 1) % bs;

		/* done if the slot at the current index is empty */
		if (dic->bucket->slot[y] == hak->_nil) break;

		ass = (hak_oop_cons_t)dic->bucket->slot[y];
	#if defined(SYMBOL_ONLY_KEY)
		/* get the natural hash index for the data in the slot at
		 * the current hash index */
		z = hak_hash_oochars(HAK_OBJ_GET_CHAR_SLOT(ass->car), HAK_OBJ_GET_SIZE(ass->car)) % bs;
	#else
		if (hak_hashobj(hak, ass->car, &z) <= -1) return -1;
		z %= bs;
	#endif

		/* move an element if necesary */
		if ((y > x && (z <= x || z > y)) ||
		    (y < x && (z <= x && z > y)))
		{
			dic->bucket->slot[x] = dic->bucket->slot[y];
			x = y;
		}
	}

	dic->bucket->slot[x] = hak->_nil;

	tally--;
	dic->tally = HAK_SMOOI_TO_OOP(tally);

	return 0;
}

hak_oop_t hak_makedic (hak_t* hak, hak_oow_t inisize)
{
#if 0
	hak_oop_dic_t obj;

	obj = (hak_oop_dic_t)hak_allocoopobj(hak, HAK_BRAND_DIC, 2);
	if (obj)
	{
		hak_oop_oop_t bucket;

		obj->tally = HAK_SMOOI_TO_OOP(0);

		hak_pushvolat(hak, (hak_oop_t*)&obj);
		bucket = (hak_oop_oop_t)hak_makearray(hak, inisize);
		hak_popvolat (hak);

		if (!bucket) obj = HAK_NULL;
		else obj->bucket = bucket;
	}

	return (hak_oop_t)obj;
#else
	hak_oop_dic_t v;

	v = (hak_oop_dic_t)hak_instantiate(hak, hak->c_dictionary, HAK_NULL, 0);
	if (HAK_UNLIKELY(!v))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt(hak, HAK_ERRNUM(hak),
			"unable to instantiate %O - %js", hak->c_dictionary->name, orgmsg);
	}
	else
	{
		hak_oop_oop_t bucket;

		v->tally = HAK_SMOOI_TO_OOP(0);

		hak_pushvolat(hak, (hak_oop_t*)&v);
		bucket = (hak_oop_oop_t)hak_makearray(hak, inisize);
		hak_popvolat (hak);

		if (HAK_UNLIKELY(!bucket))
		{
			/* TODO: can I remove the instanated object immediately above?
			 *       it must be ok as the dictionary object is never referenced.
			 *       some care must be taken not to screw up gc metadata...  */
			v = HAK_NULL;
		}
		else v->bucket = bucket;
	}

	return (hak_oop_t)v;
#endif
}

int hak_walkdic (hak_t* hak, hak_oop_dic_t dic, hak_dic_walker_t walker, void* ctx)
{
	hak_oow_t i;

	hak_pushvolat(hak, (hak_oop_t*)&dic);

	for (i = 0; i < HAK_OBJ_GET_SIZE(dic->bucket); i++)
	{
		hak_oop_t tmp = dic->bucket->slot[i];
		if (HAK_IS_CONS(hak, tmp) && walker(hak, dic, (hak_oop_cons_t)tmp, ctx) <= -1) return -1;
	}

	hak_popvolat (hak);
	return 0;
}

