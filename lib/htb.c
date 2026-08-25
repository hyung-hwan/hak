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

#include <hak-htb.h>
#include "hak-prv.h"

#define pair_t          hak_htb_pair_t
#define copier_t        hak_htb_copier_t
#define freeer_t        hak_htb_freeer_t
#define hasher_t        hak_htb_hasher_t
#define comper_t        hak_htb_comper_t
#define keeper_t        hak_htb_keeper_t
#define sizer_t         hak_htb_sizer_t
#define walker_t        hak_htb_walker_t
#define cbserter_t      hak_htb_cbserter_t
#define style_t         hak_htb_style_t
#define style_kind_t    hak_htb_style_kind_t

#define KPTR(p)  HAK_HTB_KPTR(p)
#define KLEN(p)  HAK_HTB_KLEN(p)
#define VPTR(p)  HAK_HTB_VPTR(p)
#define VLEN(p)  HAK_HTB_VLEN(p)
#define NEXT(p)  HAK_HTB_NEXT(p)

#define KTOB(htb,len) ((len) * (htb)->scale[HAK_HTB_KEY])
#define VTOB(htb,len) ((len) * (htb)->scale[HAK_HTB_VAL])

static HAK_INLINE_ALWAYS pair_t* alloc_pair (hak_htb_t* htb, void* kptr, hak_oow_t klen, void* vptr, hak_oow_t vlen)
{
	pair_t* n;
	copier_t kcop, vcop;
	hak_oow_t as;

	kcop = htb->style->copier[HAK_HTB_KEY];
	vcop = htb->style->copier[HAK_HTB_VAL];

	as = HAK_SIZEOF(pair_t);
	if (kcop == HAK_HTB_COPIER_INLINE) as += HAK_ALIGN_POW2(KTOB(htb,klen), HAK_SIZEOF_VOID_P);
	if (vcop == HAK_HTB_COPIER_INLINE) as += VTOB(htb,vlen);

	n = (pair_t*) hak_allocmem(htb->hak, as);
	if (HAK_UNLIKELY(!n)) return HAK_NULL;

	NEXT(n) = HAK_NULL;

	KLEN(n) = klen;
	if (kcop == HAK_HTB_COPIER_SIMPLE)
	{
		KPTR(n) = kptr;
	}
	else if (kcop == HAK_HTB_COPIER_INLINE)
	{
		KPTR(n) = n + 1;
		/* if kptr is HAK_NULL, the inline copier does not fill
		 * the actual key area */
		if (kptr) HAK_MEMCPY(KPTR(n), kptr, KTOB(htb,klen));
	}
	else
	{
		KPTR(n) = kcop(htb, kptr, klen);
		if (KPTR(n) == HAK_NULL)
		{
			hak_freemem(htb->hak, n);
			return HAK_NULL;
		}
	}

	VLEN(n) = vlen;
	if (vcop == HAK_HTB_COPIER_SIMPLE)
	{
		VPTR(n) = vptr;
	}
	else if (vcop == HAK_HTB_COPIER_INLINE)
	{
		VPTR(n) = n + 1;
		if (kcop == HAK_HTB_COPIER_INLINE)
			VPTR(n) = (hak_uint8_t*)VPTR(n) + HAK_ALIGN_POW2(KTOB(htb,klen), HAK_SIZEOF_VOID_P);
		/* if vptr is HAK_NULL, the inline copier does not fill
		 * the actual value area */
		if (vptr) HAK_MEMCPY(VPTR(n), vptr, VTOB(htb,vlen));
	}
	else
	{
		VPTR(n) = vcop(htb, vptr, vlen);
		if (VPTR(n) != HAK_NULL)
		{
			if (htb->style->freeer[HAK_HTB_KEY] != HAK_NULL)
				htb->style->freeer[HAK_HTB_KEY](htb, KPTR(n), KLEN(n));
			hak_freemem(htb->hak, n);
			return HAK_NULL;
		}
	}

	return n;
}

static HAK_INLINE_ALWAYS void free_pair (hak_htb_t* htb, pair_t* pair)
{
	if (htb->style->freeer[HAK_HTB_KEY] != HAK_NULL)
		htb->style->freeer[HAK_HTB_KEY](htb, KPTR(pair), KLEN(pair));
	if (htb->style->freeer[HAK_HTB_VAL] != HAK_NULL)
		htb->style->freeer[HAK_HTB_VAL](htb, VPTR(pair), VLEN(pair));
	hak_freemem(htb->hak, pair);
}

pair_t* hak_htb_allocpair (hak_htb_t* htb, void* kptr, hak_oow_t klen, void* vptr, hak_oow_t vlen)
{
	return alloc_pair(htb, kptr, klen, vptr, vlen);
}

void hak_htb_freepair (hak_htb_t* htb, pair_t* pair)
{
	free_pair(htb, pair);
}

static HAK_INLINE_ALWAYS pair_t* change_pair_val (hak_htb_t* htb, pair_t* pair, void* vptr, hak_oow_t vlen)
{
	if (VPTR(pair) == vptr && VLEN(pair) == vlen)
	{
		/* if the old value and the new value are the same,
		 * it just calls the handler for this condition.
		 * No value replacement occurs. */
		if (htb->style->keeper != HAK_NULL)
		{
			htb->style->keeper(htb, vptr, vlen);
		}
	}
	else
	{
		copier_t vcop = htb->style->copier[HAK_HTB_VAL];
		void* ovptr = VPTR(pair);
		hak_oow_t ovlen = VLEN(pair);

		/* place the new value according to the copier */
		if (vcop == HAK_HTB_COPIER_SIMPLE)
		{
			VPTR(pair) = vptr;
			VLEN(pair) = vlen;
		}
		else if (vcop == HAK_HTB_COPIER_INLINE)
		{
			if (ovlen == vlen)
			{
				if (vptr) HAK_MEMCPY(VPTR(pair), vptr, VTOB(htb,vlen));
			}
			else
			{
				/* need to reconstruct the pair */
				pair_t* p = alloc_pair(htb, KPTR(pair), KLEN(pair), vptr, vlen);
				if (HAK_UNLIKELY(!p)) return HAK_NULL;
				free_pair(htb, pair);
				return p;
			}
		}
		else
		{
			void* nvptr = vcop(htb, vptr, vlen);
			if (HAK_UNLIKELY(!nvptr)) return HAK_NULL;
			VPTR(pair) = nvptr;
			VLEN(pair) = vlen;
		}

		/* free up the old value */
		if (htb->style->freeer[HAK_HTB_VAL] != HAK_NULL)
		{
			htb->style->freeer[HAK_HTB_VAL](htb, ovptr, ovlen);
		}
	}

	return pair;
}

static style_t style[] =
{
    	/* == HAK_HTB_STYLE_DEFAULT == */
	{
		{
			HAK_HTB_COPIER_DEFAULT,
			HAK_HTB_COPIER_DEFAULT
		},
		{
			HAK_HTB_FREEER_DEFAULT,
			HAK_HTB_FREEER_DEFAULT
		},
		HAK_HTB_COMPER_DEFAULT,
		HAK_HTB_KEEPER_DEFAULT,
		HAK_HTB_SIZER_DEFAULT,
		HAK_HTB_HASHER_DEFAULT
	},

	/* == HAK_HTB_STYLE_INLINE_COPIERS == */
	{
		{
			HAK_HTB_COPIER_INLINE,
			HAK_HTB_COPIER_INLINE
		},
		{
			HAK_HTB_FREEER_DEFAULT,
			HAK_HTB_FREEER_DEFAULT
		},
		HAK_HTB_COMPER_DEFAULT,
		HAK_HTB_KEEPER_DEFAULT,
		HAK_HTB_SIZER_DEFAULT,
		HAK_HTB_HASHER_DEFAULT
	},

	/* == HAK_HTB_STYLE_INLINE_KEY_COPIER == */
	{
		{
			HAK_HTB_COPIER_INLINE,
			HAK_HTB_COPIER_DEFAULT
		},
		{
			HAK_HTB_FREEER_DEFAULT,
			HAK_HTB_FREEER_DEFAULT
		},
		HAK_HTB_COMPER_DEFAULT,
		HAK_HTB_KEEPER_DEFAULT,
		HAK_HTB_SIZER_DEFAULT,
		HAK_HTB_HASHER_DEFAULT
	},

	/* == HAK_HTB_STYLE_INLINE_VALUE_COPIER == */
	{
		{
			HAK_HTB_COPIER_DEFAULT,
			HAK_HTB_COPIER_INLINE
		},
		{
			HAK_HTB_FREEER_DEFAULT,
			HAK_HTB_FREEER_DEFAULT
		},
		HAK_HTB_COMPER_DEFAULT,
		HAK_HTB_KEEPER_DEFAULT,
		HAK_HTB_SIZER_DEFAULT,
		HAK_HTB_HASHER_DEFAULT
	}
};

const style_t* hak_get_htb_style (style_kind_t kind)
{
	return &style[kind];
}

hak_htb_t* hak_htb_open (hak_t* hak, hak_oow_t xtnsize, hak_oow_t capa, int factor, int kscale, int vscale)
{
	hak_htb_t* htb;

	htb = (hak_htb_t*)hak_allocmem(hak, HAK_SIZEOF(hak_htb_t) + xtnsize);
	if (HAK_UNLIKELY(!htb)) return HAK_NULL;

	if (hak_htb_init(htb, hak, capa, factor, kscale, vscale) <= -1)
	{
		hak_freemem(hak, htb);
		return HAK_NULL;
	}

	HAK_MEMSET(htb + 1, 0, xtnsize);
	return htb;
}

void hak_htb_close (hak_htb_t* htb)
{
	hak_htb_fini(htb);
	hak_freemem(htb->hak, htb);
}

int hak_htb_init (hak_htb_t* htb, hak_t* hak, hak_oow_t capa, int factor, int kscale, int vscale)
{
	/* The initial capacity should be greater than 0.
	 * Otherwise, it is adjusted to 1 in the release mode */
	HAK_ASSERT(hak, capa > 0);

	/* The load factor should be between 0 and 100 inclusive.
	 * In the release mode, a value out of the range is adjusted to 100 */
	HAK_ASSERT(hak, factor >= 0 && factor <= 100);

	HAK_ASSERT(hak, kscale >= 0 && kscale <= HAK_TYPE_MAX(hak_uint8_t));
	HAK_ASSERT(hak, vscale >= 0 && vscale <= HAK_TYPE_MAX(hak_uint8_t));

	/* some initial adjustment */
	if (capa <= 0) capa = 1;
	if (factor > 100) factor = 100;

	/* do not zero out the extension */
	HAK_MEMSET(htb, 0, HAK_SIZEOF(*htb));
	htb->hak = hak;

	htb->bucket = hak_allocmem(hak, capa * HAK_SIZEOF(pair_t*));
	if (HAK_UNLIKELY(!htb->bucket)) return -1;

	/*for (i = 0; i < capa; i++) htb->bucket[i] = HAK_NULL;*/
	HAK_MEMSET(htb->bucket, 0, capa * HAK_SIZEOF(pair_t*));

	htb->factor = factor;
	htb->scale[HAK_HTB_KEY] = (kscale < 1)? 1: kscale;
	htb->scale[HAK_HTB_VAL] = (vscale < 1)? 1: vscale;

	htb->size = 0;
	htb->capa = capa;
	htb->threshold = htb->capa * htb->factor / 100;
	if (htb->capa > 0 && htb->threshold <= 0) htb->threshold = 1;
	htb->rev = 0;

	htb->style = &style[0];
	return 0;
}

void hak_htb_fini (hak_htb_t* htb)
{
	hak_htb_clear(htb);
	hak_freemem(htb->hak, htb->bucket);
}

const style_t* hak_htb_getstyle (const hak_htb_t* htb)
{
	return htb->style;
}

void hak_htb_setstyle (hak_htb_t* htb, const style_t* style)
{
	HAK_ASSERT(htb->hak, style != HAK_NULL);
	htb->style = style;
}

hak_oow_t hak_htb_getsize (const hak_htb_t* htb)
{
	return htb->size;
}

hak_oow_t hak_htb_getcapa (const hak_htb_t* htb)
{
	return htb->capa;
}

hak_oow_t hak_htb_getrev (const hak_htb_t* htb)
{
	return htb->rev;
}

pair_t* hak_htb_search (const hak_htb_t* htb, const void* kptr, hak_oow_t klen)
{
	pair_t* pair;
	hak_oow_t hc;

	hc = htb->style->hasher(htb,kptr,klen) % htb->capa;
	pair = htb->bucket[hc];

	while (pair != HAK_NULL)
	{
		if (htb->style->comper(htb, KPTR(pair), KLEN(pair), kptr, klen) == 0)
		{
			return pair;
		}

		pair = NEXT(pair);
	}

	hak_seterrnum(htb->hak, HAK_ENOENT);
	return HAK_NULL;
}

static HAK_INLINE int reorganize (hak_htb_t* htb)
{
	hak_oow_t i, hc, new_capa;
	pair_t** new_buck;

	if (htb->style->sizer)
	{
		new_capa = htb->style->sizer(htb, htb->capa + 1);

		/* if no change in capacity, return success
		 * without reorganization */
		if (new_capa == htb->capa) return 0;

		/* adjust to 1 if the new capacity is not reasonable */
		if (new_capa <= 0) new_capa = 1;
	}
	else
	{
		/* the bucket is doubled until it grows up to 65536 slots.
		 * once it has reached it, it grows by 65536 slots */
		new_capa = (htb->capa >= 65536)? (htb->capa + 65536): (htb->capa << 1);
	}

	new_buck = (pair_t**)hak_allocmem(htb->hak, new_capa * HAK_SIZEOF(pair_t*));
	if (HAK_UNLIKELY(!new_buck))
	{
		/* reorganization is disabled once it fails */
		htb->threshold = 0;
		return -1;
	}

	/*for (i = 0; i < new_capa; i++) new_buck[i] = HAK_NULL;*/
	HAK_MEMSET(new_buck, 0, new_capa * HAK_SIZEOF(pair_t*));

	for (i = 0; i < htb->capa; i++)
	{
		pair_t* pair = htb->bucket[i];

		while (pair != HAK_NULL)
		{
			pair_t* next = NEXT(pair);

			hc = htb->style->hasher(htb, KPTR(pair), KLEN(pair)) % new_capa;

			NEXT(pair) = new_buck[hc];
			new_buck[hc] = pair;

			pair = next;
		}
	}

	hak_freemem(htb->hak, htb->bucket);
	htb->bucket = new_buck;
	htb->capa = new_capa;
	htb->threshold = htb->capa * htb->factor / 100;

	return 0;
}

/* insert options */
#define UPSERT 1
#define UPDATE 2
#define ENSERT 3
#define INSERT 4

static HAK_INLINE_ALWAYS pair_t* insert (hak_htb_t* htb, void* kptr, hak_oow_t klen, void* vptr, hak_oow_t vlen, int opt)
{
	pair_t* pair, * p, * prev, * next;
	hak_oow_t hc;

	hc = htb->style->hasher(htb,kptr,klen) % htb->capa;
	pair = htb->bucket[hc];
	prev = HAK_NULL;

	while (pair != HAK_NULL)
	{
		next = NEXT(pair);

		if (htb->style->comper(htb, KPTR(pair), KLEN(pair), kptr, klen) == 0)
		{
			/* found a pair with a matching key */
			switch (opt)
			{
				case UPSERT:
				case UPDATE:
					p = change_pair_val(htb, pair, vptr, vlen);
					if (!p)
					{
						/* error in changing the value */
						return HAK_NULL;
					}
					if (p != pair)
					{
						/* old pair destroyed. new pair reallocated.
						 * relink to include the new pair but to drop
						 * the old pair. */
						if (prev == HAK_NULL) htb->bucket[hc] = p;
						else NEXT(prev) = p;
						NEXT(p) = next;
					}
					htb->rev++;
					return p;

				case ENSERT:
					/* return existing pair */
					return pair;

				case INSERT:
					/* return failure */
					hak_seterrnum(htb->hak, HAK_EEXIST);
					return HAK_NULL;
			}
		}

		prev = pair;
		pair = next;
	}

	if (opt == UPDATE)
	{
		hak_seterrnum(htb->hak, HAK_ENOENT);
		return HAK_NULL;
	}

	if (htb->threshold > 0 && htb->size >= htb->threshold)
	{
		/* ingore reorganization error as it simply means
		 * more bucket collision and performance penalty. */
		if (reorganize(htb) == 0)
		{
			hc = htb->style->hasher(htb,kptr,klen) % htb->capa;
		}
	}

	HAK_ASSERT(htb->hak, pair == HAK_NULL);

	pair = alloc_pair(htb, kptr, klen, vptr, vlen);
	if (HAK_UNLIKELY(!pair)) return HAK_NULL; /* error */

	NEXT(pair) = htb->bucket[hc];
	htb->bucket[hc] = pair;
	htb->size++;

	htb->rev++;
	return pair; /* new key added */
}

pair_t* hak_htb_upsert (hak_htb_t* htb, void* kptr, hak_oow_t klen, void* vptr, hak_oow_t vlen)
{
	return insert(htb, kptr, klen, vptr, vlen, UPSERT);
}

pair_t* hak_htb_ensert (hak_htb_t* htb, void* kptr, hak_oow_t klen, void* vptr, hak_oow_t vlen)
{
	return insert(htb, kptr, klen, vptr, vlen, ENSERT);
}

pair_t* hak_htb_insert (hak_htb_t* htb, void* kptr, hak_oow_t klen, void* vptr, hak_oow_t vlen)
{
	return insert(htb, kptr, klen, vptr, vlen, INSERT);
}

pair_t* hak_htb_update (hak_htb_t* htb, void* kptr, hak_oow_t klen, void* vptr, hak_oow_t vlen)
{
	return insert(htb, kptr, klen, vptr, vlen, UPDATE);
}

pair_t* hak_htb_cbsert (hak_htb_t* htb, void* kptr, hak_oow_t klen, cbserter_t cbserter, void* ctx)
{
	pair_t* pair, * p, * prev, * next;
	hak_oow_t hc;

	hc = htb->style->hasher(htb,kptr,klen) % htb->capa;
	pair = htb->bucket[hc];
	prev = HAK_NULL;

	while (pair != HAK_NULL)
	{
		next = NEXT(pair);

		if (htb->style->comper(htb, KPTR(pair), KLEN(pair), kptr, klen) == 0)
		{
			/* found a pair with a matching key */
			p = cbserter(htb, pair, kptr, klen, ctx);
			if (!p)
			{
				/* error returned by the callback function */
				return HAK_NULL;
			}
			if (p != pair)
			{
				/* old pair destroyed. new pair reallocated.
				 * relink to include the new pair but to drop
				 * the old pair. */
				if (prev == HAK_NULL) htb->bucket[hc] = p;
				else NEXT(prev) = p;
				NEXT(p) = next;
			}
			htb->rev++;
			return p;
		}

		prev = pair;
		pair = next;
	}

	if (htb->threshold > 0 && htb->size >= htb->threshold)
	{
		/* ingore reorganization error as it simply means
		 * more bucket collision and performance penalty. */
		if (reorganize(htb) == 0)
		{
			hc = htb->style->hasher(htb,kptr,klen) % htb->capa;
		}
	}

	HAK_ASSERT(htb->hak, pair == HAK_NULL);

	pair = cbserter(htb, HAK_NULL, kptr, klen, ctx);
	if (HAK_UNLIKELY(!pair)) return HAK_NULL; /* error */

	NEXT(pair) = htb->bucket[hc];
	htb->bucket[hc] = pair;
	htb->size++;
	htb->rev++;

	return pair; /* new key added */
}

int hak_htb_delete (hak_htb_t* htb, const void* kptr, hak_oow_t klen)
{
	pair_t* pair, * prev;
	hak_oow_t hc;

	hc = htb->style->hasher(htb,kptr,klen) % htb->capa;
	pair = htb->bucket[hc];
	prev = HAK_NULL;

	while (pair != HAK_NULL)
	{
		if (htb->style->comper(htb, KPTR(pair), KLEN(pair), kptr, klen) == 0)
		{
			if (prev == HAK_NULL)
				htb->bucket[hc] = NEXT(pair);
			else NEXT(prev) = NEXT(pair);

			free_pair(htb, pair);
			htb->size--;
			htb->rev++;

			return 0;
		}

		prev = pair;
		pair = NEXT(pair);
	}

	hak_seterrnum(htb->hak, HAK_ENOENT);
	return -1;
}

void hak_htb_clear (hak_htb_t* htb)
{
	hak_oow_t i, sz;
	pair_t* pair, * next;

	sz = htb->size;
	for (i = 0; i < htb->capa; i++)
	{
		pair = htb->bucket[i];

		while (pair)
		{
			next = NEXT(pair);
			free_pair(htb, pair);
			htb->size--;
			pair = next;
		}

		htb->bucket[i] = HAK_NULL;
	}

	HAK_ASSERT(htb->hak, htb->size == 0);
	if (sz) htb->rev++;
}

void hak_htb_walk (hak_htb_t* htb, walker_t walker, void* ctx)
{
	hak_oow_t i;
	pair_t* pair, * next;

	for (i = 0; i < htb->capa; i++)
	{
		pair = htb->bucket[i];

		while (pair != HAK_NULL)
		{
			next = NEXT(pair);
			if (walker(htb, pair, ctx) == HAK_HTB_WALK_STOP) return;
			pair = next;
		}
	}
}


void hak_init_htb_itr (hak_htb_itr_t* itr)
{
	itr->pair = HAK_NULL;
	itr->buckno = 0;
}

pair_t* hak_htb_getfirstpair (hak_htb_t* htb, hak_htb_itr_t* itr)
{
	hak_oow_t i;
	pair_t* pair;

	for (i = 0; i < htb->capa; i++)
	{
		pair = htb->bucket[i];
		if (pair)
		{
			itr->buckno = i;
			itr->pair = pair;
			return pair;
		}
	}

	return HAK_NULL;
}

pair_t* hak_htb_getnextpair (hak_htb_t* htb, hak_htb_itr_t* itr)
{
	hak_oow_t i;
	pair_t* pair;

	pair = NEXT(itr->pair);
	if (pair)
	{
		/* no change in bucket number */
		itr->pair = pair;
		return pair;
	}

	for (i = itr->buckno + 1; i < htb->capa; i++)
	{
		pair = htb->bucket[i];
		if (pair)
		{
			itr->buckno = i;
			itr->pair = pair;
			return pair;
		}
	}

	return HAK_NULL;
}

hak_oow_t hak_htb_dflhash (const hak_htb_t* htb, const void* kptr, hak_oow_t klen)
{
	hak_oow_t h;
	HAK_HASH_BYTES(h, kptr, klen);
	return h ;
}

int hak_htb_dflcomp (const hak_htb_t* htb, const void* kptr1, hak_oow_t klen1, const void* kptr2, hak_oow_t klen2)
{
	if (klen1 == klen2) return HAK_MEMCMP(kptr1, kptr2, KTOB(htb,klen1));
	/* it just returns 1 to indicate that they are different. */
	return 1;
}

