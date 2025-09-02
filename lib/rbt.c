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

#include <hak-rbt.h>
#include "hak-prv.h"

#define copier_t        hak_rbt_copier_t
#define freeer_t        hak_rbt_freeer_t
#define comper_t        hak_rbt_comper_t
#define keeper_t        hak_rbt_keeper_t
#define walker_t        hak_rbt_walker_t
#define cbserter_t      hak_rbt_cbserter_t

#define KPTR(p)  HAK_RBT_KPTR(p)
#define KLEN(p)  HAK_RBT_KLEN(p)
#define VPTR(p)  HAK_RBT_VPTR(p)
#define VLEN(p)  HAK_RBT_VLEN(p)

#define KTOB(rbt,len) ((len)*(rbt)->scale[HAK_RBT_KEY])
#define VTOB(rbt,len) ((len)*(rbt)->scale[HAK_RBT_VAL])

#define UPSERT 1
#define UPDATE 2
#define ENSERT 3
#define INSERT 4

#define IS_NIL(rbt,x) ((x) == &((rbt)->xnil))
#define LEFT 0
#define RIGHT 1
#define left child[LEFT]
#define right child[RIGHT]
#define rotate_left(rbt,pivot) rotate(rbt,pivot,1);
#define rotate_right(rbt,pivot) rotate(rbt,pivot,0);

HAK_INLINE hak_rbt_pair_t* hak_rbt_allocpair (
	hak_rbt_t* rbt, void* kptr, hak_oow_t klen, void* vptr, hak_oow_t vlen)
{
	hak_rbt_pair_t* pair;

	copier_t kcop = rbt->style->copier[HAK_RBT_KEY];
	copier_t vcop = rbt->style->copier[HAK_RBT_VAL];

	hak_oow_t as = HAK_SIZEOF(hak_rbt_pair_t);
	if (kcop == HAK_RBT_COPIER_INLINE) as += HAK_ALIGN_POW2(KTOB(rbt,klen), HAK_SIZEOF_VOID_P);
	if (vcop == HAK_RBT_COPIER_INLINE) as += VTOB(rbt,vlen);

	pair = (hak_rbt_pair_t*)HAK_MMGR_ALLOC(HAK_MMGR(rbt->hak), as);
	if (pair == HAK_NULL) return HAK_NULL;

	pair->color = HAK_RBT_RED;
	pair->parent = HAK_NULL;
	pair->child[LEFT] = &rbt->xnil;
	pair->child[RIGHT] = &rbt->xnil;

	KLEN(pair) = klen;
	if (kcop == HAK_RBT_COPIER_SIMPLE)
	{
		KPTR(pair) = kptr;
	}
	else if (kcop == HAK_RBT_COPIER_INLINE)
	{
		KPTR(pair) = pair + 1;
		if (kptr) HAK_MEMCPY (KPTR(pair), kptr, KTOB(rbt,klen));
	}
	else
	{
		KPTR(pair) = kcop (rbt, kptr, klen);
		if (KPTR(pair) == HAK_NULL)
		{
			hak_freemem (rbt->hak, pair);
			return HAK_NULL;
		}
	}

	VLEN(pair) = vlen;
	if (vcop == HAK_RBT_COPIER_SIMPLE)
	{
		VPTR(pair) = vptr;
	}
	else if (vcop == HAK_RBT_COPIER_INLINE)
	{
		VPTR(pair) = pair + 1;
		if (kcop == HAK_RBT_COPIER_INLINE)
			VPTR(pair) = (hak_oob_t*)VPTR(pair) + HAK_ALIGN_POW2(KTOB(rbt,klen), HAK_SIZEOF_VOID_P);
		if (vptr) HAK_MEMCPY (VPTR(pair), vptr, VTOB(rbt,vlen));
	}
	else
	{
		VPTR(pair) = vcop (rbt, vptr, vlen);
		if (VPTR(pair) != HAK_NULL)
		{
			if (rbt->style->freeer[HAK_RBT_KEY] != HAK_NULL)
				rbt->style->freeer[HAK_RBT_KEY] (rbt, KPTR(pair), KLEN(pair));
			hak_freemem (rbt->hak, pair);
			return HAK_NULL;
		}
	}

	return pair;
}

HAK_INLINE void hak_rbt_freepair (hak_rbt_t* rbt, hak_rbt_pair_t* pair)
{
	if (rbt->style->freeer[HAK_RBT_KEY] != HAK_NULL)
		rbt->style->freeer[HAK_RBT_KEY] (rbt, KPTR(pair), KLEN(pair));
	if (rbt->style->freeer[HAK_RBT_VAL] != HAK_NULL)
		rbt->style->freeer[HAK_RBT_VAL] (rbt, VPTR(pair), VLEN(pair));
	hak_freemem (rbt->hak, pair);
}

static hak_rbt_style_t style[] =
{
	{
		{
			HAK_RBT_COPIER_DEFAULT,
			HAK_RBT_COPIER_DEFAULT
		},
		{
			HAK_RBT_FREEER_DEFAULT,
			HAK_RBT_FREEER_DEFAULT
		},
		HAK_RBT_COMPER_DEFAULT,
		HAK_RBT_KEEPER_DEFAULT
	},

	{
		{
			HAK_RBT_COPIER_INLINE,
			HAK_RBT_COPIER_INLINE
		},
		{
			HAK_RBT_FREEER_DEFAULT,
			HAK_RBT_FREEER_DEFAULT
		},
		HAK_RBT_COMPER_DEFAULT,
		HAK_RBT_KEEPER_DEFAULT
	},

	{
		{
			HAK_RBT_COPIER_INLINE,
			HAK_RBT_COPIER_DEFAULT
		},
		{
			HAK_RBT_FREEER_DEFAULT,
			HAK_RBT_FREEER_DEFAULT
		},
		HAK_RBT_COMPER_DEFAULT,
		HAK_RBT_KEEPER_DEFAULT
	},

	{
		{
			HAK_RBT_COPIER_DEFAULT,
			HAK_RBT_COPIER_INLINE
		},
		{
			HAK_RBT_FREEER_DEFAULT,
			HAK_RBT_FREEER_DEFAULT
		},
		HAK_RBT_COMPER_DEFAULT,
		HAK_RBT_KEEPER_DEFAULT
	}
};

const hak_rbt_style_t* hak_get_rbt_style (hak_rbt_style_kind_t kind)
{
	return &style[kind];
}

hak_rbt_t* hak_rbt_open (hak_t* hak, hak_oow_t xtnsize, int kscale, int vscale)
{
	hak_rbt_t* rbt;

	rbt = (hak_rbt_t*)hak_allocmem(hak, HAK_SIZEOF(hak_rbt_t) + xtnsize);
	if (!rbt) return HAK_NULL;

	if (hak_rbt_init(rbt, hak, kscale, vscale) <= -1)
	{
		hak_freemem (hak, rbt);
		return HAK_NULL;
	}

	HAK_MEMSET (rbt + 1, 0, xtnsize);
	return rbt;
}

void hak_rbt_close (hak_rbt_t* rbt)
{
	hak_rbt_fini (rbt);
	hak_freemem (rbt->hak, rbt);
}

int hak_rbt_init (hak_rbt_t* rbt, hak_t* hak, int kscale, int vscale)
{
	/* do not zero out the extension */
	HAK_MEMSET (rbt, 0, HAK_SIZEOF(*rbt));
	rbt->hak = hak;

	rbt->scale[HAK_RBT_KEY] = (kscale < 1)? 1: kscale;
	rbt->scale[HAK_RBT_VAL] = (vscale < 1)? 1: vscale;
	rbt->size = 0;

	rbt->style = &style[0];

	/* self-initializing nil */
	HAK_MEMSET(&rbt->xnil, 0, HAK_SIZEOF(rbt->xnil));
	rbt->xnil.color = HAK_RBT_BLACK;
	rbt->xnil.left = &rbt->xnil;
	rbt->xnil.right = &rbt->xnil;

	/* root is set to nil initially */
	rbt->root = &rbt->xnil;

	return 0;
}

void hak_rbt_fini (hak_rbt_t* rbt)
{
	hak_rbt_clear (rbt);
}

void* hak_rbt_getxtn (hak_rbt_t* rbt)
{
	return (void*)(rbt + 1);
}

const hak_rbt_style_t* hak_rbt_getstyle (const hak_rbt_t* rbt)
{
	return rbt->style;
}

void hak_rbt_setstyle (hak_rbt_t* rbt, const hak_rbt_style_t* style)
{
	HAK_ASSERT (rbt->hak, style != HAK_NULL);
	rbt->style = style;
}

hak_oow_t hak_rbt_getsize (const hak_rbt_t* rbt)
{
	return rbt->size;
}

hak_rbt_pair_t* hak_rbt_search (const hak_rbt_t* rbt, const void* kptr, hak_oow_t klen)
{
	hak_rbt_pair_t* pair = rbt->root;

	while (!IS_NIL(rbt,pair))
	{
		int n = rbt->style->comper (rbt, kptr, klen, KPTR(pair), KLEN(pair));
		if (n == 0) return pair;

		if (n > 0) pair = pair->right;
		else /* if (n < 0) */ pair = pair->left;
	}

	return HAK_NULL;
}

static void rotate (hak_rbt_t* rbt, hak_rbt_pair_t* pivot, int leftwise)
{
	/*
	 * == leftwise rotation
	 * move the pivot pair down to the poistion of the pivot's original
	 * left child(x). move the pivot's right child(y) to the pivot's original
	 * position. as 'c1' is between 'y' and 'pivot', move it to the right
	 * of the new pivot position.
	 *       parent                   parent
	 *        | | (left or right?)      | |
	 *       pivot                      y
	 *       /  \                     /  \
	 *     x     y    =====>      pivot   c2
	 *          / \               /  \
	 *         c1  c2            x   c1
	 *
	 * == rightwise rotation
	 * move the pivot pair down to the poistion of the pivot's original
	 * right child(y). move the pivot's left child(x) to the pivot's original
	 * position. as 'c2' is between 'x' and 'pivot', move it to the left
	 * of the new pivot position.
	 *
	 *       parent                   parent
	 *        | | (left or right?)      | |
	 *       pivot                      x
	 *       /  \                     /  \
	 *     x     y    =====>        c1   pivot
	 *    / \                            /  \
	 *   c1  c2                         c2   y
	 *
	 *
	 * the actual implementation here resolves the pivot's relationship to
	 * its parent by comparaing pointers as it is not known if the pivot pair
	 * is the left child or the right child of its parent,
	 */

	hak_rbt_pair_t* parent, * z, * c;
	int cid1, cid2;

	HAK_ASSERT (rbt->hak, pivot != HAK_NULL);

	if (leftwise)
	{
		cid1 = RIGHT;
		cid2 = LEFT;
	}
	else
	{
		cid1 = LEFT;
		cid2 = RIGHT;
	}

	parent = pivot->parent;
	/* y for leftwise rotation, x for rightwise rotation */
	z = pivot->child[cid1];
	/* c1 for leftwise rotation, c2 for rightwise rotation */
	c = z->child[cid2];

	z->parent = parent;
	if (parent)
	{
		if (parent->left == pivot)
		{
			parent->left = z;
		}
		else
		{
			HAK_ASSERT (rbt->hak, parent->right == pivot);
			parent->right = z;
		}
	}
	else
	{
		HAK_ASSERT (rbt->hak, rbt->root == pivot);
		rbt->root = z;
	}

	z->child[cid2] = pivot;
	if (!IS_NIL(rbt,pivot)) pivot->parent = z;

	pivot->child[cid1] = c;
	if (!IS_NIL(rbt,c)) c->parent = pivot;
}

static void adjust (hak_rbt_t* rbt, hak_rbt_pair_t* pair)
{
	while (pair != rbt->root)
	{
		hak_rbt_pair_t* tmp, * tmp2, * x_par;
		int leftwise;

		x_par = pair->parent;
		if (x_par->color == HAK_RBT_BLACK) break;

		HAK_ASSERT (rbt->hak, x_par->parent != HAK_NULL);

		if (x_par == x_par->parent->child[LEFT])
		{
			tmp = x_par->parent->child[RIGHT];
			tmp2 = x_par->child[RIGHT];
			leftwise = 1;
		}
		else
		{
			tmp = x_par->parent->child[LEFT];
			tmp2 = x_par->child[LEFT];
			leftwise = 0;
		}

		if (tmp->color == HAK_RBT_RED)
		{
			x_par->color = HAK_RBT_BLACK;
			tmp->color = HAK_RBT_BLACK;
			x_par->parent->color = HAK_RBT_RED;
			pair = x_par->parent;
		}
		else
		{
			if (pair == tmp2)
			{
				pair = x_par;
				rotate (rbt, pair, leftwise);
				x_par = pair->parent;
			}

			x_par->color = HAK_RBT_BLACK;
			x_par->parent->color = HAK_RBT_RED;
			rotate (rbt, x_par->parent, !leftwise);
		}
	}
}

static hak_rbt_pair_t* change_pair_val (
	hak_rbt_t* rbt, hak_rbt_pair_t* pair, void* vptr, hak_oow_t vlen)
{
	if (VPTR(pair) == vptr && VLEN(pair) == vlen)
	{
		/* if the old value and the new value are the same,
		 * it just calls the handler for this condition.
		 * No value replacement occurs. */
		if (rbt->style->keeper != HAK_NULL)
		{
			rbt->style->keeper (rbt, vptr, vlen);
		}
	}
	else
	{
		copier_t vcop = rbt->style->copier[HAK_RBT_VAL];
		void* ovptr = VPTR(pair);
		hak_oow_t ovlen = VLEN(pair);

		/* place the new value according to the copier */
		if (vcop == HAK_RBT_COPIER_SIMPLE)
		{
			VPTR(pair) = vptr;
			VLEN(pair) = vlen;
		}
		else if (vcop == HAK_RBT_COPIER_INLINE)
		{
			if (ovlen == vlen)
			{
				if (vptr) HAK_MEMCPY (VPTR(pair), vptr, VTOB(rbt,vlen));
			}
			else
			{
				/* need to reconstruct the pair */
				hak_rbt_pair_t* p = hak_rbt_allocpair (rbt,
					KPTR(pair), KLEN(pair),
					vptr, vlen);
				if (p == HAK_NULL) return HAK_NULL;

				p->color = pair->color;
				p->left = pair->left;
				p->right = pair->right;
				p->parent = pair->parent;

				if (pair->parent)
				{
					if (pair->parent->left == pair)
					{
						pair->parent->left = p;
					}
					else
					{
						HAK_ASSERT (rbt->hak, pair->parent->right == pair);
						pair->parent->right = p;
					}
				}
				if (!IS_NIL(rbt,pair->left)) pair->left->parent = p;
				if (!IS_NIL(rbt,pair->right)) pair->right->parent = p;

				if (pair == rbt->root) rbt->root = p;

				hak_rbt_freepair (rbt, pair);
				return p;
			}
		}
		else
		{
			void* nvptr = vcop (rbt, vptr, vlen);
			if (nvptr == HAK_NULL) return HAK_NULL;
			VPTR(pair) = nvptr;
			VLEN(pair) = vlen;
		}

		/* free up the old value */
		if (rbt->style->freeer[HAK_RBT_VAL] != HAK_NULL)
		{
			rbt->style->freeer[HAK_RBT_VAL] (rbt, ovptr, ovlen);
		}
	}

	return pair;
}

static hak_rbt_pair_t* insert (
	hak_rbt_t* rbt, void* kptr, hak_oow_t klen, void* vptr, hak_oow_t vlen, int opt)
{
	hak_rbt_pair_t* x_cur = rbt->root;
	hak_rbt_pair_t* x_par = HAK_NULL;
	hak_rbt_pair_t* x_new;

	while (!IS_NIL(rbt,x_cur))
	{
		int n = rbt->style->comper (rbt, kptr, klen, KPTR(x_cur), KLEN(x_cur));
		if (n == 0)
		{
			switch (opt)
			{
				case UPSERT:
				case UPDATE:
					return change_pair_val (rbt, x_cur, vptr, vlen);

				case ENSERT:
					/* return existing pair */
					return x_cur;

				case INSERT:
					/* return failure */
					return HAK_NULL;
			}
		}

		x_par = x_cur;

		if (n > 0) x_cur = x_cur->right;
		else /* if (n < 0) */ x_cur = x_cur->left;
	}

	if (opt == UPDATE) return HAK_NULL;

	x_new = hak_rbt_allocpair (rbt, kptr, klen, vptr, vlen);
	if (x_new == HAK_NULL) return HAK_NULL;

	if (x_par == HAK_NULL)
	{
		/* the tree contains no pair */
		HAK_ASSERT (rbt->hak, rbt->root == &rbt->xnil);
		rbt->root = x_new;
	}
	else
	{
		/* perform normal binary insert */
		int n = rbt->style->comper (rbt, kptr, klen, KPTR(x_par), KLEN(x_par));
		if (n > 0)
		{
			HAK_ASSERT (rbt->hak, x_par->right == &rbt->xnil);
			x_par->right = x_new;
		}
		else
		{
			HAK_ASSERT (rbt->hak, x_par->left == &rbt->xnil);
			x_par->left = x_new;
		}

		x_new->parent = x_par;
		adjust (rbt, x_new);
	}

	rbt->root->color = HAK_RBT_BLACK;
	rbt->size++;
	return x_new;
}

hak_rbt_pair_t* hak_rbt_upsert (
	hak_rbt_t* rbt, void* kptr, hak_oow_t klen, void* vptr, hak_oow_t vlen)
{
	return insert (rbt, kptr, klen, vptr, vlen, UPSERT);
}

hak_rbt_pair_t* hak_rbt_ensert (
	hak_rbt_t* rbt, void* kptr, hak_oow_t klen, void* vptr, hak_oow_t vlen)
{
	return insert (rbt, kptr, klen, vptr, vlen, ENSERT);
}

hak_rbt_pair_t* hak_rbt_insert (
	hak_rbt_t* rbt, void* kptr, hak_oow_t klen, void* vptr, hak_oow_t vlen)
{
	return insert (rbt, kptr, klen, vptr, vlen, INSERT);
}


hak_rbt_pair_t* hak_rbt_update (
	hak_rbt_t* rbt, void* kptr, hak_oow_t klen, void* vptr, hak_oow_t vlen)
{
	return insert (rbt, kptr, klen, vptr, vlen, UPDATE);
}

hak_rbt_pair_t* hak_rbt_cbsert (
	hak_rbt_t* rbt, void* kptr, hak_oow_t klen, cbserter_t cbserter, void* ctx)
{
	hak_rbt_pair_t* x_cur = rbt->root;
	hak_rbt_pair_t* x_par = HAK_NULL;
	hak_rbt_pair_t* x_new;

	while (!IS_NIL(rbt,x_cur))
	{
		int n = rbt->style->comper (rbt, kptr, klen, KPTR(x_cur), KLEN(x_cur));
		if (n == 0)
		{
			/* back up the contents of the current pair
			 * in case it is reallocated */
			hak_rbt_pair_t tmp;

			tmp = *x_cur;

			/* call the callback function to manipulate the pair */
			x_new = cbserter (rbt, x_cur, kptr, klen, ctx);
			if (x_new == HAK_NULL)
			{
				/* error returned by the callback function */
				return HAK_NULL;
			}

			if (x_new != x_cur)
			{
				/* the current pair has been reallocated, which implicitly
				 * means the previous contents were wiped out. so the contents
				 * backed up will be used for restoration/migration */

				x_new->color = tmp.color;
				x_new->left = tmp.left;
				x_new->right = tmp.right;
				x_new->parent = tmp.parent;

				if (tmp.parent)
				{
					if (tmp.parent->left == x_cur)
					{
						tmp.parent->left = x_new;
					}
					else
					{
						HAK_ASSERT (rbt->hak, tmp.parent->right == x_cur);
						tmp.parent->right = x_new;
					}
				}
				if (!IS_NIL(rbt,tmp.left)) tmp.left->parent = x_new;
				if (!IS_NIL(rbt,tmp.right)) tmp.right->parent = x_new;

				if (x_cur == rbt->root) rbt->root = x_new;
			}

			return x_new;
		}

		x_par = x_cur;

		if (n > 0) x_cur = x_cur->right;
		else /* if (n < 0) */ x_cur = x_cur->left;
	}

	x_new = cbserter (rbt, HAK_NULL, kptr, klen, ctx);
	if (x_new == HAK_NULL) return HAK_NULL;

	if (x_par == HAK_NULL)
	{
		/* the tree contains no pair */
		HAK_ASSERT (rbt->hak, rbt->root == &rbt->xnil);
		rbt->root = x_new;
	}
	else
	{
		/* perform normal binary insert */
		int n = rbt->style->comper (rbt, kptr, klen, KPTR(x_par), KLEN(x_par));
		if (n > 0)
		{
			HAK_ASSERT (rbt->hak, x_par->right == &rbt->xnil);
			x_par->right = x_new;
		}
		else
		{
			HAK_ASSERT (rbt->hak, x_par->left == &rbt->xnil);
			x_par->left = x_new;
		}

		x_new->parent = x_par;
		adjust (rbt, x_new);
	}

	rbt->root->color = HAK_RBT_BLACK;
	rbt->size++;
	return x_new;
}


static void adjust_for_delete (hak_rbt_t* rbt, hak_rbt_pair_t* pair, hak_rbt_pair_t* par)
{
	while (pair != rbt->root && pair->color == HAK_RBT_BLACK)
	{
		hak_rbt_pair_t* tmp;

		if (pair == par->left)
		{
			tmp = par->right;
			if (tmp->color == HAK_RBT_RED)
			{
				tmp->color = HAK_RBT_BLACK;
				par->color = HAK_RBT_RED;
				rotate_left (rbt, par);
				tmp = par->right;
			}

			if (tmp->left->color == HAK_RBT_BLACK &&
			    tmp->right->color == HAK_RBT_BLACK)
			{
				if (!IS_NIL(rbt,tmp)) tmp->color = HAK_RBT_RED;
				pair = par;
				par = pair->parent;
			}
			else
			{
				if (tmp->right->color == HAK_RBT_BLACK)
				{
					if (!IS_NIL(rbt,tmp->left))
						tmp->left->color = HAK_RBT_BLACK;
					tmp->color = HAK_RBT_RED;
					rotate_right (rbt, tmp);
					tmp = par->right;
				}

				tmp->color = par->color;
				if (!IS_NIL(rbt,par)) par->color = HAK_RBT_BLACK;
				if (tmp->right->color == HAK_RBT_RED)
					tmp->right->color = HAK_RBT_BLACK;

				rotate_left (rbt, par);
				pair = rbt->root;
			}
		}
		else
		{
			HAK_ASSERT (rbt->hak, pair == par->right);
			tmp = par->left;
			if (tmp->color == HAK_RBT_RED)
			{
				tmp->color = HAK_RBT_BLACK;
				par->color = HAK_RBT_RED;
				rotate_right (rbt, par);
				tmp = par->left;
			}

			if (tmp->left->color == HAK_RBT_BLACK &&
			    tmp->right->color == HAK_RBT_BLACK)
			{
				if (!IS_NIL(rbt,tmp)) tmp->color = HAK_RBT_RED;
				pair = par;
				par = pair->parent;
			}
			else
			{
				if (tmp->left->color == HAK_RBT_BLACK)
				{
					if (!IS_NIL(rbt,tmp->right))
						tmp->right->color = HAK_RBT_BLACK;
					tmp->color = HAK_RBT_RED;
					rotate_left (rbt, tmp);
					tmp = par->left;
				}
				tmp->color = par->color;
				if (!IS_NIL(rbt,par)) par->color = HAK_RBT_BLACK;
				if (tmp->left->color == HAK_RBT_RED)
					tmp->left->color = HAK_RBT_BLACK;

				rotate_right (rbt, par);
				pair = rbt->root;
			}
		}
	}

	pair->color = HAK_RBT_BLACK;
}

static void delete_pair (hak_rbt_t* rbt, hak_rbt_pair_t* pair)
{
	hak_rbt_pair_t* x, * y, * par;

	HAK_ASSERT (rbt->hak, pair && !IS_NIL(rbt,pair));

	if (IS_NIL(rbt,pair->left) || IS_NIL(rbt,pair->right))
	{
		y = pair;
	}
	else
	{
		/* find a successor with NIL as a child */
		y = pair->right;
		while (!IS_NIL(rbt,y->left)) y = y->left;
	}

	x = IS_NIL(rbt,y->left)? y->right: y->left;

	par = y->parent;
	if (!IS_NIL(rbt,x)) x->parent = par;

	if (par)
	{
		if (y == par->left)
			par->left = x;
		else
			par->right = x;
	}
	else
	{
		rbt->root = x;
	}

	if (y == pair)
	{
		if (y->color == HAK_RBT_BLACK && !IS_NIL(rbt,x))
			adjust_for_delete (rbt, x, par);

		hak_rbt_freepair (rbt, y);
	}
	else
	{
		if (y->color == HAK_RBT_BLACK && !IS_NIL(rbt,x))
			adjust_for_delete (rbt, x, par);

#if 1
		if (pair->parent)
		{
			if (pair->parent->left == pair) pair->parent->left = y;
			if (pair->parent->right == pair) pair->parent->right = y;
		}
		else
		{
			rbt->root = y;
		}

		y->parent = pair->parent;
		y->left = pair->left;
		y->right = pair->right;
		y->color = pair->color;

		if (pair->left->parent == pair) pair->left->parent = y;
		if (pair->right->parent == pair) pair->right->parent = y;
#else
		*y = *pair;
		if (y->parent)
		{
			if (y->parent->left == pair) y->parent->left = y;
			if (y->parent->right == pair) y->parent->right = y;
		}
		else
		{
			rbt->root = y;
		}

		if (y->left->parent == pair) y->left->parent = y;
		if (y->right->parent == pair) y->right->parent = y;
#endif

		hak_rbt_freepair (rbt, pair);
	}

	rbt->size--;
}

int hak_rbt_delete (hak_rbt_t* rbt, const void* kptr, hak_oow_t klen)
{
	hak_rbt_pair_t* pair;

	pair = hak_rbt_search (rbt, kptr, klen);
	if (pair == HAK_NULL) return -1;

	delete_pair (rbt, pair);
	return 0;
}

void hak_rbt_clear (hak_rbt_t* rbt)
{
	/* TODO: improve this */
	while (!IS_NIL(rbt,rbt->root)) delete_pair (rbt, rbt->root);
}

#if 0
static HAK_INLINE hak_rbt_walk_t walk_recursively (
	hak_rbt_t* rbt, walker_t walker, void* ctx, hak_rbt_pair_t* pair)
{
	if (!IS_NIL(rbt,pair->left))
	{
		if (walk_recursively (rbt, walker, ctx, pair->left) == HAK_RBT_WALK_STOP)
			return HAK_RBT_WALK_STOP;
	}

	if (walker (rbt, pair, ctx) == HAK_RBT_WALK_STOP) return HAK_RBT_WALK_STOP;

	if (!IS_NIL(rbt,pair->right))
	{
		if (walk_recursively (rbt, walker, ctx, pair->right) == HAK_RBT_WALK_STOP)
			return HAK_RBT_WALK_STOP;
	}

	return HAK_RBT_WALK_FORWARD;
}
#endif

static HAK_INLINE void walk (hak_rbt_t* rbt, walker_t walker, void* ctx, int l, int r)
{
	hak_rbt_pair_t* x_cur = rbt->root;
	hak_rbt_pair_t* prev = rbt->root->parent;

	while (x_cur && !IS_NIL(rbt,x_cur))
	{
		if (prev == x_cur->parent)
		{
			/* the previous node is the parent of the current node.
			 * it indicates that we're going down to the child[l] */
			if (!IS_NIL(rbt,x_cur->child[l]))
			{
				/* go to the child[l] child */
				prev = x_cur;
				x_cur = x_cur->child[l];
			}
			else
			{
				if (walker (rbt, x_cur, ctx) == HAK_RBT_WALK_STOP) break;

				if (!IS_NIL(rbt,x_cur->child[r]))
				{
					/* go down to the right node if exists */
					prev = x_cur;
					x_cur = x_cur->child[r];
				}
				else
				{
					/* otherwise, move up to the parent */
					prev = x_cur;
					x_cur = x_cur->parent;
				}
			}
		}
		else if (prev == x_cur->child[l])
		{
			/* the left child has been already traversed */

			if (walker (rbt, x_cur, ctx) == HAK_RBT_WALK_STOP) break;

			if (!IS_NIL(rbt,x_cur->child[r]))
			{
				/* go down to the right node if it exists */
				prev = x_cur;
				x_cur = x_cur->child[r];
			}
			else
			{
				/* otherwise, move up to the parent */
				prev = x_cur;
				x_cur = x_cur->parent;
			}
		}
		else
		{
			/* both the left child and the right child have been traversed */
			HAK_ASSERT (rbt->hak, prev == x_cur->child[r]);
			/* just move up to the parent */
			prev = x_cur;
			x_cur = x_cur->parent;
		}
	}
}

void hak_rbt_walk (hak_rbt_t* rbt, walker_t walker, void* ctx)
{
	walk (rbt, walker, ctx, LEFT, RIGHT);
}

void hak_rbt_rwalk (hak_rbt_t* rbt, walker_t walker, void* ctx)
{
	walk (rbt, walker, ctx, RIGHT, LEFT);
}

int hak_rbt_dflcomp (const hak_rbt_t* rbt, const void* kptr1, hak_oow_t klen1, const void* kptr2, hak_oow_t klen2)
{
	hak_oow_t min;
	int n, nn;

	if (klen1 < klen2)
	{
		min = klen1;
		nn = -1;
	}
	else
	{
		min = klen2;
		nn = (klen1 == klen2)? 0: 1;
	}

	n = HAK_MEMCMP (kptr1, kptr2, KTOB(rbt,min));
	if (n == 0) n = nn;
	return n;
}

