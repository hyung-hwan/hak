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

#include "hcl-rbt.h"
#include "hcl-prv.h"

#define copier_t        hcl_rbt_copier_t
#define freeer_t        hcl_rbt_freeer_t
#define comper_t        hcl_rbt_comper_t
#define keeper_t        hcl_rbt_keeper_t
#define walker_t        hcl_rbt_walker_t
#define cbserter_t      hcl_rbt_cbserter_t

#define KPTR(p)  HCL_RBT_KPTR(p)
#define KLEN(p)  HCL_RBT_KLEN(p)
#define VPTR(p)  HCL_RBT_VPTR(p)
#define VLEN(p)  HCL_RBT_VLEN(p)

#define KTOB(rbt,len) ((len)*(rbt)->scale[HCL_RBT_KEY])
#define VTOB(rbt,len) ((len)*(rbt)->scale[HCL_RBT_VAL])

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

HCL_INLINE hcl_rbt_pair_t* hcl_rbt_allocpair (
	hcl_rbt_t* rbt, void* kptr, hcl_oow_t klen, void* vptr, hcl_oow_t vlen)
{
	hcl_rbt_pair_t* pair;

	copier_t kcop = rbt->style->copier[HCL_RBT_KEY];
	copier_t vcop = rbt->style->copier[HCL_RBT_VAL];

	hcl_oow_t as = HCL_SIZEOF(hcl_rbt_pair_t);
	if (kcop == HCL_RBT_COPIER_INLINE) as += HCL_ALIGN_POW2(KTOB(rbt,klen), HCL_SIZEOF_VOID_P);
	if (vcop == HCL_RBT_COPIER_INLINE) as += VTOB(rbt,vlen);

	pair = (hcl_rbt_pair_t*)HCL_MMGR_ALLOC(hcl_getmmgr(rbt->hcl), as);
	if (pair == HCL_NULL) return HCL_NULL;

	pair->color = HCL_RBT_RED;
	pair->parent = HCL_NULL;
	pair->child[LEFT] = &rbt->xnil;
	pair->child[RIGHT] = &rbt->xnil;

	KLEN(pair) = klen;
	if (kcop == HCL_RBT_COPIER_SIMPLE)
	{
		KPTR(pair) = kptr;
	}
	else if (kcop == HCL_RBT_COPIER_INLINE)
	{
		KPTR(pair) = pair + 1;
		if (kptr) HCL_MEMCPY (KPTR(pair), kptr, KTOB(rbt,klen));
	}
	else
	{
		KPTR(pair) = kcop (rbt, kptr, klen);
		if (KPTR(pair) == HCL_NULL)
		{
			hcl_freemem (rbt->hcl, pair);
			return HCL_NULL;
		}
	}

	VLEN(pair) = vlen;
	if (vcop == HCL_RBT_COPIER_SIMPLE)
	{
		VPTR(pair) = vptr;
	}
	else if (vcop == HCL_RBT_COPIER_INLINE)
	{
		VPTR(pair) = pair + 1;
		if (kcop == HCL_RBT_COPIER_INLINE)
			VPTR(pair) = (hcl_oob_t*)VPTR(pair) + HCL_ALIGN_POW2(KTOB(rbt,klen), HCL_SIZEOF_VOID_P);
		if (vptr) HCL_MEMCPY (VPTR(pair), vptr, VTOB(rbt,vlen));
	}
	else
	{
		VPTR(pair) = vcop (rbt, vptr, vlen);
		if (VPTR(pair) != HCL_NULL)
		{
			if (rbt->style->freeer[HCL_RBT_KEY] != HCL_NULL)
				rbt->style->freeer[HCL_RBT_KEY] (rbt, KPTR(pair), KLEN(pair));
			hcl_freemem (rbt->hcl, pair);
			return HCL_NULL;
		}
	}

	return pair;
}

HCL_INLINE void hcl_rbt_freepair (hcl_rbt_t* rbt, hcl_rbt_pair_t* pair)
{
	if (rbt->style->freeer[HCL_RBT_KEY] != HCL_NULL)
		rbt->style->freeer[HCL_RBT_KEY] (rbt, KPTR(pair), KLEN(pair));
	if (rbt->style->freeer[HCL_RBT_VAL] != HCL_NULL)
		rbt->style->freeer[HCL_RBT_VAL] (rbt, VPTR(pair), VLEN(pair));
	hcl_freemem (rbt->hcl, pair);
}

static hcl_rbt_style_t style[] =
{
	{
		{
			HCL_RBT_COPIER_DEFAULT,
			HCL_RBT_COPIER_DEFAULT
		},
		{
			HCL_RBT_FREEER_DEFAULT,
			HCL_RBT_FREEER_DEFAULT
		},
		HCL_RBT_COMPER_DEFAULT,
		HCL_RBT_KEEPER_DEFAULT
	},

	{
		{
			HCL_RBT_COPIER_INLINE,
			HCL_RBT_COPIER_INLINE
		},
		{
			HCL_RBT_FREEER_DEFAULT,
			HCL_RBT_FREEER_DEFAULT
		},
		HCL_RBT_COMPER_DEFAULT,
		HCL_RBT_KEEPER_DEFAULT
	},

	{
		{
			HCL_RBT_COPIER_INLINE,
			HCL_RBT_COPIER_DEFAULT
		},
		{
			HCL_RBT_FREEER_DEFAULT,
			HCL_RBT_FREEER_DEFAULT
		},
		HCL_RBT_COMPER_DEFAULT,
		HCL_RBT_KEEPER_DEFAULT
	},

	{
		{
			HCL_RBT_COPIER_DEFAULT,
			HCL_RBT_COPIER_INLINE
		},
		{
			HCL_RBT_FREEER_DEFAULT,
			HCL_RBT_FREEER_DEFAULT
		},
		HCL_RBT_COMPER_DEFAULT,
		HCL_RBT_KEEPER_DEFAULT
	}
};

const hcl_rbt_style_t* hcl_get_rbt_style (hcl_rbt_style_kind_t kind)
{
	return &style[kind];
}

hcl_rbt_t* hcl_rbt_open (hcl_t* hcl, hcl_oow_t xtnsize, int kscale, int vscale)
{
	hcl_rbt_t* rbt;

	rbt = (hcl_rbt_t*)hcl_allocmem(hcl, HCL_SIZEOF(hcl_rbt_t) + xtnsize);
	if (!rbt) return HCL_NULL;

	if (hcl_rbt_init(rbt, hcl, kscale, vscale) <= -1)
	{
		hcl_freemem (hcl, rbt);
		return HCL_NULL;
	}

	HCL_MEMSET (rbt + 1, 0, xtnsize);
	return rbt;
}

void hcl_rbt_close (hcl_rbt_t* rbt)
{
	hcl_rbt_fini (rbt);
	hcl_freemem (rbt->hcl, rbt);
}

int hcl_rbt_init (hcl_rbt_t* rbt, hcl_t* hcl, int kscale, int vscale)
{
	/* do not zero out the extension */
	HCL_MEMSET (rbt, 0, HCL_SIZEOF(*rbt));
	rbt->hcl = hcl;

	rbt->scale[HCL_RBT_KEY] = (kscale < 1)? 1: kscale;
	rbt->scale[HCL_RBT_VAL] = (vscale < 1)? 1: vscale;
	rbt->size = 0;

	rbt->style = &style[0];

	/* self-initializing nil */
	HCL_MEMSET(&rbt->xnil, 0, HCL_SIZEOF(rbt->xnil));
	rbt->xnil.color = HCL_RBT_BLACK;
	rbt->xnil.left = &rbt->xnil;
	rbt->xnil.right = &rbt->xnil;

	/* root is set to nil initially */
	rbt->root = &rbt->xnil;

	return 0;
}

void hcl_rbt_fini (hcl_rbt_t* rbt)
{
	hcl_rbt_clear (rbt);
}

void* hcl_rbt_getxtn (hcl_rbt_t* rbt)
{
	return (void*)(rbt + 1);
}

const hcl_rbt_style_t* hcl_rbt_getstyle (const hcl_rbt_t* rbt)
{
	return rbt->style;
}

void hcl_rbt_setstyle (hcl_rbt_t* rbt, const hcl_rbt_style_t* style)
{
	HCL_ASSERT (rbt->hcl, style != HCL_NULL);
	rbt->style = style;
}

hcl_oow_t hcl_rbt_getsize (const hcl_rbt_t* rbt)
{
	return rbt->size;
}

hcl_rbt_pair_t* hcl_rbt_search (const hcl_rbt_t* rbt, const void* kptr, hcl_oow_t klen)
{
	hcl_rbt_pair_t* pair = rbt->root;

	while (!IS_NIL(rbt,pair))
	{
		int n = rbt->style->comper (rbt, kptr, klen, KPTR(pair), KLEN(pair));
		if (n == 0) return pair;

		if (n > 0) pair = pair->right;
		else /* if (n < 0) */ pair = pair->left;
	}

	return HCL_NULL;
}

static void rotate (hcl_rbt_t* rbt, hcl_rbt_pair_t* pivot, int leftwise)
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

	hcl_rbt_pair_t* parent, * z, * c;
	int cid1, cid2;

	HCL_ASSERT (rbt->hcl, pivot != HCL_NULL);

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
			HCL_ASSERT (rbt->hcl, parent->right == pivot);
			parent->right = z;
		}
	}
	else
	{
		HCL_ASSERT (rbt->hcl, rbt->root == pivot);
		rbt->root = z;
	}

	z->child[cid2] = pivot;
	if (!IS_NIL(rbt,pivot)) pivot->parent = z;

	pivot->child[cid1] = c;
	if (!IS_NIL(rbt,c)) c->parent = pivot;
}

static void adjust (hcl_rbt_t* rbt, hcl_rbt_pair_t* pair)
{
	while (pair != rbt->root)
	{
		hcl_rbt_pair_t* tmp, * tmp2, * x_par;
		int leftwise;

		x_par = pair->parent;
		if (x_par->color == HCL_RBT_BLACK) break;

		HCL_ASSERT (rbt->hcl, x_par->parent != HCL_NULL);

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

		if (tmp->color == HCL_RBT_RED)
		{
			x_par->color = HCL_RBT_BLACK;
			tmp->color = HCL_RBT_BLACK;
			x_par->parent->color = HCL_RBT_RED;
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

			x_par->color = HCL_RBT_BLACK;
			x_par->parent->color = HCL_RBT_RED;
			rotate (rbt, x_par->parent, !leftwise);
		}
	}
}

static hcl_rbt_pair_t* change_pair_val (
	hcl_rbt_t* rbt, hcl_rbt_pair_t* pair, void* vptr, hcl_oow_t vlen)
{
	if (VPTR(pair) == vptr && VLEN(pair) == vlen)
	{
		/* if the old value and the new value are the same,
		 * it just calls the handler for this condition.
		 * No value replacement occurs. */
		if (rbt->style->keeper != HCL_NULL)
		{
			rbt->style->keeper (rbt, vptr, vlen);
		}
	}
	else
	{
		copier_t vcop = rbt->style->copier[HCL_RBT_VAL];
		void* ovptr = VPTR(pair);
		hcl_oow_t ovlen = VLEN(pair);

		/* place the new value according to the copier */
		if (vcop == HCL_RBT_COPIER_SIMPLE)
		{
			VPTR(pair) = vptr;
			VLEN(pair) = vlen;
		}
		else if (vcop == HCL_RBT_COPIER_INLINE)
		{
			if (ovlen == vlen)
			{
				if (vptr) HCL_MEMCPY (VPTR(pair), vptr, VTOB(rbt,vlen));
			}
			else
			{
				/* need to reconstruct the pair */
				hcl_rbt_pair_t* p = hcl_rbt_allocpair (rbt,
					KPTR(pair), KLEN(pair),
					vptr, vlen);
				if (p == HCL_NULL) return HCL_NULL;

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
						HCL_ASSERT (rbt->hcl, pair->parent->right == pair);
						pair->parent->right = p;
					}
				}
				if (!IS_NIL(rbt,pair->left)) pair->left->parent = p;
				if (!IS_NIL(rbt,pair->right)) pair->right->parent = p;

				if (pair == rbt->root) rbt->root = p;

				hcl_rbt_freepair (rbt, pair);
				return p;
			}
		}
		else
		{
			void* nvptr = vcop (rbt, vptr, vlen);
			if (nvptr == HCL_NULL) return HCL_NULL;
			VPTR(pair) = nvptr;
			VLEN(pair) = vlen;
		}

		/* free up the old value */
		if (rbt->style->freeer[HCL_RBT_VAL] != HCL_NULL)
		{
			rbt->style->freeer[HCL_RBT_VAL] (rbt, ovptr, ovlen);
		}
	}

	return pair;
}

static hcl_rbt_pair_t* insert (
	hcl_rbt_t* rbt, void* kptr, hcl_oow_t klen, void* vptr, hcl_oow_t vlen, int opt)
{
	hcl_rbt_pair_t* x_cur = rbt->root;
	hcl_rbt_pair_t* x_par = HCL_NULL;
	hcl_rbt_pair_t* x_new;

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
					return HCL_NULL;
			}
		}

		x_par = x_cur;

		if (n > 0) x_cur = x_cur->right;
		else /* if (n < 0) */ x_cur = x_cur->left;
	}

	if (opt == UPDATE) return HCL_NULL;

	x_new = hcl_rbt_allocpair (rbt, kptr, klen, vptr, vlen);
	if (x_new == HCL_NULL) return HCL_NULL;

	if (x_par == HCL_NULL)
	{
		/* the tree contains no pair */
		HCL_ASSERT (rbt->hcl, rbt->root == &rbt->xnil);
		rbt->root = x_new;
	}
	else
	{
		/* perform normal binary insert */
		int n = rbt->style->comper (rbt, kptr, klen, KPTR(x_par), KLEN(x_par));
		if (n > 0)
		{
			HCL_ASSERT (rbt->hcl, x_par->right == &rbt->xnil);
			x_par->right = x_new;
		}
		else
		{
			HCL_ASSERT (rbt->hcl, x_par->left == &rbt->xnil);
			x_par->left = x_new;
		}

		x_new->parent = x_par;
		adjust (rbt, x_new);
	}

	rbt->root->color = HCL_RBT_BLACK;
	rbt->size++;
	return x_new;
}

hcl_rbt_pair_t* hcl_rbt_upsert (
	hcl_rbt_t* rbt, void* kptr, hcl_oow_t klen, void* vptr, hcl_oow_t vlen)
{
	return insert (rbt, kptr, klen, vptr, vlen, UPSERT);
}

hcl_rbt_pair_t* hcl_rbt_ensert (
	hcl_rbt_t* rbt, void* kptr, hcl_oow_t klen, void* vptr, hcl_oow_t vlen)
{
	return insert (rbt, kptr, klen, vptr, vlen, ENSERT);
}

hcl_rbt_pair_t* hcl_rbt_insert (
	hcl_rbt_t* rbt, void* kptr, hcl_oow_t klen, void* vptr, hcl_oow_t vlen)
{
	return insert (rbt, kptr, klen, vptr, vlen, INSERT);
}


hcl_rbt_pair_t* hcl_rbt_update (
	hcl_rbt_t* rbt, void* kptr, hcl_oow_t klen, void* vptr, hcl_oow_t vlen)
{
	return insert (rbt, kptr, klen, vptr, vlen, UPDATE);
}

hcl_rbt_pair_t* hcl_rbt_cbsert (
	hcl_rbt_t* rbt, void* kptr, hcl_oow_t klen, cbserter_t cbserter, void* ctx)
{
	hcl_rbt_pair_t* x_cur = rbt->root;
	hcl_rbt_pair_t* x_par = HCL_NULL;
	hcl_rbt_pair_t* x_new;

	while (!IS_NIL(rbt,x_cur))
	{
		int n = rbt->style->comper (rbt, kptr, klen, KPTR(x_cur), KLEN(x_cur));
		if (n == 0)
		{
			/* back up the contents of the current pair
			 * in case it is reallocated */
			hcl_rbt_pair_t tmp;

			tmp = *x_cur;

			/* call the callback function to manipulate the pair */
			x_new = cbserter (rbt, x_cur, kptr, klen, ctx);
			if (x_new == HCL_NULL)
			{
				/* error returned by the callback function */
				return HCL_NULL;
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
						HCL_ASSERT (rbt->hcl, tmp.parent->right == x_cur);
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

	x_new = cbserter (rbt, HCL_NULL, kptr, klen, ctx);
	if (x_new == HCL_NULL) return HCL_NULL;

	if (x_par == HCL_NULL)
	{
		/* the tree contains no pair */
		HCL_ASSERT (rbt->hcl, rbt->root == &rbt->xnil);
		rbt->root = x_new;
	}
	else
	{
		/* perform normal binary insert */
		int n = rbt->style->comper (rbt, kptr, klen, KPTR(x_par), KLEN(x_par));
		if (n > 0)
		{
			HCL_ASSERT (rbt->hcl, x_par->right == &rbt->xnil);
			x_par->right = x_new;
		}
		else
		{
			HCL_ASSERT (rbt->hcl, x_par->left == &rbt->xnil);
			x_par->left = x_new;
		}

		x_new->parent = x_par;
		adjust (rbt, x_new);
	}

	rbt->root->color = HCL_RBT_BLACK;
	rbt->size++;
	return x_new;
}


static void adjust_for_delete (hcl_rbt_t* rbt, hcl_rbt_pair_t* pair, hcl_rbt_pair_t* par)
{
	while (pair != rbt->root && pair->color == HCL_RBT_BLACK)
	{
		hcl_rbt_pair_t* tmp;

		if (pair == par->left)
		{
			tmp = par->right;
			if (tmp->color == HCL_RBT_RED)
			{
				tmp->color = HCL_RBT_BLACK;
				par->color = HCL_RBT_RED;
				rotate_left (rbt, par);
				tmp = par->right;
			}

			if (tmp->left->color == HCL_RBT_BLACK &&
			    tmp->right->color == HCL_RBT_BLACK)
			{
				if (!IS_NIL(rbt,tmp)) tmp->color = HCL_RBT_RED;
				pair = par;
				par = pair->parent;
			}
			else
			{
				if (tmp->right->color == HCL_RBT_BLACK)
				{
					if (!IS_NIL(rbt,tmp->left))
						tmp->left->color = HCL_RBT_BLACK;
					tmp->color = HCL_RBT_RED;
					rotate_right (rbt, tmp);
					tmp = par->right;
				}

				tmp->color = par->color;
				if (!IS_NIL(rbt,par)) par->color = HCL_RBT_BLACK;
				if (tmp->right->color == HCL_RBT_RED)
					tmp->right->color = HCL_RBT_BLACK;

				rotate_left (rbt, par);
				pair = rbt->root;
			}
		}
		else
		{
			HCL_ASSERT (rbt->hcl, pair == par->right);
			tmp = par->left;
			if (tmp->color == HCL_RBT_RED)
			{
				tmp->color = HCL_RBT_BLACK;
				par->color = HCL_RBT_RED;
				rotate_right (rbt, par);
				tmp = par->left;
			}

			if (tmp->left->color == HCL_RBT_BLACK &&
			    tmp->right->color == HCL_RBT_BLACK)
			{
				if (!IS_NIL(rbt,tmp)) tmp->color = HCL_RBT_RED;
				pair = par;
				par = pair->parent;
			}
			else
			{
				if (tmp->left->color == HCL_RBT_BLACK)
				{
					if (!IS_NIL(rbt,tmp->right))
						tmp->right->color = HCL_RBT_BLACK;
					tmp->color = HCL_RBT_RED;
					rotate_left (rbt, tmp);
					tmp = par->left;
				}
				tmp->color = par->color;
				if (!IS_NIL(rbt,par)) par->color = HCL_RBT_BLACK;
				if (tmp->left->color == HCL_RBT_RED)
					tmp->left->color = HCL_RBT_BLACK;

				rotate_right (rbt, par);
				pair = rbt->root;
			}
		}
	}

	pair->color = HCL_RBT_BLACK;
}

static void delete_pair (hcl_rbt_t* rbt, hcl_rbt_pair_t* pair)
{
	hcl_rbt_pair_t* x, * y, * par;

	HCL_ASSERT (rbt->hcl, pair && !IS_NIL(rbt,pair));

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
		if (y->color == HCL_RBT_BLACK && !IS_NIL(rbt,x))
			adjust_for_delete (rbt, x, par);

		hcl_rbt_freepair (rbt, y);
	}
	else
	{
		if (y->color == HCL_RBT_BLACK && !IS_NIL(rbt,x))
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

		hcl_rbt_freepair (rbt, pair);
	}

	rbt->size--;
}

int hcl_rbt_delete (hcl_rbt_t* rbt, const void* kptr, hcl_oow_t klen)
{
	hcl_rbt_pair_t* pair;

	pair = hcl_rbt_search (rbt, kptr, klen);
	if (pair == HCL_NULL) return -1;

	delete_pair (rbt, pair);
	return 0;
}

void hcl_rbt_clear (hcl_rbt_t* rbt)
{
	/* TODO: improve this */
	while (!IS_NIL(rbt,rbt->root)) delete_pair (rbt, rbt->root);
}

#if 0
static HCL_INLINE hcl_rbt_walk_t walk_recursively (
	hcl_rbt_t* rbt, walker_t walker, void* ctx, hcl_rbt_pair_t* pair)
{
	if (!IS_NIL(rbt,pair->left))
	{
		if (walk_recursively (rbt, walker, ctx, pair->left) == HCL_RBT_WALK_STOP)
			return HCL_RBT_WALK_STOP;
	}

	if (walker (rbt, pair, ctx) == HCL_RBT_WALK_STOP) return HCL_RBT_WALK_STOP;

	if (!IS_NIL(rbt,pair->right))
	{
		if (walk_recursively (rbt, walker, ctx, pair->right) == HCL_RBT_WALK_STOP)
			return HCL_RBT_WALK_STOP;
	}

	return HCL_RBT_WALK_FORWARD;
}
#endif

static HCL_INLINE void walk (hcl_rbt_t* rbt, walker_t walker, void* ctx, int l, int r)
{
	hcl_rbt_pair_t* x_cur = rbt->root;
	hcl_rbt_pair_t* prev = rbt->root->parent;

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
				if (walker (rbt, x_cur, ctx) == HCL_RBT_WALK_STOP) break;

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

			if (walker (rbt, x_cur, ctx) == HCL_RBT_WALK_STOP) break;

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
			HCL_ASSERT (rbt->hcl, prev == x_cur->child[r]);
			/* just move up to the parent */
			prev = x_cur;
			x_cur = x_cur->parent;
		}
	}
}

void hcl_rbt_walk (hcl_rbt_t* rbt, walker_t walker, void* ctx)
{
	walk (rbt, walker, ctx, LEFT, RIGHT);
}

void hcl_rbt_rwalk (hcl_rbt_t* rbt, walker_t walker, void* ctx)
{
	walk (rbt, walker, ctx, RIGHT, LEFT);
}

int hcl_rbt_dflcomp (const hcl_rbt_t* rbt, const void* kptr1, hcl_oow_t klen1, const void* kptr2, hcl_oow_t klen2)
{
	hcl_oow_t min;
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

	n = HCL_MEMCMP (kptr1, kptr2, KTOB(rbt,min));
	if (n == 0) n = nn;
	return n;
}

