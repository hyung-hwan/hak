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

#ifndef _HAK_RBT_H_
#define _HAK_RBT_H_

#include <hak-cmn.h>

/** \file
 * This file provides a red-black tree encapsulated in the #hak_rbt_t type that
 * implements a self-balancing binary search tree.Its interface is very close
 * to #hak_htb_t.
 *
 * This sample code adds a series of keys and values and print them
 * in descending key order.
 * \code
 * #include <hak-rbt.h>
 *
 * static hak_rbt_walk_t walk (hak_rbt_t* rbt, hak_rbt_pair_t* pair, void* ctx)
 * {
 *   hak_printf (HAK_T("key = %d, value = %d\n"),
 *     *(int*)HAK_RBT_KPTR(pair), *(int*)HAK_RBT_VPTR(pair));
 *   return HAK_RBT_WALK_FORWARD;
 * }
 *
 * int main ()
 * {
 *   hak_rbt_t* s1;
 *   int i;
 *
 *   s1 = hak_rbt_open(HAK_MMGR_GETDFL(), 0, 1, 1); // error handling skipped
 *   hak_rbt_setstyle(s1, hak_get_rbt_style(HAK_RBT_STYLE_INLINE_COPIERS));
 *
 *   for (i = 0; i < 20; i++)
 *   {
 *     int x = i * 20;
 *     hak_rbt_insert (s1, &i, HAK_SIZEOF(i), &x, HAK_SIZEOF(x)); // eror handling skipped
 *   }
 *
 *   hak_rbt_rwalk (s1, walk, HAK_NULL);
 *
 *   hak_rbt_close (s1);
 *   return 0;
 * }
 * \endcode
 */

typedef struct hak_rbt_t hak_rbt_t;
typedef struct hak_rbt_pair_t hak_rbt_pair_t;

/**
 * The hak_rbt_walk_t type defines values that the callback function can
 * return to control hak_rbt_walk() and hak_rbt_rwalk().
 */
enum hak_rbt_walk_t
{
	HAK_RBT_WALK_STOP    = 0,
	HAK_RBT_WALK_FORWARD = 1
};
typedef enum hak_rbt_walk_t hak_rbt_walk_t;

/**
 * The hak_rbt_id_t type defines IDs to indicate a key or a value in various
 * functions
 */
enum hak_rbt_id_t
{
	HAK_RBT_KEY = 0, /**< indicate a key */
	HAK_RBT_VAL = 1  /**< indicate a value */
};
typedef enum hak_rbt_id_t hak_rbt_id_t;

/**
 * The hak_rbt_copier_t type defines a pair contruction callback.
 */
typedef void* (*hak_rbt_copier_t) (
	hak_rbt_t* rbt  /**< red-black tree */,
	void*      dptr /**< pointer to a key or a value */,
	hak_oow_t  dlen /**< length of a key or a value */
);

/**
 * The hak_rbt_freeer_t defines a key/value destruction callback.
 */
typedef void (*hak_rbt_freeer_t) (
	hak_rbt_t* rbt,  /**< red-black tree */
	void*      dptr, /**< pointer to a key or a value */
	hak_oow_t  dlen  /**< length of a key or a value */
);

/**
 * The hak_rbt_comper_t type defines a key comparator that is called when
 * the rbt needs to compare keys. A red-black tree is created with a default
 * comparator which performs bitwise comparison of two keys.
 * The comparator should return 0 if the keys are the same, 1 if the first
 * key is greater than the second key, -1 otherwise.
 */
typedef int (*hak_rbt_comper_t) (
	const hak_rbt_t* rbt,    /**< red-black tree */
	const void*      kptr1,  /**< key pointer */
	hak_oow_t        klen1,  /**< key length */
	const void*      kptr2,  /**< key pointer */
	hak_oow_t        klen2   /**< key length */
);

/**
 * The hak_rbt_keeper_t type defines a value keeper that is called when
 * a value is retained in the context that it should be destroyed because
 * it is identical to a new value. Two values are identical if their
 * pointers and lengths are equal.
 */
typedef void (*hak_rbt_keeper_t) (
	hak_rbt_t* rbt,    /**< red-black tree */
	void*       vptr,   /**< value pointer */
	hak_oow_t vlen    /**< value length */
);

/**
 * The hak_rbt_walker_t defines a pair visitor.
 */
typedef hak_rbt_walk_t (*hak_rbt_walker_t) (
	hak_rbt_t*      rbt,   /**< red-black tree */
	hak_rbt_pair_t* pair,  /**< pointer to a key/value pair */
	void*            ctx    /**< pointer to user-defined data */
);

/**
 * The hak_rbt_cbserter_t type defines a callback function for hak_rbt_cbsert().
 * The hak_rbt_cbserter() function calls it to allocate a new pair for the
 * key pointed to by \a kptr of the length \a klen and the callback context
 * \a ctx. The second parameter \a pair is passed the pointer to the existing
 * pair for the key or #HAK_NULL in case of no existing key. The callback
 * must return a pointer to a new or a reallocated pair. When reallocating the
 * existing pair, this callback must destroy the existing pair and return the
 * newly reallocated pair. It must return #HAK_NULL for failure.
 */
typedef hak_rbt_pair_t* (*hak_rbt_cbserter_t) (
	hak_rbt_t*      rbt,    /**< red-black tree */
	hak_rbt_pair_t* pair,   /**< pair pointer */
	void*           kptr,   /**< key pointer */
	hak_oow_t       klen,   /**< key length */
	void*           ctx     /**< callback context */
);


enum hak_rbt_pair_color_t
{
	HAK_RBT_RED,
	HAK_RBT_BLACK
};
typedef enum hak_rbt_pair_color_t hak_rbt_pair_color_t;

/**
 * The hak_rbt_pair_t type defines red-black tree pair. A pair is composed
 * of a key and a value. It maintains pointers to the beginning of a key and
 * a value plus their length. The length is scaled down with the scale factor
 * specified in an owning tree. Use macros defined in the
 */
struct hak_rbt_pair_t
{
	struct
	{
		void*     ptr;
		hak_oow_t len;
	} key;

	struct
	{
		void*       ptr;
		hak_oow_t len;
	} val;

	/* management information below */
	hak_rbt_pair_color_t color;
	hak_rbt_pair_t* parent;
	hak_rbt_pair_t* child[2]; /* left and right */
};

typedef struct hak_rbt_style_t hak_rbt_style_t;

/**
 * The hak_rbt_style_t type defines callback function sets for key/value
 * pair manipulation.
 */
struct hak_rbt_style_t
{
	hak_rbt_copier_t copier[2]; /**< key and value copier */
	hak_rbt_freeer_t freeer[2]; /**< key and value freeer */
	hak_rbt_comper_t comper;    /**< key comparator */
	hak_rbt_keeper_t keeper;    /**< value keeper */
};

/**
 * The hak_rbt_style_kind_t type defines the type of predefined
 * callback set for pair manipulation.
 */
enum hak_rbt_style_kind_t
{
	/** store the key and the value pointer */
	HAK_RBT_STYLE_DEFAULT,
	/** copy both key and value into the pair */
	HAK_RBT_STYLE_INLINE_COPIERS,
	/** copy the key into the pair but store the value pointer */
	HAK_RBT_STYLE_INLINE_KEY_COPIER,
	/** copy the value into the pair but store the key pointer */
	HAK_RBT_STYLE_INLINE_VALUE_COPIER
};

typedef enum hak_rbt_style_kind_t  hak_rbt_style_kind_t;

/**
 * The hak_rbt_t type defines a red-black tree.
 */
struct hak_rbt_t
{
	hak_t*                 hak;
	const hak_rbt_style_t* style;
	hak_oob_t              scale[2];  /**< length scale */
	hak_rbt_pair_t         xnil;      /**< internal nil node */
	hak_oow_t              size;      /**< number of pairs */
	hak_rbt_pair_t*        root;      /**< root pair */
};

/**
 * The HAK_RBT_COPIER_SIMPLE macros defines a copier that remembers the
 * pointer and length of data in a pair.
 */
#define HAK_RBT_COPIER_SIMPLE ((hak_rbt_copier_t)1)

/**
 * The HAK_RBT_COPIER_INLINE macros defines a copier that copies data into
 * a pair.
 */
#define HAK_RBT_COPIER_INLINE ((hak_rbt_copier_t)2)

#define HAK_RBT_COPIER_DEFAULT (HAK_RBT_COPIER_SIMPLE)
#define HAK_RBT_FREEER_DEFAULT (HAK_NULL)
#define HAK_RBT_COMPER_DEFAULT (hak_rbt_dflcomp)
#define HAK_RBT_KEEPER_DEFAULT (HAK_NULL)

/**
 * The HAK_RBT_SIZE() macro returns the number of pairs in red-black tree.
 */
#define HAK_RBT_SIZE(m)   ((const hak_oow_t)(m)->size)
#define HAK_RBT_KSCALE(m) ((const int)(m)->scale[HAK_RBT_KEY])
#define HAK_RBT_VSCALE(m) ((const int)(m)->scale[HAK_RBT_VAL])

#define HAK_RBT_KPTL(p) (&(p)->key)
#define HAK_RBT_VPTL(p) (&(p)->val)

#define HAK_RBT_KPTR(p) ((p)->key.ptr)
#define HAK_RBT_KLEN(p) ((p)->key.len)
#define HAK_RBT_VPTR(p) ((p)->val.ptr)
#define HAK_RBT_VLEN(p) ((p)->val.len)

#define HAK_RBT_NEXT(p) ((p)->next)

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * The hak_get_rbt_style() functions returns a predefined callback set for
 * pair manipulation.
 */
HAK_EXPORT const hak_rbt_style_t* hak_get_rbt_style (
	hak_rbt_style_kind_t kind
);

/**
 * The hak_rbt_open() function creates a red-black tree.
 * \return hak_rbt_t pointer on success, HAK_NULL on failure.
 */
HAK_EXPORT hak_rbt_t* hak_rbt_open (
	hak_t*      hak,
	hak_oow_t   xtnsize, /**< extension size in bytes */
	int         kscale,  /**< key scale */
	int         vscale   /**< value scale */
);

/**
 * The hak_rbt_close() function destroys a red-black tree.
 */
HAK_EXPORT void hak_rbt_close (
	hak_rbt_t* rbt   /**< red-black tree */
);

/**
 * The hak_rbt_init() function initializes a red-black tree
 */
HAK_EXPORT int hak_rbt_init (
	hak_rbt_t*  rbt,    /**< red-black tree */
	hak_t*      hak,
	int         kscale, /**< key scale */
	int         vscale  /**< value scale */
);

/**
 * The hak_rbt_fini() funtion finalizes a red-black tree
 */
HAK_EXPORT void hak_rbt_fini (
	hak_rbt_t* rbt  /**< red-black tree */
);

HAK_EXPORT void* hak_rbt_getxtn (
	hak_rbt_t* rbt
);

/**
 * The hak_rbt_getstyle() function gets manipulation callback function set.
 */
HAK_EXPORT const hak_rbt_style_t* hak_rbt_getstyle (
	const hak_rbt_t* rbt /**< red-black tree */
);

/**
 * The hak_rbt_setstyle() function sets internal manipulation callback
 * functions for data construction, destruction, comparison, etc.
 * The callback structure pointed to by \a style must outlive the tree
 * pointed to by \a htb as the tree doesn't copy the contents of the
 * structure.
 */
HAK_EXPORT void hak_rbt_setstyle (
	hak_rbt_t*             rbt,  /**< red-black tree */
	const hak_rbt_style_t* style /**< callback function set */
);

/**
 * The hak_rbt_getsize() function gets the number of pairs in red-black tree.
 */
HAK_EXPORT hak_oow_t hak_rbt_getsize (
	const hak_rbt_t* rbt  /**< red-black tree */
);

/**
 * The hak_rbt_search() function searches red-black tree to find a pair with a
 * matching key. It returns the pointer to the pair found. If it fails
 * to find one, it returns HAK_NULL.
 * \return pointer to the pair with a maching key,
 *         or HAK_NULL if no match is found.
 */
HAK_EXPORT hak_rbt_pair_t* hak_rbt_search (
	const hak_rbt_t* rbt,   /**< red-black tree */
	const void*      kptr,  /**< key pointer */
	hak_oow_t        klen   /**< the size of the key */
);

/**
 * The hak_rbt_upsert() function searches red-black tree for the pair with a
 * matching key. If one is found, it updates the pair. Otherwise, it inserts
 * a new pair with the key and the value given. It returns the pointer to the
 * pair updated or inserted.
 * \return a pointer to the updated or inserted pair on success,
 *         HAK_NULL on failure.
 */
HAK_EXPORT hak_rbt_pair_t* hak_rbt_upsert (
	hak_rbt_t* rbt,   /**< red-black tree */
	void*      kptr,  /**< key pointer */
	hak_oow_t  klen,  /**< key length */
	void*      vptr,  /**< value pointer */
	hak_oow_t  vlen   /**< value length */
);

/**
 * The hak_rbt_ensert() function inserts a new pair with the key and the value
 * given. If there exists a pair with the key given, the function returns
 * the pair containing the key.
 * \return pointer to a pair on success, HAK_NULL on failure.
 */
HAK_EXPORT hak_rbt_pair_t* hak_rbt_ensert (
	hak_rbt_t* rbt,   /**< red-black tree */
	void*      kptr,  /**< key pointer */
	hak_oow_t  klen,  /**< key length */
	void*      vptr,  /**< value pointer */
	hak_oow_t  vlen   /**< value length */
);

/**
 * The hak_rbt_insert() function inserts a new pair with the key and the value
 * given. If there exists a pair with the key given, the function returns
 * HAK_NULL without channging the value.
 * \return pointer to the pair created on success, HAK_NULL on failure.
 */
HAK_EXPORT hak_rbt_pair_t* hak_rbt_insert (
	hak_rbt_t* rbt,   /**< red-black tree */
	void*      kptr,  /**< key pointer */
	hak_oow_t  klen,  /**< key length */
	void*      vptr,  /**< value pointer */
	hak_oow_t  vlen   /**< value length */
);

/**
 * The hak_rbt_update() function updates the value of an existing pair
 * with a matching key.
 * \return pointer to the pair on success, HAK_NULL on no matching pair
 */
HAK_EXPORT hak_rbt_pair_t* hak_rbt_update (
	hak_rbt_t* rbt,   /**< red-black tree */
	void*      kptr,  /**< key pointer */
	hak_oow_t  klen,  /**< key length */
	void*      vptr,  /**< value pointer */
	hak_oow_t  vlen   /**< value length */
);

/**
 * The hak_rbt_cbsert() function inserts a key/value pair by delegating pair
 * allocation to a callback function. Depending on the callback function,
 * it may behave like hak_rbt_insert(), hak_rbt_upsert(), hak_rbt_update(),
 * hak_rbt_ensert(), or totally differently. The sample code below inserts
 * a new pair if the key is not found and appends the new value to the
 * existing value delimited by a comma if the key is found.
 *
 * \code
 * hak_rbt_walk_t print_map_pair (hak_rbt_t* map, hak_rbt_pair_t* pair, void* ctx)
 * {
 *   hak_printf (HAK_T("%.*s[%d] => %.*s[%d]\n"),
 *     (int)HAK_RBT_KLEN(pair), HAK_RBT_KPTR(pair), (int)HAK_RBT_KLEN(pair),
 *     (int)HAK_RBT_VLEN(pair), HAK_RBT_VPTR(pair), (int)HAK_RBT_VLEN(pair));
 *   return HAK_RBT_WALK_FORWARD;
 * }
 *
 * hak_rbt_pair_t* cbserter (
 *   hak_rbt_t* rbt, hak_rbt_pair_t* pair,
 *   void* kptr, hak_oow_t klen, void* ctx)
 * {
 *   hak_cstr_t* v = (hak_cstr_t*)ctx;
 *   if (pair == HAK_NULL)
 *   {
 *     // no existing key for the key
 *     return hak_rbt_allocpair (rbt, kptr, klen, v->ptr, v->len);
 *   }
 *   else
 *   {
 *     // a pair with the key exists.
 *     // in this sample, i will append the new value to the old value
 *     // separated by a comma
 *     hak_rbt_pair_t* new_pair;
 *     hak_ooch_t comma = HAK_T(',');
 *     hak_oob_t* vptr;
 *
 *     // allocate a new pair, but without filling the actual value.
 *     // note vptr is given HAK_NULL for that purpose
 *     new_pair = hak_rbt_allocpair (
 *       rbt, kptr, klen, HAK_NULL, pair->vlen + 1 + v->len);
 *     if (new_pair == HAK_NULL) return HAK_NULL;
 *
 *     // fill in the value space
 *     vptr = new_pair->vptr;
 *     hak_memcpy (vptr, pair->vptr, pair->vlen*HAK_SIZEOF(hak_ooch_t));
 *     vptr += pair->vlen*HAK_SIZEOF(hak_ooch_t);
 *     hak_memcpy (vptr, &comma, HAK_SIZEOF(hak_ooch_t));
 *     vptr += HAK_SIZEOF(hak_ooch_t);
 *     hak_memcpy (vptr, v->ptr, v->len*HAK_SIZEOF(hak_ooch_t));
 *
 *     // this callback requires the old pair to be destroyed
 *     hak_rbt_freepair (rbt, pair);
 *
 *     // return the new pair
 *     return new_pair;
 *   }
 * }
 *
 * int main ()
 * {
 *   hak_rbt_t* s1;
 *   int i;
 *   hak_ooch_t* keys[] = { HAK_T("one"), HAK_T("two"), HAK_T("three") };
 *   hak_ooch_t* vals[] = { HAK_T("1"), HAK_T("2"), HAK_T("3"), HAK_T("4"), HAK_T("5") };
 *
 *   s1 = hak_rbt_open (
 *     HAK_MMGR_GETDFL(), 0,
 *     HAK_SIZEOF(hak_ooch_t), HAK_SIZEOF(hak_ooch_t)
 *   ); // note error check is skipped
 *   hak_rbt_setstyle (s1, &style1);
 *
 *   for (i = 0; i < HAK_COUNTOF(vals); i++)
 *   {
 *     hak_cstr_t ctx;
 *     ctx.ptr = vals[i]; ctx.len = hak_strlen(vals[i]);
 *     hak_rbt_cbsert (s1,
 *       keys[i%HAK_COUNTOF(keys)], hak_strlen(keys[i%HAK_COUNTOF(keys)]),
 *       cbserter, &ctx
 *     ); // note error check is skipped
 *   }
 *   hak_rbt_walk (s1, print_map_pair, HAK_NULL);
 *
 *   hak_rbt_close (s1);
 *   return 0;
 * }
 * \endcode
 */
HAK_EXPORT hak_rbt_pair_t* hak_rbt_cbsert (
	hak_rbt_t*         rbt,      /**< red-black tree */
	void*              kptr,     /**< key pointer */
	hak_oow_t          klen,     /**< key length */
	hak_rbt_cbserter_t cbserter, /**< callback function */
	void*              ctx       /**< callback context */
);

/**
 * The hak_rbt_delete() function deletes a pair with a matching key
 * \return 0 on success, -1 on failure
 */
HAK_EXPORT int hak_rbt_delete (
	hak_rbt_t*  rbt,   /**< red-black tree */
	const void* kptr,  /**< key pointer */
	hak_oow_t   klen   /**< key size */
);

/**
 * The hak_rbt_clear() function empties a red-black tree.
 */
HAK_EXPORT void hak_rbt_clear (
	hak_rbt_t* rbt /**< red-black tree */
);

/**
 * The hak_rbt_walk() function traverses a red-black tree in preorder
 * from the leftmost child.
 */
HAK_EXPORT void hak_rbt_walk (
	hak_rbt_t*       rbt,    /**< red-black tree */
	hak_rbt_walker_t walker, /**< callback function for each pair */
	void*            ctx     /**< pointer to user-specific data */
);

/**
 * The hak_rbt_walk() function traverses a red-black tree in preorder
 * from the rightmost child.
 */
HAK_EXPORT void hak_rbt_rwalk (
	hak_rbt_t*       rbt,    /**< red-black tree */
	hak_rbt_walker_t walker, /**< callback function for each pair */
	void*            ctx     /**< pointer to user-specific data */
);

/**
 * The hak_rbt_allocpair() function allocates a pair for a key and a value
 * given. But it does not chain the pair allocated into the red-black tree \a rbt.
 * Use this function at your own risk.
 *
 * Take note of he following special behavior when the copier is
 * #HAK_RBT_COPIER_INLINE.
 * - If \a kptr is #HAK_NULL, the key space of the size \a klen is reserved but
 *   not propagated with any data.
 * - If \a vptr is #HAK_NULL, the value space of the size \a vlen is reserved
 *   but not propagated with any data.
 */
HAK_EXPORT hak_rbt_pair_t* hak_rbt_allocpair (
	hak_rbt_t*  rbt,
	void*       kptr,
	hak_oow_t   klen,
	void*       vptr,
	hak_oow_t   vlen
);

/**
 * The hak_rbt_freepair() function destroys a pair. But it does not detach
 * the pair destroyed from the red-black tree \a rbt. Use this function at your
 * own risk.
 */
HAK_EXPORT void hak_rbt_freepair (
	hak_rbt_t*      rbt,
	hak_rbt_pair_t* pair
);

/**
 * The hak_rbt_dflcomp() function defines the default key comparator.
 */
HAK_EXPORT int hak_rbt_dflcomp (
	const hak_rbt_t* rbt,
	const void*      kptr1,
	hak_oow_t        klen1,
	const void*      kptr2,
	hak_oow_t        klen2
);

#if defined(__cplusplus)
}
#endif

#endif
