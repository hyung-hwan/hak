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

#if (HAK_LIW_BITS == HAK_OOW_BITS)
	/* nothing special */
#elif (HAK_LIW_BITS == HAK_OOHW_BITS)
#	define MAKE_WORD(hw1,hw2) ((hak_oow_t)(hw1) | (hak_oow_t)(hw2) << HAK_LIW_BITS)
#else
#	error UNSUPPORTED LIW BIT SIZE
#endif

#define IS_SIGN_DIFF(x,y) (((x) ^ (y)) < 0)

/*#define IS_POW2(ui) (((ui) > 0) && (((ui) & (~(ui)+ 1)) == (ui)))*/
#define IS_POW2(ui) (((ui) > 0) && ((ui) & ((ui) - 1)) == 0)

/* digit character array */
static const char* _digitc_array[] =
{
	"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",
	"0123456789abcdefghijklmnopqrstuvwxyz"
};

/* exponent table for pow2 between 1 and 32 inclusive. */
static hak_uint8_t _exp_tab[32] =
{
	0, 1, 0, 2, 0, 0, 0, 3,
	0, 0, 0, 0, 0, 0, 0, 4,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 5
};

static const hak_uint8_t debruijn_32[32] =
{
	0, 1, 28, 2, 29, 14, 24, 3,
	30, 22, 20, 15, 25, 17, 4, 8,
	31, 27, 13, 23, 21, 19, 16, 7,
	26, 12, 18, 6, 11, 5, 10, 9
};

static const hak_uint8_t debruijn_64[64] =
{
	0, 1,  2, 53,  3,  7, 54, 27,
	4, 38, 41,  8, 34, 55, 48, 28,
	62,  5, 39, 46, 44, 42, 22,  9,
	24, 35, 59, 56, 49, 18, 29, 11,
	63, 52,  6, 26, 37, 40, 33, 47,
	61, 45, 43, 21, 23, 58, 17, 10,
	51, 25, 36, 32, 60, 20, 57, 16,
	50, 31, 19, 15, 30, 14, 13, 12
};

#define make_pbigint(hak, ptr, len) (hak_instantiate(hak, hak->c_large_positive_integer, ptr, len))
#define make_nbigint(hak, ptr, len) (hak_instantiate(hak, hak->c_large_negative_integer, ptr, len))

#if defined(HAK_HAVE_UINT32_T)
#	define LOG2_FOR_POW2_32(x) (debruijn_32[(hak_uint32_t)((hak_uint32_t)(x) * 0x077CB531) >> 27])
#endif

#if defined(HAK_HAVE_UINT64_T)
#	define LOG2_FOR_POW2_64(x) (debruijn_64[(hak_uint64_t)((hak_uint64_t)(x) * 0x022fdd63cc95386d) >> 58])
#endif

#if defined(HAK_HAVE_UINT32_T) && (HAK_SIZEOF_OOW_T == HAK_SIZEOF_UINT32_T)
#	define LOG2_FOR_POW2(x) LOG2_FOR_POW2_32(x)
#elif defined(HAK_HAVE_UINT64_T) && (HAK_SIZEOF_OOW_T == HAK_SIZEOF_UINT64_T)
#	define LOG2_FOR_POW2(x) LOG2_FOR_POW2_64(x)
#else
#	define LOG2_FOR_POW2(x) hak_get_pos_of_msb_set_pow2(x)
#endif

#if (HAK_SIZEOF_OOW_T == HAK_SIZEOF_INT) && defined(HAK_HAVE_BUILTIN_UADD_OVERFLOW)
#	define oow_add_overflow(a,b,c)  __builtin_uadd_overflow(a,b,c)
#elif (HAK_SIZEOF_OOW_T == HAK_SIZEOF_LONG) && defined(HAK_HAVE_BUILTIN_UADDL_OVERFLOW)
#	define oow_add_overflow(a,b,c)  __builtin_uaddl_overflow(a,b,c)
#elif (HAK_SIZEOF_OOW_T == HAK_SIZEOF_LONG_LONG) && defined(HAK_HAVE_BUILTIN_UADDLL_OVERFLOW)
#	define oow_add_overflow(a,b,c)  __builtin_uaddll_overflow(a,b,c)
#else
static HAK_INLINE int oow_add_overflow (hak_oow_t a, hak_oow_t b, hak_oow_t* c)
{
	*c = a + b;
	return b > HAK_TYPE_MAX(hak_oow_t) - a;
}
#endif

#if (HAK_SIZEOF_OOW_T == HAK_SIZEOF_INT) && defined(HAK_HAVE_BUILTIN_UMUL_OVERFLOW)
#	define oow_mul_overflow(a,b,c)  __builtin_umul_overflow(a,b,c)
#elif (HAK_SIZEOF_OOW_T == HAK_SIZEOF_LONG) && defined(HAK_HAVE_BUILTIN_UMULL_OVERFLOW)
#	define oow_mul_overflow(a,b,c)  __builtin_umull_overflow(a,b,c)
#elif (HAK_SIZEOF_OOW_T == HAK_SIZEOF_LONG_LONG) && defined(HAK_HAVE_BUILTIN_UMULLL_OVERFLOW)
#	define oow_mul_overflow(a,b,c)  __builtin_umulll_overflow(a,b,c)
#else
static HAK_INLINE int oow_mul_overflow (hak_oow_t a, hak_oow_t b, hak_oow_t* c)
{
#if (HAK_SIZEOF_UINTMAX_T > HAK_SIZEOF_OOW_T)
	hak_uintmax_t k;
	k = (hak_uintmax_t)a * (hak_uintmax_t)b;
	*c = (hak_oow_t)k;
	return (k >> HAK_OOW_BITS) > 0;
	/*return k > HAK_TYPE_MAX(hak_oow_t);*/
#else
	*c = a * b;
	return b != 0 && a > HAK_TYPE_MAX(hak_oow_t) / b; /* works for unsigned types only */
#endif
}
#endif

#if (HAK_SIZEOF_OOI_T == HAK_SIZEOF_INT) && defined(HAK_HAVE_BUILTIN_SMUL_OVERFLOW)
#	define shaki_mul_overflow(hak,a,b,c)  __builtin_smul_overflow(a,b,c)
#elif (HAK_SIZEOF_OOI_T == HAK_SIZEOF_LONG) && defined(HAK_HAVE_BUILTIN_SMULL_OVERFLOW)
#	define shaki_mul_overflow(hak,a,b,c)  __builtin_smull_overflow(a,b,c)
#elif (HAK_SIZEOF_OOI_T == HAK_SIZEOF_LONG_LONG) && defined(HAK_HAVE_BUILTIN_SMULLL_OVERFLOW)
#	define shaki_mul_overflow(hak,a,b,c)  __builtin_smulll_overflow(a,b,c)
#else
static HAK_INLINE int shaki_mul_overflow (hak_t* hak, hak_ooi_t a, hak_ooi_t b, hak_ooi_t* c)
{
	/* take note that this function is not supposed to handle
	 * the whole hak_ooi_t range. it handles the shaki subrange */

#if (HAK_SIZEOF_UINTMAX_T > HAK_SIZEOF_OOI_T)
	hak_intmax_t k;

	HAK_ASSERT(hak, HAK_IN_SMOOI_RANGE(a));
	HAK_ASSERT(hak, HAK_IN_SMOOI_RANGE(b));

	k = (hak_intmax_t)a * (hak_intmax_t)b;
	*c = (hak_ooi_t)k;

	return k > HAK_TYPE_MAX(hak_ooi_t) || k < HAK_TYPE_MIN(hak_ooi_t);
#else

	hak_ooi_t ua, ub;

	HAK_ASSERT(hak, HAK_IN_SMOOI_RANGE(a));
	HAK_ASSERT(hak, HAK_IN_SMOOI_RANGE(b));

	*c = a * b;

	ub = (b >= 0)? b: -b;
	ua = (a >= 0)? a: -a;

	/* though this fomula basically works for unsigned types in principle,
	 * the values used here are all absolute values and they fall in
	 * a safe range to apply this fomula. the safe range is guaranteed because
	 * the sources are supposed to be shakis. */
	return ub != 0 && ua > HAK_TYPE_MAX(hak_ooi_t) / ub;
#endif
}
#endif

#if (HAK_SIZEOF_LIW_T == HAK_SIZEOF_INT) && defined(HAK_HAVE_BUILTIN_UADD_OVERFLOW)
#	define liw_add_overflow(a,b,c)  __builtin_uadd_overflow(a,b,c)
#elif (HAK_SIZEOF_LIW_T == HAK_SIZEOF_LONG) && defined(HAK_HAVE_BUILTIN_UADDL_OVERFLOW)
#	define liw_add_overflow(a,b,c)  __builtin_uaddl_overflow(a,b,c)
#elif (HAK_SIZEOF_LIW_T == HAK_SIZEOF_LONG_LONG) && defined(HAK_HAVE_BUILTIN_UADDLL_OVERFLOW)
#	define liw_add_overflow(a,b,c)  __builtin_uaddll_overflow(a,b,c)
#else
static HAK_INLINE int liw_add_overflow (hak_liw_t a, hak_liw_t b, hak_liw_t* c)
{
	*c = a + b;
	return b > HAK_TYPE_MAX(hak_liw_t) - a;
}
#endif

#if (HAK_SIZEOF_LIW_T == HAK_SIZEOF_INT) && defined(HAK_HAVE_BUILTIN_UMUL_OVERFLOW)
#	define liw_mul_overflow(a,b,c)  __builtin_umul_overflow(a,b,c)
#elif (HAK_SIZEOF_LIW_T == HAK_SIZEOF_LONG) && defined(HAK_HAVE_BUILTIN_UMULL_OVERFLOW)
#	define liw_mul_overflow(a,b,c)  __builtin_uaddl_overflow(a,b,c)
#elif (HAK_SIZEOF_LIW_T == HAK_SIZEOF_LONG_LONG) && defined(HAK_HAVE_BUILTIN_UMULLL_OVERFLOW)
#	define liw_mul_overflow(a,b,c)  __builtin_uaddll_overflow(a,b,c)
#else
static HAK_INLINE int liw_mul_overflow (hak_liw_t a, hak_liw_t b, hak_liw_t* c)
{
#if (HAK_SIZEOF_UINTMAX_T > HAK_SIZEOF_LIW_T)
	hak_uintmax_t k;
	k = (hak_uintmax_t)a * (hak_uintmax_t)b;
	*c = (hak_liw_t)k;
	return (k >> HAK_LIW_BITS) > 0;
	/*return k > HAK_TYPE_MAX(hak_liw_t);*/
#else
	*c = a * b;
	return b != 0 && a > HAK_TYPE_MAX(hak_liw_t) / b; /* works for unsigned types only */
#endif
}
#endif

static int is_normalized_integer (hak_t* hak, hak_oop_t oop)
{
	if (HAK_OOP_IS_SMOOI(oop)) return 1;
	if (HAK_IS_BIGINT(hak,oop))
	{
		hak_oow_t sz;
		sz = HAK_OBJ_GET_SIZE(oop);
		HAK_ASSERT(hak, sz >= 1);
		return HAK_OBJ_GET_LIWORD_VAL(oop, sz - 1) != 0;
	}

	return 0;
}

static HAK_INLINE int bigint_to_oow_noseterr (hak_t* hak, hak_oop_t num, hak_oow_t* w)
{
	HAK_ASSERT(hak, HAK_IS_BIGINT(hak,num));

#if (HAK_LIW_BITS == HAK_OOW_BITS)
	HAK_ASSERT(hak, HAK_OBJ_GET_SIZE(num) >= 1);
	if (HAK_OBJ_GET_SIZE(num) == 1)
	{
		*w = HAK_OBJ_GET_WORD_VAL(num, 0);
		return HAK_IS_NBIGINT(hak,num)? -1: 1;
	}

#elif (HAK_LIW_BITS == HAK_OOHW_BITS)
	/* this function must be called with a real large integer.
	 * a real large integer is at least 2 half-word long.
	 * you must not call this function with an unnormalized
	 * large integer. */

	HAK_ASSERT(hak, HAK_OBJ_GET_SIZE(num) >= 2);
	if (HAK_OBJ_GET_SIZE(num) == 2)
	{
		*w = MAKE_WORD(HAK_OBJ_GET_HALFWORD_VAL(num, 0), HAK_OBJ_GET_HALFWORD_VAL(num, 1));
		return HAK_IS_NBIGINT(hak,num)? -1: 1;
	}
	if (HAK_OBJ_GET_SIZE(num) == 1)
	{
		/* if someone create a big number with a small integer,
		 * it can just be one half-word */
		*w = HAK_OBJ_GET_HALFWORD_VAL(num, 0);
		return HAK_IS_NBIGINT(hak,num)? -1: 1;
	}
#else
#	error UNSUPPORTED LIW BIT SIZE
#endif

	return 0; /* not convertable */
}

static HAK_INLINE int integer_to_oow_noseterr (hak_t* hak, hak_oop_t x, hak_oow_t* w)
{
	/* return value
	 *   1 - a positive number including 0 that can fit into hak_oow_t
	 *  -1 - a negative number whose absolute value can fit into hak_oow_t
	 *   0 - number too large or too small
	 */

	if (HAK_OOP_IS_SMOOI(x))
	{
		hak_ooi_t v;

		v = HAK_OOP_TO_SMOOI(x);
		if (v < 0)
		{
			*w = -v;
			return -1;
		}
		else
		{
			*w = v;
			return 1;
		}
	}

	HAK_ASSERT(hak, hak_isbigint(hak, x));
	return bigint_to_oow_noseterr(hak, x, w);
}

int hak_inttooow_noseterr (hak_t* hak, hak_oop_t x, hak_oow_t* w)
{
	if (HAK_OOP_IS_SMOOI(x))
	{
		hak_ooi_t v;

		v = HAK_OOP_TO_SMOOI(x);
		if (v < 0)
		{
			*w = -v;
			return -1; /* negative number negated - kind of an error */
		}
		else
		{
			*w = v;
			return 1; /* zero or positive number */
		}
	}

	/* 0 -> too big, too small, or not an integer */
	return hak_isbigint(hak, x)? bigint_to_oow_noseterr(hak, x, w): 0;
}

int hak_inttooow (hak_t* hak, hak_oop_t x, hak_oow_t* w)
{
	if (HAK_OOP_IS_SMOOI(x))
	{
		hak_ooi_t v;

		v = HAK_OOP_TO_SMOOI(x);
		if (v < 0)
		{
			*w = -v;
			hak_seterrnum(hak, HAK_ERANGE);
			return -1; /* negative number negated - kind of an error */
		}
		else
		{
			*w = v;
			return 1; /* zero or positive number */
		}
	}

	if (hak_isbigint(hak, x))
	{
		int n;
		if ((n = bigint_to_oow_noseterr(hak, x, w)) <= 0) hak_seterrnum(hak, HAK_ERANGE);
		return n;
	}

	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O", x);
	return 0; /* not convertable - too big, too small, or not integer */
}

int hak_inttoooi_noseterr (hak_t* hak, hak_oop_t x, hak_ooi_t* i)
{
	if (HAK_OOP_IS_SMOOI(x))
	{
		*i = HAK_OOP_TO_SMOOI(x);
		return (*i < 0)? -1: 1;
	}

	if (hak_isbigint(hak, x))
	{
		hak_oow_t w;
		int n;

		n = bigint_to_oow_noseterr(hak, x, &w);
		if (n < 0)
		{
			HAK_STATIC_ASSERT(HAK_TYPE_MAX(hak_ooi_t) + HAK_TYPE_MIN(hak_ooi_t) == -1); /* assume 2's complement */
			if (w > (hak_oow_t)HAK_TYPE_MAX(hak_ooi_t) + 1) return 0; /* too small */
			*i = (w <= (hak_oow_t)HAK_TYPE_MAX(hak_ooi_t))? -(hak_ooi_t)w: HAK_TYPE_MIN(hak_ooi_t); /* negate back */
		}
		else if (n > 0)
		{
			if (w > HAK_TYPE_MAX(hak_ooi_t)) return 0; /* too big */
			*i = w;
		}

		return n;
	}

	return 0;  /* not integer */
}

int hak_inttoooi (hak_t* hak, hak_oop_t x, hak_ooi_t* i)
{
	if (HAK_OOP_IS_SMOOI(x))
	{
		*i = HAK_OOP_TO_SMOOI(x);
		return (*i < 0)? -1: 1;
	}

	if (hak_isbigint(hak, x))
	{
		hak_oow_t w;
		int n;

		n = bigint_to_oow_noseterr(hak, x, &w);
		if (n < 0)
		{
			HAK_STATIC_ASSERT(HAK_TYPE_MAX(hak_ooi_t) + HAK_TYPE_MIN(hak_ooi_t) == -1); /* assume 2's complement */
			if (w > (hak_oow_t)HAK_TYPE_MAX(hak_ooi_t) + 1)
			{
				hak_seterrnum(hak, HAK_ERANGE);
				return 0; /* too small */
			}
			*i = (w <= (hak_oow_t)HAK_TYPE_MAX(hak_ooi_t))? -(hak_ooi_t)w: HAK_TYPE_MIN(hak_ooi_t); /* negate back */
		}
		else if (n > 0)
		{
			if (w > HAK_TYPE_MAX(hak_ooi_t))
			{
				hak_seterrnum(hak, HAK_ERANGE);
				return 0; /* too big */
			}
			*i = w;
		}
		else
		{
			hak_seterrnum(hak, HAK_ERANGE);
		}

		return n;
	}

	hak_seterrbfmt(hak, HAK_EINVAL, "not an integer - %O", x);
	return 0;  /* not integer */
}

#if (HAK_SIZEOF_UINTMAX_T == HAK_SIZEOF_OOW_T)

	/* do nothing. required macros are defined in hak.h */

#elif (HAK_SIZEOF_UINTMAX_T == HAK_SIZEOF_OOW_T * 2) || (HAK_SIZEOF_UINTMAX_T == HAK_SIZEOF_OOW_T * 4)
static HAK_INLINE int bigint_to_uintmax_noseterr (hak_t* hak, hak_oop_t num, hak_uintmax_t* w)
{
	HAK_ASSERT(hak, HAK_OOP_IS_POINTER(num));
	HAK_ASSERT(hak, HAK_IS_PBIGINT(hak, num) || HAK_IS_NBIGINT(hak, num));

#if (HAK_LIW_BITS == HAK_OOW_BITS)
	HAK_ASSERT(hak, HAK_OBJ_GET_SIZE(num) >= 1);

	switch (HAK_OBJ_GET_SIZE(num))
	{
		case 1:
			*w = (hak_uintmax_t)HAK_OBJ_GET_WORD_VAL(num, 0);
			goto done;

		case 2:
			*w = ((hak_uintmax_t)HAK_OBJ_GET_WORD_VAL(num, 0) << HAK_LIW_BITS) |
			     ((hak_uintmax_t)HAK_OBJ_GET_WORD_VAL(num, 1));
			goto done;

	#if (HAK_SIZEOF_UINTMAX_T >= HAK_SIZEOF_OOW_T * 4)
		case 3:
			*w = ((hak_uintmax_t)HAK_OBJ_GET_WORD_VAL(num, 0) << (HAK_LIW_BITS * 2)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_WORD_VAL(num, 1) << (HAK_LIW_BITS * 1)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_WORD_VAL(num, 2))
			goto done;

		case 4:
			*w = ((hak_uintmax_t)HAK_OBJ_GET_WORD_VAL(num, 0) << (HAK_LIW_BITS * 3)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_WORD_VAL(num, 1) << (HAK_LIW_BITS * 2)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_WORD_VAL(num, 2) << (HAK_LIW_BITS * 1)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_WORD_VAL(num, 3))
			goto done;
	#endif

		default:
			return 0; /* not convertable */
	}

#elif (HAK_LIW_BITS == HAK_OOHW_BITS)
	HAK_ASSERT(hak, HAK_OBJ_GET_SIZE(num) >= 2);
	switch (HAK_OBJ_GET_SIZE(num))
	{
		case 2:
			*w = ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 0) << HAK_LIW_BITS) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 1));
			goto done;

		case 4:
			*w = ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 0) << (HAK_LIW_BITS * 3)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 1) << (HAK_LIW_BITS * 2)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 2) << (HAK_LIW_BITS * 1)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 3));
			goto done;

	#if (HAK_SIZEOF_UINTMAX_T >= HAK_SIZEOF_OOW_T * 4)
		case 6:
			*w = ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 0) << (HAK_LIW_BITS * 5)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 1) << (HAK_LIW_BITS * 4)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 2) << (HAK_LIW_BITS * 3)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 3) << (HAK_LIW_BITS * 2)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 4) << (HAK_LIW_BITS * 1)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 5));
			goto done;

		case 8:
			*w = ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 0) << (HAK_LIW_BITS * 7)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 1) << (HAK_LIW_BITS * 6)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 2) << (HAK_LIW_BITS * 5)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 3) << (HAK_LIW_BITS * 4)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 4) << (HAK_LIW_BITS * 3)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 5) << (HAK_LIW_BITS * 2)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 6) << (HAK_LIW_BITS * 1)) |
			     ((hak_uintmax_t)HAK_OBJ_GET_HALFWORD_VAL(num, 7));
			goto done;
	#endif

		default:
			return 0; /* not convertable */
	}
#else
#	error UNSUPPORTED LIW BIT SIZE
#endif

done:
	return (HAK_IS_NBIGINT(hak, num))? -1: 1;
}

int hak_inttouintmax_noseterr (hak_t* hak, hak_oop_t x, hak_uintmax_t* w)
{
	if (HAK_OOP_IS_SMOOI(x))
	{
		hak_ooi_t v;

		v = HAK_OOP_TO_SMOOI(x);
		if (v < 0)
		{
			*w = -v;
			return -1; /* negative number negated - kind of an error */
		}
		else
		{
			*w = v;
			return 1; /* zero or positive number */
		}
	}

	if (hak_isbigint(hak, x)) return bigint_to_uintmax_noseterr(hak, x, w);

	return 0; /* not convertable - too big, too small, or not an integer */
}

int hak_inttouintmax (hak_t* hak, hak_oop_t x, hak_uintmax_t* w)
{
	if (HAK_OOP_IS_SMOOI(x))
	{
		hak_ooi_t v;

		v = HAK_OOP_TO_SMOOI(x);
		if (v < 0)
		{
			*w = -v;
			return -1; /* negative number negated - kind of an error */
		}
		else
		{
			*w = v;
			return 1; /* zero or positive number */
		}
	}

	if (hak_isbigint(hak, x))
	{
		int n;
		n = bigint_to_uintmax_noseterr(hak, x, w);
		if (n <= 0) hak_seterrnum(hak, HAK_ERANGE);
		return n;
	}

	hak_seterrbfmt(hak, HAK_EINVAL, "not an integer - %O", x);
	return 0; /* not convertable - too big, too small, or not an integer */
}

int hak_inttointmax_noseterr (hak_t* hak, hak_oop_t x, hak_intmax_t* i)
{
	if (HAK_OOP_IS_SMOOI(x))
	{
		*i = HAK_OOP_TO_SMOOI(x);
		return (*i < 0)? -1: 1;
	}

	if (hak_isbigint(hak, x))
	{
		int n;
		hak_uintmax_t w;

		n = bigint_to_uintmax_noseterr(hak, x, &w);
		if (n < 0)
		{
			/* negative number negated to a positve number */
			HAK_STATIC_ASSERT(HAK_TYPE_MAX(hak_intmax_t) + HAK_TYPE_MIN(hak_intmax_t) == -1); /* assume 2's complement */
			if (w > (hak_uintmax_t)HAK_TYPE_MAX(hak_intmax_t) + 1) return 0; /* not convertable - too small */
			*i = (w <= (hak_uintmax_t)HAK_TYPE_MAX(hak_intmax_t))? -(hak_intmax_t)w: HAK_TYPE_MIN(hak_intmax_t); /* negate back */
		}
		else if (n > 0)
		{
			if (w > HAK_TYPE_MAX(hak_intmax_t)) return 0; /* not convertable - too big */
			*i = w;
		}

		return n;
	}

	return 0; /* not convertable - not an integer */
}

int hak_inttointmax (hak_t* hak, hak_oop_t x, hak_intmax_t* i)
{
	if (HAK_OOP_IS_SMOOI(x))
	{
		*i = HAK_OOP_TO_SMOOI(x);
		return (*i < 0)? -1: 1;
	}

	if (hak_isbigint(hak, x))
	{
		int n;
		hak_uintmax_t w;

		n = bigint_to_uintmax_noseterr(hak, x, &w);
		if (n < 0)
		{
			/* negative number negated to a positve number */
			HAK_STATIC_ASSERT(HAK_TYPE_MAX(hak_intmax_t) + HAK_TYPE_MIN(hak_intmax_t) == -1); /* assume 2's complement */
			if (w > (hak_uintmax_t)HAK_TYPE_MAX(hak_intmax_t) + 1)
			{
				hak_seterrnum(hak, HAK_ERANGE);
				return 0; /* not convertable. too small */
			}
			*i = (w <= (hak_uintmax_t)HAK_TYPE_MAX(hak_intmax_t))? -(hak_intmax_t)w: HAK_TYPE_MIN(hak_intmax_t); /* negate back */
		}
		else if (n > 0)
		{
			if (w > HAK_TYPE_MAX(hak_intmax_t))
			{
				hak_seterrnum(hak, HAK_ERANGE);
				return 0; /* not convertable. too big */
			}
			*i = w;
		}
		else
		{
			hak_seterrnum(hak, HAK_ERANGE);
		}
		return n;
	}

	hak_seterrbfmt(hak, HAK_EINVAL, "not an integer - %O", x);
	return 0; /* not convertable - too big, too small, or not an integer */
}

#else
#	error UNSUPPORTED UINTMAX SIZE
#endif

static HAK_INLINE hak_oop_t make_bigint_with_oow (hak_t* hak, hak_oow_t w)
{
#if (HAK_LIW_BITS == HAK_OOW_BITS)
	HAK_ASSERT(hak, HAK_SIZEOF(hak_oow_t) == HAK_SIZEOF(hak_liw_t));
	return make_pbigint(hak, &w, 1);
#elif (HAK_LIW_BITS == HAK_OOHW_BITS)
	hak_liw_t hw[2];
	hw[0] = w /*& HAK_LBMASK(hak_oow_t,HAK_LIW_BITS)*/;
	hw[1] = w >> HAK_LIW_BITS;
	return make_pbigint(hak, hw, (hw[1] > 0? 2: 1));
#else
#	error UNSUPPORTED LIW BIT SIZE
#endif
}

static HAK_INLINE hak_oop_t make_bigint_with_ooi (hak_t* hak, hak_ooi_t i)
{
#if (HAK_LIW_BITS == HAK_OOW_BITS)
	hak_oow_t w;

	HAK_STATIC_ASSERT(hak, HAK_SIZEOF(hak_oow_t) == HAK_SIZEOF(hak_liw_t));
	if (i >= 0)
	{
		w = i;
		return make_pbigint(hak, &w, 1);
	}
	else
	{
		w = (i == HAK_TYPE_MIN(hak_ooi_t))? ((hak_oow_t)HAK_TYPE_MAX(hak_ooi_t) + 1): -i;
		return make_nbigint(hak, &w, 1);
	}
#elif (HAK_LIW_BITS == HAK_OOHW_BITS)
	hak_liw_t hw[2];
	hak_oow_t w;

	HAK_STATIC_ASSERT(HAK_SIZEOF(hak_oohw_t) == HAK_SIZEOF(hak_liw_t));
	if (i >= 0)
	{
		w = i;
		hw[0] = w /*& HAK_LBMASK(hak_oow_t,HAK_LIW_BITS)*/;
		hw[1] = w >> HAK_LIW_BITS;
		return make_pbigint(hak, hw, (hw[1] > 0? 2: 1));
	}
	else
	{
		w = -i;
		w = (i == HAK_TYPE_MIN(hak_ooi_t))? ((hak_oow_t)HAK_TYPE_MAX(hak_ooi_t) + 1): -i;
		hw[0] = w /*& HAK_LBMASK(hak_oow_t,HAK_LIW_BITS)*/;
		hw[1] = w >> HAK_LIW_BITS;
		return make_nbigint(hak, hw, (hw[1] > 0? 2: 1));
	}
#else
#	error UNSUPPORTED LIW BIT SIZE
#endif
}

static HAK_INLINE hak_oop_t make_bloated_bigint_with_ooi (hak_t* hak, hak_ooi_t i, hak_oow_t extra)
{
#if (HAK_LIW_BITS == HAK_OOW_BITS)
	hak_oow_t w;
	hak_oop_t z;

	HAK_ASSERT(hak, extra <= HAK_OBJ_SIZE_MAX - 1);
	HAK_STATIC_ASSERT(hak, HAK_SIZEOF(hak_oow_t) == HAK_SIZEOF(hak_liw_t));
	if (i >= 0)
	{
		w = i;
		z = make_pbigint(hak, HAK_NULL, 1 + extra);
	}
	else
	{
		w = (i == HAK_TYPE_MIN(hak_ooi_t))? ((hak_oow_t)HAK_TYPE_MAX(hak_ooi_t) + 1): -i;
		z = make_nbigint(hak, HAK_NULL, 1 + extra);
	}

	if (HAK_UNLIKELY(!z)) return HAK_NULL;
	HAK_OBJ_SET_LIWORD_VAL (z, 0, w);
	return z;

#elif (HAK_LIW_BITS == HAK_OOHW_BITS)
	hak_liw_t hw[2];
	hak_oow_t w;
	hak_oop_t z;

	HAK_ASSERT(hak, extra <= HAK_OBJ_SIZE_MAX - 2);
	if (i >= 0)
	{
		w = i;
		hw[0] = w /*& HAK_LBMASK(hak_oow_t,HAK_LIW_BITS)*/;
		hw[1] = w >> HAK_LIW_BITS;
		z = make_pbigint(hak, HAK_NULL, (hw[1] > 0? 2: 1) + extra);
	}
	else
	{
		w = (i == HAK_TYPE_MIN(hak_ooi_t))? ((hak_oow_t)HAK_TYPE_MAX(hak_ooi_t) + 1): -i;
		hw[0] = w /*& HAK_LBMASK(hak_oow_t,HAK_LIW_BITS)*/;
		hw[1] = w >> HAK_LIW_BITS;
		z = make_nbigint(hak, HAK_NULL, (hw[1] > 0? 2: 1) + extra);
	}

	if (HAK_UNLIKELY(!z)) return HAK_NULL;
	HAK_OBJ_SET_LIWORD_VAL (z, 0, hw[0]);
	if (hw[1] > 0) HAK_OBJ_SET_LIWORD_VAL (z, 1, hw[1]);
	return z;
#else
#	error UNSUPPORTED LIW BIT SIZE
#endif
}

static HAK_INLINE hak_oop_t make_bigint_with_intmax (hak_t* hak, hak_intmax_t v)
{
	hak_oow_t len;
	hak_liw_t buf[HAK_SIZEOF_INTMAX_T / HAK_SIZEOF_LIW_T];
	hak_uintmax_t ui;
	hak_oop_class_t _class;

	/* this is not a generic function. it can't handle v
	 * if it's HAK_TYPE_MIN(hak_intmax_t) */
	HAK_ASSERT(hak, v > HAK_TYPE_MIN(hak_intmax_t));

	if (v >= 0)
	{
		ui = v;
		_class = hak->c_large_positive_integer;
	}
	else
	{
		ui = (v == HAK_TYPE_MIN(hak_intmax_t))? ((hak_uintmax_t)HAK_TYPE_MAX(hak_intmax_t) + 1): -v;
		_class = hak->c_large_negative_integer;
	}

	len = 0;
	do
	{
		buf[len++] = (hak_liw_t)ui;
		ui = ui >> HAK_LIW_BITS;
	}
	while (ui > 0);

	return hak_instantiate(hak, _class, buf, len);
}

static HAK_INLINE hak_oop_t make_bigint_with_uintmax (hak_t* hak, hak_uintmax_t ui)
{
	hak_oow_t len;
	hak_liw_t buf[HAK_SIZEOF_INTMAX_T / HAK_SIZEOF_LIW_T];

	len = 0;
	do
	{
		buf[len++] = (hak_liw_t)ui;
		ui = ui >> HAK_LIW_BITS;
	}
	while (ui > 0);

	return make_pbigint(hak, buf, len);
}

hak_oop_t hak_oowtoint (hak_t* hak, hak_oow_t w)
{
	HAK_ASSERT(hak, HAK_TYPE_IS_UNSIGNED(hak_oow_t));
	/*if (HAK_IN_SMOOI_RANGE(w))*/
	if (w <= HAK_SMOOI_MAX)
	{
		return HAK_SMOOI_TO_OOP(w);
	}
	else
	{
		return make_bigint_with_oow(hak, w);
	}
}

hak_oop_t hak_ooitoint (hak_t* hak, hak_ooi_t i)
{
	if (HAK_IN_SMOOI_RANGE(i))
	{
		return HAK_SMOOI_TO_OOP(i);
	}
	else
	{
		return make_bigint_with_ooi(hak, i);
	}
}

#if (HAK_SIZEOF_UINTMAX_T == HAK_SIZEOF_OOW_T)
	/* do nothing. required macros are defined in hak.h */
#else
hak_oop_t hak_intmaxtoint (hak_t* hak, hak_intmax_t i)
{
	if (HAK_IN_SMOOI_RANGE(i))
	{
		return HAK_SMOOI_TO_OOP(i);
	}
	else
	{
		return make_bigint_with_intmax(hak, i);
	}
}

hak_oop_t hak_uintmaxtoint (hak_t* hak, hak_uintmax_t i)
{
	if (HAK_IN_SMOOI_RANGE(i))
	{
		return HAK_SMOOI_TO_OOP(i);
	}
	else
	{
		return make_bigint_with_uintmax(hak, i);
	}
}
#endif

static HAK_INLINE hak_oop_t expand_bigint (hak_t* hak, hak_oop_t oop, hak_oow_t inc)
{
	hak_oop_t z;
	hak_oow_t i;
	hak_oow_t count;

	HAK_ASSERT(hak, HAK_OOP_IS_POINTER(oop));
	count = HAK_OBJ_GET_SIZE(oop);

	if (inc > HAK_OBJ_SIZE_MAX - count)
	{
		hak_seterrbfmt(hak, HAK_EOOMEM, "unable to expand bigint %O by %zu liwords", oop, inc); /* TODO: is it a soft failure or a hard failure? is this error code proper? */
		return HAK_NULL;
	}

	hak_pushvolat(hak, &oop);
	z = hak_instantiate(hak, (hak_oop_class_t)HAK_OBJ_GET_CLASS(oop), HAK_NULL, count + inc);
	hak_popvolat(hak);
	if (HAK_UNLIKELY(!z))
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to clone bigint %O for expansion - %s", oop, orgmsg);
		return HAK_NULL;
	}

	for (i = 0; i < count; i++)
	{
		((hak_oop_liword_t)z)->slot[i] = ((hak_oop_liword_t)oop)->slot[i];
	}
	return z;
}

static HAK_INLINE hak_oop_t _clone_bigint (hak_t* hak, hak_oop_t oop, hak_oow_t count, hak_oop_class_t _class)
{
	hak_oop_t z;
	hak_oow_t i;

	HAK_ASSERT(hak, HAK_OOP_IS_POINTER(oop));
	if (count <= 0) count = HAK_OBJ_GET_SIZE(oop);

	hak_pushvolat(hak, &oop);
	z = hak_instantiate(hak, _class, HAK_NULL, count);
	hak_popvolat(hak);
	if (HAK_UNLIKELY(!z)) return HAK_NULL;

	for (i = 0; i < count; i++)
	{
		((hak_oop_liword_t)z)->slot[i] = ((hak_oop_liword_t)oop)->slot[i];
	}
	return z;
}

static HAK_INLINE hak_oop_t clone_bigint (hak_t* hak, hak_oop_t oop, hak_oow_t count)
{
	return _clone_bigint(hak, oop, count, (hak_oop_class_t)HAK_OBJ_GET_CLASS(oop));
}

static HAK_INLINE hak_oop_t clone_bigint_negated (hak_t* hak, hak_oop_t oop, hak_oow_t count)
{
	hak_oop_class_t _class;

	HAK_ASSERT(hak, HAK_IS_BIGINT(hak,oop));

	if (HAK_IS_PBIGINT(hak, oop))
	{
		_class = hak->c_large_negative_integer;
	}
	else
	{
		HAK_ASSERT(hak, HAK_IS_NBIGINT(hak, oop));
		_class = hak->c_large_positive_integer;
	}

	return _clone_bigint(hak, oop, count, _class);
}

static HAK_INLINE hak_oop_t clone_bigint_to_positive (hak_t* hak, hak_oop_t oop, hak_oow_t count)
{
	return _clone_bigint(hak, oop, count, hak->c_large_positive_integer);
}

static HAK_INLINE hak_oow_t count_effective (hak_liw_t* x, hak_oow_t xs)
{
#if 0
	while (xs > 1 && x[xs - 1] == 0) xs--;
	return xs;
#else
	while (xs > 1) { if (x[--xs]) return xs + 1; }
	return 1;
#endif
}

static HAK_INLINE hak_oow_t count_effective_digits (hak_oop_t oop)
{
	hak_oow_t i;

	for (i = HAK_OBJ_GET_SIZE(oop); i > 1; )
	{
		--i;
		if (((hak_oop_liword_t)oop)->slot[i]) return i + 1;
	}

	return 1;
}

static hak_oop_t normalize_bigint (hak_t* hak, hak_oop_t oop)
{
	hak_oow_t count;

	HAK_ASSERT(hak, HAK_OOP_IS_POINTER(oop));
	count = count_effective_digits(oop);

#if (HAK_LIW_BITS == HAK_OOW_BITS)
	if (count == 1) /* 1 word */
	{
		hak_oow_t w;

		w = ((hak_oop_liword_t)oop)->slot[0];
		if (HAK_IS_PBIGINT(hak, oop))
		{
			if (w <= HAK_SMOOI_MAX) return HAK_SMOOI_TO_OOP(w);
		}
		else
		{
			HAK_ASSERT(hak, -HAK_SMOOI_MAX  == HAK_SMOOI_MIN);
			HAK_ASSERT(hak, HAK_IS_NBIGINT(hak, oop));
			if (w <= HAK_SMOOI_MAX) return HAK_SMOOI_TO_OOP(-(hak_ooi_t)w);
		}
	}
#elif (HAK_LIW_BITS == HAK_OOHW_BITS)

	if (count == 1) /* 1 half-word */
	{
		if (HAK_IS_PBIGINT(hak, oop))
		{
			return HAK_SMOOI_TO_OOP(((hak_oop_liword_t)oop)->slot[0]);
		}
		else
		{
			HAK_ASSERT(hak, HAK_IS_NBIGINT(hak, oop));
			return HAK_SMOOI_TO_OOP(-(hak_ooi_t)((hak_oop_liword_t)oop)->slot[0]);
		}
	}
	else if (count == 2) /* 2 half-words */
	{
		hak_oow_t w;

		w = MAKE_WORD(((hak_oop_liword_t)oop)->slot[0], ((hak_oop_liword_t)oop)->slot[1]);
		if (HAK_IS_PBIGINT(hak, oop))
		{
			if (w <= HAK_SMOOI_MAX) return HAK_SMOOI_TO_OOP(w);
		}
		else
		{
			HAK_ASSERT(hak, -HAK_SMOOI_MAX  == HAK_SMOOI_MIN);
			HAK_ASSERT(hak, HAK_IS_NBIGINT(hak, oop));
			if (w <= HAK_SMOOI_MAX) return HAK_SMOOI_TO_OOP(-(hak_ooi_t)w);
		}
	}
#else
#	error UNSUPPORTED LIW BIT SIZE
#endif
	if (HAK_OBJ_GET_SIZE(oop) == count)
	{
		/* no compaction is needed. return it as it is */
		return oop;
	}

	return clone_bigint(hak, oop, count);
}

static HAK_INLINE int is_less_unsigned_array (const hak_liw_t* x, hak_oow_t xs, const hak_liw_t* y, hak_oow_t ys)
{
	hak_oow_t i;

	if (xs != ys) return xs < ys;
	for (i = xs; i > 0; )
	{
		--i;
		if (x[i] != y[i]) return x[i] < y[i];
	}

	return 0;
}

static HAK_INLINE int is_less_unsigned (hak_oop_t x, hak_oop_t y)
{
	return is_less_unsigned_array (
		((hak_oop_liword_t)x)->slot, HAK_OBJ_GET_SIZE(x),
		((hak_oop_liword_t)y)->slot, HAK_OBJ_GET_SIZE(y));
}

static HAK_INLINE int is_less (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	if (HAK_OBJ_GET_CLASS(x) != HAK_OBJ_GET_CLASS(y)) return HAK_IS_NBIGINT(hak, x);
	if (HAK_IS_PBIGINT(hak, x)) return is_less_unsigned(x, y);
	return is_less_unsigned (y, x);
}

static HAK_INLINE int is_greater_unsigned_array (const hak_liw_t* x, hak_oow_t xs, const hak_liw_t* y, hak_oow_t ys)
{
	hak_oow_t i;

	if (xs != ys) return xs > ys;
	for (i = xs; i > 0; )
	{
		--i;
		if (x[i] != y[i]) return x[i] > y[i];
	}

	return 0;
}

static HAK_INLINE int is_greater_unsigned (hak_oop_t x, hak_oop_t y)
{
	return is_greater_unsigned_array (
		((hak_oop_liword_t)x)->slot, HAK_OBJ_GET_SIZE(x),
		((hak_oop_liword_t)y)->slot, HAK_OBJ_GET_SIZE(y));
}

static HAK_INLINE int is_greater (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	if (HAK_OBJ_GET_CLASS(x) != HAK_OBJ_GET_CLASS(y)) return HAK_IS_NBIGINT(hak, y);
	if (HAK_IS_PBIGINT(hak, x)) return is_greater_unsigned (x, y);
	return is_greater_unsigned (y, x);
}

static HAK_INLINE int is_equal_unsigned_array (const hak_liw_t* x, hak_oow_t xs, const hak_liw_t* y, hak_oow_t ys)
{
	return xs == ys && HAK_MEMCMP(x, y, xs * HAK_SIZEOF(*x)) == 0;
}

static HAK_INLINE int is_equal_unsigned (hak_oop_t x, hak_oop_t y)
{
	return is_equal_unsigned_array(
		((hak_oop_liword_t)x)->slot, HAK_OBJ_GET_SIZE(x),
		((hak_oop_liword_t)y)->slot, HAK_OBJ_GET_SIZE(y));
}

static HAK_INLINE int is_equal (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	/* check if two large integers are equal to each other */
	/*return HAK_OBJ_GET_CLASS(x) == HAK_OBJ_GET_CLASS(y) && HAK_OBJ_GET_SIZE(x) == HAK_OBJ_GET_SIZE(y) &&
	       HAK_MEMCMP(((hak_oop_liword_t)x)->slot,  ((hak_oop_liword_t)y)->slot, HAK_OBJ_GET_SIZE(x) * HAK_SIZEOF(hak_liw_t)) == 0;*/
	return HAK_OBJ_GET_CLASS(x) == HAK_OBJ_GET_CLASS(y) && HAK_OBJ_GET_SIZE(x) == HAK_OBJ_GET_SIZE(y) && is_equal_unsigned(x, y);
}

static void complement2_unsigned_array (hak_t* hak, const hak_liw_t* x, hak_oow_t xs, hak_liw_t* z)
{
	hak_oow_t i;
	hak_lidw_t w;
	hak_lidw_t carry;

	/* get 2's complement (~x + 1) */

	carry = 1;
	for (i = 0; i < xs; i++)
	{
		w = (hak_lidw_t)(~x[i]) + carry;
		/*w = (hak_lidw_t)(x[i] ^ (~(hak_liw_t)0)) + carry;*/
		carry = w >> HAK_LIW_BITS;
		z[i] = w;
	}

	/* if the array pointed to by x contains all zeros, carry will be
	 * 1 here and it actually requires 1 more slot. Let't take this 8-bit
	 * zero for instance:
	 *    2r00000000  -> 2r11111111 + 1 => 2r0000000100000000
	 *
	 * this function is not designed to handle such a case.
	 * in fact, 0 is a small integer and it must not stand a change
	 * to be given to this function */
	HAK_ASSERT(hak, carry == 0);
}

static HAK_INLINE hak_oow_t add_unsigned_array (const hak_liw_t* x, hak_oow_t xs, const hak_liw_t* y, hak_oow_t ys, hak_liw_t* z)
{
#if 1
	register hak_oow_t i;
	hak_lidw_t w;

	if (xs < ys)
	{
		/* swap x and y */
		i = xs;
		xs = ys;
		ys = i;

		i = (hak_oow_t)x;
		x = y;
		y = (hak_liw_t*)i;
	}

	w = 0;
	i = 0;
	while (i < ys)
	{
		w += (hak_lidw_t)x[i] + (hak_lidw_t)y[i];
		z[i++] = w & HAK_LBMASK(hak_lidw_t, HAK_LIW_BITS);
		w >>= HAK_LIW_BITS;
	}

	while (w && i < xs)
	{
		w += x[i];
		z[i++] = w & HAK_LBMASK(hak_lidw_t, HAK_LIW_BITS);
		w >>= HAK_LIW_BITS;
	}

	while (i < xs)
	{
		z[i] = x[i];
		i++;
	}
	if (w) z[i++] = w & HAK_LBMASK(hak_lidw_t, HAK_LIW_BITS);
	return i;

#else
	register hak_oow_t i;
	hak_lidw_t w;
	hak_liw_t carry = 0;

	if (xs < ys)
	{
		/* swap x and y */
		i = xs;
		xs = ys;
		ys = i;

		i = (hak_oow_t)x;
		x = y;
		y = (hak_liw_t*)i;
	}


	for (i = 0; i < ys; i++)
	{
		w = (hak_lidw_t)x[i] + (hak_lidw_t)y[i] + carry;
		carry = w >> HAK_LIW_BITS;
		z[i] = w /*& HAK_LBMASK(hak_lidw_t, HAK_LIW_BITS) */;
	}

	if (x == z)
	{
		for (; carry && i < xs; i++)
		{
			w = (hak_lidw_t)x[i] + carry;
			carry = w >> HAK_LIW_BITS;
			z[i] = w /*& HAK_LBMASK(hak_lidw_t, HAK_LIW_BITS) */;
		}
		i = xs;
	}
	else
	{
		for (; i < xs; i++)
		{
			w = (hak_lidw_t)x[i] + carry;
			carry = w >> HAK_LIW_BITS;
			z[i] = w /*& HAK_LBMASK(hak_lidw_t, HAK_LIW_BITS)*/;
		}
	}

	if (carry) z[i++] = carry;
	return i; /* the number of effective digits in the result */
#endif
}

static HAK_INLINE hak_oow_t subtract_unsigned_array (hak_t* hak, const hak_liw_t* x, hak_oow_t xs, const hak_liw_t* y, hak_oow_t ys, hak_liw_t* z)
{
#if 1
	hak_oow_t i;
	hak_lidi_t w = 0;

	if (x == y)
	{
		HAK_ASSERT(hak, xs == ys);
		z[0] = 0;
		return 1;
	}

	HAK_ASSERT(hak, !is_less_unsigned_array(x, xs, y, ys));

	for (i = 0; i < ys; i++)
	{
		w += (hak_lidi_t)x[i] - (hak_lidi_t)y[i];
		z[i] = w & HAK_LBMASK(hak_lidw_t, HAK_LIW_BITS);
		w >>= HAK_LIW_BITS;
	}

	while (w && i < xs)
	{
		w += x[i];
		z[i++] = w & HAK_LBMASK(hak_lidw_t, HAK_LIW_BITS);
		w >>= HAK_LIW_BITS;
	}

	while (i < xs)
	{
		z[i] = x[i];
		i++;
	}

	while (i > 1 && z[i - 1] == 0) i--;
	return i;

#else
	hak_oow_t i;
	hak_lidw_t w;
	hak_lidw_t borrow = 0;
	hak_lidw_t borrowed_word;

	if (x == y)
	{
		HAK_ASSERT(hak, xs == ys);
		z[0] = 0;
		return 1;
	}

	HAK_ASSERT(hak, !is_less_unsigned_array(x, xs, y, ys));

	borrowed_word = (hak_lidw_t)1 << HAK_LIW_BITS;
	for (i = 0; i < ys; i++)
	{
		w = (hak_lidw_t)y[i] + borrow;
		if ((hak_lidw_t)x[i] >= w)
		{
			z[i] = x[i] - w;
			borrow = 0;
		}
		else
		{
			z[i] = (borrowed_word + (hak_lidw_t)x[i]) - w;
			borrow = 1;
		}
	}

	for (; i < xs; i++)
	{
		if (x[i] >= borrow)
		{
			z[i] = x[i] - (hak_liw_t)borrow;
			borrow = 0;
		}
		else
		{
			z[i] = (borrowed_word + (hak_lidw_t)x[i]) - borrow;
			borrow = 1;
		}
	}

	HAK_ASSERT(hak, borrow == 0);

	while (i > 1 && z[i - 1] == 0) i--;
	return i; /* the number of effective digits in the result */
#endif
}

static HAK_INLINE void multiply_unsigned_array (const hak_liw_t* x, hak_oow_t xs, const hak_liw_t* y, hak_oow_t ys, hak_liw_t* z)
{
	hak_lidw_t v;
	hak_oow_t pa;

	if (xs < ys)
	{
		hak_oow_t i;

		/* swap x and y */
		i = xs;
		xs = ys;
		ys = i;

		i = (hak_oow_t)x;
		x = y;
		y = (hak_liw_t*)i;
	}

	if (ys == 1)
	{
		hak_lidw_t dw;
		hak_liw_t carry = 0;
		hak_oow_t i;

		for (i = 0; i < xs; i++)
		{
			dw = ((hak_lidw_t)x[i] * y[0]) + carry;
			carry = (hak_liw_t)(dw >> HAK_LIW_BITS);
			z[i] = (hak_liw_t)dw;
		}

		z[i] = carry;
		return;
	}

	pa = xs;
	if (pa <= ((hak_oow_t)1 << (HAK_LIDW_BITS - (HAK_LIW_BITS * 2))))
	{
		/* Comba(column-array) multiplication */

		/* when the input length is too long, v may overflow. if it
		 * happens, comba's method doesn't work as carry propagation is
		 * affected badly. so we need to use this method only if
		 * the input is short enough. */

		hak_oow_t pa, ix, iy, iz, tx, ty;

		pa = xs + ys;
		v = 0;
		for (ix = 0; ix < pa; ix++)
		{
			ty = (ix < ys - 1)? ix: (ys - 1);
			tx = ix - ty;
			iy = (ty + 1 < xs - tx)? (ty + 1): (xs - tx);

			for (iz = 0; iz < iy; iz++)
			{
				v = v + (hak_lidw_t)x[tx + iz] * (hak_lidw_t)y[ty - iz];
			}

			z[ix] = (hak_liw_t)v;
			v = v >> HAK_LIW_BITS;
		}
	}
	else
	{
		hak_oow_t i, j;
		hak_liw_t carry;

		for (i = 0; i < xs; i++)
		{
			if (x[i] == 0)
			{
				z[i + ys] = 0;
			}
			else
			{
				carry = 0;

				for (j = 0; j < ys; j++)
				{
					v = (hak_lidw_t)x[i] * (hak_lidw_t)y[j] + (hak_lidw_t)carry + (hak_lidw_t)z[i + j];
					z[i + j] = (hak_liw_t)v;
					carry = (hak_liw_t)(v >> HAK_LIW_BITS);
				}

				z[i + j] = carry;
			}
		}
	}
}

/* KARATSUBA MULTIPLICATION
 *
 * c = |a| * |b|
 *
 * Let B represent the radix(2^DIGIT_BITS)
 * Let n represent half the number of digits
 *
 * a = a1 * B^n + a0
 * b = b1 * B^n + b0
 * a * b => a1b1 * B^2n + ((a1 + a0)(b1 + b0) - (a0b0 + a1b1)) * B^n + a0b0
 *
 * --------------------------------------------------------------------
 * For example, for 2 number 0xFAC2 and 0xABCD => A848635A
 *   DIGIT_BITS = 8 (1 byte, each digit is 1 byte long)
 *   B = 2^8 = 0x100
 *   n = 1 (half the digits of 2 digit numbers)
 *   B^n = 0x100 ^ 1 = 0x100
 *   B^2n = 0x100 ^ 2 = 0x10000
 *   0xFAC2 = 0xFA * 0x100 + 0xC2
 *   0xABCD = 0xAB * 0x100 + 0xCD
 *   a1 = 0xFA, a0 = 0xC2
 *   b1 = 0xAB, b0 = 0xCD
 *   a1b1 = 0xFA * 0xAB = 0xA6FE
 *   a0b0 = 0xC2 * 0xCD = 0x9B5A
 *   a1 + a0 = 0xFA + 0xC2 = 0x1BC
 *   b1 + b0 = 0xAB + 0xCD = 0x178
 * --------------------------------------------------------------------
 *   (A6FE * 10000) + (((1BC * 178) - (985A + A6FE)) * 100) + 9B5A =
 *   (A6FE << (8 * 2)) + (((1BC * 178) - (985A + A6FE)) << (8 * 1)) =
 *   A6FE0000 + 14CC800 + 9B5A = 9848635A
 * --------------------------------------------------------------------
 *
 * 0xABCD9876 * 0xEFEFABAB => 0xA105C97C9755A8D2
 * B = 2^8 = 0x100
 * n = 2
 * B^n = 0x100 ^ 2 = 0x10000
 * B^2n = 0x100 ^ 4 = 0x100000000
 * 0xABCD9876 = 0xABCD * 0x10000 + 0x9876
 * 0xEFEFABAB = 0xEFEF * 0x10000 + 0xABAB
 * a1 = 0xABCD, a0 = 0x9876
 * b1 - 0xEFEF, b0 = 0xABAB
 * a1b1 = 0xA104C763
 * a0b0 = 0x663CA8D2
 * a1 + a0 = 0x14443
 * b1 + b0 = 0x19B9A
 * --------------------------------------------------------------------
 * (A104C763 * 100000000) + (((14443 * 19B9A) - (663CA8D2 + A104C763)) * 10000) + 663CA8D2 =
 * (A104C763 << (8 * 4)) + (((14443 * 19B9A) - (663CA8D2 + A104C763)) << (8 * 2)) + 663CA8D2 = A105C97C9755A8D2
 * --------------------------------------------------------------------
 *
 *  Multiplying by B is t same as shifting by DIGIT_BITS.
 *  DIGIT_BITS in this implementation is HAK_LIW_BITS
 *  B => 2^HAK_LIW_BITS
 *  X * B^n => X << (HAK_LIW_BITS * n)
 *  X * B^2n => X << (HAK_LIW_BITS * n * 2)
 * --------------------------------------------------------------------
 */

#if defined(HAK_BUILD_DEBUG)
#define CANNOT_KARATSUBA(hak,xs,ys) \
	((xs) < (hak)->option.karatsuba_cutoff || (ys) < (hak)->option.karatsuba_cutoff || \
	((xs) > (ys) && (ys) <= (((xs) + 1) / 2)) || \
	((xs) < (ys) && (xs) <= (((ys) + 1) / 2)))
#else
#define CANNOT_KARATSUBA(hak,xs,ys) \
	((xs) < HAK_KARATSUBA_CUTOFF || (ys) < HAK_KARATSUBA_CUTOFF || \
	((xs) > (ys) && (ys) <= (((xs) + 1) / 2)) || \
	((xs) < (ys) && (xs) <= (((ys) + 1) / 2)))
#endif

static HAK_INLINE hak_oow_t multiply_unsigned_array_karatsuba (hak_t* hak, const hak_liw_t* x, hak_oow_t xs, const hak_liw_t* y, hak_oow_t ys, hak_liw_t* z)
{
#if 1
	hak_lidw_t nshifts;
	hak_lidw_t ndigits_xh, ndigits_xl;
	hak_lidw_t ndigits_yh, ndigits_yl;
	hak_liw_t* tmp[2] = { HAK_NULL, HAK_NULL};
	hak_liw_t* zsp;
	hak_oow_t tmplen[2];
	hak_oow_t xlen, zcapa;

	zcapa = xs + ys; /* the caller ensures this capacity for z at the minimum*/

	if (xs < ys)
	{
		hak_oow_t i;

		/* swap x and y */
		i = xs;
		xs = ys;
		ys = i;

		i = (hak_oow_t)x;
		x = y;
		y = (hak_liw_t*)i;
	}

	if (ys == 1)
	{
		hak_lidw_t dw;
		hak_liw_t carry = 0;
		hak_oow_t i;

		for (i = 0; i < xs; i++)
		{
			dw = ((hak_lidw_t)x[i] * y[0]) + carry;
			carry = (hak_liw_t)(dw >> HAK_LIW_BITS);
			z[i] = (hak_liw_t)dw;
		}

		z[i] = carry;
		return count_effective(z, xs + 1);
	}

	/* calculate value of nshifts, that is 2^(HAK_LIW_BITS*nshifts) */
	nshifts = (xs + 1) / 2;

	ndigits_xl = nshifts; /* ndigits of lower part of x */
	ndigits_xh = xs - nshifts; /* ndigits of upper part of x */
	ndigits_yl = nshifts; /* ndigits of lower part of y */
	ndigits_yh = ys - nshifts; /* ndigits of uppoer part of y */

	HAK_ASSERT(hak, ndigits_xl >= ndigits_xh);
	HAK_ASSERT(hak, ndigits_yl >= ndigits_yh);

	/* make a temporary buffer for (b0 + b1) and (a1 * b1) */
	tmplen[0] = ndigits_xh + ndigits_yh;
	tmplen[1] = ndigits_yl + ndigits_yh + 1;
	if (tmplen[1] < tmplen[0]) tmplen[1] = tmplen[0];
	tmp[1] = (hak_liw_t*)hak_callocmem(hak, HAK_SIZEOF(hak_liw_t) * tmplen[1]); /* TODO: should i use the object memory? if not, reuse the buffer and minimize memory allocation */
	if (HAK_UNLIKELY(!tmp[1])) goto oops;

	/* make a temporary for (a0 + a1) and (a0 * b0) */
	tmplen[0] = ndigits_xl + ndigits_yl + 1;
	tmp[0] = (hak_liw_t*)hak_callocmem(hak, HAK_SIZEOF(hak_liw_t) * tmplen[0]);
	if (HAK_UNLIKELY(!tmp[0])) goto oops;

	/* tmp[0] = a0 + a1 */
	tmplen[0] = add_unsigned_array(x, ndigits_xl, x + nshifts, ndigits_xh, tmp[0]);

	/* tmp[1] = b0 + b1 */
	tmplen[1] = add_unsigned_array(y, ndigits_yl, y + nshifts, ndigits_yh, tmp[1]);

	/*HAK_DEBUG6 (hak, "karatsuba t %p u %p ndigits_xl %d ndigits_xh %d ndigits_yl %d ndigits_yh %d\n", tmp[0], tmp[1], (int)ndigits_xl, (int)ndigits_xh, (int)ndigits_yl, (int)ndigits_yh);*/
	/*HAK_DEBUG5 (hak, "zcapa %d, tmplen[0] %d tmplen[1] %d nshifts %d total %d\n", (int)zcapa, (int)tmplen[0], (int)tmplen[1], (int)nshifts, (int)(tmplen[0] + tmplen[1] + nshifts));*/

	/* place (a0 + a1) * (b0 + b1) at the shifted position */
	zsp = z + nshifts;
	if (CANNOT_KARATSUBA(hak, tmplen[0], tmplen[1]))
	{
		multiply_unsigned_array (tmp[0], tmplen[0], tmp[1], tmplen[1], zsp);
		xlen = count_effective(zsp, tmplen[0] + tmplen[1]);
	}
	else
	{
		xlen = multiply_unsigned_array_karatsuba(hak, tmp[0], tmplen[0], tmp[1], tmplen[1], zsp);
		if (xlen == 0) goto oops;
	}

	/* tmp[0] = a0 * b0 */
	tmplen[0] = ndigits_xl + ndigits_yl;
	HAK_MEMSET(tmp[0], 0, sizeof(hak_liw_t) * tmplen[0]);
	if (CANNOT_KARATSUBA(hak, ndigits_xl, ndigits_yl))
	{
		multiply_unsigned_array (x, ndigits_xl, y, ndigits_yl, tmp[0]);
		tmplen[0] = count_effective(tmp[0], tmplen[0]);
	}
	else
	{
		tmplen[0] = multiply_unsigned_array_karatsuba(hak, x, ndigits_xl, y, ndigits_yl, tmp[0]);
		if (tmplen[0] <= 0) goto oops;
	}

	/* tmp[1] = a1 * b1 */
	tmplen[1] = ndigits_xh + ndigits_yh;
	HAK_MEMSET(tmp[1], 0, sizeof(hak_liw_t) * tmplen[1]);
	if (CANNOT_KARATSUBA(hak, ndigits_xh, ndigits_yh))
	{
		multiply_unsigned_array (x + nshifts, ndigits_xh, y + nshifts, ndigits_yh, tmp[1]);
		tmplen[1] = count_effective(tmp[1], tmplen[1]);
	}
	else
	{
		tmplen[1] = multiply_unsigned_array_karatsuba(hak, x + nshifts, ndigits_xh, y + nshifts, ndigits_yh, tmp[1]);
		if (tmplen[1] <= 0) goto oops;
	}

	/* (a0+a1)*(b0+b1) -(a0*b0) */
	xlen = subtract_unsigned_array(hak, zsp, xlen, tmp[0], tmplen[0], zsp);

	/* (a0+a1)*(b0+b1) - (a0*b0) - (a1*b1) */
	xlen = subtract_unsigned_array(hak, zsp, xlen, tmp[1], tmplen[1], zsp);
	/* a1b1 is in tmp[1]. add (a1b1 * B^2n) to the high part of 'z' */
	zsp = z + (nshifts * 2); /* emulate shifting for "* B^2n". */
	xlen = zcapa - (nshifts * 2);
	xlen = add_unsigned_array(zsp, xlen, tmp[1], tmplen[1], zsp);

	/* z = z + a0b0. a0b0 is in tmp[0] */
	xlen = add_unsigned_array(z, zcapa, tmp[0], tmplen[0], z);

	hak_freemem(hak, tmp[1]);
	hak_freemem(hak, tmp[0]);
	return count_effective(z, xlen);

oops:
	if (tmp[1]) hak_freemem(hak, tmp[1]);
	if (tmp[0]) hak_freemem(hak, tmp[0]);
	return 0;

#else
	hak_lidw_t nshifts;
	hak_lidw_t ndigits_xh, ndigits_xl;
	hak_lidw_t ndigits_yh, ndigits_yl;
	hak_liw_t* tmp[3] = { HAK_NULL, HAK_NULL, HAK_NULL };
	hak_liw_t* zsp;
	hak_oow_t tmplen[3];
	hak_oow_t xlen, zcapa;

	zcapa = xs + ys; /* the caller ensures this capacity for z at the minimum*/

	if (xs < ys)
	{
		hak_oow_t i;

		/* swap x and y */
		i = xs;
		xs = ys;
		ys = i;

		i = (hak_oow_t)x;
		x = y;
		y = (hak_liw_t*)i;
	}

	if (ys == 1)
	{
		hak_lidw_t dw;
		hak_liw_t carry = 0;
		hak_oow_t i;

		for (i = 0; i < xs; i++)
		{
			dw = ((hak_lidw_t)x[i] * y[0]) + carry;
			carry = (hak_liw_t)(dw >> HAK_LIW_BITS);
			z[i] = (hak_liw_t)dw;
		}

		z[i] = carry;
		return;
	}

	/* calculate value of nshifts, that is 2^(HAK_LIW_BITS*nshifts) */
	nshifts = (xs + 1) / 2;

	ndigits_xl = nshifts; /* ndigits of lower part of x */
	ndigits_xh = xs - nshifts; /* ndigits of upper part of x */
	ndigits_yl = nshifts; /* ndigits of lower part of y */
	ndigits_yh = ys - nshifts; /* ndigits of uppoer part of y */

	HAK_ASSERT(hak, ndigits_xl >= ndigits_xh);
	HAK_ASSERT(hak, ndigits_yl >= ndigits_yh);

	/* make a temporary buffer for (b0 + b1) and (a1 * b1) */
	tmplen[0] = ndigits_yl + ndigits_yh + 1;
	tmplen[1] = ndigits_xh + ndigits_yh;
	if (tmplen[1] < tmplen[0]) tmplen[1] = tmplen[0];
	tmp[1] = (hak_liw_t*)hak_callocmem(hak, HAK_SIZEOF(hak_liw_t) * tmplen[1]);
	if (!tmp[1]) goto oops;

	/* make a temporary for (a0 + a1) and (a0 * b0) */
	tmplen[0] = ndigits_xl + ndigits_yl;
	tmp[0] = (hak_liw_t*)hak_callocmem(hak, HAK_SIZEOF(hak_liw_t) * tmplen[0]);
	if (!tmp[0]) goto oops;

	/* tmp[0] = a0 + a1 */
	tmplen[0] = add_unsigned_array(x, ndigits_xl, x + nshifts, ndigits_xh, tmp[0]);

	/* tmp[1] = b0 + b1 */
	tmplen[1] = add_unsigned_array(y, ndigits_yl, y + nshifts, ndigits_yh, tmp[1]);

	/* tmp[2] = (a0 + a1) * (b0 + b1) */
	tmplen[2] = tmplen[0] + tmplen[1];
	tmp[2] = (hak_liw_t*)hak_callocmem(hak, HAK_SIZEOF(hak_liw_t) * tmplen[2]);
	if (!tmp[2]) goto oops;
	if (CANNOT_KARATSUBA(hak, tmplen[0], tmplen[1]))
	{
		multiply_unsigned_array (tmp[0], tmplen[0], tmp[1], tmplen[1], tmp[2]);
		xlen = count_effective(tmp[2], tmplen[2]);
	}
	else
	{
		xlen = multiply_unsigned_array_karatsuba(hak, tmp[0], tmplen[0], tmp[1], tmplen[1], tmp[2]);
		if (xlen == 0) goto oops;
	}

	/* tmp[0] = a0 * b0 */
	tmplen[0] = ndigits_xl + ndigits_yl;
	HAK_MEMSET(tmp[0], 0, sizeof(hak_liw_t) * tmplen[0]);
	if (CANNOT_KARATSUBA(hak, ndigits_xl, ndigits_yl))
	{
		multiply_unsigned_array (x, ndigits_xl, y, ndigits_yl, tmp[0]);
		tmplen[0] = count_effective(tmp[0], tmplen[0]);
	}
	else
	{
		tmplen[0] = multiply_unsigned_array_karatsuba(hak, x, ndigits_xl, y, ndigits_yl, tmp[0]);
		if (tmplen[0] <= 0) goto oops;
	}

	/* tmp[1] = a1 * b1 */
	tmplen[1] = ndigits_xh + ndigits_yh;
	HAK_MEMSET(tmp[1], 0, sizeof(hak_liw_t) * tmplen[1]);
	if (CANNOT_KARATSUBA(hak, ndigits_xh, ndigits_yh))
	{
		multiply_unsigned_array (x + nshifts, ndigits_xh, y + nshifts, ndigits_yh, tmp[1]);
		tmplen[1] = count_effective(tmp[1], tmplen[1]);
	}
	else
	{
		tmplen[1] = multiply_unsigned_array_karatsuba(hak, x + nshifts, ndigits_xh, y + nshifts, ndigits_yh, tmp[1]);
		if (tmplen[1] <= 0) goto oops;
	}

	/* w = w - tmp[0] */
	xlen = subtract_unsigned_array(hak, tmp[2], xlen, tmp[0], tmplen[0], tmp[2]);

	/* r = w - tmp[1] */
	zsp = z + nshifts; /* emulate shifting for "* B^n" */
	xlen = subtract_unsigned_array(hak, tmp[2], xlen, tmp[1], tmplen[1], zsp);

	/* a1b1 is in tmp[1]. add (a1b1 * B^2n) to the high part of 'z' */
	zsp = z + (nshifts * 2); /* emulate shifting for "* B^2n". */
	xlen = zcapa - (nshifts * 2);
	xlen = add_unsigned_array(zsp, xlen, tmp[1], tmplen[1], zsp);

	/* z = z + a0b0. a0b0 is in tmp[0] */
	xlen = add_unsigned_array(z, zcapa, tmp[0], tmplen[0], z);

	hak_freemem(hak, tmp[2]);
	hak_freemem(hak, tmp[1]);
	hak_freemem(hak, tmp[0]);

	return count_effective(z, xlen);

oops:
	if (tmp[2]) hak_freemem(hak, tmp[2]);
	if (tmp[1]) hak_freemem(hak, tmp[1]);
	if (tmp[0]) hak_freemem(hak, tmp[0]);
	return 0;
#endif
}

static HAK_INLINE void lshift_unsigned_array (hak_liw_t* x, hak_oow_t xs, hak_oow_t bits)
{
	/* this function doesn't grow/shrink the array. Shifting is performed
	 * over the given array */

	hak_oow_t word_shifts, bit_shifts, bit_shifts_right;
	hak_oow_t si, di;

	/* get how many words to shift */
	word_shifts = bits / HAK_LIW_BITS;
	if (word_shifts >= xs)
	{
		HAK_MEMSET(x, 0, xs * HAK_SIZEOF(hak_liw_t));
		return;
	}

	/* get how many remaining bits to shift */
	bit_shifts = bits % HAK_LIW_BITS;
	bit_shifts_right = HAK_LIW_BITS - bit_shifts;

	/* shift words and bits */
	di = xs - 1;
	si = di - word_shifts;
	x[di] = x[si] << bit_shifts;
	while (di > word_shifts)
	{
		x[di] = x[di] | (x[--si] >> bit_shifts_right);
		x[--di] = x[si] << bit_shifts;
	}

	/* fill the remaining part with zeros */
	if (word_shifts > 0)
		HAK_MEMSET(x, 0, word_shifts * HAK_SIZEOF(hak_liw_t));
}

static HAK_INLINE void rshift_unsigned_array (hak_liw_t* x, hak_oow_t xs, hak_oow_t bits)
{
	/* this function doesn't grow/shrink the array. Shifting is performed
	 * over the given array */

	hak_oow_t word_shifts, bit_shifts, bit_shifts_left;
	hak_oow_t si, di, bound;

	/* get how many words to shift */
	word_shifts = bits / HAK_LIW_BITS;
	if (word_shifts >= xs)
	{
		HAK_MEMSET(x, 0, xs * HAK_SIZEOF(hak_liw_t));
		return;
	}

	/* get how many remaining bits to shift */
	bit_shifts = bits % HAK_LIW_BITS;
	bit_shifts_left = HAK_LIW_BITS - bit_shifts;

/* TODO: verify this function */
	/* shift words and bits */
	di = 0;
	si = word_shifts;
	x[di] = x[si] >> bit_shifts;
	bound = xs - word_shifts - 1;
	while (di < bound)
	{
		x[di] = x[di] | (x[++si] << bit_shifts_left);
		x[++di] = x[si] >> bit_shifts;
	}

	/* fill the remaining part with zeros */
	if (word_shifts > 0)
		HAK_MEMSET(&x[xs - word_shifts], 0, word_shifts * HAK_SIZEOF(hak_liw_t));
}

static void divide_unsigned_array (hak_t* hak, const hak_liw_t* x, hak_oow_t xs, const hak_liw_t* y, hak_oow_t ys, hak_liw_t* q, hak_liw_t* r)
{
/* TODO: this function needs to be rewritten for performance improvement.
 *       the binary long division is extremely slow for a big number */

	/* Perform binary long division.
	 * http://en.wikipedia.org/wiki/Division_algorithm
	 * ---------------------------------------------------------------------
	 * Q := 0                 initialize quotient and remainder to zero
	 * R := 0
	 * for i = n-1...0 do     where n is number of bits in N
	 *   R := R << 1          left-shift R by 1 bit
	 *   R(0) := X(i)         set the least-significant bit of R equal to bit i of the numerator
	 *   if R >= Y then
	 *     R = R - Y
	 *     Q(i) := 1
	 *   end
	 * end
	 */

	hak_oow_t rs, rrs, i , j;

	HAK_ASSERT(hak, xs >= ys);

	/* the caller must ensure:
	 *   - q and r are all zeros. can skip memset() with zero.
	 *   - q is as large as xs in size.
	 *   - r is as large as ys + 1 in size  */
	/*HAK_MEMSET(q, 0, HAK_SIZEOF(*q) * xs);
	HAK_MEMSET(r, 0, HAK_SIZEOF(*q) * ys);*/

	rrs = ys + 1;
	for (i = xs; i > 0; )
	{
		--i;
		for (j = HAK_LIW_BITS; j > 0;)
		{
			--j;

			/* the value of the remainder 'r' may get bigger than the
			 * divisor 'y' temporarily until subtraction is performed
			 * below. so ys + 1(kept in rrs) is needed for shifting here. */
			lshift_unsigned_array (r, rrs, 1);
			HAK_SETBITS (hak_liw_t, r[0], 0, 1, HAK_GETBITS(hak_liw_t, x[i], j, 1));

			rs = count_effective(r, rrs);
			if (!is_less_unsigned_array(r, rs, y, ys))
			{
				subtract_unsigned_array(hak, r, rs, y, ys, r);
				HAK_SETBITS (hak_liw_t, q[i], j, 1, 1);
			}
		}
	}
}

static HAK_INLINE hak_liw_t calculate_remainder (hak_t* hak, hak_liw_t* qr, hak_liw_t* y, hak_liw_t quo, int qr_start, int stop)
{
	hak_lidw_t dw;
	hak_liw_t b, c, c2, qyk;
	hak_oow_t j, k;

	for (b = 0, c = 0, c2 = 0, j = qr_start, k = 0; k < stop; j++, k++)
	{
		dw = (hak_lidw_t)qr[j] - b;
		b = (hak_liw_t)((dw >> HAK_LIW_BITS) & 1); /* b = -(dw mod BASE) */
		qr[j] = (hak_liw_t)dw;

		dw = ((hak_lidw_t)y[k] * quo) + c;
		c = (hak_liw_t)(dw >> HAK_LIW_BITS);
		qyk = (hak_liw_t)dw;

		dw = (hak_lidw_t)qr[j] - qyk;
		c2 = (hak_liw_t)((dw >> HAK_LIW_BITS) & 1);
		qr[j] = (hak_liw_t)dw;

		dw = (hak_lidw_t)b + c2 + c;
		c = (hak_liw_t)(dw >> HAK_LIW_BITS);
		b = (hak_liw_t)dw;

		HAK_ASSERT(hak, c == 0);
	}
	return b;
}

static void divide_unsigned_array2 (hak_t* hak, const hak_liw_t* x, hak_oow_t xs, const hak_liw_t* y, hak_oow_t ys, hak_liw_t* q, hak_liw_t* r)
{
	hak_oow_t i;
	hak_liw_t d, y1, y2;

	/* the caller must ensure:
	 *  - q can hold 'xs + 1' words and r can hold 'ys' words.
	 *  - q and r are set to all zeros. */
	HAK_ASSERT(hak, xs >= ys);

	if (ys == 1)
	{
		/* the divisor has a single word only. perform simple division */
		hak_lidw_t dw;
		hak_liw_t carry = 0;
		for (i = xs; i > 0; )
		{
			--i;
			dw = ((hak_lidw_t)carry << HAK_LIW_BITS) + x[i];
			q[i] = (hak_liw_t)(dw / y[0]);
			carry = (hak_liw_t)(dw % y[0]);
		}
		r[0] = carry;
		return;
	}

	for (i = 0; i < xs; i++) q[i] = x[i]; /* copy x to q */
	q[xs] = 0; /* store zero in the last extra word */
	for (i = 0; i < ys; i++) r[i] = y[i]; /* copy y to r */

	y1 = r[ys - 1]; /* highest divisor word */

	/*d = (y1 == HAK_TYPE_MAX(hak_liw_t)? ((hak_liw_t)1): ((hak_liw_t)(((hak_lidw_t)1 << HAK_LIW_BITS) / (y1 + 1))));*/
	d = (hak_liw_t)(((hak_lidw_t)1 << HAK_LIW_BITS) / ((hak_lidw_t)y1 + 1));
	if (d > 1)
	{
		hak_lidw_t dw;
		hak_liw_t carry;

		/* shift the divisor such that its high-order bit is on.
		 * shift the dividend the same amount as the previous step */

		/* r = r * d */
		for (carry = 0, i = 0; i < ys; i++)
		{
			dw = ((hak_lidw_t)r[i] * d) + carry;
			carry = (hak_liw_t)(dw >> HAK_LIW_BITS);
			r[i] = (hak_liw_t)dw;
		}
		HAK_ASSERT(hak, carry == 0);

		/* q = q * d */
		for (carry = 0, i = 0; i < xs; i++)
		{
			dw = ((hak_lidw_t)q[i] * d) + carry;
			carry = (hak_liw_t)(dw >> HAK_LIW_BITS);
			q[i] = (hak_liw_t)dw;
		}
		q[xs] = carry;
	}

	y1 = r[ys - 1];
	y2 = r[ys - 2];

	for (i = xs; i >= ys; --i)
	{
		hak_lidw_t dw, quo, rem;
		hak_liw_t b, xhi, xlo;

		/* ---------------------------------------------------------- */
		/* estimate the quotient.
		 *  2-current-dividend-words / 2-most-significant-divisor-words */

		xhi = q[i];
		xlo = q[i - 1];

		/* adjust the quotient if over-estimated */
		dw = ((hak_lidw_t)xhi << HAK_LIW_BITS) + xlo;
		/* TODO: optimize it with ASM - no seperate / and % */
		quo = dw / y1;
		rem = dw % y1;

	adjust_quotient:
		if (quo > HAK_TYPE_MAX(hak_liw_t) || (quo * y2) > ((rem << HAK_LIW_BITS) + q[i - 2]))
		{
			--quo;
			rem += y1;
			if (rem <= HAK_TYPE_MAX(hak_liw_t)) goto adjust_quotient;
		}

		/* ---------------------------------------------------------- */
		b = calculate_remainder(hak, q, r, quo, i - ys, ys);

		b = (hak_liw_t)((((hak_lidw_t)xhi - b) >> HAK_LIW_BITS) & 1); /* is the sign bit set? */
		if (b)
		{
			/* yes. underflow */
			hak_lidw_t dw;
			hak_liw_t carry;
			hak_oow_t j, k;

			for (carry = 0, j = i - ys, k = 0; k < ys; j++, k++)
			{
				dw = (hak_lidw_t)q[j] + r[k] + carry;
				carry = (hak_liw_t)(dw >> HAK_LIW_BITS);
				q[j] = (hak_liw_t)dw;
			}

			HAK_ASSERT(hak, carry == 1);
			q[i] = quo - 1;
		}
		else
		{
			q[i] = quo;
		}
	}

	if (d > 1)
	{
		hak_lidw_t dw;
		hak_liw_t carry;
		for (carry = 0, i = ys; i > 0; )
		{
			--i;
			dw = ((hak_lidw_t)carry << HAK_LIW_BITS) + q[i];
			/* TODO: optimize it with ASM - no seperate / and % */
			q[i] = (hak_liw_t)(dw / d);
			carry = (hak_liw_t)(dw % d);
		}
	}

	/* split quotient and remainder held in q to q and r respectively
	 *   q      [<--- quotient     ---->|<-- remainder     -->]
	 *  index   |xs  xs-1  ...  ys+1  ys|ys-1  ys-2  ...  1  0|
	 */
	for (i = 0; i < ys; i++) { r[i] = q[i]; q[i] = 0;  }
	for (; i <= xs; i++) { q[i - ys] = q[i]; q[i] = 0; }

}

static void divide_unsigned_array3 (hak_t* hak, const hak_liw_t* x, hak_oow_t xs, const hak_liw_t* y, hak_oow_t ys, hak_liw_t* q, hak_liw_t* r)
{
	hak_oow_t s, i, j, g, k;
	hak_lidw_t dw, qhat, rhat;
	hak_lidi_t di, ci;
	hak_liw_t* qq, y1, y2;

	/* the caller must ensure:
	 *  - q can hold 'xs + 1' words and r can hold 'ys' words.
	 *  - q and r are set to all zeros. */
	HAK_ASSERT(hak, xs >= ys);

	if (ys == 1)
	{
		/* the divisor has a single word only. perform simple division */
		hak_lidw_t dw;
		hak_liw_t carry = 0;
		for (i = xs; i > 0; )
		{
			--i;
			dw = ((hak_lidw_t)carry << HAK_LIW_BITS) + x[i];
			q[i] = (hak_liw_t)(dw / y[0]);
			carry = (hak_liw_t)(dw % y[0]);
		}
		r[0] = carry;
		return;
	}

#define SHARED_QQ

#if defined(SHARED_QQ)
	/* as long as q is 2 words longer than x, this algorithm can store
	 * both quotient and remainder in q at the same time. */
	qq = q;
#else
	/* this part requires an extra buffer. proper error handling isn't easy
	 * since the return type of this function is void */
	if (hak->inttostr.t.capa <= xs)
	{
		hak_liw_t* t;
		hak_oow_t reqcapa;

		reqcapa = HAK_ALIGN_POW2(xs + 1, 32);
		t = (hak_liw_t*)hak_reallocmem(hak, hak->inttostr.t.ptr, reqcapa * HAK_SIZEOF(*t));
		/* TODO: TODO: TODO: ERROR HANDLING
		if (!t) return -1; */

		hak->inttostr.t.capa = xs + 1;
		hak->inttostr.t.ptr = t;
	}
	qq = hak->inttostr.t.ptr;
#endif

	y1 = y[ys - 1];
	/*s = HAK_LIW_BITS - ((y1 == 0)? -1: hak_get_pos_of_msb_set(y1)) - 1;*/
	HAK_ASSERT(hak, y1 > 0); /* the highest word can't be non-zero in the context where this function is called */
	s = HAK_LIW_BITS - hak_get_pos_of_msb_set(y1) - 1;
	for (i = ys; i > 1; )
	{
		--i;
		r[i] = (y[i] << s) | ((hak_lidw_t)y[i - 1] >> (HAK_LIW_BITS - s));
	}
	r[0] = y[0] << s;

	qq[xs] = (hak_lidw_t)x[xs - 1] >> (HAK_LIW_BITS - s);
	for (i = xs; i > 1; )
	{
		--i;
		qq[i] = (x[i] << s) | ((hak_lidw_t)x[i - 1] >> (HAK_LIW_BITS - s));
	}
	qq[0] = x[0] << s;

	y1 = r[ys - 1];
	y2 = r[ys - 2];

	for (j = xs; j >= ys; --j)
	{
		g = j - ys; /* position where remainder begins in qq */

		/* estimate */
		dw = ((hak_lidw_t)qq[j] << HAK_LIW_BITS) + qq[j - 1];
		qhat = dw / y1;
		rhat = dw - (qhat * y1);

	adjust_quotient:
		if (qhat > HAK_TYPE_MAX(hak_liw_t) || (qhat * y2) > ((rhat << HAK_LIW_BITS) + qq[j - 2]))
		{
			qhat = qhat - 1;
			rhat = rhat + y1;
			if (rhat <= HAK_TYPE_MAX(hak_liw_t)) goto adjust_quotient;
		}

		/* multiply and subtract */
		for (ci = 0, i = g, k = 0; k < ys; i++, k++)
		{
			dw = qhat * r[k];
			di = qq[i] - ci - (dw & HAK_TYPE_MAX(hak_liw_t));
			ci = (dw >> HAK_LIW_BITS) - (di >> HAK_LIW_BITS);
			qq[i] = (hak_liw_t)di;
		}
		HAK_ASSERT(hak, i == j);
		di = qq[i] - ci;
		qq[i] = di;

		/* test remainder */
		if (di < 0)
		{
			for (ci = 0, i = g, k = 0; k < ys; i++, k++)
			{
				di = (hak_lidw_t)qq[i] + r[k] + ci;
				ci = (hak_liw_t)(di >> HAK_LIW_BITS);
				qq[i] = (hak_liw_t)di;
			}

			HAK_ASSERT(hak, i == j);
			/*HAK_ASSERT(hak, ci == 1);*/
			qq[i] += ci;

		#if defined(SHARED_QQ)
			/* store the quotient word right after the remainder in q */
			q[i + 1] = qhat - 1;
		#else
			q[g] = qhat - 1;
		#endif
		}
		else
		{
		#if defined(SHARED_QQ)
			/* store the quotient word right after the remainder in q */
			q[i + 1] = qhat;
		#else
			q[g] = qhat;
		#endif
		}
	}

	for (i = 0; i < ys - 1; i++)
	{
		r[i] = (qq[i] >> s) | ((hak_lidw_t)qq[i + 1] << (HAK_LIW_BITS - s));
	}
	r[i] = qq[i] >> s;

#if defined(SHARED_QQ)
	for (i = 0; i <= ys; i++) { q[i] = 0;  }
	for (; i <= xs + 1; i++) { q[i - ys - 1] = q[i]; q[i] = 0; }
#endif

}

/* ======================================================================== */

static hak_oop_t add_unsigned_integers (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	hak_oow_t as, bs, zs;
	hak_oop_t z;

	as = HAK_OBJ_GET_SIZE(x);
	bs = HAK_OBJ_GET_SIZE(y);
	zs = (as >= bs? as: bs);

	if (zs >= HAK_OBJ_SIZE_MAX)
	{
		hak_seterrnum(hak, HAK_EOOMEM); /* TOOD: is it a soft failure or hard failure? */
		return HAK_NULL;
	}
	zs++;

	hak_pushvolat(hak, &x);
	hak_pushvolat(hak, &y);
	z = hak_instantiate(hak, (hak_oop_class_t)HAK_OBJ_GET_CLASS(x), HAK_NULL, zs);
	hak_popvolats(hak, 2);
	if (HAK_UNLIKELY(!z)) return HAK_NULL;

	add_unsigned_array (
		((hak_oop_liword_t)x)->slot, as,
		((hak_oop_liword_t)y)->slot, bs,
		((hak_oop_liword_t)z)->slot
	);

	return z;
}

static hak_oop_t subtract_unsigned_integers (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	hak_oop_t z;

	HAK_ASSERT(hak, !is_less_unsigned(x, y));

	hak_pushvolat(hak, &x);
	hak_pushvolat(hak, &y);
	z = make_pbigint(hak, HAK_NULL, HAK_OBJ_GET_SIZE(x));
	hak_popvolats(hak, 2);
	if (HAK_UNLIKELY(!z)) return HAK_NULL;

	subtract_unsigned_array(hak,
		((hak_oop_liword_t)x)->slot, HAK_OBJ_GET_SIZE(x),
		((hak_oop_liword_t)y)->slot, HAK_OBJ_GET_SIZE(y),
		((hak_oop_liword_t)z)->slot);
	return z;
}

static hak_oop_t multiply_unsigned_integers (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	hak_oop_t z;
	hak_oow_t xs, ys;

	xs = HAK_OBJ_GET_SIZE(x);
	ys = HAK_OBJ_GET_SIZE(y);

	if (ys > HAK_OBJ_SIZE_MAX - xs)
	{
		hak_seterrnum(hak, HAK_EOOMEM); /* TOOD: is it a soft failure or hard failure? */
		return HAK_NULL;
	}

	hak_pushvolat(hak, &x);
	hak_pushvolat(hak, &y);
	z = make_pbigint(hak, HAK_NULL, xs + ys);
	hak_popvolats(hak, 2);
	if (HAK_UNLIKELY(!z)) return HAK_NULL;

#if defined(HAK_ENABLE_KARATSUBA)
	if (CANNOT_KARATSUBA(hak, xs, ys))
	{
#endif
		multiply_unsigned_array (
			((hak_oop_liword_t)x)->slot, HAK_OBJ_GET_SIZE(x),
			((hak_oop_liword_t)y)->slot, HAK_OBJ_GET_SIZE(y),
			((hak_oop_liword_t)z)->slot);
#if defined(HAK_ENABLE_KARATSUBA)
	}
	else
	{
		if (multiply_unsigned_array_karatsuba (
			hak,
			((hak_oop_liword_t)x)->slot, HAK_OBJ_GET_SIZE(x),
			((hak_oop_liword_t)y)->slot, HAK_OBJ_GET_SIZE(y),
			((hak_oop_liword_t)z)->slot) == 0) return HAK_NULL;
	}
#endif
	return z;
}

static hak_oop_t divide_unsigned_integers (hak_t* hak, hak_oop_t x, hak_oop_t y, hak_oop_t* r)
{
	hak_oop_t qq, rr;

	if (is_less_unsigned(x, y))
	{
		rr = clone_bigint(hak, x, HAK_OBJ_GET_SIZE(x));
		if (HAK_UNLIKELY(!rr)) return HAK_NULL;

		hak_pushvolat(hak, &rr);
		qq = make_bigint_with_ooi(hak, 0); /* TODO: inefficient. no need to create a bigint object for zero. */
		hak_popvolat(hak);

		if (HAK_LIKELY(qq)) *r = rr;
		return qq;
	}
	else if (is_equal_unsigned(x, y))
	{
		rr = make_bigint_with_ooi(hak, 0); /* TODO: inefficient. no need to create a bigint object for zero. */
		if (HAK_UNLIKELY(!rr)) return HAK_NULL;

		hak_pushvolat(hak, &rr);
		qq = make_bigint_with_ooi(hak, 1); /* TODO: inefficient. no need to create a bigint object for zero. */
		hak_popvolat(hak);

		if (HAK_LIKELY(qq)) *r = rr;
		return qq;
	}

	/* the caller must ensure that x >= y */
	HAK_ASSERT(hak, !is_less_unsigned(x, y));
	hak_pushvolat(hak, &x);
	hak_pushvolat(hak, &y);

#define USE_DIVIDE_UNSIGNED_ARRAY2
/*#define USE_DIVIDE_UNSIGNED_ARRAY3*/

#if defined(USE_DIVIDE_UNSIGNED_ARRAY3)
	qq = make_pbigint(hak, HAK_NULL, HAK_OBJ_GET_SIZE(x) + 2);
#elif defined(USE_DIVIDE_UNSIGNED_ARRAY2)
	qq = make_pbigint(hak, HAK_NULL, HAK_OBJ_GET_SIZE(x) + 1);
#else
	qq = make_pbigint(hak, HAK_NULL, HAK_OBJ_GET_SIZE(x));
#endif
	if (HAK_UNLIKELY(!qq))
	{
		hak_popvolats(hak, 2);
		return HAK_NULL;
	}

	hak_pushvolat(hak, &qq);
#if defined(USE_DIVIDE_UNSIGNED_ARRAY3)
	rr = make_pbigint(hak, HAK_NULL, HAK_OBJ_GET_SIZE(y));
#elif defined(USE_DIVIDE_UNSIGNED_ARRAY2)
	rr = make_pbigint(hak, HAK_NULL, HAK_OBJ_GET_SIZE(y));
#else
	rr = make_pbigint(hak, HAK_NULL, HAK_OBJ_GET_SIZE(y) + 1);
#endif
	hak_popvolats(hak, 3);
	if (HAK_UNLIKELY(!rr)) return HAK_NULL;

#if defined(USE_DIVIDE_UNSIGNED_ARRAY3)
	divide_unsigned_array3 (hak,
#elif defined(USE_DIVIDE_UNSIGNED_ARRAY2)
	divide_unsigned_array2 (hak,
#else
	divide_unsigned_array(hak,
#endif
		((hak_oop_liword_t)x)->slot, HAK_OBJ_GET_SIZE(x),
		((hak_oop_liword_t)y)->slot, HAK_OBJ_GET_SIZE(y),
		((hak_oop_liword_t)qq)->slot, ((hak_oop_liword_t)rr)->slot
	);

	*r = rr;
	return qq;
}

/* ======================================================================== */

hak_oop_t hak_addints (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	hak_oop_t z;

	if (HAK_OOP_IS_SMOOI(x) && HAK_OOP_IS_SMOOI(y))
	{
		hak_ooi_t i;

		/* no integer overflow/underflow must occur as the possible integer
		 * range is narrowed by the tag bits used */
		HAK_ASSERT(hak, HAK_SMOOI_MAX + HAK_SMOOI_MAX < HAK_TYPE_MAX(hak_ooi_t));
		HAK_ASSERT(hak, HAK_SMOOI_MIN + HAK_SMOOI_MIN > HAK_TYPE_MIN(hak_ooi_t));

		i = HAK_OOP_TO_SMOOI(x) + HAK_OOP_TO_SMOOI(y);
		if (HAK_IN_SMOOI_RANGE(i)) return HAK_SMOOI_TO_OOP(i);

		return make_bigint_with_ooi(hak, i);
	}
	else
	{
		hak_ooi_t v;

		if (HAK_OOP_IS_SMOOI(x))
		{
			if (!hak_isbigint(hak,y)) goto oops_einval;

			v = HAK_OOP_TO_SMOOI(x);
			if (v == 0) return clone_bigint(hak, y, HAK_OBJ_GET_SIZE(y));

			hak_pushvolat(hak, &y);
			x = make_bigint_with_ooi(hak, v);
			hak_popvolat(hak);
			if (HAK_UNLIKELY(!x)) return HAK_NULL;
		}
		else if (HAK_OOP_IS_SMOOI(y))
		{
			if (!hak_isbigint(hak,x)) goto oops_einval;

			v = HAK_OOP_TO_SMOOI(y);
			if (v == 0) return clone_bigint(hak, x, HAK_OBJ_GET_SIZE(x));

			hak_pushvolat(hak, &x);
			y = make_bigint_with_ooi(hak, v);
			hak_popvolat(hak);
			if (HAK_UNLIKELY(!y)) return HAK_NULL;
		}
		else
		{
			if (!hak_isbigint(hak,x)) goto oops_einval;
			if (!hak_isbigint(hak,y)) goto oops_einval;
		}

		if (HAK_OBJ_GET_CLASS(x) != HAK_OBJ_GET_CLASS(y))
		{
			if (HAK_IS_NBIGINT(hak, x))
			{
				/* x is negative, y is positive */
				if (is_less_unsigned(x, y))
				{
					z = subtract_unsigned_integers(hak, y, x);
					if (HAK_UNLIKELY(!z)) return HAK_NULL;
				}
				else
				{
					z = subtract_unsigned_integers(hak, x, y);
					if (HAK_UNLIKELY(!z)) return HAK_NULL;
					HAK_OBJ_SET_CLASS (z, (hak_oop_t)hak->c_large_negative_integer);
				}
			}
			else
			{
				/* x is positive, y is negative */
				if (is_less_unsigned(x, y))
				{
					z = subtract_unsigned_integers(hak, y, x);
					if (HAK_UNLIKELY(!z)) return HAK_NULL;
					HAK_OBJ_SET_CLASS (z, (hak_oop_t)hak->c_large_negative_integer);
				}
				else
				{
					z = subtract_unsigned_integers(hak, x, y);
					if (HAK_UNLIKELY(!z)) return HAK_NULL;
				}
			}
		}
		else
		{
			int neg;
			/* both are positive or negative */
			neg = HAK_IS_NBIGINT(hak, x);
			z = add_unsigned_integers(hak, x, y);
			if (HAK_UNLIKELY(!z)) return HAK_NULL;
			if (neg) HAK_OBJ_SET_CLASS (z, (hak_oop_t)hak->c_large_negative_integer);
		}
	}

	return normalize_bigint(hak, z);

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O, %O", x, y);
	return HAK_NULL;
}

hak_oop_t hak_subints (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	hak_oop_t z;

	if (HAK_OOP_IS_SMOOI(x) && HAK_OOP_IS_SMOOI(y))
	{
		hak_ooi_t i;

		/* no integer overflow/underflow must occur as the possible integer
		 * range is narrowed by the tag bits used */
		HAK_ASSERT(hak, HAK_SMOOI_MAX - HAK_SMOOI_MIN < HAK_TYPE_MAX(hak_ooi_t));
		HAK_ASSERT(hak, HAK_SMOOI_MIN - HAK_SMOOI_MAX > HAK_TYPE_MIN(hak_ooi_t));

		i = HAK_OOP_TO_SMOOI(x) - HAK_OOP_TO_SMOOI(y);
		if (HAK_IN_SMOOI_RANGE(i)) return HAK_SMOOI_TO_OOP(i);

		return make_bigint_with_ooi(hak, i);
	}
	else
	{
		hak_ooi_t v;
		int neg;

		if (HAK_OOP_IS_SMOOI(x))
		{
			if (!hak_isbigint(hak,y)) goto oops_einval;

			v = HAK_OOP_TO_SMOOI(x);
			if (v == 0)
			{
				/* switch the sign to the opposite and return it */
				return clone_bigint_negated(hak, y, HAK_OBJ_GET_SIZE(y));
			}

			hak_pushvolat(hak, &y);
			x = make_bigint_with_ooi(hak, v);
			hak_popvolat(hak);
			if (HAK_UNLIKELY(!x)) return HAK_NULL;
		}
		else if (HAK_OOP_IS_SMOOI(y))
		{
			if (!hak_isbigint(hak,x)) goto oops_einval;

			v = HAK_OOP_TO_SMOOI(y);
			if (v == 0) return clone_bigint(hak, x, HAK_OBJ_GET_SIZE(x));

			hak_pushvolat(hak, &x);
			y = make_bigint_with_ooi(hak, v);
			hak_popvolat(hak);
			if (HAK_UNLIKELY(!y)) return HAK_NULL;
		}
		else
		{
			if (!hak_isbigint(hak,x)) goto oops_einval;
			if (!hak_isbigint(hak,y)) goto oops_einval;
		}

		if (HAK_OBJ_GET_CLASS(x) != HAK_OBJ_GET_CLASS(y))
		{
			neg = HAK_IS_NBIGINT(hak, x);
			z = add_unsigned_integers(hak, x, y);
			if (HAK_UNLIKELY(!z)) return HAK_NULL;
			if (neg) HAK_OBJ_SET_CLASS (z, (hak_oop_t)hak->c_large_negative_integer);
		}
		else
		{
			/* both are positive or negative */
			if (is_less_unsigned(x, y))
			{
				neg = HAK_IS_NBIGINT(hak, x);
				z = subtract_unsigned_integers(hak, y, x);
				if (HAK_UNLIKELY(!z)) return HAK_NULL;
				if (!neg) HAK_OBJ_SET_CLASS (z, (hak_oop_t)hak->c_large_negative_integer);
			}
			else
			{
				neg = HAK_IS_NBIGINT(hak, x);
				z = subtract_unsigned_integers(hak, x, y); /* take x's sign */
				if (HAK_UNLIKELY(!z)) return HAK_NULL;
				if (neg) HAK_OBJ_SET_CLASS (z, (hak_oop_t)hak->c_large_negative_integer);
			}
		}
	}

	return normalize_bigint(hak, z);

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O, %O", x, y);
	return HAK_NULL;
}

hak_oop_t hak_mulints (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	hak_oop_t z;

	if (HAK_OOP_IS_SMOOI(x) && HAK_OOP_IS_SMOOI(y))
	{
	#if (HAK_SIZEOF_INTMAX_T > HAK_SIZEOF_OOI_T)
		hak_intmax_t i;
		i = (hak_intmax_t)HAK_OOP_TO_SMOOI(x) * (hak_intmax_t)HAK_OOP_TO_SMOOI(y);
		if (HAK_IN_SMOOI_RANGE(i)) return HAK_SMOOI_TO_OOP((hak_ooi_t)i);
		return make_bigint_with_intmax(hak, i);
	#else
		hak_ooi_t i;
		hak_ooi_t xv, yv;

		xv = HAK_OOP_TO_SMOOI(x);
		yv = HAK_OOP_TO_SMOOI(y);
		if (shaki_mul_overflow(hak, xv, yv, &i))
		{
			/* overflowed - convert x and y normal objects and carry on */

			/* no need to call hak_pushvolat before creating x because
			 * xv and yv contains actual values needed */
			x = make_bigint_with_ooi(hak, xv);
			if (HAK_UNLIKELY(!x)) return HAK_NULL;

			hak_pushvolat(hak, &x); /* protect x made above */
			y = make_bigint_with_ooi(hak, yv);
			hak_popvolat(hak);
			if (HAK_UNLIKELY(!y)) return HAK_NULL;

			goto full_multiply;
		}
		else
		{
			if (HAK_IN_SMOOI_RANGE(i)) return HAK_SMOOI_TO_OOP(i);
			return make_bigint_with_ooi(hak, i);
		}
	#endif
	}
	else
	{
		hak_ooi_t v;
		int neg;

		if (HAK_OOP_IS_SMOOI(x))
		{
			if (!hak_isbigint(hak,y)) goto oops_einval;

			v = HAK_OOP_TO_SMOOI(x);
			switch (v)
			{
				case 0:
					return HAK_SMOOI_TO_OOP(0);
				case 1:
					return clone_bigint(hak, y, HAK_OBJ_GET_SIZE(y));
				case -1:
					return clone_bigint_negated(hak, y, HAK_OBJ_GET_SIZE(y));
			}

			hak_pushvolat(hak, &y);
			x = make_bigint_with_ooi(hak, v);
			hak_popvolat(hak);
			if (HAK_UNLIKELY(!x)) return HAK_NULL;
		}
		else if (HAK_OOP_IS_SMOOI(y))
		{
			if (!hak_isbigint(hak,x)) goto oops_einval;

			v = HAK_OOP_TO_SMOOI(y);
			switch (v)
			{
				case 0:
					return HAK_SMOOI_TO_OOP(0);
				case 1:
					return clone_bigint(hak, x, HAK_OBJ_GET_SIZE(x));
				case -1:
					return clone_bigint_negated(hak, x, HAK_OBJ_GET_SIZE(x));
			}

			hak_pushvolat(hak, &x);
			y = make_bigint_with_ooi(hak, v);
			hak_popvolat(hak);
			if (HAK_UNLIKELY(!y)) return HAK_NULL;
		}
		else
		{
			if (!hak_isbigint(hak,x)) goto oops_einval;
			if (!hak_isbigint(hak,y)) goto oops_einval;
		}

	full_multiply:
		neg = (HAK_OBJ_GET_CLASS(x) != HAK_OBJ_GET_CLASS(y));
		z = multiply_unsigned_integers(hak, x, y);
		if (HAK_UNLIKELY(!z)) return HAK_NULL;
		if (neg) HAK_OBJ_SET_CLASS (z, (hak_oop_t)hak->c_large_negative_integer);
	}

	return normalize_bigint(hak, z);

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O, %O", x, y);
	return HAK_NULL;
}

hak_oop_t hak_divints (hak_t* hak, hak_oop_t x, hak_oop_t y, int modulo, hak_oop_t* rem)
{
	hak_oop_t z, r;
	int x_neg_sign, y_neg_sign;

	if (HAK_OOP_IS_SMOOI(x) && HAK_OOP_IS_SMOOI(y))
	{
		hak_ooi_t xv, yv, q, ri;

		xv = HAK_OOP_TO_SMOOI(x);
		yv = HAK_OOP_TO_SMOOI(y);

		if (yv == 0)
		{
			hak_seterrnum(hak, HAK_EDIVBY0);
			return HAK_NULL;
		}

		if (xv == 0)
		{
			if (rem) *rem = HAK_SMOOI_TO_OOP(0);
			return HAK_SMOOI_TO_OOP(0);
		}

		/* In C89, integer division with a negative number  is
		 * implementation dependent. In C99, it truncates towards zero.
		 *
		 * http://python-history.blogspot.kr/2010/08/why-pythons-integer-division-floors.html
		 *   The integer division operation (//) and its sibling,
		 *   the modulo operation (%), go together and satisfy a nice
		 *   mathematical relationship (all variables are integers):
		 *      a/b = q with remainder r
		 *   such that
		 *      b*q + r = a and 0 <= r < b (assuming- a and b are >= 0).
		 *
		 *   If you want the relationship to extend for negative a
		 *   (keeping b positive), you have two choices: if you truncate q
		 *   towards zero, r will become negative, so that the invariant
		 *   changes to 0 <= abs(r) < abs(b). otherwise, you can floor q
		 *   towards negative infinity, and the invariant remains 0 <= r < b.
		 */

		q = xv / yv;
		HAK_ASSERT(hak, HAK_IN_SMOOI_RANGE(q));

		ri = xv - yv * q; /* xv % yv; */
		if (ri)
		{
			if (modulo)
			{
				/* modulo */
				/*
					xv      yv      q       r
					-------------------------
					 7       3      2       1
					-7       3     -3       2
					 7      -3     -3      -2
					-7      -3      2      -1
				 */

				/* r must be floored. that is, it rounds away from zero
				 * and towards negative infinity */
				if (IS_SIGN_DIFF(yv, ri))
				{
					/* if the divisor has a different sign from r,
					 * change the sign of r to the divisor's sign */
					ri += yv;
					--q;
					HAK_ASSERT(hak, ri && !IS_SIGN_DIFF(yv, ri));
				}
			}
			else
			{
				/* remainder */
				/*
					xv      yv      q       r
					-------------------------
					 7       3      2       1
					-7       3     -2      -1
					 7      -3     -2       1
					-7      -3      2      -1
				 */
				if (xv && IS_SIGN_DIFF(xv, ri))
				{
					/* if the dividend has a different sign from r,
					 * change the sign of r to the dividend's sign.
					 * all the compilers i've worked with produced
					 * the quotient and the remainder that don't need
					 * any adjustment. however, there may be an esoteric
					 * architecture. */
					ri -= yv;
					++q;
					HAK_ASSERT(hak, xv && !IS_SIGN_DIFF(xv, ri));
				}
			}
		}

		if (rem)
		{
			HAK_ASSERT(hak, HAK_IN_SMOOI_RANGE(ri));
			*rem = HAK_SMOOI_TO_OOP(ri);
		}

		return HAK_SMOOI_TO_OOP((hak_ooi_t)q);
	}
	else
	{
		if (HAK_OOP_IS_SMOOI(x))
		{
			hak_ooi_t xv;

			if (!hak_isbigint(hak,y)) goto oops_einval;

			/* divide a small integer by a big integer.
			 * the dividend is guaranteed to be greater than the divisor
			 * if both are positive. */

			xv = HAK_OOP_TO_SMOOI(x);
			x_neg_sign = (xv < 0);
			y_neg_sign = HAK_IS_NBIGINT(hak, y);
			if (x_neg_sign == y_neg_sign || !modulo)
			{
				/* simple. the quotient is zero and the
				 * dividend becomes the remainder as a whole. */
				if (rem) *rem = x;
				return HAK_SMOOI_TO_OOP(0);
			}

			/* carry on to the full bigint division */
			hak_pushvolat(hak, &y);
			x = make_bigint_with_ooi(hak, xv);
			hak_popvolat(hak);
			if (HAK_UNLIKELY(!x)) return HAK_NULL;
		}
		else if (HAK_OOP_IS_SMOOI(y))
		{
			hak_ooi_t yv;

			if (!hak_isbigint(hak,x)) goto oops_einval;

			/* divide a big integer by a small integer. */

			yv = HAK_OOP_TO_SMOOI(y);
			switch (yv)
			{
				case 0:
					hak_seterrnum(hak, HAK_EDIVBY0);
					return HAK_NULL;

				case 1:
					z = clone_bigint(hak, x, HAK_OBJ_GET_SIZE(x));
					if (HAK_UNLIKELY(!z)) return HAK_NULL;
					if (rem) *rem = HAK_SMOOI_TO_OOP(0);
					return z;

				case -1:
					z = clone_bigint_negated(hak, x, HAK_OBJ_GET_SIZE(x));
					if (HAK_UNLIKELY(!z)) return HAK_NULL;
					if (rem) *rem = HAK_SMOOI_TO_OOP(0);
					return z;

				default:
				{
					hak_lidw_t dw;
					hak_liw_t carry = 0;
					hak_liw_t* zw;
					hak_oow_t zs, i;
					hak_ooi_t yv_abs, ri;

					yv_abs = (yv < 0)? -yv: yv;
				#if (HAK_LIW_BITS < HAK_OOI_BITS)
					if (yv_abs > HAK_TYPE_MAX(hak_liw_t)) break;
				#endif

					x_neg_sign = (HAK_IS_NBIGINT(hak, x));
					y_neg_sign = (yv < 0);

					z = clone_bigint_to_positive(hak, x, HAK_OBJ_GET_SIZE(x));
					if (HAK_UNLIKELY(!z)) return HAK_NULL;

					zw = ((hak_oop_liword_t)z)->slot;
					zs = HAK_OBJ_GET_SIZE(z);
					for (i = zs; i > 0; )
					{
						--i;
						dw = ((hak_lidw_t)carry << HAK_LIW_BITS) + zw[i];
						/* TODO: optimize it with ASM - no seperate / and % */
						zw[i] = (hak_liw_t)(dw / yv_abs);
						carry = (hak_liw_t)(dw % yv_abs);
					}
					/*if (zw[zs - 1] == 0) zs--;*/

					HAK_ASSERT(hak, carry <= HAK_SMOOI_MAX);
					ri = carry;
					if (x_neg_sign) ri = -ri;

					z = normalize_bigint(hak, z);
					if (HAK_UNLIKELY(!z)) return HAK_NULL;

					if (x_neg_sign != y_neg_sign)
					{
						HAK_OBJ_SET_CLASS (z, (hak_oop_t)hak->c_large_negative_integer);
						if (ri && modulo)
						{
							z = hak_subints(hak, z, HAK_SMOOI_TO_OOP(1));
							if (HAK_UNLIKELY(!z)) return HAK_NULL;
							if (rem)
							{
								hak_pushvolat(hak, &z);
								r = hak_addints(hak, HAK_SMOOI_TO_OOP(ri), HAK_SMOOI_TO_OOP(yv));
								hak_popvolat(hak);
								if (HAK_UNLIKELY(!r)) return HAK_NULL;
								*rem = r;
							}
							return z;
						}
					}

					if (rem) *rem = HAK_SMOOI_TO_OOP(ri);
					return z;
				}
			}

			/* carry on to the full bigint division */
			hak_pushvolat(hak, &x);
			y = make_bigint_with_ooi(hak, yv);
			hak_popvolat(hak);
			if (HAK_UNLIKELY(!y)) return HAK_NULL;
		}
		else
		{
			if (!hak_isbigint(hak,x)) goto oops_einval;
			if (!hak_isbigint(hak,y)) goto oops_einval;
		}
	}

	x_neg_sign = HAK_IS_NBIGINT(hak, x);
	y_neg_sign = HAK_IS_NBIGINT(hak, y);

	hak_pushvolat(hak, &x);
	hak_pushvolat(hak, &y);
	z = divide_unsigned_integers(hak, x, y, &r);
	hak_popvolats(hak, 2);
	if (HAK_UNLIKELY(!z)) return HAK_NULL;

	if (x_neg_sign)
	{
		/* the class on r must be set before normalize_bigint()
		 * because it can get changed to a small integer */
		HAK_OBJ_SET_CLASS (r, (hak_oop_t)hak->c_large_negative_integer);
	}

	if (x_neg_sign != y_neg_sign)
	{
		HAK_OBJ_SET_CLASS (z, (hak_oop_t)hak->c_large_negative_integer);

		hak_pushvolat(hak, &z);
		hak_pushvolat(hak, &y);
		r = normalize_bigint(hak, r);
		hak_popvolats(hak, 2);
		if (HAK_UNLIKELY(!r)) return HAK_NULL;

		if (r != HAK_SMOOI_TO_OOP(0) && modulo)
		{
			if (rem)
			{
				hak_pushvolat(hak, &z);
				hak_pushvolat(hak, &y);
				r = hak_addints(hak, r, y);
				hak_popvolats(hak, 2);
				if (HAK_UNLIKELY(!r)) return HAK_NULL;

				hak_pushvolat(hak, &r);
				z = normalize_bigint(hak, z);
				hak_popvolat(hak);
				if (HAK_UNLIKELY(!z)) return HAK_NULL;

				hak_pushvolat(hak, &r);
				z = hak_subints(hak, z, HAK_SMOOI_TO_OOP(1));
				hak_popvolat(hak);
				if (HAK_UNLIKELY(!z)) return HAK_NULL;

				*rem = r;
				return z;
			}
			else
			{
				/* remainder is not needed at all */
/* TODO: subtract 1 without normalization??? */
				z = normalize_bigint(hak, z);
				if (HAK_UNLIKELY(!z)) return HAK_NULL;
				return hak_subints(hak, z, HAK_SMOOI_TO_OOP(1));
			}
		}
	}
	else
	{
		hak_pushvolat(hak, &z);
		r = normalize_bigint(hak, r);
		hak_popvolat(hak);
		if (HAK_UNLIKELY(!r)) return HAK_NULL;
	}

	hak_pushvolat(hak, &r);
	z = normalize_bigint(hak, z);
	hak_popvolat(hak);

	if (z && rem) *rem = r;
	return z;

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O, %O", x, y);
	return HAK_NULL;
}


hak_oop_t hak_negateint (hak_t* hak, hak_oop_t x)
{
	if (HAK_OOP_IS_SMOOI(x))
	{
		hak_ooi_t v;
		v = HAK_OOP_TO_SMOOI(x);
		return HAK_SMOOI_TO_OOP(-v);
	}
	else
	{
		if (!hak_isbigint(hak, x)) goto oops_einval;
		return clone_bigint_negated(hak, x, HAK_OBJ_GET_SIZE(x));
	}

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O", x);
	return HAK_NULL;
}

hak_oop_t hak_bitatint (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	/* y is 0-based */

	if (HAK_OOP_IS_SMOOI(x) && HAK_OOP_IS_SMOOI(y))
	{
		hak_ooi_t v1, v2, v3;

		v1 = HAK_OOP_TO_SMOOI(x);
		v2 = HAK_OOP_TO_SMOOI(y);

		if (v2 < 0) return HAK_SMOOI_TO_OOP(0);
		if (v1 >= 0)
		{
			/* the absolute value may be composed of up to
			 * HAK_SMOOI_BITS - 1 bits as there is a sign bit.*/
			if (v2 >= HAK_SMOOI_BITS - 1) return HAK_SMOOI_TO_OOP(0);
			v3 = ((hak_oow_t)v1 >> v2) & 1;
		}
		else
		{
			if (v2 >= HAK_SMOOI_BITS - 1) return HAK_SMOOI_TO_OOP(1);
			v3 = ((~(hak_oow_t)-v1 + 1) >> v2) & 1;
		}
		return HAK_SMOOI_TO_OOP(v3);
	}
	else if (HAK_OOP_IS_SMOOI(x))
	{
		if (!hak_isbigint(hak, y)) goto oops_einval;

		if (HAK_IS_NBIGINT(hak, y)) return HAK_SMOOI_TO_OOP(0);

		/* y is definitely >= HAK_SMOOI_BITS */
		if (HAK_OOP_TO_SMOOI(x) >= 0)
			return HAK_SMOOI_TO_OOP(0);
		else
			return HAK_SMOOI_TO_OOP(1);
	}
	else if (HAK_OOP_IS_SMOOI(y))
	{
		hak_ooi_t v;
		hak_oow_t wp, bp, xs;

		if (!hak_isbigint(hak, x)) goto oops_einval;
		v = HAK_OOP_TO_SMOOI(y);

		if (v < 0) return HAK_SMOOI_TO_OOP(0);
		wp = v / HAK_LIW_BITS;
		bp = v - (wp * HAK_LIW_BITS);

		xs = HAK_OBJ_GET_SIZE(x);
		if (HAK_IS_PBIGINT(hak, x))
		{
			if (wp >= xs) return HAK_SMOOI_TO_OOP(0);
			v = (((hak_oop_liword_t)x)->slot[wp] >> bp) & 1;
		}
		else
		{
			hak_lidw_t w, carry;
			hak_oow_t i;

			if (wp >= xs) return HAK_SMOOI_TO_OOP(1);

			carry = 1;
			for (i = 0; i <= wp; i++)
			{
				w = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)x)->slot[i]) + carry;
				carry = w >> HAK_LIW_BITS;
			}
			v = ((hak_oow_t)w >> bp) & 1;
		}

		return HAK_SMOOI_TO_OOP(v);
	}
	else
	{
	#if defined(HAK_LIMIT_OBJ_SIZE)
		/* nothing */
	#else
		hak_oow_t w, wp, bp, xs;
		hak_ooi_t v;
		int sign;
	#endif

		if (!hak_isbigint(hak, x) || !hak_isbigint(hak, y)) goto oops_einval;

	#if defined(HAK_LIMIT_OBJ_SIZE)
		if (HAK_IS_NBIGINT(hak, y)) return HAK_SMOOI_TO_OOP(0);

		HAK_ASSERT(hak, HAK_OBJ_SIZE_BITS_MAX <= HAK_TYPE_MAX(hak_oow_t));
		if (HAK_IS_PBIGINT(hak, x))
		{
			return HAK_SMOOI_TO_OOP(0);
		}
		else
		{
			return HAK_SMOOI_TO_OOP(1);
		}
	#else
		xs = HAK_OBJ_GET_SIZE(x);

		if (HAK_IS_NBIGINT(hak, y)) return HAK_SMOOI_TO_OOP(0);

		sign = bigint_to_oow_noseterr(hak, y, &w);
		HAK_ASSERT(hak, sign >= 0);
		if (sign >= 1)
		{
			wp = w / HAK_LIW_BITS;
			bp = w - (wp * HAK_LIW_BITS);
		}
		else
		{
			hak_oop_t quo, rem;

			HAK_ASSERT(hak, sign == 0);

			hak_pushvolat(hak, &x);
			quo = hak_divints(hak, y, HAK_SMOOI_TO_OOP(HAK_LIW_BITS), 0, &rem);
			hak_popvolat(hak);
			if (!quo) return HAK_NULL;

			sign = integer_to_oow_noseterr(hak, quo, &wp);
			HAK_ASSERT(hak, sign >= 0);
			if (sign == 0)
			{
				/* too large. set it to xs so that it gets out of
				 * the valid range */
				wp = xs;
			}

			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(rem));
			bp = HAK_OOP_TO_SMOOI(rem);
			HAK_ASSERT(hak, bp >= 0 && bp < HAK_LIW_BITS);
		}

		if (HAK_IS_PBIGINT(hak, x))
		{
			if (wp >= xs) return HAK_SMOOI_TO_OOP(0);
			v = (((hak_oop_liword_t)x)->slot[wp] >> bp) & 1;
		}
		else
		{
			hak_lidw_t w, carry;
			hak_oow_t i;

			if (wp >= xs) return HAK_SMOOI_TO_OOP(1);

			carry = 1;
			for (i = 0; i <= wp; i++)
			{
				w = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)x)->slot[i]) + carry;
				carry = w >> HAK_LIW_BITS;
			}
			v = ((hak_oow_t)w >> bp) & 1;
		}

		return HAK_SMOOI_TO_OOP(v);
	#endif
	}

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O, %O", x, y);
	return HAK_NULL;
}

hak_oop_t hak_bitandints (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	if (HAK_OOP_IS_SMOOI(x) && HAK_OOP_IS_SMOOI(y))
	{
		hak_ooi_t v1, v2, v3;

		v1 = HAK_OOP_TO_SMOOI(x);
		v2 = HAK_OOP_TO_SMOOI(y);
		v3 = v1 & v2;

		if (HAK_IN_SMOOI_RANGE(v3)) return HAK_SMOOI_TO_OOP(v3);
		return make_bigint_with_ooi(hak, v3);
	}
	else if (HAK_OOP_IS_SMOOI(x))
	{
		hak_ooi_t v;

		if (!hak_isbigint(hak, y)) goto oops_einval;

		v = HAK_OOP_TO_SMOOI(x);
		if (v == 0) return HAK_SMOOI_TO_OOP(0);

		hak_pushvolat(hak, &y);
		x = make_bigint_with_ooi(hak, v);
		hak_popvolat(hak);
		if (HAK_UNLIKELY(!x)) return HAK_NULL;

		goto bigint_and_bigint;
	}
	else if (HAK_OOP_IS_SMOOI(y))
	{
		hak_ooi_t v;

		if (!hak_isbigint(hak, x)) goto oops_einval;

		v = HAK_OOP_TO_SMOOI(y);
		if (v == 0) return HAK_SMOOI_TO_OOP(0);

		hak_pushvolat(hak, &x);
		y = make_bigint_with_ooi(hak, v);
		hak_popvolat(hak);
		if (HAK_UNLIKELY(!x)) return HAK_NULL;

		goto bigint_and_bigint;
	}
	else
	{
		hak_oop_t z;
		hak_oow_t i, xs, ys, zs, zalloc;
		int negx, negy;

		if (!hak_isbigint(hak,x) || !hak_isbigint(hak, y)) goto oops_einval;

	bigint_and_bigint:
		xs = HAK_OBJ_GET_SIZE(x);
		ys = HAK_OBJ_GET_SIZE(y);

		if (xs < ys)
		{
			/* make sure that x is greater than or equal to y */
			z = x;
			x = y;
			y = z;
			zs = ys;
			ys = xs;
			xs = zs;
		}

		negx = HAK_IS_NBIGINT(hak, x);
		negy = HAK_IS_NBIGINT(hak, y);

		if (negx && negy)
		{
			zalloc = xs + 1;
			zs = xs;
		}
		else if (negx)
		{
			zalloc = ys;
			zs = ys;
		}
		else if (negy)
		{
			zalloc = xs;
			zs = xs;
		}
		else
		{
			zalloc = ys;
			zs = ys;
		}

		hak_pushvolat(hak, &x);
		hak_pushvolat(hak, &y);
		z = make_pbigint(hak, HAK_NULL, zalloc);
		hak_popvolats(hak, 2);
		if (HAK_UNLIKELY(!z)) return HAK_NULL;

		if (negx && negy)
		{
			/* both are negative */
			hak_lidw_t w[2];
			hak_lidw_t carry[2];

			carry[0] = 1;
			carry[1] = 1;
			/* 2's complement on both x and y and perform bitwise-and */
			for (i = 0; i < ys; i++)
			{
				w[0] = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)x)->slot[i]) + carry[0];
				carry[0] = w[0] >> HAK_LIW_BITS;

				w[1] = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)y)->slot[i]) + carry[1];
				carry[1] = w[1] >> HAK_LIW_BITS;

				((hak_oop_liword_t)z)->slot[i] = (hak_liw_t)w[0] & (hak_liw_t)w[1];
			}
			HAK_ASSERT(hak, carry[1] == 0);

			/* 2's complement on the remaining part of x. the lacking part
			 * in y is treated as if they are all 1s. */
			for (; i < xs; i++)
			{
				w[0] = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)x)->slot[i]) + carry[0];
				carry[0] = w[0] >> HAK_LIW_BITS;
				((hak_oop_liword_t)z)->slot[i] = (hak_liw_t)w[0];
			}
			HAK_ASSERT(hak, carry[0] == 0);

			/* 2's complement on the final result */
			((hak_oop_liword_t)z)->slot[zs] = ~(hak_liw_t)0;
			carry[0] = 1;
			for (i = 0; i <= zs; i++)
			{
				w[0] = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)z)->slot[i]) + carry[0];
				carry[0] = w[0] >> HAK_LIW_BITS;
				((hak_oop_liword_t)z)->slot[i] = (hak_liw_t)w[0];
			}
			HAK_ASSERT(hak, carry[0] == 0);

			HAK_OBJ_SET_CLASS (z, (hak_oop_t)hak->c_large_negative_integer);
		}
		else if (negx)
		{
			/* x is negative, y is positive */
			hak_lidw_t w, carry;

			carry = 1;
			for (i = 0; i < ys; i++)
			{
				w = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)x)->slot[i]) + carry;
				carry = w >> HAK_LIW_BITS;
				((hak_oop_liword_t)z)->slot[i] = (hak_liw_t)w & ((hak_oop_liword_t)y)->slot[i];
			}

			/* the lacking part in y is all 0's. the remaining part in x is
			 * just masked out when bitwise-anded with 0. so nothing is done
			 * to handle the remaining part in x */
		}
		else if (negy)
		{
			/* x is positive, y is negative  */
			hak_lidw_t w, carry;

			/* x & 2's complement on y up to ys */
			carry = 1;
			for (i = 0; i < ys; i++)
			{
				w = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)y)->slot[i]) + carry;
				carry = w >> HAK_LIW_BITS;
				((hak_oop_liword_t)z)->slot[i] = ((hak_oop_liword_t)x)->slot[i] & (hak_liw_t)w;
			}
			HAK_ASSERT(hak, carry == 0);

			/* handle the longer part in x than y
			 *
			 * For example,
			 *  x => + 1010 1100
			 *  y => -      0011
			 *
			 * If y is extended to the same length as x,
			 * it is a negative 0000 0001.
			 * 2's complement is performed on this imaginary extension.
			 * the result is '1111 1101' (1111 1100 + 1).
			 *
			 * when y is shorter and negative, the lacking part can be
			 * treated as all 1s in the 2's complement format.
			 *
			 * the remaining part in x can be just copied to the
			 * final result 'z'.
			 */
			for (; i < xs; i++)
			{
				((hak_oop_liword_t)z)->slot[i] = ((hak_oop_liword_t)x)->slot[i];
			}
		}
		else
		{
			/* both are positive */
			for (i = 0; i < ys; i++)
			{
				((hak_oop_liword_t)z)->slot[i] = ((hak_oop_liword_t)x)->slot[i] & ((hak_oop_liword_t)y)->slot[i];
			}
		}

		return normalize_bigint(hak, z);
	}

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O, %O", x, y);
	return HAK_NULL;
}

hak_oop_t hak_bitorints (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	if (HAK_OOP_IS_SMOOI(x) && HAK_OOP_IS_SMOOI(y))
	{
		hak_ooi_t v1, v2, v3;

		v1 = HAK_OOP_TO_SMOOI(x);
		v2 = HAK_OOP_TO_SMOOI(y);
		v3 = v1 | v2;

		if (HAK_IN_SMOOI_RANGE(v3)) return HAK_SMOOI_TO_OOP(v3);
		return make_bigint_with_ooi(hak, v3);
	}
	else if (HAK_OOP_IS_SMOOI(x))
	{
		hak_ooi_t v;

		if (!hak_isbigint(hak, y)) goto oops_einval;

		v = HAK_OOP_TO_SMOOI(x);
		if (v == 0) return clone_bigint(hak, y, HAK_OBJ_GET_SIZE(y));

		hak_pushvolat(hak, &y);
		x = make_bigint_with_ooi(hak, v);
		hak_popvolat(hak);
		if (HAK_UNLIKELY(!x)) return HAK_NULL;

		goto bigint_and_bigint;
	}
	else if (HAK_OOP_IS_SMOOI(y))
	{
		hak_ooi_t v;

		if (!hak_isbigint(hak, x)) goto oops_einval;

		v = HAK_OOP_TO_SMOOI(y);
		if (v == 0) return clone_bigint(hak, x, HAK_OBJ_GET_SIZE(x));

		hak_pushvolat(hak, &x);
		y = make_bigint_with_ooi(hak, v);
		hak_popvolat(hak);
		if (HAK_UNLIKELY(!x)) return HAK_NULL;

		goto bigint_and_bigint;
	}
	else
	{
		hak_oop_t z;
		hak_oow_t i, xs, ys, zs, zalloc;
		int negx, negy;

		if (!hak_isbigint(hak,x) || !hak_isbigint(hak, y)) goto oops_einval;

	bigint_and_bigint:
		xs = HAK_OBJ_GET_SIZE(x);
		ys = HAK_OBJ_GET_SIZE(y);

		if (xs < ys)
		{
			/* make sure that x is greater than or equal to y */
			z = x;
			x = y;
			y = z;
			zs = ys;
			ys = xs;
			xs = zs;
		}

		negx = HAK_IS_NBIGINT(hak, x);
		negy = HAK_IS_NBIGINT(hak, y);

		if (negx && negy)
		{
			zalloc = ys + 1;
			zs = ys;
		}
		else if (negx)
		{
			zalloc = xs + 1;
			zs = xs;
		}
		else if (negy)
		{
			zalloc = ys + 1;
			zs = ys;
		}
		else
		{
			zalloc = xs;
			zs = xs;
		}

		if (zalloc < zs)
		{
			/* overflow in zalloc calculation above */
			hak_seterrnum(hak, HAK_EOOMEM); /* TODO: is it a soft failure or hard failure? */
			return HAK_NULL;
		}

		hak_pushvolat(hak, &x);
		hak_pushvolat(hak, &y);
		z = make_pbigint(hak, HAK_NULL, zalloc);
		hak_popvolats(hak, 2);
		if (HAK_UNLIKELY(!z)) return HAK_NULL;

		if (negx && negy)
		{
			/* both are negative */
			hak_lidw_t w[2];
			hak_lidw_t carry[2];

			carry[0] = 1;
			carry[1] = 1;
			/* 2's complement on both x and y and perform bitwise-and */
			for (i = 0; i < ys; i++)
			{
				w[0] = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)x)->slot[i]) + carry[0];
				carry[0] = w[0] >> HAK_LIW_BITS;

				w[1] = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)y)->slot[i]) + carry[1];
				carry[1] = w[1] >> HAK_LIW_BITS;

				((hak_oop_liword_t)z)->slot[i] = (hak_liw_t)w[0] | (hak_liw_t)w[1];
			}
			HAK_ASSERT(hak, carry[1] == 0);

			/* do nothing about the extra part in x and the lacking part
			 * in y for the reason shown in [NOTE] in the 'else if' block
			 * further down. */

		adjust_to_negative:
			/* 2's complement on the final result */
			((hak_oop_liword_t)z)->slot[zs] = ~(hak_liw_t)0;
			carry[0] = 1;
			for (i = 0; i <= zs; i++)
			{
				w[0] = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)z)->slot[i]) + carry[0];
				carry[0] = w[0] >> HAK_LIW_BITS;
				((hak_oop_liword_t)z)->slot[i] = (hak_liw_t)w[0];
			}
			HAK_ASSERT(hak, carry[0] == 0);

			HAK_OBJ_SET_CLASS (z, (hak_oop_t)hak->c_large_negative_integer);
		}
		else if (negx)
		{
			/* x is negative, y is positive */
			hak_lidw_t w, carry;

			carry = 1;
			for (i = 0; i < ys; i++)
			{
				w = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)x)->slot[i]) + carry;
				carry = w >> HAK_LIW_BITS;
				((hak_oop_liword_t)z)->slot[i] = (hak_liw_t)w | ((hak_oop_liword_t)y)->slot[i];
			}

			for (; i < xs; i++)
			{
				w = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)x)->slot[i]) + carry;
				carry = w >> HAK_LIW_BITS;
				((hak_oop_liword_t)z)->slot[i] = (hak_liw_t)w;
			}

			HAK_ASSERT(hak, carry == 0);
			goto adjust_to_negative;
		}
		else if (negy)
		{
			/* x is positive, y is negative  */
			hak_lidw_t w, carry;

			/* x & 2's complement on y up to ys */
			carry = 1;
			for (i = 0; i < ys; i++)
			{
				w = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)y)->slot[i]) + carry;
				carry = w >> HAK_LIW_BITS;
				((hak_oop_liword_t)z)->slot[i] = ((hak_oop_liword_t)x)->slot[i] | (hak_liw_t)w;
			}
			HAK_ASSERT(hak, carry == 0);

			/* [NOTE]
			 *  in theory, the lacking part in ys is all 1s when y is
			 *  extended to the width of x. but those 1s are inverted to
			 *  0s when another 2's complement is performed over the final
			 *  result after the jump to 'adjust_to_negative'.
			 *  setting zs to 'xs + 1' and performing the following loop is
			 *  redundant.
			for (; i < xs; i++)
			{
				((hak_oop_liword_t)z)->slot[i] = ~(hak_liw_t)0;
			}
			*/
			goto adjust_to_negative;
		}
		else
		{
			/* both are positive */
			for (i = 0; i < ys; i++)
			{
				((hak_oop_liword_t)z)->slot[i] = ((hak_oop_liword_t)x)->slot[i] | ((hak_oop_liword_t)y)->slot[i];
			}

			for (; i < xs; i++)
			{
				((hak_oop_liword_t)z)->slot[i] = ((hak_oop_liword_t)x)->slot[i];
			}
		}

		return normalize_bigint(hak, z);
	}

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O, %O", x, y);
	return HAK_NULL;
}

hak_oop_t hak_bitxorints (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	if (HAK_OOP_IS_SMOOI(x) && HAK_OOP_IS_SMOOI(y))
	{
		hak_ooi_t v1, v2, v3;

		v1 = HAK_OOP_TO_SMOOI(x);
		v2 = HAK_OOP_TO_SMOOI(y);
		v3 = v1 ^ v2;

		if (HAK_IN_SMOOI_RANGE(v3)) return HAK_SMOOI_TO_OOP(v3);
		return make_bigint_with_ooi(hak, v3);
	}
	else if (HAK_OOP_IS_SMOOI(x))
	{
		hak_ooi_t v;

		if (!hak_isbigint(hak, y)) goto oops_einval;

		v = HAK_OOP_TO_SMOOI(x);
		if (v == 0) return clone_bigint(hak, y, HAK_OBJ_GET_SIZE(y));

		hak_pushvolat(hak, &y);
		x = make_bigint_with_ooi(hak, v);
		hak_popvolat(hak);
		if (HAK_UNLIKELY(!x)) return HAK_NULL;

		goto bigint_and_bigint;
	}
	else if (HAK_OOP_IS_SMOOI(y))
	{
		hak_ooi_t v;

		if (!hak_isbigint(hak, x)) goto oops_einval;

		v = HAK_OOP_TO_SMOOI(y);
		if (v == 0) return clone_bigint(hak, x, HAK_OBJ_GET_SIZE(x));

		hak_pushvolat(hak, &x);
		y = make_bigint_with_ooi(hak, v);
		hak_popvolat(hak);
		if (HAK_UNLIKELY(!x)) return HAK_NULL;

		goto bigint_and_bigint;
	}
	else
	{
		hak_oop_t z;
		hak_oow_t i, xs, ys, zs, zalloc;
		int negx, negy;

		if (!hak_isbigint(hak,x) || !hak_isbigint(hak, y)) goto oops_einval;

	bigint_and_bigint:
		xs = HAK_OBJ_GET_SIZE(x);
		ys = HAK_OBJ_GET_SIZE(y);

		if (xs < ys)
		{
			/* make sure that x is greater than or equal to y */
			z = x;
			x = y;
			y = z;
			zs = ys;
			ys = xs;
			xs = zs;
		}

		negx = HAK_IS_NBIGINT(hak, x);
		negy = HAK_IS_NBIGINT(hak, y);

		if (negx && negy)
		{
			zalloc = xs;
			zs = xs;
		}
		else if (negx)
		{
			zalloc = xs + 1;
			zs = xs;
		}
		else if (negy)
		{
			zalloc = xs + 1;
			zs = xs;
		}
		else
		{
			zalloc = xs;
			zs = xs;
		}

		if (zalloc < zs)
		{
			/* overflow in zalloc calculation above */
			hak_seterrnum(hak, HAK_EOOMEM); /* TODO: is it a soft failure or hard failure? */
			return HAK_NULL;
		}

		hak_pushvolat(hak, &x);
		hak_pushvolat(hak, &y);
		z = make_pbigint(hak, HAK_NULL, zalloc);
		hak_popvolats(hak, 2);
		if (!z) return HAK_NULL;

		if (negx && negy)
		{
			/* both are negative */
			hak_lidw_t w[2];
			hak_lidw_t carry[2];

			carry[0] = 1;
			carry[1] = 1;
			/* 2's complement on both x and y and perform bitwise-and */
			for (i = 0; i < ys; i++)
			{
				w[0] = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)x)->slot[i]) + carry[0];
				carry[0] = w[0] >> HAK_LIW_BITS;

				w[1] = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)y)->slot[i]) + carry[1];
				carry[1] = w[1] >> HAK_LIW_BITS;

				((hak_oop_liword_t)z)->slot[i] = (hak_liw_t)w[0] ^ (hak_liw_t)w[1];
			}
			HAK_ASSERT(hak, carry[1] == 0);

			/* treat the lacking part in y as all 1s */
			for (; i < xs; i++)
			{
				w[0] = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)x)->slot[i]) + carry[0];
				carry[0] = w[0] >> HAK_LIW_BITS;
				((hak_oop_liword_t)z)->slot[i] = (hak_liw_t)w[0] ^ (~(hak_liw_t)0);
			}
			HAK_ASSERT(hak, carry[0] == 0);
		}
		else if (negx)
		{
			/* x is negative, y is positive */
			hak_lidw_t w, carry;

			carry = 1;
			for (i = 0; i < ys; i++)
			{
				w = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)x)->slot[i]) + carry;
				carry = w >> HAK_LIW_BITS;
				((hak_oop_liword_t)z)->slot[i] = (hak_liw_t)w ^ ((hak_oop_liword_t)y)->slot[i];
			}

			/* treat the lacking part in y as all 0s */
			for (; i < xs; i++)
			{
				w = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)x)->slot[i]) + carry;
				carry = w >> HAK_LIW_BITS;
				((hak_oop_liword_t)z)->slot[i] = (hak_liw_t)w;
			}
			HAK_ASSERT(hak, carry == 0);

		adjust_to_negative:
			/* 2's complement on the final result */
			((hak_oop_liword_t)z)->slot[zs] = ~(hak_liw_t)0;
			carry = 1;
			for (i = 0; i <= zs; i++)
			{
				w = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)z)->slot[i]) + carry;
				carry = w >> HAK_LIW_BITS;
				((hak_oop_liword_t)z)->slot[i] = (hak_liw_t)w;
			}
			HAK_ASSERT(hak, carry == 0);

			HAK_OBJ_SET_CLASS (z, (hak_oop_t)hak->c_large_negative_integer);
		}
		else if (negy)
		{
			/* x is positive, y is negative  */
			hak_lidw_t w, carry;

			/* x & 2's complement on y up to ys */
			carry = 1;
			for (i = 0; i < ys; i++)
			{
				w = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)y)->slot[i]) + carry;
				carry = w >> HAK_LIW_BITS;
				((hak_oop_liword_t)z)->slot[i] = ((hak_oop_liword_t)x)->slot[i] ^ (hak_liw_t)w;
			}
			HAK_ASSERT(hak, carry == 0);

			/* treat the lacking part in y as all 1s */
			for (; i < xs; i++)
			{
				((hak_oop_liword_t)z)->slot[i] = ((hak_oop_liword_t)x)->slot[i] ^ (~(hak_liw_t)0);
			}

			goto adjust_to_negative;
		}
		else
		{
			/* both are positive */
			for (i = 0; i < ys; i++)
			{
				((hak_oop_liword_t)z)->slot[i] = ((hak_oop_liword_t)x)->slot[i] ^ ((hak_oop_liword_t)y)->slot[i];
			}

			/* treat the lacking part in y as all 0s */
			for (; i < xs; i++)
			{
				((hak_oop_liword_t)z)->slot[i] = ((hak_oop_liword_t)x)->slot[i];
			}
		}

		return normalize_bigint(hak, z);
	}

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O, %O", x, y);
	return HAK_NULL;
}

hak_oop_t hak_bitinvint (hak_t* hak, hak_oop_t x)
{
	if (HAK_OOP_IS_SMOOI(x))
	{
		hak_ooi_t v;

		v = HAK_OOP_TO_SMOOI(x);
		v = ~v;

		if (HAK_IN_SMOOI_RANGE(v)) return HAK_SMOOI_TO_OOP(v);
		return make_bigint_with_ooi(hak, v);
	}
	else
	{
		hak_oop_t z;
		hak_oow_t i, xs, zs, zalloc;
		int negx;

		if (!hak_isbigint(hak,x)) goto oops_einval;

		xs = HAK_OBJ_GET_SIZE(x);
		negx = HAK_IS_NBIGINT(hak, x);

		if (negx)
		{
			zalloc = xs;
			zs = xs;
		}
		else
		{
			zalloc = xs + 1;
			zs = xs;
		}

		if (zalloc < zs)
		{
			/* overflow in zalloc calculation above */
			hak_seterrnum(hak, HAK_EOOMEM); /* TODO: is it a soft failure or hard failure? */
			return HAK_NULL;
		}

		hak_pushvolat(hak, &x);
		z = make_pbigint(hak, HAK_NULL, zalloc);
		hak_popvolat(hak);
		if (HAK_UNLIKELY(!z)) return HAK_NULL;

		if (negx)
		{
			hak_lidw_t w, carry;

			carry = 1;
			for (i = 0; i < xs; i++)
			{
				w = (hak_lidw_t)((hak_liw_t)~HAK_OBJ_GET_LIWORD_VAL(x, i)) + carry;
				carry = w >> HAK_LIW_BITS;
				HAK_OBJ_SET_LIWORD_VAL (z, i, ~(hak_liw_t)w);
			}
			HAK_ASSERT(hak, carry == 0);
		}
		else
		{
			hak_lidw_t w, carry;

		#if 0
			for (i = 0; i < xs; i++)
			{
				HAK_OBJ_SET_LIWORD_VAL (z, i, ~HAK_OBJ_GET_LIWORD_VAL(x, i));
			}

			HAK_OBJ_SET_LIWORD_VAL (z, zs, ~(hak_liw_t)0);
			carry = 1;
			for (i = 0; i <= zs; i++)
			{
				w = (hak_lidw_t)((hak_liw_t)~HAK_OBJ_GET_LIWORD_VAL(z, i)) + carry;
				carry = w >> HAK_LIW_BITS;
				HAK_OBJ_SET_LIWORD_VAL (z, i, (hak_liw_t)w);
			}
			HAK_ASSERT(hak, carry == 0);
		#else
			carry = 1;
			for (i = 0; i < xs; i++)
			{
				w = (hak_lidw_t)(HAK_OBJ_GET_LIWORD_VAL(x, i)) + carry;
				carry = w >> HAK_LIW_BITS;
				HAK_OBJ_SET_LIWORD_VAL (z, i, (hak_liw_t)w);
			}
			HAK_ASSERT(hak, i == zs);
			HAK_OBJ_SET_LIWORD_VAL (z, i, (hak_liw_t)carry);
			HAK_ASSERT(hak, (carry >> HAK_LIW_BITS) == 0);
		#endif

			HAK_OBJ_SET_CLASS (z, (hak_oop_t)hak->c_large_negative_integer);
		}

		return normalize_bigint(hak, z);
	}

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O", x);
	return HAK_NULL;
}

static HAK_INLINE hak_oop_t rshift_negative_bigint (hak_t* hak, hak_oop_t x, hak_oow_t shift)
{
	hak_oop_t z;
	hak_lidw_t w;
	hak_lidw_t carry;
	hak_oow_t i, xs;

	HAK_ASSERT(hak, HAK_IS_NBIGINT(hak, x));
	xs = HAK_OBJ_GET_SIZE(x);

	hak_pushvolat(hak, &x);
	/* +1 for the second inversion below */
	z = make_nbigint(hak, HAK_NULL, xs + 1);
	hak_popvolat(hak);
	if (HAK_UNLIKELY(!z)) return HAK_NULL;

	/* the following lines roughly for 'z = hak_bitinv(hak, x)' */
	carry = 1;
	for (i = 0; i < xs; i++)
	{
		w = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)x)->slot[i]) + carry;
		carry = w >> HAK_LIW_BITS;
		HAK_OBJ_SET_LIWORD_VAL (z, i, ~(hak_liw_t)w);
	}
	HAK_ASSERT(hak, carry == 0);

	/* shift to the right */
	rshift_unsigned_array (((hak_oop_liword_t)z)->slot, xs, shift);

	/* the following lines roughly for 'z = hak_bitinv(hak, z)' */
#if 0
	for (i = 0; i < xs; i++)
	{
		HAK_OBJ_SET_LIWORD_VAL (z, i, ~HAK_OBJ_GET_LIWORD_VAL(z, i));
	}
	HAK_OBJ_SET_LIWORD_VAL (z, xs, ~(hak_liw_t)0);

	carry = 1;
	for (i = 0; i <= xs; i++)
	{
		w = (hak_lidw_t)((hak_liw_t)~((hak_oop_liword_t)z)->slot[i]) + carry;
		carry = w >> HAK_LIW_BITS;
		HAK_OBJ_SET_LIWORD_VAL (z, i, (hak_liw_t)w);
	}
	HAK_ASSERT(hak, carry == 0);
#else
	carry = 1;
	for (i = 0; i < xs; i++)
	{
		w = (hak_lidw_t)(((hak_oop_liword_t)z)->slot[i]) + carry;
		carry = w >> HAK_LIW_BITS;
		((hak_oop_liword_t)z)->slot[i] = (hak_liw_t)w;
	}
	HAK_OBJ_SET_LIWORD_VAL (z, i, (hak_liw_t)carry);
	HAK_ASSERT(hak, (carry >> HAK_LIW_BITS) == 0);
#endif

	return z; /* z is not normalized */
}

#if defined(HAK_LIMIT_OBJ_SIZE)
	/* nothing */
#else

static HAK_INLINE hak_oop_t rshift_negative_bigint_and_normalize (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	hak_oop_t z;
	hak_oow_t shift;
	int sign;

	HAK_ASSERT(hak, HAK_IS_NBIGINT(hak, x));
	HAK_ASSERT(hak, HAK_IS_NBIGINT(hak, y));

	/* for convenience in subtraction below.
	 * it could be HAK_TYPE_MAX(hak_oow_t)
	 * if make_bigint_with_intmax() or something
	 * similar were used instead of HAK_SMOOI_TO_OOP().*/
	shift = HAK_SMOOI_MAX;
	do
	{
		hak_pushvolat(hak, &y);
		z = rshift_negative_bigint(hak, x, shift);
		hak_popvolat(hak);
		if (HAK_UNLIKELY(!z)) return HAK_NULL;

		/* y is a negative number. use hak_addints() until it becomes 0 */
		hak_pushvolat(hak, &z);
		y = hak_addints(hak, y, HAK_SMOOI_TO_OOP(shift));
		hak_popvolat(hak);
		if (!y) return HAK_NULL;

		sign = integer_to_oow_noseterr(hak, y, &shift);
		if (sign == 0) shift = HAK_SMOOI_MAX;
		else
		{
			if (shift == 0)
			{
				/* no more shift */
				return normalize_bigint(hak, z);
			}
			HAK_ASSERT(hak, sign <= -1);
		}

		hak_pushvolat(hak, &y);
		x = normalize_bigint(hak, z);
		hak_popvolat(hak);
		if (HAK_UNLIKELY(!x)) return HAK_NULL;

		if (HAK_OOP_IS_SMOOI(x))
		{
			/* for normaization above, x can become a small integer */
			hak_ooi_t v;

			v = HAK_OOP_TO_SMOOI(x);
			HAK_ASSERT(hak, v < 0);

			/* normal right shift of a small negative integer */
			if (shift >= HAK_OOI_BITS - 1)
			{
				/* when y is still a large integer, this condition is met
				 * met as HAK_SMOOI_MAX > HAK_OOI_BITS. so i can simly
				 * terminate the loop after this */
				return HAK_SMOOI_TO_OOP(-1);
			}
			else
			{
				v = (hak_ooi_t)(((hak_oow_t)v >> shift) | HAK_HBMASK(hak_oow_t, shift));
				if (HAK_IN_SMOOI_RANGE(v))
					return HAK_SMOOI_TO_OOP(v);
				else
					return make_bigint_with_ooi(hak, v);
			}
		}
	}
	while (1);

	/* this part must not be reached */
	HAK_ASSERT(hak, !"internal error - must not happen");
	hak_seterrnum(hak, HAK_EINTERN);
	return HAK_NULL;
}

static HAK_INLINE hak_oop_t rshift_positive_bigint_and_normalize (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	hak_oop_t z;
	hak_oow_t zs, shift;
	int sign;

	HAK_ASSERT(hak, HAK_IS_PBIGINT(hak, x));
	HAK_ASSERT(hak, HAK_IS_NBIGINT(hak, y));

	zs = HAK_OBJ_GET_SIZE(x);

	hak_pushvolat(hak, &y);
	z = clone_bigint(hak, x, zs);
	hak_popvolat(hak);
	if (HAK_UNLIKELY(!z)) return HAK_NULL;

	/* for convenience in subtraction below.
	 * it could be HAK_TYPE_MAX(hak_oow_t)
	 * if make_bigint_with_intmax() or something
	 * similar were used instead of HAK_SMOOI_TO_OOP().*/
	shift = HAK_SMOOI_MAX;
	do
	{
		rshift_unsigned_array (((hak_oop_liword_t)z)->slot, zs, shift);
		if (count_effective(((hak_oop_liword_t)z)->slot, zs) == 1 &&
		    HAK_OBJ_GET_LIWORD_VAL(z, 0) == 0)
		{
			/* if z is 0, i don't have to go on */
			break;
		}

		/* y is a negative number. use hak_addints() until it becomes 0 */
		hak_pushvolat(hak, &z);
		y = hak_addints(hak, y, HAK_SMOOI_TO_OOP(shift));
		hak_popvolat(hak);
		if (!y) return HAK_NULL;

		sign = integer_to_oow_noseterr(hak, y, &shift);
		if (sign == 0) shift = HAK_SMOOI_MAX;
		else
		{
			if (shift == 0) break;
			HAK_ASSERT(hak, sign <= -1);
		}
	}
	while (1);

	return normalize_bigint(hak, z);
}

static HAK_INLINE hak_oop_t lshift_bigint_and_normalize (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	hak_oop_t z;
	hak_oow_t wshift, shift;
	int sign;

	HAK_ASSERT(hak, HAK_IS_PBIGINT(hak, y));

	/* this loop is very inefficient as shifting is repeated
	 * with lshift_unsigned_array(). however, this part of the
	 * code is not likey to be useful because the amount of
	 * memory available is certainly not enough to support
	 * huge shifts greater than HAK_TYPE_MAX(hak_oow_t) */
	shift = HAK_SMOOI_MAX;
	do
	{
		/* for convenience only in subtraction below.
		 * should it be between HAK_SMOOI_MAX and HAK_TYPE_MAX(hak_oow_t),
		 * the second parameter to hak_subints() can't be composed
		 * using HAK_SMOOI_TO_OOP() */
		wshift = shift / HAK_LIW_BITS;
		if (shift > wshift * HAK_LIW_BITS) wshift++;

		hak_pushvolat(hak, &y);
		z = expand_bigint(hak, x, wshift);
		hak_popvolat(hak);
		if (HAK_UNLIKELY(!z)) return HAK_NULL;

		lshift_unsigned_array (((hak_oop_liword_t)z)->slot, HAK_OBJ_GET_SIZE(z), shift);

		hak_pushvolat(hak, &y);
		x = normalize_bigint(hak, z);
		hak_popvolat(hak);
		if (HAK_UNLIKELY(!x)) return HAK_NULL;

		hak_pushvolat(hak, &x);
		y = hak_subints(hak, y, HAK_SMOOI_TO_OOP(shift));
		hak_popvolat(hak);
		if (!y) return HAK_NULL;

		sign = integer_to_oow_noseterr(hak, y, &shift);
		if (sign == 0) shift = HAK_SMOOI_MAX;
		else
		{
			if (shift == 0)
			{
				HAK_ASSERT(hak, is_normalized_integer(hak, x));
				return x;
			}
			HAK_ASSERT(hak, sign >= 1);
		}
	}
	while (1);

	/* this part must not be reached */
	HAK_ASSERT(hak, !"internal error - must not happen");
	hak_seterrnum(hak, HAK_EINTERN);
	return HAK_NULL;
}

#endif

hak_oop_t hak_bitshiftint (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	/* left shift if y is positive,
	 * right shift if y is negative */

	if (HAK_OOP_IS_SMOOI(x) && HAK_OOP_IS_SMOOI(y))
	{
		hak_ooi_t v1, v2;

		v1 = HAK_OOP_TO_SMOOI(x);
		v2 = HAK_OOP_TO_SMOOI(y);
		if (v1 == 0 || v2 == 0)
		{
			/* return without cloning as x is a small integer */
			return x;
		}

		if (v2 > 0)
		{
			/* left shift */
			hak_oop_t z;
			hak_oow_t wshift;

			wshift = v2 / HAK_LIW_BITS;
			if (v2 > wshift * HAK_LIW_BITS) wshift++;

			z = make_bloated_bigint_with_ooi(hak, v1, wshift);
			if (HAK_UNLIKELY(!z)) return HAK_NULL;

			lshift_unsigned_array (((hak_oop_liword_t)z)->slot, HAK_OBJ_GET_SIZE(z), v2);
			return normalize_bigint(hak, z);
		}
		else
		{
			/* right shift */
			hak_ooi_t v;

			v2 = -v2;
			if (v1 < 0)
			{
				/* guarantee arithmetic shift preserving the sign bit
				 * regardless of compiler implementation.
				 *
				 *    binary    decimal   shifted by
				 *   -------------------------------
				 *   10000011    (-125)    0
				 *   11000001    (-63)     1
				 *   11100000    (-32)     2
				 *   11110000    (-16)     3
				 *   11111000    (-8)      4
				 *   11111100    (-4)      5
				 *   11111110    (-2)      6
				 *   11111111    (-1)      7
				 *   11111111    (-1)      8
				 */

				if (v2 >= HAK_OOI_BITS - 1) v = -1;
				else
				{
					/* HAK_HBMASK_SAFE(hak_oow_t, v2 + 1) could also be
					 * used as a mask. but the sign bit is shifted in.
					 * so, masking up to 'v2' bits is sufficient */
					v = (hak_ooi_t)(((hak_oow_t)v1 >> v2) | HAK_HBMASK(hak_oow_t, v2));
				}
			}
			else
			{
				if (v2 >= HAK_OOI_BITS) v = 0;
				else v = v1 >> v2;
			}
			if (HAK_IN_SMOOI_RANGE(v)) return HAK_SMOOI_TO_OOP(v);
			return make_bigint_with_ooi(hak, v);
		}
	}
	else
	{
		int sign, negx, negy;
		hak_oow_t shift;

		if (HAK_OOP_IS_SMOOI(x))
		{
			hak_ooi_t v;

			if (!hak_isbigint(hak,y)) goto oops_einval;

			v = HAK_OOP_TO_SMOOI(x);
			if (v == 0) return HAK_SMOOI_TO_OOP(0);

			if (HAK_IS_NBIGINT(hak, y))
			{
				/* right shift - special case.
				 * x is a small integer. it is just a few bytes long.
				 * y is a large negative integer. its smallest absolute value
				 * is HAK_SMOOI_MAX. i know the final answer. */
				return (v < 0)? HAK_SMOOI_TO_OOP(-1): HAK_SMOOI_TO_OOP(0);
			}

			hak_pushvolat(hak, &y);
			x = make_bigint_with_ooi(hak, v);
			hak_popvolat(hak);
			if (HAK_UNLIKELY(!x)) return HAK_NULL;

			goto bigint_and_bigint;
		}
		else if (HAK_OOP_IS_SMOOI(y))
		{
			hak_ooi_t v;

			if (!hak_isbigint(hak,x)) goto oops_einval;

			v = HAK_OOP_TO_SMOOI(y);
			if (v == 0) return clone_bigint(hak, x, HAK_OBJ_GET_SIZE(x));

			negx = HAK_IS_NBIGINT(hak, x);
			if (v > 0)
			{
				sign = 1;
				negy = 0;
				shift = v;
				goto bigint_and_positive_oow;
			}
			else
			{
				sign = -1;
				negy = 1;
				shift = -v;
				goto bigint_and_negative_oow;
			}
		}
		else
		{
			hak_oop_t z;

			if (!hak_isbigint(hak,x) || !hak_isbigint(hak, y)) goto oops_einval;

		bigint_and_bigint:
			negx = HAK_IS_NBIGINT(hak, x);
			negy = HAK_IS_NBIGINT(hak, y);

			sign = bigint_to_oow_noseterr(hak, y, &shift);
			if (sign == 0)
			{
				/* y is too big or too small */
				if (negy)
				{
					/* right shift */
				#if defined(HAK_LIMIT_OBJ_SIZE)
					/* the maximum number of bit shifts are guaranteed to be
					 * small enough to fit into the hak_oow_t type. so i can
					 * easily assume that all bits are shifted out */
					HAK_ASSERT(hak, HAK_OBJ_SIZE_BITS_MAX <= HAK_TYPE_MAX(hak_oow_t));
					return (negx)? HAK_SMOOI_TO_OOP(-1): HAK_SMOOI_TO_OOP(0);
				#else
					if (negx)
						return rshift_negative_bigint_and_normalize(hak, x, y);
					else
						return rshift_positive_bigint_and_normalize(hak, x, y);
				#endif
				}
				else
				{
					/* left shift */
				#if defined(HAK_LIMIT_OBJ_SIZE)
					/* the maximum number of bit shifts are guaranteed to be
					 * small enough to fit into the hak_oow_t type. so i can
					 * simply return a failure here becuase it's surely too
					 * large after shifting */
					HAK_ASSERT(hak, HAK_TYPE_MAX(hak_oow_t) >= HAK_OBJ_SIZE_BITS_MAX);
					hak_seterrnum(hak, HAK_EOOMEM); /* is it a soft failure or a hard failure? is this error code proper? */
					return HAK_NULL;
				#else
					return lshift_bigint_and_normalize(hak, x, y);
				#endif
				}
			}
			else if (sign >= 1)
			{
				/* left shift */
				hak_oow_t wshift;

			bigint_and_positive_oow:
				wshift = shift / HAK_LIW_BITS;
				if (shift > wshift * HAK_LIW_BITS) wshift++;

				z = expand_bigint(hak, x, wshift);
				if (HAK_UNLIKELY(!z)) return HAK_NULL;

				lshift_unsigned_array (((hak_oop_liword_t)z)->slot, HAK_OBJ_GET_SIZE(z), shift);
			}
			else
			{
				/* right shift */
			bigint_and_negative_oow:

				HAK_ASSERT(hak, sign <= -1);

				if (negx)
				{
					z = rshift_negative_bigint(hak, x, shift);
					if (HAK_UNLIKELY(!z)) return HAK_NULL;
				}
				else
				{
					z = clone_bigint(hak, x, HAK_OBJ_GET_SIZE(x));
					if (HAK_UNLIKELY(!z)) return HAK_NULL;
					rshift_unsigned_array (((hak_oop_liword_t)z)->slot, HAK_OBJ_GET_SIZE(z), shift);
				}
			}

			return normalize_bigint(hak, z);
		}
	}

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O, %O", x, y);
	return HAK_NULL;
}

static hak_uint8_t ooch_val_tab[] =
{
	99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
	99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
	99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
	 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 99, 99, 99, 99, 99, 99,
	99, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
	25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 99, 99, 99, 99, 99,
	99, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
	25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 99, 99, 99, 99, 99
};

hak_oop_t hak_strtoint (hak_t* hak, const hak_ooch_t* str, hak_oow_t len, int radix)
{
	int sign = 1;
	const hak_ooch_t* ptr, * start, * end;
	hak_lidw_t w, v;
	hak_liw_t hw[16], * hwp = HAK_NULL;
	hak_oow_t hwlen, outlen;
	hak_oop_t res;

	if (radix < 0)
	{
		/* when radix is less than 0, it treats it as if '-' is preceeding */
		sign = -1;
		radix = -radix;
	}

	HAK_ASSERT(hak, radix >= 2 && radix <= 36);

	ptr = str;
	end = str + len;

	if (ptr < end)
	{
		if (*ptr == '+') ptr++;
		else if (*ptr == '-')
		{
			ptr++;
			sign = -1;
		}
	}

	if (ptr >= end) goto oops_einval; /* no digits */

	while (ptr < end && *ptr == '0')
	{
		/* skip leading zeros */
		ptr++;
	}
	if (ptr >= end)
	{
		/* all zeros */
		return HAK_SMOOI_TO_OOP(0);
	}

	hwlen = 0;
	start = ptr; /* this is the real start */

	if (IS_POW2(radix))
	{
		unsigned int exp;
		unsigned int bitcnt;

		/* get log2(radix) in a fast way under the fact that
		 * radix is a power of 2. the exponent acquired is
		 * the number of bits that a digit of the given radix takes up */
		/*exp = LOG2_FOR_POW2(radix);*/
		exp = _exp_tab[radix - 1];

		/* bytes */
		outlen = ((hak_oow_t)(end - str) * exp + 7) / 8;
		/* number of hak_liw_t */
		outlen = (outlen + HAK_SIZEOF(hw[0]) - 1) / HAK_SIZEOF(hw[0]);

		if (outlen > HAK_COUNTOF(hw))
		{
/* TODO: reuse this buffer? */
			hwp = (hak_liw_t*)hak_allocmem(hak, outlen * HAK_SIZEOF(hw[0]));
			if (!hwp) return HAK_NULL;
		}
		else
		{
			hwp = hw;
		}

		w = 0;
		bitcnt = 0;
		ptr = end - 1;

		while (ptr >= start)
		{
			if (*ptr < 0 || *ptr >= HAK_COUNTOF(ooch_val_tab)) goto oops_einval;
			v = ooch_val_tab[*ptr];
			if (v >= radix) goto oops_einval;

			w |= (v << bitcnt);
			bitcnt += exp;
			if (bitcnt >= HAK_LIW_BITS)
			{
				bitcnt -= HAK_LIW_BITS;
				hwp[hwlen++] = w; /*(hak_liw_t)(w & HAK_LBMASK(hak_lidw_t, HAK_LIW_BITS));*/
				w >>= HAK_LIW_BITS;
			}

			ptr--;
		}

		HAK_ASSERT(hak, w <= HAK_TYPE_MAX(hak_liw_t));
		if (hwlen == 0 || w > 0) hwp[hwlen++] = w;
	}
	else
	{
		hak_lidw_t r1, r2;
		hak_liw_t multiplier;
		int dg, i, safe_ndigits;

		w = 0;
		ptr = start;

		safe_ndigits = hak->bigint[radix].safe_ndigits;
		multiplier = (hak_liw_t)hak->bigint[radix].multiplier;

		outlen = (end - str) / safe_ndigits + 1;
		if (outlen > HAK_COUNTOF(hw))
		{
			hwp = (hak_liw_t*)hak_allocmem(hak, outlen * HAK_SIZEOF(hak_liw_t));
			if (!hwp) return HAK_NULL;
		}
		else
		{
			hwp = hw;
		}

		HAK_ASSERT(hak, ptr < end);
		do
		{
			r1 = 0;
			for (dg = 0; dg < safe_ndigits; dg++)
			{
				if (ptr >= end)
				{
					multiplier = 1;
					for (i = 0; i < dg; i++) multiplier *= radix;
					break;
				}

				if (*ptr < 0 || *ptr >= HAK_COUNTOF(ooch_val_tab)) goto oops_einval;
				v = ooch_val_tab[*ptr];
				if (v >= radix) goto oops_einval;

				r1 = r1 * radix + (hak_liw_t)v;
				ptr++;
			}

			r2 = r1;
			for (i = 0; i < hwlen; i++)
			{
				hak_liw_t high, low;

				v = (hak_lidw_t)hwp[i] * multiplier;
				high = (hak_liw_t)(v >> HAK_LIW_BITS);
				low = (hak_liw_t)(v /*& HAK_LBMASK(hak_oow_t, HAK_LIW_BITS)*/);

			#if defined(liw_add_overflow)
				/* use liw_add_overflow() only if it's compiler-builtin. */
				r2 = high + liw_add_overflow(low, r2, &low);
			#else
				/* don't use the fall-back version of liw_add_overflow() */
				low += r2;
				r2 = (hak_lidw_t)high + (low < r2);
			#endif

				hwp[i] = low;
			}
			if (r2) hwp[hwlen++] = (hak_liw_t)r2;
		}
		while (ptr < end);
	}

	HAK_ASSERT(hak, hwlen >= 1);

#if (HAK_LIW_BITS == HAK_OOW_BITS)
	if (hwlen == 1)
	{
		w = hwp[0];
		HAK_ASSERT(hak, -HAK_SMOOI_MAX == HAK_SMOOI_MIN);
		if (w <= HAK_SMOOI_MAX) return HAK_SMOOI_TO_OOP((hak_ooi_t)w * sign);
	}
#elif (HAK_LIW_BITS == HAK_OOHW_BITS)
	if (hwlen == 1)
	{
		HAK_ASSERT(hak, hwp[0] <= HAK_SMOOI_MAX);
		return HAK_SMOOI_TO_OOP((hak_ooi_t)hwp[0] * sign);
	}
	else if (hwlen == 2)
	{
		w = MAKE_WORD(hwp[0], hwp[1]);
		HAK_ASSERT(hak, -HAK_SMOOI_MAX == HAK_SMOOI_MIN);
		if (w <= HAK_SMOOI_MAX) return HAK_SMOOI_TO_OOP((hak_ooi_t)w * sign);
	}
#else
#	error UNSUPPORTED LIW BIT SIZE
#endif

	res = hak_instantiate(hak, (sign < 0? hak->c_large_negative_integer: hak->c_large_positive_integer), hwp, hwlen);
	if (hwp && hw != hwp) hak_freemem(hak, hwp);

	return res;

oops_einval:
	if (hwp && hw != hwp) hak_freemem(hak, hwp);
	hak_seterrbfmt(hak, HAK_EINVAL, "unable to convert '%.*js' to integer", len, str);
	return HAK_NULL;
}

static hak_oow_t oow_to_text (hak_t* hak, hak_oow_t w, int flagged_radix, hak_ooch_t* buf)
{
	hak_ooch_t* ptr;
	const char* _digitc;
	int radix;

	radix = flagged_radix & HAK_INTTOSTR_RADIXMASK;
	_digitc =  _digitc_array[!!(flagged_radix & HAK_INTTOSTR_LOWERCASE)];
	HAK_ASSERT(hak, radix >= 2 && radix <= 36);

	ptr = buf;
	do
	{
		*ptr++ = _digitc[w % radix];
		w /= radix;
	}
	while (w > 0);

	return ptr - buf;
}

static void reverse_string (hak_ooch_t* str, hak_oow_t len)
{
	hak_ooch_t ch;
	hak_ooch_t* start = str;
	hak_ooch_t* end = str + len - 1;

	while (start < end)
	{
		ch = *start;
		*start++ = *end;
		*end-- = ch;
	}
}

hak_oop_t hak_eqints (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	if (HAK_OOP_IS_SMOOI(x) && HAK_OOP_IS_SMOOI(y))
	{
		return (HAK_OOP_TO_SMOOI(x) == HAK_OOP_TO_SMOOI(y))? hak->_true: hak->_false;
	}
	else if (HAK_OOP_IS_SMOOI(x) || HAK_OOP_IS_SMOOI(y))
	{
		return hak->_false;
	}
	else
	{
		if (!hak_isbigint(hak, x) || !hak_isbigint(hak, y)) goto oops_einval;
		return is_equal(hak, x, y)? hak->_true: hak->_false;
	}

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O, %O", x, y);
	return HAK_NULL;
}

hak_oop_t hak_neints (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	if (HAK_OOP_IS_SMOOI(x) && HAK_OOP_IS_SMOOI(y))
	{
		return (HAK_OOP_TO_SMOOI(x) != HAK_OOP_TO_SMOOI(y))? hak->_true: hak->_false;
	}
	else if (HAK_OOP_IS_SMOOI(x) || HAK_OOP_IS_SMOOI(y))
	{
		return hak->_true;
	}
	else
	{
		if (!hak_isbigint(hak, x) || !hak_isbigint(hak, y)) goto oops_einval;
		return !is_equal(hak, x, y)? hak->_true: hak->_false;
	}

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O, %O", x, y);
	return HAK_NULL;
}

hak_oop_t hak_gtints (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	if (HAK_OOP_IS_SMOOI(x) && HAK_OOP_IS_SMOOI(y))
	{
		return (HAK_OOP_TO_SMOOI(x) > HAK_OOP_TO_SMOOI(y))? hak->_true: hak->_false;
	}
	else if (HAK_OOP_IS_SMOOI(x))
	{
		if (!hak_isbigint(hak, y)) goto oops_einval;
		return (HAK_IS_NBIGINT(hak, y))? hak->_true: hak->_false;
	}
	else if (HAK_OOP_IS_SMOOI(y))
	{
		if (!hak_isbigint(hak, x)) goto oops_einval;
		return (HAK_IS_PBIGINT(hak, x))? hak->_true: hak->_false;
	}
	else
	{
		if (!hak_isbigint(hak, x) || !hak_isbigint(hak, y)) goto oops_einval;
		return is_greater(hak, x, y)? hak->_true: hak->_false;
	}

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O, %O", x, y);
	return HAK_NULL;
}

hak_oop_t hak_geints (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	if (HAK_OOP_IS_SMOOI(x) && HAK_OOP_IS_SMOOI(y))
	{
		return (HAK_OOP_TO_SMOOI(x) >= HAK_OOP_TO_SMOOI(y))? hak->_true: hak->_false;
	}
	else if (HAK_OOP_IS_SMOOI(x))
	{
		if (!hak_isbigint(hak, y)) goto oops_einval;
		return (HAK_IS_NBIGINT(hak, y))? hak->_true: hak->_false;
	}
	else if (HAK_OOP_IS_SMOOI(y))
	{
		if (!hak_isbigint(hak, x)) goto oops_einval;
		return (HAK_IS_PBIGINT(hak, x))? hak->_true: hak->_false;
	}
	else
	{
		if (!hak_isbigint(hak, x) || !hak_isbigint(hak, y)) goto oops_einval;
		return (is_greater(hak, x, y) || is_equal(hak, x, y))? hak->_true: hak->_false;
	}

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O, %O", x, y);
	return HAK_NULL;
}

hak_oop_t hak_ltints (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	if (HAK_OOP_IS_SMOOI(x) && HAK_OOP_IS_SMOOI(y))
	{
		return (HAK_OOP_TO_SMOOI(x) < HAK_OOP_TO_SMOOI(y))? hak->_true: hak->_false;
	}
	else if (HAK_OOP_IS_SMOOI(x))
	{
		if (!hak_isbigint(hak, y)) goto oops_einval;
		return (HAK_IS_PBIGINT(hak, y))? hak->_true: hak->_false;
	}
	else if (HAK_OOP_IS_SMOOI(y))
	{
		if (!hak_isbigint(hak, x)) goto oops_einval;
		return (HAK_IS_NBIGINT(hak, x))? hak->_true: hak->_false;
	}
	else
	{
		if (!hak_isbigint(hak, x) || !hak_isbigint(hak, y)) goto oops_einval;
		return is_less(hak, x, y)? hak->_true: hak->_false;
	}

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O, %O", x, y);
	return HAK_NULL;
}

hak_oop_t hak_leints (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	if (HAK_OOP_IS_SMOOI(x) && HAK_OOP_IS_SMOOI(y))
	{
		return (HAK_OOP_TO_SMOOI(x) <= HAK_OOP_TO_SMOOI(y))? hak->_true: hak->_false;
	}
	else if (HAK_OOP_IS_SMOOI(x))
	{
		if (!hak_isbigint(hak, y)) goto oops_einval;
		return (HAK_IS_PBIGINT(hak, y))? hak->_true: hak->_false;
	}
	else if (HAK_OOP_IS_SMOOI(y))
	{
		if (!hak_isbigint(hak, x)) goto oops_einval;
		return (HAK_IS_NBIGINT(hak, x))? hak->_true: hak->_false;
	}
	else
	{
		if (!hak_isbigint(hak, x) || !hak_isbigint(hak, y)) goto oops_einval;
		return (is_less(hak, x, y) || is_equal(hak, x, y))? hak->_true: hak->_false;
	}

oops_einval:
	hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O, %O", x, y);
	return HAK_NULL;
}

hak_oop_t hak_sqrtint (hak_t* hak, hak_oop_t x)
{
	/* TODO: find a faster and more efficient algorithm??? */
	hak_oop_t a, b, m, m2, t;
	int neg;

	if (!hak_isint(hak, x))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O", x);
		return HAK_NULL;
	}

	a = hak->_nil;
	b = hak->_nil;
	m = hak->_nil;
	m2 = hak->_nil;

	hak_pushvolat(hak, &x);
	hak_pushvolat(hak, &a);
	hak_pushvolat(hak, &b);
	hak_pushvolat(hak, &m);
	hak_pushvolat(hak, &m2);

	a = hak_ltints(hak, x, HAK_SMOOI_TO_OOP(0));
	if (HAK_UNLIKELY(!a)) goto oops;
	if (a == hak->_true)
	{
		/* the given number is a negative number.
		 * i will arrange the return value to be negative. */
		x = hak_negateint(hak, x);
		if (HAK_UNLIKELY(!x)) goto oops;
		neg = 1;
	}
	else neg = 0;

	a = HAK_SMOOI_TO_OOP(1);
	b = hak_bitshiftint(hak, x, HAK_SMOOI_TO_OOP(-5));
	if (HAK_UNLIKELY(!b)) goto oops;
	b = hak_addints(hak, b, HAK_SMOOI_TO_OOP(8));
	if (HAK_UNLIKELY(!b)) goto oops;

	while (1)
	{
		t = hak_geints(hak, b, a);
		if (HAK_UNLIKELY(!t)) return HAK_NULL;
		if (t == hak->_false) break;

		m = hak_addints(hak, a, b);
		if (HAK_UNLIKELY(!m)) goto oops;
		m = hak_bitshiftint(hak, m, HAK_SMOOI_TO_OOP(-1));
		if (HAK_UNLIKELY(!m)) goto oops;
		m2 = hak_mulints(hak, m, m);
		if (HAK_UNLIKELY(!m2)) goto oops;
		t = hak_gtints(hak, m2, x);
		if (HAK_UNLIKELY(!t)) return HAK_NULL;
		if (t == hak->_true)
		{
			b = hak_subints(hak, m, HAK_SMOOI_TO_OOP(1));
			if (HAK_UNLIKELY(!b)) goto oops;
		}
		else
		{
			a = hak_addints(hak, m, HAK_SMOOI_TO_OOP(1));
			if (HAK_UNLIKELY(!a)) goto oops;
		}
	}

	hak_popvolats(hak, 5);
	x = hak_subints(hak, a, HAK_SMOOI_TO_OOP(1));
	if (HAK_UNLIKELY(!x)) return HAK_NULL;

	if (neg) x = hak_negateint(hak, x);
	return x;

oops:
	hak_popvolats(hak, 5);
	return HAK_NULL;
}

hak_oop_t hak_absint (hak_t* hak, hak_oop_t x)
{
	if (HAK_OOP_IS_SMOOI(x))
	{
		hak_ooi_t v;
		v = HAK_OOP_TO_SMOOI(x);
		if (v < 0)
		{
			v = -v;
			x = HAK_SMOOI_TO_OOP(v);
		}
	}
	else if (HAK_IS_NBIGINT(hak, x))
	{
		x = _clone_bigint(hak, x, HAK_OBJ_GET_SIZE(x), hak->c_large_positive_integer);
	}
	else if (HAK_IS_PBIGINT(hak, x))
	{
		/* do nothing. return x without change.
		 * [THINK] but do i need to clone a positive bigint? */
	}
	else
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "not integer - %O", x);
		return HAK_NULL;
	}

	return x;
}

static HAK_INLINE hak_liw_t get_last_digit (hak_t* hak, hak_liw_t* x, hak_oow_t* xs, int radix)
{
	/* this function changes the contents of the large integer word array */
	hak_oow_t oxs = *xs;
	hak_liw_t carry = 0;
	hak_oow_t i;
	hak_lidw_t dw;

	HAK_ASSERT(hak, oxs > 0);

	for (i = oxs; i > 0; )
	{
		--i;
		dw = ((hak_lidw_t)carry << HAK_LIW_BITS) + x[i];
		/* TODO: optimize it with ASM - no seperate / and % */
		x[i] = (hak_liw_t)(dw / radix);
		carry = (hak_liw_t)(dw % radix);
	}
	if (/*oxs > 0 &&*/ x[oxs - 1] == 0) *xs = oxs - 1;
	return carry;
}

hak_oop_t hak_inttostr (hak_t* hak, hak_oop_t num, int flagged_radix)
{
	hak_ooi_t v = 0;
	hak_oow_t w;
	hak_oow_t as;
	hak_liw_t* t = HAK_NULL;
	hak_ooch_t* xbuf = HAK_NULL;
	hak_oow_t xlen = 0, reqcapa;

	int radix;
	const char* _digitc;

	radix = flagged_radix & HAK_INTTOSTR_RADIXMASK;
	_digitc =  _digitc_array[!!(flagged_radix & HAK_INTTOSTR_LOWERCASE)];
	HAK_ASSERT(hak, radix >= 2 && radix <= 36);

	if (!hak_isint(hak,num)) goto oops_einval;
	v = integer_to_oow_noseterr(hak, num, &w);

	if (v)
	{
		/* The largest buffer is required for radix 2.
		 * For a binary conversion(radix 2), the number of bits is
		 * the maximum number of digits that can be produced. +1 is
		 * needed for the sign. */

		reqcapa = HAK_OOW_BITS + 1;
		if (hak->inttostr.xbuf.capa < reqcapa)
		{
			xbuf = (hak_ooch_t*)hak_reallocmem(hak, hak->inttostr.xbuf.ptr, reqcapa * HAK_SIZEOF(*xbuf));
			if (!xbuf) return HAK_NULL;
			hak->inttostr.xbuf.capa = reqcapa;
			hak->inttostr.xbuf.ptr = xbuf;
		}
		else
		{
			xbuf = hak->inttostr.xbuf.ptr;
		}

		xlen = oow_to_text(hak, w, flagged_radix, xbuf);
		if (v < 0) xbuf[xlen++] = '-';

		reverse_string (xbuf, xlen);
		if (flagged_radix & HAK_INTTOSTR_NONEWOBJ)
		{
			/* special case. don't create a new object.
			 * the caller can use the data left in hak->inttostr.xbuf */
			hak->inttostr.xbuf.len = xlen;
			return hak->_nil;
		}
		return hak_makestring(hak, xbuf, xlen);
	}

	/* the number can't be represented as a single hak_oow_t value.
	 * mutli-word conversion begins now */
	as = HAK_OBJ_GET_SIZE(num);

	reqcapa = as * HAK_LIW_BITS + 1;
	if (hak->inttostr.xbuf.capa < reqcapa)
	{
		xbuf = (hak_ooch_t*)hak_reallocmem(hak, hak->inttostr.xbuf.ptr, reqcapa * HAK_SIZEOF(*xbuf));
		if (HAK_UNLIKELY(!xbuf)) return HAK_NULL;
		hak->inttostr.xbuf.capa = reqcapa;
		hak->inttostr.xbuf.ptr = xbuf;
	}
	else
	{
		xbuf = hak->inttostr.xbuf.ptr;
	}

	if (hak->inttostr.t.capa < as)
 	{
		t = (hak_liw_t*)hak_reallocmem(hak, hak->inttostr.t.ptr, reqcapa * HAK_SIZEOF(*t));
		if (HAK_UNLIKELY(!t)) return HAK_NULL;
		hak->inttostr.t.capa = as;
		hak->inttostr.t.ptr = t;
	}
	else
	{
		t = hak->inttostr.t.ptr;
	}

	HAK_MEMCPY(t, ((hak_oop_liword_t)num)->slot, HAK_SIZEOF(*t) * as);

	do
	{
		hak_liw_t dv = get_last_digit(hak, t, &as, radix);
		xbuf[xlen++] = _digitc[dv];
	}
	while (as > 0);

	if (HAK_IS_NBIGINT(hak, num)) xbuf[xlen++] = '-';
	reverse_string (xbuf, xlen);
	if (flagged_radix & HAK_INTTOSTR_NONEWOBJ)
	{
		/* special case. don't create a new object.
		 * the caller can use the data left in hak->inttostr.xbuf */
		hak->inttostr.xbuf.len = xlen;
		return hak->_nil;
	}

	return hak_makestring(hak, xbuf, xlen);

oops_einval:
	hak_seterrnum(hak, HAK_EINVAL);
	return HAK_NULL;
}
