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

static hak_ooi_t equalize_scale (hak_t* hak, hak_oop_t* x, hak_oop_t* y)
{
	hak_ooi_t xs, ys;
	hak_oop_t nv;
	hak_oop_t xv, yv;

	/* this function assumes that x and y are protected by the caller */

	xs = 0;
	xv = *x;
	if (HAK_IS_FPDEC(hak, xv))
	{
		xs = HAK_OOP_TO_SMOOI(((hak_oop_fpdec_t)xv)->scale);
		xv = ((hak_oop_fpdec_t)xv)->value;
	}
	else if (!hak_isint(hak, xv))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not numeric - %O", xv);
		return -1;
	}

	ys = 0;
	yv = *y;
	if (HAK_IS_FPDEC(hak, yv))
	{
		ys = HAK_OOP_TO_SMOOI(((hak_oop_fpdec_t)yv)->scale);
		yv = ((hak_oop_fpdec_t)yv)->value;
	}
	else if (!hak_isint(hak, yv))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not numeric - %O", yv);
		return -1;
	}

	if (xs < ys)
	{
		nv = xv;
		while (xs < ys)
		{
			/* TODO: optmize this. less multiplications */
			nv = hak_mulints(hak, nv, HAK_SMOOI_TO_OOP(10));
			if (!nv) return -1;
			xs++;
		}

		nv = hak_makefpdec(hak, nv, xs);
		if (!nv) return -1;

		*x = nv;
	}
	else if (xs > ys)
	{
		nv = yv;
		while (ys < xs)
		{
			nv = hak_mulints(hak, nv, HAK_SMOOI_TO_OOP(10));
			if (!nv) return -1;
			ys++;
		}

		nv = hak_makefpdec(hak, nv, ys);
		if (!nv) return -1;

		*y = nv;
	}

	return xs;
}

hak_oop_t hak_truncfpdecval (hak_t* hak, hak_oop_t iv, hak_ooi_t cs, hak_ooi_t ns)
{
	/* this function truncates an existing fixed-point decimal.
	 * it doesn't create a new object */

	if (cs > ns)
	{
		do
		{
			/* TODO: optimizatino... less divisions */
			iv = hak_divints(hak, iv, HAK_SMOOI_TO_OOP(10), 0, HAK_NULL);
			if (!iv) return HAK_NULL;
			cs--;
		}
		while (cs > ns);
	}

	return iv;
}

hak_oop_t hak_addnums (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	if (!HAK_IS_FPDEC(hak, x) && !HAK_IS_FPDEC(hak, y))
	{
		/* both are probably integers */
		return hak_addints(hak, x, y);
	}
	else
	{
		hak_oop_t v;
		hak_ooi_t scale;

		hak_pushvolat(hak, &x);
		hak_pushvolat(hak, &y);

		scale = equalize_scale(hak, &x, &y);
		if (scale <= -1)
		{
			hak_popvolats(hak, 2);
			return HAK_NULL;
		}
		v = hak_addints(hak, ((hak_oop_fpdec_t)x)->value, ((hak_oop_fpdec_t)y)->value);
		hak_popvolats(hak, 2);
		if (!v) return HAK_NULL;

		return hak_makefpdec(hak, v, scale);
	}
}

hak_oop_t hak_subnums (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	if (!HAK_IS_FPDEC(hak, x) && !HAK_IS_FPDEC(hak, y))
	{
		/* both are probably integers */
		return hak_subints(hak, x, y);
	}
	else
	{
		hak_oop_t v;
		hak_ooi_t scale;

		hak_pushvolat(hak, &x);
		hak_pushvolat(hak, &y);

		scale = equalize_scale(hak, &x, &y);
		if (scale <= -1)
		{
			hak_popvolats(hak, 2);
			return HAK_NULL;
		}
		v = hak_subints(hak, ((hak_oop_fpdec_t)x)->value, ((hak_oop_fpdec_t)y)->value);
		hak_popvolats(hak, 2);
		if (!v) return HAK_NULL;

		return hak_makefpdec(hak, v, scale);
	}
}

static hak_oop_t mul_nums (hak_t* hak, hak_oop_t x, hak_oop_t y, int mult)
{
	hak_ooi_t xs, ys, cs, ns;
	hak_oop_t nv;
	hak_oop_t xv, yv;

	xs = 0;
	xv = x;
	if (HAK_IS_FPDEC(hak, xv))
	{
		xs = HAK_OOP_TO_SMOOI(((hak_oop_fpdec_t)xv)->scale);
		xv = ((hak_oop_fpdec_t)xv)->value;
	}
	else if (!hak_isint(hak, xv))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not numeric - %O", xv);
		return HAK_NULL;
	}

	ys = 0;
	yv = y;
	if (HAK_IS_FPDEC(hak, y))
	{
		ys = HAK_OOP_TO_SMOOI(((hak_oop_fpdec_t)yv)->scale);
		yv = ((hak_oop_fpdec_t)yv)->value;
	}
	else if (!hak_isint(hak, yv))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not numeric - %O", yv);
		return HAK_NULL;
	}

	nv = hak_mulints(hak, xv, yv);
	if (!nv) return HAK_NULL;

	cs = xs + ys;
	if (cs <= 0) return nv; /* the result must be an integer */

	ns = (mult || xs > ys)? xs: ys;

	/* cs may be larger than HAK_SMOOI_MAX. but ns is guaranteed to be
	 * equal to or less than HAK_SMOOI_MAX */
	HAK_ASSERT(hak, ns <= HAK_SMOOI_MAX);

	nv = hak_truncfpdecval(hak, nv, cs, ns);
	if (!nv) return HAK_NULL;

	return (ns <= 0)? nv: hak_makefpdec(hak, nv, ns);
}

hak_oop_t hak_mulnums (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	/* (* 1.00 12.123) => 12.123 */
	return mul_nums(hak, x, y, 0);
}

hak_oop_t hak_mltnums (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	/* (mlt 1.00 12.123) =>  12.12 */
	return mul_nums(hak, x, y, 1);
}

hak_oop_t hak_divnums (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	hak_ooi_t xs, ys, i;
	hak_oop_t nv;
	hak_oop_t xv, yv;

	xs = 0;
	xv = x;
	if (HAK_IS_FPDEC(hak, xv))
	{
		xs = HAK_OOP_TO_SMOOI(((hak_oop_fpdec_t)xv)->scale);
		xv = ((hak_oop_fpdec_t)xv)->value;
	}
	else if (!hak_isint(hak, xv))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not numeric - %O", xv);
		return HAK_NULL;
	}

	ys = 0;
	yv = y;
	if (HAK_IS_FPDEC(hak, y))
	{
		ys = HAK_OOP_TO_SMOOI(((hak_oop_fpdec_t)yv)->scale);
		yv = ((hak_oop_fpdec_t)yv)->value;
	}
	else if (!hak_isint(hak, yv))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not numeric - %O", yv);
		return HAK_NULL;
	}

	nv = xv;

	hak_pushvolat(hak, &yv);
	for (i = 0; i < ys; i++)
	{
		nv = hak_mulints(hak, nv, HAK_SMOOI_TO_OOP(10));
		if (!nv)
		{
			hak_popvolat(hak);
			return HAK_NULL;
		}
	}

	nv = hak_divints(hak, nv, yv, 0, HAK_NULL);
	hak_popvolat(hak);
	if (!nv) return HAK_NULL;

	return hak_makefpdec(hak, nv, xs);
}

static hak_oop_t comp_nums (hak_t* hak, hak_oop_t x, hak_oop_t y, hak_oop_t (*comper) (hak_t*, hak_oop_t, hak_oop_t))
{
	if (!HAK_IS_FPDEC(hak, x) && !HAK_IS_FPDEC(hak, y))
	{
		/* both are probably integers */
		return comper(hak, x, y);
	}
	else
	{
		hak_oop_t v;
		hak_ooi_t scale;

		hak_pushvolat(hak, &x);
		hak_pushvolat(hak, &y);

		scale = equalize_scale(hak, &x, &y);
		if (scale <= -1)
		{
			hak_popvolats(hak, 2);
			return HAK_NULL;
		}
		v = comper(hak, ((hak_oop_fpdec_t)x)->value, ((hak_oop_fpdec_t)y)->value);
		hak_popvolats(hak, 2);
		return v;
	}
}


hak_oop_t hak_gtnums (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	return comp_nums(hak, x, y, hak_gtints);
}
hak_oop_t hak_genums (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	return comp_nums(hak, x, y, hak_geints);
}


hak_oop_t hak_ltnums (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	return comp_nums(hak, x, y, hak_ltints);
}
hak_oop_t hak_lenums (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	return comp_nums(hak, x, y, hak_leints);
}

hak_oop_t hak_eqnums (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	return comp_nums(hak, x, y, hak_eqints);
}
hak_oop_t hak_nenums (hak_t* hak, hak_oop_t x, hak_oop_t y)
{
	return comp_nums(hak, x, y, hak_neints);
}

hak_oop_t hak_sqrtnum (hak_t* hak, hak_oop_t x)
{
	if (!HAK_IS_FPDEC(hak, x))
	{
		return hak_sqrtint(hak, x);
	}
	else
	{
		hak_oop_t v;
		hak_ooi_t i, scale;

		scale = HAK_OOP_TO_SMOOI(((hak_oop_fpdec_t)x)->scale);

		v = ((hak_oop_fpdec_t)x)->value;
		for (i = 0; i < scale ; i++)
		{
			v = hak_mulints(hak, v, HAK_SMOOI_TO_OOP(10));
			if (!v)
			{
				hak_popvolat(hak);
				return HAK_NULL;
			}
		}

		v = hak_sqrtint(hak, v);
		if (!v) return HAK_NULL;

		return hak_makefpdec(hak, v, scale);
	}
}

hak_oop_t hak_absnum (hak_t* hak, hak_oop_t x)
{
	if (!HAK_IS_FPDEC(hak, x))
	{
		return hak_absint(hak, x);
	}
	else
	{
		hak_oop_t v;
		hak_ooi_t scale;

		scale = HAK_OOP_TO_SMOOI(((hak_oop_fpdec_t)x)->scale);
		v = ((hak_oop_fpdec_t)x)->value;

		v = hak_absint(hak, v);
		if (!v) return HAK_NULL;

		return hak_makefpdec(hak, v, scale);
	}
}
