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

/*
 * This file contains a formatted output routine derived from kvprintf()
 * of FreeBSD. It has been heavily modified and bug-fixed.
 */

/*
 * Copyright (c) 1986, 1988, 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */


#include "hak-prv.h"


#if defined(HAK_ENABLE_FLTFMT)

#include <stdio.h> /* for snrintf(). used for floating-point number formatting */
#if defined(_MSC_VER) || (defined(__BORLANDC__) && (__BORLANDC__ > 0x520)) || (defined(__WATCOMC__) && (__WATCOMC__ < 1200))
#	define snprintf _snprintf
#	if !defined(HAVE_SNPRINTF)
#		define HAVE_SNPRINTF
#	endif
#	if defined(__OS2__) && defined(__BORLANDC__)
#		undef HAVE_SNPRINTF
#	endif
#endif

#if defined(HAVE_QUADMATH_H)
#	include <quadmath.h> /* for quadmath_snprintf() */
#elif defined(HAVE_QUADMATH_SNPRINTF)
extern int quadmath_snprintf (const char *str, size_t size, const char *format, ...);
#endif

#endif

/* Max number conversion buffer length:
 * hak_intmax_t in base 2, plus NUL byte. */
#define MAXNBUF (HAK_SIZEOF(hak_intmax_t) * HAK_BITS_PER_BYTE + 1)

enum fmt_spec_t
{
	/* integer */
	LF_C = (1 << 0),
	LF_H = (1 << 1),
	LF_J = (1 << 2),
	LF_L = (1 << 3),
	LF_Q = (1 << 4),
	LF_T = (1 << 5),
	LF_Z = (1 << 6),

	/* long double */
	LF_LD = (1 << 7),
	/* __float128 */
	LF_QD = (1 << 8)
};

static struct
{
	hak_uint8_t flag; /* for single occurrence */
	hak_uint8_t dflag; /* for double occurrence */
} lm_tab[26] =
{
	{ 0,    0 }, /* a */
	{ 0,    0 }, /* b */
	{ 0,    0 }, /* c */
	{ 0,    0 }, /* d */
	{ 0,    0 }, /* e */
	{ 0,    0 }, /* f */
	{ 0,    0 }, /* g */
	{ LF_H, LF_C }, /* h */
	{ 0,    0 }, /* i */
	{ LF_J, 0 }, /* j */
	{ 0,    0 }, /* k */
	{ LF_L, LF_Q }, /* l */
	{ 0,    0 }, /* m */
	{ 0,    0 }, /* n */
	{ 0,    0 }, /* o */
	{ 0,    0 }, /* p */
	{ LF_Q, 0 }, /* q */
	{ 0,    0 }, /* r */
	{ 0,    0 }, /* s */
	{ LF_T, 0 }, /* t */
	{ 0,    0 }, /* u */
	{ 0,    0 }, /* v */
	{ 0,    0 }, /* w */
	{ 0,    0 }, /* z */
	{ 0,    0 }, /* y */
	{ LF_Z, 0 }, /* z */
};


enum
{
	FLAGC_DOT       = (1 << 0),
	FLAGC_SPACE     = (1 << 1),
	FLAGC_SHARP     = (1 << 2),
	FLAGC_SIGN      = (1 << 3),
	FLAGC_LEFTADJ   = (1 << 4),
	FLAGC_ZEROPAD   = (1 << 5),
	FLAGC_WIDTH     = (1 << 6),
	FLAGC_PRECISION = (1 << 7),
	FLAGC_STAR1     = (1 << 8),
	FLAGC_STAR2     = (1 << 9),
	FLAGC_LENMOD    = (1 << 10) /* length modifier */
};

static const hak_bch_t hex2ascii_lower[] =
{
	'0','1','2','3','4','5','6','7','8','9',
	'a','b','c','d','e','f','g','h','i','j','k','l','m',
	'n','o','p','q','r','s','t','u','v','w','x','y','z'
};

static const hak_bch_t hex2ascii_upper[] =
{
	'0','1','2','3','4','5','6','7','8','9',
	'A','B','C','D','E','F','G','H','I','J','K','L','M',
	'N','O','P','Q','R','S','T','U','V','W','X','H','Z'
};

static hak_uch_t uch_nullstr[] = { '(','n','u','l','l', ')','\0' };
static hak_bch_t bch_nullstr[] = { '(','n','u','l','l', ')','\0' };

/* ------------------------------------------------------------------------- */

/*define static int fmt_uintmax_to_bcstr(...)*/
#undef char_t
#undef fmt_uintmax
#define char_t hak_bch_t
#define fmt_uintmax fmt_uintmax_to_bcstr
#include "fmt-imp.h"

/*define static int fmt_uintmax_to_ucstr(...)*/
#undef char_t
#undef fmt_uintmax
#define char_t hak_uch_t
#define fmt_uintmax fmt_uintmax_to_ucstr
#include "fmt-imp.h"

int hak_fmt_intmax_to_bcstr (
	hak_bch_t* buf, int size,
	hak_intmax_t value, int base_and_flags, int prec,
	hak_bch_t fillchar, const hak_bch_t* prefix)
{
	hak_bch_t signchar;
	hak_uintmax_t absvalue;

	if (value < 0)
	{
		signchar = '-';
		absvalue = -value;
	}
	else if (base_and_flags & HAK_FMT_INTMAX_TO_BCSTR_PLUSSIGN)
	{
		signchar = '+';
		absvalue = value;
	}
	else if (base_and_flags & HAK_FMT_INTMAX_TO_BCSTR_EMPTYSIGN)
	{
		signchar = ' ';
		absvalue = value;
	}
	else
	{
		signchar = '\0';
		absvalue = value;
	}

	return fmt_uintmax_to_bcstr(buf, size, absvalue, base_and_flags, prec, fillchar, signchar, prefix);
}

int hak_fmt_uintmax_to_bcstr (
	hak_bch_t* buf, int size,
	hak_uintmax_t value, int base_and_flags, int prec,
	hak_bch_t fillchar, const hak_bch_t* prefix)
{
	hak_bch_t signchar;

	/* determine if a sign character is needed */
	if (base_and_flags & HAK_FMT_INTMAX_TO_BCSTR_PLUSSIGN)
	{
		signchar = '+';
	}
	else if (base_and_flags & HAK_FMT_INTMAX_TO_BCSTR_EMPTYSIGN)
	{
		signchar = ' ';
	}
	else
	{
		signchar = '\0';
	}

	return fmt_uintmax_to_bcstr(buf, size, value, base_and_flags, prec, fillchar, signchar, prefix);
}

/* ==================== wide-char ===================================== */

int hak_fmt_intmax_to_ucstr (
	hak_uch_t* buf, int size,
	hak_intmax_t value, int base_and_flags, int prec,
	hak_uch_t fillchar, const hak_uch_t* prefix)
{
	hak_uch_t signchar;
	hak_uintmax_t absvalue;

	if (value < 0)
	{
		signchar = '-';
		absvalue = -value;
	}
	else if (base_and_flags & HAK_FMT_INTMAX_TO_UCSTR_PLUSSIGN)
	{
		signchar = '+';
		absvalue = value;
	}
	else if (base_and_flags & HAK_FMT_INTMAX_TO_UCSTR_EMPTYSIGN)
	{
		signchar = ' ';
		absvalue = value;
	}
	else
	{
		signchar = '\0';
		absvalue = value;
	}

	return fmt_uintmax_to_ucstr(buf, size, absvalue, base_and_flags, prec, fillchar, signchar, prefix);
}

int hak_fmt_uintmax_to_ucstr (
	hak_uch_t* buf, int size,
	hak_uintmax_t value, int base_and_flags, int prec,
	hak_uch_t fillchar, const hak_uch_t* prefix)
{
	hak_uch_t signchar;

	/* determine if a sign character is needed */
	if (base_and_flags & HAK_FMT_INTMAX_TO_UCSTR_PLUSSIGN)
	{
		signchar = '+';
	}
	else if (base_and_flags & HAK_FMT_INTMAX_TO_UCSTR_EMPTYSIGN)
	{
		signchar = ' ';
	}
	else
	{
		signchar = '\0';
	}

	return fmt_uintmax_to_ucstr(buf, size, value, base_and_flags, prec, fillchar, signchar, prefix);
}

/* ------------------------------------------------------------------------- */
/*
 * Put a NUL-terminated ASCII number (base <= 36) in a buffer in reverse
 * order; return an optional length and a pointer to the last character
 * written in the buffer (i.e., the first character of the string).
 * The buffer pointed to by `nbuf' must have length >= MAXNBUF.
 */

static hak_bch_t* sprintn_lower (hak_bch_t* nbuf, hak_uintmax_t num, int base, hak_ooi_t* lenp)
{
	hak_bch_t* p;

	p = nbuf;
	*p = '\0';
	do { *++p = hex2ascii_lower[num % base]; } while (num /= base);

	if (lenp) *lenp = p - nbuf;
	return p; /* returns the end */
}

static hak_bch_t* sprintn_upper (hak_bch_t* nbuf, hak_uintmax_t num, int base, hak_ooi_t* lenp)
{
	hak_bch_t* p;

	p = nbuf;
	*p = '\0';
	do { *++p = hex2ascii_upper[num % base]; } while (num /= base);

	if (lenp) *lenp = p - nbuf;
	return p; /* returns the end */
}

/* ------------------------------------------------------------------------- */
#define PUT_BCH(hak,fmtout,c,n) do { \
	hak_oow_t _yy; \
	hak_bch_t _cc = c; \
	for (_yy = 0; _yy < n; _yy++) \
	{ \
		int _xx; \
		if ((_xx = fmtout->putbchars(hak, fmtout, &_cc, 1)) <= -1) goto oops; \
		if (_xx == 0) goto done; \
		fmtout->count++; \
	} \
} while (0)

#define PUT_BCS(hak,fmtout,ptr,len) do { \
	if (len > 0) { \
		int _xx; \
		if ((_xx = fmtout->putbchars(hak, fmtout, ptr, len)) <= -1) goto oops; \
		if (_xx == 0) goto done; \
		fmtout->count += len; \
	} \
} while (0)

#define PUT_UCH(hak,fmtout,c,n) do { \
	hak_oow_t _yy; \
	hak_uch_t _cc = c; \
	for (_yy = 0; _yy < n; _yy++) \
	{ \
		int _xx; \
		if ((_xx = fmtout->putuchars(hak, fmtout, &_cc, 1)) <= -1) goto oops; \
		if (_xx == 0) goto done; \
		fmtout->count++; \
	} \
} while (0)

#define PUT_UCS(hak,fmtout,ptr,len) do { \
	if (len > 0) { \
		int _xx; \
		if ((_xx = fmtout->putuchars(hak, fmtout, ptr, len)) <= -1) goto oops; \
		if (_xx == 0) goto done; \
		fmtout->count += len; \
	} \
} while (0)


#if defined(HAK_OOCH_IS_BCH)
#	define PUT_OOCH(hak,fmtout,c,n) PUT_BCH(hak,fmtout,c,n)
#	define PUT_OOCS(hak,fmtout,ptr,len) PUT_BCS(hak,fmtout,ptr,len)
#else
#	define PUT_OOCH(hak,fmtout,c,n) PUT_UCH(hak,fmtout,c,n)
#	define PUT_OOCS(hak,fmtout,ptr,len) PUT_UCS(hak,fmtout,ptr,len)
#endif

#define BYTE_PRINTABLE(x) ((x >= 'a' && x <= 'z') || (x >= 'A' &&  x <= 'Z') || (x >= '0' && x <= '9') || (x == ' '))


#define PUT_BYTE_IN_HEX(hak,fmtout,byte,extra_flags) do { \
	hak_bch_t __xbuf[3]; \
	hak_byte_to_bcstr ((byte), __xbuf, HAK_COUNTOF(__xbuf), (16 | (extra_flags)), '0'); \
	PUT_BCH(hak, fmtout, __xbuf[0], 1); \
	PUT_BCH(hak, fmtout, __xbuf[1], 1); \
} while (0)

/* ------------------------------------------------------------------------- */
static int fmt_outv (hak_t* hak, hak_fmtout_t* fmtout, va_list ap)
{
	const hak_uint8_t* fmtptr, * percent;
	int fmtchsz;

	hak_uch_t uch;
	hak_bch_t bch;
	hak_ooch_t padc;

	int n, base, neg, sign;
	hak_ooi_t tmp, width, precision;
	int lm_flag, lm_dflag, flagc, numlen;

	hak_uintmax_t num = 0;
	hak_bch_t nbuf[MAXNBUF];
	const hak_bch_t* nbufp;
	int stop = 0;

#if defined(HAK_ENABLE_FLTFMT)
	struct
	{
		struct
		{
			hak_bch_t  sbuf[32];
			hak_bch_t* ptr;
			hak_oow_t  capa;
		} fmt;
		struct
		{
			hak_bch_t  sbuf[64];
			hak_bch_t* ptr;
			hak_oow_t  capa;
		} out;
	} fb; /* some buffers for handling float-point number formatting */
#endif

	hak_bch_t* (*sprintn) (hak_bch_t* nbuf, hak_uintmax_t num, int base, hak_ooi_t* lenp);

	fmtptr = (const hak_uint8_t*)fmtout->fmt_str;
	switch (fmtout->fmt_type)
	{
		case HAK_FMTOUT_FMT_TYPE_BCH:
			fmtchsz = HAK_SIZEOF_BCH_T;
			break;
		case HAK_FMTOUT_FMT_TYPE_UCH:
			fmtchsz = HAK_SIZEOF_UCH_T;
			break;
	}

	/* this is an internal function. it doesn't reset count to 0 */
	/* fmtout->count = 0; */
#if defined(HAK_ENABLE_FLTFMT)
	fb.fmt.ptr = fb.fmt.sbuf;
	fb.fmt.capa = HAK_COUNTOF(fb.fmt.sbuf) - 1;
	fb.out.ptr = fb.out.sbuf;
	fb.out.capa = HAK_COUNTOF(fb.out.sbuf) - 1;
#endif

	while (1)
	{
	#if defined(HAVE_LABELS_AS_VALUES)
		static void* before_percent_tab[] = { &&before_percent_bch, &&before_percent_uch };
		goto *before_percent_tab[fmtout->fmt_type];
	#else
		switch (fmtout->fmt_type)
		{
			case HAK_FMTOUT_FMT_TYPE_BCH:
				goto before_percent_bch;
			case HAK_FMTOUT_FMT_TYPE_UCH:
				goto before_percent_uch;
		}
	#endif

	before_percent_bch:
		{
			const hak_bch_t* start, * end;
			start = end = (const hak_bch_t*)fmtptr;
			while ((bch = *end++) != '%' || stop)
			{
				if (bch == '\0')
				{
					PUT_BCS(hak, fmtout, start, end - start - 1);
					goto done;
				}
			}
			PUT_BCS(hak, fmtout, start, end - start - 1);
			fmtptr = (const hak_uint8_t*)end;
			percent = (const hak_uint8_t*)(end - 1);
		}
		goto handle_percent;

	before_percent_uch:
		{
			const hak_uch_t* start, * end;
			start = end = (const hak_uch_t*)fmtptr;
			while ((uch = *end++) != '%' || stop)
			{
				if (uch == '\0')
				{
					PUT_UCS(hak, fmtout, start, end - start - 1);
					goto done;
				}
			}
			PUT_UCS(hak, fmtout, start, end - start - 1);
			fmtptr = (const hak_uint8_t*)end;
			percent = (const hak_uint8_t*)(end - 1);
		}
		goto handle_percent;

	handle_percent:
		padc = ' ';
		width = 0; precision = 0; neg = 0; sign = 0;
		lm_flag = 0; lm_dflag = 0; flagc = 0;
		sprintn = sprintn_lower;

	reswitch:
		switch (fmtout->fmt_type)
		{
			case HAK_FMTOUT_FMT_TYPE_BCH:
				uch = *(const hak_bch_t*)fmtptr;
				break;
			case HAK_FMTOUT_FMT_TYPE_UCH:
				uch = *(const hak_uch_t*)fmtptr;
				break;
		}
		fmtptr += fmtchsz;

		switch (uch)
		{
		case '%': /* %% */
			bch = uch;
			goto print_lowercase_c;

		/* flag characters */
		case '.':
			if (flagc & FLAGC_DOT) goto invalid_format;
			flagc |= FLAGC_DOT;
			goto reswitch;

		case '#':
			if (flagc & (FLAGC_WIDTH | FLAGC_DOT | FLAGC_LENMOD)) goto invalid_format;
			flagc |= FLAGC_SHARP;
			goto reswitch;

		case ' ':
			if (flagc & (FLAGC_WIDTH | FLAGC_DOT | FLAGC_LENMOD)) goto invalid_format;
			flagc |= FLAGC_SPACE;
			goto reswitch;

		case '+': /* place sign for signed conversion */
			if (flagc & (FLAGC_WIDTH | FLAGC_DOT | FLAGC_LENMOD)) goto invalid_format;
			flagc |= FLAGC_SIGN;
			goto reswitch;

		case '-': /* left adjusted */
			if (flagc & (FLAGC_WIDTH | FLAGC_DOT | FLAGC_LENMOD)) goto invalid_format;
			if (flagc & FLAGC_DOT)
			{
				goto invalid_format;
			}
			else
			{
				flagc |= FLAGC_LEFTADJ;
				if (flagc & FLAGC_ZEROPAD)
				{
					padc = ' ';
					flagc &= ~FLAGC_ZEROPAD;
				}
			}

			goto reswitch;

		case '*': /* take the length from the parameter */
			if (flagc & FLAGC_DOT)
			{
				if (flagc & (FLAGC_STAR2 | FLAGC_PRECISION)) goto invalid_format;
				flagc |= FLAGC_STAR2;

				precision = va_arg(ap, hak_ooi_t); /* this deviates from the standard printf that accepts 'int' */
				if (precision < 0)
				{
					/* if precision is less than 0,
					 * treat it as if no .precision is specified */
					flagc &= ~FLAGC_DOT;
					precision = 0;
				}
			}
			else
			{
				if (flagc & (FLAGC_STAR1 | FLAGC_WIDTH)) goto invalid_format;
				flagc |= FLAGC_STAR1;

				width = va_arg(ap, hak_ooi_t); /* it deviates from the standard printf that accepts 'int' */
				if (width < 0)
				{
					/*
					if (flagc & FLAGC_LEFTADJ)
						flagc  &= ~FLAGC_LEFTADJ;
					else
					*/
						flagc |= FLAGC_LEFTADJ;
					width = -width;
				}
			}
			goto reswitch;

		case '0': /* zero pad */
			if (flagc & FLAGC_LENMOD) goto invalid_format;
			if (!(flagc & (FLAGC_DOT | FLAGC_LEFTADJ)))
			{
				padc = '0';
				flagc |= FLAGC_ZEROPAD;
				goto reswitch;
			}
		/* end of flags characters */

		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		{
			if (flagc & FLAGC_LENMOD) goto invalid_format;
			for (n = 0;; fmtptr += fmtchsz)
			{
				n = n * 10 + uch - '0';
				switch (fmtout->fmt_type)
				{
					case HAK_FMTOUT_FMT_TYPE_BCH:
						uch = *(const hak_bch_t*)fmtptr;
						break;
					case HAK_FMTOUT_FMT_TYPE_UCH:
						uch = *(const hak_uch_t*)fmtptr;
						break;
				}
				if (uch < '0' || uch > '9') break;
			}
			if (flagc & FLAGC_DOT)
			{
				if (flagc & FLAGC_STAR2) goto invalid_format;
				precision = n;
				flagc |= FLAGC_PRECISION;
			}
			else
			{
				if (flagc & FLAGC_STAR1) goto invalid_format;
				width = n;
				flagc |= FLAGC_WIDTH;
			}
			goto reswitch;
		}

		/* length modifiers */
		case 'h': /* short int */
		case 'l': /* long int */
		case 'q': /* long long int */
		case 'j': /* hak_intmax_t/hak_uintmax_t */
		case 'z': /* hak_ooi_t/hak_oow_t */
		case 't': /* ptrdiff_t */
			if (lm_flag & (LF_LD | LF_QD)) goto invalid_format;

			flagc |= FLAGC_LENMOD;
			if (lm_dflag)
			{
				/* error */
				goto invalid_format;
			}
			else if (lm_flag)
			{
				if (lm_tab[uch - 'a'].dflag && lm_flag == lm_tab[uch - 'a'].flag)
				{
					lm_flag &= ~lm_tab[uch - 'a'].flag;
					lm_flag |= lm_tab[uch - 'a'].dflag;
					lm_dflag |= lm_flag;
					goto reswitch;
				}
				else
				{
					/* error */
					goto invalid_format;
				}
			}
			else
			{
				lm_flag |= lm_tab[uch - 'a'].flag;
				goto reswitch;
			}
			break;

		case 'L': /* long double */
			if (flagc & FLAGC_LENMOD)
			{
				/* conflict with other length modifier */
				goto invalid_format;
			}
			flagc |= FLAGC_LENMOD;
			lm_flag |= LF_LD;
			goto reswitch;

		case 'Q': /* __float128 */
			if (flagc & FLAGC_LENMOD)
			{
				/* conflict with other length modifier */
				goto invalid_format;
			}
			flagc |= FLAGC_LENMOD;
			lm_flag |= LF_QD;
			goto reswitch;
		/* end of length modifiers */

		case 'n': /* number of characters printed so far */
			if (lm_flag & LF_J) /* j */
				*(va_arg(ap, hak_intmax_t*)) = fmtout->count;
			else if (lm_flag & LF_Z) /* z */
				*(va_arg(ap, hak_ooi_t*)) = fmtout->count;
		#if (HAK_SIZEOF_LONG_LONG > 0)
			else if (lm_flag & LF_Q) /* ll */
				*(va_arg(ap, long long int*)) = fmtout->count;
		#endif
			else if (lm_flag & LF_L) /* l */
				*(va_arg(ap, long int*)) = fmtout->count;
			else if (lm_flag & LF_H) /* h */
				*(va_arg(ap, short int*)) = fmtout->count;
			else if (lm_flag & LF_C) /* hh */
				*(va_arg(ap, char*)) = fmtout->count;
			else if (flagc & FLAGC_LENMOD)
				goto invalid_format;
			else
				*(va_arg(ap, int*)) = fmtout->count;
			break;

		/* signed integer conversions */
		case 'd':
		case 'i': /* signed conversion */
			base = 10;
			sign = 1;
			goto handle_sign;
		/* end of signed integer conversions */

		/* unsigned integer conversions */
		case 'o':
			base = 8;
			goto handle_nosign;
		case 'u':
			base = 10;
			goto handle_nosign;
		case 'X':
			sprintn = sprintn_upper;
		case 'x':
			base = 16;
			goto handle_nosign;
		case 'b':
			base = 2;
			goto handle_nosign;
		/* end of unsigned integer conversions */

		case 'p': /* pointer */
			base = 16;

			if (width == 0) flagc |= FLAGC_SHARP;
			else flagc &= ~FLAGC_SHARP;

			num = (hak_uintptr_t)va_arg(ap, void*);
			goto number;

		case 'c':
		{
			/* zeropad must not take effect for 'c' */
			if (flagc & FLAGC_ZEROPAD) padc = ' ';
			if (lm_flag & LF_L) goto uppercase_c;
		#if defined(HAK_OOCH_IS_UCH)
			if (lm_flag & LF_J) goto uppercase_c;
		#endif
		lowercase_c:
			bch = HAK_SIZEOF(hak_bch_t) < HAK_SIZEOF(int)? va_arg(ap, int): va_arg(ap, hak_bch_t);

		print_lowercase_c:
			/* precision 0 doesn't kill the letter */
			width--;
			if (!(flagc & FLAGC_LEFTADJ) && width > 0) PUT_BCH(hak, fmtout, padc, width);
			PUT_BCH(hak, fmtout, bch, 1);
			if ((flagc & FLAGC_LEFTADJ) && width > 0) PUT_BCH(hak, fmtout, padc, width);
			break;
		}

		case 'C':
		{
			/* zeropad must not take effect for 'C' */
			if (flagc & FLAGC_ZEROPAD) padc = ' ';
			if (lm_flag & LF_H) goto lowercase_c;
		#if defined(HAK_OOCH_IS_BCH)
			if (lm_flag & LF_J) goto lowercase_c;
		#endif
		uppercase_c:
			uch = HAK_SIZEOF(hak_uch_t) < HAK_SIZEOF(int)? va_arg(ap, int): va_arg(ap, hak_uch_t);

			/* precision 0 doesn't kill the letter */
			width--;
			if (!(flagc & FLAGC_LEFTADJ) && width > 0) PUT_UCH(hak, fmtout, padc, width);
			PUT_UCH(hak, fmtout, uch, 1);
			if ((flagc & FLAGC_LEFTADJ) && width > 0) PUT_UCH(hak, fmtout, padc, width);
			break;
		}

		case 's':
		{
			const hak_bch_t* bsp;

			/* zeropad must not take effect for 'S' */
			if (flagc & FLAGC_ZEROPAD) padc = ' ';
			if (lm_flag & LF_L) goto uppercase_s;
		#if defined(HAK_OOCH_IS_UCH)
			if (lm_flag & LF_J) goto uppercase_s;
		#endif
		lowercase_s:
			bsp = va_arg(ap, hak_bch_t*);
			if (!bsp) bsp = bch_nullstr;

			n = 0;
			if (flagc & FLAGC_DOT)
			{
				while (n < precision && bsp[n]) n++;
			}
			else
			{
				while (bsp[n]) n++;
			}

			width -= n;

			if (!(flagc & FLAGC_LEFTADJ) && width > 0) PUT_BCH(hak, fmtout, padc, width);
			PUT_BCS(hak, fmtout, bsp, n);
			if ((flagc & FLAGC_LEFTADJ) && width > 0) PUT_BCH(hak, fmtout, padc, width);
			break;
		}

		case 'S':
		{
			const hak_uch_t* usp;

			/* zeropad must not take effect for 's' */
			if (flagc & FLAGC_ZEROPAD) padc = ' ';
			if (lm_flag & LF_H) goto lowercase_s;
		#if defined(HAK_OOCH_IS_BCH)
			if (lm_flag & LF_J) goto lowercase_s;
		#endif
		uppercase_s:
			usp = va_arg(ap, hak_uch_t*);
			if (!usp) usp = uch_nullstr;

			n = 0;
			if (flagc & FLAGC_DOT)
			{
				while (n < precision && usp[n]) n++;
			}
			else
			{
				while (usp[n]) n++;
			}

			width -= n;

			if (!(flagc & FLAGC_LEFTADJ) && width > 0) PUT_UCH(hak, fmtout, padc, width);
			PUT_UCS(hak, fmtout, usp, n);
			if ((flagc & FLAGC_LEFTADJ) && width > 0) PUT_UCH(hak, fmtout, padc, width);

			break;
		}

		case 'k':
		case 'K':
		{
			/* byte or multibyte character string in escape sequence */
			const hak_uint8_t* bsp;
			hak_oow_t k_hex_width;

			/* zeropad must not take effect for 'k' and 'K'
			 *
 			 * 'h' & 'l' is not used to differentiate hak_bch_t and hak_uch_t
			 * because 'k' means hak_byte_t.
			 * 'l', results in uppercase hexadecimal letters.
			 * 'h' drops the leading \x in the output
			 * --------------------------------------------------------
			 * hk -> \x + non-printable in lowercase hex
			 * k -> all in lowercase hex
			 * lk -> \x +  all in lowercase hex
			 * --------------------------------------------------------
			 * hK -> \x + non-printable in uppercase hex
			 * K -> all in uppercase hex
			 * lK -> \x +  all in uppercase hex
			 * --------------------------------------------------------
			 * with 'k' or 'K', i don't substitute "(null)" for the NULL pointer
			 */
			if (flagc & FLAGC_ZEROPAD) padc = ' ';

			bsp = va_arg(ap, hak_uint8_t*);
			k_hex_width = (lm_flag & (LF_H | LF_L))? 4: 2;

			if (lm_flag& LF_H)
			{
				if (flagc & FLAGC_DOT)
				{
					/* if precision is specifed, it doesn't stop at the value of zero unlike 's' or 'S' */
					for (n = 0; n < precision; n++) width -= BYTE_PRINTABLE(bsp[n])? 1: k_hex_width;
				}
				else
				{
					for (n = 0; bsp[n]; n++) width -= BYTE_PRINTABLE(bsp[n])? 1: k_hex_width;
				}
			}
			else
			{
				if (flagc & FLAGC_DOT)
				{
					/* if precision is specifed, it doesn't stop at the value of zero unlike 's' or 'S' */
					n = precision;
				}
				else
				{
					for (n = 0; bsp[n]; n++) /* nothing */;
				}
				width -= (n * k_hex_width);
			}

			if (!(flagc & FLAGC_LEFTADJ) && width > 0) PUT_OOCH(hak, fmtout, padc, width);

			while (n--)
			{
				if ((lm_flag & LF_H) && BYTE_PRINTABLE(*bsp))
				{
					PUT_BCH(hak, fmtout, *bsp, 1);
				}
				else
				{
					hak_bch_t xbuf[3];
					hak_byte_to_bcstr (*bsp, xbuf, HAK_COUNTOF(xbuf), (16 | (uch == 'k'? HAK_BYTE_TO_BCSTR_LOWERCASE: 0)), '0');
					if (lm_flag & (LF_H | LF_L)) PUT_BCS(hak, fmtout, "\\x", 2);
					PUT_BCS(hak, fmtout, xbuf, 2);
				}
				bsp++;
			}

			if ((flagc & FLAGC_LEFTADJ) && width > 0) PUT_OOCH(hak, fmtout, padc, width);
			break;
		}

		case 'w':
		case 'W':
		{
			/* unicode string in unicode escape sequence.
			 *
			 * hw -> \uXXXX, \UXXXXXXXX, printable-byte(only in ascii range)
			 * w -> \uXXXX, \UXXXXXXXX
			 * lw -> all in \UXXXXXXXX
			 */
			const hak_uch_t* usp;
			hak_oow_t uwid;

			if (flagc & FLAGC_ZEROPAD) padc = ' ';
			usp = va_arg(ap, hak_uch_t*);

			if (flagc & FLAGC_DOT)
			{
				/* if precision is specifed, it doesn't stop at the value of zero unlike 's' or 'S' */
				for (n = 0; n < precision; n++)
				{
					if ((lm_flag & LF_H) && BYTE_PRINTABLE(usp[n])) uwid = 1;
					else if (!(lm_flag & LF_L) && usp[n] <= 0xFFFF) uwid = 6;
					else uwid = 10;
					width -= uwid;
				}
			}
			else
			{
				for (n = 0; usp[n]; n++)
				{
					if ((lm_flag & LF_H) && BYTE_PRINTABLE(usp[n])) uwid = 1;
					else if (!(lm_flag & LF_L) && usp[n] <= 0xFFFF) uwid = 6;
					else uwid = 10;
					width -= uwid;
				}
			}

			if (!(flagc & FLAGC_LEFTADJ) && width > 0) PUT_OOCH(hak, fmtout, padc, width);

			while (n--)
			{
				if ((lm_flag & LF_H) && BYTE_PRINTABLE(*usp))
				{
					PUT_OOCH(hak, fmtout, *usp, 1);
				}
				else if (!(lm_flag & LF_L) && *usp <= 0xFFFF)
				{
					hak_uint16_t u16 = *usp;
					int extra_flags = ((uch) == 'w'? HAK_BYTE_TO_BCSTR_LOWERCASE: 0);
					PUT_BCS(hak, fmtout, "\\u", 2);
					PUT_BYTE_IN_HEX(hak, fmtout, (u16 >> 8) & 0xFF, extra_flags);
					PUT_BYTE_IN_HEX(hak, fmtout, u16 & 0xFF, extra_flags);
				}
				else
				{
					hak_uint32_t u32 = *usp;
					int extra_flags = ((uch) == 'w'? HAK_BYTE_TO_BCSTR_LOWERCASE: 0);
					PUT_BCS(hak, fmtout, "\\u", 2);
					PUT_BYTE_IN_HEX(hak, fmtout, (u32 >> 24) & 0xFF, extra_flags);
					PUT_BYTE_IN_HEX(hak, fmtout, (u32 >> 16) & 0xFF, extra_flags);
					PUT_BYTE_IN_HEX(hak, fmtout, (u32 >> 8) & 0xFF, extra_flags);
					PUT_BYTE_IN_HEX(hak, fmtout, u32 & 0xFF, extra_flags);
				}
				usp++;
			}

			if ((flagc & FLAGC_LEFTADJ) && width > 0) PUT_OOCH(hak, fmtout, padc, width);
			break;
		}

		case 'O': /* object - ignore precision, width, adjustment */
		{
			if (HAK_UNLIKELY(!fmtout->putobj)) goto invalid_format;
			if (fmtout->putobj(hak, fmtout, va_arg(ap, hak_oop_t)) <= -1) goto oops;
			break;
		}

		case 'J':
		{
			hak_bitmask_t tmp;
			if (HAK_UNLIKELY(!fmtout->putobj)) goto invalid_format;
			tmp = fmtout->mask;
			fmtout->mask |= HAK_LOG_PREFER_JSON;
			if (fmtout->putobj(hak, fmtout, va_arg(ap, hak_oop_t)) <= -1) goto oops;
			fmtout->mask = tmp;
			break;
		}

#if defined(HAK_ENABLE_FLTFMT)
		case 'e':
		case 'E':
		case 'f':
		case 'F':
		case 'g':
		case 'G':
		/*
		case 'a':
		case 'A':
		*/
		{
			/* let me rely on snprintf until i implement float-point to string conversion */
			int q;
			hak_oow_t fmtlen;
			union
			{
			#if (HAK_SIZEOF___FLOAT128 > 0) && defined(HAVE_QUADMATH_SNPRINTF)
				__float128 qd;
			#endif
				long double ld;
				double d;
			} v;
			int dtype = 0;
			hak_oow_t newcapa;
			hak_bch_t* bsp;

			if (lm_flag & LF_J)
			{
			#if (HAK_SIZEOF___FLOAT128 > 0) && defined(HAVE_QUADMATH_SNPRINTF) && (HAK_SIZEOF_FLTMAX_T == HAK_SIZEOF___FLOAT128)
				v.qd = va_arg(ap, hak_fltmax_t);
				dtype = LF_QD;
			#elif HAK_SIZEOF_FLTMAX_T == HAK_SIZEOF_DOUBLE
				v.d = va_arg(ap, hak_fltmax_t);
			#elif HAK_SIZEOF_FLTMAX_T == HAK_SIZEOF_LONG_DOUBLE
				v.ld = va_arg(ap, hak_fltmax_t);
				dtype = LF_LD;
			#else
				#error Unsupported hak_flt_t
			#endif
			}
			else if (lm_flag & LF_Z)
			{
				/* hak_flt_t is limited to double or long double */

				/* precedence goes to double if sizeof(double) == sizeof(long double)
				 * for example, %Lf didn't work on some old platforms.
				 * so i prefer the format specifier with no modifier.
				 */
			#if HAK_SIZEOF_FLT_T == HAK_SIZEOF_DOUBLE
				v.d = va_arg(ap, hak_flt_t);
			#elif HAK_SIZEOF_FLT_T == HAK_SIZEOF_LONG_DOUBLE
				v.ld = va_arg(ap, hak_flt_t);
				dtype = LF_LD;
			#else
				#error Unsupported hak_flt_t
			#endif
			}
			else if (lm_flag & (LF_LD | LF_L))
			{
				v.ld = va_arg(ap, long double);
				dtype = LF_LD;
			}
		#if (HAK_SIZEOF___FLOAT128 > 0) && defined(HAVE_QUADMATH_SNPRINTF)
			else if (lm_flag & (LF_QD | LF_Q))
			{
				v.qd = va_arg(ap, __float128);
				dtype = LF_QD;
			}
		#endif
			else if (flagc & FLAGC_LENMOD)
			{
				goto invalid_format;
			}
			else
			{
				v.d = va_arg(ap, double);
			}

			fmtlen = fmtptr - percent;
			if (fmtlen > fb.fmt.capa)
			{
				if (fb.fmt.ptr == fb.fmt.sbuf)
				{
					fb.fmt.ptr = (hak_bch_t*)HAK_MMGR_ALLOC(fmtout->mmgr, HAK_SIZEOF(*fb.fmt.ptr) * (fmtlen + 1));
					if (!fb.fmt.ptr) goto oops;
				}
				else
				{
					hak_bch_t* tmpptr;

					tmpptr = (hak_bch_t*)HAK_MMGR_REALLOC(fmtout->mmgr, fb.fmt.ptr, HAK_SIZEOF(*fb.fmt.ptr) * (fmtlen + 1));
					if (!tmpptr) goto oops;
					fb.fmt.ptr = tmpptr;
				}

				fb.fmt.capa = fmtlen;
			}

			/* compose back the format specifier */
			fmtlen = 0;
			fb.fmt.ptr[fmtlen++] = '%';
			if (flagc & FLAGC_SPACE) fb.fmt.ptr[fmtlen++] = ' ';
			if (flagc & FLAGC_SHARP) fb.fmt.ptr[fmtlen++] = '#';
			if (flagc & FLAGC_SIGN) fb.fmt.ptr[fmtlen++] = '+';
			if (flagc & FLAGC_LEFTADJ) fb.fmt.ptr[fmtlen++] = '-';
			if (flagc & FLAGC_ZEROPAD) fb.fmt.ptr[fmtlen++] = '0';

			if (flagc & FLAGC_STAR1) fb.fmt.ptr[fmtlen++] = '*';
			else if (flagc & FLAGC_WIDTH)
			{
				fmtlen += hak_fmt_uintmax_to_bcstr(
					&fb.fmt.ptr[fmtlen], fb.fmt.capa - fmtlen,
					width, 10, -1, '\0', HAK_NULL);
			}
			if (flagc & FLAGC_DOT) fb.fmt.ptr[fmtlen++] = '.';
			if (flagc & FLAGC_STAR2) fb.fmt.ptr[fmtlen++] = '*';
			else if (flagc & FLAGC_PRECISION)
			{
				fmtlen += hak_fmt_uintmax_to_bcstr(
					&fb.fmt.ptr[fmtlen], fb.fmt.capa - fmtlen,
					precision, 10, -1, '\0', HAK_NULL);
			}

			if (dtype == LF_LD)
				fb.fmt.ptr[fmtlen++] = 'L';
		#if (HAK_SIZEOF___FLOAT128 > 0)
			else if (dtype == LF_QD)
				fb.fmt.ptr[fmtlen++] = 'Q';
		#endif

			fb.fmt.ptr[fmtlen++] = uch;
			fb.fmt.ptr[fmtlen] = '\0';

		#if defined(HAVE_SNPRINTF)
			/* nothing special here */
		#else
			/* best effort to avoid buffer overflow when no snprintf is available.
			 * i really can't do much if it happens. */
			newcapa = precision + width + 32;
			if (fb.out.capa < newcapa)
			{
				/*HAK_ASSERT(hak, fb.out.ptr == fb.out.sbuf);*/

				fb.out.ptr = (hak_bch_t*)HAK_MMGR_ALLOC(fmtout->mmgr, HAK_SIZEOF(hak_bch_t) * (newcapa + 1));
				if (!fb.out.ptr) goto oops;
				fb.out.capa = newcapa;
			}
		#endif

			while (1)
			{
				if (dtype == LF_LD)
				{
				#if defined(HAVE_SNPRINTF)
					q = snprintf((hak_bch_t*)fb.out.ptr, fb.out.capa + 1, fb.fmt.ptr, v.ld);
				#else
					q = sprintf((hak_bch_t*)fb.out.ptr, fb.fmt.ptr, v.ld);
				#endif
				}
			#if (HAK_SIZEOF___FLOAT128 > 0) && defined(HAVE_QUADMATH_SNPRINTF)
				else if (dtype == LF_QD)
				{
					q = quadmath_snprintf((hak_bch_t*)fb.out.ptr, fb.out.capa + 1, fb.fmt.ptr, v.qd);
				}
			#endif
				else
				{
				#if defined(HAVE_SNPRINTF)
					q = snprintf((hak_bch_t*)fb.out.ptr, fb.out.capa + 1, fb.fmt.ptr, v.d);
				#else
					q = sprintf((hak_bch_t*)fb.out.ptr, fb.fmt.ptr, v.d);
				#endif
				}
				if (q <= -1) goto oops;
				if (q <= fb.out.capa) break;

				newcapa = fb.out.capa * 2;
				if (newcapa < q) newcapa = q;

				if (fb.out.ptr == fb.out.sbuf)
				{
					fb.out.ptr = (hak_bch_t*)HAK_MMGR_ALLOC(fmtout->mmgr, HAK_SIZEOF(hak_bch_t) * (newcapa + 1));
					if (!fb.out.ptr) goto oops;
				}
				else
				{
					hak_bch_t* tmpptr;
					tmpptr = (hak_bch_t*)HAK_MMGR_REALLOC(fmtout->mmgr, fb.out.ptr, HAK_SIZEOF(hak_bch_t) * (newcapa + 1));
					if (!tmpptr) goto oops;
					fb.out.ptr = tmpptr;
				}
				fb.out.capa = newcapa;
			}

			bsp = fb.out.ptr;
			n = 0; while (bsp[n] != '\0') n++;
			PUT_BCS(hak, fmtout, bsp, n);
			break;
		}
#endif

		handle_nosign:
			sign = 0;
			if (lm_flag & LF_J)
			{
			#if 1 && !defined(__clang__) && defined(__GNUC__) && \
			    (HAK_SIZEOF_UINTMAX_T > HAK_SIZEOF_OOW_T) && \
			    (HAK_SIZEOF_UINTMAX_T != HAK_SIZEOF_LONG_LONG) && \
			    (HAK_SIZEOF_UINTMAX_T != HAK_SIZEOF_LONG)
				/* GCC-compiled binaries crashed when getting hak_uintmax_t with va_arg.
				 * This is just a work-around for it */
				int i;
				for (i = 0, num = 0; i < HAK_SIZEOF(hak_uintmax_t) / HAK_SIZEOF(hak_oow_t); i++)
				{
				#if defined(HAK_ENDIAN_BIG)
					num = num << (8 * HAK_SIZEOF(hak_oow_t)) | (va_arg (ap, hak_oow_t));
				#else
					register int shift = i * HAK_SIZEOF(hak_oow_t);
					hak_oow_t x = va_arg (ap, hak_oow_t);
					num |= (hak_uintmax_t)x << (shift * HAK_BITS_PER_BYTE);
				#endif
				}
			#else
				num = va_arg (ap, hak_uintmax_t);
			#endif
			}
#if 0
			else if (lm_flag & LF_T)
				num = va_arg(ap, hak_ptrdiff_t);
#endif
			else if (lm_flag & LF_Z)
				num = va_arg(ap, hak_oow_t);
			#if (HAK_SIZEOF_LONG_LONG > 0)
			else if (lm_flag & LF_Q)
				num = va_arg(ap, unsigned long long int);
			#endif
			else if (lm_flag & (LF_L | LF_LD))
				num = va_arg(ap, unsigned long int);
			else if (lm_flag & LF_H)
				num = (unsigned short int)va_arg(ap, int);
			else if (lm_flag & LF_C)
				num = (unsigned char)va_arg(ap, int);
			else
				num = va_arg(ap, unsigned int);
			goto number;

		handle_sign:
			if (lm_flag & LF_J)
			{
			#if 1 && !defined(__clang__) && defined(__GNUC__) && \
			    (HAK_SIZEOF_INTMAX_T > HAK_SIZEOF_OOI_T) && \
			    (HAK_SIZEOF_UINTMAX_T != HAK_SIZEOF_LONG_LONG) && \
			    (HAK_SIZEOF_UINTMAX_T != HAK_SIZEOF_LONG)
				/* GCC-compiled binraries crashed when getting hak_uintmax_t with va_arg.
				 * This is just a work-around for it */
				int i;
				for (i = 0, num = 0; i < HAK_SIZEOF(hak_intmax_t) / HAK_SIZEOF(hak_oow_t); i++)
				{
				#if defined(HAK_ENDIAN_BIG)
					num = num << (8 * HAK_SIZEOF(hak_oow_t)) | (va_arg (ap, hak_oow_t));
				#else
					register int shift = i * HAK_SIZEOF(hak_oow_t);
					hak_oow_t x = va_arg (ap, hak_oow_t);
					num |= (hak_uintmax_t)x << (shift * HAK_BITS_PER_BYTE);
				#endif
				}
			#else
				num = va_arg (ap, hak_intmax_t);
			#endif
			}

#if 0
			else if (lm_flag & LF_T)
				num = va_arg(ap, hak_ptrdiff_t);
#endif
			else if (lm_flag & LF_Z)
				num = va_arg (ap, hak_ooi_t);
			#if (HAK_SIZEOF_LONG_LONG > 0)
			else if (lm_flag & LF_Q)
				num = va_arg (ap, long long int);
			#endif
			else if (lm_flag & (LF_L | LF_LD))
				num = va_arg (ap, long int);
			else if (lm_flag & LF_H)
				num = (short int)va_arg (ap, int);
			else if (lm_flag & LF_C)
				num = (char)va_arg (ap, int);
			else
				num = va_arg (ap, int);

		number:
			if (sign && (hak_intmax_t)num < 0)
			{
				neg = 1;
				num = -(hak_intmax_t)num;
			}

			nbufp = sprintn(nbuf, num, base, &tmp);
			if ((flagc & FLAGC_SHARP) && num != 0)
			{
				/* #b #o #x */
				if (base == 2 || base == 8 || base == 16) tmp += 2;
			}
			if (neg) tmp++;
			else if (flagc & FLAGC_SIGN) tmp++;
			else if (flagc & FLAGC_SPACE) tmp++;

			numlen = (int)((const hak_bch_t*)nbufp - (const hak_bch_t*)nbuf);
			if ((flagc & FLAGC_DOT) && precision > numlen)
			{
				/* extra zeros for precision specified */
				tmp += (precision - numlen);
			}

			if (!(flagc & FLAGC_LEFTADJ) && !(flagc & FLAGC_ZEROPAD) && width > 0 && (width -= tmp) > 0)
			{
				PUT_OOCH(hak, fmtout, padc, width);
				width = 0;
			}

			if (neg) PUT_OOCH(hak, fmtout, '-', 1);
			else if (flagc & FLAGC_SIGN) PUT_OOCH(hak, fmtout, '+', 1);
			else if (flagc & FLAGC_SPACE) PUT_OOCH(hak, fmtout, ' ', 1);

			if ((flagc & FLAGC_SHARP) && num != 0)
			{
				if (base == 2)
				{
					PUT_OOCH(hak, fmtout, '0', 1);
					PUT_OOCH(hak, fmtout, 'b', 1);
				}
				if (base == 8)
				{
					PUT_OOCH(hak, fmtout, '0', 1);
					PUT_OOCH(hak, fmtout, 'o', 1);
				}
				else if (base == 16)
				{
					PUT_OOCH(hak, fmtout, '0', 1);
					PUT_OOCH(hak, fmtout, 'x', 1);
				}
			}

			if ((flagc & FLAGC_DOT) && precision > numlen)
			{
				/* extra zeros for precision specified */
				PUT_OOCH(hak, fmtout, '0', precision - numlen);
			}

			if (!(flagc & FLAGC_LEFTADJ) && width > 0 && (width -= tmp) > 0)
			{
				PUT_OOCH(hak, fmtout, padc, width);
			}

			while (*nbufp) PUT_OOCH(hak, fmtout, *nbufp--, 1); /* output actual digits */

			if ((flagc & FLAGC_LEFTADJ) && width > 0 && (width -= tmp) > 0)
			{
				PUT_OOCH(hak, fmtout, padc, width);
			}
			break;

		invalid_format:
			switch (fmtout->fmt_type)
			{
				case HAK_FMTOUT_FMT_TYPE_BCH:
					PUT_BCS(hak, fmtout, (const hak_bch_t*)percent, (fmtptr - percent) / fmtchsz);
					break;
				case HAK_FMTOUT_FMT_TYPE_UCH:
					PUT_UCS(hak, fmtout, (const hak_uch_t*)percent, (fmtptr - percent) / fmtchsz);
					break;
			}
			break;

		default:
			switch (fmtout->fmt_type)
			{
				case HAK_FMTOUT_FMT_TYPE_BCH:
					PUT_BCS(hak, fmtout, (const hak_bch_t*)percent, (fmtptr - percent) / fmtchsz);
					break;
				case HAK_FMTOUT_FMT_TYPE_UCH:
					PUT_UCS(hak, fmtout, (const hak_uch_t*)percent, (fmtptr - percent) / fmtchsz);
					break;
			}
			/*
			 * Since we ignore an formatting argument it is no
			 * longer safe to obey the remaining formatting
			 * arguments as the arguments will no longer match
			 * the format specs.
			 */
			stop = 1;
			break;
		}
	}

done:
#if defined(HAK_ENABLE_FLTFMT)
	if (fb.fmt.ptr != fb.fmt.sbuf) HAK_MMGR_FREE (fmtout->mmgr, fb.fmt.ptr);
	if (fb.out.ptr != fb.out.sbuf) HAK_MMGR_FREE (fmtout->mmgr, fb.out.ptr);
#endif
	return 0;

oops:
#if defined(HAK_ENABLE_FLTFMT)
	if (fb.fmt.ptr != fb.fmt.sbuf) HAK_MMGR_FREE (fmtout->mmgr, fb.fmt.ptr);
	if (fb.out.ptr != fb.out.sbuf) HAK_MMGR_FREE (fmtout->mmgr, fb.out.ptr);
#endif
	return -1;
}

int hak_bfmt_outv (hak_t* hak, hak_fmtout_t* fmtout, const hak_bch_t* fmt, va_list ap)
{
	int n;
	const void* fmt_str;
	hak_fmtout_fmt_type_t fmt_type;

	fmt_str = fmtout->fmt_str;
	fmt_type = fmtout->fmt_type;

	fmtout->fmt_type = HAK_FMTOUT_FMT_TYPE_BCH;
	fmtout->fmt_str = fmt;

	n = fmt_outv(hak, fmtout, ap);

	fmtout->fmt_str = fmt_str;
	fmtout->fmt_type = fmt_type;
	return n;
}

int hak_ufmt_outv (hak_t* hak, hak_fmtout_t* fmtout, const hak_uch_t* fmt, va_list ap)
{
	int n;
	const void* fmt_str;
	hak_fmtout_fmt_type_t fmt_type;

	fmt_str = fmtout->fmt_str;
	fmt_type = fmtout->fmt_type;

	fmtout->fmt_type = HAK_FMTOUT_FMT_TYPE_UCH;
	fmtout->fmt_str = fmt;

	n = fmt_outv(hak, fmtout, ap);

	fmtout->fmt_str = fmt_str;
	fmtout->fmt_type = fmt_type;
	return n;
}

int hak_bfmt_out (hak_t* hak, hak_fmtout_t* fmtout, const hak_bch_t* fmt, ...)
{
	va_list ap;
	int n;
	const void* fmt_str;
	hak_fmtout_fmt_type_t fmt_type;

	fmt_str = fmtout->fmt_str;
	fmt_type = fmtout->fmt_type;

	fmtout->fmt_type = HAK_FMTOUT_FMT_TYPE_BCH;
	fmtout->fmt_str = fmt;

	va_start (ap, fmt);
	n = fmt_outv(hak, fmtout, ap);
	va_end (ap);

	fmtout->fmt_str = fmt_str;
	fmtout->fmt_type = fmt_type;
	return n;
}

int hak_ufmt_out (hak_t* hak, hak_fmtout_t* fmtout, const hak_uch_t* fmt, ...)
{
	va_list ap;
	int n;
	const void* fmt_str;
	hak_fmtout_fmt_type_t fmt_type;

	fmt_str = fmtout->fmt_str;
	fmt_type = fmtout->fmt_type;

	fmtout->fmt_type = HAK_FMTOUT_FMT_TYPE_UCH;
	fmtout->fmt_str = fmt;

	va_start (ap, fmt);
	n = fmt_outv(hak, fmtout, ap);
	va_end (ap);

	fmtout->fmt_str = fmt_str;
	fmtout->fmt_type = fmt_type;
	return n;
}

/* --------------------------------------------------------------------------
 * FORMATTED LOG OUTPUT
 * -------------------------------------------------------------------------- */

static int log_oocs (hak_t* hak, hak_fmtout_t* fmtout, const hak_ooch_t* ptr, hak_oow_t len)
{
	hak_oow_t rem;

	if (len <= 0) return 1;

	if (hak->log.len > 0 && hak->log.last_mask != fmtout->mask)
	{
		/* the mask has changed. commit the buffered text */
/* TODO: HANDLE LINE ENDING CONVENTION BETTER... */
		if (hak->log.ptr[hak->log.len - 1] != '\n')
		{
			/* no line ending - append a line terminator */
			hak->log.ptr[hak->log.len++] = '\n';
		}

		HAK_VMPRIM_LOG_WRITE(hak, hak->log.last_mask, hak->log.ptr, hak->log.len);
		hak->log.len = 0;
	}

redo:
	rem = 0;
	if (len > hak->log.capa - hak->log.len)
	{
		hak_oow_t newcapa, max;
		hak_ooch_t* tmp;

		max = HAK_TYPE_MAX(hak_oow_t) - hak->log.len;
		if (len > max)
		{
			/* data too big. */
			rem += len - max;
			len = max;
		}

		newcapa = HAK_ALIGN_POW2(hak->log.len + len, 512); /* TODO: adjust this capacity */
		if (newcapa > hak->option.log_maxcapa)
		{
			/* [NOTE]
			 * it doesn't adjust newcapa to hak->option.log_maxcapa.
			 * nor does it cut the input to fit it into the adjusted capacity.
			 * if maxcapa set is not aligned to HAK_LOG_CAPA_ALIGN,
			 * the largest buffer capacity may be suboptimal */
			goto make_do;
		}

		/* +1 to handle line ending injection more easily */
		tmp = (hak_ooch_t*)hak_reallocmem(hak, hak->log.ptr, (newcapa + 1) * HAK_SIZEOF(*tmp));
		if (HAK_UNLIKELY(!tmp))
		{
		make_do:
			if (hak->log.len > 0)
			{
				/* can't expand the buffer. just flush the existing contents */
				/* TODO: HANDLE LINE ENDING CONVENTION BETTER... */
				if (hak->log.ptr[hak->log.len - 1] != '\n')
				{
					/* no line ending - append a line terminator */
					hak->log.ptr[hak->log.len++] = '\n';
				}
				HAK_VMPRIM_LOG_WRITE(hak, hak->log.last_mask, hak->log.ptr, hak->log.len);
				hak->log.len = 0;
			}

			if (len > hak->log.capa)
			{
				rem += len - hak->log.capa;
				len = hak->log.capa;
			}
		}
		else
		{
			hak->log.ptr = tmp;
			hak->log.capa = newcapa;
		}
	}

	HAK_MEMCPY(&hak->log.ptr[hak->log.len], ptr, len * HAK_SIZEOF(*ptr));
	hak->log.len += len;
	hak->log.last_mask = fmtout->mask;

	if (rem > 0)
	{
		ptr += len;
		len = rem;
		goto redo;
	}

	return 1; /* success */
}

#if defined(HAK_OOCH_IS_BCH)
#define log_bcs log_oocs

static int log_ucs (hak_t* hak, hak_fmtout_t* fmtout, const hak_uch_t* ptr, hak_oow_t len)
{
	hak_bch_t bcs[128];
	hak_oow_t bcslen, rem;

	rem = len;
	while (rem > 0)
	{
		len = rem;
		bcslen = HAK_COUNTOF(bcs);
		hak_conv_uchars_to_bchars_with_cmgr (ptr, &len, bcs, &bcslen, HAK_CMGR(hak));
		log_bcs(hak, fmtout, bcs, bcslen);
		rem -= len;
		ptr += len;
	}
	return 1;
}


#else

#define log_ucs log_oocs

static int log_bcs (hak_t* hak, hak_fmtout_t* fmtout, const hak_bch_t* ptr, hak_oow_t len)
{
	hak_uch_t ucs[64];
	hak_oow_t ucslen, rem;

	rem = len;
	while (rem > 0)
	{
		len = rem;
		ucslen = HAK_COUNTOF(ucs);
		hak_conv_bchars_to_uchars_with_cmgr (ptr, &len, ucs, &ucslen, HAK_CMGR(hak), 1);
		log_ucs(hak, fmtout, ucs, ucslen);
		rem -= len;
		ptr += len;
	}
	return 1;
}

#endif

hak_ooi_t hak_logbfmtv (hak_t* hak, hak_bitmask_t mask, const hak_bch_t* fmt, va_list ap)

{
	int x;
	hak_fmtout_t fo;

	if (hak->log.default_type_mask & HAK_LOG_ALL_TYPES)
	{
		/* if a type is given, it's not untyped any more.
		 * mask off the UNTYPED bit */
		mask &= ~HAK_LOG_UNTYPED;

		/* if the default_type_mask has the UNTYPED bit on,
		 * it'll get turned back on */
		mask |= (hak->log.default_type_mask & HAK_LOG_ALL_TYPES);
	}
	else if (!(mask & HAK_LOG_ALL_TYPES))
	{
		/* no type is set in the given mask and no default type is set.
		 * make it UNTYPED. */
		mask |= HAK_LOG_UNTYPED;
	}

	if (!fmt)
	{
		/* perform flushing only if fmt is NULL */
		if (hak->log.len > 0)
		{
			HAK_VMPRIM_LOG_WRITE(hak, hak->log.last_mask, hak->log.ptr, hak->log.len);
			hak->log.len = 0;
		}
		HAK_VMPRIM_LOG_WRITE(hak, hak->log.last_mask, HAK_NULL, 0); /* forced flushing */
		return 0;
	}

	HAK_MEMSET(&fo, 0, HAK_SIZEOF(fo));
	fo.fmt_type = HAK_FMTOUT_FMT_TYPE_BCH;
	fo.fmt_str = fmt;
	fo.mask = mask;
	fo.mmgr = HAK_MMGR(hak);
	fo.putbchars = log_bcs;
	fo.putuchars = log_ucs;
	fo.putobj = hak_fmt_object;

	x = fmt_outv(hak, &fo, ap);

	if (hak->log.len > 0 && hak->log.ptr[hak->log.len - 1] == '\n')
	{
		HAK_VMPRIM_LOG_WRITE(hak, hak->log.last_mask, hak->log.ptr, hak->log.len);
		hak->log.len = 0;
	}

	return (x <= -1)? -1: fo.count;
}

hak_ooi_t hak_logbfmt (hak_t* hak, hak_bitmask_t mask, const hak_bch_t* fmt, ...)
{
	hak_ooi_t x;
	va_list ap;

	va_start (ap, fmt);
	x = hak_logbfmtv(hak, mask, fmt, ap);
	va_end (ap);

	return x;
}

hak_ooi_t hak_logufmtv (hak_t* hak, hak_bitmask_t mask, const hak_uch_t* fmt, va_list ap)
{
	int x;
	hak_fmtout_t fo;

	if (hak->log.default_type_mask & HAK_LOG_ALL_TYPES)
	{
		/* if a type is given, it's not untyped any more.
		 * mask off the UNTYPED bit */
		mask &= ~HAK_LOG_UNTYPED;

		/* if the default_type_mask has the UNTYPED bit on,
		 * it'll get turned back on */
		mask |= (hak->log.default_type_mask & HAK_LOG_ALL_TYPES);
	}
	else if (!(mask & HAK_LOG_ALL_TYPES))
	{
		/* no type is set in the given mask and no default type is set.
		 * make it UNTYPED. */
		mask |= HAK_LOG_UNTYPED;
	}


	if (!fmt)
	{
		/* perform flushing only if fmt is NULL */
		if (hak->log.len > 0)
		{
			HAK_VMPRIM_LOG_WRITE(hak, hak->log.last_mask, hak->log.ptr, hak->log.len);
			hak->log.len = 0;
		}
		HAK_VMPRIM_LOG_WRITE(hak, hak->log.last_mask, HAK_NULL, 0); /* forced flushing */
		return 0;
	}

	HAK_MEMSET(&fo, 0, HAK_SIZEOF(fo));
	fo.fmt_type = HAK_FMTOUT_FMT_TYPE_UCH;
	fo.fmt_str = fmt;
	fo.mask = mask;
	fo.mmgr = HAK_MMGR(hak);
	fo.putbchars = log_bcs;
	fo.putuchars = log_ucs;
	fo.putobj = hak_fmt_object;

	x = fmt_outv(hak, &fo, ap);

	if (hak->log.len > 0 && hak->log.ptr[hak->log.len - 1] == '\n')
	{
		HAK_VMPRIM_LOG_WRITE(hak, hak->log.last_mask, hak->log.ptr, hak->log.len);
		hak->log.len = 0;
	}

	return (x <= -1)? -1: fo.count;
}

hak_ooi_t hak_logufmt (hak_t* hak, hak_bitmask_t mask, const hak_uch_t* fmt, ...)
{
	hak_ooi_t x;
	va_list ap;

	va_start (ap, fmt);
	x = hak_logufmtv(hak, mask, fmt, ap);
	va_end (ap);

	return x;
}

/* --------------------------------------------------------------------------
 * PRINT SUPPORT
 * -------------------------------------------------------------------------- */

static int print_bcs (hak_t* hak, hak_fmtout_t* fmtout, const hak_bch_t* ptr, hak_oow_t len)
{
	hak_bch_t* optr;

	if (HAK_UNLIKELY(!hak->io.udo_wrtr))
	{
		hak_seterrbmsg(hak, HAK_EINVAL, "no user-defined output handler");
		return -1;
	}

	optr = (hak_bch_t*)ptr;
	while (len > 0)
	{
		hak->io.udo_arg.ptr = optr;
		hak->io.udo_arg.len = len;

		if (hak->io.udo_wrtr(hak, HAK_IO_WRITE_BYTES, &hak->io.udo_arg) <= -1) return -1;
		if (hak->io.udo_arg.xlen <= 0) return 0; /* end of stream. but not failure */

		HAK_ASSERT(hak, hak->io.udo_arg.xlen <= len);
		optr += hak->io.udo_arg.xlen;
		len -= hak->io.udo_arg.xlen;
	}

	return 1; /* success */
}

static int print_ucs (hak_t* hak, hak_fmtout_t* fmtout, const hak_uch_t* ptr, hak_oow_t len)
{
#if defined(HAK_OOCH_IS_UCH)
	hak_uch_t* optr;
#else
	hak_oow_t bcslen, ucslen;
	hak_ooch_t bcsbuf[64], * bcsptr;

#endif

	if (HAK_UNLIKELY(!hak->io.udo_wrtr))
	{
		hak_seterrbmsg(hak, HAK_EINVAL, "no user-defined output handler");
		return -1;
	}

#if defined(HAK_OOCH_IS_UCH)
	optr = (hak_uch_t*)ptr;
	while (len > 0)
	{
		hak->io.udo_arg.ptr = optr;
		hak->io.udo_arg.len = len;

		if (hak->io.udo_wrtr(hak, HAK_IO_WRITE, &hak->io.udo_arg) <= -1) return -1;
		if (hak->io.udo_arg.xlen <= 0) return 0; /* end of stream. but not failure */

		HAK_ASSERT(hak, hak->io.udo_arg.xlen <= len);
		optr += hak->io.udo_arg.xlen;
		len -= hak->io.udo_arg.xlen;
	}
#else
	while (len > 0)
	{
		ucslen = len;
		bcslen = HAK_COUNTOF(bcsbuf);
		hak_conv_uchars_to_bchars_with_cmgr(ptr, &ucslen, bcsbuf, &bcslen, HAK_CMGR(hak));

		bcsptr = bcsbuf;
		while (bcslen > 0)
		{
			hak->io.udo_arg.ptr = bcsptr;
			hak->io.udo_arg.len = bcslen;

			if (hak->io.udo_wrtr(hak, HAK_IO_WRITE, &hak->io.udo_arg) <= -1) return -1;
			if (hak->io.udo_arg.xlen <= 0) return 0; /* end of stream. but not failure */

			HAK_ASSERT(hak, hak->io.udo_arg.xlen <= len);
			bcsptr += hak->io.udo_arg.xlen;
			bcslen -= hak->io.udo_arg.xlen;
		}

		ptr += ucslen;
		len -= ucslen;
	}
#endif

	return 1; /* success */
}


hak_ooi_t hak_prbfmtv (hak_t* hak, const hak_bch_t* fmt, va_list ap)
{
	int x;
	hak_fmtout_t fo;

	HAK_MEMSET(&fo, 0, HAK_SIZEOF(fo));
	fo.fmt_type = HAK_FMTOUT_FMT_TYPE_BCH;
	fo.fmt_str = fmt;
	fo.mask = 0;
	fo.mmgr = HAK_MMGR(hak);
	fo.putbchars = print_bcs;
	fo.putuchars = print_ucs;
	fo.putobj = hak_fmt_object;

	x = fmt_outv(hak, &fo, ap);

	return (x <= -1)? -1: fo.count;
}

hak_ooi_t hak_prbfmt (hak_t* hak, const hak_bch_t* fmt, ...)
{
	hak_ooi_t x;
	va_list ap;

	va_start (ap, fmt);
	x = hak_prbfmtv(hak, fmt, ap);
	va_end (ap);

	return x;
}

hak_ooi_t hak_prufmtv (hak_t* hak, const hak_uch_t* fmt, va_list ap)
{
	int x;

	hak_fmtout_t fo;

	HAK_MEMSET(&fo, 0, HAK_SIZEOF(fo));
	fo.fmt_type = HAK_FMTOUT_FMT_TYPE_UCH;
	fo.fmt_str = fmt;
	fo.mask = 0;
	fo.mmgr = HAK_MMGR(hak);
	fo.putbchars = print_bcs;
	fo.putuchars = print_ucs;
	fo.putobj = hak_fmt_object;

	x = fmt_outv(hak, &fo, ap);

	return (x <= -1)? -1: fo.count;
}

hak_ooi_t hak_prufmt (hak_t* hak, const hak_uch_t* fmt, ...)
{
	hak_ooi_t x;
	va_list ap;

	va_start (ap, fmt);
	x = hak_prufmtv(hak, fmt, ap);
	va_end (ap);

	return x;
}

/* --------------------------------------------------------------------------
 * SUPPORT FOR FORMATTED OUTPUT TO BE USED BY BUILTIN PRIMITIVE FUNCTIONS
 * -------------------------------------------------------------------------- */

static int sprint_bcs (hak_t* hak, hak_fmtout_t* fmtout, const hak_bch_t* ptr, hak_oow_t len)
{
	hak_oow_t unused, oolen, blen;

	unused = hak->sprintf.xbuf.capa - hak->sprintf.xbuf.len;

#if defined(HAK_OOCH_IS_UCH)
	blen = len;
	hak_conv_bchars_to_uchars_with_cmgr (ptr, &blen, HAK_NULL, &oolen, HAK_CMGR(hak), 1);
#else
	oolen = len;
#endif

	if (oolen > unused)
	{
		hak_ooch_t* tmp;
		hak_oow_t newcapa;

		newcapa = hak->sprintf.xbuf.len + oolen + 1;
		newcapa = HAK_ALIGN_POW2(newcapa, 256);

		tmp = (hak_ooch_t*)hak_reallocmem(hak, hak->sprintf.xbuf.ptr, newcapa * HAK_SIZEOF(*tmp));
		if (!tmp) return -1;

		hak->sprintf.xbuf.ptr = tmp;
		hak->sprintf.xbuf.capa = newcapa;
	}

#if defined(HAK_OOCH_IS_UCH)
	hak_conv_bchars_to_uchars_with_cmgr (ptr, &len, &hak->sprintf.xbuf.ptr[hak->sprintf.xbuf.len], &oolen, HAK_CMGR(hak), 1);
#else
	HAK_MEMCPY(&hak->sprintf.xbuf.ptr[hak->sprintf.xbuf.len], ptr, len * HAK_SIZEOF(*ptr));
#endif
	hak->sprintf.xbuf.len += oolen;

	return 1; /* success */
}

static int sprint_ucs (hak_t* hak, hak_fmtout_t* fmtout, const hak_uch_t* ptr, hak_oow_t len)
{
	hak_oow_t unused, oolen, ulen;

	unused = hak->sprintf.xbuf.capa - hak->sprintf.xbuf.len;

#if defined(HAK_OOCH_IS_UCH)
	oolen = len;
#else
	ulen = len;
	hak_conv_uchars_to_bchars_with_cmgr (ptr, &ulen, HAK_NULL, &oolen, HAK_CMGR(hak));
#endif

	if (oolen > unused)
	{
		hak_ooch_t* tmp;
		hak_oow_t newcapa;

		newcapa = hak->sprintf.xbuf.len + oolen + 1;
		newcapa = HAK_ALIGN_POW2(newcapa, 256);

		tmp = (hak_ooch_t*)hak_reallocmem(hak, hak->sprintf.xbuf.ptr, newcapa * HAK_SIZEOF(*tmp));
		if (!tmp) return -1;

		hak->sprintf.xbuf.ptr = tmp;
		hak->sprintf.xbuf.capa = newcapa;
	}

#if defined(HAK_OOCH_IS_UCH)
	HAK_MEMCPY(&hak->sprintf.xbuf.ptr[hak->sprintf.xbuf.len], ptr, len * HAK_SIZEOF(*ptr));
#else
	hak_conv_uchars_to_bchars_with_cmgr (ptr, &len, &hak->sprintf.xbuf.ptr[hak->sprintf.xbuf.len], &oolen, HAK_CMGR(hak));
#endif
	hak->sprintf.xbuf.len += oolen;

	return 1; /* success */
}

#define GET_NEXT_ARG_TO(hak,nargs,arg_state,arg) do { \
	if ((arg_state)->idx >= nargs) { (arg_state)->stop = 1;  goto invalid_format; } \
	arg = HAK_STACK_GETARG(hak, nargs, (arg_state)->idx); \
	(arg_state)->idx++; \
} while(0)

#define GET_NEXT_CHAR_TO(hak,fmt,fmtend,ch) do { \
	if (fmt >= fmtend) ch = HAK_OOCI_EOF; \
	else { ch = *(fmt); (fmt)++; }\
} while(0)

static HAK_INLINE int format_stack_args (hak_t* hak, hak_fmtout_t* fmtout, hak_ooi_t nargs, int rcv_is_fmtstr)
{
	const hak_ooch_t* fmtptr, * fmtend;
	const hak_ooch_t* checkpoint, * percent;

	int n, radix, neg, sign, radix_flags;
	hak_ooi_t extra, width, precision;
	hak_ooch_t padc, ooch;
	hak_ooci_t ch;
	int flagc, lm_flag;

	struct
	{
		hak_ooi_t idx;
		int stop;
	} arg_state;
	hak_oop_t arg;

	HAK_ASSERT(hak, fmtout->putobj != HAK_NULL);

	fmtout->count = 0;

	if (rcv_is_fmtstr)
	{
		arg = HAK_STACK_GETRCV(hak, nargs);
		arg_state.idx = 0;
	}
	else
	{
		arg = HAK_STACK_GETARG(hak, nargs, 0);
		arg_state.idx = 1;
	}

	if (!HAK_OOP_IS_POINTER(arg) || HAK_OBJ_GET_FLAGS_TYPE(arg) != HAK_OBJ_TYPE_CHAR)
	{
		hak_ooi_t i;
		/* if the first argument is not a valid formatting string,
		 * print all arguments as objects */
		if (fmtout->putobj(hak, fmtout, arg) <= -1) goto oops;
		for (i = arg_state.idx; i < nargs; i++)
		{
			arg = HAK_STACK_GETARG(hak, nargs, i);
			if (fmtout->putobj(hak, fmtout, arg) <= -1) goto oops;
		}
		return 0;
	}

	arg_state.stop = 0;

	fmtptr = HAK_OBJ_GET_CHAR_SLOT(arg);
	fmtend = fmtptr + HAK_OBJ_GET_SIZE(arg);

	while (1)
	{
		checkpoint = fmtptr;

		while (1)
		{
			GET_NEXT_CHAR_TO(hak, fmtptr, fmtend, ch);
			if (ch == '%' && !arg_state.stop) break;

			if (ch == HAK_OOCI_EOF)
			{
				/* fmt is not advanced when it is length-bounded.
				 * so not fmt - checkpoint - 1 */
				PUT_OOCS(hak, fmtout, checkpoint, fmtptr - checkpoint);
				goto done;
			}
		}
		PUT_OOCS(hak, fmtout, checkpoint, fmtptr - checkpoint - 1);

		percent = fmtptr - 1;

		padc = ' ';
		width = 0; precision = 0;
		neg = 0; sign = 0;

		lm_flag = 0; flagc = 0;
		radix_flags = HAK_INTTOSTR_NONEWOBJ;

	reswitch:
		GET_NEXT_CHAR_TO(hak, fmtptr, fmtend, ch);
		switch (ch)
		{
		case '%': /* %% */
			ooch = ch;
			goto print_char;

		/* flag characters */
		case '.':
			if (flagc & FLAGC_DOT) goto invalid_format;
			flagc |= FLAGC_DOT;
			goto reswitch;

		case '#':
			if (flagc & (FLAGC_WIDTH | FLAGC_DOT | FLAGC_LENMOD)) goto invalid_format;
			flagc |= FLAGC_SHARP;
			goto reswitch;

		case ' ':
			if (flagc & (FLAGC_WIDTH | FLAGC_DOT | FLAGC_LENMOD)) goto invalid_format;
			flagc |= FLAGC_SPACE;
			goto reswitch;

		case '+': /* place sign for signed conversion */
			if (flagc & (FLAGC_WIDTH | FLAGC_DOT | FLAGC_LENMOD)) goto invalid_format;
			flagc |= FLAGC_SIGN;
			goto reswitch;

		case '-': /* left adjusted */
			if (flagc & (FLAGC_WIDTH | FLAGC_DOT | FLAGC_LENMOD)) goto invalid_format;
			if (flagc & FLAGC_DOT)
			{
				goto invalid_format;
			}
			else
			{
				flagc |= FLAGC_LEFTADJ;
				if (flagc & FLAGC_ZEROPAD)
				{
					padc = ' ';
					flagc &= ~FLAGC_ZEROPAD;
				}
			}

			goto reswitch;

		case '*': /* take the length from the parameter */
			if (flagc & FLAGC_DOT)
			{
				if (flagc & (FLAGC_STAR2 | FLAGC_PRECISION)) goto invalid_format;
				flagc |= FLAGC_STAR2;

				GET_NEXT_ARG_TO(hak, nargs, &arg_state, arg);
				if (hak_inttoooi(hak, arg, &precision) <= -1) goto invalid_format;
				if (precision < 0)
				{
					/* if precision is less than 0,
					 * treat it as if no .precision is specified */
					flagc &= ~FLAGC_DOT;
					precision = 0;
				}
			}
			else
			{
				if (flagc & (FLAGC_STAR1 | FLAGC_WIDTH)) goto invalid_format;
				flagc |= FLAGC_STAR1;

				GET_NEXT_ARG_TO(hak, nargs, &arg_state, arg);
				if (hak_inttoooi(hak, arg, &width) <= -1) goto invalid_format;
				if (width < 0)
				{
					flagc |= FLAGC_LEFTADJ;
					width = -width;
				}
			}
			goto reswitch;

		case '0': /* zero pad */
			if (flagc & FLAGC_LENMOD) goto invalid_format;
			if (!(flagc & (FLAGC_DOT | FLAGC_LEFTADJ)))
			{
				padc = '0';
				flagc |= FLAGC_ZEROPAD;
				goto reswitch;
			}
		/* end of flags characters */

		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			if (flagc & FLAGC_LENMOD) goto invalid_format;
			for (n = 0;; ++fmtptr)
			{
				n = n * 10 + ch - '0';
				ch = *fmtptr;
				if (ch < '0' || ch > '9') break;
			}
			if (flagc & FLAGC_DOT)
			{
				if (flagc & FLAGC_STAR2) goto invalid_format;
				precision = n;
				flagc |= FLAGC_PRECISION;
			}
			else
			{
				if (flagc & FLAGC_STAR1) goto invalid_format;
				width = n;
				flagc |= FLAGC_WIDTH;
			}
			goto reswitch;

		/* length modifiers - used for k/K. not useful for s/S/d/i/o/u/x/X/b/f */
		case 'h': /* short int */
		case 'l': /* long int */
			if (lm_flag & (LF_L | LF_H)) goto invalid_format;
			flagc |= FLAGC_LENMOD;
			lm_flag |= lm_tab[ch - 'a'].flag;
			goto reswitch;

		/* integer conversions */
		case 'd':
		case 'i': /* signed conversion */
			radix = 10;
			sign = 1;
			goto print_integer;
		case 'o':
			radix = 8;
			goto print_integer;
		case 'u':
			radix = 10;
			goto print_integer;
		case 'x':
			radix_flags |= HAK_INTTOSTR_LOWERCASE;
		case 'X':
			radix = 16;
			goto print_integer;
		case 'b':
			radix = 2;
			goto print_integer;
		/* end of integer conversions */

		case 'f':
		{
			const hak_ooch_t* nsptr;
			hak_oow_t nslen;
			hak_oow_t scale = 0;

			GET_NEXT_ARG_TO(hak, nargs, &arg_state, arg);
			if (HAK_OOP_IS_CHAR(arg))
			{
				arg = HAK_SMOOI_TO_OOP(HAK_OOP_TO_CHAR(arg));
			}
			else if (HAK_IS_FPDEC(hak, arg))
			{
				hak_oop_fpdec_t fa = (hak_oop_fpdec_t)arg;
				scale = HAK_OOP_TO_SMOOI(fa->scale);
				arg = fa->value;
			}

			if (!hak_inttostr(hak, arg, 10 | HAK_INTTOSTR_NONEWOBJ))
			{
				HAK_LOG2 (hak, HAK_LOG_WARN | HAK_LOG_UNTYPED, "unable to convert %O for float output\n", arg, hak_geterrmsg(hak));
				goto invalid_format;
			}

			nsptr = hak->inttostr.xbuf.ptr;
			nslen = hak->inttostr.xbuf.len;
			HAK_ASSERT(hak, nslen > 0);

			if (nsptr[0] == '-')
			{
				HAK_ASSERT(hak, (HAK_OOP_IS_SMOOI(arg) && HAK_OOP_TO_SMOOI(arg) < 0) || HAK_IS_NBIGINT(hak,arg));
				nsptr++;
				nslen--;
				neg = 1;
			}

			if (!(flagc & FLAGC_DOT))
			{
				precision = scale;
				if (precision <= 0) precision = 1;
			}

			if ((flagc & FLAGC_DOT) && precision < scale)
			{
				hak_oow_t diff  = scale - precision;
				scale = precision;
				nslen = (nslen < diff)? 0: (nslen - diff);
			}

			if (nslen < scale + 1)
			{
				extra = 1;
				if (precision > 0) extra += 1 + scale;
			}
			else
			{
				extra = 0;
				if (nslen > 0) extra += nslen - scale;
				if (precision > 0)
				{
					extra += 1;
					if (nslen > 0) extra += scale;
				}
			}

			if (neg) extra++;
			else if (flagc & FLAGC_SIGN) extra++;
			else if (flagc & FLAGC_SPACE) extra++;

			if ((flagc & FLAGC_DOT) && precision > scale)
			{
				/* trailing zeros in the fractional part */
				extra += precision - scale;
			}

			if (!(flagc & FLAGC_LEFTADJ) && !(flagc & FLAGC_ZEROPAD) && width > extra)
			{
				width -= extra;
				PUT_OOCH(hak, fmtout, padc, width);
				width = 0;
			}
			if (neg) PUT_OOCH(hak, fmtout, '-', 1);
			else if (flagc & FLAGC_SIGN) PUT_OOCH(hak, fmtout, '+', 1);
			else if (flagc & FLAGC_SPACE) PUT_OOCH(hak, fmtout, ' ', 1);

			if (!(flagc & FLAGC_LEFTADJ) && width > extra)
			{
				width -= extra;
				PUT_OOCH(hak, fmtout, padc, width);
			}

			if (nslen < scale + 1)
			{
				PUT_OOCH(hak, fmtout, '0', 1);
				if (precision > 0)
				{
					PUT_OOCH(hak, fmtout, '.', 1);
					PUT_OOCH(hak, fmtout, '0', scale - nslen);
					PUT_OOCS(hak, fmtout, nsptr, nslen);
				}
			}
			else
			{
				if (nslen > 0) PUT_OOCS(hak, fmtout, nsptr, nslen - scale);
				if (precision > 0)
				{
					PUT_OOCH(hak, fmtout, '.', 1);
					if (nslen > 0) PUT_OOCS(hak, fmtout, &nsptr[nslen - scale], scale);
				}
			}
			if (precision > scale)
			{
				/* trailing zeros in the fractional part */
				PUT_OOCH(hak, fmtout, '0', precision - scale);
			}

			if ((flagc & FLAGC_LEFTADJ) && width > extra)
			{
				width -= extra;
				PUT_OOCH(hak, fmtout, padc, width);
			}
			break;
		}

		case 'c':
		case 'C':
			GET_NEXT_ARG_TO(hak, nargs, &arg_state, arg);
			if (HAK_OOP_IS_SMOOI(arg)) arg = HAK_CHAR_TO_OOP(HAK_OOP_TO_SMOOI(arg));
			if (!HAK_OOP_IS_CHAR(arg)) goto invalid_format;
			ooch = HAK_OOP_TO_CHAR(arg);

		print_char:
			/* zeropad must not take effect for 'c' */
			if (flagc & FLAGC_ZEROPAD) padc = ' ';

			/* precision 0 doesn't kill the letter */
			width--;
			if (!(flagc & FLAGC_LEFTADJ) && width > 0) PUT_OOCH(hak, fmtout, padc, width);
			PUT_OOCH(hak, fmtout, ooch, 1);
			if ((flagc & FLAGC_LEFTADJ) && width > 0) PUT_OOCH(hak, fmtout, padc, width);
			break;

		case 's':
		case 'S':
		{
			/* zeropad must not take effect for 'S' */
			if (flagc & FLAGC_ZEROPAD) padc = ' ';

			GET_NEXT_ARG_TO(hak, nargs, &arg_state, arg);
			if (!HAK_OOP_IS_POINTER(arg)) goto invalid_format;
			switch (HAK_OBJ_GET_FLAGS_TYPE(arg))
			{
				case HAK_OBJ_TYPE_CHAR:
				{
					/* string, symbol */
					const hak_ooch_t* oosp;
					hak_oow_t oosl;

					oosp = HAK_OBJ_GET_CHAR_SLOT(arg);
					oosl = HAK_OBJ_GET_SIZE(arg);

					if (flagc & FLAGC_DOT)
					{
						if (oosl > precision) oosl = precision;
					}
					width -= oosl;

					if (!(flagc & FLAGC_LEFTADJ) && width > 0) PUT_OOCH(hak, fmtout, padc, width);
					PUT_OOCS(hak, fmtout, oosp, oosl);
					if ((flagc & FLAGC_LEFTADJ) && width > 0) PUT_OOCH(hak, fmtout, padc, width);
					break;
				}

				case HAK_OBJ_TYPE_BYTE:
				{
					/* byte array */
					const hak_uint8_t* bsp;
					hak_oow_t bsl;

					bsp = HAK_OBJ_GET_BYTE_SLOT(arg);
					bsl = HAK_OBJ_GET_SIZE(arg);

					if (flagc & FLAGC_DOT)
					{
						if (bsl > precision) bsl = precision;
					}
					width -= bsl;

					if (!(flagc & FLAGC_LEFTADJ) && width > 0) PUT_OOCH(hak, fmtout, padc, width);
					PUT_BCS(hak, fmtout, (const hak_bch_t*)bsp, bsl);
					if ((flagc & FLAGC_LEFTADJ) && width > 0) PUT_OOCH(hak, fmtout, padc, width);
					break;
				}

				default:
					goto invalid_format;
			}

			break;
		}

	#if !defined(HAK_OOCH_IS_UCH)
		case 'w': /* the string object is not in the unicode encoding */
		case 'W': /* treat w/W like k/K */
	#endif
		case 'k':
		case 'K':
		{
			const hak_uint8_t* bsp;
			hak_oow_t bsl, k_hex_width;

			GET_NEXT_ARG_TO(hak, nargs, &arg_state, arg);
			if (!HAK_OOP_IS_POINTER(arg)) goto invalid_format;

			if (flagc & FLAGC_ZEROPAD) padc = ' ';

			switch (HAK_OBJ_GET_FLAGS_TYPE(arg))
			{
				case HAK_OBJ_TYPE_CHAR:
					bsp = (const hak_uint8_t*)HAK_OBJ_GET_CHAR_SLOT(arg);
					bsl = HAK_OBJ_GET_SIZE(arg) * HAK_SIZEOF_OOCH_T;
					goto format_byte_in_k;

				case HAK_OBJ_TYPE_BYTE:
					bsp = HAK_OBJ_GET_BYTE_SLOT(arg);
					bsl = HAK_OBJ_GET_SIZE(arg);

				format_byte_in_k:
					k_hex_width = (lm_flag & (LF_H | LF_L))? 4: 2;

					if (flagc & FLAGC_DOT)
					{
						n = (precision > bsl)? bsl: precision;
					}
					else n = bsl;

					if (lm_flag & LF_H)
					{
						hak_oow_t i;
						for (i = 0; i < n; i++) width -= BYTE_PRINTABLE(bsp[i])? 1: k_hex_width;
					}
					else
					{
						width -= (n * k_hex_width);
					}

					if (!(flagc & FLAGC_LEFTADJ) && width > 0) PUT_OOCH(hak, fmtout, padc, width);

					while (n--)
					{
						if ((lm_flag & LF_H) && BYTE_PRINTABLE(*bsp))
						{
							PUT_BCH(hak, fmtout, *bsp, 1);
						}
						else
						{
							hak_bch_t xbuf[3];
							int flagged_radix = 16;
						#if defined(HAK_OOCH_IS_UCH)
							if (ch == 'k') flagged_radix |= HAK_BYTE_TO_BCSTR_LOWERCASE;
						#else
							if (ch == 'k' || ch == 'w') flagged_radix |= HAK_BYTE_TO_BCSTR_LOWERCASE;
						#endif
							hak_byte_to_bcstr (*bsp, xbuf, HAK_COUNTOF(xbuf), flagged_radix, '0');
							if (lm_flag & (LF_H | LF_L)) PUT_BCS(hak, fmtout, "\\x", 2);
							PUT_BCS(hak, fmtout, xbuf, 2);
						}
						bsp++;
					}

					if ((flagc & FLAGC_LEFTADJ) && width > 0) PUT_OOCH(hak, fmtout, padc, width);
					break;

				default:
					goto invalid_format;
			}
			break;
		}

	#if defined(HAK_OOCH_IS_UCH)
		case 'w':
		case 'W':
		{
			/* unicode string in unicode escape sequence.
			 *
			 * hw -> \uXXXX, \UXXXXXXXX, printable-byte(only in ascii range)
			 * w -> \uXXXX, \UXXXXXXXX
			 * lw -> all in \UXXXXXXXX
			 */
			const hak_uch_t* usp;
			hak_oow_t usl, i, uwid;

			GET_NEXT_ARG_TO(hak, nargs, &arg_state, arg);
			if (!HAK_OOP_IS_POINTER(arg) || HAK_OBJ_GET_FLAGS_TYPE(arg) != HAK_OBJ_TYPE_CHAR) goto invalid_format;

			if (flagc & FLAGC_ZEROPAD) padc = ' ';

			usp = HAK_OBJ_GET_CHAR_SLOT(arg);
			usl = HAK_OBJ_GET_SIZE(arg);

			if (flagc & FLAGC_DOT)
			{
				n = (precision > usl)? usl: precision;
			}
			else n = usl;

			for (i = 0; i < n; i++)
			{
				if ((lm_flag & LF_H) && BYTE_PRINTABLE(usp[n])) uwid = 1;
				else if (!(lm_flag & LF_L) && usp[n] <= 0xFFFF) uwid = 6;
				else uwid = 10;
				width -= uwid;
			}

			if (!(flagc & FLAGC_LEFTADJ) && width > 0) PUT_OOCH(hak, fmtout, padc, width);

			while (n--)
			{
				if ((lm_flag & LF_H) && BYTE_PRINTABLE(*usp))
				{
					PUT_OOCH(hak, fmtout, *usp, 1);
				}
				else if (!(lm_flag & LF_L) && *usp <= 0xFFFF)
				{
					hak_uint16_t u16 = *usp;
					int extra_flags = ((ch) == 'w'? HAK_BYTE_TO_BCSTR_LOWERCASE: 0);
					PUT_BCS(hak, fmtout, "\\u", 2);
					PUT_BYTE_IN_HEX(hak, fmtout, (u16 >> 8) & 0xFF, extra_flags);
					PUT_BYTE_IN_HEX(hak, fmtout, u16 & 0xFF, extra_flags);
				}
				else
				{
					hak_uint32_t u32 = *usp;
					int extra_flags = ((ch) == 'w'? HAK_BYTE_TO_BCSTR_LOWERCASE: 0);
					PUT_BCS(hak, fmtout, "\\u", 2);
					PUT_BYTE_IN_HEX(hak, fmtout, (u32 >> 24) & 0xFF, extra_flags);
					PUT_BYTE_IN_HEX(hak, fmtout, (u32 >> 16) & 0xFF, extra_flags);
					PUT_BYTE_IN_HEX(hak, fmtout, (u32 >> 8) & 0xFF, extra_flags);
					PUT_BYTE_IN_HEX(hak, fmtout, u32 & 0xFF, extra_flags);
				}
				usp++;
			}

			if ((flagc & FLAGC_LEFTADJ) && width > 0) PUT_OOCH(hak, fmtout, padc, width);
			break;
		}
	#endif

		case 'O': /* object - ignore precision, width, adjustment */
			GET_NEXT_ARG_TO(hak, nargs, &arg_state, arg);
			if (fmtout->putobj(hak, fmtout, arg) <= -1) goto oops;
			break;

		case 'J':
		{
			hak_bitmask_t tmp;
			GET_NEXT_ARG_TO(hak, nargs, &arg_state, arg);
			tmp = fmtout->mask;
			fmtout->mask |= HAK_LOG_PREFER_JSON;
			if (fmtout->putobj(hak, fmtout, arg) <= -1) goto oops;
			fmtout->mask = tmp;
			break;
		}

		print_integer:
		{
			const hak_ooch_t* nsptr;
			hak_oow_t nslen;

			GET_NEXT_ARG_TO(hak, nargs, &arg_state, arg);
			if (HAK_OOP_IS_CHAR(arg))
			{
				arg = HAK_SMOOI_TO_OOP(HAK_OOP_TO_CHAR(arg));
			}
			else if (HAK_IS_FPDEC(hak, arg))
			{
				hak_oop_t nv;
				hak_oop_fpdec_t fa = (hak_oop_fpdec_t)arg;

				/* the given number for integer output is a fixed-point decimal.
				 * i will drop all digits after the fixed point */
				hak_pushvolat(hak, &arg);
				nv = hak_truncfpdecval(hak, fa->value, HAK_OOP_TO_SMOOI(fa->scale), 0);
				hak_popvolat (hak);
				if (!nv)
				{
					HAK_LOG1 (hak, HAK_LOG_WARN | HAK_LOG_UNTYPED, "unable to truncate a fixed-point number %O to an integer for output\n", arg);
					goto invalid_format;
				}

				arg = nv;
			}

			if (!hak_inttostr(hak, arg, radix | radix_flags))
			{
				/*hak_seterrbfmt(hak, HAK_EINVAL, "not a valid number - %O", arg);
				goto oops;*/
				HAK_LOG2 (hak, HAK_LOG_WARN | HAK_LOG_UNTYPED, "unable to convert %O for integer output - %js\n", arg, hak_geterrmsg(hak));
				goto invalid_format;
			}

			nsptr = hak->inttostr.xbuf.ptr;
			nslen = hak->inttostr.xbuf.len;

			HAK_ASSERT(hak, nslen > 0);
			if (nsptr[0] == '-')
			{
				/* a negative number was given. i must skip the minus sign
				 * added by hak_inttostr() for a negative number. */
				HAK_ASSERT(hak, (HAK_OOP_IS_SMOOI(arg) && HAK_OOP_TO_SMOOI(arg) < 0) || HAK_IS_NBIGINT(hak,arg));
				nsptr++;
				nslen--;
			}

			extra = nslen;
			if (sign && ((HAK_OOP_IS_SMOOI(arg) && HAK_OOP_TO_SMOOI(arg) < 0) || HAK_IS_NBIGINT(hak,arg))) neg = 1;

			if ((flagc & FLAGC_SHARP) && arg != HAK_SMOOI_TO_OOP(0))
			{
				/* #b #o #x */
				if (radix == 2 || radix == 8 || radix == 16) extra += 2;
			}
			if (neg) extra++;
			else if (flagc & FLAGC_SIGN) extra++;
			else if (flagc & FLAGC_SPACE) extra++;

			if ((flagc & FLAGC_DOT) && precision > nslen)
			{
				/* extra zeros for precision specified */
				extra += (precision - nslen);
			}

			if (!(flagc & FLAGC_LEFTADJ) && !(flagc & FLAGC_ZEROPAD) && width > 0 && (width -= extra) > 0)
			{
				PUT_OOCH(hak, fmtout, padc, width);
				width = 0;
			}

			if (neg) PUT_OOCH(hak, fmtout, '-', 1);
			else if (flagc & FLAGC_SIGN) PUT_OOCH(hak, fmtout, '+', 1);
			else if (flagc & FLAGC_SPACE) PUT_OOCH(hak, fmtout, ' ', 1);

			if ((flagc & FLAGC_SHARP) && arg != HAK_SMOOI_TO_OOP(0))
			{
				if (radix == 2)
				{
					PUT_OOCH(hak, fmtout, '0', 1);
					PUT_OOCH(hak, fmtout, 'b', 1);
				}
				if (radix == 8)
				{
					PUT_OOCH(hak, fmtout, '0', 1);
					PUT_OOCH(hak, fmtout, 'o', 1);
				}
				else if (radix == 16)
				{
					PUT_OOCH(hak, fmtout, '0', 1);
					PUT_OOCH(hak, fmtout, 'x', 1);
				}
			}

			if ((flagc & FLAGC_DOT) && precision > nslen)
			{
				/* extra zeros for precision specified */
				PUT_OOCH(hak, fmtout, '0', precision - nslen);
			}

			if (!(flagc & FLAGC_LEFTADJ) && width > 0 && (width -= extra) > 0)
			{
				PUT_OOCH(hak, fmtout, padc, width);
			}

			PUT_OOCS(hak, fmtout, nsptr, nslen);

			if ((flagc & FLAGC_LEFTADJ) && width > 0 && (width -= extra) > 0)
			{
				PUT_OOCH(hak, fmtout, padc, width);
			}
			break;
		}

		invalid_format:
			PUT_OOCS(hak, fmtout, percent, fmtptr - percent);
			break;

		default:
			PUT_OOCS(hak, fmtout, percent, fmtptr - percent);
			/*
			 * Since we ignore an formatting argument it is no
			 * longer safe to obey the remaining formatting
			 * arguments as the arguments will no longer match
			 * the format specs.
			 */
			arg_state.stop = 1;
			break;
		}
	}

done:
	return 0;

oops:
	return -1;
}

int hak_strfmtcallstack (hak_t* hak, hak_ooi_t nargs)
{
	/* format a string using the receiver and arguments on the stack */
	hak_fmtout_t fo;

	HAK_MEMSET(&fo, 0, HAK_SIZEOF(fo));
	fo.putbchars = sprint_bcs;
	fo.putuchars = sprint_ucs;
	fo.putobj = hak_fmt_object;
	/* format_stack_args doesn't use fmt_str and fmt_type.
	 * it takes the format string from the stack. */

	hak->sprintf.xbuf.len = 0;
	return format_stack_args(hak, &fo, nargs, 0);
}

int hak_prfmtcallstack (hak_t* hak, hak_ooi_t nargs)
{
	/* format a string using the receiver and arguments on the stack */
	hak_fmtout_t fo;

	HAK_MEMSET(&fo, 0, HAK_SIZEOF(fo));
	fo.mask = 0;
	fo.mmgr = HAK_MMGR(hak);
	fo.putbchars = print_bcs;
	fo.putuchars = print_ucs;
	fo.putobj = hak_fmt_object;
	/* format_stack_args doesn't use fmt_str and fmt_type.
	 * it takes the format string from the stack. */
	return format_stack_args(hak, &fo, nargs, 0);
}

int hak_logfmtcallstack (hak_t* hak, hak_ooi_t nargs)
{
	/* format a string using the receiver and arguments on the stack */
	hak_fmtout_t fo;

	HAK_MEMSET(&fo, 0, HAK_SIZEOF(fo));

	fo.mask = HAK_LOG_FATAL | HAK_LOG_APP;
	if (hak->log.default_type_mask & HAK_LOG_ALL_TYPES)
	{
		/* if a type is given, it's not untyped any more.
		 * mask off the UNTYPED bit */
		fo.mask &= ~HAK_LOG_UNTYPED;

		/* if the default_type_mask has the UNTYPED bit on,
		 * it'll get turned back on */
		fo.mask |= (hak->log.default_type_mask & HAK_LOG_ALL_TYPES);
	}

	fo.mmgr = HAK_MMGR(hak);
	fo.putbchars = log_bcs;
	fo.putuchars = log_ucs;
	fo.putobj = hak_fmt_object;
	/* format_stack_args doesn't use fmt_str and fmt_type.
	 * it takes the format string from the stack. */

	return format_stack_args(hak, &fo, nargs, 0);
}

/* --------------------------------------------------------------------------
 * FORMATTED INPUT
 * -------------------------------------------------------------------------- */


static int read_bcs (hak_t* hak, hak_fmtin_t* fmtout, hak_bch_t* buf, hak_oow_t len)
{
	if (HAK_UNLIKELY(!hak->io.udo_wrtr))
	{
		hak_seterrbmsg(hak, HAK_EINVAL, "no user-defined output handler");
		return -1;
	}

	return 0;
}

static int read_ucs (hak_t* hak, hak_fmtin_t* fmtin, hak_uch_t* buf, hak_oow_t len)
{
	if (HAK_UNLIKELY(!hak->io.udo_wrtr))
	{
		hak_seterrbmsg(hak, HAK_EINVAL, "no user-defined output handler");
		return -1;
	}

	return 0;
}

static HAK_INLINE int fmtin_stack_args (hak_t* hak, hak_fmtin_t* fmtin, hak_ooi_t nargs, int rcv_is_fmtstr)
{
	/* TODO: */
	return 0;
}

int hak_scfmtcallstack (hak_t* hak, hak_ooi_t nargs)
{
	hak_fmtin_t fi;

	HAK_MEMSET(&fi, 0, HAK_SIZEOF(fi));
	/*
	 * TODO:
	fi.getbchars =
	fi.getuchars =
	*/

	return fmtin_stack_args(hak, &fi, nargs, 0);
}

/* --------------------------------------------------------------------------
 * DYNAMIC STRING FORMATTING
 * -------------------------------------------------------------------------- */

struct fmt_uch_buf_t
{
	hak_t* hak;
	hak_uch_t* ptr;
	hak_oow_t len;
	hak_oow_t capa;
};
typedef struct fmt_uch_buf_t fmt_uch_buf_t;

static int fmt_put_bchars_to_uch_buf (hak_t* hak, hak_fmtout_t* fmtout, const hak_bch_t* ptr, hak_oow_t len)
{
	fmt_uch_buf_t* b = (fmt_uch_buf_t*)fmtout->ctx;
	hak_oow_t bcslen, ucslen;
	int n;

	bcslen = len;
	ucslen = b->capa - b->len;
	n = hak_conv_bchars_to_uchars_with_cmgr(ptr, &bcslen, &b->ptr[b->len], &ucslen, b->hak->_cmgr, 1);
	b->len += ucslen;
	if (n <= -1)
	{
		if (n == -2)
		{
			return 0; /* buffer full. stop */
		}
		else
		{
			hak_seterrnum (b->hak, HAK_EECERR);
			return -1;
		}
	}

	return 1; /* success. carry on */
}

static int fmt_put_uchars_to_uch_buf (hak_t* hak, hak_fmtout_t* fmtout, const hak_uch_t* ptr, hak_oow_t len)
{
	fmt_uch_buf_t* b = (fmt_uch_buf_t*)fmtout->ctx;
	hak_oow_t n;

	/* this function null-terminates the destination. so give the restored buffer size */
	n = hak_copy_uchars_to_ucstr(&b->ptr[b->len], b->capa - b->len + 1, ptr, len);
	b->len += n;
	if (n < len)
	{
		hak_seterrnum (b->hak, HAK_EBUFFULL);
		return 0; /* stop. insufficient buffer */
	}

	return 1; /* success */
}

hak_oow_t hak_vfmttoucstr (hak_t* hak, hak_uch_t* buf, hak_oow_t bufsz, const hak_uch_t* fmt, va_list ap)
{
	hak_fmtout_t fo;
	fmt_uch_buf_t fb;

	if (bufsz <= 0) return 0;

	HAK_MEMSET(&fo, 0, HAK_SIZEOF(fo));
	fo.mmgr = hak->_mmgr;
	fo.putbchars = fmt_put_bchars_to_uch_buf;
	fo.putuchars = fmt_put_uchars_to_uch_buf;
	fo.putobj = hak_fmt_object;
	fo.ctx = &fb;

	HAK_MEMSET(&fb, 0, HAK_SIZEOF(fb));
	fb.hak = hak;
	fb.ptr = buf;
	fb.capa = bufsz - 1;

	if (hak_ufmt_outv(hak, &fo, fmt, ap) <= -1) return -1;

	buf[fb.len] = '\0';
	return fb.len;
}

hak_oow_t hak_fmttoucstr (hak_t* hak, hak_uch_t* buf, hak_oow_t bufsz, const hak_uch_t* fmt, ...)
{
	hak_oow_t x;
	va_list ap;

	va_start (ap, fmt);
	x = hak_vfmttoucstr(hak, buf, bufsz, fmt, ap);
	va_end (ap);

	return x;
}

/* ------------------------------------------------------------------------ */

struct fmt_bch_buf_t
{
	hak_t* hak;
	hak_bch_t* ptr;
	hak_oow_t len;
	hak_oow_t capa;
};
typedef struct fmt_bch_buf_t fmt_bch_buf_t;

static int fmt_put_bchars_to_bch_buf (hak_t* hak, hak_fmtout_t* fmtout, const hak_bch_t* ptr, hak_oow_t len)
{
	fmt_bch_buf_t* b = (fmt_bch_buf_t*)fmtout->ctx;
	hak_oow_t n;

	/* this function null-terminates the destination. so give the restored buffer size */
	n = hak_copy_bchars_to_bcstr(&b->ptr[b->len], b->capa - b->len + 1, ptr, len);
	b->len += n;
	if (n < len)
	{
		hak_seterrnum (b->hak, HAK_EBUFFULL);
		return 0; /* stop. insufficient buffer */
	}

	return 1; /* success */
}

static int fmt_put_uchars_to_bch_buf (hak_t* hak, hak_fmtout_t* fmtout, const hak_uch_t* ptr, hak_oow_t len)
{
	fmt_bch_buf_t* b = (fmt_bch_buf_t*)fmtout->ctx;
	hak_oow_t bcslen, ucslen;
	int n;

	bcslen = b->capa - b->len;
	ucslen = len;
	n = hak_conv_uchars_to_bchars_with_cmgr(ptr, &ucslen, &b->ptr[b->len], &bcslen, b->hak->_cmgr);
	b->len += bcslen;
	if (n <= -1)
	{
		if (n == -2)
		{
			return 0; /* buffer full. stop */
		}
		else
		{
			hak_seterrnum (b->hak, HAK_EECERR);
			return -1;
		}
	}

	return 1; /* success. carry on */
}

hak_oow_t hak_vfmttobcstr (hak_t* hak, hak_bch_t* buf, hak_oow_t bufsz, const hak_bch_t* fmt, va_list ap)
{
	hak_fmtout_t fo;
	fmt_bch_buf_t fb;

	if (bufsz <= 0) return 0;

	HAK_MEMSET(&fo, 0, HAK_SIZEOF(fo));
	fo.mmgr = hak->_mmgr;
	fo.putbchars = fmt_put_bchars_to_bch_buf;
	fo.putuchars = fmt_put_uchars_to_bch_buf;
	fo.putobj = hak_fmt_object;
	fo.ctx = &fb;

	HAK_MEMSET(&fb, 0, HAK_SIZEOF(fb));
	fb.hak = hak;
	fb.ptr = buf;
	fb.capa = bufsz - 1;

	if (hak_bfmt_outv(hak, &fo, fmt, ap) <= -1) return -1;

	buf[fb.len] = '\0';
	return fb.len;
}

hak_oow_t hak_fmttobcstr (hak_t* hak, hak_bch_t* buf, hak_oow_t bufsz, const hak_bch_t* fmt, ...)
{
	hak_oow_t x;
	va_list ap;

	va_start (ap, fmt);
	x = hak_vfmttobcstr(hak, buf, bufsz, fmt, ap);
	va_end (ap);

	return x;
}
