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

#ifndef _HCL_CHR_H_
#define _HCL_CHR_H_

#include <hcl-cmn.h>

enum hcl_ooch_prop_t
{
	HCL_OOCH_PROP_UPPER  = (1 << 0),
#define HCL_UCH_PROP_UPPER HCL_OOCH_PROP_UPPER
#define HCL_BCH_PROP_UPPER HCL_OOCH_PROP_UPPER

	HCL_OOCH_PROP_LOWER  = (1 << 1),
#define HCL_UCH_PROP_LOWER HCL_OOCH_PROP_LOWER
#define HCL_BCH_PROP_LOWER HCL_OOCH_PROP_LOWER

	HCL_OOCH_PROP_ALPHA  = (1 << 2),
#define HCL_UCH_PROP_ALPHA HCL_OOCH_PROP_ALPHA
#define HCL_BCH_PROP_ALPHA HCL_OOCH_PROP_ALPHA

	HCL_OOCH_PROP_DIGIT  = (1 << 3),
#define HCL_UCH_PROP_DIGIT HCL_OOCH_PROP_DIGIT
#define HCL_BCH_PROP_DIGIT HCL_OOCH_PROP_DIGIT

	HCL_OOCH_PROP_XDIGIT = (1 << 4),
#define HCL_UCH_PROP_XDIGIT HCL_OOCH_PROP_XDIGIT
#define HCL_BCH_PROP_XDIGIT HCL_OOCH_PROP_XDIGIT

	HCL_OOCH_PROP_ALNUM  = (1 << 5),
#define HCL_UCH_PROP_ALNUM HCL_OOCH_PROP_XDIGIT
#define HCL_BCH_PROP_ALNUM HCL_OOCH_PROP_XDIGIT

	HCL_OOCH_PROP_SPACE  = (1 << 6),
#define HCL_UCH_PROP_SPACE HCL_OOCH_PROP_SPACE
#define HCL_BCH_PROP_SPACE HCL_OOCH_PROP_SPACE

	HCL_OOCH_PROP_PRINT  = (1 << 8),
#define HCL_UCH_PROP_PRINT HCL_OOCH_PROP_PRINT
#define HCL_BCH_PROP_PRINT HCL_OOCH_PROP_PRINT

	HCL_OOCH_PROP_GRAPH  = (1 << 9),
#define HCL_UCH_PROP_GRAPH HCL_OOCH_PROP_GRAPH
#define HCL_BCH_PROP_GRAPH HCL_OOCH_PROP_GRAPH

	HCL_OOCH_PROP_CNTRL  = (1 << 10),
#define HCL_UCH_PROP_CNTRL HCL_OOCH_PROP_CNTRL
#define HCL_BCH_PROP_CNTRL HCL_OOCH_PROP_CNTRL

	HCL_OOCH_PROP_PUNCT  = (1 << 11),
#define HCL_UCH_PROP_PUNCT HCL_OOCH_PROP_PUNCT
#define HCL_BCH_PROP_PUNCT HCL_OOCH_PROP_PUNCT

	HCL_OOCH_PROP_BLANK  = (1 << 12)
#define HCL_UCH_PROP_BLANK HCL_OOCH_PROP_BLANK
#define HCL_BCH_PROP_BLANK HCL_OOCH_PROP_BLANK
};

typedef enum hcl_ooch_prop_t hcl_ooch_prop_t;
typedef enum hcl_ooch_prop_t hcl_uch_prop_t;
typedef enum hcl_ooch_prop_t hcl_bch_prop_t;

#if defined(__cplusplus)
extern "C" {
#endif

HCL_EXPORT int hcl_is_uch_type (hcl_uch_t c, hcl_uch_prop_t type);
HCL_EXPORT int hcl_is_uch_upper (hcl_uch_t c);
HCL_EXPORT int hcl_is_uch_lower (hcl_uch_t c);
HCL_EXPORT int hcl_is_uch_alpha (hcl_uch_t c);
HCL_EXPORT int hcl_is_uch_digit (hcl_uch_t c);
HCL_EXPORT int hcl_is_uch_xdigit (hcl_uch_t c);
HCL_EXPORT int hcl_is_uch_alnum (hcl_uch_t c);
HCL_EXPORT int hcl_is_uch_space (hcl_uch_t c);
HCL_EXPORT int hcl_is_uch_print (hcl_uch_t c);
HCL_EXPORT int hcl_is_uch_graph (hcl_uch_t c);
HCL_EXPORT int hcl_is_uch_cntrl (hcl_uch_t c);
HCL_EXPORT int hcl_is_uch_punct (hcl_uch_t c);
HCL_EXPORT int hcl_is_uch_blank (hcl_uch_t c);
HCL_EXPORT hcl_uch_t hcl_to_uch_upper (hcl_uch_t c);
HCL_EXPORT hcl_uch_t hcl_to_uch_lower (hcl_uch_t c);


/* ------------------------------------------------------------------------- */

HCL_EXPORT int hcl_is_bch_type (hcl_bch_t c, hcl_bch_prop_t type);

#if defined(__has_builtin)
#	if __has_builtin(__builtin_isupper)
#		define hcl_is_bch_upper __builtin_isupper
#	endif
#	if __has_builtin(__builtin_islower)
#		define hcl_is_bch_lower __builtin_islower
#	endif
#	if __has_builtin(__builtin_isalpha)
#		define hcl_is_bch_alpha __builtin_isalpha
#	endif
#	if __has_builtin(__builtin_isdigit)
#		define hcl_is_bch_digit __builtin_isdigit
#	endif
#	if __has_builtin(__builtin_isxdigit)
#		define hcl_is_bch_xdigit __builtin_isxdigit
#	endif
#	if __has_builtin(__builtin_isalnum)
#		define hcl_is_bch_alnum __builtin_isalnum
#	endif
#	if __has_builtin(__builtin_isspace)
#		define hcl_is_bch_space __builtin_isspace
#	endif
#	if __has_builtin(__builtin_isprint)
#		define hcl_is_bch_print __builtin_isprint
#	endif
#	if __has_builtin(__builtin_isgraph)
#		define hcl_is_bch_graph __builtin_isgraph
#	endif
#	if __has_builtin(__builtin_iscntrl)
#		define hcl_is_bch_cntrl __builtin_iscntrl
#	endif
#	if __has_builtin(__builtin_ispunct)
#		define hcl_is_bch_punct __builtin_ispunct
#	endif
#	if __has_builtin(__builtin_isblank)
#		define hcl_is_bch_blank __builtin_isblank
#	endif
#	if __has_builtin(__builtin_toupper)
#		define hcl_to_bch_upper __builtin_toupper
#	endif
#	if __has_builtin(__builtin_tolower)
#		define hcl_to_bch_lower __builtin_tolower
#	endif
#elif (__GNUC__ >= 4) 
#	define hcl_is_bch_upper __builtin_isupper
#	define hcl_is_bch_lower __builtin_islower
#	define hcl_is_bch_alpha __builtin_isalpha
#	define hcl_is_bch_digit __builtin_isdigit
#	define hcl_is_bch_xdigit __builtin_isxdigit
#	define hcl_is_bch_alnum __builtin_isalnum
#	define hcl_is_bch_space __builtin_isspace
#	define hcl_is_bch_print __builtin_isprint
#	define hcl_is_bch_graph __builtin_isgraph
#	define hcl_is_bch_cntrl __builtin_iscntrl
#	define hcl_is_bch_punct __builtin_ispunct
#	define hcl_is_bch_blank __builtin_isblank
#	define hcl_to_bch_upper __builtin_toupper
#	define hcl_to_bch_lower __builtin_tolower
#endif

/* the bch class functions support no locale.
 * these implemenent latin-1 only */

#if !defined(hcl_is_bch_upper) && defined(HCL_HAVE_INLINE)
static HCL_INLINE int hcl_is_bch_upper (hcl_bch_t c) { return (hcl_bcu_t)c - 'A' < 26; }
#elif !defined(hcl_is_bch_upper)
#	define hcl_is_bch_upper(c) ((hcl_bcu_t)(c) - 'A' < 26)
#endif

#if !defined(hcl_is_bch_lower) && defined(HCL_HAVE_INLINE)
static HCL_INLINE int hcl_is_bch_lower (hcl_bch_t c) { return (hcl_bcu_t)c - 'a' < 26; }
#elif !defined(hcl_is_bch_lower)
#	define hcl_is_bch_lower(c) ((hcl_bcu_t)(c) - 'a' < 26)
#endif

#if !defined(hcl_is_bch_alpha) && defined(HCL_HAVE_INLINE)
static HCL_INLINE int hcl_is_bch_alpha (hcl_bch_t c) { return ((hcl_bcu_t)c | 32) - 'a' < 26; }
#elif !defined(hcl_is_bch_alpha)
#	define hcl_is_bch_alpha(c) (((hcl_bcu_t)(c) | 32) - 'a' < 26)
#endif

#if !defined(hcl_is_bch_digit) && defined(HCL_HAVE_INLINE)
static HCL_INLINE int hcl_is_bch_digit (hcl_bch_t c) { return (hcl_bcu_t)c - '0' < 10; }
#elif !defined(hcl_is_bch_digit)
#	define hcl_is_bch_digit(c) ((hcl_bcu_t)(c) - '0' < 10)
#endif

#if !defined(hcl_is_bch_xdigit) && defined(HCL_HAVE_INLINE)
static HCL_INLINE int hcl_is_bch_xdigit (hcl_bch_t c) { return hcl_is_bch_digit(c) || ((hcl_bcu_t)c | 32) - 'a' < 6; }
#elif !defined(hcl_is_bch_xdigit)
#	define hcl_is_bch_xdigit(c) (hcl_is_bch_digit(c) || ((hcl_bcu_t)(c) | 32) - 'a' < 6)
#endif

#if !defined(hcl_is_bch_alnum) && defined(HCL_HAVE_INLINE)
static HCL_INLINE int hcl_is_bch_alnum (hcl_bch_t c) { return hcl_is_bch_alpha(c) || hcl_is_bch_digit(c); }
#elif !defined(hcl_is_bch_alnum)
#	define hcl_is_bch_alnum(c) (hcl_is_bch_alpha(c) || hcl_is_bch_digit(c))
#endif

#if !defined(hcl_is_bch_space) && defined(HCL_HAVE_INLINE)
static HCL_INLINE int hcl_is_bch_space (hcl_bch_t c) { return c == ' ' || (hcl_bcu_t)c - '\t' < 5; }
#elif !defined(hcl_is_bch_space)
#	define hcl_is_bch_space(c) ((c) == ' ' || (hcl_bcu_t)(c) - '\t' < 5)
#endif

#if !defined(hcl_is_bch_print) && defined(HCL_HAVE_INLINE)
static HCL_INLINE int hcl_is_bch_print (hcl_bch_t c) { return (hcl_bcu_t)c - ' ' < 95; }
#elif !defined(hcl_is_bch_print)
#	define hcl_is_bch_print(c) ((hcl_bcu_t)(c) - ' ' < 95)
#endif

#if !defined(hcl_is_bch_graph) && defined(HCL_HAVE_INLINE)
static HCL_INLINE int hcl_is_bch_graph (hcl_bch_t c) { return (hcl_bcu_t)c - '!' < 94; }
#elif !defined(hcl_is_bch_graph)
#	define hcl_is_bch_graph(c) ((hcl_bcu_t)(c) - '!' < 94)
#endif

#if !defined(hcl_is_bch_cntrl) && defined(HCL_HAVE_INLINE)
static HCL_INLINE int hcl_is_bch_cntrl (hcl_bch_t c) { return (hcl_bcu_t)c < ' ' || (hcl_bcu_t)c == 127; }
#elif !defined(hcl_is_bch_cntrl)
#	define hcl_is_bch_cntrl(c) ((hcl_bcu_t)(c) < ' ' || (hcl_bcu_t)(c) == 127)
#endif

#if !defined(hcl_is_bch_punct) && defined(HCL_HAVE_INLINE)
static HCL_INLINE int hcl_is_bch_punct (hcl_bch_t c) { return hcl_is_bch_graph(c) && !hcl_is_bch_alnum(c); }
#elif !defined(hcl_is_bch_punct)
#	define hcl_is_bch_punct(c) (hcl_is_bch_graph(c) && !hcl_is_bch_alnum(c))
#endif

#if !defined(hcl_is_bch_blank) && defined(HCL_HAVE_INLINE)
static HCL_INLINE int hcl_is_bch_blank (hcl_bch_t c) { return c == ' ' || c == '\t'; }
#elif !defined(hcl_is_bch_blank)
#	define hcl_is_bch_blank(c) ((c) == ' ' || (c) == '\t')
#endif

#if !defined(hcl_to_bch_upper)
HCL_EXPORT hcl_bch_t hcl_to_bch_upper (hcl_bch_t c);
#endif
#if !defined(hcl_to_bch_lower)
HCL_EXPORT hcl_bch_t hcl_to_bch_lower (hcl_bch_t c);
#endif

#if defined(HCL_OOCH_IS_UCH)
#	define hcl_is_ooch_type hcl_is_uch_type
#	define hcl_is_ooch_upper hcl_is_uch_upper
#	define hcl_is_ooch_lower hcl_is_uch_lower
#	define hcl_is_ooch_alpha hcl_is_uch_alpha
#	define hcl_is_ooch_digit hcl_is_uch_digit
#	define hcl_is_ooch_xdigit hcl_is_uch_xdigit
#	define hcl_is_ooch_alnum hcl_is_uch_alnum
#	define hcl_is_ooch_space hcl_is_uch_space
#	define hcl_is_ooch_print hcl_is_uch_print
#	define hcl_is_ooch_graph hcl_is_uch_graph
#	define hcl_is_ooch_cntrl hcl_is_uch_cntrl
#	define hcl_is_ooch_punct hcl_is_uch_punct
#	define hcl_is_ooch_blank hcl_is_uch_blank
#	define hcl_to_ooch_upper hcl_to_uch_upper
#	define hcl_to_ooch_lower hcl_to_uch_lower
#else
#	define hcl_is_ooch_type hcl_is_bch_type
#	define hcl_is_ooch_upper hcl_is_bch_upper
#	define hcl_is_ooch_lower hcl_is_bch_lower
#	define hcl_is_ooch_alpha hcl_is_bch_alpha
#	define hcl_is_ooch_digit hcl_is_bch_digit
#	define hcl_is_ooch_xdigit hcl_is_bch_xdigit
#	define hcl_is_ooch_alnum hcl_is_bch_alnum
#	define hcl_is_ooch_space hcl_is_bch_space
#	define hcl_is_ooch_print hcl_is_bch_print
#	define hcl_is_ooch_graph hcl_is_bch_graph
#	define hcl_is_ooch_cntrl hcl_is_bch_cntrl
#	define hcl_is_ooch_punct hcl_is_bch_punct
#	define hcl_is_ooch_blank hcl_is_bch_blank
#	define hcl_to_ooch_upper hcl_to_bch_upper
#	define hcl_to_ooch_lower hcl_to_bch_lower
#endif

/* ------------------------------------------------------------------------- */

HCL_EXPORT int hcl_get_ucwidth (
        hcl_uch_t uc
);

/* ------------------------------------------------------------------------- */

HCL_EXPORT hcl_oow_t hcl_uc_to_utf8 (
	hcl_uch_t    uc,
	hcl_bch_t*   utf8,
	hcl_oow_t    size
);

HCL_EXPORT hcl_oow_t hcl_utf8_to_uc (
	const hcl_bch_t* utf8,
	hcl_oow_t        size,
	hcl_uch_t*       uc
);


HCL_EXPORT hcl_oow_t hcl_uc_to_utf16 (
	hcl_uch_t    uc,
	hcl_bch_t*   utf16,
	hcl_oow_t    size
);

HCL_EXPORT hcl_oow_t hcl_utf16_to_uc (
	const hcl_bch_t* utf16,
	hcl_oow_t        size,
	hcl_uch_t*       uc
);
HCL_EXPORT hcl_oow_t hcl_uc_to_mb8 (
	hcl_uch_t    uc,
	hcl_bch_t*   mb8,
	hcl_oow_t    size
);

HCL_EXPORT hcl_oow_t hcl_mb8_to_uc (
	const hcl_bch_t* mb8,
	hcl_oow_t        size,
	hcl_uch_t*       uc
);

#if defined(__cplusplus)
}
#endif

#endif
