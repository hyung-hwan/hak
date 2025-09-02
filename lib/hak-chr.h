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

#ifndef _HAK_CHR_H_
#define _HAK_CHR_H_

#include <hak-cmn.h>

enum hak_ooch_prop_t
{
	HAK_OOCH_PROP_UPPER  = (1 << 0),
#define HAK_UCH_PROP_UPPER HAK_OOCH_PROP_UPPER
#define HAK_BCH_PROP_UPPER HAK_OOCH_PROP_UPPER

	HAK_OOCH_PROP_LOWER  = (1 << 1),
#define HAK_UCH_PROP_LOWER HAK_OOCH_PROP_LOWER
#define HAK_BCH_PROP_LOWER HAK_OOCH_PROP_LOWER

	HAK_OOCH_PROP_ALPHA  = (1 << 2),
#define HAK_UCH_PROP_ALPHA HAK_OOCH_PROP_ALPHA
#define HAK_BCH_PROP_ALPHA HAK_OOCH_PROP_ALPHA

	HAK_OOCH_PROP_DIGIT  = (1 << 3),
#define HAK_UCH_PROP_DIGIT HAK_OOCH_PROP_DIGIT
#define HAK_BCH_PROP_DIGIT HAK_OOCH_PROP_DIGIT

	HAK_OOCH_PROP_XDIGIT = (1 << 4),
#define HAK_UCH_PROP_XDIGIT HAK_OOCH_PROP_XDIGIT
#define HAK_BCH_PROP_XDIGIT HAK_OOCH_PROP_XDIGIT

	HAK_OOCH_PROP_ALNUM  = (1 << 5),
#define HAK_UCH_PROP_ALNUM HAK_OOCH_PROP_XDIGIT
#define HAK_BCH_PROP_ALNUM HAK_OOCH_PROP_XDIGIT

	HAK_OOCH_PROP_SPACE  = (1 << 6),
#define HAK_UCH_PROP_SPACE HAK_OOCH_PROP_SPACE
#define HAK_BCH_PROP_SPACE HAK_OOCH_PROP_SPACE

	HAK_OOCH_PROP_PRINT  = (1 << 8),
#define HAK_UCH_PROP_PRINT HAK_OOCH_PROP_PRINT
#define HAK_BCH_PROP_PRINT HAK_OOCH_PROP_PRINT

	HAK_OOCH_PROP_GRAPH  = (1 << 9),
#define HAK_UCH_PROP_GRAPH HAK_OOCH_PROP_GRAPH
#define HAK_BCH_PROP_GRAPH HAK_OOCH_PROP_GRAPH

	HAK_OOCH_PROP_CNTRL  = (1 << 10),
#define HAK_UCH_PROP_CNTRL HAK_OOCH_PROP_CNTRL
#define HAK_BCH_PROP_CNTRL HAK_OOCH_PROP_CNTRL

	HAK_OOCH_PROP_PUNCT  = (1 << 11),
#define HAK_UCH_PROP_PUNCT HAK_OOCH_PROP_PUNCT
#define HAK_BCH_PROP_PUNCT HAK_OOCH_PROP_PUNCT

	HAK_OOCH_PROP_BLANK  = (1 << 12)
#define HAK_UCH_PROP_BLANK HAK_OOCH_PROP_BLANK
#define HAK_BCH_PROP_BLANK HAK_OOCH_PROP_BLANK
};

typedef enum hak_ooch_prop_t hak_ooch_prop_t;
typedef enum hak_ooch_prop_t hak_uch_prop_t;
typedef enum hak_ooch_prop_t hak_bch_prop_t;

#if defined(__cplusplus)
extern "C" {
#endif

HAK_EXPORT int hak_is_uch_type (hak_uch_t c, hak_uch_prop_t type);
HAK_EXPORT int hak_is_uch_upper (hak_uch_t c);
HAK_EXPORT int hak_is_uch_lower (hak_uch_t c);
HAK_EXPORT int hak_is_uch_alpha (hak_uch_t c);
HAK_EXPORT int hak_is_uch_digit (hak_uch_t c);
HAK_EXPORT int hak_is_uch_xdigit (hak_uch_t c);
HAK_EXPORT int hak_is_uch_alnum (hak_uch_t c);
HAK_EXPORT int hak_is_uch_space (hak_uch_t c);
HAK_EXPORT int hak_is_uch_print (hak_uch_t c);
HAK_EXPORT int hak_is_uch_graph (hak_uch_t c);
HAK_EXPORT int hak_is_uch_cntrl (hak_uch_t c);
HAK_EXPORT int hak_is_uch_punct (hak_uch_t c);
HAK_EXPORT int hak_is_uch_blank (hak_uch_t c);
HAK_EXPORT hak_uch_t hak_to_uch_upper (hak_uch_t c);
HAK_EXPORT hak_uch_t hak_to_uch_lower (hak_uch_t c);


/* ------------------------------------------------------------------------- */

HAK_EXPORT int hak_is_bch_type (hak_bch_t c, hak_bch_prop_t type);

#if defined(__has_builtin)
#	if __has_builtin(__builtin_isupper)
#		define hak_is_bch_upper __builtin_isupper
#	endif
#	if __has_builtin(__builtin_islower)
#		define hak_is_bch_lower __builtin_islower
#	endif
#	if __has_builtin(__builtin_isalpha)
#		define hak_is_bch_alpha __builtin_isalpha
#	endif
#	if __has_builtin(__builtin_isdigit)
#		define hak_is_bch_digit __builtin_isdigit
#	endif
#	if __has_builtin(__builtin_isxdigit)
#		define hak_is_bch_xdigit __builtin_isxdigit
#	endif
#	if __has_builtin(__builtin_isalnum)
#		define hak_is_bch_alnum __builtin_isalnum
#	endif
#	if __has_builtin(__builtin_isspace)
#		define hak_is_bch_space __builtin_isspace
#	endif
#	if __has_builtin(__builtin_isprint)
#		define hak_is_bch_print __builtin_isprint
#	endif
#	if __has_builtin(__builtin_isgraph)
#		define hak_is_bch_graph __builtin_isgraph
#	endif
#	if __has_builtin(__builtin_iscntrl)
#		define hak_is_bch_cntrl __builtin_iscntrl
#	endif
#	if __has_builtin(__builtin_ispunct)
#		define hak_is_bch_punct __builtin_ispunct
#	endif
#	if __has_builtin(__builtin_isblank)
#		define hak_is_bch_blank __builtin_isblank
#	endif
#	if __has_builtin(__builtin_toupper)
#		define hak_to_bch_upper __builtin_toupper
#	endif
#	if __has_builtin(__builtin_tolower)
#		define hak_to_bch_lower __builtin_tolower
#	endif
#elif (__GNUC__ >= 4)
#	define hak_is_bch_upper __builtin_isupper
#	define hak_is_bch_lower __builtin_islower
#	define hak_is_bch_alpha __builtin_isalpha
#	define hak_is_bch_digit __builtin_isdigit
#	define hak_is_bch_xdigit __builtin_isxdigit
#	define hak_is_bch_alnum __builtin_isalnum
#	define hak_is_bch_space __builtin_isspace
#	define hak_is_bch_print __builtin_isprint
#	define hak_is_bch_graph __builtin_isgraph
#	define hak_is_bch_cntrl __builtin_iscntrl
#	define hak_is_bch_punct __builtin_ispunct
#	define hak_is_bch_blank __builtin_isblank
#	define hak_to_bch_upper __builtin_toupper
#	define hak_to_bch_lower __builtin_tolower
#endif

/* the bch class functions support no locale.
 * these implemenent latin-1 only */

#if !defined(hak_is_bch_upper) && defined(HAK_HAVE_INLINE)
static HAK_INLINE int hak_is_bch_upper (hak_bch_t c) { return (hak_bcu_t)c - 'A' < 26; }
#elif !defined(hak_is_bch_upper)
#	define hak_is_bch_upper(c) ((hak_bcu_t)(c) - 'A' < 26)
#endif

#if !defined(hak_is_bch_lower) && defined(HAK_HAVE_INLINE)
static HAK_INLINE int hak_is_bch_lower (hak_bch_t c) { return (hak_bcu_t)c - 'a' < 26; }
#elif !defined(hak_is_bch_lower)
#	define hak_is_bch_lower(c) ((hak_bcu_t)(c) - 'a' < 26)
#endif

#if !defined(hak_is_bch_alpha) && defined(HAK_HAVE_INLINE)
static HAK_INLINE int hak_is_bch_alpha (hak_bch_t c) { return ((hak_bcu_t)c | 32) - 'a' < 26; }
#elif !defined(hak_is_bch_alpha)
#	define hak_is_bch_alpha(c) (((hak_bcu_t)(c) | 32) - 'a' < 26)
#endif

#if !defined(hak_is_bch_digit) && defined(HAK_HAVE_INLINE)
static HAK_INLINE int hak_is_bch_digit (hak_bch_t c) { return (hak_bcu_t)c - '0' < 10; }
#elif !defined(hak_is_bch_digit)
#	define hak_is_bch_digit(c) ((hak_bcu_t)(c) - '0' < 10)
#endif

#if !defined(hak_is_bch_xdigit) && defined(HAK_HAVE_INLINE)
static HAK_INLINE int hak_is_bch_xdigit (hak_bch_t c) { return hak_is_bch_digit(c) || ((hak_bcu_t)c | 32) - 'a' < 6; }
#elif !defined(hak_is_bch_xdigit)
#	define hak_is_bch_xdigit(c) (hak_is_bch_digit(c) || ((hak_bcu_t)(c) | 32) - 'a' < 6)
#endif

#if !defined(hak_is_bch_alnum) && defined(HAK_HAVE_INLINE)
static HAK_INLINE int hak_is_bch_alnum (hak_bch_t c) { return hak_is_bch_alpha(c) || hak_is_bch_digit(c); }
#elif !defined(hak_is_bch_alnum)
#	define hak_is_bch_alnum(c) (hak_is_bch_alpha(c) || hak_is_bch_digit(c))
#endif

#if !defined(hak_is_bch_space) && defined(HAK_HAVE_INLINE)
static HAK_INLINE int hak_is_bch_space (hak_bch_t c) { return c == ' ' || (hak_bcu_t)c - '\t' < 5; }
#elif !defined(hak_is_bch_space)
#	define hak_is_bch_space(c) ((c) == ' ' || (hak_bcu_t)(c) - '\t' < 5)
#endif

#if !defined(hak_is_bch_print) && defined(HAK_HAVE_INLINE)
static HAK_INLINE int hak_is_bch_print (hak_bch_t c) { return (hak_bcu_t)c - ' ' < 95; }
#elif !defined(hak_is_bch_print)
#	define hak_is_bch_print(c) ((hak_bcu_t)(c) - ' ' < 95)
#endif

#if !defined(hak_is_bch_graph) && defined(HAK_HAVE_INLINE)
static HAK_INLINE int hak_is_bch_graph (hak_bch_t c) { return (hak_bcu_t)c - '!' < 94; }
#elif !defined(hak_is_bch_graph)
#	define hak_is_bch_graph(c) ((hak_bcu_t)(c) - '!' < 94)
#endif

#if !defined(hak_is_bch_cntrl) && defined(HAK_HAVE_INLINE)
static HAK_INLINE int hak_is_bch_cntrl (hak_bch_t c) { return (hak_bcu_t)c < ' ' || (hak_bcu_t)c == 127; }
#elif !defined(hak_is_bch_cntrl)
#	define hak_is_bch_cntrl(c) ((hak_bcu_t)(c) < ' ' || (hak_bcu_t)(c) == 127)
#endif

#if !defined(hak_is_bch_punct) && defined(HAK_HAVE_INLINE)
static HAK_INLINE int hak_is_bch_punct (hak_bch_t c) { return hak_is_bch_graph(c) && !hak_is_bch_alnum(c); }
#elif !defined(hak_is_bch_punct)
#	define hak_is_bch_punct(c) (hak_is_bch_graph(c) && !hak_is_bch_alnum(c))
#endif

#if !defined(hak_is_bch_blank) && defined(HAK_HAVE_INLINE)
static HAK_INLINE int hak_is_bch_blank (hak_bch_t c) { return c == ' ' || c == '\t'; }
#elif !defined(hak_is_bch_blank)
#	define hak_is_bch_blank(c) ((c) == ' ' || (c) == '\t')
#endif

#if !defined(hak_to_bch_upper)
HAK_EXPORT hak_bch_t hak_to_bch_upper (hak_bch_t c);
#endif
#if !defined(hak_to_bch_lower)
HAK_EXPORT hak_bch_t hak_to_bch_lower (hak_bch_t c);
#endif

#if defined(HAK_OOCH_IS_UCH)
#	define hak_is_ooch_type hak_is_uch_type
#	define hak_is_ooch_upper hak_is_uch_upper
#	define hak_is_ooch_lower hak_is_uch_lower
#	define hak_is_ooch_alpha hak_is_uch_alpha
#	define hak_is_ooch_digit hak_is_uch_digit
#	define hak_is_ooch_xdigit hak_is_uch_xdigit
#	define hak_is_ooch_alnum hak_is_uch_alnum
#	define hak_is_ooch_space hak_is_uch_space
#	define hak_is_ooch_print hak_is_uch_print
#	define hak_is_ooch_graph hak_is_uch_graph
#	define hak_is_ooch_cntrl hak_is_uch_cntrl
#	define hak_is_ooch_punct hak_is_uch_punct
#	define hak_is_ooch_blank hak_is_uch_blank
#	define hak_to_ooch_upper hak_to_uch_upper
#	define hak_to_ooch_lower hak_to_uch_lower
#else
#	define hak_is_ooch_type hak_is_bch_type
#	define hak_is_ooch_upper hak_is_bch_upper
#	define hak_is_ooch_lower hak_is_bch_lower
#	define hak_is_ooch_alpha hak_is_bch_alpha
#	define hak_is_ooch_digit hak_is_bch_digit
#	define hak_is_ooch_xdigit hak_is_bch_xdigit
#	define hak_is_ooch_alnum hak_is_bch_alnum
#	define hak_is_ooch_space hak_is_bch_space
#	define hak_is_ooch_print hak_is_bch_print
#	define hak_is_ooch_graph hak_is_bch_graph
#	define hak_is_ooch_cntrl hak_is_bch_cntrl
#	define hak_is_ooch_punct hak_is_bch_punct
#	define hak_is_ooch_blank hak_is_bch_blank
#	define hak_to_ooch_upper hak_to_bch_upper
#	define hak_to_ooch_lower hak_to_bch_lower
#endif

/* ------------------------------------------------------------------------- */

HAK_EXPORT int hak_get_ucwidth (
        hak_uch_t uc
);

/* ------------------------------------------------------------------------- */

HAK_EXPORT hak_oow_t hak_uc_to_utf8 (
	hak_uch_t    uc,
	hak_bch_t*   utf8,
	hak_oow_t    size
);

HAK_EXPORT hak_oow_t hak_utf8_to_uc (
	const hak_bch_t* utf8,
	hak_oow_t        size,
	hak_uch_t*       uc
);


HAK_EXPORT hak_oow_t hak_uc_to_utf16 (
	hak_uch_t    uc,
	hak_bch_t*   utf16,
	hak_oow_t    size
);

HAK_EXPORT hak_oow_t hak_utf16_to_uc (
	const hak_bch_t* utf16,
	hak_oow_t        size,
	hak_uch_t*       uc
);
HAK_EXPORT hak_oow_t hak_uc_to_mb8 (
	hak_uch_t    uc,
	hak_bch_t*   mb8,
	hak_oow_t    size
);

HAK_EXPORT hak_oow_t hak_mb8_to_uc (
	const hak_bch_t* mb8,
	hak_oow_t        size,
	hak_uch_t*       uc
);

#if defined(__cplusplus)
}
#endif

#endif
