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

#ifndef _HAK_CMN_H_
#define _HAK_CMN_H_

/* WARNING: NEVER CHANGE/DELETE THE FOLLOWING HAK_HAVE_CFG_H DEFINITION.
 *          IT IS USED FOR DEPLOYMENT BY MAKEFILE.AM */
/*#define HAK_HAVE_CFG_H*/

#if defined(HAK_HAVE_CFG_H)
#	include <hak-cfg.h>
#elif defined(_WIN32)
#	include <hak-msw.h>
#elif defined(__OS2__)
#	include <hak-os2.h>
#elif defined(__DOS__) || defined(__MSDOS__)
#	if defined(__MSDOS__) && !defined(__DOS__)
#	define __DOS__ __MSDOS__
#	endif
#	include <hak-dos.h>
#elif defined(macintosh)
#	include <hak-mac.h> /* classic mac os */
#else
#	error UNSUPPORTED SYSTEM
#endif

/* =========================================================================
 * ARCHITECTURE/COMPILER TWEAKS
 * ========================================================================= */

#if defined(EMSCRIPTEN)
#	if defined(HAK_SIZEOF___INT128)
#		undef HAK_SIZEOF___INT128
#		define HAK_SIZEOF___INT128 0
#	endif
#	if defined(HAK_SIZEOF_LONG) && defined(HAK_SIZEOF_INT) && (HAK_SIZEOF_LONG > HAK_SIZEOF_INT)
		/* autoconf doesn't seem to match actual emscripten */
#		undef HAK_SIZEOF_LONG
#		define HAK_SIZEOF_LONG HAK_SIZEOF_INT
#	endif
#endif

#if defined(__GNUC__) && defined(__arm__)  && !defined(__ARM_ARCH)
#	if defined(__ARM_ARCH_8__)
#		define __ARM_ARCH 8
#	elif defined(__ARM_ARCH_7__)
#		define __ARM_ARCH 7
#	elif defined(__ARM_ARCH_6__)
#		define __ARM_ARCH 6
#	elif defined(__ARM_ARCH_5__)
#		define __ARM_ARCH 5
#	elif defined(__ARM_ARCH_4__)
#		define __ARM_ARCH 4
#	endif
#endif

/* =========================================================================
 * PREPROCESSOR CAPABILITY CHECK
 * ========================================================================= */
#if !defined(__has_attribute)
	#define __has_attribute(x) 0
#endif

#if !defined(__has_builtin) && defined(_INTELC32_)
	/* intel c code builder 1.0 ended up with an error without this override */
	#define __has_builtin(x) 0
#endif

/*
#if !defined(__has_feature)
	#define __has_feature(x) 0
#endif
#if !defined(__is_identifier)
	#define __is_identifier(x) 0
#endif
*/

/* =========================================================================
 * PRIMITIVE TYPE DEFINTIONS
 * ========================================================================= */

/* hak_int8_t */
#if defined(HAK_SIZEOF_CHAR) && (HAK_SIZEOF_CHAR == 1)
#	define HAK_HAVE_UINT8_T
#	define HAK_HAVE_INT8_T
#	define HAK_SIZEOF_UINT8_T (HAK_SIZEOF_CHAR)
#	define HAK_SIZEOF_INT8_T (HAK_SIZEOF_CHAR)
	typedef unsigned char      hak_uint8_t;
	typedef signed char        hak_int8_t;
#elif defined(HAK_SIZEOF___INT8) && (HAK_SIZEOF___INT8 == 1)
#	define HAK_HAVE_UINT8_T
#	define HAK_HAVE_INT8_T
#	define HAK_SIZEOF_UINT8_T (HAK_SIZEOF___INT8)
#	define HAK_SIZEOF_INT8_T (HAK_SIZEOF___INT8)
	typedef unsigned __int8    hak_uint8_t;
	typedef signed __int8      hak_int8_t;
#elif defined(HAK_SIZEOF___INT8_T) && (HAK_SIZEOF___INT8_T == 1)
#	define HAK_HAVE_UINT8_T
#	define HAK_HAVE_INT8_T
#	define HAK_SIZEOF_UINT8_T (HAK_SIZEOF___INT8_T)
#	define HAK_SIZEOF_INT8_T (HAK_SIZEOF___INT8_T)
	typedef unsigned __int8_t  hak_uint8_t;
	typedef signed __int8_t    hak_int8_t;
#else
#	define HAK_HAVE_UINT8_T
#	define HAK_HAVE_INT8_T
#	define HAK_SIZEOF_UINT8_T (1)
#	define HAK_SIZEOF_INT8_T (1)
	typedef unsigned char      hak_uint8_t;
	typedef signed char        hak_int8_t;
#endif

/* hak_int16_t */
#if defined(HAK_SIZEOF_SHORT) && (HAK_SIZEOF_SHORT == 2)
#	define HAK_HAVE_UINT16_T
#	define HAK_HAVE_INT16_T
#	define HAK_SIZEOF_UINT16_T (HAK_SIZEOF_SHORT)
#	define HAK_SIZEOF_INT16_T (HAK_SIZEOF_SHORT)
	typedef unsigned short int  hak_uint16_t;
	typedef signed short int    hak_int16_t;
#elif defined(HAK_SIZEOF___INT16) && (HAK_SIZEOF___INT16 == 2)
#	define HAK_HAVE_UINT16_T
#	define HAK_HAVE_INT16_T
#	define HAK_SIZEOF_UINT16_T (HAK_SIZEOF___INT16)
#	define HAK_SIZEOF_INT16_T (HAK_SIZEOF___INT16)
	typedef unsigned __int16    hak_uint16_t;
	typedef signed __int16      hak_int16_t;
#elif defined(HAK_SIZEOF___INT16_T) && (HAK_SIZEOF___INT16_T == 2)
#	define HAK_HAVE_UINT16_T
#	define HAK_HAVE_INT16_T
#	define HAK_SIZEOF_UINT16_T (HAK_SIZEOF___INT16_T)
#	define HAK_SIZEOF_INT16_T (HAK_SIZEOF___INT16_T)
	typedef unsigned __int16_t  hak_uint16_t;
	typedef signed __int16_t    hak_int16_t;
#else
#	define HAK_HAVE_UINT16_T
#	define HAK_HAVE_INT16_T
#	define HAK_SIZEOF_UINT16_T (2)
#	define HAK_SIZEOF_INT16_T (2)
	typedef unsigned short int  hak_uint16_t;
	typedef signed short int    hak_int16_t;
#endif


/* hak_int32_t */
#if defined(HAK_SIZEOF_INT) && (HAK_SIZEOF_INT == 4)
#	define HAK_HAVE_UINT32_T
#	define HAK_HAVE_INT32_T
#	define HAK_SIZEOF_UINT32_T (HAK_SIZEOF_INT)
#	define HAK_SIZEOF_INT32_T (HAK_SIZEOF_INT)
	typedef unsigned int        hak_uint32_t;
	typedef signed int          hak_int32_t;
#elif defined(HAK_SIZEOF_LONG) && (HAK_SIZEOF_LONG == 4)
#	define HAK_HAVE_UINT32_T
#	define HAK_HAVE_INT32_T
#	define HAK_SIZEOF_UINT32_T (HAK_SIZEOF_LONG)
#	define HAK_SIZEOF_INT32_T (HAK_SIZEOF_LONG)
	typedef unsigned long int   hak_uint32_t;
	typedef signed long int     hak_int32_t;
#elif defined(HAK_SIZEOF___INT32) && (HAK_SIZEOF___INT32 == 4)
#	define HAK_HAVE_UINT32_T
#	define HAK_HAVE_INT32_T
#	define HAK_SIZEOF_UINT32_T (HAK_SIZEOF___INT32)
#	define HAK_SIZEOF_INT32_T (HAK_SIZEOF___INT32)
	typedef unsigned __int32    hak_uint32_t;
	typedef signed __int32      hak_int32_t;
#elif defined(HAK_SIZEOF___INT32_T) && (HAK_SIZEOF___INT32_T == 4)
#	define HAK_HAVE_UINT32_T
#	define HAK_HAVE_INT32_T
#	define HAK_SIZEOF_UINT32_T (HAK_SIZEOF___INT32_T)
#	define HAK_SIZEOF_INT32_T (HAK_SIZEOF___INT32_T)
	typedef unsigned __int32_t  hak_uint32_t;
	typedef signed __int32_t    hak_int32_t;
#elif defined(__DOS__)
#	define HAK_HAVE_UINT32_T
#	define HAK_HAVE_INT32_T
#	define HAK_SIZEOF_UINT32_T (4)
#	define HAK_SIZEOF_INT32_T (4)
	typedef unsigned long int   hak_uint32_t;
	typedef signed long int     hak_int32_t;
#else
#	define HAK_HAVE_UINT32_T
#	define HAK_HAVE_INT32_T
#	define HAK_SIZEOF_UINT32_T (4)
#	define HAK_SIZEOF_INT32_T (4)
	typedef unsigned int        hak_uint32_t;
	typedef signed int          hak_int32_t;
#endif

/* hak_int64_t */
#if defined(HAK_SIZEOF_INT) && (HAK_SIZEOF_INT == 8)
#	define HAK_HAVE_UINT64_T
#	define HAK_HAVE_INT64_T
#	define HAK_SIZEOF_UINT64_T (HAK_SIZEOF_INT)
#	define HAK_SIZEOF_INT64_T (HAK_SIZEOF_INT)
	typedef unsigned int        hak_uint64_t;
	typedef signed int          hak_int64_t;
#elif defined(HAK_SIZEOF_LONG) && (HAK_SIZEOF_LONG == 8)
#	define HAK_HAVE_UINT64_T
#	define HAK_HAVE_INT64_T
#	define HAK_SIZEOF_UINT64_T (HAK_SIZEOF_LONG)
#	define HAK_SIZEOF_INT64_T (HAK_SIZEOF_LONG)
	typedef unsigned long int  hak_uint64_t;
	typedef signed long int    hak_int64_t;
#elif defined(HAK_SIZEOF_LONG_LONG) && (HAK_SIZEOF_LONG_LONG == 8)
#	define HAK_HAVE_UINT64_T
#	define HAK_HAVE_INT64_T
#	define HAK_SIZEOF_UINT64_T (HAK_SIZEOF_LONG_LONG)
#	define HAK_SIZEOF_INT64_T (HAK_SIZEOF_LONG_LONG)
	typedef unsigned long long int  hak_uint64_t;
	typedef signed long long int    hak_int64_t;
#elif defined(HAK_SIZEOF___INT64) && (HAK_SIZEOF___INT64 == 8)
#	define HAK_HAVE_UINT64_T
#	define HAK_HAVE_INT64_T
#	define HAK_SIZEOF_UINT64_T (HAK_SIZEOF_LONG___INT64)
#	define HAK_SIZEOF_INT64_T (HAK_SIZEOF_LONG___INT64)
	typedef unsigned __int64    hak_uint64_t;
	typedef signed __int64      hak_int64_t;
#elif defined(HAK_SIZEOF___INT64_T) && (HAK_SIZEOF___INT64_T == 8)
#	define HAK_HAVE_UINT64_T
#	define HAK_HAVE_INT64_T
#	define HAK_SIZEOF_UINT64_T (HAK_SIZEOF_LONG___INT64_T)
#	define HAK_SIZEOF_INT64_T (HAK_SIZEOF_LONG___INT64_T)
	typedef unsigned __int64_t  hak_uint64_t;
	typedef signed __int64_t    hak_int64_t;
#else
	/* no 64-bit integer */
#endif

/* hak_int128_t */
#if defined(HAK_SIZEOF_INT) && (HAK_SIZEOF_INT == 16)
#	define HAK_HAVE_UINT128_T
#	define HAK_HAVE_INT128_T
#	define HAK_SIZEOF_UINT128_T (HAK_SIZEOF_INT)
#	define HAK_SIZEOF_INT128_T (HAK_SIZEOF_INT)
	typedef unsigned int        hak_uint128_t;
	typedef signed int          hak_int128_t;
#elif defined(HAK_SIZEOF_LONG) && (HAK_SIZEOF_LONG == 16)
#	define HAK_HAVE_UINT128_T
#	define HAK_HAVE_INT128_T
#	define HAK_SIZEOF_UINT128_T (HAK_SIZEOF_LONG)
#	define HAK_SIZEOF_INT128_T (HAK_SIZEOF_LONG)
	typedef unsigned long int   hak_uint128_t;
	typedef signed long int     hak_int128_t;
#elif defined(HAK_SIZEOF_LONG_LONG) && (HAK_SIZEOF_LONG_LONG == 16)
#	define HAK_HAVE_UINT128_T
#	define HAK_HAVE_INT128_T
#	define HAK_SIZEOF_UINT128_T (HAK_SIZEOF_LONG_LONG)
#	define HAK_SIZEOF_INT128_T (HAK_SIZEOF_LONG_LONG)
	typedef unsigned long long int hak_uint128_t;
	typedef signed long long int   hak_int128_t;
#elif defined(HAK_SIZEOF___INT128) && (HAK_SIZEOF___INT128 == 16)
#	define HAK_HAVE_UINT128_T
#	define HAK_HAVE_INT128_T
#	define HAK_SIZEOF_UINT128_T (HAK_SIZEOF___INT128)
#	define HAK_SIZEOF_INT128_T (HAK_SIZEOF___INT128)
	typedef unsigned __int128    hak_uint128_t;
	typedef signed __int128      hak_int128_t;
#elif defined(HAK_SIZEOF___INT128_T) && (HAK_SIZEOF___INT128_T == 16)
#	define HAK_HAVE_UINT128_T
#	define HAK_HAVE_INT128_T
#	define HAK_SIZEOF_UINT128_T (HAK_SIZEOF___INT128_T)
#	define HAK_SIZEOF_INT128_T (HAK_SIZEOF___INT128_T)
	#if defined(HAK_SIZEOF___UINT128_T) && (HAK_SIZEOF___UINT128_T == HAK_SIZEOF___INT128_T)
	typedef __uint128_t  hak_uint128_t;
	typedef __int128_t   hak_int128_t;
	#elif defined(__clang__)
	typedef __uint128_t  hak_uint128_t;
	typedef __int128_t   hak_int128_t;
	#else
	typedef unsigned __int128_t  hak_uint128_t;
	typedef signed __int128_t    hak_int128_t;
	#endif
#else
	/* no 128-bit integer */
#endif

#if defined(HAK_HAVE_UINT8_T) && (HAK_SIZEOF_VOID_P == 1)
#	error UNSUPPORTED POINTER SIZE
#elif defined(HAK_HAVE_UINT16_T) && (HAK_SIZEOF_VOID_P == 2)
	typedef hak_uint16_t hak_uintptr_t;
	typedef hak_int16_t hak_intptr_t;
	typedef hak_uint8_t hak_ushortptr_t;
	typedef hak_int8_t hak_shortptr_t;
#elif defined(HAK_HAVE_UINT32_T) && (HAK_SIZEOF_VOID_P == 4)
	typedef hak_uint32_t hak_uintptr_t;
	typedef hak_int32_t hak_intptr_t;
	typedef hak_uint16_t hak_ushortptr_t;
	typedef hak_int16_t hak_shortptr_t;
#elif defined(HAK_HAVE_UINT64_T) && (HAK_SIZEOF_VOID_P == 8)
	typedef hak_uint64_t hak_uintptr_t;
	typedef hak_int64_t hak_intptr_t;
	typedef hak_uint32_t hak_ushortptr_t;
	typedef hak_int32_t hak_shortptr_t;
#elif defined(HAK_HAVE_UINT128_T) && (HAK_SIZEOF_VOID_P == 16)
	typedef hak_uint128_t hak_uintptr_t;
	typedef hak_int128_t hak_intptr_t;
	typedef hak_uint64_t hak_ushortptr_t;
	typedef hak_int64_t hak_shortptr_t;
#else
#	error UNKNOWN POINTER SIZE
#endif

#define HAK_SIZEOF_INTPTR_T HAK_SIZEOF_VOID_P
#define HAK_SIZEOF_UINTPTR_T HAK_SIZEOF_VOID_P
#define HAK_SIZEOF_SHORTPTR_T (HAK_SIZEOF_VOID_P / 2)
#define HAK_SIZEOF_USHORTPTR_T (HAK_SIZEOF_VOID_P / 2)

#if defined(HAK_HAVE_INT128_T)
#	define HAK_SIZEOF_INTMAX_T 16
#	define HAK_SIZEOF_UINTMAX_T 16
	typedef hak_int128_t hak_intmax_t;
	typedef hak_uint128_t hak_uintmax_t;
#elif defined(HAK_HAVE_INT64_T)
#	define HAK_SIZEOF_INTMAX_T 8
#	define HAK_SIZEOF_UINTMAX_T 8
	typedef hak_int64_t hak_intmax_t;
	typedef hak_uint64_t hak_uintmax_t;
#elif defined(HAK_HAVE_INT32_T)
#	define HAK_SIZEOF_INTMAX_T 4
#	define HAK_SIZEOF_UINTMAX_T 4
	typedef hak_int32_t hak_intmax_t;
	typedef hak_uint32_t hak_uintmax_t;
#elif defined(HAK_HAVE_INT16_T)
#	define HAK_SIZEOF_INTMAX_T 2
#	define HAK_SIZEOF_UINTMAX_T 2
	typedef hak_int16_t hak_intmax_t;
	typedef hak_uint16_t hak_uintmax_t;
#elif defined(HAK_HAVE_INT8_T)
#	define HAK_SIZEOF_INTMAX_T 1
#	define HAK_SIZEOF_UINTMAX_T 1
	typedef hak_int8_t hak_intmax_t;
	typedef hak_uint8_t hak_uintmax_t;
#else
#	error UNKNOWN INTMAX SIZE
#endif

/* =========================================================================
 * FLOATING-POINT TYPE
 * ========================================================================= */
/** \typedef hak_fltbas_t
 * The hak_fltbas_t type defines the largest floating-pointer number type
 * naturally supported.
 */
#if defined(__FreeBSD__) || defined(__MINGW32__)
	/* TODO: check if the support for long double is complete.
	 *       if so, use long double for hak_flt_t */
	typedef double hak_fltbas_t;
#	define HAK_SIZEOF_FLTBAS_T HAK_SIZEOF_DOUBLE
#elif HAK_SIZEOF_LONG_DOUBLE > HAK_SIZEOF_DOUBLE
	typedef long double hak_fltbas_t;
#	define HAK_SIZEOF_FLTBAS_T HAK_SIZEOF_LONG_DOUBLE
#else
	typedef double hak_fltbas_t;
#	define HAK_SIZEOF_FLTBAS_T HAK_SIZEOF_DOUBLE
#endif

/** \typedef hak_fltmax_t
 * The hak_fltmax_t type defines the largest floating-pointer number type
 * ever supported.
 */
#if HAK_SIZEOF___FLOAT128 >= HAK_SIZEOF_FLTBAS_T
	/* the size of long double may be equal to the size of __float128
	 * for alignment on some platforms */
	typedef __float128 hak_fltmax_t;
#	define HAK_SIZEOF_FLTMAX_T HAK_SIZEOF___FLOAT128
#	define HAK_FLTMAX_REQUIRE_QUADMATH 1
#else
	typedef hak_fltbas_t hak_fltmax_t;
#	define HAK_SIZEOF_FLTMAX_T HAK_SIZEOF_FLTBAS_T
#	undef HAK_FLTMAX_REQUIRE_QUADMATH
#endif

#if defined(HAK_USE_FLTMAX)
typedef hak_fltmax_t hak_flt_t;
#define HAK_SIZEOF_FLT_T HAK_SIZEOF_FLTMAX_T
#else
typedef hak_fltbas_t hak_flt_t;
#define HAK_SIZEOF_FLT_T HAK_SIZEOF_FLTBAS_T
#endif

/* =========================================================================
 * BASIC HARD-CODED DEFINES
 * ========================================================================= */
#define HAK_BITS_PER_BYTE (8)
/* the maximum number of bch charaters to represent a single uch character */
#define HAK_BCSIZE_MAX 6

/* =========================================================================
 * BASIC HAK TYPES
 * ========================================================================= */

typedef char                    hak_bch_t;
typedef int                     hak_bci_t;
typedef unsigned int            hak_bcu_t;
typedef unsigned char           hak_bchu_t; /* unsigned version of hak_bch_t for inner working */
#define HAK_SIZEOF_BCH_T HAK_SIZEOF_CHAR
#define HAK_SIZEOF_BCI_T HAK_SIZEOF_INT

#if defined(HAK_WIDE_CHAR_SIZE) && (HAK_WIDE_CHAR_SIZE >= 4)
#	if defined(__GNUC__) && defined(__CHAR32_TYPE__)
	typedef __CHAR32_TYPE__    hak_uch_t;
#	else
	typedef hak_uint32_t       hak_uch_t;
#	endif
	typedef hak_uint32_t       hak_uchu_t; /* same as hak_uch_t as it is already unsigned */
#	define HAK_SIZEOF_UCH_T 4

#elif defined(__GNUC__) && defined(__CHAR16_TYPE__)
	typedef __CHAR16_TYPE__    hak_uch_t;
	typedef hak_uint16_t       hak_uchu_t; /* same as hak_uch_t as it is already unsigned */
#	define HAK_SIZEOF_UCH_T 2
#else
	typedef hak_uint16_t       hak_uch_t;
	typedef hak_uint16_t       hak_uchu_t; /* same as hak_uch_t as it is already unsigned */
#	define HAK_SIZEOF_UCH_T 2
#endif

typedef hak_int32_t             hak_uci_t;
typedef hak_uint32_t            hak_ucu_t;
#define HAK_SIZEOF_UCI_T 4

typedef hak_uint8_t             hak_oob_t;

/* NOTE: sizeof(hak_oop_t) must be equal to sizeof(hak_oow_t) */
typedef hak_uintptr_t           hak_oow_t;
typedef hak_intptr_t            hak_ooi_t;
#define HAK_SIZEOF_OOW_T HAK_SIZEOF_UINTPTR_T
#define HAK_SIZEOF_OOI_T HAK_SIZEOF_INTPTR_T
#define HAK_OOW_BITS  (HAK_SIZEOF_OOW_T * HAK_BITS_PER_BYTE)
#define HAK_OOI_BITS  (HAK_SIZEOF_OOI_T * HAK_BITS_PER_BYTE)

typedef hak_ushortptr_t         hak_oohw_t; /* half word - half word */
typedef hak_shortptr_t          hak_oohi_t; /* signed half word */
#define HAK_SIZEOF_OOHW_T HAK_SIZEOF_USHORTPTR_T
#define HAK_SIZEOF_OOHI_T HAK_SIZEOF_SHORTPTR_T
#define HAK_OOHW_BITS  (HAK_SIZEOF_OOHW_T * HAK_BITS_PER_BYTE)
#define HAK_OOHI_BITS  (HAK_SIZEOF_OOHI_T * HAK_BITS_PER_BYTE)

struct hak_ucs_t
{
	hak_uch_t* ptr;
	hak_oow_t  len;
};
typedef struct hak_ucs_t hak_ucs_t;

struct hak_bcs_t
{
	hak_bch_t* ptr;
	hak_oow_t  len;
};
typedef struct hak_bcs_t hak_bcs_t;

struct hak_ucsc_t
{
	/* the first two fields 'ptr' and 'len' must match those in hak_ucs_t
	 * for easy downcasting to it.  */
	hak_uch_t* ptr;
	hak_oow_t  len;  /* length */
	hak_oow_t  capa; /* capacity */
};
typedef struct hak_ucsc_t hak_ucsc_t;

struct hak_bcsc_t
{
	/* the first two fields 'ptr' and 'len' must match those in hak_bcs_t
	 * for easy downcasting to it.  */
	hak_bch_t* ptr;
	hak_oow_t  len;  /* length */
	hak_oow_t  capa; /* capacity */
};
typedef struct hak_bcsc_t hak_bcsc_t;

#if defined(HAK_ENABLE_WIDE_CHAR)
	typedef hak_uch_t               hak_ooch_t;
	typedef hak_uchu_t              hak_oochu_t;
	typedef hak_uci_t               hak_ooci_t;
	typedef hak_ucu_t               hak_oocu_t;
	typedef hak_ucs_t               hak_oocs_t;
	typedef hak_ucsc_t              hak_oocsc_t;
#	define HAK_OOCH_IS_UCH
#	define HAK_SIZEOF_OOCH_T HAK_SIZEOF_UCH_T
#else
	typedef hak_bch_t               hak_ooch_t;
	typedef hak_bchu_t              hak_oochu_t;
	typedef hak_bci_t               hak_ooci_t;
	typedef hak_bcu_t               hak_oocu_t;
	typedef hak_bcs_t               hak_oocs_t;
	typedef hak_bcsc_t              hak_oocsc_t;
#	define HAK_OOCH_IS_BCH
#	define HAK_SIZEOF_OOCH_T HAK_SIZEOF_BCH_T
#endif

typedef struct hak_ptl_t hak_ptl_t;
struct hak_ptl_t
{
	void*     ptr;
	hak_oow_t len;
};

typedef struct hak_ptlc_t hak_ptlc_t;
struct hak_ptlc_t
{
	void*     ptr;
	hak_oow_t len;
	hak_oow_t capa;
};

typedef unsigned int hak_bitmask_t;

/* =========================================================================
 * BIGINT TYPES AND MACROS
 * ========================================================================= */
#if defined(HAK_ENABLE_FULL_LIW) && (HAK_SIZEOF_UINTMAX_T > HAK_SIZEOF_OOW_T)
#	define HAK_USE_OOW_FOR_LIW
#endif

#if defined(HAK_USE_OOW_FOR_LIW)
	typedef hak_oow_t          hak_liw_t; /* large integer word */
	typedef hak_ooi_t          hak_lii_t;
	typedef hak_uintmax_t      hak_lidw_t; /* large integer double word */
	typedef hak_intmax_t       hak_lidi_t;
#	define HAK_SIZEOF_LIW_T    HAK_SIZEOF_OOW_T
#	define HAK_SIZEOF_LIDW_T   HAK_SIZEOF_UINTMAX_T
#	define HAK_LIW_BITS        HAK_OOW_BITS
#	define HAK_LIDW_BITS       (HAK_SIZEOF_UINTMAX_T * HAK_BITS_PER_BYTE)
#else
	typedef hak_oohw_t         hak_liw_t;
	typedef hak_oohi_t         hak_lii_t;
	typedef hak_oow_t          hak_lidw_t;
	typedef hak_ooi_t          hak_lidi_t;
#	define HAK_SIZEOF_LIW_T    HAK_SIZEOF_OOHW_T
#	define HAK_SIZEOF_LIDW_T   HAK_SIZEOF_OOW_T
#	define HAK_LIW_BITS        HAK_OOHW_BITS
#	define HAK_LIDW_BITS       HAK_OOW_BITS
#endif

/* =========================================================================
 * BASIC OOP ENCODING
 * ========================================================================= */

/* actual structure defined in hak.h */
typedef struct hak_obj_t           hak_obj_t;
typedef struct hak_obj_t*          hak_oop_t;

/*
 * An object pointer(OOP) is an ordinary pointer value to an object.
 * but some simple numeric values are also encoded into OOP using a simple
 * bit-shifting and masking.
 *
 * A real OOP is stored without any bit-shifting while a non-pointer value encoded
 * in an OOP is bit-shifted to the left by 2 and the 2 least-significant bits
 * are set to 1 or 2.
 *
 * This scheme works because the object allocators aligns the object size to
 * a multiple of sizeof(hak_oop_t). This way, the 2 least-significant bits
 * of a real OOP are always 0s.
 *
 * With 2 bits, i can encode only 3 special types except object pointers.
 * Since I need more than 3 special types, I extend the tag bits up to 4 bits
 * to represent a special data type that doesn't require a range as wide
 * as a small integer. A unicode character, for instance, only requires 21
 * bits at most. An error doesn't need to be as diverse as a small integer.
 */

#define HAK_OOP_TAG_BITS_LO     2
#define HAK_OOP_TAG_BITS_HI     2

#define HAK_OOP_TAG_SMOOI       1    /* 01 */
#define HAK_OOP_TAG_SMPTR       2    /* 10 */
#define HAK_OOP_TAG_EXTENDED    3    /* 11 - internal use only */
#define HAK_OOP_TAG_CHAR        3    /* 0011 */
#define HAK_OOP_TAG_ERROR       7    /* 0111 */
#define HAK_OOP_TAG_RESERVED0   11   /* 1011 */
#define HAK_OOP_TAG_RESERVED1   15   /* 1111 */

#define HAK_OOP_GET_TAG_LO(oop) (((hak_oow_t)oop) & HAK_LBMASK(hak_oow_t, HAK_OOP_TAG_BITS_LO))
#define HAK_OOP_GET_TAG_LOHI(oop) (((hak_oow_t)oop) & HAK_LBMASK(hak_oow_t, HAK_OOP_TAG_BITS_LO + HAK_OOP_TAG_BITS_HI))
#define HAK_OOP_GET_TAG(oop) (HAK_OOP_GET_TAG_LO(oop) == HAK_OOP_TAG_EXTENDED? HAK_OOP_GET_TAG_LOHI(oop): HAK_OOP_GET_TAG_LO(oop))

#define HAK_OOP_IS_NUMERIC(oop) (HAK_OOP_GET_TAG_LO(oop) != 0)
#define HAK_OOP_IS_POINTER(oop) (HAK_OOP_GET_TAG_LO(oop) == 0)

#define HAK_OOP_IS_SMOOI(oop) (HAK_OOP_GET_TAG_LO(oop) == HAK_OOP_TAG_SMOOI)
#define HAK_OOP_IS_SMPTR(oop) (HAK_OOP_GET_TAG_LO(oop) == HAK_OOP_TAG_SMPTR)

#define HAK_SMOOI_TO_OOP(num) ((hak_oop_t)((((hak_oow_t)(hak_ooi_t)(num)) << HAK_OOP_TAG_BITS_LO) | HAK_OOP_TAG_SMOOI))
#define HAK_OOP_TO_SMOOI(oop) (((hak_ooi_t)oop) >> HAK_OOP_TAG_BITS_LO)
/*
#define HAK_SMPTR_TO_OOP(ptr) ((hak_oop_t)((((hak_oow_t)(ptr)) << HAK_OOP_TAG_BITS_LO) | HAK_OOP_TAG_SMPTR))
#define HAK_OOP_TO_SMPTR(oop) (((hak_ooi_t)oop) >> HAK_OOP_TAG_BITS_LO)
*/
#define HAK_SMPTR_TO_OOP(ptr) ((hak_oop_t)(((hak_oow_t)ptr) | HAK_OOP_TAG_SMPTR))
#define HAK_OOP_TO_SMPTR(oop) ((void*)(((hak_oow_t)oop) & ~HAK_LBMASK(hak_oow_t, HAK_OOP_TAG_BITS_LO)))

#define HAK_OOP_IS_CHAR(oop) (HAK_OOP_GET_TAG(oop) == HAK_OOP_TAG_CHAR)
#define HAK_OOP_IS_ERROR(oop) (HAK_OOP_GET_TAG(oop) == HAK_OOP_TAG_ERROR)

#define HAK_OOP_TO_CHAR(oop) (((hak_oow_t)oop) >> (HAK_OOP_TAG_BITS_LO + HAK_OOP_TAG_BITS_LO))
#define HAK_CHAR_TO_OOP(num) ((hak_oop_t)((((hak_oow_t)(num)) << (HAK_OOP_TAG_BITS_LO + HAK_OOP_TAG_BITS_LO)) | HAK_OOP_TAG_CHAR))
#define HAK_OOP_TO_ERROR(oop) (((hak_oow_t)oop) >> (HAK_OOP_TAG_BITS_LO + HAK_OOP_TAG_BITS_LO))
#define HAK_ERROR_TO_OOP(num) ((hak_oop_t)((((hak_oow_t)(num)) << (HAK_OOP_TAG_BITS_LO + HAK_OOP_TAG_BITS_LO)) | HAK_OOP_TAG_ERROR))

/* SMOOI takes up 62 bit on a 64-bit architecture and 30 bits
 * on a 32-bit architecture. The absolute value takes up 61 bits and 29 bits
 * respectively for the 1 sign bit. */
#define HAK_SMOOI_BITS (HAK_OOI_BITS - HAK_OOP_TAG_BITS_LO)
#define HAK_SMOOI_ABS_BITS (HAK_SMOOI_BITS - 1)
#define HAK_SMOOI_MAX ((hak_ooi_t)(~((hak_oow_t)0) >> (HAK_OOP_TAG_BITS_LO + 1)))
/* Sacrificing 1 bit pattern for a negative SMOOI makes
 * implementation a lot eaisier in many respect. */
/*#define HAK_SMOOI_MIN (-HAK_SMOOI_MAX - 1)*/
#define HAK_SMOOI_MIN (-HAK_SMOOI_MAX)
#define HAK_IN_SMOOI_RANGE(ooi)  ((ooi) >= HAK_SMOOI_MIN && (ooi) <= HAK_SMOOI_MAX)


/* SMPTR is a special value which has been devised to encode an address value
 * whose low HAK_OOP_TAG_BITS_LO bits are 0. its class is SmallPointer. A pointer
 * returned by most of system functions would be aligned to the pointer size.
 * you can use the followings macros when converting such a pointer without loss. */
#define HAK_IN_SMPTR_RANGE(ptr) ((((hak_oow_t)ptr) & HAK_LBMASK(hak_oow_t, HAK_OOP_TAG_BITS_LO)) == 0)

#define HAK_CHAR_BITS (HAK_OOI_BITS - HAK_OOP_TAG_BITS_LO - HAK_OOP_TAG_BITS_HI)
#define HAK_CHAR_MIN 0
#define HAK_CHAR_MAX (~((hak_oow_t)0) >> (HAK_OOP_TAG_BITS_LO + HAK_OOP_TAG_BITS_HI))

#define HAK_ERROR_BITS (HAK_OOI_BITS - HAK_OOP_TAG_BITS_LO - HAK_OOP_TAG_BITS_HI)
#define HAK_ERROR_MIN 0
#define HAK_ERROR_MAX (~((hak_oow_t)0) >> (HAK_OOP_TAG_BITS_LO + HAK_OOP_TAG_BITS_HI))

/* TODO: There are untested code where smint is converted to hak_oow_t.
 *       for example, the spec making macro treats the number as hak_oow_t instead of hak_ooi_t.
 *       as of this writing, i skip testing that part with the spec value exceeding HAK_SMOOI_MAX.
 *       later, please verify it works, probably by limiting the value ranges in such macros
 */

/* =========================================================================
 * TIME-RELATED TYPES
 * =========================================================================*/
#define HAK_MSECS_PER_SEC  (1000)
#define HAK_MSECS_PER_MIN  (HAK_MSECS_PER_SEC * HAK_SECS_PER_MIN)
#define HAK_MSECS_PER_HOUR (HAK_MSECS_PER_SEC * HAK_SECS_PER_HOUR)
#define HAK_MSECS_PER_DAY  (HAK_MSECS_PER_SEC * HAK_SECS_PER_DAY)

#define HAK_USECS_PER_MSEC (1000)
#define HAK_NSECS_PER_USEC (1000)
#define HAK_NSECS_PER_MSEC (HAK_NSECS_PER_USEC * HAK_USECS_PER_MSEC)
#define HAK_USECS_PER_SEC  (HAK_USECS_PER_MSEC * HAK_MSECS_PER_SEC)
#define HAK_NSECS_PER_SEC  (HAK_NSECS_PER_USEC * HAK_USECS_PER_MSEC * HAK_MSECS_PER_SEC)

#define HAK_SECNSEC_TO_MSEC(sec,nsec) \
        (((hak_intptr_t)(sec) * HAK_MSECS_PER_SEC) + ((hak_intptr_t)(nsec) / HAK_NSECS_PER_MSEC))

#define HAK_SECNSEC_TO_USEC(sec,nsec) \
        (((hak_intptr_t)(sec) * HAK_USECS_PER_SEC) + ((hak_intptr_t)(nsec) / HAK_NSECS_PER_USEC))

#define HAK_SECNSEC_TO_NSEC(sec,nsec) \
        (((hak_intptr_t)(sec) * HAK_NSECS_PER_SEC) + (hak_intptr_t)(nsec))

#define HAK_SEC_TO_MSEC(sec) ((sec) * HAK_MSECS_PER_SEC)
#define HAK_MSEC_TO_SEC(sec) ((sec) / HAK_MSECS_PER_SEC)

#define HAK_USEC_TO_NSEC(usec) ((usec) * HAK_NSECS_PER_USEC)
#define HAK_NSEC_TO_USEC(nsec) ((nsec) / HAK_NSECS_PER_USEC)

#define HAK_MSEC_TO_NSEC(msec) ((msec) * HAK_NSECS_PER_MSEC)
#define HAK_NSEC_TO_MSEC(nsec) ((nsec) / HAK_NSECS_PER_MSEC)

#define HAK_SEC_TO_NSEC(sec) ((sec) * HAK_NSECS_PER_SEC)
#define HAK_NSEC_TO_SEC(nsec) ((nsec) / HAK_NSECS_PER_SEC)

#define HAK_SEC_TO_USEC(sec) ((sec) * HAK_USECS_PER_SEC)
#define HAK_USEC_TO_SEC(usec) ((usec) / HAK_USECS_PER_SEC)

#if defined(HAK_SIZEOF_INT64_T) && (HAK_SIZEOF_INT64_T > 0)
typedef hak_int64_t hak_ntime_sec_t;
#else
typedef hak_int32_t hak_ntime_sec_t;
#endif
typedef hak_int32_t hak_ntime_nsec_t;

typedef struct hak_ntime_t hak_ntime_t;
struct hak_ntime_t
{
	hak_ntime_sec_t  sec;
	hak_ntime_nsec_t   nsec; /* nanoseconds */
};

#define HAK_INIT_NTIME(c,s,ns) (((c)->sec = (s)), ((c)->nsec = (ns)))
#define HAK_CLEAR_NTIME(c) HAK_INIT_NTIME(c, 0, 0)

#define HAK_ADD_NTIME(c,a,b) \
	do { \
		(c)->sec = (a)->sec + (b)->sec; \
		(c)->nsec = (a)->nsec + (b)->nsec; \
		while ((c)->nsec >= HAK_NSECS_PER_SEC) { (c)->sec++; (c)->nsec -= HAK_NSECS_PER_SEC; } \
	} while(0)

#define HAK_ADD_NTIME_SNS(c,a,s,ns) \
	do { \
		(c)->sec = (a)->sec + (s); \
		(c)->nsec = (a)->nsec + (ns); \
		while ((c)->nsec >= HAK_NSECS_PER_SEC) { (c)->sec++; (c)->nsec -= HAK_NSECS_PER_SEC; } \
	} while(0)

#define HAK_SUB_NTIME(c,a,b) \
	do { \
		(c)->sec = (a)->sec - (b)->sec; \
		(c)->nsec = (a)->nsec - (b)->nsec; \
		while ((c)->nsec < 0) { (c)->sec--; (c)->nsec += HAK_NSECS_PER_SEC; } \
	} while(0)

#define HAK_SUB_NTIME_SNS(c,a,s,ns) \
	do { \
		(c)->sec = (a)->sec - s; \
		(c)->nsec = (a)->nsec - ns; \
		while ((c)->nsec < 0) { (c)->sec--; (c)->nsec += HAK_NSECS_PER_SEC; } \
	} while(0)


#define HAK_CMP_NTIME(a,b) (((a)->sec == (b)->sec)? ((a)->nsec - (b)->nsec): ((a)->sec - (b)->sec))

/* =========================================================================
 * PRIMITIVE MACROS
 * ========================================================================= */
#define HAK_UCI_EOF ((hak_uci_t)-1)
#define HAK_BCI_EOF ((hak_bci_t)-1)
#define HAK_OOCI_EOF ((hak_ooci_t)-1)

#define HAK_SIZEOF(x) (sizeof(x))
#define HAK_COUNTOF(x) (sizeof(x) / sizeof((x)[0]))
#define HAK_BITSOF(x) (sizeof(x) * HAK_BITS_PER_BYTE)

/**
 * The HAK_OFFSETOF() macro returns the offset of a field from the beginning
 * of a structure.
 */
#define HAK_OFFSETOF(type,member) ((hak_uintptr_t)&((type*)0)->member)

/**
 * The HAK_ALIGNOF() macro returns the alignment size of a structure.
 * Note that this macro may not work reliably depending on the type given.
 */
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 202311L) /* C23 */
#define HAK_ALIGNOF(type) alignof(type)
#elif defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L) /* C11 */
#define HAK_ALIGNOF(type) _Alignof(type)
#elif defined(__cplusplus) && (__cplusplus >= 201103L) /* C++11 */
#define HAK_ALIGNOF(type) alignof(type)
#else
#define HAK_ALIGNOF(type) HAK_OFFSETOF(struct { hak_uint8_t d1; type d2; }, d2)
        /*(sizeof(struct { hak_uint8_t d1; type d2; }) - sizeof(type))*/
#endif

#if defined(__cplusplus)
#	if (__cplusplus >= 201103L) /* C++11 */
#		define HAK_NULL nullptr
#	else
#		define HAK_NULL (0)
#	endif
#else
#	define HAK_NULL ((void*)0)
#endif

/* make a bit mask that can mask off low n bits */
#define HAK_LBMASK(type,n) (~(~((type)0) << (n)))
#define HAK_LBMASK_SAFE(type,n) (((n) < HAK_BITSOF(type))? HAK_LBMASK(type,n): ~(type)0)

/* make a bit mask that can mask off high n bits */
#define HAK_HBMASK(type,n) (~(~((type)0) >> (n)))
#define HAK_HBMASK_SAFE(type,n) (((n) < HAK_BITSOF(type))? HAK_HBMASK(type,n): ~(type)0)

/* get 'length' bits starting from the bit at the 'offset' */
#define HAK_GETBITS(type,value,offset,length) \
	((((type)(value)) >> (offset)) & HAK_LBMASK(type,length))

#define HAK_CLEARBITS(type,value,offset,length) \
	(((type)(value)) & ~(HAK_LBMASK(type,length) << (offset)))

#define HAK_SETBITS(type,value,offset,length,bits) \
	(value = (HAK_CLEARBITS(type,value,offset,length) | (((bits) & HAK_LBMASK(type,length)) << (offset))))

#define HAK_FLIPBITS(type,value,offset,length) \
	(((type)(value)) ^ (HAK_LBMASK(type,length) << (offset)))

#define HAK_ORBITS(type,value,offset,length,bits) \
	(value = (((type)(value)) | (((bits) & HAK_LBMASK(type,length)) << (offset))))


/**
 * The HAK_BITS_MAX() macros calculates the maximum value that the 'nbits'
 * bits of an unsigned integer of the given 'type' can hold.
 * \code
 * printf ("%u", HAK_BITS_MAX(unsigned int, 5));
 * \endcode
 */
/*#define HAK_BITS_MAX(type,nbits) ((((type)1) << (nbits)) - 1)*/
#define HAK_BITS_MAX(type,nbits) ((~(type)0) >> (HAK_BITSOF(type) - (nbits)))

/* =========================================================================
 * MMGR
 * ========================================================================= */
typedef struct hak_mmgr_t hak_mmgr_t;

/**
 * allocate a memory chunk of the size \a n.
 * \return pointer to a memory chunk on success, #HAK_NULL on failure.
 */
typedef void* (*hak_mmgr_alloc_t)   (hak_mmgr_t* mmgr, hak_oow_t n);
/**
 * resize a memory chunk pointed to by \a ptr to the size \a n.
 * \return pointer to a memory chunk on success, #HAK_NULL on failure.
 */
typedef void* (*hak_mmgr_realloc_t) (hak_mmgr_t* mmgr, void* ptr, hak_oow_t n);
/**
 * free a memory chunk pointed to by \a ptr.
 */
typedef void  (*hak_mmgr_free_t)    (hak_mmgr_t* mmgr, void* ptr);

/**
 * The hak_mmgr_t type defines the memory management interface.
 * As the type is merely a structure, it is just used as a single container
 * for memory management functions with a pointer to user-defined data.
 * The user-defined data pointer \a ctx is passed to each memory management
 * function whenever it is called. You can allocate, reallocate, and free
 * a memory chunk.
 *
 * For example, a hak_xxx_open() function accepts a pointer of the hak_mmgr_t
 * type and the xxx object uses it to manage dynamic data within the object.
 */
struct hak_mmgr_t
{
	hak_mmgr_alloc_t   allocmem;   /**< allocation function */
	hak_mmgr_realloc_t reallocmem; /**< resizing function */
	hak_mmgr_free_t    freemem;    /**< disposal function */
	void*              ctx;     /**< user-defined data pointer */
};

/**
 * The HAK_MMGR_ALLOC() macro allocates a memory block of the \a size bytes
 * using the \a mmgr memory manager.
 */
#define HAK_MMGR_ALLOC(mmgr,size) ((mmgr)->allocmem(mmgr,size))

/**
 * The HAK_MMGR_REALLOC() macro resizes a memory block pointed to by \a ptr
 * to the \a size bytes using the \a mmgr memory manager.
 */
#define HAK_MMGR_REALLOC(mmgr,ptr,size) ((mmgr)->reallocmem(mmgr,ptr,size))

/**
 * The HAK_MMGR_FREE() macro deallocates the memory block pointed to by \a ptr.
 */
#define HAK_MMGR_FREE(mmgr,ptr) ((mmgr)->freemem(mmgr,ptr))


/* =========================================================================
 * CMGR
 * =========================================================================*/

typedef struct hak_cmgr_t hak_cmgr_t;

typedef hak_oow_t (*hak_cmgr_bctouc_t) (
	const hak_bch_t*   mb,
	hak_oow_t         size,
	hak_uch_t*         wc
);

typedef hak_oow_t (*hak_cmgr_uctobc_t) (
	hak_uch_t    wc,
	hak_bch_t*   mb,
	hak_oow_t   size
);

/**
 * The hak_cmgr_t type defines the character-level interface to
 * multibyte/wide-character conversion. This interface doesn't
 * provide any facility to store conversion state in a context
 * independent manner. This leads to the limitation that it can
 * handle a stateless multibyte encoding only.
 */
struct hak_cmgr_t
{
	hak_cmgr_bctouc_t bctouc;
	hak_cmgr_uctobc_t uctobc;
};

/* =========================================================================
 * FORWARD DECLARATION FOR MAIN HAK STRUCTURE
 * =========================================================================*/
typedef struct hak_t hak_t;

/* =========================================================================
 * MACROS THAT CHANGES THE BEHAVIORS OF THE C COMPILER/LINKER
 * =========================================================================*/

#if defined(__BORLANDC__) && (__BORLANDC__ < 0x500)
#	define HAK_IMPORT
#	define HAK_EXPORT
#	define HAK_PRIVATE
#elif defined(_WIN32) || (defined(__WATCOMC__) && (__WATCOMC__ >= 1000) && !defined(__WINDOWS_386__))
#	define HAK_IMPORT __declspec(dllimport)
#	define HAK_EXPORT __declspec(dllexport)
#	define HAK_PRIVATE
#elif defined(__GNUC__) && ((__GNUC__>= 4) || (__GNUC__ == 3 && __GNUC_MINOR__ >= 3))
#	define HAK_IMPORT __attribute__((visibility("default")))
#	define HAK_EXPORT __attribute__((visibility("default")))
#	define HAK_PRIVATE __attribute__((visibility("hidden")))
/*#	define HAK_PRIVATE __attribute__((visibility("internal")))*/
#else
#	define HAK_IMPORT
#	define HAK_EXPORT
#	define HAK_PRIVATE
#endif

#if defined(__cplusplus) || (defined(__STDC_VERSION__) && (__STDC_VERSION__>=199901L))
	/* C++/C99 has inline */
#	define HAK_INLINE inline
#	define HAK_HAVE_INLINE
#elif defined(__GNUC__) && defined(__GNUC_GNU_INLINE__)
	/* gcc disables inline when -std=c89 or -ansi is used.
	 * so use __inline__ supported by gcc regardless of the options */
#	define HAK_INLINE /*extern*/ __inline__
#	define HAK_HAVE_INLINE
#else
#	define HAK_INLINE
#	undef HAK_HAVE_INLINE
#endif

#if __has_attribute(__sentinel__) || (defined(__GNUC__) && (__GNUC__ >= 4))
#	define HAK_SENTINEL(v) __attribute__((__sentinel__(x)))
#else
#	define HAK_SENTINEL(v)
#endif

#if defined(__GNUC__) && (__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ > 4))
#	define HAK_UNUSED __attribute__((__unused__))
#else
#	define HAK_UNUSED
#endif

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 202311L)
#	define HAK_NORETURN noreturn
#elif defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)
#	define HAK_NORETURN _Noreturn
#else
#	define HAK_NORETURN
#endif

/**
 * The HAK_TYPE_IS_SIGNED() macro determines if a type is signed.
 * \code
 * printf ("%d\n", (int)HAK_TYPE_IS_SIGNED(int));
 * printf ("%d\n", (int)HAK_TYPE_IS_SIGNED(unsigned int));
 * \endcode
 */
#define HAK_TYPE_IS_SIGNED(type) (((type)0) > ((type)-1))

/**
 * The HAK_TYPE_IS_SIGNED() macro determines if a type is unsigned.
 * \code
 * printf ("%d\n", HAK_TYPE_IS_UNSIGNED(int));
 * printf ("%d\n", HAK_TYPE_IS_UNSIGNED(unsigned int));
 * \endcode
 */
#define HAK_TYPE_IS_UNSIGNED(type) (((type)0) < ((type)-1))

#define HAK_TYPE_SIGNED_MAX(type) \
	((type)~((type)1 << ((type)HAK_BITSOF(type) - 1)))
#define HAK_TYPE_UNSIGNED_MAX(type) ((type)(~(type)0))

#define HAK_TYPE_SIGNED_MIN(type) \
	((type)((type)1 << ((type)HAK_BITSOF(type) - 1)))
#define HAK_TYPE_UNSIGNED_MIN(type) ((type)0)

#define HAK_TYPE_MAX(type) \
	((HAK_TYPE_IS_SIGNED(type)? HAK_TYPE_SIGNED_MAX(type): HAK_TYPE_UNSIGNED_MAX(type)))
#define HAK_TYPE_MIN(type) \
	((HAK_TYPE_IS_SIGNED(type)? HAK_TYPE_SIGNED_MIN(type): HAK_TYPE_UNSIGNED_MIN(type)))

/* round up a positive integer x to the nearst multiple of y */
#define HAK_ALIGN(x,y) ((((x) + (y) - 1) / (y)) * (y))

/* round up a positive integer x to the nearst multiple of y where
 * y must be a multiple of a power of 2*/
#define HAK_ALIGN_POW2(x,y) ((((x) + (y) - 1)) & ~((y) - 1))

#define HAK_IS_UNALIGNED_POW2(x,y) ((x) & ((y) - 1))
#define HAK_IS_ALIGNED_POW2(x,y) (!HAK_IS_UNALIGNED_POW2(x,y))

#if defined(__cplusplus) || (defined(__STDC_VERSION__) && (__STDC_VERSION__>=199901L))
/* array index */
#define HAK_AID(x) [x]=
/* struct field name */
#define HAK_SFN(x) .x=
#else
#define HAK_AID(x)
#define HAK_SFN(x)
#endif

/* =========================================================================
 * COMPILER FEATURE TEST MACROS
 * =========================================================================*/
#if defined(__has_builtin)
	#if __has_builtin(__builtin_ctz)
		#define HAK_HAVE_BUILTIN_CTZ
	#endif
	#if __has_builtin(__builtin_ctzl)
		#define HAK_HAVE_BUILTIN_CTZL
	#endif
	#if __has_builtin(__builtin_ctzll)
		#define HAK_HAVE_BUILTIN_CTZLL
	#endif

	#if __has_builtin(__builtin_clz)
		#define HAK_HAVE_BUILTIN_CLZ
	#endif
	#if __has_builtin(__builtin_clzl)
		#define HAK_HAVE_BUILTIN_CLZL
	#endif
	#if __has_builtin(__builtin_clzll)
		#define HAK_HAVE_BUILTIN_CLZLL
	#endif

	#if __has_builtin(__builtin_uadd_overflow)
		#define HAK_HAVE_BUILTIN_UADD_OVERFLOW
	#endif
	#if __has_builtin(__builtin_uaddl_overflow)
		#define HAK_HAVE_BUILTIN_UADDL_OVERFLOW
	#endif
	#if __has_builtin(__builtin_uaddll_overflow)
		#define HAK_HAVE_BUILTIN_UADDLL_OVERFLOW
	#endif
	#if __has_builtin(__builtin_umul_overflow)
		#define HAK_HAVE_BUILTIN_UMUL_OVERFLOW
	#endif
	#if __has_builtin(__builtin_umull_overflow)
		#define HAK_HAVE_BUILTIN_UMULL_OVERFLOW
	#endif
	#if __has_builtin(__builtin_umulll_overflow)
		#define HAK_HAVE_BUILTIN_UMULLL_OVERFLOW
	#endif

	#if __has_builtin(__builtin_sadd_overflow)
		#define HAK_HAVE_BUILTIN_SADD_OVERFLOW
	#endif
	#if __has_builtin(__builtin_saddl_overflow)
		#define HAK_HAVE_BUILTIN_SADDL_OVERFLOW
	#endif
	#if __has_builtin(__builtin_saddll_overflow)
		#define HAK_HAVE_BUILTIN_SADDLL_OVERFLOW
	#endif
	#if __has_builtin(__builtin_smul_overflow)
		#define HAK_HAVE_BUILTIN_SMUL_OVERFLOW
	#endif
	#if __has_builtin(__builtin_smull_overflow)
		#define HAK_HAVE_BUILTIN_SMULL_OVERFLOW
	#endif
	#if __has_builtin(__builtin_smulll_overflow)
		#define HAK_HAVE_BUILTIN_SMULLL_OVERFLOW
	#endif

	#if __has_builtin(__builtin_expect)
		#define HAK_HAVE_BUILTIN_EXPECT
	#endif


	#if __has_builtin(__sync_lock_test_and_set)
		#define HAK_HAVE_SYNC_LOCK_TEST_AND_SET
	#endif
	#if __has_builtin(__sync_lock_release)
		#define HAK_HAVE_SYNC_LOCK_RELEASE
	#endif

	#if __has_builtin(__sync_synchronize)
		#define HAK_HAVE_SYNC_SYNCHRONIZE
	#endif
	#if __has_builtin(__sync_bool_compare_and_swap)
		#define HAK_HAVE_SYNC_BOOL_COMPARE_AND_SWAP
	#endif
	#if __has_builtin(__sync_val_compare_and_swap)
		#define HAK_HAVE_SYNC_VAL_COMPARE_AND_SWAP
	#endif

	#if __has_builtin(__builtin_bswap16)
		#define HAK_HAVE_BUILTIN_BSWAP16
	#endif
	#if __has_builtin(__builtin_bswap32)
		#define HAK_HAVE_BUILTIN_BSWAP32
	#endif
	#if __has_builtin(__builtin_bswap64)
		#define HAK_HAVE_BUILTIN_BSWAP64
	#endif
	#if __has_builtin(__builtin_bswap128)
		#define HAK_HAVE_BUILTIN_BSWAP128
	#endif

#elif defined(__GNUC__) && defined(__GNUC_MINOR__)

	#if (__GNUC__ >= 4)
		#define HAK_HAVE_SYNC_LOCK_TEST_AND_SET
		#define HAK_HAVE_SYNC_LOCK_RELEASE

		#define HAK_HAVE_SYNC_SYNCHRONIZE
		#define HAK_HAVE_SYNC_BOOL_COMPARE_AND_SWAP
		#define HAK_HAVE_SYNC_VAL_COMPARE_AND_SWAP
	#endif

	#if (__GNUC__ >= 4) || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
		#define HAK_HAVE_BUILTIN_CTZ
		#define HAK_HAVE_BUILTIN_CTZL
		#define HAK_HAVE_BUILTIN_CTZLL
		#define HAK_HAVE_BUILTIN_CLZ
		#define HAK_HAVE_BUILTIN_CLZL
		#define HAK_HAVE_BUILTIN_CLZLL
		#define HAK_HAVE_BUILTIN_EXPECT
	#endif

	#if (__GNUC__ >= 5)
		#define HAK_HAVE_BUILTIN_UADD_OVERFLOW
		#define HAK_HAVE_BUILTIN_UADDL_OVERFLOW
		#define HAK_HAVE_BUILTIN_UADDLL_OVERFLOW
		#define HAK_HAVE_BUILTIN_UMUL_OVERFLOW
		#define HAK_HAVE_BUILTIN_UMULL_OVERFLOW
		#define HAK_HAVE_BUILTIN_UMULLL_OVERFLOW

		#define HAK_HAVE_BUILTIN_SADD_OVERFLOW
		#define HAK_HAVE_BUILTIN_SADDL_OVERFLOW
		#define HAK_HAVE_BUILTIN_SADDLL_OVERFLOW
		#define HAK_HAVE_BUILTIN_SMUL_OVERFLOW
		#define HAK_HAVE_BUILTIN_SMULL_OVERFLOW
		#define HAK_HAVE_BUILTIN_SMULLL_OVERFLOW
	#endif

	#if (__GNUC__ >= 5) || (__GNUC__ == 4 && __GNUC_MINOR__ >= 8)
		/* 4.8.0 or later */
		#define HAK_HAVE_BUILTIN_BSWAP16
	#endif
	#if (__GNUC__ >= 5) || (__GNUC__ == 4 && __GNUC_MINOR__ >= 3)
		/* 4.3.0 or later */
		#define HAK_HAVE_BUILTIN_BSWAP32
		#define HAK_HAVE_BUILTIN_BSWAP64
		/*#define HAK_HAVE_BUILTIN_BSWAP128*/
	#endif

#endif

#if defined(HAK_HAVE_BUILTIN_EXPECT)
#	define HAK_LIKELY(x) (__builtin_expect(!!(x),1))
#	define HAK_UNLIKELY(x) (__builtin_expect(!!(x),0))
#else
#	define HAK_LIKELY(x) (x)
#	define HAK_UNLIKELY(x) (x)
#endif

/* =========================================================================
 * STATIC ASSERTION
 * =========================================================================*/
#define HAK_STATIC_JOIN_INNER(x, y) x ## y
#define HAK_STATIC_JOIN(x, y) HAK_STATIC_JOIN_INNER(x, y)

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 202311L)
#	define HAK_STATIC_ASSERT(expr)  static_assert (expr, "invalid assertion")
#elif defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)
#	define HAK_STATIC_ASSERT(expr)  _Static_assert (expr, "invalid assertion")
#elif defined(__cplusplus) && (__cplusplus >= 201103L)
#	define HAK_STATIC_ASSERT(expr) static_assert (expr, "invalid assertion")
#else
#	define HAK_STATIC_ASSERT(expr) typedef char HAK_STATIC_JOIN(HAK_STATIC_ASSERT_T_, __LINE__)[(expr)? 1: -1] HAK_UNUSED
#endif

#define HAK_STATIC_ASSERT_EXPR(expr) ((void)HAK_SIZEOF(char[(expr)? 1: -1]))

#endif
