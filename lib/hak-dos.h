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
/* DOS for other platforms than x86?
 * If so, the endian should be defined selectively
 */
#define HAK_ENDIAN_LITTLE

#if defined(__WATCOMC__) && defined(__386__)
#	define HAK_SIZEOF_CHAR        1
#	define HAK_SIZEOF_SHORT       2
#	define HAK_SIZEOF_INT         4
#	define HAK_SIZEOF_LONG        4
#	if (__WATCOMC__ < 1200)
#		define HAK_SIZEOF_LONG_LONG   0
#	else
#		define HAK_SIZEOF_LONG_LONG   8
#	endif
#	define HAK_SIZEOF_VOID_P      4
#	define HAK_SIZEOF_FLOAT       4
#	define HAK_SIZEOF_DOUBLE      8
#	define HAK_SIZEOF_LONG_DOUBLE 8
#	define HAK_SIZEOF_WCHAR_T     2

#	define HAK_SIZEOF___INT8      1
#	define HAK_SIZEOF___INT16     2
#	define HAK_SIZEOF___INT32     4
#	define HAK_SIZEOF___INT64     8
#	define HAK_SIZEOF___INT128    0

#	define HAK_SIZEOF_OFF64_T     0
#	define HAK_SIZEOF_OFF_T       4

#	define HAK_SIZEOF_MBSTATE_T   HAK_SIZEOF_LONG
#	define HAK_MBLEN_MAX          8

#elif defined(__WATCOMC__) && !defined(__386__)
#	define HAK_SIZEOF_CHAR        1
#	define HAK_SIZEOF_SHORT       2
#	define HAK_SIZEOF_INT         2
#	define HAK_SIZEOF_LONG        4
#	define HAK_SIZEOF_LONG_LONG   8

#	define HAK_SIZEOF_VOID_P      4
#	define HAK_SIZEOF_FLOAT       4
#	define HAK_SIZEOF_DOUBLE      8
#	define HAK_SIZEOF_LONG_DOUBLE 8
#	define HAK_SIZEOF_WCHAR_T     2

#	define HAK_SIZEOF___INT8      1
#	define HAK_SIZEOF___INT16     2
#	define HAK_SIZEOF___INT32     4
#	define HAK_SIZEOF___INT64     8
#	define HAK_SIZEOF___INT128    0

#	define HAK_SIZEOF_OFF64_T     0
#	define HAK_SIZEOF_OFF_T       4

#	define HAK_SIZEOF_MBSTATE_T   HAK_SIZEOF_LONG
#	define HAK_MBLEN_MAX          8

#elif defined(__TURBOC__) || defined(_MSC_VER)
/* TODO: be more version specific wchar_t may be available in newer BCC */
#	define HAK_SIZEOF_CHAR        1
#	define HAK_SIZEOF_SHORT       2
#	define HAK_SIZEOF_INT         2
#	define HAK_SIZEOF_LONG        4
#	define HAK_SIZEOF_LONG_LONG   0

#	define HAK_SIZEOF_VOID_P      4
#	define HAK_SIZEOF_FLOAT       4
#	define HAK_SIZEOF_DOUBLE      8
#	define HAK_SIZEOF_LONG_DOUBLE 10
#	define HAK_SIZEOF_WCHAR_T     0

#	define HAK_SIZEOF___INT8      0
#	define HAK_SIZEOF___INT16     0
#	define HAK_SIZEOF___INT32     0
#	define HAK_SIZEOF___INT64     0
#	define HAK_SIZEOF___INT128    0

#	define HAK_SIZEOF_OFF64_T     0
#	define HAK_SIZEOF_OFF_T       4

#	define HAK_SIZEOF_MBSTATE_T   HAK_SIZEOF_LONG
#	define HAK_MBLEN_MAX          8

#elif defined(__ZTC__) && defined(DOS386)

	/* Zortech in DOSX 386 mode (ztc -mx) */
#	define HAK_SIZEOF_CHAR        1
#	define HAK_SIZEOF_SHORT       2
#	define HAK_SIZEOF_INT         4
#	define HAK_SIZEOF_LONG        4
#	define HAK_SIZEOF_LONG_LONG   0

#	define HAK_SIZEOF_VOID_P      4
#	define HAK_SIZEOF_FLOAT       4
#	define HAK_SIZEOF_DOUBLE      8
#	define HAK_SIZEOF_LONG_DOUBLE 8
#	define HAK_SIZEOF_WCHAR_T     1

#	define HAK_SIZEOF___INT8      0
#	define HAK_SIZEOF___INT16     0
#	define HAK_SIZEOF___INT32     0
#	define HAK_SIZEOF___INT64     0
#	define HAK_SIZEOF___INT128    0

#	define HAK_SIZEOF_OFF64_T     0
#	define HAK_SIZEOF_OFF_T       4

#	define HAK_SIZEOF_MBSTATE_T   HAK_SIZEOF_LONG
#	define HAK_MBLEN_MAX          8

#elif defined(_INTELC32_)

	/* Intel C Code Builder 1.0 */
#	define HAK_SIZEOF_CHAR        1
#	define HAK_SIZEOF_SHORT       2
#	define HAK_SIZEOF_INT         4
#	define HAK_SIZEOF_LONG        4
#	define HAK_SIZEOF_LONG_LONG   0

#	define HAK_SIZEOF_VOID_P      4
#	define HAK_SIZEOF_FLOAT       4
#	define HAK_SIZEOF_DOUBLE      8
#	define HAK_SIZEOF_LONG_DOUBLE 8
#	define HAK_SIZEOF_WCHAR_T     1

#	define HAK_SIZEOF___INT8      0
#	define HAK_SIZEOF___INT16     0
#	define HAK_SIZEOF___INT32     0
#	define HAK_SIZEOF___INT64     0
#	define HAK_SIZEOF___INT128    0

#	define HAK_SIZEOF_OFF64_T     0
#	define HAK_SIZEOF_OFF_T       4

#	define HAK_SIZEOF_MBSTATE_T   HAK_SIZEOF_LONG
#	define HAK_MBLEN_MAX          8

#else
#	error Define the size of various data types.
#endif

