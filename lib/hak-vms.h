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

/* OpenVMS.
 *
 * the values below were probed on OpenVMS VAX V7.3 with Compaq C V6.4-005
 * (__DECC_VER 60490005). alpha and itanium differ - most importantly they do
 * have 64-bit integers - so those are selected separately further down. */

#if defined(__alpha) || defined(__alpha__) || defined(__ia64) || defined(__ia64__) || defined(__x86_64) || defined(__x86_64__)
	/* alpha, itanium and x86-64 openvms */
#	define HAK_ENDIAN_LITTLE

#	define HAK_SIZEOF_CHAR        1
#	define HAK_SIZEOF_SHORT       2
#	define HAK_SIZEOF_INT         4
#	define HAK_SIZEOF_LONG        4
#	define HAK_SIZEOF_LONG_LONG   8
#	define HAK_SIZEOF_VOID_P      4 /* 32-bit pointers unless /POINTER_SIZE=64 */
#	define HAK_SIZEOF_FLOAT       4
#	define HAK_SIZEOF_DOUBLE      8
#	define HAK_SIZEOF_LONG_DOUBLE 16
#	define HAK_SIZEOF_WCHAR_T     4

#	define HAK_SIZEOF___INT8      0
#	define HAK_SIZEOF___INT16     0
#	define HAK_SIZEOF___INT32     0
#	define HAK_SIZEOF___INT64     8
#	define HAK_SIZEOF___INT128    0

#	define HAK_SIZEOF_OFF64_T     0
#	define HAK_SIZEOF_OFF_T       4

#	define HAK_SIZEOF_MBSTATE_T   24
#	define HAK_MBLEN_MAX          8

	/* these two have only to be large enough */
#	define HAK_SIZEOF_STRUCT_SOCKADDR_IN 32
#	define HAK_SIZEOF_STRUCT_SOCKADDR_IN6 64
#	define HAK_SIZEOF_SOCKLEN_T 4

#else
	/* vax.
	 *
	 * [IMPORTANT] the vax compiler rejects both 'long long' and '__int64'
	 * outright - it answers %CC-E-NOLONGLONG, 64-bit integral types are not
	 * supported on this platform. so every 64-bit width below must be 0 and
	 * the widest integer available is 32 bits. */
#	define HAK_ENDIAN_LITTLE

#	define HAK_SIZEOF_CHAR        1
#	define HAK_SIZEOF_SHORT       2
#	define HAK_SIZEOF_INT         4
#	define HAK_SIZEOF_LONG        4
#	define HAK_SIZEOF_LONG_LONG   0
#	define HAK_SIZEOF_VOID_P      4
#	define HAK_SIZEOF_FLOAT       4
#	define HAK_SIZEOF_DOUBLE      8
#	define HAK_SIZEOF_LONG_DOUBLE 8 /* same representation as double here */
#	define HAK_SIZEOF_WCHAR_T     4

#	define HAK_SIZEOF___INT8      0
#	define HAK_SIZEOF___INT16     0
#	define HAK_SIZEOF___INT32     0
#	define HAK_SIZEOF___INT64     0
#	define HAK_SIZEOF___INT128    0

#	define HAK_SIZEOF_OFF64_T     0
#	define HAK_SIZEOF_OFF_T       4

#	define HAK_SIZEOF_MBSTATE_T   24
#	define HAK_MBLEN_MAX          8

	/* these two have only to be large enough */
#	define HAK_SIZEOF_STRUCT_SOCKADDR_IN 32
#	define HAK_SIZEOF_STRUCT_SOCKADDR_IN6 64
#	define HAK_SIZEOF_SOCKLEN_T 4
#endif
