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

#ifndef _HAK_UTL_H_
#define _HAK_UTL_H_

#include <hak-cmn.h>
#include <stdarg.h>

/* =========================================================================
 * DOUBLY LINKED LIST
 * ========================================================================= */
#define HAK_APPEND_TO_LIST(list, node) do { \
	(node)->next = HAK_NULL; \
	(node)->prev = (list)->last; \
	if ((list)->first) (list)->last->next = (node); \
	else (list)->first = (node); \
	(list)->last = (node); \
} while(0)

#define HAK_PREPPEND_TO_LIST(list, node) do { \
	(node)->prev = HAK_NULL; \
	(node)->next = (list)->first; \
	if ((list)->last) (list)->first->prev = (node); \
	else (list)->last = (node); \
	(list)->first = (node); \
} while(0)

#define HAK_DELETE_FROM_LIST(list, node) do { \
	if ((node)->prev) (node)->prev->next = (node)->next; \
	else (list)->first = (node)->next; \
	if ((node)->next) (node)->next->prev = (node)->prev; \
	else (list)->last = (node)->prev; \
} while(0)



#define HAK_APPEND_TO_OOP_LIST(hak, list, node_type, node, _link) do { \
	(node)->_link.next = (node_type)(hak)->_nil; \
	(node)->_link.prev = (list)->last; \
	if ((hak_oop_t)(list)->last != (hak)->_nil) (list)->last->_link.next = (node); \
	else (list)->first = (node); \
	(list)->last = (node); \
} while(0)

#define HAK_PREPPEND_TO_OOP_LIST(hak, list, node_type, node, _link) do { \
	(node)->_link.prev = (node_type)(hak)->_nil; \
	(node)->_link.next = (list)->first; \
	if ((hak_oop_t)(list)->first != (hak)->_nil) (list)->first->_link.prev = (node); \
	else (list)->last = (node); \
	(list)->first = (node); \
} while(0)

#define HAK_DELETE_FROM_OOP_LIST(hak, list, node, _link) do { \
	if ((hak_oop_t)(node)->_link.prev != (hak)->_nil) (node)->_link.prev->_link.next = (node)->_link.next; \
	else (list)->first = (node)->_link.next; \
	if ((hak_oop_t)(node)->_link.next != (hak)->_nil) (node)->_link.next->_link.prev = (node)->_link.prev; \
	else (list)->last = (node)->_link.prev; \
} while(0)

/*
#define HAK_CLEANUP_FROM_OOP_LIST(hak, list, node, _link) do { \
	HAK_DELETE_FROM_OOP_LIST (hak, list, node, _link); \
	(node)->_link.prev = (node_type)(hak)->_nil; \
	(node)->_link.next = (node_type)(hak)->_nil; \
} while(0);
*/

/* =========================================================================
 * ENDIAN CHANGE OF A CONSTANT
 * ========================================================================= */
#define HAK_CONST_BSWAP16(x) \
	((hak_uint16_t)((((hak_uint16_t)(x) & ((hak_uint16_t)0xff << 0)) << 8) | \
	                (((hak_uint16_t)(x) & ((hak_uint16_t)0xff << 8)) >> 8)))

#define HAK_CONST_BSWAP32(x) \
	((hak_uint32_t)((((hak_uint32_t)(x) & ((hak_uint32_t)0xff <<  0)) << 24) | \
	                (((hak_uint32_t)(x) & ((hak_uint32_t)0xff <<  8)) <<  8) | \
	                (((hak_uint32_t)(x) & ((hak_uint32_t)0xff << 16)) >>  8) | \
	                (((hak_uint32_t)(x) & ((hak_uint32_t)0xff << 24)) >> 24)))

#if defined(HAK_HAVE_UINT64_T)
#define HAK_CONST_BSWAP64(x) \
	((hak_uint64_t)((((hak_uint64_t)(x) & ((hak_uint64_t)0xff <<  0)) << 56) | \
	                (((hak_uint64_t)(x) & ((hak_uint64_t)0xff <<  8)) << 40) | \
	                (((hak_uint64_t)(x) & ((hak_uint64_t)0xff << 16)) << 24) | \
	                (((hak_uint64_t)(x) & ((hak_uint64_t)0xff << 24)) <<  8) | \
	                (((hak_uint64_t)(x) & ((hak_uint64_t)0xff << 32)) >>  8) | \
	                (((hak_uint64_t)(x) & ((hak_uint64_t)0xff << 40)) >> 24) | \
	                (((hak_uint64_t)(x) & ((hak_uint64_t)0xff << 48)) >> 40) | \
	                (((hak_uint64_t)(x) & ((hak_uint64_t)0xff << 56)) >> 56)))
#endif

#if defined(HAK_HAVE_UINT128_T)
#define HAK_CONST_BSWAP128(x) \
	((hak_uint128_t)((((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 0)) << 120) | \
	                 (((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 8)) << 104) | \
	                 (((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 16)) << 88) | \
	                 (((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 24)) << 72) | \
	                 (((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 32)) << 56) | \
	                 (((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 40)) << 40) | \
	                 (((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 48)) << 24) | \
	                 (((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 56)) << 8) | \
	                 (((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 64)) >> 8) | \
	                 (((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 72)) >> 24) | \
	                 (((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 80)) >> 40) | \
	                 (((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 88)) >> 56) | \
	                 (((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 96)) >> 72) | \
	                 (((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 104)) >> 88) | \
	                 (((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 112)) >> 104) | \
	                 (((hak_uint128_t)(x) & ((hak_uint128_t)0xff << 120)) >> 120)))
#endif

#if defined(HAK_ENDIAN_LITTLE)

#	if defined(HAK_HAVE_UINT16_T)
#	define HAK_CONST_NTOH16(x) HAK_CONST_BSWAP16(x)
#	define HAK_CONST_HTON16(x) HAK_CONST_BSWAP16(x)
#	define HAK_CONST_HTOBE16(x) HAK_CONST_BSWAP16(x)
#	define HAK_CONST_HTOLE16(x) (x)
#	define HAK_CONST_BE16TOH(x) HAK_CONST_BSWAP16(x)
#	define HAK_CONST_LE16TOH(x) (x)
#	endif

#	if defined(HAK_HAVE_UINT32_T)
#	define HAK_CONST_NTOH32(x) HAK_CONST_BSWAP32(x)
#	define HAK_CONST_HTON32(x) HAK_CONST_BSWAP32(x)
#	define HAK_CONST_HTOBE32(x) HAK_CONST_BSWAP32(x)
#	define HAK_CONST_HTOLE32(x) (x)
#	define HAK_CONST_BE32TOH(x) HAK_CONST_BSWAP32(x)
#	define HAK_CONST_LE32TOH(x) (x)
#	endif

#	if defined(HAK_HAVE_UINT64_T)
#	define HAK_CONST_NTOH64(x) HAK_CONST_BSWAP64(x)
#	define HAK_CONST_HTON64(x) HAK_CONST_BSWAP64(x)
#	define HAK_CONST_HTOBE64(x) HAK_CONST_BSWAP64(x)
#	define HAK_CONST_HTOLE64(x) (x)
#	define HAK_CONST_BE64TOH(x) HAK_CONST_BSWAP64(x)
#	define HAK_CONST_LE64TOH(x) (x)
#	endif

#	if defined(HAK_HAVE_UINT128_T)
#	define HAK_CONST_NTOH128(x) HAK_CONST_BSWAP128(x)
#	define HAK_CONST_HTON128(x) HAK_CONST_BSWAP128(x)
#	define HAK_CONST_HTOBE128(x) HAK_CONST_BSWAP128(x)
#	define HAK_CONST_HTOLE128(x) (x)
#	define HAK_CONST_BE128TOH(x) HAK_CONST_BSWAP128(x)
#	define HAK_CONST_LE128TOH(x) (x)
#endif

#elif defined(HAK_ENDIAN_BIG)

#	if defined(HAK_HAVE_UINT16_T)
#	define HAK_CONST_NTOH16(x) (x)
#	define HAK_CONST_HTON16(x) (x)
#	define HAK_CONST_HTOBE16(x) (x)
#	define HAK_CONST_HTOLE16(x) HAK_CONST_BSWAP16(x)
#	define HAK_CONST_BE16TOH(x) (x)
#	define HAK_CONST_LE16TOH(x) HAK_CONST_BSWAP16(x)
#	endif

#	if defined(HAK_HAVE_UINT32_T)
#	define HAK_CONST_NTOH32(x) (x)
#	define HAK_CONST_HTON32(x) (x)
#	define HAK_CONST_HTOBE32(x) (x)
#	define HAK_CONST_HTOLE32(x) HAK_CONST_BSWAP32(x)
#	define HAK_CONST_BE32TOH(x) (x)
#	define HAK_CONST_LE32TOH(x) HAK_CONST_BSWAP32(x)
#	endif

#	if defined(HAK_HAVE_UINT64_T)
#	define HAK_CONST_NTOH64(x) (x)
#	define HAK_CONST_HTON64(x) (x)
#	define HAK_CONST_HTOBE64(x) (x)
#	define HAK_CONST_HTOLE64(x) HAK_CONST_BSWAP64(x)
#	define HAK_CONST_BE64TOH(x) (x)
#	define HAK_CONST_LE64TOH(x) HAK_CONST_BSWAP64(x)
#	endif

#	if defined(HAK_HAVE_UINT128_T)
#	define HAK_CONST_NTOH128(x) (x)
#	define HAK_CONST_HTON128(x) (x)
#	define HAK_CONST_HTOBE128(x) (x)
#	define HAK_CONST_HTOLE128(x) HAK_CONST_BSWAP128(x)
#	define HAK_CONST_BE128TOH(x) (x)
#	define HAK_CONST_LE128TOH(x) HAK_CONST_BSWAP128(x)
#	endif

#else
#	error UNKNOWN ENDIAN
#endif


#if defined(HAK_HAVE_UINT16_T) && (HAK_SIZEOF_UINT16_T == HAK_SIZEOF_OOW_T)
#	define HAK_CONST_NTOHOOW(x) HAK_CONST_NTOH16(x)
#	define HAK_CONST_HTONOOW(x) HAK_CONST_HTON16(x)
#	define HAK_CONST_HTOBEOOW(x) HAK_CONST_HTOBE16(x)
#	define HAK_CONST_HTOLEOOW(x) HAK_CONST_HTOLE16(x)
#	define HAK_CONST_BEOOWTOH(x) HAK_CONST_BE16TOH(x)
#	define HAK_CONST_LEOOWTOH(x) HAK_CONST_LE16TOH(x)
#elif defined(HAK_HAVE_UINT32_T) && (HAK_SIZEOF_UINT32_T == HAK_SIZEOF_OOW_T)
#	define HAK_CONST_NTOHOOW(x) HAK_CONST_NTOH32(x)
#	define HAK_CONST_HTONOOW(x) HAK_CONST_HTON32(x)
#	define HAK_CONST_HTOBEOOW(x) HAK_CONST_HTOBE32(x)
#	define HAK_CONST_HTOLEOOW(x) HAK_CONST_HTOLE32(x)
#	define HAK_CONST_BEOOWTOH(x) HAK_CONST_BE32TOH(x)
#	define HAK_CONST_LEOOWTOH(x) HAK_CONST_LE32TOH(x)
#elif defined(HAK_HAVE_UINT64_T) && (HAK_SIZEOF_UINT64_T == HAK_SIZEOF_OOW_T)
#	define HAK_CONST_NTOHOOW(x) HAK_CONST_NTOH64(x)
#	define HAK_CONST_HTONOOW(x) HAK_CONST_HTON64(x)
#	define HAK_CONST_HTOBEOOW(x) HAK_CONST_HTOBE64(x)
#	define HAK_CONST_HTOLEOOW(x) HAK_CONST_HTOLE64(x)
#	define HAK_CONST_BEOOWTOH(x) HAK_CONST_BE64TOH(x)
#	define HAK_CONST_LEOOWTOH(x) HAK_CONST_LE64TOH(x)
#elif defined(HAK_HAVE_UINT128_T) && (HAK_SIZEOF_UINT128_T == HAK_SIZEOF_OOW_T)
#	define HAK_CONST_NTOHOOW(x) HAK_CONST_NTOH128(x)
#	define HAK_CONST_HTONOOW(x) HAK_CONST_HTON128(x)
#	define HAK_CONST_HTOBEOOW(x) HAK_CONST_HTOBE128(x)
#	define HAK_CONST_HTOLEOOW(x) HAK_CONST_HTOLE128(x)
#	define HAK_CONST_BEOOWTOH(x) HAK_CONST_BE128TOH(x)
#	define HAK_CONST_LEOOWTOH(x) HAK_CONST_LE128TOH(x)
#endif

#if defined(HAK_HAVE_UINT16_T) && (HAK_SIZEOF_UINT16_T == HAK_SIZEOF_OOHW_T)
#	define HAK_CONST_NTOHOOHW(x) HAK_CONST_NTOH16(x)
#	define HAK_CONST_HTONOOHW(x) HAK_CONST_HTON16(x)
#	define HAK_CONST_HTOBEOOHW(x) HAK_CONST_HTOBE16(x)
#	define HAK_CONST_HTOLEOOHW(x) HAK_CONST_HTOLE16(x)
#	define HAK_CONST_BEOOHWTOH(x) HAK_CONST_BE16TOH(x)
#	define HAK_CONST_LEOOHWTOH(x) HAK_CONST_LE16TOH(x)
#elif defined(HAK_HAVE_UINT32_T) && (HAK_SIZEOF_UINT32_T == HAK_SIZEOF_OOHW_T)
#	define HAK_CONST_NTOHOOHW(x) HAK_CONST_NTOH32(x)
#	define HAK_CONST_HTONOOHW(x) HAK_CONST_HTON32(x)
#	define HAK_CONST_HTOBEOOHW(x) HAK_CONST_HTOBE32(x)
#	define HAK_CONST_HTOLEOOHW(x) HAK_CONST_HTOLE32(x)
#	define HAK_CONST_BEOOHWTOH(x) HAK_CONST_BE32TOH(x)
#	define HAK_CONST_LEOOHWTOH(x) HAK_CONST_LE32TOH(x)
#elif defined(HAK_HAVE_UINT64_T) && (HAK_SIZEOF_UINT64_T == HAK_SIZEOF_OOHW_T)
#	define HAK_CONST_NTOHOOHW(x) HAK_CONST_NTOH64(x)
#	define HAK_CONST_HTONOOHW(x) HAK_CONST_HTON64(x)
#	define HAK_CONST_HTOBEOOHW(x) HAK_CONST_HTOBE64(x)
#	define HAK_CONST_HTOLEOOHW(x) HAK_CONST_HTOLE64(x)
#	define HAK_CONST_BEOOHWTOH(x) HAK_CONST_BE64TOH(x)
#	define HAK_CONST_LEOOHWTOH(x) HAK_CONST_LE64TOH(x)
#elif defined(HAK_HAVE_UINT128_T) && (HAK_SIZEOF_UINT128_T == HAK_SIZEOF_OOHW_T)
#	define HAK_CONST_NTOHOOHW(x) HAK_CONST_NTOH128(x)
#	define HAK_CONST_HTONOOHW(x) HAK_CONST_HTON128(x)
#	define HAK_CONST_HTOBEOOHW(x) HAK_CONST_HTOBE128(x)
#	define HAK_CONST_HTOLEOOHW(x) HAK_CONST_HTOLE128(x)
#	define HAK_CONST_BEOOHWTOH(x) HAK_CONST_BE128TOH(x)
#	define HAK_CONST_LEOOHWTOH(x) HAK_CONST_LE128TOH(x)
#endif

#if defined(HAK_USE_OOW_FOR_LIW)
#	define HAK_CONST_NTOHLIW(x)  HAK_CONST_NTOHOOW(x)
#	define HAK_CONST_HTONLIW(x)  HAK_CONST_HTONOOW(x)
#	define HAK_CONST_HTOBELIW(x) HAK_CONST_HTOBEOOW(x)
#	define HAK_CONST_HTOLELIW(x) HAK_CONST_HTOLEOOW(x)
#	define HAK_CONST_BELIWTOH(x) HAK_CONST_BEOOWTOH(x)
#	define HAK_CONST_LELIWTOH(x) HAK_CONST_LEOOWTOH(x)
#else
#	define HAK_CONST_NTOHLIW(x)  HAK_CONST_NTOHOOHW(x)
#	define HAK_CONST_HTONLIW(x)  HAK_CONST_HTONOOHW(x)
#	define HAK_CONST_HTOBELIW(x) HAK_CONST_HTOBEOOHW(x)
#	define HAK_CONST_HTOLELIW(x) HAK_CONST_HTOLEOOHW(x)
#	define HAK_CONST_BELIWTOH(x) HAK_CONST_BEOOHWTOH(x)
#	define HAK_CONST_LELIWTOH(x) HAK_CONST_LEOOHWTOH(x)
#endif

/* =========================================================================
 * HASH
 * ========================================================================= */

#if (HAK_SIZEOF_OOW_T == 4)
#	define HAK_HASH_FNV_MAGIC_INIT (0x811c9dc5)
#	define HAK_HASH_FNV_MAGIC_PRIME (0x01000193)
#elif (HAK_SIZEOF_OOW_T == 8)

#	define HAK_HASH_FNV_MAGIC_INIT (0xCBF29CE484222325)
#	define HAK_HASH_FNV_MAGIC_PRIME (0x100000001B3l)

#elif (HAK_SIZEOF_OOW_T == 16)
#	define HAK_HASH_FNV_MAGIC_INIT (0x6C62272E07BB014262B821756295C58D)
#	define HAK_HASH_FNV_MAGIC_PRIME (0x1000000000000000000013B)
#endif

#if defined(HAK_HASH_FNV_MAGIC_INIT)
	/* FNV-1 hash */
#	define HAK_HASH_INIT HAK_HASH_FNV_MAGIC_INIT
#	define HAK_HASH_VALUE(hv,v) (((hv) ^ (v)) * HAK_HASH_FNV_MAGIC_PRIME)

#else
	/* SDBM hash */
#	define HAK_HASH_INIT 0
#	define HAK_HASH_VALUE(hv,v) (((hv) << 6) + ((hv) << 16) - (hv) + (v))
#endif

#define HAK_HASH_VPTL(hv, ptr, len, type) do { \
	hv = HAK_HASH_INIT; \
	HAK_HASH_MORE_VPTL (hv, ptr, len, type); \
} while(0)

#define HAK_HASH_MORE_VPTL(hv, ptr, len, type) do { \
	type* __hak_hash_more_vptl_p = (type*)(ptr); \
	type* __hak_hash_more_vptl_q = (type*)__hak_hash_more_vptl_p + (len); \
	while (__hak_hash_more_vptl_p < __hak_hash_more_vptl_q) \
	{ \
		hv = HAK_HASH_VALUE(hv, *__hak_hash_more_vptl_p); \
		__hak_hash_more_vptl_p++; \
	} \
} while(0)

#define HAK_HASH_VPTR(hv, ptr, type) do { \
	hv = HAK_HASH_INIT; \
	HAK_HASH_MORE_VPTR (hv, ptr, type); \
} while(0)

#define HAK_HASH_MORE_VPTR(hv, ptr, type) do { \
	type* __hak_hash_more_vptr_p = (type*)(ptr); \
	while (*__hak_hash_more_vptr_p) \
	{ \
		hv = HAK_HASH_VALUE(hv, *__hak_hash_more_vptr_p); \
		__hak_hash_more_vptr_p++; \
	} \
} while(0)

#define HAK_HASH_BYTES(hv, ptr, len) HAK_HASH_VPTL(hv, ptr, len, const hak_uint8_t)
#define HAK_HASH_MORE_BYTES(hv, ptr, len) HAK_HASH_MORE_VPTL(hv, ptr, len, const hak_uint8_t)

#define HAK_HASH_BCHARS(hv, ptr, len) HAK_HASH_VPTL(hv, ptr, len, const hak_bch_t)
#define HAK_HASH_MORE_BCHARS(hv, ptr, len) HAK_HASH_MORE_VPTL(hv, ptr, len, const hak_bch_t)

#define HAK_HASH_UCHARS(hv, ptr, len) HAK_HASH_VPTL(hv, ptr, len, const hak_uch_t)
#define HAK_HASH_MORE_UCHARS(hv, ptr, len) HAK_HASH_MORE_VPTL(hv, ptr, len, const hak_uch_t)

#define HAK_HASH_BCSTR(hv, ptr) HAK_HASH_VPTR(hv, ptr, const hak_bch_t)
#define HAK_HASH_MORE_BCSTR(hv, ptr) HAK_HASH_MORE_VPTR(hv, ptr, const hak_bch_t)

#define HAK_HASH_UCSTR(hv, ptr) HAK_HASH_VPTR(hv, ptr, const hak_uch_t)
#define HAK_HASH_MORE_UCSTR(hv, ptr) HAK_HASH_MORE_VPTR(hv, ptr, const hak_uch_t)

/* =========================================================================
 * PATH-RELATED MACROS
 * ========================================================================= */
#if defined(_WIN32) || defined(__OS2__) || defined(__DOS__)
#	define HAK_DFL_PATH_SEP ('\\')
#	define HAK_ALT_PATH_SEP ('/')
#	define HAK_IS_PATH_SEP(c) ((c) == HAK_DFL_PATH_SEP || (c) == HAK_ALT_PATH_SEP)
#	define HAK_HAVE_ALT_PATH_SEP 1
#else
#	define HAK_DFL_PATH_SEP ('/')
#	define HAK_ALT_PATH_SEP ('/')
#	define HAK_IS_PATH_SEP(c) ((c) == HAK_DFL_PATH_SEP)
#	undef HAK_HAVE_ALT_PATH_SEP
#endif


/* TODO: handle path with a drive letter or in the UNC notation */
#define HAK_IS_PATH_ABSOLUTE(x) HAK_IS_PATH_SEP(x[0])


#if defined(__cplusplus)
extern "C" {
#endif

HAK_EXPORT hak_oow_t hak_hash_bytes_ (
	const hak_oob_t* ptr,
	hak_oow_t        len
);

#if defined(HAK_HAVE_INLINE)
static HAK_INLINE hak_oow_t hak_hash_bytes (const hak_oob_t* ptr, hak_oow_t len)
{
	hak_oow_t hv;
	HAK_HASH_BYTES (hv, ptr, len);
	/* constrain the hash value to be representable in a small integer
		* for convenience sake */
	return hv % ((hak_oow_t)HAK_SMOOI_MAX + 1);
}

static HAK_INLINE hak_oow_t hak_hash_bchars (const hak_bch_t* ptr, hak_oow_t len)
{
	return hak_hash_bytes((const hak_oob_t*)ptr, len * HAK_SIZEOF(hak_bch_t));
}

static HAK_INLINE hak_oow_t hak_hash_uchars (const hak_uch_t* ptr, hak_oow_t len)
{
	return hak_hash_bytes((const hak_oob_t*)ptr, len * HAK_SIZEOF(hak_uch_t));
}

static HAK_INLINE hak_oow_t hak_hash_words (const hak_oow_t* ptr, hak_oow_t len)
{
	return hak_hash_bytes((const hak_oob_t*)ptr, len * HAK_SIZEOF(hak_oow_t));
}

static HAK_INLINE hak_oow_t hak_hash_halfwords (const hak_oohw_t* ptr, hak_oow_t len)
{
	return hak_hash_bytes((const hak_oob_t*)ptr, len * HAK_SIZEOF(hak_oohw_t));
}

static HAK_INLINE hak_oow_t hak_hash_liwords(const hak_liw_t* ptr, hak_oow_t len)
{
	return hak_hash_bytes((const hak_oob_t*)ptr, len * HAK_SIZEOF(hak_liw_t));
}

#else
#	define hak_hash_bytes(ptr,len)     hak_hash_bytes_(ptr, len)
#	define hak_hash_bchars(ptr,len)    hak_hash_bytes((const hak_oob_t*)ptr, len * HAK_SIZEOF(hak_bch_t))
#	define hak_hash_uchars(ptr,len)    hak_hash_bytes((const hak_oob_t*)ptr, len * HAK_SIZEOF(hak_uch_t))
#	define hak_hash_words(ptr,len)     hak_hash_bytes((const hak_oob_t*)ptr, len * HAK_SIZEOF(hak_oow_t))
#	define hak_hash_halfwords(ptr,len) hak_hash_bytes((const hak_oob_t*)ptr, len * HAK_SIZEOF(hak_oohw_t))
#	define hak_hash_liwords(ptr,len)   hak_hash_bytes((const hak_oob_t*)ptr, len * HAK_SIZEOF(hak_liw_t))
#endif

#if defined(HAK_OOCH_IS_UCH)
#	define hak_hash_oochars(ptr,len) hak_hash_uchars(ptr,len)
#else
#	define hak_hash_oochars(ptr,len) hak_hash_bchars(ptr,len)
#endif

/* =========================================================================
 * TIME CALCULATION WITH OVERFLOW/UNDERFLOW DETECTION
 * ========================================================================= */

/**
 * The hak_add_ntime() function adds two time structures pointed to by \a x and \a y
 * and stores the result in the structure pointed to by \a z. If it detects overflow/
 * underflow, it stores the largest/least possible value respectively.
 * You may use the HAK_ADD_NTIME() macro if overflow/underflow check isn't needed.
 */
HAK_EXPORT void hak_add_ntime (
	hak_ntime_t*       z,
	const hak_ntime_t* x,
	const hak_ntime_t* y
);

/**
 * The hak_sub_ntime() function subtracts the time value \a y from the time value \a x
 * and stores the result in the structure pointed to by \a z. If it detects overflow/
 * underflow, it stores the largest/least possible value respectively.
 * You may use the HAK_SUB_NTIME() macro if overflow/underflow check isn't needed.
 */
HAK_EXPORT void hak_sub_ntime (
	hak_ntime_t*       z,
	const hak_ntime_t* x,
	const hak_ntime_t* y
);

/* =========================================================================
 * PATH NAME
 * ========================================================================= */

const hak_bch_t* hak_get_base_name_from_bcstr_path (
	const hak_bch_t* path
);

const hak_uch_t* hak_get_base_name_from_ucstr_path (
	const hak_uch_t* path
);

#if defined(HAK_OOCH_IS_UCH)
#define hak_get_base_name_from_path(x) hak_get_base_name_from_ucstr_path(x)
#else
#define hak_get_base_name_from_path(x) hak_get_base_name_from_bcstr_path(x)
#endif

/* =========================================================================
 * BIT SWAP
 * ========================================================================= */
#if defined(HAK_HAVE_INLINE)

#if defined(HAK_HAVE_UINT16_T)
static HAK_INLINE hak_uint16_t hak_bswap16 (hak_uint16_t x)
{
#if defined(HAK_HAVE_BUILTIN_BSWAP16)
	return __builtin_bswap16(x);
#elif defined(__GNUC__) && (defined(__x86_64) || defined(__amd64) || defined(__i386) || defined(i386))
	__asm__ /*volatile*/ ("xchgb %b0, %h0" : "=Q"(x): "0"(x));
	return x;
#elif defined(__GNUC__) && defined(__arm__) && (defined(__ARM_ARCH) && (__ARM_ARCH >= 6))
	__asm__ /*volatile*/ ("rev16 %0, %0" : "+r"(x));
	return x;
#else
	return (x << 8) | (x >> 8);
#endif
}
#endif

#if defined(HAK_HAVE_UINT32_T)
static HAK_INLINE hak_uint32_t hak_bswap32 (hak_uint32_t x)
{
#if defined(HAK_HAVE_BUILTIN_BSWAP32)
	return __builtin_bswap32(x);
#elif defined(__GNUC__) && (defined(__x86_64) || defined(__amd64) || defined(__i386) || defined(i386))
	__asm__ /*volatile*/ ("bswapl %0" : "=r"(x) : "0"(x));
	return x;
#elif defined(__GNUC__) && defined(__aarch64__)
	__asm__ /*volatile*/ ("rev32 %0, %0" : "+r"(x));
	return x;
#elif defined(__GNUC__) && defined(__arm__) && (defined(__ARM_ARCH) && (__ARM_ARCH >= 6))
	__asm__ /*volatile*/ ("rev %0, %0" : "+r"(x));
	return x;
#elif defined(__GNUC__) && defined(__ARM_ARCH)
	hak_uint32_t tmp;
	__asm__ /*volatile*/ (
		"eor %1, %0, %0, ror #16\n\t"
		"bic %1, %1, #0x00ff0000\n\t"
		"mov %0, %0, ror #8\n\t"
		"eor %0, %0, %1, lsr #8\n\t"
		:"+r"(x), "=&r"(tmp)
	);
	return x;
#else
	return ((x >> 24)) |
	       ((x >>  8) & ((hak_uint32_t)0xff << 8)) |
	       ((x <<  8) & ((hak_uint32_t)0xff << 16)) |
	       ((x << 24));
#endif
}
#endif

#if defined(HAK_HAVE_UINT64_T)
static HAK_INLINE hak_uint64_t hak_bswap64 (hak_uint64_t x)
{
#if defined(HAK_HAVE_BUILTIN_BSWAP64)
	return __builtin_bswap64(x);
#elif defined(__GNUC__) && (defined(__x86_64) || defined(__amd64))
	__asm__ /*volatile*/ ("bswapq %0" : "=r"(x) : "0"(x));
	return x;
#elif defined(__GNUC__) && defined(__aarch64__)
	__asm__ /*volatile*/ ("rev %0, %0" : "+r"(x));
	return x;
#else
	return ((x >> 56)) |
	       ((x >> 40) & ((hak_uint64_t)0xff << 8)) |
	       ((x >> 24) & ((hak_uint64_t)0xff << 16)) |
	       ((x >>  8) & ((hak_uint64_t)0xff << 24)) |
	       ((x <<  8) & ((hak_uint64_t)0xff << 32)) |
	       ((x << 24) & ((hak_uint64_t)0xff << 40)) |
	       ((x << 40) & ((hak_uint64_t)0xff << 48)) |
	       ((x << 56));
#endif
}
#endif

#if defined(HAK_HAVE_UINT128_T)
static HAK_INLINE hak_uint128_t hak_bswap128 (hak_uint128_t x)
{
#if defined(HAK_HAVE_BUILTIN_BSWAP128)
	return __builtin_bswap128(x);
#else
	return ((x >> 120)) |
	       ((x >> 104) & ((hak_uint128_t)0xff << 8)) |
	       ((x >>  88) & ((hak_uint128_t)0xff << 16)) |
	       ((x >>  72) & ((hak_uint128_t)0xff << 24)) |
	       ((x >>  56) & ((hak_uint128_t)0xff << 32)) |
	       ((x >>  40) & ((hak_uint128_t)0xff << 40)) |
	       ((x >>  24) & ((hak_uint128_t)0xff << 48)) |
	       ((x >>   8) & ((hak_uint128_t)0xff << 56)) |
	       ((x <<   8) & ((hak_uint128_t)0xff << 64)) |
	       ((x <<  24) & ((hak_uint128_t)0xff << 72)) |
	       ((x <<  40) & ((hak_uint128_t)0xff << 80)) |
	       ((x <<  56) & ((hak_uint128_t)0xff << 88)) |
	       ((x <<  72) & ((hak_uint128_t)0xff << 96)) |
	       ((x <<  88) & ((hak_uint128_t)0xff << 104)) |
	       ((x << 104) & ((hak_uint128_t)0xff << 112)) |
	       ((x << 120));
#endif
}
#endif

#else

#if defined(HAK_HAVE_UINT16_T)
#	if defined(HAK_HAVE_BUILTIN_BSWAP16)
#	define hak_bswap16(x) ((hak_uint16_t)__builtin_bswap16((hak_uint16_t)(x)))
#	else
#	define hak_bswap16(x) ((hak_uint16_t)(((hak_uint16_t)(x)) << 8) | (((hak_uint16_t)(x)) >> 8))
#	endif
#endif

#if defined(HAK_HAVE_UINT32_T)
#	if defined(HAK_HAVE_BUILTIN_BSWAP32)
#	define hak_bswap32(x) ((hak_uint32_t)__builtin_bswap32((hak_uint32_t)(x)))
#	else
#	define hak_bswap32(x) ((hak_uint32_t)(((((hak_uint32_t)(x)) >> 24)) | \
	                                      ((((hak_uint32_t)(x)) >>  8) & ((hak_uint32_t)0xff << 8)) | \
	                                      ((((hak_uint32_t)(x)) <<  8) & ((hak_uint32_t)0xff << 16)) | \
	                                      ((((hak_uint32_t)(x)) << 24))))
#	endif
#endif

#if defined(HAK_HAVE_UINT64_T)
#	if defined(HAK_HAVE_BUILTIN_BSWAP64)
#	define hak_bswap64(x) ((hak_uint64_t)__builtin_bswap64((hak_uint64_t)(x)))
#	else
#	define hak_bswap64(x) ((hak_uint64_t)(((((hak_uint64_t)(x)) >> 56)) | \
	                                      ((((hak_uint64_t)(x)) >> 40) & ((hak_uint64_t)0xff << 8)) | \
	                                      ((((hak_uint64_t)(x)) >> 24) & ((hak_uint64_t)0xff << 16)) | \
	                                      ((((hak_uint64_t)(x)) >>  8) & ((hak_uint64_t)0xff << 24)) | \
	                                      ((((hak_uint64_t)(x)) <<  8) & ((hak_uint64_t)0xff << 32)) | \
	                                      ((((hak_uint64_t)(x)) << 24) & ((hak_uint64_t)0xff << 40)) | \
	                                      ((((hak_uint64_t)(x)) << 40) & ((hak_uint64_t)0xff << 48)) | \
	                                      ((((hak_uint64_t)(x)) << 56))))
#	endif
#endif

#if defined(HAK_HAVE_UINT128_T)
#	if defined(HAK_HAVE_BUILTIN_BSWAP128)
#	define hak_bswap128(x) ((hak_uint128_t)__builtin_bswap128((hak_uint128_t)(x)))
#	else
#	define hak_bswap128(x) ((hak_uint128_t)(((((hak_uint128_t)(x)) >> 120)) |  \
	                                        ((((hak_uint128_t)(x)) >> 104) & ((hak_uint128_t)0xff << 8)) | \
	                                        ((((hak_uint128_t)(x)) >>  88) & ((hak_uint128_t)0xff << 16)) | \
	                                        ((((hak_uint128_t)(x)) >>  72) & ((hak_uint128_t)0xff << 24)) | \
	                                        ((((hak_uint128_t)(x)) >>  56) & ((hak_uint128_t)0xff << 32)) | \
	                                        ((((hak_uint128_t)(x)) >>  40) & ((hak_uint128_t)0xff << 40)) | \
	                                        ((((hak_uint128_t)(x)) >>  24) & ((hak_uint128_t)0xff << 48)) | \
	                                        ((((hak_uint128_t)(x)) >>   8) & ((hak_uint128_t)0xff << 56)) | \
	                                        ((((hak_uint128_t)(x)) <<   8) & ((hak_uint128_t)0xff << 64)) | \
	                                        ((((hak_uint128_t)(x)) <<  24) & ((hak_uint128_t)0xff << 72)) | \
	                                        ((((hak_uint128_t)(x)) <<  40) & ((hak_uint128_t)0xff << 80)) | \
	                                        ((((hak_uint128_t)(x)) <<  56) & ((hak_uint128_t)0xff << 88)) | \
	                                        ((((hak_uint128_t)(x)) <<  72) & ((hak_uint128_t)0xff << 96)) | \
	                                        ((((hak_uint128_t)(x)) <<  88) & ((hak_uint128_t)0xff << 104)) | \
	                                        ((((hak_uint128_t)(x)) << 104) & ((hak_uint128_t)0xff << 112)) | \
	                                        ((((hak_uint128_t)(x)) << 120))))
#	endif
#endif

#endif /* HAK_HAVE_INLINE */


#if defined(HAK_ENDIAN_LITTLE)

#	if defined(HAK_HAVE_UINT16_T)
#	define hak_hton16(x) hak_bswap16(x)
#	define hak_ntoh16(x) hak_bswap16(x)
#	define hak_htobe16(x) hak_bswap16(x)
#	define hak_be16toh(x) hak_bswap16(x)
#	define hak_htole16(x) ((hak_uint16_t)(x))
#	define hak_le16toh(x) ((hak_uint16_t)(x))
#	endif

#	if defined(HAK_HAVE_UINT32_T)
#	define hak_hton32(x) hak_bswap32(x)
#	define hak_ntoh32(x) hak_bswap32(x)
#	define hak_htobe32(x) hak_bswap32(x)
#	define hak_be32toh(x) hak_bswap32(x)
#	define hak_htole32(x) ((hak_uint32_t)(x))
#	define hak_le32toh(x) ((hak_uint32_t)(x))
#	endif

#	if defined(HAK_HAVE_UINT64_T)
#	define hak_hton64(x) hak_bswap64(x)
#	define hak_ntoh64(x) hak_bswap64(x)
#	define hak_htobe64(x) hak_bswap64(x)
#	define hak_be64toh(x) hak_bswap64(x)
#	define hak_htole64(x) ((hak_uint64_t)(x))
#	define hak_le64toh(x) ((hak_uint64_t)(x))
#	endif

#	if defined(HAK_HAVE_UINT128_T)
#	define hak_hton128(x) hak_bswap128(x)
#	define hak_ntoh128(x) hak_bswap128(x)
#	define hak_htobe128(x) hak_bswap128(x)
#	define hak_be128toh(x) hak_bswap128(x)
#	define hak_htole128(x) ((hak_uint128_t)(x))
#	define hak_le128toh(x) ((hak_uint128_t)(x))
#	endif

#elif defined(HAK_ENDIAN_BIG)

#	if defined(HAK_HAVE_UINT16_T)
#	define hak_hton16(x) ((hak_uint16_t)(x))
#	define hak_ntoh16(x) ((hak_uint16_t)(x))
#	define hak_htobe16(x) ((hak_uint16_t)(x))
#	define hak_be16toh(x) ((hak_uint16_t)(x))
#	define hak_htole16(x) hak_bswap16(x)
#	define hak_le16toh(x) hak_bswap16(x)
#	endif

#	if defined(HAK_HAVE_UINT32_T)
#	define hak_hton32(x) ((hak_uint32_t)(x))
#	define hak_ntoh32(x) ((hak_uint32_t)(x))
#	define hak_htobe32(x) ((hak_uint32_t)(x))
#	define hak_be32toh(x) ((hak_uint32_t)(x))
#	define hak_htole32(x) hak_bswap32(x)
#	define hak_le32toh(x) hak_bswap32(x)
#	endif

#	if defined(HAK_HAVE_UINT64_T)
#	define hak_hton64(x) ((hak_uint64_t)(x))
#	define hak_ntoh64(x) ((hak_uint64_t)(x))
#	define hak_htobe64(x) ((hak_uint64_t)(x))
#	define hak_be64toh(x) ((hak_uint64_t)(x))
#	define hak_htole64(x) hak_bswap64(x)
#	define hak_le64toh(x) hak_bswap64(x)
#	endif

#	if defined(HAK_HAVE_UINT128_T)
#	define hak_hton128(x) ((hak_uint128_t)(x))
#	define hak_ntoh128(x) ((hak_uint128_t)(x))
#	define hak_htobe128(x) ((hak_uint128_t)(x))
#	define hak_be128toh(x) ((hak_uint128_t)(x))
#	define hak_htole128(x) hak_bswap128(x)
#	define hak_le128toh(x) hak_bswap128(x)
#	endif

#else
#	error UNKNOWN ENDIAN
#endif

#if defined(HAK_HAVE_UINT16_T) && (HAK_SIZEOF_UINT16_T == HAK_SIZEOF_OOW_T)
#	define hak_ntohoow(x) hak_ntoh16(x)
#	define hak_htonoow(x) hak_hton16(x)
#	define hak_htobeoow(x) hak_htobe116(x)
#	define hak_beoowtoh(x) hak_be16toh(x)
#	define hak_htoleoow(x) hak_htole16(x)
#	define hak_leoowtoh(x) hak_le16toh(x)
#elif defined(HAK_HAVE_UINT32_T) && (HAK_SIZEOF_UINT32_T == HAK_SIZEOF_OOW_T)
#	define hak_ntohoow(x) hak_ntoh32(x)
#	define hak_htonoow(x) hak_hton32(x)
#	define hak_htobeoow(x) hak_htobe32(x)
#	define hak_beoowtoh(x) hak_be32toh(x)
#	define hak_htoleoow(x) hak_htole32(x)
#	define hak_leoowtoh(x) hak_le32toh(x)
#elif defined(HAK_HAVE_UINT64_T) && (HAK_SIZEOF_UINT64_T == HAK_SIZEOF_OOW_T)
#	define hak_ntohoow(x) hak_ntoh64(x)
#	define hak_htonoow(x) hak_hton64(x)
#	define hak_htobeoow(x) hak_htobe64(x)
#	define hak_beoowtoh(x) hak_be64toh(x)
#	define hak_htoleoow(x) hak_htole64(x)
#	define hak_leoowtoh(x) hak_le64toh(x)
#elif defined(HAK_HAVE_UINT128_T) && (HAK_SIZEOF_UINT128_T == HAK_SIZEOF_OOW_T)
#	define hak_ntohoow(x) hak_ntoh128(x)
#	define hak_htonoow(x) hak_hton128(x)
#	define hak_htobeoow(x) hak_htobe128(x)
#	define hak_beoowtoh(x) hak_be128toh(x)
#	define hak_htoleoow(x) hak_htole128(x)
#	define hak_leoowtoh(x) hak_le128toh(x)
#endif


#if defined(HAK_HAVE_UINT16_T) && (HAK_SIZEOF_UINT16_T == HAK_SIZEOF_OOHW_T)
#	define hak_ntohoohw(x) hak_ntoh16(x)
#	define hak_htonoohw(x) hak_hton16(x)
#	define hak_htobeoohw(x) hak_htobe116(x)
#	define hak_beoohwtoh(x) hak_be16toh(x)
#	define hak_htoleoohw(x) hak_htole16(x)
#	define hak_leoohwtoh(x) hak_le16toh(x)
#elif defined(HAK_HAVE_UINT32_T) && (HAK_SIZEOF_UINT32_T == HAK_SIZEOF_OOHW_T)
#	define hak_ntohoohw(x) hak_ntoh32(x)
#	define hak_htonoohw(x) hak_hton32(x)
#	define hak_htobeoohw(x) hak_htobe32(x)
#	define hak_beoohwtoh(x) hak_be32toh(x)
#	define hak_htoleoohw(x) hak_htole32(x)
#	define hak_leoohwtoh(x) hak_le32toh(x)
#elif defined(HAK_HAVE_UINT64_T) && (HAK_SIZEOF_UINT64_T == HAK_SIZEOF_OOHW_T)
#	define hak_ntohoohw(x) hak_ntoh64(x)
#	define hak_htonoohw(x) hak_hton64(x)
#	define hak_htobeoohw(x) hak_htobe64(x)
#	define hak_beoohwtoh(x) hak_be64toh(x)
#	define hak_htoleoohw(x) hak_htole64(x)
#	define hak_leoohwtoh(x) hak_le64toh(x)
#elif defined(HAK_HAVE_UINT128_T) && (HAK_SIZEOF_UINT128_T == HAK_SIZEOF_OOHW_T)
#	define hak_ntohoohw(x) hak_ntoh128(x)
#	define hak_htonoohw(x) hak_hton128(x)
#	define hak_htobeoohw(x) hak_htobe128(x)
#	define hak_beoohwtoh(x) hak_be128toh(x)
#	define hak_htoleoohw(x) hak_htole128(x)
#	define hak_leoohwtoh(x) hak_le128toh(x)
#endif

#if defined(HAK_USE_OOW_FOR_LIW)
#	define hak_ntohliw(x)  hak_ntohoow(x)
#	define hak_htonliw(x)  hak_htonoow(x)
#	define hak_htobeliw(x) hak_htobeoow(x)
#	define hak_beliwtoh(x) hak_beoowtoh(x)
#	define hak_htoleliw(x) hak_htoleoow(x)
#	define hak_leliwtoh(x) hak_leoowtoh(x)
#else
#	define hak_ntohliw(x)  hak_ntohoohw(x)
#	define hak_htonliw(x)  hak_htonoohw(x)
#	define hak_htobeliw(x) hak_htobeoohw(x)
#	define hak_beliwtoh(x) hak_beoohwtoh(x)
#	define hak_htoleliw(x) hak_htoleoohw(x)
#	define hak_leliwtoh(x) hak_leoohwtoh(x)
#endif

/* =========================================================================
 * BIT POSITION
 * ========================================================================= */
static HAK_INLINE int hak_get_pos_of_msb_set_pow2 (hak_oow_t x)
{
	/* the caller must ensure that x is power of 2. if x happens to be zero,
	 * the return value is undefined as each method used may give different result. */
#if defined(HAK_HAVE_BUILTIN_CTZLL) && (HAK_SIZEOF_OOW_T == HAK_SIZEOF_LONG_LONG)
	return __builtin_ctzll(x); /* count the number of trailing zeros */
#elif defined(HAK_HAVE_BUILTIN_CTZL) && (HAK_SIZEOF_OOW_T == HAK_SIZEOF_LONG)
	return __builtin_ctzl(x); /* count the number of trailing zeros */
#elif defined(HAK_HAVE_BUILTIN_CTZ) && (HAK_SIZEOF_OOW_T == HAK_SIZEOF_INT)
	return __builtin_ctz(x); /* count the number of trailing zeros */
#elif defined(__GNUC__) && (defined(__x86_64) || defined(__amd64) || defined(__i386) || defined(i386))
	hak_oow_t pos;
	/* use the Bit Scan Forward instruction */
#if 1
	__asm__ volatile (
		"bsf %1,%0\n\t"
		: "=r"(pos) /* output */
		: "r"(x) /* input */
	);
#else
	__asm__ volatile (
		"bsf %[X],%[EXP]\n\t"
		: [EXP]"=r"(pos) /* output */
		: [X]"r"(x) /* input */
	);
#endif
	return (int)pos;
#elif defined(__GNUC__) && defined(__aarch64__) || (defined(__arm__) && (defined(__ARM_ARCH) && (__ARM_ARCH >= 5)))
	hak_oow_t n;
	/* CLZ is available in ARMv5T and above. there is no instruction to
	 * count trailing zeros or something similar. using RBIT with CLZ
	 * would be good in ARMv6T2 and above to avoid further calculation
	 * afte CLZ */
	__asm__ volatile (
		"clz %0,%1\n\t"
		: "=r"(n) /* output */
		: "r"(x) /* input */
	);
	return (int)(HAK_OOW_BITS - n - 1);
	/* TODO: PPC - use cntlz, cntlzw, cntlzd, SPARC - use lzcnt, MIPS clz */
#else
	int pos = 0;
	while (x >>= 1) pos++;
	return pos;
#endif
}

static HAK_INLINE int hak_get_pos_of_msb_set (hak_oow_t x)
{
	/* x doesn't have to be power of 2. if x is zero, the result is undefined */
#if defined(HAK_HAVE_BUILTIN_CLZLL) && (HAK_SIZEOF_OOW_T == HAK_SIZEOF_LONG_LONG)
	return HAK_OOW_BITS - __builtin_clzll(x) - 1; /* count the number of leading zeros */
#elif defined(HAK_HAVE_BUILTIN_CLZL) && (HAK_SIZEOF_OOW_T == HAK_SIZEOF_LONG)
	return HAK_OOW_BITS - __builtin_clzl(x) - 1; /* count the number of leading zeros */
#elif defined(HAK_HAVE_BUILTIN_CLZ) && (HAK_SIZEOF_OOW_T == HAK_SIZEOF_INT)
	return HAK_OOW_BITS - __builtin_clz(x) - 1; /* count the number of leading zeros */
#elif defined(__GNUC__) && (defined(__x86_64) || defined(__amd64) || defined(__i386) || defined(i386))
	/* bit scan reverse. not all x86 CPUs have LZCNT. */
	hak_oow_t pos;
	__asm__ volatile (
		"bsr %1,%0\n\t"
		: "=r"(pos) /* output */
		: "r"(x) /* input */
	);
	return (int)pos;
#elif defined(__GNUC__) && defined(__aarch64__) || (defined(__arm__) && (defined(__ARM_ARCH) && (__ARM_ARCH >= 5)))
	hak_oow_t n;
	__asm__ volatile (
		"clz %0,%1\n\t"
		: "=r"(n) /* output */
		: "r"(x) /* input */
	);
	return (int)(HAK_OOW_BITS - n - 1);
	/* TODO: PPC - use cntlz, cntlzw, cntlzd, SPARC - use lzcnt, MIPS clz */
#else
	int pos = 0;
	while (x >>= 1) pos++;
	return pos;
#endif
}
#if defined(__cplusplus)
}
#endif


#endif
