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

#ifndef _HAK_X_T_
#define _HAK_X_T_

#include <hak.h>
#include <sys/uio.h>

enum hak_xpkt_type_t
{
	/* the actual type field is 4 bits long. up to 16 types are possible */

	HAK_XPKT_CODE,      /* [C]->[S] input code */
	HAK_XPKT_EXECUTE,   /* [C]->[S] command to execute the code input */

	HAK_XPKT_ERROR,     /* [S]->[C] error indicator */
	HAK_XPKT_RETVAL,    /* [S]->[C] return value */

	HAK_XPKT_STDIN,     /* [C]->[S] */
	HAK_XPKT_STDOUT,    /* [S]->[C] output to stdout */
	HAK_XPKT_STDERR,    /* [S]->[C] output to stderr or output data related to error */

	HAK_XPKT_INFORM,    /* [S]->[C], [C]->[S] */

	/*TODO: define HAK_XPKT_CONTROL and make LIST_WORKS AND KILL_WORKER sub-commands of CONTORL */
	HAK_XPKT_LIST_WORKERS, /* [C]->[S] */
	HAK_XPKT_KILL_WORKER,  /* [C]->[S] */

	HAK_XPKT_DISCONNECT    /* [C]->[S], [S]->[C] */
};
typedef enum hak_xpkt_type_t hak_xpkt_type_t;

#include "hak-pac1.h"
struct hak_xpkt_hdr_t
{
	hak_uint8_t id;
	hak_uint8_t type; /* lower 4 bits represent the actual type.
	                     the upper 4 bits are part of the length extending the length to 12 bits */
	hak_uint8_t len;
};
typedef struct hak_xpkt_hdr_t hak_xpkt_hdr_t;
#include "hak-upac.h"

#define HAK_XPKT_HDR_LEN (HAK_SIZEOF(hak_xpkt_hdr_t))

/* the actual length field is 12 bits long. so the maximum payload length allowed per packet is 2^12 - 1 */
#define HAK_XPKT_MAX_PLD_LEN (4095)

/* ---------------------------------------------------------------------- */

typedef struct hak_xproto_t hak_xproto_t;

typedef int (*hak_xproto_cb_on_packet) (
	hak_xproto_t*   proto,
	hak_xpkt_type_t type,
	const void*     data,
	hak_oow_t       len
);

struct hak_xproto_cb_t
{
	hak_xproto_cb_on_packet on_packet;
};
typedef struct hak_xproto_cb_t hak_xproto_cb_t;

/* ---------------------------------------------------------------------- */

/* forward declaration to skip including <sys/uio.h> just for struct iovec */
typedef struct iovec hak_iovec_t;

/* ---------------------------------------------------------------------- */

typedef struct hak_server_proto_t hak_server_proto_t;
typedef struct hak_server_worker_t hak_server_worker_t;
typedef struct hak_server_t hak_server_t;

enum hak_server_option_t
{
	HAK_SERVER_TRAIT,
	HAK_SERVER_LOG_MASK,
	HAK_SERVER_WORKER_MAX_COUNT,
	HAK_SERVER_WORKER_STACK_SIZE,
	HAK_SERVER_WORKER_IDLE_TIMEOUT,
	HAK_SERVER_ACTOR_HEAP_SIZE,
	HAK_SERVER_ACTOR_MAX_RUNTIME,
	HAK_SERVER_SCRIPT_INCLUDE_PATH,
	HAK_SERVER_MODULE_INCTX
};
typedef enum hak_server_option_t hak_server_option_t;

enum hak_server_trait_t
{
#if defined(HAK_BUILD_DEBUG)
	HAK_SERVER_TRAIT_DEBUG_GC         = (1 << 0),
	HAK_SERVER_TRAIT_DEBUG_BIGINT     = (1 << 1)
#endif
};
typedef enum hak_server_trait_t hak_server_trait_t;

#define HAK_SERVER_WID_INVALID ((hak_oow_t)-1)
#define HAK_SERVER_WID_MAX (HAK_SERVER_WID_INVALID - 1)

typedef void (*hak_server_log_write_t) (
	hak_server_t*     server,
	hak_oow_t         wid,
	hak_bitmask_t     mask,
	const hak_ooch_t* msg,
	hak_oow_t         len
);

struct hak_server_prim_t
{
	hak_server_log_write_t log_write;
};
typedef struct hak_server_prim_t hak_server_prim_t;


/* ---------------------------------------------------------------------- */


typedef struct hak_client_t hak_client_t;

enum hak_client_option_t
{
	HAK_CLIENT_TRAIT,
	HAK_CLIENT_LOG_MASK,
};
typedef enum hak_client_option_t hak_client_option_t;

enum hak_client_trait_t
{
	/* no trait defined at this moment. XXXX is just a placeholder */
	HAK_CLIENT_XXXX  = (1 << 0)
};
typedef enum hak_client_trait_t hak_client_trait_t;


typedef void (*hak_client_log_write_t) (
	hak_client_t*     client,
	hak_bitmask_t     mask,
	const hak_ooch_t* msg,
	hak_oow_t         len
);

typedef int (*hak_client_on_packet_t) (
	hak_client_t*   client,
	hak_xpkt_type_t type,
	const void*     data,
	hak_oow_t       len
);

struct hak_client_prim_t
{
	hak_client_log_write_t     log_write;
	hak_client_on_packet_t     on_packet;
};
typedef struct hak_client_prim_t hak_client_prim_t;


/* ---------------------------------------------------------------------- */

#if (HAK_SIZEOF_SOCKLEN_T == 1)
	#if defined(HAK_SOCKLEN_T_IS_SIGNED)
		typedef hak_int8_t hak_scklen_t;
	#else
		typedef hak_uint8_t hak_scklen_t;
	#endif
#elif (HAK_SIZEOF_SOCKLEN_T == 2)
	#if defined(HAK_SOCKLEN_T_IS_SIGNED)
		typedef hak_int16_t hak_scklen_t;
	#else
		typedef hak_uint16_t hak_scklen_t;
	#endif
#elif (HAK_SIZEOF_SOCKLEN_T == 4)
	#if defined(HAK_SOCKLEN_T_IS_SIGNED)
		typedef hak_int32_t hak_scklen_t;
	#else
		typedef hak_uint32_t hak_scklen_t;
	#endif
#elif (HAK_SIZEOF_SOCKLEN_T == 8)
	#if defined(HAK_SOCKLEN_T_IS_SIGNED)
		typedef hak_int64_t hak_scklen_t;
	#else
		typedef hak_uint64_t hak_scklen_t;
	#endif
#else
	#undef HAK_SIZEOF_SOCKLEN_T
	#define HAK_SIZEOF_SOCKLEN_T HAK_SIZEOF_INT
	#define HAK_SOCKLEN_T_IS_SIGNED
	typedef int hak_scklen_t;
#endif

struct hak_sckaddr_t
{
#define HAK_SCKADDR_DATA_SIZE 0

#if (HAK_SIZEOF_STRUCT_SOCKADDR_IN > HAK_SCKADDR_DATA_SIZE)
	#undef HAK_SCKADDR_DATA_SIZE
	#define HAK_SCKADDR_DATA_SIZE HAK_SIZEOF_STRUCT_SOCKADDR_IN
#endif
#if (HAK_SIZEOF_STRUCT_SOCKADDR_IN6 > HAK_SCKADDR_DATA_SIZE)
	#undef HAK_SCKADDR_DATA_SIZE
	#define HAK_SCKADDR_DATA_SIZE HAK_SIZEOF_STRUCT_SOCKADDR_IN6
#endif
#if (HAK_SIZEOF_STRUCT_SOCKADDR_UN > HAK_SCKADDR_DATA_SIZE)
	#undef HAK_SCKADDR_DATA_SIZE
	#define HAK_SCKADDR_DATA_SIZE HAK_SIZEOF_STRUCT_SOCKADDR_UN
#endif
#if (HAK_SIZEOF_STRUCT_SOCKADDR_LL > HAK_SCKADDR_DATA_SIZE)
	#undef HAK_SCKADDR_DATA_SIZE
	#define HAK_SCKADDR_DATA_SIZE HAK_SIZEOF_STRUCT_SOCKADDR_LL
#endif

#if (HAK_SCKADDR_DATA_SIZE == 0)
	#undef HAK_SCKADDR_DATA_SIZE
	#define HAK_SCKADDR_DATA_SIZE 64
#endif
	hak_uint8_t storage[HAK_SCKADDR_DATA_SIZE];
};
typedef struct hak_sckaddr_t hak_sckaddr_t;

/* ---------------------------------------------------------------------- */

#if defined(__cplusplus)
extern "C" {
#endif

HAK_EXPORT hak_server_t* hak_server_open (
	hak_mmgr_t*        mmgr,
	hak_oow_t          xtnsize,
	hak_server_prim_t* prim,
	hak_errnum_t*      errnum
);

HAK_EXPORT void hak_server_close (
	hak_server_t* server
);

HAK_EXPORT int hak_server_start (
	hak_server_t*    server,
	const hak_bch_t* addrs
);

HAK_EXPORT void hak_server_stop (
	hak_server_t* server
);

HAK_EXPORT int hak_server_setoption (
	hak_server_t*       server,
	hak_server_option_t id,
	const void*         value
);

HAK_EXPORT int hak_server_getoption (
	hak_server_t*       server,
	hak_server_option_t id,
	void*               value
);

HAK_EXPORT void* hak_server_getxtn (
	hak_server_t* server
);

HAK_EXPORT hak_mmgr_t* hak_server_getmmgr (
	hak_server_t* server
);


HAK_EXPORT hak_cmgr_t* hak_server_getcmgr (
	hak_server_t* server
);

HAK_EXPORT void hak_server_setcmgr (
	hak_server_t* server,
	hak_cmgr_t*   cmgr
);

HAK_EXPORT hak_errnum_t hak_server_geterrnum (
	hak_server_t* server
);

HAK_EXPORT const hak_ooch_t* hak_server_geterrstr (
	hak_server_t* server
);

HAK_EXPORT const hak_ooch_t* hak_server_geterrmsg (
	hak_server_t* server
);

HAK_EXPORT void hak_server_seterrnum (
	hak_server_t* server,
	hak_errnum_t  errnum
);

HAK_EXPORT void hak_server_seterrbfmt (
	hak_server_t*    server,
	hak_errnum_t     errnum,
	const hak_bch_t* fmt,
	...
);

HAK_EXPORT void hak_server_seterrufmt (
	hak_server_t*    server,
	hak_errnum_t     errnum,
	const hak_uch_t* fmt,
	...
);

HAK_EXPORT void hak_server_logbfmt (
	hak_server_t*    server,
	hak_bitmask_t  mask,
	const hak_bch_t* fmt,
	...
);

HAK_EXPORT void hak_server_logufmt (
	hak_server_t*    server,
	hak_bitmask_t  mask,
	const hak_uch_t* fmt,
	...
);


HAK_EXPORT void* hak_server_allocmem (
	hak_server_t* server,
	hak_oow_t     size
);

HAK_EXPORT void* hak_server_callocmem (
	hak_server_t* server,
	hak_oow_t     size
);

HAK_EXPORT void* hak_server_reallocmem (
	hak_server_t* server,
	void*         ptr,
	hak_oow_t     size
);


HAK_EXPORT void hak_server_freemem (
	hak_server_t* server,
	void*         ptr
);


/* ---------------------------------------------------------------------- */

HAK_EXPORT hak_client_t* hak_client_open (
	hak_mmgr_t*        mmgr,
	hak_oow_t          xtnsize,
	hak_client_prim_t* prim,
	hak_errnum_t*      errnum
);

HAK_EXPORT void hak_client_close (
	hak_client_t* client
);

HAK_EXPORT int hak_client_start (
	hak_client_t* client,
	const char*   ipaddr,
	int           shut_wr_after_req
);

HAK_EXPORT void hak_client_stop (
	hak_client_t* client
);

HAK_EXPORT int hak_client_setoption (
	hak_client_t*       client,
	hak_client_option_t id,
	const void*         value
);

HAK_EXPORT int hak_client_getoption (
	hak_client_t*       client,
	hak_client_option_t id,
	void*               value
);


HAK_EXPORT void* hak_client_getxtn (
	hak_client_t* client
);

HAK_EXPORT hak_mmgr_t* hak_client_getmmgr (
	hak_client_t* client
);

HAK_EXPORT hak_cmgr_t* hak_client_getcmgr (
	hak_client_t* client
);

HAK_EXPORT void hak_client_setcmgr (
	hak_client_t* client,
	hak_cmgr_t*   cmgr
);


HAK_EXPORT hak_errnum_t hak_client_geterrnum (
	hak_client_t* client
);

HAK_EXPORT const hak_ooch_t* hak_client_geterrstr (
	hak_client_t* client
);

HAK_EXPORT const hak_ooch_t* hak_client_geterrmsg (
	hak_client_t* client
);

HAK_EXPORT const hak_bch_t* hak_client_geterrbmsg (
	hak_client_t* client
);

HAK_EXPORT const hak_uch_t* hak_client_geterrumsg (
	hak_client_t* client
);


HAK_EXPORT void hak_client_seterrnum (
	hak_client_t* client,
	hak_errnum_t  errnum
);

HAK_EXPORT void hak_client_seterrbfmt (
	hak_client_t*    client,
	hak_errnum_t     errnum,
	const hak_bch_t* fmt,
	...
);

HAK_EXPORT void hak_client_seterrufmt (
	hak_client_t*    client,
	hak_errnum_t     errnum,
	const hak_uch_t* fmt,
	...
);

HAK_EXPORT void hak_client_logbfmt (
	hak_client_t*    client,
	hak_bitmask_t  mask,
	const hak_bch_t* fmt,
	...
);

HAK_EXPORT void hak_client_logufmt (
	hak_client_t*    client,
	hak_bitmask_t  mask,
	const hak_uch_t* fmt,
	...
);

HAK_EXPORT void* hak_client_allocmem (
	hak_client_t* client,
	hak_oow_t     size
);

HAK_EXPORT void* hak_client_callocmem (
	hak_client_t* client,
	hak_oow_t     size
);

HAK_EXPORT void* hak_client_reallocmem (
	hak_client_t* client,
	void*         ptr,
	hak_oow_t     size
);


HAK_EXPORT void hak_client_freemem (
	hak_client_t* client,
	void*         ptr
);


/* ---------------------------------------------------------------------- */

HAK_EXPORT hak_xproto_t* hak_xproto_open (
	hak_mmgr_t*      mmgr,
	hak_xproto_cb_t* cb,
	hak_oow_t        xtnsize
);

HAK_EXPORT void hak_xproto_close (
	hak_xproto_t*  proto
);

HAK_EXPORT void* hak_xproto_getxtn (
	hak_xproto_t*  proto
);

HAK_EXPORT hak_uint8_t* hak_xproto_getbuf (
	hak_xproto_t*  proto,
	hak_oow_t*     capa
);

HAK_EXPORT int hak_xproto_geteof (
	hak_xproto_t*  proto
);

HAK_EXPORT void hak_xproto_seteof (
	hak_xproto_t*  proto,
	int            v
);

HAK_EXPORT void hak_xproto_advbuf (
	hak_xproto_t*  proto,
	hak_oow_t      inc
);

HAK_EXPORT int hak_xproto_ready (
	hak_xproto_t*  proto
);

HAK_EXPORT int hak_xproto_process (
	hak_xproto_t*  proto
);

/* ---------------------------------------------------------------------- */

HAK_EXPORT int hak_sys_send (
	int          sck,
	const void*  data,
	hak_oow_t*   size
);

HAK_EXPORT int hak_sys_send_iov (
	int          sck,
	hak_iovec_t* iov, /* note this is not read-only and can change */
	int          count
);

HAK_EXPORT int hak_sys_open_pipes (
	int          pfd[2],
	int          nonblock
);

HAK_EXPORT void hak_sys_close_pipes (
	int          pfd[2]
);

HAK_EXPORT int hak_sys_set_nonblock (
	int          fd,
	int          v
);

HAK_EXPORT int hak_sys_set_cloexec (
	int          fd,
	int          v
);

HAK_EXPORT int hak_sys_is_errno_wb (
	int          no
);

/* ---------------------------------------------------------------------- */

HAK_EXPORT int hak_ucharstosckaddr (
	hak_t*           hak,
	const hak_uch_t* str,
	hak_oow_t        len,
	hak_sckaddr_t*   sckaddr,
	hak_scklen_t*    scklen
);

HAK_EXPORT int hak_bcharstosckaddr (
	hak_t*           hak,
	const hak_bch_t* str,
	hak_oow_t        len,
	hak_sckaddr_t*   sckaddr,
	hak_scklen_t*    scklen
);

#if defined(HAK_HAVE_INLINE)
static HAK_INLINE int hak_uchars_to_sckaddr (const hak_uch_t* str, hak_oow_t len, hak_sckaddr_t* sckaddr, hak_scklen_t* scklen)
{
	return hak_ucharstosckaddr(HAK_NULL, str, len, sckaddr, scklen);
}
static HAK_INLINE int hak_bchars_to_sckaddr (const hak_bch_t* str, hak_oow_t len, hak_sckaddr_t* sckaddr, hak_scklen_t* scklen)
{
	return hak_bcharstosckaddr(HAK_NULL, str, len, sckaddr, scklen);
}
#else
#define hak_uchars_to_sckaddr(str,len,sckaddr,scklen) hak_ucharstosckaddr(HAK_NULL,str,len,sckaddr,scklen)
#define hak_bchars_to_sckaddr(str,len,sckaddr,scklen) hak_bcharstosckaddr(HAK_NULL,str,len,sckaddr,scklen)
#endif

#if defined(HAK_OOCH_IS_UCH)
#	define hak_oocharstosckaddr hak_ucharstosckaddr
#	define hak_oochars_to_sckaddr hak_uchars_to_sckaddr
#else
#	define hak_oocharstosckaddr hak_bcharstosckaddr
#	define hak_oochars_to_sckaddr hak_bchars_to_sckaddr
#endif

/**
 * The hak_get_sckaddr_info() function returns the socket family.
 * if \a scklen is not #HAK_NULL, it also sets the actual address length
 * in the memory pointed to by it.
 */
HAK_EXPORT int hak_get_sckaddr_info (
	const hak_sckaddr_t* sckaddr,
	hak_scklen_t*        scklen
);

#if defined(__cplusplus)
}
#endif

#endif
