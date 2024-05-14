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

#ifndef _HCL_X_T_
#define _HCL_X_T_

#include <hcl.h>
#include <sys/uio.h>

enum hcl_xpkt_type_t
{
	/* the actual type field is 4 bits long. up to 16 types are possible */

	HCL_XPKT_CODE,      /* [C]->[S] input code */
	HCL_XPKT_EXECUTE,   /* [C]->[S] command to execute the code input */

	HCL_XPKT_ERROR,     /* [S]->[C] error indicator */
	HCL_XPKT_RETVAL,    /* [S]->[C] return value */

	HCL_XPKT_STDIN,     /* [C]->[S] */
	HCL_XPKT_STDOUT,    /* [S]->[C] output to stdout */
	HCL_XPKT_STDERR,    /* [S]->[C] output to stderr or output data related to error */

	/*TODO: define HCL_XPKT_CONTROL and make LIST_WORKS AND KILL_WORKER sub-commands of CONTORL */
	HCL_XPKT_LIST_WORKERS, /* [C]->[S] */
	HCL_XPKT_KILL_WORKER,  /* [C]->[S] */

	HCL_XPKT_DISCONNECT    /* [C]->[S], [S]->[C] */
};
typedef enum hcl_xpkt_type_t hcl_xpkt_type_t;

#include "hcl-pac1.h"
struct hcl_xpkt_hdr_t
{
	hcl_uint8_t id;
	hcl_uint8_t type; /* lower 4 bits represent the actual type.
	                     the upper 4 bits are part of the length extending the length to 12 bits */
	hcl_uint8_t len;
};
typedef struct hcl_xpkt_hdr_t hcl_xpkt_hdr_t;
#include "hcl-upac.h"

#define HCL_XPKT_HDR_LEN (HCL_SIZEOF(hcl_xpkt_hdr_t))

/* the actual length field is 12 bits long. so the maximum payload length allowed per packet is 2^12 - 1 */
#define HCL_XPKT_MAX_PLD_LEN (4095)

/* ---------------------------------------------------------------------- */

typedef struct hcl_xproto_t hcl_xproto_t;

typedef int (*hcl_xproto_cb_on_packet) (
	hcl_xproto_t*   proto,
	hcl_xpkt_type_t type,
	const void*     data,
	hcl_oow_t       len
);

struct hcl_xproto_cb_t
{
	hcl_xproto_cb_on_packet on_packet;
};
typedef struct hcl_xproto_cb_t hcl_xproto_cb_t;

/* ---------------------------------------------------------------------- */

/* forward declaration to skip including <sys/uio.h> just for struct iovec */
typedef struct iovec hcl_iovec_t;

/* ---------------------------------------------------------------------- */

typedef struct hcl_server_proto_t hcl_server_proto_t;
typedef struct hcl_server_worker_t hcl_server_worker_t;
typedef struct hcl_server_t hcl_server_t;

enum hcl_server_option_t
{
	HCL_SERVER_TRAIT,
	HCL_SERVER_LOG_MASK,
	HCL_SERVER_WORKER_MAX_COUNT,
	HCL_SERVER_WORKER_STACK_SIZE,
	HCL_SERVER_WORKER_IDLE_TIMEOUT,
	HCL_SERVER_ACTOR_HEAP_SIZE,
	HCL_SERVER_ACTOR_MAX_RUNTIME,
	HCL_SERVER_SCRIPT_INCLUDE_PATH,
	HCL_SERVER_MODULE_INCTX
};
typedef enum hcl_server_option_t hcl_server_option_t;

enum hcl_server_trait_t
{
#if defined(HCL_BUILD_DEBUG)
	HCL_SERVER_TRAIT_DEBUG_GC         = (1 << 0),
	HCL_SERVER_TRAIT_DEBUG_BIGINT     = (1 << 1)
#endif
};
typedef enum hcl_server_trait_t hcl_server_trait_t;

#define HCL_SERVER_WID_INVALID ((hcl_oow_t)-1)
#define HCL_SERVER_WID_MAX (HCL_SERVER_WID_INVALID - 1)

typedef void (*hcl_server_log_write_t) (
	hcl_server_t*     server,
	hcl_oow_t         wid,
	hcl_bitmask_t     mask,
	const hcl_ooch_t* msg,
	hcl_oow_t         len
);

struct hcl_server_prim_t
{
	hcl_server_log_write_t log_write;
};
typedef struct hcl_server_prim_t hcl_server_prim_t;


/* ---------------------------------------------------------------------- */


typedef struct hcl_client_t hcl_client_t;

enum hcl_client_option_t
{
	HCL_CLIENT_TRAIT,
	HCL_CLIENT_LOG_MASK,
};
typedef enum hcl_client_option_t hcl_client_option_t;

enum hcl_client_trait_t
{
	/* no trait defined at this moment. XXXX is just a placeholder */
	HCL_CLIENT_XXXX  = (1 << 0)
};
typedef enum hcl_client_trait_t hcl_client_trait_t;


typedef void (*hcl_client_log_write_t) (
	hcl_client_t*     client,
	hcl_bitmask_t     mask,
	const hcl_ooch_t* msg,
	hcl_oow_t         len
);

typedef int (*hcl_client_on_packet_t) (
	hcl_client_t*   client,
	hcl_xpkt_type_t type,
	const void*     data,
	hcl_oow_t       len
);

struct hcl_client_prim_t
{
	hcl_client_log_write_t     log_write;
	hcl_client_on_packet_t     on_packet;
};
typedef struct hcl_client_prim_t hcl_client_prim_t;


/* ---------------------------------------------------------------------- */

#if defined(__cplusplus)
extern "C" {
#endif

HCL_EXPORT hcl_server_t* hcl_server_open (
	hcl_mmgr_t*        mmgr,
	hcl_oow_t          xtnsize,
	hcl_server_prim_t* prim,
	hcl_errnum_t*      errnum
);

HCL_EXPORT void hcl_server_close (
	hcl_server_t* server
);

HCL_EXPORT int hcl_server_start (
	hcl_server_t*    server,
	const hcl_bch_t* addrs
);

HCL_EXPORT void hcl_server_stop (
	hcl_server_t* server
);

HCL_EXPORT int hcl_server_setoption (
	hcl_server_t*       server,
	hcl_server_option_t id,
	const void*         value
);

HCL_EXPORT int hcl_server_getoption (
	hcl_server_t*       server,
	hcl_server_option_t id,
	void*               value
);

HCL_EXPORT void* hcl_server_getxtn (
	hcl_server_t* server
);

HCL_EXPORT hcl_mmgr_t* hcl_server_getmmgr (
	hcl_server_t* server
);


HCL_EXPORT hcl_cmgr_t* hcl_server_getcmgr (
	hcl_server_t* server
);

HCL_EXPORT void hcl_server_setcmgr (
	hcl_server_t* server,
	hcl_cmgr_t*   cmgr
);

HCL_EXPORT hcl_errnum_t hcl_server_geterrnum (
	hcl_server_t* server
);

HCL_EXPORT const hcl_ooch_t* hcl_server_geterrstr (
	hcl_server_t* server
);

HCL_EXPORT const hcl_ooch_t* hcl_server_geterrmsg (
	hcl_server_t* server
);

HCL_EXPORT void hcl_server_seterrnum (
	hcl_server_t* server,
	hcl_errnum_t  errnum
);

HCL_EXPORT void hcl_server_seterrbfmt (
	hcl_server_t*    server,
	hcl_errnum_t     errnum,
	const hcl_bch_t* fmt,
	...
);

HCL_EXPORT void hcl_server_seterrufmt (
	hcl_server_t*    server,
	hcl_errnum_t     errnum,
	const hcl_uch_t* fmt,
	...
);

HCL_EXPORT void hcl_server_logbfmt (
	hcl_server_t*    server,
	hcl_bitmask_t  mask,
	const hcl_bch_t* fmt,
	...
);

HCL_EXPORT void hcl_server_logufmt (
	hcl_server_t*    server,
	hcl_bitmask_t  mask,
	const hcl_uch_t* fmt,
	...
);


HCL_EXPORT void* hcl_server_allocmem (
	hcl_server_t* server,
	hcl_oow_t     size
);

HCL_EXPORT void* hcl_server_callocmem (
	hcl_server_t* server,
	hcl_oow_t     size
);

HCL_EXPORT void* hcl_server_reallocmem (
	hcl_server_t* server,
	void*         ptr,
	hcl_oow_t     size
);


HCL_EXPORT void hcl_server_freemem (
	hcl_server_t* server,
	void*         ptr
);


/* ---------------------------------------------------------------------- */

HCL_EXPORT hcl_client_t* hcl_client_open (
	hcl_mmgr_t*        mmgr,
	hcl_oow_t          xtnsize,
	hcl_client_prim_t* prim,
	hcl_errnum_t*      errnum
);

HCL_EXPORT void hcl_client_close (
	hcl_client_t* client
);

HCL_EXPORT int hcl_client_start (
	hcl_client_t* client,
	const char*   ipaddr,
	int           shut_wr_after_req
);

HCL_EXPORT void hcl_client_stop (
	hcl_client_t* client
);

HCL_EXPORT int hcl_client_setoption (
	hcl_client_t*       client,
	hcl_client_option_t id,
	const void*         value
);

HCL_EXPORT int hcl_client_getoption (
	hcl_client_t*       client,
	hcl_client_option_t id,
	void*               value
);


HCL_EXPORT void* hcl_client_getxtn (
	hcl_client_t* client
);

HCL_EXPORT hcl_mmgr_t* hcl_client_getmmgr (
	hcl_client_t* client
);

HCL_EXPORT hcl_cmgr_t* hcl_client_getcmgr (
	hcl_client_t* client
);

HCL_EXPORT void hcl_client_setcmgr (
	hcl_client_t* client,
	hcl_cmgr_t*   cmgr
);


HCL_EXPORT hcl_errnum_t hcl_client_geterrnum (
	hcl_client_t* client
);

HCL_EXPORT const hcl_ooch_t* hcl_client_geterrstr (
	hcl_client_t* client
);

HCL_EXPORT const hcl_ooch_t* hcl_client_geterrmsg (
	hcl_client_t* client
);

HCL_EXPORT const hcl_bch_t* hcl_client_geterrbmsg (
	hcl_client_t* client
);

HCL_EXPORT const hcl_uch_t* hcl_client_geterrumsg (
	hcl_client_t* client
);


HCL_EXPORT void hcl_client_seterrnum (
	hcl_client_t* client,
	hcl_errnum_t  errnum
);

HCL_EXPORT void hcl_client_seterrbfmt (
	hcl_client_t*    client,
	hcl_errnum_t     errnum,
	const hcl_bch_t* fmt,
	...
);

HCL_EXPORT void hcl_client_seterrufmt (
	hcl_client_t*    client,
	hcl_errnum_t     errnum,
	const hcl_uch_t* fmt,
	...
);

HCL_EXPORT void hcl_client_logbfmt (
	hcl_client_t*    client,
	hcl_bitmask_t  mask,
	const hcl_bch_t* fmt,
	...
);

HCL_EXPORT void hcl_client_logufmt (
	hcl_client_t*    client,
	hcl_bitmask_t  mask,
	const hcl_uch_t* fmt,
	...
);

HCL_EXPORT void* hcl_client_allocmem (
	hcl_client_t* client,
	hcl_oow_t     size
);

HCL_EXPORT void* hcl_client_callocmem (
	hcl_client_t* client,
	hcl_oow_t     size
);

HCL_EXPORT void* hcl_client_reallocmem (
	hcl_client_t* client,
	void*         ptr,
	hcl_oow_t     size
);


HCL_EXPORT void hcl_client_freemem (
	hcl_client_t* client,
	void*         ptr
);


/* ---------------------------------------------------------------------- */

HCL_EXPORT hcl_xproto_t* hcl_xproto_open (
	hcl_mmgr_t*      mmgr,
	hcl_xproto_cb_t* cb,
	hcl_oow_t        xtnsize
);

HCL_EXPORT void hcl_xproto_close (
	hcl_xproto_t*  proto
);

HCL_EXPORT void* hcl_xproto_getxtn (
	hcl_xproto_t*  proto
);

HCL_EXPORT hcl_uint8_t* hcl_xproto_getbuf (
	hcl_xproto_t*  proto,
	hcl_oow_t*     capa
);

HCL_EXPORT int hcl_xproto_geteof (
	hcl_xproto_t*  proto
);

HCL_EXPORT void hcl_xproto_seteof (
	hcl_xproto_t*  proto,
	int            v
);

HCL_EXPORT void hcl_xproto_advbuf (
	hcl_xproto_t*  proto,
	hcl_oow_t      inc
);

HCL_EXPORT int hcl_xproto_ready (
	hcl_xproto_t*  proto
);

HCL_EXPORT int hcl_xproto_process (
	hcl_xproto_t*  proto
);

/* ---------------------------------------------------------------------- */

HCL_EXPORT int hcl_sys_send (
	int         sck,
	const void* data,
	hcl_oow_t*  size  /* [IN] number of bytes to write, [OUT] number of bytes written */
);

HCL_EXPORT int hcl_sys_send_iov (
	int          sck,
	hcl_iovec_t* iov, /* note this is not read-only and can change */
	int          count
);

HCL_EXPORT int hcl_sys_open_pipes (
	int          pfd[2],
	int          nonblock
);

HCL_EXPORT void hcl_sys_close_pipes (
	int          pfd[2]
);

HCL_EXPORT int hcl_sys_set_nonblock (
	int          fd,
	int          v
);

HCL_EXPORT int hcl_sys_set_cloexec (
	int          fd,
	int          v
);

HCL_EXPORT int hcl_sys_is_errno_wb (
	int          no
);

#if defined(__cplusplus)
}
#endif

#endif
