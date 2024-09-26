/*
		}
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

#include <hcl-x.h>
#include <hcl-tmr.h>
#include "hcl-prv.h"

#include <stdio.h>
#include <string.h>
#include <errno.h>

#define HCL_SERVER_TOKEN_NAME_ALIGN 64
#define HCL_SERVER_WID_MAP_ALIGN 512
#define HCL_XPROTO_REPLY_BUF_SIZE 1300

#if defined(_WIN32)
#	include <windows.h>
#	include <tchar.h>
#elif defined(__OS2__)
#	define INCL_DOSMODULEMGR
#	define INCL_DOSPROCESS
#	define INCL_DOSERRORS
#	include <os2.h>
#elif defined(__DOS__)
#	include <dos.h>
#	include <time.h>
#elif defined(macintosh)
#	include <Timer.h>
#else

#	if defined(HAVE_TIME_H)
#		include <time.h>
#	endif
#	if defined(HAVE_SYS_TIME_H)
#		include <sys/time.h>
#	endif
#	if defined(HAVE_SYS_UIO_H)
#		include <sys/uio.h>
#	endif

#	include <sys/types.h>
#	include <sys/socket.h>
#	include <pthread.h>
#	include <poll.h>
#	include <unistd.h>
#endif

struct client_hcl_xtn_t
{
	hcl_client_t* client;
};
typedef struct client_hcl_xtn_t client_hcl_xtn_t;

enum state_flag_t
{
	STATE_LOCAL_IN_CLOSED  = (1 << 0),
	STATE_LOCAL_OUT_CLOSED  = (1 << 1),
	STATE_REMOTE_IN_CLOSED = (1 << 2),

	HCL_CLIENT_ALL_CLOSED = (STATE_LOCAL_IN_CLOSED | STATE_LOCAL_OUT_CLOSED | STATE_REMOTE_IN_CLOSED)
};
typedef enum state_flag_t state_flag_t;

struct hcl_client_t
{
	hcl_oow_t   _instsize;
	hcl_mmgr_t* _mmgr;
	hcl_cmgr_t* _cmgr;

	hcl_client_prim_t prim;
	hcl_t* dummy_hcl;

	hcl_errnum_t errnum;
	struct
	{
	#if defined(HCL_OOCH_IS_BCH)
		hcl_uch_t  xerrmsg[HCL_ERRMSG_CAPA];
	#else
		hcl_bch_t  xerrmsg[HCL_ERRMSG_CAPA * 2];
	#endif
		hcl_ooch_t buf[HCL_ERRMSG_CAPA];
		hcl_oow_t len;
	} errmsg;
	int stopreq;

	struct
	{
		hcl_bitmask_t trait;
		hcl_bitmask_t logmask;
	} cfg;

	int mux_pipe[2]; /* pipe to break the blocking multiplexer in the main server loop */
	int state;

	struct
	{
		int sck;
		hcl_xproto_t* proto;
	} remote;

	struct
	{
		int in;
		int out;
		int err;

		struct
		{
			hcl_uint8_t* ptr;
			hcl_oow_t capa;
			hcl_oow_t pos;
			hcl_oow_t len;
		} pw2r; /* pending write to the remote side */
	} local;


	struct
	{
		hcl_bch_t buf[4096];
		hcl_oow_t pos;
		hcl_oow_t len;
	} script;
};


/* ========================================================================= */

static void client_log_write_for_dummy (hcl_t* hcl, hcl_bitmask_t mask, const hcl_ooch_t* msg, hcl_oow_t len)
{
	client_hcl_xtn_t* xtn = (client_hcl_xtn_t*)hcl_getxtn(hcl);
	hcl_client_t* client;

	client = xtn->client;
	client->prim.log_write (client, mask, msg, len);
}

hcl_client_t* hcl_client_open (hcl_mmgr_t* mmgr, hcl_oow_t xtnsize, hcl_client_prim_t* prim, hcl_errnum_t* errnum)
{
	hcl_client_t* client = HCL_NULL;
	hcl_t* hcl = HCL_NULL;
	client_hcl_xtn_t* xtn;
	int pfd[2];

	client = (hcl_client_t*)HCL_MMGR_ALLOC(mmgr, HCL_SIZEOF(*client) + xtnsize);
	if (HCL_UNLIKELY(!client))
	{
		if (errnum) *errnum = HCL_ESYSMEM;
		return HCL_NULL;
	}

	hcl = hcl_openstdwithmmgr(mmgr, HCL_SIZEOF(*xtn), errnum);
	if (HCL_UNLIKELY(!hcl))
	{
		HCL_MMGR_FREE (mmgr, client);
		return HCL_NULL;
	}

	if (hcl_sys_open_pipes(pfd, 1) <= -1)
	{
		if (errnum) *errnum = hcl->vmprim.syserrstrb(hcl, 0, errno, HCL_NULL, 0);
		goto oops;
	}

	/* replace the vmprim.log_write function */
	hcl->vmprim.log_write = client_log_write_for_dummy;

	xtn = (client_hcl_xtn_t*)hcl_getxtn(hcl);
	xtn->client = client;

	HCL_MEMSET (client, 0, HCL_SIZEOF(*client) + xtnsize);
	client->_instsize = HCL_SIZEOF(*client);
	client->_mmgr = mmgr;
	client->_cmgr = hcl_get_utf8_cmgr();
	client->prim = *prim;
	client->dummy_hcl = hcl;
	client->mux_pipe[0] = pfd[0];
	client->mux_pipe[1] = pfd[1];
	client->remote.sck = -1;
	client->local.in = -1;
	client->local.out = -1;
	client->local.err = -1;

	client->cfg.logmask = ~(hcl_bitmask_t)0;

	/* the dummy hcl is used for this client to perform primitive operations
	 * such as getting system time or logging. so the heap size doesn't
	 * need to be changed from the tiny value set above. */
	hcl_setoption (client->dummy_hcl, HCL_LOG_MASK, &client->cfg.logmask);
	hcl_setcmgr (client->dummy_hcl, client->_cmgr);

	return client;

oops:
	/* NOTE: pipe should be closed if jump to here is made after pipe() above */
	if (hcl) hcl_close (hcl);
	if (client) HCL_MMGR_FREE (mmgr, client);
	return HCL_NULL;
}

static int is_stdio_fd (int fd)
{
	return fd == STDIN_FILENO || fd == STDOUT_FILENO || fd == STDERR_FILENO;
}

void hcl_client_close (hcl_client_t* client)
{
	if (client->remote.proto) hcl_xproto_close (client->remote.proto);
	if (client->remote.sck >= 0) close (client->remote.sck);
	if (client->local.in >= 0 && is_stdio_fd(client->local.in)) close (client->local.in);
	if (client->local.out >= 0 && is_stdio_fd(client->local.out)) close (client->local.out);
	if (client->local.err >= 0 && is_stdio_fd(client->local.err)) close (client->local.err);

	hcl_sys_close_pipes(client->mux_pipe);
	hcl_close (client->dummy_hcl);
	HCL_MMGR_FREE (client->_mmgr, client);
}

int hcl_client_setoption (hcl_client_t* client, hcl_client_option_t id, const void* value)
{
	switch (id)
	{
		case HCL_CLIENT_TRAIT:
			client->cfg.trait = *(const hcl_bitmask_t*)value;
			return 0;

		case HCL_CLIENT_LOG_MASK:
			client->cfg.logmask = *(const hcl_bitmask_t*)value;
			if (client->dummy_hcl)
			{
				/* setting this affects the dummy hcl immediately.
				 * existing hcl instances inside worker threads won't get
				 * affected. new hcl instances to be created later
				 * is supposed to use the new value */
				hcl_setoption (client->dummy_hcl, HCL_LOG_MASK, value);
			}
			return 0;
	}

	hcl_client_seterrnum (client, HCL_EINVAL);
	return -1;
}

int hcl_client_getoption (hcl_client_t* client, hcl_client_option_t id, void* value)
{
	switch (id)
	{
		case HCL_CLIENT_TRAIT:
			*(hcl_bitmask_t*)value = client->cfg.trait;
			return 0;

		case HCL_CLIENT_LOG_MASK:
			*(hcl_bitmask_t*)value = client->cfg.logmask;
			return 0;
	};

	hcl_client_seterrnum (client, HCL_EINVAL);
	return -1;
}


void* hcl_client_getxtn (hcl_client_t* client)
{
	return (void*)((hcl_uint8_t*)client + client->_instsize);
}

hcl_mmgr_t* hcl_client_getmmgr (hcl_client_t* client)
{
	return client->_mmgr;
}

hcl_cmgr_t* hcl_client_getcmgr (hcl_client_t* client)
{
	return client->_cmgr;
}

void hcl_client_setcmgr (hcl_client_t* client, hcl_cmgr_t* cmgr)
{
	client->_cmgr = cmgr;
}

hcl_errnum_t hcl_client_geterrnum (hcl_client_t* client)
{
	return client->errnum;
}

const hcl_ooch_t* hcl_client_geterrstr (hcl_client_t* client)
{
	return hcl_errnum_to_errstr(client->errnum);
}

const hcl_ooch_t* hcl_client_geterrmsg (hcl_client_t* client)
{
	if (client->errmsg.len <= 0) return hcl_errnum_to_errstr(client->errnum);
	return client->errmsg.buf;
}

const hcl_bch_t* hcl_client_geterrbmsg (hcl_client_t* client)
{
#if defined(HCL_OOCH_IS_BCH)
	return (client->errmsg.len <= 0)? hcl_errnum_to_errstr(client->errnum): client->errmsg.buf;
#else
	const hcl_ooch_t* msg;
	hcl_oow_t wcslen, mbslen;

	msg = (client->errmsg.len <= 0)? hcl_errnum_to_errstr(client->errnum): client->errmsg.buf;

	mbslen = HCL_COUNTOF(client->errmsg.xerrmsg);
	hcl_conv_ucstr_to_bcstr_with_cmgr (msg, &wcslen, client->errmsg.xerrmsg, &mbslen, client->_cmgr);

	return client->errmsg.xerrmsg;
#endif
}

const hcl_uch_t* hcl_client_geterrumsg (hcl_client_t* client)
{
#if defined(HCL_OOCH_IS_BCH)
	const hcl_ooch_t* msg;
	hcl_oow_t wcslen, mbslen;

	msg = (client->errmsg.len <= 0)? hcl_errnum_to_errstr(client->errnum): client->errmsg.buf;

	wcslen = HCL_COUNTOF(client->errmsg.xerrmsg);
	hcl_conv_bcstr_to_ucstr_with_cmgr (msg, &mbslen, client->errmsg.xerrmsg, &wcslen, client->_cmgr, 1);

	return client->errmsg.xerrmsg;
#else
	return (client->errmsg.len == '\0')? hcl_errnum_to_errstr(client->errnum): client->errmsg.buf;
#endif
}

void hcl_client_seterrnum (hcl_client_t* client, hcl_errnum_t errnum)
{
	/*if (client->shuterr) return; */
	client->errnum = errnum;
	client->errmsg.len = 0;
}

void hcl_client_seterrbfmt (hcl_client_t* client, hcl_errnum_t errnum, const hcl_bch_t* fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	hcl_seterrbfmtv (client->dummy_hcl, errnum, fmt, ap);
	va_end (ap);

	HCL_ASSERT (client->dummy_hcl, HCL_COUNTOF(client->errmsg.buf) == HCL_COUNTOF(client->dummy_hcl->errmsg.buf));
	client->errnum = errnum;
	hcl_copy_oochars (client->errmsg.buf, client->dummy_hcl->errmsg.buf, HCL_COUNTOF(client->errmsg.buf));
	client->errmsg.len = client->dummy_hcl->errmsg.len;
}

void hcl_client_seterrufmt (hcl_client_t* client, hcl_errnum_t errnum, const hcl_uch_t* fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	hcl_seterrufmtv (client->dummy_hcl, errnum, fmt, ap);
	va_end (ap);

	HCL_ASSERT (client->dummy_hcl, HCL_COUNTOF(client->errmsg.buf) == HCL_COUNTOF(client->dummy_hcl->errmsg.buf));
	client->errnum = errnum;
	hcl_copy_oochars (client->errmsg.buf, client->dummy_hcl->errmsg.buf, HCL_COUNTOF(client->errmsg.buf));
	client->errmsg.len = client->dummy_hcl->errmsg.len;
}

/* ========================================================================= */

void hcl_client_logbfmt (hcl_client_t* client, hcl_bitmask_t mask, const hcl_bch_t* fmt, ...)
{
	va_list ap;
	va_start (ap, fmt);
	hcl_logbfmtv (client->dummy_hcl, mask, fmt, ap);
	va_end (ap);
}

void hcl_client_logufmt (hcl_client_t* client, hcl_bitmask_t mask, const hcl_uch_t* fmt, ...)
{
	va_list ap;
	va_start (ap, fmt);
	hcl_logufmtv (client->dummy_hcl, mask, fmt, ap);
	va_end (ap);
}

/* ========================================================================= */

void* hcl_client_allocmem (hcl_client_t* client, hcl_oow_t size)
{
	void* ptr;

	ptr = HCL_MMGR_ALLOC(client->_mmgr, size);
	if (!ptr) hcl_client_seterrnum (client, HCL_ESYSMEM);
	return ptr;
}

void* hcl_client_callocmem (hcl_client_t* client, hcl_oow_t size)
{
	void* ptr;

	ptr = HCL_MMGR_ALLOC(client->_mmgr, size);
	if (!ptr) hcl_client_seterrnum (client, HCL_ESYSMEM);
	else HCL_MEMSET (ptr, 0, size);
	return ptr;
}

void* hcl_client_reallocmem (hcl_client_t* client, void* ptr, hcl_oow_t size)
{
	ptr = HCL_MMGR_REALLOC(client->_mmgr, ptr, size);
	if (!ptr) hcl_client_seterrnum (client, HCL_ESYSMEM);
	return ptr;
}

void hcl_client_freemem (hcl_client_t* client, void* ptr)
{
	HCL_MMGR_FREE (client->_mmgr, ptr);
}

/* ========================================================================= */

struct proto_xtn_t
{
	hcl_client_t* client;
};
typedef struct proto_xtn_t proto_xtn_t;

static int proto_on_packet (hcl_xproto_t* proto, hcl_xpkt_type_t type, const void* data, hcl_oow_t len)
{
	proto_xtn_t* proto_xtn;
	hcl_client_t* client;
	proto_xtn = hcl_xproto_getxtn(proto);
	client = proto_xtn->client;
	return client->prim.on_packet(client, type, data, len);
}

static int client_connect_to_server (hcl_client_t* client, const char* ipaddr)
{
	hcl_sckaddr_t sckaddr;
	hcl_scklen_t scklen;
	int sckfam;
	int sck = -1;
	hcl_xproto_t* proto = HCL_NULL;

	proto_xtn_t* proto_xtn;
	hcl_xproto_cb_t proto_cb;

	sckfam = hcl_bchars_to_sckaddr(ipaddr, strlen(ipaddr), &sckaddr, &scklen);
	if (sckfam <= -1)
	{
		hcl_client_seterrbfmt (client, HCL_EINVAL, "cannot convert ip address - %hs", ipaddr);
		goto oops;
	}

	sck = socket(sckfam, SOCK_STREAM, 0);
	if (sck <= -1)
	{
		hcl_client_seterrbfmt (client, HCL_ESYSERR, "cannot create socket - %hs", strerror(errno));
		goto oops;
	}

	hcl_sys_set_cloexec(sck, 1);

#if 0
	if (sckfam == AF_INET)
	{
		struct sockaddr_in anyaddr;
		int opt = 1;
		setsockopt(sck, SOL_SOCKET, SO_REUSEADDR, (char *)&opt, sizeof(opt));
		HCL_MEMSET (&anyaddr, 0, HCL_SIZEOF(anyaddr));
		anyaddr.sin_family = sckfam;
		if (bind(sck, (struct sockaddr *)&anyaddr, scklen) <= -1)
		{
			hcl_client_seterrbfmt (client, HCL_ESYSERR,
				"cannot bind socket %d - %hs", sck, strerror(errno));
			goto oops;
		}
	}
	else if (sckfam == AF_INET6)
	{
		struct sockaddr_in6 anyaddr;
		int opt = 1;
		setsockopt(sck, SOL_SOCKET, SO_REUSEADDR, (char *)&opt, sizeof(opt));
		HCL_MEMSET (&anyaddr, 0, HCL_SIZEOF(anyaddr));
		anyaddr.sin6_family = sckfam;
		if (bind(sck, (struct sockaddr *)&anyaddr, scklen) <= -1)
		{
			hcl_client_seterrbfmt (client, HCL_ESYSERR,
				"cannot bind socket %d - %hs", sck, strerror(errno));
			goto oops;
		}
	}
#endif

/* TODO: async connect? */
/* TODO: connect timeout */
	if (connect(sck, (struct sockaddr*)&sckaddr, scklen) <= -1)
	{
		hcl_client_seterrbfmt (client, HCL_ESYSERR,
			"cannot connect socket %d to %hs - %hs", sck, ipaddr, strerror(errno));
		goto oops;
	}

	hcl_sys_set_nonblock(sck, 1); /* make it nonblocking after connection has been established */

	HCL_MEMSET (&proto, 0, HCL_SIZEOF(proto_cb));
	proto_cb.on_packet = proto_on_packet;

	proto = hcl_xproto_open(hcl_client_getmmgr(client), &proto_cb, HCL_SIZEOF(*proto_xtn));
	if (HCL_UNLIKELY(!proto))
	{
		hcl_client_seterrbfmt (client, HCL_ESYSERR, "cannot open protocol to %s", ipaddr);
		goto oops;
	}
	proto_xtn = hcl_xproto_getxtn(proto);
	proto_xtn->client = client;

	client->remote.sck = sck;
	client->remote.proto = proto;
	return 0;

oops:
	if (proto) hcl_xproto_close (proto);
	if (sck >= 0) close (sck);
	return -1;
}


static void client_close (hcl_client_t* client)
{
	if (client->remote.proto)
	{
		hcl_xproto_close (client->remote.proto);
		client->remote.proto = HCL_NULL;
	}

	if (client->remote.sck >= 0)
	{
		close (client->remote.sck);
		client->remote.sck = -1;
	}
}

static int client_add_to_local_pw2r (hcl_client_t* client, const hcl_uint8_t* ptr, hcl_oow_t len)
{
	if (client->local.pw2r.len >= client->local.pw2r.capa)
	{
		hcl_uint8_t* tmp;
		hcl_oow_t newcapa;

		newcapa = HCL_ALIGN_POW2(client->local.pw2r.capa + len, 128);
		tmp = hcl_client_reallocmem(client, client->local.pw2r.ptr, newcapa * HCL_SIZEOF(*client->local.pw2r.ptr));
		if (HCL_UNLIKELY(!tmp)) return -1;

		client->local.pw2r.capa = newcapa;
		client->local.pw2r.ptr = tmp;
	}

	HCL_MEMCPY (&client->local.pw2r.ptr[client->local.pw2r.len], ptr, len);
	client->local.pw2r.len += len;
	return 0;
}

static int client_send_to_remote (hcl_client_t* client, hcl_xpkt_type_t pktype, const hcl_uint8_t* ptr, hcl_oow_t len)
{
	hcl_xpkt_hdr_t hdr;
	struct iovec iov[2];
	hcl_uint16_t seglen;
	int n, i;

	do
	{
		seglen = (len > HCL_XPKT_MAX_PLD_LEN)? HCL_XPKT_MAX_PLD_LEN: len;

		hdr.id = 1; /* TODO: */
		hdr.type = pktype | (((seglen >> 8) & 0x0F) << 4);
		hdr.len = seglen & 0xFF;

		i = 0;
		iov[i].iov_base = &hdr;
		iov[i++].iov_len = HCL_SIZEOF(hdr);
		if (seglen > 0)
		{
			iov[i].iov_base = ptr;
			iov[i++].iov_len = seglen;
		}

		n = hcl_sys_send_iov(client->remote.sck, iov, i);
		if (n <= -1) return -1;

		if (n < i || iov[n - 1].iov_len > 0)
		{
			/* the write isn't completed. */
			for (i = n; i < 2 ; i++)
			{
				if (iov[i].iov_len > 0)
				{
					/* pending write... */
					if (client_add_to_local_pw2r(client, iov[i].iov_base, iov[i].iov_len) <= -1) return -1;
				}
			}

			break;
		}

		ptr += seglen;
		len -= seglen;
	}
	while (len > 0);

	return 0;
}

/* ========================================================================= */

static void on_control_event (hcl_client_t* client, struct pollfd* pfd)
{
	char tmp[128];
hcl_client_logbfmt(client, HCL_LOG_STDERR, "ON CONTROL EVENT \n");
	while (read(client->mux_pipe[0], tmp, HCL_SIZEOF(tmp)) > 0) /* nothing */;
/* TODO: handle different command? */
}

static void on_remote_event (hcl_client_t* client, struct pollfd* pfd, int shut_wr_after_req)
{
//hcl_client_logbfmt(client, HCL_LOG_STDERR, "ON REMOTE EVENT \n");

	if (pfd->revents & POLLOUT)
	{
		ssize_t n;
		hcl_oow_t len;

		len = client->local.pw2r.len - client->local.pw2r.pos;
		n = hcl_sys_send(client->remote.sck, &client->local.pw2r.ptr[client->local.pw2r.pos], &len);
		client->local.pw2r.pos += len;
		if (client->local.pw2r.pos >= client->local.pw2r.len)
		{
			/* empty the buffer */
			client->local.pw2r.pos = 0;
			client->local.pw2r.len = 0;
		}

		if (n <= -1)
		{
			/* TODO: logging */
hcl_client_logbfmt(client, HCL_LOG_STDERR, "send error - %hs\n", strerror(errno));
			goto reqstop;
		}
	}

	if (pfd->revents & POLLIN)
	{
		hcl_oow_t bcap;
		hcl_uint8_t* bptr;
		ssize_t x;

		bptr = hcl_xproto_getbuf(client->remote.proto, &bcap);;
		x = recv(client->remote.sck, bptr, bcap, 0);
		if (x <= -1)
		{
			if (errno == EINTR) goto carry_on; /* didn't read read */
hcl_client_logbfmt(client, HCL_LOG_STDERR, "recv error from remote - %hs", strerror(errno));
			/*hcl_seterrwithsyserr (hcl, 0, errno); */
			/* TODO: error info set... */
			goto reqstop;
		}
		if (x == 0) hcl_xproto_seteof(client->remote.proto, 1);
		hcl_xproto_advbuf (client->remote.proto, x);
	}


carry_on:
	/* handle the data received from the remote side */
	while (hcl_xproto_ready(client->remote.proto))
	{
		int n;

		if ((n = hcl_xproto_process(client->remote.proto)) <= -1)
		{
			/* TODO: proper error message */
			printf ("PROTOCOL PROCESSING ERROR...\n");
			goto reqstop;
		}
		if (n == 0)
		{
			/* TODO: chceck if there is remaining data in the buffer...?? */
			printf ("CALLBACK REQUESTED TO EXIT...\n");
			goto reqstop;
		}
	}

	if (hcl_xproto_geteof(client->remote.proto))
	{
		client->state |= STATE_REMOTE_IN_CLOSED;
		goto reqstop;
	}
	return;

reqstop:
	client->stopreq = 1;
	return;
}

static void on_local_in_event (hcl_client_t* client, struct pollfd* pfd)
{
	ssize_t n;
	hcl_uint8_t buf[128];

//hcl_client_logbfmt(client, HCL_LOG_STDERR, "local in on %d\n", pfd->fd);
	n = read(pfd->fd, buf, HCL_SIZEOF(buf));
	if (n <= -1)
	{
		//if (hcl_sys_is_errno_wb(errno)) ...
hcl_client_logbfmt(client, HCL_LOG_STDERR, "local in read error - %hs\n", strerror(errno));
		client->stopreq = 1;
	}
	else if (n == 0)
	{
/*hcl_client_logbfmt(client, HCL_LOG_STDERR, "local in eof\n");*/
/* TODO ARRANGE TO FINISH.. AFTER EXUCTION OF REMAINING STUFF... */
		//client->stopreq = 1;
		client->state |= STATE_LOCAL_IN_CLOSED;
		n = client_send_to_remote(client, HCL_XPKT_EXECUTE, HCL_NULL, 0);
		if (n <= -1)
		{
hcl_client_logbfmt(client, HCL_LOG_STDERR, "local to remote  (execute)- %hs\n", strerror(errno));
		}
	}
	else
	{
/*hcl_client_logbfmt(client, HCL_LOG_STDERR, "local read - %ld\n", (long)n);*/
		n = client_send_to_remote(client, HCL_XPKT_CODE, buf, n);
		if (n <= -1)
		{
hcl_client_logbfmt(client, HCL_LOG_STDERR, "local to remote (code)- %hs\n", strerror(errno));
		}
	}
}

static int client_setup_local(hcl_client_t* client)
{
	client->local.in = STDIN_FILENO;
	client->local.out = STDOUT_FILENO;
	client->local.err = STDERR_FILENO;
	return 0;
}

int hcl_client_start (hcl_client_t* client, const char* ipaddr, int shut_wr_after_req)
{
	/*  TODO: cin, cout, cerr could be actual files or something other than the console.
	          the actual loop won't begin until all these file descriptors are ready */

	client->stopreq = 0;
	if (client_setup_local(client) <= -1) return -1;
hcl_client_logbfmt(client, HCL_LOG_STDERR, "staritg XXXXXXXXXXX loop... ...\n");
	if (client_connect_to_server(client, ipaddr) <= -1) return -1; /* TODO: support time out or abort while connecting... */

hcl_client_logbfmt(client, HCL_LOG_STDERR, "staritg client loop... ...\n");
	while (!client->stopreq)
	{
		int nfds, i;
		struct pollfd pfd[10];

		if ((client->state & HCL_CLIENT_ALL_CLOSED) == HCL_CLIENT_ALL_CLOSED)
		{
			/* no explicit stop request. but all file descriptors reached EOF */
			break;
		}

		HCL_MEMSET (pfd, 0, HCL_SIZEOF(pfd));
		nfds = 0;

		/* always monitor the control channel */
		pfd[nfds].fd = client->mux_pipe[0];
		pfd[nfds].events = POLLIN;
		pfd[nfds++].revents = 0;

		pfd[nfds].fd = client->remote.sck;
		/* TODO: if there is data received from the server, not flushed to the client side
		 *       don't monitor input */
		pfd[nfds].events = POLLIN;
		if (client->local.pw2r.len > client->local.pw2r.pos) pfd[nfds].events |= POLLOUT;
		pfd[nfds++].revents = 0;

		/* TODO: client->local.in and client->local.out can be equal.
		 *       handle this? */
		if (client->local.in >= 0)
		{
			if (client->local.pw2r.pos >= client->local.pw2r.len)
			{
//hcl_client_logbfmt(client, HCL_LOG_STDERR, "ADDING LOCAL IN TO MULTIPLEX...\n");
				pfd[nfds].fd = client->local.in;
				pfd[nfds].events = POLLIN;
				pfd[nfds++].revents = 0;
			}
		}

		i = poll(pfd, nfds, 1000);
//hcl_client_logbfmt(client, HCL_LOG_STDERR, "poll returned %d\n", i);
		if (i <= -1)
		{
			hcl_client_seterrbfmt (client, HCL_ESYSERR, "poll error - %hs", strerror(errno));
			goto oops;
		}

		if (i == 0)
		{
			/* TODO: proper timeout handling */
			continue;
		}

		for (i = 0; i < nfds; i++)
		{
			if (!pfd[i].revents) continue;

//hcl_client_logbfmt(client, HCL_LOG_STDERR, "EVENT ON %d mux[%d], remote[%d], local[%d]\n", pfd[i].fd, client->mux_pipe[0], client->remote.sck, client->local.in);
			if (pfd[i].fd == client->mux_pipe[0])
			{
				on_control_event (client, &pfd[i]);
			}
			else if (pfd[i].fd == client->remote.sck)
			{
				/* event from the server */
				on_remote_event (client, &pfd[i], shut_wr_after_req);
			}
			else if (pfd[i].fd == client->local.in)
			{
				/*if (pfd[i].revents & POLLIN)*/
					on_local_in_event (client, &pfd[i]);
			}
		}
	}

done:
/* TODO: we can check if the buffer has all been consumed. if not, there is trailing garbage.. */
	/*{
		struct linger linger;
		linger.l_onoff = 1;
		linger.l_linger = 0;
		setsockopt (client->remote.sck, SOL_SOCKET, SO_LINGER, (char *) &linger, sizeof(linger));
	}*/

	client_close (client);
	return 0;

oops:
	client_close (client);
	return -1;
}

void hcl_client_stop (hcl_client_t* client)
{
	client->stopreq = 1;
	write (client->mux_pipe[1], "Q", 1); /* don't care about failure */
}
