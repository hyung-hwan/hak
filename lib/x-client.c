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

#include <hak-x.h>
#include <hak-tmr.h>
#include "hak-prv.h"

#include <stdio.h>
#include <string.h>
#include <errno.h>

#define HAK_SERVER_TOKEN_NAME_ALIGN 64
#define HAK_SERVER_WID_MAP_ALIGN 512
#define HAK_XPROTO_REPLY_BUF_SIZE 1300

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

struct client_hak_xtn_t
{
	hak_client_t* client;
};
typedef struct client_hak_xtn_t client_hak_xtn_t;

enum state_flag_t
{
	STATE_LOCAL_IN_CLOSED  = (1 << 0),
	STATE_LOCAL_OUT_CLOSED  = (1 << 1),
	STATE_REMOTE_IN_CLOSED = (1 << 2),

	HAK_CLIENT_ALL_CLOSED = (STATE_LOCAL_IN_CLOSED | STATE_LOCAL_OUT_CLOSED | STATE_REMOTE_IN_CLOSED)
};
typedef enum state_flag_t state_flag_t;

struct hak_client_t
{
	hak_oow_t   _instsize;
	hak_mmgr_t* _mmgr;
	hak_cmgr_t* _cmgr;

	hak_client_prim_t prim;
	hak_t* dummy_hak;

	hak_errnum_t errnum;
	struct
	{
	#if defined(HAK_OOCH_IS_BCH)
		hak_uch_t  xerrmsg[HAK_ERRMSG_CAPA];
	#else
		hak_bch_t  xerrmsg[HAK_ERRMSG_CAPA * 2];
	#endif
		hak_ooch_t buf[HAK_ERRMSG_CAPA];
		hak_oow_t len;
	} errmsg;
	int stopreq;

	struct
	{
		hak_bitmask_t trait;
		hak_bitmask_t logmask;
	} cfg;

	int mux_pipe[2]; /* pipe to break the blocking multiplexer in the main server loop */
	int state;

	struct
	{
		int sck;
		hak_xproto_t* proto;
	} remote;

	struct
	{
		int in;
		int out;
		int err;

		struct
		{
			hak_uint8_t* ptr;
			hak_oow_t capa;
			hak_oow_t pos;
			hak_oow_t len;
		} pw2r; /* pending write to the remote side */
	} local;


	struct
	{
		hak_bch_t buf[4096];
		hak_oow_t pos;
		hak_oow_t len;
	} script;
};


/* ========================================================================= */

static void client_log_write_for_dummy (hak_t* hak, hak_bitmask_t mask, const hak_ooch_t* msg, hak_oow_t len)
{
	client_hak_xtn_t* xtn = (client_hak_xtn_t*)hak_getxtn(hak);
	hak_client_t* client;

	client = xtn->client;
	client->prim.log_write (client, mask, msg, len);
}

hak_client_t* hak_client_open (hak_mmgr_t* mmgr, hak_oow_t xtnsize, hak_client_prim_t* prim, hak_errnum_t* errnum)
{
	hak_client_t* client = HAK_NULL;
	hak_t* hak = HAK_NULL;
	client_hak_xtn_t* xtn;
	int pfd[2];

	client = (hak_client_t*)HAK_MMGR_ALLOC(mmgr, HAK_SIZEOF(*client) + xtnsize);
	if (HAK_UNLIKELY(!client))
	{
		if (errnum) *errnum = HAK_ESYSMEM;
		return HAK_NULL;
	}

	hak = hak_openstdwithmmgr(mmgr, HAK_SIZEOF(*xtn), errnum);
	if (HAK_UNLIKELY(!hak))
	{
		HAK_MMGR_FREE (mmgr, client);
		return HAK_NULL;
	}

	if (hak_sys_open_pipes(pfd, 1) <= -1)
	{
		if (errnum) *errnum = hak->vmprim.syserrstrb(hak, 0, errno, HAK_NULL, 0);
		goto oops;
	}

	/* replace the vmprim.log_write function */
	hak->vmprim.log_write = client_log_write_for_dummy;

	xtn = (client_hak_xtn_t*)hak_getxtn(hak);
	xtn->client = client;

	HAK_MEMSET(client, 0, HAK_SIZEOF(*client) + xtnsize);
	client->_instsize = HAK_SIZEOF(*client);
	client->_mmgr = mmgr;
	client->_cmgr = hak_get_utf8_cmgr();
	client->prim = *prim;
	client->dummy_hak = hak;
	client->mux_pipe[0] = pfd[0];
	client->mux_pipe[1] = pfd[1];
	client->remote.sck = -1;
	client->local.in = -1;
	client->local.out = -1;
	client->local.err = -1;

	client->cfg.logmask = ~(hak_bitmask_t)0;

	/* the dummy hak is used for this client to perform primitive operations
	 * such as getting system time or logging. so the heap size doesn't
	 * need to be changed from the tiny value set above. */
	hak_setoption (client->dummy_hak, HAK_LOG_MASK, &client->cfg.logmask);
	hak_setcmgr (client->dummy_hak, client->_cmgr);

	return client;

oops:
	/* NOTE: pipe should be closed if jump to here is made after pipe() above */
	if (hak) hak_close (hak);
	if (client) HAK_MMGR_FREE (mmgr, client);
	return HAK_NULL;
}

static int is_stdio_fd (int fd)
{
	return fd == STDIN_FILENO || fd == STDOUT_FILENO || fd == STDERR_FILENO;
}

void hak_client_close (hak_client_t* client)
{
	if (client->remote.proto) hak_xproto_close (client->remote.proto);
	if (client->remote.sck >= 0) close (client->remote.sck);
	if (client->local.in >= 0 && is_stdio_fd(client->local.in)) close (client->local.in);
	if (client->local.out >= 0 && is_stdio_fd(client->local.out)) close (client->local.out);
	if (client->local.err >= 0 && is_stdio_fd(client->local.err)) close (client->local.err);

	hak_sys_close_pipes(client->mux_pipe);
	hak_close (client->dummy_hak);
	HAK_MMGR_FREE (client->_mmgr, client);
}

int hak_client_setoption (hak_client_t* client, hak_client_option_t id, const void* value)
{
	switch (id)
	{
		case HAK_CLIENT_TRAIT:
			client->cfg.trait = *(const hak_bitmask_t*)value;
			return 0;

		case HAK_CLIENT_LOG_MASK:
			client->cfg.logmask = *(const hak_bitmask_t*)value;
			if (client->dummy_hak)
			{
				/* setting this affects the dummy hak immediately.
				 * existing hak instances inside worker threads won't get
				 * affected. new hak instances to be created later
				 * is supposed to use the new value */
				hak_setoption (client->dummy_hak, HAK_LOG_MASK, value);
			}
			return 0;
	}

	hak_client_seterrnum (client, HAK_EINVAL);
	return -1;
}

int hak_client_getoption (hak_client_t* client, hak_client_option_t id, void* value)
{
	switch (id)
	{
		case HAK_CLIENT_TRAIT:
			*(hak_bitmask_t*)value = client->cfg.trait;
			return 0;

		case HAK_CLIENT_LOG_MASK:
			*(hak_bitmask_t*)value = client->cfg.logmask;
			return 0;
	};

	hak_client_seterrnum (client, HAK_EINVAL);
	return -1;
}


void* hak_client_getxtn (hak_client_t* client)
{
	return (void*)((hak_uint8_t*)client + client->_instsize);
}

hak_mmgr_t* hak_client_getmmgr (hak_client_t* client)
{
	return client->_mmgr;
}

hak_cmgr_t* hak_client_getcmgr (hak_client_t* client)
{
	return client->_cmgr;
}

void hak_client_setcmgr (hak_client_t* client, hak_cmgr_t* cmgr)
{
	client->_cmgr = cmgr;
}

hak_errnum_t hak_client_geterrnum (hak_client_t* client)
{
	return client->errnum;
}

const hak_ooch_t* hak_client_geterrstr (hak_client_t* client)
{
	return hak_errnum_to_errstr(client->errnum);
}

const hak_ooch_t* hak_client_geterrmsg (hak_client_t* client)
{
	if (client->errmsg.len <= 0) return hak_errnum_to_errstr(client->errnum);
	return client->errmsg.buf;
}

const hak_bch_t* hak_client_geterrbmsg (hak_client_t* client)
{
#if defined(HAK_OOCH_IS_BCH)
	return (client->errmsg.len <= 0)? hak_errnum_to_errstr(client->errnum): client->errmsg.buf;
#else
	const hak_ooch_t* msg;
	hak_oow_t wcslen, mbslen;

	msg = (client->errmsg.len <= 0)? hak_errnum_to_errstr(client->errnum): client->errmsg.buf;

	mbslen = HAK_COUNTOF(client->errmsg.xerrmsg);
	hak_conv_ucstr_to_bcstr_with_cmgr (msg, &wcslen, client->errmsg.xerrmsg, &mbslen, client->_cmgr);

	return client->errmsg.xerrmsg;
#endif
}

const hak_uch_t* hak_client_geterrumsg (hak_client_t* client)
{
#if defined(HAK_OOCH_IS_BCH)
	const hak_ooch_t* msg;
	hak_oow_t wcslen, mbslen;

	msg = (client->errmsg.len <= 0)? hak_errnum_to_errstr(client->errnum): client->errmsg.buf;

	wcslen = HAK_COUNTOF(client->errmsg.xerrmsg);
	hak_conv_bcstr_to_ucstr_with_cmgr (msg, &mbslen, client->errmsg.xerrmsg, &wcslen, client->_cmgr, 1);

	return client->errmsg.xerrmsg;
#else
	return (client->errmsg.len == '\0')? hak_errnum_to_errstr(client->errnum): client->errmsg.buf;
#endif
}

void hak_client_seterrnum (hak_client_t* client, hak_errnum_t errnum)
{
	/*if (client->shuterr) return; */
	client->errnum = errnum;
	client->errmsg.len = 0;
}

void hak_client_seterrbfmt (hak_client_t* client, hak_errnum_t errnum, const hak_bch_t* fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	hak_seterrbfmtv (client->dummy_hak, errnum, fmt, ap);
	va_end (ap);

	HAK_ASSERT(client->dummy_hak, HAK_COUNTOF(client->errmsg.buf) == HAK_COUNTOF(client->dummy_hak->errmsg.buf));
	client->errnum = errnum;
	hak_copy_oochars (client->errmsg.buf, client->dummy_hak->errmsg.buf, HAK_COUNTOF(client->errmsg.buf));
	client->errmsg.len = client->dummy_hak->errmsg.len;
}

void hak_client_seterrufmt (hak_client_t* client, hak_errnum_t errnum, const hak_uch_t* fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	hak_seterrufmtv (client->dummy_hak, errnum, fmt, ap);
	va_end (ap);

	HAK_ASSERT(client->dummy_hak, HAK_COUNTOF(client->errmsg.buf) == HAK_COUNTOF(client->dummy_hak->errmsg.buf));
	client->errnum = errnum;
	hak_copy_oochars (client->errmsg.buf, client->dummy_hak->errmsg.buf, HAK_COUNTOF(client->errmsg.buf));
	client->errmsg.len = client->dummy_hak->errmsg.len;
}

/* ========================================================================= */

void hak_client_logbfmt (hak_client_t* client, hak_bitmask_t mask, const hak_bch_t* fmt, ...)
{
	va_list ap;
	va_start (ap, fmt);
	hak_logbfmtv (client->dummy_hak, mask, fmt, ap);
	va_end (ap);
}

void hak_client_logufmt (hak_client_t* client, hak_bitmask_t mask, const hak_uch_t* fmt, ...)
{
	va_list ap;
	va_start (ap, fmt);
	hak_logufmtv (client->dummy_hak, mask, fmt, ap);
	va_end (ap);
}

/* ========================================================================= */

void* hak_client_allocmem (hak_client_t* client, hak_oow_t size)
{
	void* ptr;

	ptr = HAK_MMGR_ALLOC(client->_mmgr, size);
	if (!ptr) hak_client_seterrnum (client, HAK_ESYSMEM);
	return ptr;
}

void* hak_client_callocmem (hak_client_t* client, hak_oow_t size)
{
	void* ptr;

	ptr = HAK_MMGR_ALLOC(client->_mmgr, size);
	if (!ptr) hak_client_seterrnum (client, HAK_ESYSMEM);
	else HAK_MEMSET(ptr, 0, size);
	return ptr;
}

void* hak_client_reallocmem (hak_client_t* client, void* ptr, hak_oow_t size)
{
	ptr = HAK_MMGR_REALLOC(client->_mmgr, ptr, size);
	if (!ptr) hak_client_seterrnum (client, HAK_ESYSMEM);
	return ptr;
}

void hak_client_freemem (hak_client_t* client, void* ptr)
{
	HAK_MMGR_FREE (client->_mmgr, ptr);
}

/* ========================================================================= */

struct proto_xtn_t
{
	hak_client_t* client;
};
typedef struct proto_xtn_t proto_xtn_t;

static int proto_on_packet (hak_xproto_t* proto, hak_xpkt_type_t type, const void* data, hak_oow_t len)
{
	proto_xtn_t* proto_xtn;
	hak_client_t* client;
	proto_xtn = hak_xproto_getxtn(proto);
	client = proto_xtn->client;
	return client->prim.on_packet(client, type, data, len);
}

static int client_connect_to_server (hak_client_t* client, const char* ipaddr)
{
	hak_sckaddr_t sckaddr;
	hak_scklen_t scklen;
	int sckfam;
	int sck = -1;
	hak_xproto_t* proto = HAK_NULL;

	proto_xtn_t* proto_xtn;
	hak_xproto_cb_t proto_cb;

	sckfam = hak_bchars_to_sckaddr(ipaddr, strlen(ipaddr), &sckaddr, &scklen);
	if (sckfam <= -1)
	{
		hak_client_seterrbfmt (client, HAK_EINVAL, "cannot convert ip address - %hs", ipaddr);
		goto oops;
	}

	sck = socket(sckfam, SOCK_STREAM, 0);
	if (sck <= -1)
	{
		hak_client_seterrbfmt (client, HAK_ESYSERR, "cannot create socket - %hs", strerror(errno));
		goto oops;
	}

	hak_sys_set_cloexec(sck, 1);

#if 0
	if (sckfam == AF_INET)
	{
		struct sockaddr_in anyaddr;
		int opt = 1;
		setsockopt(sck, SOL_SOCKET, SO_REUSEADDR, (char *)&opt, sizeof(opt));
		HAK_MEMSET(&anyaddr, 0, HAK_SIZEOF(anyaddr));
		anyaddr.sin_family = sckfam;
		if (bind(sck, (struct sockaddr *)&anyaddr, scklen) <= -1)
		{
			hak_client_seterrbfmt (client, HAK_ESYSERR,
				"cannot bind socket %d - %hs", sck, strerror(errno));
			goto oops;
		}
	}
	else if (sckfam == AF_INET6)
	{
		struct sockaddr_in6 anyaddr;
		int opt = 1;
		setsockopt(sck, SOL_SOCKET, SO_REUSEADDR, (char *)&opt, sizeof(opt));
		HAK_MEMSET(&anyaddr, 0, HAK_SIZEOF(anyaddr));
		anyaddr.sin6_family = sckfam;
		if (bind(sck, (struct sockaddr *)&anyaddr, scklen) <= -1)
		{
			hak_client_seterrbfmt (client, HAK_ESYSERR,
				"cannot bind socket %d - %hs", sck, strerror(errno));
			goto oops;
		}
	}
#endif

/* TODO: async connect? */
/* TODO: connect timeout */
	if (connect(sck, (struct sockaddr*)&sckaddr, scklen) <= -1)
	{
		hak_client_seterrbfmt (client, HAK_ESYSERR,
			"cannot connect socket %d to %hs - %hs", sck, ipaddr, strerror(errno));
		goto oops;
	}

	hak_sys_set_nonblock(sck, 1); /* make it nonblocking after connection has been established */

	HAK_MEMSET(&proto, 0, HAK_SIZEOF(proto_cb));
	proto_cb.on_packet = proto_on_packet;

	proto = hak_xproto_open(hak_client_getmmgr(client), &proto_cb, HAK_SIZEOF(*proto_xtn));
	if (HAK_UNLIKELY(!proto))
	{
		hak_client_seterrbfmt (client, HAK_ESYSERR, "cannot open protocol to %s", ipaddr);
		goto oops;
	}
	proto_xtn = hak_xproto_getxtn(proto);
	proto_xtn->client = client;

	client->remote.sck = sck;
	client->remote.proto = proto;
	return 0;

oops:
	if (proto) hak_xproto_close (proto);
	if (sck >= 0) close (sck);
	return -1;
}


static void client_close (hak_client_t* client)
{
	if (client->remote.proto)
	{
		hak_xproto_close (client->remote.proto);
		client->remote.proto = HAK_NULL;
	}

	if (client->remote.sck >= 0)
	{
		close (client->remote.sck);
		client->remote.sck = -1;
	}
}

static int client_add_to_local_pw2r (hak_client_t* client, const hak_uint8_t* ptr, hak_oow_t len)
{
	if (client->local.pw2r.len >= client->local.pw2r.capa)
	{
		hak_uint8_t* tmp;
		hak_oow_t newcapa;

		newcapa = HAK_ALIGN_POW2(client->local.pw2r.capa + len, 128);
		tmp = hak_client_reallocmem(client, client->local.pw2r.ptr, newcapa * HAK_SIZEOF(*client->local.pw2r.ptr));
		if (HAK_UNLIKELY(!tmp)) return -1;

		client->local.pw2r.capa = newcapa;
		client->local.pw2r.ptr = tmp;
	}

	HAK_MEMCPY(&client->local.pw2r.ptr[client->local.pw2r.len], ptr, len);
	client->local.pw2r.len += len;
	return 0;
}

static int client_send_to_remote (hak_client_t* client, hak_xpkt_type_t pktype, const hak_uint8_t* ptr, hak_oow_t len)
{
	hak_xpkt_hdr_t hdr;
	struct iovec iov[2];
	hak_uint16_t seglen;
	int n, i;

	do
	{
		seglen = (len > HAK_XPKT_MAX_PLD_LEN)? HAK_XPKT_MAX_PLD_LEN: len;

		hdr.id = 1; /* TODO: */
		hdr.type = pktype | (((seglen >> 8) & 0x0F) << 4);
		hdr.len = seglen & 0xFF;

		i = 0;
		iov[i].iov_base = &hdr;
		iov[i++].iov_len = HAK_SIZEOF(hdr);
		if (seglen > 0)
		{
			iov[i].iov_base = ptr;
			iov[i++].iov_len = seglen;
		}

		n = hak_sys_send_iov(client->remote.sck, iov, i);
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

static void on_control_event (hak_client_t* client, struct pollfd* pfd)
{
	char tmp[128];
hak_client_logbfmt(client, HAK_LOG_STDERR, "ON CONTROL EVENT \n");
	while (read(client->mux_pipe[0], tmp, HAK_SIZEOF(tmp)) > 0) /* nothing */;
/* TODO: handle different command? */
}

static void on_remote_event (hak_client_t* client, struct pollfd* pfd, int shut_wr_after_req)
{
//hak_client_logbfmt(client, HAK_LOG_STDERR, "ON REMOTE EVENT \n");

	if (pfd->revents & POLLOUT)
	{
		ssize_t n;
		hak_oow_t len;

		len = client->local.pw2r.len - client->local.pw2r.pos;
		n = hak_sys_send(client->remote.sck, &client->local.pw2r.ptr[client->local.pw2r.pos], &len);
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
hak_client_logbfmt(client, HAK_LOG_STDERR, "send error - %hs\n", strerror(errno));
			goto reqstop;
		}
	}

	if (pfd->revents & POLLIN)
	{
		hak_oow_t bcap;
		hak_uint8_t* bptr;
		ssize_t x;

		bptr = hak_xproto_getbuf(client->remote.proto, &bcap);;
		x = recv(client->remote.sck, bptr, bcap, 0);
		if (x <= -1)
		{
			if (errno == EINTR) goto carry_on; /* didn't read read */
hak_client_logbfmt(client, HAK_LOG_STDERR, "recv error from remote - %hs", strerror(errno));
			/*hak_seterrwithsyserr(hak, 0, errno); */
			/* TODO: error info set... */
			goto reqstop;
		}
		if (x == 0) hak_xproto_seteof(client->remote.proto, 1);
		hak_xproto_advbuf (client->remote.proto, x);
	}


carry_on:
	/* handle the data received from the remote side */
	while (hak_xproto_ready(client->remote.proto))
	{
		int n;

		if ((n = hak_xproto_process(client->remote.proto)) <= -1)
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

	if (hak_xproto_geteof(client->remote.proto))
	{
		client->state |= STATE_REMOTE_IN_CLOSED;
		goto reqstop;
	}
	return;

reqstop:
	client->stopreq = 1;
	return;
}

static void on_local_in_event (hak_client_t* client, struct pollfd* pfd)
{
	ssize_t n;
	hak_uint8_t buf[128];

//hak_client_logbfmt(client, HAK_LOG_STDERR, "local in on %d\n", pfd->fd);
	n = read(pfd->fd, buf, HAK_SIZEOF(buf));
	if (n <= -1)
	{
		//if (hak_sys_is_errno_wb(errno)) ...
hak_client_logbfmt(client, HAK_LOG_STDERR, "local in read error - %hs\n", strerror(errno));
		client->stopreq = 1;
	}
	else if (n == 0)
	{
/*hak_client_logbfmt(client, HAK_LOG_STDERR, "local in eof\n");*/
/* TODO ARRANGE TO FINISH.. AFTER EXUCTION OF REMAINING STUFF... */
		//client->stopreq = 1;
		client->state |= STATE_LOCAL_IN_CLOSED;
		n = client_send_to_remote(client, HAK_XPKT_EXECUTE, HAK_NULL, 0);
		if (n <= -1)
		{
hak_client_logbfmt(client, HAK_LOG_STDERR, "local to remote  (execute)- %hs\n", strerror(errno));
		}
	}
	else
	{
/*hak_client_logbfmt(client, HAK_LOG_STDERR, "local read - %ld\n", (long)n);*/
		n = client_send_to_remote(client, HAK_XPKT_CODE, buf, n);
		if (n <= -1)
		{
hak_client_logbfmt(client, HAK_LOG_STDERR, "local to remote (code)- %hs\n", strerror(errno));
		}
	}
}

static int client_setup_local(hak_client_t* client)
{
	client->local.in = STDIN_FILENO;
	client->local.out = STDOUT_FILENO;
	client->local.err = STDERR_FILENO;
	return 0;
}

int hak_client_start (hak_client_t* client, const char* ipaddr, int shut_wr_after_req)
{
	/*  TODO: cin, cout, cerr could be actual files or something other than the console.
	          the actual loop won't begin until all these file descriptors are ready */

	client->stopreq = 0;
	if (client_setup_local(client) <= -1) return -1;
hak_client_logbfmt(client, HAK_LOG_STDERR, "staritg XXXXXXXXXXX loop... ...\n");
	if (client_connect_to_server(client, ipaddr) <= -1) return -1; /* TODO: support time out or abort while connecting... */

hak_client_logbfmt(client, HAK_LOG_STDERR, "staritg client loop... ...\n");
	while (!client->stopreq)
	{
		int nfds, i;
		struct pollfd pfd[10];

		if ((client->state & HAK_CLIENT_ALL_CLOSED) == HAK_CLIENT_ALL_CLOSED)
		{
			/* no explicit stop request. but all file descriptors reached EOF */
			break;
		}

		HAK_MEMSET(pfd, 0, HAK_SIZEOF(pfd));
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
//hak_client_logbfmt(client, HAK_LOG_STDERR, "ADDING LOCAL IN TO MULTIPLEX...\n");
				pfd[nfds].fd = client->local.in;
				pfd[nfds].events = POLLIN;
				pfd[nfds++].revents = 0;
			}
		}

		i = poll(pfd, nfds, 1000);
//hak_client_logbfmt(client, HAK_LOG_STDERR, "poll returned %d\n", i);
		if (i <= -1)
		{
			hak_client_seterrbfmt (client, HAK_ESYSERR, "poll error - %hs", strerror(errno));
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

//hak_client_logbfmt(client, HAK_LOG_STDERR, "EVENT ON %d mux[%d], remote[%d], local[%d]\n", pfd[i].fd, client->mux_pipe[0], client->remote.sck, client->local.in);
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

void hak_client_stop (hak_client_t* client)
{
	client->stopreq = 1;
	write (client->mux_pipe[1], "Q", 1); /* don't care about failure */
}
