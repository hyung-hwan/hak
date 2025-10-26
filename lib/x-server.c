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
#	include <signal.h>
#elif defined(macintosh)
#	include <Timer.h>
#else

#	if defined(HAVE_TIME_H)
#		include <time.h>
#	endif
#	if defined(HAVE_SYS_TIME_H)
#		include <sys/time.h>
#	endif
#	if defined(HAVE_SIGNAL_H)
#		include <signal.h>
#	endif
#	if defined(HAVE_SYS_MMAN_H)
#		include <sys/mman.h>
#	endif
#	if defined(HAVE_SYS_UIO_H)
#		include <sys/uio.h>
#	endif
#	if defined(HAVE_SYS_EPOLL_H)
#		include <sys/epoll.h>
#		define USE_EPOLL
#	endif

#	include <unistd.h>
#	include <fcntl.h>
#	include <sys/types.h>
#	include <sys/socket.h>
#	include <netinet/in.h>
#	include <pthread.h>
#	include <poll.h>
#endif

struct bb_t
{
	char buf[1024];
	hak_oow_t pos;
	hak_oow_t len;
	int fd;
	hak_bch_t* fn;
};
typedef struct bb_t bb_t;

struct proto_xtn_t
{
	hak_server_worker_t* worker;
};
typedef struct proto_xtn_t proto_xtn_t;

struct worker_hak_xtn_t
{
	hak_server_worker_t* worker;
	int vm_running;
};
typedef struct worker_hak_xtn_t worker_hak_xtn_t;

struct server_hak_xtn_t
{
	hak_server_t* server;
};
typedef struct server_hak_xtn_t server_hak_xtn_t;

/* ---------------------------------- */

enum hak_server_worker_state_t
{
	HAK_SERVER_WORKER_STATE_DEAD  = 0,
	HAK_SERVER_WORKER_STATE_ALIVE = 1,
	HAK_SERVER_WORKER_STATE_ZOMBIE = 2 /* the worker is not chained in the server's client list */
};
typedef enum hak_server_worker_state_t hak_server_worker_state_t;

enum hak_server_worker_opstate_t
{
	HAK_SERVER_WORKER_OPSTATE_IDLE = 0,
	HAK_SERVER_WORKER_OPSTATE_ERROR = 1,
	HAK_SERVER_WORKER_OPSTATE_WAIT = 2,
	HAK_SERVER_WORKER_OPSTATE_READ = 3,
	HAK_SERVER_WORKER_OPSTATE_COMPILE = 4,
	HAK_SERVER_WORKER_OPSTATE_EXECUTE = 5
};
typedef enum hak_server_worker_opstate_t hak_server_worker_opstate_t;

struct hak_server_worker_t
{
	pthread_t thr;
	hak_oow_t wid;

	int sck;
	hak_sckaddr_t peeraddr;

	int claimed;

	hak_ntime_t alloc_time;
	hak_server_worker_state_t state;
	hak_server_worker_opstate_t opstate;
	hak_tmr_index_t exec_runtime_event_index;
	hak_xproto_t* proto;
	hak_t* hak;

	hak_server_t* server;
	hak_server_worker_t* prev_worker;
	hak_server_worker_t* next_worker;
};

struct hak_server_wid_map_data_t
{
	int used;
	union
	{
		hak_server_worker_t* worker;
		hak_oow_t            next;
	} u;
};
typedef struct hak_server_wid_map_data_t hak_server_wid_map_data_t;

typedef struct hak_server_listener_t hak_server_listener_t;
struct hak_server_listener_t
{
	int sck;
	hak_sckaddr_t sckaddr;
	hak_server_listener_t* next_listener;
};

struct hak_server_t
{
	hak_oow_t    _instsize;
	hak_mmgr_t*  _mmgr;
	hak_cmgr_t*  _cmgr;
	hak_server_prim_t prim;

	/* [NOTE]
	 *  this dummy_hak is used when the main thread requires logging mostly.
	 *  as there is no explicit locking when calling HAK_LOG() functions,
	 *  the code must ensure that the logging functions are called in the
	 *  context of the main server thraed only.  error message setting is
	 *  also performed in the main thread context for the same reason.
	 *
	 *  however, you may have noticed mixed use of HAK_ASSERT with dummy_hak
	 *  in both the server thread context and the client thread contexts.
	 *  it should be ok as assertion is only for debugging and it's operation
	 *  is thread safe. */
	hak_t* dummy_hak;

	hak_tmr_t* tmr;

	hak_errnum_t errnum;
	struct
	{
		hak_ooch_t buf[HAK_ERRMSG_CAPA];
		hak_oow_t len;
	} errmsg;
	int stopreq;

	struct
	{
		hak_bitmask_t trait;
		hak_bitmask_t logmask;
		hak_oow_t worker_stack_size;
		hak_oow_t worker_max_count;
		hak_ntime_t worker_idle_timeout;
		hak_oow_t actor_heap_size;
		hak_ntime_t actor_max_runtime;
		hak_ooch_t script_include_path[HAK_PATH_MAX + 1];
		void* module_inctx;
	} cfg;

	struct
	{
	#if defined(USE_EPOLL)
		int ep_fd;
		struct epoll_event ev_buf[128];
	#endif
		hak_server_listener_t* head;
		hak_oow_t count;
	} listener;

	struct
	{
		hak_server_worker_t* head;
		hak_server_worker_t* tail;
		hak_oow_t count;
	} worker_list[2]; /* DEAD and ALIVE oly. ZOMBIEs are not chained here */

	struct
	{
		hak_server_wid_map_data_t* ptr;
		hak_oow_t                  capa;
		hak_oow_t                  free_first;
		hak_oow_t                  free_last;
	} wid_map; /* worker's id map */

	int mux_pipe[2]; /* pipe to break the blocking multiplexer in the main server loop */

	pthread_mutex_t worker_mutex;
	pthread_mutex_t tmr_mutex;
	pthread_mutex_t log_mutex;
};

/* ========================================================================= */
static int send_bytes (hak_xproto_t* proto, hak_xpkt_type_t xpkt_code, const hak_bch_t* data, hak_oow_t len);

#if defined(HAK_OOCH_IS_UCH)
static int send_chars (hak_xproto_t* proto, hak_xpkt_type_t xpkt_code, const hak_ooch_t* data, hak_oow_t len);
#else
#define send_chars(proto,xpkt_code,data,len) send_bytes(proto,xpkt_code,data,len)
#endif

/* ========================================================================= */

static HAK_INLINE int open_read_stream (hak_t* hak, hak_io_cciarg_t* arg)
{
	worker_hak_xtn_t* xtn = (worker_hak_xtn_t*)hak_getxtn(hak);
	bb_t* bb = HAK_NULL;
	hak_server_t* server;

	server = xtn->worker->server;

	if (arg->includer)
	{
		/* includee */
		/* TOOD: Do i need to skip prepending the include path if the included path is an absolute path?
		 *       it may be good for security if i don't skip it. we can lock the included files in a given directory */
		hak_oow_t ucslen, bcslen, parlen;
		const hak_bch_t* fn, * fb;

	#if defined(HAK_OOCH_IS_UCH)
		if (hak_convootobcstr(hak, arg->name, &ucslen, HAK_NULL, &bcslen) <= -1) goto oops;
	#else
		bcslen = hak_count_bcstr(arg->name);
	#endif

		fn = ((bb_t*)arg->includer->handle)->fn;
		if (fn[0] == '\0' && server->cfg.script_include_path[0] != '\0')
		{
		#if defined(HAK_OOCH_IS_UCH)
			if (hak_convootobcstr(hak, server->cfg.script_include_path, &ucslen, HAK_NULL, &parlen) <= -1) goto oops;
		#else
			parlen = hak_count_bcstr(server->cfg.script_include_path);
		#endif
		}
		else
		{
			fb = hak_get_base_name_from_bcstr_path(fn);
			parlen = fb - fn;
		}

		bb = (bb_t*)hak_callocmem(hak, HAK_SIZEOF(*bb) + (HAK_SIZEOF(hak_bch_t) * (parlen + bcslen + 2)));
		if (HAK_UNLIKELY(!bb)) goto oops;

		bb->fn = (hak_bch_t*)(bb + 1);
		if (fn[0] == '\0' && server->cfg.script_include_path[0] != '\0')
		{
		#if defined(HAK_OOCH_IS_UCH)
			hak_convootobcstr(hak, server->cfg.script_include_path, &ucslen, bb->fn, &parlen);
		#else
			hak_copy_bchars (bb->fn, server->cfg.script_include_path, parlen);
		#endif
			if (!HAK_IS_PATH_SEP(bb->fn[parlen])) bb->fn[parlen++] = HAK_DFL_PATH_SEP; /* +2 was used in hak_callocmem() for this (+1 for this, +1 for '\0' */
		}
		else
		{
			hak_copy_bchars (bb->fn, fn, parlen);
		}

	#if defined(HAK_OOCH_IS_UCH)
		hak_convootobcstr(hak, arg->name, &ucslen, &bb->fn[parlen], &bcslen);
	#else
		hak_copy_bcstr (&bb->fn[parlen], bcslen + 1, arg->name);
	#endif
		bb->fd = open(bb->fn, O_RDONLY, 0);

		if (bb->fd <= -1)
		{
			hak_seterrnum(hak, HAK_EIOERR);
			goto oops;
		}
	}
	else
	{
		/* main stream */
		hak_oow_t pathlen = 0;
		bb = (bb_t*)hak_callocmem(hak, HAK_SIZEOF(*bb) + (HAK_SIZEOF(hak_bch_t) * (pathlen + 1)));
		if (HAK_UNLIKELY(!bb)) goto oops;

		/* copy ane empty string as a main stream's name */
		bb->fn = (hak_bch_t*)(bb + 1);
		hak_copy_bcstr (bb->fn, pathlen + 1, "");

		bb->fd = xtn->worker->sck;
	}

	HAK_ASSERT(hak, bb->fd >= 0);

	arg->handle = bb;
	return 0;

oops:
	if (bb)
	{
		if (bb->fd >= 0 && bb->fd != xtn->worker->sck) close (bb->fd);
		hak_freemem(hak, bb);
	}
	return -1;
}

static HAK_INLINE int close_read_stream (hak_t* hak, hak_io_cciarg_t* arg)
{
	worker_hak_xtn_t* xtn = (worker_hak_xtn_t*)hak_getxtn(hak);
	bb_t* bb;

	bb = (bb_t*)arg->handle;
	HAK_ASSERT(hak, bb != HAK_NULL && bb->fd >= 0);

	if (bb->fd != xtn->worker->sck) close (bb->fd);
	hak_freemem(hak, bb);

	arg->handle = HAK_NULL;
	return 0;
}

static HAK_INLINE int read_read_stream (hak_t* hak, hak_io_cciarg_t* arg)
{
	worker_hak_xtn_t* xtn = (worker_hak_xtn_t*)hak_getxtn(hak);
	bb_t* bb;
	hak_oow_t bcslen, ucslen, remlen;
	hak_server_worker_t* worker;
	ssize_t x;
	int y;

	bb = (bb_t*)arg->handle;
	HAK_ASSERT(hak, bb != HAK_NULL && bb->fd >= 0);

	worker = xtn->worker;

start_over:
	if (arg->includer)
	{
		/* includee */
		if (HAK_UNLIKELY(worker->server->stopreq))
		{
			hak_seterrbfmt(hak, HAK_EGENERIC, "stop requested");
			return -1;
		}

		x = read(bb->fd, &bb->buf[bb->len], HAK_COUNTOF(bb->buf) - bb->len);
		if (x <= -1)
		{
			if (errno == EINTR) goto start_over;
			hak_seterrwithsyserr(hak, 0, errno);
			return -1;
		}

		bb->len += x;
	}
	else
	{
		/* main stream */
		hak_server_t* server;

		HAK_ASSERT(hak, bb->fd == worker->sck);
		server = worker->server;

		while (1)
		{
			int n;
			struct pollfd pfd;
			int tmout, actual_tmout;

			if (HAK_UNLIKELY(server->stopreq))
			{
				hak_seterrbfmt(hak, HAK_EGENERIC, "stop requested");
				return -1;
			}

			tmout = HAK_SECNSEC_TO_MSEC(server->cfg.worker_idle_timeout.sec, server->cfg.worker_idle_timeout.nsec);
			actual_tmout = (tmout <= 0)? 10000: tmout;

			pfd.fd = bb->fd;
			pfd.events = POLLIN | POLLERR;
			n = poll(&pfd, 1, actual_tmout);
			if (n <= -1)
			{
				if (errno == EINTR) goto start_over;
				hak_seterrwithsyserr(hak, 0, errno);
				return -1;
			}
			else if (n >= 1) break;

			/* timed out - no activity on the pfd */
			if (tmout > 0)
			{
				hak_seterrbfmt(hak, HAK_EGENERIC, "no activity on the worker socket %d", bb->fd);
				return -1;
			}
		}

		x = recv(bb->fd, &bb->buf[bb->len], HAK_COUNTOF(bb->buf) - bb->len, 0);
		if (x <= -1)
		{
			if (errno == EINTR) goto start_over;
			hak_seterrwithsyserr(hak, 0, errno);
			return -1;
		}

		bb->len += x;
	}

#if defined(HAK_OOCH_IS_UCH)
	bcslen = bb->len;
	ucslen = HAK_COUNTOF(arg->buf.c);
	y = hak_convbtooochars(hak, bb->buf, &bcslen, arg->buf.c, &ucslen);
	if (y <= -1 && ucslen <= 0)
	{
		if (y == -3 && x != 0) goto start_over; /* incomplete sequence and not EOF yet */
		return -1;
	}
	/* if ucslen is greater than 0, i see that some characters have been
	 * converted properly */
#else
	bcslen = (bb->len < HAK_COUNTOF(arg->buf.b))? bb->len: HAK_COUNTOF(arg->buf.b);
	ucslen = bcslen;
	hak_copy_bchars (arg->buf.b, bb->buf, bcslen);
#endif

	remlen = bb->len - bcslen;
	if (remlen > 0) HAK_MEMMOVE(bb->buf, &bb->buf[bcslen], remlen);
	bb->len = remlen;

	arg->xlen = ucslen;
	return 0;
}


static int read_handler (hak_t* hak, hak_io_cmd_t cmd, void* arg)
{
	switch (cmd)
	{
		case HAK_IO_OPEN:
			return open_read_stream(hak, (hak_io_cciarg_t*)arg);

		case HAK_IO_CLOSE:
			return close_read_stream(hak, (hak_io_cciarg_t*)arg);

		case HAK_IO_READ:
			return read_read_stream(hak, (hak_io_cciarg_t*)arg);

		default:
			hak_seterrnum(hak, HAK_EINTERN);
			return -1;
	}
}

static int scan_handler (hak_t* hak, hak_io_cmd_t cmd, void* arg)
{
	switch (cmd)
	{
		case HAK_IO_OPEN:
			return 0;

		case HAK_IO_CLOSE:
			return 0;

		case HAK_IO_READ:
#if 0
		{
			worker_hak_xtn_t* xtn = (worker_hak_xtn_t*)hak_getxtn(hak);
			hak_io_udiarg_t* inarg = (hak_io_udiarg_t*)arg;

// what if it writes a request to require more input??
			if (hak_xproto_handle_incoming(xtn->proto) <= -1)
			{
			}
		}
#else
			/* TODO: read from the input buffer or pipe*/
#endif

		default:
			hak_seterrnum(hak, HAK_EINTERN);
			return -1;
	}
}

static int print_handler (hak_t* hak, hak_io_cmd_t cmd, void* arg)
{
	switch (cmd)
	{
		case HAK_IO_OPEN:
printf ("IO OPEN SOMETHING...........\n");
			return 0;

		case HAK_IO_CLOSE:
printf ("IO CLOSE SOMETHING...........\n");
			return 0;

		case HAK_IO_WRITE:
		{
			worker_hak_xtn_t* xtn = (worker_hak_xtn_t*)hak_getxtn(hak);
			hak_io_udoarg_t* outarg = (hak_io_udoarg_t*)arg;

/*printf ("IO WRITE SOMETHING...........\n");*/
			if (send_chars(xtn->worker->proto, HAK_XPKT_STDOUT, outarg->ptr, outarg->len) <= -1)
			{
				/* TODO: change error code and message. propagage the errormessage from proto */
				hak_seterrbfmt(hak, HAK_EIOERR, "failed to write message via proto");

				/* writing failure on the socket is a critical failure.
				 * execution must get aborted */
				hak_abort(hak);
				return -1;
			}

			outarg->xlen = outarg->len;
			return 0;
		}
		case HAK_IO_WRITE_BYTES:
		{
			worker_hak_xtn_t* xtn = (worker_hak_xtn_t*)hak_getxtn(hak);
			hak_io_udoarg_t* outarg = (hak_io_udoarg_t*)arg;

/*printf ("IO WRITE SOMETHING BYTES...........\n");*/
			if (send_bytes(xtn->worker->proto, HAK_XPKT_STDOUT, outarg->ptr, outarg->len) <= -1)
			{
				/* TODO: change error code and message. propagage the errormessage from proto */
				hak_seterrbfmt(hak, HAK_EIOERR, "failed to write message via proto");

				/* writing failure on the socket is a critical failure.
				 * execution must get aborted */
				hak_abort(hak);
				return -1;
			}
			outarg->xlen = outarg->len;
			return 0;
		}

		case HAK_IO_FLUSH:
/* TODO: flush data... */
			return 0;

		default:
			hak_seterrnum(hak, HAK_EINTERN);
			return -1;
	}
}

/* ========================================================================= */

static void server_log_write (hak_t* hak, hak_bitmask_t mask, const hak_ooch_t* msg, hak_oow_t len)
{
	worker_hak_xtn_t* xtn = (worker_hak_xtn_t*)hak_getxtn(hak);
	hak_server_t* server;

	server = xtn->worker->server;
	pthread_mutex_lock (&server->log_mutex);
	server->prim.log_write (server, xtn->worker->wid, mask, msg, len);
	pthread_mutex_unlock (&server->log_mutex);
}

static void server_log_write_for_dummy (hak_t* hak, hak_bitmask_t mask, const hak_ooch_t* msg, hak_oow_t len)
{
	server_hak_xtn_t* xtn = (server_hak_xtn_t*)hak_getxtn(hak);
	hak_server_t* server;

	server = xtn->server;
	pthread_mutex_lock (&server->log_mutex);
	server->prim.log_write (server, HAK_SERVER_WID_INVALID, mask, msg, len);
	pthread_mutex_unlock (&server->log_mutex);
}

/* ========================================================================= */

static int vm_startup (hak_t* hak)
{
	worker_hak_xtn_t* xtn = (worker_hak_xtn_t*)hak_getxtn(hak);
	xtn->vm_running = 1;
	return 0;
}

static void vm_cleanup (hak_t* hak)
{
	worker_hak_xtn_t* xtn = (worker_hak_xtn_t*)hak_getxtn(hak);
	xtn->vm_running = 0;
}

static void vm_checkbc (hak_t* hak, hak_oob_t bcode)
{
	worker_hak_xtn_t* xtn = (worker_hak_xtn_t*)hak_getxtn(hak);
	if (xtn->worker->server->stopreq) hak_abort(hak);
}

/*
static void gc_hak (hak_t* hak)
{
	worker_hak_xtn_t* xtn = (worker_hak_xtn_t*)hak_getxtn(hak);
}

static void fini_hak (hak_t* hak)
{
	worker_hak_xtn_t* xtn = (worker_hak_xtn_t*)hak_getxtn(hak);
}
*/
/* ========================================================================= */


static hak_server_worker_t* proto_to_worker (hak_xproto_t* proto)
{
	proto_xtn_t* prtxtn;
	prtxtn = (proto_xtn_t*)hak_xproto_getxtn(proto);
	return prtxtn->worker;
}

/* ========================================================================= */

#define SERVER_LOGMASK_INFO (HAK_LOG_INFO | HAK_LOG_APP)
#define SERVER_LOGMASK_ERROR (HAK_LOG_ERROR | HAK_LOG_APP)

static int on_fed_cnode (hak_t* hak, hak_cnode_t* obj)
{
	worker_hak_xtn_t* hak_xtn = (worker_hak_xtn_t*)hak_getxtn(hak);
	hak_server_worker_t* worker;
	hak_xproto_t* proto;
	int flags = 0;

	hak_xtn = (worker_hak_xtn_t*)hak_getxtn(hak);
	worker = hak_xtn->worker;
	proto = worker->proto;

	/* the compile error must not break the input loop.
	 * this function returns 0 to go on despite a compile-time error.
	 *
	 * if a single line or continued lines contain multiple expressions,
	 * execution is delayed until the last expression is compiled. */

	if (hak_compile(hak, obj, flags) <= -1)
	{
		const hak_bch_t* errmsg = hak_geterrbmsg(hak);
		send_bytes(proto, HAK_XPKT_ERROR, errmsg, hak_count_bcstr(errmsg));
/* TODO: ignore the whole line??? */
	}

	return 0;
}

static void exec_runtime_handler (hak_tmr_t* tmr, const hak_ntime_t* now, hak_tmr_event_t* evt)
{
	/* [NOTE] this handler is executed in the main server thread
	 *         when it calls hak_tmr_fire().  */
	hak_server_worker_t* worker;

	worker = proto_to_worker((hak_xproto_t*)evt->ctx);
/* TODO: can we use worker->hak for logging before abort?? */
	HAK_LOG1 (worker->server->dummy_hak, SERVER_LOGMASK_INFO, "Aborting script execution for max_actor_runtime exceeded [%zu]\n", worker->wid);
	hak_abort (worker->hak);
}

static void exec_runtime_updater (hak_tmr_t* tmr, hak_tmr_index_t old_index, hak_tmr_index_t new_index, hak_tmr_event_t* evt)
{
	/* [NOTE] this handler is executed in the main server thread
	 *        when it calls hak_tmr_fire() */
	hak_xproto_t* proto;
	hak_server_worker_t* worker;

	proto = (hak_xproto_t*)evt->ctx;
	worker = proto_to_worker(proto);
	HAK_ASSERT(worker->hak, worker->exec_runtime_event_index == old_index);

	/* the event is being removed by hak_tmr_fire() or by hak_tmr_delete()
	 * if new_index is HAK_TMR_INVALID_INDEX. it's being updated if not. */
	worker->exec_runtime_event_index = new_index;
}

static int insert_exec_timer (hak_xproto_t* proto, const hak_ntime_t* tmout)
{
	/* [NOTE] this is executed in the worker thread */

	hak_tmr_event_t event;
	hak_tmr_index_t index;
	hak_server_worker_t* worker;
	hak_server_t* server;

	worker = proto_to_worker(proto);
	server = worker->server;

	HAK_ASSERT(worker->hak, worker->exec_runtime_event_index == HAK_TMR_INVALID_INDEX);

	HAK_MEMSET(&event, 0, HAK_SIZEOF(event));
	event.ctx = proto;
	worker->hak->vmprim.vm_gettime (worker->hak, &event.when);
	HAK_ADD_NTIME (&event.when, &event.when, tmout);
	event.handler = exec_runtime_handler;
	event.updater = exec_runtime_updater;

	pthread_mutex_lock (&server->tmr_mutex);
	index = hak_tmr_insert(server->tmr, &event);
	worker->exec_runtime_event_index = index;
	if (index != HAK_TMR_INVALID_INDEX)
	{
		/* inform the server of timer event change */
		write (server->mux_pipe[1], "X", 1); /* don't care even if it fails */
	}
	pthread_mutex_unlock (&server->tmr_mutex);

	return (index == HAK_TMR_INVALID_INDEX)? -1: 0;
}

static void delete_exec_timer (hak_xproto_t* proto)
{
	/* [NOTE] this is executed in the worker thread. if the event has been fired
	 *        in the server thread, worker->exec_runtime_event_index should be
	 *        HAK_TMR_INVALID_INDEX as set by exec_runtime_handler */
	hak_server_worker_t* worker;
	hak_server_t* server;

	worker = proto_to_worker(proto);
	server = worker->server;

	pthread_mutex_lock (&server->tmr_mutex);
	if (worker->exec_runtime_event_index != HAK_TMR_INVALID_INDEX)
	{
		/* the event has not been fired yet. let's delete it
		 * if it has been fired, the index it shall be HAK_TMR_INVALID_INDEX already */

		hak_tmr_delete (server->tmr, worker->exec_runtime_event_index);
		HAK_ASSERT(worker->hak, worker->exec_runtime_event_index == HAK_TMR_INVALID_INDEX);
		/*worker->exec_runtime_event_index = HAK_TMR_INVALID_INDEX;	*/
	}
	pthread_mutex_unlock (&server->tmr_mutex);
}

static int execute_script (hak_xproto_t* proto, const hak_bch_t* trigger)
{
	hak_oop_t obj;
	const hak_ooch_t* failmsg = HAK_NULL;
	hak_server_worker_t* worker;
	hak_server_t* server;

	worker = proto_to_worker(proto);
	server = worker->server;

#if 0
	hak_xproto_start_reply (proto);
#endif
	if (server->cfg.actor_max_runtime.sec <= 0 && server->cfg.actor_max_runtime.sec <= 0)
	{
		obj = hak_execute(worker->hak);
		if (!obj) failmsg = hak_geterrmsg(worker->hak);
	}
	else
	{
		if (insert_exec_timer(proto, &server->cfg.actor_max_runtime) <= -1)
		{
			HAK_LOG0 (worker->hak, SERVER_LOGMASK_ERROR, "Cannot start execution timer\n");
			hak_seterrbfmt (worker->hak, HAK_ESYSMEM, "cannot start execution timer");  /* i do this just to compose the error message  */
			failmsg = hak_geterrmsg(worker->hak);
		}
		else
		{
			obj = hak_execute(worker->hak);
			if (!obj) failmsg = hak_geterrmsg(worker->hak);
			delete_exec_timer (proto);
		}
	}

#if 0
	if (hak_xproto_end_reply(proto, failmsg) <= -1)
	{
		HAK_LOG1 (worker->hak, SERVER_LOGMASK_ERROR, "Cannot finalize reply for %hs\n", trigger);
		return -1;
	}
#endif

	return 0;
}


static void send_error_message (hak_xproto_t* proto, const hak_ooch_t* errmsg)
{
#if 0
	hak_xproto_start_reply (proto);
	if (hak_xproto_end_reply(proto, errmsg) <= -1)
	{
		HAK_LOG1 (proto->hak, SERVER_LOGMASK_ERROR, "Unable to send error message - %s\n", errmsg);
	}
#endif
}

static void reformat_synerr (hak_t* hak)
{
	hak_synerr_t synerr;
	const hak_ooch_t* orgmsg;
	static hak_ooch_t nullstr[] = { '\0' };

	hak_getsynerr(hak, &synerr);

	orgmsg = hak_backuperrmsg(hak);
	hak_seterrbfmt (
		hak, HAK_ESYNERR,
		"%js at %js%hsline %zu column %zu",
		orgmsg,
		(synerr.loc.file? synerr.loc.file: nullstr),
		(synerr.loc.file? " ": ""),
		synerr.loc.line, synerr.loc.colm
	);
}

static void send_proto_hak_error (hak_xproto_t* proto)
{
	hak_server_worker_t* worker;
	worker = proto_to_worker(proto);
	if (HAK_ERRNUM(worker->hak) == HAK_ESYNERR) reformat_synerr (worker->hak);
	send_error_message (proto, hak_geterrmsg(worker->hak));
}

static void show_server_workers (hak_xproto_t* proto)
{
	hak_server_worker_t* worker, * w;
	hak_server_t* server;

	worker = proto_to_worker(proto);
	server = worker->server;

	pthread_mutex_lock (&server->worker_mutex);
	for (w = server->worker_list[HAK_SERVER_WORKER_STATE_ALIVE].head; w; w = w->next_worker)
	{
		/* TODO: implement this better... */
		hak_prbfmt (worker->hak, "%zu %d %d\n", w->wid, w->sck, 1000);
	}
	pthread_mutex_unlock (&server->worker_mutex);
}

static int kill_server_worker (hak_xproto_t* proto, hak_oow_t wid)
{
	hak_server_worker_t* worker;
	hak_server_t* server;
	int xret = 0;

	worker = proto_to_worker(proto);
	server = worker->server;

	pthread_mutex_lock(&server->worker_mutex);
	if (wid >= server->wid_map.capa)
	{
		hak_server_seterrnum(server, HAK_ENOENT);
		xret = -1;
	}
	else
	{
		hak_server_worker_t* worker;

		if (!server->wid_map.ptr[wid].used)
		{
			hak_server_seterrnum(server, HAK_ENOENT);
			xret = -1;
		}
		else
		{
			worker = server->wid_map.ptr[wid].u.worker;
			if (!worker)
			{
				hak_server_seterrnum(server, HAK_ENOENT);
				xret = -1;
			}
			else
			{
				if (worker->sck) shutdown(worker->sck, SHUT_RDWR);
				if (worker->hak) hak_abort(worker->hak);
			}
		}
	}
	pthread_mutex_unlock(&server->worker_mutex);
	return xret;
}

static int server_on_packet (hak_xproto_t* proto, hak_xpkt_type_t type, const void* data, hak_oow_t len)
{
	hak_server_worker_t* worker;
	hak_t* hak;

	worker = proto_to_worker(proto);
	hak = worker->hak;

printf ("HANDLE PACKET TYPE => %d\n", type);
	switch (type)
	{
		case HAK_XPKT_CODE:
printf ("FEEDING [%.*s]\n", (int)len, data);
			if (hak_feedbchars(hak, data, len) <= -1)
			{
				/* TODO: backup error message...and create a new message */
				goto oops;
			}
			break;

		case HAK_XPKT_EXECUTE:
		{
			hak_oop_t retv;
printf ("EXECUTING hak_executing......\n");

			hak_decode(hak, hak_getcode(hak), 0, hak_getbclen(hak));
			if (hak_feedpending(hak))
			{
				/* TODO: change the message */
				if (send_bytes(proto, HAK_XPKT_ERROR, "feed more",  9) <=-1)
				{
					/* TODO: error handling */
				}
				break;
			}

			retv = hak_execute(hak);
			if (HAK_UNLIKELY(!retv))
			{
				hak_bch_t errmsg[512];
				hak_oow_t errlen;

				/* TODO: backup error message...and create a new message */
				/* save error message before other calls override erro info */
				errlen = hak_copyerrbmsg(hak, errmsg, HAK_COUNTOF(errmsg));

				hak_flushudio(hak);
				hak_clearcode(hak);

				if (send_bytes(proto, HAK_XPKT_ERROR, errmsg, errlen) <= -1)
				{
					/* TODO: error handling */
				}

				goto oops;
			}
			else
			{
				hak_bch_t rvbuf[512]; /* TODO make this dynamic in side? */
				hak_oow_t rvlen;

				hak_flushudio(hak);
				hak_clearcode(hak);

				/* TODO or make hak_fmtXXXX  that accepts the output function */
				rvlen = hak_fmttobcstr(hak, rvbuf, HAK_COUNTOF(rvbuf), "[%O]", retv);
				if (send_bytes(proto, HAK_XPKT_RETVAL, rvbuf, rvlen) <= -1)
				{
				}
			}


			break;
		}

		case HAK_XPKT_STDIN:
			/* store ... push stdin pipe... */
			/*if (hak_feedstdin() <= -1) */
			break;

		case HAK_XPKT_LIST_WORKERS:
			break;

		case HAK_XPKT_KILL_WORKER:
			break;

		case HAK_XPKT_DISCONNECT:
			return 0; /* disconnect received */

		default:
			/* unknown packet type */
			/* TODO: proper error message */
			goto oops;
	}

	return 1;


oops:
	return -1;
}

static int send_bytes (hak_xproto_t* proto, hak_xpkt_type_t xpkt_type, const hak_bch_t* data, hak_oow_t len)
{
	hak_server_worker_t* worker;
	hak_xpkt_hdr_t hdr;
	struct iovec iov[2];
	const hak_bch_t* ptr, * cur, * end;
	hak_uint16_t seglen;

	worker = proto_to_worker(proto);

	ptr = cur = data;
	end = data + len;

/*printf ("SENDING BYTES [%.*s]\n", (int)len, data);*/
	do
	{
		int nv;

		while (cur != end && cur - ptr < HAK_XPKT_MAX_PLD_LEN) cur++;

		seglen = cur - ptr;

		hdr.id = 1; /* TODO: */
		hdr.type = xpkt_type | (((seglen >> 8) & 0x0F) << 4);
		hdr.len = seglen & 0xFF;

		nv = 0;
		iov[nv].iov_base = &hdr;
		iov[nv++].iov_len = HAK_SIZEOF(hdr);
		if (seglen > 0)
		{
			iov[nv].iov_base = ptr;
			iov[nv++].iov_len = seglen;
		}

		if (hak_sys_send_iov(worker->sck, iov, nv) <= -1)
		{
			/* TODO: error message */
fprintf (stderr, "Unable to sendmsg on %d - %s\n", worker->sck, strerror(errno));
			return -1;
		}

		ptr = cur;
	}
	while (ptr < end);

	return 0;
}

#if defined(HAK_OOCH_IS_UCH)
static int send_chars (hak_xproto_t* proto, hak_xpkt_type_t xpkt_type, const hak_ooch_t* data, hak_oow_t len)
{
	hak_server_worker_t* worker;
	const hak_ooch_t* ptr, * end;
	hak_bch_t tmp[256];
	hak_oow_t tln, pln;
	int n;

	worker = proto_to_worker(proto);

	ptr = data;
	end = data + len;

	while (ptr < end)
	{
		pln = end - ptr;
		tln = HAK_COUNTOF(tmp);
		n = hak_convutobchars(worker->hak, ptr, &pln, tmp, &tln);
		if (n <= -1 && n != -2) return -1;

		if (send_bytes(proto, xpkt_type, tmp, tln) <= -1) return -1;
		ptr += pln;
	}

	return 0;

}
#endif

/* ========================================================================= */

hak_server_t* hak_server_open (hak_mmgr_t* mmgr, hak_oow_t xtnsize, hak_server_prim_t* prim, hak_errinf_t* errinf)
{
	hak_server_t* server = HAK_NULL;
	hak_t* hak = HAK_NULL;
	hak_tmr_t* tmr = HAK_NULL;
	server_hak_xtn_t* xtn;
	int pfd[2], fcv;
	hak_bitmask_t trait;

	server = (hak_server_t*)HAK_MMGR_ALLOC(mmgr, HAK_SIZEOF(*server) + xtnsize);
	if (!server)
	{
	esysmem:
		if (errinf)
		{
			HAK_MEMSET(errinf, 0, HAK_SIZEOF(*errinf));
			errinf->num = HAK_ESYSMEM;
			hak_copy_oocstr(errinf->msg, HAK_COUNTOF(errinf->msg), hak_errnum_to_errstr(errinf->num));
		}
		goto oops;
	}

	hak = hak_openstdwithmmgr(mmgr, HAK_SIZEOF(*xtn), errinf);
	if (!hak) goto oops;

	/* replace the vmprim.log_write function */
	hak->vmprim.log_write = server_log_write_for_dummy;

	tmr = hak_tmr_open(hak, 0, 1024); /* TOOD: make the timer's default size configurable */
	if (!tmr) goto esysmem;

	if (hak_sys_open_pipes(pfd, 1) <= -1)
	{
		if (errinf)
		{
			hak_seterrbfmtwithsyserr(hak, 0, errno, HAK_NULL, 0);
			hak_geterrinf(hak, errinf);
		}
		goto oops;
	}

	xtn = (server_hak_xtn_t*)hak_getxtn(hak);
	xtn->server = server;

	HAK_MEMSET(server, 0, HAK_SIZEOF(*server) + xtnsize);
	server->_instsize = HAK_SIZEOF(*server);
	server->_mmgr = mmgr;
	server->_cmgr = hak_get_utf8_cmgr();
	server->prim = *prim;
	server->dummy_hak = hak;
	server->tmr = tmr;

	server->cfg.logmask = ~(hak_bitmask_t)0;
	server->cfg.worker_stack_size = 512000UL;
	server->cfg.actor_heap_size = 512000UL;

	HAK_INIT_NTIME (&server->cfg.worker_idle_timeout, 0, 0);
	HAK_INIT_NTIME (&server->cfg.actor_max_runtime, 0, 0);

	server->mux_pipe[0] = pfd[0];
	server->mux_pipe[1] = pfd[1];

	server->wid_map.free_first = HAK_SERVER_WID_INVALID;
	server->wid_map.free_last = HAK_SERVER_WID_INVALID;

	server->listener.ep_fd = -1;

	pthread_mutex_init (&server->worker_mutex, HAK_NULL);
	pthread_mutex_init (&server->tmr_mutex, HAK_NULL);
	pthread_mutex_init (&server->log_mutex, HAK_NULL);

	/* the dummy hak is used for this server to perform primitive operations
	 * such as getting system time or logging. so the heap size doesn't
	 * need to be changed from the tiny value set above. */
	hak_setoption (server->dummy_hak, HAK_LOG_MASK, &server->cfg.logmask);
	hak_setcmgr (server->dummy_hak, hak_server_getcmgr(server));
	hak_getoption (server->dummy_hak, HAK_TRAIT, &trait);
#if defined(HAK_BUILD_DEBUG)
	if (server->cfg.trait & HAK_SERVER_TRAIT_DEBUG_GC) trait |= HAK_TRAIT_DEBUG_GC;
	if (server->cfg.trait & HAK_SERVER_TRAIT_DEBUG_BIGINT) trait |= HAK_TRAIT_DEBUG_BIGINT;
#endif
	hak_setoption (server->dummy_hak, HAK_TRAIT, &trait);

	return server;

oops:
	/* NOTE: pipe should be closed if jump to here is made after pipe() above */
	if (tmr) hak_tmr_close (tmr);
	if (hak) hak_close(hak);
	if (server) HAK_MMGR_FREE(mmgr, server);
	return HAK_NULL;
}

void hak_server_close (hak_server_t* server)
{
	HAK_ASSERT(server->dummy_hak, server->listener.head == HAK_NULL);
	HAK_ASSERT(server->dummy_hak, server->listener.count == 0);
	HAK_ASSERT(server->dummy_hak, server->listener.ep_fd == -1);

	if (server->wid_map.ptr)
	{
		hak_server_freemem(server, server->wid_map.ptr);
		server->wid_map.capa = 0;
		server->wid_map.free_first = HAK_SERVER_WID_INVALID;
		server->wid_map.free_last = HAK_SERVER_WID_INVALID;
	}

	pthread_mutex_destroy (&server->log_mutex);
	pthread_mutex_destroy (&server->tmr_mutex);
	pthread_mutex_destroy (&server->worker_mutex);

	hak_sys_close_pipes (server->mux_pipe);

	hak_tmr_close (server->tmr);
	hak_close (server->dummy_hak);

	HAK_MMGR_FREE(server->_mmgr, server);
}

static HAK_INLINE int prepare_to_acquire_wid (hak_server_t* server)
{
	hak_oow_t new_capa;
	hak_oow_t i, j;
	hak_server_wid_map_data_t* tmp;

	HAK_ASSERT(server->dummy_hak, server->wid_map.free_first == HAK_SERVER_WID_INVALID);
	HAK_ASSERT(server->dummy_hak, server->wid_map.free_last == HAK_SERVER_WID_INVALID);

	new_capa = HAK_ALIGN_POW2(server->wid_map.capa + 1, HAK_SERVER_WID_MAP_ALIGN);
	if (new_capa > HAK_SERVER_WID_MAX)
	{
		if (server->wid_map.capa >= HAK_SERVER_WID_MAX)
		{
			hak_server_seterrnum(server, HAK_EFLOOD);
			return -1;
		}

		new_capa = HAK_SERVER_WID_MAX;
	}

	tmp = (hak_server_wid_map_data_t*)hak_server_reallocmem(server, server->wid_map.ptr, HAK_SIZEOF(*tmp) * new_capa);
	if (!tmp) return -1;

	server->wid_map.free_first = server->wid_map.capa;
	for (i = server->wid_map.capa, j = server->wid_map.capa + 1; j < new_capa; i++, j++)
	{
		tmp[i].used = 0;
		tmp[i].u.next = j;
	}
	tmp[i].used = 0;
	tmp[i].u.next = HAK_SERVER_WID_INVALID;
	server->wid_map.free_last = i;

	server->wid_map.ptr = tmp;
	server->wid_map.capa = new_capa;

	return 0;
}

static HAK_INLINE void acquire_wid (hak_server_t* server, hak_server_worker_t* worker)
{
	hak_oow_t wid;

	wid = server->wid_map.free_first;
	worker->wid = wid;

	server->wid_map.free_first = server->wid_map.ptr[wid].u.next;
	if (server->wid_map.free_first == HAK_SERVER_WID_INVALID) server->wid_map.free_last = HAK_SERVER_WID_INVALID;

	server->wid_map.ptr[wid].used = 1;
	server->wid_map.ptr[wid].u.worker = worker;
}

static HAK_INLINE void release_wid (hak_server_t* server, hak_server_worker_t* worker)
{
	hak_oow_t wid;

	wid = worker->wid;
	HAK_ASSERT(server->dummy_hak, wid < server->wid_map.capa && wid != HAK_SERVER_WID_INVALID);

	server->wid_map.ptr[wid].used = 0;
	server->wid_map.ptr[wid].u.next = HAK_SERVER_WID_INVALID;
	if (server->wid_map.free_last == HAK_SERVER_WID_INVALID)
	{
		HAK_ASSERT(server->dummy_hak, server->wid_map.free_first <= HAK_SERVER_WID_INVALID);
		server->wid_map.free_first = wid;
	}
	else
	{
		server->wid_map.ptr[server->wid_map.free_last].u.next = wid;
	}
	server->wid_map.free_last = wid;
	worker->wid = HAK_SERVER_WID_INVALID;
}

static hak_server_worker_t* alloc_worker (hak_server_t* server, int cli_sck, const hak_sckaddr_t* peeraddr)
{
	hak_server_worker_t* worker;

	worker = (hak_server_worker_t*)hak_server_allocmem(server, HAK_SIZEOF(*worker));
	if (!worker) return HAK_NULL;

	HAK_MEMSET(worker, 0, HAK_SIZEOF(*worker));
	worker->state = HAK_SERVER_WORKER_STATE_ZOMBIE;
	worker->opstate = HAK_SERVER_WORKER_OPSTATE_IDLE;
	worker->sck = cli_sck;
	worker->peeraddr = *peeraddr;
	worker->server = server;
	worker->exec_runtime_event_index = HAK_TMR_INVALID_INDEX;

	server->dummy_hak->vmprim.vm_gettime (server->dummy_hak, &worker->alloc_time); /* TODO: the callback may return monotonic time. find a way to guarantee it is realtime??? */

	if (server->wid_map.free_first == HAK_SERVER_WID_INVALID && prepare_to_acquire_wid(server) <= -1)
	{
		hak_server_freemem (server, worker);
		return HAK_NULL;
	}

	acquire_wid (server, worker);
	return worker;
}

static void fini_worker_socket (hak_server_worker_t* worker)
{
	if (worker->sck >= 0)
	{
		if (worker->hak)
		{
			HAK_LOG2 (worker->hak, SERVER_LOGMASK_INFO, "Closing worker socket %d [%zu]\n", worker->sck, worker->wid);
		}
		else
		{
			/* this should be in the main server thread. i use dummy_hak for logging */
			HAK_LOG2 (worker->server->dummy_hak, SERVER_LOGMASK_INFO, "Closing worker socket %d [%zu]\n", worker->sck, worker->wid);
		}
		close (worker->sck);
		worker->sck = -1;
	}
}

static void free_worker (hak_server_worker_t* worker)
{
	fini_worker_socket (worker);

	if (worker->hak)
	{
		HAK_LOG1 (worker->hak, SERVER_LOGMASK_INFO, "Killing worker [%zu]\n", worker->wid);
	}
	else
	{
		/* this should be in the main server thread. i use dummy_hak for logging */
		HAK_LOG1 (worker->server->dummy_hak, SERVER_LOGMASK_INFO, "Killing worker [%zu]\n", worker->wid);
	}

	release_wid (worker->server, worker);
	hak_server_freemem (worker->server, worker);
}

static void add_worker_to_server (hak_server_t* server, hak_server_worker_state_t wstate, hak_server_worker_t* worker)
{
	HAK_ASSERT(server->dummy_hak, worker->server == server);

	if (server->worker_list[wstate].tail)
	{
		server->worker_list[wstate].tail->next_worker = worker;
		worker->prev_worker = server->worker_list[wstate].tail;
		server->worker_list[wstate].tail = worker;
		worker->next_worker = HAK_NULL;
	}
	else
	{
		server->worker_list[wstate].tail = worker;
		server->worker_list[wstate].head = worker;
		worker->prev_worker = HAK_NULL;
		worker->next_worker = HAK_NULL;
	}

	server->worker_list[wstate].count++;
	worker->state = wstate;
}

static void zap_worker_in_server (hak_server_t* server, hak_server_worker_t* worker)
{
	hak_server_worker_state_t wstate;

	HAK_ASSERT(server->dummy_hak, worker->server == server);

	wstate = worker->state;
	if (worker->prev_worker) worker->prev_worker->next_worker = worker->next_worker;
	else server->worker_list[wstate].head = worker->next_worker;
	if (worker->next_worker) worker->next_worker->prev_worker = worker->prev_worker;
	else server->worker_list[wstate].tail = worker->prev_worker;

	HAK_ASSERT(server->dummy_hak, server->worker_list[wstate].count > 0);
	server->worker_list[wstate].count--;
	worker->state = HAK_SERVER_WORKER_STATE_ZOMBIE;
	worker->prev_worker = HAK_NULL;
	worker->next_worker = HAK_NULL;
}

static int worker_step (hak_server_worker_t* worker)
{
	hak_xproto_t* proto = worker->proto;
	hak_server_t* server = worker->server;
	hak_t* hak = worker->hak;
	struct pollfd pfd;
	int tmout, actual_tmout;
	ssize_t x;
	int n;

	//HAK_ASSERT(hak, proto->rcv.len < proto->rcv.len_needed);

	if (HAK_UNLIKELY(hak_xproto_geteof(proto)))
	{
// TODO: may not be an error if writable needs to be checked...
		hak_seterrbfmt(hak, HAK_EGENERIC, "connection closed");
		return -1;
	}

	tmout = HAK_SECNSEC_TO_MSEC(server->cfg.worker_idle_timeout.sec, server->cfg.worker_idle_timeout.nsec);
	actual_tmout = (tmout <= 0)? 10000: tmout;

	pfd.fd = worker->sck;
	pfd.events = 0;
	if (!hak_xproto_ready(proto)) pfd.events |= POLLIN;
	//if (proto->snd.len > 0) pfd.events |= POLLOUT;

	if (pfd.events)
	{
		n = poll(&pfd, 1, actual_tmout);
		if (n <= -1)
		{
			if (errno == EINTR) return 0;
			hak_seterrwithsyserr(hak, 0, errno);
			return -1;
		}
		else if (n == 0)
		{
			/* timed out - no activity on the pfd */
			if (tmout > 0)
			{
				/* timeout explicity set. no activity for that duration. considered idle */
				hak_seterrbfmt(hak, HAK_EGENERIC, "no activity on the worker socket %d", worker->sck);
				return -1;
			}

			goto carry_on;
		}

		if (pfd.revents & POLLERR)
		{
			hak_seterrbfmt(hak, HAK_EGENERIC, "error condition detected on workder socket %d", worker->sck);
			return -1;
		}

		if (pfd.revents & POLLOUT)
		{
		}

		if (pfd.revents & POLLIN)
		{
			hak_oow_t bcap;
			hak_uint8_t* bptr;

			bptr = hak_xproto_getbuf(proto, &bcap);
			x = recv(worker->sck, bptr, bcap, 0);
			if (x <= -1)
			{
				if (errno == EINTR) goto carry_on; /* didn't read read */
				hak_seterrwithsyserr(hak, 0, errno);
				return -1;
			}

			if (x == 0) hak_xproto_seteof(proto, 1);
			hak_xproto_advbuf (proto, x);
		}
	}

	/* the receiver buffer has enough data */
	while (hak_xproto_ready(worker->proto))
	{
		if ((n = hak_xproto_process(worker->proto)) <= -1)
		{
			/* TODO: proper error message */
			return -1;
		}
		if (n == 0)
		{
			/* TODO: chceck if there is remaining data in the buffer...?? */
			return 0; /* tell the caller to break the step loop */
		}
	}

carry_on:
	return 1; /* carry on */
}

static int init_worker_hak (hak_server_worker_t* worker)
{
	hak_server_t* server = worker->server;
	hak_t* hak;
	worker_hak_xtn_t* xtn;
	hak_bitmask_t trait;
	hak_cb_t hakcb;

	hak = hak_openstdwithmmgr(hak_server_getmmgr(server), HAK_SIZEOF(*xtn), HAK_NULL);
	if (HAK_UNLIKELY(!hak)) goto oops;

	/* replace the vmprim.log_write function */
	hak->vmprim.log_write = server_log_write;

	xtn = (worker_hak_xtn_t*)hak_getxtn(hak);
	xtn->worker = worker;

	hak_setoption(hak, HAK_MOD_INCTX, &server->cfg.module_inctx);
	hak_setoption(hak, HAK_LOG_MASK, &server->cfg.logmask);
	hak_setcmgr(hak, hak_server_getcmgr(server));

	hak_getoption(hak, HAK_TRAIT, &trait);
#if defined(HAK_BUILD_DEBUG)
	if (server->cfg.trait & HAK_SERVER_TRAIT_DEBUG_GC) trait |= HAK_TRAIT_DEBUG_GC;
	if (server->cfg.trait & HAK_SERVER_TRAIT_DEBUG_BIGINT) trait |= HAK_TRAIT_DEBUG_BIGINT;
#endif
	trait |= HAK_TRAIT_LANG_ENABLE_EOL;
	hak_setoption(hak, HAK_TRAIT, &trait);

	HAK_MEMSET(&hakcb, 0, HAK_SIZEOF(hakcb));
	/*hakcb.fini = fini_hak;
	hakcb.gc = gc_hak;*/
	hakcb.vm_startup =  vm_startup;
	hakcb.vm_cleanup = vm_cleanup;
	hakcb.vm_checkbc = vm_checkbc;
	hak_regcb(hak, &hakcb);

	if (hak_ignite(hak, server->cfg.actor_heap_size) <= -1) goto oops;
	if (hak_addbuiltinprims(hak) <= -1) goto oops;

	if (hak_attachccio(hak, read_handler) <= -1) goto oops;
	if (hak_attachudio(hak, scan_handler, print_handler) <= -1) goto oops;

	if (hak_beginfeed(hak, on_fed_cnode) <= -1) goto oops;

	worker->hak = hak;
	return 0;

oops:
	if (hak) hak_close(hak);
	return -1;
}


static void fini_worker_hak (hak_server_worker_t* worker)
{
	if (HAK_LIKELY(worker->hak))
	{
		hak_endfeed (worker->hak);
		hak_close (worker->hak);
		worker->hak = HAK_NULL;
	}
}


static int init_worker_proto (hak_server_worker_t* worker)
{
	hak_xproto_t* proto;
	proto_xtn_t* xtn;
	hak_xproto_cb_t cb;

	HAK_MEMSET(&cb, 0, HAK_SIZEOF(cb));
	cb.on_packet = server_on_packet;

	proto = hak_xproto_open(hak_server_getmmgr(worker->server), &cb, HAK_SIZEOF(*xtn));
	if (HAK_UNLIKELY(!proto)) return -1;

	xtn = hak_xproto_getxtn(proto);
	xtn->worker = worker;

	worker->proto = proto;
	return 0;
}

static void fini_worker_proto (hak_server_worker_t* worker)
{
	if (HAK_LIKELY(worker->proto))
	{
		hak_xproto_close (worker->proto);
		worker->proto = HAK_NULL;
	}
}

static void* worker_main (void* ctx)
{
	hak_server_worker_t* worker = (hak_server_worker_t*)ctx;
	hak_server_t* server = worker->server;
	sigset_t set;
	int n;

	sigfillset (&set);
	pthread_sigmask (SIG_BLOCK, &set, HAK_NULL);

	worker->thr = pthread_self();

	n = init_worker_hak(worker);
	if (HAK_UNLIKELY(n <= -1))
	{
		/* TODO: capture error ... */
		return HAK_NULL;
	}

	n = init_worker_proto(worker);
	if (HAK_UNLIKELY(n <= -1))
	{
		fini_worker_hak (worker);
		return HAK_NULL;
	}

	pthread_mutex_lock (&server->worker_mutex);
	add_worker_to_server (server, HAK_SERVER_WORKER_STATE_ALIVE, worker);
	pthread_mutex_unlock (&server->worker_mutex);

	/* the worker loop */
	while (!server->stopreq)
	{
		int n;
		worker->opstate = HAK_SERVER_WORKER_OPSTATE_WAIT;

		if ((n = worker_step(worker)) <= 0)
		{
			worker->opstate = (n <= -1)? HAK_SERVER_WORKER_OPSTATE_ERROR: HAK_SERVER_WORKER_OPSTATE_IDLE;
			break;
		}
	}

	hak_xproto_close (worker->proto);
	worker->proto = HAK_NULL;

	fini_worker_hak (worker);

	pthread_mutex_lock (&server->worker_mutex);
	fini_worker_socket (worker);
	if (!worker->claimed)
	{
		zap_worker_in_server (server, worker);
		add_worker_to_server (server, HAK_SERVER_WORKER_STATE_DEAD, worker);
	}
	pthread_mutex_unlock (&server->worker_mutex);

	return HAK_NULL;
}

static void purge_all_workers (hak_server_t* server, hak_server_worker_state_t wstate)
{
	hak_server_worker_t* worker;

	while (1)
	{
		pthread_mutex_lock (&server->worker_mutex);
		worker = server->worker_list[wstate].head;
		if (worker)
		{
			zap_worker_in_server (server, worker);
			worker->claimed = 1;
			if (worker->sck >= 0) shutdown (worker->sck, SHUT_RDWR);
		}
		pthread_mutex_unlock (&server->worker_mutex);
		if (!worker) break;

		pthread_join (worker->thr, HAK_NULL);
		free_worker (worker);
	}
}

void hak_server_logbfmt (hak_server_t* server, hak_bitmask_t mask, const hak_bch_t* fmt, ...)
{
	va_list ap;
	va_start (ap, fmt);
	hak_logbfmtv (server->dummy_hak, mask, fmt, ap);
	va_end (ap);
}

void hak_server_logufmt (hak_server_t* server, hak_bitmask_t mask, const hak_uch_t* fmt, ...)
{
	va_list ap;
	va_start (ap, fmt);
	hak_logufmtv (server->dummy_hak, mask, fmt, ap);
	va_end (ap);
}

static void set_err_with_syserr (hak_server_t* server, int syserr_type, int syserr_code, const char* bfmt, ...)
{
	hak_t* hak = server->dummy_hak;
	hak_errnum_t errnum;
	hak_oow_t tmplen, tmplen2;
	va_list ap;

	static hak_bch_t b_dash[] = { ' ', '-', ' ', '\0' };
	static hak_uch_t u_dash[] = { ' ', '-', ' ', '\0' };

	if (hak->shuterr) return;

	if (hak->vmprim.syserrstrb)
	{
		errnum = hak->vmprim.syserrstrb(hak, syserr_type, syserr_code, hak->errmsg.tmpbuf.bch, HAK_COUNTOF(hak->errmsg.tmpbuf.bch));

		va_start (ap, bfmt);
		hak_seterrbfmtv(hak, errnum, bfmt, ap);
		va_end (ap);

	#if defined(HAK_OOCH_IS_UCH)
		hak->errmsg.len += hak_copy_ucstr(&hak->errmsg.buf[hak->errmsg.len], HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len, u_dash);
		tmplen2 = HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len;
		hak_convbtoucstr(hak, hak->errmsg.tmpbuf.bch, &tmplen, &hak->errmsg.buf[hak->errmsg.len], &tmplen2);
		hak->errmsg.len += tmplen2; /* ignore conversion errors */
	#else
		hak->errmsg.len += hak_copy_bcstr(&hak->errmsg.buf[hak->errmsg.len], HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len, b_dash);
		hak->errmsg.len += hak_copy_bcstr(&hak->errmsg.buf[hak->errmsg.len], HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len, hak->errmsg.tmpbuf.bch);

	#endif
	}
	else
	{
		HAK_ASSERT(hak, hak->vmprim.syserrstru != HAK_NULL);

		errnum = hak->vmprim.syserrstru(hak, syserr_type, syserr_code, hak->errmsg.tmpbuf.uch, HAK_COUNTOF(hak->errmsg.tmpbuf.uch));

		va_start (ap, bfmt);
		hak_seterrbfmtv(hak, errnum, bfmt, ap);
		va_end (ap);

	#if defined(HAK_OOCH_IS_UCH)
		hak->errmsg.len += hak_copy_ucstr(&hak->errmsg.buf[hak->errmsg.len], HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len, u_dash);
		hak->errmsg.len += hak_copy_ucstr(&hak->errmsg.buf[hak->errmsg.len], HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len, hak->errmsg.tmpbuf.uch);
	#else
		hak->errmsg.len += hak_copy_bcstr(&hak->errmsg.buf[hak->errmsg.len], HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len, b_dash);
		tmplen2 = HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len;
		hak_convutobcstr(hak, hak->errmsg.tmpbuf.uch, &tmplen, &hak->errmsg.buf[hak->errmsg.len], &tmplen2);
		hak->errmsg.len += tmplen2; /* ignore conversion errors */
	#endif
	}

	server->errnum = errnum;
	hak_copy_oochars (server->errmsg.buf, server->dummy_hak->errmsg.buf, HAK_COUNTOF(server->errmsg.buf));
	server->errmsg.len = server->dummy_hak->errmsg.len;
}

static void free_all_listeners (hak_server_t* server)
{
	hak_server_listener_t* lp;
#if defined(USE_EPOLL)
	struct epoll_event dummy_ev;

	epoll_ctl (server->listener.ep_fd, EPOLL_CTL_DEL, server->mux_pipe[0], &dummy_ev);
#endif

	while (server->listener.head)
	{
		lp = server->listener.head;
		server->listener.head = lp->next_listener;
		server->listener.count--;

#if defined(USE_EPOLL)
		epoll_ctl (server->listener.ep_fd, EPOLL_CTL_DEL, lp->sck, &dummy_ev);
#endif
		close (lp->sck);
		hak_server_freemem (server, lp);
	}

#if defined(USE_EPOLL)
	HAK_ASSERT(server->dummy_hak, server->listener.ep_fd >= 0);
	close (server->listener.ep_fd);
	server->listener.ep_fd = -1;
#endif
}

static int setup_listeners (hak_server_t* server, const hak_bch_t* addrs)
{
	const hak_bch_t* addr_ptr, * comma;
	int ep_fd, fcv;
#if defined(USE_EPOLL)
	struct epoll_event ev;

	ep_fd = epoll_create(1024);
	if (ep_fd <= -1)
	{
		set_err_with_syserr (server, 0, errno, "unable to create multiplexer");
		HAK_LOG1 (server->dummy_hak, SERVER_LOGMASK_ERROR, "%js\n", hak_server_geterrmsg(server));
		return -1;
	}

	hak_sys_set_cloexec(ep_fd, 1);

	HAK_MEMSET(&ev, 0, HAK_SIZEOF(ev));
	ev.events = EPOLLIN | EPOLLHUP | EPOLLERR;
	ev.data.fd = server->mux_pipe[0];
	if (epoll_ctl(ep_fd, EPOLL_CTL_ADD, server->mux_pipe[0], &ev) <= -1)
	{
		set_err_with_syserr (server, 0, errno, "unable to register pipe %d to multiplexer", server->mux_pipe[0]);
		HAK_LOG1 (server->dummy_hak, SERVER_LOGMASK_ERROR, "%js\n", hak_server_geterrmsg(server));
		close (ep_fd);
		return -1;
	}

	server->listener.ep_fd = ep_fd;
#endif
	addr_ptr = addrs;
	while (1)
	{
		hak_sckaddr_t srv_addr;
		int srv_fd, sck_fam, optval;
		hak_scklen_t srv_len;
		hak_oow_t addr_len;
		hak_server_listener_t* listener;

		comma = hak_find_bchar_in_bcstr(addr_ptr, ',');
		addr_len = comma? comma - addr_ptr: hak_count_bcstr(addr_ptr);
		/* [NOTE] no whitespaces are allowed before and after a comma */

		sck_fam = hak_bchars_to_sckaddr(addr_ptr, addr_len, &srv_addr, &srv_len);
		if (sck_fam <= -1)
		{
			hak_server_seterrbfmt (server, HAK_EINVAL, "unable to convert address - %.*hs", addr_len, addr_ptr);
			HAK_LOG1 (server->dummy_hak, SERVER_LOGMASK_ERROR, "%js\n", hak_server_geterrmsg(server));
			goto next_segment;
		}

		srv_fd = socket(sck_fam, SOCK_STREAM, 0);
		if (srv_fd <= -1)
		{
			set_err_with_syserr (server, 0, errno, "unable to open server socket for %.*hs", addr_len, addr_ptr);
			HAK_LOG1 (server->dummy_hak, SERVER_LOGMASK_ERROR, "%js\n", hak_server_geterrmsg(server));
			goto next_segment;
		}

		optval = 1;
		setsockopt (srv_fd, SOL_SOCKET, SO_REUSEADDR, &optval, HAK_SIZEOF(int));
		hak_sys_set_nonblock (srv_fd, 1); /* the listening socket is non-blocking unlike accepted sockets */
		hak_sys_set_cloexec (srv_fd, 1);

		if (bind(srv_fd, (struct sockaddr*)&srv_addr, srv_len) == -1)
		{
			set_err_with_syserr (server, 0, errno, "unable to bind server socket %d for %.*hs", srv_fd, addr_len, addr_ptr);
			HAK_LOG1 (server->dummy_hak, SERVER_LOGMASK_ERROR, "%js\n", hak_server_geterrmsg(server));
			close (srv_fd);
			goto next_segment;
		}

		if (listen(srv_fd, 128) <= -1)
		{
			set_err_with_syserr (server, 0, errno, "unable to listen on server socket %d for %.*hs", srv_fd, addr_len, addr_ptr);
			HAK_LOG1 (server->dummy_hak, SERVER_LOGMASK_ERROR, "%js\n", hak_server_geterrmsg(server));
			close (srv_fd);
			goto next_segment;
		}


#if defined(USE_EPOLL)
		HAK_MEMSET(&ev, 0, HAK_SIZEOF(ev));
		ev.events = EPOLLIN | EPOLLHUP | EPOLLERR;
		ev.data.fd = srv_fd;
		if (epoll_ctl(ep_fd, EPOLL_CTL_ADD, srv_fd, &ev) <= -1)
		{
			set_err_with_syserr (server, 0, errno, "unable to register server socket %d to multiplexer for %.*hs", srv_fd, addr_len, addr_ptr);
			HAK_LOG1 (server->dummy_hak, SERVER_LOGMASK_ERROR, "%js\n", hak_server_geterrmsg(server));
			close (srv_fd);
			goto next_segment;
		}
#endif

		listener = (hak_server_listener_t*)hak_server_allocmem(server, HAK_SIZEOF(*listener));
		if (!listener)
		{
			close(srv_fd);
			goto next_segment;
		}

		HAK_MEMSET(listener, 0, HAK_SIZEOF(*listener));
		listener->sck = srv_fd;
		listener->sckaddr = srv_addr;
		listener->next_listener = server->listener.head;
		server->listener.head = listener;
		server->listener.count++;

	next_segment:
		if (!comma) break;
		addr_ptr = comma + 1;
	}


	if (!server->listener.head)
	{
		/* no valid server has been configured */
		hak_server_seterrbfmt (server, HAK_EINVAL, "unable to set up listeners with %hs", addrs);
		free_all_listeners (server);
		return -1;
	}

	return 0;
}

int hak_server_start (hak_server_t* server, const hak_bch_t* addrs)
{
	int xret = 0, fcv;
	pthread_attr_t thr_attr;

	if (setup_listeners(server, addrs) <= -1) return -1;

	pthread_attr_init (&thr_attr);
	pthread_attr_setstacksize (&thr_attr, server->cfg.worker_stack_size);

	server->stopreq = 0;
	while (!server->stopreq)
	{
		hak_sckaddr_t cli_addr;
		hak_scklen_t cli_len;
		int cli_fd;
		pthread_t thr;
		hak_ntime_t tmout;
		hak_server_worker_t* worker;
		int n;

		pthread_mutex_lock (&server->tmr_mutex);
		n = hak_tmr_gettmout(server->tmr,  HAK_NULL, &tmout);
		pthread_mutex_unlock (&server->tmr_mutex);
		if (n <= -1) HAK_INIT_NTIME (&tmout, 10, 0);

#if defined(USE_EPOLL)
		n = epoll_wait(server->listener.ep_fd, server->listener.ev_buf, HAK_COUNTOF(server->listener.ev_buf), HAK_SECNSEC_TO_MSEC(tmout.sec, tmout.nsec));
#else
		n = poll(); /* TODO: */
#endif

		purge_all_workers (server, HAK_SERVER_WORKER_STATE_DEAD);
		if (n <= -1)
		{
			if (server->stopreq) break; /* normal termination requested */
			if (errno == EINTR) continue; /* interrupted but not termination requested */

			set_err_with_syserr (server, 0, errno, "unable to poll for events in server");
			xret = -1;
			break;
		}

		pthread_mutex_lock (&server->tmr_mutex);
		hak_tmr_fire (server->tmr, HAK_NULL, HAK_NULL);
		pthread_mutex_unlock (&server->tmr_mutex);

		while (n > 0)
		{
#if defined(USE_EPOLL)
			struct epoll_event* evp;
#endif

			--n;

#if defined(USE_EPOLL)
			evp = &server->listener.ev_buf[n];
			if (!evp->events /*& (POLLIN | POLLHUP | POLLERR) */) continue;
#else

			/* TODO: */
#endif

			if (evp->data.fd == server->mux_pipe[0])
			{
				char tmp[128];
				while (read(server->mux_pipe[0], tmp, HAK_SIZEOF(tmp)) > 0) /* nothing */;
			}
			else
			{
				/* the reset should be the listener's socket */

				cli_len = HAK_SIZEOF(cli_addr);
				cli_fd = accept(evp->data.fd, (struct sockaddr*)&cli_addr, &cli_len);
				if (cli_fd == -1)
				{
					if (server->stopreq) break; /* normal termination requested */
					if (errno == EINTR) continue; /* interrupted but no termination requested */
					if (hak_sys_is_errno_wb(errno)) continue;
					set_err_with_syserr (server, 0, errno, "unable to accept worker on server socket %d", evp->data.fd);
					xret = -1;
					break;
				}

				hak_sys_set_nonblock (cli_fd, 0); /* force the accepted socket to be blocking */
				hak_sys_set_cloexec (cli_fd, 1);

				if (server->cfg.worker_max_count > 0)
				{
					int flood;
					pthread_mutex_lock (&server->worker_mutex);
					flood = (server->worker_list[HAK_SERVER_WORKER_STATE_ALIVE].count >= server->cfg.worker_max_count);
					pthread_mutex_unlock (&server->worker_mutex);
					if (flood)
					{
						HAK_LOG1 (server->dummy_hak, SERVER_LOGMASK_ERROR, "Not accepting connection for too many workers - socket %d\n", cli_fd);
						goto drop_connection;
					}
				}

				worker = alloc_worker(server, cli_fd, &cli_addr);
				if (!worker)
				{
					HAK_LOG1 (server->dummy_hak, SERVER_LOGMASK_ERROR, "Unable to accomodate worker - socket %d\n", cli_fd);
				drop_connection:
					close (cli_fd);
				}
				else
				{
					HAK_LOG2 (server->dummy_hak, SERVER_LOGMASK_INFO, "Accomodated worker [%zu] - socket %d\n", worker->wid, cli_fd);
					if (pthread_create(&thr, &thr_attr, worker_main, worker) != 0)
					{
						free_worker (worker);
					}
				}
			}
		}
	}

	purge_all_workers (server, HAK_SERVER_WORKER_STATE_ALIVE);
	purge_all_workers (server, HAK_SERVER_WORKER_STATE_DEAD);

	pthread_attr_destroy (&thr_attr);

	free_all_listeners (server);
	return xret;
}

void hak_server_stop (hak_server_t* server)
{
	server->stopreq = 1;
	write (server->mux_pipe[1], "Q", 1); /* don't care about failure */
}

int hak_server_setoption (hak_server_t* server, hak_server_option_t id, const void* value)
{
	switch (id)
	{
		case HAK_SERVER_TRAIT:
			server->cfg.trait = *(const hak_bitmask_t*)value;
			if (server->dummy_hak)
			{
				/* setting this affects the dummy hak immediately.
				 * existing hak instances inside worker threads won't get
				 * affected. new hak instances to be created later
				 * is supposed to use the new value */
				hak_bitmask_t trait;

				hak_getoption (server->dummy_hak, HAK_TRAIT, &trait);
			#if defined(HAK_BUILD_DEBUG)
				if (server->cfg.trait & HAK_SERVER_TRAIT_DEBUG_GC) trait |= HAK_TRAIT_DEBUG_GC;
				if (server->cfg.trait & HAK_SERVER_TRAIT_DEBUG_BIGINT) trait |= HAK_TRAIT_DEBUG_BIGINT;
			#endif
				hak_setoption (server->dummy_hak, HAK_TRAIT, &trait);
			}
			return 0;

		case HAK_SERVER_LOG_MASK:
			server->cfg.logmask = *(const hak_bitmask_t*)value;
			if (server->dummy_hak)
			{
				/* setting this affects the dummy hak immediately.
				 * existing hak instances inside worker threads won't get
				 * affected. new hak instances to be created later
				 * is supposed to use the new value */
				hak_setoption (server->dummy_hak, HAK_LOG_MASK, value);
			}
			return 0;

		case HAK_SERVER_WORKER_MAX_COUNT:
			server->cfg.worker_max_count = *(hak_oow_t*)value;
			return 0;

		case HAK_SERVER_WORKER_STACK_SIZE:
			server->cfg.worker_stack_size = *(hak_oow_t*)value;
			return 0;

		case HAK_SERVER_WORKER_IDLE_TIMEOUT:
			server->cfg.worker_idle_timeout = *(hak_ntime_t*)value;
			return 0;

		case HAK_SERVER_ACTOR_HEAP_SIZE:
			server->cfg.actor_heap_size = *(hak_oow_t*)value;
			return 0;

		case HAK_SERVER_ACTOR_MAX_RUNTIME:
			server->cfg.actor_max_runtime = *(hak_ntime_t*)value;
			return 0;

		case HAK_SERVER_SCRIPT_INCLUDE_PATH:
			hak_copy_oocstr (server->cfg.script_include_path, HAK_COUNTOF(server->cfg.script_include_path), (const hak_ooch_t*)value);
			return 0;

		case HAK_SERVER_MODULE_INCTX:
			server->cfg.module_inctx = *(void**)value;
			return 0;
	}

	hak_server_seterrnum(server, HAK_EINVAL);
	return -1;
}

int hak_server_getoption (hak_server_t* server, hak_server_option_t id, void* value)
{
	switch (id)
	{
		case HAK_SERVER_TRAIT:
			*(hak_bitmask_t*)value = server->cfg.trait;
			return 0;

		case HAK_SERVER_LOG_MASK:
			*(hak_bitmask_t*)value = server->cfg.logmask;
			return 0;

		case HAK_SERVER_WORKER_MAX_COUNT:
			*(hak_oow_t*)value = server->cfg.worker_max_count;
			return 0;

		case HAK_SERVER_WORKER_STACK_SIZE:
			*(hak_oow_t*)value = server->cfg.worker_stack_size;
			return 0;

		case HAK_SERVER_WORKER_IDLE_TIMEOUT:
			*(hak_ntime_t*)value = server->cfg.worker_idle_timeout;
			return 0;

		case HAK_SERVER_ACTOR_HEAP_SIZE:
			*(hak_oow_t*)value = server->cfg.actor_heap_size;
			return 0;

		case HAK_SERVER_ACTOR_MAX_RUNTIME:
			*(hak_ntime_t*)value = server->cfg.actor_max_runtime;
			return 0;

		case HAK_SERVER_SCRIPT_INCLUDE_PATH:
			*(hak_ooch_t**)value = server->cfg.script_include_path;
			return 0;

		case HAK_SERVER_MODULE_INCTX:
			*(void**)value = server->cfg.module_inctx;
			return 0;
	};

	hak_server_seterrnum(server, HAK_EINVAL);
	return -1;
}

void* hak_server_getxtn (hak_server_t* server)
{
	return (void*)((hak_uint8_t*)server + server->_instsize);
}

hak_mmgr_t* hak_server_getmmgr (hak_server_t* server)
{
	return server->_mmgr;
}

hak_cmgr_t* hak_server_getcmgr (hak_server_t* server)
{
	return server->_cmgr;
}

void hak_server_setcmgr (hak_server_t* server, hak_cmgr_t* cmgr)
{
	server->_cmgr = cmgr;
}

hak_errnum_t hak_server_geterrnum (hak_server_t* server)
{
	return server->errnum;
}

const hak_ooch_t* hak_server_geterrstr (hak_server_t* server)
{
	return hak_errnum_to_errstr(server->errnum);
}

const hak_ooch_t* hak_server_geterrmsg (hak_server_t* server)
{
	if (server->errmsg.len <= 0) return hak_errnum_to_errstr(server->errnum);
	return server->errmsg.buf;
}

void hak_server_seterrnum(hak_server_t* server, hak_errnum_t errnum)
{
	/*if (server->shuterr) return; */
	server->errnum = errnum;
	server->errmsg.len = 0;
}

void hak_server_seterrbfmt (hak_server_t* server, hak_errnum_t errnum, const hak_bch_t* fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	hak_seterrbfmtv (server->dummy_hak, errnum, fmt, ap);
	va_end (ap);

	HAK_ASSERT(server->dummy_hak, HAK_COUNTOF(server->errmsg.buf) == HAK_COUNTOF(server->dummy_hak->errmsg.buf));
	server->errnum = errnum;
	hak_copy_oochars (server->errmsg.buf, server->dummy_hak->errmsg.buf, HAK_COUNTOF(server->errmsg.buf));
	server->errmsg.len = server->dummy_hak->errmsg.len;
}

void hak_server_seterrufmt (hak_server_t* server, hak_errnum_t errnum, const hak_uch_t* fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	hak_seterrufmtv (server->dummy_hak, errnum, fmt, ap);
	va_end (ap);

	HAK_ASSERT(server->dummy_hak, HAK_COUNTOF(server->errmsg.buf) == HAK_COUNTOF(server->dummy_hak->errmsg.buf));
	server->errnum = errnum;
	server->errnum = errnum;
	hak_copy_oochars (server->errmsg.buf, server->dummy_hak->errmsg.buf, HAK_COUNTOF(server->errmsg.buf));
	server->errmsg.len = server->dummy_hak->errmsg.len;
}

void* hak_server_allocmem (hak_server_t* server, hak_oow_t size)
{
	void* ptr;

	ptr = HAK_MMGR_ALLOC(server->_mmgr, size);
	if (!ptr) hak_server_seterrnum(server, HAK_ESYSMEM);
	return ptr;
}

void* hak_server_callocmem (hak_server_t* server, hak_oow_t size)
{
	void* ptr;

	ptr = HAK_MMGR_ALLOC(server->_mmgr, size);
	if (!ptr) hak_server_seterrnum(server, HAK_ESYSMEM);
	else HAK_MEMSET(ptr, 0, size);
	return ptr;
}

void* hak_server_reallocmem (hak_server_t* server, void* ptr, hak_oow_t size)
{
	ptr = HAK_MMGR_REALLOC(server->_mmgr, ptr, size);
	if (!ptr) hak_server_seterrnum(server, HAK_ESYSMEM);
	return ptr;
}

void hak_server_freemem (hak_server_t* server, void* ptr)
{
	HAK_MMGR_FREE(server->_mmgr, ptr);
}
