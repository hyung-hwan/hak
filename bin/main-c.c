/*
 * $Id$
 *
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

#include "hcl-x.h"
#include "hcl-opt.h"
#include "hcl-utl.h"
#include "hcl-xutl.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <locale.h>

#if defined(HAVE_TIME_H)
#	include <time.h>
#endif
#if defined(HAVE_SYS_TIME_H)
#	include <sys/time.h>
#endif
#if defined(HAVE_SIGNAL_H)
#	include <signal.h>
#endif
#if defined(HAVE_SYS_UIO_H)
#	include <sys/uio.h>
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>

/* ========================================================================= */

typedef struct client_xtn_t client_xtn_t;
struct client_xtn_t
{
	int logfd;
	hcl_bitmask_t logmask;
	int logfd_istty;

	struct
	{
		hcl_bch_t buf[4096];
		hcl_oow_t len;
	} logbuf;
};

/* ========================================================================= */

static void* sys_alloc (hcl_mmgr_t* mmgr, hcl_oow_t size)
{
	return malloc(size);
}

static void* sys_realloc (hcl_mmgr_t* mmgr, void* ptr, hcl_oow_t size)
{
	return realloc(ptr, size);
}

static void sys_free (hcl_mmgr_t* mmgr, void* ptr)
{
	free (ptr);
}

static hcl_mmgr_t sys_mmgr =
{
	sys_alloc,
	sys_realloc,
	sys_free,
	HCL_NULL
};
/* ========================================================================= */

static int write_all (int fd, const hcl_bch_t* ptr, hcl_oow_t len)
{
	while (len > 0)
	{
		hcl_ooi_t wr;

		wr = write(fd, ptr, len);

		if (wr <= -1)
		{
		#if defined(EAGAIN) && defined(EWOULDBLOCK) && (EAGAIN == EWOULDBLOCK)
			if (errno == EAGAIN) continue;
		#else
			#if defined(EAGAIN)
			if (errno == EAGAIN) continue;
			#elif defined(EWOULDBLOCK)
			if (errno == EWOULDBLOCK) continue;
			#endif
		#endif

		#if defined(EINTR)
			/* TODO: would this interfere with non-blocking nature of this VM? */
			if (errno == EINTR) continue;
		#endif
			return -1;
		}

		ptr += wr;
		len -= wr;
	}

	return 0;
}


static int write_log (hcl_client_t* client, int fd, const hcl_bch_t* ptr, hcl_oow_t len)
{
	client_xtn_t* xtn;

	xtn = hcl_client_getxtn(client);

	while (len > 0)
	{
		if (xtn->logbuf.len > 0)
		{
			hcl_oow_t rcapa, cplen;

			rcapa = HCL_COUNTOF(xtn->logbuf.buf) - xtn->logbuf.len;
			cplen = (len >= rcapa)? rcapa: len;

			memcpy (&xtn->logbuf.buf[xtn->logbuf.len], ptr, cplen);
			xtn->logbuf.len += cplen;
			ptr += cplen;
			len -= cplen;

			if (xtn->logbuf.len >= HCL_COUNTOF(xtn->logbuf.buf))
			{
				write_all(fd, xtn->logbuf.buf, xtn->logbuf.len);
				xtn->logbuf.len = 0;
			}
		}
		else
		{
			hcl_oow_t rcapa;

			rcapa = HCL_COUNTOF(xtn->logbuf.buf);
			if (len >= rcapa)
			{
				write_all (fd, ptr, rcapa);
				ptr += rcapa;
				len -= rcapa;
			}
			else
			{
				memcpy (xtn->logbuf.buf, ptr, len);
				xtn->logbuf.len += len;
				ptr += len;
				len -= len;

			}
		}
	}

	return 0;
}

static void flush_log (hcl_client_t* client, int fd)
{
	client_xtn_t* xtn;
	xtn = hcl_client_getxtn(client);
	if (xtn->logbuf.len > 0)
	{
		write_all (fd, xtn->logbuf.buf, xtn->logbuf.len);
		xtn->logbuf.len = 0;
	}
}

static void log_write (hcl_client_t* client, hcl_bitmask_t mask, const hcl_ooch_t* msg, hcl_oow_t len)
{
	hcl_bch_t buf[256];
	hcl_oow_t ucslen, bcslen;
	client_xtn_t* xtn;
	hcl_oow_t msgidx;
	int n, logfd;

	xtn = hcl_client_getxtn(client);

	if (mask & HCL_LOG_STDERR)
	{
		/* the messages that go to STDERR don't get masked out */
		logfd = 2;
	}
	else
	{
		if (!(xtn->logmask & mask & ~HCL_LOG_ALL_LEVELS)) return;  /* check log types */
		if (!(xtn->logmask & mask & ~HCL_LOG_ALL_TYPES)) return;  /* check log levels */

		if (mask & HCL_LOG_STDOUT) logfd = 1;
		else
		{
			logfd = xtn->logfd;
			if (logfd <= -1) return;
		}
	}

/* TODO: beautify the log message.
 *       do classification based on mask. */
	if (!(mask & (HCL_LOG_STDOUT | HCL_LOG_STDERR)))
	{
		time_t now;
		char ts[32];
		size_t tslen;
		struct tm tm, *tmp;

		now = time(NULL);

	#if defined(__OS2__)
		tmp = _localtime(&now, &tm);
	#elif defined(HAVE_LOCALTIME_R)
		tmp = localtime_r(&now, &tm);
	#else
		tmp = localtime(&now);
	#endif

	#if defined(HAVE_STRFTIME_SMALL_Z)
		tslen = strftime(ts, sizeof(ts), "%Y-%m-%d %H:%M:%S %z ", tmp);
	#else
		tslen = strftime(ts, sizeof(ts), "%Y-%m-%d %H:%M:%S %Z ", tmp);
	#endif
		if (tslen == 0)
		{
			strcpy (ts, "0000-00-00 00:00:00 +0000");
			tslen = 25;
		}

		write_log (client, logfd, ts, tslen);
	}

	if (logfd == xtn->logfd && xtn->logfd_istty)
	{
		if (mask & HCL_LOG_FATAL) write_log (client, logfd, "\x1B[1;31m", 7);
		else if (mask & HCL_LOG_ERROR) write_log (client, logfd, "\x1B[1;32m", 7);
		else if (mask & HCL_LOG_WARN) write_log (client, logfd, "\x1B[1;33m", 7);
	}

#if defined(HCL_OOCH_IS_UCH)
	msgidx = 0;
	while (len > 0)
	{
		ucslen = len;
		bcslen = HCL_COUNTOF(buf);

		n = hcl_conv_oochars_to_bchars_with_cmgr(&msg[msgidx], &ucslen, buf, &bcslen, hcl_get_utf8_cmgr());
		if (n == 0 || n == -2)
		{
			/* n = 0:
			 *   converted all successfully
			 * n == -2:
			 *    buffer not sufficient. not all got converted yet.
			 *    write what have been converted this round. */

			/*HCL_ASSERT (hcl, ucslen > 0); */ /* if this fails, the buffer size must be increased */
			/*assert (ucslen > 0);*/

			/* attempt to write all converted characters */
			if (write_log(client, logfd, buf, bcslen) <= -1) break;

			if (n == 0) break;
			else
			{
				msgidx += ucslen;
				len -= ucslen;
			}
		}
		else if (n <= -1)
		{
			/* conversion error */
			if (bcslen <= 0) break;
			if (write_log(client, logfd, buf, bcslen) <= -1) break;
			msgidx += ucslen;
			len -= ucslen;
		}
	}
#else
	write_log (client, logfd, msg, len);
#endif

	if (logfd == xtn->logfd && xtn->logfd_istty)
	{
		if (mask & (HCL_LOG_FATAL | HCL_LOG_ERROR | HCL_LOG_WARN)) write_log (client, logfd, "\x1B[0m", 4);
	}

	flush_log (client, logfd);
}

/* ========================================================================= */

static hcl_client_t* g_client = HCL_NULL;

/* ========================================================================= */

typedef void (*signal_handler_t) (int, siginfo_t*, void*);

static void handle_sigint (int sig, siginfo_t* siginfo, void* ctx)
{
	/*if (g_client) hcl_client_stop (g_client);*/
}

static void set_signal (int sig, signal_handler_t handler)
{
	struct sigaction sa;

	memset (&sa, 0, sizeof(sa));
	/*sa.sa_handler = handler;*/
	sa.sa_flags = SA_SIGINFO;
	sa.sa_sigaction = handler;
	sigemptyset (&sa.sa_mask);

	sigaction (sig, &sa, NULL);
}

static void set_signal_to_ignore (int sig)
{
	struct sigaction sa;

	memset (&sa, 0, sizeof(sa));
	sa.sa_handler = SIG_IGN;
	sa.sa_flags = 0;
	sigemptyset (&sa.sa_mask);

	sigaction (sig, &sa, NULL);
}

static void set_signal_to_default (int sig)
{
	struct sigaction sa;

	memset (&sa, 0, sizeof(sa));
	sa.sa_handler = SIG_DFL;
	sa.sa_flags = 0;
	sigemptyset (&sa.sa_mask);

	sigaction (sig, &sa, NULL);
}

/* ========================================================================= */

static int handle_logopt (hcl_client_t* client, const hcl_bch_t* str)
{
	hcl_bch_t* xstr = (hcl_bch_t*)str;
	hcl_bch_t* cm, * flt;
	hcl_bitmask_t logmask;
	client_xtn_t* xtn;

	xtn = (client_xtn_t*)hcl_client_getxtn(client);

	cm = hcl_find_bchar_in_bcstr(xstr, ',');
	if (cm)
	{
		/* i duplicate this string for open() below as open() doesn't
		 * accept a length-bounded string */
		xstr = strdup(str);
		if (!xstr)
		{
			fprintf (stderr, "ERROR: out of memory in duplicating %s\n", str);
			return -1;
		}

		cm = hcl_find_bchar_in_bcstr(xstr, ',');
		*cm = '\0';

		logmask = xtn->logmask;
		do
		{
			flt = cm + 1;

			cm = hcl_find_bchar_in_bcstr(flt, ',');
			if (cm) *cm = '\0';

			if (hcl_comp_bcstr(flt, "app") == 0) logmask |= HCL_LOG_APP;
			else if (hcl_comp_bcstr(flt, "compiler") == 0) logmask |= HCL_LOG_COMPILER;
			else if (hcl_comp_bcstr(flt, "vm") == 0) logmask |= HCL_LOG_VM;
			else if (hcl_comp_bcstr(flt, "mnemonic") == 0) logmask |= HCL_LOG_MNEMONIC;
			else if (hcl_comp_bcstr(flt, "gc") == 0) logmask |= HCL_LOG_GC;
			else if (hcl_comp_bcstr(flt, "ic") == 0) logmask |= HCL_LOG_IC;
			else if (hcl_comp_bcstr(flt, "primitive") == 0) logmask |= HCL_LOG_PRIMITIVE;

			else if (hcl_comp_bcstr(flt, "fatal") == 0) logmask |= HCL_LOG_FATAL;
			else if (hcl_comp_bcstr(flt, "error") == 0) logmask |= HCL_LOG_ERROR;
			else if (hcl_comp_bcstr(flt, "warn") == 0) logmask |= HCL_LOG_WARN;
			else if (hcl_comp_bcstr(flt, "info") == 0) logmask |= HCL_LOG_INFO;
			else if (hcl_comp_bcstr(flt, "debug") == 0) logmask |= HCL_LOG_DEBUG;

			else if (hcl_comp_bcstr(flt, "fatal+") == 0) logmask |= HCL_LOG_FATAL;
			else if (hcl_comp_bcstr(flt, "error+") == 0) logmask |= HCL_LOG_FATAL | HCL_LOG_ERROR;
			else if (hcl_comp_bcstr(flt, "warn+") == 0) logmask |= HCL_LOG_FATAL | HCL_LOG_ERROR | HCL_LOG_WARN;
			else if (hcl_comp_bcstr(flt, "info+") == 0) logmask |= HCL_LOG_FATAL | HCL_LOG_ERROR | HCL_LOG_WARN | HCL_LOG_INFO;
			else if (hcl_comp_bcstr(flt, "debug+") == 0) logmask |= HCL_LOG_FATAL | HCL_LOG_ERROR | HCL_LOG_WARN | HCL_LOG_INFO | HCL_LOG_DEBUG;

			else
			{
				fprintf (stderr, "ERROR: unknown log option value - %s\n", flt);
				if (str != xstr) free (xstr);
				return -1;
			}
		}
		while (cm);

		if (!(logmask & HCL_LOG_ALL_TYPES)) logmask |= HCL_LOG_ALL_TYPES;  /* no types specified. force to all types */
		if (!(logmask & HCL_LOG_ALL_LEVELS)) logmask |= HCL_LOG_ALL_LEVELS;  /* no levels specified. force to all levels */
	}
	else
	{
		logmask = HCL_LOG_ALL_LEVELS | HCL_LOG_ALL_TYPES;
	}

	xtn->logfd = open(xstr, O_CREAT | O_WRONLY | O_APPEND , 0644);
	if (xtn->logfd == -1)
	{
		fprintf (stderr, "ERROR: cannot open a log file %s\n", xstr);
		if (str != xstr) free (xstr);
		return -1;
	}

	xtn->logmask = logmask;
#if defined(HAVE_ISATTY)
	xtn->logfd_istty = isatty(xtn->logfd);
#endif

	if (str != xstr) free (xstr);
	return 0;
}

/* ========================================================================= */

struct proto_xtn_t
{
	int x;
};
typedef struct proto_xtn_t proto_xtn_t;

static int handle_packet (hcl_xproto_t* proto, hcl_xpkt_type_t type, const void* data, hcl_oow_t len)
{
	if (type == HCL_XPKT_STDOUT)
	{
		/*if (len > 0) fwrite (data, 1, len, stdout); */
		if (len > 0) fprintf (stdout, "%.*s", (int)len, data);
	}
	return 1;
}

static int handle_request (hcl_client_t* client, const char* ipaddr, const char* script, int reuse_addr, int shut_wr_after_req)
{
	hcl_sckaddr_t sckaddr;
	hcl_scklen_t scklen;
	int sckfam;
	int sck = -1;

	hcl_oow_t used, avail;
	int x;
	hcl_bch_t buf[256];
	ssize_t n;
	const char* scptr;
	const char* sccur;
	hcl_xproto_t* proto = HCL_NULL;

	client_xtn_t* client_xtn;
	proto_xtn_t* proto_xtn;
	hcl_xproto_cb_t proto_cb;

	client_xtn = hcl_client_getxtn(client);

	sckfam = hcl_bchars_to_sckaddr(ipaddr, strlen(ipaddr), &sckaddr, &scklen);
	if (sckfam <= -1)
	{
		fprintf (stderr, "cannot convert ip address - %s\n", ipaddr);
		goto oops;
	}

	sck = socket(sckfam, SOCK_STREAM, 0);
	if (sck <= -1)
	{
		fprintf (stderr, "cannot create a socket for %s - %s\n", ipaddr, strerror(errno));
		goto oops;
	}

	if (reuse_addr)
	{
		if (sckfam == AF_INET)
		{
			struct sockaddr_in anyaddr;
			int opt = 1;
			setsockopt(sck, SOL_SOCKET, SO_REUSEADDR, (char *)&opt, sizeof(opt));
			memset (&anyaddr, 0, HCL_SIZEOF(anyaddr));
			anyaddr.sin_family = sckfam;
			bind(sck, (struct sockaddr *)&anyaddr, scklen);
		}
		else if (sckfam == AF_INET6)
		{
			struct sockaddr_in6 anyaddr;
			int opt = 1;
			setsockopt(sck, SOL_SOCKET, SO_REUSEADDR, (char *)&opt, sizeof(opt));
			memset (&anyaddr, 0, HCL_SIZEOF(anyaddr));
			anyaddr.sin6_family = sckfam;
			bind(sck, (struct sockaddr *)&anyaddr, scklen);
		}
	}

	if (connect(sck, (struct sockaddr*)&sckaddr, scklen) <= -1)
	{
		fprintf (stderr, "cannot connect to %s - %s\n", ipaddr, strerror(errno));
		goto oops;
	}

	memset (&proto, 0, HCL_SIZEOF(proto_cb));
	proto_cb.on_packet = handle_packet;

	proto = hcl_xproto_open(hcl_client_getmmgr(client), &proto_cb, HCL_SIZEOF(*proto_xtn));
	if (HCL_UNLIKELY(!proto))
	{
		fprintf (stderr, "cannot open protocol to %s\n", ipaddr);
		goto oops;
	}
	proto_xtn = hcl_xproto_getxtn(proto);
	//proto_xtn->client = client;

	scptr = sccur = script;
	while (1)
	{
		struct pollfd pfd;

		pfd.fd = sck;
		pfd.events = POLLIN;
		if (*sccur != '\0') pfd.events |= POLLOUT;
		pfd.revents = 0;

		n = poll(&pfd, 1, 1000);
		if (n <= -1)
		{
			fprintf (stderr, "poll error on %d - %s\n", sck, strerror(n));
			goto oops;
		}

		if (n == 0)
		{
			/* TODO: proper timeout handling */
			continue;
		}

		if (pfd.revents & POLLERR)
		{
			fprintf (stderr, "error condition detected on %d\n", sck);
			goto oops;
		}

		if (pfd.revents & POLLOUT)
		{
			hcl_xpkt_hdr_t hdr;
			struct iovec iov[2];
			hcl_uint16_t seglen;

			while (*sccur != '\0' && sccur - scptr < HCL_XPKT_MAX_PLD_LEN) sccur++;

			seglen = sccur - scptr;

			hdr.id = 1; /* TODO: */
			hdr.type = HCL_XPKT_CODE | (((seglen >> 8) & 0x0F) << 4);
			hdr.len = seglen & 0xFF;

			iov[0].iov_base = &hdr;
			iov[0].iov_len = HCL_SIZEOF(hdr);
			iov[1].iov_base = scptr;
			iov[1].iov_len = seglen;

			hcl_sys_send_iov (sck, iov, 2); /* TODO: error check */

			scptr = sccur;

			if (*sccur == '\0')
			{
				hdr.id = 1; /* TODO: */
				hdr.type = HCL_XPKT_EXECUTE;
				hdr.len = 0;

				iov[0].iov_base = &hdr;
				iov[0].iov_len = HCL_SIZEOF(hdr);
				hcl_sys_send_iov (sck, iov, 1);

				if (shut_wr_after_req)
				{
					shutdown (sck, SHUT_WR);
				}
				else
				{
					hdr.type = HCL_XPKT_DISCONNECT;
					hdr.id = 1; /* TODO: */
					hdr.len = 0;

					iov[0].iov_base = &hdr;
					iov[0].iov_len = HCL_SIZEOF(hdr);
					hcl_sys_send_iov (sck, iov, 1);
				}
			}
		}

		if (pfd.revents & POLLIN)
		{
			hcl_oow_t bcap;
			hcl_uint8_t* bptr;

			bptr = hcl_xproto_getbuf(proto, &bcap);;
			x = recv(sck, bptr, bcap, 0);
			if (x <= -1)
			{
				if (errno == EINTR) goto carry_on; /* didn't read read */
				/*hcl_seterrwithsyserr (hcl, 0, errno); */
				/* TODO: error info set... */
				return -1;
			}
			if (x == 0) hcl_xproto_seteof(proto, 1);
			hcl_xproto_advbuf (proto, x);
		}


	carry_on:
		while (hcl_xproto_ready(proto))
		{
			if ((n = hcl_xproto_process(proto)) <= -1)
			{
				/* TODO: proper error message */
				return -1;
			}
			if (n == 0)
			{
				/* TODO: chceck if there is remaining data in the buffer...?? */
				printf ("NO MORE DATA. EXITING...\n");
				goto done;
			}
		}

		if (hcl_xproto_geteof(proto)) break;
	}
done:

/* TODO: we can check if the buffer has all been consumed. if not, there is trailing garbage.. */
	/*{
		struct linger linger;
		linger.l_onoff = 1;
		linger.l_linger = 0;
		setsockopt (sck, SOL_SOCKET, SO_LINGER, (char *) &linger, sizeof(linger));
	}*/

	hcl_xproto_close (proto);
	close (sck);
	return 0;

oops:
	if (proto) hcl_xproto_close (proto);
	if (sck >= 0) close (sck);
	return -1;
}

int main (int argc, char* argv[])
{
	hcl_bci_t c;
	static hcl_bopt_lng_t lopt[] =
	{
		{ ":log",                  'l'  },
		{ "reuseaddr",             '\0' },
		{ "shutwr",                '\0' },
		{ HCL_NULL,                '\0' }
	};
	static hcl_bopt_t opt =
	{
		"l:",
		lopt
	};

	hcl_client_t* client;
	client_xtn_t* xtn;
	hcl_client_prim_t client_prim;
	int n;
	const char* logopt = HCL_NULL;
	int reuse_addr = 0;
	int shut_wr_after_req = 0;

	setlocale (LC_ALL, "");

	if (argc < 2)
	{
	print_usage:
		fprintf (stderr, "Usage: %s [-l/--log log-options] [--reuseaddr] [--shutwr] bind-address:port script-to-run\n", argv[0]);
		return -1;
	}

	while ((c = hcl_getbopt (argc, argv, &opt)) != HCL_BCI_EOF)
	{
		switch (c)
		{
			case 'l':
				logopt = opt.arg;
				break;

			case '\0':
				if (hcl_comp_bcstr(opt.lngopt, "reuseaddr") == 0)
				{
					reuse_addr = 1;
				}
				else if (hcl_comp_bcstr(opt.lngopt, "shutwr") == 0)
				{
					shut_wr_after_req = 1;
				}
				else
				{
					goto print_usage;
				}
				break;

			case ':':
				if (opt.lngopt)
					fprintf (stderr, "bad argument for '%s'\n", opt.lngopt);
				else
					fprintf (stderr, "bad argument for '%c'\n", opt.opt);

				return -1;

			default:
				goto print_usage;
		}
	}

	/* needs 2 fixed arguments */
	if (opt.ind + 1 >= argc) goto print_usage;

	memset (&client_prim, 0, HCL_SIZEOF(client_prim));
	client_prim.log_write = log_write;

	client = hcl_client_open(&sys_mmgr, HCL_SIZEOF(client_xtn_t), &client_prim, HCL_NULL);
	if (!client)
	{
		fprintf (stderr, "cannot open client\n");
		return -1;
	}

	xtn = (client_xtn_t*)hcl_client_getxtn(client);
	xtn->logfd = -1;
	xtn->logfd_istty = 0;

	if (logopt)
	{
		if (handle_logopt(client, logopt) <= -1) goto oops;
	}
	else
	{
		/* default logging mask when no logging option is set */
		xtn->logmask = HCL_LOG_ALL_TYPES | HCL_LOG_ERROR | HCL_LOG_FATAL;
	}

	g_client = client;
	set_signal (SIGINT, handle_sigint);
	set_signal_to_ignore (SIGPIPE);

#if 0
	n = hcl_client_connect(client, argv[opt.ind], reuse_addr);
	if (n <= -1)
	{
		fprintf (stderr, "ERROR: %s\n", hcl_client_geterrbmsg(client));
		goto oops;
	}
#else
	n = handle_request(client, argv[opt.ind], argv[opt.ind + 1], reuse_addr, shut_wr_after_req);
#endif

	set_signal_to_default (SIGINT);
	set_signal_to_default (SIGPIPE);
	g_client = NULL;

	if (xtn->logfd >= 0)
	{
		close (xtn->logfd);
		xtn->logfd = -1;
		xtn->logfd_istty = 0;
	}

	hcl_client_close (client);
	return n;

oops:
	if (client) hcl_client_close (client);
	return -1;
}
