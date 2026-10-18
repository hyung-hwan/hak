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

/* poll() for OpenVMS.
 *
 * WHY THIS IS NOT A WRAPPER ROUND ONE PRIMITIVE
 *   OpenVMS offers two ways to ask "is this ready", and neither covers the
 *   set hak actually multiplexes.
 *
 *     select() belongs to the TCP/IP services and understands sockets only.
 *     Handed a pipe it answers -1/ENOTSOCK - measured, not assumed.
 *
 *     $GETDVI with DVI$_DEVDEPEND reports a mailbox's queued message count
 *     without taking anything off the queue, which is exactly the question
 *     poll() asks, but means nothing on a socket.
 *
 *   hak needs both at once. hak_server_start() watches its listening sockets
 *   in the same set as the wake pipe, and the client adds the script file on
 *   top of that. So this file dispatches per descriptor rather than picking
 *   one mechanism for the whole call.
 *
 *   [HISTORY] an earlier version of this file treated every descriptor as a
 *   mailbox and rejected the rest as POLLNVAL. The rejection never happened
 *   for a socket, because a BG device answers DVI$_MBX = 1 and so passed for a
 *   mailbox; DVI$_DEVDEPEND was then read as a message count and returned
 *   whatever the socket driver keeps in that longword. On an idle listener
 *   that was 15024, so POLLIN came back set on every call forever. The server
 *   spun at 100% CPU and never accepted a connection. Classify first.
 *
 * HOW A DESCRIPTOR IS CLASSIFIED
 *   fstat() settles it. S_ISSOCK is true for a BG device, which has to be
 *   tested first: a BG device also answers DVI$_MBX, so a socket asked that
 *   question passes for a mailbox. Only once a socket is ruled out is a
 *   channel assigned and DVI$_MBX consulted, and a regular file is taken
 *   from st_mode.
 *
 *   No channel is ever assigned for a socket. Nothing here issues a $QIO on
 *   one - see [WHY SOCKETS DO NOT USE AN ATTENTION AST] below - so the
 *   descriptor number that select() wants is all a socket needs.
 *
 * HOW READINESS IS TESTED - the scan
 *   Level triggered, and nothing is consumed:
 *     socket  - select(). With a zero timeout it is a scan; with the
 *               caller's timeout the same call is also the wait.
 *     mailbox - $GETDVI/DVI$_DEVDEPEND, the queued message count.
 *     file    - always ready, as poll() reports for a regular file anywhere.
 *
 * HOW WAITING IS DONE - the block
 *   The two classes wait by different means, because only one of them has a
 *   documented way to say "tell me when something arrives".
 *
 *     socket  - select() does both the looking and the waiting. Given the
 *               caller's timeout it blocks until a descriptor is ready or the
 *               time runs out, and it is exact: no spurious readiness, and
 *               the timeout honoured to the second.
 *
 *     mailbox - IO$_SETMODE|IO$M_WRTATTN asks for an AST when somebody writes
 *               to the mailbox, which is the data-arrived direction.
 *               IO$M_READATTN on a mailbox means the opposite - somebody
 *               issued a read - and is not what a POLLIN waiter wants. The
 *               AST sets an event flag allocated per context by lib$get_ef(),
 *               $SETIMR is pointed at the same flag for the timeout, and
 *               $WAITFR blocks on it at no CPU cost.
 *
 *   A set holding both cannot use either on its own: select() answers
 *   ENOTSOCK for a mailbox, and $WAITFR cannot be woken by a socket. Such a
 *   set is therefore served by select() on the socket subset with a short
 *   slice, re-testing the mailboxes between slices. Mailbox latency is then
 *   bounded by VP_MIXED_SLICE rather than immediate. hak does not currently
 *   produce a mixed set - x-server and x-client watch only sockets, now that
 *   hak_sys_open_pipes() builds the wake channel from a loopback pair, and
 *   std.c watches only mailboxes - so this is the path that correctness
 *   requires rather than the one that runs.
 *
 *   [WHY SOCKETS DO NOT USE AN ATTENTION AST]
 *   They did, briefly, and it crashed the machine.
 *
 *   IO$_SETMODE|IO$M_READATTN on a BG channel does deliver an AST - that was
 *   measured, repeatedly, including with a connection already pending. But
 *   delivering an AST is not evidence that P1 and P2 mean on this driver what
 *   they mean on the mailbox driver, and they appear not to: on OpenVMS VAX
 *   V7.3 the system took an INVEXCEPTN bugcheck at IPL 8 with this call
 *   stack,
 *
 *       EXE$QIO+0028E
 *       INET$FDT_RCV+00179
 *       INET_INIT_MALLOC+00182
 *       EXE$EXCEPTION+00047
 *
 *   that is, the $QIO entered the TCP/IP driver's receive FDT routine and
 *   faulted there in kernel mode. A driver must not fault whatever it is
 *   handed, so that is a bug in the driver - but it is reached by passing it
 *   parameters whose meaning was inferred from experiment rather than read
 *   from the BG interface, and it survived every test before it did not.
 *   select() is documented, exact, and cannot bugcheck the system.
 *
 *   The mailbox AST path below is kept because it is the documented mailbox
 *   interface and is what this file has always used.
 *
 * WHAT IS STILL APPROXIMATE
 *   POLLOUT on a mailbox is answered ready without testing, as it always was:
 *   a mailbox write blocks only when the buffer quota is exhausted, and saying
 *   ready early costs at worst a blocking write, where saying not-ready
 *   wrongly would stall the caller. POLLOUT on a socket is exact - select()
 *   answers it like any other readiness.
 */

#include "poll-vms.h"

#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/socket.h>  /* fd_set and select() */
#include <sys/time.h>    /* struct timeval */

#include <descrip.h>
#include <starlet.h>
#include <lib$routines.h>
#include <ssdef.h>
#include <iodef.h>
#include <dvidef.h>

/* how long select() may block at a time when the set holds both sockets and
 * mailboxes, and neither wait primitive can cover the other. */
#define VP_MIXED_SLICE 100 /* milliseconds */

/* what a descriptor turned out to be */
#define VP_CLS_NONE    0 /* unusable - reported POLLNVAL */
#define VP_CLS_SOCKET  1
#define VP_CLS_MAILBOX 2
#define VP_CLS_FILE    3

/* what one descriptor resolved to for the duration of a single call. kept
 * beside the caller's pollfd array rather than in it, so that the public
 * struct stays the shape poll() users expect. */
typedef struct vp_ent_t vp_ent_t;
struct vp_ent_t
{
	int cls;
	unsigned short chan; /* the sdc for a socket, the assigned channel for a mailbox */
	int armed;           /* an attention AST is outstanding on chan */
};

/* ------------------------------------------------------------------------ *
 * CHANNEL CACHE
 *
 * $ASSIGN per descriptor per call would be a system service on every trip
 * through the multiplexer. The channel is kept instead, keyed by descriptor.
 *
 * Only mailboxes are cached. A socket's channel comes from decc$get_sdc(),
 * which is a lookup in the C run-time rather than a system service, and which
 * belongs to the run-time - assigning or deassigning it here would be wrong.
 *
 * A descriptor number is reused after close(), so the cached entry is only
 * trusted when the device name still matches what the descriptor names now.
 * That check is a $GETDVI-free fstat() and is what makes the cache safe.
 * ------------------------------------------------------------------------ */

#define VP_DEVNAM_LEN HAK_SYS_POLL_DEVNAM_LEN
#define VP_CACHE_SIZE HAK_SYS_POLL_MAX_FDS
typedef hak_sys_poll_chan_t vp_chan_t;

static void vp_init_cache (hak_sys_poll_ctx_t* ctx)
{
	int i;
	if (ctx->inited) return;
	for (i = 0; i < VP_CACHE_SIZE; i++) ctx->chan[i].fd = -1;
	ctx->inited = 1;
}

static void vp_drop (vp_chan_t* c)
{
	if (c->fd >= 0)
	{
		sys$dassgn(c->chan);
		c->fd = -1;
		c->chan = 0;
		c->dev[0] = '\0';
	}
}

int hak_sys_poll_ctx_init (hak_sys_poll_ctx_t* ctx)
{
	unsigned int sts;

	ctx->inited = 0;
	ctx->efn = 0;
	vp_init_cache(ctx);

	/* lib$get_ef() hands out a local event flag that nothing else in the
	 * image is using. Picking a number by hand would risk colliding with one
	 * the C run-time or the TCP/IP services reserve for themselves. */
	sts = lib$get_ef(&ctx->efn);
	if (!(sts & 1))
	{
		ctx->efn = 0;
		errno = EAGAIN;
		return -1;
	}

	return 0;
}

void hak_sys_poll_ctx_fini (hak_sys_poll_ctx_t* ctx)
{
	int i;
	if (!ctx->inited) return;
	for (i = 0; i < VP_CACHE_SIZE; i++) vp_drop(&ctx->chan[i]);
	if (ctx->efn != 0)
	{
		lib$free_ef(&ctx->efn);
		ctx->efn = 0;
	}
	ctx->inited = 0;
}

/* the device a descriptor is attached to, or -1 if it has none */
static int vp_devnam (int fd, char* buf, size_t bufsz)
{
	struct stat st;
	if (fstat(fd, &st) == -1) return -1;
	if (st.st_dev[0] == '\0') return -1;
	strncpy(buf, st.st_dev, bufsz - 1);
	buf[bufsz - 1] = '\0';
	return 0;
}

/* the channel for a mailbox descriptor, assigning and caching one if needed.
 * answers 0 on success, -1 if the descriptor names no device. */
static int vp_chan_for (hak_sys_poll_ctx_t* ctx, int fd, unsigned short* chanp)
{
	char dev[VP_DEVNAM_LEN];
	struct dsc$descriptor_s d;
	unsigned int sts;
	int i, free_slot = -1;

	vp_init_cache(ctx);

	if (vp_devnam(fd, dev, sizeof(dev)) <= -1) return -1;

	for (i = 0; i < VP_CACHE_SIZE; i++)
	{
		if (ctx->chan[i].fd == fd)
		{
			/* the descriptor number may have been closed and handed out
			 * again for something else. the device name settles it. */
			if (strcmp(ctx->chan[i].dev, dev) == 0)
			{
				*chanp = ctx->chan[i].chan;
				return 0;
			}
			vp_drop(&ctx->chan[i]);
			free_slot = i;
			break;
		}
		if (ctx->chan[i].fd < 0 && free_slot < 0) free_slot = i;
	}

	d.dsc$w_length = (unsigned short)strlen(dev);
	d.dsc$b_dtype = DSC$K_DTYPE_T;
	d.dsc$b_class = DSC$K_CLASS_S;
	d.dsc$a_pointer = dev;

	sts = sys$assign(&d, chanp, 0, 0);
	if (!(sts & 1)) return -1;

	if (free_slot >= 0)
	{
		ctx->chan[free_slot].fd = fd;
		ctx->chan[free_slot].chan = *chanp;
		strcpy(ctx->chan[free_slot].dev, dev);
	}
	/* if the cache is full the channel is still usable, it just is not kept.
	 * it leaks until hak_sys_poll_ctx_fini(), which is why the cache is sized
	 * well above the number of handles a vm realistically multiplexes. */

	return 0;
}

/* ------------------------------------------------------------------------ *
 * CLASSIFICATION
 * ------------------------------------------------------------------------ */

/* is this a mailbox? a BG socket device also answers yes here, which is why
 * the socket test must come first in vp_classify(). */
static int vp_is_mbx (unsigned short chan)
{
	struct
	{
		unsigned short buflen, itmcod;
		void* bufadr;
		void* retlen;
	} itm[2];
	int v = 0;
	unsigned int sts;

	itm[0].buflen = sizeof(v);
	itm[0].itmcod = DVI$_MBX;
	itm[0].bufadr = &v;
	itm[0].retlen = 0;
	itm[1].buflen = 0;
	itm[1].itmcod = 0;
	itm[1].bufadr = 0;
	itm[1].retlen = 0;

	sts = sys$getdviw(0, chan, 0, itm, 0, 0, 0, 0);
	if (!(sts & 1)) return 0;
	return v != 0;
}

/* decide what one descriptor is, and get hold of its channel if it is a
 * mailbox, which is the only class this file issues a $QIO on. */
static void vp_classify (hak_sys_poll_ctx_t* ctx, int fd, vp_ent_t* e)
{
	struct stat st;

	e->cls = VP_CLS_NONE;
	e->chan = 0;
	e->armed = 0;

	if (fstat(fd, &st) <= -1) return;

	/* a socket first: a BG device answers DVI$_MBX too, so asking that
	 * question of a socket would call it a mailbox */
	if (S_ISSOCK(st.st_mode))
	{
		e->cls = VP_CLS_SOCKET;
		return;
	}

	if (vp_chan_for(ctx, fd, &e->chan) == 0 && vp_is_mbx(e->chan))
	{
		e->cls = VP_CLS_MAILBOX;
		return;
	}

	/* a regular file never blocks, so poll() calls it ready. anything left -
	 * a terminal, most likely - gets POLLNVAL rather than a guess. */
	e->chan = 0;
	if (S_ISREG(st.st_mode)) e->cls = VP_CLS_FILE;
}

/* ------------------------------------------------------------------------ *
 * READINESS
 * ------------------------------------------------------------------------ */

/* queued message count, without taking anything off the queue. meaningful for
 * a mailbox only - see the note about DVI$_DEVDEPEND at the top of the file. */
static int vp_msgcnt (unsigned short chan, int* cnt)
{
	struct
	{
		unsigned short buflen, itmcod;
		void* bufadr;
		void* retlen;
	} itm[2];
	int v = 0;
	unsigned int sts;

	itm[0].buflen = sizeof(v);
	itm[0].itmcod = DVI$_DEVDEPEND;
	itm[0].bufadr = &v;
	itm[0].retlen = 0;
	itm[1].buflen = 0;
	itm[1].itmcod = 0;
	itm[1].bufadr = 0;
	itm[1].retlen = 0;

	sts = sys$getdviw(0, chan, 0, itm, 0, 0, 0, 0);
	if (!(sts & 1)) return -1;

	/* for a mailbox the low word of the device dependent longword is the
	 * number of messages waiting */
	*cnt = v & 0xFFFF;
	return 0;
}

/* Fill in revents for every descriptor. Answers how many have something.
 *
 * sel_ms is how long select() may block on the socket subset: 0 makes this a
 * pure scan, a positive value makes the same call the wait as well, and a
 * negative value lets it block until something happens. It is ignored when
 * the set holds no socket. */
static int vp_scan (struct pollfd* pfd, nfds_t nfd, vp_ent_t* ent, int sel_ms)
{
	fd_set rfds, wfds;
	struct timeval tv;
	struct timeval* tvp;
	int maxfd = -1, have_sck = 0, nready = 0;
	nfds_t i;

	/* one select() answers for every socket in the set at once */
	FD_ZERO(&rfds);
	FD_ZERO(&wfds);
	for (i = 0; i < nfd; i++)
	{
		pfd[i].revents = 0;
		if (pfd[i].fd < 0 || ent[i].cls != VP_CLS_SOCKET) continue;
		if (pfd[i].events & POLLIN) FD_SET(pfd[i].fd, &rfds);
		if (pfd[i].events & POLLOUT) FD_SET(pfd[i].fd, &wfds);
		if (pfd[i].fd > maxfd) maxfd = pfd[i].fd;
		have_sck = 1;
	}

	if (have_sck)
	{
		if (sel_ms < 0) tvp = (struct timeval*)0; /* block until something happens */
		else
		{
			tv.tv_sec = sel_ms / 1000;
			tv.tv_usec = (sel_ms % 1000) * 1000;
			tvp = &tv;
		}

		if (select(maxfd + 1, &rfds, &wfds, (fd_set*)0, tvp) <= -1)
		{
			/* report nothing rather than everything: a wrong not-ready costs
			 * one more trip round the caller's loop, a wrong ready costs a
			 * blocking read. */
			FD_ZERO(&rfds);
			FD_ZERO(&wfds);
		}
	}

	for (i = 0; i < nfd; i++)
	{
		if (pfd[i].fd < 0) continue; /* the caller has disabled this one */

		switch (ent[i].cls)
		{
			case VP_CLS_SOCKET:
				if ((pfd[i].events & POLLIN) && FD_ISSET(pfd[i].fd, &rfds)) pfd[i].revents |= POLLIN;
				if ((pfd[i].events & POLLOUT) && FD_ISSET(pfd[i].fd, &wfds)) pfd[i].revents |= POLLOUT;
				break;

			case VP_CLS_MAILBOX:
			{
				int cnt;
				if ((pfd[i].events & POLLIN) && vp_msgcnt(ent[i].chan, &cnt) == 0 && cnt > 0)
					pfd[i].revents |= POLLIN;
				/* see the header comment: writability is answered optimistically */
				if (pfd[i].events & POLLOUT) pfd[i].revents |= POLLOUT;
				break;
			}

			case VP_CLS_FILE:
				pfd[i].revents |= (pfd[i].events & (POLLIN | POLLOUT));
				break;

			default:
				pfd[i].revents = POLLNVAL;
				break;
		}

		if (pfd[i].revents) nready++;
	}

	return nready;
}

/* ------------------------------------------------------------------------ *
 * WAITING
 * ------------------------------------------------------------------------ */

/* [IMPORTANT] this runs as an AST, which interrupts the caller at an
 * arbitrary instruction. It does the least it possibly can - set the event
 * flag - for the same reason a signal handler does.
 *
 * The context arrives as the AST parameter, P2 of the IO$_SETMODE $QIO. The
 * signature stays a longword so that the $QIO prototype sees exactly the
 * function type it always has. */
static void vp_ast (hak_uintptr_t prm)
{
	hak_sys_poll_ctx_t* ctx = (hak_sys_poll_ctx_t*)prm;
	sys$setef(ctx->efn);
}

/* Ask the mailbox driver to raise an attention AST on the next write.
 * Answers how many channels took one, so that the caller knows whether
 * sleeping could ever be woken.
 *
 * Sockets are deliberately absent - see [WHY SOCKETS DO NOT USE AN ATTENTION
 * AST] at the top of this file. Nothing here may issue a $QIO on a BG
 * channel. */
static int vp_arm (hak_sys_poll_ctx_t* ctx, struct pollfd* pfd, nfds_t nfd, vp_ent_t* ent)
{
	nfds_t i;
	int armed = 0;

	for (i = 0; i < nfd; i++)
	{
		ent[i].armed = 0;
		if (pfd[i].fd < 0 || ent[i].cls != VP_CLS_MAILBOX) continue;
		if (!(pfd[i].events & POLLIN)) continue;

		/* WRTATTN, not READATTN: this fires when somebody writes a message,
		 * which is what a POLLIN waiter is waiting for. READATTN on a mailbox
		 * is the other direction - somebody issued a read.
		 *
		 * P1 is the AST routine, P2 the parameter it is handed. */
		if (sys$qio(0, ent[i].chan, IO$_SETMODE | IO$M_WRTATTN, 0, 0, 0,
		            vp_ast, (hak_uintptr_t)ctx, 0, 0, 0, 0) & 1)
		{
			ent[i].armed = 1;
			armed++;
		}
	}

	return armed;
}

/* take the attention ASTs back down. the channel is a mailbox this file
 * assigned for itself, so sys$cancel is safe: nothing else has I/O
 * outstanding on it. */
static void vp_disarm (struct pollfd* pfd, nfds_t nfd, vp_ent_t* ent)
{
	nfds_t i;
	for (i = 0; i < nfd; i++)
	{
		if (!ent[i].armed) continue;
		sys$cancel(ent[i].chan);
		ent[i].armed = 0;
	}
}

/* a delta time is a negative quadword counted in 100ns units. this platform
 * has no 64-bit integer type at all - see lib/hak-vms.h - so the quadword is
 * assembled from 32-bit halves by hand.
 *
 * magnitude = ms * 10000, computed in 16-bit pieces so that no intermediate
 * product can overflow 32 bits, then negated in two's complement across the
 * pair. */
static void vp_delta (int ms, unsigned int timbuf[2])
{
	unsigned int t, p0, p1, lo, hi;

	t = (unsigned int)ms;
	p0 = (t & 0xFFFF) * 10000;   /* at most 65535 * 10000, fits */
	p1 = (t >> 16) * 10000;      /* likewise */

	lo = p0 + (p1 << 16);
	hi = (p1 >> 16) + ((lo < p0)? 1: 0); /* carry out of the low half */

	lo = ~lo + 1;                        /* negate the pair */
	hi = ~hi + ((lo == 0)? 1: 0);

	timbuf[0] = lo;
	timbuf[1] = hi;
}

int hak_sys_poll (hak_sys_poll_ctx_t* ctx, struct pollfd* pfd, nfds_t nfd, int timeout)
{
	vp_ent_t ent[VP_CACHE_SIZE];
	unsigned int timbuf[2];
	nfds_t i;
	int nready, remaining;
	int have_sck = 0, have_mbx = 0;

	if (!ctx || !ctx->inited || ctx->efn == 0)
	{
		errno = EINVAL;
		return -1;
	}

	if (nfd > VP_CACHE_SIZE)
	{
		/* the multiplexer never watches this many handles. raising
		 * HAK_SYS_POLL_MAX_FDS is the fix if it ever does. */
		errno = EINVAL;
		return -1;
	}

	/* work out what each descriptor is, once, up front */
	for (i = 0; i < nfd; i++)
	{
		if (pfd[i].fd < 0)
		{
			ent[i].cls = VP_CLS_NONE;
			ent[i].chan = 0;
			ent[i].armed = 0;
			continue;
		}
		vp_classify(ctx, pfd[i].fd, &ent[i]);
		if (ent[i].cls == VP_CLS_SOCKET) have_sck = 1;
		else if (ent[i].cls == VP_CLS_MAILBOX) have_mbx = 1;
	}

	nready = vp_scan(pfd, nfd, ent, 0);
	if (nready > 0 || timeout == 0) return nready;

	/* nothing yet and the caller is prepared to wait. which wait depends on
	 * what is in the set - see "HOW WAITING IS DONE" at the top. */

	if (have_sck && !have_mbx)
	{
		/* sockets only. select() is both exact and able to block, so one
		 * call does the whole job and nothing else is needed. */
		return vp_scan(pfd, nfd, ent, timeout);
	}

	if (have_sck) /* ...and mailboxes too */
	{
		/* Mixed. Neither primitive can wait for the other's descriptors, so
		 * select() blocks on the sockets a slice at a time and the mailboxes
		 * are re-tested between slices. No attention AST is armed: one armed
		 * on a mailbox would not interrupt select() anyway, and the cost of
		 * arming is wasted. */
		remaining = timeout;
		while (1)
		{
			int wait_ms = remaining;
			if (wait_ms < 0 || wait_ms > VP_MIXED_SLICE) wait_ms = VP_MIXED_SLICE;

			nready = vp_scan(pfd, nfd, ent, wait_ms);
			if (nready > 0) return nready;

			if (remaining >= 0)
			{
				remaining -= wait_ms;
				if (remaining <= 0) return 0;
			}
		}
	}

	/* Mailboxes only - or nothing waitable at all. The attention AST path. */
	remaining = timeout; /* negative means forever */

	while (1)
	{
		int armed;

		/* [ORDER MATTERS] clear the flag, queue the timer, and only then arm
		 * the ASTs. $SETIMR clears the event flag it is given at the moment
		 * it queues the timer, so a timer set after the ASTs were armed would
		 * wipe a wakeup that had already arrived and the caller would sleep
		 * out the whole timeout with its data waiting. */
		sys$clref(ctx->efn);

		if (remaining >= 0)
		{
			/* the request id doubles as the handle for the cancellation
			 * below, and lets it name this timer rather than every timer the
			 * process has outstanding. the event flag is set on expiry, so no
			 * AST is needed for the timer itself. */
			vp_delta(remaining, timbuf);
			sys$setimr(ctx->efn, timbuf, 0, (hak_uintptr_t)ctx, 0);
		}

		armed = vp_arm(ctx, pfd, nfd, ent);

		if (armed <= 0 && remaining < 0)
		{
			/* nothing could ever wake us and no deadline was given. sleeping
			 * would hang the caller for good. */
			vp_disarm(pfd, nfd, ent);
			return 0;
		}

		/* [NO SCAN HERE] looking once more, in case something arrived between
		 * the scan above and the arming just done, is unnecessary: the
		 * attention AST is not purely edge triggered. Arming it with a
		 * message already sitting in the mailbox delivers the AST straight
		 * away - measured on OpenVMS VAX V7.3 - so there is no window to
		 * close. */

		sys$waitfr(ctx->efn);

		if (remaining >= 0) sys$cantim((hak_uintptr_t)ctx, 0);
		vp_disarm(pfd, nfd, ent);

		nready = vp_scan(pfd, nfd, ent, 0);
		if (nready > 0) return nready;

		/* woken with nothing to show for it: the timer expired. */
		if (remaining >= 0) return 0;
	}
}

/* The original entry points, kept for callers that predate the context -
 * std.c among them. They drive one shared context and so carry the same
 * single-threaded restriction they always had. */
static hak_sys_poll_ctx_t g_poll_ctx;
static int g_poll_ctx_ready = 0;

int poll (struct pollfd* pfd, nfds_t nfd, int timeout)
{
	if (!g_poll_ctx_ready)
	{
		if (hak_sys_poll_ctx_init(&g_poll_ctx) <= -1) return -1;
		g_poll_ctx_ready = 1;
	}
	return hak_sys_poll(&g_poll_ctx, pfd, nfd, timeout);
}

void poll_vms_cleanup (void)
{
	if (g_poll_ctx_ready)
	{
		hak_sys_poll_ctx_fini(&g_poll_ctx);
		g_poll_ctx_ready = 0;
	}
}
