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

/* poll() for OpenVMS, over $QIO.
 *
 * HOW READINESS IS TESTED
 *   A read $QIO on a mailbox consumes the message, so it cannot be used to
 *   ask "is there anything there". $GETDVI with DVI$_DEVDEPEND answers the
 *   queued message count instead and takes nothing off the queue, which is
 *   what poll() needs. This gives level-triggered semantics for free: the
 *   count stays non-zero until the caller actually reads.
 *
 * HOW WAITING IS DONE
 *   IO$_SETMODE|IO$M_WRTATTN asks for an AST when somebody writes to the
 *   mailbox. Every watched channel gets one, then the thread hibernates and
 *   the AST wakes it. There is no sleep loop and no busy polling.
 *
 *   Attention ASTs are one-shot and fire on the NEXT write, so a message
 *   already queued when the AST is armed does not trigger one. The scan is
 *   therefore repeated after arming - see the comment at that point - which
 *   is also what closes the race against a write landing between the first
 *   scan and the arming.
 *
 * WHAT IT COVERS
 *   Mailbox devices. The C run-time builds pipe() out of a mailbox, so a
 *   pipe descriptor is a mailbox and is handled; that is the case hak needs,
 *   since its signal self-pipe and sys.pipe handles are pipes. Anything else
 *   is reported POLLNVAL rather than silently treated as ready.
 *
 * WHAT IT DOES NOT DO
 *   POLLOUT is answered ready without testing. A mailbox write blocks only
 *   when the buffer quota is exhausted, and reporting ready early costs at
 *   worst a blocking write later, whereas reporting not-ready wrongly would
 *   stall the caller. IO$M_READATTN would give the exact answer and can be
 *   added if it ever matters.
 */

#include "poll-vms.h"

#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>

#include <descrip.h>
#include <starlet.h>
#include <lib$routines.h>
#include <ssdef.h>
#include <iodef.h>
#include <dvidef.h>

/* ------------------------------------------------------------------------ *
 * CHANNEL CACHE
 *
 * $ASSIGN per descriptor per call would be two system services on every trip
 * through the multiplexer. The channel is kept instead, keyed by descriptor.
 *
 * A descriptor number is reused after close(), so the cached entry is only
 * trusted when the device name still matches what the descriptor names now.
 * That check is a $GETDVI-free fstat() and is what makes the cache safe.
 * ------------------------------------------------------------------------ */

#define VP_DEVNAM_LEN 64
#define VP_CACHE_SIZE 64

typedef struct vp_chan_t vp_chan_t;
struct vp_chan_t
{
	int fd;                      /* -1 when the slot is free */
	unsigned short chan;
	char dev[VP_DEVNAM_LEN];
};

static vp_chan_t g_chan[VP_CACHE_SIZE];
static int g_chan_inited = 0;

static void vp_init_cache (void)
{
	int i;
	if (g_chan_inited) return;
	for (i = 0; i < VP_CACHE_SIZE; i++) g_chan[i].fd = -1;
	g_chan_inited = 1;
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

void poll_vms_cleanup (void)
{
	int i;
	if (!g_chan_inited) return;
	for (i = 0; i < VP_CACHE_SIZE; i++) vp_drop(&g_chan[i]);
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

/* the channel for a descriptor, assigning and caching one if needed.
 * answers 0 on success, -1 if the descriptor names no device. */
static int vp_chan_for (int fd, unsigned short* chanp)
{
	char dev[VP_DEVNAM_LEN];
	struct dsc$descriptor_s d;
	unsigned int sts;
	int i, free_slot = -1;

	vp_init_cache();

	if (vp_devnam(fd, dev, sizeof(dev)) <= -1) return -1;

	for (i = 0; i < VP_CACHE_SIZE; i++)
	{
		if (g_chan[i].fd == fd)
		{
			/* the descriptor number may have been closed and handed out
			 * again for something else. the device name settles it. */
			if (strcmp(g_chan[i].dev, dev) == 0)
			{
				*chanp = g_chan[i].chan;
				return 0;
			}
			vp_drop(&g_chan[i]);
			free_slot = i;
			break;
		}
		if (g_chan[i].fd < 0 && free_slot < 0) free_slot = i;
	}

	d.dsc$w_length = (unsigned short)strlen(dev);
	d.dsc$b_dtype = DSC$K_DTYPE_T;
	d.dsc$b_class = DSC$K_CLASS_S;
	d.dsc$a_pointer = dev;

	sts = sys$assign(&d, chanp, 0, 0);
	if (!(sts & 1)) return -1;

	if (free_slot >= 0)
	{
		g_chan[free_slot].fd = fd;
		g_chan[free_slot].chan = *chanp;
		strcpy(g_chan[free_slot].dev, dev);
	}
	/* if the cache is full the channel is still usable, it just is not kept.
	 * it leaks until poll_vms_cleanup(), which is why the cache is sized
	 * well above the number of handles a vm realistically multiplexes. */

	return 0;
}

/* ------------------------------------------------------------------------ *
 * READINESS
 * ------------------------------------------------------------------------ */

/* queued message count, without taking anything off the queue */
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

/* is this a mailbox? anything else cannot be handled here. */
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

/* ------------------------------------------------------------------------ *
 * WAITING
 * ------------------------------------------------------------------------ */

static volatile int g_woken = 0;

/* [IMPORTANT] this runs as an AST, which interrupts the caller at an
 * arbitrary instruction. It does the least it possibly can - set a flag and
 * wake the process - for the same reason a signal handler does. */
static void vp_ast (int prm)
{
	(void)prm;
	g_woken = 1;
	sys$wake(0, 0); /* zero for both arguments means the calling process */
}

/* fill in revents for every descriptor. answers how many have something. */
static int vp_scan (struct pollfd* pfd, nfds_t nfd, unsigned short* chans)
{
	nfds_t i;
	int nready = 0;

	for (i = 0; i < nfd; i++)
	{
		int cnt;

		pfd[i].revents = 0;

		if (pfd[i].fd < 0) continue; /* the caller has disabled this one */

		if (chans[i] == 0)
		{
			pfd[i].revents = POLLNVAL;
			nready++;
			continue;
		}

		if ((pfd[i].events & POLLIN) && vp_msgcnt(chans[i], &cnt) == 0 && cnt > 0)
			pfd[i].revents |= POLLIN;

		/* see the header comment: writability is answered optimistically */
		if (pfd[i].events & POLLOUT) pfd[i].revents |= POLLOUT;

		if (pfd[i].revents) nready++;
	}

	return nready;
}

int poll (struct pollfd* pfd, nfds_t nfd, int timeout)
{
	unsigned short chans[VP_CACHE_SIZE];
	unsigned int timbuf[2];
	nfds_t i;
	int nready;
	int armed = 0;
	int timer_set = 0;

	if (nfd > VP_CACHE_SIZE)
	{
		/* the multiplexer never watches this many handles. raising
		 * VP_CACHE_SIZE is the fix if it ever does. */
		errno = EINVAL;
		return -1;
	}

	/* resolve a channel for each descriptor up front */
	for (i = 0; i < nfd; i++)
	{
		chans[i] = 0;
		if (pfd[i].fd < 0) continue;
		if (vp_chan_for(pfd[i].fd, &chans[i]) <= -1) { chans[i] = 0; continue; }
		if (!vp_is_mbx(chans[i])) chans[i] = 0; /* reported POLLNVAL by vp_scan */
	}

	nready = vp_scan(pfd, nfd, chans);
	if (nready > 0 || timeout == 0) return nready;

	/* nothing yet and the caller is prepared to wait. */
	g_woken = 0;

	for (i = 0; i < nfd; i++)
	{
		if (chans[i] == 0 || !(pfd[i].events & POLLIN)) continue;
		if (sys$qio(0, chans[i], IO$_SETMODE | IO$M_WRTATTN,
		            0, 0, 0, vp_ast, 0, 0, 0, 0, 0) & 1) armed++;
	}

	if (timeout > 0)
	{
		/* A delta time is a negative quadword counted in 100ns units. This
		 * platform has no 64-bit integer type at all - see lib/hak-vms.h -
		 * so the quadword is assembled from 32-bit halves by hand.
		 *
		 * magnitude = timeout * 10000, computed in 16-bit pieces so that no
		 * intermediate product can overflow 32 bits, then negated in two's
		 * complement across the pair. */
		unsigned int t, p0, p1, lo, hi;

		t = (unsigned int)timeout;
		p0 = (t & 0xFFFF) * 10000;   /* at most 65535 * 10000, fits */
		p1 = (t >> 16) * 10000;      /* likewise */

		lo = p0 + (p1 << 16);
		hi = (p1 >> 16) + ((lo < p0)? 1: 0); /* carry out of the low half */

		lo = ~lo + 1;                        /* negate the pair */
		hi = ~hi + ((lo == 0)? 1: 0);

		timbuf[0] = lo;
		timbuf[1] = hi;
		if (sys$setimr(0, timbuf, vp_ast, 0, 0) & 1) timer_set = 1;
	}

	/* An attention AST fires on the NEXT write, so anything queued between
	 * the scan above and the arming just done would not have woken us. Look
	 * again now that the ASTs are in place: if something arrived in that
	 * window it is found here, and if not, any later write is guaranteed to
	 * raise an AST. */
	nready = vp_scan(pfd, nfd, chans);

	if (nready <= 0 && (armed > 0 || timer_set))
	{
		while (!g_woken) sys$hiber();
		nready = vp_scan(pfd, nfd, chans);
	}

	/* take the attention ASTs back down and cancel the timer */
	for (i = 0; i < nfd; i++)
	{
		if (chans[i] == 0 || !(pfd[i].events & POLLIN)) continue;
		sys$cancel(chans[i]);
	}
	if (timer_set) sys$cantim(0, 0);

	return nready;
}
