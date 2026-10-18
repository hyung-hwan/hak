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

#ifndef _POLL_VMS_H_
#define _POLL_VMS_H_

#include <hak.h>

/* OpenVMS has no poll(). This supplies one so that std.c's USE_POLL
 * multiplexer works unchanged, the same arrangement lib/poll-msw.h uses for
 * windows.
 *
 * It covers three kinds of descriptor, because hak multiplexes all three in
 * one set - the server watches its listening sockets alongside the wake pipe,
 * and the client watches the remote socket alongside the wake pipe and the
 * script file:
 *
 *   socket  (a BG device of the TCP/IP services)
 *   mailbox (the C run-time implements pipe() with one, so a pipe descriptor
 *            and anything from sys.pipe lands here)
 *   regular file (always ready, which is what poll() reports for one anyway)
 *
 * Anything else - a terminal, say - is reported POLLNVAL rather than guessed
 * at. See lib/poll-vms.c for how the three are told apart and why neither of
 * the two primitives OpenVMS offers could have covered the set on its own. */

/* Event types that can be polled for.  These bits may be set in `events'
   to indicate the interesting event types; they will appear in `revents'
   to indicate the status of the file descriptor.  */
#define POLLIN          0x001           /* There is data to read.  */
#define POLLPRI         0x002           /* There is urgent data to read.  */
#define POLLOUT         0x004           /* Writing now will not block.  */

/* Event types always implicitly polled for.  These bits need not be set in
   `events', but they will appear in `revents' to indicate the status of
   the file descriptor.  */
#define POLLERR         0x008           /* Error condition.  */
#define POLLHUP         0x010           /* Hung up.  */
#define POLLNVAL        0x020           /* Invalid polling request.  */

/* Data structure describing a polling request.  */
struct pollfd
{
	int fd;                     /* File descriptor to poll.  */
	short int events;           /* Types of events poller cares about.  */
	short int revents;          /* Types of events that actually occurred.  */
};

typedef unsigned long nfds_t;

/* The cache holds one assigned channel per mailbox descriptor, and doubles as
 * the ceiling on how many descriptors a single call may watch. */
#define HAK_SYS_POLL_MAX_FDS 64
#define HAK_SYS_POLL_DEVNAM_LEN 64

typedef struct hak_sys_poll_chan_t hak_sys_poll_chan_t;
struct hak_sys_poll_chan_t
{
	int fd;                              /* -1 when the slot is free */
	unsigned short chan;
	char dev[HAK_SYS_POLL_DEVNAM_LEN];
};

/* Per-caller polling state.
 *
 * The channel cache is mutable - a caller may sys$dassgn a slot another one is
 * in the middle of a sys$qio on - so it cannot be shared. Only mailbox
 * channels are cached: a socket's channel comes from decc$get_sdc() on every
 * pass, which is a C run-time lookup rather than a system service, and is
 * owned by the run-time rather than by us.
 *
 * The event flag is private too. A caller clears it, arms its attention ASTs,
 * then blocks on sys$waitfr() until one of its own ASTs sets it. Because the
 * flag is allocated per context with lib$get_ef(), two contexts cannot steal
 * each other's wakeups - which the earlier sys$wake(0,0)/sys$hiber() pairing
 * could not promise, since sys$wake wakes the process and not a thread.
 *
 * [NOTE] on VAX, DECthreads are user mode only, so sys$waitfr() still suspends
 * every thread in the process while it waits. That is a property of the thread
 * library rather than of this code, and it is no worse than the sys$hiber()
 * that came before, but it does mean a thread-per-connection design gains
 * nothing on that hardware. */
typedef struct hak_sys_poll_ctx_t hak_sys_poll_ctx_t;

struct hak_sys_poll_ctx_t
{
	hak_sys_poll_chan_t chan[HAK_SYS_POLL_MAX_FDS];
	unsigned int efn;                    /* from lib$get_ef(), 0 when unset */
	int inited;
};

#if defined(__cplusplus)
extern "C" {
#endif

/* Prepare a context. Answers 0, or -1 with errno set. */
int hak_sys_poll_ctx_init (hak_sys_poll_ctx_t* ctx);

/* Release the channels the context has assigned. Safe to call more than once,
   and safe on a context that was only zeroed. */
void hak_sys_poll_ctx_fini (hak_sys_poll_ctx_t* ctx);

/* poll() against a caller-owned context. */
int hak_sys_poll (hak_sys_poll_ctx_t* ctx, struct pollfd* pfd, nfds_t nfd, int timeout);

/* Poll the file descriptors described by the NFDS structures starting at
   FDS.  If TIMEOUT is nonzero and not -1, allow TIMEOUT milliseconds for
   an event to occur; if TIMEOUT is -1, block until an event occurs.
   Returns the number of file descriptors with events, zero if timed out,
   or -1 for errors.

   [NOTE] this drives one shared context and is therefore single-threaded,
   exactly as it has always been. Give each thread a context of its own and
   call hak_sys_poll() instead. */
int poll (struct pollfd* pfd, nfds_t nfd, int timeout);

/* Release the channels the shared context behind poll() has assigned.
   Optional - the channels go away when the image exits - but hak calls it
   from its cleanup so that a long-lived embedder does not accumulate them. */
void poll_vms_cleanup (void);

#if defined(__cplusplus)
}
#endif

#endif
