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

/* OpenVMS has no poll(), and its select() comes from the TCP/IP services and
 * understands sockets only - it answers ENOTSOCK for anything else. This
 * supplies a poll() over $QIO so that std.c's USE_POLL multiplexer works
 * unchanged, the same arrangement lib/poll-msw.h uses for windows.
 *
 * It covers mailbox devices. That is the useful case rather than a limitation:
 * the C run-time implements pipe() with a mailbox, so a pipe descriptor - the
 * signal self-pipe in std.c, anything from sys.pipe - is exactly what this
 * handles. A descriptor that is not a mailbox is reported POLLNVAL. */

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

/* The cache holds one assigned channel per descriptor, and doubles as the
 * ceiling on how many descriptors a single call may watch. */
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
 * Two things here cannot be shared. The channel cache is mutable - a caller
 * may sys$dassgn a slot another one is in the middle of a sys$qio on. And the
 * wake flag pairs with sys$hiber(): a caller clears it, arms its ASTs, then
 * sleeps until its own AST sets it. One flag between two sleepers loses
 * wakeups. One context per thread, never shared.
 *
 * [NOTE] a context makes the state private, but sys$wake(0,0) still wakes the
 * process rather than a thread, so a second sleeper can consume the wake meant
 * for the first. Concurrent pollers need a per-thread wait primitive - an event
 * flag with sys$waitfr, say - before that is fully solved. Until then this is
 * sound for one poller at a time, which is what hak does. */
typedef struct hak_sys_poll_ctx_t hak_sys_poll_ctx_t;

struct hak_sys_poll_ctx_t
{
	hak_sys_poll_chan_t chan[HAK_SYS_POLL_MAX_FDS];
	int inited;
	volatile int woken;
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
