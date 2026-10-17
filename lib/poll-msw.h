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

#ifndef _POLL_MSW_H_
#define _POLL_MSW_H_

#include <hak.h>

#if !defined(_WIN32_WINNT) || (_WIN32_WINNT < 0x0600)
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

/* Poll the file descriptors described by the NFDS structures starting at
   FDS.  If TIMEOUT is nonzero and not -1, allow TIMEOUT milliseconds for
   an event to occur; if TIMEOUT is -1, block until an event occurs.
   Returns the number of file descriptors with events, zero if timed out,
   or -1 for errors.  */
#endif

typedef unsigned long nfds_t;

/* Per-caller polling state.
 *
 * The emulation needs an auto-reset event to wait on, and WSAEventSelect()
 * binds every polled socket to it. A single process-wide event therefore
 * cannot serve two pollers at once: their sockets would signal the same
 * object, and an auto-reset event released by one waiter is not seen by the
 * other. Giving each caller its own context is what makes concurrent polling
 * possible - one context per thread, never shared. */
typedef struct hak_sys_poll_ctx_t hak_sys_poll_ctx_t;
struct hak_sys_poll_ctx_t
{
	/* a HANDLE to an auto-reset event. kept as void* so that this header
	 * need not include <windows.h>, whose ordering against <winsock2.h>
	 * matters to every file that includes us. */
	void* ev;
};

#if defined(__cplusplus)
extern "C" {
#endif

/* Prepare a context. Answers 0, or -1 with errno set. */
int hak_sys_poll_ctx_init (hak_sys_poll_ctx_t* ctx);

/* Release what the context holds. Safe on an already finalized context. */
void hak_sys_poll_ctx_fini (hak_sys_poll_ctx_t* ctx);

/* poll() against a caller-owned context. */
int hak_sys_poll (hak_sys_poll_ctx_t* ctx, struct pollfd *pfd, nfds_t nfd, int timeout);

/* Poll the file descriptors described by the NFDS structures starting at
   FDS.  If TIMEOUT is nonzero and not -1, allow TIMEOUT milliseconds for
   an event to occur; if TIMEOUT is -1, block until an event occurs.
   Returns the number of file descriptors with events, zero if timed out,
   or -1 for errors.

   [NOTE] this uses one shared context and is therefore not multi-thread
   safe, exactly as it has always been. Call it from a single thread, or
   give each thread a context of its own and call hak_sys_poll(). */
int poll (struct pollfd *pfd, nfds_t nfd, int timeout);

#if defined(__cplusplus)
}
#endif

#endif
