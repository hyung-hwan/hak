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

#ifndef _HAK_POLL_PRV_H_
#define _HAK_POLL_PRV_H_

/* A portable poll().
 *
 * Windows and OpenVMS have no poll() of their own. lib/poll-msw.c and
 * lib/poll-vms.c supply one, and each carries a per-caller context because
 * its emulation holds state that two callers cannot share - an auto-reset
 * event bound to every polled socket on windows, a channel cache and a wake
 * flag on VMS. A platform with a real poll() needs none of that, so the
 * context collapses to an empty struct and hak_sys_poll() is the system call.
 *
 * Spelling it the same way everywhere is the point: a caller declares a
 * context, initializes it, polls with it and finalizes it, with no platform
 * conditionals of its own. One context per thread - never shared.
 *
 *     hak_sys_poll_ctx_t ctx;
 *     if (hak_sys_poll_ctx_init(&ctx) <= -1) ...
 *     n = hak_sys_poll(&ctx, pfd, nfd, timeout_ms);
 *     hak_sys_poll_ctx_fini(&ctx);
 */

#if defined(_WIN32)
#	include "poll-msw.h"
#elif defined(__VMS)
#	include "poll-vms.h"
#else
#	include <poll.h>

typedef struct hak_sys_poll_ctx_t hak_sys_poll_ctx_t;
struct hak_sys_poll_ctx_t
{
	int unused_;
};

#	define hak_sys_poll_ctx_init(ctx) (0)
#	define hak_sys_poll_ctx_fini(ctx) ((void)0)
#	define hak_sys_poll(ctx,pfd,nfd,tmout) poll(pfd,nfd,tmout)
#endif

#endif
