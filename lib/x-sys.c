
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

#include <hcl-x.h>
#include "hcl-prv.h"

#include <stdio.h>
#include <string.h>
#include <errno.h>

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
#	if defined(HAVE_SYS_UIO_H)
#		include <sys/uio.h>
#	endif

#	include <unistd.h>
#	include <fcntl.h>
#	include <sys/types.h>
#	include <sys/socket.h>
#endif

int hcl_sys_send (int sck, const void* data, hcl_oow_t* size)
{
	ssize_t n, seglen;
	hcl_oow_t rem;

	rem = *size;
	while (rem > 0)
	{
		seglen = (rem > HCL_TYPE_MAX(ssize_t))? HCL_TYPE_MAX(ssize_t): rem;
		n = send(sck, data, seglen, 0);
		if (n <= -1)
		{
			if (hcl_sys_is_errno_wb(errno)) break;
			*size -= rem; /* update the size to the bytes sent so far upon failure*/
			return -1;
		}

		/* i'm always paranoid about 0 returned by send. is it ever possible? */
		rem -= n;
	}

	*size -= rem; /* update the size to the bytes sent */
	return 0;
}

int hcl_sys_send_iov (int sck, hcl_iovec_t* iov, int count)
{
	int index = 0;

	while (1)
	{
		ssize_t nwritten;
		struct msghdr msg;

		HCL_MEMSET (&msg, 0, HCL_SIZEOF(msg));
		msg.msg_iov = (struct iovec*)&iov[index];
		msg.msg_iovlen = count - index;
		nwritten = sendmsg(sck, &msg, 0);
		if (nwritten <= -1)
		{
			if (hcl_sys_is_errno_wb(errno))
			{
				/* the incompelete write. the caller shall check the return code
				 * and iov_len at the last written iov slot. */
				break;
			}
			return -1;
		}

		while (index < count && (size_t)nwritten >= iov[index].iov_len)
		{
			iov[index].iov_len = 0; /* this slot has been fully written */
			nwritten -= iov[index++].iov_len;
		}

		if (index == count) break;

		iov[index].iov_base = (void*)((hcl_uint8_t*)iov[index].iov_base + nwritten);
		iov[index].iov_len -= nwritten;
	}

	return index;
}


int hcl_sys_open_pipes (int pfd[2], int nonblock)
{
	/* TODO: mimic open_pipes() in std.c */

	if (pipe(pfd) <= -1) return -1;

	hcl_sys_set_nonblock(pfd[0], nonblock);
	hcl_sys_set_nonblock(pfd[1], nonblock);
	hcl_sys_set_cloexec(pfd[0], 1);
	hcl_sys_set_cloexec(pfd[1], 1);

	return 0;
}

void hcl_sys_close_pipes (int pfd[2])
{
	if (pfd[0] >= 0)
	{
		close (pfd[0]);
		pfd[0] = -1;
	}
	if (pfd[1] >= 0)
	{
		close (pfd[1]);
		pfd[1] = -1;
	}
}

int hcl_sys_set_nonblock (int fd, int v)
{
#if defined(F_GETFL) && defined(F_SETFL) && defined(O_NONBLOCK)
	int flags;

	if ((flags = fcntl(fd, F_GETFL, 0)) <= -1) return -1;

	if (v) flags |= O_NONBLOCK;
	else flags &= ~O_NONBLOCK;

	if (fcntl(fd, F_SETFL, flags) <= -1) return -1;

	return 0;
#else
	errno = ENOSYS;
	return -1;
#endif
}

int hcl_sys_set_cloexec (int fd, int v)
{
#if defined(F_GETFL) && defined(F_SETFL) && defined(FD_CLOEXEC)
	int flags;

	if ((flags = fcntl(fd, F_GETFD, 0)) <= -1) return -1;

	if (v) flags |= FD_CLOEXEC;
	else flags &= ~FD_CLOEXEC;

	if (fcntl(fd, F_SETFD, flags) <= -1) return -1;

	return 0;
#else
	errno = ENOSYS;
	return -1;
#endif
}

int hcl_sys_is_errno_wb (int no)
{
	#if defined(EWOULDBLOCK) && defined(EAGAIN) && (EWOULDBLOCK != EAGAIN)
		return no == EWOULDBLOCK || no == EAGAIN;
	#elif defined(EWOULDBLOCK)
		return no == EWOULDBLOCK;
	#elif defined(EAGAIN)
		return no == EAGAIN;
	#endif
}
