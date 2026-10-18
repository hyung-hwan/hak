
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
#include "hak-prv.h"

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
#	if defined(__VMS)
		/* FIONBIO, decc$get_sdc() for telling a socket from anything else,
		 * and the loopback pair that stands in for socketpair() */
#		include <sys/ioctl.h>
#		include <netinet/in.h>
#	endif
#endif

int hak_sys_send (int sck, const void* data, hak_oow_t* size)
{
	ssize_t n, seglen;
	hak_oow_t rem;

	rem = *size;
	while (rem > 0)
	{
		seglen = (rem > HAK_TYPE_MAX(ssize_t))? HAK_TYPE_MAX(ssize_t): rem;
		n = send(sck, data, seglen, 0);
		if (n <= -1)
		{
			if (hak_sys_is_errno_wb(errno)) break;
			*size -= rem; /* update the size to the bytes sent so far upon failure*/
			return -1;
		}

		/* i'm always paranoid about 0 returned by send. is it ever possible? */
		rem -= n;
	}

	*size -= rem; /* update the size to the bytes sent */
	return 0;
}

int hak_sys_send_iov (int sck, hak_iovec_t* iov, int count)
{
	int index = 0;

	while (1)
	{
		ssize_t nwritten;
		struct msghdr msg;

		HAK_MEMSET(&msg, 0, HAK_SIZEOF(msg));
		msg.msg_iov = (struct iovec*)&iov[index];
		msg.msg_iovlen = count - index;
		nwritten = sendmsg(sck, &msg, 0);
		if (nwritten <= -1)
		{
			if (hak_sys_is_errno_wb(errno))
			{
				/* the incompelete write. the caller shall check the return code
				 * and iov_len at the last written iov slot. */
				break;
			}
			return -1;
		}

		while (index < count && (size_t)nwritten >= iov[index].iov_len)
		{
			nwritten -= iov[index].iov_len;
			iov[index++].iov_len = 0; /* this slot has been fully written */
		}

		if (index == count) break;

		iov[index].iov_base = (void*)((hak_uint8_t*)iov[index].iov_base + nwritten);
		iov[index].iov_len -= nwritten;
	}

	return index;
}


#if defined(__VMS)
/* A socket pair for the wake channel, since OpenVMS offers no other way to
 * get one that behaves.
 *
 * pipe() here is built from a mailbox, and a mailbox has no non-blocking
 * mode - fcntl() is not implemented for it and FIONBIO is a socket call - so
 * a wake channel made from pipe() cannot be read without the risk of
 * blocking, and it is a different device class from the sockets it sits
 * beside in the poll set. socketpair() is no help: the symbol links but
 * faults at address 0, so it is not implemented.
 *
 * A loopback connection gives what is wanted - FIONBIO works, an empty read
 * answers EWOULDBLOCK, and select() sees it alongside the listening sockets.
 *
 * [NOTE] the listener is bound to 127.0.0.1 on a port the stack picks, takes
 * a backlog of one, and is closed the moment the pair is up, so the window in
 * which anything else could connect is as small as it can be made. It is
 * still a window: on a host running untrusted local code, a racing connect()
 * could be accepted in our place. That is the same trade the equivalent trick
 * makes on windows. */
static int vms_socket_pair (int pfd[2])
{
	int lsck = -1, csck = -1, asck = -1, on = 1;
	struct sockaddr_in sa;
	hak_scklen_t slen;

	lsck = socket(AF_INET, SOCK_STREAM, 0);
	if (lsck <= -1) goto oops;

	HAK_MEMSET(&sa, 0, HAK_SIZEOF(sa));
	sa.sin_family = AF_INET;
	sa.sin_port = 0; /* the stack picks a free one */
	sa.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	setsockopt(lsck, SOL_SOCKET, SO_REUSEADDR, &on, HAK_SIZEOF(on));

	if (bind(lsck, (struct sockaddr*)&sa, HAK_SIZEOF(sa)) <= -1) goto oops;

	/* read the port back - bind() was given 0 */
	slen = HAK_SIZEOF(sa);
	if (getsockname(lsck, (struct sockaddr*)&sa, &slen) <= -1) goto oops;

	if (listen(lsck, 1) <= -1) goto oops;

	csck = socket(AF_INET, SOCK_STREAM, 0);
	if (csck <= -1) goto oops;
	if (connect(csck, (struct sockaddr*)&sa, HAK_SIZEOF(sa)) <= -1) goto oops;

	/* the length must match the family exactly - OpenVMS rejects a buffer
	 * larger than the family's own sockaddr */
	slen = HAK_SIZEOF(sa);
	asck = accept(lsck, (struct sockaddr*)&sa, &slen);
	if (asck <= -1) goto oops;

	close(lsck);
	pfd[0] = asck; /* read end  - the one the poll set watches */
	pfd[1] = csck; /* write end - the one a nudge is written to */
	return 0;

oops:
	if (asck >= 0) close(asck);
	if (csck >= 0) close(csck);
	if (lsck >= 0) close(lsck);
	return -1;
}
#endif

int hak_sys_open_pipes (int pfd[2], int nonblock)
{
	/* TODO: mimic open_pipes() in std.c */

#if defined(__VMS)
	if (vms_socket_pair(pfd) <= -1) return -1;
#else
	if (pipe(pfd) <= -1) return -1;
#endif

	hak_sys_set_nonblock(pfd[0], nonblock);
	hak_sys_set_nonblock(pfd[1], nonblock);
	hak_sys_set_cloexec(pfd[0], 1);
	hak_sys_set_cloexec(pfd[1], 1);

	return 0;
}

void hak_sys_close_pipes (int pfd[2])
{
	if (pfd[0] >= 0)
	{
		close(pfd[0]);
		pfd[0] = -1;
	}
	if (pfd[1] >= 0)
	{
		close(pfd[1]);
		pfd[1] = -1;
	}
}

int hak_sys_set_nonblock (int fd, int v)
{
#if defined(__VMS)
	/* fcntl() does not work on an OpenVMS socket. F_GETFL answers -1/EINVAL
	 * and F_SETFL answers -1/ENOSYS, so the fcntl arm below would leave the
	 * descriptor blocking while reporting nothing wrong - which is how the
	 * listening socket in hak_server_start() came to block in accept() before
	 * this new vms arm was implemented. FIONBIO is the call the TCP/IP services
	 * do implement.
	 *
	 * a non-socket descriptor has no equivalent here: the run-time has no
	 * non-blocking mode for a mailbox. that is reported as a failure. */
	int on = !!v;

	if (decc$get_sdc(fd) == 0)
	{
		errno = ENOTSOCK;
		return -1;
	}

	if (ioctl(fd, FIONBIO, &on) == -1) return -1;

	return 0;
#elif defined(F_GETFL) && defined(F_SETFL) && defined(O_NONBLOCK)
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

int hak_sys_set_cloexec (int fd, int v)
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

int hak_sys_is_errno_wb (int no)
{
	#if defined(EWOULDBLOCK) && defined(EAGAIN) && (EWOULDBLOCK != EAGAIN)
		return no == EWOULDBLOCK || no == EAGAIN;
	#elif defined(EWOULDBLOCK)
		return no == EWOULDBLOCK;
	#elif defined(EAGAIN)
		return no == EAGAIN;
	#endif
}
