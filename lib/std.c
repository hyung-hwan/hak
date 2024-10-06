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

#if !defined(_GNU_SOURCE)
#	define _GNU_SOURCE
#endif

#include "hcl-prv.h"
#include <hcl-utl.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if !defined(__DOS__) && !defined(EMSCRIPTEN) && defined(HAVE_PTHREAD) && defined(HAVE_STRERROR_R)
#	define USE_THREAD
#endif

#if defined(__BORLANDC__) && defined(__DOS__)  && defined(_WIN32) && defined(__DPMI32__)
/* bcc32 with powerpack seems to define _WIN32 in DPMI32 mode */
#	undef _WIN32
#endif

#if defined(_WIN32)
#	if !defined(_WIN32_WINNT)
#		define _WIN32_WINNT 0x0400
#	endif
#	define WIN32_LEAN_AND_MEAN

#	include <winsock2.h>
#	include <ws2tcpip.h>

#	include <windows.h>
#	if !(defined(__BORLANDC__) && (__BORLANDC__ <= 0x0520))
#		include <psapi.h>
#	endif
#	include <tchar.h>
#	include <time.h>
#	include <io.h>
#	include <fcntl.h>
#	include <signal.h>
#	include <errno.h>
#	if defined(HCL_HAVE_CFG_H) && defined(HCL_ENABLE_LIBLTDL)
#		include <ltdl.h>
#		define USE_LTDL
#	else
#		define USE_WIN_DLL
#	endif

#	include "poll-msw.h"
#	define USE_POLL
#	define XPOLLIN POLLIN
#	define XPOLLOUT POLLOUT
#	define XPOLLERR POLLERR
#	define XPOLLHUP POLLHUP

#if !defined(SIZE_T)
#	define SIZE_T unsigned long int
#endif

#elif defined(__OS2__)
#	define INCL_DOSMODULEMGR
#	define INCL_DOSPROCESS
#	define INCL_DOSSEMAPHORES
#	define INCL_DOSEXCEPTIONS
#	define INCL_DOSMISC
#	define INCL_DOSDATETIME
#	define INCL_DOSFILEMGR
#	define INCL_DOSERRORS
#	include <os2.h>
#	include <time.h>
#	include <fcntl.h>
#	include <io.h>
#	include <errno.h>

#	include <types.h> /* some types for socket.h */
#	include <sys/socket.h> /* for socketpair */
#	include <sys/time.h>
#	include <sys/ioctl.h> /* FIONBIO */
#	include <nerrno.h> /* for SOCEXXX error codes */

#	define BSD_SELECT
#	if defined(TCPV40HDRS)
#	include <sys/select.h>
#	include <sys/un.h> /* for sockaddr_un */
#	else
#	include <unistd.h>
#	endif

#	define USE_SELECT
	/* fake XPOLLXXX values */
#	define XPOLLIN  (1 << 0)
#	define XPOLLOUT (1 << 1)
#	define XPOLLERR (1 << 2)
#	define XPOLLHUP (1 << 3)

#elif defined(__DOS__)
#	include <dos.h>
#	include <time.h>
#	include <io.h>
#	include <signal.h>
#	include <errno.h>
#	include <fcntl.h>
#	include <conio.h> /* inp, outp */

#	if defined(_INTELC32_)
#		define DOS_EXIT 0x4C
#		include <i32.h>
#		include <stk.h>
#	elif defined(_MSC_VER)
#		include <malloc.h>
#		define malloc(x) halloc(x, 1)
#		define free(x) hfree(x)
#	elif defined(__WATCOMC__)
#		include <dosfunc.h>
#	endif

	/* fake XPOLLXXX values */
#	define XPOLLIN  (1 << 0)
#	define XPOLLOUT (1 << 1)
#	define XPOLLERR (1 << 2)
#	define XPOLLHUP (1 << 3)

#	if !defined(STDOUT_FILENO)
#		define STDOUT_FILENO (1)
#	endif

#	if !defined(STDERR_FILENO)
#		define STDERR_FILENO (2)
#	endif

#elif defined(macintosh)
#	include <Types.h>
#	include <OSUtils.h>
#	include <Timer.h>

#	include <MacErrors.h>
#	include <Process.h>
#	include <Dialogs.h>
#	include <TextUtils.h>

#else

#	include <sys/types.h>
#	include <unistd.h>
#	include <fcntl.h>
#	include <errno.h>

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

#	if defined(HCL_ENABLE_LIBLTDL)
#		include <ltdl.h>
#		define USE_LTDL
#	elif defined(HAVE_DLFCN_H)
#		include <dlfcn.h>
#		define USE_DLFCN
#	elif defined(__APPLE__) || defined(__MACOSX__)
#		define USE_MACH_O_DYLD
#		include <mach-o/dyld.h>
#	else
#		error UNSUPPORTED DYNAMIC LINKER
#	endif

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

#	if defined(USE_THREAD)
#		include <pthread.h>
#		include <sched.h>
#	endif

#	if defined(HAVE_SYS_DEVPOLL_H)
		/* solaris */
#		include <sys/devpoll.h>
#		define USE_DEVPOLL
#		define XPOLLIN POLLIN
#		define XPOLLOUT POLLOUT
#		define XPOLLERR POLLERR
#		define XPOLLHUP POLLHUP
#	elif defined(HAVE_SYS_EVENT_H) && defined(HAVE_KQUEUE)
		/* netbsd, openbsd, etc */
#		include <sys/event.h>
#		define USE_KQUEUE
		/* fake XPOLLXXX values */
#		define XPOLLIN  (1 << 0)
#		define XPOLLOUT (1 << 1)
#		define XPOLLERR (1 << 2)
#		define XPOLLHUP (1 << 3)
#	elif defined(HAVE_SYS_EPOLL_H) && defined(HAVE_EPOLL_CREATE)
		/* linux */
#		include <sys/epoll.h>
#		define USE_EPOLL
#		define XPOLLIN EPOLLIN
#		define XPOLLOUT EPOLLOUT
#		define XPOLLERR EPOLLERR
#		define XPOLLHUP EPOLLHUP
#	elif defined(HAVE_POLL_H)
#		include <poll.h>
#		define USE_POLL
#		define XPOLLIN POLLIN
#		define XPOLLOUT POLLOUT
#		define XPOLLERR POLLERR
#		define XPOLLHUP POLLHUP
#	else
#		define USE_SELECT
		/* fake XPOLLXXX values */
#		define XPOLLIN  (1 << 0)
#		define XPOLLOUT (1 << 1)
#		define XPOLLERR (1 << 2)
#		define XPOLLHUP (1 << 3)
#	endif
#endif

#if !defined(HCL_DEFAULT_PFMODDIR)
#	define HCL_DEFAULT_PFMODDIR ""
#endif

#if !defined(HCL_DEFAULT_PFMODPREFIX)
#	if defined(_WIN32)
#		define HCL_DEFAULT_PFMODPREFIX "hcl-"
#	elif defined(__OS2__)
#		define HCL_DEFAULT_PFMODPREFIX "hcl"
#	elif defined(__DOS__)
#		define HCL_DEFAULT_PFMODPREFIX "hcl"
#	else
#		define HCL_DEFAULT_PFMODPREFIX "libhcl-"
#	endif
#endif

#if !defined(HCL_DEFAULT_PFMODPOSTFIX)
#	if defined(_WIN32)
#		define HCL_DEFAULT_PFMODPOSTFIX ""
#	elif defined(__OS2__)
#		define HCL_DEFAULT_PFMODPOSTFIX ""
#	elif defined(__DOS__)
#		define HCL_DEFAULT_PFMODPOSTFIX ""
#	else
#		if defined(USE_DLFCN)
#			define HCL_DEFAULT_PFMODPOSTFIX ".so"
#		elif defined(USE_MACH_O_DYLD)
#			define HCL_DEFAULT_PFMODPOSTFIX ".dylib"
#		else
#			define HCL_DEFAULT_PFMODPOSTFIX ""
#		endif
#	endif
#endif

#if defined(USE_THREAD)
#	define MUTEX_INIT(x) pthread_mutex_init((x), HCL_NULL)
#	define MUTEX_DESTROY(x) pthread_mutex_destroy(x)
#	define MUTEX_LOCK(x) pthread_mutex_lock(x)
#	define MUTEX_UNLOCK(x) pthread_mutex_unlock(x)
#else
#	define MUTEX_INIT(x)
#	define MUTEX_DESTROY(x)
#	define MUTEX_LOCK(x)
#	define MUTEX_UNLOCK(x)
#endif

#if defined(USE_SELECT)
struct select_fd_t
{
	int fd;
	int events;
};
#endif

typedef struct xtn_t xtn_t;
struct xtn_t
{
	hcl_t* next;
	hcl_t* prev;

	/* hcl_attachiostdwithbcstr() and hcl_attachiostdwithucstr()
	 * set these two field and reset them at the end.
	 * since hcl_attachio() callls the open handler, these fields
	 * are valid only inside the open handelr */
	const char* cci_path; /* main source file */
	const char* udi_path; /* runtime input file */
	const char* udo_path; /* runtime output file */

	int vm_running;
	int rcv_tick;

	hcl_cmgr_t* input_cmgr;
	hcl_cmgr_t* log_cmgr;

	struct
	{
		int fd;
		int fd_flags; /* bitwise OR'ed fo logfd_flag_t bits */

		struct
		{
			hcl_bch_t buf[4096];
			hcl_oow_t len;
		} out;
	} log;

	#if defined(_WIN32)
	HANDLE waitable_timer;
	DWORD tc_last;
	DWORD tc_overflow;
	#elif defined(__OS2__)
	ULONG tc_last;
	ULONG tc_overflow;
	hcl_ntime_t tc_last_ret;
	#elif defined(__DOS__)
	clock_t tc_last;
	hcl_ntime_t tc_last_ret;
	#endif

	#if defined(USE_DEVPOLL)
	int ep; /* /dev/poll */
	#elif defined(USE_KQUEUE)
	int ep; /* kqueue */
	#elif defined(USE_EPOLL)
	int ep; /* epoll */
	#elif defined(USE_POLL)
	/* nothing */
	#elif defined(USE_SELECT)
	/* nothing */
	#endif

	#if defined(USE_THREAD)
	struct
	{
		int p[2]; /* pipe for signaling */
		pthread_t thr;
		int up;
		int abort;
	} iothr;
	#endif

	struct
	{
		int p[2]; /* pipe for signaling */
	} sigfd;

	struct
	{
	#if defined(USE_DEVPOLL)
		/*TODO: make it dynamically changeable depending on the number of
		 *      file descriptors added */
		struct pollfd buf[64]; /* buffer for reading events */
	#elif defined(USE_KQUEUE)
		struct
		{
			hcl_oow_t* ptr;
			hcl_oow_t capa;
		} reg;
		struct kevent buf[64];
	#elif defined(USE_EPOLL)
		/*TODO: make it dynamically changeable depending on the number of
		 *      file descriptors added */
		struct epoll_event buf[64]; /* buffer for reading events */
	#elif defined(USE_POLL)
		struct
		{
			struct pollfd* ptr;
			hcl_oow_t capa;
			hcl_oow_t len;
		#if defined(USE_THREAD)
			pthread_mutex_t pmtx;
		#endif
		} reg; /* registry */
		struct pollfd* buf;

	#elif defined(USE_SELECT)
		struct
		{
			fd_set rfds;
			fd_set wfds;
			int maxfd;
		#if defined(USE_THREAD)
			pthread_mutex_t smtx;
		#endif
		} reg;

		struct select_fd_t buf[FD_SETSIZE];
	#endif

		hcl_oow_t len;

	#if defined(USE_THREAD)
		pthread_mutex_t mtx;
		pthread_cond_t cnd;
		pthread_cond_t cnd2;
	#endif

		int halting;
	} ev;
};

#define GET_XTN(hcl) ((xtn_t*)((hcl_uint8_t*)HCL_XTN(hcl) - HCL_SIZEOF(xtn_t)))

static hcl_t* g_hcl = HCL_NULL;

/* -----------------------------------------------------------------
 * BASIC MEMORY MANAGER
 * ----------------------------------------------------------------- */

static void* sys_allocmem (hcl_mmgr_t* mmgr, hcl_oow_t size)
{
	return malloc(size);
}

static void* sys_reallocmem (hcl_mmgr_t* mmgr, void* ptr, hcl_oow_t size)
{
	return realloc(ptr, size);
}

static void sys_freemem (hcl_mmgr_t* mmgr, void* ptr)
{
	free (ptr);
}

static hcl_mmgr_t sys_mmgr =
{
	sys_allocmem,
	sys_reallocmem,
	sys_freemem,
	HCL_NULL
};

/* -----------------------------------------------------------------
 * HEAP ALLOCATION
 * ----------------------------------------------------------------- */

static int get_huge_page_size (hcl_t* hcl, hcl_oow_t* page_size)
{
	FILE* fp;
	char buf[256];

	fp = fopen("/proc/meminfo", "r");
	if (!fp) return -1;

	while (!feof(fp))
	{
		if (fgets(buf, sizeof(buf) - 1, fp) == NULL) goto oops;

		if (strncmp(buf, "Hugepagesize: ", 13) == 0)
		{
			unsigned long int tmp;
			tmp = strtoul(&buf[13], NULL, 10);
			if (tmp == HCL_TYPE_MAX(unsigned long int) && errno == ERANGE) goto oops;

			*page_size = tmp * 1024; /* KBytes to Bytes */
			fclose (fp);
			return 0;
		}
	}

oops:
	fclose (fp);
	return -1;
}

static void* alloc_heap (hcl_t* hcl, hcl_oow_t* size)
{
#if defined(_WIN32)
	hcl_oow_t* ptr;
	hcl_oow_t req_size, align, aligned_size;
	HINSTANCE k32;
	SIZE_T (*k32_GetLargePageMinimum) (void);
	HANDLE token = HCL_NULL;
	TOKEN_PRIVILEGES new_state, prev_state;
	TOKEN_PRIVILEGES* prev_state_ptr;
	DWORD prev_state_reqsize = 0;
	int token_adjusted = 0;

	align = 2 * 1024 * 1024; /* default 2MB */

	k32 = LoadLibrary(TEXT("kernel32.dll"));
	if (k32)
	{
		k32_GetLargePageMinimum = (SIZE_T(*)(void))GetProcAddress (k32, "GetLargePageMinimum");
		if (k32_GetLargePageMinimum) align = k32_GetLargePageMinimum();
		FreeLibrary (k32);
	}
	/* the standard page size shouldn't help. so let me comment out this part.
	else
	{
		SYSTEM_INFO si;
		GetSystemInfo (&si);
		align = si.dwPageSize;
	}*/

	req_size = HCL_SIZEOF(hcl_oow_t) + size;
	aligned_size = HCL_ALIGN(req_size, align);

	if (!OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &token)) goto oops;
	if (!LookupPrivilegeValue(HCL_NULL, TEXT("SeLockMemoryPrivilege"), &new_state.Privileges[0].Luid)) goto oops;
	new_state.PrivilegeCount = 1;
	new_state.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;

	prev_state_ptr = &prev_state;
	if (!AdjustTokenPrivileges(token, FALSE, &new_state, HCL_SIZEOF(prev_state), prev_state_ptr, &prev_state_reqsize) || GetLastError() != ERROR_SUCCESS)
	{
		if (prev_state_reqsize >= HCL_SIZEOF(prev_state))
		{
			/* GetLastError() == ERROR_INSUFFICIENT_BUFFER */
			prev_state_ptr = (TOKEN_PRIVILEGES*)HeapAlloc(GetProcessHeap(), 0, prev_state_reqsize);
			if (!prev_state_ptr) goto oops;
			if (!AdjustTokenPrivileges(token, FALSE, &new_state, prev_state_reqsize, prev_state_ptr, &prev_state_reqsize) || GetLastError() != ERROR_SUCCESS) goto oops;
		}
		else goto oops;
	}
	token_adjusted = 1;

#if !defined(MEM_LARGE_PAGES)
#	define MEM_LARGE_PAGES (0x20000000)
#endif
	ptr = VirtualAlloc(HCL_NULL, aligned_size, MEM_COMMIT | MEM_RESERVE | MEM_LARGE_PAGES, PAGE_READWRITE);
	if (!ptr)
	{
		SYSTEM_INFO si;
		GetSystemInfo (&si);
		align = si.dwPageSize;
		aligned_size = HCL_ALIGN(req_size, align);
		ptr = VirtualAlloc(HCL_NULL, aligned_size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
		if (!ptr) goto oops;
	}

	AdjustTokenPrivileges (token, FALSE, prev_state_ptr, 0, HCL_NULL, 0);
	CloseHandle (token);
	if (prev_state_ptr && prev_state_ptr != &prev_state) HeapFree (GetProcessHeap(), 0, prev_state_ptr);

	*size = aligned_size;
	return ptr;

oops:
	hcl_seterrwithsyserr (hcl, 1, GetLastError());
	if (token)
	{
		if (token_adjusted) AdjustTokenPrivileges (token, FALSE, prev_state_ptr, 0, HCL_NULL, 0);
		CloseHandle (token);
	}
	if (prev_state_ptr && prev_state_ptr != &prev_state) HeapFree (GetProcessHeap(), 0, prev_state_ptr);
	return HCL_NULL;

#elif defined(HAVE_MMAP) && defined(HAVE_MUNMAP) && defined(MAP_ANONYMOUS)
	/* It's called via hcl_makeheap() when HCL creates a GC heap.
	 * The heap is large in size. I can use a different memory allocation
	 * function instead of an ordinary malloc.
	 * upon failure, it doesn't require to set error information as hcl_makeheap()
	 * set the error number to HCL_EOOMEM. */

#if !defined(MAP_HUGETLB) && (defined(__amd64__) || defined(__x86_64__))
#	define MAP_HUGETLB 0x40000
#endif

	hcl_oow_t* ptr;
	int flags;
	hcl_oow_t req_size, align, aligned_size;

	req_size = HCL_SIZEOF(hcl_oow_t) + *size;
	flags = MAP_PRIVATE | MAP_ANONYMOUS;

	#if defined(MAP_UNINITIALIZED)
	flags |= MAP_UNINITIALIZED;
	#endif

	#if defined(MAP_HUGETLB)
	if (get_huge_page_size(hcl, &align) <= -1) align = 2 * 1024 * 1024; /* default to 2MB */
	if (req_size > align / 2)
	{
		/* if the requested size is large enough, attempt HUGETLB */
		flags |= MAP_HUGETLB;
	}
	else
	{
		align = sysconf(_SC_PAGESIZE);
	}
	#else
	align = sysconf(_SC_PAGESIZE);
	#endif

	aligned_size = HCL_ALIGN_POW2(req_size, align);
	ptr = (hcl_oow_t*)mmap(NULL, aligned_size, PROT_READ | PROT_WRITE, flags, -1, 0);
	#if defined(MAP_HUGETLB)
	if (ptr == MAP_FAILED && (flags & MAP_HUGETLB))
	{
		flags &= ~MAP_HUGETLB;
		align = sysconf(_SC_PAGESIZE);
		aligned_size = HCL_ALIGN_POW2(req_size, align);
		ptr = (hcl_oow_t*)mmap(NULL, aligned_size, PROT_READ | PROT_WRITE, flags, -1, 0);
		if (ptr == MAP_FAILED)
		{
			hcl_seterrwithsyserr (hcl, 0, errno);
			return HCL_NULL;
		}
	}
	#else
	if (ptr == MAP_FAILED)
	{
		hcl_seterrwithsyserr (hcl, 0, errno);
		return HCL_NULL;
	}
	#endif

	*ptr = aligned_size;
	*size = aligned_size - HCL_SIZEOF(hcl_oow_t);
	return (void*)(ptr + 1);

#else
	return HCL_MMGR_ALLOC(hcl->_mmgr, *size);
#endif
}

static void free_heap (hcl_t* hcl, void* ptr)
{
#if defined(_WIN32)
	VirtualFree (ptr, 0, MEM_RELEASE); /* release the entire region */

#elif defined(HAVE_MMAP) && defined(HAVE_MUNMAP)
	hcl_oow_t* actual_ptr;
	actual_ptr = (hcl_oow_t*)ptr - 1;
	munmap (actual_ptr, *actual_ptr);
#else
	HCL_MMGR_FREE(hcl->_mmgr, ptr);
#endif
}


/* -----------------------------------------------------------------
 * LOGGING SUPPORT
 * ----------------------------------------------------------------- */

enum logfd_flag_t
{
	LOGFD_TTY = (1 << 0),
	LOGFD_OPENED_HERE = (1 << 1),
	LOGFD_STDERR_TTY = (1 << 2),
	LOGFD_STDOUT_TTY = (1 << 3)
};

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

static int write_log (hcl_t* hcl, int fd, const hcl_bch_t* ptr, hcl_oow_t len)
{
	xtn_t* xtn = GET_XTN(hcl);

	while (len > 0)
	{
		if (xtn->log.out.len > 0)
		{
			hcl_oow_t rcapa, cplen;

			rcapa = HCL_COUNTOF(xtn->log.out.buf) - xtn->log.out.len;
			cplen = (len >= rcapa)? rcapa: len;

			HCL_MEMCPY (&xtn->log.out.buf[xtn->log.out.len], ptr, cplen);
			xtn->log.out.len += cplen;
			ptr += cplen;
			len -= cplen;

			if (xtn->log.out.len >= HCL_COUNTOF(xtn->log.out.buf))
			{
				int n;
				n = write_all(fd, xtn->log.out.buf, xtn->log.out.len);
				xtn->log.out.len = 0;
				if (n <= -1) return -1;
			}
		}
		else
		{
			hcl_oow_t rcapa;

			rcapa = HCL_COUNTOF(xtn->log.out.buf);
			if (len >= rcapa)
			{
				if (write_all(fd, ptr, rcapa) <= -1) return -1;
				ptr += rcapa;
				len -= rcapa;
			}
			else
			{
				HCL_MEMCPY (xtn->log.out.buf, ptr, len);
				xtn->log.out.len += len;
				ptr += len;
				len -= len;
			}
		}
	}

	return 0;
}

static void flush_log (hcl_t* hcl, int fd, int force)
{
	xtn_t* xtn = GET_XTN(hcl);
	if (xtn->log.out.len > 0 || force)
	{
		write_all (fd, xtn->log.out.buf, xtn->log.out.len);
		xtn->log.out.len = 0;
	}
}

static size_t sprintf_timestamp (char* ts, struct tm* tmp)
{
	int off_h, off_m;

#if defined(__DOS__)
	off_m = _timezone / 60;
#else
	off_m = tmp->tm_gmtoff / 60;
#endif
	off_h = off_m / 60;
	off_m = off_m % 60;
	if (off_m < 0) off_m = -off_m;

	return (size_t)sprintf(ts,
		"%04d-%02d-%02d %02d:%02d:%02d %+03d%02d ",
		tmp->tm_year + 1900, tmp->tm_mon + 1, tmp->tm_mday,
		tmp->tm_hour, tmp->tm_min, tmp->tm_sec, off_h, off_m);
}

static void log_write (hcl_t* hcl, hcl_bitmask_t mask, const hcl_ooch_t* msg, hcl_oow_t len)
{
	hcl_bch_t buf[256];
	hcl_oow_t msgidx;

	xtn_t* xtn = GET_XTN(hcl);
	int logfd;
	int is_tty;
	int force_flush = 0;

	if (mask & HCL_LOG_STDERR)
	{
		logfd = STDERR_FILENO;
		is_tty = !!(xtn->log.fd_flags & LOGFD_STDERR_TTY);
	}
	else if (mask & HCL_LOG_STDOUT)
	{
		logfd = STDOUT_FILENO;
		is_tty = !!(xtn->log.fd_flags & LOGFD_STDOUT_TTY);
	}
	else
	{
		logfd = xtn->log.fd;
		if (logfd <= -1) return;
		is_tty = !!(xtn->log.fd_flags & LOGFD_TTY);
	}

/* TODO: beautify the log message.
 *       do classification based on mask. */
	if (!(mask & (HCL_LOG_STDOUT | HCL_LOG_STDERR)))
	{
		time_t now;
		char ts[64];
		size_t tslen;
		struct tm tm, *tmp;

		now = time(HCL_NULL);
	#if defined(_WIN32)
		#if 0
		tmp = localtime(&now);
		tslen = strftime(ts, sizeof(ts), "%Y-%m-%d %H:%M:%S %z ", tmp);
		if (tslen == 0) tslen = sprintf_timestamp(ts, tmp);
		#else
		/* %z for strftime() in win32 seems to produce a long non-numeric timezone name.
		 * i don't use strftime() for time formatting. */
		GetLocalTime (&now);
		if (GetTimeZoneInformation(&tzi) != TIME_ZONE_ID_INVALID)
		{
			LONG min;
			tzi.Bias = -tzi.Bias; /* utc = localtime + bias(mins). so negate it */
			min = tzi.Bias % 60;
			if (min < 0) min = -min;
			tslen = sprintf(ts, "%04d-%02d-%02d %02d:%02d:%02d %+03d%02d ",
				(int)now.wYear, (int)now.wMonth, (int)now.wDay,
				(int)now.wHour, (int)now.wMinute, (int)now.wSecond,
				(int)(tzi.Bias / 60), (int)min);
		}
		else
		{
			tslen = sprintf(ts, "%04d-%02d-%02d %02d:%02d:%02d ",
				(int)now.wYear, (int)now.wMonth, (int)now.wDay,
				(int)now.wHour, (int)now.wMinute, (int)now.wSecond);
		}
		#endif
	#elif defined(__OS2__)
		#if defined(__WATCOMC__)
		tmp = _localtime(&now, &tm);
		#else
		tmp = localtime(&now);
		#endif

		#if defined(__BORLANDC__) || defined(__IBMC__)
		/* the borland compiler doesn't handle %z properly - it showed 00 all the time.
		 * the visualage compiler 3.0 doesn't support %z */
		tslen = strftime(ts, sizeof(ts), "%Y-%m-%d %H:%M:%S %Z ", tmp);
		#else
		tslen = strftime(ts, sizeof(ts), "%Y-%m-%d %H:%M:%S %z ", tmp);
		#endif
		if (HCL_UNLIKELY(tslen == 0)) tslen = sprintf_timestamp(ts, tmp);

	#elif defined(__DOS__)
		tmp = localtime(&now);
		/* since i know that %z/%Z is not available in strftime, i switch to sprintf immediately */
		tslen = sprintf_timestamp(ts, tmp);
	#else
		#if defined(HAVE_LOCALTIME_R)
		tmp = localtime_r(&now, &tm);
		#else
		tmp = localtime(&now);
		#endif

		#if defined(HAVE_STRFTIME_SMALL_Z)
		tslen = strftime(ts, sizeof(ts), "%Y-%m-%d %H:%M:%S %z ", tmp);
		#else
		tslen = strftime(ts, sizeof(ts), "%Y-%m-%d %H:%M:%S %Z ", tmp);
		#endif
		if (HCL_UNLIKELY(tslen == 0)) tslen = sprintf_timestamp(ts, tmp);
	#endif

	#if defined(HCL_OOCH_IS_UCH)
		#if 0 /* time stamp already in ascii. this double convertion is not needed */
		if (xtn->log_cmgr && hcl_getcmgr(hcl) != xtn->log_cmgr)
		{
			hcl_uch_t tsu[64];
			hcl_oow_t tsulen;

			/* the timestamp is likely to contain simple ascii characters only.
			 * conversion is not likely to fail regardless of encodings.
			 * so i don't check errors here */
			tsulen = HCL_COUNTOF(tsu);
			hcl_convbtooochars (hcl, ts, &tslen, tsu, &tsulen);
			tslen = HCL_COUNTOF(ts);
			hcl_conv_uchars_to_bchars_with_cmgr (tsu, &tsulen, ts, &tslen, xtn->log_cmgr);
		}
		#endif
	#endif
		write_log (hcl, logfd, ts, tslen);
	}

	if (is_tty)
	{
		if (mask & HCL_LOG_FATAL) write_log (hcl, logfd, "\x1B[1;31m", 7);
		else if (mask & HCL_LOG_ERROR) write_log (hcl, logfd, "\x1B[1;32m", 7);
		else if (mask & HCL_LOG_WARN) write_log (hcl, logfd, "\x1B[1;33m", 7);
	}

	if (!msg)
	{
		force_flush = 1;
		goto flush_log_msg;
	}

#if defined(HCL_OOCH_IS_UCH)
	msgidx = 0;
	while (len > 0)
	{
		hcl_oow_t ucslen, bcslen;
		int n;

		ucslen = len;
		bcslen = HCL_COUNTOF(buf);

		n = hcl_convootobchars(hcl, &msg[msgidx], &ucslen, buf, &bcslen);
		if (n == 0 || n == -2)
		{
			/* n = 0:
			 *   converted all successfully
			 * n == -2:
			 *    buffer not sufficient. not all got converted yet.
			 *    write what have been converted this round. */

			HCL_ASSERT (hcl, ucslen > 0); /* if this fails, the buffer size must be increased */

			/* attempt to write all converted characters */
			if (write_log(hcl, logfd, buf, bcslen) <= -1) break;

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
			break;
		}
	}
#else
	write_log (hcl, logfd, msg, len);
#endif

	if (is_tty)
	{
		if (mask & (HCL_LOG_FATAL | HCL_LOG_ERROR | HCL_LOG_WARN)) write_log (hcl, logfd, "\x1B[0m", 4);
	}

flush_log_msg:
	flush_log (hcl, logfd, force_flush);
}

/* -----------------------------------------------------------------
 * SYSTEM ERROR CONVERSION
 * ----------------------------------------------------------------- */
static hcl_errnum_t errno_to_errnum (int errcode)
{
	switch (errcode)
	{
		case ENOMEM: return HCL_ESYSMEM;
		case EINVAL: return HCL_EINVAL;

	#if defined(EBUSY)
		case EBUSY: return HCL_EBUSY;
	#endif
		case EACCES: return HCL_EACCES;
	#if defined(EPERM)
		case EPERM: return HCL_EPERM;
	#endif
	#if defined(ENOTDIR)
		case ENOTDIR: return HCL_ENOTDIR;
	#endif
		case ENOENT: return HCL_ENOENT;
	#if defined(EEXIST)
		case EEXIST: return HCL_EEXIST;
	#endif
	#if defined(EINTR)
		case EINTR:  return HCL_EINTR;
	#endif

	#if defined(EPIPE)
		case EPIPE:  return HCL_EPIPE;
	#endif

	#if defined(EAGAIN) && defined(EWOULDBLOCK) && (EAGAIN != EWOULDBLOCK)
		case EAGAIN:
		case EWOULDBLOCK: return HCL_EAGAIN;
	#elif defined(EAGAIN)
		case EAGAIN: return HCL_EAGAIN;
	#elif defined(EWOULDBLOCK)
		case EWOULDBLOCK: return HCL_EAGAIN;
	#endif

	#if defined(EBADF)
		case EBADF: return HCL_EBADHND;
	#endif

	#if defined(EIO)
		case EIO: return HCL_EIOERR;
	#endif

		default: return HCL_ESYSERR;
	}
}

#if defined(_WIN32)
static hcl_errnum_t winerr_to_errnum (DWORD errcode)
{
	switch (errcode)
	{
		case ERROR_NOT_ENOUGH_MEMORY:
		case ERROR_OUTOFMEMORY:
			return HCL_ESYSMEM;

		case ERROR_INVALID_PARAMETER:
		case ERROR_INVALID_NAME:
			return HCL_EINVAL;

		case ERROR_INVALID_HANDLE:
			return HCL_EBADHND;

		case ERROR_ACCESS_DENIED:
		case ERROR_SHARING_VIOLATION:
			return HCL_EACCES;

	#if defined(ERROR_IO_PRIVILEGE_FAILED)
		case ERROR_IO_PRIVILEGE_FAILED:
	#endif
		case ERROR_PRIVILEGE_NOT_HELD:
			return HCL_EPERM;

		case ERROR_FILE_NOT_FOUND:
		case ERROR_PATH_NOT_FOUND:
			return HCL_ENOENT;

		case ERROR_ALREADY_EXISTS:
		case ERROR_FILE_EXISTS:
			return HCL_EEXIST;

		case ERROR_BROKEN_PIPE:
			return HCL_EPIPE;

		default:
			return HCL_ESYSERR;
	}
}
#endif

#if defined(__OS2__)
static hcl_errnum_t os2err_to_errnum (APIRET errcode)
{
	/* APIRET e */
	switch (errcode)
	{
		case ERROR_NOT_ENOUGH_MEMORY:
			return HCL_ESYSMEM;

		case ERROR_INVALID_PARAMETER:
		case ERROR_INVALID_NAME:
			return HCL_EINVAL;

		case ERROR_INVALID_HANDLE:
			return HCL_EBADHND;

		case ERROR_ACCESS_DENIED:
		case ERROR_SHARING_VIOLATION:
			return HCL_EACCES;

		case ERROR_FILE_NOT_FOUND:
		case ERROR_PATH_NOT_FOUND:
			return HCL_ENOENT;

		case ERROR_ALREADY_EXISTS:
			return HCL_EEXIST;

		/*TODO: add more mappings */
		default:
			return HCL_ESYSERR;
	}
}

#if !defined(TCPV40HDRS)
static hcl_errnum_t os2sockerr_to_errnum (int errcode)
{
	switch (errcode)
	{
		case SOCEPERM:  return HCL_EPERM;
		case SOCENOENT: return HCL_ENOENT;
		case SOCEINTR:  return HCL_EINTR;
		case SOCEACCES: return HCL_EACCES;
		case SOCEINVAL: return HCL_EINVAL;
		case SOCENOMEM: return HCL_ESYSMEM;
		case SOCEPIPE:  return HCL_EPIPE;
		default: return HCL_ESYSERR;
	}
}
#endif /* TCPV40HDRS */
#endif /* __OS2__ */

#if defined(macintosh)
static hcl_errnum_t macerr_to_errnum (int errcode)
{
	switch (e)
	{
		case notEnoughMemoryErr:
			return HCL_ESYSMEM;
		case paramErr:
			return HCL_EINVAL;

		case qErr: /* queue element not found during deletion */
		case fnfErr: /* file not found */
		case dirNFErr: /* direcotry not found */
		case resNotFound: /* resource not found */
		case resFNotFound: /* resource file not found */
		case nbpNotFound: /* name not found on remove */
			return HCL_ENOENT;

		/*TODO: add more mappings */
		default:
			return HCL_ESYSERR;
	}
}
#endif

hcl_errnum_t hcl_syserrstrb (hcl_t* hcl, int syserr_type, int syserr_code, hcl_bch_t* buf, hcl_oow_t len)
{
	switch (syserr_type)
	{
		case 2: /* specially for os2 */
		#if defined(__OS2__)
			#if defined(TCPV40HDRS)
			if (buf)
			{
				char tmp[64];
				sprintf (tmp, "socket error %d", (int)syserr_code);
				hcl_copy_bcstr (buf, len, tmp);
			}
			return HCL_ESYSERR;
			#else
			/* sock_strerror() available in tcpip32.dll only */
			if (buf) hcl_copy_bcstr (buf, len, sock_strerror(syserr_code));
			return os2sockerr_to_errnum(syserr_code);
			#endif
		#endif
			/* fall thru for other platforms */

		case 1:
		#if defined(_WIN32)
			if (buf)
			{
				DWORD rc;
				rc = FormatMessageA (
					FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
					NULL, syserr_code, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
					buf, len, HCL_NULL
				);
				while (rc > 0 && buf[rc - 1] == '\r' || buf[rc - 1] == '\n') buf[--rc] = '\0';
			}
			return winerr_to_errnum(syserr_code);
		#elif defined(__OS2__)
			if (buf)
			{
				char tmp[64];
				sprintf (tmp, "system error %d", (int)syserr_code);
				hcl_copy_bcstr (buf, len, tmp);
			}
			return os2err_to_errnum(syserr_code);
		#elif defined(macintosh)
			/* TODO: convert code to string */
			if (buf) hcl_copy_bcstr (buf, len, "system error");
			return macerr_to_errnum(syserr_code);
		#else
			/* in other systems, errno is still the native system error code.
			 * fall thru */
		#endif

		case 0:
		#if defined(_WIN32) && defined(__STDC_WANT_SECURE_LIB__)
			if (buf && len > 0) strerror_s (buf, len, syserr_code);
		#elif defined(HAVE_STRERROR_R)
			if (buf && len > 0)
			{
				/* to work around mess between XSI and GNU strerror_r */
				hcl_oow_t x;
				buf[0] = '\0';
				/* cast to hcl_oow_t to cater for prototype difference.
				 * one returning int, the other returning a pointer.
				 * x > 1024 in case the XSI version before glibc 2.13 returns
				 * a positive error code upon failure */
				x = (hcl_oow_t)strerror_r(syserr_code, buf, len);
				if (x != (hcl_oow_t)buf && buf[0] == '\0' && x != 0 && x > 1024 && x != (hcl_oow_t)-1)
					hcl_copy_bcstr (buf, len, (const hcl_bch_t*)x);
			}
		#else
			/* this may be thread unsafe */
			if (buf && len > 0) hcl_copy_bcstr (buf, len, strerror(syserr_code));
		#endif
			return errno_to_errnum(syserr_code);
	}

	if (buf) hcl_copy_bcstr (buf, len, "system error");
	return HCL_ESYSERR;
}

/* --------------------------------------------------------------------------
 * ASSERTION SUPPORT
 * -------------------------------------------------------------------------- */

#if defined(HCL_BUILD_RELEASE)

static void _assertfail (hcl_t* hcl, const hcl_bch_t* expr, const hcl_bch_t* file, hcl_oow_t line)
{
	/* do nothing */
}

#else /* defined(HCL_BUILD_RELEASE) */

#if defined(HCL_ENABLE_LIBUNWIND)
#include <libunwind.h>
static void backtrace_stack_frames (hcl_t* hcl)
{
	unw_cursor_t cursor;
	unw_context_t context;
	int n;

	unw_getcontext(&context);
	unw_init_local(&cursor, &context);

	hcl_logbfmt (hcl, HCL_LOG_STDERR | HCL_LOG_UNTYPED | HCL_LOG_DEBUG, "[BACKTRACE]\n");
	for (n = 0; unw_step(&cursor) > 0; n++)
	{
		unw_word_t ip, sp, off;
		char symbol[256];

		unw_get_reg (&cursor, UNW_REG_IP, &ip);
		unw_get_reg (&cursor, UNW_REG_SP, &sp);

		if (unw_get_proc_name(&cursor, symbol, HCL_COUNTOF(symbol), &off))
		{
			hcl_copy_bcstr (symbol, HCL_COUNTOF(symbol), "<unknown>");
		}

		hcl_logbfmt (hcl, HCL_LOG_STDERR | HCL_LOG_UNTYPED | HCL_LOG_DEBUG,
			"#%02d ip=0x%*p sp=0x%*p %hs+0x%zu\n",
			n, HCL_SIZEOF(void*) * 2, (void*)ip, HCL_SIZEOF(void*) * 2, (void*)sp, symbol, (hcl_oow_t)off);
	}
}
#elif defined(HAVE_BACKTRACE)
#include <execinfo.h>
static void backtrace_stack_frames (hcl_t* hcl)
{
	void* btarray[128];
	hcl_oow_t btsize;
	char** btsyms;

	btsize = backtrace (btarray, HCL_COUNTOF(btarray));
	btsyms = backtrace_symbols (btarray, btsize);
	if (btsyms)
	{
		hcl_oow_t i;
		hcl_logbfmt (hcl, HCL_LOG_STDERR | HCL_LOG_UNTYPED | HCL_LOG_DEBUG, "[BACKTRACE]\n");

		for (i = 0; i < btsize; i++)
		{
			hcl_logbfmt (hcl, HCL_LOG_STDERR | HCL_LOG_UNTYPED | HCL_LOG_DEBUG, "  %hs\n", btsyms[i]);
		}
		free (btsyms);
	}
}
#else
static void backtrace_stack_frames (hcl_t* hcl)
{
	/* do nothing. not supported */
}
#endif

static void _assertfail (hcl_t* hcl, const hcl_bch_t* expr, const hcl_bch_t* file, hcl_oow_t line)
{
	hcl_logbfmt (hcl, HCL_LOG_STDERR | HCL_LOG_UNTYPED | HCL_LOG_FATAL, "ASSERTION FAILURE: %hs at %hs:%zu\n", expr, file, line);
	backtrace_stack_frames (hcl);

#if defined(_WIN32)
	ExitProcess (249);
#elif defined(__OS2__)
	DosExit (EXIT_PROCESS, 249);
#elif defined(__DOS__)
	#if defined(__BORLANDC__) && defined(__DPMI32__)
	_exit (249);
	#else
	{
		union REGS regs;
		regs.h.ah = DOS_EXIT;
		regs.h.al = 249;
		intdos (&regs, &regs);
	}
	#endif
#elif defined(vms) || defined(__vms)
	lib$stop (SS$_ABORT); /* use SS$_OPCCUS instead? */
	/* this won't be reached since lib$stop() terminates the process */
	sys$exit (SS$_ABORT); /* this condition code can be shown with
	                       * 'show symbol $status' from the command-line. */
#elif defined(macintosh)

	ExitToShell ();

#else

	kill (getpid(), SIGABRT);
	_exit (1);
#endif
}

#endif /* defined(HCL_BUILD_RELEASE) */

/* -----------------------------------------------------------------
 * POSSIBLY MONOTONIC TIME
 * ----------------------------------------------------------------- */

static void vm_gettime (hcl_t* hcl, hcl_ntime_t* now)
{
#if defined(_WIN32)

	#if defined(_WIN64) || (defined(_WIN32_WINNT) && (_WIN32_WINNT >= 0x0600))
	hcl_uint64_t bigsec, bigmsec;
	bigmsec = GetTickCount64();
	#else
	xtn_t* xtn = GET_XTN(hcl);
	hcl_uint64_t bigsec, bigmsec;
	DWORD msec;

	msec = GetTickCount(); /* this can sustain for 49.7 days */
	if (msec < xtn->tc_last)
	{
		/* i assume the difference is never bigger than 49.7 days */
		/*diff = (HCL_TYPE_MAX(DWORD) - xtn->tc_last) + 1 + msec;*/
		xtn->tc_overflow++;
		bigmsec = ((hcl_uint64_t)HCL_TYPE_MAX(DWORD) * xtn->tc_overflow) + msec;
	}
	else bigmsec = msec;
	xtn->tc_last = msec;
	#endif

	bigsec = HCL_MSEC_TO_SEC(bigmsec);
	bigmsec -= HCL_SEC_TO_MSEC(bigsec);
	HCL_INIT_NTIME(now, bigsec, HCL_MSEC_TO_NSEC(bigmsec));

#elif defined(__OS2__)
	xtn_t* xtn = GET_XTN(hcl);
	ULONG msec;

	#if (HCL_SIZEOF_UINT64_T > 0)
	hcl_uint64_t bigsec, bigmsec;

/* TODO: use DosTmrQueryTime() and DosTmrQueryFreq()? */
	DosQuerySysInfo (QSV_MS_COUNT, QSV_MS_COUNT, &msec, HCL_SIZEOF(msec)); /* milliseconds */
	/* it must return NO_ERROR */
	if (msec < xtn->tc_last)
	{
		xtn->tc_overflow++;
		bigmsec = ((hcl_uint64_t)HCL_TYPE_MAX(ULONG) * xtn->tc_overflow) + msec;
	}
	else bigmsec = msec;
	xtn->tc_last = msec;

	bigsec = HCL_MSEC_TO_SEC(bigmsec);
	bigmsec -= HCL_SEC_TO_MSEC(bigsec);
	HCL_INIT_NTIME (now, bigsec, HCL_MSEC_TO_NSEC(bigmsec));
	#else
	hcl_uint32_t bigsec, bigmsec;

	DosQuerySysInfo (QSV_MS_COUNT, QSV_MS_COUNT, &msec, HCL_SIZEOF(msec));
	bigsec = HCL_MSEC_TO_SEC(msec);
	bigmsec = msec - HCL_SEC_TO_MSEC(bigsec);
	if (msec < xtn->tc_last)
	{
		ULONG i;
		hcl_uint32_t inc;

		xtn->tc_overflow++;
		inc = HCL_MSEC_TO_SEC(HCL_TYPE_MAX(hcl_uint32_t));
		for (i = 0; i < xtn->tc_overflow; i++)
		{
			ULONG max = HCL_TYPE_MAX(hcl_uint32_t) - bigsec;
			if (max > inc)
			{
				bigsec = HCL_TYPE_MAX(hcl_uint32_t);
				break;
			}
			bigsec += inc;
		}
	}
	HCL_INIT_NTIME (now, bigsec, HCL_MSEC_TO_NSEC(bigmsec));
    #endif

#elif defined(__DOS__)
	xtn_t* xtn = GET_XTN(hcl);
	clock_t c, elapsed;
	hcl_ntime_t et;

	c = clock();
	elapsed = (c < xtn->tc_last)? (HCL_TYPE_MAX(clock_t) - xtn->tc_last + c + 1): (c - xtn->tc_last);
	xtn->tc_last = c;

	now->sec = c / CLOCKS_PER_SEC;
	#if (CLOCKS_PER_SEC == 100)
		now->nsec = HCL_MSEC_TO_NSEC((c % (clock_t)CLOCKS_PER_SEC) * 10);
	#elif (CLOCKS_PER_SEC == 1000)
		now->nsec = HCL_MSEC_TO_NSEC(c % (clock_t)CLOCKS_PER_SEC);
	#elif (CLOCKS_PER_SEC == 1000000L)
		now->nsec = HCL_USEC_TO_NSEC(c % (clock_t)CLOCKS_PER_SEC);
	#elif (CLOCKS_PER_SEC == 1000000000L)
		now->nsec = (c % (clock_t)CLOCKS_PER_SEC);
	#else
	#	error UNSUPPORTED CLOCKS_PER_SEC
	#endif

	HCL_ADD_NTIME (&xtn->tc_last_ret, &xtn->tc_last_ret, &et);
	*now = xtn->tc_last_ret;

#elif defined(macintosh)
	UnsignedWide tick;
	hcl_uint64_t tick64;
	Microseconds (&tick);
	tick64 = *(hcl_uint64_t*)&tick;
	HCL_INIT_NTIME (now, HCL_USEC_TO_SEC(tick64), HCL_USEC_TO_NSEC(tick64));
#elif defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_MONOTONIC)
	struct timespec ts;
	clock_gettime (CLOCK_MONOTONIC, &ts);
	HCL_INIT_NTIME(now, ts.tv_sec, ts.tv_nsec);
#elif defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_REALTIME)
	struct timespec ts;
	clock_gettime (CLOCK_REALTIME, &ts);
	HCL_INIT_NTIME(now, ts.tv_sec, ts.tv_nsec);
#else
	struct timeval tv;
	gettimeofday (&tv, HCL_NULL);
	HCL_INIT_NTIME(now, tv.tv_sec, HCL_USEC_TO_NSEC(tv.tv_usec));
#endif
}

/* -----------------------------------------------------------------
 * IO MULTIPLEXING
 * ----------------------------------------------------------------- */

static int _add_poll_fd (hcl_t* hcl, int fd, int event_mask)
{
#if defined(USE_DEVPOLL)
	xtn_t* xtn = GET_XTN(hcl);
	struct pollfd ev;

	HCL_ASSERT (hcl, xtn->ep >= 0);
	ev.fd = fd;
	ev.events = event_mask;
	ev.revents = 0;
	if (write(xtn->ep, &ev, HCL_SIZEOF(ev)) != HCL_SIZEOF(ev))
	{
		hcl_seterrwithsyserr (hcl, 0, errno);
		HCL_DEBUG2 (hcl, "Cannot add file descriptor %d to devpoll - %hs\n", fd, strerror(errno));
		return -1;
	}

	return 0;

#elif defined(USE_KQUEUE)
	xtn_t* xtn = GET_XTN(hcl);
	struct kevent ev;
	hcl_oow_t rindex, roffset;
	hcl_oow_t rv = 0;

	rindex = (hcl_oow_t)fd / (HCL_BITSOF(hcl_oow_t) >> 1);
	roffset = ((hcl_oow_t)fd << 1) % HCL_BITSOF(hcl_oow_t);

	if (rindex >= xtn->ev.reg.capa)
	{
		hcl_oow_t* tmp;
		hcl_oow_t newcapa;

		HCL_STATIC_ASSERT (HCL_SIZEOF(*tmp) == HCL_SIZEOF(*xtn->ev.reg.ptr));

		newcapa = rindex + 1;
		newcapa = HCL_ALIGN_POW2(newcapa, 16);

		tmp = (hcl_oow_t*)hcl_reallocmem(hcl, xtn->ev.reg.ptr, newcapa * HCL_SIZEOF(*tmp));
		if (!tmp)
		{
			const hcl_ooch_t* oldmsg = hcl_backuperrmsg(hcl);
			hcl_seterrbfmt (hcl, HCL_ESYSERR, "unable to add file descriptor %d to kqueue - %js", fd, oldmsg);
			HCL_DEBUG1 (hcl, "%js", hcl_geterrmsg(hcl));
			return -1;
		}

		HCL_MEMSET (&tmp[xtn->ev.reg.capa], 0, newcapa - xtn->ev.reg.capa);
		xtn->ev.reg.ptr = tmp;
		xtn->ev.reg.capa = newcapa;
	}

	if (event_mask & XPOLLIN)
	{
		/*EV_SET (&ev, fd, EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, 0);*/
		HCL_MEMSET (&ev, 0, HCL_SIZEOF(ev));
		ev.ident = fd;
		ev.flags = EV_ADD;
	#if defined(USE_THREAD)
		ev.flags |= EV_CLEAR; /* EV_CLEAR for edge trigger? */
	#endif
		ev.filter = EVFILT_READ;
		if (kevent(xtn->ep, &ev, 1, HCL_NULL, 0, HCL_NULL) == -1)
		{
			hcl_seterrwithsyserr (hcl, 0, errno);
			HCL_DEBUG2 (hcl, "Cannot add file descriptor %d to kqueue for read - %hs\n", fd, strerror(errno));
			return -1;
		}

		rv |= 1;
	}
	if (event_mask & XPOLLOUT)
	{
		/*EV_SET (&ev, fd, EVFILT_WRITE, EV_ADD | EV_CLEAR, 0, 0, 0);*/
		HCL_MEMSET (&ev, 0, HCL_SIZEOF(ev));
		ev.ident = fd;
		ev.flags = EV_ADD;
	#if defined(USE_THREAD)
		ev.flags |= EV_CLEAR; /* EV_CLEAR for edge trigger? */
	#endif
		ev.filter = EVFILT_WRITE;
		if (kevent(xtn->ep, &ev, 1, HCL_NULL, 0, HCL_NULL) == -1)
		{
			hcl_seterrwithsyserr (hcl, 0, errno);
			HCL_DEBUG2 (hcl, "Cannot add file descriptor %d to kqueue for write - %hs\n", fd, strerror(errno));

			if (event_mask & XPOLLIN)
			{
				HCL_MEMSET (&ev, 0, HCL_SIZEOF(ev));
				ev.ident = fd;
				ev.flags = EV_DELETE;
				ev.filter = EVFILT_READ;
				kevent(xtn->ep, &ev, 1, HCL_NULL, 0, HCL_NULL);
			}
			return -1;
		}

		rv |= 2;
	}

	HCL_SETBITS (hcl_oow_t, xtn->ev.reg.ptr[rindex], roffset, 2, rv);
	return 0;

#elif defined(USE_EPOLL)
	xtn_t* xtn = GET_XTN(hcl);
	struct epoll_event ev;

	HCL_ASSERT (hcl, xtn->ep >= 0);
	HCL_MEMSET (&ev, 0, HCL_SIZEOF(ev));
	ev.events = event_mask;
	#if defined(USE_THREAD) && defined(EPOLLET)
	/* epoll_wait may return again if the worker thread consumes events.
	 * switch to level-trigger. */
	/* TODO: verify if EPOLLLET is desired */
	ev.events |= EPOLLET/*  | EPOLLRDHUP | EPOLLHUP */;
	#endif
	/*ev.data.ptr = (void*)event_data;*/
	ev.data.fd = fd;
	if (epoll_ctl(xtn->ep, EPOLL_CTL_ADD, fd, &ev) == -1)
	{
		hcl_seterrwithsyserr (hcl, 0, errno);
		HCL_DEBUG2 (hcl, "Cannot add file descriptor %d to epoll - %hs\n", fd, strerror(errno));
		return -1;
	}
	return 0;

#elif defined(USE_POLL)
	xtn_t* xtn = GET_XTN(hcl);

	MUTEX_LOCK (&xtn->ev.reg.pmtx);
	if (xtn->ev.reg.len >= xtn->ev.reg.capa)
	{
		struct pollfd* tmp, * tmp2;
		hcl_oow_t newcapa;

		newcapa = HCL_ALIGN_POW2 (xtn->ev.reg.len + 1, 256);
		tmp = (struct pollfd*)hcl_reallocmem(hcl, xtn->ev.reg.ptr, newcapa * HCL_SIZEOF(*tmp));
		tmp2 = (struct pollfd*)hcl_reallocmem(hcl, xtn->ev.buf, newcapa * HCL_SIZEOF(*tmp2));
		if (!tmp || !tmp2)
		{
			HCL_DEBUG2 (hcl, "Cannot add file descriptor %d to poll - %hs\n", fd, strerror(errno));
			MUTEX_UNLOCK (&xtn->ev.reg.pmtx);
			if (tmp) hcl_freemem (hcl, tmp);
			return -1;
		}

		xtn->ev.reg.ptr = tmp;
		xtn->ev.reg.capa = newcapa;

		xtn->ev.buf = tmp2;
	}

	xtn->ev.reg.ptr[xtn->ev.reg.len].fd = fd;
	xtn->ev.reg.ptr[xtn->ev.reg.len].events = event_mask;
	xtn->ev.reg.ptr[xtn->ev.reg.len].revents = 0;
	xtn->ev.reg.len++;
	MUTEX_UNLOCK (&xtn->ev.reg.pmtx);

	return 0;

#elif defined(USE_SELECT)
	xtn_t* xtn = GET_XTN(hcl);

	MUTEX_LOCK (&xtn->ev.reg.smtx);
	if (event_mask & XPOLLIN)
	{
		FD_SET (fd, &xtn->ev.reg.rfds);
		if (fd > xtn->ev.reg.maxfd) xtn->ev.reg.maxfd = fd;
	}
	if (event_mask & XPOLLOUT)
	{
		FD_SET (fd, &xtn->ev.reg.wfds);
		if (fd > xtn->ev.reg.maxfd) xtn->ev.reg.maxfd = fd;
	}
	MUTEX_UNLOCK (&xtn->ev.reg.smtx);

	return 0;

#else

	HCL_DEBUG1 (hcl, "Cannot add file descriptor %d to poll - not implemented\n", fd);
	hcl_seterrnum (hcl, HCL_ENOIMPL);
	return -1;
#endif

}

static int _del_poll_fd (hcl_t* hcl, int fd)
{

#if defined(USE_DEVPOLL)
	xtn_t* xtn = GET_XTN(hcl);
	struct pollfd ev;

	HCL_ASSERT (hcl, xtn->ep >= 0);
	ev.fd = fd;
	ev.events = POLLREMOVE;
	ev.revents = 0;
	if (write(xtn->ep, &ev, HCL_SIZEOF(ev)) != HCL_SIZEOF(ev))
	{
		hcl_seterrwithsyserr (hcl, 0, errno);
		HCL_DEBUG2 (hcl, "Cannot remove file descriptor %d from devpoll - %hs\n", fd, strerror(errno));
		return -1;
	}

	return 0;

#elif defined(USE_KQUEUE)
	xtn_t* xtn = GET_XTN(hcl);
	hcl_oow_t rindex, roffset;
	int rv;
	struct kevent ev;

	rindex = (hcl_oow_t)fd / (HCL_BITSOF(hcl_oow_t) >> 1);
	roffset = ((hcl_oow_t)fd << 1) % HCL_BITSOF(hcl_oow_t);

	if (rindex >= xtn->ev.reg.capa)
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "unknown file descriptor %d", fd);
		HCL_DEBUG2 (hcl, "Cannot remove file descriptor %d from kqueue - %js\n", fd, hcl_geterrmsg(hcl));
		return -1;
	};

	rv = HCL_GETBITS (hcl_oow_t, xtn->ev.reg.ptr[rindex], roffset, 2);

	if (rv & 1)
	{
		/*EV_SET (&ev, fd, EVFILT_READ, EV_DELETE, 0, 0, 0);*/
		HCL_MEMSET (&ev, 0, HCL_SIZEOF(ev));
		ev.ident = fd;
		ev.flags = EV_DELETE;
		ev.filter = EVFILT_READ;
		kevent(xtn->ep, &ev, 1, HCL_NULL, 0, HCL_NULL);
		/* no error check for now */
	}

	if (rv & 2)
	{
		/*EV_SET (&ev, fd, EVFILT_WRITE, EV_DELETE, 0, 0, 0);*/
		HCL_MEMSET (&ev, 0, HCL_SIZEOF(ev));
		ev.ident = fd;
		ev.flags = EV_DELETE;
		ev.filter = EVFILT_WRITE;
		kevent(xtn->ep, &ev, 1, HCL_NULL, 0, HCL_NULL);
		/* no error check for now */
	}

	HCL_SETBITS (hcl_oow_t, xtn->ev.reg.ptr[rindex], roffset, 2, 0);
	return 0;

#elif defined(USE_EPOLL)
	xtn_t* xtn = GET_XTN(hcl);
	struct epoll_event ev;

	HCL_ASSERT (hcl, xtn->ep >= 0);
	HCL_MEMSET (&ev, 0, HCL_SIZEOF(ev));
	if (epoll_ctl(xtn->ep, EPOLL_CTL_DEL, fd, &ev) == -1)
	{
		hcl_seterrwithsyserr (hcl, 0, errno);
		HCL_DEBUG2 (hcl, "Cannot remove file descriptor %d from epoll - %hs\n", fd, strerror(errno));
		return -1;
	}
	return 0;

#elif defined(USE_POLL)
	xtn_t* xtn = GET_XTN(hcl);
	hcl_oow_t i;

	/* TODO: performance boost. no linear search */
	MUTEX_LOCK (&xtn->ev.reg.pmtx);
	for (i = 0; i < xtn->ev.reg.len; i++)
	{
		if (xtn->ev.reg.ptr[i].fd == fd)
		{
			xtn->ev.reg.len--;
			HCL_MEMMOVE (&xtn->ev.reg.ptr[i], &xtn->ev.reg.ptr[i+1], (xtn->ev.reg.len - i) * HCL_SIZEOF(*xtn->ev.reg.ptr));
			MUTEX_UNLOCK (&xtn->ev.reg.pmtx);
			return 0;
		}
	}
	MUTEX_UNLOCK (&xtn->ev.reg.pmtx);


	HCL_DEBUG1 (hcl, "Cannot remove file descriptor %d from poll - not found\n", fd);
	hcl_seterrnum (hcl, HCL_ENOENT);
	return -1;

#elif defined(USE_SELECT)
	xtn_t* xtn = GET_XTN(hcl);

	MUTEX_LOCK (&xtn->ev.reg.smtx);
	FD_CLR (fd, &xtn->ev.reg.rfds);
	FD_CLR (fd, &xtn->ev.reg.wfds);
	if (fd >= xtn->ev.reg.maxfd)
	{
		int i;
		/* TODO: any way to make this search faster or to do without the search like this */
		for (i = fd - 1; i >= 0; i--)
		{
			if (FD_ISSET(i, &xtn->ev.reg.rfds) || FD_ISSET(i, &xtn->ev.reg.wfds)) break;
		}
		xtn->ev.reg.maxfd = i;
	}
	MUTEX_UNLOCK (&xtn->ev.reg.smtx);

	return 0;

#else

	HCL_DEBUG1 (hcl, "Cannot remove file descriptor %d from poll - not implemented\n", fd);
	hcl_seterrnum (hcl, HCL_ENOIMPL);
	return -1;
#endif
}


static int _mod_poll_fd (hcl_t* hcl, int fd, int event_mask)
{
#if defined(USE_DEVPOLL)

	if (_del_poll_fd (hcl, fd) <= -1) return -1;

	if (_add_poll_fd (hcl, fd, event_mask) <= -1)
	{
		/* TODO: any good way to rollback successful deletion? */
		return -1;
	}

	return 0;
#elif defined(USE_KQUEUE)
	xtn_t* xtn = GET_XTN(hcl);
	hcl_oow_t rindex, roffset;
	int rv, newrv = 0;
	struct kevent ev;

	rindex = (hcl_oow_t)fd / (HCL_BITSOF(hcl_oow_t) >> 1);
	roffset = ((hcl_oow_t)fd << 1) % HCL_BITSOF(hcl_oow_t);

	if (rindex >= xtn->ev.reg.capa)
	{
		hcl_seterrbfmt (hcl, HCL_EINVAL, "unknown file descriptor %d", fd);
		HCL_DEBUG2 (hcl, "Cannot modify file descriptor %d in kqueue - %js\n", fd, hcl_geterrmsg(hcl));
		return -1;
	};

	rv = HCL_GETBITS(hcl_oow_t, xtn->ev.reg.ptr[rindex], roffset, 2);

	if (rv & 1)
	{
		if (!(event_mask & XPOLLIN))
		{
			HCL_MEMSET (&ev, 0, HCL_SIZEOF(ev));
			ev.ident = fd;
			ev.flags = EV_DELETE;
			ev.filter = EVFILT_READ;
			if (kevent(xtn->ep, &ev, 1, HCL_NULL, 0, HCL_NULL) == -1) goto kqueue_syserr;

			newrv &= ~1;
		}
	}
	else
	{
		if (event_mask & XPOLLIN)
		{
			HCL_MEMSET (&ev, 0, HCL_SIZEOF(ev));
			ev.ident = fd;
			ev.flags = EV_ADD;
		#if defined(USE_THREAD)
			ev.flags |= EV_CLEAR; /* EV_CLEAR for edge trigger? */
		#endif
			ev.filter = EVFILT_READ;
			if (kevent(xtn->ep, &ev, 1, HCL_NULL, 0, HCL_NULL) == -1) goto kqueue_syserr;

			newrv |= 1;
		}
	}

	if (rv & 2)
	{
		if (!(event_mask & XPOLLOUT))
		{
			HCL_MEMSET (&ev, 0, HCL_SIZEOF(ev));
			ev.ident = fd;
			ev.flags = EV_DELETE;
			ev.filter = EVFILT_WRITE;
			/* there is no operation rollback for the (rv & 1) case.
			 * the rollback action may fail again even if i try it */
			if (kevent(xtn->ep, &ev, 1, HCL_NULL, 0, HCL_NULL) == -1) goto kqueue_syserr;

			newrv &= ~2;
		}
	}
	else
	{
		if (event_mask & XPOLLOUT)
		{
			HCL_MEMSET (&ev, 0, HCL_SIZEOF(ev));
			ev.ident = fd;
			ev.flags = EV_ADD;
		#if defined(USE_THREAD)
			ev.flags |= EV_CLEAR; /* EV_CLEAR for edge trigger? */
		#endif
			ev.filter = EVFILT_WRITE;

			/* there is no operation rollback for the (rv & 1) case.
			 * the rollback action may fail again even if i try it */
			if (kevent(xtn->ep, &ev, 1, HCL_NULL, 0, HCL_NULL) == -1) goto kqueue_syserr;

			newrv |= 2;
		}
	}

	HCL_SETBITS (hcl_oow_t, xtn->ev.reg.ptr[rindex], roffset, 2, newrv);
	return 0;

kqueue_syserr:
	hcl_seterrwithsyserr (hcl, 0, errno);
	HCL_DEBUG2 (hcl, "Cannot modify file descriptor %d in kqueue - %hs\n", fd, strerror(errno));
	return -1;

#elif defined(USE_EPOLL)
	xtn_t* xtn = GET_XTN(hcl);
	struct epoll_event ev;

	HCL_ASSERT (hcl, xtn->ep >= 0);
	HCL_MEMSET (&ev, 0, HCL_SIZEOF(ev));
	ev.events = event_mask;
	#if defined(USE_THREAD) && defined(EPOLLET)
	/* epoll_wait may return again if the worker thread consumes events.
	 * switch to level-trigger. */
	/* TODO: verify if EPOLLLET is desired */
	ev.events |= EPOLLET;
	#endif
	ev.data.fd = fd;
	if (epoll_ctl(xtn->ep, EPOLL_CTL_MOD, fd, &ev) == -1)
	{
		hcl_seterrwithsyserr (hcl, 0, errno);
		HCL_DEBUG2 (hcl, "Cannot modify file descriptor %d in epoll - %hs\n", fd, strerror(errno));
		return -1;
	}

	return 0;

#elif defined(USE_POLL)

	xtn_t* xtn = GET_XTN(hcl);
	hcl_oow_t i;

	MUTEX_LOCK (&xtn->ev.reg.pmtx);
	for (i = 0; i < xtn->ev.reg.len; i++)
	{
		if (xtn->ev.reg.ptr[i].fd == fd)
		{
			HCL_MEMMOVE (&xtn->ev.reg.ptr[i], &xtn->ev.reg.ptr[i+1], (xtn->ev.reg.len - i - 1) * HCL_SIZEOF(*xtn->ev.reg.ptr));
			xtn->ev.reg.ptr[i].fd = fd;
			xtn->ev.reg.ptr[i].events = event_mask;
			xtn->ev.reg.ptr[i].revents = 0;
			MUTEX_UNLOCK (&xtn->ev.reg.pmtx);

			return 0;
		}
	}
	MUTEX_UNLOCK (&xtn->ev.reg.pmtx);

	HCL_DEBUG1 (hcl, "Cannot modify file descriptor %d in poll - not found\n", fd);
	hcl_seterrnum (hcl, HCL_ENOENT);
	return -1;

#elif defined(USE_SELECT)

	xtn_t* xtn = GET_XTN(hcl);

	MUTEX_LOCK (&xtn->ev.reg.smtx);
	HCL_ASSERT (hcl, fd <= xtn->ev.reg.maxfd);

	if (event_mask & XPOLLIN)
		FD_SET (fd, &xtn->ev.reg.rfds);
	else
		FD_CLR (fd, &xtn->ev.reg.rfds);

	if (event_mask & XPOLLOUT)
		FD_SET (fd, &xtn->ev.reg.wfds);
	else
		FD_CLR (fd, &xtn->ev.reg.wfds);
	MUTEX_UNLOCK (&xtn->ev.reg.smtx);

	return 0;

#else
	HCL_DEBUG1 (hcl, "Cannot modify file descriptor %d in poll - not implemented\n", fd);
	hcl_seterrnum (hcl, HCL_ENOIMPL);
	return -1;
#endif
}

static int vm_muxadd (hcl_t* hcl, hcl_ooi_t io_handle, hcl_ooi_t mask)
{
	int event_mask;

	event_mask = 0;
	if (mask & HCL_SEMAPHORE_IO_MASK_INPUT) event_mask |= XPOLLIN;
	if (mask & HCL_SEMAPHORE_IO_MASK_OUTPUT) event_mask |= XPOLLOUT;

	if (event_mask == 0)
	{
		HCL_DEBUG2 (hcl, "<vm_muxadd> Invalid semaphore mask %zd on handle %zd\n", mask, io_handle);
		hcl_seterrbfmt (hcl, HCL_EINVAL, "invalid semaphore mask %zd on handle %zd", mask, io_handle);
		return -1;
	}

	return _add_poll_fd(hcl, io_handle, event_mask);
}

static int vm_muxmod (hcl_t* hcl, hcl_ooi_t io_handle, hcl_ooi_t mask)
{
	int event_mask;

	event_mask = 0;
	if (mask & HCL_SEMAPHORE_IO_MASK_INPUT) event_mask |= XPOLLIN;
	if (mask & HCL_SEMAPHORE_IO_MASK_OUTPUT) event_mask |= XPOLLOUT;

	if (event_mask == 0)
	{
		HCL_DEBUG2 (hcl, "<vm_muxadd> Invalid semaphore mask %zd on handle %zd\n", mask, io_handle);
		hcl_seterrbfmt (hcl, HCL_EINVAL, "invalid semaphore mask %zd on handle %zd", mask, io_handle);
		return -1;
	}

	return _mod_poll_fd(hcl, io_handle, event_mask);
}

static int vm_muxdel (hcl_t* hcl, hcl_ooi_t io_handle)
{
	return _del_poll_fd(hcl, io_handle);
}

#if defined(USE_THREAD)
static void* iothr_main (void* arg)
{
	hcl_t* hcl = (hcl_t*)arg;
	xtn_t* xtn = GET_XTN(hcl);

	/*while (!hcl->abort_req)*/
	while (!xtn->iothr.abort)
	{
		if (xtn->ev.len <= 0) /* TODO: no mutex needed for this check? */
		{
			int n;
		#if defined(USE_DEVPOLL)
			struct dvpoll dvp;
		#elif defined(USE_KQUEUE)
			struct timespec ts;
		#elif defined(USE_POLL)
			hcl_oow_t nfds;
		#elif defined(USE_SELECT)
			struct timeval tv;
			fd_set rfds;
			fd_set wfds;
			int maxfd;
		#endif

		poll_for_event:

		#if defined(USE_DEVPOLL)
			dvp.dp_timeout = 10000; /* milliseconds */
			dvp.dp_fds = xtn->ev.buf;
			dvp.dp_nfds = HCL_COUNTOF(xtn->ev.buf);
			n = ioctl (xtn->ep, DP_POLL, &dvp);
		#elif defined(USE_KQUEUE)
			ts.tv_sec = 10;
			ts.tv_nsec = 0;
			n = kevent(xtn->ep, HCL_NULL, 0, xtn->ev.buf, HCL_COUNTOF(xtn->ev.buf), &ts);
			/* n == 0: timeout
			 * n == -1: error */
		#elif defined(USE_EPOLL)
			n = epoll_wait(xtn->ep, xtn->ev.buf, HCL_COUNTOF(xtn->ev.buf), 10000); /* TODO: make this timeout value in the io thread */
		#elif defined(USE_POLL)
			MUTEX_LOCK (&xtn->ev.reg.pmtx);
			HCL_MEMCPY (xtn->ev.buf, xtn->ev.reg.ptr, xtn->ev.reg.len * HCL_SIZEOF(*xtn->ev.buf));
			nfds = xtn->ev.reg.len;
			MUTEX_UNLOCK (&xtn->ev.reg.pmtx);
			n = poll(xtn->ev.buf, nfds, 10000);
			if (n > 0)
			{
				/* compact the return buffer as poll() doesn't */
				hcl_oow_t i, j;
				for (i = 0, j = 0; i < nfds && j < n; i++)
				{
					if (xtn->ev.buf[i].revents)
					{
						if (j != i) xtn->ev.buf[j] = xtn->ev.buf[i];
						j++;
					}
				}
				n = j;
			}
		#elif defined(USE_SELECT)
			tv.tv_sec = 10;
			tv.tv_usec = 0;
			MUTEX_LOCK (&xtn->ev.reg.smtx);
			maxfd = xtn->ev.reg.maxfd;
			HCL_MEMCPY (&rfds, &xtn->ev.reg.rfds, HCL_SIZEOF(rfds));
			HCL_MEMCPY (&wfds, &xtn->ev.reg.wfds, HCL_SIZEOF(wfds));
			MUTEX_UNLOCK (&xtn->ev.reg.smtx);
			n = select (maxfd + 1, &rfds, &wfds, HCL_NULL, &tv);
			if (n > 0)
			{
				int fd, count = 0;
				for (fd = 0;  fd <= maxfd; fd++)
				{
					int events = 0;
					if (FD_ISSET(fd, &rfds)) events |= XPOLLIN;
					if (FD_ISSET(fd, &wfds)) events |= XPOLLOUT;

					if (events)
					{
						HCL_ASSERT (hcl, count < HCL_COUNTOF(xtn->ev.buf));
						xtn->ev.buf[count].fd = fd;
						xtn->ev.buf[count].events = events;
						count++;
					}
				}

				n = count;
				HCL_ASSERT (hcl, n > 0);
			}
		#endif

			pthread_mutex_lock (&xtn->ev.mtx);
			if (n <= -1)
			{
				/* TODO: don't use HCL_DEBUG2. it's not thread safe... */
				/* the following call has a race-condition issue when called in this separate thread */
				/*HCL_DEBUG2 (hcl, "Warning: multiplexer wait failure - %d, %hs\n", errno, strerror(errno));*/
			}
			else if (n > 0)
			{
				xtn->ev.len = n;
			}
			pthread_cond_signal (&xtn->ev.cnd2);
			pthread_mutex_unlock (&xtn->ev.mtx);
		}
		else
		{
			/* the event buffer has not been emptied yet */
			struct timespec ts;

			pthread_mutex_lock (&xtn->ev.mtx);
			if (xtn->ev.len <= 0)
			{
				/* it got emptied between the if check and pthread_mutex_lock() above */
				pthread_mutex_unlock (&xtn->ev.mtx);
				goto poll_for_event;
			}

		#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_REALTIME)
			clock_gettime (CLOCK_REALTIME, &ts);
		#else
			{
				struct timeval tv;
				gettimeofday (&tv, HCL_NULL);
				ts.tv_sec = tv.tv_sec;
				ts.tv_nsec = HCL_USEC_TO_NSEC(tv.tv_usec);
			}
		#endif
			ts.tv_sec += 10;
			pthread_cond_timedwait (&xtn->ev.cnd, &xtn->ev.mtx, &ts);
			pthread_mutex_unlock (&xtn->ev.mtx);
		}

		/*sched_yield ();*/
	}

	return HCL_NULL;
}
#endif

static void vm_muxwait (hcl_t* hcl, const hcl_ntime_t* dur, hcl_vmprim_muxwait_cb_t muxwcb)
{
	xtn_t* xtn = GET_XTN(hcl);

#if defined(USE_THREAD)
	int n;

	/* create a thread if mux wait is started at least once. */
	if (!xtn->iothr.up)
	{
		xtn->iothr.up = 1;
		if (pthread_create(&xtn->iothr.thr, HCL_NULL, iothr_main, hcl) != 0)
		{
			HCL_LOG2 (hcl, HCL_LOG_WARN, "Warning: pthread_create failure - %d, %hs\n", errno, strerror(errno));
			xtn->iothr.up = 0;
/* TODO: switch to the non-threaded mode? */
		}
	}

	if (xtn->iothr.abort) return;

	if (xtn->ev.len <= 0)
	{
		struct timespec ts;
		hcl_ntime_t ns;

		if (!dur) return; /* immediate check is requested. and there is no event */

	#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_REALTIME)
		clock_gettime (CLOCK_REALTIME, &ts);
		ns.sec = ts.tv_sec;
		ns.nsec = ts.tv_nsec;
	#else
		{
			struct timeval tv;
			gettimeofday (&tv, HCL_NULL);
			ns.sec = tv.tv_sec;
			ns.nsec = HCL_USEC_TO_NSEC(tv.tv_usec);
		}
	#endif
		HCL_ADD_NTIME (&ns, &ns, dur);
		ts.tv_sec = ns.sec;
		ts.tv_nsec = ns.nsec;

		pthread_mutex_lock (&xtn->ev.mtx);
		if (xtn->ev.len <= 0)
		{
			/* the event buffer is still empty */
			pthread_cond_timedwait (&xtn->ev.cnd2, &xtn->ev.mtx, &ts);
		}
		pthread_mutex_unlock (&xtn->ev.mtx);
	}

	n = xtn->ev.len;

	if (n > 0)
	{
		do
		{
			--n;

		#if defined(USE_DEVPOLL)
			if (xtn->ev.buf[n].fd == xtn->iothr.p[0])
		#elif defined(USE_KQUEUE)
			if (xtn->ev.buf[n].ident == xtn->iothr.p[0])
		#elif defined(USE_EPOLL)
			/*if (xtn->ev.buf[n].data.ptr == (void*)HCL_TYPE_MAX(hcl_oow_t))*/
			if (xtn->ev.buf[n].data.fd == xtn->iothr.p[0])
		#elif defined(USE_POLL)
			if (xtn->ev.buf[n].fd == xtn->iothr.p[0])
		#elif defined(USE_SELECT)
			if (xtn->ev.buf[n].fd == xtn->iothr.p[0])
		#else
		#	error UNSUPPORTED
		#endif
			{
				hcl_uint8_t u8;
				while (read(xtn->iothr.p[0], &u8, HCL_SIZEOF(u8)) > 0)
				{
					/* consume as much as possible */;
					if (u8 == 'Q') xtn->iothr.abort = 1;
				}
			}
			else if (muxwcb)
			{
				int revents;
				hcl_ooi_t mask;

			#if defined(USE_DEVPOLL)
				revents = xtn->ev.buf[n].revents;
			#elif defined(USE_KQUEUE)
				if (xtn->ev.buf[n].filter == EVFILT_READ) mask = HCL_SEMAPHORE_IO_MASK_INPUT;
				else if (xtn->ev.buf[n].filter == EVFILT_WRITE) mask = HCL_SEMAPHORE_IO_MASK_OUTPUT;
				else mask = 0;
				goto call_muxwcb_kqueue;
			#elif defined(USE_EPOLL)
				revents = xtn->ev.buf[n].events;
			#elif defined(USE_POLL)
				revents = xtn->ev.buf[n].revents;
			#elif defined(USE_SELECT)
				revents = xtn->ev.buf[n].events;
			#endif

				mask = 0;
				if (revents & XPOLLIN) mask |= HCL_SEMAPHORE_IO_MASK_INPUT;
				if (revents & XPOLLOUT) mask |= HCL_SEMAPHORE_IO_MASK_OUTPUT;
				if (revents & XPOLLERR) mask |= HCL_SEMAPHORE_IO_MASK_ERROR;
				if (revents & XPOLLHUP) mask |= HCL_SEMAPHORE_IO_MASK_HANGUP;

			#if defined(USE_DEVPOLL)
				muxwcb (hcl, xtn->ev.buf[n].fd, mask);
			#elif defined(USE_KQUEUE)
			call_muxwcb_kqueue:
				muxwcb (hcl, xtn->ev.buf[n].ident, mask);
			#elif defined(USE_EPOLL)
				muxwcb (hcl, xtn->ev.buf[n].data.fd, mask);
			#elif defined(USE_POLL)
				muxwcb (hcl, xtn->ev.buf[n].fd, mask);
			#elif defined(USE_SELECT)
				muxwcb (hcl, xtn->ev.buf[n].fd, mask);
			#else
			#	error UNSUPPORTED
			#endif
			}
		}
		while (n > 0);

		pthread_mutex_lock (&xtn->ev.mtx);
		xtn->ev.len = 0;
		pthread_cond_signal (&xtn->ev.cnd);
		pthread_mutex_unlock (&xtn->ev.mtx);
	}

#else /* USE_THREAD */
	int n;
	#if defined(USE_DEVPOLL)
	int tmout;
	struct dvpoll dvp;
	#elif defined(USE_KQUEUE)
	struct timespec ts;
	#elif defined(USE_EPOLL)
	int tmout;
	#elif defined(USE_POLL)
	int tmout;
	#elif defined(USE_SELECT)
	struct timeval tv;
	fd_set rfds, wfds;
	int maxfd;
	#endif

	#if defined(USE_DEVPOLL)
	tmout = dur? HCL_SECNSEC_TO_MSEC(dur->sec, dur->nsec): 0;

	dvp.dp_timeout = tmout; /* milliseconds */
	dvp.dp_fds = xtn->ev.buf;
	dvp.dp_nfds = HCL_COUNTOF(xtn->ev.buf);
	n = ioctl(xtn->ep, DP_POLL, &dvp);

	#elif defined(USE_KQUEUE)

	if (dur)
	{
		ts.tv_sec = dur->sec;
		ts.tv_nsec = dur->nsec;
	}
	else
	{
		ts.tv_sec = 0;
		ts.tv_nsec = 0;
	}

	n = kevent(xtn->ep, HCL_NULL, 0, xtn->ev.buf, HCL_COUNTOF(xtn->ev.buf), &ts);
	/* n == 0: timeout
	 * n == -1: error */

	#elif defined(USE_EPOLL)
	tmout = dur? HCL_SECNSEC_TO_MSEC(dur->sec, dur->nsec): 0;
	n = epoll_wait(xtn->ep, xtn->ev.buf, HCL_COUNTOF(xtn->ev.buf), tmout);

	#elif defined(USE_POLL)
	tmout = dur? HCL_SECNSEC_TO_MSEC(dur->sec, dur->nsec): 0;
	HCL_MEMCPY (xtn->ev.buf, xtn->ev.reg.ptr, xtn->ev.reg.len * HCL_SIZEOF(*xtn->ev.buf));
	n = poll(xtn->ev.buf, xtn->ev.reg.len, tmout);
	if (n > 0)
	{
		/* compact the return buffer as poll() doesn't */
		hcl_oow_t i, j;
		for (i = 0, j = 0; i < xtn->ev.reg.len && j < n; i++)
		{
			if (xtn->ev.buf[i].revents)
			{
				if (j != i) xtn->ev.buf[j] = xtn->ev.buf[i];
				j++;
			}
		}
		n = j;
	}
	#elif defined(USE_SELECT)
	if (dur)
	{
		tv.tv_sec = dur->sec;
		tv.tv_usec = HCL_NSEC_TO_USEC(dur->nsec);
	}
	else
	{
		tv.tv_sec = 0;
		tv.tv_usec = 0;
	}
	maxfd = xtn->ev.reg.maxfd;
	HCL_MEMCPY (&rfds, &xtn->ev.reg.rfds, HCL_SIZEOF(rfds));
	HCL_MEMCPY (&wfds, &xtn->ev.reg.wfds, HCL_SIZEOF(wfds));
	n = select(maxfd + 1, &rfds, &wfds, HCL_NULL, &tv);
	if (n > 0)
	{
		int fd, count = 0;
		for (fd = 0; fd <= maxfd; fd++)
		{
			int events = 0;
			if (FD_ISSET(fd, &rfds)) events |= XPOLLIN;
			if (FD_ISSET(fd, &wfds)) events |= XPOLLOUT;

			if (events)
			{
				HCL_ASSERT (hcl, count < HCL_COUNTOF(xtn->ev.buf));
				xtn->ev.buf[count].fd = fd;
				xtn->ev.buf[count].events = events;
				count++;
			}
		}

		n = count;
		HCL_ASSERT (hcl, n > 0);
	}
	#endif

	if (n <= -1)
	{
	#if defined(__OS2__)
		hcl_seterrwithsyserr (hcl, 2, sock_errno());
		HCL_DEBUG2 (hcl, "Warning: multiplexer wait failure - %d, %js\n", sock_errno(), hcl_geterrmsg(hcl));
	#else
		hcl_seterrwithsyserr (hcl, 0, errno);
		HCL_DEBUG2 (hcl, "Warning: multiplexer wait failure - %d, %js\n", errno, hcl_geterrmsg(hcl));
	#endif
	}
	else
	{
		xtn->ev.len = n;
	}

	/* the muxwcb must be valid all the time in a non-threaded mode */
	HCL_ASSERT (hcl, muxwcb != HCL_NULL);

	while (n > 0)
	{
		int revents;
		hcl_ooi_t mask;

		--n;

	#if defined(USE_DEVPOLL)
		revents = xtn->ev.buf[n].revents;
	#elif defined(USE_KQUEUE)
		if (xtn->ev.buf[n].filter == EVFILT_READ) mask = HCL_SEMAPHORE_IO_MASK_INPUT;
		else if (xtn->ev.buf[n].filter == EVFILT_WRITE) mask = HCL_SEMAPHORE_IO_MASK_OUTPUT;
		else mask = 0;
		goto call_muxwcb_kqueue;
	#elif defined(USE_EPOLL)
		revents = xtn->ev.buf[n].events;
	#elif defined(USE_POLL)
		revents = xtn->ev.buf[n].revents;
	#elif defined(USE_SELECT)
		revents = xtn->ev.buf[n].events;
	#else
		revents = 0; /* TODO: fake. unsupported but to compile on such an unsupported system.*/
	#endif

		mask = 0;
		if (revents & XPOLLIN) mask |= HCL_SEMAPHORE_IO_MASK_INPUT;
		if (revents & XPOLLOUT) mask |= HCL_SEMAPHORE_IO_MASK_OUTPUT;
		if (revents & XPOLLERR) mask |= HCL_SEMAPHORE_IO_MASK_ERROR;
		if (revents & XPOLLHUP) mask |= HCL_SEMAPHORE_IO_MASK_HANGUP;

	#if defined(USE_DEVPOLL)
		muxwcb (hcl, xtn->ev.buf[n].fd, mask);
	#elif defined(USE_KQUEUE)
	call_muxwcb_kqueue:
		muxwcb (hcl, xtn->ev.buf[n].ident, mask);
	#elif defined(USE_EPOLL)
		muxwcb (hcl, xtn->ev.buf[n].data.fd, mask);
	#elif defined(USE_POLL)
		muxwcb (hcl, xtn->ev.buf[n].fd, mask);
	#elif defined(USE_SELECT)
		muxwcb (hcl, xtn->ev.buf[n].fd, mask);
	#endif
	}

	xtn->ev.len = 0;
#endif  /* USE_THREAD */
}

/* -----------------------------------------------------------------
 * SLEEPING
 * ----------------------------------------------------------------- */

#if defined(__DOS__)
#	if defined(_INTELC32_)
	void _halt_cpu (void);
#	elif defined(__WATCOMC__)
	void _halt_cpu (void);
#	pragma aux _halt_cpu = "hlt"
#	elif defined(_MSC_VER)
	static void _halt_cpu (void) { _asm { hlt } }
#	elif defined(__BORLANDC__)
	static void _halt_cpu (void) { _asm { hlt } }
#	endif
#endif

static int vm_sleep (hcl_t* hcl, const hcl_ntime_t* dur)
{
#if defined(_WIN32)
	xtn_t* xtn = GET_XTN(hcl);
	if (xtn->waitable_timer)
	{
		LARGE_INTEGER li;
		li.QuadPart = -(HCL_SECNSEC_TO_NSEC(dur->sec, dur->nsec) / 100); /* in 100 nanoseconds */
		if(SetWaitableTimer(xtn->waitable_timer, &li, 0, HCL_NULL, HCL_NULL, FALSE) == FALSE) goto normal_sleep;
		WaitForSingleObject(xtn->waitable_timer, INFINITE);
	}
	else
	{
	normal_sleep:
		/* fallback to normal Sleep() */
		Sleep (HCL_SECNSEC_TO_MSEC(dur->sec,dur->nsec));
	}
#elif defined(__OS2__)

	/* TODO: in gui mode, this is not a desirable method???
	 *       this must be made event-driven coupled with the main event loop */
	DosSleep (HCL_SECNSEC_TO_MSEC(dur->sec,dur->nsec));

#elif defined(macintosh)

	/* TODO: ... */

#elif defined(__DOS__)

	clock_t c;

	c = clock();
	c += dur->sec * CLOCKS_PER_SEC;

	#if (CLOCKS_PER_SEC == 100)
		c += HCL_NSEC_TO_MSEC(dur->nsec) / 10;
	#elif (CLOCKS_PER_SEC == 1000)
		c += HCL_NSEC_TO_MSEC(dur->nsec);
	#elif (CLOCKS_PER_SEC == 1000000L)
		c += HCL_NSEC_TO_USEC(dur->nsec);
	#elif (CLOCKS_PER_SEC == 1000000000L)
		c += dur->nsec;
	#else
	#	error UNSUPPORTED CLOCKS_PER_SEC
	#endif

/* TODO: handle clock overvlow */
/* TODO: check if there is abortion request or interrupt */
	while (c > clock())
	{
		_halt_cpu();
	}

#elif defined(USE_THREAD)
	/* the sleep callback is called only if there is no IO semaphore
	 * waiting. so i can safely call vm_muxwait() without a muxwait callback
	 * when USE_THREAD is true */
	vm_muxwait (hcl, dur, HCL_NULL);

#elif defined(HAVE_NANOSLEEP)
	{
		struct timespec ts;
		ts.tv_sec = dur->sec;
		ts.tv_nsec = dur->nsec;
		nanosleep (&ts, HCL_NULL);
	}

#elif defined(HAVE_USLEEP)
	usleep (HCL_SECNSEC_TO_USEC(dur->sec, dur->nsec));

#else
#	error UNSUPPORT SLEEP
#endif

	return 0;
}

/* -----------------------------------------------------------------
 * SIGNAL AND TICK HANDLING
 * ----------------------------------------------------------------- */

static hcl_ooi_t vm_getsigfd (hcl_t* hcl)
{
	xtn_t* xtn = GET_XTN(hcl);
	return xtn->sigfd.p[0];
}

static int vm_getsig (hcl_t* hcl, hcl_uint8_t* u8)
{
	xtn_t* xtn = GET_XTN(hcl);
#if defined(_WIN32)
	/* TODO: can i make the pipe non-block in win32? */
	DWORD navail;
	if (PeekNamedPipe(_get_osfhandle(xtn->sigfd.p[0]), HCL_NULL, 0, HCL_NULL, &navail, HCL_NULL) == 0)
	{
		hcl_seterrwithsyserr (hcl, 1, GetLastError());
		return -1;
	}
	if (navail <= 0) return 0;
#endif
	if (read(xtn->sigfd.p[0], u8, HCL_SIZEOF(*u8)) == -1)
	{
	#if defined(EWOULDBLOCK)
		if (errno == EINTR || errno == EWOULDBLOCK || errno == EAGAIN) return 0;
	#else
		if (errno == EINTR || errno == EAGAIN) return 0;
	#endif
		hcl_seterrwithsyserr (hcl, 0, errno);
		return -1;
	}

	return 1;
}

static int vm_setsig (hcl_t* hcl, hcl_uint8_t u8)
{
	xtn_t* xtn = GET_XTN(hcl);
	if (write(xtn->sigfd.p[1], &u8, HCL_SIZEOF(u8)) == -1)
	{
	#if defined(EWOULDBLOCK)
		if (errno == EINTR || errno == EWOULDBLOCK || errno == EAGAIN) return 0;
	#else
		if (errno == EINTR || errno == EAGAIN) return 0;
	#endif
		hcl_seterrwithsyserr (hcl, 0, errno);
		return -1;
	}
	return 1;
}


#if defined(HAVE_SIGACTION)

typedef struct sig_state_t sig_state_t;
struct sig_state_t
{
	hcl_oow_t handler;
	hcl_oow_t old_handler;
	sigset_t  old_sa_mask;
	int       old_sa_flags;
};

typedef void (*sig_handler_t) (int sig);

static sig_state_t g_sig_state[NSIG];

static void dispatch_siginfo (int sig, siginfo_t* si, void* ctx)
{
	if (g_sig_state[sig].handler != (hcl_oow_t)SIG_IGN &&
	    g_sig_state[sig].handler != (hcl_oow_t)SIG_DFL)
	{
		((sig_handler_t)g_sig_state[sig].handler) (sig);
	}

	if (g_sig_state[sig].old_handler &&
	    g_sig_state[sig].old_handler != (hcl_oow_t)SIG_IGN &&
	    g_sig_state[sig].old_handler != (hcl_oow_t)SIG_DFL)
	{
		((void(*)(int, siginfo_t*, void*))g_sig_state[sig].old_handler) (sig, si, ctx);
	}
}

static void dispatch_signal (int sig)
{
	if (g_sig_state[sig].handler != (hcl_oow_t)SIG_IGN &&
	    g_sig_state[sig].handler != (hcl_oow_t)SIG_DFL)
	{
		((sig_handler_t)g_sig_state[sig].handler) (sig);
	}

	if (g_sig_state[sig].old_handler &&
	    g_sig_state[sig].old_handler != (hcl_oow_t)SIG_IGN &&
	    g_sig_state[sig].old_handler != (hcl_oow_t)SIG_DFL)
	{
		((sig_handler_t)g_sig_state[sig].old_handler) (sig);
	}
}

static int set_signal_handler (int sig, sig_handler_t handler, int extra_flags)
{
	if (g_sig_state[sig].handler)
	{
		/* already set - allow handler change. ignore extra_flags. */
		if (g_sig_state[sig].handler == (hcl_oow_t)handler) return -1;
		g_sig_state[sig].handler = (hcl_oow_t)handler;
	}
	else
	{
		struct sigaction sa, oldsa;

		if (sigaction(sig, HCL_NULL, &oldsa) == -1) return -1;

		HCL_MEMSET (&sa, 0, HCL_SIZEOF(sa));
		if (oldsa.sa_flags & SA_SIGINFO)
		{
			sa.sa_sigaction = dispatch_siginfo;
			sa.sa_flags = SA_SIGINFO;
		}
		else
		{
			sa.sa_handler = dispatch_signal;
			sa.sa_flags = 0;
		}
		sa.sa_flags |= extra_flags;
		/*sa.sa_flags |= SA_INTERUPT;
		sa.sa_flags |= SA_RESTART;*/
		sigfillset (&sa.sa_mask); /* block all signals while the handler is being executed */

		if (sigaction(sig, &sa, HCL_NULL) == -1) return -1;

		g_sig_state[sig].handler = (hcl_oow_t)handler;
		if (oldsa.sa_flags & SA_SIGINFO)
			g_sig_state[sig].old_handler = (hcl_oow_t)oldsa.sa_sigaction;
		else
			g_sig_state[sig].old_handler = (hcl_oow_t)oldsa.sa_handler;

		g_sig_state[sig].old_sa_mask = oldsa.sa_mask;
		g_sig_state[sig].old_sa_flags = oldsa.sa_flags;
	}

	return 0;
}

static int unset_signal_handler (int sig)
{
	struct sigaction sa;

	if (!g_sig_state[sig].handler) return -1; /* not set */

	HCL_MEMSET (&sa, 0, HCL_SIZEOF(sa));
	sa.sa_mask = g_sig_state[sig].old_sa_mask;
	sa.sa_flags = g_sig_state[sig].old_sa_flags;

	if (sa.sa_flags & SA_SIGINFO)
	{
		sa.sa_sigaction = (void(*)(int,siginfo_t*,void*))g_sig_state[sig].old_handler;
	}
	else
	{
		sa.sa_handler = (sig_handler_t)g_sig_state[sig].old_handler;
	}

	if (sigaction(sig, &sa, HCL_NULL) == -1) return -1;

	g_sig_state[sig].handler = 0;
	/* keep other fields untouched */

	return 0;
}

static int is_signal_handler_set (int sig)
{
	return !!g_sig_state[sig].handler;
}
#endif


static HCL_INLINE void abort_all_hcls (int signo)
{
	/* TODO: make this atomic */
	if (g_hcl)
	{
		hcl_t* hcl = g_hcl;
		do
		{
			xtn_t* xtn = GET_XTN(hcl);
			hcl_uint8_t u8;
			/*hcl_abortstd (hcl);*/
			u8 = signo & 0xFF;
			write (xtn->sigfd.p[1], &u8, HCL_SIZEOF(u8));
			hcl = xtn->next;
		}
		while (hcl);
	}
	/* TODO: make this atomic */
}

static HCL_INLINE void do_nothing (int unused)
{
}

/*#define HCL_TICKER_INTERVAL_USECS 10000*/ /* microseconds. 0.01 seconds */
#define HCL_TICKER_INTERVAL_USECS 20000 /* microseconds. 0.02 seconds. */

static HCL_INLINE void swproc_all_hcls (int unused)
{
	/* TODO: make this atomic */
	if (g_hcl)
	{
		hcl_t* hcl = g_hcl;
		do
		{
			xtn_t* xtn = GET_XTN(hcl);
			if (xtn->rcv_tick) hcl_switchprocess (hcl);
			hcl = xtn->next;
		}
		while (hcl);
	}
	/* TODO: make this atomic */
}

#if defined(_WIN32)

static HANDLE msw_tick_timer = HCL_NULL; /*INVALID_HANDLE_VALUE;*/
static int msw_tick_done = 0;

static DWORD WINAPI msw_wait_for_timer_event (LPVOID ctx)
{
	/* I don't think i need to use the waiting timer for this.
	 * a simple loop with sleep inside should also work as i don't do anything
	 * special except waiting for timer expiry.
	 *   while (!msw_tick_done)
	 *   {
	 *       Sleep (...);
	 *       swproc_all_hcls();
	 *   }
	 * but never mind for now. let's do it the hard way.
	 */

	msw_tick_timer = CreateWaitableTimer(HCL_NULL, FALSE, HCL_NULL);
	if (msw_tick_timer)
	{
		LARGE_INTEGER li;

		/* lpDueTime in 100 nanoseconds */
		li.QuadPart = -HCL_USEC_TO_NSEC(HCL_TICKER_INTERVAL_USECS) / 100;

	/*#define MSW_TICKER_MANUAL_RESET */
	#if defined(MSW_TICKER_MANUAL_RESET)
		/* if manual resetting is enabled, the reset is done after
		 * swproc_all_hcls has been called. so the interval is the
		 * interval specified plus the time taken in swproc_all_hcls. */
		SetWaitableTimer (msw_tick_timer, &li, 0, HCL_NULL, HCL_NULL, FALSE);
	#else
		/* with auto reset, the interval is not affected by time taken
		 * in swproc_all_hcls() */
		SetWaitableTimer (msw_tick_timer, &li, HCL_USEC_TO_MSEC(HCL_TICKER_INTERVAL_USECS), HCL_NULL, HCL_NULL, FALSE);
	#endif

		while (!msw_tick_done)
		{
			if (WaitForSingleObject(msw_tick_timer, 100000) == WAIT_OBJECT_0)
			{
				swproc_all_hcls (0);
			#if defined(MSW_TICKER_MANUAL_RESET)
				SetWaitableTimer (msw_tick_timer, &li, 0, HCL_NULL, HCL_NULL, FALSE);
			#endif
			}
		}

		CancelWaitableTimer (msw_tick_timer);

		CloseHandle (msw_tick_timer);
		msw_tick_timer = HCL_NULL;
	}

	msw_tick_done = 0;
	/*ExitThread (0);*/
	return 0;
}

static HCL_INLINE void start_ticker (void)
{
	HANDLE thr;

	msw_tick_done = 0;

	thr = CreateThread(HCL_NULL, 0, msw_wait_for_timer_event, HCL_NULL, 0, HCL_NULL);
	if (thr)
	{
		SetThreadPriority (thr, THREAD_PRIORITY_HIGHEST);

		/* MSDN - The thread object remains in the system until the thread has terminated
		 *        and all handles to it have been closed through a call to CloseHandle.
		 * it is safe to close the handle here */
		CloseHandle (thr);
	}
}

static HCL_INLINE void stop_ticker (void)
{
	if (msw_tick_timer) CancelWaitableTimer (msw_tick_timer);
	msw_tick_done = 1;
}

#elif defined(__OS2__)

static HEV os2_tick_sem;
static HTIMER os2_tick_timer;
static int os2_tick_done = 0;

static void EXPENTRY os2_wait_for_timer_event (ULONG x)
{
	APIRET rc;
	ULONG count;

	rc = DosCreateEventSem(HCL_NULL, &os2_tick_sem, DC_SEM_SHARED, FALSE);
	if (rc != NO_ERROR)
	{
		goto done;
	}

	rc = DosStartTimer(HCL_USEC_TO_MSEC(HCL_TICKER_INTERVAL_USECS), (HSEM)os2_tick_sem, &os2_tick_timer);
	if (rc != NO_ERROR)
	{
		DosCloseEventSem (os2_tick_sem);
		goto done;
	}

	while (!os2_tick_done)
	{
		rc = DosWaitEventSem(os2_tick_sem, 5000L);
	#if 0
		swproc_all_hcls (0);
		DosResetEventSem (os2_tick_sem, &count);
	#else
		DosResetEventSem (os2_tick_sem, &count);
		swproc_all_hcls (0);
	#endif
	}

	DosStopTimer (os2_tick_timer);
	DosCloseEventSem (os2_tick_sem);

done:
	os2_tick_timer = NULL;
	os2_tick_sem = NULL;
	os2_tick_done = 0;
	DosExit (EXIT_THREAD, 0);
}

static HCL_INLINE void start_ticker (void)
{
	static TID tid;
	os2_tick_done = 0;
	DosCreateThread (&tid, os2_wait_for_timer_event, 0, 0, 4096);
	/* TODO: Error check */
}

static HCL_INLINE void stop_ticker (void)
{
	if (os2_tick_sem) DosPostEventSem (os2_tick_sem);
	os2_tick_done = 1;
}

#elif defined(__DOS__) && (defined(_INTELC32_) || defined(__WATCOMC__) || defined(__BORLANDC__))

#if defined(_INTELC32_)
static void (*dos_prev_timer_intr_handler) (void);
#pragma interrupt(dos_timer_intr_handler)
static void dos_timer_intr_handler (void)
#elif defined(__WATCOMC__)
static void (__interrupt *dos_prev_timer_intr_handler) (void);
static void __interrupt dos_timer_intr_handler (void)
#else
static void (interrupt *dos_prev_timer_intr_handler) (void);
static void interrupt dos_timer_intr_handler (void)
#endif
{
	/*
	_XSTACK* stk = (_XSTACK *)_get_stk_frame();
	r = (unsigned short)stk->eax;
	*/

	/* The timer interrupt (normally) occurs 18.2 times per second. */
	swproc_all_hcls (0);
	_chain_intr (dos_prev_timer_intr_handler);
}

static HCL_INLINE void start_ticker (void)
{
	dos_prev_timer_intr_handler = _dos_getvect(0x1C);
	_dos_setvect (0x1C, dos_timer_intr_handler);
}

static HCL_INLINE void stop_ticker (void)
{
	_dos_setvect (0x1C, dos_prev_timer_intr_handler);
}

#elif defined(macintosh)

static TMTask mac_tmtask;
static ProcessSerialNumber mac_psn;

/* milliseconds if positive, microseconds(after negation) if negative */
#define TMTASK_DELAY HCL_USEC_TO_MSEC(HCL_TICKER_INTERVAL_USECS)

static pascal void timer_intr_handler (TMTask* task)
{
	swproc_all_hcls (0);
	WakeUpProcess (&mac_psn);
	PrimeTime ((QElem*)&mac_tmtask, TMTASK_DELAY);
}

static HCL_INLINE void start_ticker (void)
{
	GetCurrentProcess (&mac_psn);
	HCL_MEMSET (&mac_tmtask, 0, HCL_SIZEOF(mac_tmtask));
	mac_tmtask.tmAddr = NewTimerProc (timer_intr_handler);
	InsXTime ((QElem*)&mac_tmtask);
	PrimeTime ((QElem*)&mac_tmtask, TMTASK_DELAY);
}

static HCL_INLINE void stop_ticker (void)
{
	RmvTime ((QElem*)&mac_tmtask);
	/*DisposeTimerProc (mac_tmtask.tmAddr);*/
}

#elif defined(HAVE_SETITIMER) && defined(SIGVTALRM) && defined(ITIMER_VIRTUAL)

static HCL_INLINE void start_ticker (void)
{
	if (set_signal_handler(SIGVTALRM, swproc_all_hcls, SA_RESTART) >= 0)
	{
		struct itimerval itv;
		itv.it_interval.tv_sec = 0;
		itv.it_interval.tv_usec = HCL_TICKER_INTERVAL_USECS;
		itv.it_value.tv_sec = 0;
		itv.it_value.tv_usec = HCL_TICKER_INTERVAL_USECS;
		if (setitimer(ITIMER_VIRTUAL, &itv, HCL_NULL) == -1)
		{
			/* WSL supports ITIMER_VIRTUAL only as of windows 10.0.18362.413.
			   the following is a fallback which will get */
			unset_signal_handler (SIGVTALRM);

		#if defined(SIGALRM) && defined(ITIMER_REAL)
			if (set_signal_handler(SIGALRM, swproc_all_hcls, SA_RESTART) >= 0)
			{
				/* i double the interval as ITIMER_REAL is against the wall clock.
				 * if the underlying system is under heavy load, some signals
				 * will get lost */
				itv.it_interval.tv_sec = 0;
				itv.it_interval.tv_usec = HCL_TICKER_INTERVAL_USECS * 2;
				itv.it_value.tv_sec = 0;
				itv.it_value.tv_usec = HCL_TICKER_INTERVAL_USECS * 2;
				setitimer(ITIMER_REAL, &itv, HCL_NULL);
			}
		#endif
		}
	}
}

static HCL_INLINE void stop_ticker (void)
{
	/* ignore the signal fired by the activated timer.
	 * unsetting the signal may cause the program to terminate(default action) */
	if (is_signal_handler_set(SIGVTALRM) && set_signal_handler(SIGVTALRM, SIG_IGN, 0) >= 0)
	{
		struct itimerval itv;
		itv.it_interval.tv_sec = 0;
		itv.it_interval.tv_usec = 0;
		itv.it_value.tv_sec = 0; /* make setitimer() one-shot only */
		itv.it_value.tv_usec = 0;
		setitimer (ITIMER_VIRTUAL, &itv, HCL_NULL);
	}

	#if defined(SIGALRM) && defined(ITIMER_REAL)
	if (is_signal_handler_set(SIGALRM) && set_signal_handler(SIGALRM, SIG_IGN, 0) >= 0)
	{
		struct itimerval itv;
		itv.it_interval.tv_sec = 0;
		itv.it_interval.tv_usec = 0;
		itv.it_value.tv_sec = 0; /* make setitimer() one-shot only */
		itv.it_value.tv_usec = 0;
		setitimer (ITIMER_REAL, &itv, HCL_NULL);
	}
	#endif
}

#else

static pid_t ticker_pid = -1;

static HCL_INLINE void start_ticker (void)
{
#if defined(SIGALRM)
	if (set_signal_handler(SIGALRM, swproc_all_hcls, SA_RESTART) >= 0)
	{
		ticker_pid = fork();

		if (ticker_pid <= -1)
		{
			unset_signal_handler (SIGALRM);
		}
		else if (ticker_pid == 0)
		{
			/* child process - actual ticker */
			while (1)
			{
			#if defined(HAVE_NANOSLEEP)
				struct timespec ts;
				ts.tv_sec = 0;
				ts.tv_nsec = HCL_USEC_TO_NSEC(HCL_TICKER_INTERVAL_USECS) * 2;
				nanosleep (&ts, HCL_NULL);
			#elif defined(HAVE_USLEEP)
				usleep (HCL_TICKER_INTERVAL_USECS * 2);

			#else
			#	error UNDEFINED SLEEP
			#endif

				kill (getppid(), SIGALRM);
			}

			_exit (0);
		}

		/* parent just carries on. */
	}
#endif
}

static HCL_INLINE void stop_ticker (void)
{
#if defined(SIGALRM)
	if (ticker_pid >= 0)
	{
		int wstatus;
		kill (ticker_pid, SIGKILL);
		while (waitpid(ticker_pid, &wstatus, 0) != ticker_pid);
		ticker_pid = -1;

		unset_signal_handler (SIGALRM);
	}
#endif
}

#endif

/* -----------------------------------------------------------------
 * SHARED LIBRARY HANDLING
 * ----------------------------------------------------------------- */

#if defined(USE_LTDL)
#	define sys_dl_error() lt_dlerror()
#	define sys_dl_open(x) lt_dlopen(x)
#	define sys_dl_openext(x) lt_dlopenext(x)
#	define sys_dl_close(x) lt_dlclose(x)
#	define sys_dl_getsym(x,n) lt_dlsym(x,n)

#elif defined(USE_DLFCN)
#	define sys_dl_error() dlerror()
#	define sys_dl_open(x) dlopen(x,RTLD_NOW)
#	define sys_dl_openext(x) dlopen(x,RTLD_NOW)
#	define sys_dl_close(x) dlclose(x)
#	define sys_dl_getsym(x,n) dlsym(x,n)

#elif defined(USE_WIN_DLL)
#	define sys_dl_error() msw_dlerror()
#	define sys_dl_open(x) LoadLibraryExA(x, HCL_NULL, 0)
#	define sys_dl_openext(x) LoadLibraryExA(x, HCL_NULL, 0)
#	define sys_dl_close(x) FreeLibrary(x)
#	define sys_dl_getsym(x,n) GetProcAddress(x,n)

#elif defined(USE_MACH_O_DYLD)
#	define sys_dl_error(void) mach_dlerror()
#	define sys_dl_open(x) mach_dlopen(x)
#	define sys_dl_openext(x) mach_dlopen(x)
#	define sys_dl_close(x) mach_dlclose(x)
#	define sys_dl_getsym(x,n) mach_dlsym(x,n)

#else
static const char* sys_dl_error(void) { return HCL_NULL; }
static void* sys_dl_open(const char* x) { return HCL_NULL; }
static void* sys_dl_openext(const char* x) { return HCL_NULL; }
static void sys_dl_close(void* x) { return HCL_NULL; }
static void* sys_dl_getsym(void* x, const char* n) { return HCL_NULL; }
#endif

#if defined(USE_WIN_DLL)

static const char* msw_dlerror (void)
{
	/* TODO: handle wchar_t, hcl_ooch_t etc? */
	static char buf[256];
	DWORD rc;

	rc = FormatMessageA (
		FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL, GetLastError(), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		buf, HCL_COUNTOF(buf), HCL_NULL
	);
	while (rc > 0 && buf[rc - 1] == '\r' || buf[rc - 1] == '\n')
	{
		buf[--rc] = '\0';
	}
	return buf;
}

#elif defined(USE_MACH_O_DYLD)
static const char* mach_dlerror_str = "";

static void* mach_dlopen (const char* path)
{
	NSObjectFileImage image;
	NSObjectFileImageReturnCode rc;
	void* handle;

	mach_dlerror_str = "";
	if ((rc = NSCreateObjectFileImageFromFile(path, &image)) != NSObjectFileImageSuccess)
	{
		switch (rc)
		{
			case NSObjectFileImageFailure:
			case NSObjectFileImageFormat:
				mach_dlerror_str = "unable to crate object file image";
				break;

			case NSObjectFileImageInappropriateFile:
				mach_dlerror_str = "inappropriate file";
				break;

			case NSObjectFileImageArch:
				mach_dlerror_str = "incompatible architecture";
				break;

			case NSObjectFileImageAccess:
				mach_dlerror_str = "inaccessible file";
				break;

			default:
				mach_dlerror_str = "unknown error";
				break;
		}
		return HCL_NULL;
	}
	handle = (void*)NSLinkModule(image, path, NSLINKMODULE_OPTION_PRIVATE | NSLINKMODULE_OPTION_RETURN_ON_ERROR);
	NSDestroyObjectFileImage (image);
	return handle;
}

static HCL_INLINE void mach_dlclose (void* handle)
{
	mach_dlerror_str = "";
	NSUnLinkModule (handle, NSUNLINKMODULE_OPTION_NONE);
}

static HCL_INLINE void* mach_dlsym (void* handle, const char* name)
{
	mach_dlerror_str = "";
	return (void*)NSAddressOfSymbol(NSLookupSymbolInModule(handle, name));
}

static const char* mach_dlerror (void)
{
	int err_no;
	const char* err_file;
	NSLinkEditErrors err;

	if (mach_dlerror_str[0] == '\0')
		NSLinkEditError (&err, &err_no, &err_file, &mach_dlerror_str);

	return mach_dlerror_str;
}
#endif

static void dl_startup (hcl_t* hcl)
{
#if defined(USE_LTDL)
	lt_dlinit ();
#endif
}

static void dl_cleanup (hcl_t* hcl)
{
#if defined(USE_LTDL)
	lt_dlexit ();
#endif
}

static void* dlopen_pfmod (hcl_t* hcl, const hcl_ooch_t* name, const hcl_ooch_t* dirptr, const hcl_oow_t dirlen, hcl_bch_t* bufptr, hcl_oow_t bufcapa)
{
	void* handle;
	hcl_oow_t len, i, xlen, dlen;
	hcl_oow_t ucslen, bcslen;

	/* opening a primitive function module - mostly libhcl-xxxx.
	 * if PFMODPREFIX is absolute, never use PFMODDIR */
	if (HCL_IS_PATH_ABSOLUTE(HCL_DEFAULT_PFMODPREFIX))
	{
		dlen = 0;
		len = hcl_copy_bcstr(&bufptr[dlen], bufcapa - dlen, HCL_DEFAULT_PFMODPREFIX);
	}
	else if (dirptr)
	{
		xlen = dirlen;
		dlen = bufcapa;
		if (hcl_convootobchars(hcl, dirptr, &xlen, bufptr, &dlen) <= -1) return HCL_NULL;

		if (dlen > 0 && bufptr[dlen - 1] != HCL_DFL_PATH_SEP)
		{
		#if defined(HCL_HAVE_ALT_PATH_SEP)
			if (hcl_find_bchar(bufptr, dlen, HCL_ALT_PATH_SEP) &&
			    !hcl_find_bchar(bufptr, dlen, HCL_DFL_PATH_SEP))
				bufptr[dlen++] = HCL_ALT_PATH_SEP;
			else
				bufptr[dlen++] = HCL_DFL_PATH_SEP;
		#endif
			bufptr[dlen++] = HCL_DFL_PATH_SEP;
		}
		len = hcl_copy_bcstr(&bufptr[dlen], bufcapa - dlen, HCL_DEFAULT_PFMODPREFIX);
		len += dlen;
	}
	else
	{
		dlen = hcl_copy_bcstr(bufptr, bufcapa, HCL_DEFAULT_PFMODDIR);
		len = hcl_copy_bcstr(&bufptr[dlen], bufcapa - dlen, HCL_DEFAULT_PFMODPREFIX);
		len += dlen;
	}

	bcslen = bufcapa - len;
#if defined(HCL_OOCH_IS_UCH)
	hcl_convootobcstr(hcl, name, &ucslen, &bufptr[len], &bcslen);
#else
	bcslen = hcl_copy_bcstr(&bufptr[len], bcslen, name);
#endif

	/* length including the directory, the prefix and the name. but excluding the postfix */
	xlen  = len + bcslen;

	for (i = len; i < xlen; i++)
	{
		/* convert a period(.) to a dash(-) */
		if (bufptr[i] == '.') bufptr[i] = '-';
	}

retry:
	hcl_copy_bcstr (&bufptr[xlen], bufcapa - xlen, HCL_DEFAULT_PFMODPOSTFIX);

	/* both prefix and postfix attached. for instance, libhcl-xxx */
	HCL_DEBUG3 (hcl, "Opening(ext) PFMOD %hs[%js] - %hs\n", &bufptr[dlen], name, bufptr);
	handle = sys_dl_openext(bufptr);
	if (!handle)
	{
		HCL_DEBUG3 (hcl, "Unable to open(ext) PFMOD %hs[%js] - %hs\n", &bufptr[dlen], name, sys_dl_error());

		if (dlen > 0)
		{
			handle = sys_dl_openext(&bufptr[0]);
			if (handle) goto pfmod_open_ok;
			HCL_DEBUG3 (hcl, "Unable to open(ext) PFMOD %hs[%js] - %hs\n", &bufptr[0], name, sys_dl_error());
		}

		/* try without prefix and postfix */
		bufptr[xlen] = '\0';
		handle = sys_dl_openext(&bufptr[len]);
		if (!handle)
		{
			hcl_bch_t* dash;
			const hcl_bch_t* dl_errstr;
			dl_errstr = sys_dl_error();
			HCL_DEBUG3 (hcl, "Unable to open(ext) PFMOD %hs[%js] - %hs\n", &bufptr[len], name, dl_errstr);
			hcl_seterrbfmt (hcl, HCL_ESYSERR, "unable to open(ext) PFMOD %js - %hs", name, dl_errstr);

			dash = hcl_rfind_bchar(bufptr, hcl_count_bcstr(bufptr), '-');
			if (dash)
			{
				/* remove a segment at the back.
				 * [NOTE] a dash contained in the original name before
				 *        period-to-dash transformation may cause extraneous/wrong
				 *        loading reattempts. */
				xlen = dash - bufptr;
				goto retry;
			}
		}
		else
		{
			HCL_DEBUG3 (hcl, "Opened(ext) PFMOD %hs[%js] handle %p\n", &bufptr[len], name, handle);
		}
	}
	else
	{
	pfmod_open_ok:
		HCL_DEBUG3 (hcl, "Opened(ext) PFMOD %hs[%js] handle %p\n", &bufptr[dlen], name, handle);
	}

	return handle;
}

static void* dlopen_raw (hcl_t* hcl, const hcl_ooch_t* name, hcl_bch_t* bufptr, hcl_oow_t bufcapa)
{
	void* handle;
	hcl_oow_t ucslen, bcslen;

#if defined(HCL_OOCH_IS_UCH)
	bcslen = bufcapa;
	hcl_convootobcstr(hcl, name, &ucslen, bufptr, &bcslen);
#else
	bcslen = hcl_copy_bcstr(bufptr, bufcapa, name);
#endif

	if (hcl_find_bchar(bufptr, bcslen, '.'))
	{
		handle = sys_dl_open(bufptr);
		if (!handle)
		{
			const hcl_bch_t* dl_errstr;
			dl_errstr = sys_dl_error();
			HCL_DEBUG2 (hcl, "Unable to open DL %hs - %hs\n", bufptr, dl_errstr);
			hcl_seterrbfmt (hcl, HCL_ESYSERR, "unable to open DL %js - %hs", name, dl_errstr);
		}
		else HCL_DEBUG2 (hcl, "Opened DL %hs handle %p\n", bufptr, handle);
	}
	else
	{
		handle = sys_dl_openext(bufptr);
		if (!handle)
		{
			const hcl_bch_t* dl_errstr;
			dl_errstr = sys_dl_error();
			HCL_DEBUG2 (hcl, "Unable to open(ext) DL %hs - %hs\n", bufptr, dl_errstr);
			hcl_seterrbfmt (hcl, HCL_ESYSERR, "unable to open(ext) DL %js - %hs", name, dl_errstr);
		}
		else HCL_DEBUG2 (hcl, "Opened(ext) DL %hs handle %p\n", bufptr, handle);
	}

	return handle;
}

static void* dl_open (hcl_t* hcl, const hcl_ooch_t* name, int flags)
{
#if defined(USE_LTDL) || defined(USE_DLFCN) || defined(USE_MACH_O_DYLD)
	hcl_bch_t stabuf[128], * bufptr;
	hcl_oow_t ucslen, bcslen, bufcapa;
	void* handle = HCL_NULL;

	#if defined(HCL_OOCH_IS_UCH)
	if (hcl_convootobcstr(hcl, name, &ucslen, HCL_NULL, &bufcapa) <= -1) return HCL_NULL;

	if (hcl->option.mod[0].len > 0)
	{
		/* multiple directories separated by a colon can be specified for HCL_MOD_LIBDIRS
		 * however, use the total length to secure space just for simplicity */
		ucslen = hcl->option.mod[0].len;
		if (hcl_convootobchars(hcl, hcl->option.mod[0].ptr, &ucslen, HCL_NULL, &bcslen) <= -1) return HCL_NULL;
		bufcapa += bcslen;
	}
	#else
	bufcapa = hcl_count_bcstr(name);
	bufcapa += (hcl->option.mod[0].len > 0)? hcl->option.mod[0].len: HCL_COUNTOF(HCL_DEFAULT_PFMODDIR);
	#endif

	/* HCL_COUNTOF(HCL_DEFAULT_PFMODPREFIX) and HCL_COUNTOF(HCL_DEFAULT_PFMODPOSTIFX)
	 * include the terminating nulls. Never mind about the extra 2 characters. */
	bufcapa += HCL_COUNTOF(HCL_DEFAULT_PFMODPREFIX) + HCL_COUNTOF(HCL_DEFAULT_PFMODPOSTFIX) + 1;

	if (bufcapa <= HCL_COUNTOF(stabuf)) bufptr = stabuf;
	else
	{
		bufptr = (hcl_bch_t*)hcl_allocmem(hcl, bufcapa * HCL_SIZEOF(*bufptr));
		if (!bufptr) return HCL_NULL;
	}

	if (flags & HCL_VMPRIM_DLOPEN_PFMOD)
	{
		if (hcl->option.mod[0].len > 0)
		{
			const hcl_ooch_t* ptr, * end, * seg;

			ptr = hcl->option.mod[0].ptr;
			end = hcl->option.mod[0].ptr + hcl->option.mod[0].len;
			seg = ptr;

			while (ptr <= end)
			{
				if (ptr == end || *ptr == ':')
				{
					if (ptr - seg > 0)
					{
						handle = dlopen_pfmod(hcl, name, seg, ptr - seg, bufptr, bufcapa);
						if (handle) break;
					}

					if (ptr == end) break;
					seg = ptr + 1;
				}
				ptr++;
			}

		}

		if (!handle) handle = dlopen_pfmod(hcl, name, HCL_NULL, 0, bufptr, bufcapa);
	}
	else
	{
		/* opening a raw shared object without a prefix and/or a postfix */
		handle = dlopen_raw(hcl, name, bufptr, bufcapa);
	}

	if (bufptr != stabuf) hcl_freemem (hcl, bufptr);
	return handle;

#else

/* TODO: support various platforms */
	/* TODO: implemenent this */
	HCL_DEBUG1 (hcl, "Dynamic loading not implemented - cannot open %js\n", name);
	hcl_seterrbfmt (hcl, HCL_ENOIMPL, "dynamic loading not implemented - cannot open %js", name);
	return HCL_NULL;
#endif
}

static void dl_close (hcl_t* hcl, void* handle)
{
#if defined(USE_LTDL) || defined(USE_DLFCN) || defined(USE_MACH_O_DYLD)
	HCL_DEBUG1 (hcl, "Closed DL handle %p\n", handle);
	sys_dl_close (handle);

#else
	/* TODO: implemenent this */
	HCL_DEBUG1 (hcl, "Dynamic loading not implemented - cannot close handle %p\n", handle);
#endif
}

static void* dl_getsym (hcl_t* hcl, void* handle, const hcl_ooch_t* name)
{
#if defined(USE_LTDL) || defined(USE_DLFCN) || defined(USE_MACH_O_DYLD)
	hcl_bch_t stabuf[64], * bufptr;
	hcl_oow_t bufcapa, ucslen, bcslen, i;
	const hcl_bch_t* symname;
	void* sym;

	#if defined(HCL_OOCH_IS_UCH)
	if (hcl_convootobcstr(hcl, name, &ucslen, HCL_NULL, &bcslen) <= -1) return HCL_NULL;
	#else
	bcslen = hcl_count_bcstr (name);
	#endif

	if (bcslen >= HCL_COUNTOF(stabuf) - 2)
	{
		bufcapa = bcslen + 3;
		bufptr = (hcl_bch_t*)hcl_allocmem(hcl, bufcapa * HCL_SIZEOF(*bufptr));
		if (!bufptr) return HCL_NULL;
	}
	else
	{
		bufcapa = HCL_COUNTOF(stabuf);
		bufptr = stabuf;
	}

	bcslen = bufcapa - 1;
	#if defined(HCL_OOCH_IS_UCH)
	hcl_convootobcstr (hcl, name, &ucslen, &bufptr[1], &bcslen);
	#else
	bcslen = hcl_copy_bcstr(&bufptr[1], bcslen, name);
	#endif

	/* convert a period(.) to an underscore(_) */
	for (i = 1; i <= bcslen; i++) if (bufptr[i] == '.') bufptr[i] = '_';

	symname = &bufptr[1]; /* try the name as it is */
	sym = sys_dl_getsym(handle, symname);
	if (!sym)
	{
		bufptr[0] = '_';
		symname = &bufptr[0]; /* try _name */
		sym = sys_dl_getsym(handle, symname);
		if (!sym)
		{
			bufptr[bcslen + 1] = '_';
			bufptr[bcslen + 2] = '\0';

			symname = &bufptr[1]; /* try name_ */
			sym = sys_dl_getsym(handle, symname);

			if (!sym)
			{
				symname = &bufptr[0]; /* try _name_ */
				sym = sys_dl_getsym(handle, symname);
				if (!sym)
				{
					const hcl_bch_t* dl_errstr;
					dl_errstr = sys_dl_error();
					HCL_DEBUG3 (hcl, "Failed to get module symbol %js from handle %p - %hs\n", name, handle, dl_errstr);
					hcl_seterrbfmt (hcl, HCL_ENOENT, "unable to get module symbol %hs - %hs", symname, dl_errstr);

				}
			}
		}
	}

	if (sym) HCL_DEBUG3 (hcl, "Loaded module symbol %js from handle %p - %hs\n", name, handle, symname);
	if (bufptr != stabuf) hcl_freemem (hcl, bufptr);
	return sym;

#else
	/* TODO: IMPLEMENT THIS */
	HCL_DEBUG2 (hcl, "Dynamic loading not implemented - Cannot load module symbol %js from handle %p\n", name, handle);
	hcl_seterrbfmt (hcl, HCL_ENOIMPL, "dynamic loading not implemented - Cannot load module symbol %js from handle %p", name, handle);
	return HCL_NULL;
#endif
}

/* -----------------------------------------------------------------
 * EVENT CALLBACKS
 * ----------------------------------------------------------------- */

/*#define ENABLE_LOG_INITIALLY*/

static HCL_INLINE void reset_log_to_default (xtn_t* xtn)
{
#if defined(ENABLE_LOG_INITIALLY)
	xtn->log.fd = STDERR_FILENO;
	xtn->log.fd_flags = 0;
	#if defined(HAVE_ISATTY)
	if (isatty(xtn->log.fd)) xtn->log.fd_flags |= LOGFD_TTY;
	#endif
#else
	xtn->log.fd = -1;
	xtn->log.fd_flags = 0;
#endif

#if defined(HAVE_ISATTY)
	if (isatty(STDERR_FILENO)) xtn->log.fd_flags |= LOGFD_STDERR_TTY;
	if (isatty(STDOUT_FILENO)) xtn->log.fd_flags |= LOGFD_STDOUT_TTY;
#endif
}

static HCL_INLINE void chain (hcl_t* hcl)
{
        xtn_t* xtn = GET_XTN(hcl);

        /* TODO: make this atomic */
        xtn->prev = HCL_NULL;
        xtn->next = g_hcl;

        if (g_hcl) GET_XTN(g_hcl)->prev = hcl;
        else g_hcl = hcl;
        /* TODO: make this atomic */
}

static HCL_INLINE void unchain (hcl_t* hcl)
{
        xtn_t* xtn = GET_XTN(hcl);

        /* TODO: make this atomic */
        if (xtn->prev) GET_XTN(xtn->prev)->next = xtn->next;
        else g_hcl = xtn->next;
        if (xtn->next) GET_XTN(xtn->next)->prev = xtn->prev;
        /* TODO: make this atomic */
        xtn->prev = HCL_NULL;
        xtn->prev = HCL_NULL;
}

static void cb_on_fini (hcl_t* hcl)
{
	xtn_t* xtn = GET_XTN(hcl);
	if ((xtn->log.fd_flags & LOGFD_OPENED_HERE) && xtn->log.fd >= 0) close (xtn->log.fd);
	reset_log_to_default (xtn);
	unchain (hcl);
}

static void cb_halting (hcl_t* hcl)
{
	xtn_t* xtn = GET_XTN(hcl);
	xtn->ev.halting = 1; /* once set, vm_sleep() is supposed to return without waiting */
}

static void cb_on_option (hcl_t* hcl, hcl_option_t id, const void* value)
{
	xtn_t* xtn = GET_XTN(hcl);
	int fd;

	if (id != HCL_LOG_TARGET_BCSTR && id != HCL_LOG_TARGET_UCSTR &&
	    id != HCL_LOG_TARGET_BCS && id != HCL_LOG_TARGET_UCS) return; /* return success. not interested */

#if defined(_WIN32)
	#if defined(HCL_OOCH_IS_UCH) && (HCL_SIZEOF_UCH_T == HCL_SIZEOF_WCHAR_T)
	fd = _wopen((const wchar_t*)hcl->option.log_target_u, _O_CREAT | _O_WRONLY | _O_APPEND | _O_BINARY , 0644);
	#else
	fd = _open(hcl->option.log_target_b, _O_CREAT | _O_WRONLY | _O_APPEND | _O_BINARY , 0644);
	#endif
#else
	fd = open(hcl->option.log_target_b, O_CREAT | O_WRONLY | O_APPEND , 0644);
#endif
	if (fd == -1)
	{
		/* TODO: any warning that log file not opened??? */
	}
	else
	{
		if ((xtn->log.fd_flags & LOGFD_OPENED_HERE) && xtn->log.fd >= 0) close (xtn->log.fd);

		xtn->log.fd = fd;
		xtn->log.fd_flags &= ~LOGFD_TTY;
		xtn->log.fd_flags |= LOGFD_OPENED_HERE;
	#if defined(HAVE_ISATTY)
		if (isatty(xtn->log.fd))
		{
			xtn->log.fd_flags |= LOGFD_TTY;
			if (fd == STDERR_FILENO) xtn->log.fd_flags |= LOGFD_STDERR_TTY;
			else if (fd == STDOUT_FILENO) xtn->log.fd_flags |= LOGFD_STDOUT_TTY;
		}
	#endif
	}
}

#if defined(__OS2__) && defined(TCPV40HDRS)
static int os2_socket_pair (int p[2])
{
	int x = -1, y = -1, z;
	struct sockaddr_un sa;
	PTIB tib;
	PPIB pib;
	ULONG msec, idx;

	DosGetInfoBlocks(&tib, &pib);
	DosQuerySysInfo (QSV_MS_COUNT, QSV_MS_COUNT, &msec, HCL_SIZEOF(msec));

	x = socket(PF_OS2, SOCK_STREAM, 0);
	if (x <= -1) goto oops;

	idx = msec;

attempt_to_bind:
	HCL_MEMSET (&sa, 0, HCL_SIZEOF(sa));
	sa.sun_family = AF_OS2;

	/* OS/2 mandates the socket name should begin with \socket\ */
	sprintf (sa.sun_path, "\\socket\\hcl-%08lx-%08lx-%08lx", (unsigned long int)pib->pib_ulpid, (unsigned long int)tib->tib_ptib2->tib2_ultid, (unsigned long int)idx);

	if (bind(x, (struct sockaddr*)&sa, HCL_SIZEOF(sa)) <= -1)
	{
		if (sock_errno() != SOCEADDRINUSE) goto oops;
		if (idx - msec > 9999) goto oops; /* failure after many attempts */

		idx++;
		goto attempt_to_bind;
	}
	if (listen(x, 1) <= -1) goto oops;

	y = socket(PF_OS2, SOCK_STREAM, 0);
	if (y <= -1) goto oops;
	if (connect(y, (struct sockaddr*)&sa, HCL_SIZEOF(sa)) <= -1) goto oops;
	z = accept(x, HCL_NULL, HCL_NULL);
	if (z <= -1) goto oops;

	soclose (x);
	p[0] = z;
	p[1] = y;
	return 0;

oops:
	if (y >= 0) soclose (y);
	if (x >= 0) soclose (x);
	return -1;
}
#endif

static int open_pipes (hcl_t* hcl, int p[2])
{
#if defined(_WIN32)
	if (_pipe(p, 256, _O_BINARY | _O_NOINHERIT) == -1)
	{
		hcl_seterrbfmtwithsyserr (hcl, 0, errno, "unable to create pipes");
		return -1;
	}

#elif defined(__OS2__)
	#if defined(TCPV40HDRS)
	/* neither pipe nor socketpair available */
	if (os2_socket_pair(p) == -1)
	#else
	if (socketpair(AF_LOCAL, SOCK_STREAM, 0, p) == -1)
	#endif
	{
		hcl_seterrbfmtwithsyserr (hcl, 2, sock_errno(), "unable to create pipes");
		return -1;
	}
#elif defined(__DOS__)
	hcl_seterrbfmt (hcl, HCL_ENOIMPL, "unable to create pipes - not supported");
	return -1;
#elif defined(HAVE_PIPE2) && defined(O_CLOEXEC) && defined(O_NONBLOCK)
	if (pipe2(p, O_CLOEXEC | O_NONBLOCK) == -1)
	{
		hcl_seterrbfmtwithsyserr (hcl, 0, errno, "unable to create pipes");
		return -1;
	}

#else
	if (pipe(p) == -1)
	{
		hcl_seterrbfmtwithsyserr (hcl, 0, errno, "unable to create pipes");
		return -1;
	}
#endif

#if defined(_WIN32)
	/* Any equivalent in _WIN32?
	{
		u_long flags;
		flags = 1;
		ioctl (p[0], FIONBIO, &flags);
		ioctl (p[1], FIONBIO, &flags);
	}
	*/
#elif defined(__OS2__)
	{
		int flags = 1; /* don't block */
		ioctl (p[0], FIONBIO, (char*)&flags, HCL_SIZEOF(flags));
		ioctl (p[1], FIONBIO, (char*)&flags, HCL_SIZEOF(flags));
	}
#elif defined(HAVE_PIPE2) && defined(O_CLOEXEC) && defined(O_NONBLOCK)
	/* do nothing */
#else
	{
		int flags;
	#if defined(FD_CLOEXEC) && defined(F_GETFD) && defined(F_SETFD)
		flags = fcntl(p[0], F_GETFD);
		if (flags >= 0) fcntl (p[0], F_SETFD, flags | FD_CLOEXEC);
		flags = fcntl(p[1], F_GETFD);
		if (flags >= 0) fcntl (p[1], F_SETFD, flags | FD_CLOEXEC);
	#endif
	#if defined(O_NONBLOCK) && defined(F_GETFL) && defined(F_SETFL)
		flags = fcntl(p[0], F_GETFL);
		if (flags >= 0) fcntl (p[0], F_SETFL, flags | O_NONBLOCK);
		flags = fcntl(p[1], F_GETFL);
		if (flags >= 0) fcntl (p[1], F_SETFL, flags | O_NONBLOCK);
	#endif
	}
#endif

	return 0;
}

static void close_pipes (hcl_t* hcl, int p[2])
{
#if defined(_WIN32)
	_close (p[0]);
	_close (p[1]);
#elif defined(__OS2__)
	soclose (p[0]);
	soclose (p[1]);
#else
	close (p[0]);
	close (p[1]);
#endif
	p[0] = -1;
	p[1] = -1;
}

static int cb_vm_startup (hcl_t* hcl)
{
	xtn_t* xtn = GET_XTN(hcl);
	int sigfd_pcount = 0;
	int iothr_pcount = 0, flags;

#if defined(_WIN32)
	xtn->waitable_timer = CreateWaitableTimer(HCL_NULL, TRUE, HCL_NULL);
#endif

#if defined(USE_DEVPOLL)
	xtn->ep = open("/dev/poll", O_RDWR);
	if (xtn->ep == -1)
	{
		hcl_seterrwithsyserr (hcl, 0, errno);
		HCL_DEBUG1 (hcl, "Cannot create devpoll - %hs\n", strerror(errno));
		goto oops;
	}

	#if defined(FD_CLOEXEC)
	flags = fcntl(xtn->ep, F_GETFD);
	if (flags >= 0) fcntl (xtn->ep, F_SETFD, flags | FD_CLOEXEC);
	#endif

#elif defined(USE_KQUEUE)
	#if defined(HAVE_KQUEUE1) && defined(O_CLOEXEC)
	xtn->ep = kqueue1(O_CLOEXEC);
	if (xtn->ep == -1) xtn->ep = kqueue();
	#else
	xtn->ep = kqueue();
	#endif
	if (xtn->ep == -1)
	{
		hcl_seterrwithsyserr (hcl, 0, errno);
		HCL_DEBUG1 (hcl, "Cannot create kqueue - %hs\n", strerror(errno));
		goto oops;
	}

	#if defined(FD_CLOEXEC)
	flags = fcntl(xtn->ep, F_GETFD);
	if (flags >= 0 && !(flags & FD_CLOEXEC)) fcntl (xtn->ep, F_SETFD, flags | FD_CLOEXEC);
	#endif

#elif defined(USE_EPOLL)
	#if defined(HAVE_EPOLL_CREATE1) && defined(EPOLL_CLOEXEC)
	xtn->ep = epoll_create1(EPOLL_CLOEXEC);
	if (xtn->ep == -1) xtn->ep = epoll_create(1024);
	#else
	xtn->ep = epoll_create(1024);
	#endif
	if (xtn->ep == -1)
	{
		hcl_seterrwithsyserr (hcl, 0, errno);
		HCL_DEBUG1 (hcl, "Cannot create epoll - %hs\n", strerror(errno));
		goto oops;
	}

	#if defined(FD_CLOEXEC)
	flags = fcntl(xtn->ep, F_GETFD);
	if (flags >= 0 && !(flags & FD_CLOEXEC)) fcntl (xtn->ep, F_SETFD, flags | FD_CLOEXEC);
	#endif

#elif defined(USE_POLL)

	MUTEX_INIT (&xtn->ev.reg.pmtx);

#elif defined(USE_SELECT)
	FD_ZERO (&xtn->ev.reg.rfds);
	FD_ZERO (&xtn->ev.reg.wfds);
	xtn->ev.reg.maxfd = -1;
	MUTEX_INIT (&xtn->ev.reg.smtx);
#endif /* USE_DEVPOLL */

	if (open_pipes(hcl, xtn->sigfd.p) <= -1) goto oops;
	sigfd_pcount = 2;

#if defined(USE_THREAD)
	if (open_pipes(hcl, xtn->iothr.p) <= -1) goto oops;
	iothr_pcount = 2;

	if (_add_poll_fd(hcl, xtn->iothr.p[0], XPOLLIN) <= -1) goto oops;

	pthread_mutex_init (&xtn->ev.mtx, HCL_NULL);
	pthread_cond_init (&xtn->ev.cnd, HCL_NULL);
	pthread_cond_init (&xtn->ev.cnd2, HCL_NULL);
	xtn->ev.halting = 0;

	xtn->iothr.abort = 0;
	xtn->iothr.up = 0;
	/*pthread_create (&xtn->iothr, HCL_NULL, iothr_main, hcl);*/

#endif /* USE_THREAD */

	xtn->vm_running = 1;
	return 0;

oops:
#if defined(USE_THREAD)
	if (iothr_pcount > 0)
	{
		close (xtn->iothr.p[0]);
		close (xtn->iothr.p[1]);
	}
#endif

	if (sigfd_pcount > 0)
	{
		close (xtn->sigfd.p[0]);
		close (xtn->sigfd.p[1]);
	}

#if defined(USE_DEVPOLL) || defined(USE_EPOLL)
	if (xtn->ep >= 0)
	{
		close (xtn->ep);
		xtn->ep = -1;
	}
#endif

	return -1;
}

static void cb_vm_cleanup (hcl_t* hcl)
{
	xtn_t* xtn = GET_XTN(hcl);

	xtn->vm_running = 0;

#if defined(_WIN32)
	if (xtn->waitable_timer)
	{
		CloseHandle (xtn->waitable_timer);
		xtn->waitable_timer = HCL_NULL;
	}
#endif

#if defined(USE_THREAD)
	if (xtn->iothr.up)
	{
		xtn->iothr.abort = 1;
		write (xtn->iothr.p[1], "Q", 1);
		pthread_cond_signal (&xtn->ev.cnd);
		pthread_join (xtn->iothr.thr, HCL_NULL);
		xtn->iothr.up = 0;
	}
	pthread_cond_destroy (&xtn->ev.cnd);
	pthread_cond_destroy (&xtn->ev.cnd2);
	pthread_mutex_destroy (&xtn->ev.mtx);

	_del_poll_fd (hcl, xtn->iothr.p[0]);
	close_pipes (hcl, xtn->iothr.p);
#endif /* USE_THREAD */

	close_pipes (hcl, xtn->sigfd.p);

#if defined(USE_DEVPOLL)
	if (xtn->ep >= 0)
	{
		close (xtn->ep);
		xtn->ep = -1;
	}
	/*destroy_poll_data_space (hcl);*/
#elif defined(USE_KQUEUE)
	if (xtn->ep >= 0)
	{
		close (xtn->ep);
		xtn->ep = -1;
	}
#elif defined(USE_EPOLL)
	if (xtn->ep >= 0)
	{
		close (xtn->ep);
		xtn->ep = -1;
	}
#elif defined(USE_POLL)
	if (xtn->ev.reg.ptr)
	{
		hcl_freemem (hcl, xtn->ev.reg.ptr);
		xtn->ev.reg.ptr = HCL_NULL;
		xtn->ev.reg.len = 0;
		xtn->ev.reg.capa = 0;
	}
	if (xtn->ev.buf)
	{
		hcl_freemem (hcl, xtn->ev.buf);
		xtn->ev.buf = HCL_NULL;
	}
	/*destroy_poll_data_space (hcl);*/
	MUTEX_DESTROY (&xtn->ev.reg.pmtx);
#elif defined(USE_SELECT)
	FD_ZERO (&xtn->ev.reg.rfds);
	FD_ZERO (&xtn->ev.reg.wfds);
	xtn->ev.reg.maxfd = -1;
	MUTEX_DESTROY (&xtn->ev.reg.smtx);
#endif
}

/* -----------------------------------------------------------------
 * STANDARD HCL
 * ----------------------------------------------------------------- */
#if defined(_WIN32)
static const wchar_t* msw_exception_name (DWORD excode)
{
	switch (excode)
	{
		case EXCEPTION_ACCESS_VIOLATION:          return L"Access violation exception";
		case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:     return L"Array bounds exceeded";
		case EXCEPTION_BREAKPOINT:                return L"Breakpoint";
		case EXCEPTION_DATATYPE_MISALIGNMENT:     return L"Float datatype misalignment";
		case EXCEPTION_FLT_DENORMAL_OPERAND:      return L"Float denormal operand";
		case EXCEPTION_FLT_DIVIDE_BY_ZERO:        return L"Float divide by zero";
		case EXCEPTION_FLT_INEXACT_RESULT:        return L"Float inexact result";
		case EXCEPTION_FLT_OVERFLOW:              return L"Float overflow";
		case EXCEPTION_FLT_STACK_CHECK:           return L"Float stack check";
		case EXCEPTION_FLT_UNDERFLOW:             return L"Float underflow";
		case EXCEPTION_ILLEGAL_INSTRUCTION:       return L"Illegal instruction";
		case EXCEPTION_IN_PAGE_ERROR:             return L"In page error";
		case EXCEPTION_INT_DIVIDE_BY_ZERO:        return L"Integer divide by zero";
		case EXCEPTION_INT_OVERFLOW:              return L"Integer overflow";
		case EXCEPTION_INVALID_DISPOSITION:       return L"Invalid disposition";
		case EXCEPTION_NONCONTINUABLE_EXCEPTION:  return L"Noncontinuable exception";
		case EXCEPTION_PRIV_INSTRUCTION:          return L"Priv instruction";
		case EXCEPTION_SINGLE_STEP:               return L"Single step";
		case EXCEPTION_STACK_OVERFLOW:            return L"Stack overflow";
		default:                                  return L"Unknown exception";
	}
}

static const wchar_t* msw_exception_opname (const ULONG opcode)
{
	switch (opcode)
	{
		case 0: return L"Read attempt from inaccessible data";
		case 1: return L"Write attempt to inaccessible data";
		case 8: return L"User-mode data execution prevention violation";
		default: return L"Unknown exception operation";
	}
}

static LONG WINAPI msw_exception_filter (struct _EXCEPTION_POINTERS* exinfo)
{
	HMODULE mod;
#if defined(_WIN32_WINNT) && (_WIN32_WINNT >= 0x0501)
	MODULEINFO modinfo;
#endif
	DWORD excode;
	static wchar_t exmsg[256];
	static wchar_t expath[128];

#if defined(_WIN32_WINNT) && (_WIN32_WINNT >= 0x0501)
	GetModuleHandleExW (GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS, exinfo->ExceptionRecord->ExceptionAddress, &mod);
	/*GetModuleInformation (GetCurrentProcess(), mod, &modinfo, HCL_SIZEOF(modinfo));*/
	GetModuleFileNameExW (GetCurrentProcess(), mod, expath, HCL_SIZEOF(expath));
#else
	GetModuleFileNameW (HCL_NULL, expath, HCL_SIZEOF(expath));
#endif

	excode = exinfo->ExceptionRecord->ExceptionCode;
	if (excode == EXCEPTION_BREAKPOINT) return EXCEPTION_CONTINUE_SEARCH;

	if (excode == EXCEPTION_ACCESS_VIOLATION || excode == EXCEPTION_IN_PAGE_ERROR)
	{
		_snwprintf (exmsg, HCL_COUNTOF(exmsg), L"Exception %s(%u) at 0x%p - Invalid operation at 0x%p - %s",
			msw_exception_name(excode), (unsigned int)excode,
			exinfo->ExceptionRecord->ExceptionAddress,
			(PVOID)exinfo->ExceptionRecord->ExceptionInformation[1],
			msw_exception_opname(exinfo->ExceptionRecord->ExceptionInformation[0])
		);
	}
	else
	{
		_snwprintf (exmsg, HCL_COUNTOF(exmsg), L"Exception %s(%u) at 0x%p",
			msw_exception_name(excode), (unsigned int)excode,
			exinfo->ExceptionRecord->ExceptionAddress
		);
	}

	/* TODO: use a global output callback like vmprim.assertfail().
	 *       vmprim.assertfail() requires 'hcl'. so i need another global level callback for this */
	MessageBoxW (NULL, exmsg, expath, MB_OK | MB_ICONERROR);

	/*return EXCEPTION_CONTINUE_SEARCH;*/
	/*return EXCEPTION_CONTINUE_EXECUTION;*/
	return EXCEPTION_EXECUTE_HANDLER;
}
#endif

hcl_t* hcl_openstdwithmmgr (hcl_mmgr_t* mmgr, hcl_oow_t xtnsize, hcl_errnum_t* errnum)
{
	hcl_t* hcl;
	hcl_vmprim_t vmprim;
	hcl_cb_t cb;

	HCL_MEMSET (&vmprim, 0, HCL_SIZEOF(vmprim));
	vmprim.alloc_heap = alloc_heap;
	vmprim.free_heap = free_heap;
	vmprim.log_write = log_write;
	vmprim.syserrstrb = hcl_syserrstrb;
	vmprim.assertfail = _assertfail;
	vmprim.dl_startup = dl_startup;
	vmprim.dl_cleanup = dl_cleanup;
	vmprim.dl_open = dl_open;
	vmprim.dl_close = dl_close;
	vmprim.dl_getsym = dl_getsym;
	vmprim.vm_gettime = vm_gettime;
	vmprim.vm_muxadd = vm_muxadd;
	vmprim.vm_muxdel = vm_muxdel;
	vmprim.vm_muxmod = vm_muxmod;
	vmprim.vm_muxwait = vm_muxwait;
	vmprim.vm_sleep = vm_sleep;
	vmprim.vm_getsigfd = vm_getsigfd;
	vmprim.vm_getsig = vm_getsig;
	vmprim.vm_setsig = vm_setsig;

	hcl = hcl_open(mmgr, HCL_SIZEOF(xtn_t) + xtnsize, &vmprim, errnum);
	if (HCL_UNLIKELY(!hcl)) return HCL_NULL;

	/* adjust the object size by the sizeof xtn_t so that hcl_getxtn() returns the right pointer. */
	hcl->_instsize += HCL_SIZEOF(xtn_t);

	chain (hcl); /* call chian() before hcl_regcb() as fini_hcl() calls unchain() */
	reset_log_to_default (GET_XTN(hcl));

	HCL_MEMSET (&cb, 0, HCL_SIZEOF(cb));
	cb.on_fini   = cb_on_fini;
	cb.halting   = cb_halting;
	cb.on_option = cb_on_option;
	cb.vm_startup = cb_vm_startup;
	cb.vm_cleanup = cb_vm_cleanup;
	if (hcl_regcb(hcl, &cb) == HCL_NULL)
	{
		if (errnum) *errnum = HCL_ERRNUM(hcl);
		hcl_close (hcl);
		return HCL_NULL;
	}

#if defined(_WIN32)
	SetUnhandledExceptionFilter (msw_exception_filter);
#endif

	return hcl;
}

hcl_t* hcl_openstd (hcl_oow_t xtnsize, hcl_errnum_t* errnum)
{
	return hcl_openstdwithmmgr(&sys_mmgr, xtnsize, errnum);
}


/* --------------------------------------------------------------------- */


typedef struct bb_t bb_t;
struct bb_t
{
	char buf[4096];
	hcl_oow_t pos;
	hcl_oow_t len;

	FILE* fp;
	hcl_bch_t* fn;
};

#if defined(__DOS__) || defined(_WIN32) || defined(__OS2__)
#define FOPEN_R_FLAGS "rb"
#else
#define FOPEN_R_FLAGS "r"
#endif

static HCL_INLINE int open_cci_stream (hcl_t* hcl, hcl_io_cciarg_t* arg)
{
	xtn_t* xtn = GET_XTN(hcl);
	bb_t* bb = HCL_NULL;

/* TOOD: support predefined include directory as well */
	if (arg->includer)
	{
		/* includee */
		hcl_oow_t ucslen, bcslen, parlen;
		const hcl_bch_t* fn, * fb;

	#if defined(HCL_OOCH_IS_UCH)
		if (hcl_convootobcstr(hcl, arg->name, &ucslen, HCL_NULL, &bcslen) <= -1) goto oops;
	#else
		bcslen = hcl_count_bcstr(arg->name);
	#endif

		fn = ((bb_t*)arg->includer->handle)->fn;

		if (arg->name[0] == '/')  /* TODO: change the code to check if it's an absolute path */
		{
			fb = "";
			parlen = 0;
		}
		else
		{
			fb = hcl_get_base_name_from_bcstr_path(fn);
			parlen = fb - fn;
		}

		bb = (bb_t*)hcl_callocmem(hcl, HCL_SIZEOF(*bb) + (HCL_SIZEOF(hcl_bch_t) * (parlen + bcslen + 1)));
		if (!bb) goto oops;

		bb->fn = (hcl_bch_t*)(bb + 1);
		hcl_copy_bchars (bb->fn, fn, parlen);
	#if defined(HCL_OOCH_IS_UCH)
		hcl_convootobcstr (hcl, arg->name, &ucslen, &bb->fn[parlen], &bcslen);
	#else
		hcl_copy_bcstr (&bb->fn[parlen], bcslen + 1, arg->name);
	#endif

		bb->fp = fopen(bb->fn, FOPEN_R_FLAGS);
		if (!bb->fp)
		{
			hcl_seterrbfmt (hcl, HCL_EIOERR, "unable to open %hs", bb->fn);
			goto oops;
		}
	}
	else
	{
		/* main stream  */
		hcl_oow_t pathlen;

		pathlen = xtn->cci_path? hcl_count_bcstr(xtn->cci_path): 0;

		bb = (bb_t*)hcl_callocmem(hcl, HCL_SIZEOF(*bb) + (HCL_SIZEOF(hcl_bch_t) * (pathlen + 1)));
		if (!bb) goto oops;

		bb->fn = (hcl_bch_t*)(bb + 1);
		if (pathlen > 0 && xtn->cci_path)
		{
			hcl_copy_bcstr (bb->fn, pathlen + 1, xtn->cci_path);
			/*bb->fp = fopen(bb->fn, FOPEN_R_FLAGS);*/
		}
		else
		{
			bb->fn[0] = '\0';
			/*bb->fp = stdin;*/
		}
	}


	if (!arg->includer) /* if main stream */
	{
	/* HACK */
		HCL_ASSERT (hcl, arg->name == HCL_NULL);
		arg->name = hcl_dupbtooocstr(hcl, bb->fn, HCL_NULL);
		/* ignore duplication failure */
/* TODO: change the type of arg->name from const hcl_ooch_t* to hcl_ooch_t*.
 *       change its specification from [IN] only to [INOUT] in hcl_io_cciarg_t. */
	/* END HACK */
	}

	arg->handle = bb;
	return 0;

oops:
	if (bb)
	{
		if (bb->fp && bb->fp != stdin) fclose (bb->fp);
		hcl_freemem (hcl, bb);
	}
	return -1;
}

static HCL_INLINE int close_cci_stream (hcl_t* hcl, hcl_io_cciarg_t* arg)
{
	/*xtn_t* xtn = GET_XTN(hcl);*/
	bb_t* bb;

	bb = (bb_t*)arg->handle;
	HCL_ASSERT (hcl, bb != HCL_NULL /*&& bb->fp != HCL_NULL*/);

/* HACK */
	if (!arg->includer && arg->name)
	{
		/* main stream closing */
		hcl_freemem (hcl, (hcl_ooch_t*)arg->name);
		arg->name = HCL_NULL;
	}
/* END HACK */

	if (bb->fp /*&& bb->fp != stdin*/) fclose (bb->fp);
	hcl_freemem (hcl, bb);

	arg->handle = HCL_NULL;
	return 0;
}

static HCL_INLINE int read_cci_stream (hcl_t* hcl, hcl_io_cciarg_t* arg)
{
	/*xtn_t* xtn = GET_XTN(hcl);*/
	bb_t* bb;
	hcl_oow_t bcslen, ucslen, remlen;
	int x;

	bb = (bb_t*)arg->handle;
	HCL_ASSERT (hcl, bb != HCL_NULL);

	if (!bb->fp)
	{
		HCL_ASSERT (hcl, arg->includer == HCL_NULL);
		/* the main stream is opened with no associated file in open_cci_stream(). return no data */
		arg->xlen = 0;
		return 0;
	}

	do
	{
		x = fgetc(bb->fp);
		if (x == EOF)
		{
			if (ferror((FILE*)bb->fp))
			{
				hcl_seterrbfmt (hcl, HCL_EIOERR, "I/O error - %hs", strerror(errno));
				return -1;
			}
			break;
		}

		bb->buf[bb->len++] = x;
	}
	while (bb->len < HCL_COUNTOF(bb->buf) && x != '\r' && x != '\n');

#if defined(HCL_OOCH_IS_UCH)
	bcslen = bb->len;
	ucslen = HCL_COUNTOF(arg->buf.c);
	x = hcl_convbtooochars(hcl, bb->buf, &bcslen, arg->buf.c, &ucslen);
	if (x <= -1 && ucslen <= 0) return -1;
	/* if ucslen is greater than 0, i assume that some characters have been
	 * converted properly. as the loop above reads an entire line if not too
	 * large, the incomplete sequence error (x == -3) must happen after
	 * successful conversion of at least 1 ooch character. so no explicit
	 * check for the incomplete sequence error is required */
#else
	bcslen = (bb->len < HCL_COUNTOF(arg->buf.c))? bb->len: HCL_COUNTOF(arg->buf.c);
	ucslen = bcslen;
	hcl_copy_bchars (arg->buf.c, bb->buf, bcslen);
#endif

	remlen = bb->len - bcslen;
	if (remlen > 0) HCL_MEMMOVE (bb->buf, &bb->buf[bcslen], remlen);
	bb->len = remlen;

	arg->xlen = ucslen;
	return 0;
}

/* source code input handler */
static int cci_handler (hcl_t* hcl, hcl_io_cmd_t cmd, void* arg)
{
	switch (cmd)
	{
		case HCL_IO_OPEN:
			return open_cci_stream(hcl, (hcl_io_cciarg_t*)arg);

		case HCL_IO_CLOSE:
			return close_cci_stream(hcl, (hcl_io_cciarg_t*)arg);

		case HCL_IO_READ:
			return read_cci_stream(hcl, (hcl_io_cciarg_t*)arg);

		case HCL_IO_FLUSH:
			/* no effect on an input stream */
			return 0;

		case HCL_IO_READ_BYTES: /* byte input prohibited */
		case HCL_IO_WRITE: /*  character output prohibited */
		case HCL_IO_WRITE_BYTES: /* byte output prohibited */
		default:
			hcl_seterrnum (hcl, HCL_EINTERN);
			return -1;
	}
}

/* --------------------------------------------------------------------- */
static HCL_INLINE int open_udi_stream (hcl_t* hcl, hcl_io_udiarg_t* arg)
{
	xtn_t* xtn = GET_XTN(hcl);
	bb_t* bb = HCL_NULL;

	hcl_oow_t pathlen;

	pathlen = xtn->udi_path? hcl_count_bcstr(xtn->udi_path): 0;

	bb = (bb_t*)hcl_callocmem(hcl, HCL_SIZEOF(*bb) + (HCL_SIZEOF(hcl_bch_t) * (pathlen + 1)));
	if (!bb) goto oops;

	bb->fn = (hcl_bch_t*)(bb + 1);
	if (pathlen > 0 && xtn->udi_path)
	{
		hcl_copy_bcstr (bb->fn, pathlen + 1, xtn->udi_path);
		bb->fp = fopen(bb->fn, FOPEN_R_FLAGS);
	}
	else
	{
		bb->fn[0] = '\0';
		bb->fp = stdin;
	}

	if (!bb->fp)
	{
		hcl_seterrbfmtwithsyserr (hcl, 0, errno, "unable to open udi stream '%hs'", xtn->udi_path);
		goto oops;
	}

	arg->byte_oriented = 1;
	arg->handle = bb;
	return 0;

oops:
	if (bb)
	{
		if (bb->fp && bb->fp != stdin) fclose (bb->fp);
		hcl_freemem (hcl, bb);
	}
	return -1;
}

static HCL_INLINE int close_udi_stream (hcl_t* hcl, hcl_io_udiarg_t* arg)
{
	/*xtn_t* xtn = GET_XTN(hcl);*/
	bb_t* bb;

	bb = (bb_t*)arg->handle;
	HCL_ASSERT (hcl, bb != HCL_NULL && bb->fp != HCL_NULL);

	if (bb->fp != stdin) fclose (bb->fp);
	hcl_freemem (hcl, bb);

	arg->handle = HCL_NULL;
	return 0;
}

static HCL_INLINE int read_udi_stream (hcl_t* hcl, hcl_io_udiarg_t* arg)
{
	/*xtn_t* xtn = GET_XTN(hcl);*/
	bb_t* bb;
	hcl_oow_t bcslen, ucslen, remlen;
	int x;
#if defined(HCL_OOCH_IS_UCH)
	int fetched = 0;
#endif

	bb = (bb_t*)arg->handle;
	HCL_ASSERT (hcl, bb != HCL_NULL && bb->fp != HCL_NULL);

	if (bb->len > 0)
	{
#if defined(HCL_OOCH_IS_UCH)
	real_fetch:
		fetched = 1;
#endif
		do
		{
			x = fgetc(bb->fp);
			if (x == EOF)
			{
				if (ferror((FILE*)bb->fp))
				{
					hcl_seterrbfmtwithsyserr (hcl, 0, errno, "unable to read udi stream");
					return -1;
				}
				break;
			}

			bb->buf[bb->len++] = x;
		}
		while (bb->len < HCL_COUNTOF(bb->buf) && x != '\r' && x != '\n');
	}

#if defined(HCL_OOCH_IS_UCH)
	bcslen = bb->len;
	ucslen = HCL_COUNTOF(arg->buf.c);
	x = hcl_convbtooochars(hcl, bb->buf, &bcslen, arg->buf.c, &ucslen);
	if (x <= -1 && ucslen <= 0)
	{
		if (x == -3 && !fetched) goto real_fetch;
		return -1;
	}
	/* if ucslen is greater than 0, i assume that some characters have been
	 * converted properly. as the loop above reads an entire line if not too
	 * large, the incomplete sequence error (x == -3) must happen after
	 * successful conversion of at least 1 ooch character. so no explicit
	 * check for the incomplete sequence error is required */
#else
	bcslen = (bb->len < HCL_COUNTOF(arg->buf.c))? bb->len: HCL_COUNTOF(arg->buf.c);
	ucslen = bcslen;
	hcl_copy_bchars (arg->buf.c, bb->buf, bcslen);
#endif

	remlen = bb->len - bcslen;
	if (remlen > 0) HCL_MEMMOVE (bb->buf, &bb->buf[bcslen], remlen);
	bb->len = remlen;

	arg->xlen = ucslen;
	return 0;
}

static HCL_INLINE int read_udi_stream_bytes (hcl_t* hcl, hcl_io_udiarg_t* arg)
{
	/*xtn_t* xtn = GET_XTN(hcl);*/
	bb_t* bb;
	hcl_oow_t bcslen, ucslen, remlen;
	int x;

	bb = (bb_t*)arg->handle;
	HCL_ASSERT (hcl, bb != HCL_NULL && bb->fp != HCL_NULL);

	if (bb->len <= 0)
	{
		do
		{
			x = fgetc(bb->fp);
			if (x == EOF)
			{
				if (ferror((FILE*)bb->fp))
					{
					hcl_seterrbfmtwithsyserr (hcl, 0, errno, "unable to read udi stream");
					return -1;
				}
				break;
			}

			bb->buf[bb->len++] = x;
		}
		while (bb->len < HCL_COUNTOF(bb->buf) && x != '\r' && x != '\n');
	}

	bcslen = (bb->len < HCL_COUNTOF(arg->buf.b))? bb->len: HCL_COUNTOF(arg->buf.b);
	ucslen = bcslen;
	hcl_copy_bchars ((hcl_bch_t*)arg->buf.b, bb->buf, bcslen);

	remlen = bb->len - bcslen;
	if (remlen > 0) HCL_MEMMOVE (bb->buf, &bb->buf[bcslen], remlen);
	bb->len = remlen;

	arg->xlen = ucslen;
	return 0;
}

static int udi_handler (hcl_t* hcl, hcl_io_cmd_t cmd, void* arg)
{
	switch (cmd)
	{
		case HCL_IO_OPEN:
			return open_udi_stream(hcl, (hcl_io_udiarg_t*)arg);

		case HCL_IO_CLOSE:
			return close_udi_stream(hcl, (hcl_io_udiarg_t*)arg);

		case HCL_IO_READ:
			return read_udi_stream(hcl, (hcl_io_udiarg_t*)arg);

		case HCL_IO_READ_BYTES:
			return read_udi_stream_bytes(hcl, (hcl_io_udiarg_t*)arg);

		case HCL_IO_FLUSH:
			/* no effect on an input stream */
			return 0;

		default:
			hcl_seterrnum (hcl, HCL_EINTERN);
			return -1;
	}
}

/* --------------------------------------------------------------------- */

static HCL_INLINE int open_udo_stream (hcl_t* hcl, hcl_io_udoarg_t* arg)
{
	xtn_t* xtn = GET_XTN(hcl);
	FILE* fp;
#if defined(__DOS__) || defined(_WIN32) || defined(__OS2__)
#define FOPEN_W_FLAGS "wb"
#else
#define FOPEN_W_FLAGS "w"
#endif

	fp = (xtn->udo_path && xtn->udo_path[0] != '\0'? fopen(xtn->udo_path, FOPEN_W_FLAGS): stdout);
	if (!fp)
	{
		if (xtn->udo_path)
			hcl_seterrbfmtwithsyserr (hcl, 0, errno, "unable to open udo stream '%hs'", xtn->udo_path);
		else
			hcl_seterrbfmtwithsyserr (hcl, 0, errno, "unable to open udo stream");
		return -1;
	}

	arg->handle = fp;
	return 0;
}

static HCL_INLINE int close_udo_stream (hcl_t* hcl, hcl_io_udoarg_t* arg)
{
	/*xtn_t* xtn = GET_XTN(hcl);*/
	FILE* fp;

	fp = (FILE*)arg->handle;
	HCL_ASSERT (hcl, fp != HCL_NULL);
	if (fp != stdout) fclose (fp);
	arg->handle = HCL_NULL;
	return 0;
}

static HCL_INLINE int write_udo_stream (hcl_t* hcl, hcl_io_udoarg_t* arg)
{
	/*xtn_t* xtn = GET_XTN(hcl);*/
	const hcl_ooch_t* ptr;
	hcl_bch_t bcsbuf[1024];
	hcl_oow_t bcslen, ucslen, donelen;
	int x;

	ptr = (const hcl_ooch_t*)arg->ptr;
	donelen = 0;

	do
	{
	#if defined(HCL_OOCH_IS_UCH)
		bcslen = HCL_COUNTOF(bcsbuf);
		ucslen = arg->len - donelen;
		x = hcl_convootobchars(hcl, &ptr[donelen], &ucslen, bcsbuf, &bcslen);
		if (x <= -1 && ucslen <= 0) return -1;
	#else
		bcslen = HCL_COUNTOF(bcsbuf);
		ucslen = arg->len - donelen;
		if (ucslen > bcslen) ucslen = bcslen;
		else if (ucslen < bcslen) bcslen = ucslen;
		hcl_copy_bchars (bcsbuf, &ptr[donelen], bcslen);
	#endif

		if (fwrite(bcsbuf, HCL_SIZEOF(bcsbuf[0]), bcslen, (FILE*)arg->handle) < bcslen)
		{
			hcl_seterrbfmtwithsyserr (hcl, 0, errno, "unable to write udo stream");
			return -1;
		}

		donelen += ucslen;
	}
	while (donelen < arg->len);

	arg->xlen = arg->len;
	return 0;
}

static HCL_INLINE int write_udo_stream_bytes (hcl_t* hcl, hcl_io_udoarg_t* arg)
{
	/*xtn_t* xtn = GET_XTN(hcl);*/
	const hcl_uint8_t* ptr;

	ptr = (const hcl_uint8_t*)arg->ptr; /* take the buffer as a byte series */

	if (fwrite(ptr, HCL_SIZEOF(*ptr), arg->len, (FILE*)arg->handle) < arg->len)
	{
		hcl_seterrbfmtwithsyserr (hcl, 0, errno, "unable to write udo stream");
		return -1;
	}

	arg->xlen = arg->len;
	return 0;
}

static HCL_INLINE int flush_udo_stream (hcl_t* hcl, hcl_io_udoarg_t* arg)
{
	FILE* fp;

	fp = (FILE*)arg->handle;
	HCL_ASSERT (hcl, fp != HCL_NULL);

	fflush (fp);
	return 0;
}

static int udo_handler (hcl_t* hcl, hcl_io_cmd_t cmd, void* arg)
{
	switch (cmd)
	{
		case HCL_IO_OPEN:
			return open_udo_stream(hcl, (hcl_io_udoarg_t*)arg);

		case HCL_IO_CLOSE:
			return close_udo_stream(hcl, (hcl_io_udoarg_t*)arg);

		case HCL_IO_WRITE:
			return write_udo_stream(hcl, (hcl_io_udoarg_t*)arg);

		case HCL_IO_WRITE_BYTES:
			return write_udo_stream_bytes(hcl, (hcl_io_udoarg_t*)arg);

		case HCL_IO_FLUSH:
			return flush_udo_stream(hcl, (hcl_io_udoarg_t*)arg);

		default:
			hcl_seterrnum (hcl, HCL_EINTERN);
			return -1;
	}
}

/* --------------------------------------------------------------------- */

int hcl_attachcciostdwithbcstr (hcl_t* hcl, const hcl_bch_t* cci_file)
{
	xtn_t* xtn = GET_XTN(hcl);
	int n;

	HCL_ASSERT (hcl, xtn->cci_path == HCL_NULL);

	xtn->cci_path = cci_file;

	n = hcl_attachccio(hcl, cci_handler);

	xtn->cci_path = HCL_NULL;

	return n;
}

int hcl_attachcciostdwithucstr (hcl_t* hcl, const hcl_uch_t* cci_file)
{
	xtn_t* xtn = GET_XTN(hcl);
	int n;

	HCL_ASSERT (hcl, xtn->cci_path == HCL_NULL);

	xtn->cci_path = hcl_duputobcstr(hcl, cci_file, HCL_NULL);
	if (HCL_UNLIKELY(!xtn->cci_path)) return -1;

	n = hcl_attachccio(hcl, cci_handler);

	hcl_freemem (hcl, (void*)xtn->cci_path);
	xtn->cci_path = HCL_NULL;

	return n;
}

/* --------------------------------------------------------------------- */

int hcl_attachudiostdwithbcstr (hcl_t* hcl, const hcl_bch_t* udi_file, const hcl_bch_t* udo_file)
{
	xtn_t* xtn = GET_XTN(hcl);
	int n;

	HCL_ASSERT (hcl, xtn->udi_path == HCL_NULL);
	HCL_ASSERT (hcl, xtn->udo_path == HCL_NULL);

	xtn->udi_path = udi_file;
	xtn->udo_path = udo_file;

	n = hcl_attachudio(hcl, udi_handler, udo_handler);

	xtn->udi_path = HCL_NULL;
	xtn->udo_path = HCL_NULL;

	return n;
}

int hcl_attachudiostdwithucstr (hcl_t* hcl, const hcl_uch_t* udi_file, const hcl_uch_t* udo_file)
{
	xtn_t* xtn = GET_XTN(hcl);
	int n;

	HCL_ASSERT (hcl, xtn->udi_path == HCL_NULL);
	HCL_ASSERT (hcl, xtn->udo_path == HCL_NULL);

	xtn->udi_path = hcl_duputobcstr(hcl, udi_file, HCL_NULL);
	if (HCL_UNLIKELY(!xtn->udi_path))
	{
		hcl_freemem (hcl, (void*)xtn->cci_path);
		return -1;
	}

	xtn->udo_path = hcl_duputobcstr(hcl, udo_file, HCL_NULL);
	if (HCL_UNLIKELY(!xtn->udo_path))
	{
		hcl_freemem (hcl, (void*)xtn->udi_path);
		xtn->udi_path = HCL_NULL;
		return -1;
	}

	n = hcl_attachudio(hcl, udi_handler, udo_handler);

	hcl_freemem (hcl, (void*)xtn->udi_path);
	hcl_freemem (hcl, (void*)xtn->udo_path);

	xtn->udi_path = HCL_NULL;
	xtn->udo_path = HCL_NULL;

	return n;
}




/* ========================================================================= */

static hcl_uint32_t ticker_started = 0;

void hcl_start_ticker (void)
{
	if (++ticker_started == 1)
	{
		start_ticker ();
	}
}

void hcl_stop_ticker (void)
{
	if (ticker_started > 0 && --ticker_started == 0)
	{
		stop_ticker ();
	}
}


/* ========================================================================== */

#if defined(_WIN32)
static BOOL WINAPI handle_term (DWORD ctrl_type)
{
	if (ctrl_type == CTRL_C_EVENT || ctrl_type == CTRL_CLOSE_EVENT)
	{
		abort_all_hcls (SIGINT);
		return TRUE;
	}

	return FALSE;
}

void hcl_catch_termreq (void)
{
	SetConsoleCtrlHandler (handle_term, TRUE);
}

void hcl_uncatch_termreq (void)
{
	SetConsoleCtrlHandler (handle_term, FALSE);
}

#elif defined(__OS2__)

static EXCEPTIONREGISTRATIONRECORD os2_excrr = { 0 };

static ULONG APIENTRY handle_term (
	PEXCEPTIONREPORTRECORD p1,
	PEXCEPTIONREGISTRATIONRECORD p2,
	PCONTEXTRECORD p3,
	PVOID pv)
{
	if (p1->ExceptionNum == XCPT_SIGNAL)
	{
		if (p1->ExceptionInfo[0] == XCPT_SIGNAL_INTR ||
		    p1->ExceptionInfo[0] == XCPT_SIGNAL_KILLPROC ||
		    p1->ExceptionInfo[0] == XCPT_SIGNAL_BREAK)
		{
			abort_all_hcls (SIGINT);
			return (DosAcknowledgeSignalException(p1->ExceptionInfo[0]) != NO_ERROR)? 1: XCPT_CONTINUE_EXECUTION;
		}
	}

	return XCPT_CONTINUE_SEARCH; /* exception not resolved */
}

void hcl_catch_termreq (void)
{
	os2_excrr.ExceptionHandler = (ERR)handle_term;
	DosSetExceptionHandler (&os2_excrr); /* TODO: check if NO_ERROR is returned */
}

void hcl_uncatch_termreq (void)
{
	DosUnsetExceptionHandler (&os2_excrr);
}

#elif defined(__DOS__)

/*#define IRQ_TERM 0x23*/
/*#define IRQ_TERM 0x1B*/
#define IRQ_TERM 0x9

#if defined(_INTELC32_)
static void (*dos_prev_int23_handler) (void);
#pragma interrupt(dos_int23_handler)
static void dos_int23_handler (void)
#else
static void (__interrupt *dos_prev_int23_handler) (void);
static void __interrupt dos_int23_handler (void)
#endif
{
	/* dos int23 - ctrl-c handler */

#if (IRQ_TERM == 0x23) && defined(_INTELC32_)
	/* note this code for _INTELC32_ doesn't seem to work properly
	 * unless the program is waiting on getch() or something similar */
	/* prevent the DOS interrupt handler from being called */
	_XSTACK* stk = (_XSTACK*)_get_stk_frame();
	stk->opts |= _STK_NOINT;
	abort_all_hcls (SIGINT);
	/* if i call the previous handler, it's likely to kill the application.
	 * so i don't chain-call the previous handler. but another call could
	 * have changed the handler already to something else. then it would be
	 * better to chain-call it. TODO: find a way to chain-call it safely */
	/*_chain_intr (dos_prev_int23_handler);*/
#else


	#if 0
	static int extended = 0;
	static int keyboard[255] = { 0, };
	hcl_uint8_t sc, status;
	/* TODO: determine if the key pressed is ctrl-C or ctrl-break ... */

	sc = inp(0x60);
	/*status = inp(0x61);*/
	if (sc == 0xE0)
	{
		/* extended key prefix */
		extended = 1;
	}
	else if (sc == 0xE1)
	{
		/* pause key */
	}
	else
	{
		if (sc & 0x80)
		{
			/* key release */
			sc = sc & 0x7F;
			keyboard[sc] = 0;
			/*printf ("%key released ... %x\n", sc);*/
		}
		else
		{
			keyboard[sc] = 1;
			/*printf ("%key pressed ... %x %c\n", sc, sc);*/
			abort_all_hcls (SIGINT);
		}

		extended = 0;
	}

	/*_chain_intr (dos_prev_int23_handler);*/
	outp (0x20, 0x20);
	#else
	abort_all_hcls (SIGINT);
	_chain_intr (dos_prev_int23_handler);
	#endif
#endif
}

void hcl_catch_termreq (void)
{
	dos_prev_int23_handler = _dos_getvect(IRQ_TERM);
	_dos_setvect (IRQ_TERM, dos_int23_handler);
}

void hcl_uncatch_termreq (void)
{
	_dos_setvect (IRQ_TERM, dos_prev_int23_handler);
	dos_prev_int23_handler = HCL_NULL;
}

#else

void hcl_catch_termreq (void)
{
	set_signal_handler(SIGTERM, abort_all_hcls, 0);
	set_signal_handler(SIGHUP, abort_all_hcls, 0);
	set_signal_handler(SIGINT, abort_all_hcls, 0);
	set_signal_handler(SIGPIPE, do_nothing, 0);
}

void hcl_uncatch_termreq (void)
{
	unset_signal_handler(SIGTERM);
	unset_signal_handler(SIGHUP);
	unset_signal_handler(SIGINT);
	unset_signal_handler(SIGPIPE);
}

#endif
