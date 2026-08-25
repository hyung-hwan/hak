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


/* _GNU_SOURCE must precede any libc header - pipe2() is behind it */
#if !defined(_WIN32) && !defined(_GNU_SOURCE)
#	define _GNU_SOURCE
#endif

#include "_sys.h"
#include <hak-hnd.h>
#include <hak-str.h>
#include <stdlib.h>

#if !defined(_WIN32)
#	include <sys/types.h>
#	include <sys/stat.h>
#	include <unistd.h>
#	include <fcntl.h>
#	include <errno.h>
#endif

#if defined(HAVE_SYS_TIME_H)
#	include <sys/time.h>
#endif
#if defined(HAVE_TIME_H)
#	include <time.h>
#endif

#if defined(__DOS__)
#	include <dos.h>
#	include <time.h>
#endif

static hak_pfrc_t pf_sys_time (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_ntime_t now;
	hak_oop_t tv;
	hak->vmprim.vm_gettime(hak, &now); /* should I use time() instead? */
	tv = hak_oowtoint(hak, now.sec);
	if (!tv) return HAK_PF_FAILURE;
	HAK_STACK_SETRET(hak, nargs, tv);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_sys_stime (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t t;
	hak_ooi_t ti;

	t = HAK_STACK_GETARG(hak, nargs, 0);
	if (hak_inttoooi(hak, t, &ti) == 0)
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt (hak, HAK_EINVAL, "unacceptiable time value - %O - %js", t, orgmsg);
		return HAK_PF_FAILURE;
	}

	/* ---------------------------------------------------------------- */
#if defined(HAVE_SETTIMEOFDAY)
	{
		struct timeval tv;
		tv.tv_sec = ti;
		tv.tv_usec = 0;
		settimeofday (&tv, HAK_NULL);
	}
#elif defined(__DOS__)
	{
		struct tm* tm;
		time_t t = ti;
		struct dosdate_t dd;
		struct dostime_t dt;

		tm = localtime(&t);

		dd.day = tm->tm_mday;
		dd.month = tm->tm_mon;
		dd.year = tm->tm_year + 1900;
		dd.dayofweek = tm->tm_wday;

		dt.hour = tm->tm_hour;
		dt.minute = tm->tm_min;
		dt.second = tm->tm_sec;
		dt.hsecond = 0;

		_dos_setdate(&dd);
		_dos_settime(&dt);
	}
#else
	{
		time_t tv;
		tv = ti;
		stime (&tv);
	}
#endif
	/* ---------------------------------------------------------------- */

	HAK_STACK_SETRET(hak, nargs, hak->_nil);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_sys_srandom (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t seed;
	hak_oow_t seedw;

	seed = HAK_STACK_GETARG(hak, nargs, 0);
	if (hak_inttooow(hak, seed, &seedw) == 0)
	{
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt (hak, HAK_EINVAL, "unacceptiable seed - %O - %js", seed, orgmsg);
		return HAK_PF_FAILURE;
	}

#if defined(__DOS__)
	srand (seedw);
#else
	srandom (seedw);
#endif

	HAK_STACK_SETRET(hak, nargs, hak->_nil);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_sys_random (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	long int r;
	hak_ooi_t rv;

#if defined(__DOS__)
	r = rand();
#else
	r = random();
#endif
	rv = (hak_ooi_t)(r % HAK_SMOOI_MAX);
	HAK_STACK_SETRET(hak, nargs, HAK_SMOOI_TO_OOP(rv));
	return HAK_PF_SUCCESS;
}


/* ------------------------------------------------------------------------ *
 * SYSTEM HANDLE PRIMITIVES
 *
 * Every one of these takes or returns a handle id - a small integer resolved
 * against the handle table in hak-hnd.h. hak code never sees a descriptor, so
 * it cannot name one it did not open, and hak_closehnd() is guaranteed the
 * chance to unbind a handle from the multiplexer before it disappears.
 *
 * sys.read and sys.write follow the non-blocking contract: they return the
 * byte count, 0 at end of file, or -1 when the handle would have blocked.
 * -1 is an ordinary outcome - the caller is expected to wait on a semaphore
 * bound with sem-signal-on-input/-output and try again. Only a genuine
 * failure raises.
 * ------------------------------------------------------------------------ */

/* pull a null-terminated hak string out of an argument */
static hak_bch_t* dup_path_arg (hak_t* hak, hak_oop_t t)
{
	if (!HAK_OBJ_IS_CHAR_POINTER(t) || HAK_OBJ_GET_SIZE(t) == 0 ||
	    hak_count_oocstr(HAK_OBJ_GET_CHAR_SLOT(t)) != HAK_OBJ_GET_SIZE(t))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "path not a proper string - %O", t);
		return HAK_NULL;
	}
	return hak_dupootobcstr(hak, HAK_OBJ_GET_CHAR_SLOT(t), HAK_NULL);
}

#if !defined(_WIN32)
/* translate a mode string into open() flags. hak has no way to expose
 * O_RDONLY and friends as constants yet - HAK_PFBASE_CONST is unimplemented -
 * so a mode string is what a script can actually write today. */
static int mode_str_to_oflags (hak_t* hak, hak_oop_t t, int* oflags)
{
	const hak_ooch_t* p;
	hak_oow_t len;
	int fl;

	if (!HAK_OBJ_IS_CHAR_POINTER(t))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "mode not a string - %O", t);
		return -1;
	}

	p = HAK_OBJ_GET_CHAR_SLOT(t);
	len = HAK_OBJ_GET_SIZE(t);

	if (len == 1 && p[0] == 'r')                     fl = O_RDONLY;
	else if (len == 1 && p[0] == 'w')                fl = O_WRONLY | O_CREAT | O_TRUNC;
	else if (len == 1 && p[0] == 'a')                fl = O_WRONLY | O_CREAT | O_APPEND;
	else if (len == 2 && p[0] == 'r' && p[1] == '+') fl = O_RDWR;
	else if (len == 2 && p[0] == 'w' && p[1] == '+') fl = O_RDWR | O_CREAT | O_TRUNC;
	else if (len == 2 && p[0] == 'a' && p[1] == '+') fl = O_RDWR | O_CREAT | O_APPEND;
	else
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "unrecognized open mode - %O", t);
		return -1;
	}

#if defined(O_CLOEXEC)
	/* a child process has no business inheriting a handle hak code opened */
	fl |= O_CLOEXEC;
#endif
#if defined(O_NONBLOCK)
	/* required in the open() itself for a fifo, where a blocking open would
	 * wait for a peer. harmless on a regular file. */
	fl |= O_NONBLOCK;
#endif
#if defined(O_LARGEFILE)
	fl |= O_LARGEFILE;
#endif

	*oflags = fl;
	return 0;
}
#endif

static hak_pfrc_t pf_sys_open (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
#if defined(_WIN32)
	hak_seterrnum(hak, HAK_ENOIMPL);
	return HAK_PF_FAILURE;
#else
	hak_bch_t* path;
	hak_hnd_t* hnd;
	int oflags, fd;
	hak_ooi_t mode = 0644;

	path = dup_path_arg(hak, HAK_STACK_GETARG(hak, nargs, 0));
	if (HAK_UNLIKELY(!path)) return HAK_PF_FAILURE;

	if (mode_str_to_oflags(hak, HAK_STACK_GETARG(hak, nargs, 1), &oflags) <= -1)
	{
		hak_freemem(hak, path);
		return HAK_PF_FAILURE;
	}

	if (nargs >= 3 && hak_inttoooi(hak, HAK_STACK_GETARG(hak, nargs, 2), &mode) == 0)
	{
		hak_freemem(hak, path);
		return HAK_PF_FAILURE;
	}

	fd = open(path, oflags, (int)mode);
	if (fd <= -1)
	{
		hak_seterrbfmtwithsyserr(hak, 0, errno, "unable to open %hs", path);
		hak_freemem(hak, path);
		return HAK_PF_FAILURE;
	}
	hak_freemem(hak, path);

	/* the probe inside hak_wrapfd() decides the type and whether the
	 * multiplexer will take it, so a regular file never reaches epoll. */
	hnd = hak_wrapfd(hak, fd, 0, HAK_HND_OPEN_NONBLOCK);
	if (HAK_UNLIKELY(!hnd))
	{
		close(fd);
		return HAK_PF_FAILURE;
	}

	HAK_STACK_SETRET(hak, nargs, HAK_SMOOI_TO_OOP(hnd->id));
	return HAK_PF_SUCCESS;
#endif
}

static hak_pfrc_t pf_sys_close (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_hnd_t* hnd;

	hnd = hak_gethndwithoop(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_HND_TYPE_ALL_FD);
	if (HAK_UNLIKELY(!hnd)) return HAK_PF_FAILURE;

	/* hak_closehnd() unbinds the handle from the multiplexer before the
	 * descriptor goes away and releases the node either way. */
	if (hak_closehnd(hak, hnd) <= -1) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, hak->_nil);
	return HAK_PF_SUCCESS;
}

/* resolve the (buffer, offset, length) triple shared by read and write */
static int get_buf_args (hak_t* hak, hak_ooi_t nargs, hak_oop_t bufoop,
                         hak_oob_t** ptr, hak_oow_t* len)
{
	hak_oow_t offset = 0, length, maxlen;

	if (!HAK_OBJ_IS_BYTE_POINTER(bufoop))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "buffer not a byte array - %O", bufoop);
		return -1;
	}

	maxlen = HAK_OBJ_GET_SIZE(bufoop);
	length = maxlen;

	if (nargs >= 3)
	{
		hak_oop_t t = HAK_STACK_GETARG(hak, nargs, 2);
		if (hak_inttooow(hak, t, &offset) == 0)
		{
			hak_seterrbfmt(hak, HAK_EINVAL, "invalid offset - %O", t);
			return -1;
		}
		if (offset > maxlen)
		{
			hak_seterrbfmt(hak, HAK_ERANGE, "offset %zu past the end of a %zu byte buffer", offset, maxlen);
			return -1;
		}
		length = maxlen - offset;

		if (nargs >= 4)
		{
			t = HAK_STACK_GETARG(hak, nargs, 3);
			if (hak_inttooow(hak, t, &length) == 0)
			{
				hak_seterrbfmt(hak, HAK_EINVAL, "invalid length - %O", t);
				return -1;
			}
			if (length > maxlen - offset) length = maxlen - offset;
		}
	}

	/* no allocation happens between here and the read/write, so a raw slot
	 * pointer cannot be invalidated by a garbage collection */
	*ptr = &HAK_OBJ_GET_BYTE_SLOT(bufoop)[offset];
	*len = length;
	return 0;
}

static hak_pfrc_t pf_sys_read (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_hnd_t* hnd;
	hak_oob_t* ptr;
	hak_oow_t len;
	hak_ooi_t n;

	hnd = hak_gethndwithoop(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_HND_TYPE_ALL_FD);
	if (HAK_UNLIKELY(!hnd)) return HAK_PF_FAILURE;

	if (get_buf_args(hak, nargs, HAK_STACK_GETARG(hak, nargs, 1), &ptr, &len) <= -1) return HAK_PF_FAILURE;

	n = hak_readhnd(hak, hnd, ptr, len);
	if (n == HAK_HND_IO_ERROR) return HAK_PF_FAILURE;

	/* n is >= 0, or -1 meaning the handle would have blocked */
	HAK_STACK_SETRET(hak, nargs, HAK_SMOOI_TO_OOP(n));
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_sys_write (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_hnd_t* hnd;
	hak_oob_t* ptr;
	hak_oow_t len;
	hak_ooi_t n;

	hnd = hak_gethndwithoop(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_HND_TYPE_ALL_FD);
	if (HAK_UNLIKELY(!hnd)) return HAK_PF_FAILURE;

	if (get_buf_args(hak, nargs, HAK_STACK_GETARG(hak, nargs, 1), &ptr, &len) <= -1) return HAK_PF_FAILURE;

	n = hak_writehnd(hak, hnd, ptr, len);
	if (n == HAK_HND_IO_ERROR) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, HAK_SMOOI_TO_OOP(n));
	return HAK_PF_SUCCESS;
}

/* (sys.pipe) -> #[read-handle write-handle]
 *
 * This is how hak code obtains a handle the multiplexer will accept without
 * any raw descriptor ever crossing the boundary. Both ends are non-blocking
 * and close-on-exec. */
static hak_pfrc_t pf_sys_pipe (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
#if defined(_WIN32)
	hak_seterrnum(hak, HAK_ENOIMPL);
	return HAK_PF_FAILURE;
#else
	int p[2];
	hak_hnd_t *r = HAK_NULL, *w = HAK_NULL;
	hak_oop_t arr;

#if defined(HAVE_PIPE2) && defined(O_CLOEXEC) && defined(O_NONBLOCK)
	if (pipe2(p, O_CLOEXEC | O_NONBLOCK) <= -1)
	{
		hak_seterrbfmtwithsyserr(hak, 0, errno, "unable to create a pipe");
		return HAK_PF_FAILURE;
	}
#else
	if (pipe(p) <= -1)
	{
		hak_seterrbfmtwithsyserr(hak, 0, errno, "unable to create a pipe");
		return HAK_PF_FAILURE;
	}
#endif

	r = hak_wrapfd(hak, p[0], 0, HAK_HND_OPEN_NONBLOCK);
	if (HAK_UNLIKELY(!r)) goto oops;
	w = hak_wrapfd(hak, p[1], 0, HAK_HND_OPEN_NONBLOCK);
	if (HAK_UNLIKELY(!w)) goto oops;

	/* the allocation may collect, but the handles live outside the heap */
	arr = hak_makearray(hak, 2);
	if (HAK_UNLIKELY(!arr)) goto oops;

	HAK_OBJ_SET_OOP_VAL(arr, 0, HAK_SMOOI_TO_OOP(r->id));
	HAK_OBJ_SET_OOP_VAL(arr, 1, HAK_SMOOI_TO_OOP(w->id));

	HAK_STACK_SETRET(hak, nargs, arr);
	return HAK_PF_SUCCESS;

oops:
	if (w) hak_closehnd(hak, w); else close(p[1]);
	if (r) hak_closehnd(hak, r); else close(p[0]);
	return HAK_PF_FAILURE;
#endif
}

#include <stdio.h> // TODO: remove this and replace it by own impl
static hak_pfrc_t pf_sys_popen (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t t;
	hak_bch_t* cmd;
	FILE* pp;

	t = HAK_STACK_GETARG(hak, nargs, 0);
// TODO: support byte array?
	/*if (!HAK_IS_STRING(hak, t)) goto oops;*/
	if (!HAK_OBJ_IS_CHAR_POINTER(t) ||
	    HAK_OBJ_GET_SIZE(t) == 0 ||
	    hak_count_oocstr(HAK_OBJ_GET_CHAR_SLOT(t)) != HAK_OBJ_GET_SIZE(t))
	{
		/* invalid command arguments */
		goto oops;
	}

	cmd = hak_dupootobcstr(hak, HAK_OBJ_GET_CHAR_SLOT(t), HAK_NULL);
	if (!cmd) goto oops;

	/* TODO: we need a bidirectional popen.. replace it with our own impl. */
	pp = popen(cmd, "r");
	if (!pp) goto oops;

	if (!HAK_IN_SMPTR_RANGE(pp))
	{
		pclose(pp);
		goto oops;
	}

/* using smptr in this mannger is dangerous. because the caller may set random values to other function like pclose...... */
	HAK_STACK_SETRET(hak, nargs, HAK_SMPTR_TO_OOP(pp));
	return HAK_PF_SUCCESS;

oops:
	// TODO: set return value..
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t pf_sys_pclose (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t t;

	t = HAK_STACK_GETARG(hak, nargs, 0);
	if (HAK_OOP_IS_SMPTR(t))
	{
		FILE* pp;
		pp = (FILE*)HAK_OOP_TO_SMPTR(t);
		if (pp) pclose(pp);
	}

	HAK_STACK_SETRET(hak, nargs, HAK_SMOOI_TO_OOP(0));
	return HAK_PF_SUCCESS;
}

static hak_pfinfo_t pfinfos[] =
{
	{ "close",       { HAK_PFBASE_FUNC,  pf_sys_close,        1,  1 } },
	{ "open",        { HAK_PFBASE_FUNC,  pf_sys_open,         2,  3 } },
	{ "pclose",      { HAK_PFBASE_FUNC,  pf_sys_pclose,       1,  1 } },
	{ "pipe",        { HAK_PFBASE_FUNC,  pf_sys_pipe,         0,  0 } },
	{ "popen",       { HAK_PFBASE_FUNC,  pf_sys_popen,        1,  2 } },
	{ "random",      { HAK_PFBASE_FUNC,  pf_sys_random,       0,  0 } },
	{ "read",        { HAK_PFBASE_FUNC,  pf_sys_read,         2,  4 } },
	{ "srandom",     { HAK_PFBASE_FUNC,  pf_sys_srandom,      1,  1 } },
	{ "stime",       { HAK_PFBASE_FUNC,  pf_sys_stime,        1,  1 } },
	{ "time",        { HAK_PFBASE_FUNC,  pf_sys_time,         0,  0 } },
	{ "write",       { HAK_PFBASE_FUNC,  pf_sys_write,        2,  4 } }
};

/* ------------------------------------------------------------------------ */

static hak_pfbase_t* query (hak_t* hak, hak_mod_t* mod, const hak_ooch_t* name, hak_oow_t namelen)
{
	return hak_findpfbase(hak, pfinfos, HAK_COUNTOF(pfinfos), name, namelen);
}

static void unload (hak_t* hak, hak_mod_t* mod)
{
}

int hak_mod_sys (hak_t* hak, hak_mod_t* mod)
{
	mod->query = query;
	mod->unload = unload; 
	mod->ctx = HAK_NULL;
	return 0;
}
