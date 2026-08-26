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
#include <hak-pio.h>
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

/* ------------------------------------------------------------------------ *
 * CHILD PROCESSES
 *
 * A child is represented as a group of handles: one HAK_HND_TYPE_PROC node
 * holding the hak_pio_t, plus one HAK_HND_TYPE_PIPE node per requested stream.
 * The pipe nodes are owned by the proc node, so tearing the proc node down
 * tears the whole group down in the right order - children first, which is
 * what lets each pipe unbind itself from the multiplexer while its descriptor
 * is still valid.
 *
 * pio owns those descriptors, so a stream node closes itself through
 * hak_pio_end() rather than close(2). That keeps pio's own view consistent -
 * it nils the handle it just closed, so the later hak_pio_free() will not
 * close it twice - and it means sys.close on a child's stdin really does send
 * EOF, which is how a filter like `tr` is told to flush and exit.
 *
 * Every pipe is non-blocking, so sys.read and sys.write on a child's streams
 * behave exactly like they do on a sys.pipe handle - including returning -1
 * for "would block", which is what makes a child usable from a coprocess
 * without stalling the VM.
 * ------------------------------------------------------------------------ */

/* kept in the pio extension area, so no separate allocation is needed */
struct proc_xtn_t
{
	int reaped;  /* has the child been waited on already? */
	int status;  /* ...and what did it exit with */
};
typedef struct proc_xtn_t proc_xtn_t;

static void proc_dtor (hak_t* hak, hak_hnd_t* hnd)
{
	/* hak_pio_free() ends the pipes, reaps the child - killing it if it is
	 * still running - and frees the object, all without an unbounded wait.
	 * this runs from sys.pclose and from hak_finihndtab() alike, so a script
	 * that simply forgets a child still cannot leak one. */
	hak_pio_free((hak_pio_t*)hnd->u.ptr);
}

/* close a child's stream through pio, so that pio stops believing it still
 * owns the descriptor. the stream is identified by matching the descriptor,
 * which is unambiguous because a pio's three ends are always distinct. */
static void stream_dtor (hak_t* hak, hak_hnd_t* hnd)
{
	hak_hnd_t* ph;

	/* the owner is still alive here: hak_closehnd() closes what a node owns
	 * before closing the node itself */
	ph = hak_gethnd(hak, hnd->owner, HAK_HND_TYPE_PROC);
	if (ph)
	{
		hak_pio_t* pio = (hak_pio_t*)ph->u.ptr;
		hak_pio_hid_t hid;

		for (hid = HAK_PIO_IN; hid <= HAK_PIO_ERR; hid++)
		{
			if (hak_pio_gethnd(pio, hid) == (hak_pio_hnd_t)hnd->u.fd)
			{
				hak_pio_end(pio, hid);
				return;
			}
		}
	}

	/* the owner is gone, or the stream is no longer pio's - fall back so the
	 * descriptor is not leaked */
	close(hnd->u.fd);
}

/* wrap one of the child's streams as a pipe handle owned by the proc node */
static hak_hnd_t* wrap_stream (hak_t* hak, hak_pio_t* pio, hak_pio_hid_t hid, hak_hnd_t* owner)
{
	hak_hnd_t* h;

	h = hak_wrapfd(hak, (int)hak_pio_gethnd(pio, hid), HAK_HND_TYPE_PIPE, 0);
	if (HAK_UNLIKELY(!h)) return HAK_NULL;

	hak_ownhnd(hak, h, owner);
	h->dtor = stream_dtor;
	return h;
}

/* (sys.popen cmd [mode]) -> #[proc in out err]
 *
 * mode is any combination of 'r' (read the child's stdout), 'w' (write to its
 * stdin) and 'e' (read its stderr); the default is "r". A stream that was not
 * requested comes back as nil. The command is run through a shell, as popen()
 * does.
 */
static hak_pfrc_t pf_sys_popen (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_t cmdoop, arr;
	hak_pio_t* pio;
	proc_xtn_t* x;
	hak_hnd_t *ph, *ih = HAK_NULL, *oh = HAK_NULL, *eh = HAK_NULL;
	int flags;

	cmdoop = HAK_STACK_GETARG(hak, nargs, 0);
	if (!HAK_OBJ_IS_CHAR_POINTER(cmdoop) || HAK_OBJ_GET_SIZE(cmdoop) == 0 ||
	    hak_count_oocstr(HAK_OBJ_GET_CHAR_SLOT(cmdoop)) != HAK_OBJ_GET_SIZE(cmdoop))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "command not a proper string - %O", cmdoop);
		return HAK_PF_FAILURE;
	}

	/* every wait is non-blocking; sys.pwait reports 256 for a live child
	 * rather than stopping every other coprocess. */
	flags = HAK_PIO_SHELL | HAK_PIO_WAITNOBLOCK;

	if (nargs >= 2)
	{
		hak_oop_t m = HAK_STACK_GETARG(hak, nargs, 1);
		const hak_ooch_t* p;
		hak_oow_t i, len;

		if (!HAK_OBJ_IS_CHAR_POINTER(m))
		{
			hak_seterrbfmt(hak, HAK_EINVAL, "mode not a string - %O", m);
			return HAK_PF_FAILURE;
		}

		p = HAK_OBJ_GET_CHAR_SLOT(m);
		len = HAK_OBJ_GET_SIZE(m);
		for (i = 0; i < len; i++)
		{
			switch (p[i])
			{
				case 'r': flags |= HAK_PIO_READOUT | HAK_PIO_OUTNOBLOCK; break;
				case 'w': flags |= HAK_PIO_WRITEIN | HAK_PIO_INNOBLOCK;  break;
				case 'e': flags |= HAK_PIO_READERR | HAK_PIO_ERRNOBLOCK; break;
				default:
					hak_seterrbfmt(hak, HAK_EINVAL, "unrecognized popen mode - %O", m);
					return HAK_PF_FAILURE;
			}
		}
	}
	else flags |= HAK_PIO_READOUT | HAK_PIO_OUTNOBLOCK;

	pio = hak_pio_open(hak, HAK_SIZEOF(proc_xtn_t), HAK_OBJ_GET_CHAR_SLOT(cmdoop), flags, HAK_NULL, HAK_NULL);
	if (HAK_UNLIKELY(!pio)) return HAK_PF_FAILURE;

	x = (proc_xtn_t*)hak_pio_getxtn(pio);
	x->reaped = 0;
	x->status = 0;

	/* the proc node owns the pio object from here on: if any wrap below
	 * fails, closing it disposes of the child through proc_dtor(). */
	ph = hak_wrapptr(hak, pio, HAK_HND_TYPE_PROC, 0, proc_dtor);
	if (HAK_UNLIKELY(!ph))
	{
		hak_pio_free(pio);
		return HAK_PF_FAILURE;
	}

	if ((flags & HAK_PIO_WRITEIN) && !(ih = wrap_stream(hak, pio, HAK_PIO_IN, ph))) goto oops;
	if ((flags & HAK_PIO_READOUT) && !(oh = wrap_stream(hak, pio, HAK_PIO_OUT, ph))) goto oops;
	if ((flags & HAK_PIO_READERR) && !(eh = wrap_stream(hak, pio, HAK_PIO_ERR, ph))) goto oops;

	/* this may collect, but handle nodes live outside the object heap */
	arr = hak_makearray(hak, 4);
	if (HAK_UNLIKELY(!arr)) goto oops;

	HAK_OBJ_SET_OOP_VAL(arr, 0, HAK_SMOOI_TO_OOP(ph->id));
	HAK_OBJ_SET_OOP_VAL(arr, 1, ih? HAK_SMOOI_TO_OOP(ih->id): hak->_nil);
	HAK_OBJ_SET_OOP_VAL(arr, 2, oh? HAK_SMOOI_TO_OOP(oh->id): hak->_nil);
	HAK_OBJ_SET_OOP_VAL(arr, 3, eh? HAK_SMOOI_TO_OOP(eh->id): hak->_nil);

	HAK_STACK_SETRET(hak, nargs, arr);
	return HAK_PF_SUCCESS;

oops:
	hak_closehnd(hak, ph); /* takes the stream nodes and the child with it */
	return HAK_PF_FAILURE;
}

/* (sys.pwait proc) -> 0..255 | 256 + signo | 256 if still running
 *
 * Never blocks. A script that wants to wait can loop on this, or - once
 * SIGCHLD is routed to the signal descriptor - wait on that instead.
 */
static hak_pfrc_t pf_sys_pwait (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_hnd_t* ph;
	hak_pio_t* pio;
	proc_xtn_t* x;
	int n;

	ph = hak_gethndwithoop(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_HND_TYPE_PROC);
	if (HAK_UNLIKELY(!ph)) return HAK_PF_FAILURE;

	pio = (hak_pio_t*)ph->u.ptr;
	x = (proc_xtn_t*)hak_pio_getxtn(pio);

	if (x->reaped)
	{
		/* the child was waited on already; waitpid() would now fail with
		 * ECHILD, so report what it exited with instead */
		HAK_STACK_SETRET(hak, nargs, HAK_SMOOI_TO_OOP(x->status));
		return HAK_PF_SUCCESS;
	}

	n = hak_pio_wait(pio);
	if (n <= -1) return HAK_PF_FAILURE;

	if (n != 255 + 1)
	{
		x->reaped = 1;
		x->status = n;
	}

	HAK_STACK_SETRET(hak, nargs, HAK_SMOOI_TO_OOP(n));
	return HAK_PF_SUCCESS;
}

/* (sys.pkill proc) - SIGKILL the child */
static hak_pfrc_t pf_sys_pkill (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_hnd_t* ph;

	ph = hak_gethndwithoop(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_HND_TYPE_PROC);
	if (HAK_UNLIKELY(!ph)) return HAK_PF_FAILURE;

	if (hak_pio_kill((hak_pio_t*)ph->u.ptr) <= -1) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, hak->_nil);
	return HAK_PF_SUCCESS;
}

/* (sys.pclose proc) -> the child's exit status if it is known, else nil
 *
 * Tears the whole group down: the stream handles are released and unbound
 * from the multiplexer, and the child is reaped - killed first if it has not
 * exited. It does not wait for a running child to finish on its own, so it
 * cannot stall the other coprocesses; use sys.pwait for that.
 */
static hak_pfrc_t pf_sys_pclose (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_hnd_t* ph;
	proc_xtn_t* x;
	hak_oop_t retv;

	ph = hak_gethndwithoop(hak, HAK_STACK_GETARG(hak, nargs, 0), HAK_HND_TYPE_PROC);
	if (HAK_UNLIKELY(!ph)) return HAK_PF_FAILURE;

	x = (proc_xtn_t*)hak_pio_getxtn((hak_pio_t*)ph->u.ptr);
	retv = x->reaped? HAK_SMOOI_TO_OOP(x->status): hak->_nil;

	if (hak_closehnd(hak, ph) <= -1) return HAK_PF_FAILURE;

	HAK_STACK_SETRET(hak, nargs, retv);
	return HAK_PF_SUCCESS;
}

static hak_pfinfo_t pfinfos[] =
{
	{ "close",       { HAK_PFBASE_FUNC,  pf_sys_close,        1,  1 } },
	{ "open",        { HAK_PFBASE_FUNC,  pf_sys_open,         2,  3 } },
	{ "pclose",      { HAK_PFBASE_FUNC,  pf_sys_pclose,       1,  1 } },
	{ "pipe",        { HAK_PFBASE_FUNC,  pf_sys_pipe,         0,  0 } },
	{ "pkill",       { HAK_PFBASE_FUNC,  pf_sys_pkill,        1,  1 } },
	{ "popen",       { HAK_PFBASE_FUNC,  pf_sys_popen,        1,  2 } },
	{ "pwait",       { HAK_PFBASE_FUNC,  pf_sys_pwait,        1,  1 } },
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
