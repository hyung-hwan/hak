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

#ifndef _HAK_PIO_H_
#define _HAK_PIO_H_

#include <hak-cmn.h>

/** \file
 * This file provides a piped interface to a child process encapsulated in the
 * #hak_pio_t type. You can execute a child process, read and write to its
 * stdin, stdout and stderr, wait for it, and terminate it. It offers more
 * control than popen()/pclose(): three independent pipes instead of one,
 * cross-wiring (2>&1, 1>&2), redirection to the null device, dropping
 * descriptors, non-blocking pipes and a pluggable environment.
 *
 * hak_pio_read() and hak_pio_write() move raw bytes. There is no buffering or
 * character-set conversion layer on the data path; only the command string
 * itself is subject to #hak_ooch_t handling.
 *
 * \code
 * #include <hak-pio.h>
 *
 * hak_pio_t* pio;
 * hak_bch_t buf[256];
 * hak_ooi_t n;
 *
 * pio = hak_pio_open(hak, 0, HAK_T("ls -l | head -3"),
 *                    HAK_PIO_SHELL | HAK_PIO_READOUT, HAK_NULL, HAK_NULL);
 * if (!pio) { ... hak_geterrnum(hak) ... }
 *
 * while ((n = hak_pio_read(pio, HAK_PIO_OUT, buf, HAK_SIZEOF(buf))) > 0)
 * {
 *   ...
 * }
 *
 * hak_pio_end(pio, HAK_PIO_OUT);
 * hak_pio_wait(pio);
 * hak_pio_close(pio);
 * \endcode
 */

/**
 * The hak_pio_flag_t type defines flags to compose the \a flags argument to
 * hak_pio_open() and hak_pio_init().
 */
enum hak_pio_flag_t
{
	/** execute the command through a system shell
	 *  (/bin/sh on unix/linux, cmd.exe on windows) */
	HAK_PIO_SHELL          = (1 << 3),

	/** indicate that the command passed to hak_pio_open()/hak_pio_init() is
	 *  a #hak_bch_t string. it is useful when #HAK_OOCH_IS_UCH is defined and
	 *  the caller already holds a byte string. */
	HAK_PIO_BCSTRCMD       = (1 << 4),

	/** don't attempt to close open file descriptors unknown to pio.
	 *  it is useful only on unix-like systems where file descriptors not set
	 *  with FD_CLOEXEC are inherited by a child process. you're advised to set
	 *  this option if all normal file descriptors in your application are open
	 *  with FD_CLOEXEC set. it can skip checking a bunch of file descriptors
	 *  and arranging to close them to prevent inheritance. */
	HAK_PIO_NOCLOEXEC      = (1 << 5),

	/** indicate that the command passed to hak_pio_open()/hak_pio_init() is a
	 *  pointer to a #hak_pio_fnc_t instead of a command string. supported on
	 *  unix/linux only.
	 *
	 *  the child runs the given function without exec()ing, so it inherits the
	 *  parent's stdio buffers along with everything else. flush what you care
	 *  about before the call - anything still pending in the parent's buffers
	 *  is duplicated into the child, and lands in the child's stdout (that is,
	 *  in the pipe the parent reads) if anything there flushes it. pio itself
	 *  leaves the child through _exit(), which does not flush, but a worker
	 *  that calls exit() or touches stdio will. */
	HAK_PIO_FNCCMD         = (1 << 6),

	/** write to stdin of a child process */
	HAK_PIO_WRITEIN        = (1 << 8),
	/** read stdout of a child process */
	HAK_PIO_READOUT        = (1 << 9),
	/** read stderr of a child process */
	HAK_PIO_READERR        = (1 << 10),

	/** redirect stderr to stdout (2>&1, requires #HAK_PIO_READOUT) */
	HAK_PIO_ERRTOOUT       = (1 << 11),
	/** redirect stdout to stderr (1>&2, requires #HAK_PIO_READERR) */
	HAK_PIO_OUTTOERR       = (1 << 12),

	/** redirect stdin from the null device (</dev/null, <NUL) */
	HAK_PIO_INTONUL        = (1 << 13),
	/** redirect stderr to the null device (2>/dev/null, 2>NUL) */
	HAK_PIO_ERRTONUL       = (1 << 14),
	/** redirect stdout to the null device (>/dev/null, >NUL) */
	HAK_PIO_OUTTONUL       = (1 << 15),

	/** drop stdin */
	HAK_PIO_DROPIN         = (1 << 16),
	/** drop stdout */
	HAK_PIO_DROPOUT        = (1 << 17),
	/** drop stderr */
	HAK_PIO_DROPERR        = (1 << 18),

	/** do not reread if read has been interrupted */
	HAK_PIO_READNORETRY    = (1 << 21),
	/** do not rewrite if write has been interrupted */
	HAK_PIO_WRITENORETRY   = (1 << 22),
	/** return immediately from hak_pio_wait() if a child has not exited */
	HAK_PIO_WAITNOBLOCK    = (1 << 23),
	/** do not wait again if waitpid has been interrupted */
	HAK_PIO_WAITNORETRY    = (1 << 24),

	/** put stdin to non-blocking mode (only on supported platforms) */
	HAK_PIO_INNOBLOCK      = (1 << 25),
	/** put stdout to non-blocking mode (only on supported platforms) */
	HAK_PIO_OUTNOBLOCK     = (1 << 26),
	/** put stderr to non-blocking mode (only on supported platforms) */
	HAK_PIO_ERRNOBLOCK     = (1 << 27)
};
typedef enum hak_pio_flag_t hak_pio_flag_t;

/**
 * The hak_pio_hid_t type defines the IDs of the pipes established to a child
 * process.
 */
enum hak_pio_hid_t
{
	HAK_PIO_IN  = 0, /**< stdin of a child process */
	HAK_PIO_OUT = 1, /**< stdout of a child process */
	HAK_PIO_ERR = 2  /**< stderr of a child process */
};
typedef enum hak_pio_hid_t hak_pio_hid_t;

/**
 * The hak_pio_env_mk_type_t type defines the shape of the environment block
 * requested from a #hak_pio_env_mk_t callback.
 */
enum hak_pio_env_mk_type_t
{
	HAK_PIO_ENV_MK_BPP, /**< P1 ... PN HAK_NULL where P1 .. PN point to "K=V" */
	HAK_PIO_ENV_MK_BPN  /**< "K1=V\0K2=V\0\0" */
};
typedef enum hak_pio_env_mk_type_t hak_pio_env_mk_type_t;

/**
 * The hak_pio_env_mk_t type defines a callback that builds the environment
 * block handed to a child process. Return #HAK_NULL to make hak_pio_open() or
 * hak_pio_init() fail.
 *
 * \a type is #HAK_PIO_ENV_MK_BPP on unix (an execve() style vector) and
 * #HAK_PIO_ENV_MK_BPN on windows (a CreateProcess() style flat block).
 *
 * pio never frees the block returned by the callback and never invokes a
 * matching deallocation callback. The caller must track and release whatever
 * it allocated for the environment itself.
 */
typedef void* (*hak_pio_env_mk_t) (
	hak_pio_env_mk_type_t type,
	void*                 ctx
);

typedef int (*hak_pio_fncptr_t) (void* ctx);

/**
 * The hak_pio_fnc_t type points to the function executed in a child process
 * when #HAK_PIO_FNCCMD is specified.
 */
typedef struct hak_pio_fnc_t hak_pio_fnc_t;
struct hak_pio_fnc_t
{
	hak_pio_fncptr_t ptr;
	void*            ctx;
};

#if defined(_WIN32)
	/* <winnt.h> => typedef PVOID HANDLE; */
	typedef void* hak_pio_hnd_t; /**< defines a pipe handle type */
	typedef void* hak_pio_pid_t; /**< defines a process handle type */
#	define HAK_PIO_HND_NIL ((hak_pio_hnd_t)HAK_NULL)
#	define HAK_PIO_PID_NIL ((hak_pio_pid_t)HAK_NULL)
#else
	typedef int hak_pio_hnd_t;   /**< defines a pipe handle type */
	typedef int hak_pio_pid_t;   /**< defines a process handle type */
#	define HAK_PIO_HND_NIL ((hak_pio_hnd_t)-1)
#	define HAK_PIO_PID_NIL ((hak_pio_pid_t)-1)
#endif

typedef struct hak_pio_t hak_pio_t;

/**
 * The hak_pio_t type holds the state of piped I/O to a child process. The
 * hak_pio_xxx() functions are written around it. Do not change the value of
 * each field directly.
 */
struct hak_pio_t
{
	hak_t*        hak;
	int           flags;      /**< options */
	hak_pio_pid_t child;      /**< handle to a child process */
	hak_pio_hnd_t handle[3];  /**< pipe handles indexed by #hak_pio_hid_t */
};

/** access the \a child field of the #hak_pio_t structure */
#define HAK_PIO_CHILD(pio)       ((pio)->child)
/** get the native handle for \a hid from the #hak_pio_t structure */
#define HAK_PIO_HANDLE(pio,hid)  ((pio)->handle[hid])

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * The hak_pio_open() function executes the command \a cmd and establishes
 * pipes to it. #HAK_PIO_SHELL causes the function to execute \a cmd through
 * the default shell of the underlying system: /bin/sh on unix, cmd.exe on
 * windows. On unix without #HAK_PIO_SHELL, a full path to the command is
 * required as no PATH lookup is performed.
 *
 * \a cmd is a #hak_ooch_t string, or a #hak_bch_t string if
 * #HAK_PIO_BCSTRCMD is set, or a pointer to a #hak_pio_fnc_t if
 * #HAK_PIO_FNCCMD is set.
 *
 * \a xtnsize bytes of extra space are allocated after the #hak_pio_t structure
 * and zero-initialized; reach them with hak_pio_getxtn().
 *
 * \return #hak_pio_t pointer on success, #HAK_NULL on failure
 */
HAK_EXPORT hak_pio_t* hak_pio_open (
	hak_t*           hak,
	hak_oow_t        xtnsize, /**< extension size in bytes */
	const void*      cmd,     /**< command to execute */
	int              flags,   /**< 0 or a number OR'ed of the #hak_pio_flag_t enumerators */
	hak_pio_env_mk_t env_mk,  /**< environment builder, or #HAK_NULL to inherit */
	void*            env_ctx  /**< context passed to \a env_mk */
);

/**
 * The hak_pio_close() function closes the pipes to a child process, waits for
 * the child process to exit, and frees the #hak_pio_t structure.
 */
HAK_EXPORT void hak_pio_close (
	hak_pio_t* pio /**< pio object */
);

/**
 * The hak_pio_init() function performs the same task as hak_pio_open() except
 * that the caller provides the #hak_pio_t structure.
 * \return 0 on success, -1 on failure
 */
HAK_EXPORT int hak_pio_init (
	hak_pio_t*       pio,     /**< pio object */
	hak_t*           hak,
	const void*      cmd,     /**< command to execute */
	int              flags,   /**< 0 or a number OR'ed of the #hak_pio_flag_t enumerators */
	hak_pio_env_mk_t env_mk,  /**< environment builder, or #HAK_NULL to inherit */
	void*            env_ctx  /**< context passed to \a env_mk */
);

/**
 * The hak_pio_fini() function performs the same task as hak_pio_close() except
 * that it does not free the #hak_pio_t structure pointed to by \a pio.
 */
HAK_EXPORT void hak_pio_fini (
	hak_pio_t* pio /**< pio object */
);

#if defined(HAK_HAVE_INLINE)
static HAK_INLINE void* hak_pio_getxtn (hak_pio_t* pio) { return (void*)(pio + 1); }
#else
#define hak_pio_getxtn(pio) ((void*)((hak_pio_t*)(pio) + 1))
#endif

/**
 * The hak_pio_gethnd() function gets the native handle of a pipe.
 * \return pipe handle, #HAK_PIO_HND_NIL if the pipe is not established or has
 *         been closed
 */
HAK_EXPORT hak_pio_hnd_t hak_pio_gethnd (
	const hak_pio_t* pio, /**< pio object */
	hak_pio_hid_t    hid  /**< handle ID */
);

/**
 * The hak_pio_getchild() function gets the process handle of a child.
 * \return process handle
 */
HAK_EXPORT hak_pio_pid_t hak_pio_getchild (
	const hak_pio_t* pio /**< pio object */
);

/**
 * The hak_pio_read() function reads at most \a size bytes and stores them into
 * the buffer pointed to by \a buf.
 * \return -1 on failure, 0 on EOF, data length read on success
 */
HAK_EXPORT hak_ooi_t hak_pio_read (
	hak_pio_t*    pio,  /**< pio object */
	hak_pio_hid_t hid,  /**< handle ID */
	void*         buf,  /**< buffer to fill */
	hak_oow_t     size  /**< buffer size */
);

/**
 * The hak_pio_write() function writes up to \a size bytes from the buffer
 * pointed to by \a data.
 * \return -1 on failure, data length written on success
 */
HAK_EXPORT hak_ooi_t hak_pio_write (
	hak_pio_t*    pio,   /**< pio object */
	hak_pio_hid_t hid,   /**< handle ID */
	const void*   data,  /**< data to write */
	hak_oow_t     size   /**< data size */
);

/**
 * The hak_pio_end() function closes a pipe to a child process. A child that
 * waits for EOF on its stdin needs this before it will proceed.
 */
HAK_EXPORT void hak_pio_end (
	hak_pio_t*    pio, /**< pio object */
	hak_pio_hid_t hid  /**< handle ID */
);

/**
 * The hak_pio_wait() function waits for a child process to terminate.
 * #HAK_PIO_WAITNORETRY causes the function to fail with #HAK_EINTR if the
 * underlying system call has been interrupted. If #HAK_PIO_WAITNOBLOCK is
 * used, a return value of 256 indicates that the child process has not
 * terminated. Otherwise 256 is never returned.
 *
 * \return
 *  -1 on error, 256 if the child is alive and #HAK_PIO_WAITNOBLOCK is used,
 *  a number between 0 and 255 inclusive if the child process ends normally,
 *  256 + signal number if the child process is terminated by a signal.
 */
HAK_EXPORT int hak_pio_wait (
	hak_pio_t* pio /**< pio object */
);

/**
 * The hak_pio_kill() function terminates a child process by force. Know the
 * danger of calling this function: it can kill an unrelated process if the
 * child has already terminated and its handle has been reused.
 * \return 0 on success, -1 on failure
 */
HAK_EXPORT int hak_pio_kill (
	hak_pio_t* pio /**< pio object */
);

#if defined(__cplusplus)
}
#endif

#endif
