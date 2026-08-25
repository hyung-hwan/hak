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

#include <hak-pio.h>
#include "hak-prv.h"

#include <stdio.h>

/* ========================================================================= *
 * PLATFORM FEATURE DETECTION
 *
 * hak's configure probes spawn.h but not the individual functions below, so
 * the rest is guessed from platform macros. every one of these can be
 * overridden from the command line, e.g. -DHAK_PIO_HAVE_POSIX_SPAWN=0.
 * ========================================================================= */

#if !defined(_WIN32)

	/* configure probes each of these; the fallbacks below only matter for a
	 * build that bypasses configure (e.g. hak-msw.h / hak-dos.h). every macro
	 * can still be forced from the command line, e.g.
	 * -DHAK_PIO_HAVE_POSIX_SPAWN=0. */

#	if !defined(HAK_PIO_HAVE_POSIX_SPAWN)
#		if defined(HAVE_POSIX_SPAWN) && defined(HAVE_SPAWN_H) && \
		    !defined(__minix) && !defined(_SCO_DS)
#			define HAK_PIO_HAVE_POSIX_SPAWN 1
#		else
#			define HAK_PIO_HAVE_POSIX_SPAWN 0
#		endif
#	endif

#	if !defined(HAK_PIO_HAVE_GETRLIMIT)
#		if defined(HAVE_GETRLIMIT) && defined(HAVE_SYS_RESOURCE_H)
#			define HAK_PIO_HAVE_GETRLIMIT 1
#		else
#			define HAK_PIO_HAVE_GETRLIMIT 0
#		endif
#	endif

#	if !defined(HAK_PIO_HAVE_SYSCONF)
#		if defined(HAVE_SYSCONF)
#			define HAK_PIO_HAVE_SYSCONF 1
#		else
#			define HAK_PIO_HAVE_SYSCONF 0
#		endif
#	endif

#	if !defined(HAK_PIO_HAVE_CRT_EXTERNS_H)
#		if defined(HAVE_CRT_EXTERNS_H) || (defined(__APPLE__) && defined(__MACH__))
#			define HAK_PIO_HAVE_CRT_EXTERNS_H 1
#		else
#			define HAK_PIO_HAVE_CRT_EXTERNS_H 0
#		endif
#	endif

#endif

/* ========================================================================= *
 * SYSTEM HEADERS AND CALL WRAPPERS
 * ========================================================================= */

#if defined(_WIN32)

#	include <windows.h>

#elif defined(__OS2__) || defined(__DOS__)

	/* neither backend was carried over into this module */

#else

#	include <sys/types.h>
#	include <sys/stat.h>
#	if defined(HAVE_SYS_WAIT_H)
#		include <sys/wait.h>
#	endif
#	if HAK_PIO_HAVE_GETRLIMIT
#		include <sys/resource.h>
#	endif
#	include <unistd.h>
#	include <fcntl.h>
#	include <signal.h>
#	include <errno.h>
#	include <dirent.h>

#	if HAK_PIO_HAVE_POSIX_SPAWN
#		include <spawn.h>
#	endif

#	if HAK_PIO_HAVE_CRT_EXTERNS_H
#		include <crt_externs.h> /* MacOSX/darwin. _NSGetEnviron() */
#	endif

	typedef struct stat   pio_stat_t;
	typedef struct dirent pio_dirent_t;

	/* HAVE_DIRFD covers a real function, HAVE_DECL_DIRFD/dirfd a macro */
#	if defined(HAVE_DIRFD) || (defined(HAVE_DECL_DIRFD) && HAVE_DECL_DIRFD) || \
	    defined(dirfd) || defined(__GLIBC__) || defined(__APPLE__) || \
	    defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || \
	    defined(__DragonFly__) || defined(__sun)
#		define PIO_DIRFD(dir) dirfd(dir)
#	else
#		define PIO_DIRFD(dir) ((dir)->dd_fd)
#	endif

#	if HAK_PIO_HAVE_CRT_EXTERNS_H
#		define PIO_ENVIRON (*(_NSGetEnviron()))
#	else
		extern char** environ;
#		define PIO_ENVIRON environ
#	endif

#endif

/* the error the child exits with when it cannot reach exec() */
#define PIO_CHILD_OOPS_EXIT 128

/* ========================================================================= *
 * UNIX HELPERS
 * ========================================================================= */

#if !defined(_WIN32) && !defined(__OS2__) && !defined(__DOS__)

/* parse a decimal file descriptor number as found under /proc/self/fd */
static int parse_fd (const hak_bch_t* str, int* out)
{
	const hak_bch_t* p = str;
	long v = 0;

	if (*p == '\0') return -1;
	for (; *p != '\0'; p++)
	{
		if (*p < '0' || *p > '9') return -1;
		v = v * 10 + (*p - '0');
		if (v > 0x7FFFFFFFL) return -1; /* not a plausible descriptor */
	}

	*out = (int)v;
	return 0;
}

static int get_highest_fd (hak_pio_t* pio)
{
#if HAK_PIO_HAVE_GETRLIMIT
	struct rlimit rlim;
#endif
	int fd = -1;
	DIR* d;

#if defined(F_MAXFD)
	fd = fcntl(0, F_MAXFD, 0);
	if (fd >= 0) return fd;
#endif

	/* will getting the highest file descriptor be faster than attempting to
	 * close any file descriptors less than the system limit? */

	d = opendir("/proc/self/fd");
	if (!d)
	{
		hak_bch_t buf[64];
		/* plain snprintf() rather than hak_fmttobcstr(): this runs in the
		 * forked child on the fork()/exec() path, where touching hak state
		 * is best avoided. */
		snprintf(buf, HAK_COUNTOF(buf), "/proc/%d/fd", (int)getpid());
		d = opendir(buf);
		if (!d) d = opendir("/dev/fd"); /* Darwin, FreeBSD */
	}

	if (d)
	{
		int maxfd = -1;
		pio_dirent_t* de;
		while ((de = readdir(d)))
		{
			int xfd;

			if (de->d_name[0] == '.') continue;
			if (parse_fd(de->d_name, &xfd) <= -1) continue;

			if (xfd != PIO_DIRFD(d) && xfd > maxfd) maxfd = xfd;
		}

		closedir(d);
		return maxfd;
	}

#if HAK_PIO_HAVE_GETRLIMIT
	if (getrlimit(RLIMIT_NOFILE, &rlim) <= -1 || rlim.rlim_max == RLIM_INFINITY)
	{
	#if HAK_PIO_HAVE_SYSCONF
		fd = sysconf(_SC_OPEN_MAX);
	#endif
	}
	else fd = rlim.rlim_max;
#elif HAK_PIO_HAVE_SYSCONF
	fd = sysconf(_SC_OPEN_MAX);
#endif
	if (fd <= -1) fd = 1024; /* fallback */

	/* F_MAXFD is the highest fd, but RLIMIT_NOFILE and _SC_OPEN_MAX return
	 * the maximum number of file descriptors. make an adjustment. */
	if (fd > 0) fd--;

	return fd;
}

static int close_open_fds_using_proc (hak_pio_t* pio, hak_pio_hnd_t* excepts, hak_oow_t count)
{
	DIR* d;

	d = opendir("/proc/self/fd");
	if (!d)
	{
		hak_bch_t buf[64];
		snprintf(buf, HAK_COUNTOF(buf), "/proc/%d/fd", (int)getpid());
		d = opendir(buf);
	#if !defined(_SCO_DS)
		/* on SCO OpenServer, a range of file descriptors starting from 0 are
		 * listed under /dev/fd regardless of opening state. And some high
		 * numbered descriptors are not listed. not reliable */
		if (!d) d = opendir("/dev/fd"); /* Darwin, FreeBSD */
	#endif
	}

	if (d)
	{
		pio_dirent_t* de;
		while ((de = readdir(d)))
		{
			int fd;

			if (de->d_name[0] == '.') continue;
			if (parse_fd(de->d_name, &fd) <= -1) continue;

			if (fd != PIO_DIRFD(d) && fd > 2)
			{
				hak_oow_t i;

				for (i = 0; i < count; i++)
				{
					if (fd == excepts[i]) goto skip_close;
				}

				close(fd);

			skip_close:
				;
			}
		}

		closedir(d);
		return 0;
	}

	return -1;
}

struct param_t
{
	hak_bch_t*  mcmd;
	hak_bch_t*  fixed_argv[4];
	hak_bch_t** argv;
};
typedef struct param_t param_t;

static void free_param (hak_pio_t* pio, param_t* param)
{
	if (param->argv && param->argv != param->fixed_argv)
		hak_freemem(pio->hak, param->argv);
	if (param->mcmd) hak_freemem(pio->hak, param->mcmd);
}

/**
 * Split \a s in place into whitespace separated fields, honouring \a lquote /
 * \a rquote quoting and \a escape escaping, replacing each separator with a
 * terminating '\0'.
 *
 * \return the number of fields on success, -1 on failure
 */
static int split_cmd (hak_bch_t* s, hak_bch_t lquote, hak_bch_t rquote, hak_bch_t escape)
{
	hak_bch_t* p = s;
	hak_bch_t* o;
	int cnt = 0;

	while (*p != '\0')
	{
		o = p;
		while (hak_is_bch_space((hak_bchu_t)*p)) p++;
		if (o != p) { hak_copy_bcstr_unlimited(o, p); p = o; }

		if (lquote != '\0' && *p == lquote)
		{
			hak_copy_bcstr_unlimited(p, p + 1);

			for (;;)
			{
				if (*p == '\0') return -1; /* unterminated quote */

				if (escape != '\0' && *p == escape)
				{
					hak_copy_bcstr_unlimited(p, p + 1);
				}
				else
				{
					if (*p == rquote)
					{
						*p++ = '\0';
						cnt++;
						break;
					}
				}
				p++;
			}
		}
		else
		{
			o = p;
			for (;;)
			{
				if (*p == '\0')
				{
					if (o != p) cnt++;
					break;
				}
				if (hak_is_bch_space((hak_bchu_t)*p))
				{
					*p++ = '\0';
					cnt++;
					break;
				}
				p++;
			}
		}
	}

	return cnt;
}

static int make_param (hak_pio_t* pio, const void* cmd, int flags, param_t* param)
{
	hak_bch_t* mcmd = HAK_NULL;
	int mcmd_is_dup = 0;
	int fcnt = 0;

	HAK_MEMSET(param, 0, HAK_SIZEOF(*param));

	if (flags & HAK_PIO_BCSTRCMD)
	{
		if (flags & HAK_PIO_SHELL)
		{
			/* the string is handed to /bin/sh as a single argument and is
			 * never modified, so no private copy is needed. */
			mcmd = (hak_bch_t*)cmd;
		}
		else
		{
			/* split_cmd() rewrites the buffer, so a private copy is needed */
			mcmd = hak_dupbcstr(pio->hak, (const hak_bch_t*)cmd, HAK_NULL);
			if (HAK_UNLIKELY(!mcmd)) goto oops;
			mcmd_is_dup = 1;
		}
	}
	else
	{
		/* hak_dupootobcstr() is a plain duplication when HAK_OOCH_IS_BCH and a
		 * character-set conversion when HAK_OOCH_IS_UCH */
		mcmd = hak_dupootobcstr(pio->hak, (const hak_ooch_t*)cmd, HAK_NULL);
		if (HAK_UNLIKELY(!mcmd)) goto oops;
		mcmd_is_dup = 1;
	}

	if (flags & HAK_PIO_SHELL)
	{
		param->argv = param->fixed_argv;
		param->argv[0] = (hak_bch_t*)"/bin/sh";
		param->argv[1] = (hak_bch_t*)"-c";
		param->argv[2] = mcmd;
		param->argv[3] = HAK_NULL;
	}
	else
	{
		int i;
		hak_bch_t* mcmdptr;

		/* split_cmd() rewrites mcmd in place, so it is no longer worth
		 * reporting back once it has failed */
		fcnt = split_cmd(mcmd, '\"', '\"', '\\');
		if (fcnt < 0)
		{
			hak_seterrbfmt(pio->hak, HAK_EINVAL, "unbalanced quote in the command line");
			goto oops;
		}
		if (fcnt == 0)
		{
			hak_seterrbfmt(pio->hak, HAK_EINVAL, "blank command line");
			goto oops;
		}

		if ((hak_oow_t)fcnt < HAK_COUNTOF(param->fixed_argv))
		{
			param->argv = param->fixed_argv;
		}
		else
		{
			param->argv = (hak_bch_t**)hak_allocmem(pio->hak, (fcnt + 1) * HAK_SIZEOF(param->argv[0]));
			if (HAK_UNLIKELY(!param->argv)) goto oops;
		}

		mcmdptr = mcmd;
		for (i = 0; i < fcnt; i++)
		{
			param->argv[i] = mcmdptr;
			while (*mcmdptr != '\0') mcmdptr++;
			mcmdptr++;
		}
		param->argv[i] = HAK_NULL;
	}

	if (mcmd_is_dup) param->mcmd = mcmd;
	return 0;

oops:
	if (mcmd_is_dup) hak_freemem(pio->hak, mcmd);
	return -1;
}

static int assert_executable (hak_pio_t* pio, const hak_bch_t* path)
{
	pio_stat_t st;

	if (access(path, X_OK) <= -1)
	{
		hak_seterrbfmtwithsyserr(pio->hak, 0, errno, "cannot execute %hs", path);
		return -1;
	}

	if (stat(path, &st) <= -1)
	{
		hak_seterrbfmtwithsyserr(pio->hak, 0, errno, "cannot stat %hs", path);
		return -1;
	}

	if (!S_ISREG(st.st_mode))
	{
		hak_seterrbfmt(pio->hak, HAK_EACCES, "%hs not a regular file", path);
		return -1;
	}

	return 0;
}

#if HAK_PIO_HAVE_POSIX_SPAWN

static int is_fd_valid (int fd)
{
	return fcntl(fd, F_GETFD, 0) != -1 || errno != EBADF;
}

static int is_fd_valid_and_nocloexec (int fd)
{
	int flags = fcntl(fd, F_GETFD, 0);
	if (flags == -1)
	{
		if (errno == EBADF) return 0; /* invalid. return false */
		return -1; /* unknown. true but negative to indicate unknown */
	}
	return !(flags & FD_CLOEXEC)? 1: 0;
}

#endif

/**
 * fork() and exec() the child. exactly one of \a param and \a fnc is non-NULL:
 * \a param carries an argv vector for execve(), \a fnc carries a function
 * pointer to run in the child (#HAK_PIO_FNCCMD).
 */
static hak_pio_pid_t standard_fork_and_exec (hak_pio_t* pio, hak_pio_hnd_t pipes[], param_t* param, hak_pio_fnc_t* fnc, char* const* envp)
{
	hak_pio_pid_t pid;

	pid = fork();
	if (pid <= -1)
	{
		hak_seterrwithsyserr(pio->hak, 0, errno);
		return -1;
	}

	if (pid == 0)
	{
		/* child */
		hak_pio_hnd_t devnull = -1;

		if (!(pio->flags & HAK_PIO_NOCLOEXEC))
		{
			if (close_open_fds_using_proc(pio, pipes, 6) <= -1)
			{
				int fd = get_highest_fd(pio);

				/* close all other unknown open handles except stdin/out/err
				 * and the pipes. */
				while (fd > 2)
				{
					if (fd != pipes[0] && fd != pipes[1] &&
					    fd != pipes[2] && fd != pipes[3] &&
					    fd != pipes[4] && fd != pipes[5])
					{
						close(fd);
					}
					fd--;
				}
			}
		}

		if (pio->flags & HAK_PIO_WRITEIN)
		{
			/* child should read */
			close(pipes[1]);
			pipes[1] = HAK_PIO_HND_NIL;
			if (dup2(pipes[0], 0) <= -1) goto child_oops;
			close(pipes[0]);
			pipes[0] = HAK_PIO_HND_NIL;
		}

		if (pio->flags & HAK_PIO_READOUT)
		{
			/* child should write */
			close(pipes[2]);
			pipes[2] = HAK_PIO_HND_NIL;
			if (dup2(pipes[3], 1) <= -1) goto child_oops;

			if (pio->flags & HAK_PIO_ERRTOOUT)
			{
				if (dup2(pipes[3], 2) <= -1) goto child_oops;
			}

			close(pipes[3]);
			pipes[3] = HAK_PIO_HND_NIL;
		}

		if (pio->flags & HAK_PIO_READERR)
		{
			/* child should write */
			close(pipes[4]);
			pipes[4] = HAK_PIO_HND_NIL;
			if (dup2(pipes[5], 2) <= -1) goto child_oops;

			if (pio->flags & HAK_PIO_OUTTOERR)
			{
				if (dup2(pipes[5], 1) <= -1) goto child_oops;
			}

			close(pipes[5]);
			pipes[5] = HAK_PIO_HND_NIL;
		}

		if ((pio->flags & HAK_PIO_INTONUL) ||
		    (pio->flags & HAK_PIO_OUTTONUL) ||
		    (pio->flags & HAK_PIO_ERRTONUL))
		{
		#if defined(O_LARGEFILE)
			devnull = open("/dev/null", O_RDWR | O_LARGEFILE, 0);
		#else
			devnull = open("/dev/null", O_RDWR, 0);
		#endif
			if (devnull <= -1) goto child_oops;
		}

		if ((pio->flags & HAK_PIO_INTONUL)  && dup2(devnull, 0) <= -1) goto child_oops;
		if ((pio->flags & HAK_PIO_OUTTONUL) && dup2(devnull, 1) <= -1) goto child_oops;
		if ((pio->flags & HAK_PIO_ERRTONUL) && dup2(devnull, 2) <= -1) goto child_oops;

		if ((pio->flags & HAK_PIO_INTONUL) ||
		    (pio->flags & HAK_PIO_OUTTONUL) ||
		    (pio->flags & HAK_PIO_ERRTONUL))
		{
			close(devnull);
			devnull = -1;
		}

		if (pio->flags & HAK_PIO_DROPIN) close(0);
		if (pio->flags & HAK_PIO_DROPOUT) close(1);
		if (pio->flags & HAK_PIO_DROPERR) close(2);

		if (pio->flags & HAK_PIO_FNCCMD)
		{
			/* -----------------------------------------------
			 * the function pointer to execute has been given.
			 * -----------------------------------------------*/
			int retx;

			/* the function must use the context(fnc->ctx) passed in as a
			 * parameter to find out the actual environment values */
			retx = fnc->ptr(fnc->ctx);
			if (devnull >= 0) close(devnull);
			_exit(retx);
		}
		else
		{
			execve(param->argv[0], param->argv, envp);

			/* if exec fails, free 'param' which is an inherited pointer */
			free_param(pio, param);
		}

	child_oops:
		if (devnull >= 0) close(devnull);
		_exit(PIO_CHILD_OOPS_EXIT);
	}

	return pid;
}

#endif /* !_WIN32 && !__OS2__ && !__DOS__ */

/* ========================================================================= *
 * WIN32 HELPERS
 * ========================================================================= */

#if defined(_WIN32)

/* build "<prefix><cmd>" in a freshly allocated read-write buffer.
 * \a prefix may be HAK_NULL. */
static hak_bch_t* dup_cmdline (hak_pio_t* pio, const hak_bch_t* prefix, const void* cmd, int flags)
{
	hak_bch_t* mcmd;
	hak_bch_t* buf;
	hak_oow_t pl, cl;

	if (flags & HAK_PIO_BCSTRCMD)
		mcmd = hak_dupbcstr(pio->hak, (const hak_bch_t*)cmd, &cl);
	else
		mcmd = hak_dupootobcstr(pio->hak, (const hak_ooch_t*)cmd, &cl);
	if (HAK_UNLIKELY(!mcmd)) return HAK_NULL;

	if (!prefix) return mcmd; /* already a private read-write copy */

	pl = hak_count_bcstr(prefix);
	buf = (hak_bch_t*)hak_allocmem(pio->hak, (pl + cl + 1) * HAK_SIZEOF(*buf));
	if (HAK_UNLIKELY(!buf))
	{
		hak_freemem(pio->hak, mcmd);
		return HAK_NULL;
	}

	HAK_MEMCPY(buf, prefix, pl * HAK_SIZEOF(*buf));
	HAK_MEMCPY(buf + pl, mcmd, (cl + 1) * HAK_SIZEOF(*buf));
	hak_freemem(pio->hak, mcmd);
	return buf;
}

#endif

/* ========================================================================= *
 * COMMON
 * ========================================================================= */

static int set_pipe_nonblock (hak_pio_t* pio, hak_pio_hnd_t fd, int enabled)
{
#if defined(_WIN32)
	/* anonymous pipes created by CreatePipe() cannot be switched to
	 * non-blocking mode. */
	hak_seterrnum(pio->hak, HAK_ENOIMPL);
	return -1;
#elif defined(__OS2__) || defined(__DOS__)
	hak_seterrnum(pio->hak, HAK_ENOIMPL);
	return -1;
#elif defined(O_NONBLOCK)
	int flag = fcntl(fd, F_GETFL, 0);
	if (flag >= 0) flag = fcntl(fd, F_SETFL, (enabled? (flag | O_NONBLOCK): (flag & ~O_NONBLOCK)));
	if (flag <= -1) hak_seterrwithsyserr(pio->hak, 0, errno);
	return flag;
#else
	hak_seterrnum(pio->hak, HAK_ENOIMPL);
	return -1;
#endif
}

hak_pio_t* hak_pio_open (hak_t* hak, hak_oow_t xtnsize, const void* cmd, int flags, hak_pio_env_mk_t env_mk, void* env_ctx)
{
	hak_pio_t* pio;

	pio = (hak_pio_t*)hak_allocmem(hak, HAK_SIZEOF(hak_pio_t) + xtnsize);
	if (HAK_UNLIKELY(!pio)) return HAK_NULL;

	if (hak_pio_init(pio, hak, cmd, flags, env_mk, env_ctx) <= -1)
	{
		hak_freemem(hak, pio);
		return HAK_NULL;
	}

	HAK_MEMSET(pio + 1, 0, xtnsize);
	return pio;
}

void hak_pio_close (hak_pio_t* pio)
{
	hak_t* hak = pio->hak;
	hak_pio_fini(pio);
	hak_freemem(hak, pio);
}

int hak_pio_init (hak_pio_t* pio, hak_t* hak, const void* cmd, int flags, hak_pio_env_mk_t env_mk, void* env_ctx)
{
	hak_pio_hnd_t handle[6];
	int i;

#if defined(_WIN32)
	SECURITY_ATTRIBUTES secattr;
	PROCESS_INFORMATION procinfo;
	STARTUPINFOA startup;
	HANDLE windevnul = INVALID_HANDLE_VALUE;
	BOOL apiret;
	hak_bch_t* dupcmd = HAK_NULL;
	void* envblk = HAK_NULL;
	int create_retried;
#elif defined(__OS2__) || defined(__DOS__)
	/* nothing */
#elif HAK_PIO_HAVE_POSIX_SPAWN
	posix_spawn_file_actions_t fa;
	int fa_inited = 0;
	int pserr;
	posix_spawnattr_t psattr;
	hak_pio_pid_t pid;
	param_t param;
#else
	hak_pio_pid_t pid;
	param_t param;
#endif

	HAK_MEMSET(pio, 0, HAK_SIZEOF(*pio));
	pio->hak = hak;
	pio->flags = flags;
	/* the memset above leaves child at 0, which on unix is a live pid rather
	 * than HAK_PIO_PID_NIL(-1). without this, a hak_pio_wait() reached on a
	 * pio whose init failed would call waitpid(0, ...) and reap an arbitrary
	 * child of the calling application. */
	pio->child = HAK_PIO_PID_NIL;

	handle[0] = HAK_PIO_HND_NIL;
	handle[1] = HAK_PIO_HND_NIL;
	handle[2] = HAK_PIO_HND_NIL;
	handle[3] = HAK_PIO_HND_NIL;
	handle[4] = HAK_PIO_HND_NIL;
	handle[5] = HAK_PIO_HND_NIL;

#if defined(__OS2__) || defined(__DOS__)

	hak_seterrnum(hak, HAK_ENOIMPL);
	return -1;

#elif defined(_WIN32)
	/* http://msdn.microsoft.com/en-us/library/ms682499(VS.85).aspx */

	if (flags & HAK_PIO_FNCCMD)
	{
		/* running a function pointer in a child process requires fork() */
		hak_seterrbfmt(hak, HAK_ENOIMPL, "HAK_PIO_FNCCMD not supported on this platform");
		return -1;
	}

	secattr.nLength = HAK_SIZEOF(secattr);
	secattr.bInheritHandle = TRUE;
	secattr.lpSecurityDescriptor = HAK_NULL;

	if (flags & HAK_PIO_WRITEIN)
	{
		/* child reads, parent writes */
		if (CreatePipe(&handle[0], &handle[1], &secattr, 0) == FALSE)
		{
			hak_seterrwithsyserr(hak, 1, GetLastError());
			goto oops;
		}

		/* don't inherit write handle */
		if (SetHandleInformation(handle[1], HANDLE_FLAG_INHERIT, 0) == FALSE)
		{
			DWORD e = GetLastError();
			if (e != ERROR_CALL_NOT_IMPLEMENTED)
			{
				/* SetHandleInformation() is not implemented on win9x.
				 * so let's care only if it is implemented */
				hak_seterrwithsyserr(hak, 1, e);
				goto oops;
			}
		}

		/* handle[1] is the parent's end and is never inherited by the child,
		 * so switching it here - before the child exists - is equivalent to
		 * doing it afterwards, and keeps this failure out of the window in
		 * which a spawned child would have to be cleaned up. */
		if ((flags & HAK_PIO_INNOBLOCK) && set_pipe_nonblock(pio, handle[1], 1) <= -1) goto oops;
	}

	if (flags & HAK_PIO_READOUT)
	{
		/* child writes, parent reads */
		if (CreatePipe(&handle[2], &handle[3], &secattr, 0) == FALSE)
		{
			hak_seterrwithsyserr(hak, 1, GetLastError());
			goto oops;
		}

		/* don't inherit read handle */
		if (SetHandleInformation(handle[2], HANDLE_FLAG_INHERIT, 0) == FALSE)
		{
			DWORD e = GetLastError();
			if (e != ERROR_CALL_NOT_IMPLEMENTED)
			{
				hak_seterrwithsyserr(hak, 1, e);
				goto oops;
			}
		}

		if ((flags & HAK_PIO_OUTNOBLOCK) && set_pipe_nonblock(pio, handle[2], 1) <= -1) goto oops;
	}

	if (flags & HAK_PIO_READERR)
	{
		/* child writes, parent reads */
		if (CreatePipe(&handle[4], &handle[5], &secattr, 0) == FALSE)
		{
			hak_seterrwithsyserr(hak, 1, GetLastError());
			goto oops;
		}

		/* don't inherit read handle */
		if (SetHandleInformation(handle[4], HANDLE_FLAG_INHERIT, 0) == FALSE)
		{
			DWORD e = GetLastError();
			if (e != ERROR_CALL_NOT_IMPLEMENTED)
			{
				hak_seterrwithsyserr(hak, 1, e);
				goto oops;
			}
		}

		if ((flags & HAK_PIO_ERRNOBLOCK) && set_pipe_nonblock(pio, handle[4], 1) <= -1) goto oops;
	}

	/* allow a pure spawn/wait use case with no dedicated pipes.
	 * a system()-style caller may still want shell execution and custom
	 * environment handling without redirecting stdin/out/err. */

	if ((flags & HAK_PIO_INTONUL) ||
	    (flags & HAK_PIO_OUTTONUL) ||
	    (flags & HAK_PIO_ERRTONUL))
	{
		windevnul = CreateFileA(
			"NUL", GENERIC_READ | GENERIC_WRITE,
			FILE_SHARE_READ | FILE_SHARE_WRITE,
			&secattr, OPEN_EXISTING, 0, NULL
		);
		if (windevnul == INVALID_HANDLE_VALUE)
		{
			hak_seterrwithsyserr(hak, 1, GetLastError());
			goto oops;
		}
	}

	HAK_MEMSET(&procinfo, 0, HAK_SIZEOF(procinfo));
	HAK_MEMSET(&startup, 0, HAK_SIZEOF(startup));

	startup.cb = HAK_SIZEOF(startup);

	startup.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
	startup.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
	startup.hStdError = GetStdHandle(STD_ERROR_HANDLE);
	if (startup.hStdInput == INVALID_HANDLE_VALUE ||
	    startup.hStdOutput == INVALID_HANDLE_VALUE ||
	    startup.hStdError == INVALID_HANDLE_VALUE)
	{
		hak_seterrwithsyserr(hak, 1, GetLastError());
		goto oops;
	}

	if (flags & HAK_PIO_WRITEIN)
	{
		startup.hStdInput = handle[0];
	}

	if (flags & HAK_PIO_READOUT)
	{
		startup.hStdOutput = handle[3];
		if (flags & HAK_PIO_ERRTOOUT) startup.hStdError = handle[3];
	}

	if (flags & HAK_PIO_READERR)
	{
		startup.hStdError = handle[5];
		if (flags & HAK_PIO_OUTTOERR) startup.hStdOutput = handle[5];
	}

	if (flags & HAK_PIO_INTONUL) startup.hStdInput = windevnul;
	if (flags & HAK_PIO_OUTTONUL) startup.hStdOutput = windevnul;
	if (flags & HAK_PIO_ERRTONUL) startup.hStdError = windevnul;

	if (flags & HAK_PIO_DROPIN) startup.hStdInput = INVALID_HANDLE_VALUE;
	if (flags & HAK_PIO_DROPOUT) startup.hStdOutput = INVALID_HANDLE_VALUE;
	if (flags & HAK_PIO_DROPERR) startup.hStdError = INVALID_HANDLE_VALUE;

	startup.dwFlags |= STARTF_USESTDHANDLES;

	/* there is nothing to do for HAK_PIO_SHELL as CreateProcess takes the
	 * entire command line */

	create_retried = 0;

create_process:
	if (flags & HAK_PIO_SHELL)
	{
		static const hak_bch_t* cmdname[] =
		{
			"cmd.exe /c ",
			"command.com /c "
		};
		dupcmd = dup_cmdline(pio, cmdname[create_retried], cmd, flags);
	}
	else
	{
		/* CreateProcess requires the command buffer to be read-write */
		dupcmd = dup_cmdline(pio, HAK_NULL, cmd, flags);
	}

	if (HAK_UNLIKELY(!dupcmd)) goto oops;

	if (env_mk && !envblk)
	{
		envblk = env_mk(HAK_PIO_ENV_MK_BPN, env_ctx);
		if (HAK_UNLIKELY(!envblk))
		{
			hak_seterrbfmt(hak, HAK_EINVAL, "unable to compose the environment block");
			goto oops;
		}
	}

	apiret = CreateProcessA(
		HAK_NULL,  /* LPCSTR lpApplicationName */
		dupcmd,    /* LPSTR lpCommandLine */
		HAK_NULL,  /* LPSECURITY_ATTRIBUTES lpProcessAttributes */
		HAK_NULL,  /* LPSECURITY_ATTRIBUTES lpThreadAttributes */
		TRUE,      /* BOOL bInheritHandles */
		0,         /* DWORD dwCreationFlags */
		envblk,    /* LPVOID lpEnvironment */
		HAK_NULL,  /* LPCSTR lpCurrentDirectory */
		&startup,  /* LPSTARTUPINFOA lpStartupInfo */
		&procinfo  /* LPPROCESS_INFORMATION lpProcessInformation */
	);

	hak_freemem(hak, dupcmd);
	dupcmd = HAK_NULL;

	if (apiret == FALSE)
	{
		DWORD e = GetLastError();
		if (create_retried == 0 && (flags & HAK_PIO_SHELL) && e == ERROR_FILE_NOT_FOUND)
		{
			/* if it failed to execute cmd.exe, attempt to execute
			 * command.com. this is provision for old windows platforms */
			create_retried = 1;
			goto create_process;
		}

		hak_seterrwithsyserr(hak, 1, e);
		goto oops;
	}

	if (windevnul != INVALID_HANDLE_VALUE)
	{
		CloseHandle(windevnul);
		windevnul = INVALID_HANDLE_VALUE;
	}

	if (flags & HAK_PIO_WRITEIN)
	{
		CloseHandle(handle[0]);
		handle[0] = HAK_PIO_HND_NIL;
	}
	if (flags & HAK_PIO_READOUT)
	{
		CloseHandle(handle[3]);
		handle[3] = HAK_PIO_HND_NIL;
	}
	if (flags & HAK_PIO_READERR)
	{
		CloseHandle(handle[5]);
		handle[5] = HAK_PIO_HND_NIL;
	}

	CloseHandle(procinfo.hThread);
	pio->child = procinfo.hProcess;

#else /* unix */

	if (flags & HAK_PIO_WRITEIN)
	{
		if (pipe(&handle[0]) <= -1)
		{
			hak_seterrwithsyserr(hak, 0, errno);
			goto oops;
		}

		/* O_NONBLOCK belongs to the open file description, and handle[1] is
		 * the parent's end - a different description from the handle[0] the
		 * child inherits, and one the child closes anyway. so switching it
		 * here, before the child exists, is equivalent to doing it after the
		 * spawn, and keeps this failure out of the window in which a spawned
		 * child would have to be cleaned up. */
		if ((flags & HAK_PIO_INNOBLOCK) && set_pipe_nonblock(pio, handle[1], 1) <= -1) goto oops;
	}

	if (flags & HAK_PIO_READOUT)
	{
		if (pipe(&handle[2]) <= -1)
		{
			hak_seterrwithsyserr(hak, 0, errno);
			goto oops;
		}

		if ((flags & HAK_PIO_OUTNOBLOCK) && set_pipe_nonblock(pio, handle[2], 1) <= -1) goto oops;
	}

	if (flags & HAK_PIO_READERR)
	{
		if (pipe(&handle[4]) <= -1)
		{
			hak_seterrwithsyserr(hak, 0, errno);
			goto oops;
		}

		if ((flags & HAK_PIO_ERRNOBLOCK) && set_pipe_nonblock(pio, handle[4], 1) <= -1) goto oops;
	}

	/* allow a pure spawn/wait use case with no dedicated pipes. */

	if (flags & HAK_PIO_FNCCMD)
	{
		/* cmd is a hak_pio_fnc_t* in this case. HAK_PIO_FNCCMD always takes
		 * the plain fork()/exec() path since the function must run in a real
		 * child of this process. */
		pid = standard_fork_and_exec(pio, handle, HAK_NULL, (hak_pio_fnc_t*)cmd, HAK_NULL);
		if (pid <= -1) goto oops;
		pio->child = pid;
	}
	else
	{
		char* const* envp;

		if (env_mk)
		{
			envp = (char* const*)env_mk(HAK_PIO_ENV_MK_BPP, env_ctx);
			if (HAK_UNLIKELY(!envp))
			{
				hak_seterrbfmt(hak, HAK_EINVAL, "unable to compose the environment block");
				goto oops;
			}
			/* pio doesn't free the memory block returned by the callback
			 * function. there is no callback triggered for deallocation
			 * either. the caller side must track heap memory chunks allocated
			 * for this environment */
		}
		else
		{
			envp = PIO_ENVIRON;
		}

	#if HAK_PIO_HAVE_POSIX_SPAWN

		if ((pserr = posix_spawn_file_actions_init(&fa)) != 0)
		{
			hak_seterrwithsyserr(hak, 0, pserr);
			goto oops;
		}
		fa_inited = 1;

		if (flags & HAK_PIO_WRITEIN)
		{
			/* child should read */
			if ((pserr = posix_spawn_file_actions_addclose(&fa, handle[1])) != 0) goto ps_oops;
			if ((pserr = posix_spawn_file_actions_adddup2(&fa, handle[0], 0)) != 0) goto ps_oops;
			if ((pserr = posix_spawn_file_actions_addclose(&fa, handle[0])) != 0) goto ps_oops;
		}

		if (flags & HAK_PIO_READOUT)
		{
			/* child should write */
			if ((pserr = posix_spawn_file_actions_addclose(&fa, handle[2])) != 0) goto ps_oops;
			if ((pserr = posix_spawn_file_actions_adddup2(&fa, handle[3], 1)) != 0) goto ps_oops;
			if ((flags & HAK_PIO_ERRTOOUT) &&
			    (pserr = posix_spawn_file_actions_adddup2(&fa, handle[3], 2)) != 0) goto ps_oops;
			if ((pserr = posix_spawn_file_actions_addclose(&fa, handle[3])) != 0) goto ps_oops;
		}

		if (flags & HAK_PIO_READERR)
		{
			/* child should write */
			if ((pserr = posix_spawn_file_actions_addclose(&fa, handle[4])) != 0) goto ps_oops;
			if ((pserr = posix_spawn_file_actions_adddup2(&fa, handle[5], 2)) != 0) goto ps_oops;
			if ((flags & HAK_PIO_OUTTOERR) &&
			    (pserr = posix_spawn_file_actions_adddup2(&fa, handle[5], 1)) != 0) goto ps_oops;
			if ((pserr = posix_spawn_file_actions_addclose(&fa, handle[5])) != 0) goto ps_oops;
		}

		{
			int oflags = O_RDWR;
		#if defined(O_LARGEFILE)
			oflags |= O_LARGEFILE;
		#endif

			if ((flags & HAK_PIO_INTONUL) &&
			    (pserr = posix_spawn_file_actions_addopen(&fa, 0, "/dev/null", oflags, 0)) != 0) goto ps_oops;
			if ((flags & HAK_PIO_OUTTONUL) &&
			    (pserr = posix_spawn_file_actions_addopen(&fa, 1, "/dev/null", oflags, 0)) != 0) goto ps_oops;
			if ((flags & HAK_PIO_ERRTONUL) &&
			    (pserr = posix_spawn_file_actions_addopen(&fa, 2, "/dev/null", oflags, 0)) != 0) goto ps_oops;
		}

		/* there remains the chance of a race condition that 0, 1, 2 can be
		 * closed between addclose() and posix_spawn(). so checking the file
		 * descriptors with is_fd_valid() is just on a best-effort basis. */
		if ((flags & HAK_PIO_DROPIN) && is_fd_valid(0) &&
		    (pserr = posix_spawn_file_actions_addclose(&fa, 0)) != 0) goto ps_oops;
		if ((flags & HAK_PIO_DROPOUT) && is_fd_valid(1) &&
		    (pserr = posix_spawn_file_actions_addclose(&fa, 1)) != 0) goto ps_oops;
		if ((flags & HAK_PIO_DROPERR) && is_fd_valid(2) &&
		    (pserr = posix_spawn_file_actions_addclose(&fa, 2)) != 0) goto ps_oops;

		if (!(flags & HAK_PIO_NOCLOEXEC))
		{
			int fd = get_highest_fd(pio);
			while (fd > 2)
			{
				if (fd != handle[0] && fd != handle[1] &&
				    fd != handle[2] && fd != handle[3] &&
				    fd != handle[4] && fd != handle[5])
				{
					/* closing attempt on a best-effort basis. posix_spawn()
					 * fails if a file descriptor added with addclose() is
					 * closed before posix_spawn(). addclose() only if no
					 * FD_CLOEXEC is set or it's unknown. */
					if (is_fd_valid_and_nocloexec(fd) &&
					    (pserr = posix_spawn_file_actions_addclose(&fa, fd)) != 0) goto ps_oops;
				}
				fd--;
			}
		}

		if (make_param(pio, cmd, flags, &param) <= -1) goto oops;

		/* check whether the command (the one requested, or /bin/sh) is
		 * executable, to fail without trying to execute it - though this
		 * check alone isn't sufficient */
		if (assert_executable(pio, param.argv[0]) <= -1)
		{
			free_param(pio, &param);
			goto oops;
		}

		posix_spawnattr_init(&psattr);

		#if defined(__linux)
		#if !defined(POSIX_SPAWN_USEVFORK)
		#	define POSIX_SPAWN_USEVFORK 0x40
		#endif
		posix_spawnattr_setflags(&psattr, POSIX_SPAWN_USEVFORK);
		#endif

		pserr = posix_spawn(&pid, param.argv[0], &fa, &psattr, param.argv, envp);

		posix_spawnattr_destroy(&psattr);

		free_param(pio, &param);
		if (fa_inited)
		{
			posix_spawn_file_actions_destroy(&fa);
			fa_inited = 0;
		}
		if (pserr != 0)
		{
			hak_seterrwithsyserr(hak, 0, pserr);
			goto oops;
		}

		pio->child = pid;
		goto spawned;

	ps_oops:
		hak_seterrwithsyserr(hak, 0, pserr);
		goto oops;

	spawned:
		;

	#else /* !HAK_PIO_HAVE_POSIX_SPAWN */

		if (make_param(pio, cmd, flags, &param) <= -1) goto oops;

		/* check whether the command (the one requested, or /bin/sh) is
		 * executable, to fail without trying to execute it - though this
		 * check alone isn't sufficient */
		if (assert_executable(pio, param.argv[0]) <= -1)
		{
			free_param(pio, &param);
			goto oops;
		}

		pid = standard_fork_and_exec(pio, handle, &param, HAK_NULL, envp);
		if (pid <= -1)
		{
			free_param(pio, &param);
			goto oops;
		}

		/* parent */
		free_param(pio, &param);
		pio->child = pid;
	#endif
	}

	if (flags & HAK_PIO_WRITEIN)
	{
		/*
		 * 012345
		 * rw----
		 * X
		 * WRITE => 1
		 */
		close(handle[0]);
		handle[0] = HAK_PIO_HND_NIL;
	}

	if (flags & HAK_PIO_READOUT)
	{
		/*
		 * 012345
		 * --rw--
		 *    X
		 * READ => 2
		 */
		close(handle[3]);
		handle[3] = HAK_PIO_HND_NIL;
	}

	if (flags & HAK_PIO_READERR)
	{
		/*
		 * 012345
		 * ----rw
		 *      X
		 * READ => 4
		 */
		close(handle[5]);
		handle[5] = HAK_PIO_HND_NIL;
	}
#endif

	/* store the parent's ends of the pipes */
	pio->handle[HAK_PIO_IN] = handle[1];
	pio->handle[HAK_PIO_OUT] = handle[2];
	pio->handle[HAK_PIO_ERR] = handle[4];

	return 0;

oops:
	/* INVARIANT: no failure path exists between the spawn and the successful
	 * return, so pio->child is always HAK_PIO_PID_NIL here and there is never
	 * a live child to dispose of. everything after the spawn is either an
	 * unconditional close() or a plain struct assignment.
	 *
	 * if you ever add a step after the spawn that can fail, this stops being
	 * true and you must reap the child here instead - hak_pio_kill() it (a
	 * bare wait could block forever on a long-running child, or deadlock on
	 * one blocked writing to a pipe we still hold), then hak_pio_wait() for it
	 * with HAK_PIO_WAITNOBLOCK and HAK_PIO_WAITNORETRY cleared, saving and
	 * restoring the error with hak_geterrnum()/hak_seterrnum() around the pair
	 * so that the error which actually caused the failure survives. */

#if defined(_WIN32)
	if (dupcmd) hak_freemem(hak, dupcmd);
	if (windevnul != INVALID_HANDLE_VALUE) CloseHandle(windevnul);
#elif !defined(__OS2__) && !defined(__DOS__) && HAK_PIO_HAVE_POSIX_SPAWN
	if (fa_inited)
	{
		posix_spawn_file_actions_destroy(&fa);
		fa_inited = 0;
	}
#endif

	/* every slot is pre-set to HAK_PIO_HND_NIL, so sweeping all six with a NIL
	 * guard closes exactly what was opened, whichever flags were given. */
	for (i = 0; i < (int)HAK_COUNTOF(handle); i++)
	{
		if (handle[i] != HAK_PIO_HND_NIL)
		{
		#if defined(_WIN32)
			CloseHandle(handle[i]);
		#elif defined(__OS2__) || defined(__DOS__)
			/* no pipes are ever created on these platforms */
		#else
			close(handle[i]);
		#endif
		}
	}

	return -1;
}

void hak_pio_fini (hak_pio_t* pio)
{
	hak_pio_end(pio, HAK_PIO_ERR);
	hak_pio_end(pio, HAK_PIO_OUT);
	hak_pio_end(pio, HAK_PIO_IN);

	/* when closing, enable blocking and retrying */
	pio->flags &= ~HAK_PIO_WAITNOBLOCK;
	pio->flags &= ~HAK_PIO_WAITNORETRY;
	hak_pio_wait(pio);
}

hak_pio_hnd_t hak_pio_gethnd (const hak_pio_t* pio, hak_pio_hid_t hid)
{
	return pio->handle[hid];
}

hak_pio_pid_t hak_pio_getchild (const hak_pio_t* pio)
{
	return pio->child;
}

hak_ooi_t hak_pio_read (hak_pio_t* pio, hak_pio_hid_t hid, void* buf, hak_oow_t size)
{
	hak_pio_hnd_t hnd = pio->handle[hid];
#if defined(_WIN32)
	DWORD count;
#elif defined(__OS2__) || defined(__DOS__)
	/* nothing */
#else
	hak_ooi_t n;
#endif

	if (hnd == HAK_PIO_HND_NIL)
	{
		/* the stream is already closed */
		hak_seterrbfmt(pio->hak, HAK_EBADHND, "pipe not established or already closed");
		return (hak_ooi_t)-1;
	}

#if defined(_WIN32)

	if (size > (hak_oow_t)(HAK_TYPE_MAX(hak_ooi_t) & HAK_TYPE_MAX(DWORD)))
		size = (hak_oow_t)(HAK_TYPE_MAX(hak_ooi_t) & HAK_TYPE_MAX(DWORD));

	if (ReadFile(hnd, buf, (DWORD)size, &count, HAK_NULL) == FALSE)
	{
		/* ReadFile receives ERROR_BROKEN_PIPE when the write end is closed in
		 * the child process */
		if (GetLastError() == ERROR_BROKEN_PIPE) return 0;
		hak_seterrwithsyserr(pio->hak, 1, GetLastError());
		return -1;
	}
	return (hak_ooi_t)count;

#elif defined(__OS2__) || defined(__DOS__)

	hak_seterrnum(pio->hak, HAK_ENOIMPL);
	return -1;

#else

	if (size > (hak_oow_t)HAK_TYPE_MAX(hak_ooi_t))
		size = (hak_oow_t)HAK_TYPE_MAX(hak_ooi_t);

reread:
	n = read(hnd, buf, size);
	if (n <= -1)
	{
		if (errno == EINTR)
		{
			if (pio->flags & HAK_PIO_READNORETRY)
				hak_seterrnum(pio->hak, HAK_EINTR);
			else goto reread;
		}
		else
		{
			hak_seterrwithsyserr(pio->hak, 0, errno);
		}
	}

	return n;
#endif
}

hak_ooi_t hak_pio_write (hak_pio_t* pio, hak_pio_hid_t hid, const void* data, hak_oow_t size)
{
	hak_pio_hnd_t hnd = pio->handle[hid];
#if defined(_WIN32)
	DWORD count;
#elif defined(__OS2__) || defined(__DOS__)
	/* nothing */
#else
	hak_ooi_t n;
#endif

	if (hnd == HAK_PIO_HND_NIL)
	{
		/* the stream is already closed */
		hak_seterrbfmt(pio->hak, HAK_EBADHND, "pipe not established or already closed");
		return (hak_ooi_t)-1;
	}

#if defined(_WIN32)

	if (size > (hak_oow_t)(HAK_TYPE_MAX(hak_ooi_t) & HAK_TYPE_MAX(DWORD)))
		size = (hak_oow_t)(HAK_TYPE_MAX(hak_ooi_t) & HAK_TYPE_MAX(DWORD));

	if (WriteFile(hnd, data, (DWORD)size, &count, HAK_NULL) == FALSE)
	{
		hak_seterrwithsyserr(pio->hak, 1, GetLastError());
		return -1;
	}
	return (hak_ooi_t)count;

#elif defined(__OS2__) || defined(__DOS__)

	hak_seterrnum(pio->hak, HAK_ENOIMPL);
	return -1;

#else

	if (size > (hak_oow_t)HAK_TYPE_MAX(hak_ooi_t))
		size = (hak_oow_t)HAK_TYPE_MAX(hak_ooi_t);

rewrite:
	n = write(hnd, data, size);
	if (n <= -1)
	{
		if (errno == EINTR)
		{
			if (pio->flags & HAK_PIO_WRITENORETRY)
				hak_seterrnum(pio->hak, HAK_EINTR);
			else goto rewrite;
		}
		else
		{
			hak_seterrwithsyserr(pio->hak, 0, errno);
		}
	}
	return n;

#endif
}

void hak_pio_end (hak_pio_t* pio, hak_pio_hid_t hid)
{
	if (pio->handle[hid] != HAK_PIO_HND_NIL)
	{
#if defined(_WIN32)
		CloseHandle(pio->handle[hid]);
#elif defined(__OS2__) || defined(__DOS__)
		/* no pipes are ever created on these platforms */
#else
		close(pio->handle[hid]);
#endif
		pio->handle[hid] = HAK_PIO_HND_NIL;
	}
}

int hak_pio_wait (hak_pio_t* pio)
{
#if defined(_WIN32)

	DWORD ecode, w;

	if (pio->child == HAK_PIO_PID_NIL)
	{
		hak_seterrbfmt(pio->hak, HAK_ENOENT, "no child process to wait for");
		return -1;
	}

	w = WaitForSingleObject(pio->child, ((pio->flags & HAK_PIO_WAITNOBLOCK)? 0: INFINITE));
	if (w == WAIT_TIMEOUT)
	{
		/* the child process is still alive */
		return 255 + 1;
	}
	if (w != WAIT_OBJECT_0)
	{
		/* WAIT_FAILED, WAIT_ABANDONED */
		hak_seterrwithsyserr(pio->hak, 1, GetLastError());
		return -1;
	}

	if (GetExitCodeProcess(pio->child, &ecode) == FALSE)
	{
		/* close the handle anyway to prevent further errors when this function
		 * is called again */
		hak_seterrwithsyserr(pio->hak, 1, GetLastError());
		CloseHandle(pio->child);
		pio->child = HAK_PIO_PID_NIL;
		return -1;
	}

	/* close the handle here to emulate waitpid() as much as possible. */
	CloseHandle(pio->child);
	pio->child = HAK_PIO_PID_NIL;

	if (ecode == STILL_ACTIVE)
	{
		/* this should not happen as the control reaches here only when
		 * WaitForSingleObject() is successful. */
		hak_seterrbfmt(pio->hak, HAK_ESYSERR, "child process still active after a successful wait");
		return -1;
	}

	return ecode;

#elif defined(__OS2__) || defined(__DOS__)

	hak_seterrnum(pio->hak, HAK_ENOIMPL);
	return -1;

#else

	int opt = 0;
	int ret = -1;

	if (pio->child == HAK_PIO_PID_NIL)
	{
		hak_seterrbfmt(pio->hak, HAK_ENOENT, "no child process to wait for");
		return -1;
	}

	if (pio->flags & HAK_PIO_WAITNOBLOCK) opt |= WNOHANG;

	while (1)
	{
		int status, n;

		n = waitpid(pio->child, &status, opt);
		if (n <= -1)
		{
			if (errno == ECHILD)
			{
				/* most likely, the process has already been waitpid()ed on. */
				pio->child = HAK_PIO_PID_NIL;
			}
			else if (errno == EINTR)
			{
				if (!(pio->flags & HAK_PIO_WAITNORETRY)) continue;
			}

			hak_seterrwithsyserr(pio->hak, 0, errno);
			break;
		}

		if (n == 0)
		{
			/* when WNOHANG is not specified, 0 can't be returned */
			ret = 255 + 1;
			/* the child process is still alive */
			break;
		}

		if (n == pio->child)
		{
			if (WIFEXITED(status))
			{
				/* the child process ended normally */
				ret = WEXITSTATUS(status);
			}
			else if (WIFSIGNALED(status))
			{
				/* the child process was killed by a signal */
				ret = 255 + 1 + WTERMSIG(status);
			}
			else
			{
				/* not interested in WIFSTOPPED & WIFCONTINUED. in fact, this
				 * else-block should not be reached as WIFEXITED or WIFSIGNALED
				 * must be true. anyhow, just set the return value to 0. */
				ret = 0;
			}

			pio->child = HAK_PIO_PID_NIL;
			break;
		}
	}

	return ret;
#endif
}

int hak_pio_kill (hak_pio_t* pio)
{
#if defined(_WIN32)
	DWORD n;
#elif defined(__OS2__) || defined(__DOS__)
	/* nothing */
#else
	int n;
#endif

	if (pio->child == HAK_PIO_PID_NIL)
	{
		hak_seterrbfmt(pio->hak, HAK_ENOENT, "no child process to kill");
		return -1;
	}

#if defined(_WIN32)
	/* 9 was chosen below to treat TerminateProcess as kill -KILL. */
	n = TerminateProcess(pio->child, 255 + 1 + 9);
	if (n == FALSE)
	{
		hak_seterrwithsyserr(pio->hak, 1, GetLastError());
		return -1;
	}
	return 0;

#elif defined(__OS2__) || defined(__DOS__)

	hak_seterrnum(pio->hak, HAK_ENOIMPL);
	return -1;

#else
	n = kill(pio->child, SIGKILL);
	if (n <= -1) hak_seterrwithsyserr(pio->hak, 0, errno);
	return n;
#endif
}
