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

#if defined(_WIN32)
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <hak.h>
#include <hak-chr.h>
#include <hak-str.h>
#include <hak-utl.h>
#include <hak-opt.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
#include <locale.h>

#if defined(HAVE_ISOCLINE_H) && defined(HAVE_ISOCLINE_LIB)
#	include <isocline.h>
#	define USE_ISOCLINE
#endif

#if defined(_WIN32)
#	include <windows.h>
#	include <tchar.h>
#	include <io.h>
#	include <fcntl.h>
#	include <time.h>
#	include <signal.h>

#elif defined(__OS2__)
#	define INCL_DOSMODULEMGR
#	define INCL_DOSPROCESS
#	define INCL_DOSERRORS
#	include <os2.h>
#	include <signal.h>

#elif defined(__DOS__)
#	include <dos.h>
#	include <time.h>
#	include <signal.h>
#elif defined(macintosh)
#	include <Timer.h>
#else

#	include <sys/types.h>
#	include <errno.h>
#	include <unistd.h>
#	include <fcntl.h>

#	if defined(HAVE_TIME_H)
#		include <time.h>
#	endif
#	if defined(HAVE_SYS_TIME_H)
#		include <sys/time.h>
#	endif
#	if defined(HAVE_SIGNAL_H)
#		include <signal.h>
#	endif
#endif

#if defined(__DOS__) || defined(_WIN32) || defined(__OS2__)
#define FOPEN_R_FLAGS "rb"
#else
#define FOPEN_R_FLAGS "r"
#endif

typedef struct xtn_t xtn_t;
struct xtn_t
{
	const char* cci_path; /* main source file */
	/*const char* udi_path; */ /* not implemented as of snow */
	const char* udo_path;

	int vm_running;

	struct
	{
		hak_bch_t* ptr;
		hak_bch_t buf[1024]; /* not used if isocline is used */
		hak_oow_t len;
		hak_oow_t pos;
		int eof;
		hak_oow_t ncompexprs; /* number of compiled expressions */
	} feed;
	/*hak_oop_t sym_errstr;*/
};

/* ========================================================================= */

static hak_t* g_hak = HAK_NULL;

/* ========================================================================= */

static int vm_startup (hak_t* hak)
{
	xtn_t* xtn = (xtn_t*)hak_getxtn(hak);
	xtn->vm_running = 1;
	return 0;
}

static void vm_cleanup (hak_t* hak)
{
	xtn_t* xtn = (xtn_t*)hak_getxtn(hak);
	xtn->vm_running = 0;

#if defined(USE_ISOCLINE)
	if (xtn->feed.ptr && xtn->feed.ptr != xtn->feed.buf)
	{
		ic_free (xtn->feed.ptr);
		xtn->feed.ptr = HAK_NULL;
	}
#endif
}

/*
static void vm_checkbc (hak_t* hak, hak_oob_t bcode)
{
}
*/

static void on_gc_hak (hak_t* hak)
{
	/*xtn_t* xtn = (xtn_t*)hak_getxtn(hak);*/
	/*if (xtn->sym_errstr) xtn->sym_errstr = hak_moveoop(hak, xtn->sym_errstr);*/
}

/* ========================================================================= */

static int handle_logopt (hak_t* hak, const hak_bch_t* logstr)
{
	const hak_bch_t* cm, * flt;
	hak_bitmask_t logmask;
	hak_oow_t tlen, i;
	hak_bcs_t fname;

	static struct
	{
		const char* name;
		int op; /* 0: bitwise-OR, 1: bitwise-AND */
		hak_bitmask_t mask;
	} xtab[] =
	{
		{ "",           0, 0 },

		{ "app",        0, HAK_LOG_APP },
		{ "compiler",   0, HAK_LOG_COMPILER },
		{ "vm",         0, HAK_LOG_VM },
		{ "mnemonic",   0, HAK_LOG_MNEMONIC },
		{ "gc",         0, HAK_LOG_GC },
		{ "ic",         0, HAK_LOG_IC },
		{ "primitive",  0, HAK_LOG_PRIMITIVE },

		/* select a specific level */
		{ "fatal",      0, HAK_LOG_FATAL },
		{ "error",      0, HAK_LOG_ERROR },
		{ "warn",       0, HAK_LOG_WARN },
		{ "info",       0, HAK_LOG_INFO },
		{ "debug",      0, HAK_LOG_DEBUG },

		/* select a specific level or higher */
		{ "fatal+",     0, HAK_LOG_FATAL },
		{ "error+",     0, HAK_LOG_FATAL | HAK_LOG_ERROR },
		{ "warn+",      0, HAK_LOG_FATAL | HAK_LOG_ERROR | HAK_LOG_WARN },
		{ "info+",      0, HAK_LOG_FATAL | HAK_LOG_ERROR | HAK_LOG_WARN | HAK_LOG_INFO },
		{ "debug+",     0, HAK_LOG_FATAL | HAK_LOG_ERROR | HAK_LOG_WARN | HAK_LOG_INFO | HAK_LOG_DEBUG },

		/* select a specific level or lower */
		{ "fatal-",     0, HAK_LOG_FATAL | HAK_LOG_ERROR | HAK_LOG_WARN | HAK_LOG_INFO | HAK_LOG_DEBUG },
		{ "error-",     0, HAK_LOG_ERROR | HAK_LOG_WARN | HAK_LOG_INFO | HAK_LOG_DEBUG },
		{ "warn-",      0, HAK_LOG_WARN | HAK_LOG_INFO | HAK_LOG_DEBUG },
		{ "info-",      0, HAK_LOG_INFO | HAK_LOG_DEBUG },
		{ "debug-",     0, HAK_LOG_DEBUG },

		/* exclude a specific level */
		{ "-fatal",     1, ~(hak_bitmask_t)HAK_LOG_FATAL },
		{ "-error",     1, ~(hak_bitmask_t)HAK_LOG_ERROR },
		{ "-warn",      1, ~(hak_bitmask_t)HAK_LOG_WARN },
		{ "-info",      1, ~(hak_bitmask_t)HAK_LOG_INFO },
		{ "-debug",     1, ~(hak_bitmask_t)HAK_LOG_DEBUG },
	};

	cm = hak_find_bchar_in_bcstr(logstr, ',');
	if (cm)
	{
		fname.len = cm - logstr;
		logmask = 0;

		do
		{
			flt = cm + 1;

			cm = hak_find_bchar_in_bcstr(flt, ',');
			tlen = (cm)? (cm - flt): hak_count_bcstr(flt);

			for (i = 0; i < HAK_COUNTOF(xtab); i++)
			{
				if (hak_comp_bchars_bcstr(flt, tlen, xtab[i].name) == 0)
				{
					if (xtab[i].op) logmask &= xtab[i].mask;
					else logmask |= xtab[i].mask;
					break;
				}
			}

			if (i >= HAK_COUNTOF(xtab))
			{
				fprintf(stderr, "ERROR: unrecognized value  - [%.*s] - [%s]\n", (int)tlen, flt, logstr);
				return -1;
			}
		}
		while (cm);


		if (!(logmask & HAK_LOG_ALL_TYPES)) logmask |= HAK_LOG_ALL_TYPES;  /* no types specified. force to all types */
		if (!(logmask & HAK_LOG_ALL_LEVELS)) logmask |= HAK_LOG_ALL_LEVELS;  /* no levels specified. force to all levels */
	}
	else
	{
		logmask = HAK_LOG_ALL_LEVELS | HAK_LOG_ALL_TYPES;
		fname.len = hak_count_bcstr(logstr);
	}

	fname.ptr = (hak_bch_t*)logstr;
	hak_setoption (hak, HAK_LOG_TARGET_BCS, &fname);
	hak_setoption (hak, HAK_LOG_MASK, &logmask);
	return 0;
}

#if defined(HAK_BUILD_DEBUG)
static int handle_dbgopt (hak_t* hak, const hak_bch_t* str)
{
	/*xtn_t* xtn = (xtn_t*)hak_getxtn(hak);*/
	const hak_bch_t* cm, * flt;
	hak_oow_t len;
	hak_bitmask_t trait, dbgopt = 0;

	cm = str - 1;
	do
	{
		flt = cm + 1;

		cm = hak_find_bchar_in_bcstr(flt, ',');
		len = cm? (cm - flt): hak_count_bcstr(flt);
		if (len == 0) continue;
		else if (hak_comp_bchars_bcstr(flt, len, "gc") == 0) dbgopt |= HAK_TRAIT_DEBUG_GC;
		else if (hak_comp_bchars_bcstr(flt, len, "bigint") == 0) dbgopt |= HAK_TRAIT_DEBUG_BIGINT;
		else
		{
			fprintf(stderr, "ERROR: unknown debug option value - %.*s\n", (int)len, flt);
			return -1;
		}
	}
	while (cm);

	hak_getoption (hak, HAK_TRAIT, &trait);
	trait |= dbgopt;
	hak_setoption (hak, HAK_TRAIT, &trait);
	return 0;
}
#endif

/* ========================================================================= */

#if defined(_WIN32) || defined(__DOS__) || defined(__OS2__) || defined(macintosh)
typedef void(*signal_handler_t)(int);
#elif defined(macintosh)
typedef void(*signal_handler_t)(int); /* TODO: */
#elif defined(SA_SIGINFO)
typedef void(*signal_handler_t)(int, siginfo_t*, void*);
#else
typedef void(*signal_handler_t)(int);
#endif


#if defined(_WIN32) || defined(__DOS__) || defined(__OS2__)
static void handle_sigint (int sig)
{
	if (g_hak) hak_abort (g_hak);
}
#elif defined(macintosh)
/* TODO */
#elif defined(SA_SIGINFO)
static void handle_sigint (int sig, siginfo_t* siginfo, void* ctx)
{
	if (g_hak) hak_abort (g_hak);
}
#else
static void handle_sigint (int sig)
{
	if (g_hak) hak_abort (g_hak);
}
#endif

static void set_signal (int sig, signal_handler_t handler)
{
#if defined(_WIN32) || defined(__DOS__) || defined(__OS2__)
	signal (sig, handler);
#elif defined(macintosh)
	/* TODO: implement this */
#else
	struct sigaction sa;

	memset (&sa, 0, sizeof(sa));
	/*sa.sa_handler = handler;*/
#if defined(SA_SIGINFO)
	sa.sa_flags = SA_SIGINFO;
	sa.sa_sigaction = handler;
#else
	sa.sa_handler = handler;
#endif
	sigemptyset (&sa.sa_mask);

	sigaction (sig, &sa, NULL);
#endif
}

static void set_signal_to_default (int sig)
{
#if defined(_WIN32) || defined(__DOS__) || defined(__OS2__)
	signal (sig, SIG_DFL);
#elif defined(macintosh)
	/* TODO: implement this */
#else
	struct sigaction sa;

	memset (&sa, 0, sizeof(sa));
	sa.sa_handler = SIG_DFL;
	sa.sa_flags = 0;
	sigemptyset (&sa.sa_mask);

	sigaction (sig, &sa, NULL);
#endif
}

/* ========================================================================= */

static void print_info (void)
{
#if defined(HAK_CONFIGURE_CMD) && defined(HAK_CONFIGURE_ARGS)
	printf ("Configured with: %s %s\n", HAK_CONFIGURE_CMD, HAK_CONFIGURE_ARGS);
#elif defined(_WIN32)
	printf("Built for windows\n");
#else
	/* TODO: improve this part */
#endif
}

static void print_synerr (hak_t* hak)
{
	hak_synerr_t synerr;
	xtn_t* xtn;

	xtn = (xtn_t*)hak_getxtn(hak);
	hak_getsynerr (hak, &synerr);

	hak_logbfmt(hak,HAK_LOG_STDERR, "ERROR: ");
	if (synerr.loc.file)
		hak_logbfmt(hak, HAK_LOG_STDERR, "%js", synerr.loc.file);
	else
		hak_logbfmt(hak, HAK_LOG_STDERR, "%hs", xtn->cci_path);

	hak_logbfmt(hak, HAK_LOG_STDERR, "[%zu,%zu] %js",
		synerr.loc.line, synerr.loc.colm,
		(hak_geterrmsg(hak) != hak_geterrstr(hak)? hak_geterrmsg(hak): hak_geterrstr(hak))
	);

	if (synerr.tgt.len > 0)
		hak_logbfmt(hak, HAK_LOG_STDERR, " - %.*js", synerr.tgt.len, synerr.tgt.val);

	hak_logbfmt(hak, HAK_LOG_STDERR, "\n");
}

static void print_other_error (hak_t* hak)
{
	xtn_t* xtn;
	hak_loc_t loc;

	xtn = (xtn_t*)hak_getxtn(hak);
	hak_geterrloc(hak, &loc);

	hak_logbfmt(hak,HAK_LOG_STDERR, "ERROR: ");
	if (loc.file)
		hak_logbfmt(hak, HAK_LOG_STDERR, "%js", loc.file);
	else
		hak_logbfmt(hak, HAK_LOG_STDERR, "%hs", xtn->cci_path);

	hak_logbfmt(hak, HAK_LOG_STDERR, "[%zu,%zu] %js", loc.line, loc.colm, hak_geterrmsg(hak));

	hak_logbfmt(hak, HAK_LOG_STDERR, "\n");
}

static void print_error (hak_t* hak, const hak_bch_t* msghdr)
{
	if (HAK_ERRNUM(hak) == HAK_ESYNERR) print_synerr (hak);
	else print_other_error (hak);
	/*else hak_logbfmt(hak, HAK_LOG_STDERR, "ERROR: %hs - [%d] %js\n", msghdr, hak_geterrnum(hak), hak_geterrmsg(hak));*/
}


#if defined(USE_ISOCLINE)
static void print_incomplete_expression_error (hak_t* hak)
{
	/* isocline is supposed to return a full expression.
	 * if something is pending in the feed side, the input isn't complete yet */
	xtn_t* xtn;
	hak_loc_t loc;

	xtn = hak_getxtn(hak);
	hak_getfeedloc (hak, &loc);

	hak_logbfmt(hak, HAK_LOG_STDERR, "ERROR: ");
	if (loc.file)
		hak_logbfmt(hak, HAK_LOG_STDERR, "%js", loc.file);
	else
		hak_logbfmt(hak, HAK_LOG_STDERR, "%hs", xtn->cci_path);

	/* if the input is like this
	 *   a := 2; c := {
	 * the second expression is incompelete. however, the whole input is not executed.
	 * the number of compiled expressions so far is in xtn->feed.ncompexprs, however */
	hak_logbfmt(hak, HAK_LOG_STDERR, "[%zu,%zu] incomplete expression\n", loc.line, loc.colm);
}
#endif

static void show_prompt (hak_t* hak, int level)
{
/* TODO: different prompt per level */
	hak_resetfeedloc (hak); /* restore the line number to 1 in the interactive mode */
#if !defined(USE_ISOCLINE)
	hak_logbfmt(hak, HAK_LOG_STDOUT, "HAK> ");
	hak_logbfmt(hak, HAK_LOG_STDOUT, HAK_NULL); /* flushing */
#endif
}

static hak_oop_t execute_in_interactive_mode (hak_t* hak)
{
	hak_oop_t retv;

	hak_decode (hak, hak_getcode(hak), 0, hak_getbclen(hak));
	HAK_LOG0 (hak, HAK_LOG_MNEMONIC, "------------------------------------------\n");
	g_hak = hak;
	/*setup_tick ();*/

	retv = hak_execute(hak);

	/* flush pending output data in the interactive mode(e.g. printf without a newline) */
	hak_flushudio (hak);

	if (!retv)
	{
		print_error (hak, "execute");
	}
	else
	{
		/* print the result in the interactive mode regardless 'verbose' */
		hak_logbfmt(hak, HAK_LOG_STDOUT, "%O\n", retv); /* TODO: show this go to the output handler?? */
		/*
		 * print the value of ERRSTR.
		hak_oop_cons_t cons = hak_getatsysdic(hak, xtn->sym_errstr);
		if (cons)
		{
			HAK_ASSERT(hak, HAK_IS_CONS(hak, cons));
			HAK_ASSERT(hak, HAK_CONS_CAR(cons) == xtn->sym_errstr);
			hak_print (hak, HAK_CONS_CDR(cons));
		}
		*/
	}
	/*cancel_tick();*/
	g_hak = HAK_NULL;

	return retv;
}

static hak_oop_t execute_in_batch_mode(hak_t* hak, int verbose)
{
	hak_oop_t retv;

	hak_decode(hak, hak_getcode(hak), 0, hak_getbclen(hak));
	HAK_LOG3(hak, HAK_LOG_MNEMONIC, "BYTECODES bclen=%zu lflen=%zu ngtmprs=%zu\n", hak_getbclen(hak), hak_getlflen(hak), hak_getngtmprs(hak));
	g_hak = hak;
	/*setup_tick ();*/


/* TESTING */
#if 0
{
	hak_code_t xcode;
	hak_ptlc_t mem;

	memset (&xcode, 0, HAK_SIZEOF(xcode));
	memset (&mem, 0, HAK_SIZEOF(mem));

	hak_marshalcodetomem(hak, &hak->code, &mem);
	hak_unmarshalcodefrommem(hak, &xcode, (const hak_ptl_t*)&mem);
	hak_freemem(hak, mem.ptr);

	hak_decode(hak, &xcode, 0, xcode.bc.len);
	hak_purgecode (hak, &xcode);
}
#endif
/* END TESTING */

	retv = hak_execute(hak);
	hak_flushudio (hak);

	if (!retv) print_error (hak, "execute");
	else if (verbose) hak_logbfmt(hak, HAK_LOG_STDERR, "EXECUTION OK - EXITED WITH %O\n", retv);

	/*cancel_tick();*/
	g_hak = HAK_NULL;
	/*hak_dumpsymtab (hak);*/

	return retv;
}

static int on_fed_cnode_in_interactive_mode (hak_t* hak, hak_cnode_t* obj)
{
	xtn_t* xtn = (xtn_t*)hak_getxtn(hak);
	int flags = 0;

	/* in the interactive, the compile error must not break the input loop.
	 * this function returns 0 to go on despite a compile-time error.
	 *
	 * if a single line or continued lines contain multiple expressions,
	 * execution is delayed until the last expression is compiled. */

	if (xtn->feed.ncompexprs <= 0)
	{
		/* the first expression in the current user input line.
		 * arrange to clear byte-codes before compiling the expression. */
		flags = HAK_COMPILE_CLEAR_CODE | HAK_COMPILE_CLEAR_FUNBLK;
	}

	if (hak_compile(hak, obj, flags) <= -1)
	{
		/*print_error(hak, "compile"); */
		xtn->feed.pos = xtn->feed.len; /* arrange to discard the rest of the line */
		return -1; /* this causes the feed function to fail and
		              the error hander for to print the error message */
	}

	xtn->feed.ncompexprs++;
	return 0;
}

static int on_fed_cnode_in_batch_mode (hak_t* hak, hak_cnode_t* obj)
{
	/*xtn_t* xtn = (xtn_t*)hak_getxtn(hak);*/
	return hak_compile(hak, obj, 0);
}

#if defined(USE_ISOCLINE)
static int get_line (hak_t* hak, xtn_t* xtn, FILE* fp)
{
	char* inp, * p;
	static int inited = 0;

	if (!inited)
	{
		ic_style_def("kbd","gray underline");     // you can define your own styles
		ic_style_def("ic-prompt","ansi-maroon");  // or re-define system styles
		ic_set_history (HAK_NULL, -1);
		ic_enable_multiline (1);
		ic_enable_multiline_indent (1);
		ic_set_matching_braces ("()[]{}");
		ic_enable_brace_insertion (1);
		ic_set_insertion_braces("()[]{}\"\"''");
		inited = 1;
	}

	if (xtn->feed.eof) return 0;

	xtn->feed.pos = 0;
	xtn->feed.len = 0;
	if (xtn->feed.ptr)
	{
		HAK_ASSERT(hak, xtn->feed.ptr != xtn->feed.buf);
		ic_free (xtn->feed.ptr);
		xtn->feed.ptr = HAK_NULL;
	}

	inp = ic_readline("HAK");
	if (inp == NULL)
	{
		/* TODO: check if it's an error or Eof */
		xtn->feed.eof = 1;
		HAK_ASSERT(hak, xtn->feed.pos == 0);
		HAK_ASSERT(hak, xtn->feed.len == 0);
		return 0;
	}

	xtn->feed.len = hak_count_bcstr(inp);
	xtn->feed.ptr = inp;
	return 1;
}
#else
static int get_line (hak_t* hak, xtn_t* xtn, FILE* fp)
{
	if (xtn->feed.eof) return 0;

	xtn->feed.pos = 0;
	xtn->feed.len = 0;
	xtn->feed.ptr = xtn->feed.buf; /* use the internal buffer */

	while (1)
	{
		int ch = fgetc(fp);
		if (ch == EOF)
		{
			if (ferror(fp))
			{
				hak_logbfmt(hak, HAK_LOG_STDERR, "ERROR: failed to read - %hs - %hs\n", xtn->cci_path, strerror(errno));
				return -1;
			}

			xtn->feed.eof = 1;
			if (xtn->feed.len <= 0) return 0;

			break;
		}

		xtn->feed.buf[xtn->feed.len++] = (hak_bch_t)(unsigned int)ch;
		if (ch == '\n' || xtn->feed.len >= HAK_COUNTOF(xtn->feed.buf)) break;
	}

	return 1;
}
#endif

static int feed_loop (hak_t* hak, xtn_t* xtn, int verbose)
{
	FILE* fp = HAK_NULL;
	int is_tty;

#if defined(_WIN32) && defined(__STDC_WANT_SECURE_LIB__)
	errno_t err = fopen_s(&fp, xtn->cci_path, FOPEN_R_FLAGS);
	if (err != 0)
	{
		hak_logbfmt(hak, HAK_LOG_STDERR, "ERROR: failed to open - %hs - %hs\n", xtn->cci_path, strerror(err));
		goto oops;
	}
#else
	fp = fopen(xtn->cci_path, FOPEN_R_FLAGS);
	if (!fp)
	{
		hak_logbfmt(hak, HAK_LOG_STDERR, "ERROR: failed to open - %hs - %hs\n", xtn->cci_path, strerror(errno));
		goto oops;
	}
#endif

#if defined(_WIN32)
	is_tty = _isatty(_fileno(fp));
#else
	is_tty = isatty(fileno(fp));
#endif

	/* override the default cnode handler. the default one simply
	 * compiles the expression node without execution */
	/*if (hak_beginfeed(hak, is_tty? on_fed_cnode_in_interactive_mode: HAK_NULL) <= -1)*/
	if (hak_beginfeed(hak, is_tty? on_fed_cnode_in_interactive_mode: on_fed_cnode_in_batch_mode) <= -1)
	{
		hak_logbfmt(hak, HAK_LOG_STDERR, "ERROR: cannot begin feed - [%d] %js\n", hak_geterrnum(hak), hak_geterrmsg(hak));
		goto oops;
	}

	if (is_tty)
	{
		/* interactive mode */
		show_prompt (hak, 0);

		while (1)
		{
			int n;
			hak_oow_t pos;
			hak_oow_t len;

		#if defined(USE_ISOCLINE)
			int lf_injected = 0;
		#endif

			/* read a line regardless of the actual expression */
			n = get_line(hak, xtn, fp);
			if (n <= -1) goto oops;
			if (n == 0) break;

			/* feed the line */
			pos = xtn->feed.pos;
			/* update xtn->feed.pos before calling hak_feedbchars() so that the callback sees the updated value */
			xtn->feed.pos = xtn->feed.len;
			len = xtn->feed.len - pos;
			n = hak_feedbchars(hak, &xtn->feed.ptr[pos], len);
		#if defined(USE_ISOCLINE)
		chars_fed:
		#endif
			if (n <= -1)
			{
				print_error (hak, "feed"); /* syntax error or something - mostly compile error */

		#if defined(USE_ISOCLINE)
			reset_on_feed_error:
		#endif
				hak_resetfeed (hak);
				hak_clearcode (hak); /* clear the compiled code but not executed yet in advance */
				xtn->feed.ncompexprs = 0; /* next time, on_fed_cnode_in_interactive_mode() clears code and fnblks */
				/*if (len > 0)*/ show_prompt (hak, 0); /* show prompt after error */
			}
			else
			{
				if (!hak_feedpending(hak))
				{
					if (xtn->feed.ncompexprs > 0)
					{
						if (hak_getbclen(hak) > 0) execute_in_interactive_mode (hak);
						xtn->feed.ncompexprs = 0;
					}
					else
					{
						HAK_ASSERT(hak, hak_getbclen(hak) == 0);
						/* usually this part is reached if the input string is
						 * one or more whilespaces and/or comments only */
					}
					show_prompt (hak, 0); /* show prompt after execution */
				}
		#if defined(USE_ISOCLINE)
				else if (!lf_injected)
				{
					/* in this mode, one input string must be composed of one or more
					 * complete expression. however, it doesn't isocline doesn't include
					 * the ending line-feed in the returned input string. inject one to the feed */
					static const char lf = '\n';
					lf_injected = 1;
					n = hak_feedbchars(hak, &lf, 1);
					goto chars_fed;
				}
				else
				{
					print_incomplete_expression_error (hak);
					goto reset_on_feed_error;
				}
		#endif
			}
		}

	#if !defined(USE_ISOCLINE)
		/* eof is given, usually with ctrl-D, no new line is output after the prompt.
		 * this results in the OS prompt on the same line as this program's prompt.
		 * however ISOCLINE prints a newline upon ctrl-D. print \n when ISOCLINE is
		 * not used */
		hak_logbfmt(hak, HAK_LOG_STDOUT, "\n");
	#endif
	}
	else
	{
		/* non-interactive mode */
		while (1)
		{
			hak_bch_t buf[1024];
			hak_oow_t xlen;

			xlen = fread(buf, HAK_SIZEOF(buf[0]), HAK_COUNTOF(buf), fp);
			if (xlen > 0 && hak_feedbchars(hak, buf, xlen) <= -1) goto endfeed_error;
			if (xlen < HAK_COUNTOF(buf))
			{
				if (ferror(fp))
				{
					hak_logbfmt(hak, HAK_LOG_STDERR, "ERROR: failed to read - %hs - %hs\n", xtn->cci_path, strerror(errno));
					goto oops;
				}
				break;
			}
		}
	}

	if (hak_endfeed(hak) <= -1)
	{
	endfeed_error:
		print_error (hak, "endfeed");
		goto oops; /* TODO: proceed or just exit? */
	}
	fclose (fp);

	if (!is_tty && hak_getbclen(hak) > 0) execute_in_batch_mode (hak, verbose);
	return 0;

oops:
	if (fp) fclose (fp);
	return -1;
}

/* #define DEFAULT_HEAPSIZE (512000ul) */
#define DEFAULT_HEAPSIZE (0ul) /* don't use the pre-allocated heap */

int main (int argc, char* argv[])
{
	hak_t* hak = HAK_NULL;
	xtn_t* xtn;
	hak_cb_t hakcb;

	hak_bci_t c;
	static hak_bopt_lng_t lopt[] =
	{
#if defined(HAK_BUILD_DEBUG)
		{ ":debug",       '\0' },
#endif
		{ ":heapsize",    '\0' },
		{ ":log",         'l'  },
		{ "info",         '\0' },
		{ ":modlibdirs",  '\0' },

		{ HAK_NULL,       '\0' }
	};
	static hak_bopt_t opt =
	{
		"l:v",
		lopt
	};

	const char* logopt = HAK_NULL;
	hak_oow_t heapsize = DEFAULT_HEAPSIZE;
	int verbose = 0;
	int show_info = 0;
	const char* modlibdirs = HAK_NULL;

#if defined(HAK_BUILD_DEBUG)
	const char* dbgopt = HAK_NULL;
#endif

	setlocale(LC_ALL, "");

#if !defined(macintosh)
	if (argc < 2)
	{
	print_usage:
		fprintf(stderr, "Usage: %s [options] script-filename [output-filename]\n", argv[0]);
		fprintf(stderr, "Options are:\n");
		fprintf(stderr, " -v  show verbose messages\n");
		return -1;
	}

	while ((c = hak_getbopt(argc, argv, &opt)) != HAK_BCI_EOF)
	{
		switch (c)
		{
			case 'l':
				logopt = opt.arg;
				break;

			case 'v':
				verbose = 1;
				break;

			case '\0':
				if (hak_comp_bcstr(opt.lngopt, "heapsize") == 0)
				{
					heapsize = strtoul(opt.arg, HAK_NULL, 0);
					break;
				}
			#if defined(HAK_BUILD_DEBUG)
				else if (hak_comp_bcstr(opt.lngopt, "debug") == 0)
				{
					dbgopt = opt.arg;
					break;
				}
			#endif
				else if (hak_comp_bcstr(opt.lngopt, "info") == 0)
				{
					show_info = 1;
					break;
				}
				else if (hak_comp_bcstr(opt.lngopt, "modlibdirs") == 0)
				{
					modlibdirs = opt.arg;
					break;
				}

				goto print_usage;

			case ':':
				if (opt.lngopt)
					fprintf(stderr, "bad argument for '%s'\n", opt.lngopt);
				else
					fprintf(stderr, "bad argument for '%c'\n", opt.opt);

				return -1;

			default:
				goto print_usage;
		}
	}

	if ((opt.ind + 1) != argc && (opt.ind + 2) != argc && !show_info) goto print_usage;
#endif

	hak = hak_openstd(HAK_SIZEOF(xtn_t), HAK_NULL);
	if (HAK_UNLIKELY(!hak))
	{
		printf ("ERROR: cannot open hak\n");
		goto oops;
	}

	xtn = (xtn_t*)hak_getxtn(hak);

	{
		hak_oow_t tab_size;
		tab_size = 5000;
		hak_setoption (hak, HAK_SYMTAB_SIZE, &tab_size);
		tab_size = 5000;
		hak_setoption (hak, HAK_SYSDIC_SIZE, &tab_size);
		tab_size = 600; /* TODO: choose a better stack size or make this user specifiable */
		hak_setoption (hak, HAK_PROCSTK_SIZE, &tab_size);
	}

	{
		hak_bitmask_t trait = 0;

		/*trait |= HAK_TRAIT_NOGC;*/
		trait |= HAK_TRAIT_AWAIT_PROCS;
		trait |= HAK_TRAIT_LANG_ENABLE_EOL;
		hak_setoption (hak, HAK_TRAIT, &trait);
	}

	if (modlibdirs)
	{
	#if defined(HAK_OOCH_IS_UCH)
		hak_ooch_t* tmp;
		tmp = hak_dupbtoucstr(hak, modlibdirs, HAK_NULL);
		if (HAK_UNLIKELY(!tmp))
		{
			hak_logbfmt(hak, HAK_LOG_STDERR,"ERROR: cannot duplicate modlibdirs - [%d] %js\n", hak_geterrnum(hak), hak_geterrmsg(hak));
			goto oops;
		}

		if (hak_setoption(hak, HAK_MOD_LIBDIRS, tmp) <= -1)
		{
			hak_logbfmt(hak, HAK_LOG_STDERR,"ERROR: cannot set modlibdirs - [%d] %js\n", hak_geterrnum(hak), hak_geterrmsg(hak));
			hak_freemem(hak, tmp);
			goto oops;
		}
		hak_freemem(hak, tmp);
	#else
		if (hak_setoption(hak, HAK_MOD_LIBDIRS, modlibdirs) <= -1)
		{
			hak_logbfmt(hak, HAK_LOG_STDERR,"ERROR: cannot set modlibdirs - [%d] %js\n", hak_geterrnum(hak), hak_geterrmsg(hak));
			goto oops;
		}
	#endif
	}

	memset (&hakcb, 0, HAK_SIZEOF(hakcb));
	hakcb.on_gc = on_gc_hak;
	hakcb.vm_startup = vm_startup;
	hakcb.vm_cleanup = vm_cleanup;
	/*hakcb.vm_checkbc = vm_checkbc;*/
	hak_regcb (hak, &hakcb);

	if (logopt && handle_logopt(hak, logopt) <= -1) goto oops;

#if defined(HAK_BUILD_DEBUG)
	if (dbgopt && handle_dbgopt(hak, dbgopt) <= -1) goto oops;
#endif

	if (show_info)
	{
		print_info ();
		return 0;
	}

	if (hak_ignite(hak, heapsize) <= -1)
	{
		hak_logbfmt(hak, HAK_LOG_STDERR, "cannot ignite hak - [%d] %js\n", hak_geterrnum(hak), hak_geterrmsg(hak));
		goto oops;
	}

	if (hak_addbuiltinprims(hak) <= -1)
	{
		hak_logbfmt(hak, HAK_LOG_STDERR, "cannot add builtin primitives - [%d] %js\n", hak_geterrnum(hak), hak_geterrmsg(hak));
		goto oops;
	}

	xtn->cci_path = argv[opt.ind++]; /* input source code file */
	if (opt.ind < argc) xtn->udo_path = argv[opt.ind++];

	if (hak_attachcciostdwithbcstr(hak, xtn->cci_path) <= -1)
	{
		hak_logbfmt(hak, HAK_LOG_STDERR, "ERROR: cannot attach source input stream - [%d] %js\n", hak_geterrnum(hak), hak_geterrmsg(hak));
		goto oops;
	}

	if (hak_attachudiostdwithbcstr(hak, "", xtn->udo_path) <= -1) /* TODO: add udi path */
	{
		hak_logbfmt(hak, HAK_LOG_STDERR, "ERROR: cannot attach user data streams - [%d] %js\n", hak_geterrnum(hak), hak_geterrmsg(hak));
		goto oops;
	}

	/* -- from this point onward, any failure leads to jumping to the oops label
	 * -- instead of returning -1 immediately. --*/
	set_signal(SIGINT, handle_sigint);

#if 0
// TODO: change the option name
// in the INTERACTIVE mode, the compiler generates MAKE_FUNCTION for lambda functions.
// in the non-INTERACTIVE mode, the compiler generates MAKE_BLOCK for lambda functions.
{
	hak_bitmask_t trait;
	hak_getoption (hak, HAK_TRAIT, &trait);
	trait |= HAK_TRAIT_INTERACTIVE;
	hak_setoption (hak, HAK_TRAIT, &trait);
}
#endif

	if (feed_loop(hak, xtn, verbose) <= -1) goto oops;

	set_signal_to_default(SIGINT);
	hak_close(hak);

	return 0;

oops:
	set_signal_to_default(SIGINT); /* harmless to call multiple times without set_signal() */
	if (hak) hak_close(hak);
	return -1;
}
