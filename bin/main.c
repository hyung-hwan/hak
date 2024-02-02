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

#include <hcl.h>
#include <hcl-utl.h>
#include <hcl-opt.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
#include <locale.h>

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

typedef struct bb_t bb_t;
struct bb_t
{
	char buf[1024];
	hcl_oow_t pos;
	hcl_oow_t len;

	FILE* fp;
	hcl_bch_t* fn;
};

typedef struct xtn_t xtn_t;
struct xtn_t
{
	const char* cci_path; /* main source file */
	/*const char* udi_path; */ /* not implemented as of snow */
	const char* udo_path;

	int vm_running;
	/*hcl_oop_t sym_errstr;*/
};

/* ========================================================================= */

static hcl_t* g_hcl = HCL_NULL;

/* ========================================================================= */

static int vm_startup (hcl_t* hcl)
{
	xtn_t* xtn = (xtn_t*)hcl_getxtn(hcl);
	xtn->vm_running = 1;
	return 0;
}

static void vm_cleanup (hcl_t* hcl)
{
	xtn_t* xtn = (xtn_t*)hcl_getxtn(hcl);
	xtn->vm_running = 0;
}

/*
static void vm_checkbc (hcl_t* hcl, hcl_oob_t bcode)
{
}
*/

static void gc_hcl (hcl_t* hcl)
{
	/*xtn_t* xtn = (xtn_t*)hcl_getxtn(hcl);*/
	/*if (xtn->sym_errstr) xtn->sym_errstr = hcl_moveoop(hcl, xtn->sym_errstr);*/
}

/* ========================================================================= */

static int handle_logopt (hcl_t* hcl, const hcl_bch_t* logstr)
{
	const hcl_bch_t* cm, * flt;
	hcl_bitmask_t logmask;
	hcl_oow_t tlen, i;
	hcl_bcs_t fname;

	static struct
	{
		const char* name;
		int op; /* 0: bitwise-OR, 1: bitwise-AND */
		hcl_bitmask_t mask;
	} xtab[] =
	{
		{ "",           0, 0 },

		{ "app",        0, HCL_LOG_APP },
		{ "compiler",   0, HCL_LOG_COMPILER },
		{ "vm",         0, HCL_LOG_VM },
		{ "mnemonic",   0, HCL_LOG_MNEMONIC },
		{ "gc",         0, HCL_LOG_GC },
		{ "ic",         0, HCL_LOG_IC },
		{ "primitive",  0, HCL_LOG_PRIMITIVE },

		/* select a specific level */
		{ "fatal",      0, HCL_LOG_FATAL },
		{ "error",      0, HCL_LOG_ERROR },
		{ "warn",       0, HCL_LOG_WARN },
		{ "info",       0, HCL_LOG_INFO },
		{ "debug",      0, HCL_LOG_DEBUG },

		/* select a specific level or higher */
		{ "fatal+",     0, HCL_LOG_FATAL },
		{ "error+",     0, HCL_LOG_FATAL | HCL_LOG_ERROR },
		{ "warn+",      0, HCL_LOG_FATAL | HCL_LOG_ERROR | HCL_LOG_WARN },
		{ "info+",      0, HCL_LOG_FATAL | HCL_LOG_ERROR | HCL_LOG_WARN | HCL_LOG_INFO },
		{ "debug+",     0, HCL_LOG_FATAL | HCL_LOG_ERROR | HCL_LOG_WARN | HCL_LOG_INFO | HCL_LOG_DEBUG },

		/* select a specific level or lower */
		{ "fatal-",     0, HCL_LOG_FATAL | HCL_LOG_ERROR | HCL_LOG_WARN | HCL_LOG_INFO | HCL_LOG_DEBUG },
		{ "error-",     0, HCL_LOG_ERROR | HCL_LOG_WARN | HCL_LOG_INFO | HCL_LOG_DEBUG },
		{ "warn-",      0, HCL_LOG_WARN | HCL_LOG_INFO | HCL_LOG_DEBUG },
		{ "info-",      0, HCL_LOG_INFO | HCL_LOG_DEBUG },
		{ "debug-",     0, HCL_LOG_DEBUG },

		/* exclude a specific level */
		{ "-fatal",     1, ~HCL_LOG_FATAL },
		{ "-error",     1, ~HCL_LOG_ERROR },
		{ "-warn",      1, ~HCL_LOG_WARN },
		{ "-info",      1, ~HCL_LOG_INFO },
		{ "-debug",     1, ~HCL_LOG_DEBUG },
	};

	cm = hcl_find_bchar_in_bcstr(logstr, ',');
	if (cm)
	{
		fname.len = cm - logstr;
		logmask = 0;

		do
		{
			flt = cm + 1;

			cm = hcl_find_bchar_in_bcstr(flt, ',');
			tlen = (cm)? (cm - flt): hcl_count_bcstr(flt);

			for (i = 0; i < HCL_COUNTOF(xtab); i++)
			{
				if (hcl_comp_bchars_bcstr(flt, tlen, xtab[i].name) == 0)
				{
					if (xtab[i].op) logmask &= xtab[i].mask;
					else logmask |= xtab[i].mask;
					break;
				}
			}

			if (i >= HCL_COUNTOF(xtab))
			{
				fprintf (stderr, "ERROR: unrecognized value  - [%.*s] - [%s]\n", (int)tlen, flt, logstr);
				return -1;
			}
		}
		while (cm);


		if (!(logmask & HCL_LOG_ALL_TYPES)) logmask |= HCL_LOG_ALL_TYPES;  /* no types specified. force to all types */
		if (!(logmask & HCL_LOG_ALL_LEVELS)) logmask |= HCL_LOG_ALL_LEVELS;  /* no levels specified. force to all levels */
	}
	else
	{
		logmask = HCL_LOG_ALL_LEVELS | HCL_LOG_ALL_TYPES;
		fname.len = hcl_count_bcstr(logstr);
	}

	fname.ptr = (hcl_bch_t*)logstr;
	hcl_setoption (hcl, HCL_LOG_TARGET_BCS, &fname);
	hcl_setoption (hcl, HCL_LOG_MASK, &logmask);
	return 0;
}

#if defined(HCL_BUILD_DEBUG)
static int handle_dbgopt (hcl_t* hcl, const hcl_bch_t* str)
{
	/*xtn_t* xtn = (xtn_t*)hcl_getxtn(hcl);*/
	const hcl_bch_t* cm, * flt;
	hcl_oow_t len;
	hcl_bitmask_t trait, dbgopt = 0;

	cm = str - 1;
	do
	{
		flt = cm + 1;

		cm = hcl_find_bchar_in_bcstr(flt, ',');
		len = cm? (cm - flt): hcl_count_bcstr(flt);
		if (hcl_comp_bchars_bcstr(flt, len, "gc") == 0)  dbgopt |= HCL_TRAIT_DEBUG_GC;
		else if (hcl_comp_bchars_bcstr(flt, len, "bigint") == 0)  dbgopt |= HCL_TRAIT_DEBUG_BIGINT;
		else
		{
			fprintf (stderr, "ERROR: unknown debug option value - %.*s\n", (int)len, flt);
			return -1;
		}
	}
	while (cm);

	hcl_getoption (hcl, HCL_TRAIT, &trait);
	trait |= dbgopt;
	hcl_setoption (hcl, HCL_TRAIT, &trait);
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
	if (g_hcl) hcl_abort (g_hcl);
}
#elif defined(macintosh)
/* TODO */
#elif defined(SA_SIGINFO)
static void handle_sigint (int sig, siginfo_t* siginfo, void* ctx)
{
	if (g_hcl) hcl_abort (g_hcl);
}
#else
static void handle_sigint (int sig)
{
	if (g_hcl) hcl_abort (g_hcl);
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
#if defined(HCL_CONFIGURE_CMD) && defined(HCL_CONFIGURE_ARGS)
	printf ("Configured with: %s %s\n", HCL_CONFIGURE_CMD, HCL_CONFIGURE_ARGS);
#elif defined(_WIN32)
	printf("Built for windows\n");
#else
	/* TODO: improve this part */
#endif
}

static void print_synerr (hcl_t* hcl)
{
	hcl_synerr_t synerr;
	xtn_t* xtn;

	xtn = (xtn_t*)hcl_getxtn (hcl);
	hcl_getsynerr (hcl, &synerr);

	hcl_logbfmt (hcl,HCL_LOG_STDERR, "ERROR: ");
	if (synerr.loc.file)
	{
		hcl_logbfmt (hcl, HCL_LOG_STDERR, "%js", synerr.loc.file);
	}
	else
	{
		hcl_logbfmt (hcl, HCL_LOG_STDERR, "%hs", xtn->cci_path);
	}

	hcl_logbfmt (hcl, HCL_LOG_STDERR, "[%zu,%zu] %js",
		synerr.loc.line, synerr.loc.colm,
		(hcl_geterrmsg(hcl) != hcl_geterrstr(hcl)? hcl_geterrmsg(hcl): hcl_geterrstr(hcl))
	);

	if (synerr.tgt.len > 0)
	{
		hcl_logbfmt (hcl, HCL_LOG_STDERR, " - %.*js", synerr.tgt.len, synerr.tgt.val);
	}

	hcl_logbfmt (hcl, HCL_LOG_STDERR, "\n");
}

static void show_prompt (hcl_t* hcl, int level)
{
/* TODO: different prompt per level */
	hcl_logbfmt (hcl, HCL_LOG_STDOUT, "HCL> ");
	hcl_logbfmt (hcl, HCL_LOG_STDOUT, HCL_NULL); /* flushing */
}

static hcl_oop_t execute_in_interactive_mode (hcl_t* hcl)
{
	hcl_oop_t retv;

	hcl_decode (hcl, hcl_getcode(hcl), 0, hcl_getbclen(hcl));
	HCL_LOG0 (hcl, HCL_LOG_MNEMONIC, "------------------------------------------\n");
	g_hcl = hcl;
	/*setup_tick ();*/

	retv = hcl_execute(hcl);

	/* flush pending output data in the interactive mode(e.g. printf without a newline) */
	hcl_flushudio (hcl);

	if (!retv)
	{
		hcl_logbfmt (hcl, HCL_LOG_STDERR, "ERROR: cannot execute - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
	}
	else
	{
		/* print the result in the interactive mode regardless 'verbose' */
		hcl_logbfmt (hcl, HCL_LOG_STDOUT, "%O\n", retv); /* TODO: show this go to the output handler?? */
		/*
		 * print the value of ERRSTR.
		hcl_oop_cons_t cons = hcl_getatsysdic(hcl, xtn->sym_errstr);
		if (cons)
		{
			HCL_ASSERT (hcl, HCL_IS_CONS(hcl, cons));
			HCL_ASSERT (hcl, HCL_CONS_CAR(cons) == xtn->sym_errstr);
			hcl_print (hcl, HCL_CONS_CDR(cons));
		}
		*/
	}
	/*cancel_tick();*/
	g_hcl = HCL_NULL;

	return retv;
}

static hcl_oop_t execute_in_batch_mode(hcl_t* hcl, int verbose)
{
	hcl_oop_t retv;

	hcl_decode(hcl, hcl_getcode(hcl), 0, hcl_getbclen(hcl));
	HCL_LOG3(hcl, HCL_LOG_MNEMONIC, "BYTECODES bclen=%zu lflen=%zu ngtmprs=%zu\n", hcl_getbclen(hcl), hcl_getlflen(hcl), hcl_getngtmprs(hcl));
	g_hcl = hcl;
	/*setup_tick ();*/


/* TESTING */
#if 0
{
	hcl_code_t xcode;
	hcl_ptlc_t mem;

	memset (&xcode, 0, HCL_SIZEOF(xcode));
	memset (&mem, 0, HCL_SIZEOF(mem));

	hcl_marshalcodetomem(hcl, &hcl->code, &mem);
	hcl_unmarshalcodefrommem(hcl, &xcode, (const hcl_ptl_t*)&mem);
	hcl_freemem (hcl, mem.ptr);

	hcl_decode(hcl, &xcode, 0, xcode.bc.len);
	hcl_purgecode (hcl, &xcode);
}
#endif
/* END TESTING */

	retv = hcl_execute(hcl);
	hcl_flushudio (hcl);

	if (!retv)
	{
		hcl_logbfmt (hcl, HCL_LOG_STDERR, "ERROR: cannot execute - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
	}
	else if (verbose)
	{
		hcl_logbfmt (hcl, HCL_LOG_STDERR, "EXECUTION OK - EXITED WITH %O\n", retv);
	}

	/*cancel_tick();*/
	g_hcl = HCL_NULL;
	/*hcl_dumpsymtab (hcl);*/

	return retv;
}

static int on_fed_cnode_in_interactive_mode (hcl_t* hcl, hcl_cnode_t* obj)
{
	xtn_t* xtn = (xtn_t*)hcl_getxtn(hcl);
	if (hcl_compile(hcl, obj, HCL_COMPILE_CLEAR_CODE | HCL_COMPILE_CLEAR_FNBLK) <= -1) return -1;
	execute_in_interactive_mode (hcl);

	show_prompt (hcl, 0);
	return 0;
}

static int on_fed_cnode_in_batch_mode (hcl_t* hcl, hcl_cnode_t* obj)
{
	xtn_t* xtn = (xtn_t*)hcl_getxtn(hcl);
	return hcl_compile(hcl, obj, 0);
}

static int feed_loop (hcl_t* hcl, xtn_t* xtn, int verbose)
{
	FILE* fp = HCL_NULL;
	int is_tty;

#if defined(_WIN32) && defined(__STDC_WANT_SECURE_LIB__)
	errno_t err = fopen_s(&fp, xtn->cci_path, FOPEN_R_FLAGS);
	if (err != 0)
	{
		hcl_logbfmt(hcl, HCL_LOG_STDERR, "ERROR: failed to open - %hs - %hs\n", xtn->cci_path, strerror(err));
		goto oops;
	}
#else
	fp = fopen(xtn->cci_path, FOPEN_R_FLAGS);
	if (!fp)
	{
		hcl_logbfmt (hcl, HCL_LOG_STDERR, "ERROR: failed to open - %hs - %hs\n", xtn->cci_path, strerror(errno));
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
	/*if (hcl_beginfeed(hcl, is_tty? on_fed_cnode_in_interactive_mode: HCL_NULL) <= -1)*/
	if (hcl_beginfeed(hcl, is_tty? on_fed_cnode_in_interactive_mode: on_fed_cnode_in_batch_mode) <= -1)
	{
		hcl_logbfmt (hcl, HCL_LOG_STDERR, "ERROR: cannot begin feed - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
		goto oops;
	}

	if (is_tty) show_prompt (hcl, 0);

	while (1)
	{
		if (is_tty)
		{
			hcl_bch_t bch;
			int ch = fgetc(fp);
			if (ch == EOF)
			{
				if (ferror(fp))
				{
					hcl_logbfmt (hcl, HCL_LOG_STDERR, "ERROR: failed to read - %hs - %hs\n", xtn->cci_path, strerror(errno));
					goto oops;
				}
				break;
			}

			bch = ch;
			if (hcl_feedbchars(hcl, &bch, 1) <= -1) goto feed_error;
		}
		else
		{
			hcl_bch_t buf[1024];
			hcl_oow_t xlen;

			xlen = fread(buf, HCL_SIZEOF(buf[0]), HCL_COUNTOF(buf), fp);
			if (xlen > 0 && hcl_feedbchars(hcl, buf, xlen) <= -1) goto feed_error;
			if (xlen < HCL_COUNTOF(buf))
			{
				if (ferror(fp))
				{
					hcl_logbfmt (hcl, HCL_LOG_STDERR, "ERROR: failed to read - %hs - %hs\n", xtn->cci_path, strerror(errno));
					goto oops;
				}
				break;
			}
		}
	}

	if (hcl_endfeed(hcl) <= -1)
	{
	feed_error:
		if (hcl->errnum == HCL_ESYNERR) print_synerr (hcl);
		else hcl_logbfmt (hcl, HCL_LOG_STDERR, "ERROR: cannot feed - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
		goto oops; /* TODO: proceed or just exit? */
	}
	fclose (fp);

	if (!is_tty && hcl_getbclen(hcl) > 0) execute_in_batch_mode (hcl, verbose);
	return 0;

oops:
	if (fp) fclose (fp);
	return -1;
}

/* #define DEFAULT_HEAPSIZE (512000ul) */
#define DEFAULT_HEAPSIZE (0ul) /* don't use the pre-allocated heap */

int main (int argc, char* argv[])
{
	hcl_t* hcl = HCL_NULL;
	xtn_t* xtn;
	hcl_cb_t hclcb;

	hcl_bci_t c;
	static hcl_bopt_lng_t lopt[] =
	{
#if defined(HCL_BUILD_DEBUG)
		{ ":debug",       '\0' },
#endif
		{ ":heapsize",    '\0' },
		{ ":log",         'l'  },
		{ "info",         '\0' },
		{ ":modlibdirs",  '\0' },

		{ HCL_NULL,       '\0' }
	};
	static hcl_bopt_t opt =
	{
		"l:bnv",
		lopt
	};

	const char* logopt = HCL_NULL;
	hcl_oow_t heapsize = DEFAULT_HEAPSIZE;
	int verbose = 0;
	int show_info = 0;
	int enable_block = 0;
	int nl_terminator = 0;
	const char* modlibdirs = HCL_NULL;

#if defined(HCL_BUILD_DEBUG)
	const char* dbgopt = HCL_NULL;
#endif

	setlocale (LC_ALL, "");

#if !defined(macintosh)
	if (argc < 2)
	{
	print_usage:
		fprintf (stderr, "Usage: %s filename ...\n", argv[0]);
		return -1;
	}

	while ((c = hcl_getbopt(argc, argv, &opt)) != HCL_BCI_EOF)
	{
		switch (c)
		{
			case 'l':
				logopt = opt.arg;
				break;

			case 'b':
				enable_block = 1;
				break;

			case 'n':
				nl_terminator = 1;
				break;

			case 'v':
				verbose = 1;
				break;

			case '\0':
				if (hcl_comp_bcstr(opt.lngopt, "heapsize") == 0)
				{
					heapsize = strtoul(opt.arg, HCL_NULL, 0);
					break;
				}
			#if defined(HCL_BUILD_DEBUG)
				else if (hcl_comp_bcstr(opt.lngopt, "debug") == 0)
				{
					dbgopt = opt.arg;
					break;
				}
			#endif
				else if (hcl_comp_bcstr(opt.lngopt, "info") == 0)
				{
					show_info = 1;
					break;
				}
				else if (hcl_comp_bcstr(opt.lngopt, "modlibdirs") == 0)
				{
					modlibdirs = opt.arg;
					break;
				}

				goto print_usage;

			case ':':
				if (opt.lngopt)
					fprintf (stderr, "bad argument for '%s'\n", opt.lngopt);
				else
					fprintf (stderr, "bad argument for '%c'\n", opt.opt);

				return -1;

			default:
				goto print_usage;
		}
	}

	if ((opt.ind + 1) != argc && !show_info) goto print_usage;
#endif

	hcl = hcl_openstd(HCL_SIZEOF(xtn_t), HCL_NULL);
	if (HCL_UNLIKELY(!hcl))
	{
		printf ("ERROR: cannot open hcl\n");
		goto oops;
	}

	xtn = (xtn_t*)hcl_getxtn(hcl);

	{
		hcl_oow_t tab_size;
		tab_size = 5000;
		hcl_setoption (hcl, HCL_SYMTAB_SIZE, &tab_size);
		tab_size = 5000;
		hcl_setoption (hcl, HCL_SYSDIC_SIZE, &tab_size);
		tab_size = 600; /* TODO: choose a better stack size or make this user specifiable */
		hcl_setoption (hcl, HCL_PROCSTK_SIZE, &tab_size);
	}

	{
		hcl_bitmask_t trait = 0;

		/*trait |= HCL_TRAIT_NOGC;*/
		trait |= HCL_TRAIT_AWAIT_PROCS;
		if (enable_block) trait |= HCL_TRAIT_LANG_ENABLE_BLOCK;
		if (nl_terminator) trait |= HCL_TRAIT_LANG_ENABLE_EOL;;
		hcl_setoption (hcl, HCL_TRAIT, &trait);
	}

	if (modlibdirs)
	{
	#if defined(HCL_OOCH_IS_UCH)
		hcl_ooch_t* tmp;
		tmp = hcl_dupbtoucstr(hcl, modlibdirs, HCL_NULL);
		if (HCL_UNLIKELY(!tmp))
		{
			hcl_logbfmt (hcl, HCL_LOG_STDERR,"ERROR: cannot duplicate modlibdirs - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
			goto oops;
		}

		if (hcl_setoption(hcl, HCL_MOD_LIBDIRS, tmp) <= -1)
		{
			hcl_logbfmt (hcl, HCL_LOG_STDERR,"ERROR: cannot set modlibdirs - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
			hcl_freemem (hcl, tmp);
			goto oops;
		}
		hcl_freemem (hcl, tmp);
	#else
		if (hcl_setoption(hcl, HCL_MOD_LIBDIRS, modlibdirs) <= -1)
		{
			hcl_logbfmt (hcl, HCL_LOG_STDERR,"ERROR: cannot set modlibdirs - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
			goto oops;
		}
	#endif
	}

	memset (&hclcb, 0, HCL_SIZEOF(hclcb));
	hclcb.gc = gc_hcl;
	hclcb.vm_startup = vm_startup;
	hclcb.vm_cleanup = vm_cleanup;
	/*hclcb.vm_checkbc = vm_checkbc;*/
	hcl_regcb (hcl, &hclcb);

	if (logopt)
	{
		if (handle_logopt(hcl, logopt) <= -1) goto oops;
	}

#if defined(HCL_BUILD_DEBUG)
	if (dbgopt)
	{
		if (handle_dbgopt(hcl, dbgopt) <= -1) goto oops;
	}
#endif

	if (show_info)
	{
		print_info ();
		return 0;
	}

	if (hcl_ignite(hcl, heapsize) <= -1)
	{
		hcl_logbfmt (hcl, HCL_LOG_STDERR, "cannot ignite hcl - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
		goto oops;
	}

	if (hcl_addbuiltinprims(hcl) <= -1)
	{
		hcl_logbfmt (hcl, HCL_LOG_STDERR, "cannot add builtin primitives - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
		goto oops;
	}

	xtn->cci_path = argv[opt.ind++]; /* input source code file */
	if (opt.ind < argc) xtn->udo_path = argv[opt.ind++];

	if (hcl_attachcciostdwithbcstr(hcl, xtn->cci_path) <= -1)
	{
		hcl_logbfmt (hcl, HCL_LOG_STDERR, "ERROR: cannot attach source input stream - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
		goto oops;
	}

	if (hcl_attachudiostdwithbcstr(hcl, "", xtn->udo_path) <= -1) /* TODO: add udi path */
	{
		hcl_logbfmt (hcl, HCL_LOG_STDERR, "ERROR: cannot attach user data streams - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
		goto oops;
	}

	/* -- from this point onward, any failure leads to jumping to the oops label
	 * -- instead of returning -1 immediately. --*/
	set_signal (SIGINT, handle_sigint);

#if 0
// TODO: change the option name
// in the INTERACTIVE mode, the compiler generates MAKE_FUNCTION for lambda functions.
// in the non-INTERACTIVE mode, the compiler generates MAKE_BLOCK for lambda functions.
{
	hcl_bitmask_t trait;
	hcl_getoption (hcl, HCL_TRAIT, &trait);
	trait |= HCL_TRAIT_INTERACTIVE;
	hcl_setoption (hcl, HCL_TRAIT, &trait);
}
#endif

	if (feed_loop(hcl, xtn, verbose) <= -1) goto oops;

	set_signal_to_default (SIGINT);
	hcl_close (hcl);

	return 0;

oops:
	set_signal_to_default (SIGINT); /* harmless to call multiple times without set_signal() */
	if (hcl) hcl_close (hcl);
	return -1;
}
