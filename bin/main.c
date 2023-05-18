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
	const char* read_path; /* main source file */
	const char* print_path;

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
	xtn_t* xtn = (xtn_t*)hcl_getxtn(hcl);
	/*if (xtn->sym_errstr) xtn->sym_errstr = hcl_moveoop(hcl, xtn->sym_errstr);*/
}

/* ========================================================================= */

static int handle_logopt (hcl_t* hcl, const hcl_bch_t* logstr)
{
	hcl_bch_t* cm, * flt;
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

#if defined(_WIN32) || defined(__MSDOS__) || defined(__OS2__) || defined(macintosh)
typedef void(*signal_handler_t)(int);
#elif defined(macintosh)
typedef void(*signal_handler_t)(int); /* TODO: */
#elif defined(SA_SIGINFO)
typedef void(*signal_handler_t)(int, siginfo_t*, void*);
#else
typedef void(*signal_handler_t)(int);
#endif


#if defined(_WIN32) || defined(__MSDOS__) || defined(__OS2__)
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
#if defined(_WIN32) || defined(__MSDOS__) || defined(__OS2__)
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
#if defined(_WIN32) || defined(__MSDOS__) || defined(__OS2__)
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
		hcl_logbfmt (hcl, HCL_LOG_STDERR, "%s", xtn->read_path);
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


static hcl_oop_t execute_in_interactive_mode (hcl_t* hcl)
{
	hcl_oop_t retv;

	hcl_decode (hcl, 0, hcl_getbclen(hcl));
	HCL_LOG0 (hcl, HCL_LOG_MNEMONIC, "------------------------------------------\n");
	g_hcl = hcl;
	/*setup_tick ();*/

	retv = hcl_execute(hcl);

	/* flush pending output data in the interactive mode(e.g. printf without a newline) */
	hcl_flushio (hcl);

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


static hcl_oop_t execute_in_batch_mode (hcl_t* hcl, int verbose)
{
	hcl_oop_t retv;

	hcl_decode (hcl, 0, hcl_getbclen(hcl));
	HCL_LOG2 (hcl, HCL_LOG_MNEMONIC, "BYTECODES bclen = > %zu lflen => %zu\n", hcl_getbclen(hcl), hcl_getlflen(hcl));
	g_hcl = hcl;
	/*setup_tick ();*/

	retv = hcl_execute(hcl);
	hcl_flushio (hcl);

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

static int main_loop (hcl_t* hcl, xtn_t* xtn, int cflags, int verbose)
{
	while (1)
	{
		hcl_cnode_t* obj;
		int n;

/*
static int count = 0;
if (count %5 == 0) hcl_reset (hcl);
count++;
*/
		obj = hcl_read(hcl);
		if (!obj)
		{
			if (hcl->errnum == HCL_EFINIS)
			{
				/* end of input */
				break;
			}
			else if (hcl->errnum == HCL_ESYNERR)
			{
				print_synerr (hcl);
				if (hcl_isstdreadertty(hcl) && hcl_getsynerrnum(hcl) != HCL_SYNERR_EOF)
				{
					/* TODO: drain remaining data in the reader including the actual input stream and buffered data in hcl */
					continue;
				}
			}
			else
			{
				hcl_logbfmt (hcl, HCL_LOG_STDERR, "ERROR: cannot read object - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
			}
			goto oops;
		}

		if (verbose) hcl_prbfmt (hcl, "\n"); /* flush the output buffer by hcl_print above */
		n = hcl_compile(hcl, obj, cflags);
		hcl_freecnode (hcl, obj); /* not needed any more */

		if (n <= -1)
		{
			if (hcl->errnum == HCL_ESYNERR)
			{
				print_synerr (hcl);
			}
			else
			{
				hcl_logbfmt (hcl, HCL_LOG_STDERR, "ERROR: cannot compile object - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
			}
			/* carry on? */

			if (!hcl_isstdreadertty(hcl)) goto oops;
		}
		else if (hcl_isstdreadertty(hcl))
		{
			/* interactive mode */
			execute_in_interactive_mode (hcl);
		}
	}

	if (!hcl_isstdreadertty(hcl) && hcl_getbclen(hcl) > 0) execute_in_batch_mode (hcl, verbose);

	return 0;

oops:
	return -1;
}

static int on_fed_cnode_in_interactive_mode (hcl_t* hcl, hcl_cnode_t* obj)
{
	if (hcl_compile(hcl, obj, HCL_COMPILE_CLEAR_CODE | HCL_COMPILE_CLEAR_FNBLK) <= -1) return -1;
	execute_in_interactive_mode (hcl);
	return 0;
}

static int feed_loop (hcl_t* hcl, xtn_t* xtn, int cflags, int verbose)
{
	hcl_ioinarg_t* inarg;

	/* override the default cnode handler. the default one simply
	 * compiles the expression node without execution */
	if (hcl_beginfeed (hcl, hcl_isstdreadertty(hcl)? on_fed_cnode_in_interactive_mode: HCL_NULL) <= -1)
	{
		hcl_logbfmt (hcl, HCL_LOG_STDERR, "ERROR: cannot begin feed - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
		goto oops;
	}

	/* [NOTE] it isn't a very nice idea to get this internal data and use it with read_input() */
	inarg = hcl_getbaseinarg(hcl); 
	while (1)
	{
		//if (read_input(hcl, inarg) <= -1) goto oops;
		if (hcl_readbaseinraw(hcl) <= -1) goto oops;
		if (inarg->xlen <= 0) break;
		if (hcl_feed(hcl, inarg->buf, inarg->xlen) <= -1) goto feed_error;
	}
	if (hcl_endfeed(hcl) <= -1)
	{
	feed_error:
		if (hcl->errnum == HCL_ESYNERR) print_synerr (hcl);
		else hcl_logbfmt (hcl, HCL_LOG_STDERR, "ERROR: cannot feed - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
		goto oops; /* TODO: proceed or just exit? */
	}

	if (!hcl_isstdreadertty(hcl) && hcl_getbclen(hcl) > 0) execute_in_batch_mode (hcl, verbose);
	return 0;

oops:
	return -1;
}

#define DEFAULT_HEAPSIZE 512000ul

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
		{ ":log",         'l' },

		{ HCL_NULL,       '\0' }
	};
	static hcl_bopt_t opt =
	{
		"l:xv",
		lopt
	};

	const char* logopt = HCL_NULL;
	hcl_oow_t heapsize = DEFAULT_HEAPSIZE;
	int cflags;
	int verbose = 0;
	int experimental = 0;

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

	while ((c = hcl_getbopt (argc, argv, &opt)) != HCL_BCI_EOF)
	{
		switch (c)
		{
			case 'l':
				logopt = opt.arg;
				break;

			case 'x':
				experimental = 1;
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

	if (opt.ind >= argc) goto print_usage;
#endif

	hcl = hcl_openstd(HCL_SIZEOF(xtn_t), HCL_NULL);
	if (HCL_UNLIKELY(!hcl))
	{
		printf ("ERROR: cannot open hcl\n");
		goto oops;
	}


	{
		hcl_oow_t tab_size;
		tab_size = 5000;
		hcl_setoption (hcl, HCL_SYMTAB_SIZE, &tab_size);
		tab_size = 5000;
		hcl_setoption (hcl, HCL_SYSDIC_SIZE, &tab_size);
		tab_size = 600;
		hcl_setoption (hcl, HCL_PROCSTK_SIZE, &tab_size);
	}

	{
		hcl_bitmask_t trait = 0;

		/*trait |= HCL_TRAIT_NOGC;*/
		trait |= HCL_TRAIT_AWAIT_PROCS;
		hcl_setoption (hcl, HCL_TRAIT, &trait);

		/* disable GC logs */
		/*trait = ~HCL_LOG_GC;
		hcl_setoption (hcl, HCL_LOG_MASK, &trait);*/
	}

	xtn = (xtn_t*)hcl_getxtn(hcl);

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

	xtn->read_path = argv[opt.ind++];
	if (opt.ind < argc) xtn->print_path = argv[opt.ind++];

	//if (hcl_attachio(hcl, read_handler, print_handler) <= -1)
	if (hcl_attachiostdwithbcstr(hcl, xtn->read_path, xtn->print_path) <= -1)
	{
		hcl_logbfmt (hcl, HCL_LOG_STDERR, "ERROR: cannot attach IO streams - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
		goto oops;
	}

	/*
	{
		hcl_ooch_t errstr[] =  { 'E', 'R', 'R', 'S', 'T', 'R' };
		xtn->sym_errstr = hcl_makesymbol(hcl, errstr, 6);
		if (!xtn->sym_errstr)
		{
			hcl_logbfmt (hcl, HCL_LOG_STDERR, "ERROR: cannot create the ERRSTR symbol - [%d] %js\n", hcl_geterrnum(hcl), hcl_geterrmsg(hcl));
			goto oops;
		}
		HCL_OBJ_SET_FLAGS_KERNEL (xtn->sym_errstr, 1);
	}
	*/

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

	cflags = 0;
	if (hcl_isstdreadertty(hcl)) cflags = HCL_COMPILE_CLEAR_CODE | HCL_COMPILE_CLEAR_FNBLK;

	if (experimental)
	{
		/* this is to test the feed-based reader */
		if (feed_loop(hcl, xtn, cflags, verbose) <= -1) goto oops;
	}
	else
	{
		if (main_loop(hcl, xtn, cflags, verbose) <= -1) goto oops;
	}

	set_signal_to_default (SIGINT);
	hcl_close (hcl);

	return 0;

oops:
	set_signal_to_default (SIGINT); /* harmless to call multiple times without set_signal() */
	if (hcl) hcl_close (hcl);
	return -1;
}
