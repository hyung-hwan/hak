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


#include "_sys.h"
#include <stdlib.h>

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
	HAK_STACK_SETRET (hak, nargs, tv);
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

	HAK_STACK_SETRET (hak, nargs, hak->_nil);
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

	HAK_STACK_SETRET (hak, nargs, hak->_nil);
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
	HAK_STACK_SETRET (hak, nargs, HAK_SMOOI_TO_OOP(rv));
	return HAK_PF_SUCCESS;
}

static hak_pfinfo_t pfinfos[] =
{
	{ "random",      { HAK_PFBASE_FUNC,  pf_sys_random,       0,  0 } },
	{ "srandom",     { HAK_PFBASE_FUNC,  pf_sys_srandom,      1,  1 } },
	{ "stime",       { HAK_PFBASE_FUNC,  pf_sys_stime,        1,  1 } },
	{ "time",        { HAK_PFBASE_FUNC,  pf_sys_time,         0,  0 } }
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
