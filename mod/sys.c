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

static hcl_pfrc_t pf_sys_time (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_ntime_t now;
	hcl_oop_t tv;
	hcl->vmprim.vm_gettime(hcl, &now); /* should I use time() instead? */
	tv = hcl_oowtoint(hcl, now.sec);
	if (!tv) return HCL_PF_FAILURE;
	HCL_STACK_SETRET (hcl, nargs, tv);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_sys_stime (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t t;
	hcl_ooi_t ti;

	t = HCL_STACK_GETARG(hcl, nargs, 0);
	if (hcl_inttoooi(hcl, t, &ti) == 0)
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_seterrbfmt (hcl, HCL_EINVAL, "unacceptiable time value - %O - %js", t, orgmsg);
		return HCL_PF_FAILURE;
	}

	/* ---------------------------------------------------------------- */
#if defined(HAVE_SETTIMEOFDAY)
	{
		struct timeval tv;
		tv.tv_sec = ti;
		tv.tv_usec = 0;
		settimeofday (&tv, HCL_NULL);
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

	HCL_STACK_SETRET (hcl, nargs, hcl->_nil);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_sys_srandom (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	hcl_oop_t seed;
	hcl_oow_t seedw;

	seed = HCL_STACK_GETARG(hcl, nargs, 0);
	if (hcl_inttooow(hcl, seed, &seedw) == 0)
	{
		const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
		hcl_seterrbfmt (hcl, HCL_EINVAL, "unacceptiable seed - %O - %js", seed, orgmsg);
		return HCL_PF_FAILURE;
	}

#if defined(__DOS__)
	srand (seedw);
#else
	srandom (seedw);
#endif

	HCL_STACK_SETRET (hcl, nargs, hcl->_nil);
	return HCL_PF_SUCCESS;
}

static hcl_pfrc_t pf_sys_random (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs)
{
	long int r;
	hcl_ooi_t rv;

#if defined(__DOS__)
	r = rand();
#else
	r = random();
#endif
	rv = (hcl_ooi_t)(r % HCL_SMOOI_MAX);
	HCL_STACK_SETRET (hcl, nargs, HCL_SMOOI_TO_OOP(rv));
	return HCL_PF_SUCCESS;
}

static hcl_pfinfo_t pfinfos[] =
{
	{ { 'r','a','n','d','o','m','\0' },      { HCL_PFBASE_FUNC,  pf_sys_random,       0,  0 } },
	{ { 's','r','a','n','d','o','m','\0' },  { HCL_PFBASE_FUNC,  pf_sys_srandom,      1,  1 } },
	{ { 's','t','i','m','e','\0' },          { HCL_PFBASE_FUNC,  pf_sys_stime,        1,  1 } },
	{ { 't','i','m','e','\0' },              { HCL_PFBASE_FUNC,  pf_sys_time,         0,  0 } }
};

/* ------------------------------------------------------------------------ */

static hcl_pfbase_t* query (hcl_t* hcl, hcl_mod_t* mod, const hcl_ooch_t* name, hcl_oow_t namelen)
{
	return hcl_findpfbase(hcl, pfinfos, HCL_COUNTOF(pfinfos), name, namelen);
}

static void unload (hcl_t* hcl, hcl_mod_t* mod)
{
}

int hcl_mod_sys (hcl_t* hcl, hcl_mod_t* mod)
{
	mod->query = query;
	mod->unload = unload; 
	mod->ctx = HCL_NULL;
	return 0;
}
