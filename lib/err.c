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

#include "hcl-prv.h"

#if defined(HCL_ENABLE_LIBUNWIND)
#	define UNW_LOCAL_ONLY
#	include <libunwind.h>
#endif

static hcl_ooch_t errstr_0[] = {'n','o',' ','e','r','r','o','r','\0'};
static hcl_ooch_t errstr_1[] = {'g','e','n','e','r','i','c',' ','e','r','r','o','r','\0'};
static hcl_ooch_t errstr_2[] = {'n','o','t',' ','i','m','p','l','e','m','e','n','t','e','d','\0'};
static hcl_ooch_t errstr_3[] = {'s','u','b','s','y','s','t','e','m',' ','e','r','r','o','r','\0'};
static hcl_ooch_t errstr_4[] = {'i','n','t','e','r','n','a','l',' ','e','r','r','o','r',' ','t','h','a','t',' ','s','h','o','u','l','d',' ','n','e','v','e','r',' ','h','a','v','e',' ','h','a','p','p','e','n','e','d','\0'};

static hcl_ooch_t errstr_5[] = {'i','n','s','u','f','f','i','c','i','e','n','t',' ','s','y','s','t','e','m',' ','m','e','m','o','r','y','\0'};
static hcl_ooch_t errstr_6[] = {'i','n','s','u','f','f','i','c','i','e','n','t',' ','o','b','j','e','c','t',' ','m','e','m','o','r','y','\0'};
static hcl_ooch_t errstr_7[] = {'i','n','v','a','l','i','d',' ','c','l','a','s','s','/','t','y','p','e','\0'};
static hcl_ooch_t errstr_8[] = {'i','n','v','a','l','i','d',' ','p','a','r','a','m','e','t','e','r','/','a','r','g','u','m','e','n','t','\0'};
static hcl_ooch_t errstr_9[] = {'d','a','t','a',' ','n','o','t',' ','f','o','u','n','d','\0'};

static hcl_ooch_t errstr_10[] = {'e','x','i','s','t','i','n','g','/','d','u','p','l','i','c','a','t','e',' ','d','a','t','a','\0'};
static hcl_ooch_t errstr_11[] = {'b','u','s','y','\0'};
static hcl_ooch_t errstr_12[] = {'a','c','c','e','s','s',' ','d','e','n','i','e','d','\0'};
static hcl_ooch_t errstr_13[] = {'o','p','e','r','a','t','i','o','n',' ','n','o','t',' ','p','e','r','m','i','t','t','e','d','\0'};
static hcl_ooch_t errstr_14[] = {'n','o','t',' ','a',' ','d','i','r','e','c','t','o','r','y','\0'};

static hcl_ooch_t errstr_15[] = {'i','n','t','e','r','r','u','p','t','e','d','\0'};
static hcl_ooch_t errstr_16[] = {'p','i','p','e',' ','e','r','r','o','r','\0'};
static hcl_ooch_t errstr_17[] = {'r','e','s','o','u','r','c','e',' ','t','e','m','p','o','r','a','r','i','l','y',' ','u','n','a','v','a','i','l','a','b','l','e','\0'};
static hcl_ooch_t errstr_18[] = {'b','a','d',' ','s','y','s','t','e','m',' ','h','a','n','d','l','e','\0'};
static hcl_ooch_t errstr_19[] = {'t','o','o',' ','m','a','n','y',' ','f','r','a','m','e','s','\0'};

static hcl_ooch_t errstr_20[] = {'m','e','s','s','a','g','e',' ','r','e','c','e','i','v','e','r',' ','e','r','r','o','r','\0'};
static hcl_ooch_t errstr_21[] = {'m','e','s','s','a','g','e',' ','s','e','n','d','i','n','g',' ','e','r','r','o','r','\0'};
static hcl_ooch_t errstr_22[] = {'w','r','o','n','g',' ','n','u','m','b','e','r',' ','o','f',' ','a','r','g','u','m','e','n','t','s','\0'};
static hcl_ooch_t errstr_23[] = {'r','a','n','g','e',' ','e','r','r','o','r','\0'};
static hcl_ooch_t errstr_24[] = {'b','y','t','e','-','c','o','d','e',' ','f','u','l','l','\0'};

static hcl_ooch_t errstr_25[] = {'d','i','c','t','i','o','n','a','r','y',' ','f','u','l','l','\0'};
static hcl_ooch_t errstr_26[] = {'p','r','o','c','e','s','s','o','r',' ','f','u','l','l','\0'};
static hcl_ooch_t errstr_27[] = {'n','o',' ','m','o','r','e',' ','i','n','p','u','t','\0'};
static hcl_ooch_t errstr_28[] = {'t','o','o',' ','m','a','n','y',' ','i','t','e','m','s','\0'};
static hcl_ooch_t errstr_29[] = {'d','i','v','i','d','e',' ','b','y',' ','z','e','r','o','\0'};

static hcl_ooch_t errstr_30[] = {'I','/','O',' ','e','r','r','o','r','\0'};
static hcl_ooch_t errstr_31[] = {'e','n','c','o','d','i','n','g',' ','c','o','n','v','e','r','s','i','o','n',' ','e','r','r','o','r','\0'};
static hcl_ooch_t errstr_32[] = {'b','u','f','f','e','r',' ','f','u','l','l','\0'};
static hcl_ooch_t errstr_33[] = {'s','y','n','t','a','x',' ','e','r','r','o','r','\0'};
static hcl_ooch_t errstr_34[] = {'c','a','l','l',' ','e','r','r','o','r','\0'};

static hcl_ooch_t errstr_35[] = {'a','r','g','u','m','e','n','t',' ','n','u','m','b','e','r',' ','e','r','r','o','r','\0'};
static hcl_ooch_t errstr_36[] = {'r','e','t','u','r','n',' ','c','o','u','n','t',' ','e','r','r','o','r','\0'};
static hcl_ooch_t errstr_37[] = {'t','o','o',' ','m','a','n','y',' ','s','e','m','a','p','h','o','r','e','s','\0'};
static hcl_ooch_t errstr_38[] = {'e','x','c','e','p','a','i','o','n',' ','n','o','t',' ','h','a','n','d','l','e','d','\0'};
static hcl_ooch_t errstr_39[] = {'s','t','a','c','k',' ','u','n','d','e','r','f','l','o','w','\0'};

static hcl_ooch_t errstr_40[] = {'s','t','a','c','k',' ','o','v','e','r','f','l','o','w','\0'};
static hcl_ooch_t errstr_41[] = {'u','n','d','e','f','i','n','e','d',' ','v','a','r','i','a','b','l','e',' ','a','c','c','e','s','s','\0'};

static hcl_ooch_t* errstr[] =
{
	errstr_0, errstr_1, errstr_2, errstr_3, errstr_4, errstr_5, errstr_6, errstr_7,
	errstr_8, errstr_9, errstr_10, errstr_11, errstr_12, errstr_13, errstr_14, errstr_15,
	errstr_16, errstr_17, errstr_18, errstr_19, errstr_20, errstr_21, errstr_22, errstr_23,
	errstr_24, errstr_25, errstr_26, errstr_27, errstr_28, errstr_29, errstr_30, errstr_31,
	errstr_32, errstr_33, errstr_34, errstr_35, errstr_36, errstr_37, errstr_38, errstr_39,
	errstr_40, errstr_41
};


static char* synerrstr[] =
{
	"no error",
	"internal error",
	"unexpected compiler node",
	"illegal character",
	"illegal token",
	"comment not closed",
	"wrong character literal",
	"wrong string literal",
	"wrong symbol literal",
	"invalid hashed literal",
	"invalid numeric literal",
	"out of integer range",
	"wrong error literal",
	"wrong smptr literal",
	"wrong multi-segment identifer",
	"invalid radix for a numeric literal",

	"sudden end of input",
	"sudden end of expression",
	"( expected",
	") expected",
	"] expected",
	"} expected",
	"| expected",

	"string expected",
	"byte too small or too large",
	"nesting level too deep",

	", expected",
	"| disallowed",
	". disallowed",
	", disallowed",
	": disallowed",
	":= disallowed",
	"no value after ,",
	"no value after :",
	"missing value",
	"no separator between array/dictionary elements",
	"#include error",

	"... disallowed",
	"::: disallowed",
	"loop body too big",
	"if body too big",
	"block too big",
	"block too deep",
	"name expected",
	"argument name list expected",
	"argument name expected",
	"duplicate argument name",
	"variable name expected",
	"wrong number of arguments",
	"too many arguments defined",
	"too many variables defined",
	"variable declaration disallowed",
	"duplicate variable name",
	"unknown variable name",

	"disallowed variable name",
	"disallowed argument name",
	"disallowed",

	"elif without if",
	"else without if",
	"catch without try",
	"break outside loop",

	"invalid callable",
	"unbalanced key/value pair",
	"unbalanced parenthesis/brace/bracket",
	"unexpected semicolon",
	"stray backslash",
	"block expression expected",
	"block expression disallowed",
	"invalid lvalue",
	"invalid rvalue"
};

/* --------------------------------------------------------------------------
 * ERROR NUMBER TO STRING CONVERSION
 * -------------------------------------------------------------------------- */

static hcl_bch_t e_unknown_b[] = {'u','n','k','n','o','w','n',' ','e','r','r','o','r','\0'};
static hcl_uch_t e_unknown_u[] = {'u','n','k','n','o','w','n',' ','e','r','r','o','r','\0'};
#if defined(HCL_OOCH_IS_BCH)
#	define e_unknown e_unknown_b
#else
#	define e_unknown e_unknown_u
#endif


int hcl_errnum_is_synerr (hcl_errnum_t errnum)
{
	return errnum == HCL_ESYNERR;
}

const hcl_ooch_t* hcl_errnum_to_errstr (hcl_errnum_t errnum)
{
	return (errnum >= 0 && errnum < HCL_COUNTOF(errstr))? errstr[errnum]: e_unknown;
}

const hcl_bch_t* hcl_errnum_to_errbcstr (hcl_errnum_t errnum, hcl_bch_t* buf, hcl_oow_t len)
{
	/* it's ok to copy without conversion because the messages above are simple acsii text */
#if defined(HCL_OOCH_IS_BCH)
	hcl_copy_bcstr(buf, len, (errnum >= 0 && errnum < HCL_COUNTOF(errstr))? errstr[errnum]: e_unknown_b);
#else
	hcl_copy_ucstr_to_bcstr(buf, len, (errnum >= 0 && errnum < HCL_COUNTOF(errstr))? errstr[errnum]: e_unknown_u);
#endif
	return buf;
}

const hcl_uch_t* hcl_errnum_to_errucstr (hcl_errnum_t errnum, hcl_uch_t* buf, hcl_oow_t len)
{
	/* it's ok to copy without conversion because the messages above are simple acsii text */
#if defined(HCL_OOCH_IS_BCH)
	hcl_copy_bcstr_to_ucstr(buf, len, (errnum >= 0 && errnum < HCL_COUNTOF(errstr))? errstr[errnum]: e_unknown_b);
#else
	hcl_copy_ucstr(buf, len, (errnum >= 0 && errnum < HCL_COUNTOF(errstr))? errstr[errnum]: e_unknown_u);
#endif
	return buf;
}

static const hcl_bch_t* synerr_to_errstr (hcl_synerrnum_t errnum)
{
	return (errnum >= 0 && errnum < HCL_COUNTOF(synerrstr))? synerrstr[errnum]: e_unknown_b;
}

/* --------------------------------------------------------------------------
 * ERROR NUMBER/MESSAGE HANDLING
 * -------------------------------------------------------------------------- */

const hcl_ooch_t* hcl_geterrstr (hcl_t* hcl)
{
	return hcl_errnum_to_errstr(hcl->errnum);
}

/*
const hcl_ooch_t* hcl_geterrmsg (hcl_t* hcl)
{
	if (hcl->errmsg.len <= 0) return hcl_errnum_to_errstr(hcl->errnum);
	return hcl->errmsg.buf;
}
*/

const hcl_bch_t* hcl_geterrbmsg (hcl_t* hcl)
{
#if defined(HCL_OOCH_IS_BCH)
	return (hcl->errmsg.len <= 0)? hcl_errnum_to_errstr(hcl->errnum): hcl->errmsg.buf;
#else
	const hcl_ooch_t* msg;
	hcl_oow_t wcslen, mbslen;

	msg = (hcl->errmsg.len <= 0)? hcl_errnum_to_errstr(hcl->errnum): hcl->errmsg.buf;

	mbslen = HCL_COUNTOF(hcl->errmsg.xerrmsg);
	hcl_conv_ucstr_to_bcstr_with_cmgr (msg, &wcslen, hcl->errmsg.xerrmsg, &mbslen, HCL_CMGR(hcl));

	return hcl->errmsg.xerrmsg;
#endif
}

const hcl_uch_t* hcl_geterrumsg (hcl_t* hcl)
{
#if defined(HCL_OOCH_IS_BCH)
	const hcl_ooch_t* msg;
	hcl_oow_t wcslen, mbslen;

	msg = (hcl->errmsg.len <= 0)? hcl_errnum_to_errstr(hcl->errnum): hcl->errmsg.buf;

	wcslen = HCL_COUNTOF(hcl->errmsg.xerrmsg);
	hcl_conv_bcstr_to_ucstr_with_cmgr (msg, &mbslen, hcl->errmsg.xerrmsg, &wcslen, HCL_CMGR(hcl), 1);

	return hcl->errmsg.xerrmsg;
#else
	return (hcl->errmsg.len == '\0')? hcl_errnum_to_errstr(hcl->errnum): hcl->errmsg.buf;
#endif
}

hcl_oow_t hcl_copyerrbmsg (hcl_t* hcl, hcl_bch_t* buf, hcl_oow_t len)
{
	return hcl_copy_bcstr(buf, len, hcl_geterrbmsg(hcl));
}

hcl_oow_t hcl_copyerrumsg (hcl_t* hcl, hcl_uch_t* buf, hcl_oow_t len)
{
	return hcl_copy_ucstr(buf, len, hcl_geterrumsg(hcl));
}

const hcl_ooch_t* hcl_backuperrmsg (hcl_t* hcl)
{
	hcl_copy_oocstr (hcl->errmsg.tmpbuf.ooch, HCL_COUNTOF(hcl->errmsg.tmpbuf.ooch), hcl_geterrmsg(hcl));
	return hcl->errmsg.tmpbuf.ooch;
}

hcl_errnum_t hcl_geterrnum (hcl_t* hcl)
{
	return HCL_ERRNUM(hcl);
}

void hcl_seterrnum (hcl_t* hcl, hcl_errnum_t errnum)
{
	if (hcl->shuterr) return;
	hcl->errnum = errnum;
	hcl->errmsg.len = 0;
	HCL_MEMSET (&hcl->errloc, 0, HCL_SIZEOF(hcl->errloc));
}

void hcl_geterrloc (hcl_t* hcl, hcl_loc_t* loc)
{
	if (loc) *loc = hcl->errloc;
}

void hcl_seterrbmsg (hcl_t* hcl, hcl_errnum_t errnum, const hcl_bch_t* errmsg)
{
	hcl_seterrbfmt(hcl, errnum, "%hs", errmsg);
}

void hcl_seterrumsg (hcl_t* hcl, hcl_errnum_t errnum, const hcl_uch_t* errmsg)
{
	hcl_seterrbfmt(hcl, errnum, "%ls", errmsg);
}

static int err_bcs (hcl_t* hcl, hcl_fmtout_t* fmtout, const hcl_bch_t* ptr, hcl_oow_t len)
{
	hcl_oow_t max;

	max = HCL_COUNTOF(hcl->errmsg.buf) - hcl->errmsg.len - 1;

#if defined(HCL_OOCH_IS_UCH)
	if (max <= 0) return 1;
	hcl_conv_bchars_to_uchars_with_cmgr (ptr, &len, &hcl->errmsg.buf[hcl->errmsg.len], &max, HCL_CMGR(hcl), 1);
	hcl->errmsg.len += max;
#else
	if (len > max) len = max;
	if (len <= 0) return 1;
	HCL_MEMCPY (&hcl->errmsg.buf[hcl->errmsg.len], ptr, len * HCL_SIZEOF(*ptr));
	hcl->errmsg.len += len;
#endif

	hcl->errmsg.buf[hcl->errmsg.len] = '\0';

	return 1; /* success */
}

static int err_ucs (hcl_t* hcl, hcl_fmtout_t* fmtout, const hcl_uch_t* ptr, hcl_oow_t len)
{
	hcl_oow_t max;

	max = HCL_COUNTOF(hcl->errmsg.buf) - hcl->errmsg.len - 1;

#if defined(HCL_OOCH_IS_UCH)
	if (len > max) len = max;
	if (len <= 0) return 1;
	HCL_MEMCPY (&hcl->errmsg.buf[hcl->errmsg.len], ptr, len * HCL_SIZEOF(*ptr));
	hcl->errmsg.len += len;
#else
	if (max <= 0) return 1;
	hcl_conv_uchars_to_bchars_with_cmgr (ptr, &len, &hcl->errmsg.buf[hcl->errmsg.len], &max, HCL_CMGR(hcl));
	hcl->errmsg.len += max;
#endif
	hcl->errmsg.buf[hcl->errmsg.len] = '\0';
	return 1; /* success */
}

void hcl_seterrbfmt (hcl_t* hcl, hcl_errnum_t errnum, const hcl_bch_t* fmt, ...)
{
	va_list ap;
	hcl_fmtout_t fo;

	if (hcl->shuterr) return;
	hcl->errmsg.len = 0;

	HCL_MEMSET (&fo, 0, HCL_SIZEOF(fo));
	fo.putbchars = err_bcs;
	fo.putuchars = err_ucs;
	fo.putobj = hcl_fmt_object;

	va_start (ap, fmt);
	hcl_bfmt_outv (hcl, &fo, fmt, ap);
	va_end (ap);

	hcl->errnum = errnum;
	HCL_MEMSET (&hcl->errloc, 0, HCL_SIZEOF(hcl->errloc));
}

void hcl_seterrufmt (hcl_t* hcl, hcl_errnum_t errnum, const hcl_uch_t* fmt, ...)
{
	va_list ap;
	hcl_fmtout_t fo;

	if (hcl->shuterr) return;
	hcl->errmsg.len = 0;

	HCL_MEMSET (&fo, 0, HCL_SIZEOF(fo));
	fo.putbchars = err_bcs;
	fo.putuchars = err_ucs;
	fo.putobj = hcl_fmt_object;

	va_start (ap, fmt);
	hcl_ufmt_outv (hcl, &fo, fmt, ap);
	va_end (ap);

	hcl->errnum = errnum;
	HCL_MEMSET (&hcl->errloc, 0, HCL_SIZEOF(hcl->errloc));
}


void hcl_seterrbfmtv (hcl_t* hcl, hcl_errnum_t errnum, const hcl_bch_t* fmt, va_list ap)
{
	hcl_fmtout_t fo;

	if (hcl->shuterr) return;

	hcl->errmsg.len = 0;

	HCL_MEMSET (&fo, 0, HCL_SIZEOF(fo));
	fo.putbchars = err_bcs;
	fo.putuchars = err_ucs;
	fo.putobj = hcl_fmt_object;

	hcl_bfmt_outv (hcl, &fo, fmt, ap);
	hcl->errnum = errnum;
	HCL_MEMSET (&hcl->errloc, 0, HCL_SIZEOF(hcl->errloc));
}

void hcl_seterrufmtv (hcl_t* hcl, hcl_errnum_t errnum, const hcl_uch_t* fmt, va_list ap)
{
	hcl_fmtout_t fo;

	if (hcl->shuterr) return;

	hcl->errmsg.len = 0;

	HCL_MEMSET (&fo, 0, HCL_SIZEOF(fo));
	fo.putbchars = err_bcs;
	fo.putuchars = err_ucs;
	fo.putobj = hcl_fmt_object;

	hcl_ufmt_outv (hcl, &fo, fmt, ap);
	hcl->errnum = errnum;
	HCL_MEMSET (&hcl->errloc, 0, HCL_SIZEOF(hcl->errloc));
}

void hcl_seterrbfmtloc (hcl_t* hcl, hcl_errnum_t errnum, const hcl_loc_t* loc, const hcl_bch_t* fmt, ...)
{
	va_list ap;
	va_start (ap, fmt);
	hcl_seterrbfmtv (hcl, errnum, fmt, ap);
	va_end (ap);
	hcl->errloc = *loc;
}

void hcl_seterrufmtloc (hcl_t* hcl, hcl_errnum_t errnum, const hcl_loc_t* loc, const hcl_uch_t* fmt, ...)
{
	va_list ap;
	va_start (ap, fmt);
	hcl_seterrufmtv (hcl, errnum, fmt, ap);
	va_end (ap);
	hcl->errloc = *loc;
}

void hcl_seterrwithsyserr (hcl_t* hcl, int syserr_type, int syserr_code)
{
	hcl_errnum_t errnum;

	if (hcl->shuterr) return;

	if (hcl->vmprim.syserrstrb)
	{
		errnum = hcl->vmprim.syserrstrb(hcl, syserr_type, syserr_code, hcl->errmsg.tmpbuf.bch, HCL_COUNTOF(hcl->errmsg.tmpbuf.bch));
		hcl_seterrbfmt (hcl, errnum, "%hs", hcl->errmsg.tmpbuf.bch);
	}
	else
	{
		HCL_ASSERT (hcl, hcl->vmprim.syserrstru != HCL_NULL);
		errnum = hcl->vmprim.syserrstru(hcl, syserr_type, syserr_code, hcl->errmsg.tmpbuf.uch, HCL_COUNTOF(hcl->errmsg.tmpbuf.uch));
		hcl_seterrbfmt (hcl, errnum, "%ls", hcl->errmsg.tmpbuf.uch);
	}
}

void hcl_seterrbfmtwithsyserr (hcl_t* hcl, int syserr_type, int syserr_code, const hcl_bch_t* fmt, ...)
{
	hcl_errnum_t errnum;
	hcl_oow_t ucslen, bcslen;
	va_list ap;

	if (hcl->shuterr) return;

	if (hcl->vmprim.syserrstrb)
	{
		errnum = hcl->vmprim.syserrstrb(hcl, syserr_type, syserr_code, hcl->errmsg.tmpbuf.bch, HCL_COUNTOF(hcl->errmsg.tmpbuf.bch));

		va_start (ap, fmt);
		hcl_seterrbfmtv (hcl, errnum, fmt, ap);
		va_end (ap);

		if (HCL_COUNTOF(hcl->errmsg.buf) - hcl->errmsg.len >= 5)
		{
			hcl->errmsg.buf[hcl->errmsg.len++] = ' ';
			hcl->errmsg.buf[hcl->errmsg.len++] = '-';
			hcl->errmsg.buf[hcl->errmsg.len++] = ' ';

		#if defined(HCL_OOCH_IS_BCH)
			hcl->errmsg.len += hcl_copy_bcstr(&hcl->errmsg.buf[hcl->errmsg.len], HCL_COUNTOF(hcl->errmsg.buf) - hcl->errmsg.len, hcl->errmsg.tmpbuf.bch);
		#else
			ucslen = HCL_COUNTOF(hcl->errmsg.buf) - hcl->errmsg.len;
			hcl_convbtoucstr (hcl, hcl->errmsg.tmpbuf.bch, &bcslen, &hcl->errmsg.buf[hcl->errmsg.len], &ucslen);
			hcl->errmsg.len += ucslen;
		#endif
		}
	}
	else
	{
		HCL_ASSERT (hcl, hcl->vmprim.syserrstru != HCL_NULL);
		errnum = hcl->vmprim.syserrstru(hcl, syserr_type, syserr_code, hcl->errmsg.tmpbuf.uch, HCL_COUNTOF(hcl->errmsg.tmpbuf.uch));

		va_start (ap, fmt);
		hcl_seterrbfmtv (hcl, errnum, fmt, ap);
		va_end (ap);

		if (HCL_COUNTOF(hcl->errmsg.buf) - hcl->errmsg.len >= 5)
		{
			hcl->errmsg.buf[hcl->errmsg.len++] = ' ';
			hcl->errmsg.buf[hcl->errmsg.len++] = '-';
			hcl->errmsg.buf[hcl->errmsg.len++] = ' ';

		#if defined(HCL_OOCH_IS_BCH)
			bcslen = HCL_COUNTOF(hcl->errmsg.buf) - hcl->errmsg.len;
			hcl_convutobcstr (hcl, hcl->errmsg.tmpbuf.uch, &ucslen, &hcl->errmsg.buf[hcl->errmsg.len], &bcslen);
			hcl->errmsg.len += bcslen;
		#else
			hcl->errmsg.len += hcl_copy_ucstr(&hcl->errmsg.buf[hcl->errmsg.len], HCL_COUNTOF(hcl->errmsg.buf) - hcl->errmsg.len, hcl->errmsg.tmpbuf.uch);
		#endif
		}
	}
}

void hcl_seterrufmtwithsyserr (hcl_t* hcl, int syserr_type, int syserr_code, const hcl_uch_t* fmt, ...)
{
	hcl_errnum_t errnum;
	hcl_oow_t ucslen, bcslen;
	va_list ap;

	if (hcl->shuterr) return;

	if (hcl->vmprim.syserrstrb)
	{
		errnum = hcl->vmprim.syserrstrb(hcl, syserr_type, syserr_code, hcl->errmsg.tmpbuf.bch, HCL_COUNTOF(hcl->errmsg.tmpbuf.bch));

		va_start (ap, fmt);
		hcl_seterrufmtv (hcl, errnum, fmt, ap);
		va_end (ap);

		if (HCL_COUNTOF(hcl->errmsg.buf) - hcl->errmsg.len >= 5)
		{
			hcl->errmsg.buf[hcl->errmsg.len++] = ' ';
			hcl->errmsg.buf[hcl->errmsg.len++] = '-';
			hcl->errmsg.buf[hcl->errmsg.len++] = ' ';

		#if defined(HCL_OOCH_IS_BCH)
			hcl->errmsg.len += hcl_copy_bcstr(&hcl->errmsg.buf[hcl->errmsg.len], HCL_COUNTOF(hcl->errmsg.buf) - hcl->errmsg.len, hcl->errmsg.tmpbuf.bch);
		#else
			ucslen = HCL_COUNTOF(hcl->errmsg.buf) - hcl->errmsg.len;
			hcl_convbtoucstr (hcl, hcl->errmsg.tmpbuf.bch, &bcslen, &hcl->errmsg.buf[hcl->errmsg.len], &ucslen);
			hcl->errmsg.len += ucslen;
		#endif
		}
	}
	else
	{
		HCL_ASSERT (hcl, hcl->vmprim.syserrstru != HCL_NULL);
		errnum = hcl->vmprim.syserrstru(hcl, syserr_type, syserr_code, hcl->errmsg.tmpbuf.uch, HCL_COUNTOF(hcl->errmsg.tmpbuf.uch));

		va_start (ap, fmt);
		hcl_seterrufmtv (hcl, errnum, fmt, ap);
		va_end (ap);

		if (HCL_COUNTOF(hcl->errmsg.buf) - hcl->errmsg.len >= 5)
		{
			hcl->errmsg.buf[hcl->errmsg.len++] = ' ';
			hcl->errmsg.buf[hcl->errmsg.len++] = '-';
			hcl->errmsg.buf[hcl->errmsg.len++] = ' ';

		#if defined(HCL_OOCH_IS_BCH)
			bcslen = HCL_COUNTOF(hcl->errmsg.buf) - hcl->errmsg.len;
			hcl_convutobcstr (hcl, hcl->errmsg.tmpbuf.uch, &ucslen, &hcl->errmsg.buf[hcl->errmsg.len], &bcslen);
			hcl->errmsg.len += bcslen;
		#else
			hcl->errmsg.len += hcl_copy_ucstr(&hcl->errmsg.buf[hcl->errmsg.len], HCL_COUNTOF(hcl->errmsg.buf) - hcl->errmsg.len, hcl->errmsg.tmpbuf.uch);
		#endif
		}
	}
}

/* --------------------------------------------------------------------------
 * SYNTAX ERROR HANDLING
 * -------------------------------------------------------------------------- */

void hcl_getsynerr (hcl_t* hcl, hcl_synerr_t* synerr)
{
	HCL_ASSERT (hcl, hcl->c != HCL_NULL);
	if (synerr) *synerr = hcl->c->synerr;
}

hcl_synerrnum_t hcl_getsynerrnum (hcl_t* hcl)
{
	HCL_ASSERT (hcl, hcl->c != HCL_NULL);
	return hcl->c->synerr.num;
}

void hcl_setsynerrbfmt (hcl_t* hcl, hcl_synerrnum_t num, const hcl_loc_t* loc, const hcl_oocs_t* tgt, const hcl_bch_t* msgfmt, ...)
{
	static hcl_bch_t syntax_error[] = "syntax error - ";

	if (hcl->shuterr) return;

	if (msgfmt)
	{
		va_list ap;
		int i, selen;

		va_start (ap, msgfmt);
		hcl_seterrbfmtv (hcl, HCL_ESYNERR, msgfmt, ap);
		va_end (ap);

		selen = HCL_COUNTOF(syntax_error) - 1;
		HCL_MEMMOVE (&hcl->errmsg.buf[selen], &hcl->errmsg.buf[0], HCL_SIZEOF(hcl->errmsg.buf[0]) * (HCL_COUNTOF(hcl->errmsg.buf) - selen));
		for (i = 0; i < selen; i++) hcl->errmsg.buf[i] = syntax_error[i];
		hcl->errmsg.buf[HCL_COUNTOF(hcl->errmsg.buf) - 1] = '\0';
	}
	else
	{
		hcl_seterrbfmt (hcl, HCL_ESYNERR, "%hs%hs", syntax_error, synerr_to_errstr(num));
	}
	hcl->c->synerr.num = num;

	/* The SCO compiler complains of this ternary operation saying:
	 *    error: operands have incompatible types: op ":"
	 * it seems to complain of type mismatch between *loc and
	 * hcl->c->tok.loc due to 'const' prefixed to loc. */
	/*hcl->c->synerr.loc = loc? *loc: hcl->c->tok.loc;*/
	if (loc)
	{
		hcl->c->synerr.loc = *loc;
	}
	else
	{
		hcl->c->synerr.loc = hcl->c->tok.loc;
	}

	if (tgt)
	{
		hcl_oow_t n;
		n = hcl_copy_oochars_to_oocstr(hcl->c->synerr.tgt.val, HCL_COUNTOF(hcl->c->synerr.tgt.val), tgt->ptr, tgt->len);
		if (n < tgt->len)
		{
			hcl->c->synerr.tgt.val[n - 1] = '.';
			hcl->c->synerr.tgt.val[n - 2] = '.';
			hcl->c->synerr.tgt.val[n - 3] = '.';
		}
		hcl->c->synerr.tgt.len = n;
	}
	else
	{
		hcl->c->synerr.tgt.val[0] = '\0';
		hcl->c->synerr.tgt.len = 0;
	}
}

void hcl_setsynerrufmt (hcl_t* hcl, hcl_synerrnum_t num, const hcl_loc_t* loc, const hcl_oocs_t* tgt, const hcl_uch_t* msgfmt, ...)
{
	static hcl_bch_t syntax_error[] = "syntax error - ";

	if (hcl->shuterr) return;

	if (msgfmt)
	{
		va_list ap;
		int i, selen;

		va_start (ap, msgfmt);
		hcl_seterrufmtv (hcl, HCL_ESYNERR, msgfmt, ap);
		va_end (ap);

		selen = HCL_COUNTOF(syntax_error) - 1;
		HCL_MEMMOVE (&hcl->errmsg.buf[selen], &hcl->errmsg.buf[0], HCL_SIZEOF(hcl->errmsg.buf[0]) * (HCL_COUNTOF(hcl->errmsg.buf) - selen));
		for (i = 0; i < selen; i++) hcl->errmsg.buf[i] = syntax_error[i];
		hcl->errmsg.buf[HCL_COUNTOF(hcl->errmsg.buf) - 1] = '\0';
	}
	else
	{
		hcl_seterrbfmt (hcl, HCL_ESYNERR, "%hs%hs", syntax_error, synerr_to_errstr(num));
	}
	hcl->c->synerr.num = num;

	/* The SCO compiler complains of this ternary operation saying:
	 *    error: operands have incompatible types: op ":"
	 * it seems to complain of type mismatch between *loc and
	 * hcl->c->tok.loc due to 'const' prefixed to loc. */
	/*hcl->c->synerr.loc = loc? *loc: hcl->c->tok.loc;*/
	if (loc)
	{
		hcl->c->synerr.loc = *loc;
	}
	else
	{
		hcl->c->synerr.loc = hcl->c->tok.loc;
	}

	if (tgt)
	{
		hcl_oow_t n;
		n = hcl_copy_oochars_to_oocstr(hcl->c->synerr.tgt.val, HCL_COUNTOF(hcl->c->synerr.tgt.val), tgt->ptr, tgt->len);
		if (n < tgt->len)
		{
			hcl->c->synerr.tgt.val[n - 1] = '.';
			hcl->c->synerr.tgt.val[n - 2] = '.';
			hcl->c->synerr.tgt.val[n - 3] = '.';
		}
		hcl->c->synerr.tgt.len = n;
	}
	else
	{
		hcl->c->synerr.tgt.val[0] = '\0';
		hcl->c->synerr.tgt.len = 0;
	}
}

