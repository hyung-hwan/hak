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

#include "hak-prv.h"

#if defined(HAK_ENABLE_LIBUNWIND)
#	define UNW_LOCAL_ONLY
#	include <libunwind.h>
#endif

static hak_ooch_t errstr_0[] = {'n','o',' ','e','r','r','o','r','\0'};
static hak_ooch_t errstr_1[] = {'g','e','n','e','r','i','c',' ','e','r','r','o','r','\0'};
static hak_ooch_t errstr_2[] = {'n','o','t',' ','i','m','p','l','e','m','e','n','t','e','d','\0'};
static hak_ooch_t errstr_3[] = {'s','u','b','s','y','s','t','e','m',' ','e','r','r','o','r','\0'};
static hak_ooch_t errstr_4[] = {'i','n','t','e','r','n','a','l',' ','e','r','r','o','r',' ','t','h','a','t',' ','s','h','o','u','l','d',' ','n','e','v','e','r',' ','h','a','v','e',' ','h','a','p','p','e','n','e','d','\0'};

static hak_ooch_t errstr_5[] = {'i','n','s','u','f','f','i','c','i','e','n','t',' ','s','y','s','t','e','m',' ','m','e','m','o','r','y','\0'};
static hak_ooch_t errstr_6[] = {'i','n','s','u','f','f','i','c','i','e','n','t',' ','o','b','j','e','c','t',' ','m','e','m','o','r','y','\0'};
static hak_ooch_t errstr_7[] = {'i','n','v','a','l','i','d',' ','c','l','a','s','s','/','t','y','p','e','\0'};
static hak_ooch_t errstr_8[] = {'i','n','v','a','l','i','d',' ','p','a','r','a','m','e','t','e','r','/','a','r','g','u','m','e','n','t','\0'};
static hak_ooch_t errstr_9[] = {'d','a','t','a',' ','n','o','t',' ','f','o','u','n','d','\0'};

static hak_ooch_t errstr_10[] = {'e','x','i','s','t','i','n','g','/','d','u','p','l','i','c','a','t','e',' ','d','a','t','a','\0'};
static hak_ooch_t errstr_11[] = {'b','u','s','y','\0'};
static hak_ooch_t errstr_12[] = {'a','c','c','e','s','s',' ','d','e','n','i','e','d','\0'};
static hak_ooch_t errstr_13[] = {'o','p','e','r','a','t','i','o','n',' ','n','o','t',' ','p','e','r','m','i','t','t','e','d','\0'};
static hak_ooch_t errstr_14[] = {'n','o','t',' ','a',' ','d','i','r','e','c','t','o','r','y','\0'};

static hak_ooch_t errstr_15[] = {'i','n','t','e','r','r','u','p','t','e','d','\0'};
static hak_ooch_t errstr_16[] = {'p','i','p','e',' ','e','r','r','o','r','\0'};
static hak_ooch_t errstr_17[] = {'r','e','s','o','u','r','c','e',' ','t','e','m','p','o','r','a','r','i','l','y',' ','u','n','a','v','a','i','l','a','b','l','e','\0'};
static hak_ooch_t errstr_18[] = {'b','a','d',' ','s','y','s','t','e','m',' ','h','a','n','d','l','e','\0'};
static hak_ooch_t errstr_19[] = {'t','o','o',' ','m','a','n','y',' ','f','r','a','m','e','s','\0'};

static hak_ooch_t errstr_20[] = {'m','e','s','s','a','g','e',' ','r','e','c','e','i','v','e','r',' ','e','r','r','o','r','\0'};
static hak_ooch_t errstr_21[] = {'m','e','s','s','a','g','e',' ','s','e','n','d','i','n','g',' ','e','r','r','o','r','\0'};
static hak_ooch_t errstr_22[] = {'w','r','o','n','g',' ','n','u','m','b','e','r',' ','o','f',' ','a','r','g','u','m','e','n','t','s','\0'};
static hak_ooch_t errstr_23[] = {'r','a','n','g','e',' ','e','r','r','o','r','\0'};
static hak_ooch_t errstr_24[] = {'b','y','t','e','-','c','o','d','e',' ','f','u','l','l','\0'};

static hak_ooch_t errstr_25[] = {'d','i','c','t','i','o','n','a','r','y',' ','f','u','l','l','\0'};
static hak_ooch_t errstr_26[] = {'p','r','o','c','e','s','s','o','r',' ','f','u','l','l','\0'};
static hak_ooch_t errstr_27[] = {'n','o',' ','m','o','r','e',' ','i','n','p','u','t','\0'};
static hak_ooch_t errstr_28[] = {'t','o','o',' ','m','a','n','y',' ','i','t','e','m','s','\0'};
static hak_ooch_t errstr_29[] = {'d','i','v','i','d','e',' ','b','y',' ','z','e','r','o','\0'};

static hak_ooch_t errstr_30[] = {'I','/','O',' ','e','r','r','o','r','\0'};
static hak_ooch_t errstr_31[] = {'e','n','c','o','d','i','n','g',' ','c','o','n','v','e','r','s','i','o','n',' ','e','r','r','o','r','\0'};
static hak_ooch_t errstr_32[] = {'b','u','f','f','e','r',' ','f','u','l','l','\0'};
static hak_ooch_t errstr_33[] = {'s','y','n','t','a','x',' ','e','r','r','o','r','\0'};
static hak_ooch_t errstr_34[] = {'c','a','l','l',' ','e','r','r','o','r','\0'};

static hak_ooch_t errstr_35[] = {'a','r','g','u','m','e','n','t',' ','n','u','m','b','e','r',' ','e','r','r','o','r','\0'};
static hak_ooch_t errstr_36[] = {'r','e','t','u','r','n',' ','c','o','u','n','t',' ','e','r','r','o','r','\0'};
static hak_ooch_t errstr_37[] = {'t','o','o',' ','m','a','n','y',' ','s','e','m','a','p','h','o','r','e','s','\0'};
static hak_ooch_t errstr_38[] = {'e','x','c','e','p','a','i','o','n',' ','n','o','t',' ','h','a','n','d','l','e','d','\0'};
static hak_ooch_t errstr_39[] = {'s','t','a','c','k',' ','u','n','d','e','r','f','l','o','w','\0'};

static hak_ooch_t errstr_40[] = {'s','t','a','c','k',' ','o','v','e','r','f','l','o','w','\0'};
static hak_ooch_t errstr_41[] = {'u','n','d','e','f','i','n','e','d',' ','v','a','r','i','a','b','l','e',' ','a','c','c','e','s','s','\0'};

static hak_ooch_t* errstr[] =
{
	errstr_0, errstr_1, errstr_2, errstr_3, errstr_4, errstr_5, errstr_6, errstr_7,
	errstr_8, errstr_9, errstr_10, errstr_11, errstr_12, errstr_13, errstr_14, errstr_15,
	errstr_16, errstr_17, errstr_18, errstr_19, errstr_20, errstr_21, errstr_22, errstr_23,
	errstr_24, errstr_25, errstr_26, errstr_27, errstr_28, errstr_29, errstr_30, errstr_31,
	errstr_32, errstr_33, errstr_34, errstr_35, errstr_36, errstr_37, errstr_38, errstr_39,
	errstr_40, errstr_41
};


static const char* synerrstr[] =
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
	"invalid numeric literal",
	"out of integer range",
	"wrong error literal",
	"wrong smptr literal",
	"invalid radix for a numeric literal",

	"sudden end of input",
	"( expected",
	") expected",
	"] expected",
	"} expected",
	"| expected",

	"identifier expected",
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

	"invalid class definition",
	"invalid function definition",
	"invalid variable declaration",
	"elif without if",
	"else without if",
	"catch without try",
	"break outside loop",

	"invalid callable",
	"invalid message",
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

static hak_bch_t e_unknown_b[] = {'u','n','k','n','o','w','n',' ','e','r','r','o','r','\0'};
static hak_uch_t e_unknown_u[] = {'u','n','k','n','o','w','n',' ','e','r','r','o','r','\0'};
#if defined(HAK_OOCH_IS_BCH)
#	define e_unknown e_unknown_b
#else
#	define e_unknown e_unknown_u
#endif


int hak_errnum_is_synerr (hak_errnum_t errnum)
{
	return errnum == HAK_ESYNERR;
}

const hak_ooch_t* hak_errnum_to_errstr (hak_errnum_t errnum)
{
	return (errnum >= 0 && errnum < HAK_COUNTOF(errstr))? errstr[errnum]: e_unknown;
}

const hak_bch_t* hak_errnum_to_errbcstr (hak_errnum_t errnum, hak_bch_t* buf, hak_oow_t len)
{
	/* it's ok to copy without conversion because the messages above are simple acsii text */
#if defined(HAK_OOCH_IS_BCH)
	hak_copy_bcstr(buf, len, (errnum >= 0 && errnum < HAK_COUNTOF(errstr))? errstr[errnum]: e_unknown_b);
#else
	hak_copy_ucstr_to_bcstr(buf, len, (errnum >= 0 && errnum < HAK_COUNTOF(errstr))? errstr[errnum]: e_unknown_u);
#endif
	return buf;
}

const hak_uch_t* hak_errnum_to_errucstr (hak_errnum_t errnum, hak_uch_t* buf, hak_oow_t len)
{
	/* it's ok to copy without conversion because the messages above are simple acsii text */
#if defined(HAK_OOCH_IS_BCH)
	hak_copy_bcstr_to_ucstr(buf, len, (errnum >= 0 && errnum < HAK_COUNTOF(errstr))? errstr[errnum]: e_unknown_b);
#else
	hak_copy_ucstr(buf, len, (errnum >= 0 && errnum < HAK_COUNTOF(errstr))? errstr[errnum]: e_unknown_u);
#endif
	return buf;
}

static const hak_bch_t* synerr_to_errstr (hak_synerrnum_t errnum)
{
	return (errnum >= 0 && errnum < HAK_COUNTOF(synerrstr))? synerrstr[errnum]: e_unknown_b;
}

/* --------------------------------------------------------------------------
 * ERROR NUMBER/MESSAGE HANDLING
 * -------------------------------------------------------------------------- */

const hak_ooch_t* hak_geterrstr (hak_t* hak)
{
	return hak_errnum_to_errstr(hak->errnum);
}

/*
const hak_ooch_t* hak_geterrmsg (hak_t* hak)
{
	if (hak->errmsg.len <= 0) return hak_errnum_to_errstr(hak->errnum);
	return hak->errmsg.buf;
}
*/

const hak_bch_t* hak_geterrbmsg (hak_t* hak)
{
#if defined(HAK_OOCH_IS_BCH)
	return (hak->errmsg.len <= 0)? hak_errnum_to_errstr(hak->errnum): hak->errmsg.buf;
#else
	const hak_ooch_t* msg;
	hak_oow_t wcslen, mbslen;

	msg = (hak->errmsg.len <= 0)? hak_errnum_to_errstr(hak->errnum): hak->errmsg.buf;

	mbslen = HAK_COUNTOF(hak->errmsg.xerrmsg);
	hak_conv_ucstr_to_bcstr_with_cmgr(msg, &wcslen, hak->errmsg.xerrmsg, &mbslen, HAK_CMGR(hak));

	return hak->errmsg.xerrmsg;
#endif
}

const hak_uch_t* hak_geterrumsg (hak_t* hak)
{
#if defined(HAK_OOCH_IS_BCH)
	const hak_ooch_t* msg;
	hak_oow_t wcslen, mbslen;

	msg = (hak->errmsg.len <= 0)? hak_errnum_to_errstr(hak->errnum): hak->errmsg.buf;

	wcslen = HAK_COUNTOF(hak->errmsg.xerrmsg);
	hak_conv_bcstr_to_ucstr_with_cmgr(msg, &mbslen, hak->errmsg.xerrmsg, &wcslen, HAK_CMGR(hak), 1);

	return hak->errmsg.xerrmsg;
#else
	return (hak->errmsg.len == '\0')? hak_errnum_to_errstr(hak->errnum): hak->errmsg.buf;
#endif
}

void hak_geterrbinf (hak_t* hak, hak_errbinf_t* errinf)
{
#if defined(HAK_OOCH_IS_BCH)
	errinf->num = hak->errnum;
	errinf->loc = hak->errloc;
	hak_copy_oocstr(errinf->msg, HAK_COUNTOF(errinf->msg), (hak->errmsg[0] == '\0'? hak_errnum_to_errstr(hak->errnum): hak->errmsg));
#else
	const hak_ooch_t* msg;
	hak_oow_t wcslen, mbslen;

	/*errinf->num = hak->errnum;*/
	errinf->loc.line = hak->errloc.line;
	errinf->loc.colm = hak->errloc.colm;
	if (!hak->errloc.file) errinf->loc.file = HAK_NULL;
	else
	{
		mbslen = HAK_COUNTOF(hak->errmsg.xerrlocfile);
		hak_conv_ucstr_to_bcstr_with_cmgr(hak->errloc.file, &wcslen, hak->errmsg.xerrlocfile, &mbslen, hak->_cmgr);
		errinf->loc.file = hak->errmsg.xerrlocfile; /* this can be truncated and is transient */
	}

	msg = (hak->errmsg.buf[0] == '\0')? hak_errnum_to_errstr(hak->errnum): hak->errmsg.buf;
	mbslen = HAK_COUNTOF(errinf->msg);
	hak_conv_ucstr_to_bcstr_with_cmgr(msg, &wcslen, errinf->msg, &mbslen, hak->_cmgr);
#endif
}

void hak_geterruinf (hak_t* hak, hak_erruinf_t* errinf)
{
#if defined(HAK_OOCH_IS_BCH)
	const hak_ooch_t* msg;
	hak_oow_t wcslen, mbslen;

	/*errinf->num = hak->errnum;*/
	errinf->loc.line = hak->errloc.line;
	errinf->loc.colm = hak->errloc.colm;
	if (!hak->errloc.file) errinf->loc.file = HAK_NULL;
	else
	{
		wcslen = HAK_COUNTOF(hak->xerrlocfile);
		hak_conv_bcstr_to_ucstr_with_cmgr(hak->errloc.file, &mbslen, hak->xerrlocfile, &wcslen, hak->_cmgr, 1);
		errinf->loc.file = hak->xerrlocfile; /* this can be truncated and is transient */
	}

	msg = (hak->errmsg[0] == '\0')? hak_errnum_to_errstr(hak->errnum): hak->errmsg;
	wcslen = HAK_COUNTOF(errinf->msg);
	hak_conv_bcstr_to_ucstr_with_cmgr(msg, &mbslen, errinf->msg, &wcslen, hak->_cmgr, 1);
#else
	errinf->num = hak->errnum;
	errinf->loc = hak->errloc;
	hak_copy_oocstr(errinf->msg, HAK_COUNTOF(errinf->msg), (hak->errmsg.buf[0] == '\0'? hak_errnum_to_errstr(hak->errnum): hak->errmsg.buf));
#endif
}

hak_oow_t hak_copyerrbmsg (hak_t* hak, hak_bch_t* buf, hak_oow_t len)
{
	return hak_copy_bcstr(buf, len, hak_geterrbmsg(hak));
}

hak_oow_t hak_copyerrumsg (hak_t* hak, hak_uch_t* buf, hak_oow_t len)
{
	return hak_copy_ucstr(buf, len, hak_geterrumsg(hak));
}

const hak_ooch_t* hak_backuperrmsg (hak_t* hak)
{
	hak_copy_oocstr (hak->errmsg.tmpbuf.ooch, HAK_COUNTOF(hak->errmsg.tmpbuf.ooch), hak_geterrmsg(hak));
	return hak->errmsg.tmpbuf.ooch;
}

hak_errnum_t hak_geterrnum (hak_t* hak)
{
	return HAK_ERRNUM(hak);
}

void hak_seterrnum (hak_t* hak, hak_errnum_t errnum)
{
	if (hak->shuterr) return;
	hak->errnum = errnum;
	hak->errmsg.len = 0;
	HAK_MEMSET(&hak->errloc, 0, HAK_SIZEOF(hak->errloc));
}

void hak_geterrloc (hak_t* hak, hak_loc_t* loc)
{
	if (loc) *loc = hak->errloc;
}

void hak_seterrbmsg (hak_t* hak, hak_errnum_t errnum, const hak_bch_t* errmsg)
{
	hak_seterrbfmt(hak, errnum, "%hs", errmsg);
}

void hak_seterrumsg (hak_t* hak, hak_errnum_t errnum, const hak_uch_t* errmsg)
{
	hak_seterrbfmt(hak, errnum, "%ls", errmsg);
}

static int err_bcs (hak_t* hak, hak_fmtout_t* fmtout, const hak_bch_t* ptr, hak_oow_t len)
{
	hak_oow_t max;

	max = HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len - 1;

#if defined(HAK_OOCH_IS_UCH)
	if (max <= 0) return 1;
	hak_conv_bchars_to_uchars_with_cmgr(ptr, &len, &hak->errmsg.buf[hak->errmsg.len], &max, HAK_CMGR(hak), 1);
	hak->errmsg.len += max;
#else
	if (len > max) len = max;
	if (len <= 0) return 1;
	HAK_MEMCPY(&hak->errmsg.buf[hak->errmsg.len], ptr, len * HAK_SIZEOF(*ptr));
	hak->errmsg.len += len;
#endif

	hak->errmsg.buf[hak->errmsg.len] = '\0';

	return 1; /* success */
}

static int err_ucs (hak_t* hak, hak_fmtout_t* fmtout, const hak_uch_t* ptr, hak_oow_t len)
{
	hak_oow_t max;

	max = HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len - 1;

#if defined(HAK_OOCH_IS_UCH)
	if (len > max) len = max;
	if (len <= 0) return 1;
	HAK_MEMCPY(&hak->errmsg.buf[hak->errmsg.len], ptr, len * HAK_SIZEOF(*ptr));
	hak->errmsg.len += len;
#else
	if (max <= 0) return 1;
	hak_conv_uchars_to_bchars_with_cmgr(ptr, &len, &hak->errmsg.buf[hak->errmsg.len], &max, HAK_CMGR(hak));
	hak->errmsg.len += max;
#endif
	hak->errmsg.buf[hak->errmsg.len] = '\0';
	return 1; /* success */
}

void hak_seterrbfmt (hak_t* hak, hak_errnum_t errnum, const hak_bch_t* fmt, ...)
{
	va_list ap;
	hak_fmtout_t fo;

	if (hak->shuterr) return;
	hak->errmsg.len = 0;

	HAK_MEMSET(&fo, 0, HAK_SIZEOF(fo));
	fo.putbchars = err_bcs;
	fo.putuchars = err_ucs;
	fo.putobj = hak_fmt_object;

	va_start(ap, fmt);
	hak_bfmt_outv(hak, &fo, fmt, ap);
	va_end(ap);

	hak->errnum = errnum;
	HAK_MEMSET(&hak->errloc, 0, HAK_SIZEOF(hak->errloc));
}

void hak_seterrufmt (hak_t* hak, hak_errnum_t errnum, const hak_uch_t* fmt, ...)
{
	va_list ap;
	hak_fmtout_t fo;

	if (hak->shuterr) return;
	hak->errmsg.len = 0;

	HAK_MEMSET(&fo, 0, HAK_SIZEOF(fo));
	fo.putbchars = err_bcs;
	fo.putuchars = err_ucs;
	fo.putobj = hak_fmt_object;

	va_start(ap, fmt);
	hak_ufmt_outv(hak, &fo, fmt, ap);
	va_end(ap);

	hak->errnum = errnum;
	HAK_MEMSET(&hak->errloc, 0, HAK_SIZEOF(hak->errloc));
}


void hak_seterrbfmtv (hak_t* hak, hak_errnum_t errnum, const hak_bch_t* fmt, va_list ap)
{
	hak_fmtout_t fo;

	if (hak->shuterr) return;

	hak->errmsg.len = 0;

	HAK_MEMSET(&fo, 0, HAK_SIZEOF(fo));
	fo.putbchars = err_bcs;
	fo.putuchars = err_ucs;
	fo.putobj = hak_fmt_object;

	hak_bfmt_outv(hak, &fo, fmt, ap);
	hak->errnum = errnum;
	HAK_MEMSET(&hak->errloc, 0, HAK_SIZEOF(hak->errloc));
}

void hak_seterrufmtv (hak_t* hak, hak_errnum_t errnum, const hak_uch_t* fmt, va_list ap)
{
	hak_fmtout_t fo;

	if (hak->shuterr) return;

	hak->errmsg.len = 0;

	HAK_MEMSET(&fo, 0, HAK_SIZEOF(fo));
	fo.putbchars = err_bcs;
	fo.putuchars = err_ucs;
	fo.putobj = hak_fmt_object;

	hak_ufmt_outv(hak, &fo, fmt, ap);
	hak->errnum = errnum;
	HAK_MEMSET(&hak->errloc, 0, HAK_SIZEOF(hak->errloc));
}

void hak_seterrbfmtloc (hak_t* hak, hak_errnum_t errnum, const hak_loc_t* loc, const hak_bch_t* fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	hak_seterrbfmtv(hak, errnum, fmt, ap);
	va_end(ap);
	hak->errloc = *loc;
}

void hak_seterrufmtloc (hak_t* hak, hak_errnum_t errnum, const hak_loc_t* loc, const hak_uch_t* fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	hak_seterrufmtv(hak, errnum, fmt, ap);
	va_end(ap);
	hak->errloc = *loc;
}

void hak_seterrwithsyserr (hak_t* hak, int syserr_type, int syserr_code)
{
	hak_errnum_t errnum;

	if (hak->shuterr) return;

	if (hak->vmprim.syserrstrb)
	{
		errnum = hak->vmprim.syserrstrb(hak, syserr_type, syserr_code, hak->errmsg.tmpbuf.bch, HAK_COUNTOF(hak->errmsg.tmpbuf.bch));
		hak_seterrbfmt(hak, errnum, "%hs", hak->errmsg.tmpbuf.bch);
	}
	else
	{
		HAK_ASSERT(hak, hak->vmprim.syserrstru != HAK_NULL);
		errnum = hak->vmprim.syserrstru(hak, syserr_type, syserr_code, hak->errmsg.tmpbuf.uch, HAK_COUNTOF(hak->errmsg.tmpbuf.uch));
		hak_seterrbfmt(hak, errnum, "%ls", hak->errmsg.tmpbuf.uch);
	}
}

void hak_seterrbfmtwithsyserr (hak_t* hak, int syserr_type, int syserr_code, const hak_bch_t* fmt, ...)
{
	hak_errnum_t errnum;
	hak_oow_t ucslen, bcslen;
	va_list ap;

	if (hak->shuterr) return;

	if (hak->vmprim.syserrstrb)
	{
		errnum = hak->vmprim.syserrstrb(hak, syserr_type, syserr_code, hak->errmsg.tmpbuf.bch, HAK_COUNTOF(hak->errmsg.tmpbuf.bch));

		va_start(ap, fmt);
		hak_seterrbfmtv(hak, errnum, fmt, ap);
		va_end(ap);

		if (HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len >= 5)
		{
			hak->errmsg.buf[hak->errmsg.len++] = ' ';
			hak->errmsg.buf[hak->errmsg.len++] = '-';
			hak->errmsg.buf[hak->errmsg.len++] = ' ';

		#if defined(HAK_OOCH_IS_BCH)
			hak->errmsg.len += hak_copy_bcstr(&hak->errmsg.buf[hak->errmsg.len], HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len, hak->errmsg.tmpbuf.bch);
		#else
			ucslen = HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len;
			hak_convbtoucstr(hak, hak->errmsg.tmpbuf.bch, &bcslen, &hak->errmsg.buf[hak->errmsg.len], &ucslen);
			hak->errmsg.len += ucslen;
		#endif
		}
	}
	else
	{
		HAK_ASSERT(hak, hak->vmprim.syserrstru != HAK_NULL);
		errnum = hak->vmprim.syserrstru(hak, syserr_type, syserr_code, hak->errmsg.tmpbuf.uch, HAK_COUNTOF(hak->errmsg.tmpbuf.uch));

		va_start(ap, fmt);
		hak_seterrbfmtv(hak, errnum, fmt, ap);
		va_end(ap);

		if (HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len >= 5)
		{
			hak->errmsg.buf[hak->errmsg.len++] = ' ';
			hak->errmsg.buf[hak->errmsg.len++] = '-';
			hak->errmsg.buf[hak->errmsg.len++] = ' ';

		#if defined(HAK_OOCH_IS_BCH)
			bcslen = HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len;
			hak_convutobcstr(hak, hak->errmsg.tmpbuf.uch, &ucslen, &hak->errmsg.buf[hak->errmsg.len], &bcslen);
			hak->errmsg.len += bcslen;
		#else
			hak->errmsg.len += hak_copy_ucstr(&hak->errmsg.buf[hak->errmsg.len], HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len, hak->errmsg.tmpbuf.uch);
		#endif
		}
	}
}

void hak_seterrufmtwithsyserr (hak_t* hak, int syserr_type, int syserr_code, const hak_uch_t* fmt, ...)
{
	hak_errnum_t errnum;
	hak_oow_t ucslen, bcslen;
	va_list ap;

	if (hak->shuterr) return;

	if (hak->vmprim.syserrstrb)
	{
		errnum = hak->vmprim.syserrstrb(hak, syserr_type, syserr_code, hak->errmsg.tmpbuf.bch, HAK_COUNTOF(hak->errmsg.tmpbuf.bch));

		va_start(ap, fmt);
		hak_seterrufmtv(hak, errnum, fmt, ap);
		va_end(ap);

		if (HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len >= 5)
		{
			hak->errmsg.buf[hak->errmsg.len++] = ' ';
			hak->errmsg.buf[hak->errmsg.len++] = '-';
			hak->errmsg.buf[hak->errmsg.len++] = ' ';

		#if defined(HAK_OOCH_IS_BCH)
			hak->errmsg.len += hak_copy_bcstr(&hak->errmsg.buf[hak->errmsg.len], HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len, hak->errmsg.tmpbuf.bch);
		#else
			ucslen = HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len;
			hak_convbtoucstr(hak, hak->errmsg.tmpbuf.bch, &bcslen, &hak->errmsg.buf[hak->errmsg.len], &ucslen);
			hak->errmsg.len += ucslen;
		#endif
		}
	}
	else
	{
		HAK_ASSERT(hak, hak->vmprim.syserrstru != HAK_NULL);
		errnum = hak->vmprim.syserrstru(hak, syserr_type, syserr_code, hak->errmsg.tmpbuf.uch, HAK_COUNTOF(hak->errmsg.tmpbuf.uch));

		va_start(ap, fmt);
		hak_seterrufmtv(hak, errnum, fmt, ap);
		va_end(ap);

		if (HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len >= 5)
		{
			hak->errmsg.buf[hak->errmsg.len++] = ' ';
			hak->errmsg.buf[hak->errmsg.len++] = '-';
			hak->errmsg.buf[hak->errmsg.len++] = ' ';

		#if defined(HAK_OOCH_IS_BCH)
			bcslen = HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len;
			hak_convutobcstr(hak, hak->errmsg.tmpbuf.uch, &ucslen, &hak->errmsg.buf[hak->errmsg.len], &bcslen);
			hak->errmsg.len += bcslen;
		#else
			hak->errmsg.len += hak_copy_ucstr(&hak->errmsg.buf[hak->errmsg.len], HAK_COUNTOF(hak->errmsg.buf) - hak->errmsg.len, hak->errmsg.tmpbuf.uch);
		#endif
		}
	}
}

/* --------------------------------------------------------------------------
 * SYNTAX ERROR HANDLING
 * -------------------------------------------------------------------------- */

void hak_getsynerr (hak_t* hak, hak_synerr_t* synerr)
{
	HAK_ASSERT(hak, hak->c != HAK_NULL);
	if (synerr) *synerr = hak->c->synerr;
}

hak_synerrnum_t hak_getsynerrnum (hak_t* hak)
{
	HAK_ASSERT(hak, hak->c != HAK_NULL);
	return hak->c->synerr.num;
}

void hak_setsynerrbfmt (hak_t* hak, hak_synerrnum_t num, const hak_loc_t* loc, const hak_oocs_t* tgt, const hak_bch_t* msgfmt, ...)
{
	static hak_bch_t syntax_error[] = "syntax error - ";

	if (hak->shuterr) return;

	if (msgfmt)
	{
		va_list ap;
		int i, selen;

		va_start(ap, msgfmt);
		hak_seterrbfmtv(hak, HAK_ESYNERR, msgfmt, ap);
		va_end(ap);

		selen = HAK_COUNTOF(syntax_error) - 1;
		HAK_MEMMOVE(&hak->errmsg.buf[selen], &hak->errmsg.buf[0], HAK_SIZEOF(hak->errmsg.buf[0]) * (HAK_COUNTOF(hak->errmsg.buf) - selen));
		for (i = 0; i < selen; i++) hak->errmsg.buf[i] = syntax_error[i];
		hak->errmsg.buf[HAK_COUNTOF(hak->errmsg.buf) - 1] = '\0';
	}
	else
	{
		hak_seterrbfmt(hak, HAK_ESYNERR, "%hs%hs", syntax_error, synerr_to_errstr(num));
	}
	hak->c->synerr.num = num;

	/* The SCO compiler complains of this ternary operation saying:
	 *    error: operands have incompatible types: op ":"
	 * it seems to complain of type mismatch between *loc and
	 * hak->c->tok.loc due to 'const' prefixed to loc. */
	/*hak->c->synerr.loc = loc? *loc: hak->c->tok.loc;*/
	if (loc)
	{
		hak->c->synerr.loc = *loc;
	}
	else
	{
		hak->c->synerr.loc = hak->c->tok.loc;
	}

	if (tgt)
	{
		hak_oow_t n;
		n = hak_copy_oochars_to_oocstr(hak->c->synerr.tgt.val, HAK_COUNTOF(hak->c->synerr.tgt.val), tgt->ptr, tgt->len);
		if (n < tgt->len)
		{
			hak->c->synerr.tgt.val[n - 1] = '.';
			hak->c->synerr.tgt.val[n - 2] = '.';
			hak->c->synerr.tgt.val[n - 3] = '.';
		}
		hak->c->synerr.tgt.len = n;
	}
	else
	{
		hak->c->synerr.tgt.val[0] = '\0';
		hak->c->synerr.tgt.len = 0;
	}
}

void hak_setsynerrufmt (hak_t* hak, hak_synerrnum_t num, const hak_loc_t* loc, const hak_oocs_t* tgt, const hak_uch_t* msgfmt, ...)
{
	static hak_bch_t syntax_error[] = "syntax error - ";

	if (hak->shuterr) return;

	if (msgfmt)
	{
		va_list ap;
		int i, selen;

		va_start(ap, msgfmt);
		hak_seterrufmtv(hak, HAK_ESYNERR, msgfmt, ap);
		va_end(ap);

		selen = HAK_COUNTOF(syntax_error) - 1;
		HAK_MEMMOVE(&hak->errmsg.buf[selen], &hak->errmsg.buf[0], HAK_SIZEOF(hak->errmsg.buf[0]) * (HAK_COUNTOF(hak->errmsg.buf) - selen));
		for (i = 0; i < selen; i++) hak->errmsg.buf[i] = syntax_error[i];
		hak->errmsg.buf[HAK_COUNTOF(hak->errmsg.buf) - 1] = '\0';
	}
	else
	{
		hak_seterrbfmt(hak, HAK_ESYNERR, "%hs%hs", syntax_error, synerr_to_errstr(num));
	}
	hak->c->synerr.num = num;

	/* The SCO compiler complains of this ternary operation saying:
	 *    error: operands have incompatible types: op ":"
	 * it seems to complain of type mismatch between *loc and
	 * hak->c->tok.loc due to 'const' prefixed to loc. */
	/*hak->c->synerr.loc = loc? *loc: hak->c->tok.loc;*/
	if (loc)
	{
		hak->c->synerr.loc = *loc;
	}
	else
	{
		hak->c->synerr.loc = hak->c->tok.loc;
	}

	if (tgt)
	{
		hak_oow_t n;
		n = hak_copy_oochars_to_oocstr(hak->c->synerr.tgt.val, HAK_COUNTOF(hak->c->synerr.tgt.val), tgt->ptr, tgt->len);
		if (n < tgt->len)
		{
			hak->c->synerr.tgt.val[n - 1] = '.';
			hak->c->synerr.tgt.val[n - 2] = '.';
			hak->c->synerr.tgt.val[n - 3] = '.';
		}
		hak->c->synerr.tgt.len = n;
	}
	else
	{
		hak->c->synerr.tgt.val[0] = '\0';
		hak->c->synerr.tgt.len = 0;
	}
}

