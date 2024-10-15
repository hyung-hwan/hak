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

#include <hcl-cmgr.h>
#include <hcl-chr.h>
#include <hcl-str.h>

static hcl_cmgr_t builtin_cmgr[] =
{
	/* keep the order aligned with hcl_cmgr_id_t values in <hcl-utl.h> */
	{ hcl_utf8_to_uc,  hcl_uc_to_utf8 },
	{ hcl_utf16_to_uc, hcl_uc_to_utf16 },
	{ hcl_mb8_to_uc,   hcl_uc_to_mb8 }
};

hcl_cmgr_t* hcl_get_cmgr_by_id (hcl_cmgr_id_t id)
{
	return &builtin_cmgr[id];
}

static struct
{
	const hcl_bch_t* name;
	hcl_cmgr_id_t     id;
} builtin_cmgr_tab[] =
{
	{ "utf8",    HCL_CMGR_UTF8 },
	{ "utf16",   HCL_CMGR_UTF16 },
	{ "mb8",     HCL_CMGR_MB8 }
};

hcl_cmgr_t* hcl_get_cmgr_by_bcstr (const hcl_bch_t* name)
{
	if (name)
	{
		hcl_oow_t i;

		for (i = 0; i < HCL_COUNTOF(builtin_cmgr_tab); i++)
		{
			if (hcl_comp_bcstr(name, builtin_cmgr_tab[i].name) == 0)
			{
				return &builtin_cmgr[builtin_cmgr_tab[i].id];
			}
		 }
	}

	return HCL_NULL;
}

hcl_cmgr_t* hcl_get_cmgr_by_ucstr (const hcl_uch_t* name)
{
	if (name)
	{
		hcl_oow_t i;

		for (i = 0; i < HCL_COUNTOF(builtin_cmgr_tab); i++)
		{
			if (hcl_comp_ucstr_bcstr(name, builtin_cmgr_tab[i].name) == 0)
			{
				return &builtin_cmgr[builtin_cmgr_tab[i].id];
			}
		 }
	}

	return HCL_NULL;
}

/* ----------------------------------------------------------------------- */

int hcl_conv_utf8_to_uchars (const hcl_bch_t* bcs, hcl_oow_t* bcslen, hcl_uch_t* ucs, hcl_oow_t* ucslen)
{
	/* the source is length bound */
	return hcl_conv_bchars_to_uchars_with_cmgr(bcs, bcslen, ucs, ucslen, &builtin_cmgr[HCL_CMGR_UTF8], 0);
}

int hcl_conv_uchars_to_utf8 (const hcl_uch_t* ucs, hcl_oow_t* ucslen, hcl_bch_t* bcs, hcl_oow_t* bcslen)
{
	/* length bound */
	return hcl_conv_uchars_to_bchars_with_cmgr(ucs, ucslen, bcs, bcslen, &builtin_cmgr[HCL_CMGR_UTF8]);
}

int hcl_conv_utf8_to_ucstr (const hcl_bch_t* bcs, hcl_oow_t* bcslen, hcl_uch_t* ucs, hcl_oow_t* ucslen)
{
	/* null-terminated. */
	return hcl_conv_bcstr_to_ucstr_with_cmgr(bcs, bcslen, ucs, ucslen, &builtin_cmgr[HCL_CMGR_UTF8], 0);
}

int hcl_conv_ucstr_to_utf8 (const hcl_uch_t* ucs, hcl_oow_t* ucslen, hcl_bch_t* bcs, hcl_oow_t* bcslen)
{
	/* null-terminated */
	return hcl_conv_ucstr_to_bcstr_with_cmgr(ucs, ucslen, bcs, bcslen, &builtin_cmgr[HCL_CMGR_UTF8]);
}

/* ----------------------------------------------------------------------- */

int hcl_conv_utf16_to_uchars (const hcl_bch_t* bcs, hcl_oow_t* bcslen, hcl_uch_t* ucs, hcl_oow_t* ucslen)
{
	/* the source is length bound */
	return hcl_conv_bchars_to_uchars_with_cmgr(bcs, bcslen, ucs, ucslen, &builtin_cmgr[HCL_CMGR_UTF16], 0);
}

int hcl_conv_uchars_to_utf16 (const hcl_uch_t* ucs, hcl_oow_t* ucslen, hcl_bch_t* bcs, hcl_oow_t* bcslen)
{
	/* length bound */
	return hcl_conv_uchars_to_bchars_with_cmgr(ucs, ucslen, bcs, bcslen, &builtin_cmgr[HCL_CMGR_UTF16]);
}

int hcl_conv_utf16_to_ucstr (const hcl_bch_t* bcs, hcl_oow_t* bcslen, hcl_uch_t* ucs, hcl_oow_t* ucslen)
{
	/* null-terminated. */
	return hcl_conv_bcstr_to_ucstr_with_cmgr(bcs, bcslen, ucs, ucslen, &builtin_cmgr[HCL_CMGR_UTF16], 0);
}

int hcl_conv_ucstr_to_utf16 (const hcl_uch_t* ucs, hcl_oow_t* ucslen, hcl_bch_t* bcs, hcl_oow_t* bcslen)
{
	/* null-terminated */
	return hcl_conv_ucstr_to_bcstr_with_cmgr(ucs, ucslen, bcs, bcslen, &builtin_cmgr[HCL_CMGR_UTF16]);
}

/* ----------------------------------------------------------------------- */

int hcl_conv_mb8_to_uchars (const hcl_bch_t* bcs, hcl_oow_t* bcslen, hcl_uch_t* ucs, hcl_oow_t* ucslen)
{
	/* the source is length bound */
	return hcl_conv_bchars_to_uchars_with_cmgr(bcs, bcslen, ucs, ucslen, &builtin_cmgr[HCL_CMGR_MB8], 0);
}

int hcl_conv_uchars_to_mb8 (const hcl_uch_t* ucs, hcl_oow_t* ucslen, hcl_bch_t* bcs, hcl_oow_t* bcslen)
{
	/* length bound */
	return hcl_conv_uchars_to_bchars_with_cmgr(ucs, ucslen, bcs, bcslen, &builtin_cmgr[HCL_CMGR_MB8]);
}

int hcl_conv_mb8_to_ucstr (const hcl_bch_t* bcs, hcl_oow_t* bcslen, hcl_uch_t* ucs, hcl_oow_t* ucslen)
{
	/* null-terminated. */
	return hcl_conv_bcstr_to_ucstr_with_cmgr(bcs, bcslen, ucs, ucslen, &builtin_cmgr[HCL_CMGR_MB8], 0);
}

int hcl_conv_ucstr_to_mb8 (const hcl_uch_t* ucs, hcl_oow_t* ucslen, hcl_bch_t* bcs, hcl_oow_t* bcslen)
{
	/* null-terminated */
	return hcl_conv_ucstr_to_bcstr_with_cmgr(ucs, ucslen, bcs, bcslen, &builtin_cmgr[HCL_CMGR_MB8]);
}
