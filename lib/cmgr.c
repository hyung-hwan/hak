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

#include <hak-cmgr.h>
#include <hak-chr.h>
#include <hak-str.h>

static hak_cmgr_t builtin_cmgr[] =
{
	/* keep the order aligned with hak_cmgr_id_t values in <hak-utl.h> */
	{ hak_utf8_to_uc,  hak_uc_to_utf8 },
	{ hak_utf16_to_uc, hak_uc_to_utf16 },
	{ hak_mb8_to_uc,   hak_uc_to_mb8 }
};

hak_cmgr_t* hak_get_cmgr_by_id (hak_cmgr_id_t id)
{
	return &builtin_cmgr[id];
}

static struct
{
	const hak_bch_t* name;
	hak_cmgr_id_t     id;
} builtin_cmgr_tab[] =
{
	{ "utf8",    HAK_CMGR_UTF8 },
	{ "utf16",   HAK_CMGR_UTF16 },
	{ "mb8",     HAK_CMGR_MB8 }
};

hak_cmgr_t* hak_get_cmgr_by_bcstr (const hak_bch_t* name)
{
	if (name)
	{
		hak_oow_t i;

		for (i = 0; i < HAK_COUNTOF(builtin_cmgr_tab); i++)
		{
			if (hak_comp_bcstr(name, builtin_cmgr_tab[i].name) == 0)
			{
				return &builtin_cmgr[builtin_cmgr_tab[i].id];
			}
		 }
	}

	return HAK_NULL;
}

hak_cmgr_t* hak_get_cmgr_by_ucstr (const hak_uch_t* name)
{
	if (name)
	{
		hak_oow_t i;

		for (i = 0; i < HAK_COUNTOF(builtin_cmgr_tab); i++)
		{
			if (hak_comp_ucstr_bcstr(name, builtin_cmgr_tab[i].name) == 0)
			{
				return &builtin_cmgr[builtin_cmgr_tab[i].id];
			}
		 }
	}

	return HAK_NULL;
}

/* ----------------------------------------------------------------------- */

int hak_conv_utf8_to_uchars (const hak_bch_t* bcs, hak_oow_t* bcslen, hak_uch_t* ucs, hak_oow_t* ucslen)
{
	/* the source is length bound */
	return hak_conv_bchars_to_uchars_with_cmgr(bcs, bcslen, ucs, ucslen, &builtin_cmgr[HAK_CMGR_UTF8], 0);
}

int hak_conv_uchars_to_utf8 (const hak_uch_t* ucs, hak_oow_t* ucslen, hak_bch_t* bcs, hak_oow_t* bcslen)
{
	/* length bound */
	return hak_conv_uchars_to_bchars_with_cmgr(ucs, ucslen, bcs, bcslen, &builtin_cmgr[HAK_CMGR_UTF8]);
}

int hak_conv_utf8_to_ucstr (const hak_bch_t* bcs, hak_oow_t* bcslen, hak_uch_t* ucs, hak_oow_t* ucslen)
{
	/* null-terminated. */
	return hak_conv_bcstr_to_ucstr_with_cmgr(bcs, bcslen, ucs, ucslen, &builtin_cmgr[HAK_CMGR_UTF8], 0);
}

int hak_conv_ucstr_to_utf8 (const hak_uch_t* ucs, hak_oow_t* ucslen, hak_bch_t* bcs, hak_oow_t* bcslen)
{
	/* null-terminated */
	return hak_conv_ucstr_to_bcstr_with_cmgr(ucs, ucslen, bcs, bcslen, &builtin_cmgr[HAK_CMGR_UTF8]);
}

/* ----------------------------------------------------------------------- */

int hak_conv_utf16_to_uchars (const hak_bch_t* bcs, hak_oow_t* bcslen, hak_uch_t* ucs, hak_oow_t* ucslen)
{
	/* the source is length bound */
	return hak_conv_bchars_to_uchars_with_cmgr(bcs, bcslen, ucs, ucslen, &builtin_cmgr[HAK_CMGR_UTF16], 0);
}

int hak_conv_uchars_to_utf16 (const hak_uch_t* ucs, hak_oow_t* ucslen, hak_bch_t* bcs, hak_oow_t* bcslen)
{
	/* length bound */
	return hak_conv_uchars_to_bchars_with_cmgr(ucs, ucslen, bcs, bcslen, &builtin_cmgr[HAK_CMGR_UTF16]);
}

int hak_conv_utf16_to_ucstr (const hak_bch_t* bcs, hak_oow_t* bcslen, hak_uch_t* ucs, hak_oow_t* ucslen)
{
	/* null-terminated. */
	return hak_conv_bcstr_to_ucstr_with_cmgr(bcs, bcslen, ucs, ucslen, &builtin_cmgr[HAK_CMGR_UTF16], 0);
}

int hak_conv_ucstr_to_utf16 (const hak_uch_t* ucs, hak_oow_t* ucslen, hak_bch_t* bcs, hak_oow_t* bcslen)
{
	/* null-terminated */
	return hak_conv_ucstr_to_bcstr_with_cmgr(ucs, ucslen, bcs, bcslen, &builtin_cmgr[HAK_CMGR_UTF16]);
}

/* ----------------------------------------------------------------------- */

int hak_conv_mb8_to_uchars (const hak_bch_t* bcs, hak_oow_t* bcslen, hak_uch_t* ucs, hak_oow_t* ucslen)
{
	/* the source is length bound */
	return hak_conv_bchars_to_uchars_with_cmgr(bcs, bcslen, ucs, ucslen, &builtin_cmgr[HAK_CMGR_MB8], 0);
}

int hak_conv_uchars_to_mb8 (const hak_uch_t* ucs, hak_oow_t* ucslen, hak_bch_t* bcs, hak_oow_t* bcslen)
{
	/* length bound */
	return hak_conv_uchars_to_bchars_with_cmgr(ucs, ucslen, bcs, bcslen, &builtin_cmgr[HAK_CMGR_MB8]);
}

int hak_conv_mb8_to_ucstr (const hak_bch_t* bcs, hak_oow_t* bcslen, hak_uch_t* ucs, hak_oow_t* ucslen)
{
	/* null-terminated. */
	return hak_conv_bcstr_to_ucstr_with_cmgr(bcs, bcslen, ucs, ucslen, &builtin_cmgr[HAK_CMGR_MB8], 0);
}

int hak_conv_ucstr_to_mb8 (const hak_uch_t* ucs, hak_oow_t* ucslen, hak_bch_t* bcs, hak_oow_t* bcslen)
{
	/* null-terminated */
	return hak_conv_ucstr_to_bcstr_with_cmgr(ucs, ucslen, bcs, bcslen, &builtin_cmgr[HAK_CMGR_MB8]);
}
