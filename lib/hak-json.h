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

#ifndef _HAK_JSON_H_
#define _HAK_JSON_H_

#include <hak.h>

/**
 * The hak_json_t type defines a simple json parser.
 */
typedef struct hak_json_t hak_json_t;

enum hak_json_option_t
{
	HAK_JSON_TRAIT,
	HAK_JSON_LOG_MASK,
};
typedef enum hak_json_option_t hak_json_option_t;

enum hak_json_trait_t
{
	/* no trait defined at this moment. XXXX is just a placeholder */
	HAK_JSON_XXXX  = (1 << 0)
};
typedef enum hak_json_trait_t hak_json_trait_t;

/* ========================================================================= */

enum hak_json_state_t
{
	HAK_JSON_STATE_START,
	HAK_JSON_STATE_IN_ARRAY,
	HAK_JSON_STATE_IN_DIC,

	HAK_JSON_STATE_IN_WORD_VALUE,
	HAK_JSON_STATE_IN_NUMERIC_VALUE,
	HAK_JSON_STATE_IN_STRING_VALUE,
	HAK_JSON_STATE_IN_CHARACTER_VALUE
};
typedef enum hak_json_state_t hak_json_state_t;


/* ========================================================================= */
enum hak_json_inst_t
{
	HAK_JSON_INST_START_ARRAY,
	HAK_JSON_INST_END_ARRAY,
	HAK_JSON_INST_START_DIC,
	HAK_JSON_INST_END_DIC,

	HAK_JSON_INST_KEY,

	HAK_JSON_INST_CHARACTER, /* there is no such element as character in real JSON */
	HAK_JSON_INST_STRING,
	HAK_JSON_INST_NUMBER,
	HAK_JSON_INST_NIL,
	HAK_JSON_INST_TRUE,
	HAK_JSON_INST_FALSE,
};
typedef enum hak_json_inst_t hak_json_inst_t;

typedef void (*hak_json_log_write_t) (
	hak_json_t*       json,
	hak_bitmask_t   mask,
	const hak_ooch_t* msg,
	hak_oow_t         len
);

typedef int (*hak_json_instcb_t) (
	hak_json_t*           json,
	hak_json_inst_t       inst,
	const hak_oocs_t*     str
);

struct hak_json_prim_t
{
	hak_json_log_write_t     log_write;
	hak_json_instcb_t        instcb;
};
typedef struct hak_json_prim_t hak_json_prim_t;

/* ========================================================================= */

#if defined(__cplusplus)
extern "C" {
#endif

HAK_EXPORT hak_json_t* hak_json_open (
	hak_mmgr_t*        mmgr,
	hak_oow_t          xtnsize,
	hak_json_prim_t*   prim,
	hak_errinf_t*      errinf
);

HAK_EXPORT void hak_json_close (
	hak_json_t* json
);

HAK_EXPORT void hak_json_reset (
	hak_json_t* json
);

HAK_EXPORT int hak_json_feedbchars (
	hak_json_t*      json,
	const hak_bch_t* ptr,
	hak_oow_t        len,
	hak_oow_t*       xlen
);

HAK_EXPORT int hak_json_feeduchars (
	hak_json_t*      json,
	const hak_uch_t* ptr,
	hak_oow_t        len,
	hak_oow_t*       xlen
);


#if defined(HAK_OOCH_IS_UCH)
#	define hak_json_feed hak_json_feeduchars
#else
#	define hak_json_feed hak_json_feedbchars
#endif


HAK_EXPORT hak_json_state_t hak_json_getstate (
	hak_json_t* json
);

HAK_EXPORT int hak_json_setoption (
	hak_json_t*         json,
	hak_json_option_t   id,
	const void*         value
);

HAK_EXPORT int hak_json_getoption (
	hak_json_t*         json,
	hak_json_option_t   id,
	void*               value
);


HAK_EXPORT void* hak_json_getxtn (
	hak_json_t* json
);

HAK_EXPORT hak_mmgr_t* hak_json_getmmgr (
	hak_json_t* json
);

HAK_EXPORT hak_cmgr_t* hak_json_getcmgr (
	hak_json_t* json
);

HAK_EXPORT void hak_json_setcmgr (
	hak_json_t* json,
	hak_cmgr_t*   cmgr
);


HAK_EXPORT hak_errnum_t hak_json_geterrnum (
	hak_json_t* json
);

HAK_EXPORT const hak_ooch_t* hak_json_geterrstr (
	hak_json_t* json
);

HAK_EXPORT const hak_ooch_t* hak_json_geterrmsg (
	hak_json_t* json
);

HAK_EXPORT void hak_json_seterrnum (
	hak_json_t* json,
	hak_errnum_t  errnum
);

HAK_EXPORT void hak_json_seterrbfmt (
	hak_json_t*    json,
	hak_errnum_t     errnum,
	const hak_bch_t* fmt,
	...
);

HAK_EXPORT void hak_json_seterrufmt (
	hak_json_t*    json,
	hak_errnum_t     errnum,
	const hak_uch_t* fmt,
	...
);

HAK_EXPORT void hak_json_logbfmt (
	hak_json_t*      json,
	hak_bitmask_t  mask,
	const hak_bch_t* fmt,
	...
);

HAK_EXPORT void hak_json_logufmt (
	hak_json_t*      json,
	hak_bitmask_t  mask,
	const hak_uch_t* fmt,
	...
);

HAK_EXPORT void* hak_json_allocmem (
	hak_json_t* json,
	hak_oow_t     size
);

HAK_EXPORT void* hak_json_callocmem (
	hak_json_t* json,
	hak_oow_t     size
);

HAK_EXPORT void* hak_json_reallocmem (
	hak_json_t* json,
	void*         ptr,
	hak_oow_t     size
);

HAK_EXPORT void hak_json_freemem (
	hak_json_t* json,
	void*         ptr
);

#if defined(__cplusplus)
}
#endif

#endif
