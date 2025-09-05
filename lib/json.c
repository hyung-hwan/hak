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

#include <hak-json.h>
#include "hak-prv.h"

#include <string.h>
#include <errno.h>

#define HAK_JSON_TOKEN_NAME_ALIGN 64

struct json_hak_xtn_t
{
	hak_json_t* json;
};
typedef struct json_hak_xtn_t json_hak_xtn_t;


typedef struct hak_json_state_node_t hak_json_state_node_t;
struct hak_json_state_node_t
{
	hak_json_state_t state;
	union
	{
		struct
		{
			int got_value;
		} ia; /* in array */

		struct
		{
			/* 0: ready to get key (at the beginning or got comma),
			 * 1: got key, 2: got colon, 3: got value */
			int state;
		} id; /* in dictionary */
		struct
		{
			int escaped;
			int digit_count;
			/* acc is always of unicode type to handle \u and \U.
			 * in the bch mode, it will get converted to a utf8 stream. */
			hak_uch_t acc;
		} sv;
		struct
		{
			int escaped;
			int digit_count;
			/* for a character, no way to support the unicode character
			 * in the bch mode */
			hak_ooch_t acc;
		} cv;
		struct
		{
			int dotted;
		} nv;
	} u;
	hak_json_state_node_t* next;
};

struct hak_json_t
{
	hak_mmgr_t* mmgr;
	hak_cmgr_t* cmgr;
	hak_json_prim_t prim;
	hak_t* dummy_hak;

	hak_errnum_t errnum;
	struct
	{
		hak_ooch_t buf[HAK_ERRMSG_CAPA];
		hak_oow_t len;
	} errmsg;

	struct
	{
		hak_bitmask_t trait;
		hak_bitmask_t logmask;
	} cfg;

	hak_json_state_node_t state_top;
	hak_json_state_node_t* state_stack;

	hak_oocs_t tok;
	hak_oow_t tok_capa;
};


/* ========================================================================= */

static void log_write_for_dummy (hak_t* hak, hak_bitmask_t mask, const hak_ooch_t* msg, hak_oow_t len)
{
	json_hak_xtn_t* xtn = (json_hak_xtn_t*)hak_getxtn(hak);
	hak_json_t* json;

	json = xtn->json;
	json->prim.log_write (json, mask, msg, len);
}

/* ========================================================================= */

static HAK_INLINE int is_spacechar (hak_bch_t c)
{
	/* TODO: handle other space unicode characters */
	switch (c)
	{
		case ' ':
		case '\f': /* formfeed */
		case '\n': /* new line */
		case '\r': /* carriage return */
		case '\t': /* horizon tab */
		case '\v': /* vertical tab */
			return 1;

		default:
			return 0;
	}
}

static HAK_INLINE int is_alphachar (hak_ooci_t c)
{
/* TODO: support full unicode */
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static HAK_INLINE int is_digitchar (hak_ooci_t c)
{
/* TODO: support full unicode */
	return (c >= '0' && c <= '9');
}

static void clear_token (hak_json_t* json)
{
	json->tok.len = 0;
	if (json->tok_capa > 0) json->tok.ptr[json->tok.len] = '\0';
}

static int add_char_to_token (hak_json_t* json, hak_ooch_t ch)
{
	if (json->tok.len >= json->tok_capa)
	{
		hak_ooch_t* tmp;
		hak_oow_t newcapa;

		newcapa = HAK_ALIGN_POW2(json->tok.len + 2, HAK_JSON_TOKEN_NAME_ALIGN);  /* +2 here because of -1 when setting newcapa */
		tmp = (hak_ooch_t*)hak_json_reallocmem(json, json->tok.ptr, newcapa * HAK_SIZEOF(*tmp));
		if (!tmp) return -1;

		json->tok_capa = newcapa - 1; /* -1 to secure space for terminating null */
		json->tok.ptr = tmp;
	}

	json->tok.ptr[json->tok.len++] = ch;
	json->tok.ptr[json->tok.len] = '\0';
	return 0;
}

static int add_chars_to_token (hak_json_t* json, const hak_ooch_t* ptr, hak_oow_t len)
{
	hak_oow_t i;

	if (json->tok_capa - json->tok.len > len)
	{
		hak_ooch_t* tmp;
		hak_oow_t newcapa;

		newcapa = HAK_ALIGN_POW2(json->tok.len + len + 1, HAK_JSON_TOKEN_NAME_ALIGN);
		tmp = (hak_ooch_t*)hak_json_reallocmem(json, json->tok.ptr, newcapa * HAK_SIZEOF(*tmp));
		if (!tmp) return -1;

		json->tok_capa = newcapa - 1;
		json->tok.ptr = tmp;
	}

	for (i = 0; i < len; i++)
		json->tok.ptr[json->tok.len++] = ptr[i];
	json->tok.ptr[json->tok.len] = '\0';
	return 0;
}

static HAK_INLINE hak_ooch_t unescape (hak_ooch_t c)
{
	switch (c)
	{
		case 'a': return '\a';
		case 'b': return '\b';
		case 'f': return '\f';
		case 'n': return '\n';
		case 'r': return '\r';
		case 't': return '\t';
		case 'v': return '\v';
		default: return c;
	}
}

/* ========================================================================= */

static int push_state (hak_json_t* json, hak_json_state_t state)
{
	hak_json_state_node_t* ss;

	ss = (hak_json_state_node_t*)hak_json_callocmem(json, HAK_SIZEOF(*ss));
	if (!ss) return -1;

	ss->state = state;
	ss->next = json->state_stack;

	json->state_stack = ss;
	return 0;
}

static void pop_state (hak_json_t* json)
{
	hak_json_state_node_t* ss;

	ss = json->state_stack;
	HAK_ASSERT(json->dummy_hak, ss != HAK_NULL && ss != &json->state_top);
	json->state_stack = ss->next;

	if (json->state_stack->state == HAK_JSON_STATE_IN_ARRAY)
	{
		json->state_stack->u.ia.got_value = 1;
	}
	else if (json->state_stack->state == HAK_JSON_STATE_IN_DIC)
	{
		json->state_stack->u.id.state++;
	}

/* TODO: don't free this. move it to the free list? */
	hak_json_freemem (json, ss);
}

static void pop_all_states (hak_json_t* json)
{
	while (json->state_stack != &json->state_top) pop_state (json);
}

/* ========================================================================= */

static int invoke_data_inst (hak_json_t* json, hak_json_inst_t inst)
{
	if (json->state_stack->state == HAK_JSON_STATE_IN_DIC && json->state_stack->u.id.state == 1)
	{
		if (inst != HAK_JSON_INST_STRING)
		{
			hak_json_seterrbfmt (json, HAK_EINVAL, "dictionary key not a string - %.*js", json->tok.len, json->tok.ptr);
			return -1;
		}

		inst = HAK_JSON_INST_KEY;
	}

	if (json->prim.instcb(json, inst, &json->tok) <= -1) return -1;
	return 0;
}

static int handle_string_value_char (hak_json_t* json, hak_ooci_t c)
{
	int ret = 1;

	if (json->state_stack->u.sv.escaped == 3)
	{
		if (c >= '0' && c <= '7')
		{
			json->state_stack->u.sv.acc = json->state_stack->u.sv.acc * 8 + c - '0';
			json->state_stack->u.sv.digit_count++;
			if (json->state_stack->u.sv.digit_count >= json->state_stack->u.sv.escaped) goto add_sv_acc;
		}
		else
		{
			ret = 0;
			goto add_sv_acc;
		}
	}
	else if (json->state_stack->u.sv.escaped >= 2)
	{
		if (c >= '0' && c <= '9')
		{
			json->state_stack->u.sv.acc = json->state_stack->u.sv.acc * 16 + c - '0';
			json->state_stack->u.sv.digit_count++;
			if (json->state_stack->u.sv.digit_count >= json->state_stack->u.sv.escaped) goto add_sv_acc;
		}
		else if (c >= 'a' && c <= 'f')
		{
			json->state_stack->u.sv.acc = json->state_stack->u.sv.acc * 16 + c - 'a' + 10;
			json->state_stack->u.sv.digit_count++;
			if (json->state_stack->u.sv.digit_count >= json->state_stack->u.sv.escaped) goto add_sv_acc;
		}
		else if (c >= 'A' && c <= 'F')
		{
			json->state_stack->u.sv.acc = json->state_stack->u.sv.acc * 16 + c - 'A' + 10;
			json->state_stack->u.sv.digit_count++;
			if (json->state_stack->u.sv.digit_count >= json->state_stack->u.sv.escaped) goto add_sv_acc;
		}
		else
		{
			ret = 0;
		add_sv_acc:
		#if defined(HAK_OOCH_IS_UCH)
			if (add_char_to_token(json, json->state_stack->u.sv.acc) <= -1) return -1;
		#else
			/* convert the character to utf8 */
			{
				hak_bch_t bcsbuf[HAK_BCSIZE_MAX];
				hak_oow_t n;

				n = json->cmgr->uctobc(json->state_stack->u.sv.acc, bcsbuf, HAK_COUNTOF(bcsbuf));
				if (n == 0 || n > HAK_COUNTOF(bcsbuf))
				{
					/* illegal character or buffer to small */
					hak_json_seterrbfmt (json, HAK_EECERR, "unable to convert %jc", json->state_stack->u.sv.acc);
					return -1;
				}

				if (add_chars_to_token(json, bcsbuf, n) <= -1) return -1;
			}
		#endif
			json->state_stack->u.sv.escaped = 0;
		}
	}
	else if (json->state_stack->u.sv.escaped == 1)
	{
		if (c >= '0' && c <= '8')
		{
			json->state_stack->u.sv.escaped = 3;
			json->state_stack->u.sv.digit_count = 0;
			json->state_stack->u.sv.acc = c - '0';
		}
		else if (c == 'x')
		{
			json->state_stack->u.sv.escaped = 2;
			json->state_stack->u.sv.digit_count = 0;
			json->state_stack->u.sv.acc = 0;
		}
		else if (c == 'u')
		{
			json->state_stack->u.sv.escaped = 4;
			json->state_stack->u.sv.digit_count = 0;
			json->state_stack->u.sv.acc = 0;
		}
		else if (c == 'U')
		{
			json->state_stack->u.sv.escaped = 8;
			json->state_stack->u.sv.digit_count = 0;
			json->state_stack->u.sv.acc = 0;
		}
		else
		{
			json->state_stack->u.sv.escaped = 0;
			if (add_char_to_token(json, unescape(c)) <= -1) return -1;
		}
	}
	else if (c == '\\')
	{
		json->state_stack->u.sv.escaped = 1;
	}
	else if (c == '\"')
	{
		pop_state (json);
		if (invoke_data_inst(json, HAK_JSON_INST_STRING) <= -1) return -1;
	}
	else
	{
		if (add_char_to_token(json, c) <= -1) return -1;
	}

	return ret;
}

static int handle_character_value_char (hak_json_t* json, hak_ooci_t c)
{
	/* The real JSON dones't support character literal. this is HAK's own extension. */
	int ret = 1;

	if (json->state_stack->u.cv.escaped == 3)
	{
		if (c >= '0' && c <= '7')
		{
			json->state_stack->u.cv.acc = json->state_stack->u.cv.acc * 8 + c - '0';
			json->state_stack->u.cv.digit_count++;
			if (json->state_stack->u.cv.digit_count >= json->state_stack->u.cv.escaped) goto add_cv_acc;
		}
		else
		{
			ret = 0;
			goto add_cv_acc;
		}
	}
	if (json->state_stack->u.cv.escaped >= 2)
	{
		if (c >= '0' && c <= '9')
		{
			json->state_stack->u.cv.acc = json->state_stack->u.cv.acc * 16 + c - '0';
			json->state_stack->u.cv.digit_count++;
			if (json->state_stack->u.cv.digit_count >= json->state_stack->u.cv.escaped) goto add_cv_acc;
		}
		else if (c >= 'a' && c <= 'f')
		{
			json->state_stack->u.cv.acc = json->state_stack->u.cv.acc * 16 + c - 'a' + 10;
			json->state_stack->u.cv.digit_count++;
			if (json->state_stack->u.cv.digit_count >= json->state_stack->u.cv.escaped) goto add_cv_acc;
		}
		else if (c >= 'A' && c <= 'F')
		{
			json->state_stack->u.cv.acc = json->state_stack->u.cv.acc * 16 + c - 'A' + 10;
			json->state_stack->u.cv.digit_count++;
			if (json->state_stack->u.cv.digit_count >= json->state_stack->u.cv.escaped) goto add_cv_acc;
		}
		else
		{
			ret = 0;
		add_cv_acc:
			if (add_char_to_token(json, json->state_stack->u.cv.acc) <= -1) return -1;
			json->state_stack->u.cv.escaped = 0;
		}
	}
	else if (json->state_stack->u.cv.escaped == 1)
	{
		if (c >= '0' && c <= '8')
		{
			json->state_stack->u.cv.escaped = 3;
			json->state_stack->u.cv.digit_count = 0;
			json->state_stack->u.cv.acc = c - '0';
		}
		else if (c == 'x')
		{
			json->state_stack->u.cv.escaped = 2;
			json->state_stack->u.cv.digit_count = 0;
			json->state_stack->u.cv.acc = 0;
		}
		else if (c == 'u')
		{
			json->state_stack->u.cv.escaped = 4;
			json->state_stack->u.cv.digit_count = 0;
			json->state_stack->u.cv.acc = 0;
		}
		else if (c == 'U')
		{
			json->state_stack->u.cv.escaped = 8;
			json->state_stack->u.cv.digit_count = 0;
			json->state_stack->u.cv.acc = 0;
		}
		else
		{
			json->state_stack->u.cv.escaped = 0;
			if (add_char_to_token(json, unescape(c)) <= -1) return -1;
		}
	}
	else if (c == '\\')
	{
		json->state_stack->u.cv.escaped = 1;
	}
	else if (c == '\'')
	{
		pop_state (json);

		if (json->tok.len < 1)
		{
			hak_json_seterrbfmt (json, HAK_EINVAL, "no character in a character literal");
			return -1;
		}
		if (invoke_data_inst(json, HAK_JSON_INST_CHARACTER) <= -1) return -1;
	}
	else
	{
		if (add_char_to_token(json, c) <= -1) return -1;
	}

	if (json->tok.len > 1)
	{
		hak_json_seterrbfmt (json, HAK_EINVAL, "too many characters in a character literal - %.*js", json->tok.len, json->tok.ptr);
		return -1;
	}

	return ret;
}

static int handle_numeric_value_char (hak_json_t* json, hak_ooci_t c)
{
	if (is_digitchar(c) || (json->tok.len == 0 && (c == '+' || c == '-')))
	{
		if (add_char_to_token(json, c) <= -1) return -1;
		return 1;
	}
	else if (!json->state_stack->u.nv.dotted && c == '.' &&
	         json->tok.len > 0 && is_digitchar(json->tok.ptr[json->tok.len - 1]))
	{
		if (add_char_to_token(json, c) <= -1) return -1;
		json->state_stack->u.nv.dotted = 1;
		return 1;
	}

	pop_state (json);

	HAK_ASSERT(json->dummy_hak, json->tok.len > 0);
	if (!is_digitchar(json->tok.ptr[json->tok.len - 1]))
	{
		hak_json_seterrbfmt (json, HAK_EINVAL, "invalid numeric value - %.*js", json->tok.len, json->tok.ptr);
		return -1;
	}
	if (invoke_data_inst(json, HAK_JSON_INST_NUMBER) <= -1) return -1;
	return 0; /* start over */
}

static int handle_word_value_char (hak_json_t* json, hak_ooci_t c)
{
	hak_json_inst_t inst;

	if (is_alphachar(c))
	{
		if (add_char_to_token(json, c) <= -1) return -1;
		return 1;
	}

	pop_state (json);

	if (hak_comp_oochars_bcstr(json->tok.ptr, json->tok.len, "null") == 0) inst = HAK_JSON_INST_NIL;
	else if (hak_comp_oochars_bcstr(json->tok.ptr, json->tok.len, "true") == 0) inst = HAK_JSON_INST_TRUE;
	else if (hak_comp_oochars_bcstr(json->tok.ptr, json->tok.len, "false") == 0) inst = HAK_JSON_INST_FALSE;
	else
	{
		hak_json_seterrbfmt (json, HAK_EINVAL, "invalid word value - %.*js", json->tok.len, json->tok.ptr);
		return -1;
	}

	if (invoke_data_inst(json, inst) <= -1) return -1;
	return 0; /* start over */
}

/* ========================================================================= */

static int handle_start_char (hak_json_t* json, hak_ooci_t c)
{
	if (c == '[')
	{
		if (push_state(json, HAK_JSON_STATE_IN_ARRAY) <= -1) return -1;
		json->state_stack->u.ia.got_value = 0;
		if (json->prim.instcb(json, HAK_JSON_INST_START_ARRAY, HAK_NULL) <= -1) return -1;
		return 1;
	}
	else if (c == '{')
	{
		if (push_state(json, HAK_JSON_STATE_IN_DIC) <= -1) return -1;
		json->state_stack->u.id.state = 0;
		if (json->prim.instcb(json, HAK_JSON_INST_START_DIC, HAK_NULL) <= -1) return -1;
		return 1;
	}
	else if (is_spacechar(c))
	{
		/* do nothing */
		return 1;
	}
	else
	{
		hak_json_seterrbfmt (json, HAK_EINVAL, "not starting with [ or { - %jc", (hak_ooch_t)c);
		return -1;
	}
}

static int handle_char_in_array (hak_json_t* json, hak_ooci_t c)
{
	if (c == ']')
	{
		if (json->prim.instcb(json, HAK_JSON_INST_END_ARRAY, HAK_NULL) <= -1) return -1;
		pop_state (json);
		return 1;
	}
	else if (c == ',')
	{
		if (!json->state_stack->u.ia.got_value)
		{
			hak_json_seterrbfmt (json, HAK_EINVAL, "redundant comma in array - %jc", (hak_ooch_t)c);
			return -1;
		}
		json->state_stack->u.ia.got_value = 0;
		return 1;
	}
	else if (is_spacechar(c))
	{
		/* do nothing */
		return 1;
	}
	else
	{
		if (json->state_stack->u.ia.got_value)
		{
			hak_json_seterrbfmt (json, HAK_EINVAL, "comma required in array - %jc", (hak_ooch_t)c);
			return -1;
		}

		if (c == '\"')
		{
			if (push_state(json, HAK_JSON_STATE_IN_STRING_VALUE) <= -1) return -1;
			clear_token (json);
			return 1;
		}
		else if (c == '\'')
		{
			if (push_state(json, HAK_JSON_STATE_IN_CHARACTER_VALUE) <= -1) return -1;
			clear_token (json);
			return 1;
		}
		/* TOOD: else if (c == '#') HAK radixed number
		 */
		else if (is_digitchar(c) || c == '+' || c == '-')
		{
			if (push_state(json, HAK_JSON_STATE_IN_NUMERIC_VALUE) <= -1) return -1;
			clear_token (json);
			json->state_stack->u.nv.dotted = 0;
			return 0; /* start over */
		}
		else if (is_alphachar(c))
		{
			if (push_state(json, HAK_JSON_STATE_IN_WORD_VALUE) <= -1) return -1;
			clear_token (json);
			return 0; /* start over */
		}
		else if (c == '[')
		{
			if (push_state(json, HAK_JSON_STATE_IN_ARRAY) <= -1) return -1;
			json->state_stack->u.ia.got_value = 0;
			if (json->prim.instcb(json, HAK_JSON_INST_START_ARRAY, HAK_NULL) <= -1) return -1;
			return 1;
		}
		else if (c == '{')
		{
			if (push_state(json, HAK_JSON_STATE_IN_DIC) <= -1) return -1;
			json->state_stack->u.id.state = 0;
			if (json->prim.instcb(json, HAK_JSON_INST_START_DIC, HAK_NULL) <= -1) return -1;
			return 1;
		}
		else
		{
			hak_json_seterrbfmt (json, HAK_EINVAL, "wrong character inside array - %jc[%d]", (hak_ooch_t)c, (int)c);
			return -1;
		}
	}
}

static int handle_char_in_dic (hak_json_t* json, hak_ooci_t c)
{
	if (c == '}')
	{
		if (json->prim.instcb(json, HAK_JSON_INST_END_DIC, HAK_NULL) <= -1) return -1;
		pop_state (json);
		return 1;
	}
	else if (c == ':')
	{
		if (json->state_stack->u.id.state != 1)
		{
			hak_json_seterrbfmt (json, HAK_EINVAL, "redundant colon in dictionary - %jc", (hak_ooch_t)c);
			return -1;
		}
		json->state_stack->u.id.state++;
		return 1;
	}
	else if (c == ',')
	{
		if (json->state_stack->u.id.state != 3)
		{
			hak_json_seterrbfmt (json, HAK_EINVAL, "redundant comma in dicitonary - %jc", (hak_ooch_t)c);
			return -1;
		}
		json->state_stack->u.id.state = 0;
		return 1;
	}
	else if (is_spacechar(c))
	{
		/* do nothing */
		return 1;
	}
	else
	{
		if (json->state_stack->u.id.state == 1)
		{
			hak_json_seterrbfmt (json, HAK_EINVAL, "colon required in dicitonary - %jc", (hak_ooch_t)c);
			return -1;
		}
		else if (json->state_stack->u.id.state == 3)
		{
			hak_json_seterrbfmt (json, HAK_EINVAL, "comma required in dicitonary - %jc", (hak_ooch_t)c);
			return -1;
		}

		if (c == '\"')
		{
			if (push_state(json, HAK_JSON_STATE_IN_STRING_VALUE) <= -1) return -1;
			clear_token (json);
			return 1;
		}
		else if (c == '\'')
		{
			if (push_state(json, HAK_JSON_STATE_IN_CHARACTER_VALUE) <= -1) return -1;
			clear_token (json);
			return 1;
		}
		/* TOOD: else if (c == '#') HAK radixed number
		 */
		else if (is_digitchar(c) || c == '+' || c == '-')
		{
			if (push_state(json, HAK_JSON_STATE_IN_NUMERIC_VALUE) <= -1) return -1;
			clear_token (json);
			json->state_stack->u.nv.dotted = 0;
			return 0; /* start over */
		}
		else if (is_alphachar(c))
		{
			if (push_state(json, HAK_JSON_STATE_IN_WORD_VALUE) <= -1) return -1;
			clear_token (json);
			return 0; /* start over */
		}
		else if (c == '[')
		{
			if (push_state(json, HAK_JSON_STATE_IN_ARRAY) <= -1) return -1;
			json->state_stack->u.ia.got_value = 0;
			if (json->prim.instcb(json, HAK_JSON_INST_START_ARRAY, HAK_NULL) <= -1) return -1;
			return 1;
		}
		else if (c == '{')
		{
			if (push_state(json, HAK_JSON_STATE_IN_DIC) <= -1) return -1;
			json->state_stack->u.id.state = 0;
			if (json->prim.instcb(json, HAK_JSON_INST_START_DIC, HAK_NULL) <= -1) return -1;
			return 1;
		}
		else
		{
			hak_json_seterrbfmt (json, HAK_EINVAL, "wrong character inside dictionary - %jc[%d]", (hak_ooch_t)c, (int)c);
			return -1;
		}
	}
}

/* ========================================================================= */

static int handle_char (hak_json_t* json, hak_ooci_t c)
{
	int x;

start_over:
	if (c == HAK_OOCI_EOF)
	{
		if (json->state_stack->state == HAK_JSON_STATE_START)
		{
			/* no input data */
			return 0;
		}
		else
		{
			hak_json_seterrbfmt (json, HAK_EFINIS, "unexpected end of data");
			return -1;
		}
	}

	switch (json->state_stack->state)
	{
		case HAK_JSON_STATE_START:
			x = handle_start_char(json, c);
			break;

		case HAK_JSON_STATE_IN_ARRAY:
			x = handle_char_in_array(json, c);
			break;

		case HAK_JSON_STATE_IN_DIC:
			x = handle_char_in_dic(json, c);
			break;

		case HAK_JSON_STATE_IN_WORD_VALUE:
			x = handle_word_value_char(json, c);
			break;

		case HAK_JSON_STATE_IN_STRING_VALUE:
			x = handle_string_value_char(json, c);
			break;

		case HAK_JSON_STATE_IN_CHARACTER_VALUE:
			x = handle_character_value_char(json, c);
			break;

		case HAK_JSON_STATE_IN_NUMERIC_VALUE:
			x = handle_numeric_value_char(json, c);
			break;

		default:
			hak_json_seterrbfmt (json, HAK_EINTERN, "internal error - must not be called for state %d", (int)json->state_stack->state);
			return -1;
	}

	if (x <= -1) return -1;
	if (x == 0) goto start_over;

	return 0;
}

/* ========================================================================= */

static int feed_json_data_b (hak_json_t* json, const hak_bch_t* data, hak_oow_t len, hak_oow_t* xlen)
{
	const hak_bch_t* ptr;
	const hak_bch_t* end;

	ptr = data;
	end = ptr + len;

	while (ptr < end)
	{
		hak_ooci_t c;

	#if defined(HAK_OOCH_IS_UCH)
		hak_ooch_t uc;
		hak_oow_t bcslen;
		hak_oow_t n;

		bcslen = end - ptr;
		n = json->cmgr->bctouc(ptr, bcslen, &uc);
		if (n == 0)
		{
			/* invalid sequence */
			uc = *ptr;
			n = 1;
		}
		else if (n > bcslen)
		{
			/* incomplete sequence */
			*xlen = ptr - data;
			return 0; /* feed more for incomplete sequence */
		}

		ptr += n;
		c = uc;
	#else
		c = *ptr++;
	#endif

		/* handle a signle character */
		if (handle_char(json, c) <= -1) goto oops;
	}

	*xlen = ptr - data;
	return 1;

oops:
	/* TODO: compute the number of processed bytes so far and return it via a parameter??? */
/*printf ("feed oops....\n");*/
	return -1;
}

/* ========================================================================= */

static int feed_json_data_u (hak_json_t* json, const hak_uch_t* data, hak_oow_t len, hak_oow_t* xlen)
{
	const hak_uch_t* ptr;
	const hak_uch_t* end;

	ptr = data;
	end = ptr + len;

	while (ptr < end)
	{
		hak_ooci_t c;

	#if defined(HAK_OOCH_IS_UCH)
		c = *ptr++;
		/* handle a single character */
		if (handle_char(json, c) <= -1) goto oops;
	#else
		hak_bch_t bcsbuf[HAK_BCSIZE_MAX];
		hak_oow_t mlen = 0;
		hak_oow_t n, i;

		n = json->_gem.cmgr->uctobc(*ptr++, bcsbuf, HAK_COUNTOF(bcsbuf));
		if (n == 0) goto oops; // illegal character

		for (i = 0; i < n; i++)
		{
			if (handle_char(json, bcsbuf[i]) <= -1) goto oops;
		}
	#endif
	}

	*xlen = ptr - data;
	return 1;

oops:
	/* TODO: compute the number of processed bytes so far and return it via a parameter??? */
/*printf ("feed oops....\n");*/
	return -1;
}

/* ========================================================================= */

hak_json_t* hak_json_open (hak_mmgr_t* mmgr, hak_oow_t xtnsize, hak_json_prim_t* prim, hak_errnum_t* errnum)
{
	hak_json_t* json;
	hak_t* hak;
	json_hak_xtn_t* xtn;

	json = (hak_json_t*)HAK_MMGR_ALLOC(mmgr, HAK_SIZEOF(*json) + xtnsize);
	if (!json)
	{
		if (errnum) *errnum = HAK_ESYSMEM;
		return HAK_NULL;
	}

	hak = hak_openstdwithmmgr(mmgr, HAK_SIZEOF(*xtn), errnum);
	if (!hak)
	{
		HAK_MMGR_FREE (mmgr, json);
		return HAK_NULL;
	}

	hak->vmprim.log_write = log_write_for_dummy;

	xtn = (json_hak_xtn_t*)hak_getxtn(hak);
	xtn->json = json;

	HAK_MEMSET(json, 0, HAK_SIZEOF(*json) + xtnsize);
	json->mmgr = mmgr;
	json->cmgr = hak_get_utf8_cmgr();
	json->prim = *prim;
	json->dummy_hak = hak;

	json->cfg.logmask = ~(hak_bitmask_t)0;

	/* the dummy hak is used for this json to perform primitive operations
	 * such as getting system time or logging. so the heap size doesn't
	 * need to be changed from the tiny value set above. */
	hak_setoption(json->dummy_hak, HAK_LOG_MASK, &json->cfg.logmask);
	hak_setcmgr(json->dummy_hak, json->cmgr);


	json->state_top.state = HAK_JSON_STATE_START;
	json->state_top.next = HAK_NULL;
	json->state_stack = &json->state_top;

	return json;
}

void hak_json_close (hak_json_t* json)
{
	pop_all_states(json);
	if (json->tok.ptr) hak_json_freemem(json, json->tok.ptr);
	hak_close(json->dummy_hak);
	HAK_MMGR_FREE(json->mmgr, json);
}

int hak_json_setoption (hak_json_t* json, hak_json_option_t id, const void* value)
{
	switch (id)
	{
		case HAK_JSON_TRAIT:
			json->cfg.trait = *(const hak_bitmask_t*)value;
			return 0;

		case HAK_JSON_LOG_MASK:
			json->cfg.logmask = *(const hak_bitmask_t*)value;
			if (json->dummy_hak)
			{
				/* setting this affects the dummy hak immediately.
				 * existing hak instances inside worker threads won't get
				 * affected. new hak instances to be created later
				 * is supposed to use the new value */
				hak_setoption(json->dummy_hak, HAK_LOG_MASK, value);
			}
			return 0;
	}

	hak_json_seterrnum (json, HAK_EINVAL);
	return -1;
}

int hak_json_getoption (hak_json_t* json, hak_json_option_t id, void* value)
{
	switch (id)
	{
		case HAK_JSON_TRAIT:
			*(hak_bitmask_t*)value = json->cfg.trait;
			return 0;

		case HAK_JSON_LOG_MASK:
			*(hak_bitmask_t*)value = json->cfg.logmask;
			return 0;
	};

	hak_json_seterrnum (json, HAK_EINVAL);
	return -1;
}


void* hak_json_getxtn (hak_json_t* json)
{
	return (void*)(json + 1);
}

hak_mmgr_t* hak_json_getmmgr (hak_json_t* json)
{
	return json->mmgr;
}

hak_cmgr_t* hak_json_getcmgr (hak_json_t* json)
{
	return json->cmgr;
}

void hak_json_setcmgr (hak_json_t* json, hak_cmgr_t* cmgr)
{
	json->cmgr = cmgr;
}

hak_errnum_t hak_json_geterrnum (hak_json_t* json)
{
	return json->errnum;
}

const hak_ooch_t* hak_json_geterrstr (hak_json_t* json)
{
	return hak_errnum_to_errstr(json->errnum);
}

const hak_ooch_t* hak_json_geterrmsg (hak_json_t* json)
{
	if (json->errmsg.len <= 0) return hak_errnum_to_errstr(json->errnum);
	return json->errmsg.buf;
}

void hak_json_seterrnum (hak_json_t* json, hak_errnum_t errnum)
{
	/*if (json->shuterr) return; */
	json->errnum = errnum;
	json->errmsg.len = 0;
}

void hak_json_seterrbfmt (hak_json_t* json, hak_errnum_t errnum, const hak_bch_t* fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	hak_seterrbfmtv (json->dummy_hak, errnum, fmt, ap);
	va_end (ap);

	HAK_ASSERT(json->dummy_hak, HAK_COUNTOF(json->errmsg.buf) == HAK_COUNTOF(json->dummy_hak->errmsg.buf));
	json->errnum = errnum;
	hak_copy_oochars (json->errmsg.buf, json->dummy_hak->errmsg.buf, HAK_COUNTOF(json->errmsg.buf));
	json->errmsg.len = json->dummy_hak->errmsg.len;
}

void hak_json_seterrufmt (hak_json_t* json, hak_errnum_t errnum, const hak_uch_t* fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	hak_seterrufmtv (json->dummy_hak, errnum, fmt, ap);
	va_end (ap);

	HAK_ASSERT(json->dummy_hak, HAK_COUNTOF(json->errmsg.buf) == HAK_COUNTOF(json->dummy_hak->errmsg.buf));
	json->errnum = errnum;
	hak_copy_oochars (json->errmsg.buf, json->dummy_hak->errmsg.buf, HAK_COUNTOF(json->errmsg.buf));
	json->errmsg.len = json->dummy_hak->errmsg.len;
}

/* ========================================================================= */

void hak_json_logbfmt (hak_json_t* json, hak_bitmask_t mask, const hak_bch_t* fmt, ...)
{
	va_list ap;
	va_start (ap, fmt);
	hak_logbfmtv (json->dummy_hak, mask, fmt, ap);
	va_end (ap);
}

void hak_json_logufmt (hak_json_t* json, hak_bitmask_t mask, const hak_uch_t* fmt, ...)
{
	va_list ap;
	va_start (ap, fmt);
	hak_logufmtv (json->dummy_hak, mask, fmt, ap);
	va_end (ap);
}

/* ========================================================================= */

void* hak_json_allocmem (hak_json_t* json, hak_oow_t size)
{
	void* ptr;

	ptr = HAK_MMGR_ALLOC(json->mmgr, size);
	if (!ptr) hak_json_seterrnum (json, HAK_ESYSMEM);
	return ptr;
}

void* hak_json_callocmem (hak_json_t* json, hak_oow_t size)
{
	void* ptr;

	ptr = HAK_MMGR_ALLOC(json->mmgr, size);
	if (!ptr) hak_json_seterrnum (json, HAK_ESYSMEM);
	else HAK_MEMSET(ptr, 0, size);
	return ptr;
}

void* hak_json_reallocmem (hak_json_t* json, void* ptr, hak_oow_t size)
{
	ptr = HAK_MMGR_REALLOC(json->mmgr, ptr, size);
	if (!ptr) hak_json_seterrnum (json, HAK_ESYSMEM);
	return ptr;
}

void hak_json_freemem (hak_json_t* json, void* ptr)
{
	HAK_MMGR_FREE (json->mmgr, ptr);
}

/* ========================================================================= */

hak_json_state_t hak_json_getstate (hak_json_t* json)
{
	return json->state_stack->state;
}

void hak_json_reset (hak_json_t* json)
{
	/* TODO: reset XXXXXXXXXXXXXXXXXXXXXXXXXXXxxxxx */
	pop_all_states (json);
	HAK_ASSERT(json->dummy_hak, json->state_stack == &json->state_top);
	json->state_stack->state = HAK_JSON_STATE_START;
}

int hak_json_feedbchars (hak_json_t* json, const hak_bch_t* ptr, hak_oow_t len, hak_oow_t* xlen)
{
	int x;
	hak_oow_t total, ylen;

	total = 0;
	while (total < len)
	{
		x = feed_json_data_b(json, &ptr[total], len - total, &ylen);
		if (x <= -1) return -1;

		total += ylen;
		if (x == 0) break; /* incomplete sequence encountered */
	}

	*xlen = total;
	return 0;
}

int hak_json_feeduchars (hak_json_t* json, const hak_uch_t* ptr, hak_oow_t len, hak_oow_t* xlen)
{
	int x;
	hak_oow_t total, ylen;

	total = 0;
	while (total < len)
	{
		x = feed_json_data_u(json, &ptr[total], len - total, &ylen);
		if (x <= -1) return -1;

		total += ylen;
		if (x == 0) break; /* incomplete sequence encountered */
	}

	*xlen = total;
	return 0;
}
