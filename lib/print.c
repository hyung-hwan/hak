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


#include <stdio.h>
#define PRINT_STACK_ALIGN 128

enum
{
	PRINT_STACK_CONS,
	PRINT_STACK_ARRAY,
	PRINT_STACK_ARRAY_END,
	PRINT_STACK_DIC,
	PRINT_STACK_DIC_END
};

typedef struct print_stack_t print_stack_t;
struct print_stack_t
{
	int type;
	hak_oop_t obj;
	hak_oop_t obj2;
	hak_oow_t idx;
	hak_oow_t idx2;
};

static HAK_INLINE int push (hak_t* hak, print_stack_t* info)
{
	if (hak->p.s.size >= hak->p.s.capa)
	{
		print_stack_t* tmp;
		hak_oow_t new_capa;

		new_capa = HAK_ALIGN (hak->p.s.capa + 1, PRINT_STACK_ALIGN);
		tmp = (print_stack_t*)hak_reallocmem(hak, hak->p.s.ptr, new_capa * HAK_SIZEOF(*info));
		if (!tmp) return -1;

		hak->p.s.ptr = tmp;
		hak->p.s.capa = new_capa;
	}

	((print_stack_t*)hak->p.s.ptr)[hak->p.s.size] = *info;
	hak->p.s.size++;

	return 0;
}

static HAK_INLINE void pop (hak_t* hak, print_stack_t* info)
{
	HAK_ASSERT(hak, hak->p.s.size > 0);
	hak->p.s.size--;
	*info = ((print_stack_t*)hak->p.s.ptr)[hak->p.s.size];
}

enum
{
	WORD_UNDEF,
	WORD_NIL,
	WORD_TRUE,
	WORD_FALSE,

	WORD_SET,
	WORD_PRIM,

	WORD_FUNCTION,
	WORD_BLOCK,
	WORD_CONTEXT,
	WORD_PROCESS,
	WORD_PROCESS_SCHEDULER,
	WORD_SEMAPHORE,
	WORD_SEMAPHORE_GROUP,
	WORD_CLASS,
	WORD_INSTANCE
};

static struct
{
	hak_oow_t  len;
	hak_ooch_t ptr[20];
} word[] =
{
	{  8,  { '#','<','U','N','D','E','F','>' } },
	{  3,  { 'n','i','l' } },
	{  4,  { 't','r','u','e' } },
	{  5,  { 'f','a','l','s','e' } },

	{  6,  { '#','<','S','E','T','>' } },
	{  7,  { '#','<','P','R','I','M','>' } },

	{  11, { '#','<','F','U','N','C','T','I','O','N','>' } },
	{  8,  { '#','<','B','L','O','C','K','>' } },
	{  10, { '#','<','C','O','N','T','E','X','T','>' } },
	{  10, { '#','<','P','R','O','C','E','S','S','>' } },
	{  20, { '#','<','P','R','O','C','E','S','S','-','S','C','H','E','D','U','L','E','R','>' } },
	{  12, { '#','<','S','E','M','A','P','H','O','R','E','>' } },
	{  18, { '#','<','S','E','M','A','P','H','O','R','E','-','G','R','O','U','P','>' } },

	{  8,  { '#','<','C','L','A','S','S','>' } },
	{  11, { '#','<','I','N','S','T','A','N','C','E','>' } }
};

static HAK_INLINE int print_single_char (hak_t* hak, hak_fmtout_t* fmtout, hak_ooch_t ch)
{
	hak_oochu_t chu = (hak_oochu_t)ch;
	if (chu == '\\' || chu == '\"')
	{
		if (hak_bfmt_out(hak, fmtout, "\\%jc", chu) <= -1) return -1;
	}
#if defined(HAK_OOCH_IS_UCH)
	else if (chu < ' ')
#else
	else if (chu < ' ' || chu >= 0x80)
#endif
	{
		hak_oochu_t escaped;

		switch (chu)
		{
			case '\0':
				escaped = '0';
				break;
			case '\n':
				escaped = 'n';
				break;
			case '\r':
				escaped = 'r';
				break;
			case '\t':
				escaped = 't';
				break;
			case '\f':
				escaped = 'f';
				break;
			case '\b':
				escaped = 'b';
				break;
			case '\v':
				escaped = 'v';
				break;
			case '\a':
				escaped = 'a';
				break;
			default:
				escaped = chu;
				break;
		}

		if (escaped == chu)
		{
		#if (HAK_SIZEOF_OOCH_T >= 4)
			if (chu >= 0x10000u)
			{
				if (hak_bfmt_out(fmtout, "\\U%08X", chu) <= -1) return -1;
			}
			else
		#endif
			{
		#if (HAK_SIZEOF_OOCH_T >= 2)
				if (chu >= 0x100u)
				{
					if (hak_bfmt_out(hak, fmtout, "\\u%04X", chu) <= -1) return -1;
				}
				else
		#endif
				{
					if (hak_bfmt_out(hak, fmtout, "\\x%02X", chu) <= -1) return -1;
				}
			}
		}
		else
		{
			if (hak_bfmt_out(hak, fmtout, "\\%jc", escaped) <= -1) return -1;
		}
	}
	else
	{
		if (hak_bfmt_out(hak, fmtout, "%jc", ch) <= -1) return -1;
	}

	return 0;
}

static HAK_INLINE int print_single_byte_char (hak_t* hak, hak_fmtout_t* fmtout, hak_bch_t ch)
{
	hak_bchu_t chu = (hak_bchu_t)ch;
	if (chu == '\\' || chu == '\"')
	{
		if (hak_bfmt_out(hak, fmtout, "\\%hc", chu) <= -1) return -1;
	}
#if defined(HAK_OOCH_IS_UCH)
	else if (chu < ' ')
#else
	else if (chu < ' ' || chu >= 0x80)
#endif
	{
		hak_bchu_t escaped;

		switch (chu)
		{
			case '\0':
				escaped = '0';
				break;
			case '\n':
				escaped = 'n';
				break;
			case '\r':
				escaped = 'r';
				break;
			case '\t':
				escaped = 't';
				break;
			case '\f':
				escaped = 'f';
				break;
			case '\b':
				escaped = 'b';
				break;
			case '\v':
				escaped = 'v';
				break;
			case '\a':
				escaped = 'a';
				break;
			default:
				escaped = chu;
				break;
		}

		if (escaped == chu)
		{
			if (hak_bfmt_out(hak, fmtout, "\\x%02X", chu) <= -1) return -1;
		}
		else
		{
			if (hak_bfmt_out(hak, fmtout, "\\%hc", escaped) <= -1) return -1;
		}
	}
	else
	{
		if (hak_bfmt_out(hak, fmtout, "%hc", ch) <= -1) return -1;
	}

	return 0;
}

int hak_fmt_object (hak_t* hak, hak_fmtout_t* fmtout, hak_oop_t obj)
{
	hak_oop_t cur;
	print_stack_t ps;
	hak_oop_class_t _class;
	int brand;
	int word_index;
	int json;

	static const hak_bch_t *opening_parens[][2] =
	{
		                            /* navtive   json */
		HAK_AID(HAK_CONCODE_XLIST)     { "(",     "(" },
		HAK_AID(HAK_CONCODE_MLIST)     { "(",     "(" },
		HAK_AID(HAK_CONCODE_ALIST)     { "(",     "(" },
		HAK_AID(HAK_CONCODE_BLIST)     { "(",     "(" },
		HAK_AID(HAK_CONCODE_BLOCK)     { "{",     "{" },
		HAK_AID(HAK_CONCODE_ARRAY)     { "#[",    "[" },
		HAK_AID(HAK_CONCODE_BYTEARRAY) { "#b[",   "[" },
		HAK_AID(HAK_CONCODE_CHARARRAY) { "#c[",   "[" },
		HAK_AID(HAK_CONCODE_DIC)       { "#{",    "{" },
		HAK_AID(HAK_CONCODE_QLIST)     { "#(",    "[" },
		HAK_AID(HAK_CONCODE_TUPLE)     { "[",     "[" }
	};

	static const hak_bch_t *closing_parens[][2] =
	{
		HAK_AID(HAK_CONCODE_XLIST)     { ")",     ")" },
		HAK_AID(HAK_CONCODE_MLIST)     { ")",     ")" },
		HAK_AID(HAK_CONCODE_ALIST)     { ")",     ")" },
		HAK_AID(HAK_CONCODE_BLIST)     { ")",     ")" },
		HAK_AID(HAK_CONCODE_BLOCK)     { "}",     "}" },
		HAK_AID(HAK_CONCODE_ARRAY)     { "]",     "]" },
		HAK_AID(HAK_CONCODE_BYTEARRAY) { "]",     "]" },
		HAK_AID(HAK_CONCODE_CHARARRAY) { "]",     "]" },
		HAK_AID(HAK_CONCODE_DIC)       { "}",     "}" },
		HAK_AID(HAK_CONCODE_QLIST)     { ")",     "]" },
		HAK_AID(HAK_CONCODE_TUPLE)     { "]",     "]" }
	};

	static const hak_bch_t* breakers[][2] =
	{
		{ " ",       "," }, /* item breaker */
		{ " ",       ":" }  /* key value breaker */
	};

	json = !!(fmtout->mask & HAK_LOG_PREFER_JSON);

next:
	_class = (hak_oop_class_t)HAK_CLASSOF(hak, obj);
	brand = HAK_OOP_TO_SMOOI(_class->ibrand);
	switch (brand)
	{
		case HAK_BRAND_SMOOI:
			if (hak_bfmt_out(hak, fmtout, "%zd", HAK_OOP_TO_SMOOI(obj)) <= -1) return -1;
			goto done;

		case HAK_BRAND_SMPTR:
			if (hak_bfmt_out(hak, fmtout, "#p%zX", (hak_oow_t)HAK_OOP_TO_SMPTR(obj)) <= -1) return -1;
			goto done;

		case HAK_BRAND_ERROR:
			if (hak_bfmt_out(hak, fmtout, "#e%zd", (hak_ooi_t)HAK_OOP_TO_ERROR(obj)) <= -1) return -1;
			goto done;

		case HAK_BRAND_CHARACTER:
		{
			hak_ooch_t ch = HAK_OOP_TO_CHAR(obj);
			if (hak_bfmt_out(hak, fmtout, "\'") <= -1 ||
			    print_single_char(hak, fmtout, ch) <= -1 ||
			    hak_bfmt_out(hak, fmtout, "\'") <= -1) return -1;
			goto done;
		}

		case HAK_BRAND_UNDEF:
			word_index = WORD_UNDEF;
			goto print_word;

		case HAK_BRAND_NIL:
			word_index = WORD_NIL;
			goto print_word;

		case HAK_BRAND_TRUE:
			word_index = WORD_TRUE;
			goto print_word;

		case HAK_BRAND_FALSE:
			word_index = WORD_FALSE;
			goto print_word;

		case HAK_BRAND_PBIGINT:
		case HAK_BRAND_NBIGINT:
		{
			hak_oop_t tmp;

			/* -1 to drive hak_inttostr() to not create a new string object.
			 * not using the object memory. the result stays in the temporary
			 * buffer */
			tmp = hak_inttostr(hak, obj, 10 | HAK_INTTOSTR_NONEWOBJ);
			if (!tmp) return -1;

			HAK_ASSERT(hak, (hak_oop_t)tmp == hak->_nil);
			if (hak_bfmt_out(hak, fmtout, "%.*js", hak->inttostr.xbuf.len, hak->inttostr.xbuf.ptr) <= -1) return -1;
			break;
		}

		case HAK_BRAND_FPDEC:
		{
			hak_oop_fpdec_t f = (hak_oop_fpdec_t)obj;
			hak_ooi_t scale;

			scale = HAK_OOP_TO_SMOOI(f->scale);

			if (f->value == HAK_SMOOI_TO_OOP(0))
			{
				if (scale == 0)
				{
					if (hak_bfmt_out(hak, fmtout, "0.") <= -1) return -1;
				}
				else
				{
					if (hak_bfmt_out(hak, fmtout, "0.%0*d", scale, 0) <= -1) return -1;
				}
			}
			else
			{
				hak_oop_t tmp;
				hak_oow_t len, adj;

				tmp = hak_inttostr(hak, f->value, 10 | HAK_INTTOSTR_NONEWOBJ);
				if (!tmp) return -1;

				adj = (hak->inttostr.xbuf.ptr[0] == '-');
				len = hak->inttostr.xbuf.len - adj;

				if (len <= scale)
				{
					if (scale == len)
					{
						if (hak_bfmt_out(hak, fmtout, "%.*js0.%.*js",
							adj, hak->inttostr.xbuf.ptr,
							len, &hak->inttostr.xbuf.ptr[adj]) <= -1) return -1;
					}
					else
					{
						if (hak_bfmt_out(hak, fmtout, "%.*js0.%0*d%.*js",
							adj, hak->inttostr.xbuf.ptr,
							scale - len, 0,
							len, &hak->inttostr.xbuf.ptr[adj]) <= -1) return -1;
					}
				}
				else
				{
					hak_ooi_t ndigits;
					ndigits = hak->inttostr.xbuf.len - scale;
					if (hak_bfmt_out(hak, fmtout, "%.*js.%.*js", ndigits, hak->inttostr.xbuf.ptr, scale, &hak->inttostr.xbuf.ptr[ndigits]) <= -1) return -1;
				}
			}
			break;
		}

#if 0
		case HAK_BRAND_REAL:
		{
			qse_char_t buf[256];
			hak->prm.sprintf (
				hak->prm.ctx,
				buf, HAK_COUNTOF(buf),
				HAK_T("%Lf"),
			#ifdef __MINGW32__
				(double)HAK_RVAL(obj)
			#else
				(long double)HAK_RVAL(obj)
			#endif
			);

			OUTPUT_STR(hak, buf);
			break;
		}
#endif

		case HAK_BRAND_SYMBOL:
			/* Any needs for special action if SYNT(obj) is true?
			 * I simply treat the syntax symbol as a normal symbol
			 * for printing currently. */
			/* TODO: escaping if needed */
			/*if (hak_bfmt_out(hak, fmtout, "#\"%.*js\"", HAK_OBJ_GET_SIZE(obj), HAK_OBJ_GET_CHAR_SLOT(obj)) <= -1) return -1;*/
			if (hak_bfmt_out(hak, fmtout, "%.*js", HAK_OBJ_GET_SIZE(obj), HAK_OBJ_GET_CHAR_SLOT(obj)) <= -1) return -1;
			break;

		case HAK_BRAND_STRING:
		{
			hak_ooch_t ch;
			hak_oow_t i;
			int escape = 0;

			for (i = 0; i < HAK_OBJ_GET_SIZE(obj); i++)
			{
				ch = ((hak_oop_char_t)obj)->slot[i];
				if (ch < ' ' || ch == '\"' || ch == '\\')
				{
					escape = 1;
					break;
				}
			}

			if (escape)
			{
				if (hak_bfmt_out(hak, fmtout, "\"") <= -1) return -1;
				for (i = 0; i < HAK_OBJ_GET_SIZE(obj); i++)
				{
					ch = ((hak_oop_char_t)obj)->slot[i];
					if (print_single_char(hak, fmtout, ch) <= -1) return -1;
				}
				if (hak_bfmt_out(hak, fmtout, "\"") <= -1) return -1;
			}
			else
			{
				if (hak_bfmt_out(hak, fmtout, "\"%.*js\"", HAK_OBJ_GET_SIZE(obj), HAK_OBJ_GET_CHAR_SLOT(obj)) <= -1) return -1;
			}
			break;
		}

		case HAK_BRAND_BYTE_STRING:
		{
			hak_bch_t ch;
			hak_oow_t i;
			int escape = 0;

			for (i = 0; i < HAK_OBJ_GET_SIZE(obj); i++)
			{
				ch = ((hak_oop_byte_t)obj)->slot[i];
				if (ch < ' ' || ch == '\"' || ch == '\\')
				{
					escape = 1;
					break;
				}
			}

			if (escape)
			{
				if (hak_bfmt_out(hak, fmtout, "b\"") <= -1) return -1;
				for (i = 0; i < HAK_OBJ_GET_SIZE(obj); i++)
				{
					ch = ((hak_oop_byte_t)obj)->slot[i];
					if (print_single_byte_char(hak, fmtout, ch) <= -1) return -1;
				}
				if (hak_bfmt_out(hak, fmtout, "\"") <= -1) return -1;
			}
			else
			{
				if (hak_bfmt_out(hak, fmtout, "b\"%.*hs\"", HAK_OBJ_GET_SIZE(obj), HAK_OBJ_GET_BYTE_SLOT(obj)) <= -1) return -1;
			}
			break;
		}


		case HAK_BRAND_CONS:
		{
			int concode;

			/* this part is to print a linked list of cells. ignore the
			 * request to output in the json format */

			concode = HAK_OBJ_GET_FLAGS_CONCODE(obj);
			if (hak_bfmt_out(hak, fmtout, opening_parens[concode][0]) <= -1) return -1;
			cur = obj;

			/* TODO: for MLIST, print : after the first element.
			 *       for ALIST, print := after the first element */
			do
			{
				int x;

				/* Push what to print next on to the stack
				 * the variable p is */
				ps.type = PRINT_STACK_CONS;
				ps.obj = HAK_CONS_CDR(cur);
				ps.idx = concode; /* this is not an index but use this field to restore concode */
				x = push(hak, &ps);
				if (x <= -1) return -1;

				obj = HAK_CONS_CAR(cur);
				/* Jump to the 'next' label so that the object
				 * pointed to by 'obj' is printed. Once it
				 * ends, a jump back to the 'resume' label
				 * is made at the at of this function. */
				goto next;

			resume_cons:
				HAK_ASSERT(hak, ps.type == PRINT_STACK_CONS);
				cur = ps.obj; /* Get back the CDR pushed */
				concode = ps.idx; /* restore the concode */
				if (HAK_IS_NIL(hak,cur))
				{
					/* The CDR part points to a NIL object, which
					 * indicates the end of a list. break the loop */
					break;
				}
				if (!HAK_OOP_IS_POINTER(cur) || HAK_OBJ_GET_CLASS(cur) != (hak_oop_t)hak->c_cons)
				{
					/* The CDR part does not point to a pair. */
					if (hak_bfmt_out(hak, fmtout, " . ") <= -1) return -1;

					/* Push NIL so that the HAK_IS_NIL(hak,p) test in
					 * the 'if' statement above breaks the loop
					 * after the jump is maded back to the 'resume'
					 * label. */
					ps.type = PRINT_STACK_CONS;
					ps.obj = hak->_nil;
					x = push(hak, &ps);
					if (x <= -1) return -1;

					/* Make a jump to 'next' to print the CDR part */
					obj = cur;
					goto next;
				}

				/* The CDR part points to a pair. proceed to it */
				if (hak_bfmt_out(hak, fmtout, breakers[0][0]) <= -1) return -1;
			}
			while (1);

			if (hak_bfmt_out(hak, fmtout, closing_parens[concode][0]) <= -1) return -1;
			break;
		}

		case HAK_BRAND_ARRAY:
		{
			hak_oow_t arridx;

			if (hak_bfmt_out(hak, fmtout, opening_parens[HAK_CONCODE_ARRAY][json]) <= -1) return -1;

			if (HAK_OBJ_GET_SIZE(obj) <= 0)
			{
				if (hak_bfmt_out(hak, fmtout, closing_parens[HAK_CONCODE_ARRAY][json]) <= -1) return -1;
				break;
			}
			arridx = 0;
			ps.type = PRINT_STACK_ARRAY;

			do
			{
				int x;

				/* Push what to print next on to the stack */
				ps.idx = arridx + 1;
				if (ps.idx >= HAK_OBJ_GET_SIZE(obj))
				{
					ps.type = PRINT_STACK_ARRAY_END;
				}
				else
				{
					HAK_ASSERT(hak, ps.type == PRINT_STACK_ARRAY);
					ps.obj = obj;
				}

				x = push(hak, &ps);
				if (x <= -1) return -1;

				obj = ((hak_oop_oop_t)obj)->slot[arridx];
				if (arridx > 0)
				{
					if (hak_bfmt_out(hak, fmtout, breakers[0][json]) <= -1) return -1;
				}
				/* Jump to the 'next' label so that the object
				 * pointed to by 'obj' is printed. Once it
				 * ends, a jump back to the 'resume' label
				 * is made at the end of this function. */
				goto next;

			resume_array:
				HAK_ASSERT(hak, ps.type == PRINT_STACK_ARRAY);
				arridx = ps.idx;
				obj = ps.obj;
			}
			while (1);
			break;
		}

		case HAK_BRAND_BYTE_ARRAY:
		{
			hak_oow_t i;
			if (hak_bfmt_out(hak, fmtout, opening_parens[HAK_CONCODE_BYTEARRAY][json]) <= -1) return -1;
			if (HAK_OBJ_GET_SIZE(obj) > 0)
			{
				if (hak_bfmt_out(hak, fmtout, "%d", ((hak_oop_byte_t)obj)->slot[0]) <= -1) return -1;
				for (i = 1; i < HAK_OBJ_GET_SIZE(obj); i++)
				{
					if (hak_bfmt_out(hak, fmtout, "%hs%d", breakers[0][json], ((hak_oop_byte_t)obj)->slot[i]) <= -1) return -1;
				}
			}
			if (hak_bfmt_out(hak, fmtout, closing_parens[HAK_CONCODE_BYTEARRAY][json]) <= -1) return -1;
			break;
		}

		case HAK_BRAND_CHARACTER_ARRAY:
		{
			hak_oow_t i;
			if (hak_bfmt_out(hak, fmtout, opening_parens[HAK_CONCODE_CHARARRAY][json]) <= -1) return -1;
			if (HAK_OBJ_GET_SIZE(obj) > 0)
			{
				if (hak_bfmt_out(hak, fmtout, "'%jc'", ((hak_oop_char_t)obj)->slot[0]) <= -1) return -1;
				for (i = 1; i < HAK_OBJ_GET_SIZE(obj); i++)
				{
					if (hak_bfmt_out(hak, fmtout, "%hs'%jc'", breakers[0][json], ((hak_oop_char_t)obj)->slot[i]) <= -1) return -1;
				}
			}
			if (hak_bfmt_out(hak, fmtout, closing_parens[HAK_CONCODE_CHARARRAY][json]) <= -1) return -1;
			break;
		}

		case HAK_BRAND_DIC:
		{
			hak_oow_t bucidx, bucsize, buctally;
			hak_oop_dic_t dic;

			if (hak_bfmt_out(hak, fmtout, opening_parens[HAK_CONCODE_DIC][json]) <= -1) return -1;

			dic = (hak_oop_dic_t)obj;
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(dic->tally));
			if (HAK_OOP_TO_SMOOI(dic->tally) <= 0)
			{
				if (hak_bfmt_out(hak, fmtout, closing_parens[HAK_CONCODE_DIC][json]) <= -1) return -1;
				break;
			}
			bucidx = 0;
			bucsize = HAK_OBJ_GET_SIZE(dic->bucket);
			buctally = 0;
			ps.type = PRINT_STACK_DIC;
			ps.obj2 = (hak_oop_t)dic;

			do
			{
				int x;

				if ((buctally & 1) == 0)
				{
					while (bucidx < bucsize)
					{
						/* skip an unoccupied slot in the bucket array */
						obj = dic->bucket->slot[bucidx];
						if (!HAK_IS_NIL(hak,obj)) break;
						bucidx++;
					}

					if (bucidx >= bucsize)
					{
						/* done. scanned the entire bucket */
						if (hak_bfmt_out(hak, fmtout, closing_parens[HAK_CONCODE_DIC][json]) <= -1) return -1;
						break;
					}

					ps.idx = bucidx; /* no increment yet */
					HAK_ASSERT(hak, ps.idx < bucsize);
					HAK_ASSERT(hak, ps.type == PRINT_STACK_DIC);

					ps.obj = dic->bucket->slot[ps.idx];
					ps.idx2 = buctally + 1;

					x = push(hak, &ps);
					if (x <= -1) return -1;

					HAK_ASSERT(hak, HAK_IS_CONS(hak,obj));
					obj = HAK_CONS_CAR(obj);
				}
				else
				{
					/* Push what to print next on to the stack */
					ps.idx = bucidx + 1;
					if (ps.idx >= bucsize)
					{
						ps.type = PRINT_STACK_DIC_END;
					}
					else
					{
						HAK_ASSERT(hak, ps.type == PRINT_STACK_DIC);
						ps.obj = dic->bucket->slot[ps.idx];
					}
					ps.idx2 = buctally + 1;

					x = push(hak, &ps);
					if (x <= -1) return -1;

					HAK_ASSERT(hak, HAK_IS_CONS(hak,obj));
					obj = HAK_CONS_CDR(obj);
				}

				if (buctally > 0)
				{
					if (hak_bfmt_out(hak, fmtout, breakers[buctally & 1][json]) <= -1) return -1;
				}

				/* Jump to the 'next' label so that the object
				 * pointed to by 'obj' is printed. Once it
				 * ends, a jump back to the 'resume' label
				 * is made at the end of this function. */
				goto next;

			resume_dic:
				HAK_ASSERT(hak, ps.type == PRINT_STACK_DIC);
				bucidx = ps.idx;
				buctally = ps.idx2;
				obj = ps.obj;
				dic = (hak_oop_dic_t)ps.obj2;
				bucsize = HAK_OBJ_GET_SIZE(dic->bucket);
			}
			while (1);

			break;
		}

		case HAK_BRAND_PRIM:
			word_index = WORD_PRIM;
			goto print_word;

		case HAK_BRAND_FUNCTION:
			word_index = WORD_FUNCTION;
			goto print_word;

		case HAK_BRAND_BLOCK:
			word_index = WORD_BLOCK;
			goto print_word;

		case HAK_BRAND_CONTEXT:
			word_index = WORD_CONTEXT;
			goto print_word;

		case HAK_BRAND_PROCESS:
			word_index = WORD_PROCESS;
			goto print_word;

		case HAK_BRAND_PROCESS_SCHEDULER:
			word_index = WORD_PROCESS_SCHEDULER;
			goto print_word;

		case HAK_BRAND_SEMAPHORE:
			word_index = WORD_SEMAPHORE;
			goto print_word;

		case HAK_BRAND_SEMAPHORE_GROUP:
			word_index = WORD_SEMAPHORE_GROUP;
			goto print_word;

		case HAK_BRAND_CLASS:
		{
			hak_oop_class_t _class = (hak_oop_class_t)obj;
			if (HAK_IS_NIL(hak, _class->name))
			{
				word_index = WORD_CLASS;
				goto print_word;
			}
			HAK_ASSERT(hak, HAK_IS_SYMBOL(hak, _class->name));
			if (hak_bfmt_out(hak, fmtout, "%.*js", HAK_OBJ_GET_SIZE(_class->name), HAK_OBJ_GET_CHAR_SLOT(_class->name)) <= -1) return -1;
			break;
		}

		case HAK_BRAND_INSTANCE:
		{
			hak_oop_class_t _class = (hak_oop_class_t)HAK_CLASSOF(hak, obj);
			HAK_ASSERT(hak, HAK_IS_CLASS(hak, _class));
			if (HAK_IS_NIL(hak, _class->name))
			{
				word_index = WORD_INSTANCE;
				goto print_word;
			}
			HAK_ASSERT(hak, HAK_IS_SYMBOL(hak, _class->name));
			if (hak_bfmt_out(hak, fmtout, "#INSTANCE OF %.*js", HAK_OBJ_GET_SIZE(_class->name), HAK_OBJ_GET_CHAR_SLOT(_class->name)) <= -1) return -1;
			break;
		}

		default:
			HAK_DEBUG3 (hak, "Internal error - unknown object brand %d at %s:%d\n", (int)brand, __FILE__, __LINE__);
			HAK_ASSERT(hak, "Unknown object brand" == HAK_NULL);
			hak_seterrbfmt(hak, HAK_EINTERN, "unknown object brand %d", (int)brand);
			return -1;

		print_word:
			if (hak_bfmt_out(hak, fmtout, "%.*js", word[word_index].len, word[word_index].ptr) <= -1) return -1;
			break;
	}

done:
	/* if the printing stack is not empty, we still got more to print */
	while (hak->p.s.size > 0)
	{
		pop(hak, &ps);
		switch (ps.type)
		{
			case PRINT_STACK_CONS:
				goto resume_cons;

			case PRINT_STACK_ARRAY:
				goto resume_array;

			case PRINT_STACK_ARRAY_END:
				if (hak_bfmt_out(hak, fmtout, closing_parens[HAK_CONCODE_ARRAY][json]) <= -1) return -1;
				break;

			case PRINT_STACK_DIC:
				goto resume_dic;

			case PRINT_STACK_DIC_END:
				if (hak_bfmt_out(hak, fmtout, closing_parens[HAK_CONCODE_DIC][json]) <= -1) return -1;
				break;

			default:
				HAK_DEBUG3 (hak, "Internal error - unknown print stack type %d at %s:%d\n", (int)ps.type, __FILE__, __LINE__);
				hak_seterrbfmt(hak, HAK_EINTERN, "internal error - unknown print stack type %d", (int)ps.type);
				return -1;
		}
	}
	return 0;
}

#if 0
int hak_outfmtobj (hak_t* hak, hak_bitmask_t mask, hak_oop_t obj, hak_outbfmt_t outbfmt)
{
	int n;

	/* the printer stack must be empty. buggy if not. */
	HAK_ASSERT(hak, hak->p.s.size == 0);

	hak->p.e = obj; /* remember the head of the object to print */
	n = hak_proutbfmt(hak, mask, obj);
	hak->p.e = hak->_nil; /* reset what's remembered */

	/* clear the printing stack if an error has occurred for GC not to keep
	 * the objects in the stack */
	if (n <= -1) hak->p.s.size = 0;

	/* the printer stack must get empty when done. buggy if not */
	HAK_ASSERT(hak, hak->p.s.size == 0);

	return n;
}
#endif

int hak_print (hak_t* hak, hak_oop_t obj)
{
	HAK_ASSERT(hak, hak->io.udo_wrtr != HAK_NULL);
	/*return hak_outfmtobj(hak, HAK_LOG_APP | HAK_LOG_FATAL, obj);*/
	return hak_prbfmt(hak, "%O", obj);
}

void hak_dumpcnode (hak_t* hak, hak_cnode_t* cnode, int newline)
{
	int t;

	/* TODO: this is incomplete function. make it complete */
	if (cnode)
	{
		t = HAK_CNODE_GET_TYPE(cnode);
		switch (t)
		{
			case HAK_CNODE_CHARLIT:
			case HAK_CNODE_BCHRLIT:
			case HAK_CNODE_SYMBOL:
			case HAK_CNODE_DSYMBOL:
			case HAK_CNODE_STRLIT:
			case HAK_CNODE_BSTRLIT:
			case HAK_CNODE_SYMLIT:
			case HAK_CNODE_NUMLIT:
			case HAK_CNODE_RADNUMLIT:
			case HAK_CNODE_FPDECLIT:
			case HAK_CNODE_SMPTRLIT:
			case HAK_CNODE_ERRLIT:

			case HAK_CNODE_NIL:
			case HAK_CNODE_TRUE:
			case HAK_CNODE_FALSE:
			case HAK_CNODE_SELF:
			case HAK_CNODE_SUPER:

			case HAK_CNODE_CLASS:
			case HAK_CNODE_FUN:
			case HAK_CNODE_DO:
			case HAK_CNODE_IF:
			case HAK_CNODE_ELIF:
			case HAK_CNODE_ELSE:
			case HAK_CNODE_THROW:
			case HAK_CNODE_TRY:
			case HAK_CNODE_CATCH:
			case HAK_CNODE_BREAK:
			case HAK_CNODE_CONTINUE:
			case HAK_CNODE_UNTIL:
			case HAK_CNODE_WHILE:
			case HAK_CNODE_RETURN:
			case HAK_CNODE_REVERT:
			case HAK_CNODE_AND:
			case HAK_CNODE_OR:
			case HAK_CNODE_PLUS:
			case HAK_CNODE_SET:
			case HAK_CNODE_SET_R:

			case HAK_CNODE_ELLIPSIS:
			case HAK_CNODE_TRPCOLONS:
			case HAK_CNODE_DBLCOLONS:
			case HAK_CNODE_COLON:
			case HAK_CNODE_COLONGT:
			case HAK_CNODE_COLONLT:
				hak_logbfmt(hak, HAK_LOG_FATAL, " %.*js ", HAK_CNODE_GET_TOKLEN(cnode), HAK_CNODE_GET_TOKPTR(cnode));
				break;

			case HAK_CNODE_CONS:
			{
				hak_concode_t cc;

				hak_logbfmt(hak, HAK_LOG_FATAL, " (");
				hak_dumpcnode(hak, HAK_CNODE_CONS_CAR(cnode), 0);

				cc = HAK_CNODE_CONS_CONCODE(cnode);
				switch (cc)
				{
					case HAK_CONCODE_ALIST:
						hak_logbfmt(hak, HAK_LOG_FATAL, " := ");
						break;
					case HAK_CONCODE_BLIST:
					case HAK_CONCODE_MLIST:
						hak_logbfmt(hak, HAK_LOG_FATAL, ":");
						break;
				}

				hak_dumpcnode(hak, HAK_CNODE_CONS_CDR(cnode),0);
				hak_logbfmt(hak, HAK_LOG_FATAL, ") ");
				break;
			}

			case HAK_CNODE_ELIST:
				hak_logbfmt(hak, HAK_LOG_FATAL, " () ", HAK_CNODE_GET_TOKLEN(cnode), HAK_CNODE_GET_TOKPTR(cnode));
				break;

			case HAK_CNODE_SHELL:
				hak_logbfmt(hak, HAK_LOG_FATAL, " () ", HAK_CNODE_GET_TOKLEN(cnode), HAK_CNODE_GET_TOKPTR(cnode));
				break;
		}
	}

	if  (newline) hak_logbfmt(hak, HAK_LOG_FATAL, "\n");
}


