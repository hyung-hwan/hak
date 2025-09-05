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

#define DECODE_LOG_MASK (HAK_LOG_MNEMONIC | HAK_LOG_INFO)

#define LOG_INST_0(hak,fmt) HAK_LOG1(hak, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer)
#define LOG_INST_1(hak,fmt,a1) HAK_LOG2(hak, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer, a1)
#define LOG_INST_2(hak,fmt,a1,a2) HAK_LOG3(hak, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer, a1, a2)
#define LOG_INST_3(hak,fmt,a1,a2,a3) HAK_LOG4(hak, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3)
#define LOG_INST_4(hak,fmt,a1,a2,a3,a4) HAK_LOG5(hak, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3, a4)
#define LOG_INST_5(hak,fmt,a1,a2,a3,a4,a5) HAK_LOG6(hak, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3, a4, a5)
#define LOG_INST_6(hak,fmt,a1,a2,a3,a4,a5,a6) HAK_LOG7(hak, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3, a4, a5, a6)
#define LOG_INST_7(hak,fmt,a1,a2,a3,a4,a5,a6,a7) HAK_LOG8(hak, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3, a4, a5, a6, a7)
#define LOG_INST_8(hak,fmt,a1,a2,a3,a4,a5,a6,a7,a8) HAK_LOG9(hak, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3, a4, a5, a6, a7, a8)


#define FETCH_BYTE_CODE(hak) (cdptr[ip++])
#define FETCH_BYTE_CODE_TO(hak,v_ooi) (v_ooi = FETCH_BYTE_CODE(hak))
#if (HAK_CODE_LONG_PARAM_SIZE == 2)
#	define FETCH_PARAM_CODE_TO(hak,v_ooi) \
		do { \
			v_ooi = FETCH_BYTE_CODE(hak); \
			v_ooi = (v_ooi << 8) | FETCH_BYTE_CODE(hak); \
		} while (0)
#else
#	define FETCH_PARAM_CODE_TO(hak,v_ooi) (v_ooi = FETCH_BYTE_CODE(hak))
#endif

/* TODO: check if ip shoots beyond the maximum length in fetching code and parameters */
int hak_decode (hak_t* hak, const hak_code_t* code, hak_oow_t start, hak_oow_t end)
{
	hak_oob_t bcode, * cdptr;
	hak_ooi_t ip = start, fetched_instruction_pointer;
	hak_oow_t b1, b2;

	/* the instruction at the offset 'end' is not decoded.
	 * decoding offset range is from start to end - 1. */

	if (!code) code = &hak->code;

	HAK_ASSERT(hak, start >= 0 && end >= 0);
	HAK_ASSERT(hak, code->bc.len < HAK_SMOOI_MAX); /* asserted by the compiler */
	HAK_ASSERT(hak, end <= code->bc.len); /* not harmful though this fails */
	if (start >= code->bc.len)
	{
		hak_seterrnum(hak, HAK_EINVAL);
		return -1;
	}
	if (end > code->bc.len) end = code->bc.len;

	ip = start;
	cdptr = code->bc.ptr;

/* TODO: check if ip increases beyond bcode when fetching parameters too */
	while (ip < end)
	{
		fetched_instruction_pointer = ip;
		FETCH_BYTE_CODE_TO(hak, bcode);

		switch (bcode)
		{
			/* -------------------------------------------------------- */
			case HAK_CODE_PLUS:
				LOG_INST_0 (hak, "plus");
				break;

			/* -------------------------------------------------------- */
			case HAK_CODE_PUSH_IVAR_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto push_ivar;
			case HAK_CODE_PUSH_IVAR_0:
			case HAK_CODE_PUSH_IVAR_1:
			case HAK_CODE_PUSH_IVAR_2:
			case HAK_CODE_PUSH_IVAR_3:
			case HAK_CODE_PUSH_IVAR_4:
			case HAK_CODE_PUSH_IVAR_5:
			case HAK_CODE_PUSH_IVAR_6:
			case HAK_CODE_PUSH_IVAR_7:
				b1 = bcode & 0x7; /* low 3 bits */
			push_ivar:
				LOG_INST_1 (hak, "push_ivar %zu", b1);
				break;

			/* ------------------------------------------------- */

			case HAK_CODE_STORE_INTO_IVAR_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto store_into_ivar;
			case HAK_CODE_STORE_INTO_IVAR_0:
			case HAK_CODE_STORE_INTO_IVAR_1:
			case HAK_CODE_STORE_INTO_IVAR_2:
			case HAK_CODE_STORE_INTO_IVAR_3:
			case HAK_CODE_STORE_INTO_IVAR_4:
			case HAK_CODE_STORE_INTO_IVAR_5:
			case HAK_CODE_STORE_INTO_IVAR_6:
			case HAK_CODE_STORE_INTO_IVAR_7:
				b1 = bcode & 0x7; /* low 3 bits */
			store_into_ivar:
				LOG_INST_1 (hak, "store_into_ivar %zu", b1);
				break;

			case HAK_CODE_POP_INTO_IVAR_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto pop_into_ivar;
			case HAK_CODE_POP_INTO_IVAR_0:
			case HAK_CODE_POP_INTO_IVAR_1:
			case HAK_CODE_POP_INTO_IVAR_2:
			case HAK_CODE_POP_INTO_IVAR_3:
			case HAK_CODE_POP_INTO_IVAR_4:
			case HAK_CODE_POP_INTO_IVAR_5:
			case HAK_CODE_POP_INTO_IVAR_6:
			case HAK_CODE_POP_INTO_IVAR_7:
				b1 = bcode & 0x7; /* low 3 bits */
			pop_into_ivar:
				LOG_INST_1 (hak, "pop_into_ivar %zu", b1);
				break;

			/* ------------------------------------------------- */
			case HAK_CODE_PUSH_TEMPVAR_X:
			case HAK_CODE_STORE_INTO_TEMPVAR_X:
			case HAK_CODE_POP_INTO_TEMPVAR_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto handle_tempvar;

			case HAK_CODE_PUSH_TEMPVAR_0:
			case HAK_CODE_PUSH_TEMPVAR_1:
			case HAK_CODE_PUSH_TEMPVAR_2:
			case HAK_CODE_PUSH_TEMPVAR_3:
			case HAK_CODE_PUSH_TEMPVAR_4:
			case HAK_CODE_PUSH_TEMPVAR_5:
			case HAK_CODE_PUSH_TEMPVAR_6:
			case HAK_CODE_PUSH_TEMPVAR_7:
			case HAK_CODE_STORE_INTO_TEMPVAR_0:
			case HAK_CODE_STORE_INTO_TEMPVAR_1:
			case HAK_CODE_STORE_INTO_TEMPVAR_2:
			case HAK_CODE_STORE_INTO_TEMPVAR_3:
			case HAK_CODE_STORE_INTO_TEMPVAR_4:
			case HAK_CODE_STORE_INTO_TEMPVAR_5:
			case HAK_CODE_STORE_INTO_TEMPVAR_6:
			case HAK_CODE_STORE_INTO_TEMPVAR_7:
			case HAK_CODE_POP_INTO_TEMPVAR_0:
			case HAK_CODE_POP_INTO_TEMPVAR_1:
			case HAK_CODE_POP_INTO_TEMPVAR_2:
			case HAK_CODE_POP_INTO_TEMPVAR_3:
			case HAK_CODE_POP_INTO_TEMPVAR_4:
			case HAK_CODE_POP_INTO_TEMPVAR_5:
			case HAK_CODE_POP_INTO_TEMPVAR_6:
			case HAK_CODE_POP_INTO_TEMPVAR_7:
				b1 = bcode & 0x7; /* low 3 bits */
			handle_tempvar:

				if ((bcode >> 4) & 1)
				{
					/* push - bit 4 on */
					LOG_INST_1 (hak, "push_tempvar %zu", b1);
				}
				else
				{
					/* store or pop - bit 5 off */
					if ((bcode >> 3) & 1)
					{
						/* pop - bit 3 on */
						LOG_INST_1 (hak, "pop_into_tempvar %zu", b1);
					}
					else
					{
						LOG_INST_1 (hak, "store_into_tempvar %zu", b1);
					}
				}
				break;

			/* ------------------------------------------------- */
			case HAK_CODE_PUSH_LITERAL_X2:
				FETCH_PARAM_CODE_TO(hak, b1);
				FETCH_PARAM_CODE_TO(hak, b2);
				b1 = (b1 << (8 * HAK_CODE_LONG_PARAM_SIZE)) | b2;
				goto push_literal;

			case HAK_CODE_PUSH_LITERAL_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto push_literal;

			case HAK_CODE_PUSH_LITERAL_0:
			case HAK_CODE_PUSH_LITERAL_1:
			case HAK_CODE_PUSH_LITERAL_2:
			case HAK_CODE_PUSH_LITERAL_3:
			case HAK_CODE_PUSH_LITERAL_4:
			case HAK_CODE_PUSH_LITERAL_5:
			case HAK_CODE_PUSH_LITERAL_6:
			case HAK_CODE_PUSH_LITERAL_7:
				b1 = bcode & 0x7; /* low 3 bits */
			push_literal:
				LOG_INST_1 (hak, "push_literal @%zu", b1);
				break;

			/* ------------------------------------------------- */
			case HAK_CODE_PUSH_OBJECT_X:
			case HAK_CODE_STORE_INTO_OBJECT_X:
			case HAK_CODE_POP_INTO_OBJECT_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto handle_object;

			case HAK_CODE_PUSH_OBJECT_0:
			case HAK_CODE_PUSH_OBJECT_1:
			case HAK_CODE_PUSH_OBJECT_2:
			case HAK_CODE_PUSH_OBJECT_3:
			case HAK_CODE_STORE_INTO_OBJECT_0:
			case HAK_CODE_STORE_INTO_OBJECT_1:
			case HAK_CODE_STORE_INTO_OBJECT_2:
			case HAK_CODE_STORE_INTO_OBJECT_3:
			case HAK_CODE_POP_INTO_OBJECT_0:
			case HAK_CODE_POP_INTO_OBJECT_1:
			case HAK_CODE_POP_INTO_OBJECT_2:
			case HAK_CODE_POP_INTO_OBJECT_3:
				b1 = bcode & 0x3; /* low 2 bits */
			handle_object:
				if ((bcode >> 3) & 1)
				{
					if ((bcode >> 2) & 1)
					{
						LOG_INST_1 (hak, "pop_into_object @%zu", b1);
					}
					else
					{
						LOG_INST_1 (hak, "store_into_object @%zu", b1);
					}
				}
				else
				{
					LOG_INST_1 (hak, "push_object @%zu", b1);
				}
				break;

			/* -------------------------------------------------------- */

			case HAK_CODE_JUMP_FORWARD_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "jump_forward %zu", b1);
				break;

			case HAK_CODE_JUMP_FORWARD_0:
			case HAK_CODE_JUMP_FORWARD_1:
			case HAK_CODE_JUMP_FORWARD_2:
			case HAK_CODE_JUMP_FORWARD_3:
				LOG_INST_1 (hak, "jump_forward %zu", (hak_oow_t)(bcode & 0x3)); /* low 2 bits */
				break;

			case HAK_CODE_JUMP_BACKWARD_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "jump_backward %zu", b1);
				hak->ip += b1;
				break;

			case HAK_CODE_JUMP_BACKWARD_0:
			case HAK_CODE_JUMP_BACKWARD_1:
			case HAK_CODE_JUMP_BACKWARD_2:
			case HAK_CODE_JUMP_BACKWARD_3:
				LOG_INST_1 (hak, "jump_backward %zu", (hak_oow_t)(bcode & 0x3)); /* low 2 bits */
				break;

			case HAK_CODE_JUMP_FORWARD_IF_TRUE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "jump_forward_if_true %zu", b1);
				break;

			case HAK_CODE_JUMP2_FORWARD_IF_TRUE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "jump2_forward_if_true %zu", b1);
				break;

			case HAK_CODE_JUMP_FORWARD_IF_FALSE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "jump_forward_if_false %zu", b1);
				break;

			case HAK_CODE_JUMP2_FORWARD_IF_FALSE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "jump2_forward_if_false %zu", b1);
				break;

			case HAK_CODE_JUMP2_FORWARD:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "jump2_forward %zu", b1);
				break;

			case HAK_CODE_JUMP_BACKWARD_IF_TRUE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "jump_backward_if_true %zu", b1);
				break;

			case HAK_CODE_JUMP2_BACKWARD_IF_TRUE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "jump2_backward_if_true %zu", b1);
				break;

			case HAK_CODE_JUMP_BACKWARD_IF_FALSE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "jump_backward_if_false %zu", b1);
				break;

			case HAK_CODE_JUMP2_BACKWARD_IF_FALSE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "jump2_backward_if_false %zu", b1);
				break;

			case HAK_CODE_JUMP2_BACKWARD:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "jump2_backward %zu", b1);
				break;

			/* -------------------------------------------------------- */
			case HAK_CODE_PUSH_RETURN_R:
				LOG_INST_0 (hak, "push_return_r");
				break;

			case HAK_CODE_CALL_R:
				FETCH_PARAM_CODE_TO(hak, b1); /* nargs */
				FETCH_PARAM_CODE_TO(hak, b2); /* nrvars */
				LOG_INST_2 (hak, "call %zu %zu", b1, b2);
				break;

			case HAK_CODE_CALL_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto handle_call;

			case HAK_CODE_CALL_0:
			case HAK_CODE_CALL_1:
			case HAK_CODE_CALL_2:
			case HAK_CODE_CALL_3:
				b1 = bcode & 0x3; /* low 2 bits */
			handle_call:
				LOG_INST_1 (hak, "call %zu", b1);
				break;

			/* -------------------------------------------------------- */
			case HAK_CODE_TRY_ENTER:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "try_enter %zu", b1);
				break;

			case HAK_CODE_TRY_ENTER2:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "try_enter2 %zu", b1);
				break;

			case HAK_CODE_TRY_EXIT:
				LOG_INST_0 (hak, "try_exit");
				break;

			case HAK_CODE_THROW:
				LOG_INST_0 (hak, "throw");
				break;
			/* -------------------------------------------------------- */
			case HAK_CODE_CLASS_LOAD:
				LOG_INST_0 (hak, "class_load");
				break;

			case HAK_CODE_CLASS_ENTER:
			{
				hak_oow_t b3, b4, b5;
				FETCH_PARAM_CODE_TO(hak, b1);
				FETCH_PARAM_CODE_TO(hak, b2);
				FETCH_PARAM_CODE_TO(hak, b3);
				FETCH_BYTE_CODE_TO(hak, b4); /* spec/selfspec */
				FETCH_BYTE_CODE_TO(hak, b5); /* indexed_type */
				LOG_INST_5 (hak, "class_enter %zu %zu %zu %#zx %zu", b1, b2, b3, b4, b5);
				break;
			}

			case HAK_CODE_CLASS_EXIT:
				LOG_INST_0 (hak, "class_exit");
				break;

			case HAK_CODE_CLASS_PEXIT:
				LOG_INST_0 (hak, "class_pexit");
				break;

			case HAK_CODE_CLASS_CMSTORE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "class_cmstore @%zu", b1);
				break;

			case HAK_CODE_CLASS_CIMSTORE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "class_cimstore @%zu", b1);
				break;

			case HAK_CODE_CLASS_IMSTORE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "class_imstore @%zu", b1);
				break;
			/* -------------------------------------------------------- */

			case HAK_CODE_PUSH_CTXTEMPVAR_X:
			case HAK_CODE_STORE_INTO_CTXTEMPVAR_X:
			case HAK_CODE_POP_INTO_CTXTEMPVAR_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				FETCH_PARAM_CODE_TO(hak, b2);
				goto handle_ctxtempvar;
			case HAK_CODE_PUSH_CTXTEMPVAR_0:
			case HAK_CODE_PUSH_CTXTEMPVAR_1:
			case HAK_CODE_PUSH_CTXTEMPVAR_2:
			case HAK_CODE_PUSH_CTXTEMPVAR_3:
			case HAK_CODE_STORE_INTO_CTXTEMPVAR_0:
			case HAK_CODE_STORE_INTO_CTXTEMPVAR_1:
			case HAK_CODE_STORE_INTO_CTXTEMPVAR_2:
			case HAK_CODE_STORE_INTO_CTXTEMPVAR_3:
			case HAK_CODE_POP_INTO_CTXTEMPVAR_0:
			case HAK_CODE_POP_INTO_CTXTEMPVAR_1:
			case HAK_CODE_POP_INTO_CTXTEMPVAR_2:
			case HAK_CODE_POP_INTO_CTXTEMPVAR_3:
				b1 = bcode & 0x3; /* low 2 bits */
				FETCH_BYTE_CODE_TO(hak, b2);

			handle_ctxtempvar:
				if ((bcode >> 3) & 1)
				{
					/* store or pop */

					if ((bcode >> 2) & 1)
					{
						LOG_INST_2 (hak, "pop_into_ctxtempvar %zu %zu", b1, b2);
					}
					else
					{
						LOG_INST_2 (hak, "store_into_ctxtempvar %zu %zu", b1, b2);
					}
				}
				else
				{
					/* push */
					LOG_INST_2 (hak, "push_ctxtempvar %zu %zu", b1, b2);
				}

				break;
			/* -------------------------------------------------------- */

			case HAK_CODE_PUSH_OBJVAR_X:
			case HAK_CODE_STORE_INTO_OBJVAR_X:
			case HAK_CODE_POP_INTO_OBJVAR_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				FETCH_PARAM_CODE_TO(hak, b2);
				goto handle_objvar;

			case HAK_CODE_PUSH_OBJVAR_0:
			case HAK_CODE_PUSH_OBJVAR_1:
			case HAK_CODE_PUSH_OBJVAR_2:
			case HAK_CODE_PUSH_OBJVAR_3:
			case HAK_CODE_STORE_INTO_OBJVAR_0:
			case HAK_CODE_STORE_INTO_OBJVAR_1:
			case HAK_CODE_STORE_INTO_OBJVAR_2:
			case HAK_CODE_STORE_INTO_OBJVAR_3:
			case HAK_CODE_POP_INTO_OBJVAR_0:
			case HAK_CODE_POP_INTO_OBJVAR_1:
			case HAK_CODE_POP_INTO_OBJVAR_2:
			case HAK_CODE_POP_INTO_OBJVAR_3:
				/* b1 -> variable index to the object indicated by b2.
				 * b2 -> object index stored in the literal frame. */
				b1 = bcode & 0x3; /* low 2 bits */
				FETCH_BYTE_CODE_TO(hak, b2);

			handle_objvar:
				if ((bcode >> 3) & 1)
				{
					/* store or pop */
					if ((bcode >> 2) & 1)
					{
						LOG_INST_2 (hak, "pop_into_objvar %zu %zu", b1, b2);
					}
					else
					{
						LOG_INST_2 (hak, "store_into_objvar %zu %zu", b1, b2);
					}
				}
				else
				{
					LOG_INST_2 (hak, "push_objvar %zu %zu", b1, b2);
				}

				break;

			/* -------------------------------------------------------- */

			case HAK_CODE_SEND_R:
				FETCH_PARAM_CODE_TO(hak, b1); /* nargs */
				FETCH_PARAM_CODE_TO(hak, b2); /* nrvars */
				LOG_INST_2 (hak, "send_r %zu %zu", b1, b2);
				break;
			case HAK_CODE_SEND_TO_SUPER_R:
				FETCH_PARAM_CODE_TO(hak, b1); /* nargs */
				FETCH_PARAM_CODE_TO(hak, b2); /* nrvars */
				LOG_INST_2 (hak, "send_to_super_r %zu %zu", b1, b2);
				break;

			case HAK_CODE_SEND_X:
			case HAK_CODE_SEND_TO_SUPER_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto handle_send;

			case HAK_CODE_SEND_0:
			case HAK_CODE_SEND_1:
			case HAK_CODE_SEND_2:
			case HAK_CODE_SEND_3:
			case HAK_CODE_SEND_TO_SUPER_0:
			case HAK_CODE_SEND_TO_SUPER_1:
			case HAK_CODE_SEND_TO_SUPER_2:
			case HAK_CODE_SEND_TO_SUPER_3:
				b1 = bcode & 0x3; /* low 2 bits */

			handle_send:
				LOG_INST_2 (hak, "send%hs %zu", (((bcode >> 2) & 1)? "_to_super": ""), b1);
				break;

			/* -------------------------------------------------------- */

			case HAK_CODE_PUSH_CVAR_I_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "push_cvar_i %zu", b1);
				break;

			case HAK_CODE_STORE_INTO_CVAR_I_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "store_into_cvar_i %zu", b1);
				break;

			case HAK_CODE_POP_INTO_CVAR_I_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "pop_into_cvar_i %zu", b1);
				break;

			/* -------------------------------------------------------- */

			case HAK_CODE_PUSH_CVAR_M_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "push_cvar_m %zu", b1);
				break;

			case HAK_CODE_STORE_INTO_CVAR_M_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "store_into_cvar_m %zu", b1);
				break;

			case HAK_CODE_POP_INTO_CVAR_M_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "pop_into_cvar_m %zu", b1);
				break;

			/* -------------------------------------------------------- */

			case HAK_CODE_PUSH_RECEIVER:
				LOG_INST_0 (hak, "push_receiver");
				break;

			case HAK_CODE_PUSH_NIL:
				LOG_INST_0 (hak, "push_nil");
				break;

			case HAK_CODE_PUSH_TRUE:
				LOG_INST_0 (hak, "push_true");
				break;

			case HAK_CODE_PUSH_FALSE:
				LOG_INST_0 (hak, "push_false");
				break;

			case HAK_CODE_PUSH_CONTEXT:
				LOG_INST_0 (hak, "push_context");
				break;

			case HAK_CODE_PUSH_PROCESS:
				LOG_INST_0 (hak, "push_process");
				break;

			case HAK_CODE_PUSH_NEGONE:
				LOG_INST_0 (hak, "push_negone");
				break;

			case HAK_CODE_PUSH_ZERO:
				LOG_INST_0 (hak, "push_zero");
				break;

			case HAK_CODE_PUSH_ONE:
				LOG_INST_0 (hak, "push_one");
				break;

			case HAK_CODE_PUSH_TWO:
				LOG_INST_0 (hak, "push_two");
				break;

			case HAK_CODE_PUSH_INTLIT:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "push_intlit %zu", b1);
				break;

			case HAK_CODE_PUSH_NEGINTLIT:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "push_negintlit %zu", b1);
				break;

			case HAK_CODE_PUSH_CHARLIT:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "push_charlit %zu", b1);
				break;
			/* -------------------------------------------------------- */

			case HAK_CODE_MAKE_ARRAY:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "make_array %zu", b1);
				break;

			case HAK_CODE_POP_INTO_ARRAY:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "pop_into_array %zu", b1);
				break;

			case HAK_CODE_MAKE_BYTEARRAY:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "make_bytearray %zu", b1);
				break;

			case HAK_CODE_POP_INTO_BYTEARRAY:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "pop_into_bytearray %zu", b1);
				break;

			case HAK_CODE_MAKE_CHARARRAY:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "make_chararray %zu", b1);
				break;

			case HAK_CODE_POP_INTO_CHARARRAY:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "pop_into_chararray %zu", b1);
				break;

			case HAK_CODE_MAKE_DIC:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1 (hak, "make_dic %zu", b1);
				break;

			case HAK_CODE_POP_INTO_DIC:
				LOG_INST_0 (hak, "pop_into_dic");
				break;

			case HAK_CODE_MAKE_CONS:
				LOG_INST_0 (hak, "make_cons");
				break;

			case HAK_CODE_POP_INTO_CONS:
				LOG_INST_0 (hak, "pop_into_cons");
				break;

			case HAK_CODE_POP_INTO_CONS_END:
				LOG_INST_0 (hak, "pop_into_cons_end");
				break;

			case HAK_CODE_POP_INTO_CONS_CDR:
				LOG_INST_0 (hak, "pop_into_cons_cdr");
				break;
			/* -------------------------------------------------------- */

			case HAK_CODE_DUP_STACKTOP:
				LOG_INST_0 (hak, "dup_stacktop");
				break;

			case HAK_CODE_POP_STACKTOP:
				LOG_INST_0 (hak, "pop_stacktop");
				break;

			case HAK_CODE_RETURN_STACKTOP:
				LOG_INST_0 (hak, "return_stacktop");
				break;

			case HAK_CODE_RETURN_RECEIVER:
				LOG_INST_0 (hak, "return_receiver");
				break;

			case HAK_CODE_RETURN_FROM_BLOCK:
				LOG_INST_0 (hak, "return_from_block");
				break;

			case HAK_CODE_MAKE_FUNCTION:
			{
				hak_oow_t b3, b4, x;
				/* - block mask(extended long)
				 * - literal frame index to the name symbol(extended long)
				 * - base literal frame start
				 * - base literal frame end */
				FETCH_PARAM_CODE_TO(hak, b1);
				FETCH_PARAM_CODE_TO(hak, x);
				b1 = (b1 << (8 * HAK_CODE_LONG_PARAM_SIZE)) | x;

				/* lfindex to name is an extended long parameter */
				FETCH_PARAM_CODE_TO(hak, b2);
				FETCH_PARAM_CODE_TO(hak, x);
				b2 = (b2 << (8 * HAK_CODE_LONG_PARAM_SIZE)) | x;

				FETCH_PARAM_CODE_TO(hak, b3);
				FETCH_PARAM_CODE_TO(hak, b4);

				LOG_INST_8 (hak, "make_function %zu %zu %zu %zu %zu %zu %zu %zu",
					GET_BLK_MASK_INSTA(b1),
					GET_BLK_MASK_VA(b1),
					GET_BLK_MASK_NARGS(b1),
					GET_BLK_MASK_NRVARS(b1),
					GET_BLK_MASK_NLVARS(b1),
					b2, b3, b4);

				HAK_ASSERT(hak, b1 >= 0);
				break;
			}

			case HAK_CODE_MAKE_BLOCK:
			{
				hak_oow_t x;

				/* block mask (extended long)
				 * literal frame index to the name symbol(extended long)
				 */
				FETCH_PARAM_CODE_TO(hak, b1);
				FETCH_PARAM_CODE_TO(hak, x);
				b1 = (b1 << (8 * HAK_CODE_LONG_PARAM_SIZE)) | x;

				/* lfindex to name is an extended long parameter */
				FETCH_PARAM_CODE_TO(hak, b2);
				FETCH_PARAM_CODE_TO(hak, x);
				b2 = (b2 << (8 * HAK_CODE_LONG_PARAM_SIZE)) | x;

				LOG_INST_6 (hak, "make_block %zu %zu %zu %zu %zu %zu",
					GET_BLK_MASK_INSTA(b1),
					GET_BLK_MASK_VA(b1),
					GET_BLK_MASK_NARGS(b1),
					GET_BLK_MASK_NRVARS(b1),
					GET_BLK_MASK_NLVARS(b1),
					b2);

				HAK_ASSERT(hak, b1 >= 0);
				break;
			}

			case HAK_CODE_NOOP:
				/* do nothing */
				LOG_INST_0 (hak, "noop");
				break;

			default:
				LOG_INST_1 (hak, "UNKNOWN BYTE CODE ENCOUNTERED %x", (int)bcode);
				hak_seterrnum(hak, HAK_EINTERN);
				break;
		}
	}


/* TODO: this needs changes... */
	/* print literal frame contents */
	for (ip = 0; ip < code->lit.len; ip++)
	{
		HAK_LOG2(hak, DECODE_LOG_MASK, "@%-9zd %O\n", ip, ((hak_oop_oop_t)code->lit.arr)->slot[ip]);
	}

	return 0;
}

