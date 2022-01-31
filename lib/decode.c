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
    IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WAfRRANTIES
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

#define DECODE_LOG_MASK (HCL_LOG_MNEMONIC | HCL_LOG_INFO)

#define LOG_INST_0(hcl,fmt) HCL_LOG1(hcl, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer)
#define LOG_INST_1(hcl,fmt,a1) HCL_LOG2(hcl, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer, a1)
#define LOG_INST_2(hcl,fmt,a1,a2) HCL_LOG3(hcl, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer, a1, a2)
#define LOG_INST_3(hcl,fmt,a1,a2,a3) HCL_LOG4(hcl, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3)
#define LOG_INST_4(hcl,fmt,a1,a2,a3,a4) HCL_LOG5(hcl, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3, a4)
#define LOG_INST_5(hcl,fmt,a1,a2,a3,a4,a5) HCL_LOG6(hcl, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3, a4, a5)
#define LOG_INST_6(hcl,fmt,a1,a2,a3,a4,a5,a6) HCL_LOG7(hcl, DECODE_LOG_MASK, " %06zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3, a4, a5, a6)


#define FETCH_BYTE_CODE(hcl) (cdptr[ip++])
#define FETCH_BYTE_CODE_TO(hcl,v_ooi) (v_ooi = FETCH_BYTE_CODE(hcl))
#if (HCL_CODE_LONG_PARAM_SIZE == 2)
#	define FETCH_PARAM_CODE_TO(hcl,v_ooi) \
		do { \
			v_ooi = FETCH_BYTE_CODE(hcl); \
			v_ooi = (v_ooi << 8) | FETCH_BYTE_CODE(hcl); \
		} while (0)
#else
#	define FETCH_PARAM_CODE_TO(hcl,v_ooi) (v_ooi = FETCH_BYTE_CODE(hcl))
#endif

/* TODO: check if ip shoots beyond the maximum length in fetching code and parameters */
int hcl_decode (hcl_t* hcl, hcl_oow_t start, hcl_oow_t end)
{
	hcl_oob_t bcode, * cdptr;
	hcl_ooi_t ip = start, fetched_instruction_pointer;
	hcl_oow_t b1, b2;

	/* the instruction at the offset 'end' is not decoded.
	 * decoding offset range is from start to end - 1. */

	HCL_ASSERT (hcl, start >= 0 && end >= 0);
	HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX); /* asserted by the compiler */
	HCL_ASSERT (hcl, end <= hcl->code.bc.len); /* not harmful though this fails */
	if (start >= hcl->code.bc.len)
	{
		hcl_seterrnum (hcl, HCL_EINVAL);
		return -1;
	}
	if (end > hcl->code.bc.len) end = hcl->code.bc.len;

	ip = start;
	cdptr = hcl->code.bc.ptr;

/* TODO: check if ip increases beyond bcode when fetching parameters too */
	while (ip < end)
	{
		fetched_instruction_pointer = ip;
		FETCH_BYTE_CODE_TO(hcl, bcode);

		switch (bcode)
		{
			case HCL_CODE_PUSH_INSTVAR_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				goto push_instvar;
			case HCL_CODE_PUSH_INSTVAR_0:
			case HCL_CODE_PUSH_INSTVAR_1:
			case HCL_CODE_PUSH_INSTVAR_2:
			case HCL_CODE_PUSH_INSTVAR_3:
			case HCL_CODE_PUSH_INSTVAR_4:
			case HCL_CODE_PUSH_INSTVAR_5:
			case HCL_CODE_PUSH_INSTVAR_6:
			case HCL_CODE_PUSH_INSTVAR_7:
				b1 = bcode & 0x7; /* low 3 bits */
			push_instvar:
				LOG_INST_1 (hcl, "push_instvar %zu", b1);
				break;

			/* ------------------------------------------------- */

			case HCL_CODE_STORE_INTO_INSTVAR_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				goto store_instvar;
			case HCL_CODE_STORE_INTO_INSTVAR_0:
			case HCL_CODE_STORE_INTO_INSTVAR_1:
			case HCL_CODE_STORE_INTO_INSTVAR_2:
			case HCL_CODE_STORE_INTO_INSTVAR_3:
			case HCL_CODE_STORE_INTO_INSTVAR_4:
			case HCL_CODE_STORE_INTO_INSTVAR_5:
			case HCL_CODE_STORE_INTO_INSTVAR_6:
			case HCL_CODE_STORE_INTO_INSTVAR_7:
				b1 = bcode & 0x7; /* low 3 bits */
			store_instvar:
				LOG_INST_1 (hcl, "store_into_instvar %zu", b1);
				break;

			case HCL_CODE_POP_INTO_INSTVAR_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				goto pop_into_instvar;
			case HCL_CODE_POP_INTO_INSTVAR_0:
			case HCL_CODE_POP_INTO_INSTVAR_1:
			case HCL_CODE_POP_INTO_INSTVAR_2:
			case HCL_CODE_POP_INTO_INSTVAR_3:
			case HCL_CODE_POP_INTO_INSTVAR_4:
			case HCL_CODE_POP_INTO_INSTVAR_5:
			case HCL_CODE_POP_INTO_INSTVAR_6:
			case HCL_CODE_POP_INTO_INSTVAR_7:
				b1 = bcode & 0x7; /* low 3 bits */
			pop_into_instvar:
				LOG_INST_1 (hcl, "pop_into_instvar %zu", b1);
				break;

			/* ------------------------------------------------- */
			case HCL_CODE_PUSH_TEMPVAR_X:
			case HCL_CODE_STORE_INTO_TEMPVAR_X:
			case HCL_CODE_POP_INTO_TEMPVAR_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				goto handle_tempvar;

			case HCL_CODE_PUSH_TEMPVAR_0:
			case HCL_CODE_PUSH_TEMPVAR_1:
			case HCL_CODE_PUSH_TEMPVAR_2:
			case HCL_CODE_PUSH_TEMPVAR_3:
			case HCL_CODE_PUSH_TEMPVAR_4:
			case HCL_CODE_PUSH_TEMPVAR_5:
			case HCL_CODE_PUSH_TEMPVAR_6:
			case HCL_CODE_PUSH_TEMPVAR_7:
			case HCL_CODE_STORE_INTO_TEMPVAR_0:
			case HCL_CODE_STORE_INTO_TEMPVAR_1:
			case HCL_CODE_STORE_INTO_TEMPVAR_2:
			case HCL_CODE_STORE_INTO_TEMPVAR_3:
			case HCL_CODE_STORE_INTO_TEMPVAR_4:
			case HCL_CODE_STORE_INTO_TEMPVAR_5:
			case HCL_CODE_STORE_INTO_TEMPVAR_6:
			case HCL_CODE_STORE_INTO_TEMPVAR_7:
			case HCL_CODE_POP_INTO_TEMPVAR_0:
			case HCL_CODE_POP_INTO_TEMPVAR_1:
			case HCL_CODE_POP_INTO_TEMPVAR_2:
			case HCL_CODE_POP_INTO_TEMPVAR_3:
			case HCL_CODE_POP_INTO_TEMPVAR_4:
			case HCL_CODE_POP_INTO_TEMPVAR_5:
			case HCL_CODE_POP_INTO_TEMPVAR_6:
			case HCL_CODE_POP_INTO_TEMPVAR_7:
				b1 = bcode & 0x7; /* low 3 bits */
			handle_tempvar:

				if ((bcode >> 4) & 1)
				{
					/* push - bit 4 on */
					LOG_INST_1 (hcl, "push_tempvar %zu", b1);
				}
				else
				{
					/* store or pop - bit 5 off */
					if ((bcode >> 3) & 1)
					{
						/* pop - bit 3 on */
						LOG_INST_1 (hcl, "pop_into_tempvar %zu", b1);
					}
					else
					{
						LOG_INST_1 (hcl, "store_into_tempvar %zu", b1);
					}
				}
				break;

			/* ------------------------------------------------- */
			case HCL_CODE_PUSH_LITERAL_X2:
				FETCH_PARAM_CODE_TO (hcl, b1);
				FETCH_PARAM_CODE_TO (hcl, b2);
				b1 = (b1 << (8 * HCL_CODE_LONG_PARAM_SIZE)) | b2;
				goto push_literal;

			case HCL_CODE_PUSH_LITERAL_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				goto push_literal;

			case HCL_CODE_PUSH_LITERAL_0:
			case HCL_CODE_PUSH_LITERAL_1:
			case HCL_CODE_PUSH_LITERAL_2:
			case HCL_CODE_PUSH_LITERAL_3:
			case HCL_CODE_PUSH_LITERAL_4:
			case HCL_CODE_PUSH_LITERAL_5:
			case HCL_CODE_PUSH_LITERAL_6:
			case HCL_CODE_PUSH_LITERAL_7:
				b1 = bcode & 0x7; /* low 3 bits */
			push_literal:
				LOG_INST_1 (hcl, "push_literal @%zu", b1);
				break;

			/* ------------------------------------------------- */
			case HCL_CODE_PUSH_OBJECT_X:
			case HCL_CODE_STORE_INTO_OBJECT_X:
			case HCL_CODE_POP_INTO_OBJECT_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				goto handle_object;

			case HCL_CODE_PUSH_OBJECT_0:
			case HCL_CODE_PUSH_OBJECT_1:
			case HCL_CODE_PUSH_OBJECT_2:
			case HCL_CODE_PUSH_OBJECT_3:
			case HCL_CODE_STORE_INTO_OBJECT_0:
			case HCL_CODE_STORE_INTO_OBJECT_1:
			case HCL_CODE_STORE_INTO_OBJECT_2:
			case HCL_CODE_STORE_INTO_OBJECT_3:
			case HCL_CODE_POP_INTO_OBJECT_0:
			case HCL_CODE_POP_INTO_OBJECT_1:
			case HCL_CODE_POP_INTO_OBJECT_2:
			case HCL_CODE_POP_INTO_OBJECT_3:
				b1 = bcode & 0x3; /* low 2 bits */
			handle_object:
				if ((bcode >> 3) & 1)
				{
					if ((bcode >> 2) & 1)
					{
						LOG_INST_1 (hcl, "pop_into_object @%zu", b1);
					}
					else
					{
						LOG_INST_1 (hcl, "store_into_object @%zu", b1);
					}
				}
				else
				{
					LOG_INST_1 (hcl, "push_object @%zu", b1);
				}
				break;

			/* -------------------------------------------------------- */

			case HCL_CODE_JUMP_FORWARD_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "jump_forward %zu", b1);
				break;

			case HCL_CODE_JUMP_FORWARD_0:
			case HCL_CODE_JUMP_FORWARD_1:
			case HCL_CODE_JUMP_FORWARD_2:
			case HCL_CODE_JUMP_FORWARD_3:
				LOG_INST_1 (hcl, "jump_forward %zu", (hcl_oow_t)(bcode & 0x3)); /* low 2 bits */
				break;

			case HCL_CODE_JUMP_BACKWARD_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "jump_backward %zu", b1);
				hcl->ip += b1;
				break;

			case HCL_CODE_JUMP_BACKWARD_0:
			case HCL_CODE_JUMP_BACKWARD_1:
			case HCL_CODE_JUMP_BACKWARD_2:
			case HCL_CODE_JUMP_BACKWARD_3:
				LOG_INST_1 (hcl, "jump_backward %zu", (hcl_oow_t)(bcode & 0x3)); /* low 2 bits */
				break;

			case HCL_CODE_JUMP_FORWARD_IF_TRUE:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "jump_forward_if_true %zu", b1);
				break;

			case HCL_CODE_JUMP2_FORWARD_IF_TRUE:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "jump2_forward_if_true %zu", b1);
				break;

			case HCL_CODE_JUMP_FORWARD_IF_FALSE:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "jump_forward_if_false %zu", b1);
				break;

			case HCL_CODE_JUMP2_FORWARD_IF_FALSE:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "jump2_forward_if_false %zu", b1);
				break;

			case HCL_CODE_JUMP2_FORWARD:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "jump2_forward %zu", b1);
				break;

			case HCL_CODE_JUMP_BACKWARD_IF_TRUE:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "jump_backward_if_true %zu", b1);
				break;

			case HCL_CODE_JUMP2_BACKWARD_IF_TRUE:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "jump2_backward_if_true %zu", b1);
				break;

			case HCL_CODE_JUMP_BACKWARD_IF_FALSE:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "jump_backward_if_false %zu", b1);
				break;

			case HCL_CODE_JUMP2_BACKWARD_IF_FALSE:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "jump2_backward_if_false %zu", b1);
				break;

			case HCL_CODE_JUMP2_BACKWARD:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "jump2_backward %zu", b1);
				break;

			/* -------------------------------------------------------- */
			case HCL_CODE_PUSH_RETURN_R:
				LOG_INST_0 (hcl, "push_return_r");
				break;

			case HCL_CODE_CALL_R:
				FETCH_PARAM_CODE_TO (hcl, b1); /* nargs */
				FETCH_PARAM_CODE_TO (hcl, b2); /* nrvars */
				LOG_INST_2 (hcl, "call %zu %zu", b1, b2);
				break;

			case HCL_CODE_CALL_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				goto handle_call;

			case HCL_CODE_CALL_0:
			case HCL_CODE_CALL_1:
			case HCL_CODE_CALL_2:
			case HCL_CODE_CALL_3:
				b1 = bcode & 0x3; /* low 2 bits */
			handle_call:
				LOG_INST_1 (hcl, "call %zu", b1);
				break;

			/* -------------------------------------------------------- */
			case HCL_CODE_TRY_ENTER:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "try_enter %zu", b1);
				break;

			case HCL_CODE_TRY_ENTER2:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "try_enter2 %zu", b1);
				break;

			case HCL_CODE_TRY_EXIT:
				LOG_INST_0 (hcl, "try_exit");
				break;

			case HCL_CODE_THROW:
				LOG_INST_0 (hcl, "throw");
				break;
			/* -------------------------------------------------------- */
			case HCL_CODE_CLASS_ENTER:
			{
				hcl_oow_t b3;
				
				FETCH_PARAM_CODE_TO (hcl, b1);
				FETCH_PARAM_CODE_TO (hcl, b2);
				FETCH_PARAM_CODE_TO (hcl, b3);
				LOG_INST_3 (hcl, "class_enter %zu %zu %zu", b1, b2, b3);

				break;
			}

			case HCL_CODE_CLASS_EXIT:
				LOG_INST_0 (hcl, "class_exit");
				break;

			case HCL_CODE_CLASS_PEXIT:
				LOG_INST_0 (hcl, "class_pexit");
				break;

			case HCL_CODE_CLASS_MSTORE:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "class_mstore %zu", b1);
				break;
			/* -------------------------------------------------------- */

			case HCL_CODE_PUSH_CTXTEMPVAR_X:
			case HCL_CODE_STORE_INTO_CTXTEMPVAR_X:
			case HCL_CODE_POP_INTO_CTXTEMPVAR_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				FETCH_PARAM_CODE_TO (hcl, b2);
				goto handle_ctxtempvar;
			case HCL_CODE_PUSH_CTXTEMPVAR_0:
			case HCL_CODE_PUSH_CTXTEMPVAR_1:
			case HCL_CODE_PUSH_CTXTEMPVAR_2:
			case HCL_CODE_PUSH_CTXTEMPVAR_3:
			case HCL_CODE_STORE_INTO_CTXTEMPVAR_0:
			case HCL_CODE_STORE_INTO_CTXTEMPVAR_1:
			case HCL_CODE_STORE_INTO_CTXTEMPVAR_2:
			case HCL_CODE_STORE_INTO_CTXTEMPVAR_3:
			case HCL_CODE_POP_INTO_CTXTEMPVAR_0:
			case HCL_CODE_POP_INTO_CTXTEMPVAR_1:
			case HCL_CODE_POP_INTO_CTXTEMPVAR_2:
			case HCL_CODE_POP_INTO_CTXTEMPVAR_3:
				b1 = bcode & 0x3; /* low 2 bits */
				FETCH_BYTE_CODE_TO (hcl, b2);

			handle_ctxtempvar:
				if ((bcode >> 3) & 1)
				{
					/* store or pop */

					if ((bcode >> 2) & 1)
					{
						LOG_INST_2 (hcl, "pop_into_ctxtempvar %zu %zu", b1, b2);
					}
					else
					{
						LOG_INST_2 (hcl, "store_into_ctxtempvar %zu %zu", b1, b2);
					}
				}
				else
				{
					/* push */
					LOG_INST_2 (hcl, "push_ctxtempvar %zu %zu", b1, b2);
				}

				break;
			/* -------------------------------------------------------- */

			case HCL_CODE_PUSH_OBJVAR_X:
			case HCL_CODE_STORE_INTO_OBJVAR_X:
			case HCL_CODE_POP_INTO_OBJVAR_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				FETCH_PARAM_CODE_TO (hcl, b2);
				goto handle_objvar;

			case HCL_CODE_PUSH_OBJVAR_0:
			case HCL_CODE_PUSH_OBJVAR_1:
			case HCL_CODE_PUSH_OBJVAR_2:
			case HCL_CODE_PUSH_OBJVAR_3:
			case HCL_CODE_STORE_INTO_OBJVAR_0:
			case HCL_CODE_STORE_INTO_OBJVAR_1:
			case HCL_CODE_STORE_INTO_OBJVAR_2:
			case HCL_CODE_STORE_INTO_OBJVAR_3:
			case HCL_CODE_POP_INTO_OBJVAR_0:
			case HCL_CODE_POP_INTO_OBJVAR_1:
			case HCL_CODE_POP_INTO_OBJVAR_2:
			case HCL_CODE_POP_INTO_OBJVAR_3:
				/* b1 -> variable index to the object indicated by b2.
				 * b2 -> object index stored in the literal frame. */
				b1 = bcode & 0x3; /* low 2 bits */
				FETCH_BYTE_CODE_TO (hcl, b2);

			handle_objvar:
				if ((bcode >> 3) & 1)
				{
					/* store or pop */
					if ((bcode >> 2) & 1)
					{
						LOG_INST_2 (hcl, "pop_into_objvar %zu %zu", b1, b2);
					}
					else
					{
						LOG_INST_2 (hcl, "store_into_objvar %zu %zu", b1, b2);
					}
				}
				else
				{
					LOG_INST_2 (hcl, "push_objvar %zu %zu", b1, b2);
				}

				break;

			/* -------------------------------------------------------- */

			case HCL_CODE_SEND_R:
				FETCH_PARAM_CODE_TO (hcl, b1); /* nargs */
				FETCH_PARAM_CODE_TO (hcl, b2); /* nrvars */
				LOG_INST_2 (hcl, "send_r %zu %zu", b1, b2);
				break;
			case HCL_CODE_SEND_TO_SUPER_R:
				FETCH_PARAM_CODE_TO (hcl, b1); /* nargs */
				FETCH_PARAM_CODE_TO (hcl, b2); /* nrvars */
				LOG_INST_2 (hcl, "send_to_super_r %zu %zu", b1, b2);
				break;

			case HCL_CODE_SEND_X:
			case HCL_CODE_SEND_TO_SUPER_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				goto handle_send;

			case HCL_CODE_SEND_0:
			case HCL_CODE_SEND_1:
			case HCL_CODE_SEND_2:
			case HCL_CODE_SEND_3:
			case HCL_CODE_SEND_TO_SUPER_0:
			case HCL_CODE_SEND_TO_SUPER_1:
			case HCL_CODE_SEND_TO_SUPER_2:
			case HCL_CODE_SEND_TO_SUPER_3:
				b1 = bcode & 0x3; /* low 2 bits */

			handle_send:
				LOG_INST_2 (hcl, "send%hs %zu", (((bcode >> 2) & 1)? "_to_super": ""), b1);
				break;

			/* -------------------------------------------------------- */

			case HCL_CODE_PUSH_CLSVAR_I_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "push_clsvar_i %zu", b1);
				break;

			case HCL_CODE_STORE_INTO_CLSVAR_I_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "store_into_clsvar_i %zu", b1);
				break;

			case HCL_CODE_POP_INTO_CLSVAR_I_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "pop_into_clsvar_i %zu", b1);
				break;
				
			/* -------------------------------------------------------- */				

			case HCL_CODE_PUSH_CLSVAR_M_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "push_clsvar_m %zu", b1);
				break;

			case HCL_CODE_STORE_INTO_CLSVAR_M_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "store_into_clsvar_m %zu", b1);
				break;

			case HCL_CODE_POP_INTO_CLSVAR_M_X:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "pop_into_clsvar_m %zu", b1);
				break;

			/* -------------------------------------------------------- */

			case HCL_CODE_PUSH_RECEIVER:
				LOG_INST_0 (hcl, "push_receiver");
				break;

			case HCL_CODE_PUSH_NIL:
				LOG_INST_0 (hcl, "push_nil");
				break;

			case HCL_CODE_PUSH_TRUE:
				LOG_INST_0 (hcl, "push_true");
				break;

			case HCL_CODE_PUSH_FALSE:
				LOG_INST_0 (hcl, "push_false");
				break;

			case HCL_CODE_PUSH_CONTEXT:
				LOG_INST_0 (hcl, "push_context");
				break;

			case HCL_CODE_PUSH_PROCESS:
				LOG_INST_0 (hcl, "push_process");
				break;

			case HCL_CODE_PUSH_NEGONE:
				LOG_INST_0 (hcl, "push_negone");
				break;

			case HCL_CODE_PUSH_ZERO:
				LOG_INST_0 (hcl, "push_zero");
				break;

			case HCL_CODE_PUSH_ONE:
				LOG_INST_0 (hcl, "push_one");
				break;

			case HCL_CODE_PUSH_TWO:
				LOG_INST_0 (hcl, "push_two");
				break;

			case HCL_CODE_PUSH_INTLIT:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "push_intlit %zu", b1);
				break;

			case HCL_CODE_PUSH_NEGINTLIT:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "push_negintlit %zu", b1);
				break;

			case HCL_CODE_PUSH_CHARLIT:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "push_charlit %zu", b1);
				break;
			/* -------------------------------------------------------- */

			case HCL_CODE_MAKE_ARRAY:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "make_array %zu", b1);
				break;

			case HCL_CODE_POP_INTO_ARRAY:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "pop_into_array %zu", b1);
				break;

			case HCL_CODE_MAKE_BYTEARRAY:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "make_bytearray %zu", b1);
				break;

			case HCL_CODE_POP_INTO_BYTEARRAY:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "pop_into_bytearray %zu", b1);
				break;

			case HCL_CODE_MAKE_DIC:
				FETCH_PARAM_CODE_TO (hcl, b1);
				LOG_INST_1 (hcl, "make_dic %zu", b1);
				break;

			case HCL_CODE_POP_INTO_DIC:
				LOG_INST_0 (hcl, "pop_into_dic");
				break;

			case HCL_CODE_MAKE_CONS:
				LOG_INST_0 (hcl, "make_cons");
				break;

			case HCL_CODE_POP_INTO_CONS:
				LOG_INST_0 (hcl, "pop_into_cons");
				break;

			case HCL_CODE_POP_INTO_CONS_END:
				LOG_INST_0 (hcl, "pop_into_cons_end");
				break;

			case HCL_CODE_POP_INTO_CONS_CDR:
				LOG_INST_0 (hcl, "pop_into_cons_cdr");
				break;
			/* -------------------------------------------------------- */

			case HCL_CODE_DUP_STACKTOP:
				LOG_INST_0 (hcl, "dup_stacktop");
				break;

			case HCL_CODE_POP_STACKTOP:
				LOG_INST_0 (hcl, "pop_stacktop");
				break;

			case HCL_CODE_RETURN_STACKTOP:
				LOG_INST_0 (hcl, "return_stacktop");
				break;

			case HCL_CODE_RETURN_RECEIVER:
				LOG_INST_0 (hcl, "return_receiver");
				break;

			case HCL_CODE_RETURN_FROM_BLOCK:
				LOG_INST_0 (hcl, "return_from_block");
				break;

			case HCL_CODE_MAKE_FUNCTION:
			{
				hcl_oow_t b3, b4;
				/* b1 - block temporaries mask 
				 * b2 - block temporaries mask
				 * b3 - base literal frame start
				 * b4 - base literal frame end */
				FETCH_PARAM_CODE_TO (hcl, b1);
				FETCH_PARAM_CODE_TO (hcl, b2);
				FETCH_PARAM_CODE_TO (hcl, b3);
				FETCH_PARAM_CODE_TO (hcl, b4);

				b1 = (b1 << (8 * HCL_CODE_LONG_PARAM_SIZE)) | b2;
				LOG_INST_6 (hcl, "make_function %zu %zu %zu %zu %zu %zu", 
					GET_BLKTMPR_MASK_VA(b1),
					GET_BLKTMPR_MASK_NARGS(b1),
					GET_BLKTMPR_MASK_NRVARS(b1),
					GET_BLKTMPR_MASK_NLVARS(b1),
					b3, b4);

				HCL_ASSERT (hcl, b1 >= 0);
				break;
			}

			case HCL_CODE_MAKE_BLOCK:
				/* b1 - block temporaries mask
				 * b2 - block temporaries mask */
				FETCH_PARAM_CODE_TO (hcl, b1);
				FETCH_PARAM_CODE_TO (hcl, b2);
				b1 = (b1 << (8 * HCL_CODE_LONG_PARAM_SIZE)) | b2;

				LOG_INST_4 (hcl, "make_block %zu %zu %zu %zu", 
					GET_BLKTMPR_MASK_VA(b1),
					GET_BLKTMPR_MASK_NARGS(b1),
					GET_BLKTMPR_MASK_NRVARS(b1),
					GET_BLKTMPR_MASK_NLVARS(b1));

				HCL_ASSERT (hcl, b1 >= 0);
				break;

			case HCL_CODE_NOOP:
				/* do nothing */
				LOG_INST_0 (hcl, "noop");
				break;

			default:
				LOG_INST_1 (hcl, "UNKNOWN BYTE CODE ENCOUNTERED %x", (int)bcode);
				hcl_seterrnum (hcl, HCL_EINTERN);
				break;
		}
	}


/* TODO: this needs changes... */
	/* print literal frame contents */
	for (ip = 0; ip < hcl->code.lit.len; ip++)
	{
		HCL_LOG2(hcl, DECODE_LOG_MASK, "@%-9zd %O\n", ip, ((hcl_oop_oop_t)hcl->code.lit.arr)->slot[ip]);
	}

	return 0;
}

