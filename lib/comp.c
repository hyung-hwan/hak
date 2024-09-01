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

/* limit the `do` expression to have not more than 1 expression and
 * no variable declaration if not enclosed in parentheses */
#define LANG_LIMIT_DO

#define FOR_NONE  (0)
#define FOR_IF    (1)
#define FOR_TRY   (2)
#define FOR_CLASS (3)

enum
{
	VAR_NAMED,
	VAR_INDEXED,
	VAR_INST, /* instance variable */
	VAR_CLASS_I, /* class variable in class initialization scope */
	VAR_CLASS_CM, /* class variable in class method scope */
	VAR_CLASS_IM /* class variable in instance method scope */
};

enum
{
	VAR_ACCESS_PUSH,
	VAR_ACCESS_POP,
	VAR_ACCESS_STORE
};

enum
{
	/* these enumerators are stored in the lower 8 bits of
	 * the fun_type field of hcl_fnblk_info_t.
	 * the 9th bit of the field indicate a method is defined
	 * out of a class */

	FUN_PLAIN, /* plain function */
	FUN_IM,    /* instance method */
	FUN_CM,    /* class method */
	FUN_CIM    /* class instantiation method */
};

#define TV_BUFFER_ALIGN 256
#define BLK_INFO_BUFFER_ALIGN 128

/* --------------------------------------------


(defun plus(x y)
	(printf "plus %d %d\n" x y)
	(defun minus(x y)
		(printf "minus %d %d\n" x y)
		(- x y)
	)
	(+ x y)
)

(defun dummy(q)
	(printf "%s\n" q)
)

(plus 10 20)
    <---- minus is now available
(minus 10 1)
literals -->
//
// characeter 'A'
// "string"
// B"byte string" <-- not yet implemented
// array ---> #[   ] or [ ] ? constant or not?  dynamic???
// hash table - dictionary  ---> #{   }
// the rest must be manipulated with code...
------------------------------ */

int hcl_copy_string_to (hcl_t* hcl, const hcl_oocs_t* src, hcl_oocs_t* dst, hcl_oow_t* dstcapa, int append, hcl_ooch_t delim_char)
{
	hcl_oow_t len, pos;

	if (append)
	{
		pos = dst->len;
		len = dst->len + src->len;
		if (delim_char != '\0') len++;
	}
	else
	{
		pos = 0;
		len = src->len;
	}

	if (len >= *dstcapa)
	{
		hcl_ooch_t* tmp;
		hcl_oow_t capa;

		capa = HCL_ALIGN(len + 1, TV_BUFFER_ALIGN);

		tmp = (hcl_ooch_t*)hcl_reallocmem(hcl, dst->ptr, HCL_SIZEOF(*tmp) * capa);
		if (HCL_UNLIKELY(!tmp))
		{
			const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
			hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "failed to grow string buffer - %js", orgmsg);
			return -1;
		}

		dst->ptr = tmp;
		*dstcapa = capa - 1;
	}

	if (append && delim_char != '\0') dst->ptr[pos++] = delim_char;
	hcl_copy_oochars (&dst->ptr[pos], src->ptr, src->len);
	dst->ptr[len] = '\0';
	dst->len = len;
	return 0;
}

static int __find_word_in_string (const hcl_oocs_t* haystack, const hcl_oocs_t* name, int last, hcl_oow_t* xindex)
{
	/* this function is inefficient. but considering the typical number
	 * of arguments and temporary variables, the inefficiency can be
	 * ignored in my opinion. the overhead to maintain the reverse lookup
	 * table from a name to an index should be greater than this simple
	 * inefficient lookup */

	hcl_ooch_t* t, * e;
	hcl_oow_t index, i, found;

	t = haystack->ptr;
	e = t + haystack->len;
	index = 0;
	found = HCL_TYPE_MAX(hcl_oow_t);

	while (t < e)
	{
		while (t < e && *t == ' ') t++;

		for (i = 0; i < name->len; i++)
		{
			if (t >= e || name->ptr[i] != *t) goto unmatched;
			t++;
		}
		if (t >= e || *t == ' ')
		{
			if (last)
			{
				/* if last is true, find the last matching entry */
				found = index;
			}
			else
			{
				if (xindex) *xindex = index;
				return 0; /* found */
			}
		}

	unmatched:
		while (t < e)
		{
			if (*t == ' ')
			{
				t++;
				break;
			}
			t++;
		}

		index++;
	}

	if (found != HCL_TYPE_MAX(hcl_oow_t))
	{
		if (xindex) *xindex = found;
		return 0; /* found */
	}

	return -1; /* not found */
}

static int add_temporary_variable (hcl_t* hcl, const hcl_oocs_t* name, hcl_oow_t dup_check_start)
{
	hcl_oocs_t s;
	int x;

	s.ptr = hcl->c->tv.s.ptr + dup_check_start;
	s.len = hcl->c->tv.s.len - dup_check_start;
	if (__find_word_in_string(&s, name, 0, HCL_NULL) >= 0)
	{
		hcl_seterrnum (hcl, HCL_EEXIST);
		return -1;
	}
	x = hcl_copy_string_to(hcl, name, &hcl->c->tv.s, &hcl->c->tv.capa, 1, ' ');
	if (HCL_LIKELY(x >= 0)) hcl->c->tv.wcount++;
	return x;
}

static int kill_temporary_variables (hcl_t* hcl, hcl_oow_t start_wpos, hcl_oow_t end_wpos)
{
	/* this function doesn't remove the added temporary variable nor does it lower the word count.
	 * it simply changes a word at the given postion to some garbage characters so that
	 * the variable can't be found in the search */
	hcl_oow_t i;

	for (i = start_wpos; i < end_wpos; i++)
	{
		if (hcl->c->tv.s.ptr[i] != ' ')
		{
			hcl->c->tv.s.ptr[i] = '('; /* HACK!! put a special character which can't form a variable name */
		}
	}
	return 0;
}

static void kill_temporary_variable_at_offset (hcl_t* hcl, hcl_oow_t offset)
{
	/* this is a hacky function. it's better to implement kill_temporary_variables() which uses word positions */
	HCL_ASSERT (hcl, offset < hcl->c->tv.s.len);
	HCL_ASSERT (hcl, hcl->c->tv.s.ptr[offset] != ' ');
	hcl->c->tv.s.ptr[offset] = '('; /* HACK!! put a special character which can't form a variable name */
}

static int is_in_top_scope (hcl_t* hcl)
{
	hcl_fnblk_info_t* fbi;
/*printf (">>> ---- fnblk.depth ....%d\n", (int)hcl->c->fnblk.depth);*/
	if (hcl->c->fnblk.depth > 0) return 0;
	HCL_ASSERT (hcl, hcl->c->fnblk.depth >= 0);
	fbi = &hcl->c->fnblk.info[hcl->c->fnblk.depth];
/*printf ("fbi->clsblk_top....%d\n", (int)fbi->clsblk_top);*/
	return fbi->clsblk_top < 0;
}

static int is_in_top_fun_scope (hcl_t* hcl)
{
	return hcl->c->fnblk.depth == 0;
}

static int is_in_class_init_scope (hcl_t* hcl)
{
	hcl_fnblk_info_t* fbi;
	HCL_ASSERT (hcl, hcl->c->fnblk.depth >= 0);
	fbi = &hcl->c->fnblk.info[hcl->c->fnblk.depth];
	return fbi->clsblk_top >= 0;
}

static int is_in_class_method_scope (hcl_t* hcl)
{
	hcl_oow_t i;

	HCL_ASSERT (hcl, hcl->c->fnblk.depth >= 0);
	for (i = hcl->c->fnblk.depth + 1; i > 0; )
	{
		hcl_fnblk_info_t* fbi;

		fbi = &hcl->c->fnblk.info[--i];

		if (fbi->clsblk_top >= 0)
		{
			if (i >= hcl->c->fnblk.depth) return 0; /* in class initialization scope */
			return 1; /* in class method scope */
		}
	}

	return 0; /* in plain function scope */
}

static int find_variable_backward_with_word (hcl_t* hcl, const hcl_oocs_t* name, const hcl_loc_t* loc, int class_level_only, hcl_var_info_t* vi)
{
	hcl_oow_t i;

	HCL_ASSERT (hcl, hcl->c->fnblk.depth >= 0);
	HCL_ASSERT (hcl, hcl->c->fnblk.info[hcl->c->fnblk.depth].tmprlen == hcl->c->tv.s.len);

	/* depth begins at -1. so it is the actual index. let looping begin at depth + 1
	 * to avoid an extra exit check without it */
	for (i = hcl->c->fnblk.depth + 1; i > 0; )
	{
		hcl_fnblk_info_t* fbi;
		hcl_oocs_t haystack;
		hcl_oow_t parent_tmprcnt, parent_tmprlen, index;

		fbi = &hcl->c->fnblk.info[--i];

		if (fbi->clsblk_top >= 0)
		{
			/* this function block has a class defined.
			 * that is, it is in a class defintion.
			 * variable lookup must be limited to the class scope */
			hcl_clsblk_info_t* clsbi;

		#if 0
			for (j = fbi->clsblk_top + 1; j > fbi->clsblk_base; )
			{
				clsbi = &hcl->c->clsblk.info[--j];
		#endif
				clsbi = &hcl->c->clsblk.info[fbi->clsblk_top];

				if (clsbi->ivars_str)
				{
					haystack.ptr = clsbi->ivars_str;
					haystack.len = hcl_count_oocstr(clsbi->ivars_str);
					if (__find_word_in_string(&haystack, name, 1, &index) >= 0)
					{
						hcl_oow_t fi;

						if (i >= hcl->c->fnblk.depth)
						{
							/* instance variables are accessible only in an instance method defintion scope.
							 * it is in class initialization scope */
							hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNED, loc, name, "prohibited access to instance variable");
							return -1;
						}

						for (fi = hcl->c->fnblk.depth + 1; fi > i; ) /* TOOD: review this loop for correctness */
						{
							/* 'i' is the function level that holds the class defintion block. the check must not go past it */
							if ((hcl->c->fnblk.info[--fi].fun_type & 0xFF) == FUN_CM)
							{
								/* the function where this variable is defined is a class method or an plain function block within a class method*/
								hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNED, loc, name, "prohibited access to instance variable in class method context");
								return -1;
							}

							/* instance methods and instantiation methods can access instance variables */
							if ((hcl->c->fnblk.info[fi].fun_type & 0xFF) != FUN_PLAIN) break;
						}

						vi->type = VAR_INST;
						vi->ctx_offset = 0;
						vi->index_in_ctx = index;
/*
HCL_INFO6 (hcl, "FOUND INST VAR [%.*js]...[%.*js]................ ===> ctx_offset %d index %d\n",
	haystack.len, haystack.ptr, name->len, name->ptr, (int)(vi->ctx_offset), (int)vi->index_in_ctx);
*/
						return 1;
					}
				}

				if (clsbi->cvars_str)
				{
					haystack.ptr = clsbi->cvars_str;
					haystack.len = hcl_count_oocstr(clsbi->cvars_str);
					if (__find_word_in_string(&haystack, name, 1, &index) >= 0)
					{
						/* TODO: VAR_CLASS_CM vs VAR_CLASS_IM, need to know if it's an instance method or a class method */
/* TODO: check if it's in the class variable .... */
						vi->type = (i >= hcl->c->fnblk.depth? VAR_CLASS_I: VAR_CLASS_IM);
						vi->ctx_offset = 0;
						vi->index_in_ctx = index;
/*
HCL_INFO6 (hcl, "FOUND CLASS VAR [%.*js]...[%.*js]................ ===> ctx_offset %d index %d\n",
	haystack.len, haystack.ptr, name->len, name->ptr, (int)(vi->ctx_offset), (int)vi->index_in_ctx);
*/
						return 1;
					}
				}
	#if 0
			}

			if (i == hcl->c->fnblk.depth)
			{
				/* this condition indicates that the current function level contains a class defintion
				 * and this variable is looked up inside the class defintion */
HCL_INFO2 (hcl, "CLASS NAMED VAR [%.*js]\n", name->len, name->ptr);
				vi->type = VAR_CLASS;//_NAMED; // TODO: create VAR_CLASS_NAMED???
				vi->ctx_offset = 0;
				vi->index_in_ctx = 0;
			}
	#endif

			break; /* stop searching beyond class definition */
		}

		if (class_level_only) continue; /* skip local variable declarations */

		if (HCL_LIKELY(i > 0))
		{
			parent_tmprlen = hcl->c->fnblk.info[i - 1].tmprlen;
			parent_tmprcnt = hcl->c->fnblk.info[i - 1].tmprcnt;
		}
		else
		{
			parent_tmprlen = 0;
			parent_tmprcnt = 0;
		}

		/* narrow the search scope to the current block */
		haystack.ptr = &hcl->c->tv.s.ptr[parent_tmprlen];
		haystack.len = fbi->tmprlen - parent_tmprlen;

		if (__find_word_in_string(&haystack, name, 1, &index) >= 0)
		{
			/* temporary variables or arguments */
			vi->type = VAR_INDEXED;
			vi->ctx_offset = hcl->c->fnblk.depth - i; /* context offset */
			vi->index_in_ctx = index;
/*HCL_INFO4 (hcl, "FOUND ...[%.*js]................ ===> ctx_offset %d index %d\n", name->len, name->ptr, (int)(vi->ctx_offset), (int)vi->index_in_ctx);*/

			if (vi->ctx_offset > 0)
			{
				/* the current function block accesses temporaries in an outer function block */
				hcl->c->fnblk.info[hcl->c->fnblk.depth].access_outer = 1;
				/* temporaries in an outer function block is accessed by the current function block */

				if (i > 0) hcl->c->fnblk.info[i - 1].accessed_by_inner = 1;
			}

			return 1;
		}
	}

/*HCL_INFO2 (hcl, "NOT FOUND => %.*js\n", name->len, name->ptr); */
	return 0; /* not found */
}

static int find_variable_backward_with_token (hcl_t* hcl, const hcl_cnode_t* cnode, hcl_var_info_t* vi)
{
	if (HCL_CNODE_IS_DSYMBOL_CLA(cnode))
	{
		/* prefixed with self or super. remove the first segment */
		hcl_oocs_t newtok;
		newtok = *HCL_CNODE_GET_TOK(cnode);
		while (*newtok.ptr != '.')
		{
			newtok.ptr++;
			newtok.len--;
		}
		newtok.ptr++;
		newtok.len--;
		return find_variable_backward_with_word(hcl, &newtok, HCL_CNODE_GET_LOC(cnode), 1, vi);
	}

	return find_variable_backward_with_word(hcl, HCL_CNODE_GET_TOK(cnode), HCL_CNODE_GET_LOC(cnode), HCL_CNODE_IS_DSYMBOL_CLA(cnode), vi);
}

/* ========================================================================= */

static int check_block_expression_as_body (hcl_t* hcl, hcl_cnode_t* c, const hcl_cnode_t* ctx, int for_what)
{
	hcl_cnode_t* car = HCL_NULL, * cdr;

	/* variable declaration is disallowed.
	 * a block expression is allowed if not followed by another expression.
	 * unlike the function name, other types of expressions are allowed if not followed by another expression.
	 */
	if (!c || !HCL_CNODE_IS_CONS(c)) goto no_block; /* not cons */

	car = HCL_CNODE_CONS_CAR(c);
	if (!car || (HCL_CNODE_IS_CONS_CONCODED(car, HCL_CONCODE_VLIST) ||
	             HCL_CNODE_IS_ELIST_CONCODED(car, HCL_CONCODE_VLIST)))
	{
	no_block:
		hcl_setsynerrbfmt (
			hcl, HCL_SYNERR_BLOCK, (car? HCL_CNODE_GET_LOC(car): c? HCL_CNODE_GET_LOC(c): HCL_CNODE_GET_LOC(ctx)), HCL_NULL,
			"block expression expected as '%.*js' body", HCL_CNODE_GET_TOKLEN(ctx), HCL_CNODE_GET_TOKPTR(ctx)
		);
		return -1;
	}

	if (for_what == FOR_CLASS)
	{
		/* the class body must be enclosed in { .. }. e.g class X { ... } */
		if (!HCL_CNODE_IS_CONS_CONCODED(car, HCL_CONCODE_BLOCK) &&
		    !HCL_CNODE_IS_ELIST_CONCODED(car, HCL_CONCODE_BLOCK)) goto no_block;
	}

	/* there are special words that can't start a new expression */
	if (HCL_CNODE_IS_TYPED(car, HCL_CNODE_ELIF) ||
	    HCL_CNODE_IS_TYPED(car, HCL_CNODE_ELSE) ||
	    HCL_CNODE_IS_TYPED(car, HCL_CNODE_CATCH))
	{
		goto no_block;
	}

	if (HCL_CNODE_IS_SYMBOL_SYNCODED(car, HCL_SYNCODE_ELIF) ||
	    HCL_CNODE_IS_SYMBOL_SYNCODED(car, HCL_SYNCODE_ELSE) ||
	    HCL_CNODE_IS_SYMBOL_SYNCODED(car, HCL_SYNCODE_CATCH))
	{
		goto no_block;
	}

	cdr = HCL_CNODE_CONS_CDR(c);
	if (cdr)
	{
		/* there is redundant expression after the block expression */
		if (HCL_CNODE_IS_CONS(cdr))
		{
			hcl_cnode_t* nxt;
			nxt = HCL_CNODE_CONS_CAR(cdr);

			if (for_what == FOR_IF)
			{
				/* after the body for `if` or `elif`, there can come `elif` or `else` */
				if (HCL_CNODE_IS_TYPED(nxt, HCL_CNODE_ELIF) ||
				    HCL_CNODE_IS_TYPED(nxt, HCL_CNODE_ELSE)) goto ok;
				if (HCL_CNODE_IS_SYMBOL(nxt))
				{
					int syncode = HCL_CNODE_SYMBOL_SYNCODE(nxt);
					if (syncode == HCL_SYNCODE_ELIF || syncode == HCL_SYNCODE_ELSE) goto ok;
				}
			}
			else if (for_what == FOR_TRY)
			{
				if (HCL_CNODE_IS_TYPED(nxt, HCL_CNODE_CATCH)) goto ok;
				if (HCL_CNODE_IS_SYMBOL(nxt))
				{
					int syncode = HCL_CNODE_SYMBOL_SYNCODE(nxt);
					if (syncode == HCL_SYNCODE_CATCH) goto ok;
				}
			}
		}

		hcl_setsynerrbfmt (
			hcl, HCL_SYNERR_BANNED, HCL_CNODE_GET_LOC(cdr), HCL_NULL,
			"redundant expression prohibited after '%.*js' body", HCL_CNODE_GET_TOKLEN(ctx), HCL_CNODE_GET_TOKPTR(ctx)
		);
		return -1;
	}

ok:
	return 0;
}

/* ========================================================================= */

static HCL_INLINE int add_literal (hcl_t* hcl, hcl_oop_t obj, hcl_oow_t* index)
{
	hcl_oow_t lfbase;
	lfbase = (hcl->option.trait & HCL_TRAIT_INTERACTIVE)? hcl->c->fnblk.info[hcl->c->fnblk.depth].lfbase: 0;
	return hcl_addliteraltocode(hcl, &hcl->code, obj, lfbase, index);
}

/* ========================================================================= */

static HCL_INLINE void patch_instruction (hcl_t* hcl, hcl_oow_t index, hcl_oob_t bc)
{
	HCL_ASSERT (hcl, index < hcl->code.bc.len);
	hcl->code.bc.ptr[index] = bc;
}

static int emit_byte_instruction (hcl_t* hcl, hcl_oob_t bc, const hcl_loc_t* srcloc)
{
	/* the context object has the ip field. it should be representable
	 * in a small integer. for simplicity, limit the total byte code length
	 * to fit in a small integer. because 'ip' points to the next instruction
	 * to execute, the upper bound should be (max - 1) so that 'ip' stays
	 * at the max when incremented */
	if (hcl->code.bc.len == HCL_SMOOI_MAX - 1)
	{
		hcl_seterrnum (hcl, HCL_EBCFULL); /* byte code full/too big */
		return -1;
	}

	if (hcl->code.bc.len >= hcl->code.bc.capa)
	{
		hcl_oow_t newcapa;
		hcl_oob_t* tmp;
		hcl_dbgi_t* tmp2;

		newcapa = HCL_ALIGN(hcl->code.bc.capa + 1, HCL_BC_BUFFER_ALIGN);
		tmp = (hcl_oob_t*)hcl_reallocmem(hcl, hcl->code.bc.ptr, HCL_SIZEOF(*tmp) * newcapa);
		if (HCL_UNLIKELY(!tmp))
		{
			const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
			hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "failed to grow byte code buffer - %js", orgmsg);
			return -1;
		}

		tmp2 = (hcl_dbgi_t*)hcl_reallocmem(hcl, hcl->code.dbgi, HCL_SIZEOF(*tmp2) * newcapa);
		if (HCL_UNLIKELY(!tmp2))
		{
			const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
			hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "failed to grow debug info buffer - %js", orgmsg);
			hcl_freemem (hcl, tmp);
			return -1;
		}
		HCL_MEMSET (&tmp2[hcl->code.bc.capa], 0, HCL_SIZEOF(*tmp2) * (newcapa - hcl->code.bc.capa));

		hcl->code.bc.ptr = tmp;
		hcl->code.bc.capa = newcapa;
		hcl->code.dbgi = tmp2;
	}

	hcl->code.bc.ptr[hcl->code.bc.len] = bc;

	if (srcloc)
	{
		hcl->code.dbgi[hcl->code.bc.len].fname = srcloc->file;
		hcl->code.dbgi[hcl->code.bc.len].sline = srcloc->line;
	}

	hcl->code.bc.len++;
	return 0;
}

/*
COMMENTED OUT TEMPORARILY
int hcl_emitbyteinstruction (hcl_t* hcl, hcl_oob_t bc)
{
	return emit_byte_instruction(hcl, bc, HCL_NULL);
}*/

static int emit_single_param_instruction (hcl_t* hcl, int cmd, hcl_oow_t param_1, const hcl_loc_t* srcloc)
{
	hcl_oob_t bc;

	switch (cmd)
	{
		case HCL_CODE_PUSH_IVAR_0:
		case HCL_CODE_STORE_INTO_IVAR_0:
		case HCL_CODE_POP_INTO_IVAR_0:
		case HCL_CODE_PUSH_TEMPVAR_0:
		case HCL_CODE_STORE_INTO_TEMPVAR_0:
		case HCL_CODE_POP_INTO_TEMPVAR_0:
			if (param_1 < 8)
			{
				/* low 3 bits to hold the parameter */
				bc = (hcl_oob_t)(cmd & 0xF8) | (hcl_oob_t)param_1;
				goto write_short;
			}
			else
			{
				/* convert the code to a long version */
				bc = cmd | 0x80;
				goto write_long;
			}

		case HCL_CODE_PUSH_LITERAL_0:
			if (param_1 < 8)
			{
				/* low 3 bits to hold the parameter */
				bc = (hcl_oob_t)(cmd & 0xF8) | (hcl_oob_t)param_1;
				goto write_short;
			}
			else if (param_1 <= MAX_CODE_PARAM)
			{
				bc = HCL_CODE_PUSH_LITERAL_X; /* cmd | 0x80 */
				goto write_long;
			}
			else
			{
				bc = HCL_CODE_PUSH_LITERAL_X2; /* HCL_CODE_PUSH_LITERAL_4 | 0x80 */
				goto write_long2;
			}

		case HCL_CODE_PUSH_OBJECT_0:
		case HCL_CODE_STORE_INTO_OBJECT_0:
		case HCL_CODE_POP_INTO_OBJECT_0:
		case HCL_CODE_JUMP_FORWARD_0:
		case HCL_CODE_JUMP_BACKWARD_0:
		case HCL_CODE_CALL_0:
		case HCL_CODE_SEND_0:
		case HCL_CODE_SEND_TO_SUPER_0:
			if (param_1 < 4)
			{
				/* low 2 bits to hold the parameter */
				bc = (hcl_oob_t)(cmd & 0xFC) | (hcl_oob_t)param_1;
				goto write_short;
			}
			else
			{
				/* convert the code to a long version */
				bc = cmd | 0x80;
				goto write_long;
			}

		case HCL_CODE_JUMP_FORWARD_IF_TRUE:
		case HCL_CODE_JUMP_FORWARD_IF_FALSE:
		case HCL_CODE_JUMP2_FORWARD_IF_TRUE:
		case HCL_CODE_JUMP2_FORWARD_IF_FALSE:
		case HCL_CODE_JUMP2_FORWARD:
		case HCL_CODE_JUMP_BACKWARD_IF_TRUE:
		case HCL_CODE_JUMP_BACKWARD_IF_FALSE:
		case HCL_CODE_JUMP2_BACKWARD_IF_TRUE:
		case HCL_CODE_JUMP2_BACKWARD_IF_FALSE:
		case HCL_CODE_JUMP2_BACKWARD:

		case HCL_CODE_PUSH_CVAR_I_X:
		case HCL_CODE_STORE_INTO_CVAR_I_X:
		case HCL_CODE_POP_INTO_CVAR_I_X:
		case HCL_CODE_PUSH_CVAR_M_X:
		case HCL_CODE_STORE_INTO_CVAR_M_X:
		case HCL_CODE_POP_INTO_CVAR_M_X:

		case HCL_CODE_CLASS_CMSTORE:
		case HCL_CODE_CLASS_CIMSTORE:
		case HCL_CODE_CLASS_IMSTORE:
		case HCL_CODE_TRY_ENTER:
		case HCL_CODE_TRY_ENTER2:
		case HCL_CODE_PUSH_INTLIT:
		case HCL_CODE_PUSH_NEGINTLIT:
		case HCL_CODE_PUSH_CHARLIT:

		case HCL_CODE_MAKE_DIC: /* TODO: don't these need write_long2? */
		case HCL_CODE_MAKE_ARRAY:
		case HCL_CODE_MAKE_BYTEARRAY:
		case HCL_CODE_POP_INTO_ARRAY:
		case HCL_CODE_POP_INTO_BYTEARRAY:
			bc = cmd;
			goto write_long;
	}

	hcl_seterrbfmt (hcl, HCL_EINVAL, "unhandled single-parameter instruction %u", (unsigned int)cmd);
	return -1;

write_short:
	if (emit_byte_instruction(hcl, bc, srcloc) <= -1) return -1;
	return 0;

write_long:
	if (param_1 > MAX_CODE_PARAM)
	{
		hcl_seterrbfmt (hcl, HCL_ERANGE, "parameter too large to single-parameter instruction %u", (unsigned int)cmd);
		return -1;
	}
#if (HCL_CODE_LONG_PARAM_SIZE == 2)
	if (emit_byte_instruction(hcl, bc, srcloc) <= -1 ||
	    emit_byte_instruction(hcl, (param_1 >> 8) & 0xFF, HCL_NULL) <= -1 ||
	    emit_byte_instruction(hcl, param_1 & 0xFF, HCL_NULL) <= -1) return -1;
#else
	if (emit_byte_instruction(hcl, bc, srcloc) <= -1 ||
	    emit_byte_instruction(hcl, param_1, HCL_NULL) <= -1) return -1;
#endif
	return 0;

write_long2:
	if (param_1 > MAX_CODE_PARAM2)
	{
		hcl_seterrbfmt (hcl, HCL_ERANGE, "parameter too large to single-parameter instruction %u", (unsigned int)cmd);
		return -1;
	}
#if (HCL_CODE_LONG_PARAM_SIZE == 2)
	if (emit_byte_instruction(hcl, bc, srcloc) <= -1 ||
	    emit_byte_instruction(hcl, (param_1 >> 24) & 0xFF, HCL_NULL) <= -1 ||
	    emit_byte_instruction(hcl, (param_1 >> 16) & 0xFF, HCL_NULL) <= -1 ||
	    emit_byte_instruction(hcl, (param_1 >>  8) & 0xFF, HCL_NULL) <= -1 ||
	    emit_byte_instruction(hcl, (param_1 >>  0) & 0xFF, HCL_NULL) <= -1) return -1;
#else
	if (emit_byte_instruction(hcl, bc, srcloc) <= -1 ||
	    emit_byte_instruction(hcl, (param_1 >> 8) & 0xFF, HCL_NULL) <= -1 ||
	    emit_byte_instruction(hcl, (param_1 >> 0) & 0xFF, HCL_NULL) <= -1) return -1;
#endif
	return 0;
}

static int emit_double_param_instruction (hcl_t* hcl, int cmd, hcl_oow_t param_1, hcl_oow_t param_2, const hcl_loc_t* srcloc)
{
	hcl_oob_t bc;

	switch (cmd)
	{
		case HCL_CODE_STORE_INTO_CTXTEMPVAR_0:
		case HCL_CODE_POP_INTO_CTXTEMPVAR_0:
		case HCL_CODE_PUSH_CTXTEMPVAR_0:
		case HCL_CODE_PUSH_OBJVAR_0:
		case HCL_CODE_STORE_INTO_OBJVAR_0:
		case HCL_CODE_POP_INTO_OBJVAR_0:
			if (param_1 < 4 && param_2 < 0xFF)
			{
				/* low 2 bits of the instruction code is the first parameter */
				bc = (hcl_oob_t)(cmd & 0xFC) | (hcl_oob_t)param_1;
				goto write_short;
			}
			else
			{
				/* convert the code to a long version */
				bc = cmd | 0x80;
				goto write_long;
			}

		/* MAKE_FUNCTION is a quad-parameter instruction.
		 * The caller must emit two more parameters after the call to this function.
		 * however the instruction format is the same up to the second
		 * parameters between MAKE_FUNCTION and MAKE_BLOCK.
		 */
		case HCL_CODE_MAKE_BLOCK:
		case HCL_CODE_MAKE_FUNCTION:
		case HCL_CODE_CALL_R:
		case HCL_CODE_SEND_R:
			bc = cmd;
			goto write_long;
	}

	hcl_seterrbfmt (hcl, HCL_EINVAL, "unhandled double-parameter instruction %u", (unsigned int)cmd);
	return -1;

write_short:
	if (emit_byte_instruction(hcl, bc, srcloc) <= -1 ||
	    emit_byte_instruction(hcl, param_2, HCL_NULL) <= -1) return -1;
	return 0;

write_long:
	if (param_1 > MAX_CODE_PARAM || param_2 > MAX_CODE_PARAM)
	{
		hcl_seterrbfmt (hcl, HCL_ERANGE, "parameter too large to double-parameter instruction 0x%u - param_1 0x%zu param_2 0x%zu", (unsigned int)cmd, param_1, param_2);
		return -1;
	}
#if (HCL_CODE_LONG_PARAM_SIZE == 2)
	if (emit_byte_instruction(hcl, bc, srcloc) <= -1 ||
	    emit_byte_instruction(hcl, (param_1 >> 8) & 0xFF, HCL_NULL) <= -1 ||
	    emit_byte_instruction(hcl, param_1 & 0xFF, HCL_NULL) <= -1 ||
	    emit_byte_instruction(hcl, (param_2 >> 8) & 0xFF, HCL_NULL) <= -1 ||
	    emit_byte_instruction(hcl, param_2 & 0xFF, HCL_NULL) <= -1) return -1;
#else
	if (emit_byte_instruction(hcl, bc, srcloc) <= -1 ||
	    emit_byte_instruction(hcl, param_1, HCL_NULL) <= -1 ||
	    emit_byte_instruction(hcl, param_2, HCL_NULL) <= -1) return -1;
#endif
	return 0;
}

static HCL_INLINE int emit_long_param (hcl_t* hcl, hcl_oow_t param)
{
	if (param > MAX_CODE_PARAM)
	{
		hcl_seterrnum (hcl, HCL_ERANGE);
		return -1;
	}

#if (HCL_CODE_LONG_PARAM_SIZE == 2)
	return (emit_byte_instruction(hcl, param >> 8, HCL_NULL) <= -1 ||
	        emit_byte_instruction(hcl, param & 0xFF, HCL_NULL) <= -1)? -1: 0;
#else
	return emit_byte_instruction(hcl, param_1, HCL_NULL);
#endif
}

static int emit_push_literal (hcl_t* hcl, hcl_oop_t obj, const hcl_loc_t* srcloc)
{
	hcl_oow_t index;

	if (HCL_OOP_IS_SMOOI(obj))
	{
		hcl_ooi_t i;

		i = HCL_OOP_TO_SMOOI(obj);
		switch (i)
		{
			case -1:
				return emit_byte_instruction(hcl, HCL_CODE_PUSH_NEGONE, srcloc);

			case 0:
				return emit_byte_instruction(hcl, HCL_CODE_PUSH_ZERO, srcloc);

			case 1:
				return emit_byte_instruction(hcl, HCL_CODE_PUSH_ONE, srcloc);

			case 2:
				return emit_byte_instruction(hcl, HCL_CODE_PUSH_TWO, srcloc);
		}

		if (i >= 0 && i <= MAX_CODE_PARAM)
		{
			return emit_single_param_instruction(hcl, HCL_CODE_PUSH_INTLIT, i, srcloc);
		}
		else if (i < 0 && i >= -(hcl_ooi_t)MAX_CODE_PARAM)
		{
			return emit_single_param_instruction(hcl, HCL_CODE_PUSH_NEGINTLIT, -i, srcloc);
		}
	}
	else if (HCL_OOP_IS_CHAR(obj))
	{
		hcl_ooch_t i;

		i = HCL_OOP_TO_CHAR(obj);

		if (i >= 0 && i <= MAX_CODE_PARAM)
			return emit_single_param_instruction(hcl, HCL_CODE_PUSH_CHARLIT, i, srcloc);
	}

	if (add_literal(hcl, obj, &index) <= -1 ||
	    emit_single_param_instruction(hcl, HCL_CODE_PUSH_LITERAL_0, index, srcloc) <= -1) return -1;

	return 0;
}

static HCL_INLINE void patch_long_jump (hcl_t* hcl, hcl_ooi_t jip, hcl_ooi_t jump_offset)
{
	if (jump_offset > MAX_CODE_JUMP)
	{
		/* switch to JUMP2 instruction to allow a bigger jump offset.
		 * up to twice MAX_CODE_JUMP only */

		HCL_ASSERT (hcl, jump_offset <= MAX_CODE_JUMP * 2);

		HCL_ASSERT (hcl, hcl->code.bc.ptr[jip] == HCL_CODE_JUMP_FORWARD_X ||
		                 hcl->code.bc.ptr[jip] == HCL_CODE_JUMP_FORWARD_IF_TRUE ||
		                 hcl->code.bc.ptr[jip] == HCL_CODE_JUMP_FORWARD_IF_FALSE ||
		                 hcl->code.bc.ptr[jip] == HCL_CODE_JUMP_BACKWARD_X ||
		                 hcl->code.bc.ptr[jip] == HCL_CODE_JUMP_BACKWARD_IF_TRUE ||
		                 hcl->code.bc.ptr[jip] == HCL_CODE_JUMP_BACKWARD_IF_FALSE ||
		                 hcl->code.bc.ptr[jip] == HCL_CODE_TRY_ENTER);

		/* JUMP2 instructions are chosen to be greater than its JUMP counterpart by 1 */
		patch_instruction (hcl, jip, hcl->code.bc.ptr[jip] + 1);
		jump_offset -= MAX_CODE_JUMP;
	}

#if (HCL_CODE_LONG_PARAM_SIZE == 2)
	patch_instruction (hcl, jip + 1, jump_offset >> 8);
	patch_instruction (hcl, jip + 2, jump_offset & 0xFF);
#else
	patch_instruction (hcl, jip + 1, jump_offset);
#endif
}

static HCL_INLINE void patch_long_param (hcl_t* hcl, hcl_ooi_t ip, hcl_oow_t param)
{
#if (HCL_CODE_LONG_PARAM_SIZE == 2)
	patch_instruction (hcl, ip, param >> 8);
	patch_instruction (hcl, ip + 1, param & 0xFF);
#else
	patch_instruction (hcl, ip, param);
#endif
}

static HCL_INLINE void patch_double_long_params (hcl_t* hcl, hcl_ooi_t ip, hcl_oow_t param_1, hcl_oow_t param_2)
{
#if (HCL_CODE_LONG_PARAM_SIZE == 2)
	patch_instruction (hcl, ip, param_1 >> 8);
	patch_instruction (hcl, ip + 1, param_1 & 0xFF);
	patch_instruction (hcl, ip + 2, param_2 >> 8);
	patch_instruction (hcl, ip + 3, param_2 & 0xFF);
#else
	patch_instruction (hcl, ip, param_1);
	patch_instruction (hcl, ip + 1, param_2);
#endif
}

static HCL_INLINE void patch_double_long_params_with_oow (hcl_t* hcl, hcl_ooi_t ip, hcl_oow_t param)
{
#if (HCL_CODE_LONG_PARAM_SIZE == 2)
	patch_instruction (hcl, ip,     (param >> 24) & 0xFF);
	patch_instruction (hcl, ip + 1, (param >> 16) & 0xFF);
	patch_instruction (hcl, ip + 2, (param >> 8) & 0xFF);
	patch_instruction (hcl, ip + 3, (param >> 0) & 0xFF);
#else
	patch_instruction (hcl, ip,     (param >> 8) & 9xFF);
	patch_instruction (hcl, ip + 1, (param >> 0) & 0xFF);
#endif
}

static int emit_variable_access (hcl_t* hcl, int mode, const hcl_var_info_t* vi, const hcl_loc_t* srcloc)
{
	static hcl_oob_t inst_map[][3] =
	{
		{ HCL_CODE_PUSH_CTXTEMPVAR_0, HCL_CODE_POP_INTO_CTXTEMPVAR_0, HCL_CODE_STORE_INTO_CTXTEMPVAR_0 },
		{ HCL_CODE_PUSH_IVAR_0,       HCL_CODE_POP_INTO_IVAR_0,       HCL_CODE_STORE_INTO_IVAR_0    },
		{ HCL_CODE_PUSH_CVAR_I_X,     HCL_CODE_POP_INTO_CVAR_I_X,     HCL_CODE_STORE_INTO_CVAR_I_X   },
		{ HCL_CODE_PUSH_CVAR_M_X,     HCL_CODE_POP_INTO_CVAR_M_X,     HCL_CODE_STORE_INTO_CVAR_M_X   }
	};

	switch (vi->type)
	{
		case VAR_INDEXED:
			return emit_double_param_instruction(hcl, inst_map[0][mode], vi->ctx_offset, vi->index_in_ctx, srcloc);

		case VAR_INST:
			HCL_ASSERT (hcl, vi->ctx_offset == 0);
			return emit_single_param_instruction(hcl, inst_map[1][mode], vi->index_in_ctx, srcloc);

		case VAR_CLASS_I: /* class variable in initialization scope */
			HCL_ASSERT (hcl, vi->ctx_offset == 0);
			return emit_single_param_instruction(hcl, inst_map[2][mode], vi->index_in_ctx, srcloc);

		case VAR_CLASS_CM: /* class variable in class method scope */
		case VAR_CLASS_IM: /* class variable in instance method scope */
			HCL_ASSERT (hcl, vi->ctx_offset == 0);
			return emit_single_param_instruction(hcl, inst_map[3][mode], vi->index_in_ctx, srcloc);
	}

	return -1;
}

/* ========================================================================= */
static int push_cblk (hcl_t* hcl, const hcl_loc_t* errloc, hcl_cblk_type_t type)
{
	hcl_oow_t new_depth;

	HCL_ASSERT (hcl, hcl->c->cblk.depth >= -1);

	if (hcl->c->cblk.depth == HCL_TYPE_MAX(hcl_ooi_t))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_BLKDEPTH, errloc, HCL_NULL, "control block depth too deep");
		return -1;
	}

	new_depth = hcl->c->cblk.depth + 1;
	if (hcl->c->cblk.depth >= hcl->c->cblk.info_capa)
	{
		hcl_cblk_info_t* tmp;
		hcl_oow_t newcapa;

		newcapa = HCL_ALIGN(new_depth + 1, BLK_INFO_BUFFER_ALIGN);
		tmp = (hcl_cblk_info_t*)hcl_reallocmem(hcl, hcl->c->cblk.info, newcapa * HCL_SIZEOF(*tmp));
		if (HCL_UNLIKELY(!tmp))
		{
			const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
			hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "failed to resize control block info buffer - %js", orgmsg);
			return -1;
		}

		hcl->c->cblk.info_capa = newcapa;
		hcl->c->cblk.info = tmp;
	}

	HCL_MEMSET (&hcl->c->cblk.info[new_depth], 0, HCL_SIZEOF(hcl->c->cblk.info[new_depth]));
	hcl->c->cblk.info[new_depth]._type = type;
	hcl->c->cblk.depth = new_depth;
	return 0;
}

static void pop_cblk (hcl_t* hcl)
{
	HCL_ASSERT (hcl, hcl->c->cblk.depth >= 0); /* depth is of a signed type */

	/* a control block stays inside a function block.
	 * the control block stack must not be popped past the starting base
	 * of the owning function block */
	HCL_ASSERT (hcl, hcl->c->cblk.depth - 1 >= hcl->c->fnblk.info[hcl->c->fnblk.depth].cblk_base);
	hcl->c->cblk.depth--;
}

static int push_clsblk (hcl_t* hcl, const hcl_loc_t* errloc, hcl_oow_t nivars, hcl_oow_t ncvars, const hcl_ooch_t* ivars_str, hcl_oow_t ivars_strlen, const hcl_ooch_t* cvars_str, hcl_oow_t cvars_strlen)
{
	hcl_oow_t new_depth;
	hcl_clsblk_info_t* ci;
	hcl_fnblk_info_t* fbi;

	HCL_ASSERT (hcl, hcl->c->clsblk.depth >= -1);

	if (hcl->c->clsblk.depth == HCL_TYPE_MAX(hcl_ooi_t))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_BLKDEPTH, errloc, HCL_NULL, "class block depth too deep");
		return -1;
	}

	new_depth = hcl->c->clsblk.depth + 1;
	if (hcl->c->clsblk.depth >= hcl->c->clsblk.info_capa)
	{
		hcl_clsblk_info_t* tmp;
		hcl_oow_t newcapa;

		newcapa = HCL_ALIGN(new_depth + 1, BLK_INFO_BUFFER_ALIGN);
		tmp = (hcl_clsblk_info_t*)hcl_reallocmem(hcl, hcl->c->clsblk.info, newcapa * HCL_SIZEOF(*tmp));
		if (HCL_UNLIKELY(!tmp))
		{
			const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
			hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "failed to resize class block info buffer - %js", orgmsg);
			return -1;
		}

		hcl->c->clsblk.info_capa = newcapa;
		hcl->c->clsblk.info = tmp;
	}

	ci = &hcl->c->clsblk.info[new_depth];
	HCL_MEMSET (ci, 0, HCL_SIZEOF(*ci));
	ci->nivars = nivars;
	ci->ncvars = ncvars;

	if (nivars > 0)
	{
		HCL_ASSERT (hcl, ivars_str != HCL_NULL);
		ci->ivars_str = hcl_dupoochars(hcl, ivars_str, ivars_strlen);
		if (HCL_UNLIKELY(!ci->ivars_str)) return -1;
	}
	if (ncvars > 0)
	{
		HCL_ASSERT (hcl, cvars_str != HCL_NULL);
		ci->cvars_str = hcl_dupoochars(hcl, cvars_str, cvars_strlen);
		if (HCL_UNLIKELY(!ci->cvars_str))
		{
			if (ci->ivars_str) hcl_freemem (hcl, ci->ivars_str);
			return -1;
		}
	}

	/* remember the function block depth before the class block is entered */
	ci->fnblk_base = hcl->c->fnblk.depth;

	/* attach the class block to the current function block */
	fbi = &hcl->c->fnblk.info[hcl->c->fnblk.depth];
	if (fbi->clsblk_base <= -1) fbi->clsblk_base = new_depth;
	fbi->clsblk_top = new_depth;

	hcl->c->clsblk.depth = new_depth;
	return 0;
}

static void pop_clsblk (hcl_t* hcl)
{
	hcl_fnblk_info_t* fbi;
	hcl_clsblk_info_t* cbi;

	HCL_ASSERT (hcl, hcl->c->clsblk.depth >= 0); /* depth is of a signed type */
	HCL_ASSERT (hcl, hcl->c->fnblk.depth >= 0);

	fbi = &hcl->c->fnblk.info[hcl->c->fnblk.depth];
	HCL_ASSERT (hcl, fbi->clsblk_base >= 0 && fbi->clsblk_top >= 0 && fbi->clsblk_top >= fbi->clsblk_base);
	HCL_ASSERT (hcl, fbi->clsblk_top == hcl->c->clsblk.depth);
	if (fbi->clsblk_top == fbi->clsblk_base)
	{
		/* the first class block inside a function block */
		fbi->clsblk_base = -1;
		fbi->clsblk_top = -1;
	}
	else
	{
		fbi->clsblk_top--;
	}

	cbi = &hcl->c->clsblk.info[hcl->c->clsblk.depth];
	if (cbi->cvars_str)
	{
		hcl_freemem (hcl, cbi->cvars_str);
		cbi->cvars_str = HCL_NULL;
	}
	if (cbi->ivars_str)
	{
		hcl_freemem (hcl, cbi->ivars_str);
		cbi->ivars_str = HCL_NULL;
	}
	hcl->c->clsblk.depth--;
}


static int push_fnblk (hcl_t* hcl, const hcl_loc_t* errloc,
	hcl_oow_t tmpr_va, hcl_oow_t tmpr_nargs, hcl_oow_t tmpr_nrvars, hcl_oow_t tmpr_nlvars,
	hcl_oow_t tmpr_count, hcl_oow_t tmpr_len, hcl_oow_t make_inst_pos, hcl_oow_t lfbase, unsigned int fun_type)
{
	hcl_oow_t new_depth;
	hcl_fnblk_info_t* fbi;

	HCL_ASSERT (hcl, hcl->c->fnblk.depth >= -1);
	if (hcl->c->fnblk.depth == HCL_TYPE_MAX(hcl_ooi_t))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_BLKDEPTH, errloc, HCL_NULL, "function block depth too deep");
		return -1;
	}

	new_depth = hcl->c->fnblk.depth + 1;
	if (hcl->c->fnblk.depth >= hcl->c->fnblk.info_capa)
	{
		hcl_fnblk_info_t* tmp;
		hcl_oow_t newcapa;

		newcapa = HCL_ALIGN(new_depth + 1, BLK_INFO_BUFFER_ALIGN);
		tmp = (hcl_fnblk_info_t*)hcl_reallocmem(hcl, hcl->c->fnblk.info, newcapa * HCL_SIZEOF(*tmp));
		if (HCL_UNLIKELY(!tmp))
		{
			const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
			hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "failed to resize function block info buffer - %js", orgmsg);
			return -1;
		}

		hcl->c->fnblk.info_capa = newcapa;
		hcl->c->fnblk.info = tmp;
	}

	fbi = &hcl->c->fnblk.info[new_depth];
	HCL_MEMSET (fbi, 0, HCL_SIZEOF(*fbi));

	fbi->fun_type = fun_type;

	fbi->tmprlen = tmpr_len;
	fbi->tmprcnt = tmpr_count;
	fbi->tmpr_va = tmpr_va;
	fbi->tmpr_nargs = tmpr_nargs;
	fbi->tmpr_nrvars = tmpr_nrvars;
	fbi->tmpr_nlvars = tmpr_nlvars;

	/* remember the control block depth before the function block is entered */
	fbi->cblk_base = hcl->c->cblk.depth;

	/* no class block when the funtion block is entered */
	fbi->clsblk_base = -1;
	fbi->clsblk_top = -1;

	fbi->make_inst_pos = make_inst_pos;
	fbi->lfbase = lfbase;

	fbi->access_outer = 0;
	fbi->accessed_by_inner = 0;

	hcl->c->fnblk.depth = new_depth;
	return 0;
}

static void clear_fnblk_inners (hcl_t* hcl)
{
	hcl_fnblk_info_t* fbi;
	fbi = &hcl->c->fnblk.info[hcl->c->fnblk.depth];
	while (hcl->c->cblk.depth > fbi->cblk_base) pop_cblk (hcl);
	while (!(fbi->clsblk_base <= -1 && fbi->clsblk_top <= -1)) pop_clsblk (hcl);
}

static void pop_fnblk (hcl_t* hcl)
{
	hcl_fnblk_info_t* fbi;

	HCL_ASSERT (hcl, hcl->c->fnblk.depth >= 0);

	clear_fnblk_inners (hcl);
	fbi = &hcl->c->fnblk.info[hcl->c->fnblk.depth];
	/* if pop_cblk() has been called properly, the following assertion must be true
	 * and the assignment on the next line isn't necessary */
	HCL_ASSERT (hcl, hcl->c->cblk.depth == fbi->cblk_base);
	HCL_ASSERT (hcl, fbi->clsblk_base <= -1 && fbi->clsblk_top <= -1);

	hcl->c->cblk.depth = fbi->cblk_base;
	/* keep hcl->code.lit.len without restoration */

	hcl->c->fnblk.depth--;

	if (hcl->c->fnblk.depth >= 0)
	{
		/* restore the string length and the word count to the values captured
		 * at the previous level */
		hcl->c->tv.s.len = hcl->c->fnblk.info[hcl->c->fnblk.depth].tmprlen;
		hcl->c->tv.wcount = hcl->c->fnblk.info[hcl->c->fnblk.depth].tmprcnt;
	}
	else
	{
		hcl->c->tv.s.len = 0;
		hcl->c->tv.wcount = 0;
	}

	if (fbi->make_inst_pos < hcl->code.bc.len)
	{
		hcl_oow_t attr_mask;

		/* patch the temporaries mask parameter for the MAKE_BLOCK or MAKE_FUNCTION instruction */
		HCL_ASSERT (hcl, hcl->code.bc.ptr[fbi->make_inst_pos] == HCL_CODE_MAKE_BLOCK ||
		                 hcl->code.bc.ptr[fbi->make_inst_pos] == HCL_CODE_MAKE_FUNCTION);

		/* the total number of temporaries in this function block must be the sum of
		 * the number of arguments, return variables and local variables */
		HCL_ASSERT (hcl, fbi->tmprcnt - hcl->c->tv.wcount == fbi->tmpr_nargs + fbi->tmpr_nrvars + fbi->tmpr_nlvars);

		/* the temporaries mask is a bit-mask that encodes the counts of different temporary variables.
		 * and it's split to two intruction parameters when used with MAKE_BLOCK and MAKE_FUNCTION.
		 * the INSTA bit is on if fbi->fun_type == FUN_CIM */
		attr_mask = ENCODE_BLK_MASK(((fbi->fun_type & 0xFF) == FUN_CIM), fbi->tmpr_va, fbi->tmpr_nargs, fbi->tmpr_nrvars, fbi->tmpr_nlvars);
		patch_double_long_params_with_oow (hcl, fbi->make_inst_pos + 1, attr_mask);
	}
}

/* ========================================================================= */
static HCL_INLINE int _insert_cframe (hcl_t* hcl, hcl_ooi_t index, int opcode, hcl_cnode_t* operand)
{
	hcl_cframe_t* tmp;

	HCL_ASSERT (hcl, index >= 0);

	hcl->c->cfs.top++;
	HCL_ASSERT (hcl, hcl->c->cfs.top >= 0);
	HCL_ASSERT (hcl, index <= hcl->c->cfs.top);

	if ((hcl_oow_t)hcl->c->cfs.top >= hcl->c->cfs.capa)
	{
		hcl_oow_t newcapa;

		newcapa = HCL_ALIGN (hcl->c->cfs.top + 256, 256); /* TODO: adjust this capacity */
		tmp = (hcl_cframe_t*)hcl_reallocmem(hcl, hcl->c->cfs.ptr, newcapa * HCL_SIZEOF(*tmp));
		if (HCL_UNLIKELY(!tmp))
		{
			const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
			hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "failed to grow compiler frame stack- %js", orgmsg);
			hcl->c->cfs.top--;
			return -1;
		}

		hcl->c->cfs.capa = newcapa;
		hcl->c->cfs.ptr = tmp;
	}

	if (index < hcl->c->cfs.top)
	{
		HCL_MEMMOVE (&hcl->c->cfs.ptr[index + 1], &hcl->c->cfs.ptr[index], (hcl->c->cfs.top - index) * HCL_SIZEOF(*tmp));
	}

	tmp = &hcl->c->cfs.ptr[index];
	tmp->opcode = opcode;
	tmp->operand = operand;
	HCL_MEMSET (&tmp->u, 0, HCL_SIZEOF(tmp->u));
	return 0;
}

static int insert_cframe (hcl_t* hcl, hcl_ooi_t index, int opcode, hcl_cnode_t* operand)
{
	if (hcl->c->cfs.top == HCL_TYPE_MAX(hcl_ooi_t))
	{
		hcl_seterrnum (hcl, HCL_EFRMFLOOD);
		return -1;
	}

	return _insert_cframe(hcl, index, opcode, operand);
}

static int push_cframe (hcl_t* hcl, int opcode, hcl_cnode_t* operand)
{
	if (hcl->c->cfs.top == HCL_TYPE_MAX(hcl_ooi_t))
	{
		hcl_seterrnum (hcl, HCL_EFRMFLOOD);
		return -1;
	}

	return _insert_cframe(hcl, hcl->c->cfs.top + 1, opcode, operand);
}

static HCL_INLINE void pop_cframe (hcl_t* hcl)
{
	HCL_ASSERT (hcl, hcl->c->cfs.top >= 0);
	hcl->c->cfs.top--;
}

#define PUSH_CFRAME(hcl,opcode,operand) \
	do { if (push_cframe(hcl,opcode,operand) <= -1) return -1; } while(0)

#define INSERT_CFRAME(hcl,index,opcode,operand) \
	do { if (insert_cframe(hcl,index,opcode,operand) <= -1) return -1; } while(0)

#define POP_CFRAME(hcl) pop_cframe(hcl)

#define POP_ALL_CFRAMES(hcl) (hcl->c->cfs.top = -1)

#define GET_TOP_CFRAME_INDEX(hcl) (hcl->c->cfs.top)

#define GET_TOP_CFRAME(hcl) (&hcl->c->cfs.ptr[hcl->c->cfs.top])

#define GET_CFRAME(hcl,index) (&hcl->c->cfs.ptr[index])

#define SWITCH_TOP_CFRAME(hcl,_opcode,_operand) \
	do { \
		hcl_cframe_t* _cf = GET_TOP_CFRAME(hcl); \
		_cf->opcode = _opcode; \
		_cf->operand = _operand; \
	} while(0)

#define SWITCH_CFRAME(hcl,_index,_opcode,_operand) \
	do { \
		hcl_cframe_t* _cf = GET_CFRAME(hcl,_index); \
		_cf->opcode = _opcode; \
		_cf->operand = _operand; \
	} while(0)

static int push_subcframe (hcl_t* hcl, int opcode, hcl_cnode_t* operand)
{
	hcl_cframe_t* cf, tmp;

	cf = GET_TOP_CFRAME(hcl);
	tmp = *cf;
	cf->opcode = opcode;
	cf->operand = operand;

	if (push_cframe(hcl, tmp.opcode, tmp.operand) <= -1) return -1;
	cf = GET_TOP_CFRAME(hcl);
	cf->u = tmp.u; /* copy the extra information */

	return 0;
}

static HCL_INLINE hcl_cframe_t* find_cframe_from_top (hcl_t* hcl, int opcode)
{
	hcl_cframe_t* cf;
	hcl_ooi_t i;

	for (i = hcl->c->cfs.top; i >= 0; i--)
	{
		cf = &hcl->c->cfs.ptr[i];
		if (cf->opcode == opcode) return cf;
	}

	return HCL_NULL;
}

#define PUSH_SUBCFRAME(hcl,opcode,operand) \
	do { if (push_subcframe(hcl,opcode,operand) <= -1) return -1; } while(0)

#define GET_SUBCFRAME(hcl) (&hcl->c->cfs.ptr[hcl->c->cfs.top - 1])

/* ========================================================================= */

struct class_vardcl_t
{
	hcl_oow_t nivars;
	hcl_oow_t ncvars;
	hcl_oow_t ivar_start;
	hcl_oow_t ivar_len;
	hcl_oow_t cvar_start;
	hcl_oow_t cvar_len;
};
typedef struct class_vardcl_t class_vardcl_t;

static int collect_vardcl_for_class (hcl_t* hcl, hcl_cnode_t* obj, hcl_cnode_t** nextobj, class_vardcl_t* vardcl)
{
	hcl_oow_t tv_wcount_saved, tv_slen_saved;
	hcl_cnode_t* dcl, * dcl_saved;
	int enclosed = 0;
	static const hcl_bch_t* desc[] = { "instance", "class" };

	HCL_MEMSET (vardcl, 0, HCL_SIZEOF(*vardcl));
	tv_wcount_saved = hcl->c->tv.wcount;
	tv_slen_saved = hcl->c->tv.s.len;

	dcl = HCL_CNODE_CONS_CAR(obj);
	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS_CONCODED(dcl, HCL_CONCODE_TUPLE));

	do
	{
		hcl_cnode_t* var;
		int n;
		hcl_oow_t checkpoint;

		var = HCL_CNODE_CONS_CAR(dcl);

		if (HCL_CNODE_IS_CONS_CONCODED(var, HCL_CONCODE_TUPLE)) /* [ ... ] */
		{
			if (enclosed)
			{
			synerr_varname:
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAME, HCL_CNODE_GET_LOC(var), HCL_CNODE_GET_TOK(var), "not variable name");
				return -1;
			}
			enclosed = 1;
			dcl_saved = dcl;
			dcl = var;
			continue; /* start over */
		}
		else if (HCL_CNODE_IS_ELIST_CONCODED(var, HCL_CONCODE_TUPLE))
		{
			/* no variables inside [] */
			if (enclosed) goto synerr_varname; /* [] inside [] */
			goto next;
		}

		if (!HCL_CNODE_IS_SYMBOL_PLAIN(var) || HCL_CNODE_IS_SYMBOL_PLAIN_BINOP(var)) goto synerr_varname;

		checkpoint = hcl->c->tv.s.len;
		n = add_temporary_variable(hcl, HCL_CNODE_GET_TOK(var), tv_slen_saved);
		if (n <= -1)
		{
			if (hcl->errnum == HCL_EEXIST)
			{
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAMEDUP, HCL_CNODE_GET_LOC(var), HCL_CNODE_GET_TOK(var), "duplicate %hs variable", desc[enclosed]);
			}
			return -1;
		}

		if (enclosed)
		{
			/* class variable */
			/*if (cvar_len <= 0) cvar_start = prev_tv_len;
			cvar_len = hcl->c->tv.s.len - cvar_start; */
			if (vardcl->cvar_len <= 0) vardcl->cvar_start = checkpoint;
			vardcl->cvar_len += hcl->c->tv.s.len - checkpoint;
			vardcl->ncvars++;
		}
		else
		{
			/* instance variable */
			if (vardcl->ivar_len <= 0) vardcl->ivar_start = (vardcl->cvar_len <= 0)? checkpoint: vardcl->cvar_start;
			vardcl->ivar_len += hcl->c->tv.s.len - checkpoint;
			if (vardcl->cvar_len > 0)
			{
				/* place the instance variables before the class variables
				 * if class variables "a b" has been collected before instance variables "cc dd ee"
				 * the rotation below manipulates the buffer to contain "cc dd ee a b".
				 */
				hcl_rotate_oochars (&hcl->c->tv.s.ptr[vardcl->cvar_start], hcl->c->tv.s.len - vardcl->cvar_start, -1, vardcl->cvar_len);
				vardcl->cvar_start += hcl->c->tv.s.len - checkpoint;
			}
			vardcl->nivars++;
		}

	next:
		dcl = HCL_CNODE_CONS_CDR(dcl);
		if (!dcl)
		{
			if (enclosed)
			{
				enclosed = 0;
				dcl = dcl_saved;
				goto next;
			}
			break;
		}

		if (!HCL_CNODE_IS_CONS(dcl))
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(dcl), HCL_CNODE_GET_TOK(dcl), "redundant cdr in %hs variable declaration", desc[enclosed]);
			return -1;
		}
	}
	while (1);

	HCL_ASSERT (hcl, vardcl->nivars + vardcl->ncvars == hcl->c->tv.wcount - tv_wcount_saved);
	*nextobj = HCL_CNODE_CONS_CDR(obj);

	return 0;
}

static int collect_vardcl (hcl_t* hcl, hcl_cnode_t* obj, hcl_cnode_t** nextobj, hcl_oow_t tv_dup_check_start, hcl_oow_t* nvardcls, const hcl_bch_t* desc)
{
	/* process a single variable declaration list */
	hcl_oow_t ndcls = 0;
	hcl_oow_t old_wcount = hcl->c->tv.wcount;
	hcl_cnode_t* dcl;
	hcl_cnode_t* var;

	dcl = HCL_CNODE_CONS_CAR(obj);
	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS_CONCODED(dcl, HCL_CONCODE_VLIST));

	do
	{
		var = HCL_CNODE_CONS_CAR(dcl);
	#if 0
		if (!HCL_CNODE_IS_SYMBOL(var))
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGNAME, HCL_CNODE_GET_LOC(var), HCL_CNODE_GET_TOK(var), "local variable not symbol");
			return -1;
		}

		if (HCL_CNODE_IS_SYMBOL(var) && HCL_CNODE_SYMBOL_SYNCODE(var) /* || HCL_OBJ_GET_FLAGS_KERNEL(var) >= 2 */)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNEDARGNAME, HCL_CNODE_GET_LOC(var), HCL_CNODE_GET_TOK(var), "special symbol not to be declared as local variable");
			return -1;
		}
	#else
		/* the above checks are not needed as the reader guarantees the followings */
		HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_PLAIN(var));
	#endif

		if (add_temporary_variable(hcl, HCL_CNODE_GET_TOK(var), tv_dup_check_start) <= -1)
		{
			if (hcl->errnum == HCL_EEXIST)
			{
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAMEDUP, HCL_CNODE_GET_LOC(var), HCL_CNODE_GET_TOK(var), "duplicate %hs variable", desc);
			}
			return -1;
		}
		ndcls++;

		dcl = HCL_CNODE_CONS_CDR(dcl);
		if (!dcl) break;

		if (!HCL_CNODE_IS_CONS(dcl))
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(dcl), HCL_CNODE_GET_TOK(dcl), "redundant cdr in %hs variable list", desc);
			return -1;
		}
	}
	while (1);

	HCL_ASSERT (hcl, ndcls == hcl->c->tv.wcount - old_wcount);
	*nextobj = HCL_CNODE_CONS_CDR(obj);
	*nvardcls = ndcls;

	return 0;
}

static int collect_vardcls (hcl_t* hcl, hcl_cnode_t* obj, hcl_cnode_t** nextobj, hcl_oow_t tv_dup_check_start, hcl_oow_t* nvardcls, const hcl_bch_t* desc)
{
	/* process zero or more variable declaration lists in a row */
	hcl_oow_t ndcls = 0;
	hcl_oow_t old_wcount = hcl->c->tv.wcount;

	while (obj && HCL_CNODE_IS_CONS(obj))
	{
		hcl_cnode_t* dcl;
		hcl_oow_t dclcount;

		dcl = HCL_CNODE_CONS_CAR(obj);
		if (!HCL_CNODE_IS_CONS_CONCODED(dcl, HCL_CONCODE_VLIST)) break;

		if (collect_vardcl(hcl, obj, &obj, tv_dup_check_start, &dclcount, desc) <= -1) return -1;
		ndcls += dclcount;
	}

	HCL_ASSERT (hcl, ndcls == hcl->c->tv.wcount - old_wcount);
	*nvardcls = ndcls;
	*nextobj = obj;
	return 0;
}

static int is_followed_by_vlist (hcl_t* hcl, hcl_cnode_t* obj)
{
	if (obj && HCL_CNODE_IS_CONS(obj))
	{
		hcl_cnode_t* dcl;
		dcl = HCL_CNODE_CONS_CAR(obj);
		return HCL_CNODE_IS_CONS_CONCODED(dcl, HCL_CONCODE_VLIST);
	}
	return 0;
}

static int check_if_plain_cnode (hcl_t* hcl, hcl_cnode_t* obj, hcl_cnode_t* prev, hcl_cnode_t* container, hcl_synerrnum_t errnum, const hcl_bch_t* bname)
{
	if (!obj)
	{
		hcl_setsynerrbfmt (hcl, errnum, HCL_CNODE_GET_LOC(prev), HCL_NULL, "no %hs in %.*js", bname, HCL_CNODE_GET_TOKLEN(container), HCL_CNODE_GET_TOKPTR(container));
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(prev), HCL_CNODE_GET_TOK(obj), "redundant cdr where %.*js is expected in %.*js", bname, HCL_CNODE_GET_TOKLEN(container), HCL_CNODE_GET_TOKPTR(container));
		return -1;
	}

	return 0;
}

/* ========================================================================= */

enum
{
	COP_COMPILE_OBJECT,
	COP_COMPILE_OBJECT_R,
	COP_COMPILE_SYMBOL_LITERAL,

	COP_COMPILE_ARGUMENT_LIST,
	COP_COMPILE_OBJECT_LIST,
	COP_COMPILE_OBJECT_LIST_TAIL,
	COP_COMPILE_IF_OBJECT_LIST,
	COP_COMPILE_IF_OBJECT_LIST_TAIL,
	COP_COMPILE_TRY_OBJECT_LIST,
	COP_COMPILE_TRY_OBJECT_LIST_TAIL,

	COP_COMPILE_ARRAY_LIST,
	COP_COMPILE_BYTEARRAY_LIST,
	COP_COMPILE_DIC_LIST,
	COP_COMPILE_QLIST, /* compile data list */

	COP_COMPILE_ELIF,
	COP_COMPILE_ELSE,
	COP_COMPILE_CATCH,

	COP_COMPILE_AND_P1,
	COP_COMPILE_AND_P2,

	COP_COMPILE_BREAK_P1,

	COP_COMPILE_DO_P1,

	COP_COMPILE_OR_P1,
	COP_COMPILE_OR_P2,

	COP_COMPILE_CLASS_P1,
	COP_COMPILE_CLASS_P2,
	COP_COMPILE_CLASS_P3,

	COP_EMIT_PUSH_NIL,
	COP_EMIT_PUSH_SYMBOL,
	COP_EMIT_CALL,
	COP_EMIT_SEND,

	COP_EMIT_MAKE_ARRAY,
	COP_EMIT_MAKE_BYTEARRAY,
	COP_EMIT_MAKE_DIC,
	COP_EMIT_MAKE_CONS,
	COP_EMIT_POP_INTO_ARRAY,
	COP_EMIT_POP_INTO_BYTEARRAY,
	COP_EMIT_POP_INTO_DIC,
	COP_EMIT_POP_INTO_CONS,
	COP_EMIT_POP_INTO_CONS_END,
	COP_EMIT_POP_INTO_CONS_CDR,

	COP_EMIT_FUN,
	COP_EMIT_PLUS,
	COP_EMIT_POP_STACKTOP,
	COP_EMIT_RETURN,
	COP_EMIT_SET,
	COP_EMIT_CLASS_CMSTORE,
	COP_EMIT_CLASS_CIMSTORE,
	COP_EMIT_CLASS_IMSTORE,
	COP_EMIT_THROW,

	COP_POST_IF_COND,
	COP_POST_IF_BODY,

	COP_POST_UNTIL_BODY,
	COP_POST_UNTIL_COND,
	COP_POST_WHILE_BODY,
	COP_POST_WHILE_COND,

	COP_POST_TRY,
	COP_POST_CATCH,

	COP_POST_FUN
};

/* ========================================================================= */

static int compile_and (hcl_t* hcl, hcl_cnode_t* src)
{
	hcl_cnode_t* obj, * expr;

	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));
	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_AND));

	obj = HCL_CNODE_CONS_CDR(src);

	if (!obj)
	{
		/* no value */
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(src), HCL_NULL, "no expression specified in and");
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in and");
		return -1;
	}

/* TODO: optimization - eat away all true expressions */
	expr = HCL_CNODE_CONS_CAR(obj);
	obj = HCL_CNODE_CONS_CDR(obj);

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, expr); /* 1 */
	if (obj) PUSH_SUBCFRAME (hcl, COP_COMPILE_AND_P1, obj); /* 2 */

	return 0;
}


static HCL_INLINE int compile_and_p1 (hcl_t* hcl)
{
	hcl_cnode_t* obj, * expr;
	hcl_cframe_t* cf;
	hcl_ooi_t jump_inst_pos;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_AND_P1);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

/* TODO: optimization - eat away all true expressions */
	obj = cf->operand;
	if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in and");
		return -1;
	}

	HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX);
	jump_inst_pos = hcl->code.bc.len;

	/* this conditional jump make evaluation short-circuited. the actual jump point is to be patched in compile_and_p2() */
	if (emit_single_param_instruction(hcl, HCL_CODE_JUMP_FORWARD_IF_FALSE, MAX_CODE_JUMP, HCL_CNODE_GET_LOC(obj)) <= -1) return -1;
	if (emit_byte_instruction(hcl, HCL_CODE_POP_STACKTOP, HCL_CNODE_GET_LOC(obj)) <= -1) return -1;

	expr = HCL_CNODE_CONS_CAR(obj);
	obj = HCL_CNODE_CONS_CDR(obj);

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, expr); /* 1 - compile the current part */

	PUSH_SUBCFRAME (hcl, COP_COMPILE_AND_P2, expr); /* 3 - patch the conditional jump instruction */
	cf = GET_SUBCFRAME(hcl);
	cf->u.post_and.jump_inst_pos = jump_inst_pos;

	if (obj) PUSH_SUBCFRAME (hcl, COP_COMPILE_AND_P1, obj); /* 2 - recurse to compile remaining parts */
	return 0;
}

static HCL_INLINE int compile_and_p2 (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_ooi_t jip;
	hcl_oow_t jump_offset;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_AND_P2);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX);
	jip = cf->u.post_and.jump_inst_pos;

	/* patch the jump insruction emitted after each expression inside the 'and' expression
	 * the jump make evaluation short-circuited. */
	jump_offset = hcl->code.bc.len - jip - (HCL_CODE_LONG_PARAM_SIZE + 1);
	patch_long_jump (hcl, jip, jump_offset);

	POP_CFRAME(hcl);
	return 0;
}

/* ========================================================================= */

static int compile_or (hcl_t* hcl, hcl_cnode_t* src)
{
	hcl_cnode_t* obj, * expr;

	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));
	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_OR));

	obj = HCL_CNODE_CONS_CDR(src);

	if (!obj)
	{
		/* no value */
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(src), HCL_NULL, "no expression specified in or");
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in and");
		return -1;
	}

/* TODO: optimization - eat away all false expressions */
	expr = HCL_CNODE_CONS_CAR(obj);
	obj = HCL_CNODE_CONS_CDR(obj);

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, expr); /* 1 */
	PUSH_SUBCFRAME (hcl, COP_COMPILE_OR_P1, obj); /* 2 */

	return 0;
}


static HCL_INLINE int compile_or_p1 (hcl_t* hcl)
{
	hcl_cnode_t* obj, * expr;
	hcl_cframe_t* cf;
	hcl_ooi_t jump_inst_pos;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_OR_P1);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

/* TODO: optimization - eat away all false expressions */

	obj = cf->operand;
	if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in or");
		return -1;
	}

	HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX);
	jump_inst_pos = hcl->code.bc.len;

	/* this conditional jump makes evaluation short-circuited. the actual jump point is to be patched in compile_or_p2() */
	if (emit_single_param_instruction(hcl, HCL_CODE_JUMP_FORWARD_IF_TRUE, MAX_CODE_JUMP, HCL_CNODE_GET_LOC(obj)) <= -1) return -1;
	if (emit_byte_instruction(hcl, HCL_CODE_POP_STACKTOP, HCL_CNODE_GET_LOC(obj)) <= -1) return -1;

	expr = HCL_CNODE_CONS_CAR(obj);
	obj = HCL_CNODE_CONS_CDR(obj);

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, expr); /* 1 */

	PUSH_SUBCFRAME (hcl, COP_COMPILE_OR_P2, expr); /* 3 */
	cf = GET_SUBCFRAME(hcl);
	cf->u.post_or.jump_inst_pos = jump_inst_pos;

	if (obj) PUSH_SUBCFRAME (hcl, COP_COMPILE_OR_P1, obj); /* 2 */

	return 0;
}

static HCL_INLINE int compile_or_p2 (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_ooi_t jip;
	hcl_oow_t jump_offset;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_OR_P2);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX);
	jip = cf->u.post_or.jump_inst_pos;

	/* patch the jump insruction emitted after each expression inside the 'and' expression */
	jump_offset = hcl->code.bc.len - jip - (HCL_CODE_LONG_PARAM_SIZE + 1);
	patch_long_jump (hcl, jip, jump_offset);

	POP_CFRAME(hcl);
	return 0;
}

/* ========================================================================= */

/* EXPERIMENT WITH BINOP */
static int compile_plus (hcl_t* hcl, hcl_cnode_t* src)
{
	hcl_cnode_t* obj, * expr;

	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));
	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_PLUS));

	obj = HCL_CNODE_CONS_CDR(src);
	if (!obj)
	{
		/* no value */
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(src), HCL_NULL, "no expression specified in plus");
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in plus");
		return -1;
	}

	expr = HCL_CNODE_CONS_CAR(obj);
	obj = HCL_CNODE_CONS_CDR(obj);

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, expr); /* 1 */

	if (!obj)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(src), HCL_NULL, "no second expression specified in plus");
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in plus");
		return -1;
	}

	expr = HCL_CNODE_CONS_CAR(obj);
	obj = HCL_CNODE_CONS_CDR(obj);

	if (obj)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in plus");
		return -1;
	}

/* TODO: more check on obj */
	PUSH_SUBCFRAME (hcl, COP_EMIT_PLUS, src); /* 3 */
	PUSH_SUBCFRAME (hcl, COP_COMPILE_OBJECT, expr); /* 2 */
	return 0;
}

static HCL_INLINE int emit_plus (hcl_t* hcl)
{
	hcl_cframe_t* cf;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_PLUS);

	if (emit_byte_instruction(hcl, HCL_CODE_PLUS, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	POP_CFRAME (hcl);
	return 0;
}

/* ========================================================================= */

static int compile_break (hcl_t* hcl, hcl_cnode_t* src)
{
	/* (break) */
	hcl_cnode_t* cmd, * obj;
	hcl_ooi_t i;

	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));
	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_BREAK) || HCL_CNODE_IS_TYPED(HCL_CNODE_CONS_CAR(src), HCL_CNODE_BREAK));

	cmd = HCL_CNODE_CONS_CAR(src);
	obj = HCL_CNODE_CONS_CDR(src);
	if (obj)
	{
		if (HCL_CNODE_IS_CONS(obj))
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(obj), HCL_NULL, "redundant argument in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		}
		else
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		}
		return -1;
	}

	for (i = hcl->c->cblk.depth; i > hcl->c->fnblk.info[hcl->c->fnblk.depth].cblk_base; --i)
	{
		switch (hcl->c->cblk.info[i]._type)
		{
			case HCL_CBLK_TYPE_LOOP:
				goto inside_loop;

			case HCL_CBLK_TYPE_TRY:
				/* emit an instruction to exit from the try loop. */
				if (emit_byte_instruction(hcl, HCL_CODE_TRY_EXIT, HCL_CNODE_GET_LOC(src)) <= -1) return -1;
				break;

			case HCL_CBLK_TYPE_CLASS:
				/* emit an instruction to exit from the class definition scope being defined */
				if (emit_byte_instruction(hcl, HCL_CODE_CLASS_EXIT, HCL_CNODE_GET_LOC(src)) <= -1) return -1;
				break;
		}
	}

	hcl_setsynerrbfmt (
		hcl, HCL_SYNERR_BREAK, HCL_CNODE_GET_LOC(src), HCL_NULL,
		"%.*js outside loop", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
	return -1;

inside_loop:
	for (i = hcl->c->cfs.top; i >= 0; --i)
	{
		const hcl_cframe_t* tcf;
		tcf = &hcl->c->cfs.ptr[i];

		if (tcf->opcode == COP_EMIT_FUN) break; /* seems to cross function boundary */

		if (tcf->opcode == COP_POST_UNTIL_BODY || tcf->opcode == COP_POST_WHILE_BODY)
		{
			hcl_ooi_t jump_inst_pos;
			hcl_cframe_t* cf;

			/* (break) is not really a function call. but to make it look like a
			 * function call, i generate PUSH_NIL so nil becomes a return value.
			 *     (set x (until #f (break)))
			 * x will get nill. */
			if (emit_byte_instruction(hcl, HCL_CODE_PUSH_NIL, HCL_CNODE_GET_LOC(cmd)) <= -1) return -1;

/* TODO: study if supporting expression after break is good like return. (break (+ 10 20)) */
			HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX);
			jump_inst_pos = hcl->code.bc.len;

			if (emit_single_param_instruction(hcl, HCL_CODE_JUMP_FORWARD_0, MAX_CODE_JUMP, HCL_CNODE_GET_LOC(cmd)) <= -1) return -1;
			INSERT_CFRAME (hcl, i, COP_COMPILE_BREAK_P1, cmd);
			cf = GET_CFRAME(hcl, i);
			cf->u._break.jump_inst_pos = jump_inst_pos;

			POP_CFRAME (hcl);
			return 0;
		}
	}

	/* this part must no be reached. if a loop control block is found,
	 * there must exist a COP_POST_UNTIL_BODY or COP_POST_WHILE_BODY frame */
	hcl_setsynerrbfmt (
		hcl, HCL_SYNERR_INTERN, HCL_CNODE_GET_LOC(src), HCL_NULL,
		"internal error in compiling %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
	return -1;
}

static int compile_break_p1 (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_ooi_t jip, jump_offset;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_BREAK_P1);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	jip = cf->u._break.jump_inst_pos;;

	/* HCL_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD instruction */
	jump_offset = hcl->code.bc.len - jip - (HCL_CODE_LONG_PARAM_SIZE + 1);

	/* no explicit about jump_offset. because break can only place inside
	 * a loop, the same check in post_while_body() must assert
	 * this break jump_offset to be small enough */
	HCL_ASSERT (hcl, jump_offset <= MAX_CODE_JUMP * 2);
	patch_long_jump (hcl, jip, jump_offset);

	POP_CFRAME (hcl);
	return 0;
}

/* ========================================================================= */

static int compile_continue (hcl_t* hcl, hcl_cnode_t* src)
{
	/* (continue) */
	hcl_cnode_t* cmd, * obj;
	hcl_ooi_t i;

	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));
	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_CONTINUE) || HCL_CNODE_IS_TYPED(HCL_CNODE_CONS_CAR(src), HCL_CNODE_CONTINUE));

	cmd = HCL_CNODE_CONS_CAR(src);
	obj = HCL_CNODE_CONS_CDR(src);
	if (obj)
	{
		if (HCL_CNODE_IS_CONS(obj))
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(obj), HCL_NULL, "redundant argument in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		}
		else
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		}
		return -1;
	}

	for (i = hcl->c->cblk.depth; i > hcl->c->fnblk.info[hcl->c->fnblk.depth].cblk_base; --i)
	{
		switch (hcl->c->cblk.info[i]._type)
		{
			case HCL_CBLK_TYPE_LOOP:
				goto inside_loop;

			case HCL_CBLK_TYPE_TRY:
				/*must emit an instruction to exit from the try loop.*/
				if (emit_byte_instruction(hcl, HCL_CODE_TRY_EXIT, HCL_CNODE_GET_LOC(src)) <= -1) return -1;
				break;

			case HCL_CBLK_TYPE_CLASS:
				if (emit_byte_instruction(hcl, HCL_CODE_CLASS_EXIT, HCL_CNODE_GET_LOC(src)) <= -1) return -1;
				break;
		}
	}

	hcl_setsynerrbfmt (hcl, HCL_SYNERR_BREAK, HCL_CNODE_GET_LOC(src), HCL_NULL, "%.*js outside loop", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
	return -1;

inside_loop:
	for (i = hcl->c->cfs.top; i >= 0; --i)
	{
		const hcl_cframe_t* tcf;
		tcf = &hcl->c->cfs.ptr[i];

		if (tcf->opcode == COP_EMIT_FUN) break; /* seems to cross function boundary */

		if (tcf->opcode == COP_POST_UNTIL_BODY || tcf->opcode == COP_POST_WHILE_BODY)
		{
			hcl_ooi_t jump_offset;

			HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX);
			jump_offset = hcl->code.bc.len - tcf->u.post_while.cond_pos + 1;
			if (jump_offset > 3) jump_offset += HCL_CODE_LONG_PARAM_SIZE;
			if (emit_single_param_instruction(hcl, HCL_CODE_JUMP_BACKWARD_0, jump_offset, HCL_CNODE_GET_LOC(cmd)) <= -1) return -1;

			POP_CFRAME (hcl);
			return 0;
		}
	}

	/* this part must no be reached. if a loop control block is found,
	 * there must exist a COP_POST_UNTIL_BODY or COP_POST_WHILE_BODY frame */
	hcl_setsynerrbfmt (hcl, HCL_SYNERR_INTERN, HCL_CNODE_GET_LOC(src), HCL_NULL, "internal error in compiling %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
	return -1;
}

/* ========================================================================= */

#define CEB_IS_BLOCK (1 << 0)
#define CEB_AUTO_FORGED (1 << 1)

static int compile_expression_block (hcl_t* hcl, hcl_cnode_t* src, const hcl_bch_t* ctxname, int flags)
{
	hcl_cnode_t* cmd, * obj;
	hcl_oow_t nlvars, tvslen;
	hcl_fnblk_info_t* fbi;
	hcl_cframe_t* cf;

	if (flags & CEB_IS_BLOCK)
	{
		HCL_ASSERT (hcl, HCL_CNODE_IS_CONS_CONCODED(src, HCL_CONCODE_BLOCK) || HCL_CNODE_IS_ELIST_CONCODED(src, HCL_CONCODE_BLOCK));
		cmd = src; /* it's the cons cell itself  */
		/* `obj` must point to the cons cell pointing to the braced expression list */
		obj = HCL_CNODE_IS_ELIST(src)? HCL_NULL: src;
		/* no check for redundant cdr because {} cannot be dotted */
	}
	else
	{
		cmd = HCL_CNODE_CONS_CAR(src); /* `do` itself */
		/* `obj` must point to the expression list after `do` */
		obj = HCL_CNODE_CONS_CDR(src); /* expression list after it */
		if (obj && !HCL_CNODE_IS_CONS(obj))
		{
			hcl_setsynerrbfmt (
				hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj),
				"redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
			return -1;
		}
	}

	if (is_followed_by_vlist(hcl, obj))
	{
		if (is_in_class_init_scope(hcl))
		{
			hcl_setsynerrbfmt (
				hcl, HCL_SYNERR_VARDCLBANNED, HCL_CNODE_GET_LOC(obj), HCL_NULL,
				"variable declaration disallowed in class init scope");
			return -1;
		}

#if defined(LANG_LIMIT_DO) /* this limitation doesn't seem really useful? or make it #pragma based? */
		if (!(flags & CEB_IS_BLOCK) && (flags & CEB_AUTO_FORGED))
		{
			/* `do` not explicitly enclosed in ().
			 * e.g. do | x | { set x 20; };
			 *         ^
			 *         +-- this is not allowed
			 */
			hcl_setsynerrbfmt (
				hcl, HCL_SYNERR_VARDCLBANNED, HCL_CNODE_GET_LOC(obj), HCL_NULL,
				"variable declaration disallowed in %hs context", ctxname);
			return -1;
		}
#endif
	}

	tvslen = hcl->c->tv.s.len;
	nlvars = 0;
	if (obj)
	{
		hcl_cnode_t* tmp = obj;
		if (collect_vardcls(hcl, obj, &obj, tvslen, &nlvars, ctxname) <= -1) return -1;
		if (nlvars > MAX_CODE_NBLKLVARS)
		{
			hcl_setsynerrbfmt (
				hcl, HCL_SYNERR_VARFLOOD, HCL_CNODE_GET_LOC(tmp), HCL_NULL,
				"too many(%zu) variables in %.*js", nlvars, HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
			return -1;
		}
	}

#if defined(LANG_LIMIT_DO)
	if (!(flags & CEB_IS_BLOCK) && (flags & CEB_AUTO_FORGED))
	{
		if (obj && HCL_CNODE_IS_CONS(obj) && hcl_countcnodecons(hcl, obj) != 1)
		{
			hcl_setsynerrbfmt (
				hcl, HCL_SYNERR_VARDCLBANNED, HCL_CNODE_GET_LOC(obj), HCL_NULL,
				"more than one expression after %hs", ctxname);
			return -1;
		}
	}
#endif

	fbi = &hcl->c->fnblk.info[hcl->c->fnblk.depth];
	fbi->tmprlen = hcl->c->tv.s.len;
	fbi->tmprcnt = hcl->c->tv.wcount;
	fbi->tmpr_nlvars = fbi->tmpr_nlvars + nlvars;

	/* for an expression like `(do   )` or `(do | a b |  ) , `obj` will point to HCL_NULL.
	 * let `obj` point to the internal cnode to convert the expression like `(do #nil)` or `(do |a b| #nil)`. */
	if (!obj) obj = &hcl->c->fake_cnode.cons_to_nil;

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT_LIST, obj);  /* 1 */

	PUSH_SUBCFRAME (hcl, COP_COMPILE_DO_P1, src); /* 2 */
	cf = GET_SUBCFRAME(hcl);
	cf->u.post_do.lvar_start = tvslen;
	cf->u.post_do.lvar_end = fbi->tmprlen;

	return 0;
}

static int compile_do (hcl_t* hcl, hcl_cnode_t* xlist)
{
#if 0
	hcl_cnode_t* cmd, * obj;
#endif
	int flags = 0;

	/* (do
	 *   (+ 10 20)
	 *   (* 2 30)
	 *  ...
	 * )
	 * you can use this to combine multiple expressions to a single expression
	 */

	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(xlist));
	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(xlist), HCL_SYNCODE_DO) || HCL_CNODE_IS_TYPED(HCL_CNODE_CONS_CAR(xlist), HCL_CNODE_DO));

#if 0
	cmd = HCL_CNODE_CONS_CAR(xlist); /* do itself */
	obj = HCL_CNODE_CONS_CDR(xlist); /* expression list after it */
#endif

	if (HCL_CNODE_GET_FLAGS(xlist) & HCL_CNODE_AUTO_FORGED) flags |= CEB_AUTO_FORGED;
	return compile_expression_block(hcl, xlist, "do", flags);
}

static int compile_do_p1 (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	cf = GET_TOP_CFRAME(hcl);
	kill_temporary_variables (hcl, cf->u.post_do.lvar_start, cf->u.post_do.lvar_end);
	POP_CFRAME (hcl);
	return 0;
}

/* ========================================================================= */

static int compile_if (hcl_t* hcl, hcl_cnode_t* src)
{
	hcl_cnode_t* cmd, * obj, * cond;
	hcl_cframe_t* cf;

	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));
	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_IF) || HCL_CNODE_IS_TYPED(HCL_CNODE_CONS_CAR(src), HCL_CNODE_IF));

	/* (if (< 20 30)
	 *   (perform this)
	 *   (perform that)
	 * elif (< 20 30)
	 *   (perform it)
	 * else
	 *   (perform this finally)
	 * )
	 */
	cmd = HCL_CNODE_CONS_CAR(src); /* if itself */
	obj = HCL_CNODE_CONS_CDR(src);

	if (!obj)
	{
		/* no value */
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(src), HCL_NULL, "no conditional expression after '%.*js'", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	cond = HCL_CNODE_CONS_CAR(obj);
	obj = HCL_CNODE_CONS_CDR(obj);

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, cond); /* 1 */
	PUSH_SUBCFRAME (hcl, COP_POST_IF_COND, obj); /* 2 */
	cf = GET_SUBCFRAME(hcl);
	cf->u.post_if.body_pos = -1; /* unknown yet */
	cf->u.post_if.jump_inst_pos = -1; /* not needed */
	cf->u.post_if.start_loc = *HCL_CNODE_GET_LOC(src);
	cf->u.post_if.cmd_cnode = cmd;
/* TODO: OPTIMIZATION:
 *       pass information on the conditional if it's an absoluate true or absolute false to
 *       eliminate some code .. i can't eliminate code because there can be else or elif...
 *       if absoluate true, don't need else or other elif part
 *       if absoluate false, else or other elif part is needed.
 */
	return 0;
}

static HCL_INLINE int patch_nearest_post_if_body (hcl_t* hcl, hcl_cnode_t* cmd)
{
	hcl_ooi_t jump_inst_pos, body_pos;
	hcl_ooi_t jip, jump_offset;
	hcl_cframe_t* cf;

	cf = find_cframe_from_top(hcl, COP_POST_IF_BODY);
	HCL_ASSERT (hcl, cf != HCL_NULL);
	HCL_ASSERT (hcl, cf->opcode == COP_POST_IF_BODY);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	/* jump instruction position of the JUMP_FORWARD_IF_FALSE after the conditional of the previous if or elif*/
	jip = cf->u.post_if.jump_inst_pos;

	if (hcl->code.bc.len <= cf->u.post_if.body_pos)
	{
		/* the if body is empty. */
		if (emit_byte_instruction(hcl, HCL_CODE_PUSH_NIL, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;
	}

	HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX);
	jump_inst_pos = hcl->code.bc.len;

	/* emit jump_forward before the beginning of the else block.
	 * this is to make the earlier if or elif block to skip
	 * the else part. it is to be patched in post_else_body(). */
	if (emit_single_param_instruction (hcl, HCL_CODE_JUMP_FORWARD_0, MAX_CODE_JUMP, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	/* HCL_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD instruction */
	jump_offset = hcl->code.bc.len - jip - (HCL_CODE_LONG_PARAM_SIZE + 1);

	if (jump_offset > MAX_CODE_JUMP * 2)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_IFFLOOD, HCL_CNODE_GET_LOC(cmd), HCL_NULL, "code in %.*js too big - size %zu", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd), jump_offset);
		return -1;
	}
	patch_long_jump (hcl, jip, jump_offset);

	/* beginning of the elif/else block code */
	/* to drop the result of the conditional when the conditional is false */
	if (emit_byte_instruction (hcl, HCL_CODE_POP_STACKTOP, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	/* this is the actual beginning */
	HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX);
	body_pos = hcl->code.bc.len;

	/* modify the POST_IF_BODY frame */
	HCL_ASSERT (hcl, cf->opcode == COP_POST_IF_BODY);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);
	cf->u.post_if.body_pos = body_pos;
	cf->u.post_if.jump_inst_pos = jump_inst_pos;

	return 0;
}

static HCL_INLINE int compile_elif (hcl_t* hcl)
{
	hcl_cnode_t* cmd, * obj, * cond, * src;
	hcl_cframe_t* cf;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_ELIF);

	src = cf->operand;
	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));
	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_ELIF) || HCL_CNODE_IS_TYPED(HCL_CNODE_CONS_CAR(src), HCL_CNODE_ELIF));

	cmd = HCL_CNODE_CONS_CAR(src); /* elif itself */
	obj = HCL_CNODE_CONS_CDR(src);

	if (!obj)
	{
		/* no value */
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(src), HCL_NULL, "no conditional expression after '%.*js'", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	cond = HCL_CNODE_CONS_CAR(obj);
	obj = HCL_CNODE_CONS_CDR(obj);

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, cond); /* 1 */
	PUSH_SUBCFRAME (hcl, COP_POST_IF_COND, obj); /* 2 */
	cf = GET_SUBCFRAME(hcl);
	cf->u.post_if.body_pos = -1; /* unknown yet */
	cf->u.post_if.jump_inst_pos = -1; /* not needed */
	cf->u.post_if.start_loc = *HCL_CNODE_GET_LOC(src);
	cf->u.post_if.cmd_cnode = cmd;

	return patch_nearest_post_if_body(hcl, cmd);
}

static HCL_INLINE int compile_else (hcl_t* hcl)
{
	hcl_cnode_t* cmd, * obj, * src;
	hcl_cframe_t* cf;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_ELSE);

	src = cf->operand;
	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));
	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_ELSE) || HCL_CNODE_IS_TYPED(HCL_CNODE_CONS_CAR(src), HCL_CNODE_ELSE));

	cmd = HCL_CNODE_CONS_CAR(src); /* else itself */
	obj = HCL_CNODE_CONS_CDR(src);

	if (obj && !HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	if (check_block_expression_as_body(hcl, obj, cmd, FOR_NONE) <= -1) return -1;

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT_LIST, obj);

	return patch_nearest_post_if_body(hcl, cmd);
}

/* ========================================================================= */

/*
	(class A
		| x y | ; instance variables
		:: | x y z | ; class variables <--- how to initialize the class variables???

		; everything inside defclass after the variable declarations are normal expressions.
		; however, the resolution of some variables will fall under the enclosing class.
		(set x 20)
		(printf "normal statement ....\n");

		(defun new (a b c)
(printf "%O\n" self) ; self is A
			(set obj super.new)
			(obj.init a b c)
			;(obj.x 10)
			;(obj.y 20)
			(return obj)
		)
	)

	(class B :: A ; A is a parent class
		| p q |
		....
	)

*/

static int compile_class (hcl_t* hcl, hcl_cnode_t* src, int defclass)
{
	hcl_cframe_t* cf;
	hcl_cnode_t* cmd, * obj, * tmp;
	hcl_cnode_t* class_name, * superclass;
	int nsuperclasses;

	cmd = HCL_CNODE_CONS_CAR(src);
	obj = HCL_CNODE_CONS_CDR(src);

	if (defclass)
	{
		/* defclass must be followed by an explicit class name */
		HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(cmd, HCL_SYNCODE_DEFCLASS));

	class_as_defclass:
		if (!obj)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_NAME, HCL_CNODE_GET_LOC(src), HCL_NULL, "no class name in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
			return -1;
		}
		else if (!HCL_CNODE_IS_CONS(obj))
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
			return -1;
		}

		class_name = HCL_CNODE_CONS_CAR(obj);
		if (!HCL_CNODE_IS_SYMBOL(class_name))
		{
			hcl_setsynerrbfmt (
				hcl, HCL_SYNERR_VARNAME, HCL_CNODE_GET_LOC(class_name), HCL_CNODE_GET_TOK(class_name),
				"class name not symbol in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
			return -1;
		}

		if (HCL_CNODE_SYMBOL_SYNCODE(class_name)) /*|| HCL_OBJ_GET_FLAGS_KERNEL(class_name) >= 1) */
		{
			hcl_setsynerrbfmt (
				hcl, HCL_SYNERR_BANNEDVARNAME, HCL_CNODE_GET_LOC(class_name), HCL_CNODE_GET_TOK(class_name),
				"special symbol not to be used as class name");
			return -1;
		}

		obj = HCL_CNODE_CONS_CDR(obj);
	}
	else
	{
		HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(cmd, HCL_SYNCODE_CLASS) || HCL_CNODE_IS_TYPED(cmd, HCL_CNODE_CLASS));

		if (obj && HCL_CNODE_IS_CONS(obj))
		{
			class_name = HCL_CNODE_CONS_CAR(obj);
			if (HCL_CNODE_IS_SYMBOL_PLAIN(class_name))
			{
				/* to handle 'class' in place of 'defclass' */
				defclass = 1;
				goto class_as_defclass;
			}
		}

		class_name = HCL_NULL;
	}

	if (!obj)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_BLOCK, HCL_CNODE_GET_LOC(src), HCL_NULL, "no class body", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	tmp = HCL_CNODE_CONS_CAR(obj);
	if (/*HCL_CNODE_IS_COLON(tmp) || */HCL_CNODE_IS_DBLCOLONS(tmp))
	{
		hcl_cnode_t* marker;

		marker = tmp;
		obj = HCL_CNODE_CONS_CDR(obj);
		if (!obj || !HCL_CNODE_IS_CONS(obj))
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_EOX, HCL_CNODE_GET_LOC(marker), HCL_NULL,
				"no expression or declaration after %.*js", HCL_CNODE_GET_TOKLEN(marker), HCL_CNODE_GET_TOKPTR(marker));
			return -1;
		}

		/* superclass part */
		superclass = HCL_CNODE_CONS_CAR(obj);
		if (!HCL_CNODE_IS_SYMBOL_PLAIN(superclass))
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_NAME, HCL_CNODE_GET_LOC(marker), HCL_NULL,
				"no valid superclass name found after %.*js", HCL_CNODE_GET_TOKLEN(marker), HCL_CNODE_GET_TOKPTR(marker));
			return -1;
		}

		nsuperclasses = 1;
		obj = HCL_CNODE_CONS_CDR(obj);
	}
	else
	{
		nsuperclasses = 0;
		superclass = HCL_NULL;
	}


	if (class_name)
	{
		//SWITCH_TOP_CFRAME (hcl, COP_COMPILE_SYMBOL_LITERAL, class_name); /* 1 - push the class name for a named class */
		hcl_oow_t index;
		hcl_oop_t cons, sym;

		sym = hcl_makesymbol(hcl, HCL_CNODE_GET_TOKPTR(class_name), HCL_CNODE_GET_TOKLEN(class_name));
		if (HCL_UNLIKELY(!sym)) return -1;

		cons = (hcl_oop_t)hcl_getatsysdic(hcl, sym);
		if (!cons)
		{
			cons = (hcl_oop_t)hcl_putatsysdic(hcl, sym, hcl->_nil);
			if (HCL_UNLIKELY(!cons)) return -1;
		}

		if (add_literal(hcl, cons, &index) <= -1) return -1;
		if (emit_single_param_instruction(hcl, HCL_CODE_PUSH_LITERAL_0, index, HCL_CNODE_GET_LOC(class_name)) <= -1) return -1;
	}
	else
	{
		if (emit_byte_instruction(hcl, HCL_CODE_PUSH_NIL,HCL_CNODE_GET_LOC(cmd)) <= -1) return -1; /* push nil for class name of an anonymous class */
	}
	POP_CFRAME (hcl);

	PUSH_CFRAME (hcl, COP_COMPILE_CLASS_P2, class_name); /* 3 - use class name for assignment */
	cf = GET_TOP_CFRAME(hcl);
	cf->u._class.nsuperclasses = 0; /* unsed for CLASS_P2 */
	cf->u._class.start_loc = *HCL_CNODE_GET_LOC(src); /* TODO: use *HCL_CNODE_GET_LOC(cmd) instead? */
	cf->u._class.cmd_cnode = cmd;

	PUSH_CFRAME (hcl, COP_COMPILE_CLASS_P1, obj); /* 2 - variables declaraions and actual body */
	cf = GET_TOP_CFRAME(hcl);
	cf->u._class.nsuperclasses = nsuperclasses; /* this needs to change if we support multiple superclasses... */
	cf->u._class.start_loc = *HCL_CNODE_GET_LOC(src); /* TODO: use *HCL_CNODE_GET_LOC(cmd) instead? */
	cf->u._class.cmd_cnode = cmd;

	if (superclass) PUSH_CFRAME (hcl, COP_COMPILE_OBJECT, superclass); /* 1 - superclass expression */
	return 0;
}

static HCL_INLINE int compile_class_p1 (hcl_t* hcl)
{
	/* collect information about declared class-level variables */
	hcl_cframe_t* cf;
	hcl_cnode_t* obj;
	hcl_oow_t saved_tv_wcount, saved_tv_slen;
	class_vardcl_t vardcl;

	cf = GET_TOP_CFRAME(hcl);
	obj = cf->operand;

	saved_tv_wcount = hcl->c->tv.wcount;
	saved_tv_slen = hcl->c->tv.s.len;

	HCL_MEMSET (&vardcl, 0, HCL_SIZEOF(vardcl));

	if (obj && HCL_CNODE_IS_CONS(obj))
	{
		/* class-level variables - instance variables and class variable
		 *  - class X [ a b c [d e] x ] { ... }
		 *  - a b c are instance variables.
		 *  - d e, enclsoed in another [], are class variables.
		 * */
		hcl_cnode_t* tmp;
		tmp = HCL_CNODE_CONS_CAR(obj);
		if (HCL_CNODE_IS_CONS_CONCODED(tmp, HCL_CONCODE_TUPLE))
		{
			if (collect_vardcl_for_class(hcl, obj, &obj, &vardcl) <= -1) return -1;
		}
	}

	if (vardcl.nivars > 0)
	{
		hcl_oop_t tmp;
		int adj;

		if (vardcl.nivars > HCL_SMOOI_MAX)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARFLOOD, HCL_CNODE_GET_LOC(cf->operand), HCL_NULL, "too many(%zu) instance variables", vardcl.nivars);
			goto oops;
		}

		/* set starting point past the added space (+1 to index, -1 to length) */
		adj = (hcl->c->tv.s.ptr[vardcl.ivar_start] == ' ');
		tmp = hcl_makestring(hcl, &hcl->c->tv.s.ptr[vardcl.ivar_start + adj], vardcl.ivar_len - adj, 0);
		if (HCL_UNLIKELY(!tmp)) goto oops;
		if (emit_push_literal(hcl, tmp, &cf->u._class.start_loc) <= -1) goto oops;
	}

	if (vardcl.ncvars > 0)
	{
		hcl_oop_t tmp;
		int adj;

		if (vardcl.ncvars > HCL_SMOOI_MAX)
		{
			/* TOOD: change the error location ?? */
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARFLOOD, HCL_CNODE_GET_LOC(cf->operand), HCL_NULL, "too many(%zu) class variables", vardcl.ncvars);
			goto oops;
		}

		adj = (hcl->c->tv.s.ptr[vardcl.cvar_start] == ' ');
		tmp = hcl_makestring(hcl, &hcl->c->tv.s.ptr[vardcl.cvar_start + adj], vardcl.cvar_len - adj, 0);
		if (HCL_UNLIKELY(!tmp)) goto oops;
		if (emit_push_literal(hcl, tmp, &cf->u._class.start_loc) <= -1) goto oops;
	}

	if (check_block_expression_as_body(hcl, obj, cf->u._class.cmd_cnode, FOR_CLASS) <= -1) return -1;

	if (push_clsblk(hcl, &cf->u._class.start_loc, vardcl.nivars, vardcl.ncvars, &hcl->c->tv.s.ptr[vardcl.ivar_start], vardcl.ivar_len, &hcl->c->tv.s.ptr[vardcl.cvar_start], vardcl.cvar_len) <= -1) goto oops;
	if (push_cblk(hcl, &cf->u._class.start_loc, HCL_CBLK_TYPE_CLASS) <= -1) goto oops; /* the class block shall be treated as a control block, too */

	/* discard the instance variables and class variables in the temporary variable collection buffer
	 * because they have been pushed to the class block structure */
	hcl->c->tv.s.len = saved_tv_slen;
	hcl->c->tv.wcount = saved_tv_wcount;

	/* class_enter nsuperclasses, nivars, ncvars  */
	if (emit_byte_instruction(hcl, HCL_CODE_CLASS_ENTER, &cf->u._class.start_loc) <= -1) goto oops;
	if (emit_long_param(hcl, cf->u._class.nsuperclasses) <= -1) goto oops;
	if (emit_long_param(hcl, vardcl.nivars) <= -1) goto oops;
	if (emit_long_param(hcl, vardcl.ncvars) <= -1) goto oops;

	/* remember the first byte code position to be emitted for the body of
	 * this class. this posistion is used for empty class body check at the
	 * end of the class before 'class_exit' is generated */
	hcl->c->clsblk.info[hcl->c->clsblk.depth].class_start_inst_pos = hcl->code.bc.len;

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT_LIST, obj);
	return 0;

oops:
	hcl->c->tv.s.len = saved_tv_slen;
	hcl->c->tv.wcount = saved_tv_wcount;
	return -1;
}

static HCL_INLINE int compile_class_p3 (hcl_t* hcl)
{
	hcl_cframe_t* cf;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_CLASS_P3);

	/* should i make the assignment in POST?  or after variable declarations immediately? */
/* TODO: emit instruction to store into the class name...? */
/* TODO: NEED TO EMIT POP_STACKTOP???? IN THIS CASE CLASS_EXIT MUST PUSH SOMETHING? */
	if (emit_byte_instruction(hcl, HCL_CODE_CLASS_EXIT, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	POP_CFRAME (hcl);
	return 0;
}

static HCL_INLINE int compile_class_p2 (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_cnode_t* class_name;
	hcl_loc_t class_loc;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_CLASS_P2);
	/*HCL_ASSERT (hcl, cf->operand != HCL_NULL);*/

	class_name = cf->operand;
	class_loc = class_name? *HCL_CNODE_GET_LOC(class_name): cf->u._class.start_loc;

	if (hcl->code.bc.len > hcl->c->clsblk.info[hcl->c->clsblk.depth].class_start_inst_pos)
	{
		/* no instructions generated after the class_enter instruction */
		if (emit_byte_instruction(hcl, HCL_CODE_POP_STACKTOP, &class_loc) <= -1) return -1;
	}

	pop_cblk (hcl);
	pop_clsblk (hcl);  /* end of the class block */

	if (emit_byte_instruction(hcl, HCL_CODE_CLASS_PEXIT, &class_loc) <= -1) return -1; /* pop + exit */

	if (class_name)
	{
		/* a class name is treated like a global variable */
		SWITCH_TOP_CFRAME (hcl, COP_EMIT_SET, class_name);
		cf = GET_TOP_CFRAME(hcl);
		cf->u.set.vi.type = VAR_NAMED;
		cf->u.set.mode = VAR_ACCESS_STORE;
	}
	else
	{
		POP_CFRAME (hcl);
	}

	return 0;
}

/* ========================================================================= */

static int compile_fun (hcl_t* hcl, hcl_cnode_t* src, int defun)
{
	hcl_cnode_t* cmd, * obj, * args;
	hcl_oow_t va, nargs, nrvars, nlvars;
	hcl_ooi_t jump_inst_pos, lfbase_pos, lfsize_pos;
	hcl_oow_t saved_tv_wcount, tv_dup_start;
	hcl_cnode_t* defun_name;
	hcl_cnode_t* class_name;
	hcl_cframe_t* cf;
	unsigned int fun_type = FUN_PLAIN;

	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));

	saved_tv_wcount = hcl->c->tv.wcount;
	cmd = HCL_CNODE_CONS_CAR(src);
	obj = HCL_CNODE_CONS_CDR(src);

	class_name = HCL_NULL;

	if (!defun && (obj && HCL_CNODE_IS_CONS(obj)))
	{
		/* some inaccurate prior check if 'fun' is followed by an argument list
		 * without a function name. stop-gap measure to support 'fun' in place of 'defun' */
		HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(cmd, HCL_SYNCODE_FUN) || HCL_CNODE_IS_TYPED(cmd, HCL_CNODE_FUN));
		args = HCL_CNODE_CONS_CAR(obj);
		if (!HCL_CNODE_IS_ELIST_CONCODED(args, HCL_CONCODE_XLIST) &&
		    !HCL_CNODE_IS_CONS_CONCODED(args, HCL_CONCODE_XLIST))
		{
			/* not followed by an argument list */
			defun = 1;
			goto fun_as_defun;
		}
	}

	if (defun)
	{
		/* defun must be followed by an explicit function name */
		HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(cmd, HCL_SYNCODE_DEFUN));

	fun_as_defun:
		if (!obj)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGNAMELIST, HCL_CNODE_GET_LOC(src), HCL_NULL, "no name in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
			return -1;
		}
		else if (!HCL_CNODE_IS_CONS(obj))
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
			return -1;
		}

		defun_name = HCL_CNODE_CONS_CAR(obj);
		if (is_in_class_init_scope(hcl))
		{
			if ((HCL_CNODE_IS_DBLCOLONS(defun_name) || HCL_CNODE_IS_COLONSTAR(defun_name)))
			{
				/* class method - (defun ::xxxx () ...) inside class definition */
				/* class instantiation method - (defun :*xxxx() ...) inside class definition */
				obj = HCL_CNODE_CONS_CDR(obj);
				if (!obj)
				{
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGNAMELIST, HCL_CNODE_GET_LOC(src), HCL_NULL, "no name in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
					return -1;
				}

				fun_type = HCL_CNODE_IS_DBLCOLONS(defun_name)? FUN_CM: FUN_CIM;
				defun_name = HCL_CNODE_CONS_CAR(obj); /* advance to the actual name */
			}
			else
			{
				if (HCL_CNODE_IS_SYMBOL_PLAIN(defun_name))
				{
					/* probably this form - defun XXX:yyy () ...
					 * the class name must not be specified in the class initialization scope */
					hcl_cnode_t* tmp;
					tmp = HCL_CNODE_CONS_CDR(obj);
					if (tmp && HCL_CNODE_IS_CONS(tmp))
					{
						tmp = HCL_CNODE_CONS_CAR(tmp);
						if (HCL_CNODE_IS_COLON(tmp) || HCL_CNODE_IS_DBLCOLONS(tmp) || HCL_CNODE_IS_COLONSTAR(tmp)/*(HCL_CNODE_IS_SYMBOL_PLAIN(tmp)*/)
						{
							hcl_setsynerrbfmt (
								hcl, HCL_SYNERR_VARNAME,
								HCL_CNODE_GET_LOC(defun_name), HCL_CNODE_GET_TOK(defun_name),
								"function name not valid followed by %.*js",
								HCL_CNODE_GET_TOKLEN(tmp), HCL_CNODE_GET_TOKPTR(tmp));
							return -1;
						}
					}
				}

				fun_type = FUN_IM;
			}
		}
		else if (HCL_CNODE_IS_SYMBOL_PLAIN(defun_name))
		{
			hcl_cnode_t* tmp, marker;
			tmp = HCL_CNODE_CONS_CDR(obj);
			if (tmp && HCL_CNODE_IS_CONS(tmp))
			{
				hcl_cnode_t* marker;
				marker = HCL_CNODE_CONS_CAR(tmp);
				if (HCL_CNODE_IS_COLON(marker) || HCL_CNODE_IS_DBLCOLONS(marker) || HCL_CNODE_IS_COLONSTAR(marker))
				{
					/* fun A:aaa A::aaa A:*aaa */

					tmp = HCL_CNODE_CONS_CDR(tmp);
					if (tmp && HCL_CNODE_IS_CONS(tmp))
					{
						hcl_cnode_t* cand;
						cand = HCL_CNODE_CONS_CAR(tmp);
						if (HCL_CNODE_IS_SYMBOL_PLAIN(cand))
						{
							/* out-of-class method definition
							 * for defun String:length() { ...  },
							 * class_name is String, defun_name is length. */
							fun_type = HCL_CNODE_IS_DBLCOLONS(marker)? FUN_CM:
							           HCL_CNODE_IS_COLONSTAR(marker)? FUN_CIM: FUN_IM;

							/* indicates that this method is defined using the AAA:bbb syntax.
							 * the form of method defintion can still be inside a class if this
							 * form is place inside another normal method.
							 *  class X {
							 *    fun x()  {
							 *      fun J:q() { .... } ## this defintion
							 *    }
							 *  }
							 * */
							fun_type |= 0x100;

							class_name = defun_name;
							defun_name = HCL_CNODE_CONS_CAR(tmp);
							obj = tmp;
						}
					}
				}
			}
		}

		if (!HCL_CNODE_IS_SYMBOL(defun_name))
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAME, HCL_CNODE_GET_LOC(defun_name), HCL_CNODE_GET_TOK(defun_name), "function name not symbol in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
			return -1;
		}

		if (HCL_CNODE_SYMBOL_SYNCODE(defun_name)) /*|| HCL_OBJ_GET_FLAGS_KERNEL(defun_name) >= 1) */
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNEDVARNAME, HCL_CNODE_GET_LOC(defun_name), HCL_CNODE_GET_TOK(defun_name), "special symbol not to be used as function name");
			return -1;
		}

		obj = HCL_CNODE_CONS_CDR(obj);
	}
	else
	{
		HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(cmd, HCL_SYNCODE_FUN) || HCL_CNODE_IS_TYPED(cmd, HCL_CNODE_FUN));
		defun_name = HCL_NULL;
	}

	if (!obj)
	{
	no_arg_list:
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGNAMELIST, HCL_CNODE_GET_LOC(src), HCL_NULL, "argument list missing in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
	redundant_cdr:
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in argument list in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	/* process the argument list */
	va = 0;
	nargs = 0;
	nrvars = 0;
	args = HCL_CNODE_CONS_CAR(obj);
	HCL_ASSERT (hcl, args != HCL_NULL);

	if (HCL_CNODE_IS_ELIST_CONCODED(args, HCL_CONCODE_XLIST))
	{
		/* empty list - no argument - (fun () (+ 10 20)) */
	}
	else if (!HCL_CNODE_IS_CONS_CONCODED(args, HCL_CONCODE_XLIST))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGNAMELIST, HCL_CNODE_GET_LOC(args), HCL_CNODE_GET_TOK(args), "no argument list in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else
	{
		hcl_cnode_t* arg, * dcl;
		int in_ret_args = 0;

		tv_dup_start = hcl->c->tv.s.len;
		dcl = args;
		do
		{
			arg = HCL_CNODE_CONS_CAR(dcl);

			if (in_ret_args)
			{
				if (!HCL_CNODE_IS_SYMBOL(arg))
				{
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAME, HCL_CNODE_GET_LOC(arg), HCL_CNODE_GET_TOK(arg), "return variable not symbol in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
					return -1;
				}

				if (HCL_CNODE_IS_SYMBOL(arg) && HCL_CNODE_SYMBOL_SYNCODE(arg) /* || HCL_OBJ_GET_FLAGS_KERNEL(arg) >= 2 */)
				{
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNEDVARNAME, HCL_CNODE_GET_LOC(arg), HCL_CNODE_GET_TOK(arg), "special symbol not to be declared as return variable in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
					return -1;
				}

				if (add_temporary_variable(hcl, HCL_CNODE_GET_TOK(arg), tv_dup_start) <= -1)
				{
					if (hcl->errnum == HCL_EEXIST)
					{
						hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAMEDUP, HCL_CNODE_GET_LOC(arg), HCL_CNODE_GET_TOK(arg), "return variable duplicate in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
					}
					return -1;
				}
				nrvars++;
			}
			else if (va)
			{
				if (HCL_CNODE_IS_DBLCOLONS(arg))
				{
					in_ret_args = 1;
				}
				else
				{
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_CNODE, HCL_CNODE_GET_LOC(arg), HCL_CNODE_GET_TOK(arg), "unexpected element in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
					return -1;
				}
			}
			else
			{
				if (HCL_CNODE_IS_DBLCOLONS(arg))
				{
					in_ret_args = 1;
				}
				else if (HCL_CNODE_IS_ELLIPSIS(arg))
				{
					va = 1;
				}
				else if (!HCL_CNODE_IS_SYMBOL(arg))
				{
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGNAME, HCL_CNODE_GET_LOC(arg), HCL_CNODE_GET_TOK(arg), "argument not symbol in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
					return -1;
				}
				else
				{
					if (HCL_CNODE_IS_SYMBOL(arg) && HCL_CNODE_SYMBOL_SYNCODE(arg) /* || HCL_OBJ_GET_FLAGS_KERNEL(arg) >= 2 */)
					{
						hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNEDARGNAME, HCL_CNODE_GET_LOC(arg), HCL_CNODE_GET_TOK(arg), "special symbol not to be declared as argument in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
						return -1;
					}

					if (add_temporary_variable(hcl, HCL_CNODE_GET_TOK(arg), tv_dup_start) <= -1)
					{
						if (hcl->errnum == HCL_EEXIST)
						{
							hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGNAMEDUP, HCL_CNODE_GET_LOC(arg), HCL_CNODE_GET_TOK(arg), "argument duplicate in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
						}
						return -1;
					}
					nargs++;
				}
			}

			dcl = HCL_CNODE_CONS_CDR(dcl);
			if (!dcl) break;

			if (!HCL_CNODE_IS_CONS(dcl))
			{
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(dcl), HCL_CNODE_GET_TOK(dcl), "redundant cdr in argument list in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
				return -1;
			}
		}
		while (1);
	}

	if (nargs > MAX_CODE_NBLKARGS) /*TODO: change this limit to max call argument count */
	{
		/* while an integer object is pused to indicate the number of
		 * block arguments, evaluation which is done by message passing
		 * limits the number of arguments that can be passed. so the
		 * check is implemented */
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGFLOOD, HCL_CNODE_GET_LOC(args), HCL_NULL, "too many(%zu) arguments in %.*js", nargs, HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	if (nrvars > MAX_CODE_NBLKLVARS)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARFLOOD, HCL_CNODE_GET_LOC(args), HCL_NULL, "too many(%zu) return variables in %.*js", nrvars, HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	HCL_ASSERT (hcl, nargs + nrvars == hcl->c->tv.wcount - saved_tv_wcount);


	/*
	 * defun aa(a b) { ...	};
	 * (defun aa(a b) { ... })
	 *
	 * the block expression must be the first and the only expression at the body position.
	 * the variable declaration can't be placed before the block expression.
	 * it is supported inside the block expression itself.
	 */
	hcl_cnode_t* blk;
	blk = HCL_CNODE_CONS_CDR(obj);
	if (check_block_expression_as_body(hcl, blk, cmd, FOR_NONE) <= -1) return -1;
	obj = blk;
	nlvars = 0; /* no known local variables until the actual block is processed */

	HCL_ASSERT (hcl, nargs + nrvars + nlvars == hcl->c->tv.wcount - saved_tv_wcount);

	if (push_fnblk(hcl, HCL_CNODE_GET_LOC(src), va, nargs, nrvars, nlvars, hcl->c->tv.wcount, hcl->c->tv.s.len, hcl->code.bc.len, hcl->code.lit.len, fun_type) <= -1) return -1;

	if (hcl->option.trait & HCL_TRAIT_INTERACTIVE)
	{
		/* MAKE_FUNCTION attr_mask_1 attr_mask_2 lfbase lfsize */
		if (emit_double_param_instruction(hcl, HCL_CODE_MAKE_FUNCTION, 0, 0, HCL_CNODE_GET_LOC(cmd)) <= -1) return -1;
		lfbase_pos = hcl->code.bc.len;
		if (emit_long_param(hcl, hcl->code.lit.len - hcl->c->fnblk.info[hcl->c->fnblk.depth - 1].lfbase) <= -1) return -1; /* lfbase(literal frame base) */
		lfsize_pos = hcl->code.bc.len; /* literal frame size */
		if (emit_long_param(hcl, 0) <= -1) return -1; /* place holder for lfsize */
	}
	else
	{
		/* MAKE_BLOCK attr_mask_1 attr_mask_2 - will patch attr_mask in pop_fnblk() */
		if (emit_double_param_instruction(hcl, HCL_CODE_MAKE_BLOCK, 0, 0, HCL_CNODE_GET_LOC(cmd)) <= -1) return -1;
	}

	HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX);  /* guaranteed in emit_byte_instruction() */
	jump_inst_pos = hcl->code.bc.len;
	/* specifying MAX_CODE_JUMP causes emit_single_param_instruction() to
	 * produce the long jump instruction (HCL_CODE_JUMP_FORWARD_X) */
	if (emit_single_param_instruction(hcl, HCL_CODE_JUMP_FORWARD_0, MAX_CODE_JUMP, HCL_CNODE_GET_LOC(cmd)) <= -1) return -1;

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT_LIST, obj); /* 1 */
	PUSH_SUBCFRAME (hcl, COP_POST_FUN, defun_name); /* 3*/
	cf = GET_SUBCFRAME(hcl);
	cf->u.fun.fun_type = fun_type;
	cf->u.fun.class_name = class_name;

	PUSH_SUBCFRAME (hcl, COP_EMIT_FUN, src); /* 2 */
	cf = GET_SUBCFRAME(hcl);
	cf->u.fun.fun_type = fun_type;
	cf->u.fun.jump_inst_pos = jump_inst_pos;

	if (hcl->option.trait & HCL_TRAIT_INTERACTIVE)
	{
		cf->u.fun.lfbase_pos = lfbase_pos;
		cf->u.fun.lfsize_pos = lfsize_pos;
	}

	return 0;
}

static int compile_return (hcl_t* hcl, hcl_cnode_t* src, int ret_from_home)
{
	hcl_cnode_t* obj, * val;
	hcl_cframe_t* cf;
	hcl_fnblk_info_t* fbi;
	hcl_ooi_t i;

	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));
	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_RETURN) ||
	                 HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_REVERT) ||
	                 HCL_CNODE_IS_TYPED(HCL_CNODE_CONS_CAR(src), HCL_CNODE_RETURN) ||
	                 HCL_CNODE_IS_TYPED(HCL_CNODE_CONS_CAR(src), HCL_CNODE_REVERT));

	fbi = &hcl->c->fnblk.info[hcl->c->fnblk.depth];
	obj = HCL_CNODE_CONS_CDR(src);

	for (i = hcl->c->cblk.depth; i > hcl->c->fnblk.info[hcl->c->fnblk.depth].cblk_base; --i)
	{
		switch (hcl->c->cblk.info[i]._type)
		{
			case HCL_CBLK_TYPE_LOOP:
				/* do nothing */
				break;

			case HCL_CBLK_TYPE_TRY:
				if (emit_byte_instruction(hcl, HCL_CODE_TRY_EXIT, HCL_CNODE_GET_LOC(src)) <= -1) return -1;
				break;

			case HCL_CBLK_TYPE_CLASS:
				if (emit_byte_instruction(hcl, HCL_CODE_CLASS_EXIT, HCL_CNODE_GET_LOC(src)) <= -1) return -1;
				break;
		}
	}

	if (fbi->tmpr_nrvars > 0)
	{
		hcl_cnode_t* tmp = HCL_CNODE_CONS_CAR(src);

		if (ret_from_home)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNED, HCL_CNODE_GET_LOC(src), HCL_NULL, "%.*js not compatible with return variables", HCL_CNODE_GET_TOKLEN(tmp), HCL_CNODE_GET_TOKPTR(tmp));
			return -1;
		}

		/* if a return variable are specified in the current function block, the return statement must not be followed by a return value */
		if (obj)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNED, HCL_CNODE_GET_LOC(src), HCL_NULL, "use of return value in %.*js not compatible with return variables", HCL_CNODE_GET_TOKLEN(tmp), HCL_CNODE_GET_TOKPTR(tmp));
			return -1;
		}

/* TODO: pop stack if this is not the first statement... */
		if (emit_byte_instruction(hcl, HCL_CODE_PUSH_RETURN_R, HCL_CNODE_GET_LOC(tmp)) <= -1) return -1;
		POP_CFRAME (hcl);
	}
	else
	{
		if (!obj)
		{
	/* TODO: should i allow (return)? does it return the last value on the stack? */
			/* no value */
			hcl_cnode_t* tmp = HCL_CNODE_CONS_CAR(src);
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(src), HCL_NULL, "no value specified in %.*js", HCL_CNODE_GET_TOKLEN(tmp), HCL_CNODE_GET_TOKPTR(tmp));
			return -1;
		}
		else if (!HCL_CNODE_IS_CONS(obj))
		{
			hcl_cnode_t* tmp = HCL_CNODE_CONS_CAR(src);
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(tmp), HCL_CNODE_GET_TOKPTR(tmp));
			return -1;
		}

		val = HCL_CNODE_CONS_CAR(obj);

		obj = HCL_CNODE_CONS_CDR(obj);
		if (obj)
		{
			hcl_cnode_t* tmp = HCL_CNODE_CONS_CAR(src);
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "more than 1 argument in %.*js", HCL_CNODE_GET_TOKLEN(tmp), HCL_CNODE_GET_TOKPTR(tmp));
			return -1;
		}

		SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, val);

		PUSH_SUBCFRAME (hcl, COP_EMIT_RETURN, src);
		cf = GET_SUBCFRAME(hcl);
		cf->u._return.from_home = ret_from_home;
	}

	return 0;
}

static int compile_set (hcl_t* hcl, hcl_cnode_t* src)
{
	hcl_cframe_t* cf;
	hcl_cnode_t* cmd, * obj, * var, * val;
	hcl_var_info_t vi;
	int x;

	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));
	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_SET));

	cmd = HCL_CNODE_CONS_CAR(src);
	obj = HCL_CNODE_CONS_CDR(src);

	if (!obj)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAME, HCL_CNODE_GET_LOC(src), HCL_NULL, "no variable name in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	var = HCL_CNODE_CONS_CAR(obj);
	if (!HCL_CNODE_IS_SYMBOL(var) && !HCL_CNODE_IS_DSYMBOL_CLA(var))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAME, HCL_CNODE_GET_LOC(var), HCL_CNODE_GET_TOK(var), "variable name not symbol in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	if (HCL_CNODE_IS_SYMBOL(var) && HCL_CNODE_SYMBOL_SYNCODE(var)/* || HCL_OBJ_GET_FLAGS_KERNEL(var) >= 2*/)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNEDVARNAME, HCL_CNODE_GET_LOC(var), HCL_CNODE_GET_TOK(var), "special symbol not to be used as variable name in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	obj = HCL_CNODE_CONS_CDR(obj);
	if (!obj)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(src), HCL_NULL, "no value specified in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	val = HCL_CNODE_CONS_CAR(obj);

	obj = HCL_CNODE_CONS_CDR(obj);
	if (obj)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "too many arguments to %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, val);

	x = find_variable_backward_with_token(hcl, var, &vi);
	if (x <= -1) return -1;

	if (x == 0)
	{
		if (HCL_CNODE_IS_DSYMBOL_CLA(var))
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAMEUNKNOWN, HCL_CNODE_GET_LOC(var), HCL_CNODE_GET_TOK(var), "unknown class-level variable name", HCL_CNODE_GET_TOKLEN(var), HCL_CNODE_GET_TOKPTR(var));
			return -1;

		}

		PUSH_SUBCFRAME (hcl, COP_EMIT_SET, var); /* set doesn't evaluate the variable name */
		cf = GET_SUBCFRAME(hcl);
		cf->u.set.vi.type = VAR_NAMED;
	}
	else
	{
		/* the check in compile_fun() must ensure this condition */
		PUSH_SUBCFRAME (hcl, COP_EMIT_SET, cmd);
		cf = GET_SUBCFRAME(hcl);
		cf->u.set.vi = vi;
	}
	cf->u.set.mode = VAR_ACCESS_STORE;

	return 0;
}

static int compile_set_r (hcl_t* hcl, hcl_cnode_t* src)
{
	hcl_cframe_t* cf;
	hcl_cnode_t* cmd, * obj, * var, * val, * var_start;
	hcl_oow_t nvars, i;
	hcl_var_info_t vi;

	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));
	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_SET_R));

	cmd = HCL_CNODE_CONS_CAR(src);
	obj = HCL_CNODE_CONS_CDR(src);

	if (!obj)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAME, HCL_CNODE_GET_LOC(src), HCL_NULL, "no variable name in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	nvars = 0;
	var_start = obj;
	do
	{
		var = HCL_CNODE_CONS_CAR(obj);
		if (!HCL_CNODE_IS_SYMBOL(var))
		{
			if (nvars > 0) break;
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAME, HCL_CNODE_GET_LOC(var), HCL_CNODE_GET_TOK(var), "variable name not symbol in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
			return -1;
		}

		if (HCL_CNODE_SYMBOL_SYNCODE(var)/* || HCL_OBJ_GET_FLAGS_KERNEL(var) >= 2*/)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNEDVARNAME, HCL_CNODE_GET_LOC(var), HCL_CNODE_GET_TOK(var), "special symbol not to be used as variable name in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
			return -1;
		}

		nvars++;
		obj = HCL_CNODE_CONS_CDR(obj);
	}
	while (obj);


	if (!obj)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(src), HCL_NULL, "no value specified in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	val = HCL_CNODE_CONS_CAR(obj);

	obj = HCL_CNODE_CONS_CDR(obj);
	if (obj)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "too many arguments to %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT_R, val); /* special for set_r */
	cf = GET_TOP_CFRAME(hcl);
	cf->u.obj_r.nrets = nvars; /* number of return variables to get assigned */

	for (i = 0, obj = var_start; i < nvars; i++, obj = HCL_CNODE_CONS_CDR(obj))
	{
		int x;

		var = HCL_CNODE_CONS_CAR(obj);

		x = find_variable_backward_with_token(hcl, var, &vi);
		if (x <= -1) return -1;

		if (x == 0)
		{
			if (HCL_CNODE_IS_DSYMBOL_CLA(var))
			{
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAMEUNKNOWN, HCL_CNODE_GET_LOC(var), HCL_CNODE_GET_TOK(var), "unknown class-level variable name", HCL_CNODE_GET_TOKLEN(var), HCL_CNODE_GET_TOKPTR(var));
				return -1;
			}

			PUSH_SUBCFRAME (hcl, COP_EMIT_SET, var); /* set_r doesn't evaluate the variable name */
			cf = GET_SUBCFRAME(hcl);
			cf->u.set.vi.type = VAR_NAMED;
		}
		else
		{
			PUSH_SUBCFRAME (hcl, COP_EMIT_SET, cmd);
			cf = GET_SUBCFRAME(hcl);
			cf->u.set.vi = vi;
		}

		/*
		 * (defun f(x y ::: aa bb cc) ....)
		 * (set_r a b c (f 1 2))
		 *
		 *    the call to f
		 *       call 2 3  ; 2 arguments, 3 return variables (CALL_R)
		 *                 ; 3 to be emitted from cf->u.obj_r.nrets
		 *                 ; this gets remembered in req_nrvars of the created context.
		 *
		 *    the return from f must push 3 values.
		 *        push_return_r  ; as remembered in the ctx->req_nrvars
		 *
		 *    emit store_into_xxx instruction for the first return variable assignment.
		 *    emit pop_into_xxx instructions for the rest.
		 *        pop_into c
		 *        pop_into b
		 *        store_into a
		 */
		cf->u.set.mode = (i <= 0)? VAR_ACCESS_STORE: VAR_ACCESS_POP; /* STORE_INTO or POP_INTO  */
	}

	return 0;
}

/* ========================================================================= */

static int compile_try (hcl_t* hcl, hcl_cnode_t* src)
{
	hcl_cnode_t* cmd, * obj;
	hcl_cframe_t* cf;
	hcl_ooi_t jump_inst_pos;

	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));

	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_TRY) || HCL_CNODE_IS_TYPED(HCL_CNODE_CONS_CAR(src), HCL_CNODE_TRY));

	/* (try
	 *   (perform this)
	 *   (perform that)
	 *   (throw 10)
	 *  catch (x)
	 *   (perform xxx)
	 *   (perform yyy)
	 * )
	 */
	cmd = HCL_CNODE_CONS_CAR(src);
	obj = HCL_CNODE_CONS_CDR(src);

	if (!obj)
	{
		/* no value */
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(src), HCL_NULL, "no expression specified in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	if (push_cblk(hcl, HCL_CNODE_GET_LOC(src), HCL_CBLK_TYPE_TRY) <= -1) return -1;

/* TODO: HCL_TRAIT_INTERACTIVE??? */

	jump_inst_pos = hcl->code.bc.len;
	if (emit_single_param_instruction(hcl, HCL_CODE_TRY_ENTER, MAX_CODE_JUMP, HCL_CNODE_GET_LOC(cmd)) <= -1) return -1;

	if (check_block_expression_as_body(hcl, obj, cmd, FOR_TRY) <= -1) return -1;

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_TRY_OBJECT_LIST, obj);  /* 1*/
	PUSH_SUBCFRAME (hcl, COP_POST_TRY, cmd); /* 2 */
	cf = GET_SUBCFRAME(hcl);
	cf->u.post_try.jump_inst_pos = jump_inst_pos;

	return 0;
}

static HCL_INLINE int patch_nearest_post_try (hcl_t* hcl, hcl_ooi_t* catch_skip_jip)
{
	hcl_ooi_t jip, block_code_size;
	hcl_cframe_t* cf;

	cf = find_cframe_from_top(hcl, COP_POST_TRY);
	HCL_ASSERT (hcl, cf != HCL_NULL);
	HCL_ASSERT (hcl, cf->opcode == COP_POST_TRY);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	jip = cf->u.post_try.jump_inst_pos;

	/* HCL_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD instruction */
	block_code_size = hcl->code.bc.len - jip - (HCL_CODE_LONG_PARAM_SIZE + 1);

	if (block_code_size == 0)
	{
		/* no body in try */
/* TODO: is this correct??? */
		if (emit_byte_instruction(hcl, HCL_CODE_PUSH_NIL, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;
	}

	if (emit_byte_instruction(hcl, HCL_CODE_TRY_EXIT, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	*catch_skip_jip = hcl->code.bc.len;
	if (emit_single_param_instruction(hcl, HCL_CODE_JUMP_FORWARD_0,  MAX_CODE_JUMP, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	/* HCL_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD instruction */
	block_code_size = hcl->code.bc.len - jip - (HCL_CODE_LONG_PARAM_SIZE + 1);

	if (block_code_size > MAX_CODE_JUMP * 2)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_BLKFLOOD, HCL_CNODE_GET_LOC(cf->operand), HCL_NULL, "code too big - size %zu", block_code_size);
		return -1;
	}
	patch_long_jump (hcl, jip, block_code_size); /* patch TRY_ENTER */

	return 0;
}

static HCL_INLINE int compile_catch (hcl_t* hcl)
{
	hcl_cnode_t* cmd, * obj, * src, * exarg;
	hcl_cframe_t* cf;
	hcl_ooi_t jump_inst_pos;
	hcl_oow_t exarg_offset;
	hcl_var_info_t vi;
	hcl_fnblk_info_t* fbi;
	hcl_oow_t par_tmprcnt;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_CATCH);

	src = cf->operand;
	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));
	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_CATCH) || HCL_CNODE_IS_TYPED(HCL_CNODE_CONS_CAR(src), HCL_CNODE_CATCH));

	cmd = HCL_CNODE_CONS_CAR(src);
	obj = HCL_CNODE_CONS_CDR(src);

	if (!obj)
	{
/* TODO: change error code */
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAME, HCL_CNODE_GET_LOC(src), HCL_NULL, "no exception variable for '%.*js'", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	exarg = HCL_CNODE_CONS_CAR(obj);
	if (!HCL_CNODE_IS_CONS_CONCODED(exarg, HCL_CONCODE_XLIST) || hcl_countcnodecons(hcl, exarg) != 1)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAME, HCL_CNODE_GET_LOC(exarg), HCL_NULL, "not proper exception variable in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	exarg = HCL_CNODE_CONS_CAR(exarg);
	if (!HCL_CNODE_IS_SYMBOL(exarg) || HCL_CNODE_SYMBOL_SYNCODE(exarg))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAME, HCL_CNODE_GET_LOC(exarg), HCL_CNODE_GET_TOK(exarg), "invalid exception variable name in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	/* add the exception variable to the local variable list. increase the number of local variables */
	exarg_offset = hcl->c->tv.s.len + 1; /* when the variable name is added, its offset will be the current length + 1 for a space character added */

	if (hcl->c->fnblk.depth > 0)
	{
		fbi = &hcl->c->fnblk.info[hcl->c->fnblk.depth - 1]; /* parent block */
		par_tmprcnt = fbi->tmprcnt;
	}
	else
	{
		par_tmprcnt = 0;
	}

	/* fill the variable information structure as if it's found by find_variable_backward_with_token().
	 * we know it's the last variable as add_temporary_variable() is called below.
	 * there is no need to call find_variable_backward_with_token() */
	vi.type = VAR_INDEXED;
	vi.ctx_offset = 0;
	vi.index_in_ctx = hcl->c->tv.wcount - par_tmprcnt;
	if (add_temporary_variable(hcl, HCL_CNODE_GET_TOK(exarg), hcl->c->tv.s.len) <= -1) return -1;

	fbi = &hcl->c->fnblk.info[hcl->c->fnblk.depth];
	HCL_ASSERT (hcl, fbi->tmprlen == hcl->c->tv.s.len - HCL_CNODE_GET_TOKLEN(exarg) - 1);
	HCL_ASSERT (hcl, fbi->tmprcnt == vi.index_in_ctx + par_tmprcnt);
	fbi->tmprlen = hcl->c->tv.s.len;
	fbi->tmprcnt = hcl->c->tv.wcount;
	fbi->tmpr_nlvars = fbi->tmpr_nlvars + 1;

	HCL_ASSERT (hcl, fbi->tmpr_nargs + fbi->tmpr_nrvars + fbi->tmpr_nlvars == fbi->tmprcnt - par_tmprcnt);

	obj = HCL_CNODE_CONS_CDR(obj);
	if (check_block_expression_as_body(hcl, obj, cmd, FOR_NONE) <= -1) return -1;

	/* jump_inst_pos hold the instruction pointer that skips the catch block at the end of the try block */
	patch_nearest_post_try (hcl, &jump_inst_pos);

	/* produce an instruction to store the exception value to an exception variable pushed by the 'throw' instruction */
	if (emit_variable_access(hcl, VAR_ACCESS_POP, &vi, HCL_CNODE_GET_LOC(src)) <= -1) return -1;

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT_LIST, obj);

	PUSH_SUBCFRAME (hcl, COP_POST_CATCH, cmd);
	cf = GET_SUBCFRAME(hcl);
	cf->u.post_catch.jump_inst_pos = jump_inst_pos;
	cf->u.post_catch.exarg_offset = exarg_offset; /* there is only 1 exception variable. using the offset is easier than to use the variable position */

	return 0;
}

static HCL_INLINE int post_try (hcl_t* hcl)
{
/* TODO: anything else? */
	pop_cblk (hcl);
	POP_CFRAME (hcl);
	return 0;
}

static HCL_INLINE int post_catch (hcl_t* hcl)
{
	hcl_ooi_t jip, block_code_size;
	hcl_cframe_t* cf;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf != HCL_NULL);
	HCL_ASSERT (hcl, cf->opcode == COP_POST_CATCH);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	jip = cf->u.post_catch.jump_inst_pos; /* jump instruction position between the try block and the catch block */

	/* HCL_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD instruction */
	block_code_size = hcl->code.bc.len - jip - (HCL_CODE_LONG_PARAM_SIZE + 1);

	if (block_code_size == 0)
	{
		/* no body in try */
/* TODO: is this correct??? */
		if (emit_byte_instruction(hcl, HCL_CODE_PUSH_NIL, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;
		block_code_size++;
	}

	if (block_code_size > MAX_CODE_JUMP * 2)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_BLKFLOOD, HCL_CNODE_GET_LOC(cf->operand), HCL_NULL, "code too big - size %zu", block_code_size);
		return -1;
	}

	patch_long_jump (hcl, jip, block_code_size); /* patch the jump between the try block and the catch block */

	/* make the exception variable unsearchable outside the catch block.
	 * the variable entity is still be accounted into the local variable list. */
	kill_temporary_variable_at_offset (hcl, cf->u.post_catch.exarg_offset);

	POP_CFRAME (hcl);
	return 0;
}

static int compile_throw (hcl_t* hcl, hcl_cnode_t* src)
{
	hcl_cnode_t* obj, * val;
	/*hcl_cframe_t* cf;*/

	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));
	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_THROW) || HCL_CNODE_IS_TYPED(HCL_CNODE_CONS_CAR(src), HCL_CNODE_THROW));

	obj = HCL_CNODE_CONS_CDR(src);

	if (!obj)
	{
/* TODO: should i allow (throw)? does it return the last value on the stack? */
		/* no value */
		hcl_cnode_t* tmp = HCL_CNODE_CONS_CAR(src);
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(src), HCL_NULL, "no value specified in %.*js", HCL_CNODE_GET_TOKLEN(tmp), HCL_CNODE_GET_TOKPTR(tmp));
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_cnode_t* tmp = HCL_CNODE_CONS_CAR(src);
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HCL_CNODE_GET_TOKLEN(tmp), HCL_CNODE_GET_TOKPTR(tmp));
		return -1;
	}

	val = HCL_CNODE_CONS_CAR(obj);

	obj = HCL_CNODE_CONS_CDR(obj);
	if (obj)
	{
		hcl_cnode_t* tmp = HCL_CNODE_CONS_CAR(src);
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "more than 1 argument in %.*js", HCL_CNODE_GET_TOKLEN(tmp), HCL_CNODE_GET_TOKPTR(tmp));
		return -1;
	}

	/* throw can be located anywhere, however,
	 * if there is no outer try-catch, it ends up with a fatal runtime error */
	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, val);

	PUSH_SUBCFRAME (hcl, COP_EMIT_THROW, src);
	/*cf = GET_SUBCFRAME(hcl);*/
	return 0;
}

/* ========================================================================= */

static int compile_while (hcl_t* hcl, hcl_cnode_t* src, int next_cop)
{
	/* (while (xxxx) ... )
	 * (until (xxxx) ... ) */
	hcl_cnode_t* cmd, * obj, * cond, * body;
	hcl_oow_t cond_pos;
	hcl_cframe_t* cf;

	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS(src));
	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_UNTIL) ||
	                 HCL_CNODE_IS_SYMBOL_SYNCODED(HCL_CNODE_CONS_CAR(src), HCL_SYNCODE_WHILE) ||
	                 HCL_CNODE_IS_TYPED(HCL_CNODE_CONS_CAR(src), HCL_CNODE_UNTIL) ||
	                 HCL_CNODE_IS_TYPED(HCL_CNODE_CONS_CAR(src), HCL_CNODE_WHILE));
	HCL_ASSERT (hcl, next_cop == COP_POST_UNTIL_COND || next_cop == COP_POST_WHILE_COND);

	cmd = HCL_CNODE_CONS_CAR(src); /* while or until itself */
	obj = HCL_CNODE_CONS_CDR(src);

	if (!obj)
	{
		/* no value */
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(src), HCL_NULL, "no loop condition specified in %.*js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HCL_CNODE_IS_CONS(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "redundant cdr in %*.js", HCL_CNODE_GET_TOKLEN(cmd), HCL_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	if (push_cblk(hcl, HCL_CNODE_GET_LOC(src), HCL_CBLK_TYPE_LOOP) <= -1) return -1;

	HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX);
	cond_pos = hcl->code.bc.len; /* position where the bytecode for the conditional is emitted */

	cond = HCL_CNODE_CONS_CAR(obj);
	body = HCL_CNODE_CONS_CDR(obj);

	if (check_block_expression_as_body(hcl, body, cmd, FOR_NONE) <= -1) return -1;

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, cond); /* 1 */

	/* pass the cons cell branching to the conditional and the body. see post_while_cond() for the reason */
	PUSH_SUBCFRAME (hcl, next_cop, obj); /* 2 */
	cf = GET_SUBCFRAME(hcl);
	cf->u.post_while.cond_pos = cond_pos;
	cf->u.post_while.body_pos = -1; /* unknown yet*/
	cf->u.post_while.jump_inst_pos = -1; /* not needed */
	cf->u.post_while.start_loc = *HCL_CNODE_GET_LOC(src);

	return 0;
}

/* ========================================================================= */

static int compile_cons_array_expression (hcl_t* hcl, hcl_cnode_t* obj)
{
	/* #[ ] */
	hcl_ooi_t nargs;
	hcl_cframe_t* cf;

	nargs = hcl_countcnodecons(hcl, obj);
	if (nargs > MAX_CODE_PARAM)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGFLOOD, HCL_CNODE_GET_LOC(obj), HCL_NULL, "too many(%zd) elements in array", nargs);
		return -1;
	}

	SWITCH_TOP_CFRAME (hcl, COP_EMIT_MAKE_ARRAY, obj);
	cf = GET_TOP_CFRAME(hcl);
	cf->u.array_list.index = nargs;

	/* redundant cdr check is performed inside compile_object_list() */
	PUSH_SUBCFRAME (hcl, COP_COMPILE_ARRAY_LIST, obj);
	cf = GET_SUBCFRAME(hcl);
	cf->u.array_list.index = 0;

	return 0;
}

static int compile_cons_bytearray_expression (hcl_t* hcl, hcl_cnode_t* obj, int concode)
{
	/* compile the singular-type array such as byte array or char array */
	/* #b[ ] - e.g. #b[1, 2, 3] or #b[ 1 2 3 ] */
	hcl_ooi_t nargs;
	hcl_cframe_t* cf;

	nargs = hcl_countcnodecons(hcl, obj);
	if (nargs > MAX_CODE_PARAM)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGFLOOD, HCL_CNODE_GET_LOC(obj), HCL_NULL, "too many(%zd) elements in byte-array", nargs);
		return -1;
	}

	SWITCH_TOP_CFRAME (hcl, COP_EMIT_MAKE_BYTEARRAY, obj);
	cf = GET_TOP_CFRAME(hcl);
	cf->u.bytearray_list.elem_type = concode;
	cf->u.bytearray_list.index = nargs;

	/* redundant cdr check is performed inside compile_object_list() */
	PUSH_SUBCFRAME (hcl, COP_COMPILE_BYTEARRAY_LIST, obj);
	cf = GET_SUBCFRAME(hcl);
	cf->u.bytearray_list.elem_type = concode;
	cf->u.bytearray_list.index = 0;

	return 0;
}

static int compile_cons_dic_expression (hcl_t* hcl, hcl_cnode_t* obj)
{
	/* { } - e.g. {1:2, 3:4,"abc":def, "hwaddr":"00:00:00:01"} or { 1 2 3 4 } */
	hcl_ooi_t nargs;
	hcl_cframe_t* cf;

	nargs = hcl_countcnodecons(hcl, obj);
	if (nargs > MAX_CODE_PARAM)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGFLOOD, HCL_CNODE_GET_LOC(obj), HCL_NULL, "too many(%zd) elements in dictionary", nargs);
		return -1;
	}

	SWITCH_TOP_CFRAME (hcl, COP_EMIT_MAKE_DIC, obj);
	cf = GET_TOP_CFRAME(hcl);
	cf->u.dic_list.index = nargs >> 1; /* only the half */

	/* redundant cdr check is performed inside compile_object_list() */
	PUSH_SUBCFRAME (hcl, COP_COMPILE_DIC_LIST, obj);

	return 0;
}

static int compile_cons_qlist_expression (hcl_t* hcl, hcl_cnode_t* obj)
{
	/* #( 1 2  3 )
	 * #(1 (+ 2 3) 5) --> #(1 5 5)
	 * */
	SWITCH_TOP_CFRAME (hcl, COP_EMIT_MAKE_CONS, obj);
	PUSH_SUBCFRAME (hcl, COP_COMPILE_QLIST, obj);
	return 0;
}

static int compile_cons_alist_expression (hcl_t* hcl, hcl_cnode_t* cmd)
{
	/* assignment expression */
	/* (a := 20)
	 * ([a,b] := (xxx 20))
	 */

	hcl_cframe_t* cf;
	hcl_cnode_t* obj, * var, * val;
	hcl_var_info_t vi;
	int x;

	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS_CONCODED(cmd, HCL_CONCODE_ALIST));

	var = HCL_CNODE_CONS_CAR(cmd);
	obj = HCL_CNODE_CONS_CDR(cmd);

	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_PLAIN(var) || HCL_CNODE_IS_DSYMBOL_CLA(var) || HCL_CNODE_IS_CONS_CONCODED(var, HCL_CONCODE_TUPLE));
	HCL_ASSERT (hcl, obj && HCL_CNODE_IS_CONS(obj)); /* reader guaranteed */

	val = HCL_CNODE_CONS_CAR(obj);
	HCL_ASSERT (hcl, HCL_CNODE_CONS_CDR(obj) ==  HCL_NULL); /* reader guaranteed */

	if (HCL_CNODE_IS_CONS_CONCODED(var, HCL_CONCODE_TUPLE))
	{
		/* multi-variable assignment
		 * fun xxx(x :: p q) { p := x + 1; q := x + 2 }
		 * ([a,b] := (xxx 20)) */
		hcl_oow_t nvars, i;

		nvars = hcl_countcnodecons(hcl, var);

		SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT_R, val); /* special for set_r */
		cf = GET_TOP_CFRAME(hcl);
		cf->u.obj_r.nrets = nvars; /* number of return variables to get assigned */

		for (i = 0, obj = var; i < nvars; i++, obj = HCL_CNODE_CONS_CDR(obj))
		{
			int x;

			var = HCL_CNODE_CONS_CAR(obj);

			HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_PLAIN(var) || HCL_CNODE_IS_DSYMBOL_CLA(var)); /* reader guaranteed */

			x = find_variable_backward_with_token(hcl, var, &vi);
			if (x <= -1) return -1;

			if (x == 0)
			{
				if (HCL_CNODE_IS_DSYMBOL_CLA(var))
				{
					hcl_setsynerrbfmt (hcl,
						HCL_SYNERR_VARNAMEUNKNOWN, HCL_CNODE_GET_LOC(var), HCL_CNODE_GET_TOK(var),
						"unknown class-level variable name", HCL_CNODE_GET_TOKLEN(var), HCL_CNODE_GET_TOKPTR(var));
					return -1;
				}

				PUSH_SUBCFRAME (hcl, COP_EMIT_SET, var); /* set_r doesn't evaluate the variable name */
				cf = GET_SUBCFRAME(hcl);
				cf->u.set.vi.type = VAR_NAMED;
			}
			else
			{
				PUSH_SUBCFRAME (hcl, COP_EMIT_SET, cmd);
				cf = GET_SUBCFRAME(hcl);
				cf->u.set.vi = vi;
			}

			/*
			 * (defun f(x y ::: aa bb cc) ....)
			 * ([a b c] := (f 1 2))
			 *
			 *    the call to f
			 *       call 2 3  ; 2 arguments, 3 return variables (CALL_R)
			 *                 ; 3 to be emitted from cf->u.obj_r.nrets
			 *                 ; this gets remembered in req_nrvars of the created context.
			 *
			 *    the return from f must push 3 values.
			 *        push_return_r  ; as remembered in the ctx->req_nrvars
			 *
			 *    emit store_into_xxx instruction for the first return variable assignment.
			 *    emit pop_into_xxx instructions for the rest.
			 *        pop_into c
			 *        pop_into b
			 *        store_into a
			 */
			cf->u.set.mode = (i <= 0)? VAR_ACCESS_STORE: VAR_ACCESS_POP; /* STORE_INTO or POP_INTO  */
		}
	}
	else
	{
		/* single-variable assignment
		 * (a := 20) */

		SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, val);

		x = find_variable_backward_with_token(hcl, var, &vi);
		if (x <= -1) return -1;

		if (x == 0)
		{
			/* not found */
			if (HCL_CNODE_IS_DSYMBOL_CLA(var))
			{
				hcl_setsynerrbfmt (hcl,
					HCL_SYNERR_VARNAMEUNKNOWN, HCL_CNODE_GET_LOC(var), HCL_CNODE_GET_TOK(var),
					"unknown class-level variable name", HCL_CNODE_GET_TOKLEN(var), HCL_CNODE_GET_TOKPTR(var));
				return -1;

			}

			PUSH_SUBCFRAME (hcl, COP_EMIT_SET, var); /* set doesn't evaluate the variable name */
			cf = GET_SUBCFRAME(hcl);
			cf->u.set.vi.type = VAR_NAMED;
		}
		else
		{
			/* the check in compile_fun() must ensure this condition */
			PUSH_SUBCFRAME (hcl, COP_EMIT_SET, cmd);
			cf = GET_SUBCFRAME(hcl);
			cf->u.set.vi = vi;
		}
		cf->u.set.mode = VAR_ACCESS_STORE;
	}

	return 0;
}

static int compile_cons_xlist_expression (hcl_t* hcl, hcl_cnode_t* obj, int nrets)
{
	hcl_cnode_t* car;
	int syncode; /* syntax code of the first element */

	/* a valid function call
	 * (function-name argument-list)
	 *   function-name can be:
	 *     a symbol.
	 *     another function call.
	 * if the name is another function call, i can't know if the
	 * function name will be valid at the compile time.
	 */
	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS_CONCODED(obj, HCL_CONCODE_XLIST));

	car = HCL_CNODE_CONS_CAR(obj);
	switch (HCL_CNODE_GET_TYPE(car))
	{
		case HCL_CNODE_CLASS:
			if (compile_class(hcl, obj, 0) <= -1) return -1;
			goto done;

		case HCL_CNODE_FUN:
			if (compile_fun(hcl, obj, 0) <= -1) return -1;
			goto done;

		case HCL_CNODE_DO:
			if (compile_do(hcl, obj) <= -1) return -1;
			goto done;

		case HCL_CNODE_IF:
			if (compile_if(hcl, obj) <= -1) return -1;
			goto done;

		case HCL_CNODE_ELIF:
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_ELSE, HCL_CNODE_GET_LOC(car), HCL_CNODE_GET_TOK(car), "else without if");
			return -1;

		case HCL_CNODE_ELSE:
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_ELIF, HCL_CNODE_GET_LOC(car), HCL_CNODE_GET_TOK(car), "elif without if");
			return -1;

		case HCL_CNODE_THROW:
			if (compile_throw(hcl, obj) <= -1) return -1;
			goto done;

		case HCL_CNODE_TRY:
			if (compile_try(hcl, obj) <= -1) return -1;
			goto done;

		case HCL_CNODE_CATCH:
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_CATCH, HCL_CNODE_GET_LOC(car), HCL_CNODE_GET_TOK(car), "catch without try");
			return -1;

		case HCL_CNODE_BREAK:
			if (compile_break(hcl, obj) <= -1) return -1;
			goto done;

		case HCL_CNODE_CONTINUE:
			if (compile_continue(hcl, obj) <= -1) return -1;
			goto done;

		case HCL_CNODE_UNTIL:
			if (compile_while(hcl, obj, COP_POST_UNTIL_COND) <= -1) return -1;
			goto done;

		case HCL_CNODE_WHILE:
			if (compile_while(hcl, obj, COP_POST_WHILE_COND) <= -1) return -1;
			goto done;

		case HCL_CNODE_RETURN:
			/* (return 10)
			 * (return (+ 10 20)) */
			if (compile_return(hcl, obj, 0) <= -1) return -1;
			goto done;

		case HCL_CNODE_REVERT:
			if (compile_return(hcl, obj, 1) <= -1) return -1;
			goto done;
	}

	if (HCL_CNODE_IS_SYMBOL(car) && (syncode = HCL_CNODE_SYMBOL_SYNCODE(car)))
	{
		if (nrets > 0)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_CALLABLE, HCL_CNODE_GET_LOC(car), HCL_CNODE_GET_TOK(car), "not a function with return-variables");
			return -1;
		}

		switch (syncode)
		{
			case HCL_SYNCODE_AND:
				if (compile_and(hcl, obj) <= -1) return -1;
				break;

			case HCL_SYNCODE_BREAK:
				/* (break) */
				if (compile_break(hcl, obj) <= -1) return -1;
				break;

			case HCL_SYNCODE_CATCH:
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_CATCH, HCL_CNODE_GET_LOC(car), HCL_CNODE_GET_TOK(car), "catch without try");
				return -1;

			case HCL_SYNCODE_CLASS:
				if (compile_class(hcl, obj, 0) <= -1) return -1;
				break;

			case HCL_SYNCODE_CONTINUE:
				/* (continue)*/
				if (compile_continue(hcl, obj) <= -1) return -1;
				break;

			case HCL_SYNCODE_DEFCLASS:
				if (compile_class(hcl, obj, 1) <= -1) return -1;
				break;

			case HCL_SYNCODE_DEFUN:
				if (compile_fun(hcl, obj, 1) <= -1) return -1;
				break;

			case HCL_SYNCODE_DO:
				if (compile_do(hcl, obj) <= -1) return -1;
				break;

			case HCL_SYNCODE_ELSE:
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_ELSE, HCL_CNODE_GET_LOC(car), HCL_CNODE_GET_TOK(car), "else without if");
				return -1;

			case HCL_SYNCODE_ELIF:
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_ELIF, HCL_CNODE_GET_LOC(car), HCL_CNODE_GET_TOK(car), "elif without if");
				return -1;

			case HCL_SYNCODE_IF:
				if (compile_if(hcl, obj) <= -1) return -1;
				break;

			case HCL_SYNCODE_FUN:
				/* (fun (x y) (+ x y)) */
				if (compile_fun(hcl, obj, 0) <= -1) return -1;
				break;

			case HCL_SYNCODE_OR:
				if (compile_or(hcl, obj) <= -1) return -1;
				break;

			case HCL_SYNCODE_PLUS:
				if (compile_plus(hcl, obj) <= -1) return -1;
				break;

			case HCL_SYNCODE_RETURN:
				/* (return 10)
				 * (return (+ 10 20)) */
				if (compile_return(hcl, obj, 0) <= -1) return -1;
				break;

			case HCL_SYNCODE_REVERT:
				if (compile_return(hcl, obj, 1) <= -1) return -1;
				break;

			case HCL_SYNCODE_SET:
				/* (set x 10)
				 * (set x (fun (x y) (+ x y)) */
				if (compile_set(hcl, obj) <= -1) return -1;
				break;

			case HCL_SYNCODE_SET_R:
				/* (set-r a b (func 10 20)) */
				if (compile_set_r(hcl, obj) <= -1) return -1;
				break;

			case HCL_SYNCODE_THROW:
				if (compile_throw(hcl, obj) <= -1) return -1;
				break;

			case HCL_SYNCODE_TRY:
				if (compile_try(hcl, obj) <= -1) return -1;
				break;

			case HCL_SYNCODE_UNTIL:
				if (compile_while(hcl, obj, COP_POST_UNTIL_COND) <= -1) return -1;
				break;

			case HCL_SYNCODE_WHILE:
				if (compile_while(hcl, obj, COP_POST_WHILE_COND) <= -1) return -1;
				break;

			default:
				HCL_DEBUG3 (hcl, "Internal error - unknown syncode %d at %s:%d\n", syncode, __FILE__, __LINE__);
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_INTERN, HCL_CNODE_GET_LOC(car), HCL_NULL, "internal error - unknown syncode %d", syncode);
				return -1;
		}
	}
	else if (HCL_CNODE_IS_SYMBOL(car)  || HCL_CNODE_IS_DSYMBOL(car) ||
	         HCL_CNODE_IS_CONS_CONCODED(car, HCL_CONCODE_XLIST) ||
	         HCL_CNODE_IS_CONS_CONCODED(car, HCL_CONCODE_MLIST) ||
	         HCL_CNODE_IS_CONS_CONCODED(car, HCL_CONCODE_ALIST))
	{
		/* normal function call
		 *  (<operator> <operand1> ...) */
		hcl_ooi_t nargs;
		hcl_ooi_t oldtop;
		hcl_cframe_t* cf;
		hcl_cnode_t* cdr;

		/* NOTE: cframe management functions don't use the object memory.
		 *       many operations can be performed without taking GC into account */

		/* store the position of COP_EMIT_CALL to be produced with
		 * SWITCH_TOP_CFRAME() in oldtop for argument count patching
		 * further down */
		oldtop = GET_TOP_CFRAME_INDEX(hcl);
		HCL_ASSERT (hcl, oldtop >= 0);

		SWITCH_TOP_CFRAME (hcl, COP_EMIT_CALL, car); /* <4> */

		/* compile <operator> */
		PUSH_CFRAME (hcl, COP_COMPILE_OBJECT, car); /* <2> */

		/* compile <operand1> ... etc */
		cdr = HCL_CNODE_CONS_CDR(obj);

		if (!cdr)
		{
			nargs = 0;
		}
		else
		{
			if (!HCL_CNODE_IS_CONS(cdr))
			{
				/* (funname . 10) */
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(cdr), HCL_CNODE_GET_TOK(cdr), "redundant cdr in function call");
				return -1;
			}

			nargs = hcl_countcnodecons(hcl, cdr);
			if (nargs > MAX_CODE_PARAM)
			{
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGFLOOD, HCL_CNODE_GET_LOC(cdr), HCL_NULL, "too many(%zd) parameters in function call", nargs);
				return -1;
			}
		}

		if (HCL_CNODE_IS_SYMBOL(car) || HCL_CNODE_IS_DSYMBOL(car))
		{
			hcl_oop_cons_t sdc;

			/* only symbols are added to the system dictionary.
			 * perform this lookup only if car is a symbol */
			sdc = hcl_lookupsysdicforsymbol_noseterr(hcl, HCL_CNODE_GET_TOK(car));
			if (sdc)
			{
				hcl_oop_word_t sdv;
				sdv = (hcl_oop_word_t)HCL_CONS_CDR(sdc);
				if (HCL_IS_PRIM(hcl, sdv))
				{
					if (nargs < sdv->slot[1] || nargs > sdv->slot[2])
					{
						hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(car), HCL_NULL,
							"parameters count(%zd) mismatch in function call - %.*js - expecting %zu-%zu parameters", nargs, HCL_CNODE_GET_TOKLEN(car), HCL_CNODE_GET_TOKPTR(car), sdv->slot[1], sdv->slot[2]);
						return -1;
					}
				}
			}
		}

		/* redundant cdr check is performed inside compile_object_list() */
		PUSH_SUBCFRAME (hcl, COP_COMPILE_ARGUMENT_LIST, cdr); /* <3> */

		/* patch the argument count in the operand field of the COP_EMIT_CALL frame */
		cf = GET_CFRAME(hcl, oldtop);
		HCL_ASSERT (hcl, cf->opcode == COP_EMIT_CALL);
		cf->u.call.index = nargs;
		cf->u.call.nrets = nrets;

		/* arrange to push a dummy receiver to make the call look like a message send.
		 * if you change the dummy receiver instruction to something else, you must change
		 * the receiver value of the initial context in start_initial_process_and_context(), too */
		PUSH_CFRAME (hcl, COP_EMIT_PUSH_NIL, car); /* <1> this will be executed the COP_COMPILE_OBJECT car frame */
	}
	else
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_CALLABLE, HCL_CNODE_GET_LOC(car), HCL_CNODE_GET_TOK(car), "invalid callable in function call");
		return -1;
	}

done:
	return 0;
}

static int compile_cons_mlist_expression (hcl_t* hcl, hcl_cnode_t* obj, int nrets)
{
	hcl_cnode_t* car, * cdr, * rcv;
	hcl_ooi_t nargs;
	hcl_ooi_t oldtop;
	hcl_cframe_t* cf;
	int syncode; /* syntax code of the first element */

	/* message sending
	 *  (:<receiver> <operator> <operand1> ...)
	 */
	HCL_ASSERT (hcl, HCL_CNODE_IS_CONS_CONCODED(obj, HCL_CONCODE_MLIST));

	car = HCL_CNODE_CONS_CAR(obj);
	if (HCL_CNODE_IS_SYMBOL(car) && (syncode = HCL_CNODE_SYMBOL_SYNCODE(car)))
	{
		/* special symbols such as 'if' is not permitted here */
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNED, HCL_CNODE_GET_LOC(car), HCL_CNODE_GET_TOK(car), "unpermitted message receiver");
		return -1;
	}

	/* store the position of COP_EMIT_CALL to be produced with
	 * SWITCH_TOP_CFRAME() in oldtop for argument count patching
	 * further down */
	oldtop = GET_TOP_CFRAME_INDEX(hcl);
	HCL_ASSERT (hcl, oldtop >= 0);

	/* compile <receiver> */
	rcv = car; /* remember the receiver node to to push it later */
	SWITCH_TOP_CFRAME (hcl, COP_EMIT_SEND, rcv);

	/* compile <operator> */
	cdr = HCL_CNODE_CONS_CDR(obj);
	if (!cdr)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_CALLABLE, HCL_CNODE_GET_LOC(car), HCL_CNODE_GET_TOK(car), "missing message");
		return -1;
	}
	if (!HCL_CNODE_IS_CONS(cdr))
	{
		/* (<receiver> . 10) */
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(cdr), HCL_CNODE_GET_TOK(cdr), "redundant cdr in message send");
		return -1;
	}
	car = HCL_CNODE_CONS_CAR(cdr);
	if (HCL_CNODE_IS_SYMBOL_PLAIN(car))
	{
		PUSH_CFRAME (hcl, COP_EMIT_PUSH_SYMBOL, car);
	}
	else
	{
/* TODO: more sanity check on what can be used as a method */
		PUSH_CFRAME (hcl, COP_COMPILE_OBJECT, car);
	}

	/* compile <operand1> ... etc */
	cdr = HCL_CNODE_CONS_CDR(cdr);
	if (!cdr)
	{
		nargs = 0;
	}
	else
	{
		if (!HCL_CNODE_IS_CONS(cdr))
		{
			/* (funname . 10) */
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(cdr), HCL_CNODE_GET_TOK(cdr), "redundant cdr in function call");
			return -1;
		}

		nargs = hcl_countcnodecons(hcl, cdr);
		if (nargs > MAX_CODE_PARAM)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGFLOOD, HCL_CNODE_GET_LOC(cdr), HCL_NULL, "too many(%zd) parameters in function call", nargs);
			return -1;
		}
	}

#if 0
	if (HCL_CNODE_IS_SYMBOL(car) || HCL_CNODE_IS_DSYMBOL(car))
	{
		hcl_oop_cons_t sdc;

		/* only symbols are added to the system dictionary.
		 * perform this lookup only if car is a symbol */
		sdc = hcl_lookupsysdicforsymbol_noseterr(hcl, HCL_CNODE_GET_TOK(car));
		if (sdc)
		{
			hcl_oop_word_t sdv;
			sdv = (hcl_oop_word_t)HCL_CONS_CDR(sdc);
			if (HCL_IS_PRIM(hcl, sdv))
			{
				if (nargs < sdv->slot[1] || nargs > sdv->slot[2])
				{
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_ARGCOUNT, HCL_CNODE_GET_LOC(car), HCL_NULL,
						"parameters count(%zd) mismatch in function call - %.*js - expecting %zu-%zu parameters", nargs, HCL_CNODE_GET_TOKLEN(car), HCL_CNODE_GET_TOKPTR(car), sdv->slot[1], sdv->slot[2]);
					return -1;
				}
			}
		}
	}
#endif

	/* redundant cdr check is performed inside compile_object_list() */
	PUSH_SUBCFRAME (hcl, COP_COMPILE_ARGUMENT_LIST, cdr);

	/* patch the argument count in the operand field of the COP_EMIT_CALL frame */
	cf = GET_CFRAME(hcl, oldtop);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_SEND);
	cf->u.sendmsg.nargs = nargs;
	cf->u.sendmsg.nrets = nrets;
	cf->u.sendmsg.to_super = (HCL_CNODE_GET_TYPE(rcv) == HCL_CNODE_SUPER);

	PUSH_CFRAME (hcl, COP_COMPILE_OBJECT, rcv);
	return 0;
}

static int compile_cons_block_expression (hcl_t* hcl, hcl_cnode_t* obj)
{
	return compile_expression_block(hcl, obj, "block", CEB_IS_BLOCK);
}

static HCL_INLINE int compile_symbol (hcl_t* hcl, hcl_cnode_t* obj)
{
	hcl_var_info_t vi;
	int x;

	HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL(obj));

	if (HCL_CNODE_SYMBOL_SYNCODE(obj))
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNEDVARNAME, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "special symbol not to be used as variable name");
		return -1;
	}

	/* check if a symbol is a local variable */
	x = find_variable_backward_with_token(hcl, obj, &vi);
	if (x <= -1) return -1;

	if (x == 0)
	{
		hcl_oop_t sym, cons;
		hcl_oow_t index;
/* TODO: if i require all variables to be declared, this part is not needed and should handle it as an error */
/* TODO: change the scheme... allow declaration??? */
		/* global variable */
		sym = hcl_makesymbol(hcl, HCL_CNODE_GET_TOKPTR(obj), HCL_CNODE_GET_TOKLEN(obj));
		if (HCL_UNLIKELY(!sym)) return -1;

		cons = (hcl_oop_t)hcl_getatsysdic(hcl, sym);
		if (!cons)
		{
			cons = (hcl_oop_t)hcl_putatsysdic(hcl, sym, hcl->_undef);
			if (HCL_UNLIKELY(!cons)) return -1;
		}

		/* add the entire cons pair to the literal frame */
		if (add_literal(hcl, cons, &index) <= -1 ||
		    emit_single_param_instruction(hcl, HCL_CODE_PUSH_OBJECT_0, index, HCL_CNODE_GET_LOC(obj)) <= -1) return -1;

		return 0;
	}
	else
	{
		HCL_ASSERT (hcl, vi.type != VAR_NAMED);
		return emit_variable_access(hcl, VAR_ACCESS_PUSH, &vi, HCL_CNODE_GET_LOC(obj));
	}
}

static HCL_INLINE int compile_dsymbol (hcl_t* hcl, hcl_cnode_t* obj)
{
	hcl_oop_t cons;
	hcl_oow_t index;

/* TODO: need a total revamp on the dotted symbols.
 *       must differentiate module access and dictioary member access...
 *       must implementate dictionary member access syntax... */

	/* the dot notation collides with car/cdr separator? no. dotted symbols don't contains space.
	 * the car cdr separator must be a single character */
	{ /* HACK FOR NOW */
		const hcl_ooch_t* sep;
		hcl_oocs_t name;
		int x = 0;
		hcl_var_info_t vi;
		hcl_fnblk_info_t* fbi;

		name = *HCL_CNODE_GET_TOK(obj);
		fbi = &hcl->c->fnblk.info[hcl->c->fnblk.depth];

		sep = hcl_find_oochar(name.ptr, name.len, '.');
		HCL_ASSERT (hcl, sep != HCL_NULL);
		if (hcl_comp_oochars_bcstr(name.ptr, (sep - (const hcl_ooch_t*)name.ptr), "self") == 0)
		{
			/* instance variable?  or instance method? */
			if (fbi->fun_type >> 8)
			{
				/* if defined using A::xxx syntax, it's not possible to know the instance position of an instance variable.
				 * class X [ a b ] {
				 *   fun a() {
				 *      fun J::t() {
				 *        ## J has nothing to to with X in priciple even if J may point to X when a() is executed.
				 *        ## it's not meaningful to look up the variable `a` in the context of class X.
				 *        ## it must be prohibited to access instance variables using the self or super prefix
				 *        ## in this context
				 *        return self.a
				 *      }
				 *   }
				 * }
				 */
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAME, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "not allowed to prefix with self in out-of-class method context");
				return -1;
			}
			name.ptr = (hcl_ooch_t*)(sep + 1);
			name.len -= 5;
			x = find_variable_backward_with_word(hcl, &name, HCL_CNODE_GET_LOC(obj), 1, &vi);
		}
		else if (hcl_comp_oochars_bcstr(name.ptr, sep - (const hcl_ooch_t*)name.ptr, "super") == 0)
		{
			if (fbi->fun_type >> 8) /* if defined using A::xxx syntax */
			{
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAME, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "not allowed to prefix with super in out-of-class method context");
				return -1;
			}
			name.ptr = (hcl_ooch_t*)(sep + 1);
			name.len -= 6;
			x = find_variable_backward_with_word(hcl, &name, HCL_CNODE_GET_LOC(obj), 2, &vi); /* TODO: arrange to skip the current class */
		}

		if (x <= -1) return -1; /* error */
		if (x >= 1)
		{
			/* found */
			HCL_ASSERT (hcl, vi.type != VAR_NAMED);
			return emit_variable_access(hcl, VAR_ACCESS_PUSH, &vi, HCL_CNODE_GET_LOC(obj));
		}

/* TODO: check if it's the method name??? NOT POSSIBLE??? */

		/* if not found or not beginning with self/super, carry on with remaining resolution methods */
	}

	cons = (hcl_oop_t)hcl_lookupsysdicforsymbol_noseterr(hcl, HCL_CNODE_GET_TOK(obj));
	if (!cons)
	{
		/* query the module for information if it is the first time
		 * when the dotted symbol is seen */

		hcl_pfbase_t* pfbase;
		hcl_mod_t* mod;
		hcl_oop_t sym, val;
		unsigned int kernel_bits;

		pfbase = hcl_querymod(hcl, HCL_CNODE_GET_TOKPTR(obj), HCL_CNODE_GET_TOKLEN(obj), &mod);
		if (!pfbase)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAME, HCL_CNODE_GET_LOC(obj), HCL_CNODE_GET_TOK(obj), "unknown dotted symbol");
			return -1;
		}

		sym = hcl_makesymbol(hcl, HCL_CNODE_GET_TOKPTR(obj), HCL_CNODE_GET_TOKLEN(obj));
		if (HCL_UNLIKELY(!sym)) return -1;

		hcl_pushvolat (hcl, &sym);
		switch (pfbase->type)
		{
			case HCL_PFBASE_FUNC:
				kernel_bits = 2;
				val = hcl_makeprim(hcl, pfbase->handler, pfbase->minargs, pfbase->maxargs, mod);
				break;

			case HCL_PFBASE_VAR:
				kernel_bits = 1;
				val = hcl->_nil;
				break;

			case HCL_PFBASE_CONST:
				/* TODO: create a value from the pfbase information. it needs to get extended first
				 * can i make use of pfbase->handler type-cast to a differnt type? */
				kernel_bits = 2;
				val = hcl->_nil;
				break;

			default:
				hcl_popvolat (hcl);
				hcl_seterrbfmt (hcl, HCL_EINVAL, "invalid pfbase type - %d\n", pfbase->type);
				return -1;
		}

		if (!val || !(cons = (hcl_oop_t)hcl_putatsysdic(hcl, sym, val)))
		{
			hcl_popvolat (hcl);
			return -1;
		}
		hcl_popvolat (hcl);

		/* make this dotted symbol special that it can't get changed
		 * to a different value */
		HCL_OBJ_SET_FLAGS_KERNEL (sym, kernel_bits);
	}

	if (add_literal(hcl, cons, &index) <= -1 ||
	    emit_single_param_instruction(hcl, HCL_CODE_PUSH_OBJECT_0, index, HCL_CNODE_GET_LOC(obj)) <= -1) return -1;

	return 0;
}

HCL_UNUSED static int string_to_ooi (hcl_t* hcl, hcl_oocs_t* str, int radixed, hcl_ooi_t* num)
{
	/* it is not a generic conversion function.
	 * it assumes a certain pre-sanity check on the string
	 * done by the lexical analyzer */

	int v, negsign, base;
	const hcl_ooch_t* ptr, * end;
	hcl_oow_t value, old_value;

	negsign = 0;
	ptr = str->ptr,
	end = str->ptr + str->len;

	HCL_ASSERT (hcl, ptr < end);

	if (*ptr == '+' || *ptr == '-')
	{
		negsign = *ptr - '+';
		ptr++;
	}

	if (radixed)
	{
		HCL_ASSERT (hcl, ptr < end);

		if (*ptr != '#')
		{
			hcl_seterrbfmt (hcl, HCL_EINVAL, "radixed number not starting with # - %*.js", str->len, str->ptr);
			return -1;
		}
		ptr++; /* skip '#' */

		if (*ptr == 'x') base = 16;
		else if (*ptr == 'o') base = 8;
		else if (*ptr == 'b') base = 2;
		else
		{
			hcl_seterrbfmt (hcl, HCL_EINVAL, "invalid radix specifier - %c", *ptr);
			return -1;
		}
		ptr++;
	}
	else base = 10;

	HCL_ASSERT (hcl, ptr < end);

	value = old_value = 0;
	while (ptr < end && (v = HCL_CHAR_TO_NUM(*ptr, base)) < base)
	{
		value = value * base + v;
		if (value < old_value)
		{
			/* overflow must have occurred */
			hcl_seterrbfmt (hcl, HCL_ERANGE, "number too big - %.*js", str->len, str->ptr);
			return -1;
		}
		old_value = value;
		ptr++;
	}

	if (ptr < end)
	{
		/* trailing garbage? */
		hcl_seterrbfmt (hcl, HCL_EINVAL, "trailing garbage after numeric literal - %.*js", str->len, str->ptr);
		return -1;
	}

	if (value > HCL_TYPE_MAX(hcl_ooi_t) + (negsign? 1: 0)) /* assume 2's complement */
	{
		hcl_seterrbfmt (hcl, HCL_ERANGE, "number too big - %.*js", str->len, str->ptr);
		return -1;
	}

	*num = value;
	if (negsign) *num *= -1;

	return 0;
}

static hcl_oop_t string_to_num (hcl_t* hcl, hcl_oocs_t* str, const hcl_loc_t* loc, int radixed)
{
	int negsign, base;
	const hcl_ooch_t* ptr, * end;

	negsign = 0;
	ptr = str->ptr,
	end = str->ptr + str->len;

	HCL_ASSERT (hcl, ptr < end);

	if (*ptr == '+' || *ptr == '-')
	{
		negsign = *ptr - '+';
		ptr++;
	}

#if 0
	if (radixed)
	{
		/* 16r1234, 2r1111 */
		HCL_ASSERT (hcl, ptr < end);

		base = 0;
		do
		{
			base = base * 10 + HCL_CHAR_TO_NUM(*ptr, 10);
			ptr++;
		}
		while (*ptr != 'r');

		ptr++;
	}
	else base = 10;
#else
	if (radixed)
	{
		/* #xFF80, #b1111 */
		HCL_ASSERT (hcl, ptr < end);

		if (*ptr != '#')
		{
			hcl_setsynerrbfmt(hcl, HCL_SYNERR_RADIX, loc, str, "radixed number not starting with #");
			return HCL_NULL;
		}
		ptr++; /* skip '#' */

		if (*ptr == 'x') base = 16;
		else if (*ptr == 'o') base = 8;
		else if (*ptr == 'b') base = 2;
		else
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_RADIX, loc, str, "invalid radix specifier %c", *ptr);
			return HCL_NULL;
		}
		ptr++;
	}
	else base = 10;
#endif

/* TODO: handle floating point numbers ... etc */
	if (negsign) base = -base;
	return hcl_strtoint(hcl, ptr, end - ptr, base);
}

static hcl_oop_t string_to_fpdec (hcl_t* hcl, hcl_oocs_t* str, const hcl_loc_t* loc)
{
	hcl_oow_t pos;
	hcl_oow_t scale = 0;
	hcl_oop_t v;

	pos = str->len;
	while (pos > 0)
	{
		pos--;
		if (str->ptr[pos] == '.')
		{
			scale = str->len - pos - 1;
			if (scale > HCL_SMOOI_MAX)
			{
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_NUMRANGE, loc, str, "too many digits after decimal point");
				return HCL_NULL;
			}

			HCL_ASSERT (hcl, scale > 0);
			/*if (scale > 0)*/ HCL_MEMMOVE (&str->ptr[pos], &str->ptr[pos + 1], scale * HCL_SIZEOF(str->ptr[0])); /* remove the decimal point */
			break;
		}
	}

	/* if no decimal point is included or no digit after the point , you must not call this function */
	HCL_ASSERT (hcl, scale > 0);

	v = hcl_strtoint(hcl, str->ptr, str->len - 1, 10);
	if (HCL_UNLIKELY(!v)) return HCL_NULL;

	return hcl_makefpdec(hcl, v, scale);
}

static int compile_symbol_literal (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_cnode_t* oprnd;
	hcl_oop_t lit;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_SYMBOL_LITERAL);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	oprnd = cf->operand;
	HCL_ASSERT (hcl, HCL_CNODE_GET_TYPE(oprnd) == HCL_CNODE_SYMBOL);

	/* treat a symbol as a string */
	/* TODO: do i need to create a symbol literal like smalltalk? */
	lit = hcl_makesymbol(hcl, HCL_CNODE_GET_TOKPTR(oprnd), HCL_CNODE_GET_TOKLEN(oprnd));
	if (HCL_UNLIKELY(!lit)) return -1;

	if (emit_push_literal(hcl, lit, HCL_CNODE_GET_LOC(oprnd)) <= -1) return -1;
	POP_CFRAME (hcl);
	return 0;
}

static int compile_object (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_cnode_t* oprnd;
	hcl_oop_t lit;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_OBJECT);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	oprnd = cf->operand;
redo:
	switch (HCL_CNODE_GET_TYPE(oprnd))
	{
		case HCL_CNODE_NIL:
			if (emit_byte_instruction(hcl, HCL_CODE_PUSH_NIL, HCL_CNODE_GET_LOC(oprnd)) <= -1) return -1;
			goto done;

		case HCL_CNODE_TRUE:
			if (emit_byte_instruction(hcl, HCL_CODE_PUSH_TRUE, HCL_CNODE_GET_LOC(oprnd)) <= -1) return -1;
			goto done;

		case HCL_CNODE_FALSE:
			if (emit_byte_instruction(hcl, HCL_CODE_PUSH_FALSE, HCL_CNODE_GET_LOC(oprnd)) <= -1) return -1;
			goto done;

		case HCL_CNODE_SELF:
		case HCL_CNODE_SUPER:
			/* if super is not sent a message, super represents the receiver just like self does */
/* TODO: SELF and SUPER must be limited to methods or is it ok if it just pushes the fake receiver in a normal function call?? */
			if (emit_byte_instruction(hcl, HCL_CODE_PUSH_RECEIVER, HCL_CNODE_GET_LOC(oprnd)) <= -1) return -1;
			goto done;

		/* TODO: this-context */

		case HCL_CNODE_CHARLIT:
			lit = HCL_CHAR_TO_OOP(oprnd->u.charlit.v);
			goto literal;

		case HCL_CNODE_BCHRLIT: /* byte character still converts to charcter */
			lit = HCL_CHAR_TO_OOP((hcl_ooch_t)oprnd->u.bchrlit.v);
			goto literal;

		case HCL_CNODE_STRLIT:
			lit = hcl_makestring(hcl, HCL_CNODE_GET_TOKPTR(oprnd), HCL_CNODE_GET_TOKLEN(oprnd), 0);
			if (HCL_UNLIKELY(!lit)) return -1;
			goto literal;

		case HCL_CNODE_BSTRLIT:
			lit = hcl_makebytestring(hcl, HCL_CNODE_GET_TOKPTR(oprnd), HCL_CNODE_GET_TOKLEN(oprnd), 0);
			if (HCL_UNLIKELY(!lit)) return -1;
			goto literal;

		case HCL_CNODE_NUMLIT:
			lit = string_to_num(hcl, HCL_CNODE_GET_TOK(oprnd), HCL_CNODE_GET_LOC(oprnd), 0);
			if (HCL_UNLIKELY(!lit)) return -1;
			goto literal;

		case HCL_CNODE_RADNUMLIT:
			lit = string_to_num(hcl, HCL_CNODE_GET_TOK(oprnd), HCL_CNODE_GET_LOC(oprnd), 1);
			if (HCL_UNLIKELY(!lit)) return -1;
			goto literal;

		case HCL_CNODE_FPDECLIT:
			lit = string_to_fpdec(hcl, HCL_CNODE_GET_TOK(oprnd), HCL_CNODE_GET_LOC(oprnd));
			if (HCL_UNLIKELY(!lit)) return -1;
			goto literal;

		case HCL_CNODE_SMPTRLIT:
			lit = HCL_SMPTR_TO_OOP(oprnd->u.smptrlit.v);
			goto literal;

		case HCL_CNODE_ERRLIT:
			lit = HCL_ERROR_TO_OOP(oprnd->u.errlit.v);
			goto literal;

		case HCL_CNODE_SYMBOL:
			if (compile_symbol(hcl, oprnd) <= -1) return -1;
			goto done;

		case  HCL_CNODE_DSYMBOL:
			if (compile_dsymbol(hcl, oprnd) <= -1) return -1;
			goto done;

		case HCL_CNODE_CONS:
		{
			switch (HCL_CNODE_CONS_CONCODE(oprnd))
			{
				case HCL_CONCODE_ALIST:
					if (compile_cons_alist_expression(hcl, oprnd) <= -1) return -1;
					break;

				case HCL_CONCODE_XLIST:
					if (compile_cons_xlist_expression(hcl, oprnd, 0) <= -1) return -1;
					break;

				case HCL_CONCODE_MLIST:
					if (compile_cons_mlist_expression(hcl, oprnd, 0) <= -1) return -1;
					break;

				case HCL_CONCODE_BLOCK:
					if (compile_cons_block_expression(hcl, oprnd) <= -1) return -1;
					break;

				case HCL_CONCODE_ARRAY:
					if (compile_cons_array_expression(hcl, oprnd) <= -1) return -1;
					break;

				case HCL_CONCODE_BYTEARRAY:
				case HCL_CONCODE_CHARARRAY:
					if (compile_cons_bytearray_expression(hcl, oprnd, HCL_CNODE_CONS_CONCODE(oprnd)) <= -1) return -1;
					break;

				case HCL_CONCODE_DIC:
					if (compile_cons_dic_expression(hcl, oprnd) <= -1) return -1;
					break;

				case HCL_CONCODE_QLIST:
					if (compile_cons_qlist_expression(hcl, oprnd) <= -1) return -1;
					break;

				case HCL_CONCODE_VLIST:
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARDCLBANNED, HCL_CNODE_GET_LOC(oprnd), HCL_NULL, "variable declaration disallowed");
					return -1;

				case HCL_CONCODE_TUPLE:
					/* [a, b] is only allowed as a lvalue or in class member varialble declaration for now */
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNED, HCL_CNODE_GET_LOC(oprnd), HCL_NULL, "tuple disallowed");
					return -1;

				default:
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_INTERN, HCL_CNODE_GET_LOC(oprnd), HCL_NULL, "internal error - unknown cons type %d", HCL_CNODE_CONS_CONCODE(oprnd));
					return -1;
			}

			break;
		}

		case HCL_CNODE_ELIST:
		{
			/* empty list */
			switch (HCL_CNODE_ELIST_CONCODE(oprnd))
			{
				case HCL_CONCODE_ALIST:
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNED, HCL_CNODE_GET_LOC(oprnd), HCL_NULL, "empty assignment list");
					return -1;

				case HCL_CONCODE_XLIST:
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNED, HCL_CNODE_GET_LOC(oprnd), HCL_NULL, "empty executable list");
					return -1;

				case HCL_CONCODE_MLIST:
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNED, HCL_CNODE_GET_LOC(oprnd), HCL_NULL, "empty message send list");
					return -1;

				case HCL_CONCODE_BLOCK:
					if (compile_cons_block_expression(hcl, oprnd) <= -1) return -1;
					break;

				case HCL_CONCODE_ARRAY:
					if (emit_single_param_instruction(hcl, HCL_CODE_MAKE_ARRAY, 0, HCL_CNODE_GET_LOC(oprnd)) <= -1) return -1;
					goto done;

				case HCL_CONCODE_BYTEARRAY:
					if (emit_single_param_instruction(hcl, HCL_CODE_MAKE_BYTEARRAY, 0, HCL_CNODE_GET_LOC(oprnd)) <= -1) return -1;
					goto done;

				case HCL_CONCODE_DIC:
					if (emit_single_param_instruction(hcl, HCL_CODE_MAKE_DIC, 16, HCL_CNODE_GET_LOC(oprnd)) <= -1) return -1;
					goto done;

				case HCL_CONCODE_QLIST:
					if (emit_byte_instruction(hcl, HCL_CODE_PUSH_NIL, HCL_CNODE_GET_LOC(oprnd)) <= -1) return -1;
					goto done;

				case HCL_CONCODE_VLIST:
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNED, HCL_CNODE_GET_LOC(oprnd), HCL_NULL, "empty variable declaration");
					return -1;

				case HCL_CONCODE_TUPLE:
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNED, HCL_CNODE_GET_LOC(oprnd), HCL_NULL, "empty tuple");
					return -1;

				default:
					hcl_setsynerrbfmt (hcl, HCL_SYNERR_INTERN, HCL_CNODE_GET_LOC(oprnd), HCL_NULL, "internal error - unknown list type %d", HCL_CNODE_CONS_CONCODE(oprnd));
					return -1;
			}

			break;
		}

		case HCL_CNODE_SHELL:
			/* a shell node is just a wrapper of an actual node */
			oprnd = oprnd->u.shell.obj;
			goto redo;

		case HCL_CNODE_ELLIPSIS:
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_ELLIPSISBANNED, HCL_CNODE_GET_LOC(oprnd), HCL_CNODE_GET_TOK(oprnd), "ellipsis disallowed in this context", HCL_CNODE_GET_TYPE(oprnd));
			return -1;

		case HCL_CNODE_TRPCOLONS:
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_TRPCOLONSBANNED, HCL_CNODE_GET_LOC(oprnd), HCL_CNODE_GET_TOK(oprnd), "triple colons disallowed in this context", HCL_CNODE_GET_TYPE(oprnd));
			return -1;

		case HCL_CNODE_DBLCOLONS:
		case HCL_CNODE_COLON:
		case HCL_CNODE_COLONLT:
		case HCL_CNODE_COLONGT:
		case HCL_CNODE_COLONSTAR:
		default:
			/*
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_INTERN, HCL_CNODE_GET_LOC(oprnd), HCL_CNODE_GET_TOK(oprnd), "internal error - unexpected object type %d", HCL_CNODE_GET_TYPE(oprnd));
			*/
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNED, HCL_CNODE_GET_LOC(oprnd), HCL_CNODE_GET_TOK(oprnd), "prohibited in this context", HCL_CNODE_GET_TYPE(oprnd));
			return -1;
	}

	/* the control reaches here in case a compile_xxxx() functionse.g. compile_cons_xlist_expression) is called.
	 * such a function removes the top cframe. so POP_CFRAME() needs not be called here */
	return 0;

literal:
	if (emit_push_literal(hcl, lit, HCL_CNODE_GET_LOC(oprnd)) <= -1) return -1;

done:
	POP_CFRAME (hcl);
	return 0;
}

static int compile_object_r (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_cnode_t* oprnd;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_OBJECT_R);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	oprnd = cf->operand;
	if (HCL_CNODE_IS_CONS_CONCODED(oprnd, HCL_CONCODE_XLIST))
	{
		return compile_cons_xlist_expression(hcl, oprnd, cf->u.obj_r.nrets);
	}
	else if (HCL_CNODE_IS_CONS_CONCODED(oprnd, HCL_CONCODE_MLIST))
	{
		return compile_cons_mlist_expression(hcl, oprnd, cf->u.obj_r.nrets);
	}

	/*
	else if (HCL_CNODE_IS_CONS_CONCODED(oprnd, HCL_CONCODE_ALIST))
	{
		ALIST is transformed to XLIST with or set or set-r by the reader.
		so it must not appear here..
	}
	*/

	hcl_setsynerrbfmt (hcl, HCL_SYNERR_BANNED, HCL_CNODE_GET_LOC(oprnd), HCL_NULL, "non-function call/non-message send disallowed");
	return -1;
}

static int compile_object_list (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_cnode_t* oprnd;
	int cop;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_ARGUMENT_LIST ||
	                 cf->opcode == COP_COMPILE_OBJECT_LIST ||
	                 cf->opcode == COP_COMPILE_OBJECT_LIST_TAIL ||
	                 cf->opcode == COP_COMPILE_IF_OBJECT_LIST ||
	                 cf->opcode == COP_COMPILE_IF_OBJECT_LIST_TAIL ||
	                 cf->opcode == COP_COMPILE_TRY_OBJECT_LIST ||
	                 cf->opcode == COP_COMPILE_TRY_OBJECT_LIST_TAIL);

	cop = cf->opcode;
	oprnd = cf->operand;

	if (!oprnd)
	{
		POP_CFRAME (hcl);
	}
	else
	{
		hcl_cnode_t* car, * cdr;

#if 0
		if (cop != COP_COMPILE_ARGUMENT_LIST)
		{
			/* eliminate unnecessary non-function calls. keep the last one */
			while (HCL_CNODE_IS_CONS(oprnd))
			{
				cdr = HCL_CNODE_CONS_CDR(oprnd);
				if (!cdr) break; /* keep the last one */

				if (HCL_CNODE_IS_CONS(cdr))
				{
					/* look ahead */
					/* keep the last one before elif or else... */
					car = HCL_CNODE_CONS_CAR(cdr);
					if (HCL_CNODE_IS_SYMBOL(car) && HCL_CNODE_SYMBOL_SYNCODE(car)) break;
				}

				car = HCL_CNODE_CONS_CAR(oprnd);
/* this optimization is buggy for now... need to perfect the break condition here */
				if (HCL_CNODE_IS_CONS(car) || (HCL_CNODE_IS_SYMBOL(car) && HCL_CNODE_SYMBOL_SYNCODE(car)) || HCL_CNODE_IS_ELLIPSIS(car) || HCL_CNODE_IS_DBLCOLONS(car)) break;
				oprnd = cdr;
			}

			HCL_ASSERT (hcl, oprnd != HCL_NULL);
		}
#endif
		if (!HCL_CNODE_IS_CONS(oprnd))
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(oprnd), HCL_CNODE_GET_TOK(oprnd), "redundant cdr in the object list");
			return -1;
		}

		car = HCL_CNODE_CONS_CAR(oprnd);
		cdr = HCL_CNODE_CONS_CDR(oprnd);

		if (cop == COP_COMPILE_IF_OBJECT_LIST || cop == COP_COMPILE_IF_OBJECT_LIST_TAIL)
		{
			if (HCL_CNODE_IS_TYPED(car, HCL_CNODE_ELIF))
			{
				SWITCH_TOP_CFRAME (hcl, COP_COMPILE_ELIF, oprnd);
				goto done;
			}
			else if (HCL_CNODE_IS_TYPED(car, HCL_CNODE_ELSE))
			{
				SWITCH_TOP_CFRAME (hcl, COP_COMPILE_ELSE, oprnd);
				goto done;
			}
			else if (HCL_CNODE_IS_SYMBOL_SYNCODED(car, HCL_SYNCODE_ELIF))
			{
				SWITCH_TOP_CFRAME (hcl, COP_COMPILE_ELIF, oprnd);
				goto done;
			}
			else if (HCL_CNODE_IS_SYMBOL_SYNCODED(car, HCL_SYNCODE_ELSE))
			{
				SWITCH_TOP_CFRAME (hcl, COP_COMPILE_ELSE, oprnd);
				goto done;
			}
		}
		else if (cop == COP_COMPILE_TRY_OBJECT_LIST || cop == COP_COMPILE_TRY_OBJECT_LIST_TAIL)
		{
			if (HCL_CNODE_IS_SYMBOL_SYNCODED(car, HCL_SYNCODE_CATCH) || HCL_CNODE_IS_TYPED(car, HCL_CNODE_CATCH))
			{
				SWITCH_TOP_CFRAME (hcl, COP_COMPILE_CATCH, oprnd);
				goto done;
			}
		}

		SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, car);

		if (cdr)
		{
			/* there is a next statement to compile
			 *
			 * (+ 1 2 3) - argument list. 1, 2, 3 pushed must remain in
			 *             the stack until the function '+' is called.
			 *
			 * (fun (x y) (+ x 10) (+ y 20))
			 *    - the result of (+ x 10) should be popped before (+ y 20)
			 *      is executed
			 *
			 * for the latter, inject POP_STACKTOP after each object evaluation
			 * except the last.
			 */
			int nextcop;
			nextcop = (cop == COP_COMPILE_OBJECT_LIST)?     COP_COMPILE_OBJECT_LIST_TAIL:
			          (cop == COP_COMPILE_IF_OBJECT_LIST)?  COP_COMPILE_IF_OBJECT_LIST_TAIL:
			          (cop == COP_COMPILE_TRY_OBJECT_LIST)? COP_COMPILE_TRY_OBJECT_LIST_TAIL: cop;
			PUSH_SUBCFRAME (hcl, nextcop, cdr);
		}

		if (cop == COP_COMPILE_OBJECT_LIST_TAIL ||
		    cop == COP_COMPILE_IF_OBJECT_LIST_TAIL ||
		    cop == COP_COMPILE_TRY_OBJECT_LIST_TAIL)
		{
			/* emit POP_STACKTOP before evaluating the second objects
			 * and onwards. this goes above COP_COMPILE_OBJECT.*/

			/* TODO: if the previous operators is known to divert execution flow, it may skip this.
			 *       for instance, some 'RETURN' or 'JUMP' operators */
			PUSH_CFRAME (hcl, COP_EMIT_POP_STACKTOP, oprnd);
		}
	}

done:
	return 0;
}

static int compile_array_list (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_cnode_t* oprnd;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_ARRAY_LIST);

	oprnd = cf->operand;

	if (!oprnd)
	{
		POP_CFRAME (hcl);
	}
	else
	{
		hcl_cnode_t* car, * cdr;
		hcl_ooi_t oldidx;

		if (!HCL_CNODE_IS_CONS(oprnd))
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(oprnd), HCL_CNODE_GET_TOK(oprnd), "redundant cdr in the array list");
			return -1;
		}

		car = HCL_CNODE_CONS_CAR(oprnd);
		cdr = HCL_CNODE_CONS_CDR(oprnd);

		oldidx = cf->u.array_list.index;

		SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, car);
		if (cdr)
		{
			PUSH_SUBCFRAME (hcl, COP_COMPILE_ARRAY_LIST, cdr);
			cf = GET_SUBCFRAME(hcl);
			cf->u.array_list.index = oldidx + 1;
		}

		PUSH_SUBCFRAME (hcl, COP_EMIT_POP_INTO_ARRAY, car);
		cf = GET_SUBCFRAME(hcl);
		cf->u.array_list.index = oldidx;
	}

	return 0;
}

static int compile_bytearray_list (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_cnode_t* oprnd;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_BYTEARRAY_LIST);

	oprnd = cf->operand;

	if (!oprnd)
	{
		POP_CFRAME (hcl);
	}
	else
	{
		hcl_cnode_t* car, * cdr;
		hcl_ooi_t oldidx;
		int elem_type;

		if (!HCL_CNODE_IS_CONS(oprnd))
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(oprnd), HCL_CNODE_GET_TOK(oprnd), "redundant cdr in the byte-array list");
			return -1;
		}

		car = HCL_CNODE_CONS_CAR(oprnd);
		cdr = HCL_CNODE_CONS_CDR(oprnd);

		oldidx = cf->u.bytearray_list.index;
		elem_type = cf->u.bytearray_list.index;

/* TODO: compile type check if the data element is literal...
	 runtime check if the data is a variable or something... */

		SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, car);
		if (cdr)
		{
			PUSH_SUBCFRAME (hcl, COP_COMPILE_BYTEARRAY_LIST, cdr);
			cf = GET_SUBCFRAME(hcl);
			cf->u.bytearray_list.elem_type = elem_type;
			cf->u.bytearray_list.index = oldidx + 1;
		}

		PUSH_SUBCFRAME (hcl, COP_EMIT_POP_INTO_BYTEARRAY, car);
		cf = GET_SUBCFRAME(hcl);
		cf->u.bytearray_list.elem_type = elem_type;
		cf->u.bytearray_list.index = oldidx;
	}

	return 0;
}


static int compile_dic_list (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_cnode_t* oprnd;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_DIC_LIST);

	oprnd = cf->operand;

	if (!oprnd)
	{
		POP_CFRAME (hcl);
	}
	else
	{
		hcl_cnode_t* car, * cdr, * cadr, * cddr;

		if (!HCL_CNODE_IS_CONS(oprnd))
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_DOTBANNED, HCL_CNODE_GET_LOC(oprnd), HCL_CNODE_GET_TOK(oprnd), "redundant cdr in the dictionary list");
			return -1;
		}

		car = HCL_CNODE_CONS_CAR(oprnd);
		cdr = HCL_CNODE_CONS_CDR(oprnd);

		SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, car);
		if (!cdr)
		{
			hcl_setsynerrbfmt (hcl, HCL_SYNERR_UNBALKV, HCL_CNODE_GET_LOC(car), HCL_NULL, "no value for key %.*js", HCL_CNODE_GET_TOKLEN(car), HCL_CNODE_GET_TOKPTR(car));
			return -1;
		}

		cadr = HCL_CNODE_CONS_CAR(cdr);
		cddr = HCL_CNODE_CONS_CDR(cdr);

		if (cddr)
		{
			PUSH_SUBCFRAME (hcl, COP_COMPILE_DIC_LIST, cddr);
		}

		PUSH_SUBCFRAME (hcl, COP_EMIT_POP_INTO_DIC, cdr);
		PUSH_SUBCFRAME(hcl, COP_COMPILE_OBJECT, cadr);
	}

	return 0;
}

static int compile_qlist (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_cnode_t* oprnd;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_COMPILE_QLIST);

	oprnd = cf->operand;

	if (!oprnd)
	{
		POP_CFRAME (hcl);
	}
	else
	{
		if (!HCL_CNODE_IS_CONS(oprnd))
		{
			/* the last element after . */
			SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, oprnd);
			PUSH_SUBCFRAME (hcl, COP_EMIT_POP_INTO_CONS_CDR, oprnd);
		}
		else
		{
			hcl_cnode_t* car, * cdr;

			car = HCL_CNODE_CONS_CAR(oprnd);
			cdr = HCL_CNODE_CONS_CDR(oprnd);

			SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT, car); /* 1 */
			if (cdr)
			{
				PUSH_SUBCFRAME (hcl, COP_COMPILE_QLIST, cdr); /* 3 */
				PUSH_SUBCFRAME (hcl, COP_EMIT_POP_INTO_CONS, car); /* 2 */
			}
			else
			{
				/* the last element */
				PUSH_SUBCFRAME (hcl, COP_EMIT_POP_INTO_CONS_END, car); /* 2 */
			}
		}
	}

	return 0;
}

/* ========================================================================= */

static HCL_INLINE int post_if_cond (hcl_t* hcl)
{
	hcl_cframe_t* cf, * cf2;
	hcl_ooi_t jump_inst_pos;
	hcl_ooi_t body_pos;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_POST_IF_COND);
	/* cf->operand can be HCL_NULL in these expressions
	 *   (if true)
	 *   (if false)
	 *   (if true 20 elif false)
	 */
	/*HCL_ASSERT (hcl, cf->operand != HCL_NULL);*/

	HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX);
	jump_inst_pos = hcl->code.bc.len;

	if (emit_single_param_instruction(hcl, HCL_CODE_JUMP_FORWARD_IF_FALSE, MAX_CODE_JUMP, &cf->u.post_if.start_loc) <= -1) return -1;

	/* to drop the result of the conditional when it is true */
	if (emit_byte_instruction(hcl, HCL_CODE_POP_STACKTOP, &cf->u.post_if.start_loc) <= -1) return -1;

	HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX);
	body_pos = hcl->code.bc.len;

	if (check_block_expression_as_body(hcl, cf->operand, cf->u.post_if.cmd_cnode, FOR_IF) <= -1) return -1;

	SWITCH_TOP_CFRAME (hcl, COP_COMPILE_IF_OBJECT_LIST, cf->operand); /* 1 */
	PUSH_SUBCFRAME (hcl, COP_POST_IF_BODY, cf->operand); /* 2 */
	cf2 = GET_SUBCFRAME(hcl);
	cf2->u.post_if.body_pos = body_pos;
	cf2->u.post_if.jump_inst_pos = jump_inst_pos;
	cf2->u.post_if.start_loc = cf->u.post_if.start_loc;
	cf2->u.post_if.cmd_cnode = cf->u.post_if.cmd_cnode;
	return 0;
}

static HCL_INLINE int post_if_body (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_ooi_t jip;
	hcl_oow_t jump_offset;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_POST_IF_BODY);

	/* cf->operand can be HCL_NULL in these expressions
	 *   (if true)
	 *   (if false)
	 *   (if true 20 elif false)
	 */
	/*HCL_ASSERT (hcl, cf->operand != HCL_NULL);*/

	jip = cf->u.post_if.jump_inst_pos;

	if (hcl->code.bc.len <= cf->u.post_if.body_pos)
	{
		/* if body is empty */
		if (emit_byte_instruction(hcl, HCL_CODE_PUSH_NIL, &cf->u.post_if.start_loc) <= -1) return -1;
	}

	/* HCL_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD_IF_FALSE instruction */
	jump_offset = hcl->code.bc.len - jip - (HCL_CODE_LONG_PARAM_SIZE + 1);

	if (jump_offset > MAX_CODE_JUMP * 2)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_IFFLOOD, &cf->u.post_if.start_loc, HCL_NULL, "code too big - size %zu", jump_offset);
		return -1;
	}
	patch_long_jump (hcl, jip, jump_offset);

	POP_CFRAME (hcl);
	return 0;
}

/* ========================================================================= */
static HCL_INLINE int post_while_cond (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_ooi_t jump_inst_pos;
	hcl_ooi_t cond_pos, body_pos;
	hcl_loc_t start_loc;
	int jump_inst, next_cop;
	hcl_cnode_t* cond, * body;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_POST_UNTIL_COND || cf->opcode == COP_POST_WHILE_COND);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	/* the caller must pass the cons cell branching to the conditinal and the body
	 * if the body cell is given, things get complicated because the body part can be HCL_NULL.
	 * for instance, the body part is empty in (while (< i 1)  ) */
	cond = HCL_CNODE_CONS_CAR(cf->operand);
	body = HCL_CNODE_CONS_CDR(cf->operand);

	cond_pos = cf->u.post_while.cond_pos;
	start_loc = cf->u.post_while.start_loc;
	HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX);
	jump_inst_pos = hcl->code.bc.len;

	if (cf->opcode == COP_POST_UNTIL_COND)
	{
		jump_inst = HCL_CODE_JUMP_FORWARD_IF_TRUE;
		next_cop = COP_POST_UNTIL_BODY;
	}
	else
	{
		jump_inst = HCL_CODE_JUMP_FORWARD_IF_FALSE;
		next_cop = COP_POST_WHILE_BODY;
	}

	if (emit_single_param_instruction (hcl, jump_inst, MAX_CODE_JUMP, HCL_CNODE_GET_LOC(cond)) <= -1) return -1;
	if (emit_byte_instruction(hcl, HCL_CODE_POP_STACKTOP, HCL_CNODE_GET_LOC(cond)) <= -1) return -1;

	HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX);
	body_pos = hcl->code.bc.len;

	if (body)
	{
		SWITCH_TOP_CFRAME (hcl, COP_COMPILE_OBJECT_LIST, body); /* 1 */
		PUSH_SUBCFRAME (hcl, next_cop, cf->operand); /* 2 */
		cf = GET_SUBCFRAME(hcl);
	}
	else
	{
		/* the body is empty */
		SWITCH_TOP_CFRAME (hcl, next_cop, cond); /* 2 */
		cf = GET_TOP_CFRAME(hcl);
	}
	cf->u.post_while.cond_pos = cond_pos;
	cf->u.post_while.body_pos = body_pos;
	cf->u.post_while.jump_inst_pos = jump_inst_pos;
	cf->u.post_while.start_loc = start_loc;

	return 0;
}

static HCL_INLINE int post_while_body (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_ooi_t jip;
	hcl_ooi_t jump_offset;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_POST_UNTIL_BODY || cf->opcode == COP_POST_WHILE_BODY);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	HCL_ASSERT (hcl, hcl->code.bc.len >= cf->u.post_while.cond_pos);
	if (hcl->code.bc.len > cf->u.post_while.body_pos)
	{
		/* some code exist after POP_STACKTOP after JUMP_FORWARD_IF_TRUE/FALSE.
		 * (until #f) =>
		 *   push_false
		 *   jump_forward_if_true XXXX
		 *   pop_stacktop            <-- 1) emitted in post_while_cond();
		 *   jump_backward YYYY      <-- 2) emitted below
		 *   pop_stacktop
		 * this check prevents another pop_stacktop between 1) and 2)
		 */
		if (emit_byte_instruction (hcl, HCL_CODE_POP_STACKTOP, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;
	}

	HCL_ASSERT (hcl, hcl->code.bc.len < HCL_SMOOI_MAX);
	jump_offset = hcl->code.bc.len - cf->u.post_while.cond_pos + 1;
	if (jump_offset > 3) jump_offset += HCL_CODE_LONG_PARAM_SIZE;
	if (emit_single_param_instruction (hcl, HCL_CODE_JUMP_BACKWARD_0, jump_offset, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	jip = cf->u.post_while.jump_inst_pos;
	/* HCL_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD_IF_FALSE/JUMP_FORWARD_IF_TRUE instruction */
	jump_offset = hcl->code.bc.len - jip - (HCL_CODE_LONG_PARAM_SIZE + 1);
	if (jump_offset > MAX_CODE_JUMP * 2)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_BLKFLOOD, &cf->u.post_while.start_loc, HCL_NULL, "code too big - size %zu", jump_offset);
		return -1;
	}
	patch_long_jump (hcl, jip, jump_offset);

	POP_CFRAME (hcl);

	HCL_ASSERT (hcl, hcl->c->cblk.info[hcl->c->cblk.depth]._type == HCL_CBLK_TYPE_LOOP);
	pop_cblk (hcl);

	return 0;
}


/* ========================================================================= */

static HCL_INLINE int emit_call (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_CALL);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	if (cf->u.call.nrets > 0)
	{
		n = emit_double_param_instruction(hcl, HCL_CODE_CALL_R, cf->u.call.index, cf->u.call.nrets, HCL_CNODE_GET_LOC(cf->operand));
	}
	else
	{
		n = emit_single_param_instruction(hcl, HCL_CODE_CALL_0, cf->u.call.index, HCL_CNODE_GET_LOC(cf->operand));
	}

	POP_CFRAME (hcl);
	return n;
}

static HCL_INLINE int emit_push_nil (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_PUSH_NIL);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	n = emit_byte_instruction(hcl, HCL_CODE_PUSH_NIL, HCL_CNODE_GET_LOC(cf->operand));
	POP_CFRAME (hcl);
	return n;
}

static HCL_INLINE int emit_push_symbol (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_oop_t lit;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_PUSH_SYMBOL);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	lit = hcl_makesymbol(hcl, HCL_CNODE_GET_TOKPTR(cf->operand), HCL_CNODE_GET_TOKLEN(cf->operand));
	if (HCL_UNLIKELY(!lit)) return -1;
	if (emit_push_literal(hcl, lit, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	POP_CFRAME (hcl);
	return 0;
}

static HCL_INLINE int emit_send (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_SEND);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	if (cf->u.sendmsg.nrets > 0)
	{
		n = emit_double_param_instruction(hcl, (cf->u.sendmsg.to_super? HCL_CODE_SEND_TO_SUPER_R: HCL_CODE_SEND_R), cf->u.sendmsg.nargs, cf->u.sendmsg.nrets, HCL_CNODE_GET_LOC(cf->operand));
	}
	else
	{
		n = emit_single_param_instruction(hcl, (cf->u.sendmsg.to_super? HCL_CODE_SEND_TO_SUPER_0: HCL_CODE_SEND_0), cf->u.sendmsg.nargs, HCL_CNODE_GET_LOC(cf->operand));
	}

	POP_CFRAME (hcl);
	return n;
}

/* ========================================================================= */

static HCL_INLINE int emit_make_array (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_MAKE_ARRAY);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	n = emit_single_param_instruction(hcl, HCL_CODE_MAKE_ARRAY, cf->u.array_list.index, HCL_CNODE_GET_LOC(cf->operand));

	POP_CFRAME (hcl);
	return n;
}

static HCL_INLINE int emit_make_bytearray (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_MAKE_BYTEARRAY);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	n = emit_single_param_instruction(hcl, HCL_CODE_MAKE_BYTEARRAY, cf->u.bytearray_list.index, HCL_CNODE_GET_LOC(cf->operand));

	POP_CFRAME (hcl);
	return n;
}

static HCL_INLINE int emit_make_dic (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_MAKE_DIC);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	n = emit_single_param_instruction(hcl, HCL_CODE_MAKE_DIC, cf->u.dic_list.index, HCL_CNODE_GET_LOC(cf->operand));

	POP_CFRAME (hcl);
	return n;
}

static HCL_INLINE int emit_make_cons (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_MAKE_CONS);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	n = emit_byte_instruction(hcl, HCL_CODE_MAKE_CONS, HCL_CNODE_GET_LOC(cf->operand));

	POP_CFRAME (hcl);
	return n;
}

static HCL_INLINE int emit_pop_into_array (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_POP_INTO_ARRAY);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	n = emit_single_param_instruction(hcl, HCL_CODE_POP_INTO_ARRAY, cf->u.array_list.index, HCL_CNODE_GET_LOC(cf->operand));

	POP_CFRAME (hcl);
	return n;
}

static HCL_INLINE int emit_pop_into_bytearray (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_POP_INTO_BYTEARRAY);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	n = emit_single_param_instruction(hcl, HCL_CODE_POP_INTO_BYTEARRAY, cf->u.bytearray_list.index, HCL_CNODE_GET_LOC(cf->operand));

	POP_CFRAME (hcl);
	return n;
}

static HCL_INLINE int emit_pop_into_dic (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_POP_INTO_DIC);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	n = emit_byte_instruction(hcl, HCL_CODE_POP_INTO_DIC, HCL_CNODE_GET_LOC(cf->operand));

	POP_CFRAME (hcl);
	return n;
}

static HCL_INLINE int emit_pop_into_cons (hcl_t* hcl, int cmd)
{
	hcl_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_POP_INTO_CONS ||
	                 cf->opcode == COP_EMIT_POP_INTO_CONS_END ||
	                 cf->opcode == COP_EMIT_POP_INTO_CONS_CDR);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	n = emit_byte_instruction (hcl, cmd, HCL_CNODE_GET_LOC(cf->operand));

	POP_CFRAME (hcl);
	return n;
}

/* ========================================================================= */

static HCL_INLINE int emit_fun (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_oow_t block_code_size, lfsize;
	hcl_ooi_t jip;
	hcl_fnblk_info_t* fbi;
	hcl_loc_t* oploc;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_FUN);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	oploc = HCL_CNODE_GET_LOC(cf->operand);
	fbi = &hcl->c->fnblk.info[hcl->c->fnblk.depth];
	jip = cf->u.fun.jump_inst_pos;

	if (hcl->option.trait & HCL_TRAIT_INTERACTIVE)
		lfsize = hcl->code.lit.len - hcl->c->fnblk.info[hcl->c->fnblk.depth].lfbase;

	/* HCL_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD instruction */
	block_code_size = hcl->code.bc.len - jip - (HCL_CODE_LONG_PARAM_SIZE + 1);

	if (fbi->tmpr_nrvars > 0)
	{
		/* this function block defines one or more return variables */
		if (block_code_size > 0)
		{
			if (emit_byte_instruction(hcl, HCL_CODE_POP_STACKTOP, oploc) <= -1) return -1;
			block_code_size++;
		}
		if (emit_byte_instruction(hcl, HCL_CODE_PUSH_RETURN_R, oploc) <= -1) return -1;
		block_code_size++;
	}
	else
	{
		/* single return value */
		if ((cf->u.fun.fun_type & 0xFF) == FUN_PLAIN)
		{
			if (block_code_size == 0)
			{
				/* no body in fun - (fun (a b c)) */
	/* TODO: is this correct??? */
				if (emit_byte_instruction(hcl, HCL_CODE_PUSH_NIL, oploc) <= -1) return -1;
				block_code_size++;
			}
		}
		else
		{
			/* in-class methods */
			if (block_code_size == 1)
			{
				/* simple optimization not to skip emitting POP_STACKTOP */
				HCL_ASSERT (hcl, hcl->code.bc.len > 0);
				if (hcl->code.bc.ptr[hcl->code.bc.len - 1] == HCL_CODE_PUSH_NIL)
				{
					hcl->code.bc.len--;
					block_code_size--;
				}
				else if (hcl->code.bc.ptr[hcl->code.bc.len - 1] == HCL_CODE_PUSH_RECEIVER)
				{
					goto emit_return_from_block;
				}
			}

			if (block_code_size > 0)
			{
				if (emit_byte_instruction(hcl, HCL_CODE_POP_STACKTOP, oploc) <= -1) return -1;
				block_code_size++;
			}

			if (emit_byte_instruction(hcl, HCL_CODE_PUSH_RECEIVER, oploc) <= -1) return -1;
			block_code_size++;
		}

	emit_return_from_block:
		if (emit_byte_instruction(hcl, HCL_CODE_RETURN_FROM_BLOCK, oploc) <= -1) return -1;
		block_code_size++;
	}

	if (block_code_size > MAX_CODE_JUMP * 2)
	{
		hcl_setsynerrbfmt (hcl, HCL_SYNERR_BLKFLOOD, oploc, HCL_NULL, "code too big - size %zu", block_code_size);
		return -1;
	}
	patch_long_jump (hcl, jip, block_code_size);

	if (hcl->option.trait & HCL_TRAIT_INTERACTIVE)
		patch_long_param (hcl, cf->u.fun.lfsize_pos, lfsize);

	POP_CFRAME (hcl);
	return 0;
}

static HCL_INLINE int post_fun (hcl_t* hcl)
{
	hcl_cframe_t* cf;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_POST_FUN);

	/*hcl->c->fnblk.depth--;
	hcl->c->tv.s.len = hcl->c->fnblk.info[hcl->c->fnblk.depth].tmprlen;
	hcl->c->tv.wcount = hcl->c->fnblk.info[hcl->c->fnblk.depth].tmprcnt;*/
	pop_fnblk (hcl);

	if (cf->operand)
	{
		/* (defun x()  ; this x refers to a variable in the outer scope.
		 *     | t1 t2 x |
		 *     (set x 10)  ; this x refers to the local variable.
		 * )
		 *
		 * the block has been exited(blk.depth--) before finding 'x' in the outer scope.
		 */
		hcl_cnode_t* defun_name = cf->operand;
		hcl_cnode_t* class_name = cf->u.fun.class_name;
		hcl_var_info_t vi;
		int x;

		HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL_PLAIN(defun_name));

		if (is_in_class_init_scope(hcl))
		{
			/* method definition */

			if (class_name)
			{
				/* something wrong - this must not happen because the reader must prevent this
				 * but if it happens, it is a syntax error */
				hcl_setsynerrbfmt(hcl, HCL_SYNERR_BANNED, HCL_CNODE_GET_LOC(class_name), HCL_CNODE_GET_TOK(class_name), "class name prohibited");
				return -1;
			}

			x = find_variable_backward_with_token(hcl, defun_name, &vi);
			if (x <= -1) return -1;
			if (x == 0)
			{
				/* arrange to save to the method slot */
				switch (cf->u.fun.fun_type & 0xFF)
				{
					case FUN_CM: /* class method */
						SWITCH_TOP_CFRAME (hcl, COP_EMIT_CLASS_CMSTORE, defun_name);
						break;

					case FUN_CIM: /* class instantiation method */
						SWITCH_TOP_CFRAME (hcl, COP_EMIT_CLASS_CIMSTORE, defun_name);
						break;

					case FUN_IM: /* instance method */
						SWITCH_TOP_CFRAME (hcl, COP_EMIT_CLASS_IMSTORE, defun_name);
						break;

					default:
						/* in the class initialization scope, the type must not be other than the listed above */
						HCL_DEBUG1 (hcl, "Internal error - invalid method type %d\n", cf->u.fun.fun_type & 0xFF);
						hcl_seterrbfmt (hcl, HCL_EINTERN, "internal error - invalid method type %d", cf->u.fun.fun_type & 0xFF);
						return -1;
				}
				cf = GET_TOP_CFRAME(hcl);
			}
			else
			{
				hcl_setsynerrbfmt (hcl, HCL_SYNERR_VARNAMEDUP, HCL_CNODE_GET_LOC(defun_name), HCL_CNODE_GET_TOK(defun_name), "duplicate method name");
				return -1;
			}
		}
		else
		{
			/* the function name must be global or module-wide.(no module implemented yet. so only global) */
		#if 0
			x = find_variable_backward_with_token(hcl, defun_name, &vi);
			if (x <= -1) return -1;
			if (x == 0)
			{
				SWITCH_TOP_CFRAME (hcl, COP_EMIT_SET, defun_name);
				cf = GET_TOP_CFRAME(hcl);
				cf->u.set.vi.type = VAR_NAMED;
			}
			else
			{
				SWITCH_TOP_CFRAME (hcl, COP_EMIT_SET, defun_name);
				cf = GET_TOP_CFRAME(hcl);
				cf->u.set.vi = vi;
			}
			cf->u.set.mode = VAR_ACCESS_STORE;
		#else
			if (class_name)
			{
				/* out-of-class definition */
/* TODO:  - other types of out-of-class definition - CIM_STORE, CM_STORE...  use different marker? */
				hcl_oow_t index;
				hcl_oop_t lit;
				int inst;

				/* treat the class name part as a normal variable.
				 * it can be a global variable like 'String' or a local variable declared */
				if (compile_symbol(hcl, class_name) <= -1) return -1;

				if (emit_byte_instruction(hcl, HCL_CODE_CLASS_LOAD, HCL_CNODE_GET_LOC(class_name)) <= -1) return -1;

				/* the function name is always named */
				lit = hcl_makesymbol(hcl, HCL_CNODE_GET_TOKPTR(defun_name), HCL_CNODE_GET_TOKLEN(defun_name));
				if (HCL_UNLIKELY(!lit)) return -1;
				if (add_literal(hcl, lit, &index) <= -1) return -1;

				switch (cf->u.fun.fun_type & 0xFF)
				{
					case FUN_CM: /* class method */
						inst =  HCL_CODE_CLASS_CMSTORE;
						break;

					case FUN_CIM: /* class instantiation method */
						inst =  HCL_CODE_CLASS_CIMSTORE;
						break;

					case FUN_IM: /* instance method */
						inst =  HCL_CODE_CLASS_IMSTORE;
						break;

					default:
						/* in the class initialization scope, the type must not be other than the listed above */
						HCL_DEBUG1 (hcl, "Internal error - invalid function type %d\n", cf->u.fun.fun_type & 0xFF);
						hcl_seterrbfmt (hcl, HCL_EINTERN, "internal error - invalid function type %d", cf->u.fun.fun_type & 0xFF);
						return -1;
				}

				if (emit_single_param_instruction(hcl, inst, index, HCL_CNODE_GET_LOC(defun_name)) <= -1) return -1;
				if (emit_byte_instruction(hcl, HCL_CODE_CLASS_EXIT, HCL_CNODE_GET_LOC(class_name)) <= -1) return -1;
				POP_CFRAME (hcl);
			}
			else
			{
				/* An explicitly named function is always global */
				SWITCH_TOP_CFRAME (hcl, COP_EMIT_SET, defun_name);
				cf = GET_TOP_CFRAME(hcl);
				cf->u.set.vi.type = VAR_NAMED;
				cf->u.set.mode = VAR_ACCESS_STORE;
			}
		#endif
		}
	}
	else
	{
		POP_CFRAME (hcl);
	}

	return 0;
}

/* ========================================================================= */

static HCL_INLINE int emit_pop_stacktop (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_POP_STACKTOP);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	n = emit_byte_instruction(hcl, HCL_CODE_POP_STACKTOP, HCL_CNODE_GET_LOC(cf->operand));

	POP_CFRAME (hcl);
	return n;
}

static HCL_INLINE int emit_return (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_RETURN);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	n = emit_byte_instruction(hcl, (cf->u._return.from_home? HCL_CODE_RETURN_STACKTOP: HCL_CODE_RETURN_FROM_BLOCK), HCL_CNODE_GET_LOC(cf->operand));

	POP_CFRAME (hcl);
	return n;
}

static HCL_INLINE int emit_set (hcl_t* hcl)
{
	hcl_cframe_t* cf;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_SET);
	HCL_ASSERT (hcl, cf->u.set.mode == VAR_ACCESS_POP || cf->u.set.mode == VAR_ACCESS_STORE);

	if (cf->u.set.vi.type == VAR_NAMED)
	{
		hcl_oow_t index;
		hcl_oop_t cons, sym;

		HCL_ASSERT (hcl, HCL_CNODE_IS_SYMBOL(cf->operand));

		sym = hcl_makesymbol(hcl, HCL_CNODE_GET_TOKPTR(cf->operand), HCL_CNODE_GET_TOKLEN(cf->operand));
		if (HCL_UNLIKELY(!sym)) return -1;

		cons = (hcl_oop_t)hcl_getatsysdic(hcl, sym);
		if (!cons)
		{
			cons = (hcl_oop_t)hcl_putatsysdic(hcl, sym, hcl->_nil);
			if (HCL_UNLIKELY(!cons)) return -1;
		}

		if (add_literal(hcl, cons, &index) <= -1) return -1;
		if (emit_single_param_instruction(hcl, (cf->u.set.mode == VAR_ACCESS_POP? HCL_CODE_POP_INTO_OBJECT_0: HCL_CODE_STORE_INTO_OBJECT_0), index, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;
	}
	else
	{
		HCL_ASSERT (hcl, cf->operand != HCL_NULL);
		if (emit_variable_access(hcl, cf->u.set.mode, &cf->u.set.vi, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;
	}

	POP_CFRAME (hcl);
	return 0;
}

static HCL_INLINE int emit_class_cmstore (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_oop_t lit;
	hcl_oow_t index;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_CLASS_CMSTORE);

	lit = hcl_makesymbol(hcl, HCL_CNODE_GET_TOKPTR(cf->operand), HCL_CNODE_GET_TOKLEN(cf->operand));
	if (HCL_UNLIKELY(!lit)) return -1;

	if (add_literal(hcl, lit, &index) <= -1) return -1;
	if (emit_single_param_instruction(hcl, HCL_CODE_CLASS_CMSTORE, index, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	POP_CFRAME (hcl);
	return 0;
}

static HCL_INLINE int emit_class_cimstore (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_oop_t lit;
	hcl_oow_t index;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_CLASS_CIMSTORE);

	lit = hcl_makesymbol(hcl, HCL_CNODE_GET_TOKPTR(cf->operand), HCL_CNODE_GET_TOKLEN(cf->operand));
	if (HCL_UNLIKELY(!lit)) return -1;

	if (add_literal(hcl, lit, &index) <= -1) return -1;
	if (emit_single_param_instruction(hcl, HCL_CODE_CLASS_CIMSTORE, index, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	POP_CFRAME (hcl);
	return 0;
}

static HCL_INLINE int emit_class_imstore (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	hcl_oop_t lit;
	hcl_oow_t index;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_CLASS_IMSTORE);

	lit = hcl_makesymbol(hcl, HCL_CNODE_GET_TOKPTR(cf->operand), HCL_CNODE_GET_TOKLEN(cf->operand));
	if (HCL_UNLIKELY(!lit)) return -1;

	if (add_literal(hcl, lit, &index) <= -1) return -1;
	if (emit_single_param_instruction(hcl, HCL_CODE_CLASS_IMSTORE, index, HCL_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	POP_CFRAME (hcl);
	return 0;
}

static HCL_INLINE int emit_throw (hcl_t* hcl)
{
	hcl_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hcl);
	HCL_ASSERT (hcl, cf->opcode == COP_EMIT_THROW);
	HCL_ASSERT (hcl, cf->operand != HCL_NULL);

	n = emit_byte_instruction(hcl, HCL_CODE_THROW, HCL_CNODE_GET_LOC(cf->operand));

	POP_CFRAME (hcl);
	return n;
}

/* ========================================================================= */

int hcl_compile (hcl_t* hcl, hcl_cnode_t* obj, int flags)
{
	hcl_oow_t saved_bc_len, saved_lit_len;
	hcl_bitmask_t log_default_type_mask;
	hcl_fnblk_info_t top_fnblk_saved;
	int top_fnblk_pushed_here = 0;

	hcl->c->flags = flags;

	HCL_ASSERT (hcl, hcl->c->fnblk.depth <= 0); /* 0 or 1 fnblk must exist at this phase */
	HCL_ASSERT (hcl, GET_TOP_CFRAME_INDEX(hcl) < 0);

	if (flags & HCL_COMPILE_CLEAR_FNBLK)
	{
		/* if the program is executed in the interactive mode,
		 * each compiled expression is executed immediately.
		 * that is, hcl_compile() is followed by hcl_execute()
		 * immediately.
		 *
		 *   (1) a := 20
		 *   (2) { | b c | b := 20; c := 30 }
		 *   (3) printf "%d\n" a
		 *
		 * in the interactive mode,
		 *  (1) is compiled and executed
		 *  (2) is compiled and executed
		 *  (3) is compiled and executed
		 *
		 * in the non-interactive mode,
		 *  (1) is compiled, (2) is compiled, (3) is compiled
		 *  (1), (2), (3) are executed
		 * fnblk holds information about temporaries seen so far.
		 * (2) has defined two temporary variables. this count
		 * must get carried until (3) has been compiled in the
		 * non-interactive mode. the accumulated count is used
		 * in creating an initial context for execution.
		 *
		 * in the interactive mode, the information doesn't have
		 * to get carried over.
		 */
		while (hcl->c->fnblk.depth >= 0) pop_fnblk (hcl);
		HCL_ASSERT (hcl, hcl->c->fnblk.depth == -1);
		/* it will be recreated below */
	}
	if (flags & HCL_COMPILE_CLEAR_CODE) hcl_clearcode (hcl);

	saved_bc_len = hcl->code.bc.len;
	saved_lit_len = hcl->code.lit.len;

	log_default_type_mask = hcl->log.default_type_mask;
	hcl->log.default_type_mask |= HCL_LOG_COMPILER;

	/*
	 * In the non-INTERACTIVE mode, the literal frame base(lfbase) doesn't matter.
	 * Only the initial function object contains the literal frame.
	 * No other function objects are created. All 'defun/fun' defintions are
	 * translated to block context objects instead.
	 *
	 * In the INTERACTIVE mode, the literal frame base(lfbase) plays a key role.
	 * hcl_compile() is called for the top-level expression and the literal
	 * frame base can be 0. The means it is ok for a top-level code to
	 * reference part of the literal frame reserved for a function.
	 *
	 *  (set b 1)
	 *  (defun set-a(x) (set a x))
	 *  (set a 2)
	 *  (set-a 4)
	 *  (printf "%d\n" a)
	 *
	 * the global literal frame looks like this:
	 *  @0         (b)
	 *  @1         (a)
	 *  @2         (set-a)
	 *  @3         (printf . #<PRIM>)
	 *  @4         "%d\n"
	 *
	 * @1 to @2 will be copied to a function object when defun is executed.
	 * The literal frame of the created function object for set-a looks
	 * like this
	 *  @0         (a)
	 *  @1         (set-a)
	 */

/* TODO: in case i implement all global variables as block arguments at the top level...what should i do? */
	HCL_ASSERT (hcl, hcl->c->cblk.depth == -1);

	if (hcl->c->fnblk.depth <= -1)
	{
		HCL_ASSERT (hcl, hcl->c->fnblk.depth == -1);
		HCL_ASSERT (hcl, hcl->c->tv.s.len == 0);
		HCL_ASSERT (hcl, hcl->c->tv.wcount == 0);

		/* keep a virtual function block for the top-level compilation.
		 * pass HCL_TYPE_MAX(hcl_oow_t) as make_inst_pos because there is
		 * no actual MAKE_BLOCK/MAKE_FUNCTION instruction which otherwise
		 * would be patched in pop_fnblk(). */

		if (push_fnblk(
			hcl, HCL_NULL,
			0, /* tmpr_va */
			0, /* tmpr_nargs */
			0, /* tmpr_nrvars */
			hcl->c->tv.wcount, /* tmpr_nlvars */
			hcl->c->tv.wcount, /* tmpr_count */
			hcl->c->tv.s.len, /* tmpr_len */
			HCL_TYPE_MAX(hcl_oow_t), /* make_inst_pos */
			0, /* lfbase */
			FUN_PLAIN /* fun_type */
		) <= -1) return -1; /* must not goto oops */

		top_fnblk_pushed_here = 1;
	}
	top_fnblk_saved = hcl->c->fnblk.info[0];
	HCL_ASSERT (hcl, hcl->c->fnblk.depth == 0); /* ensure the virtual function block is added */

	PUSH_CFRAME (hcl, COP_COMPILE_OBJECT, obj);

	while (GET_TOP_CFRAME_INDEX(hcl) >= 0)
	{
		hcl_cframe_t* cf;

		cf = GET_TOP_CFRAME(hcl);

/* TODO: tabulate this switch-based dispatch */
		switch (cf->opcode)
		{
			case COP_COMPILE_OBJECT:
				if (compile_object(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_OBJECT_R:
				if (compile_object_r(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_SYMBOL_LITERAL:
				if (compile_symbol_literal(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_ARGUMENT_LIST:
			case COP_COMPILE_OBJECT_LIST:
			case COP_COMPILE_OBJECT_LIST_TAIL:
			case COP_COMPILE_IF_OBJECT_LIST:
			case COP_COMPILE_IF_OBJECT_LIST_TAIL:
			case COP_COMPILE_TRY_OBJECT_LIST:
			case COP_COMPILE_TRY_OBJECT_LIST_TAIL:
				if (compile_object_list(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_ARRAY_LIST:
				if (compile_array_list(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_BYTEARRAY_LIST:
				if (compile_bytearray_list(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_DIC_LIST:
				if (compile_dic_list(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_QLIST:
				if (compile_qlist(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_ELIF:
				if (compile_elif(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_ELSE:
				if (compile_else(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_CATCH:
				if (compile_catch(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_AND_P1:
				if (compile_and_p1(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_AND_P2:
				if (compile_and_p2(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_BREAK_P1:
				if (compile_break_p1(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_CLASS_P1:
				if (compile_class_p1(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_CLASS_P2:
				if (compile_class_p2(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_CLASS_P3:
				if (compile_class_p3(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_DO_P1:
				if (compile_do_p1(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_OR_P1:
				if (compile_or_p1(hcl) <= -1) goto oops;
				break;

			case COP_COMPILE_OR_P2:
				if (compile_or_p2(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_CALL:
				if (emit_call(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_PUSH_NIL:
				if (emit_push_nil(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_PUSH_SYMBOL:
				if (emit_push_symbol(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_SEND:
				if (emit_send(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_MAKE_ARRAY:
				if (emit_make_array(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_MAKE_BYTEARRAY:
				if (emit_make_bytearray(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_MAKE_DIC:
				if (emit_make_dic(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_MAKE_CONS:
				if (emit_make_cons(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_POP_INTO_ARRAY:
				if (emit_pop_into_array(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_POP_INTO_BYTEARRAY:
				if (emit_pop_into_bytearray(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_POP_INTO_DIC:
				if (emit_pop_into_dic(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_POP_INTO_CONS:
				if (emit_pop_into_cons(hcl, HCL_CODE_POP_INTO_CONS) <= -1) goto oops;
				break;

			case COP_EMIT_POP_INTO_CONS_END:
				if (emit_pop_into_cons(hcl, HCL_CODE_POP_INTO_CONS_END) <= -1) goto oops;
				break;

			case COP_EMIT_POP_INTO_CONS_CDR:
				if (emit_pop_into_cons(hcl, HCL_CODE_POP_INTO_CONS_CDR) <= -1) goto oops;
				break;

			case COP_EMIT_FUN:
				if (emit_fun(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_PLUS:
				if (emit_plus(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_POP_STACKTOP:
				if (emit_pop_stacktop(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_RETURN:
				if (emit_return(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_SET:
				if (emit_set(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_CLASS_CMSTORE:
				if (emit_class_cmstore(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_CLASS_CIMSTORE:
				if (emit_class_cimstore(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_CLASS_IMSTORE:
				if (emit_class_imstore(hcl) <= -1) goto oops;
				break;

			case COP_EMIT_THROW:
				if (emit_throw(hcl) <= -1) goto oops;
				break;

			case COP_POST_IF_COND:
				if (post_if_cond(hcl) <= -1) goto oops;
				break;

			case COP_POST_IF_BODY:
				if (post_if_body(hcl) <= -1) goto oops;
				break;

			case COP_POST_UNTIL_BODY:
			case COP_POST_WHILE_BODY:
				if (post_while_body(hcl) <= -1) goto oops;
				break;

			case COP_POST_UNTIL_COND:
			case COP_POST_WHILE_COND:
				if (post_while_cond(hcl) <= -1) goto oops;
				break;

			case COP_POST_TRY:
				if (post_try(hcl) <= -1) goto oops;
				break;
			case COP_POST_CATCH:
				if (post_catch(hcl) <= -1) goto oops;
				break;

			case COP_POST_FUN:
				if (post_fun(hcl) <= -1) goto oops;
				break;

			default:
				HCL_DEBUG1 (hcl, "Internal error - invalid compiler opcode %d\n", cf->opcode);
				hcl_seterrbfmt (hcl, HCL_EINTERN, "internal error - invalid compiler opcode %d", cf->opcode);
				goto oops;
		}
	}

	/* emit the pop instruction to clear the final result */
	if (emit_byte_instruction(hcl, HCL_CODE_POP_STACKTOP, HCL_NULL) <= -1) goto oops;

	HCL_ASSERT (hcl, GET_TOP_CFRAME_INDEX(hcl) < 0);
	HCL_ASSERT (hcl, hcl->c->tv.s.len >= hcl->c->fnblk.info[0].tmprlen);
	HCL_ASSERT (hcl, hcl->c->tv.wcount >= hcl->c->fnblk.info[0].tmprcnt);
	HCL_ASSERT (hcl, hcl->c->cblk.depth == -1); /* no control blocks expected at this point */

	HCL_ASSERT (hcl, hcl->c->fnblk.depth == 0); /* ensure the virtual function block be the only one left */
	hcl->code.ngtmprs = hcl->c->fnblk.info[0].tmprcnt; /* populate the number of global temporary variables */

#if defined(CLEAR_FNBLK_ALWAYS)
	pop_fnblk (hcl);
	HCL_ASSERT (hcl, hcl->c->tv.s.len == 0);
	HCL_ASSERT (hcl, hcl->c->tv.wcount == 0);
#endif

	hcl->log.default_type_mask = log_default_type_mask;
	return 0;

oops:
	POP_ALL_CFRAMES (hcl);

	hcl->log.default_type_mask = log_default_type_mask;

	/* rollback any bytecodes or literals emitted so far */
	hcl->code.bc.len = saved_bc_len;
	hcl->code.lit.len = saved_lit_len;

	while (hcl->c->fnblk.depth > 0) pop_fnblk (hcl);
	HCL_ASSERT (hcl, hcl->c->fnblk.depth == 0);

	if (top_fnblk_pushed_here)
	{
		pop_fnblk (hcl);
		HCL_ASSERT (hcl, hcl->c->fnblk.depth == -1);
		HCL_ASSERT (hcl, hcl->c->tv.s.len == 0);
		HCL_ASSERT (hcl, hcl->c->tv.wcount == 0);
	}
	else
	{
		/*
		{ |a b c| } ## tv.s.len 6, tv.wcount 3
		{ |k a| (set x y z) }
		*
		* at this point when (set a b c) triggers a syntax error
		* tv.s.len is 10 and tv.wcount is 5.
		* it must be restored to 6 and 3 each.
		*/

		/* restore the top level function block as it's first captured in this function */
		clear_fnblk_inners (hcl);
		HCL_ASSERT (hcl, hcl->c->fnblk.depth == 0);
		hcl->c->fnblk.info[0] = top_fnblk_saved;
		hcl->c->tv.s.len = top_fnblk_saved.tmprlen;
		hcl->c->tv.wcount = top_fnblk_saved.tmprcnt;
	}

	return -1;
}
