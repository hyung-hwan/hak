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

/* limit the `do` expression to have not more than 1 expression and
 * no variable declaration if not enclosed in parentheses */
#define LANG_LIMIT_DO

#define FOR_NONE  (0)
#define FOR_IF    (1)
#define FOR_TRY   (2)
#define FOR_CLASS (3)

#define MAX_NIVARS (HAK_SMOOI_MAX < MAX_CODE_PARAM2? HAK_SMOOI_MAX: MAX_CODE_PARAM2)
#define MAX_NCVARS (HAK_SMOOI_MAX < MAX_CODE_PARAM2? HAK_SMOOI_MAX: MAX_CODE_PARAM2)

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
	 * the fun_type field of hak_funblk_info_t.
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


(fun plus(x y)
	(printf "plus %d %d\n" x y)
	(fun minus(x y)
		(printf "minus %d %d\n" x y)
		(- x y)
	)
	(+ x y)
)

(fun dummy(q)
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

int hak_copy_string_to (hak_t* hak, const hak_oocs_t* src, hak_oocs_t* dst, hak_oow_t* dstcapa, int append, hak_ooch_t delim_char)
{
	hak_oow_t len, pos;
	int delim = 0;

	if (append)
	{
		pos = dst->len;
		len = dst->len + src->len;
		if (dst->len > 0 && delim_char != '\0')
		{
			delim = 1;
			len++;
		}
	}
	else
	{
		pos = 0;
		len = src->len;
	}

	if (len >= *dstcapa)
	{
		hak_ooch_t* tmp;
		hak_oow_t capa;

		capa = HAK_ALIGN(len + 1, TV_BUFFER_ALIGN);

		tmp = (hak_ooch_t*)hak_reallocmem(hak, dst->ptr, HAK_SIZEOF(*tmp) * capa);
		if (HAK_UNLIKELY(!tmp))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "failed to grow string buffer - %js", orgmsg);
			return -1;
		}

		dst->ptr = tmp;
		*dstcapa = capa - 1;
	}

	if (delim) dst->ptr[pos++] = delim_char;
	hak_copy_oochars (&dst->ptr[pos], src->ptr, src->len);
	dst->ptr[len] = '\0';
	dst->len = len;
	return 0;
}

static int __find_word_in_string (const hak_oocs_t* haystack, const hak_oocs_t* name, int last, hak_oow_t* xindex)
{
	/* this function is inefficient. but considering the typical number
	 * of arguments and temporary variables, the inefficiency can be
	 * ignored in my opinion. the overhead to maintain the reverse lookup
	 * table from a name to an index should be greater than this simple
	 * inefficient lookup */

	hak_ooch_t* t, * e;
	hak_oow_t index, i, found;

	t = haystack->ptr;
	e = t + haystack->len;
	index = 0;
	found = HAK_TYPE_MAX(hak_oow_t);

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

	if (found != HAK_TYPE_MAX(hak_oow_t))
	{
		if (xindex) *xindex = found;
		return 0; /* found */
	}

	return -1; /* not found */
}

static int add_temporary_variable (hak_t* hak, const hak_cnode_t* var, hak_oow_t dup_check_start, const hak_bch_t* desc, const hak_oocs_t* tgt)
{
	hak_oocs_t s;
	hak_oocs_t* name;
	int x;

	name = HAK_CNODE_GET_TOK(var);

	s.ptr = hak->c->tv.s.ptr + dup_check_start;
	s.len = hak->c->tv.s.len - dup_check_start;
	if (__find_word_in_string(&s, name, 0, HAK_NULL) >= 0)
	{
		if (tgt)
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_VARNAMEDUP, HAK_CNODE_GET_LOC(var), HAK_NULL,
				"duplicate %hs name '%.*js' for '%.*js'",
				desc, name->len, name->ptr, tgt->len, tgt->ptr);
		}
		else
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_VARNAMEDUP, HAK_CNODE_GET_LOC(var), HAK_NULL,
				"duplicate %hs name '%.*js'",
				desc, name->len, name->ptr);
		}
		return -1;
	}
	x = hak_copy_string_to(hak, name, &hak->c->tv.s, &hak->c->tv.capa, 1, ' ');
	if (HAK_LIKELY(x >= 0)) hak->c->tv.wcount++;
	return x;
}

static int kill_temporary_variables (hak_t* hak, hak_oow_t start_wpos, hak_oow_t end_wpos)
{
	/* this function doesn't remove the added temporary variable nor does it lower the word count.
	 * it simply changes a word at the given postion to some garbage characters so that
	 * the variable can't be found in the search */
	hak_oow_t i;

	for (i = start_wpos; i < end_wpos; i++)
	{
		if (hak->c->tv.s.ptr[i] != ' ')
		{
			hak->c->tv.s.ptr[i] = '('; /* HACK!! put a special character which can't form a variable name */
		}
	}
	return 0;
}

static void kill_temporary_variable_at_offset (hak_t* hak, hak_oow_t offset)
{
	/* this is a hacky function. it's better to implement kill_temporary_variables() which uses word positions */
	HAK_ASSERT(hak, offset < hak->c->tv.s.len);
	HAK_ASSERT(hak, hak->c->tv.s.ptr[offset] != ' ');
	hak->c->tv.s.ptr[offset] = '('; /* HACK!! put a special character which can't form a variable name */
}

static int init_class_level_variable_buffer (hak_t* hak, hak_oocsc_t* dst, const hak_ooch_t* ivptr, hak_oow_t ivlen)
{
	hak_ooch_t* tmp;
	hak_oow_t capa = 128;

	if (ivlen > capa) capa = ivlen;

	tmp = (hak_ooch_t*)hak_allocmem(hak, (capa + 1) * HAK_SIZEOF(*dst->ptr));
	if (HAK_UNLIKELY(!tmp)) return -1;

	dst->ptr = tmp;
	dst->len = ivlen;
	dst->capa = capa;

	if (ivlen > 0) hak_copy_oochars_to_oocstr(dst->ptr, dst->capa + 1, ivptr, ivlen);

	return 0;
}

static void fini_class_level_variable_buffer (hak_t* hak, hak_oocsc_t* dst)
{
	if (dst->ptr)
	{
		hak_freemem(hak, dst->ptr);
		dst->ptr = HAK_NULL;
		dst->len = 0;
		dst->capa = 0;
	}
}

static int add_class_level_variable (hak_t* hak, hak_oocsc_t* dst, hak_oocsc_t* altdst, const hak_cnode_t* var, const hak_bch_t* desc)
{
	/* it downcasts hak_oocsc_t* to hak_oocs_t*. take extra care to keep their type defintion
	 * compatible for downcasting in hak-cmn.h */
	hak_oocs_t* name;

	name = HAK_CNODE_GET_TOK(var);
	if (__find_word_in_string((hak_oocs_t*)dst, name, 0, HAK_NULL) >= 0 ||
	   (__find_word_in_string((hak_oocs_t*)altdst, name, 0, HAK_NULL) >= 0))
	{
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_VARNAMEDUP, HAK_CNODE_GET_LOC(var), HAK_NULL,
			"duplicate %hs variable name '%.*js'", desc, name->len, name->ptr);
		return -1;
	}
	return hak_copy_string_to(hak, name, (hak_oocs_t*)dst, &dst->capa, 1, ' ');
}

static int is_in_top_scope (hak_t* hak)
{
	hak_funblk_info_t* fbi;
/*printf (">>> ---- funblk.depth ....%d\n", (int)hak->c->funblk.depth);*/
	if (hak->c->funblk.depth > 0) return 0;
	HAK_ASSERT(hak, hak->c->funblk.depth >= 0);
	fbi = &hak->c->funblk.info[hak->c->funblk.depth];
/*printf ("fbi->clsblk_top....%d\n", (int)fbi->clsblk_top);*/
	return fbi->clsblk_top < 0;
}

static int is_in_top_fun_scope (hak_t* hak)
{
	return hak->c->funblk.depth == 0;
}

static int is_in_class_init_scope (hak_t* hak)
{
	hak_funblk_info_t* fbi;
	HAK_ASSERT(hak, hak->c->funblk.depth >= 0);
	fbi = &hak->c->funblk.info[hak->c->funblk.depth];
	return fbi->clsblk_top >= 0;
}

static int is_in_class_method_scope (hak_t* hak)
{
	hak_oow_t i;

	HAK_ASSERT(hak, hak->c->funblk.depth >= 0);
	for (i = hak->c->funblk.depth + 1; i > 0; )
	{
		hak_funblk_info_t* fbi;

		fbi = &hak->c->funblk.info[--i];

		if (fbi->clsblk_top >= 0)
		{
			if (i >= hak->c->funblk.depth) return 0; /* in class initialization scope */
			return 1; /* in class method scope */
		}
	}

	return 0; /* in plain function scope */
}

static int find_variable_backward_with_word (hak_t* hak, const hak_oocs_t* name, const hak_loc_t* loc, int class_level_only, hak_var_info_t* vi)
{
	hak_oow_t i;

	HAK_ASSERT(hak, hak->c->funblk.depth >= 0);
	HAK_ASSERT(hak, hak->c->funblk.info[hak->c->funblk.depth].tmprlen == hak->c->tv.s.len);

	/* depth begins at -1. so it is the actual index. let looping begin at depth + 1
	 * to avoid an extra exit check without it */
	for (i = hak->c->funblk.depth + 1; i > 0; )
	{
		hak_funblk_info_t* fbi;
		hak_oocs_t haystack;
		hak_oow_t parent_tmprcnt, parent_tmprlen, index;

		fbi = &hak->c->funblk.info[--i];

		if (fbi->clsblk_top >= 0)
		{
			/* this function block has a class defined.
			 * that is, it is in a class defintion.
			 * variable lookup must be limited to the class scope */
			hak_clsblk_info_t* cbi;

		#if 0
			for (j = fbi->clsblk_top + 1; j > fbi->clsblk_base; )
			{
				cbi = &hak->c->clsblk.info[--j];
		#endif
				cbi = &hak->c->clsblk.info[fbi->clsblk_top];

				/* find the name in the instance variables */
				haystack.ptr = cbi->ivars.ptr;
				haystack.len = cbi->ivars.len;

				if (__find_word_in_string(&haystack, name, 1, &index) >= 0)
				{
					hak_oow_t fi;

					if (i >= hak->c->funblk.depth)
					{
						/* instance variables are accessible only in an instance method defintion scope.
						 * it is in class initialization scope */
						hak_setsynerrbfmt(hak, HAK_SYNERR_BANNED, loc, name, "prohibited access to instance variable");
						return -1;
					}

					for (fi = hak->c->funblk.depth + 1; fi > i; ) /* TOOD: review this loop for correctness */
					{
						/* 'i' is the function level that holds the class defintion block. the check must not go past it */
						if ((hak->c->funblk.info[--fi].fun_type & 0xFF) == FUN_CM)
						{
							/* the function where this variable is defined is a class method or an plain function block within a class method*/
							hak_setsynerrbfmt(hak, HAK_SYNERR_BANNED, loc, name, "prohibited access to instance variable in class method context");
							return -1;
						}

						/* instance methods and instantiation methods can access instance variables */
						if ((hak->c->funblk.info[fi].fun_type & 0xFF) != FUN_PLAIN) break;
					}

					vi->type = VAR_INST;
					vi->ctx_offset = 0;
					vi->index_in_ctx = index;
/*
HAK_INFO6(hak, "FOUND INST VAR [%.*js]...[%.*js]................ ===> ctx_offset %d index %d\n",
	haystack.len, haystack.ptr, name->len, name->ptr, (int)(vi->ctx_offset), (int)vi->index_in_ctx);
*/
					return 1;
				}

				/* find the name in the class variables */
				haystack.ptr = cbi->cvars.ptr;
				haystack.len = cbi->cvars.len;
				if (__find_word_in_string(&haystack, name, 1, &index) >= 0)
				{
					/* TODO: VAR_CLASS_CM vs VAR_CLASS_IM, need to know if it's an instance method or a class method */
/* TODO: check if it's in the class variable .... */
					vi->type = (i >= hak->c->funblk.depth? VAR_CLASS_I: VAR_CLASS_IM);
					vi->ctx_offset = 0;
					vi->index_in_ctx = index;
/*
HAK_INFO6(hak, "FOUND CLASS VAR [%.*js]...[%.*js]................ ===> ctx_offset %d index %d\n",
	haystack.len, haystack.ptr, name->len, name->ptr, (int)(vi->ctx_offset), (int)vi->index_in_ctx);
*/
					return 1;
				}
	#if 0
			}

			if (i == hak->c->funblk.depth)
			{
				/* this condition indicates that the current function level contains a class defintion
				 * and this variable is looked up inside the class defintion */
HAK_INFO2(hak, "CLASS NAMED VAR [%.*js]\n", name->len, name->ptr);
				vi->type = VAR_CLASS;//_NAMED; // TODO: create VAR_CLASS_NAMED???
				vi->ctx_offset = 0;
				vi->index_in_ctx = 0;
			}
	#endif

			break; /* stop searching beyond class definition */
		}

		if (class_level_only) continue; /* skip local variable declarations */

		if (HAK_LIKELY(i > 0))
		{
			parent_tmprlen = hak->c->funblk.info[i - 1].tmprlen;
			parent_tmprcnt = hak->c->funblk.info[i - 1].tmprcnt;
		}
		else
		{
			parent_tmprlen = 0;
			parent_tmprcnt = 0;
		}

		/* narrow the search scope to the current block */
		haystack.ptr = &hak->c->tv.s.ptr[parent_tmprlen];
		haystack.len = fbi->tmprlen - parent_tmprlen;

		if (__find_word_in_string(&haystack, name, 1, &index) >= 0)
		{
			/* temporary variables or arguments */
			vi->type = VAR_INDEXED;
			vi->ctx_offset = hak->c->funblk.depth - i; /* context offset */
			vi->index_in_ctx = index;
/*HAK_INFO4(hak, "FOUND ...[%.*js]................ ===> ctx_offset %d index %d\n", name->len, name->ptr, (int)(vi->ctx_offset), (int)vi->index_in_ctx);*/

			if (vi->ctx_offset > 0)
			{
				/* the current function block accesses temporaries in an outer function block */
				hak->c->funblk.info[hak->c->funblk.depth].access_outer = 1;
				/* temporaries in an outer function block is accessed by the current function block */

				if (i > 0) hak->c->funblk.info[i - 1].accessed_by_inner = 1;
			}

			return 1;
		}
	}

/*HAK_INFO2(hak, "NOT FOUND => %.*js\n", name->len, name->ptr); */
	return 0; /* not found */
}

static int find_variable_backward_with_token (hak_t* hak, const hak_cnode_t* cnode, hak_var_info_t* vi)
{
	if (HAK_CNODE_IS_DSYMBOL_CLA(cnode))
	{
		/* prefixed with self or super. remove the first segment */
		hak_oocs_t newtok;
		newtok = *HAK_CNODE_GET_TOK(cnode);
		while (*newtok.ptr != '.')
		{
			newtok.ptr++;
			newtok.len--;
		}
		newtok.ptr++;
		newtok.len--;
		return find_variable_backward_with_word(hak, &newtok, HAK_CNODE_GET_LOC(cnode), 1, vi);
	}

	return find_variable_backward_with_word(hak, HAK_CNODE_GET_TOK(cnode), HAK_CNODE_GET_LOC(cnode), HAK_CNODE_IS_DSYMBOL_CLA(cnode), vi);
}

/* ========================================================================= */

static int check_block_expression_as_body (hak_t* hak, hak_cnode_t* c, const hak_cnode_t* ctx, int for_what)
{
	hak_cnode_t* car = HAK_NULL, * cdr;

	/* variable declaration is disallowed.
	 * a block expression is allowed if not followed by another expression.
	 * unlike the function name, other types of expressions are allowed if not followed by another expression.
	 */
	if (!c || !HAK_CNODE_IS_CONS(c)) goto no_block; /* not cons */

	car = HAK_CNODE_CONS_CAR(c);
	if (!car || (HAK_CNODE_IS_CONS_CONCODED(car, HAK_CONCODE_VLIST) ||
	             HAK_CNODE_IS_ELIST_CONCODED(car, HAK_CONCODE_VLIST)))
	{
	no_block:
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_BLOCK,
			(car? HAK_CNODE_GET_LOC(car): c? HAK_CNODE_GET_LOC(c): HAK_CNODE_GET_LOC(ctx)), HAK_NULL,
			"block expression expected as '%.*js' body", HAK_CNODE_GET_TOKLEN(ctx), HAK_CNODE_GET_TOKPTR(ctx)
		);
		return -1;
	}

	if (for_what == FOR_CLASS)
	{
		/* the class body must be enclosed in { .. }. e.g class X { ... } */
		if (!HAK_CNODE_IS_CONS_CONCODED(car, HAK_CONCODE_BLOCK) &&
		    !HAK_CNODE_IS_ELIST_CONCODED(car, HAK_CONCODE_BLOCK)) goto no_block;
	}

	/* there are special words that can't start a new expression */
	if (HAK_CNODE_IS_TYPED(car, HAK_CNODE_ELIF) ||
	    HAK_CNODE_IS_TYPED(car, HAK_CNODE_ELSE) ||
	    HAK_CNODE_IS_TYPED(car, HAK_CNODE_CATCH))
	{
		goto no_block;
	}

	cdr = HAK_CNODE_CONS_CDR(c);
	if (cdr)
	{
		/* there is redundant expression after the block expression */
		if (HAK_CNODE_IS_CONS(cdr))
		{
			hak_cnode_t* nxt;
			nxt = HAK_CNODE_CONS_CAR(cdr);

			if (for_what == FOR_IF)
			{
				/* after the body for `if` or `elif`, there can come `elif` or `else` */
				if (HAK_CNODE_IS_TYPED(nxt, HAK_CNODE_ELIF) ||
				    HAK_CNODE_IS_TYPED(nxt, HAK_CNODE_ELSE)) goto ok;
			}
			else if (for_what == FOR_TRY)
			{
				if (HAK_CNODE_IS_TYPED(nxt, HAK_CNODE_CATCH)) goto ok;
			}
		}

		hak_setsynerrbfmt(
			hak, HAK_SYNERR_BANNED, HAK_CNODE_GET_LOC(cdr), HAK_NULL,
			"redundant expression prohibited after '%.*js' body",
			HAK_CNODE_GET_TOKLEN(ctx), HAK_CNODE_GET_TOKPTR(ctx)
		);
		return -1;
	}

ok:
	return 0;
}

/* ========================================================================= */

static HAK_INLINE int add_literal (hak_t* hak, hak_oop_t obj, hak_oow_t* index)
{
	hak_oow_t lfbase;
	int n;

	lfbase = (hak->option.trait & HAK_TRAIT_INTERACTIVE)? hak->c->funblk.info[hak->c->funblk.depth].lfbase: 0;
	n = hak_addliteraltocode(hak, &hak->code, obj, lfbase, index);
	if (n <= -1) return -1;
	if (*index >= MAX_CODE_PARAM2)
	{
		/* most literal-related instructions can't handle index higher than MAX_CODE_PARAM2.
		 * MAKE_FUNCTION uses MAX_CODE_PARAM2 as an indicator that no name literal is specified.
		 * To be safe, limit the maximum number of index to MAX_CODE_PARAM2 - 1 */
		hak_seterrbfmt(hak, HAK_EFLOOD, "literal frame full - %O\n", obj);
		return -1;
	}
	return 0;
}

/* ========================================================================= */

static HAK_INLINE void patch_instruction (hak_t* hak, hak_oow_t index, hak_oob_t bc)
{
	HAK_ASSERT(hak, index < hak->code.bc.len);
	hak->code.bc.ptr[index] = bc;
}

static int emit_byte_instruction (hak_t* hak, hak_oob_t bc, const hak_loc_t* srcloc)
{
	/* the context object has the ip field. it should be representable
	 * in a small integer. for simplicity, limit the total byte code length
	 * to fit in a small integer. because 'ip' points to the next instruction
	 * to execute, the upper bound should be (max - 1) so that 'ip' stays
	 * at the max when incremented */
	if (hak->code.bc.len == HAK_SMOOI_MAX - 1)
	{
		hak_seterrnum(hak, HAK_EBCFULL); /* byte code full/too big */
		return -1;
	}

	if (hak->code.bc.len >= hak->code.bc.capa)
	{
		hak_oow_t newcapa;
		hak_oob_t* tmp;
		hak_dbgi_t* tmp2;

		newcapa = HAK_ALIGN(hak->code.bc.capa + 1, HAK_BC_BUFFER_ALIGN);
		tmp = (hak_oob_t*)hak_reallocmem(hak, hak->code.bc.ptr, HAK_SIZEOF(*tmp) * newcapa);
		if (HAK_UNLIKELY(!tmp))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "failed to grow byte code buffer - %js", orgmsg);
			return -1;
		}

		tmp2 = (hak_dbgi_t*)hak_reallocmem(hak, hak->code.dbgi, HAK_SIZEOF(*tmp2) * newcapa);
		if (HAK_UNLIKELY(!tmp2))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "failed to grow debug info buffer - %js", orgmsg);
			hak_freemem(hak, tmp);
			return -1;
		}
		HAK_MEMSET(&tmp2[hak->code.bc.capa], 0, HAK_SIZEOF(*tmp2) * (newcapa - hak->code.bc.capa));

		hak->code.bc.ptr = tmp;
		hak->code.bc.capa = newcapa;
		hak->code.dbgi = tmp2;
	}

	hak->code.bc.ptr[hak->code.bc.len] = bc;

	if (srcloc)
	{
		hak->code.dbgi[hak->code.bc.len].fname = srcloc->file;
		hak->code.dbgi[hak->code.bc.len].sline = srcloc->line;
	}

	hak->code.bc.len++;
	return 0;
}

/*
COMMENTED OUT TEMPORARILY
int hak_emitbyteinstruction (hak_t* hak, hak_oob_t bc)
{
	return emit_byte_instruction(hak, bc, HAK_NULL);
}*/

static int emit_one_param_instruction (hak_t* hak, int cmd, hak_oow_t param_1, const hak_loc_t* srcloc)
{
	hak_oob_t bc;

	switch (cmd)
	{
		case HAK_CODE_PUSH_IVAR_0:
		case HAK_CODE_STORE_INTO_IVAR_0:
		case HAK_CODE_POP_INTO_IVAR_0:
		case HAK_CODE_PUSH_TEMPVAR_0:
		case HAK_CODE_STORE_INTO_TEMPVAR_0:
		case HAK_CODE_POP_INTO_TEMPVAR_0:
			if (param_1 < 8)
			{
				/* low 3 bits to hold the parameter */
				bc = (hak_oob_t)(cmd & 0xF8) | (hak_oob_t)param_1;
				goto write_short;
			}
			else
			{
				/* convert the code to a long version */
				bc = cmd | 0x80;
				goto write_long;
			}

		case HAK_CODE_PUSH_LITERAL_0:
			if (param_1 < 8)
			{
				/* low 3 bits to hold the parameter */
				bc = (hak_oob_t)(cmd & 0xF8) | (hak_oob_t)param_1;
				goto write_short;
			}
			else if (param_1 <= MAX_CODE_PARAM)
			{
				bc = HAK_CODE_PUSH_LITERAL_X; /* cmd | 0x80 */
				goto write_long;
			}
			else
			{
				bc = HAK_CODE_PUSH_LITERAL_X2; /* HAK_CODE_PUSH_LITERAL_4 | 0x80 */
				goto write_long2;
			}

		case HAK_CODE_PUSH_OBJECT_0:
		case HAK_CODE_STORE_INTO_OBJECT_0:
		case HAK_CODE_POP_INTO_OBJECT_0:
		case HAK_CODE_JUMP_FORWARD_0:
		case HAK_CODE_JUMP_BACKWARD_0:
		case HAK_CODE_CALL_0:
		case HAK_CODE_SEND_0:
		case HAK_CODE_SEND_TO_SUPER_0:
			if (param_1 < 4)
			{
				/* low 2 bits to hold the parameter */
				bc = (hak_oob_t)(cmd & 0xFC) | (hak_oob_t)param_1;
				goto write_short;
			}
			else
			{
				/* convert the code to a long version */
				bc = cmd | 0x80;
				goto write_long;
			}

		case HAK_CODE_JUMP_FORWARD_IF_TRUE:
		case HAK_CODE_JUMP_FORWARD_IF_FALSE:
		case HAK_CODE_JUMP2_FORWARD_IF_TRUE:
		case HAK_CODE_JUMP2_FORWARD_IF_FALSE:
		case HAK_CODE_JUMP2_FORWARD:
		case HAK_CODE_JUMP_BACKWARD_IF_TRUE:
		case HAK_CODE_JUMP_BACKWARD_IF_FALSE:
		case HAK_CODE_JUMP2_BACKWARD_IF_TRUE:
		case HAK_CODE_JUMP2_BACKWARD_IF_FALSE:
		case HAK_CODE_JUMP2_BACKWARD:

		case HAK_CODE_PUSH_CVAR_I_X:
		case HAK_CODE_STORE_INTO_CVAR_I_X:
		case HAK_CODE_POP_INTO_CVAR_I_X:
		case HAK_CODE_PUSH_CVAR_M_X:
		case HAK_CODE_STORE_INTO_CVAR_M_X:
		case HAK_CODE_POP_INTO_CVAR_M_X:

		case HAK_CODE_CLASS_CMSTORE:
		case HAK_CODE_CLASS_CIMSTORE:
		case HAK_CODE_CLASS_IMSTORE:
		case HAK_CODE_TRY_ENTER:
		case HAK_CODE_TRY_ENTER2:
		case HAK_CODE_PUSH_INTLIT:
		case HAK_CODE_PUSH_NEGINTLIT:
		case HAK_CODE_PUSH_CHARLIT:

		case HAK_CODE_MAKE_DIC: /* TODO: don't these need write_long2? */
		case HAK_CODE_MAKE_ARRAY:
		case HAK_CODE_MAKE_BYTEARRAY:
		case HAK_CODE_MAKE_CHARARRAY:
		case HAK_CODE_POP_INTO_ARRAY:
		case HAK_CODE_POP_INTO_BYTEARRAY:
		case HAK_CODE_POP_INTO_CHARARRAY:
			bc = cmd;
			goto write_long;
	}

	hak_seterrbfmt(hak, HAK_EINVAL, "unhandled single-parameter instruction %u", (unsigned int)cmd);
	return -1;

write_short: /* short parameter */
	if (emit_byte_instruction(hak, bc, srcloc) <= -1) return -1;
	return 0;

write_long: /* long parameter */
	if (param_1 > MAX_CODE_PARAM)
	{
		hak_seterrbfmt(hak, HAK_ERANGE, "parameter too large to single-parameter instruction %u", (unsigned int)cmd);
		return -1;
	}
#if (HAK_CODE_LONG_PARAM_SIZE == 2)
	if (emit_byte_instruction(hak, bc, srcloc) <= -1 ||
	    emit_byte_instruction(hak, (param_1 >> 8) & 0xFF, HAK_NULL) <= -1 ||
	    emit_byte_instruction(hak, (param_1 >> 0) & 0xFF, HAK_NULL) <= -1) return -1;
#else
	if (emit_byte_instruction(hak, bc, srcloc) <= -1 ||
	    emit_byte_instruction(hak, param_1, HAK_NULL) <= -1) return -1;
#endif
	return 0;

write_long2: /* double-long parameter */
	if (param_1 > MAX_CODE_PARAM2)
	{
		hak_seterrbfmt(hak, HAK_ERANGE, "parameter too large to single-parameter instruction %u", (unsigned int)cmd);
		return -1;
	}
#if (HAK_CODE_LONG_PARAM_SIZE == 2)
	if (emit_byte_instruction(hak, bc, srcloc) <= -1 ||
	    emit_byte_instruction(hak, (param_1 >> 24) & 0xFF, HAK_NULL) <= -1 ||
	    emit_byte_instruction(hak, (param_1 >> 16) & 0xFF, HAK_NULL) <= -1 ||
	    emit_byte_instruction(hak, (param_1 >>  8) & 0xFF, HAK_NULL) <= -1 ||
	    emit_byte_instruction(hak, (param_1 >>  0) & 0xFF, HAK_NULL) <= -1) return -1;
#else
	if (emit_byte_instruction(hak, bc, srcloc) <= -1 ||
	    emit_byte_instruction(hak, (param_1 >> 8) & 0xFF, HAK_NULL) <= -1 ||
	    emit_byte_instruction(hak, (param_1 >> 0) & 0xFF, HAK_NULL) <= -1) return -1;
#endif
	return 0;
}

static int emit_two_param_instruction (hak_t* hak, int cmd, hak_oow_t param_1, hak_oow_t param_2, const hak_loc_t* srcloc)
{
	hak_oob_t bc;

	switch (cmd)
	{
		case HAK_CODE_STORE_INTO_CTXTEMPVAR_0:
		case HAK_CODE_POP_INTO_CTXTEMPVAR_0:
		case HAK_CODE_PUSH_CTXTEMPVAR_0:
		case HAK_CODE_PUSH_OBJVAR_0:
		case HAK_CODE_STORE_INTO_OBJVAR_0:
		case HAK_CODE_POP_INTO_OBJVAR_0:
			if (param_1 < 4 && param_2 < 0xFF)
			{
				/* low 2 bits of the instruction code is the first parameter */
				bc = (hak_oob_t)(cmd & 0xFC) | (hak_oob_t)param_1;
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
		case HAK_CODE_MAKE_FUNCTION:
		case HAK_CODE_MAKE_BLOCK:
		case HAK_CODE_CALL_R:
		case HAK_CODE_SEND_R:
			bc = cmd;
			goto write_long;
	}

	hak_seterrbfmt(hak, HAK_EINVAL, "unhandled double-parameter instruction %u", (unsigned int)cmd);
	return -1;

write_short:
	if (emit_byte_instruction(hak, bc, srcloc) <= -1 ||
	    emit_byte_instruction(hak, param_2, HAK_NULL) <= -1) return -1;
	return 0;

write_long:
	if (param_1 > MAX_CODE_PARAM || param_2 > MAX_CODE_PARAM)
	{
		hak_seterrbfmt(hak, HAK_ERANGE, "parameter too large to double-parameter instruction 0x%u - param_1 0x%zu param_2 0x%zu", (unsigned int)cmd, param_1, param_2);
		return -1;
	}
#if (HAK_CODE_LONG_PARAM_SIZE == 2)
	if (emit_byte_instruction(hak, bc, srcloc) <= -1 ||
	    emit_byte_instruction(hak, (param_1 >> 8) & 0xFF, HAK_NULL) <= -1 ||
	    emit_byte_instruction(hak, (param_1 >> 0) & 0xFF, HAK_NULL) <= -1 ||
	    emit_byte_instruction(hak, (param_2 >> 8) & 0xFF, HAK_NULL) <= -1 ||
	    emit_byte_instruction(hak, (param_2 >> 0) & 0xFF, HAK_NULL) <= -1) return -1;
#else
	if (emit_byte_instruction(hak, bc, srcloc) <= -1 ||
	    emit_byte_instruction(hak, param_1, HAK_NULL) <= -1 ||
	    emit_byte_instruction(hak, param_2, HAK_NULL) <= -1) return -1;
#endif
	return 0;
}

static HAK_INLINE int emit_long_param (hak_t* hak, hak_oow_t param)
{
	/* generate a long parameter */
	if (param > MAX_CODE_PARAM)
	{
		hak_seterrnum(hak, HAK_ERANGE);
		return -1;
	}

#if (HAK_CODE_LONG_PARAM_SIZE == 2)
	/* take up 2 bytes */
	return (emit_byte_instruction(hak, (param >> 8) & 0xFF, HAK_NULL) <= -1 ||
	        emit_byte_instruction(hak, (param >> 0) & 0xFF, HAK_NULL) <= -1)? -1: 0;
#else
	/* take up 1 byte */
	return emit_byte_instruction(hak, param_1, HAK_NULL);
#endif
}

static HAK_INLINE int emit_extended_long_param (hak_t* hak, hak_oow_t param)
{
	/* generate an extended long parameter */
	if (param > MAX_CODE_PARAM2)
	{
		hak_seterrnum(hak, HAK_ERANGE);
		return -1;
	}

#if (HAK_CODE_LONG_PARAM_SIZE == 2)
	/* take up 4 bytes */
	return (emit_byte_instruction(hak, (param >> 24) & 0xFF, HAK_NULL) <= -1 ||
	        emit_byte_instruction(hak, (param >> 16) & 0xFF, HAK_NULL) <= -1 ||
	        emit_byte_instruction(hak, (param >> 8) & 0xFF, HAK_NULL) <= -1 ||
	        emit_byte_instruction(hak, (param >> 0) & 0xFF, HAK_NULL) <= -1)? -1: 0;
#else
	/* take up 2 bytes */
	return (emit_byte_instruction(hak, (param >> 8) & 0xFF, HAK_NULL) <= -1 ||
	        emit_byte_instruction(hak, (param >> 0) & 0xFF, HAK_NULL) <= -1)? -1: 0;
#endif
}

static int emit_push_literal (hak_t* hak, hak_oop_t obj, const hak_loc_t* srcloc)
{
	hak_oow_t index;

	if (HAK_OOP_IS_SMOOI(obj))
	{
		hak_ooi_t i;

		i = HAK_OOP_TO_SMOOI(obj);
		switch (i)
		{
			case -1:
				return emit_byte_instruction(hak, HAK_CODE_PUSH_NEGONE, srcloc);

			case 0:
				return emit_byte_instruction(hak, HAK_CODE_PUSH_ZERO, srcloc);

			case 1:
				return emit_byte_instruction(hak, HAK_CODE_PUSH_ONE, srcloc);

			case 2:
				return emit_byte_instruction(hak, HAK_CODE_PUSH_TWO, srcloc);
		}

		if (i >= 0 && i <= MAX_CODE_PARAM)
		{
			return emit_one_param_instruction(hak, HAK_CODE_PUSH_INTLIT, i, srcloc);
		}
		else if (i < 0 && i >= -(hak_ooi_t)MAX_CODE_PARAM)
		{
			return emit_one_param_instruction(hak, HAK_CODE_PUSH_NEGINTLIT, -i, srcloc);
		}
	}
	else if (HAK_OOP_IS_CHAR(obj))
	{
		hak_ooch_t i;

		i = HAK_OOP_TO_CHAR(obj);

		if (i >= 0 && i <= MAX_CODE_PARAM)
			return emit_one_param_instruction(hak, HAK_CODE_PUSH_CHARLIT, i, srcloc);
	}

	if (add_literal(hak, obj, &index) <= -1 ||
	    emit_one_param_instruction(hak, HAK_CODE_PUSH_LITERAL_0, index, srcloc) <= -1) return -1;

	return 0;
}

static HAK_INLINE void patch_long_jump (hak_t* hak, hak_ooi_t jip, hak_ooi_t jump_offset)
{
	if (jump_offset > MAX_CODE_JUMP)
	{
		/* switch to JUMP2 instruction to allow a bigger jump offset.
		 * up to twice MAX_CODE_JUMP only */

		HAK_ASSERT(hak, jump_offset <= MAX_CODE_JUMP * 2);

		HAK_ASSERT(hak, hak->code.bc.ptr[jip] == HAK_CODE_JUMP_FORWARD_X ||
		                 hak->code.bc.ptr[jip] == HAK_CODE_JUMP_FORWARD_IF_TRUE ||
		                 hak->code.bc.ptr[jip] == HAK_CODE_JUMP_FORWARD_IF_FALSE ||
		                 hak->code.bc.ptr[jip] == HAK_CODE_JUMP_BACKWARD_X ||
		                 hak->code.bc.ptr[jip] == HAK_CODE_JUMP_BACKWARD_IF_TRUE ||
		                 hak->code.bc.ptr[jip] == HAK_CODE_JUMP_BACKWARD_IF_FALSE ||
		                 hak->code.bc.ptr[jip] == HAK_CODE_TRY_ENTER);

		/* JUMP2 instructions are chosen to be greater than its JUMP counterpart by 1 */
		patch_instruction(hak, jip, hak->code.bc.ptr[jip] + 1);
		jump_offset -= MAX_CODE_JUMP;
	}

#if (HAK_CODE_LONG_PARAM_SIZE == 2)
	patch_instruction(hak, jip + 1, jump_offset >> 8);
	patch_instruction(hak, jip + 2, jump_offset & 0xFF);
#else
	patch_instruction(hak, jip + 1, jump_offset);
#endif
}

static HAK_INLINE void patch_long_param (hak_t* hak, hak_ooi_t ip, hak_oow_t param)
{
	HAK_ASSERT(hak, param <= MAX_CODE_PARAM);

#if (HAK_CODE_LONG_PARAM_SIZE == 2)
	patch_instruction(hak, ip, param >> 8);
	patch_instruction(hak, ip + 1, param & 0xFF);
#else
	patch_instruction(hak, ip, param);
#endif
}

/*
static HAK_INLINE void patch_two_long_params (hak_t* hak, hak_ooi_t ip, hak_oow_t param_1, hak_oow_t param_2)
{
#if (HAK_CODE_LONG_PARAM_SIZE == 2)
	patch_instruction(hak, ip, param_1 >> 8);
	patch_instruction(hak, ip + 1, param_1 & 0xFF);
	patch_instruction(hak, ip + 2, param_2 >> 8);
	patch_instruction(hak, ip + 3, param_2 & 0xFF);
#else
	patch_instruction(hak, ip, param_1);
	patch_instruction(hak, ip + 1, param_2);
#endif
}
*/

static HAK_INLINE void patch_extended_long_param (hak_t* hak, hak_ooi_t ip, hak_oow_t param)
{
	HAK_ASSERT(hak, param <= MAX_CODE_PARAM2);

#if (HAK_CODE_LONG_PARAM_SIZE == 2)
	patch_instruction(hak, ip,     (param >> 24) & 0xFF);
	patch_instruction(hak, ip + 1, (param >> 16) & 0xFF);
	patch_instruction(hak, ip + 2, (param >> 8) & 0xFF);
	patch_instruction(hak, ip + 3, (param >> 0) & 0xFF);
#else
	patch_instruction(hak, ip,     (param >> 8) & 9xFF);
	patch_instruction(hak, ip + 1, (param >> 0) & 0xFF);
#endif
}

static int emit_variable_access (hak_t* hak, int mode, const hak_var_info_t* vi, const hak_loc_t* srcloc)
{
	static hak_oob_t inst_map[][3] =
	{
		{ HAK_CODE_PUSH_CTXTEMPVAR_0, HAK_CODE_POP_INTO_CTXTEMPVAR_0, HAK_CODE_STORE_INTO_CTXTEMPVAR_0 },
		{ HAK_CODE_PUSH_IVAR_0,       HAK_CODE_POP_INTO_IVAR_0,       HAK_CODE_STORE_INTO_IVAR_0    },
		{ HAK_CODE_PUSH_CVAR_I_X,     HAK_CODE_POP_INTO_CVAR_I_X,     HAK_CODE_STORE_INTO_CVAR_I_X   },
		{ HAK_CODE_PUSH_CVAR_M_X,     HAK_CODE_POP_INTO_CVAR_M_X,     HAK_CODE_STORE_INTO_CVAR_M_X   }
	};

	switch (vi->type)
	{
		case VAR_INDEXED:
			return emit_two_param_instruction(hak, inst_map[0][mode], vi->ctx_offset, vi->index_in_ctx, srcloc);

		case VAR_INST:
			HAK_ASSERT(hak, vi->ctx_offset == 0);
			return emit_one_param_instruction(hak, inst_map[1][mode], vi->index_in_ctx, srcloc);

		case VAR_CLASS_I: /* class variable in initialization scope */
			HAK_ASSERT(hak, vi->ctx_offset == 0);
			return emit_one_param_instruction(hak, inst_map[2][mode], vi->index_in_ctx, srcloc);

		case VAR_CLASS_CM: /* class variable in class method scope */
		case VAR_CLASS_IM: /* class variable in instance method scope */
			HAK_ASSERT(hak, vi->ctx_offset == 0);
			return emit_one_param_instruction(hak, inst_map[3][mode], vi->index_in_ctx, srcloc);
	}

	return -1;
}

/* ========================================================================= */
static int push_ctlblk (hak_t* hak, const hak_loc_t* errloc, hak_ctlblk_type_t type)
{
	hak_oow_t new_depth;

	HAK_ASSERT(hak, hak->c->ctlblk.depth >= -1);

	if (hak->c->ctlblk.depth == HAK_TYPE_MAX(hak_ooi_t))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_BLKDEPTH, errloc, HAK_NULL, "control block depth too deep");
		return -1;
	}

	new_depth = hak->c->ctlblk.depth + 1;
	if (hak->c->ctlblk.depth >= hak->c->ctlblk.info_capa)
	{
		hak_ctlblk_info_t* tmp;
		hak_oow_t newcapa;

		newcapa = HAK_ALIGN(new_depth + 1, BLK_INFO_BUFFER_ALIGN);
		tmp = (hak_ctlblk_info_t*)hak_reallocmem(hak, hak->c->ctlblk.info, newcapa * HAK_SIZEOF(*tmp));
		if (HAK_UNLIKELY(!tmp))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "failed to resize control block info buffer - %js", orgmsg);
			return -1;
		}

		hak->c->ctlblk.info_capa = newcapa;
		hak->c->ctlblk.info = tmp;
	}

	HAK_MEMSET(&hak->c->ctlblk.info[new_depth], 0, HAK_SIZEOF(hak->c->ctlblk.info[new_depth]));
	hak->c->ctlblk.info[new_depth]._type = type;
	hak->c->ctlblk.depth = new_depth;
	return 0;
}

static void pop_ctlblk (hak_t* hak)
{
	HAK_ASSERT(hak, hak->c->ctlblk.depth >= 0); /* depth is of a signed type */

	/* a control block stays inside a function block.
	 * the control block stack must not be popped past the starting base
	 * of the owning function block */
	HAK_ASSERT(hak, hak->c->ctlblk.depth - 1 >= hak->c->funblk.info[hak->c->funblk.depth].ctlblk_base);
	hak->c->ctlblk.depth--;
}

static int push_clsblk (
	hak_t* hak, const hak_loc_t* errloc, hak_cnode_t* class_name, hak_oow_t nivars, hak_oow_t ncvars,
	const hak_ooch_t* ivars_str, hak_oow_t ivars_strlen, const hak_ooch_t* cvars_str, hak_oow_t cvars_strlen)
{
	hak_oow_t new_depth;
	hak_clsblk_info_t* ci;
	hak_funblk_info_t* fbi;

	HAK_ASSERT(hak, hak->c->clsblk.depth >= -1);

	if (hak->c->clsblk.depth == HAK_TYPE_MAX(hak_ooi_t))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_BLKDEPTH, errloc, HAK_NULL, "class block depth too deep");
		return -1;
	}

	new_depth = hak->c->clsblk.depth + 1;
	if (hak->c->clsblk.depth >= hak->c->clsblk.info_capa)
	{
		hak_clsblk_info_t* tmp;
		hak_oow_t newcapa;

		newcapa = HAK_ALIGN(new_depth + 1, BLK_INFO_BUFFER_ALIGN);
		tmp = (hak_clsblk_info_t*)hak_reallocmem(hak, hak->c->clsblk.info, newcapa * HAK_SIZEOF(*tmp));
		if (HAK_UNLIKELY(!tmp))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "failed to resize class block info buffer - %js", orgmsg);
			return -1;
		}

		hak->c->clsblk.info_capa = newcapa;
		hak->c->clsblk.info = tmp;
	}

	ci = &hak->c->clsblk.info[new_depth];
	HAK_MEMSET(ci, 0, HAK_SIZEOF(*ci));
	ci->class_name = class_name;
	ci->nivars = nivars;
	ci->ncvars = ncvars;

	if (init_class_level_variable_buffer(hak, &ci->ivars, ivars_str, ivars_strlen) <= -1) return -1;
	if (init_class_level_variable_buffer(hak, &ci->cvars, cvars_str, cvars_strlen) <= -1) return -1;

	/* remember the function block depth before the class block is entered */
	ci->funblk_base = hak->c->funblk.depth;

	/* attach the class block to the current function block */
	fbi = &hak->c->funblk.info[hak->c->funblk.depth];
	if (fbi->clsblk_base <= -1) fbi->clsblk_base = new_depth;
	fbi->clsblk_top = new_depth;

	hak->c->clsblk.depth = new_depth;
	return 0;
}

static void pop_clsblk (hak_t* hak)
{
	hak_funblk_info_t* fbi;
	hak_clsblk_info_t* cbi;

	HAK_ASSERT(hak, hak->c->clsblk.depth >= 0); /* depth is of a signed type */
	HAK_ASSERT(hak, hak->c->funblk.depth >= 0);

	fbi = &hak->c->funblk.info[hak->c->funblk.depth];
	HAK_ASSERT(hak, fbi->clsblk_base >= 0 && fbi->clsblk_top >= 0 && fbi->clsblk_top >= fbi->clsblk_base);
	HAK_ASSERT(hak, fbi->clsblk_top == hak->c->clsblk.depth);
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

	cbi = &hak->c->clsblk.info[hak->c->clsblk.depth];
	fini_class_level_variable_buffer(hak, &cbi->ivars);
	fini_class_level_variable_buffer(hak, &cbi->cvars);
	hak->c->clsblk.depth--;
}


static int push_funblk (hak_t* hak, const hak_loc_t* errloc,
	hak_oow_t tmpr_va, hak_oow_t tmpr_nargs, hak_oow_t tmpr_nrvars, hak_oow_t tmpr_nlvars,
	hak_oow_t tmpr_count, hak_oow_t tmpr_len, hak_oow_t make_inst_pos, hak_oow_t lfbase, unsigned int fun_type)
{
	hak_oow_t new_depth;
	hak_funblk_info_t* fbi;

	HAK_ASSERT(hak, hak->c->funblk.depth >= -1);
	if (hak->c->funblk.depth == HAK_TYPE_MAX(hak_ooi_t))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_BLKDEPTH, errloc, HAK_NULL, "function block depth too deep");
		return -1;
	}

	new_depth = hak->c->funblk.depth + 1;
	if (hak->c->funblk.depth >= hak->c->funblk.info_capa)
	{
		hak_funblk_info_t* tmp;
		hak_oow_t newcapa;

		newcapa = HAK_ALIGN(new_depth + 1, BLK_INFO_BUFFER_ALIGN);
		tmp = (hak_funblk_info_t*)hak_reallocmem(hak, hak->c->funblk.info, newcapa * HAK_SIZEOF(*tmp));
		if (HAK_UNLIKELY(!tmp))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "failed to resize function block info buffer - %js", orgmsg);
			return -1;
		}

		hak->c->funblk.info_capa = newcapa;
		hak->c->funblk.info = tmp;
	}

	fbi = &hak->c->funblk.info[new_depth];
	HAK_MEMSET(fbi, 0, HAK_SIZEOF(*fbi));

	fbi->fun_type = fun_type;

	fbi->tmprlen = tmpr_len;
	fbi->tmprcnt = tmpr_count;
	fbi->tmpr_va = tmpr_va;
	fbi->tmpr_nargs = tmpr_nargs;
	fbi->tmpr_nrvars = tmpr_nrvars;
	fbi->tmpr_nlvars = tmpr_nlvars;

	/* remember the control block depth before the function block is entered */
	fbi->ctlblk_base = hak->c->ctlblk.depth;

	/* no class block when the funtion block is entered */
	fbi->clsblk_base = -1;
	fbi->clsblk_top = -1;

	fbi->make_inst_pos = make_inst_pos;
	fbi->lfbase = lfbase;

	fbi->access_outer = 0;
	fbi->accessed_by_inner = 0;

	hak->c->funblk.depth = new_depth;
	return 0;
}

static void clear_funblk_inners (hak_t* hak)
{
	hak_funblk_info_t* fbi;
	fbi = &hak->c->funblk.info[hak->c->funblk.depth];
	while (hak->c->ctlblk.depth > fbi->ctlblk_base) pop_ctlblk(hak);
	while (!(fbi->clsblk_base <= -1 && fbi->clsblk_top <= -1)) pop_clsblk(hak);
}

static void pop_funblk (hak_t* hak)
{
	hak_funblk_info_t* fbi;

	HAK_ASSERT(hak, hak->c->funblk.depth >= 0);

	clear_funblk_inners(hak);
	fbi = &hak->c->funblk.info[hak->c->funblk.depth];
	/* if pop_ctlblk() has been called properly, the following assertion must be true
	 * and the assignment on the next line isn't necessary */
	HAK_ASSERT(hak, hak->c->ctlblk.depth == fbi->ctlblk_base);
	HAK_ASSERT(hak, fbi->clsblk_base <= -1 && fbi->clsblk_top <= -1);

	hak->c->ctlblk.depth = fbi->ctlblk_base;
	/* keep hak->code.lit.len without restoration */

	hak->c->funblk.depth--;

	if (hak->c->funblk.depth >= 0)
	{
		/* restore the string length and the word count to the values captured
		 * at the previous level */
		hak->c->tv.s.len = hak->c->funblk.info[hak->c->funblk.depth].tmprlen;
		hak->c->tv.wcount = hak->c->funblk.info[hak->c->funblk.depth].tmprcnt;
	}
	else
	{
		hak->c->tv.s.len = 0;
		hak->c->tv.wcount = 0;
	}

	if (fbi->make_inst_pos < hak->code.bc.len)
	{
		hak_oow_t attr_mask;

		/* patch the temporaries mask parameter for the MAKE_BLOCK or MAKE_FUNCTION instruction */
		HAK_ASSERT(hak, hak->code.bc.ptr[fbi->make_inst_pos] == HAK_CODE_MAKE_BLOCK ||
		                hak->code.bc.ptr[fbi->make_inst_pos] == HAK_CODE_MAKE_FUNCTION);

		/* the total number of temporaries in this function block must be the sum of
		 * the number of arguments, return variables and local variables */
		HAK_ASSERT(hak, fbi->tmprcnt - hak->c->tv.wcount == fbi->tmpr_nargs + fbi->tmpr_nrvars + fbi->tmpr_nlvars);

		/* the temporaries mask is a bit-mask that encodes the counts of different temporary variables.
		 * and it's split to two intruction parameters when used with MAKE_BLOCK and MAKE_FUNCTION.
		 * the INSTA bit is on if fbi->fun_type == FUN_CIM */
		attr_mask = ENCODE_BLK_MASK(((fbi->fun_type & 0xFF) == FUN_CIM), fbi->tmpr_va, fbi->tmpr_nargs, fbi->tmpr_nrvars, fbi->tmpr_nlvars);
		patch_extended_long_param(hak, fbi->make_inst_pos + 1, attr_mask);
	}
}

/* ========================================================================= */
static HAK_INLINE int _insert_cframe (hak_t* hak, hak_ooi_t index, int opcode, hak_cnode_t* operand)
{
	hak_cframe_t* tmp;

	HAK_ASSERT(hak, index >= 0);

	hak->c->cfs.top++;
	HAK_ASSERT(hak, hak->c->cfs.top >= 0);
	HAK_ASSERT(hak, index <= hak->c->cfs.top);

	if ((hak_oow_t)hak->c->cfs.top >= hak->c->cfs.capa)
	{
		hak_oow_t newcapa;

		newcapa = HAK_ALIGN (hak->c->cfs.top + 256, 256); /* TODO: adjust this capacity */
		tmp = (hak_cframe_t*)hak_reallocmem(hak, hak->c->cfs.ptr, newcapa * HAK_SIZEOF(*tmp));
		if (HAK_UNLIKELY(!tmp))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "failed to grow compiler frame stack- %js", orgmsg);
			hak->c->cfs.top--;
			return -1;
		}

		hak->c->cfs.capa = newcapa;
		hak->c->cfs.ptr = tmp;
	}

	if (index < hak->c->cfs.top)
	{
		HAK_MEMMOVE(&hak->c->cfs.ptr[index + 1], &hak->c->cfs.ptr[index], (hak->c->cfs.top - index) * HAK_SIZEOF(*tmp));
	}

	tmp = &hak->c->cfs.ptr[index];
	tmp->opcode = opcode;
	tmp->operand = operand;
	HAK_MEMSET(&tmp->u, 0, HAK_SIZEOF(tmp->u));
	return 0;
}

static int insert_cframe (hak_t* hak, hak_ooi_t index, int opcode, hak_cnode_t* operand)
{
	if (hak->c->cfs.top == HAK_TYPE_MAX(hak_ooi_t))
	{
		hak_seterrnum(hak, HAK_EFRMFLOOD);
		return -1;
	}

	return _insert_cframe(hak, index, opcode, operand);
}

static int push_cframe (hak_t* hak, int opcode, hak_cnode_t* operand)
{
	if (hak->c->cfs.top == HAK_TYPE_MAX(hak_ooi_t))
	{
		hak_seterrnum(hak, HAK_EFRMFLOOD);
		return -1;
	}

	return _insert_cframe(hak, hak->c->cfs.top + 1, opcode, operand);
}

static HAK_INLINE void pop_cframe (hak_t* hak)
{
	HAK_ASSERT(hak, hak->c->cfs.top >= 0);
	hak->c->cfs.top--;
}

#define PUSH_CFRAME(hak,opcode,operand) \
	do { if (push_cframe(hak,opcode,operand) <= -1) return -1; } while (0)

#define INSERT_CFRAME(hak,index,opcode,operand) \
	do { if (insert_cframe(hak,index,opcode,operand) <= -1) return -1; } while (0)

#define POP_CFRAME(hak) pop_cframe(hak)

#define POP_ALL_CFRAMES(hak) (hak->c->cfs.top = -1)

#define GET_TOP_CFRAME_INDEX(hak) (hak->c->cfs.top)

#define GET_TOP_CFRAME(hak) (&hak->c->cfs.ptr[hak->c->cfs.top])

#define GET_CFRAME(hak,index) (&hak->c->cfs.ptr[index])

#define SWITCH_TOP_CFRAME(hak,_opcode,_operand) \
	do { \
		hak_cframe_t* _cf = GET_TOP_CFRAME(hak); \
		_cf->opcode = _opcode; \
		_cf->operand = _operand; \
	} while (0)

#define SWITCH_CFRAME(hak,_index,_opcode,_operand) \
	do { \
		hak_cframe_t* _cf = GET_CFRAME(hak,_index); \
		_cf->opcode = _opcode; \
		_cf->operand = _operand; \
	} while (0)

static int push_subcframe (hak_t* hak, int opcode, hak_cnode_t* operand)
{
	hak_cframe_t* cf, tmp;

	cf = GET_TOP_CFRAME(hak);
	tmp = *cf;
	cf->opcode = opcode;
	cf->operand = operand;

	if (push_cframe(hak, tmp.opcode, tmp.operand) <= -1) return -1;
	cf = GET_TOP_CFRAME(hak);
	cf->u = tmp.u; /* copy the extra information */

	return 0;
}

static HAK_INLINE hak_cframe_t* find_cframe_from_top (hak_t* hak, int opcode)
{
	hak_cframe_t* cf;
	hak_ooi_t i;

	for (i = hak->c->cfs.top; i >= 0; i--)
	{
		cf = &hak->c->cfs.ptr[i];
		if (cf->opcode == opcode) return cf;
	}

	return HAK_NULL;
}

#define PUSH_SUBCFRAME(hak,opcode,operand) \
	do { if (push_subcframe(hak,opcode,operand) <= -1) return -1; } while (0)

#define GET_SUBCFRAME(hak) (&hak->c->cfs.ptr[hak->c->cfs.top - 1])

/* ========================================================================= */

struct class_vardcl_t
{
	hak_oow_t nivars;
	hak_oow_t ncvars;
	hak_oow_t ivar_start;
	hak_oow_t ivar_len;
	hak_oow_t cvar_start;
	hak_oow_t cvar_len;
};
typedef struct class_vardcl_t class_vardcl_t;

static int collect_vardcl_for_class (hak_t* hak, hak_cnode_t* obj, hak_cnode_t** nextobj, class_vardcl_t* vardcl)
{
	hak_oow_t tv_wcount_saved, tv_slen_saved;
	hak_cnode_t* dcl, * dcl_saved;
	int enclosed = 0;
	static const hak_bch_t* desc[] = { "instance variable", "class variable" };

	HAK_MEMSET(vardcl, 0, HAK_SIZEOF(*vardcl));
	tv_wcount_saved = hak->c->tv.wcount;
	tv_slen_saved = hak->c->tv.s.len;

	dcl = HAK_CNODE_CONS_CAR(obj);
	HAK_ASSERT(hak, HAK_CNODE_IS_CONS_CONCODED(dcl, HAK_CONCODE_TUPLE));

	do
	{
		hak_cnode_t* var;
		int n;
		hak_oow_t checkpoint;

		var = HAK_CNODE_CONS_CAR(dcl);

		if (HAK_CNODE_IS_CONS_CONCODED(var, HAK_CONCODE_TUPLE)) /* [ ... ] */
		{
			if (enclosed)
			{
			synerr_varname:
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_VARNAME, HAK_CNODE_GET_LOC(var), HAK_NULL,
					"not variable name '%.*js'", HAK_CNODE_GET_TOKLEN(var), HAK_CNODE_GET_TOKPTR(var));
				return -1;
			}
			enclosed = 1;
			dcl_saved = dcl;
			dcl = var;
			continue; /* start over */
		}
		else if (HAK_CNODE_IS_ELIST_CONCODED(var, HAK_CONCODE_TUPLE))
		{
			/* no variables inside [] */
			if (enclosed) goto synerr_varname; /* [] inside [] */
			goto next;
		}

		if (!HAK_CNODE_IS_SYMBOL(var) || HAK_CNODE_IS_SYMBOL_BINOP(var)) goto synerr_varname;

		checkpoint = hak->c->tv.s.len;
		n = add_temporary_variable(hak, var, tv_slen_saved, desc[enclosed], HAK_NULL);
		if (n <= -1) return -1;

		if (enclosed)
		{
			/* class variable */
			if (vardcl->nivars >= MAX_NCVARS)
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_VARFLOOD, HAK_CNODE_GET_LOC(var), HAK_NULL,
					"too many(%zu) class variables before '%.*js'",
					vardcl->nivars, HAK_CNODE_GET_TOKLEN(var), HAK_CNODE_GET_TOKPTR(var));
				return -1;
			}
			/*if (cvar_len <= 0) cvar_start = prev_tv_len;
			cvar_len = hak->c->tv.s.len - cvar_start; */
			if (vardcl->cvar_len <= 0) vardcl->cvar_start = checkpoint;
			vardcl->cvar_len += hak->c->tv.s.len - checkpoint;
			vardcl->ncvars++;
		}
		else
		{
			/* instance variable */
			if (vardcl->nivars >= MAX_NIVARS)
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_VARFLOOD, HAK_CNODE_GET_LOC(var), HAK_NULL,
					"too many(%zu) instance variables before '%.*js'",
					vardcl->nivars, HAK_CNODE_GET_TOKLEN(var), HAK_CNODE_GET_TOKPTR(var));
				return -1;
			}
			if (vardcl->ivar_len <= 0) vardcl->ivar_start = (vardcl->cvar_len <= 0)? checkpoint: vardcl->cvar_start;
			vardcl->ivar_len += hak->c->tv.s.len - checkpoint;
			if (vardcl->cvar_len > 0)
			{
				/* place the instance variables before the class variables
				 * if class variables "a b" has been collected before instance variables "cc dd ee"
				 * the rotation below manipulates the buffer to contain "cc dd ee a b".
				 */
				hak_rotate_oochars (&hak->c->tv.s.ptr[vardcl->cvar_start], hak->c->tv.s.len - vardcl->cvar_start, -1, vardcl->cvar_len);
				vardcl->cvar_start += hak->c->tv.s.len - checkpoint;
			}
			vardcl->nivars++;
		}

	next:
		dcl = HAK_CNODE_CONS_CDR(dcl);
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

		if (!HAK_CNODE_IS_CONS(dcl))
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(dcl), HAK_CNODE_GET_TOK(dcl), "redundant cdr in %hs declaration", desc[enclosed]);
			return -1;
		}
	}
	while (1);

	HAK_ASSERT(hak, vardcl->nivars + vardcl->ncvars == hak->c->tv.wcount - tv_wcount_saved);
	*nextobj = HAK_CNODE_CONS_CDR(obj);

	return 0;
}

static int collect_vardcl (hak_t* hak, hak_cnode_t* obj, hak_cnode_t** nextobj, hak_oow_t tv_dup_check_start, hak_oow_t* nvardcls, const hak_bch_t* desc)
{
	/* process a single variable declaration list */
	hak_oow_t ndcls = 0;
	hak_oow_t old_wcount = hak->c->tv.wcount;
	hak_cnode_t* dcl;
	hak_cnode_t* var;

	dcl = HAK_CNODE_CONS_CAR(obj);
	HAK_ASSERT(hak, HAK_CNODE_IS_CONS_CONCODED(dcl, HAK_CONCODE_VLIST));

	do
	{
		var = HAK_CNODE_CONS_CAR(dcl);
	#if 0
		if (!HAK_CNODE_IS_SYMBOL(var))
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_ARGNAME, HAK_CNODE_GET_LOC(var), HAK_NULL,
				"invalid local variable name '%.*js'",
				HAK_CNODE_GET_TOKLEN(var), HAK_CNODE_GET_TOKPTR(var));
			return -1;
		}
	#else
		/* the above checks are not needed as the reader guarantees the followings */
		HAK_ASSERT(hak, HAK_CNODE_IS_SYMBOL(var));
	#endif

		if (add_temporary_variable(hak, var, tv_dup_check_start, desc, HAK_NULL) <= -1) return -1;
		ndcls++;

		dcl = HAK_CNODE_CONS_CDR(dcl);
		if (!dcl) break;

		if (!HAK_CNODE_IS_CONS(dcl))
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(dcl), HAK_CNODE_GET_TOK(dcl), "redundant cdr in %hs variable list", desc);
			return -1;
		}
	}
	while (1);

	HAK_ASSERT(hak, ndcls == hak->c->tv.wcount - old_wcount);
	*nextobj = HAK_CNODE_CONS_CDR(obj);
	*nvardcls = ndcls;

	return 0;
}

static int collect_vardcls (hak_t* hak, hak_cnode_t* obj, hak_cnode_t** nextobj, hak_oow_t tv_dup_check_start, hak_oow_t* nvardcls, const hak_bch_t* desc)
{
	/* process zero or more variable declaration lists in a row */
	hak_oow_t ndcls = 0;
	hak_oow_t old_wcount = hak->c->tv.wcount;

	while (obj && HAK_CNODE_IS_CONS(obj))
	{
		hak_cnode_t* dcl;
		hak_oow_t dclcount;

		dcl = HAK_CNODE_CONS_CAR(obj);
		if (!HAK_CNODE_IS_CONS_CONCODED(dcl, HAK_CONCODE_VLIST)) break;

		if (collect_vardcl(hak, obj, &obj, tv_dup_check_start, &dclcount, desc) <= -1) return -1;
		ndcls += dclcount;
	}

	HAK_ASSERT(hak, ndcls == hak->c->tv.wcount - old_wcount);
	*nvardcls = ndcls;
	*nextobj = obj;
	return 0;
}

static int is_followed_by_vlist (hak_t* hak, hak_cnode_t* obj)
{
	if (obj && HAK_CNODE_IS_CONS(obj))
	{
		hak_cnode_t* dcl;
		dcl = HAK_CNODE_CONS_CAR(obj);
		return HAK_CNODE_IS_CONS_CONCODED(dcl, HAK_CONCODE_VLIST);
	}
	return 0;
}

static int check_if_plain_cnode (hak_t* hak, hak_cnode_t* obj, hak_cnode_t* prev, hak_cnode_t* container, hak_synerrnum_t errnum, const hak_bch_t* bname)
{
	if (!obj)
	{
		hak_setsynerrbfmt(hak, errnum, HAK_CNODE_GET_LOC(prev), HAK_NULL, "no %hs in %.*js", bname, HAK_CNODE_GET_TOKLEN(container), HAK_CNODE_GET_TOKPTR(container));
		return -1;
	}
	else if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(prev), HAK_CNODE_GET_TOK(obj), "redundant cdr where %.*js is expected in %.*js", bname, HAK_CNODE_GET_TOKLEN(container), HAK_CNODE_GET_TOKPTR(container));
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
	COP_COMPILE_PURE_ARRAY_LIST,
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

	COP_EMIT_PUSH_NIL,
	COP_EMIT_PUSH_SYMBOL,
	COP_EMIT_CALL,
	COP_EMIT_SEND,

	COP_EMIT_MAKE_ARRAY,
	COP_EMIT_MAKE_PURE_ARRAY,
	COP_EMIT_MAKE_DIC,
	COP_EMIT_MAKE_CONS,
	COP_EMIT_POP_INTO_ARRAY,
	COP_EMIT_POP_INTO_PURE_ARRAY,
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

static int compile_and (hak_t* hak, hak_cnode_t* src)
{
	hak_cnode_t* obj, * expr;

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));
	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_AND));

	obj = HAK_CNODE_CONS_CDR(src);

	if (!obj)
	{
		/* no value */
		hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(src), HAK_NULL, "no expression specified in and");
		return -1;
	}
	else if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in and");
		return -1;
	}

/* TODO: optimization - eat away all true expressions */
	expr = HAK_CNODE_CONS_CAR(obj);
	obj = HAK_CNODE_CONS_CDR(obj);

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, expr); /* 1 */
	if (obj) PUSH_SUBCFRAME(hak, COP_COMPILE_AND_P1, obj); /* 2 */

	return 0;
}


static HAK_INLINE int compile_and_p1 (hak_t* hak)
{
	hak_cnode_t* obj, * expr;
	hak_cframe_t* cf;
	hak_ooi_t jump_inst_pos;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_AND_P1);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

/* TODO: optimization - eat away all true expressions */
	obj = cf->operand;
	if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in and");
		return -1;
	}

	HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX);
	jump_inst_pos = hak->code.bc.len;

	/* this conditional jump make evaluation short-circuited. the actual jump point is to be patched in compile_and_p2() */
	if (emit_one_param_instruction(hak, HAK_CODE_JUMP_FORWARD_IF_FALSE, MAX_CODE_JUMP, HAK_CNODE_GET_LOC(obj)) <= -1) return -1;
	if (emit_byte_instruction(hak, HAK_CODE_POP_STACKTOP, HAK_CNODE_GET_LOC(obj)) <= -1) return -1;

	expr = HAK_CNODE_CONS_CAR(obj);
	obj = HAK_CNODE_CONS_CDR(obj);

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, expr); /* 1 - compile the current part */

	PUSH_SUBCFRAME(hak, COP_COMPILE_AND_P2, expr); /* 3 - patch the conditional jump instruction */
	cf = GET_SUBCFRAME(hak);
	cf->u.post_and.jump_inst_pos = jump_inst_pos;

	if (obj) PUSH_SUBCFRAME(hak, COP_COMPILE_AND_P1, obj); /* 2 - recurse to compile remaining parts */
	return 0;
}

static HAK_INLINE int compile_and_p2 (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_ooi_t jip;
	hak_oow_t jump_offset;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_AND_P2);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX);
	jip = cf->u.post_and.jump_inst_pos;

	/* patch the jump insruction emitted after each expression inside the 'and' expression
	 * the jump make evaluation short-circuited. */
	jump_offset = hak->code.bc.len - jip - (HAK_CODE_LONG_PARAM_SIZE + 1);
	patch_long_jump(hak, jip, jump_offset);

	POP_CFRAME(hak);
	return 0;
}

/* ========================================================================= */

static int compile_or (hak_t* hak, hak_cnode_t* src)
{
	hak_cnode_t* obj, * expr;

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));
	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_OR));

	obj = HAK_CNODE_CONS_CDR(src);

	if (!obj)
	{
		/* no value */
		hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(src), HAK_NULL, "no expression specified in or");
		return -1;
	}
	else if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in and");
		return -1;
	}

/* TODO: optimization - eat away all false expressions */
	expr = HAK_CNODE_CONS_CAR(obj);
	obj = HAK_CNODE_CONS_CDR(obj);

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, expr); /* 1 */
	PUSH_SUBCFRAME(hak, COP_COMPILE_OR_P1, obj); /* 2 */

	return 0;
}


static HAK_INLINE int compile_or_p1 (hak_t* hak)
{
	hak_cnode_t* obj, * expr;
	hak_cframe_t* cf;
	hak_ooi_t jump_inst_pos;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_OR_P1);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

/* TODO: optimization - eat away all false expressions */

	obj = cf->operand;
	if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in or");
		return -1;
	}

	HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX);
	jump_inst_pos = hak->code.bc.len;

	/* this conditional jump makes evaluation short-circuited. the actual jump point is to be patched in compile_or_p2() */
	if (emit_one_param_instruction(hak, HAK_CODE_JUMP_FORWARD_IF_TRUE, MAX_CODE_JUMP, HAK_CNODE_GET_LOC(obj)) <= -1) return -1;
	if (emit_byte_instruction(hak, HAK_CODE_POP_STACKTOP, HAK_CNODE_GET_LOC(obj)) <= -1) return -1;

	expr = HAK_CNODE_CONS_CAR(obj);
	obj = HAK_CNODE_CONS_CDR(obj);

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, expr); /* 1 */

	PUSH_SUBCFRAME(hak, COP_COMPILE_OR_P2, expr); /* 3 */
	cf = GET_SUBCFRAME(hak);
	cf->u.post_or.jump_inst_pos = jump_inst_pos;

	if (obj) PUSH_SUBCFRAME(hak, COP_COMPILE_OR_P1, obj); /* 2 */

	return 0;
}

static HAK_INLINE int compile_or_p2 (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_ooi_t jip;
	hak_oow_t jump_offset;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_OR_P2);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX);
	jip = cf->u.post_or.jump_inst_pos;

	/* patch the jump insruction emitted after each expression inside the 'and' expression */
	jump_offset = hak->code.bc.len - jip - (HAK_CODE_LONG_PARAM_SIZE + 1);
	patch_long_jump(hak, jip, jump_offset);

	POP_CFRAME(hak);
	return 0;
}

/* ========================================================================= */

/* EXPERIMENT WITH BINOP */
static int compile_plus (hak_t* hak, hak_cnode_t* src)
{
	hak_cnode_t* obj, * expr;

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));
	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_PLUS));

	obj = HAK_CNODE_CONS_CDR(src);
	if (!obj)
	{
		/* no value */
		hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(src), HAK_NULL, "no expression specified in plus");
		return -1;
	}
	else if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in plus");
		return -1;
	}

	expr = HAK_CNODE_CONS_CAR(obj);
	obj = HAK_CNODE_CONS_CDR(obj);

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, expr); /* 1 */

	if (!obj)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(src), HAK_NULL, "no second expression specified in plus");
		return -1;
	}
	else if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in plus");
		return -1;
	}

	expr = HAK_CNODE_CONS_CAR(obj);
	obj = HAK_CNODE_CONS_CDR(obj);

	if (obj)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in plus");
		return -1;
	}

/* TODO: more check on obj */
	PUSH_SUBCFRAME(hak, COP_EMIT_PLUS, src); /* 3 */
	PUSH_SUBCFRAME(hak, COP_COMPILE_OBJECT, expr); /* 2 */
	return 0;
}

static HAK_INLINE int emit_plus (hak_t* hak)
{
	hak_cframe_t* cf;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_PLUS);

	if (emit_byte_instruction(hak, HAK_CODE_PLUS, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	POP_CFRAME(hak);
	return 0;
}

/* ========================================================================= */

static int compile_break (hak_t* hak, hak_cnode_t* src)
{
	/* (break) */
	hak_cnode_t* cmd, * obj;
	hak_ooi_t i;

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));
	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_BREAK));

	cmd = HAK_CNODE_CONS_CAR(src);
	obj = HAK_CNODE_CONS_CDR(src);
	if (obj)
	{
		if (HAK_CNODE_IS_CONS(obj))
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(obj), HAK_NULL, "redundant argument in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		}
		else
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		}
		return -1;
	}

	for (i = hak->c->ctlblk.depth; i > hak->c->funblk.info[hak->c->funblk.depth].ctlblk_base; --i)
	{
		switch (hak->c->ctlblk.info[i]._type)
		{
			case HAK_CTLBLK_TYPE_LOOP:
				goto inside_loop;

			case HAK_CTLBLK_TYPE_TRY:
				/* emit an instruction to exit from the try loop. */
				if (emit_byte_instruction(hak, HAK_CODE_TRY_EXIT, HAK_CNODE_GET_LOC(src)) <= -1) return -1;
				break;

			case HAK_CTLBLK_TYPE_CLASS:
				/* emit an instruction to exit from the class definition scope being defined */
				if (emit_byte_instruction(hak, HAK_CODE_CLASS_EXIT, HAK_CNODE_GET_LOC(src)) <= -1) return -1;
				break;
		}
	}

	hak_setsynerrbfmt(
		hak, HAK_SYNERR_BREAK, HAK_CNODE_GET_LOC(src), HAK_NULL,
		"%.*js outside loop", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
	return -1;

inside_loop:
	for (i = hak->c->cfs.top; i >= 0; --i)
	{
		const hak_cframe_t* tcf;
		tcf = &hak->c->cfs.ptr[i];

		if (tcf->opcode == COP_EMIT_FUN) break; /* seems to cross function boundary */

		if (tcf->opcode == COP_POST_UNTIL_BODY || tcf->opcode == COP_POST_WHILE_BODY)
		{
			hak_ooi_t jump_inst_pos;
			hak_cframe_t* cf;

			/* (break) is not really a function call. but to make it look like a
			 * function call, i generate PUSH_NIL so nil becomes a return value.
			 *     (set x (until #f (break)))
			 * x will get nill. */
			if (emit_byte_instruction(hak, HAK_CODE_PUSH_NIL, HAK_CNODE_GET_LOC(cmd)) <= -1) return -1;

/* TODO: study if supporting expression after break is good like return. (break (+ 10 20)) */
			HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX);
			jump_inst_pos = hak->code.bc.len;

			if (emit_one_param_instruction(hak, HAK_CODE_JUMP_FORWARD_0, MAX_CODE_JUMP, HAK_CNODE_GET_LOC(cmd)) <= -1) return -1;
			INSERT_CFRAME(hak, i, COP_COMPILE_BREAK_P1, cmd);
			cf = GET_CFRAME(hak, i);
			cf->u._break.jump_inst_pos = jump_inst_pos;

			POP_CFRAME(hak);
			return 0;
		}
	}

	/* this part must no be reached. if a loop control block is found,
	 * there must exist a COP_POST_UNTIL_BODY or COP_POST_WHILE_BODY frame */
	hak_setsynerrbfmt(
		hak, HAK_SYNERR_INTERN, HAK_CNODE_GET_LOC(src), HAK_NULL,
		"internal error in compiling %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
	return -1;
}

static int compile_break_p1 (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_ooi_t jip, jump_offset;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_BREAK_P1);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	jip = cf->u._break.jump_inst_pos;;

	/* HAK_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD instruction */
	jump_offset = hak->code.bc.len - jip - (HAK_CODE_LONG_PARAM_SIZE + 1);

	/* no explicit about jump_offset. because break can only place inside
	 * a loop, the same check in post_while_body() must assert
	 * this break jump_offset to be small enough */
	HAK_ASSERT(hak, jump_offset <= MAX_CODE_JUMP * 2);
	patch_long_jump(hak, jip, jump_offset);

	POP_CFRAME(hak);
	return 0;
}

/* ========================================================================= */

static int compile_continue (hak_t* hak, hak_cnode_t* src)
{
	/* (continue) */
	hak_cnode_t* cmd, * obj;
	hak_ooi_t i;

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));
	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_CONTINUE));

	cmd = HAK_CNODE_CONS_CAR(src);
	obj = HAK_CNODE_CONS_CDR(src);
	if (obj)
	{
		if (HAK_CNODE_IS_CONS(obj))
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(obj), HAK_NULL, "redundant argument in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		}
		else
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		}
		return -1;
	}

	for (i = hak->c->ctlblk.depth; i > hak->c->funblk.info[hak->c->funblk.depth].ctlblk_base; --i)
	{
		switch (hak->c->ctlblk.info[i]._type)
		{
			case HAK_CTLBLK_TYPE_LOOP:
				goto inside_loop;

			case HAK_CTLBLK_TYPE_TRY:
				/*must emit an instruction to exit from the try loop.*/
				if (emit_byte_instruction(hak, HAK_CODE_TRY_EXIT, HAK_CNODE_GET_LOC(src)) <= -1) return -1;
				break;

			case HAK_CTLBLK_TYPE_CLASS:
				if (emit_byte_instruction(hak, HAK_CODE_CLASS_EXIT, HAK_CNODE_GET_LOC(src)) <= -1) return -1;
				break;
		}
	}

	hak_setsynerrbfmt(hak, HAK_SYNERR_BREAK, HAK_CNODE_GET_LOC(src), HAK_NULL, "%.*js outside loop", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
	return -1;

inside_loop:
	for (i = hak->c->cfs.top; i >= 0; --i)
	{
		const hak_cframe_t* tcf;
		tcf = &hak->c->cfs.ptr[i];

		if (tcf->opcode == COP_EMIT_FUN) break; /* seems to cross function boundary */

		if (tcf->opcode == COP_POST_UNTIL_BODY || tcf->opcode == COP_POST_WHILE_BODY)
		{
			hak_ooi_t jump_offset;

			HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX);
			jump_offset = hak->code.bc.len - tcf->u.post_while.cond_pos + 1;
			if (jump_offset > 3) jump_offset += HAK_CODE_LONG_PARAM_SIZE;
			if (emit_one_param_instruction(hak, HAK_CODE_JUMP_BACKWARD_0, jump_offset, HAK_CNODE_GET_LOC(cmd)) <= -1) return -1;

			POP_CFRAME(hak);
			return 0;
		}
	}

	/* this part must no be reached. if a loop control block is found,
	 * there must exist a COP_POST_UNTIL_BODY or COP_POST_WHILE_BODY frame */
	hak_setsynerrbfmt(hak, HAK_SYNERR_INTERN, HAK_CNODE_GET_LOC(src), HAK_NULL, "internal error in compiling %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
	return -1;
}

/* ========================================================================= */

#define CEB_IS_BLOCK (1 << 0)
#define CEB_AUTO_FORGED (1 << 1)

static int compile_expression_block (hak_t* hak, hak_cnode_t* src, const hak_bch_t* ctxname, int flags)
{
	hak_cnode_t* cmd, * obj;
	hak_oow_t nlvars, tvslen;
	hak_funblk_info_t* fbi;
	hak_cframe_t* cf;

	/* called for {} after 'do' or a standalone {} */

	if (flags & CEB_IS_BLOCK)
	{
		HAK_ASSERT(hak, HAK_CNODE_IS_CONS_CONCODED(src, HAK_CONCODE_BLOCK) || HAK_CNODE_IS_ELIST_CONCODED(src, HAK_CONCODE_BLOCK));
		cmd = src; /* it's the cons cell itself  */
		/* `obj` must point to the cons cell pointing to the braced expression list */
		obj = HAK_CNODE_IS_ELIST(src)? HAK_NULL: src;
		/* no check for redundant cdr because {} cannot be dotted */
	}
	else
	{
		/* called for 'do ...' */
		cmd = HAK_CNODE_CONS_CAR(src); /* `do` itself */
		/* `obj` must point to the expression list after `do` */
		obj = HAK_CNODE_CONS_CDR(src); /* expression list after it */
		if (obj && !HAK_CNODE_IS_CONS(obj))
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj),
				"redundant cdr in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
			return -1;
		}
	}

	if (is_followed_by_vlist(hak, obj))
	{
		if (is_in_class_init_scope(hak))
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_VARDCLBANNED, HAK_CNODE_GET_LOC(obj), HAK_NULL,
				"variable declaration disallowed in class init scope");
			return -1;
		}

#if defined(LANG_LIMIT_DO) /* this limitation doesn't seem really useful? or make it #pragma based? */
		if (!(flags & CEB_IS_BLOCK) /*&& (flags & CEB_AUTO_FORGED)*/)
		{
			/*
			 * e.g. do | x | { set x 20; };
			 *         ^
			 *         this is not allowed
			 *
			 *      k := (do | x | { set x 20; })
			 *               ^
			 *               not allowed either
			 */
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_VARDCLBANNED, HAK_CNODE_GET_LOC(obj), HAK_NULL,
				"variable declaration disallowed in '%hs' context", ctxname);
			return -1;
		}
#endif
	}

	tvslen = hak->c->tv.s.len;
	nlvars = 0;
	if (obj)
	{
		hak_cnode_t* tmp = obj;
		if (collect_vardcls(hak, obj, &obj, tvslen, &nlvars, ctxname) <= -1) return -1;
		if (nlvars > MAX_CODE_NBLKLVARS)
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_VARFLOOD, HAK_CNODE_GET_LOC(tmp), HAK_NULL,
				"too many(%zu) variables in %.*js", nlvars, HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
			return -1;
		}
	}

	fbi = &hak->c->funblk.info[hak->c->funblk.depth];
	fbi->tmprlen = hak->c->tv.s.len;
	fbi->tmprcnt = hak->c->tv.wcount;
	fbi->tmpr_nlvars = fbi->tmpr_nlvars + nlvars;

	/* for an expression like `(do   )` or `(do | a b |  ) , `obj` will point to HAK_NULL.
	 * let `obj` point to the internal cnode to convert the expression like `(do #nil)` or `(do |a b| #nil)`. */
	if (!obj) obj = &hak->c->fake_cnode.cons_to_nil;

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT_LIST, obj);  /* 1 */

	PUSH_SUBCFRAME(hak, COP_COMPILE_DO_P1, src); /* 2 */
	cf = GET_SUBCFRAME(hak);
	cf->u.post_do.lvar_start = tvslen;
	cf->u.post_do.lvar_end = fbi->tmprlen;

	return 0;
}

static int compile_do (hak_t* hak, hak_cnode_t* xlist)
{
#if 0
	hak_cnode_t* cmd, * obj;
#endif
	int flags = 0;

	/*
	 * 'do' evaluates series of expressions.
	 *
	 * do { ... }
	 * do { ... }  { ... }
	 * do 1 2 3
	 * do (printf "111\n") (printf "2222\n")
	 *
	 * while it looks like a cosmetic element, the followings show obvious difference:
	 *   1 2 3    ## the first element is treated as a callable element. in this case syntax error
	 *   do 1 2 3 ## 1, 2, 3 are just evaulated in that order without 1 being treated as a callable
	 *
	 * More serious example:
	 *   (fun(a b) { return (+ a b) }) 20 30  ## the returned value is 50
	 *   do (fun(a b) { return (+ a b) }) 20 30  ## the returned value is 30
	 *
	 * The following two are mostly equivalent:
	 *   do (a := 20) (b := 30)
	 *   { a := 20; b := 30 }
	 */

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(xlist));
	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(xlist), HAK_CNODE_DO));

#if 0
	cmd = HAK_CNODE_CONS_CAR(xlist); /* do itself */
	obj = HAK_CNODE_CONS_CDR(xlist); /* expression list after it */
#endif

	if (HAK_CNODE_GET_FLAGS(xlist) & HAK_CNODE_AUTO_FORGED) flags |= CEB_AUTO_FORGED;
	return compile_expression_block(hak, xlist, "do", flags);
}

static int compile_do_p1 (hak_t* hak)
{
	hak_cframe_t* cf;
	cf = GET_TOP_CFRAME(hak);

	/* invalidate variables without shrinking the hak->c->tv buffer.
	 *   { | a b | }
	 *   { | d | }
	 * this way, 'a' doesn't overlap with 'd' in terms of their position
	 * within the same function context and. however, it make 'a' invisible in
	 * the second block where 'd' is declared.
	 *
	 * If we shrinked the buffer instead, some extra code to initialize overlapping
	 *    hak->c->tv.wcount = saved_wcount (not available in the current code)
	 *    hak->c->tv.s.len = cf->u.post_do.lvar_start;
	 * variables upon entry to a block would need to be emitted. 'd' would need to
	 * get explicitly initialized to `nil` in the above case.
	 */
	kill_temporary_variables(hak, cf->u.post_do.lvar_start, cf->u.post_do.lvar_end);

	POP_CFRAME(hak);
	return 0;
}

/* ========================================================================= */

static int compile_if (hak_t* hak, hak_cnode_t* src)
{
	hak_cnode_t* cmd, * obj, * cond;
	hak_cframe_t* cf;

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));
	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_IF));

	/* (if (< 20 30)
	 *   (perform this)
	 *   (perform that)
	 * elif (< 20 30)
	 *   (perform it)
	 * else
	 *   (perform this finally)
	 * )
	 */
	cmd = HAK_CNODE_CONS_CAR(src); /* if itself */
	obj = HAK_CNODE_CONS_CDR(src);

	if (!obj)
	{
		/* no value */
		hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(src), HAK_NULL, "no conditional expression after '%.*js'", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	cond = HAK_CNODE_CONS_CAR(obj);
	obj = HAK_CNODE_CONS_CDR(obj);

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, cond); /* 1 */
	PUSH_SUBCFRAME(hak, COP_POST_IF_COND, obj); /* 2 */
	cf = GET_SUBCFRAME(hak);
	cf->u.post_if.body_pos = -1; /* unknown yet */
	cf->u.post_if.jump_inst_pos = -1; /* not needed */
	cf->u.post_if.start_loc = *HAK_CNODE_GET_LOC(src);
	cf->u.post_if.cmd_cnode = cmd;
/* TODO: OPTIMIZATION:
 *       pass information on the conditional if it's an absoluate true or absolute false to
 *       eliminate some code .. i can't eliminate code because there can be else or elif...
 *       if absoluate true, don't need else or other elif part
 *       if absoluate false, else or other elif part is needed.
 */
	return 0;
}

static HAK_INLINE int patch_nearest_post_if_body (hak_t* hak, hak_cnode_t* cmd)
{
	hak_ooi_t jump_inst_pos, body_pos;
	hak_ooi_t jip, jump_offset;
	hak_cframe_t* cf;

	cf = find_cframe_from_top(hak, COP_POST_IF_BODY);
	HAK_ASSERT(hak, cf != HAK_NULL);
	HAK_ASSERT(hak, cf->opcode == COP_POST_IF_BODY);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	/* jump instruction position of the JUMP_FORWARD_IF_FALSE after the conditional of the previous if or elif*/
	jip = cf->u.post_if.jump_inst_pos;

	if (hak->code.bc.len <= cf->u.post_if.body_pos)
	{
		/* the if body is empty. */
		if (emit_byte_instruction(hak, HAK_CODE_PUSH_NIL, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;
	}

	HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX);
	jump_inst_pos = hak->code.bc.len;

	/* emit jump_forward before the beginning of the else block.
	 * this is to make the earlier if or elif block to skip
	 * the else part. it is to be patched in post_else_body(). */
	if (emit_one_param_instruction(hak, HAK_CODE_JUMP_FORWARD_0, MAX_CODE_JUMP, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	/* HAK_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD instruction */
	jump_offset = hak->code.bc.len - jip - (HAK_CODE_LONG_PARAM_SIZE + 1);

	if (jump_offset > MAX_CODE_JUMP * 2)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_IFFLOOD, HAK_CNODE_GET_LOC(cmd), HAK_NULL, "code in %.*js too big - size %zu", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd), jump_offset);
		return -1;
	}
	patch_long_jump(hak, jip, jump_offset);

	/* beginning of the elif/else block code */
	/* to drop the result of the conditional when the conditional is false */
	if (emit_byte_instruction(hak, HAK_CODE_POP_STACKTOP, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	/* this is the actual beginning */
	HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX);
	body_pos = hak->code.bc.len;

	/* modify the POST_IF_BODY frame */
	HAK_ASSERT(hak, cf->opcode == COP_POST_IF_BODY);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);
	cf->u.post_if.body_pos = body_pos;
	cf->u.post_if.jump_inst_pos = jump_inst_pos;

	return 0;
}

static HAK_INLINE int compile_elif (hak_t* hak)
{
	hak_cnode_t* cmd, * obj, * cond, * src;
	hak_cframe_t* cf;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_ELIF);

	src = cf->operand;
	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));
	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_ELIF));

	cmd = HAK_CNODE_CONS_CAR(src); /* elif itself */
	obj = HAK_CNODE_CONS_CDR(src);

	if (!obj)
	{
		/* no value */
		hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(src), HAK_NULL, "no conditional expression after '%.*js'", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	cond = HAK_CNODE_CONS_CAR(obj);
	obj = HAK_CNODE_CONS_CDR(obj);

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, cond); /* 1 */
	PUSH_SUBCFRAME(hak, COP_POST_IF_COND, obj); /* 2 */
	cf = GET_SUBCFRAME(hak);
	cf->u.post_if.body_pos = -1; /* unknown yet */
	cf->u.post_if.jump_inst_pos = -1; /* not needed */
	cf->u.post_if.start_loc = *HAK_CNODE_GET_LOC(src);
	cf->u.post_if.cmd_cnode = cmd;

	return patch_nearest_post_if_body(hak, cmd);
}

static HAK_INLINE int compile_else (hak_t* hak)
{
	hak_cnode_t* cmd, * obj, * src;
	hak_cframe_t* cf;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_ELSE);

	src = cf->operand;
	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));
	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_ELSE));

	cmd = HAK_CNODE_CONS_CAR(src); /* else itself */
	obj = HAK_CNODE_CONS_CDR(src);

	if (obj && !HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	if (check_block_expression_as_body(hak, obj, cmd, FOR_NONE) <= -1) return -1;

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT_LIST, obj);

	return patch_nearest_post_if_body(hak, cmd);
}

/* ========================================================================= */

static int check_class_attr_list (hak_t* hak, hak_cnode_t* attr_list, unsigned int* indexed_type, hak_cnode_t* cmd, hak_cnode_t* class_name)
{
	static struct
	{
		const hak_bch_t* name;
		int shifts;
		unsigned int mask;
		unsigned int value;
	} attr_tab[] = {
		/* the value with 0xFF in the mask field takes up the whole byte
		 * the value with 0x00 in the mask field is a bit value.
		 *
		 * shifts: 0 for object type, 8 for selfspec bit, 12 for spec bit
		 * mask: 0xFF for object type, 0x00 for spec/selfspec bit.
		 *
		 * keep the table sorted in alphabestical order ascending for
		 * binary search */

		{ "b",           0,  0xFF, HAK_OBJ_TYPE_BYTE },
		{ "byte",        0,  0xFF, HAK_OBJ_TYPE_BYTE },
		{ "c",           0,  0xFF, HAK_OBJ_TYPE_CHAR },
		{ "char",        0,  0xFF, HAK_OBJ_TYPE_CHAR },
		{ "character",   0,  0xFF, HAK_OBJ_TYPE_CHAR },

		{ "final",       8,  0x00, HAK_CLASS_SELFSPEC_FLAG_FINAL },

		{ "halfword",    0,  0xFF, HAK_OBJ_TYPE_HALFWORD },
		{ "hw",          0,  0xFF, HAK_OBJ_TYPE_HALFWORD },

		{ "immutable",  12,  0x00, HAK_CLASS_SPEC_FLAG_IMMUTABLE },

		{ "limited",     8,  0x00, HAK_CLASS_SELFSPEC_FLAG_LIMITED },

		{ "uncopyable", 12,  0x00, HAK_CLASS_SPEC_FLAG_UNCOPYABLE },

		{ "v",          12,  0x00, HAK_CLASS_SPEC_FLAG_INDEXED },
		{ "var",        12,  0x00, HAK_CLASS_SPEC_FLAG_INDEXED },
		{ "varying",    12,  0x00, HAK_CLASS_SPEC_FLAG_INDEXED },

		{ "w",           0,  0xFF, HAK_OBJ_TYPE_WORD },
		{ "word",        0,  0xFF, HAK_OBJ_TYPE_WORD }

		/* TODO: uint32 uint16 .. etc */
	};
	hak_obj_type_t ct;

	ct = HAK_OBJ_TYPE_OOP;

	HAK_ASSERT(hak, attr_list != HAK_NULL);
	HAK_ASSERT(hak, HAK_CNODE_IS_CONS_CONCODED(attr_list, HAK_CONCODE_XLIST) ||
	                 HAK_CNODE_IS_ELIST_CONCODED(attr_list, HAK_CONCODE_XLIST));

	if (HAK_CNODE_IS_ELIST_CONCODED(attr_list, HAK_CONCODE_XLIST))
	{
		/* don't allow empty attribute list */
		if (class_name)
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(attr_list), HAK_NULL,
				"empty attribute list on '%.*js' for '%.*js'",
				HAK_CNODE_GET_TOKLEN(class_name), HAK_CNODE_GET_TOKPTR(class_name),
				HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		}
		else
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(attr_list), HAK_NULL,
				"empty attribute list on unnamed class for '%.*js'",
				HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		}
		return -1;
	}

	if (HAK_CNODE_IS_CONS_CONCODED(attr_list, HAK_CONCODE_XLIST))
	{
		hak_cnode_t* c;
		const hak_ooch_t* tokptr;
		hak_oow_t toklen;

		c = attr_list;
		while (c)
		{
			/* [NOTE] this algorithm is underflow safe with hak_oow_t types */
			hak_oow_t base, lim;
			hak_cnode_t* attr;

			attr = HAK_CNODE_CONS_CAR(c);

			tokptr = HAK_CNODE_GET_TOKPTR(attr);
			toklen = HAK_CNODE_GET_TOKLEN(attr);

			if (!HAK_CNODE_IS_TYPED(attr, HAK_CNODE_SYMLIT))
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(attr), HAK_NULL,
					"invalid class attribute name '%.*js'", toklen, tokptr);
				return -1;
			}

			/*
			 * 4 bits for spec flags (bit 12 .. 15)
			 * 4 bits for selfspec flags (bit 8 .. 11)
			 * 8 bits for object type for indexing/variablility (bit 0 .. 7)
			 */

			for (base = 0, lim = HAK_COUNTOF(attr_tab); lim > 0; lim >>= 1) /* binary search */
			{
				hak_oow_t i;
				int n;

				i = base + (lim >> 1); /* mid-point */
				n = hak_comp_oochars_bcstr(tokptr, toklen, attr_tab[i].name);
				if (n == 0)
				{
					/* this is to derive the real mask: (attr_tab[i].mask | attr_tab[i].value).
					 * roughly speaking, it's similary to
					 * 	real_mask = attr_tab[i].mask == 0? attr_tab[i].value: attr_tab[i].mask;
					 *
					 * To flag out duplicate or conflicting attribute, we check if
					 *  - the same bit is already set for a bit-based item (mask field 0x00).
					 *  - a value is non-zero for a byte-based item (mask field 0xFF)
					 */
					if (!!((ct >> attr_tab[i].shifts) & (attr_tab[i].mask | attr_tab[i].value)))
					{
						hak_setsynerrbfmt(
							hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(attr), HAK_NULL,
							"conflicting or duplicate class attribute name '#%.*js'", toklen, tokptr);
						return -1;
					}

					ct &= ~attr_tab[i].mask;
					ct |= (attr_tab[i].value << attr_tab[i].shifts);
					goto found;
				}

				if (n > 0) { base = i + 1; lim--; }
			}

			if (lim <= 0)
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(attr), HAK_NULL,
					"unrecognized class attribute name '#%.*js'", toklen, tokptr);
				return -1;
			}

		found:
			c = HAK_CNODE_CONS_CDR(c);
		}
	}

	*indexed_type = ct;
	return 0;
}

/*
	(class A
		[ x y ] ## instance variables
		:: | x y z | ## class variables <--- how to initialize the class variables???

		## everything inside class after the variable declarations are normal expressions.
		## however, the resolution of some variables will fall under the enclosing class.
		(set x 20)
		(printf "normal statement ....\n");

		(fun new (a b c)
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

static int compile_class (hak_t* hak, hak_cnode_t* src)
{
	hak_cframe_t* cf;
	hak_cnode_t* cmd, * obj, * tmp;
	hak_cnode_t* attr_list;
	hak_cnode_t* class_name, * superclass;
	int nsuperclasses;
	unsigned int indexed_type;

	cmd = HAK_CNODE_CONS_CAR(src);
	obj = HAK_CNODE_CONS_CDR(src);

	attr_list = HAK_NULL;
	class_name = HAK_NULL;
	indexed_type = HAK_OBJ_TYPE_OOP;

	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(cmd, HAK_CNODE_CLASS));

	if (obj)
	{
		HAK_ASSERT(hak, HAK_CNODE_IS_CONS(obj));

		tmp = HAK_CNODE_CONS_CAR(obj);
		if (HAK_CNODE_IS_ELIST_CONCODED(tmp, HAK_CONCODE_XLIST) ||
		    HAK_CNODE_IS_CONS_CONCODED(tmp, HAK_CONCODE_XLIST))
		{
			attr_list = tmp;
			obj = HAK_CNODE_CONS_CDR(obj);
		}
		else goto check_class_name; /* for optimzation. it still works without this jump */
	}

	if (obj)
	{
		HAK_ASSERT(hak, HAK_CNODE_IS_CONS(obj));

		tmp = HAK_CNODE_CONS_CAR(obj);
	check_class_name:
		if (HAK_CNODE_IS_FOR_DATA_SIMPLE(tmp) || HAK_CNODE_IS_FOR_LANG(tmp))
		{
			if (!HAK_CNODE_IS_SYMBOL_IDENT(tmp))
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_CLASS, HAK_CNODE_GET_LOC(tmp), HAK_NULL,
					"invalid class name '%.*js' for '%.*js'",
					HAK_CNODE_GET_TOKLEN(tmp), HAK_CNODE_GET_TOKPTR(tmp),
					HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
				return -1;
			}

			class_name = tmp;
			obj = HAK_CNODE_CONS_CDR(obj);
		}
	}

	if (!obj)
	{
		if (class_name)
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_CLASS, HAK_CNODE_GET_LOC(src), HAK_NULL,
				"incomplete definition of '%.*js' for '%.*js'",
				HAK_CNODE_GET_TOKLEN(class_name), HAK_CNODE_GET_TOKPTR(class_name),
				HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		}
		else
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_CLASS, HAK_CNODE_GET_LOC(src), HAK_NULL,
				"incomplete defintion of unnamed class for '%.*js'",
				HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		}
		return -1;
	}

	if (attr_list && check_class_attr_list(hak, attr_list, &indexed_type, cmd, class_name) <= -1) return -1;

	tmp = HAK_CNODE_CONS_CAR(obj);
	if (HAK_CNODE_IS_COLON(tmp)) /* check for superclass marker */
	{
		hak_cnode_t* marker;

		marker = tmp;
		obj = HAK_CNODE_CONS_CDR(obj);
		if (!obj || !HAK_CNODE_IS_CONS(obj))
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_CLASS, HAK_CNODE_GET_LOC(marker), HAK_NULL,
				"no expression or declaration after %.*js",
				HAK_CNODE_GET_TOKLEN(marker), HAK_CNODE_GET_TOKPTR(marker));
			return -1;
		}

		/* superclass part */
		superclass = HAK_CNODE_CONS_CAR(obj);
		if (!HAK_CNODE_IS_SYMBOL(superclass))
		{
			if (HAK_CNODE_IS_FOR_DATA_SIMPLE(superclass) || HAK_CNODE_IS_FOR_LANG(superclass))
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_CLASS, HAK_CNODE_GET_LOC(marker), HAK_NULL,
					"invalid superclass name '%.*js' after '%.*js' for '%.*js'",
					HAK_CNODE_GET_TOKLEN(superclass), HAK_CNODE_GET_TOKPTR(superclass),
					HAK_CNODE_GET_TOKLEN(marker), HAK_CNODE_GET_TOKPTR(marker),
					HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
			}
			else
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_CLASS, HAK_CNODE_GET_LOC(marker), HAK_NULL,
					"no valid superclass name after '%.*js' for '%.*js'",
					HAK_CNODE_GET_TOKLEN(marker), HAK_CNODE_GET_TOKPTR(marker),
					HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
			}
			return -1;
		}

		nsuperclasses = 1;
		obj = HAK_CNODE_CONS_CDR(obj);
	}
	else
	{
		nsuperclasses = 0;
		superclass = HAK_NULL;
	}

	if (class_name)
	{
	#if 0
		SWITCH_TOP_CFRAME(hak, COP_COMPILE_SYMBOL_LITERAL, class_name); /* 1 - push the class name for a named class */
	#else
		hak_oow_t index;
		hak_oop_t cons, sym;

		sym = hak_makesymbol(hak, HAK_CNODE_GET_TOKPTR(class_name), HAK_CNODE_GET_TOKLEN(class_name));
		if (HAK_UNLIKELY(!sym)) return -1;

		cons = (hak_oop_t)hak_getatsysdic(hak, sym);
		if (!cons)
		{
			cons = (hak_oop_t)hak_putatsysdic(hak, sym, hak->_nil);
			if (HAK_UNLIKELY(!cons)) return -1;
		}

		/* add an association in the system dictionary to the literal frame.
		 * this provides performance advantage at the execution time because
		 * the dictionary doesn't need to be searched for the object.  */
		if (add_literal(hak, cons, &index) <= -1 ||
		    emit_one_param_instruction(hak, HAK_CODE_PUSH_LITERAL_0, index, HAK_CNODE_GET_LOC(class_name)) <= -1) return -1;
	#endif
	}
	else
	{
		/* push nil for class name of an anonymous class */
		if (emit_byte_instruction(hak, HAK_CODE_PUSH_NIL, HAK_CNODE_GET_LOC(cmd)) <= -1) return -1;
	}
	POP_CFRAME(hak);

	PUSH_CFRAME(hak, COP_COMPILE_CLASS_P2, class_name); /* 3 - use class name for assignment */
	cf = GET_TOP_CFRAME(hak);
	cf->u._class.nsuperclasses = 0; /* unsed for CLASS_P2 */
	cf->u._class.indexed_type = indexed_type;
	cf->u._class.start_loc = *HAK_CNODE_GET_LOC(src); /* TODO: use *HAK_CNODE_GET_LOC(cmd) instead? */
	cf->u._class.cmd_cnode = cmd;
	cf->u._class.class_name_cnode = class_name; /* duplicate with operand to COP_COMPILE_CLASS_P2 */

	PUSH_CFRAME(hak, COP_COMPILE_CLASS_P1, obj); /* 2 - variables declaraions and actual body */
	cf = GET_TOP_CFRAME(hak);
	cf->u._class.nsuperclasses = nsuperclasses; /* this needs to change if we support multiple superclasses... */
	cf->u._class.indexed_type = indexed_type;
	cf->u._class.start_loc = *HAK_CNODE_GET_LOC(src); /* TODO: use *HAK_CNODE_GET_LOC(cmd) instead? */
	cf->u._class.cmd_cnode = cmd;
	cf->u._class.class_name_cnode = class_name;

	if (superclass) PUSH_CFRAME(hak, COP_COMPILE_OBJECT, superclass); /* 1 - superclass expression */
	return 0;
}

static HAK_INLINE int compile_class_p1 (hak_t* hak)
{
	/* collect information about declared class-level variables */
	hak_cframe_t* cf;
	hak_cnode_t* obj;
	hak_oow_t saved_tv_wcount, saved_tv_slen;
	class_vardcl_t vardcl;

	cf = GET_TOP_CFRAME(hak);
	obj = cf->operand;

	saved_tv_wcount = hak->c->tv.wcount;
	saved_tv_slen = hak->c->tv.s.len;

	HAK_MEMSET(&vardcl, 0, HAK_SIZEOF(vardcl));

	if (obj && HAK_CNODE_IS_CONS(obj))
	{
		/* class-level variables - instance variables and class variable
		 *  - class X [ a b c [d e] x ] { ... }
		 *  - a b c are instance variables.
		 *  - d e, enclsoed in another [], are class variables.
		 * */
		hak_cnode_t* tmp;
		tmp = HAK_CNODE_CONS_CAR(obj);
		if (HAK_CNODE_IS_CONS_CONCODED(tmp, HAK_CONCODE_TUPLE))
		{
			if (collect_vardcl_for_class(hak, obj, &obj, &vardcl) <= -1) return -1;
		}
	}

	/* emit placeholder instructions to be patched in compile_class_p2() */
	if (emit_one_param_instruction(hak, HAK_CODE_PUSH_LITERAL_0, MAX_CODE_PARAM2, &cf->u._class.start_loc) <= -1) return -1;
	if (emit_one_param_instruction(hak, HAK_CODE_PUSH_LITERAL_0, MAX_CODE_PARAM2, &cf->u._class.start_loc) <= -1) return -1;

	if (check_block_expression_as_body(hak, obj, cf->u._class.cmd_cnode, FOR_CLASS) <= -1) return -1;

	if (push_clsblk(hak, &cf->u._class.start_loc,
		cf->u._class.class_name_cnode, vardcl.nivars, vardcl.ncvars,
		&hak->c->tv.s.ptr[vardcl.ivar_start], vardcl.ivar_len,
		&hak->c->tv.s.ptr[vardcl.cvar_start], vardcl.cvar_len) <= -1) goto oops;
	if (push_ctlblk(hak, &cf->u._class.start_loc, HAK_CTLBLK_TYPE_CLASS) <= -1) goto oops; /* the class block shall be treated as a control block, too */

	/* discard the instance variables and class variables in the temporary variable collection buffer
	 * because they have been pushed to the class block structure */
	hak->c->tv.s.len = saved_tv_slen;
	hak->c->tv.wcount = saved_tv_wcount;

	/* the position of the CLASS_ENTER instruction */
	hak->c->clsblk.info[hak->c->clsblk.depth].class_enter_inst_pos = hak->code.bc.len;

	/* class_enter nsuperclasses, nivars, ncvars  */
	if (emit_byte_instruction(hak, HAK_CODE_CLASS_ENTER, &cf->u._class.start_loc) <= -1) goto oops;
	if (emit_long_param(hak, cf->u._class.nsuperclasses) <= -1) goto oops;
	if (emit_long_param(hak, vardcl.nivars) <= -1) goto oops;
	if (emit_long_param(hak, vardcl.ncvars) <= -1) goto oops;
	if (emit_byte_instruction(hak, (hak_oob_t)((cf->u._class.indexed_type >> 8) & 0xFF), &cf->u._class.start_loc) <= -1) goto oops;
	if (emit_byte_instruction(hak, (hak_oob_t)(cf->u._class.indexed_type & 0xFF), &cf->u._class.start_loc) <= -1) goto oops;

	/* remember the first byte code position to be emitted for the body of
	 * this class. this posistion is used for empty class body check at the
	 * end of the class before 'class_exit' is generated */
	hak->c->clsblk.info[hak->c->clsblk.depth].class_start_inst_pos = hak->code.bc.len;

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT_LIST, obj);
	return 0;

oops:
	hak->c->tv.s.len = saved_tv_slen;
	hak->c->tv.wcount = saved_tv_wcount;
	return -1;
}

static HAK_INLINE int compile_class_p2 (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_cnode_t* class_name;
	hak_loc_t class_loc;
	hak_clsblk_info_t* cbi;
	hak_oow_t patch_pos, patch_end;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_CLASS_P2);
	/*HAK_ASSERT(hak, cf->operand != HAK_NULL);*/

	class_name = cf->operand;
	class_loc = class_name? *HAK_CNODE_GET_LOC(class_name): cf->u._class.start_loc;

	if (hak->code.bc.len > hak->c->clsblk.info[hak->c->clsblk.depth].class_start_inst_pos)
	{
		/* no instructions generated after the class_enter instruction */
		if (emit_byte_instruction(hak, HAK_CODE_POP_STACKTOP, &class_loc) <= -1) return -1;
	}

	cbi = &hak->c->clsblk.info[hak->c->clsblk.depth];
	/* patch the CLASS_ENTER instruction with the final nicvars and ncvars values.
	 * CLASS_ENTER nsuperclasses(lp)|nivars(lp)|ncvars(lp)|spec/selfspec(b)|index_type(b)
	 *  (lp) = long param, (b) = byte */
	patch_pos = cbi->class_enter_inst_pos + 1;
	patch_pos += HAK_CODE_LONG_PARAM_SIZE; /* skip nsuperclasses */
	patch_long_param(hak, patch_pos, cbi->nivars);
	patch_pos += HAK_CODE_LONG_PARAM_SIZE; /* skip nivars */
	patch_long_param(hak, patch_pos, cbi->ncvars);

	/* two placeholder instructions have been pushed before push_clsblk()
	 * in compile_class_p1().
	 *   push_literal long-param long-param <-- (1) position of first long-param
	 *   push_literal long-param long-param <-- (2) position of first long-param
	 *   class_enter ...                    <-- class_enter_inst_pos
	 */
	patch_pos = cbi->class_enter_inst_pos - (HAK_CODE_LONG_PARAM_SIZE * 4 + 1); /* (1) */
	if (cbi->nivars > 0)
	{
		/* patch the PUSH_LITERAL instruction for ivars */
		/* TODO: reduce space waste for fixed double-long param */
		hak_oop_t obj;
		hak_oow_t index;

		HAK_ASSERT(hak, cbi->nivars <= MAX_NIVARS);
		obj = hak_makestring(hak, cbi->ivars.ptr, cbi->ivars.len);
		if (HAK_UNLIKELY(!obj)) return -1;
		if (add_literal(hak, obj, &index) <= -1) return -1;
		patch_extended_long_param(hak, patch_pos, index);
	}
	else
	{
		/* TODO: reduce space waste for patched NOOP */
		patch_end = (--patch_pos) + (HAK_CODE_LONG_PARAM_SIZE * 2) + 1;
		for (; patch_pos < patch_end; patch_pos++)
			patch_instruction(hak, patch_pos, HAK_CODE_NOOP);
	}

	patch_pos = cbi->class_enter_inst_pos - (HAK_CODE_LONG_PARAM_SIZE * 2); /* (2) */
	if (cbi->ncvars > 0)
	{
		/* patch the PUSH_LITERAL instruction for cvars */
		/* TODO: reduce space waste for fixed double-long param */
		hak_oop_t obj;
		hak_oow_t index;

		HAK_ASSERT(hak, cbi->ncvars <= MAX_NCVARS);
		obj = hak_makestring(hak, cbi->cvars.ptr, cbi->cvars.len);
		if (HAK_UNLIKELY(!obj)) return -1;
		if (add_literal(hak, obj, &index) <= -1) return -1;
		patch_extended_long_param(hak, patch_pos, index);
	}
	else
	{
		/* TODO: reduce space waste for patched NOOP */
		patch_end = (--patch_pos) + (HAK_CODE_LONG_PARAM_SIZE * 2) + 1;
		HAK_ASSERT(hak, patch_end == cbi->class_enter_inst_pos);
		for (;patch_pos < patch_end; patch_pos++)
			patch_instruction(hak, patch_pos, HAK_CODE_NOOP);
	}

	pop_ctlblk(hak);
	pop_clsblk(hak);  /* end of the class block */

	if (emit_byte_instruction(hak, HAK_CODE_CLASS_PEXIT, &class_loc) <= -1) return -1; /* pop + exit */

	if (class_name)
	{
		/* a class name is treated like a global variable */
		SWITCH_TOP_CFRAME(hak, COP_EMIT_SET, class_name);
		cf = GET_TOP_CFRAME(hak);
		cf->u.set.vi.type = VAR_NAMED;
		cf->u.set.mode = VAR_ACCESS_STORE;
	}
	else
	{
		POP_CFRAME(hak);
	}

	return 0;
}

/* ========================================================================= */

static int check_fun_attr_list (hak_t* hak, hak_cnode_t* attr_list, unsigned int* fun_type, hak_cnode_t* cmd, hak_cnode_t* class_name, hak_cnode_t* fun_name)
{
	unsigned int ft;

	ft = FUN_IM;

	HAK_ASSERT(hak, attr_list != HAK_NULL);
	HAK_ASSERT(hak, HAK_CNODE_IS_CONS_CONCODED(attr_list, HAK_CONCODE_XLIST) ||
	                 HAK_CNODE_IS_ELIST_CONCODED(attr_list, HAK_CONCODE_XLIST));

	if (HAK_CNODE_IS_ELIST_CONCODED(attr_list, HAK_CONCODE_XLIST))
	{
		/* don't allow empty attribute list */
		if (class_name && fun_name)
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(attr_list), HAK_NULL,
				"empty attribute list on '%.*js:%.*js' for '%.*js'",
				HAK_CNODE_GET_TOKLEN(class_name), HAK_CNODE_GET_TOKPTR(class_name),
				HAK_CNODE_GET_TOKLEN(fun_name), HAK_CNODE_GET_TOKPTR(fun_name),
				HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		}
		else if (fun_name)
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(attr_list), HAK_NULL,
				"empty attribute list on '%.*js' for '%.*js'",
				HAK_CNODE_GET_TOKLEN(fun_name), HAK_CNODE_GET_TOKPTR(fun_name),
				HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		}
		else
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(attr_list), HAK_NULL,
				"empty attribute list on unnamed function for '%.*js'",
				HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		}
		return -1;
	}

	if (HAK_CNODE_IS_CONS_CONCODED(attr_list, HAK_CONCODE_XLIST))
	{
		hak_cnode_t* c, * a;
		const hak_ooch_t* tokptr;
		hak_oow_t toklen;

		c = attr_list;
		while (c)
		{
			a = HAK_CNODE_CONS_CAR(c);

			tokptr = HAK_CNODE_GET_TOKPTR(a);
			toklen = HAK_CNODE_GET_TOKLEN(a);

			if (!HAK_CNODE_IS_TYPED(a, HAK_CNODE_SYMLIT))
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(a), HAK_NULL,
					"invalid function attribute name '%.*js'", toklen, tokptr);
				return -1;
			}

			if (hak_comp_oochars_bcstr(tokptr, toklen, "class") == 0 ||
			    hak_comp_oochars_bcstr(tokptr, toklen, "c") == 0)
			{
				if (ft != FUN_IM)
				{
				conflicting:
					hak_setsynerrbfmt(
						hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(a), HAK_NULL,
						"conflicting function attribute name '#%.*js'", toklen, tokptr);
					return -1;
				}
				ft = FUN_CM;
			}
			else if (hak_comp_oochars_bcstr(tokptr, toklen, "classinst") == 0 ||
			         hak_comp_oochars_bcstr(tokptr, toklen, "ci") == 0)
			{
				if (ft != FUN_IM) goto conflicting;
				ft = FUN_CIM;
			}
			else
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(a), HAK_NULL,
					"unrecognized function attribute name '#%.*js'", toklen, tokptr);
				return -1;
			}

			c = HAK_CNODE_CONS_CDR(c);
		}
	}

	*fun_type = ft;
	return 0;
}

static int compile_fun (hak_t* hak, hak_cnode_t* src)
{
	hak_cnode_t* cmd, * next;
	hak_oow_t va, nargs, nrvars, nlvars;
	hak_ooi_t jump_inst_pos, lfsize_pos;
	hak_oow_t saved_tv_wcount, tv_dup_start;
	hak_cnode_t* fun_name;
	hak_ooi_t fun_name_lfindex;
	hak_cnode_t* class_name;
	hak_cnode_t* attr_list;
	hak_cnode_t* arg_list;
	hak_cnode_t* fun_body;
	hak_cframe_t* cf;
	unsigned int fun_type;

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));

	saved_tv_wcount = hak->c->tv.wcount;
	cmd = HAK_CNODE_CONS_CAR(src);
	next = HAK_CNODE_CONS_CDR(src);
	fun_name = HAK_NULL;
	class_name = HAK_NULL;
	attr_list = HAK_NULL;
	arg_list = HAK_NULL;
	fun_body = HAK_NULL;
	fun_type = FUN_PLAIN;

	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(cmd, HAK_CNODE_FUN));

	if (next)
	{
		hak_cnode_t* tmp;

		/* the reader ensures that the cdr field of a cons cell points to the next cell.
		 * and only the field of the last cons cell is NULL. */
		HAK_ASSERT(hak, HAK_CNODE_IS_CONS(next));

		/* fun (arg..)
		 * fun name(arg..)
		 * fun(#attr..) name(arg..)  ## valid as class method, not valid as plain function
		 * fun(#attr..) (arg..)      ## not valid. not attribute list for unnamed functions
		 * fun(#attr..) class:name(arg..)
		 */

		tmp = HAK_CNODE_CONS_CAR(next);
		if (HAK_CNODE_IS_SYMBOL(tmp))
		{
			/* 'fun' followed by name */
		fun_got_name:
			/* name must be followed by argument list */
			fun_name = tmp;

			next = HAK_CNODE_CONS_CDR(next);
			if (!next)
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(fun_name), HAK_NULL,
					"function name '%.*js' not followed by ( or : for '%.*js'",
					HAK_CNODE_GET_TOKLEN(fun_name), HAK_CNODE_GET_TOKPTR(fun_name),
					HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
				return -1;
			}

			tmp = HAK_CNODE_CONS_CAR(next);
			if (HAK_CNODE_IS_COLON(tmp))
			{
				/* fun class:name(arg..)
				 * fun(#attr..) class:name(arg..) */

				class_name = fun_name;
				next = HAK_CNODE_CONS_CDR(next);
				if (!next)
				{
					hak_setsynerrbfmt(
						hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(class_name), HAK_NULL,
						"no function name after class name '%.*js:' for '%.*js'",
						HAK_CNODE_GET_TOKLEN(class_name), HAK_CNODE_GET_TOKPTR(class_name),
						HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
					return -1;
				}

				tmp = HAK_CNODE_CONS_CAR(next);
				if (!HAK_CNODE_IS_SYMBOL(tmp))
				{
					hak_setsynerrbfmt(
						hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(tmp), HAK_NULL,
						"invalid function name '%.*js' after '%.*js:' for '%.*js'",
						HAK_CNODE_GET_TOKLEN(tmp), HAK_CNODE_GET_TOKPTR(tmp),
						HAK_CNODE_GET_TOKLEN(class_name), HAK_CNODE_GET_TOKPTR(class_name),
						HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
					return -1;
				}
				fun_name = tmp;

				next = HAK_CNODE_CONS_CDR(next);
				if (!next)
				{
					hak_setsynerrbfmt(
						hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(fun_name), HAK_NULL,
						"function name '%.*js:%.*js' not followed by ( for '%.*js'",
						HAK_CNODE_GET_TOKLEN(class_name), HAK_CNODE_GET_TOKPTR(class_name),
						HAK_CNODE_GET_TOKLEN(fun_name), HAK_CNODE_GET_TOKPTR(fun_name),
						HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
					return -1;
				}

				tmp = HAK_CNODE_CONS_CAR(next); /* pointing to argument list */

				if (is_in_class_init_scope(hak))
				{
					hak_setsynerrbfmt(
						hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(fun_name), HAK_NULL,
						"class name '%.*js' before :'%.*js' prohibited in class initialization context",
						HAK_CNODE_GET_TOKLEN(class_name), HAK_CNODE_GET_TOKPTR(class_name),
						HAK_CNODE_GET_TOKLEN(fun_name), HAK_CNODE_GET_TOKPTR(fun_name));
					return -1;
				}
			}
		}

		if (HAK_CNODE_IS_CONS_CONCODED(tmp, HAK_CONCODE_XLIST) ||
		    HAK_CNODE_IS_ELIST_CONCODED(tmp, HAK_CONCODE_XLIST))
		{
			/* 'fun' followed by attribute or argument list */
			arg_list = tmp;
			if (fun_name)
			{
				/* argument list for sure
				 *   fun class:name()
				 *   fun name      ()
				 *                 ^  */
				next = HAK_CNODE_CONS_CDR(next); /* point past argument list */
				if (!next)
				{
					hak_setsynerrbfmt(
						hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(cmd), HAK_NULL,
						"no function body after argument list of function '%.*js' for '%.*js'",
						HAK_CNODE_GET_TOKLEN(fun_name), HAK_CNODE_GET_TOKPTR(fun_name),
						HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
					return -1;
				}
				fun_body = next;
			}
			else
			{
				/* not clear if it is attribute list or argument list */
				next = HAK_CNODE_CONS_CDR(next); /* point past attribute/argument list */
				if (!next)
				{
					/* TODO: guess if the current list looks like attribute list or
					 *       not by inspecting elements and produce better error mesage.
					 *       another hack is to disallow ELIST as attribute list? */
					hak_setsynerrbfmt(
						hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(cmd), HAK_NULL,
						"unnamed function not followed by function body for '%.*js'",
						HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
					return -1;
				}

				tmp = HAK_CNODE_CONS_CAR(next);
				if (HAK_CNODE_IS_SYMBOL(tmp))
				{
					/* it is attribute list for sure. fun(#attr..) name */
					attr_list = arg_list;
					arg_list = HAK_NULL;
					goto fun_got_name;
				}
				else if (HAK_CNODE_IS_CONS_CONCODED(tmp, HAK_CONCODE_XLIST) ||
				         HAK_CNODE_IS_ELIST_CONCODED(tmp, HAK_CONCODE_XLIST))
				{
					/* fun(#attr..) (arg..) .. */
					attr_list = arg_list;
					arg_list = tmp;

					next = HAK_CNODE_CONS_CDR(next); /* point past argument list */
					if (!next)
					{
						hak_setsynerrbfmt(
							hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(cmd), HAK_NULL,
							"no function body after attribute list and argument list of unnamed function for '%.*js'",
							HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
						return -1;
					}
					fun_body = next;
				}
				else
				{
					/* fun(arg..) .. */
					fun_body = next;
				}
			}

			if (!fun_name && is_in_class_init_scope(hak))
			{
				/* TODO: it must allow as rvalue..
				 * class X {
				 *     b := (fun() {}) ## this is allowed  <- TODO: not supported yet
				 *     fun() {} ## this is prohibited
				 * }
				 */
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(cmd), HAK_NULL,
					"unnamed function defined with '%.*js' prohibited in class initialziation context",
					HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
				return -1;
			}
		}
		else
		{
			/* not valid argument list or attribute list
			 * fun if         ## cmd is 'fun', fun_name is nil
			 * fun a if       ## cmd is 'fun', fun_name is 'a'
			 * fun a:b if     ## cmd is 'fun', fun_name is 'b'
			 * fun self.a     ## cmd is 'fun', fun_name is nil
			 */
			if (fun_name)
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(tmp), HAK_NULL,
					"'%.*js' not followed by ( but followed by '%.*js'",
					HAK_CNODE_GET_TOKLEN(fun_name), HAK_CNODE_GET_TOKPTR(fun_name),
					HAK_CNODE_GET_TOKLEN(tmp), HAK_CNODE_GET_TOKPTR(tmp));
			}
			else
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(tmp), HAK_NULL,
					"invalid function name '%.*js' for '%.*js'",
					HAK_CNODE_GET_TOKLEN(tmp), HAK_CNODE_GET_TOKPTR(tmp),
					HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
			}
			return -1;
		}
	}
	else
	{
		/* nothing after 'fun' (e.g. fun ) */
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(cmd), HAK_NULL,
			"'%.*js' not followed by name or (",
			HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	if (attr_list)
	{
		if (is_in_class_init_scope(hak) || class_name)
		{
/* TODO: */
			/* TODO: THIS IS ALSO WRONG.
			 *  class X {
			 *    a := (fun x(){}) ## this context is also class_init_scope. so the check above isn't good enough
			 *  } */
			if (check_fun_attr_list(hak, attr_list, &fun_type, cmd, class_name, fun_name) <= -1) return -1;
			if (class_name) fun_type |= 0x100; /* defined in `fun class:xxx` style outside class */
		}
		else
		{
			if (fun_name)
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(attr_list), HAK_NULL,
					"attribute list prohibited on plain function '%.*js'",
					HAK_CNODE_GET_TOKLEN(fun_name), HAK_CNODE_GET_TOKPTR(fun_name));
			}
			else
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_FUN, HAK_CNODE_GET_LOC(attr_list), HAK_NULL,
					"attribute list prohibited on unnamed function for '%.*js'",
					HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
			}
			return -1;
		}
	}
	else
	{
		if (is_in_class_init_scope(hak) || class_name)
		{
			fun_type = FUN_IM;
			if (class_name) fun_type |= 0x100;
		}
	}

	/* process the argument list */
	va = 0;
	nargs = 0;
	nrvars = 0;

	HAK_ASSERT(hak, HAK_CNODE_IS_ELIST_CONCODED(arg_list, HAK_CONCODE_XLIST) ||
	                 HAK_CNODE_IS_CONS_CONCODED(arg_list, HAK_CONCODE_XLIST));
	if (HAK_CNODE_IS_ELIST_CONCODED(arg_list, HAK_CONCODE_XLIST))
	{
		/* empty list - no argument - fun () {+ 10 20} */
		/* do nothing */
	}
	else
	{
		hak_cnode_t* arg, * dcl;
		int in_ret_args = 0;

		tv_dup_start = hak->c->tv.s.len;
		dcl = arg_list;
		do
		{
			arg = HAK_CNODE_CONS_CAR(dcl);

			if (in_ret_args)
			{
				if (!HAK_CNODE_IS_SYMBOL_IDENT(arg))
				{
					/* in 'fun x (x :: 20) { }', '20' is not a valid return variable name.
					 * in 'fun x (x :: if) { }', 'if' is not a valid return variable name. */
					hak_setsynerrbfmt(
						hak, HAK_SYNERR_VARNAME, HAK_CNODE_GET_LOC(arg), HAK_NULL,
						"invalid return variable '%.*js' for '%.*js'",
						HAK_CNODE_GET_TOKLEN(arg), HAK_CNODE_GET_TOKPTR(arg),
						HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
					return -1;
				}

				if (add_temporary_variable(hak, arg, tv_dup_start, "return variable", HAK_CNODE_GET_TOK(cmd)) <= -1) return -1;
				nrvars++;
			}
			else if (va)
			{
				if (HAK_CNODE_IS_DBLCOLONS(arg))
				{
					in_ret_args = 1;
				}
				else
				{
					/* in 'fun x (... a) {}', 'a' is an unexpected token.
					 * only ')' or '::' can follow ... */
					hak_setsynerrbfmt(
						hak, HAK_SYNERR_CNODE, HAK_CNODE_GET_LOC(arg), HAK_NULL,
						"unexpected token '%.*js' after '...' for '%.*js'",
						HAK_CNODE_GET_TOKLEN(arg), HAK_CNODE_GET_TOKPTR(arg),
						HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
					return -1;
				}
			}
			else
			{
				if (HAK_CNODE_IS_DBLCOLONS(arg))
				{
					in_ret_args = 1;
				}
				else if (HAK_CNODE_IS_ELLIPSIS(arg))
				{
					va = 1;
				}
				else if (!HAK_CNODE_IS_SYMBOL_IDENT(arg))
				{
					hak_setsynerrbfmt(
						hak, HAK_SYNERR_ARGNAME, HAK_CNODE_GET_LOC(arg), HAK_NULL,
						"invalid argument name '%.*js' for '%.*js'",
						HAK_CNODE_GET_TOKLEN(arg), HAK_CNODE_GET_TOKPTR(arg),
						HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
					return -1;
				}
				else
				{
					if (add_temporary_variable(hak, arg, tv_dup_start, "argument", HAK_CNODE_GET_TOK(cmd)) <= -1) return -1;
					nargs++;
				}
			}

			dcl = HAK_CNODE_CONS_CDR(dcl);
			if (!dcl) break;

			if (!HAK_CNODE_IS_CONS(dcl))
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(dcl), HAK_CNODE_GET_TOK(dcl),
					"redundant cdr in argument list in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
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
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_ARGFLOOD, HAK_CNODE_GET_LOC(arg_list), HAK_NULL,
			"too many(%zu) arguments in %.*js", nargs,
			HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	if (nrvars > MAX_CODE_NBLKLVARS)
	{
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_VARFLOOD, HAK_CNODE_GET_LOC(arg_list), HAK_NULL,
			"too many(%zu) return variables in %.*js", nrvars,
			HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	HAK_ASSERT(hak, nargs + nrvars == hak->c->tv.wcount - saved_tv_wcount);

	/*
	 * fun aa(a b) { ...	};
	 * (fun aa(a b) { ... })
	 *
	 * the block expression must be the first and the only expression at the body position.
	 * the variable declaration can't be placed before the block expression.
	 * it is supported inside the block expression itself.
	 */
	if (check_block_expression_as_body(hak, fun_body, cmd, FOR_NONE) <= -1) return -1;

	HAK_ASSERT(hak, fun_body != HAK_NULL);
	nlvars = 0; /* no known local variables until the actual block is processed */

	HAK_ASSERT(hak, nargs + nrvars + nlvars == hak->c->tv.wcount - saved_tv_wcount);

	if (push_funblk(
		hak, HAK_CNODE_GET_LOC(src), va, nargs, nrvars, nlvars, hak->c->tv.wcount,
		hak->c->tv.s.len, hak->code.bc.len, hak->code.lit.len, fun_type) <= -1) return -1;

	fun_name_lfindex = MAX_CODE_PARAM2; /* this maximum value indicates no function name */
	if (fun_name)
	{
		hak_oop_t sym;
		sym = hak_makesymbol(hak, HAK_CNODE_GET_TOKPTR(fun_name), HAK_CNODE_GET_TOKLEN(fun_name));
		if (HAK_UNLIKELY(!sym)) return -1;
		if (add_literal(hak, sym, &fun_name_lfindex) <= -1) return -1;
	}

	if (hak->option.trait & HAK_TRAIT_INTERACTIVE)
	{
		/* MAKE_FUNCTION attr_mask_1 attr_mask_2 name_lfindex lfbase lfsize */
		hak_oow_t lfbase;

		lfbase = hak->code.lit.len - hak->c->funblk.info[hak->c->funblk.depth - 1].lfbase;
		if (lfbase > MAX_CODE_PARAM2)
		{
			hak_seterrbfmt(hak, HAK_ERANGE, "literal frame base too large");
			return -1;
		}

		if (emit_two_param_instruction(hak, HAK_CODE_MAKE_FUNCTION, 0, 0, HAK_CNODE_GET_LOC(cmd)) <= -1) return -1;
		if (emit_extended_long_param(hak, fun_name_lfindex) <= -1) return -1;
		if (emit_long_param(hak, lfbase) <= -1) return -1; /* lfbase(literal frame base) */
		lfsize_pos = hak->code.bc.len; /* literal frame size */
		if (emit_long_param(hak, 0) <= -1) return -1; /* place holder for lfsize */
	}
	else
	{
		/* MAKE_BLOCK attr_mask_1 attr_mask_2 - will patch attr_mask in pop_funblk() */
		if (emit_two_param_instruction(hak, HAK_CODE_MAKE_BLOCK, 0, 0, HAK_CNODE_GET_LOC(cmd)) <= -1) return -1;
		if (emit_extended_long_param(hak, fun_name_lfindex) <= -1) return -1;
	}

	HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX);  /* guaranteed in emit_byte_instruction() */
	jump_inst_pos = hak->code.bc.len;
	/* specifying MAX_CODE_JUMP causes emit_one_param_instruction() to
	 * produce the long jump instruction (HAK_CODE_JUMP_FORWARD_X) */
	if (emit_one_param_instruction(hak, HAK_CODE_JUMP_FORWARD_0, MAX_CODE_JUMP, HAK_CNODE_GET_LOC(cmd)) <= -1) return -1;

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT_LIST, fun_body); /* 1 */
	PUSH_SUBCFRAME(hak, COP_POST_FUN, fun_name); /* 3*/
	cf = GET_SUBCFRAME(hak);
	cf->u.fun.fun_type = fun_type;
	cf->u.fun.class_name = class_name;
	cf->u.fun.jump_inst_pos = jump_inst_pos;
	if (hak->option.trait & HAK_TRAIT_INTERACTIVE) cf->u.fun.lfsize_pos = lfsize_pos;

	PUSH_SUBCFRAME(hak, COP_EMIT_FUN, src); /* 2 */
	cf = GET_SUBCFRAME(hak);
	cf->u.fun.fun_type = fun_type;
	cf->u.fun.class_name = class_name;
	cf->u.fun.jump_inst_pos = jump_inst_pos;
	if (hak->option.trait & HAK_TRAIT_INTERACTIVE) cf->u.fun.lfsize_pos = lfsize_pos;

	return 0;
}
/* ========================================================================= */

static int check_var_attr_list (hak_t* hak, hak_cnode_t* attr_list, unsigned int* var_type, hak_cnode_t* cmd, hak_cnode_t* class_name, hak_cnode_t* var_name)
{
	unsigned int ft;

	ft = VAR_INST;

	HAK_ASSERT(hak, attr_list != HAK_NULL);
	HAK_ASSERT(hak, HAK_CNODE_IS_CONS_CONCODED(attr_list, HAK_CONCODE_XLIST) ||
	                 HAK_CNODE_IS_ELIST_CONCODED(attr_list, HAK_CONCODE_XLIST));

	if (HAK_CNODE_IS_ELIST_CONCODED(attr_list, HAK_CONCODE_XLIST))
	{
		/* don't allow empty attribute list */
		if (class_name)
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_VAR, HAK_CNODE_GET_LOC(attr_list), HAK_NULL,
				"empty attribute list on '%.*js' in '%.*js' for '%.*js'",
				HAK_CNODE_GET_TOKLEN(var_name), HAK_CNODE_GET_TOKPTR(var_name),
				HAK_CNODE_GET_TOKLEN(class_name), HAK_CNODE_GET_TOKPTR(class_name),
				HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		}
		else
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_VAR, HAK_CNODE_GET_LOC(attr_list), HAK_NULL,
				"empty attribute list on '%.*js' in unnamed class for '%.*js'",
				HAK_CNODE_GET_TOKLEN(var_name), HAK_CNODE_GET_TOKPTR(var_name),
				HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		}
		return -1;
	}

	if (HAK_CNODE_IS_CONS_CONCODED(attr_list, HAK_CONCODE_XLIST))
	{
		hak_cnode_t* c, * a;
		const hak_ooch_t* tokptr;
		hak_oow_t toklen;

		c = attr_list;
		while (c)
		{
			a = HAK_CNODE_CONS_CAR(c);

			tokptr = HAK_CNODE_GET_TOKPTR(a);
			toklen = HAK_CNODE_GET_TOKLEN(a);

			if (!HAK_CNODE_IS_TYPED(a, HAK_CNODE_SYMLIT))
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_VAR, HAK_CNODE_GET_LOC(a), HAK_NULL,
					"invalid variable attribute name '%.*js'", toklen, tokptr);
				return -1;
			}

			if (hak_comp_oochars_bcstr(tokptr, toklen, "class") == 0 ||
			    hak_comp_oochars_bcstr(tokptr, toklen, "c") == 0)
			{
				if (ft != VAR_INST)
				{
				conflicting:
					hak_setsynerrbfmt(
						hak, HAK_SYNERR_VAR, HAK_CNODE_GET_LOC(a), HAK_NULL,
						"conflicting variable attribute name '#%.*js'", toklen, tokptr);
					return -1;
				}
				ft = VAR_CLASS_I;
			}
			else
			{
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_VAR, HAK_CNODE_GET_LOC(a), HAK_NULL,
					"unrecognized variable attribute name '#%.*js'", toklen, tokptr);
				return -1;
			}

			c = HAK_CNODE_CONS_CDR(c);
		}
	}

	*var_type = ft;
	return 0;
}

static int compile_var (hak_t* hak, hak_cnode_t* src)
{
	hak_cnode_t* cmd, * next, * tmp;
	hak_cnode_t* attr_list;
	hak_clsblk_info_t* cbi;

	/* this is for instance/class variable declaration
	 * inside the class body block */

	cmd = HAK_CNODE_CONS_CAR(src);
	next = HAK_CNODE_CONS_CDR(src);
	attr_list = HAK_NULL;

/*TODO: put some assertion regarding funblk to cslblk relaion */
	cbi = &hak->c->clsblk.info[hak->c->clsblk.depth];

	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(cmd, HAK_CNODE_VAR));

	if (!next)
	{
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_VAR, HAK_CNODE_GET_LOC(cmd), HAK_NULL,
			"'%.*js' not followed by name or (",
			HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		goto oops;
	}

	/* the reader ensures that the cdr field of a cons cell points to the next cell.
	 * and only the field of the last cons cell is NULL. */
	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(next));

	if (!is_in_class_init_scope(hak))
	{
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_BANNED, HAK_CNODE_GET_LOC(cmd), HAK_NULL,
			"'%.*js' prohibited in this context",
			HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		goto oops;
	}

	tmp = HAK_CNODE_CONS_CAR(next);
	if (HAK_CNODE_IS_CONS_CONCODED(tmp, HAK_CONCODE_XLIST) ||
	    HAK_CNODE_IS_ELIST_CONCODED(tmp, HAK_CONCODE_XLIST))
	{
		/* probably attribute list */
		attr_list = tmp;

		next = HAK_CNODE_CONS_CDR(next);
		if (!next)
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_VAR, HAK_CNODE_GET_LOC(attr_list), HAK_NULL,
				"no name after attribute list for '%.*js'",
				HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
			goto oops;
		}

		tmp = HAK_CNODE_CONS_CAR(next);
	}

	if (HAK_CNODE_IS_SYMBOL_IDENT(tmp))
	{
		unsigned int var_type = VAR_INST;

		if (attr_list && check_var_attr_list(hak, attr_list, &var_type, cmd, cbi->class_name, tmp) <= -1) goto oops;

		HAK_ASSERT(hak, var_type == VAR_INST || var_type == VAR_CLASS_I);
		while (1)
		{
			if (var_type == VAR_INST)
			{
				if (cbi->nivars >= MAX_NIVARS)
				{
					hak_setsynerrbfmt(
						hak, HAK_SYNERR_VARFLOOD, HAK_CNODE_GET_LOC(tmp), HAK_NULL,
						"too many(%zu) instance variables before '%.*js'",
						cbi->nivars, HAK_CNODE_GET_TOKLEN(tmp), HAK_CNODE_GET_TOKPTR(tmp));
					goto oops;
				}

				if (add_class_level_variable(hak, &cbi->ivars, &cbi->cvars, tmp, "instance") <= -1) goto oops;
				cbi->nivars++;
			}
			else
			{
				if (cbi->ncvars >= MAX_NCVARS)
				{
					hak_setsynerrbfmt(
						hak, HAK_SYNERR_VARFLOOD, HAK_CNODE_GET_LOC(tmp), HAK_NULL,
						"too many(%zu) class variables before '%.*js'",
						cbi->ncvars, HAK_CNODE_GET_TOKLEN(tmp), HAK_CNODE_GET_TOKPTR(tmp));
					goto oops;
				}

				if (add_class_level_variable(hak, &cbi->cvars, &cbi->ivars, tmp, "class") <= -1) goto oops;
				cbi->ncvars++;
			}

			next = HAK_CNODE_CONS_CDR(next);
			if (!next) break;
			tmp = HAK_CNODE_CONS_CAR(next);
			if (!HAK_CNODE_IS_SYMBOL_IDENT(tmp)) goto not_ident;
		}
	}
	else
	{
	not_ident:
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_VAR, HAK_CNODE_GET_LOC(tmp), HAK_NULL,
			"invalid variable name '%.*js' for '%.*js'",
			HAK_CNODE_GET_TOKLEN(tmp), HAK_CNODE_GET_TOKPTR(tmp),
			HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		goto oops;
	}

/* if there is assignment with expression, we can arragne to compile RHS. in that case
 * the explicit PUSH_NIL here isn't needed */
	/* TODO: hack for POP_STACKPOP generated in compile_object_list().
	 *       remove generating this instruction after having fixed the problem in that function */
	if (emit_byte_instruction(hak, HAK_CODE_PUSH_NIL, HAK_CNODE_GET_LOC(cmd)) <= -1) return -1;

	POP_CFRAME(hak);
	return 0;


oops:
	return -1;
}

static int compile_return (hak_t* hak, hak_cnode_t* src, int ret_from_home)
{
	hak_cnode_t* obj, * val;
	hak_cframe_t* cf;
	hak_funblk_info_t* fbi;
	hak_ooi_t i;

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));
	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_RETURN) ||
	                 HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_REVERT));

	fbi = &hak->c->funblk.info[hak->c->funblk.depth];
	obj = HAK_CNODE_CONS_CDR(src);

	for (i = hak->c->ctlblk.depth; i > hak->c->funblk.info[hak->c->funblk.depth].ctlblk_base; --i)
	{
		switch (hak->c->ctlblk.info[i]._type)
		{
			case HAK_CTLBLK_TYPE_LOOP:
				/* do nothing */
				break;

			case HAK_CTLBLK_TYPE_TRY:
				if (emit_byte_instruction(hak, HAK_CODE_TRY_EXIT, HAK_CNODE_GET_LOC(src)) <= -1) return -1;
				break;

			case HAK_CTLBLK_TYPE_CLASS:
				if (emit_byte_instruction(hak, HAK_CODE_CLASS_EXIT, HAK_CNODE_GET_LOC(src)) <= -1) return -1;
				break;
		}
	}

	if (fbi->tmpr_nrvars > 0)
	{
		hak_cnode_t* tmp = HAK_CNODE_CONS_CAR(src);

		if (ret_from_home)
		{
			hak_setsynerrbfmt(
				hak, HAK_SYNERR_BANNED, HAK_CNODE_GET_LOC(src), HAK_NULL,
				"%.*js not compatible with return variables",
				HAK_CNODE_GET_TOKLEN(tmp), HAK_CNODE_GET_TOKPTR(tmp));
			return -1;
		}

		/* if a return variable are specified in the current function block, the return statement must not be followed by a return value */
		if (obj)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_BANNED, HAK_CNODE_GET_LOC(src), HAK_NULL, "use of return value in %.*js not compatible with return variables", HAK_CNODE_GET_TOKLEN(tmp), HAK_CNODE_GET_TOKPTR(tmp));
			return -1;
		}

/* TODO: pop stack if this is not the first statement... */
		if (emit_byte_instruction(hak, HAK_CODE_PUSH_RETURN_R, HAK_CNODE_GET_LOC(tmp)) <= -1) return -1;
		POP_CFRAME(hak);
	}
	else
	{
		if (!obj)
		{
	/* TODO: should i allow (return)? does it return the last value on the stack? */
			/* no value */
			hak_cnode_t* tmp = HAK_CNODE_CONS_CAR(src);
			hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(src), HAK_NULL, "no value specified in %.*js", HAK_CNODE_GET_TOKLEN(tmp), HAK_CNODE_GET_TOKPTR(tmp));
			return -1;
		}
		else if (!HAK_CNODE_IS_CONS(obj))
		{
			hak_cnode_t* tmp = HAK_CNODE_CONS_CAR(src);
			hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HAK_CNODE_GET_TOKLEN(tmp), HAK_CNODE_GET_TOKPTR(tmp));
			return -1;
		}

		val = HAK_CNODE_CONS_CAR(obj);

		obj = HAK_CNODE_CONS_CDR(obj);
		if (obj)
		{
			hak_cnode_t* tmp = HAK_CNODE_CONS_CAR(src);
			hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "more than 1 argument in %.*js", HAK_CNODE_GET_TOKLEN(tmp), HAK_CNODE_GET_TOKPTR(tmp));
			return -1;
		}

		SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, val);

		PUSH_SUBCFRAME(hak, COP_EMIT_RETURN, src);
		cf = GET_SUBCFRAME(hak);
		cf->u._return.from_home = ret_from_home;
	}

	return 0;
}

static int compile_set (hak_t* hak, hak_cnode_t* src)
{
	hak_cframe_t* cf;
	hak_cnode_t* cmd, * obj, * var, * val;
	hak_var_info_t vi;
	int x;

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));
	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_SET));

	cmd = HAK_CNODE_CONS_CAR(src);
	obj = HAK_CNODE_CONS_CDR(src);

	if (!obj)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_VARNAME, HAK_CNODE_GET_LOC(src), HAK_NULL, "no variable name in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	var = HAK_CNODE_CONS_CAR(obj);
	if (!HAK_CNODE_IS_SYMBOL(var) && !HAK_CNODE_IS_DSYMBOL_CLA(var))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_VARNAME, HAK_CNODE_GET_LOC(var), HAK_CNODE_GET_TOK(var), "variable name not symbol in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	obj = HAK_CNODE_CONS_CDR(obj);
	if (!obj)
	{
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(var), HAK_NULL,
			"no value after '%.*js' for '%.*js'",
			HAK_CNODE_GET_TOKLEN(var), HAK_CNODE_GET_TOKPTR(var),
			HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	val = HAK_CNODE_CONS_CAR(obj);

	obj = HAK_CNODE_CONS_CDR(obj);
	if (obj)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "too many arguments to %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, val);

	x = find_variable_backward_with_token(hak, var, &vi);
	if (x <= -1) return -1;

	if (x == 0)
	{
		if (HAK_CNODE_IS_DSYMBOL_CLA(var))
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_VARNAMEUNKNOWN, HAK_CNODE_GET_LOC(var), HAK_CNODE_GET_TOK(var), "unknown class-level variable name", HAK_CNODE_GET_TOKLEN(var), HAK_CNODE_GET_TOKPTR(var));
			return -1;

		}

		PUSH_SUBCFRAME(hak, COP_EMIT_SET, var); /* set doesn't evaluate the variable name */
		cf = GET_SUBCFRAME(hak);
		cf->u.set.vi.type = VAR_NAMED;
	}
	else
	{
		/* the check in compile_fun() must ensure this condition */
		PUSH_SUBCFRAME(hak, COP_EMIT_SET, cmd);
		cf = GET_SUBCFRAME(hak);
		cf->u.set.vi = vi;
	}
	cf->u.set.mode = VAR_ACCESS_STORE;

	return 0;
}

static int compile_set_r (hak_t* hak, hak_cnode_t* src)
{
	hak_cframe_t* cf;
	hak_cnode_t* cmd, * obj, * var, * val, * var_start;
	hak_oow_t nvars, i;
	hak_var_info_t vi;

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));
	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_SET_R));

	cmd = HAK_CNODE_CONS_CAR(src);
	obj = HAK_CNODE_CONS_CDR(src);

	if (!obj)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_VARNAME, HAK_CNODE_GET_LOC(src), HAK_NULL, "no variable name in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	nvars = 0;
	var_start = obj;
	do
	{
		var = HAK_CNODE_CONS_CAR(obj);
		if (!HAK_CNODE_IS_SYMBOL(var)) /* TODO: should this be HAK_CNODE_IS_SYMBOL(var)?? */
		{
			if (nvars > 0) break;
			hak_setsynerrbfmt(hak, HAK_SYNERR_VARNAME, HAK_CNODE_GET_LOC(var), HAK_CNODE_GET_TOK(var), "variable name not symbol in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
			return -1;
		}

		nvars++;
		obj = HAK_CNODE_CONS_CDR(obj);
	}
	while (obj);


	if (!obj)
	{
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(var), HAK_NULL,
			"no value after '%.*js' for '%.*js'",
			HAK_CNODE_GET_TOKLEN(var), HAK_CNODE_GET_TOKPTR(var),
			HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	val = HAK_CNODE_CONS_CAR(obj);

	obj = HAK_CNODE_CONS_CDR(obj);
	if (obj)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "too many arguments to %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT_R, val); /* special for set_r */
	cf = GET_TOP_CFRAME(hak);
	cf->u.obj_r.nrets = nvars; /* number of return variables to get assigned */

	for (i = 0, obj = var_start; i < nvars; i++, obj = HAK_CNODE_CONS_CDR(obj))
	{
		int x;

		var = HAK_CNODE_CONS_CAR(obj);

		x = find_variable_backward_with_token(hak, var, &vi);
		if (x <= -1) return -1;

		if (x == 0)
		{
			if (HAK_CNODE_IS_DSYMBOL_CLA(var))
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_VARNAMEUNKNOWN, HAK_CNODE_GET_LOC(var), HAK_CNODE_GET_TOK(var), "unknown class-level variable name", HAK_CNODE_GET_TOKLEN(var), HAK_CNODE_GET_TOKPTR(var));
				return -1;
			}

			PUSH_SUBCFRAME(hak, COP_EMIT_SET, var); /* set_r doesn't evaluate the variable name */
			cf = GET_SUBCFRAME(hak);
			cf->u.set.vi.type = VAR_NAMED;
		}
		else
		{
			PUSH_SUBCFRAME(hak, COP_EMIT_SET, cmd);
			cf = GET_SUBCFRAME(hak);
			cf->u.set.vi = vi;
		}

		/*
		 * (fun f(x y ::: aa bb cc) ....)
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

static int compile_try (hak_t* hak, hak_cnode_t* src)
{
	hak_cnode_t* cmd, * obj;
	hak_cframe_t* cf;
	hak_ooi_t jump_inst_pos;

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));

	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_TRY));

	/* (try
	 *   (perform this)
	 *   (perform that)
	 *   (throw 10)
	 *  catch (x)
	 *   (perform xxx)
	 *   (perform yyy)
	 * )
	 */
	cmd = HAK_CNODE_CONS_CAR(src);
	obj = HAK_CNODE_CONS_CDR(src);

	if (!obj)
	{
		/* no value */
		hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(src), HAK_NULL, "no expression specified in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	if (push_ctlblk(hak, HAK_CNODE_GET_LOC(src), HAK_CTLBLK_TYPE_TRY) <= -1) return -1;

/* TODO: HAK_TRAIT_INTERACTIVE??? */

	jump_inst_pos = hak->code.bc.len;
	if (emit_one_param_instruction(hak, HAK_CODE_TRY_ENTER, MAX_CODE_JUMP, HAK_CNODE_GET_LOC(cmd)) <= -1) return -1;

	if (check_block_expression_as_body(hak, obj, cmd, FOR_TRY) <= -1) return -1;

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_TRY_OBJECT_LIST, obj);  /* 1*/
	PUSH_SUBCFRAME(hak, COP_POST_TRY, cmd); /* 2 */
	cf = GET_SUBCFRAME(hak);
	cf->u.post_try.jump_inst_pos = jump_inst_pos;

	return 0;
}

static HAK_INLINE int patch_nearest_post_try (hak_t* hak, hak_ooi_t* catch_skip_jip)
{
	hak_ooi_t jip, block_code_size;
	hak_cframe_t* cf;

	cf = find_cframe_from_top(hak, COP_POST_TRY);
	HAK_ASSERT(hak, cf != HAK_NULL);
	HAK_ASSERT(hak, cf->opcode == COP_POST_TRY);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	jip = cf->u.post_try.jump_inst_pos;

	/* HAK_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD instruction */
	block_code_size = hak->code.bc.len - jip - (HAK_CODE_LONG_PARAM_SIZE + 1);

	if (block_code_size == 0)
	{
		/* no body in try */
/* TODO: is this correct??? */
		if (emit_byte_instruction(hak, HAK_CODE_PUSH_NIL, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;
	}

	if (emit_byte_instruction(hak, HAK_CODE_TRY_EXIT, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	*catch_skip_jip = hak->code.bc.len;
	if (emit_one_param_instruction(hak, HAK_CODE_JUMP_FORWARD_0,  MAX_CODE_JUMP, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	/* HAK_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD instruction */
	block_code_size = hak->code.bc.len - jip - (HAK_CODE_LONG_PARAM_SIZE + 1);

	if (block_code_size > MAX_CODE_JUMP * 2)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_BLKFLOOD, HAK_CNODE_GET_LOC(cf->operand), HAK_NULL, "code too big - size %zu", block_code_size);
		return -1;
	}
	patch_long_jump(hak, jip, block_code_size); /* patch TRY_ENTER */

	return 0;
}

static HAK_INLINE int compile_catch (hak_t* hak)
{
	hak_cnode_t* cmd, * obj, * src, * exarg;
	hak_cframe_t* cf;
	hak_ooi_t jump_inst_pos;
	hak_oow_t exarg_offset;
	hak_var_info_t vi;
	hak_funblk_info_t* fbi;
	hak_oow_t par_tmprcnt;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_CATCH);

	src = cf->operand;
	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));
	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_CATCH));

	cmd = HAK_CNODE_CONS_CAR(src);
	obj = HAK_CNODE_CONS_CDR(src);

	if (!obj)
	{
/* TODO: change error code */
		hak_setsynerrbfmt(hak, HAK_SYNERR_VARNAME, HAK_CNODE_GET_LOC(src), HAK_NULL, "no exception variable for '%.*js'", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	exarg = HAK_CNODE_CONS_CAR(obj);
	if (!HAK_CNODE_IS_CONS_CONCODED(exarg, HAK_CONCODE_XLIST) || hak_countcnodecons(hak, exarg) != 1)
	{
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_VARNAME, HAK_CNODE_GET_LOC(exarg), HAK_NULL,
			"improper exception variable for '%.*js'",
			HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	exarg = HAK_CNODE_CONS_CAR(exarg);
	if (!HAK_CNODE_IS_SYMBOL(exarg))
	{
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_VARNAME, HAK_CNODE_GET_LOC(exarg), HAK_NULL,
			"invalid exception variable name '%.*js' for '%.*js'",
			HAK_CNODE_GET_TOKLEN(exarg), HAK_CNODE_GET_TOKPTR(exarg),
			HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	/* add the exception variable to the local variable list. increase the number of local variables */
	exarg_offset = hak->c->tv.s.len;
	if (hak->c->tv.s.len > 0) exarg_offset++; /* if the variable is not the first, add 1 for a preceding space */

	if (hak->c->funblk.depth > 0)
	{
		fbi = &hak->c->funblk.info[hak->c->funblk.depth - 1]; /* parent block */
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
	vi.index_in_ctx = hak->c->tv.wcount - par_tmprcnt;
	if (add_temporary_variable(hak, exarg, hak->c->tv.s.len, "exception variable", HAK_NULL) <= -1) return -1;

	fbi = &hak->c->funblk.info[hak->c->funblk.depth];
	/* a variable string inserts a space(' ') between variable name.
	 * if 'exarg' is the first variable in the variable string, there is no preceding space.
	 * if it is not, there is a preceding space. */
	HAK_ASSERT(hak, (hak->c->tv.s.len == HAK_CNODE_GET_TOKLEN(exarg) && fbi->tmprlen == hak->c->tv.s.len - HAK_CNODE_GET_TOKLEN(exarg)) || /* first */
	                 (hak->c->tv.s.len > HAK_CNODE_GET_TOKLEN(exarg) && fbi->tmprlen == hak->c->tv.s.len - HAK_CNODE_GET_TOKLEN(exarg) - 1)); /* not first */
	HAK_ASSERT(hak, fbi->tmprcnt == vi.index_in_ctx + par_tmprcnt);
	fbi->tmprlen = hak->c->tv.s.len;
	fbi->tmprcnt = hak->c->tv.wcount;
	fbi->tmpr_nlvars = fbi->tmpr_nlvars + 1;

	HAK_ASSERT(hak, fbi->tmpr_nargs + fbi->tmpr_nrvars + fbi->tmpr_nlvars == fbi->tmprcnt - par_tmprcnt);

	obj = HAK_CNODE_CONS_CDR(obj);
	if (check_block_expression_as_body(hak, obj, cmd, FOR_NONE) <= -1) return -1;

	/* jump_inst_pos hold the instruction pointer that skips the catch block at the end of the try block */
	patch_nearest_post_try(hak, &jump_inst_pos);

	/* produce an instruction to store the exception value to an exception variable pushed by the 'throw' instruction */
	if (emit_variable_access(hak, VAR_ACCESS_POP, &vi, HAK_CNODE_GET_LOC(src)) <= -1) return -1;

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT_LIST, obj);

	PUSH_SUBCFRAME(hak, COP_POST_CATCH, cmd);
	cf = GET_SUBCFRAME(hak);
	cf->u.post_catch.jump_inst_pos = jump_inst_pos;
	cf->u.post_catch.exarg_offset = exarg_offset; /* there is only 1 exception variable. using the offset is easier than to use the variable position */

	return 0;
}

static HAK_INLINE int post_try (hak_t* hak)
{
/* TODO: anything else? */
	pop_ctlblk(hak);
	POP_CFRAME(hak);
	return 0;
}

static HAK_INLINE int post_catch (hak_t* hak)
{
	hak_ooi_t jip, block_code_size;
	hak_cframe_t* cf;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf != HAK_NULL);
	HAK_ASSERT(hak, cf->opcode == COP_POST_CATCH);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	jip = cf->u.post_catch.jump_inst_pos; /* jump instruction position between the try block and the catch block */

	/* HAK_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD instruction */
	block_code_size = hak->code.bc.len - jip - (HAK_CODE_LONG_PARAM_SIZE + 1);

	if (block_code_size == 0)
	{
		/* no body in try */
/* TODO: is this correct??? */
		if (emit_byte_instruction(hak, HAK_CODE_PUSH_NIL, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;
		block_code_size++;
	}

	if (block_code_size > MAX_CODE_JUMP * 2)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_BLKFLOOD, HAK_CNODE_GET_LOC(cf->operand), HAK_NULL, "code too big - size %zu", block_code_size);
		return -1;
	}

	patch_long_jump(hak, jip, block_code_size); /* patch the jump between the try block and the catch block */

	/* make the exception variable unsearchable outside the catch block.
	 * the variable entity is still be accounted into the local variable list. */
	kill_temporary_variable_at_offset(hak, cf->u.post_catch.exarg_offset);

	POP_CFRAME(hak);
	return 0;
}

static int compile_throw (hak_t* hak, hak_cnode_t* src)
{
	hak_cnode_t* obj, * val;
	/*hak_cframe_t* cf;*/

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));
	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_THROW));

	obj = HAK_CNODE_CONS_CDR(src);

	if (!obj)
	{
/* TODO: should i allow (throw)? does it return the last value on the stack? */
		/* no value */
		hak_cnode_t* tmp = HAK_CNODE_CONS_CAR(src);
		hak_setsynerrbfmt(
			hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(src), HAK_NULL,
			"no value or expression after '%.*js'",
			HAK_CNODE_GET_TOKLEN(tmp), HAK_CNODE_GET_TOKPTR(tmp));
		return -1;
	}
	else if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_cnode_t* tmp = HAK_CNODE_CONS_CAR(src);
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in %.*js", HAK_CNODE_GET_TOKLEN(tmp), HAK_CNODE_GET_TOKPTR(tmp));
		return -1;
	}

	val = HAK_CNODE_CONS_CAR(obj);

	obj = HAK_CNODE_CONS_CDR(obj);
	if (obj)
	{
		hak_cnode_t* tmp = HAK_CNODE_CONS_CAR(src);
		hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "more than 1 argument in %.*js", HAK_CNODE_GET_TOKLEN(tmp), HAK_CNODE_GET_TOKPTR(tmp));
		return -1;
	}

	/* throw can be located anywhere, however,
	 * if there is no outer try-catch, it ends up with a fatal runtime error */
	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, val);

	PUSH_SUBCFRAME(hak, COP_EMIT_THROW, src);
	/*cf = GET_SUBCFRAME(hak);*/
	return 0;
}

/* ========================================================================= */

static int compile_while (hak_t* hak, hak_cnode_t* src, int next_cop)
{
	/* (while (xxxx) ... )
	 * (until (xxxx) ... ) */
	hak_cnode_t* cmd, * obj, * cond, * body;
	hak_oow_t cond_pos;
	hak_cframe_t* cf;

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS(src));
	HAK_ASSERT(hak, HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_UNTIL) ||
	                 HAK_CNODE_IS_TYPED(HAK_CNODE_CONS_CAR(src), HAK_CNODE_WHILE));
	HAK_ASSERT(hak, next_cop == COP_POST_UNTIL_COND || next_cop == COP_POST_WHILE_COND);

	cmd = HAK_CNODE_CONS_CAR(src); /* while or until itself */
	obj = HAK_CNODE_CONS_CDR(src);

	if (!obj)
	{
		/* no value */
		hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(src), HAK_NULL, "no loop condition specified in %.*js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}
	else if (!HAK_CNODE_IS_CONS(obj))
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "redundant cdr in %*.js", HAK_CNODE_GET_TOKLEN(cmd), HAK_CNODE_GET_TOKPTR(cmd));
		return -1;
	}

	if (push_ctlblk(hak, HAK_CNODE_GET_LOC(src), HAK_CTLBLK_TYPE_LOOP) <= -1) return -1;

	HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX);
	cond_pos = hak->code.bc.len; /* position where the bytecode for the conditional is emitted */

	cond = HAK_CNODE_CONS_CAR(obj);
	body = HAK_CNODE_CONS_CDR(obj);

	if (check_block_expression_as_body(hak, body, cmd, FOR_NONE) <= -1) return -1;

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, cond); /* 1 */

	/* pass the cons cell branching to the conditional and the body. see post_while_cond() for the reason */
	PUSH_SUBCFRAME(hak, next_cop, obj); /* 2 */
	cf = GET_SUBCFRAME(hak);
	cf->u.post_while.cond_pos = cond_pos;
	cf->u.post_while.body_pos = -1; /* unknown yet*/
	cf->u.post_while.jump_inst_pos = -1; /* not needed */
	cf->u.post_while.start_loc = *HAK_CNODE_GET_LOC(src);

	return 0;
}

/* ========================================================================= */

static int compile_cons_array_expression (hak_t* hak, hak_cnode_t* obj)
{
	/* #[ ] */
	hak_ooi_t nargs;
	hak_cframe_t* cf;

	nargs = hak_countcnodecons(hak, obj);
	if (nargs > MAX_CODE_PARAM)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_ARGFLOOD, HAK_CNODE_GET_LOC(obj), HAK_NULL, "too many(%zd) elements in array", nargs);
		return -1;
	}

	SWITCH_TOP_CFRAME(hak, COP_EMIT_MAKE_ARRAY, obj);
	cf = GET_TOP_CFRAME(hak);
	cf->u.array_list.index = nargs;

	/* redundant cdr check is performed inside compile_object_list() */
	PUSH_SUBCFRAME(hak, COP_COMPILE_ARRAY_LIST, obj);
	cf = GET_SUBCFRAME(hak);
	cf->u.array_list.index = 0;

	return 0;
}

static int compile_cons_bytearray_expression (hak_t* hak, hak_cnode_t* obj, int concode)
{
	/* compile the singular-type array such as byte array or char array */
	/* #b[ ] - e.g. #b[1, 2, 3] or #b[ 1 2 3 ] */
	hak_ooi_t nargs;
	hak_cframe_t* cf;

	nargs = hak_countcnodecons(hak, obj);
	if (nargs > MAX_CODE_PARAM)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_ARGFLOOD, HAK_CNODE_GET_LOC(obj), HAK_NULL, "too many(%zd) elements in byte-array", nargs);
		return -1;
	}

	SWITCH_TOP_CFRAME(hak, COP_EMIT_MAKE_PURE_ARRAY, obj);
	cf = GET_TOP_CFRAME(hak);
	cf->u.pure_array_list.elem_type = concode;
	cf->u.pure_array_list.index = nargs;

	/* redundant cdr check is performed inside compile_object_list() */
	PUSH_SUBCFRAME(hak, COP_COMPILE_PURE_ARRAY_LIST, obj);
	cf = GET_SUBCFRAME(hak);
	cf->u.pure_array_list.elem_type = concode;
	cf->u.pure_array_list.index = 0;

	return 0;
}

static int compile_cons_dic_expression (hak_t* hak, hak_cnode_t* obj)
{
	/* { } - e.g. {1:2, 3:4,"abc":def, "hwaddr":"00:00:00:01"} or { 1 2 3 4 } */
	hak_ooi_t nargs;
	hak_cframe_t* cf;

	nargs = hak_countcnodecons(hak, obj);
	if (nargs > MAX_CODE_PARAM)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_ARGFLOOD, HAK_CNODE_GET_LOC(obj), HAK_NULL, "too many(%zd) elements in dictionary", nargs);
		return -1;
	}

	SWITCH_TOP_CFRAME(hak, COP_EMIT_MAKE_DIC, obj);
	cf = GET_TOP_CFRAME(hak);
	cf->u.dic_list.index = nargs >> 1; /* only the half */

	/* redundant cdr check is performed inside compile_object_list() */
	PUSH_SUBCFRAME(hak, COP_COMPILE_DIC_LIST, obj);

	return 0;
}

static int compile_cons_qlist_expression (hak_t* hak, hak_cnode_t* obj)
{
	/* #( 1 2  3 )
	 * #(1 (+ 2 3) 5) --> #(1 5 5)
	 * */
	SWITCH_TOP_CFRAME(hak, COP_EMIT_MAKE_CONS, obj);
	PUSH_SUBCFRAME(hak, COP_COMPILE_QLIST, obj);
	return 0;
}

static int compile_cons_alist_expression (hak_t* hak, hak_cnode_t* cmd)
{
	/* assignment expression */
	/* (a := 20)
	 * ([a,b] := (xxx 20))
	 */

	hak_cframe_t* cf;
	hak_cnode_t* obj, * var, * val;
	hak_var_info_t vi;
	int x;

	HAK_ASSERT(hak, HAK_CNODE_IS_CONS_CONCODED(cmd, HAK_CONCODE_ALIST));

	var = HAK_CNODE_CONS_CAR(cmd);
	obj = HAK_CNODE_CONS_CDR(cmd);

	HAK_ASSERT(hak, HAK_CNODE_IS_SYMBOL(var) || HAK_CNODE_IS_DSYMBOL_CLA(var) || HAK_CNODE_IS_CONS_CONCODED(var, HAK_CONCODE_TUPLE));
	HAK_ASSERT(hak, obj && HAK_CNODE_IS_CONS(obj)); /* reader guaranteed */

	val = HAK_CNODE_CONS_CAR(obj);
	HAK_ASSERT(hak, HAK_CNODE_CONS_CDR(obj) ==  HAK_NULL); /* reader guaranteed */

	if (HAK_CNODE_IS_CONS_CONCODED(var, HAK_CONCODE_TUPLE))
	{
		/* multi-variable assignment
		 * fun xxx(x :: p q) { p := x + 1; q := x + 2 }
		 * ([a,b] := (xxx 20)) */
		hak_oow_t nvars, i;

		nvars = hak_countcnodecons(hak, var);

		SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT_R, val); /* special for set_r */
		cf = GET_TOP_CFRAME(hak);
		cf->u.obj_r.nrets = nvars; /* number of return variables to get assigned */

		for (i = 0, obj = var; i < nvars; i++, obj = HAK_CNODE_CONS_CDR(obj))
		{
			int x;

			var = HAK_CNODE_CONS_CAR(obj);

			HAK_ASSERT(hak, HAK_CNODE_IS_SYMBOL(var) || HAK_CNODE_IS_DSYMBOL_CLA(var)); /* reader guaranteed */

			x = find_variable_backward_with_token(hak, var, &vi);
			if (x <= -1) return -1;

			if (x == 0)
			{
				if (HAK_CNODE_IS_DSYMBOL_CLA(var))
				{
					hak_setsynerrbfmt(hak,
						HAK_SYNERR_VARNAMEUNKNOWN, HAK_CNODE_GET_LOC(var), HAK_CNODE_GET_TOK(var),
						"unknown class-level variable name", HAK_CNODE_GET_TOKLEN(var), HAK_CNODE_GET_TOKPTR(var));
					return -1;
				}

				PUSH_SUBCFRAME(hak, COP_EMIT_SET, var); /* set_r doesn't evaluate the variable name */
				cf = GET_SUBCFRAME(hak);
				cf->u.set.vi.type = VAR_NAMED;
			}
			else
			{
				PUSH_SUBCFRAME(hak, COP_EMIT_SET, cmd);
				cf = GET_SUBCFRAME(hak);
				cf->u.set.vi = vi;
			}

			/*
			 * (fun f(x y ::: aa bb cc) ....)
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

		SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, val);

		x = find_variable_backward_with_token(hak, var, &vi);
		if (x <= -1) return -1;

		if (x == 0)
		{
			/* not found */
			if (HAK_CNODE_IS_DSYMBOL_CLA(var))
			{
				hak_setsynerrbfmt(hak,
					HAK_SYNERR_VARNAMEUNKNOWN, HAK_CNODE_GET_LOC(var), HAK_CNODE_GET_TOK(var),
					"unknown class-level variable name", HAK_CNODE_GET_TOKLEN(var), HAK_CNODE_GET_TOKPTR(var));
				return -1;

			}

			PUSH_SUBCFRAME(hak, COP_EMIT_SET, var); /* set doesn't evaluate the variable name */
			cf = GET_SUBCFRAME(hak);
			cf->u.set.vi.type = VAR_NAMED;
		}
		else
		{
			/* the check in compile_fun() must ensure this condition */
			PUSH_SUBCFRAME(hak, COP_EMIT_SET, cmd);
			cf = GET_SUBCFRAME(hak);
			cf->u.set.vi = vi;
		}
		cf->u.set.mode = VAR_ACCESS_STORE;
	}

	return 0;
}

static int compile_cons_xlist_expression (hak_t* hak, hak_cnode_t* obj, int nrets)
{
	hak_cnode_t* car;

	/* a valid function call
	 * (function-name argument-list)
	 *   function-name can be:
	 *     a symbol.
	 *     another function call.
	 * if the name is another function call, i can't know if the
	 * function name will be valid at the compile time.
	 */
	HAK_ASSERT(hak, HAK_CNODE_IS_CONS_CONCODED(obj, HAK_CONCODE_XLIST));

	car = HAK_CNODE_CONS_CAR(obj);
	switch (HAK_CNODE_GET_TYPE(car))
	{
		case HAK_CNODE_CLASS:
			if (compile_class(hak, obj) <= -1) return -1;
			goto done;

		case HAK_CNODE_FUN:
			if (compile_fun(hak, obj) <= -1) return -1;
			goto done;

		case HAK_CNODE_VAR:
			if (compile_var(hak, obj) <= -1) return -1;
			goto done;

		case HAK_CNODE_DO:
			if (compile_do(hak, obj) <= -1) return -1;
			goto done;

		case HAK_CNODE_IF:
			if (compile_if(hak, obj) <= -1) return -1;
			goto done;

		case HAK_CNODE_ELIF:
			hak_setsynerrbfmt(hak, HAK_SYNERR_ELSE, HAK_CNODE_GET_LOC(car), HAK_CNODE_GET_TOK(car), "elif without if");
			return -1;

		case HAK_CNODE_ELSE:
			hak_setsynerrbfmt(hak, HAK_SYNERR_ELIF, HAK_CNODE_GET_LOC(car), HAK_CNODE_GET_TOK(car), "else without if");
			return -1;

		case HAK_CNODE_THROW:
			if (compile_throw(hak, obj) <= -1) return -1;
			goto done;

		case HAK_CNODE_TRY:
			if (compile_try(hak, obj) <= -1) return -1;
			goto done;

		case HAK_CNODE_CATCH:
			hak_setsynerrbfmt(hak, HAK_SYNERR_CATCH, HAK_CNODE_GET_LOC(car), HAK_CNODE_GET_TOK(car), "catch without try");
			return -1;

		case HAK_CNODE_BREAK:
			if (compile_break(hak, obj) <= -1) return -1;
			goto done;

		case HAK_CNODE_CONTINUE:
			if (compile_continue(hak, obj) <= -1) return -1;
			goto done;

		case HAK_CNODE_UNTIL:
			if (compile_while(hak, obj, COP_POST_UNTIL_COND) <= -1) return -1;
			goto done;

		case HAK_CNODE_WHILE:
			if (compile_while(hak, obj, COP_POST_WHILE_COND) <= -1) return -1;
			goto done;

		case HAK_CNODE_RETURN:
			/* (return 10)
			 * (return (+ 10 20)) */
			if (compile_return(hak, obj, 0) <= -1) return -1;
			goto done;

		case HAK_CNODE_REVERT:
			if (compile_return(hak, obj, 1) <= -1) return -1;
			goto done;

		case HAK_CNODE_AND:
			if (compile_and(hak, obj) <= -1) return -1;
			goto done;

		case HAK_CNODE_OR:
			if (compile_or(hak, obj) <= -1) return -1;
			goto done;

		case HAK_CNODE_PLUS:
			if (compile_plus(hak, obj) <= -1) return -1;
			goto done;

		case HAK_CNODE_SET:
			if (compile_set(hak, obj) <= -1) return -1;
			goto done;

		case HAK_CNODE_SET_R:
			if (compile_set_r(hak, obj) <= -1) return -1;
			goto done;
	}

	if (HAK_CNODE_IS_SYMBOL(car)  || HAK_CNODE_IS_DSYMBOL(car) ||
	    HAK_CNODE_IS_CONS_CONCODED(car, HAK_CONCODE_XLIST) ||
	    HAK_CNODE_IS_CONS_CONCODED(car, HAK_CONCODE_MLIST) ||
	    HAK_CNODE_IS_CONS_CONCODED(car, HAK_CONCODE_BLIST) ||
	    HAK_CNODE_IS_CONS_CONCODED(car, HAK_CONCODE_ALIST))
	{
		/* normal function call
		 *  (<operator> <operand1> ...) */
		hak_ooi_t nargs;
		hak_ooi_t oldtop;
		hak_cframe_t* cf;
		hak_cnode_t* cdr;

		/* NOTE: cframe management functions don't use the object memory.
		 *       many operations can be performed without taking GC into account */

		/* store the position of COP_EMIT_CALL to be produced with
		 * SWITCH_TOP_CFRAME() in oldtop for argument count patching
		 * further down */
		oldtop = GET_TOP_CFRAME_INDEX(hak);
		HAK_ASSERT(hak, oldtop >= 0);

		SWITCH_TOP_CFRAME(hak, COP_EMIT_CALL, car); /* <4> */

		/* compile <operator> */
		PUSH_CFRAME(hak, COP_COMPILE_OBJECT, car); /* <2> */

		/* compile <operand1> ... etc */
		cdr = HAK_CNODE_CONS_CDR(obj);

		if (!cdr)
		{
			nargs = 0;
		}
		else
		{
			if (!HAK_CNODE_IS_CONS(cdr))
			{
				/* (funname . 10) */
				hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(cdr), HAK_CNODE_GET_TOK(cdr), "redundant cdr in function call");
				return -1;
			}

			nargs = hak_countcnodecons(hak, cdr);
			if (nargs > MAX_CODE_PARAM)
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_ARGFLOOD, HAK_CNODE_GET_LOC(cdr), HAK_NULL, "too many(%zd) parameters in function call", nargs);
				return -1;
			}
		}

		if (HAK_CNODE_IS_SYMBOL(car) || HAK_CNODE_IS_DSYMBOL(car))
		{
			hak_oop_cons_t sdc;

			/* only symbols are added to the system dictionary.
			 * perform this lookup only if car is a symbol */
			sdc = hak_lookupsysdicforsymbol_noseterr(hak, HAK_CNODE_GET_TOK(car));
			if (sdc)
			{
				hak_oop_word_t sdv;
				sdv = (hak_oop_word_t)HAK_CONS_CDR(sdc);
				if (HAK_IS_PRIM(hak, sdv))
				{
					if (nargs < sdv->slot[1] || nargs > sdv->slot[2])
					{
						hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(car), HAK_NULL,
							"parameters count(%zd) mismatch in function call - %.*js - expecting %zu-%zu parameters",
							nargs, HAK_CNODE_GET_TOKLEN(car), HAK_CNODE_GET_TOKPTR(car), sdv->slot[1], sdv->slot[2]);
						return -1;
					}
				}
			}
		}

		/* redundant cdr check is performed inside compile_object_list() */
		PUSH_SUBCFRAME(hak, COP_COMPILE_ARGUMENT_LIST, cdr); /* <3> */

		/* patch the argument count in the operand field of the COP_EMIT_CALL frame */
		cf = GET_CFRAME(hak, oldtop);
		HAK_ASSERT(hak, cf->opcode == COP_EMIT_CALL);
		cf->u.call.index = nargs;
		cf->u.call.nrets = nrets;

		/* arrange to push a dummy receiver to make the call look like a message send.
		 * if you change the dummy receiver instruction to something else, you must change
		 * the receiver value of the initial context in start_initial_process_and_context(), too */
		PUSH_CFRAME(hak, COP_EMIT_PUSH_NIL, car); /* <1> this will be executed the COP_COMPILE_OBJECT car frame */
	}
	else
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_CALLABLE, HAK_CNODE_GET_LOC(car), HAK_NULL,
			"invalid callable '%.*js' in function call",
			HAK_CNODE_GET_TOKLEN(car), HAK_CNODE_GET_TOKPTR(car));
		return -1;
	}

done:
	return 0;
}

static int compile_cons_mlist_expression (hak_t* hak, hak_cnode_t* obj, int nrets)
{
	hak_cnode_t* car, * cdr, * rcv;
	hak_ooi_t nargs;
	hak_ooi_t oldtop;
	hak_cframe_t* cf;

	/* message sending
	 *  (:<receiver> <operator> <operand1> ...)
	 *  (<receiver>:<operator> <operand1> ...)
	 *  (<receiver> <binop> <operand>
	 */
	HAK_ASSERT(hak, HAK_CNODE_IS_CONS_CONCODED(obj, HAK_CONCODE_BLIST) ||
	                 HAK_CNODE_IS_CONS_CONCODED(obj, HAK_CONCODE_MLIST));

	car = HAK_CNODE_CONS_CAR(obj);

	/* store the position of COP_EMIT_CALL to be produced with
	 * SWITCH_TOP_CFRAME() in oldtop for argument count patching
	 * further down */
	oldtop = GET_TOP_CFRAME_INDEX(hak);
	HAK_ASSERT(hak, oldtop >= 0);

	/* compile <receiver> */
	rcv = car; /* remember the receiver node to to push it later */
	SWITCH_TOP_CFRAME(hak, COP_EMIT_SEND, rcv);

	/* compile <operator> */
	cdr = HAK_CNODE_CONS_CDR(obj);
	if (!cdr)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_CALLABLE, HAK_CNODE_GET_LOC(car), HAK_CNODE_GET_TOK(car), "missing message");
		return -1;
	}
	if (!HAK_CNODE_IS_CONS(cdr))
	{
		/* (<receiver> . 10) */
		hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(cdr), HAK_CNODE_GET_TOK(cdr), "redundant cdr in message send");
		return -1;
	}
	car = HAK_CNODE_CONS_CAR(cdr);
	if (HAK_CNODE_IS_SYMBOL(car))
	{
		PUSH_CFRAME(hak, COP_EMIT_PUSH_SYMBOL, car);
	}
	else
	{
/* TODO: more sanity check on what can be used as a method */
		PUSH_CFRAME(hak, COP_COMPILE_OBJECT, car);
	}

	/* compile <operand1> ... etc */
	cdr = HAK_CNODE_CONS_CDR(cdr);
	if (!cdr)
	{
		nargs = 0;
	}
	else
	{
		if (!HAK_CNODE_IS_CONS(cdr))
		{
			/* (funname . 10) */
			hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(cdr), HAK_CNODE_GET_TOK(cdr), "redundant cdr in function call");
			return -1;
		}

		nargs = hak_countcnodecons(hak, cdr);
		if (nargs > MAX_CODE_PARAM)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_ARGFLOOD, HAK_CNODE_GET_LOC(cdr), HAK_NULL, "too many(%zd) parameters in function call", nargs);
			return -1;
		}
	}

#if 0
	if (HAK_CNODE_IS_SYMBOL(car) || HAK_CNODE_IS_DSYMBOL(car))
	{
		hak_oop_cons_t sdc;

		/* only symbols are added to the system dictionary.
		 * perform this lookup only if car is a symbol */
		sdc = hak_lookupsysdicforsymbol_noseterr(hak, HAK_CNODE_GET_TOK(car));
		if (sdc)
		{
			hak_oop_word_t sdv;
			sdv = (hak_oop_word_t)HAK_CONS_CDR(sdc);
			if (HAK_IS_PRIM(hak, sdv))
			{
				if (nargs < sdv->slot[1] || nargs > sdv->slot[2])
				{
					hak_setsynerrbfmt(hak, HAK_SYNERR_ARGCOUNT, HAK_CNODE_GET_LOC(car), HAK_NULL,
						"parameters count(%zd) mismatch in function call - %.*js - expecting %zu-%zu parameters", nargs, HAK_CNODE_GET_TOKLEN(car), HAK_CNODE_GET_TOKPTR(car), sdv->slot[1], sdv->slot[2]);
					return -1;
				}
			}
		}
	}
#endif

	/* redundant cdr check is performed inside compile_object_list() */
	PUSH_SUBCFRAME(hak, COP_COMPILE_ARGUMENT_LIST, cdr);

	/* patch the argument count in the operand field of the COP_EMIT_CALL frame */
	cf = GET_CFRAME(hak, oldtop);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_SEND);
	cf->u.sendmsg.nargs = nargs;
	cf->u.sendmsg.nrets = nrets;
	cf->u.sendmsg.to_super = (HAK_CNODE_GET_TYPE(rcv) == HAK_CNODE_SUPER);

	PUSH_CFRAME(hak, COP_COMPILE_OBJECT, rcv);
	return 0;
}

static int compile_cons_block_expression (hak_t* hak, hak_cnode_t* obj)
{
	return compile_expression_block(hak, obj, "block", CEB_IS_BLOCK);
}

static HAK_INLINE int compile_symbol (hak_t* hak, hak_cnode_t* obj)
{
	hak_var_info_t vi;
	int x;

	HAK_ASSERT(hak, HAK_CNODE_IS_SYMBOL(obj));

	/* check if a symbol is a local variable */
	x = find_variable_backward_with_token(hak, obj, &vi);
	if (x <= -1) return -1;

	if (x == 0)
	{
		hak_oop_t sym, cons;
		hak_oow_t index;
/* TODO: if i require all variables to be declared, this part is not needed and should handle it as an error */
/* TODO: change the scheme... allow declaration??? */
		/* global variable */
		sym = hak_makesymbol(hak, HAK_CNODE_GET_TOKPTR(obj), HAK_CNODE_GET_TOKLEN(obj));
		if (HAK_UNLIKELY(!sym)) return -1;

		cons = (hak_oop_t)hak_getatsysdic(hak, sym);
		if (!cons)
		{
			cons = (hak_oop_t)hak_putatsysdic(hak, sym, hak->_undef);
			if (HAK_UNLIKELY(!cons)) return -1;
		}

		/* add the entire cons pair to the literal frame */
		if (add_literal(hak, cons, &index) <= -1 ||
		    emit_one_param_instruction(hak, HAK_CODE_PUSH_OBJECT_0, index, HAK_CNODE_GET_LOC(obj)) <= -1) return -1;

		return 0;
	}
	else
	{
		HAK_ASSERT(hak, vi.type != VAR_NAMED);
		return emit_variable_access(hak, VAR_ACCESS_PUSH, &vi, HAK_CNODE_GET_LOC(obj));
	}
}

static HAK_INLINE int compile_dsymbol (hak_t* hak, hak_cnode_t* obj)
{
	hak_oop_t cons;
	hak_oow_t index;

/* TODO: need a total revamp on the dotted symbols.
 *       must differentiate module access and dictioary member access...
 *       must implementate dictionary member access syntax... */

	/* the dot notation collides with car/cdr separator? no. dotted symbols don't contains space.
	 * the car cdr separator must be a single character */
	{ /* HACK FOR NOW */
		const hak_ooch_t* sep;
		hak_oocs_t name;
		int x = 0;
		hak_var_info_t vi;
		hak_funblk_info_t* fbi;

		name = *HAK_CNODE_GET_TOK(obj);
		fbi = &hak->c->funblk.info[hak->c->funblk.depth];

		sep = hak_find_oochar(name.ptr, name.len, '.');
		HAK_ASSERT(hak, sep != HAK_NULL);
		if (hak_comp_oochars_bcstr(name.ptr, (sep - (const hak_ooch_t*)name.ptr), "self") == 0)
		{
			/* instance variable?  or instance method? */
			if (fbi->fun_type >> 8)
			{
				/* if defined using A::xxx syntax, it's not possible to know the instance position of an instance variable.
				 * class X [ a b ] {
				 *   fun a() {
				 *      fun J:t() {
				 *        ## J has nothing to to with X in priciple even if J may point to X when a() is executed.
				 *        ## it's not meaningful to look up the variable `a` in the context of class X.
				 *        ## it must be prohibited to access instance variables using the self or super prefix
				 *        ## in this context
				 *        return self.a
				 *      }
				 *   }
				 * }
				 */
				hak_setsynerrbfmt(hak, HAK_SYNERR_VARNAME, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "not allowed to prefix with self in out-of-class method context");
				return -1;
			}
			name.ptr = (hak_ooch_t*)(sep + 1);
			name.len -= 5;
			x = find_variable_backward_with_word(hak, &name, HAK_CNODE_GET_LOC(obj), 1, &vi);
		}
		else if (hak_comp_oochars_bcstr(name.ptr, sep - (const hak_ooch_t*)name.ptr, "super") == 0)
		{
			if (fbi->fun_type >> 8) /* if defined using A:xxx syntax */
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_VARNAME, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "not allowed to prefix with super in out-of-class method context");
				return -1;
			}
			name.ptr = (hak_ooch_t*)(sep + 1);
			name.len -= 6;
			x = find_variable_backward_with_word(hak, &name, HAK_CNODE_GET_LOC(obj), 2, &vi); /* TODO: arrange to skip the current class */
		}

		if (x <= -1) return -1; /* error */
		if (x >= 1)
		{
			/* found */
			HAK_ASSERT(hak, vi.type != VAR_NAMED);
			return emit_variable_access(hak, VAR_ACCESS_PUSH, &vi, HAK_CNODE_GET_LOC(obj));
		}

/* TODO: check if it's the method name??? NOT POSSIBLE??? */

		/* if not found or not beginning with self/super, carry on with remaining resolution methods */
	}

	cons = (hak_oop_t)hak_lookupsysdicforsymbol_noseterr(hak, HAK_CNODE_GET_TOK(obj));
	if (!cons)
	{
		/* query the module for information if it is the first time
		 * when the dotted symbol is seen */

		hak_pfbase_t* pfbase;
		hak_mod_t* mod;
		hak_oop_t sym, val;
		unsigned int kernel_bits;

		pfbase = hak_querymod(hak, HAK_CNODE_GET_TOKPTR(obj), HAK_CNODE_GET_TOKLEN(obj), &mod);
		if (!pfbase)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_VARNAME, HAK_CNODE_GET_LOC(obj), HAK_CNODE_GET_TOK(obj), "unknown dotted symbol");
			return -1;
		}

		sym = hak_makesymbol(hak, HAK_CNODE_GET_TOKPTR(obj), HAK_CNODE_GET_TOKLEN(obj));
		if (HAK_UNLIKELY(!sym)) return -1;

		hak_pushvolat(hak, &sym);
		switch (pfbase->type)
		{
			case HAK_PFBASE_FUNC:
				kernel_bits = 2;
				val = hak_makeprim(hak, pfbase->handler, pfbase->minargs, pfbase->maxargs, mod);
				break;

			case HAK_PFBASE_VAR:
				kernel_bits = 1;
				val = hak->_nil;
				break;

			case HAK_PFBASE_CONST:
				/* TODO: create a value from the pfbase information. it needs to get extended first
				 * can i make use of pfbase->handler type-cast to a differnt type? */
				kernel_bits = 2;
				val = hak->_nil;
				break;

			default:
				hak_popvolat(hak);
				hak_seterrbfmt(hak, HAK_EINVAL, "invalid pfbase type - %d\n", pfbase->type);
				return -1;
		}

		if (!val || !(cons = (hak_oop_t)hak_putatsysdic(hak, sym, val)))
		{
			hak_popvolat(hak);
			return -1;
		}
		hak_popvolat(hak);

		/* make this dotted symbol special that it can't get changed
		 * to a different value */
		HAK_OBJ_SET_FLAGS_KERNEL (sym, kernel_bits);
	}

	if (add_literal(hak, cons, &index) <= -1 ||
	    emit_one_param_instruction(hak, HAK_CODE_PUSH_OBJECT_0, index, HAK_CNODE_GET_LOC(obj)) <= -1) return -1;

	return 0;
}

HAK_UNUSED static int string_to_ooi (hak_t* hak, hak_oocs_t* str, int radixed, hak_ooi_t* num)
{
	/* [NOTE]
	 * it is not a generic conversion functionu
	 * it assumes a certain pre-sanity check on the string
	 * done by the lexical analyzer */

	int v, negsign, base = 10;
	const hak_ooch_t* ptr, * end;
	hak_oow_t value, old_value;

	negsign = 0;
	ptr = str->ptr,
	end = str->ptr + str->len;

	HAK_ASSERT(hak, ptr < end);

	if (*ptr == '+' || *ptr == '-')
	{
		negsign = *ptr - '+';
		ptr++;
	}

	if (radixed)
	{
		/* 0xFF80, 0b1111 */
		HAK_ASSERT(hak, ptr < end);

		if (*ptr == '0')
		{
			ptr++;
			HAK_ASSERT(hak, ptr < end);

			if (*ptr == 'x') base = 16;
			else if (*ptr == 'o') base = 8;
			else if (*ptr == 'b') base = 2;
			else goto radix_r;

			ptr++;
		}

		if (base == 10)
		{
		radix_r:
			base = 0;
			do
			{
				base = base * 10 + HAK_CHAR_TO_NUM(*ptr, 10);
				ptr++;
			}
			while (*ptr != 'r');
			ptr++;
		}

		if (base < 2 || base > 36)
		{
			hak_seterrbfmt(hak, HAK_EINVAL,
				"unsupported radix '%d' in radixed number '%.*js'", base, str->len, str->ptr);
			return -1;
		}
	}

	HAK_ASSERT(hak, ptr < end);

	value = old_value = 0;
	while (ptr < end && (v = HAK_CHAR_TO_NUM(*ptr, base)) < base)
	{
		value = value * base + v;
		if (value < old_value)
		{
			/* overflow must have occurred */
			hak_seterrbfmt(hak, HAK_ERANGE, "number too big - %.*js", str->len, str->ptr);
			return -1;
		}
		old_value = value;
		ptr++;
	}

	if (ptr < end)
	{
		/* trailing garbage? */
		hak_seterrbfmt(hak, HAK_EINVAL, "trailing garbage after numeric literal - %.*js", str->len, str->ptr);
		return -1;
	}

	if (value > HAK_TYPE_MAX(hak_ooi_t) + (negsign? 1: 0)) /* assume 2's complement */
	{
		hak_seterrbfmt(hak, HAK_ERANGE, "number too big - %.*js", str->len, str->ptr);
		return -1;
	}

	*num = value;
	if (negsign) *num *= -1;

	return 0;
}

static hak_oop_t string_to_num (hak_t* hak, hak_oocs_t* str, const hak_loc_t* loc, int radixed)
{
	int negsign, base = 10;
	const hak_ooch_t* ptr, * end;

	negsign = 0;
	ptr = str->ptr,
	end = str->ptr + str->len;

	/* [NOTE]
	 * - this is not a generic conversion functionu
	 * - it assumes a certain pre-sanity check on the string
	 *   done by the lexical analyzer.
	 * - it also assumes that the reader ensures that
	 *   there is at least 1 valid digit after radix specifier. */

	HAK_ASSERT(hak, ptr < end);

	if (*ptr == '+' || *ptr == '-')
	{
		negsign = *ptr - '+';
		ptr++;
	}

	if (radixed)
	{
		/* 0xFF80, 0b1111 */
		HAK_ASSERT(hak, ptr < end);

		if (*ptr == '0')
		{
			ptr++;
			HAK_ASSERT(hak, ptr < end);

			if (*ptr == 'x') base = 16;
			else if (*ptr == 'o') base = 8;
			else if (*ptr == 'b') base = 2;
			else goto radix_r;

			ptr++;
		}


		if (base == 10)
		{
		radix_r:
			base = 0;
			do
			{
				base = base * 10 + HAK_CHAR_TO_NUM(*ptr, 10);
				ptr++;
			}
			while (*ptr != 'r');
			ptr++;
		}

		HAK_ASSERT(hak, base >= 2 && base <= 36); /* the lexer must guarantee this */
		/*
		if (base < 2 || base > 36)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_RADIX, loc, HAK_NULL,
				"unsupported radix '%d' in radixed number '%.*js'", base, str->len, str->ptr);
			return HAK_NULL;
		}*/
	}

/* TODO: handle floating point numbers ... etc */
	if (negsign) base = -base;
	return hak_strtoint(hak, ptr, end - ptr, base);
}

static hak_oop_t string_to_fpdec (hak_t* hak, hak_oocs_t* str, const hak_loc_t* loc)
{
	hak_oow_t pos;
	hak_oow_t scale = 0;
	hak_oop_t v;

	pos = str->len;
	while (pos > 0)
	{
		pos--;
		if (str->ptr[pos] == '.')
		{
			scale = str->len - pos - 1;
			if (scale > HAK_SMOOI_MAX)
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_NUMRANGE, loc, str, "too many digits after decimal point");
				return HAK_NULL;
			}

			HAK_ASSERT(hak, scale > 0);
			/*if (scale > 0)*/ HAK_MEMMOVE(&str->ptr[pos], &str->ptr[pos + 1], scale * HAK_SIZEOF(str->ptr[0])); /* remove the decimal point */
			break;
		}
	}

	/* if no decimal point is included or no digit after the point , you must not call this function */
	HAK_ASSERT(hak, scale > 0);

	v = hak_strtoint(hak, str->ptr, str->len - 1, 10);
	if (HAK_UNLIKELY(!v)) return HAK_NULL;

	return hak_makefpdec(hak, v, scale);
}

static int compile_symbol_literal (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_cnode_t* oprnd;
	hak_oop_t lit;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_SYMBOL_LITERAL);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	oprnd = cf->operand;
	HAK_ASSERT(hak, HAK_CNODE_GET_TYPE(oprnd) == HAK_CNODE_SYMBOL);

	lit = hak_makesymbol(hak, HAK_CNODE_GET_TOKPTR(oprnd), HAK_CNODE_GET_TOKLEN(oprnd));
	if (HAK_UNLIKELY(!lit)) return -1;

	if (emit_push_literal(hak, lit, HAK_CNODE_GET_LOC(oprnd)) <= -1) return -1;
	POP_CFRAME(hak);
	return 0;
}

static int compile_object (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_cnode_t* oprnd;
	hak_oop_t lit;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_OBJECT);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	oprnd = cf->operand;
redo:
	switch (HAK_CNODE_GET_TYPE(oprnd))
	{
		case HAK_CNODE_NIL:
			if (emit_byte_instruction(hak, HAK_CODE_PUSH_NIL, HAK_CNODE_GET_LOC(oprnd)) <= -1) return -1;
			goto done;

		case HAK_CNODE_TRUE:
			if (emit_byte_instruction(hak, HAK_CODE_PUSH_TRUE, HAK_CNODE_GET_LOC(oprnd)) <= -1) return -1;
			goto done;

		case HAK_CNODE_FALSE:
			if (emit_byte_instruction(hak, HAK_CODE_PUSH_FALSE, HAK_CNODE_GET_LOC(oprnd)) <= -1) return -1;
			goto done;

		case HAK_CNODE_SELF:
		case HAK_CNODE_SUPER:
			/* if super is not sent a message, super represents the receiver just like self does */
/* TODO: SELF and SUPER must be limited to methods or is it ok if it just pushes the fake receiver in a normal function call?? */
			if (emit_byte_instruction(hak, HAK_CODE_PUSH_RECEIVER, HAK_CNODE_GET_LOC(oprnd)) <= -1) return -1;
			goto done;

		/* TODO: this-context */

		case HAK_CNODE_CHARLIT:
			lit = HAK_CHAR_TO_OOP(oprnd->u.charlit.v);
			goto literal;

		case HAK_CNODE_BCHRLIT: /* byte character still converts to charcter */
			lit = HAK_CHAR_TO_OOP((hak_ooch_t)oprnd->u.bchrlit.v);
			goto literal;

		case HAK_CNODE_STRLIT:
			lit = hak_makestring(hak, HAK_CNODE_GET_TOKPTR(oprnd), HAK_CNODE_GET_TOKLEN(oprnd));
			if (HAK_UNLIKELY(!lit)) return -1;
			goto literal;

		case HAK_CNODE_BSTRLIT:
			lit = hak_makebytestring(hak, HAK_CNODE_GET_TOKPTR(oprnd), HAK_CNODE_GET_TOKLEN(oprnd));
			if (HAK_UNLIKELY(!lit)) return -1;
			goto literal;

		case HAK_CNODE_SYMLIT:
			lit = hak_makesymbol(hak, HAK_CNODE_GET_TOKPTR(oprnd), HAK_CNODE_GET_TOKLEN(oprnd));
			if (HAK_UNLIKELY(!lit)) return -1;
			goto literal;

		case HAK_CNODE_NUMLIT:
			lit = string_to_num(hak, HAK_CNODE_GET_TOK(oprnd), HAK_CNODE_GET_LOC(oprnd), 0);
			if (HAK_UNLIKELY(!lit)) return -1;
			goto literal;

		case HAK_CNODE_RADNUMLIT:
			lit = string_to_num(hak, HAK_CNODE_GET_TOK(oprnd), HAK_CNODE_GET_LOC(oprnd), 1);
			if (HAK_UNLIKELY(!lit)) return -1;
			goto literal;

		case HAK_CNODE_FPDECLIT:
			lit = string_to_fpdec(hak, HAK_CNODE_GET_TOK(oprnd), HAK_CNODE_GET_LOC(oprnd));
			if (HAK_UNLIKELY(!lit)) return -1;
			goto literal;

		case HAK_CNODE_SMPTRLIT:
			lit = HAK_SMPTR_TO_OOP(oprnd->u.smptrlit.v);
			goto literal;

		case HAK_CNODE_ERRLIT:
			lit = HAK_ERROR_TO_OOP(oprnd->u.errlit.v);
			goto literal;

		case HAK_CNODE_SYMBOL: /* symbol. but not a literal. usually a variable */
			if (compile_symbol(hak, oprnd) <= -1) return -1;
			goto done;

		case  HAK_CNODE_DSYMBOL:
			if (compile_dsymbol(hak, oprnd) <= -1) return -1;
			goto done;

		case HAK_CNODE_CONS:
		{
			switch (HAK_CNODE_CONS_CONCODE(oprnd))
			{
				case HAK_CONCODE_ALIST:
					if (compile_cons_alist_expression(hak, oprnd) <= -1) return -1;
					break;

				case HAK_CONCODE_XLIST:
					if (compile_cons_xlist_expression(hak, oprnd, 0) <= -1) return -1;
					break;

				case HAK_CONCODE_BLIST: /* message send with binop */
				case HAK_CONCODE_MLIST: /* message send expression */
					if (compile_cons_mlist_expression(hak, oprnd, 0) <= -1) return -1;
					break;

				case HAK_CONCODE_BLOCK:
					if (compile_cons_block_expression(hak, oprnd) <= -1) return -1;
					break;

				case HAK_CONCODE_ARRAY:
					if (compile_cons_array_expression(hak, oprnd) <= -1) return -1;
					break;

				case HAK_CONCODE_BYTEARRAY:
				case HAK_CONCODE_CHARARRAY:
					if (compile_cons_bytearray_expression(hak, oprnd, HAK_CNODE_CONS_CONCODE(oprnd)) <= -1) return -1;
					break;

				case HAK_CONCODE_DIC:
					if (compile_cons_dic_expression(hak, oprnd) <= -1) return -1;
					break;

				case HAK_CONCODE_QLIST:
					if (compile_cons_qlist_expression(hak, oprnd) <= -1) return -1;
					break;

				case HAK_CONCODE_VLIST:
					hak_setsynerrbfmt(hak, HAK_SYNERR_VARDCLBANNED, HAK_CNODE_GET_LOC(oprnd), HAK_NULL, "variable declaration disallowed");
					return -1;

				case HAK_CONCODE_TUPLE:
					/* [a, b] is only allowed as a lvalue or in class member varialble declaration for now */
					hak_setsynerrbfmt(hak, HAK_SYNERR_BANNED, HAK_CNODE_GET_LOC(oprnd), HAK_NULL, "tuple disallowed");
					return -1;

				default:
					hak_setsynerrbfmt(hak, HAK_SYNERR_INTERN, HAK_CNODE_GET_LOC(oprnd), HAK_NULL, "internal error - unknown cons type %d", HAK_CNODE_CONS_CONCODE(oprnd));
					return -1;
			}

			break;
		}

		case HAK_CNODE_ELIST:
		{
			/* empty list */
			switch (HAK_CNODE_ELIST_CONCODE(oprnd))
			{
				case HAK_CONCODE_ALIST:
					hak_setsynerrbfmt(hak, HAK_SYNERR_BANNED, HAK_CNODE_GET_LOC(oprnd), HAK_NULL, "empty assignment list");
					return -1;

				case HAK_CONCODE_XLIST:
					hak_setsynerrbfmt(hak, HAK_SYNERR_BANNED, HAK_CNODE_GET_LOC(oprnd), HAK_NULL, "empty executable list");
					return -1;

				case HAK_CONCODE_BLIST:
					/* this must not happend as the reader prevents it */
					hak_setsynerrbfmt(hak, HAK_SYNERR_BANNED, HAK_CNODE_GET_LOC(oprnd), HAK_NULL, "empty binop list");
					return -1;

				case HAK_CONCODE_MLIST:
					hak_setsynerrbfmt(hak, HAK_SYNERR_BANNED, HAK_CNODE_GET_LOC(oprnd), HAK_NULL, "empty message send list");
					return -1;

				case HAK_CONCODE_BLOCK:
					if (compile_cons_block_expression(hak, oprnd) <= -1) return -1;
					break;

				case HAK_CONCODE_ARRAY:
					if (emit_one_param_instruction(hak, HAK_CODE_MAKE_ARRAY, 0, HAK_CNODE_GET_LOC(oprnd)) <= -1) return -1;
					goto done;

				case HAK_CONCODE_BYTEARRAY:
					if (emit_one_param_instruction(hak, HAK_CODE_MAKE_BYTEARRAY, 0, HAK_CNODE_GET_LOC(oprnd)) <= -1) return -1;
					goto done;

				case HAK_CONCODE_CHARARRAY:
					if (emit_one_param_instruction(hak, HAK_CODE_MAKE_CHARARRAY, 0, HAK_CNODE_GET_LOC(oprnd)) <= -1) return -1;
					goto done;

				case HAK_CONCODE_DIC:
					if (emit_one_param_instruction(hak, HAK_CODE_MAKE_DIC, 16, HAK_CNODE_GET_LOC(oprnd)) <= -1) return -1;
					goto done;

				case HAK_CONCODE_QLIST:
					if (emit_byte_instruction(hak, HAK_CODE_PUSH_NIL, HAK_CNODE_GET_LOC(oprnd)) <= -1) return -1;
					goto done;

				case HAK_CONCODE_VLIST:
					hak_setsynerrbfmt(hak, HAK_SYNERR_BANNED, HAK_CNODE_GET_LOC(oprnd), HAK_NULL, "empty variable declaration");
					return -1;

				case HAK_CONCODE_TUPLE:
					hak_setsynerrbfmt(hak, HAK_SYNERR_BANNED, HAK_CNODE_GET_LOC(oprnd), HAK_NULL, "empty tuple");
					return -1;

				default:
					hak_setsynerrbfmt(hak, HAK_SYNERR_INTERN, HAK_CNODE_GET_LOC(oprnd), HAK_NULL, "internal error - unknown list type %d", HAK_CNODE_CONS_CONCODE(oprnd));
					return -1;
			}

			break;
		}

		case HAK_CNODE_SHELL:
			/* a shell node is just a wrapper of an actual node */
			oprnd = oprnd->u.shell.obj;
			goto redo;

		case HAK_CNODE_ELLIPSIS:
		case HAK_CNODE_TRPCOLONS:
		case HAK_CNODE_DBLCOLONS:
		case HAK_CNODE_COLON:
		case HAK_CNODE_COLONLT:
		case HAK_CNODE_COLONGT:
		default:
			/*
			hak_setsynerrbfmt(hak, HAK_SYNERR_INTERN, HAK_CNODE_GET_LOC(oprnd), HAK_CNODE_GET_TOK(oprnd), "internal error - unexpected object type %d", HAK_CNODE_GET_TYPE(oprnd));
			*/
			hak_setsynerrbfmt(hak, HAK_SYNERR_BANNED, HAK_CNODE_GET_LOC(oprnd), HAK_NULL,
				"'%.*js' prohibited in this context",
				HAK_CNODE_GET_TOKLEN(oprnd), HAK_CNODE_GET_TOKPTR(oprnd));
			return -1;
	}

	/* the control reaches here in case a compile_xxxx() function(e.g. compile_cons_xlist_expression()) is called.
	 * such a function removes the top cframe. so POP_CFRAME() needs not be called here */
	return 0;

literal:
	if (emit_push_literal(hak, lit, HAK_CNODE_GET_LOC(oprnd)) <= -1) return -1;

done:
	POP_CFRAME(hak);
	return 0;
}

static int compile_object_r (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_cnode_t* oprnd;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_OBJECT_R);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	oprnd = cf->operand;
	if (HAK_CNODE_IS_CONS(oprnd))
	{
		hak_concode_t cc;

		cc = HAK_CNODE_CONS_CONCODE(oprnd);

		switch (cc)
		{
			case HAK_CONCODE_XLIST:
				return compile_cons_xlist_expression(hak, oprnd, cf->u.obj_r.nrets);

			case HAK_CONCODE_BLIST:
			case HAK_CONCODE_MLIST:
				return compile_cons_mlist_expression(hak, oprnd, cf->u.obj_r.nrets);

#if 0
			case HAK_CONCODE_ALIST:
				/* TODO: can support it? */
				k := ([a, b, c] := (+ 10 20 30))
				break;
#endif
			default:
				break;
		}
	}

	hak_setsynerrbfmt(hak, HAK_SYNERR_BANNED, HAK_CNODE_GET_LOC(oprnd), HAK_NULL, "non-function call/non-message send disallowed");
	return -1;
}

static int compile_object_list (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_cnode_t* oprnd;
	int cop;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_ARGUMENT_LIST ||
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
		POP_CFRAME(hak);
	}
	else
	{
		hak_cnode_t* car, * cdr;

#if 0
		if (cop != COP_COMPILE_ARGUMENT_LIST)
		{
			/* eliminate unnecessary non-function calls. keep the last one */
			while (HAK_CNODE_IS_CONS(oprnd))
			{
				cdr = HAK_CNODE_CONS_CDR(oprnd);
				if (!cdr) break; /* keep the last one */

				if (HAK_CNODE_IS_CONS(cdr))
				{
					/* look ahead */
					/* keep the last one before elif or else... */
					car = HAK_CNODE_CONS_CAR(cdr);
					if (HAK_CNODE_IS_SYMBOL(car) && HAK_CNODE_SYMBOL_SYNCODE(car)) break;
				}

				car = HAK_CNODE_CONS_CAR(oprnd);
/* this optimization is buggy for now... need to perfect the break condition here */
				if (HAK_CNODE_IS_CONS(car) || (HAK_CNODE_IS_SYMBOL(car) && HAK_CNODE_SYMBOL_SYNCODE(car)) || HAK_CNODE_IS_ELLIPSIS(car) || HAK_CNODE_IS_DBLCOLONS(car)) break;
				oprnd = cdr;
			}

			HAK_ASSERT(hak, oprnd != HAK_NULL);
		}
#endif
		if (!HAK_CNODE_IS_CONS(oprnd))
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(oprnd), HAK_CNODE_GET_TOK(oprnd), "redundant cdr in the object list");
			return -1;
		}

		car = HAK_CNODE_CONS_CAR(oprnd);
		cdr = HAK_CNODE_CONS_CDR(oprnd);

		if (cop == COP_COMPILE_IF_OBJECT_LIST || cop == COP_COMPILE_IF_OBJECT_LIST_TAIL)
		{
			if (HAK_CNODE_IS_TYPED(car, HAK_CNODE_ELIF))
			{
				SWITCH_TOP_CFRAME(hak, COP_COMPILE_ELIF, oprnd);
				goto done;
			}
			else if (HAK_CNODE_IS_TYPED(car, HAK_CNODE_ELSE))
			{
				SWITCH_TOP_CFRAME(hak, COP_COMPILE_ELSE, oprnd);
				goto done;
			}
		}
		else if (cop == COP_COMPILE_TRY_OBJECT_LIST || cop == COP_COMPILE_TRY_OBJECT_LIST_TAIL)
		{
			if (HAK_CNODE_IS_TYPED(car, HAK_CNODE_CATCH))
			{
				SWITCH_TOP_CFRAME(hak, COP_COMPILE_CATCH, oprnd);
				goto done;
			}
		}

		SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, car);

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
			PUSH_SUBCFRAME(hak, nextcop, cdr);
		}

		if (cop == COP_COMPILE_OBJECT_LIST_TAIL ||
		    cop == COP_COMPILE_IF_OBJECT_LIST_TAIL ||
		    cop == COP_COMPILE_TRY_OBJECT_LIST_TAIL)
		{
			/* emit POP_STACKTOP before evaluating the second objects
			 * and onwards. this goes above COP_COMPILE_OBJECT.*/
			/* TODO: if the previous operators is known to divert execution flow, it may skip this.
			 *       for instance, some 'RETURN' or 'JUMP' operators or class-level variable declaration with 'var' */
			PUSH_CFRAME(hak, COP_EMIT_POP_STACKTOP, oprnd);
		}
	}

done:
	return 0;
}

static int compile_array_list (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_cnode_t* oprnd;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_ARRAY_LIST);

	oprnd = cf->operand;

	if (!oprnd)
	{
		POP_CFRAME(hak);
	}
	else
	{
		hak_cnode_t* car, * cdr;
		hak_ooi_t oldidx;

		if (!HAK_CNODE_IS_CONS(oprnd))
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(oprnd), HAK_CNODE_GET_TOK(oprnd), "redundant cdr in the array list");
			return -1;
		}

		car = HAK_CNODE_CONS_CAR(oprnd);
		cdr = HAK_CNODE_CONS_CDR(oprnd);

		oldidx = cf->u.array_list.index;

		SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, car);
		if (cdr)
		{
			PUSH_SUBCFRAME(hak, COP_COMPILE_ARRAY_LIST, cdr);
			cf = GET_SUBCFRAME(hak);
			cf->u.array_list.index = oldidx + 1;
		}

		PUSH_SUBCFRAME(hak, COP_EMIT_POP_INTO_ARRAY, car);
		cf = GET_SUBCFRAME(hak);
		cf->u.array_list.index = oldidx;
	}

	return 0;
}

static int compile_pure_array_list (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_cnode_t* oprnd;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_PURE_ARRAY_LIST);

	oprnd = cf->operand;

	if (!oprnd)
	{
		POP_CFRAME(hak);
	}
	else
	{
		hak_cnode_t* car, * cdr;
		hak_ooi_t oldidx;
		int elem_type;

		if (!HAK_CNODE_IS_CONS(oprnd))
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(oprnd), HAK_CNODE_GET_TOK(oprnd), "redundant cdr in the byte-array list");
			return -1;
		}

		car = HAK_CNODE_CONS_CAR(oprnd);
		cdr = HAK_CNODE_CONS_CDR(oprnd);

		oldidx = cf->u.pure_array_list.index;
		elem_type = cf->u.pure_array_list.elem_type;

/* TODO: compile type check if the data element is literal...
	 runtime check if the data is a variable or something... */

		SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, car);
		if (cdr)
		{
			PUSH_SUBCFRAME(hak, COP_COMPILE_PURE_ARRAY_LIST, cdr);
			cf = GET_SUBCFRAME(hak);
			cf->u.pure_array_list.elem_type = elem_type;
			cf->u.pure_array_list.index = oldidx + 1;
		}

		PUSH_SUBCFRAME(hak, COP_EMIT_POP_INTO_PURE_ARRAY, car);
		cf = GET_SUBCFRAME(hak);
		cf->u.pure_array_list.elem_type = elem_type;
		cf->u.pure_array_list.index = oldidx;
	}

	return 0;
}


static int compile_dic_list (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_cnode_t* oprnd;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_DIC_LIST);

	oprnd = cf->operand;

	if (!oprnd)
	{
		POP_CFRAME(hak);
	}
	else
	{
		hak_cnode_t* car, * cdr, * cadr, * cddr;

		if (!HAK_CNODE_IS_CONS(oprnd))
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_DOTBANNED, HAK_CNODE_GET_LOC(oprnd), HAK_CNODE_GET_TOK(oprnd), "redundant cdr in the dictionary list");
			return -1;
		}

		car = HAK_CNODE_CONS_CAR(oprnd);
		cdr = HAK_CNODE_CONS_CDR(oprnd);

		SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, car);
		if (!cdr)
		{
			hak_setsynerrbfmt(hak, HAK_SYNERR_UNBALKV, HAK_CNODE_GET_LOC(car), HAK_NULL, "no value for key %.*js", HAK_CNODE_GET_TOKLEN(car), HAK_CNODE_GET_TOKPTR(car));
			return -1;
		}

		cadr = HAK_CNODE_CONS_CAR(cdr);
		cddr = HAK_CNODE_CONS_CDR(cdr);

		if (cddr)
		{
			PUSH_SUBCFRAME(hak, COP_COMPILE_DIC_LIST, cddr);
		}

		PUSH_SUBCFRAME(hak, COP_EMIT_POP_INTO_DIC, cdr);
		PUSH_SUBCFRAME(hak, COP_COMPILE_OBJECT, cadr);
	}

	return 0;
}

static int compile_qlist (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_cnode_t* oprnd;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_COMPILE_QLIST);

	oprnd = cf->operand;

	if (!oprnd)
	{
		POP_CFRAME(hak);
	}
	else
	{
		if (!HAK_CNODE_IS_CONS(oprnd))
		{
			/* the last element after . */
			SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, oprnd);
			PUSH_SUBCFRAME(hak, COP_EMIT_POP_INTO_CONS_CDR, oprnd);
		}
		else
		{
			hak_cnode_t* car, * cdr;

			car = HAK_CNODE_CONS_CAR(oprnd);
			cdr = HAK_CNODE_CONS_CDR(oprnd);

			SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT, car); /* 1 */
			if (cdr)
			{
				PUSH_SUBCFRAME(hak, COP_COMPILE_QLIST, cdr); /* 3 */
				PUSH_SUBCFRAME(hak, COP_EMIT_POP_INTO_CONS, car); /* 2 */
			}
			else
			{
				/* the last element */
				PUSH_SUBCFRAME(hak, COP_EMIT_POP_INTO_CONS_END, car); /* 2 */
			}
		}
	}

	return 0;
}

/* ========================================================================= */

static HAK_INLINE int post_if_cond (hak_t* hak)
{
	hak_cframe_t* cf, * cf2;
	hak_ooi_t jump_inst_pos;
	hak_ooi_t body_pos;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_POST_IF_COND);
	/* cf->operand can be HAK_NULL in these expressions
	 *   (if true)
	 *   (if false)
	 *   (if true 20 elif false)
	 */
	/*HAK_ASSERT(hak, cf->operand != HAK_NULL);*/

	HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX);
	jump_inst_pos = hak->code.bc.len;

	if (emit_one_param_instruction(hak, HAK_CODE_JUMP_FORWARD_IF_FALSE, MAX_CODE_JUMP, &cf->u.post_if.start_loc) <= -1) return -1;

	/* to drop the result of the conditional when it is true */
	if (emit_byte_instruction(hak, HAK_CODE_POP_STACKTOP, &cf->u.post_if.start_loc) <= -1) return -1;

	HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX);
	body_pos = hak->code.bc.len;

	if (check_block_expression_as_body(hak, cf->operand, cf->u.post_if.cmd_cnode, FOR_IF) <= -1) return -1;

	SWITCH_TOP_CFRAME(hak, COP_COMPILE_IF_OBJECT_LIST, cf->operand); /* 1 */
	PUSH_SUBCFRAME(hak, COP_POST_IF_BODY, cf->operand); /* 2 */
	cf2 = GET_SUBCFRAME(hak);
	cf2->u.post_if.body_pos = body_pos;
	cf2->u.post_if.jump_inst_pos = jump_inst_pos;
	cf2->u.post_if.start_loc = cf->u.post_if.start_loc;
	cf2->u.post_if.cmd_cnode = cf->u.post_if.cmd_cnode;
	return 0;
}

static HAK_INLINE int post_if_body (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_ooi_t jip;
	hak_oow_t jump_offset;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_POST_IF_BODY);

	/* cf->operand can be HAK_NULL in these expressions
	 *   (if true)
	 *   (if false)
	 *   (if true 20 elif false)
	 */
	/*HAK_ASSERT(hak, cf->operand != HAK_NULL);*/

	jip = cf->u.post_if.jump_inst_pos;

	if (hak->code.bc.len <= cf->u.post_if.body_pos)
	{
		/* if body is empty */
		if (emit_byte_instruction(hak, HAK_CODE_PUSH_NIL, &cf->u.post_if.start_loc) <= -1) return -1;
	}

	/* HAK_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD_IF_FALSE instruction */
	jump_offset = hak->code.bc.len - jip - (HAK_CODE_LONG_PARAM_SIZE + 1);

	if (jump_offset > MAX_CODE_JUMP * 2)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_IFFLOOD, &cf->u.post_if.start_loc, HAK_NULL, "code too big - size %zu", jump_offset);
		return -1;
	}
	patch_long_jump(hak, jip, jump_offset);

	POP_CFRAME(hak);
	return 0;
}

/* ========================================================================= */
static HAK_INLINE int post_while_cond (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_ooi_t jump_inst_pos;
	hak_ooi_t cond_pos, body_pos;
	hak_loc_t start_loc;
	int jump_inst, next_cop;
	hak_cnode_t* cond, * body;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_POST_UNTIL_COND || cf->opcode == COP_POST_WHILE_COND);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	/* the caller must pass the cons cell branching to the conditinal and the body
	 * if the body cell is given, things get complicated because the body part can be HAK_NULL.
	 * for instance, the body part is empty in (while (< i 1)  ) */
	cond = HAK_CNODE_CONS_CAR(cf->operand);
	body = HAK_CNODE_CONS_CDR(cf->operand);

	cond_pos = cf->u.post_while.cond_pos;
	start_loc = cf->u.post_while.start_loc;
	HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX);
	jump_inst_pos = hak->code.bc.len;

	if (cf->opcode == COP_POST_UNTIL_COND)
	{
		jump_inst = HAK_CODE_JUMP_FORWARD_IF_TRUE;
		next_cop = COP_POST_UNTIL_BODY;
	}
	else
	{
		jump_inst = HAK_CODE_JUMP_FORWARD_IF_FALSE;
		next_cop = COP_POST_WHILE_BODY;
	}

	if (emit_one_param_instruction(hak, jump_inst, MAX_CODE_JUMP, HAK_CNODE_GET_LOC(cond)) <= -1) return -1;
	if (emit_byte_instruction(hak, HAK_CODE_POP_STACKTOP, HAK_CNODE_GET_LOC(cond)) <= -1) return -1;

	HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX);
	body_pos = hak->code.bc.len;

	if (body)
	{
		SWITCH_TOP_CFRAME(hak, COP_COMPILE_OBJECT_LIST, body); /* 1 */
		PUSH_SUBCFRAME(hak, next_cop, cf->operand); /* 2 */
		cf = GET_SUBCFRAME(hak);
	}
	else
	{
		/* the body is empty */
		SWITCH_TOP_CFRAME(hak, next_cop, cond); /* 2 */
		cf = GET_TOP_CFRAME(hak);
	}
	cf->u.post_while.cond_pos = cond_pos;
	cf->u.post_while.body_pos = body_pos;
	cf->u.post_while.jump_inst_pos = jump_inst_pos;
	cf->u.post_while.start_loc = start_loc;

	return 0;
}

static HAK_INLINE int post_while_body (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_ooi_t jip;
	hak_ooi_t jump_offset;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_POST_UNTIL_BODY || cf->opcode == COP_POST_WHILE_BODY);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	HAK_ASSERT(hak, hak->code.bc.len >= cf->u.post_while.cond_pos);
	if (hak->code.bc.len > cf->u.post_while.body_pos)
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
		if (emit_byte_instruction(hak, HAK_CODE_POP_STACKTOP, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;
	}

	HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX);
	jump_offset = hak->code.bc.len - cf->u.post_while.cond_pos + 1;
	if (jump_offset > 3) jump_offset += HAK_CODE_LONG_PARAM_SIZE;
	if (emit_one_param_instruction(hak, HAK_CODE_JUMP_BACKWARD_0, jump_offset, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	jip = cf->u.post_while.jump_inst_pos;
	/* HAK_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD_IF_FALSE/JUMP_FORWARD_IF_TRUE instruction */
	jump_offset = hak->code.bc.len - jip - (HAK_CODE_LONG_PARAM_SIZE + 1);
	if (jump_offset > MAX_CODE_JUMP * 2)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_BLKFLOOD, &cf->u.post_while.start_loc, HAK_NULL, "code too big - size %zu", jump_offset);
		return -1;
	}
	patch_long_jump(hak, jip, jump_offset);

	POP_CFRAME(hak);

	HAK_ASSERT(hak, hak->c->ctlblk.info[hak->c->ctlblk.depth]._type == HAK_CTLBLK_TYPE_LOOP);
	pop_ctlblk(hak);

	return 0;
}


/* ========================================================================= */

static HAK_INLINE int emit_call (hak_t* hak)
{
	hak_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_CALL);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	if (cf->u.call.nrets > 0)
	{
		n = emit_two_param_instruction(hak, HAK_CODE_CALL_R, cf->u.call.index, cf->u.call.nrets, HAK_CNODE_GET_LOC(cf->operand));
	}
	else
	{
		n = emit_one_param_instruction(hak, HAK_CODE_CALL_0, cf->u.call.index, HAK_CNODE_GET_LOC(cf->operand));
	}

	POP_CFRAME(hak);
	return n;
}

static HAK_INLINE int emit_push_nil (hak_t* hak)
{
	hak_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_PUSH_NIL);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	n = emit_byte_instruction(hak, HAK_CODE_PUSH_NIL, HAK_CNODE_GET_LOC(cf->operand));
	POP_CFRAME(hak);
	return n;
}

static HAK_INLINE int emit_push_symbol (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_oop_t lit;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_PUSH_SYMBOL);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	lit = hak_makesymbol(hak, HAK_CNODE_GET_TOKPTR(cf->operand), HAK_CNODE_GET_TOKLEN(cf->operand));
	if (HAK_UNLIKELY(!lit)) return -1;
	if (emit_push_literal(hak, lit, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	POP_CFRAME(hak);
	return 0;
}

static HAK_INLINE int emit_send (hak_t* hak)
{
	hak_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_SEND);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	if (cf->u.sendmsg.nrets > 0)
	{
		n = emit_two_param_instruction(hak, (cf->u.sendmsg.to_super? HAK_CODE_SEND_TO_SUPER_R: HAK_CODE_SEND_R), cf->u.sendmsg.nargs, cf->u.sendmsg.nrets, HAK_CNODE_GET_LOC(cf->operand));
	}
	else
	{
		n = emit_one_param_instruction(hak, (cf->u.sendmsg.to_super? HAK_CODE_SEND_TO_SUPER_0: HAK_CODE_SEND_0), cf->u.sendmsg.nargs, HAK_CNODE_GET_LOC(cf->operand));
	}

	POP_CFRAME(hak);
	return n;
}

/* ========================================================================= */

static HAK_INLINE int emit_make_array (hak_t* hak)
{
	hak_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_MAKE_ARRAY);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	n = emit_one_param_instruction(hak, HAK_CODE_MAKE_ARRAY, cf->u.array_list.index, HAK_CNODE_GET_LOC(cf->operand));

	POP_CFRAME(hak);
	return n;
}

static HAK_INLINE int emit_make_pure_array (hak_t* hak)
{
	hak_cframe_t* cf;
	int n, inst;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_MAKE_PURE_ARRAY);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	inst = (cf->u.pure_array_list.elem_type == HAK_CONCODE_BYTEARRAY)? HAK_CODE_MAKE_BYTEARRAY: HAK_CODE_MAKE_CHARARRAY;
	n = emit_one_param_instruction(hak, inst, cf->u.pure_array_list.index, HAK_CNODE_GET_LOC(cf->operand));

	POP_CFRAME(hak);
	return n;
}

static HAK_INLINE int emit_make_dic (hak_t* hak)
{
	hak_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_MAKE_DIC);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	n = emit_one_param_instruction(hak, HAK_CODE_MAKE_DIC, cf->u.dic_list.index, HAK_CNODE_GET_LOC(cf->operand));

	POP_CFRAME(hak);
	return n;
}

static HAK_INLINE int emit_make_cons (hak_t* hak)
{
	hak_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_MAKE_CONS);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	n = emit_byte_instruction(hak, HAK_CODE_MAKE_CONS, HAK_CNODE_GET_LOC(cf->operand));

	POP_CFRAME(hak);
	return n;
}

static HAK_INLINE int emit_pop_into_array (hak_t* hak)
{
	hak_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_POP_INTO_ARRAY);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	n = emit_one_param_instruction(hak, HAK_CODE_POP_INTO_ARRAY, cf->u.array_list.index, HAK_CNODE_GET_LOC(cf->operand));

	POP_CFRAME(hak);
	return n;
}

static HAK_INLINE int emit_pop_into_pure_array (hak_t* hak)
{
	hak_cframe_t* cf;
	int n, inst;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_POP_INTO_PURE_ARRAY);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	inst = (cf->u.pure_array_list.elem_type == HAK_CONCODE_BYTEARRAY)? HAK_CODE_POP_INTO_BYTEARRAY: HAK_CODE_POP_INTO_CHARARRAY;
	n = emit_one_param_instruction(hak, inst, cf->u.pure_array_list.index, HAK_CNODE_GET_LOC(cf->operand));

	POP_CFRAME(hak);
	return n;
}

static HAK_INLINE int emit_pop_into_dic (hak_t* hak)
{
	hak_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_POP_INTO_DIC);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	n = emit_byte_instruction(hak, HAK_CODE_POP_INTO_DIC, HAK_CNODE_GET_LOC(cf->operand));

	POP_CFRAME(hak);
	return n;
}

static HAK_INLINE int emit_pop_into_cons (hak_t* hak, int cmd)
{
	hak_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_POP_INTO_CONS ||
	                 cf->opcode == COP_EMIT_POP_INTO_CONS_END ||
	                 cf->opcode == COP_EMIT_POP_INTO_CONS_CDR);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	n = emit_byte_instruction(hak, cmd, HAK_CNODE_GET_LOC(cf->operand));

	POP_CFRAME(hak);
	return n;
}

/* ========================================================================= */

static HAK_INLINE int emit_fun (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_oow_t block_code_size, lfsize;
	hak_ooi_t jip;
	hak_funblk_info_t* fbi;
	hak_loc_t* oploc;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_FUN);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	oploc = HAK_CNODE_GET_LOC(cf->operand);
	fbi = &hak->c->funblk.info[hak->c->funblk.depth];
	jip = cf->u.fun.jump_inst_pos;

	if (hak->option.trait & HAK_TRAIT_INTERACTIVE)
		lfsize = hak->code.lit.len - hak->c->funblk.info[hak->c->funblk.depth].lfbase;

	/* HAK_CODE_LONG_PARAM_SIZE + 1 => size of the long JUMP_FORWARD instruction */
	block_code_size = hak->code.bc.len - jip - (HAK_CODE_LONG_PARAM_SIZE + 1);

	if (fbi->tmpr_nrvars > 0)
	{
		/* this function block defines one or more return variables */
		if (block_code_size > 0)
		{
			if (emit_byte_instruction(hak, HAK_CODE_POP_STACKTOP, oploc) <= -1) return -1;
			block_code_size++;
		}
		if (emit_byte_instruction(hak, HAK_CODE_PUSH_RETURN_R, oploc) <= -1) return -1;
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
				if (emit_byte_instruction(hak, HAK_CODE_PUSH_NIL, oploc) <= -1) return -1;
				block_code_size++;
			}
		}
		else
		{
			/* in-class methods */
			if (block_code_size == 1)
			{
				/* simple optimization not to skip emitting POP_STACKTOP */
				HAK_ASSERT(hak, hak->code.bc.len > 0);
				if (hak->code.bc.ptr[hak->code.bc.len - 1] == HAK_CODE_PUSH_NIL)
				{
					hak->code.bc.len--;
					block_code_size--;
				}
				else if (hak->code.bc.ptr[hak->code.bc.len - 1] == HAK_CODE_PUSH_RECEIVER)
				{
					goto emit_return_from_block;
				}
			}

			if (block_code_size > 0)
			{
				if (emit_byte_instruction(hak, HAK_CODE_POP_STACKTOP, oploc) <= -1) return -1;
				block_code_size++;
			}

			if (emit_byte_instruction(hak, HAK_CODE_PUSH_RECEIVER, oploc) <= -1) return -1;
			block_code_size++;
		}

	emit_return_from_block:
		if (emit_byte_instruction(hak, HAK_CODE_RETURN_FROM_BLOCK, oploc) <= -1) return -1;
		block_code_size++;
	}

	if (block_code_size > MAX_CODE_JUMP * 2)
	{
		hak_setsynerrbfmt(hak, HAK_SYNERR_BLKFLOOD, oploc, HAK_NULL, "code too big - size %zu", block_code_size);
		return -1;
	}
	patch_long_jump(hak, jip, block_code_size);

	if (hak->option.trait & HAK_TRAIT_INTERACTIVE)
		patch_long_param(hak, cf->u.fun.lfsize_pos, lfsize);

	POP_CFRAME(hak);
	return 0;
}

static HAK_INLINE int post_fun (hak_t* hak)
{
	hak_cframe_t* cf;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_POST_FUN);

	/*hak->c->funblk.depth--;
	hak->c->tv.s.len = hak->c->funblk.info[hak->c->funblk.depth].tmprlen;
	hak->c->tv.wcount = hak->c->funblk.info[hak->c->funblk.depth].tmprcnt;*/
	pop_funblk(hak);

	if (cf->operand)
	{
		/* (fun x()  ; this x refers to a variable in the outer scope.
		 *     | t1 t2 x |
		 *     (set x 10)  ; this x refers to the local variable.
		 * )
		 *
		 * the block has been exited(blk.depth--) before finding 'x' in the outer scope.
		 */
		hak_cnode_t* fun_name = cf->operand;
		hak_cnode_t* class_name = cf->u.fun.class_name;
		hak_var_info_t vi;
		int x;

		HAK_ASSERT(hak, HAK_CNODE_IS_SYMBOL(fun_name));

		if (is_in_class_init_scope(hak))
		{
			/* method definition */

			if (class_name)
			{
				/* something wrong - this must not happen because the reader must prevent this
				 * but if it happens, it is a syntax error */
				hak_setsynerrbfmt(
					hak, HAK_SYNERR_BANNED, HAK_CNODE_GET_LOC(class_name), HAK_NULL,
					"class name '%.js' prohibited class initialization context",
					HAK_CNODE_GET_TOKLEN(class_name), HAK_CNODE_GET_TOKPTR(class_name));
				return -1;
			}

			x = find_variable_backward_with_token(hak, fun_name, &vi);
			if (x <= -1) return -1;
			if (x == 0)
			{
				/* arrange to save to the method slot */
				switch (cf->u.fun.fun_type & 0xFF)
				{
					case FUN_CM: /* class method */
						SWITCH_TOP_CFRAME(hak, COP_EMIT_CLASS_CMSTORE, fun_name);
						break;

					case FUN_CIM: /* class instantiation method */
						SWITCH_TOP_CFRAME(hak, COP_EMIT_CLASS_CIMSTORE, fun_name);
						break;

					case FUN_IM: /* instance method */
						SWITCH_TOP_CFRAME(hak, COP_EMIT_CLASS_IMSTORE, fun_name);
						break;

					default:
						/* in the class initialization scope, the type must not be other than the listed above */
						HAK_DEBUG1(hak, "Internal error - invalid method type %d\n", cf->u.fun.fun_type & 0xFF);
						hak_seterrbfmt(hak, HAK_EINTERN, "internal error - invalid method type %d", cf->u.fun.fun_type & 0xFF);
						return -1;
				}
				cf = GET_TOP_CFRAME(hak);
			}
			else
			{
				hak_setsynerrbfmt(hak, HAK_SYNERR_VARNAMEDUP, HAK_CNODE_GET_LOC(fun_name), HAK_CNODE_GET_TOK(fun_name), "duplicate method name");
				return -1;
			}
		}
		else
		{
			/* the function name must be global or module-wide.(no module implemented yet. so only global) */
		#if 0
			x = find_variable_backward_with_token(hak, fun_name, &vi);
			if (x <= -1) return -1;
			if (x == 0)
			{
				SWITCH_TOP_CFRAME(hak, COP_EMIT_SET, fun_name);
				cf = GET_TOP_CFRAME(hak);
				cf->u.set.vi.type = VAR_NAMED;
			}
			else
			{
				SWITCH_TOP_CFRAME(hak, COP_EMIT_SET, fun_name);
				cf = GET_TOP_CFRAME(hak);
				cf->u.set.vi = vi;
			}
			cf->u.set.mode = VAR_ACCESS_STORE;
		#else
			if (class_name)
			{
				/* out-of-class definition */
/* TODO:  - other types of out-of-class definition - CIM_STORE, CM_STORE...  use different marker? */
				hak_oow_t index;
				hak_oop_t lit;
				int inst;

				/* treat the class name part as a normal variable.
				 * it can be a global variable like 'String' or a local variable declared */
				if (compile_symbol(hak, class_name) <= -1) return -1;

				if (emit_byte_instruction(hak, HAK_CODE_CLASS_LOAD, HAK_CNODE_GET_LOC(class_name)) <= -1) return -1;

				/* the function name is always named */
				lit = hak_makesymbol(hak, HAK_CNODE_GET_TOKPTR(fun_name), HAK_CNODE_GET_TOKLEN(fun_name));
				if (HAK_UNLIKELY(!lit)) return -1;
				if (add_literal(hak, lit, &index) <= -1) return -1;

				switch (cf->u.fun.fun_type & 0xFF)
				{
					case FUN_CM: /* class method */
						inst =  HAK_CODE_CLASS_CMSTORE;
						break;

					case FUN_CIM: /* class instantiation method */
						inst =  HAK_CODE_CLASS_CIMSTORE;
						break;

					case FUN_IM: /* instance method */
						inst =  HAK_CODE_CLASS_IMSTORE;
						break;

					default:
						/* in the class initialization scope, the type must not be other than the listed above */
						HAK_DEBUG1(hak, "Internal error - invalid function type %d\n", cf->u.fun.fun_type & 0xFF);
						hak_seterrbfmt(hak, HAK_EINTERN, "internal error - invalid function type %d", cf->u.fun.fun_type & 0xFF);
						return -1;
				}

				if (emit_one_param_instruction(hak, inst, index, HAK_CNODE_GET_LOC(fun_name)) <= -1) return -1;
				if (emit_byte_instruction(hak, HAK_CODE_CLASS_EXIT, HAK_CNODE_GET_LOC(class_name)) <= -1) return -1;
				POP_CFRAME(hak);
			}
			else
			{
				/* An explicitly named function is always global */
				SWITCH_TOP_CFRAME(hak, COP_EMIT_SET, fun_name);
				cf = GET_TOP_CFRAME(hak);
				cf->u.set.vi.type = VAR_NAMED;
				cf->u.set.mode = VAR_ACCESS_STORE;
			}
		#endif
		}
	}
	else
	{
		POP_CFRAME(hak);
	}

	return 0;
}

/* ========================================================================= */

static HAK_INLINE int emit_pop_stacktop (hak_t* hak)
{
	hak_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_POP_STACKTOP);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	n = emit_byte_instruction(hak, HAK_CODE_POP_STACKTOP, HAK_CNODE_GET_LOC(cf->operand));

	POP_CFRAME(hak);
	return n;
}

static HAK_INLINE int emit_return (hak_t* hak)
{
	hak_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_RETURN);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	n = emit_byte_instruction(hak, (cf->u._return.from_home? HAK_CODE_RETURN_STACKTOP: HAK_CODE_RETURN_FROM_BLOCK), HAK_CNODE_GET_LOC(cf->operand));

	POP_CFRAME(hak);
	return n;
}

static HAK_INLINE int emit_set (hak_t* hak)
{
	hak_cframe_t* cf;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_SET);
	HAK_ASSERT(hak, cf->u.set.mode == VAR_ACCESS_POP || cf->u.set.mode == VAR_ACCESS_STORE);

	if (cf->u.set.vi.type == VAR_NAMED)
	{
		hak_oow_t index;
		hak_oop_t cons, sym;

		HAK_ASSERT(hak, HAK_CNODE_IS_SYMBOL(cf->operand));

		sym = hak_makesymbol(hak, HAK_CNODE_GET_TOKPTR(cf->operand), HAK_CNODE_GET_TOKLEN(cf->operand));
		if (HAK_UNLIKELY(!sym)) return -1;

		cons = (hak_oop_t)hak_getatsysdic(hak, sym);
		if (!cons)
		{
			cons = (hak_oop_t)hak_putatsysdic(hak, sym, hak->_nil);
			if (HAK_UNLIKELY(!cons)) return -1;
		}

		if (add_literal(hak, cons, &index) <= -1) return -1;
		if (emit_one_param_instruction(hak, (cf->u.set.mode == VAR_ACCESS_POP? HAK_CODE_POP_INTO_OBJECT_0: HAK_CODE_STORE_INTO_OBJECT_0), index, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;
	}
	else
	{
		HAK_ASSERT(hak, cf->operand != HAK_NULL);
		if (emit_variable_access(hak, cf->u.set.mode, &cf->u.set.vi, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;
	}

	POP_CFRAME(hak);
	return 0;
}

static HAK_INLINE int emit_class_cmstore (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_oop_t lit;
	hak_oow_t index;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_CLASS_CMSTORE);

	lit = hak_makesymbol(hak, HAK_CNODE_GET_TOKPTR(cf->operand), HAK_CNODE_GET_TOKLEN(cf->operand));
	if (HAK_UNLIKELY(!lit)) return -1;

	if (add_literal(hak, lit, &index) <= -1) return -1;
	if (emit_one_param_instruction(hak, HAK_CODE_CLASS_CMSTORE, index, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	POP_CFRAME(hak);
	return 0;
}

static HAK_INLINE int emit_class_cimstore (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_oop_t lit;
	hak_oow_t index;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_CLASS_CIMSTORE);

	lit = hak_makesymbol(hak, HAK_CNODE_GET_TOKPTR(cf->operand), HAK_CNODE_GET_TOKLEN(cf->operand));
	if (HAK_UNLIKELY(!lit)) return -1;

	if (add_literal(hak, lit, &index) <= -1) return -1;
	if (emit_one_param_instruction(hak, HAK_CODE_CLASS_CIMSTORE, index, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	POP_CFRAME(hak);
	return 0;
}

static HAK_INLINE int emit_class_imstore (hak_t* hak)
{
	hak_cframe_t* cf;
	hak_oop_t lit;
	hak_oow_t index;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_CLASS_IMSTORE);

	lit = hak_makesymbol(hak, HAK_CNODE_GET_TOKPTR(cf->operand), HAK_CNODE_GET_TOKLEN(cf->operand));
	if (HAK_UNLIKELY(!lit)) return -1;

	if (add_literal(hak, lit, &index) <= -1) return -1;
	if (emit_one_param_instruction(hak, HAK_CODE_CLASS_IMSTORE, index, HAK_CNODE_GET_LOC(cf->operand)) <= -1) return -1;

	POP_CFRAME(hak);
	return 0;
}

static HAK_INLINE int emit_throw (hak_t* hak)
{
	hak_cframe_t* cf;
	int n;

	cf = GET_TOP_CFRAME(hak);
	HAK_ASSERT(hak, cf->opcode == COP_EMIT_THROW);
	HAK_ASSERT(hak, cf->operand != HAK_NULL);

	n = emit_byte_instruction(hak, HAK_CODE_THROW, HAK_CNODE_GET_LOC(cf->operand));

	POP_CFRAME(hak);
	return n;
}

/* ========================================================================= */

int hak_compile (hak_t* hak, hak_cnode_t* obj, int flags)
{
	hak_oow_t saved_bc_len, saved_lit_len;
	hak_bitmask_t log_default_type_mask;
	hak_funblk_info_t top_funblk_saved;
	int top_funblk_pushed_here = 0;

	hak->c->flags = flags;

	HAK_ASSERT(hak, hak->c->funblk.depth <= 0); /* 0 or 1 funblk must exist at this phase */
	HAK_ASSERT(hak, GET_TOP_CFRAME_INDEX(hak) < 0);

	if (flags & HAK_COMPILE_CLEAR_FUNBLK)
	{
		/* if the program is executed in the interactive mode,
		 * each compiled expression is executed immediately.
		 * that is, hak_compile() is followed by hak_execute()
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
		 * funblk holds information about temporaries seen so far.
		 * (2) has defined two temporary variables. this count
		 * must get carried until (3) has been compiled in the
		 * non-interactive mode. the accumulated count is used
		 * in creating an initial context for execution.
		 *
		 * in the interactive mode, the information doesn't have
		 * to get carried over.
		 */
		while (hak->c->funblk.depth >= 0) pop_funblk(hak);
		HAK_ASSERT(hak, hak->c->funblk.depth == -1);
		/* it will be recreated below */
	}
	if (flags & HAK_COMPILE_CLEAR_CODE) hak_clearcode(hak);

	saved_bc_len = hak->code.bc.len;
	saved_lit_len = hak->code.lit.len;

	log_default_type_mask = hak->log.default_type_mask;
	hak->log.default_type_mask |= HAK_LOG_COMPILER;

	/*
	 * In the non-INTERACTIVE mode, the literal frame base(lfbase) doesn't matter.
	 * Only the initial function object contains the literal frame.
	 * No other function objects are created. All 'fun' defintions are
	 * translated to block context objects instead.
	 *
	 * In the INTERACTIVE mode, the literal frame base(lfbase) plays a key role.
	 * hak_compile() is called for the top-level expression and the literal
	 * frame base can be 0. The means it is ok for a top-level code to
	 * reference part of the literal frame reserved for a function.
	 *
	 *  (set b 1)
	 *  (fun set-a(x) (set a x))
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
	 * @1 to @2 will be copied to a function object when fun is executed.
	 * The literal frame of the created function object for set-a looks
	 * like this
	 *  @0         (a)
	 *  @1         (set-a)
	 */

/* TODO: in case i implement all global variables as block arguments at the top level...what should i do? */
	HAK_ASSERT(hak, hak->c->ctlblk.depth == -1);

	if (hak->c->funblk.depth <= -1)
	{
		HAK_ASSERT(hak, hak->c->funblk.depth == -1);
		HAK_ASSERT(hak, hak->c->tv.s.len == 0);
		HAK_ASSERT(hak, hak->c->tv.wcount == 0);

		/* keep a virtual function block for the top-level compilation.
		 * pass HAK_TYPE_MAX(hak_oow_t) as make_inst_pos because there is
		 * no actual MAKE_BLOCK/MAKE_FUNCTION instruction which otherwise
		 * would be patched in pop_funblk(). */

		if (push_funblk(
			hak, HAK_NULL,
			0, /* tmpr_va */
			0, /* tmpr_nargs */
			0, /* tmpr_nrvars */
			hak->c->tv.wcount, /* tmpr_nlvars */
			hak->c->tv.wcount, /* tmpr_count */
			hak->c->tv.s.len, /* tmpr_len */
			HAK_TYPE_MAX(hak_oow_t), /* make_inst_pos */
			0, /* lfbase */
			FUN_PLAIN /* fun_type */
		) <= -1) return -1; /* must not goto oops */

		top_funblk_pushed_here = 1;
	}
	top_funblk_saved = hak->c->funblk.info[0];
	HAK_ASSERT(hak, hak->c->funblk.depth == 0); /* ensure the virtual function block is added */

	PUSH_CFRAME(hak, COP_COMPILE_OBJECT, obj);

	while (GET_TOP_CFRAME_INDEX(hak) >= 0)
	{
		hak_cframe_t* cf;

		cf = GET_TOP_CFRAME(hak);

/* TODO: tabulate this switch-based dispatch */
		switch (cf->opcode)
		{
			case COP_COMPILE_OBJECT:
				if (compile_object(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_OBJECT_R:
				if (compile_object_r(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_SYMBOL_LITERAL:
				if (compile_symbol_literal(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_ARGUMENT_LIST:
			case COP_COMPILE_OBJECT_LIST:
			case COP_COMPILE_OBJECT_LIST_TAIL:
			case COP_COMPILE_IF_OBJECT_LIST:
			case COP_COMPILE_IF_OBJECT_LIST_TAIL:
			case COP_COMPILE_TRY_OBJECT_LIST:
			case COP_COMPILE_TRY_OBJECT_LIST_TAIL:
				if (compile_object_list(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_ARRAY_LIST:
				if (compile_array_list(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_PURE_ARRAY_LIST:
				if (compile_pure_array_list(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_DIC_LIST:
				if (compile_dic_list(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_QLIST:
				if (compile_qlist(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_ELIF:
				if (compile_elif (hak) <= -1) goto oops;
				break;

			case COP_COMPILE_ELSE:
				if (compile_else(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_CATCH:
				if (compile_catch(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_AND_P1:
				if (compile_and_p1(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_AND_P2:
				if (compile_and_p2(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_BREAK_P1:
				if (compile_break_p1(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_CLASS_P1:
				if (compile_class_p1(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_CLASS_P2:
				if (compile_class_p2(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_DO_P1:
				if (compile_do_p1(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_OR_P1:
				if (compile_or_p1(hak) <= -1) goto oops;
				break;

			case COP_COMPILE_OR_P2:
				if (compile_or_p2(hak) <= -1) goto oops;
				break;

			case COP_EMIT_CALL:
				if (emit_call(hak) <= -1) goto oops;
				break;

			case COP_EMIT_PUSH_NIL:
				if (emit_push_nil(hak) <= -1) goto oops;
				break;

			case COP_EMIT_PUSH_SYMBOL:
				if (emit_push_symbol(hak) <= -1) goto oops;
				break;

			case COP_EMIT_SEND:
				if (emit_send(hak) <= -1) goto oops;
				break;

			case COP_EMIT_MAKE_ARRAY:
				if (emit_make_array(hak) <= -1) goto oops;
				break;

			case COP_EMIT_MAKE_PURE_ARRAY:
				if (emit_make_pure_array(hak) <= -1) goto oops;
				break;

			case COP_EMIT_MAKE_DIC:
				if (emit_make_dic(hak) <= -1) goto oops;
				break;

			case COP_EMIT_MAKE_CONS:
				if (emit_make_cons(hak) <= -1) goto oops;
				break;

			case COP_EMIT_POP_INTO_ARRAY:
				if (emit_pop_into_array(hak) <= -1) goto oops;
				break;

			case COP_EMIT_POP_INTO_PURE_ARRAY:
				if (emit_pop_into_pure_array(hak) <= -1) goto oops;
				break;

			case COP_EMIT_POP_INTO_DIC:
				if (emit_pop_into_dic(hak) <= -1) goto oops;
				break;

			case COP_EMIT_POP_INTO_CONS:
				if (emit_pop_into_cons(hak, HAK_CODE_POP_INTO_CONS) <= -1) goto oops;
				break;

			case COP_EMIT_POP_INTO_CONS_END:
				if (emit_pop_into_cons(hak, HAK_CODE_POP_INTO_CONS_END) <= -1) goto oops;
				break;

			case COP_EMIT_POP_INTO_CONS_CDR:
				if (emit_pop_into_cons(hak, HAK_CODE_POP_INTO_CONS_CDR) <= -1) goto oops;
				break;

			case COP_EMIT_FUN:
				if (emit_fun(hak) <= -1) goto oops;
				break;

			case COP_EMIT_PLUS:
				if (emit_plus(hak) <= -1) goto oops;
				break;

			case COP_EMIT_POP_STACKTOP:
				if (emit_pop_stacktop(hak) <= -1) goto oops;
				break;

			case COP_EMIT_RETURN:
				if (emit_return(hak) <= -1) goto oops;
				break;

			case COP_EMIT_SET:
				if (emit_set(hak) <= -1) goto oops;
				break;

			case COP_EMIT_CLASS_CMSTORE:
				if (emit_class_cmstore(hak) <= -1) goto oops;
				break;

			case COP_EMIT_CLASS_CIMSTORE:
				if (emit_class_cimstore(hak) <= -1) goto oops;
				break;

			case COP_EMIT_CLASS_IMSTORE:
				if (emit_class_imstore(hak) <= -1) goto oops;
				break;

			case COP_EMIT_THROW:
				if (emit_throw(hak) <= -1) goto oops;
				break;

			case COP_POST_IF_COND:
				if (post_if_cond(hak) <= -1) goto oops;
				break;

			case COP_POST_IF_BODY:
				if (post_if_body(hak) <= -1) goto oops;
				break;

			case COP_POST_UNTIL_BODY:
			case COP_POST_WHILE_BODY:
				if (post_while_body(hak) <= -1) goto oops;
				break;

			case COP_POST_UNTIL_COND:
			case COP_POST_WHILE_COND:
				if (post_while_cond(hak) <= -1) goto oops;
				break;

			case COP_POST_TRY:
				if (post_try(hak) <= -1) goto oops;
				break;
			case COP_POST_CATCH:
				if (post_catch(hak) <= -1) goto oops;
				break;

			case COP_POST_FUN:
				if (post_fun(hak) <= -1) goto oops;
				break;

			default:
				HAK_DEBUG1(hak, "Internal error - invalid compiler opcode %d\n", cf->opcode);
				hak_seterrbfmt(hak, HAK_EINTERN, "internal error - invalid compiler opcode %d", cf->opcode);
				goto oops;
		}
	}

	/* emit the pop instruction to clear the final result */
	if (emit_byte_instruction(hak, HAK_CODE_POP_STACKTOP, HAK_NULL) <= -1) goto oops;

	HAK_ASSERT(hak, GET_TOP_CFRAME_INDEX(hak) < 0);
	HAK_ASSERT(hak, hak->c->tv.s.len >= hak->c->funblk.info[0].tmprlen);
	HAK_ASSERT(hak, hak->c->tv.wcount >= hak->c->funblk.info[0].tmprcnt);
	HAK_ASSERT(hak, hak->c->ctlblk.depth == -1); /* no control blocks expected at this point */

	HAK_ASSERT(hak, hak->c->funblk.depth == 0); /* ensure the virtual function block be the only one left */
	hak->code.ngtmprs = hak->c->funblk.info[0].tmprcnt; /* populate the number of global temporary variables */

#if defined(CLEAR_FUNBLK_ALWAYS)
	pop_funblk(hak);
	HAK_ASSERT(hak, hak->c->tv.s.len == 0);
	HAK_ASSERT(hak, hak->c->tv.wcount == 0);
#endif

	hak->log.default_type_mask = log_default_type_mask;
	return 0;

oops:
	POP_ALL_CFRAMES(hak);

	hak->log.default_type_mask = log_default_type_mask;

	/* rollback any bytecodes or literals emitted so far */
	hak->code.bc.len = saved_bc_len;
	hak->code.lit.len = saved_lit_len;

	while (hak->c->funblk.depth > 0) pop_funblk(hak);
	HAK_ASSERT(hak, hak->c->funblk.depth == 0);

	if (top_funblk_pushed_here)
	{
		pop_funblk(hak);
		HAK_ASSERT(hak, hak->c->funblk.depth == -1);
		HAK_ASSERT(hak, hak->c->tv.s.len == 0);
		HAK_ASSERT(hak, hak->c->tv.wcount == 0);
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
		clear_funblk_inners(hak);
		HAK_ASSERT(hak, hak->c->funblk.depth == 0);
		hak->c->funblk.info[0] = top_funblk_saved;
		hak->c->tv.s.len = top_funblk_saved.tmprlen;
		hak->c->tv.wcount = top_funblk_saved.tmprcnt;
	}

	return -1;
}
