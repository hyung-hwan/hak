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

#ifndef _HCL_PRV_H_
#define _HCL_PRV_H_

#include <hcl.h>
#include <hcl-chr.h>
#include <hcl-fmt.h>
#include <hcl-utl.h>

/* you can define this to either 1 or 2 */
#define HCL_CODE_LONG_PARAM_SIZE 2

/* this is useful for debugging. hcl_gc() can be called
 * while hcl has not been fully initialized when this is defined*/
#define HCL_SUPPORT_GC_DURING_IGNITION

/* define this to enable karatsuba multiplication in bigint */
#define HCL_ENABLE_KARATSUBA
#define HCL_KARATSUBA_CUTOFF 32
#define HCL_KARATSUBA_CUTOFF_DEBUG 3

/* enable floating-pointer number support in the basic formatting functions */
#define HCL_ENABLE_FLTFMT

#if defined(HCL_BUILD_DEBUG)
/*#define HCL_DEBUG_LEXER 1*/
#define HCL_DEBUG_VM_PROCESSOR 1
#define HCL_DEBUG_VM_EXEC 1
/*#define HCL_PROFILE_VM 1*/
#endif

/* allow the caller to drive process switching by calling
 * stix_switchprocess(). */
#define HCL_EXTERNAL_PROCESS_SWITCH

/* limit the maximum object size such that:
 *   1. an index to an object field can be represented in a small integer.
 *   2. the maximum number of bits including bit-shifts can be represented
 *      in the hcl_oow_t type.
 */
#define HCL_LIMIT_OBJ_SIZE


#define HCL_BC_BUFFER_INIT  10240
#define HCL_BC_BUFFER_ALIGN 10240

#define HCL_LIT_BUFFER_INIT 1024
#define HCL_LIT_BUFFER_ALIGN 1024

#if defined(__has_builtin)

#	if (!__has_builtin(__builtin_memset) || !__has_builtin(__builtin_memcpy) || !__has_builtin(__builtin_memmove) || !__has_builtin(__builtin_memcmp))
#	include <string.h>
#	endif

#	if __has_builtin(__builtin_memset)
#		define HCL_MEMSET(dst,src,size)  __builtin_memset(dst,src,size)
#	else
#		define HCL_MEMSET(dst,src,size)  memset(dst,src,size)
#	endif
#	if __has_builtin(__builtin_memcpy)
#		define HCL_MEMCPY(dst,src,size)  __builtin_memcpy(dst,src,size)
#	else
#		define HCL_MEMCPY(dst,src,size)  memcpy(dst,src,size)
#	endif
#	if __has_builtin(__builtin_memmove)
#		define HCL_MEMMOVE(dst,src,size)  __builtin_memmove(dst,src,size)
#	else
#		define HCL_MEMMOVE(dst,src,size)  memmove(dst,src,size)
#	endif
#	if __has_builtin(__builtin_memcmp)
#		define HCL_MEMCMP(dst,src,size)  __builtin_memcmp(dst,src,size)
#	else
#		define HCL_MEMCMP(dst,src,size)  memcmp(dst,src,size)
#	endif

#else

	/* g++ 2.95 had a problem with __builtin_memxxx functions:
	 * implicit declaration of function `__builtin_memmove' */
#	if defined(__cplusplus) && defined(__GNUC__) && (__GNUC__ <= 2)
#		undef HAVE___BUILTIN_MEMSET
#		undef HAVE___BUILTIN_MEMCPY
#		undef HAVE___BUILTIN_MEMMOVE
#		undef HAVE___BUILTIN_MEMCMP
#	endif

#	if !defined(HAVE___BUILTIN_MEMSET) || \
	   !defined(HAVE___BUILTIN_MEMCPY) || \
	   !defined(HAVE___BUILTIN_MEMMOVE) || \
	   !defined(HAVE___BUILTIN_MEMCMP)
#		include <string.h>
#	endif

#	if defined(HAVE___BUILTIN_MEMSET)
#		define HCL_MEMSET(dst,src,size)  __builtin_memset(dst,src,size)
#       else
#		define HCL_MEMSET(dst,src,size)  memset(dst,src,size)
#       endif
#       if defined(HAVE___BUILTIN_MEMCPY)
#		define HCL_MEMCPY(dst,src,size)  __builtin_memcpy(dst,src,size)
#       else
#		define HCL_MEMCPY(dst,src,size)  memcpy(dst,src,size)
#       endif
#       if defined(HAVE___BUILTIN_MEMMOVE)
#		define HCL_MEMMOVE(dst,src,size)  __builtin_memmove(dst,src,size)
#       else
#		define HCL_MEMMOVE(dst,src,size)  memmove(dst,src,size)
#       endif
#       if defined(HAVE___BUILTIN_MEMCMP)
#		define HCL_MEMCMP(dst,src,size)  __builtin_memcmp(dst,src,size)
#       else
#		define HCL_MEMCMP(dst,src,size)  memcmp(dst,src,size)
#       endif

#endif

#if defined(HCL_LIMIT_OBJ_SIZE)
/* limit the maximum object size such that:
 *   1. an index to an object field can be represented in a small integer.
 *   2. the maximum number of bit shifts can be represented in the hcl_oow_t type.
 */
#	define HCL_OBJ_SIZE_MAX ((hcl_oow_t)HCL_SMOOI_MAX)
#	define HCL_OBJ_SIZE_BITS_MAX (HCL_OBJ_SIZE_MAX * HCL_BITS_PER_BYTE)
#else
#	define HCL_OBJ_SIZE_MAX ((hcl_oow_t)HCL_TYPE_MAX(hcl_oow_t))
#	define HCL_OBJ_SIZE_BITS_MAX (HCL_OBJ_SIZE_MAX * HCL_BITS_PER_BYTE)
#endif

/* ========================================================================= */
/* CLASS SPEC ENCODING                                                       */
/* ========================================================================= */
/*
 * The spec field of a class object encodes the number of the fixed part
 * and the type of the indexed part. The fixed part is the number of
 * named instance variables. If the spec of a class is indexed, the object
 * of the class can be instantiated with the size of the indexed part.
 *
 * For example, on a platform where sizeof(hcl_oow_t) is 4,
 * the layout of the spec field of a class as an OOP value looks like this:
 *
 *  31                          12 11  10 9 8 7 6 5 4 3 2   1 0
 * |number of named instance variables|indexed-type|flags |oop-tag|
 *
 * the number of named instance variables is stored in high 21 bits.
 * the indexed type takes up bit 5 to bit 10 (assuming HCL_OBJ_TYPE_BITS is 6.
 * HCL_OBJ_TYPE_XXX enumerators are used to represent actual values).
 * and the indexability is stored in the flag bits which span from bit 2 to 4.
 *
 * The maximum number of named(fixed) instance variables for a class is:
 *     2 ^ ((BITS-IN-OOW - HCL_OOP_TAG_BITS_LO) - HCL_OBJ_TYPE_BITS - 1 - 2) - 1
 *
 * HCL_OOP_TAG_BITS_LO are decremented from the number of bits in OOW because
 * the spec field is always encoded as a small integer.
 *
 * The number of named instance variables can be greater than 0 if the
 * class spec is not indexed or if it's a pointer indexed class
 * (indexed_type == HCL_OBJ_TYPE_OOP)
 *
 * indexed_type is one of the #hcl_obj_type_t enumerators.
 */

#define HCL_CLASS_SPEC_FLAG_BITS (3)

/*
 * The HCL_CLASS_SPEC_MAKE() macro creates a class spec value.
 *  _class->spec = HCL_SMOOI_TO_OOP(HCL_CLASS_SPEC_MAKE(0, 1, HCL_OBJ_TYPE_CHAR));
 */
#define HCL_CLASS_SPEC_MAKE(named_instvar,flags,indexed_type) ( \
	(((hcl_oow_t)(named_instvar)) << (HCL_OBJ_FLAGS_TYPE_BITS + HCL_CLASS_SPEC_FLAG_BITS)) |  \
	(((hcl_oow_t)(indexed_type)) << (HCL_CLASS_SPEC_FLAG_BITS)) | (((hcl_oow_t)flags) & HCL_LBMASK(hcl_oow_t,HCL_CLASS_SPEC_FLAG_BITS)))

/* what is the number of named instance variables?
 *  HCL_CLASS_SPEC_NAMED_INSTVARS(HCL_OOP_TO_SMOOI(_class->spec))
 * ensure to update Class<<specNumInstVars if you change this macro. */
#define HCL_CLASS_SPEC_NAMED_INSTVARS(spec) \
	(((hcl_oow_t)(spec)) >> (HCL_OBJ_FLAGS_TYPE_BITS + HCL_CLASS_SPEC_FLAG_BITS))

/* is it a user-indexable class?
 * all objects can be indexed with basicAt:.
 * this indicates if an object can be instantiated with a dynamic size
 * (new: size) and and can be indexed with at:.
 */
#define HCL_CLASS_SPEC_FLAGS(spec) (((hcl_oow_t)(spec)) & HCL_LBMASK(hcl_oow_t,HCL_CLASS_SPEC_FLAG_BITS))

/* if so, what is the indexing type? character? pointer? etc? */
#define HCL_CLASS_SPEC_INDEXED_TYPE(spec) \
	((((hcl_oow_t)(spec)) >> HCL_CLASS_SPEC_FLAG_BITS) & HCL_LBMASK(hcl_oow_t, HCL_OBJ_FLAGS_TYPE_BITS))

#define HCL_CLASS_SPEC_FLAG_INDEXED    (1 << 0)
#define HCL_CLASS_SPEC_FLAG_IMMUTABLE  (1 << 1)
#define HCL_CLASS_SPEC_FLAG_UNCOPYABLE (1 << 2)

#define HCL_CLASS_SPEC_IS_INDEXED(spec) (HCL_CLASS_SPEC_FLAGS(spec) & HCL_CLASS_SPEC_FLAG_INDEXED)
#define HCL_CLASS_SPEC_IS_IMMUTABLE(spec) (HCL_CLASS_SPEC_FLAGS(spec) & HCL_CLASS_SPEC_FLAG_IMMUTABLE)
#define HCL_CLASS_SPEC_IS_UNCOPYABLE(spec) (HCL_CLASS_SPEC_FLAGS(spec) & HCL_CLASS_SPEC_FLAG_UNCOPYABLE)

/* What is the maximum number of named instance variables?
 * This limit is set this way because the number must be encoded into
 * the spec field of the class with limited number of bits assigned to
 * the number of named instance variables.
 */
#define HCL_MAX_NAMED_INSTVARS \
	HCL_BITS_MAX(hcl_oow_t, HCL_SMOOI_ABS_BITS - (HCL_OBJ_FLAGS_TYPE_BITS + HCL_CLASS_SPEC_FLAG_BITS))

/* Given the number of named instance variables, what is the maximum number
 * of indexed instance variables? The number of indexed instance variables
 * is not stored in the spec field of the class. It only affects the actual
 * size of an object(obj->_size) selectively combined with the number of
 * named instance variables. So it's the maximum value of obj->_size minus
 * the number of named instance variables.
 */
#define HCL_MAX_INDEXED_INSTVARS(named_instvar) (HCL_OBJ_SIZE_MAX - named_instvar)

/*
 * self-specification of a class
 *   | classinstvars     | classvars         | flags |
 *
 * When converted to a small integer
 *   | sign-bit | classinstvars | classvars | flags | tag |
 */
#define HCL_CLASS_SELFSPEC_FLAG_BITS (3)
#define HCL_CLASS_SELFSPEC_CLASSINSTVAR_BITS ((HCL_SMOOI_ABS_BITS - HCL_CLASS_SELFSPEC_FLAG_BITS) / 2)
#define HCL_CLASS_SELFSPEC_CLASSVAR_BITS (HCL_SMOOI_ABS_BITS - (HCL_CLASS_SELFSPEC_CLASSINSTVAR_BITS + HCL_CLASS_SELFSPEC_FLAG_BITS))

#define HCL_CLASS_SELFSPEC_MAKE(class_var,classinst_var,flag) \
	((((hcl_oow_t)class_var)     << (HCL_CLASS_SELFSPEC_CLASSINSTVAR_BITS + HCL_CLASS_SELFSPEC_FLAG_BITS)) | \
	 (((hcl_oow_t)classinst_var) << (HCL_CLASS_SELFSPEC_FLAG_BITS)) | \
	 (((hcl_oow_t)flag)          << (0)))

#define HCL_CLASS_SELFSPEC_CLASSVARS(spec) \
	(((hcl_oow_t)spec) >> (HCL_CLASS_SELFSPEC_CLASSINSTVAR_BITS + HCL_CLASS_SELFSPEC_FLAG_BITS))

#define HCL_CLASS_SELFSPEC_CLASSINSTVARS(spec) \
	((((hcl_oow_t)spec) >> HCL_CLASS_SELFSPEC_FLAG_BITS) & HCL_LBMASK(hcl_oow_t, HCL_CLASS_SELFSPEC_CLASSINSTVAR_BITS))

#define HCL_CLASS_SELFSPEC_FLAGS(spec) \
	(((hcl_oow_t)spec) & HCL_LBMASK(hcl_oow_t, HCL_CLASS_SELFSPEC_FLAG_BITS))

#define HCL_CLASS_SELFSPEC_FLAG_FINAL   (1 << 0)
#define HCL_CLASS_SELFSPEC_FLAG_LIMITED (1 << 1)


#define HCL_MAX_CLASSVARS      HCL_BITS_MAX(hcl_oow_t, HCL_CLASS_SELFSPEC_CLASSVAR_BITS)
#define HCL_MAX_CLASSINSTVARS  HCL_BITS_MAX(hcl_oow_t, HCL_CLASS_SELFSPEC_CLASSINSTVAR_BITS)

/* ========================================================================= */
/* END OF CLASS SPEC ENCODING                                                */
/* ========================================================================= */



#if defined(HCL_INCLUDE_COMPILER)

/* ========================================================================= */
/* SOURCE CODE I/O FOR COMPILER                                              */
/* ========================================================================= */
enum hcl_tok_type_t
{
	HCL_TOK_EOF,

	/* the following 4 items must be in this order for code
	 * in flx_quote_token() in read.c */
	HCL_TOK_CHARLIT,
	HCL_TOK_BCHRLIT,
	HCL_TOK_STRLIT,
	HCL_TOK_BSTRLIT,

	HCL_TOK_NUMLIT,
	HCL_TOK_RADNUMLIT,
	HCL_TOK_FPDECLIT,
	HCL_TOK_SMPTRLIT,
	HCL_TOK_ERRLIT,
	HCL_TOK_NIL,
	HCL_TOK_TRUE,
	HCL_TOK_FALSE,
	HCL_TOK_SELF,
	HCL_TOK_SUPER,

	HCL_TOK_BINOP,
	HCL_TOK_IDENT,
	HCL_TOK_IDENT_DOTTED,
	HCL_TOK_IDENT_DOTTED_CLA,
	HCL_TOK_DOT,       /* . */
	HCL_TOK_DBLDOTS,   /* .. */
	HCL_TOK_ELLIPSIS,  /* ... */
	HCL_TOK_COLON,     /* : */
	HCL_TOK_DBLCOLONS, /* :: */
	HCL_TOK_TRPCOLONS, /* ::: */
	HCL_TOK_COLONEQ,   /* := */
	HCL_TOK_COLONGT,   /* :+ */
	HCL_TOK_COLONLT,   /* :+ */
	HCL_TOK_COLONSTAR, /* :* */
	HCL_TOK_SEMICOLON, /* ; */
	HCL_TOK_COMMA,     /* , */
	HCL_TOK_LPAREN,    /* ( */
	HCL_TOK_RPAREN,    /* ) */

#if 0 /* use the (obj:message ... ) syntax instad. no more mlist by (: */
	HCL_TOK_LPARCOLON, /* (: */
#define HCL_TOK_LPARCOLON HCL_TOK_LPARCOLON
#endif

	HCL_TOK_APAREN,    /* #[ - array parenthesis */
	HCL_TOK_BAPAREN,   /* #b[ - byte array parenthesis */
	HCL_TOK_CAPAREN,   /* #c[ - character array parenthesis */
#if 0
	HCL_TOK_WAPAREN,   /* #w[ - word array parenthesis */
	HCL_TOK_HWAPAREN,   /* #hw[ - half-word array parenthesis */
#endif
	HCL_TOK_QLPAREN,   /* #( - quoted-list parenthesis */
	HCL_TOK_DLPAREN,   /* #{ - dictionary parenthese */
	HCL_TOK_LBRACK,    /* [ - group */
	HCL_TOK_RBRACK,    /* ] */
	HCL_TOK_LBRACE,    /* { - block */
	HCL_TOK_RBRACE,    /* } */
	HCL_TOK_VBAR,      /* | */
	HCL_TOK_EOL,       /* end of line */

	HCL_TOK_INCLUDE,
	HCL_TOK_PRAGMA
};
typedef enum hcl_tok_type_t hcl_tok_type_t;

typedef struct hcl_tok_t hcl_tok_t;
struct hcl_tok_t
{
	hcl_tok_type_t type;
	hcl_oocs_t name;
	hcl_oow_t name_capa;
	hcl_loc_t loc;
};


typedef struct hcl_link_t hcl_link_t;
struct hcl_link_t
{
	hcl_link_t* link;
};

enum hcl_cnode_type_t
{
	HCL_CNODE_CHARLIT,
	HCL_CNODE_BCHRLIT,
	HCL_CNODE_SYMBOL,
	HCL_CNODE_DSYMBOL, /* dotted symbol */
	HCL_CNODE_STRLIT,
	HCL_CNODE_BSTRLIT,
	HCL_CNODE_NUMLIT,
	HCL_CNODE_RADNUMLIT,
	HCL_CNODE_FPDECLIT,
	HCL_CNODE_SMPTRLIT,
	HCL_CNODE_ERRLIT,
	HCL_CNODE_NIL,
	HCL_CNODE_TRUE,
	HCL_CNODE_FALSE,
	HCL_CNODE_SELF,
	HCL_CNODE_SUPER,
	HCL_CNODE_ELLIPSIS,
	HCL_CNODE_TRPCOLONS,
	HCL_CNODE_DBLCOLONS, /* :: */
	HCL_CNODE_COLONGT, /* :> */
	HCL_CNODE_COLONLT, /* :< */
	HCL_CNODE_COLONSTAR, /* :* */

	HCL_CNODE_CONS,
	HCL_CNODE_ELIST, /* empty list */
	HCL_CNODE_SHELL  /* pseudo-node to hold another actual node */
};
typedef enum hcl_cnode_type_t hcl_cnode_type_t;

enum hcl_cnode_flag_t
{
	HCL_CNODE_AUTO_FORGED = (1 << 0)
};
typedef enum hcl_cnode_flag_t hcl_cnode_flag_t;

#define HCL_CNODE_GET_TYPE(x) ((x)->cn_type)
#define HCL_CNODE_GET_FLAGS(x) ((x)->cn_flags)
#define HCL_CNODE_GET_LOC(x) (&(x)->cn_loc)
#define HCL_CNODE_GET_TOK(x) (&(x)->cn_tok)
#define HCL_CNODE_GET_TOKPTR(x) ((x)->cn_tok.ptr)
#define HCL_CNODE_GET_TOKLEN(x) ((x)->cn_tok.len)

#define HCL_CNODE_IS_ELLIPSIS(x) ((x)->cn_type == HCL_CNODE_ELLIPSIS)
#define HCL_CNODE_IS_TRPCOLONS(x) ((x)->cn_type == HCL_CNODE_TRPCOLONS)
#define HCL_CNODE_IS_DBLCOLONS(x) ((x)->cn_type == HCL_CNODE_DBLCOLONS)
#define HCL_CNODE_IS_COLONGT(x) ((x)->cn_type == HCL_CNODE_COLONGT)
#define HCL_CNODE_IS_COLONLT(x) ((x)->cn_type == HCL_CNODE_COLONLT)
#define HCL_CNODE_IS_COLONSTAR(x) ((x)->cn_type == HCL_CNODE_COLONSTAR)

#define HCL_CNODE_IS_SYMBOL(x) ((x)->cn_type == HCL_CNODE_SYMBOL)
#define HCL_CNODE_IS_SYMBOL_PLAIN(x) ((x)->cn_type == HCL_CNODE_SYMBOL && (x)->u.symbol.syncode == 0)
#define HCL_CNODE_IS_SYMBOL_SYNCODED(x, code) ((x)->cn_type == HCL_CNODE_SYMBOL && (x)->u.symbol.syncode == (code))
#define HCL_CNODE_SYMBOL_SYNCODE(x) ((x)->u.symbol.syncode)

#define HCL_CNODE_IS_DSYMBOL(x) ((x)->cn_type == HCL_CNODE_DSYMBOL)
#define HCL_CNODE_IS_DSYMBOL_CLA(x) ((x)->cn_type == HCL_CNODE_DSYMBOL && (x)->u.dsymbol.is_cla)

#define HCL_CNODE_IS_CONS(x) ((x)->cn_type == HCL_CNODE_CONS)
#define HCL_CNODE_IS_CONS_CONCODED(x, code) ((x)->cn_type == HCL_CNODE_CONS && (x)->u.cons.concode == (code))
#define HCL_CNODE_CONS_CONCODE(x) ((x)->u.cons.concode)
#define HCL_CNODE_CONS_CAR(x) ((x)->u.cons.car)
#define HCL_CNODE_CONS_CDR(x) ((x)->u.cons.cdr)

#define HCL_CNODE_IS_ELIST(x) ((x)->cn_type == HCL_CNODE_ELIST)
#define HCL_CNODE_IS_ELIST_CONCODED(x, code) ((x)->cn_type == HCL_CNODE_ELIST && (x)->u.elist.concode == (code))
#define HCL_CNODE_ELIST_CONCODE(x) ((x)->u.elist.concode)

/* NOTE: hcl_cnode_t used by the built-in compiler is not an OOP object */
struct hcl_cnode_t
{
	hcl_cnode_type_t cn_type;
	int cn_flags;
	hcl_loc_t cn_loc;
	hcl_oocs_t cn_tok;

	union
	{
		struct
		{
			hcl_ooch_t v;
		} charlit;
		struct
		{
			hcl_oob_t v;
		} bchrlit;
		struct
		{
			hcl_syncode_t syncode; /* special if non-zero */
		} symbol;
		struct
		{
			int is_cla; /* class-level accessor. prefixed with self or super */
		} dsymbol;
		struct
		{
			hcl_oow_t v;
		} smptrlit;
		struct
		{
			hcl_ooi_t v;
		} errlit;
		struct
		{
			hcl_concode_t concode;
			hcl_cnode_t* car;
			hcl_cnode_t* cdr;
		} cons;
		struct
		{
			hcl_concode_t concode;
		} elist;
		struct
		{
			hcl_cnode_t* obj;
		} shell;
	} u;
};

struct hcl_var_info_t
{
	int type;
	/* ctx_offset 0 means the current context.
	 *            1 means current->home.
	 *            2 means current->home->home.
	 * index_in_ctx is a relative index within the context found.
	 */
	hcl_oow_t ctx_offset; /* context offset */
	hcl_oow_t index_in_ctx; /* index in the current scope */
};
typedef struct hcl_var_info_t hcl_var_info_t;


/* NOTE: hcl_cframe_t used by the built-in compiler is not an OOP object */
struct hcl_cframe_t
{
	int          opcode;
	hcl_cnode_t* operand;

	union
	{
		/* COP_COMPILE_OBJECT_R */
		struct
		{
			hcl_ooi_t nrets;
		} obj_r;

		/* COP_EMIT_CALL */
		struct
		{
			hcl_ooi_t index;
			hcl_ooi_t nrets;
		} call;

		/* COP_EMIT_SEND_MESSAGE */
		struct
		{
			hcl_ooi_t nargs;
			hcl_ooi_t nrets;
			int to_super;
		} sendmsg;

		/* COP_EMIT_SET */
		struct
		{
			int mode; /* VAR_ACCESS_STORE or VAR_ACCESS_POP */
			hcl_var_info_t vi;
		} set;

		struct
		{
			hcl_ooi_t cond_pos;
			hcl_ooi_t body_pos;
			hcl_ooi_t jump_inst_pos;
			hcl_loc_t start_loc;
		} post_while;

		struct
		{
			hcl_ooi_t body_pos;
			hcl_ooi_t jump_inst_pos;
			hcl_loc_t start_loc;
			hcl_cnode_t* cmd_cnode;
		} post_if;

		struct
		{
			hcl_ooi_t jump_inst_pos;
		} post_and;

		struct
		{
			hcl_ooi_t jump_inst_pos;
		} post_or;

		/* COP_POST_TRY */
		struct
		{
			hcl_oow_t jump_inst_pos;
		} post_try;

		/* COP_POST_CATCH */
		struct
		{
			hcl_oow_t jump_inst_pos;
			hcl_oow_t exarg_offset;
		} post_catch;

		struct
		{
			hcl_oow_t lvar_start;
			hcl_oow_t lvar_end;
		} post_do;

		/* COP_COMPILE_ARRAY_LIST, COP_POP_INTO_ARRAY, COP_EMIT_MAKE_ARRAY */
		struct
		{
			hcl_ooi_t index;
		} array_list;

		/* COP_COMPILE_BYTEARRAY_LIST, COP_POP_INTO_BYTEARRAY, COP_EMIT_MAKE_BYTEARRAY */
		struct
		{
			int elem_type;
			hcl_ooi_t index;
		} bytearray_list;

		/* COP_EMIT_MAKE_DIC */
		struct
		{
			hcl_ooi_t index;
		} dic_list;

		/* COP_POST_LAMBDA, COP_EMIT_LAMBDA */
		struct
		{
			int fun_type;
			hcl_oow_t jump_inst_pos;
			hcl_ooi_t lfbase_pos;
			hcl_ooi_t lfsize_pos;
			hcl_cnode_t* class_name; /* class name for out-of-class method definition */
		} lambda;

		/* COP_EMIT_RETURN */
		struct
		{
			int from_home;
		} _return;

		/* COP_POST_BREAK */
		struct
		{
			hcl_ooi_t jump_inst_pos;
		} _break;

		/* COP_COMPILE_CLASS_P1, COP_COMPILE_CLASS_P2 */
		struct
		{
			hcl_ooi_t nsuperclasses;
			hcl_loc_t start_loc;
			hcl_cnode_t* cmd_cnode;
		} _class;
	} u;
};
typedef struct hcl_cframe_t hcl_cframe_t;

enum hcl_cblk_type_t
{
	HCL_CBLK_TYPE_LOOP,
	HCL_CBLK_TYPE_TRY,
	HCL_CBLK_TYPE_CLASS
};
typedef enum hcl_cblk_type_t hcl_cblk_type_t;

/* control block information for the compiler */
struct hcl_cblk_info_t
{
	hcl_cblk_type_t _type;
};
typedef struct hcl_cblk_info_t hcl_cblk_info_t;

/* function block information for the compiler */
struct hcl_fnblk_info_t
{
	int fun_type;

	hcl_oow_t tmprlen; /* accumulated length of the temporaries string including outer blocks */
	hcl_oow_t tmprcnt; /* accumulated number of temporaries including outer blocks */

	hcl_oow_t tmpr_va; /* 0 or 1 */
	hcl_oow_t tmpr_nargs; /* number of fixed arguments */
	hcl_oow_t tmpr_nrvars; /* number of return variables */
	hcl_oow_t tmpr_nlvars; /* number of local variables */

	hcl_oow_t make_inst_pos;
	hcl_oow_t lfbase;

	hcl_ooi_t cblk_base;

	hcl_ooi_t clsblk_base;
	hcl_ooi_t clsblk_top;

	unsigned int access_outer: 1;
	unsigned int accessed_by_inner: 1;
};
typedef struct hcl_fnblk_info_t hcl_fnblk_info_t;

/* class block information for the compiler */

struct hcl_clsblk_info_t
{
	hcl_oow_t nivars;
	hcl_oow_t ncvars;
	hcl_ooch_t* ivars_str;
	hcl_ooch_t* cvars_str;
	hcl_oow_t spec; /* TODO: byte indexed, word indexed? */

	hcl_ooi_t fnblk_base;
	hcl_ooi_t class_start_inst_pos; /* the position of the first instruction in the class body after CLASS_ENTER */
};
typedef struct hcl_clsblk_info_t hcl_clsblk_info_t;


/* reader stack for list reading */
typedef struct hcl_rstl_t hcl_rstl_t;
struct hcl_rstl_t
{
	hcl_cnode_t* head;
	hcl_cnode_t* tail;
	hcl_loc_t loc;
	int flagv;
	hcl_oow_t count;
	hcl_rstl_t* prev;
};

typedef struct hcl_flx_dt_t hcl_flx_dt_t; /* delemiter token */
struct hcl_flx_dt_t
{
	int row_start;
	int row_end;
	int col_next;
};


typedef struct hcl_flx_hc_t hcl_flx_hc_t; /* hash-marked character like #\, #\newline */
struct hcl_flx_hc_t
{
	/* state data */
	hcl_oow_t char_count;
};

typedef struct hcl_flx_hi_t hcl_flx_hi_t; /* hash-marked identifier */
struct hcl_flx_hi_t
{
	/* state data */
	hcl_oow_t char_count;
};

typedef struct hcl_flx_hb_t hcl_flx_hb_t; /* intermediate state for #b */
struct hcl_flx_hb_t
{
	/* state data */
	hcl_ooch_t start_c;
};

typedef struct hcl_flx_hn_t hcl_flx_hn_t; /* hash-marked number - radixed number */
struct hcl_flx_hn_t
{
	/* input data */
	hcl_tok_type_t tok_type;
	hcl_synerrnum_t synerr_code;
	int radix;

	/* state data */
	hcl_oow_t digit_count;
	hcl_oow_t invalid_digit_count;
};

typedef struct hcl_flx_pi_t hcl_flx_pi_t;
struct hcl_flx_pi_t
{
	/* state data */
	hcl_oow_t char_count;
	hcl_oow_t seg_count;
	hcl_oow_t seg_len;
	hcl_oow_t non_ident_seg_count;
	hcl_tok_type_t last_non_ident_type;
	int is_cla; /* class-level accrssor. prefixed with self/super */
};

typedef struct hcl_flx_binop_t hcl_flx_binop_t;
struct hcl_flx_binop_t
{
	hcl_oow_t _not_used;
};

typedef struct hcl_flx_pn_t hcl_flx_pn_t;
struct hcl_flx_pn_t
{
	/* state data */
	int fpdec;
	hcl_oow_t digit_count[2];
};

typedef struct hcl_flx_qt_t hcl_flx_qt_t; /* quoted token */
struct hcl_flx_qt_t
{
	/* input data */
	hcl_tok_type_t tok_type;
	hcl_synerrnum_t synerr_code;
	hcl_ooch_t end_char;
	hcl_ooch_t esc_char;
	hcl_oow_t min_len;
	hcl_oow_t max_len;
	unsigned int is_byte: 1;
	unsigned int regex: 1;

	/* state data */
	unsigned int escaped: 4; /* must be large enough to hold 1, 2, 4, 8 */
	int digit_count;
	hcl_ooci_t c_acc;
};

typedef struct hcl_flx_st_t hcl_flx_st_t;
struct hcl_flx_st_t
{
	/* input data */
	hcl_ooch_t sign_c;

	/* state data */
	hcl_oow_t char_count;
	int hmarked;
};


typedef struct hcl_flx_bcp_t hcl_flx_bcp_t;
struct hcl_flx_bcp_t
{
	hcl_ooch_t start_c;
};

enum hcl_flx_state_t
{
	HCL_FLX_START,
	HCL_FLX_BACKSLASHED,
	HCL_FLX_COMMENT,
	HCL_FLX_DELIM_TOKEN,
	HCL_FLX_HMARKED_TOKEN,  /* hash-marked token */
	HCL_FLX_HMARKED_B,      /* #b - intermediate state before #b[ or #b-radixed binary number */
	HCL_FLX_HMARKED_CHAR,   /* hash-marked character that begins with #\ */
	HCL_FLX_HMARKED_IDENT,  /* hash-marked identifier like #include, etc */
	HCL_FLX_HMARKED_NUMBER, /* hash-marked number - radixed number like #xABCD */
	HCL_FLX_PLAIN_IDENT,    /* plain identifier */
	HCL_FLX_BINOP,          /* binary operator */
	HCL_FLX_PLAIN_NUMBER,   /* plain number */
	HCL_FLX_QUOTED_TOKEN,   /* string, character */
	HCL_FLX_SIGNED_TOKEN,   /* prefixed with + or - */
	HCL_FLX_BC_PREFIX       /* b or C prefix before " or ' */
};
typedef enum hcl_flx_state_t hcl_flx_state_t;

typedef struct hcl_frd_t hcl_frd_t;
struct hcl_frd_t
{
	int level;
	int flagv;
	int expect_include_file;
	int expect_vlist_item;
	int do_include_file;
	hcl_cnode_t* obj;
	hcl_loc_t list_loc;
};

struct hcl_compiler_t
{
	/* flags passed in via hcl_compile() */
	int flags;

	/* callback pointer registerd upon compiler creation */
	hcl_cb_t* cbp;

	/* input handler */
	hcl_io_impl_t cci_rdr;

	/* static input data buffer */
	hcl_io_cciarg_t  cci_arg;

	/* pointer to the current input data. initially, it points to &inarg */
	hcl_io_cciarg_t* curinp;

	/* information about the last meaningful character read.
	 * this is a copy of curinp->lxc if no ungetting is performed.
	 * if there is something in the unget buffer, this is overwritten
	 * by a value from the buffer when the request to read a character
	 * is served */
	hcl_lxc_t  lxc;

	/* unget buffer */
	hcl_lxc_t  ungot[10];
	int        nungots;

	/* the last token read */
	hcl_tok_t   tok;
	hcl_link_t* sr_names;

	hcl_synerr_t synerr;

	/* temporary space to handle an illegal character */
	hcl_ooch_t ilchr;
	hcl_oocs_t ilchr_ucs;

	/* == READER == */
	struct
	{
		hcl_oop_t s;  /* stack for reading */
		hcl_oop_t e;  /* last object read */
		hcl_rstl_t* st;
	} r; /* reading */
	/* == END READER == */

	struct
	{
	#if defined(HCL_OOCH_IS_UCH)
		struct
		{
			hcl_bch_t buf[HCL_BCSIZE_MAX];
			hcl_oow_t len;
			int no_check;
		} rsd; /* residue - incomplete sequence at the end of the last data fed by hcl_feedbchars() */
	#endif

		struct
		{
			hcl_flx_state_t state;
			hcl_loc_t loc;
			hcl_loc_t _oloc;

			union
			{
				hcl_flx_dt_t dt; /* delimiter token */
				hcl_flx_hc_t hc; /* hash-marked character */
				hcl_flx_hi_t hi; /* hash-marked identifier */
				hcl_flx_hb_t hb; /* #b ... */
				hcl_flx_hn_t hn; /* hash-marked number - radixed number */
				hcl_flx_pi_t pi; /* plain identifier */
				hcl_flx_binop_t binop; /* binary operator */
				hcl_flx_pn_t pn; /* plain number */
				hcl_flx_qt_t qt; /* quoted token */
				hcl_flx_st_t st; /* signed token */
				hcl_flx_bcp_t bcp; /* b or c prefix */
			} u;
		} lx;

		struct hcl_frd_t rd;
		hcl_on_cnode_t on_cnode;
	} feed;

	/* == COMPILER STACK == */
	struct
	{
		hcl_cframe_t* ptr;
		hcl_ooi_t     top;
		hcl_oow_t     capa;
	} cfs;
	/* == END COMPILER STACK == */

	struct
	{
		hcl_oocs_t s; /* buffer */
		hcl_oow_t capa; /* bufer capacity */
		hcl_oow_t wcount; /* word count */
	} tv; /* temporary variables including arguments */

	struct
	{
		hcl_ooi_t depth; /* signed because it starts with -1 */
		hcl_cblk_info_t* info;
		hcl_oow_t info_capa;
	} cblk; /* control block - loop, try-catch */

	struct
	{
		hcl_ooi_t depth; /* signed because it starts with -1 */
		hcl_fnblk_info_t* info;
		hcl_oow_t  info_capa;
	} fnblk; /* lambda/function block */

	struct
	{
		hcl_ooi_t depth; /* signed because it starts with -1 */
		hcl_clsblk_info_t* info;
		hcl_oow_t info_capa;
	} clsblk; /* class block */


	struct
	{
		hcl_cnode_t cons_to_nil;
		hcl_cnode_t nil;
	} fake_cnode;
};
#endif



/* hcl_context_t, hcl_lambda_t, hcl_function_t stores the local variable information
 *
 * Use up to 29 bits in a 32-bit hcl_ooi_t. Exclude the tag bit and the sign bit.
 * | SIGN | INSTA | VA | NARGS | NRVARS | NLVARS | TAG |
 *     1            1     8       8        11        2    <= 32
 * -----------------------------------------------------------
 * Parameters to the MAKE_LAMBDA or MAKE_FUNCTION instructions
 *  | INSTA | VA | NARGS | NRVARS | NLVARS
 *    1       1      4      4        6         <= 16 (HCL_CODE_LONG_PARAM_SIZE 1, two params)
 *    1       1      8      8        11        <= 32 (HCL_CODE_LONG_PARAM_SIZE 2, two params, use 29 bits to avoid collection when converted to a smooi)
 *
 *
 * NARGS and NRVARS are also used for the CALL and CALL2 instructions.
 * CALL encodes NARGS in one parameter.
 * CALLR encodes NARGS in one parameter and NRVARS in another parameter.
 * NARGS and NRVARS must not exceed a single parameter size.
 */

#if defined(HCL_CODE_LONG_PARAM_SIZE) && (HCL_CODE_LONG_PARAM_SIZE == 1)

#	define MAX_CODE_NBLKARGS            (0xFu) /* 15 - 4 bits*/
#	define MAX_CODE_NBLKRVARS           (0xFu) /* 15 - 4 bits*/
#	define MAX_CODE_NBLKLVARS           (0x3Fu) /* 63 - 6 bits */

#	define ENCODE_BLK_MASK(insta,va,nargs,nrvars,nlvars) \
		((((insta) & 0x1) << 15) | (((va) & 0x1) << 14) | (((nargs) & 0xF) << 10) | (((nrvars) & 0xF) << 6) | (((nlvars) & 0x3FF)))
#	define GET_BLK_MASK_INSTA(x) (((x) >> 15) & 0x1)
#	define GET_BLK_MASK_VA(x) (((x) >> 14) & 0x1)
#	define GET_BLK_MASK_NARGS(x) (((x) >> 10) & 0xF)
#	define GET_BLK_MASK_NRVARS(x) (((x) >> 6) & 0xF)
#	define GET_BLK_MASK_NLVARS(x) ((x) & 0x3F)

#	define MAX_CODE_JUMP                (0xFFu)
#	define MAX_CODE_PARAM               (0xFFu)
#	define MAX_CODE_PARAM2              (0xFFFFu) /* 16 bits */
#elif defined(HCL_CODE_LONG_PARAM_SIZE) && (HCL_CODE_LONG_PARAM_SIZE == 2)

#	define MAX_CODE_NBLKARGS            (0xFFu) /* 255, 8 bits */
#	define MAX_CODE_NBLKRVARS           (0xFFu) /* 255, 8 bits */
#	define MAX_CODE_NBLKLVARS           (0x7FFu) /* 2047, 11 bits */
#	define ENCODE_BLK_MASK(insta,va,nargs,nrvars,nlvars) \
		((((insta) & 0x1) << 28) | (((va) & 0x1) << 27) | (((nargs) & 0xFF) << 19) | (((nrvars) & 0xFF) << 11) | (((nlvars) & 0x7FF)))
#	define GET_BLK_MASK_INSTA(x) (((x) >> 28) & 0x1)
#	define GET_BLK_MASK_VA(x) (((x) >> 27) & 0x1)
#	define GET_BLK_MASK_NARGS(x) (((x) >> 19) & 0xFF)
#	define GET_BLK_MASK_NRVARS(x) (((x) >> 11) & 0xFF)
#	define GET_BLK_MASK_NLVARS(x) ((x) & 0x7FF)

#	define MAX_CODE_JUMP                (0xFFFFu)
#	define MAX_CODE_PARAM               (0xFFFFu)
#	define MAX_CODE_PARAM2              (0xFFFFFFFFu) /* 32 bits */
#else
#	error Unsupported HCL_CODE_LONG_PARAM_SIZE
#endif



/*
----------------------------------------------------------------------------------------------------------------
SHORT INSTRUCTION CODE                                        LONG INSTRUCTION CODE
----------------------------------------------------------------------------------------------------------------
                                                                      v v
0-3      0000 00XX STORE_INTO_INSTVAR                         128  1000 0000 XXXXXXXX STORE_INTO_IVAR_X                    (bit 4 off, bit 3 off)
4-7      0000 01XX STORE_INTO_INSTVAR
8-11     0000 10XX POP_INTO_INSTVAR                           136  1000 1000 XXXXXXXX POP_INTO_IVAR_X                      (bit 4 off, bit 3 on)
12-15    0000 11XX POP_INTO_INSTVAR
16-19    0001 00XX PUSH_INSTVAR                               144  1001 0000 XXXXXXXX PUSH_IVAR_X                          (bit 4 on)
20-23    0001 01XX PUSH_INSTVAR

                                                                      v v
24-27    0001 10XX PUSH_TEMPVAR                               152  1001 1000 XXXXXXXX PUSH_TEMPVAR_X                          (bit 4 on)
28-31    0001 11XX PUSH_TEMPVAR
32-35    0010 00XX STORE_INTO_TEMPVAR                         160  1010 0000 XXXXXXXX STORE_INTO_TEMPVAR_X                    (bit 4 off, bit 3 off)
36-39    0010 01XX STORE_INTO_TEMPVAR
40-43    0010 10XX POP_INTO_TEMPVAR                           168  1010 1000 XXXXXXXX POP_INTO_TEMPVAR_X                      (bit 4 off, bit 3 on)
44-47    0010 11XX POP_INTO_TEMPVAR

48-51    0011 00XX PUSH_LITERAL                               176  1011 0000 XXXXXXXX PUSH_LITERAL_X
52-55    0011 01XX PUSH_LITERAL                               177  1011 0001 XXXXXXXX XXXXXXXX PUSH_LITERAL_X2

                                                                        vv
56-59    0011 10XX STORE_INTO_OBJECT                          184  1011 1000 XXXXXXXX STORE_INTO_OBJECT                       (bit 3 on, bit 2 off)
60-63    0011 11XX POP_INTO_OBJECT                            188  1011 1100 XXXXXXXX POP_INTO_OBJECT                         (bit 3 on, bit 2 on)
64-67    0100 00XX PUSH_OBJECT                                192  1100 0000 XXXXXXXX PUSH_OBJECT                             (bit 3 off)


68-71    0100 01XX JUMP_FORWARD                               196  1100 0100 XXXXXXXX JUMP_FORWARD_X
                                                              197  1100 0101 XXXXXXXX JUMP2_FORWARD
72-75    0100 10XX JUMP_BACKWARD                              200  1100 1000 XXXXXXXX JUMP_BACKWARD_X
                                                              201  1100 1001 XXXXXXXX JUMP2_BACKWARD
76-79    0100 11XX UNUSED                                     204  1100 1100 XXXXXXXX JUMP_FORWARD_IF_TRUE
                                                              205  1100 1101 XXXXXXXX JUMP2_FORWARD_IF_TRUE
                                                              206  1100 1110 XXXXXXXX JUMP_BACKWARD_IF_TRUE
                                                              207  1100 1111 XXXXXXXX JUMP2_BACKWARD_IF_TRUE
80-83    0101 00XX UNUSED                                     208  1101 0000 XXXXXXXX JUMP_FORWARD_IF_FALSE
                                                              209  1101 0001 XXXXXXXX JUMP2_FORWARD_IF_FALSE
                                                              210  1101 0010 XXXXXXXX JUMP_BACKWARD_IF_FALSE
                                                              211  1101 0011 XXXXXXXX JUMP2_BACKWARD_IF_FALSE

84-87    0101 01XX CALL                                       212  1101 0100 XXXXXXXX CALL_X

                                                                        vv
88-91    0101 10XX YYYYYYYY STORE_INTO_CTXTEMPVAR             216  1101 1000 XXXXXXXX YYYYYYYY STORE_INTO_CTXTEMPVAR_X        (bit 3 on, bit 2 off)
92-95    0101 11XX YYYYYYYY POP_INTO_CTXTEMPVAR               220  1101 1100 XXXXXXXX YYYYYYYY POP_INTO_CTXTEMPVAR_X          (bit 3 on, bit 2 on)
96-99    0110 00XX YYYYYYYY PUSH_CTXTEMPVAR                   224  1110 0000 XXXXXXXX YYYYYYYY PUSH_CTXTEMPVAR_X              (bit 3 off)
# XXXth outer-frame, YYYYYYYY local variable

100-103  0110 01XX YYYYYYYY PUSH_OBJVAR                       228  1110 0100 XXXXXXXX YYYYYYYY PUSH_OBJVAR_X                  (bit 3 off)
104-107  0110 10XX YYYYYYYY STORE_INTO_OBJVAR                 232  1110 1000 XXXXXXXX YYYYYYYY STORE_INTO_OBJVAR_X            (bit 3 on, bit 2 off)
108-111  0110 11XX YYYYYYYY POP_INTO_OBJVAR                   236  1110 1100 XXXXXXXX YYYYYYYY POP_INTO_OBJVAR_X              (bit 3 on, bit 2 on)
# XXXth instance variable of YYYYYYYY object

                                                                         v
112-115  0111 00XX YYYYYYYY SEND_MESSAGE                      240  1111 0000 XXXXXXXX YYYYYYYY SEND_X                 (bit 2 off)
116-119  0111 01XX YYYYYYYY SEND_TO_SUPER                     244  1111 0100 XXXXXXXX YYYYYYYY SEND_TO_SUPER_X        (bit 2 on)
# XXX args, YYYYYYYY message

120      0111 1000 YYYYYYYY PUSH_CVAR_I_X
121      0111 1001 YYYYYYYY STORE_INTO_CVAR_I_X
122      0111 1010 YYYYYYYY POP_INTO_CVAR_I_X

123      0111 1011 YYYYYYYY PUSH_CVAR_M_X
124      0111 1100 YYYYYYYY STORE_INTO_CVAR_M_X
125      0111 1101 YYYYYYYY POP_INTO_CVAR_M_X

126      0111 1110  UNUSED
127      0111 1111  UNUSED

##
## "SHORT_CODE_0 | 0x80" becomes "LONG_CODE_X".
## A special single byte instruction is assigned an unused number greater than 128.
##
*/

enum hcl_bcode_t
{
	HCL_CODE_STORE_INTO_IVAR_0        = 0x00,
	HCL_CODE_STORE_INTO_IVAR_1        = 0x01,
	HCL_CODE_STORE_INTO_IVAR_2        = 0x02,
	HCL_CODE_STORE_INTO_IVAR_3        = 0x03,

	HCL_CODE_STORE_INTO_IVAR_4        = 0x04,
	HCL_CODE_STORE_INTO_IVAR_5        = 0x05,
	HCL_CODE_STORE_INTO_IVAR_6        = 0x06,
	HCL_CODE_STORE_INTO_IVAR_7        = 0x07,

	HCL_CODE_POP_INTO_IVAR_0          = 0x08,
	HCL_CODE_POP_INTO_IVAR_1          = 0x09,
	HCL_CODE_POP_INTO_IVAR_2          = 0x0A,
	HCL_CODE_POP_INTO_IVAR_3          = 0x0B,

	HCL_CODE_POP_INTO_IVAR_4          = 0x0C,
	HCL_CODE_POP_INTO_IVAR_5          = 0x0D,
	HCL_CODE_POP_INTO_IVAR_6          = 0x0E,
	HCL_CODE_POP_INTO_IVAR_7          = 0x0F,

	HCL_CODE_PUSH_IVAR_0              = 0x10,
	HCL_CODE_PUSH_IVAR_1              = 0x11,
	HCL_CODE_PUSH_IVAR_2              = 0x12,
	HCL_CODE_PUSH_IVAR_3              = 0x13,

	HCL_CODE_PUSH_IVAR_4              = 0x14,
	HCL_CODE_PUSH_IVAR_5              = 0x15,
	HCL_CODE_PUSH_IVAR_6              = 0x16,
	HCL_CODE_PUSH_IVAR_7              = 0x17,

	HCL_CODE_PUSH_TEMPVAR_0           = 0x18,
	HCL_CODE_PUSH_TEMPVAR_1           = 0x19,
	HCL_CODE_PUSH_TEMPVAR_2           = 0x1A,
	HCL_CODE_PUSH_TEMPVAR_3           = 0x1B,

	HCL_CODE_PUSH_TEMPVAR_4           = 0x1C,
	HCL_CODE_PUSH_TEMPVAR_5           = 0x1D,
	HCL_CODE_PUSH_TEMPVAR_6           = 0x1E,
	HCL_CODE_PUSH_TEMPVAR_7           = 0x1F,

	HCL_CODE_STORE_INTO_TEMPVAR_0     = 0x20,
	HCL_CODE_STORE_INTO_TEMPVAR_1     = 0x21,
	HCL_CODE_STORE_INTO_TEMPVAR_2     = 0x22,
	HCL_CODE_STORE_INTO_TEMPVAR_3     = 0x23,

	HCL_CODE_STORE_INTO_TEMPVAR_4     = 0x24,
	HCL_CODE_STORE_INTO_TEMPVAR_5     = 0x25,
	HCL_CODE_STORE_INTO_TEMPVAR_6     = 0x26,
	HCL_CODE_STORE_INTO_TEMPVAR_7     = 0x27,

	HCL_CODE_POP_INTO_TEMPVAR_0       = 0x28,
	HCL_CODE_POP_INTO_TEMPVAR_1       = 0x29,
	HCL_CODE_POP_INTO_TEMPVAR_2       = 0x2A,
	HCL_CODE_POP_INTO_TEMPVAR_3       = 0x2B,

	HCL_CODE_POP_INTO_TEMPVAR_4       = 0x2C,
	HCL_CODE_POP_INTO_TEMPVAR_5       = 0x2D,
	HCL_CODE_POP_INTO_TEMPVAR_6       = 0x2E,
	HCL_CODE_POP_INTO_TEMPVAR_7       = 0x2F,

	HCL_CODE_PUSH_LITERAL_0           = 0x30,
	HCL_CODE_PUSH_LITERAL_1           = 0x31,
	HCL_CODE_PUSH_LITERAL_2           = 0x32,
	HCL_CODE_PUSH_LITERAL_3           = 0x33,

	HCL_CODE_PUSH_LITERAL_4           = 0x34,
	HCL_CODE_PUSH_LITERAL_5           = 0x35,
	HCL_CODE_PUSH_LITERAL_6           = 0x36,
	HCL_CODE_PUSH_LITERAL_7           = 0x37,

	/* -------------------------------------- */

	HCL_CODE_STORE_INTO_OBJECT_0      = 0x38,
	HCL_CODE_STORE_INTO_OBJECT_1      = 0x39,
	HCL_CODE_STORE_INTO_OBJECT_2      = 0x3A,
	HCL_CODE_STORE_INTO_OBJECT_3      = 0x3B,

	HCL_CODE_POP_INTO_OBJECT_0        = 0x3C,
	HCL_CODE_POP_INTO_OBJECT_1        = 0x3D,
	HCL_CODE_POP_INTO_OBJECT_2        = 0x3E,
	HCL_CODE_POP_INTO_OBJECT_3        = 0x3F,

	HCL_CODE_PUSH_OBJECT_0            = 0x40,
	HCL_CODE_PUSH_OBJECT_1            = 0x41,
	HCL_CODE_PUSH_OBJECT_2            = 0x42,
	HCL_CODE_PUSH_OBJECT_3            = 0x43,

	HCL_CODE_JUMP_FORWARD_0           = 0x44, /* 68 */
	HCL_CODE_JUMP_FORWARD_1           = 0x45, /* 69 */
	HCL_CODE_JUMP_FORWARD_2           = 0x46, /* 70 */
	HCL_CODE_JUMP_FORWARD_3           = 0x47, /* 71 */

	HCL_CODE_JUMP_BACKWARD_0          = 0x48, /* 72 */
	HCL_CODE_JUMP_BACKWARD_1          = 0x49, /* 73 */
	HCL_CODE_JUMP_BACKWARD_2          = 0x4A, /* 74 */
	HCL_CODE_JUMP_BACKWARD_3          = 0x4B, /* 75 */

	/* UNUSED 0x4C - 0x53 */

	HCL_CODE_CALL_0                   = 0x54, /* 84 */
	HCL_CODE_CALL_1                   = 0x55, /* 85 */
	HCL_CODE_CALL_2                   = 0x56, /* 86 */
	HCL_CODE_CALL_3                   = 0x57, /* 87 */

	HCL_CODE_STORE_INTO_CTXTEMPVAR_0  = 0x58, /* 88 */
	HCL_CODE_STORE_INTO_CTXTEMPVAR_1  = 0x59, /* 89 */
	HCL_CODE_STORE_INTO_CTXTEMPVAR_2  = 0x5A, /* 90 */
	HCL_CODE_STORE_INTO_CTXTEMPVAR_3  = 0x5B, /* 91 */

	HCL_CODE_POP_INTO_CTXTEMPVAR_0    = 0x5C, /* 92 */
	HCL_CODE_POP_INTO_CTXTEMPVAR_1    = 0x5D, /* 93 */
	HCL_CODE_POP_INTO_CTXTEMPVAR_2    = 0x5E, /* 94 */
	HCL_CODE_POP_INTO_CTXTEMPVAR_3    = 0x5F, /* 95 */

	HCL_CODE_PUSH_CTXTEMPVAR_0        = 0x60, /* 96 */
	HCL_CODE_PUSH_CTXTEMPVAR_1        = 0x61, /* 97 */
	HCL_CODE_PUSH_CTXTEMPVAR_2        = 0x62, /* 98 */
	HCL_CODE_PUSH_CTXTEMPVAR_3        = 0x63, /* 99 */

	HCL_CODE_PUSH_OBJVAR_0            = 0x64,
	HCL_CODE_PUSH_OBJVAR_1            = 0x65,
	HCL_CODE_PUSH_OBJVAR_2            = 0x66,
	HCL_CODE_PUSH_OBJVAR_3            = 0x67,

	HCL_CODE_STORE_INTO_OBJVAR_0      = 0x68,
	HCL_CODE_STORE_INTO_OBJVAR_1      = 0x69,
	HCL_CODE_STORE_INTO_OBJVAR_2      = 0x6A,
	HCL_CODE_STORE_INTO_OBJVAR_3      = 0x6B,

	HCL_CODE_POP_INTO_OBJVAR_0        = 0x6C,
	HCL_CODE_POP_INTO_OBJVAR_1        = 0x6D,
	HCL_CODE_POP_INTO_OBJVAR_2        = 0x6E,
	HCL_CODE_POP_INTO_OBJVAR_3        = 0x6F,

	HCL_CODE_SEND_0                   = 0x70, /* 112 */
	HCL_CODE_SEND_1                   = 0x71, /* 113 */
	HCL_CODE_SEND_2                   = 0x72, /* 114 */
	HCL_CODE_SEND_3                   = 0x73, /* 115 */

	HCL_CODE_SEND_TO_SUPER_0          = 0x74, /* 116 */
	HCL_CODE_SEND_TO_SUPER_1          = 0x75, /* 117 */
	HCL_CODE_SEND_TO_SUPER_2          = 0x76, /* 118 */
	HCL_CODE_SEND_TO_SUPER_3          = 0x77, /* 119 */

	HCL_CODE_PUSH_CVAR_I_X            = 0x78, /* 120 */
	HCL_CODE_STORE_INTO_CVAR_I_X      = 0x79, /* 121 */
	HCL_CODE_POP_INTO_CVAR_I_X        = 0x7A, /* 122 */

	HCL_CODE_PUSH_CVAR_M_X            = 0x7B, /* 123 */
	HCL_CODE_STORE_INTO_CVAR_M_X      = 0x7C, /* 124 */
	HCL_CODE_POP_INTO_CVAR_M_X        = 0x7D, /* 125 */

	/* UNUSED 0x7E - 0x7F */
	HCL_CODE_STORE_INTO_IVAR_X        = 0x80, /* 128 */

	HCL_CODE_PUSH_RECEIVER            = 0x81, /* 129 */
	HCL_CODE_PUSH_NIL                 = 0x82, /* 130 */
	HCL_CODE_PUSH_TRUE                = 0x83, /* 131 */
	HCL_CODE_PUSH_FALSE               = 0x84, /* 132 */
	HCL_CODE_PUSH_CONTEXT             = 0x85, /* 133 */
	HCL_CODE_PUSH_PROCESS             = 0x86, /* 134 */
	/* UNUSED 0x87 */

	HCL_CODE_POP_INTO_IVAR_X          = 0x88, /* 136 ## */

	HCL_CODE_PUSH_NEGONE              = 0x89, /* 137 */
	HCL_CODE_PUSH_ZERO                = 0x8A, /* 138 */
	HCL_CODE_PUSH_ONE                 = 0x8B, /* 139 */
	HCL_CODE_PUSH_TWO                 = 0x8C, /* 140 */

	HCL_CODE_PUSH_IVAR_X              = 0x90, /* 144 ## */
	HCL_CODE_PUSH_TEMPVAR_X           = 0x98, /* 152 ## */
	HCL_CODE_STORE_INTO_TEMPVAR_X     = 0xA0, /* 160 ## */
	HCL_CODE_POP_INTO_TEMPVAR_X       = 0xA8, /* 168 ## */

	HCL_CODE_PUSH_LITERAL_X           = 0xB0, /* 176 ## */
	HCL_CODE_PUSH_LITERAL_X2          = 0xB1, /* 177 */

	HCL_CODE_PUSH_INTLIT              = 0xB2, /* 178 */
	HCL_CODE_PUSH_NEGINTLIT           = 0xB3, /* 179 */
	HCL_CODE_PUSH_CHARLIT             = 0xB4, /* 180 */

	HCL_CODE_PLUS = 0xB5, /* 181 TOOD: move it to a lower code number later after killing OBJVAR instructions */
	/* UNUSED - 0xB6 - 0xB7 */

	HCL_CODE_STORE_INTO_OBJECT_X      = 0xB8, /* 184 ## */
	HCL_CODE_POP_INTO_OBJECT_X        = 0xBC, /* 188 ## */
	HCL_CODE_PUSH_OBJECT_X            = 0xC0, /* 192 ## */

	/* UNUSED - 0xC1 - 0xC3 */

	HCL_CODE_JUMP_FORWARD_X           = 0xC4, /* 196 ## */
	HCL_CODE_JUMP2_FORWARD            = 0xC5, /* 197 */

	/* UNUSED - 0xC6 - 0xC7 */

	HCL_CODE_JUMP_BACKWARD_X          = 0xC8, /* 200 ## */
	HCL_CODE_JUMP2_BACKWARD           = 0xC9, /* 201 */

	/* UNUSED - 0xCA - 0xCB */

	HCL_CODE_JUMP_FORWARD_IF_TRUE     = 0xCC, /* 204 ## */
	HCL_CODE_JUMP2_FORWARD_IF_TRUE    = 0xCD, /* 205 */
	HCL_CODE_JUMP_BACKWARD_IF_TRUE    = 0xCE, /* 206 ## */
	HCL_CODE_JUMP2_BACKWARD_IF_TRUE   = 0xCF, /* 207 */

	HCL_CODE_JUMP_FORWARD_IF_FALSE    = 0xD0, /* 208 ## */
	HCL_CODE_JUMP2_FORWARD_IF_FALSE   = 0xD1, /* 209 */
	HCL_CODE_JUMP_BACKWARD_IF_FALSE   = 0xD2, /* 210 ## */
	HCL_CODE_JUMP2_BACKWARD_IF_FALSE  = 0xD3, /* 211 */

	HCL_CODE_CALL_X                   = 0xD4, /* 212 ## */
	HCL_CODE_CALL_R                   = 0xD5, /* 213 ## ##*/
	HCL_CODE_PUSH_RETURN_R            = 0xD6, /* 214 */
	HCL_CODE_TRY_ENTER                = 0xD7, /* 215 ## */

	HCL_CODE_STORE_INTO_CTXTEMPVAR_X  = 0xD8, /* 216 ## */
	HCL_CODE_TRY_ENTER2               = 0xD9, /* 217 ## */
	HCL_CODE_TRY_EXIT                 = 0xDA, /* 218 */
	HCL_CODE_THROW                    = 0xDB, /* 219 */

	HCL_CODE_POP_INTO_CTXTEMPVAR_X    = 0xDC, /* 220 ## */
	HCL_CODE_CLASS_LOAD               = 0xDD, /* 221 ## */

	/* UNUSED - 0xDE - 0xDF */

	HCL_CODE_PUSH_CTXTEMPVAR_X        = 0xE0, /* 224 ## */
	HCL_CODE_CLASS_ENTER              = 0xE1, /* 225 ## */
	HCL_CODE_CLASS_EXIT               = 0xE2, /* 226 */
	HCL_CODE_CLASS_PEXIT              = 0xE3, /* 227 */

	HCL_CODE_PUSH_OBJVAR_X            = 0xE4, /* 228 ## */
	HCL_CODE_CLASS_CMSTORE            = 0xE5, /* 229 */
	HCL_CODE_CLASS_CIMSTORE           = 0xE6, /* 230 */
	HCL_CODE_CLASS_IMSTORE            = 0xE7, /* 231 */

	HCL_CODE_STORE_INTO_OBJVAR_X      = 0xE8, /* 232 ## */
	HCL_CODE_MAKE_ARRAY               = 0xE9, /* 233 ## */
	HCL_CODE_MAKE_BYTEARRAY           = 0xEA, /* 234 ## */
	HCL_CODE_MAKE_DIC                 = 0xEB, /* 235 ## */

	HCL_CODE_POP_INTO_OBJVAR_X        = 0xEC, /* 236 ## */
	HCL_CODE_POP_INTO_ARRAY           = 0xED, /* 237 ## */
	HCL_CODE_POP_INTO_BYTEARRAY       = 0xEE, /* 238 ## */
	HCL_CODE_POP_INTO_DIC             = 0xEF, /* 239 */

	HCL_CODE_SEND_X                   = 0xF0, /* 240 ## */
	HCL_CODE_SEND_R                   = 0xF1, /* 241 ## ## - [NOTE] ((code >> 2) & 1) must be 0 */

	HCL_CODE_MAKE_CONS                = 0xF2, /* 242 */
	HCL_CODE_POP_INTO_CONS            = 0xF3, /* 243 */

	HCL_CODE_SEND_TO_SUPER_X          = 0xF4, /* 244 ## */
	HCL_CODE_SEND_TO_SUPER_R          = 0xF5, /* 245 ## ## - [NOTE] ((code >> 2) & 1) must be 0 */

	HCL_CODE_POP_INTO_CONS_END        = 0xF6, /* 246 */
	HCL_CODE_POP_INTO_CONS_CDR        = 0xF7, /* 247 */
	/* -------------------------------------- */

	HCL_CODE_DUP_STACKTOP             = 0xF8, /* 248 */
	HCL_CODE_POP_STACKTOP             = 0xF9, /* 249 */
	HCL_CODE_RETURN_STACKTOP          = 0xFA, /* 250 */
	HCL_CODE_RETURN_RECEIVER          = 0xFB, /* 251 */
	HCL_CODE_RETURN_FROM_BLOCK        = 0xFC, /* 252, return the stack top from a block */

	HCL_CODE_MAKE_FUNCTION            = 0xFD, /* 253 */
	HCL_CODE_MAKE_LAMBDA               = 0xFE, /* 254 */
	HCL_CODE_NOOP                     = 0xFF  /* 255 */
};



typedef hcl_ooi_t (*hcl_outbfmt_t) (
	hcl_t*           hcl,
	hcl_bitmask_t  mask,
	const hcl_bch_t* fmt,
	...
);

/* i don't want an error raised inside the callback to override
 * the existing error number and message. */
#define HCL_VMPRIM_LOG_WRITE(hcl,mask,ptr,len) do { \
		int shuterr = (hcl)->shuterr; \
		(hcl)->shuterr = 1; \
		(hcl)->vmprim.log_write (hcl, mask, ptr, len); \
		(hcl)->shuterr = shuterr; \
	} while(0)


#define HCL_CHAR_TO_NUM(c,base) \
        ((c >= '0' && c <= '9')? ((c - '0' < base)? (c - '0'): base): \
         (c >= 'A' && c <= 'Z')? ((c - 'A' + 10 < base)? (c - 'A' + 10): base): \
         (c >= 'a' && c <= 'z')? ((c - 'a' + 10 < base)? (c - 'a' + 10): base): base)

#if defined(__cplusplus)
extern "C" {
#endif

/* ========================================================================= */
/* heap.c                                                                    */
/* ========================================================================= */

/**
 * The hcl_makeheap() function creates a new heap of the \a size bytes.
 *
 * \return heap pointer on success and #HCL_NULL on failure.
 */
hcl_heap_t* hcl_makeheap (
	hcl_t*     hcl,
	hcl_oow_t  size
);

/**
 * The hcl_killheap() function destroys the heap pointed to by \a heap.
 */
void hcl_killheap (
	hcl_t*      hcl,
	hcl_heap_t* heap
);

/**
 * The hcl_allocheapmem() function allocates \a size bytes from the given heap
 * and clears it with zeros.
 */
void* hcl_callocheapmem (
	hcl_t*       hcl,
	hcl_heap_t*  heap,
	hcl_oow_t    size
);

void* hcl_callocheapmem_noseterr (
	hcl_t*       hcl,
	hcl_heap_t*  heap,
	hcl_oow_t    size
);

void hcl_freeheapmem (
	hcl_t*       hcl,
	hcl_heap_t*  heap,
	void*        ptr
);

/* ========================================================================= */
/* obj.c                                                                     */
/* ========================================================================= */
void* hcl_allocbytes (
	hcl_t*     hcl,
	hcl_oow_t  size
);

/**
 * The hcl_allocoopobj() function allocates a raw object composed of \a size
 * pointer fields excluding the header.
 */
hcl_oop_t hcl_allocoopobj (
	hcl_t*    hcl,
	int       brand,
	hcl_oow_t size
);

hcl_oop_t hcl_allocoopobjwithtrailer (
	hcl_t*           hcl,
	int              brand,
	hcl_oow_t        size,
	const hcl_oob_t* tptr,
	hcl_oow_t        tlen
);

hcl_oop_t hcl_alloccharobj (
	hcl_t*            hcl,
	int               brand,
	const hcl_ooch_t* ptr,
	hcl_oow_t         len
);

hcl_oop_t hcl_allocbyteobj (
	hcl_t*            hcl,
	int               brand,
	const hcl_oob_t*  ptr,
	hcl_oow_t         len
);

hcl_oop_t hcl_allochalfwordobj (
	hcl_t*            hcl,
	int               brand,
	const hcl_oohw_t* ptr,
	hcl_oow_t         len
);

hcl_oop_t hcl_allocwordobj (
	hcl_t*           hcl,
	int               brand,
	const hcl_oow_t* ptr,
	hcl_oow_t        len
);

hcl_oop_t hcl_instantiate (
	hcl_t*          hcl,
	hcl_oop_class_t _class,
	const void*     vptr,
	hcl_oow_t       vlen
);

hcl_oop_t hcl_instantiatewithtrailer (
	hcl_t*           hcl,
	hcl_oop_class_t  _class,
	hcl_oow_t        vlen,
	const hcl_oob_t* trptr,
	hcl_oow_t        trlen
);

/* ========================================================================= */
/* sym.c                                                                     */
/* ========================================================================= */
hcl_oop_t hcl_makesymbol (
	hcl_t*             hcl,
	const hcl_ooch_t*  ptr,
	hcl_oow_t          len
);

hcl_oop_t hcl_makesymbolwithbcstr (
	hcl_t*             hcl,
	const hcl_bch_t*   ptr
);

hcl_oop_t hcl_makesymbolwithucstr (
	hcl_t*             hcl,
	const hcl_uch_t*   ptr
);

hcl_oop_t hcl_findsymbol (
	hcl_t*             hcl,
	const hcl_ooch_t*  ptr,
	hcl_oow_t          len
);


/* ========================================================================= */
/* proc.c                                                                    */
/* ========================================================================= */
hcl_oop_process_t hcl_makeproc (
	hcl_t* hcl
);

/* ========================================================================= */
/* gc.c                                                                    */
/* ========================================================================= */

hcl_oow_t hcl_getobjpayloadbytes (
	hcl_t*    hcl,
	hcl_oop_t oop
);

void hcl_gc_ms_sweep_lazy (
	hcl_t*    hcl,
	hcl_oow_t allocsize
);

hcl_syncode_t hcl_getsyncodebyoocs_noseterr (
	hcl_t*            hcl,
	const hcl_oocs_t* name
);

hcl_syncode_t hcl_getsyncode_noseterr (
	hcl_t*            hcl,
	const hcl_ooch_t* ptr,
	const hcl_oow_t   len
);

const hcl_ooch_t* hcl_getsyncodename_noseterr (
	hcl_t*        hcl,
	hcl_syncode_t syncode
);


/* ========================================================================= */
/* utf8.c                                                                    */
/* ========================================================================= */
hcl_oow_t hcl_uc_to_utf8 (
	hcl_uch_t    uc,
	hcl_bch_t*   utf8,
	hcl_oow_t    size
);

hcl_oow_t hcl_utf8_to_uc (
	const hcl_bch_t* utf8,
	hcl_oow_t        size,
	hcl_uch_t*       uc
);

int hcl_ucstoutf8 (
	const hcl_uch_t*    ucs,
	hcl_oow_t*          ucslen,
	hcl_bch_t*          bcs,
	hcl_oow_t*          bcslen
);

/**
 * The hcl_utf8_to_ucs() function converts a UTF8 string to a uncide string.
 *
 * It never returns -2 if \a ucs is #HCL_NULL.
 *
 * \code
 *  const hcl_bch_t* bcs = "test string";
 *  hcl_uch_t ucs[100];
 *  hcl_oow_t ucslen = HCL_COUNTOF(buf), n;
 *  hcl_oow_t bcslen = 11;
 *  int n;
 *  n = hcl_utf8_to_ucs (bcs, &bcslen, ucs, &ucslen);
 *  if (n <= -1) { invalid/incomplenete sequence or buffer to small }
 * \endcode
 *
 * For a null-terminated string, you can specify ~(hcl_oow_t)0 in
 * \a bcslen. The destination buffer \a ucs also must be large enough to
 * store a terminating null. Otherwise, -2 is returned.
 *
 * The resulting \a ucslen can still be greater than 0 even if the return
 * value is negative. The value indiates the number of characters converted
 * before the error has occurred.
 *
 * \return 0 on success.
 *         -1 if \a bcs contains an illegal character.
 *         -2 if the wide-character string buffer is too small.
 *         -3 if \a bcs is not a complete sequence.
 */
int hcl_utf8_to_ucs (
	const hcl_bch_t*   bcs,
	hcl_oow_t*         bcslen,
	hcl_uch_t*         ucs,
	hcl_oow_t*         ucslen
);

/* ========================================================================= */
/* bigint.c                                                                  */
/* ========================================================================= */
static HCL_INLINE int hcl_isbigint (hcl_t* hcl, hcl_oop_t x)
{
	return HCL_IS_BIGINT(hcl, x);
}

static HCL_INLINE int hcl_isint (hcl_t* hcl, hcl_oop_t x)
{
	return HCL_OOP_IS_SMOOI(x) || HCL_IS_BIGINT(hcl, x);
}

hcl_oop_t hcl_addints (
	hcl_t*    hcl,
	hcl_oop_t x,
	hcl_oop_t y
);

hcl_oop_t hcl_subints (
	hcl_t*    hcl,
	hcl_oop_t x,
	hcl_oop_t y
);

hcl_oop_t hcl_mulints (
	hcl_t*    hcl,
	hcl_oop_t x,
	hcl_oop_t y
);

hcl_oop_t hcl_divints (
	hcl_t*     hcl,
	hcl_oop_t  x,
	hcl_oop_t  y,
	int        modulo,
	hcl_oop_t* rem
);

hcl_oop_t hcl_negateint (
	hcl_t*    hcl,
	hcl_oop_t x
);

hcl_oop_t hcl_bitatint (
	hcl_t*    hcl,
	hcl_oop_t x,
	hcl_oop_t y
);

hcl_oop_t hcl_bitandints (
	hcl_t*    hcl,
	hcl_oop_t x,
	hcl_oop_t y
);

hcl_oop_t hcl_bitorints (
	hcl_t*    hcl,
	hcl_oop_t x,
	hcl_oop_t y
);

hcl_oop_t hcl_bitxorints (
	hcl_t*    hcl,
	hcl_oop_t x,
	hcl_oop_t y
);

hcl_oop_t hcl_bitinvint (
	hcl_t*    hcl,
	hcl_oop_t x
);

hcl_oop_t hcl_bitshiftint (
	hcl_t*    hcl,
	hcl_oop_t x,
	hcl_oop_t y
);

hcl_oop_t hcl_eqints (
	hcl_t* hcl,
	hcl_oop_t x,
	hcl_oop_t y
);

hcl_oop_t hcl_neints (
	hcl_t*    hcl,
	hcl_oop_t x,
	hcl_oop_t y
);

hcl_oop_t hcl_gtints (
	hcl_t*    hcl,
	hcl_oop_t x,
	hcl_oop_t y
);

hcl_oop_t hcl_geints (
	hcl_t*    hcl,
	hcl_oop_t x,
	hcl_oop_t y
);

hcl_oop_t hcl_ltints (
	hcl_t*    hcl,
	hcl_oop_t x,
	hcl_oop_t y
);

hcl_oop_t hcl_leints (
	hcl_t*    hcl,
	hcl_oop_t x,
	hcl_oop_t y
);

hcl_oop_t hcl_sqrtint (
	hcl_t*    hcl,
	hcl_oop_t x
);

hcl_oop_t hcl_absint (
	hcl_t*    hcl,
	hcl_oop_t x
);

hcl_oop_t hcl_strtoint (
	hcl_t*            hcl,
	const hcl_ooch_t* str,
	hcl_oow_t         len,
	int               radix
);


#define HCL_INTTOSTR_RADIXMASK (0xFF)
#define HCL_INTTOSTR_LOWERCASE (1 << 8)
#define HCL_INTTOSTR_NONEWOBJ  (1 << 9)
/**
 * The hcl_inttostr() function converts an integer object to a string object
 * printed in the given radix. If HCL_INTTOSTR_NONEWOBJ is set in flags_radix,
 * it returns hcl->_nil but keeps the result in the buffer pointed to by
 * hcl->inttostr.xbuf.ptr with the length stored in hcl->inttostr.xbuf.len.
 * If the function fails, it returns #HCL_NULL.
 */
hcl_oop_t hcl_inttostr (
	hcl_t*      hcl,
	hcl_oop_t   num,
	int         flagged_radix /* radix between 2 and 36 inclusive, optionally bitwise ORed of HCL_INTTOSTR_XXX bits */
);

/* ========================================================================= */
/* number.c                                                                    */
/* ========================================================================= */
hcl_oop_t hcl_addnums (
	hcl_t*       hcl,
	hcl_oop_t    x,
	hcl_oop_t    y
);

hcl_oop_t hcl_subnums (
	hcl_t*       hcl,
	hcl_oop_t    x,
	hcl_oop_t    y
);

hcl_oop_t hcl_mulnums (
	hcl_t*       hcl,
	hcl_oop_t    x,
	hcl_oop_t    y
);

hcl_oop_t hcl_mltnums (
	hcl_t*       hcl,
	hcl_oop_t    x,
	hcl_oop_t    y
);

hcl_oop_t hcl_divnums (
	hcl_t*       hcl,
	hcl_oop_t    x,
	hcl_oop_t    y
);

hcl_oop_t hcl_truncfpdecval (
	hcl_t*       hcl,
	hcl_oop_t    iv, /* integer */
	hcl_ooi_t    cs, /* current scale */
	hcl_ooi_t    ns  /* new scale */
);

hcl_oop_t hcl_gtnums (
	hcl_t*       hcl,
	hcl_oop_t    x,
	hcl_oop_t    y
);

hcl_oop_t hcl_genums (
	hcl_t*       hcl,
	hcl_oop_t    x,
	hcl_oop_t    y
);

hcl_oop_t hcl_ltnums (
	hcl_t*       hcl,
	hcl_oop_t    x,
	hcl_oop_t    y
);

hcl_oop_t hcl_lenums (
	hcl_t*       hcl,
	hcl_oop_t    x,
	hcl_oop_t    y
);

hcl_oop_t hcl_eqnums (
	hcl_t*       hcl,
	hcl_oop_t    x,
	hcl_oop_t    y
);

hcl_oop_t hcl_nenums (
	hcl_t*       hcl,
	hcl_oop_t    x,
	hcl_oop_t    y
);

hcl_oop_t hcl_sqrtnum (
	hcl_t*       hcl,
	hcl_oop_t    x
);

hcl_oop_t hcl_absnum (
	hcl_t*       hcl,
	hcl_oop_t    x
);

/* ========================================================================= */
/* hcl.c                                                                     */
/* ========================================================================= */

hcl_mod_data_t* hcl_openmod (
	hcl_t*            hcl,
	const hcl_ooch_t* name,
	hcl_oow_t         namelen
);

void hcl_closemod (
	hcl_t*            hcl,
	hcl_mod_data_t*   mdp
);

/*
 * The hcl_querymod() function finds a primitive function in modules
 * with a full primitive identifier.
 */
hcl_pfbase_t* hcl_querymod (
	hcl_t*            hcl,
	const hcl_ooch_t* pfid,
	hcl_oow_t         pfidlen,
	hcl_mod_t**       mod
);

/* ========================================================================= */
/* fmt.c                                                                     */
/* ========================================================================= */
int hcl_fmt_object (
	hcl_t*        hcl,
	hcl_fmtout_t* fmtout,
	hcl_oop_t     oop
);

int hcl_prfmtcallstack (
	hcl_t*    hcl,
	hcl_ooi_t nargs
);

int hcl_logfmtcallstack (
	hcl_t*    hcl,
	hcl_ooi_t nargs
);

int hcl_strfmtcallstack (
	hcl_t*    hcl,
	hcl_ooi_t nargs
);

int hcl_scfmtcallstack (
	hcl_t*    hcl,
	hcl_ooi_t nargs
);

/* ========================================================================= */
/* comp.c                                                                    */
/* ========================================================================= */
int hcl_emitbyteinstruction (
	hcl_t*     hcl,
	hcl_oob_t  bc
);

/* ========================================================================= */
/* cnode.c                                                                   */
/* ========================================================================= */
hcl_cnode_t* hcl_makecnodenil (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodetrue (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodefalse (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodeself (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodesuper (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodeellipsis (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodetrpcolons (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodedblcolons (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodecolongt (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodecolonlt (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodecolonstar (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodecharlit (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok, hcl_ooch_t v);
hcl_cnode_t* hcl_makecnodebchrlit (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok, hcl_oob_t v);
hcl_cnode_t* hcl_makecnodesymbol (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodedsymbol (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok, int is_cla);
hcl_cnode_t* hcl_makecnodestrlit (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodebstrlit (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodenumlit (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnoderadnumlit (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodefpdeclit (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok);
hcl_cnode_t* hcl_makecnodesmptrlit (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok, hcl_oow_t v);
hcl_cnode_t* hcl_makecnodeerrlit (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok, hcl_ooi_t v);
hcl_cnode_t* hcl_makecnodecons (hcl_t* hcl, int flags, const hcl_loc_t* loc, const hcl_oocs_t* tok, hcl_cnode_t* car, hcl_cnode_t* cdr);
hcl_cnode_t* hcl_makecnodeelist (hcl_t* hcl, int flags, const hcl_loc_t* loc, hcl_concode_t type);
hcl_cnode_t* hcl_makecnodeshell (hcl_t* hcl, int flags, const hcl_loc_t* loc, hcl_cnode_t* obj);
void hcl_freesinglecnode (hcl_t* hcl, hcl_cnode_t* c);
hcl_oow_t hcl_countcnodecons (hcl_t* hcl, hcl_cnode_t* cons);
void hcl_dumpcnode (hcl_t* hcl,  hcl_cnode_t* c, int newline);


/* ========================================================================= */
/* exec.c                                                                    */
/* ========================================================================= */
hcl_pfrc_t hcl_pf_process_current (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);
hcl_pfrc_t hcl_pf_process_fork (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);
hcl_pfrc_t hcl_pf_process_resume (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);
hcl_pfrc_t hcl_pf_process_suspend (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);
hcl_pfrc_t hcl_pf_process_terminate (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);
hcl_pfrc_t hcl_pf_process_terminate_all (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);
hcl_pfrc_t hcl_pf_process_yield (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);

hcl_pfrc_t hcl_pf_semaphore_new (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);
hcl_pfrc_t hcl_pf_semaphore_wait (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);
hcl_pfrc_t hcl_pf_semaphore_signal (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);
hcl_pfrc_t hcl_pf_semaphore_signal_timed (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);
hcl_pfrc_t hcl_pf_semaphore_signal_on_input (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);
hcl_pfrc_t hcl_pf_semaphore_signal_on_output (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);
/*hcl_pfrc_t hcl_pf_semaphore_signal_on_gcfin (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);*/
hcl_pfrc_t hcl_pf_semaphore_unsignal (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);

hcl_pfrc_t hcl_pf_semaphore_group_new (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);
hcl_pfrc_t hcl_pf_semaphore_group_add_semaphore (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);
hcl_pfrc_t hcl_pf_semaphore_group_remove_semaphore (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);
hcl_pfrc_t hcl_pf_semaphore_group_wait (hcl_t* hcl, hcl_mod_t* mod, hcl_ooi_t nargs);

/* ========================================================================= */
/* std.c                                                                    */
/* ========================================================================= */
hcl_errnum_t hcl_syserrstrb (hcl_t* hcl, int syserr_type, int syserr_code, hcl_bch_t* buf, hcl_oow_t len);

#if defined(__cplusplus)
}
#endif

#endif
