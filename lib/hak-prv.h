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

#ifndef _HAK_PRV_H_
#define _HAK_PRV_H_

#include <hak.h>
#include <hak-chr.h>
#include <hak-cmgr.h>
#include <hak-fmt.h>
#include <hak-str.h>
#include <hak-utl.h>

/* you can define this to either 1 or 2 */
#define HAK_CODE_LONG_PARAM_SIZE 2

/* this is useful for debugging. hak_gc() can be called
 * while hak has not been fully initialized when this is defined*/
#define HAK_SUPPORT_GC_DURING_IGNITION

/* define this to enable karatsuba multiplication in bigint */
#define HAK_ENABLE_KARATSUBA
#define HAK_KARATSUBA_CUTOFF 32
#define HAK_KARATSUBA_CUTOFF_DEBUG 3

/* enable floating-pointer number support in the basic formatting functions */
#define HAK_ENABLE_FLTFMT

#if defined(HAK_BUILD_DEBUG)
/*#define HAK_DEBUG_LEXER 1*/
#define HAK_DEBUG_VM_PROCESSOR 1
#define HAK_DEBUG_VM_EXEC 1
/*#define HAK_PROFILE_VM 1*/
#endif

/* allow the caller to drive process switching by calling
 * stix_switchprocess(). */
#define HAK_EXTERNAL_PROCESS_SWITCH

/* limit the maximum object size such that:
 *   1. an index to an object field can be represented in a small integer.
 *   2. the maximum number of bits including bit-shifts can be represented
 *      in the hak_oow_t type.
 */
#define HAK_LIMIT_OBJ_SIZE


#define HAK_BC_BUFFER_INIT  10240
#define HAK_BC_BUFFER_ALIGN 10240

#define HAK_LIT_BUFFER_INIT 1024
#define HAK_LIT_BUFFER_ALIGN 1024

#if defined(__has_builtin)

#	if (!__has_builtin(__builtin_memset) || !__has_builtin(__builtin_memcpy) || !__has_builtin(__builtin_memmove) || !__has_builtin(__builtin_memcmp))
#	include <string.h>
#	endif

#	if __has_builtin(__builtin_memset)
#		define HAK_MEMSET(dst,src,size)  __builtin_memset(dst,src,size)
#	else
#		define HAK_MEMSET(dst,src,size)  memset(dst,src,size)
#	endif
#	if __has_builtin(__builtin_memcpy)
#		define HAK_MEMCPY(dst,src,size)  __builtin_memcpy(dst,src,size)
#	else
#		define HAK_MEMCPY(dst,src,size)  memcpy(dst,src,size)
#	endif
#	if __has_builtin(__builtin_memmove)
#		define HAK_MEMMOVE(dst,src,size)  __builtin_memmove(dst,src,size)
#	else
#		define HAK_MEMMOVE(dst,src,size)  memmove(dst,src,size)
#	endif
#	if __has_builtin(__builtin_memcmp)
#		define HAK_MEMCMP(dst,src,size)  __builtin_memcmp(dst,src,size)
#	else
#		define HAK_MEMCMP(dst,src,size)  memcmp(dst,src,size)
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
#		define HAK_MEMSET(dst,src,size)  __builtin_memset(dst,src,size)
#       else
#		define HAK_MEMSET(dst,src,size)  memset(dst,src,size)
#       endif
#       if defined(HAVE___BUILTIN_MEMCPY)
#		define HAK_MEMCPY(dst,src,size)  __builtin_memcpy(dst,src,size)
#       else
#		define HAK_MEMCPY(dst,src,size)  memcpy(dst,src,size)
#       endif
#       if defined(HAVE___BUILTIN_MEMMOVE)
#		define HAK_MEMMOVE(dst,src,size)  __builtin_memmove(dst,src,size)
#       else
#		define HAK_MEMMOVE(dst,src,size)  memmove(dst,src,size)
#       endif
#       if defined(HAVE___BUILTIN_MEMCMP)
#		define HAK_MEMCMP(dst,src,size)  __builtin_memcmp(dst,src,size)
#       else
#		define HAK_MEMCMP(dst,src,size)  memcmp(dst,src,size)
#       endif

#endif

#if defined(HAK_LIMIT_OBJ_SIZE)
/* limit the maximum object size such that:
 *   1. an index to an object field can be represented in a small integer.
 *   2. the maximum number of bit shifts can be represented in the hak_oow_t type.
 */
#	define HAK_OBJ_SIZE_MAX ((hak_oow_t)HAK_SMOOI_MAX)
#	define HAK_OBJ_SIZE_BITS_MAX (HAK_OBJ_SIZE_MAX * HAK_BITS_PER_BYTE)
#else
#	define HAK_OBJ_SIZE_MAX ((hak_oow_t)HAK_TYPE_MAX(hak_oow_t))
#	define HAK_OBJ_SIZE_BITS_MAX (HAK_OBJ_SIZE_MAX * HAK_BITS_PER_BYTE)
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
 * For example, on a platform where sizeof(hak_oow_t) is 4,
 * the layout of the spec field of a class as an OOP value looks like this:
 *
 *  31                          12 11  10 9 8 7 6 5 4 3 2   1 0
 * |number of named instance variables|indexed-type|flags |oop-tag|
 *
 * the number of named instance variables is stored in high 21 bits.
 * the indexed type takes up bit 5 to bit 10 (assuming HAK_OBJ_TYPE_BITS is 6.
 * HAK_OBJ_TYPE_XXX enumerators are used to represent actual values).
 * and the indexability is stored in the flag bits which span from bit 2 to 4.
 *
 * The maximum number of named(fixed) instance variables for a class is:
 *     2 ^ ((BITS-IN-OOW - HAK_OOP_TAG_BITS_LO) - HAK_OBJ_TYPE_BITS - 1 - 2) - 1
 *
 * HAK_OOP_TAG_BITS_LO are decremented from the number of bits in OOW because
 * the spec field is always encoded as a small integer.
 *
 * The number of named instance variables can be greater than 0 if the
 * class spec is not indexed or if it's a pointer indexed class
 * (indexed_type == HAK_OBJ_TYPE_OOP)
 *
 * indexed_type is one of the #hak_obj_type_t enumerators.
 */

#define HAK_CLASS_SPEC_FLAG_BITS (3)

/*
 * The HAK_CLASS_SPEC_MAKE() macro creates a class spec value.
 *  _class->spec = HAK_SMOOI_TO_OOP(HAK_CLASS_SPEC_MAKE(0, 1, HAK_OBJ_TYPE_CHAR));
 */
#define HAK_CLASS_SPEC_MAKE(named_instvar,flags,indexed_type) ( \
	(((hak_oow_t)(named_instvar)) << (HAK_OBJ_FLAGS_TYPE_BITS + HAK_CLASS_SPEC_FLAG_BITS)) |  \
	(((hak_oow_t)(indexed_type)) << (HAK_CLASS_SPEC_FLAG_BITS)) | (((hak_oow_t)flags) & HAK_LBMASK(hak_oow_t,HAK_CLASS_SPEC_FLAG_BITS)))

/* what is the number of named instance variables?
 *  HAK_CLASS_SPEC_NAMED_INSTVARS(HAK_OOP_TO_SMOOI(_class->spec))
 * ensure to update Class<<specNumInstVars if you change this macro. */
#define HAK_CLASS_SPEC_NAMED_INSTVARS(spec) \
	(((hak_oow_t)(spec)) >> (HAK_OBJ_FLAGS_TYPE_BITS + HAK_CLASS_SPEC_FLAG_BITS))

/* is it a user-indexable class?
 * all objects can be indexed with basicAt:.
 * this indicates if an object can be instantiated with a dynamic size
 * (new: size) and and can be indexed with at:.
 */
#define HAK_CLASS_SPEC_FLAGS(spec) (((hak_oow_t)(spec)) & HAK_LBMASK(hak_oow_t,HAK_CLASS_SPEC_FLAG_BITS))

/* if so, what is the indexing type? character? pointer? etc? */
#define HAK_CLASS_SPEC_INDEXED_TYPE(spec) \
	((((hak_oow_t)(spec)) >> HAK_CLASS_SPEC_FLAG_BITS) & HAK_LBMASK(hak_oow_t, HAK_OBJ_FLAGS_TYPE_BITS))

/* If you add more than 4 items, you must update code related to CLASS_ENTER instruction
 * and class attributes as well as HAK_CLASS_SPEC_FLAG_BITS. */
#define HAK_CLASS_SPEC_FLAG_INDEXED    (1 << 0)
#define HAK_CLASS_SPEC_FLAG_IMMUTABLE  (1 << 1)
#define HAK_CLASS_SPEC_FLAG_UNCOPYABLE (1 << 2)

#define HAK_CLASS_SPEC_IS_INDEXED(spec) (HAK_CLASS_SPEC_FLAGS(spec) & HAK_CLASS_SPEC_FLAG_INDEXED)
#define HAK_CLASS_SPEC_IS_IMMUTABLE(spec) (HAK_CLASS_SPEC_FLAGS(spec) & HAK_CLASS_SPEC_FLAG_IMMUTABLE)
#define HAK_CLASS_SPEC_IS_UNCOPYABLE(spec) (HAK_CLASS_SPEC_FLAGS(spec) & HAK_CLASS_SPEC_FLAG_UNCOPYABLE)

/* What is the maximum number of named instance variables?
 * This limit is set this way because the number must be encoded into
 * the spec field of the class with limited number of bits assigned to
 * the number of named instance variables.
 */
#define HAK_CLASS_SPEC_INSTVAR_BITS (HAK_SMOOI_ABS_BITS - (HAK_OBJ_FLAGS_TYPE_BITS + HAK_CLASS_SPEC_FLAG_BITS))
#define HAK_MAX_NAMED_INSTVARS HAK_BITS_MAX(hak_oow_t, HAK_CLASS_SPEC_INSTVAR_BITS)

/* Given the number of named instance variables, what is the maximum number
 * of indexed instance variables? The number of indexed instance variables
 * is not stored in the spec field of the class. It only affects the actual
 * size of an object(obj->_size) selectively combined with the number of
 * named instance variables. So it's the maximum value of obj->_size minus
 * the number of named instance variables.
 */
#define HAK_MAX_INDEXED_INSTVARS(named_instvar) (HAK_OBJ_SIZE_MAX - named_instvar)

/*
 * self-specification of a class
 *   | classinstvars     | classvars         | flags |
 *
 * When converted to a small integer
 *   | sign-bit | classinstvars | classvars | flags | tag |
 */
#define HAK_CLASS_SELFSPEC_FLAG_BITS (3)
#define HAK_CLASS_SELFSPEC_CLASSINSTVAR_BITS ((HAK_SMOOI_ABS_BITS - HAK_CLASS_SELFSPEC_FLAG_BITS) / 2)
#define HAK_CLASS_SELFSPEC_CLASSVAR_BITS (HAK_SMOOI_ABS_BITS - (HAK_CLASS_SELFSPEC_CLASSINSTVAR_BITS + HAK_CLASS_SELFSPEC_FLAG_BITS))

#define HAK_CLASS_SELFSPEC_MAKE(class_var,classinst_var,flag) \
	((((hak_oow_t)class_var)     << (HAK_CLASS_SELFSPEC_CLASSINSTVAR_BITS + HAK_CLASS_SELFSPEC_FLAG_BITS)) | \
	 (((hak_oow_t)classinst_var) << (HAK_CLASS_SELFSPEC_FLAG_BITS)) | \
	 (((hak_oow_t)flag)          << (0)))

#define HAK_CLASS_SELFSPEC_CLASSVARS(spec) \
	(((hak_oow_t)spec) >> (HAK_CLASS_SELFSPEC_CLASSINSTVAR_BITS + HAK_CLASS_SELFSPEC_FLAG_BITS))

#define HAK_CLASS_SELFSPEC_CLASSINSTVARS(spec) \
	((((hak_oow_t)spec) >> HAK_CLASS_SELFSPEC_FLAG_BITS) & HAK_LBMASK(hak_oow_t, HAK_CLASS_SELFSPEC_CLASSINSTVAR_BITS))

#define HAK_CLASS_SELFSPEC_FLAGS(spec) \
	(((hak_oow_t)spec) & HAK_LBMASK(hak_oow_t, HAK_CLASS_SELFSPEC_FLAG_BITS))

/* If you add more than 4 items, you must update code related to CLASS_ENTER instruction
 * and class attributes as well as HAK_CLASS_SELFSPEC_FLAG_BITS. */
#define HAK_CLASS_SELFSPEC_FLAG_FINAL   (1 << 0)
#define HAK_CLASS_SELFSPEC_FLAG_LIMITED (1 << 1) /* not allowed to instantiate normally */


#define HAK_MAX_CLASSVARS      HAK_BITS_MAX(hak_oow_t, HAK_CLASS_SELFSPEC_CLASSVAR_BITS)
#define HAK_MAX_CLASSINSTVARS  HAK_BITS_MAX(hak_oow_t, HAK_CLASS_SELFSPEC_CLASSINSTVAR_BITS)

/* ========================================================================= */
/* END OF CLASS SPEC ENCODING                                                */
/* ========================================================================= */



#if defined(HAK_INCLUDE_COMPILER)

/* ========================================================================= */
/* SOURCE CODE I/O FOR COMPILER                                              */
/* ========================================================================= */
enum hak_tok_type_t
{
	HAK_TOK_EOF,

	/* the following 5 items must be in this order for code
	 * in flx_quoted_token() in read.c */
	HAK_TOK_CHARLIT,
	HAK_TOK_BCHRLIT,
	HAK_TOK_STRLIT,
	HAK_TOK_BSTRLIT,
	HAK_TOK_SYMLIT,

	HAK_TOK_NUMLIT,
	HAK_TOK_RADNUMLIT,
	HAK_TOK_FPDECLIT,
	HAK_TOK_SMPTRLIT,
	HAK_TOK_ERRLIT,

	HAK_TOK_NIL,
	HAK_TOK_TRUE,
	HAK_TOK_FALSE,
	HAK_TOK_SELF,
	HAK_TOK_SUPER,

	HAK_TOK_CLASS,
	HAK_TOK_FUN,
	HAK_TOK_VAR,
	HAK_TOK_DO,
	HAK_TOK_IF,
	HAK_TOK_ELIF,
	HAK_TOK_ELSE,
	HAK_TOK_THROW,
	HAK_TOK_TRY,
	HAK_TOK_CATCH,
	HAK_TOK_BREAK,
	HAK_TOK_CONTINUE,
	HAK_TOK_UNTIL,
	HAK_TOK_WHILE,
	HAK_TOK_RETURN,
	HAK_TOK_REVERT,
	HAK_TOK_AND,
	HAK_TOK_OR,
#if defined(USE_KW_PLUS)
	HAK_TOK_PLUS,
#endif
	HAK_TOK_SET,
	HAK_TOK_SET_R,

	HAK_TOK_BINOP,
	HAK_TOK_IDENT,
	HAK_TOK_IDENT_DOTTED,
	HAK_TOK_IDENT_DOTTED_CLA,
	HAK_TOK_DOT,       /* . */
	HAK_TOK_DBLDOTS,   /* .. */
	HAK_TOK_ELLIPSIS,  /* ... */
	HAK_TOK_COLON,     /* : */
	HAK_TOK_DBLCOLONS, /* :: */
	HAK_TOK_TRPCOLONS, /* ::: */
	HAK_TOK_COLONEQ,   /* := */
	HAK_TOK_SEMICOLON, /* ; */
	HAK_TOK_COMMA,     /* , */
	HAK_TOK_LPAREN,    /* ( */
	HAK_TOK_RPAREN,    /* ) */

#if 0 /* use the (obj:message ... ) syntax instad. no more mlist by (: */
	HAK_TOK_LPARCOLON, /* (: */
#define HAK_TOK_LPARCOLON HAK_TOK_LPARCOLON
#endif

	HAK_TOK_APAREN,    /* #[ - array parenthesis */
	HAK_TOK_BAPAREN,   /* #b[ - byte array parenthesis */
	HAK_TOK_CAPAREN,   /* #c[ - character array parenthesis */
#if 0
	HAK_TOK_WAPAREN,   /* #w[ - word array parenthesis */
	HAK_TOK_HWAPAREN,   /* #hw[ - half-word array parenthesis */
#endif
	HAK_TOK_QLPAREN,   /* #( - quoted-list parenthesis */
	HAK_TOK_DLPAREN,   /* #{ - dictionary parenthese */
	HAK_TOK_LBRACK,    /* [ - group */
	HAK_TOK_RBRACK,    /* ] */
	HAK_TOK_LBRACE,    /* { - block */
	HAK_TOK_RBRACE,    /* } */
	HAK_TOK_VBAR,      /* | */
	HAK_TOK_EOL,       /* end of line */

	HAK_TOK_INCLUDE,
	HAK_TOK_PRAGMA
};
typedef enum hak_tok_type_t hak_tok_type_t;

typedef struct hak_tok_t hak_tok_t;
struct hak_tok_t
{
	hak_tok_type_t type;
	hak_oocs_t name;
	hak_oow_t name_capa;
	hak_loc_t loc;
};


typedef struct hak_link_t hak_link_t;
struct hak_link_t
{
	hak_link_t* link;
};

enum hak_cnode_type_t
{
	HAK_CNODE_CHARLIT,
	HAK_CNODE_BCHRLIT,
	HAK_CNODE_SYMBOL,
	HAK_CNODE_DSYMBOL, /* dotted symbol */
	HAK_CNODE_BINOP,
	HAK_CNODE_STRLIT,
	HAK_CNODE_BSTRLIT,
	HAK_CNODE_SYMLIT,
	HAK_CNODE_NUMLIT,
	HAK_CNODE_RADNUMLIT,
	HAK_CNODE_FPDECLIT,
	HAK_CNODE_SMPTRLIT,
	HAK_CNODE_ERRLIT, /* last item for HAK_CNODE_IS_FOR_DATA_LITERAL */

	HAK_CNODE_NIL,
	HAK_CNODE_TRUE,
	HAK_CNODE_FALSE,
	HAK_CNODE_SELF,
	HAK_CNODE_SUPER, /* last item for HAK_CNODE_IS_FOR_DATA_SIMPLE */

	HAK_CNODE_CONS,
	HAK_CNODE_ELIST, /* empty list */
	HAK_CNODE_SHELL, /* pseudo-node to hold another actual node */

	/* If HAK_CNODE_SHELL is not the last item before the horizontal line,
	 * HAK_CNDOE_IS_FOR_DATA(x) must be revised */
	/* ------------------------------------------------------------------ */

	/* the cnode types from here don't represent actual data.
	 * these represent syntactical elements of the language only.  */
	HAK_CNODE_CLASS, /* first item for  HAK_CNODE_IS_FOR_LANG */
	HAK_CNODE_FUN,
	HAK_CNODE_VAR,
	HAK_CNODE_DO,
	HAK_CNODE_IF,
	HAK_CNODE_ELIF,
	HAK_CNODE_ELSE,
	HAK_CNODE_THROW,
	HAK_CNODE_TRY,
	HAK_CNODE_CATCH,
	HAK_CNODE_BREAK,
	HAK_CNODE_CONTINUE,
	HAK_CNODE_UNTIL,
	HAK_CNODE_WHILE,
	HAK_CNODE_RETURN,
	HAK_CNODE_REVERT,
	HAK_CNODE_AND,
	HAK_CNODE_OR,
#if defined(USE_KW_PLUS)
	HAK_CNODE_PLUS,
#endif
	HAK_CNODE_SET,
	HAK_CNODE_SET_R,  /* language item for HAK_CODE_IS_FOR_LANG */

	HAK_CNODE_ELLIPSIS, /* ... */
	HAK_CNODE_TRPCOLONS, /* ::: */
	HAK_CNODE_DBLCOLONS, /* :: */
	HAK_CNODE_COLON, /* : */
};
typedef enum hak_cnode_type_t hak_cnode_type_t;

enum hak_cnode_flag_t
{
	HAK_CNODE_AUTO_FORGED = (1 << 0)
};
typedef enum hak_cnode_flag_t hak_cnode_flag_t;

#define HAK_CNODE_GET_TYPE(x) ((x)->cn_type)
#define HAK_CNODE_GET_FLAGS(x) ((x)->cn_flags)
#define HAK_CNODE_GET_LLVL(x) ((x)->cn_llvl)
#define HAK_CNODE_GET_LOC(x) (&(x)->cn_loc)
#define HAK_CNODE_GET_TOK(x) (&(x)->cn_tok)
#define HAK_CNODE_GET_TOKPTR(x) ((x)->cn_tok.ptr)
#define HAK_CNODE_GET_TOKLEN(x) ((x)->cn_tok.len)

#define HAK_CNODE_IS_TYPED(x, _type) ((x)->cn_type == _type)
#define HAK_CNODE_IS_FOR_DATA(x) ((x)->cn_type <= HAK_CNODE_SHELL)
#define HAK_CNODE_IS_FOR_DATA_SIMPLE(x) ((x)->cn_type <= HAK_CNODE_SUPER)
#define HAK_CNODE_IS_FOR_DATA_LITERAL(x) ((x)->cn_type <= HAK_CNODE_ERRLIT)

/* words to compose the language itself.
 * the words pointing to data items(e.g. super, self, nil, true, false) are excluded */
#define HAK_CNODE_IS_FOR_LANG(x)((x)->cn_type >= HAK_CNODE_CLASS && (x)->cn_type <= HAK_CNODE_SET_R)

#define HAK_CNODE_IS_ELLIPSIS(x) ((x)->cn_type == HAK_CNODE_ELLIPSIS)
#define HAK_CNODE_IS_TRPCOLONS(x) ((x)->cn_type == HAK_CNODE_TRPCOLONS)
#define HAK_CNODE_IS_DBLCOLONS(x) ((x)->cn_type == HAK_CNODE_DBLCOLONS)
#define HAK_CNODE_IS_COLON(x) ((x)->cn_type == HAK_CNODE_COLON)

#define HAK_CNODE_IS_SYMBOL(x) ((x)->cn_type == HAK_CNODE_SYMBOL)
#define HAK_CNODE_IS_BINOP(x) ((x)->cn_type == HAK_CNODE_BINOP)
#define HAK_CNODE_IS_STRLIT(x) ((x)->cn_type == HAK_CNODE_STRLIT)
#define HAK_CNODE_IS_SYMLIT(x) ((x)->cn_type == HAK_CNODE_SYMLIT)

#define HAK_CNODE_IS_DSYMBOL(x) ((x)->cn_type == HAK_CNODE_DSYMBOL)
#define HAK_CNODE_IS_DSYMBOL_CLA(x) ((x)->cn_type == HAK_CNODE_DSYMBOL && (x)->u.dsymbol.is_cla)

#define HAK_CNODE_IS_CONS(x) ((x)->cn_type == HAK_CNODE_CONS)
#define HAK_CNODE_IS_CONS_CONCODED(x, code) ((x)->cn_type == HAK_CNODE_CONS && (x)->u.cons.concode == (code))
#define HAK_CNODE_CONS_CONCODE(x) ((x)->u.cons.concode)
#define HAK_CNODE_CONS_CAR(x) ((x)->u.cons.car)
#define HAK_CNODE_CONS_CDR(x) ((x)->u.cons.cdr)

#define HAK_CNODE_IS_ELIST(x) ((x)->cn_type == HAK_CNODE_ELIST)
#define HAK_CNODE_IS_ELIST_CONCODED(x, code) ((x)->cn_type == HAK_CNODE_ELIST && (x)->u.elist.concode == (code))
#define HAK_CNODE_ELIST_CONCODE(x) ((x)->u.elist.concode)

/* NOTE: hak_cnode_t used by the built-in compiler is not an OOP object */
struct hak_cnode_t
{
	hak_cnode_type_t cn_type;
	int cn_flags;
	hak_loc_t cn_loc;
	hak_oocs_t cn_tok;
	hak_oow_t cn_llvl; /* list level */

	union
	{
		struct
		{
			hak_ooch_t v;
		} charlit;
		struct
		{
			hak_oob_t v;
		} bchrlit;
		struct
		{
			int is_cla; /* class-level accessor. prefixed with self or super */
		} dsymbol;
		struct
		{
			hak_oow_t v;
		} smptrlit;
		struct
		{
			hak_ooi_t v;
		} errlit;
		struct
		{
			hak_concode_t concode;
			hak_cnode_t* car;
			hak_cnode_t* cdr;
		} cons;
		struct
		{
			hak_concode_t concode;
		} elist;
		struct
		{
			hak_cnode_t* obj;
		} shell;
	} u;
};

struct hak_var_info_t
{
	int type;
	/* ctx_offset 0 means the current context.
	 *            1 means current->home.
	 *            2 means current->home->home.
	 * index_in_ctx is a relative index within the context found.
	 */
	hak_oow_t ctx_offset; /* context offset */
	hak_oow_t index_in_ctx; /* index in the current scope */
};
typedef struct hak_var_info_t hak_var_info_t;


/* NOTE: hak_cframe_t used by the built-in compiler is not an OOP object */
struct hak_cframe_t
{
	int          opcode;
	hak_cnode_t* operand;

	union
	{
		/* COP_COMPILE_OBJECT_R */
		struct
		{
			hak_ooi_t nrets;
		} obj_r;

		/* COP_EMIT_CALL */
		struct
		{
			hak_ooi_t index;
			hak_ooi_t nrets;
		} call;

		/* COP_EMIT_SEND_MESSAGE */
		struct
		{
			hak_ooi_t nargs;
			hak_ooi_t nrets;
			int to_super;
		} sendmsg;

		/* COP_EMIT_SET */
		struct
		{
			int mode; /* VAR_ACCESS_STORE or VAR_ACCESS_POP */
			hak_var_info_t vi;
		} set;

		struct
		{
			hak_ooi_t cond_pos;
			hak_ooi_t body_pos;
			hak_ooi_t jump_inst_pos;
			hak_loc_t start_loc;
		} post_while;

		struct
		{
			hak_ooi_t body_pos;
			hak_ooi_t jump_inst_pos;
			hak_loc_t start_loc;
			hak_cnode_t* cmd_cnode;
		} post_if;

		struct
		{
			hak_ooi_t jump_inst_pos;
		} post_and;

		struct
		{
			hak_ooi_t jump_inst_pos;
		} post_or;

		/* COP_POST_TRY */
		struct
		{
			hak_oow_t jump_inst_pos;
		} post_try;

		/* COP_POST_CATCH */
		struct
		{
			hak_oow_t jump_inst_pos;
			hak_oow_t exarg_offset;
		} post_catch;

		struct
		{
			hak_oow_t lvar_start;
			hak_oow_t lvar_end;
		} post_do;

		/* COP_COMPILE_ARRAY_LIST, COP_POP_INTO_ARRAY, COP_EMIT_MAKE_ARRAY */
		struct
		{
			hak_ooi_t index;
		} array_list;

		/* COP_COMPILE_PURE_ARRAY_LIST, COP_POP_INTO_PURE_ARRAY, COP_EMIT_MAKE_PURE_ARRAY */
		struct
		{
			int elem_type;
			hak_ooi_t index;
		} pure_array_list;

		/* COP_EMIT_MAKE_DIC */
		struct
		{
			hak_ooi_t index;
		} dic_list;

		/* COP_POST_FUN, COP_EMIT_FUN */
		struct
		{
			unsigned int fun_type;
			hak_ooi_t jump_inst_pos;
			hak_ooi_t lfsize_pos;
			hak_cnode_t* class_name; /* class name for out-of-class method definition */
		} fun;

		/* COP_EMIT_RETURN */
		struct
		{
			int from_home;
		} _return;

		/* COP_POST_BREAK */
		struct
		{
			hak_ooi_t jump_inst_pos;
		} _break;

		/* COP_COMPILE_CLASS_P1, COP_COMPILE_CLASS_P2 */
		struct
		{
			hak_ooi_t nsuperclasses;
			unsigned int indexed_type;
			hak_oow_t llvl; /* copied from cnode->cn_llvl */
			hak_loc_t start_loc;
			hak_cnode_t* cmd_cnode;
			hak_cnode_t* class_name_cnode;
		} _class;
	} u;
};
typedef struct hak_cframe_t hak_cframe_t;

enum hak_ctlblk_type_t
{
	HAK_CTLBLK_TYPE_LOOP,
	HAK_CTLBLK_TYPE_TRY,
	HAK_CTLBLK_TYPE_CLASS
};
typedef enum hak_ctlblk_type_t hak_ctlblk_type_t;

/* control block information for the compiler */
struct hak_ctlblk_info_t
{
	hak_ctlblk_type_t _type;
};
typedef struct hak_ctlblk_info_t hak_ctlblk_info_t;

/* function block information for the compiler */
struct hak_funblk_info_t
{
	unsigned int fun_type;

	hak_oow_t tmprlen; /* accumulated length of the temporaries string including outer blocks */
	hak_oow_t tmprcnt; /* accumulated number of temporaries including outer blocks */

	hak_oow_t tmpr_va; /* 0 or 1 */
	hak_oow_t tmpr_nargs; /* number of fixed arguments */
	hak_oow_t tmpr_nrvars; /* number of return variables */
	hak_oow_t tmpr_nlvars; /* number of local variables */

	hak_oow_t make_inst_pos;
	hak_oow_t lfbase;

	hak_ooi_t ctlblk_base;

	hak_ooi_t clsblk_base;
	hak_ooi_t clsblk_top;

	unsigned int access_outer: 1;
	unsigned int accessed_by_inner: 1;
};
typedef struct hak_funblk_info_t hak_funblk_info_t;

/* class block information for the compiler */

struct hak_clsblk_info_t
{
	hak_oow_t def_llvl; /* defined list level */
	hak_cnode_t* class_name;

	hak_oocsc_t ivars;
	hak_oocsc_t cvars;

	hak_oow_t nivars;
	hak_oow_t ncvars;
	hak_oow_t spec; /* TODO: byte indexed, word indexed? */

	hak_ooi_t funblk_base;
	hak_ooi_t class_start_inst_pos; /* the position of the first instruction in the class body after CLASS_ENTER */
	hak_ooi_t class_enter_inst_pos; /* the position of the CLASS_ENTER instruction */
};
typedef struct hak_clsblk_info_t hak_clsblk_info_t;


/* reader stack for list reading */
typedef struct hak_rstl_t hak_rstl_t;
struct hak_rstl_t
{
	hak_cnode_t* head;
	hak_cnode_t* tail;
	hak_loc_t loc;
	int flagv;
	hak_oow_t count;
	hak_rstl_t* prev;
};

typedef struct hak_flx_dt_t hak_flx_dt_t; /* delemiter token */
struct hak_flx_dt_t
{
	int row_start;
	int row_end;
	int col_next;
};

typedef struct hak_flx_di_t hak_flx_di_t; /* dollared-signed identifier */
struct hak_flx_di_t
{
	/* state data */
	hak_oow_t char_count;
};

typedef struct hak_flx_hc_t hak_flx_hc_t; /* hash-marked character like #\, #\newline */
struct hak_flx_hc_t
{
	/* state data */
	hak_oow_t char_count;
};

typedef struct hak_flx_hi_t hak_flx_hi_t; /* hash-marked identifier */
struct hak_flx_hi_t
{
	/* state data */
	hak_oow_t char_count;
};

typedef struct hak_flx_hbc_t hak_flx_hbc_t; /* intermediate state for #b */
struct hak_flx_hbc_t
{
	/* state data */
	hak_ooch_t start_c;
};

typedef struct hak_flx_pi_t hak_flx_pi_t;
struct hak_flx_pi_t
{
	/* state data */
	hak_oow_t char_count;
	hak_oow_t seg_count;
	hak_oow_t seg_len;
	hak_oow_t non_ident_seg_count;
	hak_tok_type_t last_non_ident_type;
	hak_oow_t last_non_ident_seg;
	int is_cla; /* class-level accrssor. prefixed with self/super */
};

typedef struct hak_flx_pn_t hak_flx_pn_t;
struct hak_flx_pn_t
{
	/* state data */
	int fpdec;
	int radix;
	int radix_cand_overflown;
	hak_oow_t radix_cand;
	hak_tok_type_t tok_type;
	hak_oow_t digit_count[2];
	hak_ooch_t start_digit;
};

typedef struct hak_flx_qt_t hak_flx_qt_t; /* quoted token */
struct hak_flx_qt_t
{
	/* input data */
	hak_tok_type_t tok_type;
	hak_synerrnum_t synerr_code;
	hak_ooch_t end_char;
	hak_ooch_t esc_char;
	hak_oow_t min_len;
	hak_oow_t max_len;
	unsigned int is_byte: 1;
	unsigned int regex: 1;

	/* state data */
	unsigned int escaped: 4; /* must be large enough to hold 1, 2, 4, 8 */
	int digit_count;
	hak_ooci_t c_acc;
};

typedef struct hak_flx_st_t hak_flx_st_t;
struct hak_flx_st_t
{
	/* input data */
	hak_ooch_t sign_c;

	/* state data */
	hak_oow_t char_count;
	int hmarked;
};


typedef struct hak_flx_bcp_t hak_flx_bcp_t;
struct hak_flx_bcp_t
{
	hak_ooch_t start_c;
};

enum hak_flx_state_t
{
	HAK_FLX_START,
	HAK_FLX_BACKSLASHED,
	HAK_FLX_COMMENT,
	HAK_FLX_COLON_TOKEN,    /* token beginning with : */
	HAK_FLX_COLONEQ_TOKEN,  /* token beginning with := */
	HAK_FLX_DELIM_TOKEN,
	HAK_FLX_DOLLARED_IDENT,
	HAK_FLX_HMARKED_TOKEN,  /* hash-marked token */
	HAK_FLX_HMARKED_BC,     /* #b - intermediate state before #b[, #c[, or #b-radixed binary number */
	HAK_FLX_HMARKED_BINOP,  /* #++ - binary operator symbol */
	HAK_FLX_HMARKED_CHAR,   /* hash-marked character that begins with #\ */
	HAK_FLX_HMARKED_IDENT,  /* literal symbol */
	HAK_FLX_PLAIN_IDENT,    /* plain identifier */
	HAK_FLX_PLAIN_NUMBER,   /* plain number */
	HAK_FLX_QUOTED_TOKEN,   /* string, character */
	HAK_FLX_SIGNED_TOKEN,   /* prefixed with + or - */
	HAK_FLX_BC_PREFIX       /* b or C prefix before " or ' */
};
typedef enum hak_flx_state_t hak_flx_state_t;

typedef struct hak_frd_t hak_frd_t;
struct hak_frd_t
{
	int level;
	int flagv;
	int expect_pragma_item;
	int expect_include_file;
	int expect_vlist_item;
	int do_include_file;
	hak_cnode_t* obj;
	hak_loc_t list_loc;
};

struct hak_compiler_t
{
	/* flags passed in via hak_compile() */
	int flags;

	/* callback pointer registerd upon compiler creation */
	hak_cb_t* cbp;

	/* input handler */
	hak_io_impl_t cci_rdr;

	/* static input data buffer */
	hak_io_cciarg_t  cci_arg;

	/* pointer to the current input data. initially, it points to &inarg */
	hak_io_cciarg_t* curinp;

	/* information about the last meaningful character read.
	 * this is a copy of curinp->lxc if no ungetting is performed.
	 * if there is something in the unget buffer, this is overwritten
	 * by a value from the buffer when the request to read a character
	 * is served */
	hak_lxc_t  lxc;

	/* unget buffer */
	hak_lxc_t  ungot[10];
	int        nungots;

	/* the last token read */
	hak_tok_t   tok;
	hak_link_t* sr_names;

	hak_synerr_t synerr;

	/* temporary space to handle an illegal character */
	hak_ooch_t ilchr;
	hak_oocs_t ilchr_ucs;

	/* == READER == */
	struct
	{
		hak_oop_t s;  /* stack for reading */
		hak_oop_t e;  /* last object read */
		hak_rstl_t* st;
	} r; /* reading */
	/* == END READER == */

	struct
	{
	#if defined(HAK_OOCH_IS_UCH)
		struct
		{
			hak_bch_t buf[HAK_BCSIZE_MAX];
			hak_oow_t len;
			int no_check;
		} rsd; /* residue - incomplete sequence at the end of the last data fed by hak_feedbchars() */
	#endif

		struct
		{
			hak_flx_state_t state;
			hak_loc_t loc;
			hak_loc_t _oloc;

			union
			{
				hak_flx_dt_t dt; /* delimiter token */
				hak_flx_di_t di; /* dollar-signed identifier */
				hak_flx_hc_t hc; /* hash-marked character */
				hak_flx_hi_t hi; /* hash-marked identifier - literal symbol */
				hak_flx_hbc_t hbc; /* #b #c ... */
				hak_flx_pi_t pi; /* plain identifier */
				hak_flx_pn_t pn; /* plain number */
				hak_flx_qt_t qt; /* quoted token */
				hak_flx_st_t st; /* signed token */
				hak_flx_bcp_t bcp; /* b or c prefix */
			} u;
		} lx;

		struct hak_frd_t rd;
		hak_on_cnode_t on_cnode;
	} feed;

	/* == COMPILER STACK == */
	struct
	{
		hak_cframe_t* ptr;
		hak_ooi_t     top;
		hak_oow_t     capa;
	} cfs;
	/* == END COMPILER STACK == */

	struct
	{
		hak_oocs_t s; /* buffer */
		hak_oow_t capa; /* bufer capacity */
		hak_oow_t wcount; /* word count */
	} tv; /* temporary variables including arguments */

	struct
	{
		hak_ooi_t depth; /* signed because it starts with -1 */
		hak_ctlblk_info_t* info;
		hak_oow_t info_capa;
	} ctlblk; /* control block - loop, try-catch */

	struct
	{
		hak_ooi_t depth; /* signed because it starts with -1 */
		hak_funblk_info_t* info;
		hak_oow_t  info_capa;
	} funblk; /* function block */

	struct
	{
		hak_ooi_t depth; /* signed because it starts with -1 */
		hak_clsblk_info_t* info;
		hak_oow_t info_capa;
	} clsblk; /* class block */


	struct
	{
		hak_cnode_t cons_to_nil;
		hak_cnode_t nil;
	} fake_cnode;
};
#endif



/* hak_context_t, hak_block_t, hak_function_t stores the local variable information
 *
 * Use up to 29 bits in a 32-bit hak_ooi_t. Exclude the tag bit and the sign bit.
 * | SIGN | INSTA | VA | NARGS | NRVARS | NLVARS | TAG |
 *     1            1     8       8        11        2    <= 32
 * -----------------------------------------------------------
 * Parameters to the MAKE_BLOCK or MAKE_FUNCTION instructions
 *  | INSTA | VA | NARGS | NRVARS | NLVARS
 *    1       1      4      4        6         <= 16 (HAK_CODE_LONG_PARAM_SIZE 1, two params)
 *    1       1      8      8        11        <= 32 (HAK_CODE_LONG_PARAM_SIZE 2, two params, use 29 bits to avoid collection when converted to a smooi)
 *
 * INSTA indicates the class instantiation method.
 * NARGS and NRVARS are also used for the CALL and CALL2 instructions.
 * CALL encodes NARGS in one parameter.
 * CALLR encodes NARGS in one parameter and NRVARS in another parameter.
 * NARGS and NRVARS must not exceed a single parameter size.
 */

#if defined(HAK_CODE_LONG_PARAM_SIZE) && (HAK_CODE_LONG_PARAM_SIZE == 1)

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
#elif defined(HAK_CODE_LONG_PARAM_SIZE) && (HAK_CODE_LONG_PARAM_SIZE == 2)

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
#	error Unsupported HAK_CODE_LONG_PARAM_SIZE
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

enum hak_bcode_t
{
	HAK_CODE_STORE_INTO_IVAR_0        = 0x00,
	HAK_CODE_STORE_INTO_IVAR_1        = 0x01,
	HAK_CODE_STORE_INTO_IVAR_2        = 0x02,
	HAK_CODE_STORE_INTO_IVAR_3        = 0x03,

	HAK_CODE_STORE_INTO_IVAR_4        = 0x04,
	HAK_CODE_STORE_INTO_IVAR_5        = 0x05,
	HAK_CODE_STORE_INTO_IVAR_6        = 0x06,
	HAK_CODE_STORE_INTO_IVAR_7        = 0x07,

	HAK_CODE_POP_INTO_IVAR_0          = 0x08,
	HAK_CODE_POP_INTO_IVAR_1          = 0x09,
	HAK_CODE_POP_INTO_IVAR_2          = 0x0A,
	HAK_CODE_POP_INTO_IVAR_3          = 0x0B,

	HAK_CODE_POP_INTO_IVAR_4          = 0x0C,
	HAK_CODE_POP_INTO_IVAR_5          = 0x0D,
	HAK_CODE_POP_INTO_IVAR_6          = 0x0E,
	HAK_CODE_POP_INTO_IVAR_7          = 0x0F,

	HAK_CODE_PUSH_IVAR_0              = 0x10,
	HAK_CODE_PUSH_IVAR_1              = 0x11,
	HAK_CODE_PUSH_IVAR_2              = 0x12,
	HAK_CODE_PUSH_IVAR_3              = 0x13,

	HAK_CODE_PUSH_IVAR_4              = 0x14,
	HAK_CODE_PUSH_IVAR_5              = 0x15,
	HAK_CODE_PUSH_IVAR_6              = 0x16,
	HAK_CODE_PUSH_IVAR_7              = 0x17,

	HAK_CODE_PUSH_TEMPVAR_0           = 0x18,
	HAK_CODE_PUSH_TEMPVAR_1           = 0x19,
	HAK_CODE_PUSH_TEMPVAR_2           = 0x1A,
	HAK_CODE_PUSH_TEMPVAR_3           = 0x1B,

	HAK_CODE_PUSH_TEMPVAR_4           = 0x1C,
	HAK_CODE_PUSH_TEMPVAR_5           = 0x1D,
	HAK_CODE_PUSH_TEMPVAR_6           = 0x1E,
	HAK_CODE_PUSH_TEMPVAR_7           = 0x1F,

	HAK_CODE_STORE_INTO_TEMPVAR_0     = 0x20,
	HAK_CODE_STORE_INTO_TEMPVAR_1     = 0x21,
	HAK_CODE_STORE_INTO_TEMPVAR_2     = 0x22,
	HAK_CODE_STORE_INTO_TEMPVAR_3     = 0x23,

	HAK_CODE_STORE_INTO_TEMPVAR_4     = 0x24,
	HAK_CODE_STORE_INTO_TEMPVAR_5     = 0x25,
	HAK_CODE_STORE_INTO_TEMPVAR_6     = 0x26,
	HAK_CODE_STORE_INTO_TEMPVAR_7     = 0x27,

	HAK_CODE_POP_INTO_TEMPVAR_0       = 0x28,
	HAK_CODE_POP_INTO_TEMPVAR_1       = 0x29,
	HAK_CODE_POP_INTO_TEMPVAR_2       = 0x2A,
	HAK_CODE_POP_INTO_TEMPVAR_3       = 0x2B,

	HAK_CODE_POP_INTO_TEMPVAR_4       = 0x2C,
	HAK_CODE_POP_INTO_TEMPVAR_5       = 0x2D,
	HAK_CODE_POP_INTO_TEMPVAR_6       = 0x2E,
	HAK_CODE_POP_INTO_TEMPVAR_7       = 0x2F,

	HAK_CODE_PUSH_LITERAL_0           = 0x30,
	HAK_CODE_PUSH_LITERAL_1           = 0x31,
	HAK_CODE_PUSH_LITERAL_2           = 0x32,
	HAK_CODE_PUSH_LITERAL_3           = 0x33,

	HAK_CODE_PUSH_LITERAL_4           = 0x34,
	HAK_CODE_PUSH_LITERAL_5           = 0x35,
	HAK_CODE_PUSH_LITERAL_6           = 0x36,
	HAK_CODE_PUSH_LITERAL_7           = 0x37,

	/* -------------------------------------- */

	HAK_CODE_STORE_INTO_OBJECT_0      = 0x38,
	HAK_CODE_STORE_INTO_OBJECT_1      = 0x39,
	HAK_CODE_STORE_INTO_OBJECT_2      = 0x3A,
	HAK_CODE_STORE_INTO_OBJECT_3      = 0x3B,

	HAK_CODE_POP_INTO_OBJECT_0        = 0x3C,
	HAK_CODE_POP_INTO_OBJECT_1        = 0x3D,
	HAK_CODE_POP_INTO_OBJECT_2        = 0x3E,
	HAK_CODE_POP_INTO_OBJECT_3        = 0x3F,

	HAK_CODE_PUSH_OBJECT_0            = 0x40,
	HAK_CODE_PUSH_OBJECT_1            = 0x41,
	HAK_CODE_PUSH_OBJECT_2            = 0x42,
	HAK_CODE_PUSH_OBJECT_3            = 0x43,

	HAK_CODE_JUMP_FORWARD_0           = 0x44, /* 68 */
	HAK_CODE_JUMP_FORWARD_1           = 0x45, /* 69 */
	HAK_CODE_JUMP_FORWARD_2           = 0x46, /* 70 */
	HAK_CODE_JUMP_FORWARD_3           = 0x47, /* 71 */

	HAK_CODE_JUMP_BACKWARD_0          = 0x48, /* 72 */
	HAK_CODE_JUMP_BACKWARD_1          = 0x49, /* 73 */
	HAK_CODE_JUMP_BACKWARD_2          = 0x4A, /* 74 */
	HAK_CODE_JUMP_BACKWARD_3          = 0x4B, /* 75 */

	/* UNUSED 0x4C - 0x53 */

	HAK_CODE_CALL_0                   = 0x54, /* 84 */
	HAK_CODE_CALL_1                   = 0x55, /* 85 */
	HAK_CODE_CALL_2                   = 0x56, /* 86 */
	HAK_CODE_CALL_3                   = 0x57, /* 87 */

	HAK_CODE_STORE_INTO_CTXTEMPVAR_0  = 0x58, /* 88 */
	HAK_CODE_STORE_INTO_CTXTEMPVAR_1  = 0x59, /* 89 */
	HAK_CODE_STORE_INTO_CTXTEMPVAR_2  = 0x5A, /* 90 */
	HAK_CODE_STORE_INTO_CTXTEMPVAR_3  = 0x5B, /* 91 */

	HAK_CODE_POP_INTO_CTXTEMPVAR_0    = 0x5C, /* 92 */
	HAK_CODE_POP_INTO_CTXTEMPVAR_1    = 0x5D, /* 93 */
	HAK_CODE_POP_INTO_CTXTEMPVAR_2    = 0x5E, /* 94 */
	HAK_CODE_POP_INTO_CTXTEMPVAR_3    = 0x5F, /* 95 */

	HAK_CODE_PUSH_CTXTEMPVAR_0        = 0x60, /* 96 */
	HAK_CODE_PUSH_CTXTEMPVAR_1        = 0x61, /* 97 */
	HAK_CODE_PUSH_CTXTEMPVAR_2        = 0x62, /* 98 */
	HAK_CODE_PUSH_CTXTEMPVAR_3        = 0x63, /* 99 */

	HAK_CODE_PUSH_OBJVAR_0            = 0x64,
	HAK_CODE_PUSH_OBJVAR_1            = 0x65,
	HAK_CODE_PUSH_OBJVAR_2            = 0x66,
	HAK_CODE_PUSH_OBJVAR_3            = 0x67,

	HAK_CODE_STORE_INTO_OBJVAR_0      = 0x68,
	HAK_CODE_STORE_INTO_OBJVAR_1      = 0x69,
	HAK_CODE_STORE_INTO_OBJVAR_2      = 0x6A,
	HAK_CODE_STORE_INTO_OBJVAR_3      = 0x6B,

	HAK_CODE_POP_INTO_OBJVAR_0        = 0x6C,
	HAK_CODE_POP_INTO_OBJVAR_1        = 0x6D,
	HAK_CODE_POP_INTO_OBJVAR_2        = 0x6E,
	HAK_CODE_POP_INTO_OBJVAR_3        = 0x6F,

	HAK_CODE_SEND_0                   = 0x70, /* 112 */
	HAK_CODE_SEND_1                   = 0x71, /* 113 */
	HAK_CODE_SEND_2                   = 0x72, /* 114 */
	HAK_CODE_SEND_3                   = 0x73, /* 115 */

	HAK_CODE_SEND_TO_SUPER_0          = 0x74, /* 116 */
	HAK_CODE_SEND_TO_SUPER_1          = 0x75, /* 117 */
	HAK_CODE_SEND_TO_SUPER_2          = 0x76, /* 118 */
	HAK_CODE_SEND_TO_SUPER_3          = 0x77, /* 119 */

	HAK_CODE_PUSH_CVAR_I_X            = 0x78, /* 120 */
	HAK_CODE_STORE_INTO_CVAR_I_X      = 0x79, /* 121 */
	HAK_CODE_POP_INTO_CVAR_I_X        = 0x7A, /* 122 */

	HAK_CODE_PUSH_CVAR_M_X            = 0x7B, /* 123 */
	HAK_CODE_STORE_INTO_CVAR_M_X      = 0x7C, /* 124 */
	HAK_CODE_POP_INTO_CVAR_M_X        = 0x7D, /* 125 */

	/* UNUSED 0x7E - 0x7F */
	HAK_CODE_STORE_INTO_IVAR_X        = 0x80, /* 128 */

	HAK_CODE_PUSH_RECEIVER            = 0x81, /* 129 */
	HAK_CODE_PUSH_NIL                 = 0x82, /* 130 */
	HAK_CODE_PUSH_TRUE                = 0x83, /* 131 */
	HAK_CODE_PUSH_FALSE               = 0x84, /* 132 */
	HAK_CODE_PUSH_CONTEXT             = 0x85, /* 133 */
	HAK_CODE_PUSH_PROCESS             = 0x86, /* 134 */
	/* UNUSED 0x87 */

	HAK_CODE_POP_INTO_IVAR_X          = 0x88, /* 136 ## */

	HAK_CODE_PUSH_NEGONE              = 0x89, /* 137 */
	HAK_CODE_PUSH_ZERO                = 0x8A, /* 138 */
	HAK_CODE_PUSH_ONE                 = 0x8B, /* 139 */
	HAK_CODE_PUSH_TWO                 = 0x8C, /* 140 */

	HAK_CODE_PUSH_IVAR_X              = 0x90, /* 144 ## */
	HAK_CODE_PUSH_TEMPVAR_X           = 0x98, /* 152 ## */
	HAK_CODE_STORE_INTO_TEMPVAR_X     = 0xA0, /* 160 ## */
	HAK_CODE_POP_INTO_TEMPVAR_X       = 0xA8, /* 168 ## */

	HAK_CODE_PUSH_LITERAL_X           = 0xB0, /* 176 ## */
	HAK_CODE_PUSH_LITERAL_X2          = 0xB1, /* 177 */

	HAK_CODE_PUSH_INTLIT              = 0xB2, /* 178 */
	HAK_CODE_PUSH_NEGINTLIT           = 0xB3, /* 179 */
	HAK_CODE_PUSH_CHARLIT             = 0xB4, /* 180 */

/* TODO: generalize it to support binops, not just plus */
	HAK_CODE_PLUS = 0xB5, /* 181 TOOD: move it to a lower code number later after killing OBJVAR instructions */
	/* UNUSED - 0xB6-0xB7 */

	HAK_CODE_STORE_INTO_OBJECT_X      = 0xB8, /* 184 ## */
	HAK_CODE_POP_INTO_OBJECT_X        = 0xBC, /* 188 ## */
	HAK_CODE_PUSH_OBJECT_X            = 0xC0, /* 192 ## */

	/* UNUSED - 0xC1 - 0xC3 */

	HAK_CODE_JUMP_FORWARD_X           = 0xC4, /* 196 ## */
	HAK_CODE_JUMP2_FORWARD            = 0xC5, /* 197 */

	/* UNUSED - 0xC6 - 0xC7 */

	HAK_CODE_JUMP_BACKWARD_X          = 0xC8, /* 200 ## */
	HAK_CODE_JUMP2_BACKWARD           = 0xC9, /* 201 */

	/* UNUSED - 0xCA - 0xCB */

	HAK_CODE_JUMP_FORWARD_IF_TRUE     = 0xCC, /* 204 ## */
	HAK_CODE_JUMP2_FORWARD_IF_TRUE    = 0xCD, /* 205 */
	HAK_CODE_JUMP_BACKWARD_IF_TRUE    = 0xCE, /* 206 ## */
	HAK_CODE_JUMP2_BACKWARD_IF_TRUE   = 0xCF, /* 207 */

	HAK_CODE_JUMP_FORWARD_IF_FALSE    = 0xD0, /* 208 ## */
	HAK_CODE_JUMP2_FORWARD_IF_FALSE   = 0xD1, /* 209 */
	HAK_CODE_JUMP_BACKWARD_IF_FALSE   = 0xD2, /* 210 ## */
	HAK_CODE_JUMP2_BACKWARD_IF_FALSE  = 0xD3, /* 211 */

	HAK_CODE_CALL_X                   = 0xD4, /* 212 ## */
	HAK_CODE_CALL_R                   = 0xD5, /* 213 ## ##*/
	HAK_CODE_PUSH_RETURN_R            = 0xD6, /* 214 */
	HAK_CODE_TRY_ENTER                = 0xD7, /* 215 ## */

	HAK_CODE_STORE_INTO_CTXTEMPVAR_X  = 0xD8, /* 216 ## */
	HAK_CODE_TRY_ENTER2               = 0xD9, /* 217 ## */
	HAK_CODE_TRY_EXIT                 = 0xDA, /* 218 */
	HAK_CODE_THROW                    = 0xDB, /* 219 */

	HAK_CODE_POP_INTO_CTXTEMPVAR_X    = 0xDC, /* 220 ## */
	HAK_CODE_CLASS_LOAD               = 0xDD, /* 221 ## */
	HAK_CODE_MAKE_CHARARRAY           = 0xDE, /* 222 ## */
	HAK_CODE_POP_INTO_CHARARRAY       = 0xDF, /* 223 ## */

	HAK_CODE_PUSH_CTXTEMPVAR_X        = 0xE0, /* 224 ## */
	HAK_CODE_CLASS_ENTER              = 0xE1, /* 225 ## */
	HAK_CODE_CLASS_EXIT               = 0xE2, /* 226 */
	HAK_CODE_CLASS_PEXIT              = 0xE3, /* 227 */

	HAK_CODE_PUSH_OBJVAR_X            = 0xE4, /* 228 ## */
	HAK_CODE_CLASS_CMSTORE            = 0xE5, /* 229 */
	HAK_CODE_CLASS_IMSTORE            = 0xE6, /* 230 */
	HAK_CODE_CLASS_CIMSTORE           = 0xE7, /* 231 */

	HAK_CODE_STORE_INTO_OBJVAR_X      = 0xE8, /* 232 ## */
	HAK_CODE_MAKE_ARRAY               = 0xE9, /* 233 ## */
	HAK_CODE_MAKE_BYTEARRAY           = 0xEA, /* 234 ## */
	HAK_CODE_MAKE_DIC                 = 0xEB, /* 235 ## */

	HAK_CODE_POP_INTO_OBJVAR_X        = 0xEC, /* 236 ## */
	HAK_CODE_POP_INTO_ARRAY           = 0xED, /* 237 ## */
	HAK_CODE_POP_INTO_BYTEARRAY       = 0xEE, /* 238 ## */
	HAK_CODE_POP_INTO_DIC             = 0xEF, /* 239 */

	HAK_CODE_SEND_X                   = 0xF0, /* 240 ## */
	HAK_CODE_SEND_R                   = 0xF1, /* 241 ## ## - [NOTE] ((code >> 2) & 1) must be 0 */

	HAK_CODE_MAKE_CONS                = 0xF2, /* 242 */
	HAK_CODE_POP_INTO_CONS            = 0xF3, /* 243 */

	HAK_CODE_SEND_TO_SUPER_X          = 0xF4, /* 244 ## */
	HAK_CODE_SEND_TO_SUPER_R          = 0xF5, /* 245 ## ## - [NOTE] ((code >> 2) & 1) must be 0 */

	HAK_CODE_POP_INTO_CONS_END        = 0xF6, /* 246 */
	HAK_CODE_POP_INTO_CONS_CDR        = 0xF7, /* 247 */
	/* -------------------------------------- */

	HAK_CODE_DUP_STACKTOP             = 0xF8, /* 248 */
	HAK_CODE_POP_STACKTOP             = 0xF9, /* 249 */
	HAK_CODE_RETURN_STACKTOP          = 0xFA, /* 250 */
	HAK_CODE_RETURN_RECEIVER          = 0xFB, /* 251 */
	HAK_CODE_RETURN_FROM_BLOCK        = 0xFC, /* 252, return the stack top from a block */

	HAK_CODE_MAKE_FUNCTION            = 0xFD, /* 253 */
	HAK_CODE_MAKE_BLOCK               = 0xFE, /* 254 */
	HAK_CODE_NOOP                     = 0xFF  /* 255 */
};

typedef hak_ooi_t (*hak_outbfmt_t) (
	hak_t*           hak,
	hak_bitmask_t  mask,
	const hak_bch_t* fmt,
	...
);

/* i don't want an error raised inside the callback to override
 * the existing error number and message. */
#define HAK_VMPRIM_LOG_WRITE(hak,mask,ptr,len) do { \
		int shuterr = (hak)->shuterr; \
		(hak)->shuterr = 1; \
		(hak)->vmprim.log_write (hak, mask, ptr, len); \
		(hak)->shuterr = shuterr; \
	} while(0)


#define HAK_CHAR_TO_NUM(c,base) \
        ((c >= '0' && c <= '9')? ((c - '0' < base)? (c - '0'): base): \
         (c >= 'A' && c <= 'Z')? ((c - 'A' + 10 < base)? (c - 'A' + 10): base): \
         (c >= 'a' && c <= 'z')? ((c - 'a' + 10 < base)? (c - 'a' + 10): base): base)


/* receiver check failure leads to hard failure.
 * RATIONAL: the primitive handler should be used by relevant classes and
 *           objects only. if the receiver check fails, you must review
 *           your class library */
#define HAK_PF_CHECK_RCV(hak,cond) do { \
	if (!(cond)) { hak_seterrnum((hak), HAK_EMSGRCV); return HAK_PF_HARD_FAILURE; } \
} while(0)

/* argument check failure causes the wrapping method to return an error.
 * RATIONAL: Being a typeless language, it's hard to control the kinds of
 *           arguments.
 */
#define HAK_PF_CHECK_ARGS(hak,nargs,cond) do { \
	if (!(cond)) { hak_seterrnum (hak, HAK_EINVAL); return HAK_PF_FAILURE; } \
} while(0)

#if defined(__cplusplus)
extern "C" {
#endif

/* ========================================================================= */
/* heap.c                                                                    */
/* ========================================================================= */

/**
 * The hak_makeheap() function creates a new heap of the \a size bytes.
 *
 * \return heap pointer on success and #HAK_NULL on failure.
 */
hak_heap_t* hak_makeheap (
	hak_t*     hak,
	hak_oow_t  size
);

/**
 * The hak_killheap() function destroys the heap pointed to by \a heap.
 */
void hak_killheap (
	hak_t*      hak,
	hak_heap_t* heap
);

/**
 * The hak_allocheapmem() function allocates \a size bytes from the given heap
 * and clears it with zeros.
 */
void* hak_callocheapmem (
	hak_t*       hak,
	hak_heap_t*  heap,
	hak_oow_t    size
);

void* hak_callocheapmem_noseterr (
	hak_t*       hak,
	hak_heap_t*  heap,
	hak_oow_t    size
);

void hak_freeheapmem (
	hak_t*       hak,
	hak_heap_t*  heap,
	void*        ptr
);

/* ========================================================================= */
/* obj.c                                                                     */
/* ========================================================================= */
void* hak_allocbytes (
	hak_t*     hak,
	hak_oow_t  size
);

/**
 * The hak_allocoopobj() function allocates a raw object composed of \a size
 * pointer fields excluding the header.
 */
hak_oop_t hak_allocoopobj (
	hak_t*    hak,
	hak_oow_t size
);

hak_oop_t hak_allocoopobjwithtrailer (
	hak_t*           hak,
	hak_oow_t        size,
	const hak_oob_t* tptr,
	hak_oow_t        tlen
);

hak_oop_t hak_alloccharobj (
	hak_t*            hak,
	const hak_ooch_t* ptr,
	hak_oow_t         len
);

hak_oop_t hak_allocbyteobj (
	hak_t*            hak,
	const hak_oob_t*  ptr,
	hak_oow_t         len
);

hak_oop_t hak_allochalfwordobj (
	hak_t*            hak,
	const hak_oohw_t* ptr,
	hak_oow_t         len
);

hak_oop_t hak_allocwordobj (
	hak_t*           hak,
	const hak_oow_t* ptr,
	hak_oow_t        len
);

hak_oop_t hak_instantiatewithtrailer (
	hak_t*           hak,
	hak_oop_class_t  _class,
	hak_oow_t        vlen,
	const hak_oob_t* trptr,
	hak_oow_t        trlen
);

/* ========================================================================= */
/* sym.c                                                                     */
/* ========================================================================= */
hak_oop_t hak_makesymbol (
	hak_t*             hak,
	const hak_ooch_t*  ptr,
	hak_oow_t          len
);

hak_oop_t hak_makesymbolwithbcstr (
	hak_t*             hak,
	const hak_bch_t*   ptr
);

hak_oop_t hak_makesymbolwithbchars (
	hak_t*             hak,
	const hak_bch_t*   ptr,
	hak_oow_t          len
);

hak_oop_t hak_makesymbolwithucstr (
	hak_t*             hak,
	const hak_uch_t*   ptr
);

hak_oop_t hak_makesymbolwithuchars (
	hak_t*             hak,
	const hak_uch_t*   ptr,
	hak_oow_t          len
);

hak_oop_t hak_findsymbol (
	hak_t*             hak,
	const hak_ooch_t*  ptr,
	hak_oow_t          len
);


/* ========================================================================= */
/* dic.c                                                                    */
/* ========================================================================= */
hak_oop_cons_t hak_putatdic_method (
	hak_t*        hak,
	hak_oop_dic_t dic,
	hak_oop_t     key,
	hak_oop_t     value,
	int*          mtype_flags /* [IN] 1 for class method, 2 for instance method,
	                             [OUT] bit 0(1) or bit 1(2) set for each method type redefined */
);

/* ========================================================================= */
/* gc.c                                                                    */
/* ========================================================================= */

hak_oow_t hak_getobjpayloadbytes (
	hak_t*    hak,
	hak_oop_t oop
);

void hak_gc_ms_sweep_lazy (
	hak_t*    hak,
	hak_oow_t allocsize
);

/* ========================================================================= */
/* utf8.c                                                                    */
/* ========================================================================= */
hak_oow_t hak_uc_to_utf8 (
	hak_uch_t    uc,
	hak_bch_t*   utf8,
	hak_oow_t    size
);

hak_oow_t hak_utf8_to_uc (
	const hak_bch_t* utf8,
	hak_oow_t        size,
	hak_uch_t*       uc
);

int hak_ucstoutf8 (
	const hak_uch_t*    ucs,
	hak_oow_t*          ucslen,
	hak_bch_t*          bcs,
	hak_oow_t*          bcslen
);

/**
 * The hak_utf8_to_ucs() function converts a UTF8 string to a uncide string.
 *
 * It never returns -2 if \a ucs is #HAK_NULL.
 *
 * \code
 *  const hak_bch_t* bcs = "test string";
 *  hak_uch_t ucs[100];
 *  hak_oow_t ucslen = HAK_COUNTOF(buf), n;
 *  hak_oow_t bcslen = 11;
 *  int n;
 *  n = hak_utf8_to_ucs (bcs, &bcslen, ucs, &ucslen);
 *  if (n <= -1) { invalid/incomplenete sequence or buffer to small }
 * \endcode
 *
 * For a null-terminated string, you can specify ~(hak_oow_t)0 in
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
int hak_utf8_to_ucs (
	const hak_bch_t*   bcs,
	hak_oow_t*         bcslen,
	hak_uch_t*         ucs,
	hak_oow_t*         ucslen
);

/* ========================================================================= */
/* bigint.c                                                                  */
/* ========================================================================= */
static HAK_INLINE int hak_isbigint (hak_t* hak, hak_oop_t x)
{
	return HAK_IS_BIGINT(hak, x);
}

static HAK_INLINE int hak_isint (hak_t* hak, hak_oop_t x)
{
	return HAK_OOP_IS_SMOOI(x) || HAK_IS_BIGINT(hak, x);
}

hak_oop_t hak_addints (
	hak_t*    hak,
	hak_oop_t x,
	hak_oop_t y
);

hak_oop_t hak_subints (
	hak_t*    hak,
	hak_oop_t x,
	hak_oop_t y
);

hak_oop_t hak_mulints (
	hak_t*    hak,
	hak_oop_t x,
	hak_oop_t y
);

hak_oop_t hak_divints (
	hak_t*     hak,
	hak_oop_t  x,
	hak_oop_t  y,
	int        modulo,
	hak_oop_t* rem
);

hak_oop_t hak_negateint (
	hak_t*    hak,
	hak_oop_t x
);

hak_oop_t hak_bitatint (
	hak_t*    hak,
	hak_oop_t x,
	hak_oop_t y
);

hak_oop_t hak_bitandints (
	hak_t*    hak,
	hak_oop_t x,
	hak_oop_t y
);

hak_oop_t hak_bitorints (
	hak_t*    hak,
	hak_oop_t x,
	hak_oop_t y
);

hak_oop_t hak_bitxorints (
	hak_t*    hak,
	hak_oop_t x,
	hak_oop_t y
);

hak_oop_t hak_bitinvint (
	hak_t*    hak,
	hak_oop_t x
);

hak_oop_t hak_bitshiftint (
	hak_t*    hak,
	hak_oop_t x,
	hak_oop_t y
);

hak_oop_t hak_eqints (
	hak_t* hak,
	hak_oop_t x,
	hak_oop_t y
);

hak_oop_t hak_neints (
	hak_t*    hak,
	hak_oop_t x,
	hak_oop_t y
);

hak_oop_t hak_gtints (
	hak_t*    hak,
	hak_oop_t x,
	hak_oop_t y
);

hak_oop_t hak_geints (
	hak_t*    hak,
	hak_oop_t x,
	hak_oop_t y
);

hak_oop_t hak_ltints (
	hak_t*    hak,
	hak_oop_t x,
	hak_oop_t y
);

hak_oop_t hak_leints (
	hak_t*    hak,
	hak_oop_t x,
	hak_oop_t y
);

hak_oop_t hak_sqrtint (
	hak_t*    hak,
	hak_oop_t x
);

hak_oop_t hak_absint (
	hak_t*    hak,
	hak_oop_t x
);

hak_oop_t hak_strtoint (
	hak_t*            hak,
	const hak_ooch_t* str,
	hak_oow_t         len,
	int               radix
);


#define HAK_INTTOSTR_RADIXMASK (0xFF)
#define HAK_INTTOSTR_LOWERCASE (1 << 8)
#define HAK_INTTOSTR_NONEWOBJ  (1 << 9)
/**
 * The hak_inttostr() function converts an integer object to a string object
 * printed in the given radix. If HAK_INTTOSTR_NONEWOBJ is set in flags_radix,
 * it returns hak->_nil but keeps the result in the buffer pointed to by
 * hak->inttostr.xbuf.ptr with the length stored in hak->inttostr.xbuf.len.
 * If the function fails, it returns #HAK_NULL.
 */
hak_oop_t hak_inttostr (
	hak_t*      hak,
	hak_oop_t   num,
	int         flagged_radix /* radix between 2 and 36 inclusive, optionally bitwise ORed of HAK_INTTOSTR_XXX bits */
);

/* ========================================================================= */
/* number.c                                                                    */
/* ========================================================================= */
hak_oop_t hak_addnums (
	hak_t*       hak,
	hak_oop_t    x,
	hak_oop_t    y
);

hak_oop_t hak_subnums (
	hak_t*       hak,
	hak_oop_t    x,
	hak_oop_t    y
);

hak_oop_t hak_mulnums (
	hak_t*       hak,
	hak_oop_t    x,
	hak_oop_t    y
);

hak_oop_t hak_mltnums (
	hak_t*       hak,
	hak_oop_t    x,
	hak_oop_t    y
);

hak_oop_t hak_divnums (
	hak_t*       hak,
	hak_oop_t    x,
	hak_oop_t    y
);

hak_oop_t hak_truncfpdecval (
	hak_t*       hak,
	hak_oop_t    iv, /* integer */
	hak_ooi_t    cs, /* current scale */
	hak_ooi_t    ns  /* new scale */
);

hak_oop_t hak_gtnums (
	hak_t*       hak,
	hak_oop_t    x,
	hak_oop_t    y
);

hak_oop_t hak_genums (
	hak_t*       hak,
	hak_oop_t    x,
	hak_oop_t    y
);

hak_oop_t hak_ltnums (
	hak_t*       hak,
	hak_oop_t    x,
	hak_oop_t    y
);

hak_oop_t hak_lenums (
	hak_t*       hak,
	hak_oop_t    x,
	hak_oop_t    y
);

hak_oop_t hak_eqnums (
	hak_t*       hak,
	hak_oop_t    x,
	hak_oop_t    y
);

hak_oop_t hak_nenums (
	hak_t*       hak,
	hak_oop_t    x,
	hak_oop_t    y
);

hak_oop_t hak_sqrtnum (
	hak_t*       hak,
	hak_oop_t    x
);

hak_oop_t hak_absnum (
	hak_t*       hak,
	hak_oop_t    x
);

/* ========================================================================= */
/* hak.c                                                                     */
/* ========================================================================= */

hak_mod_data_t* hak_openmod (
	hak_t*            hak,
	const hak_ooch_t* name,
	hak_oow_t         namelen
);

void hak_closemod (
	hak_t*            hak,
	hak_mod_data_t*   mdp
);

/*
 * The hak_querymod() function finds a primitive function in modules
 * with a full primitive identifier.
 */
hak_pfbase_t* hak_querymod (
	hak_t*            hak,
	const hak_ooch_t* pfid,
	hak_oow_t         pfidlen,
	hak_mod_t**       mod
);

/* ========================================================================= */
/* fmt.c                                                                     */
/* ========================================================================= */
int hak_fmt_object (
	hak_t*        hak,
	hak_fmtout_t* fmtout,
	hak_oop_t     oop
);

int hak_prfmtcallstack (
	hak_t*    hak,
	hak_ooi_t nargs
);

int hak_logfmtcallstack (
	hak_t*    hak,
	hak_ooi_t nargs
);

int hak_strfmtcallstack (
	hak_t*    hak,
	hak_ooi_t nargs
);

int hak_scfmtcallstack (
	hak_t*    hak,
	hak_ooi_t nargs
);

/* ========================================================================= */
/* comp.c                                                                    */
/* ========================================================================= */
int hak_emitbyteinstruction (
	hak_t*     hak,
	hak_oob_t  bc
);

int hak_copy_string_to (
	hak_t*            hak,
	const hak_oocs_t* src,
	hak_oocs_t*       dst,
	hak_oow_t*        dstcapa,
	int               append,
	hak_ooch_t        delim_char
);

/* ========================================================================= */
/* cnode.c                                                                   */
/* ========================================================================= */
hak_cnode_t* hak_makecnode (hak_t* hak, hak_cnode_type_t type, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodenil (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodetrue (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodefalse (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodeself (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodesuper (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodeellipsis (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodetrpcolons (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodedblcolons (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodecolon (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodecharlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok, hak_ooch_t v);
hak_cnode_t* hak_makecnodebchrlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok, hak_oob_t v);
hak_cnode_t* hak_makecnodesymbol (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodedsymbol (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok, int is_cla);
hak_cnode_t* hak_makecnodebinop (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodestrlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodebstrlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodesymlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodenumlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnoderadnumlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodefpdeclit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok);
hak_cnode_t* hak_makecnodesmptrlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok, hak_oow_t v);
hak_cnode_t* hak_makecnodeerrlit (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok, hak_ooi_t v);
hak_cnode_t* hak_makecnodecons (hak_t* hak, int flags, const hak_loc_t* loc, const hak_oocs_t* tok, hak_cnode_t* car, hak_cnode_t* cdr);
hak_cnode_t* hak_makecnodeelist (hak_t* hak, int flags, const hak_loc_t* loc, hak_concode_t type);
hak_cnode_t* hak_makecnodeshell (hak_t* hak, int flags, const hak_loc_t* loc, hak_cnode_t* obj);
void hak_freesinglecnode (hak_t* hak, hak_cnode_t* c);
hak_oow_t hak_countcnodecons (hak_t* hak, hak_cnode_t* cons);
void hak_dumpcnode (hak_t* hak,  hak_cnode_t* c, int newline);


/* ========================================================================= */
/* read.c                                                                    */
/* ========================================================================= */
int hak_is_binop_string (const hak_oocs_t* v);

/* ========================================================================= */
/* exec.c                                                                    */
/* ========================================================================= */
int hak_class_responds_to (hak_t* hak, hak_oop_t rcv, hak_oop_t msg);
int hak_inst_responds_to (hak_t* hak, hak_oop_t rcv, hak_oop_t msg);

hak_pfrc_t hak_pf_number_add (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_number_sub (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_number_mul (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_number_div (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_number_sqrt (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_number_abs (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);

hak_pfrc_t hak_pf_integer_band (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_integer_blshift (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_integer_bnot (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_integer_bor (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_integer_brshift (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_integer_bshift (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_integer_bxor (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);

hak_pfrc_t hak_pf_number_gt (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_number_ge (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_number_lt (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_number_le (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_number_eq (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_number_ne (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);

hak_pfrc_t hak_pf_eqv (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_eql (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_eqk (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_nqv (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_nql (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_nqk (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);

hak_pfrc_t hak_pf_process_current (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_process_fork (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_process_resume (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_process_suspend (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_process_terminate (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_process_terminate_all (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_process_yield (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);

hak_pfrc_t hak_pf_semaphore_new (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_semaphore_wait (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_semaphore_signal (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_semaphore_signal_timed (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_semaphore_signal_on_input (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_semaphore_signal_on_output (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
/*hak_pfrc_t hak_pf_semaphore_signal_on_gcfin (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);*/
hak_pfrc_t hak_pf_semaphore_unsignal (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);

hak_pfrc_t hak_pf_semaphore_group_new (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_semaphore_group_add_semaphore (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_semaphore_group_remove_semaphore (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);
hak_pfrc_t hak_pf_semaphore_group_wait (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs);

/* ========================================================================= */
/* std.c                                                                    */
/* ========================================================================= */
hak_errnum_t hak_syserrstrb (hak_t* hak, int syserr_type, int syserr_code, hak_bch_t* buf, hak_oow_t len);

#if defined(__cplusplus)
}
#endif

#endif
