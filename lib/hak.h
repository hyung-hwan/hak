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

#ifndef _HAK_H_
#define _HAK_H_

#include <hak-cmn.h>
#include <hak-rbt.h>
#include <hak-xma.h>
#include <stdarg.h>

/* TODO: move this macro out to the build files.... */
#define HAK_INCLUDE_COMPILER

/* ========================================================================== */

typedef struct hak_mod_t hak_mod_t;

/* ========================================================================== */

struct hak_bloc_t
{
	hak_oow_t line; /**< line */
	hak_oow_t colm; /**< column */
	const hak_bch_t* file; /**< file specified in #include */
};
typedef struct hak_bloc_t hak_bloc_t;

struct hak_uloc_t
{
	hak_oow_t line; /**< line */
	hak_oow_t colm; /**< column */
	const hak_uch_t* file; /**< file specified in #include */
};
typedef struct hak_uloc_t hak_uloc_t;

#if defined(HAK_OOCH_IS_UCH)
typedef hak_uloc_t hak_loc_t;
#else
typedef hak_bloc_t hak_loc_t;
#endif

/* ========================================================================== */

/**
 * The hak_errnum_t type defines the error codes.
 */
enum hak_errnum_t
{
	HAK_ENOERR,   /**< no error */
	HAK_EGENERIC, /**< generic error */
	HAK_ENOIMPL,  /**< not implemented */
	HAK_ESYSERR,  /**< subsystem error */
	HAK_EINTERN,  /**< internal error */

	HAK_ESYSMEM,  /**< insufficient system memory */
	HAK_EOOMEM,   /**< insufficient object memory */
	HAK_ETYPE,    /**< invalid class/type */
	HAK_EINVAL,   /**< invalid parameter or data */
	HAK_ENOENT,   /**< data not found */

	HAK_EEXIST,   /**< existing/duplicate data */
	HAK_EBUSY,
	HAK_EACCES,
	HAK_EPERM,
	HAK_ENOTDIR,

	HAK_EINTR,
	HAK_EPIPE,
	HAK_EAGAIN,
	HAK_EBADHND,
	HAK_EFRMFLOOD,  /**< too many frames */

	HAK_EMSGRCV,    /**< mesasge receiver error */
	HAK_EMSGSND,    /**< message sending error. even doesNotUnderstand: is not found */
	HAK_ENUMARGS,   /**< wrong number of arguments */
	HAK_ERANGE,     /**< range error. overflow and underflow */
	HAK_EBCFULL,    /**< byte-code full */

	HAK_EDFULL,     /**< dictionary full */
	HAK_EPFULL,     /**< processor full */
	HAK_EFINIS,     /**< unexpected end of data/input/stream/etc */
	HAK_EFLOOD,     /**< too many items/data */
	HAK_EDIVBY0,    /**< divide by zero */

	HAK_EIOERR,     /**< I/O error */
	HAK_EECERR,     /**< encoding conversion error */
	HAK_EBUFFULL,   /**< buffer full */
	HAK_ESYNERR,    /**< syntax error */
	HAK_ECALL,      /**< runtime error - cannot call */
	HAK_ECALLARG,   /**< runtime error - wrong number of arguments to call */
	HAK_ECALLRET,   /**< runtime error - wrong number of return variables to call */
	HAK_ESEMFLOOD,  /**< runtime error - too many semaphores */
	HAK_EEXCEPT,    /**< runtime error - exception not handled */
	HAK_ESTKOVRFLW, /**< runtime error - stack overflow */
	HAK_ESTKUNDFLW, /**< runtime error - stack overflow */
	HAK_EUNDEFVAR   /**< runtime error - undefined variable access */
};
typedef enum hak_errnum_t hak_errnum_t;
/**/

enum hak_synerrnum_t
{
	HAK_SYNERR_NOERR,
	HAK_SYNERR_INTERN,        /* internal error */
	HAK_SYNERR_CNODE,         /* unexpected compiler node */
	HAK_SYNERR_ILCHR,         /* illegal character */
	HAK_SYNERR_ILTOK,         /* invalid token */
	HAK_SYNERR_CMTNC,         /* comment not closed */
	HAK_SYNERR_CHARLIT,       /* wrong character literal */
	HAK_SYNERR_STRLIT,        /* wrong string literal */
	HAK_SYNERR_SYMLIT,        /* wrong symbol literal */
	HAK_SYNERR_NUMLIT ,       /* invalid numeric literal */
	HAK_SYNERR_NUMRANGE,      /* number range error */
	HAK_SYNERR_ERRLIT,        /* wrong error literal */
	HAK_SYNERR_SMPTRLIT,      /* wrong smptr literal */
	HAK_SYNERR_RADIX,         /* invalid radix for a numeric literal */

	HAK_SYNERR_EOF,           /* sudden end of input */
	HAK_SYNERR_LPAREN,        /* ( expected */
	HAK_SYNERR_RPAREN,        /* ) expected */
	HAK_SYNERR_RBRACK,        /* ] expected */
	HAK_SYNERR_RBRACE,        /* } expected */
	HAK_SYNERR_VBAR,          /* | expected */

	HAK_SYNERR_IDENT,         /* identifier expected */
	HAK_SYNERR_STRING,        /* string expected */
	HAK_SYNERR_BYTERANGE,     /* byte too small or too large */
	HAK_SYNERR_NESTING,       /* nesting level too deep */

	HAK_SYNERR_COMMA,         /* , expected */
	HAK_SYNERR_VBARBANNED,    /* | disallowed */
	HAK_SYNERR_DOTBANNED,     /* . disallowed */
	HAK_SYNERR_COMMABANNED,   /* , disallowed */
	HAK_SYNERR_COLONBANNED,   /* : disallowed */
	HAK_SYNERR_COLONEQBANNED, /* := disallowed */
	HAK_SYNERR_COMMANOVALUE,  /* no value after , */
	HAK_SYNERR_COLONNOVALUE,  /* no value after : */
	HAK_SYNERR_NOVALUE,       /* missing value */
	HAK_SYNERR_NOSEP,         /* no separator between array/dictionary elements */
	HAK_SYNERR_INCLUDE,       /* #include error */

	HAK_SYNERR_ELLIPSISBANNED, /* ... disallowed */
	HAK_SYNERR_TRPCOLONSBANNED, /* ::: disallowed */
	HAK_SYNERR_LOOPFLOOD,     /* loop body too big */
	HAK_SYNERR_IFFLOOD,       /* if body too big */
	HAK_SYNERR_BLKFLOOD,      /* block too big */
	HAK_SYNERR_BLKDEPTH,      /* block too deep */
	HAK_SYNERR_ARGNAMELIST,   /* argument name list expected */
	HAK_SYNERR_ARGNAME,       /* argument name expected */
	HAK_SYNERR_ARGNAMEDUP,    /* duplicate argument name  */
	HAK_SYNERR_VARNAME,       /* variable name expected */
	HAK_SYNERR_ARGCOUNT,      /* wrong number of arguments */
	HAK_SYNERR_ARGFLOOD,      /* too many arguments defined */
	HAK_SYNERR_VARFLOOD,      /* too many variables defined */
	HAK_SYNERR_VARDCLBANNED,  /* variable declaration disallowed */
	HAK_SYNERR_VARNAMEDUP,    /* duplicate variable name */
	HAK_SYNERR_VARNAMEUNKNOWN, /* unknown variable name */

	HAK_SYNERR_BANNEDVARNAME, /* disallowed varible name */
	HAK_SYNERR_BANNEDARGNAME, /* disallowed argument name */
	HAK_SYNERR_BANNED,        /* prohibited */

	HAK_SYNERR_CLASS,         /* invalid class definition */
	HAK_SYNERR_FUN,           /* invalid function definition */
	HAK_SYNERR_VAR,           /* invalid variable declaration */
	HAK_SYNERR_ELIF,          /* elif without if */
	HAK_SYNERR_ELSE,          /* else without if */
	HAK_SYNERR_CATCH,         /* catch outside try */
	HAK_SYNERR_BREAK,         /* break outside loop */

	HAK_SYNERR_CALLABLE,      /* invalid callable */
	HAK_SYNERR_MESSAGE,       /* invalid message */
	HAK_SYNERR_UNBALKV,       /* unbalanced key/value pair */
	HAK_SYNERR_UNBALPBB,      /* unbalanced parenthesis/brace/bracket */
	HAK_SYNERR_SEMICOLON,     /* unexpected semicolon */
	HAK_SYNERR_BACKSLASH,     /* stray backslash */
	HAK_SYNERR_BLOCK,         /* block expression expected */
	HAK_SYNERR_BLOCKBANNED,   /* block expression disallowed */
	HAK_SYNERR_LVALUE,        /* invalid lvalue */
	HAK_SYNERR_RVALUE         /* invalid rvalue */
};
typedef enum hak_synerrnum_t hak_synerrnum_t;


/* ========================================================================== */

#define HAK_ERRMSG_CAPA (2048)

/**
 * The hak_errbinf_t type defines a placeholder for error information.
 */
struct hak_errbinf_t
{
	hak_oow_t    _instsize;
	hak_errnum_t num;                  /**< error number */
	hak_bch_t    msg[HAK_ERRMSG_CAPA]; /**< error message */
	hak_bloc_t   loc;                  /**< error location */
};
typedef struct hak_errbinf_t hak_errbinf_t;

struct hak_erruinf_t
{
	hak_oow_t    _instsize;
	hak_errnum_t num;                  /**< error number */
	hak_uch_t    msg[HAK_ERRMSG_CAPA]; /**< error message */
	hak_uloc_t   loc;                  /**< error location */
};
typedef struct hak_erruinf_t hak_erruinf_t;

#if defined(HAK_OOCH_IS_UCH)
typedef hak_erruinf_t hak_errinf_t;
#else
typedef hak_errbinf_t hak_errinf_t;
#endif

/* ========================================================================== */

enum hak_option_t
{
	HAK_TRAIT,
	HAK_LOG_MASK,
	HAK_LOG_MAXCAPA,

	HAK_LOG_TARGET_BCSTR,
	HAK_LOG_TARGET_UCSTR,
	HAK_LOG_TARGET_BCS,
	HAK_LOG_TARGET_UCS,
#if defined(HAK_OOCH_IS_UCH)
#	define HAK_LOG_TARGET HAK_LOG_TARGET_UCSTR
#	define HAK_LOG_TARGET_OOCSTR HAK_LOG_TARGET_UCSTR
#	define HAK_LOG_TARGET_OOCS HAK_LOG_TARGET_UCS
#else
#	define HAK_LOG_TARGET HAK_LOG_TARGET_BCSTR
#	define HAK_LOG_TARGET_OOCSTR HAK_LOG_TARGET_BCSTR
#	define HAK_LOG_TARGET_OOCS HAK_LOG_TARGET_BCS
#endif

	HAK_SYMTAB_SIZE,  /* default system table size */
	HAK_SYSDIC_SIZE,  /* default system dictionary size */
	HAK_PROCSTK_SIZE, /* default process stack size */

	HAK_MOD_LIBDIRS,
	HAK_MOD_PREFIX,
	HAK_MOD_POSTFIX,

	HAK_MOD_INCTX
};
typedef enum hak_option_t hak_option_t;

/* [NOTE] ensure that it is a power of 2 */
#define HAK_LOG_CAPA_ALIGN 512

enum hak_option_dflval_t
{
	HAK_DFL_LOG_MAXCAPA = HAK_LOG_CAPA_ALIGN * 16,
	HAK_DFL_SYMTAB_SIZE = 5000,
	HAK_DFL_SYSDIC_SIZE = 5000,
	HAK_DFL_PROCSTK_SIZE = 5000
};
typedef enum hak_option_dflval_t hak_option_dflval_t;

enum hak_trait_t
{
#if defined(HAK_BUILD_DEBUG)
	HAK_TRAIT_DEBUG_GC     = (((hak_bitmask_t)1) << 0),
	HAK_TRAIT_DEBUG_BIGINT = (((hak_bitmask_t)1) << 1),
#endif

	HAK_TRAIT_INTERACTIVE = (((hak_bitmask_t)1) << 7),

	/* perform no garbage collection when the heap is full.
	 * you still can use hak_gc() explicitly. */
	HAK_TRAIT_NOGC = (((hak_bitmask_t)1) << 8),

	/* wait for running process when exiting from the main method */
	HAK_TRAIT_AWAIT_PROCS = (((hak_bitmask_t)1) << 9),

	/* return EOL as a token */ /* TODO: make this pragma controllable */
	HAK_TRAIT_LANG_ENABLE_EOL = (((hak_bitmask_t)1) << 14),

	/* TODO: make this pragma controllable */
	HAK_TRAIT_LANG_LIBERAL = ((hak_bitmask_t)1) << 15
};
typedef enum hak_trait_t hak_trait_t;

/* =========================================================================
 * SPECIALIZED OOP TYPES
 * ========================================================================= */

/* hak_oop_t defined in hak-cmn.h
 * hak_obj_t defined further down */

/* these are more specialized types for hak_obj_t */
typedef struct hak_obj_oop_t       hak_obj_oop_t;
typedef struct hak_obj_char_t      hak_obj_char_t;
typedef struct hak_obj_byte_t      hak_obj_byte_t;
typedef struct hak_obj_halfword_t  hak_obj_halfword_t;
typedef struct hak_obj_word_t      hak_obj_word_t;

/* these are more specialized types for hak_oop_t */
typedef struct hak_obj_oop_t*      hak_oop_oop_t;
typedef struct hak_obj_char_t*     hak_oop_char_t;
typedef struct hak_obj_byte_t*     hak_oop_byte_t;
typedef struct hak_obj_halfword_t* hak_oop_halfword_t;
typedef struct hak_obj_word_t*     hak_oop_word_t;

#define HAK_OOP_BITS  (HAK_SIZEOF_OOP_T * HAK_BITS_PER_BYTE)

#if defined(HAK_USE_OOW_FOR_LIW)
	typedef hak_oop_word_t     hak_oop_liword_t;
#	define HAK_OBJ_TYPE_LIWORD HAK_OBJ_TYPE_WORD
#else
	typedef hak_oop_halfword_t hak_oop_liword_t;
#	define HAK_OBJ_TYPE_LIWORD HAK_OBJ_TYPE_HALFWORD
#endif

/* =========================================================================
 * HEADER FOR GC IMPLEMENTATION
 * ========================================================================= */
typedef struct hak_gchdr_t hak_gchdr_t;
struct hak_gchdr_t
{
        hak_gchdr_t* next;
};
/* The size of hak_gchdr_t must be aligned to HAK_SIZEOF_OOP_T */

/* =========================================================================
 * OBJECT STRUCTURE
 * ========================================================================= */
enum hak_obj_type_t
{
	HAK_OBJ_TYPE_OOP,
	HAK_OBJ_TYPE_CHAR,
	HAK_OBJ_TYPE_BYTE,
	HAK_OBJ_TYPE_HALFWORD,
	HAK_OBJ_TYPE_WORD

/*
	HAK_OBJ_TYPE_UINT8,
	HAK_OBJ_TYPE_UINT16,
	HAK_OBJ_TYPE_UINT32,
*/

/* NOTE: you can have HAK_OBJ_SHORT, HAK_OBJ_INT
 * HAK_OBJ_LONG, HAK_OBJ_FLOAT, HAK_OBJ_DOUBLE, etc
 * type field being 6 bits long, you can have up to 64 different types.

	HAK_OBJ_TYPE_SHORT,
	HAK_OBJ_TYPE_INT,
	HAK_OBJ_TYPE_LONG,
	HAK_OBJ_TYPE_FLOAT,
	HAK_OBJ_TYPE_DOUBLE */
};
typedef enum hak_obj_type_t hak_obj_type_t;

/* =========================================================================
 * Object header structure
 *
 * _flags:
 *   type: the type of a payload item.
 *         one of HAK_OBJ_TYPE_OOP, HAK_OBJ_TYPE_CHAR,
 *                HAK_OBJ_TYPE_BYTE, HAK_OBJ_TYPE_HALFWORD, HAK_OBJ_TYPE_WORD
 *   unit: the size of a payload item in bytes.
 *   extra: 0 or 1. 1 indicates that the payload contains 1 more
 *          item than the value of the size field. used for a
 *          terminating null in a variable-char object. internel
 *          use only.
 *   kernel: 0 - ordinary object.
 *           1 - kernel object. can survive hak_resetcode().
 *           2 - kernel object. can survive hak_resetcode().
 *               a symbol object with 2 in the kernel bits cannot be assigned a
 *               value with the 'set' special form.
 *   moved: 0 or 1. used by GC. internal use only.
 *   ngc: 0 or 1, used by GC. internal use only.
 *   trailer: 0 or 1. indicates that there are trailing bytes
 *            after the object payload. internal use only.
 *
 * _size: the number of payload items in an object.
 *        it doesn't include the header size.
 *
 * The total number of bytes occupied by an object can be calculated
 * with this fomula:
 *    sizeof(hak_obj_t) + ALIGN((size + extra) * unit), sizeof(hak_oop_t))
 *
 * If the type is known to be not HAK_OBJ_TYPE_CHAR, you can assume that
 * 'extra' is 0. So you can simplify the fomula in such a context.
 *    sizeof(hak_obj_t) + ALIGN(size * unit), sizeof(hak_oop_t))
 *
 * The ALIGN() macro is used above since allocation adjusts the payload
 * size to a multiple of sizeof(hak_oop_t). it assumes that sizeof(hak_obj_t)
 * is a multiple of sizeof(hak_oop_t). See the HAK_BYTESOF() macro.
 *
 * Due to the header structure, an object can only contain items of
 * homogeneous data types in the payload.
 *
 * It's actually possible to split the size field into 2. For example,
 * the upper half contains the number of oops and the lower half contains
 * the number of bytes/chars. This way, a variable-byte class or a variable-char
 * class can have normal instance variables. On the contrary, the actual byte
 * size calculation and the access to the payload fields become more complex.
 * Therefore, i've dropped the idea.
 * ========================================================================= */
#define HAK_OBJ_FLAGS_TYPE_BITS      (6) /* 6 */
#define HAK_OBJ_FLAGS_UNIT_BITS      (5) /* 11 */
#define HAK_OBJ_FLAGS_EXTRA_BITS     (1) /* 12 */
#define HAK_OBJ_FLAGS_KERNEL_BITS    (2) /* 14 */
#define HAK_OBJ_FLAGS_MOVED_BITS     (2) /* 16 */
#define HAK_OBJ_FLAGS_NGC_BITS       (1) /* 17 */
#define HAK_OBJ_FLAGS_TRAILER_BITS   (1) /* 18 */
#define HAK_OBJ_FLAGS_CONCODE_BITS   (4) /* 22 - concode for cons */
#define HAK_OBJ_FLAGS_FLEXI_BITS     (1) /* 23 */
#define HAK_OBJ_FLAGS_RDONLY_BITS    (1) /* 24 */
#define HAK_OBJ_FLAGS_PROC_BITS      (1) /* 25 */

/*
#define HAK_OBJ_FLAGS_PERM_BITS       1
#define HAK_OBJ_FLAGS_MOVED_BITS      2
#define HAK_OBJ_FLAGS_GCFIN_BITS      4
#define HAK_OBJ_FLAGS_TRAILER_BITS    1
#define HAK_OBJ_FLAGS_HASH_BITS       2
#define HAK_OBJ_FLAGS_UNCOPYABLE_BITS 1
*/

#define HAK_OBJ_FLAGS_TYPE_SHIFT        (HAK_OBJ_FLAGS_UNIT_BITS      + HAK_OBJ_FLAGS_UNIT_SHIFT)
#define HAK_OBJ_FLAGS_UNIT_SHIFT        (HAK_OBJ_FLAGS_EXTRA_BITS     + HAK_OBJ_FLAGS_EXTRA_SHIFT)
#define HAK_OBJ_FLAGS_EXTRA_SHIFT       (HAK_OBJ_FLAGS_KERNEL_BITS    + HAK_OBJ_FLAGS_KERNEL_SHIFT)
#define HAK_OBJ_FLAGS_KERNEL_SHIFT      (HAK_OBJ_FLAGS_MOVED_BITS     + HAK_OBJ_FLAGS_MOVED_SHIFT)
#define HAK_OBJ_FLAGS_MOVED_SHIFT       (HAK_OBJ_FLAGS_NGC_BITS       + HAK_OBJ_FLAGS_NGC_SHIFT)
#define HAK_OBJ_FLAGS_NGC_SHIFT         (HAK_OBJ_FLAGS_TRAILER_BITS   + HAK_OBJ_FLAGS_TRAILER_SHIFT)
#define HAK_OBJ_FLAGS_TRAILER_SHIFT     (HAK_OBJ_FLAGS_CONCODE_BITS   + HAK_OBJ_FLAGS_CONCODE_SHIFT)
#define HAK_OBJ_FLAGS_CONCODE_SHIFT     (HAK_OBJ_FLAGS_FLEXI_BITS     + HAK_OBJ_FLAGS_FLEXI_SHIFT)
#define HAK_OBJ_FLAGS_FLEXI_SHIFT       (HAK_OBJ_FLAGS_RDONLY_BITS    + HAK_OBJ_FLAGS_RDONLY_SHIFT)
#define HAK_OBJ_FLAGS_RDONLY_SHIFT      (HAK_OBJ_FLAGS_PROC_BITS      + HAK_OBJ_FLAGS_PROC_SHIFT)
#define HAK_OBJ_FLAGS_PROC_SHIFT        (0)

#define HAK_OBJ_GET_FLAGS_TYPE(oop)        HAK_GETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_TYPE_SHIFT,      HAK_OBJ_FLAGS_TYPE_BITS)
#define HAK_OBJ_GET_FLAGS_UNIT(oop)        HAK_GETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_UNIT_SHIFT,      HAK_OBJ_FLAGS_UNIT_BITS)
#define HAK_OBJ_GET_FLAGS_EXTRA(oop)       HAK_GETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_EXTRA_SHIFT,     HAK_OBJ_FLAGS_EXTRA_BITS)
#define HAK_OBJ_GET_FLAGS_KERNEL(oop)      HAK_GETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_KERNEL_SHIFT,    HAK_OBJ_FLAGS_KERNEL_BITS)
#define HAK_OBJ_GET_FLAGS_MOVED(oop)       HAK_GETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_MOVED_SHIFT,     HAK_OBJ_FLAGS_MOVED_BITS)
#define HAK_OBJ_GET_FLAGS_NGC(oop)         HAK_GETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_NGC_SHIFT,       HAK_OBJ_FLAGS_NGC_BITS)
#define HAK_OBJ_GET_FLAGS_TRAILER(oop)     HAK_GETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_TRAILER_SHIFT,   HAK_OBJ_FLAGS_TRAILER_BITS)
#define HAK_OBJ_GET_FLAGS_CONCODE(oop)     HAK_GETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_CONCODE_SHIFT,   HAK_OBJ_FLAGS_CONCODE_BITS)
#define HAK_OBJ_GET_FLAGS_FLEXI(oop)       HAK_GETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_FLEXI_SHIFT,     HAK_OBJ_FLAGS_FLEXI_BITS)
#define HAK_OBJ_GET_FLAGS_RDONLY(oop)      HAK_GETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_RDONLY_SHIFT,    HAK_OBJ_FLAGS_RDONLY_BITS)
#define HAK_OBJ_GET_FLAGS_PROC(oop)        HAK_GETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_PROC_SHIFT,      HAK_OBJ_FLAGS_PROC_BITS)

#define HAK_OBJ_SET_FLAGS_TYPE(oop,v)      HAK_SETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_TYPE_SHIFT,      HAK_OBJ_FLAGS_TYPE_BITS,      v)
#define HAK_OBJ_SET_FLAGS_UNIT(oop,v)      HAK_SETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_UNIT_SHIFT,      HAK_OBJ_FLAGS_UNIT_BITS,      v)
#define HAK_OBJ_SET_FLAGS_EXTRA(oop,v)     HAK_SETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_EXTRA_SHIFT,     HAK_OBJ_FLAGS_EXTRA_BITS,     v)
#define HAK_OBJ_SET_FLAGS_KERNEL(oop,v)    HAK_SETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_KERNEL_SHIFT,    HAK_OBJ_FLAGS_KERNEL_BITS,    v)
#define HAK_OBJ_SET_FLAGS_MOVED(oop,v)     HAK_SETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_MOVED_SHIFT,     HAK_OBJ_FLAGS_MOVED_BITS,     v)
#define HAK_OBJ_SET_FLAGS_NGC(oop,v)       HAK_SETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_NGC_SHIFT,       HAK_OBJ_FLAGS_NGC_BITS,       v)
#define HAK_OBJ_SET_FLAGS_TRAILER(oop,v)   HAK_SETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_TRAILER_SHIFT,   HAK_OBJ_FLAGS_TRAILER_BITS,   v)
#define HAK_OBJ_SET_FLAGS_CONCODE(oop,v)   HAK_SETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_CONCODE_SHIFT,   HAK_OBJ_FLAGS_CONCODE_BITS,   v)
#define HAK_OBJ_SET_FLAGS_FLEXI(oop,v)     HAK_SETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_FLEXI_SHIFT,     HAK_OBJ_FLAGS_FLEXI_BITS, v)
#define HAK_OBJ_SET_FLAGS_RDONLY(oop,v)    HAK_SETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_RDONLY_SHIFT,    HAK_OBJ_FLAGS_RDONLY_BITS, v)
#define HAK_OBJ_SET_FLAGS_PROC(oop,v)      HAK_SETBITS(hak_oow_t, (oop)->_flags, HAK_OBJ_FLAGS_PROC_SHIFT,      HAK_OBJ_FLAGS_PROC_BITS, v)

/* [NOTE] this macro doesn't check the range of the actual value.
 *        make sure that the value of each bit fields given falls within the
 *        possible range of the defined bits */
#define HAK_OBJ_MAKE_FLAGS(t,u,e,k,m,g,r) ( \
	(((hak_oow_t)(t)) << HAK_OBJ_FLAGS_TYPE_SHIFT) | \
	(((hak_oow_t)(u)) << HAK_OBJ_FLAGS_UNIT_SHIFT) | \
	(((hak_oow_t)(e)) << HAK_OBJ_FLAGS_EXTRA_SHIFT) | \
	(((hak_oow_t)(k)) << HAK_OBJ_FLAGS_KERNEL_SHIFT) | \
	(((hak_oow_t)(m)) << HAK_OBJ_FLAGS_MOVED_SHIFT) | \
	(((hak_oow_t)(g)) << HAK_OBJ_FLAGS_NGC_SHIFT) | \
	(((hak_oow_t)(r)) << HAK_OBJ_FLAGS_TRAILER_SHIFT) \
)

#define HAK_OBJ_FLAGS_KERNEL_USER     0  /* not a kernel object */
#define HAK_OBJ_FLAGS_KERNEL_IMMATURE 1  /* incomplete kernel object. defined in gc.c for bootstrapping. but no complete class definition has been read */
#define HAK_OBJ_FLAGS_KERNEL_MATURE   2  /* kernel  object with its full class defintion read in */

#define HAK_OBJ_GET_SIZE(oop) ((oop)->_size)
#define HAK_OBJ_GET_CLASS(oop) ((oop)->_class)

#define HAK_OBJ_SET_SIZE(oop,v) ((oop)->_size = (v))
#define HAK_OBJ_SET_CLASS(oop,c) ((oop)->_class = (c))

/* [NOTE] this macro doesn't include the size of the trailer */
#define HAK_OBJ_BYTESOF(oop) ((HAK_OBJ_GET_SIZE(oop) + HAK_OBJ_GET_FLAGS_EXTRA(oop)) * HAK_OBJ_GET_FLAGS_UNIT(oop))

#define HAK_OBJ_IS_OOP_POINTER(oop)      (HAK_OOP_IS_POINTER(oop) && (HAK_OBJ_GET_FLAGS_TYPE(oop) == HAK_OBJ_TYPE_OOP))
#define HAK_OBJ_IS_CHAR_POINTER(oop)     (HAK_OOP_IS_POINTER(oop) && (HAK_OBJ_GET_FLAGS_TYPE(oop) == HAK_OBJ_TYPE_CHAR))
#define HAK_OBJ_IS_BYTE_POINTER(oop)     (HAK_OOP_IS_POINTER(oop) && (HAK_OBJ_GET_FLAGS_TYPE(oop) == HAK_OBJ_TYPE_BYTE))
#define HAK_OBJ_IS_HALFWORD_POINTER(oop) (HAK_OOP_IS_POINTER(oop) && (HAK_OBJ_GET_FLAGS_TYPE(oop) == HAK_OBJ_TYPE_HALFWORD))
#define HAK_OBJ_IS_WORD_POINTER(oop)     (HAK_OOP_IS_POINTER(oop) && (HAK_OBJ_GET_FLAGS_TYPE(oop) == HAK_OBJ_TYPE_WORD))

#define HAK_STORE_OOP(hak,var,val) (*(var) = val)

#define HAK_OBJ_HEADER \
	hak_oow_t _flags; \
	hak_oow_t _size; \
	hak_oop_t _class

struct hak_obj_t
{
	HAK_OBJ_HEADER;
};

struct hak_obj_oop_t
{
	HAK_OBJ_HEADER;
	hak_oop_t slot[1];
};

struct hak_obj_char_t
{
	HAK_OBJ_HEADER;
	hak_ooch_t slot[1];
};

struct hak_obj_byte_t
{
	HAK_OBJ_HEADER;
	hak_oob_t slot[1];
};

struct hak_obj_halfword_t
{
	HAK_OBJ_HEADER;
	hak_oohw_t slot[1];
};

struct hak_obj_word_t
{
	HAK_OBJ_HEADER;
	hak_oow_t slot[1];
};

#define HAK_OBJ_GET_OOP_SLOT(oop)      (((hak_oop_oop_t)(oop))->slot)
#define HAK_OBJ_GET_CHAR_SLOT(oop)     (((hak_oop_char_t)(oop))->slot)
#define HAK_OBJ_GET_BYTE_SLOT(oop)     (((hak_oop_byte_t)(oop))->slot)
#define HAK_OBJ_GET_HALFWORD_SLOT(oop) (((hak_oop_halfword_t)(oop))->slot)
#define HAK_OBJ_GET_WORD_SLOT(oop)     (((hak_oop_word_t)(oop))->slot)
#define HAK_OBJ_GET_LIWORD_SLOT(oop)   (((hak_oop_liword_t)(oop))->slot)

#define HAK_OBJ_GET_OOP_PTR(oop,idx)      (&(((hak_oop_oop_t)(oop))->slot)[idx])
#define HAK_OBJ_GET_CHAR_PTR(oop,idx)     (&(((hak_oop_char_t)(oop))->slot)[idx])
#define HAK_OBJ_GET_BYTE_PTR(oop,idx)     (&(((hak_oop_byte_t)(oop))->slot)[idx])
#define HAK_OBJ_GET_HALFWORD_PTR(oop,idx) (&(((hak_oop_halfword_t)(oop))->slot)[idx])
#define HAK_OBJ_GET_WORD_PTR(oop,idx)     (&(((hak_oop_word_t)(oop))->slot)[idx])
#define HAK_OBJ_GET_LIWORD_PTR(oop,idx)   (&(((hak_oop_liword_t)(oop))->slot)[idx])

#define HAK_OBJ_GET_OOP_VAL(oop,idx)      ((((hak_oop_oop_t)(oop))->slot)[idx])
#define HAK_OBJ_GET_CHAR_VAL(oop,idx)     ((((hak_oop_char_t)(oop))->slot)[idx])
#define HAK_OBJ_GET_BYTE_VAL(oop,idx)     ((((hak_oop_byte_t)(oop))->slot)[idx])
#define HAK_OBJ_GET_HALFWORD_VAL(oop,idx) ((((hak_oop_halfword_t)(oop))->slot)[idx])
#define HAK_OBJ_GET_WORD_VAL(oop,idx)     ((((hak_oop_word_t)(oop))->slot)[idx])
#define HAK_OBJ_GET_LIWORD_VAL(oop,idx)   ((((hak_oop_liword_t)(oop))->slot)[idx])

#define HAK_OBJ_SET_OOP_VAL(oop,idx,val)      ((((hak_oop_oop_t)(oop))->slot)[idx] = (val))
#define HAK_OBJ_SET_CHAR_VAL(oop,idx,val)     ((((hak_oop_char_t)(oop))->slot)[idx] = (val))
#define HAK_OBJ_SET_BYTE_VAL(oop,idx,val)     ((((hak_oop_byte_t)(oop))->slot)[idx] = (val))
#define HAK_OBJ_SET_HALFWORD_VAL(oop,idx,val) ((((hak_oop_halfword_t)(oop))->slot)[idx] = (val))
#define HAK_OBJ_SET_WORD_VAL(oop,idx,val)     ((((hak_oop_word_t)(oop))->slot)[idx] = (val))
#define HAK_OBJ_SET_LIWORD_VAL(oop,idx,val)   ((((hak_oop_liword_t)(oop))->slot)[idx] = (val))

typedef struct hak_trailer_t hak_trailer_t;
struct hak_trailer_t
{
	hak_oow_t size;
	hak_oob_t slot[1];
};

#define HAK_OBJ_GET_TRAILER_BYTE(oop) ((hak_oob_t*)&((hak_oop_oop_t)oop)->slot[HAK_OBJ_GET_SIZE(oop) + 1])
#define HAK_OBJ_GET_TRAILER_SIZE(oop) ((hak_oow_t)((hak_oop_oop_t)oop)->slot[HAK_OBJ_GET_SIZE(oop)])

#define HAK_PRIM_NAMED_INSTVARS 4
typedef struct hak_prim_t hak_prim_t;
typedef struct hak_prim_t* hak_oop_prim_t;
struct hak_prim_t
{
	HAK_OBJ_HEADER;
	hak_oow_t impl;
	hak_oow_t min_nargs;
	hak_oow_t max_nargs;
	hak_oow_t mod;
};

#define HAK_CONS_NAMED_INSTVARS 2
typedef struct hak_cons_t hak_cons_t;
typedef struct hak_cons_t* hak_oop_cons_t;
struct hak_cons_t
{
	HAK_OBJ_HEADER;
	hak_oop_t car;
	hak_oop_t cdr;
};

#define HAK_DIC_NAMED_INSTVARS 2
typedef struct hak_dic_t hak_dic_t;
typedef struct hak_dic_t* hak_oop_dic_t;
struct hak_dic_t
{
	HAK_OBJ_HEADER;
	hak_oop_t     tally;  /* SmallInteger */
	hak_oop_oop_t bucket; /* Array */
};

#define HAK_FPDEC_NAMED_INSTVARS 2
typedef struct hak_fpdec_t hak_fpdec_t;
typedef struct hak_fpdec_t* hak_oop_fpdec_t;
struct hak_fpdec_t
{
	HAK_OBJ_HEADER;
	hak_oop_t value; /* smooi or bigint */
	hak_oop_t scale; /* smooi, positive */
};

/* the first byte after the main payload is the trailer size
 * the code bytes are placed after the trailer size.
 *
 * code bytes -> ((hak_oob_t*)&((hak_oop_oop_t)m)->slot[HAK_OBJ_GET_SIZE(m) + 1]) or
 *               ((hak_oob_t*)&((hak_oop_function_t)m)->literal_frame[HAK_OBJ_GET_SIZE(m) + 1 - HAK_METHOD_NAMED_INSTVARS])
 * size -> ((hak_oow_t)((hak_oop_oop_t)m)->slot[HAK_OBJ_GET_SIZE(m)])*/
#define HAK_FUNCTION_GET_CODE_BYTE(m) HAK_OBJ_GET_TRAILER_BYTE(m)
#define HAK_FUNCTION_GET_CODE_SIZE(m) HAK_OBJ_GET_TRAILER_SIZE(m)

#define HAK_FUNCTION_NAMED_INSTVARS 5   /* this excludes literal frames and byte codes */
typedef struct hak_function_t hak_function_t;
typedef struct hak_function_t* hak_oop_function_t;

#define HAK_BLOCK_NAMED_INSTVARS 4
typedef struct hak_block_t hak_block_t;
typedef struct hak_block_t* hak_oop_block_t;

#define HAK_CONTEXT_NAMED_INSTVARS 11
typedef struct hak_context_t hak_context_t;
typedef struct hak_context_t* hak_oop_context_t;

struct hak_function_t
{
	HAK_OBJ_HEADER;

	hak_oop_t         attr_mask; /* smooi */
	hak_oop_t         name; /* symbol or nil */
	hak_oop_context_t home; /* home context. nil for the initial function */

	hak_oop_t dbgi; /* byte array containing debug information. nil if not available */
	hak_oop_t literal_frame_size;

	/* == variable indexed part == */
	hak_oop_t literal_frame[1]; /* it stores literals. it may not exist */

	/* after the literal frame comes the actual byte code */
};

/* hak_function_t copies the byte codes and literal frames into itself
 * hak_block_t contains minimal information(ip) for referening byte codes
 * and literal frames available in home->origin. it represents the compiled block.
 */
struct hak_block_t
{
	HAK_OBJ_HEADER;

	hak_oop_t         attr_mask; /* smooi */
	hak_oop_t         name; /* symbol or nil */
	hak_oop_context_t home; /* home context */
	hak_oop_t         ip; /* smooi. instruction pointer where the byte code begins in home->base */
};

struct hak_context_t
{
	HAK_OBJ_HEADER;

	/* SmallInteger */
	hak_oop_t          req_nrets;

	/* SmallInteger. */
	hak_oop_t          attr_mask;
	hak_oop_t          name; /* symbol or nil. copied from the compiled block.  TODO: is it better to maintain the backward pointer to the compiled block itself? */

	/* SmallInteger, instruction pointer */
	hak_oop_t          ip;

	/* the initial context is created with the initial function object in this field.
	 * a function-based context is created with the activating function object.
	 * a block-based context is created with the function object that the base field of
	 * the home context of the activating block context points to. */
	hak_oop_function_t base; /* function */

	/* it points to the active context at the moment when
	 * this context object has been activated. a new method context
	 * is activated as a result of normal message sending and a block
	 * context is activated when it is sent 'value'. it's set to
	 * nil if a block context created hasn't received 'value'. */
	hak_oop_context_t  sender; /* context or nil */

	/* it points to the receiver of the message for a method context.
	 * a block context points to a block object and a function context
	 * points to a function object */
	hak_oop_t          receiver;

	/* for a block context, it points to the active context at the
	 * moment the block context was created. that is, it points to
	 * a method context where the base block has been defined.
	 * an activated block context copies this field from the base block context. */
	hak_oop_context_t  home; /* context or nil */

	/* it is set to itself for a method context, nil for other contexts.
	 * TODO: this field may not be needed.. mthhome access has been commented out.. so remove this field */
	hak_oop_context_t mthhome;

	/* instance variable access instructions hold the index to a variable within
	 * the the containing class. If the class inherits from a superclass and the
	 * superclass chain contains instance variables, the actual index must be
	 * added with the number of instance variables in the superclass chain.
	 *
	 * for example, the following instruction accesses the instance variable slot
	 * at index 3. if the class of the instance has 4 instance variables in the
	 * superclass side, the method context activated has 4 in thie field.
	 * therefore, the instruction accesses the instance variable slot at index 7.
	 *   push_ivar 3
	 * the debug output shows this instruction as "push_ivar 3; [4]"
	 */
	hak_oop_t          ivaroff;

	/* method owner if this context is created of a message send. nil otherwise */
	hak_oop_t          owner;  /* class(hak_oop_class_t) or nil */

	/* variable indexed part */
	hak_oop_t          slot[1]; /* arguments, return variables, local variables, other arguments, etc */
};

#define HAK_PROCESS_NAMED_INSTVARS (15)
typedef struct hak_process_t hak_process_t;
typedef struct hak_process_t* hak_oop_process_t;

#define HAK_SEMAPHORE_NAMED_INSTVARS (11)
typedef struct hak_semaphore_t hak_semaphore_t;
typedef struct hak_semaphore_t* hak_oop_semaphore_t;

#define HAK_SEMAPHORE_GROUP_NAMED_INSTVARS (8)
typedef struct hak_semaphore_group_t hak_semaphore_group_t;
typedef struct hak_semaphore_group_t* hak_oop_semaphore_group_t;


#define HAK_PROCESS_STATE_RUNNING (3)
#define HAK_PROCESS_STATE_WAITING (2)
#define HAK_PROCESS_STATE_RUNNABLE (1)
#define HAK_PROCESS_STATE_SUSPENDED (0)
#define HAK_PROCESS_STATE_TERMINATED (-1)

struct hak_process_t
{
	HAK_OBJ_HEADER;
	hak_oop_context_t initial_context;
	hak_oop_context_t current_context;

	hak_oop_t         id; /* SmallInteger */
	hak_oop_t         state; /* SmallInteger */

	hak_oop_t         sp;   /* stack pointer. SmallInteger */
	hak_oop_t         st;   /* stack top */

	hak_oop_t         exsp; /* exception stack pointer. SmallInteger */
	hak_oop_t         exst; /* exception stack top */

	hak_oop_t         clsp; /* class stack pointer */
	hak_oop_t         clst; /* class stack  top */

	struct
	{
		hak_oop_process_t prev;
		hak_oop_process_t next;
	} ps;  /* links to use with the process scheduler */

	struct
	{
		hak_oop_process_t prev;
		hak_oop_process_t next;
	} sem_wait; /* links to use with a semaphore */

	hak_oop_t sem; /* nil, semaphore, or semaphore group */

	/* == variable indexed part == */
	hak_oop_t slot[1]; /* process stack */

	/* after the process stack comes the exception stack.
	 * the exception stack is composed of instruction pointers and some context values.
	 * the instruction pointers are OOPs of small integers. safe without GC.
	 * the context values must be referenced by the active call chain. GC doesn't need to scan this area.
	 * If this assumption is not correct, GC code must be modified.
	 * so the garbage collector is free to ignore the exception stack */
};

enum hak_semaphore_subtype_t
{
	HAK_SEMAPHORE_SUBTYPE_TIMED = 0,
	HAK_SEMAPHORE_SUBTYPE_IO    = 1
};
typedef enum hak_semaphore_subtype_t hak_semaphore_subtype_t;

enum hak_semaphore_io_type_t
{
	HAK_SEMAPHORE_IO_TYPE_INPUT   = 0,
	HAK_SEMAPHORE_IO_TYPE_OUTPUT  = 1
};
typedef enum hak_semaphore_io_type_t hak_semaphore_io_type_t;

enum hak_semaphore_io_mask_t
{
	HAK_SEMAPHORE_IO_MASK_INPUT   = (1 << 0),
	HAK_SEMAPHORE_IO_MASK_OUTPUT  = (1 << 1),
	HAK_SEMAPHORE_IO_MASK_HANGUP  = (1 << 2),
	HAK_SEMAPHORE_IO_MASK_ERROR   = (1 << 3)
};
typedef enum hak_semaphore_io_mask_t hak_semaphore_io_mask_t;

struct hak_semaphore_t
{
	HAK_OBJ_HEADER;

	/* [IMPORTANT] make sure that the position of 'waiting' in hak_semaphore_t
	 *             must be exactly the same as its position in hak_semaphore_group_t */
	struct
	{
		hak_oop_process_t first;
		hak_oop_process_t last;
	} waiting; /* list of processes waiting on this semaphore */
	/* [END IMPORTANT] */

	hak_oop_t count; /* SmallInteger */

	/* nil for normal. SmallInteger if associated with
	 * timer(HAK_SEMAPHORE_SUBTYPE_TIMED) or IO(HAK_SEMAPHORE_SUBTYPE_IO). */
	hak_oop_t subtype;

	union
	{
		struct
		{
			hak_oop_t index; /* index to the heap that stores timed semaphores */
			hak_oop_t ftime_sec; /* firing time */
			hak_oop_t ftime_nsec; /* firing time */
		} timed;

		struct
		{
			hak_oop_t index; /* index to sem_io_tuple */
			hak_oop_t handle;
			hak_oop_t type; /* SmallInteger */
		} io;
	} u;

	hak_oop_t signal_action;

	hak_oop_semaphore_group_t group; /* nil or belonging semaphore group */
	struct
	{
		hak_oop_semaphore_t prev;
		hak_oop_semaphore_t next;
	} grm; /* group membership chain */
};

#define HAK_SEMAPHORE_GROUP_SEMS_UNSIG 0
#define HAK_SEMAPHORE_GROUP_SEMS_SIG   1

struct hak_semaphore_group_t
{
	HAK_OBJ_HEADER;

	/* [IMPORTANT] make sure that the position of 'waiting' in hak_semaphore_group_t
	 *             must be exactly the same as its position in hak_semaphore_t */
	struct
	{
		hak_oop_process_t first;
		hak_oop_process_t last;
	} waiting; /* list of processes waiting on this semaphore group */
	/* [END IMPORTANT] */

	struct
	{
		hak_oop_semaphore_t first;
		hak_oop_semaphore_t last;
	} sems[2]; /* sems[0] - unsignaled semaphores, sems[1] - signaled semaphores */

	hak_oop_t sem_io_count; /* the number of io semaphores in the group */
	hak_oop_t sem_count; /* the total number of semaphores in the group */
};

#define HAK_PROCESS_SCHEDULER_NAMED_INSTVARS 8
typedef struct hak_process_scheduler_t hak_process_scheduler_t;
typedef struct hak_process_scheduler_t* hak_oop_process_scheduler_t;
struct hak_process_scheduler_t
{
	HAK_OBJ_HEADER;

	hak_oop_process_t active; /*  pointer to an active process in the runnable process list */
	hak_oop_t total_count;  /* smooi, total number of processes - runnable/running/suspended */

	struct
	{
		hak_oop_t         count; /* smooi, the number of runnable/running processes */
		hak_oop_process_t first; /* runnable process list */
		hak_oop_process_t last; /* runnable process list */
	} runnable;

	struct
	{
		hak_oop_t         count; /* smooi, the number of suspended processes */
		hak_oop_process_t first; /* suspended process list */
		hak_oop_process_t last; /* suspended process list */
	} suspended;
};


#define HAK_CLASS_NAMED_INSTVARS 9
typedef struct hak_class_t hak_class_t;
typedef struct hak_class_t* hak_oop_class_t;
struct hak_class_t
{
	HAK_OBJ_HEADER;

	hak_oop_t name; /* class name. nil for unnamed class */
	hak_oop_t mdic; /* method dictionary. nil or a dictionary object */

	hak_oop_t spec;     /* SmallInteger. instance specification */
	hak_oop_t selfspec; /* SmallInteger. specification of the class object itself */

	hak_oop_t superclass;
	hak_oop_t nivars_super; /* SmallInteger */
	hak_oop_t ibrand; /* SmallInteger */

	hak_oop_char_t ivarnames;
	hak_oop_char_t cvarnames;

	/* indexed part afterwards - not included in HAK_CLASS_NAMED_INSTVARS */
	hak_oop_t      cvar[1];   /* class variables. */
};

#if 0
struct hak_class_t
{
	HAK_OBJ_HEADER;

	/* === the following five fields must be in sync with hak_methowner_t === */
	hak_oop_char_t name; /* Symbol */

	/* [0] - instance methods, MethodDictionary
	 * [1] - class methods, MethodDictionary */
	hak_oop_dic_t  mthdic[2];

	hak_oop_nsdic_t nsup; /* pointer to the upper namespace */
	hak_oop_nsdic_t nsdic; /* dictionary used for namespacing - may be nil when there are no subitems underneath */
	/* ===================================================================== */

	hak_oop_t      spec;          /* SmallInteger. instance specification */
	hak_oop_t      selfspec;      /* SmallInteger. specification of the class object itself */

	hak_oop_t      superclass;    /* Another class */
	hak_oop_t      subclasses;    /* Array of subclasses */

	hak_oop_t      modname;       /* Symbol if importing a module. nil if not. */

	/* == NEVER CHANGE THIS ORDER OF 3 ITEMS BELOW == */
	hak_oop_char_t ivars;  /* String */
	hak_oop_char_t civars; /* String */
	hak_oop_char_t cvars;  /* String */
	/* == NEVER CHANGE THE ORDER OF 3 ITEMS ABOVE == */

#if 0
	hak_oop_char_t pooldics;      /* String - pool dictionaries imported */

	hak_oop_t      trsize; /* trailer size for new instances */
	hak_oop_t      trgc; /* trailer gc callback */

	/* [0] - initial values for instance variables of new instances
	 * [1] - initial values for class instance variables */
	hak_oop_t      initv[2];
#endif
	/* indexed part afterwards */
	hak_oop_t      cvar[1];   /* class instance variables and class variables. */
};
#endif

/**
 * The HAK_CLASSOF() macro return the class of an object including a numeric
 * object encoded into a pointer.
 */
#define HAK_CLASSOF(hak,oop) \
	(HAK_OOP_GET_TAG(oop)? ((hak_oop_t)(*(hak)->tagged_classes[HAK_OOP_GET_TAG(oop)])): HAK_OBJ_GET_CLASS(oop))
/**/


/**
 * The HAK_BYTESOF() macro returns the size of the payload of
 * an object in bytes. If the pointer given encodes a numeric value,
 * it returns the size of #hak_oow_t in bytes.
 */
#define HAK_BYTESOF(hak,oop) \
	(HAK_OOP_IS_NUMERIC(oop)? HAK_SIZEOF(hak_oow_t): HAK_OBJ_BYTESOF(oop))
/**/


/**
 * The HAK_ISTYPEOF() macro is a safe replacement for HAK_OBJ_GET_FLAGS_TYPE()
 */
#define HAK_ISTYPEOF(hak,oop,type) \
	(!HAK_OOP_IS_NUMERIC(oop) && HAK_OBJ_GET_FLAGS_TYPE(oop) == (type))
/**/

/* =========================================================================
 * HEAP
 * ========================================================================= */

typedef struct hak_heap_t hak_heap_t;

struct hak_heap_t
{
	hak_uint8_t* base;  /* start of a heap */
	hak_oow_t    size;
	hak_xma_t*   xma;
	hak_mmgr_t   xmmgr;
};

/* =========================================================================
 * VM LOGGING
 * ========================================================================= */

enum hak_log_mask_t
{
	HAK_LOG_DEBUG       = ((hak_bitmask_t)1 << 0),
	HAK_LOG_INFO        = ((hak_bitmask_t)1 << 1),
	HAK_LOG_WARN        = ((hak_bitmask_t)1 << 2),
	HAK_LOG_ERROR       = ((hak_bitmask_t)1 << 3),
	HAK_LOG_FATAL       = ((hak_bitmask_t)1 << 4),

	HAK_LOG_UNTYPED     = ((hak_bitmask_t)1 << 6), /* only to be used by HAK_DEBUGx() and HAK_INFOx() */
	HAK_LOG_COMPILER    = ((hak_bitmask_t)1 << 7),
	HAK_LOG_VM          = ((hak_bitmask_t)1 << 8),
	HAK_LOG_MNEMONIC    = ((hak_bitmask_t)1 << 9), /* bytecode mnemonic */
	HAK_LOG_GC          = ((hak_bitmask_t)1 << 10),
	HAK_LOG_IC          = ((hak_bitmask_t)1 << 11), /* instruction cycle, fetch-decode-execute */
	HAK_LOG_PRIMITIVE   = ((hak_bitmask_t)1 << 12),

	HAK_LOG_APP         = ((hak_bitmask_t)1 << 13), /* hak applications, set by hak logging primitive */
	HAK_LOG_APP_X1      = ((hak_bitmask_t)1 << 14), /* more hak applications, you can choose to use one of APP_X? randomly */
	HAK_LOG_APP_X2      = ((hak_bitmask_t)1 << 15),
	HAK_LOG_APP_X3      = ((hak_bitmask_t)1 << 16),

	HAK_LOG_ALL_LEVELS  = (HAK_LOG_DEBUG  | HAK_LOG_INFO | HAK_LOG_WARN | HAK_LOG_ERROR | HAK_LOG_FATAL),
	HAK_LOG_ALL_TYPES   = (HAK_LOG_UNTYPED | HAK_LOG_COMPILER | HAK_LOG_VM | HAK_LOG_MNEMONIC | HAK_LOG_GC | HAK_LOG_IC | HAK_LOG_PRIMITIVE | HAK_LOG_APP | HAK_LOG_APP_X1 | HAK_LOG_APP_X2 | HAK_LOG_APP_X3),


	HAK_LOG_STDOUT      = ((hak_bitmask_t)1 << 20),  /* write log messages to stdout without timestamp. HAK_LOG_STDOUT wins over HAK_LOG_STDERR. */
	HAK_LOG_STDERR      = ((hak_bitmask_t)1 << 21),  /* write log messages to stderr without timestamp. */

	HAK_LOG_PREFER_JSON = ((hak_bitmask_t)1 << 30)   /* write a object in the json format. don't set this explicitly. use %J instead */
};
typedef enum hak_log_mask_t hak_log_mask_t;

/* all bits must be set to get enabled */
#define HAK_LOG_ENABLED(hak,mask) (((hak)->option.log_mask & (mask)) == (mask))

#define HAK_LOG0(hak,mask,fmt) do { if (HAK_LOG_ENABLED(hak,mask)) hak_logbfmt(hak, mask, fmt); } while(0)
#define HAK_LOG1(hak,mask,fmt,a1) do { if (HAK_LOG_ENABLED(hak,mask)) hak_logbfmt(hak, mask, fmt, a1); } while(0)
#define HAK_LOG2(hak,mask,fmt,a1,a2) do { if (HAK_LOG_ENABLED(hak,mask)) hak_logbfmt(hak, mask, fmt, a1, a2); } while(0)
#define HAK_LOG3(hak,mask,fmt,a1,a2,a3) do { if (HAK_LOG_ENABLED(hak,mask)) hak_logbfmt(hak, mask, fmt, a1, a2, a3); } while(0)
#define HAK_LOG4(hak,mask,fmt,a1,a2,a3,a4) do { if (HAK_LOG_ENABLED(hak,mask)) hak_logbfmt(hak, mask, fmt, a1, a2, a3, a4); } while(0)
#define HAK_LOG5(hak,mask,fmt,a1,a2,a3,a4,a5) do { if (HAK_LOG_ENABLED(hak,mask)) hak_logbfmt(hak, mask, fmt, a1, a2, a3, a4, a5); } while(0)
#define HAK_LOG6(hak,mask,fmt,a1,a2,a3,a4,a5,a6) do { if (HAK_LOG_ENABLED(hak,mask)) hak_logbfmt(hak, mask, fmt, a1, a2, a3, a4, a5, a6); } while(0)
#define HAK_LOG7(hak,mask,fmt,a1,a2,a3,a4,a5,a6,a7) do { if (HAK_LOG_ENABLED(hak,mask)) hak_logbfmt(hak, mask, fmt, a1, a2, a3, a4, a5, a6, a7); } while(0)
#define HAK_LOG8(hak,mask,fmt,a1,a2,a3,a4,a5,a6,a7,a8) do { if (HAK_LOG_ENABLED(hak,mask)) hak_logbfmt(hak, mask, fmt, a1, a2, a3, a4, a5, a6, a7, a8); } while(0)
#define HAK_LOG9(hak,mask,fmt,a1,a2,a3,a4,a5,a6,a7,a8,a9) do { if (HAK_LOG_ENABLED(hak,mask)) hak_logbfmt(hak, mask, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9); } while(0)

#if defined(HAK_BUILD_RELEASE)
	/* [NOTE]
	 *  get rid of debugging message totally regardless of
	 *  the log mask in the release build.
	 */
#	define HAK_DEBUG0(hak,fmt)
#	define HAK_DEBUG1(hak,fmt,a1)
#	define HAK_DEBUG2(hak,fmt,a1,a2)
#	define HAK_DEBUG3(hak,fmt,a1,a2,a3)
#	define HAK_DEBUG4(hak,fmt,a1,a2,a3,a4)
#	define HAK_DEBUG5(hak,fmt,a1,a2,a3,a4,a5)
#	define HAK_DEBUG6(hak,fmt,a1,a2,a3,a4,a5,a6)
#	define HAK_DEBUG7(hak,fmt,a1,a2,a3,a4,a5,a6,a7)
#	define HAK_DEBUG8(hak,fmt,a1,a2,a3,a4,a5,a6,a7,a8)
#	define HAK_DEBUG9(hak,fmt,a1,a2,a3,a4,a5,a6,a7,a8,a9)
#else
#	define HAK_DEBUG0(hak,fmt) HAK_LOG0(hak, HAK_LOG_DEBUG | HAK_LOG_UNTYPED, fmt)
#	define HAK_DEBUG1(hak,fmt,a1) HAK_LOG1(hak, HAK_LOG_DEBUG | HAK_LOG_UNTYPED, fmt, a1)
#	define HAK_DEBUG2(hak,fmt,a1,a2) HAK_LOG2(hak, HAK_LOG_DEBUG | HAK_LOG_UNTYPED, fmt, a1, a2)
#	define HAK_DEBUG3(hak,fmt,a1,a2,a3) HAK_LOG3(hak, HAK_LOG_DEBUG | HAK_LOG_UNTYPED, fmt, a1, a2, a3)
#	define HAK_DEBUG4(hak,fmt,a1,a2,a3,a4) HAK_LOG4(hak, HAK_LOG_DEBUG | HAK_LOG_UNTYPED, fmt, a1, a2, a3, a4)
#	define HAK_DEBUG5(hak,fmt,a1,a2,a3,a4,a5) HAK_LOG5(hak, HAK_LOG_DEBUG | HAK_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5)
#	define HAK_DEBUG6(hak,fmt,a1,a2,a3,a4,a5,a6) HAK_LOG6(hak, HAK_LOG_DEBUG | HAK_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6)
#	define HAK_DEBUG7(hak,fmt,a1,a2,a3,a4,a5,a6,a7) HAK_LOG7(hak, HAK_LOG_DEBUG | HAK_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6, a7)
#	define HAK_DEBUG8(hak,fmt,a1,a2,a3,a4,a5,a6,a7,a8) HAK_LOG8(hak, HAK_LOG_DEBUG | HAK_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6, a7, a8)
#	define HAK_DEBUG9(hak,fmt,a1,a2,a3,a4,a5,a6,a7,a8,a9) HAK_LOG9(hak, HAK_LOG_DEBUG | HAK_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
#endif

#define HAK_INFO0(hak,fmt) HAK_LOG0(hak, HAK_LOG_INFO | HAK_LOG_UNTYPED, fmt)
#define HAK_INFO1(hak,fmt,a1) HAK_LOG1(hak, HAK_LOG_INFO | HAK_LOG_UNTYPED, fmt, a1)
#define HAK_INFO2(hak,fmt,a1,a2) HAK_LOG2(hak, HAK_LOG_INFO | HAK_LOG_UNTYPED, fmt, a1, a2)
#define HAK_INFO3(hak,fmt,a1,a2,a3) HAK_LOG3(hak, HAK_LOG_INFO | HAK_LOG_UNTYPED, fmt, a1, a2, a3)
#define HAK_INFO4(hak,fmt,a1,a2,a3,a4) HAK_LOG4(hak, HAK_LOG_INFO | HAK_LOG_UNTYPED, fmt, a1, a2, a3, a4)
#define HAK_INFO5(hak,fmt,a1,a2,a3,a4,a5) HAK_LOG5(hak, HAK_LOG_INFO | HAK_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5)
#define HAK_INFO6(hak,fmt,a1,a2,a3,a4,a5,a6) HAK_LOG6(hak, HAK_LOG_INFO | HAK_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6)
#define HAK_INFO7(hak,fmt,a1,a2,a3,a4,a5,a6,a7) HAK_LOG7(hak, HAK_LOG_INFO | HAK_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6, a7)
#define HAK_INFO8(hak,fmt,a1,a2,a3,a4,a5,a6,a7,a8) HAK_LOG8(hak, HAK_LOG_INFO | HAK_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6, a7, a8)
#define HAK_INFO9(hak,fmt,a1,a2,a3,a4,a5,a6,a7,a8,a9) HAK_LOG9(hak, HAK_LOG_INFO | HAK_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)


/* =========================================================================
 * VIRTUAL MACHINE PRIMITIVES
 * ========================================================================= */

typedef void* (*hak_alloc_heap_t) (
	hak_t*             hak,
	hak_oow_t*         size /* [IN] requested size, [OUT] allocated size */
);

typedef void (*hak_free_heap_t) (
	hak_t*             hak,
	void*              ptr
);

typedef void (*hak_log_write_t) (
	hak_t*             hak,
	hak_bitmask_t    mask,
	const hak_ooch_t*  msg,
	hak_oow_t          len
);

typedef hak_errnum_t (*hak_syserrstrb_t) (
	hak_t*             hak,
	int                syserr_type,
	int                syserr_code,
	hak_bch_t*         buf,
	hak_oow_t          len
);

typedef hak_errnum_t (*hak_syserrstru_t) (
	hak_t*             hak,
	int                syserr_type,
	int                syserr_code,
	hak_uch_t*         buf,
	hak_oow_t          len
);

typedef void (*hak_assertfail_t) (
	hak_t*             hak,
	const hak_bch_t*   expr,
	const hak_bch_t*   file,
	hak_oow_t          line
);

enum hak_vmprim_dlopen_flag_t
{
	HAK_VMPRIM_DLOPEN_PFMOD = (1 << 0)
};
typedef enum hak_vmprim_dlopen_flag_t hak_vmprim_dlopen_flag_t;

typedef void (*hak_vmprim_dlstartup_t) (
	hak_t*             hak
);

typedef void (*hak_vmprim_dlcleanup_t) (
	hak_t*             hak
);

typedef void* (*hak_vmprim_dlopen_t) (
	hak_t*             hak,
	const hak_ooch_t*  name,
	int                flags
);

typedef void (*hak_vmprim_dlclose_t) (
	hak_t*             hak,
	void*              handle
);

typedef void* (*hak_vmprim_dlgetsym_t) (
	hak_t*             hak,
	void*              handle,
	const hak_ooch_t*  name
);

typedef void (*hak_vmprim_gettime_t) (
	hak_t*             hak,
	hak_ntime_t*       now
);

typedef int (*hak_vmprim_muxadd_t) (
	hak_t*                  hak,
	hak_ooi_t               io_handle,
	hak_ooi_t               masks
);

typedef int (*hak_vmprim_muxmod_t) (
	hak_t*                  hak,
	hak_ooi_t               io_handle,
	hak_ooi_t               masks
);

typedef int (*hak_vmprim_muxdel_t) (
	hak_t*                  hak,
	hak_ooi_t               io_handle
);

typedef void (*hak_vmprim_muxwait_cb_t) (
	hak_t*                  hak,
	hak_ooi_t               io_handle,
	hak_ooi_t               masks
);

typedef void (*hak_vmprim_muxwait_t) (
	hak_t*                  hak,
	const hak_ntime_t*      duration,
	hak_vmprim_muxwait_cb_t muxwcb
);

typedef int (*hak_vmprim_sleep_t) (
	hak_t*             hak,
	const hak_ntime_t* duration
);

typedef hak_ooi_t (*hak_vmprim_getsigfd_t) (
	hak_t*             hak
);

typedef int (*hak_vmprim_getsig_t) (
	hak_t*             hak,
	hak_uint8_t*       sig
);

typedef int (*hak_vmprim_setsig_t) (
	hak_t*             hak,
	hak_uint8_t        sig
);

struct hak_vmprim_t
{
	/* The alloc_heap callback function is called very earlier
	 * before hak is fully initialized. so few features are availble
	 * in this callback function. If it's not provided, the default
	 * implementation is used. */
	hak_alloc_heap_t       alloc_heap; /* optional */

	/* If you customize the heap allocator by providing the alloc_heap
	 * callback, you should implement the heap freer. otherwise the default
	 * implementation doesn't know how to free the heap allocated by
	 * the allocator callback. */
	hak_free_heap_t        free_heap; /* optional */

	hak_log_write_t        log_write; /* required */
	hak_syserrstrb_t       syserrstrb; /* one of syserrstrb or syserrstru required */
	hak_syserrstru_t       syserrstru;
	hak_assertfail_t       assertfail;

	hak_vmprim_dlstartup_t dl_startup; /* optional */
	hak_vmprim_dlcleanup_t dl_cleanup; /* optional */
	hak_vmprim_dlopen_t    dl_open; /* required */
	hak_vmprim_dlclose_t   dl_close; /* required */
	hak_vmprim_dlgetsym_t  dl_getsym; /* requried */

	hak_vmprim_gettime_t   vm_gettime; /* required */
	hak_vmprim_muxadd_t    vm_muxadd;
	hak_vmprim_muxdel_t    vm_muxdel;
	hak_vmprim_muxmod_t    vm_muxmod;
	hak_vmprim_muxwait_t   vm_muxwait;
	hak_vmprim_sleep_t     vm_sleep; /* required */

	hak_vmprim_getsigfd_t  vm_getsigfd;
	hak_vmprim_getsig_t    vm_getsig;
	hak_vmprim_setsig_t    vm_setsig;
};

typedef struct hak_vmprim_t hak_vmprim_t;

/* =========================================================================
 * IO MANIPULATION
 * ========================================================================= */

enum hak_io_cmd_t
{
	HAK_IO_OPEN,
	HAK_IO_CLOSE,
	HAK_IO_READ,
	HAK_IO_READ_BYTES,
	HAK_IO_WRITE,
	HAK_IO_WRITE_BYTES,
	HAK_IO_FLUSH
};
typedef enum hak_io_cmd_t hak_io_cmd_t;

struct hak_lxc_t
{
	hak_ooci_t   c; /**< character */
	hak_loc_t    l; /**< location */
};
typedef struct hak_lxc_t hak_lxc_t;

#if !defined(HAK_CCI_BUF_LEN)
#define HAK_CCI_BUF_LEN (2048)
#endif

/**
 * The hak_io_cciarg_t defines the input stream to the compiler.
 */
typedef struct hak_io_cciarg_t hak_io_cciarg_t;
struct hak_io_cciarg_t
{
	/**
	 * [IN] I/O object name.
	 * It is #HAK_NULL for the main stream and points to a non-NULL string
	 * for an included stream.
	 */
	const hak_ooch_t* name;

	/**
	 * [OUT] I/O handle set by a handler.
	 * The source stream handler can set this field when it opens a stream.
	 * All subsequent operations on the stream see this field as set
	 * during opening.
	 */
	void* handle;

	/**
	 * [OUT] set this to non-zero in HAK_IO_OPEN if the handler fills the buffer with bytes.
	 *       the caller issues HAK_IO_READ_BYTES if it's set to non-zero, expecting bytes.
	 *       otherwise it issues HAK_IO_READ expecting characters.
	 */
	int byte_oriented;

	/**
	 * [OUT] place data here for #HAK_IO_READ or #HAK_IO_READ_BYTES
	 */
	union
	{
		hak_ooch_t c[HAK_CCI_BUF_LEN];
		hak_uint8_t b[HAK_CCI_BUF_LEN * HAK_SIZEOF(hak_ooch_t)];
	} buf;

	/**
	 * [OUT] place the number of characters read here for #HAK_IO_READ
	 */
	hak_oow_t  xlen;

	/**
	 * [IN] points to the data of the includer. It is #HAK_NULL for the
	 * main stream.
	 */
	hak_io_cciarg_t* includer;

	/*-----------------------------------------------------------------*/
	/*----------- from here down, internal use only -------------------*/
	struct
	{
		hak_oow_t pos;
		hak_oow_t len;
	} b; /* buffer(buf.c or buf.b) usage status */

	struct
	{
		hak_uint8_t buf[HAK_MBLEN_MAX];
		hak_oow_t len;
	} rsd; /* residue bytes for HAK_IO_READ_BYTES */

	hak_oow_t line;
	hak_oow_t colm;
	hak_ooci_t nl;

	hak_lxc_t lxc;
	/*-----------------------------------------------------------------*/
};
/**/

/**
 * The hak_io_udiarg_t defines the user-defined input stream handler.
 */
typedef struct hak_io_udiarg_t hak_io_udiarg_t;
struct hak_io_udiarg_t
{
	/**
	 * [OUT] I/O handle set by a handler.
	 * The stream handler can set this field when it opens a stream.
	 * All subsequent operations on the stream see this field as set
	 * during opening.
	 */
	void* handle;

	/**
	 * [OUT] indicates if HAK_IO_READ_BYTES is implemented
	 */
	int byte_oriented;

	/**
	 * [OUT] place data in c for #HAK_IO_READ and in d for #HAK_IO_READ_BYTES
	 */
	union {
		hak_ooch_t  c[2048]; /* TODO: resize this if necessary */
		hak_uint8_t b[2048 * HAK_SIZEOF(hak_ooch_t)]; /* TODO: resize this if necessary */
	} buf;

	/**
	 * [OUT] place the number of characters read here for
	 * #HAK_IO_READ or #HAK_IO_READ_BYTES
	 */
	hak_oow_t xlen;


	/*-----------------------------------------------------------------*/
	/*----------- from here down, internal use only -------------------*/
	struct
	{
		hak_oow_t pos;
		hak_oow_t len;
	} b; /* buffer(buf.c or buf.b) usage status */

	struct
	{
		hak_uint8_t buf[HAK_MBLEN_MAX];
		hak_oow_t len;
	} rsd; /* residue bytes for HAK_IO_READ_BYTES */

	int eof_reached;
};
/**/

/**
 * The hak_io_udiarg_t defines the user-defined output stream handler.
 */
typedef struct hak_io_udoarg_t hak_io_udoarg_t;
struct hak_io_udoarg_t
{
	/**
	 * [OUT] I/O handle set by a handler.
	 * The stream handler can set this field when it opens a stream.
	 * All subsequent operations on the stream see this field as set
	 * during opening.
	 */
	void* handle;

	/**
	 * [IN] the pointer to the beginning of the character/byte string
	 *      to write.
	 *      hak_ooch_t* for HAK_IO_WRITE
	 *      hak_bch_t* or hak_uint8_t* for HAK_IO_WRITE_BYTES
	 */
	void* ptr;

	/**
	 * [IN] total number of characters/bytes to write
	 */
	hak_oow_t len;

	/**
	 * [OUT] place the number of characters/bytes written here for
	 *       #HAK_IO_WRITE or #HAK_IO_WRITE_BYTES
	 */
	hak_oow_t xlen;
};
/**/

/**
 * The hak_io_impl_t type defines a callback function prototype
 * for I/O operations.
 */
typedef int (*hak_io_impl_t) (
	hak_t*        hak,
	hak_io_cmd_t  cmd,
	void*         arg /* one of hak_io_cciarg_t*, hak_io_udiarg_t*, hak_io_udoarg_t* */
);
/**/

/* =========================================================================
 * CALLBACK MANIPULATION
 * ========================================================================= */


typedef void (*hak_cb_on_fini_t)    (hak_t* hak);
typedef void (*hak_cb_on_halting_t) (hak_t* hak);
typedef void (*hak_cb_on_option_t)  (hak_t* hak, hak_option_t id, const void* val);
typedef void (*hak_cb_on_gc_t)      (hak_t* hak);
typedef int  (*hak_cb_vm_startup_t) (hak_t* hak);
typedef void (*hak_cb_vm_cleanup_t) (hak_t* hak);
typedef void (*hak_cb_vm_checkbc_t) (hak_t* hak, hak_oob_t bcode);

typedef struct hak_cb_t hak_cb_t;
struct hak_cb_t
{
	hak_cb_on_fini_t    on_fini; /* called from hak_fini() */
	hak_cb_on_halting_t halting;
	hak_cb_on_option_t  on_option; /* called from hak_setoption() */
	hak_cb_on_gc_t      on_gc; /* called from hak_gc() */

	hak_cb_vm_startup_t vm_startup;
	hak_cb_vm_cleanup_t vm_cleanup;
	hak_cb_vm_checkbc_t vm_checkbc;

	/* private below */
	hak_cb_t*     prev;
	hak_cb_t*     next;
};


/* =========================================================================
 * PRIMITIVE FUNCTIONS
 * ========================================================================= */
enum hak_pfrc_t
{
	HAK_PF_FAILURE = -1,
	HAK_PF_SUCCESS = 0
};
typedef enum hak_pfrc_t hak_pfrc_t;

typedef hak_pfrc_t (*hak_pfimpl_t) (
	hak_t*     hak,
	hak_mod_t* mod,
	hak_ooi_t  nargs
);

enum hak_pfbase_type_t
{
	HAK_PFBASE_FUNC  = 0,
	HAK_PFBASE_VAR   = 1,
	HAK_PFBASE_CONST = 2
};
typedef enum hak_pfbase_type_t hak_pfbase_type_t;

typedef struct hak_pfbase_t hak_pfbase_t;
struct hak_pfbase_t
{
	hak_pfbase_type_t type;
	hak_pfimpl_t      handler;
	hak_oow_t         minargs;
	hak_oow_t         maxargs;
};

typedef struct hak_pfinfo_t hak_pfinfo_t;
struct hak_pfinfo_t
{
	const hak_bch_t*  mthname;
	hak_pfbase_t      base;
};
/* =========================================================================
 * PRIMITIVE MODULE MANIPULATION
 * ========================================================================= */
#define HAK_MOD_NAME_LEN_MAX 120

typedef int (*hak_mod_load_t) (
	hak_t*     hak,
	hak_mod_t* mod
);

typedef hak_pfbase_t* (*hak_mod_query_t) (
	hak_t*            hak,
	hak_mod_t*        mod,
	const hak_ooch_t* name,
	hak_oow_t         namelen
);

typedef void (*hak_mod_unload_t) (
	hak_t*     hak,
	hak_mod_t* mod
);

typedef void (*hak_mod_gc_t) (
	hak_t*     hak,
	hak_mod_t* mod
);

struct hak_mod_t
{
	/* input */
	hak_ooch_t       name[HAK_MOD_NAME_LEN_MAX + 1];
	void*            inctx;

	/* user-defined data - the module intializer shoudl fill in the following fields. */
	hak_mod_query_t  query;
	hak_mod_unload_t unload;
	hak_mod_gc_t     gc;
	void*            ctx;
};

struct hak_mod_data_t
{
	void*           handle;
	hak_rbt_pair_t* pair; /* internal backreference to hak->modtab */
	hak_mod_t       mod;
};
typedef struct hak_mod_data_t hak_mod_data_t;


struct hak_sem_tuple_t
{
	hak_oop_semaphore_t sem[2]; /* [0] input, [1] output */
	hak_ooi_t handle; /* io handle */
	hak_ooi_t mask;
};
typedef struct hak_sem_tuple_t hak_sem_tuple_t;

/* =========================================================================
 * HAK VM
 * ========================================================================= */
typedef struct hak_synerr_t hak_synerr_t;
struct hak_synerr_t
{
	hak_synerrnum_t num;
	hak_loc_t       loc;
	struct
	{
		hak_ooch_t val[256];
		hak_oow_t  len;
	} tgt;
};

typedef struct hak_dbgi_t hak_dbgi_t;
struct hak_dbgi_t
{
	const hak_ooch_t* fname; /* file name */
	hak_oow_t sline; /* source line in the file */
};

#if defined(HAK_INCLUDE_COMPILER)
typedef struct hak_compiler_t hak_compiler_t;
typedef struct hak_cnode_t hak_cnode_t;

typedef int (*hak_on_cnode_t) (hak_t* hak, hak_cnode_t* obj);

enum hak_compile_flag_t
{
	/* clear byte codes at the beginnign of hak_compile() */
	HAK_COMPILE_CLEAR_CODE  = (1 << 0),

	/* clear the top-level function block at the end of hak_compile() */
	HAK_COMPILE_CLEAR_FUNBLK = (1 << 1)
};
typedef enum hak_compile_flag_t hak_compile_flag_t;
#endif

#define HAK_ERRMSG_CAPA (2048)

struct hak_code_t
{
	struct
	{
		hak_oob_t* ptr; /* byte code array */
		hak_oow_t len;
		hak_oow_t capa;
	} bc;

	struct
	{
		hak_oop_oop_t arr; /* literal array - not part of object memory */
		hak_oow_t len;
	} lit;

	/* the cumulative number of temporaries collected at the global(top-level) level */
	hak_oow_t ngtmprs;

	/* array that holds the location of the byte code emitted */
	hak_dbgi_t* dbgi;
};
typedef struct hak_code_t hak_code_t;

struct hak_t
{
	hak_oow_t    _instsize;
	hak_mmgr_t*  _mmgr;
	hak_cmgr_t*  _cmgr;

	hak_errnum_t errnum;
	struct
	{
		union
		{
			hak_ooch_t ooch[HAK_ERRMSG_CAPA];
			hak_bch_t bch[HAK_ERRMSG_CAPA];
			hak_uch_t uch[HAK_ERRMSG_CAPA];
		} tmpbuf;
	#if defined(HAK_OOCH_IS_UCH)
		hak_bch_t  xerrmsg[HAK_ERRMSG_CAPA * 2];
		hak_bch_t  xerrlocfile[256 * 2];
	#else
		hak_uch_t  xerrmsg[HAK_ERRMSG_CAPA];
		hak_uch_t  xerrlocfile[256];
	#endif
		hak_ooch_t buf[HAK_ERRMSG_CAPA];
		hak_oow_t len;
	} errmsg;
	hak_loc_t errloc;
	int shuterr;

	struct
	{
		hak_bitmask_t trait;
		hak_bitmask_t log_mask;
		hak_oow_t log_maxcapa;
		hak_bch_t* log_target_b;
		hak_uch_t* log_target_u;
		hak_oow_t dfl_symtab_size;
		hak_oow_t dfl_sysdic_size;
		hak_oow_t dfl_procstk_size;
		void* mod_inctx;

		hak_oocs_t mod[3];

	#if defined(HAK_BUILD_DEBUG)
		/* set automatically when trait is set */
		hak_oow_t karatsuba_cutoff;
	#endif
	} option;

	hak_vmprim_t vmprim;

	hak_oow_t vm_checkbc_cb_count;
	hak_cb_t* cblist;
	hak_rbt_t modtab; /* primitive module table */

	struct
	{
		hak_ooch_t* ptr;
		hak_oow_t len;
		hak_oow_t capa;
		hak_bitmask_t last_mask;
		hak_bitmask_t default_type_mask;
	} log;
	/* ========================= */

	hak_heap_t* heap;

	/* ========================= */
	hak_oop_t _undef; /* special internal value for uninitialized global variables */
	hak_oop_t _nil;  /* pointer to the nil object */
	hak_oop_t _true;
	hak_oop_t _false;

	hak_oop_dic_t symtab; /* system-wide symbol table. */
	hak_oop_dic_t sysdic; /* system dictionary. */
	hak_oop_process_scheduler_t processor; /* instance of ProcessScheduler */
	hak_oop_process_t nil_process; /* instance of Process */

	/* =============================================================
	 * KERNEL CLASSES
	 *  Be sure to Keep these kernel class pointers registered in the
	 *  kernel_classes table in gc.c
	 * ============================================================= */
	hak_oop_class_t c_apex; /* Apex */
	hak_oop_class_t c_class; /* Class */
	hak_oop_class_t c_undefobj; /* UndefinedObject */
	hak_oop_class_t c_nilobj; /* NilObject */
#if 0
	hak_oop_class_t c_interface; /* Interface */
#endif
	hak_oop_class_t c_object; /* Object */

	hak_oop_class_t c_collection; /* Collection */
	hak_oop_class_t c_indexed_collection; /* IndexedCollection */
	hak_oop_class_t c_fixed_sized_collection; /* FixedSizedCollection */
	hak_oop_class_t c_string; /* String */
	hak_oop_class_t c_byte_string; /* String */
	hak_oop_class_t c_symbol; /* Symbol */
	hak_oop_class_t c_array; /* Array */
	hak_oop_class_t c_character_array; /* CharacterArray */
	hak_oop_class_t c_byte_array; /* ByteArray */
	hak_oop_class_t c_symtab; /* SymbolTable */
	hak_oop_class_t c_dictionary;
	hak_oop_class_t c_cons; /* Cons */

#if 0
	hak_oop_class_t c_namespace; /* Namespace */
	hak_oop_class_t c_pool_dictionary; /* PoolDictionary */
#endif
	hak_oop_class_t c_method_dictionary; /* MethodDictionary */
#if 0
	hak_oop_class_t c_method; /* CompiledMethod */
	hak_oop_class_t c_methsig; /* MethodSignature */
#endif
	hak_oop_class_t c_function; /* Function */
	hak_oop_class_t c_primitive; /* Primitive */
	hak_oop_class_t c_compiled_block; /* CompiledBlock */

	hak_oop_class_t c_block_context; /* BlockContext */
	hak_oop_class_t c_process; /* Process */
	hak_oop_class_t c_semaphore; /* Semaphore */
	hak_oop_class_t c_semaphore_group; /* SemaphoreGroup */
	hak_oop_class_t c_process_scheduler; /* ProcessScheduler */

	hak_oop_class_t c_error; /* Error */
	hak_oop_class_t c_true; /* True */
	hak_oop_class_t c_false; /* False */
	hak_oop_class_t c_magnitude; /* Magnitude */
	hak_oop_class_t c_character; /* Character */
	hak_oop_class_t c_number; /* Number */
	hak_oop_class_t c_small_integer; /* SmallInteger */

	hak_oop_class_t c_large_positive_integer; /* LargePositiveInteger */
	hak_oop_class_t c_large_negative_integer; /* LargeNegativeInteger */
	hak_oop_class_t c_fixed_point_decimal; /* FixedPointDecimal */

	hak_oop_class_t c_small_pointer;
	hak_oop_class_t c_large_pointer;
	hak_oop_class_t c_system;

	/* ============================================================================= */

	/* pending asynchronous semaphores */
	hak_oop_semaphore_t* sem_list;
	hak_oow_t sem_list_count;
	hak_oow_t sem_list_capa;

	/* semaphores sorted according to time-out.
	 * organize entries using heap as the earliest entry
	 * needs to be checked first */
	hak_oop_semaphore_t* sem_heap;
	hak_oow_t sem_heap_count;
	hak_oow_t sem_heap_capa;

	/* semaphores for I/O handling. plain array */
	/*hak_oop_semaphore_t* sem_io;*/
	hak_sem_tuple_t* sem_io_tuple;
	hak_oow_t sem_io_tuple_count;
	hak_oow_t sem_io_tuple_capa;

	hak_oow_t sem_io_count;
	hak_oow_t sem_io_wait_count; /* number of processes waiting on an IO semaphore */

	hak_ooi_t* sem_io_map;
	hak_oow_t sem_io_map_capa;
	/* ============================================================================= */

	hak_oop_t* proc_map;
	hak_oow_t proc_map_capa;
	hak_oow_t proc_map_used;
	hak_ooi_t proc_map_free_first;
	hak_ooi_t proc_map_free_last;

	/* 2 tag bits(lo) + 2 extended tag bits(hi). not all slots are used
	 * because the 2 high extended bits are used only if the low tag bits
	 * are 3 */
	int tagged_brands[16];
	hak_oop_class_t* tagged_classes[16]; /* this is a pointer to hak_oop_class_t which is also a pointer */

	hak_oop_t* volat_stack[256]; /* stack for temporaries */
	hak_oow_t volat_count;

	/* == EXECUTION REGISTERS == */
	hak_oop_function_t initial_function;
	hak_oop_context_t initial_context; /* fake initial context */
	hak_oop_context_t active_context;
	hak_oop_function_t active_function;
	hak_oob_t* active_code;
	hak_ooi_t sp;
	hak_ooi_t ip;
	int no_proc_switch; /* process switching disabled */
	int proc_switched; /* TODO: this is temporary. implement something else to skip immediate context switching */
	int switch_proc;
	int abort_req;
	hak_ntime_t exec_start_time;
	hak_ntime_t exec_end_time;
	hak_oop_t last_retv;
	/* == END EXECUTION REGISTERS == */

	/* == BIGINT CONVERSION == */
	struct
	{
		int safe_ndigits;
		hak_oow_t multiplier;
	} bigint[37];

	struct
	{
		struct
		{
			hak_ooch_t* ptr;
			hak_oow_t capa;
			hak_oow_t len;
		} xbuf;
		struct
		{
			hak_liw_t* ptr;
			hak_oow_t capa;
		} t;
	} inttostr;
	/* == END BIGINT CONVERSION == */

	struct
	{
		struct
		{
			hak_ooch_t* ptr;
			hak_oow_t capa;
			hak_oow_t len;
		} xbuf; /* buffer to support sprintf */
	} sprintf;

	hak_code_t code;

	/* == PRINTER to udo stream == */
	struct
	{
		struct
		{
			void*        ptr;
			hak_oow_t    capa;
			hak_oow_t    size;
		} s;
		hak_oop_t e; /* top entry being printed */
	} p;
	/* == PRINTER to udo stream == */

	struct
	{
		hak_gchdr_t* b; /* object blocks allocated */
		struct
		{
			hak_gchdr_t* curr;
			hak_gchdr_t* prev;
		} ls;
		hak_oow_t bsz; /* total size of object blocks allocated */
		hak_oow_t threshold;
		int lazy_sweep;

		struct
		{
			hak_oop_t* ptr;
			hak_oow_t capa;
			hak_oow_t len;
			hak_oow_t max;
		} stack;

		struct
		{
			hak_ntime_t alloc;
			hak_ntime_t mark;
			hak_ntime_t sweep;
		} stat;
	} gci;

	struct
	{
		/* input handler */
		hak_io_impl_t udi_rdr;
		hak_io_udiarg_t udi_arg;

		/* output handler */
		hak_io_impl_t udo_wrtr;
		hak_io_udoarg_t udo_arg;
	} io;

#if defined(HAK_INCLUDE_COMPILER)
	hak_compiler_t* c;
#endif
};


/* TODO: stack bound check when pushing */
#define HAK_STACK_PUSH(hak,v) \
	do { \
		if (HAK_UNLIKELY((hak)->sp >= HAK_OOP_TO_SMOOI((hak)->processor->active->st))) \
		{ \
			hak_seterrbfmt (hak, HAK_EOOMEM, "process stack overflow"); \
			(hak)->abort_req = -1; \
		} \
		(hak)->sp = (hak)->sp + 1; \
		(hak)->processor->active->slot[(hak)->sp] = v; \
	} while (0)

#define HAK_STACK_GET(hak,sp_) ((hak)->processor->active->slot[sp_])
#define HAK_STACK_SET(hak,sp_,obj_) ((hak)->processor->active->slot[sp_] = obj_)

#define HAK_STACK_GETTOP(hak) HAK_STACK_GET(hak, (hak)->sp)
#define HAK_STACK_SETTOP(hak,obj_) HAK_STACK_SET(hak, (hak)->sp, obj_)

/* [NOTE]
 *  the following macros don't commit the active stack pointer(hak->sp)
 *  to hak->processor->active->sp immediately.
 */
#define HAK_STACK_POP(hak) ((hak)->sp = (hak)->sp - 1)
#define HAK_STACK_POPS(hak,count) ((hak)->sp = (hak)->sp - (count))
#define HAK_STACK_POP_TO(hak,v) \
	do { \
		v = HAK_STACK_GETTOP(hak); \
		HAK_STACK_POP (hak); \
	} while(0)

#define HAK_STACK_GET_ST(hak) HAK_OOP_TO_SMOOI((hak)->processor->active->st)
#define HAK_STACK_GET_SP(hak) ((hak)->sp)
#define HAK_STACK_IS_EMPTY(hak) ((hak)->sp <= -1)

/* get the stack pointer of the argument at the given index */
#define HAK_STACK_GETARGSP(hak,nargs,idx) ((hak)->sp - ((nargs) - (idx) - 1))
/* get the argument at the given index */
#define HAK_STACK_GETARG(hak,nargs,idx) HAK_STACK_GET(hak, (hak)->sp - ((nargs) - (idx) - 1))
/* get the receiver of a message */
#define HAK_STACK_GETRCV(hak,nargs) HAK_STACK_GET(hak, (hak)->sp - nargs - 1)
/* get the operator such as the called function/block/method */
#define HAK_STACK_GETOP(hak,nargs) HAK_STACK_GET(hak, (hak)->sp - nargs)

/* change the receiver of a message */
#define HAK_STACK_SETRCV(hak,nargs,newrcv) HAK_STACK_SET(hak, (hak)->sp - nargs - 1, newrcv)

/*
 * .....
 * argument 1
 * argument 0
 * operator
 * receiver
 */

/* you can't access arguments and receiver after this macro.
 * also you must not call this macro more than once */

#define HAK_STACK_SETRET(hak,nargs,retv) \
	do { \
		HAK_STACK_POPS(hak, nargs + 1); \
		HAK_STACK_SETTOP(hak, (retv)); \
	} while(0)

#define HAK_STACK_SETRETTOERRNUM(hak,nargs) HAK_STACK_SETRET(hak, nargs, HAK_ERROR_TO_OOP(hak->errnum))
#define HAK_STACK_SETRETTOERROR(hak,nargs,ec) HAK_STACK_SETRET(hak, nargs, HAK_ERROR_TO_OOP(ec))

/* =========================================================================
 * HAK ASSERTION
 * ========================================================================= */
#if defined(HAK_BUILD_RELEASE)
#	define HAK_ASSERT(hak,expr) ((void)0)
#else
#	define HAK_ASSERT(hak,expr) ((void)((expr) || ((hak)->vmprim.assertfail (hak, #expr, __FILE__, __LINE__), 0)))
#endif

/* =========================================================================
 * HAK COMMON OBJECTS
 * ========================================================================= */
enum hak_brand_t
{
	HAK_BRAND_SMOOI = 1, /* never used because a small integer is encoded in an object pointer */
	HAK_BRAND_SMPTR,
	HAK_BRAND_ERROR,
	HAK_BRAND_CHARACTER,

	HAK_BRAND_UNDEF,
	HAK_BRAND_NIL,
	HAK_BRAND_TRUE,
	HAK_BRAND_FALSE,

	HAK_BRAND_PBIGINT, /* positive big integer */
	HAK_BRAND_NBIGINT, /* negative big integer */
	HAK_BRAND_CONS,
	HAK_BRAND_ARRAY,
	HAK_BRAND_BYTE_ARRAY,
	HAK_BRAND_CHARACTER_ARRAY,
	HAK_BRAND_SYMBOL,
	HAK_BRAND_STRING,
	HAK_BRAND_BYTE_STRING,
	HAK_BRAND_DIC,
	HAK_BRAND_FPDEC, /* fixed-point decimal */

	HAK_BRAND_PRIM,

	HAK_BRAND_FUNCTION,
	HAK_BRAND_BLOCK,
	HAK_BRAND_CONTEXT,
	HAK_BRAND_PROCESS,
	HAK_BRAND_PROCESS_SCHEDULER,
	HAK_BRAND_SEMAPHORE,
	HAK_BRAND_SEMAPHORE_GROUP,
	HAK_BRAND_CLASS,
	HAK_BRAND_INSTANCE


	/* [NOTE] each enumerator must not exceed the maximum value that can be
	 *        represented with HAK_OBJ_FLAGS_BRAND_BITS bits */
};
typedef enum hak_brand_t hak_brand_t;

/* TODO: this concode stuff has become mostly useless as the bits are never set as of now */
enum hak_concode_t
{
	/* these can be set in the CONCODE flags for a cons cell */
	/* if you have more than 16 elements, increase HAK_OBJ_FLAGS_CONCODE_BITS */

	HAK_CONCODE_XLIST = 0,  /* ( ) - executable list */
	HAK_CONCODE_MLIST,      /* (obj:message) - message send list */
	HAK_CONCODE_ALIST,      /* (a := 20) assignment list */
	HAK_CONCODE_BLIST,      /* (10 + 20) expression with binary operator */
	HAK_CONCODE_BLOCK,      /* { } */
	HAK_CONCODE_ARRAY,      /* #[ ] */
	HAK_CONCODE_BYTEARRAY,  /* #b[ ] */
	HAK_CONCODE_CHARARRAY,  /* #c[ ] */
	HAK_CONCODE_DIC,        /* #{ } */
	HAK_CONCODE_QLIST,      /* #( ) - data list */
	HAK_CONCODE_TUPLE,      /* [ ] */
	HAK_CONCODE_VLIST       /* | | - symbol list */
};
typedef enum hak_concode_t hak_concode_t;

#define HAK_IS_UNDEF(hak,v) (v == (hak)->_undef)
#define HAK_IS_NIL(hak,v) (v == (hak)->_nil)
#define HAK_IS_TRUE(hak,v) (v == (hak)->_true)
#define HAK_IS_FALSE(hak,v) (v == (hak)->_false)

#define HAK_IS_SYMBOL(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_symbol)
#define HAK_IS_STRING(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_string)
#define HAK_IS_CONTEXT(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_block_context)
#define HAK_IS_FUNCTION(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_function)
#define HAK_IS_COMPILED_BLOCK(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_compiled_block)
#define HAK_IS_CLASS(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_class)
#define HAK_IS_INSTANCE(hak,v) (HAK_OOP_IS_POINTER(v) && HAK_OBJ_GET_FLAGS_BRAND(v) == HAK_BRAND_INSTANCE)
#define HAK_IS_CONS(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_cons)
#define HAK_IS_CONS_CONCODED(hak,v,concode) (HAK_IS_CONS(hak,v) && HAK_OBJ_GET_FLAGS_CONCODE(v) == (concode))
#define HAK_IS_ARRAY(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_array)
#define HAK_IS_BYTEARRAY(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_byte_array)
#define HAK_IS_DIC(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_dictionary)
#define HAK_IS_PRIM(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_primitive)
#define HAK_IS_PBIGINT(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_large_positive_integer)
#define HAK_IS_NBIGINT(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_large_negative_integer)
#define HAK_IS_BIGINT(hak,v) (HAK_OOP_IS_POINTER(v) && (HAK_OBJ_GET_CLASS(v) == (hak_oop_t)(hak)->c_large_positive_integer || HAK_OBJ_GET_CLASS(v) == (hak_oop_t)(hak)->c_large_negative_integer))
#define HAK_IS_FPDEC(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_fixed_point_decimal)
#define HAK_IS_PROCESS(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_process)
#define HAK_IS_SEMAPHORE(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_semaphore)
#define HAK_IS_SEMAPHORE_GROUP(hak,v) (HAK_CLASSOF(hak,v) == (hak_oop_t)(hak)->c_semaphore_group)

#define HAK_CONS_CAR(v)  (((hak_cons_t*)(v))->car)
#define HAK_CONS_CDR(v)  (((hak_cons_t*)(v))->cdr)

typedef int (*hak_dic_walker_t) (
	hak_t*          hak,
	hak_oop_dic_t   dic,
	hak_oop_cons_t  pair,
	void*           ctx
);

typedef int (*hak_xchg_reader_t) (
	hak_t*      hak,
	void*       buf,
	hak_oow_t   len,
	void*       ctx
);

typedef int (*hak_xchg_writer_t) (
	hak_t*      hak,
	const void* ptr,
	hak_oow_t   len,
	void*       ctx
);

#if defined(__cplusplus)
extern "C" {
#endif

HAK_EXPORT const hak_bch_t* hak_obj_type_to_bcstr (
	hak_obj_type_t type
);

/**
 * The hak_open() function creates a hak object.
 */
HAK_EXPORT hak_t* hak_open (
	hak_mmgr_t*         mmgr,
	hak_oow_t           xtnsize,
	const hak_vmprim_t* vmprim,
	hak_errinf_t*       errinf
);
/**/

/**
 * The hak_openstdwithmmgr() function creates a hak objuect with the default primitve functions.
 */
HAK_EXPORT hak_t* hak_openstdwithmmgr (
	hak_mmgr_t*         mmgr,
	hak_oow_t           xtnsize,
	hak_errinf_t*       errinf
);
/**/

HAK_EXPORT hak_t* hak_openstd (
	hak_oow_t           xtnsize,
	hak_errinf_t*       errinf
);

/**
 * The hak_close() function destroys a hak object.
 */
HAK_EXPORT void hak_close (
	hak_t* hak
);
/**/

HAK_EXPORT int hak_init (
	hak_t*              hak,
	hak_mmgr_t*         mmgr,
	const hak_vmprim_t* vmprim
);

HAK_EXPORT void hak_fini (
	hak_t*              hak
);

HAK_EXPORT hak_cmgr_t* hak_getcmgr (
	hak_t* hak
);

HAK_EXPORT void hak_setcmgr (
	hak_t*      hak,
	hak_cmgr_t* cmgr
);

HAK_EXPORT hak_errnum_t hak_geterrnum (
	hak_t* hak
);

HAK_EXPORT void hak_seterrnum (
	hak_t*       hak,
	hak_errnum_t errnum
);

HAK_EXPORT void hak_geterrloc (
	hak_t*     hak,
	hak_loc_t* loc
);

HAK_EXPORT void hak_seterrbmsg (
	hak_t*           hak,
	hak_errnum_t     errnum,
	const hak_bch_t* errmsg
);

HAK_EXPORT void hak_seterrumsg (
	hak_t*           hak,
	hak_errnum_t     errnum,
	const hak_uch_t* errmsg
);

HAK_EXPORT void hak_seterrwithsyserr (
	hak_t*       hak,
	int          syserr_type,
	int          syserr_code
);

HAK_EXPORT void hak_seterrbfmtwithsyserr (
	hak_t*           hak,
	int              syserr_type,
	int              syserr_code,
	const hak_bch_t* fmt,
       	...
);

HAK_EXPORT void hak_seterrufmtwithsyserr (
	hak_t*           hak,
	int              syserr_type,
	int              syserr_code,
	const hak_uch_t* fmt,
       	...
);

HAK_EXPORT void hak_seterrbfmt (
	hak_t*           hak,
	hak_errnum_t     errnum,
	const hak_bch_t* fmt,
	...
);

HAK_EXPORT void hak_seterrufmt (
	hak_t*           hak,
	hak_errnum_t     errnum,
	const hak_uch_t* fmt,
	...
);

HAK_EXPORT void hak_seterrbfmtloc (
	hak_t*           hak,
	hak_errnum_t     errnum,
	const hak_loc_t* loc,
	const hak_bch_t* fmt,
	...
);

HAK_EXPORT void hak_seterrufmtloc (
	hak_t*           hak,
	hak_errnum_t     errnum,
	const hak_loc_t* loc,
	const hak_uch_t* fmt,
	...
);

HAK_EXPORT void hak_seterrbfmtv (
	hak_t*           hak,
	hak_errnum_t     errnum,
	const hak_bch_t* fmt,
	va_list          ap
);

HAK_EXPORT void hak_seterrufmtv (
	hak_t*           hak,
	hak_errnum_t     errnum,
	const hak_uch_t* fmt,
	va_list          ap
);


HAK_EXPORT const hak_ooch_t* hak_geterrstr (
	hak_t* hak
);

HAK_EXPORT const hak_uch_t* hak_geterrumsg (
	hak_t* hak
);

HAK_EXPORT const hak_bch_t* hak_geterrbmsg (
	hak_t* hak
);

HAK_EXPORT void hak_geterrbinf (
	hak_t*         hak,
	hak_errbinf_t* errinf
);

HAK_EXPORT void hak_geterruinf (
	hak_t*         hak,
	hak_erruinf_t* errinf
);
#if defined(HAK_OOCH_IS_UCH)
#	define hak_geterrinf hak_geterruinf
#else
#	define hak_geterrinf hak_geterrbinf
#endif

HAK_EXPORT hak_oow_t hak_copyerrbmsg (
	hak_t*     hak,
	hak_bch_t* buf,
	hak_oow_t  len
);

HAK_EXPORT hak_oow_t hak_copyerrumsg (
	hak_t*     hak,
	hak_uch_t* buf,
	hak_oow_t  len
);

#if defined(HAK_OOCH_IS_UCH)
#	define hak_geterrmsg  hak_geterrumsg
#	define hak_seterrmsg  hak_seterrumsg
#	define hak_copyerrmsg hak_copyerrumsg
#else
#	define hak_geterrmsg  hak_geterrbmsg
#	define hak_seterrmsg  hak_seterrbmsg
#	define hak_copyerrmsg hak_copyerrbmsg
#endif

HAK_EXPORT const hak_ooch_t* hak_backuperrmsg (
	hak_t* hak
);

HAK_EXPORT int hak_errnum_is_synerr (
	hak_errnum_t errnum
);

HAK_EXPORT const hak_ooch_t* hak_errnum_to_errstr (
	hak_errnum_t errnum
);

HAK_EXPORT const hak_bch_t* hak_errnum_to_errbcstr (
	hak_errnum_t errnum,
	hak_bch_t*   buf,
	hak_oow_t    len
);

HAK_EXPORT const hak_uch_t* hak_errnum_to_errucstr (
	hak_errnum_t errnum,
	hak_uch_t*   buf,
	hak_oow_t    len
);

/**
 * The hak_getoption() function gets the value of an option
 * specified by \a id into the buffer pointed to by \a value.
 *
 * \return 0 on success, -1 on failure
 */
HAK_EXPORT int hak_getoption (
	hak_t*         hak,
	hak_option_t   id,
	void*          value
);

/**
 * The hak_setoption() function sets the value of an option
 * specified by \a id to the value pointed to by \a value.
 *
 * \return 0 on success, -1 on failure
 */
HAK_EXPORT int hak_setoption (
	hak_t*        hak,
	hak_option_t  id,
	const void*   value
);

HAK_EXPORT hak_cb_t* hak_regcb (
	hak_t*    hak,
	hak_cb_t* tmpl
);

HAK_EXPORT void hak_deregcb (
	hak_t*    hak,
	hak_cb_t* cb
);

/**
 * The hak_gc() function performs garbage collection.
 * It is not affected by #HAK_TRAIT_NOGC.
 */
HAK_EXPORT void hak_gc (
	hak_t* hak,
	int    full
);


/**
 * The hak_moveoop() function is used to move a live object to a new
 * location in hak_gc(). When hak_gc() invokes registered gc callbacks,
 * you may call this function to protect extra objects you might have
 * allocated manually.
 */
hak_oop_t hak_moveoop (
	hak_t*     hak,
	hak_oop_t  oop
);

HAK_EXPORT hak_oop_t hak_shallowcopy (
	hak_t*      hak,
	hak_oop_t   oop
);

/**
 * The hak_ignite() function creates key initial objects.
 */
HAK_EXPORT int hak_ignite (
	hak_t*      hak,
	hak_oow_t   heapsize
);

HAK_EXPORT int hak_addbuiltinprims (
	hak_t*      hak
);

/**
 * The hak_execute() function executes an activated context.
 */
HAK_EXPORT hak_oop_t hak_execute (
	hak_t* hak
);

HAK_EXPORT void hak_abort (
	hak_t* hak
);


#if defined(HAK_HAVE_INLINE)
	static HAK_INLINE void hak_switchprocess (hak_t* hak) { hak->switch_proc = 1; }
#else
#	define hak_switchprocess(hak) ((hak)->switch_proc = 1)
#endif

HAK_EXPORT void hak_setbasesrloc (
	hak_t*    hak,
	hak_oow_t line,
	hak_oow_t colm
);

/* if you should read charcters from the input stream before hak_read(),
 * you can call hak_readbasesrchar() */
HAK_EXPORT hak_lxc_t* hak_readbasesrchar (
	hak_t* hak
);

HAK_EXPORT int hak_attachccio (
	hak_t*        hak,
	hak_io_impl_t cci_rdr
);

HAK_EXPORT void hak_detachccio (
	hak_t*        hak
);

HAK_EXPORT int hak_attachudio (
	hak_t*        hak,
	hak_io_impl_t udi_rdr,
	hak_io_impl_t udo_wrtr
);

HAK_EXPORT void hak_detachudio (
	hak_t*        hak
);


HAK_EXPORT int hak_attachcciostdwithucstr (
	hak_t*           hak,
	const hak_uch_t* cci_file
);

HAK_EXPORT int hak_attachcciostdwithbcstr (
	hak_t*           hak,
	const hak_bch_t* cci_file
);

HAK_EXPORT int hak_attachudiostdwithucstr (
	hak_t*           hak,
	const hak_uch_t* udi_file,
	const hak_uch_t* udo_file
);

HAK_EXPORT int hak_attachudiostdwithbcstr (
	hak_t*           hak,
	const hak_bch_t* udi_file,
	const hak_bch_t* udo_file
);

HAK_EXPORT void hak_detachio (
	hak_t*       hak
);

HAK_EXPORT void hak_flushudio (
	hak_t*       hak
);

HAK_EXPORT int hak_print (
	hak_t*       hak,
	hak_oop_t    obj
);

HAK_EXPORT hak_ooi_t hak_proutbfmt (
	hak_t*           hak,
	hak_bitmask_t    mask,
	const hak_bch_t* fmt,
	...
);

HAK_EXPORT hak_ooi_t hak_proutufmt (
	hak_t*           hak,
	hak_bitmask_t    mask,
	const hak_uch_t* fmt,
	...
);

#if defined(HAK_INCLUDE_COMPILER)

HAK_EXPORT void hak_freecnode (
	hak_t*       hak,
	hak_cnode_t* cnode
);

HAK_EXPORT int hak_beginfeed (
	hak_t*            hak,
	hak_on_cnode_t    on_cnode
);

HAK_EXPORT int hak_feed (
	hak_t*            hak,
	const hak_ooch_t* data,
	hak_oow_t         len
);

HAK_EXPORT int hak_feeduchars (
	hak_t*           hak,
	const hak_uch_t* data,
	hak_oow_t        len
);

HAK_EXPORT int hak_feedbchars (
	hak_t*           hak,
	const hak_bch_t* data,
	hak_oow_t        len
);

HAK_EXPORT int hak_feedpending (
	hak_t*           hak
);

HAK_EXPORT void hak_resetfeed (
	hak_t*           hak
);

HAK_EXPORT void hak_resetfeedloc (
	hak_t*           hak
);

HAK_EXPORT  void hak_getfeedloc (
	hak_t*           hak,
	hak_loc_t*       loc
);

HAK_EXPORT int hak_endfeed (
	hak_t*            hak
);

HAK_EXPORT int hak_compile (
	hak_t*       hak,
	hak_cnode_t* obj,
	int          flags
);
#endif

HAK_EXPORT int hak_addliteraltocode (
	hak_t*        hak,
	hak_code_t*   code,
	hak_oop_t     obj,
	hak_oow_t     lfbase,
	hak_oow_t*    index
);

/**
 * The hak_brewcode() initializes the structure pointed to by \a code partially or entirely.
 * The part already initialized is not destroyed and/or reinitialized.
 */
HAK_EXPORT int hak_brewcode (
	hak_t*      hak,
	hak_code_t* code
);

/**
 * The hak_purgecode() function cleans up the data held in code space memory
 * pointed to by \a code.
 */
HAK_EXPORT void hak_purgecode (
	hak_t*       hak,
	hak_code_t*  code
);

/**
 * The hak_decode() function decodes instructions from the position
 * \a start to the position \a end - 1, and prints the decoded instructions
 * in the textual form.
 */
HAK_EXPORT int hak_decode (
	hak_t*             hak,
	const hak_code_t*  code,
	hak_oow_t          start,
	hak_oow_t          end
);

/**
 * The hak_resetcode() function some internal states back to the initial state.
 * The affected internal states include byte code buffer, literal frame,
 * ordinary global variables. You should take extra precaution as it is
 * a risky function. For instance, a global variable inserted manually
 * with hak_putatsysdic() gets deleted if the kernel bit is not set on
 * the variable symbol.
 */
HAK_EXPORT void hak_resetcode (
	hak_t*   hak
);

HAK_EXPORT void hak_clearcode (
	hak_t* hak
);

#define HAK_XTN(hak) ((void*)((hak_uint8_t*)hak + ((hak_t*)hak)->_instsize))
#define HAK_MMGR(hak) (((hak_t*)(hak))->_mmgr)
#define HAK_CMGR(hak) (((hak_t*)(hak))->_cmgr)
#define HAK_ERRNUM(hak) (((hak_t*)(hak))->errnum)

void* hak_getxtn (
	hak_t* hak
);


#if defined(HAK_HAVE_INLINE)
static HAK_INLINE hak_code_t* hak_getcode (hak_t* hak) { return &hak->code; }
static HAK_INLINE hak_oob_t* hak_getbcptr (hak_t* hak) { return hak->code.bc.ptr; }
static HAK_INLINE hak_oow_t hak_getbclen (hak_t* hak) { return hak->code.bc.len; }
static HAK_INLINE hak_oow_t hak_getlflen (hak_t* hak) { return hak->code.lit.len; }
static HAK_INLINE hak_oow_t hak_getngtmprs (hak_t* hak) { return hak->code.ngtmprs; }
static HAK_INLINE hak_ooi_t hak_getip (hak_t* hak) { return hak->ip; }
#else
#	define hak_getcode(hak) (&(hak)->code)
#	define hak_getbcptr(hak) ((hak)->code.bc.ptr)
#	define hak_getbclen(hak) ((hak)->code.bc.len)
#	define hak_getlflen(hak) ((hak)->code.lit.len)
#	define hak_getngtmprs(hak) ((hak)->code.ngtmprs)
#	define hak_getip(hak) ((hak)->ip)
#endif

/* =========================================================================
 * SYNTAX ERROR HANDLING
 * ========================================================================= */
HAK_EXPORT void hak_getsynerr (
	hak_t*             hak,
	hak_synerr_t*      synerr
);

HAK_EXPORT hak_synerrnum_t hak_getsynerrnum (
	hak_t*             hak
);

HAK_EXPORT void hak_setsynerrbfmt (
	hak_t*              hak,
	hak_synerrnum_t     num,
	const hak_loc_t*    loc,
	const hak_oocs_t*   tgt,
	const hak_bch_t*    msgfmt,
	...
);

HAK_EXPORT void hak_setsynerrufmt (
	hak_t*              hak,
	hak_synerrnum_t     num,
	const hak_loc_t*    loc,
	const hak_oocs_t*   tgt,
	const hak_uch_t*    msgfmt,
	...
);

#if defined(HAK_HAVE_INLINE)
static HAK_INLINE void hak_setsynerr (hak_t* hak, hak_synerrnum_t num, const hak_loc_t* loc, const hak_oocs_t* tgt)
{
	hak_setsynerrbfmt (hak, num, loc, tgt, HAK_NULL);
}
#else
#	define hak_setsynerr(hak,num,loc,tgt) hak_setsynerrbfmt(hak,num,loc,tgt,HAK_NULL)
#endif

/* =========================================================================
 * TEMPORARY OOP MANAGEMENT FUNCTIONS
 * ========================================================================= */
HAK_EXPORT void hak_pushvolat (
	hak_t*     hak,
	hak_oop_t* oop_ptr
);

HAK_EXPORT void hak_popvolat (
	hak_t*     hak
);

HAK_EXPORT void hak_popvolats (
	hak_t*     hak,
	hak_oow_t  count
);

/* =========================================================================
 * SYSTEM MEMORY MANAGEMENT FUCNTIONS VIA MMGR
 * ========================================================================= */
HAK_EXPORT void* hak_allocmem (
	hak_t*    hak,
	hak_oow_t size
);

HAK_EXPORT void* hak_callocmem (
	hak_t*    hak,
	hak_oow_t size
);

HAK_EXPORT void* hak_reallocmem (
	hak_t*    hak,
	void*     ptr,
	hak_oow_t size
);

HAK_EXPORT void hak_freemem (
	hak_t* hak,
	void*  ptr
);


/* =========================================================================
 * PRIMITIVE FUNCTION MANIPULATION
 * ========================================================================= */
HAK_EXPORT hak_pfbase_t* hak_findpfbase (
	hak_t*              hak,
	hak_pfinfo_t*       pfinfo,
	hak_oow_t           pfcount,
	const hak_ooch_t*   name,
	hak_oow_t           namelen
);


/* =========================================================================
 * LOGGING
 * ========================================================================= */

HAK_EXPORT hak_ooi_t hak_logbfmt (
	hak_t*           hak,
	hak_bitmask_t    mask,
	const hak_bch_t* fmt,
	...
);

HAK_EXPORT hak_ooi_t hak_logbfmtv (
	hak_t*           hak,
	hak_bitmask_t    mask,
	const hak_bch_t* fmt,
	va_list          ap
);

HAK_EXPORT hak_ooi_t hak_logufmt (
	hak_t*           hak,
	hak_bitmask_t    mask,
	const hak_uch_t* fmt,
	...
);

HAK_EXPORT hak_ooi_t hak_logufmtv (
	hak_t*           hak,
	hak_bitmask_t    mask,
	const hak_uch_t* fmt,
	va_list          ap
);

#if defined(HAK_OOCH_IS_UCH)
#	define hak_logoofmt hak_logufmt
#	define hak_logoofmtv hak_logufmtv
#else
#	define hak_logoofmt hak_logbfmt
#	define hak_logoofmtv hak_logbfmtv
#endif

HAK_EXPORT hak_ooi_t hak_prbfmt (
	hak_t*           hak,
	const hak_bch_t* fmt,
	...
);

HAK_EXPORT hak_ooi_t hak_prbfmtv (
	hak_t*           hak,
	const hak_bch_t* fmt,
	va_list          ap
);

HAK_EXPORT hak_ooi_t hak_prufmt (
	hak_t*           hak,
	const hak_uch_t* fmt,
	...
);

HAK_EXPORT hak_ooi_t hak_prufmtv (
	hak_t*           hak,
	const hak_uch_t* fmt,
	va_list          ap
);

#if defined(HAK_OOCH_IS_UCH)
#	define hak_proofmt hak_prufmt
#	define hak_proofmtv hak_prufmtv
#else
#	define hak_proofmt hak_prbfmt
#	define hak_proofmtv hak_prbfmtv
#endif


/* =========================================================================
 * STRING FORMATTING
 * ========================================================================= */

HAK_EXPORT hak_oow_t hak_vfmttoucstr (
	hak_t*           hak,
	hak_uch_t*       buf,
	hak_oow_t        bufsz,
	const hak_uch_t* fmt,
	va_list          ap
);

HAK_EXPORT hak_oow_t hak_fmttoucstr (
	hak_t*           hak,
	hak_uch_t*       buf,
	hak_oow_t        bufsz,
	const hak_uch_t* fmt,
	...
);

HAK_EXPORT hak_oow_t hak_vfmttobcstr (
	hak_t*           hak,
	hak_bch_t*       buf,
	hak_oow_t        bufsz,
	const hak_bch_t* fmt,
	va_list          ap
);

HAK_EXPORT hak_oow_t hak_fmttobcstr (
	hak_t*           hak,
	hak_bch_t*       buf,
	hak_oow_t        bufsz,
	const hak_bch_t* fmt,
	...
);

#if defined(HAK_OOCH_IS_UCH)
#	define hak_vfmttooocstr hak_vfmttoucstr
#	define hak_fmttooocstr hak_fmttoucstr
#else
#	define hak_vfmttooocstr hak_vfmttobcstr
#	define hak_fmttooocstr hak_fmttobcstr
#endif


/* =========================================================================
 * OBJECT MANAGEMENT
 * ========================================================================= */
HAK_EXPORT hak_oop_t hak_hatchundef (
	hak_t*     hak
);

HAK_EXPORT hak_oop_t hak_hatchnil (
	hak_t*     hak
);

HAK_EXPORT hak_oop_t hak_instantiate (
	hak_t*          hak,
	hak_oop_class_t _class,
	const void*     vptr,
	hak_oow_t       vlen
);

HAK_EXPORT hak_oop_t hak_makecons (
	hak_t*     hak,
	hak_oop_t  car,
	hak_oop_t  cdr
);

HAK_EXPORT hak_oop_t hak_makearray (
	hak_t*     hak,
	hak_oow_t  len
);

HAK_EXPORT hak_oop_t hak_makechararray (
	hak_t*            hak,
	const hak_ooch_t* ptr,
	hak_oow_t         len
);

HAK_EXPORT hak_oop_t hak_makebytearray (
	hak_t*           hak,
	const hak_oob_t* ptr,
	hak_oow_t        len
);

HAK_EXPORT hak_oop_t hak_makebytestringwithbytes (
	hak_t*           hak,
	const hak_oob_t* ptr,
	hak_oow_t        len
);

HAK_EXPORT hak_oop_t hak_makebytestring (
	hak_t*            hak,
	const hak_ooch_t* ptr,
	hak_oow_t         len
);

HAK_EXPORT hak_oop_t hak_makestring (
	hak_t*            hak,
	const hak_ooch_t* ptr,
	hak_oow_t         len
);

HAK_EXPORT hak_oop_t hak_makefpdec (
	hak_t*            hak,
	hak_oop_t         value,
	hak_ooi_t         scale
);

HAK_EXPORT hak_oop_t hak_makedic (
	hak_t*            hak,
	hak_oow_t         inisize /* initial bucket size */
);

HAK_EXPORT hak_oop_t hak_makeclass (
	hak_t*            hak,
	hak_oop_t         name,
	hak_oop_t         superclass,
	hak_ooi_t         spec,
	hak_ooi_t         selfspec,
	hak_oop_t         ivars_str,
	hak_oop_t         cvars_str
);

HAK_EXPORT void hak_freengcobj (
	hak_t*           hak,
	hak_oop_t        obj
);

HAK_EXPORT hak_oop_t hak_makengcbytearray (
	hak_t*           hak,
	const hak_oob_t* ptr,
	hak_oow_t        len
);

HAK_EXPORT hak_oop_t hak_remakengcbytearray (
	hak_t*           hak,
	hak_oop_t        obj,
	hak_oow_t        newsz
);

HAK_EXPORT hak_oop_t hak_makengcarray (
	hak_t*           hak,
	hak_oow_t        len
);

HAK_EXPORT hak_oop_t hak_remakengcarray (
	hak_t*           hak,
	hak_oop_t        obj,
	hak_oow_t        newsz
);

HAK_EXPORT hak_oop_t hak_makeprim (
	hak_t*          hak,
	hak_pfimpl_t    primimpl,
	hak_oow_t       minargs,
	hak_oow_t       maxargs,
	hak_mod_t*      mod
);

HAK_EXPORT hak_oop_t hak_oowtoint (
	hak_t*     hak,
	hak_oow_t  w
);

HAK_EXPORT hak_oop_t hak_ooitoint (
	hak_t*    hak,
	hak_ooi_t i
);

HAK_EXPORT int hak_inttooow_noseterr (
	hak_t*     hak,
	hak_oop_t  x,
	hak_oow_t* w
);

HAK_EXPORT int hak_inttooow (
	hak_t*     hak,
	hak_oop_t  x,
	hak_oow_t* w
);

HAK_EXPORT int hak_inttoooi_noseterr (
	hak_t*     hak,
	hak_oop_t  x,
	hak_ooi_t* i
);

HAK_EXPORT int hak_inttoooi (
	hak_t*     hak,
	hak_oop_t  x,
	hak_ooi_t* i
);

#if (HAK_SIZEOF_UINTMAX_T == HAK_SIZEOF_OOW_T)
#   define hak_inttouintmax_noseterr hak_inttooow_noseterr
#   define hak_inttouintmax hak_inttooow
#   define hak_inttointmax_noseterr hak_inttoooi_noseterr
#   define hak_inttointmax hak_inttoooi
#   define hak_uintmaxtoint hak_oowtoint
#   define hak_intmaxtoint hak_ooitoint
#else

HAK_EXPORT hak_oop_t hak_intmaxtoint (
	hak_t*       hak,
	hak_intmax_t i
);

HAK_EXPORT hak_oop_t hak_uintmaxtoint (
	hak_t*        hak,
	hak_uintmax_t i
);

HAK_EXPORT int hak_inttouintmax_noseterr (
	hak_t*         hak,
	hak_oop_t      x,
	hak_uintmax_t* w
);

HAK_EXPORT int hak_inttouintmax (
	hak_t*         hak,
	hak_oop_t      x,
	hak_uintmax_t* w
);

HAK_EXPORT int hak_inttointmax_noseterr (
	hak_t*        hak,
	hak_oop_t     x,
	hak_intmax_t* i
);

HAK_EXPORT int hak_inttointmax (
	hak_t*        hak,
	hak_oop_t     x,
	hak_intmax_t* i
);
#endif

/* =========================================================================
 * CONS OBJECT UTILITIES
 * ========================================================================= */
HAK_EXPORT hak_oow_t hak_countcons (
	hak_t*           hak,
	hak_oop_t        cons
);


HAK_EXPORT hak_oop_t hak_getlastconscdr (
	hak_t*           hak,
	hak_oop_t        cons
);

HAK_EXPORT hak_oop_t hak_reversecons (
	hak_t*           hak,
	hak_oop_t        cons
);


/* =========================================================================
 * CODE MARSHALING/UNMARSHALING
 * ========================================================================= */
HAK_EXPORT int hak_marshalcode (
	hak_t*            hak,
	const hak_code_t* code,
	hak_xchg_writer_t wrtr,
	void*             ctx
);

HAK_EXPORT int hak_unmarshalcode (
	hak_t*            hak,
	hak_code_t*       code,
	hak_xchg_reader_t rdr,
	void*             ctx
);

HAK_EXPORT int hak_marshalcodetomem (
	hak_t*            hak,
	const hak_code_t* code,
	hak_ptlc_t*        dst
);

HAK_EXPORT int hak_unmarshalcodefrommem (
	hak_t*            hak,
	hak_code_t*       code,
	const hak_ptl_t*  src
);

/* =========================================================================
 * DICTIONARY ACCESS FUNCTIONS
 * ========================================================================= */
HAK_EXPORT hak_oop_cons_t hak_putatsysdic (
	hak_t*     hak,
	hak_oop_t  key,
	hak_oop_t  value
);

HAK_EXPORT hak_oop_cons_t hak_getatsysdic (
	hak_t*     hak,
	hak_oop_t  key
);

HAK_EXPORT hak_oop_cons_t hak_lookupsysdicforsymbol (
	hak_t*            hak,
	const hak_oocs_t* name
);

HAK_EXPORT hak_oop_cons_t hak_lookupsysdicforsymbol_noseterr (
	hak_t*            hak,
	const hak_oocs_t* name
);

HAK_EXPORT int hak_zapatsysdic (
	hak_t*     hak,
	hak_oop_t  key
);

HAK_EXPORT hak_oop_cons_t hak_lookupdicforsymbol (
	hak_t*            hak,
	hak_oop_dic_t     dic,
	const hak_oocs_t* name
);

HAK_EXPORT hak_oop_cons_t hak_lookupdicforsymbol_noseterr (
	hak_t*            hak,
	hak_oop_dic_t     dic,
	const hak_oocs_t* name
);

HAK_EXPORT hak_oop_cons_t hak_putatdic (
	hak_t*        hak,
	hak_oop_dic_t dic,
	hak_oop_t     key,
	hak_oop_t     value
);

HAK_EXPORT hak_oop_cons_t hak_getatdic (
	hak_t*        hak,
	hak_oop_dic_t dic,
	hak_oop_t     key
);


HAK_EXPORT int hak_zapatdic (
	hak_t*        hak,
	hak_oop_dic_t dic,
	hak_oop_t     key
);

HAK_EXPORT int hak_walkdic (
	hak_t*           hak,
	hak_oop_dic_t    dic,
	hak_dic_walker_t walker,
	void*            ctx
);



/* =========================================================================
 * OBJECT HASHING AND COMPARISION
 * ========================================================================= */

HAK_EXPORT int hak_hashobj (
	hak_t*     hak,
	hak_oop_t  obj,
	hak_oow_t* xhv
);

HAK_EXPORT int hak_equalobjs (
	hak_t*     hak,
	hak_oop_t  rcv,
	hak_oop_t  arg
);


/* =========================================================================
 * STRING ENCODING CONVERSION
 * ========================================================================= */

#if defined(HAK_OOCH_IS_UCH)
#	define hak_convootobchars(hak,oocs,oocslen,bcs,bcslen) hak_convutobchars(hak,oocs,oocslen,bcs,bcslen)
#	define hak_convbtooochars(hak,bcs,bcslen,oocs,oocslen) hak_convbtouchars(hak,bcs,bcslen,oocs,oocslen)
#	define hak_convootobcstr(hak,oocs,oocslen,bcs,bcslen) hak_convutobcstr(hak,oocs,oocslen,bcs,bcslen)
#	define hak_convbtooocstr(hak,bcs,bcslen,oocs,oocslen) hak_convbtoucstr(hak,bcs,bcslen,oocs,oocslen)
#else
#	define hak_convootouchars(hak,oocs,oocslen,ucs,ucslen) hak_convbtouchars(hak,oocs,oocslen,ucs,ucslen)
#	define hak_convutooochars(hak,ucs,ucslen,oocs,oocslen) hak_convutobchars(hak,ucs,ucslen,oocs,oocslen)
#	define hak_convootoucstr(hak,oocs,oocslen,ucs,ucslen) hak_convbtoucstr(hak,oocs,oocslen,ucs,ucslen)
#	define hak_convutooocstr(hak,ucs,ucslen,oocs,oocslen) hak_convutobcstr(hak,ucs,ucslen,oocs,oocslen)
#endif

HAK_EXPORT int hak_convbtouchars (
	hak_t*           hak,
	const hak_bch_t* bcs,
	hak_oow_t*       bcslen,
	hak_uch_t*       ucs,
	hak_oow_t*       ucslen
);

HAK_EXPORT int hak_convutobchars (
	hak_t*           hak,
	const hak_uch_t* ucs,
	hak_oow_t*       ucslen,
	hak_bch_t*       bcs,
	hak_oow_t*       bcslen
);


/**
 * The hak_convbtoucstr() function converts a null-terminated byte string
 * to a wide string.
 */
HAK_EXPORT int hak_convbtoucstr (
	hak_t*           hak,
	const hak_bch_t* bcs,
	hak_oow_t*       bcslen,
	hak_uch_t*       ucs,
	hak_oow_t*       ucslen
);


/**
 * The hak_convutobcstr() function converts a null-terminated wide string
 * to a byte string.
 */
HAK_EXPORT int hak_convutobcstr (
	hak_t*           hak,
	const hak_uch_t* ucs,
	hak_oow_t*       ucslen,
	hak_bch_t*       bcs,
	hak_oow_t*       bcslen
);

#if defined(HAK_OOCH_IS_UCH)
#	define hak_dupootobcharswithheadroom(hak,hrb,oocs,oocslen,bcslen) hak_duputobcharswithheadroom(hak,hrb,oocs,oocslen,bcslen)
#	define hak_dupbtooocharswithheadroom(hak,hrb,bcs,bcslen,oocslen) hak_dupbtoucharswithheadroom(hak,hrb,bcs,bcslen,oocslen)
#	define hak_dupootobchars(hak,oocs,oocslen,bcslen) hak_duputobchars(hak,oocs,oocslen,bcslen)
#	define hak_dupbtooochars(hak,bcs,bcslen,oocslen) hak_dupbtouchars(hak,bcs,bcslen,oocslen)

#	define hak_dupootobcstrwithheadroom(hak,hrb,oocs,bcslen) hak_duputobcstrwithheadroom(hak,hrb,oocs,bcslen)
#	define hak_dupbtooocstrwithheadroom(hak,hrb,bcs,oocslen) hak_dupbtoucstrwithheadroom(hak,hrb,bcs,oocslen)
#	define hak_dupootobcstr(hak,oocs,bcslen) hak_duputobcstr(hak,oocs,bcslen)
#	define hak_dupbtooocstr(hak,bcs,oocslen) hak_dupbtoucstr(hak,bcs,oocslen)

#   define hak_dupootoucstr(hak,oocs,ucslen) hak_dupucstr(hak,oocs,ucslen)
#   define hak_duputooocstr(hak,ucs,oocslen) hak_dupucstr(hak,ucs,oocslen)
#else
#	define hak_dupootoucharswithheadroom(hak,hrb,oocs,oocslen,ucslen) hak_dupbtoucharswithheadroom(hak,hrb,oocs,oocslen,ucslen)
#	define hak_duputooocharswithheadroom(hak,hrb,ucs,ucslen,oocslen) hak_duputobcharswithheadroom(hak,hrb,ucs,ucslen,oocslen)
#	define hak_dupootouchars(hak,oocs,oocslen,ucslen) hak_dupbtouchars(hak,oocs,oocslen,ucslen)
#	define hak_duputooochars(hak,ucs,ucslen,oocslen) hak_duputobchars(hak,ucs,ucslen,oocslen)

#	define hak_dupootoucstrwithheadroom(hak,hrb,oocs,ucslen) hak_dupbtoucstrwithheadroom(hak,hrb,oocs,ucslen)
#	define hak_duputooocstrwithheadroom(hak,hrb,ucs,oocslen) hak_duputobcstrwithheadroom(hak,hrb,ucs,oocslen)
#	define hak_dupootoucstr(hak,oocs,ucslen) hak_dupbtoucstr(hak,oocs,ucslen)
#	define hak_duputooocstr(hak,ucs,oocslen) hak_duputobcstr(hak,ucs,oocslen)

#	define hak_dupootobcstr(hak,oocs,bcslen) hak_dupbcstr(hak,oocs,bcslen)
#	define hak_dupbtooocstr(hak,bcs,oocslen) hak_dupbcstr(hak,bcs,oocslen)
#endif


HAK_EXPORT hak_uch_t* hak_dupbtoucharswithheadroom (
	hak_t*           hak,
	hak_oow_t        headroom_bytes,
	const hak_bch_t* bcs,
	hak_oow_t        bcslen,
	hak_oow_t*       ucslen
);

HAK_EXPORT hak_bch_t* hak_duputobcharswithheadroom (
	hak_t*           hak,
	hak_oow_t        headroom_bytes,
	const hak_uch_t* ucs,
	hak_oow_t        ucslen,
	hak_oow_t*       bcslen
);

HAK_EXPORT hak_uch_t* hak_dupbtouchars (
	hak_t*           hak,
	const hak_bch_t* bcs,
	hak_oow_t        bcslen,
	hak_oow_t*       ucslen
);

HAK_EXPORT hak_bch_t* hak_duputobchars (
	hak_t*           hak,
	const hak_uch_t* ucs,
	hak_oow_t        ucslen,
	hak_oow_t*       bcslen
);


HAK_EXPORT hak_uch_t* hak_dupbtoucstrwithheadroom (
	hak_t*           hak,
	hak_oow_t        headroom_bytes,
	const hak_bch_t* bcs,
	hak_oow_t*       ucslen
);

HAK_EXPORT hak_bch_t* hak_duputobcstrwithheadroom (
	hak_t*           hak,
	hak_oow_t        headroom_bytes,
	const hak_uch_t* ucs,
	hak_oow_t* bcslen
);

HAK_EXPORT hak_uch_t* hak_dupbtoucstr (
	hak_t*           hak,
	const hak_bch_t* bcs,
	hak_oow_t*       ucslen /* optional: length of returned string */
);

HAK_EXPORT hak_bch_t* hak_duputobcstr (
	hak_t*           hak,
	const hak_uch_t* ucs,
	hak_oow_t*       bcslen /* optional: length of returned string */
);


#if defined(HAK_OOCH_IS_UCH)
#	define hak_dupoochars(hak,oocs,oocslen) hak_dupuchars(hak,oocs,oocslen)
#	define hak_dupoocstr(hak,oocs,oocslen) hak_dupucstr(hak,oocs,oocslen)
#else
#	define hak_dupoochars(hak,oocs,oocslen) hak_dupbchars(hak,oocs,oocslen)
#   define hak_dupoocstr(hak,oocs,oocslen) hak_dupbcstr(hak,oocs,oocslen)
#endif

HAK_EXPORT hak_uch_t* hak_dupuchars (
    hak_t*           hak,
    const hak_uch_t* ucs,
    hak_oow_t        ucslen
);

HAK_EXPORT hak_bch_t* hak_dupbchars (
    hak_t*           hak,
    const hak_bch_t* bcs,
    hak_oow_t        bcslen
);

HAK_EXPORT hak_uch_t* hak_dupucstr (
    hak_t*           hak,
    const hak_uch_t* ucs,
    hak_oow_t*       ucslen
);

HAK_EXPORT hak_bch_t* hak_dupbcstr (
    hak_t*           hak,
    const hak_bch_t* bcs,
    hak_oow_t*       bcslen
);

/* =========================================================================
 * ASSERTION SUPPORT
 * ========================================================================= */
HAK_EXPORT void hak_assertfailed (
	hak_t*           hak,
	const hak_bch_t* expr,
	const hak_bch_t* file,
	hak_oow_t        line
);


/* =========================================================================
 * HELPERS
 * ========================================================================= */
HAK_EXPORT void hak_start_ticker (
	void
);

HAK_EXPORT void hak_stop_ticker (
	void
);

HAK_EXPORT void hak_catch_termreq (
	void
);

HAK_EXPORT void hak_uncatch_termreq (
	void
);

#if defined(__cplusplus)
}
#endif

#endif
