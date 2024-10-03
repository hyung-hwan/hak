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

#ifndef _HCL_H_
#define _HCL_H_

#include <hcl-cmn.h>
#include <hcl-rbt.h>
#include <hcl-xma.h>
#include <stdarg.h>

/* TODO: move this macro out to the build files.... */
#define HCL_INCLUDE_COMPILER

/* ========================================================================== */

typedef struct hcl_mod_t hcl_mod_t;

/* ========================================================================== */

/**
 * The hcl_errnum_t type defines the error codes.
 */
enum hcl_errnum_t
{
	HCL_ENOERR,   /**< no error */
	HCL_EGENERIC, /**< generic error */
	HCL_ENOIMPL,  /**< not implemented */
	HCL_ESYSERR,  /**< subsystem error */
	HCL_EINTERN,  /**< internal error */

	HCL_ESYSMEM,  /**< insufficient system memory */
	HCL_EOOMEM,   /**< insufficient object memory */
	HCL_ETYPE,    /**< invalid class/type */
	HCL_EINVAL,   /**< invalid parameter or data */
	HCL_ENOENT,   /**< data not found */

	HCL_EEXIST,   /**< existing/duplicate data */
	HCL_EBUSY,
	HCL_EACCES,
	HCL_EPERM,
	HCL_ENOTDIR,

	HCL_EINTR,
	HCL_EPIPE,
	HCL_EAGAIN,
	HCL_EBADHND,
	HCL_EFRMFLOOD,  /**< too many frames */

	HCL_EMSGRCV,    /**< mesasge receiver error */
	HCL_EMSGSND,    /**< message sending error. even doesNotUnderstand: is not found */
	HCL_ENUMARGS,   /**< wrong number of arguments */
	HCL_ERANGE,     /**< range error. overflow and underflow */
	HCL_EBCFULL,    /**< byte-code full */

	HCL_EDFULL,     /**< dictionary full */
	HCL_EPFULL,     /**< processor full */
	HCL_EFINIS,     /**< unexpected end of data/input/stream/etc */
	HCL_EFLOOD,     /**< too many items/data */
	HCL_EDIVBY0,    /**< divide by zero */

	HCL_EIOERR,     /**< I/O error */
	HCL_EECERR,     /**< encoding conversion error */
	HCL_EBUFFULL,   /**< buffer full */
	HCL_ESYNERR,    /**< syntax error */
	HCL_ECALL,      /**< runtime error - cannot call */
	HCL_ECALLARG,   /**< runtime error - wrong number of arguments to call */
	HCL_ECALLRET,   /**< runtime error - wrong number of return variables to call */
	HCL_ESEMFLOOD,  /**< runtime error - too many semaphores */
	HCL_EEXCEPT,    /**< runtime error - exception not handled */
	HCL_ESTKOVRFLW, /**< runtime error - stack overflow */
	HCL_ESTKUNDFLW, /**< runtime error - stack overflow */
	HCL_EUNDEFVAR  /**< runtime error - undefined variable access */
};
typedef enum hcl_errnum_t hcl_errnum_t;

enum hcl_synerrnum_t
{
	HCL_SYNERR_NOERR,
	HCL_SYNERR_INTERN,        /* internal error */
	HCL_SYNERR_CNODE,         /* unexpected compiler node */
	HCL_SYNERR_ILCHR,         /* illegal character */
	HCL_SYNERR_ILTOK,         /* invalid token */
	HCL_SYNERR_CMTNC,         /* comment not closed */
	HCL_SYNERR_CHARLIT,       /* wrong character literal */
	HCL_SYNERR_STRLIT,        /* wrong string literal */
	HCL_SYNERR_SYMLIT,        /* wrong symbol literal */
	HCL_SYNERR_HASHLIT,       /* wrong hashed literal */
	HCL_SYNERR_NUMLIT ,       /* invalid numeric literal */
	HCL_SYNERR_NUMRANGE,      /* number range error */
	HCL_SYNERR_ERRLIT,        /* wrong error literal */
	HCL_SYNERR_SMPTRLIT,      /* wrong smptr literal */
	HCL_SYNERR_RADIX,         /* invalid radix for a numeric literal */

	HCL_SYNERR_EOF,           /* sudden end of input */
	HCL_SYNERR_EOX,           /* sudden end of expression */
	HCL_SYNERR_LPAREN,        /* ( expected */
	HCL_SYNERR_RPAREN,        /* ) expected */
	HCL_SYNERR_RBRACK,        /* ] expected */
	HCL_SYNERR_RBRACE,        /* } expected */
	HCL_SYNERR_VBAR,          /* | expected */

	HCL_SYNERR_STRING,        /* string expected */
	HCL_SYNERR_BYTERANGE,     /* byte too small or too large */
	HCL_SYNERR_NESTING,       /* nesting level too deep */

	HCL_SYNERR_COMMA,         /* , expected */
	HCL_SYNERR_VBARBANNED,    /* | disallowed */
	HCL_SYNERR_DOTBANNED,     /* . disallowed */
	HCL_SYNERR_COMMABANNED,   /* , disallowed */
	HCL_SYNERR_COLONBANNED,   /* : disallowed */
	HCL_SYNERR_COLONEQBANNED, /* := disallowed */
	HCL_SYNERR_COMMANOVALUE,  /* no value after , */
	HCL_SYNERR_COLONNOVALUE,  /* no value after : */
	HCL_SYNERR_NOVALUE,       /* missing value */
	HCL_SYNERR_NOSEP,         /* no separator between array/dictionary elements */
	HCL_SYNERR_INCLUDE,       /* #include error */

	HCL_SYNERR_ELLIPSISBANNED, /* ... disallowed */
	HCL_SYNERR_TRPCOLONSBANNED, /* ::: disallowed */
	HCL_SYNERR_LOOPFLOOD,     /* loop body too big */
	HCL_SYNERR_IFFLOOD,       /* if body too big */
	HCL_SYNERR_BLKFLOOD,      /* block too big */
	HCL_SYNERR_BLKDEPTH,      /* block too deep */
	HCL_SYNERR_NAME,          /* name expected */
	HCL_SYNERR_ARGNAMELIST,   /* argument name list expected */
	HCL_SYNERR_ARGNAME,       /* argument name expected */
	HCL_SYNERR_ARGNAMEDUP,    /* duplicate argument name  */
	HCL_SYNERR_VARNAME,       /* variable name expected */
	HCL_SYNERR_ARGCOUNT,      /* wrong number of arguments */
	HCL_SYNERR_ARGFLOOD,      /* too many arguments defined */
	HCL_SYNERR_VARFLOOD,      /* too many variables defined */
	HCL_SYNERR_VARDCLBANNED,  /* variable declaration disallowed */
	HCL_SYNERR_VARNAMEDUP,    /* duplicate variable name */
	HCL_SYNERR_VARNAMEUNKNOWN, /* unknown variable name */

	HCL_SYNERR_BANNEDVARNAME, /* disallowed varible name */
	HCL_SYNERR_BANNEDARGNAME, /* disallowed argument name */
	HCL_SYNERR_BANNED,        /* prohibited */

	HCL_SYNERR_FUN,           /* invalid function definition */
	HCL_SYNERR_ELIF,          /* elif without if */
	HCL_SYNERR_ELSE,          /* else without if */
	HCL_SYNERR_CATCH,         /* catch outside try */
	HCL_SYNERR_BREAK,         /* break outside loop */

	HCL_SYNERR_CALLABLE,      /* invalid callable */
	HCL_SYNERR_UNBALKV,       /* unbalanced key/value pair */
	HCL_SYNERR_UNBALPBB,      /* unbalanced parenthesis/brace/bracket */
	HCL_SYNERR_SEMICOLON,     /* unexpected semicolon */
	HCL_SYNERR_BACKSLASH,     /* stray backslash */
	HCL_SYNERR_BLOCK,         /* block expression expected */
	HCL_SYNERR_BLOCKBANNED,   /* block expression disallowed */
	HCL_SYNERR_LVALUE,        /* invalid lvalue */
	HCL_SYNERR_RVALUE         /* invalid rvalue */
};
typedef enum hcl_synerrnum_t hcl_synerrnum_t;

enum hcl_option_t
{
	HCL_TRAIT,
	HCL_LOG_MASK,
	HCL_LOG_MAXCAPA,

	HCL_LOG_TARGET_BCSTR,
	HCL_LOG_TARGET_UCSTR,
	HCL_LOG_TARGET_BCS,
	HCL_LOG_TARGET_UCS,
#if defined(HCL_OOCH_IS_UCH)
#	define HCL_LOG_TARGET HCL_LOG_TARGET_UCSTR
#	define HCL_LOG_TARGET_OOCSTR HCL_LOG_TARGET_UCSTR
#	define HCL_LOG_TARGET_OOCS HCL_LOG_TARGET_UCS
#else
#	define HCL_LOG_TARGET HCL_LOG_TARGET_BCSTR
#	define HCL_LOG_TARGET_OOCSTR HCL_LOG_TARGET_BCSTR
#	define HCL_LOG_TARGET_OOCS HCL_LOG_TARGET_BCS
#endif

	HCL_SYMTAB_SIZE,  /* default system table size */
	HCL_SYSDIC_SIZE,  /* default system dictionary size */
	HCL_PROCSTK_SIZE, /* default process stack size */

	HCL_MOD_LIBDIRS,
	HCL_MOD_PREFIX,
	HCL_MOD_POSTFIX,

	HCL_MOD_INCTX
};
typedef enum hcl_option_t hcl_option_t;

/* [NOTE] ensure that it is a power of 2 */
#define HCL_LOG_CAPA_ALIGN 512

enum hcl_option_dflval_t
{
	HCL_DFL_LOG_MAXCAPA = HCL_LOG_CAPA_ALIGN * 16,
	HCL_DFL_SYMTAB_SIZE = 5000,
	HCL_DFL_SYSDIC_SIZE = 5000,
	HCL_DFL_PROCSTK_SIZE = 5000
};
typedef enum hcl_option_dflval_t hcl_option_dflval_t;

enum hcl_trait_t
{
#if defined(HCL_BUILD_DEBUG)
	HCL_TRAIT_DEBUG_GC     = (((hcl_bitmask_t)1) << 0),
	HCL_TRAIT_DEBUG_BIGINT = (((hcl_bitmask_t)1) << 1),
#endif

	HCL_TRAIT_INTERACTIVE = (((hcl_bitmask_t)1) << 7),

	/* perform no garbage collection when the heap is full.
	 * you still can use hcl_gc() explicitly. */
	HCL_TRAIT_NOGC = (((hcl_bitmask_t)1) << 8),

	/* wait for running process when exiting from the main method */
	HCL_TRAIT_AWAIT_PROCS = (((hcl_bitmask_t)1) << 9),

	/* return EOL as a token */ /* TODO: make this pragma controllable */
	HCL_TRAIT_LANG_ENABLE_EOL = (((hcl_bitmask_t)1) << 14),
};
typedef enum hcl_trait_t hcl_trait_t;

/* =========================================================================
 * SPECIALIZED OOP TYPES
 * ========================================================================= */

/* hcl_oop_t defined in hcl-cmn.h
 * hcl_obj_t defined further down */

/* these are more specialized types for hcl_obj_t */
typedef struct hcl_obj_oop_t       hcl_obj_oop_t;
typedef struct hcl_obj_char_t      hcl_obj_char_t;
typedef struct hcl_obj_byte_t      hcl_obj_byte_t;
typedef struct hcl_obj_halfword_t  hcl_obj_halfword_t;
typedef struct hcl_obj_word_t      hcl_obj_word_t;

/* these are more specialized types for hcl_oop_t */
typedef struct hcl_obj_oop_t*      hcl_oop_oop_t;
typedef struct hcl_obj_char_t*     hcl_oop_char_t;
typedef struct hcl_obj_byte_t*     hcl_oop_byte_t;
typedef struct hcl_obj_halfword_t* hcl_oop_halfword_t;
typedef struct hcl_obj_word_t*     hcl_oop_word_t;

#define HCL_OOP_BITS  (HCL_SIZEOF_OOP_T * HCL_BITS_PER_BYTE)

#if defined(HCL_USE_OOW_FOR_LIW)
	typedef hcl_oop_word_t     hcl_oop_liword_t;
#	define HCL_OBJ_TYPE_LIWORD HCL_OBJ_TYPE_WORD
#else
	typedef hcl_oop_halfword_t hcl_oop_liword_t;
#	define HCL_OBJ_TYPE_LIWORD HCL_OBJ_TYPE_HALFWORD
#endif

/* =========================================================================
 * HEADER FOR GC IMPLEMENTATION
 * ========================================================================= */
typedef struct hcl_gchdr_t hcl_gchdr_t;
struct hcl_gchdr_t
{
        hcl_gchdr_t* next;
};
/* The size of hcl_gchdr_t must be aligned to HCL_SIZEOF_OOP_T */

/* =========================================================================
 * OBJECT STRUCTURE
 * ========================================================================= */
enum hcl_obj_type_t
{
	HCL_OBJ_TYPE_OOP,
	HCL_OBJ_TYPE_CHAR,
	HCL_OBJ_TYPE_BYTE,
	HCL_OBJ_TYPE_HALFWORD,
	HCL_OBJ_TYPE_WORD

/*
	HCL_OBJ_TYPE_UINT8,
	HCL_OBJ_TYPE_UINT16,
	HCL_OBJ_TYPE_UINT32,
*/

/* NOTE: you can have HCL_OBJ_SHORT, HCL_OBJ_INT
 * HCL_OBJ_LONG, HCL_OBJ_FLOAT, HCL_OBJ_DOUBLE, etc
 * type field being 6 bits long, you can have up to 64 different types.

	HCL_OBJ_TYPE_SHORT,
	HCL_OBJ_TYPE_INT,
	HCL_OBJ_TYPE_LONG,
	HCL_OBJ_TYPE_FLOAT,
	HCL_OBJ_TYPE_DOUBLE */
};
typedef enum hcl_obj_type_t hcl_obj_type_t;

/* =========================================================================
 * Object header structure
 *
 * _flags:
 *   type: the type of a payload item.
 *         one of HCL_OBJ_TYPE_OOP, HCL_OBJ_TYPE_CHAR,
 *                HCL_OBJ_TYPE_BYTE, HCL_OBJ_TYPE_HALFWORD, HCL_OBJ_TYPE_WORD
 *   unit: the size of a payload item in bytes.
 *   extra: 0 or 1. 1 indicates that the payload contains 1 more
 *          item than the value of the size field. used for a
 *          terminating null in a variable-char object. internel
 *          use only.
 *   kernel: 0 - ordinary object.
 *           1 - kernel object. can survive hcl_reset().
 *           2 - kernel object. can survive hcl_reset().
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
 *    sizeof(hcl_obj_t) + ALIGN((size + extra) * unit), sizeof(hcl_oop_t))
 *
 * If the type is known to be not HCL_OBJ_TYPE_CHAR, you can assume that
 * 'extra' is 0. So you can simplify the fomula in such a context.
 *    sizeof(hcl_obj_t) + ALIGN(size * unit), sizeof(hcl_oop_t))
 *
 * The ALIGN() macro is used above since allocation adjusts the payload
 * size to a multiple of sizeof(hcl_oop_t). it assumes that sizeof(hcl_obj_t)
 * is a multiple of sizeof(hcl_oop_t). See the HCL_BYTESOF() macro.
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
#define HCL_OBJ_FLAGS_TYPE_BITS      (6) /* 6 */
#define HCL_OBJ_FLAGS_UNIT_BITS      (5) /* 11 */
#define HCL_OBJ_FLAGS_EXTRA_BITS     (1) /* 12 */
#define HCL_OBJ_FLAGS_KERNEL_BITS    (2) /* 14 */
#define HCL_OBJ_FLAGS_MOVED_BITS     (2) /* 16 */
#define HCL_OBJ_FLAGS_NGC_BITS       (1) /* 17 */
#define HCL_OBJ_FLAGS_TRAILER_BITS   (1) /* 18 */
#define HCL_OBJ_FLAGS_SYNCODE_BITS   (5) /* 23 - syncode for symbol, concode for cons */
#define HCL_OBJ_FLAGS_FLEXI_BITS     (1) /* 24 */
#define HCL_OBJ_FLAGS_RDONLY_BITS    (1) /* 25 */
#define HCL_OBJ_FLAGS_PROC_BITS      (1) /* 26 */

/*
#define HCL_OBJ_FLAGS_PERM_BITS       1
#define HCL_OBJ_FLAGS_MOVED_BITS      2
#define HCL_OBJ_FLAGS_GCFIN_BITS      4
#define HCL_OBJ_FLAGS_TRAILER_BITS    1
#define HCL_OBJ_FLAGS_HASH_BITS       2
#define HCL_OBJ_FLAGS_UNCOPYABLE_BITS 1
*/

#define HCL_OBJ_FLAGS_TYPE_SHIFT        (HCL_OBJ_FLAGS_UNIT_BITS      + HCL_OBJ_FLAGS_UNIT_SHIFT)
#define HCL_OBJ_FLAGS_UNIT_SHIFT        (HCL_OBJ_FLAGS_EXTRA_BITS     + HCL_OBJ_FLAGS_EXTRA_SHIFT)
#define HCL_OBJ_FLAGS_EXTRA_SHIFT       (HCL_OBJ_FLAGS_KERNEL_BITS    + HCL_OBJ_FLAGS_KERNEL_SHIFT)
#define HCL_OBJ_FLAGS_KERNEL_SHIFT      (HCL_OBJ_FLAGS_MOVED_BITS     + HCL_OBJ_FLAGS_MOVED_SHIFT)
#define HCL_OBJ_FLAGS_MOVED_SHIFT       (HCL_OBJ_FLAGS_NGC_BITS       + HCL_OBJ_FLAGS_NGC_SHIFT)
#define HCL_OBJ_FLAGS_NGC_SHIFT         (HCL_OBJ_FLAGS_TRAILER_BITS   + HCL_OBJ_FLAGS_TRAILER_SHIFT)
#define HCL_OBJ_FLAGS_TRAILER_SHIFT     (HCL_OBJ_FLAGS_SYNCODE_BITS   + HCL_OBJ_FLAGS_SYNCODE_SHIFT)
#define HCL_OBJ_FLAGS_SYNCODE_SHIFT     (HCL_OBJ_FLAGS_FLEXI_BITS     + HCL_OBJ_FLAGS_FLEXI_SHIFT)
#define HCL_OBJ_FLAGS_FLEXI_SHIFT       (HCL_OBJ_FLAGS_RDONLY_BITS    + HCL_OBJ_FLAGS_RDONLY_SHIFT)
#define HCL_OBJ_FLAGS_RDONLY_SHIFT      (HCL_OBJ_FLAGS_PROC_BITS      + HCL_OBJ_FLAGS_PROC_SHIFT)
#define HCL_OBJ_FLAGS_PROC_SHIFT        (0)

#define HCL_OBJ_GET_FLAGS_TYPE(oop)        HCL_GETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_TYPE_SHIFT,      HCL_OBJ_FLAGS_TYPE_BITS)
#define HCL_OBJ_GET_FLAGS_UNIT(oop)        HCL_GETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_UNIT_SHIFT,      HCL_OBJ_FLAGS_UNIT_BITS)
#define HCL_OBJ_GET_FLAGS_EXTRA(oop)       HCL_GETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_EXTRA_SHIFT,     HCL_OBJ_FLAGS_EXTRA_BITS)
#define HCL_OBJ_GET_FLAGS_KERNEL(oop)      HCL_GETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_KERNEL_SHIFT,    HCL_OBJ_FLAGS_KERNEL_BITS)
#define HCL_OBJ_GET_FLAGS_MOVED(oop)       HCL_GETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_MOVED_SHIFT,     HCL_OBJ_FLAGS_MOVED_BITS)
#define HCL_OBJ_GET_FLAGS_NGC(oop)         HCL_GETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_NGC_SHIFT,       HCL_OBJ_FLAGS_NGC_BITS)
#define HCL_OBJ_GET_FLAGS_TRAILER(oop)     HCL_GETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_TRAILER_SHIFT,   HCL_OBJ_FLAGS_TRAILER_BITS)
#define HCL_OBJ_GET_FLAGS_SYNCODE(oop)     HCL_GETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_SYNCODE_SHIFT,   HCL_OBJ_FLAGS_SYNCODE_BITS)
#define HCL_OBJ_GET_FLAGS_FLEXI(oop)       HCL_GETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_FLEXI_SHIFT,     HCL_OBJ_FLAGS_FLEXI_BITS)
#define HCL_OBJ_GET_FLAGS_RDONLY(oop)      HCL_GETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_RDONLY_SHIFT,    HCL_OBJ_FLAGS_RDONLY_BITS)
#define HCL_OBJ_GET_FLAGS_PROC(oop)        HCL_GETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_PROC_SHIFT,      HCL_OBJ_FLAGS_PROC_BITS)

#define HCL_OBJ_SET_FLAGS_TYPE(oop,v)      HCL_SETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_TYPE_SHIFT,      HCL_OBJ_FLAGS_TYPE_BITS,      v)
#define HCL_OBJ_SET_FLAGS_UNIT(oop,v)      HCL_SETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_UNIT_SHIFT,      HCL_OBJ_FLAGS_UNIT_BITS,      v)
#define HCL_OBJ_SET_FLAGS_EXTRA(oop,v)     HCL_SETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_EXTRA_SHIFT,     HCL_OBJ_FLAGS_EXTRA_BITS,     v)
#define HCL_OBJ_SET_FLAGS_KERNEL(oop,v)    HCL_SETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_KERNEL_SHIFT,    HCL_OBJ_FLAGS_KERNEL_BITS,    v)
#define HCL_OBJ_SET_FLAGS_MOVED(oop,v)     HCL_SETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_MOVED_SHIFT,     HCL_OBJ_FLAGS_MOVED_BITS,     v)
#define HCL_OBJ_SET_FLAGS_NGC(oop,v)       HCL_SETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_NGC_SHIFT,       HCL_OBJ_FLAGS_NGC_BITS,       v)
#define HCL_OBJ_SET_FLAGS_TRAILER(oop,v)   HCL_SETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_TRAILER_SHIFT,   HCL_OBJ_FLAGS_TRAILER_BITS,   v)
#define HCL_OBJ_SET_FLAGS_SYNCODE(oop,v)   HCL_SETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_SYNCODE_SHIFT,   HCL_OBJ_FLAGS_SYNCODE_BITS,   v)
#define HCL_OBJ_SET_FLAGS_FLEXI(oop,v)     HCL_SETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_FLEXI_SHIFT,     HCL_OBJ_FLAGS_FLEXI_BITS, v)
#define HCL_OBJ_SET_FLAGS_RDONLY(oop,v)    HCL_SETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_RDONLY_SHIFT,    HCL_OBJ_FLAGS_RDONLY_BITS, v)
#define HCL_OBJ_SET_FLAGS_PROC(oop,v)      HCL_SETBITS(hcl_oow_t, (oop)->_flags, HCL_OBJ_FLAGS_PROC_SHIFT,      HCL_OBJ_FLAGS_PROC_BITS, v)

/* [NOTE] this macro doesn't check the range of the actual value.
 *        make sure that the value of each bit fields given falls within the
 *        possible range of the defined bits */
#define HCL_OBJ_MAKE_FLAGS(t,u,e,k,m,g,r) ( \
	(((hcl_oow_t)(t)) << HCL_OBJ_FLAGS_TYPE_SHIFT) | \
	(((hcl_oow_t)(u)) << HCL_OBJ_FLAGS_UNIT_SHIFT) | \
	(((hcl_oow_t)(e)) << HCL_OBJ_FLAGS_EXTRA_SHIFT) | \
	(((hcl_oow_t)(k)) << HCL_OBJ_FLAGS_KERNEL_SHIFT) | \
	(((hcl_oow_t)(m)) << HCL_OBJ_FLAGS_MOVED_SHIFT) | \
	(((hcl_oow_t)(g)) << HCL_OBJ_FLAGS_NGC_SHIFT) | \
	(((hcl_oow_t)(r)) << HCL_OBJ_FLAGS_TRAILER_SHIFT) \
)

#define HCL_OBJ_FLAGS_KERNEL_USER     0  /* not a kernel object */
#define HCL_OBJ_FLAGS_KERNEL_IMMATURE 1  /* incomplete kernel object. defined in gc.c for bootstrapping. but no complete class definition has been read */
#define HCL_OBJ_FLAGS_KERNEL_MATURE   2  /* kernel  object with its full class defintion read in */

#define HCL_OBJ_GET_SIZE(oop) ((oop)->_size)
#define HCL_OBJ_GET_CLASS(oop) ((oop)->_class)

#define HCL_OBJ_SET_SIZE(oop,v) ((oop)->_size = (v))
#define HCL_OBJ_SET_CLASS(oop,c) ((oop)->_class = (c))

/* [NOTE] this macro doesn't include the size of the trailer */
#define HCL_OBJ_BYTESOF(oop) ((HCL_OBJ_GET_SIZE(oop) + HCL_OBJ_GET_FLAGS_EXTRA(oop)) * HCL_OBJ_GET_FLAGS_UNIT(oop))

#define HCL_OBJ_IS_OOP_POINTER(oop)      (HCL_OOP_IS_POINTER(oop) && (HCL_OBJ_GET_FLAGS_TYPE(oop) == HCL_OBJ_TYPE_OOP))
#define HCL_OBJ_IS_CHAR_POINTER(oop)     (HCL_OOP_IS_POINTER(oop) && (HCL_OBJ_GET_FLAGS_TYPE(oop) == HCL_OBJ_TYPE_CHAR))
#define HCL_OBJ_IS_BYTE_POINTER(oop)     (HCL_OOP_IS_POINTER(oop) && (HCL_OBJ_GET_FLAGS_TYPE(oop) == HCL_OBJ_TYPE_BYTE))
#define HCL_OBJ_IS_HALFWORD_POINTER(oop) (HCL_OOP_IS_POINTER(oop) && (HCL_OBJ_GET_FLAGS_TYPE(oop) == HCL_OBJ_TYPE_HALFWORD))
#define HCL_OBJ_IS_WORD_POINTER(oop)     (HCL_OOP_IS_POINTER(oop) && (HCL_OBJ_GET_FLAGS_TYPE(oop) == HCL_OBJ_TYPE_WORD))

#define HCL_STORE_OOP(hcl,var,val) (*(var) = val)

#define HCL_OBJ_HEADER \
	hcl_oow_t _flags; \
	hcl_oow_t _size; \
	hcl_oop_t _class

struct hcl_obj_t
{
	HCL_OBJ_HEADER;
};

struct hcl_obj_oop_t
{
	HCL_OBJ_HEADER;
	hcl_oop_t slot[1];
};

struct hcl_obj_char_t
{
	HCL_OBJ_HEADER;
	hcl_ooch_t slot[1];
};

struct hcl_obj_byte_t
{
	HCL_OBJ_HEADER;
	hcl_oob_t slot[1];
};

struct hcl_obj_halfword_t
{
	HCL_OBJ_HEADER;
	hcl_oohw_t slot[1];
};

struct hcl_obj_word_t
{
	HCL_OBJ_HEADER;
	hcl_oow_t slot[1];
};

#define HCL_OBJ_GET_OOP_SLOT(oop)      (((hcl_oop_oop_t)(oop))->slot)
#define HCL_OBJ_GET_CHAR_SLOT(oop)     (((hcl_oop_char_t)(oop))->slot)
#define HCL_OBJ_GET_BYTE_SLOT(oop)     (((hcl_oop_byte_t)(oop))->slot)
#define HCL_OBJ_GET_HALFWORD_SLOT(oop) (((hcl_oop_halfword_t)(oop))->slot)
#define HCL_OBJ_GET_WORD_SLOT(oop)     (((hcl_oop_word_t)(oop))->slot)
#define HCL_OBJ_GET_LIWORD_SLOT(oop)   (((hcl_oop_liword_t)(oop))->slot)

#define HCL_OBJ_GET_OOP_PTR(oop,idx)      (&(((hcl_oop_oop_t)(oop))->slot)[idx])
#define HCL_OBJ_GET_CHAR_PTR(oop,idx)     (&(((hcl_oop_char_t)(oop))->slot)[idx])
#define HCL_OBJ_GET_BYTE_PTR(oop,idx)     (&(((hcl_oop_byte_t)(oop))->slot)[idx])
#define HCL_OBJ_GET_HALFWORD_PTR(oop,idx) (&(((hcl_oop_halfword_t)(oop))->slot)[idx])
#define HCL_OBJ_GET_WORD_PTR(oop,idx)     (&(((hcl_oop_word_t)(oop))->slot)[idx])
#define HCL_OBJ_GET_LIWORD_PTR(oop,idx)   (&(((hcl_oop_liword_t)(oop))->slot)[idx])

#define HCL_OBJ_GET_OOP_VAL(oop,idx)      ((((hcl_oop_oop_t)(oop))->slot)[idx])
#define HCL_OBJ_GET_CHAR_VAL(oop,idx)     ((((hcl_oop_char_t)(oop))->slot)[idx])
#define HCL_OBJ_GET_BYTE_VAL(oop,idx)     ((((hcl_oop_byte_t)(oop))->slot)[idx])
#define HCL_OBJ_GET_HALFWORD_VAL(oop,idx) ((((hcl_oop_halfword_t)(oop))->slot)[idx])
#define HCL_OBJ_GET_WORD_VAL(oop,idx)     ((((hcl_oop_word_t)(oop))->slot)[idx])
#define HCL_OBJ_GET_LIWORD_VAL(oop,idx)   ((((hcl_oop_liword_t)(oop))->slot)[idx])

#define HCL_OBJ_SET_OOP_VAL(oop,idx,val)      ((((hcl_oop_oop_t)(oop))->slot)[idx] = (val))
#define HCL_OBJ_SET_CHAR_VAL(oop,idx,val)     ((((hcl_oop_char_t)(oop))->slot)[idx] = (val))
#define HCL_OBJ_SET_BYTE_VAL(oop,idx,val)     ((((hcl_oop_byte_t)(oop))->slot)[idx] = (val))
#define HCL_OBJ_SET_HALFWORD_VAL(oop,idx,val) ((((hcl_oop_halfword_t)(oop))->slot)[idx] = (val))
#define HCL_OBJ_SET_WORD_VAL(oop,idx,val)     ((((hcl_oop_word_t)(oop))->slot)[idx] = (val))
#define HCL_OBJ_SET_LIWORD_VAL(oop,idx,val)   ((((hcl_oop_liword_t)(oop))->slot)[idx] = (val))

typedef struct hcl_trailer_t hcl_trailer_t;
struct hcl_trailer_t
{
	hcl_oow_t size;
	hcl_oob_t slot[1];
};

#define HCL_OBJ_GET_TRAILER_BYTE(oop) ((hcl_oob_t*)&((hcl_oop_oop_t)oop)->slot[HCL_OBJ_GET_SIZE(oop) + 1])
#define HCL_OBJ_GET_TRAILER_SIZE(oop) ((hcl_oow_t)((hcl_oop_oop_t)oop)->slot[HCL_OBJ_GET_SIZE(oop)])

#define HCL_PRIM_NAMED_INSTVARS 4
typedef struct hcl_prim_t hcl_prim_t;
typedef struct hcl_prim_t* hcl_oop_prim_t;
struct hcl_prim_t
{
	HCL_OBJ_HEADER;
	hcl_oow_t impl;
	hcl_oow_t min_nargs;
	hcl_oow_t max_nargs;
	hcl_oow_t mod;
};

#define HCL_CONS_NAMED_INSTVARS 2
typedef struct hcl_cons_t hcl_cons_t;
typedef struct hcl_cons_t* hcl_oop_cons_t;
struct hcl_cons_t
{
	HCL_OBJ_HEADER;
	hcl_oop_t car;
	hcl_oop_t cdr;
};

#define HCL_DIC_NAMED_INSTVARS 2
typedef struct hcl_dic_t hcl_dic_t;
typedef struct hcl_dic_t* hcl_oop_dic_t;
struct hcl_dic_t
{
	HCL_OBJ_HEADER;
	hcl_oop_t     tally;  /* SmallInteger */
	hcl_oop_oop_t bucket; /* Array */
};

#define HCL_FPDEC_NAMED_INSTVARS 2
typedef struct hcl_fpdec_t hcl_fpdec_t;
typedef struct hcl_fpdec_t* hcl_oop_fpdec_t;
struct hcl_fpdec_t
{
	HCL_OBJ_HEADER;
	hcl_oop_t value; /* smooi or bigint */
	hcl_oop_t scale; /* smooi, positive */
};

/* the first byte after the main payload is the trailer size
 * the code bytes are placed after the trailer size.
 *
 * code bytes -> ((hcl_oob_t*)&((hcl_oop_oop_t)m)->slot[HCL_OBJ_GET_SIZE(m) + 1]) or
 *               ((hcl_oob_t*)&((hcl_oop_function_t)m)->literal_frame[HCL_OBJ_GET_SIZE(m) + 1 - HCL_METHOD_NAMED_INSTVARS])
 * size -> ((hcl_oow_t)((hcl_oop_oop_t)m)->slot[HCL_OBJ_GET_SIZE(m)])*/
#define HCL_FUNCTION_GET_CODE_BYTE(m) HCL_OBJ_GET_TRAILER_BYTE(m)
#define HCL_FUNCTION_GET_CODE_SIZE(m) HCL_OBJ_GET_TRAILER_SIZE(m)

#define HCL_FUNCTION_NAMED_INSTVARS 3   /* this excludes literal frames and byte codes */
typedef struct hcl_function_t hcl_function_t;
typedef struct hcl_function_t* hcl_oop_function_t;

#define HCL_BLOCK_NAMED_INSTVARS 3
typedef struct hcl_block_t hcl_block_t;
typedef struct hcl_block_t* hcl_oop_block_t;

#define HCL_CONTEXT_NAMED_INSTVARS 10
typedef struct hcl_context_t hcl_context_t;
typedef struct hcl_context_t* hcl_oop_context_t;

struct hcl_function_t
{
	HCL_OBJ_HEADER;

	hcl_oop_t         attr_mask; /* smooi */
	hcl_oop_context_t home; /* home context. nil for the initial function */

	hcl_oop_t dbgi; /* byte array containing debug information. nil if not available */

	/* == variable indexed part == */
	hcl_oop_t literal_frame[1]; /* it stores literals. it may not exist */

	/* after the literal frame comes the actual byte code */
};

/* hcl_function_t copies the byte codes and literal frames into itself
 * hcl_block_t contains minimal information(ip) for referening byte codes
 * and literal frames available in home->origin. it represents the compiled block.
 */
struct hcl_block_t
{
	HCL_OBJ_HEADER;

	hcl_oop_t         attr_mask; /* smooi */
	hcl_oop_context_t home; /* home context */
	hcl_oop_t         ip; /* smooi. instruction pointer where the byte code begins in home->base */
};

struct hcl_context_t
{
	HCL_OBJ_HEADER;

	/* SmallInteger */
	hcl_oop_t          req_nrets;

	/* SmallInteger. */
	hcl_oop_t          attr_mask;

	/* SmallInteger, instruction pointer */
	hcl_oop_t          ip;

	/* the initial context is created with the initial function object in this field.
	 * a function-based context is created with the activating function object.
	 * a block-based context is created with the function object that the base field of
	 * the home context of the activating block context points to. */
	hcl_oop_function_t base; /* function */

	/* it points to the active context at the moment when
	 * this context object has been activated. a new method context
	 * is activated as a result of normal message sending and a block
	 * context is activated when it is sent 'value'. it's set to
	 * nil if a block context created hasn't received 'value'. */
	hcl_oop_context_t  sender; /* context or nil */

	/* it points to the receiver of the message for a method context.
	 * a block context points to a block object and a function context
	 * points to a function object */
	hcl_oop_t          receiver;

	/* for a block context, it points to the active context at the
	 * moment the block context was created. that is, it points to
	 * a method context where the base block has been defined.
	 * an activated block context copies this field from the base block context. */
	hcl_oop_context_t  home; /* context or nil */

	/* it is set to itself for a method context, nil for other contexts.
	 * TODO: this field may not be needed.. mthhome access has been commented out.. so remove this field */
	hcl_oop_context_t mthhome;

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
	hcl_oop_t          ivaroff;

	/* method owner if this context is created of a message send. nil otherwise */
	hcl_oop_t          owner;  /* class(hcl_oop_class_t) or nil */

	/* variable indexed part */
	hcl_oop_t          slot[1]; /* arguments, return variables, local variables, other arguments, etc */
};

#define HCL_PROCESS_NAMED_INSTVARS (15)
typedef struct hcl_process_t hcl_process_t;
typedef struct hcl_process_t* hcl_oop_process_t;

#define HCL_SEMAPHORE_NAMED_INSTVARS (11)
typedef struct hcl_semaphore_t hcl_semaphore_t;
typedef struct hcl_semaphore_t* hcl_oop_semaphore_t;

#define HCL_SEMAPHORE_GROUP_NAMED_INSTVARS (8)
typedef struct hcl_semaphore_group_t hcl_semaphore_group_t;
typedef struct hcl_semaphore_group_t* hcl_oop_semaphore_group_t;


#define HCL_PROCESS_STATE_RUNNING (3)
#define HCL_PROCESS_STATE_WAITING (2)
#define HCL_PROCESS_STATE_RUNNABLE (1)
#define HCL_PROCESS_STATE_SUSPENDED (0)
#define HCL_PROCESS_STATE_TERMINATED (-1)

struct hcl_process_t
{
	HCL_OBJ_HEADER;
	hcl_oop_context_t initial_context;
	hcl_oop_context_t current_context;

	hcl_oop_t         id; /* SmallInteger */
	hcl_oop_t         state; /* SmallInteger */

	hcl_oop_t         sp;   /* stack pointer. SmallInteger */
	hcl_oop_t         st;   /* stack top */

	hcl_oop_t         exsp; /* exception stack pointer. SmallInteger */
	hcl_oop_t         exst; /* exception stack top */

	hcl_oop_t         clsp; /* class stack pointer */
	hcl_oop_t         clst; /* class stack  top */

	struct
	{
		hcl_oop_process_t prev;
		hcl_oop_process_t next;
	} ps;  /* links to use with the process scheduler */

	struct
	{
		hcl_oop_process_t prev;
		hcl_oop_process_t next;
	} sem_wait; /* links to use with a semaphore */

	hcl_oop_t sem; /* nil, semaphore, or semaphore group */

	/* == variable indexed part == */
	hcl_oop_t slot[1]; /* process stack */

	/* after the process stack comes the exception stack.
	 * the exception stack is composed of instruction pointers and some context values.
	 * the instruction pointers are OOPs of small integers. safe without GC.
	 * the context values must be referenced by the active call chain. GC doesn't need to scan this area.
	 * If this assumption is not correct, GC code must be modified.
	 * so the garbage collector is free to ignore the exception stack */
};

enum hcl_semaphore_subtype_t
{
	HCL_SEMAPHORE_SUBTYPE_TIMED = 0,
	HCL_SEMAPHORE_SUBTYPE_IO    = 1
};
typedef enum hcl_semaphore_subtype_t hcl_semaphore_subtype_t;

enum hcl_semaphore_io_type_t
{
	HCL_SEMAPHORE_IO_TYPE_INPUT   = 0,
	HCL_SEMAPHORE_IO_TYPE_OUTPUT  = 1
};
typedef enum hcl_semaphore_io_type_t hcl_semaphore_io_type_t;

enum hcl_semaphore_io_mask_t
{
	HCL_SEMAPHORE_IO_MASK_INPUT   = (1 << 0),
	HCL_SEMAPHORE_IO_MASK_OUTPUT  = (1 << 1),
	HCL_SEMAPHORE_IO_MASK_HANGUP  = (1 << 2),
	HCL_SEMAPHORE_IO_MASK_ERROR   = (1 << 3)
};
typedef enum hcl_semaphore_io_mask_t hcl_semaphore_io_mask_t;

struct hcl_semaphore_t
{
	HCL_OBJ_HEADER;

	/* [IMPORTANT] make sure that the position of 'waiting' in hcl_semaphore_t
	 *             must be exactly the same as its position in hcl_semaphore_group_t */
	struct
	{
		hcl_oop_process_t first;
		hcl_oop_process_t last;
	} waiting; /* list of processes waiting on this semaphore */
	/* [END IMPORTANT] */

	hcl_oop_t count; /* SmallInteger */

	/* nil for normal. SmallInteger if associated with
	 * timer(HCL_SEMAPHORE_SUBTYPE_TIMED) or IO(HCL_SEMAPHORE_SUBTYPE_IO). */
	hcl_oop_t subtype;

	union
	{
		struct
		{
			hcl_oop_t index; /* index to the heap that stores timed semaphores */
			hcl_oop_t ftime_sec; /* firing time */
			hcl_oop_t ftime_nsec; /* firing time */
		} timed;

		struct
		{
			hcl_oop_t index; /* index to sem_io_tuple */
			hcl_oop_t handle;
			hcl_oop_t type; /* SmallInteger */
		} io;
	} u;

	hcl_oop_t signal_action;

	hcl_oop_semaphore_group_t group; /* nil or belonging semaphore group */
	struct
	{
		hcl_oop_semaphore_t prev;
		hcl_oop_semaphore_t next;
	} grm; /* group membership chain */
};

#define HCL_SEMAPHORE_GROUP_SEMS_UNSIG 0
#define HCL_SEMAPHORE_GROUP_SEMS_SIG   1

struct hcl_semaphore_group_t
{
	HCL_OBJ_HEADER;

	/* [IMPORTANT] make sure that the position of 'waiting' in hcl_semaphore_group_t
	 *             must be exactly the same as its position in hcl_semaphore_t */
	struct
	{
		hcl_oop_process_t first;
		hcl_oop_process_t last;
	} waiting; /* list of processes waiting on this semaphore group */
	/* [END IMPORTANT] */

	struct
	{
		hcl_oop_semaphore_t first;
		hcl_oop_semaphore_t last;
	} sems[2]; /* sems[0] - unsignaled semaphores, sems[1] - signaled semaphores */

	hcl_oop_t sem_io_count; /* the number of io semaphores in the group */
	hcl_oop_t sem_count; /* the total number of semaphores in the group */
};

#define HCL_PROCESS_SCHEDULER_NAMED_INSTVARS 8
typedef struct hcl_process_scheduler_t hcl_process_scheduler_t;
typedef struct hcl_process_scheduler_t* hcl_oop_process_scheduler_t;
struct hcl_process_scheduler_t
{
	HCL_OBJ_HEADER;

	hcl_oop_process_t active; /*  pointer to an active process in the runnable process list */
	hcl_oop_t total_count;  /* smooi, total number of processes - runnable/running/suspended */

	struct
	{
		hcl_oop_t         count; /* smooi, the number of runnable/running processes */
		hcl_oop_process_t first; /* runnable process list */
		hcl_oop_process_t last; /* runnable process list */
	} runnable;

	struct
	{
		hcl_oop_t         count; /* smooi, the number of suspended processes */
		hcl_oop_process_t first; /* suspended process list */
		hcl_oop_process_t last; /* suspended process list */
	} suspended;
};


#define HCL_CLASS_NAMED_INSTVARS 9
typedef struct hcl_class_t hcl_class_t;
typedef struct hcl_class_t* hcl_oop_class_t;
struct hcl_class_t
{
	HCL_OBJ_HEADER;

	hcl_oop_t name; /* class name. nil for unnamed class */
	hcl_oop_t mdic; /* method dictionary. nil or a dictionary object */

	hcl_oop_t spec;     /* SmallInteger. instance specification */
	hcl_oop_t selfspec; /* SmallInteger. specification of the class object itself */

	hcl_oop_t superclass;
	hcl_oop_t nivars_super; /* SmallInteger */
	hcl_oop_t ibrand; /* SmallInteger */

	hcl_oop_char_t ivarnames;
	hcl_oop_char_t cvarnames;

	/* indexed part afterwards - not included in HCL_CLASS_NAMED_INSTVARS */
	hcl_oop_t      cvar[1];   /* class variables. */
};

#if 0
struct hcl_class_t
{
	HCL_OBJ_HEADER;

	/* === the following five fields must be in sync with hcl_methowner_t === */
	hcl_oop_char_t name; /* Symbol */

	/* [0] - instance methods, MethodDictionary
	 * [1] - class methods, MethodDictionary */
	hcl_oop_dic_t  mthdic[2];

	hcl_oop_nsdic_t nsup; /* pointer to the upper namespace */
	hcl_oop_nsdic_t nsdic; /* dictionary used for namespacing - may be nil when there are no subitems underneath */
	/* ===================================================================== */

	hcl_oop_t      spec;          /* SmallInteger. instance specification */
	hcl_oop_t      selfspec;      /* SmallInteger. specification of the class object itself */

	hcl_oop_t      superclass;    /* Another class */
	hcl_oop_t      subclasses;    /* Array of subclasses */

	hcl_oop_t      modname;       /* Symbol if importing a module. nil if not. */

	/* == NEVER CHANGE THIS ORDER OF 3 ITEMS BELOW == */
	hcl_oop_char_t ivars;  /* String */
	hcl_oop_char_t civars; /* String */
	hcl_oop_char_t cvars;  /* String */
	/* == NEVER CHANGE THE ORDER OF 3 ITEMS ABOVE == */

#if 0
	hcl_oop_char_t pooldics;      /* String - pool dictionaries imported */

	hcl_oop_t      trsize; /* trailer size for new instances */
	hcl_oop_t      trgc; /* trailer gc callback */

	/* [0] - initial values for instance variables of new instances
	 * [1] - initial values for class instance variables */
	hcl_oop_t      initv[2];
#endif
	/* indexed part afterwards */
	hcl_oop_t      cvar[1];   /* class instance variables and class variables. */
};
#endif

/**
 * The HCL_CLASSOF() macro return the class of an object including a numeric
 * object encoded into a pointer.
 */
#define HCL_CLASSOF(hcl,oop) \
	(HCL_OOP_GET_TAG(oop)? ((hcl_oop_t)(*(hcl)->tagged_classes[HCL_OOP_GET_TAG(oop)])): HCL_OBJ_GET_CLASS(oop))


/**
 * The HCL_BYTESOF() macro returns the size of the payload of
 * an object in bytes. If the pointer given encodes a numeric value,
 * it returns the size of #hcl_oow_t in bytes.
 */
#define HCL_BYTESOF(hcl,oop) \
	(HCL_OOP_IS_NUMERIC(oop)? HCL_SIZEOF(hcl_oow_t): HCL_OBJ_BYTESOF(oop))


/**
 * The HCL_ISTYPEOF() macro is a safe replacement for HCL_OBJ_GET_FLAGS_TYPE()
 */
#define HCL_ISTYPEOF(hcl,oop,type) \
	(!HCL_OOP_IS_NUMERIC(oop) && HCL_OBJ_GET_FLAGS_TYPE(oop) == (type))

/* =========================================================================
 * HEAP
 * ========================================================================= */

typedef struct hcl_heap_t hcl_heap_t;

struct hcl_heap_t
{
	hcl_uint8_t* base;  /* start of a heap */
	hcl_oow_t    size;
	hcl_xma_t*   xma;
	hcl_mmgr_t   xmmgr;
};

/* =========================================================================
 * VM LOGGING
 * ========================================================================= */

enum hcl_log_mask_t
{
	HCL_LOG_DEBUG       = ((hcl_bitmask_t)1 << 0),
	HCL_LOG_INFO        = ((hcl_bitmask_t)1 << 1),
	HCL_LOG_WARN        = ((hcl_bitmask_t)1 << 2),
	HCL_LOG_ERROR       = ((hcl_bitmask_t)1 << 3),
	HCL_LOG_FATAL       = ((hcl_bitmask_t)1 << 4),

	HCL_LOG_UNTYPED     = ((hcl_bitmask_t)1 << 6), /* only to be used by HCL_DEBUGx() and HCL_INFOx() */
	HCL_LOG_COMPILER    = ((hcl_bitmask_t)1 << 7),
	HCL_LOG_VM          = ((hcl_bitmask_t)1 << 8),
	HCL_LOG_MNEMONIC    = ((hcl_bitmask_t)1 << 9), /* bytecode mnemonic */
	HCL_LOG_GC          = ((hcl_bitmask_t)1 << 10),
	HCL_LOG_IC          = ((hcl_bitmask_t)1 << 11), /* instruction cycle, fetch-decode-execute */
	HCL_LOG_PRIMITIVE   = ((hcl_bitmask_t)1 << 12),

	HCL_LOG_APP         = ((hcl_bitmask_t)1 << 13), /* hcl applications, set by hcl logging primitive */
	HCL_LOG_APP_X1      = ((hcl_bitmask_t)1 << 14), /* more hcl applications, you can choose to use one of APP_X? randomly */
	HCL_LOG_APP_X2      = ((hcl_bitmask_t)1 << 15),
	HCL_LOG_APP_X3      = ((hcl_bitmask_t)1 << 16),

	HCL_LOG_ALL_LEVELS  = (HCL_LOG_DEBUG  | HCL_LOG_INFO | HCL_LOG_WARN | HCL_LOG_ERROR | HCL_LOG_FATAL),
	HCL_LOG_ALL_TYPES   = (HCL_LOG_UNTYPED | HCL_LOG_COMPILER | HCL_LOG_VM | HCL_LOG_MNEMONIC | HCL_LOG_GC | HCL_LOG_IC | HCL_LOG_PRIMITIVE | HCL_LOG_APP | HCL_LOG_APP_X1 | HCL_LOG_APP_X2 | HCL_LOG_APP_X3),


	HCL_LOG_STDOUT      = ((hcl_bitmask_t)1 << 20),  /* write log messages to stdout without timestamp. HCL_LOG_STDOUT wins over HCL_LOG_STDERR. */
	HCL_LOG_STDERR      = ((hcl_bitmask_t)1 << 21),  /* write log messages to stderr without timestamp. */

	HCL_LOG_PREFER_JSON = ((hcl_bitmask_t)1 << 30)   /* write a object in the json format. don't set this explicitly. use %J instead */
};
typedef enum hcl_log_mask_t hcl_log_mask_t;

/* all bits must be set to get enabled */
#define HCL_LOG_ENABLED(hcl,mask) (((hcl)->option.log_mask & (mask)) == (mask))

#define HCL_LOG0(hcl,mask,fmt) do { if (HCL_LOG_ENABLED(hcl,mask)) hcl_logbfmt(hcl, mask, fmt); } while(0)
#define HCL_LOG1(hcl,mask,fmt,a1) do { if (HCL_LOG_ENABLED(hcl,mask)) hcl_logbfmt(hcl, mask, fmt, a1); } while(0)
#define HCL_LOG2(hcl,mask,fmt,a1,a2) do { if (HCL_LOG_ENABLED(hcl,mask)) hcl_logbfmt(hcl, mask, fmt, a1, a2); } while(0)
#define HCL_LOG3(hcl,mask,fmt,a1,a2,a3) do { if (HCL_LOG_ENABLED(hcl,mask)) hcl_logbfmt(hcl, mask, fmt, a1, a2, a3); } while(0)
#define HCL_LOG4(hcl,mask,fmt,a1,a2,a3,a4) do { if (HCL_LOG_ENABLED(hcl,mask)) hcl_logbfmt(hcl, mask, fmt, a1, a2, a3, a4); } while(0)
#define HCL_LOG5(hcl,mask,fmt,a1,a2,a3,a4,a5) do { if (HCL_LOG_ENABLED(hcl,mask)) hcl_logbfmt(hcl, mask, fmt, a1, a2, a3, a4, a5); } while(0)
#define HCL_LOG6(hcl,mask,fmt,a1,a2,a3,a4,a5,a6) do { if (HCL_LOG_ENABLED(hcl,mask)) hcl_logbfmt(hcl, mask, fmt, a1, a2, a3, a4, a5, a6); } while(0)
#define HCL_LOG7(hcl,mask,fmt,a1,a2,a3,a4,a5,a6,a7) do { if (HCL_LOG_ENABLED(hcl,mask)) hcl_logbfmt(hcl, mask, fmt, a1, a2, a3, a4, a5, a6, a7); } while(0)
#define HCL_LOG8(hcl,mask,fmt,a1,a2,a3,a4,a5,a6,a7,a8) do { if (HCL_LOG_ENABLED(hcl,mask)) hcl_logbfmt(hcl, mask, fmt, a1, a2, a3, a4, a5, a6, a7, a8); } while(0)
#define HCL_LOG9(hcl,mask,fmt,a1,a2,a3,a4,a5,a6,a7,a8,a9) do { if (HCL_LOG_ENABLED(hcl,mask)) hcl_logbfmt(hcl, mask, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9); } while(0)

#if defined(HCL_BUILD_RELEASE)
	/* [NOTE]
	 *  get rid of debugging message totally regardless of
	 *  the log mask in the release build.
	 */
#	define HCL_DEBUG0(hcl,fmt)
#	define HCL_DEBUG1(hcl,fmt,a1)
#	define HCL_DEBUG2(hcl,fmt,a1,a2)
#	define HCL_DEBUG3(hcl,fmt,a1,a2,a3)
#	define HCL_DEBUG4(hcl,fmt,a1,a2,a3,a4)
#	define HCL_DEBUG5(hcl,fmt,a1,a2,a3,a4,a5)
#	define HCL_DEBUG6(hcl,fmt,a1,a2,a3,a4,a5,a6)
#	define HCL_DEBUG7(hcl,fmt,a1,a2,a3,a4,a5,a6,a7)
#	define HCL_DEBUG8(hcl,fmt,a1,a2,a3,a4,a5,a6,a7,a8)
#	define HCL_DEBUG9(hcl,fmt,a1,a2,a3,a4,a5,a6,a7,a8,a9)
#else
#	define HCL_DEBUG0(hcl,fmt) HCL_LOG0(hcl, HCL_LOG_DEBUG | HCL_LOG_UNTYPED, fmt)
#	define HCL_DEBUG1(hcl,fmt,a1) HCL_LOG1(hcl, HCL_LOG_DEBUG | HCL_LOG_UNTYPED, fmt, a1)
#	define HCL_DEBUG2(hcl,fmt,a1,a2) HCL_LOG2(hcl, HCL_LOG_DEBUG | HCL_LOG_UNTYPED, fmt, a1, a2)
#	define HCL_DEBUG3(hcl,fmt,a1,a2,a3) HCL_LOG3(hcl, HCL_LOG_DEBUG | HCL_LOG_UNTYPED, fmt, a1, a2, a3)
#	define HCL_DEBUG4(hcl,fmt,a1,a2,a3,a4) HCL_LOG4(hcl, HCL_LOG_DEBUG | HCL_LOG_UNTYPED, fmt, a1, a2, a3, a4)
#	define HCL_DEBUG5(hcl,fmt,a1,a2,a3,a4,a5) HCL_LOG5(hcl, HCL_LOG_DEBUG | HCL_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5)
#	define HCL_DEBUG6(hcl,fmt,a1,a2,a3,a4,a5,a6) HCL_LOG6(hcl, HCL_LOG_DEBUG | HCL_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6)
#	define HCL_DEBUG7(hcl,fmt,a1,a2,a3,a4,a5,a6,a7) HCL_LOG7(hcl, HCL_LOG_DEBUG | HCL_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6, a7)
#	define HCL_DEBUG8(hcl,fmt,a1,a2,a3,a4,a5,a6,a7,a8) HCL_LOG8(hcl, HCL_LOG_DEBUG | HCL_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6, a7, a8)
#	define HCL_DEBUG9(hcl,fmt,a1,a2,a3,a4,a5,a6,a7,a8,a9) HCL_LOG9(hcl, HCL_LOG_DEBUG | HCL_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
#endif

#define HCL_INFO0(hcl,fmt) HCL_LOG0(hcl, HCL_LOG_INFO | HCL_LOG_UNTYPED, fmt)
#define HCL_INFO1(hcl,fmt,a1) HCL_LOG1(hcl, HCL_LOG_INFO | HCL_LOG_UNTYPED, fmt, a1)
#define HCL_INFO2(hcl,fmt,a1,a2) HCL_LOG2(hcl, HCL_LOG_INFO | HCL_LOG_UNTYPED, fmt, a1, a2)
#define HCL_INFO3(hcl,fmt,a1,a2,a3) HCL_LOG3(hcl, HCL_LOG_INFO | HCL_LOG_UNTYPED, fmt, a1, a2, a3)
#define HCL_INFO4(hcl,fmt,a1,a2,a3,a4) HCL_LOG4(hcl, HCL_LOG_INFO | HCL_LOG_UNTYPED, fmt, a1, a2, a3, a4)
#define HCL_INFO5(hcl,fmt,a1,a2,a3,a4,a5) HCL_LOG5(hcl, HCL_LOG_INFO | HCL_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5)
#define HCL_INFO6(hcl,fmt,a1,a2,a3,a4,a5,a6) HCL_LOG6(hcl, HCL_LOG_INFO | HCL_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6)
#define HCL_INFO7(hcl,fmt,a1,a2,a3,a4,a5,a6,a7) HCL_LOG7(hcl, HCL_LOG_INFO | HCL_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6, a7)
#define HCL_INFO8(hcl,fmt,a1,a2,a3,a4,a5,a6,a7,a8) HCL_LOG8(hcl, HCL_LOG_INFO | HCL_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6, a7, a8)
#define HCL_INFO9(hcl,fmt,a1,a2,a3,a4,a5,a6,a7,a8,a9) HCL_LOG9(hcl, HCL_LOG_INFO | HCL_LOG_UNTYPED, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)


/* =========================================================================
 * VIRTUAL MACHINE PRIMITIVES
 * ========================================================================= */

typedef void* (*hcl_alloc_heap_t) (
	hcl_t*             hcl,
	hcl_oow_t*         size /* [IN] requested size, [OUT] allocated size */
);

typedef void (*hcl_free_heap_t) (
	hcl_t*             hcl,
	void*              ptr
);

typedef void (*hcl_log_write_t) (
	hcl_t*             hcl,
	hcl_bitmask_t    mask,
	const hcl_ooch_t*  msg,
	hcl_oow_t          len
);

typedef hcl_errnum_t (*hcl_syserrstrb_t) (
	hcl_t*             hcl,
	int                syserr_type,
	int                syserr_code,
	hcl_bch_t*         buf,
	hcl_oow_t          len
);

typedef hcl_errnum_t (*hcl_syserrstru_t) (
	hcl_t*             hcl,
	int                syserr_type,
	int                syserr_code,
	hcl_uch_t*         buf,
	hcl_oow_t          len
);

typedef void (*hcl_assertfail_t) (
	hcl_t*             hcl,
	const hcl_bch_t*   expr,
	const hcl_bch_t*   file,
	hcl_oow_t          line
);

enum hcl_vmprim_dlopen_flag_t
{
	HCL_VMPRIM_DLOPEN_PFMOD = (1 << 0)
};
typedef enum hcl_vmprim_dlopen_flag_t hcl_vmprim_dlopen_flag_t;

typedef void (*hcl_vmprim_dlstartup_t) (
	hcl_t*             hcl
);

typedef void (*hcl_vmprim_dlcleanup_t) (
	hcl_t*             hcl
);

typedef void* (*hcl_vmprim_dlopen_t) (
	hcl_t*             hcl,
	const hcl_ooch_t*  name,
	int                flags
);

typedef void (*hcl_vmprim_dlclose_t) (
	hcl_t*             hcl,
	void*              handle
);

typedef void* (*hcl_vmprim_dlgetsym_t) (
	hcl_t*             hcl,
	void*              handle,
	const hcl_ooch_t*  name
);

typedef void (*hcl_vmprim_gettime_t) (
	hcl_t*             hcl,
	hcl_ntime_t*       now
);

typedef int (*hcl_vmprim_muxadd_t) (
	hcl_t*                  hcl,
	hcl_ooi_t               io_handle,
	hcl_ooi_t               masks
);

typedef int (*hcl_vmprim_muxmod_t) (
	hcl_t*                  hcl,
	hcl_ooi_t               io_handle,
	hcl_ooi_t               masks
);

typedef int (*hcl_vmprim_muxdel_t) (
	hcl_t*                  hcl,
	hcl_ooi_t               io_handle
);

typedef void (*hcl_vmprim_muxwait_cb_t) (
	hcl_t*                  hcl,
	hcl_ooi_t               io_handle,
	hcl_ooi_t               masks
);

typedef void (*hcl_vmprim_muxwait_t) (
	hcl_t*                  hcl,
	const hcl_ntime_t*      duration,
	hcl_vmprim_muxwait_cb_t muxwcb
);

typedef int (*hcl_vmprim_sleep_t) (
	hcl_t*             hcl,
	const hcl_ntime_t* duration
);

typedef hcl_ooi_t (*hcl_vmprim_getsigfd_t) (
	hcl_t*             hcl
);

typedef int (*hcl_vmprim_getsig_t) (
	hcl_t*             hcl,
	hcl_uint8_t*       sig
);

typedef int (*hcl_vmprim_setsig_t) (
	hcl_t*             hcl,
	hcl_uint8_t        sig
);

struct hcl_vmprim_t
{
	/* The alloc_heap callback function is called very earlier
	 * before hcl is fully initialized. so few features are availble
	 * in this callback function. If it's not provided, the default
	 * implementation is used. */
	hcl_alloc_heap_t       alloc_heap; /* optional */

	/* If you customize the heap allocator by providing the alloc_heap
	 * callback, you should implement the heap freer. otherwise the default
	 * implementation doesn't know how to free the heap allocated by
	 * the allocator callback. */
	hcl_free_heap_t        free_heap; /* optional */

	hcl_log_write_t        log_write; /* required */
	hcl_syserrstrb_t       syserrstrb; /* one of syserrstrb or syserrstru required */
	hcl_syserrstru_t       syserrstru;
	hcl_assertfail_t       assertfail;

	hcl_vmprim_dlstartup_t dl_startup; /* optional */
	hcl_vmprim_dlcleanup_t dl_cleanup; /* optional */
	hcl_vmprim_dlopen_t    dl_open; /* required */
	hcl_vmprim_dlclose_t   dl_close; /* required */
	hcl_vmprim_dlgetsym_t  dl_getsym; /* requried */

	hcl_vmprim_gettime_t   vm_gettime; /* required */
	hcl_vmprim_muxadd_t    vm_muxadd;
	hcl_vmprim_muxdel_t    vm_muxdel;
	hcl_vmprim_muxmod_t    vm_muxmod;
	hcl_vmprim_muxwait_t   vm_muxwait;
	hcl_vmprim_sleep_t     vm_sleep; /* required */

	hcl_vmprim_getsigfd_t  vm_getsigfd;
	hcl_vmprim_getsig_t    vm_getsig;
	hcl_vmprim_setsig_t    vm_setsig;
};

typedef struct hcl_vmprim_t hcl_vmprim_t;

/* =========================================================================
 * IO MANIPULATION
 * ========================================================================= */

enum hcl_io_cmd_t
{
	HCL_IO_OPEN,
	HCL_IO_CLOSE,
	HCL_IO_READ,
	HCL_IO_READ_BYTES,
	HCL_IO_WRITE,
	HCL_IO_WRITE_BYTES,
	HCL_IO_FLUSH
};
typedef enum hcl_io_cmd_t hcl_io_cmd_t;

struct hcl_loc_t
{
	hcl_oow_t line; /**< line */
	hcl_oow_t colm; /**< column */
	const hcl_ooch_t*  file; /**< file specified in #include */
};
typedef struct hcl_loc_t hcl_loc_t;

struct hcl_lxc_t
{
	hcl_ooci_t   c; /**< character */
	hcl_loc_t    l; /**< location */
};
typedef struct hcl_lxc_t hcl_lxc_t;

#if !defined(HCL_CCI_BUF_LEN)
#define HCL_CCI_BUF_LEN (2048)
#endif

typedef struct hcl_io_cciarg_t hcl_io_cciarg_t;
struct hcl_io_cciarg_t
{
	/**
	 * [IN] I/O object name.
	 * It is #HCL_NULL for the main stream and points to a non-NULL string
	 * for an included stream.
	 */
	const hcl_ooch_t* name;

	/**
	 * [OUT] I/O handle set by a handler.
	 * The source stream handler can set this field when it opens a stream.
	 * All subsequent operations on the stream see this field as set
	 * during opening.
	 */
	void* handle;

	/**
	 * [OUT] set this to non-zero in HCL_IO_OPEN if the handler fills the buffer with bytes.
	 *       the caller issues HCL_IO_READ_BYTES if it's set to non-zero, expecting bytes.
	 *       otherwise it issues HCL_IO_READ expecting characters.
	 */
	int byte_oriented;

	/**
	 * [OUT] place data here for #HCL_IO_READ or #HCL_IO_READ_BYTES
	 */
	union
	{
		hcl_ooch_t c[HCL_CCI_BUF_LEN];
		hcl_uint8_t b[HCL_CCI_BUF_LEN * HCL_SIZEOF(hcl_ooch_t)];
	} buf;

	/**
	 * [OUT] place the number of characters read here for #HCL_IO_READ
	 */
	hcl_oow_t  xlen;

	/**
	 * [IN] points to the data of the includer. It is #HCL_NULL for the
	 * main stream.
	 */
	hcl_io_cciarg_t* includer;

	/*-----------------------------------------------------------------*/
	/*----------- from here down, internal use only -------------------*/
	struct
	{
		hcl_oow_t pos;
		hcl_oow_t len;
	} b; /* buffer(buf.c or buf.b) usage status */

	struct
	{
		hcl_uint8_t buf[HCL_MBLEN_MAX];
		hcl_oow_t len;
	} rsd; /* residue bytes for HCL_IO_READ_BYTES */

	hcl_oow_t line;
	hcl_oow_t colm;
	hcl_ooci_t nl;

	hcl_lxc_t lxc;
	/*-----------------------------------------------------------------*/
};

typedef struct hcl_io_udiarg_t hcl_io_udiarg_t;
struct hcl_io_udiarg_t
{
	/**
	 * [OUT] I/O handle set by a handler.
	 * The stream handler can set this field when it opens a stream.
	 * All subsequent operations on the stream see this field as set
	 * during opening.
	 */
	void* handle;

	/**
	 * [OUT] indicates if HCL_IO_READ_BYTES is implemented
	 */
	int byte_oriented;

	/**
	 * [OUT] place data in c for #HCL_IO_READ and in d for #HCL_IO_READ_BYTES
	 */
	union {
		hcl_ooch_t  c[2048]; /* TODO: resize this if necessary */
		hcl_uint8_t b[2048 * HCL_SIZEOF(hcl_ooch_t)]; /* TODO: resize this if necessary */
	} buf;

	/**
	 * [OUT] place the number of characters read here for
	 * #HCL_IO_READ or #HCL_IO_READ_BYTES
	 */
	hcl_oow_t xlen;


	/*-----------------------------------------------------------------*/
	/*----------- from here down, internal use only -------------------*/
	struct
	{
		hcl_oow_t pos;
		hcl_oow_t len;
	} b; /* buffer(buf.c or buf.b) usage status */

	struct
	{
		hcl_uint8_t buf[HCL_MBLEN_MAX];
		hcl_oow_t len;
	} rsd; /* residue bytes for HCL_IO_READ_BYTES */

	int eof_reached;
};

typedef struct hcl_io_udoarg_t hcl_io_udoarg_t;
struct hcl_io_udoarg_t
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
	 *      hcl_ooch_t* for HCL_IO_WRITE
	 *      hcl_bch_t* or hcl_uint8_t* for HCL_IO_WRITE_BYTES
	 */
	void* ptr;

	/**
	 * [IN] total number of characters/bytes to write
	 */
	hcl_oow_t len;

	/**
	 * [OUT] place the number of characters/bytes written here for
	 *       #HCL_IO_WRITE or #HCL_IO_WRITE_BYTES
	 */
	hcl_oow_t xlen;
};

/**
 * The hcl_io_impl_t type defines a callback function prototype
 * for I/O operations.
 */
typedef int (*hcl_io_impl_t) (
	hcl_t*        hcl,
	hcl_io_cmd_t  cmd,
	void*         arg /* one of hcl_io_cciarg_t*, hcl_io_udiarg_t*, hcl_io_udoarg_t* */
);

/* =========================================================================
 * CALLBACK MANIPULATION
 * ========================================================================= */


typedef void (*hcl_cb_on_fini_t)    (hcl_t* hcl);
typedef void (*hcl_cb_on_halting_t) (hcl_t* hcl);
typedef void (*hcl_cb_on_option_t)  (hcl_t* hcl, hcl_option_t id, const void* val);
typedef void (*hcl_cb_on_gc_t)      (hcl_t* hcl);
typedef int  (*hcl_cb_vm_startup_t) (hcl_t* hcl);
typedef void (*hcl_cb_vm_cleanup_t) (hcl_t* hcl);
typedef void (*hcl_cb_vm_checkbc_t) (hcl_t* hcl, hcl_oob_t bcode);

typedef struct hcl_cb_t hcl_cb_t;
struct hcl_cb_t
{
	hcl_cb_on_fini_t    on_fini; /* called from hcl_fini() */
	hcl_cb_on_halting_t halting;
	hcl_cb_on_option_t  on_option; /* called from hcl_setoption() */
	hcl_cb_on_gc_t      on_gc; /* called from hcl_gc() */

	hcl_cb_vm_startup_t vm_startup;
	hcl_cb_vm_cleanup_t vm_cleanup;
	hcl_cb_vm_checkbc_t vm_checkbc;

	/* private below */
	hcl_cb_t*     prev;
	hcl_cb_t*     next;
};


/* =========================================================================
 * PRIMITIVE FUNCTIONS
 * ========================================================================= */
enum hcl_pfrc_t
{
	HCL_PF_FAILURE = -1,
	HCL_PF_SUCCESS = 0
};
typedef enum hcl_pfrc_t hcl_pfrc_t;

typedef hcl_pfrc_t (*hcl_pfimpl_t) (
	hcl_t*     hcl,
	hcl_mod_t* mod,
	hcl_ooi_t  nargs
);

enum hcl_pfbase_type_t
{
	HCL_PFBASE_FUNC  = 0,
	HCL_PFBASE_VAR   = 1,
	HCL_PFBASE_CONST = 2
};
typedef enum hcl_pfbase_type_t hcl_pfbase_type_t;

typedef struct hcl_pfbase_t hcl_pfbase_t;
struct hcl_pfbase_t
{
	hcl_pfbase_type_t type;
	hcl_pfimpl_t      handler;
	hcl_oow_t         minargs;
	hcl_oow_t         maxargs;
};

typedef struct hcl_pfinfo_t hcl_pfinfo_t;
struct hcl_pfinfo_t
{
	const hcl_bch_t*  mthname;
	hcl_pfbase_t      base;
};
/* =========================================================================
 * PRIMITIVE MODULE MANIPULATION
 * ========================================================================= */
#define HCL_MOD_NAME_LEN_MAX 120

typedef int (*hcl_mod_load_t) (
	hcl_t*     hcl,
	hcl_mod_t* mod
);

typedef hcl_pfbase_t* (*hcl_mod_query_t) (
	hcl_t*            hcl,
	hcl_mod_t*        mod,
	const hcl_ooch_t* name,
	hcl_oow_t         namelen
);

typedef void (*hcl_mod_unload_t) (
	hcl_t*     hcl,
	hcl_mod_t* mod
);

typedef void (*hcl_mod_gc_t) (
	hcl_t*     hcl,
	hcl_mod_t* mod
);

struct hcl_mod_t
{
	/* input */
	hcl_ooch_t       name[HCL_MOD_NAME_LEN_MAX + 1];
	void*            inctx;

	/* user-defined data - the module intializer shoudl fill in the following fields. */
	hcl_mod_query_t  query;
	hcl_mod_unload_t unload;
	hcl_mod_gc_t     gc;
	void*            ctx;
};

struct hcl_mod_data_t
{
	void*           handle;
	hcl_rbt_pair_t* pair; /* internal backreference to hcl->modtab */
	hcl_mod_t       mod;
};
typedef struct hcl_mod_data_t hcl_mod_data_t;


struct hcl_sem_tuple_t
{
	hcl_oop_semaphore_t sem[2]; /* [0] input, [1] output */
	hcl_ooi_t handle; /* io handle */
	hcl_ooi_t mask;
};
typedef struct hcl_sem_tuple_t hcl_sem_tuple_t;

/* =========================================================================
 * HCL VM
 * ========================================================================= */
typedef struct hcl_synerr_t hcl_synerr_t;
struct hcl_synerr_t
{
	hcl_synerrnum_t num;
	hcl_loc_t     loc;
	struct
	{
		hcl_ooch_t val[256];
		hcl_oow_t len;
	} tgt;
};

typedef struct hcl_dbgi_t hcl_dbgi_t;
struct hcl_dbgi_t
{
	const hcl_ooch_t* fname; /* file name */
	hcl_oow_t sline; /* source line in the file */
};

#if defined(HCL_INCLUDE_COMPILER)
typedef struct hcl_compiler_t hcl_compiler_t;
typedef struct hcl_cnode_t hcl_cnode_t;

typedef int (*hcl_on_cnode_t) (hcl_t* hcl, hcl_cnode_t* obj);

enum hcl_compile_flag_t
{
	/* clear byte codes at the beginnign of hcl_compile() */
	HCL_COMPILE_CLEAR_CODE  = (1 << 0),

	/* clear the top-level function block at the end of hcl_compile() */
	HCL_COMPILE_CLEAR_FNBLK = (1 << 1)
};
typedef enum hcl_compile_flag_t hcl_compile_flag_t;
#endif

#define HCL_ERRMSG_CAPA (2048)

struct hcl_code_t
{
	struct
	{
		hcl_oob_t* ptr; /* byte code array */
		hcl_oow_t len;
		hcl_oow_t capa;
	} bc;

	struct
	{
		hcl_oop_oop_t arr; /* literal array - not part of object memory */
		hcl_oow_t len;
	} lit;

	/* the cumulative number of temporaries collected at the global(top-level) level */
	hcl_oow_t ngtmprs;

	/* array that holds the location of the byte code emitted */
	hcl_dbgi_t* dbgi;
};
typedef struct hcl_code_t hcl_code_t;

struct hcl_t
{
	hcl_oow_t    _instsize;
	hcl_mmgr_t*  _mmgr;
	hcl_cmgr_t*  _cmgr;

	hcl_errnum_t errnum;
	struct
	{
		union
		{
			hcl_ooch_t ooch[HCL_ERRMSG_CAPA];
			hcl_bch_t bch[HCL_ERRMSG_CAPA];
			hcl_uch_t uch[HCL_ERRMSG_CAPA];
		} tmpbuf;
	#if defined(HCL_OOCH_IS_BCH)
		hcl_uch_t  xerrmsg[HCL_ERRMSG_CAPA];
	#else
		hcl_bch_t  xerrmsg[HCL_ERRMSG_CAPA * 2];
	#endif
		hcl_ooch_t buf[HCL_ERRMSG_CAPA];
		hcl_oow_t len;

	} errmsg;
	hcl_loc_t errloc;
	int shuterr;

	struct
	{
		hcl_bitmask_t trait;
		hcl_bitmask_t log_mask;
		hcl_oow_t log_maxcapa;
		hcl_bch_t* log_target_b;
		hcl_uch_t* log_target_u;
		hcl_oow_t dfl_symtab_size;
		hcl_oow_t dfl_sysdic_size;
		hcl_oow_t dfl_procstk_size;
		void* mod_inctx;

		hcl_oocs_t mod[3];

	#if defined(HCL_BUILD_DEBUG)
		/* set automatically when trait is set */
		hcl_oow_t karatsuba_cutoff;
	#endif
	} option;

	hcl_vmprim_t vmprim;

	hcl_oow_t vm_checkbc_cb_count;
	hcl_cb_t* cblist;
	hcl_rbt_t modtab; /* primitive module table */

	struct
	{
		hcl_ooch_t* ptr;
		hcl_oow_t len;
		hcl_oow_t capa;
		hcl_bitmask_t last_mask;
		hcl_bitmask_t default_type_mask;
	} log;
	/* ========================= */

	hcl_heap_t* heap;

	/* ========================= */
	hcl_oop_t _undef; /* special internal value for uninitialized global variables */
	hcl_oop_t _nil;  /* pointer to the nil object */
	hcl_oop_t _true;
	hcl_oop_t _false;

	hcl_oop_t s_and;    /* symbol */
	hcl_oop_t s_break;  /* symbol */
	hcl_oop_t s_catch;  /* symbol */
	hcl_oop_t s_class;  /* symbol */
	hcl_oop_t s_continue; /* symbol */
	hcl_oop_t s_defun;  /* symbol */
	hcl_oop_t s_do;     /* symbol */
	hcl_oop_t s_elif;   /* symbol */
	hcl_oop_t s_else;   /* symbol */
	hcl_oop_t s_fun;    /* symbol */
	hcl_oop_t s_if;     /* symbol */
	hcl_oop_t s_or;     /* symbol */
	hcl_oop_t s_plus;   /* symbol */
	hcl_oop_t s_return; /* symbol */
	hcl_oop_t s_revert; /* symbol */
	hcl_oop_t s_set;    /* symbol */
	hcl_oop_t s_set_r;  /* symbol */
	hcl_oop_t s_throw;  /* symbol */
	hcl_oop_t s_try;   /* symbol */  /* _try is special in MSVC. */
	hcl_oop_t s_until;  /* symbol */
	hcl_oop_t s_while;  /* symbol */

	hcl_oop_dic_t symtab; /* system-wide symbol table. */
	hcl_oop_dic_t sysdic; /* system dictionary. */
	hcl_oop_process_scheduler_t processor; /* instance of ProcessScheduler */
	hcl_oop_process_t nil_process; /* instance of Process */

	/* =============================================================
	 * KERNEL CLASSES
	 *  Be sure to Keep these kernel class pointers registered in the
	 *  kernel_classes table in gc.c
	 * ============================================================= */
	hcl_oop_class_t c_apex; /* Apex */
	hcl_oop_class_t c_class; /* Class */
	hcl_oop_class_t c_undefobj; /* UndefinedObject */
	hcl_oop_class_t c_nilobj; /* NilObject */
#if 0
	hcl_oop_class_t c_interface; /* Interface */
#endif
	hcl_oop_class_t c_object; /* Object */

	hcl_oop_class_t c_collection; /* Collection */
	hcl_oop_class_t c_indexed_collection; /* IndexedCollection */
	hcl_oop_class_t c_fixed_sized_collection; /* FixedSizedCollection */
	hcl_oop_class_t c_string; /* String */
	hcl_oop_class_t c_byte_string; /* String */
	hcl_oop_class_t c_symbol; /* Symbol */
	hcl_oop_class_t c_array; /* Array */
	hcl_oop_class_t c_character_array; /* CharacterArray */
	hcl_oop_class_t c_byte_array; /* ByteArray */
	hcl_oop_class_t c_symtab; /* SymbolTable */
	hcl_oop_class_t c_dictionary;
	hcl_oop_class_t c_cons; /* Cons */

#if 0
	hcl_oop_class_t c_namespace; /* Namespace */
	hcl_oop_class_t c_pool_dictionary; /* PoolDictionary */
#endif
	hcl_oop_class_t c_method_dictionary; /* MethodDictionary */
#if 0
	hcl_oop_class_t c_method; /* CompiledMethod */
	hcl_oop_class_t c_methsig; /* MethodSignature */
#endif
	hcl_oop_class_t c_function; /* Function */
	hcl_oop_class_t c_primitive; /* Primitive */
	hcl_oop_class_t c_compiled_block; /* CompiledBlock */

	hcl_oop_class_t c_block_context; /* BlockContext */
	hcl_oop_class_t c_process; /* Process */
	hcl_oop_class_t c_semaphore; /* Semaphore */
	hcl_oop_class_t c_semaphore_group; /* SemaphoreGroup */
	hcl_oop_class_t c_process_scheduler; /* ProcessScheduler */

	hcl_oop_class_t c_error; /* Error */
	hcl_oop_class_t c_true; /* True */
	hcl_oop_class_t c_false; /* False */
	hcl_oop_class_t c_magnitude; /* Magnitude */
	hcl_oop_class_t c_character; /* Character */
	hcl_oop_class_t c_number; /* Number */
	hcl_oop_class_t c_small_integer; /* SmallInteger */

	hcl_oop_class_t c_large_positive_integer; /* LargePositiveInteger */
	hcl_oop_class_t c_large_negative_integer; /* LargeNegativeInteger */
	hcl_oop_class_t c_fixed_point_decimal; /* FixedPointDecimal */

	hcl_oop_class_t c_small_pointer;
	hcl_oop_class_t c_large_pointer;
	hcl_oop_class_t c_system;

	/* ============================================================================= */

	/* pending asynchronous semaphores */
	hcl_oop_semaphore_t* sem_list;
	hcl_oow_t sem_list_count;
	hcl_oow_t sem_list_capa;

	/* semaphores sorted according to time-out.
	 * organize entries using heap as the earliest entry
	 * needs to be checked first */
	hcl_oop_semaphore_t* sem_heap;
	hcl_oow_t sem_heap_count;
	hcl_oow_t sem_heap_capa;

	/* semaphores for I/O handling. plain array */
	/*hcl_oop_semaphore_t* sem_io;*/
	hcl_sem_tuple_t* sem_io_tuple;
	hcl_oow_t sem_io_tuple_count;
	hcl_oow_t sem_io_tuple_capa;

	hcl_oow_t sem_io_count;
	hcl_oow_t sem_io_wait_count; /* number of processes waiting on an IO semaphore */

	hcl_ooi_t* sem_io_map;
	hcl_oow_t sem_io_map_capa;
	/* ============================================================================= */

	hcl_oop_t* proc_map;
	hcl_oow_t proc_map_capa;
	hcl_oow_t proc_map_used;
	hcl_ooi_t proc_map_free_first;
	hcl_ooi_t proc_map_free_last;

	/* 2 tag bits(lo) + 2 extended tag bits(hi). not all slots are used
	 * because the 2 high extended bits are used only if the low tag bits
	 * are 3 */
	int tagged_brands[16];
	hcl_oop_class_t* tagged_classes[16]; /* this is a pointer to hcl_oop_class_t which is also a pointer */


	hcl_oop_t* volat_stack[256]; /* stack for temporaries */
	hcl_oow_t volat_count;

	/* == EXECUTION REGISTERS == */
	hcl_oop_function_t initial_function;
	hcl_oop_context_t initial_context; /* fake initial context */
	hcl_oop_context_t active_context;
	hcl_oop_function_t active_function;
	hcl_oob_t* active_code;
	hcl_ooi_t sp;
	hcl_ooi_t ip;
	int no_proc_switch; /* process switching disabled */
	int proc_switched; /* TODO: this is temporary. implement something else to skip immediate context switching */
	int switch_proc;
	int abort_req;
	hcl_ntime_t exec_start_time;
	hcl_ntime_t exec_end_time;
	hcl_oop_t last_retv;
	/* == END EXECUTION REGISTERS == */

	/* == BIGINT CONVERSION == */
	struct
	{
		int safe_ndigits;
		hcl_oow_t multiplier;
	} bigint[37];

	struct
	{
		struct
		{
			hcl_ooch_t* ptr;
			hcl_oow_t capa;
			hcl_oow_t len;
		} xbuf;
		struct
		{
			hcl_liw_t* ptr;
			hcl_oow_t capa;
		} t;
	} inttostr;
	/* == END BIGINT CONVERSION == */

	struct
	{
		struct
		{
			hcl_ooch_t* ptr;
			hcl_oow_t capa;
			hcl_oow_t len;
		} xbuf; /* buffer to support sprintf */
	} sprintf;


	hcl_code_t code;

	/* == PRINTER to udo stream == */
	struct
	{
		struct
		{
			void*        ptr;
			hcl_oow_t    capa;
			hcl_oow_t    size;
		} s;
		hcl_oop_t e; /* top entry being printed */
	} p;
	/* == PRINTER to udo stream == */

	struct
	{
		hcl_gchdr_t* b; /* object blocks allocated */
		struct
		{
			hcl_gchdr_t* curr;
			hcl_gchdr_t* prev;
		} ls;
		hcl_oow_t bsz; /* total size of object blocks allocated */
		hcl_oow_t threshold;
		int lazy_sweep;

		struct
		{
			hcl_oop_t* ptr;
			hcl_oow_t capa;
			hcl_oow_t len;
			hcl_oow_t max;
		} stack;

		struct
		{
			hcl_ntime_t alloc;
			hcl_ntime_t mark;
			hcl_ntime_t sweep;
		} stat;
	} gci;

	struct
	{
		/* input handler */
		hcl_io_impl_t udi_rdr;
		hcl_io_udiarg_t udi_arg;

		/* output handler */
		hcl_io_impl_t udo_wrtr;
		hcl_io_udoarg_t udo_arg;
	} io;

#if defined(HCL_INCLUDE_COMPILER)
	hcl_compiler_t* c;
#endif
};


/* TODO: stack bound check when pushing */
#define HCL_STACK_PUSH(hcl,v) \
	do { \
		if (HCL_UNLIKELY((hcl)->sp >= HCL_OOP_TO_SMOOI((hcl)->processor->active->st))) \
		{ \
			hcl_seterrbfmt (hcl, HCL_EOOMEM, "process stack overflow"); \
			(hcl)->abort_req = -1; \
		} \
		(hcl)->sp = (hcl)->sp + 1; \
		(hcl)->processor->active->slot[(hcl)->sp] = v; \
	} while (0)

#define HCL_STACK_GET(hcl,sp_) ((hcl)->processor->active->slot[sp_])
#define HCL_STACK_SET(hcl,sp_,obj_) ((hcl)->processor->active->slot[sp_] = obj_)

#define HCL_STACK_GETTOP(hcl) HCL_STACK_GET(hcl, (hcl)->sp)
#define HCL_STACK_SETTOP(hcl,obj_) HCL_STACK_SET(hcl, (hcl)->sp, obj_)

/* [NOTE]
 *  the following macros don't commit the active stack pointer(hcl->sp)
 *  to hcl->processor->active->sp immediately.
 */
#define HCL_STACK_POP(hcl) ((hcl)->sp = (hcl)->sp - 1)
#define HCL_STACK_POPS(hcl,count) ((hcl)->sp = (hcl)->sp - (count))
#define HCL_STACK_POP_TO(hcl,v) \
	do { \
		v = HCL_STACK_GETTOP(hcl); \
		HCL_STACK_POP (hcl); \
	} while(0)

#define HCL_STACK_GET_ST(hcl) HCL_OOP_TO_SMOOI((hcl)->processor->active->st)
#define HCL_STACK_GET_SP(hcl) ((hcl)->sp)
#define HCL_STACK_IS_EMPTY(hcl) ((hcl)->sp <= -1)

/* get the stack pointer of the argument at the given index */
#define HCL_STACK_GETARGSP(hcl,nargs,idx) ((hcl)->sp - ((nargs) - (idx) - 1))
/* get the argument at the given index */
#define HCL_STACK_GETARG(hcl,nargs,idx) HCL_STACK_GET(hcl, (hcl)->sp - ((nargs) - (idx) - 1))
/* get the receiver of a message */
#define HCL_STACK_GETRCV(hcl,nargs) HCL_STACK_GET(hcl, (hcl)->sp - nargs - 1)
/* get the operator such as the called function/block/method */
#define HCL_STACK_GETOP(hcl,nargs) HCL_STACK_GET(hcl, (hcl)->sp - nargs)

/* change the receiver of a message */
#define HCL_STACK_SETRCV(hcl,nargs,newrcv) HCL_STACK_SET(hcl, (hcl)->sp - nargs - 1, newrcv)

/*
 * .....
 * argument 1
 * argument 0
 * operator
 * receiver
 */

/* you can't access arguments and receiver after this macro.
 * also you must not call this macro more than once */

#define HCL_STACK_SETRET(hcl,nargs,retv) \
	do { \
		HCL_STACK_POPS(hcl, nargs + 1); \
		HCL_STACK_SETTOP(hcl, (retv)); \
	} while(0)

#define HCL_STACK_SETRETTOERRNUM(hcl,nargs) HCL_STACK_SETRET(hcl, nargs, HCL_ERROR_TO_OOP(hcl->errnum))
#define HCL_STACK_SETRETTOERROR(hcl,nargs,ec) HCL_STACK_SETRET(hcl, nargs, HCL_ERROR_TO_OOP(ec))

/* =========================================================================
 * HCL ASSERTION
 * ========================================================================= */
#if defined(HCL_BUILD_RELEASE)
#	define HCL_ASSERT(hcl,expr) ((void)0)
#else
#	define HCL_ASSERT(hcl,expr) ((void)((expr) || ((hcl)->vmprim.assertfail (hcl, #expr, __FILE__, __LINE__), 0)))
#endif

/* =========================================================================
 * HCL COMMON OBJECTS
 * ========================================================================= */
enum hcl_brand_t
{
	HCL_BRAND_SMOOI = 1, /* never used because a small integer is encoded in an object pointer */
	HCL_BRAND_SMPTR,
	HCL_BRAND_ERROR,
	HCL_BRAND_CHARACTER,

	HCL_BRAND_UNDEF,
	HCL_BRAND_NIL,
	HCL_BRAND_TRUE,
	HCL_BRAND_FALSE,

	HCL_BRAND_PBIGINT, /* positive big integer */
	HCL_BRAND_NBIGINT, /* negative big integer */
	HCL_BRAND_CONS,
	HCL_BRAND_ARRAY,
	HCL_BRAND_BYTE_ARRAY,
	HCL_BRAND_CHARACTER_ARRAY,
	HCL_BRAND_SYMBOL,
	HCL_BRAND_STRING,
	HCL_BRAND_BYTE_STRING,
	HCL_BRAND_DIC,
	HCL_BRAND_FPDEC, /* fixed-point decimal */

	HCL_BRAND_PRIM,

	HCL_BRAND_FUNCTION,
	HCL_BRAND_BLOCK,
	HCL_BRAND_CONTEXT,
	HCL_BRAND_PROCESS,
	HCL_BRAND_PROCESS_SCHEDULER,
	HCL_BRAND_SEMAPHORE,
	HCL_BRAND_SEMAPHORE_GROUP,
	HCL_BRAND_CLASS,
	HCL_BRAND_INSTANCE


	/* [NOTE] each enumerator must not exceed the maximum value that can be
	 *        represented with HCL_OBJ_FLAGS_BRAND_BITS bits */
};
typedef enum hcl_brand_t hcl_brand_t;

enum hcl_syncode_t
{
	HCL_SYNCODE_NONE = 0,

	/* SYNCODE 0 means it's not a syncode object. so the actual code begins with 1 */
	/* these enumerators can be set in the SYNCODE flags for a symbol */
	HCL_SYNCODE_AND = 1,
	HCL_SYNCODE_BREAK,
	HCL_SYNCODE_CATCH,
	HCL_SYNCODE_CLASS,
	HCL_SYNCODE_CONTINUE,
	HCL_SYNCODE_DO,
	HCL_SYNCODE_ELIF,
	HCL_SYNCODE_ELSE,
	HCL_SYNCODE_IF,
	HCL_SYNCODE_FUN,
	HCL_SYNCODE_OR,
	HCL_SYNCODE_PLUS,
	HCL_SYNCODE_RETURN, /* local return. return from the current context. */
	HCL_SYNCODE_REVERT, /* non-local return. return from home context */
	HCL_SYNCODE_SET,
	HCL_SYNCODE_SET_R,
	HCL_SYNCODE_THROW,
	HCL_SYNCODE_TRY,
	HCL_SYNCODE_UNTIL,
	HCL_SYNCODE_WHILE
};
typedef enum hcl_syncode_t hcl_syncode_t;

enum hcl_concode_t
{
	/* these can be set in the SYNCODE flags for a cons cell */
	HCL_CONCODE_XLIST = 0,  /* ( ) - executable list */
	HCL_CONCODE_MLIST,      /* (obj:message) - message send list */
	HCL_CONCODE_ALIST,      /* (a := 20) assignment list */
	HCL_CONCODE_BLIST,      /* (10 + 20) expression with binary operator */
	HCL_CONCODE_BLOCK,      /* { } */
	HCL_CONCODE_ARRAY,      /* #[ ] */
	HCL_CONCODE_BYTEARRAY,  /* #b[ ] */
	HCL_CONCODE_CHARARRAY,  /* #c[ ] */
	HCL_CONCODE_DIC,        /* #{ } */
	HCL_CONCODE_QLIST,      /* #( ) - data list */
	HCL_CONCODE_TUPLE,      /* [ ] */
	HCL_CONCODE_VLIST       /* | | - symbol list */
};
typedef enum hcl_concode_t hcl_concode_t;

#define HCL_IS_UNDEF(hcl,v) (v == (hcl)->_undef)
#define HCL_IS_NIL(hcl,v) (v == (hcl)->_nil)
#define HCL_IS_TRUE(hcl,v) (v == (hcl)->_true)
#define HCL_IS_FALSE(hcl,v) (v == (hcl)->_false)

#define HCL_IS_SYMBOL(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_symbol)
#define HCL_IS_STRING(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_string)
#define HCL_IS_CONTEXT(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_block_context)
#define HCL_IS_FUNCTION(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_function)
#define HCL_IS_COMPILED_BLOCK(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_compiled_block)
#define HCL_IS_CLASS(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_class)
#define HCL_IS_INSTANCE(hcl,v) (HCL_OOP_IS_POINTER(v) && HCL_OBJ_GET_FLAGS_BRAND(v) == HCL_BRAND_INSTANCE)
#define HCL_IS_CONS(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_cons)
#define HCL_IS_CONS_CONCODED(hcl,v,concode) (HCL_IS_CONS(hcl,v) && HCL_OBJ_GET_FLAGS_SYNCODE(v) == (concode))
#define HCL_IS_ARRAY(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_array)
#define HCL_IS_BYTEARRAY(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_byte_array)
#define HCL_IS_DIC(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_dictionary)
#define HCL_IS_PRIM(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_primitive)
#define HCL_IS_PBIGINT(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_large_positive_integer)
#define HCL_IS_NBIGINT(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_large_negative_integer)
#define HCL_IS_BIGINT(hcl,v) (HCL_OOP_IS_POINTER(v) && (HCL_OBJ_GET_CLASS(v) == (hcl_oop_t)(hcl)->c_large_positive_integer || HCL_OBJ_GET_CLASS(v) == (hcl_oop_t)(hcl)->c_large_negative_integer))
#define HCL_IS_FPDEC(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_fixed_point_decimal)
#define HCL_IS_PROCESS(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_process)
#define HCL_IS_SEMAPHORE(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_semaphore)
#define HCL_IS_SEMAPHORE_GROUP(hcl,v) (HCL_CLASSOF(hcl,v) == (hcl_oop_t)(hcl)->c_semaphore_group)

#define HCL_CONS_CAR(v)  (((hcl_cons_t*)(v))->car)
#define HCL_CONS_CDR(v)  (((hcl_cons_t*)(v))->cdr)

typedef int (*hcl_dic_walker_t) (
	hcl_t*          hcl,
	hcl_oop_dic_t   dic,
	hcl_oop_cons_t  pair,
	void*           ctx
);

typedef int (*hcl_xchg_reader_t) (
	hcl_t*      hcl,
	void*       buf,
	hcl_oow_t   len,
	void*       ctx
);

typedef int (*hcl_xchg_writer_t) (
	hcl_t*      hcl,
	const void* ptr,
	hcl_oow_t   len,
	void*       ctx
);

#if defined(__cplusplus)
extern "C" {
#endif

HCL_EXPORT hcl_t* hcl_open (
	hcl_mmgr_t*         mmgr,
	hcl_oow_t           xtnsize,
	const hcl_vmprim_t* vmprim,
	hcl_errnum_t*       errnum
);

HCL_EXPORT hcl_t* hcl_openstdwithmmgr (
	hcl_mmgr_t*         mmgr,
	hcl_oow_t           xtnsize,
	hcl_errnum_t*       errnum
);

HCL_EXPORT hcl_t* hcl_openstd (
	hcl_oow_t           xtnsize,
	hcl_errnum_t*       errnum
);

HCL_EXPORT void hcl_close (
	hcl_t* vm
);

HCL_EXPORT int hcl_init (
	hcl_t*              hcl,
	hcl_mmgr_t*         mmgr,
	const hcl_vmprim_t* vmprim
);

HCL_EXPORT void hcl_fini (
	hcl_t*              hcl
);

/**
 * The hcl_reset() function some internal states back to the initial state.
 * The affected internal states include byte code buffer, literal frame,
 * ordinary global variables. You should take extra precaution as it is
 * a risky function. For instance, a global variable inserted manually
 * with hcl_putatsysdic() gets deleted if the kernel bit is not set on
 * the variable symbol.
 */
HCL_EXPORT void hcl_reset (
	hcl_t*   hcl
);

#define HCL_XTN(hcl) ((void*)((hcl_uint8_t*)hcl + ((hcl_t*)hcl)->_instsize))
#define HCL_MMGR(hcl) (((hcl_t*)(hcl))->_mmgr)
#define HCL_CMGR(hcl) (((hcl_t*)(hcl))->_cmgr)
#define HCL_ERRNUM(hcl) (((hcl_t*)(hcl))->errnum)

void* hcl_getxtn (
	hcl_t* hcl
);

HCL_EXPORT hcl_cmgr_t* hcl_getcmgr (
	hcl_t* hcl
);

HCL_EXPORT void hcl_setcmgr (
	hcl_t*      hcl,
	hcl_cmgr_t* cmgr
);

HCL_EXPORT hcl_errnum_t hcl_geterrnum (
	hcl_t* hcl
);

HCL_EXPORT void hcl_seterrnum (
	hcl_t*       hcl,
	hcl_errnum_t errnum
);

HCL_EXPORT void hcl_geterrloc (
	hcl_t*     hcl,
	hcl_loc_t* loc
);

HCL_EXPORT void hcl_seterrbmsg (
	hcl_t*           hcl,
	hcl_errnum_t     errnum,
	const hcl_bch_t* errmsg
);

HCL_EXPORT void hcl_seterrumsg (
	hcl_t*           hcl,
	hcl_errnum_t     errnum,
	const hcl_uch_t* errmsg
);

HCL_EXPORT void hcl_seterrwithsyserr (
	hcl_t*       hcl,
	int          syserr_type,
	int          syserr_code
);

HCL_EXPORT void hcl_seterrbfmtwithsyserr (
	hcl_t*           hcl,
	int              syserr_type,
	int              syserr_code,
	const hcl_bch_t* fmt,
       	...
);

HCL_EXPORT void hcl_seterrufmtwithsyserr (
	hcl_t*           hcl,
	int              syserr_type,
	int              syserr_code,
	const hcl_uch_t* fmt,
       	...
);

HCL_EXPORT void hcl_seterrbfmt (
	hcl_t*           hcl,
	hcl_errnum_t     errnum,
	const hcl_bch_t* fmt,
	...
);

HCL_EXPORT void hcl_seterrufmt (
	hcl_t*           hcl,
	hcl_errnum_t     errnum,
	const hcl_uch_t* fmt,
	...
);

HCL_EXPORT void hcl_seterrbfmtloc (
	hcl_t*           hcl,
	hcl_errnum_t     errnum,
	const hcl_loc_t* loc,
	const hcl_bch_t* fmt,
	...
);

HCL_EXPORT void hcl_seterrufmtloc (
	hcl_t*           hcl,
	hcl_errnum_t     errnum,
	const hcl_loc_t* loc,
	const hcl_uch_t* fmt,
	...
);

HCL_EXPORT void hcl_seterrbfmtv (
	hcl_t*           hcl,
	hcl_errnum_t     errnum,
	const hcl_bch_t* fmt,
	va_list          ap
);

HCL_EXPORT void hcl_seterrufmtv (
	hcl_t*           hcl,
	hcl_errnum_t     errnum,
	const hcl_uch_t* fmt,
	va_list          ap
);


HCL_EXPORT const hcl_ooch_t* hcl_geterrstr (
	hcl_t* hcl
);

HCL_EXPORT const hcl_uch_t* hcl_geterrumsg (
	hcl_t* hcl
);

HCL_EXPORT const hcl_bch_t* hcl_geterrbmsg (
	hcl_t* hcl
);

HCL_EXPORT hcl_oow_t hcl_copyerrbmsg (
	hcl_t*     hcl,
	hcl_bch_t* buf,
	hcl_oow_t  len
);

HCL_EXPORT hcl_oow_t hcl_copyerrumsg (
	hcl_t*     hcl,
	hcl_uch_t* buf,
	hcl_oow_t  len
);

#if defined(HCL_OOCH_IS_UCH)
#	define hcl_geterrmsg  hcl_geterrumsg
#	define hcl_seterrmsg  hcl_seterrumsg
#	define hcl_copyerrmsg hcl_copyerrumsg
#else
#	define hcl_geterrmsg  hcl_geterrbmsg
#	define hcl_seterrmsg  hcl_seterrbmsg
#	define hcl_copyerrmsg hcl_copyerrbmsg
#endif

HCL_EXPORT const hcl_ooch_t* hcl_backuperrmsg (
	hcl_t* hcl
);

HCL_EXPORT int hcl_errnum_is_synerr (
	hcl_errnum_t errnum
);

HCL_EXPORT const hcl_ooch_t* hcl_errnum_to_errstr (
	hcl_errnum_t errnum
);

HCL_EXPORT const hcl_bch_t* hcl_errnum_to_errbcstr (
	hcl_errnum_t errnum,
	hcl_bch_t*   buf,
	hcl_oow_t    len
);

HCL_EXPORT const hcl_uch_t* hcl_errnum_to_errucstr (
	hcl_errnum_t errnum,
	hcl_uch_t*   buf,
	hcl_oow_t    len
);

/**
 * The hcl_getoption() function gets the value of an option
 * specified by \a id into the buffer pointed to by \a value.
 *
 * \return 0 on success, -1 on failure
 */
HCL_EXPORT int hcl_getoption (
	hcl_t*         hcl,
	hcl_option_t   id,
	void*          value
);

/**
 * The hcl_setoption() function sets the value of an option
 * specified by \a id to the value pointed to by \a value.
 *
 * \return 0 on success, -1 on failure
 */
HCL_EXPORT int hcl_setoption (
	hcl_t*        hcl,
	hcl_option_t  id,
	const void*   value
);

HCL_EXPORT hcl_cb_t* hcl_regcb (
	hcl_t*    hcl,
	hcl_cb_t* tmpl
);

HCL_EXPORT void hcl_deregcb (
	hcl_t*    hcl,
	hcl_cb_t* cb
);

/**
 * The hcl_gc() function performs garbage collection.
 * It is not affected by #HCL_TRAIT_NOGC.
 */
HCL_EXPORT void hcl_gc (
	hcl_t* hcl,
	int    full
);


/**
 * The hcl_moveoop() function is used to move a live object to a new
 * location in hcl_gc(). When hcl_gc() invokes registered gc callbacks,
 * you may call this function to protect extra objects you might have
 * allocated manually.
 */
hcl_oop_t hcl_moveoop (
	hcl_t*     hcl,
	hcl_oop_t  oop
);

HCL_EXPORT hcl_oop_t hcl_shallowcopy (
	hcl_t*      hcl,
	hcl_oop_t   oop
);

/**
 * The hcl_ignite() function creates key initial objects.
 */
HCL_EXPORT int hcl_ignite (
	hcl_t*      hcl,
	hcl_oow_t   heapsize
);

HCL_EXPORT int hcl_addbuiltinprims (
	hcl_t*      hcl
);

/**
 * The hcl_execute() function executes an activated context.
 */
HCL_EXPORT hcl_oop_t hcl_execute (
	hcl_t* hcl
);

HCL_EXPORT void hcl_abort (
	hcl_t* hcl
);


#if defined(HCL_HAVE_INLINE)
	static HCL_INLINE void hcl_switchprocess (hcl_t* hcl) { hcl->switch_proc = 1; }
#else
#	define hcl_switchprocess(hcl) ((hcl)->switch_proc = 1)
#endif

HCL_EXPORT void hcl_setbasesrloc (
	hcl_t*    hcl,
	hcl_oow_t line,
	hcl_oow_t colm
);

/* if you should read charcters from the input stream before hcl_read(),
 * you can call hcl_readbasesrchar() */
HCL_EXPORT hcl_lxc_t* hcl_readbasesrchar (
	hcl_t* hcl
);

HCL_EXPORT int hcl_attachccio (
	hcl_t*        hcl,
	hcl_io_impl_t cci_rdr
);

HCL_EXPORT void hcl_detachccio (
	hcl_t*        hcl
);

HCL_EXPORT int hcl_attachudio (
	hcl_t*        hcl,
	hcl_io_impl_t udi_rdr,
	hcl_io_impl_t udo_wrtr
);

HCL_EXPORT void hcl_detachudio (
	hcl_t*        hcl
);


HCL_EXPORT int hcl_attachcciostdwithucstr (
	hcl_t*           hcl,
	const hcl_uch_t* cci_file
);

HCL_EXPORT int hcl_attachcciostdwithbcstr (
	hcl_t*           hcl,
	const hcl_bch_t* cci_file
);

HCL_EXPORT int hcl_attachudiostdwithucstr (
	hcl_t*           hcl,
	const hcl_uch_t* udi_file,
	const hcl_uch_t* udo_file
);

HCL_EXPORT int hcl_attachudiostdwithbcstr (
	hcl_t*           hcl,
	const hcl_bch_t* udi_file,
	const hcl_bch_t* udo_file
);

HCL_EXPORT void hcl_detachio (
	hcl_t*       hcl
);

HCL_EXPORT void hcl_flushudio (
	hcl_t*       hcl
);

HCL_EXPORT int hcl_print (
	hcl_t*       hcl,
	hcl_oop_t    obj
);

HCL_EXPORT hcl_ooi_t hcl_proutbfmt (
	hcl_t*           hcl,
	hcl_bitmask_t    mask,
	const hcl_bch_t* fmt,
	...
);

HCL_EXPORT hcl_ooi_t hcl_proutufmt (
	hcl_t*           hcl,
	hcl_bitmask_t    mask,
	const hcl_uch_t* fmt,
	...
);

#if defined(HCL_INCLUDE_COMPILER)

HCL_EXPORT void hcl_freecnode (
	hcl_t*       hcl,
	hcl_cnode_t* cnode
);

HCL_EXPORT int hcl_beginfeed (
	hcl_t*            hcl,
	hcl_on_cnode_t    on_cnode
);

HCL_EXPORT int hcl_feed (
	hcl_t*            hcl,
	const hcl_ooch_t* data,
	hcl_oow_t         len
);

HCL_EXPORT int hcl_feeduchars (
	hcl_t*           hcl,
	const hcl_uch_t* data,
	hcl_oow_t        len
);

HCL_EXPORT int hcl_feedbchars (
	hcl_t*           hcl,
	const hcl_bch_t* data,
	hcl_oow_t        len
);

HCL_EXPORT int hcl_feedpending (
	hcl_t*           hcl
);


HCL_EXPORT void hcl_resetfeedloc (
	hcl_t*           hcl
);

HCL_EXPORT int hcl_endfeed (
	hcl_t*            hcl
);

HCL_EXPORT int hcl_compile (
	hcl_t*       hcl,
	hcl_cnode_t* obj,
	int          flags
);
#endif

HCL_EXPORT int hcl_addliteraltocode (
	hcl_t*        hcl,
	hcl_code_t*   code,
	hcl_oop_t     obj,
	hcl_oow_t     lfbase,
	hcl_oow_t*    index
);

/**
 * The hcl_brewcode() initializes the structure pointed to by \a code partially or entirely.
 * The part already initialized is not destroyed and/or reinitialized.
 */
HCL_EXPORT int hcl_brewcode (
	hcl_t*      hcl,
	hcl_code_t* code
);

/**
 * The hcl_purgecode() function cleans up the data held in code space memory
 * pointed to by \a code.
 */
HCL_EXPORT void hcl_purgecode (
	hcl_t*       hcl,
	hcl_code_t*  code
);

/**
 * The hcl_decode() function decodes instructions from the position
 * \a start to the position \a end - 1, and prints the decoded instructions
 * in the textual form.
 */
HCL_EXPORT int hcl_decode (
	hcl_t*             hcl,
	const hcl_code_t*  code,
	hcl_oow_t          start,
	hcl_oow_t          end
);

HCL_EXPORT void hcl_clearcode (
	hcl_t* hcl
);

#if defined(HCL_HAVE_INLINE)
static HCL_INLINE hcl_code_t* hcl_getcode (hcl_t* hcl) { return &hcl->code; }
static HCL_INLINE hcl_oob_t* hcl_getbcptr (hcl_t* hcl) { return hcl->code.bc.ptr; }
static HCL_INLINE hcl_oow_t hcl_getbclen (hcl_t* hcl) { return hcl->code.bc.len; }
static HCL_INLINE hcl_oow_t hcl_getlflen (hcl_t* hcl) { return hcl->code.lit.len; }
static HCL_INLINE hcl_oow_t hcl_getngtmprs (hcl_t* hcl) { return hcl->code.ngtmprs; }
static HCL_INLINE hcl_ooi_t hcl_getip (hcl_t* hcl) { return hcl->ip; }
#else
#	define hcl_getcode(hcl) (&(hcl)->code)
#	define hcl_getbcptr(hcl) ((hcl)->code.bc.ptr)
#	define hcl_getbclen(hcl) ((hcl)->code.bc.len)
#	define hcl_getlflen(hcl) ((hcl)->code.lit.len)
#	define hcl_getngtmprs(hcl) ((hcl)->code.ngtmprs)
#	define hcl_getip(hcl) ((hcl)->ip)
#endif

/* =========================================================================
 * SYNTAX ERROR HANDLING
 * ========================================================================= */
HCL_EXPORT void hcl_getsynerr (
	hcl_t*             hcl,
	hcl_synerr_t*      synerr
);

HCL_EXPORT hcl_synerrnum_t hcl_getsynerrnum (
	hcl_t*             hcl
);

HCL_EXPORT void hcl_setsynerrbfmt (
	hcl_t*              hcl,
	hcl_synerrnum_t     num,
	const hcl_loc_t*    loc,
	const hcl_oocs_t*   tgt,
	const hcl_bch_t*    msgfmt,
	...
);

HCL_EXPORT void hcl_setsynerrufmt (
	hcl_t*              hcl,
	hcl_synerrnum_t     num,
	const hcl_loc_t*    loc,
	const hcl_oocs_t*   tgt,
	const hcl_uch_t*    msgfmt,
	...
);

#if defined(HCL_HAVE_INLINE)
static HCL_INLINE void hcl_setsynerr (hcl_t* hcl, hcl_synerrnum_t num, const hcl_loc_t* loc, const hcl_oocs_t* tgt)
{
	hcl_setsynerrbfmt (hcl, num, loc, tgt, HCL_NULL);
}
#else
#	define hcl_setsynerr(hcl,num,loc,tgt) hcl_setsynerrbfmt(hcl,num,loc,tgt,HCL_NULL)
#endif

/* =========================================================================
 * TEMPORARY OOP MANAGEMENT FUNCTIONS
 * ========================================================================= */
HCL_EXPORT void hcl_pushvolat (
	hcl_t*     hcl,
	hcl_oop_t* oop_ptr
);

HCL_EXPORT void hcl_popvolat (
	hcl_t*     hcl
);

HCL_EXPORT void hcl_popvolats (
	hcl_t*     hcl,
	hcl_oow_t  count
);

/* =========================================================================
 * SYSTEM MEMORY MANAGEMENT FUCNTIONS VIA MMGR
 * ========================================================================= */
HCL_EXPORT void* hcl_allocmem (
	hcl_t*    hcl,
	hcl_oow_t size
);

HCL_EXPORT void* hcl_callocmem (
	hcl_t*    hcl,
	hcl_oow_t size
);

HCL_EXPORT void* hcl_reallocmem (
	hcl_t*    hcl,
	void*     ptr,
	hcl_oow_t size
);

HCL_EXPORT void hcl_freemem (
	hcl_t* hcl,
	void*  ptr
);


/* =========================================================================
 * PRIMITIVE FUNCTION MANIPULATION
 * ========================================================================= */
HCL_EXPORT hcl_pfbase_t* hcl_findpfbase (
	hcl_t*              hcl,
	hcl_pfinfo_t*       pfinfo,
	hcl_oow_t           pfcount,
	const hcl_ooch_t*   name,
	hcl_oow_t           namelen
);


/* =========================================================================
 * LOGGING
 * ========================================================================= */

HCL_EXPORT hcl_ooi_t hcl_logbfmt (
	hcl_t*           hcl,
	hcl_bitmask_t    mask,
	const hcl_bch_t* fmt,
	...
);

HCL_EXPORT hcl_ooi_t hcl_logbfmtv (
	hcl_t*           hcl,
	hcl_bitmask_t    mask,
	const hcl_bch_t* fmt,
	va_list          ap
);

HCL_EXPORT hcl_ooi_t hcl_logufmt (
	hcl_t*           hcl,
	hcl_bitmask_t    mask,
	const hcl_uch_t* fmt,
	...
);

HCL_EXPORT hcl_ooi_t hcl_logufmtv (
	hcl_t*           hcl,
	hcl_bitmask_t    mask,
	const hcl_uch_t* fmt,
	va_list          ap
);

#if defined(HCL_OOCH_IS_UCH)
#	define hcl_logoofmt hcl_logufmt
#	define hcl_logoofmtv hcl_logufmtv
#else
#	define hcl_logoofmt hcl_logbfmt
#	define hcl_logoofmtv hcl_logbfmtv
#endif

HCL_EXPORT hcl_ooi_t hcl_prbfmt (
	hcl_t*           hcl,
	const hcl_bch_t* fmt,
	...
);

HCL_EXPORT hcl_ooi_t hcl_prbfmtv (
	hcl_t*           hcl,
	const hcl_bch_t* fmt,
	va_list          ap
);

HCL_EXPORT hcl_ooi_t hcl_prufmt (
	hcl_t*           hcl,
	const hcl_uch_t* fmt,
	...
);

HCL_EXPORT hcl_ooi_t hcl_prufmtv (
	hcl_t*           hcl,
	const hcl_uch_t* fmt,
	va_list          ap
);

#if defined(HCL_OOCH_IS_UCH)
#	define hcl_proofmt hcl_prufmt
#	define hcl_proofmtv hcl_prufmtv
#else
#	define hcl_proofmt hcl_prbfmt
#	define hcl_proofmtv hcl_prbfmtv
#endif


/* =========================================================================
 * STRING FORMATTING
 * ========================================================================= */

HCL_EXPORT hcl_oow_t hcl_vfmttoucstr (
	hcl_t*           hcl,
	hcl_uch_t*       buf,
	hcl_oow_t        bufsz,
	const hcl_uch_t* fmt,
	va_list          ap
);

HCL_EXPORT hcl_oow_t hcl_fmttoucstr (
	hcl_t*           hcl,
	hcl_uch_t*       buf,
	hcl_oow_t        bufsz,
	const hcl_uch_t* fmt,
	...
);

HCL_EXPORT hcl_oow_t hcl_vfmttobcstr (
	hcl_t*           hcl,
	hcl_bch_t*       buf,
	hcl_oow_t        bufsz,
	const hcl_bch_t* fmt,
	va_list          ap
);

HCL_EXPORT hcl_oow_t hcl_fmttobcstr (
	hcl_t*           hcl,
	hcl_bch_t*       buf,
	hcl_oow_t        bufsz,
	const hcl_bch_t* fmt,
	...
);

#if defined(HCL_OOCH_IS_UCH)
#	define hcl_vfmttooocstr hcl_vfmttoucstr
#	define hcl_fmttooocstr hcl_fmttoucstr
#else
#	define hcl_vfmttooocstr hcl_vfmttobcstr
#	define hcl_fmttooocstr hcl_fmttobcstr
#endif


/* =========================================================================
 * OBJECT MANAGEMENT
 * ========================================================================= */
HCL_EXPORT hcl_oop_t hcl_hatchundef (
	hcl_t*     hcl
);

HCL_EXPORT hcl_oop_t hcl_hatchnil (
	hcl_t*     hcl
);

HCL_EXPORT hcl_oop_t hcl_instantiate (
	hcl_t*          hcl,
	hcl_oop_class_t _class,
	const void*     vptr,
	hcl_oow_t       vlen
);

HCL_EXPORT hcl_oop_t hcl_makecons (
	hcl_t*     hcl,
	hcl_oop_t  car,
	hcl_oop_t  cdr
);

HCL_EXPORT hcl_oop_t hcl_makearray (
	hcl_t*     hcl,
	hcl_oow_t  len
);

HCL_EXPORT hcl_oop_t hcl_makechararray (
	hcl_t*            hcl,
	const hcl_ooch_t* ptr,
	hcl_oow_t         len
);

HCL_EXPORT hcl_oop_t hcl_makebytearray (
	hcl_t*           hcl,
	const hcl_oob_t* ptr,
	hcl_oow_t        len
);

HCL_EXPORT hcl_oop_t hcl_makebytestringwithbytes (
	hcl_t*           hcl,
	const hcl_oob_t* ptr,
	hcl_oow_t        len
);

HCL_EXPORT hcl_oop_t hcl_makebytestring (
	hcl_t*            hcl,
	const hcl_ooch_t* ptr,
	hcl_oow_t         len
);

HCL_EXPORT hcl_oop_t hcl_makestring (
	hcl_t*            hcl,
	const hcl_ooch_t* ptr,
	hcl_oow_t         len
);

HCL_EXPORT hcl_oop_t hcl_makefpdec (
	hcl_t*            hcl,
	hcl_oop_t         value,
	hcl_ooi_t         scale
);

HCL_EXPORT hcl_oop_t hcl_makedic (
	hcl_t*            hcl,
	hcl_oow_t         inisize /* initial bucket size */
);

HCL_EXPORT hcl_oop_t hcl_makeclass (
	hcl_t*            hcl,
	hcl_oop_t         name,
	hcl_oop_t         superclass,
	hcl_ooi_t         nivars,
	hcl_ooi_t         ncvars,
	hcl_oop_t         ivars_str,
	hcl_oop_t         cvars_str
);

HCL_EXPORT void hcl_freengcobj (
	hcl_t*           hcl,
	hcl_oop_t        obj
);

HCL_EXPORT hcl_oop_t hcl_makengcbytearray (
	hcl_t*           hcl,
	const hcl_oob_t* ptr,
	hcl_oow_t        len
);

HCL_EXPORT hcl_oop_t hcl_remakengcbytearray (
	hcl_t*           hcl,
	hcl_oop_t        obj,
	hcl_oow_t        newsz
);

HCL_EXPORT hcl_oop_t hcl_makengcarray (
	hcl_t*           hcl,
	hcl_oow_t        len
);

HCL_EXPORT hcl_oop_t hcl_remakengcarray (
	hcl_t*           hcl,
	hcl_oop_t        obj,
	hcl_oow_t        newsz
);

HCL_EXPORT hcl_oop_t hcl_makeprim (
	hcl_t*          hcl,
	hcl_pfimpl_t    primimpl,
	hcl_oow_t       minargs,
	hcl_oow_t       maxargs,
	hcl_mod_t*      mod
);

HCL_EXPORT hcl_oop_t hcl_oowtoint (
	hcl_t*     hcl,
	hcl_oow_t  w
);

HCL_EXPORT hcl_oop_t hcl_ooitoint (
	hcl_t*    hcl,
	hcl_ooi_t i
);

HCL_EXPORT int hcl_inttooow_noseterr (
	hcl_t*     hcl,
	hcl_oop_t  x,
	hcl_oow_t* w
);

HCL_EXPORT int hcl_inttooow (
	hcl_t*     hcl,
	hcl_oop_t  x,
	hcl_oow_t* w
);

HCL_EXPORT int hcl_inttoooi_noseterr (
	hcl_t*     hcl,
	hcl_oop_t  x,
	hcl_ooi_t* i
);

HCL_EXPORT int hcl_inttoooi (
	hcl_t*     hcl,
	hcl_oop_t  x,
	hcl_ooi_t* i
);

#if (HCL_SIZEOF_UINTMAX_T == HCL_SIZEOF_OOW_T)
#   define hcl_inttouintmax hcl_inttooow
#   define hcl_inttointmax hcl_inttoooi
#   define hcl_uintmaxtoint hcl_oowtoint
#   define hcl_intmaxtoint hcl_ooitoint
#else

HCL_EXPORT hcl_oop_t hcl_intmaxtoint (
	hcl_t*       hcl,
	hcl_intmax_t i
);

HCL_EXPORT hcl_oop_t hcl_uintmaxtoint (
	hcl_t*        hcl,
	hcl_uintmax_t i
);

HCL_EXPORT int hcl_inttouintmax (
	hcl_t*         hcl,
	hcl_oop_t      x,
	hcl_uintmax_t* w
);

HCL_EXPORT int hcl_inttointmax (
	hcl_t*        hcl,
	hcl_oop_t     x,
	hcl_intmax_t* i
);
#endif

/* =========================================================================
 * CONS OBJECT UTILITIES
 * ========================================================================= */
HCL_EXPORT hcl_oow_t hcl_countcons (
	hcl_t*           hcl,
	hcl_oop_t        cons
);


HCL_EXPORT hcl_oop_t hcl_getlastconscdr (
	hcl_t*           hcl,
	hcl_oop_t        cons
);

HCL_EXPORT hcl_oop_t hcl_reversecons (
	hcl_t*           hcl,
	hcl_oop_t        cons
);


/* =========================================================================
 * CODE MARSHALING/UNMARSHALING
 * ========================================================================= */
HCL_EXPORT int hcl_marshalcode (
	hcl_t*            hcl,
	const hcl_code_t* code,
	hcl_xchg_writer_t wrtr,
	void*             ctx
);

HCL_EXPORT int hcl_unmarshalcode (
	hcl_t*            hcl,
	hcl_code_t*       code,
	hcl_xchg_reader_t rdr,
	void*             ctx
);

HCL_EXPORT int hcl_marshalcodetomem (
	hcl_t*            hcl,
	const hcl_code_t* code,
	hcl_ptlc_t*        dst
);

HCL_EXPORT int hcl_unmarshalcodefrommem (
	hcl_t*            hcl,
	hcl_code_t*       code,
	const hcl_ptl_t*  src
);

/* =========================================================================
 * DICTIONARY ACCESS FUNCTIONS
 * ========================================================================= */
HCL_EXPORT hcl_oop_cons_t hcl_putatsysdic (
	hcl_t*     hcl,
	hcl_oop_t  key,
	hcl_oop_t  value
);

HCL_EXPORT hcl_oop_cons_t hcl_getatsysdic (
	hcl_t*     hcl,
	hcl_oop_t  key
);

HCL_EXPORT hcl_oop_cons_t hcl_lookupsysdicforsymbol (
	hcl_t*            hcl,
	const hcl_oocs_t* name
);

HCL_EXPORT hcl_oop_cons_t hcl_lookupsysdicforsymbol_noseterr (
	hcl_t*            hcl,
	const hcl_oocs_t* name
);

HCL_EXPORT int hcl_zapatsysdic (
	hcl_t*     hcl,
	hcl_oop_t  key
);

HCL_EXPORT hcl_oop_cons_t hcl_lookupdicforsymbol (
	hcl_t*            hcl,
	hcl_oop_dic_t     dic,
	const hcl_oocs_t* name
);

HCL_EXPORT hcl_oop_cons_t hcl_lookupdicforsymbol_noseterr (
	hcl_t*            hcl,
	hcl_oop_dic_t     dic,
	const hcl_oocs_t* name
);

HCL_EXPORT hcl_oop_cons_t hcl_putatdic (
	hcl_t*        hcl,
	hcl_oop_dic_t dic,
	hcl_oop_t     key,
	hcl_oop_t     value
);

HCL_EXPORT hcl_oop_cons_t hcl_getatdic (
	hcl_t*        hcl,
	hcl_oop_dic_t dic,
	hcl_oop_t     key
);


HCL_EXPORT int hcl_zapatdic (
	hcl_t*        hcl,
	hcl_oop_dic_t dic,
	hcl_oop_t     key
);

HCL_EXPORT int hcl_walkdic (
	hcl_t*           hcl,
	hcl_oop_dic_t    dic,
	hcl_dic_walker_t walker,
	void*            ctx
);



/* =========================================================================
 * OBJECT HASHING AND COMPARISION
 * ========================================================================= */

HCL_EXPORT int hcl_hashobj (
	hcl_t*     hcl,
	hcl_oop_t  obj,
	hcl_oow_t* xhv
);

HCL_EXPORT int hcl_equalobjs (
	hcl_t*     hcl,
	hcl_oop_t  rcv,
	hcl_oop_t  arg
);


/* =========================================================================
 * STRING ENCODING CONVERSION
 * ========================================================================= */

#if defined(HCL_OOCH_IS_UCH)
#	define hcl_convootobchars(hcl,oocs,oocslen,bcs,bcslen) hcl_convutobchars(hcl,oocs,oocslen,bcs,bcslen)
#	define hcl_convbtooochars(hcl,bcs,bcslen,oocs,oocslen) hcl_convbtouchars(hcl,bcs,bcslen,oocs,oocslen)
#	define hcl_convootobcstr(hcl,oocs,oocslen,bcs,bcslen) hcl_convutobcstr(hcl,oocs,oocslen,bcs,bcslen)
#	define hcl_convbtooocstr(hcl,bcs,bcslen,oocs,oocslen) hcl_convbtoucstr(hcl,bcs,bcslen,oocs,oocslen)
#else
#	define hcl_convootouchars(hcl,oocs,oocslen,ucs,ucslen) hcl_convbtouchars(hcl,oocs,oocslen,ucs,ucslen)
#	define hcl_convutooochars(hcl,ucs,ucslen,oocs,oocslen) hcl_convutobchars(hcl,ucs,ucslen,oocs,oocslen)
#	define hcl_convootoucstr(hcl,oocs,oocslen,ucs,ucslen) hcl_convbtoucstr(hcl,oocs,oocslen,ucs,ucslen)
#	define hcl_convutooocstr(hcl,ucs,ucslen,oocs,oocslen) hcl_convutobcstr(hcl,ucs,ucslen,oocs,oocslen)
#endif

HCL_EXPORT int hcl_convbtouchars (
	hcl_t*           hcl,
	const hcl_bch_t* bcs,
	hcl_oow_t*       bcslen,
	hcl_uch_t*       ucs,
	hcl_oow_t*       ucslen
);

HCL_EXPORT int hcl_convutobchars (
	hcl_t*           hcl,
	const hcl_uch_t* ucs,
	hcl_oow_t*       ucslen,
	hcl_bch_t*       bcs,
	hcl_oow_t*       bcslen
);


/**
 * The hcl_convbtoucstr() function converts a null-terminated byte string
 * to a wide string.
 */
HCL_EXPORT int hcl_convbtoucstr (
	hcl_t*           hcl,
	const hcl_bch_t* bcs,
	hcl_oow_t*       bcslen,
	hcl_uch_t*       ucs,
	hcl_oow_t*       ucslen
);


/**
 * The hcl_convutobcstr() function converts a null-terminated wide string
 * to a byte string.
 */
HCL_EXPORT int hcl_convutobcstr (
	hcl_t*           hcl,
	const hcl_uch_t* ucs,
	hcl_oow_t*       ucslen,
	hcl_bch_t*       bcs,
	hcl_oow_t*       bcslen
);

#if defined(HCL_OOCH_IS_UCH)
#	define hcl_dupootobcharswithheadroom(hcl,hrb,oocs,oocslen,bcslen) hcl_duputobcharswithheadroom(hcl,hrb,oocs,oocslen,bcslen)
#	define hcl_dupbtooocharswithheadroom(hcl,hrb,bcs,bcslen,oocslen) hcl_dupbtoucharswithheadroom(hcl,hrb,bcs,bcslen,oocslen)
#	define hcl_dupootobchars(hcl,oocs,oocslen,bcslen) hcl_duputobchars(hcl,oocs,oocslen,bcslen)
#	define hcl_dupbtooochars(hcl,bcs,bcslen,oocslen) hcl_dupbtouchars(hcl,bcs,bcslen,oocslen)

#	define hcl_dupootobcstrwithheadroom(hcl,hrb,oocs,bcslen) hcl_duputobcstrwithheadroom(hcl,hrb,oocs,bcslen)
#	define hcl_dupbtooocstrwithheadroom(hcl,hrb,bcs,oocslen) hcl_dupbtoucstrwithheadroom(hcl,hrb,bcs,oocslen)
#	define hcl_dupootobcstr(hcl,oocs,bcslen) hcl_duputobcstr(hcl,oocs,bcslen)
#	define hcl_dupbtooocstr(hcl,bcs,oocslen) hcl_dupbtoucstr(hcl,bcs,oocslen)

#   define hcl_dupootoucstr(hcl,oocs,ucslen) hcl_dupucstr(hcl,oocs,ucslen)
#   define hcl_duputooocstr(hcl,ucs,oocslen) hcl_dupucstr(hcl,ucs,oocslen)
#else
#	define hcl_dupootoucharswithheadroom(hcl,hrb,oocs,oocslen,ucslen) hcl_dupbtoucharswithheadroom(hcl,hrb,oocs,oocslen,ucslen)
#	define hcl_duputooocharswithheadroom(hcl,hrb,ucs,ucslen,oocslen) hcl_duputobcharswithheadroom(hcl,hrb,ucs,ucslen,oocslen)
#	define hcl_dupootouchars(hcl,oocs,oocslen,ucslen) hcl_dupbtouchars(hcl,oocs,oocslen,ucslen)
#	define hcl_duputooochars(hcl,ucs,ucslen,oocslen) hcl_duputobchars(hcl,ucs,ucslen,oocslen)

#	define hcl_dupootoucstrwithheadroom(hcl,hrb,oocs,ucslen) hcl_dupbtoucstrwithheadroom(hcl,hrb,oocs,ucslen)
#	define hcl_duputooocstrwithheadroom(hcl,hrb,ucs,oocslen) hcl_duputobcstrwithheadroom(hcl,hrb,ucs,oocslen)
#	define hcl_dupootoucstr(hcl,oocs,ucslen) hcl_dupbtoucstr(hcl,oocs,ucslen)
#	define hcl_duputooocstr(hcl,ucs,oocslen) hcl_duputobcstr(hcl,ucs,oocslen)

#	define hcl_dupootobcstr(hcl,oocs,bcslen) hcl_dupbcstr(hcl,oocs,bcslen)
#	define hcl_dupbtooocstr(hcl,bcs,oocslen) hcl_dupbcstr(hcl,bcs,oocslen)
#endif


HCL_EXPORT hcl_uch_t* hcl_dupbtoucharswithheadroom (
	hcl_t*           hcl,
	hcl_oow_t        headroom_bytes,
	const hcl_bch_t* bcs,
	hcl_oow_t        bcslen,
	hcl_oow_t*       ucslen
);

HCL_EXPORT hcl_bch_t* hcl_duputobcharswithheadroom (
	hcl_t*           hcl,
	hcl_oow_t        headroom_bytes,
	const hcl_uch_t* ucs,
	hcl_oow_t        ucslen,
	hcl_oow_t*       bcslen
);

HCL_EXPORT hcl_uch_t* hcl_dupbtouchars (
	hcl_t*           hcl,
	const hcl_bch_t* bcs,
	hcl_oow_t        bcslen,
	hcl_oow_t*       ucslen
);

HCL_EXPORT hcl_bch_t* hcl_duputobchars (
	hcl_t*           hcl,
	const hcl_uch_t* ucs,
	hcl_oow_t        ucslen,
	hcl_oow_t*       bcslen
);


HCL_EXPORT hcl_uch_t* hcl_dupbtoucstrwithheadroom (
	hcl_t*           hcl,
	hcl_oow_t        headroom_bytes,
	const hcl_bch_t* bcs,
	hcl_oow_t*       ucslen
);

HCL_EXPORT hcl_bch_t* hcl_duputobcstrwithheadroom (
	hcl_t*           hcl,
	hcl_oow_t        headroom_bytes,
	const hcl_uch_t* ucs,
	hcl_oow_t* bcslen
);

HCL_EXPORT hcl_uch_t* hcl_dupbtoucstr (
	hcl_t*           hcl,
	const hcl_bch_t* bcs,
	hcl_oow_t*       ucslen /* optional: length of returned string */
);

HCL_EXPORT hcl_bch_t* hcl_duputobcstr (
	hcl_t*           hcl,
	const hcl_uch_t* ucs,
	hcl_oow_t*       bcslen /* optional: length of returned string */
);


#if defined(HCL_OOCH_IS_UCH)
#	define hcl_dupoochars(hcl,oocs,oocslen) hcl_dupuchars(hcl,oocs,oocslen)
#	define hcl_dupoocstr(hcl,oocs,oocslen) hcl_dupucstr(hcl,oocs,oocslen)
#else
#	define hcl_dupoochars(hcl,oocs,oocslen) hcl_dupbchars(hcl,oocs,oocslen)
#   define hcl_dupoocstr(hcl,oocs,oocslen) hcl_dupbcstr(hcl,oocs,oocslen)
#endif

HCL_EXPORT hcl_uch_t* hcl_dupuchars (
    hcl_t*           hcl,
    const hcl_uch_t* ucs,
    hcl_oow_t        ucslen
);

HCL_EXPORT hcl_bch_t* hcl_dupbchars (
    hcl_t*           hcl,
    const hcl_bch_t* bcs,
    hcl_oow_t        bcslen
);

HCL_EXPORT hcl_uch_t* hcl_dupucstr (
    hcl_t*           hcl,
    const hcl_uch_t* ucs,
    hcl_oow_t*       ucslen
);

HCL_EXPORT hcl_bch_t* hcl_dupbcstr (
    hcl_t*           hcl,
    const hcl_bch_t* bcs,
    hcl_oow_t*       bcslen
);

/* =========================================================================
 * ASSERTION SUPPORT
 * ========================================================================= */
HCL_EXPORT void hcl_assertfailed (
	hcl_t*           hcl,
	const hcl_bch_t* expr,
	const hcl_bch_t* file,
	hcl_oow_t        line
);


/* =========================================================================
 * HELPERS
 * ========================================================================= */
HCL_EXPORT void hcl_start_ticker (
	void
);

HCL_EXPORT void hcl_stop_ticker (
	void
);

HCL_EXPORT void hcl_catch_termreq (
	void
);

HCL_EXPORT void hcl_uncatch_termreq (
	void
);

#if defined(__cplusplus)
}
#endif

#endif
