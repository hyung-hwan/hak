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

#if defined(HCL_PROFILE_VM)
#include <sys/time.h>
#include <sys/resource.h> /* getrusage */
#endif

static struct
{
	hcl_oow_t  len;
	hcl_ooch_t ptr[20];
	int syncode;
	hcl_oow_t  offset;
} syminfo[] =
{
	{  3, { 'a','n','d' },                      HCL_SYNCODE_AND,       HCL_OFFSETOF(hcl_t,s_and)    },
	{  5, { 'b','r','e','a','k' },              HCL_SYNCODE_BREAK,     HCL_OFFSETOF(hcl_t,s_break)  },
	{  5, { 'c','a','t','c','h' },              HCL_SYNCODE_CATCH,     HCL_OFFSETOF(hcl_t,s_catch)  },
	{  5, { 'c','l','a','s','s' },              HCL_SYNCODE_CLASS,     HCL_OFFSETOF(hcl_t,s_class)  },
	{  8, { 'c','o','n','t','i','n','u','e' },  HCL_SYNCODE_CONTINUE,  HCL_OFFSETOF(hcl_t,s_continue) },
	{  8, { 'd','e','f','c','l','a','s','s' },  HCL_SYNCODE_DEFCLASS,  HCL_OFFSETOF(hcl_t,s_defclass) },
	{  5, { 'd','e','f','u','n' },              HCL_SYNCODE_DEFUN,     HCL_OFFSETOF(hcl_t,s_defun)  },
	{  2, { 'd','o' },                          HCL_SYNCODE_DO,        HCL_OFFSETOF(hcl_t,s_do)     },
	{  4, { 'e','l','i','f' },                  HCL_SYNCODE_ELIF,      HCL_OFFSETOF(hcl_t,s_elif)   },
	{  4, { 'e','l','s','e' },                  HCL_SYNCODE_ELSE,      HCL_OFFSETOF(hcl_t,s_else)   },
	{  3, { 'f','u','n' },                      HCL_SYNCODE_LAMBDA,    HCL_OFFSETOF(hcl_t,s_fun)    }, /* same syncode as lambda */
	{  2, { 'i','f' },                          HCL_SYNCODE_IF,        HCL_OFFSETOF(hcl_t,s_if)     },
	{  6, { 'l','a','m','b','d','a' },          HCL_SYNCODE_LAMBDA,    HCL_OFFSETOF(hcl_t,s_lambda) },
	{  2, { 'o','r' },                          HCL_SYNCODE_OR,        HCL_OFFSETOF(hcl_t,s_or)     },
	{  4, { 'p','l','u','s' },                  HCL_SYNCODE_PLUS,      HCL_OFFSETOF(hcl_t,s_plus)   },
	{  6, { 'r','e','t','u','r','n'},           HCL_SYNCODE_RETURN,    HCL_OFFSETOF(hcl_t,s_return) },
	{ 16, { 'r','e','t','u','r','n','-','f','r','o','m','-','h','o','m','e'},
	                                            HCL_SYNCODE_RETURN_FROM_HOME,  HCL_OFFSETOF(hcl_t,s_return_from_home) },
	{  3, { 's','e','t' },                      HCL_SYNCODE_SET,       HCL_OFFSETOF(hcl_t,s_set)    },
	{  5, { 's','e','t','-','r' },              HCL_SYNCODE_SET_R,     HCL_OFFSETOF(hcl_t,s_set_r)  },
	{  5, { 't','h','r','o','w' },              HCL_SYNCODE_THROW,     HCL_OFFSETOF(hcl_t,s_throw)  },
	{  3, { 't','r','y' },                      HCL_SYNCODE_TRY,       HCL_OFFSETOF(hcl_t,s_try)   },
	{  5, { 'u','n','t','i','l' },              HCL_SYNCODE_UNTIL,     HCL_OFFSETOF(hcl_t,s_until)  },
	{  5, { 'w','h','i','l','e' },              HCL_SYNCODE_WHILE,     HCL_OFFSETOF(hcl_t,s_while)  }
};

/* ========================================================================= */



static struct
{
	const char* name;
	hcl_oow_t c_offset; /* class offset */
	hcl_oow_t sc_offset; /* superclass offset */
	hcl_oow_t nivars;
	hcl_oow_t ncvars;
} kctab[] = {
	{ "Apex",
	  HCL_OFFSETOF(hcl_t, c_apex),
	  HCL_TYPE_MAX(hcl_oow_t),
	  0, 0 },

	{ "Object",
	  HCL_OFFSETOF(hcl_t, c_object),
	  HCL_OFFSETOF(hcl_t, c_apex),
	  0, 0 },

	{ "UndefinedObject",
	  HCL_OFFSETOF(hcl_t, c_undefobj),
	  HCL_OFFSETOF(hcl_t, c_apex),
	  0, 0 },

	{ "Class",
	  HCL_OFFSETOF(hcl_t, c_class),
	  HCL_OFFSETOF(hcl_t, c_object),
	  HCL_CLASS_NAMED_INSTVARS, 0 },

	{ "String",
	  HCL_OFFSETOF(hcl_t, c_string),
	  HCL_OFFSETOF(hcl_t, c_object),
	  0, 0 },

	{ "Symbol",
	  HCL_OFFSETOF(hcl_t, c_symbol),
	  HCL_OFFSETOF(hcl_t, c_string),
	  0, 0 },

#if 0
	{ "Boolean",
	  HCL_OFFSETOF(hcl_t, c_boolean),
	  HCL_OFFSETOF(hcl_t, c_object),
	  0, 0 },

	{ "True",
	  HCL_OFFSETOF(hcl_t, c_true),
	  HCL_OFFSETOF(hcl_t, c_boolean),
	  0, 0 },

	{ "False",
	  HCL_OFFSETOF(hcl_t, c_false),
	  HCL_OFFSETOF(hcl_t, c_boolean),
	  0, 0 },
#endif

	{ "System",
	  HCL_OFFSETOF(hcl_t, c_system),
	  HCL_OFFSETOF(hcl_t, c_object),
	  0, 0 },
};

/* ========================================================================= */

/*
 *    Apex......................
 *    ^ ^ ^                    :   .......
 *    | | |                    v   v     :
 *    | | +------------------- Class .....
 *    | |                       ^  ^
 *    | +-------- NilObject ......:  :
 *    |                 ^........ nil  :
 *   Object ...........................:
 *    ^
 *    |
 *
 * The class hierarchy is roughly as follows:
 *
 *   Apex
 *      Class
 *      NilObject
 *      Object
 *         Collection
 *            IndexedCollection
 *               FixedSizedCollection
 *                  Array
 *                  ByteArray
 *                     String
 *                        Symbol
 *            Set
 *               Dictionary
 *                  SystemDictionary
 *               SymbolSet
 *         Magnitude
 *            Association
 *            Character
 *            Number
 *               Integer
 *                  SmallInteger
 *                  LargeInteger
 *                     LargePositiveInteger
 *                     LargeNegativeInteger
 *
 * Apex has no instance variables.
 *
 */

struct kernel_class_info_t
{
	const hcl_bch_t* name;
	int class_flags;
	int class_num_classvars;

	int class_spec_named_instvars;
	int class_spec_flags;
	int class_spec_indexed_type;

	hcl_oow_t  offset;
};
typedef struct kernel_class_info_t kernel_class_info_t;

static kernel_class_info_t kernel_classes[] =
{
	/* --------------------------------------------------------------
	 * Apex            - proto-object with 1 class variable.
	 * UndefinedObject - class for the nil object.
	 * Object          - top of all ordinary objects.
	 * String
	 * Symbol
	 * Array
	 * ByteArray
	 * SymbolSet
	 * Character
	 * SmallIntger
	 * -------------------------------------------------------------- */

	{ "Apex",
	  0,
	  0,
	  0,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_apex) },

	{ "UndefinedObject",
	  0,
	  0,
	  0,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_undefobj) },

#define KCI_CLASS 2 /* index to the Class entry in this table */
	{ "Class",
	  HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  HCL_CLASS_NAMED_INSTVARS,
	  HCL_CLASS_SPEC_FLAG_INDEXED | HCL_CLASS_SPEC_FLAG_UNCOPYABLE,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_class) },

#if 0
	{ "Interface",
	  HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  HCL_INTERFACE_NAMED_INSTVARS,
	  HCL_CLASS_SPEC_FLAG_INDEXED | HCL_CLASS_SPEC_FLAG_UNCOPYABLE,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, _interface) },
#endif

	{ "Object",
	  0,
	  0,
	  0,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_object) },

	{ "String",
	  0,
	  0,
	  0,
	  HCL_CLASS_SPEC_FLAG_INDEXED,
	  HCL_OBJ_TYPE_CHAR,
	  HCL_OFFSETOF(hcl_t, c_string) },

	{ "Symbol",
	  HCL_CLASS_SELFSPEC_FLAG_FINAL | HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  0,
	  HCL_CLASS_SPEC_FLAG_INDEXED | HCL_CLASS_SPEC_FLAG_IMMUTABLE,
	  HCL_OBJ_TYPE_CHAR,
	  HCL_OFFSETOF(hcl_t, c_symbol) },

	{ "Array",
	  0,
	  0,
	  0,
	  HCL_CLASS_SPEC_FLAG_INDEXED,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_array) },

	{ "ByteArray",
	  0,
	  0,
	  0,
	  HCL_CLASS_SPEC_FLAG_INDEXED,
	  HCL_OBJ_TYPE_BYTE,
	  HCL_OFFSETOF(hcl_t, c_byte_array) },

	{ "SymbolTable",
	  0,
	  0,
	  HCL_DIC_NAMED_INSTVARS,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_symtab) },

	{ "Dictionary",
	  0,
	  0,
	  HCL_DIC_NAMED_INSTVARS,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_dictionary) },

	{ "Cons",
	  0,
	  0,
	  HCL_CONS_NAMED_INSTVARS,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_cons) },

#if 0
	{ "Namespace",
	  HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  HCL_NSDIC_NAMED_INSTVARS,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_namespace) },

	{ "PoolDictionary",
	  0,
	  0,
	  HCL_DIC_NAMED_INSTVARS,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_pool_dictionary) },
#endif

	{ "MethodDictionary",
	  0,
	  0,
	  HCL_DIC_NAMED_INSTVARS,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_method_dictionary) },

#if 0
	{ "CompiledMethod",
	  0,
	  0,
	  HCL_METHOD_NAMED_INSTVARS,
	  HCL_CLASS_SPEC_FLAG_INDEXED,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_method) },

	{ "MethodSignature",
	  0,
	  0,
	  HCL_METHSIG_NAMED_INSTVARS,
	  HCL_CLASS_SPEC_FLAG_INDEXED,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_methsig) },
#endif

	{ "CompiledBlock",
	  0,
	  0,
	  HCL_BLOCK_NAMED_INSTVARS,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_block) },

	{ "MethodContext",
	  HCL_CLASS_SELFSPEC_FLAG_FINAL | HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  HCL_CONTEXT_NAMED_INSTVARS,
	  HCL_CLASS_SPEC_FLAG_INDEXED,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_method_context) },

	{ "BlockContext",
	  HCL_CLASS_SELFSPEC_FLAG_FINAL | HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  HCL_CONTEXT_NAMED_INSTVARS,
	  HCL_CLASS_SPEC_FLAG_INDEXED,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_block_context) },

	{ "Process",
	  HCL_CLASS_SELFSPEC_FLAG_FINAL | HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  HCL_PROCESS_NAMED_INSTVARS,
	  HCL_CLASS_SPEC_FLAG_INDEXED | HCL_CLASS_SPEC_FLAG_UNCOPYABLE,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_process) },

	{ "Semaphore",
	  0,
	  0,
	  HCL_SEMAPHORE_NAMED_INSTVARS,
	  HCL_CLASS_SPEC_FLAG_UNCOPYABLE,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_semaphore) },

	{ "SemaphoreGroup",
	  0,
	  0,
	  HCL_SEMAPHORE_GROUP_NAMED_INSTVARS,
	  HCL_CLASS_SPEC_FLAG_UNCOPYABLE,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_semaphore_group) },

	{ "ProcessScheduler",
	  HCL_CLASS_SELFSPEC_FLAG_FINAL | HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  HCL_PROCESS_SCHEDULER_NAMED_INSTVARS,
	  HCL_CLASS_SPEC_FLAG_UNCOPYABLE,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_process_scheduler) },

	{ "Error",
	  HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  0,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_error) },

	{ "True",
	  HCL_CLASS_SELFSPEC_FLAG_LIMITED | HCL_CLASS_SELFSPEC_FLAG_FINAL,
	  0,
	  0,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_true) },

	{ "False",
	  HCL_CLASS_SELFSPEC_FLAG_LIMITED | HCL_CLASS_SELFSPEC_FLAG_FINAL,
	  0,
	  0,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_false) },

	/* TOOD: what is a proper spec for Character and SmallInteger?
	 *       If the fixed part is  0, its instance must be an object of 0 payload fields.
	 *       Does this make sense? */
	{ "Character",
	  HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  0,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_character) },

	{ "SmallInteger",
	  HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  0,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_small_integer) },

	{ "LargePositiveInteger",
	  HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  0,
	  HCL_CLASS_SPEC_FLAG_INDEXED | HCL_CLASS_SPEC_FLAG_IMMUTABLE,
	  HCL_OBJ_TYPE_LIWORD,
	  HCL_OFFSETOF(hcl_t, c_large_positive_integer) },

	{ "LargeNegativeInteger",
	  HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  0,
	  HCL_CLASS_SPEC_FLAG_INDEXED | HCL_CLASS_SPEC_FLAG_IMMUTABLE,
	  HCL_OBJ_TYPE_LIWORD,
	  HCL_OFFSETOF(hcl_t, c_large_negative_integer) },

	{ "FixedPointDecimal",
	  HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  HCL_FPDEC_NAMED_INSTVARS,
	  HCL_CLASS_SPEC_FLAG_IMMUTABLE,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_fixed_point_decimal) },

	{ "SmallPointer",
	  HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  0,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_small_pointer) },

	{ "LargePointer",
	  HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  1,  /* #word(1) */
	  HCL_CLASS_SPEC_FLAG_IMMUTABLE | HCL_CLASS_SPEC_FLAG_INDEXED,
	  HCL_OBJ_TYPE_WORD,
	  HCL_OFFSETOF(hcl_t, c_large_pointer) },

	{ "System",
	  0,
	  5, /* asyncsg, gcfin_sem, gcfin_should_exit, ossig_pid, shr */
	  0,
	  0,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, c_system) }
};

/* ========================================================================= */

static void compact_symbol_table (hcl_t* hcl, hcl_oop_t _nil)
{
	hcl_oop_char_t symbol;
	hcl_oow_t i, x, y, z;
	hcl_oow_t bucket_size, index;
	hcl_ooi_t tally;

#if defined(HCL_SUPPORT_GC_DURING_IGNITION)
	if (!hcl->symtab) return; /* symbol table has not been created */
#endif

	/* the symbol table doesn't allow more data items than HCL_SMOOI_MAX.
	 * so hcl->symtab->tally must always be a small integer */
	HCL_ASSERT (hcl, HCL_OOP_IS_SMOOI(hcl->symtab->tally));
	tally = HCL_OOP_TO_SMOOI(hcl->symtab->tally);
	HCL_ASSERT (hcl, tally >= 0); /* it must not be less than 0 */
	if (tally <= 0) return;

	/* NOTE: in theory, the bucket size can be greater than HCL_SMOOI_MAX
	 * as it is an internal header field and is of an unsigned type */
	bucket_size = HCL_OBJ_GET_SIZE(hcl->symtab->bucket);

	for (index = 0; index < bucket_size; )
	{
		if (HCL_OBJ_GET_FLAGS_MOVED(hcl->symtab->bucket->slot[index]))
		{
			index++;
			continue;
		}

		HCL_ASSERT (hcl, hcl->symtab->bucket->slot[index] != _nil);

		for (i = 0, x = index, y = index; i < bucket_size; i++)
		{
			y = (y + 1) % bucket_size;

			/* done if the slot at the current hash index is _nil */
			if (hcl->symtab->bucket->slot[y] == _nil) break;

			/* get the natural hash index for the data in the slot
			 * at the current hash index */
			symbol = (hcl_oop_char_t)hcl->symtab->bucket->slot[y];

			HCL_ASSERT (hcl, HCL_IS_SYMBOL(hcl, symbol));

			z = hcl_hash_oochars(symbol->slot, HCL_OBJ_GET_SIZE(symbol)) % bucket_size;

			/* move an element if necessary */
			if ((y > x && (z <= x || z > y)) ||
			    (y < x && (z <= x && z > y)))
			{
				hcl->symtab->bucket->slot[x] = hcl->symtab->bucket->slot[y];
				x = y;
			}
		}

		hcl->symtab->bucket->slot[x] = _nil;
		tally--;
	}

	HCL_ASSERT (hcl, tally >= 0);
	HCL_ASSERT (hcl, tally <= HCL_SMOOI_MAX);
	hcl->symtab->tally = HCL_SMOOI_TO_OOP(tally);
}

hcl_oow_t hcl_getobjpayloadbytes (hcl_t* hcl, hcl_oop_t oop)
{
	hcl_oow_t nbytes_aligned;

	if (HCL_OBJ_GET_FLAGS_TRAILER(oop))
	{
		hcl_oow_t nbytes;

		/* only an OOP object can have the trailer.
		 *
		 * | _flags    |
		 * | _size     |  <-- if it's 3
		 * | _class    |
		 * |   X       |
		 * |   X       |
		 * |   X       |
		 * |   Y       | <-- it may exist if EXTRA is set in _flags.
		 * |   Z       | <-- if TRAILER is set, it is the number of bytes in the trailer
		 * |  |  |  |  |
		 */
		HCL_ASSERT (hcl, HCL_OBJ_GET_FLAGS_TYPE(oop) == HCL_OBJ_TYPE_OOP);
		HCL_ASSERT (hcl, HCL_OBJ_GET_FLAGS_UNIT(oop) == HCL_SIZEOF(hcl_oow_t));
		HCL_ASSERT (hcl, HCL_OBJ_GET_FLAGS_EXTRA(oop) == 0); /* no 'extra' for an OOP object */

		nbytes = HCL_OBJ_BYTESOF(oop) + HCL_SIZEOF(hcl_oow_t) + HCL_OBJ_GET_TRAILER_SIZE(oop);
		nbytes_aligned = HCL_ALIGN(nbytes, HCL_SIZEOF(hcl_oop_t));
	}
	else
	{
		/* calculate the payload size in bytes */
		nbytes_aligned = HCL_ALIGN(HCL_OBJ_BYTESOF(oop), HCL_SIZEOF(hcl_oop_t));
	}

	return nbytes_aligned;
}


/* ----------------------------------------------------------------------- */

#if 0
static HCL_INLINE void gc_ms_mark (hcl_t* hcl, hcl_oop_t oop)
{
	hcl_oow_t i, sz;

#if defined(HCL_SUPPORT_GC_DURING_IGNITION)
	if (!oop) return;
#endif

	if (!HCL_OOP_IS_POINTER(oop)) return;
	if (HCL_OBJ_GET_FLAGS_MOVED(oop)) return; /* already marked */

	HCL_OBJ_SET_FLAGS_MOVED(oop, 1); /* mark */

	/*gc_ms_mark (hcl, (hcl_oop_t)HCL_OBJ_GET_CLASS(oop));*/ /* TODO: remove recursion */

	if (HCL_OBJ_GET_FLAGS_TYPE(oop) == HCL_OBJ_TYPE_OOP)
	{
		hcl_oow_t size, i;

		/* is it really better to use a flag bit in the header to
		 * determine that it is an instance of process? */
		if (HCL_UNLIKELY(HCL_OBJ_GET_FLAGS_PROC(oop)))
		{
			/* the stack in a process object doesn't need to be
			 * scanned in full. the slots above the stack pointer
			 * are garbages. */
			size = HCL_PROCESS_NAMED_INSTVARS + HCL_OOP_TO_SMOOI(((hcl_oop_process_t)oop)->sp) + 1;
			HCL_ASSERT (hcl, size <= HCL_OBJ_GET_SIZE(oop));
		}
		else
		{
			size = HCL_OBJ_GET_SIZE(oop);
		}

		for (i = 0; i < size; i++)
		{
			hcl_oop_t tmp = HCL_OBJ_GET_OOP_VAL(oop, i);
			if (HCL_OOP_IS_POINTER(tmp)) gc_ms_mark (hcl, tmp);  /* TODO: no resursion */
		}
	}
}
#else
static HCL_INLINE void gc_ms_mark_object (hcl_t* hcl, hcl_oop_t oop)
{
#if defined(HCL_SUPPORT_GC_DURING_IGNITION)
	if (!oop) return;
#endif
	if (!HCL_OOP_IS_POINTER(oop) || HCL_OBJ_GET_FLAGS_MOVED(oop)) return; /* non-pointer or already marked */

	HCL_OBJ_SET_FLAGS_MOVED(oop, 1); /* mark */
HCL_ASSERT (hcl, hcl->gci.stack.len < hcl->gci.stack.capa);
	hcl->gci.stack.ptr[hcl->gci.stack.len++] = oop; /* push */
if (hcl->gci.stack.len > hcl->gci.stack.max) hcl->gci.stack.max = hcl->gci.stack.len;
}

static HCL_INLINE void gc_ms_scan_stack (hcl_t* hcl)
{
	hcl_oop_t oop;

	while (hcl->gci.stack.len > 0)
	{
		oop = hcl->gci.stack.ptr[--hcl->gci.stack.len];

		gc_ms_mark_object (hcl, (hcl_oop_t)HCL_OBJ_GET_CLASS(oop));

		if (HCL_OBJ_GET_FLAGS_TYPE(oop) == HCL_OBJ_TYPE_OOP)
		{
			hcl_ooi_t i, ll;

			/* is it really better to use a flag bit in the header to
			 * determine that it is an instance of process? */
			/* if (HCL_UNLIKELY(HCL_OBJ_GET_FLAGS_PROC(oop))) */
			if (HCL_OBJ_GET_FLAGS_BRAND(oop) == HCL_BRAND_PROCESS)
			{
				hcl_oop_process_t proc;

				/* the stack in a process object doesn't need to be
				 * scanned in full. the slots above the stack pointer
				 * are garbages. */
				proc = (hcl_oop_process_t)oop;

				/* the fixed part */
				ll = HCL_PROCESS_NAMED_INSTVARS;
				for (i = 0; i < ll; i++) gc_ms_mark_object (hcl, HCL_OBJ_GET_OOP_VAL(oop, i));

				/* stack */
				ll = HCL_OOP_TO_SMOOI(proc->sp);
				HCL_ASSERT (hcl, ll < (hcl_ooi_t)(HCL_OBJ_GET_SIZE(oop) - HCL_PROCESS_NAMED_INSTVARS));
				for (i = 0; i <= ll; i++) gc_ms_mark_object (hcl, proc->slot[i]);
				/* exception stack */
				ll = HCL_OOP_TO_SMOOI(proc->exsp);
				HCL_ASSERT (hcl, ll < (hcl_ooi_t)(HCL_OBJ_GET_SIZE(oop) - HCL_PROCESS_NAMED_INSTVARS));
				for (i = HCL_OOP_TO_SMOOI(proc->st) + 1; i <= ll; i++) gc_ms_mark_object (hcl, proc->slot[i]);
				/* class stack */
				ll = HCL_OOP_TO_SMOOI(proc->clsp);
				HCL_ASSERT (hcl, ll < (hcl_ooi_t)(HCL_OBJ_GET_SIZE(oop) - HCL_PROCESS_NAMED_INSTVARS));
				for (i = HCL_OOP_TO_SMOOI(proc->exst) + 1; i <= ll; i++) gc_ms_mark_object (hcl, proc->slot[i]);
			}
			else
			{
				ll = HCL_OBJ_GET_SIZE(oop);
				for (i = 0; i < ll; i++) gc_ms_mark_object (hcl, HCL_OBJ_GET_OOP_VAL(oop, i));
			}
		}
	}
}

static HCL_INLINE void gc_ms_mark (hcl_t* hcl, hcl_oop_t oop)
{
	gc_ms_mark_object (hcl, oop);
	gc_ms_scan_stack (hcl);
}
#endif

static HCL_INLINE void gc_ms_mark_roots (hcl_t* hcl)
{
	hcl_oow_t i;
#if defined(ENABLE_GCFIN)
	hcl_oow_t gcfin_count;
#endif
	hcl_cb_t* cb;

#if defined(HCL_PROFILE_VM)
	struct rusage ru;
	hcl_ntime_t rut;
	getrusage(RUSAGE_SELF, &ru);
	HCL_INIT_NTIME (&rut,  ru.ru_utime.tv_sec, HCL_USEC_TO_NSEC(ru.ru_utime.tv_usec));
#endif

	if (hcl->processor && hcl->processor->active)
	{
		HCL_ASSERT (hcl, (hcl_oop_t)hcl->processor != hcl->_nil);
		HCL_ASSERT (hcl, (hcl_oop_t)hcl->processor->active != hcl->_nil);

		/* commit the stack pointer to the active process because
		 * gc needs the correct stack pointer for a process object */
		hcl->processor->active->sp = HCL_SMOOI_TO_OOP(hcl->sp);
	}

	gc_ms_mark (hcl, hcl->_undef);
	gc_ms_mark (hcl, hcl->_nil);
	gc_ms_mark (hcl, hcl->_true);
	gc_ms_mark (hcl, hcl->_false);

	for (i = 0; i < HCL_COUNTOF(syminfo); i++)
	{
		gc_ms_mark (hcl, *(hcl_oop_t*)((hcl_uint8_t*)hcl + syminfo[i].offset));
	}

	for (i = 0; i < HCL_COUNTOF(kernel_classes); i++)
	{
		gc_ms_mark (hcl, *(hcl_oop_t*)((hcl_uint8_t*)hcl + kernel_classes[i].offset));
	}

	gc_ms_mark (hcl, (hcl_oop_t)hcl->sysdic);
	gc_ms_mark (hcl, (hcl_oop_t)hcl->processor);
	gc_ms_mark (hcl, (hcl_oop_t)hcl->nil_process);


	for (i = 0; i < hcl->code.lit.len; i++)
	{
		/* the literal array ia a NGC object. but the literal objects
		 * pointed by the elements of this array must be gabage-collected. */
		gc_ms_mark (hcl, ((hcl_oop_oop_t)hcl->code.lit.arr)->slot[i]);
	}
	gc_ms_mark (hcl, hcl->p.e);

	for (i = 0; i < hcl->sem_list_count; i++)
	{
		gc_ms_mark (hcl, (hcl_oop_t)hcl->sem_list[i]);
	}

	for (i = 0; i < hcl->sem_heap_count; i++)
	{
		gc_ms_mark (hcl, (hcl_oop_t)hcl->sem_heap[i]);
	}

	for (i = 0; i < hcl->sem_io_tuple_count; i++)
	{
		if (hcl->sem_io_tuple[i].sem[HCL_SEMAPHORE_IO_TYPE_INPUT])
			gc_ms_mark (hcl, (hcl_oop_t)hcl->sem_io_tuple[i].sem[HCL_SEMAPHORE_IO_TYPE_INPUT]);
		if (hcl->sem_io_tuple[i].sem[HCL_SEMAPHORE_IO_TYPE_OUTPUT])
			gc_ms_mark (hcl, (hcl_oop_t)hcl->sem_io_tuple[i].sem[HCL_SEMAPHORE_IO_TYPE_OUTPUT]);
	}

#if defined(ENABLE_GCFIN)
	gc_ms_mark (hcl, (hcl_oop_t)hcl->sem_gcfin);
#endif

	for (i = 0; i < hcl->proc_map_capa; i++)
	{
		gc_ms_mark (hcl, hcl->proc_map[i]);
	}

	for (i = 0; i < hcl->volat_count; i++)
	{
		gc_ms_mark (hcl, *hcl->volat_stack[i]);
	}

	if (hcl->initial_context) gc_ms_mark (hcl, (hcl_oop_t)hcl->initial_context);
	if (hcl->active_context) gc_ms_mark (hcl, (hcl_oop_t)hcl->active_context);
	if (hcl->initial_function) gc_ms_mark (hcl, (hcl_oop_t)hcl->initial_function);
	if (hcl->active_function) gc_ms_mark (hcl, (hcl_oop_t)hcl->active_function);

	if (hcl->last_retv) gc_ms_mark (hcl, hcl->last_retv);

	/*hcl_rbt_walk (&hcl->modtab, call_module_gc, hcl); */

	for (cb = hcl->cblist; cb; cb = cb->next)
	{
		if (cb->gc) cb->gc (hcl);
	}

#if defined(ENABLE_GCFIN)
	gcfin_count = move_finalizable_objects(hcl); /* mark finalizable objects */
#endif

	if (hcl->symtab)
	{
		compact_symbol_table (hcl, hcl->_nil); /* delete symbol table entries that are not marked */
	#if 0
		gc_ms_mark (hcl, (hcl_oop_t)hcl->symtab); /* mark the symbol table */
	#else
		HCL_OBJ_SET_FLAGS_MOVED(hcl->symtab, 1); /* mark */
		HCL_OBJ_SET_FLAGS_MOVED(hcl->symtab->bucket, 1); /* mark */
	#endif
	}

#if defined(ENABLE_GCFIN)
	if (gcfin_count > 0) hcl->sem_gcfin_sigreq = 1;
#endif

	if (hcl->active_function) hcl->active_code = HCL_FUNCTION_GET_CODE_BYTE(hcl->active_function); /* update hcl->active_code */

#if defined(HCL_PROFILE_VM)
	getrusage(RUSAGE_SELF, &ru);
	HCL_SUB_NTIME_SNS (&rut, &rut, ru.ru_utime.tv_sec, HCL_USEC_TO_NSEC(ru.ru_utime.tv_usec));
	HCL_SUB_NTIME (&hcl->gci.stat.mark, &hcl->gci.stat.mark, &rut); /* do subtraction because rut is negative */
#endif
}

void hcl_gc_ms_sweep_lazy (hcl_t* hcl, hcl_oow_t allocsize)
{
	hcl_gchdr_t* curr, * next, * prev;
	hcl_oop_t obj;
	hcl_oow_t freed_size;

#if defined(HCL_PROFILE_VM)
	struct rusage ru;
	hcl_ntime_t rut;
	getrusage(RUSAGE_SELF, &ru);
	HCL_INIT_NTIME (&rut,  ru.ru_utime.tv_sec, HCL_USEC_TO_NSEC(ru.ru_utime.tv_usec));
#endif

	if (!hcl->gci.ls.curr) goto done;

	freed_size = 0;

	prev = hcl->gci.ls.prev;
	curr = hcl->gci.ls.curr;

	while (curr)
	{
		next = curr->next;
		obj = (hcl_oop_t)(curr + 1);

		if (HCL_OBJ_GET_FLAGS_MOVED(obj)) /* if marked */
		{
			HCL_OBJ_SET_FLAGS_MOVED (obj, 0); /* unmark */
			prev = curr;
		}
		else
		{
			hcl_oow_t objsize;

			if (prev) prev->next = next;
			else hcl->gci.b = next;

			objsize = HCL_SIZEOF(hcl_obj_t) + hcl_getobjpayloadbytes(hcl, obj);
			freed_size += objsize;
			hcl->gci.bsz -= objsize;
			hcl_freeheapmem (hcl, hcl->heap, curr); /* destroy */

			/*if (freed_size > allocsize)*/  /* TODO: can it secure large enough space? */
			if (objsize == allocsize)
			{
				hcl->gci.ls.prev = prev;
				hcl->gci.ls.curr = next; /* let the next lazy sweeping begin at this point */
				goto done;
			}
		}

		curr = next;
	}

	hcl->gci.ls.curr = HCL_NULL;

done:
#if defined(HCL_PROFILE_VM)
	getrusage(RUSAGE_SELF, &ru);
	HCL_SUB_NTIME_SNS (&rut, &rut, ru.ru_utime.tv_sec, HCL_USEC_TO_NSEC(ru.ru_utime.tv_usec));
	HCL_SUB_NTIME (&hcl->gci.stat.sweep, &hcl->gci.stat.sweep, &rut); /* do subtraction because rut is negative */
#endif
	return;
}

static HCL_INLINE void gc_ms_sweep (hcl_t* hcl)
{
	hcl_gchdr_t* curr, * next, * prev;
	hcl_oop_t obj;

#if defined(HCL_PROFILE_VM)
	struct rusage ru;
	hcl_ntime_t rut;
	getrusage(RUSAGE_SELF, &ru);
	HCL_INIT_NTIME (&rut,  ru.ru_utime.tv_sec, HCL_USEC_TO_NSEC(ru.ru_utime.tv_usec));
#endif

	prev = HCL_NULL;
	curr = hcl->gci.b;
	while (curr)
	{
		next = curr->next;
		obj = (hcl_oop_t)(curr + 1);

		if (HCL_OBJ_GET_FLAGS_MOVED(obj)) /* if marked */
		{
			HCL_OBJ_SET_FLAGS_MOVED (obj, 0); 	/* unmark */
			prev = curr;
		}
		else
		{
			if (prev) prev->next = next;
			else hcl->gci.b = next;

			hcl->gci.bsz -= HCL_SIZEOF(hcl_obj_t) + hcl_getobjpayloadbytes(hcl, obj);
			hcl_freeheapmem (hcl, hcl->heap, curr); /* destroy */
		}

		curr = next;
	}

	hcl->gci.ls.curr = HCL_NULL;

#if defined(HCL_PROFILE_VM)
	getrusage(RUSAGE_SELF, &ru);
	HCL_SUB_NTIME_SNS (&rut, &rut, ru.ru_utime.tv_sec, HCL_USEC_TO_NSEC(ru.ru_utime.tv_usec));
	HCL_SUB_NTIME (&hcl->gci.stat.sweep, &hcl->gci.stat.sweep, &rut); /* do subtraction because rut is negative */
#endif
}

void hcl_gc (hcl_t* hcl, int full)
{
	if (hcl->gci.lazy_sweep) hcl_gc_ms_sweep_lazy (hcl, HCL_TYPE_MAX(hcl_oow_t));

	HCL_LOG1 (hcl, HCL_LOG_GC | HCL_LOG_INFO, "Starting GC (mark-sweep) - gci.bsz = %zu\n", hcl->gci.bsz);

	hcl->gci.stack.len = 0;
	/*hcl->gci.stack.max = 0;*/
	gc_ms_mark_roots (hcl);

	if (!full && hcl->gci.lazy_sweep)
	{
		/* set the lazy sweeping point to the head of the allocated blocks.
		 * hawk_allocbytes() updates hcl->gci.ls.prev if it is called while
		 * hcl->gci.ls.curr stays at hcl->gci.b */
		hcl->gci.ls.prev = HCL_NULL;
		hcl->gci.ls.curr = hcl->gci.b;
	}
	else
	{
	    gc_ms_sweep (hcl);
	}

	HCL_LOG2 (hcl, HCL_LOG_GC | HCL_LOG_INFO, "Finished GC (mark-sweep) - gci.bsz = %zu, gci.stack.max %zu\n", hcl->gci.bsz, hcl->gci.stack.max);
}

hcl_oop_t hcl_moveoop (hcl_t* hcl, hcl_oop_t oop)
{
	if (oop) gc_ms_mark (hcl, oop);
	return oop;
}

#if 0
void hcl_gc (hcl_t* hcl)
{
	/*
	 * move a referenced object to the new heap.
	 * inspect the fields of the moved object in the new heap.
	 * move objects pointed to by the fields to the new heap.
	 * finally perform some tricky symbol table clean-up.
	 */
	hcl_uint8_t* ptr;
	hcl_heap_t* tmp;
	hcl_oop_t old_nil;
	hcl_oow_t i;
	hcl_cb_t* cb;

	if (hcl->active_context)
	{
		HCL_ASSERT (hcl, (hcl_oop_t)hcl->processor != hcl->_nil);
		HCL_ASSERT (hcl, (hcl_oop_t)hcl->processor->active != hcl->_nil);
		HCL_ASSERT (hcl, HCL_IS_PROCESS(hcl, hcl->processor->active));
		/* commit the stack pointer to the active process */
		hcl->processor->active->sp = HCL_SMOOI_TO_OOP(hcl->sp);
		/* commit the instruction pointer to the active context */
		hcl->active_context->ip = HCL_SMOOI_TO_OOP(hcl->ip);
	}

	HCL_LOG4 (hcl, HCL_LOG_GC | HCL_LOG_INFO,
		"Starting GC curheap base %p ptr %p newheap base %p ptr %p\n",
		hcl->curheap->base, hcl->curheap->ptr, hcl->newheap->base, hcl->newheap->ptr);

	/* TODO: allocate common objects like _nil and the root dictionary
	 *       in the permanant heap.  minimize moving around */
	old_nil = hcl->_nil;

	/* move _nil and the root object table */
	hcl->_undef = hcl_moveoop(hcl, hcl->_undef);
	hcl->_nil = hcl_moveoop(hcl, hcl->_nil);
	hcl->_true = hcl_moveoop(hcl, hcl->_true);
	hcl->_false = hcl_moveoop(hcl, hcl->_false);

	for (i = 0; i < HCL_COUNTOF(syminfo); i++)
	{
		hcl_oop_t tmp;
		tmp = *(hcl_oop_t*)((hcl_uint8_t*)hcl + syminfo[i].offset);
		tmp = hcl_moveoop(hcl, tmp);
		*(hcl_oop_t*)((hcl_uint8_t*)hcl + syminfo[i].offset) = tmp;
	}

	for (i = 0; i < HCL_COUNTOF(kernel_classes); i++)
	{
		hcl_oop_t tmp;
		tmp = *(hcl_oop_t*)((hcl_uint8_t*)hcl + kernel_classes[i].offset);
		tmp = hcl_moveoop(hcl, tmp);
		*(hcl_oop_t*)((hcl_uint8_t*)hcl + kernel_classes[i].offset) = tmp;
	}

	hcl->sysdic = (hcl_oop_dic_t)hcl_moveoop(hcl, (hcl_oop_t)hcl->sysdic);
	hcl->processor = (hcl_oop_process_scheduler_t)hcl_moveoop(hcl, (hcl_oop_t)hcl->processor);
	hcl->nil_process = (hcl_oop_process_t)hcl_moveoop(hcl, (hcl_oop_t)hcl->nil_process);

	for (i = 0; i < hcl->code.lit.len; i++)
	{
		/* the literal array ia a NGC object. but the literal objects
		 * pointed by the elements of this array must be gabage-collected. */
		((hcl_oop_oop_t)hcl->code.lit.arr)->slot[i] =
			hcl_moveoop(hcl, ((hcl_oop_oop_t)hcl->code.lit.arr)->slot[i]);
	}

	hcl->p.e = hcl_moveoop(hcl, hcl->p.e);

	for (i = 0; i < hcl->sem_list_count; i++)
	{
		hcl->sem_list[i] = (hcl_oop_semaphore_t)hcl_moveoop(hcl, (hcl_oop_t)hcl->sem_list[i]);
	}

	for (i = 0; i < hcl->sem_heap_count; i++)
	{
		hcl->sem_heap[i] = (hcl_oop_semaphore_t)hcl_moveoop(hcl, (hcl_oop_t)hcl->sem_heap[i]);
	}

	for (i = 0; i < hcl->sem_io_tuple_count; i++)
	{
		if (hcl->sem_io_tuple[i].sem[HCL_SEMAPHORE_IO_TYPE_INPUT])
			hcl->sem_io_tuple[i].sem[HCL_SEMAPHORE_IO_TYPE_INPUT] = (hcl_oop_semaphore_t)hcl_moveoop(hcl, (hcl_oop_t)hcl->sem_io_tuple[i].sem[HCL_SEMAPHORE_IO_TYPE_INPUT]);
		if (hcl->sem_io_tuple[i].sem[HCL_SEMAPHORE_IO_TYPE_OUTPUT])
			hcl->sem_io_tuple[i].sem[HCL_SEMAPHORE_IO_TYPE_OUTPUT] = (hcl_oop_semaphore_t)hcl_moveoop(hcl, (hcl_oop_t)hcl->sem_io_tuple[i].sem[HCL_SEMAPHORE_IO_TYPE_OUTPUT]);
	}

#if defined(ENABLE_GCFIN)
	hcl->sem_gcfin = (hcl_oop_semaphore_t)hcl_moveoop(hcl, (hcl_oop_t)hcl->sem_gcfin);
#endif

	for (i = 0; i < hcl->proc_map_capa; i++)
	{
		hcl->proc_map[i] = hcl_moveoop(hcl, hcl->proc_map[i]);
	}

	for (i = 0; i < hcl->volat_count; i++)
	{
		*hcl->volat_stack[i] = hcl_moveoop(hcl, *hcl->volat_stack[i]);
	}

	if (hcl->initial_context)
		hcl->initial_context = (hcl_oop_context_t)hcl_moveoop(hcl, (hcl_oop_t)hcl->initial_context);
	if (hcl->active_context)
		hcl->active_context = (hcl_oop_context_t)hcl_moveoop(hcl, (hcl_oop_t)hcl->active_context);
	if (hcl->initial_function)
		hcl->initial_function = (hcl_oop_function_t)hcl_moveoop(hcl, (hcl_oop_t)hcl->initial_function);
	if (hcl->active_function)
		hcl->active_function = (hcl_oop_function_t)hcl_moveoop(hcl, (hcl_oop_t)hcl->active_function);

	if (hcl->last_retv) hcl->last_retv = hcl_moveoop(hcl, hcl->last_retv);

	for (cb = hcl->cblist; cb; cb = cb->next)
	{
		if (cb->gc) cb->gc (hcl);
	}

	/* scan the new heap to move referenced objects */
	ptr = (hcl_uint8_t*) HCL_ALIGN ((hcl_uintptr_t)hcl->newheap->base, HCL_SIZEOF(hcl_oop_t));
	ptr = scan_new_heap (hcl, ptr);

	/* traverse the symbol table for unreferenced symbols.
	 * if the symbol has not moved to the new heap, the symbol
	 * is not referenced by any other objects than the symbol
	 * table itself */
	compact_symbol_table (hcl, old_nil);

	/* move the symbol table itself */
	hcl->symtab = (hcl_oop_dic_t)hcl_moveoop(hcl, (hcl_oop_t)hcl->symtab);

	/* scan the new heap again from the end position of
	 * the previous scan to move referenced objects by
	 * the symbol table. */
	ptr = scan_new_heap (hcl, ptr);

	/* the contents of the current heap is not needed any more.
	 * reset the upper bound to the base. don't forget to align the heap
	 * pointer to the OOP size. See hcl_makeheap() also */
	hcl->curheap->ptr = (hcl_uint8_t*)HCL_ALIGN(((hcl_uintptr_t)hcl->curheap->base), HCL_SIZEOF(hcl_oop_t));

	/* swap the current heap and old heap */
	tmp = hcl->curheap;
	hcl->curheap = hcl->newheap;
	hcl->newheap = tmp;

/*
	if (hcl->symtab && HCL_LOG_ENABLED(hcl, HCL_LOG_GC | HCL_LOG_DEBUG))
	{
		hcl_oow_t index;
		hcl_oop_oop_t buc;
		HCL_LOG0 (hcl, HCL_LOG_GC | HCL_LOG_DEBUG, "--------- SURVIVING SYMBOLS IN GC ----------\n");
		buc = (hcl_oop_oop_t) hcl->symtab->bucket;
		for (index = 0; index < HCL_OBJ_GET_SIZE(buc); index++)
		{
			if ((hcl_oop_t)buc->slot[index] != hcl->_nil)
			{
				HCL_LOG1 (hcl, HCL_LOG_GC | HCL_LOG_DEBUG, "\t%O\n", buc->slot[index]);
			}
		}
		HCL_LOG0 (hcl, HCL_LOG_GC | HCL_LOG_DEBUG, "--------------------------------------------\n");
	}
*/

	if (hcl->active_function) hcl->active_code = HCL_FUNCTION_GET_CODE_BYTE(hcl->active_function);  /* update hcl->active_code */

/* TODO: include some gc statstics like number of live objects, gc performance, etc */
	HCL_LOG4 (hcl, HCL_LOG_GC | HCL_LOG_INFO,
		"Finished GC curheap base %p ptr %p newheap base %p ptr %p\n",
		hcl->curheap->base, hcl->curheap->ptr, hcl->newheap->base, hcl->newheap->ptr);
}
#endif

void hcl_pushvolat (hcl_t* hcl, hcl_oop_t* oop_ptr)
{
	/* if you have too many temporaries pushed, something must be wrong.
	 * change your code not to exceede the stack limit */
	HCL_ASSERT (hcl, hcl->volat_count < HCL_COUNTOF(hcl->volat_stack));
	hcl->volat_stack[hcl->volat_count++] = oop_ptr;
}

void hcl_popvolat (hcl_t* hcl)
{
	HCL_ASSERT (hcl, hcl->volat_count > 0);
	hcl->volat_count--;
}

void hcl_popvolats (hcl_t* hcl, hcl_oow_t count)
{
	HCL_ASSERT (hcl, hcl->volat_count >= count);
	hcl->volat_count -= count;
}


hcl_oop_t hcl_shallowcopy (hcl_t* hcl, hcl_oop_t oop)
{
	if (HCL_OOP_IS_POINTER(oop) && HCL_OBJ_GET_FLAGS_BRAND(oop) != HCL_BRAND_SYMBOL)
	{
		hcl_oop_t z;
		hcl_oow_t total_bytes;

		total_bytes = HCL_SIZEOF(hcl_obj_t) + hcl_getobjpayloadbytes(hcl, oop);

		hcl_pushvolat (hcl, &oop);
		z = (hcl_oop_t)hcl_allocbytes(hcl, total_bytes);
		hcl_popvolat(hcl);

		HCL_MEMCPY (z, oop, total_bytes);
		return z;
	}

	return oop;
}

/* ========================================================================= */

#if 0

static hcl_oow_t move_finalizable_objects (hcl_t* hcl);

/* -----------------------------------------------------------------------
 * BOOTSTRAPPER
 * ----------------------------------------------------------------------- */

static hcl_oop_class_t alloc_kernel_class (hcl_t* hcl, int class_flags, hcl_oow_t num_classvars, hcl_oow_t spec)
{
	hcl_oop_class_t c;
	hcl_ooi_t cspec;

	c = (hcl_oop_class_t)hcl_allocoopobj(hcl, HCL_CLASS_NAMED_INSTVARS + num_classvars);
	if (!c) return HCL_NULL;

	HCL_OBJ_SET_FLAGS_KERNEL (c, HCL_OBJ_FLAGS_KERNEL_IMMATURE);

	cspec = kernel_classes[KCI_CLASS].class_spec_flags;
	if (HCL_CLASS_SPEC_IS_IMMUTABLE(cspec)) HCL_OBJ_SET_FLAGS_RDONLY (c, 1); /* just for completeness of code. will never be true as it's not defined in the kernel class info table */
	if (HCL_CLASS_SPEC_IS_UNCOPYABLE(cspec)) HCL_OBJ_SET_FLAGS_UNCOPYABLE (c, 1); /* class itself is uncopyable */

	HCL_OBJ_SET_CLASS (c, (hcl_oop_t)hcl->_class);
	c->spec = HCL_SHCLI_TO_OOP(spec);
	c->selfspec = HCL_SHCLI_TO_OOP(HCL_CLASS_SELFSPEC_MAKE(num_classvars, 0, class_flags));

	return c;
}

static int ignite_1 (hcl_t* hcl)
{
	hcl_oow_t i;

	/*
	 * Create fundamental class objects with some fields mis-initialized yet.
	 * Such fields include 'superclass', 'subclasses', 'name', etc.
	 */
	HCL_ASSERT (hcl, hcl->_nil != HCL_NULL);
	HCL_ASSERT (hcl, HCL_OBJ_GET_CLASS(hcl->_nil) == HCL_NULL);

	HCL_ASSERT (hcl, hcl->_class == HCL_NULL);
	/* --------------------------------------------------------------
	 * Class
	 * The instance of Class can have indexed instance variables
	 * which are actually class variables.
	 * -------------------------------------------------------------- */
	hcl->_class = alloc_kernel_class(
		hcl, kernel_classes[KCI_CLASS].class_flags,
		kernel_classes[KCI_CLASS].class_num_classvars,
		HCL_CLASS_SPEC_MAKE(kernel_classes[KCI_CLASS].class_spec_named_instvars,
		                    kernel_classes[KCI_CLASS].class_spec_flags,
		                    kernel_classes[KCI_CLASS].class_spec_indexed_type));
	if (!hcl->_class) return -1;

	HCL_ASSERT (hcl, HCL_OBJ_GET_CLASS(hcl->_class) == HCL_NULL);
	HCL_OBJ_SET_CLASS (hcl->_class, (hcl_oop_t)hcl->_class);

	for (i = 0; i < HCL_COUNTOF(kernel_classes); i++)
	{
		hcl_oop_class_t tmp;

		if (i == KCI_CLASS) continue; /* skip Class as it's created above */

		tmp = alloc_kernel_class(
			hcl, kernel_classes[i].class_flags,
			kernel_classes[i].class_num_classvars,
			HCL_CLASS_SPEC_MAKE(kernel_classes[i].class_spec_named_instvars,
			                    kernel_classes[i].class_spec_flags,
			                    kernel_classes[i].class_spec_indexed_type));
		if (!tmp) return -1;
		*(hcl_oop_class_t*)((hcl_uint8_t*)hcl + kernel_classes[i].offset) = tmp;
	}

	HCL_OBJ_SET_CLASS (hcl->_nil, (hcl_oop_t)hcl->_undefined_object);

	/* an instance of a method class stores byte codes in the trailer space.
	 * unlike other classes with trailer size set, the size of the trailer
	 * space is not really determined by the traailer size set in the class.
	 * the compiler determines the actual size of the trailer space depending
	 * on the byte codes generated. i should set the following fields to avoid
	 * confusion at the GC phase. */
	hcl->_method->trsize = HCL_SHCLI_TO_OOP(0);
	hcl->_method->trgc = HCL_SMPTR_TO_OOP(0);

	return 0;
}

static int ignite_2 (hcl_t* hcl)
{
	hcl_oop_t tmp;
	int old_igniting = hcl->igniting;

	/* Create 'true' and 'false objects */
	hcl->_true = hcl_instantiate(hcl, hcl->_true_class, HCL_NULL, 0);
	hcl->_false = hcl_instantiate(hcl, hcl->_false_class, HCL_NULL, 0);
	if (HCL_UNLIKELY(!hcl->_true) || HCL_UNLIKELY(!hcl->_false)) return -1;

	/* Prevent the object instations in the permspace.
	 *
	 * 1. The symbol table is big and it may resize after ignition.
	 *    the resizing operation will migrate the obejct out of the
	 *    permspace. The space taken by the symbol table and the
	 *    system dictionary is wasted. I'd rather allocate these
	 *    in the normal space.
	 *
	 * 2. For compact_symbol_table() to work properly, hcl_gc() must not
	 *    scan the symbol table before it executes compact_symbol_table().
	 *    since hcl_gc() scans the entire perspace, it naturally gets to
	 *    hcl->symtab, which causes problems in compact_symbol_table().
	 *    I may reserve a special space for only the symbol table
	 *    to overcome this issue.
	 *
	 * For now, let's just allocate the symbol table and the system dictionary
	 * in the normal space */
	hcl->igniting = 0;

	/* Create the symbol table */
	tmp = hcl_instantiate(hcl, hcl->_symbol_table, HCL_NULL, 0);
	if (HCL_UNLIKELY(!tmp)) return -1;
	hcl->symtab = (hcl_oop_dic_t)tmp;

	hcl->symtab->tally = HCL_SHCLI_TO_OOP(0);
	/* It's important to assign the result of hcl_instantiate() to a temporary
	 * variable first and then assign it to hcl->symtab->bucket.
	 * The pointer 'hcl->symtab; can change in hcl_instantiate() and the
	 * target address of assignment may get set before hcl_instantiate()
	 * is called. */
	tmp = hcl_instantiate(hcl, hcl->_array, HCL_NULL, hcl->option.dfl_symtab_size);
	if (!tmp) return -1;
	hcl->symtab->bucket = (hcl_oop_oop_t)tmp;

	/* Create the system dictionary */
	tmp = (hcl_oop_t)hcl_makensdic(hcl, hcl->_namespace, hcl->option.dfl_sysdic_size);
	if (!tmp) return -1;
	hcl->sysdic = (hcl_oop_nsdic_t)tmp;

	hcl->igniting = old_igniting; /* back to the permspace */

	/* Create a nil process used to simplify nil check in GC.
	 * only accessible by VM. not exported via the global dictionary. */
	tmp = (hcl_oop_t)hcl_instantiate(hcl, hcl->_process, HCL_NULL, 0);
	if (!tmp) return -1;
	hcl->nil_process = (hcl_oop_process_t)tmp;
	hcl->nil_process->sp = HCL_SHCLI_TO_OOP(-1);
	hcl->nil_process->id = HCL_SHCLI_TO_OOP(-1);
	hcl->nil_process->perr = HCL_ERROR_TO_OOP(HCL_ENOERR);
	hcl->nil_process->perrmsg = hcl->_nil;

	/* Create a process scheduler */
	tmp = (hcl_oop_t)hcl_instantiate(hcl, hcl->_process_scheduler, HCL_NULL, 0);
	if (!tmp) return -1;
	hcl->processor = (hcl_oop_process_scheduler_t)tmp;
	hcl->processor->active = hcl->nil_process;
	hcl->processor->total_count = HCL_SHCLI_TO_OOP(0);
	hcl->processor->runnable.count = HCL_SHCLI_TO_OOP(0);
	hcl->processor->suspended.count = HCL_SHCLI_TO_OOP(0);

	return 0;
}

static int ignite_3 (hcl_t* hcl)
{
	/* Register kernel classes manually created so far to the system dictionary */
	static hcl_ooch_t str_processor[] = { 'P', 'r', 'o', 'c', 'e', 's', 's', 'o', 'r' };
	static hcl_ooch_t str_dicnew[] = { 'n', 'e', 'w', ':' };
	static hcl_ooch_t str_dicputassoc[] = { '_','_','p', 'u', 't', '_', 'a', 's', 's', 'o', 'c', ':' };
	static hcl_ooch_t str_does_not_understand[] = { 'd', 'o', 'e', 's', 'N', 'o', 't', 'U', 'n', 'd', 'e', 'r', 's', 't', 'a', 'n', 'd', ':' };
	static hcl_ooch_t str_primitive_failed[] = {  'p', 'r', 'i', 'm', 'i', 't', 'i', 'v', 'e', 'F', 'a', 'i', 'l', 'e', 'd' };
	static hcl_ooch_t str_unwindto_return[] = { 'u', 'n', 'w', 'i', 'n', 'd', 'T', 'o', ':', 'r', 'e', 't', 'u', 'r', 'n', ':' };

	hcl_oow_t i;
	hcl_oop_t sym;
	hcl_oop_class_t cls;

	for (i = 0; i < HCL_COUNTOF(kernel_classes); i++)
	{
		sym = hcl_makesymbol(hcl, kernel_classes[i].name, kernel_classes[i].len);
		if (!sym) return -1;

		cls = *(hcl_oop_class_t*)((hcl_uint8_t*)hcl + kernel_classes[i].offset);
		HCL_STORE_OOP (hcl, (hcl_oop_t*)&cls->name, sym);
		HCL_STORE_OOP (hcl, (hcl_oop_t*)&cls->nsup, (hcl_oop_t)hcl->sysdic);

		if (!hcl_putatsysdic(hcl, sym, (hcl_oop_t)cls)) return -1;
	}

	/* Attach the system dictionary to the nsdic field of the System class */
	HCL_STORE_OOP (hcl, (hcl_oop_t*)&hcl->_system->nsdic, (hcl_oop_t)hcl->sysdic);
	/* Set the name field of the system dictionary */
	HCL_STORE_OOP (hcl, (hcl_oop_t*)&hcl->sysdic->name, (hcl_oop_t)hcl->_system->name);
	/* Set the owning class field of the system dictionary, it's circular here */
	HCL_STORE_OOP (hcl, (hcl_oop_t*)&hcl->sysdic->nsup, (hcl_oop_t)hcl->_system);

	/* Make the process scheduler avaialble as the global name 'Processor' */
	sym = hcl_makesymbol(hcl, str_processor, HCL_COUNTOF(str_processor));
	if (!sym) return -1;
	if (!hcl_putatsysdic(hcl, sym, (hcl_oop_t)hcl->processor)) return -1;

	sym = hcl_makesymbol(hcl, str_dicnew, HCL_COUNTOF(str_dicnew));
	if (!sym) return -1;
	hcl->dicnewsym = (hcl_oop_char_t)sym;

	sym = hcl_makesymbol(hcl, str_dicputassoc, HCL_COUNTOF(str_dicputassoc));
	if (!sym) return -1;
	hcl->dicputassocsym = (hcl_oop_char_t)sym;

	sym = hcl_makesymbol(hcl, str_does_not_understand, HCL_COUNTOF(str_does_not_understand));
	if (!sym) return -1;
	hcl->does_not_understand_sym = (hcl_oop_char_t)sym;

	sym = hcl_makesymbol(hcl, str_primitive_failed, HCL_COUNTOF(str_primitive_failed));
	if (!sym) return -1;
	hcl->primitive_failed_sym = (hcl_oop_char_t)sym;

	sym = hcl_makesymbol(hcl, str_unwindto_return, HCL_COUNTOF(str_unwindto_return));
	if (!sym) return -1;
	hcl->unwindto_return_sym = (hcl_oop_char_t)sym;

	return 0;
}

struct hcl_class_t
{
        HCL_OBJ_HEADER;

        hcl_oop_t mdic; /* method dictionary. nil or a dictionary object */

        hcl_oop_t superclass;
        hcl_oop_t nivars; /* smooi. */
        hcl_oop_t ncvars; /* smooi. */
        hcl_oop_t nivars_super; /* smooi */

        hcl_oop_char_t ivarnames;
        hcl_oop_char_t cvarnames;

        /* indexed part afterwards - not included in HCL_CLASS_NAMED_INSTVARS */
        hcl_oop_t      cvar[1];   /* class variables. */
};
#endif

static int make_kernel_classes (hcl_t* hcl)
{
	hcl_oop_class_t c;
	hcl_oow_t i;

	/* make_kernel_classes() creates a chain of classes for initial bootstrapping.
	 * when the objects are loaded from an image file, this function is skipped */

#if 0
	/* create class objects */
	for (i = 0; i < HCL_COUNTOF(kctab); i++)
	{
		if (kctab[i].c_offset >= HCL_SIZEOF(*hcl)) continue;

		c = (hcl_oop_class_t)hcl_makeclass(hcl, hcl->_nil, nivars, ncvars, "ivars_str", "cvars_str");
		if (HCL_UNLIKELY(!c)) return -1;

		*(hcl_oop_class_t*)((hcl_uint8_t*)hcl + kctab[i].c_offset) = c;
	}

	/* update the superclass field */
	for (i = 0; i < HCL_COUNTOF(kctab); i++)
	{
		if (kctab[i].sc_offset >= HCL_SIZEOF(*hcl)) continue;

		c = *(hcl_oop_class_t*)((hcl_uint8_t*)hcl + kctab[i].c_offset);
		c->superclass = *(hcl_oop_t*)((hcl_uint8_t*)hcl + kctab[i].sc_offset);
	}
#endif

	return 0;
}

int hcl_ignite (hcl_t* hcl, hcl_oow_t heapsize)
{
	hcl_oow_t i;

	if (!hcl->heap)
	{
		hcl->heap = hcl_makeheap(hcl, heapsize);
		if (HCL_UNLIKELY(!hcl->heap)) return -1;
	}

	if (!hcl->_undef)
	{
		hcl->_undef = hcl_makeundef(hcl);
		if (HCL_UNLIKELY(!hcl->_undef)) return -1;
	}

	if (!hcl->_nil)
	{
		hcl->_nil = hcl_makenil(hcl);
		if (HCL_UNLIKELY(!hcl->_nil)) return -1;
	}

	if (!hcl->_true)
	{
		hcl->_true = hcl_maketrue(hcl);
		if (HCL_UNLIKELY(!hcl->_true)) return -1;
	}
	if (!hcl->_false)
	{
		hcl->_false = hcl_makefalse(hcl);
		if (HCL_UNLIKELY(!hcl->_false)) return -1;
	}


	if (!hcl->symtab)
	{
		hcl->symtab = (hcl_oop_dic_t)hcl_makedic(hcl, hcl->option.dfl_symtab_size);
		if (HCL_UNLIKELY(!hcl->symtab)) return -1;
	}

	if (!hcl->sysdic)
	{
		hcl->sysdic = (hcl_oop_dic_t)hcl_makedic(hcl, hcl->option.dfl_sysdic_size);
		if (HCL_UNLIKELY(!hcl->sysdic)) return -1;
	}

	/* symbol table available now. symbols can be created */
	for (i = 0; i < HCL_COUNTOF(syminfo); i++)
	{
		hcl_oop_t tmp;

		tmp = hcl_makesymbol(hcl, syminfo[i].ptr, syminfo[i].len);
		if (HCL_UNLIKELY(!tmp)) return -1;

		HCL_OBJ_SET_FLAGS_SYNCODE (tmp, syminfo[i].syncode);
		*(hcl_oop_t*)((hcl_uint8_t*)hcl + syminfo[i].offset) = tmp;
	}

	if (!hcl->nil_process)
	{
		/* Create a nil process used to simplify nil check in GC.
		 * only accessible by VM. not exported via the global dictionary. */
		hcl->nil_process = (hcl_oop_process_t)hcl_allocoopobj(hcl, HCL_BRAND_PROCESS, HCL_PROCESS_NAMED_INSTVARS);
		if (HCL_UNLIKELY(!hcl->nil_process)) return -1;

		/* unusable stack */
		hcl->nil_process->sp = HCL_SMOOI_TO_OOP(-1);
		hcl->nil_process->st = HCL_SMOOI_TO_OOP(-1);
		/* unusable exception stack */
		hcl->nil_process->exsp = HCL_SMOOI_TO_OOP(-1);
		hcl->nil_process->exst = HCL_SMOOI_TO_OOP(-1);
		/* unusable class stack */
		hcl->nil_process->clsp = HCL_SMOOI_TO_OOP(-1);
		hcl->nil_process->clst =  HCL_SMOOI_TO_OOP(-1);
	}

	if (!hcl->processor)
	{
		hcl->processor = (hcl_oop_process_scheduler_t)hcl_allocoopobj(hcl, HCL_BRAND_PROCESS_SCHEDULER, HCL_PROCESS_SCHEDULER_NAMED_INSTVARS);
		if (HCL_UNLIKELY(!hcl->processor)) return -1;
		hcl->processor->active = hcl->nil_process;
		hcl->processor->total_count = HCL_SMOOI_TO_OOP(0);
		hcl->processor->runnable.count = HCL_SMOOI_TO_OOP(0);
		hcl->processor->suspended.count = HCL_SMOOI_TO_OOP(0);

		/* commit the sp field of the initial active context to hcl->sp */
		hcl->sp = HCL_OOP_TO_SMOOI(hcl->processor->active->sp);
	}

	if (make_kernel_classes(hcl) <= -1) return -1;

	/* TODO: move this initialization to hcl_init? */
	if (hcl_brewcode(hcl, &hcl->code) <= -1) return -1;

	hcl->p.e = hcl->_nil;
	return 0;
}

int hcl_getsyncodebyoocs_noseterr (hcl_t* hcl, const hcl_oocs_t* name)
{
	hcl_oow_t i;
	for (i = 0; i < HCL_COUNTOF(syminfo); i++)
	{
		if (hcl_comp_oochars(syminfo[i].ptr, syminfo[i].len, name->ptr, name->len) == 0)
			return syminfo[i].syncode;
	}
	return 0; /* 0 indicates no syntax code found */
}

int hcl_getsyncode_noseterr (hcl_t* hcl, const hcl_ooch_t* ptr, const hcl_oow_t len)
{
	hcl_oow_t i;
	for (i = 0; i < HCL_COUNTOF(syminfo); i++)
	{
		if (hcl_comp_oochars(syminfo[i].ptr, syminfo[i].len, ptr, len) == 0)
			return syminfo[i].syncode;
	}
	return 0; /* 0 indicates no syntax code found */
}

const hcl_ooch_t* hcl_getsyncodename_noseterr (hcl_t* hcl, hcl_syncode_t syncode)
{
	hcl_oow_t i;
	for (i = 0; i < HCL_COUNTOF(syminfo); i++)
	{
		if (syncode == syminfo[i].syncode) return syminfo[i].ptr;
	}
	return HCL_NULL;
}
