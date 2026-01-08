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

#if defined(HAK_PROFILE_VM)
#include <sys/time.h>
#include <sys/resource.h> /* getrusage */
#endif

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
 *                  String
 *                    Symbol
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
	const hak_bch_t* name;
	int superclass_kci;
	int class_brand;
	int class_flags; /* class flags for selfspec */
	int class_ncvars; /* number of class variables */

	int class_spec_nivars; /* number of named instance variables */
	int class_spec_flags;
	int class_spec_indexed_type;

	hak_oow_t  offset; /* offset to the field in hak_t that stored the class pointer */
};
typedef struct kernel_class_info_t kernel_class_info_t;

enum {
	KCI_APEX = 0,
	KCI_CLASS,
	KCI_UNDEFINED_OBJECT,
	KCI_NIL_OBJECT,
	KCI_OBJECT,
	KCI_COLLECTION,
	KCI_INDEXED_COLLECTION,
	KCI_FIXED_SIZED_COLLECTION,
	KCI_STRING,
	KCI_BYTE_STRING,
	KCI_SYMBOL,
	KCI_ARRAY,
	KCI_CHARACTER_ARRAY,
	KCI_BYTE_ARRAY,
	KCI_SYMBOL_TABLE,
	KCI_DICTIONARY,
	KCI_CONS,
	KCI_METHOD_DICTIONARY,
	KCI_FUNCTION,
	KCI_PRIMITIVE,
	KCI_COMPILED_BLOCK,
	KCI_BLOCK_CONTEXT,
	KCI_PROCESS,
	KCI_SEMAPHORE,
	KCI_SEMAPHORE_GROUP,
	KCI_PROCESS_SCHEDULER,
	KCI_ERROR,
	KCI_TRUE,
	KCI_FALSE,
	KCI_MAGNITUDE,
	KCI_CHARACTER,
	KCI_NUMBER,
	KCI_SMALL_INTEGER,
	KCI_LARGE_POSITIVE_INTEGER,
	KCI_LARGE_NEGATIVE_INTEGER,
	KCI_FIXED_POINT_DECIMAL,
	KCI_SMALL_POINTER,
	KCI_LARGE_POINTER,
	KCI_SYSTEM,

	__KCI_MAX__
};
#define KCI(x) HAK_AID(x)

static kernel_class_info_t kernel_classes[__KCI_MAX__] =
{
	/* --------------------------------------------------------------
	 * Apex              - proto-object with 1 class variable.
	 *   Class
	 *   UndefinedObject - class for the undefined state
     *   NilObject       - class for nil
	 *   Object          - top of all ordinary objects.
	 *     String
	 *       Symbol
	 *     Array
	 *     ByteArray
	 *     Character
	 *     SmallIntger
	 * -------------------------------------------------------------- */
	KCI(KCI_APEX) {
		"Apex",
		-1, /* no superclass */
		0, /* brand */
		0, /* selfspec flags */
		0, /* ncvars */
		0, /* nivars */
		0, /* spec flags */
		HAK_OBJ_TYPE_OOP, /* indexed type */
		HAK_OFFSETOF(hak_t, c_apex)
	},

	KCI(KCI_CLASS) {
		"Class",
		KCI_APEX,
		HAK_BRAND_CLASS,
		HAK_CLASS_SELFSPEC_FLAG_FINAL | HAK_CLASS_SELFSPEC_FLAG_LIMITED,
		0, /* ncvars */
		HAK_CLASS_NAMED_INSTVARS, /* nivars */
		HAK_CLASS_SPEC_FLAG_INDEXED | HAK_CLASS_SPEC_FLAG_UNCOPYABLE,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_class)
	},

	KCI(KCI_UNDEFINED_OBJECT) {
		"UndefinedObject",
		KCI_APEX,
		HAK_BRAND_UNDEF,
		0,
		0,
		0,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_undefobj)
	},

	KCI(KCI_NIL_OBJECT) {
		"NilObject",
		KCI_APEX,
		HAK_BRAND_NIL,
		0,
		0,
		0,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_nilobj)
	},

#if 0
	{ "Interface",
	  HAK_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  HAK_INTERFACE_NAMED_INSTVARS,
	  HAK_CLASS_SPEC_FLAG_INDEXED | HAK_CLASS_SPEC_FLAG_UNCOPYABLE,
	  HAK_OBJ_TYPE_OOP,
	  HAK_OFFSETOF(hak_t, _interface) },
#endif

	KCI(KCI_OBJECT) {
		"Object",
		KCI_APEX,
		0, /* brand */
		0, /* selfspec flags */
		0, /* ncvars */
		0, /* nivars */
		0, /* spec flags */
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_object)
	},

	KCI(KCI_COLLECTION) {
		"Collection",
		KCI_OBJECT,
		0, /* brand */
		0,
		0,
		0,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_collection)
	},

	KCI(KCI_INDEXED_COLLECTION) {
		"IndexedCollection",
		KCI_COLLECTION,
		0, /* brand */
		0,
		0,
		0,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_indexed_collection)
	},

	KCI(KCI_FIXED_SIZED_COLLECTION) {
		"FixedSizedCollection",
		KCI_INDEXED_COLLECTION,
		0, /* brand */
		0,
		0,
		0,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_fixed_sized_collection)
	},

	KCI(KCI_STRING) {
		"String",
		KCI_FIXED_SIZED_COLLECTION,
		HAK_BRAND_STRING,
		0,
		0,
		0,
		HAK_CLASS_SPEC_FLAG_INDEXED,
		HAK_OBJ_TYPE_CHAR,
		HAK_OFFSETOF(hak_t, c_string)
	},

	KCI(KCI_BYTE_STRING) {
		"ByteString",
		KCI_FIXED_SIZED_COLLECTION,
		HAK_BRAND_BYTE_STRING,
		0,
		0,
		0,
		HAK_CLASS_SPEC_FLAG_INDEXED,
		HAK_OBJ_TYPE_BYTE,
		HAK_OFFSETOF(hak_t, c_byte_string)
	},

	KCI(KCI_SYMBOL) {
		"Symbol",
		KCI_STRING,
		HAK_BRAND_SYMBOL,
		HAK_CLASS_SELFSPEC_FLAG_FINAL | HAK_CLASS_SELFSPEC_FLAG_LIMITED, /* TODO: these flags not implemented yet */
		0,
		0,
		HAK_CLASS_SPEC_FLAG_INDEXED | HAK_CLASS_SPEC_FLAG_IMMUTABLE,
		HAK_OBJ_TYPE_CHAR,
		HAK_OFFSETOF(hak_t, c_symbol)
	},

	KCI(KCI_ARRAY) {
		"Array",
		KCI_FIXED_SIZED_COLLECTION,
		HAK_BRAND_ARRAY,
		0,
		0,
		0,
		HAK_CLASS_SPEC_FLAG_INDEXED,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_array)
	},

	KCI(KCI_CHARACTER_ARRAY) {
		"CharacterArray",
		KCI_FIXED_SIZED_COLLECTION,
		HAK_BRAND_CHARACTER_ARRAY,
		0,
		0,
		0,
		HAK_CLASS_SPEC_FLAG_INDEXED,
		HAK_OBJ_TYPE_CHAR,
		HAK_OFFSETOF(hak_t, c_character_array)
	},

	KCI(KCI_BYTE_ARRAY) {
		"ByteArray",
		KCI_FIXED_SIZED_COLLECTION,
		HAK_BRAND_BYTE_ARRAY,
		0,
		0,
		0,
		HAK_CLASS_SPEC_FLAG_INDEXED,
		HAK_OBJ_TYPE_BYTE,
		HAK_OFFSETOF(hak_t, c_byte_array)
	},

	/* A special incarnation of a dictionary that allows only a symbol as a value.
	 * The value in bucket is a symbol while the value in a normal dictionary is a
	 * pair(cons) that contains a key and a value. */
	KCI(KCI_SYMBOL_TABLE) {
		"SymbolTable",
		KCI_COLLECTION,
		HAK_BRAND_DIC, /* TODO: make this a special child class of Dictionary?? */
		0,
		0,
		HAK_DIC_NAMED_INSTVARS,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_symtab)
	},

	KCI(KCI_DICTIONARY) {
		"Dictionary",
		KCI_COLLECTION,
		HAK_BRAND_DIC,
		0,
		0,
		HAK_DIC_NAMED_INSTVARS,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_dictionary)
	},

	KCI(KCI_CONS) {
		"Cons",
		KCI_OBJECT,
		HAK_BRAND_CONS,
		0,
		0,
		HAK_CONS_NAMED_INSTVARS,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_cons)
	},

#if 0
	{ "Namespace",
	  HAK_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  HAK_NSDIC_NAMED_INSTVARS,
	  0,
	  HAK_OBJ_TYPE_OOP,
	  HAK_OFFSETOF(hak_t, c_namespace) },

	{ "PoolDictionary",
	  0,
	  0,
	  HAK_DIC_NAMED_INSTVARS,
	  0,
	  HAK_OBJ_TYPE_OOP,
	  HAK_OFFSETOF(hak_t, c_pool_dictionary) },
#endif

	KCI(KCI_METHOD_DICTIONARY) {
		"MethodDictionary",
		KCI_DICTIONARY,
		0,
		0,
		0,
		HAK_DIC_NAMED_INSTVARS,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_method_dictionary)
	},

#if 0
	{ "CompiledMethod",
	  0,
	  0,
	  HAK_METHOD_NAMED_INSTVARS,
	  HAK_CLASS_SPEC_FLAG_INDEXED,
	  HAK_OBJ_TYPE_OOP,
	  HAK_OFFSETOF(hak_t, c_method) },

	{ "MethodSignature",
	  0,
	  0,
	  HAK_METHSIG_NAMED_INSTVARS,
	  HAK_CLASS_SPEC_FLAG_INDEXED,
	  HAK_OBJ_TYPE_OOP,
	  HAK_OFFSETOF(hak_t, c_methsig) },
#endif

	/* special function created with MAKE_FUNCTION in interactive mode
	 * for execution of code fed and compiled.  */
	KCI(KCI_FUNCTION) {
		"Function",
		KCI_OBJECT,
		HAK_BRAND_FUNCTION,
		0,
		0,
		HAK_FUNCTION_NAMED_INSTVARS,
		HAK_CLASS_SPEC_FLAG_INDEXED,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_function)
	},

	KCI(KCI_PRIMITIVE) {
		"Primitive",
		KCI_OBJECT,
		HAK_BRAND_PRIM,
		0,
		0,
		HAK_PRIM_NAMED_INSTVARS,
		0,
		HAK_OBJ_TYPE_WORD,
		HAK_OFFSETOF(hak_t, c_primitive)
	},

	KCI(KCI_COMPILED_BLOCK) {
		"CompiledBlock",
		KCI_OBJECT,
		HAK_BRAND_BLOCK,
		0,
		0,
		HAK_BLOCK_NAMED_INSTVARS,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_compiled_block)
	},

	KCI(KCI_BLOCK_CONTEXT) {
		"BlockContext",
		KCI_OBJECT,
		HAK_BRAND_CONTEXT,
		HAK_CLASS_SELFSPEC_FLAG_FINAL | HAK_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		HAK_CONTEXT_NAMED_INSTVARS,
		HAK_CLASS_SPEC_FLAG_INDEXED,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_block_context)
	},

	KCI(KCI_PROCESS) {
		"Process",
		KCI_OBJECT,
		HAK_BRAND_PROCESS,
		HAK_CLASS_SELFSPEC_FLAG_FINAL | HAK_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		HAK_PROCESS_NAMED_INSTVARS,
		HAK_CLASS_SPEC_FLAG_INDEXED | HAK_CLASS_SPEC_FLAG_UNCOPYABLE,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_process)
	},

	KCI(KCI_SEMAPHORE) {
		"Semaphore",
		KCI_OBJECT,
		HAK_BRAND_SEMAPHORE,
		0,
		0,
		HAK_SEMAPHORE_NAMED_INSTVARS,
		HAK_CLASS_SPEC_FLAG_UNCOPYABLE,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_semaphore)
	},

	KCI(KCI_SEMAPHORE_GROUP) {
		"SemaphoreGroup",
		KCI_OBJECT,
		HAK_BRAND_SEMAPHORE_GROUP,
		0,
		0,
		HAK_SEMAPHORE_GROUP_NAMED_INSTVARS,
		HAK_CLASS_SPEC_FLAG_UNCOPYABLE,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_semaphore_group)
	},

	KCI(KCI_PROCESS_SCHEDULER) {
		"ProcessScheduler",
		KCI_OBJECT,
		HAK_BRAND_PROCESS_SCHEDULER,
		HAK_CLASS_SELFSPEC_FLAG_FINAL | HAK_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		HAK_PROCESS_SCHEDULER_NAMED_INSTVARS,
		HAK_CLASS_SPEC_FLAG_UNCOPYABLE,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_process_scheduler)
	},

	KCI(KCI_ERROR) {
		"Error",
		KCI_OBJECT,
		0,
		HAK_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		0,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_error)
	},

	KCI(KCI_TRUE) {
		"True",
		KCI_OBJECT,
		HAK_BRAND_TRUE,
		HAK_CLASS_SELFSPEC_FLAG_LIMITED | HAK_CLASS_SELFSPEC_FLAG_FINAL,
		0,
		0,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_true)
	},

	KCI(KCI_FALSE) {
		"False",
		KCI_OBJECT,
		HAK_BRAND_FALSE,
		HAK_CLASS_SELFSPEC_FLAG_LIMITED | HAK_CLASS_SELFSPEC_FLAG_FINAL,
		0,
		0,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_false)
	},

	KCI(KCI_MAGNITUDE) {
		"Magnitude",
		KCI_OBJECT,
		0, /* brand */
		0,
		0,
		0,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_magnitude)
	},

	/* TOOD: what is a proper spec for Character and SmallInteger?
	 *       If the fixed part is  0, its instance must be an object of 0 payload fields.
	 *       Does this make sense? */
	KCI(KCI_CHARACTER) {
		"Character",
		KCI_MAGNITUDE,
		HAK_BRAND_CHARACTER,
		HAK_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		0,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_character)
	},

	KCI(KCI_NUMBER) {
		"Number",
		KCI_MAGNITUDE,
		0, /* brand */
		HAK_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		0,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_number)
	},

	KCI(KCI_SMALL_INTEGER) {
		"SmallInteger",
		KCI_NUMBER,
		HAK_BRAND_SMOOI,
		HAK_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		0,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_small_integer)
	},

	KCI(KCI_LARGE_POSITIVE_INTEGER) {
		"LargePositiveInteger",
		KCI_NUMBER,
		HAK_BRAND_PBIGINT,
		HAK_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		0,
		HAK_CLASS_SPEC_FLAG_INDEXED | HAK_CLASS_SPEC_FLAG_IMMUTABLE,
		HAK_OBJ_TYPE_LIWORD,
		HAK_OFFSETOF(hak_t, c_large_positive_integer)
	},

	KCI(KCI_LARGE_NEGATIVE_INTEGER) {
		"LargeNegativeInteger",
		KCI_NUMBER,
		HAK_BRAND_NBIGINT,
		HAK_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		0,
		HAK_CLASS_SPEC_FLAG_INDEXED | HAK_CLASS_SPEC_FLAG_IMMUTABLE,
		HAK_OBJ_TYPE_LIWORD,
		HAK_OFFSETOF(hak_t, c_large_negative_integer)
	},

	KCI(KCI_FIXED_POINT_DECIMAL) {
		"FixedPointDecimal",
		KCI_NUMBER,
		HAK_BRAND_FPDEC,
		HAK_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		HAK_FPDEC_NAMED_INSTVARS,
		HAK_CLASS_SPEC_FLAG_IMMUTABLE,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_fixed_point_decimal)
	},

	KCI(KCI_SMALL_POINTER) {
		"SmallPointer",
		KCI_MAGNITUDE,
		HAK_BRAND_SMPTR,
		HAK_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		0,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_small_pointer)
	},

	KCI(KCI_LARGE_POINTER) {
		"LargePointer",
		KCI_MAGNITUDE,
		0,
		HAK_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		1,  /* #word(1) */
		HAK_CLASS_SPEC_FLAG_IMMUTABLE | HAK_CLASS_SPEC_FLAG_INDEXED,
		HAK_OBJ_TYPE_WORD,
		HAK_OFFSETOF(hak_t, c_large_pointer)
	},

	KCI(KCI_SYSTEM) {
		"System",
		KCI_APEX,
		0,
		0,
		5, /* asyncsg, gcfin_sem, gcfin_should_exit, ossig_pid, shr */
		0,
		0,
		HAK_OBJ_TYPE_OOP,
		HAK_OFFSETOF(hak_t, c_system)
	}
};

/* ========================================================================= */

static void compact_symbol_table (hak_t* hak, hak_oop_t _nil)
{
	hak_oop_char_t symbol;
	hak_oow_t i, x, y, z;
	hak_oow_t bucket_size, index;
	hak_ooi_t tally;

#if defined(HAK_SUPPORT_GC_DURING_IGNITION)
	if (!hak->symtab) return; /* symbol table has not been created */
#endif

	/* the symbol table doesn't allow more data items than HAK_SMOOI_MAX.
	 * so hak->symtab->tally must always be a small integer */
	HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(hak->symtab->tally));
	tally = HAK_OOP_TO_SMOOI(hak->symtab->tally);
	HAK_ASSERT(hak, tally >= 0); /* it must not be less than 0 */
	if (tally <= 0) return;

	/* NOTE: in theory, the bucket size can be greater than HAK_SMOOI_MAX
	 * as it is an internal header field and is of an unsigned type */
	bucket_size = HAK_OBJ_GET_SIZE(hak->symtab->bucket);

	for (index = 0; index < bucket_size; )
	{
		if (HAK_OBJ_GET_FLAGS_MOVED(hak->symtab->bucket->slot[index]))
		{
			index++;
			continue;
		}

		HAK_ASSERT(hak, hak->symtab->bucket->slot[index] != _nil);

		for (i = 0, x = index, y = index; i < bucket_size; i++)
		{
			y = (y + 1) % bucket_size;

			/* done if the slot at the current hash index is _nil */
			if (hak->symtab->bucket->slot[y] == _nil) break;

			/* get the natural hash index for the data in the slot
			 * at the current hash index */
			symbol = (hak_oop_char_t)hak->symtab->bucket->slot[y];

			HAK_ASSERT(hak, HAK_IS_SYMBOL(hak, symbol));

			z = hak_hash_oochars(symbol->slot, HAK_OBJ_GET_SIZE(symbol)) % bucket_size;

			/* move an element if necessary */
			if ((y > x && (z <= x || z > y)) ||
			    (y < x && (z <= x && z > y)))
			{
				hak->symtab->bucket->slot[x] = hak->symtab->bucket->slot[y];
				x = y;
			}
		}

		hak->symtab->bucket->slot[x] = _nil;
		tally--;
	}

	HAK_ASSERT(hak, tally >= 0);
	HAK_ASSERT(hak, tally <= HAK_SMOOI_MAX);
	hak->symtab->tally = HAK_SMOOI_TO_OOP(tally);
}

hak_oow_t hak_getobjpayloadbytes (hak_t* hak, hak_oop_t oop)
{
	hak_oow_t nbytes_aligned;

	if (HAK_OBJ_GET_FLAGS_TRAILER(oop))
	{
		hak_oow_t nbytes;

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
		HAK_ASSERT(hak, HAK_OBJ_GET_FLAGS_TYPE(oop) == HAK_OBJ_TYPE_OOP);
		HAK_ASSERT(hak, HAK_OBJ_GET_FLAGS_UNIT(oop) == HAK_SIZEOF(hak_oow_t));
		HAK_ASSERT(hak, HAK_OBJ_GET_FLAGS_EXTRA(oop) == 0); /* no 'extra' for an OOP object */

		nbytes = HAK_OBJ_BYTESOF(oop) + HAK_SIZEOF(hak_oow_t) + HAK_OBJ_GET_TRAILER_SIZE(oop);
		nbytes_aligned = HAK_ALIGN(nbytes, HAK_SIZEOF(hak_oop_t));
	}
	else
	{
		/* calculate the payload size in bytes */
		nbytes_aligned = HAK_ALIGN(HAK_OBJ_BYTESOF(oop), HAK_SIZEOF(hak_oop_t));
	}

	return nbytes_aligned;
}


/* ----------------------------------------------------------------------- */

#if 0
static HAK_INLINE void gc_ms_mark (hak_t* hak, hak_oop_t oop)
{
	hak_oow_t i, sz;

#if defined(HAK_SUPPORT_GC_DURING_IGNITION)
	if (!oop) return;
#endif

	if (!HAK_OOP_IS_POINTER(oop)) return;
	if (HAK_OBJ_GET_FLAGS_MOVED(oop)) return; /* already marked */

	HAK_OBJ_SET_FLAGS_MOVED(oop, 1); /* mark */

	/*gc_ms_mark(hak, (hak_oop_t)HAK_OBJ_GET_CLASS(oop));*/ /* TODO: remove recursion */

	if (HAK_OBJ_GET_FLAGS_TYPE(oop) == HAK_OBJ_TYPE_OOP)
	{
		hak_oow_t size, i;

		/* is it really better to use a flag bit in the header to
		 * determine that it is an instance of process? */
		if (HAK_UNLIKELY(HAK_OBJ_GET_FLAGS_PROC(oop)))
		{
			/* the stack in a process object doesn't need to be
			 * scanned in full. the slots above the stack pointer
			 * are garbages. */
			size = HAK_PROCESS_NAMED_INSTVARS + HAK_OOP_TO_SMOOI(((hak_oop_process_t)oop)->sp) + 1;
			HAK_ASSERT(hak, size <= HAK_OBJ_GET_SIZE(oop));
		}
		else
		{
			size = HAK_OBJ_GET_SIZE(oop);
		}

		for (i = 0; i < size; i++)
		{
			hak_oop_t tmp = HAK_OBJ_GET_OOP_VAL(oop, i);
			if (HAK_OOP_IS_POINTER(tmp)) gc_ms_mark(hak, tmp);  /* TODO: no resursion */
		}
	}
}
#else
static HAK_INLINE void gc_ms_mark_object (hak_t* hak, hak_oop_t oop)
{
#if defined(HAK_SUPPORT_GC_DURING_IGNITION)
	if (!oop) return;
#endif
	if (!HAK_OOP_IS_POINTER(oop) || HAK_OBJ_GET_FLAGS_MOVED(oop)) return; /* non-pointer or already marked */

	HAK_OBJ_SET_FLAGS_MOVED(oop, 1); /* mark */
	HAK_ASSERT(hak, hak->gci.stack.len < hak->gci.stack.capa);
	hak->gci.stack.ptr[hak->gci.stack.len++] = oop; /* push */
	if (hak->gci.stack.len > hak->gci.stack.max) hak->gci.stack.max = hak->gci.stack.len;
}

static HAK_INLINE void gc_ms_scan_stack (hak_t* hak)
{
	hak_oop_t oop;

	while (hak->gci.stack.len > 0)
	{
		oop = hak->gci.stack.ptr[--hak->gci.stack.len];

		gc_ms_mark_object(hak, (hak_oop_t)HAK_OBJ_GET_CLASS(oop));

		if (HAK_OBJ_GET_FLAGS_TYPE(oop) == HAK_OBJ_TYPE_OOP)
		{
			hak_ooi_t i, ll;

			/* is it really better to use a flag bit in the header to
			 * determine that it is an instance of process? */
			if (HAK_UNLIKELY(HAK_OBJ_GET_FLAGS_PROC(oop)))
			{
				hak_oop_process_t proc;

				HAK_ASSERT(hak, HAK_IS_PROCESS(hak, oop));
				/* the stack in a process object doesn't need to be
				 * scanned in full. the slots above the stack pointer
				 * are garbages. */
				proc = (hak_oop_process_t)oop;

				/* the fixed part */
				ll = HAK_PROCESS_NAMED_INSTVARS;
				for (i = 0; i < ll; i++) gc_ms_mark_object(hak, HAK_OBJ_GET_OOP_VAL(oop, i));

				/* stack */
				ll = HAK_OOP_TO_SMOOI(proc->sp);
				HAK_ASSERT(hak, ll < (hak_ooi_t)(HAK_OBJ_GET_SIZE(oop) - HAK_PROCESS_NAMED_INSTVARS));
				for (i = 0; i <= ll; i++) gc_ms_mark_object(hak, proc->slot[i]);

				/* exception stack */
				ll = HAK_OOP_TO_SMOOI(proc->exsp);
				HAK_ASSERT(hak, ll < (hak_ooi_t)(HAK_OBJ_GET_SIZE(oop) - HAK_PROCESS_NAMED_INSTVARS));
				for (i = HAK_OOP_TO_SMOOI(proc->st) + 1; i <= ll; i++) gc_ms_mark_object(hak, proc->slot[i]);

				/* class stack */
				ll = HAK_OOP_TO_SMOOI(proc->clsp);
				HAK_ASSERT(hak, ll < (hak_ooi_t)(HAK_OBJ_GET_SIZE(oop) - HAK_PROCESS_NAMED_INSTVARS));
				for (i = HAK_OOP_TO_SMOOI(proc->exst) + 1; i <= ll; i++) gc_ms_mark_object(hak, proc->slot[i]);

				/* frame stack */
				ll = HAK_OOP_TO_SMOOI(proc->fsp);
				HAK_ASSERT(hak, ll < (hak_ooi_t)(HAK_OBJ_GET_SIZE(oop) - HAK_PROCESS_NAMED_INSTVARS));
				for (i = HAK_OOP_TO_SMOOI(proc->clst) + 1; i <= ll; i++) gc_ms_mark_object(hak, proc->slot[i]);
			}
			else
			{
				ll = HAK_OBJ_GET_SIZE(oop);
				for (i = 0; i < ll; i++) gc_ms_mark_object(hak, HAK_OBJ_GET_OOP_VAL(oop, i));
			}
		}
	}
}

static HAK_INLINE void gc_ms_mark (hak_t* hak, hak_oop_t oop)
{
	gc_ms_mark_object(hak, oop);
	gc_ms_scan_stack(hak);
}
#endif

static HAK_INLINE void gc_ms_mark_roots (hak_t* hak)
{
	hak_oow_t i;
#if defined(ENABLE_GCFIN)
	hak_oow_t gcfin_count;
#endif
	hak_cb_t* cb;

#if defined(HAK_PROFILE_VM)
	struct rusage ru;
	hak_ntime_t rut;
	getrusage(RUSAGE_SELF, &ru);
	HAK_INIT_NTIME (&rut,  ru.ru_utime.tv_sec, HAK_USEC_TO_NSEC(ru.ru_utime.tv_usec));
#endif

	if (hak->processor && hak->processor->active)
	{
		HAK_ASSERT(hak, (hak_oop_t)hak->processor != hak->_nil);
		HAK_ASSERT(hak, (hak_oop_t)hak->processor->active != hak->_nil);

		/* commit the stack pointer to the active process because
		 * gc needs the correct stack pointer for a process object */
		hak->processor->active->sp = HAK_SMOOI_TO_OOP(hak->sp);
	}

	gc_ms_mark(hak, hak->_undef);
	gc_ms_mark(hak, hak->_nil);
	gc_ms_mark(hak, hak->_true);
	gc_ms_mark(hak, hak->_false);

	for (i = 0; i < HAK_COUNTOF(kernel_classes); i++)
	{
		gc_ms_mark(hak, *(hak_oop_t*)((hak_uint8_t*)hak + kernel_classes[i].offset));
	}

	gc_ms_mark(hak, (hak_oop_t)hak->sysdic);
	gc_ms_mark(hak, (hak_oop_t)hak->processor);
	gc_ms_mark(hak, (hak_oop_t)hak->nil_process);


	for (i = 0; i < hak->code.lit.len; i++)
	{
		/* the literal array ia a NGC object. but the literal objects
		 * pointed by the elements of this array must be gabage-collected. */
		gc_ms_mark(hak, ((hak_oop_oop_t)hak->code.lit.arr)->slot[i]);
	}
	gc_ms_mark(hak, hak->p.e);

	for (i = 0; i < hak->sem_list_count; i++)
	{
		gc_ms_mark(hak, (hak_oop_t)hak->sem_list[i]);
	}

	for (i = 0; i < hak->sem_heap_count; i++)
	{
		gc_ms_mark(hak, (hak_oop_t)hak->sem_heap[i]);
	}

	for (i = 0; i < hak->sem_io_tuple_count; i++)
	{
		if (hak->sem_io_tuple[i].sem[HAK_SEMAPHORE_IO_TYPE_INPUT])
			gc_ms_mark(hak, (hak_oop_t)hak->sem_io_tuple[i].sem[HAK_SEMAPHORE_IO_TYPE_INPUT]);
		if (hak->sem_io_tuple[i].sem[HAK_SEMAPHORE_IO_TYPE_OUTPUT])
			gc_ms_mark(hak, (hak_oop_t)hak->sem_io_tuple[i].sem[HAK_SEMAPHORE_IO_TYPE_OUTPUT]);
	}

#if defined(ENABLE_GCFIN)
	gc_ms_mark(hak, (hak_oop_t)hak->sem_gcfin);
#endif

	for (i = 0; i < hak->proc_map_capa; i++)
	{
		gc_ms_mark(hak, hak->proc_map[i]);
	}

	for (i = 0; i < hak->volat_count; i++)
	{
		gc_ms_mark(hak, *hak->volat_stack[i]);
	}

	if (hak->initial_context) gc_ms_mark(hak, (hak_oop_t)hak->initial_context);
	if (hak->active_context) gc_ms_mark(hak, (hak_oop_t)hak->active_context);
	if (hak->initial_function) gc_ms_mark(hak, (hak_oop_t)hak->initial_function);
	if (hak->active_function) gc_ms_mark(hak, (hak_oop_t)hak->active_function);

	if (hak->last_retv) gc_ms_mark(hak, hak->last_retv);

	/*hak_rbt_walk (&hak->modtab, call_module_gc, hak); */

	for (cb = hak->cblist; cb; cb = cb->next)
	{
		if (cb->on_gc) cb->on_gc(hak);
	}

#if defined(ENABLE_GCFIN)
	gcfin_count = move_finalizable_objects(hak); /* mark finalizable objects */
#endif

	if (hak->symtab)
	{
		compact_symbol_table(hak, hak->_nil); /* delete symbol table entries that are not marked */
	#if 0
		gc_ms_mark(hak, (hak_oop_t)hak->symtab); /* mark the symbol table */
	#else
		HAK_OBJ_SET_FLAGS_MOVED(hak->symtab, 1); /* mark */
		HAK_OBJ_SET_FLAGS_MOVED(hak->symtab->bucket, 1); /* mark */
	#endif
	}

#if defined(ENABLE_GCFIN)
	if (gcfin_count > 0) hak->sem_gcfin_sigreq = 1;
#endif

	if (hak->active_function) hak->active_code = HAK_FUNCTION_GET_CODE_BYTE(hak->active_function); /* update hak->active_code */

#if defined(HAK_PROFILE_VM)
	getrusage(RUSAGE_SELF, &ru);
	HAK_SUB_NTIME_SNS (&rut, &rut, ru.ru_utime.tv_sec, HAK_USEC_TO_NSEC(ru.ru_utime.tv_usec));
	HAK_SUB_NTIME (&hak->gci.stat.mark, &hak->gci.stat.mark, &rut); /* do subtraction because rut is negative */
#endif
}

void hak_gc_ms_sweep_lazy (hak_t* hak, hak_oow_t allocsize)
{
	hak_gchdr_t* curr, * next, * prev;
	hak_oop_t obj;
	hak_oow_t freed_size;

#if defined(HAK_PROFILE_VM)
	struct rusage ru;
	hak_ntime_t rut;
	getrusage(RUSAGE_SELF, &ru);
	HAK_INIT_NTIME (&rut,  ru.ru_utime.tv_sec, HAK_USEC_TO_NSEC(ru.ru_utime.tv_usec));
#endif

	if (!hak->gci.ls.curr) goto done;

	freed_size = 0;

	prev = hak->gci.ls.prev;
	curr = hak->gci.ls.curr;

	while (curr)
	{
		next = curr->next;
		obj = (hak_oop_t)(curr + 1);

		if (HAK_OBJ_GET_FLAGS_MOVED(obj)) /* if marked */
		{
			HAK_OBJ_SET_FLAGS_MOVED(obj, 0); /* unmark */
			prev = curr;
		}
		else
		{
			hak_oow_t objsize;

			if (prev) prev->next = next;
			else hak->gci.b = next;

			objsize = HAK_SIZEOF(hak_obj_t) + hak_getobjpayloadbytes(hak, obj);
			freed_size += objsize;
			hak->gci.bsz -= objsize;
			hak_freeheapmem(hak, hak->heap, curr); /* destroy */

			/*if (freed_size > allocsize)*/  /* TODO: can it secure large enough space? */
			if (objsize == allocsize)
			{
				hak->gci.ls.prev = prev;
				hak->gci.ls.curr = next; /* let the next lazy sweeping begin at this point */
				goto done;
			}
		}

		curr = next;
	}

	hak->gci.ls.curr = HAK_NULL;

done:
#if defined(HAK_PROFILE_VM)
	getrusage(RUSAGE_SELF, &ru);
	HAK_SUB_NTIME_SNS(&rut, &rut, ru.ru_utime.tv_sec, HAK_USEC_TO_NSEC(ru.ru_utime.tv_usec));
	HAK_SUB_NTIME(&hak->gci.stat.sweep, &hak->gci.stat.sweep, &rut); /* do subtraction because rut is negative */
#endif
	return;
}

static HAK_INLINE void gc_ms_sweep (hak_t* hak)
{
	hak_gchdr_t* curr, * next, * prev;
	hak_oop_t obj;

#if defined(HAK_PROFILE_VM)
	struct rusage ru;
	hak_ntime_t rut;
	getrusage(RUSAGE_SELF, &ru);
	HAK_INIT_NTIME(&rut,  ru.ru_utime.tv_sec, HAK_USEC_TO_NSEC(ru.ru_utime.tv_usec));
#endif

	prev = HAK_NULL;
	curr = hak->gci.b;
	while (curr)
	{
		next = curr->next;
		obj = (hak_oop_t)(curr + 1);

		if (HAK_OBJ_GET_FLAGS_MOVED(obj)) /* if marked */
		{
			HAK_OBJ_SET_FLAGS_MOVED(obj, 0); 	/* unmark */
			prev = curr;
		}
		else
		{
			if (prev) prev->next = next;
			else hak->gci.b = next;

			hak->gci.bsz -= HAK_SIZEOF(hak_obj_t) + hak_getobjpayloadbytes(hak, obj);
			hak_freeheapmem(hak, hak->heap, curr); /* destroy */
		}

		curr = next;
	}

	hak->gci.ls.curr = HAK_NULL;

#if defined(HAK_PROFILE_VM)
	getrusage(RUSAGE_SELF, &ru);
	HAK_SUB_NTIME_SNS(&rut, &rut, ru.ru_utime.tv_sec, HAK_USEC_TO_NSEC(ru.ru_utime.tv_usec));
	HAK_SUB_NTIME(&hak->gci.stat.sweep, &hak->gci.stat.sweep, &rut); /* do subtraction because rut is negative */
#endif
}

void hak_gc (hak_t* hak, int full)
{
	if (hak->gci.lazy_sweep) hak_gc_ms_sweep_lazy(hak, HAK_TYPE_MAX(hak_oow_t));

	HAK_LOG1 (hak, HAK_LOG_GC | HAK_LOG_INFO, "Starting GC (mark-sweep) - gci.bsz = %zu\n", hak->gci.bsz);

	hak->gci.stack.len = 0;
	/*hak->gci.stack.max = 0;*/
	gc_ms_mark_roots(hak);

	if (!full && hak->gci.lazy_sweep)
	{
		/* set the lazy sweeping pointer to the head of the allocated blocks.
		 * hak_allocbytes() updates hak->gci.ls.prev if it is called while
		 * hak->gci.ls.curr stays at hak->gci.b */
		hak->gci.ls.prev = HAK_NULL;
		hak->gci.ls.curr = hak->gci.b;
	}
	else
	{
	    gc_ms_sweep(hak);
	}

	HAK_LOG2 (hak, HAK_LOG_GC | HAK_LOG_INFO, "Finished GC (mark-sweep) - gci.bsz = %zu, gci.stack.max %zu\n", hak->gci.bsz, hak->gci.stack.max);
}

hak_oop_t hak_moveoop (hak_t* hak, hak_oop_t oop)
{
	if (oop) gc_ms_mark(hak, oop);
	return oop;
}

#if 0
void hak_gc (hak_t* hak)
{
	/*
	 * move a referenced object to the new heap.
	 * inspect the fields of the moved object in the new heap.
	 * move objects pointed to by the fields to the new heap.
	 * finally perform some tricky symbol table clean-up.
	 */
	hak_uint8_t* ptr;
	hak_heap_t* tmp;
	hak_oop_t old_nil;
	hak_oow_t i;
	hak_cb_t* cb;

	if (hak->active_context)
	{
		HAK_ASSERT(hak, (hak_oop_t)hak->processor != hak->_nil);
		HAK_ASSERT(hak, (hak_oop_t)hak->processor->active != hak->_nil);
		HAK_ASSERT(hak, HAK_IS_PROCESS(hak, hak->processor->active));
		/* commit the stack pointer to the active process */
		hak->processor->active->sp = HAK_SMOOI_TO_OOP(hak->sp);
		/* commit the instruction pointer to the active context */
		hak->active_context->ip = HAK_SMOOI_TO_OOP(hak->ip);
	}

	HAK_LOG4 (hak, HAK_LOG_GC | HAK_LOG_INFO,
		"Starting GC curheap base %p ptr %p newheap base %p ptr %p\n",
		hak->curheap->base, hak->curheap->ptr, hak->newheap->base, hak->newheap->ptr);

	/* TODO: allocate common objects like _nil and the root dictionary
	 *       in the permanant heap.  minimize moving around */
	old_nil = hak->_nil;

	/* move _nil and the root object table */
	hak->_undef = hak_moveoop(hak, hak->_undef);
	hak->_nil = hak_moveoop(hak, hak->_nil);
	hak->_true = hak_moveoop(hak, hak->_true);
	hak->_false = hak_moveoop(hak, hak->_false);

	for (i = 0; i < HAK_COUNTOF(kernel_classes); i++)
	{
		hak_oop_t tmp;
		tmp = *(hak_oop_t*)((hak_uint8_t*)hak + kernel_classes[i].offset);
		tmp = hak_moveoop(hak, tmp);
		*(hak_oop_t*)((hak_uint8_t*)hak + kernel_classes[i].offset) = tmp;
	}

	hak->sysdic = (hak_oop_dic_t)hak_moveoop(hak, (hak_oop_t)hak->sysdic);
	hak->processor = (hak_oop_process_scheduler_t)hak_moveoop(hak, (hak_oop_t)hak->processor);
	hak->nil_process = (hak_oop_process_t)hak_moveoop(hak, (hak_oop_t)hak->nil_process);

	for (i = 0; i < hak->code.lit.len; i++)
	{
		/* the literal array ia a NGC object. but the literal objects
		 * pointed by the elements of this array must be gabage-collected. */
		((hak_oop_oop_t)hak->code.lit.arr)->slot[i] =
			hak_moveoop(hak, ((hak_oop_oop_t)hak->code.lit.arr)->slot[i]);
	}

	hak->p.e = hak_moveoop(hak, hak->p.e);

	for (i = 0; i < hak->sem_list_count; i++)
	{
		hak->sem_list[i] = (hak_oop_semaphore_t)hak_moveoop(hak, (hak_oop_t)hak->sem_list[i]);
	}

	for (i = 0; i < hak->sem_heap_count; i++)
	{
		hak->sem_heap[i] = (hak_oop_semaphore_t)hak_moveoop(hak, (hak_oop_t)hak->sem_heap[i]);
	}

	for (i = 0; i < hak->sem_io_tuple_count; i++)
	{
		if (hak->sem_io_tuple[i].sem[HAK_SEMAPHORE_IO_TYPE_INPUT])
			hak->sem_io_tuple[i].sem[HAK_SEMAPHORE_IO_TYPE_INPUT] = (hak_oop_semaphore_t)hak_moveoop(hak, (hak_oop_t)hak->sem_io_tuple[i].sem[HAK_SEMAPHORE_IO_TYPE_INPUT]);
		if (hak->sem_io_tuple[i].sem[HAK_SEMAPHORE_IO_TYPE_OUTPUT])
			hak->sem_io_tuple[i].sem[HAK_SEMAPHORE_IO_TYPE_OUTPUT] = (hak_oop_semaphore_t)hak_moveoop(hak, (hak_oop_t)hak->sem_io_tuple[i].sem[HAK_SEMAPHORE_IO_TYPE_OUTPUT]);
	}

#if defined(ENABLE_GCFIN)
	hak->sem_gcfin = (hak_oop_semaphore_t)hak_moveoop(hak, (hak_oop_t)hak->sem_gcfin);
#endif

	for (i = 0; i < hak->proc_map_capa; i++)
	{
		hak->proc_map[i] = hak_moveoop(hak, hak->proc_map[i]);
	}

	for (i = 0; i < hak->volat_count; i++)
	{
		*hak->volat_stack[i] = hak_moveoop(hak, *hak->volat_stack[i]);
	}

	if (hak->initial_context)
		hak->initial_context = (hak_oop_context_t)hak_moveoop(hak, (hak_oop_t)hak->initial_context);
	if (hak->active_context)
		hak->active_context = (hak_oop_context_t)hak_moveoop(hak, (hak_oop_t)hak->active_context);
	if (hak->initial_function)
		hak->initial_function = (hak_oop_function_t)hak_moveoop(hak, (hak_oop_t)hak->initial_function);
	if (hak->active_function)
		hak->active_function = (hak_oop_function_t)hak_moveoop(hak, (hak_oop_t)hak->active_function);

	if (hak->last_retv) hak->last_retv = hak_moveoop(hak, hak->last_retv);

	for (cb = hak->cblist; cb; cb = cb->next)
	{
		if (cb->gc) cb->gc(hak);
	}

	/* scan the new heap to move referenced objects */
	ptr = (hak_uint8_t*)HAK_ALIGN((hak_uintptr_t)hak->newheap->base, HAK_SIZEOF(hak_oop_t));
	ptr = scan_new_heap(hak, ptr);

	/* traverse the symbol table for unreferenced symbols.
	 * if the symbol has not moved to the new heap, the symbol
	 * is not referenced by any other objects than the symbol
	 * table itself */
	compact_symbol_table(hak, old_nil);

	/* move the symbol table itself */
	hak->symtab = (hak_oop_dic_t)hak_moveoop(hak, (hak_oop_t)hak->symtab);

	/* scan the new heap again from the end position of
	 * the previous scan to move referenced objects by
	 * the symbol table. */
	ptr = scan_new_heap(hak, ptr);

	/* the contents of the current heap is not needed any more.
	 * reset the upper bound to the base. don't forget to align the heap
	 * pointer to the OOP size. See hak_makeheap() also */
	hak->curheap->ptr = (hak_uint8_t*)HAK_ALIGN(((hak_uintptr_t)hak->curheap->base), HAK_SIZEOF(hak_oop_t));

	/* swap the current heap and old heap */
	tmp = hak->curheap;
	hak->curheap = hak->newheap;
	hak->newheap = tmp;

/*
	if (hak->symtab && HAK_LOG_ENABLED(hak, HAK_LOG_GC | HAK_LOG_DEBUG))
	{
		hak_oow_t index;
		hak_oop_oop_t buc;
		HAK_LOG0 (hak, HAK_LOG_GC | HAK_LOG_DEBUG, "--------- SURVIVING SYMBOLS IN GC ----------\n");
		buc = (hak_oop_oop_t) hak->symtab->bucket;
		for (index = 0; index < HAK_OBJ_GET_SIZE(buc); index++)
		{
			if ((hak_oop_t)buc->slot[index] != hak->_nil)
			{
				HAK_LOG1 (hak, HAK_LOG_GC | HAK_LOG_DEBUG, "\t%O\n", buc->slot[index]);
			}
		}
		HAK_LOG0 (hak, HAK_LOG_GC | HAK_LOG_DEBUG, "--------------------------------------------\n");
	}
*/

	if (hak->active_function) hak->active_code = HAK_FUNCTION_GET_CODE_BYTE(hak->active_function);  /* update hak->active_code */

/* TODO: include some gc statstics like number of live objects, gc performance, etc */
	HAK_LOG4 (hak, HAK_LOG_GC | HAK_LOG_INFO,
		"Finished GC curheap base %p ptr %p newheap base %p ptr %p\n",
		hak->curheap->base, hak->curheap->ptr, hak->newheap->base, hak->newheap->ptr);
}
#endif

void hak_pushvolat (hak_t* hak, hak_oop_t* oop_ptr)
{
	/* if you have too many temporaries pushed, something must be wrong.
	 * change your code not to exceede the stack limit */
	HAK_ASSERT(hak, hak->volat_count < HAK_COUNTOF(hak->volat_stack));
	hak->volat_stack[hak->volat_count++] = oop_ptr;
}

void hak_popvolat (hak_t* hak)
{
	HAK_ASSERT(hak, hak->volat_count > 0);
	hak->volat_count--;
}

void hak_popvolats (hak_t* hak, hak_oow_t count)
{
	HAK_ASSERT(hak, hak->volat_count >= count);
	hak->volat_count -= count;
}


hak_oop_t hak_shallowcopy (hak_t* hak, hak_oop_t oop)
{
	if (HAK_OOP_IS_POINTER(oop) && HAK_OBJ_GET_CLASS(oop) != (hak_oop_t)hak->c_symbol)
	{
		hak_oop_t z;
		hak_oow_t total_bytes;

		total_bytes = HAK_SIZEOF(hak_obj_t) + hak_getobjpayloadbytes(hak, oop);

		hak_pushvolat(hak, &oop);
		z = (hak_oop_t)hak_allocbytes(hak, total_bytes);
		hak_popvolat(hak);

		HAK_MEMCPY(z, oop, total_bytes);
		return z;
	}

	return oop;
}

/* ========================================================================= */

/* -----------------------------------------------------------------------
 * BOOTSTRAPPER
 * ----------------------------------------------------------------------- */

static hak_oop_class_t alloc_kernel_class (hak_t* hak, int class_flags, hak_oow_t num_classvars, hak_oow_t spec, hak_ooi_t nivars_super, int ibrand)
{
	hak_oop_class_t c;
	hak_ooi_t cspec;

	c = (hak_oop_class_t)hak_allocoopobj(hak, HAK_CLASS_NAMED_INSTVARS + num_classvars);
	if (HAK_UNLIKELY(!c)) return HAK_NULL;

	HAK_OBJ_SET_FLAGS_KERNEL(c, HAK_OBJ_FLAGS_KERNEL_IMMATURE);

	cspec = kernel_classes[KCI_CLASS].class_spec_flags;
	if (HAK_CLASS_SPEC_IS_IMMUTABLE(cspec)) HAK_OBJ_SET_FLAGS_RDONLY(c, 1); /* just for completeness of code. will never be true as it's not defined in the kernel class info table */
#if 0 /* TODO extend the flags and uncomment this part */
	if (HAK_CLASS_SPEC_IS_UNCOPYABLE(cspec)) HAK_OBJ_SET_FLAGS_UNCOPYABLE(c, 1); /* class itself is uncopyable */
#endif

	HAK_OBJ_SET_CLASS(c, (hak_oop_t)hak->c_class);
	c->spec = HAK_SMOOI_TO_OOP(spec);
	c->selfspec = HAK_SMOOI_TO_OOP(HAK_CLASS_SELFSPEC_MAKE(num_classvars, 0, class_flags));
	c->nivars_super = HAK_SMOOI_TO_OOP(nivars_super); /* TODO: encode it into spec? */
	c->ibrand = HAK_SMOOI_TO_OOP(ibrand);

	return c;
}

static int ignite_1 (hak_t* hak)
{
	hak_oow_t i;

	/*
	 * Create fundamental class objects with some fields mis-initialized yet.
	 * Such fields include 'superclass', 'subclasses', 'name', etc.
	 */
	HAK_ASSERT(hak, hak->_nil != HAK_NULL);
	HAK_ASSERT(hak, HAK_OBJ_GET_CLASS(hak->_nil) == HAK_NULL);

	HAK_ASSERT(hak, hak->c_class == HAK_NULL);
	/* --------------------------------------------------------------
	 * Class
	 * The instance of Class can have indexed instance variables
	 * which are actually class variables.
	 * -------------------------------------------------------------- */
	if (HAK_LIKELY(!hak->c_class))
	{
		HAK_ASSERT(hak, kernel_classes[KCI_CLASS].superclass_kci >= 0);
		hak->c_class = alloc_kernel_class(
			hak,
			kernel_classes[KCI_CLASS].class_flags,
			kernel_classes[KCI_CLASS].class_ncvars,
			HAK_CLASS_SPEC_MAKE(kernel_classes[KCI_CLASS].class_spec_nivars,
			                    kernel_classes[KCI_CLASS].class_spec_flags,
			                    kernel_classes[KCI_CLASS].class_spec_indexed_type),
			kernel_classes[kernel_classes[KCI_CLASS].superclass_kci].class_spec_nivars,
			kernel_classes[KCI_CLASS].class_brand);
		if (HAK_UNLIKELY(!hak->c_class))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to allocate %hs - %js", kernel_classes[KCI_CLASS].name, orgmsg);
			return -1;
		}

		HAK_ASSERT(hak, HAK_OBJ_GET_CLASS(hak->c_class) == HAK_NULL);
		HAK_OBJ_SET_CLASS (hak->c_class, (hak_oop_t)hak->c_class);
	}

	/* create class objects except Class */
	for (i = 0; i < HAK_COUNTOF(kernel_classes); i++)
	{
		hak_oop_class_t tmp;
		hak_ooi_t nivars_super;
		int superclass_kci;

		if (i == KCI_CLASS) continue; /* skip Class as it's created above */

		superclass_kci = kernel_classes[i].superclass_kci;
		nivars_super = superclass_kci <= -1? 0: kernel_classes[superclass_kci].class_spec_nivars;
		tmp = alloc_kernel_class(
			hak,
			kernel_classes[i].class_flags,
			kernel_classes[i].class_ncvars,
			HAK_CLASS_SPEC_MAKE(kernel_classes[i].class_spec_nivars,
			                    kernel_classes[i].class_spec_flags,
			                    kernel_classes[i].class_spec_indexed_type),
			nivars_super,
			kernel_classes[i].class_brand);
		if (HAK_UNLIKELY(!tmp))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to allocate %hs - %js", kernel_classes[i].name, orgmsg);
			return -1;
		}
		*(hak_oop_class_t*)((hak_uint8_t*)hak + kernel_classes[i].offset) = tmp;
	}

	/* update the superclass field */
	for (i = 0; i < HAK_COUNTOF(kernel_classes); i++)
	{
		int skci;
		skci = kernel_classes[i].superclass_kci;
		if (skci >= 0)
		{
			hak_oop_class_t* x, * y;
			x = (hak_oop_class_t*)((hak_uint8_t*)hak + kernel_classes[i].offset);
			y = (hak_oop_class_t*)((hak_uint8_t*)hak + kernel_classes[skci].offset);
			(*x)->superclass = (hak_oop_t)*y;
		}
	}

#if 0
	/* an instance of a method class stores byte codes in the trailer space.
	 * unlike other classes with trailer size set, the size of the trailer
	 * space is not really determined by the trailer size set in the class.
	 * the compiler determines the actual size of the trailer space depending
	 * on the byte codes generated. i should set the following fields to avoid
	 * confusion at the GC phase. */
	hak->c_method->trsize = HAK_SMOOI_TO_OOP(0);
	hak->c_method->trgc = HAK_SMPTR_TO_OOP(0);
#endif

	return 0;
}

static int ignite_2 (hak_t* hak)
{
	hak_oop_t tmp;
#if 0
	int old_igniting = hak->igniting;
#endif

	/* Create 'true' and 'false objects */
	if (HAK_LIKELY(!hak->_true))
	{
		hak->_true = hak_instantiate(hak, hak->c_true, HAK_NULL, 0);
		if (HAK_UNLIKELY(!hak->_true)) goto oops;
	}

	if (HAK_LIKELY(!hak->_false))
	{
		hak->_false = hak_instantiate(hak, hak->c_false, HAK_NULL, 0);
		if (HAK_UNLIKELY(!hak->_false)) goto oops;
	}

#if 0
	/* Prevent the object instantions in the permspace.
	 *
	 * 1. The symbol table is big and it may resize after ignition.
	 *    the resizing operation will migrate the obejct out of the
	 *    permspace. The space taken by the symbol table and the
	 *    system dictionary is wasted. I'd rather allocate these
	 *    in the normal space.
	 *
	 * 2. For compact_symbol_table() to work properly, hak_gc() must not
	 *    scan the symbol table before it executes compact_symbol_table().
	 *    since hak_gc() scans the entire perspace, it naturally gets to
	 *    hak->symtab, which causes problems in compact_symbol_table().
	 *    I may reserve a special space for only the symbol table
	 *    to overcome this issue.
	 *
	 * For now, let's just allocate the symbol table and the system dictionary
	 * in the normal space */
	hak->igniting = 0;
#endif

	if (HAK_LIKELY(!hak->symtab))
	{
		/* Create the symbol table - values in the bucket are limited to symbols only */
		tmp = hak_instantiate(hak, hak->c_symtab, HAK_NULL, 0);
		if (HAK_UNLIKELY(!tmp)) goto oops;
		hak->symtab = (hak_oop_dic_t)tmp;
		hak->symtab->tally = HAK_SMOOI_TO_OOP(0);
	}

	if (HAK_LIKELY((hak_oop_t)hak->symtab->bucket == hak->_nil))
	{
		/* It's important to assign the result of hak_instantiate() to a temporary
		* variable first and then assign it to hak->symtab->bucket.
		* The pointer 'hak->symtab; can change in hak_instantiate() and the
		* target address of assignment may get set before hak_instantiate()
		* is called. */
		HAK_ASSERT(hak, hak->option.dfl_symtab_size > 0);
		tmp = hak_instantiate(hak, hak->c_array, HAK_NULL, hak->option.dfl_symtab_size);
		if (HAK_UNLIKELY(!tmp)) goto oops; /* TODO: delete hak->symtab instad of this separate initialization of the bucket??? */
		hak->symtab->bucket = (hak_oop_oop_t)tmp;
	}

#if 0
	/* Create the system dictionary */
	tmp = (hak_oop_t)hak_makensdic(hak, hak->_namespace, hak->option.dfl_sysdic_size);
	if (!tmp) return -1;
	hak->sysdic = (hak_oop_nsdic_t)tmp;
#else
	if (HAK_LIKELY(!hak->sysdic))
	{
		tmp = hak_instantiate(hak, hak->c_dictionary, HAK_NULL, 0);
		if (HAK_UNLIKELY(!tmp)) goto oops;
		hak->sysdic = (hak_oop_dic_t)tmp;
		hak->sysdic->tally = HAK_SMOOI_TO_OOP(0);
	}

	if (HAK_LIKELY((hak_oop_t)hak->sysdic->bucket == hak->_nil))
	{
		/* It's important to assign the result of hak_instantiate() to a temporary
		* variable first and then assign it to hak->symtab->bucket.
		* The pointer 'hak->symtab; can change in hak_instantiate() and the
		* target address of assignment may get set before hak_instantiate()
		* is called. */
		tmp = hak_instantiate(hak, hak->c_array, HAK_NULL, hak->option.dfl_sysdic_size);
		if (HAK_UNLIKELY(!tmp)) goto oops;
		hak->sysdic->bucket = (hak_oop_oop_t)tmp;
	}
#endif

#if 0
	hak->igniting = old_igniting; /* back to the permspace */
#endif

	if (HAK_LIKELY(!hak->nil_process))
	{
		/* Create a nil process used to simplify nil check in GC.
		* only accessible by VM. not exported via the global dictionary. */
		tmp = (hak_oop_t)hak_instantiate(hak, hak->c_process, HAK_NULL, 0);
		if (HAK_UNLIKELY(!tmp)) goto oops;
		hak->nil_process = (hak_oop_process_t)tmp;
		hak->nil_process->id = HAK_SMOOI_TO_OOP(-1);
		hak->nil_process->state = HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_TERMINATED);
	#if 0
		hak->nil_process->perr = HAK_ERROR_TO_OOP(HAK_ENOERR);
		hak->nil_process->perrmsg = hak->_nil;
	#endif

		/* unusable stack */
		hak->nil_process->sp = HAK_SMOOI_TO_OOP(-1);
		hak->nil_process->st = HAK_SMOOI_TO_OOP(-1);
		/* unusable exception stack */
		hak->nil_process->exsp = HAK_SMOOI_TO_OOP(-1);
		hak->nil_process->exst = HAK_SMOOI_TO_OOP(-1);
		/* unusable class stack */
		hak->nil_process->clsp = HAK_SMOOI_TO_OOP(-1);
		hak->nil_process->clst =  HAK_SMOOI_TO_OOP(-1);
	}

	if (HAK_LIKELY(!hak->processor))
	{
		/* Create a process scheduler */
		tmp = (hak_oop_t)hak_instantiate(hak, hak->c_process_scheduler, HAK_NULL, 0);
		if (HAK_UNLIKELY(!tmp)) goto oops;
		hak->processor = (hak_oop_process_scheduler_t)tmp;
		hak->processor->active = hak->nil_process;
		hak->processor->total_count = HAK_SMOOI_TO_OOP(0);
		hak->processor->runnable.count = HAK_SMOOI_TO_OOP(0);
		hak->processor->suspended.count = HAK_SMOOI_TO_OOP(0);
	}

	return 0;

oops:
	return -1;
}

static int ignite_3 (hak_t* hak)
{
	/* Register kernel classes manually created so far to the system dictionary */
#if 0
	static hak_ooch_t str_processor[] = { 'P', 'r', 'o', 'c', 'e', 's', 's', 'o', 'r' };
	static hak_ooch_t str_dicnew[] = { 'n', 'e', 'w', ':' };
	static hak_ooch_t str_dicputassoc[] = { '_','_','p', 'u', 't', '_', 'a', 's', 's', 'o', 'c', ':' };
	static hak_ooch_t str_does_not_understand[] = { 'd', 'o', 'e', 's', 'N', 'o', 't', 'U', 'n', 'd', 'e', 'r', 's', 't', 'a', 'n', 'd', ':' };
	static hak_ooch_t str_primitive_failed[] = {  'p', 'r', 'i', 'm', 'i', 't', 'i', 'v', 'e', 'F', 'a', 'i', 'l', 'e', 'd' };
	static hak_ooch_t str_unwindto_return[] = { 'u', 'n', 'w', 'i', 'n', 'd', 'T', 'o', ':', 'r', 'e', 't', 'u', 'r', 'n', ':' };
#endif

	hak_oow_t i;
	hak_oop_t sym;
	hak_oop_class_t _class;

	for (i = 0; i < HAK_COUNTOF(kernel_classes); i++)
	{
		sym = hak_makesymbolwithbcstr(hak, kernel_classes[i].name);
		if (HAK_UNLIKELY(!sym)) return -1;

		_class = *(hak_oop_class_t*)((hak_uint8_t*)hak + kernel_classes[i].offset);
		HAK_STORE_OOP(hak, (hak_oop_t*)&_class->name, sym);
#if 0
		HAK_STORE_OOP(hak, (hak_oop_t*)&_class->nsup, (hak_oop_t)hak->sysdic);
#endif

		if (!hak_putatsysdic(hak, sym, (hak_oop_t)_class)) return -1;
	}

#if 0
	/* Attach the system dictionary to the nsdic field of the System class */
	HAK_STORE_OOP(hak, (hak_oop_t*)&hak->_system->nsdic, (hak_oop_t)hak->sysdic);
	/* Set the name field of the system dictionary */
	HAK_STORE_OOP(hak, (hak_oop_t*)&hak->sysdic->name, (hak_oop_t)hak->_system->name);
	/* Set the owning class field of the system dictionary, it's circular here */
	HAK_STORE_OOP(hak, (hak_oop_t*)&hak->sysdic->nsup, (hak_oop_t)hak->_system);

	/* Make the process scheduler avaialble as the global name 'Processor' */
	sym = hak_makesymbol(hak, str_processor, HAK_COUNTOF(str_processor));
	if (!sym) return -1;
	if (!hak_putatsysdic(hak, sym, (hak_oop_t)hak->processor)) return -1;

	sym = hak_makesymbol(hak, str_dicnew, HAK_COUNTOF(str_dicnew));
	if (!sym) return -1;
	hak->dicnewsym = (hak_oop_char_t)sym;

	sym = hak_makesymbol(hak, str_dicputassoc, HAK_COUNTOF(str_dicputassoc));
	if (!sym) return -1;
	hak->dicputassocsym = (hak_oop_char_t)sym;

	sym = hak_makesymbol(hak, str_does_not_understand, HAK_COUNTOF(str_does_not_understand));
	if (!sym) return -1;
	hak->does_not_understand_sym = (hak_oop_char_t)sym;

	sym = hak_makesymbol(hak, str_primitive_failed, HAK_COUNTOF(str_primitive_failed));
	if (!sym) return -1;
	hak->primitive_failed_sym = (hak_oop_char_t)sym;

	sym = hak_makesymbol(hak, str_unwindto_return, HAK_COUNTOF(str_unwindto_return));
	if (!sym) return -1;
	hak->unwindto_return_sym = (hak_oop_char_t)sym;
#endif

	return 0;
}


static int make_kernel_objs (hak_t* hak)
{
	/* make_kernel_objs() creates a chain of classes as well as some key objects
	 * for initial bootstrapping. when the objects are loaded from an image file,
	 * this function is skipped */

#if 0
	hak->igniting = 1;
#endif
	if (HAK_LIKELY(!hak->_undef))
	{ /* TODO: create it as nogc */
		hak->_undef = hak_hatchundef(hak);
		if (HAK_UNLIKELY(!hak->_undef)) goto oops;
	}

	if (HAK_LIKELY(!hak->_nil))
	{ /* TODO: create it as nogc? */
		hak->_nil = hak_hatchnil(hak);
		if (HAK_UNLIKELY(!hak->_nil)) goto oops;
	}

	if (ignite_1(hak) <= -1) goto oops;

	/* ready to set the class of object created prior to class creation in ignite_1() */
	HAK_OBJ_SET_CLASS (hak->_nil, (hak_oop_t)hak->c_undefobj);
	HAK_OBJ_SET_CLASS (hak->_undef, (hak_oop_t)hak->c_undefobj);

	if (ignite_2(hak) <= -1) goto oops;

	if (ignite_3(hak) <= -1) goto oops;

/* TODO: scan the heap. and fix the class of objects using brand if the class is NULL */

#if 0
	hak->igniting = 0;
#endif
	return 0;

oops:
#if 0
	hak>igniting = 0;
#endif
	return -1;
}

int hak_ignite (hak_t* hak, hak_oow_t heapsize)
{
	if (!hak->heap)
	{
		hak->heap = hak_makeheap(hak, heapsize);
		if (HAK_UNLIKELY(!hak->heap)) return -1;
	}

	if (make_kernel_objs(hak) <= -1) return -1;

	HAK_ASSERT(hak, hak->_true != HAK_NULL);
	HAK_ASSERT(hak, hak->_false != HAK_NULL);

	if (!hak->symtab)
	{
		hak->symtab = (hak_oop_dic_t)hak_makedic(hak, hak->option.dfl_symtab_size);
		if (HAK_UNLIKELY(!hak->symtab)) goto oops;
	}

	if (!hak->sysdic)
	{
		hak->sysdic = (hak_oop_dic_t)hak_makedic(hak, hak->option.dfl_sysdic_size);
		if (HAK_UNLIKELY(!hak->sysdic)) goto oops;
	}

	if (!hak->nil_process)
	{
		/* Create a nil process used to simplify nil check in GC.
		 * only accessible by VM. not exported via the global dictionary. */
		/*hak->nil_process = (hak_oop_process_t)hak_allocoopobj(hak, HAK_BRAND_PROCESS, HAK_PROCESS_NAMED_INSTVARS);*/
		hak->nil_process = (hak_oop_process_t)hak_instantiate(hak, hak->c_process, HAK_NULL, 0);
		if (HAK_UNLIKELY(!hak->nil_process))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak),
				"unable to instantiate %O to be nil process - %js", hak->c_process->name, orgmsg);
			goto oops;
		}

		/* unusable stack */
		hak->nil_process->sp = HAK_SMOOI_TO_OOP(-1);
		hak->nil_process->st = HAK_SMOOI_TO_OOP(-1);
		/* unusable exception stack */
		hak->nil_process->exsp = HAK_SMOOI_TO_OOP(-1);
		hak->nil_process->exst = HAK_SMOOI_TO_OOP(-1);
		/* unusable class stack */
		hak->nil_process->clsp = HAK_SMOOI_TO_OOP(-1);
		hak->nil_process->clst =  HAK_SMOOI_TO_OOP(-1);
	}

	if (!hak->processor)
	{
		/*hak->processor = (hak_oop_process_scheduler_t)hak_allocoopobj(hak, HAK_BRAND_PROCESS_SCHEDULER, HAK_PROCESS_SCHEDULER_NAMED_INSTVARS);*/
		hak->processor = (hak_oop_process_scheduler_t)hak_instantiate(hak, hak->c_process_scheduler, HAK_NULL, 0);
		if (HAK_UNLIKELY(!hak->processor))
		{
			const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, HAK_ERRNUM(hak),
				"unable to instantiate %O - %js",hak->c_process_scheduler->name, orgmsg);
			goto oops;
		}

		hak->processor->active = hak->nil_process;
		hak->processor->total_count = HAK_SMOOI_TO_OOP(0);
		hak->processor->runnable.count = HAK_SMOOI_TO_OOP(0);
		hak->processor->suspended.count = HAK_SMOOI_TO_OOP(0);

		/* commit the sp field of the initial active context to hak->sp */
		hak->sp = HAK_OOP_TO_SMOOI(hak->processor->active->sp);
	}

	/* TODO: move this initialization to hak_init? */
	if (hak_brewcode(hak, &hak->code) <= -1) goto oops;

	hak->p.e = hak->_nil;
	return 0;

oops:
	return -1;
}
