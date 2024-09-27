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
	hcl_syncode_t syncode;
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
	{  3, { 'f','u','n' },                      HCL_SYNCODE_FUN,       HCL_OFFSETOF(hcl_t,s_fun)    },
	{  2, { 'i','f' },                          HCL_SYNCODE_IF,        HCL_OFFSETOF(hcl_t,s_if)     },
	{  2, { 'o','r' },                          HCL_SYNCODE_OR,        HCL_OFFSETOF(hcl_t,s_or)     },
	{  4, { 'p','l','u','s' },                  HCL_SYNCODE_PLUS,      HCL_OFFSETOF(hcl_t,s_plus)   },
	{  6, { 'r','e','t','u','r','n'},           HCL_SYNCODE_RETURN,    HCL_OFFSETOF(hcl_t,s_return) },
	{  6, { 'r','e','v','e','r','t'},           HCL_SYNCODE_REVERT,    HCL_OFFSETOF(hcl_t,s_revert) },
	{  3, { 's','e','t' },                      HCL_SYNCODE_SET,       HCL_OFFSETOF(hcl_t,s_set)    },
	{  5, { 's','e','t','-','r' },              HCL_SYNCODE_SET_R,     HCL_OFFSETOF(hcl_t,s_set_r)  },
	{  5, { 't','h','r','o','w' },              HCL_SYNCODE_THROW,     HCL_OFFSETOF(hcl_t,s_throw)  },
	{  3, { 't','r','y' },                      HCL_SYNCODE_TRY,       HCL_OFFSETOF(hcl_t,s_try)   },
	{  5, { 'u','n','t','i','l' },              HCL_SYNCODE_UNTIL,     HCL_OFFSETOF(hcl_t,s_until)  },
	{  5, { 'w','h','i','l','e' },              HCL_SYNCODE_WHILE,     HCL_OFFSETOF(hcl_t,s_while)  }
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
	const hcl_bch_t* name;
	int superclass_kci;
	int class_brand;
	int class_flags; /* class flags for selfspec */
	int class_ncvars; /* number of class variables */

	int class_spec_nivars; /* number of named instance variables */
	int class_spec_flags;
	int class_spec_indexed_type;

	hcl_oow_t  offset; /* offset to the field in hcl_t that stored the class pointer */
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
#define KCI(x) HCL_AID(x)

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
		HCL_OBJ_TYPE_OOP, /* indexed type */
		HCL_OFFSETOF(hcl_t, c_apex)
	},

	KCI(KCI_CLASS) {
		"Class",
		KCI_APEX,
		HCL_BRAND_CLASS,
		HCL_CLASS_SELFSPEC_FLAG_LIMITED,
		0, /* ncvars */
		HCL_CLASS_NAMED_INSTVARS, /* nivars */
		HCL_CLASS_SPEC_FLAG_INDEXED | HCL_CLASS_SPEC_FLAG_UNCOPYABLE,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_class)
	},

	KCI(KCI_UNDEFINED_OBJECT) {
		"UndefinedObject",
		KCI_APEX,
		HCL_BRAND_UNDEF,
		0,
		0,
		0,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_undefobj)
	},

	KCI(KCI_NIL_OBJECT) {
		"NilObject",
		KCI_APEX,
		HCL_BRAND_NIL,
		0,
		0,
		0,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_nilobj)
	},

#if 0
	{ "Interface",
	  HCL_CLASS_SELFSPEC_FLAG_LIMITED,
	  0,
	  HCL_INTERFACE_NAMED_INSTVARS,
	  HCL_CLASS_SPEC_FLAG_INDEXED | HCL_CLASS_SPEC_FLAG_UNCOPYABLE,
	  HCL_OBJ_TYPE_OOP,
	  HCL_OFFSETOF(hcl_t, _interface) },
#endif

	KCI(KCI_OBJECT) {
		"Object",
		KCI_APEX,
		0, /* brand */
		0, /* selfspec flags */
		0, /* ncvars */
		0, /* nivars */
		0, /* spec flags */
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_object)
	},

	KCI(KCI_COLLECTION) {
		"Collection",
		KCI_OBJECT,
		0, /* brand */
		0,
		0,
		0,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_collection)
	},

	KCI(KCI_INDEXED_COLLECTION) {
		"IndexedCollection",
		KCI_COLLECTION,
		0, /* brand */
		0,
		0,
		0,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_indexed_collection)
	},

	KCI(KCI_FIXED_SIZED_COLLECTION) {
		"FixedSizedCollection",
		KCI_INDEXED_COLLECTION,
		0, /* brand */
		0,
		0,
		0,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_fixed_sized_collection)
	},

	KCI(KCI_STRING) {
		"String",
		KCI_FIXED_SIZED_COLLECTION,
		HCL_BRAND_STRING,
		0,
		0,
		0,
		HCL_CLASS_SPEC_FLAG_INDEXED,
		HCL_OBJ_TYPE_CHAR,
		HCL_OFFSETOF(hcl_t, c_string)
	},

	KCI(KCI_BYTE_STRING) {
		"ByteString",
		KCI_FIXED_SIZED_COLLECTION,
		HCL_BRAND_BYTE_STRING,
		0,
		0,
		0,
		HCL_CLASS_SPEC_FLAG_INDEXED,
		HCL_OBJ_TYPE_BYTE,
		HCL_OFFSETOF(hcl_t, c_byte_string)
	},

	KCI(KCI_SYMBOL) {
		"Symbol",
		KCI_STRING,
		HCL_BRAND_SYMBOL,
		HCL_CLASS_SELFSPEC_FLAG_FINAL | HCL_CLASS_SELFSPEC_FLAG_LIMITED, /* TODO: these flags not implemented yet */
		0,
		0,
		HCL_CLASS_SPEC_FLAG_INDEXED | HCL_CLASS_SPEC_FLAG_IMMUTABLE,
		HCL_OBJ_TYPE_CHAR,
		HCL_OFFSETOF(hcl_t, c_symbol)
	},

	KCI(KCI_ARRAY) {
		"Array",
		KCI_FIXED_SIZED_COLLECTION,
		HCL_BRAND_ARRAY,
		0,
		0,
		0,
		HCL_CLASS_SPEC_FLAG_INDEXED,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_array)
	},

	KCI(KCI_CHARACTER_ARRAY) {
		"CharacterArray",
		KCI_FIXED_SIZED_COLLECTION,
		HCL_BRAND_CHARACTER_ARRAY,
		0,
		0,
		0,
		HCL_CLASS_SPEC_FLAG_INDEXED,
		HCL_OBJ_TYPE_CHAR,
		HCL_OFFSETOF(hcl_t, c_character_array)
	},

	KCI(KCI_BYTE_ARRAY) {
		"ByteArray",
		KCI_FIXED_SIZED_COLLECTION,
		HCL_BRAND_BYTE_ARRAY,
		0,
		0,
		0,
		HCL_CLASS_SPEC_FLAG_INDEXED,
		HCL_OBJ_TYPE_BYTE,
		HCL_OFFSETOF(hcl_t, c_byte_array)
	},

	/* A special incarnation of a dictionary that allows only a symbol as a value.
	 * The value in bucket is a symbol while the value in a normal dictionary is a
	 * pair(cons) that contains a key and a value. */
	KCI(KCI_SYMBOL_TABLE) {
		"SymbolTable",
		KCI_COLLECTION,
		HCL_BRAND_DIC, /* TODO: make this a special child class of Dictionary?? */
		0,
		0,
		HCL_DIC_NAMED_INSTVARS,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_symtab)
	},

	KCI(KCI_DICTIONARY) {
		"Dictionary",
		KCI_COLLECTION,
		HCL_BRAND_DIC,
		0,
		0,
		HCL_DIC_NAMED_INSTVARS,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_dictionary)
	},

	KCI(KCI_CONS) {
		"Cons",
		KCI_OBJECT,
		HCL_BRAND_CONS,
		0,
		0,
		HCL_CONS_NAMED_INSTVARS,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_cons)
	},

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

	KCI(KCI_METHOD_DICTIONARY) {
		"MethodDictionary",
		KCI_DICTIONARY,
		0,
		0,
		0,
		HCL_DIC_NAMED_INSTVARS,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_method_dictionary)
	},

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

	/* special function created with MAKE_FUNCTION in interactive mode
	 * for execution of code fed and compiled.  */
	KCI(KCI_FUNCTION) {
		"Function",
		KCI_OBJECT,
		HCL_BRAND_FUNCTION,
		0,
		0,
		HCL_FUNCTION_NAMED_INSTVARS,
		HCL_CLASS_SPEC_FLAG_INDEXED,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_function)
	},

	KCI(KCI_PRIMITIVE) {
		"Primitive",
		KCI_OBJECT,
		HCL_BRAND_PRIM,
		0,
		0,
		HCL_PRIM_NAMED_INSTVARS,
		0,
		HCL_OBJ_TYPE_WORD,
		HCL_OFFSETOF(hcl_t, c_primitive)
	},

	KCI(KCI_COMPILED_BLOCK) {
		"CompiledBlock",
		KCI_OBJECT,
		HCL_BRAND_BLOCK,
		0,
		0,
		HCL_BLOCK_NAMED_INSTVARS,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_compiled_block)
	},

	KCI(KCI_BLOCK_CONTEXT) {
		"BlockContext",
		KCI_OBJECT,
		HCL_BRAND_CONTEXT,
		HCL_CLASS_SELFSPEC_FLAG_FINAL | HCL_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		HCL_CONTEXT_NAMED_INSTVARS,
		HCL_CLASS_SPEC_FLAG_INDEXED,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_block_context)
	},

	KCI(KCI_PROCESS) {
		"Process",
		KCI_OBJECT,
		HCL_BRAND_PROCESS,
		HCL_CLASS_SELFSPEC_FLAG_FINAL | HCL_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		HCL_PROCESS_NAMED_INSTVARS,
		HCL_CLASS_SPEC_FLAG_INDEXED | HCL_CLASS_SPEC_FLAG_UNCOPYABLE,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_process)
	},

	KCI(KCI_SEMAPHORE) {
		"Semaphore",
		KCI_OBJECT,
		HCL_BRAND_SEMAPHORE,
		0,
		0,
		HCL_SEMAPHORE_NAMED_INSTVARS,
		HCL_CLASS_SPEC_FLAG_UNCOPYABLE,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_semaphore)
	},

	KCI(KCI_SEMAPHORE_GROUP) {
		"SemaphoreGroup",
		KCI_OBJECT,
		HCL_BRAND_SEMAPHORE_GROUP,
		0,
		0,
		HCL_SEMAPHORE_GROUP_NAMED_INSTVARS,
		HCL_CLASS_SPEC_FLAG_UNCOPYABLE,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_semaphore_group)
	},

	KCI(KCI_PROCESS_SCHEDULER) {
		"ProcessScheduler",
		KCI_OBJECT,
		HCL_BRAND_PROCESS_SCHEDULER,
		HCL_CLASS_SELFSPEC_FLAG_FINAL | HCL_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		HCL_PROCESS_SCHEDULER_NAMED_INSTVARS,
		HCL_CLASS_SPEC_FLAG_UNCOPYABLE,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_process_scheduler)
	},

	KCI(KCI_ERROR) {
		"Error",
		KCI_OBJECT,
		0,
		HCL_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		0,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_error)
	},

	KCI(KCI_TRUE) {
		"True",
		KCI_OBJECT,
		HCL_BRAND_TRUE,
		HCL_CLASS_SELFSPEC_FLAG_LIMITED | HCL_CLASS_SELFSPEC_FLAG_FINAL,
		0,
		0,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_true)
	},

	KCI(KCI_FALSE) {
		"False",
		KCI_OBJECT,
		HCL_BRAND_FALSE,
		HCL_CLASS_SELFSPEC_FLAG_LIMITED | HCL_CLASS_SELFSPEC_FLAG_FINAL,
		0,
		0,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_false)
	},

	KCI(KCI_MAGNITUDE) {
		"Magnitude",
		KCI_OBJECT,
		0, /* brand */
		0,
		0,
		0,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_magnitude)
	},

	/* TOOD: what is a proper spec for Character and SmallInteger?
	 *       If the fixed part is  0, its instance must be an object of 0 payload fields.
	 *       Does this make sense? */
	KCI(KCI_CHARACTER) {
		"Character",
		KCI_MAGNITUDE,
		HCL_BRAND_CHARACTER,
		HCL_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		0,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_character)
	},

	KCI(KCI_NUMBER) {
		"Number",
		KCI_MAGNITUDE,
		0, /* brand */
		0,
		0,
		0,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_number)
	},

	KCI(KCI_SMALL_INTEGER) {
		"SmallInteger",
		KCI_NUMBER,
		HCL_BRAND_SMOOI,
		HCL_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		0,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_small_integer)
	},

	KCI(KCI_LARGE_POSITIVE_INTEGER) {
		"LargePositiveInteger",
		KCI_NUMBER,
		HCL_BRAND_PBIGINT,
		HCL_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		0,
		HCL_CLASS_SPEC_FLAG_INDEXED | HCL_CLASS_SPEC_FLAG_IMMUTABLE,
		HCL_OBJ_TYPE_LIWORD,
		HCL_OFFSETOF(hcl_t, c_large_positive_integer)
	},

	KCI(KCI_LARGE_NEGATIVE_INTEGER) {
		"LargeNegativeInteger",
		KCI_NUMBER,
		HCL_BRAND_NBIGINT,
		HCL_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		0,
		HCL_CLASS_SPEC_FLAG_INDEXED | HCL_CLASS_SPEC_FLAG_IMMUTABLE,
		HCL_OBJ_TYPE_LIWORD,
		HCL_OFFSETOF(hcl_t, c_large_negative_integer)
	},

	KCI(KCI_FIXED_POINT_DECIMAL) {
		"FixedPointDecimal",
		KCI_NUMBER,
		HCL_BRAND_FPDEC,
		HCL_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		HCL_FPDEC_NAMED_INSTVARS,
		HCL_CLASS_SPEC_FLAG_IMMUTABLE,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_fixed_point_decimal)
	},

	KCI(KCI_SMALL_POINTER) {
		"SmallPointer",
		KCI_MAGNITUDE,
		HCL_BRAND_SMPTR,
		HCL_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		0,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_small_pointer)
	},

	KCI(KCI_LARGE_POINTER) {
		"LargePointer",
		KCI_MAGNITUDE,
		0,
		HCL_CLASS_SELFSPEC_FLAG_LIMITED,
		0,
		1,  /* #word(1) */
		HCL_CLASS_SPEC_FLAG_IMMUTABLE | HCL_CLASS_SPEC_FLAG_INDEXED,
		HCL_OBJ_TYPE_WORD,
		HCL_OFFSETOF(hcl_t, c_large_pointer)
	},

	KCI(KCI_SYSTEM) {
		"System",
		KCI_OBJECT,
		0,
		0,
		5, /* asyncsg, gcfin_sem, gcfin_should_exit, ossig_pid, shr */
		0,
		0,
		HCL_OBJ_TYPE_OOP,
		HCL_OFFSETOF(hcl_t, c_system)
	}
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
			if (HCL_UNLIKELY(HCL_OBJ_GET_FLAGS_PROC(oop)))
			{
				hcl_oop_process_t proc;

				HCL_ASSERT (hcl, HCL_IS_PROCESS(hcl, oop));
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
		if (cb->on_gc) cb->on_gc (hcl);
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
		/* set the lazy sweeping pointer to the head of the allocated blocks.
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
	ptr = (hcl_uint8_t*)HCL_ALIGN((hcl_uintptr_t)hcl->newheap->base, HCL_SIZEOF(hcl_oop_t));
	ptr = scan_new_heap(hcl, ptr);

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
	if (HCL_OOP_IS_POINTER(oop) && HCL_OBJ_GET_CLASS(oop) != (hcl_oop_t)hcl->c_symbol)
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

/* -----------------------------------------------------------------------
 * BOOTSTRAPPER
 * ----------------------------------------------------------------------- */

static hcl_oop_class_t alloc_kernel_class (hcl_t* hcl, int class_flags, hcl_oow_t num_classvars, hcl_oow_t spec, hcl_ooi_t nivars_super, int ibrand)
{
	hcl_oop_class_t c;
	hcl_ooi_t cspec;

	c = (hcl_oop_class_t)hcl_allocoopobj(hcl, HCL_BRAND_CLASS, HCL_CLASS_NAMED_INSTVARS + num_classvars);
	if (HCL_UNLIKELY(!c)) return HCL_NULL;

	HCL_OBJ_SET_FLAGS_KERNEL (c, HCL_OBJ_FLAGS_KERNEL_IMMATURE);

	cspec = kernel_classes[KCI_CLASS].class_spec_flags;
	if (HCL_CLASS_SPEC_IS_IMMUTABLE(cspec)) HCL_OBJ_SET_FLAGS_RDONLY (c, 1); /* just for completeness of code. will never be true as it's not defined in the kernel class info table */
#if 0 /* TODO extend the flags and uncomment this part */
	if (HCL_CLASS_SPEC_IS_UNCOPYABLE(cspec)) HCL_OBJ_SET_FLAGS_UNCOPYABLE (c, 1); /* class itself is uncopyable */
#endif

	HCL_OBJ_SET_CLASS (c, (hcl_oop_t)hcl->c_class);
	c->spec = HCL_SMOOI_TO_OOP(spec);
	c->selfspec = HCL_SMOOI_TO_OOP(HCL_CLASS_SELFSPEC_MAKE(num_classvars, 0, class_flags));
	c->nivars_super = HCL_SMOOI_TO_OOP(nivars_super); /* TODO: encode it into spec? */
	c->ibrand = HCL_SMOOI_TO_OOP(ibrand);

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

	HCL_ASSERT (hcl, hcl->c_class == HCL_NULL);
	/* --------------------------------------------------------------
	 * Class
	 * The instance of Class can have indexed instance variables
	 * which are actually class variables.
	 * -------------------------------------------------------------- */
	if (HCL_LIKELY(!hcl->c_class))
	{
		HCL_ASSERT (hcl, kernel_classes[KCI_CLASS].superclass_kci >= 0);
		hcl->c_class = alloc_kernel_class(
			hcl,
			kernel_classes[KCI_CLASS].class_flags,
			kernel_classes[KCI_CLASS].class_ncvars,
			HCL_CLASS_SPEC_MAKE(kernel_classes[KCI_CLASS].class_spec_nivars,
			                    kernel_classes[KCI_CLASS].class_spec_flags,
			                    kernel_classes[KCI_CLASS].class_spec_indexed_type),
			kernel_classes[kernel_classes[KCI_CLASS].superclass_kci].class_spec_nivars,
			kernel_classes[KCI_CLASS].class_brand);
		if (HCL_UNLIKELY(!hcl->c_class))
		{
			const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
			hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to allocate %hs - %js", kernel_classes[KCI_CLASS].name, orgmsg);
			return -1;
		}

		HCL_ASSERT (hcl, HCL_OBJ_GET_CLASS(hcl->c_class) == HCL_NULL);
		HCL_OBJ_SET_CLASS (hcl->c_class, (hcl_oop_t)hcl->c_class);
	}

	/* create class objects except Class */
	for (i = 0; i < HCL_COUNTOF(kernel_classes); i++)
	{
		hcl_oop_class_t tmp;
		hcl_ooi_t nivars_super;
		int superclass_kci;

		if (i == KCI_CLASS) continue; /* skip Class as it's created above */

		superclass_kci = kernel_classes[i].superclass_kci;
		nivars_super = superclass_kci <= -1? 0: kernel_classes[superclass_kci].class_spec_nivars;
		tmp = alloc_kernel_class(
			hcl,
			kernel_classes[i].class_flags,
			kernel_classes[i].class_ncvars,
			HCL_CLASS_SPEC_MAKE(kernel_classes[i].class_spec_nivars,
			                    kernel_classes[i].class_spec_flags,
			                    kernel_classes[i].class_spec_indexed_type),
			nivars_super,
			kernel_classes[i].class_brand);
		if (HCL_UNLIKELY(!tmp))
		{
			const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
			hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to allocate %hs - %js", kernel_classes[i].name, orgmsg);
			return -1;
		}
		*(hcl_oop_class_t*)((hcl_uint8_t*)hcl + kernel_classes[i].offset) = tmp;
	}

	/* update the superclass field */
	for (i = 0; i < HCL_COUNTOF(kernel_classes); i++)
	{
		int skci;
		skci = kernel_classes[i].superclass_kci;
		if (skci >= 0)
		{
			hcl_oop_class_t* x, * y;
			x = (hcl_oop_class_t*)((hcl_uint8_t*)hcl + kernel_classes[i].offset);
			y = (hcl_oop_class_t*)((hcl_uint8_t*)hcl + kernel_classes[skci].offset);
			(*x)->superclass = (hcl_oop_t)*y;
		}
	}

#if 0
	/* an instance of a method class stores byte codes in the trailer space.
	 * unlike other classes with trailer size set, the size of the trailer
	 * space is not really determined by the trailer size set in the class.
	 * the compiler determines the actual size of the trailer space depending
	 * on the byte codes generated. i should set the following fields to avoid
	 * confusion at the GC phase. */
	hcl->c_method->trsize = HCL_SMOOI_TO_OOP(0);
	hcl->c_method->trgc = HCL_SMPTR_TO_OOP(0);
#endif

	return 0;
}

static int ignite_2 (hcl_t* hcl)
{
	hcl_oop_t tmp;
#if 0
	int old_igniting = hcl->igniting;
#endif

	/* Create 'true' and 'false objects */
	if (HCL_LIKELY(!hcl->_true))
	{
		hcl->_true = hcl_instantiate(hcl, hcl->c_true, HCL_NULL, 0);
		if (HCL_UNLIKELY(!hcl->_true)) goto oops;
	}

	if (HCL_LIKELY(!hcl->_false))
	{
		hcl->_false = hcl_instantiate(hcl, hcl->c_false, HCL_NULL, 0);
		if (HCL_UNLIKELY(!hcl->_false)) goto oops;
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
#endif

	if (HCL_LIKELY(!hcl->symtab))
	{
		/* Create the symbol table - values in the bucket are limited to symbols only */
		tmp = hcl_instantiate(hcl, hcl->c_symtab, HCL_NULL, 0);
		if (HCL_UNLIKELY(!tmp)) goto oops;
		hcl->symtab = (hcl_oop_dic_t)tmp;
		hcl->symtab->tally = HCL_SMOOI_TO_OOP(0);
	}

	if (HCL_LIKELY((hcl_oop_t)hcl->symtab->bucket == hcl->_nil))
	{
		/* It's important to assign the result of hcl_instantiate() to a temporary
		* variable first and then assign it to hcl->symtab->bucket.
		* The pointer 'hcl->symtab; can change in hcl_instantiate() and the
		* target address of assignment may get set before hcl_instantiate()
		* is called. */
		HCL_ASSERT (hcl, hcl->option.dfl_symtab_size > 0);
		tmp = hcl_instantiate(hcl, hcl->c_array, HCL_NULL, hcl->option.dfl_symtab_size);
		if (HCL_UNLIKELY(!tmp)) goto oops; /* TODO: delete hcl->symtab instad of this separate initialization of the bucket??? */
		hcl->symtab->bucket = (hcl_oop_oop_t)tmp;
	}

#if 0
	/* Create the system dictionary */
	tmp = (hcl_oop_t)hcl_makensdic(hcl, hcl->_namespace, hcl->option.dfl_sysdic_size);
	if (!tmp) return -1;
	hcl->sysdic = (hcl_oop_nsdic_t)tmp;
#else
	if (HCL_LIKELY(!hcl->sysdic))
	{
		tmp = hcl_instantiate(hcl, hcl->c_dictionary, HCL_NULL, 0);
		if (HCL_UNLIKELY(!tmp)) goto oops;
		hcl->sysdic = (hcl_oop_dic_t)tmp;
		hcl->sysdic->tally = HCL_SMOOI_TO_OOP(0);
	}

	if (HCL_LIKELY((hcl_oop_t)hcl->sysdic->bucket == hcl->_nil))
	{
		/* It's important to assign the result of hcl_instantiate() to a temporary
		* variable first and then assign it to hcl->symtab->bucket.
		* The pointer 'hcl->symtab; can change in hcl_instantiate() and the
		* target address of assignment may get set before hcl_instantiate()
		* is called. */
		tmp = hcl_instantiate(hcl, hcl->c_array, HCL_NULL, hcl->option.dfl_sysdic_size);
		if (HCL_UNLIKELY(!tmp)) goto oops;
		hcl->sysdic->bucket = (hcl_oop_oop_t)tmp;
	}
#endif

#if 0
	hcl->igniting = old_igniting; /* back to the permspace */
#endif

	if (HCL_LIKELY(!hcl->nil_process))
	{
		/* Create a nil process used to simplify nil check in GC.
		* only accessible by VM. not exported via the global dictionary. */
		tmp = (hcl_oop_t)hcl_instantiate(hcl, hcl->c_process, HCL_NULL, 0);
		if (HCL_UNLIKELY(!tmp)) goto oops;
		hcl->nil_process = (hcl_oop_process_t)tmp;
		hcl->nil_process->id = HCL_SMOOI_TO_OOP(-1);
		hcl->nil_process->state = HCL_SMOOI_TO_OOP(HCL_PROCESS_STATE_TERMINATED);
	#if 0
		hcl->nil_process->perr = HCL_ERROR_TO_OOP(HCL_ENOERR);
		hcl->nil_process->perrmsg = hcl->_nil;
	#endif

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

	if (HCL_LIKELY(!hcl->processor))
	{
		/* Create a process scheduler */
		tmp = (hcl_oop_t)hcl_instantiate(hcl, hcl->c_process_scheduler, HCL_NULL, 0);
		if (HCL_UNLIKELY(!tmp)) goto oops;
		hcl->processor = (hcl_oop_process_scheduler_t)tmp;
		hcl->processor->active = hcl->nil_process;
		hcl->processor->total_count = HCL_SMOOI_TO_OOP(0);
		hcl->processor->runnable.count = HCL_SMOOI_TO_OOP(0);
		hcl->processor->suspended.count = HCL_SMOOI_TO_OOP(0);
	}

	return 0;

oops:
	return -1;
}

static int ignite_3 (hcl_t* hcl)
{
	/* Register kernel classes manually created so far to the system dictionary */
#if 0
	static hcl_ooch_t str_processor[] = { 'P', 'r', 'o', 'c', 'e', 's', 's', 'o', 'r' };
	static hcl_ooch_t str_dicnew[] = { 'n', 'e', 'w', ':' };
	static hcl_ooch_t str_dicputassoc[] = { '_','_','p', 'u', 't', '_', 'a', 's', 's', 'o', 'c', ':' };
	static hcl_ooch_t str_does_not_understand[] = { 'd', 'o', 'e', 's', 'N', 'o', 't', 'U', 'n', 'd', 'e', 'r', 's', 't', 'a', 'n', 'd', ':' };
	static hcl_ooch_t str_primitive_failed[] = {  'p', 'r', 'i', 'm', 'i', 't', 'i', 'v', 'e', 'F', 'a', 'i', 'l', 'e', 'd' };
	static hcl_ooch_t str_unwindto_return[] = { 'u', 'n', 'w', 'i', 'n', 'd', 'T', 'o', ':', 'r', 'e', 't', 'u', 'r', 'n', ':' };
#endif

	hcl_oow_t i;
	hcl_oop_t sym;
	hcl_oop_class_t _class;

	for (i = 0; i < HCL_COUNTOF(kernel_classes); i++)
	{
		sym = hcl_makesymbolwithbcstr(hcl, kernel_classes[i].name);
		if (HCL_UNLIKELY(!sym)) return -1;

		_class = *(hcl_oop_class_t*)((hcl_uint8_t*)hcl + kernel_classes[i].offset);
		HCL_STORE_OOP (hcl, (hcl_oop_t*)&_class->name, sym);
#if 0
		HCL_STORE_OOP (hcl, (hcl_oop_t*)&_class->nsup, (hcl_oop_t)hcl->sysdic);
#endif

		if (!hcl_putatsysdic(hcl, sym, (hcl_oop_t)_class)) return -1;
	}

#if 0
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
#endif

	return 0;
}


static int make_kernel_objs (hcl_t* hcl)
{
	/* make_kernel_objs() creates a chain of classes as well as some key objects
	 * for initial bootstrapping. when the objects are loaded from an image file,
	 * this function is skipped */

#if 0
	hcl->igniting = 1;
#endif
	if (HCL_LIKELY(!hcl->_undef))
	{ /* TODO: create it as nogc */
		hcl->_undef = hcl_hatchundef(hcl);
		if (HCL_UNLIKELY(!hcl->_undef)) goto oops;
	}

	if (HCL_LIKELY(!hcl->_nil))
	{ /* TODO: create it as nogc? */
		hcl->_nil = hcl_hatchnil(hcl);
		if (HCL_UNLIKELY(!hcl->_nil)) goto oops;
	}

	if (ignite_1(hcl) <= -1) goto oops;

	/* ready to set the class of object created prior to class creation in ignite_1() */
	HCL_OBJ_SET_CLASS (hcl->_nil, (hcl_oop_t)hcl->c_undefobj);
	HCL_OBJ_SET_CLASS (hcl->_undef, (hcl_oop_t)hcl->c_undefobj);

	if (ignite_2(hcl) <= -1) goto oops;

	if (ignite_3(hcl) <= -1) goto oops;

/* TODO: scan the heap. and fix the class of objects using brand if the class is NULL */

#if 0
	hcl->igniting = 0;
#endif
	return 0;

oops:
#if 0
	hcl>igniting = 0;
#endif
	return -1;
}

int hcl_ignite (hcl_t* hcl, hcl_oow_t heapsize)
{
	hcl_oow_t i;

	if (!hcl->heap)
	{
		hcl->heap = hcl_makeheap(hcl, heapsize);
		if (HCL_UNLIKELY(!hcl->heap)) return -1;
	}

	if (make_kernel_objs(hcl) <= -1) return -1;

	HCL_ASSERT (hcl, hcl->_true != HCL_NULL);
	HCL_ASSERT (hcl, hcl->_false != HCL_NULL);

	if (!hcl->symtab)
	{
		hcl->symtab = (hcl_oop_dic_t)hcl_makedic(hcl, hcl->option.dfl_symtab_size);
		if (HCL_UNLIKELY(!hcl->symtab)) goto oops;
	}

	if (!hcl->sysdic)
	{
		hcl->sysdic = (hcl_oop_dic_t)hcl_makedic(hcl, hcl->option.dfl_sysdic_size);
		if (HCL_UNLIKELY(!hcl->sysdic)) goto oops;
	}

	/* symbol table available now. symbols can be created */
	for (i = 0; i < HCL_COUNTOF(syminfo); i++)
	{
		hcl_oop_t tmp;

		tmp = hcl_makesymbol(hcl, syminfo[i].ptr, syminfo[i].len);
		if (HCL_UNLIKELY(!tmp)) goto oops;

		HCL_OBJ_SET_FLAGS_SYNCODE (tmp, syminfo[i].syncode);
		*(hcl_oop_t*)((hcl_uint8_t*)hcl + syminfo[i].offset) = tmp;
	}

	if (!hcl->nil_process)
	{
		/* Create a nil process used to simplify nil check in GC.
		 * only accessible by VM. not exported via the global dictionary. */
		/*hcl->nil_process = (hcl_oop_process_t)hcl_allocoopobj(hcl, HCL_BRAND_PROCESS, HCL_PROCESS_NAMED_INSTVARS);*/
		hcl->nil_process = (hcl_oop_process_t)hcl_instantiate(hcl, hcl->c_process, HCL_NULL, 0);
		if (HCL_UNLIKELY(!hcl->nil_process))
		{
			const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
			hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to make nil process - %js", orgmsg);
			goto oops;
		}

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
		/*hcl->processor = (hcl_oop_process_scheduler_t)hcl_allocoopobj(hcl, HCL_BRAND_PROCESS_SCHEDULER, HCL_PROCESS_SCHEDULER_NAMED_INSTVARS);*/
		hcl->processor = (hcl_oop_process_scheduler_t)hcl_instantiate(hcl, hcl->c_process_scheduler, HCL_NULL, 0);
		if (HCL_UNLIKELY(!hcl->processor))
		{
			const hcl_ooch_t* orgmsg = hcl_backuperrmsg(hcl);
			hcl_seterrbfmt (hcl, HCL_ERRNUM(hcl), "unable to make process scheduler - %js", orgmsg);
			goto oops;
		}

		hcl->processor->active = hcl->nil_process;
		hcl->processor->total_count = HCL_SMOOI_TO_OOP(0);
		hcl->processor->runnable.count = HCL_SMOOI_TO_OOP(0);
		hcl->processor->suspended.count = HCL_SMOOI_TO_OOP(0);

		/* commit the sp field of the initial active context to hcl->sp */
		hcl->sp = HCL_OOP_TO_SMOOI(hcl->processor->active->sp);
	}

	/* TODO: move this initialization to hcl_init? */
	if (hcl_brewcode(hcl, &hcl->code) <= -1) goto oops;

	hcl->p.e = hcl->_nil;
	return 0;

oops:
	return -1;
}

hcl_syncode_t hcl_getsyncodebyoocs_noseterr (hcl_t* hcl, const hcl_oocs_t* name)
{
	hcl_oow_t i;
	for (i = 0; i < HCL_COUNTOF(syminfo); i++)
	{
		if (hcl_comp_oochars(syminfo[i].ptr, syminfo[i].len, name->ptr, name->len) == 0)
			return syminfo[i].syncode;
	}
	return HCL_SYNCODE_NONE; /* indicates no syntax code found */
}

hcl_syncode_t hcl_getsyncode_noseterr (hcl_t* hcl, const hcl_ooch_t* ptr, const hcl_oow_t len)
{
	hcl_oow_t i;
	for (i = 0; i < HCL_COUNTOF(syminfo); i++)
	{
		if (hcl_comp_oochars(syminfo[i].ptr, syminfo[i].len, ptr, len) == 0)
			return syminfo[i].syncode;
	}
	return HCL_SYNCODE_NONE; /* indicates no syntax code found */
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
