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

#ifndef _HAK_XMA_H_
#define _HAK_XMA_H_

/** @file
 * This file defines an extravagant memory allocator. Why? It may be so.
 * The memory allocator allows you to maintain memory blocks from a
 * larger memory chunk allocated with an outer memory allocator.
 * Typically, an outer memory allocator is a standard memory allocator
 * like malloc(). You can isolate memory blocks into a particular chunk.
 *
 * See the example below. Note it omits error handling.
 *
 * @code
 * #include <hak-xma.h>
 * #include <stdio.h>
 * #include <stdarg.h>
 * void dumper (void* ctx, const char* fmt, ...)
 * {
 * 	va_list ap;
 * 	va_start (ap, fmt);
 * 	vfprintf (fmt, ap);
 * 	va_end (ap);
 * }
 * int main ()
 * {
 *   hak_xma_t* xma;
 *   void* ptr1, * ptr2;
 *
 *   // create a new memory allocator obtaining a 100K byte zone
 *   // with the default memory allocator
 *   xma = hak_xma_open(HAK_NULL, 0, 100000L);
 *
 *   ptr1 = hak_xma_alloc(xma, 5000); // allocate a 5K block from the zone
 *   ptr2 = hak_xma_alloc(xma, 1000); // allocate a 1K block from the zone
 *   ptr1 = hak_xma_realloc(xma, ptr1, 6000); // resize the 5K block to 6K.
 *
 *   hak_xma_dump (xma, dumper, HAK_NULL); // dump memory blocks
 *
 *   // the following two lines are not actually needed as the allocator
 *   // is closed after them.
 *   hak_xma_free (xma, ptr2); // dispose of the 1K block
 *   hak_xma_free (xma, ptr1); // dispose of the 6K block
 *
 *   hak_xma_close (xma); //  destroy the memory allocator
 *   return 0;
 * }
 * @endcode
 */
#include <hak-cmn.h>

#define HAK_XMA_ENABLE_STAT

/** @struct hak_xma_t
 * The hak_xma_t type defines a simple memory allocator over a memory zone.
 * It can obtain a relatively large zone of memory and manage it.
 */
typedef struct hak_xma_t hak_xma_t;

/**
 * The hak_xma_fblk_t type defines a memory block allocated.
 */
typedef struct hak_xma_fblk_t hak_xma_fblk_t;
typedef struct hak_xma_mblk_t hak_xma_mblk_t;

#define HAK_XMA_FIXED (32)
#define HAK_XMA_SIZE_BITS ((HAK_SIZEOF_OOW_T*8)-1)

struct hak_xma_t
{
	hak_mmgr_t* _mmgr;

	hak_uint8_t* start; /* zone beginning */
	hak_uint8_t* end; /* zone end */
	int          internal;

	/** pointer array to free memory blocks */
	hak_xma_fblk_t* xfree[HAK_XMA_FIXED + HAK_XMA_SIZE_BITS + 1];

	/** pre-computed value for fast xfree index calculation */
	hak_oow_t     bdec;

#if defined(HAK_XMA_ENABLE_STAT)
	struct
	{
		hak_oow_t total;
		hak_oow_t alloc; /* allocated size */
		hak_oow_t avail; /* available size */
		hak_oow_t nused; /* nubmer of used blocks */
		hak_oow_t nfree; /* number of free blocks */
		hak_oow_t alloc_hwmark; /* high watermark - highest total memory ever allocated */
		hak_oow_t nallocops; /* number of alloc operations */
		hak_oow_t nallocgoodops; /* number of successful alloc operations */
		hak_oow_t nallocbadops; /* number of failed alloc operations */
		hak_oow_t nreallocops; /* number of realloc operations */
		hak_oow_t nreallocgoodops; /* number of good realloc operations */
		hak_oow_t nreallocbadops; /* number of failed realloc operations - could fall back to normal alloc*/
		hak_oow_t nfreeops; /* number of free operations */
	} stat;
#endif
};

/**
 * The hak_xma_dumper_t type defines a printf-like output function
 * for hak_xma_dump().
 */
typedef void (*hak_xma_dumper_t) (
	void*            ctx,
	const hak_bch_t* fmt,
	...
);

#if defined(__cplusplus)
extern "C" {
#endif

/**
 * The hak_xma_open() function creates a memory allocator. It obtains a memory
 * zone of the @a zonesize bytes with the memory manager @a mmgr. It also makes
 * available the extension area of the @a xtnsize bytes that you can get the
 * pointer to with hak_xma_getxtn().
 *
 * @return pointer to a memory allocator on success, #HAK_NULL on failure
 */
HAK_EXPORT hak_xma_t* hak_xma_open (
	hak_mmgr_t* mmgr,    /**< memory manager */
	hak_oow_t   xtnsize, /**< extension size in bytes */
	void*       zoneptr,
	hak_oow_t   zonesize /**< zone size in bytes */
);

/**
 * The hak_xma_close() function destroys a memory allocator. It also frees
 * the memory zone obtained, which invalidates the memory blocks within
 * the zone. Call this function to destroy a memory allocator created with
 * hak_xma_open().
 */
HAK_EXPORT void hak_xma_close (
	hak_xma_t* xma /**< memory allocator */
);

#if defined(HAK_HAVE_INLINE)
static HAK_INLINE hak_mmgr_t* hak_xma_getmmgr (hak_xma_t* xma) { return xma->_mmgr; }
#else
#	define hak_xma_getmmgr(xma) (((hak_xma_t*)(xma))->_mmgr)
#endif

#if defined(HAK_HAVE_INLINE)
static HAK_INLINE void* hak_xma_getxtn (hak_xma_t* xma) { return (void*)(xma + 1); }
#else
#define hak_xma_getxtn(xma) ((void*)((hak_xma_t*)(xma) + 1))
#endif

/**
 * The hak_xma_init() initializes a memory allocator. If you have the hak_xma_t
 * structure statically declared or already allocated, you may pass the pointer
 * to this function instead of calling hak_xma_open(). It obtains a memory zone
 * of @a zonesize bytes with the memory manager @a mmgr. Unlike hak_xma_open(),
 * it does not accept the extension size, thus not creating an extention area.
 * @return 0 on success, -1 on failure
 */
HAK_EXPORT int hak_xma_init (
	hak_xma_t*  xma,     /**< memory allocator */
	hak_mmgr_t* mmgr,    /**< memory manager */
	void*       zoneptr,  /**< pointer to memory zone. if #HAK_NULL, a zone is auto-created */
	hak_oow_t   zonesize /**< zone size in bytes */
);

/**
 * The hak_xma_fini() function finalizes a memory allocator. Call this
 * function to finalize a memory allocator initialized with hak_xma_init().
 */
HAK_EXPORT void hak_xma_fini (
	hak_xma_t* xma /**< memory allocator */
);

/**
 * The hak_xma_alloc() function allocates @a size bytes.
 * @return pointer to a memory block on success, #HAK_NULL on failure
 */
HAK_EXPORT void* hak_xma_alloc (
	hak_xma_t* xma, /**< memory allocator */
	hak_oow_t  size /**< size in bytes */
);

HAK_EXPORT void* hak_xma_calloc (
	hak_xma_t* xma,
	hak_oow_t  size
);

/**
 * The hak_xma_alloc() function resizes the memory block @a b to @a size bytes.
 * @return pointer to a resized memory block on success, #HAK_NULL on failure
 */
HAK_EXPORT void* hak_xma_realloc (
	hak_xma_t* xma,  /**< memory allocator */
	void*      b,    /**< memory block */
	hak_oow_t  size  /**< new size in bytes */
);

/**
 * The hak_xma_alloc() function frees the memory block @a b.
 */
HAK_EXPORT void hak_xma_free (
	hak_xma_t* xma, /**< memory allocator */
	void*      b    /**< memory block */
);

/**
 * The hak_xma_dump() function dumps the contents of the memory zone
 * with the output function @a dumper provided. The debug build shows
 * more statistical counters.
 */
HAK_EXPORT void hak_xma_dump (
	hak_xma_t*       xma,    /**< memory allocator */
	hak_xma_dumper_t dumper, /**< output function */
	void*            ctx     /**< first parameter to output function */
);

#if defined(__cplusplus)
}
#endif

#endif
