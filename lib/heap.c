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

static void* xma_alloc (hak_mmgr_t* mmgr, hak_oow_t size)
{
	return hak_xma_alloc((hak_xma_t*)mmgr->ctx, size);
}

static void* xma_realloc (hak_mmgr_t* mmgr, void* ptr, hak_oow_t size)
{
	return hak_xma_realloc((hak_xma_t*)mmgr->ctx, ptr, size);
}

static void xma_free (hak_mmgr_t* mmgr, void* ptr)
{
	hak_xma_free((hak_xma_t*)mmgr->ctx, ptr);
}

hak_heap_t* hak_makeheap (hak_t* hak, hak_oow_t size)
{
	hak_heap_t* heap;
	hak_oow_t alloc_size;

	if (size <= HAK_SIZEOF(*heap)) /* 0 or smaller than the heap header */
	{
		/* make a zero-sized heap using the default memory manager.
		 * this zero-sized heap contains only the heap header */
		size = 0;
		alloc_size = HAK_SIZEOF(*heap);
		heap = (hak_heap_t*)hak_allocmem(hak, alloc_size);
	}
	else
	{
		/* if a non-zero heap size is given, create the heap with
		 * the dedicated heap allocator which is allowed to create
		 * a bigger heap than requested  */
		alloc_size = size;
		heap = (hak_heap_t*)hak->vmprim.alloc_heap(hak, &alloc_size);
	}
	if (HAK_UNLIKELY(!heap))
	{
		const hak_ooch_t* oldmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to allocate a heap - %js", oldmsg);
		return HAK_NULL;
	}

	/* the vmprim.alloc_heap() function is allowed to create a bigger heap than the requested size.
	 * if the created heap is bigger than requested, the heap will be utilized in full. */
	HAK_ASSERT(hak, alloc_size >= HAK_SIZEOF(*heap));
	HAK_MEMSET(heap, 0, alloc_size);

	alloc_size -= HAK_SIZEOF(*heap); /* exclude the header size */
	heap->base = (hak_uint8_t*)(heap + 1);
	heap->size = alloc_size;

	if (size == 0)
	{
		/* use the existing memory allocator */
		HAK_ASSERT(hak, alloc_size == 0);
		heap->xmmgr = *HAK_MMGR(hak);
	}
	else
	{
		/* create a new memory allocator over the allocated heap */
		heap->xma = hak_xma_open(HAK_MMGR(hak), 0, heap->base, heap->size);
		if (HAK_UNLIKELY(!heap->xma))
		{
			hak->vmprim.free_heap(hak, heap);
			hak_seterrbfmt(hak, HAK_ESYSMEM, "unable to allocate a memory manager over a heap");
			return HAK_NULL;
		}

		heap->xmmgr.allocmem = xma_alloc;
		heap->xmmgr.reallocmem = xma_realloc;
		heap->xmmgr.freemem = xma_free;
		heap->xmmgr.ctx = heap->xma;
	}

	return heap;
}

void hak_killheap (hak_t* hak, hak_heap_t* heap)
{
	if (heap->size == 0)
	{
		hak_freemem(hak, heap);
	}
	else
	{
		if (heap->xma) hak_xma_close(heap->xma);
		hak->vmprim.free_heap(hak, heap);
	}
}

void* hak_callocheapmem (hak_t* hak, hak_heap_t* heap, hak_oow_t size)
{
	void* ptr;

	ptr = HAK_MMGR_ALLOC(&heap->xmmgr, size);
	if (HAK_UNLIKELY(!ptr))
	{
		HAK_DEBUG2 (hak, "Cannot callocate %zd bytes from heap - ptr %p\n", size, heap);
		hak_seterrnum(hak, HAK_EOOMEM);
	}
	else
	{
		HAK_MEMSET(ptr, 0, size);
	}
	return ptr;
}

void* hak_callocheapmem_noseterr (hak_t* hak, hak_heap_t* heap, hak_oow_t size)
{
	void* ptr;
	ptr = HAK_MMGR_ALLOC(&heap->xmmgr, size);
	if (HAK_LIKELY(ptr)) HAK_MEMSET(ptr, 0, size);
	return ptr;
}

void hak_freeheapmem (hak_t* hak, hak_heap_t* heap, void* ptr)
{
	HAK_MMGR_FREE(&heap->xmmgr, ptr);
}
