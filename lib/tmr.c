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

#include <hak-tmr.h>
#include "hak-prv.h"

#define HEAP_PARENT(x) (((x) - 1) / 2)
#define HEAP_LEFT(x)   ((x) * 2 + 1)
#define HEAP_RIGHT(x)  ((x) * 2 + 2)

#define YOUNGER_THAN(x,y) (HAK_CMP_NTIME(&(x)->when, &(y)->when) < 0)

hak_tmr_t* hak_tmr_open (hak_t* hak, hak_oow_t xtnsize, hak_oow_t capa)
{
	hak_tmr_t* tmr;

	tmr = (hak_tmr_t*)hak_allocmem(hak, HAK_SIZEOF(*tmr) + xtnsize);
	if (tmr)
	{
		if (hak_tmr_init(tmr, hak, capa) <= -1)
		{
			hak_freemem (tmr->hak, tmr);
			return HAK_NULL;
		}
		else HAK_MEMSET(tmr + 1, 0, xtnsize);
	}

	return tmr;
}

void hak_tmr_close (hak_tmr_t* tmr)
{
	hak_tmr_fini (tmr);
	hak_freemem (tmr->hak, tmr);
}

int hak_tmr_init (hak_tmr_t* tmr, hak_t* hak, hak_oow_t capa)
{
	hak_tmr_event_t* tmp;

	HAK_MEMSET(tmr, 0, HAK_SIZEOF(*tmr));

	if (capa <= 0) capa = 1;

	tmp = (hak_tmr_event_t*)hak_allocmem(hak, capa * HAK_SIZEOF(*tmp));
	if (!tmp) return -1;

	tmr->hak = hak;
	tmr->capa = capa;
	tmr->event = tmp;

	return 0;
}

void hak_tmr_fini (hak_tmr_t* tmr)
{
	hak_tmr_clear (tmr);
	if (tmr->event)
	{
		hak_freemem (tmr->hak, tmr->event);
		tmr->event = HAK_NULL;
	}
}

/*
hak_mmgr_t* hak_tmr_getmmgr (hak_tmr_t* tmr)
{
	return tmr->hak->mmgr;
}*/

void* hak_tmr_getxtn (hak_tmr_t* tmr)
{
	return (void*)(tmr + 1);
}

void hak_tmr_clear (hak_tmr_t* tmr)
{
	while (tmr->size > 0) hak_tmr_delete (tmr, 0);
}

static hak_tmr_index_t sift_up (hak_tmr_t* tmr, hak_tmr_index_t index, int notify)
{
	hak_oow_t parent;

	parent = HEAP_PARENT(index);
	if (index > 0 && YOUNGER_THAN(&tmr->event[index], &tmr->event[parent]))
	{
		hak_tmr_event_t item;
		hak_oow_t old_index;

		item = tmr->event[index];
		old_index = index;

		do
		{
			/* move down the parent to my current position */
			tmr->event[index] = tmr->event[parent];
			tmr->event[index].updater (tmr, parent, index, &tmr->event[index]);

			/* traverse up */
			index = parent;
			parent = HEAP_PARENT(parent);
		}
		while (index > 0 && YOUNGER_THAN(&item, &tmr->event[parent]));

		/* we send no notification if the item is added with hak_tmr_insert()
		 * or updated with hak_tmr_update(). the caller of these functions
		 * must rely on the return value. */
		tmr->event[index] = item;
		if (notify && index != old_index)
			tmr->event[index].updater (tmr, old_index, index, &tmr->event[index]);
	}

	return index;
}

static hak_tmr_index_t sift_down (hak_tmr_t* tmr, hak_tmr_index_t index, int notify)
{
	hak_oow_t base = tmr->size / 2;

	if (index < base) /* at least 1 child is under the 'index' position */
	{
		hak_tmr_event_t item;
		hak_oow_t old_index;

		item = tmr->event[index];
		old_index = index;

		do
		{
			hak_oow_t left, right, younger;

			left = HEAP_LEFT(index);
			right = HEAP_RIGHT(index);

			if (right < tmr->size && YOUNGER_THAN(&tmr->event[right], &tmr->event[left]))
			{
				younger = right;
			}
			else
			{
				younger = left;
			}

			if (YOUNGER_THAN(&item, &tmr->event[younger])) break;

			tmr->event[index] = tmr->event[younger];
			tmr->event[index].updater (tmr, younger, index, &tmr->event[index]);

			index = younger;
		}
		while (index < base);

		tmr->event[index] = item;
		if (notify && index != old_index)
			tmr->event[index].updater (tmr, old_index, index, &tmr->event[index]);
	}

	return index;
}

void hak_tmr_delete (hak_tmr_t* tmr, hak_tmr_index_t index)
{
	hak_tmr_event_t item;

	HAK_ASSERT(tmr->hak, index < tmr->size);

	item = tmr->event[index];
	tmr->event[index].updater (tmr, index, HAK_TMR_INVALID_INDEX, &tmr->event[index]);

	tmr->size = tmr->size - 1;
	if (tmr->size > 0 && index != tmr->size)
	{
		tmr->event[index] = tmr->event[tmr->size];
		tmr->event[index].updater (tmr, tmr->size, index, &tmr->event[index]);
		YOUNGER_THAN(&tmr->event[index], &item)? sift_up(tmr, index, 1): sift_down(tmr, index, 1);
	}
}

hak_tmr_index_t hak_tmr_insert (hak_tmr_t* tmr, const hak_tmr_event_t* event)
{
	hak_tmr_index_t index = tmr->size;

	if (index >= tmr->capa)
	{
		hak_tmr_event_t* tmp;
		hak_oow_t new_capa;

		HAK_ASSERT(tmr->hak, tmr->capa >= 1);
		new_capa = tmr->capa * 2;
		tmp = (hak_tmr_event_t*)hak_reallocmem(tmr->hak, tmr->event, new_capa * HAK_SIZEOF(*tmp));
		if (!tmp) return HAK_TMR_INVALID_INDEX;

		tmr->event = tmp;
		tmr->capa = new_capa;
	}

	HAK_ASSERT(tmr->hak, event->handler != HAK_NULL);
	HAK_ASSERT(tmr->hak, event->updater != HAK_NULL);

	tmr->size = tmr->size + 1;
	tmr->event[index] = *event;
	return sift_up(tmr, index, 0);
}

hak_tmr_index_t hak_tmr_update (hak_tmr_t* tmr, hak_oow_t index, const hak_tmr_event_t* event)
{
	hak_tmr_event_t item;

	HAK_ASSERT(tmr->hak, event->handler != HAK_NULL);
	HAK_ASSERT(tmr->hak, event->updater != HAK_NULL);

	item = tmr->event[index];
	tmr->event[index] = *event;
	return YOUNGER_THAN(event, &item)? sift_up(tmr, index, 0): sift_down(tmr, index, 0);
}

int hak_tmr_fire (hak_tmr_t* tmr, const hak_ntime_t* tm, hak_oow_t* firecnt)
{
	hak_ntime_t now;
	hak_tmr_event_t event;
	hak_oow_t fire_count = 0;

	/* if the current time is not specified, get it from the system */
	if (tm) now = *tm;
	/*else if (hak_gettime(&now) <= -1) return -1;*/
	tmr->hak->vmprim.vm_gettime (tmr->hak, &now);

	while (tmr->size > 0)
	{
		if (HAK_CMP_NTIME(&tmr->event[0].when, &now) > 0) break;

		event = tmr->event[0];
		hak_tmr_delete (tmr, 0); /* remove the registered event structure */

		fire_count++;
		event.handler (tmr, &now, &event); /* then fire the event */
	}

	if (firecnt) *firecnt = fire_count;
	return 0;
}

int hak_tmr_gettmout (hak_tmr_t* tmr, const hak_ntime_t* tm, hak_ntime_t* tmout)
{
	hak_ntime_t now;

	/* time-out can't be calculated when there's no event scheduled */
	if (tmr->size <= 0) return -1;

	/* if the current time is not specified, get it from the system */
	if (tm) now = *tm;
	/*else if (hak_gettime(&now) <= -1) return -1;*/
	tmr->hak->vmprim.vm_gettime (tmr->hak, &now);

	HAK_SUB_NTIME (tmout, &tmr->event[0].when, &now);
	if (tmout->sec < 0) HAK_CLEAR_NTIME (tmout);

	return 0;
}

hak_tmr_event_t* hak_tmr_getevent (hak_tmr_t* tmr, hak_tmr_index_t index)
{
	return (index < 0 || index >= tmr->size)? HAK_NULL: &tmr->event[index];
}
