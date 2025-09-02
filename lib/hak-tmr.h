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

#ifndef _HAK_TMR_H_
#define _HAK_TMR_H_

#include <hak-cmn.h>

typedef struct hak_tmr_t hak_tmr_t;
typedef struct hak_tmr_event_t hak_tmr_event_t;
typedef hak_oow_t hak_tmr_index_t;

typedef void (*hak_tmr_handler_t) (
	hak_tmr_t*         tmr,
	const hak_ntime_t* now,
	hak_tmr_event_t*   evt
);

typedef void (*hak_tmr_updater_t) (
	hak_tmr_t*        tmr,
	hak_tmr_index_t   old_index,
	hak_tmr_index_t   new_index,
	hak_tmr_event_t*  evt
);

struct hak_tmr_t
{
	hak_t*           hak;
	hak_oow_t        capa;
	hak_oow_t        size;
	hak_tmr_event_t* event;
};

struct hak_tmr_event_t
{
	void*              ctx;    /* primary context pointer */
	hak_ntime_t        when;
	hak_tmr_handler_t  handler;
	hak_tmr_updater_t  updater;
};

#define HAK_TMR_INVALID_INDEX ((hak_tmr_index_t)-1)

#define HAK_TMR_SIZE(tmr) ((tmr)->size)
#define HAK_TMR_CAPA(tmr) ((tmr)->capa);

#if defined(__cplusplus)
extern "C" {
#endif

HAK_EXPORT hak_tmr_t* hak_tmr_open (
	hak_t*     mmgr,
	hak_oow_t  xtnsize,
	hak_oow_t  capa
);

HAK_EXPORT void hak_tmr_close (
	hak_tmr_t* tmr
);

HAK_EXPORT int hak_tmr_init (
	hak_tmr_t*  tmr,
	hak_t*      mmgr,
	hak_oow_t   capa
);

HAK_EXPORT void hak_tmr_fini (
	hak_tmr_t* tmr
);

/*
HAK_EXPORT hak_mmgr_t* hak_tmr_getmmgr (
	hak_tmr_t* tmr
);*/

HAK_EXPORT void* hak_tmr_getxtn (
	hak_tmr_t* tmr
);

HAK_EXPORT void hak_tmr_clear (
	hak_tmr_t* tmr
);

/**
 * The hak_tmr_insert() function schedules a new event.
 *
 * \return #HAK_TMR_INVALID_INDEX on failure, valid index on success.
 */

HAK_EXPORT hak_tmr_index_t hak_tmr_insert (
	hak_tmr_t*             tmr,
	const hak_tmr_event_t* event
);

HAK_EXPORT hak_tmr_index_t hak_tmr_update (
	hak_tmr_t*             tmr,
	hak_tmr_index_t        index,
	const hak_tmr_event_t* event
);

HAK_EXPORT void hak_tmr_delete (
	hak_tmr_t*      tmr,
	hak_tmr_index_t index
);

HAK_EXPORT int hak_tmr_fire (
	hak_tmr_t*         tmr,
	const hak_ntime_t* tm,
	hak_oow_t*         firecnt
);

HAK_EXPORT int hak_tmr_gettmout (
	hak_tmr_t*         tmr,
	const hak_ntime_t* tm,
	hak_ntime_t*       tmout
);

/**
 * The hak_tmr_getevent() function returns the
 * pointer to the registered event at the given index.
 */
HAK_EXPORT hak_tmr_event_t* hak_tmr_getevent (
	hak_tmr_t*        tmr,
	hak_tmr_index_t   index
);

#if defined(__cplusplus)
}
#endif

#endif
