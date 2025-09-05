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

hak_t* hak_open (hak_mmgr_t* mmgr, hak_oow_t xtnsize, const hak_vmprim_t* vmprim, hak_errnum_t* errnum)
{
	hak_t* hak;

	/* if this assertion fails, correct the type definition in hak.h */
	HAK_ASSERT(hak, HAK_SIZEOF(hak_oow_t) == HAK_SIZEOF(hak_oop_t));

	hak = (hak_t*)HAK_MMGR_ALLOC(mmgr, HAK_SIZEOF(*hak) + xtnsize);
	if (hak)
	{
		if (hak_init(hak, mmgr, vmprim) <= -1)
		{
			if (errnum) *errnum = hak->errnum;
			HAK_MMGR_FREE (mmgr, hak);
			hak = HAK_NULL;
		}
		else HAK_MEMSET(hak + 1, 0, xtnsize);
	}
	else if (errnum) *errnum = HAK_ESYSMEM;

	return hak;
}

void hak_close (hak_t* hak)
{
	hak_fini (hak);
	HAK_MMGR_FREE (HAK_MMGR(hak), hak);
}

void* hak_getxtn (hak_t* hak)
{
	return (void*)((hak_uint8_t*)hak + hak->_instsize);
}

hak_mmgr_t* hak_getmmgr (hak_t* hak)
{
	return hak->_mmgr;
}

hak_cmgr_t* hak_getcmgr (hak_t* hak)
{
	return hak->_cmgr;
}
void hak_setcmgr (hak_t* hak, hak_cmgr_t* cmgr)
{
	hak->_cmgr = cmgr;
}

static void fill_bigint_tables (hak_t* hak)
{
	int radix, safe_ndigits;
	hak_oow_t ub, w;
	hak_oow_t multiplier;

	for (radix = 2; radix <= 36; radix++)
	{
		w = 0;
		ub = (hak_oow_t)HAK_TYPE_MAX(hak_liw_t) / radix - (radix - 1);
		multiplier = 1;
		safe_ndigits = 0;

		while (w <= ub)
		{
			w = w * radix + (radix - 1);
			multiplier *= radix;
			safe_ndigits++;
		}

		/* safe_ndigits contains the number of digits that never
		 * cause overflow when computed normally with a native type. */
		hak->bigint[radix].safe_ndigits = safe_ndigits;
		hak->bigint[radix].multiplier = multiplier;
	}
}

static void* alloc_heap (hak_t* hak, hak_oow_t* size)
{
	return hak_allocmem(hak, *size);
}

static void free_heap (hak_t* hak, void* ptr)
{
	hak_freemem(hak, ptr);
}

int hak_init (hak_t* hak, hak_mmgr_t* mmgr, const hak_vmprim_t* vmprim)
{
	int modtab_inited = 0;
	int n;

	if (!vmprim->syserrstrb && !vmprim->syserrstru)
	{
		hak_seterrnum(hak, HAK_EINVAL);
		return -1;
	}

#if !defined(HAK_BUILD_RELEASE)
	if (!vmprim->assertfail)
	{
		hak_seterrnum(hak, HAK_EINVAL);
		return -1;
	}
#endif

	HAK_MEMSET(hak, 0, HAK_SIZEOF(*hak));
	hak->_instsize = HAK_SIZEOF(*hak);
	hak->_mmgr = mmgr;
	hak->_cmgr = hak_get_utf8_cmgr();
	hak->vmprim = *vmprim;
	if (!hak->vmprim.alloc_heap) hak->vmprim.alloc_heap = alloc_heap;
	if (!hak->vmprim.free_heap) hak->vmprim.free_heap = free_heap;

	/*hak->option.log_mask = HAK_LOG_ALL_LEVELS | HAK_LOG_ALL_TYPES;*/
	hak->option.log_mask = (hak_bitmask_t)0; /* log nothing by default */
	hak->option.log_maxcapa = HAK_DFL_LOG_MAXCAPA;
	hak->option.dfl_symtab_size = HAK_DFL_SYMTAB_SIZE;
	hak->option.dfl_sysdic_size = HAK_DFL_SYSDIC_SIZE;
	hak->option.dfl_procstk_size = HAK_DFL_PROCSTK_SIZE;
#if defined(HAK_BUILD_DEBUG)
	hak->option.karatsuba_cutoff = HAK_KARATSUBA_CUTOFF; /* this won't be used when NDEBUG is set */
#endif

	hak->log.capa = HAK_ALIGN_POW2(1, HAK_LOG_CAPA_ALIGN); /* TODO: is this a good initial size? */
	/* alloate the log buffer in advance though it may get reallocated
	 * in put_oocs and put_ooch in logfmt.c. this is to let the logging
	 * routine still function despite some side-effects when
	 * reallocation fails */
	/* +1 required for consistency with put_oocs and put_ooch in logfmt.c */
	hak->log.ptr = (hak_ooch_t*)hak_allocmem(hak, (hak->log.capa + 1) * HAK_SIZEOF(*hak->log.ptr));
	if (HAK_UNLIKELY(!hak->log.ptr)) goto oops;

	hak->gci.stack.capa = HAK_ALIGN_POW2(1, 1024); /* TODO: is this a good initial size? */
	hak->gci.stack.ptr = (hak_oop_t*)hak_allocmem(hak, (hak->gci.stack.capa + 1) * HAK_SIZEOF(*hak->gci.stack.ptr));
	if (HAK_UNLIKELY(!hak->gci.stack.ptr)) goto oops;

	n = hak_rbt_init(&hak->modtab, hak, HAK_SIZEOF(hak_ooch_t), 1);
	if (HAK_UNLIKELY(n <= -1)) goto oops;
	modtab_inited = 1;
	hak_rbt_setstyle(&hak->modtab, hak_get_rbt_style(HAK_RBT_STYLE_INLINE_COPIERS));

	fill_bigint_tables (hak);

	hak->tagged_brands[HAK_OOP_TAG_SMOOI] = HAK_BRAND_SMOOI;
	hak->tagged_brands[HAK_OOP_TAG_SMPTR] = HAK_BRAND_SMPTR;
	hak->tagged_brands[HAK_OOP_TAG_CHAR] = HAK_BRAND_CHARACTER;
	hak->tagged_brands[HAK_OOP_TAG_ERROR] = HAK_BRAND_ERROR;

	hak->tagged_classes[HAK_OOP_TAG_SMOOI] = &hak->c_small_integer;
	hak->tagged_classes[HAK_OOP_TAG_SMPTR] = &hak->c_small_pointer;
	hak->tagged_classes[HAK_OOP_TAG_CHAR] = &hak->c_character;
	hak->tagged_classes[HAK_OOP_TAG_ERROR] = &hak->c_error;

	hak->proc_map_free_first = -1;
	hak->proc_map_free_last = -1;

	 /* hak_execute() resets 'sp' to -1 when it initializes the initial context.
	  * set it to -1 here in case hak_gc() is called before a call to hak_execute() */
	hak->sp = -1;
	hak->ip = 0;

	if (hak->vmprim.dl_startup) hak->vmprim.dl_startup (hak);
	return 0;

oops:
	if (modtab_inited) hak_rbt_fini(&hak->modtab);
	if (hak->gci.stack.ptr)
	{
		hak_freemem(hak, hak->gci.stack.ptr);
		hak->gci.stack.capa = 0;
	}
	if (hak->log.ptr) hak_freemem(hak, hak->log.ptr);
	hak->log.capa = 0;
	return -1;
}

static hak_rbt_walk_t unload_module (hak_rbt_t* rbt, hak_rbt_pair_t* pair, void* ctx)
{
	hak_t* hak = (hak_t*)ctx;
	hak_mod_data_t* mdp;

	mdp = (hak_mod_data_t*)HAK_RBT_VPTR(pair);
	HAK_ASSERT(hak, mdp != HAK_NULL);

	mdp->pair = HAK_NULL; /* to prevent hak_closemod() from calling  hak_rbt_delete() */
	hak_closemod(hak, mdp);

	return HAK_RBT_WALK_FORWARD;
}

void hak_fini (hak_t* hak)
{
	hak_cb_t* cb;
	hak_oow_t i;

	hak_rbt_walk(&hak->modtab, unload_module, hak);
	hak_rbt_fini(&hak->modtab);

	if (hak->log.len > 0)
	{
		/* flush pending log messages just in case. */
		HAK_ASSERT(hak, hak->log.ptr != HAK_NULL);
		HAK_VMPRIM_LOG_WRITE(hak, hak->log.last_mask, hak->log.ptr, hak->log.len);
	}

	for (cb = hak->cblist; cb; cb = cb->next)
	{
		if (cb->on_fini) cb->on_fini (hak);
	}

	if (hak->log.len > 0)
	{
		/* flush pending log message that could be generated by the fini
		 * callbacks. however, the actual logging might not be produced at
		 * this point because one of the callbacks could arrange to stop
		 * logging */
		HAK_ASSERT(hak, hak->log.ptr != HAK_NULL);
		HAK_VMPRIM_LOG_WRITE(hak, hak->log.last_mask, hak->log.ptr, hak->log.len);
	}

	/* deregister all callbacks */
	while (hak->cblist) hak_deregcb(hak, hak->cblist);

	/* detach the user data io handlers just in case */
	hak_detachudio (hak);

	if (hak->sem_list)
	{
		hak_freemem(hak, hak->sem_list);
		hak->sem_list_capa = 0;
		hak->sem_list_count = 0;
	}

	if (hak->sem_heap)
	{
		hak_freemem(hak, hak->sem_heap);
		hak->sem_heap_capa = 0;
		hak->sem_heap_count = 0;
	}

	if (hak->sem_io_tuple)
	{
		hak_freemem(hak, hak->sem_io_tuple);
		hak->sem_io_tuple_capa = 0;
		hak->sem_io_tuple_count = 0;
	}

	if (hak->sem_io_map)
	{
		hak_freemem(hak, hak->sem_io_map);
		hak->sem_io_map_capa = 0;
	}

	if (hak->proc_map)
	{
		hak_freemem(hak, hak->proc_map);
		hak->proc_map_capa = 0;
		hak->proc_map_used = 0;
		hak->proc_map_free_first = -1;
		hak->proc_map_free_last = -1;
	}

	hak_purgecode(hak, &hak->code);

	if (hak->p.s.ptr)
	{
		hak_freemem(hak, hak->p.s.ptr);
		hak->p.s.ptr= HAK_NULL;
		hak->p.s.capa = 0;
		hak->p.s.size = 0;
	}

	if (hak->gci.b)
	{
		hak_gchdr_t* next;
		do
		{
			next = hak->gci.b->next;
			hak->gci.bsz -= HAK_SIZEOF(hak_obj_t) + hak_getobjpayloadbytes(hak, (hak_oop_t)(hak->gci.b + 1));
			hak_freeheapmem(hak, hak->heap, hak->gci.b);
			hak->gci.b = next;
		}
		while (hak->gci.b);

		HAK_ASSERT(hak, hak->gci.bsz == 0);
	}

	if (hak->gci.stack.ptr)
	{
		hak_freemem(hak, hak->gci.stack.ptr);
		hak->gci.stack.ptr = 0;
		hak->gci.stack.capa = 0;
		hak->gci.stack.len = 0;
	}

	if (hak->heap) hak_killheap(hak, hak->heap);

	if (hak->log.ptr)
	{
		hak_freemem(hak, hak->log.ptr);
		hak->log.capa = 0;
		hak->log.len = 0;
	}

	if (hak->option.log_target_u)
	{
		hak_freemem(hak, hak->option.log_target_u);
		hak->option.log_target_u = HAK_NULL;
	}

	if (hak->option.log_target_b)
	{
		hak_freemem(hak, hak->option.log_target_b);
		hak->option.log_target_b = HAK_NULL;
	}

	for (i = 0; i < HAK_COUNTOF(hak->option.mod); i++)
	{
		if (hak->option.mod[i].ptr) hak_freemem(hak, hak->option.mod[i].ptr);
	}

	if (hak->inttostr.xbuf.ptr)
	{
		hak_freemem(hak, hak->inttostr.xbuf.ptr);
		hak->inttostr.xbuf.ptr = HAK_NULL;
		hak->inttostr.xbuf.capa = 0;
		hak->inttostr.xbuf.len = 0;
	}

	if (hak->inttostr.t.ptr)
	{
		hak_freemem(hak, hak->inttostr.t.ptr);
		hak->inttostr.t.ptr = HAK_NULL;
		hak->inttostr.t.capa = 0;
	}

	if (hak->sprintf.xbuf.ptr)
	{
		hak_freemem(hak, hak->sprintf.xbuf.ptr);
		hak->sprintf.xbuf.ptr = HAK_NULL;
		hak->sprintf.xbuf.capa = 0;
		hak->sprintf.xbuf.len = 0;
	}

	if (hak->vmprim.dl_cleanup) hak->vmprim.dl_cleanup (hak);
}

void hak_resetcode (hak_t* hak)
{
	hak_oop_t v;
	hak_oow_t i;

	/* delete all literals shown in the literal frame from the system dictionary
	 * excluding special kernel symbols. */
	for (i = 0; i < hak->code.lit.len; i++)
	{
		v = ((hak_oop_oop_t)hak->code.lit.arr)->slot[i];
		if (HAK_IS_CONS(hak, v))
		{
			hak_oop_t key = HAK_CONS_CAR(v);
			if (!HAK_IS_SYMBOL(hak,key) || !HAK_OBJ_GET_FLAGS_KERNEL(key))
				hak_zapatsysdic(hak, HAK_CONS_CAR(v));
		}
	}

	/* zap the byte code buffer and the literal frame */
	hak->code.bc.len = 0;
	hak->code.lit.len = 0;

	/* clean up object memory */
	hak_gc(hak, 1);
}

void hak_clearcode (hak_t* hak)
{
	/* clear the code buffer and the literal frame only */
	hak->code.bc.len = 0;
	hak->code.lit.len = 0;
}

static int dup_str_opt (hak_t* hak, const hak_ooch_t* value, hak_oocs_t* tmp)
{
	if (value)
	{
		tmp->ptr = hak_dupoocstr(hak, value, &tmp->len);
		if (HAK_UNLIKELY(!tmp->ptr)) return -1;
	}
	else
	{
		tmp->ptr = HAK_NULL;
		tmp->len = 0;
	}

	return 0;
}

int hak_setoption (hak_t* hak, hak_option_t id, const void* value)
{
	hak_cb_t* cb;

	switch (id)
	{
		case HAK_TRAIT:
			hak->option.trait = *(const hak_bitmask_t*)value;
		#if defined(HAK_BUILD_DEBUG)
			hak->option.karatsuba_cutoff = ((hak->option.trait & HAK_TRAIT_DEBUG_BIGINT)? HAK_KARATSUBA_CUTOFF_DEBUG: HAK_KARATSUBA_CUTOFF);
		#endif
			break;

		case HAK_LOG_MASK:
			hak->option.log_mask = *(const hak_bitmask_t*)value;
			break;

		case HAK_LOG_MAXCAPA:
			hak->option.log_maxcapa = *(hak_oow_t*)value;
			break;


		case HAK_LOG_TARGET_BCSTR:
		{
			hak_bch_t* v1;
			hak_uch_t* v2;

			v1 = hak_dupbcstr(hak, (const hak_bch_t*)value, HAK_NULL);
			if (HAK_UNLIKELY(!v1)) return -1;

			v2 = hak_dupbtoucstr(hak, (const hak_bch_t*)value, HAK_NULL);
			if (HAK_UNLIKELY(!v2))
			{
				hak_freemem(hak, v1);
				return -1;
			}

			hak->option.log_target_u = v2;
			hak->option.log_target_b = v1;
			break;
		}

		case HAK_LOG_TARGET_UCSTR:
		{
			hak_uch_t* v1;
			hak_bch_t* v2;

			v1 = hak_dupucstr(hak, (const hak_uch_t*)value, HAK_NULL);
			if (HAK_UNLIKELY(!v1)) return -1;

			v2 = hak_duputobcstr(hak, (const hak_uch_t*)value, HAK_NULL);
			if (HAK_UNLIKELY(!v2))
			{
				hak_freemem(hak, v1);
				return -1;
			}

			hak->option.log_target_u = v1;
			hak->option.log_target_b = v2;
			break;
		}

		case HAK_LOG_TARGET_BCS:
		{
			hak_bch_t* v1;
			hak_uch_t* v2;
			hak_bcs_t* v = (hak_bcs_t*)value;

			v1 = hak_dupbchars(hak, v->ptr, v->len);
			if (HAK_UNLIKELY(!v1)) return -1;

			v2 = hak_dupbtouchars(hak, v->ptr, v->len, HAK_NULL);
			if (HAK_UNLIKELY(!v2))
			{
				hak_freemem(hak, v1);
				return -1;
			}

			hak->option.log_target_u = v2;
			hak->option.log_target_b = v1;
			break;
		}

		case HAK_LOG_TARGET_UCS:
		{
			hak_uch_t* v1;
			hak_bch_t* v2;
			hak_ucs_t* v = (hak_ucs_t*)value;

			v1 = hak_dupuchars(hak, v->ptr, v->len);
			if (HAK_UNLIKELY(!v1)) return -1;

			v2 = hak_duputobchars(hak, v->ptr, v->len, HAK_NULL);
			if (HAK_UNLIKELY(!v2))
			{
				hak_freemem(hak, v1);
				return -1;
			}

			hak->option.log_target_u = v1;
			hak->option.log_target_b = v2;
			break;
		}

		case HAK_SYMTAB_SIZE:
		{
			hak_oow_t w;

			w = *(hak_oow_t*)value;
			if (w <= 0 || w > HAK_SMOOI_MAX) goto einval;

			hak->option.dfl_symtab_size = *(hak_oow_t*)value;
			break;
		}

		case HAK_SYSDIC_SIZE:
		{
			hak_oow_t w;

			w = *(hak_oow_t*)value;
			if (w <= 0 || w > HAK_SMOOI_MAX) goto einval;

			hak->option.dfl_sysdic_size = *(hak_oow_t*)value;
			break;
		}

		case HAK_PROCSTK_SIZE:
		{
			hak_oow_t w;

			w = *(hak_oow_t*)value;
			if (w <= 0 || w > HAK_SMOOI_MAX) goto einval;

			hak->option.dfl_procstk_size = *(hak_oow_t*)value;
			break;
		}

		case HAK_MOD_LIBDIRS:
		case HAK_MOD_PREFIX:
		case HAK_MOD_POSTFIX:
		{
			hak_oocs_t tmp;
			int idx;

			if (dup_str_opt(hak, (const hak_ooch_t*)value, &tmp) <= -1) return -1;

			idx = id - HAK_MOD_LIBDIRS;
			if (hak->option.mod[idx].ptr) hak_freemem(hak, hak->option.mod[idx].ptr);

			hak->option.mod[idx] = tmp;
			return 0;
		}

		case HAK_MOD_INCTX:
			hak->option.mod_inctx = *(void**)value;
			break;

		default:
			goto einval;
	}

	for (cb = hak->cblist; cb; cb = cb->next)
	{
		if (cb->on_option) cb->on_option(hak, id, value);
	}

	return 0;

einval:
	hak_seterrnum(hak, HAK_EINVAL);
	return -1;
}

int hak_getoption (hak_t* hak, hak_option_t id, void* value)
{
	switch  (id)
	{
		case HAK_TRAIT:
			*(hak_bitmask_t*)value = hak->option.trait;
			return 0;

		case HAK_LOG_MASK:
			*(hak_bitmask_t*)value = hak->option.log_mask;
			return 0;

		case HAK_LOG_MAXCAPA:
			*(hak_oow_t*)value = hak->option.log_maxcapa;
			return 0;

		case HAK_LOG_TARGET_BCSTR:
			*(hak_bch_t**)value = hak->option.log_target_b;
			return 0;

		case HAK_LOG_TARGET_UCSTR:
			*(hak_uch_t**)value = hak->option.log_target_u;
			return 0;

		case HAK_LOG_TARGET_BCS:
			((hak_bcs_t*)value)->ptr = hak->option.log_target_b;
			((hak_bcs_t*)value)->len = hak_count_bcstr(hak->option.log_target_b);
			return 0;

		case HAK_LOG_TARGET_UCS:
			((hak_ucs_t*)value)->ptr = hak->option.log_target_u;
			((hak_ucs_t*)value)->len = hak_count_ucstr(hak->option.log_target_u);
			return 0;

		case HAK_SYMTAB_SIZE:
			*(hak_oow_t*)value = hak->option.dfl_symtab_size;
			return 0;

		case HAK_SYSDIC_SIZE:
			*(hak_oow_t*)value = hak->option.dfl_sysdic_size;
			return 0;

		case HAK_PROCSTK_SIZE:
			*(hak_oow_t*)value = hak->option.dfl_procstk_size;
			return 0;

		case HAK_MOD_LIBDIRS:
		case HAK_MOD_PREFIX:
		case HAK_MOD_POSTFIX:
			*(const hak_ooch_t**)value = hak->option.mod[id - HAK_MOD_LIBDIRS].ptr;
			return 0;

		case HAK_MOD_INCTX:
			*(void**)value = hak->option.mod_inctx;
			return 0;
	};

	hak_seterrnum(hak, HAK_EINVAL);
	return -1;
}


hak_cb_t* hak_regcb (hak_t* hak, hak_cb_t* tmpl)
{
	hak_cb_t* actual;

	actual = (hak_cb_t*)hak_allocmem(hak, HAK_SIZEOF(*actual));
	if (!actual) return HAK_NULL;

	*actual = *tmpl;
	actual->next = hak->cblist;
	actual->prev = HAK_NULL;
	hak->cblist = actual;

	/* vm_checkbc is invoked very frequently.
	 * and there might be multiple vm_checkbc callbacks registered.
	 * keeping the count of vm_checkbc callbacks registered
	 * speeds up the check */
	if (actual->vm_checkbc) hak->vm_checkbc_cb_count++;

	return actual;
}

void hak_deregcb (hak_t* hak, hak_cb_t* cb)
{
	if (cb == hak->cblist)
	{
		hak->cblist = hak->cblist->next;
		if (hak->cblist) hak->cblist->prev = HAK_NULL;
	}
	else
	{
		if (cb->next) cb->next->prev = cb->prev;
		if (cb->prev) cb->prev->next = cb->next;
	}

	if (cb->vm_checkbc)
	{
		HAK_ASSERT(hak, hak->vm_checkbc_cb_count > 0);
		hak->vm_checkbc_cb_count--;
	}
	hak_freemem(hak, cb);
}

void* hak_allocmem (hak_t* hak, hak_oow_t size)
{
	void* ptr;

	ptr = HAK_MMGR_ALLOC(HAK_MMGR(hak), size);
	if (!ptr) hak_seterrnum(hak, HAK_ESYSMEM);
	return ptr;
}

void* hak_callocmem (hak_t* hak, hak_oow_t size)
{
	void* ptr;

	ptr = HAK_MMGR_ALLOC(HAK_MMGR(hak), size);
	if (!ptr) hak_seterrnum(hak, HAK_ESYSMEM);
	else HAK_MEMSET(ptr, 0, size);
	return ptr;
}

void* hak_reallocmem (hak_t* hak, void* ptr, hak_oow_t size)
{
	ptr = HAK_MMGR_REALLOC(HAK_MMGR(hak), ptr, size);
	if (!ptr) hak_seterrnum(hak, HAK_ESYSMEM);
	return ptr;
}

void hak_freemem (hak_t* hak, void* ptr)
{
	HAK_MMGR_FREE (HAK_MMGR(hak), ptr);
}


/* -------------------------------------------------------------------------- */

#define MOD_PREFIX "hak_mod_"
#define MOD_PREFIX_LEN 8

#if defined(HAK_ENABLE_STATIC_MODULE)

#include "../mod/_core.h"
#include "../mod/_dic.h"
#include "../mod/_sys.h"

static struct
{
	const hak_bch_t* modname;
	int (*modload) (hak_t* hak, hak_mod_t* mod);
}
static_modtab[] =
{
	{ "core",     hak_mod_core },
	{ "dic",      hak_mod_dic },
	{ "sys",      hak_mod_sys }
};
#endif

hak_mod_data_t* hak_openmod (hak_t* hak, const hak_ooch_t* name, hak_oow_t namelen)
{
	hak_rbt_pair_t* pair;
	hak_mod_data_t* mdp;
	hak_mod_data_t md;
	hak_mod_load_t load = HAK_NULL;
#if defined(HAK_ENABLE_STATIC_MODULE)
	int n;
#endif

	/* maximum module name length is HAK_MOD_NAME_LEN_MAX.
	 *   MOD_PREFIX_LEN for MOD_PREFIX
	 *   1 for _ at the end when hak_mod_xxx_ is attempted.
	 *   1 for the terminating '\0'.
	 */
	hak_ooch_t buf[MOD_PREFIX_LEN + HAK_MOD_NAME_LEN_MAX + 1 + 1];

	/* copy instead of encoding conversion. MOD_PREFIX must not
	 * include a character that requires encoding conversion.
	 * note the terminating null isn't needed in buf here. */
	hak_copy_bchars_to_oochars (buf, MOD_PREFIX, MOD_PREFIX_LEN);

	if (namelen > HAK_COUNTOF(buf) - (MOD_PREFIX_LEN + 1 + 1))
	{
		/* module name too long  */
		hak_seterrnum(hak, HAK_EINVAL); /* TODO: change the  error number to something more specific */
		return HAK_NULL;
	}

	hak_copy_oochars(&buf[MOD_PREFIX_LEN], name, namelen);
	buf[MOD_PREFIX_LEN + namelen] = '\0';

#if defined(HAK_ENABLE_STATIC_MODULE)
	/* attempt to find a statically linked module */

	/* TODO: binary search ... */
	for (n = 0; n < HAK_COUNTOF(static_modtab); n++)
	{
		if (hak_comp_oochars_bcstr(name, namelen, static_modtab[n].modname) == 0)
		{
			load = static_modtab[n].modload;
			break;
		}
	}

	if (load)
	{
		/* found the module in the static module table */

		HAK_MEMSET(&md, 0, HAK_SIZEOF(md));
		md.mod.inctx = hak->option.mod_inctx;
		hak_copy_oochars ((hak_ooch_t*)md.mod.name, name, namelen);
		/* Note md.handle is HAK_NULL for a static module */

		/* i copy-insert 'md' into the table before calling 'load'.
		 * to pass the same address to load(), query(), etc */
		pair = hak_rbt_insert(&hak->modtab, (hak_ooch_t*)name, namelen, &md, HAK_SIZEOF(md));
		if (HAK_UNLIKELY(!pair))
		{
			hak_seterrbfmt(hak, HAK_ESYSMEM, "insufficient system memory in storing static module handle");
			return HAK_NULL;
		}

		mdp = (hak_mod_data_t*)HAK_RBT_VPTR(pair);
		if (load(hak, &mdp->mod) <= -1)
		{
			hak_rbt_delete(&hak->modtab, (hak_ooch_t*)name, namelen);
			return HAK_NULL;
		}

		mdp->pair = pair;

		HAK_DEBUG1(hak, "Opened a static module [%js]\n", mdp->mod.name);
		return mdp;
	}
	else
	{
	#if !defined(HAK_ENABLE_DYNAMIC_MODULE)
		HAK_DEBUG2(hak, "Cannot find a static module [%.*js]\n", namelen, name);
		hak_seterrbfmt(hak, HAK_ENOENT, "unable to find a static module [%.*js]", namelen, name);
		return HAK_NULL;
	#endif
	}
#endif

#if !defined(HAK_ENABLE_DYNAMIC_MODULE)
	HAK_DEBUG2(hak, "Cannot open module [%.*js] - module loading disabled\n", namelen, name);
	hak_seterrbfmt(hak, HAK_ENOIMPL, "unable to open module [%.*js] - module loading disabled", namelen, name);
	return HAK_NULL;
#endif

	/* attempt to find a dynamic external module */
	HAK_MEMSET(&md, 0, HAK_SIZEOF(md));
	md.mod.inctx = hak->option.mod_inctx;
	hak_copy_oochars((hak_ooch_t*)md.mod.name, name, namelen);
	if (hak->vmprim.dl_open && hak->vmprim.dl_getsym && hak->vmprim.dl_close)
	{
		md.handle = hak->vmprim.dl_open(hak, &buf[MOD_PREFIX_LEN], HAK_VMPRIM_DLOPEN_PFMOD);
	}

	if (md.handle == HAK_NULL)
	{
		HAK_DEBUG2(hak, "Cannot open a module [%.*js]\n", namelen, name);
		hak_seterrbfmt(hak, HAK_ENOENT, "unable to open a module [%.*js]", namelen, name);
		return HAK_NULL;
	}

	/* attempt to get hak_mod_xxx where xxx is the module name*/
	load = (hak_mod_load_t)hak->vmprim.dl_getsym(hak, md.handle, buf);
	if (!load)
	{
		hak_seterrbfmt(hak, HAK_ERRNUM(hak), "unable to get module symbol [%js] in [%.*js]", buf, namelen, name);
		HAK_DEBUG3(hak, "Cannot get a module symbol [%js] in [%.*js]\n", buf, namelen, name);
		hak->vmprim.dl_close(hak, md.handle);
		return HAK_NULL;
	}

	/* i copy-insert 'md' into the table before calling 'load'.
	 * to pass the same address to load(), query(), etc */
	pair = hak_rbt_insert(&hak->modtab, (void*)name, namelen, &md, HAK_SIZEOF(md));
	if (pair == HAK_NULL)
	{
		HAK_DEBUG2(hak, "Cannot register a module [%.*js]\n", namelen, name);
		hak_seterrbfmt(hak, HAK_ESYSMEM, "unable to register a module [%.*js] for memory shortage", namelen, name);
		hak->vmprim.dl_close(hak, md.handle);
		return HAK_NULL;
	}

	mdp = (hak_mod_data_t*)HAK_RBT_VPTR(pair);
	if (load(hak, &mdp->mod) <= -1)
	{
		const hak_ooch_t* oldmsg = hak_backuperrmsg (hak);
		hak_seterrbfmt(hak, HAK_ERRNUM(hak), "module initializer [%js] returned failure in [%.*js] - %js", buf, namelen, name, oldmsg);
		HAK_DEBUG3(hak, "Module function [%js] returned failure in [%.*js]\n", buf, namelen, name);
		hak_rbt_delete(&hak->modtab, name, namelen);
		hak->vmprim.dl_close(hak, mdp->handle);
		return HAK_NULL;
	}

	mdp->pair = pair;

	HAK_DEBUG2(hak, "Opened a module [%js] - %p\n", mdp->mod.name, mdp->handle);

	/* the module loader must ensure to set a proper query handler */
	HAK_ASSERT(hak, mdp->mod.query != HAK_NULL);

	return mdp;
}

void hak_closemod (hak_t* hak, hak_mod_data_t* mdp)
{
	if (mdp->mod.unload) mdp->mod.unload(hak, &mdp->mod);

	if (mdp->handle)
	{
		hak->vmprim.dl_close(hak, mdp->handle);
		HAK_DEBUG2(hak, "Closed a module [%js] - %p\n", mdp->mod.name, mdp->handle);
		mdp->handle = HAK_NULL;
	}
	else
	{
		HAK_DEBUG1(hak, "Closed a static module [%js]\n", mdp->mod.name);
	}

	if (mdp->pair)
	{
		/*mdp->pair = HAK_NULL;*/ /* this reset isn't needed as the area will get freed by hak_rbt_delete()) */
		hak_rbt_delete(&hak->modtab, mdp->mod.name, hak_count_oocstr(mdp->mod.name));
	}
}

hak_pfbase_t* hak_querymod (hak_t* hak, const hak_ooch_t* pfid, hak_oow_t pfidlen, hak_mod_t** mod)
{
	/* primitive function identifier
	 *   _funcname
	 *   modname_funcname
	 */
	hak_rbt_pair_t* pair;
	hak_mod_data_t* mdp;
	const hak_ooch_t* sep;

	hak_oow_t mod_name_len;
	hak_pfbase_t* pfbase;

	sep = hak_rfind_oochar(pfid, pfidlen, '.');
	if (!sep)
	{
		/* i'm writing a conservative code here. the compiler should
		 * guarantee that a period is included in an primitive function identifer.
		 * what if the compiler is broken? imagine a buggy compiler rewritten
		 * in hak itself? */
		HAK_DEBUG2(hak, "Internal error - no period in a primitive function identifier [%.*js] - buggy compiler?\n", pfidlen, pfid);
		hak_seterrbfmt(hak, HAK_EINTERN, "no period in a primitive function identifier [%.*js]", pfidlen, pfid);
		return HAK_NULL;
	}

	mod_name_len = sep - pfid;

	/* the first segment through the segment before the last compose a
	 * module id. the last segment is the primitive function name.
	 * for instance, in con.window.open, con.window is a module id and
	 * open is the primitive function name. */
	pair = hak_rbt_search(&hak->modtab, pfid, mod_name_len);
	if (pair)
	{
		mdp = (hak_mod_data_t*)HAK_RBT_VPTR(pair);
		HAK_ASSERT(hak, mdp != HAK_NULL);
	}
	else
	{
		/* open a module using the part before the last period */
		mdp = hak_openmod(hak, pfid, mod_name_len);
		if (!mdp) return HAK_NULL;
	}

	if ((pfbase = mdp->mod.query(hak, &mdp->mod, sep + 1, pfidlen - mod_name_len - 1)) == HAK_NULL)
	{
		/* the primitive function is not found. but keep the module open even if it's opened above */
		HAK_DEBUG3(hak, "Cannot find a primitive function [%.*js] in a module [%js]\n", pfidlen - mod_name_len - 1, sep + 1, mdp->mod.name);
		hak_seterrbfmt(hak, HAK_ENOENT, "unable to find a primitive function [%.*js] in a module [%js]", pfidlen - mod_name_len - 1, sep + 1, mdp->mod.name);
		return HAK_NULL;
	}

	*mod = &mdp->mod;

	HAK_DEBUG4(hak, "Found a primitive function [%.*js] in a module [%js] - %p\n", pfidlen - mod_name_len - 1, sep + 1, mdp->mod.name, pfbase);
	return pfbase;
}

hak_pfbase_t* hak_findpfbase (hak_t* hak, hak_pfinfo_t* pfinfo, hak_oow_t pfcount, const hak_ooch_t* name, hak_oow_t namelen)
{
	int n;

	/* binary search */
#if 0
	/* [NOTE] this algorithm is NOT underflow safe with hak_oow_t types */
	int left, right, mid;

	for (left = 0, right = pfcount - 1; left <= right; )
	{
		/*mid = (left + right) / 2;*/
		mid = left + ((right - left) / 2);

		n = hak_comp_oochars_bcstr(name, namelen, pfinfo[mid].mthname);
		if (n < 0) right = mid - 1; /* this substraction can make right negative. so i can't use hak_oow_t for the variable */
		else if (n > 0) left = mid + 1;
		else return &pfinfo[mid].base;
	}
#else
	/* [NOTE] this algorithm is underflow safe with hak_oow_t types */
	hak_oow_t base, mid, lim;

	for (base = 0, lim = pfcount; lim > 0; lim >>= 1)
	{
		mid = base + (lim >> 1);
		n = hak_comp_oochars_bcstr(name, namelen, pfinfo[mid].mthname);
		if (n == 0) return &pfinfo[mid].base;
		if (n > 0) { base = mid + 1; lim--; }
	}
#endif

	hak_seterrnum(hak, HAK_ENOENT);
	return HAK_NULL;
}

