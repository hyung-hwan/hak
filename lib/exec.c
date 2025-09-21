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

#define ENABLE_SYSCMD

static const char* io_type_str[] =
{
	"input",
	"output"
};

static HAK_INLINE const char* proc_state_to_string (int state)
{
	static const hak_bch_t* str[] =
	{
		"TERMINATED",
		"SUSPENDED",
		"RUNNABLE",
		"WAITING",
		"RUNNING"
	};

	return str[state + 1];
}

static hak_ooch_t oocstr_colon[2] = { ':', '\0' };
static hak_ooch_t oocstr_dash[2] = { '-', '\0' };
static hak_ooch_t oocstr_none[1] = { '\0' };

#define PROC_MAP_INC 64

/* TODO: adjust these max semaphore pointer buffer capacity,
 *       proably depending on the object memory size? */
#define SEM_LIST_INC 256
#define SEM_HEAP_INC 256
#define SEM_IO_TUPLE_INC 256
#define SEM_LIST_MAX (SEM_LIST_INC * 1000)
#define SEM_HEAP_MAX (SEM_HEAP_INC * 1000)
#define SEM_IO_TUPLE_MAX (SEM_IO_TUPLE_INC * 1000)
#define SEM_IO_MAP_ALIGN 1024 /* this must a power of 2 */

#define SEM_HEAP_PARENT(x) (((x) - 1) / 2)
#define SEM_HEAP_LEFT(x)   ((x) * 2 + 1)
#define SEM_HEAP_RIGHT(x)  ((x) * 2 + 2)

#define SEM_HEAP_EARLIER_THAN(stx,x,y) ( \
	(HAK_OOP_TO_SMOOI((x)->u.timed.ftime_sec) < HAK_OOP_TO_SMOOI((y)->u.timed.ftime_sec)) || \
	(HAK_OOP_TO_SMOOI((x)->u.timed.ftime_sec) == HAK_OOP_TO_SMOOI((y)->u.timed.ftime_sec) && HAK_OOP_TO_SMOOI((x)->u.timed.ftime_nsec) < HAK_OOP_TO_SMOOI((y)->u.timed.ftime_nsec)) \
)

#define LOAD_IP(hak, v_ctx) ((hak)->ip = HAK_OOP_TO_SMOOI((v_ctx)->ip))
#define STORE_IP(hak, v_ctx) ((v_ctx)->ip = HAK_SMOOI_TO_OOP((hak)->ip))

#define LOAD_SP(hak, v_ctx) ((hak)->sp = HAK_OOP_TO_SMOOI((v_ctx)->sp))
#define STORE_SP(hak, v_ctx) ((v_ctx)->sp = HAK_SMOOI_TO_OOP((hak)->sp))

#define LOAD_ACTIVE_IP(hak) LOAD_IP(hak, (hak)->active_context)
#define STORE_ACTIVE_IP(hak) STORE_IP(hak, (hak)->active_context)

#define LOAD_ACTIVE_SP(hak) LOAD_SP(hak, (hak)->processor->active)
#define STORE_ACTIVE_SP(hak) STORE_SP(hak, (hak)->processor->active)

#define SWITCH_ACTIVE_CONTEXT(hak,v_ctx) \
	do \
	{ \
		STORE_ACTIVE_IP(hak); \
		(hak)->active_context = (v_ctx); \
		(hak)->active_function = (hak)->active_context->base; \
		(hak)->active_code = HAK_FUNCTION_GET_CODE_BYTE((hak)->active_function); \
		LOAD_ACTIVE_IP(hak); \
		(hak)->processor->active->current_context = (hak)->active_context; \
	} while (0)

/*#define FETCH_BYTE_CODE(hak) ((hak)->code.bc.arr->slot[(hak)->ip++])*/
#define FETCH_BYTE_CODE(hak) ((hak)->active_code[(hak)->ip++])
#define FETCH_BYTE_CODE_TO(hak, v_oow) (v_oow = FETCH_BYTE_CODE(hak))
#if (HAK_CODE_LONG_PARAM_SIZE == 2)
#	define FETCH_PARAM_CODE_TO(hak, v_oow) \
		do { \
			v_oow = FETCH_BYTE_CODE(hak); \
			v_oow = (v_oow << 8) | FETCH_BYTE_CODE(hak); \
		} while (0)
#else
#	define FETCH_PARAM_CODE_TO(hak, v_oow) (v_oow = FETCH_BYTE_CODE(hak))
#endif


#if defined(HAK_DEBUG_VM_EXEC)
#	define LOG_MASK_INST (HAK_LOG_IC | HAK_LOG_MNEMONIC | HAK_LOG_INFO)

#	define LOG_INST_0(hak,fmt) HAK_LOG1(hak, LOG_MASK_INST, "%010zd " fmt "\n", fetched_instruction_pointer)
#	define LOG_INST_1(hak,fmt,a1) HAK_LOG2(hak, LOG_MASK_INST, "%010zd " fmt "\n",fetched_instruction_pointer, a1)
#	define LOG_INST_2(hak,fmt,a1,a2) HAK_LOG3(hak, LOG_MASK_INST, "%010zd " fmt "\n", fetched_instruction_pointer, a1, a2)
#	define LOG_INST_3(hak,fmt,a1,a2,a3) HAK_LOG4(hak, LOG_MASK_INST, "%010zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3)
#	define LOG_INST_4(hak,fmt,a1,a2,a3,a4) HAK_LOG5(hak, LOG_MASK_INST, "%010zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3, a4)
#	define LOG_INST_5(hak,fmt,a1,a2,a3,a4,a5) HAK_LOG6(hak, LOG_MASK_INST, "%010zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3, a4, a5)
#	define LOG_INST_6(hak,fmt,a1,a2,a3,a4,a5,a6) HAK_LOG7(hak, LOG_MASK_INST, "%010zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3, a4, a5, a6)
#	define LOG_INST_7(hak,fmt,a1,a2,a3,a4,a5,a6,a7) HAK_LOG8(hak, LOG_MASK_INST, "%010zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3, a4, a5, a6, a7)
#	define LOG_INST_8(hak,fmt,a1,a2,a3,a4,a5,a6,a7,a8) HAK_LOG9(hak, LOG_MASK_INST, "%010zd " fmt "\n", fetched_instruction_pointer, a1, a2, a3, a4, a5, a6, a7, a8)
#else
#	define LOG_INST_0(hak,fmt)
#	define LOG_INST_1(hak,fmt,a1)
#	define LOG_INST_2(hak,fmt,a1,a2)
#	define LOG_INST_3(hak,fmt,a1,a2,a3)
#	define LOG_INST_4(hak,fmt,a1,a2,a3,a4)
#	define LOG_INST_5(hak,fmt,a1,a2,a3,a4,a5)
#	define LOG_INST_6(hak,fmt,a1,a2,a3,a4,a5,a6)
#	define LOG_INST_7(hak,fmt,a1,a2,a3,a4,a5,a6,a7)
#	define LOG_INST_8(hak,fmt,a1,a2,a3,a4,a5,a6,a7,a8)
#endif

static int delete_sem_from_sem_io_tuple (hak_t* hak, hak_oop_semaphore_t sem, int force);
static void signal_io_semaphore (hak_t* hak, hak_ooi_t io_handle, hak_ooi_t mask);
static void terminate_all_processes (hak_t* hak);

/* ------------------------------------------------------------------------- */

#define HAK_EXSTACK_PUSH(hak, ctx_, ip_, clsp_, sp_) \
	do { \
		hak_oop_process_t ap = (hak)->processor->active; \
		hak_ooi_t exsp = HAK_OOP_TO_SMOOI(ap->exsp); \
		if (exsp >= HAK_OOP_TO_SMOOI(ap->exst) - 1) \
		{ \
			hak_seterrbfmt(hak, HAK_EOOMEM, "process exception stack overflow"); \
			(hak)->abort_req = -1; \
		} \
		exsp++; ap->slot[exsp] = (hak_oop_t)(ctx_); \
		exsp++; ap->slot[exsp] = HAK_SMOOI_TO_OOP(ip_); \
		exsp++; ap->slot[exsp] = HAK_SMOOI_TO_OOP(clsp_); \
		exsp++; ap->slot[exsp] = HAK_SMOOI_TO_OOP(sp_); \
		ap->exsp = HAK_SMOOI_TO_OOP(exsp); \
	} while (0)

#define HAK_EXSTACK_POP(hak) \
	do { \
		hak_oop_process_t ap = (hak)->processor->active; \
		hak_ooi_t exsp = HAK_OOP_TO_SMOOI(ap->exsp); \
		exsp -= 4; \
		ap->exsp = HAK_SMOOI_TO_OOP(exsp); \
	} while (0)

#define HAK_EXSTACK_POP_TO(hak, ctx_, ip_, clsp_, sp_) \
	do { \
		hak_oop_process_t ap = (hak)->processor->active; \
		hak_ooi_t exsp = HAK_OOP_TO_SMOOI(ap->exsp); \
		sp_ = HAK_OOP_TO_SMOOI(ap->slot[exsp]); exsp--; \
		clsp_ = HAK_OOP_TO_SMOOI(ap->slot[exsp]); exsp--; \
		ip_ = HAK_OOP_TO_SMOOI(ap->slot[exsp]); exsp--; \
		ctx_ = (hak_oop_context_t)ap->slot[exsp]; exsp--; \
		ap->exsp = HAK_SMOOI_TO_OOP(exsp); \
	} while (0)

#define HAK_EXSTACK_GET_ST(hak) HAK_OOP_TO_SMOOI(((hak)->processor->active)->exst)
#define HAK_EXSTACK_GET_SP(hak) HAK_OOP_TO_SMOOI(((hak)->processor->active)->exsp)

#define HAK_EXSTACK_IS_EMPTY(hak) (HAK_OOP_TO_SMOOI(((hak)->processor->active)->exsp) <= HAK_OOP_TO_SMOOI(((hak)->processor->active)->st))


/* ------------------------------------------------------------------------- */

#define HAK_CLSTACK_PUSH(hak, v) \
	do { \
		hak_oop_process_t ap = (hak)->processor->active; \
		hak_ooi_t clsp_ = HAK_OOP_TO_SMOOI(ap->clsp); \
		if (clsp_ >= HAK_OOP_TO_SMOOI(ap->clst)) \
		{ \
			hak_seterrbfmt(hak, HAK_EOOMEM, "process class stack overflow"); \
			(hak)->abort_req = -1; \
		} \
		clsp_++; ap->slot[clsp_] = (v); \
		ap->clsp = HAK_SMOOI_TO_OOP(clsp_); \
	} while (0)

#define HAK_CLSTACK_POP(hak) \
	do { \
		hak_oop_process_t ap = (hak)->processor->active; \
		hak_ooi_t clsp_ = HAK_OOP_TO_SMOOI(ap->clsp); \
		clsp_--; \
		ap->clsp = HAK_SMOOI_TO_OOP(clsp_); \
	} while (0)

#define HAK_CLSTACK_POPS(hak, count) \
	do { \
		hak_oop_process_t ap = (hak)->processor->active; \
		hak_ooi_t clsp_ = HAK_OOP_TO_SMOOI(ap->clsp); \
		clsp_ -= count; \
		ap->clsp = HAK_SMOOI_TO_OOP(clsp_); \
	} while (0)

#define HAK_CLSTACK_POP_TO(hak, v) \
	do { \
		hak_oop_process_t ap = (hak)->processor->active; \
		hak_ooi_t clsp_ = HAK_OOP_TO_SMOOI(ap->clsp); \
		v = ap->slot[clsp_]; clsp_--; \
		ap->clsp = HAK_SMOOI_TO_OOP(clsp_); \
	} while (0)

#define HAK_CLSTACK_FETCH_TOP_TO(hak, v) \
	do { \
		hak_oop_process_t ap = (hak)->processor->active; \
		hak_ooi_t clsp_ = HAK_OOP_TO_SMOOI(ap->clsp); \
		v = ap->slot[clsp_]; \
	} while (0)


#define HAK_CLSTACK_CHOP(hak, clsp_) ((hak)->processor->active->clsp = HAK_SMOOI_TO_OOP(clsp_))

#define HAK_CLSTACK_GET_ST(hak) HAK_OOP_TO_SMOOI(((hak)->processor->active)->clst)
#define HAK_CLSTACK_GET_SP(hak) HAK_OOP_TO_SMOOI(((hak)->processor->active)->clsp)

#define HAK_CLSTACK_IS_EMPTY(hak) (HAK_OOP_TO_SMOOI(((hak)->processor->active)->clsp) <= HAK_OOP_TO_SMOOI(((hak)->processor->active)->exst))

/* ------------------------------------------------------------------------- */

static HAK_INLINE int vm_startup (hak_t* hak)
{
	hak_cb_t* cb;
	hak_oow_t i;

	HAK_DEBUG1 (hak, "VM started up at IP %zd\n", hak->ip);

	for (cb = hak->cblist; cb; cb = cb->next)
	{
		if (cb->vm_startup && cb->vm_startup(hak) <= -1)
		{
			for (cb = cb->prev; cb; cb = cb->prev)
			{
				if (cb->vm_cleanup) cb->vm_cleanup(hak);
			}
			return -1;
		}
	}

	for (i = 0; i < hak->sem_io_map_capa; i++)
	{
		hak->sem_io_map[i] = -1;
	}

#if defined(ENABLE_GCFIN)
	hak->sem_gcfin = (hak_oop_semaphore_t)hak->_nil;
	hak->sem_gcfin_sigreq = 0;
#endif

	hak->vmprim.vm_gettime(hak, &hak->exec_start_time); /* raw time. no adjustment */

	return 0;
}

static void vm_cleanup (hak_t* hak)
{
	hak_cb_t* cb;
	hak_oow_t i;

	if (hak->processor->total_count != HAK_SMOOI_TO_OOP(0))
	{
		/* if there is a suspended process, your program is probably wrong */
		HAK_LOG3(hak, HAK_LOG_WARN, "Warning - non-zero number of processes upon VM clean-up - total: %zd runnable: %zd suspended: %zd\n",
			(hak_ooi_t)HAK_OOP_TO_SMOOI(hak->processor->total_count),
			(hak_ooi_t)HAK_OOP_TO_SMOOI(hak->processor->runnable.count),
			(hak_ooi_t)HAK_OOP_TO_SMOOI(hak->processor->suspended.count));

		HAK_LOG0 (hak, HAK_LOG_WARN, "Warning - terminating all residue processes\n");
		terminate_all_processes(hak);
	}

	HAK_ASSERT(hak, hak->processor->active == hak->nil_process);
	HAK_ASSERT(hak, HAK_OOP_TO_SMOOI(hak->processor->total_count) == 0);
	HAK_ASSERT(hak, HAK_OOP_TO_SMOOI(hak->processor->runnable.count) == 0);
	HAK_ASSERT(hak, HAK_OOP_TO_SMOOI(hak->processor->suspended.count) == 0);

	for (i = 0; i < hak->sem_io_map_capa;)
	{
		hak_ooi_t sem_io_index;
		if ((sem_io_index = hak->sem_io_map[i]) >= 0)
		{
			HAK_ASSERT(hak, sem_io_index < hak->sem_io_tuple_count);
			HAK_ASSERT(hak, hak->sem_io_tuple[sem_io_index].sem[HAK_SEMAPHORE_IO_TYPE_INPUT] ||
			                 hak->sem_io_tuple[sem_io_index].sem[HAK_SEMAPHORE_IO_TYPE_OUTPUT]);

			if (hak->sem_io_tuple[sem_io_index].sem[HAK_SEMAPHORE_IO_TYPE_INPUT])
			{
				delete_sem_from_sem_io_tuple(hak, hak->sem_io_tuple[sem_io_index].sem[HAK_SEMAPHORE_IO_TYPE_INPUT], 1);
			}
			if (hak->sem_io_tuple[sem_io_index].sem[HAK_SEMAPHORE_IO_TYPE_OUTPUT])
			{
				delete_sem_from_sem_io_tuple(hak, hak->sem_io_tuple[sem_io_index].sem[HAK_SEMAPHORE_IO_TYPE_OUTPUT], 1);
			}

			HAK_ASSERT(hak, hak->sem_io_map[i] <= -1);
		}
		else
		{
			i++;
		}
	}

	HAK_ASSERT(hak, hak->sem_io_tuple_count == 0);
	HAK_ASSERT(hak, hak->sem_io_count == 0);

	hak->vmprim.vm_gettime(hak, &hak->exec_end_time); /* raw time. no adjustment */
	for (cb = hak->cblist; cb; cb = cb->next)
	{
		if (cb->vm_cleanup) cb->vm_cleanup(hak);
	}

#if defined(ENABLE_GCFIN)
	hak->sem_gcfin = (hak_oop_semaphore_t)hak->_nil;
	hak->sem_gcfin_sigreq = 0;

	/* deregister all pending finalizable objects pending just in case these
	 * have not been removed for various reasons. (e.g. sudden VM abortion)
	 */
	hak_deregallfinalizables(hak);
#endif

	HAK_DEBUG0 (hak, "VM cleaned up\n");
}

static HAK_INLINE void vm_gettime (hak_t* hak, hak_ntime_t* now)
{
	hak->vmprim.vm_gettime(hak, now);
	/* in vm_startup(), hak->exec_start_time has been set to the time of
	 * that moment. time returned here get offset by hak->exec_start_time and
	 * thus becomes relative to it. this way, it is kept small such that it
	 * can be represented in a small integer with leaving almost zero chance
	 * of overflow. */
	HAK_SUB_NTIME (now, now, &hak->exec_start_time);  /* now = now - exec_start_time */
}

static HAK_INLINE int vm_sleep (hak_t* hak, const hak_ntime_t* dur)
{
/* TODO: return 1 if it gets into the halting state */
	hak->vmprim.vm_sleep(hak, dur);
	return 0;
}

static HAK_INLINE void vm_muxwait (hak_t* hak, const hak_ntime_t* dur)
{
	hak->vmprim.vm_muxwait(hak, dur, signal_io_semaphore);
}

static void vm_checkbc (hak_t* hak, hak_oob_t bcode)
{
	hak_cb_t* cb;
	for (cb = hak->cblist; cb; cb = cb->next)
	{
		if (cb->vm_checkbc) cb->vm_checkbc(hak, bcode);
	}
}

/* ------------------------------------------------------------------------- */

static HAK_INLINE hak_oop_context_t make_context (hak_t* hak, hak_ooi_t ntmprs)
{
	hak_oop_context_t ctx;
	HAK_ASSERT(hak, ntmprs >= 0);
	/*return (hak_oop_context_t)hak_allocoopobj(hak, HAK_BRAND_CONTEXT, HAK_CONTEXT_NAMED_INSTVARS + (hak_oow_t)ntmprs);*/
	ctx = (hak_oop_context_t)hak_instantiate(hak, hak->c_block_context, HAK_NULL, ntmprs);

	/* TODO: a good way to initialize smooi field to 0 in hak_insstantiate()?
	 *      for this, there must be a way to specify the type of the member variables...
	 *      it's error-prone to initialize the numeric value to nil where 0 is necessary */

	if (HAK_LIKELY(ctx)) ctx->ivaroff = HAK_SMOOI_TO_OOP(0);
	return ctx;
}

static HAK_INLINE hak_oop_function_t make_function (hak_t* hak, hak_oow_t lfsize, const hak_oob_t* bptr, hak_oow_t blen, hak_dbgi_t* dbgi)
{
	hak_oop_function_t func;

	/* the literal frame is placed in the variable part.
	 * the byte code is placed in the trailer space. */
	/*func = (hak_oop_function_t)hak_allocoopobjwithtrailer(hak, HAK_BRAND_FUNCTION, HAK_FUNCTION_NAMED_INSTVARS + lfsize, bptr, blen);*/
	func = (hak_oop_function_t)hak_instantiatewithtrailer(hak, hak->c_function, lfsize, bptr, blen);
	if (HAK_UNLIKELY(!func)) return HAK_NULL;

	if (dbgi)
	{
		hak_oop_t tmp;
		hak_pushvolat(hak, (hak_oop_t*)&func);
		tmp = hak_makebytearray(hak, (hak_oob_t*)dbgi, HAK_SIZEOF(*dbgi) * blen);
		hak_popvolat(hak);
		if (HAK_LIKELY(tmp)) func->dbgi = tmp;
	}

	func->attr_mask = HAK_SMOOI_TO_OOP(0);
	return func;
}

static HAK_INLINE void fill_function_data (hak_t* hak, hak_oop_function_t func, hak_ooi_t attr_mask, hak_oow_t name_lfindex, hak_oop_context_t homectx, const hak_oop_t* lfptr, hak_oow_t lfsize)
{
	/* Although this function could be integrated into make_function(),
	 * this function has been separated from make_function() to make GC handling simpler */
	hak_oow_t i;

	HAK_ASSERT(hak, attr_mask >= 0 && attr_mask <= HAK_SMOOI_MAX);

	/* copy literal frames */
	HAK_ASSERT(hak, lfsize <= HAK_OBJ_GET_SIZE(func) - HAK_FUNCTION_NAMED_INSTVARS);
	for (i = 0; i < lfsize; i++)
	{
		func->literal_frame[i] = lfptr[i];
	#if 0
		HAK_DEBUG2 (hak, "literal frame %d => %O\n", (int)i, lfptr[i]);
	#endif
	}

	/* initialize other fields */
	if (name_lfindex < lfsize)
	{
		/* the compiler must ensure that lfsize is less than the maximum extended long parameter value */
		func->name = hak->active_function->literal_frame[name_lfindex];
	}

	func->home = homectx;
	func->attr_mask = HAK_SMOOI_TO_OOP(attr_mask);
	func->literal_frame_size = HAK_SMOOI_TO_OOP(lfsize);
}

static HAK_INLINE hak_oop_block_t make_compiled_block (hak_t* hak)
{
	/* create a base block used for creation of a block context */
	/*return (hak_oop_block_t)hak_allocoopobj(hak, HAK_BRAND_BLOCK, HAK_BLOCK_NAMED_INSTVARS);*/
	return (hak_oop_block_t)hak_instantiate(hak, hak->c_compiled_block, HAK_NULL, 0);
}

static HAK_INLINE void fill_block_data (hak_t* hak, hak_oop_block_t blk, hak_ooi_t attr_mask, hak_oow_t name_lfindex, hak_ooi_t ip, hak_oop_context_t homectx)
{
	HAK_ASSERT(hak, attr_mask >= 0 && attr_mask <= HAK_SMOOI_MAX);
	HAK_ASSERT(hak, ip >= 0 && ip <= HAK_SMOOI_MAX);

	if (name_lfindex < HAK_OOP_TO_SMOOI(hak->active_function->literal_frame_size))
		blk->name = hak->active_function->literal_frame[name_lfindex];

	blk->home = homectx;
	blk->ip = HAK_SMOOI_TO_OOP(ip);
	blk->attr_mask = HAK_SMOOI_TO_OOP(attr_mask);
}

static HAK_INLINE int prepare_to_alloc_pid (hak_t* hak)
{
	hak_oow_t new_capa;
	hak_ooi_t i, j;
	hak_oop_t* tmp;

	HAK_ASSERT(hak, hak->proc_map_free_first <= -1);
	HAK_ASSERT(hak, hak->proc_map_free_last <= -1);

	new_capa = hak->proc_map_capa + PROC_MAP_INC;
	if (new_capa > HAK_SMOOI_MAX)
	{
		if (hak->proc_map_capa >= HAK_SMOOI_MAX)
		{
		#if defined(HAK_DEBUG_VM_PROCESSOR)
			HAK_LOG0 (hak, HAK_LOG_IC | HAK_LOG_FATAL, "Processor - too many processes\n");
		#endif
			hak_seterrbfmt(hak, HAK_EPFULL, "maximum number(%zd) of processes reached", HAK_SMOOI_MAX);
			return -1;
		}

		new_capa = HAK_SMOOI_MAX;
	}

	tmp = (hak_oop_t*)hak_reallocmem(hak, hak->proc_map, HAK_SIZEOF(hak_oop_t) * new_capa);
	if (HAK_UNLIKELY(!tmp)) return -1;

	hak->proc_map_free_first = hak->proc_map_capa;
	for (i = hak->proc_map_capa, j = hak->proc_map_capa + 1; j < new_capa; i++, j++)
	{
		tmp[i] = HAK_SMOOI_TO_OOP(j);
	}
	tmp[i] = HAK_SMOOI_TO_OOP(-1);
	hak->proc_map_free_last = i;

	hak->proc_map = tmp;
	hak->proc_map_capa = new_capa;

	return 0;
}

static HAK_INLINE void alloc_pid (hak_t* hak, hak_oop_process_t proc)
{
	hak_ooi_t pid;

	pid = hak->proc_map_free_first;
	proc->id = HAK_SMOOI_TO_OOP(pid);
	HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(hak->proc_map[pid]));
	hak->proc_map_free_first = HAK_OOP_TO_SMOOI(hak->proc_map[pid]);
	if (hak->proc_map_free_first <= -1) hak->proc_map_free_last = -1;
	hak->proc_map[pid] = (hak_oop_t)proc;
	hak->proc_map_used++;
}

static HAK_INLINE void free_pid (hak_t* hak, hak_oop_process_t proc)
{
	hak_ooi_t pid;

	pid = HAK_OOP_TO_SMOOI(proc->id);
	HAK_ASSERT(hak, pid < hak->proc_map_capa);
	HAK_ASSERT(hak, hak->proc_map_used > 0);

	hak->proc_map[pid] = HAK_SMOOI_TO_OOP(-1);
	if (hak->proc_map_free_last <= -1)
	{
		HAK_ASSERT(hak, hak->proc_map_free_first <= -1);
		hak->proc_map_free_first = pid;
	}
	else
	{
		hak->proc_map[hak->proc_map_free_last] = HAK_SMOOI_TO_OOP(pid);
	}
	hak->proc_map_free_last = pid;
	hak->proc_map_used--;
}


static hak_oop_process_t make_process (hak_t* hak, hak_oop_context_t c)
{
	hak_oop_process_t proc;
	hak_oow_t stksize, exstksize, clstksize, maxsize;
	hak_ooi_t total_count;
	hak_ooi_t suspended_count;

	total_count = HAK_OOP_TO_SMOOI(hak->processor->total_count);
	if (total_count >= HAK_SMOOI_MAX)
	{
	#if defined(HAK_DEBUG_VM_PROCESSOR)
		HAK_LOG0 (hak, HAK_LOG_IC | HAK_LOG_FATAL, "Processor - too many processes\n");
	#endif
		hak_seterrbfmt(hak, HAK_EPFULL, "maximum number(%zd) of processes reached", HAK_SMOOI_MAX);
		return HAK_NULL;
	}

	if (hak->proc_map_free_first <= -1 && prepare_to_alloc_pid(hak) <= -1) return HAK_NULL;

	stksize = hak->option.dfl_procstk_size; /* stack */
	exstksize = 128; /* exception stack size */ /* TODO: make it configurable */
	clstksize = 64; /* class stack size */ /* TODO: make it configurable too */

	maxsize = (HAK_TYPE_MAX(hak_ooi_t) - HAK_PROCESS_NAMED_INSTVARS) / 3;

	if (stksize > maxsize) stksize = maxsize;
	else if (stksize < 192) stksize = 192;

	if (exstksize > maxsize) exstksize = maxsize;
	else if (exstksize < 128) exstksize = 128;

	if (clstksize > maxsize) clstksize = maxsize;
	else if (clstksize < 32) clstksize = 32;

	hak_pushvolat(hak, (hak_oop_t*)&c);
	proc = (hak_oop_process_t)hak_instantiate(hak, hak->c_process, HAK_NULL, stksize + exstksize + clstksize);
	hak_popvolat(hak);
	if (HAK_UNLIKELY(!proc))
	{
		const hak_ooch_t* oldmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt(hak, hak->errnum,
			"unable to instantiate %O - %js", hak->c_process->name, oldmsg);
		return HAK_NULL;
	}

	HAK_OBJ_SET_FLAGS_PROC (proc, 1); /* a special flag to indicate an object is a process instance */
	proc->state = HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_SUSPENDED);

	/* assign a process id to the process */
	alloc_pid(hak, proc);

	proc->initial_context = c;
	proc->current_context = c;

	/* stack */
	proc->sp = HAK_SMOOI_TO_OOP(-1); /* no item */
	proc->st = HAK_SMOOI_TO_OOP(stksize - 1);

	/* exception stack */
	proc->exsp = proc->st; /* no item pushed yet */
	proc->exst = HAK_SMOOI_TO_OOP(stksize + exstksize - 1);

	/* class stack */
	proc->clsp = proc->exst; /* no item pushed yet */
	proc->clst = HAK_SMOOI_TO_OOP(stksize + exstksize + clstksize - 1);

	HAK_ASSERT(hak, (hak_oop_t)c->sender == hak->_nil);

#if defined(HAK_DEBUG_VM_PROCESSOR)
	HAK_LOG2(hak, HAK_LOG_IC | HAK_LOG_DEBUG, "Processor - process[%zd] **CREATED**->%hs\n", HAK_OOP_TO_SMOOI(proc->id), proc_state_to_string(HAK_OOP_TO_SMOOI(proc->state)));
#endif

	/* a process is created in the SUSPENDED state. chain it to the suspended process list */
	suspended_count = HAK_OOP_TO_SMOOI(hak->processor->suspended.count);
	HAK_APPEND_TO_OOP_LIST(hak, &hak->processor->suspended, hak_oop_process_t, proc, ps);
	suspended_count++;
	hak->processor->suspended.count = HAK_SMOOI_TO_OOP(suspended_count);

	total_count++;
	hak->processor->total_count = HAK_SMOOI_TO_OOP(total_count);

	return proc;
}

static HAK_INLINE void sleep_active_process (hak_t* hak, int state)
{
	STORE_ACTIVE_SP(hak);

	/* store the current active context to the current process.
	 * it is the suspended context of the process to be suspended */
	HAK_ASSERT(hak, hak->processor->active != hak->nil_process);

#if defined(HAK_DEBUG_VM_PROCESSOR)
	HAK_LOG3(hak, HAK_LOG_IC | HAK_LOG_DEBUG, "Processor - process[%zd] %hs->%hs in sleep_active_process\n", HAK_OOP_TO_SMOOI(hak->processor->active->id), proc_state_to_string(HAK_OOP_TO_SMOOI(hak->processor->active->state)), proc_state_to_string(state));
#endif

	hak->processor->active->current_context = hak->active_context;
	hak->processor->active->state = HAK_SMOOI_TO_OOP(state);
}

static HAK_INLINE void wake_process (hak_t* hak, hak_oop_process_t proc)
{
#if defined(HAK_DEBUG_VM_PROCESSOR)
	HAK_LOG2(hak, HAK_LOG_IC | HAK_LOG_DEBUG, "Processor - process[%zd] %hs->RUNNING in wake_process\n", HAK_OOP_TO_SMOOI(proc->id), proc_state_to_string(HAK_OOP_TO_SMOOI(proc->state)));
#endif

	/* activate the given process */
	HAK_ASSERT(hak, proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNABLE));
	proc->state = HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNING);
	hak->processor->active = proc;

	LOAD_ACTIVE_SP(hak);

	/* activate the suspended context of the new process */
	SWITCH_ACTIVE_CONTEXT(hak, proc->current_context);

#if defined(HAK_DEBUG_VM_PROCESSOR) && (HAK_DEBUG_VM_PROCESSOR >= 2)
	HAK_LOG3(hak, HAK_LOG_IC | HAK_LOG_DEBUG, "Processor - woke up process[%zd] context %O ip=%zd\n", HAK_OOP_TO_SMOOI(hak->processor->active->id), hak->active_context, hak->ip);
#endif
}

static void switch_to_process (hak_t* hak, hak_oop_process_t proc, int new_state_for_old_active)
{
	/* the new process must not be the currently active process */
	HAK_ASSERT(hak, hak->processor->active != proc);

	/* the new process must be in the runnable state */
	HAK_ASSERT(hak, proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNABLE) ||
	                 proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_WAITING));

	sleep_active_process(hak, new_state_for_old_active);
	wake_process(hak, proc);

	hak->proc_switched = 1;
}

static HAK_INLINE void switch_to_process_from_nil (hak_t* hak, hak_oop_process_t proc)
{
	HAK_ASSERT(hak, hak->processor->active == hak->nil_process);
	wake_process(hak, proc);
	hak->proc_switched = 1;
}

static HAK_INLINE hak_oop_process_t find_next_runnable_process (hak_t* hak)
{
	hak_oop_process_t nrp;
	HAK_ASSERT(hak, hak->processor->active->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNING));
	nrp = hak->processor->active->ps.next;
	if ((hak_oop_t)nrp == hak->_nil) nrp = hak->processor->runnable.first;
	return nrp;
}

static HAK_INLINE void switch_to_next_runnable_process (hak_t* hak)
{
	hak_oop_process_t nrp;
	nrp = find_next_runnable_process(hak);
	if (nrp != hak->processor->active) switch_to_process(hak, nrp, HAK_PROCESS_STATE_RUNNABLE);
}

static HAK_INLINE void chain_into_processor (hak_t* hak, hak_oop_process_t proc, int new_state)
{
	/* the process is not scheduled at all.
	 * link it to the processor's process list. */
	hak_ooi_t runnable_count;
	hak_ooi_t suspended_count;

	/*HAK_ASSERT(hak, (hak_oop_t)proc->ps.prev == hak->_nil);
	HAK_ASSERT(hak, (hak_oop_t)proc->ps.next == hak->_nil);*/

	HAK_ASSERT(hak, proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_SUSPENDED));
	HAK_ASSERT(hak, new_state == HAK_PROCESS_STATE_RUNNABLE || new_state == HAK_PROCESS_STATE_RUNNING);

#if defined(HAK_DEBUG_VM_PROCESSOR)
	HAK_LOG3(hak, HAK_LOG_IC | HAK_LOG_DEBUG,
		"Processor - process[%zd] %hs->%hs in chain_into_processor\n",
		HAK_OOP_TO_SMOOI(proc->id),
		proc_state_to_string(HAK_OOP_TO_SMOOI(proc->state)),
		proc_state_to_string(new_state));
#endif

	runnable_count = HAK_OOP_TO_SMOOI(hak->processor->runnable.count);

	HAK_ASSERT(hak, runnable_count >= 0);

	suspended_count = HAK_OOP_TO_SMOOI(hak->processor->suspended.count);
	HAK_DELETE_FROM_OOP_LIST(hak, &hak->processor->suspended, proc, ps);
	suspended_count--;
	hak->processor->suspended.count = HAK_SMOOI_TO_OOP(suspended_count);

	/* append to the runnable list */
	HAK_APPEND_TO_OOP_LIST(hak, &hak->processor->runnable, hak_oop_process_t, proc, ps);
	proc->state = HAK_SMOOI_TO_OOP(new_state);

	runnable_count++;
	hak->processor->runnable.count = HAK_SMOOI_TO_OOP(runnable_count);
}

static HAK_INLINE void unchain_from_processor (hak_t* hak, hak_oop_process_t proc, int new_state)
{
	hak_ooi_t runnable_count;
	hak_ooi_t suspended_count;
	hak_ooi_t total_count;

	HAK_ASSERT(hak, proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNING) ||
	                 proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNABLE) ||
	                 proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_SUSPENDED));

	HAK_ASSERT(hak, proc->state != HAK_SMOOI_TO_OOP(new_state));

#if defined(HAK_DEBUG_VM_PROCESSOR)
	HAK_LOG3(hak, HAK_LOG_IC | HAK_LOG_DEBUG, "Processor - process[%zd] %hs->%hs in unchain_from_processor\n", HAK_OOP_TO_SMOOI(proc->id), proc_state_to_string(HAK_OOP_TO_SMOOI(proc->state)), proc_state_to_string(HAK_OOP_TO_SMOOI(new_state)));
#endif

	if (proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_SUSPENDED))
	{
		suspended_count = HAK_OOP_TO_SMOOI(hak->processor->suspended.count);
		HAK_ASSERT(hak, suspended_count > 0);
		HAK_DELETE_FROM_OOP_LIST(hak, &hak->processor->suspended, proc, ps);
		suspended_count--;
		hak->processor->suspended.count = HAK_SMOOI_TO_OOP(suspended_count);
	}
	else
	{
		runnable_count = HAK_OOP_TO_SMOOI(hak->processor->runnable.count);
		HAK_ASSERT(hak, runnable_count > 0);
		HAK_DELETE_FROM_OOP_LIST(hak, &hak->processor->runnable, proc, ps);
		runnable_count--;
		hak->processor->runnable.count = HAK_SMOOI_TO_OOP(runnable_count);
		if (runnable_count == 0) hak->processor->active = hak->nil_process;
	}

	if (new_state == HAK_PROCESS_STATE_TERMINATED)
	{
		/* do not chain it to the suspended process list as it's being terminated */
		proc->ps.prev = (hak_oop_process_t)hak->_nil;
		proc->ps.next = (hak_oop_process_t)hak->_nil;

		total_count = HAK_OOP_TO_SMOOI(hak->processor->total_count);
		total_count--;
		hak->processor->total_count = HAK_SMOOI_TO_OOP(total_count);
	}
	else
	{
		/* append to the suspended process list */
		HAK_ASSERT(hak, new_state == HAK_PROCESS_STATE_SUSPENDED);

		suspended_count = HAK_OOP_TO_SMOOI(hak->processor->suspended.count);
		HAK_APPEND_TO_OOP_LIST(hak, &hak->processor->suspended, hak_oop_process_t, proc, ps);
		suspended_count++;
		hak->processor->suspended.count= HAK_SMOOI_TO_OOP(suspended_count);
	}

	proc->state = HAK_SMOOI_TO_OOP(new_state);
}

static HAK_INLINE void chain_into_semaphore (hak_t* hak, hak_oop_process_t proc, hak_oop_semaphore_t sem)
{
	/* append a process to the process list of a semaphore or a semaphore group */

	/* a process chained to a semaphore cannot get chained to
	 * a semaphore again. a process can get chained to a single semaphore
	 * or a single semaphore group only */
	if ((hak_oop_t)proc->sem != hak->_nil) return; /* ignore it if it happens anyway. TODO: is it desirable???? */

	HAK_ASSERT(hak, (hak_oop_t)proc->sem == hak->_nil);
	HAK_ASSERT(hak, (hak_oop_t)proc->sem_wait.prev == hak->_nil);
	HAK_ASSERT(hak, (hak_oop_t)proc->sem_wait.next == hak->_nil);

	/* a semaphore or a semaphore group must be given for process chaining */
	HAK_ASSERT(hak, HAK_IS_SEMAPHORE(hak, sem) || HAK_IS_SEMAPHORE_GROUP(hak, sem));

	/* i assume the head part of the semaphore has the same layout as
	 * the semaphore group */
	HAK_ASSERT(hak, HAK_OFFSETOF(hak_semaphore_t,waiting) ==
	                 HAK_OFFSETOF(hak_semaphore_group_t,waiting));

	HAK_APPEND_TO_OOP_LIST(hak, &sem->waiting, hak_oop_process_t, proc, sem_wait);

	proc->sem = (hak_oop_t)sem;
}

static HAK_INLINE void unchain_from_semaphore (hak_t* hak, hak_oop_process_t proc)
{
	hak_oop_semaphore_t sem;

	HAK_ASSERT(hak, (hak_oop_t)proc->sem != hak->_nil);
	HAK_ASSERT(hak, HAK_IS_SEMAPHORE(hak, proc->sem) || HAK_IS_SEMAPHORE_GROUP(hak, proc->sem));
	HAK_ASSERT(hak, HAK_OFFSETOF(hak_semaphore_t,waiting) == HAK_OFFSETOF(hak_semaphore_group_t,waiting));

	/* proc->sem may be one of a semaphore or a semaphore group.
	 * i assume that 'waiting' is defined to the same position
	 * in both Semaphore and SemaphoreGroup. there is no need to
	 * write different code for each class. */
	sem = (hak_oop_semaphore_t)proc->sem;  /* semgrp = (hak_oop_semaphore_group_t)proc->sem */
	HAK_DELETE_FROM_OOP_LIST(hak, &sem->waiting, proc, sem_wait);

	proc->sem_wait.prev = (hak_oop_process_t)hak->_nil;
	proc->sem_wait.next = (hak_oop_process_t)hak->_nil;
	proc->sem = hak->_nil;
}

static void dump_process_info (hak_t* hak, hak_bitmask_t log_mask)
{
	if (HAK_OOP_TO_SMOOI(hak->processor->runnable.count) > 0)
	{
		hak_oop_process_t p;
		HAK_LOG0 (hak, log_mask, "> Runnable:");
		p = hak->processor->runnable.first;
		while (p)
		{
			HAK_LOG1(hak, log_mask, " %O", p->id);
			if (p == hak->processor->runnable.last) break;
			p = p->ps.next;
		}
		HAK_LOG0 (hak, log_mask, "\n");
	}
	if (HAK_OOP_TO_SMOOI(hak->processor->suspended.count) > 0)
	{
		hak_oop_process_t p;
		HAK_LOG0 (hak, log_mask, "> Suspended:");
		p = hak->processor->suspended.first;
		while (p)
		{
			HAK_LOG1(hak, log_mask, " %O", p->id);
			if (p == hak->processor->suspended.last) break;
			p = p->ps.next;
		}
		HAK_LOG0 (hak, log_mask, "\n");
	}
	if (hak->sem_io_wait_count > 0)
	{
		hak_ooi_t io_handle;

		HAK_LOG0 (hak, log_mask, "> IO semaphores:");
		for (io_handle = 0; io_handle < hak->sem_io_map_capa; io_handle++)
		{
			hak_ooi_t index;

			index = hak->sem_io_map[io_handle];
			if (index >= 0)
			{
				hak_oop_semaphore_t sem;

				HAK_LOG1(hak, log_mask, " h=%zd", io_handle);

				/* dump process IDs waiting for input signaling */
				HAK_LOG0 (hak, log_mask, "(wpi");
				sem = hak->sem_io_tuple[index].sem[HAK_SEMAPHORE_IO_TYPE_INPUT];
				if (sem)
				{
					hak_oop_process_t wp; /* waiting process */
					for (wp = sem->waiting.first; (hak_oop_t)wp != hak->_nil; wp = wp->sem_wait.next)
					{
						HAK_LOG1(hak, log_mask, ":%zd", HAK_OOP_TO_SMOOI(wp->id));
					}
				}
				else
				{
					HAK_LOG0 (hak, log_mask, ":none");
				}

				/* dump process IDs waitingt for output signaling */
				HAK_LOG0 (hak, log_mask, ",wpo");
				sem = hak->sem_io_tuple[index].sem[HAK_SEMAPHORE_IO_TYPE_OUTPUT];
				if (sem)
				{
					hak_oop_process_t wp; /* waiting process */
					for (wp = sem->waiting.first; (hak_oop_t)wp != hak->_nil; wp = wp->sem_wait.next)
					{
						HAK_LOG1(hak, log_mask, ":%zd", HAK_OOP_TO_SMOOI(wp->id));
					}
				}
				else
				{
					HAK_LOG0 (hak, log_mask, ":none");
				}

				HAK_LOG0 (hak, log_mask, ")");
			}
		}
		HAK_LOG0 (hak, log_mask, "\n");
	}
}

static HAK_INLINE void reset_process_stack_pointers (hak_t* hak, hak_oop_process_t proc)
{
#if defined(HAK_DEBUG_VM_PROCESSOR)
	HAK_LOG4(hak, HAK_LOG_IC | HAK_LOG_DEBUG,
		"Processor - process[%zd] SP: %zd(%zd) ST: %zd",
		HAK_OOP_TO_SMOOI(proc->id),
		HAK_OOP_TO_SMOOI(proc->sp), HAK_OOP_TO_SMOOI(proc->sp) - (-1), HAK_OOP_TO_SMOOI(proc->st));
	HAK_LOG6(hak, HAK_LOG_IC | HAK_LOG_DEBUG,
		" EXSP: %zd(%zd) EXST: %zd CLSP: %zd(%zd) CLST: %zd\n",
		HAK_OOP_TO_SMOOI(proc->exsp), HAK_OOP_TO_SMOOI(proc->exsp) - HAK_OOP_TO_SMOOI(proc->st), HAK_OOP_TO_SMOOI(proc->exst),
		HAK_OOP_TO_SMOOI(proc->clsp), HAK_OOP_TO_SMOOI(proc->clsp) - HAK_OOP_TO_SMOOI(proc->exst), HAK_OOP_TO_SMOOI(proc->clst));
#endif

	proc->sp = HAK_SMOOI_TO_OOP(-1); /* invalidate the process stack */
	proc->exsp = proc->st;
	proc->clsp = proc->clst;
}

static void terminate_process (hak_t* hak, hak_oop_process_t proc)
{
	if (proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNING) ||
	    proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNABLE))
	{
		/* RUNNING/RUNNABLE ---> TERMINATED */
	#if defined(HAK_DEBUG_VM_PROCESSOR)
		HAK_LOG2(hak, HAK_LOG_IC | HAK_LOG_DEBUG, "Processor - process[%zd] %hs->TERMINATED in terminate_process\n", HAK_OOP_TO_SMOOI(proc->id), proc_state_to_string(HAK_OOP_TO_SMOOI(proc->state)));
	#endif

		if (proc == hak->processor->active)
		{
			hak_oop_process_t nrp;

			/* terminating the active process */
			HAK_ASSERT(hak, proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNING));

			nrp = find_next_runnable_process(hak);

			STORE_ACTIVE_SP(hak); /* commit the stack pointer before termination */

			unchain_from_processor(hak, proc, HAK_PROCESS_STATE_TERMINATED);
			reset_process_stack_pointers(hak, proc); /* invalidate the process stack */
			proc->current_context = proc->initial_context; /* not needed but just in case */
			/* a runnable or running process must not be chanined to the
			 * process list of a semaphore */
			HAK_ASSERT(hak, (hak_oop_t)proc->sem == hak->_nil);

			if (nrp == proc)
			{
				/* no runnable process after termination */
				HAK_ASSERT(hak, hak->processor->active == hak->nil_process);
				if (HAK_LOG_ENABLED(hak, HAK_LOG_IC | HAK_LOG_DEBUG))
				{
					HAK_LOG5(hak, HAK_LOG_IC | HAK_LOG_DEBUG,
						"No runnable process after termination of process %zd - total %zd runnable/running %zd suspended %zd - sem_io_wait_count %zu\n",
						HAK_OOP_TO_SMOOI(proc->id),
						HAK_OOP_TO_SMOOI(hak->processor->total_count),
						HAK_OOP_TO_SMOOI(hak->processor->runnable.count),
						HAK_OOP_TO_SMOOI(hak->processor->suspended.count),
						hak->sem_io_wait_count
					);

					dump_process_info(hak, HAK_LOG_IC | HAK_LOG_DEBUG);
				}
			}
			else
			{
				/* there are other processes to schedule */
				switch_to_process(hak, nrp, HAK_PROCESS_STATE_TERMINATED);
			}
		}
		else
		{
			/* termiante a runnable process which is not an actively running process */
			HAK_ASSERT(hak, proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNABLE));
			unchain_from_processor(hak, proc, HAK_PROCESS_STATE_TERMINATED);
			reset_process_stack_pointers(hak, proc); /* invalidate the process stack */
		}

		/* when terminated, clear it from the pid table and set the process id to a negative number */
		free_pid(hak, proc);
	}
	else if (proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_SUSPENDED))
	{
		/* SUSPENDED ---> TERMINATED */
	#if defined(HAK_DEBUG_VM_PROCESSOR)
		HAK_LOG2(hak, HAK_LOG_IC | HAK_LOG_DEBUG, "Processor - process[%zd] %hs->TERMINATED in terminate_process\n", HAK_OOP_TO_SMOOI(proc->id), proc_state_to_string(HAK_OOP_TO_SMOOI(proc->state)));
	#endif

		/*proc->state = HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_TERMINATED);*/
		unchain_from_processor(hak, proc, HAK_PROCESS_STATE_TERMINATED);
		reset_process_stack_pointers(hak, proc); /* invalidate the process stack */

		if ((hak_oop_t)proc->sem != hak->_nil)
		{
			if (HAK_IS_SEMAPHORE_GROUP(hak, proc->sem))
			{
				if (HAK_OOP_TO_SMOOI(((hak_oop_semaphore_group_t)proc->sem)->sem_io_count) > 0)
				{
					HAK_ASSERT(hak, hak->sem_io_wait_count > 0);
					hak->sem_io_wait_count--;
					HAK_DEBUG1 (hak, "terminate_process(sg) - lowered sem_io_wait_count to %zu\n", hak->sem_io_wait_count);
				}
			}
			else
			{
				HAK_ASSERT(hak, HAK_IS_SEMAPHORE(hak, proc->sem));
				if (((hak_oop_semaphore_t)proc->sem)->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_IO))
				{
					HAK_ASSERT(hak, hak->sem_io_wait_count > 0);
					hak->sem_io_wait_count--;
					HAK_DEBUG3 (hak, "terminate_process(s) - lowered sem_io_wait_count to %zu for IO semaphore at index %zd handle %zd\n",
						hak->sem_io_wait_count,
						HAK_OOP_TO_SMOOI(((hak_oop_semaphore_t)proc->sem)->u.io.index),
						HAK_OOP_TO_SMOOI(((hak_oop_semaphore_t)proc->sem)->u.io.handle)
					);
				}
			}

			unchain_from_semaphore(hak, proc);
		}

		/* when terminated, clear it from the pid table and set the process id to a negative number */
		free_pid(hak, proc);
	}
#if 0
	else if (proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_WAITING))
	{
		/* WAITING ---> TERMINATED */
		/* TODO: */
	}
#endif
}

static void terminate_all_processes (hak_t* hak)
{
	while (HAK_OOP_TO_SMOOI(hak->processor->suspended.count) > 0)
	{
		terminate_process(hak, hak->processor->suspended.first);
	}

	while (HAK_OOP_TO_SMOOI(hak->processor->runnable.count) > 0)
	{
		terminate_process(hak, hak->processor->runnable.first);
	}

	HAK_ASSERT(hak, HAK_OOP_TO_SMOOI(hak->processor->total_count) == 0);
	HAK_ASSERT(hak, hak->processor->active == hak->nil_process);
}

static void resume_process (hak_t* hak, hak_oop_process_t proc)
{
	if (proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_SUSPENDED))
	{
		/* SUSPENDED ---> RUNNABLE */
		/*HAK_ASSERT(hak, (hak_oop_t)proc->ps.prev == hak->_nil);
		HAK_ASSERT(hak, (hak_oop_t)proc->ps.next == hak->_nil);*/

	#if defined(HAK_DEBUG_VM_PROCESSOR)
		HAK_LOG2(hak, HAK_LOG_IC | HAK_LOG_DEBUG, "Processor - process[%zd] %hs->RUNNABLE in resume_process\n", HAK_OOP_TO_SMOOI(proc->id), proc_state_to_string(HAK_OOP_TO_SMOOI(proc->state)));
	#endif

		/* don't switch to this process. just change the state to RUNNABLE.
		 * process switching should be triggerd by the process scheduler. */
		chain_into_processor(hak, proc, HAK_PROCESS_STATE_RUNNABLE);
		/*proc->current_context = proc->initial_context;*/
	}
#if 0
	else if (proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNABLE))
	{
		/* RUNNABLE ---> RUNNING */
		/* TODO: should i allow this? */
		HAK_ASSERT(hak, hak->processor->active != proc);
		switch_to_process(hak, proc, HAK_PROCESS_STATE_RUNNABLE);
	}
#endif
}

static void suspend_process (hak_t* hak, hak_oop_process_t proc)
{
	if (proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNING) ||
	    proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNABLE))
	{
		/* RUNNING/RUNNABLE ---> SUSPENDED */

	#if defined(HAK_DEBUG_VM_PROCESSOR)
		HAK_LOG2(hak, HAK_LOG_IC | HAK_LOG_DEBUG, "Processor - process[%zd] %hs->SUSPENDED in suspend_process\n", HAK_OOP_TO_SMOOI(proc->id), proc_state_to_string(HAK_OOP_TO_SMOOI(proc->state)));
	#endif

		if (proc == hak->processor->active)
		{
			/* suspend the active process */
			hak_oop_process_t nrp;

			nrp = find_next_runnable_process(hak);
			if (nrp == proc)
			{
				/* no runnable process after suspension */
				sleep_active_process(hak, HAK_PROCESS_STATE_RUNNABLE);
				unchain_from_processor(hak, proc, HAK_PROCESS_STATE_SUSPENDED);

				/* the last running/runnable process has been unchained
				 * from the processor and set to SUSPENDED. the active
				 * process must be the nil process */
				HAK_ASSERT(hak, hak->processor->active == hak->nil_process);
			}
			else
			{
				/* unchain_from_processor moves the process to the suspended
				 * process and sets its state to the given state(SUSPENDED here).
				 * it doesn't change the active process. we switch the active
				 * process with switch_to_process(). setting the state of the
				 * old active process to SUSPENDED is redundant because it's
				 * done in unchain_from_processor(). the state of the active
				 * process is somewhat wrong for a short period of time until
				 * switch_to_process() has changed the active process. */
				unchain_from_processor(hak, proc, HAK_PROCESS_STATE_SUSPENDED);
				HAK_ASSERT(hak, hak->processor->active != hak->nil_process);
				switch_to_process(hak, nrp, HAK_PROCESS_STATE_SUSPENDED);
			}
		}
		else
		{
			unchain_from_processor(hak, proc, HAK_PROCESS_STATE_SUSPENDED);
		}
	}
}

static void yield_process (hak_t* hak, hak_oop_process_t proc)
{
	if (proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNING))
	{
		/* RUNNING --> RUNNABLE */
		hak_oop_process_t nrp;

		HAK_ASSERT(hak, proc == hak->processor->active);
		HAK_ASSERT(hak, HAK_IS_PROCESS(hak, proc));

		nrp = find_next_runnable_process(hak);
		/* if there are more than 1 runnable processes, the next
		 * runnable process must be different from proc */
		if (nrp != proc)
		{
		#if defined(HAK_DEBUG_VM_PROCESSOR)
			HAK_LOG2(hak, HAK_LOG_IC | HAK_LOG_DEBUG, "Processor - process[%zd] %hs->RUNNABLE in yield_process\n", HAK_OOP_TO_SMOOI(proc->id), proc_state_to_string(HAK_OOP_TO_SMOOI(proc->state)));
		#endif
			switch_to_process(hak, nrp, HAK_PROCESS_STATE_RUNNABLE);
		}
	}
}


static int async_signal_semaphore (hak_t* hak, hak_oop_semaphore_t sem)
{
#if 0
	if (hak->sem_list_count >= SEM_LIST_MAX)
	{
		hak_seterrnum(hak, HAK_ESLFULL);
		return -1;
	}

	if (hak->sem_list_count >= hak->sem_list_capa)
	{
		hak_oow_t new_capa;
		hak_oop_semaphore_t* tmp;

		new_capa = hak->sem_list_capa + SEM_LIST_INC; /* TODO: overflow check.. */
		tmp = (hak_oop_semaphore_t*)hak_reallocmem(hak, hak->sem_list, HAK_SIZEOF(hak_oop_semaphore_t) * new_capa);
		if (HAK_UNLIKELY(!tmp)) return -1;

		hak->sem_list = tmp;
		hak->sem_list_capa = new_capa;
	}

	hak->sem_list[hak->sem_list_count] = sem;
	hak->sem_list_count++;
#endif
	return 0;
}

static hak_oop_process_t signal_semaphore (hak_t* hak, hak_oop_semaphore_t sem)
{
	hak_oop_process_t proc;
	hak_ooi_t count;
	hak_oop_semaphore_group_t sg;

	sg = sem->group;
	if ((hak_oop_t)sg != hak->_nil)
	{
		/* the semaphore belongs to a semaphore group */
		if ((hak_oop_t)sg->waiting.first != hak->_nil)
		{
			hak_ooi_t sp;

			/* there is a process waiting on the process group */
			proc = sg->waiting.first; /* will wake the first process in the waiting list */

			unchain_from_semaphore(hak, proc);
			resume_process(hak, proc);

			/* [IMPORTANT] RETURN VALUE of SemaphoreGroup's wait.
			 * ------------------------------------------------------------
			 * the waiting process has been suspended after a waiting
			 * primitive function in Semaphore or SemaphoreGroup.
			 * the top of the stack of the process must hold the temporary
			 * return value set by await_semaphore() or await_semaphore_group().
			 * change the return value forcibly to the actual signaled
			 * semaphore */
			HAK_ASSERT(hak, HAK_OOP_TO_SMOOI(proc->sp) < (hak_ooi_t)(HAK_OBJ_GET_SIZE(proc) - HAK_PROCESS_NAMED_INSTVARS));
			sp = HAK_OOP_TO_SMOOI(proc->sp);
			proc->slot[sp] = (hak_oop_t)sem;

			/* i should decrement the counter as long as the group being
			 * signaled contains an IO semaphore */
			if (HAK_OOP_TO_SMOOI(sg->sem_io_count) > 0)
			{
				HAK_ASSERT(hak, hak->sem_io_wait_count > 0);
				hak->sem_io_wait_count--;
				HAK_DEBUG2 (hak, "signal_semaphore(sg) - lowered sem_io_wait_count to %zu for handle %zd\n", hak->sem_io_wait_count, HAK_OOP_TO_SMOOI(sem->u.io.handle));
			}
			return proc;
		}
	}

	/* if the semaphore belongs to a semaphore group and the control reaches
	 * here, no process is waiting on the semaphore group. however, a process
	 * may still be waiting on the semaphore. If a process waits on a semaphore
	 * group and another process wait on a semaphore that belongs to the
	 * semaphore group, the process waiting on the group always wins.
	 *
	 *    TODO: implement a fair scheduling policy. or do i simply have to disallow individual wait on a semaphore belonging to a group?
	 *
	 * if it doesn't belong to a sempahore group, i'm free from the starvation issue.
	 */
	if ((hak_oop_t)sem->waiting.first == hak->_nil)
	{
		/* no process is waiting on this semaphore */

		count = HAK_OOP_TO_SMOOI(sem->count);
		count++;
		sem->count = HAK_SMOOI_TO_OOP(count);

		HAK_ASSERT(hak, count >= 1);
		if (count == 1 && (hak_oop_t)sg != hak->_nil)
		{
			/* move the semaphore from the unsignaled list to the signaled list
			 * if the semaphore count has changed from 0 to 1 in a group */
			HAK_DELETE_FROM_OOP_LIST(hak, &sg->sems[HAK_SEMAPHORE_GROUP_SEMS_UNSIG], sem, grm);
			HAK_APPEND_TO_OOP_LIST(hak, &sg->sems[HAK_SEMAPHORE_GROUP_SEMS_SIG], hak_oop_semaphore_t, sem, grm);
		}

		/* no process has been resumed */
		return (hak_oop_process_t)hak->_nil;
	}
	else
	{
		proc = sem->waiting.first;

		/* [NOTE] no GC must occur as 'proc' isn't protected with hak_pushvolat(). */

		/* detach a process from a semaphore's waiting list and
		 * make it runnable */
		unchain_from_semaphore(hak, proc);
		resume_process(hak, proc);

		if (sem->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_IO))
		{
			HAK_ASSERT(hak, hak->sem_io_wait_count > 0);
			hak->sem_io_wait_count--;
			HAK_DEBUG3 (hak, "signal_semaphore(s) - lowered sem_io_wait_count to %zu for IO semaphore at index %zd handle %zd\n",
				hak->sem_io_wait_count, HAK_OOP_TO_SMOOI(sem->u.io.index), HAK_OOP_TO_SMOOI(sem->u.io.handle));
		}

		/* return the resumed(runnable) process */
		return proc;
	}
}

static HAK_INLINE int can_await_semaphore (hak_t* hak, hak_oop_semaphore_t sem)
{
	/* a sempahore that doesn't belong to a gruop can be waited on */
	return (hak_oop_t)sem->group == hak->_nil;
}

static HAK_INLINE void await_semaphore (hak_t* hak, hak_oop_semaphore_t sem)
{
	hak_oop_process_t proc;
	hak_ooi_t count;
	hak_oop_semaphore_group_t semgrp;

	semgrp = sem->group;

	/* the caller of this function must ensure that the semaphore doesn't belong to a group */
	HAK_ASSERT(hak, (hak_oop_t)semgrp == hak->_nil);

	count = HAK_OOP_TO_SMOOI(sem->count);
	if (count > 0)
	{
		/* it's already signaled */
		count--;
		sem->count = HAK_SMOOI_TO_OOP(count);

		if ((hak_oop_t)semgrp != hak->_nil && count == 0)
		{

			int sems_idx;
			/* TODO: if i disallow individual wait on a semaphore in a group,
			 *       this membership manipulation is redundant */
			HAK_DELETE_FROM_OOP_LIST(hak, &semgrp->sems[HAK_SEMAPHORE_GROUP_SEMS_SIG], sem, grm);
			sems_idx = count > 0? HAK_SEMAPHORE_GROUP_SEMS_SIG: HAK_SEMAPHORE_GROUP_SEMS_UNSIG;
			HAK_APPEND_TO_OOP_LIST(hak, &semgrp->sems[sems_idx], hak_oop_semaphore_t, sem, grm);
		}
	}
	else
	{
		/* not signaled. need to wait */
		proc = hak->processor->active;

		/* suspend the active process */
		suspend_process(hak, proc);

		/* link the suspended process to the semaphore's process list */
		chain_into_semaphore(hak, proc, sem);

		HAK_ASSERT(hak, sem->waiting.last == proc);

		if (sem->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_IO))
		{
			hak->sem_io_wait_count++;
			HAK_DEBUG3 (hak, "await_semaphore - raised sem_io_wait_count to %zu for IO semaphore at index %zd handle %zd\n",
				hak->sem_io_wait_count, HAK_OOP_TO_SMOOI(sem->u.io.index), HAK_OOP_TO_SMOOI(sem->u.io.handle));
		}

		HAK_ASSERT(hak, hak->processor->active != proc);
	}
}

static HAK_INLINE hak_oop_t await_semaphore_group (hak_t* hak, hak_oop_semaphore_group_t semgrp)
{
	/* wait for one of semaphores in the group to be signaled */

	hak_oop_process_t proc;
	hak_oop_semaphore_t sem;

	HAK_ASSERT(hak, HAK_IS_SEMAPHORE_GROUP(hak, semgrp));

	if (HAK_OOP_TO_SMOOI(semgrp->sem_count) <= 0)
	{
		/* cannot wait on a semaphore group that has no member semaphores.
		 * return failure if waiting on such a semapohre group is attempted */
		HAK_ASSERT(hak, (hak_oop_t)semgrp->sems[HAK_SEMAPHORE_GROUP_SEMS_SIG].first == hak->_nil);
		HAK_ASSERT(hak, (hak_oop_t)semgrp->sems[HAK_SEMAPHORE_GROUP_SEMS_SIG].last == hak->_nil);
		return HAK_ERROR_TO_OOP(HAK_EINVAL); /* TODO: better error code? */
	}

	sem = semgrp->sems[HAK_SEMAPHORE_GROUP_SEMS_SIG].first;
	if ((hak_oop_t)sem != hak->_nil)
	{
		hak_ooi_t count;
		int sems_idx;

		/* there is a semaphore signaled in the group */
		count = HAK_OOP_TO_SMOOI(sem->count);
		HAK_ASSERT(hak, count > 0);
		count--;
		sem->count = HAK_SMOOI_TO_OOP(count);

		HAK_DELETE_FROM_OOP_LIST(hak, &semgrp->sems[HAK_SEMAPHORE_GROUP_SEMS_SIG], sem, grm);
		sems_idx = count > 0? HAK_SEMAPHORE_GROUP_SEMS_SIG: HAK_SEMAPHORE_GROUP_SEMS_UNSIG;
		HAK_APPEND_TO_OOP_LIST(hak, &semgrp->sems[sems_idx], hak_oop_semaphore_t, sem, grm);

		return (hak_oop_t)sem;
	}

	/* no semaphores have been signaled. suspend the current process
	 * until at least one of them is signaled */
	proc = hak->processor->active;

	/* suspend the active process */
	suspend_process(hak, proc);

	/* link the suspended process to the semaphore group's process list */
	chain_into_semaphore(hak, proc, (hak_oop_semaphore_t)semgrp);

	HAK_ASSERT(hak, semgrp->waiting.last == proc);

	if (HAK_OOP_TO_SMOOI(semgrp->sem_io_count) > 0)
	{
		/* there might be more than 1 IO semaphores in the group
		 * but i increment hak->sem_io_wait_count by 1 only */
		hak->sem_io_wait_count++;
		HAK_DEBUG1 (hak, "await_semaphore_group - raised sem_io_wait_count to %zu\n", hak->sem_io_wait_count);
	}

	/* the current process will get suspended after the caller (mostly a
	 * a primitive function handler) is over as it's added to a suspened
	 * process list above */
	HAK_ASSERT(hak, hak->processor->active != proc);
	return hak->_nil;
}

static void sift_up_sem_heap (hak_t* hak, hak_ooi_t index)
{
	if (index > 0)
	{
		hak_ooi_t parent;
		hak_oop_semaphore_t sem, parsem;

		parent = SEM_HEAP_PARENT(index);
		sem = hak->sem_heap[index];
		parsem = hak->sem_heap[parent];
		if (SEM_HEAP_EARLIER_THAN(hak, sem, parsem))
		{
			do
			{
				/* move down the parent to the current position */
				parsem->u.timed.index = HAK_SMOOI_TO_OOP(index);
				hak->sem_heap[index] = parsem;

				/* traverse up */
				index = parent;
				if (index <= 0) break;

				parent = SEM_HEAP_PARENT(parent);
				parsem = hak->sem_heap[parent];
			}
			while (SEM_HEAP_EARLIER_THAN(hak, sem, parsem));

			sem->u.timed.index = HAK_SMOOI_TO_OOP(index);
			hak->sem_heap[index] = sem;
		}
	}
}

static void sift_down_sem_heap (hak_t* hak, hak_ooi_t index)
{
	hak_ooi_t base = hak->sem_heap_count / 2;

	if (index < base) /* at least 1 child is under the 'index' position */
	{
		hak_ooi_t left, right, child;
		hak_oop_semaphore_t sem, chisem;

		sem = hak->sem_heap[index];
		do
		{
			left = SEM_HEAP_LEFT(index);
			right = SEM_HEAP_RIGHT(index);

			if (right < hak->sem_heap_count && SEM_HEAP_EARLIER_THAN(hak, hak->sem_heap[right], hak->sem_heap[left]))
			{
				child = right;
			}
			else
			{
				child = left;
			}

			chisem = hak->sem_heap[child];
			if (SEM_HEAP_EARLIER_THAN(hak, sem, chisem)) break;

			chisem->u.timed.index = HAK_SMOOI_TO_OOP(index);
			hak->sem_heap[index] = chisem;

			index = child;
		}
		while (index < base);

		sem->u.timed.index = HAK_SMOOI_TO_OOP(index);
		hak->sem_heap[index] = sem;
	}
}

static int add_to_sem_heap (hak_t* hak, hak_oop_semaphore_t sem)
{
	hak_ooi_t index;

	HAK_ASSERT(hak, sem->subtype == hak->_nil);

	if (hak->sem_heap_count >= SEM_HEAP_MAX)
	{
		hak_seterrbfmt(hak, HAK_ESEMFLOOD, "too many semaphores in the semaphore heap");
		return -1;
	}

	if (hak->sem_heap_count >= hak->sem_heap_capa)
	{
		hak_oow_t new_capa;
		hak_oop_semaphore_t* tmp;

		/* no overflow check when calculating the new capacity
		 * owing to SEM_HEAP_MAX check above */
		new_capa = hak->sem_heap_capa + SEM_HEAP_INC;
		tmp = (hak_oop_semaphore_t*)hak_reallocmem(hak, hak->sem_heap, HAK_SIZEOF(hak_oop_semaphore_t) * new_capa);
		if (HAK_UNLIKELY(!tmp)) return -1;

		hak->sem_heap = tmp;
		hak->sem_heap_capa = new_capa;
	}

	HAK_ASSERT(hak, hak->sem_heap_count <= HAK_SMOOI_MAX);

	index = hak->sem_heap_count;
	hak->sem_heap[index] = sem;
	sem->u.timed.index = HAK_SMOOI_TO_OOP(index);
	sem->subtype = HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_TIMED);
	hak->sem_heap_count++;

	sift_up_sem_heap(hak, index);
	return 0;
}

static void delete_from_sem_heap (hak_t* hak, hak_ooi_t index)
{
	hak_oop_semaphore_t sem, lastsem;

	HAK_ASSERT(hak, index >= 0 && index < hak->sem_heap_count);

	sem = hak->sem_heap[index];

	sem->subtype = hak->_nil;
	sem->u.timed.index = hak->_nil;
	sem->u.timed.ftime_sec = hak->_nil;
	sem->u.timed.ftime_nsec = hak->_nil;

	hak->sem_heap_count--;
	if (/*hak->sem_heap_count > 0 &&*/ index != hak->sem_heap_count)
	{
		/* move the last item to the deletion position */
		lastsem = hak->sem_heap[hak->sem_heap_count];
		lastsem->u.timed.index = HAK_SMOOI_TO_OOP(index);
		hak->sem_heap[index] = lastsem;

		if (SEM_HEAP_EARLIER_THAN(hak, lastsem, sem))
			sift_up_sem_heap(hak, index);
		else
			sift_down_sem_heap(hak, index);
	}
}

#if 0
/* unused */
static void update_sem_heap (hak_t* hak, hak_ooi_t index, hak_oop_semaphore_t newsem)
{
	hak_oop_semaphore_t sem;

	sem = hak->sem_heap[index];
	sem->timed.index = hak->_nil;

	newsem->timed.index = HAK_SMOOI_TO_OOP(index);
	hak->sem_heap[index] = newsem;

	if (SEM_HEAP_EARLIER_THAN(hak, newsem, sem))
		sift_up_sem_heap(hak, index);
	else
		sift_down_sem_heap(hak, index);
}
#endif

static int add_sem_to_sem_io_tuple (hak_t* hak, hak_oop_semaphore_t sem, hak_ooi_t io_handle, hak_semaphore_io_type_t io_type)
{
	hak_ooi_t index;
	hak_ooi_t new_mask;
	int n, tuple_added = 0;

	HAK_ASSERT(hak, sem->subtype == (hak_oop_t)hak->_nil);
	HAK_ASSERT(hak, sem->u.io.index == (hak_oop_t)hak->_nil);
	/*HAK_ASSERT(hak, sem->io.handle == (hak_oop_t)hak->_nil);
	HAK_ASSERT(hak, sem->io.type == (hak_oop_t)hak->_nil);*/

	if (io_handle < 0)
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "handle %zd out of supported range", io_handle);
		return -1;
	}

	if (io_handle >= hak->sem_io_map_capa)
	{
		hak_oow_t new_capa, i;
		hak_ooi_t* tmp;

/* TODO: specify the maximum io_handle supported and check it here instead of just relying on memory allocation success/failure? */
		new_capa = HAK_ALIGN_POW2(io_handle + 1, SEM_IO_MAP_ALIGN);

		tmp = (hak_ooi_t*)hak_reallocmem(hak, hak->sem_io_map, HAK_SIZEOF(*tmp) * new_capa);
		if (HAK_UNLIKELY(!tmp))
		{
			const hak_ooch_t* oldmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, hak->errnum, "handle %zd out of supported range - %js", oldmsg);
			return -1;
		}

		for (i = hak->sem_io_map_capa; i < new_capa; i++) tmp[i] = -1;

		hak->sem_io_map = tmp;
		hak->sem_io_map_capa = new_capa;
	}

	index = hak->sem_io_map[io_handle];
	if (index <= -1)
	{
		/* this handle is not in any tuples. add it to a new tuple */
		if (hak->sem_io_tuple_count >= SEM_IO_TUPLE_MAX)
		{
			hak_seterrbfmt(hak, HAK_ESEMFLOOD, "too many IO semaphore tuples");
			return -1;
		}

		if (hak->sem_io_tuple_count >= hak->sem_io_tuple_capa)
		{
			hak_oow_t new_capa;
			hak_sem_tuple_t* tmp;

			/* no overflow check when calculating the new capacity
			 * owing to SEM_IO_TUPLE_MAX check above */
			new_capa = hak->sem_io_tuple_capa + SEM_IO_TUPLE_INC;
			tmp = (hak_sem_tuple_t*)hak_reallocmem(hak, hak->sem_io_tuple, HAK_SIZEOF(hak_sem_tuple_t) * new_capa);
			if (HAK_UNLIKELY(!tmp)) return -1;

			hak->sem_io_tuple = tmp;
			hak->sem_io_tuple_capa = new_capa;
		}

		/* this condition must be true assuming SEM_IO_TUPLE_MAX <= HAK_SMOOI_MAX */
		HAK_ASSERT(hak, hak->sem_io_tuple_count <= HAK_SMOOI_MAX);
		index = hak->sem_io_tuple_count;

		tuple_added = 1;

		/* safe to initialize before vm_muxadd() because
		 * hak->sem_io_tuple_count has not been incremented.
		 * still no impact even if it fails. */
		hak->sem_io_tuple[index].sem[HAK_SEMAPHORE_IO_TYPE_INPUT] = HAK_NULL;
		hak->sem_io_tuple[index].sem[HAK_SEMAPHORE_IO_TYPE_OUTPUT] = HAK_NULL;
		hak->sem_io_tuple[index].handle = io_handle;
		hak->sem_io_tuple[index].mask = 0;

		new_mask = ((hak_ooi_t)1 << io_type);

		hak_pushvolat(hak, (hak_oop_t*)&sem);
		n = hak->vmprim.vm_muxadd(hak, io_handle, new_mask);
		hak_popvolat(hak);
	}
	else
	{
		if (hak->sem_io_tuple[index].sem[io_type])
		{
			hak_seterrbfmt(hak, HAK_EINVAL, "handle %zd already linked with an IO semaphore for %hs", io_handle, io_type_str[io_type]);
			return -1;
		}

		new_mask = hak->sem_io_tuple[index].mask; /* existing mask */
		new_mask |= ((hak_ooi_t)1 << io_type);

		hak_pushvolat(hak, (hak_oop_t*)&sem);
		n = hak->vmprim.vm_muxmod(hak, io_handle, new_mask);
		hak_popvolat(hak);
	}

	if (n <= -1)
	{
		HAK_LOG3(hak, HAK_LOG_WARN, "Failed to add IO semaphore at index %zd for %hs on handle %zd\n", index, io_type_str[io_type], io_handle);
		return -1;
	}

	HAK_LOG3(hak, HAK_LOG_DEBUG, "Added IO semaphore at index %zd for %hs on handle %zd\n", index, io_type_str[io_type], io_handle);

	sem->subtype = HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_IO);
	sem->u.io.index = HAK_SMOOI_TO_OOP(index);
	sem->u.io.handle = HAK_SMOOI_TO_OOP(io_handle);
	sem->u.io.type = HAK_SMOOI_TO_OOP((hak_ooi_t)io_type);

	hak->sem_io_tuple[index].handle = io_handle;
	hak->sem_io_tuple[index].mask = new_mask;
	hak->sem_io_tuple[index].sem[io_type] = sem;

	hak->sem_io_count++;
	if (tuple_added)
	{
		hak->sem_io_tuple_count++;
		hak->sem_io_map[io_handle] = index;
	}

	/* update the number of IO semaphores in a group if necessary */
	if ((hak_oop_t)sem->group != hak->_nil)
	{
		hak_ooi_t count;
		count = HAK_OOP_TO_SMOOI(sem->group->sem_io_count);
		count++;
		sem->group->sem_io_count = HAK_SMOOI_TO_OOP(count);
	}

	return 0;
}

static int delete_sem_from_sem_io_tuple (hak_t* hak, hak_oop_semaphore_t sem, int force)
{
	hak_ooi_t index;
	hak_ooi_t new_mask, io_handle, io_type;
	int x;

	HAK_ASSERT(hak, sem->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_IO));
	HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.io.type));
	HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.io.index));
	HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.io.handle));

	index = HAK_OOP_TO_SMOOI(sem->u.io.index);
	HAK_ASSERT(hak, index >= 0 && index < hak->sem_io_tuple_count);

	io_handle = HAK_OOP_TO_SMOOI(sem->u.io.handle);
	if (io_handle < 0 || io_handle >= hak->sem_io_map_capa)
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "handle %zd out of supported range", io_handle);
		return -1;
	}
	HAK_ASSERT(hak, hak->sem_io_map[io_handle] == HAK_OOP_TO_SMOOI(sem->u.io.index));

	io_type = HAK_OOP_TO_SMOOI(sem->u.io.type);

	new_mask = hak->sem_io_tuple[index].mask;
	new_mask &= ~((hak_ooi_t)1 << io_type); /* this is the new mask after deletion */

	hak_pushvolat(hak, (hak_oop_t*)&sem);
	x = new_mask? hak->vmprim.vm_muxmod(hak, io_handle, new_mask):
	              hak->vmprim.vm_muxdel(hak, io_handle);
	hak_popvolat(hak);
	if (x <= -1)
	{
		HAK_LOG3(hak, HAK_LOG_WARN, "Failed to delete IO semaphore at index %zd handle %zd for %hs\n", index, io_handle, io_type_str[io_type]);
		if (!force) return -1;

		/* [NOTE]
		 *   this means there could be some issue handling the file handles.
		 *   the file handle might have been closed before reaching here.
		 *   assuming the callback works correctly, it's not likely that the
		 *   underlying operating system returns failure for no reason.
		 *   i should inspect the overall vm implementation */
		HAK_LOG1(hak, HAK_LOG_ERROR, "Forcibly unmapping the IO semaphored handle %zd as if it's deleted\n", io_handle);
	}
	else
	{
		HAK_LOG3(hak, HAK_LOG_DEBUG, "Deleted IO semaphore at index %zd handle %zd for %hs\n", index, io_handle, io_type_str[io_type]);
	}

	sem->subtype = hak->_nil;
	sem->u.io.index = hak->_nil;
	sem->u.io.handle = hak->_nil;
	sem->u.io.type = hak->_nil;
	hak->sem_io_count--;

	if ((hak_oop_t)sem->group != hak->_nil)
	{
		hak_ooi_t count;
		count = HAK_OOP_TO_SMOOI(sem->group->sem_io_count);
		HAK_ASSERT(hak, count > 0);
		count--;
		sem->group->sem_io_count = HAK_SMOOI_TO_OOP(count);
	}

	if (new_mask)
	{
		hak->sem_io_tuple[index].mask = new_mask;
		hak->sem_io_tuple[index].sem[io_type] = HAK_NULL;
	}
	else
	{
		hak->sem_io_tuple_count--;

		if (/*hak->sem_io_tuple_count > 0 &&*/ index != hak->sem_io_tuple_count)
		{
			/* migrate the last item to the deleted slot to compact the gap */
			hak->sem_io_tuple[index] = hak->sem_io_tuple[hak->sem_io_tuple_count];

			if (hak->sem_io_tuple[index].sem[HAK_SEMAPHORE_IO_TYPE_INPUT])
				hak->sem_io_tuple[index].sem[HAK_SEMAPHORE_IO_TYPE_INPUT]->u.io.index = HAK_SMOOI_TO_OOP(index);
			if (hak->sem_io_tuple[index].sem[HAK_SEMAPHORE_IO_TYPE_OUTPUT])
				hak->sem_io_tuple[index].sem[HAK_SEMAPHORE_IO_TYPE_OUTPUT]->u.io.index = HAK_SMOOI_TO_OOP(index);

			hak->sem_io_map[hak->sem_io_tuple[index].handle] = index;

			HAK_LOG2(hak, HAK_LOG_DEBUG, "Migrated IO semaphore tuple from index %zd to %zd\n", hak->sem_io_tuple_count, index);
		}

		hak->sem_io_map[io_handle] = -1;
	}

	return 0;
}

static void _signal_io_semaphore (hak_t* hak, hak_oop_semaphore_t sem)
{
	hak_oop_process_t proc;

	proc = signal_semaphore(hak, sem);

	if (hak->processor->active == hak->nil_process && (hak_oop_t)proc != hak->_nil)
	{
		/* this is the only runnable process.
		 * switch the process to the running state.
		 * it uses wake_process() instead of
		 * switch_to_process() as there is no running
		 * process at this moment */
		HAK_ASSERT(hak, proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNABLE));
		HAK_ASSERT(hak, proc == hak->processor->runnable.first);

	#if 0
		wake_process(hak, proc); /* switch to running */
		hak->proc_switched = 1;
	#else
		switch_to_process_from_nil(hak, proc);
	#endif
	}
}

static void signal_io_semaphore (hak_t* hak, hak_ooi_t io_handle, hak_ooi_t mask)
{
	if (io_handle >= 0 && io_handle < hak->sem_io_map_capa && hak->sem_io_map[io_handle] >= 0)
	{
		hak_oop_semaphore_t insem, outsem;
		hak_ooi_t sem_io_index;

		sem_io_index = hak->sem_io_map[io_handle];
		insem = hak->sem_io_tuple[sem_io_index].sem[HAK_SEMAPHORE_IO_TYPE_INPUT];
		outsem = hak->sem_io_tuple[sem_io_index].sem[HAK_SEMAPHORE_IO_TYPE_OUTPUT];

		if (outsem)
		{
			if ((mask & (HAK_SEMAPHORE_IO_MASK_OUTPUT | HAK_SEMAPHORE_IO_MASK_ERROR)) ||
			    (!insem && (mask & HAK_SEMAPHORE_IO_MASK_HANGUP)))
			{
				_signal_io_semaphore(hak, outsem);
			}
		}

		if (insem)
		{
			if (mask & (HAK_SEMAPHORE_IO_MASK_INPUT | HAK_SEMAPHORE_IO_MASK_HANGUP | HAK_SEMAPHORE_IO_MASK_ERROR))
			{
				_signal_io_semaphore(hak, insem);
			}
		}
	}
	else
	{
		/* you may come across this warning message if the multiplexer returned
		 * an IO event */
		HAK_LOG2(hak, HAK_LOG_WARN, "Warning - semaphore signaling requested on an unmapped handle %zd with mask %#zx\n", io_handle, mask);
	}
}

void hak_releaseiohandle (hak_t* hak, hak_ooi_t io_handle)
{
	/* TODO: optimize io semapore unmapping. since i'm to close the handle,
	 *       i don't need to call delete_sem_from_sem_io_tuple() seperately for input
	 *       and output. */
	if (io_handle < hak->sem_io_map_capa)
	{
		hak_ooi_t index;
		hak_oop_semaphore_t sem;

		index = hak->sem_io_map[io_handle];
		if (index >= 0)
		{
			HAK_ASSERT(hak, hak->sem_io_tuple[index].handle == io_handle);
			sem = hak->sem_io_tuple[index].sem[HAK_SEMAPHORE_IO_TYPE_INPUT];
			if (sem)
			{
				HAK_ASSERT(hak, sem->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_IO));
				delete_sem_from_sem_io_tuple(hak, sem, 0);
			}
		}
	}

	if (io_handle < hak->sem_io_map_capa)
	{
		hak_ooi_t index;
		hak_oop_semaphore_t sem;

		index = hak->sem_io_map[io_handle];
		if (index >= 0)
		{
			HAK_ASSERT(hak, hak->sem_io_tuple[index].handle == io_handle);
			sem = hak->sem_io_tuple[index].sem[HAK_SEMAPHORE_IO_TYPE_OUTPUT];
			if (sem)
			{
				HAK_ASSERT(hak, sem->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_IO));
				delete_sem_from_sem_io_tuple(hak, sem, 0);
			}
		}
	}
}

/* ------------------------------------------------------------------------- */

static int prepare_new_context (hak_t* hak, hak_oop_block_t op_blk, hak_ooi_t nargs, int nargs_offset, hak_ooi_t req_nrvars, int copy_args, int is_msgsend, hak_ooi_t msg_ivaroff, hak_oop_context_t* pnewctx)
{
	/* prepare a new block context for activation.
	 * the passed block context becomes the base for a new block context. */

	hak_oop_context_t blkctx;
	hak_ooi_t attr_mask;
	hak_ooi_t fblk_nrvars, fblk_nlvars;
	hak_ooi_t fixed_nargs, actual_nargs, excess_nargs;

	/* the receiver must be a block context */
	HAK_ASSERT(hak, HAK_IS_COMPILED_BLOCK(hak, op_blk));

	attr_mask = HAK_OOP_TO_SMOOI(op_blk->attr_mask);

	fblk_nrvars = GET_BLK_MASK_NRVARS(attr_mask);
	fblk_nlvars = GET_BLK_MASK_NLVARS(attr_mask);
	fixed_nargs = GET_BLK_MASK_NARGS(attr_mask);
	actual_nargs = nargs - nargs_offset;
	excess_nargs = actual_nargs - fixed_nargs;

	if (actual_nargs < fixed_nargs || (!GET_BLK_MASK_VA(attr_mask) && actual_nargs > fixed_nargs))
	{
		HAK_LOG3(hak, HAK_LOG_IC | HAK_LOG_ERROR,
			"Error - wrong number of arguments to a block %O - expecting %zd, got %zd\n",
			op_blk, fixed_nargs, actual_nargs);
		hak_seterrbfmt(hak, HAK_ECALLARG, "wrong number of argument passed to function block - %zd expected, %zd passed", fixed_nargs, actual_nargs);
		return -1;
	}

	if (req_nrvars > fblk_nrvars)
	{
		HAK_LOG3(hak, HAK_LOG_IC | HAK_LOG_ERROR,
			"Error - wrong number of returns specified of a block %O - max expected %zd, requested %zd\n",
			op_blk, fblk_nrvars, req_nrvars);
		hak_seterrbfmt(hak, HAK_ECALLRET, "wrong number of returns requested of function block - %zd expected at most, %zd requested", fblk_nrvars, req_nrvars);
		return -1;
	}

	/* create a new block context to clone op_blk */
	hak_pushvolat(hak, (hak_oop_t*)&op_blk);
	blkctx = make_context(hak, fixed_nargs + fblk_nrvars + fblk_nlvars + excess_nargs);
	hak_popvolat(hak);
	if (HAK_UNLIKELY(!blkctx)) return -1;

#if 0
	/* shallow-copy the named part including home, origin, etc. */
	for (i = 0; i < HAK_CONTEXT_NAMED_INSTVARS; i++)
	{
		((hak_oop_oop_t)blkctx)->slot[i] = ((hak_oop_oop_t)op_blk)->slot[i];
	}
#else
	blkctx->ip = op_blk->ip;
	blkctx->req_nrets = HAK_SMOOI_TO_OOP(req_nrvars);
	blkctx->attr_mask = op_blk->attr_mask;
	blkctx->base = op_blk->home->base;
	blkctx->name = op_blk->name;

	if (is_msgsend)
	{
		/*blkctx->home = blkctx;*/ /* itself */
		blkctx->home = op_blk->home;
		blkctx->mthhome = blkctx;
		blkctx->receiver = HAK_STACK_GETRCV(hak, nargs);
		blkctx->ivaroff = HAK_SMOOI_TO_OOP(msg_ivaroff);
	}
	else
	{
		blkctx->home = op_blk->home;
		blkctx->mthhome = (hak_oop_context_t)hak->_nil;
		blkctx->receiver = op_blk->home->receiver;
	#if 0 /* filled by make_context() already */
		blkctx->ivaroff = HAK_SMOOI_TO_OOP(0); /* not useful if it's not message send */
	#endif
	}
#endif

	if (HAK_LIKELY(copy_args))
	{
		hak_ooi_t i, j;

		/* copy the fixed arguments to the beginning of the variable part of the context block */
		for (i = 0, j = nargs_offset; i < fixed_nargs; i++, j++)
		{
			blkctx->slot[i] = HAK_STACK_GETARG(hak, nargs, j);
		}

		/* variable arguments. place them behind after local variables. */
		for (i = fixed_nargs + fblk_nrvars + fblk_nlvars ; j < nargs; i++, j++)
		{
			blkctx->slot[i] = HAK_STACK_GETARG(hak, nargs, j);
		}
	}

	HAK_ASSERT(hak, (hak_oop_t)blkctx->home != hak->_nil); /* if not intial context, the home must not be null */
	HAK_ASSERT(hak, (hak_oop_t)blkctx->sender == hak->_nil); /* the sender is not set. the caller must set this if needed */

	*pnewctx = blkctx;
	return 0;
}

static HAK_INLINE int __activate_block (hak_t* hak, hak_oop_block_t op_blk, hak_ooi_t nargs, hak_ooi_t nrvars, int is_msgsend, hak_ooi_t msg_ivaroff, hak_oop_context_t* pnewctx)
{
	int x;

	HAK_ASSERT(hak, HAK_IS_COMPILED_BLOCK(hak, op_blk));

	x = prepare_new_context(
		hak,
		op_blk,
		nargs, /* nargs */
		0, /* nargs_offset */
		nrvars,
		1, /* copy_args */
		is_msgsend,
		msg_ivaroff,
		pnewctx);
	if (HAK_UNLIKELY(x <= -1)) return -1;

	HAK_STACK_POPS(hak, nargs + 2); /* pop arguments, called block/function/method, and receiver */
	(*pnewctx)->sender = hak->active_context;

	return 0;
}

static HAK_INLINE int activate_block (hak_t* hak, hak_ooi_t nargs, hak_ooi_t nrvars)
{
	hak_oop_block_t op_blk;
	hak_oop_context_t newctx;
	int x;

	op_blk = (hak_oop_block_t)HAK_STACK_GETOP(hak, nargs);
	HAK_ASSERT(hak, HAK_IS_COMPILED_BLOCK(hak, op_blk));

	x = __activate_block(hak, op_blk, nargs, nrvars, 0, 0, &newctx);
	if (HAK_UNLIKELY(x <= -1)) return -1;

	SWITCH_ACTIVE_CONTEXT(hak, newctx);
	return 0;
}

/* ------------------------------------------------------------------------- */

static int __activate_function (hak_t* hak, hak_oop_function_t op_func, hak_ooi_t nargs, hak_oop_context_t* pnewctx)
{
	/* prepare a new block context for activation */

	hak_oop_context_t functx;
	hak_ooi_t i, j;
	hak_ooi_t attr_mask;
	hak_ooi_t nrvars, nlvars, fixed_nargs, actual_nargs, excess_nargs;
	hak_ooi_t nargs_offset = 0;

	/*
	  (defun sum (x)
	      (if (< x 2) 1
	       else (+ x (sum (- x 1)))))
	  (printf ">>>> %d\n" (sum 10))
	 */

	HAK_ASSERT(hak, HAK_IS_FUNCTION(hak, op_func));

	attr_mask = HAK_OOP_TO_SMOOI(op_func->attr_mask);
	nrvars = GET_BLK_MASK_NRVARS(attr_mask);
	nlvars = GET_BLK_MASK_NLVARS(attr_mask);
	fixed_nargs = GET_BLK_MASK_NARGS(attr_mask);
	actual_nargs = nargs - nargs_offset;
	excess_nargs = actual_nargs - fixed_nargs;

	if (actual_nargs < fixed_nargs || (!GET_BLK_MASK_VA(attr_mask) && actual_nargs > fixed_nargs))
	{
		HAK_LOG3(hak, HAK_LOG_IC | HAK_LOG_ERROR,
			"Error - wrong number of arguments to a function %O - expecting %zd, got %zd\n",
			op_func, fixed_nargs, nargs);
		hak_seterrnum(hak, HAK_ECALLARG);
		return -1;
	}

	/* create a new block context to clone op_func */
	hak_pushvolat(hak, (hak_oop_t*)&op_func);
	functx = make_context(hak, fixed_nargs + nrvars + nlvars + excess_nargs);
	hak_popvolat(hak);
	if (HAK_UNLIKELY(!functx)) return -1;

	functx->ip = HAK_SMOOI_TO_OOP(0);
	functx->req_nrets = HAK_SMOOI_TO_OOP(1);
	functx->attr_mask = op_func->attr_mask;
	functx->base = op_func;
	functx->home = op_func->home;
	functx->receiver = HAK_STACK_GETRCV(hak, nargs);

	/* copy the fixed arguments to the beginning of the variable part of the context block */
	for (i = 0, j = nargs_offset; i < fixed_nargs; i++, j++)
	{
		functx->slot[i] = HAK_STACK_GETARG(hak, nargs, j);
	}

	/* variable arguments. place them behind after local variables. */
	for (i = fixed_nargs + nrvars + nlvars ; j < nargs; i++, j++)
	{
		functx->slot[i] = HAK_STACK_GETARG(hak, nargs, j);
	}

	HAK_STACK_POPS(hak, nargs + 2); /* pop arguments, called function/block/method, and receiver */

	HAK_ASSERT(hak, (hak_oop_t)functx->home != hak->_nil);
	functx->sender = hak->active_context;

	*pnewctx = functx;
	return 0;
}

static HAK_INLINE int activate_function (hak_t* hak, hak_ooi_t nargs)
{
	int x;
	hak_oop_function_t op_func;
	hak_oop_context_t newctx;

	op_func = (hak_oop_function_t)HAK_STACK_GETOP(hak, nargs);
	HAK_ASSERT(hak, HAK_IS_FUNCTION(hak, op_func));

	x = __activate_function(hak, op_func, nargs, &newctx);
	if (HAK_UNLIKELY(x <= -1)) return -1;

	SWITCH_ACTIVE_CONTEXT(hak, newctx);
	return 0;
}

/* ------------------------------------------------------------------------- */
static HAK_INLINE int call_primitive (hak_t* hak, hak_ooi_t nargs)
{
	hak_oop_prim_t op_prim;

	op_prim = (hak_oop_prim_t)HAK_STACK_GETOP(hak, nargs);
	HAK_ASSERT(hak, HAK_IS_PRIM(hak, op_prim));
	HAK_ASSERT(hak, HAK_OBJ_GET_SIZE(op_prim) == HAK_PRIM_NAMED_INSTVARS);

	if (nargs < op_prim->min_nargs && nargs > op_prim->max_nargs)
	{
/* TODO: include a primitive name... */
		HAK_LOG3(hak, HAK_LOG_IC | HAK_LOG_ERROR,
			"Error - wrong number of arguments to a primitive - expecting %zd-%zd, got %zd\n",
			op_prim->min_nargs, op_prim->max_nargs, nargs);
		hak_seterrnum(hak, HAK_ECALLARG);
		return -1;
	}

	return ((hak_pfimpl_t)op_prim->impl)(hak, (hak_mod_t*)op_prim->mod, nargs);
}

/* ------------------------------------------------------------------------- */

static hak_oop_block_t find_imethod_in_class_noseterr (hak_t* hak, hak_oop_class_t _class, hak_oocs_t* name, hak_ooi_t* ivaroff, hak_oop_class_t* owner)
{
	hak_oop_t dic;

	dic = _class->mdic;
	HAK_ASSERT(hak, HAK_IS_NIL(hak, dic) || HAK_IS_DIC(hak, dic));

	if (HAK_LIKELY(!HAK_IS_NIL(hak, dic)))
	{
		hak_oop_cons_t ass;
		ass = (hak_oop_cons_t)hak_lookupdicforsymbol_noseterr(hak, (hak_oop_dic_t)dic, name);
		if (HAK_LIKELY(ass))
		{
			hak_oop_t val;
			val = HAK_CONS_CDR(ass);
			HAK_ASSERT(hak, HAK_IS_CONS(hak, val));
			if (!HAK_IS_NIL(hak, HAK_CONS_CDR(val)))
			{
				/* TODO: further check if it's a method block? */
				*owner = _class;
				*ivaroff = HAK_OOP_TO_SMOOI(_class->nivars_super);
				return (hak_oop_block_t)HAK_CONS_CDR(val); /* car - class method, cdr - instance method */
			}
		}
	}

	return HAK_NULL;
}

static hak_oop_block_t find_imethod_noseterr (hak_t* hak, hak_oop_class_t class_, hak_oop_t op_name, int to_super, hak_ooi_t* ivaroff, hak_oop_class_t* owner)
{
	hak_oocs_t name;

	HAK_ASSERT(hak, HAK_IS_CLASS(hak, class_));
	/*HAK_ASSERT(hak, HAK_IS_SYMBOL(hak, op_name));*/
	HAK_ASSERT(hak, HAK_OBJ_IS_CHAR_POINTER(op_name));

	name.ptr = HAK_OBJ_GET_CHAR_SLOT(op_name);
	name.len = HAK_OBJ_GET_SIZE(op_name);

	if (to_super)
	{
		class_ = (hak_oop_class_t)class_->superclass;
		if (!HAK_IS_CLASS(hak, class_)) return HAK_NULL;
	}

	do
	{
		hak_oop_block_t mth;
		mth = find_imethod_in_class_noseterr(hak, class_, &name, ivaroff, owner);
		if (mth) return mth;
		class_ = (hak_oop_class_t)class_->superclass;
	}
	while (HAK_IS_CLASS(hak, class_));

	return HAK_NULL;
}

static hak_oop_block_t find_cmethod_noseterr (hak_t* hak, hak_oop_class_t _class, hak_oop_t op_name, int to_super, hak_ooi_t* ivaroff, hak_oop_class_t* owner)
{
	hak_oocs_t name;
	hak_oop_class_t xclass;

/* TODO: implement method cache */
	HAK_ASSERT(hak, HAK_IS_CLASS(hak, _class));
	/*HAK_ASSERT(hak, HAK_IS_SYMBOL(hak, op_name));*/
	HAK_ASSERT(hak, HAK_OBJ_IS_CHAR_POINTER(op_name));

	name.ptr = HAK_OBJ_GET_CHAR_SLOT(op_name);
	name.len = HAK_OBJ_GET_SIZE(op_name);

	xclass = _class;
	if (to_super)
	{
		xclass = (hak_oop_class_t)xclass->superclass;
		if (!HAK_IS_CLASS(hak, xclass)) return HAK_NULL;
	}

	do
	{
		hak_oop_t dic;

		dic = xclass->mdic;
		HAK_ASSERT(hak, HAK_IS_NIL(hak, dic) || HAK_IS_DIC(hak, dic));

		if (HAK_LIKELY(!HAK_IS_NIL(hak, dic)))
		{
			hak_oop_cons_t ass;
			ass = (hak_oop_cons_t)hak_lookupdicforsymbol_noseterr(hak, (hak_oop_dic_t)dic, &name);
			if (HAK_LIKELY(ass))
			{
				hak_oop_t val;
				val = HAK_CONS_CDR(ass);
				HAK_ASSERT(hak, HAK_IS_CONS(hak, val));
				if (!HAK_IS_NIL(hak, HAK_CONS_CAR(val)))
				{
					/* TODO: further check if it's a method block? */
					*owner = xclass;
					/* ivaroff isn't useful for a class method but is useful for class instatiation method
					 * (INSTA bit on in the mask field) */
					*ivaroff = HAK_OOP_TO_SMOOI(xclass->nivars_super);
					return (hak_oop_block_t)HAK_CONS_CAR(val); /* car - class method, cdr - instance method */
				}
			}
		}
		xclass = (hak_oop_class_t)xclass->superclass;
	}
	while (HAK_IS_CLASS(hak, xclass));

	/* If the following two lines are uncommented, the class method of Class must be explicitly defined
	 *   fun Class:name() {...}
	 *   class X { }
	 * If name is defined as an instance method of Class, other classes can call 'name' as a class method.
	 *    X:name
	 * but Class itself can't call it as Class:name. This is possible only if 'fun Class::name()' is also
	 * defined.
	xclass = HAK_CLASSOF(hak, _class);
	if (xclass == _class) return HAK_NULL;
	*/

	/* find the instance method of the Class class as a class is an instance of the Class class. */
	/* TODO: may need to traverse up if Class is a subclass in some other Clss-related abstraction... */
	return find_imethod_in_class_noseterr(hak, (hak_oop_class_t)HAK_CLASSOF(hak, _class), &name, ivaroff, owner);
}

int hak_class_responds_to (hak_t* hak, hak_oop_t rcv, hak_oop_t msg)
{
	hak_oop_block_t mth_blk;
	hak_oop_class_t owner;
	hak_ooi_t ivaroff;

	HAK_ASSERT(hak, HAK_IS_CLASS(hak, rcv));
	mth_blk = find_cmethod_noseterr(hak, (hak_oop_class_t)rcv, msg, 0, &ivaroff, &owner);

	return mth_blk != HAK_NULL;
}

int hak_inst_responds_to (hak_t* hak, hak_oop_t rcv, hak_oop_t msg)
{
	hak_oop_block_t mth_blk;
	hak_oop_class_t _class, owner;
	hak_ooi_t ivaroff;

	_class = (hak_oop_class_t)HAK_CLASSOF(hak, rcv);
	HAK_ASSERT(hak, _class != HAK_NULL);
	HAK_ASSERT(hak, HAK_IS_CLASS(hak, _class));
	mth_blk = find_imethod_noseterr(hak, _class, msg, 0, &ivaroff, &owner);

	return mth_blk != HAK_NULL;
}

static HAK_INLINE int send_message (hak_t* hak, hak_oop_t rcv, hak_oop_t msg, int to_super, hak_ooi_t nargs, hak_ooi_t nrvars)
{
	hak_oop_block_t mth_blk;
	hak_oop_context_t newctx;
	hak_oop_class_t _class, owner;
	hak_ooi_t ivaroff;
	int x;

	HAK_ASSERT(hak, HAK_OBJ_IS_CHAR_POINTER(msg));
	/*HAK_ASSERT(hak, HAK_IS_SYMBOL(hak, msg));*/

/* ============================= */
/* TODO: implement methods cache */
/* ============================= */
	if (HAK_IS_CLASS(hak, rcv))
	{
		_class = (hak_oop_class_t)rcv;
		mth_blk = find_cmethod_noseterr(hak, _class, msg, to_super, &ivaroff, &owner);

		if (!mth_blk) goto msg_not_found;

		if (GET_BLK_MASK_INSTA(HAK_OOP_TO_SMOOI(mth_blk->attr_mask)))
		{
			hak_oop_t newrcv;

			hak_pushvolat(hak, (hak_oop_t*)&mth_blk);
			hak_pushvolat(hak, &msg);
			hak_pushvolat(hak, &rcv);
			newrcv = hak_instantiate(hak, (hak_oop_class_t)_class, HAK_NULL, 0);
			hak_popvolats(hak, 3);
			if (HAK_UNLIKELY(!newrcv)) return -1;

			HAK_STACK_SETRCV(hak, nargs, newrcv); /* prepare_new_context() will take this as a receiver */
		}
	}
	else
	{
		/*HAK_ASSERT(hak, HAK_IS_INSTANCE(hak, rcv));*/
		_class = (hak_oop_class_t)HAK_CLASSOF(hak, rcv);
		HAK_ASSERT(hak, _class != HAK_NULL);
		HAK_ASSERT(hak, HAK_IS_CLASS(hak, _class));
		mth_blk = find_imethod_noseterr(hak, _class, msg, to_super, &ivaroff, &owner);
		if (!mth_blk)
		{
		msg_not_found:
			hak_seterrbfmt(hak, HAK_ENOENT, "'%.*js' not found in %O", HAK_OBJ_GET_SIZE(msg), HAK_OBJ_GET_CHAR_SLOT(msg), _class);
			return -1;
		}
	}

	x = __activate_block(hak, mth_blk, nargs, nrvars, 1 /* is_msgsend */, ivaroff, &newctx);
	if (HAK_UNLIKELY(x <= -1)) return -1;

	/* update the method owner field of the new context created */
	newctx->owner = (hak_oop_t)owner;

	SWITCH_ACTIVE_CONTEXT(hak, newctx);
	return 0;
}

/* ------------------------------------------------------------------------- */

static HAK_INLINE int do_throw (hak_t* hak, hak_oop_t val, hak_ooi_t ip)
{
	hak_oop_context_t catch_ctx;
	hak_ooi_t catch_ip, clsp, sp;
	hak_oop_context_t c;

	if (HAK_EXSTACK_IS_EMPTY(hak))
	{
		hak_oop_function_t f;

		/* the exception stack is empty.
		 * clear the class stack if it is not empty */
		while (!HAK_CLSTACK_IS_EMPTY(hak)) HAK_CLSTACK_POP(hak);

		f = hak->active_function;
		if (f->dbgi != hak->_nil && ip >= 0)
		{
			hak_dbgi_t* dbgi;
			hak_loc_t loc;

			dbgi = (hak_dbgi_t*)HAK_OBJ_GET_BYTE_SLOT(f->dbgi);
			HAK_LOG3(hak, HAK_LOG_IC | HAK_LOG_WARN, "Warning - exception not handled %js:%zu - %O\n", (dbgi[ip].fname? dbgi[ip].fname: oocstr_dash), dbgi[ip].sline, val);
			HAK_MEMSET(&loc, 0, HAK_SIZEOF(loc));
			loc.file = dbgi[ip].fname;
			loc.line = dbgi[ip].sline;
			hak_seterrbfmtloc(hak, HAK_EEXCEPT, &loc, "exception not handled - %O", val);
			/* column number is not available */
		}
		else
		{
			HAK_LOG1(hak, HAK_LOG_IC | HAK_LOG_WARN, "Warning - exception not handled - %O", val);
			hak_seterrbfmt(hak, HAK_EEXCEPT, "exception not handled - %O", val);
		}

		/* output backtrace */
		HAK_LOG0(hak, HAK_LOG_IC | HAK_LOG_INFO, "[BACKTRACE]\n");
		c = hak->active_context;
		if ((hak_oop_t)c != hak->_nil && ip >= 0)
		{
			hak_ooi_t cip;

			cip = ip; /* use the given ip for the active context instead of the value from the ip field */
			do
			{
				f = c->base;
				if (f->dbgi != hak->_nil)
				{
					hak_dbgi_t* dbgi;
					const hak_ooch_t* fname;

					dbgi = (hak_dbgi_t*)HAK_OBJ_GET_BYTE_SLOT(f->dbgi);

					fname = dbgi[cip].fname;
					if (!fname && hak->c) fname = hak->c->cci_arg.name;

					/* TODO: include arguments? */
					HAK_LOG7(hak, HAK_LOG_IC | HAK_LOG_INFO, "  %.*js%js%.*js(%js:%zu)\n",
						(c->owner == hak->_nil? 0: HAK_OBJ_GET_SIZE(((hak_oop_class_t)c->owner)->name)),
						(c->owner == hak->_nil? oocstr_none: ((hak_oop_char_t)((hak_oop_class_t)c->owner)->name)->slot),
						(c->owner == hak->_nil? oocstr_none: oocstr_colon),
						(c->name == hak->_nil? 0: HAK_OBJ_GET_SIZE(((hak_oop_char_t)c->name))),
						(c->name == hak->_nil? oocstr_none: ((hak_oop_char_t)c->name)->slot),
						(fname? fname: oocstr_dash), dbgi[cip].sline);
				}
				c = c->sender;
				if ((hak_oop_t)c == hak->_nil) break;
				cip = HAK_OOP_TO_SMOOI(c->ip);
			} while (1);
		}

		/* exception not handled. terminate the active process */
		/*terminate_process(hak, hak->processor->active); <- the vm cleanup code will do this */
		return -1;
	}

	/* pop the exception stack to get information to rewind context */
	HAK_EXSTACK_POP_TO(hak, catch_ctx, catch_ip, clsp, sp);

	/* discard unfinished class definitions for the exception thrown.
	 *
	 * (try
	 *    (class X
	 *      (throw "exception")
	 *  catch (x)
	 *    (printf "exception %O\n" x)
	 * )
	 * 'throw' is triggered before the end of defintion of X is reached.
	 */
	HAK_CLSTACK_CHOP(hak, clsp);

	/* the below code is similar to do_return_from_block() */
	hak->ip = -1; /* mark context dead. saved into hak->active_context->ip in SWITCH_ACTIVE_CONTEXT */
	SWITCH_ACTIVE_CONTEXT(hak, catch_ctx);
	hak->ip = catch_ip; /* override the instruction pointer */

	hak->sp = sp; /* restore the stack pointer of the active process context */

	/* push the exception value to the stack */
	HAK_STACK_PUSH(hak, val);
	return 0;
}

/* ------------------------------------------------------------------------- */

static void supplement_errmsg (hak_t* hak, hak_ooi_t ip)
{
	if (hak->active_function->dbgi != hak->_nil)
	{
		hak_dbgi_t* dbgi;
		hak_loc_t orgloc = hak->errloc;
		const hak_ooch_t* orgmsg = hak_backuperrmsg(hak);
		hak_errnum_t orgnum = HAK_ERRNUM(hak);

		HAK_ASSERT(hak, HAK_IS_BYTEARRAY(hak, hak->active_function->dbgi));
		dbgi = (hak_dbgi_t*)HAK_OBJ_GET_BYTE_SLOT(hak->active_function->dbgi);

		orgloc.line = dbgi[ip].sline; /* update the line of the location at least */
		hak_seterrbfmtloc(hak, orgnum, &orgloc, "%js (%js:%zu)", orgmsg,
			(dbgi[ip].fname? dbgi[ip].fname: oocstr_dash), dbgi[ip].sline);

		/* no column info available */
	}
}

static int do_throw_with_internal_errmsg (hak_t* hak, hak_ooi_t ip)
{
	hak_oop_t ex;
/* TODO: consider throwing an exception object instead of a string? */
	ex = hak_makestring(hak, hak->errmsg.buf, hak->errmsg.len); /* TODO: include error location in the message? */
	if (HAK_UNLIKELY(!ex)) return -1;
	if (do_throw(hak, ex, ip) <= -1) return -1;
	return 0;
}

/* ------------------------------------------------------------------------- */


#if defined(ENABLE_SYSCMD)
/* EXPERIMENTAL CODE INTEGRATING EXTERNAL COMMANDS */

#include <unistd.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>

extern char **environ;

#define _PATH_DEFPATH "/usr/bin:/bin"

static int is_regular_executable_file_by_me(const char *path)
{
	struct stat st;
	if (stat(path, &st) == -1) return 0;
	return S_ISREG(st.st_mode) && access(path, X_OK) == 0; /* use eaccess instead?? */
}

static char* find_exec (hak_t* hak, const char *name)
{
	size_t lp, ln;
	char buf[PATH_MAX];
	const char *bp, *path, *p;

	bp = buf;

	/* Get the path we're searching. */
	if (!(path = getenv("PATH"))) path = _PATH_DEFPATH;

	ln = strlen(name);
	do
	{
		/* Find the end of this path element. */
		for (p = path; *path != 0 && *path != ':'; path++) ;

		/*
		 * It's a SHELL path -- double, leading and trailing colons
		 * mean the current directory.
		 */
		if (p == path)
		{
			p = ".";
			lp = 1;
		}
		else
		{
			lp = path - p;
		}

		/*
		 * If the path is too long complain.  This is a possible
		 * security issue; given a way to make the path too long
		 * the user may execute the wrong program.
		 */
		if (lp + ln + 2 > sizeof(buf)) continue;

		memcpy(buf, p, lp);
		buf[lp] = '/';
		memcpy(buf + lp + 1, name, ln);
		buf[lp + ln + 1] = '\0';

		if (is_regular_executable_file_by_me(bp)) return strdup(bp);

	}
	while (*path++ == ':'); /* Otherwise, *path was NUL */


done:
	hak_seterrbfmt(hak, HAK_ENOENT, "callable %hs not found", name);
	return HAK_NULL;
}


static HAK_INLINE int exec_syscmd (hak_t* hak, hak_ooi_t nargs)
{
	hak_oop_word_t op_cmd;
	hak_bch_t* cmd = HAK_NULL;
	hak_bch_t* xcmd = HAK_NULL;

	op_cmd = (hak_oop_word_t)HAK_STACK_GETOP(hak, nargs);
	/*HAK_ASSERT(hak, HAK_IS_STRING(hak, op_cmd) || HAK_IS_SYMBOL(hak, op_cmd));*/
	HAK_ASSERT(hak, HAK_OBJ_IS_CHAR_POINTER(op_cmd));

	if (HAK_OBJ_GET_SIZE(op_cmd) == 0 || hak_count_oocstr(HAK_OBJ_GET_CHAR_SLOT(op_cmd)) != HAK_OBJ_GET_SIZE(op_cmd))
	{
		/* '\0' is contained in the middle */
		hak_seterrbfmt(hak, HAK_EINVAL, "invalid callable %O", op_cmd);
		goto oops;
	}

	cmd = hak_dupootobcstr(hak, HAK_OBJ_GET_CHAR_SLOT(op_cmd), HAK_NULL);
	if (!cmd) goto oops;

	if (hak_find_bchar_in_bcstr(cmd, '/'))
	{
		if (!is_regular_executable_file_by_me(cmd))
		{
			hak_seterrbfmt(hak, HAK_ECALL, "cannot execute %O", op_cmd);
			goto oops;
		}

		xcmd = cmd;
	}
	else
	{
		xcmd = find_exec(hak, cmd);
		if (!xcmd) goto oops;
	}

{ /* TODO: make it a callback ... */
	pid_t pid;
	int status;

	pid = fork();
	if (pid == -1) goto oops;

/* TODO: set a new process group / session leader??? */

	if (pid == 0)
	{
		hak_bch_t** argv;
		hak_ooi_t i;

		/* TODO: close file descriptors??? */
		argv = (hak_bch_t**)hak_allocmem(hak, (nargs + 2) * HAK_SIZEOF(*argv));
		if (HAK_LIKELY(argv))
		{
			argv[0] = cmd;
HAK_DEBUG1 (hak, "NARG %d\n", (int)nargs);
			for (i = 0; i < nargs;)
			{
				hak_oop_t ta = HAK_STACK_GETARG(hak, nargs, i);
/* TODO: check if an argument is a string or a symbol */
				if (HAK_OOP_IS_SMOOI(ta))
				{
/* TODO: rewrite this part */
					hak_bch_t tmp[64];
					snprintf (tmp, sizeof(tmp), "%ld", (long int)HAK_OOP_TO_SMOOI(ta));
					argv[++i] = hak_dupbchars(hak, tmp, strlen(tmp));
				}
				else
				{
					argv[++i] = hak_dupootobchars(hak, HAK_OBJ_GET_CHAR_SLOT(ta), HAK_OBJ_GET_SIZE(ta), HAK_NULL);
				}
				/*HAK_DEBUG2 (hak, "ARG %d -> %hs\n", (int)i - 1, argv[i]);*/
			}
			argv[nargs + 1] = HAK_NULL;
			execvp (xcmd, argv);
		}

		if (cmd) hak_freemem(hak, cmd);
		if (xcmd && xcmd != cmd) hak_freemem(hak, xcmd);
		_exit (255);
	}

	waitpid (pid, &status, 0); /* TOOD: enhance this waiting */

	HAK_STACK_SETRET(hak, nargs, HAK_SMOOI_TO_OOP(WEXITSTATUS(status)));
}

	hak_freemem(hak, cmd);
	if (xcmd != cmd) hak_freemem(hak, xcmd);
	return 0;

oops:
	if (cmd) hak_freemem(hak, cmd);
	if (xcmd && xcmd != cmd) hak_freemem(hak, xcmd);
	return -1;
}

#endif

/* ------------------------------------------------------------------------- */
static hak_oop_process_t start_initial_process (hak_t* hak, hak_oop_context_t ctx)
{
	hak_oop_process_t proc;

	/* there must be no active process when this function is called */
	HAK_ASSERT(hak, hak->processor->runnable.count == HAK_SMOOI_TO_OOP(0));
	HAK_ASSERT(hak, hak->processor->active == hak->nil_process);

	proc = make_process(hak, ctx);
	if (HAK_UNLIKELY(!proc)) return HAK_NULL;

	/* skip RUNNABLE and go to RUNNING */
	chain_into_processor(hak, proc, HAK_PROCESS_STATE_RUNNING);
	hak->processor->active = proc;

	/* do something that resume_process() would do with less overhead */
	HAK_ASSERT(hak, (hak_oop_t)proc->current_context != hak->_nil);
	HAK_ASSERT(hak, proc->current_context == proc->initial_context);
	SWITCH_ACTIVE_CONTEXT(hak, proc->current_context);

	return proc;
}

static int start_initial_process_and_context (hak_t* hak, hak_ooi_t initial_ip, hak_ooi_t nlvars)
{
	hak_oop_context_t ctx;
	hak_oop_process_t proc;
	hak_ooi_t attr_mask;

	attr_mask = ENCODE_BLK_MASK(0, 0, 0, 0, nlvars);
	/* create the initial context over the initial function */
	ctx = make_context(hak, nlvars);
	if (HAK_UNLIKELY(!ctx)) return -1;

	hak->ip = initial_ip;
	hak->sp = -1;

	ctx->ip = HAK_SMOOI_TO_OOP(initial_ip);
	ctx->req_nrets = HAK_SMOOI_TO_OOP(1);
	ctx->attr_mask = HAK_SMOOI_TO_OOP(attr_mask);
	ctx->home = hak->initial_function->home; /* this should be nil */
	ctx->sender = (hak_oop_context_t)hak->_nil; /* the initial context has nil in the sender field */
	ctx->base = hak->initial_function;
	ctx->receiver = hak->_nil; /* TODO: change this? keep this in sync with the dummy receiver used in the call instruction generated for xlist */
	HAK_ASSERT(hak, (hak_oop_t)ctx->home == hak->_nil);

	/* [NOTE]
	 *  the sender field of the initial context is nil.
	 *  especially, the fact that the sender field is nil is used by
	 *  the main execution loop for breaking out of the loop */

	HAK_ASSERT(hak, hak->active_context == HAK_NULL);

	/* hak_gc() uses hak->processor when hak->active_context
	 * is not NULL. at this poinst, hak->processor should point to
	 * an instance of ProcessScheduler. */
	HAK_ASSERT(hak, (hak_oop_t)hak->processor != hak->_nil);
	HAK_ASSERT(hak, hak->processor->runnable.count == HAK_SMOOI_TO_OOP(0));

	/* start_initial_process() calls the SWITCH_ACTIVE_CONTEXT() macro.
	 * the macro assumes a non-null value in hak->active_context.
	 * let's force set active_context to ctx directly. */
	hak->active_context = ctx;

	hak_pushvolat(hak, (hak_oop_t*)&ctx);
	proc = start_initial_process(hak, ctx);
	hak_popvolat(hak);
	if (HAK_UNLIKELY(!proc)) return -1;

	/* the stack must contain nothing as it should emulate the expresssion - (the-initial-function).
	 * for a normal function call, the function object and arguments are pushed by the caller.
	 * __activate_function() creates a new context and pops the function object and arguments off the stack.
	 * at this point, it should be as if the pop-off has been completed.
	 * because this is the very beginning, nothing should exist in the stack */
	HAK_ASSERT(hak, hak->sp == -1);
	HAK_ASSERT(hak, hak->sp == HAK_OOP_TO_SMOOI(proc->sp));

	HAK_ASSERT(hak, proc == hak->processor->active);
	hak->initial_context = proc->initial_context;
	HAK_ASSERT(hak, hak->initial_context == hak->active_context);

	return 0;
}

/* ------------------------------------------------------------------------- */

static HAK_INLINE int switch_process_if_needed (hak_t* hak)
{
	if (hak->sem_heap_count > 0)
	{
		/* handle timed semaphores */
		hak_ntime_t ft, now;

		vm_gettime(hak, &now);

		do
		{
			HAK_ASSERT(hak, hak->sem_heap[0]->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_TIMED));
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(hak->sem_heap[0]->u.timed.ftime_sec));
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(hak->sem_heap[0]->u.timed.ftime_nsec));

			HAK_INIT_NTIME (&ft,
				HAK_OOP_TO_SMOOI(hak->sem_heap[0]->u.timed.ftime_sec),
				HAK_OOP_TO_SMOOI(hak->sem_heap[0]->u.timed.ftime_nsec)
			);

			if (HAK_CMP_NTIME(&ft, (hak_ntime_t*)&now) <= 0)
			{
				hak_oop_process_t proc;

			signal_timed:
				/* waited long enough. signal the semaphore */

				proc = signal_semaphore(hak, hak->sem_heap[0]);
				/* [NOTE] no hak_pushvolat() on proc. no GC must occur
				 *        in the following line until it's used for
				 *        wake_process() below. */
				delete_from_sem_heap(hak, 0); /* hak->sem_heap_count is decremented in delete_from_sem_heap() */

				/* if no process is waiting on the semaphore,
				 * signal_semaphore() returns hak->_nil. */

				if (hak->processor->active == hak->nil_process && (hak_oop_t)proc != hak->_nil)
				{
					/* this is the only runnable process.
					 * switch the process to the running state.
					 * it uses wake_process() instead of
					 * switch_to_process() as there is no running
					 * process at this moment */

				#if defined(HAK_DEBUG_VM_PROCESSOR) && (HAK_DEBUG_VM_PROCESSOR >= 2)
					HAK_LOG2(hak, HAK_LOG_IC | HAK_LOG_DEBUG, "Processor - switching to a process[%zd] while no process is active - total runnables %zd\n", HAK_OOP_TO_SMOOI(proc->id), HAK_OOP_TO_SMOOI(hak->processor->runnable.count));
				#endif

					HAK_ASSERT(hak, proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNABLE));
					HAK_ASSERT(hak, proc == hak->processor->runnable.last); /* resume_process() appends to the runnable list */
				#if 0
					wake_process(hak, proc); /* switch to running */
					hak->proc_switched = 1;
				#else
					switch_to_process_from_nil(hak, proc);
				#endif
				}
			}
			else if (hak->processor->active == hak->nil_process)
			{
				/* no running process. before firing time. */
				HAK_SUB_NTIME (&ft, &ft, (hak_ntime_t*)&now);

				if (hak->sem_io_wait_count > 0)
				{
					/* no running process but io semaphore being waited on */
					vm_muxwait(hak, &ft);

					/* exit early if a process has been woken up.
					 * the break in the else part further down will get hit
					 * eventually even if the following line doesn't exist.
					 * having the following line causes to skip firing the
					 * timed semaphore that would expire between now and the
					 * moment the next inspection occurs. */
					if (hak->processor->active != hak->nil_process) goto switch_to_next;
				}
				else
				{
					int halting;

#if defined(ENABLE_GCFIN)
					/* no running process, no io semaphore */
					if ((hak_oop_t)hak->sem_gcfin != hak->_nil && hak->sem_gcfin_sigreq) goto signal_sem_gcfin;
#endif
					halting = vm_sleep(hak, &ft);

					if (halting)
					{
						vm_gettime(hak, &now);
						goto signal_timed;
					}
				}
				vm_gettime(hak, &now);
			}
			else
			{
				/* there is a running process. go on */
				break;
			}
		}
		while (hak->sem_heap_count > 0 && !hak->abort_req);
	}

	if (hak->sem_io_wait_count > 0)
	{
		if (hak->processor->active == hak->nil_process)
		{
			hak_ntime_t ft;

			HAK_ASSERT(hak, hak->processor->runnable.count == HAK_SMOOI_TO_OOP(0));
			/* no running process while there is an io semaphore being waited for */

#if defined(ENABLE_GCFIN)
			if ((hak_oop_t)hak->sem_gcfin != hak->_nil && hak->sem_gcfin_sigreq) goto signal_sem_gcfin;
#endif

			if (hak->processor->suspended.count == HAK_SMOOI_TO_OOP(0))
			{
				/* no suspended process. the program is buggy or is probably being terminated forcibly.
				 * the default signal handler may lead to this situation. */
				hak->abort_req = 1;
			}
			else
			{
				do
				{
					HAK_INIT_NTIME (&ft, 3, 0); /* TODO: use a configured time */
					vm_muxwait(hak, &ft);
				}
				while (hak->processor->active == hak->nil_process && !hak->abort_req);
			}
		}
		else
		{
			/* well, there is a process waiting on one or more semaphores while
			 * there are other normal processes to run. check IO activities
			 * before proceeding to handle normal process scheduling */

			/* [NOTE] the check with the multiplexer may happen too frequently
			 *       because this is called everytime process switching is requested.
			 *       the actual callback implementation should try to avoid invoking
			 *       actual system calls too frequently for less overhead. */
			vm_muxwait(hak, HAK_NULL);
		}
	}

#if defined(ENABLE_GCFIN)
	if ((hak_oop_t)hak->sem_gcfin != hak->_nil)
	{
		hak_oop_process_t proc;

		if (hak->sem_gcfin_sigreq)
		{
		signal_sem_gcfin:
			HAK_LOG0 (hak, HAK_LOG_IC | HAK_LOG_DEBUG, "Signaled GCFIN semaphore\n");
			proc = signal_semaphore(hak, hak->sem_gcfin);

			if (hak->processor->active == hak->nil_process && (hak_oop_t)proc != hak->_nil)
			{
				HAK_ASSERT(hak, proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNABLE));
				HAK_ASSERT(hak, proc == hak->processor->runnable.first);
				switch_to_process_from_nil(hak, proc);
			}

			hak->sem_gcfin_sigreq = 0;
		}
		else
		{
			/* the gcfin semaphore signalling is not requested and there are
			 * no runnable processes nor no waiting semaphores. if there is
			 * process waiting on the gcfin semaphore, i will just schedule
			 * it to run by calling signal_semaphore() on hak->sem_gcfin.
			 */
			/* TODO: check if this is the best implementation practice */
			if (hak->processor->active == hak->nil_process)
			{
				/* there is no active process. in most cases, the only process left
				 * should be the gc finalizer process started in the System>>startup.
				 * if there are other suspended processes at this point, the processes
				 * are not likely to run again.
				 *
				 * imagine the following single line program that creates a process
				 * but never start it.
				 *
				 *    method(#class) main { | p |  p := [] newProcess. }
				 *
				 * the gc finalizer process and the process assigned to p exist.
				 * when the code reaches here, the 'p' process still is alive
				 * despite no active process nor no process waiting on timers
				 * and semaphores. so when the entire program terminates, there
				 * might still be some suspended processes that are not possible
				 * to schedule.
				 */

				HAK_LOG4(hak, HAK_LOG_IC | HAK_LOG_DEBUG,
					"Signaled GCFIN semaphore without gcfin signal request - total %zd runnable/running %zd suspended %zd - sem_io_wait_count %zu\n",
					HAK_OOP_TO_SMOOI(hak->processor->total_count),
					HAK_OOP_TO_SMOOI(hak->processor->runnable.count),
					HAK_OOP_TO_SMOOI(hak->processor->suspended.count),
					hak->sem_io_wait_count);
				proc = signal_semaphore(hak, hak->sem_gcfin);
				if ((hak_oop_t)proc != hak->_nil)
				{
					HAK_ASSERT(hak, proc->state == HAK_SMOOI_TO_OOP(HAK_PROCESS_STATE_RUNNABLE));
					HAK_ASSERT(hak, proc == hak->processor->runnable.first);
					hak->_system->cvar[2] = hak->_true; /* set gcfin_should_exit in System to true. if the postion of the class variable changes, the index must get changed, too. */
					switch_to_process_from_nil(hak, proc); /* sechedule the gc finalizer process */
				}
			}
		}
	}
#endif


#if 0
	while (hak->sem_list_count > 0)
	{
		/* handle async signals */
		--hak->sem_list_count;
		signal_semaphore(hak, hak->sem_list[hak->sem_list_count]);
		if (hak->processor->active == hak->nil_process)
		{suspended process
		}
	}
	/*
	if (semaphore heap has pending request)
	{
		signal them...
	}*/
#endif

	if (hak->processor->active == hak->nil_process)
	{
		/* no more waiting semaphore and no more process */
		HAK_ASSERT(hak, hak->processor->runnable.count = HAK_SMOOI_TO_OOP(0));
		HAK_LOG0 (hak, HAK_LOG_IC | HAK_LOG_DEBUG, "No more runnable process\n");

		if (HAK_OOP_TO_SMOOI(hak->processor->suspended.count) > 0)
		{
			/* there exist suspended processes while no processes are runnable.
			 * most likely, the running program contains process/semaphore related bugs */
			HAK_LOG1(hak, HAK_LOG_IC | HAK_LOG_WARN,
				"Warning - %zd suspended process(es) found in process switcher - check your program\n",
				HAK_OOP_TO_SMOOI(hak->processor->suspended.count));
		}
		return 0;
	}

switch_to_next:
	/* TODO: implement different process switching scheme - time-slice or clock based??? */
#if defined(HAK_EXTERNAL_PROCESS_SWITCH)
	if (hak->switch_proc)
	{
#endif
		if (!hak->proc_switched)
		{
			switch_to_next_runnable_process(hak);
			hak->proc_switched = 0;
		}
#if defined(HAK_EXTERNAL_PROCESS_SWITCH)
		hak->switch_proc = 0;
	}
	else hak->proc_switched = 0;
#endif

	return 1;
}

/* ------------------------------------------------------------------------- */

static HAK_INLINE int do_return_from_block (hak_t* hak)
{
	/*if (hak->active_context == hak->processor->active->initial_context)*/
	if ((hak_oop_t)hak->active_context->home == hak->_nil)
	{
		/* the active context to return from is an initial context of
		 * the active process. let's terminate the process.
		 * the initial context has been forged over the initial function
		 * in start_initial_process_and_context() */
		HAK_ASSERT(hak, (hak_oop_t)hak->active_context->sender == hak->_nil);
		hak->active_context->ip = HAK_SMOOI_TO_OOP(-1); /* mark context dead */
		terminate_process(hak, hak->processor->active);
		return 1; /* indiate process termination */
	}
	else
	{
		/* The compiler produces the class_exit instruction and the try_exit instruction
		 * for return, break, continue in a class defintion scope and in a try-catch scope
		 * respectively.

		[CASE 1]
		(class X
			; ....
			(return 20) ;  the class defintion isn't over, but return is executed,
			; ....
		)

		[CASE 2]
		(try
			(class C
				(return 200)
				(printf "============================\n"))
		catch (e)
			(printf "EXCEPTION => %O\n" e)
		)

		[CASE 3]
		(class C
			(try
			    (return 99)
			catch (e)
			    (printf "EXCEPTOIN => %O\n" e)
			)
			(printf "============================\n")
		)

		[CASE 4]
		(try
			(class C
				(try
				    (return 99)
				catch (e)
				    (printf "EXCEPTOIN => %O\n" e)
				)
				(printf "============================\n")
			)
		catch (e)
			(printf "EXCEPTOIN => %O\n" e)
		)

		[CASE 5]
		(try
			(class D
				(class C
					(try
					    (return 99)
					catch (e)
					    (printf "EXCEPTOIN => %O\n" e)
					)
					(printf "============================\n")
				)
			}
		catch (e)
			(printf "EXCEPTOIN => %O\n" e)
		)

		 * the actual return instruction handler doesn't need to care about the
		 * class stack and exception stack.
		 */

		HAK_ASSERT(hak, (hak_oop_t)hak->active_context->sender != hak->_nil);
		if (HAK_UNLIKELY(hak->active_context->sender->ip == HAK_SMOOI_TO_OOP(-1)))
		{
			HAK_LOG0 (hak, HAK_LOG_IC | HAK_LOG_ERROR, "Error - cannot return to dead context\n");
			hak_seterrbfmt(hak, HAK_EINTERN, "unable to return to dead context"); /* TODO: can i make this error catchable at the hak level? */
			return -1;
		}

		/* it is a normal block return as the active block context
		 * is not the initial context of a process */
		hak->ip = -1; /* mark context dead. saved into hak->active_context->ip in SWITCH_ACTIVE_CONTEXT */
		SWITCH_ACTIVE_CONTEXT(hak, (hak_oop_context_t)hak->active_context->sender);
		return 0; /* normal return */
	}
}

static HAK_INLINE int do_return_from_home (hak_t* hak, hak_oop_t return_value, hak_ooi_t ip)
{
#if 0
	/* if (hak->active_context == hak->processor->active->initial_context) // read the interactive mode note below... */
	if ((hak_oop_t)hak->active_context->home == hak->_nil)
	{
		/* returning from the intial context.
		 *  (return-from-home 999)
		 * the return-from-home is executed in the initial context */
		HAK_ASSERT(hak, (hak_oop_t)hak->active_context->sender == hak->_nil);
		hak->active_context->ip = HAK_SMOOI_TO_OOP(-1); /* mark the active context dead */

		if (hak->sp >= 0)
		{
			/* return-from-home has been called from where it shouldn't be. for instance,
			 *  (printf "xxx %d\n" (return-from-home 999))
			 *  -----------------------------------------------
			 *  (if (>  19 (return-from-home 20)) 30) */
			HAK_LOG1(hak, HAK_LOG_IC | HAK_LOG_WARN, "Warning - stack not empty on return-from-home - SP %zd\n", hak->sp); /* TODO: include line number and file name */
		}

		terminate_process(hak, hak->processor->active);
	}
	/*else if (hak->active_context->home == hak->processor->active->initial_context) // read the interactive mode note below...*/
	else if ((hak_oop_t)hak->active_context->home->home == hak->_nil)
	{
		/* non-local return out of the initial context
		 *  (defun y(x) (return-from-home (* x x)))
		 *  (y 999) */

		/* [NOTE]
		 * in the interactive mode, a new initial context/function/process is created
		 * for each expression (as implemented bin/main.c)
		 * hak->active_context may be the intial context of the previous expression.
		 *   (defun y(x) (return-from-home (* x x))) <-- initial context
		 *   (y 999) <- another initial context
		 * when y is called from the second initial context, the home context to return
		 * from the the first initial context. comparing hak->active_context->home against
		 * hak->initial_context doesn't return true in this case.
		 */

		HAK_ASSERT(hak, (hak_oop_t)hak->active_context->home->sender == hak->_nil);
		hak->active_context->home->ip = HAK_SMOOI_TO_OOP(-1); /* mark that the home context has returned */

		if (hak->sp >= 0)
		{
			/* return-from-home has been called from where it shouldn't be
			 *  (defun y(x) (return-from-home (* x x)))
			 *  (printf "xxx %d\n" (y 999)) */
			HAK_LOG1(hak, HAK_LOG_IC | HAK_LOG_WARN, "Warning - stack not empty on non-local return-from-home - SP %zd\n", hak->sp); /* TODO: include line number and file name */
		}

		terminate_process(hak, hak->processor->active);
	}
	else
	{
		/*
		(defun f(x)
			(defun y(x) (return-from-home (* x  x)))
			(y x)
			(printf "this line must not be printed\n");
		)
		(printf "%d\n" (f 90)) ; this should print 8100.
		(y 10); this ends up with the "unable to return from dead context" error.
		*/
		HAK_ASSERT(hak, hak->active_context != hak->processor->active->initial_context);
		HAK_ASSERT(hak, (hak_oop_t)hak->active_context->home->sender != hak->_nil);

		if (hak->active_context->home->ip == HAK_SMOOI_TO_OOP(-1))
		{
			HAK_LOG0 (hak, HAK_LOG_IC | HAK_LOG_ERROR, "Error - cannot return from dead context\n");
			hak_seterrbfmt(hak, HAK_EINTERN, "unable to return from dead context"); /* TODO: can i make this error catchable at the hak level? */
			return -1;
		}

		hak->active_context->home->ip = HAK_SMOOI_TO_OOP(-1); /* mark that the home context has returned */
		hak->ip = -1; /* mark that the active context has returned. committed to hak->active_context->ip in SWITCH_ACTIVE_CONTEXT() */
		SWITCH_ACTIVE_CONTEXT(hak, hak->active_context->home->sender);

		/* push the return value to the stack of the final active context */
		HAK_STACK_PUSH(hak, return_value);

	#if 0
		/* stack dump */
		HAK_DEBUG1 (hak, "****** non local returning %O\n", return_value);
		{
			int i;
			for (i = hak->sp; i >= 0; i--)
			{
				HAK_DEBUG2 (hak, "STACK[%d] => %O\n", i, HAK_STACK_GET(hak, i));
			}
		}
	#endif
	}

#else

	/* this part implements the non-local return by traversing the call chain
	 * until the sender of the home context is reached.
	 * it is slower than immediat return from the home context but detetts
	 * dead context better */

	if ((hak_oop_t)hak->active_context->home == hak->_nil)
	{
		/* non-local return from the intial context.
		 *  (return-from-home 999)
		 */

		/* the current active context must be the initial context of the active process */
		HAK_ASSERT(hak, hak->active_context == hak->processor->active->initial_context);
		HAK_ASSERT(hak, (hak_oop_t)hak->active_context->sender == hak->_nil);

		hak->active_context->ip = HAK_SMOOI_TO_OOP(-1); /* mark the active context dead */

	term_proc:
		if (hak->sp >= 0)
		{
			/* return-from-home has been called from where it shouldn't be. for instance,
			 *  (printf "xxx %d\n" (return-from-home 999))
			 *  -----------------------------------------------
			 *  (if (>  19 (return-from-home 20)) 30) */
			HAK_LOG1(hak, HAK_LOG_IC | HAK_LOG_WARN, "Warning - stack not empty on return-from-home - SP %zd\n", hak->sp); /* TODO: include line number and file name */
		}

		/* as the process is terminated here, the nonempty stack or not invalidating the
		 * intermediates contexts deson't really matter. */
		terminate_process(hak, hak->processor->active);
	}
	else
	{
		hak_oop_context_t sender, home, ctx;

		home = hak->active_context->home;
		sender = hak->active_context->home->sender;

		/* check if the home context is in the current call chain */
		ctx = hak->active_context;
		while ((hak_oop_t)ctx != hak->_nil)
		{
			ctx = ctx->sender;
			if (ctx == home) goto do_return;
		}

		if (hak->active_function->dbgi != hak->_nil)
		{
			hak_dbgi_t* dbgi = (hak_dbgi_t*)HAK_OBJ_GET_BYTE_SLOT(hak->active_function->dbgi);
			HAK_LOG2(hak, HAK_LOG_IC | HAK_LOG_ERROR, "Error - cannot return from dead context - throwing an exception (%js:%zu)\n", (dbgi[ip].fname? dbgi[ip].fname: oocstr_dash), dbgi[ip].sline);
		}
		else
		{
			HAK_LOG0 (hak, HAK_LOG_IC | HAK_LOG_ERROR, "Error - cannot return from dead context - throwing an exception\n");
		}
		hak_seterrbfmt(hak, HAK_EINTERN, "unable to return from dead context"); /* TODO: can i make this error catchable at the hak level? */
		return do_throw_with_internal_errmsg(hak, ip);

	do_return:
		while (hak->active_context != home)
		{
			hak->ip = -1; /* mark context dead. saved into hak->active_context->ip in SWITCH_ACTIVE_CONTEXT */
			SWITCH_ACTIVE_CONTEXT(hak, (hak_oop_context_t)hak->active_context->sender);
		}

		if (HAK_UNLIKELY((hak_oop_t)sender == hak->_nil))
		{
			/* non-local return out of the initial context
			 *   (defun y(x) (return-from-home (* x x)))
			 *   (y 999)
			 * when y is activated, y's home context is itself. but the
			 *
			 * [NOTE]
			 * in the interactive mode, a new initial context/function/process is created
			 * for each expression (as implemented bin/main.c)
			 * hak->active_context may be the intial context of the previous expression.
			 *   (defun y(x) (return-from-home (* x x))) <-- initial context
			 *   (y 999) <- another initial context
			 * when y is called from the second initial context, the home context to return
			 * from the the first initial context. comparing hak->active_context->home against
			 * hak->initial_context doesn't return true in this case. */
			HAK_ASSERT(hak, (hak_oop_t)home->home == hak->_nil);
			HAK_ASSERT(hak, (hak_oop_t)hak->active_context->sender == hak->_nil);

			home->ip = HAK_SMOOI_TO_OOP(-1); /* mark the home context dead */
			goto term_proc;
		}

		HAK_ASSERT(hak, hak->active_context->sender == sender);
		hak->ip = -1; /* mark context dead. saved into hak->active_context->ip in SWITCH_ACTIVE_CONTEXT */
		SWITCH_ACTIVE_CONTEXT(hak, (hak_oop_context_t)hak->active_context->sender);
		HAK_STACK_PUSH(hak, return_value);
	}
#endif

	return 0;
}

/* ------------------------------------------------------------------------- */

static void xma_dumper (void* ctx, const char* fmt, ...)
{
	va_list ap;
	va_start (ap, fmt);
	hak_logbfmtv ((hak_t*)ctx, HAK_LOG_IC | HAK_LOG_INFO, fmt, ap);
	va_end (ap);
}

static hak_oop_t fetch_numeric_rcv_slot (hak_t* hak, hak_oop_t rcv, hak_oow_t b1)
{
	hak_oow_t w;
	hak_obj_type_t rcv_type;

	rcv_type = (hak_obj_type_t)HAK_OBJ_GET_FLAGS_TYPE(rcv);
	switch (rcv_type)
	{
		case HAK_OBJ_TYPE_CHAR:
			w = ((hak_oop_char_t)rcv)->slot[b1];
			return HAK_CHAR_TO_OOP(w);

		case HAK_OBJ_TYPE_BYTE:
			w = ((hak_oop_byte_t)rcv)->slot[b1];
			return HAK_SMOOI_TO_OOP(w);

		case HAK_OBJ_TYPE_HALFWORD:
			w = ((hak_oop_halfword_t)rcv)->slot[b1];
			return HAK_SMOOI_TO_OOP(w);

		case HAK_OBJ_TYPE_WORD:
			w = ((hak_oop_word_t)rcv)->slot[b1];
			return hak_oowtoint(hak, w);

		default:
			hak_seterrbfmt(hak, HAK_EINTERN, "internal error - invalid receiver type in fetching numeric slot value - %d", rcv_type);
			return HAK_NULL;
	}
}

static int store_into_numeric_rcv_slot (hak_t* hak, hak_oop_t rcv, hak_oow_t b1, hak_oop_t v)
{
	hak_oow_t w;
	hak_obj_type_t rcv_type;

	if (HAK_OOP_IS_CHAR(v)) w = HAK_OOP_TO_CHAR(v);
	else if (hak_inttooow(hak, v, &w) <= 0) return -1;

	rcv_type = (hak_obj_type_t)HAK_OBJ_GET_FLAGS_TYPE(rcv);
	switch (rcv_type)
	{
		case HAK_OBJ_TYPE_CHAR:
			((hak_oop_char_t)rcv)->slot[b1] = (hak_ooch_t)(hak_oochu_t)w;
			break;

		case HAK_OBJ_TYPE_BYTE:
			((hak_oop_byte_t)rcv)->slot[b1] = (hak_oob_t)w;
			break;

		case HAK_OBJ_TYPE_HALFWORD:
			((hak_oop_halfword_t)rcv)->slot[b1] = (hak_oohw_t)w;
			break;

		case HAK_OBJ_TYPE_WORD:
			((hak_oop_word_t)rcv)->slot[b1] = w;
			break;

		default:
			hak_seterrbfmt(hak, HAK_EINTERN, "internal error - invalid receiver type in storing in numeric slot - %d", rcv_type);
			return -1;
	}

	return 0;
}

static int execute (hak_t* hak)
{
	hak_oob_t bcode;
	hak_oow_t b1, b2;
	hak_oop_t return_value;
	hak_ooi_t fetched_instruction_pointer;

#if defined(HAK_PROFILE_VM)
	hak_uintmax_t inst_counter = 0;
#endif

	HAK_ASSERT(hak, hak->active_context != HAK_NULL);

	hak->abort_req = 0;
	if (vm_startup(hak) <= -1) return -1;
	hak->proc_switched = 0;

	hak->gci.lazy_sweep = 1; /* TODO: make it configurable?? */
	HAK_INIT_NTIME (&hak->gci.stat.alloc, 0, 0);
	HAK_INIT_NTIME (&hak->gci.stat.mark, 0, 0);
	HAK_INIT_NTIME (&hak->gci.stat.sweep, 0, 0);

	fetched_instruction_pointer = -1;

	while (1)
	{
		/* stop requested or no more runnable process */
		if (hak->abort_req < 0) goto oops;
		if (hak->abort_req > 0 || (!hak->no_proc_switch && switch_process_if_needed(hak) == 0)) break;

		if (HAK_UNLIKELY(hak->ip < 0 || hak->ip >= HAK_FUNCTION_GET_CODE_SIZE(hak->active_function)))
		{
			if (hak->ip < 0)
			{
				/* do_return_from_home() implements a simple check against a dead context.
				 * but the check is far from perfect. there are many ways to return from an
				 * active context and enter a dead context thereafter.
					(defun t(f)
						(set q (fun()
							(printf "hello word\n")
							(return-from-home 200)
						))
						(f)
					)
					(defun x()
						(t (fun() (return-from-home 100)))
						(printf ">>>>>>>>>>>>>>>>>>>>>>>>\n");
					)
					(x) ; x is exited by (return-from-home 100) triggered by (f)
					(printf "------------------------\n")
					(q) ; (return-from-home 200) exits t and since t is called from x, it flows back to the dead x.
				 */
				HAK_DEBUG1 (hak, "Stopping execution as a dead context gets active  - IP %zd\n", hak->ip);
			}
			else
			{
				HAK_DEBUG2 (hak, "Stopping execution as IP reached the end of bytecode(%zu) - IP %zd\n", hak->code.bc.len, hak->ip);
			}
			return_value = hak->_nil;
			goto handle_return;
		}

		fetched_instruction_pointer = hak->ip;
		FETCH_BYTE_CODE_TO(hak, bcode);
		/*while (bcode == HAK_CODE_NOOP) FETCH_BYTE_CODE_TO(hak, bcode);*/

		if (hak->vm_checkbc_cb_count) vm_checkbc(hak, bcode);

		if (HAK_UNLIKELY(hak->abort_req))
		{
			/* i place this abortion check after vm_checkbc()
			 * to honor hak_abort() if called in the callback, */
			HAK_DEBUG0 (hak, "Stopping execution for abortion request\n");
			return_value = hak->_nil;
			goto handle_return;
		}

	#if defined(HAK_PROFILE_VM)
		inst_counter++;
	#endif

		switch (bcode)
		{
			/* -------------------------------------------------------- */
			case HAK_CODE_PLUS:
			{
				/* TODO: support other binary arithmetic operators */
				hak_oop_t x1, x2, x3;
				LOG_INST_0(hak, "plus");
				x2 = HAK_STACK_GETTOP(hak); HAK_STACK_POP(hak);
				x1 = HAK_STACK_GETTOP(hak); HAK_STACK_POP(hak);
				x3 = hak_addnums(hak, x1, x2);
				if (HAK_UNLIKELY(!x3))
				{
					if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
					goto oops_with_errmsg_supplement;
				}
				HAK_STACK_PUSH(hak, x3);
				break;
			}

			/* ------------------------------------------------- */
			case HAK_CODE_PUSH_IVAR_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto push_ivar;
			case HAK_CODE_PUSH_IVAR_0:
			case HAK_CODE_PUSH_IVAR_1:
			case HAK_CODE_PUSH_IVAR_2:
			case HAK_CODE_PUSH_IVAR_3:
			case HAK_CODE_PUSH_IVAR_4:
			case HAK_CODE_PUSH_IVAR_5:
			case HAK_CODE_PUSH_IVAR_6:
			case HAK_CODE_PUSH_IVAR_7:
			{
				hak_oop_t rcv;

				b1 = bcode & 0x7; /* low 3 bits */
			push_ivar:
				LOG_INST_2(hak, "push_ivar %zu ## [%zd]", b1, HAK_OOP_TO_SMOOI(hak->active_context/*->mthhome*/->ivaroff));
				b1 += HAK_OOP_TO_SMOOI(hak->active_context/*->mthhome*/->ivaroff);
				rcv = hak->active_context->receiver;
				/*HAK_ASSERT(hak, HAK_OBJ_GET_FLAGS_TYPE(rcv) == HAK_OBJ_TYPE_OOP);*/
				if (HAK_LIKELY(HAK_OBJ_GET_FLAGS_TYPE(rcv) == HAK_OBJ_TYPE_OOP))
				{
					HAK_STACK_PUSH(hak, ((hak_oop_oop_t)rcv)->slot[b1]);
				}
				else
				{
					hak_oop_t v;

					v = fetch_numeric_rcv_slot(hak, rcv, b1);

					if (HAK_UNLIKELY(!v))
					{
						if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
						goto oops_with_errmsg_supplement;
					}

					HAK_STACK_PUSH(hak, v);
				}
				break;
			}

			/* ------------------------------------------------- */

			case HAK_CODE_STORE_INTO_IVAR_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto store_instvar;
			case HAK_CODE_STORE_INTO_IVAR_0:
			case HAK_CODE_STORE_INTO_IVAR_1:
			case HAK_CODE_STORE_INTO_IVAR_2:
			case HAK_CODE_STORE_INTO_IVAR_3:
			case HAK_CODE_STORE_INTO_IVAR_4:
			case HAK_CODE_STORE_INTO_IVAR_5:
			case HAK_CODE_STORE_INTO_IVAR_6:
			case HAK_CODE_STORE_INTO_IVAR_7:
			{
				hak_oop_t rcv, top;

				b1 = bcode & 0x7; /* low 3 bits */
			store_instvar:
				LOG_INST_2(hak, "store_into_ivar %zu ## [%zd]", b1, HAK_OOP_TO_SMOOI(hak->active_context/*->mthhome*/->ivaroff));
				b1 += HAK_OOP_TO_SMOOI(hak->active_context/*->mthhome*/->ivaroff);
				rcv = hak->active_context->receiver;
				top = HAK_STACK_GETTOP(hak);
				/*HAK_ASSERT(hak, HAK_OBJ_GET_FLAGS_TYPE(rcv) == HAK_OBJ_TYPE_OOP);*/
				if (HAK_LIKELY(HAK_OBJ_GET_FLAGS_TYPE(rcv) == HAK_OBJ_TYPE_OOP))
				{
					((hak_oop_oop_t)rcv)->slot[b1] = top;
				}
				else
				{
					if (HAK_UNLIKELY(store_into_numeric_rcv_slot(hak, rcv, b1, top) <= -1))
					{
						if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
						goto oops_with_errmsg_supplement;
					}
				}
				break;
			}

			/* ------------------------------------------------- */
			case HAK_CODE_POP_INTO_IVAR_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto pop_into_ivar;
			case HAK_CODE_POP_INTO_IVAR_0:
			case HAK_CODE_POP_INTO_IVAR_1:
			case HAK_CODE_POP_INTO_IVAR_2:
			case HAK_CODE_POP_INTO_IVAR_3:
			case HAK_CODE_POP_INTO_IVAR_4:
			case HAK_CODE_POP_INTO_IVAR_5:
			case HAK_CODE_POP_INTO_IVAR_6:
			case HAK_CODE_POP_INTO_IVAR_7:
			{
				hak_oop_t rcv, top;

				b1 = bcode & 0x7; /* low 3 bits */
			pop_into_ivar:
				LOG_INST_2(hak, "pop_into_ivar %zu ## [%zd]", b1, HAK_OOP_TO_SMOOI(hak->active_context->home->ivaroff));
				b1 += HAK_OOP_TO_SMOOI(hak->active_context->home->ivaroff);
				rcv = hak->active_context->receiver;
				top = HAK_STACK_GETTOP(hak);
				/*HAK_ASSERT(hak, HAK_OBJ_GET_FLAGS_TYPE(rcv) == HAK_OBJ_TYPE_OOP);*/
				if (HAK_LIKELY(HAK_OBJ_GET_FLAGS_TYPE(rcv) == HAK_OBJ_TYPE_OOP))
				{
					((hak_oop_oop_t)rcv)->slot[b1] = top;
				}
				else
				{
					if (HAK_UNLIKELY(store_into_numeric_rcv_slot(hak, rcv, b1, top) <= -1))
					{
						if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
						goto oops_with_errmsg_supplement;
					}
				}
				HAK_STACK_POP(hak);
				break;
			}

			/* ------------------------------------------------- */
		#if 0
			/* the compiler never emits these instructions. reuse these instructions for other purposes */
			case HAK_CODE_PUSH_TEMPVAR_X:
			case HAK_CODE_STORE_INTO_TEMPVAR_X:
			case HAK_CODE_POP_INTO_TEMPVAR_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto handle_tempvar;

			case HAK_CODE_PUSH_TEMPVAR_0:
			case HAK_CODE_PUSH_TEMPVAR_1:
			case HAK_CODE_PUSH_TEMPVAR_2:
			case HAK_CODE_PUSH_TEMPVAR_3:
			case HAK_CODE_PUSH_TEMPVAR_4:
			case HAK_CODE_PUSH_TEMPVAR_5:
			case HAK_CODE_PUSH_TEMPVAR_6:
			case HAK_CODE_PUSH_TEMPVAR_7:
			case HAK_CODE_STORE_INTO_TEMPVAR_0:
			case HAK_CODE_STORE_INTO_TEMPVAR_1:
			case HAK_CODE_STORE_INTO_TEMPVAR_2:
			case HAK_CODE_STORE_INTO_TEMPVAR_3:
			case HAK_CODE_STORE_INTO_TEMPVAR_4:
			case HAK_CODE_STORE_INTO_TEMPVAR_5:
			case HAK_CODE_STORE_INTO_TEMPVAR_6:
			case HAK_CODE_STORE_INTO_TEMPVAR_7:
			case HAK_CODE_POP_INTO_TEMPVAR_0:
			case HAK_CODE_POP_INTO_TEMPVAR_1:
			case HAK_CODE_POP_INTO_TEMPVAR_2:
			case HAK_CODE_POP_INTO_TEMPVAR_3:
			case HAK_CODE_POP_INTO_TEMPVAR_4:
			case HAK_CODE_POP_INTO_TEMPVAR_5:
			case HAK_CODE_POP_INTO_TEMPVAR_6:
			case HAK_CODE_POP_INTO_TEMPVAR_7:
			{
				hak_oop_context_t ctx;
				hak_ooi_t bx;

				b1 = bcode & 0x7; /* low 3 bits */
			handle_tempvar:

				/* when CTXTEMPVAR instructions are used, the above
				 * instructions are used only for temporary access
				 * outside a block. i can assume that the temporary
				 * variable index is pointing to one of temporaries
				 * in the relevant method context */
				ctx = hak->active_context->origin;
				bx = b1;
				HAK_ASSERT(hak, HAK_IS_CONTEXT(hak, ctx));

				if ((bcode >> 4) & 1)
				{
					/* push - bit 4 on */
					LOG_INST_1(hak, "push_tempvar %zu", b1);
					HAK_STACK_PUSH(hak, ctx->slot[bx]);
				}
				else
				{
					/* store or pop - bit 5 off */
					ctx->slot[bx] = HAK_STACK_GETTOP(hak);

					if ((bcode >> 3) & 1)
					{
						/* pop - bit 3 on */
						LOG_INST_1(hak, "pop_into_tempvar %zu", b1);
						HAK_STACK_POP(hak);
					}
					else
					{
						LOG_INST_1(hak, "store_into_tempvar %zu", b1);
					}
				}

				break;
			}
		#endif

			/* ------------------------------------------------- */
			case HAK_CODE_PUSH_LITERAL_X2:
				FETCH_PARAM_CODE_TO(hak, b1);
				FETCH_PARAM_CODE_TO(hak, b2);
				b1 = (b1 << (8 * HAK_CODE_LONG_PARAM_SIZE)) | b2;
				goto push_literal;

			case HAK_CODE_PUSH_LITERAL_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto push_literal;

			case HAK_CODE_PUSH_LITERAL_0:
			case HAK_CODE_PUSH_LITERAL_1:
			case HAK_CODE_PUSH_LITERAL_2:
			case HAK_CODE_PUSH_LITERAL_3:
			case HAK_CODE_PUSH_LITERAL_4:
			case HAK_CODE_PUSH_LITERAL_5:
			case HAK_CODE_PUSH_LITERAL_6:
			case HAK_CODE_PUSH_LITERAL_7:
				b1 = bcode & 0x7; /* low 3 bits */
			push_literal:
				LOG_INST_1(hak, "push_literal @%zu", b1);
				/*HAK_STACK_PUSH(hak, hak->code.lit.arr->slot[b1]);*/
				HAK_STACK_PUSH(hak, hak->active_function->literal_frame[b1]);
				break;

			/* ------------------------------------------------- */
			case HAK_CODE_PUSH_OBJECT_X:
			case HAK_CODE_STORE_INTO_OBJECT_X:
			case HAK_CODE_POP_INTO_OBJECT_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto handle_object;

			case HAK_CODE_PUSH_OBJECT_0:
			case HAK_CODE_PUSH_OBJECT_1:
			case HAK_CODE_PUSH_OBJECT_2:
			case HAK_CODE_PUSH_OBJECT_3:
			case HAK_CODE_STORE_INTO_OBJECT_0:
			case HAK_CODE_STORE_INTO_OBJECT_1:
			case HAK_CODE_STORE_INTO_OBJECT_2:
			case HAK_CODE_STORE_INTO_OBJECT_3:
			case HAK_CODE_POP_INTO_OBJECT_0:
			case HAK_CODE_POP_INTO_OBJECT_1:
			case HAK_CODE_POP_INTO_OBJECT_2:
			case HAK_CODE_POP_INTO_OBJECT_3:
			{
				hak_oop_cons_t ass;

				b1 = bcode & 0x3; /* low 2 bits */
			handle_object:
				/*ass = hak->code.lit.arr->slot[b1];*/
				ass = (hak_oop_cons_t)hak->active_function->literal_frame[b1];
				HAK_ASSERT(hak, HAK_IS_CONS(hak, ass));
				/* this association is an entry in the system dictionary.
				 * it doesn't need to look up the dictionary for each access
				 * as the pointer to the association is in the literal frame */

				if ((bcode >> 3) & 1)
				{
					hak_oop_t v;

					/* store or pop */
					v = HAK_STACK_GETTOP(hak);
					if (HAK_IS_CLASS(hak, ass->cdr) && ((hak_oop_class_t)ass->cdr)->name == ass->car && v != ass->cdr)
					{
						/* the existing value is a class.
						 * the class name is the same as the key value of the pair.
						 * disallow re-definition if the new value is not itself. */
						hak_seterrbfmt(hak, HAK_EPERM, "prohibited redefintion of %.*js", HAK_OBJ_GET_SIZE(ass->car), HAK_OBJ_GET_CHAR_SLOT(ass->car));
						if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
						goto oops_with_errmsg_supplement;
					}

					ass->cdr = v; /* update the value */
					if ((bcode >> 2) & 1)
					{
						/* pop */
						LOG_INST_1(hak, "pop_into_object @%zu", b1);
						HAK_STACK_POP(hak);
					}
					else
					{
						LOG_INST_1(hak, "store_into_object @%zu", b1);
					}
				}
				else
				{
					/* push */
					LOG_INST_1(hak, "push_object @%zu", b1);
					if (HAK_IS_UNDEF(hak, ass->cdr))
					{
						hak_seterrbfmt(hak, HAK_EUNDEFVAR, "%.*js accessed without initialization", HAK_OBJ_GET_SIZE(ass->car), HAK_OBJ_GET_CHAR_SLOT(ass->car));
						if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
						goto oops_with_errmsg_supplement;
					}
					HAK_STACK_PUSH(hak, ass->cdr);
				}
				break;
			}

			/* -------------------------------------------------------- */

			case HAK_CODE_JUMP_FORWARD_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "jump_forward %zu", b1);
				hak->ip += b1;
				break;

			case HAK_CODE_JUMP_FORWARD_0:
			case HAK_CODE_JUMP_FORWARD_1:
			case HAK_CODE_JUMP_FORWARD_2:
			case HAK_CODE_JUMP_FORWARD_3:
				LOG_INST_1(hak, "jump_forward %zu", (hak_oow_t)(bcode & 0x3));
				hak->ip += (bcode & 0x3); /* low 2 bits */
				break;

			case HAK_CODE_JUMP_BACKWARD_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "jump_backward %zu", b1);
				hak->ip -= b1;
				break;

			case HAK_CODE_JUMP_BACKWARD_0:
			case HAK_CODE_JUMP_BACKWARD_1:
			case HAK_CODE_JUMP_BACKWARD_2:
			case HAK_CODE_JUMP_BACKWARD_3:
				LOG_INST_1(hak, "jump_backward %zu", (hak_oow_t)(bcode & 0x3));
				hak->ip -= (bcode & 0x3); /* low 2 bits */
				break;

			case HAK_CODE_JUMP_FORWARD_IF_TRUE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "jump_forward_if_true %zu", b1);
				/*if (HAK_STACK_GETTOP(hak) == hak->_true) hak->ip += b1; TODO: _true or not _false?*/
				if (HAK_STACK_GETTOP(hak) != hak->_false) hak->ip += b1;
				break;

			case HAK_CODE_JUMP2_FORWARD_IF_TRUE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "jump2_forward_if_true %zu", b1);
				/*if (HAK_STACK_GETTOP(hak) == hak->_true) hak->ip += MAX_CODE_JUMP + b1;*/
				if (HAK_STACK_GETTOP(hak) != hak->_false) hak->ip += MAX_CODE_JUMP + b1;
				break;

			case HAK_CODE_JUMP_FORWARD_IF_FALSE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "jump_forward_if_false %zu", b1);
				if (HAK_STACK_GETTOP(hak) == hak->_false) hak->ip += b1;
				break;

			case HAK_CODE_JUMP2_FORWARD_IF_FALSE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "jump2_forward_if_false %zu", b1);
				if (HAK_STACK_GETTOP(hak) == hak->_false) hak->ip += MAX_CODE_JUMP + b1;
				break;

			case HAK_CODE_JUMP2_FORWARD:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "jump2_forward %zu", b1);
				hak->ip += MAX_CODE_JUMP + b1;
				break;

			case HAK_CODE_JUMP_BACKWARD_IF_TRUE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "jump_backward_if_true %zu", b1);
				if (HAK_STACK_GETTOP(hak) != hak->_false) hak->ip -= b1;
				break;

			case HAK_CODE_JUMP2_BACKWARD_IF_TRUE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "jump2_backward_if_true %zu", b1);
				if (HAK_STACK_GETTOP(hak) != hak->_false) hak->ip -= MAX_CODE_JUMP + b1;
				break;

			case HAK_CODE_JUMP_BACKWARD_IF_FALSE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "jump_backward_if_false %zu", b1);
				if (HAK_STACK_GETTOP(hak) == hak->_false) hak->ip -= b1;
				break;

			case HAK_CODE_JUMP2_BACKWARD_IF_FALSE:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "jump2_backward_if_false %zu", b1);
				if (HAK_STACK_GETTOP(hak) == hak->_false) hak->ip -= MAX_CODE_JUMP + b1;
				break;

			case HAK_CODE_JUMP2_BACKWARD:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "jump2_backward %zu", b1);
				hak->ip -= MAX_CODE_JUMP + b1;
				break;

			/* -------------------------------------------------------- */

			case HAK_CODE_PUSH_RETURN_R:
			{
				hak_oop_context_t ctx;
				hak_oow_t i;
				hak_ooi_t attr_mask, fixed_nargs, req_nrets;

				LOG_INST_0(hak, "push_return_r");

				HAK_ASSERT(hak, HAK_IS_CONTEXT(hak, hak->active_context));

				ctx = hak->active_context;

				attr_mask = HAK_OOP_TO_SMOOI(ctx->attr_mask);
				fixed_nargs = GET_BLK_MASK_NARGS(attr_mask);

				req_nrets = HAK_OOP_TO_SMOOI(ctx->req_nrets);

				if (req_nrets <= 0)
				{
					/* if a function with return variables is called in the single-return value call style,
					 * req_nrets becomes 0. but this instruction has to push one value in such a case */
					req_nrets = 1;
				}

				/* return variables are placed after the fixed arguments */
				for (i = 0; i < req_nrets; i++)
				{
					HAK_STACK_PUSH(hak, ctx->slot[fixed_nargs + i]);
				}

				/* similar to HAK_CODE_RETURN_FROM_BLOCK */
				hak->last_retv = ctx->slot[fixed_nargs]; /* remember the first pushed one as the last return value. currently no good way to hak_execute() recognize multiple return values. */
				do_return_from_block(hak);

				break;
			}

			case HAK_CODE_CALL_R:
			{
				hak_oop_t rcv;

				FETCH_PARAM_CODE_TO(hak, b1); /* nargs */
				FETCH_PARAM_CODE_TO(hak, b2); /* nrvars */
				LOG_INST_2(hak, "call %zu %zu", b1, b2);

				rcv = HAK_STACK_GETOP(hak, b1);
				if (HAK_IS_COMPILED_BLOCK(hak, rcv))
				{
					if (activate_block(hak, b1, b2) <= -1) goto call2_failed;
					break;
				}
				else
				{
					hak_seterrbfmt(hak, HAK_ECALL, "cannot call %O", rcv);
				call2_failed:
					goto oops_with_errmsg_supplement;
				}

				break;
			}

			case HAK_CODE_CALL_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto handle_call;
			case HAK_CODE_CALL_0:
			case HAK_CODE_CALL_1:
			case HAK_CODE_CALL_2:
			case HAK_CODE_CALL_3:
			{
				hak_oop_t op;

				b1 = bcode & 0x3; /* low 2 bits */
			handle_call:
				LOG_INST_1(hak, "call %zu", b1);

			/* TODO: check if the rcv is the dummy receiver
				rcv = HAK_STACK_GETRCV(hak, b1);
			 * */
				op = HAK_STACK_GETOP(hak, b1);
				if (HAK_OOP_IS_POINTER(op))
				{
					hak_oop_class_t c;
					c = (hak_oop_class_t)HAK_OBJ_GET_CLASS(op);
/* TODO: use class to check? no more ibrand? */
					switch (HAK_OOP_TO_SMOOI(c->ibrand))
					{
						case HAK_BRAND_FUNCTION:
							if (activate_function(hak, b1) <= -1) goto call_failed;
							break;

						case HAK_BRAND_BLOCK:
							if (activate_block(hak, b1, 0) <= -1) goto call_failed;
							break;

						case HAK_BRAND_PRIM:
							if (call_primitive(hak, b1) <= -1)
							{
							/* TODO: do i have tell a catchable exception from a fatal error? */
								if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
								goto call_failed;
							}
							break;

#if defined(ENABLE_SYSCMD)
						case HAK_BRAND_STRING:
						case HAK_BRAND_SYMBOL:
							if (exec_syscmd(hak, b1) <= -1)
							{
								if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
								goto call_failed;
							}
							break;
#endif

						default:
							goto cannot_call;
					}
				}
				else
				{
				cannot_call:
					/* run time error */
					hak_seterrbfmt(hak, HAK_ECALL, "cannot call %O", op);
					if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
				call_failed:
					goto oops_with_errmsg_supplement;
				}
				break;
			}

			/* -------------------------------------------------------- */
			case HAK_CODE_TRY_ENTER:
			{
				hak_ooi_t catch_ip, clsp;

				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "try_enter %zu", b1);

				catch_ip = hak->ip + b1;
				/* TODO: ip overflow check? */
				clsp = HAK_CLSTACK_GET_SP(hak);

				HAK_EXSTACK_PUSH(hak, hak->active_context, catch_ip, clsp, hak->sp);
				break;
			}

			case HAK_CODE_TRY_ENTER2:
			{
				hak_ooi_t catch_ip, clsp;

				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "try_enter2 %zu", b1);

				catch_ip = hak->ip + MAX_CODE_JUMP + b1;
				/* TODO: ip overflow check? */
				clsp = HAK_CLSTACK_GET_SP(hak);

				HAK_EXSTACK_PUSH(hak, hak->active_context, catch_ip, clsp, hak->sp);
				break;
			}

			case HAK_CODE_TRY_EXIT:
				LOG_INST_0(hak, "try_exit");
				/* TODO: stack underflow check? */
				HAK_EXSTACK_POP(hak);
				break;

			case HAK_CODE_THROW:
				LOG_INST_0(hak, "throw");
				return_value = HAK_STACK_GETTOP(hak);
				HAK_STACK_POP(hak);
				if (do_throw(hak, return_value, fetched_instruction_pointer) <= -1) goto oops;
				break;
			/* -------------------------------------------------------- */

			case HAK_CODE_CLASS_LOAD:
			{
				hak_oop_t t;

				/* push the class off the stack top on the class stack */
				LOG_INST_0(hak, "class_load");

				HAK_STACK_POP_TO(hak, t);
				if (!HAK_IS_CLASS(hak, t))
				{
					/*hak_seterrbfmt(hak, HAK_EUNDEFVAR, "%.*js is not class", HAK_OBJ_GET_SIZE(t->car), HAK_OBJ_GET_CHAR_SLOT(t->car));*/
					hak_seterrbfmt(hak, HAK_EUNDEFVAR, "not class"); /* TODO: change error code */
					if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
					goto oops_with_errmsg_supplement;
				}
				HAK_CLSTACK_PUSH(hak, t);
				break;
			}

			case HAK_CODE_CLASS_ENTER:
			{
				/* push an association with class_name as a key or push nil
				   push superclass (only if nsuperclassses > 0)
				   push ivars_string
				   push cvars_string
				   class_enter nsuperclasses nivars ncvars spec/selfspec, indexed_tye
				 */
				hak_oop_t superclass, ivars_str, cvars_str, class_name, v;
				hak_ooi_t expected_spec, expected_selfspec;
				hak_oop_class_t class_obj;
				hak_oow_t b3, b4, b5;

				FETCH_PARAM_CODE_TO(hak, b1); /* nsuperclasses */
				FETCH_PARAM_CODE_TO(hak, b2); /* nivars */
				FETCH_PARAM_CODE_TO(hak, b3); /* ncvars */
				FETCH_BYTE_CODE_TO(hak, b4); /* spec/selfspec */
				FETCH_BYTE_CODE_TO(hak, b5); /* indexed_type */

				LOG_INST_5(hak, "class_enter %zu %zu %zu %#zx %zu", b1, b2, b3, b4, b5);

				if (b3 > 0)
				{
					HAK_STACK_POP_TO(hak, cvars_str);
					HAK_ASSERT(hak, HAK_IS_STRING(hak, cvars_str));
				}
				else cvars_str = hak->_nil;

				if (b2 > 0)
				{
					HAK_STACK_POP_TO(hak, ivars_str);
					HAK_ASSERT(hak, HAK_IS_STRING(hak, ivars_str));
				}
				else ivars_str = hak->_nil;

				if (b1 > 0)
				{
					hak_ooi_t super_spec;
					hak_obj_type_t super_indexed_type;

					HAK_STACK_POP_TO(hak, superclass); /* TODO: support more than 1 superclass later when the compiler supports more */
					if (!HAK_IS_CLASS(hak, superclass))
					{
						hak_seterrbfmt(hak, HAK_ECALL, "invalid superclass %O", superclass);
						if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
						goto oops_with_errmsg_supplement;
					}

					super_spec = HAK_OOP_TO_SMOOI(((hak_oop_class_t)superclass)->spec);
					super_indexed_type = HAK_CLASS_SPEC_INDEXED_TYPE(super_spec);
					if (HAK_CLASS_SPEC_NAMED_INSTVARS(super_spec) > 0 && super_indexed_type != b5)
					{
						/* TODO: include the class indexed type in the message .. */
						hak_seterrbfmt(hak, HAK_ECALL, "incompatible %hs superclass %O with %hs class",
							hak_obj_type_to_bcstr(super_indexed_type), superclass, hak_obj_type_to_bcstr(b5));
						if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
						goto oops_with_errmsg_supplement;
					}
 				}
				else superclass = hak->_nil;

				expected_spec = HAK_CLASS_SPEC_MAKE(b2, ((b4 >> 4) & 0x0F), (b5 & 0xFF));
				expected_selfspec = HAK_CLASS_SELFSPEC_MAKE(b3, 0, (b4 & 0x0F));

				HAK_STACK_POP_TO(hak, v);
				if (HAK_IS_CONS(hak, v))
				{
					/* named class. the compiler generates code to push a pair
					 * holding a name and a class object for a name class. */
					class_name = HAK_CONS_CAR(v);
					HAK_ASSERT(hak, HAK_IS_SYMBOL(hak, class_name));

					class_obj = (hak_oop_class_t)HAK_CONS_CDR(v);
					if (HAK_IS_CLASS(hak, class_obj))
					{
						/* the existing value must be a class. disallow re-definition */

						/* 0(non-kernel object), 1(incomplete kernel object), 2(complete kernel object) */
						if (HAK_OBJ_GET_FLAGS_KERNEL(class_obj) == 1)
						{
							/* check if the new definition is compatible with kernel definition */
							hak_ooi_t spec, selfspec, nivars_super, nivars_super_real;

							spec = HAK_OOP_TO_SMOOI(class_obj->spec);
							selfspec = HAK_OOP_TO_SMOOI(class_obj->selfspec);
							nivars_super = HAK_OOP_TO_SMOOI(class_obj->nivars_super);
							nivars_super_real = HAK_IS_NIL(hak, superclass)? 0: HAK_OOP_TO_SMOOI(((hak_oop_class_t)superclass)->nivars_super);
#if 0
hak_logbfmt(hak, HAK_LOG_STDERR, ">>>%O c->sc=%O sc=%O b2=%d b3=%d nivars=%d ncvars=%d<<<\n", class_obj, class_obj->superclass, superclass, b2, b3, (int)HAK_CLASS_SPEC_NAMED_INSTVARS(spec), (int)HAK_CLASS_SELFSPEC_CLASSVARS(spec));
#endif
							if (class_obj->superclass != superclass ||
							    expected_spec != spec ||
							    expected_selfspec != selfspec ||
							    nivars_super != nivars_super_real)
							{
								hak_seterrbfmt(hak, HAK_EPERM, "incompatible redefintion of %.*js", HAK_OBJ_GET_SIZE(class_name), HAK_OBJ_GET_CHAR_SLOT(class_name));
								if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
								goto oops_with_errmsg_supplement;
							}
						}
						else
						{
							hak_seterrbfmt(hak, HAK_EPERM, "prohibited redefintion of %.*js", HAK_OBJ_GET_SIZE(class_name), HAK_OBJ_GET_CHAR_SLOT(class_name));
							if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
							goto oops_with_errmsg_supplement;
						}
					}
					else
					{
						HAK_ASSERT(hak, HAK_IS_NIL(hak, (hak_oop_t)class_obj));
						goto make_class;
					}
				}
				else
				{
					/* anonymous class */
					HAK_ASSERT(hak, HAK_IS_NIL(hak, v));
					class_name = hak->_nil;

				make_class:
					class_obj = (hak_oop_class_t)hak_makeclass(hak, class_name, superclass, expected_spec, expected_selfspec, ivars_str, cvars_str);
					if (HAK_UNLIKELY(!class_obj)) goto oops_with_errmsg_supplement;
				}

				/* push the class created to the class stack.
				 * but don't push to the normal operation stack */
				HAK_CLSTACK_PUSH(hak, (hak_oop_t)class_obj);
				break;
			}

			case HAK_CODE_CLASS_EXIT:
			{
				LOG_INST_0(hak, "class_exit");
				if (HAK_CLSTACK_IS_EMPTY(hak))
				{
					hak_seterrbfmt(hak, HAK_ESTKUNDFLW, "class stack underflow");
					goto oops_with_errmsg_supplement;
				}
				HAK_CLSTACK_POP(hak);
				break;
			}

			case HAK_CODE_CLASS_PEXIT: /* pop + exit */
			{
				hak_oop_t c;

				LOG_INST_0(hak, "class_pexit");

				if (HAK_CLSTACK_IS_EMPTY(hak))
				{
					hak_seterrbfmt(hak, HAK_ESTKUNDFLW, "class stack underflow");
					goto oops_with_errmsg_supplement;
				}
				HAK_CLSTACK_POP_TO(hak, c);
				HAK_STACK_PUSH(hak, c);

				break;
			}

			case HAK_CODE_CLASS_CMSTORE:
			case HAK_CODE_CLASS_IMSTORE:
			case HAK_CODE_CLASS_CIMSTORE:
			{
				hak_oop_t _class;
				hak_oop_t mdic, blk, name;
				int mtype;
				static const hak_bch_t* pfx[] = { "c", "i", "ci" };

				mtype = (bcode - HAK_CODE_CLASS_CMSTORE) + 1;
				HAK_ASSERT(hak, mtype >= 1 && mtype <= 3);
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_2(hak, "class_%hsmstore @%zu", pfx[mtype - 1], b1);

				/* store the stack top in the member dictionary of the currect class with the key indicated by 'b1' */

				HAK_ASSERT(hak, !HAK_CLSTACK_IS_EMPTY(hak));

				HAK_CLSTACK_FETCH_TOP_TO(hak, _class);
				HAK_ASSERT(hak, HAK_IS_CLASS(hak, _class));

				mdic = ((hak_oop_class_t)_class)->mdic; /* instance-side dictionary */
				HAK_ASSERT(hak, HAK_IS_NIL(hak, mdic) || HAK_IS_DIC(hak, mdic));
				if (HAK_IS_NIL(hak, mdic))
				{
					hak_pushvolat(hak, (hak_oop_t*)&_class);
					mdic = hak_makedic(hak, 16); /* TODO: configurable initial size? */
					hak_popvolat(hak);
					if (HAK_UNLIKELY(!mdic)) goto oops_with_errmsg_supplement;
					((hak_oop_class_t)_class)->mdic = mdic;
				}

				blk = HAK_STACK_GETTOP(hak);
				name = hak->active_function->literal_frame[b1]; /* method name */
				/* put the code at method dictionary
				   pass 1 for class method, 2 for instance method, 3 for class instantiation method */
				if (!hak_putatdic_method(hak, (hak_oop_dic_t)mdic, name, blk, mtype)) goto oops_with_errmsg_supplement;
				break;
			}
			/* -------------------------------------------------------- */

			case HAK_CODE_PUSH_CTXTEMPVAR_X:
			case HAK_CODE_STORE_INTO_CTXTEMPVAR_X:
			case HAK_CODE_POP_INTO_CTXTEMPVAR_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				FETCH_PARAM_CODE_TO(hak, b2);
				goto handle_ctxtempvar;
			case HAK_CODE_PUSH_CTXTEMPVAR_0:
			case HAK_CODE_PUSH_CTXTEMPVAR_1:
			case HAK_CODE_PUSH_CTXTEMPVAR_2:
			case HAK_CODE_PUSH_CTXTEMPVAR_3:
			case HAK_CODE_STORE_INTO_CTXTEMPVAR_0:
			case HAK_CODE_STORE_INTO_CTXTEMPVAR_1:
			case HAK_CODE_STORE_INTO_CTXTEMPVAR_2:
			case HAK_CODE_STORE_INTO_CTXTEMPVAR_3:
			case HAK_CODE_POP_INTO_CTXTEMPVAR_0:
			case HAK_CODE_POP_INTO_CTXTEMPVAR_1:
			case HAK_CODE_POP_INTO_CTXTEMPVAR_2:
			case HAK_CODE_POP_INTO_CTXTEMPVAR_3:
			{
				hak_ooi_t i;
				hak_oop_context_t ctx;

				b1 = bcode & 0x3; /* low 2 bits */
				FETCH_BYTE_CODE_TO(hak, b2);

			handle_ctxtempvar:
				ctx = hak->active_context;
				HAK_ASSERT(hak, (hak_oop_t)ctx != hak->_nil);
				for (i = 0; i < b1; i++)
				{
					ctx = (hak_oop_context_t)ctx->home;
					/* the initial context has nil in the home field.
					 * the loop must not reach beyond the initial context */
					HAK_ASSERT(hak, (hak_oop_t)ctx != hak->_nil);
				}

				if ((bcode >> 3) & 1)
				{
					/* store or pop */
					ctx->slot[b2] = HAK_STACK_GETTOP(hak);

					if ((bcode >> 2) & 1)
					{
						/* pop */
						HAK_STACK_POP(hak);
						LOG_INST_2(hak, "pop_into_ctxtempvar %zu %zu", b1, b2);
					}
					else
					{
						LOG_INST_2(hak, "store_into_ctxtempvar %zu %zu", b1, b2);
					}
				}
				else
				{
					/* push */
					HAK_STACK_PUSH(hak, ctx->slot[b2]);
					LOG_INST_2(hak, "push_ctxtempvar %zu %zu", b1, b2);
				}

				break;
			}
			/* -------------------------------------------------------- */

			case HAK_CODE_PUSH_OBJVAR_X:
			case HAK_CODE_STORE_INTO_OBJVAR_X:
			case HAK_CODE_POP_INTO_OBJVAR_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				FETCH_PARAM_CODE_TO(hak, b2);
				goto handle_objvar;

			case HAK_CODE_PUSH_OBJVAR_0:
			case HAK_CODE_PUSH_OBJVAR_1:
			case HAK_CODE_PUSH_OBJVAR_2:
			case HAK_CODE_PUSH_OBJVAR_3:
			case HAK_CODE_STORE_INTO_OBJVAR_0:
			case HAK_CODE_STORE_INTO_OBJVAR_1:
			case HAK_CODE_STORE_INTO_OBJVAR_2:
			case HAK_CODE_STORE_INTO_OBJVAR_3:
			case HAK_CODE_POP_INTO_OBJVAR_0:
			case HAK_CODE_POP_INTO_OBJVAR_1:
			case HAK_CODE_POP_INTO_OBJVAR_2:
			case HAK_CODE_POP_INTO_OBJVAR_3:
			{
				hak_oop_oop_t t;

				/* b1 -> variable index in the object indicated by b2.
				 * b2 -> object index stored in the literal frame. */
				b1 = bcode & 0x3; /* low 2 bits */
				FETCH_BYTE_CODE_TO(hak, b2);

			handle_objvar:
				/*t = hak->code.lit.arr->slot[b2];*/
				t = (hak_oop_oop_t)hak->active_function->literal_frame[b2];
				HAK_ASSERT(hak, HAK_OBJ_GET_FLAGS_TYPE(t) == HAK_OBJ_TYPE_OOP);
				HAK_ASSERT(hak, b1 < HAK_OBJ_GET_SIZE(t));

				if ((bcode >> 3) & 1)
				{
					/* store or pop */
					t->slot[b1] = HAK_STACK_GETTOP(hak);

					if ((bcode >> 2) & 1)
					{
						/* pop */
						LOG_INST_2(hak, "pop_into_objvar %zu %zu", b1, b2);
						HAK_STACK_POP(hak);
					}
					else
					{
						LOG_INST_2(hak, "store_into_objvar %zu %zu", b1, b2);
					}
				}
				else
				{
					/* push */
					LOG_INST_2(hak, "push_objvar %zu %zu", b1, b2);
					HAK_STACK_PUSH(hak, t->slot[b1]);
				}
				break;
			}

			/* -------------------------------------------------------- */
			case HAK_CODE_SEND_R: /* send message with return variables */
			case HAK_CODE_SEND_TO_SUPER_R:

				FETCH_PARAM_CODE_TO(hak, b1); /* nargs */
				FETCH_PARAM_CODE_TO(hak, b2); /* nrvars */

				LOG_INST_3(hak, "send%hs %zu %zu", (((bcode >> 2) & 1)? "_to_super": ""), b1, b2);
				goto handle_send_2;


			case HAK_CODE_SEND_X:
			case HAK_CODE_SEND_TO_SUPER_X:
				FETCH_PARAM_CODE_TO(hak, b1);
				goto handle_send;

			case HAK_CODE_SEND_0:
			case HAK_CODE_SEND_1:
			case HAK_CODE_SEND_2:
			case HAK_CODE_SEND_3:
			case HAK_CODE_SEND_TO_SUPER_0:
			case HAK_CODE_SEND_TO_SUPER_1:
			case HAK_CODE_SEND_TO_SUPER_2:
			case HAK_CODE_SEND_TO_SUPER_3:
			{
				hak_oop_t rcv, op;

				b1 = bcode & 0x3; /* low 2 bits */
			handle_send:
				b2 = 0;
				LOG_INST_2(hak, "send%hs %zu", (((bcode >> 2) & 1)? "_to_super": ""), b1);

			handle_send_2:
				rcv = HAK_STACK_GETRCV(hak, b1);
				op = HAK_STACK_GETOP(hak, b1);
				if (!HAK_OBJ_IS_CHAR_POINTER(op)) /*if (!HAK_IS_SYMBOL(hak, op))*/
				{
					hak_seterrbfmt(hak, HAK_ECALL, "unable to send %O to %O - invalid message", op, rcv); /* TODO: change to HAK_ESEND?? */
				cannot_send:
					if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
					goto oops_with_errmsg_supplement;
				}
				else
				{
					if (send_message(hak, rcv, op, ((bcode >> 2) & 1) /* to_super */, b1 /* nargs */, b2 /* nrvars */) <= -1)
					{
						const hak_ooch_t* msg = hak_backuperrmsg(hak);
						hak_seterrbfmt(hak, HAK_ECALL, "unable to send %O to %O - %js", op, rcv, msg); /* TODO: change to HAK_ESEND?? */
						goto cannot_send;
					}
				}
				break;
			}

			/* -------------------------------------------------------- */

			/* access the class variables in the initialization context.
			 * the class object is at the class stack top */
			case HAK_CODE_PUSH_CVAR_I_X:
			{
				hak_oop_t t;
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "push_cvar_i %zu", b1);
				HAK_CLSTACK_FETCH_TOP_TO(hak, t);
				HAK_ASSERT(hak, HAK_IS_CLASS(hak, t));
				HAK_STACK_PUSH(hak, ((hak_oop_class_t)t)->cvar[b1]);
				break;
			}

			case HAK_CODE_STORE_INTO_CVAR_I_X:
			{
				hak_oop_t t;
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "store_into_cvar_i %zu", b1);
				if (HAK_CLSTACK_IS_EMPTY(hak))
				{
					hak_seterrbfmt(hak, HAK_ESTKUNDFLW, "empty class stack");
				/* TODO: do throw??? instead */
					goto oops_with_errmsg_supplement;
				}
				HAK_CLSTACK_FETCH_TOP_TO(hak, t);
				HAK_ASSERT(hak, HAK_IS_CLASS(hak, t));
				((hak_oop_class_t)t)->cvar[b1] = HAK_STACK_GETTOP(hak);
				break;
			}

			case HAK_CODE_POP_INTO_CVAR_I_X:
			{
				hak_oop_t t;
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "pop_into_cvar_i %zu", b1);
				if (HAK_CLSTACK_IS_EMPTY(hak))
				{
					hak_seterrbfmt(hak, HAK_ESTKUNDFLW, "empty class stack");
				/* TODO: do throw??? instead */
					goto oops_with_errmsg_supplement;
				}
				HAK_CLSTACK_FETCH_TOP_TO(hak, t);
				HAK_ASSERT(hak, HAK_IS_CLASS(hak, t));
				((hak_oop_class_t)t)->cvar[b1] = HAK_STACK_GETTOP(hak);
				HAK_STACK_POP(hak);
				break;
			}

			/* -------------------------------------------------------- */


			/* access class variables referenced in a method context.
			 * the class variables slots in the owning class of the method that triggerred the current active context */
			case HAK_CODE_PUSH_CVAR_M_X:
			{
				hak_oop_t t;
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "push_cvar_m %zu", b1);
				HAK_ASSERT(hak, (hak_oop_t)hak->active_context/*->mthhome*/ != hak->_nil);
				t = hak->active_context/*->mthhome*/->owner;
				if (HAK_UNLIKELY(!HAK_IS_CLASS(hak, t)))
				{
					/* this is an internal error or the bytecodes are compromised */
					hak_seterrbfmt(hak, HAK_EINTERN, "non-class owner in class variable access");
					goto oops_with_errmsg_supplement;
				}
				HAK_STACK_PUSH(hak, ((hak_oop_class_t)t)->cvar[b1]);
				break;
			}

			case HAK_CODE_STORE_INTO_CVAR_M_X:
			{
				hak_oop_t t;
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "store_into_cvar_m %zu", b1);
				HAK_ASSERT(hak, (hak_oop_t)hak->active_context/*->mthhome*/ != hak->_nil);
				t = hak->active_context/*->mthhome*/->owner;
				if (HAK_UNLIKELY(!HAK_IS_CLASS(hak, t)))
				{
					/* this is an internal error or the bytecodes are compromised */
					hak_seterrbfmt(hak, HAK_EINTERN, "non-class owner in class variable access");
					goto oops_with_errmsg_supplement;
				}
				((hak_oop_class_t)t)->cvar[b1] = HAK_STACK_GETTOP(hak);
				break;
			}

			case HAK_CODE_POP_INTO_CVAR_M_X:
			{
				hak_oop_t t;
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "pop_into_cvar_m %zu", b1);
				HAK_ASSERT(hak, (hak_oop_t)hak->active_context/*->mthhome*/ != hak->_nil);
				t = hak->active_context/*->mthhome*/->owner;
				if (HAK_UNLIKELY(!HAK_IS_CLASS(hak, t)))
				{
					/* this is an internal error or the bytecodes are compromised */
					hak_seterrbfmt(hak, HAK_EINTERN, "non-class owner in class variable access");
					goto oops_with_errmsg_supplement;
				}
				((hak_oop_class_t)t)->cvar[b1] = HAK_STACK_GETTOP(hak);
				HAK_STACK_POP(hak);
				break;
			}

			/* -------------------------------------------------------- */

			case HAK_CODE_PUSH_RECEIVER: /* push self or super */
				LOG_INST_0(hak, "push_receiver");
				HAK_STACK_PUSH(hak, hak->active_context->receiver);
				break;

			case HAK_CODE_PUSH_NIL:
				LOG_INST_0(hak, "push_nil");
				HAK_STACK_PUSH(hak, hak->_nil);
				break;

			case HAK_CODE_PUSH_TRUE:
				LOG_INST_0(hak, "push_true");
				HAK_STACK_PUSH(hak, hak->_true);
				break;

			case HAK_CODE_PUSH_FALSE:
				LOG_INST_0(hak, "push_false");
				HAK_STACK_PUSH(hak, hak->_false);
				break;

			case HAK_CODE_PUSH_CONTEXT:
				LOG_INST_0(hak, "push_context");
				HAK_STACK_PUSH(hak, (hak_oop_t)hak->active_context);
				break;

			case HAK_CODE_PUSH_PROCESS:
				LOG_INST_0(hak, "push_process");
				HAK_STACK_PUSH(hak, (hak_oop_t)hak->processor->active);
				break;

			case HAK_CODE_PUSH_NEGONE:
				LOG_INST_0(hak, "push_negone");
				HAK_STACK_PUSH(hak, HAK_SMOOI_TO_OOP(-1));
				break;

			case HAK_CODE_PUSH_ZERO:
				LOG_INST_0(hak, "push_zero");
				HAK_STACK_PUSH(hak, HAK_SMOOI_TO_OOP(0));
				break;

			case HAK_CODE_PUSH_ONE:
				LOG_INST_0(hak, "push_one");
				HAK_STACK_PUSH(hak, HAK_SMOOI_TO_OOP(1));
				break;

			case HAK_CODE_PUSH_TWO:
				LOG_INST_0(hak, "push_two");
				HAK_STACK_PUSH(hak, HAK_SMOOI_TO_OOP(2));
				break;

			case HAK_CODE_PUSH_INTLIT:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "push_intlit %zu", b1);
				HAK_STACK_PUSH(hak, HAK_SMOOI_TO_OOP(b1));
				break;

			case HAK_CODE_PUSH_NEGINTLIT:
			{
				hak_ooi_t num;
				FETCH_PARAM_CODE_TO(hak, b1);
				num = b1;
				LOG_INST_1(hak, "push_negintlit %zu", b1);
				HAK_STACK_PUSH(hak, HAK_SMOOI_TO_OOP(-num));
				break;
			}

			case HAK_CODE_PUSH_CHARLIT:
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "push_charlit %zu", b1);
				HAK_STACK_PUSH(hak, HAK_CHAR_TO_OOP(b1));
				break;
			/* -------------------------------------------------------- */

			case HAK_CODE_MAKE_ARRAY:
			{
				hak_oop_t t;

				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "make_array %zu", b1);

				/* create an empty array */
				t = hak_makearray(hak, b1);
				if (HAK_UNLIKELY(!t)) goto oops_with_errmsg_supplement;

				HAK_STACK_PUSH(hak, t); /* push the array created */
				break;
			}

			case HAK_CODE_POP_INTO_ARRAY:
			{
				hak_oop_t t1, t2;
				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "pop_into_array %zu", b1);
				t1 = HAK_STACK_GETTOP(hak); /* value to store */
				HAK_STACK_POP(hak);
				t2 = HAK_STACK_GETTOP(hak); /* array */
				if (HAK_UNLIKELY(b1 >= HAK_OBJ_GET_SIZE(t2)))
				{
					hak_seterrbfmt(hak, HAK_ECALL, "array index %zu out of upper bound %zd ", b1, (hak_oow_t)HAK_OBJ_GET_SIZE(t2));
					if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
					goto oops_with_errmsg_supplement;
				}

				((hak_oop_oop_t)t2)->slot[b1] = t1;
				break;
			}

			case HAK_CODE_MAKE_BYTEARRAY:
			{
				hak_oop_t t;

				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "make_bytearray %zu", b1);

				/* create an empty array */
				t = hak_makebytearray(hak, HAK_NULL, b1);
				if (HAK_UNLIKELY(!t)) goto oops_with_errmsg_supplement;

				HAK_STACK_PUSH(hak, t); /* push the byte array created */
				break;
			}

			case HAK_CODE_POP_INTO_BYTEARRAY:
			{
				hak_oop_t t1, t2;
				hak_ooi_t bv;

				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "pop_into_bytearray %zu", b1);

				t1 = HAK_STACK_GETTOP(hak); /* value to store */
				if (!HAK_OOP_IS_SMOOI(t1) || (bv = HAK_OOP_TO_SMOOI(t1)) < 0 || bv > 255)
				{
					hak_seterrbfmt(hak, HAK_ERANGE, "not a byte or out of byte range - %O", t1);
					if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
					goto oops_with_errmsg_supplement;
				}
				HAK_STACK_POP(hak);
				t2 = HAK_STACK_GETTOP(hak); /* byte array */

				if (HAK_UNLIKELY(b1 >= HAK_OBJ_GET_SIZE(t2)))
				{
					hak_seterrbfmt(hak, HAK_ECALL, "bytearray index %zu out of upper bound %zd ", b1, (hak_oow_t)HAK_OBJ_GET_SIZE(t2));
					if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
					goto oops_with_errmsg_supplement;
				}
				((hak_oop_byte_t)t2)->slot[b1] = bv;
				break;
			}

			case HAK_CODE_MAKE_CHARARRAY:
			{
				hak_oop_t t;

				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "make_chararray %zu", b1);

				/* create an empty array */
				t = hak_makechararray(hak, HAK_NULL, b1);
				if (HAK_UNLIKELY(!t)) goto oops_with_errmsg_supplement;

				HAK_STACK_PUSH(hak, t); /* push the char array created */
				break;
			}

			case HAK_CODE_POP_INTO_CHARARRAY:
			{
				hak_oop_t t1, t2;
				hak_ooi_t bv;

				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "pop_into_chararray %zu", b1);

				t1 = HAK_STACK_GETTOP(hak); /* value to store */
				if (!HAK_OOP_IS_CHAR(t1) || (bv = HAK_OOP_TO_CHAR(t1)) < 0 || bv > 255)
				{
					hak_seterrbfmt(hak, HAK_ERANGE, "not a character or out of character range - %O", t1);
					if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
					goto oops_with_errmsg_supplement;
				}
				HAK_STACK_POP(hak);
				t2 = HAK_STACK_GETTOP(hak); /* char array */

				if (HAK_UNLIKELY(b1 >= HAK_OBJ_GET_SIZE(t2)))
				{
					hak_seterrbfmt(hak, HAK_ECALL, "character array index %zu out of upper bound %zd ", b1, (hak_oow_t)HAK_OBJ_GET_SIZE(t2));
					if (do_throw_with_internal_errmsg(hak, fetched_instruction_pointer) >= 0) break;
					goto oops_with_errmsg_supplement;
				}
				((hak_oop_char_t)t2)->slot[b1] = bv;
				break;
			}

			case HAK_CODE_MAKE_DIC:
			{
				hak_oop_t t;

				FETCH_PARAM_CODE_TO(hak, b1);
				LOG_INST_1(hak, "make_dic %zu", b1);
				t = (hak_oop_t)hak_makedic(hak, b1 + 10);
				if (HAK_UNLIKELY(!t)) goto oops_with_errmsg_supplement;
				HAK_STACK_PUSH(hak, t);
				break;
			}

			case HAK_CODE_POP_INTO_DIC:
			{
				hak_oop_t t1, t2, t3;

				LOG_INST_0(hak, "pop_into_dic");
				t1 = HAK_STACK_GETTOP(hak); /* value */
				HAK_STACK_POP(hak);
				t2 = HAK_STACK_GETTOP(hak); /* key */
				HAK_STACK_POP(hak);
				t3 = HAK_STACK_GETTOP(hak); /* dictionary */
				if (!hak_putatdic(hak, (hak_oop_dic_t)t3, t2, t1)) goto oops_with_errmsg_supplement;
				break;
			}

			case HAK_CODE_MAKE_CONS:
			{
				hak_oop_t t;

				LOG_INST_0(hak, "make_cons");

				t = hak_makecons(hak, hak->_nil, hak->_nil);
				if (HAK_UNLIKELY(!t)) goto oops_with_errmsg_supplement;

				HAK_STACK_PUSH(hak, t); /* push the head cons cell */
				HAK_STACK_PUSH(hak, hak->_nil); /* sentinnel */
				break;
			}

			case HAK_CODE_POP_INTO_CONS:
			{
				hak_oop_t t1, t2, t3;
				LOG_INST_0(hak, "pop_into_cons");

				t1 = HAK_STACK_GETTOP(hak); /* value to store */
				HAK_STACK_POP(hak);

				t3 = HAK_STACK_GETTOP(hak); /* sentinnel */
				HAK_STACK_POP(hak);

				t2 = HAK_STACK_GETTOP(hak); /* head cons */
				if (HAK_UNLIKELY(!HAK_IS_CONS(hak, t2)))
				{
					hak_seterrbfmt(hak, HAK_EINTERN, "internal error - invalid vm state detected in pop_into_cons");
					goto oops;
				}

				if (t3 == hak->_nil)
				{
					((hak_oop_oop_t)t2)->slot[0] = t1;
					HAK_STACK_PUSH(hak, t2); /* push self again */
				}
				else
				{
					hak_oop_t t;

					hak_pushvolat(hak, &t3);
					t = hak_makecons(hak, t1, hak->_nil);
					hak_popvolat(hak);
					if (HAK_UNLIKELY(!t)) goto oops_with_errmsg_supplement;

					((hak_oop_oop_t)t3)->slot[1] = t;
					HAK_STACK_PUSH(hak, t);
				}

				break;
			}

			case HAK_CODE_POP_INTO_CONS_END:
			{
				hak_oop_t t1, t2, t3;
				LOG_INST_0(hak, "pop_into_cons_end");

				t1 = HAK_STACK_GETTOP(hak); /* value to store */
				HAK_STACK_POP(hak);

				t3 = HAK_STACK_GETTOP(hak); /* sentinnel */
				HAK_STACK_POP(hak);

				t2 = HAK_STACK_GETTOP(hak); /* head cons */
				if (HAK_UNLIKELY(!HAK_IS_CONS(hak, t2)))
				{
					hak_seterrbfmt(hak, HAK_EINTERN, "internal error - invalid vm state detected in pop_into_cons");
					goto oops;
				}

				if (t3 == hak->_nil)
				{
					((hak_oop_oop_t)t2)->slot[0] = t1;
				}
				else
				{
					hak_oop_t t;

					hak_pushvolat(hak, &t3);
					t = hak_makecons(hak, t1, hak->_nil);
					hak_popvolat(hak);
					if (HAK_UNLIKELY(!t)) goto oops_with_errmsg_supplement;

					((hak_oop_oop_t)t3)->slot[1] = t;
				}

				break;
			}

			case HAK_CODE_POP_INTO_CONS_CDR:
			{
				hak_oop_t t1, t2, t3;
				LOG_INST_0(hak, "pop_into_cons_end");

				t1 = HAK_STACK_GETTOP(hak); /* value to store */
				HAK_STACK_POP(hak);

				t3 = HAK_STACK_GETTOP(hak); /* sentinnel */
				HAK_STACK_POP(hak);

				t2 = HAK_STACK_GETTOP(hak); /* head cons */
				if (HAK_UNLIKELY(!HAK_IS_CONS(hak, t2)))
				{
					hak_seterrbfmt(hak, HAK_EINTERN, "internal error - invalid vm state detected in pop_into_cons");
					goto oops;
				}

				if (t3 == hak->_nil)
				{
					((hak_oop_oop_t)t2)->slot[1] = t1;
				}
				else
				{
					((hak_oop_oop_t)t3)->slot[1] = t1;
				}

				/* no push back of the sentinnel */
				break;
			}

			/* -------------------------------------------------------- */
			case HAK_CODE_DUP_STACKTOP:
			{
				hak_oop_t t;
				LOG_INST_0(hak, "dup_stacktop");
				HAK_ASSERT(hak, !HAK_STACK_IS_EMPTY(hak));
				t = HAK_STACK_GETTOP(hak);
				HAK_STACK_PUSH(hak, t);
				break;
			}

			case HAK_CODE_POP_STACKTOP:
				LOG_INST_0(hak, "pop_stacktop");
				HAK_ASSERT(hak, !HAK_STACK_IS_EMPTY(hak));

				/* at the top level, the value is just popped off the stack
				 * after evaluation of an expression. so it's likely the
				 * return value of the last expression unless explicit
				 * returning is performed */
				hak->last_retv = HAK_STACK_GETTOP(hak);
				HAK_STACK_POP(hak);
				break;

			case HAK_CODE_RETURN_STACKTOP:
/* [NOTE] this implements the non-local return. the non-local return is not compatible with stack based try-catch implementation.
 * [TODO] can make it compatiable? */
				LOG_INST_0(hak, "return_stacktop");
				return_value = HAK_STACK_GETTOP(hak);
				HAK_STACK_POP(hak);
				goto handle_return;

			case HAK_CODE_RETURN_RECEIVER:
				LOG_INST_0(hak, "return_receiver");
				return_value = hak->active_context->receiver;

			handle_return:
				hak->last_retv = return_value;
				if (do_return_from_home(hak, return_value, fetched_instruction_pointer) <= -1) goto oops_with_errmsg_supplement;
				break;

			case HAK_CODE_RETURN_FROM_BLOCK:
				LOG_INST_0(hak, "return_from_block");

				HAK_ASSERT(hak, HAK_IS_CONTEXT(hak, hak->active_context));
				hak->last_retv = HAK_STACK_GETTOP(hak); /* get the stack top */
				do_return_from_block(hak);

				break;

			case HAK_CODE_MAKE_FUNCTION:
			{
				hak_oop_function_t funcobj;
				hak_oow_t b3, b4, x;
				hak_oow_t joff;

				/* b1 - block temporaries mask
				 * b2 - block temporaries mask
				 * b3 - literal frame index to name
				 * b4 - literal frame base
				 * b5 - literal frame size */
				FETCH_PARAM_CODE_TO(hak, b1);
				FETCH_PARAM_CODE_TO(hak, x);
				b1 = (b1 << (8 * HAK_CODE_LONG_PARAM_SIZE)) | x;

				FETCH_PARAM_CODE_TO(hak, b2);
				FETCH_PARAM_CODE_TO(hak, x);
				b2 = (b2 << (8 * HAK_CODE_LONG_PARAM_SIZE)) | x;

				FETCH_PARAM_CODE_TO(hak, b3);
				FETCH_PARAM_CODE_TO(hak, b4);

				LOG_INST_8(hak, "make_function %zu %zu %zu %zu %zu %zu %zu %zu",
					GET_BLK_MASK_INSTA(b1),
					GET_BLK_MASK_VA(b1),
					GET_BLK_MASK_NARGS(b1),
					GET_BLK_MASK_NRVARS(b1),
					GET_BLK_MASK_NLVARS(b1),
					b2, b3, b4);

				HAK_ASSERT(hak, b1 >= 0);

				/* the MAKE_FUNCTION instruction is followed by the long JUMP_FORWARD_X instruction.
				 * i can decode the instruction and get the size of instructions of the block context */
				HAK_ASSERT(hak, hak->active_code[hak->ip] == HAK_CODE_JUMP_FORWARD_X);
			#if (HAK_CODE_LONG_PARAM_SIZE == 2)
				/* don't use the FETCH_BYTE_CODE() macros or the like to not increment hak->ip */
				joff = hak->active_code[hak->ip + 1];
				joff = (joff << 8) | hak->active_code[hak->ip + 2];
				/* copy the byte codes from the active context to the new context */
				funcobj = make_function(hak, b4, &hak->active_code[hak->ip + 3], joff, HAK_NULL);
			#else
				joff = hak->active_code[hak->ip + 1];
				funcobj = make_function(hak, b4, &hak->active_code[hak->ip + 2], joff, HAK_NULL);
			#endif
				if (HAK_UNLIKELY(!funcobj)) goto oops;

				fill_function_data(hak, funcobj, b1, b2, hak->active_context, &hak->active_function->literal_frame[b3], b4);

				/* push the new function to the stack of the active context */
				HAK_STACK_PUSH(hak, (hak_oop_t)funcobj);
				break;
			}

			case HAK_CODE_MAKE_BLOCK:
			{
				hak_oow_t x;
				hak_oop_block_t blkobj;

				/* block temporaries mask (extended long)
				 * literal frame index to name (extended long) */
				FETCH_PARAM_CODE_TO(hak, b1);
				FETCH_PARAM_CODE_TO(hak, x);
				b1 = (b1 << (8 * HAK_CODE_LONG_PARAM_SIZE)) | x;

				FETCH_PARAM_CODE_TO(hak, b2);
				FETCH_PARAM_CODE_TO(hak, x);
				b2 = (b2 << (8 * HAK_CODE_LONG_PARAM_SIZE)) | x;

				LOG_INST_6(hak, "make_block %zu %zu %zu %zu %zu %zu",
					GET_BLK_MASK_INSTA(b1),
					GET_BLK_MASK_VA(b1),
					GET_BLK_MASK_NARGS(b1),
					GET_BLK_MASK_NRVARS(b1),
					GET_BLK_MASK_NLVARS(b1),
					b2);

				HAK_ASSERT(hak, b1 >= 0);

				blkobj = make_compiled_block(hak);
				if (HAK_UNLIKELY(!blkobj)) goto oops;

				/* the long forward jump instruction has the format of
				 *   11000100 KKKKKKKK or 11000100 KKKKKKKK KKKKKKKK
				 * depending on HAK_CODE_LONG_PARAM_SIZE. change 'ip' to point to
				 * the instruction after the jump. */
				fill_block_data(hak, blkobj, b1, b2, hak->ip + HAK_CODE_LONG_PARAM_SIZE + 1, hak->active_context);

				/* push the new block context to the stack of the active context */
				HAK_STACK_PUSH(hak, (hak_oop_t)blkobj);
				break;
			}

			case HAK_CODE_NOOP:
				/* do nothing */
				LOG_INST_0(hak, "noop");
				break;

			default:
				HAK_LOG1(hak, HAK_LOG_IC | HAK_LOG_FATAL, "Fatal error - unknown byte code 0x%zx\n", bcode);
				hak_seterrnum(hak, HAK_EINTERN);
				goto oops;
		}
	}

done:
	hak->gci.lazy_sweep = 1;

	vm_cleanup(hak);
#if defined(HAK_PROFILE_VM)
	HAK_LOG1(hak, HAK_LOG_IC | HAK_LOG_INFO, "EXEC OK - TOTAL INST COUTNER = %zu\n", inst_counter);
#endif
	return 0;

oops_with_errmsg_supplement:
	supplement_errmsg(hak, fetched_instruction_pointer);

oops:
	hak->gci.lazy_sweep = 1;

	vm_cleanup(hak);
#if defined(HAK_PROFILE_VM)
	HAK_LOG1(hak, HAK_LOG_IC | HAK_LOG_INFO, "EXEC ERROR - TOTAL INST COUTNER = %zu\n", inst_counter);
#endif

	return -1;
}

hak_oop_t hak_execute (hak_t* hak)
{
	hak_oop_function_t funcobj;
	int n;
	hak_bitmask_t log_default_type_mask;

	HAK_ASSERT(hak, hak->code.bc.len < HAK_SMOOI_MAX); /* asserted by the compiler */

	log_default_type_mask = hak->log.default_type_mask;
	hak->log.default_type_mask |= HAK_LOG_VM;

	HAK_ASSERT(hak, hak->initial_context == HAK_NULL);
	HAK_ASSERT(hak, hak->active_context == HAK_NULL);

	/* the code generated doesn't cater for its use as an initial funtion.
	 * mutate the generated code so that the intiail function can break
	 * out of the execution loop in execute() smoothly */

	if (hak->code.bc.len > 0)
	{
		HAK_ASSERT(hak, hak->code.bc.ptr[hak->code.bc.len - 1] == HAK_CODE_POP_STACKTOP);
	#if 1
		/* append RETURN_FROM_BLOCK
		 * if (hak_emitbyteinstruction(hak, HAK_CODE_RETURN_FROM_BLOCK) <= -1) return -1;*/
		/* substitute RETURN_FROM_BLOCK for POP_STACKTOP) */
		hak->code.bc.ptr[hak->code.bc.len - 1] = HAK_CODE_RETURN_FROM_BLOCK;
	#else
		/* substitute RETURN_STACKTOP for POP_STACKTOP) */
		hak->code.bc.ptr[hak->code.bc.len - 1] = HAK_CODE_RETURN_STACKTOP;
	#endif
	}

	/* create a virtual function object that holds the byte code generated plus the literal frame */
	funcobj = make_function(hak, hak->code.lit.len, hak->code.bc.ptr, hak->code.bc.len, hak->code.dbgi);
	if (HAK_UNLIKELY(!funcobj)) return HAK_NULL;

	/* pass nil for the home context of the initial function */
	fill_function_data(hak, funcobj, ENCODE_BLK_MASK(0,0,0,0,hak->code.ngtmprs), HAK_TYPE_MAX(hak_oow_t), (hak_oop_context_t)hak->_nil, hak->code.lit.arr->slot, hak->code.lit.len);

	hak->initial_function = funcobj; /* the initial function is ready */

#if 0
	/* unless the system is buggy, hak->proc_map_used should be 0.
	 * the standard library terminates all processes before halting.
	 *
	 * [EXPERIMENTAL]
	 * if you like the process allocation to start from 0, uncomment
	 * the following 'if' block */
	if (hak->proc_map_capa > 0 && hak->proc_map_used == 0)
	{
		/* rechain the process map. it must be compatible with prepare_to_alloc_pid().
		 * by placing the low indiced slot at the beginning of the free list,
		 * the special processes (main_proc, gcfin_proc, ossig_proc) are allocated
		 * with low process IDs. */
		hak_ooi_t i, j;

		hak->proc_map_free_first = 0;
		for (i = 0, j = 1; j < hak->proc_map_capa; i++, j++)
		{
			hak->proc_map[i] = HAK_SMOOI_TO_OOP(j);
		}
		hak->proc_map[i] = HAK_SMOOI_TO_OOP(-1);
		hak->proc_map_free_last = i;
	}
#endif

	n = start_initial_process_and_context(hak, 0, hak->code.ngtmprs); /*  set up the initial context over the initial function */
	if (n >= 0)
	{
		hak->last_retv = hak->_nil;
		n = execute(hak);
		HAK_INFO1 (hak, "RETURNED VALUE - %O\n", hak->last_retv);
	}

	hak->initial_context = HAK_NULL;
	hak->active_context = HAK_NULL;

	HAK_ASSERT(hak, hak->processor->active == hak->nil_process);
	HAK_ASSERT(hak, HAK_OOP_TO_SMOOI(hak->processor->total_count) == 0);
	HAK_ASSERT(hak, HAK_OOP_TO_SMOOI(hak->processor->runnable.count) == 0);
	HAK_ASSERT(hak, HAK_OOP_TO_SMOOI(hak->processor->suspended.count) == 0);

	LOAD_ACTIVE_SP(hak); /* sync hak->nil_process->sp with hak->sp */
	HAK_ASSERT(hak, hak->sp == -1);

#if defined(HAK_PROFILE_VM)
	HAK_LOG2(hak, HAK_LOG_IC | HAK_LOG_INFO, "GC - gci.bsz: %zu, gci.stack.max: %zu\n", hak->gci.bsz, hak->gci.stack.max);
	if (hak->heap->xma) hak_xma_dump (hak->heap->xma, xma_dumper, hak);
	HAK_LOG2(hak, HAK_LOG_IC | HAK_LOG_INFO, "GC - gci.stat.alloc: %ld.%09u\n", (unsigned long int)hak->gci.stat.alloc.sec, (unsigned int)hak->gci.stat.alloc.nsec);
	HAK_LOG2(hak, HAK_LOG_IC | HAK_LOG_INFO, "GC - gci.stat.mark: %ld.%09u\n", (unsigned long int)hak->gci.stat.mark.sec, (unsigned int)hak->gci.stat.mark.nsec);
	HAK_LOG2(hak, HAK_LOG_IC | HAK_LOG_INFO, "GC - gci.stat.sweep: %ld.%09u\n", (unsigned long int)hak->gci.stat.sweep.sec, (unsigned int)hak->gci.stat.sweep.nsec);
#endif

	hak->log.default_type_mask = log_default_type_mask;
	return (n <= -1)? HAK_NULL: hak->last_retv;
}

void hak_abort (hak_t* hak)
{
	hak->abort_req = 1;
}

/* ------------------------------------------------------------------ */

hak_pfrc_t hak_pf_process_current (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	HAK_STACK_SETRET(hak, nargs, (hak_oop_t)hak->processor->active);
	return HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_process_fork (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_block_t blk;
	hak_oop_context_t newctx;
	hak_oop_process_t newprc;
	int x;

	blk = (hak_oop_block_t)HAK_STACK_GETARG(hak, nargs, 0);
	if (!HAK_IS_COMPILED_BLOCK(hak, blk))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not compiled block - %O", blk);
		return HAK_PF_FAILURE;
	}

	/* (defun x(a b) ...)
	 * (fork x 1 2)
	 * among three arguments to fork, the first is the function block.
	 * the remaining two should become arguments to the function block.
	 * pass nargs_offset of 1 to prepare_new_context() to achieve it.
	 */
	x = prepare_new_context(
		hak,
		blk,
		nargs, /* nargs */
		1, /* nargs_offset */
		0, /* number of return variables expected */
		1, /* copy_args */
		0, /* is_msgsend */
		0, /* msg_ivaroff */
		&newctx);
	if (HAK_UNLIKELY(x <= -1)) return HAK_PF_FAILURE;

	HAK_ASSERT(hak, (hak_oop_t)newctx->sender == hak->_nil);
	newctx->home = (hak_oop_context_t)hak->_nil; /* the new context is the initial context in the new process. so reset it to nil */

	hak_pushvolat(hak, (hak_oop_t*)&newctx);
	newprc = make_process(hak, newctx);
	hak_popvolat(hak);
	if (HAK_UNLIKELY(!newprc)) return HAK_PF_FAILURE;

	chain_into_processor(hak, newprc, HAK_PROCESS_STATE_RUNNABLE);

	HAK_STACK_SETRET(hak, nargs, (hak_oop_t)newprc);
	return HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_process_resume (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_process_t prc;

	prc = (hak_oop_process_t)HAK_STACK_GETARG(hak, nargs, 0);
	if (!HAK_IS_PROCESS(hak, prc))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not process - %O", prc);
		return HAK_PF_FAILURE;
	}

	resume_process(hak, prc);
	return  HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_process_suspend (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_process_t prc;

	if (nargs >= 1)
	{
		prc = (hak_oop_process_t)HAK_STACK_GETARG(hak, nargs, 0);
		if (!HAK_IS_PROCESS(hak, prc))
		{
			hak_seterrbfmt(hak, HAK_EINVAL, "parameter not process - %O", prc);
			return HAK_PF_FAILURE;
		}
	}
	else
	{
		prc = hak->processor->active;
	}

	suspend_process(hak, prc);
	return  HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_process_terminate (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_process_t prc;

	if (nargs >= 1)
	{
		prc = (hak_oop_process_t)HAK_STACK_GETARG(hak, nargs, 0);
		if (!HAK_IS_PROCESS(hak, prc))
		{
			hak_seterrbfmt(hak, HAK_EINVAL, "parameter not process - %O", prc);
			return HAK_PF_FAILURE;
		}
	}
	else
	{
		prc = hak->processor->active;
	}

	terminate_process(hak, prc);
	return  HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_process_terminate_all (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	terminate_all_processes(hak);
	return  HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_process_yield (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	yield_process(hak, hak->processor->active);
	return  HAK_PF_SUCCESS;
}

/* ------------------------------------------------------------------ */
hak_pfrc_t hak_pf_semaphore_new (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_semaphore_t sem;

	/*sem = (hak_oop_semaphore_t)hak_allocoopobj(hak, HAK_BRAND_SEMAPHORE, HAK_SEMAPHORE_NAMED_INSTVARS);*/
	sem = (hak_oop_semaphore_t)hak_instantiate(hak, hak->c_semaphore, HAK_NULL, 0);
	if (HAK_UNLIKELY(!sem))
	{
		const hak_ooch_t* oldmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt(hak, hak->errnum,
			"unable to instantiate %O - %js", hak->c_semaphore->name, oldmsg);
		return HAK_PF_FAILURE;
	}

	sem->count = HAK_SMOOI_TO_OOP(0);
/* TODO: sem->signal_action? */
	/* other fields are all set to nil */

	HAK_STACK_SETRET(hak, nargs, (hak_oop_t)sem);
	return HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_semaphore_signal (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_semaphore_t sem;
	hak_oop_t sec, nsec;
	hak_ntime_t now, ft;

	sem = (hak_oop_semaphore_t)HAK_STACK_GETARG(hak, nargs, 0);
	if (!HAK_IS_SEMAPHORE(hak, sem))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not semaphore - %O", sem);
		return HAK_PF_FAILURE;
	}

	if (nargs <= 1)
	{
		/* signal_semaphore() may change the active process though the
		 * implementation as of this writing makes runnable the process waiting
		 * on the signal to be processed. it is safer to set the return value
		 * before calling signal_sempahore() */
		HAK_STACK_SETRET(hak, nargs, (hak_oop_t)sem);
		signal_semaphore(hak, sem);
		return HAK_PF_SUCCESS;
	}

	sec = HAK_STACK_GETARG(hak, nargs, 1);
	nsec = (nargs >= 3? HAK_STACK_GETARG(hak, nargs, 2): HAK_SMOOI_TO_OOP(0));

	if (!HAK_OOP_IS_SMOOI(sec))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "invalid second - %O", sec);
		return HAK_PF_FAILURE;
	}

	if (!HAK_OOP_IS_SMOOI(sec))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "invalid nanosecond - %O", nsec);
		return HAK_PF_FAILURE;
	}

#if 0
	if (sem->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_TIMED))
	{
		HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.timed.index) && HAK_OOP_TO_SMOOI(sem->u.timed.index) >= 0);

		/* if the semaphore is already been added. remove it first */
		delete_from_sem_heap(hak, HAK_OOP_TO_SMOOI(sem->u.timed.index));
		HAK_ASSERT(hak, sem->subtype == hak->_nil && sem->u.timed.index == hak->_nil);

		/*
		Is this more desired???
		HAK_STACK_SETRET(hak, nargs, hak->_false);
		return HAK_PF_SUCCESS;
		*/
	}
#else
	if (sem->subtype != hak->_nil)
	{
		if (sem->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_IO))
		{
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.io.index) && HAK_OOP_TO_SMOOI(sem->u.io.index) >= 0);
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.io.handle) && HAK_OOP_TO_SMOOI(sem->u.io.handle) >= 0);
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.io.type));
			hak_seterrbfmt(hak, HAK_EINVAL, "semaphore already linked with a handle %zd", HAK_OOP_TO_SMOOI(sem->u.io.handle));
		}
		else
		{
			HAK_ASSERT(hak, sem->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_TIMED));
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.timed.index) && HAK_OOP_TO_SMOOI(sem->u.timed.index) >= 0);
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.timed.ftime_sec));
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.timed.ftime_nsec));
			hak_seterrbfmt(hak, HAK_EINVAL, "semaphore already activated for timer");
		}

		return HAK_PF_FAILURE;
	}
#endif
	/* this code assumes that the monotonic clock returns a small value
	 * that can fit into a SmallInteger, even after some additions. */
	vm_gettime(hak, &now);
	HAK_ADD_NTIME_SNS (&ft, &now, HAK_OOP_TO_SMOOI(sec), HAK_OOP_TO_SMOOI(nsec));
	if (ft.sec < 0 || ft.sec > HAK_SMOOI_MAX)
	{
		/* soft error - cannot represent the expiry time in a small integer. */
		HAK_LOG2(hak, HAK_LOG_PRIMITIVE | HAK_LOG_ERROR,
			"Error - time (%ld) out of range(0 - %zd) when adding a timed semaphore\n",
			(unsigned long int)ft.sec, (hak_ooi_t)HAK_SMOOI_MAX);

		hak_seterrnum(hak, HAK_ERANGE);
		return HAK_PF_FAILURE;
	}

	sem->u.timed.ftime_sec = HAK_SMOOI_TO_OOP(ft.sec);
	sem->u.timed.ftime_nsec = HAK_SMOOI_TO_OOP(ft.nsec);

	if (add_to_sem_heap(hak, sem) <= -1) return HAK_PF_FAILURE;
	HAK_ASSERT(hak, sem->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_TIMED));

	HAK_STACK_SETRET(hak, nargs, (hak_oop_t)sem);
	return HAK_PF_SUCCESS;
}

static hak_pfrc_t __semaphore_signal_on_io (hak_t* hak, hak_ooi_t nargs, hak_semaphore_io_type_t io_type)
{
	hak_oop_semaphore_t sem;
	hak_oop_t fd;

	sem = (hak_oop_semaphore_t)HAK_STACK_GETARG(hak, nargs, 0);
	if (!HAK_IS_SEMAPHORE(hak, sem))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not semaphore - %O", sem);
		return HAK_PF_FAILURE;
	}

	fd = HAK_STACK_GETARG(hak, nargs, 1);

	if (!HAK_OOP_IS_SMOOI(fd))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "handle not a small integer - %O", fd);
		return HAK_PF_FAILURE;
	}

	if (sem->subtype != hak->_nil)
	{
		if (sem->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_IO))
		{
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.io.index) && HAK_OOP_TO_SMOOI(sem->u.io.index) >= 0);
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.io.handle) && HAK_OOP_TO_SMOOI(sem->u.io.handle) >= 0);
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.io.type));
			hak_seterrbfmt(hak, HAK_EINVAL, "semaphore already linked with a handle %zd", HAK_OOP_TO_SMOOI(sem->u.io.handle));
		}
		else
		{
			HAK_ASSERT(hak, sem->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_TIMED));
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.timed.index) && HAK_OOP_TO_SMOOI(sem->u.timed.index) >= 0);
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.timed.ftime_sec));
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.timed.ftime_nsec));
			hak_seterrbfmt(hak, HAK_EINVAL, "semaphore already activated for timer");
		}

		return HAK_PF_FAILURE;
	}

	if (add_sem_to_sem_io_tuple(hak, sem, HAK_OOP_TO_SMOOI(fd), io_type) <= -1)
	{
		const hak_ooch_t* oldmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt(hak, hak->errnum, "unable to add the handle %zd to the multiplexer for %hs - %js", HAK_OOP_TO_SMOOI(fd), io_type_str[io_type], oldmsg);
		return HAK_PF_FAILURE;
	}

	HAK_STACK_SETRET(hak, nargs, (hak_oop_t)sem);
	return HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_semaphore_signal_on_input (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	return __semaphore_signal_on_io(hak, nargs, HAK_SEMAPHORE_IO_TYPE_INPUT);
}

hak_pfrc_t hak_pf_semaphore_signal_on_output (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	return __semaphore_signal_on_io(hak, nargs, HAK_SEMAPHORE_IO_TYPE_OUTPUT);
}

#if 0
hak_pfrc_t hak_pf_semaphore_signal_on_gcfin (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_semaphore_t sem;

	sem = (hak_oop_semaphore_t)HAK_STACK_GETRCV(hak, nargs);
	HAK_PF_CHECK_RCV(hak, hak_iskindof(hak, (hak_oop_t)sem, hak->_semaphore));

/* TODO: should i prevent overwriting? */
	hak->sem_gcfin = sem;

	HAK_STACK_SETRETTORCV(hak, nargs); /* ^self */
	return HAK_PF_SUCCESS;
}
#endif

hak_pfrc_t hak_pf_semaphore_wait (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_semaphore_t sem;

	sem = (hak_oop_semaphore_t)HAK_STACK_GETARG(hak, nargs, 0);
	if (!HAK_IS_SEMAPHORE(hak, sem))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not semaphore - %O", sem);
		return HAK_PF_FAILURE;
	}

	if (!can_await_semaphore(hak, sem))
	{
		hak_seterrbfmt(hak, HAK_EPERM, "not allowed to wait on a semaphore that belongs to a semaphore group");
		return HAK_PF_FAILURE;
	}

	/* i must set the return value before calling await_semaphore().
	 * await_semaphore() may switch the active process and the stack
	 * manipulation macros target at the active process. i'm not supposed
	 * to change the return value of a new active process. */
	HAK_STACK_SETRET(hak, nargs, (hak_oop_t)sem);

	await_semaphore(hak, sem);
	return HAK_PF_SUCCESS;
}


hak_pfrc_t hak_pf_semaphore_unsignal (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	/* remove a semaphore from processor's signal scheduling.
	 * it takes no effect on a plain semaphore. */
	hak_oop_semaphore_t sem;

	sem = (hak_oop_semaphore_t)HAK_STACK_GETARG(hak, nargs, 0);
	if (!HAK_IS_SEMAPHORE(hak, sem))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not semaphore - %O", sem);
		return HAK_PF_FAILURE;
	}

/*
TODO: add this back if gcfin support is added
	if (sem == hak->sem_gcfin)
	{
		hak->sem_gcfin = (hak_oop_semaphore_t)hak->_nil;
	}
*/

	if (sem->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_TIMED))
	{
		/* the semaphore is in the timed semaphore heap */
		HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.timed.index) && HAK_OOP_TO_SMOOI(sem->u.timed.index) >= 0);
		delete_from_sem_heap(hak, HAK_OOP_TO_SMOOI(sem->u.timed.index));
		HAK_ASSERT(hak, sem->u.timed.index == hak->_nil);
	}
	else if (sem->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_IO))
	{
		hak_oop_process_t wp; /* waiting process */

		/* the semaphore is associated with IO */
		HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.io.index) && HAK_OOP_TO_SMOOI(sem->u.io.index) >= 0);
		HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.io.type));
		HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.io.handle) && HAK_OOP_TO_SMOOI(sem->u.io.handle) >= 0);

		if (delete_sem_from_sem_io_tuple(hak, sem, 0) <= -1)
		{
			const hak_ooch_t* oldmsg = hak_backuperrmsg(hak);
			hak_seterrbfmt(hak, hak->errnum, "cannot delete the handle %zd from the multiplexer - %js", HAK_OOP_TO_SMOOI(sem->u.io.handle), oldmsg);
			return HAK_PF_FAILURE;
		}

		HAK_ASSERT(hak, (hak_oop_t)sem->u.io.index == hak->_nil);
		HAK_ASSERT(hak, (hak_oop_t)sem->u.io.handle == hak->_nil);

		/* the semaphore gets changed to a plain semaphore after
		 * delete_sem_from_sem_io_tuple(). if there is a process
		 * waiting on this IO semaphore, the process now is treated
		 * as if it's waiting on a plain semaphore. let's adjust
		 * the number of processes waiting on IO semaphores */
		for (wp = sem->waiting.first; (hak_oop_t)wp != hak->_nil; wp = wp->sem_wait.next)
		{
			HAK_ASSERT(hak, hak->sem_io_wait_count > 0);
			hak->sem_io_wait_count--;
		}
	}
	HAK_ASSERT(hak, sem->subtype == hak->_nil);

	HAK_STACK_SETRET(hak, nargs, (hak_oop_t)sem);
	return HAK_PF_SUCCESS;
}

/* ------------------------------------------------------------------ */
hak_pfrc_t hak_pf_semaphore_group_new (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_semaphore_group_t sg;

	/*sg = (hak_oop_semaphore_group_t)hak_allocoopobj(hak, HAK_BRAND_SEMAPHORE_GROUP, HAK_SEMAPHORE_GROUP_NAMED_INSTVARS);*/
	sg = (hak_oop_semaphore_group_t)hak_instantiate(hak, hak->c_semaphore_group, HAK_NULL, 0);
	if (HAK_UNLIKELY(!sg))
	{
		const hak_ooch_t* oldmsg = hak_backuperrmsg(hak);
		hak_seterrbfmt(hak, hak->errnum,
			"unable to instantiate %O - %js", hak->c_semaphore_group->name, oldmsg);
		return HAK_PF_FAILURE;
	}

	sg->sem_io_count = HAK_SMOOI_TO_OOP(0);
	sg->sem_count = HAK_SMOOI_TO_OOP(0);
/* TODO: sem->signal_action? */
	/* other fields are all set to nil */

	HAK_STACK_SETRET(hak, nargs, (hak_oop_t)sg);
	return HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_semaphore_group_add_semaphore (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_semaphore_group_t sg;
	hak_oop_semaphore_t sem;

	sg = (hak_oop_semaphore_group_t)HAK_STACK_GETARG(hak, nargs, 0);
	if (!HAK_IS_SEMAPHORE_GROUP(hak, sg))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not semaphore group - %O", sg);
		return HAK_PF_FAILURE;
	}

	sem = (hak_oop_semaphore_t)HAK_STACK_GETARG(hak, nargs, 1);
	if (!HAK_IS_SEMAPHORE(hak, sem))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not semaphore - %O", sem);
		return HAK_PF_FAILURE;
	}

	if ((hak_oop_t)sem->group == hak->_nil)
	{
		/* the semaphore doesn't belong to a group */
		hak_ooi_t count;
		int sems_idx;

		sems_idx = HAK_OOP_TO_SMOOI(sem->count) > 0? HAK_SEMAPHORE_GROUP_SEMS_SIG: HAK_SEMAPHORE_GROUP_SEMS_UNSIG;
		HAK_APPEND_TO_OOP_LIST(hak, &sg->sems[sems_idx], hak_oop_semaphore_t, sem, grm);
		sem->group = sg;

		count = HAK_OOP_TO_SMOOI(sg->sem_count);
		HAK_ASSERT(hak, count >= 0);
		count++;
		sg->sem_count = HAK_SMOOI_TO_OOP(count);

		if (sem->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_IO))
		{
			/* the semaphore being added is associated with I/O operation. */
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.io.index) &&
			                 HAK_OOP_TO_SMOOI(sem->u.io.index) >= 0 &&
			                 HAK_OOP_TO_SMOOI(sem->u.io.index) < hak->sem_io_tuple_count);

			count = HAK_OOP_TO_SMOOI(sg->sem_io_count);
			HAK_ASSERT(hak, count >= 0);
			count++;
			sg->sem_io_count = HAK_SMOOI_TO_OOP(count);

			if (count == 1)
			{
				/* the first IO semaphore is being added to the semaphore group.
				 * but there are already processes waiting on the semaphore group.
				 *
				 * for instance,
				 *  [Process 1]
				 *     sg := SemaphoreGroup new.
				 *     sg wait.
				 *  [Process 2]
				 *     sg addSemaphore: (Semaphore new).
				 */

				hak_oop_process_t wp;
				/* TODO: add sem_wait_count to process. no traversal... */
				for (wp = sg->waiting.first; (hak_oop_t)wp != hak->_nil; wp = wp->sem_wait.next)
				{
					hak->sem_io_wait_count++;
					HAK_DEBUG1 (hak, "hak_pf_semaphore_group_add_semaphore - raised sem_io_wait_count to %zu\n", hak->sem_io_wait_count);
				}
			}
		}

		HAK_STACK_SETRET(hak, nargs, (hak_oop_t)sg);
	}
	else if (sem->group == sg)
	{
		/* do nothing. don't change the group of the semaphore */
		HAK_STACK_SETRET(hak, nargs, (hak_oop_t)sg);
	}
	else
	{
		/* the semaphore belongs to a group already */
/* TODO: is it better to move this semaphore to the new group? */
		hak_seterrbfmt(hak, HAK_EPERM, "not allowed to relocate a semaphore to a different group");
		return HAK_PF_FAILURE;
	}

	return HAK_PF_SUCCESS;
}

hak_pfrc_t hak_pf_semaphore_group_remove_semaphore (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_semaphore_group_t sg;
	hak_oop_semaphore_t sem;
	hak_ooi_t count;

	sg = (hak_oop_semaphore_group_t)HAK_STACK_GETARG(hak, nargs, 0);
	if (!HAK_IS_SEMAPHORE_GROUP(hak, sg))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not semaphore group - %O", sg);
		return HAK_PF_FAILURE;
	}

	sem = (hak_oop_semaphore_t)HAK_STACK_GETARG(hak, nargs, 1);
	if (!HAK_IS_SEMAPHORE(hak, sem))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not semaphore - %O", sem);
		return HAK_PF_FAILURE;
	}

	if (sem->group == sg)
	{
		int sems_idx;

#if 0
		if ((hak_oop_t)sg->waiting.first != hak->_nil)
		{
			/* there is a process waiting on this semaphore group.
			 * i don't allow a semaphore to be removed from the group.
			 * i want to dodge potential problems arising when removal is allowed.
			 *
			 * for instance, consider this psuedo code.
			 *   sg addSemaphore: s
			 *   [ sg wait ] fork.
			 *   [ sg wait ] fork.
			 *   [ sg wait ] fork.
			 *   sg removeSemaphore: s.
			 *
			 */
			hak_seterrbfmt(hak, HAK_EPERM, "not allowed to remove a semaphore from a group being waited on");
			return HAK_PF_FAILURE;
		}
#endif

		sems_idx = HAK_OOP_TO_SMOOI(sem->count) > 0? HAK_SEMAPHORE_GROUP_SEMS_SIG: HAK_SEMAPHORE_GROUP_SEMS_UNSIG;
		HAK_DELETE_FROM_OOP_LIST(hak, &sg->sems[sems_idx], sem, grm);
		sem->grm.prev = (hak_oop_semaphore_t)hak->_nil;
		sem->grm.next = (hak_oop_semaphore_t)hak->_nil;
		sem->group = (hak_oop_semaphore_group_t)hak->_nil;

		count = HAK_OOP_TO_SMOOI(sg->sem_count);
		HAK_ASSERT(hak, count > 0);
		count--;
		sg->sem_count = HAK_SMOOI_TO_OOP(count);

		if (sem->subtype == HAK_SMOOI_TO_OOP(HAK_SEMAPHORE_SUBTYPE_IO))
		{
			HAK_ASSERT(hak, HAK_OOP_IS_SMOOI(sem->u.io.index) &&
			                HAK_OOP_TO_SMOOI(sem->u.io.index) >= 0 &&
			                HAK_OOP_TO_SMOOI(sem->u.io.index) < hak->sem_io_tuple_count);

			count = HAK_OOP_TO_SMOOI(sg->sem_io_count);
			HAK_ASSERT(hak, count > 0);
			count--;
			sg->sem_io_count = HAK_SMOOI_TO_OOP(count);

			if (count == 0)
			{
				hak_oop_process_t wp;
				/* TODO: add sem_wait_count to process. no traversal... */
				for (wp = sg->waiting.first; (hak_oop_t)wp != hak->_nil; wp = wp->sem_wait.next)
				{
					HAK_ASSERT(hak, hak->sem_io_wait_count > 0);
					hak->sem_io_wait_count--;
					HAK_DEBUG1 (hak, "hak_pf_semaphore_group_remove_semaphore - lowered sem_io_wait_count to %zu\n", hak->sem_io_wait_count);
				}
			}
		}

		HAK_STACK_SETRET(hak, nargs, (hak_oop_t)sg);
		return HAK_PF_SUCCESS;
	}

	/* it doesn't belong to a group or belongs to a different group */
	hak_seterrbfmt(hak, HAK_EPERM, "not allowed to remove a semaphore from a non-owning group");
	return HAK_PF_FAILURE;
}

hak_pfrc_t hak_pf_semaphore_group_wait (hak_t* hak, hak_mod_t* mod, hak_ooi_t nargs)
{
	hak_oop_semaphore_group_t sg;
	hak_oop_t sem;


	sg = (hak_oop_semaphore_group_t)HAK_STACK_GETARG(hak, nargs, 0);
	if (!HAK_IS_SEMAPHORE_GROUP(hak, sg))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "parameter not semaphore group - %O", sg);
		return HAK_PF_FAILURE;
	}

	/* i must set the return value before calling await_semaphore_group().
	 * HAK_STACK_SETRETTORCV() manipulates the stack of the currently active
	 * process(hak->processor->active). hak->processor->active may become
	 * hak->nil_process if the current active process must get suspended.
	 * it is safer to set the return value of the calling method here.
	 * but the arguments and the receiver information will be lost from
	 * the stack from this moment on. */
	HAK_STACK_SETRET(hak, nargs, (hak_oop_t)sg);

	sem = await_semaphore_group(hak, sg);
	if (sem != hak->_nil)
	{
		/* there was a signaled semaphore. the active process won't get
		 * suspended. change the return value of the current process
		 * forcibly to the signaled semaphore */
		HAK_STACK_SETTOP(hak, sem);
	}

	/* the return value will get changed to an actual semaphore signaled
	 * when the semaphore is signaled. see signal_semaphore() */
	return HAK_PF_SUCCESS;
}

