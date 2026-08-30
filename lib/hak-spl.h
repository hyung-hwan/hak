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


#ifndef _HAK_SPL_H_
#define _HAK_SPL_H_

#include <hak-cmn.h>

/** \file
 * This file provides a spinlock.
 *
 * A spinlock is the lock of last resort: it burns processor time while it
 * waits, so a real mutex is preferable wherever one exists. What it has that
 * a mutex does not is that zero is a valid unlocked state. HAK_SPL_INIT is a
 * constant, so a spinlock can be a statically initialised global that is ready
 * before main() runs and needs no library initialisation call to construct it.
 *
 * It must never be taken from a signal handler or an interrupt handler. The
 * atomics it is built from are async-signal-safe, but that is not the binding
 * constraint: a signal delivered to the thread already holding the lock would
 * spin forever on a lock that thread can no longer reach the end of.
 *
 * Keep the guarded region to plain memory access. Spinning while the holder
 * sits in a system call wastes exactly the time the spinlock was chosen to
 * save.
 *
 * HAK_SUPPORT_SPL is left undefined if no implementation fits the target, so
 * a caller can fall back to something else:
 *
 * \code
 * #if defined(HAK_SUPPORT_SPL)
 * static hak_spl_t lck = HAK_SPL_INIT;
 * hak_spl_lock(&lck);
 * ...
 * hak_spl_unlock(&lck);
 * #endif
 * \endcode
 *
 * Define HAK_SPL_UNSUPPORTED_ERROR to turn an unsupported target into a
 * compile-time error instead.
 */

#define HAK_SUPPORT_SPL

typedef volatile hak_uint32_t hak_spl_t;

#define HAK_SPL_INIT (0)

#if defined(HAK_HAVE_INLINE)
	static HAK_INLINE void hak_spl_init (hak_spl_t* spl) { *spl = HAK_SPL_INIT; }
#else
#	define hak_spl_init(spl) ((*(spl)) = HAK_SPL_INIT)
#endif

/* hint to the processor that this is a spin-wait, and give up the timeslice
 * where that is cheap to do. without it a waiter can starve the holder on a
 * single processor. */
#if defined(_WIN32)
#	define HAK_SPL_RELAX() Sleep(0)
#elif defined(__OS2__)
#	define HAK_SPL_RELAX() DosSleep(0)
#elif defined(__GNUC__) && (defined(__x86_64) || defined(__amd64) || defined(__i386) || defined(i386))
	/* "rep; nop" is the pause instruction, and decodes as a plain nop on
	 * processors that predate it, so it is safe unconditionally. */
#	define HAK_SPL_RELAX() __asm__ __volatile__ ("rep; nop" : : : "memory")
#else
#	define HAK_SPL_RELAX() ((void)0)
#endif

/* __sync_lock_test_and_set()/__sync_lock_release() are the pair gcc documents
 * for building a spinlock: the first is an acquire-barrier exchange, the second
 * a release-barrier store. hak-cmn.h probes them with __has_builtin, which is
 * itself newer than the builtins are, so accept any gcc from 4.1 as well. That
 * covers every architecture gcc targets, leaving the hand-written arms below
 * for compilers older than that. */
#if defined(HAK_HAVE_SYNC_LOCK_TEST_AND_SET) && defined(HAK_HAVE_SYNC_LOCK_RELEASE)
#	define HAK_SPL_USE_SYNC_BUILTINS
#elif defined(__GNUC__) && ((__GNUC__ > 4) || (__GNUC__ == 4 && __GNUC_MINOR__ >= 1))
#	define HAK_SPL_USE_SYNC_BUILTINS
#endif

#if defined(HAK_SPL_USE_SYNC_BUILTINS)
	/* =======================================================================
	 * COMPILERS WITH BUILTIN ATOMICS
	 * ======================================================================= */

#if defined(HAK_HAVE_INLINE)
	static HAK_INLINE_ALWAYS int hak_spl_trylock (hak_spl_t* spl) { return !__sync_lock_test_and_set(spl, 1); }
	static HAK_INLINE_ALWAYS void hak_spl_lock (hak_spl_t* spl) { while (__sync_lock_test_and_set(spl, 1)) HAK_SPL_RELAX(); }
	static HAK_INLINE_ALWAYS void hak_spl_unlock (hak_spl_t* spl) { __sync_lock_release(spl); }
#else
#	define hak_spl_trylock(spl) (!__sync_lock_test_and_set(spl, 1))
#	define hak_spl_lock(spl) do { while (__sync_lock_test_and_set(spl, 1)) HAK_SPL_RELAX(); } while(0)
#	define hak_spl_unlock(spl) (__sync_lock_release(spl))
#endif

#elif defined(_WIN32)
	/* =======================================================================
	 * WIN32 WITHOUT GCC - MSVC AND FRIENDS
	 *
	 * InterlockedCompareExchange() has been available since NT 3.51 and
	 * carries a full barrier, so this arm holds for the whole range this
	 * file targets. It is the reason the header exists: _WIN32 runs a
	 * ticker thread but has no pthreads, and CRITICAL_SECTION cannot be
	 * initialised statically.
	 * ======================================================================= */

#if defined(HAK_HAVE_INLINE)
	static HAK_INLINE_ALWAYS int hak_spl_trylock (hak_spl_t* spl) { return InterlockedCompareExchange((LONG volatile*)spl, 1, 0) == 0; }
	static HAK_INLINE_ALWAYS void hak_spl_lock (hak_spl_t* spl) { while (InterlockedCompareExchange((LONG volatile*)spl, 1, 0)) HAK_SPL_RELAX(); }
	static HAK_INLINE_ALWAYS void hak_spl_unlock (hak_spl_t* spl) { InterlockedExchange((LONG volatile*)spl, 0); }
#else
#	define hak_spl_trylock(spl) (InterlockedCompareExchange((LONG volatile*)(spl), 1, 0) == 0)
#	define hak_spl_lock(spl) do { while (InterlockedCompareExchange((LONG volatile*)(spl), 1, 0)) HAK_SPL_RELAX(); } while(0)
#	define hak_spl_unlock(spl) (InterlockedExchange((LONG volatile*)(spl), 0))
#endif

#elif defined(_SCO_DS)
/* =======================================================================
 * SCO DEVELOPEMENT SYSTEM
 *
 *  NOTE: when the asm macros were indented, the compiler/linker ended up
 *        with undefined symbols. never indent hak_spl_xxx macros.
 * ======================================================================= */
asm int hak_spl_trylock (hak_spl_t* spl)
{
%reg spl
	movl   $1, %eax
	xchgl  (spl), %eax
	xorl   $1, %eax     / return zero on failure, non-zero on success

%mem spl
	movl  spl,  %ecx
	movl  $1,     %eax
	xchgl (%ecx), %eax
	xorl  $1,     %eax  / return zero on failure, non-zero on success
}

/* jump labels cannot be made unique across multiple occurrences of an asm
 * macro, so the loop lives in C instead. */
#define hak_spl_lock(x) do { while (!hak_spl_trylock(x)) HAK_SPL_RELAX(); } while(0)

asm void hak_spl_unlock (hak_spl_t* spl)
{
	/* don't need xchg as movl on an aligned data is atomic */
	/* mfence is 0F AE F0 */
%reg spl
	.byte 0x0F
	.byte 0xAE
	.byte 0xF0
	movl $0, (spl)

%mem spl
	.byte 0x0F
	.byte 0xAE
	.byte 0xF0
	movl spl, %ecx
	movl $0, (%ecx)
}

#elif defined(__GNUC__) && (defined(__x86_64) || defined(__amd64) || defined(__i386) || defined(i386))
	/* =======================================================================
	 * GCC OLDER THAN 4.1 ON X86
	 * ======================================================================= */

	static HAK_INLINE int hak_spl_trylock (hak_spl_t* spl)
	{
		int x = 1;
		__asm__ volatile (
			"xchgl %0, (%2)\n"
			: "=r"(x)
			: "0"(x), "r"(spl)
			: "memory"
		);
		return !x;
	}
	static HAK_INLINE void hak_spl_lock (hak_spl_t* spl)
	{
		while (!hak_spl_trylock(spl)) HAK_SPL_RELAX();
	}
	static HAK_INLINE void hak_spl_unlock (hak_spl_t* spl)
	{
	#if defined(__x86_64) || defined(__amd64)
		__asm__ volatile (
			"mfence\n\t"
			"movl $0, (%0)\n"
			:
			:"r"(spl)
			:"memory"
		);
	#else
		__asm__ volatile (
			"movl $0, (%0)\n"
			:
			:"r"(spl)
			:"memory"
		);
	#endif
	}

#elif defined(__GNUC__) && (defined(__POWERPC__) || defined(__powerpc) || defined(__powerpc__) || defined(__ppc))
	/* =======================================================================
	 * GCC OLDER THAN 4.1 ON POWERPC
	 *
	 * lwarx loads the word and reserves the location; the paired stwcx.
	 * stores only if the reservation still holds.
	 * ======================================================================= */

	static HAK_INLINE int hak_spl_trylock (hak_spl_t* spl)
	{
		unsigned int rc;

		__asm__ volatile (
			"1:\n"
			"lwarx        %0,0,%1\n"  /* load and reserve. rc(%0) = *spl(%1) */
			"cmpwi        cr0,%0,0\n" /* cr0 = (rc compare-with 0) */
			"li           %0,0\n"     /* rc = 0(failure) */
			"bne          cr0,2f\n"   /* if cr0 != 0, goto 2; */
			"li           %0,1\n"     /* rc = 1(success) */
			"stwcx.       %0,0,%1\n"  /* *spl(%1) = 1(value in rc) if reserved */
			"bne          cr0,1b\n"   /* if reservation is lost, goto 1 */
			"lwsync\n"
			"2:\n"
			: "=&r"(rc)
			: "r"(spl)
			: "cr0", "memory"
		);

		return rc;
	}
	static HAK_INLINE void hak_spl_lock (hak_spl_t* spl)
	{
		while (!hak_spl_trylock(spl)) HAK_SPL_RELAX();
	}
	static HAK_INLINE void hak_spl_unlock (hak_spl_t* spl)
	{
		__asm__ volatile ("lwsync\n" : : : "memory");
		*spl = 0;
	}

#else
	/* no implementation fits. leave HAK_SUPPORT_SPL undefined so the caller
	 * can choose something else - hak's own targets that land here, __DOS__
	 * and EMSCRIPTEN, cannot run a second thread and need no lock at all. */
#	undef HAK_SUPPORT_SPL
#	if defined(HAK_SPL_UNSUPPORTED_ERROR)
#		error UNSUPPORTED
#	endif
#endif

#endif
