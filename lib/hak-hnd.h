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

#ifndef _HAK_HND_H_
#define _HAK_HND_H_

#include <hak.h>

/** \file
 * This file provides the system handle table - the single place where hak
 * keeps track of operating system resources handed out to hak code.
 *
 * hak code never sees a raw file descriptor or a pointer. It sees a small
 * non-negative integer id which is resolved against this table on every use.
 * That resolution is what makes the following impossible:
 *
 * - naming a descriptor the script never opened (including hak's own
 *   multiplexer, signal and io-thread descriptors, and any descriptor the
 *   host application that embeds hak happens to hold open),
 * - aiming hak_releaseiohandle() at a descriptor hak does not own, which
 *   would delete the VM's own multiplexer registration for it,
 * - keeping a multiplexer registration alive across a close(), where the
 *   descriptor number may be recycled for something unrelated.
 *
 * There is exactly one table per #hak_t, shared by every module, so ids are
 * unique across subsystems and the close protocol lives in one place.
 *
 * A node is one waitable operating system handle. A resource made of several
 * handles (a child process with its pipes, say) is represented as several
 * nodes tied together by \a owner, so that "which handle am I waiting on"
 * never becomes a parameter of read, write or bind.
 */

/**
 * The hak_hnd_type_t type enumerates the kinds of resource a node may hold.
 * The values are bit flags so that hak_gethnd() can be given the set of
 * types a caller is prepared to accept.
 */
enum hak_hnd_type_t
{
	HAK_HND_TYPE_FILE = (1 << 0), /**< regular file, directory or block device - not muxable */
	HAK_HND_TYPE_PIPE = (1 << 1), /**< pipe or fifo end */
	HAK_HND_TYPE_SCK  = (1 << 2), /**< socket */
	HAK_HND_TYPE_CHR  = (1 << 3), /**< terminal or character device */
	HAK_HND_TYPE_DIR  = (1 << 4), /**< directory stream - a pointer, not a descriptor */
	HAK_HND_TYPE_PROC = (1 << 5)  /**< child process - a pointer, not a descriptor */
};
typedef enum hak_hnd_type_t hak_hnd_type_t;

/** every type that is backed by a file descriptor */
#define HAK_HND_TYPE_ALL_FD \
	(HAK_HND_TYPE_FILE | HAK_HND_TYPE_PIPE | HAK_HND_TYPE_SCK | HAK_HND_TYPE_CHR)

/** every type the multiplexer can accept */
#define HAK_HND_TYPE_ALL_MUXABLE \
	(HAK_HND_TYPE_PIPE | HAK_HND_TYPE_SCK | HAK_HND_TYPE_CHR)

enum hak_hnd_flag_t
{
	/** the multiplexer accepts this handle. set by hak_wrapfd() from the
	 *  probed type; a regular file never gets it because epoll refuses one
	 *  outright and poll() would report it permanently ready. */
	HAK_HND_FLAG_MUXABLE  = (1 << 0),

	/** O_NONBLOCK is set on the handle */
	HAK_HND_FLAG_NONBLOCK = (1 << 1),

	/** a semaphore is currently bound to this handle. hak_closehnd() uses
	 *  this to know it must call hak_releaseiohandle() first. */
	HAK_HND_FLAG_IN_MUX   = (1 << 2),

	/** release the node without closing the underlying handle. useful for a
	 *  descriptor owned by someone else that was only wrapped for the ride. */
	HAK_HND_FLAG_KEEPOPEN = (1 << 3)
};
typedef enum hak_hnd_flag_t hak_hnd_flag_t;

/** hak_wrapfd() should put the descriptor into non-blocking mode */
#define HAK_HND_OPEN_NONBLOCK HAK_HND_FLAG_NONBLOCK
/** hak_closehnd() should not close the underlying handle */
#define HAK_HND_OPEN_KEEPOPEN HAK_HND_FLAG_KEEPOPEN

typedef struct hak_hnd_t hak_hnd_t;

/**
 * The hak_hnd_dtor_t type defines how a node's underlying resource is
 * released. It is what lets a pointer-shaped resource - a directory stream, a
 * child process - be disposed of correctly even when hak code never closed it
 * and hak_finihndtab() is doing the closing at teardown.
 *
 * The node itself is still returned to the free list afterwards; a destructor
 * only has to deal with what \a hnd points at.
 */
typedef void (*hak_hnd_dtor_t) (
	hak_t*     hak,
	hak_hnd_t* hnd
);

struct hak_hnd_t
{
	/* the id is what hak code holds. it is always >= 0 and always within
	 * HAK_SMOOI_MAX so that it can be handed over as a small integer. */
	hak_ooi_t      id;
	hak_hnd_type_t type;
	int            flags;

	/* id of the node that owns this one, or -1. closing an owner closes
	 * everything it owns. */
	hak_ooi_t      owner;

	union
	{
		int   fd;  /**< HAK_HND_TYPE_FILE, _PIPE, _SCK, _CHR */
		void* ptr; /**< HAK_HND_TYPE_DIR, _PROC */
	} u;

	/* how to release u.ptr, or a descriptor needing more than close().
	 * HAK_NULL means the default: close() for a descriptor, nothing for a
	 * pointer - which is why a pointer-shaped node without one leaks. */
	hak_hnd_dtor_t dtor;

	/* house keeping. do not touch from outside hnd.c */
	hak_hnd_t* prev;
	hak_hnd_t* next;
};

typedef struct hak_hndtab_t hak_hndtab_t;

/* ========================================================================= */
/* THE UNIFORM I/O CONTRACT                                                  */
/* ========================================================================= */

/**
 * hak_readhnd() and hak_writehnd() never block and never raise. They return
 * the number of bytes transferred, 0 at end of file, or one of the two values
 * below. #HAK_HND_IO_WOULDBLOCK is an ordinary outcome that the caller is
 * expected to hand back to hak code so that it can wait on a semaphore and
 * retry; only #HAK_HND_IO_ERROR means the hak error has been set and the
 * primitive should fail.
 */
#define HAK_HND_IO_WOULDBLOCK ((hak_ooi_t)-1)
#define HAK_HND_IO_ERROR      ((hak_ooi_t)-2)

#if defined(__cplusplus)
extern "C" {
#endif

/* ========================================================================= */
/* TABLE LIFECYCLE - called by hak_init()/hak_fini(), not by modules         */
/* ========================================================================= */

int  hak_inithndtab (hak_t* hak);

/**
 * Closes every handle still open and frees the table. Handles that outlive
 * the hak instance would otherwise leak descriptors and child processes.
 */
void hak_finihndtab (hak_t* hak);

/* ========================================================================= */
/* CREATION                                                                  */
/* ========================================================================= */

/**
 * Wraps the descriptor \a fd into a new node and returns it.
 *
 * The kind of descriptor is probed with fstat() and recorded in \a type,
 * together with #HAK_HND_FLAG_MUXABLE when the multiplexer can accept it.
 * Pass 0 for \a type_hint to take whatever the probe finds, or a mask of
 * acceptable #hak_hnd_type_t values to require one of them.
 *
 * \a flags may carry #HAK_HND_OPEN_NONBLOCK and #HAK_HND_OPEN_KEEPOPEN.
 *
 * On success the table owns \a fd. On failure \a fd is left alone, so the
 * caller remains responsible for closing it.
 *
 * \return node pointer on success, #HAK_NULL on failure
 */
hak_hnd_t* hak_wrapfd (
	hak_t*         hak,
	int            fd,
	hak_hnd_type_t type_hint,
	int            flags
);

/**
 * Wraps a pointer-shaped resource, such as a directory stream or a child
 * process object. Such a node is never muxable.
 *
 * \a dtor is how \a ptr gets released, and is called by hak_closehnd() -
 * including the closes that hak_finihndtab() performs at teardown. Passing
 * #HAK_NULL means the pointer is owned elsewhere and this node must not
 * release it.
 */
hak_hnd_t* hak_wrapptr (
	hak_t*         hak,
	void*          ptr,
	hak_hnd_type_t type,
	int            flags,
	hak_hnd_dtor_t dtor
);

/**
 * Like hak_wrapfd() but idempotent: if \a fd already has a node, that node is
 * returned instead of failing the way hak_wrapfd() does. Use it for a
 * descriptor the VM owns and hands out repeatedly - the signal descriptor, for
 * instance - so that hak code always sees the same id for it.
 *
 * Pass #HAK_HND_OPEN_KEEPOPEN for anything the handle table must not close.
 */
hak_hnd_t* hak_wrapfd_once (
	hak_t*         hak,
	int            fd,
	hak_hnd_type_t type_hint,
	int            flags
);

/**
 * Makes \a hnd owned by \a owner, so that closing \a owner closes \a hnd too.
 */
void hak_ownhnd (
	hak_t*     hak,
	hak_hnd_t* hnd,
	hak_hnd_t* owner
);

/* ========================================================================= */
/* LOOKUP - the validation gate                                              */
/* ========================================================================= */

/**
 * Resolves \a id to a node, requiring its type to be among
 * \a acceptable_types. Sets the hak error and returns #HAK_NULL when the id
 * is out of range, refers to no live node, or refers to a node of the wrong
 * kind - so a caller can simply return HAK_PF_FAILURE.
 */
hak_hnd_t* hak_gethnd (
	hak_t*     hak,
	hak_ooi_t  id,
	int        acceptable_types
);

/**
 * Like hak_gethnd() but takes the id as an object, which is what a primitive
 * has at hand. Rejects anything that is not a small integer.
 */
hak_hnd_t* hak_gethndwithoop (
	hak_t*     hak,
	hak_oop_t  id,
	int        acceptable_types
);

/* ========================================================================= */
/* DESTRUCTION                                                               */
/* ========================================================================= */

/**
 * Closes \a hnd. Handles owned by it are closed first, any multiplexer
 * registration is dropped through hak_releaseiohandle() before the underlying
 * handle goes away, the resource is released through the node's
 * #hak_hnd_dtor_t (or close() when it has none and is a descriptor), and the
 * node is returned to the free list.
 * \return 0 on success, -1 if the underlying close failed (the node is
 *         released either way)
 */
int hak_closehnd (
	hak_t*     hak,
	hak_hnd_t* hnd
);

/* ========================================================================= */
/* MULTIPLEXER BINDING - the only path into the io semaphore tuples          */
/* ========================================================================= */

/**
 * Binds \a sem to \a hnd so that the semaphore is signalled when the handle
 * becomes ready for \a io_type. Fails if the handle is not muxable.
 *
 * Unbinding is done with hak_pf_semaphore_unsignal() from hak code, which
 * works from the semaphore rather than from the handle and so needs no
 * handle-side counterpart.
 *
 * \return 0 on success, -1 on failure
 */
int hak_bindhnd (
	hak_t*                  hak,
	hak_hnd_t*              hnd,
	hak_oop_semaphore_t     sem,
	hak_semaphore_io_type_t io_type
);

/* ========================================================================= */
/* I/O                                                                       */
/* ========================================================================= */

hak_ooi_t hak_readhnd (
	hak_t*     hak,
	hak_hnd_t* hnd,
	void*      buf,
	hak_oow_t  len
);

hak_ooi_t hak_writehnd (
	hak_t*      hak,
	hak_hnd_t*  hnd,
	const void* buf,
	hak_oow_t   len
);

#if defined(__cplusplus)
}
#endif

#endif
