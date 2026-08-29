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

#if !defined(_GNU_SOURCE)
#	define _GNU_SOURCE
#endif

#include <hak-hnd.h>
#include "hak-prv.h"

#if defined(_WIN32)
#	include <windows.h>
#else
#	include <sys/types.h>
#	include <sys/stat.h>
#	include <unistd.h>
#	include <fcntl.h>
#	include <errno.h>
#endif

/* how much the id map and the descriptor reverse map grow by */
#define MAP_ALIGN 64

/* the id map is indexed by id, which is dense, so an int-sized ceiling is
 * plenty and keeps every id comfortably inside HAK_SMOOI_MAX */
#define MAP_CAPA_MAX HAK_TYPE_MAX(int)

struct hak_hndtab_t
{
	/* circular lists with real nodes as sentinels. a cast-a-2-pointer-struct
	 * sentinel would silently depend on prev/next sitting at the very front
	 * of hak_hnd_t; spending two nodes here removes that coupling. */
	hak_hnd_t used;
	hak_hnd_t free;

	/* id -> node */
	struct
	{
		hak_hnd_t** tab;
		hak_ooi_t   capa;
		hak_ooi_t   high; /* the next id to hand out */
	} map;

	/* descriptor -> id, so that a descriptor cannot be wrapped twice.
	 * two nodes over one descriptor would make hak_closehnd() on either of
	 * them drop the other's multiplexer registration. */
	struct
	{
		hak_ooi_t* tab;
		hak_ooi_t  capa;
	} fdmap;
};

/* ------------------------------------------------------------------------- */

static HAK_INLINE void chain_to_free (hak_hndtab_t* tab, hak_hnd_t* node)
{
	node->next = &tab->free;
	node->prev = tab->free.prev;
	node->prev->next = node;
	tab->free.prev = node;
}

static HAK_INLINE void chain_to_used (hak_hndtab_t* tab, hak_hnd_t* node)
{
	node->next = &tab->used;
	node->prev = tab->used.prev;
	node->prev->next = node;
	tab->used.prev = node;
}

static HAK_INLINE void unchain (hak_hnd_t* node)
{
	node->prev->next = node->next;
	node->next->prev = node->prev;
}

/* ------------------------------------------------------------------------- */

int hak_inithndtab (hak_t* hak)
{
	hak_hndtab_t* tab;

	tab = (hak_hndtab_t*)hak_callocmem(hak, HAK_SIZEOF(*tab));
	if (HAK_UNLIKELY(!tab)) return -1;

	tab->used.prev = tab->used.next = &tab->used;
	tab->free.prev = tab->free.next = &tab->free;

	hak->hndtab = tab;
	return 0;
}

void hak_finihndtab (hak_t* hak)
{
	hak_hndtab_t* tab = hak->hndtab;
	hak_hnd_t* node;

	if (!tab) return;

	/* close whatever hak code left open. an owner closes the handles it owns,
	 * so walk from the head each time rather than caching a next pointer. */
	while ((node = tab->used.next) != &tab->used)
	{
		hak_closehnd(hak, node);
	}

	while ((node = tab->free.next) != &tab->free)
	{
		unchain(node);
		hak_freemem(hak, node);
	}

	if (tab->map.tab) hak_freemem(hak, tab->map.tab);
	if (tab->fdmap.tab) hak_freemem(hak, tab->fdmap.tab);
	hak_freemem(hak, tab);
	hak->hndtab = HAK_NULL;
}

/* ------------------------------------------------------------------------- */

static hak_hnd_t* alloc_node (hak_t* hak)
{
	hak_hndtab_t* tab = hak->hndtab;
	hak_hnd_t* node;

	if (tab->free.next != &tab->free)
	{
		node = tab->free.next;
		unchain(node);
	}
	else
	{
		hak_ooi_t id;

		/* NOTE: the condition is >=, not <=. hawk's idmap-imp.h uses <=,
		 * which reallocates on every single node creation. */
		if (tab->map.high >= tab->map.capa)
		{
			hak_ooi_t newcapa, inc;
			hak_hnd_t** tmp;

			inc = MAP_CAPA_MAX - tab->map.capa;
			if (inc <= 0)
			{
				hak_seterrbfmt(hak, HAK_EFLOOD, "too many system handles");
				return HAK_NULL;
			}
			if (inc > MAP_ALIGN) inc = MAP_ALIGN;
			newcapa = tab->map.capa + inc;

			tmp = (hak_hnd_t**)hak_reallocmem(hak, tab->map.tab, HAK_SIZEOF(*tmp) * newcapa);
			if (HAK_UNLIKELY(!tmp)) return HAK_NULL;

			HAK_MEMSET(&tmp[tab->map.capa], 0, HAK_SIZEOF(*tmp) * (newcapa - tab->map.capa));
			tab->map.tab = tmp;
			tab->map.capa = newcapa;
		}

		id = tab->map.high;
		/* an id travels to hak code as a small integer */
		if (!HAK_IN_SMOOI_RANGE(id))
		{
			hak_seterrbfmt(hak, HAK_EFLOOD, "system handle id %zd out of the permitted range", id);
			return HAK_NULL;
		}

		node = (hak_hnd_t*)hak_callocmem(hak, HAK_SIZEOF(*node));
		if (HAK_UNLIKELY(!node)) return HAK_NULL;

		node->id = id;
		tab->map.high++;
	}

	HAK_ASSERT(hak, tab->map.tab[node->id] == HAK_NULL);
	tab->map.tab[node->id] = node;
	chain_to_used(tab, node);

	node->owner = -1;
	return node;
}

static void free_node (hak_t* hak, hak_hnd_t* node)
{
	hak_hndtab_t* tab = hak->hndtab;
	hak_ooi_t id = node->id;

	unchain(node);
	tab->map.tab[id] = HAK_NULL;

	node->type = 0;
	node->flags = 0;
	node->owner = -1;
	node->u.ptr = HAK_NULL;
	node->dtor = HAK_NULL;

	if (tab->map.high == id + 1)
	{
		/* the highest id. give the memory back and lower the watermark. */
		hak_freemem(hak, node);
		tab->map.high--;
	}
	else
	{
		node->id = id; /* keep the id for reuse */
		chain_to_free(tab, node);
	}
}

/* ------------------------------------------------------------------------- */

static int remember_fd (hak_t* hak, int fd, hak_ooi_t id)
{
	hak_hndtab_t* tab = hak->hndtab;

	if (fd < 0) return 0;

	if (fd >= tab->fdmap.capa)
	{
		hak_ooi_t newcapa, i;
		hak_ooi_t* tmp;

		newcapa = HAK_ALIGN_POW2((hak_ooi_t)fd + 1, MAP_ALIGN);
		tmp = (hak_ooi_t*)hak_reallocmem(hak, tab->fdmap.tab, HAK_SIZEOF(*tmp) * newcapa);
		if (HAK_UNLIKELY(!tmp)) return -1;

		for (i = tab->fdmap.capa; i < newcapa; i++) tmp[i] = -1;
		tab->fdmap.tab = tmp;
		tab->fdmap.capa = newcapa;
	}

	tab->fdmap.tab[fd] = id;
	return 0;
}

static HAK_INLINE void forget_fd (hak_t* hak, int fd)
{
	hak_hndtab_t* tab = hak->hndtab;
	if (fd >= 0 && fd < tab->fdmap.capa) tab->fdmap.tab[fd] = -1;
}

static HAK_INLINE hak_ooi_t fd_to_id (hak_t* hak, int fd)
{
	hak_hndtab_t* tab = hak->hndtab;
	if (fd < 0 || fd >= tab->fdmap.capa) return -1;
	return tab->fdmap.tab[fd];
}

/* ------------------------------------------------------------------------- */

/**
 * Work out what kind of descriptor \a fd is and whether the multiplexer will
 * take it. Doing this once here, at wrap time, is what keeps every caller
 * from discovering it later as an EPERM out of epoll_ctl - or, on a poll()
 * build, from never discovering it at all because poll() reports a regular
 * file as permanently ready.
 */
static int probe_fd (hak_t* hak, int fd, hak_hnd_type_t* type, int* muxable)
{
#if defined(_WIN32)
	/* TODO: GetFileType() on the underlying HANDLE. until then a caller must
	 *       state the type and nothing is muxable. */
	*type = 0;
	*muxable = 0;
	return 0;
#else
	struct stat st;

	if (fstat(fd, &st) <= -1)
	{
		hak_seterrbfmtwithsyserr(hak, 0, errno, "unable to identify handle %d", fd);
		return -1;
	}

	if (S_ISFIFO(st.st_mode))      { *type = HAK_HND_TYPE_PIPE; *muxable = 1; }
	else if (S_ISSOCK(st.st_mode)) { *type = HAK_HND_TYPE_SCK;  *muxable = 1; }
	else if (S_ISCHR(st.st_mode))  { *type = HAK_HND_TYPE_CHR;  *muxable = 1; }
	else if ((st.st_mode & S_IFMT) == 0)
	{
		/* an anonymous inode. linux hands these out for pidfd, eventfd,
		 * timerfd and signalfd: no file type bits at all, yet all of them are
		 * pollable. without this arm they would fall through to FILE below and
		 * the multiplexer would refuse them. */
		*type = HAK_HND_TYPE_EVT;  *muxable = 1;
	}
	else                           { *type = HAK_HND_TYPE_FILE; *muxable = 0; }

	return 0;
#endif
}

static int set_nonblock (hak_t* hak, int fd)
{
#if defined(_WIN32)
	hak_seterrnum(hak, HAK_ENOIMPL);
	return -1;
#elif defined(O_NONBLOCK)
	int fl;

	fl = fcntl(fd, F_GETFL, 0);
	if (fl <= -1 || fcntl(fd, F_SETFL, fl | O_NONBLOCK) <= -1)
	{
		hak_seterrbfmtwithsyserr(hak, 0, errno, "unable to set handle %d non-blocking", fd);
		return -1;
	}
	return 0;
#else
	hak_seterrnum(hak, HAK_ENOIMPL);
	return -1;
#endif
}

/* ------------------------------------------------------------------------- */

hak_hnd_t* hak_wrapfd (hak_t* hak, int fd, hak_hnd_type_t type_hint, int flags)
{
	hak_hnd_t* node;
	hak_hnd_type_t type;
	int muxable = 0;

	if (fd < 0)
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "invalid handle %d", fd);
		return HAK_NULL;
	}

	if (fd_to_id(hak, fd) >= 0)
	{
		/* refuse rather than hand out a second node. see fdmap above. */
		hak_seterrbfmt(hak, HAK_EEXIST, "handle %d already wrapped as %zd", fd, fd_to_id(hak, fd));
		return HAK_NULL;
	}

	if (probe_fd(hak, fd, &type, &muxable) <= -1) return HAK_NULL;

	if (type_hint && !(type_hint & type))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "handle %d not of the required kind", fd);
		return HAK_NULL;
	}

	if ((flags & HAK_HND_OPEN_NONBLOCK) && set_nonblock(hak, fd) <= -1) return HAK_NULL;

	node = alloc_node(hak);
	if (HAK_UNLIKELY(!node)) return HAK_NULL;

	node->type = type;
	node->flags = flags & (HAK_HND_FLAG_NONBLOCK | HAK_HND_FLAG_KEEPOPEN);
	/* the probe decides, unless the caller asserts it knows better */
	if (muxable || (flags & HAK_HND_FLAG_MUXABLE)) node->flags |= HAK_HND_FLAG_MUXABLE;
	node->u.fd = fd;

	if (remember_fd(hak, fd, node->id) <= -1)
	{
		free_node(hak, node);
		return HAK_NULL;
	}

	return node;
}

hak_hnd_t* hak_wrapptr (hak_t* hak, void* ptr, hak_hnd_type_t type, int flags, hak_hnd_dtor_t dtor)
{
	hak_hnd_t* node;

	if (!ptr || !(type & (HAK_HND_TYPE_DIR | HAK_HND_TYPE_PROC)))
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "invalid pointer-shaped handle");
		return HAK_NULL;
	}

	node = alloc_node(hak);
	if (HAK_UNLIKELY(!node)) return HAK_NULL;

	node->type = type;
	node->flags = flags & HAK_HND_FLAG_KEEPOPEN; /* never muxable */
	node->u.ptr = ptr;
	node->dtor = dtor;
	return node;
}

hak_hnd_t* hak_wrapfd_once (hak_t* hak, int fd, hak_hnd_type_t type_hint, int flags)
{
	hak_ooi_t id;

	if (fd < 0)
	{
		hak_seterrbfmt(hak, HAK_EINVAL, "invalid handle %d", fd);
		return HAK_NULL;
	}

	id = fd_to_id(hak, fd);
	if (id >= 0) return hak->hndtab->map.tab[id];

	return hak_wrapfd(hak, fd, type_hint, flags);
}

void hak_ownhnd (hak_t* hak, hak_hnd_t* hnd, hak_hnd_t* owner)
{
	HAK_ASSERT(hak, hnd != owner);
	hnd->owner = owner? owner->id: -1;
}

/* ------------------------------------------------------------------------- */

hak_hnd_t* hak_gethnd (hak_t* hak, hak_ooi_t id, int acceptable_types)
{
	hak_hndtab_t* tab = hak->hndtab;
	hak_hnd_t* node;

	if (id < 0 || id >= tab->map.high || !(node = tab->map.tab[id]))
	{
		hak_seterrbfmt(hak, HAK_EBADHND, "invalid system handle %zd", id);
		return HAK_NULL;
	}

	if (!(node->type & acceptable_types))
	{
		hak_seterrbfmt(hak, HAK_EBADHND, "system handle %zd not of an acceptable kind", id);
		return HAK_NULL;
	}

	return node;
}

hak_hnd_t* hak_gethndwithoop (hak_t* hak, hak_oop_t id, int acceptable_types)
{
	if (!HAK_OOP_IS_SMOOI(id))
	{
		hak_seterrbfmt(hak, HAK_EBADHND, "system handle not a small integer - %O", id);
		return HAK_NULL;
	}
	return hak_gethnd(hak, HAK_OOP_TO_SMOOI(id), acceptable_types);
}

/* ------------------------------------------------------------------------- */

int hak_closehnd (hak_t* hak, hak_hnd_t* hnd)
{
	hak_hndtab_t* tab = hak->hndtab;
	hak_hnd_t* p;
	hak_hnd_t* next;
	hak_ooi_t id = hnd->id;
	int n = 0;

	/* 1. handles this one owns go first. a child process must not outlive the
	 *    node that represents it. */
	for (p = tab->used.next; p != &tab->used; p = next)
	{
		next = p->next;
		if (p != hnd && p->owner == id) hak_closehnd(hak, p);
	}

	/* 2. drop any multiplexer registration while the handle is still valid.
	 *    doing this after the close would leave the VM watching a descriptor
	 *    number that may already have been recycled. */
	if ((hnd->flags & HAK_HND_FLAG_IN_MUX) && (hnd->type & HAK_HND_TYPE_ALL_FD))
	{
		hak_releaseiohandle(hak, hnd->u.fd);
		hnd->flags &= ~HAK_HND_FLAG_IN_MUX;
	}

	/* 3. release the resource itself */
	if (!(hnd->flags & HAK_HND_FLAG_KEEPOPEN))
	{
		if (hnd->dtor) /* destructor available */
		{
			/* the subsystem that created the resource knows how to dispose of
			 * it. this is also the path hak_finihndtab() takes, which is why a
			 * pointer-shaped node needs a destructor to avoid leaking both the
			 * resource and, for a child process, the process itself. */
			hnd->dtor(hak, hnd);
		}
		else if (hnd->type & HAK_HND_TYPE_ALL_FD)
		{
			/* all file-descriptor based handles */
		#if defined(_WIN32)
			if (!CloseHandle((HANDLE)(hak_uintptr_t)hnd->u.fd)) n = -1;
		#else
			if (close(hnd->u.fd) <= -1)
			{
				hak_seterrbfmtwithsyserr(hak, 0, errno, "unable to close handle %d", hnd->u.fd);
				n = -1;
			}
		#endif
		}
		/* a pointer-shaped node with no destructor releases nothing, which is
		 * only correct when the pointer is owned elsewhere. */
	}

	if (hnd->type & HAK_HND_TYPE_ALL_FD)
	{
		/* all file-descriptor based handles */
		forget_fd(hak, hnd->u.fd);
	}

	free_node(hak, hnd);
	return n;
}

/* ------------------------------------------------------------------------- */

int hak_bindhnd (hak_t* hak, hak_hnd_t* hnd, hak_oop_semaphore_t sem, hak_semaphore_io_type_t io_type)
{
	if (!(hnd->flags & HAK_HND_FLAG_MUXABLE))
	{
		/* a regular file is the common case here. it is never reported as
		 * not-ready, so waiting on one is meaningless as well as unsupported. */
		hak_seterrbfmt(hak, HAK_EINVAL, "system handle %zd cannot be multiplexed", hnd->id);
		return -1;
	}

	if (hak_add_sem_to_sem_io_tuple(hak, sem, hnd->u.fd, io_type) <= -1) return -1;

	hnd->flags |= HAK_HND_FLAG_IN_MUX;
	return 0;
}

/* ------------------------------------------------------------------------- */

hak_ooi_t hak_readhnd (hak_t* hak, hak_hnd_t* hnd, void* buf, hak_oow_t len)
{
#if !defined(_WIN32)
	hak_ooi_t n;
#endif

	if (!(hnd->type & HAK_HND_TYPE_ALL_STREAM))
	{
		hak_seterrbfmt(hak, HAK_EBADHND, "system handle %zd not readable", hnd->id);
		return HAK_HND_IO_ERROR;
	}

	if (len > (hak_oow_t)HAK_TYPE_MAX(hak_ooi_t)) len = (hak_oow_t)HAK_TYPE_MAX(hak_ooi_t);

#if defined(_WIN32)
	{
		DWORD count;
		if (len > (hak_oow_t)HAK_TYPE_MAX(DWORD)) len = (hak_oow_t)HAK_TYPE_MAX(DWORD);
		if (!ReadFile((HANDLE)(hak_uintptr_t)hnd->u.fd, buf, (DWORD)len, &count, HAK_NULL))
		{
			DWORD e = GetLastError();
			if (e == ERROR_BROKEN_PIPE) return 0; /* end of file */
			hak_seterrwithsyserr(hak, 1, e);
			return HAK_HND_IO_ERROR;
		}
		return (hak_ooi_t)count;
	}
#else
	n = read(hnd->u.fd, buf, len);
	if (n <= -1)
	{
		if (errno == EINTR) return HAK_HND_IO_WOULDBLOCK; /* let hak code retry */
	#if defined(EWOULDBLOCK) && defined(EAGAIN) && (EWOULDBLOCK != EAGAIN)
		if (errno == EAGAIN || errno == EWOULDBLOCK) return HAK_HND_IO_WOULDBLOCK;
	#elif defined(EAGAIN)
		if (errno == EAGAIN) return HAK_HND_IO_WOULDBLOCK;
	#elif defined(EWOULDBLOCK)
		if (errno == EWOULDBLOCK) return HAK_HND_IO_WOULDBLOCK;
	#endif
		hak_seterrwithsyserr(hak, 0, errno);
		return HAK_HND_IO_ERROR;
	}
	return n;
#endif
}

hak_ooi_t hak_writehnd (hak_t* hak, hak_hnd_t* hnd, const void* buf, hak_oow_t len)
{
#if !defined(_WIN32)
	hak_ooi_t n;
#endif

	if (!(hnd->type & HAK_HND_TYPE_ALL_STREAM))
	{
		hak_seterrbfmt(hak, HAK_EBADHND, "system handle %zd not writable", hnd->id);
		return HAK_HND_IO_ERROR;
	}

	if (len > (hak_oow_t)HAK_TYPE_MAX(hak_ooi_t)) len = (hak_oow_t)HAK_TYPE_MAX(hak_ooi_t);

#if defined(_WIN32)
	{
		DWORD count;
		if (len > (hak_oow_t)HAK_TYPE_MAX(DWORD)) len = (hak_oow_t)HAK_TYPE_MAX(DWORD);
		if (!WriteFile((HANDLE)(hak_uintptr_t)hnd->u.fd, buf, (DWORD)len, &count, HAK_NULL))
		{
			hak_seterrwithsyserr(hak, 1, GetLastError());
			return HAK_HND_IO_ERROR;
		}
		return (hak_ooi_t)count;
	}
#else
	n = write(hnd->u.fd, buf, len);
	if (n <= -1)
	{
		if (errno == EINTR) return HAK_HND_IO_WOULDBLOCK;
	#if defined(EWOULDBLOCK) && defined(EAGAIN) && (EWOULDBLOCK != EAGAIN)
		if (errno == EAGAIN || errno == EWOULDBLOCK) return HAK_HND_IO_WOULDBLOCK;
	#elif defined(EAGAIN)
		if (errno == EAGAIN) return HAK_HND_IO_WOULDBLOCK;
	#elif defined(EWOULDBLOCK)
		if (errno == EWOULDBLOCK) return HAK_HND_IO_WOULDBLOCK;
	#endif
		hak_seterrwithsyserr(hak, 0, errno);
		return HAK_HND_IO_ERROR;
	}
	return n;
#endif
}
