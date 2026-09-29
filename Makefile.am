AUTOMAKE_OPTION = foreign
ACLOCAL_AMFLAGS = -I m4

haksrcdir = $(datadir)/hak
haksrc_DATA = \
	src/collection.hak \
	src/external-process.hak  \
	src/kernel.hak  \
	src/mutex.hak \
	src/process.hak \
	src/stream.hak  \
	src/text-stream.hak \
	src/context.hak \
	src/magnitude.hak \
	src/object.hak \
	src/semaphore.hak \
	src/system.hak

gosrcdir = $(datadir)/hak/go
gosrc_DATA = go.mod hak.go hak-inst.go hak-cb.go

EXTRA_DIST = t t/test-bi.hak src

SUBDIRS =

if ENABLE_STATIC_MODULE
SUBDIRS += mod lib bin t
else
SUBDIRS += lib mod bin t
endif

DIST_SUBDIRS = $(SUBDIRS) pas

distclean-local:
	@rm -rf $(top_srcdir)/autom4te.cache

clean-local:
	rm -rf $(builddir)/go.mod $(builddir)/go.sum
	go clean -C $(srcdir) -x -modfile $(abs_builddir)/go.mod -cache

