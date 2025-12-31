AUTOMAKE_OPTION = foreign
ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = go.mod t t/test-bi.hak

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

