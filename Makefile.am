AUTOMAKE_OPTION = foreign
ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = t t/test-bi.hcl

SUBDIRS =

if ENABLE_STATIC_MODULE
SUBDIRS += mod lib bin t
else
SUBDIRS += lib mod bin t
endif

if ENABLE_HCLGO
SUBDIRS += go
endif

DIST_SUBDIRS = $(SUBDIRS)

distclean-local:
	@rm -rf $(top_srcdir)/autom4te.cache
