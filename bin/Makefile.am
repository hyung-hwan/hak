AUTOMAKE_OPTIONS = nostdinc

CPPFLAGS_COMMON = \
	-I$(abs_builddir) \
	-I$(abs_builddir)/../lib  \
	-I$(abs_srcdir) \
	-I$(abs_srcdir)/../lib \
	-I$(includedir)	

#### $(LTDLINCL)

CFLAGS_COMMON =
LDFLAGS_COMMON = -L$(abs_builddir)/../lib -L$(libdir)
LIBADD_COMMON =

##################################################
# MAIN LIBRARY 
##################################################

if ENABLE_LIBLTDL
LIBADD_COMMON += $(LTDL_LIBS)
else
LIBADD_COMMON += $(DL_LIBS)
endif

bin_PROGRAMS = hcl
hcl_SOURCES = hcl.c
hcl_CPPFLAGS = $(CPPFLAGS_COMMON) 
hcl_LDFLAGS = $(LDFLAGS_COMMON)
hcl_LDADD = ../lib/libhcl.la $(LIBADD_COMMON) $(ISOCLINE_LIBS)

if ENABLE_HCLX

bin_PROGRAMS += hclx
hclx_SOURCES = hclx.c
hclx_CPPFLAGS = $(CPPFLAGS_COMMON)
hclx_LDFLAGS = $(LDFLAGS_COMMON)
hclx_LDADD = ../lib/libhclx.la $(LIBADD_COMMON)

endif
