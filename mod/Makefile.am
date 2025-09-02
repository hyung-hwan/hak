AUTOMAKE_OPTIONS = nostdinc


CPPFLAGS_COMMON = \
	-I$(abs_builddir) \
	-I$(abs_builddir)/../lib \
	-I$(abs_srcdir) \
	-I$(abs_srcdir)/../lib \
	-I$(includedir)

CFLAGS_COMMON = 

if ENABLE_STATIC_MODULE

##################################################
# STATIC MODULES BUILT INTO MAIN LIBRARY
##################################################
LDFLAGS_COMMON =-L$(libdir)
LIBADD_COMMON =

noinst_LTLIBRARIES =

noinst_LTLIBRARIES += libhak-core.la
noinst_LTLIBRARIES += libhak-dic.la
noinst_LTLIBRARIES += libhak-sys.la

else

##################################################
# DYNAMIC MODULES 
##################################################
LDFLAGS_COMMON = -L$(abs_builddir)/../lib -L$(libdir) -version-info 1:0:0 -no-undefined
LIBADD_COMMON = -lhak

pkgmodexecdir = $(libdir)
pkgmodexec_LTLIBRARIES =

pkgmodexec_LTLIBRARIES += libhak-core.la
pkgmodexec_LTLIBRARIES += libhak-dic.la 
pkgmodexec_LTLIBRARIES += libhak-sys.la 

endif

libhak_core_la_SOURCES = core.c _core.h
libhak_core_la_CPPFLAGS = $(CPPFLAGS_COMMON)
libhak_core_la_CFLAGS = $(CFLAGS_COMMON)
libhak_core_la_LDFLAGS = $(LDFLAGS_COMMON)
libhak_core_la_LIBADD = $(LIBADD_COMMON)

libhak_dic_la_SOURCES = dic.c _dic.h
libhak_dic_la_CPPFLAGS = $(CPPFLAGS_COMMON)
libhak_dic_la_CFLAGS = $(CFLAGS_COMMON)
libhak_dic_la_LDFLAGS = $(LDFLAGS_COMMON)
libhak_dic_la_LIBADD = $(LIBADD_COMMON)

libhak_sys_la_SOURCES = sys.c _sys.h
libhak_sys_la_CPPFLAGS = $(CPPFLAGS_COMMON)
libhak_sys_la_CFLAGS = $(CFLAGS_COMMON)
libhak_sys_la_LDFLAGS = $(LDFLAGS_COMMON)
libhak_sys_la_LIBADD = $(LIBADD_COMMON)
