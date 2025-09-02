AUTOMAKE_OPTIONS = nostdinc

CPPFLAGS_ALL_COMMON = \
	-I$(abs_builddir) \
	-I$(abs_srcdir) \
	-I$(includedir)	

if ENABLE_STATIC_MODULE
LDFLAGS_ALL_COMMON = -L$(abs_builddir)/../mod -L$(abs_builddir) -L$(libdir)
else
LDFLAGS_ALL_COMMON = -L$(abs_builddir) -L$(libdir)
endif


##################################################
# MAIN LIBRARY 
##################################################

CPPFLAGS_LIB_COMMON = $(CPPFLAGS_ALL_COMMON)
LDFLAGS_LIB_COMMON = $(LDFLAGS_ALL_COMMON) -version-info 1:0:0 -no-undefined
LIBADD_LIB_COMMON = $(LIBM)

if ENABLE_LIBLTDL
LIBADD_LIB_COMMON += $(LTDL_LIBS)
else
LIBADD_LIB_COMMON += $(DL_LIBS)
endif

if ENABLE_LIBUNWIND
LIBADD_LIB_COMMON += $(UNWIND_LIBS)
endif

if WIN32
# you must adjust the value of DEFAULT_MODPOSTFIX according 
# to the first number in -version-info above
CPPFLAGS_PFMOD = -DHAK_DEFAULT_PFMODPREFIX=\"libhak-\" -DHAK_DEFAULT_PFMODPOSTFIX=\"-1.dll\"
else
CPPFLAGS_PFMOD = -DHAK_DEFAULT_PFMODPREFIX=\"$(libdir)/libhak-\" 
if MACOSX
CPPFLAGS_PFMOD += -DHAK_DEFAULT_PFMODPOSTFIX=\".dylib\"
else
CPPFLAGS_PFMOD += -DHAK_DEFAULT_PFMODPOSTFIX=\".so\"
endif
endif

pkgincludedir = $(includedir)
pkglibdir = $(libdir)

pkginclude_HEADERS = \
	hak.h \
	hak-chr.h \
	hak-cmgr.h \
	hak-cmn.h \
	hak-fmt.h \
	hak-opt.h \
	hak-pac1.h \
	hak-rbt.h \
	hak-str.h \
	hak-upac.h \
	hak-utl.h \
	hak-xma.h

pkglib_LTLIBRARIES = libhak.la
libhak_la_SOURCES = \
	hak-prv.h \
	bigint.c \
	chr.c \
	cmgr.c \
	cnode.c \
	comp.c \
	debug.c \
	decode.c \
	dic.c \
	err.c \
	exec.c \
	fmt-imp.h \
	fmt.c \
	gc.c \
	hak.c \
	heap.c \
	mb8.c \
	number.c \
	obj.c \
	opt-impl.h \
	opt.c \
	prim.c \
	print.c \
	rbt.c \
	read.c \
	std.c \
	str.c \
	sym.c \
	uch-case.h \
	uch-prop.h \
	utf16.c \
	utf8.c \
	utl.c \
	xchg.c \
	xma.c 
libhak_la_CPPFLAGS = $(CPPFLAGS_LIB_COMMON)
libhak_la_LDFLAGS = $(LDFLAGS_LIB_COMMON)
libhak_la_LIBADD = $(LIBADD_LIB_COMMON)

if ENABLE_STATIC_MODULE
libhak_la_LIBADD += ../mod/libhak-core.la
libhak_la_LIBADD += ../mod/libhak-dic.la
libhak_la_LIBADD += ../mod/libhak-sys.la
endif

if ENABLE_HAKX
pkglib_LTLIBRARIES += libhakx.la
pkginclude_HEADERS += hak-x.h hak-tmr.h hak-json.h
libhakx_la_SOURCES = \
	hak-json.h \
	hak-tmr.h \
	hak-x.h \
	json.c \
	tmr.c \
	x-client.c \
	x-proto.c \
	x-server.c \
	x-sys.c \
	x-utl.c \
	x-utl-sa.h
libhakx_la_CPPFLAGS = $(CPPFLAGS_LIB_COMMON) $(CPPFLAGS_PFMOD)
libhakx_la_LDFLAGS = $(LDFLAGS_LIB_COMMON)
libhakx_la_LIBADD = libhak.la $(LIBADD_LIB_COMMON)
endif


install-data-hook:
	@echo "#ifndef _HAK_CFG_H_" > "$(DESTDIR)$(pkgincludedir)/hak-cfg.h"
	@echo "#define _HAK_CFG_H_" >> "$(DESTDIR)$(pkgincludedir)/hak-cfg.h"
	@$(EGREP) "#define[ ]+HAK_" "$(abs_builddir)/hak-cfg.h" >> "$(DESTDIR)$(pkgincludedir)/hak-cfg.h" 
	@echo "#endif" >> "$(DESTDIR)$(pkgincludedir)/hak-cfg.h"
	@rm -f "$(DESTDIR)$(pkgincludedir)/hak-cfg.h.in"
	@$(SED) 's|/\*#define HAK_HAVE_CFG_H\*/|#define HAK_HAVE_CFG_H|' "$(srcdir)/hak-cmn.h" > "$(DESTDIR)$(pkgincludedir)/hak-cmn.h"

uninstall-hook:
	@rm -f "$(DESTDIR)$(pkgincludedir)/hak-cfg.h"

