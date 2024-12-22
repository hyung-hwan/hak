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
# BUNDLED LIBRARIES
##################################################

noinst_LIBRARIES = libisocline.a
noinst_HEADERS = \
	isocline/include/isocline.h \
	isocline/src/attr.h \
	isocline/src/bbcode.h \
	isocline/src/common.h \
	isocline/src/completions.h \
	isocline/src/env.h \
	isocline/src/highlight.h \
	isocline/src/history.h \
	isocline/src/stringbuf.h \
	isocline/src/term.h \
	isocline/src/tty.h \
	isocline/src/undo.h \
	isocline/src/editline_completion.c \
	isocline/src/editline_help.c \
	isocline/src/editline_history.c \
	isocline/src/term_color.c
libisocline_a_SOURCES = \
	isocline/src/attr.c \
	isocline/src/bbcode.c \
	isocline/src/bbcode_colors.c \
	isocline/src/common.c \
	isocline/src/completers.c \
	isocline/src/completions.c \
	isocline/src/editline.c \
	isocline/src/highlight.c \
	isocline/src/history.c \
	isocline/src/isocline.c \
	isocline/src/stringbuf.c \
	isocline/src/term.c \
	isocline/src/tty.c \
	isocline/src/tty_esc.c \
	isocline/src/undo.c \
	isocline/src/wcwidth.c

##################################################
# MAIN BINARIES
##################################################

if ENABLE_LIBLTDL
LIBADD_COMMON += $(LTDL_LIBS)
else
LIBADD_COMMON += $(DL_LIBS)
endif

bin_PROGRAMS = hcl
hcl_SOURCES = hcl.c
hcl_CPPFLAGS = $(CPPFLAGS_COMMON) -I$(srcdir)/isocline/include -DHAVE_ISOCLINE_H -DHAVE_ISOCLINE_LIB
hcl_LDFLAGS = $(LDFLAGS_COMMON)
hcl_LDADD = ../lib/libhcl.la $(LIBADD_COMMON) ./libisocline.a

if ENABLE_HCLX

bin_PROGRAMS += hclx
hclx_SOURCES = hclx.c
hclx_CPPFLAGS = $(CPPFLAGS_COMMON)
hclx_LDFLAGS = $(LDFLAGS_COMMON)
hclx_LDADD = ../lib/libhclx.la $(LIBADD_COMMON)

endif
