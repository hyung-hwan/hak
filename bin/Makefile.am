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

if ENABLE_ISOCLINE
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
endif

##################################################
# MAIN BINARIES
##################################################

if ENABLE_LIBLTDL
LIBADD_COMMON += $(LTDL_LIBS)
else
LIBADD_COMMON += $(DL_LIBS)
endif

bin_PROGRAMS = hak
hak_SOURCES = hak.c
hak_CPPFLAGS = $(CPPFLAGS_COMMON)
hak_LDFLAGS = $(LDFLAGS_COMMON)
hak_LDADD = ../lib/libhak.la $(LIBADD_COMMON)
hak_DEPENDENCIES = ../lib/libhak.la

if ENABLE_ISOCLINE
hak_CPPFLAGS += -I$(srcdir)/isocline/include -DHAVE_ISOCLINE_H -DHAVE_ISOCLINE_LIB
hak_LDADD += ./libisocline.a
##hak_DEPENDENCIES += ./libisocline.a
hak_DEPENDENCIES += libisocline.a
endif

if ENABLE_HAKX

bin_PROGRAMS += hakx
hakx_SOURCES = hakx.c
hakx_CPPFLAGS = $(CPPFLAGS_COMMON)
hakx_LDFLAGS = $(LDFLAGS_COMMON)
hakx_LDADD = ../lib/libhakx.la $(LIBADD_COMMON)
hakx_DEPENDENCIES = ../lib/libhakx.la

endif

if ENABLE_HAKGO

## the attempt to compose a proper procedure using a regular compiler failed.
## e.g ./configure GOC=opt/go/pkg/tool/linux_amd64/
## while go.m4 is included in autoconf 2.71, the support for the go language
## is very limited and the go language itself is pursuing the module based builder.
##
## the following is to trick autoconf/automake as if it's building go files with
## a real compiler whereas the actual building is done thru `go build`

bin_PROGRAMS += hakgo
hakgo_SOURCES = \
	../go.mod \
	../hak.go \
	../hak-cb.go \
	../hak-inst.go \
	main.go
##hakgo_DEPENDENCIES = 

## let the linker to move hakgo.bin to the actual target
##hakgo_LINK = mv -f hakgo.bin hakgo$(EXEEXT) || echo "FAILED TO LINK"
hakgo_LINK =

if ENABLE_STATIC
CGO_CFLAGS_EXTRA="-static"
CGO_LDFLAGS_EXTRA="-static"
else
CGO_CFLAGS_EXTRA=""
CGO_LDFLAGS_EXTRA=""
endif

if WIN32
# go does not infer the target from CC, so a cross build needs GOOS/GOARCH set
# explicitly - otherwise it runs the host toolchain against the mingw compiler.
# GOARCH is mapped from $(host_cpu) at recipe time. windows has no libdl, and
# libhak pulls in winsock, so the system libraries differ from the posix case.
CGO_TARGET_ENV = GOOS=windows CGO_ENABLED=1 GOARCH=`case "$(host_cpu)" in i?86) echo 386;; x86_64|amd64) echo amd64;; aarch64|arm64) echo arm64;; *) echo "$(host_cpu)";; esac`
CGO_SYS_LIBS = -lws2_32
else
CGO_TARGET_ENV =
CGO_SYS_LIBS = -ldl
endif

hakgo$(EXEEXT): ../lib/libhak.la $(hakgo_OBJECTS)
	##cp -pf $(top_srcdir)/go.mod $(builddir)/go.mod >/dev/null 2>&1 || true
	##chmod u+w $(builddir)/go.mod ## with `make distcheck`, the echo's redirection to the file fails without this permission change
	[ -f $(srcdir)/go.sum ] && cp -pf $(srcdir)/go.sum $(builddir)/go.sum >/dev/null 2>&1 || true
	## ---------------------------------------------------------------
	CC=$(CC) \
	$(CGO_TARGET_ENV) \
	CGO_CFLAGS="-I$(abs_top_srcdir)/lib -I$(abs_top_builddir)/lib $(CFLAGS) $(CGO_CFLAGS_EXTRA)" \
	CGO_LDFLAGS="-L$(abs_top_builddir)/lib -L$(abs_top_builddir)/lib/.libs -lhak $(CGO_SYS_LIBS) $(LIBM) $(CGO_LDFLAGS_EXTRA)"  \
	go build -C $(srcdir) -ldflags "-X 'main.BINDIR=$(bindir)' -X 'main.SBINDIR=$(sbindir)' -X 'main.LIBDIR=$(libdir)' -X 'main.SYSCONFDIR=$(sysconfdir)'" -x -o $(abs_builddir)/$@ -modfile $(abs_top_srcdir)/go.mod  $(abs_srcdir)/main.go
	## ---------------------------------------------------------------
	go clean -C $(srcdir) -x -modfile $(abs_top_srcdir)/go.mod

## the go to o recipe is fake to deceive make
.go.o:
	echo $< > $@

.mod.o:
	echo $< > $@

endif


