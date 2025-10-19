AUTOMAKE_OPTION = foreign
ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = t t/test-bi.hak

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

if ENABLE_HAKGO

## the attempt to compose a proper procedure using a regular compiler failed.
## e.g ./configure GOC=opt/go/pkg/tool/linux_amd64/
## while go.m4 is included in autoconf 2.71, the support for the go language
## is very limited and the go language itself is pursuing the module based builder.
##
## the following is to trick autoconf/automake as if it's building go files with
## a real compiler whereas the actual building is done thru `go build`

bin_PROGRAMS = hakgo
hakgo_SOURCES = \
	go/hak.go \
	go/cb.go \
	go/inst.go \
	main.go \
	go.mod
hakgo_DEPENDENCIES = hakgo.bin

## let the linker to move hakgo.bin to the actual target
hakgo_LINK = cp -pf hakgo.bin hakgo$(EXEEXT) || echo "FAILED TO LINK"

if ENABLE_STATIC
CGO_CFLAGS_EXTRA="-static"
CGO_LDFLAGS_EXTRA="-static"
else
CGO_CFLAGS_EXTRA=""
CGO_LDFLAGS_EXTRA=""
endif

hakgo.bin: lib/libhak.la $(hakgo_OBJECTS)
	cp -pf $(srcdir)/go.mod $(builddir)/go.mod >/dev/null 2>&1 || true
	chmod u+w $(builddir)/go.mod ## with `make distcheck`, the echo's redirection to the file fails without this permission change
	[ -f $(srcdir)/go.sum ] && cp -pf $(srcdir)/go.sum $(builddir)/go.sum >/dev/null 2>&1 || true
	## ---------------------------------------------------------------
	CC=$(CC) \
	CGO_CFLAGS="-I$(abs_srcdir)/lib -I$(abs_builddir)/lib $(CFLAGS) $(CGO_CFLAGS_EXTRA)" \
	CGO_LDFLAGS="-L$(abs_builddir)/lib -L$(abs_builddir)/lib/.libs -lhak -ldl $(LIBM) $(CGO_LDFLAGS_EXTRA)"  \
	go build -C $(srcdir) -ldflags "-X 'main.BINDIR=$(bindir)' -X 'main.SBINDIR=$(sbindir)' -X 'main.LIBDIR=$(libdir)' -X 'main.SYSCONFDIR=$(sysconfdir)'" -x -o $(abs_builddir)/hakgo.bin -modfile $(abs_builddir)/go.mod
	## ---------------------------------------------------------------
	go clean -C $(srcdir) -x -modfile $(abs_builddir)/go.mod

## the go to o recipe is fake to deceive make
.go.o:
	echo $< > $@

.mod.o:
	echo $< > $@


endif


