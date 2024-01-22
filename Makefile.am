AUTOMAKE_OPTION = foreign
ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = t t/test-bi.hcl

SUBDIRS =

if ENABLE_STATIC_MODULE
SUBDIRS += mod lib bin t
else
SUBDIRS += lib mod bin t
endif

DIST_SUBDIRS = $(SUBDIRS)

distclean-local:
	@rm -rf $(top_srcdir)/autom4te.cache


clean-local:
	rm -rf $(builddir)/go.mod $(builddir)/go.sum

if ENABLE_HCLGO

## the attempt to compose a proper procedure using a regular compiler failed.
## e.g ./configure GOC=opt/go/pkg/tool/linux_amd64/
## while go.m4 is included in autoconf 2.71, the support for the go language
## is very limited and the go language itself is pursuing the module based builder.
##
## the following is to trick autoconf/automake as if it's building go files with
## a real compiler whereas the actual building is done thru `go build`

bin_PROGRAMS = hclgo
hclgo_SOURCES = \
	go/hcl.go \
	go/cb.go \
	go/inst.go \
	main.go \
	go.mod
hclgo_DEPENDENCIES = hclgo.bin

## let the linker to move hclgo.bin to the actual target
hclgo_LINK = cp -pf hclgo.bin hclgo$(EXEEXT) || echo "FAILED TO LINK"

hclgo.bin: lib/libhcl.la $(hclgo_OBJECTS)
	cp -pf $(srcdir)/go.mod $(builddir)/go.mod >/dev/null 2>&1 || true
	chmod u+w $(builddir)/go.mod ## with `make distcheck`, the echo's redirection to the file fails without this permission change
	[ -f $(srcdir)/go.sum ] && cp -pf $(srcdir)/go.sum $(builddir)/go.sum >/dev/null 2>&1 || true
	## ---------------------------------------------------------------
	CC=$(CC) \
	CGO_CFLAGS="-I$(abs_srcdir)/lib -I$(abs_builddir)/lib $(CFLAGS)" \
	CGO_LDFLAGS="-L$(abs_builddir)/lib -L$(abs_builddir)/lib/.libs -lhcl -ldl -lquadmath"  \
	go build -C $(srcdir) -ldflags "-X 'main.BINDIR=$(bindir)' -X 'main.SBINDIR=$(sbindir)' -X 'main.LIBDIR=$(libdir)' -X 'main.SYSCONFDIR=$(sysconfdir)'" -x -o $(abs_builddir)/hclgo.bin -modfile $(abs_builddir)/go.mod
	## ---------------------------------------------------------------
	go clean -C $(srcdir) -x -modfile $(abs_builddir)/go.mod

## the go to o recipe is fake to deceive make
.go.o:
	echo $< > $@

.mod.o:
	echo $< > $@


endif


