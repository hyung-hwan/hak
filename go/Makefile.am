AUTOMAKE_OPTIONS = nostdinc

## the attempt to compose a proper procedure using a regular compiler failed.
## e.g ./configure GOC=opt/go/pkg/tool/linux_amd64/
## while go.m4 is included in autoconf 2.71, the support for the go language
## is very limited and the go language itself is pursuing the module based builder.
##
## the following is to trick autoconf/automake as if it's building go files with
## a real compiler whereas the actual building is done thru `go build`

bin_PROGRAMS = hclgo
hclgo_SOURCES = \
	pkg/hcl.go \
	pkg/cb.go \
	pkg/inst.go \
	main.go \
	go.mod
hclgo_DEPENDENCIES = hclgo.bin

## let the linker to move hclgo.bin to the actual target
hclgo_LINK = mv -f hclgo.bin hclgo$(EXEEXT) || echo "FAILED TO LINK"

hclgo.bin:
	cp -pf $(srcdir)/go.mod $(builddir)/go.mod >/dev/null 2>&1 || true
	chmod u+w $(builddir)/go.mod ## with `make distcheck`, the echo's redirection to the file fails without this permission change
	sed -ri "/^[[:space:]]*replace[[:space:]]+cfg[[:space:]]*=>/d" $(builddir)/go.mod
	echo -e "\nreplace cfg => $(abs_builddir)/cfg" >> $(builddir)/go.mod
	[ -f $(srcdir)/go.sum ] && cp -pf $(srcdir)/go.sum $(builddir)/go.sum >/dev/null 2>&1 || true
	go build -C $(srcdir) -x -o $(abs_builddir)/hclgo.bin -modfile $(abs_builddir)/go.mod
	go clean -C $(srcdir) -x -modfile $(abs_builddir)/go.mod
	rm -rf $(builddir)/go.mod $(builddir)/go.sum

## the go to o recipe is fake to deceive make
.go.o:
	echo $< > $@

.mod.o:
	echo $< > $@


clean-local:
	rm -rf $(builddir)/go.mod $(builddir)/go.sum
