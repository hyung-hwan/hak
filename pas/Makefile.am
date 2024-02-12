AUTOMAKE_OPTIONS = nostdinc

bin_PROGRAMS = hcl
hcl_SOURCES = hcl.pas main.pas 
hcl_CPPFLAGS = 
hcl_DEPENDENCIES = hcl.bin

PASFLAGS = -Mobjfpc -FcUTF8 -g
if HCL_LIB_QUADMATH_REQUIRED
PASFLAGS += -dHCL_LIB_QUADMATH_REQUIRED
endif

hcl_LINK = cp -pf hcl.bin $(builddir)/hcl$(EXEEXT) || echo 

hcl.bin: $(hcl_SOURCES) ../lib/libhcl.la $(hcl_OBJECTS)
	FL='-Fl../lib:../lib/.libs'; \
	for i in @HCL_SYS_LIB_SEARCH_PATH_SPEC@; do \
		FL="$${FL}:$${i}"; \
	done; \
	echo fpc -o$(builddir)/$@ $(PASFLAGS) "$${FL}" $(srcdir)/main.pas; \
	fpc -o$(builddir)/$@ $(PASFLAGS) $(PASFLAGS) "$${FL}" $(srcdir)/main.pas

clean-local:
	rm -f *.ppu *.res hcl.bin
