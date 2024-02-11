AUTOMAKE_OPTIONS = nostdinc

bin_PROGRAMS = hcl
hcl_SOURCES = hcl.pas main.pas 
hcl_CPPFLAGS = 
hcl_DEPENDENCIES = hcl.bin

hcl_LINK = cp -pf hcl.bin $(builddir)/hcl$(EXEEXT) || echo 

hcl.bin: $(hcl_SOURCES) ../lib/libhcl.la $(hcl_OBJECTS)
	FL='-Fl../lib:../lib/.libs'; \
	for i in @HCL_CONFIGURE_SYS_LIB_SEARCH_PATH_SPEC@; do \
		FL="$${FL}:$${i}"; \
	done; \
	echo fpc -o$(builddir)/$@ -g -Mobjfpc -FcUTF8 "$${FL}" $(srcdir)/main.pas; \
	fpc -o$(builddir)/$@ -g -Mobjfpc -FcUTF8 "$${FL}" $(srcdir)/main.pas

clean-local:
	rm -f *.ppu *.res hcl.bin
