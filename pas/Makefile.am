AUTOMAKE_OPTIONS = nostdinc

bin_PROGRAMS = hcl
hcl_SOURCES = hcl.pas main.pas 
hcl_CPPFLAGS = 
hcl_DEPENDENCIES = hcl.bin

hcl_LINK = cp -pf hcl.bin $(builddir)/hcl$(EXEEXT) || echo 

hcl.bin: $(hcl_SOURCES) ../lib/libhcl.la $(hcl_OBJECTS)
	fpc -o$(builddir)/$@ -g -Mobjfpc -FcUTF8 -Fl../lib -Fl../lib/.libs -Fl/usr/lib/gcc/x86_64-linux-gnu/11 $(srcdir)/main.pas

clean-local:
	rm -f *.ppu *.res hcl.bin
