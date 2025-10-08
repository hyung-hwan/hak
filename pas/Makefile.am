AUTOMAKE_OPTIONS = nostdinc

bin_PROGRAMS = hakpas
hakpas_SOURCES = hak.pas main.pas 
hakpas_CPPFLAGS = 
hakpas_DEPENDENCIES = hakpas.bin

PASFLAGS = -Mobjfpc -FcUTF8 -Sm -g
if HAK_LIB_QUADMATH_REQUIRED
PASFLAGS += -dHAK_LIB_QUADMATH_REQUIRED
endif
if HAK_WIDE_CHAR_SIZE_IS_4
PASFLAGS += -dHAK_WIDE_CHAR_SIZE_IS_4
endif

hakpas_LINK = cp -pf hakpas.bin $(builddir)/hakpas$(EXEEXT) || echo 

hakpas.bin: $(hakpas_SOURCES) ../lib/libhak.la $(hakpas_OBJECTS)
	FL='-Fl../lib:../lib/.libs'; \
	for i in @HAK_SYS_LIB_SEARCH_PATH_SPEC@; do \
		FL="$${FL}:$${i}"; \
	done; \
	echo fpc -o$(builddir)/$@ $(PASFLAGS) "$${FL}" $(srcdir)/main.pas; \
	fpc -o$(builddir)/$@ $(PASFLAGS) $(PASFLAGS) "$${FL}" $(srcdir)/main.pas

clean-local:
	rm -f *.ppu *.res hakpas.bin
