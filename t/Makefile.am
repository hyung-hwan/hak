AUTOMAKE_OPTIONS = nostdinc

AM_CPPFLAGS = \
	-I$(abs_builddir) \
	-I$(abs_builddir)/../lib \
	-I$(abs_srcdir) \
	-I$(abs_srcdir)/../lib \
	-I$(includedir)
LDADD = ../lib/libhak.la

check_SCRIPTS = \
	classof-01.hak \
	cons-01.hak \
	dic-01.hak \
	feed-01.hak \
	fun-01.hak \
	hnd-01.hak \
	hnd-02.hak \
	hnd-03.hak \
	insta-01.hak \
	insta-02.hak \
	mux-01.hak \
	mux-02.hak \
	mux-03.hak \
	prim-01.hak \
	proc-01.hak \
	proclib-01.hak \
	ret-01.hak \
	retvar-01.hak \
	sig-01.hak \
	sig-02.hak \
	sysproc-01.hak \
	sysproc-02.hak \
	tick-01.hak \
	try-01.hak \
	va-01.hak \
	var-01.hak \
	var-02.hak \
	var-03.hak

check_ERRORS = \
	call-5001.err \
	class-5001.err \
	feed-5001.err \
	hnd-5001.err \
	mlist-5001.err \
	sig-5001.err \
	var-5001.err \
	var-5002.err \
	var-5004.err

check_PROGRAMS = \
	t-001 \
	t-002 \
	t-003 \
	t-004

t_001_SOURCES = t-001.c tap.h
t_002_SOURCES = t-002.c tap.h
t_003_SOURCES = t-003.c tap.h
t_004_SOURCES = t-004.c tap.h

##noinst_SCRIPTS = $(check_SCRIPTS)
EXTRA_DIST = $(check_SCRIPTS) $(check_ERRORS)

TESTS = $(check_PROGRAMS) $(check_SCRIPTS) $(check_ERRORS)

TEST_EXTENSIONS = .hak .err

HAKBIN = $(top_builddir)/bin/hak

HAK_LOG_COMPILER = $(SHELL) $(abs_srcdir)/run.sh $(HAKBIN) --modlibdirs="@abs_top_builddir@/mod:@abs_top_builddir@/mod/.libs" --heapsize=0
AM_HAK_LOG_FLAGS =

ERR_LOG_COMPILER = $(SHELL) $(abs_srcdir)/err.sh $(HAKBIN) --modlibdirs="@abs_top_builddir@/mod:@abs_top_builddir@/mod/.libs" --heapsize=0
AM_ERR_LOG_FLAGS =
