AUTOMAKE_OPTIONS = nostdinc

AM_CPPFLAGS = \
	-I$(abs_builddir) \
	-I$(abs_builddir)/../lib \
	-I$(abs_srcdir) \
	-I$(abs_srcdir)/../lib \
	-I$(includedir)
LDADD = ../lib/libhak.la

check_SCRIPTS = \
	feed-01.hak \
	fun-01.hak \
	insta-01.hak \
	insta-02.hak \
	prim-01.hak \
	proc-01.hak \
	ret-01.hak \
	retvar-01.hak \
	va-01.hak \
	var-01.hak \
	var-02.hak \
	var-03.hak

check_ERRORS = \
	call-5001.err \
	class-5001.err \
	feed-5001.err \
	mlist-5001.err \
	var-5001.err \
	var-5002.err \
	var-5004.err

check_PROGRAMS = \
	t-001

t_001_SOURCES = t-001.c tap.h

##noinst_SCRIPTS = $(check_SCRIPTS)
EXTRA_DIST = $(check_SCRIPTS) $(check_ERRORS)

TESTS = $(check_PROGRAMS) $(check_SCRIPTS) $(check_ERRORS)

TEST_EXTENSIONS = .hak .err

HAKBIN = $(top_builddir)/bin/hak

HAK_LOG_COMPILER = $(SHELL) $(abs_srcdir)/run.sh $(HAKBIN) --modlibdirs="@abs_top_builddir@/mod:@abs_top_builddir@/mod/.libs" --heapsize=0
AM_HAK_LOG_FLAGS =

ERR_LOG_COMPILER = $(SHELL) $(abs_srcdir)/err.sh $(HAKBIN) --modlibdirs="@abs_top_builddir@/mod:@abs_top_builddir@/mod/.libs" --heapsize=0
AM_ERR_LOG_FLAGS =
