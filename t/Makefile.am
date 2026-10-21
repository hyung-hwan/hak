AUTOMAKE_OPTIONS = nostdinc

AM_CPPFLAGS = \
	-I$(abs_builddir) \
	-I$(abs_builddir)/../lib \
	-I$(abs_srcdir) \
	-I$(abs_srcdir)/../lib \
	-I$(includedir) \
	-DHAK_TEST_MODLIBDIRS='"@abs_top_builddir@/mod:@abs_top_builddir@/mod/.libs"'
LDADD = ../lib/libhak.la

check_SCRIPTS = \
	assign-01.hak \
	char-01.hak \
	classof-01.hak \
	coll-01.hak \
	coll-02.hak \
	cons-01.hak \
	dic-01.hak \
	feed-01.hak \
	fork-01.hak \
	fork-02.hak \
	fun-01.hak \
	funval-01.hak \
	hnd-01.hak \
	hnd-02.hak \
	hnd-03.hak \
	insta-01.hak \
	insta-02.hak \
	liberal-01.hak \
	mutex-01.hak \
	mutex-02.hak \
	mutex-03.hak \
	mux-01.hak \
	mux-02.hak \
	mux-03.hak \
	mux-04.hak \
	prim-01.hak \
	proc-01.hak \
	proc-02.hak \
	proc-03.hak \
	proclib-01.hak \
	putio-01.hak \
	ret-01.hak \
	retvar-01.hak \
	scanf-01.hak \
	send-01.hak \
	sig-01.hak \
	sig-02.hak \
	stream-01.hak \
	super-01.hak \
	sysproc-01.hak \
	sysproc-02.hak \
	tick-01.hak \
	try-01.hak \
	until-01.hak \
	va-01.hak \
	va-02.hak \
	var-01.hak \
	var-02.hak \
	var-03.hak

check_ERRORS = \
	assign-5001.err \
	call-5001.err \
	class-5001.err \
	feed-5001.err \
	fun-5001.err \
	hnd-5001.err \
	liberal-5001.err \
	mlist-5001.err \
	send-5001.err \
	sig-5001.err \
	until-5001.err \
	va-5001.err \
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
## files pulled in by a test with $include. they carry no assertions of their
## own, so they are shipped but deliberately kept out of TESTS.
check_FIXTURES = \
	liberal-inc.hak \
	liberal-inc2.hak

## scripts driven through hakx rather than hak: xrun.sh starts a server, runs
## the client against it several times over and insists the server is still
## accepting afterwards. they are only built - and only meaningful - when
## libhakx and the hakx driver were configured in.
check_XTESTS = \
	x-basic-01.hakx \
	x-output-01.hakx \
	x-proc-01.hakx

EXTRA_DIST = $(check_SCRIPTS) $(check_ERRORS) $(check_FIXTURES) $(check_XTESTS) \
	run.sh err.sh xrun.sh

if ENABLE_HAKX
TESTS = $(check_PROGRAMS) $(check_SCRIPTS) $(check_ERRORS) $(check_XTESTS)
else
TESTS = $(check_PROGRAMS) $(check_SCRIPTS) $(check_ERRORS)
endif

TEST_EXTENSIONS = .hak .err .hakx

HAKBIN = $(top_builddir)/bin/hak
HAKXBIN = $(top_builddir)/bin/hakx

HAK_LOG_COMPILER = $(SHELL) $(abs_srcdir)/run.sh $(HAKBIN) --incdirs="@abs_top_srcdir@/src" --modlibdirs="@abs_top_builddir@/mod:@abs_top_builddir@/mod/.libs" --heapsize=0
AM_HAK_LOG_FLAGS =

ERR_LOG_COMPILER = $(SHELL) $(abs_srcdir)/err.sh $(HAKBIN) --incdirs="@abs_top_srcdir@/src" --modlibdirs="@abs_top_builddir@/mod:@abs_top_builddir@/mod/.libs" --heapsize=0
AM_ERR_LOG_FLAGS =

HAKX_LOG_COMPILER = $(SHELL) $(abs_srcdir)/xrun.sh $(HAKXBIN) "@abs_top_srcdir@/src"
AM_HAKX_LOG_FLAGS =
