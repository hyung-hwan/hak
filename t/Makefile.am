AUTOMAKE_OPTIONS = nostdinc

check_SCRIPTS = \
	fun-01.hcl \
	insta-01.hcl \
	insta-02.hcl \
	ret-01.hcl \
	retvar-01.hcl \
	va-01.hcl \
	var-02.hcl

check_ERRORS = \
	call-01.err \
	class-01.err \
	do-01.err \
	do-02.err \
	feed-01.err \
	feed-02.err \
	feed-03.err \
	feed-04.err \
	mlist-01.err \
	var-01.err \
	var-02.err \
	var-03.err \
	var-04.err

##noinst_SCRIPTS = $(check_SCRIPTS)
EXTRA_DIST = $(check_SCRIPTS) $(check_ERRORS)

check_PROGRAMS =

TESTS = $(check_PROGRAMS) $(check_SCRIPTS) $(check_ERRORS)

TEST_EXTENSIONS = .hcl .err

HCL_LOG_COMPILER = sh $(abs_srcdir)/run.sh ../bin/hcl --modlibdirs="@abs_top_builddir@/mod:@abs_top_builddir@/mod/.libs" --heapsize=0 -b -n
AM_HCL_LOG_FLAGS =

ERR_LOG_COMPILER = sh $(abs_srcdir)/err.sh ../bin/hcl --modlibdirs="@abs_top_builddir@/mod:@abs_top_builddir@/mod/.libs" --heapsize=0 -b -n
AM_ERR_LOG_FLAGS =
