AUTOMAKE_OPTIONS = nostdinc

check_SCRIPTS = \
	feed-01.hcl \
	fun-01.hcl \
	insta-01.hcl \
	insta-02.hcl \
	ret-01.hcl \
	retvar-01.hcl \
	va-01.hcl \
	var-02.hcl

check_ERRORS = \
	call-5001.err \
	class-5001.err \
	do-5001.err \
	do-5002.err \
	feed-5001.err \
	feed-5002.err \
	feed-5003.err \
	feed-5004.err \
	feed-5005.err \
	feed-5006.err \
	feed-5007.err \
	mlist-5001.err \
	var-5001.err \
	var-5002.err \
	var-5003.err \
	var-5004.err

##noinst_SCRIPTS = $(check_SCRIPTS)
EXTRA_DIST = $(check_SCRIPTS) $(check_ERRORS)

check_PROGRAMS =

TESTS = $(check_PROGRAMS) $(check_SCRIPTS) $(check_ERRORS)

TEST_EXTENSIONS = .hcl .err

HCL_LOG_COMPILER = sh $(abs_srcdir)/run.sh ../bin/hcl --modlibdirs="@abs_top_builddir@/mod:@abs_top_builddir@/mod/.libs" --heapsize=0 -b -n
AM_HCL_LOG_FLAGS =

ERR_LOG_COMPILER = sh $(abs_srcdir)/err.sh ../bin/hcl --modlibdirs="@abs_top_builddir@/mod:@abs_top_builddir@/mod/.libs" --heapsize=0 -b -n
AM_ERR_LOG_FLAGS =
