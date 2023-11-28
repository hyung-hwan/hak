AUTOMAKE_OPTIONS = nostdinc

check_SCRIPTS = \
	fun-01.hcl \
	insta-01.hcl \
	insta-02.hcl \
	ret-01.hcl \
	retvar-01.hcl \
	va-01.hcl

check_ERRORS = \
	do-01.err \
	do-02.err \
	feed-01.err \
	var-01.err \
	var-02.err \
	var-03.err

##noinst_SCRIPTS = $(check_SCRIPTS)
EXTRA_DIST = $(check_SCRIPTS) $(check_ERRORS)

check_PROGRAMS =

TESTS = $(check_PROGRAMS) $(check_SCRIPTS) $(check_ERRORS)

TEST_EXTENSIONS = .hcl .err

HCL_LOG_COMPILER = sh $(abs_srcdir)/run.sh ../bin/hcl -x
AM_HCL_LOG_FLAGS =

ERR_LOG_COMPILER = sh $(abs_srcdir)/err.sh ../bin/hcl -x
AM_ERR_LOG_FLAGS =
