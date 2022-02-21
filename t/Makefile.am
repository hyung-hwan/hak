AUTOMAKE_OPTIONS = nostdinc

check_SCRIPTS = \
	ret-01.hcl \
	retvar-01.hcl

##noinst_SCRIPTS = $(check_SCRIPTS)
EXTRA_DIST = $(check_SCRIPTS)

check_PROGRAMS =


TESTS = $(check_PROGRAMS) $(check_SCRIPTS)

TEST_EXTENSIONS = .hcl
HCL_LOG_COMPILER = ../bin/hcl
AM_HCL_LOG_FLAGS =
