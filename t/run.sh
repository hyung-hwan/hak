#!/bin/sh
## use a subshell to execute the actual program in case the
## program crashes or exits without an error message.
echo RUN "[$@]"
## [NOTE] the marker is looked for anywhere on a line rather than anchored to its
## start. a test that writes to the output handler can leave text sitting in
## front of it - "5ERROR: ..." when a write of "5" precedes the report - and an
## anchored match would let that failure pass unnoticed.
##
## "$@" is quoted so the argument vector reaches the program exactly as received.
## unquoted $@ re-splits each argument, which mangles any that contains a space -
## --incdirs= carries a path, so this is not hypothetical.
("$@" 2>&1 || echo "ERROR: exited with $?") | grep -E 'ERROR:' && exit 1
##[ "x$MEMCHECK" = "xyes" ] && {
##	[ -x /usr/bin/valgrind ] && {
##		valgrind --leak-check=full --show-reachable=yes --track-fds=yes --log-file=/tmp/x "$@" 2>&1
##	}
##}
exit 0
