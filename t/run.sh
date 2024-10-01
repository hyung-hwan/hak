#!/bin/sh
## use a subshell to execute the actual program in case the
## program crashes or exits without an error message.
echo RUN "[$@]"
($@ 2>&1 || echo "ERROR: exited with $?") | grep -E '^ERROR:' && exit 1
##[ "x$MEMCHECK" = "xyes" ] && {
##	[ -x /usr/bin/valgrind ] && {
##		valgrind --leak-check=full --show-reachable=yes --track-fds=yes --log-file=/tmp/x "$@" 2>&1
##	}
##}
exit 0
