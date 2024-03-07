#!/bin/sh
## use a subshell to execute the actual program in case the
## program crashes or exits without an error message.
($@ 2>&1 || echo "ERROR: exited with $?") | grep -E '^ERROR:' && exit 1
exit 0
