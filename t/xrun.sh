#!/bin/sh
## ---------------------------------------------------------------------------
## Run one hak script through hakx: start a server, drive it with the client,
## and insist that the script's own assertions pass.
##
## A test passes when no client output contains a line matching ERROR: - the
## same rule run.sh applies to a plain script - and every client exits 0.
##
## WHY EACH TEST RUNS THREE TIMES
##
## One client is not enough to exercise this pair. The connection patterns
## below fail independently of each other:
##
##   single      one client against a fresh server.
##
##   sequential  a second client after the first has finished. This is the
##               pattern that matters most. A finishing worker nudges the
##               accept loop by writing to the wake channel, and that nudge is
##               the first thing to touch it - so a server can serve its first
##               client perfectly and then never accept another. A one-client
##               test cannot see that class of bug at all.
##
##   concurrent  two clients at once, so more than one worker is alive and the
##               poll set holds more than one descriptor.
##
## usage: xrun.sh <hakx> <script-include-dir> <script>
## ---------------------------------------------------------------------------

hakx=$1
incdir=$2
shift 2
script=$1

echo "RUN [$hakx server+client $script]"

if [ ! -x "$hakx" ]; then
	echo "ERROR: $hakx is not executable"
	exit 1
fi

work=xrun-$$.dir
srvpid=
rm -rf "$work"
mkdir "$work" || { echo "ERROR: cannot create $work"; exit 1; }

cleanup()
{
	if [ -n "$srvpid" ]; then
		kill "$srvpid" 2>/dev/null
		wait "$srvpid" 2>/dev/null
	fi
	rm -rf "$work"
}
trap cleanup EXIT
trap 'cleanup; exit 1' INT TERM

## a script of its own for the readiness probe, so that a server which is not
## listening yet is never mistaken for a test that failed
echo 'printf "XRUNREADY\n"' > "$work/ready.hak"

## ---------------------------------------------------------------------------
## bring a server up on a port nothing else holds
## ---------------------------------------------------------------------------
port=`expr 20000 + \( $$ % 20000 \)`
attempt=0
ready=no

while [ "$attempt" -lt 5 ]; do
	"$hakx" server --script-include-path="$incdir" \
		-l"$work/server.log",error+ "127.0.0.1:$port" >"$work/server.out" 2>&1 &
	srvpid=$!

	## the probe doubles as the wait for the listener: a client that gets its
	## answer proves the server is accepting
	i=0
	while [ "$i" -lt 30 ]; do
		if ! kill -0 "$srvpid" 2>/dev/null; then
			break   ## server gave up - most likely the port was taken
		fi
		if "$hakx" client "127.0.0.1:$port" "$work/ready.hak" 2>&1 | grep -q XRUNREADY; then
			ready=yes
			break
		fi
		i=`expr $i + 1`
		sleep 1
	done

	[ "$ready" = yes ] && break

	kill "$srvpid" 2>/dev/null
	wait "$srvpid" 2>/dev/null
	srvpid=
	port=`expr $port + 1`
	attempt=`expr $attempt + 1`
done

if [ "$ready" != yes ]; then
	echo "ERROR: no server came up after $attempt attempts"
	echo "--- server log ---"; cat "$work/server.log" 2>/dev/null
	echo "--- server output ---"; cat "$work/server.out" 2>/dev/null
	exit 1
fi

## ---------------------------------------------------------------------------
## the phases
## ---------------------------------------------------------------------------
rc=0

## report on one client run. $1 phase name, $2 exit status, $3 output file
judge()
{
	_phase=$1
	_status=$2
	_out=$3

	if [ "$_status" -ne 0 ]; then
		echo "ERROR: $_phase - client exited with $_status"
		rc=1
	elif [ ! -s "$_out" ]; then
		echo "ERROR: $_phase - client produced no output"
		rc=1
	fi

	if grep -E 'ERROR:' "$_out" >/dev/null 2>&1; then
		echo "--- $_phase ---"
		cat "$_out"
		rc=1
	else
		echo "ok $_phase"
	fi
}

"$hakx" client "127.0.0.1:$port" "$script" >"$work/single.out" 2>&1
judge single $? "$work/single.out"

"$hakx" client "127.0.0.1:$port" "$script" >"$work/seq.out" 2>&1
judge sequential $? "$work/seq.out"

"$hakx" client "127.0.0.1:$port" "$script" >"$work/con-a.out" 2>&1 &
pa=$!
"$hakx" client "127.0.0.1:$port" "$script" >"$work/con-b.out" 2>&1 &
pb=$!
wait "$pa"; sa=$?
wait "$pb"; sb=$?
judge concurrent-a "$sa" "$work/con-a.out"
judge concurrent-b "$sb" "$work/con-b.out"

## the server must still be alive and accepting after all of that
if ! kill -0 "$srvpid" 2>/dev/null; then
	echo "ERROR: the server died during the run"
	echo "--- server log ---"; cat "$work/server.log" 2>/dev/null
	rc=1
elif ! "$hakx" client "127.0.0.1:$port" "$work/ready.hak" 2>&1 | grep -q XRUNREADY; then
	echo "ERROR: the server stopped accepting after the run"
	echo "--- server log ---"; cat "$work/server.log" 2>/dev/null
	rc=1
else
	echo "ok server still accepting"
fi

## anything the server itself logged at error level is a failure too
if [ -s "$work/server.log" ]; then
	echo "ERROR: the server logged errors"
	cat "$work/server.log"
	rc=1
fi

exit $rc
