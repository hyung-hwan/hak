/* multiple hak instances in one process.
 *
 * lib/std.c keeps process-wide state that no single hak instance owns: the
 * g_hak chain that links every live instance, and g_sig_state[] holding the
 * signal dispositions saved before hak installed its own. An embedder may
 * hold several instances at once, so that state is shared and is guarded by
 * a global lock rather than by any per-instance one.
 *
 * Run with no arguments, as make check does, this covers the sequential case,
 * which is deterministic and is what regressions are caught by. Pass a thread
 * count for the concurrent stress mode:
 *
 *     ./t-002 8
 *
 * The stress mode is kept out of make check because its runtime scales with
 * the thread count and it is timing dependent, so it is a poor gate even
 * though it passes. Reach for it when touching chain(), unchain() or the
 * signal handler bookkeeping. */

#include <hak.h>
#include "tap.h"

#include <signal.h>
#include <stdlib.h>

#if !defined(__DOS__) && !defined(EMSCRIPTEN) && defined(HAVE_PTHREAD) && defined(HAVE_STRERROR_R)
#	define USE_THREAD
#	include <pthread.h>
#endif

#define NINST 4

#if defined(HAVE_SIGACTION)
static int sigpipe_handled (void)
{
	struct sigaction sa;
	if (sigaction(SIGPIPE, (struct sigaction*)0, &sa) <= -1) return -1;
	return sa.sa_handler != SIG_DFL;
}
#else
static int sigpipe_handled (void) { return -1; }
#endif

/* open NINST instances, then close them in an order that leaves the one being
 * unchained with a live neighbour on each side, which the plain open-then-close
 * pattern never produces. */
static void interleaved (void)
{
	hak_t* inst[NINST];
	int i;
	static const int order[NINST] = { 1, 3, 0, 2 }; /* middles first */

	for (i = 0; i < NINST; i++)
	{
		inst[i] = hak_openstd(0, HAK_NULL);
		OK (inst[i] != HAK_NULL, "instantiation with siblings already open");
	}

	for (i = 0; i < NINST; i++)
	{
		if (inst[order[i]]) hak_close(inst[order[i]]);
		inst[order[i]] = HAK_NULL;
	}

	/* the chain must still be usable once emptied out of order */
	inst[0] = hak_openstd(0, HAK_NULL);
	OK (inst[0] != HAK_NULL, "instantiation after out-of-order teardown");
	if (inst[0]) hak_close(inst[0]);
}

/* a surviving instance must still work after its neighbours are gone */
static void survivor (void)
{
	hak_t* keep;
	hak_t* tmp;
	int i, n;

	keep = hak_openstd(0, HAK_NULL);
	OK (keep != HAK_NULL, "instantiation of the survivor");
	if (!keep) return;

	for (i = 0; i < NINST; i++)
	{
		tmp = hak_openstd(0, HAK_NULL);
		if (tmp) hak_close(tmp);
	}

	n = hak_ignite(keep, 0);
	OK (n == 0, "survivor ignites after its neighbours are closed");
	n = hak_addbuiltinprims(keep);
	OK (n == 0, "survivor registers builtin primitives");

	hak_close(keep);
}

#if defined(USE_THREAD)
/* Concurrent open/close, hammering the global lock that guards g_hak and
 * g_sig_state. Removing either GLOBAL_LOCK() in lib/std.c makes this fault
 * within a couple of runs at eight threads. */
#define ROUNDS 150
static void* worker (void* arg)
{
	int i;
	for (i = 0; i < ROUNDS; i++)
	{
		hak_t* h = hak_openstd(0, HAK_NULL);
		if (h) hak_close(h);
	}
	return (void*)0;
}

static int stress (int nthr)
{
	pthread_t t[64];
	int i;

	if (nthr < 1) nthr = 1;
	if (nthr > 64) nthr = 64;

	for (i = 0; i < nthr; i++)
	{
		if (pthread_create(&t[i], (pthread_attr_t*)0, worker, (void*)0) != 0) break;
	}
	nthr = i;
	for (i = 0; i < nthr; i++) pthread_join(t[i], (void**)0);

	printf("# %d threads x %d instances completed\n", nthr, ROUNDS);
	return 0;
}
#else
static int stress (int nthr)
{
	printf("# built without thread support - stress mode unavailable\n");
	return 0;
}
#endif

int main (int argc, char* argv[])
{
	int before, during;

	if (argc > 1) return stress(atoi(argv[1]));

	no_plan();

	before = sigpipe_handled();

	{
		hak_t* h = hak_openstd(0, HAK_NULL);
		OK (h != HAK_NULL, "instantiation failure");
		during = sigpipe_handled();
		if (h) hak_close(h);
	}

	/* hak neutralises SIGPIPE so a write to a dead pipe cannot kill the host.
	 * It is installed on the first instance and, by design, is never restored
	 * on close - so it stays installed once any instance has existed. */
	if (before >= 0)
	{
		OK (during == 1, "SIGPIPE is handled while an instance is open");
	}
	else
	{
		printf("# sigaction unavailable - SIGPIPE disposition not checked\n");
	}

	interleaved();
	survivor();

	return exit_status();
}
