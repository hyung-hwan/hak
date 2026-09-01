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
static int disposition_of (int sig, void** handler)
{
	struct sigaction sa;
	if (sigaction(sig, (struct sigaction*)0, &sa) <= -1) return -1;
	*handler = (sa.sa_flags & SA_SIGINFO)? (void*)sa.sa_sigaction: (void*)sa.sa_handler;
	return 0;
}
#else
static int disposition_of (int sig, void** handler) { *handler = (void*)0; return -1; }
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

/* Signal dispositions are process-wide, so the library must not touch them
 * behind the host's back. Neutralising SIGPIPE is the application's call -
 * bin/hak.c makes it - and a program that would rather die quietly on a broken
 * pipe is entitled to that. Opening and closing an instance must therefore
 * leave every disposition exactly as it found it. */
static const int WATCHED[] = {
#if defined(SIGPIPE)
	SIGPIPE,
#endif
	SIGINT, SIGTERM
};
#define NWATCHED ((int)(sizeof(WATCHED) / sizeof(WATCHED[0])))

static void host_signals_untouched (void)
{
	void* before[NWATCHED];
	void* during[NWATCHED];
	void* after[NWATCHED];
	hak_t* h;
	int i, probed = 0;

	for (i = 0; i < NWATCHED; i++)
	{
		if (disposition_of(WATCHED[i], &before[i]) <= -1) return; /* no sigaction */
		probed = 1;
	}
	if (!probed) return;

	h = hak_openstd(0, HAK_NULL);
	OK (h != HAK_NULL, "instantiation failure");
	if (!h) return;

	for (i = 0; i < NWATCHED; i++) disposition_of(WATCHED[i], &during[i]);
	hak_close(h);
	for (i = 0; i < NWATCHED; i++) disposition_of(WATCHED[i], &after[i]);

	for (i = 0; i < NWATCHED; i++)
	{
		OK (during[i] == before[i], "an open instance leaves the host disposition alone");
		OK (after[i] == before[i], "a closed instance leaves the host disposition alone");
	}
}

/* hak_catch_termreq() is the sanctioned way to hand hak the termination
 * signals, and hak_uncatch_termreq() must put the host's dispositions back
 * exactly as it found them. Nothing in the tree calls either, so this is the
 * only exercise they get - and the only coverage of the restore path in
 * unset_signal_handler(). */
static const int TERMREQ[] = {
	SIGTERM,
	SIGINT
#if defined(SIGHUP)
	, SIGHUP
#endif
};
#define NTERMREQ ((int)(sizeof(TERMREQ) / sizeof(TERMREQ[0])))

static void termreq_round_trips (void)
{
	void* before[NTERMREQ];
	void* during[NTERMREQ];
	void* after[NTERMREQ];
	int i;
#if defined(SIGPIPE)
	/* SIGPIPE is deliberately NOT in the set above. It is not a termination
	 * request - it is the opposite, a measure against being terminated - and
	 * neutralising it is the application's call, not the library's, because
	 * the disposition is process-wide. bin/hak.c makes that call for itself.
	 * So termreq must leave it exactly alone. */
	void* pipe_before;
	void* pipe_during;
	int pipe_probed;
#endif

	for (i = 0; i < NTERMREQ; i++)
	{
		if (disposition_of(TERMREQ[i], &before[i]) <= -1) return; /* no sigaction */
	}

#if defined(SIGPIPE)
	pipe_probed = (disposition_of(SIGPIPE, &pipe_before) >= 0);
#endif

	hak_catch_termreq();
	for (i = 0; i < NTERMREQ; i++) disposition_of(TERMREQ[i], &during[i]);
#if defined(SIGPIPE)
	if (pipe_probed) disposition_of(SIGPIPE, &pipe_during);
#endif

	hak_uncatch_termreq();
	for (i = 0; i < NTERMREQ; i++) disposition_of(TERMREQ[i], &after[i]);

	for (i = 0; i < NTERMREQ; i++)
	{
		OK (during[i] != before[i], "hak_catch_termreq installs a handler");
		OK (after[i] == before[i], "hak_uncatch_termreq restores the original");
	}
#if defined(SIGPIPE)
	if (pipe_probed) OK (pipe_during == pipe_before, "hak_catch_termreq leaves SIGPIPE to the application");
#endif
}

int main (int argc, char* argv[])
{
	if (argc > 1) return stress(atoi(argv[1]));

	no_plan();

	host_signals_untouched();
	termreq_round_trips();

	interleaved();
	survivor();

	return exit_status();
}
