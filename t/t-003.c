/* hak_raisetick() and hak_rcvtick() - the per-instance tick counter pair.
 *
 * The tick is published as a counter rather than a flag so that the raiser and
 * the scheduler never write the same field: hak_raisetick() only increments
 * hak->tick, and the scheduler only writes hak->last_tick, copying the value
 * it observed. A raise landing between the scheduler's test and its update is
 * therefore still pending afterwards rather than being lost, and no atomic
 * operation is needed to get that.
 *
 * This checks that contract directly on the fields, which is white-box but is
 * the only way to reach hak_raisetick() - it has no script-level binding, and
 * driving it through a running VM cannot isolate it from the global tick.
 *
 * The second half then drives it end to end: a script forks a process and
 * spins without ever yielding, and hak_raisetick() is called from the
 * vm_checkbc callback - ordinary context, where an embedder would call it.
 * The OS ticker is deliberately never started, so gtick never moves and the
 * global half plays no part; only the per-instance tick can break the spin.
 * t/tick-01.hak covers the global half. */

#include <hak.h>
#include "tap.h"
#include <string.h>

static void state_contract (void)
{
	hak_t* hak;

	hak = hak_openstd(0, HAK_NULL);
	OK (hak != HAK_NULL, "instantiation");
	if (!hak) return;

	/* enabling reception seeds the watermark, so an instance does not act on
	 * ticks raised before it was listening */
	hak_raisetick(hak);
	hak_raisetick(hak);
	hak_rcvtick(hak, 1);
	OK (hak->last_tick == hak->tick, "enabling reception discards earlier ticks");

	hak_raisetick(hak);
	OK (hak->last_tick != hak->tick, "hak_raisetick leaves a tick pending");

	/* a second raise while one is already pending must not cancel it - the
	 * flag version could lose one here, the counter cannot */
	hak_raisetick(hak);
	OK (hak->last_tick != hak->tick, "a second raise keeps the tick pending");

	/* the scheduler consumes a tick by copying, never by clearing */
	hak->last_tick = hak->tick;
	OK (hak->last_tick == hak->tick, "copying the observed value consumes it");

	hak_raisetick(hak);
	OK (hak->last_tick != hak->tick, "and a later raise is seen again");

	/* disabling reception must not lose a pending tick either - rcv_tick
	 * gates whether ticks are acted on, not whether they are recorded */
	hak_rcvtick(hak, 0);
	OK (hak->last_tick != hak->tick, "disabling reception leaves the tick recorded");
	OK (hak->rcv_tick == 0, "reception is off");

	hak_rcvtick(hak, 1);
	OK (hak->last_tick == hak->tick, "re-enabling reception reseeds the watermark");
	OK (hak->rcv_tick == 1, "reception is on");

	hak_close(hak);
}

/* ------------------------------------------------------------------------ */

/* The spinner breaks out as soon as the forked process sets the flag, so a
 * count below the bound means the tick was delivered and acted on.
 *
 * hak_execute() answers the value of whichever process finishes LAST, so the
 * sem-wait after the loop is load-bearing: it makes the main process finish
 * last in both outcomes, and the returned value is therefore always its own
 * counter rather than the forked process's. Without it, a run in which nothing
 * preempted the spinner would answer the setter's value and read as a pass. */
#define SPIN_BOUND      300000
#define BC_BEFORE_RAISE  20000

static const char SRC[] =
	"flag := 0\n"
	"s := (sem-new)\n"
	"fun setter() { flag := 1 ; sem-signal s }\n"
	"p := (fork setter)\n"
	"i := 0\n"
	"while (< i 300000) { if (== flag 1) { break } ; i := (+ i 1) }\n"
	"sem-wait s\n"
	"r := i\n";

static hak_oow_t bc_seen = 0;
static int raised = 0;

static void cb_checkbc (hak_t* hak, hak_oob_t bcode)
{
	if (!raised && ++bc_seen >= BC_BEFORE_RAISE)
	{
		raised = 1;
		hak_raisetick(hak);
	}
}

static int on_cnode (hak_t* hak, hak_cnode_t* obj)
{
	return hak_compile(hak, obj, 0);
}

static void preempts_a_spinner (void)
{
	hak_t* hak;
	hak_cb_t cb;
	hak_bitmask_t trait;
	hak_oop_t retv;
	hak_ooi_t iters = -1;

	hak = hak_openstd(0, HAK_NULL);
	OK (hak != HAK_NULL, "instantiation");
	if (!hak) return;

	hak_getoption(hak, HAK_TRAIT, &trait);
	trait |= HAK_TRAIT_AWAIT_PROCS | HAK_TRAIT_LANG_ENABLE_EOL;
	hak_setoption(hak, HAK_TRAIT, &trait);

	memset (&cb, 0, sizeof(cb));
	cb.vm_checkbc = cb_checkbc;
	OK (hak_regcb(hak, &cb) != HAK_NULL, "callback registration");

	OK (hak_ignite(hak, 0) == 0, "ignition");
	OK (hak_addbuiltinprims(hak) == 0, "builtin primitives");
	OK (hak_attachcciostdwithbcstr(hak, HAK_NULL) == 0, "source input stream");
	OK (hak_attachudiostdwithbcstr(hak, "", "") == 0, "user data streams");
	OK (hak_beginfeed(hak, on_cnode) == 0, "begin feed");
	OK (hak_feedbchars(hak, SRC, strlen(SRC)) == 0, "feed the script");
	OK (hak_endfeed(hak) == 0, "end feed");

	hak_rcvtick(hak, 1); /* without this every tick is ignored */

	retv = hak_execute(hak);
	if (!retv) printf("# execute failed: [%d] %s\n", (int)hak_geterrnum(hak), hak_geterrbmsg(hak));
	OK (retv != HAK_NULL, "execution");
	if (retv && HAK_OOP_IS_SMOOI(retv)) iters = HAK_OOP_TO_SMOOI(retv);

	printf("# raised=%d iterations=%ld (bound %d)\n", raised, (long)iters, SPIN_BOUND);

	OK (raised == 1, "the callback reached the raise");
	OK (iters >= 0 && iters < SPIN_BOUND,
	    "hak_raisetick preempts a process that never yields");
	OK (iters > 100, "the switch came from the tick, not from an eager fork");

	hak_close(hak);
}

int main (int argc, char* argv[])
{
	no_plan();
	state_contract();
	preempts_a_spinner();
	return exit_status();
}
