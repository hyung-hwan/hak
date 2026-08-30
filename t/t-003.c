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
 * What this does NOT cover is the scheduler acting on a pending tick.
 * t/tick-01.hak covers that for the global half. */

#include <hak.h>
#include "tap.h"

int main (int argc, char* argv[])
{
	hak_t* hak;

	no_plan();

	hak = hak_openstd(0, HAK_NULL);
	OK (hak != HAK_NULL, "instantiation");
	if (!hak) return exit_status();

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
	return exit_status();
}
