/* process stack overflow must be reported, not swallowed.
 *
 * HAK_STACK_PUSH() detects the overflow, sets HAK_ESTKOVRFLW and stores -1 in
 * hak->abort_req; the interpreter loop then takes "goto oops" and answers a
 * failure. That path is only reachable while abort_req is a signed type - when
 * it was hak_uint8_t the -1 became 255, the "abort_req < 0" test could never
 * be true, and the overflow fell through to the ordinary "abort_req > 0" break
 * instead. hak_execute() then answered success and the script simply stopped
 * running with nothing reported.
 *
 * This is checked here rather than in t/ as a script because neither script
 * harness can express it: run.sh fails a test as soon as the output contains
 * an ERROR: line, and err.sh insists the error be reported at the line its
 * ##ERROR: marker sits on, whereas a stack overflow carries no source
 * location and is reported at [0,0]. Only a C test can assert on the value
 * hak_execute() answers, which is the part that regressed.
 *
 * The nesting depth is what drives the operand stack: evaluating
 * (+ 1 (+ 1 ... 0)) has to hold every pending left operand at once. The
 * process stack size is set explicitly here because there is no single
 * default to rely on - HAK_DFL_PROCSTK_SIZE is 5000, bin/hak asks for 600,
 * and exec.c clamps whatever it is given up to a floor of 192. */

#include <hak.h>
#include "tap.h"
#include <string.h>
#include <stdlib.h>

/* slots per nesting level are an implementation detail, so keep a wide margin
 * on both sides of the limit rather than probing for the exact threshold */
#define STK_SLOTS   192
#define DEEP_DEPTH  2000
#define SHALLOW_DEPTH 10

static int on_cnode (hak_t* hak, hak_cnode_t* obj)
{
	return hak_compile(hak, obj, 0);
}

/* build "x := (+ 1 (+ 1 ... 0))" nested to the given depth */
static char* make_src (int depth)
{
	hak_oow_t capa;
	char* buf;
	char* p;
	int i;

	capa = (hak_oow_t)depth * 8 + 64;
	buf = (char*)malloc(capa);
	if (!buf) return HAK_NULL;

	p = buf;
	memcpy(p, "x := ", 5); p += 5;
	for (i = 0; i < depth; i++) { memcpy(p, "(+ 1 ", 5); p += 5; }
	*p++ = '0';
	for (i = 0; i < depth; i++) *p++ = ')';
	*p++ = '\n';
	*p = '\0';

	return buf;
}

/* returns 0 if the run completed, -1 if the fixture itself could not be built */
static int run_at_depth (int depth, int expect_overflow)
{
	hak_t* hak;
	hak_bitmask_t trait;
	hak_oow_t stksize;
	hak_oop_t retv;
	char* src;
	int errnum;
	int rc = -1;

	src = make_src(depth);
	OK (src != HAK_NULL, "source built");
	if (!src) return -1;

	hak = hak_openstd(0, HAK_NULL);
	OK (hak != HAK_NULL, "instantiation");
	if (!hak) goto done;

	hak_getoption(hak, HAK_TRAIT, &trait);
	trait |= HAK_TRAIT_LANG_ENABLE_EOL;
	hak_setoption(hak, HAK_TRAIT, &trait);

	stksize = STK_SLOTS;
	OK (hak_setoption(hak, HAK_PROCSTK_SIZE, &stksize) == 0, "process stack size");

	OK (hak_ignite(hak, 0) == 0, "ignition");
	OK (hak_addbuiltinprims(hak) == 0, "builtin primitives");
	OK (hak_attachcciostdwithbcstr(hak, HAK_NULL) == 0, "source input stream");
	OK (hak_attachudiostdwithbcstr(hak, "", "") == 0, "user data streams");
	OK (hak_beginfeed(hak, on_cnode) == 0, "begin feed");
	OK (hak_feedbchars(hak, src, strlen(src)) == 0, "feed the script");
	OK (hak_endfeed(hak) == 0, "end feed");

	retv = hak_execute(hak);
	errnum = (int)hak_geterrnum(hak);

	if (expect_overflow)
	{
		printf("# depth=%d retv=%s errnum=%d (%s)\n", depth,
			retv? "value": "HAK_NULL", errnum,
			retv? "-": hak_geterrbmsg(hak));
		/* the assertion that regressed: a swallowed overflow answers a value */
		OK (retv == HAK_NULL, "an overflowing script fails rather than answering a value");
		OK (errnum == HAK_ESTKOVRFLW, "the failure is reported as HAK_ESTKOVRFLW");
	}
	else
	{
		if (!retv) printf("# depth=%d unexpected failure: [%d] %s\n", depth, errnum, hak_geterrbmsg(hak));
		OK (retv != HAK_NULL, "a script within the stack limit still runs");
	}

	rc = 0;
	hak_close(hak);

done:
	free(src);
	return rc;
}

int main (int argc, char* argv[])
{
	no_plan();
	/* the control comes first: if this one failed the deep case would pass
	 * for the wrong reason, since any failure at all answers HAK_NULL */
	run_at_depth(SHALLOW_DEPTH, 0);
	run_at_depth(DEEP_DEPTH, 1);
	return exit_status();
}
