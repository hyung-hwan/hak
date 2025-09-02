#include <hak.h>
#include "tap.h"

int main(int argc, char* argv[])
{
	hak_t* hak;
	hak_oop_t v;
	hak_liw_t liw;
	hak_ooi_t i, j;
	int n;

	no_plan();

	hak = hak_openstd(0, HAK_NULL);
	OK (hak != HAK_NULL, "instantiation failure");

	n = hak_ignite(hak, 0);
	OK (n == 0, "ignition failure");

	n = hak_addbuiltinprims(hak);
	OK (n == 0, "registration failure of builtin primitives");

	for (j = HAK_TYPE_MIN(hak_ooi_t); j <= HAK_TYPE_MIN(hak_ooi_t) + 5 ; j++)
	{
		v = hak_ooitoint(hak, j);
		OK (v != HAK_NULL, "bigint instantiation");
		n = hak_inttoooi(hak, v, &i);
		OK (n < 0, "bigint to ooi conversion not negative");
		OK (i == j, "big to ooi conversion result");
	}

	hak_close(hak);
	return exit_status();
}

