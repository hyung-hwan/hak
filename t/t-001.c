#include <hcl.h>
#include "tap.h"

int main(int argc, char* argv[])
{
	hcl_t* hcl;
	hcl_oop_t v;
	hcl_liw_t liw;
	hcl_ooi_t i, j;
	int n;

	no_plan();

	hcl = hcl_openstd(0, HCL_NULL);
	OK (hcl != HCL_NULL, "instantiation failure");

	n = hcl_ignite(hcl, 0);
	OK (n == 0, "ignition failure");

	n = hcl_addbuiltinprims(hcl);
	OK (n == 0, "registration failure of builtin primitives");

	for (j = HCL_TYPE_MIN(hcl_ooi_t); j <= HCL_TYPE_MIN(hcl_ooi_t) + 5 ; j++)
	{
		v = hcl_ooitoint(hcl, j);
		OK (v != HCL_NULL, "bigint instantiation");
		n = hcl_inttoooi(hcl, v, &i);
		OK (n < 0, "bigint to ooi conversion not negative");
		OK (i == j, "big to ooi conversion result");
	}

	hcl_close(hcl);
	return exit_status();
}

