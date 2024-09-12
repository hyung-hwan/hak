fun repeat(n f) {
	while (> n 0) {
		f;
		set n (- n 1);
	};
};

fun test-non-local-ret-1(k) {
	repeat 10 (fun() {
		set k (+ k 2);
		if (= k 28) { revert k };
	});

	return k;
};

set a (test-non-local-ret-1 20);
if (~= a 28) { printf "ERROR: a must be 28\n" } \
else { printf "OK %d\n" a };

set a (test-non-local-ret-1 21);
if (~= a 41) { printf "ERROR: a must be 41\n" } \
else { printf "OK %d\n" a };

fun dd() { return (- 99999999999999991111111111111111111111111111111111111111.111111111 999999999999999999999999999999.999999999999) }

fun ee() { return (+ 1111111111111111111111111111111111111111111111111 999999999999999999999999999999999999999999) }

fun ff() { return 999 };

## test a normal block return with a fixed point decimal
set a (dd);
if (~= a 99999999999999991111111110111111111111111111111111111111.111111111001) { printf "ERROR: a must be 99999999999999991111111110111111111111111111111111111111.111111111001\n" } \
else  { printf "OK %f\n" a };

## test a normal block return with a large integer
set a (ee);
if (~= a 1111112111111111111111111111111111111111111111110) { printf "ERROR: a must be 1111112111111111111111111111111111111111111111110\n" } \
else  { printf "OK %d\n" a };

## test a normal block return
set a (ff);
if (~= a 999) { printf "ERROR: a must be 999\n" } \
else  { printf "OK %d\n" a };

## return from top-level
return 10;
printf "ERROR: this line must not be printed\n";
