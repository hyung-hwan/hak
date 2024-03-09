defun aaa(a b) {
	| c |
	set c (+ a b);
	return c;
};

set k (aaa 10 20);

if (= k 30) {
	printf "OK - %d\n" k;
} else {
	printf "ERROR - %d\n" k;
};

## --------------------------------------

defun mkfun(t) {
	return (fun(c) {
		return (+ t c);
	});
};

f := (mkfun 20);
set k (f 50);
if (k = 70) {
	printf "OK - %d\n" k;
} else {
	printf "ERROR - %d\n" k;
};

k := {
	(mkfun 20) 30
}
if (k = 50) {
	printf "OK - %d\n" k
} else {
	printf "ERROR - %d\n" k
};

k := {
	(mkfun 20) 30
	(mkfun 20) 40
}
if (k = 60) {
	printf "OK - %d\n" k
} else {
	printf "ERROR - %d\n" k
};

## --------------------------------------

defclass A | a b c | {
	defun :* newInstance(x y z) {
		set a x
		set b y
		set c z
		return self
	};

	defun get-a() { return a; };
	##defun get-b() b;
	##defun get-c() c;
};

k := (A:newInstance 11 22 33);
##set k (A:newInstance 11 22 33);

set v (k:get-a);
if (= v 11) {
	printf "OK - %d\n" v;
} else {
	printf "ERROR - %d\n" v;
};
