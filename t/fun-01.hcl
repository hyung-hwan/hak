fun aaa(a b) {
	| c |
	set c (+ a b);
	return c;
};

set k (aaa 10 20);

if (== k 30) {
	printf "OK - %d\n" k;
} else {
	printf "ERROR - %d\n" k;
};

## --------------------------------------

fun mkfun(t) {
	return (fun(c) {
		return (+ t c);
	});
}

fun mkfun2(t) {
	return {fun(c) {
		return (fun(d) {
			return (+ d c t)
		})
	}}
}

f := (mkfun 20);
set k (f 50);
if (== k 70) {
	printf "OK - %d\n" k;
} else {
	printf "ERROR - %d\n" k;
};

k := {
	(mkfun 20) 30
}
if (== k 50) {
	printf "OK - %d\n" k
} else {
	printf "ERROR - %d\n" k
};

k := {
	(mkfun 20) 30 ## the return value of this expression is ignored
	(mkfun 20) 40 ## the return value of this expression is the return value of the block expression
}
if (== k 60) {
	printf "OK - %d\n" k
} else {
	printf "ERROR - %d\n" k
};

k := (((mkfun2 10) 40) 30)
if (== k 80) {
	printf "OK - %d\n" k
} else {
	printf "ERROR - %d\n" k
};
## --------------------------------------

## multiple return values
fun f(a :: b c) { b := (+ a 10); c := (+ a 20) }
[x, y] := (f 9)
if (== x 19) {
	printf "OK - %d\n" x
} else {
	printf "ERROR - %d\n" x
}
if (== y 29) {
	printf "OK - %d\n" y
} else {
	printf "ERROR - %d\n" y
}

## --------------------------------------
k := (fun qq(t) (+ t 20))
x := (k 8)
y := (qq 9)

if (== x 28) {
	printf "OK - %d\n" x
} else {
	printf "ERROR - %d\n" x
}

if (== y 29) {
	printf "OK - %d\n" x
} else {
	printf "ERROR - %d\n" x
}

## --------------------------------------

defclass A [ a b c ] {
	fun :* newInstance(x y z) {
		set a x
		set b y
		set c z
		return self
	};

	fun get-a() { return a; };
	##fun get-b() b;
	##fun get-c() c;
};

k := (A:newInstance 11 22 33);
##set k (A:newInstance 11 22 33);

set v (k:get-a);
if (== v 11) {
	printf "OK - %d\n" v;
} else {
	printf "ERROR - %d\n" v;
};
