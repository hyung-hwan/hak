defun x (a :: x y z) {
	x := (* a a);
	y := (+ a a);
	z := (- x y);
};

j := 21;

if (eqv? j 20) {
	[a,b,c] := (x 20);
	q := (x 20);
} else {
	[a,b,c] := (x 30);
	q := (x 30);
};

if (/= a 900) { print "ERROR: a is not 900\n" } \
else { printf "OK: %d\n" a };

if (/= b 60) { print "ERROR: b is not 60\n" } \
else { printf "OK: %d\n" b };

if (/= c 840) { print "ERROR: c is not 840\n" } \
else { printf "OK: %d\n" c };
