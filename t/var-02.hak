fun x (a :: x y z) {
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

if (~= a 900) { printf "ERROR: a is not 900\n" } \
else { printf "OK: %d\n" a };

if (~= b 60) { printf "ERROR: b is not 60\n" } \
else { printf "OK: %d\n" b };

if (~= c 840) { printf "ERROR: c is not 840\n" } \
else { printf "OK: %d\n" c };

[aa,bb,cc] := ((xx := x) 10)

if (~= aa 100) { printf "ERROR: aa is not 100\n" } \
else { printf "OK: %d\n" aa };

if (~= bb 20) { printf "ERROR: bb is not 20\n" } \
else { printf "OK: %d\n" bb };

if (~= cc 80) { printf "ERROR: cc is not 80\n" } \
else { printf "OK: %d\n" cc };

if (nqv? xx x) { printf "ERROR: xx is not equal to x\n"} \
else { printf "OK: xx and x are equal\n" }
