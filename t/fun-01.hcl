defun aaa(a b) {
	| c |
	set c (+ a b);
	return c;
};

set k (aaa 10 20);

if (= k 30) { printf "OK\n"; }
else { printf "ERROR\n"; };

## --------------------------------------

defun mkfun(t) {
	return (fun(c) {
		return (+ t c);
	});
};

set f (mkfun 20);
set k (f 50);
if (= k 50) { printf "OK\n"; }
else { printf "ERROR\n"; };
