defun aaa(a b) {
	| c |
	set c (+ a b);
	return c;
};

set k (aaa 10 20);

if (= k 30) {
	printf "OK\n";
}
else {
	printf "ERROR\n";
};



