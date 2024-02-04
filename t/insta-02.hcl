set t (
	class | x | {
		defun :* make() { x := 1234; return self; };
		defun get-x() { return x };
	}
);

set X t;

if (nqv? t X) { printf "ERROR: t must point to X\n" } \
else { printf "OK: t points to X\n" };

set t ((t:make):get-x);

if (nqv? t 1234) { printf "ERROR: t must be 1234\n" } \
else { printf "OK: t is %d\n" t };


j := #{ ((X:make):get-x): 9999, 4512: ((X: make): get-x) };
v := (dic.get j 1234);
if (nqv? v 9999) { printf "ERROR: v is not 9999\n" } \
else {  printf "OK:  value is %d\n" v };

v := (dic.get j 4512);
if (nqv? v 1234) { printf "ERROR: v is not 1234\n" } \
else { printf "OK:  value is %d\n" v };
