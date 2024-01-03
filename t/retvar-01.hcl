
((fun() {
	## test return variables

	| v1 v2 v3 i a b c d |

	set i 100;

	defun ff(a b ::: x y z) {
		set x (+ a b i);
		set y (+ x x);
		set z (+ 999 i);
		set i (* i 10);
	};

	(set-r v1 v2 v3 (ff 10 20))
	(if (/= v1 130) (printf "ERROR: v1 must be 130\n"))
	(if (/= v2 260) (printf "ERROR: v2 must be 260\n"))
	(if (/= v3 1099) (printf "ERROR: v3 must be 1099\n"))
	(printf "OK v1=%d v2=%d v3=%d\n" v1 v2 v3)

	(set-r v1 v2 (ff 1 2)) ## using 2 return variables only. not assigning to v3
	(if (/= v1 1003) (printf "ERROR: v1 must be 1003\n"))
	(if (/= v2 2006) (printf "ERROR: v2 must be 2006\n"))
	(if (/= v3 1099) (printf "ERROR: v3 must be 1099\n"))
	(printf "OK v1=%d v2=%d v3=%d\n" v1 v2 v3)




	## test return variables in message sends
	defclass B ::: | X1 X2 | {

		set X1 999;
		set X2 888;

		defun ::: get ( ::: x y)  {
			(set x X1) 
			(set y X2)
		};

		defun ::: get2 (inc ::: x y)  {
			(set x (+ X1 inc)) 
			(set y (+ X2 inc))
		};
	};

	(set-r a b (B:get))
	(set-r c d (B:get2 -100))

	(if (/= a 999) (printf "ERROR: a must be 999\n"))
	(if (/= b 888) (printf "ERROR: b must be 888\n"))
	(if (/= c 899) (printf "ERROR: c must be 899\n"))
	(if (/= d 788) (printf "ERROR: d must be 788\n"))

	printf "OK a=%d b=%d c=%d d=%d\n" a b c d;
}));
