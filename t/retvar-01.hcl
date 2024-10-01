
(fun() {
	## test return variables

	| v1 v2 v3 i a b c d |

	set i 100;

	fun ff(a b :: x y z) {
		set x (+ a b i);
		set y (+ x x);
		set z (+ 999 i);
		set i (* i 10);
	};

	set-r v1 v2 v3 (ff 10 20);
	if (~= v1 130) { printf "ERROR: v1 must be 130\n" } \
	else { printf "OK: v1=%d\n" v1 }
	if (~= v2 260) { printf "ERROR: v2 must be 260\n" } \
	else { printf "OK: v2=%d\n" v2 }
	if (~= v3 1099) { printf "ERROR: v3 must be 1099\n" } \
	else { printf "OK: v3=%d\n" v3 }

	set-r v1 v2 (ff 1 2); ## using 2 return variables only. not assigning to v3
	if (~= v1 1003) { printf "ERROR: v1 must be 1003\n" } \
	else { printf "OK: v1=%d\n" v1 }
	if (~= v2 2006) { printf "ERROR: v2 must be 2006\n" } \
	else { printf "OK: v2=%d\n" v2 }
	if (~= v3 1099) { printf "ERROR: v3 must be 1099\n" } \
	else { printf "OK: v3=%d\n" v3 }


	## test return variables in message sends
	defclass B [ [X1 X2] ] {

		set X1 999;
		set X2 888;

		fun(#class) get ( :: x y)  {
			set x X1;
			set y X2;
		};

		fun(#class) get2 (inc :: x y)  {
			set x (+ X1 inc);
			set y (+ X2 inc);
		};
	};

	set-r a b (B:get);
	set-r c d (B:get2 -100);

	if (~= a 999) { printf "ERROR: a must be 999\n" } \
	else { printf "OK: a=%d\n" a }
	if (~= b 888) { printf "ERROR: b must be 888\n" } \
	else { printf "OK: b=%d\n" b }
	if (~= c 899) { printf "ERROR: c must be 899\n" } \
	else { printf "OK: c=%d\n" c }
	if (~= d 788) { printf "ERROR: d must be 788\n" } \
	else { printf "OK: d=%d\n" d }

	class X [ x, y ] {
		fun(#class) f(a :: b c) { b := (+ a 10); c := (+ a 20) }

		fun(#classinst) new(z) {
			## multi-variable assignment with return variables to member variables
			[self.x, self.y] := (X:f z)
			return self;
		}

		fun getX() { return self.x }
		fun getY() { return self.y }
	}

	z := (X:new 9)
	if (~= (x := (z:getX)) 19) { printf "ERROR: z:getX must return 19\n" } \
	else { printf "OK: z:getX=%d\n" x }

	if (~= (y := (z:getY)) 29) { printf "ERROR: z:getX must return 29\n" } \
	else { printf "OK: z:getY=%d\n" y }
});



## create a new binary operator message returning two output values
fun Number:// (x :: quo rem) {
        quo := (/ self x)
        rem := (- self (* quo x))
}

[q,r] := (123 // 4)
if (~= q 30) { printf "ERROR: q is not 30" } \
else { printf "OK: q is %d\n" q }
if (~= r 3) { printf "ERROR: r is not 3" } \
else { printf "OK: r is %d\n" r }
