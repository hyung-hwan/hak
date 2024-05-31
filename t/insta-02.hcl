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

## --------------------------------------------------------------

class X | a b c d | {
        fun :*new() {
                return self;
        }

        fun x() {
		a := 20 ; self.b:=(a + 10); c := (b + 20)
		printf "%d %d %d\n" self.a self.b self.c
		return (+ self.a self.b self.c)
        }
	fun y() {
		self.d := (fun(k) {
			return (k + 1)
		})
		return self.d
	}

}; a := (X:new); v := (a:x)
if (nqv? v 100) { printf "ERROR: v is not 100\n" } \
else { printf "OK:  value is %d\n" v }

v := ((a:y) 20);
if (nqv? v 21) { printf "ERROR: v is not 21\n" } \
else { printf "OK:  value is %d\n" v }

## --------------------------------------------------------------

class F | j t | {
}

class X | a  b c | {
	fun :* new () {
		self.a := 20
		return self
	}

	fun getA() { return self.a }

	fun make(t a b) {
		| v |
		v := 50
		if (t > 5) {
			fun X:get_j() { return (((1 + t) + a) + b) }
		} else {
			fun X:get_j() { return ((2 * (t + a)) + b) }
		}
		return self
	}
}

fun X:get_a() {
	return (self:getA)
}

v := ((X:new):get_a)
if (nqv? v 20) { printf "ERROR: v is not 20 - %d\n" v } \
else { printf "OK:  value is %d\n" v }

v := (((X:new):make 5 6 7):get_j)
if (nqv? v 29) { printf "ERROR: v is not 29 - %d\n" v } \
else { printf "OK:  value is %d\n" v }

v := (((X:new):make 6 6 7):get_j)
if (nqv? v 20) { printf "ERROR: v is not 20 - %d\n" v } \
else { printf "OK:  value is %d\n" v }
