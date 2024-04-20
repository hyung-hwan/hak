defun x (a b :: r) {
	
	| x y |

	set x a
	set y b

	if (> a b) {
		| a b |
		set a (mod x y)
		set b (+ x y)
		set r (* a b)
	} else {
		| a b |
		set a (* x y)
		set b (- x y)
		set r (* a b)
	}


	if (/= x a) (printf "ERROR: x is not equal to a\n")
	if (/= y b) (printf "ERROR: y is not equal to b\n")
}

t := (x 10 20)
if (/= t -2000) (printf "ERROR: t is not equal to -2000\n") \
else (printf "OK: %d\n" t)
set t (x 30 20)
if (/= t 500) (printf "ERROR: t is not equal to 500\n") \
else (printf "OK: %d\n" t)


defun x () {

	| x y |

	set x 99
	try {
		| x |
		set x 88
		if (/= x 88) (printf "ERROR: x is not 88\n") \
	  	else (printf "OK: %d\n" x)
		throw 1000
	} catch (x) {
		if (/= x 1000) (printf "ERROR: x is not 1000\n") \
	 	else (printf "OK: %d\n" x)
		set y x
	}

	if (/= x 99) (printf "ERROR: x is not 99\n") \
	else (printf "OK: %d\n" x)
	if (/= y 1000) (print "ERROR: y is not 1000\n") \
	else (printf "OK: %d\n" y)
} 

x


class T | j | {

	defun :* new() {
		set j 99
		return self
	}

	defun x() {
		set R {
			| x |
			set x 1
			while (< x j) {
				defun Q() x
				set x (+ x 1)
			}
		}
	}
}

set t (T:new)
t:x
set t (Q)
if (/= t 99) (print "ERROR: t is not 99\n") \
else (printf "OK: %d\n" t)

if (nqv? R false) (print "ERROR: R is not false\n") \
else (printf "OK: %O\n" R)

set v #(
	(do |a b| (set a 10) (set b 20) (+ a b) )
 	(do |a b| (set a 11) (set b 21) (+ a b) )
   	999
)

set v2 #(30 32 999)

if (nql? v v2) (print "ERROR: v1 and v2 are not equal\n")\
else (printf "OK: v and v2 equal\n")

