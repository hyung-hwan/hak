## this file is to test the reader/feeder against weirdly formatted input text.

{  ## START

| J |

defun xxx (x y z
	:: r ) {

	| k
	b
	s |

	k := (+ x y z)
	b := (* k k)
	s := (* b b)

	printf "%d %d %d\n" k b s

	r := s
	J := r
}


[
j
] \
	:= (xxx
	10
	20
	30)

if (eqv? j 12960000) \
{
	printf "OK: j is 12960000\n"
} else {
	printf "BAD: j is not 12960000\n"
}

xxx \
	1 \
	2 \
	3

if (eqv? J 1296) {
	printf "OK: J is 1296\n"
} else {
	printf "BAD: J is not 1296\n"
}


k := 5
if { q := 10; k < q } {  ## a block expression is a normal expression. so t can be used as a conditional expression for if
	printf "OK: k is less than q\n"
} else (printf "BAD: k is not less than q\n")

}  ## END
