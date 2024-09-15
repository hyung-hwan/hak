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
if { q := 10; < k q } {  ## a block expression is a normal expression. so it can be used as a conditional expression for if
	printf "OK: k is less than q\n"
} else (printf "BAD: k is not less than q\n")


fun ByteArray:at(pos) {
	return (core.basicAt self pos)
}

fun CharacterArray:at(pos) {
	return (core.basicAt self pos)
}

fun SmallInteger:toCharacter() {
	return (core.smooiToChar self)
}

fun Character:toCode() {
	return (core.charToSmooi self)
}

ba := #B[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]
i := 0
while (< i 15) {
	x := (+ i 1)
	if (== (ba:at i) x) { printf "OK: (ba:at %d) is %O\n" i (ba:at i) } \
	else { printf "ERROR: (ba:at %d) is not %d\n" i x }
	i := x
}

ca := #C[
	'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
	'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
	'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
]

i := 0
while (< i 26) {
	x := ((+ i ('a':toCode)):toCharacter)
	if (eqv? (ca:at i) x) { printf "OK: (ca:at %d) is %O\n" i x } \
	else { printf "ERROR: (ca:at %d) is not %O\n" i x}
	i := (+ i 1)
}

i := 26
while (< i 52) {
	x := ((+ (- i 26) ('A':toCode)):toCharacter)
	if (eqv? (ca:at i) x) { printf "OK: (ca:at %d) is %O\n" i x } \
	else { printf "ERROR: (ca:at %d) is not %O\n" i x}
	i := (+ i 1)
}

}  ## END
