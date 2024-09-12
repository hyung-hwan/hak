z1 := 0
z2 := 0

defun loop1() {
	| k |

	k := 1
	while (< k 100) {
		printf "loop1 => %d\n" k
		k := (+ k 2)
		yield
	}

	z1 := k
	sem-signal s1
}

defun loop2() {
	| k |

	k := 0
	while (< k 100) {
		printf "loop2 => %d\n" k
		k := (+ k 2)
		yield
	}

	z2 := k
	sem-signal s2
}

s1 := (sem-new)
s2 := (sem-new)

p1 := (fork loop1)
p2 := (fork loop2)

##suspend p1
##suspend p2
##resume p1
##resume p2

sem-wait s1
sem-wait s2

if (== z1 101) { printf "OK: z1 is %d\n" z1 } \
else { printf "ERROR: z1 is not 101 - %d\n" z1 }
if (== z2 100) { printf "OK: z2 is %d\n" z2 } \
else { printf "ERROR: z1 is not 100 - %d\n" z2 }

