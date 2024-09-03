## there are 8 local temporaries variables in the top-level context
## the 8 temporaries defined inside the block expression are parked
## at the top-level context. 'a' assigned before the block expression
## is a global variable.

a := 1234

{
	| a b c d |
	| e f g h |
	a := 10
	b := 20
	c := 30
	d := 40
	e := 50
	f := 60
	g := 70
	h := 80
		
	if (~= a 10) { print "ERROR: a inside the block expression is not 10\n" } \
	else { printf "OK: %d\n" a };
}

a := (+ a 1)
c := (sprintf "%d" a)

if (~= a 1235) { printf "ERROR: a is not 1235\n" } \
else { printf "OK: %d\n" a };

if (nql? c "1235") { printf "ERROR: c is not \"1235\"\n" } \
else { printf "OK: %s\n" c };
