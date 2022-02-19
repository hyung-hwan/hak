((lambda ()
	; test return variables

	| v1 v2 v3 i |

	(set i 100)

	(defun ff(a b ::: x y z)
		(set x (+ a b i))
		(set y (+ x x))
		(set z (+ 999 i))
		(set i (* i 10))
	)

	(set-r v1 v2 v3 (ff 10 20))
	(if (/= v1 130) (printf "ERROR: v1 must be 130\n"))
	(if (/= v2 260) (printf "ERROR: v2 must be 260\n"))
	(if (/= v3 1099) (printf "ERROR: v3 must be 1099\n"))
	(printf "OK v1=%d v2=%d v3=%d\n" v1 v2 v3)

	(set-r v1 v2 (ff 1 2)) ; using 2 return variables only. not assigning to v3
	(if (/= v1 1003) (printf "ERROR: v1 must be 1003\n"))
	(if (/= v2 2006) (printf "ERROR: v2 must be 2006\n"))
	(if (/= v3 1099) (printf "ERROR: v3 must be 1099\n"))
	(printf "OK v1=%d v2=%d v3=%d\n" v1 v2 v3)
))
