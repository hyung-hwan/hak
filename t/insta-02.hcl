(set t 
	(defclass X
		| x | 
		(defun ::* make() (set x 1234) self)
		(defun get-x() x)
	)
)
(if (nqv? t X) (printf "ERROR: t must point to X\n"))
(printf "OK: t points to X\n")

(set t (:(:t make) get-x))
(if (nqv? t 1234) (printf "ERROR: t must be 1234\n"))
(printf "OK: t is %d\n" t)
