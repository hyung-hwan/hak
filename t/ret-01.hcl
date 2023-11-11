(defun repeat(n f)
	(while (> n 0)
		(f)
		(set n (- n 1))
	)
)

(defun test-non-local-ret-1(k)
	(repeat 10 (lambda()
		(set k (+ k 2))
		(if (= k 28) (return-from-home k))
	))	

	(return k)
)

(set a (test-non-local-ret-1 20))
(if (/= a 28) (printf "ERROR: a must be 28\n"))
(printf "OK %d\n" a)

(set a (test-non-local-ret-1 21))
(if (/= a 41) (printf "ERROR: a must be 41\n"))
(printf "OK %d\n" a)


(defun ff() (return 999))

## test a normal block return
(set a (ff))
(if (/= a 999) (printf "ERROR: a must be 999\n"))
(printf "OK %d\n" a)

## return from top-level
(return 10)
(printf "ERROR: this line must not be printed\n")
