
(defun ff() (return 999))

; test a normal block return
(set a (ff))
(if (/= a 999) (printf "ERROR: a must be 999\n"))
(printf "OK %d\n" a)

; return from top-level
(return 10)
(printf "ERROR: this line must not be printed\n")
