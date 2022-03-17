; test class instantiation methods

(defclass A
	| a b c |

	(defun ::* newInstance(x y z)
		(set a x)
		(set b y)
		(set c z)
		(return self)
	)

	(defun get-a() a)
	(defun get-b() b)
	(defun get-c() c)
)	

(defclass B ::: A
	| d e f |

	(defun ::* newInstance(x y z)
		(:super newInstance (* x 2) (* y 2) (* z 2))
		(set d x)	
		(set e y)	
		(set f z)	
		(return self)
	)

	(defun sum()
		(+ (:super get-a) (:super get-b) (:super get-c) d e f)
	)

)

(set a (:(:B newInstance 1 2 3) sum))
(if (/= a 18) (printf "ERROR: a must be 18\n"))
(printf "OK %d\n" a)

(set b (:B newInstance 2 3 4))
(set a (:b get-a))
(if (/= a 4) (printf "ERROR: a must be 4\n"))
(printf "OK %d\n" a)
(set a (:b get-b))
(if (/= a 6) (printf "ERROR: a must be 6\n"))
(printf "OK %d\n" a)
(set a (:b get-c))
(if (/= a 8) (printf "ERROR: a must be 8\n"))
(printf "OK %d\n" a)
(set a (:b sum))
(if (/= a 27) (printf "ERROR: a must be 27\n"))
(printf "OK %d\n" a)


