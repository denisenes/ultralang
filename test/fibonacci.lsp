(fn fibonacci (n) (
	cond ((= n 0) 1)
	     ((= n 1) 1)
	     (#t (+ (fibonacci (- n 1)) (fibonacci (- n 2))))
	)
)

(fibonacci 15)
