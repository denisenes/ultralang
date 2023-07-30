(fn dec (x) (- x 1))

(fn factorial (num)
	(if (= num 0)
    		1
		(* num (factorial (dec num)))
	)
)
