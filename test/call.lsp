(fn test1 () (+ 123 321))

(fn test2 () (if #t ~123 321))

(+ (test1) (test2))
