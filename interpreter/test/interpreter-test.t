  $ export INTERPRETER="dune exec main test"

Sanity check
  $ echo "Testing started"
  Testing started

Integers
  $ ${INTERPRETER} "1"
  1
  $ ${INTERPRETER} "10"
  10
  $ ${INTERPRETER} "987654321"
  987654321

Negative integers
  $ ${INTERPRETER} "~123"
  -123
  $ ${INTERPRETER} "~987654321"
  -987654321

Boolean literals
  $ ${INTERPRETER} "#t"
  #t
  $ ${INTERPRETER} "#f"
  #f

Arithmetic ops
  $ ${INTERPRETER} "(+ 64 32)"
  96
  $ ${INTERPRETER} "(- 64 32)"
  32
  $ ${INTERPRETER} "(+ 123 ~321)"
  -198
  $ ${INTERPRETER} "(- ~0 0)"
  0
  $ ${INTERPRETER} "(* (* 2 3) (/ 100 (/ 4 2)))"
  300
  $ ${INTERPRETER} "(/ (* (- (+ 1000 2000) 1976) 4) 512)"
  8

Logical ops
  $ ${INTERPRETER} "(and #f #f)"
  #f
  $ ${INTERPRETER} "(or  #f #f)"
  #f
  $ ${INTERPRETER} "(and #t #f)"
  #f
  $ ${INTERPRETER} "(or  #t #f)"
  #t
  $ ${INTERPRETER} "(and #t #t)"
  #t
  $ ${INTERPRETER} "(or  #t #t)"
  #t

If expression
  $ ${INTERPRETER} "(if #t 666 777)"
  666
  $ ${INTERPRETER} "(if #f #f #t)"
  #t
  $ ${INTERPRETER} "(if (or #t #f) (and #f #t) (+ 123 321))"
  #f

Pair
  $ ${INTERPRETER} "(pair 123 321)"
  (123 . 321)
  $ ${INTERPRETER} "(pair (* 100 10) (/ 100 10))"
  (1000 . 10)

Val
  $ ${INTERPRETER} "(val a 300)"
  300
  $ ${INTERPRETER} "(val a (- 321 21))"
  300

List
  $ ${INTERPRETER} "(list 1 2 3 4 5)"
  (1 2 3 4 5)
  $ ${INTERPRETER} "(list 1 #f 2 #t)"
  (1 #f 2 #t)

Apply
  $ ${INTERPRETER} "(apply pair (list #f #t))"
  (#f . #t)
  $ ${INTERPRETER} "(apply + (list 23 34))"
  57

Quote
  $ ${INTERPRETER} "'4"
  4
  $ ${INTERPRETER} "'(100 200 300 #t)"
  (100 200 300 #t)
  $ ${INTERPRETER} "'(this is test of quotation)"
  (this is test of quotation)
  $ ${INTERPRETER} "'(if #t (+ 1 2) (- 2 3))"
  (if #t (+ 1 2) (- 2 3))
  $ ${INTERPRETER} "(quote (some other test of quotation))"
  (some other test of quotation)
  $ ${INTERPRETER} "(apply + '(123 321))"
  444
  $ ${INTERPRETER} "(val x 'x)"
  x
