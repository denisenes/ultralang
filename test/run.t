  $ export INTERPRETER="./main.exe test"

Sanity check
  $ echo "Testing started"
  Testing started

=================================================
INTERPRETER TESTS
=================================================

Integers
  $ ${INTERPRETER} "1;;"
  1
  $ ${INTERPRETER} "10;;"
  10
  $ ${INTERPRETER} "987654321;;"
  987654321

Negative integers
  $ ${INTERPRETER} "-123;;"
  -123
  $ ${INTERPRETER} "-987654321;;"
  -987654321

Boolean literals
  $ ${INTERPRETER} "#True;;"
  #t
  $ ${INTERPRETER} "#False;;"
  #f

Arithmetic ops
  $ ${INTERPRETER} "64 + 32;;"
  96
  $ ${INTERPRETER} "64 - 32;;"
  32
  $ ${INTERPRETER} "123 + (-321);;"
  -198
  $ ${INTERPRETER} "(-0) + 0;;"
  0
  $ ${INTERPRETER} "(2 * 3) * (100 / (4 / 2));;"
  300
  $ ${INTERPRETER} "(((1000 + 2000) - 1976) * 4) / 512;;"
  8

Logical ops
  $ ${INTERPRETER} "#False && #False;;"
  #f
  $ ${INTERPRETER} "#False || #False;;"
  #f
  $ ${INTERPRETER} "#True && #False;;"
  #f
  $ ${INTERPRETER} "#True || #False;;"
  #t
  $ ${INTERPRETER} "#True && #True;;"
  #t
  $ ${INTERPRETER} "#True || #True;;"
  #t

If expression
  $ ${INTERPRETER} "if #True then 666 else 777;;"
  666
  $ ${INTERPRETER} "if #False then #True else #True;;"
  #t
  $ ${INTERPRETER} "if #True || #False then #False && #True else 123 + 321;;"
  #f

Cons
  $ ${INTERPRETER} "123 : (321 : []);;"
  [123, 321]
  $ ${INTERPRETER} "(100 * 10) : ((100 / 10) : []);;"
  [1000, 10]

Call
  $ ${INTERPRETER} "$(cat call3.ul)"
  500

Val
  $ ${INTERPRETER} "val a = 300;;"
  300
  $ ${INTERPRETER} "val a = 321 - 21;;"
  300

List
  $ ${INTERPRETER} "[1, 2,3 , 4,  5];;"
  [1, 2, 3, 4, 5]
  $ ${INTERPRETER} "[1, #False, 2, #True];;"
  [1, #f, 2, #t]

Apply
  TODO $ ${INTERPRETER} "apply([[:]], #False, #True);;"
  TODO [#f, #t]
  $ ${INTERPRETER} "apply((lam x y = x + y), 23, 34);;"
  57

Lambda
  $ ${INTERPRETER} "val inc = (lambda x = x + 1);; inc(1);;"
  2

Define
  $ ${INTERPRETER} "fn inc x = x + 1;; inc(10);;"
  11

Let
  $ ${INTERPRETER} "let x = 123 in x + 123;;"
  246
  $ ${INTERPRETER} "let x = 10 * 10 in let y = 1000 / 2 in x + y;;"
  600

Factorial
  $ ${INTERPRETER} "$(cat fact.ul)"
  1307674368000

Fibonacci
  $ ${INTERPRETER} "$(cat fibonacci.ul)"
  987

Map
  $ ${INTERPRETER} "$(cat map.ul)"
  55
