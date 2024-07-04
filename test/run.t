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
  Int
  $ ${INTERPRETER} "10;;"
  10
  Int
  $ ${INTERPRETER} "987654321;;"
  987654321
  Int

Negative integers
  $ ${INTERPRETER} "-123;;"
  -123
  Int
  $ ${INTERPRETER} "-987654321;;"
  -987654321
  Int

Boolean literals
  $ ${INTERPRETER} "#True;;"
  #True
  Bool
  $ ${INTERPRETER} "#False;;"
  #False
  Bool

Arithmetic ops
  $ ${INTERPRETER} "64 + 32;;"
  96
  Int
  $ ${INTERPRETER} "64 - 32;;"
  32
  Int
  $ ${INTERPRETER} "123 + (-321);;"
  -198
  Int
  $ ${INTERPRETER} "(-0) + 0;;"
  0
  Int
  $ ${INTERPRETER} "(2 * 3) * (100 / (4 / 2));;"
  300
  Int
  $ ${INTERPRETER} "(((1000 + 2000) - 1976) * 4) / 512;;"
  8
  Int

Logical ops
  $ ${INTERPRETER} "#False && #False;;"
  #False
  Bool
  $ ${INTERPRETER} "#False || #False;;"
  #False
  Bool
  $ ${INTERPRETER} "#True && #False;;"
  #False
  Bool
  $ ${INTERPRETER} "#True || #False;;"
  #True
  Bool
  $ ${INTERPRETER} "#True && #True;;"
  #True
  Bool
  $ ${INTERPRETER} "#True || #True;;"
  #True
  Bool

If expression
  $ ${INTERPRETER} "if #True then 666 else 777;;"
  666
  Int
  $ ${INTERPRETER} "if #False then #True else #True;;"
  #True
  Bool
  $ ${INTERPRETER} "if #True || #False then #False && #True else 123 + 321;;"
  Fatal error: exception Shared.Common.TypeCheckError("Can't unify types: t1=Bool with t2=Int")
  [2]

Cons
  $ ${INTERPRETER} "123 : (321 : []);;"
  [123, 321]
  List(Int)
  $ ${INTERPRETER} "(100 * 10) : ((100 / 10) : []);;"
  [1000, 10]
  List(Int)

Call
  $ ${INTERPRETER} "$(cat call3.ul)"
  500
  ([Int Int] -> Int)
  ([] -> Int)
  Int

Val
  $ ${INTERPRETER} "val a = 300;;"
  300
  Int
  $ ${INTERPRETER} "val a = 321 - 21;;"
  300
  Int

List
  $ ${INTERPRETER} "[1, 2, 3, 4, 5];;"
  [1, 2, 3, 4, 5]
  List(Int)
  $ ${INTERPRETER} "[1, #False, 2, #True];;"
  Fatal error: exception Shared.Common.TypeCheckError("Can't unify types: t1=Int with t2=Bool")
  [2]
  $ ${INTERPRETER} "$(cat car.ul)"
  [1, 4]
  ([] -> List(List(Int)))
  ([] -> List(Int))
  ([] -> List(Int))
  List(List(Int))
  List(Int)
  List(Int)
  $ ${INTERPRETER} "$(cat cdr.ul)"
  [[2, 3, 4], [3]]
  ([] -> List(List(Int)))
  List(List(Int))

Apply
  TODO $ ${INTERPRETER} "apply([[:]], #False, #True);;"
  TODO [#False, #True]
  $ ${INTERPRETER} "apply((fn x y = x + y), 23, 34);;"
  57

Lambda
  $ ${INTERPRETER} "val inc = (fn x = x + 1);; inc(1);;"
  2
  ([Int] -> Int)
  Int
  $ ${INTERPRETER} "$(cat lam2.ul)"
  165
  ([Int] -> Int)
  Int

Define
  $ ${INTERPRETER} "fn inc x = x + 1;; inc(10);;"
  11
  ([Int] -> Int)
  Int

Let
  $ ${INTERPRETER} "let x = 123 in x + 123;;"
  246
  Int
  $ ${INTERPRETER} "let x = 10 * 10 in let y = 1000 / 2 in x + y;;"
  600
  Int
  $ ${INTERPRETER} "let id = (fn x = x) in let r1 = id(123) in let r2 = id(#True) in r2;;"
  #True
  Bool
  $ ${INTERPRETER} "let sqr = (fn x = x * x) in sqr(10) + sqr(20);;"
  500
  Int

Simple recursion
  $ ${INTERPRETER} "$(cat recurs.ul)"
  #True
  ([Int] -> Bool)
  ([] -> Bool)
  Bool

Factorial
  $ ${INTERPRETER} "$(cat fact.ul)"
  1307674368000
  ([Int] -> Int)
  ([Int] -> Int)
  Int

Fibonacci
  $ ${INTERPRETER} "$(cat fibonacci.ul)"
  987

Combinators (trivial test of polymorphic functions)
  $ ${INTERPRETER} "$(cat combinators.ul)"
  0
  ([T2] -> T2)
  ([T6 T7] -> T6)
  ([([T14 T15] -> T16) ([T14] -> T15) T14] -> T16)
  ([] -> Int)
  Int

Gcd
  $ ${INTERPRETER} "$(cat gcd.ul)"
  25
  ([Int Int Int] -> Int)
  ([] -> Int)
  Int

Map
  $ ${INTERPRETER} "$(cat map.ul)"
  55
