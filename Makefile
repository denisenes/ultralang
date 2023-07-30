repl: FORCE
	dune exec main repl

compile: FORCE
	gcc -O3 --omit-frame-pointer ultra.s runtime/*.c

test: FORCE
	dune clean
	dune build	
	dune test

FORCE:
