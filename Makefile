repl: FORCE
	dune exec main repl

compile: FORCE
	dune exec main compile
	cd runtime && make

test: FORCE
	dune clean
	dune build	
	dune test

FORCE:
