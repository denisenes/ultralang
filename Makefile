repl: FORCE
	dune exec main repl

test: FORCE
	dune clean
	dune build	
	dune test

FORCE:
