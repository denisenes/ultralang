repl: FORCE
	dune exec main repl

compile: FORCE
	dune exec main compile
	cd runtime && make

test: FORCE
	mv test test.t
	dune clean
	dune build
	cp _build/default/main.exe test.t/
	-dune test
	mv test.t test
	rm -f test/main.exe

FORCE:
