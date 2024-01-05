repl: FORCE
	OCAMLRUNPARAM=b dune exec main repl

compile: FORCE
	dune exec main compile $(SRC)
	cd runtime && make

test: FORCE
	dune clean
	dune build

	@cp _build/default/main.exe test/
	@cp -r runtime test/runtime
	
	-cd test && python3 compiler_tester.py

	@rm -f test/main.exe
	@rm -r test/runtime

FORCE:
