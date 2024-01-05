repl: FORCE
	OCAMLRUNPARAM=b dune exec main repl

compile: FORCE
	dune exec main compile $(SRC)
	cd runtime && make

testi: FORCE
	@cp _build/default/main.exe test/
	@mv test test.t

	-dune test
	
	@mv test.t test
	@rm -f test/main.exe

testc: FORCE
	dune clean
	dune build

	@cp _build/default/main.exe test/
	@cp -r runtime test/runtime
	
	-cd test && python3 compiler_tester.py

	@rm -f test/main.exe
	@rm -r test/runtime

FORCE:
