repl: FORCE
	dune exec main repl

compile: FORCE
	cd runtime
	gcc -c -O3 --omit-frame-pointer %.s %.c
	#gcc *.o -o ultra
	#rm *.o

test: FORCE
	dune clean
	dune build	
	dune test

FORCE:
