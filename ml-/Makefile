.PHONY : all run clean install-deps

all : clean
	ocamlbuild -use-menhir -Is src/ main.native

exampleRun :
	ocamlbuild -Is src/ main.native -- example1.txt
	
clean :
	ocamlbuild -clean

install-deps :
	opam install menhir
