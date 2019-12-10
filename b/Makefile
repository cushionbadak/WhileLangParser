.PHONY : all run clean install-deps
 
all : clean
	ocamlbuild -use-menhir -Is src/ main.native
 
exampleRun :
	ocamlbuild -Is src/ main.native -- parser_testcase
     
clean :
	ocamlbuild -clean
 
install-deps :
	opam install menhir

