
CAMLC = ocamlfind ocamlc -thread -g $(LIB)
LIB = -package extlib,postgresql -linkpkg

all:
	$(CAMLC) $(LIB) psqltest.ml -o psqltest
