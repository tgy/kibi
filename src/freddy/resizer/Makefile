OCAMLBUILD=ocamlbuild -use-ocamlfind
OCAMLBUILD_DIR=$(shell ocamlbuild -where)

all:
	        $(OCAMLBUILD) test.native

clean:
	        $(OCAMLBUILD) -clean
					rm -rf out/*

run:
	./test.native
