OCAMLBUILD=ocamlbuild -use-ocamlfind
OCAMLBUILD_DIR=$(shell ocamlbuild -where)

all:
	$(OCAMLBUILD) test.native

run:
	./test.native

debug:
	$(OCAMLBUILD) test.d.byte

clean:
	$(OCAMLBUILD) -clean
	rm -rf out/*

