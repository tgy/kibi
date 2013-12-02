OCAMLBUILD=ocamlbuild -use-ocamlfind
OCAMLBUILD_DIR=$(shell ocamlbuild -where)

all:
	$(OCAMLBUILD) test.native

run:
	$(OCAMLBUILD) test.native && ./test.native

debug:
	$(OCAMLBUILD) test.d.byte

clean:
	$(OCAMLBUILD) -clean

