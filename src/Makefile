OCAMLBUILD=ocamlbuild -use-ocamlfind
OCAMLBUILD_DIR=$(shell ocamlbuild -where)

all: kibi

kibi:
	$(OCAMLBUILD) kibi.native

test:
	$(OCAMLBUILD) test.native

robert::
	$(OCAMLBUILD) robert.native

genfonts:
	$(OCAMLBUILD) setgen.native

run:
	./kibi.native

run-test:
	./test.native

debug:
	$(OCAMLBUILD) setgen.d.byte

debug-test:
	$(OCAMLBUILD) test.d.byte

clean:
	$(OCAMLBUILD) -clean
	cd anna && make clean
	cd setgen && make clean
	cd lenna && make clean
	cd freddy && make clean
	cd robert && make clean
