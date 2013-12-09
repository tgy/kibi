OCAMLBUILD=ocamlbuild -use-ocamlfind
OCAMLBUILD_DIR=$(shell ocamlbuild -where)

all:
	        $(OCAMLBUILD) segmentation.native

clean:
	        $(OCAMLBUILD) -clean

run: all
	        ./segmentation.native
					
resize:
					$(OCAMLBUILD) resize.native

rr: resize
					./resize.native

boxmaker:
					$(OCAMLBUILD) boxmaker.native

br: boxmaker
					./boxmaker.native
