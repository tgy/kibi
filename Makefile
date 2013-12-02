all:
	cd src; make
	cp src/kibi.native .

run:
	./kibi.native

clean:
	rm -rf *.{native,byte}
	cd src; make clean
