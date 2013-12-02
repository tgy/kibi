all:
	cd src; make
	cp src/kibi.native .

run:
	./kibi.native

clean:
	cd src; make clean
