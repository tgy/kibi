all:
	xelatex --shell-escape report.tex

run: all
	evince report.pdf

clean:
	rm -rf *.{aux,nav,toc,log,out,snm,vrb,pyg}
