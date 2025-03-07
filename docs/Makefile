.PHONY: html github clean clobber

# Needs to be consistent with input to Ford in snes.md
markdown := $(filter-out images/index.md, $(wildcard *.md))
fortran := $(wildcard ../src/*.f90)

html: snes.md $(markdown) $(fortran)
	uv run ford snes.md

github:
	ghp-import --no-jekyll --no-history --push html

clean:
	rm -f doxygen.log

clobber: clean
	rm -rf html latex
