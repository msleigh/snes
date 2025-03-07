SOURCE = src
VPATH = $(SOURCE)
OBJDIR = ./snes
MODDIR = $(SOURCE)

LFLAGS =

FFLAGS = -pedantic \
         -std=f95  \
         -fmax-errors=1 \
         -fcheck=all \
         -Wall -Wextra -Werror \
         -Wno-error=unused-function

CMP = gfortran

OBJ := $(shell cat ./Objects)

ifeq ($(origin TEST_PROBLEMS), undefined)
  TEST_PROBLEMS = $(sort $(wildcard qa/snestp*.in))
endif
TEST_OUTPUTS = $(TEST_PROBLEMS:.in=.outs)
TEST_OUTPUTL = $(TEST_PROBLEMS:.in=.outl)

.SUFFIXES: .f90
.PHONY: tests testl clean cleaner veryclean cleantest cleanertest verycleantest docs

%.o: %.F90
	$(CMP) $(FFLAGS) -c -D$(MACRO) -o $@ $<

%.o: %.f90
	$(CMP) $(FFLAGS) -c -o $@ $<

%.outs: %.in snes qa/jcf nucdata/*
	./qa/jcf "s" $< 2>&1 > $*.logs

%.outl: %.in snel qa/jcf nucdata/*
	./qa/jcf "l" $< 2>&1 > $*.logl

snes: MACRO=SNES
snes: $(OBJ) Objects
	$(CMP) $(FFLAGS) $(LFLAGS) $(OBJ) -o snes$(VERSION)

snel: MACRO=SNEL
snel: $(OBJ) Objects
	$(CMP) $(FFLAGS) $(LFLAGS) $(OBJ) -o snel$(VERSION)

tests: $(TEST_OUTPUTS) references
	./check "s"

testl: $(TEST_OUTPUTL) referencel
	./check "l"

clean:
	rm -f *.lst *.o *.mod loadmap

cleaner:
	rm -f snes snel

veryclean: clean cleaner

clobber: veryclean verycleantest

cleantest:
	rm -rf qa/snestp*.log* qa/tpx*

cleanertest:
	rm -rf qa/snestp*.out* qa/snestp*.flx*

verycleantest: cleantest cleanertest

docs: docs/docs/images/keff_results.png
	make -C docs html

docs/docs/images/keff_results.png: verification.py
	uv run python verification.py
