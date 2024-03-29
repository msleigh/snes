VERSION = 1.2
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
.PHONY: tests testl clean cleaner veryclean cleantest cleanertest verycleantest

%.o: %.F90
	$(CMP) $(FFLAGS) -c -D$(MACRO) -o $@ $<

%.o: %.f90
	$(CMP) $(FFLAGS) -c -o $@ $<

%.outs: %.in snes$(VERSION) qa/jcf nucdata/*
	./qa/jcf $(VERSION) "s" $< 2>&1 > $*.logs

%.outl: %.in snel$(VERSION) qa/jcf nucdata/*
	./qa/jcf $(VERSION) "l" $< 2>&1 > $*.logl

snes$(VERSION): MACRO=CODETYPE
snes$(VERSION): $(OBJ) Objects
	$(CMP) $(FFLAGS) $(LFLAGS) $(OBJ) -o snes$(VERSION)

snel$(VERSION): MACRO=SNEL
snel$(VERSION): $(OBJ) Objects
	$(CMP) $(FFLAGS) $(LFLAGS) $(OBJ) -o snel$(VERSION)

tests: $(TEST_OUTPUTS) references
	./check "s"

testl: $(TEST_OUTPUTL) referencel
	./check "l"

clean:
	rm -f *.lst *.o *.mod loadmap

cleaner:
	rm -f snes$(VERSION) snel$(VERSION)

veryclean: clean cleaner

cleantest:
	rm -rf qa/snestp*.log* qa/tpx*

cleanertest:
	rm -rf qa/snestp*.out* qa/snestp*.flx*

verycleantest: cleantest cleanertest

clobber: veryclean verycleantest
