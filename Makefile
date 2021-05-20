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
OBJL := $(shell cat ./Objectsl)

ifeq ($(origin TEST_PROBLEMS), undefined)
  TEST_PROBLEMS = $(sort $(wildcard qa/snestp*.in))
endif
TEST_OUTPUT = $(TEST_PROBLEMS:.in=.out)
TEST_OUTPUTL = $(TEST_PROBLEMS:.in=.outl)

.SUFFIXES: .f90
.PHONY: tests testl clean cleaner veryclean cleantest cleanertest verycleantest

%.o: %.F90
	$(CMP) $(FFLAGS) -c -D$(MACRO) -o $@ $<

%.o: %.f90
	$(CMP) $(FFLAGS) -c -o $@ $<

%.out: %.in snes$(VERSION)_LINUX qa/snestp001.jcf nucdata/*
	./qa/snestp001.jcf $< 2>&1 > $*.log

%.outl: %.in snel$(VERSION)_LINUX qa/sneltp001.jcf nucdata/*
	./qa/sneltp001.jcf $< 2>&1 > $*.logl

snes$(VERSION)_LINUX: MACRO=CODETYPE
snes$(VERSION)_LINUX: $(OBJ) Objects
	$(CMP) $(FFLAGS) $(LFLAGS) $(OBJ) -o snes$(VERSION)_LINUX

snel$(VERSION)_LINUX: MACRO=SNEL
snel$(VERSION)_LINUX: $(OBJL) Objectsl
	$(CMP) $(FFLAGS) $(LFLAGS) $(OBJL) -o snel$(VERSION)_LINUX

tests: $(TEST_OUTPUT) reference
	./check

testl: $(TEST_OUTPUTL) referencel
	./checkl

clean:
	rm -f *.lst *.o *.mod loadmap

cleaner:
	rm -f snes$(VERSION)_LINUX snel$(VERSION)_LINUX

veryclean: clean cleaner

cleantest:
	rm -rf qa/snestp*.log*

cleanertest:
	rm -rf qa/snestp*.out*

verycleantest: cleantest cleanertest
