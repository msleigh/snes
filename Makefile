SOURCE = src
BUILD_BASE = build

FFLAGS = -pedantic \
         -std=f95  \
         -fmax-errors=1 \
         -fcheck=all \
         -Wall -Wextra -Werror \
         -Wno-error=unused-function

CMP = gfortran

# Object files (without path)
OBJS = getkinds.o \
       setdata.o \
       casechange.o \
       typechange.o \
       io_utils.o \
       readline.o \
       readkeys.o \
       allocstor.o \
       readnucdat.o \
       readmats.o \
       readsrcs.o \
       initmesh.o \
       quadsets.o \
       fisssource.o \
       scatsource.o \
       updatesource.o \
       sweep.o \
       iterate.o \
       printflux1.o \
       printflux2.o \
       deallocstor.o \
       snes.o

ifeq ($(origin TEST_PROBLEMS), undefined)
  TEST_PROBLEMS = $(sort $(wildcard qa/snestp*.in))
endif
TEST_OUTPUTS = $(TEST_PROBLEMS:.in=.outs)
TEST_OUTPUTL = $(TEST_PROBLEMS:.in=.outl)

.SUFFIXES: .f90 .F90
.PHONY: tests testl clean cleaner veryclean cleantest cleanertest verycleantest docs snes snel

# SNES build
BUILD_SNES = $(BUILD_BASE)/snes
OBJS_SNES = $(addprefix $(BUILD_SNES)/,$(OBJS))

snes: $(BUILD_SNES)/.dir $(OBJS_SNES)
	$(CMP) $(FFLAGS) $(OBJS_SNES) -o snes$(VERSION)

$(BUILD_SNES)/.dir:
	mkdir -p $(BUILD_SNES)
	touch $@

$(BUILD_SNES)/%.o: $(SOURCE)/%.f90 | $(BUILD_SNES)/.dir
	$(CMP) $(FFLAGS) -J$(BUILD_SNES) -c -o $@ $<

$(BUILD_SNES)/%.o: $(SOURCE)/%.F90 | $(BUILD_SNES)/.dir
	$(CMP) $(FFLAGS) -J$(BUILD_SNES) -c -DSNES -o $@ $<

# SNEL build
BUILD_SNEL = $(BUILD_BASE)/snel
OBJS_SNEL = $(addprefix $(BUILD_SNEL)/,$(OBJS))

snel: $(BUILD_SNEL)/.dir $(OBJS_SNEL)
	$(CMP) $(FFLAGS) $(OBJS_SNEL) -o snel$(VERSION)

$(BUILD_SNEL)/.dir:
	mkdir -p $(BUILD_SNEL)
	touch $@

$(BUILD_SNEL)/%.o: $(SOURCE)/%.f90 | $(BUILD_SNEL)/.dir
	$(CMP) $(FFLAGS) -J$(BUILD_SNEL) -c -o $@ $<

$(BUILD_SNEL)/%.o: $(SOURCE)/%.F90 | $(BUILD_SNEL)/.dir
	$(CMP) $(FFLAGS) -J$(BUILD_SNEL) -c -DSNEL -o $@ $<

# Test targets
%.outs: %.in snes qa/jcf nucdata/*
	./qa/jcf "s" $< 2>&1 > $*.logs

%.outl: %.in snel qa/jcf nucdata/*
	./qa/jcf "l" $< 2>&1 > $*.logl

tests: $(TEST_OUTPUTS) references
	./check "s"

testl: $(TEST_OUTPUTL) referencel
	./check "l"

# Clean targets
clean:
	rm -rf $(BUILD_BASE)

cleaner:
	rm -f snes snel

veryclean: clean cleaner

clobber: veryclean verycleantest

cleantest:
	rm -rf qa/snestp*.log* qa/tpx*

cleanertest:
	rm -rf qa/snestp*.out* qa/snestp*.flx*

verycleantest: cleantest cleanertest

# Documentation
docs: docs/docs/images/keff_results.png
	make -C docs html

docs/docs/images/keff_results.png: verification.py
	uv run python verification.py
