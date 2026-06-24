SOURCE = src
BUILD_BASE = build

FFLAGS = -pedantic \
         -std=f95  \
         -fmax-errors=1 \
         -fcheck=all \
         -Wall -Wextra -Werror \
         -Wno-error=unused-function

CMP = gfortran
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
  OPEN_CMD = open
else ifeq ($(UNAME_S),Linux)
  OPEN_CMD = xdg-open
else
  OPEN_CMD =
endif

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
VERIFICATION_SOURCES = $(wildcard $(SOURCE)/*.f90 $(SOURCE)/*.F90)
VERIFICATION_STAMP_S = $(BUILD_BASE)/.verification_snes.stamp
VERIFICATION_STAMP_L = $(BUILD_BASE)/.verification_snel.stamp
PLOT_STAMP = $(BUILD_BASE)/.verification_plots.stamp
PLOT_FILES = images/figures/index.html \
             images/figures/keff_results.png \
             images/figures/test11_flux_comparison.png
PLOT_DOC_FILES = docs/docs/images/keff_results.png \
                 docs/docs/images/test11_flux_comparison.png

.SUFFIXES: .f90 .F90
.PHONY: tests testl clean cleaner veryclean cleantest cleanertest verycleantest docs plots view-plots snes snel

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

$(VERIFICATION_STAMP_S): $(TEST_PROBLEMS) $(VERIFICATION_SOURCES) qa/jcf nucdata/* references
	$(MAKE) tests
	touch $@

$(VERIFICATION_STAMP_L): $(TEST_PROBLEMS) $(VERIFICATION_SOURCES) qa/jcf nucdata/* referencel
	$(MAKE) testl
	touch $@

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
docs: $(PLOT_FILES) $(PLOT_DOC_FILES)
	make -C docs html

plots: $(PLOT_FILES) $(PLOT_DOC_FILES)

view-plots: images/figures/index.html
	@if [ -z "$(OPEN_CMD)" ]; then \
		echo "No supported browser opener found for $(UNAME_S); open images/figures/index.html manually" >&2; \
		exit 1; \
	fi
	$(OPEN_CMD) images/figures/index.html

$(PLOT_STAMP): verification.py $(VERIFICATION_STAMP_S) $(VERIFICATION_STAMP_L)
	uv run python verification.py
	touch -r images/figures/index.html $@

$(PLOT_FILES) $(PLOT_DOC_FILES): $(PLOT_STAMP)
	@test -f "$@" || { \
		rm -f $(PLOT_STAMP); \
		$(MAKE) $(PLOT_STAMP); \
	}
