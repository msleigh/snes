.PHONY: html mkdocs ford sync-ford sync-images clean clobber

# Needs to be consistent with input to Ford in snes.md
markdown := $(wildcard *.md)
fortran := $(wildcard ../src/*.f90)

FORD := ford
MKDOCS := mkdocs

MKDOCS_DIR := ../mkdocs
MKDOCS_DOCS := $(MKDOCS_DIR)/docs
MKDOCS_CFG := $(MKDOCS_DIR)/mkdocs.yml

MKDOCS_SITE := $(MKDOCS_DIR)/site
MKDOCS_DOCS_IMAGES := $(MKDOCS_DOCS)/images
MKDOCS_DOCS_FORD := $(MKDOCS_DOCS)/ford
FORD_DIR := html
FORD_OUT := $(FORD_DIR)/index.html
REFERENCE_IMAGES := docs/images
CLEAN_DIRS := latex $(FORD_DIR) $(MKDOCS_DIR)/.cache $(MKDOCS_DOCS_FORD)

html: mkdocs

mkdocs: sync-ford sync-images
	$(MKDOCS) build -f $(MKDOCS_CFG)

sync-ford: $(FORD_OUT)
	rm -rf $(MKDOCS_DOCS_FORD)
	mkdir -p $(MKDOCS_DOCS_FORD)
	rsync -a --delete $(FORD_DIR)/ $(MKDOCS_DOCS_FORD)/

$(FORD_OUT): snes.md $(markdown) $(fortran)
	$(FORD) snes.md

ford: $(FORD_OUT)

sync-images:
	rm -rf $(MKDOCS_DOCS_IMAGES)
	mkdir -p $(MKDOCS_DOCS_IMAGES)
	rsync -a --delete $(REFERENCE_IMAGES)/ $(MKDOCS_DOCS_IMAGES)/

clean:
	rm -rf $(CLEAN_DIRS)

clobber: clean
	rm -rf $(MKDOCS_SITE)
