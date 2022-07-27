snes
====

A one-dimensional discrete-ordinates neutron transport code written in Fortran, with diamond-difference and linear-
discontinuous variants.

<img src="https://img.shields.io/github/v/release/msleigh/snes?include_prereleases">
<img src="https://img.shields.io/github/license/msleigh/fcimc">
<img src="https://img.shields.io/github/last-commit/msleigh/snes">
<img src="https://img.shields.io/tokei/lines/github/msleigh/snes">

![Build status (`develop`)](https://github.com/msleigh/snes/actions/workflows/main.yml/badge.svg?branch=main)

## Dependencies

### Code

- GFortran

### Bundled calculations

- Matplotlib
- Jupyter

### Documentation

- Doxygen
- Graphviz
- LaTeX
- ghp-import (optional, to push documentation to GitHub Pages)

### Misc

- Docker (optional)

## Installation



## Usage

### Execution

To build and run the tests:

    make tests  # Diamond-difference version
    make testl  # Linear-discontinuous version

Do a `make clean` when switching between versions.

### Documentation

To build the documentation:

    make -C docs html
    open docs/html/index.html

### Cleaning

To clean up intermediate build files etc.:

    make clean

### Docker

To create the Docker build (used in GitHub to build and run the tests):

    docker build -t snes .

##  Verification

Run the Jupyter notebook to plot the results:

    jupyter notebook verification.ipynb

