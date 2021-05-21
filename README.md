snes
====

A one-dimensional discrete-ordinates neutron transport code written in Fortran, with diamond-difference and linear-
discontinuous variants.

<img src="https://img.shields.io/github/v/release/msleigh/snes?include_prereleases"> <img src="https://img.shields.io/github/license/msleigh/fcimc"> <img src="https://img.shields.io/tokei/lines/github/msleigh/snes"> <img src="https://img.shields.io/github/last-commit/msleigh/snes">

![Build status (`develop`)](https://github.com/msleigh/snes/actions/workflows/main.yml/badge.svg?branch=develop)

## Dependencies

- Make
- GFortran
- Doxygen (optional)
- Docker (optional)
- Jupyter (optional)

## Usage

### Execution

To build and run the tests:

    make tests  # Diamond-difference version
    make testl  # Linear-discontinuous version

Do a `make clean` when switching between versions.

### Documentation

To build the documentation:

    doxygen
    open html/index.html

### Docker

To create the Docker build (used in GitHub to build and run the tests):

    docker build -t snes .

##  Verification

Run the Jupyter notebook to plot the results:

    jupyter notebook verification.ipynb

