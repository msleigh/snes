---
title: User Documentation
---

This section provides guidance for users on how to install, configure, and use the software effectively.

## Installation

Prerequisites:

* A Fortran compiler (e.g., gfortran) that supports Fortran 95 or later.
* GNU Make.
* A Unix-like environment.

To build SNES, open a terminal at the project root and run:
```bash
make
```
This will compile the code using the provided Makefile.

By default, this build uses diamond differencing (MACRO=SNES). To build SNES with diamond differencing (default), simply run:
```bash
make
```
To build SNES with linear-discontinuous spatial differencing (MACRO=SNEL), run:
```bash
make snel1.2
```

## Usage

Examples and explanations on how to use the software.

## Building Documentation Locally

To build the documentation locally, ensure you have all the necessary dependencies installed. Then, navigate to the `docs` directory and run:

```bash
make -C docs html
```

This will generate the HTML documentation in the `docs/docs/_build/html` directory.

## Docker Build Instructions

To build the Docker container locally, run:
```bash
docker buildx build -t snes .
```
This command will build the container using the provided Dockerfile.
