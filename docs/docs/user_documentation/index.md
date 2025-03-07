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

By default, this build uses diamond differencing (MACRO=CODETYPE). To build SNES with diamond differencing (default), simply run:
```bash
make
```
To build SNES with linear-discontinuous spatial differencing (MACRO=SNEL), run:
```bash
make snel1.2
```

## Usage

Examples and explanations on how to use the software.
