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

Once the code is built, you can run the tests to ensure everything is functioning correctly. To run the tests, execute the following command in the terminal:

```bash
make tests
```

This will run the test suite using the `snes` executable. To run the tests with the `snel` executable, use:

```bash
make testl
```

After running the tests, you can check the results by examining the output files generated in the `qa` directory. The output files will have extensions `.outs` for `snes` and `.outl` for `snel`. You can also review the log files with extensions `.logs` and `.logl` for detailed information about the test runs.

### Using the `compare` Script

The `compare` script is used to compare the effective multiplication factors (K_EFF) between the `snes` and `snel` test outputs. To use it, run:

```bash
./compare
```

This will output the differences in K_EFF values between the two sets of test outputs, helping you identify any discrepancies.

### Using the `updateref` Script

The `updateref` script updates the reference files with the current test outputs. To use it, run:

```bash
./updateref s
```

or

```bash
./updateref l
```

This will update the reference files with the current outputs, which is useful if you have verified that the new outputs are correct.

## Building Documentation Locally

To build the documentation locally, ensure you have the project dependencies installed via uv. Then, from the repository root, run:

```bash
uv run make -C docs html
```

This will generate the final HTML site in the `mkdocs/site` directory. The Ford API reference is generated in `docs/html` and copied into the site during the build.

To try the experimental Zensical build with the same content and `mkdocs/mkdocs.yml` configuration, run:

```bash
uv run make -C docs zensical
```

This uses the same content and writes to the same `site_dir` configured in `mkdocs/mkdocs.yml`.

## Docker Build Instructions

To build the Docker container locally, run:
```bash
docker buildx build -t snes .
```
This command will build the container using the provided Dockerfile.
