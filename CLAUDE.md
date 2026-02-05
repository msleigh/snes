# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

SNES (Simple Neutron-Transport Equation Solver) is a one-dimensional discrete-ordinates neutron transport code written in Fortran. It has two variants:
- **SNES**: Diamond-difference spatial differencing
- **SNEL**: Linear-discontinuous spatial differencing

The variants share the same source files but use conditional compilation (`-DSNES` or `-DSNEL` preprocessor flags) for variant-specific behavior in `.F90` files.

## Build Commands

```bash
make snes          # Build diamond-difference executable
make snel          # Build linear-discontinuous executable
make tests         # Build SNES and run all tests with validation
make testl         # Build SNEL and run all tests with validation
make clean         # Remove build artifacts (required when switching variants)
make clobber       # Full clean including test outputs
```

## Running Individual Tests

```bash
./qa/jcf "s" qa/snestp001.in    # Run single test with SNES
./qa/jcf "l" qa/snestp001.in    # Run single test with SNEL
./check "s"                      # Validate all SNES outputs against references
./check "l"                      # Validate all SNEL outputs against references
```

## Documentation

```bash
make -C docs html                # Build full documentation (Ford + MkDocs)
uv run ford docs/snes.md         # Build Ford API docs only
```

## Code Architecture

### Source Structure
- `src/` - All Fortran source files (~7000 lines total)
- `src/snes.f90` - Main program entry point
- `src/*.F90` - Files with conditional compilation for variant-specific code
- `src/*.f90` - Shared code between variants

### Build System
- Separate build directories: `build/snes/` and `build/snel/`
- Compiler: GFortran with F95 standard (`-std=f95 -pedantic`)
- All warnings treated as errors (`-Werror`)

### Test Infrastructure
- `qa/snestp*.in` - 95+ test problem input files
- `references` / `referencel` - Reference K_EFF values for validation
- `numcheck.py` - Numerical comparison with relative tolerance (1e-4)

### Nuclear Data
- `nucdata/` - Nuclear cross-section data files used by test problems

## Development Setup

```bash
uv sync                 # Install Python dependencies
pre-commit install      # Set up git hooks (notebook output stripping, shellcheck)
```

## Key Conventions

- Fortran code must comply with F95 standard
- Test validation compares computed K_EFF (eigenvalue) against reference values
- Run `make clean` when switching between SNES and SNEL builds
