# Getting Started

## Prerequisites

- GFortran
- Make
- Python 3 (for verification and docs)

## Build and test

Build and run the diamond-difference tests:

```sh
make tests
```

Build and run the linear-discontinuous tests:

```sh
make testl
```

## Documentation

The Ford documentation is built from the `docs/` folder:

```sh
make -C docs html
```

## Verification plot

Generate the comparison plot used in the docs:

```sh
uv run python verification.py
```

The output image is written to `docs/docs/images/keff_results.png`.
