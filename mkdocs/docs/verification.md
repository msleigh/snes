# Verification

Verification compares SNES results against the checked-in references for both
schemes. The test harness uses the `qa/` inputs and stores outputs alongside
logs for inspection.

## Running the tests

Diamond-difference:

```sh
make tests
```

Linear-discontinuous:

```sh
make testl
```

## Checking references

Use the `check` helper to compare new outputs against references:

```sh
./check s
./check l
```

## Plotting k_eff

The verification plot gathers `k_eff` values from the test outputs and writes
an image used in the docs:

```sh
uv run python verification.py
```
