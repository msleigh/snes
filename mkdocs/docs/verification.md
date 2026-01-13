# Verification

Verification compares SNES results against the checked-in references for both
schemes. The test harness uses the `qa/` inputs and stores outputs alongside
logs for inspection.

The following plot shows the $k_\mathrm{eff}$ results for the test problems:

![$k_\mathrm{eff}$ results](images/keff_results.png)

## Running verification

Run the test suites:

```bash
make tests
make testl
```

Use the `check` helper to compare new outputs against references:

```bash
./check s
./check l
```

The verification plot gathers `k_eff` values from the test outputs and writes
an image used in the docs.

Generate the plot used above:

```bash
uv run python verification.py
```
