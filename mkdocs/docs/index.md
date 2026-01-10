# SNES Documentation

SNES is a one-dimensional discrete-ordinates neutron transport solver with
both diamond-difference and linear-discontinuous variants. This site provides
an overview of the model, how to run it, and how to verify outputs.

## What you can find here

- Quick setup steps and basic commands.
- A concise outline of the transport theory used.
- Verification guidance and example checks.

## Project structure

- `src/` contains the Fortran implementation.
- `qa/` includes test problems and reference outputs.
- `docs/` builds the Ford API docs.
- `verification.py` generates a quick comparison plot.
