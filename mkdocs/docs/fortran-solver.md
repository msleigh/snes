# Fortran Solver

This page walks through the core solver implementation in `src/`, with a focus
on the control flow, iteration strategy, and transport sweep.

## Entry point and flow

The main program in `src/snes.f90` wires together the solver stages:

1. Read input keywords and material data (`readkeys`, `readmats`, `readsrcs`).
2. Allocate arrays and build the spatial mesh (`allocstor`, `initmesh`).
3. Build the angular quadrature set (`quadsets`).
4. Iterate to solve the transport equation (`iterate`).
5. Write flux output (`printflux1`, `printflux2`).
6. Deallocate all storage (`deallocstor`).

This matches the structure printed in the runtime banner and is the best place
to start when tracking overall execution.

## Compile-time scheme selection

Two spatial differencing schemes are compiled via preprocessor macros:

- `SNES` (diamond-difference): built by the `snes` target in `Makefile`.
- `SNEL` (linear-discontinuous): built by the `snel` target in `Makefile`.

The `-D` macro sets conditional code paths in files like `src/iterate.F90` and
`src/sweep.F90`, changing the number of nodes per cell and the update formulas.

## Data layout and mesh

Global data is defined in `src/setdata.f90`:

- Mesh arrays: `width`, `centre`, `origin`, `matnum`.
- Quadrature: `mu` (direction cosines), `wgt` (weights).
- Cross sections: `sigma_t`, `sigma_s`, `sigma_f`.
- Sources: `source_i` (imposed), `source_s` (scattering), `source_f` (fission).
- Fluxes: `scalflux` and snapshot arrays for convergence tracking.

The number of nodes per cell (`numnodes`) distinguishes the spatial scheme:
`1` for diamond-difference and `2` for linear-discontinuous.

## Iteration strategy

`src/iterate.F90` implements nested iterations:

- **Outer loop (fission source)** for eigenvalue problems (`calctype = 1`).
  - Updates the fission source and estimates $k_\mathrm{eff}$.
  - Converges on `epsouter` with a max of `imaxouter` iterations.
- **Inner loop (scattering source)** for each energy group.
  - Rebuilds scattering source, then calls the sweep.
  - Converges on `epsinner` with a max of `imaxinner` iterations.

For fixed-source problems (`calctype = 2`), the outer loop exits after a single
pass because the fission source is not updated.

## Transport sweep

`src/sweep.F90` performs the discrete-ordinates sweep:

- Two sweeps per solve (`numsweeps = 2`), right-to-left then left-to-right.
- Incoming angular flux is set by boundary conditions
  (`lhbc`, `lh_eflux`, `rh_eflux`).
- For each cell and direction, the angular flux is updated and accumulated into
  the scalar flux using quadrature weights.
- Negative flux fix-up is controlled by `nffu` (counts in `nffu_call`).

The update formulas differ between `SNES` and `SNEL`, but both paths produce the
scalar flux arrays used for convergence checks and output.

## Sources and cross sections

Source terms are constructed in:

- `src/fisssource.F90` for fission production.
- `src/scatsource.f90` for scattering contribution.
- `src/updatesource.f90` for per-iteration source updates.

Material and nuclear data are loaded by `readmats` and `readnucdat`, which fill
the cross-section arrays that drive the sweep.

## Extending the solver

Common extension points:

- Add new quadrature sets in `src/quadsets.f90`.
- Add new material input handling in `src/readmats.f90`.
- Adjust convergence logic in `src/iterate.F90`.
- Modify sweep formulas in `src/sweep.F90`.

Keep `setdata.f90` consistent when adding new arrays or configuration options.
