---
project: snes
author: msleigh
license: MIT
website: https://msleigh.io
doc_license: by
extra_filetypes:
    sh #
    py #
graph: false
output_dir: ./html
page_dir: ./docs
preprocess: true
print_creation_date: true
project_github: https://github.com/msleigh/snes
src_dir: ../src
summary: A one-dimensional discrete-ordinates neutron transport code written in Fortran, with diamond-difference and linear-discontinuous variants.
---

## Fortran API reference

The Fortran code is organized into modules that separate configuration, iteration
control, and transport sweeps. `setdata_mod` defines global arrays for mesh,
cross sections, sources, and fluxes. The `iterate_mod` driver manages the
outer (fission) and inner (scattering) iterations and convergence checks, while
`sweep_mod` performs the discrete-ordinates sweeps across cells and directions.
Input parsing and data setup are handled in modules like `readkeys_mod`,
`readmats_mod`, and `initmesh_mod`. Use the module pages to navigate routines,
shared data, and call relationships across the solver.
