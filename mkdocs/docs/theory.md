# Theory

SNES solves the steady-state, one-dimensional neutron transport equation using
a discrete-ordinates (S_N) angular discretization. Two spatial differencing
schemes are provided:

- Diamond-difference for a compact, second-order scheme.
- Linear-discontinuous for improved accuracy in optically thick cells.

## Discrete-ordinates form

The angular flux is expanded in a set of discrete directions and weights. The
resulting system is solved by sweeping over angles and spatial cells, enforcing
boundary conditions at the domain edges.

## Scattering and sources

Materials are described through macroscopic cross sections and scattering
moments. Fixed source problems are supported, and eigenvalue mode is used for
criticality calculations.

## Outputs

Primary quantities include scalar flux and the effective multiplication factor
(k_eff) for critical problems. Reference outputs in `references/` and
`referencel/` are used to validate solver changes.
