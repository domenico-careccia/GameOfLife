# Conway's Game of Life (Fortran + MPI)

This project is an implementation of the **notorious [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)**.

The project is written in **Fortran 90**, and features **domain decomposition and parallelization** using the **MPI** library + **CUDA**. Although the Game of Life is a simplified discrete system, the parallelization techniques used here are foundational for more complex CFD (Computational Fluid Dynamics) simulations.

## Features

- Parallel implementation using **MPI** + **CUDA**
- Domain decomposition based on Cartesian topology with halo exchange
- Configurable grid size and number of steps
