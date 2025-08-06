program gameOfLife
use mpi
use mpi_utils
use params
implicit none

call set_mpi_parameters()
call mpi_initial_stuff() 
call mpi_cartesian_stuff()
call mpi_barrier(mpi_comm_cart, ierr)



call mpi_final_stuff()


end program gameOfLife