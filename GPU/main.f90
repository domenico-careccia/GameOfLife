program gameOfLife
use mpi
use mpi_utils
use params
use grid
use evolution
use mpi_comms
use cudafor
implicit none
integer :: iter
real*8 :: startTime, endTime

call set_mpi_parameters()
call mpi_initial_stuff() 
call mpi_cartesian_stuff()
ierr = cudaSetDevice(0)

call set_grid_parameters()
call set_local_grid()
call export_global_coords()

write(*,*) 'Process ', myrank, 'coords', mycoords, 'RU', rightuprank

call field_initialize()


startTime = mpi_wtime()
do iter = 1,700
  field = field_d
  !call field_write()
  !write(*,*) 'Eseguo passo iterativo'
  call field_step()
  !write(*,*) 'Fine step'
end do
field = field_d
call field_write()
endTime =  mpi_wtime()
if (myrank == 0) print '("Time = ",f10.3," seconds.")', endTime - startTime

call mpi_final_stuff()

end program gameOfLife