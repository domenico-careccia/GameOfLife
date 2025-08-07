module params
use mpi
implicit none
integer :: ndims
integer, allocatable :: dims(:)
logical, allocatable :: periodic(:)
logical :: reorder

integer :: nxglobal, nyglobal, nzglobal


contains
subroutine set_mpi_parameters

ndims = 2
allocate(dims(0:ndims-1))
allocate(periodic(0:ndims-1))


dims(0) = 1
dims(1) = 1
reorder = .false.
periodic = .true.

end subroutine set_mpi_parameters

subroutine set_grid_parameters
implicit none
nxglobal = 10000
nyglobal = 10000

end subroutine set_grid_parameters





end module params