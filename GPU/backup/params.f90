module params
use mpi
implicit none
integer :: ndims
integer, allocatable :: dims(:)
logical, allocatable :: periodic(:)
logical :: reorder





contains
subroutine set_mpi_parameters

ndims = 2
allocate(dims(0:ndims-1))
allocate(periodic(0:ndims-1))


dims(0) = 3
dims(1) = 2
reorder = .false.
periodic = .false.


end subroutine set_mpi_parameters




end module params