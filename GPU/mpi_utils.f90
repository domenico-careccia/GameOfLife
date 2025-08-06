module mpi_utils
use mpi
use params
use cudafor
implicit none
integer :: myrank, commsize
integer :: ierr
integer :: mpi_comm_cart
integer, allocatable :: mycoords(:)

integer :: leftrank, rightrank, uprank, downrank, frontrank, backrank
integer :: leftdownrank, leftuprank, rightdownrank, rightuprank
integer :: status(mpi_status_size)

!integer :: mpi_integer1


contains

subroutine mpi_initial_stuff
implicit none
call mpi_init(ierr)
call mpi_comm_size(mpi_comm_world, commsize, ierr)
call mpi_comm_rank(mpi_comm_world, myrank, ierr)

!call mpi_type_match_size(mpi_typeclass_integer, 1, mpi_integer1, ierr)
end subroutine mpi_initial_stuff

subroutine mpi_final_stuff
implicit none
call mpi_finalize(ierr)

end subroutine mpi_final_stuff

subroutine mpi_cartesian_stuff
implicit none

call mpi_cart_create(mpi_comm_world, ndims, dims, periodic, reorder, mpi_comm_cart, ierr)
allocate(mycoords(0:ndims-1))


call mpi_cart_coords(mpi_comm_cart, myrank, ndims, mycoords, ierr)
call mpi_cart_shift(mpi_comm_cart, 0, 1, leftrank, rightrank, ierr)
call mpi_cart_shift(mpi_comm_cart, 1, 1, downrank, uprank, ierr)

! This is safe only because I'm using periodic = true
call mpi_cart_rank(mpi_comm_cart, mycoords + [-1,-1], leftdownrank, ierr)
call mpi_cart_rank(mpi_comm_cart, mycoords + [ 1,-1], rightdownrank, ierr)
call mpi_cart_rank(mpi_comm_cart, mycoords + [-1, 1], leftuprank, ierr)
call mpi_cart_rank(mpi_comm_cart, mycoords + [ 1, 1], rightuprank, ierr)
end subroutine mpi_cartesian_stuff


end module mpi_utils