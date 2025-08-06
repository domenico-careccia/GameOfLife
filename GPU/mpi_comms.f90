module mpi_comms
use grid
use mpi_utils
use cudafor

implicit none
contains


subroutine communicate
implicit none

call mpi_sendrecv(field(1,1:ny), ny, mpi_integer1, leftrank, 0, &
                  field(nx+1,1:ny), ny, mpi_integer1, rightrank, 0, &
                  mpi_comm_cart, status, ierr)

call mpi_sendrecv(field(nx,1:ny), ny, mpi_integer1, rightrank, 0, &
                  field(0,1:ny), ny, mpi_integer1, leftrank, 0, &
                  mpi_comm_cart, status, ierr)

call mpi_sendrecv(field(1:nx,1), nx, mpi_integer1, downrank, 0, &
                  field(1:nx,ny+1), nx, mpi_integer1, uprank, 0, &
                  mpi_comm_cart, status, ierr)

call mpi_sendrecv(field(1:nx,ny), nx, mpi_integer1, uprank, 0, &
                  field(1:nx,0), nx, mpi_integer1, downrank, 0, &
                  mpi_comm_cart, status, ierr)

call mpi_sendrecv(field(1,1), 1, mpi_integer1, leftdownrank, 0, &
                  field(nx+1,ny+1), 1, mpi_integer1, rightuprank, 0, &
                  mpi_comm_cart, status, ierr)

call mpi_sendrecv(field(nx,ny), 1, mpi_integer1, rightuprank, 0, &
                  field(0,0), 1, mpi_integer1, leftdownrank, 0, &
                  mpi_comm_cart, status, ierr)

call mpi_sendrecv(field(1,ny), 1, mpi_integer1, leftuprank, 0, &
                  field(nx+1,0), 1, mpi_integer1, rightdownrank, 0, &
                  mpi_comm_cart, status, ierr)

call mpi_sendrecv(field(nx,1), 1, mpi_integer1, rightdownrank, 0, &
                  field(0,ny+1), 1, mpi_integer1, leftuprank, 0, &
                  mpi_comm_cart, status, ierr)

end subroutine communicate



end module mpi_comms