module evolution
use params
use grid
use mpi
use mpi_utils
use mpi_comms

implicit none
integer :: time

contains

subroutine field_initialize
implicit none
integer :: i, j

time = 0
field = 0 
do i = 1,nx
do j = 1,ny
  if (x(i) == 20 .and. y(j) == 130) field(i,j) = 1
  if (x(i) == 21 .and. y(j) == 129) field(i,j) = 1
  if (x(i) == 22 .and. y(j) == 129) field(i,j) = 1
  if (x(i) == 22 .and. y(j) == 130) field(i,j) = 1
  if (x(i) == 22 .and. y(j) == 131) field(i,j) = 1
end do
end do
end subroutine field_initialize


subroutine field_write
implicit none
character(len=20) :: filename

write(filename, '("field_", I4.4, "_"I2.2, ".dat")') time, myrank
open(10, file=filename, form="unformatted", access="stream", status="replace", action="write")
write(10) field(1:nx,1:ny)
close(10)
end subroutine field_write


subroutine field_step
implicit none
integer :: i, j, neighbors_active

time  = time + 1
call communicate()

do i = 1,nx
do j = 1,ny

  !call count_active_neighbors(i, j, neighbors_active)         ! This is inefficient
  neighbors_active = sum(field(i-1:i+1, j-1:j+1)) - field(i,j) ! This is more efficient



  if (field(i,j) == 1 ) then
    if (neighbors_active < 2) field_new(i,j) = 0
    if (neighbors_active == 2 .or. neighbors_active == 3) field_new(i,j) = 1
    if (neighbors_active > 3) field_new(i,j) = 0
  elseif (field(i,j) == 0) then
    if (neighbors_active == 3) field_new(i,j) = 1
    if (neighbors_active /= 3) field_new(i,j) = 0
  endif

  ! This seems as efficient (a bit less efficient, more compact):
  !field_new(i,j) = merge(1,0, (field(i,j) == 1 .and. (neighbors_active == 2 .or. neighbors_active == 3).or.&
  !                 (field(i,j) == 0 .and. neighbors_active == 3)))

end do
end do
field = field_new
field_new = 0
end subroutine field_step


subroutine count_active_neighbors(i, j, neighbors_active)
! This subroutine is quite inefficient and can be replaced by one line
implicit none
integer, intent(in) :: i, j
integer, intent(out) :: neighbors_active
integer :: ii, jj

neighbors_active = 0
do ii = -1,1
do jj = -1,1
  if (ii == 0 .and. jj == 0) cycle
  if (field(i+ii, j+jj) == 1) neighbors_active = neighbors_active + 1 
end do
end do
end subroutine count_active_neighbors





end module evolution
