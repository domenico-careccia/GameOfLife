module grid
use mpi
use mpi_utils
use params

implicit none
integer :: nx, ny, nz
integer :: imin, imax, jmin, jmax

integer(kind=1), allocatable :: field(:,:), field_new(:,:)
integer, allocatable :: x(:), y(:)

contains

subroutine set_local_grid
implicit none
integer :: basex, basey, basez
integer :: extrax, extray, extraz
integer :: i,j

extrax = mod(nxglobal, dims(0))
extray = mod(nyglobal, dims(1))
basex = (nxglobal - extrax) / dims(0)
basey = (nyglobal - extray) / dims(1)
nx = basex
ny = basey
if (mycoords(0) < extrax) nx = nx + 1
if (mycoords(1) < extray) ny = ny + 1
allocate(field(0:nx+1, 0:ny+1), field_new(0:nx+1, 0:ny+1))
allocate(x(0:nx+1))
allocate(y(0:ny+1))

field = 0
field_new = 0

imin = mycoords(0) * basex
if (mycoords(0) < extrax) then 
  imin = imin + mycoords(0)
else
  imin = imin + extrax
endif
imax = imin + nx + 1

jmin = mycoords(1) * basey
if (mycoords(1) < extray) then
  jmin = jmin + mycoords(1)
else
  jmin = jmin + extray
endif
jmax = jmin + ny + 1

x = [(i, i=imin,imax)]
y = [(j, j=jmin,jmax)]
end subroutine set_local_grid

subroutine export_global_coords
implicit none
character(len=20) :: filename

write(filename, '("coords_", I2.2, ".dat")') myrank
open(11, file=filename, form="unformatted", access="stream", status="replace")
write(11) imin+1, imax-1, jmin+1, jmax-1
close(11)

end subroutine export_global_coords

end module grid