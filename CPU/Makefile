FC = mpif90
FFLAGS = -O3

SRC = params.f90 mpi_utils.f90 grid.f90 mpi_comms.f90 evolution.f90 main.f90
EXE = main.exe


all: $(EXE)
	@rm -f *.mod


$(EXE): $(SRC)
	$(FC) $(FFLAGS) $(SRC) -o $(EXE)


clean:
	rm -f *.o *.mod $(EXE)
