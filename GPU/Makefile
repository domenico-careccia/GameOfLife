FC = mpifort
FFLAGS = -O3 -cuda -Minfo=accel

SRC = params.f90 mpi_utils.f90 grid.f90 mpi_comms.f90 evolution.cuf main.f90
OBJ = $(SRC:.f90=.o)
OBJ := $(OBJ:.cuf=.o)
EXE = main.exe

all: $(EXE)
	@rm -f *.mod

$(EXE): $(OBJ)
	$(FC) $(FFLAGS) $(OBJ) -o $(EXE)

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

%.o: %.cuf
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -f *.o *.mod $(EXE)

