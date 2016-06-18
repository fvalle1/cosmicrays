# Makefile for the raggi simulation

# Compiler and flags
FC = gfortran
FCFLAGS = -O3 -fopenmp

# Objects
OBJ = raggiparallelo.o par_zig_mod.o

# Rule to build the main executable
raggi: $(OBJ)
	$(FC) $(FCFLAGS) -o $@ $^

# Dependencies from include files
par_zig_mod.o: par_zig_mod.mod

# Default rules
%.o: %.f95
	$(FC) $(FCFLAGS) -c $<

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

# Make clean
clean:
	rm -f $(OBJ)

