INCLUDE=-I/usr/local/netcdf-4.6.1/include
LIBS=-L/usr/local/netcdf-4.6.1/lib -lnetcdff -lnetcdf

main.exe : fd1d_heat_explicit.o libfmw.a 
	nagfor $< -L. -lfmw ${LIBS} -o $@
libfmw.a : CFL_mod.o IO_mod.o RHS_mod.o Solver_mod.o Types_mod.o 
	ar rcs libfmw.a $^
fd1d_heat_explicit.o :  fd1d_heat_explicit.f90 libfmw.a
	nagfor -c -I. ${INCLUDE} fd1d_heat_explicit.f90
CFL_mod.o : CFL_mod.f90 Types_mod.o
	nagfor -c -I. $<
IO_mod.o : IO_mod.f90 Types_mod.o
	nagfor -c -I. ${INCLUDE} $<
RHS_mod.o : RHS_mod.f90 Types_mod.o
	nagfor -c -I. $<
Solver_mod.o : Solver_mod.f90 RHS_mod.o Types_mod.o 
	nagfor -c -I. $<
Types_mod.o : Types_mod.f90
	nagfor -c $<

.PHONY: clean

clean:
	rm -f *.mod *.o *.png *.exe *.a