INCLUDE=-I/usr/local/netcdf-4.6.1/include -I/usr/local/plplot-5.13.0/lib/fortran/modules/plplot
LIBS=-L/usr/local/netcdf-4.6.1/lib -lnetcdff -lnetcdf -L/usr/local/plplot-5.13.0/lib -lplplotfortran -lplplot

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
movie : 
	ffmpeg -f image2 -i image%*.png fd1d_heat_explicit.mp4
nccheck : 
	ncdiff --overwrite h_test01.nc h_test01.nc.valid -o diff.nc
	ncwa -y max --overwrite diff.nc out.nc
	ncdump out.nc | grep "solution ="
unitcheck : 
	${PFUNIT}/bin/pFUnitParser.py testCFL.pf testCFL.F90 -I.
	nagfor -I${PFUNIT}/mod -I. -c testCFL.F90
	nagfor -I${PFUNIT}/mod ${PFUNIT}/include/driver.F90 CFL_mod.o testCFL.o -L${PFUNIT}/lib -lpfunit -I. -o tests.exe
	./tests.exe

.PHONY: clean

clean:
	rm -f *.mod *.o *.png *.exe *.a
