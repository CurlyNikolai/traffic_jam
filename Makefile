all:
	gfortran -c -O2 src/mer.f90 src/subs.f90 src/ana.f90 src/main.f90
	gfortran -o jam mer.o subs.o ana.o main.o
	mv jam run/

.PHONY: clean

clean:
	rm *.o
	rm *.mod


