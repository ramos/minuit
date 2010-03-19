
F90=ifort
PATHLIB=/usr/local/lib/
PATHINC=/usr/local/include

all: api lib

lib: 
	$(F90) -c *.F
	rm minuitAPI.o 
	ar rcs libminuit.a *.o

api: minuitAPI.f90
	$(F90) -c minuitAPI.f90

install: all
	cp libminuit.a  $(PATHLIB)
	cp minuitapi.mod $(PATHINC)

clean: 
	rm *.o 
	rm *.mod
	rm *.a
