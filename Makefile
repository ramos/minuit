
F90=ifort
PATHLIB=/usr/local/lib/

all: lib

lib: 
	$(F90) -c *.F
	ar rcs libminuit.a *.o

install: all
	cp libminuit.a  $(PATHLIB)

clean: 
	rm *.o 
	rm *.mod
	rm *.a
