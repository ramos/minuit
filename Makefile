
F90=gcc -O3 -fpic
PATHLIB=../OneFit-Engine/lib

all: lib


lib: 
	$(F90) -c *.F
	ar rcs libminuit.a *.o

install: all
	cp libminuit.a  $(PATHLIB)

clean: 
	rm *.o 
	rm *.a
