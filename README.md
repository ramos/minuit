This repository is a fork of https://github.com/ramos/minuit

The minuit source code compiles well with gcc -O3 -fpic and links with -lgfortran,\
but reports a large number or warnings concerning obsolet DO...END DO and IF(A) kk,nn,mm\
statements \
The minuit.patch elimininates the compilation warnings and sets minuit to give the smae results \
as the last compiled version available for debian ...20061220+dfsg3-4.4

prompt> patch -p1 < minuit.patch && make

