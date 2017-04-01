#!/bin/bash

#
#  Simple build script.
#
#  Requires: FoBiS and Ford
#

MODCODE='csv_module.F90'            # module file name
LIBOUT='libcsv.a'                   # name of library
DOCDIR='./doc/'                     # build directory for documentation
SRCDIR='./src/'                     # library source directory
TESTSRCDIR='./src/tests/'           # unit test source directory
BINDIR='./bin/'                     # build directory for unit tests
LIBDIR='./lib/'                     # build directory for library
FORDMD='fortran-csv-module.md'      # FORD config file name

#compiler flags:

FCOMPILER='gnu' #Set compiler to gfortran
FCOMPILERFLAGS='-c -O2 -std=f2008'
#FCOMPILER='intel' #Set compiler to intel
#FCOMPILERFLAGS='-c -O0 -warn -stand f08 -traceback -g'

#build using FoBiS:

if hash FoBiS.py 2>/dev/null; then

    echo "Building library..."

    FoBiS.py build -compiler ${FCOMPILER} -cflags "${FCOMPILERFLAGS}" -dbld ${LIBDIR} -s ${SRCDIR} -dmod ./ -dobj ./ -t ${MODCODE} -o ${LIBOUT} -mklib static -colors

    echo "Building test programs..."

    FoBiS.py build -compiler ${FCOMPILER} -cflags "${FCOMPILERFLAGS}" -dbld ${BINDIR} -s ${TESTSRCDIR} -dmod ./ -dobj ./ -colors -libs ${LIBDIR}${LIBOUT} --include ${LIBDIR}

else
    echo "FoBiS.py not found! Cannot build library. Install using: sudo pip install FoBiS.py"
fi

# build the documentation using FORD:

if hash ford 2>/dev/null; then

    echo "Building documentation..."

    ford ${FORDMD}

else
    echo "Ford not found! Cannot build documentation. Install using: sudo pip install ford"
fi
