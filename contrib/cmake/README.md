# Contributed CMake Support

The following CMake recipes are intended for developers using
csv-fortran as a dependency in their own projects.

Please read through the recipes before use; they contain
documentation that will help integrate csv-fortran
with a CMake based project.

Each recipe has advantages and disadvantages as listed below.
Only one should be needed but they may be chained such that
CMake preferentially uses a local installation and falls back
to pulling source code over the network or vice versa.

## FindFCSV.cmake

FindFCSV searches the host system for a previously installed fcsv library
(*e.g.* `libfcsv.a`) and the Fortran module file `csv_module.mod`.
No version checking is performed and there will likely be problems
if csv-fortran and the parent project are built with
different Fortran compilers.

## BuildFCSV.cmake

__Important__: Review and modify `GIT_REPOSITORY` and `GIT_TAG` before
use; they currently point to a test fork of the library. Also, ensure that
`include(Git)` and `include(ExternalProject)` are called in the parent
project before this recipe.

This recipe uses `ExternalProject_Add` to retrieve the csv-fortran
project and build it as an standalone dependency of the parent
project. Tests and compiler flags of FCSV are kept separate from the
parent project.

## IncludeFCSV.cmake

__Important__: Review and modify `GIT_REPOSITORY` and `GIT_TAG` before
use; they currently point to a test fork of the library. Also, ensure that
`include(Git)` and `include(FetchContent)` are called in the parent
project before this recipe.

This recipe uses `FetchContent` to retrieve the csv-fortran
project and build it as part of the parent project. This has two
important effects: compiler flags of the parent and child project
are merged and CTest will run FCSV's tests in addition to those of
the parent project.
