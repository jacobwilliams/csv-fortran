![fortran-csv-module](/media/logo.png)
============

![CI Status](https://github.com/jacobwilliams/fortran-csv-module/actions/workflows/CI.yml/badge.svg)
[![codecov](https://codecov.io/gh/jacobwilliams/fortran-csv-module/branch/master/graph/badge.svg?token=43HK33CSMY)](https://codecov.io/gh/jacobwilliams/fortran-csv-module)

### Description

A modern Fortran library for reading and writing CSV (comma-separated value) files.

### Latest Release

[![GitHub release](https://img.shields.io/github/release/jacobwilliams/fortran-csv-module.svg?style=plastic)](https://github.com/jacobwilliams/fortran-csv-module/releases/latest)

### Getting started
#### Get the code
```bash
git clone https://github.com/jacobwilliams/fortran-csv-module
cd fortran-csv-module
```
#### Dependencies
1. Git
2. [FoBis](https://github.com/szaghi/FoBiS), [fpm](https://github.com/fortran-lang/fpm), or [CMake](https://cmake.org)
3. [FORD](https://github.com/Fortran-FOSS-Programmers/ford) (optional)

#### Build with FoBis
You can build using provided `build.sh`:
```bash
./build.sh
```

#### Build with [fortran-lang/fpm](https://github.com/fortran-lang/fpm)
Fortran Package Manager (fpm) is a great package manager and build system for Fortran.
You can build using provided `fpm.toml`:
```bash
fpm build
```
To use `fortran-csv-module` within your fpm project, add the following to your `fpm.toml` file:
```toml
[dependencies]
fortran-csv-module = { git="https://github.com/jacobwilliams/fortran-csv-module.git" }
```

### Examples

Everything is handled by an object-oriented `csv_file` class. Here is an example for writing a file:

```fortran
program csv_write_test

use csv_module
use iso_fortran_env, only: wp => real64

implicit none

type(csv_file) :: f
logical :: status_ok

! open the file
call f%open('test.csv',n_cols=4,status_ok=status_ok)

! add header
call f%add(['x','y','z','t'])
call f%next_row()

! add some data:
call f%add([1.0_wp,2.0_wp,3.0_wp],real_fmt='(F5.3)')
call f%add(.true.)
call f%next_row()
call f%add([4.0_wp,5.0_wp,6.0_wp],real_fmt='(F5.3)')
call f%add(.false.)
call f%next_row()

! finished
call f%close(status_ok)

end program csv_write_test
```

Which produces the following file:
```
x,y,z,t
1.000,2.000,3.000,T
4.000,5.000,6.000,F
```

Real, integer, logical, or character data can be added as scalars, vectors, and matrices.

When reading a CSV file, the data is stored internally in the class as allocatable character strings, which can be retrieved as real, integer, logical or character vectors as necessary. For example, to get the `x`, `y`, `z`, and `t` vectors from the previously-generated file:

```fortran
program csv_read_test

use csv_module
use iso_fortran_env, only: wp => real64

implicit none

type(csv_file) :: f
character(len=30),dimension(:),allocatable :: header
real(wp),dimension(:),allocatable :: x,y,z
logical,dimension(:),allocatable :: t
logical :: status_ok
integer,dimension(:),allocatable :: itypes

! read the file
call f%read('test.csv',header_row=1,status_ok=status_ok)

! get the header and type info
call f%get_header(header,status_ok)
call f%variable_types(itypes,status_ok)

! get some data
call f%get(1,x,status_ok)
call f%get(2,y,status_ok)
call f%get(3,z,status_ok)
call f%get(4,t,status_ok)

! destroy the file
call f%destroy()

end program csv_read_test
```

Various options are user-selectable for specifying the format (e.g., changing the quote or delimiter characters). You can choose to enclose strings (or all fields) in quotes or not. The library works pretty well, and there are probably additional improvements that could be made. For one thing, it doesn't properly handle the case of a string that contains the delimiter character (I'll eventually fix this). If anybody has any other improvements, fork it and send me a pull request.

### License

This library is released under a [BSD-3 license](https://github.com/jacobwilliams/fortran-csv-module/blob/master/LICENSE).
