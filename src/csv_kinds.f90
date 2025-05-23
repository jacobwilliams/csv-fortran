!*******************************************************************************
!>
!  Numeric kinds.

    module csv_kinds

    use iso_fortran_env, only: real128,real64,real32,int32

    private

    integer,parameter,public :: qp = real128 !! quad real kind
    integer,parameter,public :: wp = real64  !! default real kind
    integer,parameter,public :: sp = real32  !! additional real kind, single precision
    integer,parameter,public :: ip = int32   !! default integer kind

    end module csv_kinds
!*******************************************************************************
