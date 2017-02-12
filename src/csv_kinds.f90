!*******************************************************************************
!>
!  Numeric kinds.

    module csv_kinds

    use iso_fortran_env, only: real64,int32

    private

    integer,parameter,public :: wp = real64  !! default real kind
    integer,parameter,public :: ip = int32   !! default integer kind

    end module csv_kinds
!*******************************************************************************
