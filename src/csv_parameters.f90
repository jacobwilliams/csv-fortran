!*******************************************************************************
!>
!  Various parameters.

    module csv_parameters

    use csv_kinds

    private

    integer(ip),parameter,public :: max_real_str_len = 27 !! maximum string length of a real number
    character(len=*),parameter,public :: default_real_fmt = '(E27.17E4)'
        !! default real number format statement (for writing real values to strings and files).

    integer(ip),parameter,public :: max_integer_str_len = 256 !! maximum string length of an integer.
    character(len=*),parameter,public :: default_int_fmt  = '(I256)'
        !! default integer number format statement (for writing real values to strings and files).

    end module csv_parameters
!*******************************************************************************
