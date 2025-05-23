!*******************************************************************************
!>
!  Various parameters.

    module csv_parameters

    use csv_kinds

    private

    integer(ip),parameter,public :: max_real_str_len = 256 !! maximum string length of a real number

    character(len=*),parameter,public :: default_sp_fmt = '(E17.8E3)'  !! default single number format statement
    character(len=*),parameter,public :: default_wp_fmt = '(E27.17E4)' !! default double number format statement 
    character(len=*),parameter,public :: default_qp_fmt = '(E46.35E5)' !! default quad number format statement

    integer(ip),parameter,public :: max_integer_str_len = 256 !! maximum string length of an integer.
    character(len=*),parameter,public :: default_int_fmt  = '(I256)'
        !! default integer number format statement (for writing integer values to strings and files).

    end module csv_parameters
!*******************************************************************************
