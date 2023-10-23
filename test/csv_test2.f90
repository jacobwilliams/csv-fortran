

!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  Test of reading a CSV file.

    program csv_test2

    use csv_module
    use iso_fortran_env, only: wp => real64, sp => real32

    implicit none

    type(csv_file) :: file
    logical        :: status_ok

    write(*,*) ''
    write(*,*) '============================'
    write(*,*) ' csv_test_2 '
    write(*,*) '============================'
    write(*,*) ''

    call file%read('files/test2.csv',header_row=1,status_ok=status_ok)
    write(*,*) 'status_ok = ', status_ok

end program csv_test2