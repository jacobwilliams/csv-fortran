!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  Test of reading and writing CSV files.

    program csv_test

    use csv_module
    use kinds_module

    implicit none

    type(csv_file) :: f
    integer :: i !! counter
    character(len=30),dimension(:),allocatable :: header  !! the header
    character(len=30),dimension(:,:),allocatable :: csv_data  !! the data from the file as strings

    real(wp),dimension(:),allocatable :: x
    real(wp),dimension(:),allocatable :: y
    real(wp),dimension(:),allocatable :: z
    logical :: status_ok
    integer,dimension(:),allocatable :: itypes

    ! read the file:
    call f%read('test.csv',header_row=1,status_ok=status_ok)

    ! print the header and type info:
    call f%get_header(header,status_ok)
    call f%variable_types(itypes,status_ok)
    write(*,*) ''
    write(*,'(*(A30,1X,A4))') 'Header', 'Type'
    do i=1,size(header)
        write(*,'(*(A30,1X,I4))') header(i), itypes(i)
    end do

    write(*,*) ''
    write(*,*) 'print all the rows:'

    call f%get(csv_data,status_ok)
    do i=1,size(csv_data,1)
        write(*,'(*(A30,1X))') csv_data(i,:)
    end do

    write(*,*) ''
    write(*,*) 'get the position vectors:'

    write(*,*) ''
    write(*,*) 'x:'
    call f%get(2,x,status_ok)
    write(*,'(F27.16,1x)',advance='NO') x
    write(*,*) ''
    write(*,*) 'y:'
    call f%get(3,y,status_ok)
    write(*,'(F27.16,1x)',advance='NO') y
    write(*,*) ''
    write(*,*) 'z:'
    call f%get(4,z,status_ok)
    write(*,'(F27.16,1x)',advance='NO') z
    write(*,*) ''

    end program csv_test
!*****************************************************************************************
