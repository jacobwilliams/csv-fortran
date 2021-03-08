!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  Test of reading and writing CSV files.

    program csv_test

    use csv_module
    use iso_fortran_env, only: wp => real64

    implicit none

    type(csv_file) :: f
    type(csv_file) :: f2
    integer :: i !! counter
    character(len=30),dimension(:),allocatable :: header  !! the header
    character(len=30),dimension(:,:),allocatable :: csv_data  !! the data from the file as strings
    real(wp),dimension(:),allocatable :: x  !! for getting a real vector from a csv file
    logical :: status_ok  !! error flag
    integer,dimension(:),allocatable :: itypes  !! array of variable types in the file
    integer :: ifile !! file counter
    character(len=30),dimension(:),allocatable :: names

    character(len=*),dimension(2),parameter :: files_to_test = ['test.csv          ',&
                                                                'test_2_columns.csv']

    do ifile = 1, size(files_to_test)

        ! read the file:
        if (ifile==1) then
            call f%read(trim(files_to_test(ifile)),&
                        header_row=1,status_ok=status_ok)
        else
            ! also skip a row
            call f%read(trim(files_to_test(ifile)),&
                        header_row=1,skip_rows=[2],status_ok=status_ok)
        end if

        write(*,*) ''
        write(*,*) 'File: '//trim(files_to_test(ifile))
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
        write(*,*) 'get some vectors:'
        if (ifile==1) then
            write(*,*) ''
            write(*,*) 'age:'
            call f%get(3,x,status_ok)
            write(*,'(F6.3,1x)',advance='NO') x
            write(*,*) ''
        else
            write(*,*) ''
            write(*,*) 'name:'
            call f%get(2,names,status_ok)
            write(*,'(A10,1x)',advance='NO') names
            write(*,*) ''
        end if

    end do

    ! now test creating a CSV:
    call f2%initialize(enclose_strings_in_quotes=.false.,verbose=.true.)
    call f2%open('test2.csv',n_cols=4,status_ok=status_ok)
    if (status_ok) then
        call f2%add(['x','y','z','t'])    ! add header as vector
        call f2%next_row()
        call f2%add(1.0_wp)  ! add as scalars
        call f2%add(2.0_wp)
        call f2%add(3.0_wp)
        call f2%add(.true.)
        call f2%next_row()
        call f2%add([4.0_wp,5.0_wp,6.0_wp],real_fmt='(F5.3)') ! add as vectors
        call f2%add(.false.)
        call f2%next_row()
    end if
    call f2%close(status_ok)

    end program csv_test
!*****************************************************************************************
