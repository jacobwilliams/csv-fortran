!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  Test of reading and writing CSV files.

    program csv_test

    use csv_module
    use iso_fortran_env, only: wp => real64

    implicit none

    call csv_test_1()
    call csv_write_test()
    call csv_read_test()

    contains

        subroutine csv_test_1()

        implicit none

        type(csv_file) :: f
        type(csv_file) :: f2
        integer :: i !! counter
        integer :: k !! counter
        character(len=30),dimension(:),allocatable :: header  !! the header
        character(len=30),dimension(:,:),allocatable :: csv_data  !! the data from the file as strings
        real(wp),dimension(:),allocatable :: x  !! for getting a real vector from a csv file
        logical :: status_ok  !! error flag
        integer,dimension(:),allocatable :: itypes  !! array of variable types in the file
        integer :: ifile !! file counter
        character(len=30),dimension(:),allocatable :: names
        character(len=:),allocatable :: file

        character(len=*),dimension(2),parameter :: files_to_test = ['test.csv          ',&
                                                                    'test_2_columns.csv']
        character(len=*),dimension(4),parameter :: dirs_to_try = ['../files/', &
                                                                  './files/ ', &
                                                                  './       ', &
                                                                  '         ']

        write(*,*) ''
        write(*,*) '============================'
        write(*,*) ' csv_test_1 '
        write(*,*) '============================'
        write(*,*) ''

        do ifile = 1, size(files_to_test)

            ! a hack to get it to work with all the different build systems
            ! no matter the working directory
            do k = 1, size(dirs_to_try)
                file = trim(dirs_to_try(k))//trim(files_to_test(ifile))
                if (file_exists(file)) exit ! found it
            end do
            write(*,*) 'read file: '//trim(file)

            ! read the file:
            if (ifile==1) then
                call f%read(file,header_row=1,status_ok=status_ok)
            else
                ! also skip a row
                call f%read(file,header_row=1,skip_rows=[2],status_ok=status_ok)
            end if

            if (.not. status_ok) then
                error stop 'could not open file'
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

        end subroutine csv_test_1

        subroutine csv_write_test()

        implicit none

        type(csv_file) :: f
        logical :: status_ok

        write(*,*) ''
        write(*,*) '============================'
        write(*,*) ' csv_write_test '
        write(*,*) '============================'
        write(*,*) ''

        ! open the file
        call f%open('test_write.csv',n_cols=4,status_ok=status_ok)
        if (status_ok) then

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

        else
            error stop 'could not open file'
        end if

        end subroutine csv_write_test

        subroutine csv_read_test()

        implicit none

        type(csv_file) :: f
        character(len=30),dimension(:),allocatable :: header
        real(wp),dimension(:),allocatable :: x,y,z
        logical,dimension(:),allocatable :: t
        logical :: status_ok
        integer,dimension(:),allocatable :: itypes

        write(*,*) ''
        write(*,*) '============================'
        write(*,*) ' csv_read_test '
        write(*,*) '============================'
        write(*,*) ''

        ! read the file
        call f%read('test_write.csv',header_row=1,status_ok=status_ok)

        if (status_ok) then

            ! get the header and type info
            call f%get_header(header,status_ok)
            call f%variable_types(itypes,status_ok)

            ! get some data
            call f%get(1,x,status_ok)
            call f%get(2,y,status_ok)
            call f%get(3,z,status_ok)
            call f%get(4,t,status_ok)

            write(*,*) 'x=',x
            write(*,*) 'y=',y
            write(*,*) 'z=',z
            write(*,*) 't=',t

            ! destroy the file
            call f%destroy()

        else
            error stop 'could not open file'
        end if

        end subroutine csv_read_test

        function file_exists(file) result(exists)

        !! returns True if the file exists

        implicit none
        character(len=*),intent(in) :: file
        logical :: exists

        integer :: istat

        inquire(file=file,exist=exists,iostat=istat)

        exists = exists .and. istat==0 ! just in case?

        end function file_exists

    end program csv_test
!*****************************************************************************************
