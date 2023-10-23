!*****************************************************************************************
!>
!  Test of [[csv_utilities]].

program csv_test3

    use csv_utilities
    use iso_fortran_env, only: wp => real64

    implicit none

    integer,dimension(:),allocatable :: ivec

    write(*,*) ''
    write(*,*) '============================'
    write(*,*) ' csv_test_4 '
    write(*,*) '============================'
    write(*,*) ''

    allocate(ivec(10000))

    ivec = 0
    ivec(500) = 1

    call sort_ascending(ivec)
    if (.not. all(ivec(1:10000-1)==0)) error stop 'sort error'
    if (ivec(10000)/=1) error stop 'sort error'

    ivec = unique(ivec,chunk_size=10)
    if (.not. all (ivec == [0,1])) error stop 'unique error'

    write(*,*) 'PASSED'

  end program csv_test3
