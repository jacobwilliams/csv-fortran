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
call f%read('test_write.csv',header_row=1,status_ok=status_ok)

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

end program csv_read_test
