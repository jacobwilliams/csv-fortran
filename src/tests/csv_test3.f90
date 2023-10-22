program csv_test3

use csv_module
use iso_fortran_env, only: wp => real64

implicit none

type(csv_file) :: f
logical :: status_ok
character(len=30),dimension(:),allocatable :: header, col1
integer :: i
character(len=100) :: tmp_str

! set optional inputs:
call f%initialize(verbose = .true.)

! open the file
call f%open('test.csv',n_cols=4,status_ok=status_ok)

! add header
call f%add(['x','y','z','t'])
call f%next_row()

! add some data:
call f%add([1.0_wp,2.0_wp,3.0_wp],real_fmt='(F5.3)')
call f%add(.true.)
call f%next_row()
call f%add([2.0_wp,5.0_wp,6.0_wp],real_fmt='(F5.3)')
call f%add(.false.)
call f%next_row()
call f%add([3.0_wp,5.0_wp,6.0_wp],real_fmt='(F5.3)')
call f%add(.false.)
call f%next_row()
call f%add([4.0_wp,5.0_wp,6.0_wp],real_fmt='(F5.3)')
call f%add(.false.)
call f%next_row()
call f%add([5.0_wp,5.0_wp,6.0_wp],real_fmt='(F5.3)')
call f%add(.false.)
call f%next_row()

! finished
call f%close(status_ok)
if (.not. status_ok) error stop 'error closing file'

! read the file
call f%read('test.csv',header_row=1,status_ok=status_ok)
if (.not. status_ok) error stop 'error reading file'

! get the header and type info
call f%get_header(header,status_ok)

print "(*(g0))", "HEADER:"
do i = 1, size(header)
  print "(*(g0))", ">"//trim(header(i))//"<"
end do
if (.not. all(header == ['x','y','z','t'])) error stop 'error reading header'

call f%get(1,col1,status_ok)
print "(*(g0))", "col1:"
do i = 1, size(col1)
  print "(*(g0))", ">",trim(col1(i)),"<"
end do

do i = 1, 5
  write(tmp_str,'(F5.3)') real(i,wp)
  if (col1(i) /= tmp_str) error stop 'error converting cell to string:'//tmp_str
end do

! destroy the file
call f%destroy()

end program csv_test3
