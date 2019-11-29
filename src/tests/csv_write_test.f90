program csv_write_test

use csv_module
use iso_fortran_env, only: wp => real64

implicit none

type(csv_file) :: f
logical :: status_ok
real(wp),dimension(3),parameter :: xvec = [1.0_wp, 2.0_wp, 3.0_wp]
integer,dimension(4),parameter :: ivec = [1,2,3,4]
logical,dimension(4),parameter :: lvec = [.true.,.true.,.true.,.true.]

! open the file
call f%open('test.csv',n_cols=4,status_ok=status_ok)

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
call f%add(xvec,real_fmt='(F5.3)')
call f%add(.true.)
call f%next_row()
call f%add([1,2,3,4])
call f%next_row()
call f%add(ivec)
call f%next_row()
call f%add([.true.,.true.,.true.,.true.])
call f%next_row()
call f%add(lvec)
call f%next_row()

! finished
call f%close(status_ok)

end program csv_write_test
