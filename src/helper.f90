
! Subroutine zum Pretty Printen einer REAL Matrix.
subroutine printMatrix(matrix, groesse)
  implicit none
  integer, INTENT(IN) :: groesse
  real, dimension(groesse,groesse), INTENT(IN) :: matrix
  integer :: i,j
  character(len=15) :: formatString


  write(formatString,fmt='(A1,I3,A7)') '(',groesse,'F30.22)'

  do i=1,groesse
    write(*,fmt=formatString) (matrix(i,j), j=1,groesse)
  end do

end subroutine printMatrix

subroutine writeMatrix(pfad, matrix, groesse)
  implicit none
  integer, INTENT(IN) :: groesse
  real, dimension(groesse,groesse), INTENT(IN) :: matrix
  integer :: i,j
  character(len=5) :: pfad
  character(len=15) :: formatString

  open(unit=100, file=pfad)

  write(formatString,fmt='(A1,I3,A7)') '(',groesse,'F30.22)'
  do i=1,groesse
    write(100,fmt=formatString) (matrix(i,j), j=1,groesse)
  end do

  close(100)
end subroutine writeMatrix
