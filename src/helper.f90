
! Subroutine zum Pretty Printen einer REAL Matrix.
subroutine printMatrix(matrix, groesse)
  implicit none
  integer, INTENT(IN) :: groesse
  real, dimension(groesse,groesse), INTENT(IN) :: matrix
  integer :: i,j
  character(len=9) :: formatString

  write(formatString,fmt='(A1,I2,A5)') '(',groesse,'F8.3)'
  do i=1,groesse
    write(*,fmt=formatString) (matrix(i,j), j=1,groesse)
  end do

end subroutine printMatrix
