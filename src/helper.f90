
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

! Subroutine zum Befüllen einer Groessen Matrix mit einer kleineren Matrix
! Die einzelnen Elemente der Matrix werden addiert
! Es wird ANGENOMMEN, dass die kleine Matrix eine 3x3 Matrix ist.
subroutine fillIntoMatrix(gross, klein, groesseGross, spalteGross, zeileGross)
  implicit none
  real, dimension(groesseGros,groesseGros), INTENT(OUT) :: gross
  real, dimension(3,3), INTENT(IN) :: klein
  integer, INTENT(IN) :: groesseGros
  integer, INTENT(IN) :: spalteGross
  integer, INTENT(IN) :: zeileGross

  integer :: i,j,starti, startj

  ! Errechnen des Indezes in der Großen Matrix
  starti = spalteGross*3-2
  startj = zeileGross*3-2

  ! Befüllen der Großen Matrix mit der kleinen Matrix
  do i=0,2
    do j=0,2
      ! Wichtig, wir wollen ja die Elemente addieren!
      gross(starti+i,startj+j) = gross(starti+i,startj+j) + klein(i+1,j+1)
    end do
  end do
end subroutine fillIntoMatrix
