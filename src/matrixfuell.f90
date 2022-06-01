! Subroutine zum Befüllen einer Groessen Matrix mit einer kleineren Matrix
! Die einzelnen Elemente der Matrix werden addiert
! Es wird ANGENOMMEN, dass die kleine Matrix eine 3x3 Matrix ist.
subroutine fillIntoMatrix(gross, klein, groesseGross, spalteGross, zeileGross)
  implicit none
  real, dimension(groesseGross,groesseGross), INTENT(OUT) :: gross
  real, dimension(3,3), INTENT(IN) :: klein
  integer, INTENT(IN) :: groesseGross
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
