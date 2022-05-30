!Grundidee ist dass eine 3x3 Matrix in einer größere Matrix einsortiert werden kann
! In welche Matrix wird es einsortiert?
subroutine matrixfuell(ki,kj)
!Eingabegroessen: gewünschte Zeile in Gesamtsteifigkeitsmatrix, gewünschte Spalte in Gesamtsteifigkeitsmatrix

  use kenngroessen                                        !Definierte Datentypen laden
  implicit none

  integer i, j, f, ki, kj, starti, startj
  real, dimension(3,3) :: smallMatrix

  allocate(Gesamtsteifigkeitsmatrix((nk-nf)*3,(nk-nf)*3))  !Dimension = AnzKnoten- AnzStützknoten = AnzfreieKnoten

  smallMatrix = 1
  Gesamtsteifigkeitsmatrix = 0

  !kleine Matrix in die große einfügen, wobei ki und kj den Ort in der großen Matrix angeben
  call fillIntoMatrix(Gesamtsteifigkeitsmatrix, smallMatrix, (nk-nf)*3, 3, 4)


  call printMatrix(Gesamtsteifigkeitsmatrix, (nk-nf)*3)

end subroutine

! Subroutine zum Befüllen einer Groessen Matrix mit einer kleineren Matrix
! Die einzelnen Elemente der Matrix werden addiert
! Es wird ANGENOMMEN, dass die kleine Matrix eine 3x3 Matrix ist.
subroutine fillIntoMatrix(gross, klein, groesseGros, spalteGross, zeileGross)
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
