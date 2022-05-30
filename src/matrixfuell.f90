!Grundidee ist dass eine 3x3 Matrix in einer größere Matrix einsortiert werden kann

subroutine matrixfuell(ki,kj)
!Eingabegroessen: gewünschte Zeile in Gesamtsteifigkeitsmatrix, gewünschte Spalte in Gesamtsteifigkeitsmatrix

  use kenngroessen                                        !Definierte Datentypen laden
  implicit none

  integer, parameter :: x=18                               !NUR AUSGABE

  !Variablendeklarationen
  integer i, j, f, ki, kj, starti, startj

  integer N                                               !NUR AUSGABE
  real, dimension(x,x) :: c                               !NUR AUSGABE
  character(len=20) :: exFmt                              !NUR AUSGABE

  !f=0                                                    !für Testmatrix

  allocate(Gesamtsteifigkeitsmatrix((nk-nf)*3,(nk-nf)*3))  !Dimension = AnzKnoten- AnzStützknoten = AnzfreieKnoten

  Gesamtsteifigkeitsmatrix = 0

  !kleine Matrix in die große einfügen, wobei ki und kj den Ort in der großen Matrix angeben
  starti = ki*3-2
  startj = kj*3-2

  do i=0,2
    do j=0,2
      Gesamtsteifigkeitsmatrix(starti+i,startj+j) = Stabsteifigkeitsmatrix(i+1,j+1)
    end do
  end do


  c = 1.0                                                   !NUR AUSGABE
  write(exFmt,'("(",I0,"(F8.3))")') x                       !NUR AUSGABE
  do i=1,x                                                  !NUR AUSGABE
    write(*,exFmt) (Gesamtsteifigkeitsmatrix(i,j), j=1,x)  !NUR AUSGABE
  end do                                                    !NUR AUSGABE

end subroutine
