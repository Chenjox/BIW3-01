!Grundidee ist dass eine 3x3 Matrix in einer größere Matrix einsortiert werden kann

subroutine matrixfuell

use kenngroessen
implicit none
integer, parameter :: x=18
integer i, j, f, ki, kj, N, starti, startj
real, dimension(x,x) :: c
character(len=20) :: exFmt

f=0

allocate(Gesamtsteifigkeitsmatrix((nk-nf)*3,(nk-nf)*3))  !Dimension = AnzKnoten- AnzStützknoten = AnzfreieKnoten

Gesamtsteifigkeitsmatrix = 0

!Test 3x3 Matrix befüllen
do i=1,3                              !Zeilen
  do j=1,3                            !Spalten
      f=f+1
      Stabsteifigkeitsmatrix(i,j) = f
  end do
end do

write(*,*) Stabsteifigkeitsmatrix

ki=3                                  !große Zeile Gesamtmatrix
kj=3                                  !große Zeile Gesamtmatrix

!kleine Matrix in die große einfügen, wobei ki und kj den Ort angeben

starti = ki*3-2
startj = kj*3-2

do i=0,2
  do j=0,2
    Gesamtsteifigkeitsmatrix(starti+i,startj+j) = Stabsteifigkeitsmatrix(i+1,j+1)
  end do
end do


  c = 1.0
  write(exFmt,'("(",I0,"(F8.3))")') x
  do i=1,x
     write(*,exFmt) (Gesamtsteifigkeitsmatrix(i,j), j=1,x)
  end do

end subroutine
