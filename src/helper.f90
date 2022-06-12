
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
  real*8, dimension(groesse,groesse), INTENT(IN) :: matrix
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

subroutine UnterMatrix(A,n,z,s,Unter)
  implicit none
  integer :: n,z,s ! groesse, zeile und spalte die weggelassen werden
  real*8    :: A(n,n)
  real*8   ,INTENT(OUT):: Unter(n-1,n-1)
  integer :: i,j

  zeilen: do i = 1, n
    if ( i.eq.z ) then ! Die Zeile mit Index i wird ignoriert
      cycle zeilen
    end if
    spalten: do j = 1, n
      if ( j.eq.s ) then ! Die Spalte mit index s wird ignoriert
        cycle spalten
      else if ( j.gt.s ) then ! Ist j größer als s dann müssen wir immer eins von j abziehen
        if(i.gt.z) then
          Unter(i-1,j-1) = A(i,j) !wevon j>s und i>z von beiden 1 abziehen
        else
          Unter(i,j-1) = A(i,j) !wenn nur j>s nur von j (spalten) 1 abziehen
        end if
      else ! j ist kleiner als s
        if(i.gt.z) then
          Unter(i-1,j) = A(i,j) !wenn nur i>z nur von i (zeilen) 1 abziehen
        else
          Unter(i,j) = A(i,j)
        end if ! wenn beides kleiner oder gleich nichts abziehen
      end if
    end do spalten
  end do zeilen

end subroutine UnterMatrix

subroutine UnterVektor(v,n,z,Unter)
  implicit none
  integer :: n,z ! groesse, zeile und spalte die weggelassen werden
  real*8    :: v(n)
  real*8   ,INTENT(OUT):: Unter(n-1)
  integer :: i,j

  do i = 1, n-1
    if(i.lt.z) then
      Unter(i) = v(i)
    else
      Unter(i) = v(i+1)
    end if
  end do
end subroutine UnterVektor

subroutine StreichVektor(V,n,streich, streichAnzahl, K)
  implicit none
  integer ,INTENT(IN):: n, streichAnzahl
  integer ,INTENT(IN):: streich(streichAnzahl)
  real*8  ,INTENT(IN):: V(n)
  real*8, INTENT(OUT) :: K(n-streichAnzahl)
  integer :: i,g
  integer :: gestrichenI ! Anzahl der bereits gestrichenen Zeilen

  gestrichenI = 0
  K = 0.0

  do i = 1, n - streichAnzahl
    do g = 1, streichAnzahl
      if ( (i+gestrichenI).eq.streich(g) ) then
        gestrichenI = gestrichenI + 1
      end if
    end do
    K(i) = V(i + gestrichenI)
  end do

end subroutine StreichVektor

! Umkehrfunktion von StreichVektor
subroutine InsertVektor(V,n,hinzu,hinzuAnzahl, K)
  integer ,INTENT(IN):: n, hinzuAnzahl
  integer ,INTENT(IN):: hinzu(hinzuAnzahl)
  real*8  ,INTENT(IN):: V(n-hinzuAnzahl)
  real*8, INTENT(OUT) :: K(n)
  integer :: i,g
  integer :: hinzuI ! Anzahl der bereits gestrichenen Zeilen

  hinzuI = 0
  K = 0.0

  do i = 1, n
    do g = 1, hinzuAnzahl
      if ( (i+gestrichenI).eq.hinzu(g) ) then
        hinzuI = hinzuI + 1
      end if
    end do
    K(i) = V(i - hinzuI)
  end do
  ! Die hinzugefügten einträge sind mit 0.0 gefüllt
end subroutine InsertVektor

subroutine StreichMatrix(M, n, streich, streichAnzahl,K)
  implicit none
  integer ,INTENT(IN):: n, streichAnzahl
  integer ,INTENT(IN):: streich(streichAnzahl)
  real*8  ,INTENT(IN):: M(n,n)
  real*8, INTENT(OUT) :: K(n-streichAnzahl,n-streichAnzahl)
  integer :: i,j,g
  integer :: gestrichenI,gestrichenJ ! Anzahl der bereits gestrichenen Zeilen/Spalten

  gestrichenI = 0
  gestrichenJ = 0
  K = 0.0


  ! Wie bestimmen wir ob eine Zeile/Spalte gestrichen werden muss?
  ! Richtig, wir lesen jedes mal streich durch.

  do i = 1, n-streichAnzahl
    do g = 1, streichAnzahl
      if((i+gestrichenI).eq.streich(g)) then
        gestrichenI = gestrichenI + 1
      end if
    end do
    do j = 1, n-streichAnzahl
      do g = 1, streichAnzahl
        if((j+gestrichenJ).eq.streich(g)) then
          gestrichenJ = gestrichenJ + 1
        end if
      end do
      K(i, j) = M(i + gestrichenI, j + gestrichenJ)
    end do
    gestrichenJ = 0
  end do

end subroutine StreichMatrix
