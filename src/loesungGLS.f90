subroutine lousungGLS
  use kenngroessen
  implicit none
  real*8, allocatable :: GesamtsteifigkeitsmatrixMitStreichungen(:,:)
  real*8, allocatable :: VektorKnotenlastenMitStreichungen(:)
  real*8, allocatable :: VektorRandschnittkraefteMitStreichungen(:)
  real*8, allocatable :: VektorKnotenverschiebungenMitStreichungen(:)

  real*8, allocatable :: streichMatrix1(:)
  real*8, allocatable :: streichMatrix2(:)

  real*8, allocatable :: streichVektor1(:)
  real*8, allocatable :: streichVektor2(:)

  integer :: NMatrix1, NMatrix2, NVektor1, NVektor2

  integer             :: i,j
  integer             :: AnzahlStreichungen
  integer, allocatable:: OrteStreichungen(:)

  AnzahlStreichungen = 0

  ! Anzahl der Streichungen herausfinden
  do i = 1, 20
    if(int(Belastungsmatrix(i,1)).eq.0) then
      exit
    else if(int(Belastungsmatrix(i,2)).eq.1) then
      AnzahlStreichungen = AnzahlStreichungen+1
    end if
  end do

  allocate(OrteStreichungen(AnzahlStreichungen))
  OrteStreichungen = 0
  k = 1
  do i = 1, 20
    if(int(Belastungsmatrix(i,1)).eq.0) then
      exit
    else if(int(Belastungsmatrix(i,2)).eq.1) then
      do j = 1, 3
        if(Belastungsmatrix(i,2+j).ne.0.0) then
          OrteStreichungen(k) = (int(Belastungsmatrix(i,1))*3-2)+j-1
          k = k + 1
        end if
      end do
    end if
  end do

  ! Hier streichen wir die Zeilen aus der Matrix, und die Einträge in den Vektoren
  ! Jetzt Wissen wir wie groß die einzelnen Sachen sein müssen.
  !if(AnzahlStreichungen.ne.0) then
  !  do i = 1, AnzahlStreichungen
  !
  !  end do
  !end if
  !TODO Streichungsalgorithmus implementieren




  VektorKnotenverschiebungen = 0

  write(unit=*, fmt=*) 'Jetzt wird das GLS gelöst.'

  call gausz((nk-nf)*3,Gesamtsteifigkeitsmatrix, &
            VektorKnotenlasten-VektorRandschnittkraefte,VektorKnotenverschiebungen)

end subroutine lousungGLS
