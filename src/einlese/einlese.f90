
subroutine einlesen(filepath)
  implicit none
  use stabkenngroessen

  character(len = 80) :: filepath

  integer :: ios
  integer :: nKnoten
  integer :: nStaebe
  integer :: nStuetzKnoten
  real    :: Emodul
  real, allocatable    :: PunktKoordinaten(:,:) ! Feld mit 2 x nKnoten einträgen
  type(stab), allocatable :: Staebe(:) ! Feld mit Stabeinträgen
  integer :: i,k

  open(unit=10, file=filepath, iostat=ios, status="old",action="read")
  if ( ios /= 0 ) stop "Error opening file filepath"

  read(unit=10, fmt=*) nKnoten, nStaebe
  read(unit=10, fmt=*) nStuetzKnoten
  read(unit=10, fmt=*) Emodul

  allocate(PunktKoordinaten(2,nKnoten))

  do k = 1, nKnoten
    ! Einlesen der Koordinaten der Knoten.
    ! Der Erste Index ist jeweils die X und Y Koordinate
    ! Der Zweite Index ist die Nummer des Knotens
    ! Die letzten nStuetzKnoten sind die Auflagerknoten!
    read(unit=10, fmt=*) PunktKoordinaten(1,k),PunktKoordinaten(2,k)
  end do

  ! Jetzt kommen die Stäbe
  allocate(Staebe(nStaebe))
  do i = 1, nStaebe
    ! Stab-Nr.  Nr.Anfangsknoten  Nr.Endknoten  A[m2]  I[m4]  alpha[rad]  Art
    read(unit=10, fmt=0) k, Staebe(i)%vonKnoten, Staebe(i)%zuKnoten, Staebe(i)%Area, Staebe(i)%FTM, Staebe(i)%alpha, Staebe(i)%KnotenUnstetigkeiten
  end do

  write(*, fmt="(2I3.2)") nKnoten, nStaebe
  do k = 1, nKnoten
    write(unit=*, fmt=*) PunktKoordinaten(1,k),PunktKoordinaten(2,k)
  end do


end subroutine einlesen
