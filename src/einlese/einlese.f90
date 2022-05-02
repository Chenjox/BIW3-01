
subroutine einlesen(filepath)
  use kenngroessen
  implicit none


  character(len = 80) :: filepath

  integer :: ios
  integer :: nKnoten
  integer :: nStaebe
  integer :: nStuetzKnoten
  real    :: Emodul
  real    :: Knotenlasten(:)
  real    :: Knotenverschiebungen(:)
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
    read(unit=10, fmt=*) k, Staebe(i)%vonKnoten, Staebe(i)%zuKnoten, Staebe(i)%Area, Staebe(i)%FTM, Staebe(i)%alpha, Staebe(i)%KnotenUnstetigkeiten
  end do

  ! Und abschließend die Einwirkungen
  ! Am Ende des Tages sind dies zwei Vektoren der Größe AnzahlKnoten*3 mit 0.0 und Unbekannten, hier mit NaN bezeichnet
  allocate(Knotenlasten(3*nKnoten))
  allocate(Knotenverschiebungen(3*nKnoten))

  Knotenlasten=0.0
  Knotenverschiebungen=0.0

  ! Wir lesen bis zum Ende der Datei, und brechen im richtigen Durchlauf ab
  reading: do
    ! Zuerst die eventuell vorhandene Knotennummer
    read(unit=10, iostat=ios, fmt=*, advance="no") k
    if ( ios.neq.0 ) then ! Sind wir beim EOF angekommen kommt in ios ein Wert ungleich 0 raus.
      exit reading
    else
      !Knotennummer ist tatsächlich vorhanden also den Rest einlesen
      read(unit=10, fmt=*, advance="no") i ! i ist hierbei die Art der Belastung
      if ( i.eq.0 ) then
        read(unit=10, fmt=*) Knotenlasten(3*Knotennummer),Knotenlasten(3*Knotennummer+1),Knotenlasten(3*Knotennummer+2)
      else
        read(unit=10, fmt=*) Knotenverschiebung(3*Knotennummer), Knotenverschiebung(3*Knotennummer+1), Knotenverschiebung(3*Knotennummer+2)
      end if
    end if
  end do reading

  write(*, fmt="(2I3.2)") nKnoten, nStaebe
  do k = 1, nKnoten
    write(unit=*, fmt=*) PunktKoordinaten(1,k),PunktKoordinaten(2,k)
  end do

  close(unit=10)

end subroutine einlesen
