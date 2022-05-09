
subroutine systemEinlesen(ioUnit, nKnoten, nStaebe,system)
  use kenngroessen
  implicit none

  integer :: ioUnit ! Hier ist die ioUnit
  integer :: nKnoten
  integer :: nStaebe
  integer :: ios
  integer :: nStuetzKnoten
  real    :: Emodul
  type(system2D(AnzahlKnoten,AnzahlStaebe)) :: system
  integer :: i,k

  read(unit=ioUnit, fmt=*) nStuetzKnoten
  read(unit=ioUnit, fmt=*) Emodul

  !allocate(system2D(nKnoten, nStaebe) :: system)

  do k = 1, nKnoten
    ! Einlesen der Koordinaten der Knoten.
    ! Der Erste Index ist jeweils die X und Y Koordinate
    ! Der Zweite Index ist die Nummer des Knotens
    ! Die letzten nStuetzKnoten sind die Auflagerknoten!
    read(unit=ioUnit, fmt=*) system%KnotenKoordinaten(1,k), & !Zeile zu lang...
                         system%KnotenKoordinaten(2,k)
  end do

  ! Jetzt kommen die Stäbe
  ! allocate(Staebe(nStaebe))
  do i = 1, nStaebe
    ! Stab-Nr.  Nr.Anfangsknoten  Nr.Endknoten  A[m2]  I[m4]  alpha[rad]  Art
    read(unit=ioUnit, fmt=*) k, system%staebe(i)%vonKnoten, &
                            system%staebe(i)%zuKnoten, &
                            system%staebe(i)%Area, &
                            system%staebe(i)%FTM, &
                            system%staebe(i)%alpha, &
                            system%staebe(i)%KnotenUnstetigkeiten
    ! Die Stabnummer ist der Index des Arrays, ich gehe davon aus, dass die nicht von dem ganzen anderem Abweicht
  end do

  ! Und abschließend die Einwirkungen
  ! Am Ende des Tages sind dies zwei Vektoren der Größe AnzahlKnoten*3 mit 0.0 und Unbekannten, hier mit NaN bezeichnet
  system%VektorKnotenlasten=0.0
  system%VektorVerschiebungen=0.0

  ! Wir lesen bis zum Ende der Datei, und brechen im richtigen Durchlauf ab
  reading: do
    ! Zuerst die eventuell vorhandene Knotennummer
    read(unit=ioUnit, iostat=ios, fmt="(I5.2)", advance="no") k
    if ( ios.ne.0 ) then ! Sind wir beim EOF angekommen kommt in ios ein Wert ungleich 0 raus.
      exit reading
    else
      !Knotennummer ist tatsächlich vorhanden also den Rest einlesen
      read(unit=ioUnit, fmt="(I5.2)", advance="no") i ! i ist hierbei die Art der Belastung
      if ( i.eq.0 ) then
        read(unit=ioUnit, fmt=*) system%VektorKnotenlasten(3*k), &
                             system%VektorKnotenlasten(3*k+1), &
                             system%VektorKnotenlasten(3*k+2)
      else
        read(unit=ioUnit, fmt=*) system%VektorVerschiebungen(3*k), &
                             system%VektorVerschiebungen(3*k+1), &
                             system%VektorVerschiebungen(3*k+2)
      end if
    end if
  end do reading


end subroutine systemEinlesen
