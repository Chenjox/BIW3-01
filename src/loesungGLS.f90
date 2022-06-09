subroutine lousungGLS
  use kenngroessen
  implicit none

  VektorKnotenverschiebungen = 0

  write(unit=*, fmt=*) 'Jetzt wird das GLS gelöst.'

  call gausz((nk-nf)*3,Gesamtsteifigkeitsmatrix, &
            VektorKnotenlasten,VektorKnotenverschiebungen)

end subroutine lousungGLS
