subroutine lousungGLS
  use kenngroessen
  implicit none

  VektorKnotenverschiebungen = 0

  call gausz((nk-nf)*3,Gesamtsteifigkeitsmatrix, &
            VektorKnotenlasten,VektorKnotenverschiebungen)

end subroutine lousungGLS
