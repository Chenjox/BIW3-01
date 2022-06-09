
module kenngroessen

  implicit none

  real*8, allocatable :: Gesamtsteifigkeitsmatrix(:,:)
  real*8, allocatable :: VektorKnotenlasten(:)
  real*8, allocatable :: VektorKnotenverschiebungen(:)

!-------------------------------------------------------------------------------
!Belastungsmatrix
!FÜR ART 0 (KRAFT/MOMENT): (NummerKnoten, Art, P1, P2, M3) bzw P in x1, P in x2, M in Phi
!FÜR ART 1 (VERSCHIEBUNG): (NummerKnoten, Art, Verschiebung in x, Verschiebung in y, Verdrehung Phi)
  real*8 :: Belastungsmatrix(20,5)

  type stab                                   !definieren eines komplexen Datentypen Stab
    integer :: NrStab, K1, K2                 !Stabnummer, Knoten1, Knoten2, Stabnummer
    real*8 :: E,A,I, alpha                      !E-Modul,QSFläche, Flächenträgheitsmoment, Winkel Alpha
    integer :: art                            !Art(Unstetigkeit)
  end type stab

  !type(stab) :: staebe(1:100)
  !-----------------------------------------------------------------------------
  !Deklarieren unserer Variablen
  integer nk, ns                                !deklarieren von Knotenanzahl, Stabanzahl
  integer nf                                    !deklarieren von der Anzahl der Stützknoten
  real*8 E
  integer              :: AnzBelastung          !Anzahl der Belastungen
  real*8, allocatable :: koordinatenmatrix(:,:)   ! Erster Index x bzw. y, zweiter index = Knotenindex
  type(stab), allocatable :: staebe(:)              !Anzahl der Stäbe
  !-----------------------------------------------------------------------------
  !Initialiseren unserer Variablen für die Knotenanzahl und die Stabanzahl findet beim Einlesen

end module kenngroessen
