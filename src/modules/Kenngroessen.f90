
module kenngroessen

  implicit none

  integer nk, ns                          !deklarieren von Knotenanzahl, Stabanzahl
  integer nf                              !deklarieren von der Anzahl der Stützknoten
  real E
  !-----------------------------------------------------------------------------
  !Initialiseren unserer Variablen für die Knotenanzahl und die Stabanzahl findet beim Einlesen

  ! Erster Index x bzw. y, zweiter index = Knotenindex
  real :: koordinatenmatrix(20,2)

!-------------------------------------------------------------------------------
!Belastungsmatrix
!FÜR ART 0 (KRAFT/MOMENT): (NummerKnoten, Art, P1, P2, M3) bzw P in x1, P in x2, M in Phi
!FÜR ART 1 (VERSCHIEBUNG): (NummerKnoten, Art, Verschiebung in x, Verschiebung in y, Verdrehung Phi)
  real :: Belastungsmatrix(20,5)

  type stab                                   !definieren eines komplexen Datentypen Stab
    integer :: NrStab, K1, K2                 !Stabnummer, Knoten1, Knoten2, Stabnummer
    real :: E,A,I, alpha                      !E-Modul,QSFläche, Flächenträgheitsmoment, Winkel Alpha
    integer :: art                            !Art(Unstetigkeit)
  end type stab

  type(stab) :: staebe(1:100)

end module kenngroessen
