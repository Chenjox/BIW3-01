
module kenngroessen

  implicit none

  integer nk, ns                          !deklarieren von Knotenanzahl, Stabanzahl
  integer nf                              !deklarieren von der Anzahl der Stützknoten
  real E
  !-----------------------------------------------------------------------------
  !Initialiseren unserer Variablen für die Knotenanzahl und die Stabanzahl findet beim Einlesen

  ! Erster Index x bzw. y, zweiter index = Knotenindex
  real :: koordinatenmatrix(2,100)
  type FreierKnoten                       !definieren eines komplexen Datentypen Stab
    integer :: art
    real :: x,y                           !Koordinaten des Knotens
  end type

  !type(Knoten) :: Knoten(1:100)

  type stab                               !definieren eines komplexen Datentypen Stab
    integer :: art
    real :: x(2),y(2),E,A,I               !x1,x2,y1,y2,E-Modul,QSFläche, Flächenträgheitsmoment
  end type stab

  type(stab) :: staebe(1:100)

end module kenngroessen
