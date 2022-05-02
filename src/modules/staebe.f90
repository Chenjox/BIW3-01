module kenngroessen

  implicit none
  type stab
    integer :: vonKnoten
    integer :: zuKnoten
    real    :: Area
    real    :: FTM
    real    :: alpha
    integer :: KnotenUnstetigkeiten
  end type stab

  type system2D
    integer    :: AnzahlStaebe
    integer    :: AnzahlKnoten
    type(stab) :: staebe(AnzahlStaebe)
    real       :: knotenKoordinaten(2,AnzahlKnoten)
    real       :: VektorKnotenlasten(AnzahlKnoten*3)
    real       :: VektorVerschiebungen(AnzahlKnoten*3)
  end type system2D

end module kenngroessen
