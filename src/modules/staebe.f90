module kenngroessen

  implicit none

  system2D(:,:) :: systemA

  type stab
    integer :: vonKnoten
    integer :: zuKnoten
    real    :: Area
    real    :: FTM
    real    :: alpha
    integer :: KnotenUnstetigkeiten
  end type stab

  ! Dieser Typ verursacht sehr viele ICEs in Version 9.2.0
  ! Ich muss das Programm um diese Limitation drumherum bauen
  ! Ansatz: Alle Arrays der Typen einzeln verwalten, ohne datenstruktur
  type system2D(AnzahlStaebe, AnzahlKnoten)
    integer, len    :: AnzahlStaebe
    integer, len    :: AnzahlKnoten
    type(stab) :: staebe(AnzahlStaebe)
    real       :: KnotenKoordinaten(2,AnzahlKnoten)
    real       :: VektorKnotenlasten(AnzahlKnoten*3)
    real       :: VektorVerschiebungen(AnzahlKnoten*3)
  end type system2D
  !


end module kenngroessen
