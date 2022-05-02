module stabkenngroessen

  implicit none
  type stab
    integer :: vonKnoten
    integer :: zuKnoten
    real    :: Area
    real    :: FTM
    real    :: alpha
    integer :: KnotenUnstetigkeiten
  end type stab

end module stabkenngroessen
