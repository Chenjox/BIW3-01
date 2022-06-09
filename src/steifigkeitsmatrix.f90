
subroutine StabsteifigkeitLokalKIKI(Stabs, zuBefuellendeMatrix)
  use kenngroessen
  implicit none
  type(stab) :: Stabs
  real*8, dimension(3,3) :: zuBefuellendeMatrix
  real*8 :: l

  l=sqrt((koordinatenmatrix(Stabs%K2,1) - koordinatenmatrix(Stabs%K1,1))**2  &
        +(koordinatenmatrix(Stabs%K2,2) - koordinatenmatrix(Stabs%K1,2))**2)   !Stablängen bestimmen "Wurzel((x2-x1)^2-(y2-y1)^2)"

  zuBefuellendeMatrix = 0                                                          !Alle Einträge der Stabsteifigkeitsmatrix werden zunächst Null gesetzt

  zuBefuellendeMatrix(1,1) =   (   Stabs%E*Stabs%A)/l
  zuBefuellendeMatrix(2,2) =   (12*Stabs%E*Stabs%I)/l**3
  zuBefuellendeMatrix(2,3) =   (6* Stabs%E*Stabs%I)/l**2
  zuBefuellendeMatrix(3,2) =   (6* Stabs%E*Stabs%I)/l**2
  zuBefuellendeMatrix(3,3) =   (4* Stabs%E*Stabs%I)/l

end subroutine StabsteifigkeitLokalKIKI
!Stabsteifigkeitsmatrix K(ik,k) für Elemente die obere rechte Dreiecksmatrix (untere linke ergibt sich daraus weil symmetrisch)

subroutine StabsteifigkeitLokalKIIK(Stabs, zuBefuellendeMatrix)
  use kenngroessen
  implicit none
  type(stab) :: Stabs
  real, dimension(3,3) :: zuBefuellendeMatrix
  real :: l

  l=sqrt((koordinatenmatrix(Stabs%K2,1) - koordinatenmatrix(Stabs%K1,1))**2  &
        +(koordinatenmatrix(Stabs%K2,2) - koordinatenmatrix(Stabs%K1,2))**2)   !Stablängen bestimmen "Wurzel((x2-x1)^2-(y2-y1)^2)"

  zuBefuellendeMatrix = 0                                                          !Alle Einträge der Stabsteifigkeitsmatrix werden zunächst Null gesetzt

  zuBefuellendeMatrix(1,1) = - (   Stabs%E*Stabs%A)/l
  zuBefuellendeMatrix(2,2) = - (12*Stabs%E*Stabs%I)/l**3
  zuBefuellendeMatrix(2,3) =   (6* Stabs%E*Stabs%I)/l**2
  zuBefuellendeMatrix(3,2) = - (6* Stabs%E*Stabs%I)/l**2
  zuBefuellendeMatrix(3,3) =   (2* Stabs%E*Stabs%I)/l

end subroutine StabsteifigkeitLokalKIIK

subroutine StabsteifigkeitLokalIKKI(Stabs, zuBefuellendeMatrix)
  use kenngroessen
  implicit none
  type(stab) :: Stabs
  real, dimension(3,3) :: zuBefuellendeMatrix
  real :: l

  l=sqrt((koordinatenmatrix(Stabs%K2,1) - koordinatenmatrix(Stabs%K1,1))**2  &
        +(koordinatenmatrix(Stabs%K2,2) - koordinatenmatrix(Stabs%K1,2))**2)   !Stablängen bestimmen "Wurzel((x2-x1)^2-(y2-y1)^2)"

  zuBefuellendeMatrix = 0                                                          !Alle Einträge der Stabsteifigkeitsmatrix werden zunächst Null gesetzt

  zuBefuellendeMatrix(1,1) = - (   Stabs%E*Stabs%A)/l
  zuBefuellendeMatrix(2,2) = - (12*Stabs%E*Stabs%I)/l**3
  zuBefuellendeMatrix(2,3) = - (6* Stabs%E*Stabs%I)/l**2
  zuBefuellendeMatrix(3,2) =   (6* Stabs%E*Stabs%I)/l**2
  zuBefuellendeMatrix(3,3) =   (2* Stabs%E*Stabs%I)/l

end subroutine StabsteifigkeitLokalIKKI

subroutine StabsteifigkeitLokalIKIK(Stabs, zuBefuellendeMatrix)
  use kenngroessen
  implicit none
  type(stab) :: Stabs
  real, dimension(3,3) :: zuBefuellendeMatrix
  real :: l

  l=sqrt((koordinatenmatrix(Stabs%K2,1) - koordinatenmatrix(Stabs%K1,1))**2  &
        +(koordinatenmatrix(Stabs%K2,2) - koordinatenmatrix(Stabs%K1,2))**2)   !Stablängen bestimmen "Wurzel((x2-x1)^2-(y2-y1)^2)"

  zuBefuellendeMatrix = 0                                                          !Alle Einträge der Stabsteifigkeitsmatrix werden zunächst Null gesetzt

  zuBefuellendeMatrix(1,1) =   (   Stabs%E*Stabs%A)/l
  zuBefuellendeMatrix(2,2) =   (12*Stabs%E*Stabs%I)/l**3
  zuBefuellendeMatrix(2,3) = - (6* Stabs%E*Stabs%I)/l**2
  zuBefuellendeMatrix(3,2) = - (6* Stabs%E*Stabs%I)/l**2
  zuBefuellendeMatrix(3,3) =   (4* Stabs%E*Stabs%I)/l

end subroutine StabsteifigkeitLokalIKIK
