
subroutine StabsteifigkeitLokalKIKI(Stab, zuBefuellendeMatrix)
  use kenngroessen
  implicit none
  type(stab) :: Stab
  real, dimension(3,3) :: zuBefuellendeMatrix

  l=sqrt((koordinatenmatrix(Stab%K2,1) - koordinatenmatrix(Stab%K1,1))**2  &
        +(koordinatenmatrix(Stab%K2,2) - koordinatenmatrix(Stab%K1,2))**2)   !Stablängen bestimmen "Wurzel((x2-x1)^2-(y2-y1)^2)"

  zuBefuellendeMatrix = 0                                                          !Alle Einträge der Stabsteifigkeitsmatrix werden zunächst Null gesetzt

  zuBefuellendeMatrix(1,1) =   (   Stab(s)%E*Stab%A)/l
  zuBefuellendeMatrix(2,2) =   (12*Stab(s)%E*Stab%I)/l**3
  zuBefuellendeMatrix(2,3) =   (6* Stab(s)%E*Stab%I)/l**2
  zuBefuellendeMatrix(3,2) =   (6* Stab(s)%E*Stab%I)/l**2
  zuBefuellendeMatrix(3,3) =   (4* Stab(s)%E*Stab%I)/l

end subroutine StabsteifigkeitLokalKIKI
!Stabsteifigkeitsmatrix K(ik,k) für Elemente die obere rechte Dreiecksmatrix (untere linke ergibt sich daraus weil symmetrisch)

subroutine StabsteifigkeitLokalKIIK(Stab, zuBefuellendeMatrix)
  use kenngroessen
  implicit none
  type(stab) :: Stab
  real, dimension(3,3) :: zuBefuellendeMatrix

  l=sqrt((koordinatenmatrix(Stab%K2,1) - koordinatenmatrix(Stab%K1,1))**2  &
        +(koordinatenmatrix(Stab%K2,2) - koordinatenmatrix(Stab%K1,2))**2)   !Stablängen bestimmen "Wurzel((x2-x1)^2-(y2-y1)^2)"

  zuBefuellendeMatrix = 0                                                          !Alle Einträge der Stabsteifigkeitsmatrix werden zunächst Null gesetzt

  zuBefuellendeMatrix(1,1) = - (   Stab(s)%E*Stab%A)/l
  zuBefuellendeMatrix(2,2) = - (12*Stab(s)%E*Stab%I)/l**3
  zuBefuellendeMatrix(2,3) =   (6* Stab(s)%E*Stab%I)/l**2
  zuBefuellendeMatrix(3,2) = - (6* Stab(s)%E*Stab%I)/l**2
  zuBefuellendeMatrix(3,3) =   (2* Stab(s)%E*Stab%I)/l

end subroutine StabsteifigkeitLokalKIIK

subroutine StabsteifigkeitLokalIKKI(Stab, zuBefuellendeMatrix)
  use kenngroessen
  implicit none
  type(stab) :: Stab
  real, dimension(3,3) :: zuBefuellendeMatrix

  l=sqrt((koordinatenmatrix(Stab%K2,1) - koordinatenmatrix(Stab%K1,1))**2  &
        +(koordinatenmatrix(Stab%K2,2) - koordinatenmatrix(Stab%K1,2))**2)   !Stablängen bestimmen "Wurzel((x2-x1)^2-(y2-y1)^2)"

  zuBefuellendeMatrix = 0                                                          !Alle Einträge der Stabsteifigkeitsmatrix werden zunächst Null gesetzt

  zuBefuellendeMatrix(1,1) = - (   Stab(s)%E*Stab%A)/l
  zuBefuellendeMatrix(2,2) = - (12*Stab(s)%E*Stab%I)/l**3
  zuBefuellendeMatrix(2,3) = - (6* Stab(s)%E*Stab%I)/l**2
  zuBefuellendeMatrix(3,2) =   (6* Stab(s)%E*Stab%I)/l**2
  zuBefuellendeMatrix(3,3) =   (2* Stab(s)%E*Stab%I)/l

end subroutine StabsteifigkeitLokalIKKI

subroutine StabsteifigkeitLokalIKIK(Stab, zuBefuellendeMatrix)
  use kenngroessen
  implicit none
  type(stab) :: Stab
  real, dimension(3,3) :: zuBefuellendeMatrix

  l=sqrt((koordinatenmatrix(Stab%K2,1) - koordinatenmatrix(Stab%K1,1))**2  &
        +(koordinatenmatrix(Stab%K2,2) - koordinatenmatrix(Stab%K1,2))**2)   !Stablängen bestimmen "Wurzel((x2-x1)^2-(y2-y1)^2)"

  zuBefuellendeMatrix = 0                                                          !Alle Einträge der Stabsteifigkeitsmatrix werden zunächst Null gesetzt

  zuBefuellendeMatrix(1,1) =   (   Stab(s)%E*Stab%A)/l
  zuBefuellendeMatrix(2,2) =   (12*Stab(s)%E*Stab%I)/l**3
  zuBefuellendeMatrix(2,3) = - (6* Stab(s)%E*Stab%I)/l**2
  zuBefuellendeMatrix(3,2) = - (6* Stab(s)%E*Stab%I)/l**2
  zuBefuellendeMatrix(3,3) =   (4* Stab(s)%E*Stab%I)/l

end subroutine StabsteifigkeitLokalIKIK
