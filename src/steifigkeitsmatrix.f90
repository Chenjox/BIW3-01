use kenngroessen

real :: Stabsteifigkeitsmatrix(3,3)


!Stabsteifigkeitsmatrix K(ik,k) für Elemente die obere rechte Dreiecksmatrix (untere linke ergibt sich daraus weil symmetrisch)
do s=1, ns
  if(staebe(s)%K1 .eq. i)                                                               !Zeile gleich Anfangsknoten?

    l=sqrt((koordinatenmatrix(staebe(s)%K2,1)-koordinatenmatrix(staebe(s)%K1,1))^2  &
          +(koordinatenmatrix(staebe(s)%K2,2)-koordinatenmatrix(staebe(s)%K1,2))^2)     !Stablängen bestimmen "Wurzel((x2-x1)^2-(y2-y1)^2)"

    Stabsteifigkeitsmatrix = 0
    Stabsteifigkeitsmatrix(1,1) = -(staebe(s)%E*staebe(s)%A)/l
    Stabsteifigkeitsmatrix(2,2) = -(12*staebe(s)%E*staebe(s)%I)/l^3
    Stabsteifigkeitsmatrix(2,3) = (6*staebe(s)%E*staebe(s)%I)/l^2
    Stabsteifigkeitsmatrix(1,1) = -(6*staebe(s)%E*staebe(s)%I)/l^2
    Stabsteifigkeitsmatrix(1,1) = (2*staebe(s)%E*staebe(s)%I)/l

  end if
end do
