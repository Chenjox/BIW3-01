
subroutine aufbauVektorknotenverschiebungen
  use kenngroessen
  implicit none
  integer :: i,j
  integer :: momentanerKnotenMitVerschiebung
  real*8 :: eingepraegteVerschiebung(3,1),eingepraegteRSK(3,1)
  real*8 :: steifigkeitsmatrixLokal(3,3)
  real*8 :: steifigkeitsmatrixGlobal(3,3)
  integer, allocatable :: stabindezes(:)
  integer :: stabanzahl,stabindex

  VektorRandschnittkraefte = 0



  ! Hier gehen wir alle möglichen Verschiebungen durch
  do i = 1, 20
    if(int(Belastungsmatrix(i,1)).eq.0) then
      exit
    else if(int(Belastungsmatrix(i,2)).eq.1) then
      ! Verschiebungen sind ja schon im Globalen Koordinatensystem, daher sind die Steifigkeitsmatrizen zu transformieren.
      ! Initialiseren der Vektoren etc.
      eingepraegteVerschiebung = 0
      momentanerKnotenMitVerschiebung = 0
      ! Rauslesen aus der Belastungsmatrix der eingabedatei
      momentanerKnotenMitVerschiebung = int(Belastungsmatrix(i,1))
      eingepraegteVerschiebung(1,1) = Belastungsmatrix(i,3)
      eingepraegteVerschiebung(2,1) = Belastungsmatrix(i,4)
      eingepraegteVerschiebung(3,1) = Belastungsmatrix(i,5)

      ! Buchführung über die Angeschlossenen Stäbe
      stabanzahl = 0
      do j = 1,ns
        ! Abzählen der Anzahl der Stäbe
        if(staebe(j)%K1.eq.momentanerKnotenMitVerschiebung.or. &
           staebe(j)%K2.eq.momentanerKnotenMitVerschiebung) then
          stabanzahl = stabanzahl + 1
        end if
      end do
      allocate(stabindezes(Stabanzahl))
      stabindezes = 0
      stabindex = 1
      do j = 1,ns
        ! Abzählen der Anzahl der Stäbe
        if(staebe(j)%K1.eq.momentanerKnotenMitVerschiebung.or. &
           staebe(j)%K2.eq.momentanerKnotenMitVerschiebung) then
          ! In stabindezes sind die Indezes der Angeschlossenen Stäbe in der steabeMatrix hintereinander gespeichert.
          stabindezes(stabindex) = j
          stabindex = stabindex+1
        end if
      end do
      write(*,*) 'Angeschlossene Staebe sind'
      write(*,*) stabindezes
      read(*,*)
      ! Jetzt gehen wir durch alle Angeschlossenen Stäbe durch und holen uns die Matrizen

      do j = 1,stabanzahl
        ! Der Stab staebe(stabindezes) holt uns die jeweiligen Stab
        !staebe(stabindezes(j))
        ! Überprüfen ob der Knoten Anfangsknoten oder Endknoten ist.
        if (staebe(stabindezes(j))%K1.eq.momentanerKnotenMitVerschiebung) then
          call StabsteifigkeitLokalIKIK(staebe(stabindezes(j)),steifigkeitsmatrixLokal)
          call transglobal(staebe(i)%alpha,steifigkeitsmatrixLokal,steifigkeitsmatrixGlobal)

          eingepraegteRSK = 0
          eingepraegteRSK = MATMUL(steifigkeitsmatrixGlobal,eingepraegteVerschiebung)

          VektorRandschnittkraefte((staebe(stabindezes(j))%K1*3-2)) = &
            VektorRandschnittkraefte((staebe(stabindezes(j))%K1*3-2)) + eingepraegteRSK(1,1)

          VektorRandschnittkraefte((staebe(stabindezes(j))%K1*3-2)+1) = &
            VektorRandschnittkraefte((staebe(stabindezes(j))%K1*3-2)+1) + eingepraegteRSK(2,1)

          VektorRandschnittkraefte((staebe(stabindezes(j))%K1*3-2)+2) = &
            VektorRandschnittkraefte((staebe(stabindezes(j))%K1*3-2)+2) + eingepraegteRSK(3,1)

          call StabsteifigkeitLokalKIIK(staebe(stabindezes(j)),steifigkeitsmatrixLokal)
          call transglobal(staebe(i)%alpha,steifigkeitsmatrixLokal,steifigkeitsmatrixGlobal)

          eingepraegteRSK = 0
          eingepraegteRSK = MATMUL(steifigkeitsmatrixGlobal,eingepraegteVerschiebung)

          VektorRandschnittkraefte((staebe(stabindezes(j))%K2*3-2)) = &
            VektorRandschnittkraefte((staebe(stabindezes(j))%K2*3-2)) + eingepraegteRSK(1,1)

          VektorRandschnittkraefte((staebe(stabindezes(j))%K2*3-2)+1) = &
            VektorRandschnittkraefte((staebe(stabindezes(j))%K2*3-2)+1) + eingepraegteRSK(2,1)

          VektorRandschnittkraefte((staebe(stabindezes(j))%K2*3-2)+2) = &
            VektorRandschnittkraefte((staebe(stabindezes(j))%K2*3-2)+2) + eingepraegteRSK(3,1)
          else
            call StabsteifigkeitLokalIKKI(staebe(stabindezes(j)),steifigkeitsmatrixLokal)
            call transglobal(staebe(i)%alpha,steifigkeitsmatrixLokal,steifigkeitsmatrixGlobal)

            eingepraegteRSK = 0
            eingepraegteRSK = MATMUL(steifigkeitsmatrixGlobal,eingepraegteVerschiebung)

            VektorRandschnittkraefte((staebe(stabindezes(j))%K1*3-2)) = &
              VektorRandschnittkraefte((staebe(stabindezes(j))%K1*3-2)) + eingepraegteRSK(1,1)

            VektorRandschnittkraefte((staebe(stabindezes(j))%K1*3-2)+1) = &
              VektorRandschnittkraefte((staebe(stabindezes(j))%K1*3-2)+1) + eingepraegteRSK(2,1)

            VektorRandschnittkraefte((staebe(stabindezes(j))%K1*3-2)+2) = &
              VektorRandschnittkraefte((staebe(stabindezes(j))%K1*3-2)+2) + eingepraegteRSK(3,1)

            call StabsteifigkeitLokalKIKI(staebe(stabindezes(j)),steifigkeitsmatrixLokal)
            call transglobal(staebe(i)%alpha,steifigkeitsmatrixLokal,steifigkeitsmatrixGlobal)

            eingepraegteRSK = 0
            eingepraegteRSK = MATMUL(steifigkeitsmatrixGlobal,eingepraegteVerschiebung)

            VektorRandschnittkraefte((staebe(stabindezes(j))%K2*3-2)) = &
              VektorRandschnittkraefte((staebe(stabindezes(j))%K2*3-2)) + eingepraegteRSK(1,1)

            VektorRandschnittkraefte((staebe(stabindezes(j))%K2*3-2)+1) = &
              VektorRandschnittkraefte((staebe(stabindezes(j))%K2*3-2)+1) + eingepraegteRSK(2,1)

            VektorRandschnittkraefte((staebe(stabindezes(j))%K2*3-2)+2) = &
              VektorRandschnittkraefte((staebe(stabindezes(j))%K2*3-2)+2) + eingepraegteRSK(3,1)
        end if
      end do



    end if
  end do

end subroutine aufbauVektorknotenverschiebungen
