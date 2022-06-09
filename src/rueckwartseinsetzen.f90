subroutine rueckwaerts
  use kenngroessen
  implicit none
  integer :: startKnotenIndex, endKnotenIndex, i
  real*8  :: lokaleStartVerschiebungen(3,1), lokaleEndVerschiebungen(3,1)
  real*8  :: lokaleStartMatrix(3,3), lokaleEndMatrix(3,3)
  real*8  :: knotenrandschnittkraefte(3,1)
  real*8  :: globaleVerschiebungen(3,1)

  Stabrandschnittkraftmatrix = 0

  do i = 1, ns
    startKnotenIndex = staebe(i)%K1*3-2
    endKnotenIndex = staebe(i)%K2*3-2

    lokaleStartVerschiebungen = 0
    lokaleEndVerschiebungen   = 0
    globaleVerschiebungen     = 0

    ! Wenn es KEIN Stützknoten ist, dann hole dir die Verschiebungen

    if(staebe(i)%K1.le.(nk-nf)) then
      globaleVerschiebungen(1,1) = VektorKnotenverschiebungen(startKnotenIndex)
      globaleVerschiebungen(2,1) = VektorKnotenverschiebungen(startKnotenIndex+1)
      globaleVerschiebungen(3,1) = VektorKnotenverschiebungen(startKnotenIndex+2)

      call translokalVektor(staebe(i)%alpha, globaleVerschiebungen, lokaleStartVerschiebungen)
    else
      lokaleStartVerschiebungen = 0
    end if

    if(staebe(i)%K2.le.(nk-nf)) then
      globaleVerschiebungen(1,1) = VektorKnotenverschiebungen(endKnotenIndex)
      globaleVerschiebungen(2,1) = VektorKnotenverschiebungen(endKnotenIndex+1)
      globaleVerschiebungen(3,1) = VektorKnotenverschiebungen(endKnotenIndex+2)

      call translokalVektor(staebe(i)%alpha, globaleVerschiebungen, lokaleEndVerschiebungen)
    else
      lokaleEndVerschiebungen = 0
    end if

    ! Zuerst der Anfangsknoten

    lokaleStartMatrix = 0
    lokaleEndMatrix = 0
    knotenrandschnittkraefte = 0

    call StabsteifigkeitLokalIKIK(staebe(i),lokaleStartMatrix)
    call StabsteifigkeitLokalKIIK(staebe(i),lokaleEndMatrix)

    knotenrandschnittkraefte = MATMUL(lokaleStartMatrix,lokaleStartVerschiebungen) &
                              + MATMUL(lokaleEndMatrix,lokaleEndVerschiebungen)

<<<<<<< HEAD
    !write(*,*) 'Randschnittkraefte am Stab ',staebe(i)%K1,' ',staebe(i)%K2,knotenrandschnittkraefte
=======
    write(*,*) 'Randschnittkraefte am Stab ',staebe(i)%K1,' ',staebe(i)%K2,knotenrandschnittkraefte
>>>>>>> e2d5ae501533b43dcdc55311958a361ad5eb8a29

    !Einsortieren
    Stabrandschnittkraftmatrix(i,1) = knotenrandschnittkraefte(1,1)
    Stabrandschnittkraftmatrix(i,2) = knotenrandschnittkraefte(2,1)
    Stabrandschnittkraftmatrix(i,3) = knotenrandschnittkraefte(3,1)

    ! Jetzt der Andere Knoten

    lokaleStartMatrix = 0
    lokaleEndMatrix = 0
    knotenrandschnittkraefte = 0

    call StabsteifigkeitLokalIKKI(staebe(i),lokaleStartMatrix)
    call StabsteifigkeitLokalKIKI(staebe(i),lokaleEndMatrix)

    knotenrandschnittkraefte = MATMUL(lokaleStartMatrix,lokaleStartVerschiebungen) &
                              + MATMUL(lokaleEndMatrix,lokaleEndVerschiebungen)

    !Einsortieren
    Stabrandschnittkraftmatrix(i,4) = knotenrandschnittkraefte(1,1)
    Stabrandschnittkraftmatrix(i,5) = knotenrandschnittkraefte(2,1)
    Stabrandschnittkraftmatrix(i,6) = knotenrandschnittkraefte(3,1)

  end do



end subroutine rueckwaerts
