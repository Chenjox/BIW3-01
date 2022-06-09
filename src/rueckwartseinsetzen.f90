subroutine rueckwaerts
  use kenngroessen
  implicit none
  integer :: startKnotenIndex, endKnotenIndex, i
  real*8  :: lokaleStartVerschiebungen(3,1), lokaleEndVerschiebungen(3,1)
  real*8  :: lokaleStartMatrix(3,3), lokaleEndMatrix(3,3)
  real*8  :: knotenrandschnittkraefte(3,1)
  real*8  :: globaleVerschiebungen(3,1)

  do i = 1, ns
    startKnotenIndex = staebe(i)%K1*3-2
    endKnotenIndex = staebe(i)%K2*3-2

    lokaleStartVerschiebungen = 0
    lokaleEndVerschiebungen   = 0
    globaleVerschiebungen     = 0

    globaleVerschiebungen(1,1) = VektorKnotenverschiebungen(startKnotenIndex)
    globaleVerschiebungen(2,1) = VektorKnotenverschiebungen(startKnotenIndex+1)
    globaleVerschiebungen(3,1) = VektorKnotenverschiebungen(startKnotenIndex+2)

    call translokalVektor(staebe(i)%alpha, globaleVerschiebungen, lokaleStartVerschiebungen)

    globaleVerschiebungen(1,1) = VektorKnotenverschiebungen(endKnotenIndex)
    globaleVerschiebungen(2,1) = VektorKnotenverschiebungen(endKnotenIndex+1)
    globaleVerschiebungen(3,1) = VektorKnotenverschiebungen(endKnotenIndex+2)

    call translokalVektor(staebe(i)%alpha, globaleVerschiebungen, lokaleEndVerschiebungen)

    call StabsteifigkeitLokalIKIK(staebe(i),lokaleStartMatrix)
    call StabsteifigkeitLokalKIIK(staebe(i),lokaleEndMatrix)

    knotenrandschnittkraefte = MATMUL(lokaleStartMatrix,lokaleStartVerschiebungen) &
                              + MATMUL(lokaleEndMatrix,lokaleEndVerschiebungen)

    !Einsortieren
    Stabrandschnittkraftmatrix(i,1) = knotenrandschnittkraefte(1,1)
    Stabrandschnittkraftmatrix(i,2) = knotenrandschnittkraefte(2,1)
    Stabrandschnittkraftmatrix(i,3) = knotenrandschnittkraefte(3,1)

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
