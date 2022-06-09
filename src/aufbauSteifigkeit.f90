subroutine aufbauSystemsteifigkeitsmatrix
  use kenngroessen
  implicit none
  real*8 :: StabsteifigkeitLokal(3,3)
  real*8 :: StabsteifigkeitGlobal(3,3)
  integer :: i
  integer :: currentK1, currentK2

  ! Iteration durch die Stäbe
  do i = 1, ns
    currentK1 = staebe(i)%K1
    currentK2 = staebe(i)%K2
    StabsteifigkeitLokal=0
    StabsteifigkeitGlobal=0
    ! Oben Links
    if(currentK1.le.(nk-nf)) then
      ! Der Knoten ist relevant für unser System
      ! Subroutine für die Abschnitte
      call StabsteifigkeitLokalKIKI(staebe(i),StabsteifigkeitLokal)
      call transglobal(staebe(i)%alpha,StabsteifigkeitLokal,StabsteifigkeitGlobal)
      call fillIntoMatrix(Gesamtsteifigkeitsmatrix,StabsteifigkeitGlobal, &
                            (nk-nf)*3, currentK1,currentK1)
    end if
    StabsteifigkeitLokal=0
    StabsteifigkeitGlobal=0
    ! Unten Rechts
    if(currentK2.le.(nk-nf)) then
      ! Der Knoten ist relevant für unser System
      ! Subroutine für die Abschnitte
      call StabsteifigkeitLokalIKIK(staebe(i),StabsteifigkeitLokal)
      call transglobal(staebe(i)%alpha,StabsteifigkeitLokal,StabsteifigkeitGlobal)
      call fillIntoMatrix(Gesamtsteifigkeitsmatrix,StabsteifigkeitGlobal, &
                            (nk-nf)*3, currentK2,currentK2)
    end if
    StabsteifigkeitLokal=0
    StabsteifigkeitGlobal=0
    ! Oben Rechts und Unten Links
    if(currentK1.le.(nk-nf).and.currentK2.le.(nk-nf)) then
      ! Der Knoten ist relevant für unser System
      ! Subroutine für die Abschnitte
      call StabsteifigkeitLokalKIIK(staebe(i),StabsteifigkeitLokal)
      call transglobal(staebe(i)%alpha,StabsteifigkeitLokal,StabsteifigkeitGlobal)
      call fillIntoMatrix(Gesamtsteifigkeitsmatrix,StabsteifigkeitGlobal, &
                            (nk-nf)*3, currentK1,currentK2)
      StabsteifigkeitLokal=0
      StabsteifigkeitGlobal=0
      call StabsteifigkeitLokalIKKI(staebe(i),StabsteifigkeitLokal)
      call transglobal(staebe(i)%alpha,StabsteifigkeitLokal,StabsteifigkeitGlobal)
      call fillIntoMatrix(Gesamtsteifigkeitsmatrix,StabsteifigkeitGlobal, &
                            (nk-nf)*3, currentK2,currentK1)
    end if
  end do
  write(unit=*, fmt=*) 'Hier kommt die Gesamtsteifigkeitsmatrix'
  read(*,*)
  call writeMatrix("K.TXT",Gesamtsteifigkeitsmatrix,(nk-nf)*3)
  read(*,*)

end subroutine aufbauSystemsteifigkeitsmatrix
