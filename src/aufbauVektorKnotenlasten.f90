subroutine aufbauVektorknotenlasten
  use kenngroessen
  implicit none
  integer :: i

  VektorKnotenlasten=0


  do i = 1, 20
    if(int(Belastungsmatrix(i,1)).eq.0) then
      exit
    else if(int(Belastungsmatrix(i,2)).eq.0) then
      VektorKnotenlasten(int(Belastungsmatrix(i,2))*3) = Belastungsmatrix(i,3)
      VektorKnotenlasten(int(Belastungsmatrix(i,2))*3+1) = Belastungsmatrix(i,4)
      VektorKnotenlasten(int(Belastungsmatrix(i,2))*3+2) = Belastungsmatrix(i,5)
    end if
  end do
end subroutine aufbauVektorknotenlasten
