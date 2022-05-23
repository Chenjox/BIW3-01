
module kenngroessen

  implicit none

  type stab
    integer :: art
    real*8 :: x(2),y(2),E,A,I
  end type stab

  type(stab) :: staebe(1:100)

end module kenngroessen
