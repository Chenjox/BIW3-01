
program test

integer i, f, j
real alpha
real, dimension(3,3) :: matlokal
real,dimension(3,3) :: matglobal

alpha = 1.5*3.14159

write(*,*) "alpha"
write(*,*) alpha

matlokal=0

matlokal(1,1)=7
matlokal(1,2)=0
matlokal(1,3)=0

matlokal(2,1)=0
matlokal(2,2)=0.933
matlokal(2,3)=1.4

matlokal(3,1)=0
matlokal(3,2)=1.4
matlokal(3,3)=2.8

write(*,*) "matlokal"
write(*,*) matlokal

call transglobal(alpha,matlokal,matglobal)

write(*,*) "matglobal"
write(*,*) matglobal
pause


endprogram

subroutine transglobal(alpha,matlokal,matglobal)

  !use kenngroessen

  implicit none

  real :: alpha
  real, dimension(3,3):: T
  real, dimension(3,3), INTENT(OUT):: matglobal
  real, dimension(3,3), INTENT(IN):: matlokal

  T=0

  T(1,1) = cos(alpha)
  T(1,2) = sin(alpha)
  T(2,1) = -sin(alpha)
  T(2,2) = cos(alpha)
  T(3,3) = 1
  
  write(*,*) "transformationsmat"
  write(*,*)T
	read(*,*)
	write(*,*) "Ttransformationsmat"
	write(*,*) TRANSPOSE(T)
  
  matglobal = 0
  matglobal = MATMUL(MATMUL(T,matlokal),TRANSPOSE(T))
 


  !call printMatrix(matglobal,3)

endsubroutine


