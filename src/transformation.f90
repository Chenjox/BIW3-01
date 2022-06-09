subroutine transglobal(alpha,matlokal,matglobal)
!Lokale Steifigkeitsmatrix in globale Steifigkeitsmatrix umrechnen
!Eingabegroessen: Transformationswinkel alpha, lokale Steifigkeitsmatrix
!Ausgabegroessen: globale Steifigkeitsmatrix

use kenngroessen

implicit none

real alpha
real, dimension(3,3):: T
real, dimension(3,3), INTENT(IN):: matlokal
real, dimension(3,3), INTENT(OUT):: matglobal


!Transformationsmatrix aus Transformationswinkel erstellen
  T=0                                                     !Alle Zellen der Matrix zu Null

  T(1,1) = cos(alpha)
  T(1,2) = sin(alpha)
  T(2,1) = -sin(alpha)
  T(2,2) = cos(alpha)
  T(3,3) = 1

!Globale Steifigkeitsmatrix ausrechnen
  matglobal = 0                                           !Funktionen von Fortran:
  matglobal = MATMUL(MATMUL(T,matlokal),TRANSPOSE(T))     !MATMUL     - Matrizenmultiplikation
                                                          !TRANSPOSE  - Transponierte Matrix erstellen

  !call printMatrix(matglobal,3)

endsubroutine


!Transformation in lokale Koordinaten ------------------------------------------

subroutine translokal(alpha,matglobal,matlokal)
!globale Steifigkeitsmatrix in lokale Steifigkeitsmatrix umrechnen
!Eingabegroessen: Transformationswinkel alpha, globale Steifigkeitsmatrix
!Ausgabegroessen: lokale Steifigkeitsmatrix

use kenngroessen

implicit none

real alpha
real, dimension(3,3):: T
real, dimension(3,3), INTENT(OUT):: matlokal
real, dimension(3,3), INTENT(IN):: matglobal


!Transformationsmatrix aus Transformationswinkel erstellen
  T=0                                                     !Alle Zellen der Matrix zu Null

  T(1,1) = cos(alpha)
  T(1,2) = sin(alpha)
  T(2,1) = -sin(alpha)
  T(2,2) = cos(alpha)
  T(3,3) = 1

!Globale Steifigkeitsmatrix ausrechnen
  matlokal = 0                                           !Funktionen von Fortran:
  matlokal = MATMUL(TRANSPOSE(T),matglobal)              !MATMUL     - Matrizenmultiplikation
                                                          !TRANSPOSE  - Transponierte Matrix erstellen)

endsubroutine
