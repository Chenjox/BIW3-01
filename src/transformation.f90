subroutine transglobal(alpha,matlokal,matglobal)
!Lokale Steifigkeitsmatrix in globale Steifigkeitsmatrix umrechnen
!Eingabegroessen: Transformationswinkel alpha, lokale Steifigkeitsmatrix
!Ausgabegroessen: globale Steifigkeitsmatrix

implicit none

real*8 alpha
real*8, dimension(3,3):: T
real*8, dimension(3,3), INTENT(IN):: matlokal
real*8, dimension(3,3), INTENT(OUT):: matglobal


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

implicit none

real*8 :: alpha
real*8, dimension(3,3):: T
real*8, dimension(3,3), INTENT(OUT):: matlokal
real*8, dimension(3,3), INTENT(IN):: matglobal


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

subroutine translokalVektor(alpha,vekglobal,veklokal)
!globalen Vektor in lokalen Vektor umrechnen
!Eingabegroessen: Transformationswinkel alpha, globaler Verschiebungsvektor
!Ausgabegroessen: lokaler Verschiebungsvektor

implicit none

real*8 :: alpha
real*8, dimension(3,3):: T
real*8, dimension(3,1), INTENT(OUT):: veklokal
real*8, dimension(3,1), INTENT(IN)::  vekglobal


!Transformationsmatrix aus Transformationswinkel erstellen
  T=0                                                     !Alle Zellen der Matrix zu Null

  T(1,1) = cos(alpha)
  T(1,2) = sin(alpha)
  T(2,1) = -sin(alpha)
  T(2,2) = cos(alpha)
  T(3,3) = 1

!Globale Steifigkeitsmatrix ausrechnen
  veklokal = 0                                           !Funktionen von Fortran:
  veklokal = MATMUL(TRANSPOSE(T),vekglobal)              !MATMUL     - Matrizenmultiplikation
                                                          !TRANSPOSE  - Transponierte Matrix erstellen)

endsubroutine
