!---------------------------
program haupt
!----------------------------
  use kenngroessen
  implicit none
  ! -------------------------

  ! Variablendeklarationen
  character(len=255) :: pfad
  integer            :: FehlerNummer
  integer i, j                                             !Zählvariable

    !für matrixfuell
    integer ki, kj, f

  ! Variableninitialisierungen
  FehlerNummer = 1
  pfad = ''
  ! Einlesen der Datei

  do while(FehlerNummer.ne.0)

    ! --- Pfad zur Datei
    pfad = ''
    write(*,*) 'Dateipfad der einzulesenden Datei angeben (max. 255 Zeichen):'
    read(*,'(A255)') pfad
    ! --- Datei verarbeiten
    call einlesen(pfad, FehlerNummer)

  end do

  read(*,*)

  ! Dimension = AnzKnoten- AnzStützknoten = AnzfreieKnoten
  allocate(Gesamtsteifigkeitsmatrix((nk-nf)*3,(nk-nf)*3))  !Dimension
  allocate(VektorKnotenlasten((nk-nf)*3))
  allocate(VektorKnotenverschiebungen((nk-nf)*3))

  ! Deformationsmethode -- Aufbau der Globalen Steifigkeitsmatrix
  call aufbauSystemsteifigkeitsmatrix

  read(*,*)

  ! Deformationsmethode -- Aufbau des Vektors der Knotenlasten

  call aufbauVektorknotenlasten

  write(*,*) VektorKnotenlasten

  ! Deformationsmethode -- Aufbau des Vektors der Knotenverschiebungen

  ! Deformationsmethode -- Lösen des Gleichungssystems

  call lousungGLS

  write(*,*) VektorKnotenverschiebungen

  ! Deformationsmethode -- Rückwärtseinsetzen der Verschiebungen

  ! Deformationsmethode -- Übertragen der ausgerechneten Schnittreaktionen in die lokalen KOS

  ! Ausgeben der SKs und Auflagerreaktionen

  ! Ausgeben mit Gnuplot


  !Befüllung der Gesamtmatrix mit einzelnen Steifigkeitsmatrizen

  !call matrixfuell(nk,nf,Stabsteifigkeitsmatrix,ki,kj)


!Testabschnitt für transformation.f90
!  Stabsteifigkeitsmatrix=1
!  StabsteifigkeitGlobal=0
!
!  call transglobal(staebe(4)%alpha,Stabsteifigkeitsmatrix,StabsteifigkeitGlobal)
!
!  write(*,*) 'Alpha'
!  write(*,*)  staebe(4)%alpha
!  write(*,*) 'lokal'
!  write(*,*) Stabsteifigkeitsmatrix
!  write(*,*) 'global'
!  write(*,*) StabsteifigkeitGlobal
!  read(*,*)
!-------------------------------------

end program haupt
