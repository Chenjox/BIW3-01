!---------------------------
program haupt
!----------------------------
  use kenngroessen
  implicit none
  ! -------------------------

  ! Variablendeklarationen
  character(len=255) :: pfad
  integer            :: FehlerNummer
  integer i                                             !Zählvariable
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
write(*,*) 'DEBUGGING'
  do i=1, nk
    write(*,*) koordinatenmatrix(i,1), koordinatenmatrix(i,2)
  enddo

  write(*,*) 'E-Modul: ', E
  write(*,*) 'nf, ns, nk: ',nf, ns,nk
  write(*,*) 'Stab: ', staebe(11)

    read(*,*)

  do i=1,5
    write(*,*)Belastungsmatrix(i,1),Belastungsmatrix(i,2),Belastungsmatrix(i,3),Belastungsmatrix(i,4),Belastungsmatrix(i,5)
  enddo

write(*,*) 'Anzahl der Belastungen: ', AnzBelastung

  read(*,*)



  ! Deformationsmethode -- Aufbau der Globalen Steifigkeitsmatrix

  ! Deformationsmethode -- Aufbau des Vektors der Knotenlasten

  ! Deformationsmethode -- Aufbau des Vektors der Knotenverschiebungen

  ! Deformationsmethode -- Lösen des Gleichungssystems
  ! --- Auflagerknoten müssen in der Steifigkeitsmatrix rausgestrichen werden

  ! Deformationsmethode -- Rückwärtseinsetzen der Verschiebungen

  ! Deformationsmethode -- Übertragen der ausgerechneten Schnittreaktionen in die lokalen KOS

  ! Ausgeben der SKs und Auflagerreaktionen

  ! Ausgeben mit Gnuplot


!Befüllung der Gesamtmatrix mit einzelnen Steifigkeitsmatrizen

call matrixfuell

  read(*,*)

end program haupt
