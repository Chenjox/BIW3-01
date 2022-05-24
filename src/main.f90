!---------------------------
program haupt
!----------------------------
  use kenngroessen
  implicit none
  ! -------------------------

  ! Variablendeklarationen
  character(len=255) :: pfad
  integer            :: FehlerNummer
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

  write(*,*) koordinatenmatrix

  ! Deformationsmethode -- Aufbau der Globalen Steifigkeitsmatrix

  ! Deformationsmethode -- Aufbau des Vektors der Knotenlasten

  ! Deformationsmethode -- Aufbau des Vektors der Knotenverschiebungen

  ! Deformationsmethode -- Lösen des Gleichungssystems
  ! --- Auflagerknoten müssen in der Steifigkeitsmatrix rausgestrichen werden

  ! Deformationsmethode -- Rückwärtseinsetzen der Verschiebungen

  ! Deformationsmethode -- Übertragen der ausgerechneten Schnittreaktionen in die lokalen KOS

  ! Ausgeben der SKs und Auflagerreaktionen

  ! Ausgeben mit Gnuplot


end program haupt
