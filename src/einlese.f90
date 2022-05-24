

! ------------------------------------------------------------------------------
subroutine einlesen(pfad, FehlerNummer)
  ! ----------------------------------------------------------------------------
  use kenngroessen
  implicit none
  ! Variablendeklarationen -----------------------------------------------------
  integer, INTENT(OUT) :: FehlerNummer
  integer              :: ios, Listenlaenge
  character(len=255), INTENT(IN) :: pfad
  character(len=255)   :: momentaneZeile
  real                 :: ZahlenListe(100)
  ! Variableninitialisierungen -------------------------------------------------
  integer i                                                 !deklarieren von einer Zählvariable
  ! Datei öffen

  open(unit=20, file=pfad, status='old',iostat=ios)
  if ( ios /= 0 ) stop "Error opening file 20"

  read(unit=20,fmt='(A255)') momentaneZeile

  call ffread(momentaneZeile, ZahlenListe, Listenlaenge)

  if(int(ZahlenListe(1)).le.0) then
    stop "Anzahl der Knoten ist kleiner gleich 0!"
  end if
  if(int(ZahlenListe(2)).le.0) then
    stop "Anzahl der Steabe ist kleiner gleich 0!"
  end if

  nk = ZahlenListe(1)                                       !Initialiseren von der Anzahl der Knoten
  ns = ZahlenListe(2)                                       !Initialiseren von der Anzahl der Stäbe

  momentaneZeile = ''                                       !ausleeren der momentaneZeile
  call ffread(momentaneZeile, ZahlenListe, Listenlaenge)    !Einlesen der nächsten Zeile

  if (int(ZahlenListe(1)).le.0) then
    stop "Anzahl der Stützknoten ist kleiner gleich 0!"     !Fehlercode ausgabe
  end if

  nf = ZahlenListe(1)                                       !Initialiseren von der Anzahl der Stäbe

  momentaneZeile = ''                                       !ausleeren der momentaneZeile

  call ffread(momentaneZeile, ZahlenListe, Listenlaenge)    !Einlesen der nächsten Zeile

  if (int(ZahlenListe(1)).le.0) then
    stop "E-Modul ist kleiner gleich 0!!"                   !Fehlercode ausgabe
  end if

  E = ZahlenListe(1)                                        !Initialiseren vom E-Modul

  do i = 1, nk
    momentaneZeile = ''                                       !PURGE!!!! der momentaneZeile
    call ffread(momentaneZeile, ZahlenListe, Listenlaenge)    !Einlesen der nächsten Zeile
    koordinatenmatrix(i,1) = ZahlenListe(1)                   !Initialiseren von x
    koordinatenmatrix(i,2) = ZahlenListe(2)                   !Initialiseren von y
  enddo

end subroutine einlesen
