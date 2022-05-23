

! ------------------------------------------------------------------------------
subroutine einlesen(pfad, FehlerNummer)
  ! ----------------------------------------------------------------------------
  use kenngroessen
  implicit none
  ! Variablendeklarationen -----------------------------------------------------
  integer, INTENT(OUT) :: FehlerNummer
  integer              :: ios, LaengeListe
  character(len=255), INTENT(IN) :: pfad
  character(len=255)   :: momentaneZeile
  real                 :: ZahlenListe(100)
  ! Variableninitialisierungen -------------------------------------------------

  ! Datei öffen

  open(unit=20, file=pfad, status='old',iostat=ios)
  if ( ios /= 0 ) stop "Error opening file 20"

  read(unit=20,fmt='(A255)') momentaneZeile

  call ffread(momentaneZeile, ZahlenListe, LaengeListe)

  if(int(ZahlenListe(1)).le.0) then
    stop "Anzahl der Knoten ist kleiner gleich 0!"
  end if
  if(int(ZahlenListe(2)).le.0) then
    stop "Anzahl der Steabe ist kleiner gleich 0!"
  end if

end subroutine einlesen
