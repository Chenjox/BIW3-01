

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

  write(*,*) 'Ich versuche die folgende Datei einzulesen:', pfad
  read(*,*)
  open(unit=20, file=pfad, status='old',iostat=ios)
  if(ios.ne.0) then
    FehlerNummer=1
    return
  end if


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
  read(unit=20,fmt='(A255)') momentaneZeile                 !Einlesen
  call ffread(momentaneZeile, ZahlenListe, Listenlaenge)    !Interpretrieren der nächsten Zeile

  if (int(ZahlenListe(1)).le.0) then
    stop "Anzahl der Stützknoten ist kleiner gleich 0!"     !Fehlercode ausgabe
  end if

  nf = ZahlenListe(1)                                       !Initialiseren von der Anzahl der Stäbe

  momentaneZeile = ''                                       !ausleeren der momentaneZeile
  read(unit=20,fmt='(A255)') momentaneZeile                 !Einlesen
  call ffread(momentaneZeile, ZahlenListe, Listenlaenge)    !Einlesen der nächsten Zeile

  if (int(ZahlenListe(1)).le.0) then
    stop "E-Modul ist kleiner gleich 0!!"                   !Fehlercode ausgabe
  end if

  E = ZahlenListe(1)                                        !Initialiseren vom E-Modul
! ------------------------------------------------------------------------------
!Einlesen der Knoten und der Koordinaten der Knoten
! ------------------------------------------------------------------------------
  do i = 1, nk
    momentaneZeile = ''                                       !PURGE!!!! der momentaneZeile
    read(unit=20,fmt='(A255)') momentaneZeile                 !Einlesen
    call ffread(momentaneZeile, ZahlenListe, Listenlaenge)    !Einlesen der nächsten Zeile
    koordinatenmatrix(i,1) = ZahlenListe(1)                   !Initialiseren von x
    koordinatenmatrix(i,2) = ZahlenListe(2)                   !Initialiseren von y
  enddo

! ------------------------------------------------------------------------------
!Einlesen der Stäbe in den Komplexen Datentypen
! ------------------------------------------------------------------------------
  do i = 1, ns
    momentaneZeile = ''                                       !PURGE!!!! der momentaneZeile
    read(unit=20,fmt='(A255)') momentaneZeile                 !Einlesen
    call ffread(momentaneZeile, ZahlenListe, Listenlaenge)    !Einlesen der nächsten Zeile
    staebe(i)%NrStab= ZahlenListe(1)
    staebe(i)%K1= ZahlenListe(2)
    staebe(i)%K2= ZahlenListe(3)
    staebe(i)%A= ZahlenListe(4)
    staebe(i)%I= ZahlenListe(5)
    staebe(i)%alpha= ZahlenListe(6)
    staebe(i)%art= ZahlenListe(7)
    staebe(i)%E= E
  enddo

! ------------------------------------------------------------------------------
!Einlesen der Belastungen in die Matrix
! ------------------------------------------------------------------------------


!Listenlaenge = 1
i= 0

do                                                          !UNENDLICHE SCHLEIFE bis Austrittsbedingung erfüllt ist
  i = i +1                                                  !Hochzählen des Integers um die Belastungsmatrix zu füllen
  momentaneZeile = ''                                       !PURGE!!!! der momentaneZeile
  read(unit=20,fmt='(A255)',iostat=ios) momentaneZeile      !Einlesen, wenn end of file ios wird zu 1 gesetzt!!!!!!

  if (ios.eq.0) then                                        !Abbruchkriterium End of File feststelen
    call ffread(momentaneZeile, ZahlenListe, Listenlaenge)    !Einlesen der nächsten Zeile
      Belastungsmatrix(i,1) = ZahlenListe(1)                   !Initialiseren von Knotennummer
      Belastungsmatrix(i,2) = ZahlenListe(2)                   !Initialiseren von ART
      Belastungsmatrix(i,3) = ZahlenListe(3)                   !Initialiseren von P1/V1
      Belastungsmatrix(i,4) = ZahlenListe(4)                   !Initialiseren von P2/V2
      Belastungsmatrix(i,5) = ZahlenListe(5)                   !Initialiseren von M3/Phi
  else
    exit
  end if
enddo


  FehlerNummer = 0                                            !Alles OKAY

end subroutine einlesen
