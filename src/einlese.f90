

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

  write(*,*) 'Ich versuche die folgende Datei einzulesen: ', pfad
  open(unit=20, file=pfad, status='old',iostat=ios)
  if(ios.ne.0) then
    write(*,*) 'Ich konnte die Datei nicht lesen. Bitte nochmal versuchen.'
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

  nk = int(ZahlenListe(1))                                       !Initialiseren von der Anzahl der Knoten
  ns = int(ZahlenListe(2))                                       !Initialiseren von der Anzahl der Stäbe

  momentaneZeile = ''                                       !ausleeren der momentaneZeile
  read(unit=20,fmt='(A255)') momentaneZeile                 !Einlesen
  call ffread(momentaneZeile, ZahlenListe, Listenlaenge)    !Interpretrieren der nächsten Zeile

  if (int(ZahlenListe(1)).le.0) then
    stop "Anzahl der Stützknoten ist kleiner gleich 0!"     !Fehlercode ausgabe
  end if

  nf = int(ZahlenListe(1))                                       !Initialiseren von der Anzahl der Stäbe

  momentaneZeile = ''                                       !ausleeren der momentaneZeile
  read(unit=20,fmt='(A255)') momentaneZeile                 !Einlesen
  call ffread(momentaneZeile, ZahlenListe, Listenlaenge)    !Einlesen der nächsten Zeile

  if (int(ZahlenListe(1)).le.0) then
    stop "E-Modul ist kleiner gleich 0!!"                   !Fehlercode ausgabe
  end if

  E = real(ZahlenListe(1),8)                                        !Initialiseren vom E-Modul
! ------------------------------------------------------------------------------
!Einlesen der Knoten und der Koordinaten der Knoten
! ------------------------------------------------------------------------------
  allocate (koordinatenmatrix(nk,2))                            !die Größe der Matrix mit den Koordinaten der Knoten erfolgt in diesem Schritt
  do i = 1, nk
    momentaneZeile = ''                                       !PURGE!!!! der momentaneZeile
    read(unit=20,fmt='(A255)') momentaneZeile                 !Einlesen
    call ffread(momentaneZeile, ZahlenListe, Listenlaenge)    !Einlesen der nächsten Zeile
    koordinatenmatrix(i,1) = real(ZahlenListe(1),8)                   !Initialiseren von x
    koordinatenmatrix(i,2) = real(ZahlenListe(2),8)                   !Initialiseren von y
  enddo

! ------------------------------------------------------------------------------
!Einlesen der Stäbe in den Komplexen Datentypen
! ------------------------------------------------------------------------------
  allocate(staebe(ns))
  do i = 1, ns
    momentaneZeile = ''                                       !PURGE!!!! der momentaneZeile
    read(unit=20,fmt='(A255)') momentaneZeile                 !Einlesen
    call ffread(momentaneZeile, ZahlenListe, Listenlaenge)    !Einlesen der nächsten Zeile
    staebe(i)%NrStab= int(ZahlenListe(1))
    staebe(i)%K1=     int(ZahlenListe(2))
    staebe(i)%K2=     int(ZahlenListe(3))
    staebe(i)%A=      real(ZahlenListe(4),8)
    staebe(i)%I=      real(ZahlenListe(5),8)
    staebe(i)%alpha=  real(ZahlenListe(6),8)
    staebe(i)%art=    int(ZahlenListe(7))
    staebe(i)%E=      real(E,8)
  enddo

! ------------------------------------------------------------------------------
!Einlesen der Belastungen in die Matrix
! ------------------------------------------------------------------------------


  !Listenlaenge = 1
  i= 0
  AnzBelastung = 0
  do                                                          !UNENDLICHE SCHLEIFE bis Austrittsbedingung erfüllt ist
    i = i +1                                                  !Hochzählen des Integers um die Belastungsmatrix zu füllen
    momentaneZeile = ''                                       !PURGE!!!! der momentaneZeile
    read(unit=20,fmt='(A255)',iostat=ios) momentaneZeile      !Einlesen, wenn end of file ios wird zu 1 gesetzt!!!!!!

    if (ios.eq.0) then                                        !Abbruchkriterium End of File feststelen
      call ffread(momentaneZeile, ZahlenListe, Listenlaenge)    !Einlesen der nächsten Zeile
      Belastungsmatrix(i,1) = real(ZahlenListe(1),8)                   !Initialiseren von Knotennummer
      Belastungsmatrix(i,2) = real(ZahlenListe(2),8)                   !Initialiseren von ART
      Belastungsmatrix(i,3) = real(ZahlenListe(3),8)                   !Initialiseren von P1/V1
      Belastungsmatrix(i,4) = real(ZahlenListe(4),8)                   !Initialiseren von P2/V2
      Belastungsmatrix(i,5) = real(ZahlenListe(5),8)                   !Initialiseren von M3/Phi

      AnzBelastung = AnzBelastung+1
    else
      exit
    end if
  enddo


  FehlerNummer = 0                                            !Alles OKAY

end subroutine einlesen
