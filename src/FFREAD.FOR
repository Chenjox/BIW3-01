c----------------------------------------------------------------------
c  formatfreies Lesen von REAL-Zahlen auf einer Zeichenkette
c  alle Zeichen ausser Ziffern, Vorzeichen, Exponentzeichen und
c  Dezimalpunkt sind Trennzeichen
c  verwendet wird die subroutine rconv(io,z,fehler)
c----------------------------------------------------------------------
      subroutine ffread(zeile,r,n)
c----------------------------------------------------------------------
c   zeile........char*80......i.......Zeichenkette
c   r............real(1)......o.......Vektor der gelesenen Zahlen
c   n............int..........i/o.....Anzahl der zu lesenden/der
c                                     gelesenen Zahlen
c----------------------------------------------------------------------
      character    zeile*80,io*80
      integer      z,i1,i,n
      real         r(1)
      logical      Zi,fehler
c----------------------------------------------------------------------
      n=1
      zi=.FALSE.
      do 3 i=1,80
         z=ichar(zeile(i:i))
c----------------------------------------------------------------------
         if (   (z.EQ.43).OR.(z.EQ.45).OR.(z.EQ.46).OR.
c     *          (z.eq.44).or.
     *         ((z.GT.47).AND.(z.LT.58)).OR.
     *          (z.EQ.69).OR.(z.EQ.101)                 ) then
            if (.NOT.(Zi)) then
              i1=i
              Zi=.TRUE.
            end if
         else if (Zi) then
                 io=zeile(i1:i)
                 call rconv(io,r(n),fehler)
                 io=' '
                 if (.NOT.(Fehler)) n=n+1
                 Zi=.FALSE.
         end if
 3    continue
      n=n-1
c-----------------------------------------------------------------------
      return
      end