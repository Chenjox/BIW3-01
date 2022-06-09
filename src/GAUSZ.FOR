      SUBROUTINE gausz(n,kmat,rvek,vs)
c======================================================================
c     Verketteter Gauszscher Algorithmus
c======================================================================
      INTEGER*4  n
      REAL*8     kmat(1:n,1:n),rvek(n),vs(n)
c----------------------------------------------------------------------
c     n....i4...Anzahl der Unbekannten
c     SYSTEM:
c          Kmat * Vs = Rvek
c----------------------------------------------------------------------
      parameter  (rnul = 1.0D-10)
      INTEGER*4  i,j,k,p
      REAL*8     s,u(1:n,1:n),y(n) 
c----------------------------------------------------------------------
      do 10 i=1,n
         y(i)=0.
      do 11 j=1,n
11       u(i,j)=0.
10    continue
c----------------------------------------------------------------------
c     Aufteilen der Koeff.-Matrix in obere und untere Dreiecks-Matrix
c----------------------------------------------------------------------
      do 20 k=1,n
         do 21 j=k,n
            s=0.
            if (k.NE.1) then
               do 22 p=1,(k-1)
22                s=s+u(k,p)*u(p,j)
            end if
            u(k,j)=kmat(k,j)-s
            i=j+1
            if (i.GT.n) goto 1
            s=0.
            if (k.NE.1) then
               do 23 p=1,(k-1)
23                s=s+u(i,p)*u(p,k)
            end if
            u(i,k)=kmat(i,k)-s
            u(i,k)=u(i,k)/u(k,k)
1           s=0.
21       continue
20    continue
c----------------------------------------------------------------------
c     Rechte Seite vorwaerts
c----------------------------------------------------------------------
      y(1)=rvek(1)
      do 30 i=2,n
         s=0.
         do 31 k=1,(i-1)
31          s=s+u(i,k)*y(k)
         y(i)=rvek(i)-s
30    continue
c----------------------------------------------------------------------
c     Kontrolle auf Singularitaet
c----------------------------------------------------------------------
      do 35 i=1,n
         If(abs(u(i,i)) .LT. rnul) then
            Write(*,'(''Fataler Fehler !'')')
            Write(*,
     *      '(''Singularitaet im Hauptdiagonalen-Element Nr.: '',i3)') i
            STOP
         endif
 35   continue
c----------------------------------------------------------------------
c     Rechte Seite rueckwaerts
c----------------------------------------------------------------------
      y(n)=y(n)/u(n,n)
      do 40 i=(n-1),1,-1
         s=0.
	 do 41 k=(i+1),n
41          s=s+u(i,k)*y(k)
         y(i)=(y(i)-s)/u(i,i)
40    continue
c----------------------------------------------------------------------
      do 50 i=1,n
50       vs(i)=y(i)
c----------------------------------------------------------------------
      return
      end
      