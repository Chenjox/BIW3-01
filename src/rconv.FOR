c--------------------------------------------------------------------
c  Konvertierung der Zeichenkette zeich in eine Real-Zahl feld
c  Ist die Konvertierung nicht moeglich ist fehler true
c  Änderung: Komma ist dem Punkt gleichgestellt
c--------------------------------------------------------------------
      subroutine rconv(zeich,feld,fehler)
c==============================================================================
      INTEGER hwert(80),vorzei,ipu,kennz,iexp
      real feld,zwert,ex
      logical fehler
      CHARACTER byte,zeich*80
c
c      write(3,'(1x,2a)') 'TRCONV : ',zeich
      feld=0.
      vorzei=1
      kennz=0
      ipu=0
      iexp=0
      zwert=0.
      fehler=.true.
c
c
      DO 1 i=1,80
         byte=zeich(i:i)
         k=ichar(byte)
c
        IF ((k.GT.47).AND.(k.LT.58)) THEN
           IF (kennz.EQ.0) THEN
              n=i
              kennz=1
           END IF
           hwert(i)=k-48
        ELSE IF ((k.EQ.43).AND.(kennz.EQ.0)) then
           vorzei=1
        ELSE IF ((k.EQ.45).AND.(kennz.EQ.0)) then
           vorzei=-1
        ELSE
           IF ((k.EQ.46) .or. (k.eq.44)) then           !Punkt oder Komma
              ipu=i
           ELSE
              if ((kennz.EQ.0).AND.(iexp.EQ.0)) then
                 vorzei=1
              else
                 if ((kennz.EQ.1).AND.(ipu.EQ.0)) ipu=i
                 if (.NOT.((iexp.EQ.1).AND.(kennz.EQ.0))) then
c                 if ((iexp.EQ.0).OR.(kennz.EQ.0)) then
c                    write(3,'(1x,a,3i4)') 'Inter n,i,ipu ',n,i,ipu
                    j=i-1
                    ex=1
                    do 3 l=ipu-1,n,-1
                          zwert=zwert+float(hwert(l))*ex
                          ex=ex*10
 3                  continue
c                    write(3,'(1x,a,F10.5)') 'Vorkomma',zwert
                    ex=0.1
                    do 5 l=ipu+1,j
                          zwert=zwert+float(hwert(l))*ex
                          ex=ex/10
 5                  continue
c                     write(3,'(1x,a,F10.5)') 'gesamt',zwert
                 end if
                 if (iexp.EQ.0) then
                    feld=zwert*vorzei
                    IF ((k.EQ.69).OR.(k.EQ.101)) THEN
                       iexp=1
                    ELSE
                       GOTO 4
                    END IF
                 ELSE
                    iex=zwert+0.01
                    iex=iex*vorzei
                    feld=feld*10.**iex
                    GOTO 4
                 END IF
                 vorzei=1
                 kennz=0
                 ipu=0
                 zwert=0.
              end if
           end if
        end if
 1    CONTINUE
 4    CONTINUE
      if (kennz.EQ.1) fehler=.false.


      RETURN
      END
