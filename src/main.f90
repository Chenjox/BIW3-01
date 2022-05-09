program haupt
  use kenngroessen
  implicit none
  integer :: nKnoten
  integer :: nStaebe
  type(system2D(:,:)), allocatable :: systema
  interface
    subroutine systemEinlesen(ioUnit, nKnoten, nStaebe,system)
      use kenngroessen
      integer :: ioUnit ! Hier ist die ioUnit
      integer :: nKnoten
      integer :: nStaebe
      type(system2D(nKnoten,nStaebe)) :: system
    end subroutine systemEinlesen
  end interface

  ! Hier kommt jetzt extremst hässlicher code, aber es geht nicht anders
  ! In Compiler Version 9.2.0 kommt sonst
  ! f951.exe: internal compiler error: gfc_compare_array_spec(): Array spec clobbered
  open(unit=10, file="testdm1.TXT")

  read(unit=10, fmt=*) nKnoten, nStaebe

  allocate(system2D(nKnoten, nStaebe) :: systema)

  call systemEinlesen(10, nKnoten, nStaebe, systema)

  close(unit=10)

end program haupt
