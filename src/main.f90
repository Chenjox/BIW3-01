program haupt
  use kenngroessen
  implicit none
  type(system2D(1,1)) :: systema
  interface
    function systemEinlesen(filename) result(system)
      use kenngroessen
      character :: filename
      type(system2D(:,:)), allocatable :: system
    end function systemEinlesen
  end interface
  systema = systemEinlesen("testdm1.TXT")

end program haupt
