      module allClassFuncs_mod
        implicit none
      contains
        
       function newAndOpen(dir,name) result(NU)
        implicit none
         character(len=*),intent(in) :: dir,name
         integer :: NU
         NU = newUnit()
         open(NU,file=trim(dir) // trim(name) // ".txt")
       end function

       subroutine closeAndMessage(u,name)
        implicit none
         integer,intent(in) :: u
         character(len=*),intent(in) :: name
           close(u)
           write(*,*) '+++ Data for ' // name // ' written to file +++'
       end subroutine

       function newUnit() result(nu)
         implicit none
         ! local
         integer, parameter :: LUN_MIN=10, LUN_MAX=1000
         logical :: opened
         integer :: lun,nu
         ! begin
         nu=-1
         do lun=LUN_MIN,LUN_MAX
           inquire(unit=lun,opened=opened)
           if (.not. opened) then
             nu=lun
           exit
           endif
         enddo
       end function

      end module
