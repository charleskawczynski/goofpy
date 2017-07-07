       module surface_mod
       use allClassFuncs_mod
       use constants_mod
       use simParams_mod
       use geomParams_mod
       use debugger_mod
       use counters_mod
       use exportData_mod
       use line_mod


       type surface
         private
         integer :: id
         type(line) :: l
         logical :: entrance
         logical :: exit
       endtype

       contains

       function setSurface(id,l,entrance,exit) result(this)
         implicit none
         type(surface) :: this
         integer, intent(in) :: id
         type(line), intent(in) :: l
         logical, intent(in) :: entrance
         logical, intent(in) :: exit
         call SetSurfaceId(this,id)
         call SetSurfaceL(this,l)
         call SetSurfaceEntrance(this,entrance)
         call SetSurfaceExit(this,exit)
       end function

       subroutine setSurfaceId(this,id)
         implicit none
         type(surface),intent(inout) :: this
         integer, intent(in) :: id
         this%id = id
       end subroutine

       subroutine setSurfaceL(this,l)
         implicit none
         type(surface),intent(inout) :: this
         type(line), intent(in) :: l
         this%l = l
       end subroutine

       subroutine setSurfaceEntrance(this,entrance)
         implicit none
         type(surface),intent(inout) :: this
         logical, intent(in) :: entrance
         this%entrance = entrance
       end subroutine

       subroutine setSurfaceExit(this,exit)
         implicit none
         type(surface),intent(inout) :: this
         logical, intent(in) :: exit
         this%exit = exit
       end subroutine

       function getSurfaceId(this) result(id)
         implicit none
         type(surface),intent(in) :: this
         integer :: id
         id = this%id
       end function

       function getSurfaceL(this) result(l)
         implicit none
         type(surface),intent(in) :: this
         type(line) :: l
         l = this%l
       end function

       function getSurfaceEntrance(this) result(entrance)
         implicit none
         type(surface),intent(in) :: this
         logical :: entrance
         entrance = this%entrance
       end function

       function getSurfaceExit(this) result(exit)
         implicit none
         type(surface),intent(in) :: this
         logical :: exit
         exit = this%exit
       end function

       subroutine getSurface(this,id,l,entrance,exit)
         implicit none
         type(surface),intent(in) :: this
         integer, intent(out) :: id
         type(line), intent(out) :: l
         logical, intent(out) :: entrance
         logical, intent(out) :: exit
         id = getSurfaceId(this)
         l = getSurfaceL(this)
         entrance = getSurfaceEntrance(this)
         exit = getSurfaceExit(this)
       end subroutine

       subroutine printSurfaceId(this)
         implicit none
         type(surface),intent(in) :: this
         integer :: id
         write(*,*) "id: ", this%id
       end subroutine

       subroutine printSurfaceL(this)
         implicit none
         type(surface),intent(in) :: this
         type(line) :: l
         call printLine(this%l)
       end subroutine

       subroutine printSurfaceEntrance(this)
         implicit none
         type(surface),intent(in) :: this
         logical :: entrance
         write(*,*) "entrance: ", this%entrance
       end subroutine

       subroutine printSurfaceExit(this)
         implicit none
         type(surface),intent(in) :: this
         logical :: exit
         write(*,*) "exit: ", this%exit
       end subroutine

       subroutine printSurface(this)
         implicit none
         type(surface), intent(in) :: this
         write(*,*) 'Printed data for surface'
         write(*,*) '***************'
         call printSurfaceId(this)
         call printLine(this%l)
         call printSurfaceEntrance(this)
         call printSurfaceExit(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeSurfaceId(this,dir,parentUnit)
         implicit none
         type(surface),intent(in) :: this
         integer :: id
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "id: ", this%id
         else
           call newAndOpen(dir,"id")
           write(NewU,*) "id: ", this%id
           call closeAndMessage(NewU,"id")
         endif
       end subroutine

       subroutine writeSurfaceL(this,dir)
         implicit none
         type(surface),intent(in) :: this
         character(len=*),intent(in) :: dir
         type(line) :: l
         call writeLine(this%l,dir)
       end subroutine

       subroutine writeSurfaceEntrance(this,dir,parentUnit)
         implicit none
         type(surface),intent(in) :: this
         logical :: entrance
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "entrance: ", this%entrance
         else
           call newAndOpen(dir,"entrance")
           write(NewU,*) "entrance: ", this%entrance
           call closeAndMessage(NewU,"entrance")
         endif
       end subroutine

       subroutine writeSurfaceExit(this,dir,parentUnit)
         implicit none
         type(surface),intent(in) :: this
         logical :: exit
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "exit: ", this%exit
         else
           call newAndOpen(dir,"exit")
           write(NewU,*) "exit: ", this%exit
           call closeAndMessage(NewU,"exit")
         endif
       end subroutine

       subroutine writeSurface(this,dir,parentUnit)
         implicit none
         type(surface), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Surface.txt")
         endif
         call writeSurfaceId(this,dir,NewU)
         call writeLine(this%l,dir,NewU)
         call writeSurfaceEntrance(this,dir,NewU)
         call writeSurfaceExit(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for surface written to file +++'
         endif
       end subroutine

         include '../helper/surface_helper.f'

       end module surface_mod