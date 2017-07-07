       module grid_field_mod
       use allClassFuncs_mod


       type grid_field
         integer :: i
         integer :: j
         integer :: k
         integer :: i_geometry
       endtype

       contains

       function setGrid_field(i,j,k,i_geometry) result(this)
         implicit none
         type(grid_field) :: this
         integer, intent(in) :: i
         integer, intent(in) :: j
         integer, intent(in) :: k
         integer, intent(in) :: i_geometry
         call SetGrid_fieldI(this,i)
         call SetGrid_fieldJ(this,j)
         call SetGrid_fieldK(this,k)
         call SetGrid_fieldI_geometry(this,i_geometry)
       end function

       subroutine setGrid_fieldI(this,i)
         implicit none
         type(grid_field),intent(inout) :: this
         integer, intent(in) :: i
         this%i = i
       end subroutine

       subroutine setGrid_fieldJ(this,j)
         implicit none
         type(grid_field),intent(inout) :: this
         integer, intent(in) :: j
         this%j = j
       end subroutine

       subroutine setGrid_fieldK(this,k)
         implicit none
         type(grid_field),intent(inout) :: this
         integer, intent(in) :: k
         this%k = k
       end subroutine

       subroutine setGrid_fieldI_geometry(this,i_geometry)
         implicit none
         type(grid_field),intent(inout) :: this
         integer, intent(in) :: i_geometry
         this%i_geometry = i_geometry
       end subroutine

       function getGrid_fieldI(this) result(i)
         implicit none
         type(grid_field),intent(in) :: this
         integer :: i
         i = this%i
       end function

       function getGrid_fieldJ(this) result(j)
         implicit none
         type(grid_field),intent(in) :: this
         integer :: j
         j = this%j
       end function

       function getGrid_fieldK(this) result(k)
         implicit none
         type(grid_field),intent(in) :: this
         integer :: k
         k = this%k
       end function

       function getGrid_fieldI_geometry(this) result(i_geometry)
         implicit none
         type(grid_field),intent(in) :: this
         integer :: i_geometry
         i_geometry = this%i_geometry
       end function

       subroutine getGrid_field(this,i,j,k,i_geometry)
         implicit none
         type(grid_field),intent(in) :: this
         integer, intent(out) :: i
         integer, intent(out) :: j
         integer, intent(out) :: k
         integer, intent(out) :: i_geometry
         i = getGrid_fieldI(this)
         j = getGrid_fieldJ(this)
         k = getGrid_fieldK(this)
         i_geometry = getGrid_fieldI_geometry(this)
       end subroutine

       subroutine printGrid_fieldI(this)
         implicit none
         type(grid_field),intent(in) :: this
         integer :: i
         write(*,*) "i: ", this%i
       end subroutine

       subroutine printGrid_fieldJ(this)
         implicit none
         type(grid_field),intent(in) :: this
         integer :: j
         write(*,*) "j: ", this%j
       end subroutine

       subroutine printGrid_fieldK(this)
         implicit none
         type(grid_field),intent(in) :: this
         integer :: k
         write(*,*) "k: ", this%k
       end subroutine

       subroutine printGrid_fieldI_geometry(this)
         implicit none
         type(grid_field),intent(in) :: this
         integer :: i_geometry
         write(*,*) "i_geometry: ", this%i_geometry
       end subroutine

       subroutine printGrid_field(this)
         implicit none
         type(grid_field), intent(in) :: this
         write(*,*) 'Printed data for grid_field'
         write(*,*) '***************'
         call printGrid_fieldI(this)
         call printGrid_fieldJ(this)
         call printGrid_fieldK(this)
         call printGrid_fieldI_geometry(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeGrid_fieldI(this,dir,parentUnit)
         implicit none
         type(grid_field),intent(in) :: this
         integer :: i
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "i: ", this%i
         else
           call newAndOpen(dir,"i")
           write(NewU,*) "i: ", this%i
           call closeAndMessage(NewU,"i")
         endif
       end subroutine

       subroutine writeGrid_fieldJ(this,dir,parentUnit)
         implicit none
         type(grid_field),intent(in) :: this
         integer :: j
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "j: ", this%j
         else
           call newAndOpen(dir,"j")
           write(NewU,*) "j: ", this%j
           call closeAndMessage(NewU,"j")
         endif
       end subroutine

       subroutine writeGrid_fieldK(this,dir,parentUnit)
         implicit none
         type(grid_field),intent(in) :: this
         integer :: k
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "k: ", this%k
         else
           call newAndOpen(dir,"k")
           write(NewU,*) "k: ", this%k
           call closeAndMessage(NewU,"k")
         endif
       end subroutine

       subroutine writeGrid_fieldI_geometry(this,dir,parentUnit)
         implicit none
         type(grid_field),intent(in) :: this
         integer :: i_geometry
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "i_geometry: ", this%i_geometry
         else
           call newAndOpen(dir,"i_geometry")
           write(NewU,*) "i_geometry: ", this%i_geometry
           call closeAndMessage(NewU,"i_geometry")
         endif
       end subroutine

       subroutine writeGrid_field(this,dir,parentUnit)
         implicit none
         type(grid_field), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Grid_field.txt")
         endif
         call writeGrid_fieldI(this,dir,NewU)
         call writeGrid_fieldJ(this,dir,NewU)
         call writeGrid_fieldK(this,dir,NewU)
         call writeGrid_fieldI_geometry(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for grid_field written to file +++'
         endif
       end subroutine


       end module grid_field_mod