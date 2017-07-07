       module BCs_mod
       use allClassFuncs_mod
       use constants_mod
       use simParams_mod
       use debugger_mod
       use vector_mod


       type BCs
         private
         real(cp) :: x_min
         real(cp) :: x_max
         real(cp) :: y_min
         real(cp) :: y_max
         logical :: xmin_IO
         logical :: xmax_IO
         logical :: ymin_IO
         logical :: ymax_IO
       endtype

       contains

       function setBcs(x_min,x_max,y_min,y_max,xmin_IO,xmax_IO,ymin_IO,
     &   ymax_IO) result(this)
         implicit none
         type(BCs) :: this
         real(cp), intent(in) :: x_min
         real(cp), intent(in) :: x_max
         real(cp), intent(in) :: y_min
         real(cp), intent(in) :: y_max
         logical, intent(in) :: xmin_IO
         logical, intent(in) :: xmax_IO
         logical, intent(in) :: ymin_IO
         logical, intent(in) :: ymax_IO
         call SetBcsX_min(this,x_min)
         call SetBcsX_max(this,x_max)
         call SetBcsY_min(this,y_min)
         call SetBcsY_max(this,y_max)
         call SetBcsXmin_io(this,xmin_IO)
         call SetBcsXmax_io(this,xmax_IO)
         call SetBcsYmin_io(this,ymin_IO)
         call SetBcsYmax_io(this,ymax_IO)
       end function

       subroutine setBcsX_min(this,x_min)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp), intent(in) :: x_min
         this%x_min = x_min
       end subroutine

       subroutine setBcsX_max(this,x_max)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp), intent(in) :: x_max
         this%x_max = x_max
       end subroutine

       subroutine setBcsY_min(this,y_min)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp), intent(in) :: y_min
         this%y_min = y_min
       end subroutine

       subroutine setBcsY_max(this,y_max)
         implicit none
         type(BCs),intent(inout) :: this
         real(cp), intent(in) :: y_max
         this%y_max = y_max
       end subroutine

       subroutine setBcsXmin_io(this,xmin_IO)
         implicit none
         type(BCs),intent(inout) :: this
         logical, intent(in) :: xmin_IO
         this%xmin_IO = xmin_IO
       end subroutine

       subroutine setBcsXmax_io(this,xmax_IO)
         implicit none
         type(BCs),intent(inout) :: this
         logical, intent(in) :: xmax_IO
         this%xmax_IO = xmax_IO
       end subroutine

       subroutine setBcsYmin_io(this,ymin_IO)
         implicit none
         type(BCs),intent(inout) :: this
         logical, intent(in) :: ymin_IO
         this%ymin_IO = ymin_IO
       end subroutine

       subroutine setBcsYmax_io(this,ymax_IO)
         implicit none
         type(BCs),intent(inout) :: this
         logical, intent(in) :: ymax_IO
         this%ymax_IO = ymax_IO
       end subroutine

       function getBcsX_min(this) result(x_min)
         implicit none
         type(BCs),intent(in) :: this
         real(cp) :: x_min
         x_min = this%x_min
       end function

       function getBcsX_max(this) result(x_max)
         implicit none
         type(BCs),intent(in) :: this
         real(cp) :: x_max
         x_max = this%x_max
       end function

       function getBcsY_min(this) result(y_min)
         implicit none
         type(BCs),intent(in) :: this
         real(cp) :: y_min
         y_min = this%y_min
       end function

       function getBcsY_max(this) result(y_max)
         implicit none
         type(BCs),intent(in) :: this
         real(cp) :: y_max
         y_max = this%y_max
       end function

       function getBcsXmin_io(this) result(xmin_IO)
         implicit none
         type(BCs),intent(in) :: this
         logical :: xmin_IO
         xmin_IO = this%xmin_IO
       end function

       function getBcsXmax_io(this) result(xmax_IO)
         implicit none
         type(BCs),intent(in) :: this
         logical :: xmax_IO
         xmax_IO = this%xmax_IO
       end function

       function getBcsYmin_io(this) result(ymin_IO)
         implicit none
         type(BCs),intent(in) :: this
         logical :: ymin_IO
         ymin_IO = this%ymin_IO
       end function

       function getBcsYmax_io(this) result(ymax_IO)
         implicit none
         type(BCs),intent(in) :: this
         logical :: ymax_IO
         ymax_IO = this%ymax_IO
       end function

       subroutine getBcs(this,x_min,x_max,y_min,y_max,xmin_IO,xmax_IO,
     &   ymin_IO,ymax_IO)
         implicit none
         type(BCs),intent(in) :: this
         real(cp), intent(out) :: x_min
         real(cp), intent(out) :: x_max
         real(cp), intent(out) :: y_min
         real(cp), intent(out) :: y_max
         logical, intent(out) :: xmin_IO
         logical, intent(out) :: xmax_IO
         logical, intent(out) :: ymin_IO
         logical, intent(out) :: ymax_IO
         x_min = getBcsX_min(this)
         x_max = getBcsX_max(this)
         y_min = getBcsY_min(this)
         y_max = getBcsY_max(this)
         xmin_IO = getBcsXmin_io(this)
         xmax_IO = getBcsXmax_io(this)
         ymin_IO = getBcsYmin_io(this)
         ymax_IO = getBcsYmax_io(this)
       end subroutine

       subroutine printBcsX_min(this)
         implicit none
         type(BCs),intent(in) :: this
         real(cp) :: x_min
         write(*,*) "x_min: ", this%x_min
       end subroutine

       subroutine printBcsX_max(this)
         implicit none
         type(BCs),intent(in) :: this
         real(cp) :: x_max
         write(*,*) "x_max: ", this%x_max
       end subroutine

       subroutine printBcsY_min(this)
         implicit none
         type(BCs),intent(in) :: this
         real(cp) :: y_min
         write(*,*) "y_min: ", this%y_min
       end subroutine

       subroutine printBcsY_max(this)
         implicit none
         type(BCs),intent(in) :: this
         real(cp) :: y_max
         write(*,*) "y_max: ", this%y_max
       end subroutine

       subroutine printBcsXmin_io(this)
         implicit none
         type(BCs),intent(in) :: this
         logical :: xmin_IO
         write(*,*) "xmin_IO: ", this%xmin_IO
       end subroutine

       subroutine printBcsXmax_io(this)
         implicit none
         type(BCs),intent(in) :: this
         logical :: xmax_IO
         write(*,*) "xmax_IO: ", this%xmax_IO
       end subroutine

       subroutine printBcsYmin_io(this)
         implicit none
         type(BCs),intent(in) :: this
         logical :: ymin_IO
         write(*,*) "ymin_IO: ", this%ymin_IO
       end subroutine

       subroutine printBcsYmax_io(this)
         implicit none
         type(BCs),intent(in) :: this
         logical :: ymax_IO
         write(*,*) "ymax_IO: ", this%ymax_IO
       end subroutine

       subroutine printBcs(this)
         implicit none
         type(BCs), intent(in) :: this
         write(*,*) 'Printed data for BCs'
         write(*,*) '***************'
         call printBcsX_min(this)
         call printBcsX_max(this)
         call printBcsY_min(this)
         call printBcsY_max(this)
         call printBcsXmin_io(this)
         call printBcsXmax_io(this)
         call printBcsYmin_io(this)
         call printBcsYmax_io(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeBcsX_min(this,dir,parentUnit)
         implicit none
         type(BCs),intent(in) :: this
         real(cp) :: x_min
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "x_min: ", this%x_min
         else
           call newAndOpen(dir,"x_min")
           write(NewU,*) "x_min: ", this%x_min
           call closeAndMessage(NewU,"x_min")
         endif
       end subroutine

       subroutine writeBcsX_max(this,dir,parentUnit)
         implicit none
         type(BCs),intent(in) :: this
         real(cp) :: x_max
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "x_max: ", this%x_max
         else
           call newAndOpen(dir,"x_max")
           write(NewU,*) "x_max: ", this%x_max
           call closeAndMessage(NewU,"x_max")
         endif
       end subroutine

       subroutine writeBcsY_min(this,dir,parentUnit)
         implicit none
         type(BCs),intent(in) :: this
         real(cp) :: y_min
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "y_min: ", this%y_min
         else
           call newAndOpen(dir,"y_min")
           write(NewU,*) "y_min: ", this%y_min
           call closeAndMessage(NewU,"y_min")
         endif
       end subroutine

       subroutine writeBcsY_max(this,dir,parentUnit)
         implicit none
         type(BCs),intent(in) :: this
         real(cp) :: y_max
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "y_max: ", this%y_max
         else
           call newAndOpen(dir,"y_max")
           write(NewU,*) "y_max: ", this%y_max
           call closeAndMessage(NewU,"y_max")
         endif
       end subroutine

       subroutine writeBcsXmin_io(this,dir,parentUnit)
         implicit none
         type(BCs),intent(in) :: this
         logical :: xmin_IO
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "xmin_IO: ", this%xmin_IO
         else
           call newAndOpen(dir,"xmin_IO")
           write(NewU,*) "xmin_IO: ", this%xmin_IO
           call closeAndMessage(NewU,"xmin_IO")
         endif
       end subroutine

       subroutine writeBcsXmax_io(this,dir,parentUnit)
         implicit none
         type(BCs),intent(in) :: this
         logical :: xmax_IO
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "xmax_IO: ", this%xmax_IO
         else
           call newAndOpen(dir,"xmax_IO")
           write(NewU,*) "xmax_IO: ", this%xmax_IO
           call closeAndMessage(NewU,"xmax_IO")
         endif
       end subroutine

       subroutine writeBcsYmin_io(this,dir,parentUnit)
         implicit none
         type(BCs),intent(in) :: this
         logical :: ymin_IO
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "ymin_IO: ", this%ymin_IO
         else
           call newAndOpen(dir,"ymin_IO")
           write(NewU,*) "ymin_IO: ", this%ymin_IO
           call closeAndMessage(NewU,"ymin_IO")
         endif
       end subroutine

       subroutine writeBcsYmax_io(this,dir,parentUnit)
         implicit none
         type(BCs),intent(in) :: this
         logical :: ymax_IO
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "ymax_IO: ", this%ymax_IO
         else
           call newAndOpen(dir,"ymax_IO")
           write(NewU,*) "ymax_IO: ", this%ymax_IO
           call closeAndMessage(NewU,"ymax_IO")
         endif
       end subroutine

       subroutine writeBcs(this,dir,parentUnit)
         implicit none
         type(BCs), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Bcs.txt")
         endif
         call writeBcsX_min(this,dir,NewU)
         call writeBcsX_max(this,dir,NewU)
         call writeBcsY_min(this,dir,NewU)
         call writeBcsY_max(this,dir,NewU)
         call writeBcsXmin_io(this,dir,NewU)
         call writeBcsXmax_io(this,dir,NewU)
         call writeBcsYmin_io(this,dir,NewU)
         call writeBcsYmax_io(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for BCs written to file +++'
         endif
       end subroutine

         include '../helper/BCs_helper.f'

       end module BCs_mod