       module vLine_mod
       use allClassFuncs_mod
       use constants_mod
       use simParams_mod
       use debugger_mod
       use vector_mod
       use BCs_mod


       type vLine
         private
         integer :: id
         type(BCs) :: BC
         real(cp) :: x
         logical :: normalRight
       endtype

       contains

       function setVline(id,BC,x,normalRight) result(this)
         implicit none
         type(vLine) :: this
         integer, intent(in) :: id
         type(BCs), intent(in) :: BC
         real(cp), intent(in) :: x
         logical, intent(in) :: normalRight
         call SetVlineId(this,id)
         call SetVlineBc(this,BC)
         call SetVlineX(this,x)
         call SetVlineNormalright(this,normalRight)
       end function

       subroutine setVlineId(this,id)
         implicit none
         type(vLine),intent(inout) :: this
         integer, intent(in) :: id
         this%id = id
       end subroutine

       subroutine setVlineBc(this,BC)
         implicit none
         type(vLine),intent(inout) :: this
         type(BCs), intent(in) :: BC
         this%BC = BC
       end subroutine

       subroutine setVlineX(this,x)
         implicit none
         type(vLine),intent(inout) :: this
         real(cp), intent(in) :: x
         this%x = x
       end subroutine

       subroutine setVlineNormalright(this,normalRight)
         implicit none
         type(vLine),intent(inout) :: this
         logical, intent(in) :: normalRight
         this%normalRight = normalRight
       end subroutine

       function getVlineId(this) result(id)
         implicit none
         type(vLine),intent(in) :: this
         integer :: id
         id = this%id
       end function

       function getVlineBc(this) result(BC)
         implicit none
         type(vLine),intent(in) :: this
         type(BCs) :: BC
         BC = this%BC
       end function

       function getVlineX(this) result(x)
         implicit none
         type(vLine),intent(in) :: this
         real(cp) :: x
         x = this%x
       end function

       function getVlineNormalright(this) result(normalRight)
         implicit none
         type(vLine),intent(in) :: this
         logical :: normalRight
         normalRight = this%normalRight
       end function

       subroutine getVline(this,id,BC,x,normalRight)
         implicit none
         type(vLine),intent(in) :: this
         integer, intent(out) :: id
         type(BCs), intent(out) :: BC
         real(cp), intent(out) :: x
         logical, intent(out) :: normalRight
         id = getVlineId(this)
         BC = getVlineBc(this)
         x = getVlineX(this)
         normalRight = getVlineNormalright(this)
       end subroutine

       subroutine printVlineId(this)
         implicit none
         type(vLine),intent(in) :: this
         integer :: id
         write(*,*) "id: ", this%id
       end subroutine

       subroutine printVlineBc(this)
         implicit none
         type(vLine),intent(in) :: this
         type(BCs) :: BC
         call printBcs(this%BC)
       end subroutine

       subroutine printVlineX(this)
         implicit none
         type(vLine),intent(in) :: this
         real(cp) :: x
         write(*,*) "x: ", this%x
       end subroutine

       subroutine printVlineNormalright(this)
         implicit none
         type(vLine),intent(in) :: this
         logical :: normalRight
         write(*,*) "normalRight: ", this%normalRight
       end subroutine

       subroutine printVline(this)
         implicit none
         type(vLine), intent(in) :: this
         write(*,*) 'Printed data for vLine'
         write(*,*) '***************'
         call printVlineId(this)
         call printBcs(this%BC)
         call printVlineX(this)
         call printVlineNormalright(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeVlineId(this,dir,parentUnit)
         implicit none
         type(vLine),intent(in) :: this
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

       subroutine writeVlineBc(this,dir)
         implicit none
         type(vLine),intent(in) :: this
         character(len=*),intent(in) :: dir
         type(BCs) :: BC
         call writeBcs(this%BC,dir)
       end subroutine

       subroutine writeVlineX(this,dir,parentUnit)
         implicit none
         type(vLine),intent(in) :: this
         real(cp) :: x
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "x: ", this%x
         else
           call newAndOpen(dir,"x")
           write(NewU,*) "x: ", this%x
           call closeAndMessage(NewU,"x")
         endif
       end subroutine

       subroutine writeVlineNormalright(this,dir,parentUnit)
         implicit none
         type(vLine),intent(in) :: this
         logical :: normalRight
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "normalRight: ", this%normalRight
         else
           call newAndOpen(dir,"normalRight")
           write(NewU,*) "normalRight: ", this%normalRight
           call closeAndMessage(NewU,"normalRight")
         endif
       end subroutine

       subroutine writeVline(this,dir,parentUnit)
         implicit none
         type(vLine), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Vline.txt")
         endif
         call writeVlineId(this,dir,NewU)
         call writeBcs(this%BC,dir,NewU)
         call writeVlineX(this,dir,NewU)
         call writeVlineNormalright(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for vLine written to file +++'
         endif
       end subroutine

         include '../helper/vLine_helper.f'

       end module vLine_mod