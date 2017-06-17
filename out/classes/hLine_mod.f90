       module hLine_mod
       use allClassFuncs_mod
       use constants_mod
       use simParams_mod
       use debugger_mod
       use vector_mod
       use BCs_mod


       type hLine
         private
         integer :: id
         type(BCs) :: BC
         real(kind=dp_num) :: y
         logical :: normalUp
       endtype

       contains

       function setHline(id,BC,y,normalUp) result(this)
         implicit none
         type(hLine) :: this
         integer, intent(in) :: id
         type(BCs), intent(in) :: BC
         real(kind=dp_num), intent(in) :: y
         logical, intent(in) :: normalUp
         call SetHlineId(this,id)
         call SetHlineBc(this,BC)
         call SetHlineY(this,y)
         call SetHlineNormalup(this,normalUp)
       end function

       subroutine setHlineId(this,id)
         implicit none
         type(hLine),intent(inout) :: this
         integer, intent(in) :: id
         this%id = id
       end subroutine

       subroutine setHlineBc(this,BC)
         implicit none
         type(hLine),intent(inout) :: this
         type(BCs), intent(in) :: BC
         this%BC = BC
       end subroutine

       subroutine setHlineY(this,y)
         implicit none
         type(hLine),intent(inout) :: this
         real(kind=dp_num), intent(in) :: y
         this%y = y
       end subroutine

       subroutine setHlineNormalup(this,normalUp)
         implicit none
         type(hLine),intent(inout) :: this
         logical, intent(in) :: normalUp
         this%normalUp = normalUp
       end subroutine

       function getHlineId(this) result(id)
         implicit none
         type(hLine),intent(in) :: this
         integer :: id
         id = this%id
       end function

       function getHlineBc(this) result(BC)
         implicit none
         type(hLine),intent(in) :: this
         type(BCs) :: BC
         BC = this%BC
       end function

       function getHlineY(this) result(y)
         implicit none
         type(hLine),intent(in) :: this
         real(kind=dp_num) :: y
         y = this%y
       end function

       function getHlineNormalup(this) result(normalUp)
         implicit none
         type(hLine),intent(in) :: this
         logical :: normalUp
         normalUp = this%normalUp
       end function

       subroutine getHline(this,id,BC,y,normalUp)
         implicit none
         type(hLine),intent(in) :: this
         integer, intent(out) :: id
         type(BCs), intent(out) :: BC
         real(kind=dp_num), intent(out) :: y
         logical, intent(out) :: normalUp
         id = getHlineId(this)
         BC = getHlineBc(this)
         y = getHlineY(this)
         normalUp = getHlineNormalup(this)
       end subroutine

       subroutine printHlineId(this)
         implicit none
         type(hLine),intent(in) :: this
         integer :: id
         write(*,*) "id: ", this%id
       end subroutine

       subroutine printHlineBc(this)
         implicit none
         type(hLine),intent(in) :: this
         type(BCs) :: BC
         call printBcs(this%BC)
       end subroutine

       subroutine printHlineY(this)
         implicit none
         type(hLine),intent(in) :: this
         real(kind=dp_num) :: y
         write(*,*) "y: ", this%y
       end subroutine

       subroutine printHlineNormalup(this)
         implicit none
         type(hLine),intent(in) :: this
         logical :: normalUp
         write(*,*) "normalUp: ", this%normalUp
       end subroutine

       subroutine printHline(this)
         implicit none
         type(hLine), intent(in) :: this
         write(*,*) 'Printed data for hLine'
         write(*,*) '***************'
         call printHlineId(this)
         call printBcs(this%BC)
         call printHlineY(this)
         call printHlineNormalup(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeHlineId(this,dir,parentUnit)
         implicit none
         type(hLine),intent(in) :: this
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

       subroutine writeHlineBc(this,dir)
         implicit none
         type(hLine),intent(in) :: this
         character(len=*),intent(in) :: dir
         type(BCs) :: BC
         call writeBcs(this%BC,dir)
       end subroutine

       subroutine writeHlineY(this,dir,parentUnit)
         implicit none
         type(hLine),intent(in) :: this
         real(kind=dp_num) :: y
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "y: ", this%y
         else
           call newAndOpen(dir,"y")
           write(NewU,*) "y: ", this%y
           call closeAndMessage(NewU,"y")
         endif
       end subroutine

       subroutine writeHlineNormalup(this,dir,parentUnit)
         implicit none
         type(hLine),intent(in) :: this
         logical :: normalUp
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "normalUp: ", this%normalUp
         else
           call newAndOpen(dir,"normalUp")
           write(NewU,*) "normalUp: ", this%normalUp
           call closeAndMessage(NewU,"normalUp")
         endif
       end subroutine

       subroutine writeHline(this,dir,parentUnit)
         implicit none
         type(hLine), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Hline.txt")
         endif
         call writeHlineId(this,dir,NewU)
         call writeBcs(this%BC,dir,NewU)
         call writeHlineY(this,dir,NewU)
         call writeHlineNormalup(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for hLine written to file +++'
         endif
       end subroutine

         include '../helper/hLine_helper.f'

       end module hLine_mod