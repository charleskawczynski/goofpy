       module line_mod
       use allClassFuncs_mod
       use constants_mod
       use simParams_mod
       use debugger_mod
       use vector_mod
       use BCs_mod
       use hLine_mod
       use vLine_mod
       use eLine_mod


       type line
         private
         integer :: id
         type(hLine), pointer :: ptr_h
         type(vLine), pointer :: ptr_v
         type(eLine), pointer :: ptr_e
       endtype

       contains

       function setLine(id,h,v,e) result(this)
         implicit none
         type(line) :: this
         integer, intent(in) :: id
         type(hLine), intent(in),optional,target :: h
         type(vLine), intent(in),optional,target :: v
         type(eLine), intent(in),optional,target :: e
         call SetLineId(this,id)
         if (present(h)) then
           call SetPolyHline(this,h)
         nullify(this%ptr_v)
         nullify(this%ptr_e)
         endif
         if (present(v)) then
           call SetPolyVline(this,v)
         nullify(this%ptr_h)
         nullify(this%ptr_e)
         endif
         if (present(e)) then
           call SetPolyEline(this,e)
         nullify(this%ptr_h)
         nullify(this%ptr_v)
         endif
       end function

       subroutine setLineId(this,id)
         implicit none
         type(line),intent(inout) :: this
         integer, intent(in) :: id
         this%id = id
       end subroutine

       subroutine setPolyHline(this,h)
         implicit none
         type(line),intent(inout) :: this
         type(hLine), intent(in),optional,target :: h
         this%ptr_h => h
         nullify(this%ptr_v)
         nullify(this%ptr_e)
       end subroutine

       subroutine setPolyVline(this,v)
         implicit none
         type(line),intent(inout) :: this
         type(vLine), intent(in),optional,target :: v
         this%ptr_v => v
         nullify(this%ptr_h)
         nullify(this%ptr_e)
       end subroutine

       subroutine setPolyEline(this,e)
         implicit none
         type(line),intent(inout) :: this
         type(eLine), intent(in),optional,target :: e
         this%ptr_e => e
         nullify(this%ptr_h)
         nullify(this%ptr_v)
       end subroutine

       function getLineId(this) result(id)
         implicit none
         type(line),intent(in) :: this
         integer :: id
         id = this%id
       end function

       function getPolyHline(this) result(h)
         implicit none
         type(line),intent(in) :: this
         type(hLine),target :: h
         h = this%ptr_h
       end function

       function getPolyVline(this) result(v)
         implicit none
         type(line),intent(in) :: this
         type(vLine),target :: v
         v = this%ptr_v
       end function

       function getPolyEline(this) result(e)
         implicit none
         type(line),intent(in) :: this
         type(eLine),target :: e
         e = this%ptr_e
       end function

       subroutine getLine(this,id,h,v,e)
         implicit none
         type(line),intent(in) :: this
         integer, intent(out) :: id
         type(hLine), intent(out),optional,target :: h
         type(vLine), intent(out),optional,target :: v
         type(eLine), intent(out),optional,target :: e
         id = getLineId(this)
         if (associated(this%ptr_h)) then
           h = getPolyHline(this)
         endif
         if (associated(this%ptr_v)) then
           v = getPolyVline(this)
         endif
         if (associated(this%ptr_e)) then
           e = getPolyEline(this)
         endif
       end subroutine

       subroutine printLineId(this)
         implicit none
         type(line),intent(in) :: this
         integer :: id
         write(*,*) "id: ", this%id
       end subroutine

       subroutine printLineH(this)
         implicit none
         type(line),intent(in) :: this
         call printHline(this%ptr_h)
       end subroutine

       subroutine printLineV(this)
         implicit none
         type(line),intent(in) :: this
         call printVline(this%ptr_v)
       end subroutine

       subroutine printLineE(this)
         implicit none
         type(line),intent(in) :: this
         call printEline(this%ptr_e)
       end subroutine

       subroutine printLine(this)
         implicit none
         type(line), intent(in) :: this
         write(*,*) 'Printed data for line'
         write(*,*) '***************'
         call printLineId(this)
         if (associated(this%ptr_h)) then
           call printHlineh(this%ptr_h)
         endif
         if (associated(this%ptr_v)) then
           call printVlinev(this%ptr_v)
         endif
         if (associated(this%ptr_e)) then
           call printElinee(this%ptr_e)
         endif
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeLineId(this,dir,parentUnit)
         implicit none
         type(line),intent(in) :: this
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

       subroutine writeLineH(this,dir)
         implicit none
         type(line),intent(in) :: this
         character(len=*),intent(in) :: dir
         call writeHline(this%ptr_h,dir)
       end subroutine

       subroutine writeLineV(this,dir)
         implicit none
         type(line),intent(in) :: this
         character(len=*),intent(in) :: dir
         call writeVline(this%ptr_v,dir)
       end subroutine

       subroutine writeLineE(this,dir)
         implicit none
         type(line),intent(in) :: this
         character(len=*),intent(in) :: dir
         call writeEline(this%ptr_e,dir)
       end subroutine

       subroutine writeLine(this,dir,parentUnit)
         implicit none
         type(line), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Line.txt")
         endif
         call writeLineId(this,dir,NewU)
         if (associated(this%ptr_h)) then
           call writeHline(this%ptr_h,dir,NewU)
         endif
         if (associated(this%ptr_v)) then
           call writeVline(this%ptr_v,dir,NewU)
         endif
         if (associated(this%ptr_e)) then
           call writeEline(this%ptr_e,dir,NewU)
         endif
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for line written to file +++'
         endif
       end subroutine

         include '../helper/line_helper.f'

       end module line_mod