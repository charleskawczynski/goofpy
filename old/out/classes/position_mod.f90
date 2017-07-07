       module position_mod
       use allClassFuncs_mod
       use constants_mod
       use vector_mod


       type position
         private
         type(vector) :: initial
         type(vector) :: instant
         type(vector) :: final
       endtype

       contains

       function setPosition(initial,instant,final) result(this)
         implicit none
         type(position) :: this
         type(vector), intent(in) :: initial
         type(vector), intent(in) :: instant
         type(vector), intent(in) :: final
         call SetPositionInitial(this,initial)
         call SetPositionInstant(this,instant)
         call SetPositionFinal(this,final)
       end function

       subroutine setPositionInitial(this,initial)
         implicit none
         type(position),intent(inout) :: this
         type(vector), intent(in) :: initial
         this%initial = initial
       end subroutine

       subroutine setPositionInstant(this,instant)
         implicit none
         type(position),intent(inout) :: this
         type(vector), intent(in) :: instant
         this%instant = instant
       end subroutine

       subroutine setPositionFinal(this,final)
         implicit none
         type(position),intent(inout) :: this
         type(vector), intent(in) :: final
         this%final = final
       end subroutine

       function getPositionInitial(this) result(initial)
         implicit none
         type(position),intent(in) :: this
         type(vector) :: initial
         initial = this%initial
       end function

       function getPositionInstant(this) result(instant)
         implicit none
         type(position),intent(in) :: this
         type(vector) :: instant
         instant = this%instant
       end function

       function getPositionFinal(this) result(final)
         implicit none
         type(position),intent(in) :: this
         type(vector) :: final
         final = this%final
       end function

       subroutine getPosition(this,initial,instant,final)
         implicit none
         type(position),intent(in) :: this
         type(vector), intent(out) :: initial
         type(vector), intent(out) :: instant
         type(vector), intent(out) :: final
         initial = getPositionInitial(this)
         instant = getPositionInstant(this)
         final = getPositionFinal(this)
       end subroutine

       subroutine printPositionInitial(this)
         implicit none
         type(position),intent(in) :: this
         type(vector) :: initial
         call printVector(this%initial)
       end subroutine

       subroutine printPositionInstant(this)
         implicit none
         type(position),intent(in) :: this
         type(vector) :: instant
         call printVector(this%instant)
       end subroutine

       subroutine printPositionFinal(this)
         implicit none
         type(position),intent(in) :: this
         type(vector) :: final
         call printVector(this%final)
       end subroutine

       subroutine printPosition(this)
         implicit none
         type(position), intent(in) :: this
         write(*,*) 'Printed data for position'
         write(*,*) '***************'
         call printVector(this%initial)
         call printVector(this%instant)
         call printVector(this%final)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writePositionInitial(this,dir)
         implicit none
         type(position),intent(in) :: this
         character(len=*),intent(in) :: dir
         type(vector) :: initial
         call writeVector(this%initial,dir)
       end subroutine

       subroutine writePositionInstant(this,dir)
         implicit none
         type(position),intent(in) :: this
         character(len=*),intent(in) :: dir
         type(vector) :: instant
         call writeVector(this%instant,dir)
       end subroutine

       subroutine writePositionFinal(this,dir)
         implicit none
         type(position),intent(in) :: this
         character(len=*),intent(in) :: dir
         type(vector) :: final
         call writeVector(this%final,dir)
       end subroutine

       subroutine writePosition(this,dir,parentUnit)
         implicit none
         type(position), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Position.txt")
         endif
         call writeVector(this%initial,dir,NewU)
         call writeVector(this%instant,dir,NewU)
         call writeVector(this%final,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for position written to file +++'
         endif
       end subroutine

         include '../helper/position_helper.f'

       end module position_mod