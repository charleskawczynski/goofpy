       module vector_mod
       use allClassFuncs_mod
       use constants_mod
       use counters_mod
       use simParams_mod
       use debugger_mod


       type vector
         private
         real(kind=dp_num) :: x
         real(kind=dp_num) :: y
         real(kind=dp_num) :: z
         real(kind=dp_num) :: mag
         real(kind=dp_num) :: slope
       endtype

       contains

       function setVector(x,y,z) result(this)
         implicit none
         type(vector) :: this
         real(kind=dp_num), intent(in) :: x
         real(kind=dp_num), intent(in) :: y
         real(kind=dp_num), intent(in) :: z
         call SetVectorX(this,x)
         call SetVectorY(this,y)
         call SetVectorZ(this,z)
         call CalcVectorMag(this)
         call CalcVectorSlope(this)
       end function

       subroutine setVectorX(this,x)
         implicit none
         type(vector),intent(inout) :: this
         real(kind=dp_num), intent(in) :: x
         this%x = x
       end subroutine

       subroutine setVectorY(this,y)
         implicit none
         type(vector),intent(inout) :: this
         real(kind=dp_num), intent(in) :: y
         this%y = y
       end subroutine

       subroutine setVectorZ(this,z)
         implicit none
         type(vector),intent(inout) :: this
         real(kind=dp_num), intent(in) :: z
         this%z = z
       end subroutine

       subroutine setVectorMag(this,mag)
         implicit none
         type(vector),intent(inout) :: this
         real(kind=dp_num), intent(in) :: mag
         this%mag = mag
       end subroutine

       subroutine setVectorSlope(this,slope)
         implicit none
         type(vector),intent(inout) :: this
         real(kind=dp_num), intent(in) :: slope
         this%slope = slope
       end subroutine

       function getVectorX(this) result(x)
         implicit none
         type(vector),intent(in) :: this
         real(kind=dp_num) :: x
         x = this%x
       end function

       function getVectorY(this) result(y)
         implicit none
         type(vector),intent(in) :: this
         real(kind=dp_num) :: y
         y = this%y
       end function

       function getVectorZ(this) result(z)
         implicit none
         type(vector),intent(in) :: this
         real(kind=dp_num) :: z
         z = this%z
       end function

       function getVectorMag(this) result(mag)
         implicit none
         type(vector),intent(in) :: this
         real(kind=dp_num) :: mag
         mag = this%mag
       end function

       function getVectorSlope(this) result(slope)
         implicit none
         type(vector),intent(in) :: this
         real(kind=dp_num) :: slope
         slope = this%slope
       end function

       subroutine getVector(this,x,y,z,mag,slope)
         implicit none
         type(vector),intent(in) :: this
         real(kind=dp_num), intent(out) :: x
         real(kind=dp_num), intent(out) :: y
         real(kind=dp_num), intent(out) :: z
         real(kind=dp_num), intent(out) :: mag
         real(kind=dp_num), intent(out) :: slope
         x = getVectorX(this)
         y = getVectorY(this)
         z = getVectorZ(this)
         mag = getVectorMag(this)
         slope = getVectorSlope(this)
       end subroutine

       subroutine printVectorX(this)
         implicit none
         type(vector),intent(in) :: this
         real(kind=dp_num) :: x
         write(*,*) "x: ", this%x
       end subroutine

       subroutine printVectorY(this)
         implicit none
         type(vector),intent(in) :: this
         real(kind=dp_num) :: y
         write(*,*) "y: ", this%y
       end subroutine

       subroutine printVectorZ(this)
         implicit none
         type(vector),intent(in) :: this
         real(kind=dp_num) :: z
         write(*,*) "z: ", this%z
       end subroutine

       subroutine printVectorMag(this)
         implicit none
         type(vector),intent(in) :: this
         real(kind=dp_num) :: mag
         write(*,*) "mag: ", this%mag
       end subroutine

       subroutine printVectorSlope(this)
         implicit none
         type(vector),intent(in) :: this
         real(kind=dp_num) :: slope
         write(*,*) "slope: ", this%slope
       end subroutine

       subroutine printVector(this)
         implicit none
         type(vector), intent(in) :: this
         write(*,*) 'Printed data for vector'
         write(*,*) '***************'
         call printVectorX(this)
         call printVectorY(this)
         call printVectorZ(this)
         call printVectorMag(this)
         call printVectorSlope(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeVectorX(this,dir,parentUnit)
         implicit none
         type(vector),intent(in) :: this
         real(kind=dp_num) :: x
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

       subroutine writeVectorY(this,dir,parentUnit)
         implicit none
         type(vector),intent(in) :: this
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

       subroutine writeVectorZ(this,dir,parentUnit)
         implicit none
         type(vector),intent(in) :: this
         real(kind=dp_num) :: z
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "z: ", this%z
         else
           call newAndOpen(dir,"z")
           write(NewU,*) "z: ", this%z
           call closeAndMessage(NewU,"z")
         endif
       end subroutine

       subroutine writeVectorMag(this,dir,parentUnit)
         implicit none
         type(vector),intent(in) :: this
         real(kind=dp_num) :: mag
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "mag: ", this%mag
         else
           call newAndOpen(dir,"mag")
           write(NewU,*) "mag: ", this%mag
           call closeAndMessage(NewU,"mag")
         endif
       end subroutine

       subroutine writeVectorSlope(this,dir,parentUnit)
         implicit none
         type(vector),intent(in) :: this
         real(kind=dp_num) :: slope
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "slope: ", this%slope
         else
           call newAndOpen(dir,"slope")
           write(NewU,*) "slope: ", this%slope
           call closeAndMessage(NewU,"slope")
         endif
       end subroutine

       subroutine writeVector(this,dir,parentUnit)
         implicit none
         type(vector), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Vector.txt")
         endif
         call writeVectorX(this,dir,NewU)
         call writeVectorY(this,dir,NewU)
         call writeVectorZ(this,dir,NewU)
         call writeVectorMag(this,dir,NewU)
         call writeVectorSlope(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for vector written to file +++'
         endif
       end subroutine

         include '../helper/vector_helper.f'
         include '../calc/calc_mag.f'
         include '../calc/calc_slope.f'

       end module vector_mod