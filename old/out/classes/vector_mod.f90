       module vector_mod
       use allClassFuncs_mod
       use constants_mod
       use counters_mod
       use simParams_mod
       use debugger_mod


       type vector
         private
         real(cp) :: x
         real(cp) :: y
         real(cp) :: z
         real(cp) :: mag
         real(cp) :: slope
       endtype

       contains

       function setVector(x,y,z) result(this)
         implicit none
         type(vector) :: this
         real(cp), intent(in) :: x
         real(cp), intent(in) :: y
         real(cp), intent(in) :: z
         call SetVectorX(this,x)
         call SetVectorY(this,y)
         call SetVectorZ(this,z)
       end function

       subroutine setVectorX(this,x)
         implicit none
         type(vector),intent(inout) :: this
         real(cp), intent(in) :: x
         this%x = x
       end subroutine

       subroutine setVectorY(this,y)
         implicit none
         type(vector),intent(inout) :: this
         real(cp), intent(in) :: y
         this%y = y
       end subroutine

       subroutine setVectorZ(this,z)
         implicit none
         type(vector),intent(inout) :: this
         real(cp), intent(in) :: z
         this%z = z
       end subroutine

       subroutine setVectorMag(this,mag)
         implicit none
         type(vector),intent(inout) :: this
         real(cp), intent(in) :: mag
         this%mag = mag
       end subroutine

       subroutine setVectorSlope(this,slope)
         implicit none
         type(vector),intent(inout) :: this
         real(cp), intent(in) :: slope
         this%slope = slope
       end subroutine

       function getVectorX(this) result(x)
         implicit none
         type(vector),intent(in) :: this
         real(cp) :: x
         x = this%x
       end function

       function getVectorY(this) result(y)
         implicit none
         type(vector),intent(in) :: this
         real(cp) :: y
         y = this%y
       end function

       function getVectorZ(this) result(z)
         implicit none
         type(vector),intent(in) :: this
         real(cp) :: z
         z = this%z
       end function

       function getVectorMag(this) result(mag)
         implicit none
         type(vector),intent(in) :: this
         real(cp) :: mag
         mag = this%mag
       end function

       function getVectorSlope(this) result(slope)
         implicit none
         type(vector),intent(in) :: this
         real(cp) :: slope
         slope = this%slope
       end function

       subroutine getVector(this,x,y,z,mag,slope)
         implicit none
         type(vector),intent(in) :: this
         real(cp), intent(out) :: x
         real(cp), intent(out) :: y
         real(cp), intent(out) :: z
         real(cp), intent(out) :: mag
         real(cp), intent(out) :: slope
         x = getVectorX(this)
         y = getVectorY(this)
         z = getVectorZ(this)
         mag = getVectorMag(this)
         slope = getVectorSlope(this)
       end subroutine

       subroutine printVectorX(this)
         implicit none
         type(vector),intent(in) :: this
         real(cp) :: x
         write(*,*) "x: ", this%x
       end subroutine

       subroutine printVectorY(this)
         implicit none
         type(vector),intent(in) :: this
         real(cp) :: y
         write(*,*) "y: ", this%y
       end subroutine

       subroutine printVectorZ(this)
         implicit none
         type(vector),intent(in) :: this
         real(cp) :: z
         write(*,*) "z: ", this%z
       end subroutine

       subroutine printVectorMag(this)
         implicit none
         type(vector),intent(in) :: this
         real(cp) :: mag
         write(*,*) "mag: ", this%mag
       end subroutine

       subroutine printVectorSlope(this)
         implicit none
         type(vector),intent(in) :: this
         real(cp) :: slope
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

       subroutine writeVectorY(this,dir,parentUnit)
         implicit none
         type(vector),intent(in) :: this
         real(cp) :: y
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
         real(cp) :: z
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
         real(cp) :: mag
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
         real(cp) :: slope
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