       subroutine CalcVectorMag(this)
         implicit none
         type(vector),intent(inout) :: this
         real(kind=dp_num) :: x
         real(kind=dp_num) :: y
         real(kind=dp_num) :: z
         x = getVectorX(this)
         y = getVectorY(this)
         z = getVectorZ(this)
         this%mag = 
       end subroutine
