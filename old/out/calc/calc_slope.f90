       subroutine CalcVectorSlope(this)
         implicit none
         type(vector),intent(inout) :: this
         real(kind=dp_num) :: x
         real(kind=dp_num) :: y
         x = getVectorX(this)
         y = getVectorY(this)
         this%slope = 
       end subroutine
