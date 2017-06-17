       subroutine CalcGeomparamsK_ellipse(this)
         implicit none
         type(geomParams),intent(inout) :: this
         real(kind=dp_num) :: b_out
         b_out = getGeomparamsB_out(this)
         this%k_ellipse = 
       end subroutine
