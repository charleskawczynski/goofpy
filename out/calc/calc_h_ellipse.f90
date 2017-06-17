       subroutine CalcGeomparamsH_ellipse(this)
         implicit none
         type(geomParams),intent(inout) :: this
         real(kind=dp_num) :: Length1
         Length1 = getGeomparamsLength1(this)
         this%h_ellipse = 
       end subroutine
