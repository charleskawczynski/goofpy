       subroutine CalcGeomparamsB_in(this)
         implicit none
         type(geomParams),intent(inout) :: this
         real(kind=dp_num) :: b_out
         real(kind=dp_num) :: Diameter_in
         b_out = getGeomparamsB_out(this)
         Diameter_in = getGeomparamsDiameter_in(this)
         this%b_in = 
       end subroutine
