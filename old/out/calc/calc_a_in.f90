       subroutine CalcGeomparamsA_in(this)
         implicit none
         type(geomParams),intent(inout) :: this
         real(kind=dp_num) :: a_out
         real(kind=dp_num) :: Diameter_out
         a_out = getGeomparamsA_out(this)
         Diameter_out = getGeomparamsDiameter_out(this)
         this%a_in = 
       end subroutine
