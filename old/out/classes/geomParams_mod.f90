       module geomParams_mod
       use allClassFuncs_mod
       use counters_mod
       use constants_mod
       use simParams_mod
       use debugger_mod


       type geomParams
         private
         real(cp) :: Length1
         real(cp) :: Length2
         real(cp) :: Height
         real(cp) :: Width
         real(cp) :: Diameter_in
         real(cp) :: Diameter_out
         real(cp) :: a_out
         real(cp) :: b_out
         real(cp) :: a_in
         real(cp) :: b_in
         real(cp) :: h_ellipse
         real(cp) :: k_ellipse
       endtype

       contains

       function setGeomparams(Length1,Length2,Height,Width,Diameter_in,
     &   Diameter_out,a_out,b_out) result(this)
         implicit none
         type(geomParams) :: this
         real(cp), intent(in) :: Length1
         real(cp), intent(in) :: Length2
         real(cp), intent(in) :: Height
         real(cp), intent(in) :: Width
         real(cp), intent(in) :: Diameter_in
         real(cp), intent(in) :: Diameter_out
         real(cp), intent(in) :: a_out
         real(cp), intent(in) :: b_out
         call SetGeomparamsLength1(this,Length1)
         call SetGeomparamsLength2(this,Length2)
         call SetGeomparamsHeight(this,Height)
         call SetGeomparamsWidth(this,Width)
         call SetGeomparamsDiameter_in(this,Diameter_in)
         call SetGeomparamsDiameter_out(this,Diameter_out)
         call SetGeomparamsA_out(this,a_out)
         call SetGeomparamsB_out(this,b_out)
       end function

       subroutine setGeomparamsLength1(this,Length1)
         implicit none
         type(geomParams),intent(inout) :: this
         real(cp), intent(in) :: Length1
         this%Length1 = Length1
       end subroutine

       subroutine setGeomparamsLength2(this,Length2)
         implicit none
         type(geomParams),intent(inout) :: this
         real(cp), intent(in) :: Length2
         this%Length2 = Length2
       end subroutine

       subroutine setGeomparamsHeight(this,Height)
         implicit none
         type(geomParams),intent(inout) :: this
         real(cp), intent(in) :: Height
         this%Height = Height
       end subroutine

       subroutine setGeomparamsWidth(this,Width)
         implicit none
         type(geomParams),intent(inout) :: this
         real(cp), intent(in) :: Width
         this%Width = Width
       end subroutine

       subroutine setGeomparamsDiameter_in(this,Diameter_in)
         implicit none
         type(geomParams),intent(inout) :: this
         real(cp), intent(in) :: Diameter_in
         this%Diameter_in = Diameter_in
       end subroutine

       subroutine setGeomparamsDiameter_out(this,Diameter_out)
         implicit none
         type(geomParams),intent(inout) :: this
         real(cp), intent(in) :: Diameter_out
         this%Diameter_out = Diameter_out
       end subroutine

       subroutine setGeomparamsA_out(this,a_out)
         implicit none
         type(geomParams),intent(inout) :: this
         real(cp), intent(in) :: a_out
         this%a_out = a_out
       end subroutine

       subroutine setGeomparamsB_out(this,b_out)
         implicit none
         type(geomParams),intent(inout) :: this
         real(cp), intent(in) :: b_out
         this%b_out = b_out
       end subroutine

       subroutine setGeomparamsA_in(this,a_in)
         implicit none
         type(geomParams),intent(inout) :: this
         real(cp), intent(in) :: a_in
         this%a_in = a_in
       end subroutine

       subroutine setGeomparamsB_in(this,b_in)
         implicit none
         type(geomParams),intent(inout) :: this
         real(cp), intent(in) :: b_in
         this%b_in = b_in
       end subroutine

       subroutine setGeomparamsH_ellipse(this,h_ellipse)
         implicit none
         type(geomParams),intent(inout) :: this
         real(cp), intent(in) :: h_ellipse
         this%h_ellipse = h_ellipse
       end subroutine

       subroutine setGeomparamsK_ellipse(this,k_ellipse)
         implicit none
         type(geomParams),intent(inout) :: this
         real(cp), intent(in) :: k_ellipse
         this%k_ellipse = k_ellipse
       end subroutine

       function getGeomparamsLength1(this) result(Length1)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Length1
         Length1 = this%Length1
       end function

       function getGeomparamsLength2(this) result(Length2)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Length2
         Length2 = this%Length2
       end function

       function getGeomparamsHeight(this) result(Height)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Height
         Height = this%Height
       end function

       function getGeomparamsWidth(this) result(Width)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Width
         Width = this%Width
       end function

       function getGeomparamsDiameter_in(this) result(Diameter_in)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Diameter_in
         Diameter_in = this%Diameter_in
       end function

       function getGeomparamsDiameter_out(this) result(Diameter_out)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Diameter_out
         Diameter_out = this%Diameter_out
       end function

       function getGeomparamsA_out(this) result(a_out)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: a_out
         a_out = this%a_out
       end function

       function getGeomparamsB_out(this) result(b_out)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: b_out
         b_out = this%b_out
       end function

       function getGeomparamsA_in(this) result(a_in)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: a_in
         a_in = this%a_in
       end function

       function getGeomparamsB_in(this) result(b_in)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: b_in
         b_in = this%b_in
       end function

       function getGeomparamsH_ellipse(this) result(h_ellipse)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: h_ellipse
         h_ellipse = this%h_ellipse
       end function

       function getGeomparamsK_ellipse(this) result(k_ellipse)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: k_ellipse
         k_ellipse = this%k_ellipse
       end function

       subroutine getGeomparams(this,Length1,Length2,Height,Width,
     &   Diameter_in,Diameter_out,a_out,b_out,a_in,b_in,h_ellipse,
     &   k_ellipse)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp), intent(out) :: Length1
         real(cp), intent(out) :: Length2
         real(cp), intent(out) :: Height
         real(cp), intent(out) :: Width
         real(cp), intent(out) :: Diameter_in
         real(cp), intent(out) :: Diameter_out
         real(cp), intent(out) :: a_out
         real(cp), intent(out) :: b_out
         real(cp), intent(out) :: a_in
         real(cp), intent(out) :: b_in
         real(cp), intent(out) :: h_ellipse
         real(cp), intent(out) :: k_ellipse
         Length1 = getGeomparamsLength1(this)
         Length2 = getGeomparamsLength2(this)
         Height = getGeomparamsHeight(this)
         Width = getGeomparamsWidth(this)
         Diameter_in = getGeomparamsDiameter_in(this)
         Diameter_out = getGeomparamsDiameter_out(this)
         a_out = getGeomparamsA_out(this)
         b_out = getGeomparamsB_out(this)
         a_in = getGeomparamsA_in(this)
         b_in = getGeomparamsB_in(this)
         h_ellipse = getGeomparamsH_ellipse(this)
         k_ellipse = getGeomparamsK_ellipse(this)
       end subroutine

       subroutine printGeomparamsLength1(this)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Length1
         write(*,*) "Length1: ", this%Length1
       end subroutine

       subroutine printGeomparamsLength2(this)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Length2
         write(*,*) "Length2: ", this%Length2
       end subroutine

       subroutine printGeomparamsHeight(this)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Height
         write(*,*) "Height: ", this%Height
       end subroutine

       subroutine printGeomparamsWidth(this)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Width
         write(*,*) "Width: ", this%Width
       end subroutine

       subroutine printGeomparamsDiameter_in(this)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Diameter_in
         write(*,*) "Diameter_in: ", this%Diameter_in
       end subroutine

       subroutine printGeomparamsDiameter_out(this)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Diameter_out
         write(*,*) "Diameter_out: ", this%Diameter_out
       end subroutine

       subroutine printGeomparamsA_out(this)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: a_out
         write(*,*) "a_out: ", this%a_out
       end subroutine

       subroutine printGeomparamsB_out(this)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: b_out
         write(*,*) "b_out: ", this%b_out
       end subroutine

       subroutine printGeomparamsA_in(this)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: a_in
         write(*,*) "a_in: ", this%a_in
       end subroutine

       subroutine printGeomparamsB_in(this)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: b_in
         write(*,*) "b_in: ", this%b_in
       end subroutine

       subroutine printGeomparamsH_ellipse(this)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: h_ellipse
         write(*,*) "h_ellipse: ", this%h_ellipse
       end subroutine

       subroutine printGeomparamsK_ellipse(this)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: k_ellipse
         write(*,*) "k_ellipse: ", this%k_ellipse
       end subroutine

       subroutine printGeomparams(this)
         implicit none
         type(geomParams), intent(in) :: this
         write(*,*) 'Printed data for geomParams'
         write(*,*) '***************'
         call printGeomparamsLength1(this)
         call printGeomparamsLength2(this)
         call printGeomparamsHeight(this)
         call printGeomparamsWidth(this)
         call printGeomparamsDiameter_in(this)
         call printGeomparamsDiameter_out(this)
         call printGeomparamsA_out(this)
         call printGeomparamsB_out(this)
         call printGeomparamsA_in(this)
         call printGeomparamsB_in(this)
         call printGeomparamsH_ellipse(this)
         call printGeomparamsK_ellipse(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeGeomparamsLength1(this,dir,parentUnit)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Length1
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "Length1: ", this%Length1
         else
           call newAndOpen(dir,"Length1")
           write(NewU,*) "Length1: ", this%Length1
           call closeAndMessage(NewU,"Length1")
         endif
       end subroutine

       subroutine writeGeomparamsLength2(this,dir,parentUnit)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Length2
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "Length2: ", this%Length2
         else
           call newAndOpen(dir,"Length2")
           write(NewU,*) "Length2: ", this%Length2
           call closeAndMessage(NewU,"Length2")
         endif
       end subroutine

       subroutine writeGeomparamsHeight(this,dir,parentUnit)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Height
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "Height: ", this%Height
         else
           call newAndOpen(dir,"Height")
           write(NewU,*) "Height: ", this%Height
           call closeAndMessage(NewU,"Height")
         endif
       end subroutine

       subroutine writeGeomparamsWidth(this,dir,parentUnit)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Width
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "Width: ", this%Width
         else
           call newAndOpen(dir,"Width")
           write(NewU,*) "Width: ", this%Width
           call closeAndMessage(NewU,"Width")
         endif
       end subroutine

       subroutine writeGeomparamsDiameter_in(this,dir,parentUnit)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Diameter_in
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "Diameter_in: ", this%Diameter_in
         else
           call newAndOpen(dir,"Diameter_in")
           write(NewU,*) "Diameter_in: ", this%Diameter_in
           call closeAndMessage(NewU,"Diameter_in")
         endif
       end subroutine

       subroutine writeGeomparamsDiameter_out(this,dir,parentUnit)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: Diameter_out
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "Diameter_out: ", this%Diameter_out
         else
           call newAndOpen(dir,"Diameter_out")
           write(NewU,*) "Diameter_out: ", this%Diameter_out
           call closeAndMessage(NewU,"Diameter_out")
         endif
       end subroutine

       subroutine writeGeomparamsA_out(this,dir,parentUnit)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: a_out
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "a_out: ", this%a_out
         else
           call newAndOpen(dir,"a_out")
           write(NewU,*) "a_out: ", this%a_out
           call closeAndMessage(NewU,"a_out")
         endif
       end subroutine

       subroutine writeGeomparamsB_out(this,dir,parentUnit)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: b_out
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "b_out: ", this%b_out
         else
           call newAndOpen(dir,"b_out")
           write(NewU,*) "b_out: ", this%b_out
           call closeAndMessage(NewU,"b_out")
         endif
       end subroutine

       subroutine writeGeomparamsA_in(this,dir,parentUnit)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: a_in
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "a_in: ", this%a_in
         else
           call newAndOpen(dir,"a_in")
           write(NewU,*) "a_in: ", this%a_in
           call closeAndMessage(NewU,"a_in")
         endif
       end subroutine

       subroutine writeGeomparamsB_in(this,dir,parentUnit)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: b_in
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "b_in: ", this%b_in
         else
           call newAndOpen(dir,"b_in")
           write(NewU,*) "b_in: ", this%b_in
           call closeAndMessage(NewU,"b_in")
         endif
       end subroutine

       subroutine writeGeomparamsH_ellipse(this,dir,parentUnit)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: h_ellipse
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "h_ellipse: ", this%h_ellipse
         else
           call newAndOpen(dir,"h_ellipse")
           write(NewU,*) "h_ellipse: ", this%h_ellipse
           call closeAndMessage(NewU,"h_ellipse")
         endif
       end subroutine

       subroutine writeGeomparamsK_ellipse(this,dir,parentUnit)
         implicit none
         type(geomParams),intent(in) :: this
         real(cp) :: k_ellipse
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "k_ellipse: ", this%k_ellipse
         else
           call newAndOpen(dir,"k_ellipse")
           write(NewU,*) "k_ellipse: ", this%k_ellipse
           call closeAndMessage(NewU,"k_ellipse")
         endif
       end subroutine

       subroutine writeGeomparams(this,dir,parentUnit)
         implicit none
         type(geomParams), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Geomparams.txt")
         endif
         call writeGeomparamsLength1(this,dir,NewU)
         call writeGeomparamsLength2(this,dir,NewU)
         call writeGeomparamsHeight(this,dir,NewU)
         call writeGeomparamsWidth(this,dir,NewU)
         call writeGeomparamsDiameter_in(this,dir,NewU)
         call writeGeomparamsDiameter_out(this,dir,NewU)
         call writeGeomparamsA_out(this,dir,NewU)
         call writeGeomparamsB_out(this,dir,NewU)
         call writeGeomparamsA_in(this,dir,NewU)
         call writeGeomparamsB_in(this,dir,NewU)
         call writeGeomparamsH_ellipse(this,dir,NewU)
         call writeGeomparamsK_ellipse(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for geomParams written to file +++'
         endif
       end subroutine

         include '../helper/geomParams_helper.f'
         include '../calc/calc_a_in.f'
         include '../calc/calc_b_in.f'
         include '../calc/calc_h_ellipse.f'
         include '../calc/calc_k_ellipse.f'

       end module geomParams_mod