       module photonResults_mod
       use allClassFuncs_mod
       use constants_mod
       use simParams_mod
       use photon_mod


       type photonResults
         private
         type(photon) :: p
       endtype

       contains

       function setPhotonresults(p) result(this)
         implicit none
         type(photonResults) :: this
         type(photon), intent(in) :: p
         call SetPhotonresultsP(this,p)
       end function

       subroutine setPhotonresultsP(this,p)
         implicit none
         type(photonResults),intent(inout) :: this
         type(photon), intent(in) :: p
         this%p = p
       end subroutine

       function getPhotonresultsP(this) result(p)
         implicit none
         type(photonResults),intent(in) :: this
         type(photon) :: p
         p = this%p
       end function

       subroutine getPhotonresults(this,p)
         implicit none
         type(photonResults),intent(in) :: this
         type(photon), intent(out) :: p
         p = getPhotonresultsP(this)
       end subroutine

       subroutine printPhotonresultsP(this)
         implicit none
         type(photonResults),intent(in) :: this
         type(photon) :: p
         call printPhoton(this%p)
       end subroutine

       subroutine printPhotonresults(this)
         implicit none
         type(photonResults), intent(in) :: this
         write(*,*) 'Printed data for photonResults'
         write(*,*) '***************'
         call printPhoton(this%p)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writePhotonresultsP(this,dir)
         implicit none
         type(photonResults),intent(in) :: this
         character(len=*),intent(in) :: dir
         type(photon) :: p
         call writePhoton(this%p,dir)
       end subroutine

       subroutine writePhotonresults(this,dir,parentUnit)
         implicit none
         type(photonResults), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Photonresults.txt")
         endif
         call writePhoton(this%p,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for photonResults written to file +++'
         endif
       end subroutine

         include '../helper/photonResults_helper.f'

       end module photonResults_mod