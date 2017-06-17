       module photon_mod
       use allClassFuncs_mod
       use constants_mod
       use simParams_mod
       use debugger_mod
       use distribution_mod
       use exportData_mod
       use vector_mod
       use line_mod
       use surface_mod
       use position_mod
       use direction_mod
       use energy_mod


       type photon
         private
         type(position) :: p
         type(direction) :: d
         type(energy) :: e
         integer :: id
         integer :: S_id
         logical :: endPursuit
       endtype

       contains

       function setPhoton(p,d,e,id,S_id,endPursuit) result(this)
         implicit none
         type(photon) :: this
         type(position), intent(in) :: p
         type(direction), intent(in) :: d
         type(energy), intent(in) :: e
         integer, intent(in) :: id
         integer, intent(in) :: S_id
         logical, intent(in) :: endPursuit
         call SetPhotonP(this,p)
         call SetPhotonD(this,d)
         call SetPhotonE(this,e)
         call SetPhotonId(this,id)
         call SetPhotonS_id(this,S_id)
         call SetPhotonEndpursuit(this,endPursuit)
       end function

       subroutine setPhotonP(this,p)
         implicit none
         type(photon),intent(inout) :: this
         type(position), intent(in) :: p
         this%p = p
       end subroutine

       subroutine setPhotonD(this,d)
         implicit none
         type(photon),intent(inout) :: this
         type(direction), intent(in) :: d
         this%d = d
       end subroutine

       subroutine setPhotonE(this,e)
         implicit none
         type(photon),intent(inout) :: this
         type(energy), intent(in) :: e
         this%e = e
       end subroutine

       subroutine setPhotonId(this,id)
         implicit none
         type(photon),intent(inout) :: this
         integer, intent(in) :: id
         this%id = id
       end subroutine

       subroutine setPhotonS_id(this,S_id)
         implicit none
         type(photon),intent(inout) :: this
         integer, intent(in) :: S_id
         this%S_id = S_id
       end subroutine

       subroutine setPhotonEndpursuit(this,endPursuit)
         implicit none
         type(photon),intent(inout) :: this
         logical, intent(in) :: endPursuit
         this%endPursuit = endPursuit
       end subroutine

       function getPhotonP(this) result(p)
         implicit none
         type(photon),intent(in) :: this
         type(position) :: p
         p = this%p
       end function

       function getPhotonD(this) result(d)
         implicit none
         type(photon),intent(in) :: this
         type(direction) :: d
         d = this%d
       end function

       function getPhotonE(this) result(e)
         implicit none
         type(photon),intent(in) :: this
         type(energy) :: e
         e = this%e
       end function

       function getPhotonId(this) result(id)
         implicit none
         type(photon),intent(in) :: this
         integer :: id
         id = this%id
       end function

       function getPhotonS_id(this) result(S_id)
         implicit none
         type(photon),intent(in) :: this
         integer :: S_id
         S_id = this%S_id
       end function

       function getPhotonEndpursuit(this) result(endPursuit)
         implicit none
         type(photon),intent(in) :: this
         logical :: endPursuit
         endPursuit = this%endPursuit
       end function

       subroutine getPhoton(this,p,d,e,id,S_id,endPursuit)
         implicit none
         type(photon),intent(in) :: this
         type(position), intent(out) :: p
         type(direction), intent(out) :: d
         type(energy), intent(out) :: e
         integer, intent(out) :: id
         integer, intent(out) :: S_id
         logical, intent(out) :: endPursuit
         p = getPhotonP(this)
         d = getPhotonD(this)
         e = getPhotonE(this)
         id = getPhotonId(this)
         S_id = getPhotonS_id(this)
         endPursuit = getPhotonEndpursuit(this)
       end subroutine

       subroutine printPhotonP(this)
         implicit none
         type(photon),intent(in) :: this
         type(position) :: p
         call printPosition(this%p)
       end subroutine

       subroutine printPhotonD(this)
         implicit none
         type(photon),intent(in) :: this
         type(direction) :: d
         call printDirection(this%d)
       end subroutine

       subroutine printPhotonE(this)
         implicit none
         type(photon),intent(in) :: this
         type(energy) :: e
         call printEnergy(this%e)
       end subroutine

       subroutine printPhotonId(this)
         implicit none
         type(photon),intent(in) :: this
         integer :: id
         write(*,*) "id: ", this%id
       end subroutine

       subroutine printPhotonS_id(this)
         implicit none
         type(photon),intent(in) :: this
         integer :: S_id
         write(*,*) "S_id: ", this%S_id
       end subroutine

       subroutine printPhotonEndpursuit(this)
         implicit none
         type(photon),intent(in) :: this
         logical :: endPursuit
         write(*,*) "endPursuit: ", this%endPursuit
       end subroutine

       subroutine printPhoton(this)
         implicit none
         type(photon), intent(in) :: this
         write(*,*) 'Printed data for photon'
         write(*,*) '***************'
         call printPosition(this%p)
         call printDirection(this%d)
         call printEnergy(this%e)
         call printPhotonId(this)
         call printPhotonS_id(this)
         call printPhotonEndpursuit(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writePhotonP(this,dir)
         implicit none
         type(photon),intent(in) :: this
         character(len=*),intent(in) :: dir
         type(position) :: p
         call writePosition(this%p,dir)
       end subroutine

       subroutine writePhotonD(this,dir)
         implicit none
         type(photon),intent(in) :: this
         character(len=*),intent(in) :: dir
         type(direction) :: d
         call writeDirection(this%d,dir)
       end subroutine

       subroutine writePhotonE(this,dir)
         implicit none
         type(photon),intent(in) :: this
         character(len=*),intent(in) :: dir
         type(energy) :: e
         call writeEnergy(this%e,dir)
       end subroutine

       subroutine writePhotonId(this,dir,parentUnit)
         implicit none
         type(photon),intent(in) :: this
         integer :: id
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "id: ", this%id
         else
           call newAndOpen(dir,"id")
           write(NewU,*) "id: ", this%id
           call closeAndMessage(NewU,"id")
         endif
       end subroutine

       subroutine writePhotonS_id(this,dir,parentUnit)
         implicit none
         type(photon),intent(in) :: this
         integer :: S_id
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "S_id: ", this%S_id
         else
           call newAndOpen(dir,"S_id")
           write(NewU,*) "S_id: ", this%S_id
           call closeAndMessage(NewU,"S_id")
         endif
       end subroutine

       subroutine writePhotonEndpursuit(this,dir,parentUnit)
         implicit none
         type(photon),intent(in) :: this
         logical :: endPursuit
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "endPursuit: ", this%endPursuit
         else
           call newAndOpen(dir,"endPursuit")
           write(NewU,*) "endPursuit: ", this%endPursuit
           call closeAndMessage(NewU,"endPursuit")
         endif
       end subroutine

       subroutine writePhoton(this,dir,parentUnit)
         implicit none
         type(photon), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Photon.txt")
         endif
         call writePosition(this%p,dir,NewU)
         call writeDirection(this%d,dir,NewU)
         call writeEnergy(this%e,dir,NewU)
         call writePhotonId(this,dir,NewU)
         call writePhotonS_id(this,dir,NewU)
         call writePhotonEndpursuit(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for photon written to file +++'
         endif
       end subroutine

         include '../helper/photon_helper.f'

       end module photon_mod