       module simResults_mod
       use allClassFuncs_mod
       use constants_mod
       use simParams_mod
       use exportData_mod
       use photon_mod
       use photonResults_mod
       use combinedNormTau_mod


       type simResults
         private
         real(kind=dp_num) :: irradiated
         real(kind=dp_num) :: tau
         real(kind=dp_num) :: normTau
         real(kind=dp_num) :: theta
         real(kind=dp_num) :: phi
         real(kind=dp_num) :: time
       endtype

       contains

       function setSimresults(irradiated,tau,normTau,theta,phi,time)
     &    result(this)
         implicit none
         type(simResults) :: this
         real(kind=dp_num), intent(in) :: irradiated
         real(kind=dp_num), intent(in) :: tau
         real(kind=dp_num), intent(in) :: normTau
         real(kind=dp_num), intent(in) :: theta
         real(kind=dp_num), intent(in) :: phi
         real(kind=dp_num), intent(in) :: time
         call SetSimresultsIrradiated(this,irradiated)
         call SetSimresultsTau(this,tau)
         call SetSimresultsNormtau(this,normTau)
         call SetSimresultsTheta(this,theta)
         call SetSimresultsPhi(this,phi)
         call SetSimresultsTime(this,time)
       end function

       subroutine setSimresultsIrradiated(this,irradiated)
         implicit none
         type(simResults),intent(inout) :: this
         real(kind=dp_num), intent(in) :: irradiated
         this%irradiated = irradiated
       end subroutine

       subroutine setSimresultsTau(this,tau)
         implicit none
         type(simResults),intent(inout) :: this
         real(kind=dp_num), intent(in) :: tau
         this%tau = tau
       end subroutine

       subroutine setSimresultsNormtau(this,normTau)
         implicit none
         type(simResults),intent(inout) :: this
         real(kind=dp_num), intent(in) :: normTau
         this%normTau = normTau
       end subroutine

       subroutine setSimresultsTheta(this,theta)
         implicit none
         type(simResults),intent(inout) :: this
         real(kind=dp_num), intent(in) :: theta
         this%theta = theta
       end subroutine

       subroutine setSimresultsPhi(this,phi)
         implicit none
         type(simResults),intent(inout) :: this
         real(kind=dp_num), intent(in) :: phi
         this%phi = phi
       end subroutine

       subroutine setSimresultsTime(this,time)
         implicit none
         type(simResults),intent(inout) :: this
         real(kind=dp_num), intent(in) :: time
         this%time = time
       end subroutine

       function getSimresultsIrradiated(this) result(irradiated)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: irradiated
         irradiated = this%irradiated
       end function

       function getSimresultsTau(this) result(tau)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: tau
         tau = this%tau
       end function

       function getSimresultsNormtau(this) result(normTau)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: normTau
         normTau = this%normTau
       end function

       function getSimresultsTheta(this) result(theta)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: theta
         theta = this%theta
       end function

       function getSimresultsPhi(this) result(phi)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: phi
         phi = this%phi
       end function

       function getSimresultsTime(this) result(time)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: time
         time = this%time
       end function

       subroutine getSimresults(this,irradiated,tau,normTau,theta,phi,
     &   time)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num), intent(out) :: irradiated
         real(kind=dp_num), intent(out) :: tau
         real(kind=dp_num), intent(out) :: normTau
         real(kind=dp_num), intent(out) :: theta
         real(kind=dp_num), intent(out) :: phi
         real(kind=dp_num), intent(out) :: time
         irradiated = getSimresultsIrradiated(this)
         tau = getSimresultsTau(this)
         normTau = getSimresultsNormtau(this)
         theta = getSimresultsTheta(this)
         phi = getSimresultsPhi(this)
         time = getSimresultsTime(this)
       end subroutine

       subroutine printSimresultsIrradiated(this)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: irradiated
         write(*,*) "irradiated: ", this%irradiated
       end subroutine

       subroutine printSimresultsTau(this)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: tau
         write(*,*) "tau: ", this%tau
       end subroutine

       subroutine printSimresultsNormtau(this)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: normTau
         write(*,*) "normTau: ", this%normTau
       end subroutine

       subroutine printSimresultsTheta(this)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: theta
         write(*,*) "theta: ", this%theta
       end subroutine

       subroutine printSimresultsPhi(this)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: phi
         write(*,*) "phi: ", this%phi
       end subroutine

       subroutine printSimresultsTime(this)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: time
         write(*,*) "time: ", this%time
       end subroutine

       subroutine printSimresults(this)
         implicit none
         type(simResults), intent(in) :: this
         write(*,*) 'Printed data for simResults'
         write(*,*) '***************'
         call printSimresultsIrradiated(this)
         call printSimresultsTau(this)
         call printSimresultsNormtau(this)
         call printSimresultsTheta(this)
         call printSimresultsPhi(this)
         call printSimresultsTime(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeSimresultsIrradiated(this,dir,parentUnit)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: irradiated
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "irradiated: ", this%irradiated
         else
           call newAndOpen(dir,"irradiated")
           write(NewU,*) "irradiated: ", this%irradiated
           call closeAndMessage(NewU,"irradiated")
         endif
       end subroutine

       subroutine writeSimresultsTau(this,dir,parentUnit)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: tau
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "tau: ", this%tau
         else
           call newAndOpen(dir,"tau")
           write(NewU,*) "tau: ", this%tau
           call closeAndMessage(NewU,"tau")
         endif
       end subroutine

       subroutine writeSimresultsNormtau(this,dir,parentUnit)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: normTau
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "normTau: ", this%normTau
         else
           call newAndOpen(dir,"normTau")
           write(NewU,*) "normTau: ", this%normTau
           call closeAndMessage(NewU,"normTau")
         endif
       end subroutine

       subroutine writeSimresultsTheta(this,dir,parentUnit)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: theta
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "theta: ", this%theta
         else
           call newAndOpen(dir,"theta")
           write(NewU,*) "theta: ", this%theta
           call closeAndMessage(NewU,"theta")
         endif
       end subroutine

       subroutine writeSimresultsPhi(this,dir,parentUnit)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: phi
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "phi: ", this%phi
         else
           call newAndOpen(dir,"phi")
           write(NewU,*) "phi: ", this%phi
           call closeAndMessage(NewU,"phi")
         endif
       end subroutine

       subroutine writeSimresultsTime(this,dir,parentUnit)
         implicit none
         type(simResults),intent(in) :: this
         real(kind=dp_num) :: time
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "time: ", this%time
         else
           call newAndOpen(dir,"time")
           write(NewU,*) "time: ", this%time
           call closeAndMessage(NewU,"time")
         endif
       end subroutine

       subroutine writeSimresults(this,dir,parentUnit)
         implicit none
         type(simResults), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Simresults.txt")
         endif
         call writeSimresultsIrradiated(this,dir,NewU)
         call writeSimresultsTau(this,dir,NewU)
         call writeSimresultsNormtau(this,dir,NewU)
         call writeSimresultsTheta(this,dir,NewU)
         call writeSimresultsPhi(this,dir,NewU)
         call writeSimresultsTime(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for simResults written to file +++'
         endif
       end subroutine

         include '../helper/simResults_helper.f'

       end module simResults_mod