       module energy_mod
       use allClassFuncs_mod
       use constants_mod
       use simParams_mod
       use debugger_mod


       type energy
         private
         real(kind=dp_num) :: irradiated
         real(kind=dp_num) :: instant
         real(kind=dp_num) :: transmitted
         real(kind=dp_num) :: absorbed
         real(kind=dp_num) :: escaped
       endtype

       contains

       function setEnergy(irradiated,instant,transmitted,absorbed,
     &   escaped) result(this)
         implicit none
         type(energy) :: this
         real(kind=dp_num), intent(in) :: irradiated
         real(kind=dp_num), intent(in) :: instant
         real(kind=dp_num), intent(in) :: transmitted
         real(kind=dp_num), intent(in) :: absorbed
         real(kind=dp_num), intent(in) :: escaped
         call SetEnergyIrradiated(this,irradiated)
         call SetEnergyInstant(this,instant)
         call SetEnergyTransmitted(this,transmitted)
         call SetEnergyAbsorbed(this,absorbed)
         call SetEnergyEscaped(this,escaped)
       end function

       subroutine setEnergyIrradiated(this,irradiated)
         implicit none
         type(energy),intent(inout) :: this
         real(kind=dp_num), intent(in) :: irradiated
         this%irradiated = irradiated
       end subroutine

       subroutine setEnergyInstant(this,instant)
         implicit none
         type(energy),intent(inout) :: this
         real(kind=dp_num), intent(in) :: instant
         this%instant = instant
       end subroutine

       subroutine setEnergyTransmitted(this,transmitted)
         implicit none
         type(energy),intent(inout) :: this
         real(kind=dp_num), intent(in) :: transmitted
         this%transmitted = transmitted
       end subroutine

       subroutine setEnergyAbsorbed(this,absorbed)
         implicit none
         type(energy),intent(inout) :: this
         real(kind=dp_num), intent(in) :: absorbed
         this%absorbed = absorbed
       end subroutine

       subroutine setEnergyEscaped(this,escaped)
         implicit none
         type(energy),intent(inout) :: this
         real(kind=dp_num), intent(in) :: escaped
         this%escaped = escaped
       end subroutine

       function getEnergyIrradiated(this) result(irradiated)
         implicit none
         type(energy),intent(in) :: this
         real(kind=dp_num) :: irradiated
         irradiated = this%irradiated
       end function

       function getEnergyInstant(this) result(instant)
         implicit none
         type(energy),intent(in) :: this
         real(kind=dp_num) :: instant
         instant = this%instant
       end function

       function getEnergyTransmitted(this) result(transmitted)
         implicit none
         type(energy),intent(in) :: this
         real(kind=dp_num) :: transmitted
         transmitted = this%transmitted
       end function

       function getEnergyAbsorbed(this) result(absorbed)
         implicit none
         type(energy),intent(in) :: this
         real(kind=dp_num) :: absorbed
         absorbed = this%absorbed
       end function

       function getEnergyEscaped(this) result(escaped)
         implicit none
         type(energy),intent(in) :: this
         real(kind=dp_num) :: escaped
         escaped = this%escaped
       end function

       subroutine getEnergy(this,irradiated,instant,transmitted,
     &   absorbed,escaped)
         implicit none
         type(energy),intent(in) :: this
         real(kind=dp_num), intent(out) :: irradiated
         real(kind=dp_num), intent(out) :: instant
         real(kind=dp_num), intent(out) :: transmitted
         real(kind=dp_num), intent(out) :: absorbed
         real(kind=dp_num), intent(out) :: escaped
         irradiated = getEnergyIrradiated(this)
         instant = getEnergyInstant(this)
         transmitted = getEnergyTransmitted(this)
         absorbed = getEnergyAbsorbed(this)
         escaped = getEnergyEscaped(this)
       end subroutine

       subroutine printEnergyIrradiated(this)
         implicit none
         type(energy),intent(in) :: this
         real(kind=dp_num) :: irradiated
         write(*,*) "irradiated: ", this%irradiated
       end subroutine

       subroutine printEnergyInstant(this)
         implicit none
         type(energy),intent(in) :: this
         real(kind=dp_num) :: instant
         write(*,*) "instant: ", this%instant
       end subroutine

       subroutine printEnergyTransmitted(this)
         implicit none
         type(energy),intent(in) :: this
         real(kind=dp_num) :: transmitted
         write(*,*) "transmitted: ", this%transmitted
       end subroutine

       subroutine printEnergyAbsorbed(this)
         implicit none
         type(energy),intent(in) :: this
         real(kind=dp_num) :: absorbed
         write(*,*) "absorbed: ", this%absorbed
       end subroutine

       subroutine printEnergyEscaped(this)
         implicit none
         type(energy),intent(in) :: this
         real(kind=dp_num) :: escaped
         write(*,*) "escaped: ", this%escaped
       end subroutine

       subroutine printEnergy(this)
         implicit none
         type(energy), intent(in) :: this
         write(*,*) 'Printed data for energy'
         write(*,*) '***************'
         call printEnergyIrradiated(this)
         call printEnergyInstant(this)
         call printEnergyTransmitted(this)
         call printEnergyAbsorbed(this)
         call printEnergyEscaped(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeEnergyIrradiated(this,dir,parentUnit)
         implicit none
         type(energy),intent(in) :: this
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

       subroutine writeEnergyInstant(this,dir,parentUnit)
         implicit none
         type(energy),intent(in) :: this
         real(kind=dp_num) :: instant
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "instant: ", this%instant
         else
           call newAndOpen(dir,"instant")
           write(NewU,*) "instant: ", this%instant
           call closeAndMessage(NewU,"instant")
         endif
       end subroutine

       subroutine writeEnergyTransmitted(this,dir,parentUnit)
         implicit none
         type(energy),intent(in) :: this
         real(kind=dp_num) :: transmitted
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "transmitted: ", this%transmitted
         else
           call newAndOpen(dir,"transmitted")
           write(NewU,*) "transmitted: ", this%transmitted
           call closeAndMessage(NewU,"transmitted")
         endif
       end subroutine

       subroutine writeEnergyAbsorbed(this,dir,parentUnit)
         implicit none
         type(energy),intent(in) :: this
         real(kind=dp_num) :: absorbed
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "absorbed: ", this%absorbed
         else
           call newAndOpen(dir,"absorbed")
           write(NewU,*) "absorbed: ", this%absorbed
           call closeAndMessage(NewU,"absorbed")
         endif
       end subroutine

       subroutine writeEnergyEscaped(this,dir,parentUnit)
         implicit none
         type(energy),intent(in) :: this
         real(kind=dp_num) :: escaped
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "escaped: ", this%escaped
         else
           call newAndOpen(dir,"escaped")
           write(NewU,*) "escaped: ", this%escaped
           call closeAndMessage(NewU,"escaped")
         endif
       end subroutine

       subroutine writeEnergy(this,dir,parentUnit)
         implicit none
         type(energy), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Energy.txt")
         endif
         call writeEnergyIrradiated(this,dir,NewU)
         call writeEnergyInstant(this,dir,NewU)
         call writeEnergyTransmitted(this,dir,NewU)
         call writeEnergyAbsorbed(this,dir,NewU)
         call writeEnergyEscaped(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for energy written to file +++'
         endif
       end subroutine

         include '../helper/energy_helper.f'

       end module energy_mod