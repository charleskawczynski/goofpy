       module distribution_mod
       use allClassFuncs_mod
       use constants_mod
       use simParams_mod


       type distribution
         private
         real(kind=dp_num) :: min
         real(kind=dp_num) :: max
       endtype

       contains

       function setDistribution(min,max) result(this)
         implicit none
         type(distribution) :: this
         real(kind=dp_num), intent(in) :: min
         real(kind=dp_num), intent(in) :: max
         call SetDistributionMin(this,min)
         call SetDistributionMax(this,max)
       end function

       subroutine setDistributionMin(this,min)
         implicit none
         type(distribution),intent(inout) :: this
         real(kind=dp_num), intent(in) :: min
         this%min = min
       end subroutine

       subroutine setDistributionMax(this,max)
         implicit none
         type(distribution),intent(inout) :: this
         real(kind=dp_num), intent(in) :: max
         this%max = max
       end subroutine

       function getDistributionMin(this) result(min)
         implicit none
         type(distribution),intent(in) :: this
         real(kind=dp_num) :: min
         min = this%min
       end function

       function getDistributionMax(this) result(max)
         implicit none
         type(distribution),intent(in) :: this
         real(kind=dp_num) :: max
         max = this%max
       end function

       subroutine getDistribution(this,min,max)
         implicit none
         type(distribution),intent(in) :: this
         real(kind=dp_num), intent(out) :: min
         real(kind=dp_num), intent(out) :: max
         min = getDistributionMin(this)
         max = getDistributionMax(this)
       end subroutine

       subroutine printDistributionMin(this)
         implicit none
         type(distribution),intent(in) :: this
         real(kind=dp_num) :: min
         write(*,*) "min: ", this%min
       end subroutine

       subroutine printDistributionMax(this)
         implicit none
         type(distribution),intent(in) :: this
         real(kind=dp_num) :: max
         write(*,*) "max: ", this%max
       end subroutine

       subroutine printDistribution(this)
         implicit none
         type(distribution), intent(in) :: this
         write(*,*) 'Printed data for distribution'
         write(*,*) '***************'
         call printDistributionMin(this)
         call printDistributionMax(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeDistributionMin(this,dir,parentUnit)
         implicit none
         type(distribution),intent(in) :: this
         real(kind=dp_num) :: min
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "min: ", this%min
         else
           call newAndOpen(dir,"min")
           write(NewU,*) "min: ", this%min
           call closeAndMessage(NewU,"min")
         endif
       end subroutine

       subroutine writeDistributionMax(this,dir,parentUnit)
         implicit none
         type(distribution),intent(in) :: this
         real(kind=dp_num) :: max
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "max: ", this%max
         else
           call newAndOpen(dir,"max")
           write(NewU,*) "max: ", this%max
           call closeAndMessage(NewU,"max")
         endif
       end subroutine

       subroutine writeDistribution(this,dir,parentUnit)
         implicit none
         type(distribution), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Distribution.txt")
         endif
         call writeDistributionMin(this,dir,NewU)
         call writeDistributionMax(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for distribution written to file +++'
         endif
       end subroutine

         include '../helper/distribution_helper.f'

       end module distribution_mod