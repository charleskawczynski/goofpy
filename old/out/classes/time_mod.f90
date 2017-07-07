       module time_mod
       use allClassFuncs_mod
       use constants_mod
       use counters_mod
       use simResults_mod


       type time
         private
         real(cp) :: elapsed
         real(cp) :: cumulative
         real(cp) :: average
         real(cp) :: remaining
         real(cp) :: N_Photons
       endtype

       contains

       function setTime(elapsed,cumulative,average,remaining,N_Photons)
     &    result(this)
         implicit none
         type(time) :: this
         real(cp), intent(in) :: elapsed
         real(cp), intent(in) :: cumulative
         real(cp), intent(in) :: average
         real(cp), intent(in) :: remaining
         real(cp), intent(in) :: N_Photons
         call SetTimeElapsed(this,elapsed)
         call SetTimeCumulative(this,cumulative)
         call SetTimeAverage(this,average)
         call SetTimeRemaining(this,remaining)
         call SetTimeN_photons(this,N_Photons)
       end function

       subroutine setTimeElapsed(this,elapsed)
         implicit none
         type(time),intent(inout) :: this
         real(cp), intent(in) :: elapsed
         this%elapsed = elapsed
       end subroutine

       subroutine setTimeCumulative(this,cumulative)
         implicit none
         type(time),intent(inout) :: this
         real(cp), intent(in) :: cumulative
         this%cumulative = cumulative
       end subroutine

       subroutine setTimeAverage(this,average)
         implicit none
         type(time),intent(inout) :: this
         real(cp), intent(in) :: average
         this%average = average
       end subroutine

       subroutine setTimeRemaining(this,remaining)
         implicit none
         type(time),intent(inout) :: this
         real(cp), intent(in) :: remaining
         this%remaining = remaining
       end subroutine

       subroutine setTimeN_photons(this,N_Photons)
         implicit none
         type(time),intent(inout) :: this
         real(cp), intent(in) :: N_Photons
         this%N_Photons = N_Photons
       end subroutine

       function getTimeElapsed(this) result(elapsed)
         implicit none
         type(time),intent(in) :: this
         real(cp) :: elapsed
         elapsed = this%elapsed
       end function

       function getTimeCumulative(this) result(cumulative)
         implicit none
         type(time),intent(in) :: this
         real(cp) :: cumulative
         cumulative = this%cumulative
       end function

       function getTimeAverage(this) result(average)
         implicit none
         type(time),intent(in) :: this
         real(cp) :: average
         average = this%average
       end function

       function getTimeRemaining(this) result(remaining)
         implicit none
         type(time),intent(in) :: this
         real(cp) :: remaining
         remaining = this%remaining
       end function

       function getTimeN_photons(this) result(N_Photons)
         implicit none
         type(time),intent(in) :: this
         real(cp) :: N_Photons
         N_Photons = this%N_Photons
       end function

       subroutine getTime(this,elapsed,cumulative,average,remaining,
     &   N_Photons)
         implicit none
         type(time),intent(in) :: this
         real(cp), intent(out) :: elapsed
         real(cp), intent(out) :: cumulative
         real(cp), intent(out) :: average
         real(cp), intent(out) :: remaining
         real(cp), intent(out) :: N_Photons
         elapsed = getTimeElapsed(this)
         cumulative = getTimeCumulative(this)
         average = getTimeAverage(this)
         remaining = getTimeRemaining(this)
         N_Photons = getTimeN_photons(this)
       end subroutine

       subroutine printTimeElapsed(this)
         implicit none
         type(time),intent(in) :: this
         real(cp) :: elapsed
         write(*,*) "elapsed: ", this%elapsed
       end subroutine

       subroutine printTimeCumulative(this)
         implicit none
         type(time),intent(in) :: this
         real(cp) :: cumulative
         write(*,*) "cumulative: ", this%cumulative
       end subroutine

       subroutine printTimeAverage(this)
         implicit none
         type(time),intent(in) :: this
         real(cp) :: average
         write(*,*) "average: ", this%average
       end subroutine

       subroutine printTimeRemaining(this)
         implicit none
         type(time),intent(in) :: this
         real(cp) :: remaining
         write(*,*) "remaining: ", this%remaining
       end subroutine

       subroutine printTimeN_photons(this)
         implicit none
         type(time),intent(in) :: this
         real(cp) :: N_Photons
         write(*,*) "N_Photons: ", this%N_Photons
       end subroutine

       subroutine printTime(this)
         implicit none
         type(time), intent(in) :: this
         write(*,*) 'Printed data for time'
         write(*,*) '***************'
         call printTimeElapsed(this)
         call printTimeCumulative(this)
         call printTimeAverage(this)
         call printTimeRemaining(this)
         call printTimeN_photons(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeTimeElapsed(this,dir,parentUnit)
         implicit none
         type(time),intent(in) :: this
         real(cp) :: elapsed
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "elapsed: ", this%elapsed
         else
           call newAndOpen(dir,"elapsed")
           write(NewU,*) "elapsed: ", this%elapsed
           call closeAndMessage(NewU,"elapsed")
         endif
       end subroutine

       subroutine writeTimeCumulative(this,dir,parentUnit)
         implicit none
         type(time),intent(in) :: this
         real(cp) :: cumulative
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "cumulative: ", this%cumulative
         else
           call newAndOpen(dir,"cumulative")
           write(NewU,*) "cumulative: ", this%cumulative
           call closeAndMessage(NewU,"cumulative")
         endif
       end subroutine

       subroutine writeTimeAverage(this,dir,parentUnit)
         implicit none
         type(time),intent(in) :: this
         real(cp) :: average
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "average: ", this%average
         else
           call newAndOpen(dir,"average")
           write(NewU,*) "average: ", this%average
           call closeAndMessage(NewU,"average")
         endif
       end subroutine

       subroutine writeTimeRemaining(this,dir,parentUnit)
         implicit none
         type(time),intent(in) :: this
         real(cp) :: remaining
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "remaining: ", this%remaining
         else
           call newAndOpen(dir,"remaining")
           write(NewU,*) "remaining: ", this%remaining
           call closeAndMessage(NewU,"remaining")
         endif
       end subroutine

       subroutine writeTimeN_photons(this,dir,parentUnit)
         implicit none
         type(time),intent(in) :: this
         real(cp) :: N_Photons
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "N_Photons: ", this%N_Photons
         else
           call newAndOpen(dir,"N_Photons")
           write(NewU,*) "N_Photons: ", this%N_Photons
           call closeAndMessage(NewU,"N_Photons")
         endif
       end subroutine

       subroutine writeTime(this,dir,parentUnit)
         implicit none
         type(time), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Time.txt")
         endif
         call writeTimeElapsed(this,dir,NewU)
         call writeTimeCumulative(this,dir,NewU)
         call writeTimeAverage(this,dir,NewU)
         call writeTimeRemaining(this,dir,NewU)
         call writeTimeN_photons(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for time written to file +++'
         endif
       end subroutine

         include '../helper/time_helper.f'

       end module time_mod