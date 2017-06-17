       module counters_mod
       use allClassFuncs_mod


       type counters
         integer :: i
         integer :: j
         integer :: k
         integer :: i_geometry
       endtype

       contains

       function setCounters(i,j,k,i_geometry) result(this)
         implicit none
         type(counters) :: this
         integer, intent(in) :: i
         integer, intent(in) :: j
         integer, intent(in) :: k
         integer, intent(in) :: i_geometry
         call SetCountersI(this,i)
         call SetCountersJ(this,j)
         call SetCountersK(this,k)
         call SetCountersI_geometry(this,i_geometry)
       end function

       subroutine setCountersI(this,i)
         implicit none
         type(counters),intent(inout) :: this
         integer, intent(in) :: i
         this%i = i
       end subroutine

       subroutine setCountersJ(this,j)
         implicit none
         type(counters),intent(inout) :: this
         integer, intent(in) :: j
         this%j = j
       end subroutine

       subroutine setCountersK(this,k)
         implicit none
         type(counters),intent(inout) :: this
         integer, intent(in) :: k
         this%k = k
       end subroutine

       subroutine setCountersI_geometry(this,i_geometry)
         implicit none
         type(counters),intent(inout) :: this
         integer, intent(in) :: i_geometry
         this%i_geometry = i_geometry
       end subroutine

       function getCountersI(this) result(i)
         implicit none
         type(counters),intent(in) :: this
         integer :: i
         i = this%i
       end function

       function getCountersJ(this) result(j)
         implicit none
         type(counters),intent(in) :: this
         integer :: j
         j = this%j
       end function

       function getCountersK(this) result(k)
         implicit none
         type(counters),intent(in) :: this
         integer :: k
         k = this%k
       end function

       function getCountersI_geometry(this) result(i_geometry)
         implicit none
         type(counters),intent(in) :: this
         integer :: i_geometry
         i_geometry = this%i_geometry
       end function

       subroutine getCounters(this,i,j,k,i_geometry)
         implicit none
         type(counters),intent(in) :: this
         integer, intent(out) :: i
         integer, intent(out) :: j
         integer, intent(out) :: k
         integer, intent(out) :: i_geometry
         i = getCountersI(this)
         j = getCountersJ(this)
         k = getCountersK(this)
         i_geometry = getCountersI_geometry(this)
       end subroutine

       subroutine printCountersI(this)
         implicit none
         type(counters),intent(in) :: this
         integer :: i
         write(*,*) "i: ", this%i
       end subroutine

       subroutine printCountersJ(this)
         implicit none
         type(counters),intent(in) :: this
         integer :: j
         write(*,*) "j: ", this%j
       end subroutine

       subroutine printCountersK(this)
         implicit none
         type(counters),intent(in) :: this
         integer :: k
         write(*,*) "k: ", this%k
       end subroutine

       subroutine printCountersI_geometry(this)
         implicit none
         type(counters),intent(in) :: this
         integer :: i_geometry
         write(*,*) "i_geometry: ", this%i_geometry
       end subroutine

       subroutine printCounters(this)
         implicit none
         type(counters), intent(in) :: this
         write(*,*) 'Printed data for counters'
         write(*,*) '***************'
         call printCountersI(this)
         call printCountersJ(this)
         call printCountersK(this)
         call printCountersI_geometry(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeCountersI(this,dir,parentUnit)
         implicit none
         type(counters),intent(in) :: this
         integer :: i
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "i: ", this%i
         else
           call newAndOpen(dir,"i")
           write(NewU,*) "i: ", this%i
           call closeAndMessage(NewU,"i")
         endif
       end subroutine

       subroutine writeCountersJ(this,dir,parentUnit)
         implicit none
         type(counters),intent(in) :: this
         integer :: j
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "j: ", this%j
         else
           call newAndOpen(dir,"j")
           write(NewU,*) "j: ", this%j
           call closeAndMessage(NewU,"j")
         endif
       end subroutine

       subroutine writeCountersK(this,dir,parentUnit)
         implicit none
         type(counters),intent(in) :: this
         integer :: k
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "k: ", this%k
         else
           call newAndOpen(dir,"k")
           write(NewU,*) "k: ", this%k
           call closeAndMessage(NewU,"k")
         endif
       end subroutine

       subroutine writeCountersI_geometry(this,dir,parentUnit)
         implicit none
         type(counters),intent(in) :: this
         integer :: i_geometry
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "i_geometry: ", this%i_geometry
         else
           call newAndOpen(dir,"i_geometry")
           write(NewU,*) "i_geometry: ", this%i_geometry
           call closeAndMessage(NewU,"i_geometry")
         endif
       end subroutine

       subroutine writeCounters(this,dir,parentUnit)
         implicit none
         type(counters), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Counters.txt")
         endif
         call writeCountersI(this,dir,NewU)
         call writeCountersJ(this,dir,NewU)
         call writeCountersK(this,dir,NewU)
         call writeCountersI_geometry(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for counters written to file +++'
         endif
       end subroutine


       end module counters_mod