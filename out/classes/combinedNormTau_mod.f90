       module combinedNormTau_mod
       use allClassFuncs_mod
       use constants_mod
       use counters_mod
       use exportData_mod


       type combinedNormTau
         private
         real(kind=dp_num) :: CNT
       endtype

       contains

       function setCombinednormtau(CNT) result(this)
         implicit none
         type(combinedNormTau) :: this
         real(kind=dp_num), intent(in) :: CNT
         call SetCombinednormtauCnt(this,CNT)
       end function

       subroutine setCombinednormtauCnt(this,CNT)
         implicit none
         type(combinedNormTau),intent(inout) :: this
         real(kind=dp_num), intent(in) :: CNT
         this%CNT = CNT
       end subroutine

       function getCombinednormtauCnt(this) result(CNT)
         implicit none
         type(combinedNormTau),intent(in) :: this
         real(kind=dp_num) :: CNT
         CNT = this%CNT
       end function

       subroutine getCombinednormtau(this,CNT)
         implicit none
         type(combinedNormTau),intent(in) :: this
         real(kind=dp_num), intent(out) :: CNT
         CNT = getCombinednormtauCnt(this)
       end subroutine

       subroutine printCombinednormtauCnt(this)
         implicit none
         type(combinedNormTau),intent(in) :: this
         real(kind=dp_num) :: CNT
         write(*,*) "CNT: ", this%CNT
       end subroutine

       subroutine printCombinednormtau(this)
         implicit none
         type(combinedNormTau), intent(in) :: this
         write(*,*) 'Printed data for combinedNormTau'
         write(*,*) '***************'
         call printCombinednormtauCnt(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeCombinednormtauCnt(this,dir,parentUnit)
         implicit none
         type(combinedNormTau),intent(in) :: this
         real(kind=dp_num) :: CNT
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "CNT: ", this%CNT
         else
           call newAndOpen(dir,"CNT")
           write(NewU,*) "CNT: ", this%CNT
           call closeAndMessage(NewU,"CNT")
         endif
       end subroutine

       subroutine writeCombinednormtau(this,dir,parentUnit)
         implicit none
         type(combinedNormTau), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Combinednormtau.txt")
         endif
         call writeCombinednormtauCnt(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*)
     &    '+++ Data for combinedNormTau written to file +++'
         endif
       end subroutine

         include '../helper/combinedNormTau_helper.f'

       end module combinedNormTau_mod